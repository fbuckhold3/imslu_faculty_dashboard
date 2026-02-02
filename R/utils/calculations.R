library(tidyverse)
library(lubridate)

# ==============================================================================
# Constants
# ==============================================================================

MIN_EVALUATIONS <- 5           # Minimum evals per period to display
EVALUATION_DELAY_MONTHS <- 6   # Delay before showing evaluations

# ==============================================================================
# Rotation Mapping Constants
# ==============================================================================

# Map rotation codes to rotation names and divisions
# Based on q_rotation field from questions form
ROTATION_MAPPING <- list(
  # GIM - Hospitalist (fac_div = 7)
  "1" = list(name = "Red Team", division = 7, label = "GIM - Hospitalist"),
  "2" = list(name = "Green Team", division = 7, label = "GIM - Hospitalist"),
  "3" = list(name = "White Team", division = 7, label = "GIM - Hospitalist"),
  "5" = list(name = "Diamond Team", division = 7, label = "GIM - Hospitalist"),
  "6" = list(name = "Gold Team", division = 7, label = "GIM - Hospitalist"),
  "13" = list(name = "VA A", division = 7, label = "GIM - Hospitalist", va_only = TRUE),
  "14" = list(name = "VA B", division = 7, label = "GIM - Hospitalist", va_only = TRUE),
  "15" = list(name = "VA C", division = 7, label = "GIM - Hospitalist", va_only = TRUE),
  "16" = list(name = "VA D", division = 7, label = "GIM - Hospitalist", va_only = TRUE),

  # GIM - Primary Care (fac_div = 8)
  "4" = list(name = "Yellow Team", division = 8, label = "GIM - Primary Care"),
  "10" = list(name = "Bridge/Acute Care", division = 8, label = "GIM - Primary Care"),

  # Pulmonary / Critical Care (fac_div = 13)
  "7" = list(name = "MICU", division = 13, label = "Pulmonary/Critical Care"),

  # Gastroenterology (fac_div = 5)
  "8" = list(name = "Bronze Team", division = 5, label = "Gastroenterology"),

  # Cardiology (fac_div = 3)
  "9" = list(name = "Cardiology", division = 3, label = "Cardiology"),

  # Other (less relevant per user feedback)
  "11" = list(name = "Consults - SLUH", division = NA, label = "Other"),
  "12" = list(name = "Elective/Clinics CSM", division = NA, label = "Other"),
  "17" = list(name = "VA Clinics/Consults", division = NA, label = "Other", va_only = TRUE)
)

# Primary evaluation domains (all on 1-5 scale) - used for spider plots and main comparisons
PRIMARY_EVAL_DOMAINS <- c(
  "approachability",    # How approachable is the attending/fellow to address questions or updates?
  "respect",            # Is the attending/fellow respectful of all members of the healthcare team?
  "bedside_manner",     # Does the attending/fellow role model a good bedside manner?
  "time_teaching",      # Does the attending/fellow ensure time for teaching?
  "ques_clin_des",      # Does the attending/fellow ask questions about your clinical decisions?
  "autonomy",           # The attending/fellow allowed me autonomy in my clinical decision making
  "feedback",           # The attending/fellow provided me with feedback that allowed me to improve my performance
  "organ"               # The clinical session (or sessions) I had with the attending were conducted in an organized fashion?
)

# Secondary metrics (different scales - NOT for spider plots)
SECONDARY_METRICS <- c(
  "att_overall",      # Overall teaching rating (different scale)
  "att_ext_tea",      # Extra teaching effort (different scale)
  "att_give_feed",    # Feedback type given (different scale)
  "eval_done"         # Evaluation completion status (different scale)
)

# For backward compatibility - default to primary domains
EVAL_DOMAINS <- PRIMARY_EVAL_DOMAINS

# ==============================================================================
# Filtering Functions
# ==============================================================================

#' Filter evaluations by academic year
#'
#' @param data Data frame with date column
#' @param date_col Name of date column (default: "fac_eval_date")
#' @param year Academic year ("current", "all", or specific like "2024-2025")
#' @return Filtered data frame
filter_by_academic_year <- function(data, date_col = "fac_eval_date", year = "current") {
  if (year == "all") {
    # All time - include everything (even records without dates)
    return(data)
  }

  # Get current academic year if needed
  if (year == "current") {
    year <- get_current_academic_year()
  }

  # Add academic year column if not present
  if (!"academic_year" %in% names(data)) {
    data <- data %>%
      mutate(academic_year = assign_academic_year(.data[[date_col]]))
  }

  # Filter for specific year - EXCLUDE records with NA dates
  # Records without dates can't be assigned to current year
  data %>%
    filter(!is.na(academic_year) & academic_year == year)
}

#' Apply time delay filter to protect faculty privacy
#'
#' @param data Data frame with date column
#' @param date_col Name of date column
#' @param delay_months Number of months to delay (default: 6)
#' @return Filtered data frame
apply_time_delay <- function(data, date_col = "fac_eval_date", delay_months = EVALUATION_DELAY_MONTHS) {
  # If date column doesn't exist, return all data
  if (!date_col %in% names(data)) {
    return(data)
  }

  # Handle numeric YYYYM format from REDCap
  dates <- data[[date_col]]

  if (is.numeric(dates)) {
    # Convert YYYYM to comparable format
    # Current date in YYYYM format
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    current_yyyym <- current_year * 100 + current_month

    # Calculate cutoff (6 months ago)
    cutoff_date <- Sys.Date() - months(delay_months)
    cutoff_year <- as.numeric(format(cutoff_date, "%Y"))
    cutoff_month <- as.numeric(format(cutoff_date, "%m"))
    cutoff_yyyym <- cutoff_year * 100 + cutoff_month

    # Include records where date is before cutoff OR date is NA
    data %>%
      filter(is.na(.data[[date_col]]) | .data[[date_col]] <= cutoff_yyyym)
  } else {
    # Standard date format
    cutoff_date <- Sys.Date() - months(delay_months)
    data %>%
      filter(is.na(.data[[date_col]]) | .data[[date_col]] <= cutoff_date)
  }
}

#' Check if data meets minimum threshold
#'
#' @param data Data frame
#' @param threshold Minimum number of rows (default: 5)
#' @return TRUE if meets threshold, FALSE otherwise
check_minimum_threshold <- function(data, threshold = MIN_EVALUATIONS) {
  nrow(data) >= threshold
}

# ==============================================================================
# Assessment Counting Functions
# ==============================================================================

#' Count assessments given by each faculty member
#'
#' @param assessment_data Assessment data frame with ass_faculty and ass_date columns
#' @param faculty_list Optional vector of faculty names to filter by
#' @param year Academic year ("current", "all", or specific like "2024-2025")
#' @return Data frame with faculty name and assessment count
count_assessments_by_faculty <- function(assessment_data, faculty_list = NULL, year = "current") {
  # Filter by year if requested
  if (year != "all") {
    assessment_data <- filter_by_academic_year(assessment_data, date_col = "ass_date", year = year)
  }

  # Filter by faculty list if provided
  if (!is.null(faculty_list)) {
    assessment_data <- assessment_data %>%
      filter(ass_faculty %in% faculty_list)
  }

  # Count assessments per faculty
  counts <- assessment_data %>%
    filter(!is.na(ass_faculty), ass_faculty != "") %>%
    group_by(ass_faculty) %>%
    summarize(
      assessment_count = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(assessment_count))

  return(counts)
}

# ==============================================================================
# Calculation Functions
# ==============================================================================

#' Calculate mean evaluation scores for specific faculty
#'
#' @param eval_data Faculty evaluation data
#' @param faculty_name Faculty name to filter (if NULL, calculates for all)
#' @param domains Vector of domain column names to calculate (default: PRIMARY_EVAL_DOMAINS)
#' @return Data frame with domain names and mean scores
calculate_faculty_eval_means <- function(eval_data,
                                          faculty_name = NULL,
                                          domains = PRIMARY_EVAL_DOMAINS) {
  # Filter by faculty if specified
  if (!is.null(faculty_name)) {
    eval_data <- eval_data %>%
      filter(fac_fell_name == faculty_name)
  }

  # Calculate means for each domain
  results <- tibble(
    domain = character(),
    mean_score = numeric(),
    n = integer()
  )

  for (domain in domains) {
    if (domain %in% names(eval_data)) {
      mean_val <- mean(eval_data[[domain]], na.rm = TRUE)
      n_val <- sum(!is.na(eval_data[[domain]]))

      results <- results %>%
        add_row(
          domain = domain,
          mean_score = mean_val,
          n = n_val
        )
    }
  }

  results
}

#' Calculate evaluation means for all faculty (for comparison)
#'
#' @param eval_data Faculty evaluation data
#' @param domains Vector of domain column names to calculate (default: PRIMARY_EVAL_DOMAINS)
#' @return Data frame with domain names and mean scores across all faculty
calculate_all_faculty_means <- function(eval_data, domains = PRIMARY_EVAL_DOMAINS) {
  calculate_faculty_eval_means(
    eval_data = eval_data,
    faculty_name = NULL,  # NULL = all faculty
    domains = domains
  )
}

#' Create comparison table showing individual vs all faculty
#'
#' @param individual_means Means for specific faculty
#' @param all_means Means for all faculty
#' @return Data frame with comparison
create_comparison_table <- function(individual_means, all_means) {
  individual_means %>%
    rename(
      individual_score = mean_score,
      individual_n = n
    ) %>%
    left_join(
      all_means %>%
        select(domain, all_score = mean_score, all_n = n),
      by = "domain"
    ) %>%
    mutate(
      difference = individual_score - all_score,
      percent_diff = round((difference / all_score) * 100, 1)
    )
}

#' Calculate secondary metrics (different scales from primary domains)
#'
#' @param eval_data Faculty evaluation data
#' @param faculty_name Faculty name to filter (if NULL, calculates for all)
#' @return Data frame with secondary metric statistics
calculate_secondary_metrics <- function(eval_data, faculty_name = NULL) {
  # Filter by faculty if specified
  if (!is.null(faculty_name)) {
    eval_data <- eval_data %>%
      filter(fac_fell_name == faculty_name)
  }

  # Calculate statistics for each secondary metric
  results <- tibble(
    metric = character(),
    mean_value = numeric(),
    n = integer()
  )

  for (metric in SECONDARY_METRICS) {
    if (metric %in% names(eval_data)) {
      mean_val <- mean(eval_data[[metric]], na.rm = TRUE)
      n_val <- sum(!is.na(eval_data[[metric]]))

      results <- results %>%
        add_row(
          metric = metric,
          mean_value = mean_val,
          n = n_val
        )
    }
  }

  results
}

#' Get plus/delta feedback for a faculty member
#'
#' @param eval_data Faculty evaluation data
#' @param faculty_name Faculty name to filter
#' @return List with plus and delta data frames
get_faculty_feedback <- function(eval_data, faculty_name) {
  faculty_evals <- eval_data %>%
    filter(fac_fell_name == faculty_name)

  list(
    plus = faculty_evals %>%
      filter(!is.na(plus), plus != "") %>%
      select(fac_eval_date, plus) %>%
      arrange(desc(fac_eval_date)),

    delta = faculty_evals %>%
      filter(!is.na(delta), delta != "") %>%
      select(fac_eval_date, delta) %>%
      arrange(desc(fac_eval_date))
  )
}

#' Create faculty summary table with evaluation and assessment stats
#'
#' @param faculty_eval_data Faculty evaluation data
#' @param assessment_data Assessment data
#' @param faculty_list Vector of faculty names to include
#' @param year Academic year for filtering
#' @return Data frame with faculty summary statistics
create_faculty_summary_table <- function(faculty_eval_data, assessment_data, faculty_list, year = "current") {
  # Get evaluation counts and averages for each faculty
  eval_summary <- faculty_eval_data %>%
    filter(fac_fell_name %in% faculty_list) %>%
    {if (year != "all") filter_by_academic_year(., "fac_eval_date", year) else .} %>%
    group_by(fac_fell_name) %>%
    summarize(
      evaluations_received = n(),
      avg_overall = mean(att_overall, na.rm = TRUE),
      avg_time_teaching = mean(time_teaching, na.rm = TRUE),
      .groups = "drop"
    )

  # Get assessment counts
  assessment_counts <- count_assessments_by_faculty(assessment_data, faculty_list, year) %>%
    rename(fac_fell_name = ass_faculty)

  # Combine
  summary_table <- expand.grid(
    fac_fell_name = faculty_list,
    stringsAsFactors = FALSE
  ) %>%
    left_join(eval_summary, by = "fac_fell_name") %>%
    left_join(assessment_counts, by = "fac_fell_name") %>%
    mutate(
      evaluations_received = replace_na(evaluations_received, 0),
      assessment_count = replace_na(assessment_count, 0),
      avg_overall = round(avg_overall, 2),
      avg_time_teaching = round(avg_time_teaching, 2)
    ) %>%
    arrange(desc(evaluations_received))

  return(summary_table)
}

# ==============================================================================
# Label Functions
# ==============================================================================

#' Get human-readable labels for evaluation domains
#'
#' @param domain_name Column name
#' @return Human-readable label
get_domain_label <- function(domain_name) {
  # Primary evaluation domain labels (1-5 scale) - shortened for clarity
  primary_labels <- c(
    "approachability" = "Approachability",
    "respect" = "Respectful to Team",
    "bedside_manner" = "Bedside Manner",
    "time_teaching" = "Time for Teaching",
    "ques_clin_des" = "Questions Clinical Decisions",
    "autonomy" = "Allows Autonomy",
    "feedback" = "Actionable Feedback",
    "organ" = "Organized Sessions"
  )

  # Secondary metric labels (different scales) - shortened for clarity
  secondary_labels <- c(
    "att_overall" = "Overall Teaching",
    "att_ext_tea" = "Extra Teaching Effort",
    "att_give_feed" = "Feedback Type",
    "eval_done" = "Completed Evaluation"
  )

  # Combine all labels
  all_labels <- c(primary_labels, secondary_labels)

  ifelse(domain_name %in% names(all_labels), all_labels[domain_name], domain_name)
}

#' Add labels to evaluation results
#'
#' @param eval_results Data frame with domain column
#' @return Data frame with domain_label column added
add_domain_labels <- function(eval_results) {
  eval_results %>%
    mutate(
      domain_label = sapply(domain, get_domain_label)
    )
}

# ==============================================================================
# Conference Attendance Functions
# ==============================================================================

#' Get rotation codes for a specific faculty division and clinical site
#'
#' @param faculty_division Division code (fac_div)
#' @param faculty_clinical Clinical affiliate (fac_clin): "1" = SSM, "2" = VA
#' @return Vector of rotation codes
get_rotations_for_faculty <- function(faculty_division, faculty_clinical = "1") {
  # Get all rotation codes that match the faculty's division
  matching_rotations <- c()

  for (rotation_code in names(ROTATION_MAPPING)) {
    rotation_info <- ROTATION_MAPPING[[rotation_code]]

    # Skip rotations without a division (consults/electives)
    if (is.na(rotation_info$division)) {
      next
    }

    # Check if division matches
    if (rotation_info$division == faculty_division) {
      # For VA rotations, only include if faculty is at VA
      if (!is.null(rotation_info$va_only) && rotation_info$va_only) {
        if (faculty_clinical == "2") {
          matching_rotations <- c(matching_rotations, rotation_code)
        }
      } else {
        # Non-VA rotation - include if faculty is at SSM
        if (faculty_clinical == "1") {
          matching_rotations <- c(matching_rotations, rotation_code)
        }
      }
    }
  }

  return(matching_rotations)
}

#' Get rotation name from code
#'
#' @param rotation_code Rotation code as string or numeric
#' @return Rotation name
get_rotation_name <- function(rotation_code) {
  code_str <- as.character(rotation_code)

  if (code_str %in% names(ROTATION_MAPPING)) {
    return(ROTATION_MAPPING[[code_str]]$name)
  } else {
    return(paste0("Rotation ", code_str))
  }
}

#' Filter and aggregate conference attendance data
#'
#' @param questions_data Questions form data with q_date and q_rotation
#' @param rotation_codes Vector of rotation codes to include
#' @param weeks Number of weeks to look back (default: 4)
#' @return Data frame with weekly attendance by rotation
aggregate_conference_attendance <- function(questions_data, rotation_codes, weeks = 4) {
  # Calculate date range (last N weeks)
  end_date <- Sys.Date()
  start_date <- end_date - lubridate::weeks(weeks)

  # Handle numeric YYYYM date format from REDCap
  if (is.numeric(questions_data$q_date)) {
    # Convert YYYYM to date for filtering
    questions_data <- questions_data %>%
      mutate(
        q_date_converted = lubridate::ymd(paste0(
          floor(q_date / 100), "-",
          sprintf("%02d", q_date %% 100), "-01"
        ))
      )
    date_col <- "q_date_converted"
  } else {
    # Standard date format
    questions_data <- questions_data %>%
      mutate(q_date_converted = as.Date(q_date))
    date_col <- "q_date_converted"
  }

  # Filter data
  filtered_data <- questions_data %>%
    filter(
      !is.na(.data[[date_col]]),
      .data[[date_col]] >= start_date,
      .data[[date_col]] <= end_date,
      as.character(q_rotation) %in% rotation_codes
    )

  # If no data, return empty structure
  if (nrow(filtered_data) == 0) {
    return(tibble(
      week_label = character(),
      rotation_code = character(),
      rotation_name = character(),
      attendance_count = numeric()
    ))
  }

  # Assign week labels
  filtered_data <- filtered_data %>%
    mutate(
      week_num = as.numeric(difftime(end_date, .data[[date_col]], units = "weeks")),
      week_num = floor(week_num),
      week_label = case_when(
        week_num == 0 ~ "This Week",
        week_num == 1 ~ "1 Week Ago",
        week_num == 2 ~ "2 Weeks Ago",
        week_num == 3 ~ "3 Weeks Ago",
        TRUE ~ paste0(week_num, " Weeks Ago")
      ),
      rotation_code = as.character(q_rotation)
    ) %>%
    filter(week_num < weeks)  # Only include last N weeks

  # Aggregate by week and rotation
  aggregated <- filtered_data %>%
    group_by(week_label, week_num, rotation_code) %>%
    summarize(attendance_count = n(), .groups = "drop") %>%
    mutate(rotation_name = sapply(rotation_code, get_rotation_name)) %>%
    arrange(desc(week_num), rotation_code)

  return(aggregated)
}

#' Calculate academic year total conference attendance
#'
#' @param questions_data Questions form data with q_date and q_rotation
#' @param rotation_codes Vector of rotation codes to include
#' @return Data frame with total attendance by rotation for current academic year
aggregate_conference_academic_year <- function(questions_data, rotation_codes) {
  # Get current academic year
  current_year <- get_current_academic_year()

  # Handle numeric YYYYM date format from REDCap
  if (is.numeric(questions_data$q_date)) {
    # Convert YYYYM to date for filtering
    questions_data <- questions_data %>%
      mutate(
        q_date_converted = lubridate::ymd(paste0(
          floor(q_date / 100), "-",
          sprintf("%02d", q_date %% 100), "-01"
        ))
      )
    date_col <- "q_date_converted"
  } else {
    # Standard date format
    questions_data <- questions_data %>%
      mutate(q_date_converted = as.Date(q_date))
    date_col <- "q_date_converted"
  }

  # Add academic year column
  questions_data <- questions_data %>%
    mutate(academic_year = assign_academic_year(.data[[date_col]]))

  # Filter for current academic year and relevant rotations
  filtered_data <- questions_data %>%
    filter(
      academic_year == current_year,
      as.character(q_rotation) %in% rotation_codes
    ) %>%
    mutate(rotation_code = as.character(q_rotation))

  # If no data, return empty structure
  if (nrow(filtered_data) == 0) {
    return(tibble(
      rotation_code = character(),
      rotation_name = character(),
      attendance_count = numeric()
    ))
  }

  # Aggregate by rotation
  aggregated <- filtered_data %>%
    group_by(rotation_code) %>%
    summarize(attendance_count = n(), .groups = "drop") %>%
    mutate(rotation_name = sapply(rotation_code, get_rotation_name)) %>%
    arrange(rotation_code)

  return(aggregated)
}

# ==============================================================================
# Assessment Count Functions (Faculty evaluating residents)
# ==============================================================================

#' Count assessments completed by a specific faculty member
#'
#' @param assessment_data Assessment form data
#' @param faculty_name Faculty name to filter by (NULL for all faculty)
#' @param academic_year "current", "all", or specific year like "2024-2025"
#' @return Integer count of assessments
count_faculty_assessments <- function(assessment_data, faculty_name = NULL, academic_year = "current") {
  if (is.null(assessment_data) || nrow(assessment_data) == 0) {
    return(0)
  }

  # Filter by faculty if specified
  if (!is.null(faculty_name)) {
    assessment_data <- assessment_data %>%
      filter(ass_faculty == faculty_name)
  }

  # Filter by academic year
  if (academic_year != "all" && "ass_date" %in% names(assessment_data)) {
    assessment_data <- filter_by_academic_year(assessment_data, "ass_date", academic_year)
  }

  nrow(assessment_data)
}

#' Get assessment counts for all faculty
#'
#' @param assessment_data Assessment form data
#' @param faculty_list Optional vector of faculty names to include
#' @param academic_year "current", "all", or specific year
#' @return Data frame with faculty_name, assessment_count, sorted by count descending
get_all_faculty_assessment_counts <- function(assessment_data, faculty_list = NULL, academic_year = "current") {
  if (is.null(assessment_data) || nrow(assessment_data) == 0) {
    return(tibble(
      faculty_name = character(),
      assessment_count = integer(),
      academic_year = character()
    ))
  }

  # Filter by academic year first
  if (academic_year != "all" && "ass_date" %in% names(assessment_data)) {
    assessment_data <- filter_by_academic_year(assessment_data, "ass_date", academic_year)
  }

  # Filter to valid faculty entries
  assessment_data <- assessment_data %>%
    filter(!is.na(ass_faculty) & ass_faculty != "")

  # Filter to specific faculty list if provided
  if (!is.null(faculty_list)) {
    assessment_data <- assessment_data %>%
      filter(ass_faculty %in% faculty_list)
  }

  # Count by faculty
  counts <- assessment_data %>%
    count(ass_faculty, name = "assessment_count") %>%
    rename(faculty_name = ass_faculty) %>%
    arrange(desc(assessment_count))

  # Add academic year label
  year_label <- if (academic_year == "current") {
    get_current_academic_year()
  } else if (academic_year == "all") {
    "All Time"
  } else {
    academic_year
  }

  counts$academic_year <- year_label

  counts
}

#' Get assessment statistics for a faculty member compared to peers
#'
#' @param assessment_data Assessment form data
#' @param faculty_name Faculty name
#' @param peer_faculty_list Vector of peer faculty names for comparison
#' @param academic_year "current", "all", or specific year
#' @return List with individual count, peer mean, peer median, rank
get_faculty_assessment_stats <- function(assessment_data, faculty_name, peer_faculty_list = NULL, academic_year = "current") {
  # Get individual count
  individual_count <- count_faculty_assessments(assessment_data, faculty_name, academic_year)

  # Get all counts for comparison
  all_counts <- get_all_faculty_assessment_counts(assessment_data, peer_faculty_list, academic_year)

  if (nrow(all_counts) == 0) {
    return(list(
      individual_count = individual_count,
      peer_mean = NA,
      peer_median = NA,
      rank = NA,
      total_faculty = 0
    ))
  }

  # Calculate stats
  peer_mean <- mean(all_counts$assessment_count, na.rm = TRUE)
  peer_median <- median(all_counts$assessment_count, na.rm = TRUE)

  # Find rank (1 = most assessments)
  rank <- which(all_counts$faculty_name == faculty_name)
  if (length(rank) == 0) rank <- NA else rank <- rank[1]

  list(
    individual_count = individual_count,
    peer_mean = round(peer_mean, 1),
    peer_median = peer_median,
    rank = rank,
    total_faculty = nrow(all_counts)
  )
}
