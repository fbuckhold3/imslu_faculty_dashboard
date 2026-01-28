library(tidyverse)
library(lubridate)

# ==============================================================================
# Constants
# ==============================================================================

MIN_EVALUATIONS <- 5           # Minimum evals per period to display
EVALUATION_DELAY_MONTHS <- 6   # Delay before showing evaluations

# Primary evaluation domains (all on 1-5 scale) - used for spider plots and main comparisons
PRIMARY_EVAL_DOMAINS <- c(
  "approachability",    # How approachable is the attending/fellow to address questions or updates?
  "respect",            # Is the attending/fellow respectful of all members of the healthcare team?
  "bedside_manner",     # Does the attending/fellow role model a good bedside manner?
  "time_teaching",      # Does the attending/fellow ensure time for teaching?
  "ques_clin_des",      # Does the attending/fellow ask questions about your clinical decisions?
  "autonomy",           # The attending/fellow allowed me autonomy in my clinical decision making
  "provide_feedback",   # The attending/fellow provided me with feedback that allowed me to improve my performance (NOTE: verify field name)
  "organized_session"   # The clinical session (or sessions) I had with the attending were conducted in an organized fashion? (NOTE: verify field name)
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
  # Primary evaluation domain labels (1-5 scale)
  primary_labels <- c(
    "approachability" = "Approachability",
    "respect" = "Respectful of Healthcare Team",
    "bedside_manner" = "Role Models Good Bedside Manner",
    "time_teaching" = "Ensures Time for Teaching",
    "ques_clin_des" = "Asks About Clinical Decisions",
    "autonomy" = "Allows Autonomy in Decision Making",
    "provide_feedback" = "Provides Feedback for Improvement",
    "organized_session" = "Conducts Organized Sessions"
  )

  # Secondary metric labels (different scales)
  secondary_labels <- c(
    "att_overall" = "Overall Teaching Quality",
    "att_ext_tea" = "Extra Teaching Effort",
    "att_give_feed" = "Feedback Type Given",
    "eval_done" = "Evaluation Completion"
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
