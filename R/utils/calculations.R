library(tidyverse)
library(lubridate)

# ==============================================================================
# Constants
# ==============================================================================

MIN_EVALUATIONS <- 5           # Minimum evals per period to display
EVALUATION_DELAY_MONTHS <- 6   # Delay before showing evaluations

# Evaluation domains to analyze
EVAL_DOMAINS <- c(
  "time_teaching",    # Ensures time for teaching (1-5)
  "att_overall",      # Overall teaching rating (1-5)
  "att_ext_tea",      # Extra teaching effort
  "att_give_feed"     # Feedback quality
)

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

  # Filter - include records with NA academic year (missing dates) OR matching year
  data %>%
    filter(is.na(academic_year) | academic_year == year)
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

  cutoff_date <- Sys.Date() - months(delay_months)

  # Include records where date is before cutoff OR date is NA (missing dates included)
  data %>%
    filter(is.na(.data[[date_col]]) | .data[[date_col]] <= cutoff_date)
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
# Calculation Functions
# ==============================================================================

#' Calculate mean evaluation scores for specific faculty
#'
#' @param eval_data Faculty evaluation data
#' @param faculty_name Faculty name to filter (if NULL, calculates for all)
#' @param domains Vector of domain column names to calculate
#' @return Data frame with domain names and mean scores
calculate_faculty_eval_means <- function(eval_data,
                                          faculty_name = NULL,
                                          domains = EVAL_DOMAINS) {
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
#' @param domains Vector of domain column names to calculate
#' @return Data frame with domain names and mean scores across all faculty
calculate_all_faculty_means <- function(eval_data, domains = EVAL_DOMAINS) {
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

# ==============================================================================
# Label Functions
# ==============================================================================

#' Get human-readable labels for evaluation domains
#'
#' @param domain_name Column name
#' @return Human-readable label
get_domain_label <- function(domain_name) {
  labels <- c(
    "time_teaching" = "Ensures Time for Teaching",
    "att_overall" = "Overall Teaching Quality",
    "att_ext_tea" = "Extra Teaching Effort",
    "att_give_feed" = "Quality of Feedback Given"
  )

  ifelse(domain_name %in% names(labels), labels[domain_name], domain_name)
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
