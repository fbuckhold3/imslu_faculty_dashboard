# Diagnostic Script to Check Data Download and Filtering
# Run this to verify data is loading correctly

library(tidyverse)
source("R/utils/data_processing.R")
source("R/utils/calculations.R")

cat("\n=== DIAGNOSTIC CHECK ===\n\n")

# Load data
cat("1. Loading data from REDCap...\n")
faculty_data <- download_faculty_data()
rdm_data <- download_rdm_focused(faculty_data = faculty_data)
cat("   ✓ Data loaded\n\n")

# Check faculty data
cat("2. Faculty Database:\n")
cat("   Total faculty records:", nrow(faculty_data), "\n")
cat("   Active faculty (archived == 0):", sum(faculty_data$archived == 0, na.rm = TRUE), "\n")
cat("   Fields present:", paste(names(faculty_data)[1:10], collapse=", "), "...\n\n")

# Check for Fred Buckhold
cat("3. Looking for Fred Buckhold:\n")
fred <- faculty_data %>%
  filter(str_detect(tolower(fac_name), "buckhold"))
if (nrow(fred) > 0) {
  cat("   ✓ Found:", fred$fac_name, "\n")
  cat("   Record ID:", fred$record_id, "\n")
  cat("   Archived:", fred$archived, "\n")
  cat("   Division:", fred$fac_div, "\n")
  if ("fac_meded_lead" %in% names(fred)) {
    cat("   Med Ed Leader:", fred$fac_meded_lead, "\n")
  }
} else {
  cat("   ✗ Fred Buckhold NOT FOUND in faculty database\n")
  cat("   First few faculty names:\n")
  print(head(faculty_data$fac_name, 10))
}
cat("\n")

# Check evaluation data
cat("4. Faculty Evaluation Data:\n")
eval_data <- rdm_data$faculty_evaluation
cat("   Total evaluations:", nrow(eval_data), "\n")
cat("   Unique faculty evaluated:", length(unique(eval_data$fac_fell_name)), "\n")
cat("   Date field exists:", "fac_eval_date" %in% names(eval_data), "\n")
if ("fac_eval_date" %in% names(eval_data)) {
  cat("   Evaluations with dates:", sum(!is.na(eval_data$fac_eval_date)), "\n")
  cat("   Evaluations without dates:", sum(is.na(eval_data$fac_eval_date)), "\n")
}
cat("\n")

# Check evaluations for Fred Buckhold
cat("5. Evaluations for Fred Buckhold:\n")
if (nrow(fred) > 0) {
  fred_name <- fred$fac_name

  # Try exact match
  fred_evals_exact <- eval_data %>%
    filter(fac_fell_name == fred_name)
  cat("   Exact match (", fred_name, "):", nrow(fred_evals_exact), "\n")

  # Try partial match - buckhold
  fred_evals_buckhold <- eval_data %>%
    filter(str_detect(tolower(fac_fell_name), "buckhold"))
  cat("   Partial match (contains 'buckhold'):", nrow(fred_evals_buckhold), "\n")

  # Try partial match - fred
  fred_evals_fred <- eval_data %>%
    filter(!is.na(fac_fell_name), str_detect(tolower(fac_fell_name), "fred"))
  cat("   Partial match (contains 'fred'):", nrow(fred_evals_fred), "\n")

  # Try first and last name from faculty record
  if ("fac_f_name" %in% names(fred) && "fac_l_name" %in% names(fred)) {
    if (!is.na(fred$fac_l_name)) {
      fred_evals_lastname <- eval_data %>%
        filter(!is.na(fac_fell_name), str_detect(tolower(fac_fell_name), tolower(fred$fac_l_name)))
      cat("   Partial match (last name '", fred$fac_l_name, "'):", nrow(fred_evals_lastname), "\n")
    }
  }

  fred_evals_partial <- fred_evals_buckhold

  if (nrow(fred_evals_partial) > 0) {
    cat("\n   Name variations found:\n")
    print(unique(fred_evals_partial$fac_fell_name))

    cat("\n   Date breakdown:\n")
    cat("     With dates:", sum(!is.na(fred_evals_partial$fac_eval_date)), "\n")
    cat("     Without dates:", sum(is.na(fred_evals_partial$fac_eval_date)), "\n")

    if (sum(!is.na(fred_evals_partial$fac_eval_date)) > 0) {
      cat("\n   Date range:\n")
      dates <- fred_evals_partial$fac_eval_date[!is.na(fred_evals_partial$fac_eval_date)]
      cat("     Earliest:", min(dates), "\n")
      cat("     Latest:", max(dates), "\n")
      cat("     Current date:", Sys.Date(), "\n")
      cat("     6 months ago:", Sys.Date() - months(6), "\n")
    }
  }
} else {
  cat("   Cannot check - Fred Buckhold not found in faculty database\n")
}
cat("\n")

# Check filtering logic
cat("6. Testing Filters:\n")
if (nrow(fred) > 0 && "fac_fell_name" %in% names(eval_data)) {
  fred_evals <- eval_data %>%
    filter(str_detect(tolower(fac_fell_name), "buckhold"))

  cat("   Base evaluations:", nrow(fred_evals), "\n")

  # Test time delay
  if ("fac_eval_date" %in% names(fred_evals)) {
    after_delay <- apply_time_delay(fred_evals, "fac_eval_date")
    cat("   After 6-month delay:", nrow(after_delay), "\n")

    # Test current year filter
    current_year <- filter_by_academic_year(after_delay, "fac_eval_date", "current")
    cat("   Current academic year only:", nrow(current_year), "\n")

    # Test all time filter
    all_time <- filter_by_academic_year(after_delay, "fac_eval_date", "all")
    cat("   All time:", nrow(all_time), "\n")
  }
}
cat("\n")

# Check name matching across databases
cat("7. Name Matching Check:\n")
cat("   Faculty names in database (first 10):\n")
print(head(faculty_data$fac_name, 10))
cat("\n   Faculty names in evaluations (first 10 unique):\n")
print(head(unique(eval_data$fac_fell_name), 10))

cat("\n=== END DIAGNOSTIC CHECK ===\n")
