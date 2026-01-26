library(REDCapR)
library(tidyverse)
library(lubridate)

# ==============================================================================
# REDCap Data Download Functions
# ==============================================================================

# Download full Faculty database
download_faculty_data <- function() {
  result <- REDCapR::redcap_read(
    redcap_uri = Sys.getenv("REDCAP_URL"),
    token = Sys.getenv("FACULTY_REDCAP_TOKEN")
  )
  
  return(result$data)
}

# Download RDM data - simplified approach, download everything
download_rdm_focused <- function() {
  cat("Downloading complete RDM database (all data)...\n")

  # Simple download - get ALL data, no form filtering
  # This ensures we get all repeating instances
  all_data <- REDCapR::redcap_read(
    redcap_uri = Sys.getenv("REDCAP_URL"),
    token = Sys.getenv("RDM_REDCAP_TOKEN")
  )$data

  cat("✓ Downloaded", nrow(all_data), "total records\n")
  cat("Separating forms by completion fields...\n")

  # Separate by form completion fields AND repeating instrument field
  # For repeating instruments, check redcap_repeat_instrument column

  resident_data <- all_data %>%
    filter(is.na(redcap_repeat_instrument) & !is.na(resident_data_complete))

  assessment <- all_data %>%
    filter((redcap_repeat_instrument == "assessment" | is.na(redcap_repeat_instrument)) &
           !is.na(assessment_complete))

  faculty_eval <- all_data %>%
    filter((redcap_repeat_instrument == "faculty_evaluation" |
            (!is.na(faculty_evaluation_complete) & is.na(redcap_repeat_instrument))))

  s_eval <- all_data %>%
    filter((redcap_repeat_instrument == "s_eval" | is.na(redcap_repeat_instrument)) &
           !is.na(s_eval_complete))

  ilp <- all_data %>%
    filter((redcap_repeat_instrument == "ilp" | is.na(redcap_repeat_instrument)) &
           !is.na(ilp_complete))

  questions <- all_data %>%
    filter((redcap_repeat_instrument == "questions" | is.na(redcap_repeat_instrument)) &
           !is.na(questions_complete))

  cat("✓ Forms separated:\n")
  cat("  • resident_data:", nrow(resident_data), "rows\n")
  cat("  • assessment:", nrow(assessment), "rows\n")
  cat("  • faculty_evaluation:", nrow(faculty_eval), "rows\n")
  cat("  • s_eval:", nrow(s_eval), "rows\n")
  cat("  • ilp:", nrow(ilp), "rows\n")
  cat("  • questions:", nrow(questions), "rows\n")

  # Return as named list
  list(
    resident_data = resident_data,
    assessment = assessment,
    faculty_evaluation = faculty_eval,
    s_eval = s_eval,
    ilp = ilp,
    questions = questions
  )
}

# Save data locally for testing (avoids hitting API repeatedly during development)
save_test_data <- function() {
  cat("\n=== Saving test data for offline development ===\n")
  
  faculty_data <- download_faculty_data()
  rdm_data <- download_rdm_focused()
  
  # Save as RDS files
  saveRDS(faculty_data, "data/faculty_test.rds")
  saveRDS(rdm_data, "data/rdm_test.rds")
  
  cat("\n✓ Data saved successfully!\n")
  cat("  - data/faculty_test.rds:", nrow(faculty_data), "rows\n")
  cat("  - data/rdm_test.rds: List with", length(rdm_data), "forms\n")
  for (form_name in names(rdm_data)) {
    cat("    •", form_name, ":", nrow(rdm_data[[form_name]]), "rows\n")
  }
}

# Load cached test data (for development)
load_test_data <- function() {
  if (!file.exists("data/faculty_test.rds") || !file.exists("data/rdm_test.rds")) {
    stop("Test data not found. Run save_test_data() first.")
  }
  
  list(
    faculty = readRDS("data/faculty_test.rds"),
    rdm = readRDS("data/rdm_test.rds")
  )
}

# ==============================================================================
# Data Cleaning Functions
# ==============================================================================

clean_faculty_names <- function(names) {
  # Standardize name format
  # Trim whitespace, handle case sensitivity
  names %>%
    str_trim() %>%
    str_to_title() %>%
    str_squish()  # Remove extra internal whitespace
}

assign_academic_year <- function(date_vector) {
  # Assign academic year based on date
  # July-June year definition
  # Return academic year labels (e.g., "2024-2025")

  # Handle different date formats
  if (is.numeric(date_vector)) {
    # REDCap stores dates as numeric YYYYM format (e.g., 20251 = 2025 January)
    year_val <- floor(date_vector / 100)
    month_val <- date_vector %% 100
  } else {
    if (is.character(date_vector)) {
      date_vector <- as.Date(date_vector)
    }
    year_val <- lubridate::year(date_vector)
    month_val <- lubridate::month(date_vector)
  }

  # If July or later, academic year starts this year
  # Otherwise, academic year started last year
  ifelse(
    month_val >= 7,
    paste0(year_val, "-", year_val + 1),
    paste0(year_val - 1, "-", year_val)
  )
}

get_current_academic_year <- function() {
  today <- Sys.Date()
  year_val <- lubridate::year(today)
  month_val <- lubridate::month(today)
  
  if (month_val >= 7) {
    paste0(year_val, "-", year_val + 1)
  } else {
    paste0(year_val - 1, "-", year_val)
  }
}

parse_checkbox_field <- function(data, field_prefix) {
  # Parse REDCap checkbox fields
  # REDCap exports checkboxes as multiple columns: field___1, field___2, etc.
  # 
  # Args:
  #   data: data frame containing checkbox columns
  #   field_prefix: the base field name (e.g., "fac_med_ed", "s_e_topic_sel")
  #
  # Returns: list column with selected options for each row
  
  # Find all columns matching the pattern
  checkbox_cols <- names(data)[str_detect(names(data), paste0("^", field_prefix, "___"))]
  
  if (length(checkbox_cols) == 0) {
    warning(paste("No checkbox columns found for", field_prefix))
    return(list())
  }
  
  # For each row, get the selected options
  apply(data[, checkbox_cols], 1, function(row) {
    # Checkbox columns are 1 if checked, 0 or NA if not
    selected <- names(row)[row == 1 & !is.na(row)]
    
    # Extract the option number from column name
    option_nums <- str_extract(selected, "\\d+$")
    
    if (length(option_nums) == 0) {
      return(NA_character_)
    } else {
      return(paste(option_nums, collapse = ","))
    }
  })
}

# ==============================================================================
# Data Validation Functions
# ==============================================================================

check_name_matching <- function(faculty_data, rdm_data) {
  # Check how well faculty names match between databases
  
  faculty_names <- faculty_data %>%
    filter(!is.na(fac_name), fac_name != "") %>%
    pull(fac_name) %>%
    clean_faculty_names()
  
  # Check in faculty_evaluation
  eval_names <- rdm_data$faculty_evaluation %>%
    filter(!is.na(fac_fell_name), fac_fell_name != "") %>%
    pull(fac_fell_name) %>%
    unique() %>%
    clean_faculty_names()
  
  # Check in assessment
  assess_names <- rdm_data$assessment %>%
    filter(!is.na(ass_faculty), ass_faculty != "") %>%
    pull(ass_faculty) %>%
    unique() %>%
    clean_faculty_names()
  
  # Find matches and mismatches
  cat("\n=== Name Matching Report ===\n")
  cat("Faculty database:", length(faculty_names), "unique names\n")
  cat("Faculty evaluations:", length(eval_names), "unique names\n")
  cat("Assessments:", length(assess_names), "unique names\n\n")
  
  cat("Names in evaluations NOT in faculty DB:\n")
  print(setdiff(eval_names, faculty_names))
  
  cat("\nNames in assessments NOT in faculty DB:\n")
  print(setdiff(assess_names, faculty_names))
  
  cat("\nNames in faculty DB NOT in evaluations:\n")
  print(setdiff(faculty_names, eval_names))
}