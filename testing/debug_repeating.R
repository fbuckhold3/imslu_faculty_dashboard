# Debug repeating instrument filtering

library(tidyverse)
source("R/utils/data_processing.R")

cat("\n=== DEBUGGING REPEATING INSTRUMENT FILTER ===\n\n")

# Download faculty data first
cat("Loading faculty data...\n")
faculty_data <- download_faculty_data()
cat("✓ Faculty data loaded:", nrow(faculty_data), "records\n\n")

# Download RDM data (raw, without mapping applied yet for debugging)
cat("Downloading raw RDM data...\n")
all_data <- REDCapR::redcap_read(
  redcap_uri = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("RDM_REDCAP_TOKEN")
)$data

cat("Total records downloaded:", nrow(all_data), "\n\n")

# Check redcap_repeat_instrument values
cat("Unique values in redcap_repeat_instrument:\n")
print(table(all_data$redcap_repeat_instrument, useNA = "always"))

cat("\n\nRecords where redcap_repeat_instrument == 'faculty_evaluation':\n")
fac_eval_repeat <- all_data %>%
  filter(redcap_repeat_instrument == "faculty_evaluation")
cat("  Count:", nrow(fac_eval_repeat), "\n")

# Check how many have fac_fell_name filled
cat("\n  Of these, how many have fac_fell_name filled:\n")
cat("    With fac_fell_name:", sum(!is.na(fac_eval_repeat$fac_fell_name)), "\n")
cat("    Without fac_fell_name:", sum(is.na(fac_eval_repeat$fac_fell_name)), "\n")

# Check Fred Buckhold specifically in repeating instances
cat("\n  Looking for Fred Buckhold in repeating instances:\n")
if ("fac_fell_name" %in% names(fac_eval_repeat)) {
  fred_repeating <- fac_eval_repeat %>%
    filter(!is.na(fac_fell_name)) %>%
    filter(str_detect(tolower(as.character(fac_fell_name)), "buckhold"))
  cat("    Found:", nrow(fred_repeating), "rows\n")

  if (nrow(fred_repeating) > 0) {
    cat("\n    First few fac_fell_name values:\n")
    print(head(fred_repeating$fac_fell_name, 10))
  }
}

# Check if fac_fell_name might be coded
cat("\n\nChecking if fac_fell_name is numeric (coded field):\n")
cat("  Class:", class(all_data$fac_fell_name), "\n")
if (is.numeric(all_data$fac_fell_name)) {
  cat("  Unique codes:\n")
  print(sort(unique(all_data$fac_fell_name[!is.na(all_data$fac_fell_name)])))

  # Try to find Fred Buckhold's code
  cat("\n  Looking for Fred Buckhold in faculty database:\n")
  fred <- faculty_data %>%
    filter(str_detect(tolower(fac_name), "buckhold"))

  if (nrow(fred) > 0) {
    cat("    Found:", fred$fac_name, "\n")
    cat("    record_id:", fred$record_id, "\n")

    # Check if this record_id appears in fac_fell_name
    fred_code_evals <- all_data %>%
      filter(!is.na(fac_fell_name) & fac_fell_name == fred$record_id)

    cat("    Evaluations with fac_fell_name ==", fred$record_id, ":", nrow(fred_code_evals), "\n")

    if (nrow(fred_code_evals) > 0) {
      cat("    ✓ FOUND EVALUATIONS USING NUMERIC CODE!\n")
      cat("    With redcap_repeat_instrument == 'faculty_evaluation':",
          sum(fred_code_evals$redcap_repeat_instrument == "faculty_evaluation", na.rm = TRUE), "\n")
      cat("    With NA redcap_repeat_instrument:",
          sum(is.na(fred_code_evals$redcap_repeat_instrument)), "\n")
    }
  }
}

cat("\n=== Now testing the NEW download_rdm_focused function with mapping ===\n")
rdm_mapped <- download_rdm_focused(faculty_data = faculty_data)

cat("\n✓ After mapping, faculty_evaluation has:", nrow(rdm_mapped$faculty_evaluation), "rows\n")

# Check Fred Buckhold in mapped data
fred_mapped <- rdm_mapped$faculty_evaluation %>%
  filter(!is.na(fac_fell_name), str_detect(tolower(fac_fell_name), "buckhold"))

cat("✓ Fred Buckhold evaluations in mapped data:", nrow(fred_mapped), "\n")

if (nrow(fred_mapped) > 0) {
  cat("\n  Sample fac_fell_name values:\n")
  print(head(unique(fred_mapped$fac_fell_name), 5))
}

cat("\n=== END DEBUG ===\n")
