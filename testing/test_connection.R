# Test script for faculty dashboard data connections
# Run this to verify your REDCap connections and save test data

source("R/utils/data_processing.R")

# ==============================================================================
# Test Data Downloads
# ==============================================================================

cat("\n╔════════════════════════════════════════╗\n")
cat("║  Faculty Dashboard - Data Test        ║\n")
cat("╚════════════════════════════════════════╝\n\n")

# Test Faculty REDCap
cat("📊 Testing Faculty REDCap connection...\n")
faculty_test <- download_faculty_data()
cat("✓ Success! Rows:", nrow(faculty_test), "| Columns:", ncol(faculty_test), "\n\n")

# Test RDM REDCap (only needed forms)
cat("📊 Testing RDM REDCap connection...\n")
rdm_test <- download_rdm_focused()
cat("✓ Success! Downloaded", length(rdm_test), "forms\n")
for (form_name in names(rdm_test)) {
  cat("  •", form_name, ":", nrow(rdm_test[[form_name]]), "rows\n")
}

# ==============================================================================
# Save for Offline Development
# ==============================================================================

cat("\n💾 Saving data for offline development...\n")
save_test_data()

# ==============================================================================
# Explore the Data Structure
# ==============================================================================

cat("\n╔════════════════════════════════════════╗\n")
cat("║  Data Structure Overview               ║\n")
cat("╚════════════════════════════════════════╝\n\n")

# Faculty Data
cat("📋 Faculty REDCap Columns:\n")
cat("───────────────────────────────────────\n")
print(names(faculty_test))

cat("\n📊 Faculty Summary:\n")
cat("───────────────────────────────────────\n")
cat("Total faculty records:", nrow(faculty_test), "\n")
cat("Active (not archived):", sum(faculty_test$archived == 0, na.rm = TRUE), "\n")
cat("Faculty vs Fellows:", "\n")
print(table(faculty_test$fac_fell, useNA = "ifany"))
cat("\nClinical affiliates:\n")
print(table(faculty_test$fac_clin, useNA = "ifany"))

# Faculty Evaluations
cat("\n📋 Faculty Evaluation Data:\n")
cat("───────────────────────────────────────\n")
faculty_evals <- rdm_test$faculty_evaluation
cat("Total evaluation rows:", nrow(faculty_evals), "\n")
cat("Unique faculty evaluated:", 
    length(unique(faculty_evals$fac_fell_name[!is.na(faculty_evals$fac_fell_name)])), "\n")

cat("\nKey columns:\n")
eval_cols <- c("record_id", "fac_fell_name", "time_teaching", "att_overall", 
               "att_ext_tea", "att_give_feed", "plus", "delta")
print(eval_cols[eval_cols %in% names(faculty_evals)])

cat("\nOverall teaching ratings distribution:\n")
print(table(faculty_evals$att_overall, useNA = "ifany"))

# Assessments
cat("\n📋 Assessment Data:\n")
cat("───────────────────────────────────────\n")
assessments <- rdm_test$assessment
cat("Total assessment rows:", nrow(assessments), "\n")
cat("Unique faculty assessing:", 
    length(unique(assessments$ass_faculty[!is.na(assessments$ass_faculty)])), "\n")

cat("\nKey columns:\n")
assess_cols <- c("record_id", "ass_date", "ass_faculty", "ass_level", 
                 "ass_plus", "ass_delta", "ass_specialty")
print(assess_cols[assess_cols %in% names(assessments)])

# Resident Data (for work-room)
cat("\n📋 Resident Data:\n")
cat("───────────────────────────────────────\n")
resident_data <- rdm_test$resident_data
cat("Total resident records:", nrow(resident_data), "\n")
cat("Active residents:", sum(resident_data$res_archive == 0, na.rm = TRUE), "\n")

# Self-Eval Data
cat("\n📋 Self-Evaluation Data (for report cards):\n")
cat("───────────────────────────────────────\n")
s_eval <- rdm_test$s_eval
cat("Total s_eval rows:", nrow(s_eval), "\n")
cat("Residents with self-evals:", 
    length(unique(s_eval$record_id[!is.na(s_eval$record_id)])), "\n")

# Check for learning style and topic fields
learning_style_cols <- names(s_eval)[str_detect(names(s_eval), "s_e_learn_style")]
topic_cols <- names(s_eval)[str_detect(names(s_eval), "s_e_topic_sel")]
cat("\nLearning style checkbox columns:", length(learning_style_cols), "\n")
cat("Topic selection checkbox columns:", length(topic_cols), "\n")

# Questions/Attendance Data
cat("\n📋 Questions/Attendance Data:\n")
cat("───────────────────────────────────────\n")
questions <- rdm_test$questions
cat("Total question/attendance rows:", nrow(questions), "\n")

# Check for key fields
key_question_cols <- c("record_id", "q_date", "q_rotation")
cat("Key columns present:", 
    paste(key_question_cols[key_question_cols %in% names(questions)], collapse = ", "), 
    "\n")

# ==============================================================================
# Name Matching Validation
# ==============================================================================

cat("\n╔════════════════════════════════════════╗\n")
cat("║  Name Matching Validation              ║\n")
cat("╚════════════════════════════════════════╝\n")

check_name_matching(faculty_test, rdm_test)

# ==============================================================================
# Sample Faculty Evaluation Data
# ==============================================================================

cat("\n╔════════════════════════════════════════╗\n")
cat("║  Sample Faculty Evaluation Records     ║\n")
cat("╚════════════════════════════════════════╝\n\n")

sample_evals <- faculty_evals %>%
  filter(!is.na(fac_fell_name), fac_fell_name != "") %>%
  select(record_id, fac_fell_name, time_teaching, att_overall, plus, delta) %>%
  head(5)

print(sample_evals)

# ==============================================================================
# Test Academic Year Calculation
# ==============================================================================

cat("\n╔════════════════════════════════════════╗\n")
cat("║  Academic Year Calculation Test        ║\n")
cat("╚════════════════════════════════════════╝\n\n")

cat("Current date:", as.character(Sys.Date()), "\n")
cat("Current academic year:", get_current_academic_year(), "\n\n")

# Test with some sample dates
test_dates <- c("2024-06-30", "2024-07-01", "2025-01-06", "2025-06-30", "2025-07-01")
cat("Academic year examples:\n")
for (date in test_dates) {
  cat("  ", date, "→", assign_academic_year(date), "\n")
}

cat("\n✓ All tests complete!\n")
cat("────────────────────────────────────────\n")
cat("Next steps:\n")
cat("  1. Review the data structure above\n")
cat("  2. Check the name matching report\n")
cat("  3. Ready to build the app!\n\n")