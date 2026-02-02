# Test user's exact approach
library(httr)
library(tidyverse)

cat("Testing user's exact POST approach...\n\n")

token <- Sys.getenv("RDM_REDCAP_TOKEN")
url <- Sys.getenv("REDCAP_URL")

formData <- list("token"=token,
    content='record',
    action='export',
    format='csv',
    type='flat',
    csvDelimiter='',
    'forms[0]'='resident_data',
    'forms[1]'='assessment',
    'forms[2]'='faculty_evaluation',
    'forms[3]'='s_eval',
    'forms[4]'='ilp',
    'forms[5]'='questions',
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

cat("Total records:", nrow(result), "\n")
cat("Total columns:", ncol(result), "\n\n")

# Check faculty_evaluation
fac_eval <- result %>%
  filter(redcap_repeat_instrument == "faculty_evaluation")

cat("Faculty evaluation records:", nrow(fac_eval), "\n")
cat("Records with fac_fell_name populated:", sum(!is.na(fac_eval$fac_fell_name)), "\n\n")

# Check Fred Buckhold
fred <- result %>%
  filter(fac_fell_name == "Fred Buckhold")

cat("Fred Buckhold evaluations:", nrow(fred), "\n")

if (nrow(fred) > 0) {
  cat("\nFirst few records:\n")
  print(fred %>% select(record_id, redcap_repeat_instance, fac_fell_name, fac_eval_date) %>% head(10))
}
