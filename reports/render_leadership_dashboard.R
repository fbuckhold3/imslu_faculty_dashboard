# render_leadership_dashboard.R
#
# Renders ONLY the Leadership Dashboard (pd_report.qmd) —
# the full-department view of all faculty and fellows.
#
# Usage (from imslu_faculty_dashboard project root):
#   Sys.setenv(PRODUCTION_MODE = "TRUE")
#   source("reports/render_leadership_dashboard.R")
#
# Required .Renviron vars: FAC_TOKEN, RDM_TOKEN, REDCAP_URL
# Output: /Users/home_base/Developer/outputs/<month_yr>/leadership/Leadership_Dashboard.html

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

library(tidyverse)
library(quarto)

if (!file.exists("reports/pd_report.qmd")) {
  stop("Run this script from the imslu_faculty_dashboard project root.")
}

source("R/utils/data_processing.R")

OUTPUT_BASE  <- "/Users/home_base/Developer/outputs"
PROJECT_ROOT <- getwd()

# ── Output folder ──────────────────────────────────────────────────────────────
run_folder     <- tolower(format(Sys.Date(), "%B_%y"))
dir_leadership <- file.path(OUTPUT_BASE, run_folder, "leadership")
dir.create(dir_leadership, recursive = TRUE, showWarnings = FALSE)

# ── Fetch data once ────────────────────────────────────────────────────────────
message("Fetching faculty data from REDCap...")
faculty_data <- download_faculty_data()
message("Fetching RDM evaluation data from REDCap...")
rdm_data     <- download_rdm_focused()

tmp_faculty_rds <- tempfile(fileext = ".rds")
tmp_rdm_rds     <- tempfile(fileext = ".rds")
saveRDS(faculty_data, tmp_faculty_rds)
saveRDS(rdm_data,     tmp_rdm_rds)
message("  Data cached to temp files")

# ── Render ─────────────────────────────────────────────────────────────────────
out_file    <- file.path(dir_leadership, "Leadership_Dashboard.html")
default_out <- file.path(PROJECT_ROOT, "reports", "pd_report.html")

if (file.exists(default_out)) file.remove(default_out)

message("Rendering Leadership Dashboard...")
ok <- tryCatch({
  quarto::quarto_render(
    input          = "reports/pd_report.qmd",
    execute_params = list(faculty_rds = tmp_faculty_rds, rdm_rds = tmp_rdm_rds),
    execute_dir    = PROJECT_ROOT
  )
  TRUE
}, error = function(e) {
  message("ERROR: ", e$message)
  FALSE
})

if (ok && file.exists(default_out)) {
  file.copy(default_out, out_file, overwrite = TRUE)
  file.remove(default_out)
  message(sprintf("\nDone. Report saved to:\n  %s", out_file))
} else if (!ok) {
  message("Render failed — check errors above.")
} else {
  message(sprintf("WARNING: output not found at %s", default_out))
}
