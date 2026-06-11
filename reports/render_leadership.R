# render_leadership.R
#
# Renders only the leadership reports — division summaries, fellowship summaries,
# and the Fellowship PD report. Skips all ~195 individual faculty/fellow renders.
#
# Use this for quick re-runs when you only need to refresh the leadership views,
# or when individual reports are already rendered from a prior full run.
#
# Output folder structure (same as send_faculty_reports.R):
#   ~/Desktop/FacultyReports/<month_yr>/
#     division/    — one HTML per division with active faculty
#     fellowship/  — one HTML per division with active fellows
#     leadership/  — Leadership_Dashboard.html
#     manifest.csv — routing table (leadership rows only)
#
# Usage (run from imslu_faculty_dashboard project root):
#   Sys.setenv(PRODUCTION_MODE = "TRUE")
#   source("reports/render_leadership.R")
#
# Required .Renviron vars: FAC_TOKEN, RDM_TOKEN, REDCAP_URL

# ── Load .Renviron (needed when run via cron) ──────────────────────────────────
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

library(tidyverse)
library(quarto)

if (!file.exists("reports/faculty_report.qmd")) {
  stop("Run this script from the imslu_faculty_dashboard project root.")
}

source("R/utils/data_processing.R")

# ── Configuration ─────────────────────────────────────────────────────────────
OUTPUT_BASE  <- "/Users/home_base/Developer/outputs"
PROJECT_ROOT <- getwd()
TEST_MODE    <- FALSE

DIV_LABELS <- c(
  "1"  = "Addiction Medicine",   "2"  = "Allergy",
  "3"  = "Cardiology",           "4"  = "Endocrinology",
  "5"  = "Gastroenterology",     "6"  = "Geriatrics",
  "7"  = "GIM - Hospitalist",    "8"  = "GIM - Primary Care",
  "9"  = "Hematology / Oncology","10" = "Infectious Disease",
  "11" = "Nephrology",           "12" = "Palliative Care",
  "13" = "Pulmonary / Critical Care", "14" = "Rheumatology",
  "15" = "Other"
)

# ── Output folders ─────────────────────────────────────────────────────────────
run_folder     <- tolower(format(Sys.Date(), "%B_%y"))
run_dir        <- file.path(OUTPUT_BASE, run_folder)
dir_division   <- file.path(run_dir, "division")
dir_fellowship <- file.path(run_dir, "fellowship")
dir_leadership <- file.path(run_dir, "leadership")
for (d in c(dir_division, dir_fellowship, dir_leadership)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ── Fetch all data once ────────────────────────────────────────────────────────
message("Fetching faculty data from REDCap...")
faculty_data <- download_faculty_data()
message("Fetching RDM evaluation data from REDCap...")
rdm_data     <- download_rdm_focused()

tmp_faculty_rds <- tempfile(fileext = ".rds")
tmp_rdm_rds     <- tempfile(fileext = ".rds")
saveRDS(faculty_data, tmp_faculty_rds)
saveRDS(rdm_data,     tmp_rdm_rds)
message("  Data cached to temp files")

# ── Active faculty / fellows / directors / PDs ────────────────────────────────
active <- faculty_data |>
  filter(
    archived == 0 | is.na(archived),
    record_id != 1,
    !is.na(fac_name), nchar(trimws(fac_name)) > 0,
    !fac_name %in% c("Pending", "pending")
  ) |>
  mutate(
    fac_name  = clean_faculty_names(fac_name),
    fac_div   = as.character(fac_div),
    div_label = DIV_LABELS[fac_div]
  ) |>
  filter(!is.na(fac_email), nchar(trimws(fac_email)) > 0)

faculty <- active |> filter(fac_fell == 1 | is.na(fac_fell))
fellows <- active |> filter(fac_fell == 2)

directors <- active |>
  filter(fac_admin == 1) |>
  select(fac_div, director_email = fac_email, director_name = fac_name)

pds <- active |>
  filter(fac_med_ed___5 == 1) |>
  select(fac_div, pd_email = fac_email, pd_name = fac_name)

message(sprintf("  %d faculty, %d fellows, %d directors, %d PDs",
                nrow(faculty), nrow(fellows), nrow(directors), nrow(pds)))

# ── Render helper ──────────────────────────────────────────────────────────────
render_report <- function(qmd, out_file, params_list, label) {
  if (TEST_MODE) {
    message(sprintf("  [TEST] %s -> %s", label, basename(out_file)))
    return(TRUE)
  }
  message(sprintf("  Rendering: %s", label))

  default_out <- file.path(
    PROJECT_ROOT, dirname(qmd),
    paste0(tools::file_path_sans_ext(basename(qmd)), ".html")
  )
  if (file.exists(default_out)) file.remove(default_out)

  ok <- tryCatch({
    quarto::quarto_render(input = qmd, execute_params = params_list)
    TRUE
  }, error = function(e) {
    message(sprintf("  ERROR: %s — %s", label, e$message))
    FALSE
  })

  if (ok) {
    if (file.exists(default_out)) {
      file.copy(default_out, out_file, overwrite = TRUE)
      file.remove(default_out)
    } else {
      message(sprintf("  WARNING: output not found at %s", default_out))
      ok <- FALSE
    }
  }
  ok
}

safe_filename <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)

# ── Manifest accumulator ───────────────────────────────────────────────────────
manifest_rows <- list()

add_manifest <- function(filename, to_email, to_name, cc_email, cc_name,
                          report_type, division, subfolder) {
  manifest_rows[[length(manifest_rows) + 1]] <<- tibble(
    filename    = filename,
    subfolder   = subfolder,
    report_type = report_type,
    to_email    = to_email,
    to_name     = to_name,
    cc_email    = if (is.na(cc_email) || nchar(cc_email) == 0) "" else cc_email,
    cc_name     = if (is.na(cc_name)  || nchar(cc_name)  == 0) "" else cc_name,
    division    = division
  )
}

# ── 1. Division summary reports ────────────────────────────────────────────────
message("\n--- Division summary reports ---")

div_faculty_groups <- faculty |>
  filter(!is.na(fac_div), !is.na(div_label)) |>
  group_by(fac_div, div_label) |>
  summarise(n = n(), .groups = "drop")

for (i in seq_len(nrow(div_faculty_groups))) {
  div_row   <- div_faculty_groups[i, ]
  dir_email <- directors |> filter(fac_div == div_row$fac_div) |> pull(director_email) |> first()
  dir_name  <- directors |> filter(fac_div == div_row$fac_div) |> pull(director_name)  |> first()

  if (is.na(dir_email)) {
    message(sprintf("  SKIP division %s — no director email found", div_row$div_label))
    next
  }

  filename <- paste0("Division_", safe_filename(div_row$div_label), "_summary.html")
  out_file <- file.path(dir_division, filename)

  ok <- render_report(
    qmd         = "reports/division_report.qmd",
    out_file    = out_file,
    params_list = list(
      fac_div     = div_row$fac_div,
      report_type = "division",
      div_label   = div_row$div_label,
      faculty_rds = tmp_faculty_rds,
      rdm_rds     = tmp_rdm_rds
    ),
    label = paste("Division:", div_row$div_label)
  )

  if (ok) {
    add_manifest(filename = filename, to_email = dir_email, to_name = dir_name,
                 cc_email = NA, cc_name = NA, report_type = "division_summary",
                 division = div_row$div_label, subfolder = "division")
  }
}

# ── 2. Fellowship summary reports ─────────────────────────────────────────────
message("\n--- Fellowship summary reports ---")

div_fellow_groups <- fellows |>
  filter(!is.na(fac_div), !is.na(div_label)) |>
  group_by(fac_div, div_label) |>
  summarise(n = n(), .groups = "drop")

for (i in seq_len(nrow(div_fellow_groups))) {
  div_row  <- div_fellow_groups[i, ]
  pd_email <- pds |> filter(fac_div == div_row$fac_div) |> pull(pd_email) |> first()
  pd_name  <- pds |> filter(fac_div == div_row$fac_div) |> pull(pd_name)  |> first()

  if (is.na(pd_email)) {
    message(sprintf("  SKIP fellowship %s — no PD email found", div_row$div_label))
    next
  }

  filename <- paste0("Fellowship_", safe_filename(div_row$div_label), "_summary.html")
  out_file <- file.path(dir_fellowship, filename)

  ok <- render_report(
    qmd         = "reports/division_report.qmd",
    out_file    = out_file,
    params_list = list(
      fac_div     = div_row$fac_div,
      report_type = "fellowship",
      div_label   = div_row$div_label,
      faculty_rds = tmp_faculty_rds,
      rdm_rds     = tmp_rdm_rds
    ),
    label = paste("Fellowship:", div_row$div_label)
  )

  if (ok) {
    add_manifest(filename = filename, to_email = pd_email, to_name = pd_name,
                 cc_email = NA, cc_name = NA, report_type = "fellowship_summary",
                 division = div_row$div_label, subfolder = "fellowship")
  }
}

# ── 3. Fellowship PD report (all fellows, one file) ───────────────────────────
message("\n--- Fellowship PD report ---")

pd_filename <- "Leadership_Dashboard.html"
pd_out_file <- file.path(dir_leadership, pd_filename)

pd_ok <- render_report(
  qmd         = "reports/pd_report.qmd",
  out_file    = pd_out_file,
  params_list = list(faculty_rds = tmp_faculty_rds, rdm_rds = tmp_rdm_rds),
  label       = "Fellowship PD Report"
)

if (pd_ok) {
  all_pd_emails <- paste(unique(pds$pd_email[!is.na(pds$pd_email)]), collapse = "; ")
  add_manifest(filename = pd_filename, to_email = all_pd_emails,
               to_name = "Fellowship Program Directors",
               cc_email = NA, cc_name = NA, report_type = "pd_report",
               division = "All Divisions", subfolder = "leadership")
}

# ── Write manifest ─────────────────────────────────────────────────────────────
manifest      <- bind_rows(manifest_rows)
manifest_path <- file.path(run_dir, "manifest_leadership.csv")
write_csv(manifest, manifest_path)

message(sprintf(
  "\nDone. %d leadership files + manifest written to:\n  %s",
  nrow(manifest), run_dir
))
message("Upload the ", run_folder, "/ folder to OneDrive, then trigger your Power Automate flow.")
