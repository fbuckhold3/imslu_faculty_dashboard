# send_faculty_reports.R
#
# Renders individual, division, and fellowship reports for all active faculty,
# writes a manifest.csv, and saves everything to a local folder that OneDrive
# syncs automatically (no auth required).
#
# Power Automate then reads the OneDrive folder, routes each file using
# manifest.csv, sends emails, and archives the files.
#
# Report types rendered:
#   individual/  — one per faculty/fellow  → sent to that person
#   division/    — one per division        → sent to division/section director
#   fellowship/  — one per division w/fellows → sent to fellowship PD
#
# Usage:
#   source("reports/send_faculty_reports.R")
#
# Required .Renviron vars:
#   FAC_TOKEN, RDM_TOKEN, REDCAP_URL
#
# Optional .Renviron var:
#   OUTPUT_BASE   Full path to OneDrive sync folder (defaults to ~/Desktop/FacultyReports)

# ── Load .Renviron (needed when run via cron — not loaded automatically) ──────
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

library(tidyverse)
library(quarto)

# Change wd to project root so QMDs can source("R/utils/...")
if (!file.exists("reports/faculty_report.qmd")) {
  stop("Run this script from the imslu_faculty_dashboard project root.")
}

source("R/utils/data_processing.R")

# ── Configuration ─────────────────────────────────────────────────────────────

# Where to write output (set OUTPUT_BASE in .Renviron to your OneDrive sync path)
# Default: e.g. "~/Library/CloudStorage/OneDrive-SaintLouisUniversity/Faculty Reports/pending"
OUTPUT_BASE <- "/Users/home_base/Developer/outputs"

# Project root — passed to quarto as execute_dir so source("R/utils/...") works
PROJECT_ROOT <- getwd()

# Set TRUE to skip rendering and just print routing — fast check before a real run
TEST_MODE <- FALSE

# Division label lookup (matches REDCap data dictionary)
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

# ── Output folders ────────────────────────────────────────────────────────────
# Top-level folder named month_yr, e.g. "june_26"
run_folder     <- tolower(format(Sys.Date(), "%B_%y"))   # e.g. "june_26"
run_dir        <- file.path(OUTPUT_BASE, run_folder)
dir_individual <- file.path(run_dir, "individual")
dir_division   <- file.path(run_dir, "division")
dir_fellowship <- file.path(run_dir, "fellowship")
for (d in c(dir_individual, dir_division, dir_fellowship)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Fetch all data once (passed to QMDs via temp RDS — no subprocess API calls) ─
message("Fetching faculty data from REDCap...")
faculty_data <- download_faculty_data()
message("Fetching RDM evaluation data from REDCap...")
rdm_data     <- download_rdm_focused()

# Save to temp files so QMD subprocesses can load instead of re-fetching
tmp_faculty_rds <- tempfile(fileext = ".rds")
tmp_rdm_rds     <- tempfile(fileext = ".rds")
saveRDS(faculty_data, tmp_faculty_rds)
saveRDS(rdm_data,     tmp_rdm_rds)
message(sprintf("  Data cached to temp files for subprocess use"))

active <- faculty_data |>
  filter(
    archived == 0 | is.na(archived),
    record_id != 1,                          # skip header/test record
    !is.na(fac_name), nchar(trimws(fac_name)) > 0,
    !fac_name %in% c("Pending", "pending")   # skip queue placeholder records
  ) |>
  mutate(
    fac_name  = clean_faculty_names(fac_name),
    fac_div   = as.character(fac_div),
    div_label = DIV_LABELS[fac_div]
  ) |>
  filter(!is.na(fac_email), nchar(trimws(fac_email)) > 0)  # must have an email

faculty <- active |> filter(fac_fell == 1 | is.na(fac_fell))
fellows <- active |> filter(fac_fell == 2)

# Directors: fac_admin == 1, one per division
directors <- active |>
  filter(fac_admin == 1) |>
  select(fac_div, director_email = fac_email, director_name = fac_name)

# Fellowship PDs: fac_med_ed___5 == 1, one per division
pds <- active |>
  filter(fac_med_ed___5 == 1) |>
  select(fac_div, pd_email = fac_email, pd_name = fac_name)

message(sprintf("  %d faculty, %d fellows, %d directors, %d fellowship PDs",
                nrow(faculty), nrow(fellows), nrow(directors), nrow(pds)))

# ── Helper: render one QMD ────────────────────────────────────────────────────
render_report <- function(qmd, out_file, params_list, label) {
  if (TEST_MODE) {
    message(sprintf("  [TEST] would render: %s -> %s", qmd, basename(out_file)))
    return(TRUE)
  }
  message(sprintf("  Rendering: %s", label))

  # Quarto places output next to the input QMD (same dir, .html extension).
  # We render there, then move to the desired output folder.
  # Quarto outputs next to the QMD — we move it after
  default_out <- file.path(
    PROJECT_ROOT, dirname(qmd),
    paste0(tools::file_path_sans_ext(basename(qmd)), ".html")
  )
  if (file.exists(default_out)) file.remove(default_out)

  ok <- tryCatch({
    quarto::quarto_render(
      input          = qmd,
      execute_params = params_list,
      # Run the qmd with wd anchored to the project root, so relative
      # paths inside the qmd (data/*.rds caches, here::here() anchors)
      # resolve the same way they would when sourced directly. Matches
      # the pattern used by render_all_reports.R.
      execute_dir    = PROJECT_ROOT
      # no output_file — quarto rejects paths; file is moved after
    )
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

# ── Manifest accumulator ──────────────────────────────────────────────────────
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

# ── 1. Individual faculty reports ─────────────────────────────────────────────
message("\n--- Individual faculty reports ---")

for (i in seq_len(nrow(faculty))) {
  row      <- faculty[i, ]
  filename <- paste0(safe_filename(row$fac_name), "_report.html")
  out_file <- file.path(dir_individual, filename)

  ok <- render_report(
    qmd         = "reports/faculty_report.qmd",
    out_file    = out_file,
    params_list = list(record_id   = row$record_id,
                       faculty_rds = tmp_faculty_rds,
                       rdm_rds     = tmp_rdm_rds),
    label       = row$fac_name
  )

  if (ok) {
    add_manifest(
      filename    = filename,
      to_email    = row$fac_email,
      to_name     = row$fac_name,
      cc_email    = NA,
      cc_name     = NA,
      report_type = "individual_faculty",
      division    = row$div_label,
      subfolder   = "individual"
    )
  }
}

# ── 2. Individual fellow reports ──────────────────────────────────────────────
message("\n--- Individual fellow reports ---")

for (i in seq_len(nrow(fellows))) {
  row      <- fellows[i, ]
  filename <- paste0(safe_filename(row$fac_name), "_report.html")
  out_file <- file.path(dir_individual, filename)

  ok <- render_report(
    qmd         = "reports/faculty_report.qmd",
    out_file    = out_file,
    params_list = list(record_id   = row$record_id,
                       faculty_rds = tmp_faculty_rds,
                       rdm_rds     = tmp_rdm_rds),
    label       = paste("Fellow:", row$fac_name)
  )

  if (ok) {
    add_manifest(
      filename    = filename,
      to_email    = row$fac_email,
      to_name     = row$fac_name,
      cc_email    = NA,
      cc_name     = NA,
      report_type = "individual_fellow",
      division    = row$div_label,
      subfolder   = "individual"
    )
  }
}

# ── 3. Division summary reports (one per division with active faculty) ─────────
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
    add_manifest(
      filename    = filename,
      to_email    = dir_email,
      to_name     = dir_name,
      cc_email    = NA,
      cc_name     = NA,
      report_type = "division_summary",
      division    = div_row$div_label,
      subfolder   = "division"
    )
  }
}

# ── 4. Fellowship summary reports (one per division with active fellows) ───────
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
    add_manifest(
      filename    = filename,
      to_email    = pd_email,
      to_name     = pd_name,
      cc_email    = NA,
      cc_name     = NA,
      report_type = "fellowship_summary",
      division    = div_row$div_label,
      subfolder   = "fellowship"
    )
  }
}

# ── 5. PD report (all fellows, one file for the dept) ────────────────────────
message("\n--- PD report (all fellows) ---")

dir_leadership <- file.path(run_dir, "leadership")
dir.create(dir_leadership, recursive = TRUE, showWarnings = FALSE)

pd_filename <- "Leadership_Dashboard.html"
pd_out_file <- file.path(dir_leadership, pd_filename)

pd_ok <- render_report(
  qmd         = "reports/pd_report.qmd",
  out_file    = pd_out_file,
  params_list = list(
    faculty_rds = tmp_faculty_rds,
    rdm_rds     = tmp_rdm_rds
  ),
  label = "Fellowship PD Report"
)

# NOTE: deliberately NOT added to the manifest. The leadership dashboard is
# Fred's own view — it gets rendered to leadership/ but is sent manually,
# not by the Power Automate flow. (Decision 2026-06-10.)

# ── 6. Write manifest ─────────────────────────────────────────────────────────
# distinct() guards against duplicate roster records (same person, two record_ids,
# e.g. dual appointments) producing duplicate emails to the same address.
manifest <- bind_rows(manifest_rows) |>
  mutate(.email_key = tolower(trimws(to_email))) |>
  distinct(subfolder, filename, .email_key, .keep_all = TRUE) |>
  select(-.email_key)
manifest_path <- file.path(run_dir, "manifest.csv")
write_csv(manifest, manifest_path)

# Excel manifest with a named table — required by the Power Automate flow,
# which uses Excel Online's "List rows present in a table" (table: "Manifest").
xlsx_path <- file.path(run_dir, "manifest.xlsx")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "manifest")
openxlsx::writeDataTable(wb, "manifest", manifest, tableName = "Manifest")
openxlsx::setColWidths(wb, "manifest", cols = seq_along(manifest), widths = "auto")
openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
message(sprintf("Excel manifest (table 'Manifest') written to:\n  %s", xlsx_path))

message(sprintf("\nDone. %d files + manifest written to:\n  %s", nrow(manifest), run_dir))
message("Upload the ", run_folder, "/ folder to OneDrive, then trigger your Power Automate flow.")
