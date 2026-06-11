# render_all_reports.R
#
# Renders one self-contained HTML teaching report per active faculty member
# and writes a Power Automate manifest CSV with recipient emails.
#
# USAGE (run from project root):
#   Rscript reports/render_all_reports.R
#   Rscript reports/render_all_reports.R "Buckhold"     # single faculty (name match)
#
# PRODUCTION:
#   PRODUCTION_MODE=TRUE Rscript reports/render_all_reports.R
#
# OUTPUT DIRECTORY:
#   Default: reports/output/
#   Override: set OUTPUT_DIR env var to a SharePoint-synced OneDrive path, e.g.:
#     OUTPUT_DIR="/Users/you/OneDrive - SSM Health/Faculty Reports" Rscript ...
#
# DEPENDENCIES: quarto R package (`install.packages("quarto")`)

suppressPackageStartupMessages({
  library(tidyverse)
  library(quarto)
})

# ── Data loading ───────────────────────────────────────────────────────────────
source("R/utils/data_processing.R")

if (Sys.getenv("PRODUCTION_MODE") == "TRUE") {
  cat("Production mode: pulling from REDCap...\n")
  faculty_data <- download_faculty_data()
} else {
  cat("Development mode: loading cached test data...\n")
  td           <- load_test_data()
  faculty_data <- td$faculty
}

# ── CC routing ────────────────────────────────────────────────────────────────
# Faculty  (fac_fell == 1) → CC the division admin (fac_admin == "Yes", same fac_div)
# Fellows  (fac_fell == 2) → CC the fellowship PD (fac_med_ed___5 == 1, same fac_div)

# Division head lookup: fac_div → semicolon-separated admin emails
div_head_lookup <- faculty_data |>
  filter((archived == 0 | is.na(archived)),
         fac_admin == "Yes",
         !is.na(fac_email), fac_email != "") |>
  group_by(fac_div) |>
  summarise(div_head_email = paste(unique(fac_email), collapse = ";"),
            .groups = "drop")

# Fellowship PD lookup: fac_div → PD email (fac_med_ed___5 == 1)
fellowship_pd_lookup <- faculty_data |>
  filter((archived == 0 | is.na(archived)),
         fac_med_ed___5 %in% c(1, "1"),
         !is.na(fac_email), fac_email != "") |>
  group_by(fac_div) |>
  summarise(fellowship_pd_email = paste(unique(fac_email), collapse = ";"),
            .groups = "drop")

# ── Build faculty + fellow list ────────────────────────────────────────────────
active_faculty <- faculty_data |>
  filter(archived == 0 | is.na(archived)) |>           # active only
  filter(fac_fell %in% c(1, 2) | is.na(fac_fell)) |>  # faculty (1) AND fellows (2)
  filter(!is.na(fac_name), fac_name != "") |>
  left_join(div_head_lookup,    by = "fac_div") |>
  left_join(fellowship_pd_lookup, by = "fac_div") |>
  mutate(
    cc_email = case_when(
      fac_fell == 2 ~ fellowship_pd_email,   # fellows → fellowship PD in same division
      TRUE          ~ div_head_email          # faculty → division admin
    )
  )

# Optional: single-name filter for testing
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  name_filter    <- args[1]
  active_faculty <- active_faculty |>
    filter(str_detect(fac_name, regex(name_filter, ignore_case = TRUE)))
  cat(sprintf("Filtering to %d faculty matching '%s'\n", nrow(active_faculty), name_filter))
}

if (nrow(active_faculty) == 0) stop("No faculty found. Check filters or test data.")

# ── Output directory ───────────────────────────────────────────────────────────
# Set OUTPUT_DIR env var to point at a SharePoint-synced OneDrive folder.
out_dir <- Sys.getenv("OUTPUT_DIR", unset = "reports/output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Rendering %d reports into: %s\n\n", nrow(active_faculty), out_dir))

# ── Render loop ────────────────────────────────────────────────────────────────
results <- tibble(
  record_id   = integer(),
  fac_name    = character(),
  fac_type    = character(),   # "Faculty" or "Fellow"
  fac_email   = character(),
  cc_email    = character(),   # division head (faculty) or fellowship PD (fellow)
  status      = character(),
  output_file = character(),
  rendered_at = character()
)

project_root <- getwd()

for (i in seq_len(nrow(active_faculty))) {
  fac          <- active_faculty[i, ]
  safe_name    <- gsub("[^a-zA-Z0-9]", "_", trimws(fac$fac_name))
  out_filename <- sprintf("%s_teaching_report_%s.html", safe_name, Sys.Date())
  out_path     <- file.path(out_dir, out_filename)

  cat(sprintf("[%d/%d] %-35s", i, nrow(active_faculty), fac$fac_name))

  tryCatch({
    quarto::quarto_render(
      input          = "reports/faculty_report.qmd",
      execute_params = list(record_id = as.integer(fac$record_id)),
      execute_dir    = project_root,
      output_file    = out_filename,
      quiet          = TRUE
    )

    # quarto_render drops the file next to the .qmd (reports/); move it
    rendered_path <- file.path("reports", out_filename)
    if (file.exists(rendered_path)) {
      file.rename(rendered_path, out_path)
    } else if (!file.exists(out_path)) {
      warning("Output file not found after render: ", rendered_path)
    }

    cat("✓\n")
    results <- add_row(results,
      record_id   = as.integer(fac$record_id),
      fac_name    = fac$fac_name,
      fac_type    = if (!is.na(fac$fac_fell) && fac$fac_fell == 2) "Fellow" else "Faculty",
      fac_email   = coalesce(fac$fac_email, NA_character_),
      cc_email    = coalesce(fac$cc_email,  NA_character_),
      status      = "success",
      output_file = out_path,
      rendered_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  }, error = function(e) {
    cat("✗  ERROR:", conditionMessage(e), "\n")
    results <<- add_row(results,
      record_id   = as.integer(fac$record_id),
      fac_name    = fac$fac_name,
      fac_type    = if (!is.na(fac$fac_fell) && fac$fac_fell == 2) "Fellow" else "Faculty",
      fac_email   = coalesce(fac$fac_email, NA_character_),
      cc_email    = coalesce(fac$cc_email,  NA_character_),
      status      = paste("error:", conditionMessage(e)),
      output_file = NA_character_,
      rendered_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
}

# ── Summary ────────────────────────────────────────────────────────────────────
n_ok  <- sum(results$status == "success")
n_err <- nrow(results) - n_ok

cat(sprintf("\n=== Done: %d succeeded, %d failed ===\n", n_ok, n_err))

if (n_err > 0) {
  cat("\nFailed:\n")
  results |>
    filter(status != "success") |>
    select(fac_name, status) |>
    print(n = Inf)
}

# Warn about faculty with no email (PA can't send to them)
no_email <- results |> filter(status == "success", is.na(fac_email) | fac_email == "")
if (nrow(no_email) > 0) {
  cat(sprintf("\nWARNING: %d faculty have no email address — Power Automate will skip them:\n",
              nrow(no_email)))
  print(select(no_email, fac_name), n = Inf)
}

no_cc <- results |> filter(status == "success", is.na(cc_email))
if (nrow(no_cc) > 0) {
  cat(sprintf("\nNOTE: %d people have no CC address — email will be sent without CC:\n",
              nrow(no_cc)))
  print(select(no_cc, fac_name, fac_type), n = Inf)
}

# ── Write Power Automate manifest ──────────────────────────────────────────────
# Power Automate reads this CSV to know: who to email, what to attach, who to CC.
# Column reference for the PA flow:
#   fac_email   → To  (primary recipient — faculty or fellow)
#   cc_email    → CC  (division head for faculty; fellowship PD for fellows;
#                      semicolon-separated if multiple admins in a division)
#   fac_type    → "Faculty" or "Fellow" — use in subject line / email body if desired
#   output_file → filename of the HTML attachment in the SharePoint folder
#   fac_name    → used in subject line / body text
#   status      → filter rows to "success" before sending
manifest_path <- file.path(out_dir, sprintf("pa_manifest_%s.csv", Sys.Date()))
write_csv(results, manifest_path)
cat(sprintf("\nManifest (for Power Automate): %s\n", manifest_path))
cat(sprintf("Reports folder:               %s/\n", out_dir))
