# refresh_feedback_data.R
#
# Twice-yearly refresh script — pulls all faculty assessments since the
# start of the current academic year, deidentifies them, and runs the v3
# synthesis pipeline for each faculty member (active or archived — anyone
# who authored real feedback) with at least attendfeedback::SYNTH_MIN_EVALUATIONS
# resident evaluations.
#
# WRITES:
#   data/deid_<AY_TAG>.rds            — full deidentified corpus snapshot
#                                       (overwritten on each refresh)
#   data/synthesis_cache.rds          — per-faculty synthesis results
#                                       (grows incrementally; one entry
#                                       per (faculty, prompt_version,
#                                       model, corpus_tag) tuple)
#
# RUN:
#   Rscript data-refresh/refresh_feedback_data.R
#
# COST:
#   ~$0.20 per faculty synthesis call. ~180 active faculty × $0.20 = ~$36
#   per refresh. Cache is keyed on the snapshot tag, so each refresh
#   produces a fresh batch of entries (prior snapshots preserved for
#   audit / future trend views).
#
# PRECONDITIONS:
#   - ~/.Renviron defines RDM_TOKEN, FAC_TOKEN, REDCAP_URL,
#     ANTHROPIC_API_KEY
#   - attendfeedback installed (provides synthesis pipeline +
#     SYNTH_MIN_EVALUATIONS + render helpers + robust archived filter)

suppressPackageStartupMessages({
  library(attendfeedback)
  library(dplyr)
})

# ── Configuration ───────────────────────────────────────────────────────────
# Bump AY_TAG and AY_START_DATE at each academic-year rollover.

AY_TAG        <- "AY2025-2026"
AY_START_DATE <- "2025-07-01"
SNAPSHOT_TAG  <- paste0(AY_TAG, "__", format(Sys.Date(), "%Y-%m-%d"))
DATA_DIR      <- "data"
DEID_PATH     <- file.path(DATA_DIR, paste0("deid_", AY_TAG, ".rds"))
SYNTH_PATH    <- file.path(DATA_DIR, "synthesis_cache.rds")

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# ── 1. Pull + deidentify ────────────────────────────────────────────────────
#
# IMPORTANT: do NOT pass date_from into pull_assessment_data() here. REDCap
# only returns each resident's `name` field on their one-time BASE record row
# (redcap_repeat_instrument is NA there) — `ass_date` lives on the repeating
# "Assessment" instrument, so that base row always has ass_date = NA.
# pull_assessment_data(date_from=...) filters client-side on ass_date and
# drops NA rows, which deletes the only row carrying `name` — before
# deidentify_comments() ever runs its forward-fill (group_by(record_id) |>
# fill(name)). With nothing to fill from, BOTH the record-based and
# roster-based de-id passes silently fail for that resident, corpus-wide.
#
# Confirmed 2026-09-02: this leaked the assessed resident's own name into
# 243/1330 records (18%) of the AY2025-2026__2026-06-09 snapshot, and 13 of
# the 33 already-produced faculty reports had a resident's real name visible
# in the delivered synthesis text (reports already went out — Fred is aware
# and accepted that; this fix is for future refreshes only).
#
# Fix: pull the full corpus with no date filter, deidentify it whole so every
# base name-row survives for the forward-fill, THEN filter to the target
# date range.

message("== Refresh ", SNAPSHOT_TAG, " ==")
message("Pulling full assessment corpus from REDCap (deidentifying before date-filtering)...")

raw       <- attendfeedback::pull_assessment_data()
deid_full <- attendfeedback::deidentify_comments(raw)
deid      <- deid_full |>
  dplyr::filter(!is.na(ass_date), as.Date(ass_date) >= as.Date(AY_START_DATE))
saveRDS(deid, DEID_PATH)

message(sprintf("  %d assessment records (%s onward) → %s",
                nrow(deid), AY_START_DATE, DEID_PATH))

# ── 2. Faculty roster (active + archived) ────────────────────────────────────
# Include archived faculty — someone who has since left or changed roles
# still authored real feedback that's worth synthesizing. Excluding them
# denied synthesis to faculty who cleared the evaluation threshold purely
# because of their current roster status (e.g. Kelvin Pollard, Hayden
# Rotramel in the 2026-06-09 run — both archived, both with real corpora).
# Pull the full roster instead of attendfeedback::pull_active_faculty()'s active-only
# wrapper; still drop blank names to filter out incomplete roster rows.

faculty_roster <- REDCapR::redcap_read(
  redcap_uri   = Sys.getenv("REDCAP_URL"),
  token        = Sys.getenv("FAC_TOKEN"),
  fields       = "fac_name",
  raw_or_label = "raw",
  verbose      = FALSE
)$data

faculty <- faculty_roster |>
  dplyr::filter(!is.na(fac_name), nchar(trimws(fac_name)) > 0) |>
  dplyr::pull(fac_name) |>
  unique()

message(sprintf("  %d faculty in FAC_TOKEN roster (active + archived).", length(faculty)))

# ── 3. Eligibility check — minimum-evaluation threshold ─────────────────────
# Threshold lives in attendfeedback::SYNTH_MIN_EVALUATIONS (default 10). Below this,
# the synthesis isn't meaningful and the qmd shows an explanatory notice
# instead.

counts <- deid |>
  dplyr::filter(ass_faculty %in% faculty) |>
  dplyr::count(ass_faculty, name = "n_evaluations")

eligible <- counts |>
  dplyr::filter(n_evaluations >= attendfeedback::SYNTH_MIN_EVALUATIONS) |>
  dplyr::pull(ass_faculty)

# Faculty in roster but below threshold (under-evaluating)
below_threshold <- counts |>
  dplyr::filter(n_evaluations < attendfeedback::SYNTH_MIN_EVALUATIONS) |>
  dplyr::arrange(n_evaluations)

# Faculty in roster with zero records in the deid corpus
no_records <- setdiff(faculty, counts$ass_faculty)

message(sprintf("  %d eligible faculty (≥ %d evaluations).",
                length(eligible), attendfeedback::SYNTH_MIN_EVALUATIONS))
message(sprintf("  %d below threshold; %d with no evaluations this period.",
                nrow(below_threshold), length(no_records)))

if (nrow(below_threshold) > 0 && nrow(below_threshold) <= 30) {
  message("\n  Below-threshold faculty (n_evaluations):")
  for (i in seq_len(nrow(below_threshold))) {
    message(sprintf("    %-35s  %d", below_threshold$ass_faculty[i],
                    below_threshold$n_evaluations[i]))
  }
}

# ── 4. Synthesize eligible faculty ──────────────────────────────────────────

message(sprintf("\nSynthesizing %d faculty. ETA ~%.0f min at ~30s/call.",
                length(eligible), length(eligible) * 30 / 60))
message(sprintf("Cache: %s", SYNTH_PATH))
message(sprintf("Snapshot tag: %s\n", SNAPSHOT_TAG))

results <- attendfeedback::synthesize_faculty_batch(
  faculty_names = eligible,
  deid_data     = deid,
  cache_path    = SYNTH_PATH,
  corpus_tag    = SNAPSHOT_TAG
)

# ── 5. Report ───────────────────────────────────────────────────────────────

n_success <- sum(!vapply(results, inherits, logical(1), what = "synthesis_error"))
n_failed  <- length(results) - n_success

message(sprintf("\n== Refresh complete: %d succeeded, %d failed ==",
                n_success, n_failed))

if (n_failed > 0) {
  message("\nFailures:")
  for (fac in names(results)) {
    r <- results[[fac]]
    if (inherits(r, "synthesis_error")) {
      message(sprintf("  %-35s  %s", fac, r$error))
    }
  }
}
