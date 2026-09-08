# refresh_feedback_data.R
#
# Twice-yearly refresh script — pulls all faculty assessments since the
# start of the current academic year, deidentifies them, and runs the v3
# synthesis pipeline for each faculty member (active or archived — anyone
# who authored real feedback) with at least attendfeedback::SYNTH_MIN_EVALUATIONS
# resident evaluations.
#
# 2026-09-08: rewritten to call attendfeedback::run_deidentification()
# instead of a hand-rolled pull+deidentify dance. That one function now
# handles what this script used to do by hand: pulling assessment data
# with no premature date filter (see the 2026-09-02 incident this script
# used to document at length), pulling a FRESH resident roster on every
# run (so new residents are covered automatically, no code change
# needed), nickname-variant expansion, and known pooled/rotator record_id
# redaction — all on by default. Faculty cross-mention scanning is left
# OFF here (`scan_faculty = FALSE`) since this corpus feeds faculty-facing
# reports where real faculty names are meant to show — see
# `attendfeedback/data-refresh` in the research-dataset script for where
# that scan is actually used.
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
#   - attendfeedback installed (provides the de-id + synthesis pipeline,
#     SYNTH_MIN_EVALUATIONS, and render helpers)

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

# ── 1. Pull + deidentify ─────────────────────────────────────────────────────
# Fresh resident roster, nickname expansion, and pooled-record redaction
# are all handled internally — see run_deidentification()'s docs.

message("== Refresh ", SNAPSHOT_TAG, " ==")

deid_run <- run_deidentification(
  scan_faculty = FALSE,   # faculty-facing reports show real faculty names
                          # by design; the faculty cross-mention scan is
                          # for the de-identified research export instead
  date_from    = AY_START_DATE
)

deid <- deid_run$data
saveRDS(deid, DEID_PATH)

message(sprintf("  %d assessment records (%s onward) -> %s",
                nrow(deid), AY_START_DATE, DEID_PATH))

if (nrow(deid_run$report) > 0) {
  message(sprintf("  %d row(s) flagged for review (resident-name gaps / pooled-record buckets) -- see deid_run$report",
                  nrow(deid_run$report)))
}

# ── 2. Faculty roster (active + archived) ────────────────────────────────────
# Include archived faculty — someone who has since left or changed roles
# still authored real feedback that's worth synthesizing. Excluding them
# denied synthesis to faculty who cleared the evaluation threshold purely
# because of their current roster status (e.g. Kelvin Pollard, Hayden
# Rotramel in the 2026-06-09 run — both archived, both with real corpora).

faculty <- pull_active_faculty(include_archived = TRUE)

message(sprintf("  %d faculty in FAC_TOKEN roster (active + archived).", length(faculty)))

# ── 3. Eligibility check — minimum-evaluation threshold ─────────────────────
# Threshold lives in attendfeedback::SYNTH_MIN_EVALUATIONS (default 10). Below this,
# the synthesis isn't meaningful and the qmd shows an explanatory notice
# instead.

counts <- deid |>
  dplyr::filter(ass_faculty %in% faculty) |>
  dplyr::count(ass_faculty, name = "n_evaluations")

eligible <- counts |>
  dplyr::filter(n_evaluations >= SYNTH_MIN_EVALUATIONS) |>
  dplyr::pull(ass_faculty)

# Faculty in roster but below threshold (under-evaluating)
below_threshold <- counts |>
  dplyr::filter(n_evaluations < SYNTH_MIN_EVALUATIONS) |>
  dplyr::arrange(n_evaluations)

# Faculty in roster with zero records in the deid corpus
no_records <- setdiff(faculty, counts$ass_faculty)

message(sprintf("  %d eligible faculty (>= %d evaluations).",
                length(eligible), SYNTH_MIN_EVALUATIONS))
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

results <- synthesize_faculty_batch(
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
