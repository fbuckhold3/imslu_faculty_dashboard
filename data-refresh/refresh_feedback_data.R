# refresh_feedback_data.R
#
# Twice-yearly refresh script — pulls all faculty assessments since the
# start of the current academic year, deidentifies them, and runs the v3
# synthesis pipeline for each active faculty member with at least
# gmed::SYNTH_MIN_EVALUATIONS resident evaluations.
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
#   - gmed installed at >= 0.2.2 (provides synthesis pipeline +
#     SYNTH_MIN_EVALUATIONS + render helpers + robust archived filter)

suppressPackageStartupMessages({
  library(gmed)
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

message("== Refresh ", SNAPSHOT_TAG, " ==")
message("Pulling assessments from REDCap (", AY_START_DATE, " onward)...")

raw  <- gmed::pull_assessment_data(date_from = AY_START_DATE)
deid <- gmed::deidentify_comments(raw)
saveRDS(deid, DEID_PATH)

message(sprintf("  %d assessment records → %s",
                nrow(deid), DEID_PATH))

# ── 2. Active faculty roster ─────────────────────────────────────────────────

faculty <- gmed::pull_active_faculty()
message(sprintf("  %d active faculty in FAC_TOKEN roster.", length(faculty)))

# ── 3. Eligibility check — minimum-evaluation threshold ─────────────────────
# Threshold lives in gmed::SYNTH_MIN_EVALUATIONS (default 10). Below this,
# the synthesis isn't meaningful and the qmd shows an explanatory notice
# instead.

counts <- deid |>
  dplyr::filter(ass_faculty %in% faculty) |>
  dplyr::count(ass_faculty, name = "n_evaluations")

eligible <- counts |>
  dplyr::filter(n_evaluations >= gmed::SYNTH_MIN_EVALUATIONS) |>
  dplyr::pull(ass_faculty)

# Faculty in roster but below threshold (under-evaluating)
below_threshold <- counts |>
  dplyr::filter(n_evaluations < gmed::SYNTH_MIN_EVALUATIONS) |>
  dplyr::arrange(n_evaluations)

# Faculty in roster with zero records in the deid corpus
no_records <- setdiff(faculty, counts$ass_faculty)

message(sprintf("  %d eligible faculty (≥ %d evaluations).",
                length(eligible), gmed::SYNTH_MIN_EVALUATIONS))
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

results <- gmed::synthesize_faculty_batch(
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
