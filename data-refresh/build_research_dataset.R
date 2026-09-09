# build_research_dataset.R
#
# Consolidated, re-runnable replacement for the 2026-09-07 one-off sequence
# (build_faculty_deid.R -> scan_resident_name_leaks.R ->
# fix_rotator157_leaks.R), now that the gaps those scripts found by hand
# (nicknames, pooled/rotator records, faculty cross-mentions) are built
# into attendfeedback's core pipeline. Those three scripts are left in
# place as the historical record of how the 2026-09-07 dataset was
# actually produced — this script is what to run instead, going forward
# (originally built for the AIMW27 poster; reusable for any future
# de-identified research export from this corpus).
#
# Products (same as before):
#   1. Name <-> opaque study-ID crosswalk
#      (data/faculty_crosswalk_CONFIDENTIAL.rds) — kept OUT of anything
#      that ships in a deliverable. IDs are stable across re-runs.
#   2. data/deid_analysis_<AY_TAG>.rds — the assessment corpus with
#      ass_faculty -> faculty_id, ass_specialty DROPPED (small-cell risk
#      on ~14 divisions with only a handful of eligible faculty each;
#      decision from 2026-09-07, unchanged).
#   3. data/synthesis_deid_<AY_TAG>.rds — the synthesis snapshot for the
#      target corpus_tag, .faculty_name -> faculty_id.
#
# What's now handled automatically (vs. the three one-off scripts):
#   - Resident name leaks (nicknames, cross-mentions, pooled/rotator
#     records) — attendfeedback::run_deidentification() with
#     scan_faculty = TRUE also runs the faculty cross-mention scan on the
#     INPUT comments in the same call.
#   - Faculty crosswalk substitution + the adversarial free-text sweep
#     (nicknames, "Dr. Lastname", full-name cross-mentions) in one call —
#     attendfeedback::deidentify_faculty(), added 2026-09-09 (see
#     fof_research/ROADMAP.md's "42 leaks" incident note for why the
#     sweep matters: a name-column-only substitution would have missed
#     all of them).
#   - Independent verification (deliberately NOT the same matching code
#     as the redaction) — attendfeedback::verify_faculty_deidentified().
#   - Leaks in the DELIVERED synthesis narrative text (the "Nathan"
#     class of gap — de-identified input, but the model's OUTPUT still
#     said the name) — attendfeedback::scan_synthesis_for_leaks(), run
#     against the de-identified synthesis snapshot below. Report-only —
#     review hits and redact by hand (see fix_rotator157_leaks.R's
#     get_by_path/set_by_path pattern for how) rather than risk garbling
#     AI-generated prose with an automatic substitution.
#
# RUN:
#   Rscript data-refresh/build_research_dataset.R
#
# PRECONDITIONS:
#   - data/synthesis_cache.rds already exists (produced by
#     data-refresh/refresh_feedback_data.R) with an entry for SNAPSHOT_TAG
#   - ~/.Renviron: RDM_TOKEN, FAC_TOKEN, REDCAP_URL
#   - attendfeedback installed

suppressPackageStartupMessages({
  library(attendfeedback)
  library(dplyr)
  library(tibble)
})

# ── Configuration ────────────────────────────────────────────────────────────

DATA_DIR           <- "data"
AY_TAG             <- "AY2025-2026"
AY_START_DATE      <- "2025-07-01"
SNAPSHOT_TAG       <- "AY2025-2026__2026-09-02"  # bump to match the refresh you're building from
SYNTH_PATH         <- file.path(DATA_DIR, "synthesis_cache.rds")
CROSSWALK_PATH     <- file.path(DATA_DIR, "faculty_crosswalk_CONFIDENTIAL.rds")
DEID_ANALYSIS_PATH <- file.path(DATA_DIR, paste0("deid_analysis_", AY_TAG, ".rds"))
SYNTH_DEID_PATH    <- file.path(DATA_DIR, paste0("synthesis_deid_", AY_TAG, ".rds"))

stopifnot(file.exists(SYNTH_PATH))

# ── 1. Pull + deidentify fresh, WITH the faculty cross-mention tier on ──────
#
# Deliberately a fresh pull rather than reusing data/deid_<AY_TAG>.rds:
# that file's ass_plus_clean/ass_delta_clean columns are already
# de-identified output, not the raw ass_plus/ass_delta/name columns
# deidentify_comments() needs — it can't be re-run on its own output.
# refresh_feedback_data.R runs with scan_faculty = FALSE (that corpus
# feeds faculty-facing reports where real faculty names are meant to
# show); this research export needs faculty names protected too.

message("== Building research dataset (", AY_TAG, ") ==")

faculty_roster <- pull_active_faculty(include_archived = TRUE)

deid_run <- run_deidentification(
  scan_faculty  = TRUE,
  faculty_names = faculty_roster,
  date_from     = AY_START_DATE
)
deid <- deid_run$data

message(sprintf("  Re-scanned with faculty cross-mention tier on: %d row(s) flagged for review.",
                nrow(deid_run$report)))

# ── 2. Build / update the faculty crosswalk ─────────────────────────────────

all_faculty <- sort(unique(deid$ass_faculty))
existing_crosswalk <- if (file.exists(CROSSWALK_PATH)) readRDS(CROSSWALK_PATH) else NULL

# Back-compat: the crosswalk file predates build_faculty_crosswalk() and
# used column name `fac_name` instead of `name` — normalize either shape.
if (!is.null(existing_crosswalk) && "fac_name" %in% names(existing_crosswalk)) {
  existing_crosswalk <- existing_crosswalk |> dplyr::rename(name = fac_name)
}

crosswalk <- build_faculty_crosswalk(all_faculty, existing = existing_crosswalk, prefix = "FAC")
saveRDS(crosswalk, CROSSWALK_PATH)

message(sprintf("  Crosswalk: %s (%d faculty total, IDs %s .. %s)",
                CROSSWALK_PATH, nrow(crosswalk), min(crosswalk$study_id), max(crosswalk$study_id)))

# ── 3. De-identified assessment-corpus analysis dataset ─────────────────────
# deidentify_faculty() does the crosswalk substitution AND the adversarial
# free-text sweep (full name / nickname / "Dr. Lastname") on the comment
# columns in one call -- this is the step that actually caught the 42
# leaks in the original ad hoc sweep, now built into the function instead
# of requiring three separate scripts run in the right order by hand.

deid_analysis <- deidentify_faculty(
  deid, crosswalk,
  name_col  = "ass_faculty",
  id_col    = "faculty_id",
  text_cols = c("ass_plus_clean", "ass_delta_clean"),
  drop_cols = "ass_specialty"
)

saveRDS(deid_analysis, DEID_ANALYSIS_PATH)
message(sprintf("  Analysis dataset: %s (%d records, %d faculty, no ass_specialty column)",
                DEID_ANALYSIS_PATH, nrow(deid_analysis), dplyr::n_distinct(deid_analysis$faculty_id)))

# Independent verification -- deliberately NOT the same matching code as
# deidentify_faculty()'s redaction (see verify_faculty_deidentified()'s
# docs for why that matters).
verify_hits <- verify_faculty_deidentified(
  deid_analysis, faculty_names = all_faculty,
  text_cols = c("ass_plus_clean", "ass_delta_clean")
)
message(sprintf("  Independent verification: %d hit(s) on the assessment corpus.", nrow(verify_hits)))
if (nrow(verify_hits) > 0) {
  message("  Review each hit (this is a genuinely separate check from the redaction above):")
  print(verify_hits, n = Inf)
}

# ── 4. De-identify the synthesis snapshot ───────────────────────────────────

synth_cache <- readRDS(SYNTH_PATH)
is_target <- vapply(synth_cache, function(x) identical(x$.corpus_tag, SNAPSHOT_TAG), logical(1))
target <- synth_cache[is_target]

message(sprintf("\n== Synthesis snapshot %s: %d faculty entries ==", SNAPSHOT_TAG, length(target)))

missing_from_crosswalk <- setdiff(
  vapply(target, function(x) x$.faculty_name, character(1)),
  crosswalk$name
)
if (length(missing_from_crosswalk) > 0) {
  stop("Faculty in synthesis snapshot missing from crosswalk: ",
      paste(missing_from_crosswalk, collapse = ", "))
}

synth_deid <- lapply(target, function(entry) {
  entry$.faculty_name <- crosswalk$study_id[match(entry$.faculty_name, crosswalk$name)]
  entry
})
names(synth_deid) <- vapply(synth_deid, function(e) {
  paste(e$.faculty_name, e$.prompt_version, e$.model, e$.corpus_tag, sep = "::")
}, character(1))

saveRDS(synth_deid, SYNTH_DEID_PATH)
message(sprintf("  De-identified synthesis: %s (%d faculty)", SYNTH_DEID_PATH, length(synth_deid)))
message("  (synthesis_cache.rds left untouched -- still drives faculty-facing reports)")

# ── 5. Scan the DELIVERED synthesis text for leaks (report-only) ───────────

resident_roster <- pull_resident_roster()

leak_report <- scan_synthesis_for_leaks(
  synth_deid,
  resident_names = resident_roster,
  faculty_names  = all_faculty
)

message(sprintf("\n== Synthesis narrative leak scan: %d hit(s) ==", nrow(leak_report)))
if (nrow(leak_report) > 0) {
  message("  Review each hit and redact by hand (this step does not auto-redact):")
  print(leak_report, n = Inf)
} else {
  message("  Clean.")
}

# ── 6. Summary ───────────────────────────────────────────────────────────────

message("\n== Summary ==")
message(sprintf("  Crosswalk:            %s (%d faculty)", CROSSWALK_PATH, nrow(crosswalk)))
message(sprintf("  Assessment analysis:  %s (%d records)", DEID_ANALYSIS_PATH, nrow(deid_analysis)))
message(sprintf("  Synthesis de-id:      %s (%d faculty)", SYNTH_DEID_PATH, length(synth_deid)))
message(sprintf("  Input re-scan flags:  %d", nrow(deid_run$report)))
message(sprintf("  Independent verify:   %d", nrow(verify_hits)))
message(sprintf("  Narrative leak hits:  %d", nrow(leak_report)))
