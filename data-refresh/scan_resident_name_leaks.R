# scan_resident_name_leaks.R
#
# Follow-up to build_faculty_deid.R (2026-09-07). That script's faculty
# name-leak scan incidentally surfaced a bare first name ("Patrick") in
# FAC-097's theme example inside data/synthesis_deid_AY2025-2026.rds — it
# matched a faculty first name (Patrick Keller) by coincidence, but the
# text is actually about a resident, not that faculty member.
#
# This script:
#   1. Redacts that one instance to [RESIDENT] in the de-identified
#      synthesis file (does NOT touch synthesis_cache.rds, the identified
#      original still driving faculty-facing reports).
#   2. Pulls the real resident-name roster from RDM (production, RDM_TOKEN),
#      scoped to just the record_ids already present in this corpus
#      (privacy-minimizing — no need for the full resident roster), and
#      re-scans EVERY free-text field in both:
#        - data/synthesis_deid_AY2025-2026.rds  (40 faculty, narrative text)
#        - data/deid_analysis_AY2025-2026.rds   (1677 records, ass_plus_clean/
#          ass_delta_clean — the underlying corpus the narratives were built
#          from)
#      using the same two-tier approach as the faculty scan: full "First
#      Last" match (low false-positive risk) + filtered first-name roster
#      scan (attendfeedback::build_resident_first_names() denylists common-
#      word first names).
#
# RUN:
#   Rscript data-refresh/scan_resident_name_leaks.R
#
# PRECONDITIONS:
#   - data/synthesis_deid_AY2025-2026.rds and data/deid_analysis_AY2025-2026.rds
#     already exist (from build_faculty_deid.R)
#   - ~/.Renviron: RDM_TOKEN, REDCAP_URL

suppressPackageStartupMessages({
  library(attendfeedback)
  library(dplyr)
  library(tibble)
  library(stringr)
})

DATA_DIR           <- "data"
SYNTH_DEID_PATH    <- file.path(DATA_DIR, "synthesis_deid_AY2025-2026.rds")
DEID_ANALYSIS_PATH <- file.path(DATA_DIR, "deid_analysis_AY2025-2026.rds")

stopifnot(file.exists(SYNTH_DEID_PATH), file.exists(DEID_ANALYSIS_PATH))

# ── 1. Redact the known leak ─────────────────────────────────────────────────

synth_deid <- readRDS(SYNTH_DEID_PATH)

target_key <- grep("^FAC-097::", names(synth_deid), value = TRUE)
stopifnot(length(target_key) == 1)

before <- synth_deid[[target_key]]$themes[[1]]$example
redacted <- attendfeedback::apply_roster_scan(before, "Patrick", replacement = "[RESIDENT]")

if (redacted$replaced) {
  synth_deid[[target_key]]$themes[[1]]$example <- redacted$text
  saveRDS(synth_deid, SYNTH_DEID_PATH)
  message("== Redaction ==")
  message("  BEFORE: ", before)
  message("  AFTER:  ", redacted$text)
  message(sprintf("  Written back to %s\n", SYNTH_DEID_PATH))
} else {
  message("== Redaction ==")
  message("  Already redacted (no 'Patrick' found) — skipping, file unchanged.\n")
}

# ── 2. Pull the resident roster for record_ids in this corpus ───────────────

deid_analysis <- readRDS(DEID_ANALYSIS_PATH)
record_ids <- sort(unique(deid_analysis$record_id))

message(sprintf("== Pulling resident names for %d record_id(s) in the corpus (RDM_TOKEN, production) ==",
                length(record_ids)))

roster_pull <- REDCapR::redcap_read(
  redcap_uri   = Sys.getenv("REDCAP_URL"),
  token        = Sys.getenv("RDM_TOKEN"),
  fields       = c("record_id", "name"),
  records      = as.character(record_ids),
  raw_or_label = "raw",
  verbose      = FALSE
)
if (!isTRUE(roster_pull$success)) {
  stop("RDM_TOKEN pull failed: ", roster_pull$outcome_message)
}

resident_names_raw <- roster_pull$data |>
  tibble::as_tibble() |>
  dplyr::filter(!is.na(name), nchar(trimws(name)) > 0) |>
  dplyr::pull(name) |>
  unique()

# record_id 2039 resolves to the literal name "test" — dummy/placeholder
# data, not a real resident. Same problem as "TEST Faculty" in
# build_faculty_deid.R: "test" is also an ordinary clinical word ("order a
# test", "one test or finding"), so leaving it in floods both scan tiers
# with false positives. Exclude it from the scan roster.
resident_names <- resident_names_raw[!tolower(resident_names_raw) %in% c("test", "test test")]
n_excluded <- length(resident_names_raw) - length(resident_names)

message(sprintf("  %d unique resident name(s) resolved (%d excluded as dummy/placeholder).\n",
                length(resident_names), n_excluded))

# ── 3. Two-tier scan (mirrors the faculty scan in build_faculty_deid.R) ─────

roster_firsts <- attendfeedback::build_resident_first_names(resident_names)

extract_synth_text <- function(entry) {
  theme_text <- unlist(lapply(entry$themes, function(t) c(t$description, t$example)))
  growth_text <- unlist(lapply(entry$growth_pointers, function(g) {
    c(g$recommendation, g$rationale, g$example_revision)
  }))
  c(entry$faculty_summary$score_explanation, theme_text, growth_text,
    entry$pd_coaching_note, entry$faculty_coaching_note)
}

scan_texts <- function(texts, source_labels, id_labels) {
  full_hits   <- tibble::tibble(id = character(0), field = character(0),
                                matched_name = character(0), snippet = character(0))
  roster_hits <- tibble::tibble(id = character(0), field = character(0),
                                matched_first = character(0), snippet = character(0))
  for (i in seq_along(texts)) {
    txt <- texts[i]
    if (is.na(txt) || !nzchar(txt)) next

    for (nm in resident_names) {
      if (stringr::str_detect(txt, stringr::regex(paste0("\\b\\Q", nm, "\\E\\b"), ignore_case = TRUE))) {
        full_hits <- dplyr::bind_rows(full_hits, tibble::tibble(
          id = id_labels[i], field = source_labels[i], matched_name = nm, snippet = substr(txt, 1, 200)
        ))
      }
    }

    scan <- attendfeedback::apply_roster_scan(txt, roster_firsts, replacement = "[RESIDENT]")
    if (scan$replaced) {
      roster_hits <- dplyr::bind_rows(roster_hits, tibble::tibble(
        id = id_labels[i], field = source_labels[i],
        matched_first = paste(scan$matches, collapse = ", "), snippet = substr(txt, 1, 200)
      ))
    }
  }
  list(full = full_hits, roster = roster_hits)
}

# -- 3a. Synthesis narrative text (post-redaction) --

synth_texts <- character(0)
synth_labels <- character(0)
synth_ids <- character(0)
for (entry in synth_deid) {
  txts <- extract_synth_text(entry)
  synth_texts <- c(synth_texts, txts)
  synth_labels <- c(synth_labels, rep("synthesis narrative", length(txts)))
  synth_ids <- c(synth_ids, rep(entry$.faculty_name, length(txts)))
}
synth_scan <- scan_texts(synth_texts, synth_labels, synth_ids)

# -- 3b. Underlying assessment corpus (ass_plus_clean / ass_delta_clean) --

corpus_texts <- c(deid_analysis$ass_plus_clean, deid_analysis$ass_delta_clean)
corpus_labels <- c(rep("ass_plus_clean", nrow(deid_analysis)), rep("ass_delta_clean", nrow(deid_analysis)))
corpus_ids <- c(paste0("faculty=", deid_analysis$faculty_id, " record=", deid_analysis$record_id),
                paste0("faculty=", deid_analysis$faculty_id, " record=", deid_analysis$record_id))
corpus_scan <- scan_texts(corpus_texts, corpus_labels, corpus_ids)

# ── 4. Report ────────────────────────────────────────────────────────────────

message("== Resident-name leak scan: synthesis_deid_AY2025-2026.rds ==")
message(sprintf("  Full-name hits: %d | Filtered first-name hits: %d",
                nrow(synth_scan$full), nrow(synth_scan$roster)))
if (nrow(synth_scan$full) > 0)   { message("  FULL-NAME HITS:");   print(synth_scan$full, n = Inf) }
if (nrow(synth_scan$roster) > 0) { message("  FIRST-NAME HITS:"); print(synth_scan$roster, n = Inf) }

message("\n== Resident-name leak scan: deid_analysis_AY2025-2026.rds (underlying corpus) ==")
message(sprintf("  Full-name hits: %d | Filtered first-name hits: %d",
                nrow(corpus_scan$full), nrow(corpus_scan$roster)))
if (nrow(corpus_scan$full) > 0)   { message("  FULL-NAME HITS:");   print(corpus_scan$full, n = Inf) }
if (nrow(corpus_scan$roster) > 0) { message("  FIRST-NAME HITS:"); print(corpus_scan$roster, n = Inf) }

total_hits <- nrow(synth_scan$full) + nrow(synth_scan$roster) +
              nrow(corpus_scan$full) + nrow(corpus_scan$roster)
message(sprintf("\n== TOTAL remaining resident-name leak candidates: %d ==", total_hits))
