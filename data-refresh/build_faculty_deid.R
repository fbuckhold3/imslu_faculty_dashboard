# build_faculty_deid.R
#
# One-off (re-runnable) build of the faculty de-identification crosswalk,
# for the AIMW27 poster. Two products:
#
#   1. A name <-> opaque study-ID crosswalk (data/faculty_crosswalk_CONFIDENTIAL.rds),
#      kept OUT of anything that ships in a deliverable. IDs are assigned once
#      per faculty member and are stable across re-runs (existing mappings are
#      read back and kept; only newly-seen names get new IDs appended).
#
#   2. Two de-identified analysis datasets built from that crosswalk:
#        - data/deid_analysis_AY2025-2026.rds     (from deid_AY2025-2026.rds,
#          the 135-faculty assessment corpus: ass_faculty -> faculty_id,
#          ass_specialty DROPPED — small-cell risk on ~14 divisions with
#          only 1-2 of the 40 eligible faculty each; not worth coarsening
#          or keeping with a disclosure note, per decision 2026-09-07)
#        - data/synthesis_deid_AY2025-2026.rds     (from synthesis_cache.rds,
#          just the AY2025-2026__2026-09-02 snapshot / 40 faculty:
#          .faculty_name -> faculty_id; synthesis_cache.rds itself is left
#          untouched — it still drives the faculty-facing individual reports)
#
# Sanity check: after building the synthesis de-id file, scans every free-text
# field (theme descriptions/examples, growth-pointer recommendation/rationale/
# example_revision, score_explanation, pd_coaching_note, faculty_coaching_note)
# for any of the 135 crosswalk names leaking into the narrative text itself —
# the same concern the resident de-id step (attendfeedback::deidentify_comments)
# already guards against for resident names. See "Name-leak scan" section below
# for why it's a two-tier check (full-name + filtered first-name), not a bare
# last-name scan.
#
# RUN:
#   Rscript data-refresh/build_faculty_deid.R
#
# PRECONDITIONS:
#   - data/deid_AY2025-2026.rds and data/synthesis_cache.rds already exist
#     (produced by data-refresh/refresh_feedback_data.R)
#   - attendfeedback installed

suppressPackageStartupMessages({
  library(attendfeedback)
  library(dplyr)
  library(tibble)
  library(stringr)
})

# ── Configuration ────────────────────────────────────────────────────────────

DATA_DIR           <- "data"
DEID_PATH          <- file.path(DATA_DIR, "deid_AY2025-2026.rds")
SYNTH_PATH         <- file.path(DATA_DIR, "synthesis_cache.rds")
CROSSWALK_PATH     <- file.path(DATA_DIR, "faculty_crosswalk_CONFIDENTIAL.rds")
DEID_ANALYSIS_PATH <- file.path(DATA_DIR, "deid_analysis_AY2025-2026.rds")
SYNTH_DEID_PATH    <- file.path(DATA_DIR, "synthesis_deid_AY2025-2026.rds")
SNAPSHOT_TAG       <- "AY2025-2026__2026-09-02"

stopifnot(file.exists(DEID_PATH), file.exists(SYNTH_PATH))

# ── 1. Build / update the crosswalk ─────────────────────────────────────────
#
# IDs are assigned in alphabetical order of name on first build. On a re-run,
# existing (name -> ID) pairs are preserved untouched and any newly-seen
# names are appended with the next available number — so a person's ID never
# changes across refreshes, even as new faculty are added to later corpora.

build_or_update_crosswalk <- function(faculty_names, path) {
  faculty_names <- unique(faculty_names)

  existing <- if (file.exists(path)) {
    readRDS(path)
  } else {
    tibble::tibble(fac_name = character(0), study_id = character(0))
  }

  new_names <- setdiff(faculty_names, existing$fac_name)
  if (length(new_names) > 0) {
    max_n <- if (nrow(existing) > 0) {
      max(as.integer(sub("^FAC-", "", existing$study_id)))
    } else {
      0L
    }
    new_names <- sort(new_names)
    new_rows <- tibble::tibble(
      fac_name = new_names,
      study_id = sprintf("FAC-%03d", max_n + seq_along(new_names))
    )
    existing <- dplyr::bind_rows(existing, new_rows)
  }

  existing
}

deid_full <- readRDS(DEID_PATH)
all_faculty <- sort(unique(deid_full$ass_faculty))

message(sprintf("== Faculty crosswalk: %d faculty in %s ==", length(all_faculty), DEID_PATH))

crosswalk <- build_or_update_crosswalk(all_faculty, CROSSWALK_PATH)
saveRDS(crosswalk, CROSSWALK_PATH)

message(sprintf("  Crosswalk written: %s (%d faculty total, IDs %s .. %s)",
                CROSSWALK_PATH, nrow(crosswalk),
                min(crosswalk$study_id), max(crosswalk$study_id)))

# ── 2. De-identified assessment-corpus analysis dataset ─────────────────────
# ass_faculty -> faculty_id via the crosswalk; ass_specialty dropped entirely
# (not coarsened, not kept with a disclosure note — small-cell risk decision).

deid_analysis <- deid_full |>
  dplyr::left_join(crosswalk, by = c("ass_faculty" = "fac_name")) |>
  dplyr::select(-ass_faculty, -ass_specialty) |>
  dplyr::rename(faculty_id = study_id) |>
  dplyr::relocate(faculty_id, .after = record_id)

n_unmatched <- sum(is.na(deid_analysis$faculty_id))
if (n_unmatched > 0) {
  stop(sprintf("%d record(s) failed to match a faculty_id — crosswalk join incomplete.",
              n_unmatched))
}

saveRDS(deid_analysis, DEID_ANALYSIS_PATH)
message(sprintf("  Analysis dataset written: %s (%d records, %d faculty, no ass_specialty column)",
                DEID_ANALYSIS_PATH, nrow(deid_analysis),
                dplyr::n_distinct(deid_analysis$faculty_id)))

# ── 3. De-identify the synthesis snapshot (40 faculty) ───────────────────────

synth_cache <- readRDS(SYNTH_PATH)

is_target <- vapply(synth_cache, function(x) identical(x$.corpus_tag, SNAPSHOT_TAG), logical(1))
target <- synth_cache[is_target]

message(sprintf("\n== Synthesis snapshot %s: %d faculty entries ==", SNAPSHOT_TAG, length(target)))

missing_from_crosswalk <- setdiff(
  vapply(target, function(x) x$.faculty_name, character(1)),
  crosswalk$fac_name
)
if (length(missing_from_crosswalk) > 0) {
  stop("Faculty in synthesis snapshot missing from crosswalk: ",
      paste(missing_from_crosswalk, collapse = ", "))
}

synth_deid <- lapply(target, function(entry) {
  entry$.faculty_name <- crosswalk$study_id[match(entry$.faculty_name, crosswalk$fac_name)]
  entry
})
names(synth_deid) <- vapply(synth_deid, function(e) {
  paste(e$.faculty_name, e$.prompt_version, e$.model, e$.corpus_tag, sep = "::")
}, character(1))

saveRDS(synth_deid, SYNTH_DEID_PATH)
message(sprintf("  De-identified synthesis written: %s (%d faculty)",
                SYNTH_DEID_PATH, length(synth_deid)))
message(sprintf("  (synthesis_cache.rds left untouched — still drives faculty-facing reports)"))

# ── 4. Name-leak scan ─────────────────────────────────────────────────────
#
# Two-tier check, mirroring attendfeedback::deidentify_comments()'s design
# for resident names:
#
#   (a) Full "First Last" scan against all 135 crosswalk names. Low false-
#       positive risk since it requires both tokens adjacent.
#
#   (b) First-name-only roster scan via attendfeedback::build_resident_first_names()
#       + apply_roster_scan(), which already excludes ambiguous first names
#       that double as common English words (its AMBIGUOUS_FIRST_NAMES
#       denylist — "Will", "Mark", "May", "Grace", "Hope", ... — and
#       "Mark Dykewicz" is in fact one of our 135 faculty, so this filter
#       is load-bearing here, not theoretical).
#
# Deliberately NOT scanning bare last names: several of the 135 surnames are
# themselves common English words (Go, Moon, Wood, Cole, ...) and a bare
# scan would drown real hits in false positives. This matches the original
# pipeline's choice to roster-scan only first names, never last names alone.

extract_free_text <- function(entry) {
  theme_text <- unlist(lapply(entry$themes, function(t) c(t$description, t$example)))
  growth_text <- unlist(lapply(entry$growth_pointers, function(g) {
    c(g$recommendation, g$rationale, g$example_revision)
  }))
  c(
    entry$faculty_summary$score_explanation,
    theme_text,
    growth_text,
    entry$pd_coaching_note,
    entry$faculty_coaching_note
  )
}

all_names <- crosswalk$fac_name

# "TEST Faculty" is dummy roster data, not a real person to protect — and its
# first name "TEST" collides with the common clinical word "test" ("order a
# test", "one test or finding"), which would otherwise flood the roster scan
# with false positives. Exclude it from the first-name roster only; it's
# still scanned normally by the full-name check.
roster_source <- setdiff(all_names, "TEST Faculty")
roster_firsts <- attendfeedback::build_resident_first_names(roster_source)

full_name_hits  <- tibble::tibble(faculty_id = character(0), field = character(0),
                                  matched_name = character(0), snippet = character(0))
roster_hits <- tibble::tibble(faculty_id = character(0), field = character(0),
                              matched_first = character(0), snippet = character(0))

field_labels <- function(entry) {
  c(
    "faculty_summary$score_explanation",
    rep("themes[].description/example", length(unlist(lapply(entry$themes, function(t) c(t$description, t$example))))),
    rep("growth_pointers[].rec/rationale/revision",
        length(unlist(lapply(entry$growth_pointers, function(g) c(g$recommendation, g$rationale, g$example_revision))))),
    "pd_coaching_note",
    "faculty_coaching_note"
  )
}

for (entry in synth_deid) {
  texts  <- extract_free_text(entry)
  labels <- field_labels(entry)
  texts  <- texts[!is.na(texts)]
  labels <- labels[seq_along(texts)]

  for (i in seq_along(texts)) {
    txt <- texts[i]

    # (a) full "First Last" scan
    for (nm in all_names) {
      if (stringr::str_detect(txt, stringr::regex(paste0("\\b\\Q", nm, "\\E\\b"), ignore_case = TRUE))) {
        full_name_hits <- dplyr::bind_rows(full_name_hits, tibble::tibble(
          faculty_id = entry$.faculty_name, field = labels[i],
          matched_name = nm, snippet = substr(txt, 1, 200)
        ))
      }
    }

    # (b) filtered first-name roster scan
    scan <- attendfeedback::apply_roster_scan(txt, roster_firsts, replacement = "[FACULTY]")
    if (scan$replaced) {
      roster_hits <- dplyr::bind_rows(roster_hits, tibble::tibble(
        faculty_id = entry$.faculty_name, field = labels[i],
        matched_first = paste(scan$matches, collapse = ", "), snippet = substr(txt, 1, 200)
      ))
    }
  }
}

n_leaks <- nrow(full_name_hits) + nrow(roster_hits)

message(sprintf("\n== Name-leak scan: %d full-name hit(s), %d filtered-first-name hit(s) ==",
                nrow(full_name_hits), nrow(roster_hits)))

if (n_leaks > 0) {
  message("  FULL-NAME HITS:")
  if (nrow(full_name_hits) > 0) print(full_name_hits, n = Inf)
  message("  FIRST-NAME (roster) HITS:")
  if (nrow(roster_hits) > 0) print(roster_hits, n = Inf)
} else {
  message("  Clean — 0 real faculty names detected in any free-text field.")
}

# ── 5. Summary ───────────────────────────────────────────────────────────────

message("\n== Summary ==")
message(sprintf("  Crosswalk:            %s (%d faculty)", CROSSWALK_PATH, nrow(crosswalk)))
message(sprintf("  Assessment analysis:  %s (%d records, ass_specialty dropped)",
                DEID_ANALYSIS_PATH, nrow(deid_analysis)))
message(sprintf("  Synthesis de-id:      %s (%d faculty)", SYNTH_DEID_PATH, length(synth_deid)))
message(sprintf("  Name leaks found:     %d", n_leaks))
