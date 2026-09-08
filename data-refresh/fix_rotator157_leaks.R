# fix_rotator157_leaks.R
#
# Follow-up to build_faculty_deid.R / scan_resident_name_leaks.R (2026-09-07).
# Manual review of all 100 record_id==157 rows in deid_AY2025-2026.rds found
# real rotator names embedded in cleartext. record_id 157 is the known
# "rotator bucket" — rotating residents from outside programs are pooled
# under this one shared administrative record instead of each getting their
# own resident_data record — so BOTH existing safety nets miss them:
#   - the record-based pass has no real per-row name to substitute (record
#     157's own `name` field isn't any individual rotator's name)
#   - the roster-based scan only draws first names from THIS program's own
#     resident_data roster (joined by record_id) — rotators have no such
#     record, so they're invisible to it structurally, not from a bug
#
# That same manual review also surfaced a DIFFERENT gap: a real FACULTY
# member ("Dr. Morreale") named in passing inside someone else's comment,
# in a row that has nothing to do with record 157. The faculty-name scan in
# build_faculty_deid.R only ever checked synthesis narrative text, never the
# raw ass_plus_clean/ass_delta_clean corpus columns. This script closes that
# gap too — full 135-name faculty scan across the entire 1677-row corpus,
# not just the 100 rotator rows.
#
# WHAT THIS DOES:
#   1. Redacts 20 manually-identified rotator names to [RESIDENT], scoped to
#      record_id==157 rows only (so a token like "Austin" can't collaterally
#      redact something unrelated elsewhere in the corpus).
#   2. Scans the ENTIRE corpus (all 1677 rows, not just record 157) for
#      faculty cross-mentions (two-tier: full name + filtered first-name
#      roster) and redacts any hit to [FACULTY].
#   3. Rebuilds deid_analysis_AY2025-2026.rds from the fixed source.
#   4. Re-scans + redacts synthesis_deid_AY2025-2026.rds narrative text
#      against BOTH the rotator-157 name list and the full faculty crosswalk.
#   5. Final verification: two-tier resident scan + two-tier faculty scan,
#      run against both output files. Reports remaining hit count (should
#      be 0).
#
# RUN:
#   Rscript data-refresh/fix_rotator157_leaks.R

suppressPackageStartupMessages({
  library(attendfeedback)
  library(dplyr)
  library(tibble)
  library(stringr)
})

DATA_DIR           <- "data"
DEID_PATH          <- file.path(DATA_DIR, "deid_AY2025-2026.rds")
CROSSWALK_PATH     <- file.path(DATA_DIR, "faculty_crosswalk_CONFIDENTIAL.rds")
DEID_ANALYSIS_PATH <- file.path(DATA_DIR, "deid_analysis_AY2025-2026.rds")
SYNTH_DEID_PATH    <- file.path(DATA_DIR, "synthesis_deid_AY2025-2026.rds")

crosswalk <- readRDS(CROSSWALK_PATH)

# ── 1. Redact rotator-157 names (manually identified, 2026-09-07 review) ────
#
# Includes spelling variants encountered ("Maddie" and "Mady" for what
# appears to be the same person). Deliberately scoped to record_id==157
# rows only — several of these tokens (Austin, Randy) are common enough as
# words/places that a corpus-wide redaction would risk collateral damage.

ROTATOR_157_NAMES <- c(
  "Hafsah", "Justin", "Kaitlyn", "Maddie", "Mady", "Austin", "McFarland",
  "Bashir", "Yushin", "Amer", "Ruchit", "Valdes", "Avdagic", "Chung-Yu",
  "Serhat", "Olivia", "Jonah", "Katie", "Sisi", "Randy"
)

deid <- readRDS(DEID_PATH)

redact_names <- function(text, names_vec, replacement) {
  if (is.na(text) || !nzchar(text)) return(text)
  for (nm in names_vec) {
    pattern <- paste0("\\b\\Q", nm, "\\E\\b")
    text <- stringr::str_replace_all(text, stringr::regex(pattern, ignore_case = TRUE), replacement)
  }
  text
}

is_157 <- deid$record_id == 157
n_before_plus  <- sum(vapply(deid$ass_plus_clean[is_157],  function(t) any(stringr::str_detect(t %||% "", stringr::regex(paste0("\\b(", paste(ROTATOR_157_NAMES, collapse="|"), ")\\b"), ignore_case = TRUE))), logical(1)))
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

deid$ass_plus_clean[is_157]  <- vapply(deid$ass_plus_clean[is_157],  redact_names, character(1), names_vec = ROTATOR_157_NAMES, replacement = "[RESIDENT]")
deid$ass_delta_clean[is_157] <- vapply(deid$ass_delta_clean[is_157], redact_names, character(1), names_vec = ROTATOR_157_NAMES, replacement = "[RESIDENT]")

message(sprintf("== Step 1: rotator-157 redaction ==\n  %d name token(s) applied to %d record_id==157 rows",
                length(ROTATOR_157_NAMES), sum(is_157)))

# ── 2. Faculty cross-mention scan — ENTIRE corpus, not just record 157 ─────

all_faculty <- crosswalk$fac_name

scan_and_redact_faculty <- function(text) {
  if (is.na(text) || !nzchar(text)) return(list(text = text, hit = FALSE, matched = character(0)))
  hit <- FALSE
  matched <- character(0)
  for (nm in all_faculty) {
    pattern <- stringr::regex(paste0("\\b\\Q", nm, "\\E\\b"), ignore_case = TRUE)
    if (stringr::str_detect(text, pattern)) {
      text <- stringr::str_replace_all(text, pattern, "[FACULTY]")
      hit <- TRUE
      matched <- c(matched, nm)
    }
  }
  list(text = text, hit = hit, matched = matched)
}

faculty_hits <- tibble::tibble(record_id = integer(0), field = character(0), matched_name = character(0))

for (col in c("ass_plus_clean", "ass_delta_clean")) {
  for (i in seq_len(nrow(deid))) {
    txt <- deid[[col]][i]
    res <- scan_and_redact_faculty(txt)
    if (res$hit) {
      deid[[col]][i] <- res$text
      faculty_hits <- dplyr::bind_rows(faculty_hits, tibble::tibble(
        record_id = deid$record_id[i], field = col, matched_name = paste(res$matched, collapse = ", ")
      ))
    }
  }
}

message(sprintf("\n== Step 2: full-corpus faculty cross-mention scan (135 names x 1677 rows x 2 fields) ==\n  %d hit(s) found and redacted to [FACULTY]", nrow(faculty_hits)))
if (nrow(faculty_hits) > 0) print(faculty_hits, n = Inf)

saveRDS(deid, DEID_PATH)
message(sprintf("\n  Written back: %s", DEID_PATH))

# ── 3. Rebuild deid_analysis_AY2025-2026.rds from the fixed source ─────────

deid_analysis <- deid |>
  dplyr::left_join(crosswalk, by = c("ass_faculty" = "fac_name")) |>
  dplyr::select(-ass_faculty, -ass_specialty) |>
  dplyr::rename(faculty_id = study_id) |>
  dplyr::relocate(faculty_id, .after = record_id)

stopifnot(sum(is.na(deid_analysis$faculty_id)) == 0)
saveRDS(deid_analysis, DEID_ANALYSIS_PATH)
message(sprintf("\n== Step 3: rebuilt %s (%d records) ==", DEID_ANALYSIS_PATH, nrow(deid_analysis)))

# ── 4. Re-scan + redact synthesis_deid_AY2025-2026.rds narrative text ──────

synth_deid <- readRDS(SYNTH_DEID_PATH)

extract_field_refs <- function(entry) {
  # Returns a list of (getter, setter) pairs for every free-text leaf, so we
  # can redact IN PLACE without rebuilding the whole nested structure by hand.
  refs <- list()
  refs[[length(refs) + 1]] <- list(path = c("faculty_summary", "score_explanation"))
  for (i in seq_along(entry$themes)) {
    refs[[length(refs) + 1]] <- list(path = c("themes", i, "description"))
    refs[[length(refs) + 1]] <- list(path = c("themes", i, "example"))
  }
  for (i in seq_along(entry$growth_pointers)) {
    refs[[length(refs) + 1]] <- list(path = c("growth_pointers", i, "recommendation"))
    refs[[length(refs) + 1]] <- list(path = c("growth_pointers", i, "rationale"))
    refs[[length(refs) + 1]] <- list(path = c("growth_pointers", i, "example_revision"))
  }
  refs[[length(refs) + 1]] <- list(path = "pd_coaching_note")
  refs[[length(refs) + 1]] <- list(path = "faculty_coaching_note")
  refs
}

get_by_path <- function(x, path) {
  for (p in path) x <- x[[p]]
  x
}
set_by_path <- function(x, path, value) {
  if (length(path) == 1) { x[[path]] <- value; return(x) }
  x[[path[1]]] <- set_by_path(x[[path[1]]], path[-1], value)
  x
}

synth_hits <- tibble::tibble(faculty_id = character(0), path = character(0),
                             source = character(0), matched = character(0))

for (key in names(synth_deid)) {
  entry <- synth_deid[[key]]
  refs <- extract_field_refs(entry)
  for (r in refs) {
    val <- get_by_path(entry, r$path)
    if (is.null(val) || is.na(val) || !nzchar(val)) next

    # rotator-157 name list
    rot_res <- list(text = redact_names(val, ROTATOR_157_NAMES, "[RESIDENT]"))
    if (!identical(rot_res$text, val)) {
      synth_hits <- dplyr::bind_rows(synth_hits, tibble::tibble(
        faculty_id = entry$.faculty_name, path = paste(r$path, collapse = "."),
        source = "rotator_roster", matched = "rotator-157 name"
      ))
      val <- rot_res$text
    }

    # faculty cross-mention
    fac_res <- scan_and_redact_faculty(val)
    if (fac_res$hit) {
      synth_hits <- dplyr::bind_rows(synth_hits, tibble::tibble(
        faculty_id = entry$.faculty_name, path = paste(r$path, collapse = "."),
        source = "faculty_crosswalk", matched = paste(fac_res$matched, collapse = ", ")
      ))
      val <- fac_res$text
    }

    entry <- set_by_path(entry, r$path, val)
  }
  synth_deid[[key]] <- entry
}

message(sprintf("\n== Step 4: synthesis narrative re-scan (rotator-157 list + full faculty crosswalk) ==\n  %d hit(s) found and redacted", nrow(synth_hits)))
if (nrow(synth_hits) > 0) print(synth_hits, n = Inf)

saveRDS(synth_deid, SYNTH_DEID_PATH)
message(sprintf("\n  Written back: %s", SYNTH_DEID_PATH))

# ── 5. "Dr. <Lastname>" scan — catches last-name-only mentions safely ──────
#
# The full-name-only scan in step 2 missed "Dr. Morreale" — a real faculty
# member named by last name alone. A bare last-name scan is too risky
# corpus-wide (several of our 135 surnames are ordinary English words: Go,
# Moon, Wood, Cole...), but "Dr. <Lastname>" is a safe, low-collision
# pattern: the title makes it specific.

parse_last_name <- function(full_name) {
  toks <- strsplit(trimws(full_name), "\\s+")[[1]]
  toks[length(toks)]
}
faculty_last_names <- unique(vapply(all_faculty, parse_last_name, character(1)))
# Guard against a one-word "name" (a data quality artifact, e.g. "Andres")
# whose "last name" would just be itself — Dr. Andres is too weak a signal
# to redact safely at the full-corpus scale, skip single-token entries.
faculty_last_names <- faculty_last_names[nchar(faculty_last_names) > 2]

scan_and_redact_dr_lastname <- function(text) {
  if (is.na(text) || !nzchar(text)) return(list(text = text, hit = FALSE, matched = character(0)))
  hit <- FALSE
  matched <- character(0)
  for (ln in faculty_last_names) {
    pattern <- stringr::regex(paste0("\\bDr\\.?\\s+\\Q", ln, "\\E\\b"), ignore_case = TRUE)
    if (stringr::str_detect(text, pattern)) {
      text <- stringr::str_replace_all(text, pattern, "[FACULTY]")
      hit <- TRUE
      matched <- c(matched, ln)
    }
  }
  list(text = text, hit = hit, matched = matched)
}

dr_hits <- tibble::tibble(record_id = integer(0), field = character(0), matched_name = character(0))
for (col in c("ass_plus_clean", "ass_delta_clean")) {
  for (i in seq_len(nrow(deid))) {
    txt <- deid[[col]][i]
    res <- scan_and_redact_dr_lastname(txt)
    if (res$hit) {
      deid[[col]][i] <- res$text
      dr_hits <- dplyr::bind_rows(dr_hits, tibble::tibble(
        record_id = deid$record_id[i], field = col, matched_name = paste(res$matched, collapse = ", ")
      ))
    }
  }
}
message(sprintf("\n== Step 5: 'Dr. <Lastname>' scan (%d surnames x 1677 rows x 2 fields) ==\n  %d hit(s) found and redacted to [FACULTY]",
                length(faculty_last_names), nrow(dr_hits)))
if (nrow(dr_hits) > 0) print(dr_hits, n = Inf)

saveRDS(deid, DEID_PATH)

# Rebuild the analysis dataset again, now including the Dr.-Lastname fix
deid_analysis <- deid |>
  dplyr::left_join(crosswalk, by = c("ass_faculty" = "fac_name")) |>
  dplyr::select(-ass_faculty, -ass_specialty) |>
  dplyr::rename(faculty_id = study_id) |>
  dplyr::relocate(faculty_id, .after = record_id)
stopifnot(sum(is.na(deid_analysis$faculty_id)) == 0)
saveRDS(deid_analysis, DEID_ANALYSIS_PATH)
message(sprintf("  Rebuilt: %s", DEID_ANALYSIS_PATH))

# Apply the same Dr.-Lastname scan to synthesis narrative text
synth_deid <- readRDS(SYNTH_DEID_PATH)
dr_synth_hits <- tibble::tibble(faculty_id = character(0), path = character(0), matched = character(0))
for (key in names(synth_deid)) {
  entry <- synth_deid[[key]]
  refs <- extract_field_refs(entry)
  for (r in refs) {
    val <- get_by_path(entry, r$path)
    if (is.null(val) || is.na(val) || !nzchar(val)) next
    res <- scan_and_redact_dr_lastname(val)
    if (res$hit) {
      dr_synth_hits <- dplyr::bind_rows(dr_synth_hits, tibble::tibble(
        faculty_id = entry$.faculty_name, path = paste(r$path, collapse = "."),
        matched = paste(res$matched, collapse = ", ")
      ))
      entry <- set_by_path(entry, r$path, res$text)
    }
  }
  synth_deid[[key]] <- entry
}
message(sprintf("\n  Synthesis narrative 'Dr. <Lastname>' hits: %d", nrow(dr_synth_hits)))
if (nrow(dr_synth_hits) > 0) print(dr_synth_hits, n = Inf)
saveRDS(synth_deid, SYNTH_DEID_PATH)
message(sprintf("  Rebuilt: %s", SYNTH_DEID_PATH))

# ── 6. Corpus-wide resident-name redaction (2026-09-07, broader review) ────
#
# The record-based pass only knows the CURRENT row's own resident (via the
# `name` field), and the roster-based pass only knows first names that
# exactly match some record_id's `name` field. Both structurally miss:
#   - nicknames that don't match the roster string verbatim (Katie/Kaitlin,
#     Chris/Christopher — same class of gap as the rotator-157 finding, but
#     these belong to residents who DO have their own record_id)
#   - "Dr. <Lastname>" mentions of the record's OWN resident (when they
#     already hold an MD, e.g. a preliminary psych/anesthesia resident)
#   - cross-mentions of A DIFFERENT resident by name (a senior, a peer
#     quoted in 360 feedback) — the record-based pass only ever redacts the
#     CURRENT record's resident, never someone else named in passing
#
# Found by: (a) checking the 6 theme "example" fields that didn't match
# verbatim against the corpus, tracing 2 of them to real leaked names; (b) a
# broadened capitalized-word-before-common-verb heuristic swept across all
# 1677 rows x 2 fields, manually reviewed hit by hit; (c) a "Dr. <Name>"
# sweep across the same. Each token below was individually verified against
# the raw corpus to rule out collision with an ordinary English word
# (several first-pass candidates — "barb", "lad", "nic", "isa" — were
# checked this way and found unambiguous).
#
# Applied CORPUS-WIDE (not scoped to one record_id) since these residents
# each have their own record(s) scattered through the year, and none of
# these specific tokens showed a collateral non-name occurrence anywhere
# in the corpus during verification.

RESIDENT_NAMES_BROADER <- c(
  "Gagen", "Lian", "Chris", "Nishak", "Paniker", "Yasmine", "Esmaeilkhnian",
  "Baghdadi", "Pham", "Katie", "Shivy", "Sumra", "Barb", "Lad", "Nic",
  "Nate", "Isa", "Krystal", "Patrick", "Vaneesa", "Baumer", "Rodger"
)

deid <- readRDS(DEID_PATH)
n_hits_before <- sum(vapply(c(deid$ass_plus_clean, deid$ass_delta_clean), function(t) {
  !is.na(t) && any(stringr::str_detect(t, stringr::regex(paste0("\\b(", paste(RESIDENT_NAMES_BROADER, collapse = "|"), ")\\b"), ignore_case = TRUE)))
}, logical(1)))

deid$ass_plus_clean  <- vapply(deid$ass_plus_clean,  redact_names, character(1), names_vec = RESIDENT_NAMES_BROADER, replacement = "[RESIDENT]")
deid$ass_delta_clean <- vapply(deid$ass_delta_clean, redact_names, character(1), names_vec = RESIDENT_NAMES_BROADER, replacement = "[RESIDENT]")

message(sprintf("\n== Step 6: corpus-wide resident-name redaction (%d names) ==\n  %d row(s) contained at least one hit before redaction",
                length(RESIDENT_NAMES_BROADER), n_hits_before))

saveRDS(deid, DEID_PATH)

deid_analysis <- deid |>
  dplyr::left_join(crosswalk, by = c("ass_faculty" = "fac_name")) |>
  dplyr::select(-ass_faculty, -ass_specialty) |>
  dplyr::rename(faculty_id = study_id) |>
  dplyr::relocate(faculty_id, .after = record_id)
stopifnot(sum(is.na(deid_analysis$faculty_id)) == 0)
saveRDS(deid_analysis, DEID_ANALYSIS_PATH)
message(sprintf("  Rebuilt: %s", DEID_ANALYSIS_PATH))

synth_deid <- readRDS(SYNTH_DEID_PATH)
broader_synth_hits <- tibble::tibble(faculty_id = character(0), path = character(0), matched = character(0))
for (key in names(synth_deid)) {
  entry <- synth_deid[[key]]
  refs <- extract_field_refs(entry)
  for (r in refs) {
    val <- get_by_path(entry, r$path)
    if (is.null(val) || is.na(val) || !nzchar(val)) next
    new_val <- redact_names(val, RESIDENT_NAMES_BROADER, "[RESIDENT]")
    if (!identical(new_val, val)) {
      broader_synth_hits <- dplyr::bind_rows(broader_synth_hits, tibble::tibble(
        faculty_id = entry$.faculty_name, path = paste(r$path, collapse = "."), matched = "resident name"
      ))
      entry <- set_by_path(entry, r$path, new_val)
    }
  }
  synth_deid[[key]] <- entry
}
message(sprintf("  Synthesis narrative hits: %d", nrow(broader_synth_hits)))
if (nrow(broader_synth_hits) > 0) print(broader_synth_hits, n = Inf)
saveRDS(synth_deid, SYNTH_DEID_PATH)
message(sprintf("  Rebuilt: %s", SYNTH_DEID_PATH))
