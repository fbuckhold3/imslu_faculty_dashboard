# Faculty Dashboard — imslu_faculty_dashboard

Individual and leadership dashboard for IMSLU Internal Medicine faculty.
Built with R/Shiny, integrating two REDCap databases (IMSLUFaculty + RDM2_0).
Each faculty member logs in with a unique access code and sees only their
own data; division admins and department leaders get scoped aggregate views.

**Repository:** https://github.com/fbuckhold3/imslu_faculty_dashboard
**Not deployed.** `manifest.json` and `renv.lock` are vestigial — nothing on
Posit Connect reads them. Pushing to `main` is a plain push, no auto-deploy.

## Current state (corrected 2026-09-09)

The interactive Shiny app (`app.R` / `global.R` / `R/modules/`) below is no
longer how this project is actually used — Fred no longer uses the online
dashboard. **Current real workflow:** `data-refresh/refresh_feedback_data.R`
pulls + de-identifies + synthesizes feedback (via the `attendfeedback`
package), `reports/*.qmd` render per-faculty/division/PD HTML reports from
that data, and those reports go out by email via Power Automate (see
`reports/render_all_reports.R` and the `pa_manifest_*.csv` it writes). See
[[project_attendfeedback_pipeline]] in memory for the pipeline's current
state. The Shiny app's architecture is documented below as-is in case it's
ever revived, but treat it as dormant, not the active path.

Project plan, data-dictionary design, visualization inventory, and decision
log from the original interactive-dashboard effort live in the Cowork
sub-project (also likely stale relative to the above):

```
~/Library/Cowork/faculty-dashboard-v1-launch/
├── plan.md                # Phase 0–4 task list
├── data-dictionary.md     # New REDCap fields being designed
├── visualizations.md      # New viz + role × view access matrix
├── decisions.md           # Architectural decision log
└── launch-checklist.md    # Phase 4 production rollout
```

When starting a new chat on this app: read this file → then the Cowork README →
then whichever phase doc is current.

## Architecture

### Role-based access

Three tiers, computed at login from faculty record fields. Defined in
`R/modules/mod_login.R`.

| Role | Trigger field | Data scope |
|---|---|---|
| `individual` | default | Own evaluations only |
| `division_admin` | `fac_admin == "Yes"` | Faculty in same `fac_div` + same `fac_clin` |
| `department_leader` | `dep_lead == "Yes"` | All faculty (scoped to `fac_clin` if set; full oversight if `fac_clin` is blank) |

The login server returns a reactive `faculty_info()` list with `record_id`,
`fac_name`, `access_level`, `accessible_faculty` (the list of names the user is
allowed to view), plus division/site labels and an `has_full_oversight` flag for
the top tier.

### Production mode toggle

`mod_login.R` checks the `PRODUCTION_MODE` environment variable:

- `PRODUCTION_MODE=TRUE` → access-code field only
- unset / `FALSE` → adds a testing dropdown of all active faculty (default: Fred Buckhold)

Set on Posit Connect as an env var. Local dev runs without it set.

### Data sources

| DB | Token (env var) | What's in it |
|---|---|---|
| IMSLUFaculty | `FAC_TOKEN` | Faculty roster, access codes, roles, divisions |
| RDM2_0 | `RDM_TOKEN` | All evaluations, assessments, ILPs, attendance |
| (base URL) | `REDCAP_URL` | Shared base URL for the REDCap API |

All tokens live in `~/.Renviron` locally and as Connect env vars in production —
never in the repo.

## Data structure

### IMSLUFaculty (237 records, 29+ columns)

```
record_id          Unique faculty ID
fac_name           Faculty name — PRIMARY LINKING FIELD to RDM data
fac_email          Email
fac_clin           Clinical affiliate (1=SSM, 2=VA, 3=Other)
fac_div            Division/Section (numeric code; labels via REDCap data dict)
fac_fell           Faculty (1) or Fellow (2)
fac_med_ed___*     Med Ed leadership roles (checkbox)
fac_meded_fte      Educational FTE (0–1)
fac_access         Access code (login key)
fac_admin          Division admin flag (yesno) — drives division_admin role
dep_lead           Department leader flag (yesno) — drives department_leader role
archived           Archived (0=active)
```

Active faculty: ~179 of 237.

### RDM2_0 (6 forms downloaded via `download_rdm_focused()`)

```r
rdm_redcap_data <- list(
  resident_data       = df,  # Resident demographics
  assessment          = df,  # Faculty evaluating residents
  faculty_evaluation  = df,  # Residents evaluating faculty
  s_eval              = df,  # Resident self-evaluations
  ilp                 = df,  # Individual learning plans (loaded but not yet consumed)
  questions           = df   # Attendance / noon conference tracking
)
```

The pull separates forms by `redcap_repeat_instrument` after one combined POST
(faster than per-form API calls).

**Key fields by form:**

`faculty_evaluation` — residents evaluating faculty
```
record_id            Resident submitting the eval
fac_fell_name        Faculty being evaluated (LINKS TO fac_name)
time_teaching        Ensures time for teaching (1–5)
att_overall          Overall teaching rating (1–5)
att_ext_tea          Extra teaching effort
att_give_feed        Feedback type given
plus / delta         Free-text feedback
```

`assessment` — faculty evaluating residents
```
record_id            Resident being assessed
ass_date             Assessment date
ass_faculty          Faculty performing (LINKS TO fac_name)
ass_level            Resident level (1=Intern, 2=PGY2, 3=PGY3)
ass_plus / ass_delta Feedback
ass_obs_*            Observation-specific fields
```

`s_eval` — for resident report cards (use highest `redcap_repeat_instance`)
```
s_e_topic_sel___*    Topics less confident in (checkbox, ~23 options)
s_e_learn_style___*  Learning style preferences (checkbox, ~12 options)
```

`ilp` — individual learning plans (loaded, not yet wired into UI)

`questions` — attendance tracking
```
record_id            Resident
q_date               Date
q_rotation           Rotation
```

`resident_data`
```
record_id, first_name, last_name, name, grad_yr
type                 1=Preliminary, 2=Categorical, 3=Dismissed
res_archive          0=active (~55 currently)
coach                Coach assignment
```

### Linking strategy

Primary join field is faculty name:
- `IMSLUFaculty$fac_name` ↔ `faculty_evaluation$fac_fell_name`
- `IMSLUFaculty$fac_name` ↔ `assessment$ass_faculty`

Use `clean_faculty_names()` for standardization (trim, title case, squish).
Test names in evaluations (Mrs. Buttersworth, Mr. Bigglesworth, etc.) are
expected legacy test data and will fail name matching by design.

## File organization

```
imslu_faculty_dashboard/
├── app.R                          # Main Shiny app
├── global.R                       # Library loads + initial REDCap pull
├── Claude.md                      # This file
├── README.md
├── manifest.json                  # Posit Connect deployment manifest
├── .gitignore                     # Protects tokens, data, rsconnect
│
├── R/
│   ├── modules/
│   │   ├── mod_login.R            # Auth + role determination
│   │   ├── mod_faculty_eval.R     # Individual faculty's own evaluations
│   │   └── mod_leader_dashboard.R # Aggregate view for admins/leaders
│   │
│   └── utils/
│       ├── data_processing.R      # REDCap pulls, division labels, name cleaning
│       ├── calculations.R         # Means, aggregations, filtering by year
│       └── plot_functions.R       # Plotly spider, bar charts, etc.
│
├── data/                          # Local test cache (gitignored)
│   ├── faculty_test.rds
│   └── rdm_test.rds
│
├── www/                           # Static assets (CSS, images)
└── testing/                       # Diagnostic scripts (not part of app)
```

The `R/wrappers/` folder mentioned in older docs is not in use.

## Key functions

### `R/utils/data_processing.R`

```r
download_faculty_data()           # IMSLUFaculty pull
download_rdm_focused()            # RDM pull (single POST, separates forms)
save_test_data()                  # Cache both to data/*.rds
load_test_data()                  # Read cached rds

clean_faculty_names(names)        # Trim, title case, squish
assign_academic_year(dates)       # Date → "2024-2025" academic year string
get_current_academic_year()       # Today's academic year
parse_checkbox_field(data, prefix)# Collapse REDCap checkbox columns

get_division_mapping()            # Read fac_div choices from REDCap data dict
get_division_label(code)          # Numeric → label
add_division_labels(faculty_data) # Append fac_div_label column

check_name_matching(faculty, rdm) # Diagnostic: faculty-vs-eval name overlap
```

Division mapping is cached in `.GlobalEnv` after first call to avoid repeat
metadata API hits. Has a fallback path if the metadata pull fails — the app
still works, divisions just render as "Division N".

### `R/utils/calculations.R` (~715 lines)

Filtering, aggregation, and threshold helpers for both individual and
aggregate views. See file for the full list.

### `R/utils/plot_functions.R` (~412 lines)

Plotly builders: spider/radar, bar chart, conference attendance stacked bar.

## Academic year logic

- Academic year runs July 1 → June 30
- Current: 2025-2026 (as of June 2026)
- `2024-06-30` → `"2023-2024"`
- `2024-07-01` → `"2024-2025"`
- Handles both `Date` and REDCap numeric `YYYYM` format

## Development workflow

### Working offline

```r
# In global.R, swap the live pull for cached:
test_data <- load_test_data()
faculty_redcap_data <- test_data$faculty
rdm_redcap_data <- test_data$rdm

# To refresh the cache:
source("R/utils/data_processing.R")
save_test_data()
```

### Running locally

```r
shiny::runApp()
# Or in Positron: source app.R then runApp() at the bottom
```

### Branch / commit

Per global conventions: imperative tense, ≤50-char subject. Examples from
this repo: `Add production mode`, `Implement full drill-down in leadership dashboard`,
`Restructure evaluation domains to use correct 1-5 scale fields`.

## Module template

```r
mod_<name>_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # bs4Card / fluidRow / column / value boxes / outputs
  )
}

mod_<name>_server <- function(id, faculty_info, rdm_data, faculty_data) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      req(faculty_info())
      # Filter by faculty_info()$accessible_faculty
    })

    output$something <- renderPlotly({ ... })

    # Return reactive if needed
  })
}
```

## Important constants

```r
MIN_EVALUATIONS         <- 5            # Min evals per period to display
EVALUATION_DELAY_MONTHS <- 6            # Delay before showing recent evals

TEACHING_SCALE <- c("1" = "Never ensures time", "3" = "Occasionally", "5" = "Always")
OVERALL_SCALE  <- c("1" = "Needs improvement", "2" = "Satisfactory",
                    "3" = "Good", "4" = "Very good", "5" = "Outstanding")
```

## Styling preferences

- Plotly for interactive, ggplot for static
- DT::datatable: `pageLength = 10`, `searching = TRUE`
- bs4Dash status colors: primary, success, warning, danger
- Value boxes for summary stats
- Spider plot for multi-domain comparisons

## Security

**Never commit:**
- `.Renviron` (tokens)
- `data/*.rds` (PHI)
- `rsconnect/`

`.gitignore` should include all of the above plus `.Renviron.local`, `*.env`.

In code, always `Sys.getenv("VAR_NAME")` — never hardcoded values.

## Deployment

Not deployed (corrected 2026-09-09 — this section previously described a
live Posit Connect Cloud deployment that no longer exists). `manifest.json`
and `renv.lock` are leftover from when it was, both stale (neither lists
`gmed` or `attendfeedback`), and safe to ignore or clean up — nothing reads
them. Reports are produced and distributed by running
`data-refresh/refresh_feedback_data.R` then `reports/render_all_reports.R`
by hand (or via a scheduled local job), not via a Connect redeploy.

## Quick reference

```r
# Reload everything
source("global.R")

# Run
shiny::runApp()

# Refresh cache
source("R/utils/data_processing.R"); save_test_data()

# Diagnostic name match
check_name_matching(faculty_redcap_data, rdm_redcap_data)

# Academic year sanity check
assign_academic_year("2025-01-06")  # → "2024-2025"
```

## Getting help

- Architecture / planning: `~/Library/Cowork/faculty-dashboard-v1-launch/`
- REDCap field references: REDCap data dictionary (download via REDCap UI)
- Similar viz patterns: `gmed/` and the sibling apps (`imslu.ind.dash`, `imslu.coach.dash`, `imslu.ccc.dashboard`)
- Diagnostic scripts: `testing/` folder

## gmed / attendfeedback dependency status

The Shiny app itself (`app.R`/`global.R`/`R/modules/`) does NOT depend on
`gmed` or `attendfeedback` — it rolled its own data processing,
calculations, and plotting helpers. The "Phase 2 migrate shared bits into
gmed" plan below refers to that dormant app and hasn't happened.

The actual active workflow (`data-refresh/refresh_feedback_data.R`,
`data-refresh/build_research_dataset.R`, and the `.qmd` reports) DOES
depend on `attendfeedback` (not `gmed` — that pipeline was extracted to its
own package 2026-09; see [[project_attendfeedback_pipeline]]). Install it
with `remotes::install_github("fbuckhold3/attendfeedback")` before running
those scripts.
