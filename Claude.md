# Faculty Dashboard Development Guide

## Project Overview
**Repository:** https://github.com/fbuckhold3/imslu_faculty_dashboard

Individual dashboard for Internal Medicine faculty to review assessments and teaching-related information through secure, unique access codes. Built with R/Shiny, integrating data from two REDCap databases (IMSLUFaculty and RDM2_0).

## Current State

### ✅ Completed
- [x] Project structure created (R/modules, R/utils, R/wrappers, data, www)
- [x] REDCap API connections working
- [x] Data download functions optimized (single API call for speed)
- [x] Test data saved locally for offline development
- [x] Data validation completed - 1,404 faculty evaluations available
- [x] Academic year calculation functions (July-June cycle)
- [x] Name matching/cleaning utilities
- [x] Minimal app (v0.1) with data display working

### 🎯 Current Focus: Phase 1 Development

**Priority Features:**
1. Login module (faculty selection/access code)
2. Faculty evaluations display with filtering
3. Calculation of means and comparisons
4. Resident work-room features

## Data Structure

### REDCap Databases

**1. IMSLUFaculty Database** (237 records, 29 columns)
```
Key Fields:
- record_id: Unique faculty ID
- fac_name: Faculty name (PRIMARY LINKING FIELD)
- fac_email: Email address
- fac_clin: Clinical affiliate (1=SSM, 2=VA, 3=Other)
- fac_div: Division/Section
- fac_fell: Faculty or Fellow (1=Faculty, 2=Fellow)
- fac_med_ed___*: Med Ed leadership roles (checkbox)
- fac_meded_fte: Educational FTE (0-1)
- archived: Archived status (0=active)
- fac_access: Access code (TO BE ADDED)
```

**2. RDM2_0 Database** (5 forms downloaded)

Data structure in R:
```r
rdm_redcap_data <- list(
  faculty_evaluation = df,  # 1,404 rows - residents evaluating faculty
  assessment = df,          # 207 rows - faculty evaluating residents
  s_eval = df,             # 328 rows - resident self-evaluations
  questions = df,          # 254 rows - attendance tracking
  resident_data = df       # 194 rows - resident demographics
)
```

**faculty_evaluation form** (1,404 evaluations, 46 columns):
```
Key Fields:
- record_id: Resident ID who submitted evaluation
- fac_fell_name: Faculty being evaluated (LINKS TO fac_name)
- time_teaching: Ensures time for teaching (1-5 scale)
- att_overall: Overall teaching rating (1-5 scale)
- att_ext_tea: Extra teaching effort (dropdown)
- att_give_feed: Feedback type given
- plus: Positive feedback (text)
- delta: Improvement suggestions (text)

Rating Distribution:
  1: 15, 2: 25, 3: 66, 4: 267, 5: 744, NA: 287
  Average: ~4.5 (very positive!)
```

**assessment form** (207 rows, 118 columns):
```
Key Fields:
- record_id: Resident being assessed
- ass_date: Assessment date
- ass_faculty: Faculty performing assessment (LINKS TO fac_name)
- ass_level: Resident level (1=Intern, 2=PGY2, 3=PGY3, etc)
- ass_plus, ass_delta: Feedback
- ass_obs_*: Observation-specific fields
```

**s_eval form** (328 rows, 130 columns):
```
For Resident Report Cards:
- s_e_topic_sel___*: Topics less confident in (checkbox, 23 options)
- s_e_learn_style___*: Learning style preferences (checkbox, 12 options)
- Use highest redcap_repeat_instance for most recent data
```

**questions form** (254 rows, 7 columns):
```
For Attendance Tracking:
- record_id: Resident ID
- q_date: Date
- q_rotation: Rotation identifier
```

**resident_data form** (194 rows, 28 columns):
```
- record_id: Resident ID
- first_name, last_name, name: Resident names
- grad_yr: Graduation year
- type: Resident type (1=Preliminary, 2=Categorical, 3=Dismissed)
- res_archive: Archived status (0=active, 55 currently active)
- coach: Resident coach assignment
```

### Data Linking Strategy

**Primary Link:** Faculty Name
- `IMSLUFaculty$fac_name` ↔ `RDM2_0$faculty_evaluation$fac_fell_name` (residents evaluating faculty)
- `IMSLUFaculty$fac_name` ↔ `RDM2_0$assessment$ass_faculty` (faculty evaluating residents)

**Name Cleaning:** Use `clean_faculty_names()` for standardization (trim, title case, squish whitespace)

**Known Name Mismatches:** 9 test names in evaluations not in faculty DB - this is expected test data

## File Organization

```
imslu_faculty_dashboard/
├── app.R                    # Main Shiny app (or split to ui.R/server.R/global.R)
├── global.R                 # Global setup, data loading
├── Claude.md                # This file
├── README.md
├── .gitignore              # Protects tokens, data files
├── .Renviron               # API credentials (NOT in Git)
│
├── R/
│   ├── modules/            # Shiny modules
│   │   ├── mod_login.R     # TO BUILD: Login/authentication
│   │   ├── mod_faculty_eval.R  # TO BUILD: Faculty evaluations display
│   │   ├── mod_student_eval.R  # PLACEHOLDER
│   │   ├── mod_resident_workroom.R  # TO BUILD: Report cards, attendance
│   │   ├── mod_teaching_portfolio.R # PLACEHOLDER
│   │   ├── mod_educator_milestones.R # PLACEHOLDER
│   │   └── mod_grand_rounds.R      # PLACEHOLDER
│   │
│   ├── utils/              # Utility functions
│   │   ├── data_processing.R  # ✓ COMPLETE: REDCap download, cleaning
│   │   ├── calculations.R     # TO BUILD: Means, aggregations
│   │   └── plot_functions.R   # TO BUILD: Plotly visualizations
│   │
│   └── wrappers/           # Wrapper functions
│       └── faculty_wrapper.R  # TO BUILD: Faculty-specific filtering
│
├── data/                   # Local test data (NOT in Git)
│   ├── faculty_test.rds    # ✓ EXISTS: Cached faculty data
│   └── rdm_test.rds        # ✓ EXISTS: Cached RDM data
│
└── www/                    # Static assets (CSS, images)
```

## Key Functions Reference

### Data Processing (R/utils/data_processing.R) ✓ COMPLETE

```r
# Download functions
download_faculty_data()           # Get IMSLUFaculty data
download_rdm_focused()           # Get RDM forms (fast, single call)
save_test_data()                 # Cache data locally
load_test_data()                 # Load cached data

# Cleaning functions
clean_faculty_names(names)       # Standardize names
assign_academic_year(dates)      # Convert dates to academic year
get_current_academic_year()      # Get current academic year
parse_checkbox_field(data, prefix) # Parse REDCap checkboxes

# Validation
check_name_matching(faculty_data, rdm_data)  # Validate name links
```

### Academic Year Logic
- Academic year: July 1 to June 30
- Current academic year: 2025-2026 (as of Jan 2026)
- Examples:
  - 2024-06-30 → 2023-2024
  - 2024-07-01 → 2024-2025
  - 2025-01-06 → 2024-2025 (we're here)

## Development Tasks

### 🔥 IMMEDIATE: Build Login Module

**File:** `R/modules/mod_login.R`

**Requirements:**
- For testing: Dropdown to select faculty (later will be access code input)
- Validate against `faculty_redcap_data$fac_name`
- Return reactive containing faculty info (record_id, fac_name, etc.)
- Show welcome message on successful login

**Module Structure:**
```r
mod_login_ui <- function(id) {
  ns <- NS(id)
  # UI elements: selectInput for faculty, actionButton for login
}

mod_login_server <- function(id, faculty_data) {
  moduleServer(id, function(input, output, session) {
    # Return reactive faculty_info()
  })
}
```

**Integration in app.R:**
```r
# In server:
faculty_info <- mod_login_server("login", faculty_redcap_data)

# Hide tabs until logged in:
observe({
  if (is.null(faculty_info())) {
    hideTab(inputId = "sidebar", target = "eval_tab")
  } else {
    showTab(inputId = "sidebar", target = "eval_tab")
    updateTabItems(session, "sidebar", "eval_tab")
  }
})
```

### 📊 NEXT: Faculty Evaluation Module

**File:** `R/modules/mod_faculty_eval.R`

**Requirements:**

1. **Data Filtering:**
   - Filter `faculty_evaluation` where `fac_fell_name` matches logged-in faculty
   - Apply academic year filter (current year vs. all time)
   - Check minimum 5 evaluations per period
   - Apply 6-month delay: `eval_date <= today() - months(6)`

2. **Calculations (need R/utils/calculations.R):**
   ```r
   calculate_faculty_eval_means(eval_data, domains = c("time_teaching", "att_overall"))
   calculate_all_faculty_means(rdm_data)  # For comparison
   ```

3. **UI Components:**
   - Time filter: Radio buttons (Current Year / All Time)
   - Summary table: This Faculty | All Faculty | Difference
   - Spider plot: Plotly radar chart comparing faculty vs. mean
   - Plus/Delta tables: Sortable, searchable DT::datatable

4. **Evaluation Domains to Display:**
   - Time for Teaching (`time_teaching`)
   - Overall Teaching (`att_overall`)
   - Extra Teaching (`att_ext_tea`)
   - Feedback Quality (`att_give_feed`)

### 🔧 Supporting Files Needed

**R/utils/calculations.R:**
```r
# Calculate mean scores for evaluation domains
calculate_faculty_eval_means <- function(eval_data, faculty_name = NULL) {
  # If faculty_name provided: filter and calculate means for that faculty
  # If NULL: calculate means across all faculty (for comparison)
  # Return: data frame with domain names and mean scores
}

# Filter by academic year
filter_by_academic_year <- function(data, year = "current") {
  # year can be "current", "all", or specific "2024-2025"
}

# Apply time delay filter
apply_time_delay <- function(data, date_col, delay_months = 6) {
  # Filter out records within delay_months of today
}

# Check minimum threshold
check_minimum_threshold <- function(data, threshold = 5) {
  # Return TRUE if nrow(data) >= threshold
}
```

**R/utils/plot_functions.R:**
```r
# Create Plotly spider/radar plot
create_spider_plot <- function(faculty_means, comparison_means, domains) {
  # Use plot_ly(type = 'scatterpolar')
  # Two traces: This Faculty vs. All Faculty
  # Return: plotly object
}

# Create comparison table
create_eval_comparison_table <- function(faculty_means, all_means) {
  # Create DT::datatable with:
  # Columns: Domain | This Faculty | All Faculty | Difference
  # Color code differences (green if above, red if below)
}
```

### 🏠 Resident Work-Room Module

**File:** `R/modules/mod_resident_workroom.R`

**Sub-features:**

1. **Resident Report Cards:**
   - Multi-select picker for residents
   - Display cards showing:
     - Learning styles from `s_e_learn_style___*`
     - Topics needing work from `s_e_topic_sel___*`
   - Use highest `redcap_repeat_instance` per resident

2. **Team Attendance:**
   - Date range picker (default to current week)
   - Table showing residents, rotations, dates from `questions` form
   - Visual attendance grid

3. **Observation Matrix:**
   - Heatmap showing observation completion by resident
   - Parse `ass_obs_*` fields from assessment form
   - Highlight areas needing observations

## Development Workflow

### 1. Working with Cached Data

During development, use cached data to avoid API calls:

```r
# In global.R
test_data <- load_test_data()
faculty_redcap_data <- test_data$faculty
rdm_redcap_data <- test_data$rdm
```

To refresh test data:
```r
source("R/utils/data_processing.R")
save_test_data()  # Takes ~5 seconds
```

### 2. Git Workflow

```bash
# Start new feature
git checkout -b feature/login-module
# ... develop ...
git add R/modules/mod_login.R
git commit -m "Add login module with faculty selection"
git push -u origin feature/login-module

# Merge when complete
git checkout main
git merge feature/login-module
git push
```

### 3. Testing Strategy

```r
# Test individual module in console
source("R/modules/mod_login.R")
source("R/utils/data_processing.R")

test_data <- load_test_data()
# Test module functions here
```

## Module Template

Use this template for new modules:

```r
# UI Function
mod_[name]_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      box(
        title = "[Module Title]",
        width = 12,
        # UI elements here
      )
    )
  )
}

# Server Function
mod_[name]_server <- function(id, faculty_info, rdm_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data filtering
    filtered_data <- reactive({
      req(faculty_info())
      # Filter logic here
    })
    
    # Outputs
    output$something <- renderPlotly({ ... })
    
    # Return value if needed
    return(reactive({ ... }))
  })
}
```

## Important Constants

```r
# In global.R or calculations.R
MIN_EVALUATIONS <- 5           # Minimum evals per period to display
EVALUATION_DELAY_MONTHS <- 6   # Delay before showing evaluations
CURRENT_ACADEMIC_YEAR <- "2025-2026"

# Rating scales
TEACHING_SCALE <- c("1" = "Never ensures time",
                    "2" = "",
                    "3" = "Occasionally ensures time", 
                    "4" = "",
                    "5" = "Always ensures time")

OVERALL_SCALE <- c("1" = "Needs improvement",
                   "2" = "Satisfactory",
                   "3" = "Good",
                   "4" = "Very good",
                   "5" = "Outstanding")
```

## Styling Preferences

- **Plots:** Plotly (interactive) over ggplot (static)
- **Tables:** DT::datatable with pageLength = 10, searching = TRUE
- **Colors:** Use shinydashboard color palette (blue, green, yellow, red, purple)
- **Layout:** Use `fluidRow` and `column` for responsive design
- **Value boxes:** For summary statistics

## Security Notes

**Never commit:**
- `.Renviron` (contains API tokens)
- `data/*.rds` (contains PHI)
- Any files with actual faculty/resident names or data

**Check .gitignore includes:**
```
.Renviron
data/
*.rds
rsconnect/
```

## Testing Data Notes

- Test names in evaluations (Mrs Buttersworth, Mr. Bigglesworth, etc.) are expected
- 30 real faculty being evaluated with 1,404 total evaluations
- Active faculty: 179 out of 237
- Active residents: 55 out of 194

## Future Phases (Not Current Priority)

- Student evaluations (placeholder)
- Teaching portfolio tracking (placeholder)
- Clinical educator milestones (placeholder)
- Grand rounds attendance (placeholder)
- Program-level dashboard with drill-down

## Quick Reference Commands

```r
# Reload all functions
source("global.R")

# Run app
shiny::runApp()

# Refresh test data
source("R/utils/data_processing.R")
save_test_data()

# Check name matching
check_name_matching(faculty_redcap_data, rdm_redcap_data)

# Test academic year calculation
assign_academic_year("2025-01-06")  # Returns "2024-2025"
```

## Questions to Ask When Building

1. **Data filtering:** What's the primary key/linking field?
2. **Calculations:** Individual or aggregate? Current year or all time?
3. **Visualization:** Static or interactive? What comparison is needed?
4. **User interaction:** What filters/selectors are needed?
5. **Performance:** Using cached data or live API calls?

## Success Criteria

**Phase 1 Complete When:**
- ✅ Faculty can log in (select their name)
- ✅ Faculty see only their evaluations
- ✅ Evaluation means calculated correctly
- ✅ Spider plot shows faculty vs. all-faculty comparison
- ✅ Plus/Delta comments displayed in searchable tables
- ✅ Academic year filtering works
- ✅ Resident report cards display learning styles and topics
- ✅ Attendance tracking shows weekly view
- ✅ Observation matrix identifies gaps

## Getting Help

- **Data questions:** Check this Claude.md
- **REDCap structure:** See data dictionary files in /mnt/project/
- **Examples:** Look at gmed repository for similar visualizations
- **Testing:** Use diagnostic.R or test_connection_v2.R scripts

---

**Last Updated:** January 6, 2026
**Current Version:** v0.1 (minimal app with data display)
**Next Milestone:** v0.2 (add login module)
