# Login Module
# Handles faculty authentication and determines access level:
# - Individual: Regular faculty see only their own data
# - Division Admin: Faculty with fac_admin == "Yes" see their division
# - Department Leader: Faculty with dep_lead == "Yes" see all faculty/divisions
#
# Set PRODUCTION_MODE=TRUE environment variable to disable testing dropdown

# Check if in production mode
is_production_mode <- function() {

  prod_mode <- Sys.getenv("PRODUCTION_MODE", "FALSE")
  return(toupper(prod_mode) %in% c("TRUE", "1", "YES"))
}

mod_login_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 6,
      offset = 3,
      box(
        title = "Faculty Dashboard Login",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        # Dynamic UI based on production mode
        uiOutput(ns("login_form")),

        actionButton(
          ns("login_btn"),
          "Login",
          class = "btn-primary",
          icon = icon("sign-in-alt"),
          width = "100%"
        ),

        br(), br(),

        uiOutput(ns("login_message"))
      )
    )
  )
}

mod_login_server <- function(id, faculty_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render login form based on mode
    output$login_form <- renderUI({
      if (is_production_mode()) {
        # Production mode: access code only
        tagList(
          tags$p(
            class = "text-muted",
            "Enter your unique access code to view your dashboard."
          ),
          textInput(
            ns("access_code"),
            "Access Code:",
            placeholder = "Enter your unique access code"
          )
        )
      } else {
        # Testing mode: dropdown + access code
        tagList(
          tags$div(
            class = "alert alert-info",
            icon("flask"),
            tags$strong(" Testing Mode"),
            tags$br(),
            tags$small("Faculty dropdown is enabled for testing. Set PRODUCTION_MODE=TRUE to disable.")
          ),
          selectInput(
            ns("faculty_select"),
            "Select Faculty (Testing Only):",
            choices = NULL
          ),
          hr(),
          textInput(
            ns("access_code"),
            "Or Enter Access Code:",
            placeholder = "Enter your unique access code"
          )
        )
      }
    })

    # Populate faculty dropdown (for testing mode only)
    observe({
      if (!is_production_mode()) {
        faculty_choices <- faculty_data %>%
          filter(archived == 0) %>%
          arrange(fac_name) %>%
          pull(fac_name)

        # Set Fred Buckhold as default
        default_faculty <- "Fred Buckhold"
        if (!(default_faculty %in% faculty_choices)) {
          default_faculty <- faculty_choices[1]
        }

        updateSelectInput(
          session,
          "faculty_select",
          choices = c("", faculty_choices),
          selected = default_faculty
        )
      }
    })

    # Reactive to store faculty info after login
    faculty_info <- reactiveVal(NULL)

    # Handle login
    observeEvent(input$login_btn, {
      selected_name <- NULL

      # Try access code first
      if (!is.null(input$access_code) && input$access_code != "") {
        match <- faculty_data %>%
          filter(fac_access == input$access_code)

        if (nrow(match) == 1) {
          selected_name <- match$fac_name
        } else {
          output$login_message <- renderUI({
            tags$div(
              class = "alert alert-danger",
              icon("times-circle"),
              " Invalid access code. Please try again."
            )
          })
          return()
        }
      } else if (!is_production_mode() && !is.null(input$faculty_select) && input$faculty_select != "") {
        # Testing mode: use dropdown selection
        selected_name <- input$faculty_select
      } else {
        output$login_message <- renderUI({
          tags$div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            if (is_production_mode()) {
              " Please enter your access code."
            } else {
              " Please select a faculty member or enter an access code."
            }
          )
        })
        return()
      }

      # Get faculty record
      faculty_record <- faculty_data %>%
        filter(fac_name == selected_name)

      if (nrow(faculty_record) == 0) {
        output$login_message <- renderUI({
          tags$div(
            class = "alert alert-danger",
            icon("times-circle"),
            " Faculty not found."
          )
        })
        return()
      }

      # Determine access level
      access_level <- "individual"
      accessible_divisions <- NULL
      all_divisions <- NULL

      # Check if Department Leader (highest privilege)
      if (!is.na(faculty_record$dep_lead) && (faculty_record$dep_lead == 1 || faculty_record$dep_lead == "Yes")) {
        access_level <- "department_leader"
        all_divisions <- faculty_data %>%
          filter(archived == 0, !is.na(fac_div)) %>%
          select(fac_div) %>%
          distinct() %>%
          mutate(fac_div_label = sapply(fac_div, get_division_label)) %>%
          arrange(fac_div_label)
      } else if (!is.na(faculty_record$fac_admin) && (faculty_record$fac_admin == 1 || faculty_record$fac_admin == "Yes")) {
        # Division admin
        access_level <- "division_admin"
        accessible_divisions <- faculty_record$fac_div
        accessible_divisions_label <- get_division_label(faculty_record$fac_div)
      }

      # Get list of faculty names this user can access
      if (access_level == "department_leader") {
        if (!is.na(faculty_record$fac_clin) && faculty_record$fac_clin != "") {
          accessible_faculty <- faculty_data %>%
            filter(archived == 0, fac_clin == faculty_record$fac_clin) %>%
            pull(fac_name)
        } else {
          accessible_faculty <- faculty_data %>%
            filter(archived == 0) %>%
            pull(fac_name)
        }
      } else if (access_level == "division_admin") {
        accessible_faculty <- faculty_data %>%
          filter(
            archived == 0,
            fac_div == accessible_divisions,
            fac_clin == faculty_record$fac_clin
          ) %>%
          pull(fac_name)
      } else {
        accessible_faculty <- selected_name
      }

      # Store faculty info
      info <- list(
        record_id = faculty_record$record_id,
        fac_name = faculty_record$fac_name,
        fac_email = faculty_record$fac_email,
        fac_div = faculty_record$fac_div,
        fac_div_label = get_division_label(faculty_record$fac_div),
        fac_clin = faculty_record$fac_clin,
        access_level = access_level,
        accessible_faculty = accessible_faculty,
        accessible_divisions = accessible_divisions,
        accessible_divisions_label = if(exists("accessible_divisions_label")) accessible_divisions_label else NULL,
        all_divisions = all_divisions,
        has_full_oversight = (access_level == "department_leader" && (is.na(faculty_record$fac_clin) || faculty_record$fac_clin == ""))
      )

      faculty_info(info)

      # Show success message
      access_description <- switch(
        access_level,
        "individual" = "Individual Dashboard",
        "division_admin" = paste0("Division Leader Dashboard (",
                                   if(!is.null(info$accessible_divisions_label)) info$accessible_divisions_label else accessible_divisions, ")"),
        "department_leader" = "Department Leader Dashboard (All Faculty & Divisions)"
      )

      output$login_message <- renderUI({
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          tags$strong(" Login successful!"),
          tags$br(),
          "Welcome, ", tags$strong(faculty_record$fac_name),
          tags$br(),
          "Access Level: ", tags$strong(access_description)
        )
      })
    })

    return(faculty_info)
  })
}
