# Login Module
# Handles faculty authentication and determines access level:
# - Individual: Regular faculty see only their own data
# - Division Admin: Faculty with fac_admin == "Yes" see their division
# - Department Leader: Faculty with dep_lead == "Yes" see all faculty/divisions

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

        # Testing mode: Select faculty from dropdown
        # Production mode: Enter access code
        selectInput(
          ns("faculty_select"),
          "Select Faculty (Testing Mode):",
          choices = NULL,  # Populated in server
          selected = NULL
        ),

        hr(),

        # Access code input (for production)
        textInput(
          ns("access_code"),
          "Or Enter Access Code:",
          placeholder = "Enter your unique access code"
        ),

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

    # Populate faculty dropdown (for testing)
    observe({
      faculty_choices <- faculty_data %>%
        filter(archived == 0) %>%  # Only active faculty
        arrange(fac_name) %>%
        pull(fac_name)

      # Set Fred Buckhold as default
      default_faculty <- "Fred Buckhold"
      if (!(default_faculty %in% faculty_choices)) {
        default_faculty <- faculty_choices[1]  # Fallback if Fred not found
      }

      updateSelectInput(
        session,
        "faculty_select",
        choices = c("", faculty_choices),
        selected = default_faculty
      )
    })

    # Reactive to store faculty info after login
    faculty_info <- reactiveVal(NULL)

    # Handle login
    observeEvent(input$login_btn, {
      # Try access code first, then dropdown
      selected_name <- NULL

      if (!is.null(input$access_code) && input$access_code != "") {
        # Production mode: validate access code
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
      } else if (!is.null(input$faculty_select) && input$faculty_select != "") {
        # Testing mode: use dropdown selection
        selected_name <- input$faculty_select
      } else {
        output$login_message <- renderUI({
          tags$div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please select a faculty member or enter an access code."
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
      if (!is.na(faculty_record$dep_lead) && faculty_record$dep_lead == "Yes") {
        access_level <- "department_leader"
        # Get all unique divisions for selector
        all_divisions <- faculty_data %>%
          filter(archived == 0, !is.na(fac_div)) %>%
          pull(fac_div) %>%
          unique() %>%
          sort()
      } else if (!is.na(faculty_record$fac_admin) && faculty_record$fac_admin == "Yes") {
        # Division admin - can see their specific division
        access_level <- "division_admin"
        accessible_divisions <- faculty_record$fac_div
      }

      # Get list of faculty names this user can access
      if (access_level == "department_leader") {
        accessible_faculty <- faculty_data %>%
          filter(archived == 0) %>%
          pull(fac_name)
      } else if (access_level == "division_admin") {
        accessible_faculty <- faculty_data %>%
          filter(archived == 0, fac_div == accessible_divisions) %>%
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
        access_level = access_level,
        accessible_faculty = accessible_faculty,
        accessible_divisions = accessible_divisions,
        all_divisions = all_divisions
      )

      faculty_info(info)

      # Show success message
      access_description <- switch(
        access_level,
        "individual" = "Individual Dashboard",
        "division_admin" = paste0("Division Leader Dashboard (", accessible_divisions, ")"),
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

    # Return faculty_info reactive
    return(faculty_info)
  })
}
