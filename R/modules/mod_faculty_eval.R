# Faculty Evaluation Module (Simplified)
# Displays individual faculty evaluation data with filtering and visualizations
# Leaders can select which faculty member to view

mod_faculty_eval_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filter controls
    fluidRow(
      column(
        width = 12,
        box(
          title = "Filters",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          fluidRow(
            # Faculty selector (for leaders to select individual faculty)
            column(
              width = 4,
              uiOutput(ns("faculty_selector_ui"))
            ),
            column(
              width = 4,
              radioButtons(
                ns("time_filter"),
                "Time Period:",
                choices = c(
                  "Current Academic Year" = "current",
                  "All Time" = "all"
                ),
                selected = "current"
              )
            ),
            column(
              width = 4,
              uiOutput(ns("filter_info"))
            )
          )
        )
      )
    ),

    # Summary value boxes
    fluidRow(
      valueBoxOutput(ns("total_evals"), width = 3),
      valueBoxOutput(ns("avg_overall"), width = 3),
      valueBoxOutput(ns("avg_time_teaching"), width = 3),
      valueBoxOutput(ns("plus_count"), width = 3)
    ),

    # Visualizations
    fluidRow(
      column(
        width = 6,
        box(
          title = "Evaluation Comparison - Spider Plot",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotlyOutput(ns("spider_plot"), height = "400px")
        )
      ),
      column(
        width = 6,
        box(
          title = "Evaluation Comparison - Bar Chart",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotlyOutput(ns("bar_chart"), height = "400px")
        )
      )
    ),

    # Comparison table
    fluidRow(
      column(
        width = 12,
        box(
          title = "Detailed Score Comparison",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput(ns("comparison_table"))
        )
      )
    ),

    # Feedback tables
    fluidRow(
      column(
        width = 6,
        box(
          title = "Positive Feedback (Plus)",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput(ns("plus_table"))
        )
      ),
      column(
        width = 6,
        box(
          title = "Areas for Growth (Delta)",
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          DT::dataTableOutput(ns("delta_table"))
        )
      )
    )
  )
}

mod_faculty_eval_server <- function(id, faculty_info, rdm_data, faculty_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show faculty selector for leaders
    output$faculty_selector_ui <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level == "individual") {
        # Regular faculty - no selector, show message
        tags$div(
          tags$strong("Viewing:"),
          tags$p(class = "text-info", style = "font-size: 18px; margin-top: 10px;",
                 faculty_info()$fac_name)
        )
      } else {
        # Leaders - show faculty selector
        accessible_faculty <- faculty_info()$accessible_faculty

        selectInput(
          ns("selected_faculty"),
          "Select Faculty to View:",
          choices = setNames(accessible_faculty, accessible_faculty),
          selected = accessible_faculty[1]
        )
      }
    })

    # Determine which faculty to show
    display_faculty <- reactive({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level == "individual") {
        # Regular faculty - show their own data
        return(faculty_info()$fac_name)
      } else {
        # Leaders - show selected faculty
        req(input$selected_faculty)
        return(input$selected_faculty)
      }
    })

    # Reactive: filtered evaluation data for selected faculty
    filtered_evals <- reactive({
      req(display_faculty())

      faculty_to_show <- display_faculty()

      # Get evaluations for this specific faculty member
      evals <- rdm_data$faculty_evaluation %>%
        filter(fac_fell_name == faculty_to_show)

      # Apply academic year filter
      if (input$time_filter == "current" && "fac_eval_date" %in% names(evals)) {
        evals <- filter_by_academic_year(evals, "fac_eval_date", "current")
      }

      evals
    })

    # Reactive: all faculty evaluations (for comparison baseline)
    all_evals <- reactive({
      rdm_data$faculty_evaluation
    })

    # Reactive: check if minimum threshold met
    meets_threshold <- reactive({
      check_minimum_threshold(filtered_evals())
    })

    # Reactive: calculate means for individual faculty
    individual_means <- reactive({
      req(meets_threshold())
      calculate_faculty_eval_means(filtered_evals(), faculty_name = display_faculty())
    })

    # Reactive: all faculty means for comparison
    all_means <- reactive({
      calculate_all_faculty_means(all_evals())
    })

    # Comparison data
    comparison_data <- reactive({
      req(meets_threshold())
      create_comparison_table(individual_means(), all_means())
    })

    # Filter info output
    output$filter_info <- renderUI({
      req(display_faculty())

      n_evals <- nrow(filtered_evals())
      current_year <- get_current_academic_year()
      faculty_to_show <- display_faculty()

      if (!meets_threshold()) {
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          tags$strong(faculty_to_show), tags$br(),
          sprintf(
            "Fewer than %d evaluations available for this period. Data not shown to protect privacy.",
            MIN_EVALUATIONS
          )
        )
      } else {
        if (input$time_filter == "current") {
          time_info <- paste0(" (Current year: ", current_year, ")")
          note <- "Comparison to entire department baseline."
        } else {
          time_info <- " (All time)"
          note <- "Includes evaluations without dates. Comparison to entire department baseline."
        }

        tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          tags$strong(faculty_to_show), tags$br(),
          sprintf(
            "Showing %d evaluations%s. %s",
            n_evals,
            time_info,
            note
          )
        )
      }
    })

    # Value boxes
    output$total_evals <- renderValueBox({
      n <- nrow(filtered_evals())
      valueBox(
        n,
        "Total Evaluations",
        icon = icon("clipboard-check"),
        color = if (meets_threshold()) "blue" else "red"
      )
    })

    output$avg_overall <- renderValueBox({
      if (!meets_threshold()) {
        valueBox("--", "Avg Overall", icon = icon("star"), color = "light-blue")
      } else {
        avg <- mean(filtered_evals()$att_overall, na.rm = TRUE)
        valueBox(
          round(avg, 2),
          "Avg Overall Rating",
          icon = icon("star"),
          color = "green"
        )
      }
    })

    output$avg_time_teaching <- renderValueBox({
      if (!meets_threshold()) {
        valueBox("--", "Avg Time Teaching", icon = icon("clock"), color = "light-blue")
      } else {
        avg <- mean(filtered_evals()$time_teaching, na.rm = TRUE)
        valueBox(
          round(avg, 2),
          "Avg Time for Teaching",
          icon = icon("clock"),
          color = "yellow"
        )
      }
    })

    output$plus_count <- renderValueBox({
      n <- sum(!is.na(filtered_evals()$plus) & filtered_evals()$plus != "")
      valueBox(
        n,
        "Positive Comments",
        icon = icon("thumbs-up"),
        color = "purple"
      )
    })

    # Spider plot
    output$spider_plot <- renderPlotly({
      if (!meets_threshold()) {
        plotly_empty() %>%
          layout(
            title = "Insufficient data (minimum 5 evaluations required)",
            annotations = list(
              text = "Not enough evaluations to display",
              showarrow = FALSE
            )
          )
      } else {
        create_spider_plot(individual_means(), all_means(), individual_label = "Your Scores")
      }
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      if (!meets_threshold()) {
        plotly_empty() %>%
          layout(
            title = "Insufficient data",
            annotations = list(
              text = "Not enough evaluations to display",
              showarrow = FALSE
            )
          )
      } else {
        create_comparison_bar_chart(comparison_data(), individual_label = "Your Score")
      }
    })

    # Comparison table
    output$comparison_table <- DT::renderDataTable({
      if (!meets_threshold()) {
        datatable(
          data.frame(Message = "Minimum 5 evaluations required to protect privacy"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        create_eval_comparison_table(comparison_data())
      }
    })

    # Plus feedback table
    output$plus_table <- DT::renderDataTable({
      faculty_to_show <- display_faculty()
      feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
      create_feedback_table(feedback$plus, "plus")
    })

    # Delta feedback table
    output$delta_table <- DT::renderDataTable({
      faculty_to_show <- display_faculty()
      feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
      create_feedback_table(feedback$delta, "delta")
    })
  })
}
