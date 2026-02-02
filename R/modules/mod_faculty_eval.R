# Faculty Evaluation Module
# Displays the logged-in faculty member's OWN evaluation data only
# Leaders use the Leadership Dashboard to view other faculty

mod_faculty_eval_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Prominent header showing the logged-in faculty member's name
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("faculty_header"))
      )
    ),

    # Filter controls (time period only - no faculty selector)
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
            column(
              width = 6,
              radioButtons(
                ns("time_filter"),
                "Time Period:",
                choices = c(
                  "Current Academic Year" = "current",
                  "All Time" = "all"
                ),
                selected = "current",
                inline = TRUE
              )
            ),
            column(
              width = 6,
              uiOutput(ns("filter_info"))
            )
          )
        )
      )
    ),

    # Summary value boxes - Evaluations received
    fluidRow(
      valueBoxOutput(ns("total_evals"), width = 3),
      valueBoxOutput(ns("avg_overall"), width = 3),
      valueBoxOutput(ns("avg_time_teaching"), width = 3),
      valueBoxOutput(ns("plus_count"), width = 3)
    ),

    # Assessments completed section
    fluidRow(
      column(
        width = 12,
        box(
          title = "My Resident Assessments Completed",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            valueBoxOutput(ns("my_assessments_count"), width = 4),
            valueBoxOutput(ns("assessments_vs_mean"), width = 4),
            valueBoxOutput(ns("assessments_rank"), width = 4)
          )
        )
      )
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

    # Comparison table (Primary Domains - 1-5 scale)
    fluidRow(
      column(
        width = 12,
        box(
          title = "Primary Evaluation Domains (1-5 Scale)",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput(ns("comparison_table")),
          tags$p(
            class = "text-muted",
            tags$small("These domains all use a 1-5 scale and are shown in the spider plot above.")
          )
        )
      )
    ),

    # Secondary metrics table
    fluidRow(
      column(
        width = 12,
        box(
          title = "Additional Metrics (Various Scales)",
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          DT::dataTableOutput(ns("secondary_metrics_table")),
          tags$p(
            class = "text-muted",
            tags$small("These metrics use different scales and are displayed separately from the primary domains.")
          )
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
    ),

    # Conference Attendance
    fluidRow(
      column(
        width = 8,
        box(
          title = "Conference Attendance - Last 4 Weeks",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          plotlyOutput(ns("conference_chart"), height = "400px"),
          tags$p(
            class = "text-muted",
            tags$small("Shows attendance at conferences for rotations in your division over the past 4 weeks.")
          )
        )
      ),
      column(
        width = 4,
        box(
          title = "Conference Attendance Summary",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          DT::dataTableOutput(ns("conference_summary"))
        )
      )
    )
  )
}

mod_faculty_eval_server <- function(id, faculty_info, rdm_data, faculty_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prominent header showing the logged-in faculty member's name
    output$faculty_header <- renderUI({
      req(faculty_info())

      logged_in_name <- faculty_info()$fac_name

      tags$div(
        style = "background: linear-gradient(135deg, #3c8dbc 0%, #2c6d9c 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 15px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        tags$h2(
          style = "margin: 0 0 5px 0; font-weight: bold;",
          icon("user-circle"), " ", logged_in_name
        ),
        tags$p(
          style = "margin: 0; opacity: 0.9;",
          "My Teaching Evaluation Summary"
        )
      )
    })

    # Simple reactive to get the logged-in faculty name
    # This tab ALWAYS shows only the logged-in user's data
    display_faculty <- reactive({
      req(faculty_info())
      faculty_info()$fac_name
    })

    # Reactive: filtered evaluation data for the LOGGED-IN faculty member only
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

    # Secondary metrics
    individual_secondary_metrics <- reactive({
      req(meets_threshold())
      calculate_secondary_metrics(filtered_evals(), faculty_name = display_faculty())
    })

    all_secondary_metrics <- reactive({
      calculate_secondary_metrics(all_evals(), faculty_name = NULL)
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

    # Assessment counts (faculty evaluating residents)
    assessment_stats <- reactive({
      req(display_faculty())
      get_faculty_assessment_stats(
        rdm_data$assessment,
        faculty_name = display_faculty(),
        peer_faculty_list = NULL,  # Compare to all faculty
        academic_year = input$time_filter
      )
    })

    output$my_assessments_count <- renderValueBox({
      stats <- assessment_stats()
      valueBox(
        stats$individual_count,
        "Assessments I Completed",
        icon = icon("edit"),
        color = "green"
      )
    })

    output$assessments_vs_mean <- renderValueBox({
      stats <- assessment_stats()
      diff <- stats$individual_count - stats$peer_mean
      color <- if (is.na(diff)) "light-blue" else if (diff >= 0) "green" else "yellow"
      subtitle <- if (is.na(stats$peer_mean)) {
        "vs. Peer Average"
      } else {
        paste0("vs. Avg: ", stats$peer_mean)
      }
      valueBox(
        if (is.na(diff)) "--" else paste0(ifelse(diff >= 0, "+", ""), round(diff, 0)),
        subtitle,
        icon = icon("balance-scale"),
        color = color
      )
    })

    output$assessments_rank <- renderValueBox({
      stats <- assessment_stats()
      rank_text <- if (is.na(stats$rank)) {
        "--"
      } else {
        paste0("#", stats$rank, " of ", stats$total_faculty)
      }
      valueBox(
        rank_text,
        "Rank Among Faculty",
        icon = icon("trophy"),
        color = "blue"
      )
    })

    # Spider plot
    output$spider_plot <- renderPlotly({
      if (!meets_threshold()) {
        plot_ly() %>%
          layout(
            title = "Insufficient data (minimum 5 evaluations required)",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Not enough evaluations to display",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                xanchor = "center",
                yanchor = "middle",
                showarrow = FALSE,
                font = list(size = 14)
              )
            )
          )
      } else {
        create_spider_plot(individual_means(), all_means(), individual_label = "Your Scores")
      }
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      if (!meets_threshold()) {
        plot_ly() %>%
          layout(
            title = "Insufficient data",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Not enough evaluations to display",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                xanchor = "center",
                yanchor = "middle",
                showarrow = FALSE,
                font = list(size = 14)
              )
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

    # Secondary metrics table
    output$secondary_metrics_table <- DT::renderDataTable({
      if (!meets_threshold()) {
        datatable(
          data.frame(Message = "Minimum 5 evaluations required to protect privacy"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        create_secondary_metrics_table(individual_secondary_metrics(), all_secondary_metrics())
      }
    })

    # Plus feedback table - only show if threshold met
    output$plus_table <- DT::renderDataTable({
      if (!meets_threshold()) {
        return(datatable(
          data.frame(Message = "Insufficient evaluations to display feedback"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      faculty_to_show <- display_faculty()
      feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
      create_feedback_table(feedback$plus, "plus")
    })

    # Delta feedback table - only show if threshold met
    output$delta_table <- DT::renderDataTable({
      if (!meets_threshold()) {
        return(datatable(
          data.frame(Message = "Insufficient evaluations to display feedback"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      faculty_to_show <- display_faculty()
      feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
      create_feedback_table(feedback$delta, "delta")
    })

    # Conference attendance data
    conference_rotations <- reactive({
      req(faculty_info())

      # Get faculty division and clinical site
      faculty_record <- faculty_info()
      fac_div <- faculty_record$fac_div
      fac_clin <- faculty_record$fac_clin

      # Get rotations for this faculty's division
      get_rotations_for_faculty(fac_div, fac_clin)
    })

    conference_weekly_data <- reactive({
      req(conference_rotations())

      # Get questions data
      questions_data <- rdm_data$questions

      if (is.null(questions_data) || nrow(questions_data) == 0) {
        return(NULL)
      }

      # Aggregate attendance for last 4 weeks
      aggregate_conference_attendance(questions_data, conference_rotations(), weeks = 4)
    })

    conference_yearly_data <- reactive({
      req(conference_rotations())

      # Get questions data
      questions_data <- rdm_data$questions

      if (is.null(questions_data) || nrow(questions_data) == 0) {
        return(NULL)
      }

      # Aggregate attendance for academic year
      aggregate_conference_academic_year(questions_data, conference_rotations())
    })

    # Conference chart
    output$conference_chart <- renderPlotly({
      weekly_data <- conference_weekly_data()
      create_conference_attendance_chart(weekly_data, "Conference Attendance - Last 4 Weeks")
    })

    # Conference summary table
    output$conference_summary <- DT::renderDataTable({
      weekly_data <- conference_weekly_data()
      yearly_data <- conference_yearly_data()
      create_conference_summary_table(weekly_data, yearly_data)
    })
  })
}
