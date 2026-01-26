# Faculty Evaluation Module
# Displays individual faculty evaluation data with filtering and visualizations

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
            # Division selector (for department leaders only)
            column(
              width = 3,
              uiOutput(ns("division_selector_ui"))
            ),
            # Faculty selector (for admins and leaders)
            column(
              width = 3,
              uiOutput(ns("faculty_selector_ui"))
            ),
            column(
              width = 3,
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
              width = 3,
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

    # Show division selector for department leaders
    output$division_selector_ui <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level != "department_leader") {
        # Only department leaders see division selector
        return(NULL)
      }

      all_divisions <- faculty_info()$all_divisions

      selectInput(
        ns("selected_division"),
        "Select Division:",
        choices = c(
          "All Divisions" = "__all__",
          setNames(all_divisions, all_divisions)
        ),
        selected = "__all__"
      )
    })

    # Show faculty selector for admins and leaders
    output$faculty_selector_ui <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level == "individual") {
        # Regular faculty - no selector needed
        return(NULL)
      }

      # Get accessible faculty, filtered by division if department leader
      if (access_level == "department_leader") {
        req(input$selected_division)

        if (input$selected_division == "__all__") {
          # Show all active faculty
          accessible_faculty <- faculty_data %>%
            filter(archived == 0) %>%
            arrange(fac_name) %>%
            pull(fac_name)
        } else {
          # Filter to selected division
          accessible_faculty <- faculty_data %>%
            filter(archived == 0, fac_div == input$selected_division) %>%
            arrange(fac_name) %>%
            pull(fac_name)
        }

        view_label <- "View: Division or Individual"
      } else {
        # Division admin - show their division
        accessible_faculty <- faculty_info()$accessible_faculty
        view_label <- "View: Division or Individual"
      }

      tagList(
        selectInput(
          ns("selected_faculty"),
          view_label,
          choices = c(
            "Aggregate Dashboard (Summary)" = "__dashboard__",
            setNames(accessible_faculty, accessible_faculty)
          ),
          selected = "__dashboard__"
        ),
        tags$small(
          class = "text-muted",
          "Aggregate shows summary statistics. Select individual faculty to see their comments and detailed evaluation data."
        )
      )
    })

    # Determine which faculty to show
    display_faculty <- reactive({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level == "individual") {
        # Regular faculty - show their own data
        return(faculty_info()$fac_name)
      }

      # Admin or med ed leader
      req(input$selected_faculty)

      # Validate selection
      if (is.null(input$selected_faculty) || input$selected_faculty == "") {
        return("__dashboard__")  # Default to dashboard if invalid selection
      }

      if (input$selected_faculty == "__dashboard__") {
        # Show aggregate dashboard
        return("__dashboard__")
      } else {
        # Show selected individual faculty
        return(input$selected_faculty)
      }
    })

    # Reactive: filtered evaluation data
    filtered_evals <- reactive({
      req(faculty_info(), display_faculty())

      faculty_to_show <- display_faculty()
      access_level <- faculty_info()$access_level

      # Get base evaluations
      if (faculty_to_show == "__dashboard__") {
        # Dashboard mode - determine scope based on access level and division selection

        if (access_level == "department_leader") {
          # Department leader - check if specific division selected
          if (!is.null(input$selected_division) && input$selected_division != "__all__") {
            # Filter to specific division
            division_faculty <- faculty_data %>%
              filter(archived == 0, fac_div == input$selected_division) %>%
              pull(fac_name)

            evals <- rdm_data$faculty_evaluation %>%
              filter(fac_fell_name %in% division_faculty)
          } else {
            # All divisions - show all accessible faculty
            evals <- rdm_data$faculty_evaluation %>%
              filter(fac_fell_name %in% faculty_info()$accessible_faculty)
          }
        } else {
          # Division admin - show their division
          evals <- rdm_data$faculty_evaluation %>%
            filter(fac_fell_name %in% faculty_info()$accessible_faculty)
        }
      } else {
        # Individual mode - show selected faculty
        evals <- rdm_data$faculty_evaluation %>%
          filter(fac_fell_name == faculty_to_show)
      }

      # Apply academic year filter
      if (input$time_filter == "current" && "fac_eval_date" %in% names(evals)) {
        evals <- filter_by_academic_year(evals, "fac_eval_date", "current")
      }

      evals
    })

    # Reactive: all faculty evaluations (for comparison - ENTIRE dataset, no filters)
    all_evals <- reactive({
      # Return complete dataset for comparison means
      # No time delay, no academic year filter
      rdm_data$faculty_evaluation
    })

    # Reactive: check if minimum threshold met
    meets_threshold <- reactive({
      check_minimum_threshold(filtered_evals())
    })

    # Reactive: calculate means
    individual_means <- reactive({
      req(meets_threshold())

      faculty_to_show <- display_faculty()

      if (faculty_to_show == "__dashboard__") {
        # Dashboard mode - calculate aggregate for accessible faculty
        calculate_faculty_eval_means(filtered_evals(), faculty_name = NULL)
      } else {
        # Individual mode - calculate for specific faculty
        calculate_faculty_eval_means(filtered_evals(), faculty_name = faculty_to_show)
      }
    })

    all_means <- reactive({
      calculate_all_faculty_means(all_evals())
    })

    comparison_data <- reactive({
      req(meets_threshold())
      create_comparison_table(individual_means(), all_means())
    })

    # Filter info output
    output$filter_info <- renderUI({
      req(faculty_info())

      n_evals <- nrow(filtered_evals())
      current_year <- get_current_academic_year()
      faculty_to_show <- display_faculty()

      # Determine what's being shown
      view_description <- if (faculty_to_show == "__dashboard__") {
        access_level <- faculty_info()$access_level
        if (access_level == "department_leader") {
          if (!is.null(input$selected_division) && input$selected_division != "__all__") {
            paste0("Division: ", input$selected_division, " (Aggregate)")
          } else {
            "All Divisions (Aggregate)"
          }
        } else if (access_level == "division_admin") {
          paste0("Division: ", faculty_info()$accessible_divisions, " (Aggregate)")
        } else {
          "Division (Aggregate)"
        }
      } else {
        paste0("Individual: ", faculty_to_show)
      }

      if (!meets_threshold()) {
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          tags$strong(view_description), tags$br(),
          sprintf(
            "Fewer than %d evaluations available for this period. Data not shown to protect privacy.",
            MIN_EVALUATIONS
          )
        )
      } else {
        if (input$time_filter == "current") {
          time_info <- paste0(" (Current year: ", current_year, ")")
          note <- "Comparison to entire dataset baseline."
        } else {
          time_info <- " (All time)"
          note <- "Includes evaluations without dates. Comparison to entire dataset baseline."
        }

        tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          tags$strong(view_description), tags$br(),
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
        faculty_to_show <- display_faculty()

        # Determine label
        if (faculty_to_show == "__dashboard__") {
          access_level <- faculty_info()$access_level
          if (access_level == "department_leader") {
            if (!is.null(input$selected_division) && input$selected_division != "__all__") {
              label <- paste0(input$selected_division, " Average")
            } else {
              label <- "All Divisions Average"
            }
          } else {
            label <- "Division Average"
          }
        } else {
          label <- "Your Scores"
        }

        create_spider_plot(individual_means(), all_means(), individual_label = label)
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
        faculty_to_show <- display_faculty()

        # Determine label
        if (faculty_to_show == "__dashboard__") {
          access_level <- faculty_info()$access_level
          if (access_level == "department_leader") {
            if (!is.null(input$selected_division) && input$selected_division != "__all__") {
              label <- input$selected_division
            } else {
              label <- "All Divisions"
            }
          } else {
            label <- "Division"
          }
        } else {
          label <- "Your Score"
        }

        create_comparison_bar_chart(comparison_data(), individual_label = label)
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

      if (faculty_to_show == "__dashboard__") {
        # Dashboard mode - don't show individual feedback
        datatable(
          data.frame(Message = "Select an individual faculty member to view feedback"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
        create_feedback_table(feedback$plus, "plus")
      }
    })

    # Delta feedback table
    output$delta_table <- DT::renderDataTable({
      faculty_to_show <- display_faculty()

      if (faculty_to_show == "__dashboard__") {
        # Dashboard mode - don't show individual feedback
        datatable(
          data.frame(Message = "Select an individual faculty member to view feedback"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        feedback <- get_faculty_feedback(filtered_evals(), faculty_to_show)
        create_feedback_table(feedback$delta, "delta")
      }
    })
  })
}
