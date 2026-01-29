# Leadership Dashboard Module
# Aggregate view for department leaders and division admins
# Shows division/department statistics, faculty summary table, and comparisons

mod_leader_dashboard_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filter controls
    fluidRow(
      column(
        width = 12,
        box(
          title = "Dashboard Filters",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          fluidRow(
            # Division selector (for department leaders only)
            column(
              width = 4,
              uiOutput(ns("division_selector_ui"))
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
              uiOutput(ns("dashboard_info"))
            )
          )
        )
      )
    ),

    # Faculty drill-down selector (prominent)
    fluidRow(
      column(
        width = 12,
        box(
          title = "Faculty Drill-Down",
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          uiOutput(ns("faculty_drilldown_ui")),
          tags$p(
            class = "text-info",
            style = "margin-top: 10px;",
            tags$strong("Tip:"), " Select one or more faculty members to view their detailed evaluation data, or click on a faculty name in the summary table below."
          )
        )
      )
    ),

    # Summary value boxes
    fluidRow(
      valueBoxOutput(ns("total_faculty"), width = 3),
      valueBoxOutput(ns("total_evals"), width = 3),
      valueBoxOutput(ns("avg_overall"), width = 3),
      valueBoxOutput(ns("total_assessments"), width = 3)
    ),

    # Faculty summary table (only shown in aggregate view)
    uiOutput(ns("faculty_summary_table_ui")),

    # Visualizations
    fluidRow(
      column(
        width = 6,
        box(
          title = uiOutput(ns("spider_plot_title")),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotlyOutput(ns("spider_plot"), height = "400px")
        )
      ),
      column(
        width = 6,
        box(
          title = uiOutput(ns("bar_chart_title")),
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
          title = uiOutput(ns("comparison_table_title")),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput(ns("comparison_table")),
          uiOutput(ns("comparison_table_help"))
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

    # Feedback tables (only shown when faculty selected)
    uiOutput(ns("feedback_tables_ui"))
  )
}

mod_leader_dashboard_server <- function(id, faculty_info, rdm_data, faculty_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show division selector for department leaders
    output$division_selector_ui <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level != "department_leader") {
        # Division admins see their division label only (no selector)
        div_label <- faculty_info()$accessible_divisions_label
        if (is.null(div_label)) {
          div_label <- get_division_label(faculty_info()$accessible_divisions)
        }

        tags$div(
          tags$strong("Your Division:"),
          tags$p(class = "text-info", style = "font-size: 18px; margin-top: 10px;",
                 div_label)
        )
      } else {
        # Department leaders get selector with labels
        all_divisions_df <- faculty_info()$all_divisions

        # Create choices: codes as values, labels as display names
        division_choices <- c(
          "All Divisions (Department-Wide)" = "__all__",
          setNames(all_divisions_df$fac_div, all_divisions_df$fac_div_label)
        )

        selectInput(
          ns("selected_division"),
          "Select Division:",
          choices = division_choices,
          selected = "__all__"
        )
      }
    })

    # Faculty drill-down selector
    output$faculty_drilldown_ui <- renderUI({
      req(scoped_faculty())

      tags$div(
        style = "font-size: 16px;",
        selectizeInput(
          ns("selected_faculty"),
          tags$strong("Select Faculty Member(s):"),
          choices = c("All Faculty (Aggregate)" = "__all__", setNames(scoped_faculty(), scoped_faculty())),
          selected = "__all__",
          multiple = TRUE,
          options = list(
            placeholder = 'Type to search or click on a faculty name in the table below...',
            plugins = list('remove_button')
          )
        )
      )
    })

    # Determine which faculty to show (for drilldown)
    display_faculty <- reactive({
      if (is.null(input$selected_faculty) || "__all__" %in% input$selected_faculty) {
        return("__all__")
      } else {
        return(input$selected_faculty)
      }
    })

    # Get faculty list for current scope
    scoped_faculty <- reactive({
      req(faculty_info())

      access_level <- faculty_info()$access_level

      if (access_level == "department_leader") {
        if (!is.null(input$selected_division) && input$selected_division != "__all__") {
          # Specific division
          faculty_data %>%
            filter(archived == 0, fac_div == input$selected_division) %>%
            pull(fac_name)
        } else {
          # All divisions
          faculty_info()$accessible_faculty
        }
      } else {
        # Division admin
        faculty_info()$accessible_faculty
      }
    })

    # Filtered evaluation data for scoped faculty (or drilled-down faculty)
    scoped_evals <- reactive({
      faculty_to_filter <- display_faculty()

      if (faculty_to_filter[1] == "__all__") {
        # Show all faculty in scope (division/department)
        evals <- rdm_data$faculty_evaluation %>%
          filter(fac_fell_name %in% scoped_faculty())
      } else {
        # Show only selected faculty
        evals <- rdm_data$faculty_evaluation %>%
          filter(fac_fell_name %in% faculty_to_filter)
      }

      # Apply time filter
      if (input$time_filter == "current") {
        evals <- filter_by_academic_year(evals, "fac_eval_date", "current")
      }

      evals
    })

    # All evaluations (for comparison baseline)
    all_evals <- reactive({
      rdm_data$faculty_evaluation
    })

    # Calculate means for scoped faculty or individual if selected
    scoped_means <- reactive({
      req(nrow(scoped_evals()) >= MIN_EVALUATIONS)

      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        # Aggregate view - calculate means across all scoped faculty
        calculate_faculty_eval_means(scoped_evals(), faculty_name = NULL)
      } else if (length(faculty_to_show) == 1) {
        # Single faculty selected - calculate individual means
        calculate_faculty_eval_means(scoped_evals(), faculty_name = faculty_to_show[1])
      } else {
        # Multiple faculty selected - calculate aggregate for selected group
        calculate_faculty_eval_means(scoped_evals(), faculty_name = NULL)
      }
    })

    # Calculate department-wide means
    all_means <- reactive({
      calculate_all_faculty_means(all_evals())
    })

    # Comparison data
    comparison_data <- reactive({
      req(nrow(scoped_evals()) >= MIN_EVALUATIONS)
      create_comparison_table(scoped_means(), all_means())
    })

    # Secondary metrics
    scoped_secondary_metrics <- reactive({
      req(nrow(scoped_evals()) >= MIN_EVALUATIONS)

      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        # Aggregate view
        calculate_secondary_metrics(scoped_evals(), faculty_name = NULL)
      } else if (length(faculty_to_show) == 1) {
        # Single faculty selected
        calculate_secondary_metrics(scoped_evals(), faculty_name = faculty_to_show[1])
      } else {
        # Multiple faculty selected - aggregate
        calculate_secondary_metrics(scoped_evals(), faculty_name = NULL)
      }
    })

    all_secondary_metrics <- reactive({
      calculate_secondary_metrics(all_evals(), faculty_name = NULL)
    })

    # Dashboard info
    output$dashboard_info <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level
      n_faculty <- length(scoped_faculty())
      n_evals <- nrow(scoped_evals())
      current_year <- get_current_academic_year()

      # Determine scope description
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] != "__all__") {
        # Specific faculty selected
        if (length(faculty_to_show) == 1) {
          scope_desc <- paste0("Individual: ", faculty_to_show[1])
        } else {
          scope_desc <- paste0("Selected Faculty (", length(faculty_to_show), " faculty)")
        }
      } else {
        # Aggregate view
        if (access_level == "department_leader") {
          if (!is.null(input$selected_division) && input$selected_division != "__all__") {
            scope_desc <- paste0("Division: ", get_division_label(input$selected_division))
          } else {
            scope_desc <- "All Divisions (Department-Wide)"
          }
        } else {
          div_label <- faculty_info()$accessible_divisions_label
          if (is.null(div_label)) {
            div_label <- get_division_label(faculty_info()$accessible_divisions)
          }
          scope_desc <- paste0("Division: ", div_label)
        }
      }

      time_info <- if (input$time_filter == "current") {
        paste0("Current Academic Year: ", current_year)
      } else {
        "All Time (includes evaluations without dates)"
      }

      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        tags$strong(scope_desc), tags$br(),
        tags$strong(n_evals), " evaluations", tags$br(),
        time_info
      )
    })

    # Value boxes
    output$total_faculty <- renderValueBox({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        # Aggregate view - show count
        n <- length(scoped_faculty())
        valueBox(
          n,
          "Faculty Members",
          icon = icon("user-tie"),
          color = "blue"
        )
      } else if (length(faculty_to_show) == 1) {
        # Individual view - show faculty name
        valueBox(
          icon("user"),
          faculty_to_show[1],
          icon = icon("user-tie"),
          color = "blue"
        )
      } else {
        # Multiple selected
        n <- length(faculty_to_show)
        valueBox(
          n,
          "Selected Faculty",
          icon = icon("user-tie"),
          color = "blue"
        )
      }
    })

    output$total_evals <- renderValueBox({
      n <- nrow(scoped_evals())
      faculty_to_show <- display_faculty()

      label <- if (faculty_to_show[1] == "__all__") {
        "Total Evaluations"
      } else if (length(faculty_to_show) == 1) {
        "Evaluations Received"
      } else {
        "Total Evaluations"
      }

      valueBox(
        n,
        label,
        icon = icon("clipboard-check"),
        color = "green"
      )
    })

    output$avg_overall <- renderValueBox({
      if (nrow(scoped_evals()) < MIN_EVALUATIONS) {
        valueBox("--", "Avg Overall Rating", icon = icon("star"), color = "light-blue")
      } else {
        avg <- mean(scoped_evals()$att_overall, na.rm = TRUE)
        valueBox(
          round(avg, 2),
          "Avg Overall Rating",
          icon = icon("star"),
          color = "yellow"
        )
      }
    })

    output$total_assessments <- renderValueBox({
      year <- if (input$time_filter == "current") "current" else "all"
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        # Aggregate - all scoped faculty
        assessment_counts <- count_assessments_by_faculty(
          rdm_data$assessment,
          scoped_faculty(),
          year
        )
        n <- sum(assessment_counts$assessment_count)
        label <- "Assessments Given"
      } else if (length(faculty_to_show) == 1) {
        # Individual faculty
        assessment_counts <- count_assessments_by_faculty(
          rdm_data$assessment,
          faculty_to_show,
          year
        )
        n <- sum(assessment_counts$assessment_count)
        label <- "Assessments Given"
      } else {
        # Multiple faculty
        assessment_counts <- count_assessments_by_faculty(
          rdm_data$assessment,
          faculty_to_show,
          year
        )
        n <- sum(assessment_counts$assessment_count)
        label <- "Assessments Given"
      }

      valueBox(
        n,
        label,
        icon = icon("clipboard-list"),
        color = "purple"
      )
    })

    # Faculty summary table UI (conditional)
    output$faculty_summary_table_ui <- renderUI({
      faculty_to_show <- display_faculty()

      # Only show table in aggregate view
      if (faculty_to_show[1] == "__all__") {
        fluidRow(
          column(
            width = 12,
            box(
              title = "Faculty Performance Summary",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              DT::dataTableOutput(ns("faculty_summary_table")),
              tags$p(
                class = "text-muted",
                tags$small("Shows evaluation and assessment statistics for the selected scope. ", tags$strong("Click on any row"), " to automatically select that faculty member for detailed drill-down. Evaluations Received = evaluations of this faculty member's teaching. Assessments Given = resident assessments conducted by this faculty member.")
              )
            )
          )
        )
      } else {
        # Individual view - don't show summary table
        NULL
      }
    })

    # Faculty summary table
    output$faculty_summary_table <- DT::renderDataTable({
      req(scoped_faculty())

      year <- if (input$time_filter == "current") "current" else "all"

      summary_data <- create_faculty_summary_table(
        rdm_data$faculty_evaluation,
        rdm_data$assessment,
        scoped_faculty(),
        year
      )

      datatable(
        summary_data,
        colnames = c(
          "Faculty Name",
          "Evaluations Received",
          "Avg Overall Rating",
          "Avg Time for Teaching",
          "Assessments Given"
        ),
        selection = 'single',  # Enable single row selection
        options = list(
          pageLength = 25,
          order = list(list(1, 'desc')),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = c("avg_overall", "avg_time_teaching"), digits = 2)
    })

    # Observer: When a row is clicked in the faculty summary table, update the faculty selector
    observeEvent(input$faculty_summary_table_rows_selected, {
      req(scoped_faculty())
      selected_row <- input$faculty_summary_table_rows_selected

      if (!is.null(selected_row) && length(selected_row) > 0) {
        year <- if (input$time_filter == "current") "current" else "all"
        summary_data <- create_faculty_summary_table(
          rdm_data$faculty_evaluation,
          rdm_data$assessment,
          scoped_faculty(),
          year
        )

        # Get the faculty name from the selected row
        selected_faculty_name <- summary_data$fac_fell_name[selected_row]

        # Update the selectizeInput
        updateSelectizeInput(
          session,
          "selected_faculty",
          selected = selected_faculty_name
        )
      }
    })

    # Spider plot title
    output$spider_plot_title <- renderUI({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
        "Faculty vs Department Comparison - Spider Plot"
      } else {
        "Division/Department Comparison - Spider Plot"
      }
    })

    # Spider plot
    output$spider_plot <- renderPlotly({
      if (nrow(scoped_evals()) < MIN_EVALUATIONS) {
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

        # Determine label based on what's being displayed
        if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
          # Single faculty selected - show their name
          label <- faculty_to_show[1]
        } else if (faculty_to_show[1] != "__all__") {
          # Multiple faculty selected
          label <- paste0("Selected Faculty (", length(faculty_to_show), ")")
        } else {
          # Aggregate view - use division/department label
          access_level <- faculty_info()$access_level

          if (access_level == "department_leader") {
            if (!is.null(input$selected_division) && input$selected_division != "__all__") {
              # Convert code to label for display
              label <- paste0(get_division_label(input$selected_division), " Average")
            } else {
              label <- "Department Average"
            }
          } else {
            # Division admin - use label
            div_label <- faculty_info()$accessible_divisions_label
            if (is.null(div_label)) {
              div_label <- get_division_label(faculty_info()$accessible_divisions)
            }
            label <- paste0(div_label, " Average")
          }
        }

        create_spider_plot(scoped_means(), all_means(), individual_label = label)
      }
    })

    # Bar chart title
    output$bar_chart_title <- renderUI({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
        "Faculty vs Department Comparison - Bar Chart"
      } else {
        "Division/Department Comparison - Bar Chart"
      }
    })

    # Bar chart
    output$bar_chart <- renderPlotly({
      if (nrow(scoped_evals()) < MIN_EVALUATIONS) {
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

        # Determine label based on what's being displayed
        if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
          # Single faculty selected - show their name (shortened for bar chart)
          label <- faculty_to_show[1]
        } else if (faculty_to_show[1] != "__all__") {
          # Multiple faculty selected
          label <- "Selected Faculty"
        } else {
          # Aggregate view - use division/department label
          access_level <- faculty_info()$access_level

          if (access_level == "department_leader") {
            if (!is.null(input$selected_division) && input$selected_division != "__all__") {
              # Convert code to label for display
              label <- get_division_label(input$selected_division)
            } else {
              label <- "Department"
            }
          } else {
            # Division admin - use label
            div_label <- faculty_info()$accessible_divisions_label
            if (is.null(div_label)) {
              div_label <- get_division_label(faculty_info()$accessible_divisions)
            }
            label <- div_label
          }
        }

        create_comparison_bar_chart(comparison_data(), individual_label = label)
      }
    })

    # Comparison table title
    output$comparison_table_title <- renderUI({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
        "Primary Evaluation Domains (1-5 Scale) - Individual Comparison"
      } else {
        "Primary Evaluation Domains (1-5 Scale)"
      }
    })

    # Comparison table help text
    output$comparison_table_help <- renderUI({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] != "__all__" && length(faculty_to_show) == 1) {
        tags$p(
          class = "text-muted",
          tags$small("These domains all use a 1-5 scale. Comparing this faculty member against the department average.")
        )
      } else {
        tags$p(
          class = "text-muted",
          tags$small("These domains all use a 1-5 scale and are shown in the spider plot above.")
        )
      }
    })

    # Comparison table
    output$comparison_table <- DT::renderDataTable({
      if (nrow(scoped_evals()) < MIN_EVALUATIONS) {
        datatable(
          data.frame(Message = "Minimum 5 evaluations required to display statistics"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        create_eval_comparison_table(comparison_data())
      }
    })

    # Secondary metrics table
    output$secondary_metrics_table <- DT::renderDataTable({
      if (nrow(scoped_evals()) < MIN_EVALUATIONS) {
        datatable(
          data.frame(Message = "Minimum 5 evaluations required to display statistics"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        create_secondary_metrics_table(scoped_secondary_metrics(), all_secondary_metrics())
      }
    })

    # Feedback tables UI (only show when specific faculty selected)
    output$feedback_tables_ui <- renderUI({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        # Aggregate view - don't show feedback
        return(NULL)
      } else {
        # Individual or multiple faculty selected - show feedback
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
      }
    })

    # Plus feedback table
    output$plus_table <- DT::renderDataTable({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        return(NULL)
      }

      # Get feedback for selected faculty
      feedback_data <- scoped_evals() %>%
        filter(!is.na(plus), plus != "") %>%
        select(fac_fell_name, fac_eval_date, plus) %>%
        arrange(desc(fac_eval_date))

      if (nrow(feedback_data) == 0) {
        datatable(
          data.frame(Message = "No positive feedback available for selected faculty"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        datatable(
          feedback_data,
          colnames = c("Faculty", "Date", "Positive Feedback"),
          options = list(
            pageLength = 10,
            searching = TRUE,
            order = list(list(1, 'desc'))
          ),
          rownames = FALSE
        )
      }
    })

    # Delta feedback table
    output$delta_table <- DT::renderDataTable({
      faculty_to_show <- display_faculty()

      if (faculty_to_show[1] == "__all__") {
        return(NULL)
      }

      # Get feedback for selected faculty
      feedback_data <- scoped_evals() %>%
        filter(!is.na(delta), delta != "") %>%
        select(fac_fell_name, fac_eval_date, delta) %>%
        arrange(desc(fac_eval_date))

      if (nrow(feedback_data) == 0) {
        datatable(
          data.frame(Message = "No growth feedback available for selected faculty"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        datatable(
          feedback_data,
          colnames = c("Faculty", "Date", "Areas for Growth"),
          options = list(
            pageLength = 10,
            searching = TRUE,
            order = list(list(1, 'desc'))
          ),
          rownames = FALSE
        )
      }
    })
  })
}
