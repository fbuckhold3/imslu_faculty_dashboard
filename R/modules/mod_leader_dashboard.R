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

    # Summary value boxes
    fluidRow(
      valueBoxOutput(ns("total_faculty"), width = 3),
      valueBoxOutput(ns("total_evals"), width = 3),
      valueBoxOutput(ns("avg_overall"), width = 3),
      valueBoxOutput(ns("total_assessments"), width = 3)
    ),

    # Faculty summary table
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
            tags$small("Shows evaluation and assessment statistics for the selected scope. Click on a faculty name to view their individual dashboard. Evaluations Received = evaluations of this faculty member's teaching. Assessments Given = resident assessments conducted by this faculty member.")
          )
        )
      )
    ),

    # Visualizations
    fluidRow(
      column(
        width = 6,
        box(
          title = "Division/Department Comparison - Spider Plot",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          plotlyOutput(ns("spider_plot"), height = "400px")
        )
      ),
      column(
        width = 6,
        box(
          title = "Division/Department Comparison - Bar Chart",
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
    )
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
        # Division admins see their division name only (no selector)
        tags$div(
          tags$strong("Your Division:"),
          tags$p(class = "text-info", style = "font-size: 18px; margin-top: 10px;",
                 faculty_info()$accessible_divisions)
        )
      } else {
        # Department leaders get selector
        all_divisions <- faculty_info()$all_divisions

        selectInput(
          ns("selected_division"),
          "Select Division:",
          choices = c(
            "All Divisions (Department-Wide)" = "__all__",
            setNames(all_divisions, all_divisions)
          ),
          selected = "__all__"
        )
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

    # Filtered evaluation data for scoped faculty
    scoped_evals <- reactive({
      req(scoped_faculty())

      evals <- rdm_data$faculty_evaluation %>%
        filter(fac_fell_name %in% scoped_faculty())

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

    # Calculate aggregate means for scoped faculty
    scoped_means <- reactive({
      req(nrow(scoped_evals()) >= MIN_EVALUATIONS)
      calculate_faculty_eval_means(scoped_evals(), faculty_name = NULL)
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

    # Dashboard info
    output$dashboard_info <- renderUI({
      req(faculty_info())

      access_level <- faculty_info()$access_level
      n_faculty <- length(scoped_faculty())
      n_evals <- nrow(scoped_evals())
      current_year <- get_current_academic_year()

      # Determine scope description
      if (access_level == "department_leader") {
        if (!is.null(input$selected_division) && input$selected_division != "__all__") {
          scope_desc <- paste0("Division: ", input$selected_division)
        } else {
          scope_desc <- "All Divisions (Department-Wide)"
        }
      } else {
        scope_desc <- paste0("Division: ", faculty_info()$accessible_divisions)
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
        tags$strong(n_faculty), " faculty, ",
        tags$strong(n_evals), " evaluations", tags$br(),
        time_info
      )
    })

    # Value boxes
    output$total_faculty <- renderValueBox({
      n <- length(scoped_faculty())
      valueBox(
        n,
        "Faculty Members",
        icon = icon("user-tie"),
        color = "blue"
      )
    })

    output$total_evals <- renderValueBox({
      n <- nrow(scoped_evals())
      valueBox(
        n,
        "Total Evaluations",
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

      assessment_counts <- count_assessments_by_faculty(
        rdm_data$assessment,
        scoped_faculty(),
        year
      )

      n <- sum(assessment_counts$assessment_count)
      valueBox(
        n,
        "Assessments Given",
        icon = icon("clipboard-list"),
        color = "purple"
      )
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
        access_level <- faculty_info()$access_level

        if (access_level == "department_leader") {
          if (!is.null(input$selected_division) && input$selected_division != "__all__") {
            label <- paste0(input$selected_division, " Average")
          } else {
            label <- "Department Average"
          }
        } else {
          label <- paste0(faculty_info()$accessible_divisions, " Average")
        }

        create_spider_plot(scoped_means(), all_means(), individual_label = label)
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
        access_level <- faculty_info()$access_level

        if (access_level == "department_leader") {
          if (!is.null(input$selected_division) && input$selected_division != "__all__") {
            label <- input$selected_division
          } else {
            label <- "Department"
          }
        } else {
          label <- faculty_info()$accessible_divisions
        }

        create_comparison_bar_chart(comparison_data(), individual_label = label)
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
  })
}
