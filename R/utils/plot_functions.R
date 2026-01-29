library(plotly)
library(DT)
library(tidyverse)

# ==============================================================================
# Plotly Visualization Functions
# ==============================================================================

#' Create spider/radar plot comparing individual faculty to all faculty
#'
#' @param individual_means Mean scores for individual faculty
#' @param all_means Mean scores for all faculty
#' @param individual_label Label for individual/group (default: "Your Scores")
#' @return Plotly radar chart object
create_spider_plot <- function(individual_means, all_means, individual_label = "Your Scores") {
  # Combine data and add labels
  plot_data <- individual_means %>%
    select(domain, individual_score = mean_score) %>%
    left_join(
      all_means %>% select(domain, all_score = mean_score),
      by = "domain"
    ) %>%
    mutate(domain_label = sapply(domain, get_domain_label))

  # Create radar chart
  fig <- plot_ly(
    type = 'scatterpolar',
    mode = 'lines+markers',
    fill = 'toself'
  )

  # Add trace for all faculty (comparison)
  fig <- fig %>%
    add_trace(
      r = plot_data$all_score,
      theta = plot_data$domain_label,
      name = 'All Faculty Average',
      fillcolor = 'rgba(200, 200, 200, 0.3)',
      line = list(color = 'rgba(100, 100, 100, 0.8)')
    )

  # Add trace for individual faculty
  fig <- fig %>%
    add_trace(
      r = plot_data$individual_score,
      theta = plot_data$domain_label,
      name = individual_label,
      fillcolor = 'rgba(67, 133, 245, 0.3)',
      line = list(color = 'rgba(67, 133, 245, 1)')
    )

  # Layout
  fig <- fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 5),
          tickmode = 'linear',
          tick0 = 0,
          dtick = 1
        )
      ),
      showlegend = TRUE,
      title = "Teaching Evaluation Comparison"
    )

  fig
}

#' Create bar chart comparing scores
#'
#' @param comparison_data Data frame with individual and all faculty scores
#' @param individual_label Label for individual/group (default: "Your Score")
#' @return Plotly bar chart object
create_comparison_bar_chart <- function(comparison_data, individual_label = "Your Score") {
  # Add labels
  plot_data <- comparison_data %>%
    mutate(domain_label = sapply(domain, get_domain_label))

  fig <- plot_ly(plot_data)

  # All faculty bar
  fig <- fig %>%
    add_trace(
      x = ~domain_label,
      y = ~all_score,
      name = 'All Faculty',
      type = 'bar',
      marker = list(color = 'rgba(200, 200, 200, 0.7)')
    )

  # Individual faculty bar
  fig <- fig %>%
    add_trace(
      x = ~domain_label,
      y = ~individual_score,
      name = individual_label,
      type = 'bar',
      marker = list(color = 'rgba(67, 133, 245, 0.8)')
    )

  # Layout
  fig <- fig %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Mean Score", range = c(0, 5)),
      barmode = 'group',
      title = "Teaching Evaluation Scores"
    )

  fig
}

# ==============================================================================
# DataTable Functions
# ==============================================================================

#' Create formatted comparison table
#'
#' @param comparison_data Data frame with comparison metrics
#' @return DT::datatable object
create_eval_comparison_table <- function(comparison_data) {
  # Prepare data with labels and formatting
  table_data <- comparison_data %>%
    mutate(
      Domain = sapply(domain, get_domain_label),
      `Your Score` = round(individual_score, 2),
      `All Faculty` = round(all_score, 2),
      Difference = round(difference, 2),
      `Your N` = individual_n,
      `All N` = all_n
    ) %>%
    select(Domain, `Your Score`, `All Faculty`, Difference, `Your N`, `All N`)

  # Create datatable with color coding
  datatable(
    table_data,
    options = list(
      pageLength = 10,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:5)
      )
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'Difference',
      backgroundColor = styleInterval(
        cuts = c(-0.01, 0.01),
        values = c('rgba(255, 99, 71, 0.3)', 'rgba(255, 255, 255, 0)', 'rgba(144, 238, 144, 0.3)')
      )
    )
}

#' Create feedback table (plus or delta)
#'
#' @param feedback_data Data frame with eval_date and feedback text
#' @param feedback_type "plus" or "delta"
#' @return DT::datatable object
create_feedback_table <- function(feedback_data, feedback_type = "plus") {
  # Determine column name
  text_col <- if (feedback_type == "plus") "plus" else "delta"

  # Check if column exists
  if (!text_col %in% names(feedback_data)) {
    return(datatable(data.frame(Message = "No feedback data available")))
  }

  # Prepare data
  if (nrow(feedback_data) == 0) {
    return(datatable(
      data.frame(Message = paste0("No ", feedback_type, " feedback recorded")),
      options = list(dom = 't')
    ))
  }

  table_data <- feedback_data %>%
    mutate(
      Year = if_else(
        is.na(fac_eval_date),
        "Unknown",
        assign_academic_year(fac_eval_date)
      ),
      Feedback = .data[[text_col]]
    ) %>%
    select(Year, Feedback)

  # Create table
  datatable(
    table_data,
    options = list(
      pageLength = 10,
      searching = TRUE,
      paging = TRUE,
      order = list(list(0, 'desc'))  # Sort by year descending
    ),
    rownames = FALSE,
    class = 'cell-border stripe',
    filter = 'top'
  )
}

# ==============================================================================
# Secondary Metrics Display
# ==============================================================================

#' Create table for secondary metrics (different scales from primary domains)
#'
#' @param individual_metrics Secondary metrics for individual faculty
#' @param all_metrics Secondary metrics for all faculty
#' @return DT::datatable object
create_secondary_metrics_table <- function(individual_metrics, all_metrics) {
  # Prepare data with labels
  table_data <- individual_metrics %>%
    rename(
      individual_value = mean_value,
      individual_n = n
    ) %>%
    left_join(
      all_metrics %>%
        select(metric, all_value = mean_value, all_n = n),
      by = "metric"
    ) %>%
    mutate(
      Metric = sapply(metric, get_domain_label),
      `Your Score` = round(individual_value, 2),
      `All Faculty` = round(all_value, 2),
      Difference = round(individual_value - all_value, 2),
      `Your N` = individual_n,
      `All N` = all_n
    ) %>%
    select(Metric, `Your Score`, `All Faculty`, Difference, `Your N`, `All N`)

  # Create datatable
  datatable(
    table_data,
    options = list(
      pageLength = 10,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:5)
      )
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'Difference',
      backgroundColor = styleInterval(
        cuts = c(-0.01, 0.01),
        values = c('rgba(255, 99, 71, 0.3)', 'rgba(255, 255, 255, 0)', 'rgba(144, 238, 144, 0.3)')
      )
    )
}

# ==============================================================================
# Summary Boxes
# ==============================================================================

#' Calculate summary statistics for value boxes
#'
#' @param eval_data Evaluation data for a faculty member
#' @return List with summary stats
calculate_eval_summary <- function(eval_data) {
  list(
    total_evals = nrow(eval_data),
    mean_overall = mean(eval_data$att_overall, na.rm = TRUE),
    mean_time_teaching = mean(eval_data$time_teaching, na.rm = TRUE),
    plus_count = sum(!is.na(eval_data$plus) & eval_data$plus != ""),
    delta_count = sum(!is.na(eval_data$delta) & eval_data$delta != "")
  )
}

# ==============================================================================
# Conference Attendance Visualizations
# ==============================================================================

#' Create stacked bar chart for conference attendance over time
#'
#' @param attendance_data Data frame with week_label, rotation_name, attendance_count
#' @param title Chart title
#' @return Plotly object
create_conference_attendance_chart <- function(attendance_data, title = "Conference Attendance - Last 4 Weeks") {
  # Check if we have data
  if (is.null(attendance_data) || nrow(attendance_data) == 0) {
    # Create empty plot with message
    return(
      plot_ly() %>%
        layout(
          title = "No conference attendance data available",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "No attendance records found for the selected rotations in the last 4 weeks",
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
    )
  }

  # Create stacked bar chart
  plot_ly(
    data = attendance_data,
    x = ~week_label,
    y = ~attendance_count,
    color = ~rotation_name,
    type = "bar",
    text = ~paste0(rotation_name, ": ", attendance_count),
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "%{text}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = title,
      xaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = c("3 Weeks Ago", "2 Weeks Ago", "1 Week Ago", "This Week")
      ),
      yaxis = list(
        title = "Attendance Count"
      ),
      barmode = "stack",
      legend = list(
        title = list(text = "Rotation"),
        orientation = "v",
        x = 1.02,
        y = 1
      ),
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE)
}

#' Create summary table for conference attendance
#'
#' @param weekly_data Data frame with weekly attendance
#' @param yearly_data Data frame with academic year totals
#' @return DT::datatable object
create_conference_summary_table <- function(weekly_data, yearly_data) {
  # If no data, return empty message
  if ((is.null(weekly_data) || nrow(weekly_data) == 0) &&
      (is.null(yearly_data) || nrow(yearly_data) == 0)) {
    return(
      datatable(
        data.frame(Message = "No conference attendance data available"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    )
  }

  # Aggregate weekly data by rotation
  if (!is.null(weekly_data) && nrow(weekly_data) > 0) {
    weekly_totals <- weekly_data %>%
      group_by(rotation_name) %>%
      summarize(last_4_weeks = sum(attendance_count), .groups = "drop")
  } else {
    weekly_totals <- tibble(rotation_name = character(), last_4_weeks = numeric())
  }

  # Get yearly totals
  if (!is.null(yearly_data) && nrow(yearly_data) > 0) {
    yearly_totals <- yearly_data %>%
      select(rotation_name, academic_year = attendance_count)
  } else {
    yearly_totals <- tibble(rotation_name = character(), academic_year = numeric())
  }

  # Combine
  summary_table <- full_join(weekly_totals, yearly_totals, by = "rotation_name") %>%
    mutate(
      last_4_weeks = replace_na(last_4_weeks, 0),
      academic_year = replace_na(academic_year, 0)
    ) %>%
    rename(
      Rotation = rotation_name,
      `Last 4 Weeks` = last_4_weeks,
      `Academic Year` = academic_year
    )

  # Create datatable
  datatable(
    summary_table,
    options = list(
      pageLength = 15,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:2)
      )
    ),
    rownames = FALSE
  )
}
