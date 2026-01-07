source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Faculty Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        h2("Faculty Dashboard"),
        
        fluidRow(
          valueBoxOutput("faculty_count"),
          valueBoxOutput("eval_count"),
          valueBoxOutput("avg_rating")
        ),
        
        fluidRow(
          box(
            title = "Faculty Evaluations Sample",
            width = 12,
            DT::dataTableOutput("eval_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$faculty_count <- renderValueBox({
    valueBox(
      sum(faculty_redcap_data$archived == 0),
      "Active Faculty",
      icon = icon("user-tie"),
      color = "blue"
    )
  })
  
  output$eval_count <- renderValueBox({
    valueBox(
      nrow(rdm_redcap_data$faculty_evaluation),
      "Total Evaluations",
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$avg_rating <- renderValueBox({
    avg <- mean(rdm_redcap_data$faculty_evaluation$att_overall, na.rm = TRUE)
    valueBox(
      round(avg, 2),
      "Average Rating",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$eval_table <- DT::renderDataTable({
    rdm_redcap_data$faculty_evaluation %>%
      filter(!is.na(fac_fell_name)) %>%
      select(fac_fell_name, time_teaching, att_overall, plus, delta) %>%
      head(20) %>%
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
