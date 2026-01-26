source("global.R")

# Load modules
source("R/modules/mod_login.R")
source("R/modules/mod_faculty_eval.R")

ui <- dashboardPage(
  dashboardHeader(title = "Faculty Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
      menuItem("My Evaluations", tabName = "evaluations", icon = icon("star")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # Login tab
      tabItem(
        tabName = "login",
        mod_login_ui("login")
      ),

      # My Evaluations tab
      tabItem(
        tabName = "evaluations",
        h2("My Teaching Evaluations"),
        mod_faculty_eval_ui("faculty_eval")
      ),

      # Overview tab (summary stats)
      tabItem(
        tabName = "overview",
        h2("Faculty Dashboard Overview"),

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

  # Login module
  faculty_info <- mod_login_server("login", faculty_redcap_data)

  # Faculty evaluation module
  mod_faculty_eval_server("faculty_eval", faculty_info, rdm_redcap_data, faculty_redcap_data)

  # Hide tabs until logged in
  observe({
    if (is.null(faculty_info())) {
      hideTab(inputId = "sidebar", target = "evaluations")
      hideTab(inputId = "sidebar", target = "overview")
    } else {
      showTab(inputId = "sidebar", target = "evaluations")
      showTab(inputId = "sidebar", target = "overview")

      # Switch to evaluations tab after login
      updateTabItems(session, "sidebar", "evaluations")
    }
  })

  # Overview tab outputs
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
