source("global.R")

# Load modules
source("R/modules/mod_login.R")
source("R/modules/mod_faculty_eval.R")
source("R/modules/mod_leader_dashboard.R")

ui <- dashboardPage(
  dashboardHeader(title = "Faculty Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
      menuItem("My Evaluations", tabName = "evaluations", icon = icon("star")),
      menuItem("Leadership Dashboard", tabName = "leader_dashboard", icon = icon("chart-line"))
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
        mod_faculty_eval_ui("faculty_eval")
      ),

      # Leadership Dashboard tab
      tabItem(
        tabName = "leader_dashboard",
        mod_leader_dashboard_ui("leader_dashboard")
      )
    )
  )
)

server <- function(input, output, session) {

  # Login module
  faculty_info <- mod_login_server("login", faculty_redcap_data)

  # Faculty evaluation module
  mod_faculty_eval_server("faculty_eval", faculty_info, rdm_redcap_data, faculty_redcap_data)

  # Leadership dashboard module
  mod_leader_dashboard_server("leader_dashboard", faculty_info, rdm_redcap_data, faculty_redcap_data)

  # Hide tabs until logged in
  observe({
    if (is.null(faculty_info())) {
      hideTab(inputId = "sidebar", target = "evaluations")
      hideTab(inputId = "sidebar", target = "leader_dashboard")
    } else {
      showTab(inputId = "sidebar", target = "evaluations")

      # Show leadership dashboard only to leaders
      access_level <- faculty_info()$access_level
      if (access_level %in% c("department_leader", "division_admin")) {
        showTab(inputId = "sidebar", target = "leader_dashboard")
        # Leaders default to leadership dashboard
        updateTabItems(session, "sidebar", "leader_dashboard")
      } else {
        hideTab(inputId = "sidebar", target = "leader_dashboard")
        # Regular faculty default to evaluations
        updateTabItems(session, "sidebar", "evaluations")
      }
    }
  })
}

shinyApp(ui, server)
