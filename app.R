source("global.R")

# Load modules
source("R/modules/mod_login.R")
source("R/modules/mod_faculty_eval.R")
source("R/modules/mod_leader_dashboard.R")

ui <- bs4DashPage(
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,

  header = bs4DashNavbar(
    title = dashboardBrand(
      title = "Faculty Dashboard",
      color = "primary"
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    compact = FALSE
  ),

  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,

    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem(
        "Login",
        tabName = "login",
        icon = icon("sign-in-alt")
      ),
      bs4SidebarMenuItem(
        "My Evaluations",
        tabName = "evaluations",
        icon = icon("user-circle")
      ),
      bs4SidebarMenuItem(
        "Leadership Dashboard",
        tabName = "leader_dashboard",
        icon = icon("users")
      )
    )
  ),

  body = bs4DashBody(
    useShinyjs(),
    bs4TabItems(
      # Login tab
      bs4TabItem(
        tabName = "login",
        mod_login_ui("login")
      ),

      # My Evaluations tab
      bs4TabItem(
        tabName = "evaluations",
        mod_faculty_eval_ui("faculty_eval")
      ),

      # Leadership Dashboard tab
      bs4TabItem(
        tabName = "leader_dashboard",
        mod_leader_dashboard_ui("leader_dashboard")
      )
    )
  ),

  footer = bs4DashFooter(
    left = "Internal Medicine Faculty Dashboard",
    right = paste0("v1.0 | ", format(Sys.Date(), "%Y"))
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
      # Hide menu items using updatebs4Sidebar or hide via JS
      shinyjs::hide(selector = "a[data-value='evaluations']")
      shinyjs::hide(selector = "a[data-value='leader_dashboard']")
    } else {
      shinyjs::show(selector = "a[data-value='evaluations']")

      # Show leadership dashboard only to leaders
      access_level <- faculty_info()$access_level
      if (access_level %in% c("department_leader", "division_admin")) {
        shinyjs::show(selector = "a[data-value='leader_dashboard']")
        # Leaders default to leadership dashboard
        updatebs4TabItems(session, "sidebar", "leader_dashboard")
      } else {
        shinyjs::hide(selector = "a[data-value='leader_dashboard']")
        # Regular faculty default to evaluations
        updatebs4TabItems(session, "sidebar", "evaluations")
      }
    }
  })
}

shinyApp(ui, server)
