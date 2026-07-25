#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  
  selected_team <- reactive({
    req(input$selected_team)
    input$selected_team
  })
  
  mod_executive_dashboard_server(
    "executive_dashboard",
    selected_team = selected_team
  )
  
  mod_team_overview_server("team_overview")
  mod_roster_contracts_server("roster_contracts")
  mod_salary_cap_server("salary_cap")
  mod_trade_analyzer_server("trade_analyzer")
  mod_five_year_outlook_server("five_year_outlook")
  mod_draft_assets_server("draft_assets")
  mod_extension_simulator_server("extension_simulator")
}