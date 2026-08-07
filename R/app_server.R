#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Primary application navigation
  # ----------------------------------------------------------
  
  navigation_map <- list(
    nav_executive = list(
      panel = "executive",
      title = "Executive Dashboard"
    ),
    nav_team = list(
      panel = "team",
      title = "Team Overview"
    ),
    nav_roster = list(
      panel = "roster",
      title = "Roster Intelligence"
    ),
    nav_player_manager = list(
      panel = "player_manager",
      title = "Player Management"
    ),
    nav_salary = list(
      panel = "salary",
      title = "Salary Cap Intelligence"
    ),
    nav_extension = list(
      panel = "extension",
      title = "Extension Simulator"
    ),
    nav_trade = list(
      panel = "trade",
      title = "Trade Intelligence"
    ),
    nav_draft = list(
      panel = "draft",
      title = "Draft Intelligence"
    ),
    nav_outlook = list(
      panel = "outlook",
      title = "Five-Year Outlook"
    )
  )
  
  
  # ----------------------------------------------------------
  # Navigation helper
  # ----------------------------------------------------------
  
  select_navigation <- function(panel, title, active_id) {
    
    bslib::nav_select(
      id = "primary_navigation",
      selected = panel,
      session = session
    )
    
    session$sendCustomMessage(
      type = "tbi-set-navigation",
      message = list(
        activeId = active_id,
        title = title
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Navigation events
  # ----------------------------------------------------------
  
  shiny::observeEvent(input$nav_executive, {
    select_navigation(
      panel = "executive",
      title = "Executive Dashboard",
      active_id = "nav_executive"
    )
  })
  
  shiny::observeEvent(input$nav_team, {
    select_navigation(
      panel = "team",
      title = "Team Overview",
      active_id = "nav_team"
    )
  })
  
  shiny::observeEvent(input$nav_roster, {
    select_navigation(
      panel = "roster",
      title = "Roster Intelligence",
      active_id = "nav_roster"
    )
  })
  
  shiny::observeEvent(input$nav_player_manager, {
    select_navigation(
      panel = "player_manager",
      title = "Player Management",
      active_id = "nav_player_manager"
    )
  })
  
  shiny::observeEvent(input$nav_salary, {
    select_navigation(
      panel = "salary",
      title = "Salary Cap Intelligence",
      active_id = "nav_salary"
    )
  })
  
  shiny::observeEvent(input$nav_extension, {
    select_navigation(
      panel = "extension",
      title = "Extension Simulator",
      active_id = "nav_extension"
    )
  })
  
  shiny::observeEvent(input$nav_trade, {
    select_navigation(
      panel = "trade",
      title = "Trade Intelligence",
      active_id = "nav_trade"
    )
  })
  
  shiny::observeEvent(input$nav_draft, {
    select_navigation(
      panel = "draft",
      title = "Draft Intelligence",
      active_id = "nav_draft"
    )
  })
  
  shiny::observeEvent(input$nav_outlook, {
    select_navigation(
      panel = "outlook",
      title = "Five-Year Outlook",
      active_id = "nav_outlook"
    )
  })
  
  
  # ----------------------------------------------------------
  # Shared application filters
  # ----------------------------------------------------------
  
  output$sidebar_team_name <- shiny::renderText({
    input$selected_team
  })
  
  selected_team <- shiny::reactive({
    shiny::req(input$selected_team)
    input$selected_team
  })
  
  selected_season <- shiny::reactive({
    shiny::req(input$selected_season)
    input$selected_season
  })
  
  
  # ----------------------------------------------------------
  # Application modules
  # ----------------------------------------------------------
  
  mod_executive_dashboard_server(
    "executive_dashboard",
    selected_team = selected_team
  )
  
  mod_team_overview_server(
    "team_overview",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_depth_chart_server(
    "depth_chart",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_roster_contracts_server(
    "roster_contracts",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_player_manager_server(
    "player_manager",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_salary_cap_server(
    "salary_cap",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_trade_analyzer_server(
    "trade_analyzer",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_five_year_outlook_server(
    "five_year_outlook",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_draft_assets_server(
    "draft_assets",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_extension_simulator_server(
    "extension_simulator",
    selected_team = selected_team,
    selected_season = selected_season
  )
}