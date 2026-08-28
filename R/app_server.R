# ============================================================
# TBI NBA Basketball Operations Platform
# UI Freeze Baseline
# Includes shared transaction-state integrations through
# Player Management / Team Overview / Roster / Cap / Executive /
# Five-Year Outlook / Trade Intelligence.
# ============================================================

# ============================================================
# Basketball Operations Platform
# FINAL APP SERVER — V2 + CBA INFO HUB
# ============================================================

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Shared application reactives
  # ----------------------------------------------------------
  
  selected_team <- shiny::reactive({
    shiny::req(input$selected_team)
    input$selected_team
  })
  
  selected_season <- shiny::reactive({
    shiny::req(input$selected_season)
    input$selected_season
  })
  
  
  # ----------------------------------------------------------
  # Shared front-office scenario state
  # ----------------------------------------------------------
  
  transaction_state <- tbi_transaction_state()
  rotation_route <- tbi_rotation_route(
    Sys.getenv("TBI_ROTATION_MODEL", unset = "v2_shadow")
  )
  session$userData$rotation_model_route <- rotation_route

  if (isTRUE(session$userData$tbi_feedback_mode)) {
    shiny::observe({
      session$sendCustomMessage(
        "tbi-demo-scenario-vault-policy",
        tbi_feedback_scenario_vault_policy(
          transaction_state$snapshot(),
          feedback_mode = TRUE
        )
      )
    })
  }
  
  
  
  # ----------------------------------------------------------
  # Sidebar organization label
  # ----------------------------------------------------------
  
  output$sidebar_team_name <- shiny::renderText({
    selected_team()
  })
  
  
  # ----------------------------------------------------------
  # CBA Info Hub deep-link request
  # ----------------------------------------------------------
  
  cba_term_request <- shiny::reactiveVal(
    NULL
  )
  
  
  # ----------------------------------------------------------
  # Scenario lifecycle
  # ----------------------------------------------------------
  
  # Keep the active scenario when switching organizations.
  # This allows the same pending transaction to be inspected
  # from either team's perspective.
  
  shiny::observeEvent(
    selected_season(),
    {
      scenario <- transaction_state$snapshot()
      if (!tbi_scenario_matches_season(scenario, selected_season())) {
        transaction_state$clear()
      }
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Application modules
  # ----------------------------------------------------------
  
  depth_chart_state <- mod_depth_chart_server(
    "depth_chart",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state,
    rotation_route = rotation_route
  )
  session$userData$v2_rotation_shadow <- depth_chart_state$v2_rotation_shadow

  output$phase3_global_context <- shiny::renderUI({
    scenario <- if (!is.null(transaction_state$snapshot)) {
      transaction_state$snapshot()
    } else NULL
    v2_ui_global_context(
      depth_chart_state$v2_rotation_shadow(),
      scenario = scenario,
      team = selected_team()
    )
  })

  mod_executive_dashboard_server(
    "executive_dashboard",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state,
    v2_rotation_shadow = depth_chart_state$v2_rotation_shadow
  )
  
  mod_team_overview_server(
    "team_overview",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state
  )
  
  mod_roster_contracts_server(
    "roster_contracts",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state
  )
  
  mod_player_manager_server(
    "player_manager",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state
  )
  
  mod_salary_cap_server(
    "salary_cap",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state
  )
  
  mod_extension_simulator_server(
    "extension_simulator",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  trade_analyzer_state <- mod_trade_analyzer_server(
    "trade_analyzer",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state,
    builder_only = TRUE
  )

  v2_trade_intelligence_server(
    "trade_v2",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state,
    reset_two_team_builder = trade_analyzer_state$reset_builder
  )
  
  mod_draft_assets_server(
    "draft_assets",
    selected_team = selected_team,
    selected_season = selected_season
  )
  
  mod_five_year_outlook_server(
    "five_year_outlook",
    selected_team = selected_team,
    selected_season = selected_season,
    transaction_state = transaction_state
  )
  
  mod_cba_glossary_server(
    "cba_glossary",
    external_term = cba_term_request
  )
  
  
  # ----------------------------------------------------------
  # Navigation helper
  # ----------------------------------------------------------
  
  navigate_to <- function(page, active_id, title) {
    
    bslib::nav_select(
      id = "primary_navigation",
      selected = page,
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
  # Context-aware CBA navigation
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$cba_open_term,
    {
      
      request <- input$cba_open_term
      
      if (
        is.null(request) ||
        is.null(request$term)
      ) {
        return()
      }
      
      cba_term_request(
        list(
          term = as.character(
            request$term
          ),
          nonce = request$nonce %||%
            as.numeric(Sys.time())
        )
      )
      
      navigate_to(
        page = "cba_glossary",
        active_id = "nav_cba_glossary",
        title = "CBA Info Hub"
      )
    },
    ignoreInit = TRUE
  )
  
  
  shiny::observeEvent(
    input$cba_open_module,
    {
      
      request <- input$cba_open_module
      
      if (
        is.null(request) ||
        is.null(request$module)
      ) {
        return()
      }
      
      module_name <- as.character(
        request$module
      )
      
      module_map <- list(
        "Executive Dashboard" = list(
          page = "executive",
          active_id = "nav_executive",
          title = "Executive Dashboard"
        ),
        "Team Overview" = list(
          page = "team",
          active_id = "nav_team",
          title = "Team Overview"
        ),
        "Roster Intelligence" = list(
          page = "roster",
          active_id = "nav_roster",
          title = "Roster Intelligence"
        ),
        "Player Management" = list(
          page = "player_manager",
          active_id = "nav_player_manager",
          title = "Player Management"
        ),
        "Cap Intelligence" = list(
          page = "salary",
          active_id = "nav_salary",
          title = "Salary Cap Intelligence"
        ),
        "Salary Cap Intelligence" = list(
          page = "salary",
          active_id = "nav_salary",
          title = "Salary Cap Intelligence"
        ),
        "Trade Intelligence" = list(
          page = "trade",
          active_id = "nav_trade",
          title = "Trade Intelligence"
        ),
        "Draft Intelligence" = list(
          page = "draft",
          active_id = "nav_draft",
          title = "Draft Intelligence"
        ),
        "Extension Simulator" = list(
          page = "extension",
          active_id = "nav_extension",
          title = "Extension Simulator"
        ),
        "Five-Year Outlook" = list(
          page = "outlook",
          active_id = "nav_outlook",
          title = "Five-Year Outlook"
        )
      )
      
      target <- module_map[[module_name]]
      
      if (is.null(target)) {
        return()
      }
      
      navigate_to(
        page = target$page,
        active_id = target$active_id,
        title = target$title
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Executive
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_executive,
    {
      navigate_to(
        page = "executive",
        active_id = "nav_executive",
        title = "Executive Dashboard"
      )
    },
    ignoreInit = TRUE
  )
  
  shiny::observeEvent(
    input$nav_team,
    {
      navigate_to(
        page = "team",
        active_id = "nav_team",
        title = "Team Overview"
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Roster
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_roster,
    {
      navigate_to(
        page = "roster",
        active_id = "nav_roster",
        title = "Roster Intelligence"
      )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # Phase 15K-B4F — Dedicated Depth Chart navigation
  # ----------------------------------------------------------
  shiny::observeEvent(
    input$nav_depth,
    {
      navigate_to(
        "depth",
        "nav_depth",
        "Depth Chart"
      )
    },
    ignoreInit = TRUE
  )


  shiny::observeEvent(
    input$nav_player_manager,
    {
      navigate_to(
        page = "player_manager",
        active_id = "nav_player_manager",
        title = "Player Management"
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Financial
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_salary,
    {
      navigate_to(
        page = "salary",
        active_id = "nav_salary",
        title = "Salary Cap Intelligence"
      )
    },
    ignoreInit = TRUE
  )
  
  shiny::observeEvent(
    input$nav_extension,
    {
      navigate_to(
        page = "extension",
        active_id = "nav_extension",
        title = "Extension Simulator"
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Transactions
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_trade,
    {
      navigate_to(
        page = "trade",
        active_id = "nav_trade",
        title = "Trade Intelligence"
      )
    },
    ignoreInit = TRUE
  )
  
  shiny::observeEvent(
    input$nav_draft,
    {
      navigate_to(
        page = "draft",
        active_id = "nav_draft",
        title = "Draft Intelligence"
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Strategy
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_outlook,
    {
      navigate_to(
        page = "outlook",
        active_id = "nav_outlook",
        title = "Five-Year Outlook"
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Reference
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$nav_cba_glossary,
    {
      navigate_to(
        page = "cba_glossary",
        active_id = "nav_cba_glossary",
        title = "CBA Info Hub"
      )
    },
    ignoreInit = TRUE
  )
}
