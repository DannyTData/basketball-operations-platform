# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 13
# Finalization / Main-App Wiring Helpers
# ============================================================


# ------------------------------------------------------------
# UI mount helper
#
# Put this where the final Basketball Intelligence page/panel
# should render:
#
#   phase3_dashboard_app_ui("phase3_basketball")
# ------------------------------------------------------------

phase3_dashboard_app_ui <- function(
    id = "phase3_basketball") {
  
  mod_phase3_dashboard_ui(
    id
  )
}


# ------------------------------------------------------------
# Server mount helper
#
# Put this inside server:
#
#   phase3_dashboard_app_server(
#     "phase3_basketball",
#     team_name = reactive(input$organization),
#     season = reactive(input$season)
#   )
# ------------------------------------------------------------

phase3_dashboard_app_server <- function(
    id = "phase3_basketball",
    team_name,
    season) {
  
  mod_phase3_dashboard_server(
    id =
      id,
    team_name =
      team_name,
    season =
      season
  )
}


# ------------------------------------------------------------
# Final system preflight
# ------------------------------------------------------------

phase3_final_preflight <- function(
    team_name = "Boston Celtics",
    season = "2025-26") {
  
  required <- c(
    "bie_capabilities",
    "bie_phase3_final_healthcheck",
    "print_bie_phase3_freeze_report",
    "mod_phase3_dashboard_ui",
    "mod_phase3_dashboard_server"
  )
  
  missing <- required[
    !vapply(
      required,
      exists,
      logical(1),
      mode = "function",
      inherits = TRUE
    )
  ]
  
  if (length(missing)) {
    return(
      list(
        status =
          "REVIEW",
        missing_functions =
          missing
      )
    )
  }
  
  result <-
    bie_phase3_final_healthcheck(
      team_name =
        team_name,
      season =
        season
    )
  
  list(
    status =
      result$status,
    engine_version =
      result$engine_version,
    validation_team =
      result$validation_team,
    season =
      result$season,
    player_impact_rows =
      result$player_impact_rows,
    recommendation_sanity_flags =
      result$recommendation_sanity_flags,
    lineup_suite_ready =
      result$lineup_suite_ready,
    rotation_minutes =
      result$rotation_minutes,
    dashboard_functions_ready =
      result$dashboard_functions_ready
  )
}


# ------------------------------------------------------------
# Freeze marker
#
# Does NOT modify the database.
# It returns the canonical freeze metadata after a clean check.
# ------------------------------------------------------------

phase3_freeze_marker <- function(
    team_name = "Boston Celtics",
    season = "2025-26") {
  
  health <-
    bie_phase3_final_healthcheck(
      team_name =
        team_name,
      season =
        season
    )
  
  if (
    !identical(
      health$status,
      "READY TO FREEZE"
    )
  ) {
    stop(
      "Phase 3 is not ready to freeze. Run print_bie_phase3_freeze_report() and review the failing checks."
    )
  }
  
  list(
    project =
      "TBI NBA Basketball Operations Platform",
    engine =
      "Thompson's Basketball Intelligence",
    phase =
      "Phase 3",
    status =
      "FROZEN",
    version =
      health$engine_version,
    season =
      season,
    validation_team =
      team_name,
    performance_layer =
      "ACTIVE",
    player_roles =
      "ACTIVE",
    player_impact =
      "ACTIVE",
    lineup_rotation =
      "ACTIVE",
    front_office_recommendations =
      "ACTIVE",
    dashboard =
      "ACTIVE",
    tracking_matchups =
      "NOT LOADED",
    projections =
      "NOT LOADED"
  )
}