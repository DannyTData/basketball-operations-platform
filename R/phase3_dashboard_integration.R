# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 12
# Dashboard Integration
#
# Purpose:
#   Surface the Phase-3 basketball intelligence stack inside
#   the Shiny application without modifying the giant BIE core.
#
# Displays:
#   - Team performance summary
#   - Player BIE ratings
#   - Roles / archetypes
#   - Front-office recommendations
#   - Starting / closing / offense / defense / balanced lineups
#   - 240-minute rotation
#   - Roster needs
#   - Executive recommendation summary
#
# Dependencies:
#   - Step 8 player impact
#   - Step 9 lineup / rotation
#   - Step 10 decision integration
#   - Step 11 calibrated recommendations
#   - shiny
#   - bslib
# ============================================================


# ------------------------------------------------------------
# Namespace-safe helpers
# ------------------------------------------------------------

p3s12_num <- function(
    x,
    default = NA_real_) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !is.finite(value[[1]])
  ) {
    return(default)
  }
  
  value[[1]]
}


p3s12_text <- function(
    x,
    default = "") {
  
  value <- as.character(x)
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !nzchar(
      trimws(
        value[[1]]
      )
    )
  ) {
    return(default)
  }
  
  trimws(
    value[[1]]
  )
}


p3s12_round <- function(
    x,
    digits = 1) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  ifelse(
    is.finite(value),
    round(
      value,
      digits
    ),
    NA_real_
  )
}


# ------------------------------------------------------------
# Package guard
# ------------------------------------------------------------

phase3_step12_require_packages <- function() {
  
  required <- c(
    "shiny",
    "bslib"
  )
  
  missing <- required[
    !vapply(
      required,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  
  if (length(missing)) {
    
    stop(
      paste0(
        "Install required packages first: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# UI helper: metric card
# ------------------------------------------------------------

p3s12_metric_card <- function(
    title,
    value,
    subtitle = NULL) {
  
  phase3_step12_require_packages()
  
  bslib::card(
    class = "h-100",
    bslib::card_header(
      shiny::tags$strong(
        title
      )
    ),
    shiny::div(
      class = "p-3",
      shiny::div(
        style =
          "font-size:2rem;font-weight:700;line-height:1;",
        value
      ),
      if (
        !is.null(
          subtitle
        )
      ) {
        shiny::div(
          class = "text-muted mt-2",
          subtitle
        )
      }
    )
  )
}


# ------------------------------------------------------------
# UI helper: badge
# ------------------------------------------------------------

p3s12_badge <- function(text) {
  
  shiny::span(
    class = "badge rounded-pill text-bg-secondary",
    p3s12_text(
      text,
      "Unavailable"
    )
  )
}


# ------------------------------------------------------------
# Table formatter
# ------------------------------------------------------------

p3s12_player_table <- function(df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- df
  
  numeric_cols <- intersect(
    c(
      "bie_performance_rating",
      "bie_performance_percentile",
      "offensive_impact_score",
      "defensive_impact_score",
      "team_importance_score",
      "roster_need_match",
      "recommended_minutes"
    ),
    names(d)
  )
  
  for (column in numeric_cols) {
    
    d[[column]] <-
      p3s12_round(
        d[[column]],
        1
      )
  }
  
  d
}


# ------------------------------------------------------------
# Friendly dashboard column labels
# ------------------------------------------------------------

p3s12_friendly_names <- function(df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !ncol(df)
  ) {
    return(df)
  }
  
  label_map <- c(
    player_name = "Player",
    primary_position = "Pos",
    bie_performance_rating = "BIE Rating",
    bie_performance_percentile = "BIE %ile",
    impact_tier = "Impact Tier",
    impact_confidence = "Confidence",
    offensive_impact_score = "Offense",
    defensive_impact_score = "Defense",
    primary_role = "Primary Role",
    archetype = "Archetype",
    evaluation_label = "Stats / Evidence From",
    evaluation_source = "Evaluation Source",
    current_team_name = "Current Team",
    performance_team_name = "Performance Team",
    recommendation = "Action",
    recommendation_confidence = "Action Conf.",
    team_importance_score = "Team Importance",
    roster_need_match = "Need Fit",
    recommended_minutes = "Min",
    rotation_rank = "#"
  )
  
  current <- names(df)
  
  names(df) <- ifelse(
    current %in%
      names(label_map),
    unname(
      label_map[
        current
      ]
    ),
    current
  )
  
  df
}


# ------------------------------------------------------------
# Self-contained front-office decision package
#
# Uses Step-10 package helper when available.
# Falls back to direct Phase-3 / legacy BIE calls otherwise.
# ------------------------------------------------------------

p3s12_front_office_decision_package <- function(
    team_name,
    season = "2025-26") {
  
  if (
    exists(
      "run_phase3_front_office_decision_package",
      mode = "function",
      inherits = TRUE
    )
  ) {
    
    return(
      run_phase3_front_office_decision_package(
        team_name =
          team_name,
        season =
          season
      )
    )
  }
  
  # ----------------------------------------------------------
  # Resolve integrated roster
  # ----------------------------------------------------------
  
  roster <- NULL
  
  if (
    exists(
      "p3s11_get_integrated_team_roster",
      mode = "function",
      inherits = TRUE
    )
  ) {
    
    roster <-
      p3s11_get_integrated_team_roster(
        team_name =
          team_name,
        season =
          season
      )
    
  } else if (
    exists(
      "get_phase3_integrated_team_roster",
      mode = "function",
      inherits = TRUE
    )
  ) {
    
    roster <-
      get_phase3_integrated_team_roster(
        team_name =
          team_name,
        season =
          season
      )
  }
  
  if (
    is.null(roster) ||
    !is.data.frame(roster) ||
    !nrow(roster)
  ) {
    
    stop(
      paste(
        "Step 12 could not build the integrated team roster.",
        "Load Step 11.2 or Step 10 before Step 12."
      )
    )
  }
  
  # ----------------------------------------------------------
  # Required legacy decision functions
  # ----------------------------------------------------------
  
  required <- c(
    "evaluate_bie_roster_decisions",
    "evaluate_bie_roster_needs",
    "evaluate_bie_executive_front_office"
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
    
    stop(
      paste0(
        "Step 12 is missing required BIE functions: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  roster_decision <-
    evaluate_bie_roster_decisions(
      roster_players =
        roster
    )
  
  roster_needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster,
      roster_decision =
        roster_decision
    )
  
  executive <-
    evaluate_bie_executive_front_office(
      roster_decision =
        roster_decision,
      roster_needs =
        roster_needs,
      competitive_tier =
        "Unknown",
      financial_status =
        "Unknown"
    )
  
  lineup_rotation <- if (
    exists(
      "run_phase3_lineup_rotation_optimization",
      mode = "function",
      inherits = TRUE
    )
  ) {
    
    run_phase3_lineup_rotation_optimization(
      team_name =
        team_name,
      season =
        season
    )
    
  } else {
    
    list(
      status =
        "STEP 9 NOT LOADED"
    )
  }
  
  list(
    status =
      "OK",
    team =
      team_name,
    season =
      season,
    roster =
      roster,
    roster_decision =
      roster_decision,
    roster_needs =
      roster_needs,
    lineup_rotation =
      lineup_rotation,
    executive =
      executive,
    performance_rows =
      sum(
        is.finite(
          suppressWarnings(
            as.numeric(
              roster$
                bie_performance_rating
            )
          )
        )
      ),
    performance_scope =
      "PHASE 3 PERFORMANCE-BACKED"
  )
}


# ------------------------------------------------------------
# Build complete dashboard data package
# ------------------------------------------------------------

get_phase3_dashboard_package <- function(
    team_name,
    season = "2025-26") {
  
  phase3_step12_require_packages()
  
  decision_package <-
    p3s12_front_office_decision_package(
      team_name =
        team_name,
      season =
        season
    )
  
  recommendation_package <-
    run_phase3_recommendation_package(
      team_name =
        team_name,
      season =
        season
    )
  
  lineup_package <-
    run_phase3_lineup_rotation_optimization(
      team_name =
        team_name,
      season =
        season
    )
  
  player_impact <-
    get_phase3_player_impact(
      season =
        season,
      team_name =
        team_name
    )
  
  list(
    status = if (
      identical(
        decision_package$status,
        "OK"
      ) &&
      identical(
        recommendation_package$status,
        "OK"
      ) &&
      identical(
        lineup_package$status,
        "OK"
      )
    ) {
      "OK"
    } else {
      "REVIEW"
    },
    team =
      team_name,
    season =
      season,
    decision =
      decision_package,
    recommendations =
      recommendation_package,
    lineups =
      lineup_package,
    player_impact =
      player_impact
  )
}



# ------------------------------------------------------------
# Phase 3.1 Player Spot Check / Audit helpers
# ------------------------------------------------------------

p3s12_audit_value <- function(
    row,
    field,
    formatter = c("number", "percent", "text"),
    digits = 1,
    default = "—") {
  
  formatter <- match.arg(formatter)
  
  if (
    is.null(row) ||
    !is.data.frame(row) ||
    !nrow(row) ||
    !field %in% names(row)
  ) {
    return(default)
  }
  
  value <- row[[field]][[1]]
  
  if (
    is.null(value) ||
    length(value) == 0 ||
    is.na(value)
  ) {
    return(default)
  }
  
  if (formatter == "text") {
    
    value <- as.character(value)
    
    if (!nzchar(trimws(value))) {
      return(default)
    }
    
    return(trimws(value))
  }
  
  num <- suppressWarnings(
    as.numeric(value)
  )
  
  if (!is.finite(num)) {
    return(default)
  }
  
  if (formatter == "percent") {
    
    if (abs(num) <= 1.5) {
      num <- num * 100
    }
    
    return(
      paste0(
        round(num, digits),
        "%"
      )
    )
  }
  
  as.character(
    round(
      num,
      digits
    )
  )
}


p3s12_audit_metric_table <- function(
    row,
    metrics) {
  
  if (
    is.null(row) ||
    !is.data.frame(row) ||
    !nrow(row)
  ) {
    return(data.frame())
  }
  
  output <- lapply(
    metrics,
    function(metric) {
      
      data.frame(
        Metric =
          metric$label,
        
        Value =
          p3s12_audit_value(
            row =
              row,
            field =
              metric$field,
            formatter =
              metric$formatter %||%
              "number",
            digits =
              metric$digits %||%
              1
          ),
        
        stringsAsFactors =
          FALSE
      )
    }
  )
  
  do.call(
    rbind,
    output
  )
}


p3s12_audit_player_choices <- function(
    roster) {
  
  if (
    is.null(roster) ||
    !is.data.frame(roster) ||
    !nrow(roster)
  ) {
    return(character())
  }
  
  labels <- paste0(
    roster$player_name,
    " — ",
    ifelse(
      is.na(
        roster$primary_position
      ) |
        !nzchar(
          as.character(
            roster$primary_position
          )
        ),
      "Pos N/A",
      roster$primary_position
    )
  )
  
  stats::setNames(
    as.character(
      roster$player_id
    ),
    labels
  )
}



# ------------------------------------------------------------
# Phase 3.2 — League-wide team selector helpers
# ------------------------------------------------------------

get_tbi_team_choices <- function() {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name
    FROM teams
    ORDER BY team_name
    "
  )
  
  if (
    is.null(teams) ||
    !is.data.frame(teams) ||
    !nrow(teams)
  ) {
    return(
      character()
    )
  }
  
  stats::setNames(
    as.character(
      teams$team_name
    ),
    as.character(
      teams$team_name
    )
  )
}


# ------------------------------------------------------------
# Dashboard UI module
# ------------------------------------------------------------

mod_phase3_dashboard_ui <- function(
    id) {
  
  phase3_step12_require_packages()
  
  ns <- shiny::NS(
    id
  )
  
  shiny::div(
    class = "p3-dashboard-root",
    
    
    shiny::tags$style(
      shiny::HTML(
        "
        .p3-dashboard-section {
          margin-bottom: 1.25rem;
        }

        .p3-dashboard-title {
          font-weight: 700;
          margin-bottom: 0.75rem;
        }

        .p3-dashboard-note {
          color: #6c757d;
          font-size: 0.9rem;
        }

        /* --------------------------------------------------
           Natural-height cards.
           No vertical clipping at the card level.
           -------------------------------------------------- */

        .p3-section-card {
          height: auto !important;
          min-height: 0 !important;
          overflow: visible !important;
        }

        .p3-section-card > .card-body,
        .p3-section-card .card-body {
          height: auto !important;
          min-height: 0 !important;
          max-height: none !important;
          overflow: visible !important;
        }

        .p3-section-card .card-header {
          position: relative;
          z-index: 2;
        }

        /* --------------------------------------------------
           Tables scroll horizontally only.
           Vertical height expands naturally with rows.
           -------------------------------------------------- */

        .p3-table-shell {
          width: 100%;
          max-width: 100%;
          overflow-x: auto !important;
          overflow-y: visible !important;
          max-height: none !important;
          height: auto !important;
          scrollbar-gutter: stable;
        }

        .p3-table-shell table {
          width: max-content;
          min-width: 100%;
          margin-bottom: 0;
          white-space: nowrap;
        }

        .p3-table-shell th,
        .p3-table-shell td {
          vertical-align: middle;
          padding: 0.55rem 0.70rem;
        }

        /* Keep the biggest tables readable without forcing
           their parent cards to become scroll containers. */

        .p3-player-table table {
          min-width: 1250px;
        }

        .p3-recommendation-table table {
          min-width: 1150px;
        }

        .p3-lineup-table table {
          min-width: 1050px;
        }

        .p3-rotation-table table {
          min-width: 1200px;
        }

        /* --------------------------------------------------
           Lineup tabs need natural content height.
           -------------------------------------------------- */

        .p3-lineup-card,
        .p3-lineup-card .card-body,
        .p3-lineup-card .tab-content,
        .p3-lineup-card .tab-pane,
        .p3-lineup-card .bslib-card {
          height: auto !important;
          min-height: 0 !important;
          max-height: none !important;
          overflow: visible !important;
        }

        .p3-lineup-card .tab-pane {
          padding-top: 0.75rem;
        }

        /* --------------------------------------------------
           Rotation must finish before the next row starts.
           -------------------------------------------------- */

        .p3-rotation-card {
          display: block;
          clear: both;
          height: auto !important;
          overflow: visible !important;
        }

        .p3-rotation-card .card-body {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
          padding-bottom: 1rem;
        }

        .p3-bottom-grid {
          clear: both;
          position: relative;
          margin-top: 1.25rem;
          z-index: 1;
        }

        .p3-bottom-grid .card {
          height: 100%;
          overflow: visible !important;
        }

        .p3-header-subtitle {
          color: #6c757d;
          font-size: 0.9rem;
          margin-left: 0.35rem;
        }

        /* Prevent nested bslib fill behavior from squeezing
           table cards into tiny internal viewports. */

        .p3-dashboard-root,
        .p3-dashboard-root .html-fill-container,
        .p3-dashboard-root .html-fill-item {
          min-height: 0 !important;
        }

        .p3-dashboard-root .card {
          flex: none !important;
        }
        "
      )
    ),
    
    # --------------------------------------------------------
    # Header
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3",
      bslib::card_body(
        shiny::div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          shiny::div(
            shiny::h3(
              "Basketball Intelligence"
            ),
            shiny::div(
              class = "text-muted",
              shiny::textOutput(
                ns(
                  "team_context"
                ),
                inline = TRUE
              )
            )
          ),
          shiny::uiOutput(
            ns(
              "status_badge"
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # League-wide controls
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3",
      
      bslib::card_body(
        
        shiny::div(
          class = "p3-dashboard-title",
          "League View"
        ),
        
        bslib::layout_columns(
          col_widths = c(
            4,
            4,
            4
          ),
          
          shiny::selectInput(
            ns(
              "team_selector"
            ),
            "NBA Team",
            choices =
              character()
          ),
          
          shiny::selectInput(
            ns(
              "roster_season_selector"
            ),
            "Current Roster Season",
            choices = c(
              "2026-27"
            ),
            selected =
              "2026-27"
          ),
          
          shiny::selectInput(
            ns(
              "performance_season_selector"
            ),
            "Performance Season",
            choices = c(
              "2025-26"
            ),
            selected =
              "2025-26"
          )
        ),
        
        shiny::div(
          class = "p3-dashboard-note",
          "Select any NBA team. TBI evaluates the current roster using the selected prior-season performance evidence."
        )
      )
    ),
    
    # --------------------------------------------------------
    # Top summary cards
    # --------------------------------------------------------
    
    shiny::div(
      class = "p3-dashboard-section",
      bslib::layout_columns(
        col_widths = c(
          3,
          3,
          3,
          3
        ),
        
        shiny::uiOutput(
          ns(
            "avg_rating_card"
          )
        ),
        
        shiny::uiOutput(
          ns(
            "primary_need_card"
          )
        ),
        
        shiny::uiOutput(
          ns(
            "recommendation_card"
          )
        ),
        
        shiny::uiOutput(
          ns(
            "rotation_card"
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Player intelligence
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3 p3-section-card",
      
      bslib::card_header(
        shiny::div(
          class = "d-flex align-items-center flex-wrap gap-1",
          shiny::tags$strong(
            "Player Intelligence"
          ),
          shiny::span(
            class = "p3-header-subtitle",
            "Performance-backed Phase 3 ratings"
          )
        )
      ),
      
      bslib::card_body(
        shiny::div(
          class = "p3-table-shell p3-player-table",
          shiny::tableOutput(
            ns(
              "player_intelligence_table"
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Player Spot Check / Audit
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3 p3-section-card",
      
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          shiny::tags$strong(
            "Player Spot Check / Audit"
          ),
          shiny::span(
            class = "text-muted",
            "Trace the evidence behind the BIE rating"
          )
        )
      ),
      
      bslib::card_body(
        
        bslib::layout_columns(
          col_widths = c(
            4,
            8
          ),
          
          shiny::selectInput(
            ns(
              "audit_player_id"
            ),
            "Select Player",
            choices =
              character()
          ),
          
          shiny::uiOutput(
            ns(
              "audit_context_ui"
            )
          )
        ),
        
        shiny::uiOutput(
          ns(
            "audit_summary_cards"
          )
        ),
        
        shiny::div(
          class = "mt-3",
          
          bslib::navset_card_tab(
            
            bslib::nav_panel(
              "Raw Stats",
              shiny::tableOutput(
                ns(
                  "audit_raw_table"
                )
              )
            ),
            
            bslib::nav_panel(
              "Advanced",
              shiny::tableOutput(
                ns(
                  "audit_advanced_table"
                )
              )
            ),
            
            bslib::nav_panel(
              "Shooting / Spacing",
              shiny::tableOutput(
                ns(
                  "audit_shooting_table"
                )
              )
            ),
            
            bslib::nav_panel(
              "Creation",
              shiny::tableOutput(
                ns(
                  "audit_creation_table"
                )
              )
            ),
            
            bslib::nav_panel(
              "Defense / Rebounding",
              shiny::tableOutput(
                ns(
                  "audit_defense_table"
                )
              )
            ),
            
            bslib::nav_panel(
              "Role / BIE",
              shiny::tableOutput(
                ns(
                  "audit_bie_table"
                )
              )
            )
          )
        ),
        
        bslib::card(
          class = "mt-3",
          
          bslib::card_header(
            shiny::tags$strong(
              "Current-Team Recommendation"
            )
          ),
          
          bslib::card_body(
            shiny::uiOutput(
              ns(
                "audit_recommendation_ui"
              )
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Front-office recommendations
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3 p3-section-card",
      
      bslib::card_header(
        shiny::tags$strong(
          "Front Office Recommendations"
        )
      ),
      
      bslib::card_body(
        shiny::div(
          class = "p3-table-shell p3-recommendation-table",
          shiny::tableOutput(
            ns(
              "recommendation_table"
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Lineup tabs
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3 p3-section-card p3-lineup-card",
      
      bslib::card_header(
        shiny::tags$strong(
          "Lineup Optimization"
        )
      ),
      
      bslib::card_body(
        bslib::navset_card_tab(
          
          bslib::nav_panel(
            "Starting Five",
            shiny::div(
              class = "p3-table-shell p3-lineup-table",
              shiny::tableOutput(
                ns(
                  "starting_five_table"
                )
              )
            )
          ),
          
          bslib::nav_panel(
            "Closing Five",
            shiny::div(
              class = "p3-table-shell p3-lineup-table",
              shiny::tableOutput(
                ns(
                  "closing_five_table"
                )
              )
            )
          ),
          
          bslib::nav_panel(
            "Offense",
            shiny::div(
              class = "p3-table-shell p3-lineup-table",
              shiny::tableOutput(
                ns(
                  "offensive_five_table"
                )
              )
            )
          ),
          
          bslib::nav_panel(
            "Defense",
            shiny::div(
              class = "p3-table-shell p3-lineup-table",
              shiny::tableOutput(
                ns(
                  "defensive_five_table"
                )
              )
            )
          ),
          
          bslib::nav_panel(
            "Balanced",
            shiny::div(
              class = "p3-table-shell p3-lineup-table",
              shiny::tableOutput(
                ns(
                  "balanced_five_table"
                )
              )
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Rotation
    # --------------------------------------------------------
    
    bslib::card(
      class = "mb-3 p3-section-card p3-rotation-card",
      
      bslib::card_header(
        shiny::tags$strong(
          "Recommended Rotation"
        )
      ),
      
      bslib::card_body(
        shiny::div(
          class = "p3-table-shell p3-rotation-table",
          shiny::tableOutput(
            ns(
              "rotation_table"
            )
          )
        ),
        shiny::div(
          class = "p3-dashboard-note mt-2",
          "Regulation allocation target: 240 total player-minutes."
        )
      )
    ),
    
    # --------------------------------------------------------
    # Executive intelligence
    # --------------------------------------------------------
    
    shiny::div(
      class = "p3-bottom-grid",
      
      bslib::layout_columns(
        col_widths = c(
          6,
          6
        ),
        
        bslib::card(
          class = "mb-3 p3-section-card",
          
          bslib::card_header(
            shiny::tags$strong(
              "Roster Needs"
            )
          ),
          
          bslib::card_body(
            shiny::uiOutput(
              ns(
                "roster_needs_ui"
              )
            )
          )
        ),
        
        bslib::card(
          class = "mb-3 p3-section-card",
          
          bslib::card_header(
            shiny::tags$strong(
              "Executive Intelligence"
            )
          ),
          
          bslib::card_body(
            shiny::uiOutput(
              ns(
                "executive_ui"
              )
            )
          )
        )
      )
    )
  )
}


# ------------------------------------------------------------
# Dashboard server module
# ------------------------------------------------------------

mod_phase3_dashboard_server <- function(
    id,
    team_name = shiny::reactive(
      "Philadelphia 76ers"
    ),
    season = shiny::reactive(
      "2025-26"
    ),
    current_roster_season = shiny::reactive(
      "2026-27"
    )) {
  
  phase3_step12_require_packages()
  
  shiny::moduleServer(
    id,
    function(
    input,
    output,
    session) {
      
      team_rx <- if (
        shiny::is.reactive(
          team_name
        )
      ) {
        team_name
      } else {
        shiny::reactive(
          team_name
        )
      }
      
      season_rx <- if (
        shiny::is.reactive(
          season
        )
      ) {
        season
      } else {
        shiny::reactive(
          season
        )
      }
      
      roster_season_rx <- if (
        shiny::is.reactive(
          current_roster_season
        )
      ) {
        current_roster_season
      } else {
        shiny::reactive(
          current_roster_season
        )
      }
      
      # ------------------------------------------------------
      # League-wide selector state
      # ------------------------------------------------------
      
      team_choices <- shiny::reactive({
        
        tryCatch(
          get_tbi_team_choices(),
          error = function(e) {
            character()
          }
        )
      })
      
      shiny::observe({
        
        choices <-
          team_choices()
        
        shiny::req(
          length(choices)
        )
        
        requested_team <-
          tryCatch(
            as.character(
              team_rx()
            ),
            error = function(e) {
              ""
            }
          )
        
        selected_value <- if (
          length(requested_team) &&
          requested_team[[1]] %in%
          unname(choices)
        ) {
          requested_team[[1]]
        } else if (
          "Philadelphia 76ers" %in%
          unname(choices)
        ) {
          "Philadelphia 76ers"
        } else {
          unname(
            choices[[1]]
          )
        }
        
        shiny::updateSelectInput(
          session,
          "team_selector",
          choices =
            choices,
          selected =
            selected_value
        )
      })
      
      shiny::observe({
        
        requested <-
          tryCatch(
            as.character(
              roster_season_rx()
            )[[1]],
            error = function(e) {
              "2026-27"
            }
          )
        
        if (
          is.na(requested) ||
          !nzchar(requested)
        ) {
          requested <-
            "2026-27"
        }
        
        shiny::updateSelectInput(
          session,
          "roster_season_selector",
          choices =
            unique(
              c(
                requested,
                "2026-27"
              )
            ),
          selected =
            requested
        )
      })
      
      shiny::observe({
        
        requested <-
          tryCatch(
            as.character(
              season_rx()
            )[[1]],
            error = function(e) {
              "2025-26"
            }
          )
        
        if (
          is.na(requested) ||
          !nzchar(requested)
        ) {
          requested <-
            "2025-26"
        }
        
        shiny::updateSelectInput(
          session,
          "performance_season_selector",
          choices =
            unique(
              c(
                requested,
                "2025-26"
              )
            ),
          selected =
            requested
        )
      })
      
      selected_team <- shiny::reactive({
        
        shiny::req(
          input$team_selector
        )
        
        as.character(
          input$team_selector
        )
      })
      
      selected_roster_season <- shiny::reactive({
        
        shiny::req(
          input$roster_season_selector
        )
        
        as.character(
          input$roster_season_selector
        )
      })
      
      selected_performance_season <- shiny::reactive({
        
        shiny::req(
          input$performance_season_selector
        )
        
        as.character(
          input$performance_season_selector
        )
      })
      
      dashboard_data <- shiny::reactive({
        
        shiny::req(
          selected_team(),
          selected_roster_season(),
          selected_performance_season()
        )
        
        get_phase3_dashboard_package(
          team_name =
            selected_team(),
          season =
            selected_performance_season(),
          current_roster_season =
            selected_roster_season()
        )
      })
      
      # ------------------------------------------------------
      # Player Spot Check / Audit state
      # ------------------------------------------------------
      
      shiny::observe({
        
        roster <-
          dashboard_data()$
          current_roster
        
        choices <-
          p3s12_audit_player_choices(
            roster
          )
        
        current_selected <-
          isolate(
            input$audit_player_id
          )
        
        selected <- if (
          length(choices) &&
          !is.null(current_selected) &&
          current_selected %in%
          unname(choices)
        ) {
          current_selected
        } else if (
          length(choices)
        ) {
          unname(
            choices[[1]]
          )
        } else {
          character()
        }
        
        shiny::updateSelectInput(
          session,
          "audit_player_id",
          choices =
            choices,
          selected =
            selected
        )
      })
      
      audit_player <- shiny::reactive({
        
        shiny::req(
          input$audit_player_id
        )
        
        roster <-
          dashboard_data()$
          current_roster
        
        shiny::req(
          is.data.frame(roster),
          nrow(roster)
        )
        
        hit <- roster[
          as.character(
            roster$player_id
          ) ==
            as.character(
              input$audit_player_id
            ),
          ,
          drop = FALSE
        ]
        
        shiny::req(
          nrow(hit)
        )
        
        hit[
          1,
          ,
          drop = FALSE
        ]
      })
      
      audit_recommendation <- shiny::reactive({
        
        shiny::req(
          input$audit_player_id
        )
        
        recs <-
          dashboard_data()$
          recommendations$
          player_recommendations
        
        if (
          is.null(recs) ||
          !is.data.frame(recs) ||
          !nrow(recs) ||
          !"player_id" %in% names(recs)
        ) {
          return(
            data.frame()
          )
        }
        
        recs[
          as.character(
            recs$player_id
          ) ==
            as.character(
              input$audit_player_id
            ),
          ,
          drop = FALSE
        ]
      })
      
      output$audit_context_ui <-
        shiny::renderUI({
          
          row <- audit_player()
          
          shiny::tagList(
            shiny::h4(
              class = "mb-1",
              p3s12_audit_value(
                row,
                "player_name",
                "text"
              )
            ),
            
            shiny::div(
              class = "text-muted",
              paste0(
                "Current Team: ",
                p3s12_audit_value(
                  row,
                  "current_team_name",
                  "text"
                ),
                " • Evidence: ",
                p3s12_audit_value(
                  row,
                  "evaluation_label",
                  "text"
                ),
                " • ",
                p3s12_audit_value(
                  row,
                  "evaluation_source",
                  "text"
                )
              )
            )
          )
        })
      
      output$audit_summary_cards <-
        shiny::renderUI({
          
          row <- audit_player()
          rec <- audit_recommendation()
          
          action <- if (
            is.data.frame(rec) &&
            nrow(rec) &&
            "recommendation" %in% names(rec)
          ) {
            as.character(
              rec$recommendation[[1]]
            )
          } else {
            "REVIEW"
          }
          
          bslib::layout_columns(
            col_widths = c(
              3,
              3,
              3,
              3
            ),
            
            p3s12_metric_card(
              "BIE Rating",
              p3s12_audit_value(
                row,
                "bie_performance_rating",
                "number"
              ),
              p3s12_audit_value(
                row,
                "impact_tier",
                "text"
              )
            ),
            
            p3s12_metric_card(
              "BIE Percentile",
              p3s12_audit_value(
                row,
                "bie_performance_percentile",
                "number"
              ),
              p3s12_audit_value(
                row,
                "impact_confidence",
                "text"
              )
            ),
            
            p3s12_metric_card(
              "Primary Role",
              p3s12_audit_value(
                row,
                "primary_role",
                "text"
              ),
              p3s12_audit_value(
                row,
                "archetype",
                "text"
              )
            ),
            
            p3s12_metric_card(
              "Recommendation",
              action,
              "Current-team context"
            )
          )
        })
      
      output$audit_raw_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "Games", field = "games_played", digits = 0),
              list(label = "Minutes", field = "minutes", digits = 0),
              list(label = "MPG", field = "minutes_per_game"),
              list(label = "PPG", field = "points_per_game"),
              list(label = "RPG", field = "rebounds_per_game"),
              list(label = "APG", field = "assists_per_game"),
              list(label = "SPG", field = "steals_per_game"),
              list(label = "BPG", field = "blocks_per_game"),
              list(label = "TOV/G", field = "turnovers_per_game"),
              list(label = "FG%", field = "field_goal_pct", formatter = "percent"),
              list(label = "3P%", field = "three_point_pct", formatter = "percent"),
              list(label = "FT%", field = "free_throw_pct", formatter = "percent")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_advanced_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "TS%", field = "true_shooting_pct", formatter = "percent"),
              list(label = "eFG%", field = "effective_field_goal_pct", formatter = "percent"),
              list(label = "BPM", field = "box_plus_minus"),
              list(label = "OBPM", field = "offensive_box_plus_minus"),
              list(label = "DBPM", field = "defensive_box_plus_minus"),
              list(label = "VORP", field = "value_over_replacement"),
              list(label = "WS/48", field = "win_shares_per_48", digits = 3),
              list(label = "Net Rating", field = "net_rating")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_shooting_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "Shooting Efficiency", field = "shooting_efficiency_score"),
              list(label = "Spacing Score", field = "spacing_score"),
              list(label = "Shooting Gravity", field = "shooting_gravity_score"),
              list(label = "Rim Pressure Proxy", field = "rim_pressure_proxy_score"),
              list(label = "Stabilized 3P%", field = "stabilized_three_point_pct", formatter = "percent"),
              list(label = "Spacing Tier", field = "spacing_tier", formatter = "text"),
              list(label = "Shooting Confidence", field = "shooting_confidence", formatter = "text")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_creation_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "Creation Score", field = "creation_score"),
              list(label = "Passing Control", field = "passing_control_score"),
              list(label = "Secondary Creation", field = "secondary_creation_score"),
              list(label = "Ball Security", field = "ball_security_score"),
              list(label = "Creation Role", field = "creation_role", formatter = "text"),
              list(label = "Playmaking Confidence", field = "playmaking_confidence", formatter = "text")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_defense_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "Defense Proxy", field = "defense_proxy_score"),
              list(label = "Rebounding Score", field = "rebounding_score"),
              list(label = "Interior Impact", field = "interior_impact_score"),
              list(label = "Disruption", field = "disruption_score"),
              list(label = "Defensive Role", field = "defensive_role", formatter = "text"),
              list(label = "Rebounding Role", field = "rebounding_role", formatter = "text"),
              list(label = "Defense Confidence", field = "defense_confidence", formatter = "text")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_bie_table <-
        shiny::renderTable({
          
          p3s12_audit_metric_table(
            audit_player(),
            list(
              list(label = "Primary Role", field = "primary_role", formatter = "text"),
              list(label = "Secondary Role", field = "secondary_role", formatter = "text"),
              list(label = "Tertiary Role", field = "tertiary_role", formatter = "text"),
              list(label = "Archetype", field = "archetype", formatter = "text"),
              list(label = "Role Family", field = "role_family", formatter = "text"),
              list(label = "Shooting Component", field = "shooting_component"),
              list(label = "Creation Component", field = "creation_component"),
              list(label = "Defense Component", field = "defense_component"),
              list(label = "Rebounding Component", field = "rebounding_component"),
              list(label = "Role Component", field = "role_component"),
              list(label = "Availability Component", field = "availability_component"),
              list(label = "Advanced Impact Component", field = "advanced_impact_component"),
              list(label = "Offensive Impact", field = "offensive_impact_score"),
              list(label = "Defensive Impact", field = "defensive_impact_score"),
              list(label = "All-Around Impact", field = "all_around_impact_score"),
              list(label = "BIE Rating", field = "bie_performance_rating"),
              list(label = "BIE Percentile", field = "bie_performance_percentile"),
              list(label = "Impact Tier", field = "impact_tier", formatter = "text"),
              list(label = "Impact Confidence", field = "impact_confidence", formatter = "text")
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE)
      
      output$audit_recommendation_ui <-
        shiny::renderUI({
          
          rec <- audit_recommendation()
          
          if (
            is.null(rec) ||
            !is.data.frame(rec) ||
            !nrow(rec)
          ) {
            return(
              shiny::div(
                class = "text-muted",
                "No recommendation row is currently available for this player."
              )
            )
          }
          
          action <- if ("recommendation" %in% names(rec)) {
            as.character(rec$recommendation[[1]])
          } else {
            "REVIEW"
          }
          
          confidence <- if (
            "recommendation_confidence" %in% names(rec)
          ) {
            as.character(
              rec$recommendation_confidence[[1]]
            )
          } else {
            "LIMITED"
          }
          
          importance <- if (
            "team_importance_score" %in% names(rec)
          ) {
            p3s12_num(
              rec$team_importance_score[[1]]
            )
          } else {
            NA_real_
          }
          
          rationale <- ""
          
          for (
            candidate in c(
              "rationale",
              "recommendation_rationale",
              "explanation"
            )
          ) {
            if (
              candidate %in% names(rec) &&
              !is.na(rec[[candidate]][[1]]) &&
              nzchar(
                trimws(
                  as.character(
                    rec[[candidate]][[1]]
                  )
                )
              )
            ) {
              rationale <-
                as.character(
                  rec[[candidate]][[1]]
                )
              
              break
            }
          }
          
          shiny::tagList(
            
            shiny::p(
              shiny::tags$strong(
                "Action: "
              ),
              action
            ),
            
            shiny::p(
              shiny::tags$strong(
                "Confidence: "
              ),
              confidence
            ),
            
            if (!is.na(importance)) {
              shiny::p(
                shiny::tags$strong(
                  "Team importance: "
                ),
                round(
                  importance,
                  1
                )
              )
            },
            
            if (nzchar(rationale)) {
              shiny::div(
                class = "mt-2 text-muted",
                rationale
              )
            }
          )
        })
      
      # ------------------------------------------------------
      # Header
      # ------------------------------------------------------
      
      output$team_context <-
        shiny::renderText({
          
          paste0(
            dashboard_data()$team,
            " • Roster ",
            dashboard_data()$current_roster_season,
            " • Stats ",
            dashboard_data()$performance_season
          )
        })
      
      output$status_badge <-
        shiny::renderUI({
          
          status <-
            dashboard_data()$status
          
          shiny::span(
            class = if (
              identical(
                status,
                "OK"
              )
            ) {
              "badge rounded-pill text-bg-success"
            } else {
              "badge rounded-pill text-bg-warning"
            },
            if (
              identical(
                status,
                "OK"
              )
            ) {
              "PHASE 3 LIVE"
            } else {
              "REVIEW"
            }
          )
        })
      
      # ------------------------------------------------------
      # Summary cards
      # ------------------------------------------------------
      
      output$avg_rating_card <-
        shiny::renderUI({
          
          impact <-
            dashboard_data()$
            player_impact
          
          avg <- mean(
            suppressWarnings(
              as.numeric(
                impact$
                  bie_performance_rating
              )
            ),
            na.rm = TRUE
          )
          
          if (!is.finite(avg)) {
            avg <- NA_real_
          }
          
          p3s12_metric_card(
            "Avg BIE Rating",
            if (
              is.na(avg)
            ) {
              "—"
            } else {
              round(
                avg,
                1
              )
            },
            "Team performance layer"
          )
        })
      
      output$primary_need_card <-
        shiny::renderUI({
          
          need <-
            dashboard_data()$
            decision$
            roster_needs$
            primary_need %||%
            "Unavailable"
          
          p3s12_metric_card(
            "Primary Need",
            p3s12_text(
              need,
              "Unavailable"
            ),
            "Roster construction"
          )
        })
      
      output$recommendation_card <-
        shiny::renderUI({
          
          counts <-
            dashboard_data()$
            recommendations$
            recommendation_counts
          
          keep_count <- if (
            !is.null(counts) &&
            is.data.frame(counts) &&
            nrow(counts) &&
            "KEEP" %in%
            counts$recommendation
          ) {
            counts$players[
              counts$recommendation ==
                "KEEP"
            ][[1]]
          } else {
            0
          }
          
          p3s12_metric_card(
            "KEEP",
            keep_count,
            "Current recommendation count"
          )
        })
      
      output$rotation_card <-
        shiny::renderUI({
          
          rotation <-
            dashboard_data()$
            lineups$
            rotation
          
          minutes <- if (
            is.data.frame(
              rotation
            ) &&
            nrow(
              rotation
            )
          ) {
            sum(
              suppressWarnings(
                as.numeric(
                  rotation$
                    recommended_minutes
                )
              ),
              na.rm = TRUE
            )
          } else {
            NA_real_
          }
          
          p3s12_metric_card(
            "Rotation",
            if (
              is.na(minutes)
            ) {
              "—"
            } else {
              paste0(
                round(
                  minutes,
                  1
                ),
                " min"
              )
            },
            "Target: 240"
          )
        })
      
      # ------------------------------------------------------
      # Player table
      # ------------------------------------------------------
      
      output$player_intelligence_table <-
        shiny::renderTable({
          
          impact <-
            dashboard_data()$
            player_impact
          
          if (
            is.null(impact) ||
            !is.data.frame(impact) ||
            !nrow(impact)
          ) {
            return(
              data.frame()
            )
          }
          
          keep <- intersect(
            c(
              "player_name",
              "primary_position",
              "evaluation_label",
              "evaluation_source",
              "bie_performance_rating",
              "bie_performance_percentile",
              "impact_tier",
              "impact_confidence",
              "offensive_impact_score",
              "defensive_impact_score",
              "primary_role",
              "archetype"
            ),
            names(impact)
          )
          
          d <- impact[
            ,
            keep,
            drop = FALSE
          ]
          
          d <- p3s12_player_table(
            d
          )
          
          p3s12_friendly_names(
            d
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE,
        spacing = "s")
      
      # ------------------------------------------------------
      # Recommendation table
      # ------------------------------------------------------
      
      output$recommendation_table <-
        shiny::renderTable({
          
          recs <-
            dashboard_data()$
            recommendations$
            player_recommendations
          
          if (
            is.null(recs) ||
            !is.data.frame(recs) ||
            !nrow(recs)
          ) {
            return(
              data.frame()
            )
          }
          
          keep <- intersect(
            c(
              "player_name",
              "primary_position",
              "recommendation",
              "recommendation_confidence",
              "team_importance_score",
              "bie_performance_rating",
              "impact_tier",
              "primary_role",
              "archetype",
              "roster_need_match"
            ),
            names(recs)
          )
          
          d <- recs[
            ,
            keep,
            drop = FALSE
          ]
          
          p3s12_friendly_names(
            p3s12_player_table(
              d
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE,
        spacing = "s")
      
      # ------------------------------------------------------
      # Lineup render helper
      # ------------------------------------------------------
      
      render_lineup <- function(
    type) {
        
        shiny::renderTable({
          
          lineup <-
            phase3_step9_lineup_table(
              dashboard_data()$
                lineups,
              type =
                type
            )
          
          p3s12_friendly_names(
            p3s12_player_table(
              lineup
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE,
        spacing = "s")
      }
      
      output$starting_five_table <-
        render_lineup(
          "starting"
        )
      
      output$closing_five_table <-
        render_lineup(
          "closing"
        )
      
      output$offensive_five_table <-
        render_lineup(
          "offense"
        )
      
      output$defensive_five_table <-
        render_lineup(
          "defense"
        )
      
      output$balanced_five_table <-
        render_lineup(
          "balanced"
        )
      
      # ------------------------------------------------------
      # Rotation
      # ------------------------------------------------------
      
      output$rotation_table <-
        shiny::renderTable({
          
          rotation <-
            dashboard_data()$
            lineups$
            rotation
          
          p3s12_friendly_names(
            p3s12_player_table(
              rotation
            )
          )
        },
        striped = TRUE,
        bordered = FALSE,
        hover = TRUE,
        spacing = "s")
      
      # ------------------------------------------------------
      # Roster needs
      # ------------------------------------------------------
      
      output$roster_needs_ui <-
        shiny::renderUI({
          
          needs <-
            dashboard_data()$
            decision$
            roster_needs
          
          primary <-
            p3s12_text(
              needs$
                primary_need %||%
                "",
              "Unavailable"
            )
          
          secondary <-
            p3s12_text(
              needs$
                secondary_need %||%
                "",
              "Unavailable"
            )
          
          explanation <-
            p3s12_text(
              needs$
                explanation %||%
                "",
              ""
            )
          
          shiny::tagList(
            shiny::p(
              shiny::tags$strong(
                "Primary: "
              ),
              primary
            ),
            shiny::p(
              shiny::tags$strong(
                "Secondary: "
              ),
              secondary
            ),
            if (
              nzchar(
                explanation
              )
            ) {
              shiny::p(
                class = "text-muted",
                explanation
              )
            }
          )
        })
      
      # ------------------------------------------------------
      # Executive intelligence
      # ------------------------------------------------------
      
      output$executive_ui <-
        shiny::renderUI({
          
          executive <-
            dashboard_data()$
            decision$
            executive
          
          recommendation <-
            p3s12_text(
              executive$
                recommendation %||%
                "",
              "REVIEW"
            )
          
          explanation <-
            p3s12_text(
              executive$
                explanation %||%
                "",
              ""
            )
          
          score <-
            p3s12_num(
              executive$
                score %||%
                NA_real_
            )
          
          shiny::tagList(
            
            shiny::p(
              shiny::tags$strong(
                "Recommendation: "
              ),
              recommendation
            ),
            
            if (
              !is.na(
                score
              )
            ) {
              shiny::p(
                shiny::tags$strong(
                  "Executive score: "
                ),
                round(
                  score,
                  1
                )
              )
            },
            
            if (
              nzchar(
                explanation
              )
            ) {
              shiny::p(
                class = "text-muted",
                explanation
              )
            }
          )
        })
      
      invisible(
        dashboard_data
      )
    }
  )
}


# ------------------------------------------------------------
# Standalone demo app
#
# Optional:
#   run_phase3_dashboard_demo("Boston Celtics")
# ------------------------------------------------------------

run_phase3_dashboard_demo <- function(
    team_name = "Philadelphia 76ers",
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  phase3_step12_require_packages()
  
  ui <- bslib::page_fillable(
    title =
      "Phase 3 Basketball Intelligence",
    
    bslib::card(
      class = "m-3",
      mod_phase3_dashboard_ui(
        "phase3_dashboard"
      )
    )
  )
  
  server <- function(
    input,
    output,
    session) {
    
    mod_phase3_dashboard_server(
      "phase3_dashboard",
      team_name =
        shiny::reactive(
          team_name
        ),
      season =
        shiny::reactive(
          season
        ),
      current_roster_season =
        shiny::reactive(
          current_roster_season
        )
    )
  }
  
  shiny::shinyApp(
    ui,
    server
  )
}


# ------------------------------------------------------------
# Step 12 health check
# ------------------------------------------------------------

phase3_step12_healthcheck <- function(
    team_name,
    season = "2025-26") {
  
  phase3_step12_require_packages()
  
  required_functions <- c(
    "p3s12_front_office_decision_package",
    "run_phase3_recommendation_package",
    "run_phase3_lineup_rotation_optimization",
    "get_phase3_player_impact",
    "phase3_step9_lineup_table",
    "mod_phase3_dashboard_ui",
    "mod_phase3_dashboard_server"
  )
  
  function_status <- setNames(
    vapply(
      required_functions,
      exists,
      logical(1),
      mode = "function",
      inherits = TRUE
    ),
    required_functions
  )
  
  if (
    !all(
      function_status
    )
  ) {
    
    return(
      list(
        phase =
          "Phase 3",
        step =
          "Step 12.3 — Natural-Flow Dashboard Layout",
        status =
          "REVIEW",
        missing_functions =
          names(
            function_status[
              !function_status
            ]
          )
      )
    )
  }
  
  package <- tryCatch(
    get_phase3_dashboard_package(
      team_name =
        team_name,
      season =
        season
    ),
    error = function(e) {
      list(
        status =
          "ERROR",
        explanation =
          conditionMessage(e)
      )
    }
  )
  
  if (
    !identical(
      package$status,
      "OK"
    )
  ) {
    
    return(
      list(
        phase =
          "Phase 3",
        step =
          "Step 12.3 — Natural-Flow Dashboard Layout",
        status =
          "REVIEW",
        issue =
          package$status,
        explanation =
          package$explanation %||%
          ""
      )
    )
  }
  
  impact_rows <- if (
    is.data.frame(
      package$
      player_impact
    )
  ) {
    nrow(
      package$
        player_impact
    )
  } else {
    0L
  }
  
  recommendation_rows <- if (
    is.data.frame(
      package$
      recommendations$
      player_recommendations
    )
  ) {
    nrow(
      package$
        recommendations$
        player_recommendations
    )
  } else {
    0L
  }
  
  rotation_players <- if (
    is.data.frame(
      package$
      lineups$
      rotation
    )
  ) {
    nrow(
      package$
        lineups$
        rotation
    )
  } else {
    0L
  }
  
  list(
    phase =
      "Phase 3",
    step =
      "Step 12.3 — Natural-Flow Dashboard Layout",
    status = if (
      impact_rows > 0 &&
      recommendation_rows > 0 &&
      rotation_players > 0
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    team =
      team_name,
    season =
      season,
    player_impact_rows =
      impact_rows,
    recommendation_rows =
      recommendation_rows,
    rotation_players =
      rotation_players,
    dashboard_module =
      "mod_phase3_dashboard",
    integration_scope =
      "PLAYER + ROSTER + LINEUP + EXECUTIVE",
    layout_mode =
      "NATURAL HEIGHT + HORIZONTAL TABLE SCROLL ONLY"
  )
}


# ============================================================
# PHASE 3.1 OVERRIDE — CURRENT ROSTER DASHBOARD CONTEXT
# ============================================================

get_phase3_dashboard_package <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  phase3_step12_require_packages()
  
  if (
    !exists(
      "get_tbi_current_roster_evaluation_context",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Current roster evaluation context is not loaded."
    )
  }
  
  current_roster <- get_tbi_current_roster_evaluation_context(
    team_name = team_name,
    current_roster_season = current_roster_season,
    performance_season = season
  )
  
  decision_package <- run_phase3_front_office_decision_package(
    team_name = team_name,
    season = season,
    current_roster_season = current_roster_season
  )
  
  recommendation_package <- run_phase3_recommendation_package(
    team_name = team_name,
    season = season,
    current_roster_season = current_roster_season
  )
  
  lineup_package <- run_phase3_lineup_rotation_optimization(
    team_name = team_name,
    season = season,
    current_roster_season = current_roster_season
  )
  
  list(
    status = if (
      nrow(current_roster) > 0 &&
      identical(decision_package$status, "OK") &&
      identical(recommendation_package$status, "OK") &&
      identical(lineup_package$status, "OK")
    ) {
      "OK"
    } else {
      "REVIEW"
    },
    team = team_name,
    current_roster_season =
      unique(current_roster$current_roster_season)[[1]],
    season = season,
    performance_season = season,
    decision = decision_package,
    recommendations = recommendation_package,
    lineups = lineup_package,
    player_impact = current_roster,
    current_roster = current_roster,
    veterans_with_prior_nba_performance =
      sum(current_roster$evaluation_source == "PRIOR-SEASON NBA"),
    rookies_or_pending =
      sum(current_roster$evaluation_source != "PRIOR-SEASON NBA"),
    moved_players =
      sum(current_roster$changed_teams_since_evidence, na.rm = TRUE),
    context_rule =
      "CURRENT ROSTER + PRIOR-SEASON PLAYER EVIDENCE"
  )
}


phase3_step12_healthcheck <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  package <- tryCatch(
    get_phase3_dashboard_package(
      team_name = team_name,
      season = season,
      current_roster_season = current_roster_season
    ),
    error = function(e) {
      list(
        status = "ERROR",
        explanation = conditionMessage(e)
      )
    }
  )
  
  if (!identical(package$status, "OK")) {
    return(
      list(
        phase = "Phase 3.1",
        step = "Dashboard — Current Roster Context",
        status = "REVIEW",
        issue = package$status,
        explanation = package$explanation %||% ""
      )
    )
  }
  
  list(
    phase = "Phase 3.1",
    step = "Dashboard — Current Roster Context",
    status = "READY",
    team = team_name,
    current_roster_season = package$current_roster_season,
    performance_season = package$performance_season,
    current_roster_players = nrow(package$current_roster),
    veterans_with_prior_nba_performance =
      package$veterans_with_prior_nba_performance,
    rookies_or_pending = package$rookies_or_pending,
    moved_players = package$moved_players,
    recommendation_rows =
      nrow(package$recommendations$player_recommendations),
    rotation_players =
      nrow(package$lineups$rotation),
    dashboard_module = "mod_phase3_dashboard",
    integration_scope =
      "CURRENT ROSTER + HISTORICAL PERFORMANCE + RECOMMENDATIONS + LINEUPS"
  )
}

# ============================================================
# PHASE 3.1 FINAL DASHBOARD HOTFIX
# Signature-safe downstream calls
#
# Permanent fix:
#   The dashboard detects which arguments each loaded function
#   accepts before calling it. This prevents older compatible
#   function signatures from crashing Shiny with:
#
#   unused argument (current_roster_season = ...)
# ============================================================


p3s12_call_safe <- function(
    fn_name,
    args = list()) {
  
  if (
    !exists(
      fn_name,
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      paste0(
        "Required dashboard function is not loaded: ",
        fn_name
      )
    )
  }
  
  fn <- get(
    fn_name,
    mode = "function",
    inherits = TRUE
  )
  
  fn_formals <- names(
    formals(fn)
  )
  
  if (is.null(fn_formals)) {
    fn_formals <- character()
  }
  
  # If the function accepts ..., keep every argument.
  if ("..." %in% fn_formals) {
    return(
      do.call(
        fn,
        args
      )
    )
  }
  
  safe_args <- args[
    names(args) %in%
      fn_formals
  ]
  
  do.call(
    fn,
    safe_args
  )
}


get_phase3_dashboard_package <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  phase3_step12_require_packages()
  
  if (
    !exists(
      "get_tbi_current_roster_evaluation_context",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Current roster evaluation context is not loaded."
    )
  }
  
  current_roster <-
    get_tbi_current_roster_evaluation_context(
      team_name =
        team_name,
      current_roster_season =
        current_roster_season,
      performance_season =
        season
    )
  
  if (
    is.null(current_roster) ||
    !is.data.frame(current_roster) ||
    !nrow(current_roster)
  ) {
    return(
      list(
        status =
          "NO CURRENT ROSTER DATA",
        team =
          team_name,
        current_roster_season =
          current_roster_season,
        performance_season =
          season
      )
    )
  }
  
  decision_package <-
    p3s12_call_safe(
      "run_phase3_front_office_decision_package",
      list(
        team_name =
          team_name,
        season =
          season,
        current_roster_season =
          current_roster_season
      )
    )
  
  recommendation_package <-
    p3s12_call_safe(
      "run_phase3_recommendation_package",
      list(
        team_name =
          team_name,
        season =
          season,
        current_roster_season =
          current_roster_season
      )
    )
  
  lineup_package <-
    p3s12_call_safe(
      "run_phase3_lineup_rotation_optimization",
      list(
        team_name =
          team_name,
        season =
          season,
        current_roster_season =
          current_roster_season
      )
    )
  
  list(
    status = if (
      nrow(current_roster) > 0 &&
      identical(
        decision_package$status,
        "OK"
      ) &&
      identical(
        recommendation_package$status,
        "OK"
      ) &&
      identical(
        lineup_package$status,
        "OK"
      )
    ) {
      "OK"
    } else {
      "REVIEW"
    },
    
    team =
      unique(
        current_roster$current_team_name
      )[[1]],
    
    current_roster_season =
      unique(
        current_roster$current_roster_season
      )[[1]],
    
    season =
      season,
    
    performance_season =
      season,
    
    decision =
      decision_package,
    
    recommendations =
      recommendation_package,
    
    lineups =
      lineup_package,
    
    player_impact =
      current_roster,
    
    current_roster =
      current_roster,
    
    veterans_with_prior_nba_performance =
      sum(
        current_roster$evaluation_source ==
          "PRIOR-SEASON NBA"
      ),
    
    rookies_or_pending =
      sum(
        current_roster$evaluation_source !=
          "PRIOR-SEASON NBA"
      ),
    
    moved_players =
      sum(
        current_roster$changed_teams_since_evidence,
        na.rm = TRUE
      ),
    
    context_rule =
      "CURRENT ROSTER + PRIOR-SEASON PLAYER EVIDENCE"
  )
}


phase3_step12_healthcheck <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  package <- tryCatch(
    get_phase3_dashboard_package(
      team_name =
        team_name,
      season =
        season,
      current_roster_season =
        current_roster_season
    ),
    error = function(e) {
      list(
        status =
          "ERROR",
        explanation =
          conditionMessage(e)
      )
    }
  )
  
  if (
    !identical(
      package$status,
      "OK"
    )
  ) {
    return(
      list(
        phase =
          "Phase 3.1",
        step =
          "Dashboard — Signature-Safe Current Roster Context",
        status =
          "REVIEW",
        issue =
          package$status,
        explanation =
          package$explanation %||%
          ""
      )
    )
  }
  
  recommendation_rows <- if (
    is.data.frame(
      package$
      recommendations$
      player_recommendations
    )
  ) {
    nrow(
      package$
        recommendations$
        player_recommendations
    )
  } else {
    0L
  }
  
  rotation_players <- if (
    is.data.frame(
      package$
      lineups$
      rotation
    )
  ) {
    nrow(
      package$
        lineups$
        rotation
    )
  } else {
    0L
  }
  
  list(
    phase =
      "Phase 3.1",
    step =
      "Dashboard — Signature-Safe Current Roster Context",
    status = if (
      nrow(
        package$current_roster
      ) > 0 &&
      recommendation_rows > 0 &&
      rotation_players > 0
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    
    team =
      package$team,
    
    current_roster_season =
      package$current_roster_season,
    
    performance_season =
      package$performance_season,
    
    current_roster_players =
      nrow(
        package$current_roster
      ),
    
    veterans_with_prior_nba_performance =
      package$veterans_with_prior_nba_performance,
    
    rookies_or_pending =
      package$rookies_or_pending,
    
    moved_players =
      package$moved_players,
    
    recommendation_rows =
      recommendation_rows,
    
    rotation_players =
      rotation_players,
    
    dashboard_module =
      "mod_phase3_dashboard",
    
    player_audit =
      "ACTIVE",
    
    integration_scope =
      "CURRENT ROSTER + HISTORICAL PERFORMANCE + PLAYER AUDIT + RECOMMENDATIONS + LINEUPS",
    
    signature_safe_calls =
      TRUE
  )
}

# ============================================================
# PHASE 3.2 — LEAGUE-WIDE DASHBOARD SELECTOR HEALTHCHECK
# ============================================================

phase3_league_selector_healthcheck <- function(
    current_roster_season = "2026-27",
    performance_season = "2025-26") {
  
  choices <- tryCatch(
    get_tbi_team_choices(),
    error = function(e) {
      character()
    }
  )
  
  league <- if (
    exists(
      "tbi_league_healthcheck",
      mode = "function",
      inherits = TRUE
    )
  ) {
    tryCatch(
      tbi_league_healthcheck(
        current_roster_season =
          current_roster_season,
        performance_season =
          performance_season
      ),
      error = function(e) {
        data.frame()
      }
    )
  } else {
    data.frame()
  }
  
  ready_count <- if (
    is.data.frame(league) &&
    nrow(league) &&
    "status" %in% names(league)
  ) {
    sum(
      league$status ==
        "READY",
      na.rm = TRUE
    )
  } else {
    NA_integer_
  }
  
  list(
    phase =
      "Phase 3.2",
    
    step =
      "League-Wide TBI Dashboard Selector",
    
    status = if (
      length(choices) == 30 &&
      (
        is.na(ready_count) ||
        ready_count == 30
      )
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    
    selector_teams =
      length(choices),
    
    database_ready_teams =
      ready_count,
    
    roster_season =
      current_roster_season,
    
    performance_season =
      performance_season,
    
    player_audit =
      "ACTIVE",
    
    league_selector =
      "ACTIVE",
    
    rule =
      "ONE TBI DASHBOARD — ALL 30 NBA TEAMS"
  )
}