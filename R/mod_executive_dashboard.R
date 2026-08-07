# ------------------------------------------------------------
# Module: Executive Dashboard
# Phase 7 Executive Experience Upgrade
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Executive Dashboard UI
#'
#' @param id Internal module ID.
#' @noRd
mod_executive_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  
  kpi_tile <- function(label, output_id, icon) {
    shiny::div(
      class = "terminal-kpi",
      shiny::div(
        class = "terminal-kpi-top",
        shiny::span(class = "terminal-kpi-label", label),
        shiny::span(class = "terminal-kpi-icon", bsicons::bs_icon(icon))
      ),
      shiny::div(
        class = "terminal-kpi-value",
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  metric_cell <- function(label, output_id) {
    shiny::div(
      class = "metric-cell",
      shiny::div(class = "metric-cell-label", label),
      shiny::div(
        class = "metric-cell-value",
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  shiny::div(
    class = "executive-dashboard terminal-dashboard tbi-exec-dashboard-v2",
    
    shiny::tags$style(shiny::HTML("\n      .tbi-exec-dashboard-v2 { display:grid; gap:20px; }\n      .tbi-exec-dashboard-v2 .executive-intelligence-shell { display:grid; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation { border:1px solid #26364a; border-left:5px solid #4f8cff; border-radius:14px; background:linear-gradient(135deg,#0d1828,#101f33); padding:22px; box-shadow:0 16px 34px rgba(0,0,0,.20); }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__header { display:flex; justify-content:space-between; align-items:flex-start; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__eyebrow,\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header__eyebrow { color:#8ca6c3; font-size:11px; font-weight:800; letter-spacing:.12em; text-transform:uppercase; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__title { margin:4px 0 0; color:#f4f8fc; font-size:28px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score { color:#f4f8fc; white-space:nowrap; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score-value { font-size:34px; font-weight:800; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score-scale { color:#8ca6c3; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__meta { display:flex; gap:10px; flex-wrap:wrap; margin-top:16px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__summary { color:#c4d2e1; margin:16px 0 0; line-height:1.55; }\n      .tbi-exec-dashboard-v2 .tbi-exec-badge { display:inline-flex; align-items:center; gap:6px; border-radius:999px; padding:6px 10px; font-size:11px; font-weight:800; letter-spacing:.04em; background:#17263a; color:#dbe7f4; border:1px solid #2b4059; }\n      .tbi-exec-dashboard-v2 .tbi-exec--positive { --exec-accent:#3ecf8e; }\n      .tbi-exec-dashboard-v2 .tbi-exec--caution { --exec-accent:#e4b84e; }\n      .tbi-exec-dashboard-v2 .tbi-exec--warning { --exec-accent:#ef8a4c; }\n      .tbi-exec-dashboard-v2 .tbi-exec--negative { --exec-accent:#ef5f6c; }\n      .tbi-exec-dashboard-v2 .tbi-exec--neutral { --exec-accent:#7294b8; }\n      .tbi-exec-dashboard-v2 .tbi-exec-badge[class*='tbi-exec--'],\n      .tbi-exec-dashboard-v2 [class*='tbi-exec--'] .tbi-exec-factor-card__score { color:var(--exec-accent); }\n      .tbi-exec-dashboard-v2 .tbi-exec-scorecard,\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel,\n      .tbi-exec-dashboard-v2 .tbi-exec-opportunity-panel,\n      .tbi-exec-dashboard-v2 .tbi-exec-data-quality { background:#0b1422; border:1px solid #1f3044; border-radius:14px; padding:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header { display:flex; align-items:center; justify-content:space-between; gap:12px; margin-bottom:14px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header__title { color:#eef5fc; margin:2px 0 0; font-size:17px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card { background:#101d2d; border:1px solid #22364c; border-top:3px solid var(--exec-accent,#7294b8); border-radius:11px; padding:14px; min-width:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__header { display:flex; justify-content:space-between; gap:10px; align-items:flex-start; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__title-group { display:flex; gap:7px; color:#dbe7f4; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__title { font-size:12px; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__score { font-size:20px; font-weight:800; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__meter { height:5px; background:#203044; border-radius:999px; margin:12px 0; overflow:hidden; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__meter-fill { height:100%; background:var(--exec-accent,#7294b8); }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__explanation { color:#91a7bd; font-size:11px; line-height:1.45; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-executive-decision-view__two-column { display:grid; grid-template-columns:1fr 1fr; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__list { list-style:none; padding:0; margin:0; display:grid; gap:9px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__item { display:flex; gap:9px; color:#cbd8e6; font-size:13px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__item svg { color:#ef8a4c; flex:0 0 auto; margin-top:2px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-opportunity-panel__grid { display:grid; gap:10px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout { display:flex; gap:12px; background:#101d2d; border:1px solid #22364c; border-left:3px solid var(--exec-accent,#3ecf8e); border-radius:10px; padding:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout__title { color:#edf5fc; font-size:12px; margin:0 0 3px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout__message { color:#9fb1c4; font-size:12px; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-data-quality__grid { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card { background:#101d2d; border:1px solid #22364c; border-radius:10px; padding:13px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card__header { display:flex; justify-content:space-between; color:#8fa5bc; font-size:11px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card__value { color:#f1f6fb; font-size:24px; font-weight:800; margin:5px 0; }\n      .tbi-exec-dashboard-v2 .tbi-executive-decision-view__scope-note { color:#7189a2; font-size:11px; display:flex; gap:7px; align-items:flex-start; }\n      .tbi-exec-dashboard-v2 .tbi-exec-empty-state { padding:24px; text-align:center; border:1px dashed #30465f; border-radius:12px; color:#91a7bd; background:#0b1422; }\n      @media (max-width:1100px) { .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }\n      @media (max-width:760px) { .tbi-exec-dashboard-v2 .tbi-executive-decision-view__two-column, .tbi-exec-dashboard-v2 .tbi-exec-data-quality__grid { grid-template-columns:1fr; } .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { grid-template-columns:1fr; } }\n    ")),
    
    shiny::div(
      class = "terminal-command-bar",
      shiny::div(
        class = "terminal-command-left",
        shiny::span(class = "terminal-code", "TBI / EXEC"),
        shiny::span(class = "terminal-divider"),
        shiny::span(class = "terminal-live-dot"),
        shiny::span(class = "terminal-command-copy", "LIVE DECISION ENVIRONMENT")
      ),
      shiny::div(
        class = "terminal-command-right",
        shiny::span("INTELLIGENCE"),
        shiny::span("FINANCE"),
        shiny::span("CONTEXT")
      )
    ),
    
    shiny::div(
      class = "executive-header-row",
      shiny::div(
        class = "executive-header-copy",
        shiny::div(class = "tbi-page-eyebrow", "FRONT OFFICE COMMAND CENTER"),
        shiny::h1(
          class = "executive-main-title",
          shiny::textOutput(ns("dashboard_title"), inline = TRUE)
        ),
        shiny::p(
          class = "executive-subtitle",
          "Integrated basketball, financial, roster, and asset decision support"
        )
      ),
      shiny::div(
        class = "executive-header-badge",
        shiny::span(class = "header-badge-label", "MODEL"),
        shiny::strong("TBI v1")
      )
    ),
    
    shiny::uiOutput(ns("executive_decision"), class = "executive-intelligence-shell"),
    
    shiny::uiOutput(ns("executive_status")),
    
    shiny::div(
      class = "terminal-kpi-grid terminal-kpi-grid-six",
      kpi_tile("Team Payroll", "team_payroll", "currency-dollar"),
      kpi_tile("Payroll Rank", "payroll_rank", "list-ol"),
      kpi_tile("Contracts", "contract_count", "person-vcard"),
      kpi_tile("Conference Rank", "conference_rank", "list-ol"),
      kpi_tile("Highest Paid Player", "highest_paid_player", "person-badge"),
      kpi_tile("Scoring", "kpi_scoring", "bullseye")
    ),
    
    shiny::div(
      class = "terminal-main-grid",
      shiny::tags$section(
        class = "terminal-panel snapshot-panel",
        shiny::div(
          class = "terminal-panel-header",
          shiny::div(
            shiny::div(class = "terminal-panel-kicker", "TEAM PROFILE"),
            shiny::h3("Team Snapshot")
          ),
          shiny::span(class = "terminal-panel-tag", "CURRENT")
        ),
        shiny::div(
          class = "metric-cell-grid",
          metric_cell("Conference", "snapshot_conference"),
          metric_cell("Conference Position", "snapshot_conference_rank"),
          metric_cell("Division Position", "snapshot_division_rank"),
          metric_cell("Scoring Average", "snapshot_scoring")
        )
      ),
      shiny::tags$section(
        class = "terminal-panel outlook-panel",
        shiny::div(
          class = "terminal-panel-header",
          shiny::div(
            shiny::div(class = "terminal-panel-kicker", "DECISION SUPPORT"),
            shiny::h3(shiny::textOutput(ns("outlook_heading"), inline = TRUE))
          ),
          shiny::span(
            class = "terminal-panel-tag terminal-panel-tag-accent",
            "ASSESSMENT"
          )
        ),
        shiny::div(
          class = "outlook-summary-terminal",
          shiny::textOutput(ns("outlook_summary"), inline = TRUE)
        ),
        shiny::div(
          class = "outlook-signal-grid",
          metric_cell("Competitive Position", "competitive_position"),
          metric_cell("Division Position", "division_position"),
          metric_cell("Scoring Profile", "scoring_profile")
        )
      )
    ),
    
    shiny::tags$section(
      class = "terminal-panel standings-panel",
      shiny::div(
        class = "terminal-panel-header",
        shiny::div(
          shiny::div(class = "terminal-panel-kicker", "LEAGUE CONTEXT"),
          shiny::h3(shiny::textOutput(ns("standings_heading"), inline = TRUE))
        ),
        shiny::span(class = "terminal-panel-tag", "LIVE TABLE")
      ),
      shiny::div(
        class = "terminal-table-wrap",
        reactable::reactableOutput(ns("conference_standings"))
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Executive Dashboard Server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive expression containing selected team name.
#' @noRd
mod_executive_dashboard_server <- function(id, selected_team) {
  shiny::moduleServer(id, function(input, output, session) {
    
    safe_value <- function(expr, default = NULL) {
      tryCatch(expr, error = function(e) default)
    }
    
    safe_payroll <- shiny::reactive({
      shiny::req(selected_team())
      result <- safe_value(get_team_payroll(selected_team()), data.frame())
      if (is.null(result)) data.frame() else result
    })
    
    safe_payroll_rank <- shiny::reactive({
      shiny::req(selected_team())
      value <- safe_value(get_team_payroll_rank(selected_team()), NA_real_)
      if (is.null(value) || !length(value)) return(NA_real_)
      suppressWarnings(as.numeric(value[[1]]))
    })
    
    safe_highest_paid <- shiny::reactive({
      shiny::req(selected_team())
      result <- safe_value(get_highest_paid_player(selected_team()), data.frame())
      if (is.null(result)) data.frame() else result
    })
    
    # Shared database first; legacy DuckDB only as a guarded fallback.
    standings_table <- shiny::reactive({
      shiny::req(selected_team())
      
      standings <- NULL
      
      if (exists("connect_db", mode = "function")) {
        standings <- safe_value({
          con <- connect_db(read_only = TRUE)
          on.exit(disconnect_db(con), add = TRUE)
          if (!"standings" %in% DBI::dbListTables(con)) return(NULL)
          DBI::dbGetQuery(
            con,
            "SELECT team_name, wins, losses, win_pct, conference_rank, division_rank, points_per_game, point_diff, conference FROM standings"
          )
        }, NULL)
      }
      
      if (is.null(standings)) {
        legacy_path <- file.path("inst", "app", "data", "basketball_ops.duckdb")
        if (file.exists(legacy_path) && requireNamespace("duckdb", quietly = TRUE)) {
          standings <- safe_value({
            con <- DBI::dbConnect(duckdb::duckdb(), dbdir = legacy_path, read_only = TRUE)
            on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
            DBI::dbGetQuery(
              con,
              "SELECT team_name, wins, losses, win_pct, conference_rank, division_rank, points_per_game, point_diff, conference FROM standings"
            )
          }, NULL)
        }
      }
      
      if (is.null(standings) || !nrow(standings)) return(NULL)
      
      numeric_columns <- intersect(
        c("wins", "losses", "win_pct", "conference_rank", "division_rank", "points_per_game", "point_diff"),
        names(standings)
      )
      standings[numeric_columns] <- lapply(standings[numeric_columns], function(x) suppressWarnings(as.numeric(x)))
      
      for (conference_name in unique(standings$conference)) {
        idx <- which(standings$conference == conference_name)
        current_rank <- standings$conference_rank[idx]
        if (any(is.na(current_rank))) {
          win_pct <- standings$win_pct[idx]; win_pct[is.na(win_pct)] <- -Inf
          wins <- standings$wins[idx]; wins[is.na(wins)] <- -Inf
          point_diff <- standings$point_diff[idx]; point_diff[is.na(point_diff)] <- -Inf
          ord <- order(-win_pct, -wins, -point_diff, standings$team_name[idx])
          computed <- integer(length(idx)); computed[ord] <- seq_along(ord)
          current_rank[is.na(current_rank)] <- computed[is.na(current_rank)]
          standings$conference_rank[idx] <- current_rank
        }
      }
      
      standings[order(standings$conference, standings$conference_rank), , drop = FALSE]
    })
    
    team_data <- shiny::reactive({
      standings <- standings_table()
      if (is.null(standings) || !nrow(standings)) return(NULL)
      team <- standings[standings$team_name == selected_team(), , drop = FALSE]
      if (!nrow(team)) NULL else team[1, , drop = FALSE]
    })
    
    conference_data <- shiny::reactive({
      standings <- standings_table()
      team <- team_data()
      if (is.null(standings) || is.null(team)) return(NULL)
      result <- standings[
        standings$conference == team$conference[[1]],
        c("conference_rank", "team_name", "wins", "losses", "win_pct", "point_diff"),
        drop = FALSE
      ]
      result[order(result$conference_rank, -result$wins), , drop = FALSE]
    })
    
    competitive_tier <- shiny::reactive({
      team <- team_data()
      if (is.null(team)) return("Unknown")
      rank <- suppressWarnings(as.numeric(team$conference_rank[[1]]))
      if (is.na(rank)) return("Unknown")
      if (rank <= 3) "Contender" else if (rank <= 6) "Playoff" else if (rank <= 10) "Play-In" else "Rebuilding"
    })
    
    payroll_operating_status <- shiny::reactive({
      # This is explicitly an assumption-based status until the shared cap
      # engine supplies the official operating-status object to this module.
      rank <- safe_payroll_rank()
      if (is.na(rank)) return("Unknown")
      if (rank <= 5) "Above Second Apron" else if (rank <= 10) "Above First Apron" else if (rank <= 20) "Tax Team" else "Over Cap"
    })
    
    draft_value_result <- shiny::reactive({
      if (exists("evaluate_team_draft_value", mode = "function")) {
        result <- safe_value(evaluate_team_draft_value(selected_team()), NULL)
        if (!is.null(result)) return(result)
      }
      list(summary = list(portfolio_grade = "Unrated", review_required = 0L))
    })
    
    draft_simulation_result <- shiny::reactive({
      if (exists("simulate_team_draft_portfolio", mode = "function")) {
        result <- safe_value(
          simulate_team_draft_portfolio(selected_team(), iterations = 250L),
          NULL
        )
        if (!is.null(result)) return(result)
      }
      NULL
    })
    
    basketball_intelligence <- shiny::reactive({
      team <- team_data()
      payroll <- safe_payroll()
      rank <- safe_payroll_rank()
      
      contract_count <- if (nrow(payroll) && "contracts" %in% names(payroll)) {
        suppressWarnings(as.integer(payroll$contracts[[1]]))
      } else {
        NA_integer_
      }
      
      inputs <- list(
        competitive = list(
          competitive_tier = competitive_tier(),
          projected_wins = if (!is.null(team)) suppressWarnings(as.numeric(team$wins[[1]])) else NA_real_,
          playoff_probability = NA_real_,
          championship_probability = NA_real_
        ),
        cap_result = list(
          operating_status = payroll_operating_status()
        ),
        financial = list(
          top_three_concentration = NA_real_,
          future_committed_salary_ratio = NA_real_
        ),
        roster = list(
          guaranteed_roster_spots = contract_count,
          expiring_contracts = NA_integer_,
          team_options = NA_integer_,
          player_options = NA_integer_,
          two_way_contracts = NA_integer_,
          dead_money_ratio = NA_real_
        ),
        draft_value_result = draft_value_result(),
        draft_simulation_result = draft_simulation_result(),
        transaction = list(
          manual_review_items = if (is.na(rank)) 1L else 0L
        )
      )
      
      if (exists("evaluate_basketball_decision", mode = "function")) {
        return(safe_value(evaluate_basketball_decision(inputs), NULL))
      }
      
      NULL
    })
    
    executive_opportunities <- shiny::reactive({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) return(character())
      opportunities <- character()
      components <- intelligence$components
      
      if (!is.null(components$competitive_position$score) && components$competitive_position$score >= 65) {
        opportunities <- c(opportunities, "Competitive positioning supports targeted win-now evaluation.")
      }
      if (!is.null(components$roster_control$score) && components$roster_control$score >= 60) {
        opportunities <- c(opportunities, "Current roster control provides optionality for sequencing future moves.")
      }
      if (!is.null(components$draft_capital$score) && components$draft_capital$score >= 60) {
        opportunities <- c(opportunities, "Draft capital can support a selective transaction without exhausting the asset base.")
      }
      if (!length(opportunities)) {
        opportunities <- "Preserve flexibility while comparing lower-cost paths to improve the roster."
      }
      opportunities
    })
    
    data_quality_summary <- shiny::reactive({
      team_available <- !is.null(team_data())
      payroll_available <- nrow(safe_payroll()) > 0
      draft_grade <- executive_get(draft_value_result(), c("summary", "portfolio_grade"), "Unrated")
      draft_available <- !identical(draft_grade, "Unrated")
      
      list(
        verified_items = sum(c(team_available, payroll_available, draft_available)),
        assumption_items = 2L,
        review_items = if (draft_available) 0L else 1L,
        unavailable_items = sum(!c(team_available, payroll_available, draft_available)),
        updated_at = format(Sys.Date(), "%Y-%m-%d")
      )
    })
    
    output$executive_decision <- shiny::renderUI({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) {
        return(executive_empty_state(
          title = "Executive intelligence unavailable",
          message = "The dashboard could not assemble a complete decision-support result from the currently loaded data.",
          icon = "brain"
        ))
      }
      
      executive_decision_view(
        intelligence_result = intelligence,
        opportunities = executive_opportunities(),
        data_quality = data_quality_summary()
      )
    })
    
    output$dashboard_title <- shiny::renderText({
      paste(selected_team(), "Executive Dashboard")
    })
    
    output$executive_status <- shiny::renderUI({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) return(NULL)
      shiny::div(
        class = "executive-status-strip",
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Recommendation"),
          shiny::strong(intelligence$recommendation)
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Decision Score"),
          shiny::strong(sprintf("%.1f / 100", intelligence$score))
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Competitive Tier"),
          shiny::strong(competitive_tier())
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Financial Status"),
          shiny::strong(payroll_operating_status())
        )
      )
    })
    
    output$team_payroll <- shiny::renderText({
      payroll <- safe_payroll()
      if (!nrow(payroll) || !"cap_hit" %in% names(payroll)) return("Unavailable")
      value <- suppressWarnings(as.numeric(payroll$cap_hit[[1]]))
      if (is.na(value)) {
        "Unavailable"
      } else {
        paste0("$", format(round(value / 1e6, 1), nsmall = 1, trim = TRUE), "M")
      }
    })
    
    output$payroll_rank <- shiny::renderText({
      rank <- safe_payroll_rank()
      if (is.na(rank)) "Unavailable" else paste0("#", rank)
    })
    
    output$contract_count <- shiny::renderText({
      payroll <- safe_payroll()
      if (!nrow(payroll) || !"contracts" %in% names(payroll)) return("Unavailable")
      as.character(payroll$contracts[[1]])
    })
    
    output$conference_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0("#", team$conference_rank[[1]])
    })
    
    output$highest_paid_player <- shiny::renderText({
      player <- safe_highest_paid()
      if (!nrow(player) || !"player_name" %in% names(player)) "Unavailable" else as.character(player$player_name[[1]])
    })
    
    output$kpi_scoring <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0(sprintf("%.1f", team$points_per_game[[1]]), " PPG")
    })
    
    output$snapshot_conference <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else as.character(team$conference[[1]])
    })
    
    output$snapshot_conference_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0("#", team$conference_rank[[1]])
    })
    
    output$snapshot_division_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team) || is.na(team$division_rank[[1]])) "Unavailable" else paste0("#", team$division_rank[[1]])
    })
    
    output$snapshot_scoring <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0(sprintf("%.1f", team$points_per_game[[1]]), " PPG")
    })
    
    output$division_position <- shiny::renderText({
      team <- team_data()
      if (is.null(team) || is.na(team$division_rank[[1]])) "Unavailable" else paste0("#", team$division_rank[[1]], " in division")
    })
    
    output$competitive_position <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) return("Unavailable")
      paste0("#", team$conference_rank[[1]], " in conference — ", competitive_tier())
    })
    
    output$scoring_profile <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) return("Unavailable")
      paste0(
        sprintf("%.1f", team$points_per_game[[1]]),
        " PPG | ",
        sprintf("%+.1f", team$point_diff[[1]]),
        " differential"
      )
    })
    
    output$outlook_heading <- shiny::renderText({
      paste(selected_team(), "Strategic Outlook")
    })
    
    output$outlook_summary <- shiny::renderText({
      team <- team_data()
      intelligence <- basketball_intelligence()
      if (is.null(team) || is.null(intelligence)) {
        return(paste("A complete strategic outlook is unavailable for", selected_team()))
      }
      paste0(
        selected_team(), " is currently #", team$conference_rank[[1]],
        " in the ", team$conference[[1]], " Conference with a ",
        sprintf("%+.1f", team$point_diff[[1]]), " point differential. ",
        intelligence$executive_summary
      )
    })
    
    output$standings_heading <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Conference Standings" else paste(team$conference[[1]], "Conference Standings")
    })
    
    output$conference_standings <- reactable::renderReactable({
      standings <- conference_data()
      shiny::validate(shiny::need(!is.null(standings) && nrow(standings) > 0, "Conference standings are unavailable."))
      
      reactable::reactable(
        standings,
        searchable = FALSE,
        pagination = FALSE,
        compact = TRUE,
        bordered = FALSE,
        striped = FALSE,
        highlight = TRUE,
        defaultSorted = "conference_rank",
        rowStyle = function(index) {
          row <- standings[index, ]
          if (identical(as.character(row$team_name[[1]]), as.character(selected_team()))) {
            return(list(fontWeight = "700", background = "#122033", color = "white", borderLeft = "4px solid #4f8cff"))
          }
          list(background = "#0b1422", color = "#d8e2ef", borderBottom = "1px solid #1d2c40")
        },
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#08111f", color = "#9fb3c8", fontWeight = 600, borderBottom = "1px solid #243244"),
          style = list(fontSize = "13px")
        ),
        columns = list(
          conference_rank = reactable::colDef(name = "Rank", align = "center", width = 70),
          team_name = reactable::colDef(name = "Team", minWidth = 180),
          wins = reactable::colDef(name = "W", align = "center", width = 60),
          losses = reactable::colDef(name = "L", align = "center", width = 60),
          win_pct = reactable::colDef(name = "Win %", align = "center", width = 90, format = reactable::colFormat(digits = 3)),
          point_diff = reactable::colDef(name = "+/-", align = "center", width = 80, format = reactable::colFormat(digits = 1))
        )
      )
    })
  })
}
