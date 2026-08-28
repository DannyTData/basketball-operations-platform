# ------------------------------------------------------------
# Module: Salary Cap Intelligence
# Version 2 Executive Financial Workspace
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

cap_market_default_year <- function(years, selected_season) {
  years <- sort(unique(suppressWarnings(as.integer(years))))
  years <- years[!is.na(years)]
  if (!length(years)) return("")
  start_year <- suppressWarnings(as.integer(substr(as.character(selected_season), 1L, 4L)))
  upcoming <- years[years >= start_year + 1L]
  as.character(if (length(upcoming)) upcoming[[1L]] else utils::tail(years, 1L))
}

cap_market_performance_season <- function(selected_season) {
  start_year <- suppressWarnings(as.integer(substr(as.character(selected_season), 1L, 4L)))
  if (is.na(start_year)) return(NA_character_)
  paste0(start_year - 1L, "-", substr(as.character(start_year), 3L, 4L))
}

cap_financial_decision_state <- function(status) {
  list(
    decision = switch(
      status,
      "Above Second Apron" = "RESTRICTED",
      "Above First Apron" = "CAUTION",
      "Tax Team" = "MANAGE",
      "Over Cap" = "MANAGE",
      "Below Cap" = "FLEXIBLE",
      "REVIEW"
    ),
    explanation = switch(
      status,
      "Above Second Apron" = paste("Operate defensively. Second-apron exposure materially", "reduces transaction flexibility and requires close CBA review."),
      "Above First Apron" = paste("Preserve optionality. First-apron exposure creates", "meaningful roster-building restrictions."),
      "Tax Team" = paste("Manage tax exposure while maintaining enough flexibility", "to respond to roster opportunities."),
      "Over Cap" = paste("The team is operating above the salary cap but remains", "outside the tax and apron bands."),
      "Below Cap" = paste("The organization currently retains meaningful cap flexibility", "for roster construction."),
      "Verified threshold data is required for a final operating read."
    )
  )
}

cap_tpe_display_value <- function(count) {
  value <- suppressWarnings(as.integer(count %||% NA_integer_))
  if (!length(value) || is.na(value[[1L]])) return("TPE data not loaded")
  as.character(value[[1L]])
}

cap_market_prepare <- function(data) {
  stopifnot(is.data.frame(data))
  if (!nrow(data)) return(data)
  text <- function(name) {
    value <- as.character(roster_column(data, name, NA_character_))
    value[!is.na(value) & !nzchar(trimws(value))] <- NA_character_
    value
  }
  numeric <- function(name) {
    suppressWarnings(as.numeric(roster_column(data, name, NA_real_)))
  }

  data$free_agent_year <- suppressWarnings(as.integer(data$free_agent_year))
  data$cap_hit <- numeric("cap_hit")
  data$bie <- numeric("bie")
  data$free_agent_status <- text("free_agent_status")
  data$option_type <- text("option_type")
  data$contract_end_season <- text("contract_end_season")
  data$verified_at <- text("verified_at")

  incomplete <- is.na(data$free_agent_year) | is.na(data$free_agent_status) |
    is.na(data$contract_end_season) | is.na(data$verified_at)
  conditional <- !is.na(data$option_type)
  canonical_quality <- roster_contract_quality_status(data)
  data$data_quality <- ifelse(
    incomplete,
    "UNKNOWN",
    ifelse(conditional, "REQUIRES SOURCE VERIFICATION", canonical_quality)
  )
  data$loaded_window_fields <- !incomplete & !conditional
  data$market_window_status <- ifelse(data$data_quality == "CURRENT", "CURRENT CONTRACT WINDOW", "REVIEW")
  data$bie_grade <- ifelse(
    is.finite(data$bie),
    vapply(data$bie, function(value) if (is.finite(value)) bie_player_grade(value) else "UNKNOWN", character(1)),
    text("bie_grade")
  )
  data$bie_grade[is.na(data$bie_grade)] <- "UNKNOWN"
  data
}

cap_market_filter <- function(data,
                              year = "",
                              position = "",
                              free_agent_type = "",
                              bie_grade = "",
                              search = "",
                              current_team = "",
                              sort_by = "BIE_DESC") {
  stopifnot(is.data.frame(data))
  if (!nrow(data)) return(data)
  keep <- rep(TRUE, nrow(data))
  exact <- function(values, selected) {
    selected <- trimws(as.character(selected %||% ""))
    if (!nzchar(selected)) return(rep(TRUE, length(values)))
    !is.na(values) & as.character(values) == selected
  }
  keep <- keep & exact(data$free_agent_year, year)
  keep <- keep & roster_position_matches(data$primary_position, position)
  keep <- keep & exact(data$free_agent_status, free_agent_type)
  keep <- keep & exact(data$bie_grade, bie_grade)
  keep <- keep & exact(data$team_name, current_team)
  search <- tolower(trimws(as.character(search %||% "")))
  if (nzchar(search)) keep <- keep & grepl(search, tolower(data$player_name), fixed = TRUE)
  result <- data[keep, , drop = FALSE]
  if (!nrow(result)) return(result)
  order_index <- switch(
    sort_by,
    SALARY_DESC = order(-result$cap_hit, result$player_name, na.last = TRUE, method = "radix"),
    PLAYER_ASC = order(result$player_name, method = "radix"),
    order(-result$bie, result$player_name, na.last = TRUE, method = "radix")
  )
  result[order_index, , drop = FALSE]
}

cap_market_display_status <- function(value) {
  status <- toupper(trimws(as.character(value)))
  status[is.na(value) | !nzchar(status)] <- "UNKNOWN"
  mapped <- rep("UNKNOWN", length(status))
  mapped[status %in% c("CURRENT", "CURRENT CONTRACT WINDOW")] <-
    "CURRENT / VERIFIED"
  mapped[status %in% c("REVIEW", "REQUIRES REVIEW", "REQUIRES SOURCE VERIFICATION")] <-
    "REQUIRES REVIEW"
  pass_through <- status %in% c("UNKNOWN", "STALE", "CONFLICT")
  mapped[pass_through] <- status[pass_through]
  unname(mapped)
}

#' Salary Cap Intelligence UI
#'
#' @param id Internal module ID.
#' @noRd
mod_salary_cap_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # ----------------------------------------------------------
  # Local CBA-link treatment for this page
  # ----------------------------------------------------------
  
  shiny::tags$style(
    shiny::HTML(
      "
      .tbi-cap-threshold-cba-link {
        color:#f1f5f9 !important;
        font-weight:800 !important;
        text-align:left;
      }

      .tbi-cap-threshold-cba-link::after {
        content:'  ↗';
        color:#5f9fee;
        font-size:.62em;
        opacity:.72;
      }

      .tbi-cap-threshold-cba-link:hover {
        color:#78b3ff !important;
        text-decoration:none !important;
      }

      .tbi-v2-score-name:has(.tbi-cap-threshold-cba-link) {
        position:relative;
      }

      .tbi-v2-score-name:has(.tbi-cap-threshold-cba-link):hover {
        background:rgba(59,130,246,.025);
      }

      .tbi-cap-scenario-banner {
        display:flex;
        align-items:center;
        justify-content:space-between;
        gap:16px;
        padding:11px 14px;
        border:1px solid rgba(96,165,250,.24);
        border-radius:10px;
        background:
          linear-gradient(
            90deg,
            rgba(59,130,246,.10),
            rgba(59,130,246,.035)
          );
      }

      .tbi-cap-scenario-banner-copy {
        display:flex;
        align-items:center;
        gap:10px;
        min-width:0;
      }

      .tbi-cap-scenario-badge {
        display:inline-flex;
        align-items:center;
        gap:6px;
        padding:5px 8px;
        border-radius:999px;
        background:rgba(59,130,246,.12);
        color:#79b6ff;
        font-size:.55rem;
        font-weight:900;
        letter-spacing:.08em;
        white-space:nowrap;
      }

      .tbi-cap-scenario-text {
        color:#aab8c9;
        font-size:.61rem;
        line-height:1.4;
      }

      .tbi-cap-scenario-delta {
        color:#f4f7fb;
        font-size:.64rem;
        font-weight:850;
        white-space:nowrap;
      }
      "
    )
  )
  
  snapshot_item <- function(label, output_id, icon, tone = "blue") {
    shiny::div(
      class = paste(
        "tbi-v2-snapshot-item",
        paste0("tbi-v2-tone-", tone)
      ),
      shiny::div(
        class = "tbi-v2-snapshot-icon",
        bsicons::bs_icon(icon)
      ),
      shiny::div(
        class = "tbi-v2-snapshot-copy",
        shiny::span(
          class = "tbi-v2-snapshot-label",
          label
        ),
        shiny::strong(
          class = "tbi-v2-snapshot-value",
          shiny::textOutput(
            ns(output_id),
            inline = TRUE
          )
        )
      )
    )
  }
  
  shiny::div(
    class = "tbi-module-page tbi-v2-cap-page",
    `data-tbi-subtab-input` = ns("active_subtab"),
    
    # --------------------------------------------------------
    # Page identity
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-module-intro",
      
      shiny::div(
        shiny::div(
          class = "tbi-page-eyebrow",
          "FINANCIAL STRATEGY"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Salary Cap Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Monitor payroll, apron exposure, exception inventory,",
            "contract concentration, and financial flexibility."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "CBA VIEW"
        ),
        shiny::strong("2026-27")
      )
    ),
    
    shiny::div(
      class = "tbi-cap-tab-target",
      `data-tbi-cap-tab` = "overview",
      shiny::uiOutput(
        ns("scenario_banner")
      )
    ),

    shiny::div(
      class = "cap-v2-overview-grid tbi-cap-tab-target",
      `data-tbi-cap-tab` = "overview",
      shiny::tags$section(
        class = "tbi-v2-context-panel cap-v2-overview-panel",
        shiny::div(class = "tbi-v2-context-header", shiny::div(shiny::div(class = "tbi-page-eyebrow", "FINANCIAL POSITION"), shiny::h3("Current money position"))),
        shiny::div(class = "cap-v2-overview-body", shiny::uiOutput(ns("overview_position")))
      ),
      shiny::tags$section(
        class = "tbi-v2-context-panel cap-v2-overview-panel",
        shiny::div(class = "tbi-v2-context-header", shiny::div(shiny::div(class = "tbi-page-eyebrow", "APRON / FLEXIBILITY"), shiny::h3("Operating room"))),
        shiny::div(class = "cap-v2-overview-body", shiny::uiOutput(ns("overview_flexibility")))
      ),
      shiny::tags$section(
        class = "tbi-v2-context-panel cap-v2-overview-panel",
        shiny::div(class = "tbi-v2-context-header", shiny::div(shiny::div(class = "tbi-page-eyebrow", "TEAM-BUILDING WATCH"), shiny::h3("Next financial decision"))),
        shiny::div(class = "cap-v2-overview-body", shiny::uiOutput(ns("overview_watch")))
      )
    ),
    
    # --------------------------------------------------------
    # Executive snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot tbi-v2-cap-snapshot tbi-cap-tab-target",
      `data-tbi-cap-tab` = "overview",
      
      shiny::div(
        class = "tbi-v2-section-title-row",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("cash-stack")
          ),
          shiny::span("FINANCIAL SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "CURRENT"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "TEAM PAYROLL",
          "team_payroll",
          "cash-stack",
          "blue"
        ),
        
        snapshot_item(
          "CAP STATUS",
          "cap_status",
          "currency-dollar",
          "orange"
        ),
        
        snapshot_item(
          "CAP ROOM / OVERAGE",
          "cap_room",
          "graph-up-arrow",
          "blue"
        ),
        
        snapshot_item(
          "2ND APRON ROOM",
          "second_apron_room",
          "exclamation-triangle",
          "orange"
        ),
        
        snapshot_item(
          "ACTIVE TPEs",
          "active_tpes",
          "arrow-left-right",
          "green"
        ),
        
        snapshot_item(
          "CONTRACTS",
          "contract_count",
          "people",
          "purple"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Financial decision + threshold scorecard
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-main-grid tbi-v2-cap-main-grid tbi-cap-tab-target tbi-cap-hidden",
      `data-tbi-cap-tab` = "decision",
      
      shiny::tags$section(
        class = "tbi-v2-decision-card tbi-v2-cap-decision",
        
        shiny::div(
          class = "tbi-v2-section-title",
          
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-warning",
            bsicons::bs_icon("exclamation-triangle")
          ),
          
          shiny::span("FINANCIAL OPERATING DECISION")
        ),
        
        shiny::uiOutput(
          ns("financial_decision")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-scorecard-panel tbi-v2-cap-threshold-panel",
        
        shiny::div(
          class = "tbi-v2-scorecard-header",
          
          shiny::div(
            class = "tbi-v2-section-title",
            shiny::span(
              class = "tbi-v2-section-icon",
              bsicons::bs_icon("graph-up-arrow")
            ),
            shiny::span("CAP THRESHOLD POSITION")
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Payroll"),
            shiny::strong(
              shiny::textOutput(
                ns("payroll_pct_cap"),
                inline = TRUE
              )
            ),
            shiny::span("of cap")
          )
        ),
        
        shiny::uiOutput(
          ns("threshold_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # CBA alerts / risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-bottom-grid tbi-v2-cap-alert-grid tbi-cap-tab-target tbi-cap-hidden",
      `data-tbi-cap-tab` = "risk",
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-headlines-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("currency-dollar")
          ),
          shiny::span("CBA ALERTS")
        ),
        
        shiny::uiOutput(
          ns("cba_alerts")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-risks-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-danger",
            bsicons::bs_icon("exclamation-triangle")
          ),
          shiny::span("FINANCIAL RISKS")
        ),
        
        shiny::uiOutput(
          ns("financial_risks")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-opportunities-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-success",
            bsicons::bs_icon("bullseye")
          ),
          shiny::span("FLEXIBILITY OPPORTUNITIES")
        ),
        
        shiny::uiOutput(
          ns("financial_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Contract ledger + front office readout
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-cap-detail-grid tbi-cap-tab-target tbi-cap-hidden",
      `data-tbi-cap-tab` = "contracts",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel tbi-v2-cap-ledger",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "CONTRACT LEDGER"
            ),
            shiny::h3("Current-season commitments")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "LIVE DATA"
          )
        ),
        
        shiny::div(
          class = "tbi-v2-cap-table-wrap",
          reactable::reactableOutput(
            ns("salary_table")
          )
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel tbi-v2-cap-readout",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "CAP ENGINE"
            ),
            shiny::h3("Front-office readout")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "DECISION SUPPORT"
          )
        ),
        
        shiny::div(
          class = "tbi-v2-cap-readout-body",
          shiny::uiOutput(
            ns("cap_signals")
          )
        )
      )
    ),

    shiny::tags$section(
      class = "cap-v2-market tbi-v2-context-panel tbi-cap-tab-target tbi-cap-hidden",
      `data-tbi-cap-tab` = "market",
      shiny::div(
        class = "tbi-v2-context-header",
        shiny::div(
          shiny::div(class = "tbi-page-eyebrow", "LEAGUE CONTRACT DATA"),
          shiny::h3("Free Agent Market")
        ),
        shiny::span(class = "tbi-v2-context-tag", "LOADED DATA / REVIEW")
      ),
      shiny::div(
        class = "cap-v2-market-body",
        shiny::p(
          class = "cap-v2-market-note",
          paste(
            "Loaded contract facts and same-season BIE outputs only.",
            "REQUIRES REVIEW means contract dates, option status, or source reconciliation",
            "leaves free-agency availability unverified."
          )
        ),
        shiny::div(
          class = "cap-v2-market-toolbar",
          shiny::selectInput(ns("market_year"), "FA year", choices = character()),
          shiny::selectInput(ns("market_position"), "Position", choices = c("All positions" = "", "PG", "SG", "SF", "PF", "C")),
          shiny::selectInput(ns("market_type"), "FA type", choices = c("All types" = "")),
          shiny::selectInput(ns("market_grade"), "BIE grade", choices = c("All grades" = "")),
          shiny::selectInput(ns("market_team"), "Current team", choices = c("All teams" = "")),
          shiny::selectInput(ns("market_sort"), "Sort", choices = c("BIE high to low" = "BIE_DESC", "Salary high to low" = "SALARY_DESC", "Player A-Z" = "PLAYER_ASC")),
          shiny::textInput(ns("market_search"), "Player search", placeholder = "Search player"),
          shiny::actionButton(ns("clear_market_filters"), "Clear filters", class = "cap-v2-clear-filters")
        ),
        shiny::div(class = "cap-v2-market-summary", shiny::uiOutput(ns("market_summary"))),
        shiny::div(class = "tbi-v2-cap-table-wrap cap-v2-market-table", reactable::reactableOutput(ns("market_table")))
      )
    ),

    shiny::tags$section(
      class = "cap-v2-recommendation tbi-cap-tab-target tbi-cap-hidden",
      `data-tbi-cap-tab` = "recommendation",
      shiny::div(
        class = "cap-v2-recommendation-posture",
        shiny::div(class = "tbi-page-eyebrow", "FINANCIAL RECOMMENDATION"),
        shiny::uiOutput(ns("cap_recommendation_posture"))
      ),
      shiny::div(
        class = "cap-v2-recommendation-grid",
        shiny::tags$section(
          class = "tbi-v2-context-panel",
          shiny::div(class = "tbi-v2-context-header", shiny::h3("Money position & flexibility")),
          shiny::div(class = "cap-v2-recommendation-body", shiny::uiOutput(ns("cap_recommendation_money")))
        ),
        shiny::tags$section(
          class = "tbi-v2-context-panel",
          shiny::div(class = "tbi-v2-context-header", shiny::h3("Contract & market watch")),
          shiny::div(class = "cap-v2-recommendation-body", shiny::uiOutput(ns("cap_recommendation_watch")))
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Salary Cap Intelligence server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @noRd
mod_salary_cap_server <- function(
    id,
    selected_team,
    selected_season,
    transaction_state = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
      money <- function(x) {
        x <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(x) ||
          is.na(x)
        ) {
          return("$0")
        }
        
        if (abs(x) >= 1e9) {
          return(
            sprintf(
              "$%.2fB",
              x / 1e9
            )
          )
        }
        
        if (abs(x) >= 1e6) {
          return(
            sprintf(
              "$%.1fM",
              x / 1e6
            )
          )
        }
        
        if (abs(x) >= 1e3) {
          return(
            sprintf(
              "$%.0fK",
              x / 1e3
            )
          )
        }
        
        paste0(
          "$",
          format(
            round(x),
            big.mark = ",",
            scientific = FALSE
          )
        )
      }
      
      pct_of <- function(
    value,
    threshold) {
        
        value <- suppressWarnings(
          as.numeric(value)
        )
        
        threshold <- suppressWarnings(
          as.numeric(threshold)
        )
        
        if (
          !length(value) ||
          !length(threshold) ||
          is.na(value) ||
          is.na(threshold) ||
          threshold <= 0
        ) {
          return("—")
        }
        
        sprintf(
          "%.1f%%",
          100 * value / threshold
        )
      }
      
      numeric_or_zero <- function(x) {
        x <- suppressWarnings(
          as.numeric(x)
        )
        
        x[
          is.na(x)
        ] <- 0
        
        x
      }
      
      # ------------------------------------------------------
      # Contract data
      # ------------------------------------------------------
      
      base_salary_data <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        con <- connect_db()
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        DBI::dbGetQuery(
          con,
          "
          SELECT
            p.player_id,
            p.player_name,
            p.primary_position,
            cy.base_salary,
            cy.cap_hit,
            cy.guaranteed_amount,
            cy.option_type,
            cy.verified_at,
            c.contract_type,
            c.contract_end_season,
            c.free_agent_year,
            c.bird_rights
          FROM contract_years cy
          JOIN players p
            ON p.player_id = cy.player_id
          JOIN teams t
            ON t.team_id = cy.team_id
          LEFT JOIN contracts c
            ON c.contract_id = cy.contract_id
          WHERE t.team_name = ?
            AND cy.season = ?
          ORDER BY
            cy.cap_hit DESC,
            p.player_name
          ",
          params = list(
            selected_team(),
            selected_season()
          )
        )
      })
      
      base_salary_data <- shiny::bindCache(
        base_salary_data,
        selected_team(),
        selected_season(),
        cache = "session"
      )

      market_data <- shiny::reactive({
        shiny::req(selected_season())
        performance_season <- cap_market_performance_season(selected_season())
        con <- connect_db(read_only = TRUE)
        on.exit(disconnect_db(con), add = TRUE)
        rows <- DBI::dbGetQuery(
          con,
          "
          WITH canonical_impact AS (
            SELECT *
            FROM (
              SELECT
                player_id,
                season,
                bie_performance_rating,
                impact_tier,
                impact_confidence,
                ROW_NUMBER() OVER (
                  PARTITION BY player_id
                  ORDER BY minutes DESC, games_played DESC, team_id ASC
                ) AS impact_rank
              FROM player_season_impact
              WHERE season = ?
            )
            WHERE impact_rank = 1
          ),
          contract_options AS (
            SELECT
              contract_id,
              GROUP_CONCAT(DISTINCT NULLIF(TRIM(option_type), '')) AS option_type
            FROM contract_years
            WHERE option_type IS NOT NULL
              AND TRIM(option_type) <> ''
            GROUP BY contract_id
          )
          SELECT
            p.player_id,
            p.player_name,
            p.primary_position,
            p.player_age,
            t.team_name,
            cy.cap_hit,
            c.contract_type,
            c.contract_end_season,
            c.free_agent_year,
            c.free_agent_status,
            options.option_type,
            cy.verified_at,
            impact.bie_performance_rating AS bie,
            impact.impact_tier AS bie_grade,
            impact.season AS bie_season,
            impact.impact_confidence AS bie_confidence
          FROM contract_years cy
          JOIN players p ON p.player_id = cy.player_id
          JOIN teams t ON t.team_id = cy.team_id
          LEFT JOIN contracts c ON c.contract_id = cy.contract_id
          LEFT JOIN contract_options options ON options.contract_id = cy.contract_id
          LEFT JOIN canonical_impact impact ON impact.player_id = p.player_id
          WHERE cy.season = ?
          ORDER BY p.player_name, t.team_name
          ",
          params = list(performance_season, selected_season())
        )
        rows <- rows[!duplicated(rows$player_id), , drop = FALSE]
        cap_market_prepare(rows)
      })

      market_data <- shiny::bindCache(
        market_data,
        selected_season(),
        cache = "session"
      )

      shiny::observeEvent(list(input$active_subtab, selected_season()), {
        shiny::req(identical(input$active_subtab, "market"))
        market <- market_data()
        years <- sort(unique(market$free_agent_year[!is.na(market$free_agent_year)]))
        types <- sort(unique(market$free_agent_status[!is.na(market$free_agent_status)]))
        grades <- sort(unique(market$bie_grade[!is.na(market$bie_grade) & market$bie_grade != "UNKNOWN"]))
        teams <- sort(unique(market$team_name[!is.na(market$team_name)]))
        selected_year <- roster_filter_selection(as.character(years), input$market_year)
        if (!nzchar(selected_year)) selected_year <- cap_market_default_year(years, selected_season())
        shiny::updateSelectInput(session, "market_year", choices = stats::setNames(as.character(years), as.character(years)), selected = selected_year)
        shiny::updateSelectInput(session, "market_type", choices = c("All types" = "", stats::setNames(types, types)), selected = roster_filter_selection(types, input$market_type))
        shiny::updateSelectInput(session, "market_grade", choices = c("All grades" = "", stats::setNames(grades, grades)), selected = roster_filter_selection(grades, input$market_grade))
        shiny::updateSelectInput(session, "market_team", choices = c("All teams" = "", stats::setNames(teams, teams)), selected = roster_filter_selection(teams, input$market_team))
      }, ignoreInit = FALSE)

      shiny::observeEvent(input$clear_market_filters, {
        years <- sort(unique(market_data()$free_agent_year[!is.na(market_data()$free_agent_year)]))
        shiny::updateSelectInput(session, "market_year", selected = cap_market_default_year(years, selected_season()))
        shiny::updateSelectInput(session, "market_position", selected = "")
        shiny::updateSelectInput(session, "market_type", selected = "")
        shiny::updateSelectInput(session, "market_grade", selected = "")
        shiny::updateSelectInput(session, "market_team", selected = "")
        shiny::updateSelectInput(session, "market_sort", selected = "BIE_DESC")
        shiny::updateTextInput(session, "market_search", value = "")
      }, ignoreInit = TRUE)

      filtered_market <- shiny::reactive({
        cap_market_filter(
          market_data(),
          year = input$market_year %||% cap_market_default_year(market_data()$free_agent_year, selected_season()),
          position = input$market_position %||% "",
          free_agent_type = input$market_type %||% "",
          bie_grade = input$market_grade %||% "",
          search = input$market_search %||% "",
          current_team = input$market_team %||% "",
          sort_by = input$market_sort %||% "BIE_DESC"
        )
      })

      
      salary_data <- shiny::reactive({
        
        current <- base_salary_data()
        
        if (
          is.null(transaction_state) ||
          !exists(
            "tbi_apply_trade_scenario_to_roster",
            mode = "function"
          )
        ) {
          return(current)
        }
        
        preview <- tbi_apply_trade_scenario_to_roster(
          roster = current,
          transaction_state = transaction_state,
          team_name = selected_team()
        )
        
        if (
          nrow(preview) &&
          "cap_hit" %in% names(preview)
        ) {
          preview <- preview[
            order(
              suppressWarnings(
                as.numeric(
                  preview$cap_hit
                )
              ),
              decreasing = TRUE,
              na.last = TRUE
            ),
            ,
            drop = FALSE
          ]
        }
        
        preview
      })
      
      # ------------------------------------------------------
      # Verified thresholds
      # ------------------------------------------------------
      
      threshold_data <- shiny::reactive({
        shiny::req(
          selected_season()
        )
        
        get_cap_thresholds(
          selected_season()
        )
      })
      
      base_payroll_total <- shiny::reactive({
        sum(
          numeric_or_zero(
            base_salary_data()$cap_hit
          ),
          na.rm = TRUE
        )
      })
      
      
      payroll_total <- shiny::reactive({
        
        current <- base_payroll_total()
        
        if (
          is.null(transaction_state) ||
          !exists(
            "tbi_apply_trade_scenario_to_payroll",
            mode = "function"
          )
        ) {
          return(current)
        }
        
        tbi_apply_trade_scenario_to_payroll(
          current_payroll = current,
          transaction_state = transaction_state,
          team_name = selected_team()
        )
      })
      
      active_trade_scenario <- shiny::reactive({
        
        if (
          is.null(transaction_state) ||
          is.null(transaction_state$snapshot)
        ) {
          return(NULL)
        }
        
        scenario <- transaction_state$snapshot()
        
        if (
          !isTRUE(scenario$active) ||
          !identical(
            as.character(
              scenario$scenario_type
            ),
            "trade"
          ) ||
          !identical(
            as.character(
              scenario$season
            ),
            as.character(
              selected_season()
            )
          )
        ) {
          return(NULL)
        }
        
        team <- as.character(
          selected_team()
        )
        
        if (
          !team %in%
          c(
            as.character(
              scenario$team
            ),
            as.character(
              scenario$partner_team
            )
          )
        ) {
          return(NULL)
        }
        
        scenario
      })
      
      
      scenario_salary_delta <- shiny::reactive({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(0)
        }
        
        if (
          identical(
            as.character(
              selected_team()
            ),
            as.character(
              scenario$team
            )
          )
        ) {
          return(
            suppressWarnings(
              as.numeric(
                scenario$salary_delta %||% 0
              )
            )
          )
        }
        
        -suppressWarnings(
          as.numeric(
            scenario$salary_delta %||% 0
          )
        )
      })
      
      
      output$scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        delta <- scenario_salary_delta()
        
        delta_text <- if (
          is.na(delta) ||
          abs(delta) < 1
        ) {
          "No payroll change"
        } else if (delta > 0) {
          paste0(
            "+",
            money(delta),
            " projected payroll"
          )
        } else {
          paste0(
            "-",
            money(abs(delta)),
            " projected payroll"
          )
        }
        
        counterpart <- if (
          identical(
            as.character(
              selected_team()
            ),
            as.character(
              scenario$team
            )
          )
        ) {
          scenario$partner_team
        } else {
          scenario$team
        }
        
        shiny::div(
          class = "tbi-cap-scenario-banner",
          
          shiny::div(
            class = "tbi-cap-scenario-banner-copy",
            
            shiny::span(
              class = "tbi-cap-scenario-badge",
              bsicons::bs_icon(
                "arrow-left-right"
              ),
              "TRADE SCENARIO"
            ),
            
            shiny::span(
              class = "tbi-cap-scenario-text",
              paste0(
                "Previewing ",
                selected_team(),
                " ↔ ",
                counterpart,
                ". Cap outputs below reflect the shared Trade Intelligence scenario."
              )
            )
          ),
          
          shiny::span(
            class = "tbi-cap-scenario-delta",
            delta_text
          )
        )
      })
      
      
      team_cap_status <- shiny::reactive({
        th <- threshold_data()
        payroll <- payroll_total()
        
        if (!nrow(th)) {
          return("Thresholds unavailable")
        }
        
        cap <- as.numeric(
          th$salary_cap[[1]]
        )
        
        tax <- as.numeric(
          th$luxury_tax[[1]]
        )
        
        first <- as.numeric(
          th$first_apron[[1]]
        )
        
        second <- as.numeric(
          th$second_apron[[1]]
        )
        
        if (payroll > second) {
          "Above Second Apron"
        } else if (payroll > first) {
          "Above First Apron"
        } else if (payroll > tax) {
          "Tax Team"
        } else if (payroll > cap) {
          "Over Cap"
        } else {
          "Below Cap"
        }
      })
      
      flexibility_status <- shiny::reactive({
        status <- team_cap_status()
        
        switch(
          status,
          "Above Second Apron" = "Severely Restricted",
          "Above First Apron" = "Restricted",
          "Tax Team" = "Limited",
          "Over Cap" = "Managed",
          "Below Cap" = "Flexible",
          "Review Required"
        )
      })
      
      # ------------------------------------------------------
      # Trade exception inventory
      # ------------------------------------------------------
      
      active_tpe_count <- shiny::reactive({
        con <- tryCatch(
          connect_db(),
          error = function(e) NULL
        )
        
        if (is.null(con)) {
          return(NA_integer_)
        }
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        tables <- DBI::dbListTables(
          con
        )
        
        candidate <- intersect(
          c(
            "trade_exceptions",
            "tpe_ledger",
            "team_trade_exceptions"
          ),
          tables
        )
        
        if (!length(candidate)) {
          return(NA_integer_)
        }
        
        table_name <- candidate[[1]]
        fields <- DBI::dbListFields(
          con,
          table_name
        )
        
        teams <- get_teams()
        
        team_row <- teams[
          teams$team_name == selected_team(),
          ,
          drop = FALSE
        ]
        
        if (!nrow(team_row)) {
          return(NA_integer_)
        }
        
        rows <- NULL
        
        if ("team_id" %in% fields) {
          rows <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT * FROM ",
              table_name,
              " WHERE team_id = ?"
            ),
            params = list(
              team_row$team_id[[1]]
            )
          )
        } else if ("team_name" %in% fields) {
          rows <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT * FROM ",
              table_name,
              " WHERE team_name = ?"
            ),
            params = list(
              selected_team()
            )
          )
        }
        
        if (is.null(rows)) {
          return(NA_integer_)
        }
        
        if (!nrow(rows)) {
          return(0L)
        }
        
        if ("status" %in% names(rows)) {
          rows <- rows[
            !tolower(
              as.character(
                rows$status
              )
            ) %in% c(
              "used",
              "expired",
              "archived"
            ),
            ,
            drop = FALSE
          ]
        }
        
        if ("remaining_amount" %in% names(rows)) {
          rows <- rows[
            numeric_or_zero(
              rows$remaining_amount
            ) > 0,
            ,
            drop = FALSE
          ]
        }
        
        nrow(rows)
      })

      visible_tpe_count <- shiny::reactive({
        if (!v2_trade_test_mode_enabled()) return(NA_integer_)
        active_tpe_count()
      })
      
      # ------------------------------------------------------
      # Executive snapshot outputs
      # ------------------------------------------------------
      
      output$team_payroll <- shiny::renderText({
        money(
          payroll_total()
        )
      })
      
      output$cap_status <- shiny::renderText({
        team_cap_status()
      })
      
      output$cap_room <- shiny::renderText({
        th <- threshold_data()
        
        if (!nrow(th)) {
          return("—")
        }
        
        difference <-
          as.numeric(
            th$salary_cap[[1]]
          ) -
          payroll_total()
        
        if (difference >= 0) {
          paste(
            money(difference),
            "available"
          )
        } else {
          paste(
            money(abs(difference)),
            "over"
          )
        }
      })
      
      output$second_apron_room <- shiny::renderText({
        th <- threshold_data()
        
        if (!nrow(th)) {
          return("—")
        }
        
        difference <-
          as.numeric(
            th$second_apron[[1]]
          ) -
          payroll_total()
        
        if (difference >= 0) {
          paste(
            money(difference),
            "below"
          )
        } else {
          paste(
            money(abs(difference)),
            "over"
          )
        }
      })
      
      output$active_tpes <- shiny::renderText({
        cap_tpe_display_value(visible_tpe_count())
      })
      
      output$contract_count <- shiny::renderText({
        nrow(
          salary_data()
        )
      })
      
      output$payroll_pct_cap <- shiny::renderText({
        th <- threshold_data()
        
        if (!nrow(th)) {
          return("—")
        }
        
        pct_of(
          payroll_total(),
          th$salary_cap[[1]]
        )
      })
      
      # ------------------------------------------------------
      # Financial operating decision
      # ------------------------------------------------------
      
      output$financial_decision <- shiny::renderUI({
        status <- team_cap_status()
        flexibility <- flexibility_status()
        state <- cap_financial_decision_state(status)
        decision <- state$decision
        explanation <- state$explanation
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  identical(
                    decision,
                    "FLEXIBLE"
                  )
                ) {
                  "bullseye"
                } else {
                  "exclamation-triangle"
                }
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-copy",
              
              shiny::strong(
                class = "tbi-v2-decision-word",
                decision
              ),
              
              shiny::p(
                explanation
              )
            )
          ),
          
          shiny::div(
            class = "tbi-v2-decision-metrics",
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              
              shiny::span(
                "OPERATING BAND"
              ),
              
              shiny::strong(
                status
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              
              shiny::span(
                "FLEXIBILITY"
              ),
              
              shiny::strong(
                flexibility
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Cap threshold scorecard
      # ------------------------------------------------------
      
      output$threshold_scorecard <- shiny::renderUI({
        th <- threshold_data()
        
        if (!nrow(th)) {
          return(
            shiny::div(
              class = "tbi-v2-decision-empty",
              "Verified cap thresholds are not loaded for this season."
            )
          )
        }
        
        payroll <- payroll_total()
        
        rows <- list(
          list(
            label = "Salary Cap",
            subtitle = "Standard team salary cap",
            threshold = as.numeric(
              th$salary_cap[[1]]
            ),
            icon = "currency-dollar"
          ),
          list(
            label = "Luxury Tax",
            subtitle = "Tax threshold",
            threshold = as.numeric(
              th$luxury_tax[[1]]
            ),
            icon = "cash-stack"
          ),
          list(
            label = "First Apron",
            subtitle = "First apron threshold",
            threshold = as.numeric(
              th$first_apron[[1]]
            ),
            icon = "exclamation-triangle"
          ),
          list(
            label = "Second Apron",
            subtitle = "Second apron threshold",
            threshold = as.numeric(
              th$second_apron[[1]]
            ),
            icon = "exclamation-triangle"
          )
        )
        
        shiny::tagList(
          lapply(
            rows,
            function(row) {
              
              percent <- if (
                row$threshold > 0
              ) {
                100 * payroll /
                  row$threshold
              } else {
                0
              }
              
              meter <- max(
                0,
                min(
                  100,
                  percent
                )
              )
              
              over <- payroll >
                row$threshold
              
              tone <- if (over) {
                "orange"
              } else if (
                percent >= 95
              ) {
                "blue"
              } else {
                "green"
              }
              
              difference <-
                row$threshold -
                payroll
              
              shiny::div(
                class = paste(
                  "tbi-v2-score-row",
                  paste0(
                    "tbi-v2-score-",
                    tone
                  )
                ),
                
                shiny::div(
                  class = "tbi-v2-score-name",
                  
                  shiny::span(
                    class = "tbi-v2-score-icon",
                    bsicons::bs_icon(
                      row$icon
                    )
                  ),
                  
                  shiny::div(
                    shiny::strong(
                      if (
                        exists(
                          "tbi_cba_link",
                          mode = "function"
                        )
                      ) {
                        tbi_cba_link(
                          term = row$label,
                          label = row$label,
                          class = "tbi-cap-threshold-cba-link"
                        )
                      } else {
                        shiny::span(
                          row$label
                        )
                      }
                    ),
                    shiny::tags$small(
                      paste(
                        row$subtitle,
                        "•",
                        money(
                          row$threshold
                        )
                      )
                    )
                  )
                ),
                
                shiny::div(
                  class = "tbi-v2-score-meter",
                  
                  shiny::div(
                    class = "tbi-v2-score-track",
                    
                    shiny::div(
                      class = "tbi-v2-score-fill",
                      style = paste0(
                        "width:",
                        meter,
                        "%;"
                      )
                    )
                  )
                ),
                
                shiny::strong(
                  class = "tbi-v2-score-number",
                  sprintf(
                    "%.0f%%",
                    percent
                  )
                ),
                
                shiny::span(
                  class = "tbi-v2-score-rating",
                  if (over) {
                    paste(
                      money(
                        abs(
                          difference
                        )
                      ),
                      "over"
                    )
                  } else {
                    paste(
                      money(
                        difference
                      ),
                      "below"
                    )
                  }
                )
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Alerts
      # ------------------------------------------------------
      
      output$cba_alerts <- shiny::renderUI({
        status <- team_cap_status()
        th <- threshold_data()
        alerts <- character()
        
        if (
          identical(
            status,
            "Above Second Apron"
          )
        ) {
          alerts <- c(
            alerts,
            "Second-apron transaction restrictions require review.",
            "Salary aggregation may be restricted in trade scenarios.",
            "Incoming salary flexibility is materially constrained."
          )
        } else if (
          identical(
            status,
            "Above First Apron"
          )
        ) {
          alerts <- c(
            alerts,
            "First-apron restrictions are active.",
            "Hard-cap and exception consequences require transaction-level review."
          )
        } else if (
          identical(
            status,
            "Tax Team"
          )
        ) {
          alerts <- c(
            alerts,
            "Luxury-tax exposure is active.",
            "Tax-team salary matching rules apply in transaction screening."
          )
        } else {
          alerts <- c(
            alerts,
            "No apron restriction is currently triggered by team payroll."
          )
        }
        
        tpes <- visible_tpe_count()
        
        if (!is.na(tpes)) {
          alerts <- c(
            alerts,
            paste0(
              tpes,
              " active trade exception",
              if (tpes == 1) "" else "s",
              " currently recorded."
            )
          )
        }
        
        shiny::tagList(
          lapply(
            alerts,
            function(alert) {
              shiny::div(
                class = "tbi-v2-headline-row",
                shiny::span(
                  class = "tbi-v2-headline-dot tbi-v2-headline-dot-orange"
                ),
                shiny::span(
                  alert
                )
              )
            }
          )
        )
      })
      
      output$financial_risks <- shiny::renderUI({
        d <- salary_data()
        total <- payroll_total()
        
        top3 <- if (nrow(d)) {
          sum(
            utils::head(
              numeric_or_zero(
                d$cap_hit
              ),
              3
            ),
            na.rm = TRUE
          )
        } else {
          0
        }
        
        concentration <- if (
          total > 0
        ) {
          top3 / total
        } else {
          0
        }
        
        expiring <- sum(
          !is.na(
            d$free_agent_year
          ) &
            suppressWarnings(
              as.numeric(
                d$free_agent_year
              )
            ) <= 2028,
          na.rm = TRUE
        )
        
        risks <- character()
        
        if (
          team_cap_status() %in%
          c(
            "Above First Apron",
            "Above Second Apron"
          )
        ) {
          risks <- c(
            risks,
            paste(
              team_cap_status(),
              "reduces roster-building flexibility."
            )
          )
        }
        
        if (concentration >= .50) {
          risks <- c(
            risks,
            sprintf(
              "Top-three contracts account for %.1f%% of current payroll.",
              100 * concentration
            )
          )
        }
        
        if (expiring >= 3) {
          risks <- c(
            risks,
            paste0(
              expiring,
              " contracts reach free agency by 2028."
            )
          )
        }
        
        if (!length(risks)) {
          risks <- "No major structural cap risk is currently identified."
        }
        
        shiny::tagList(
          lapply(
            risks,
            function(risk) {
              shiny::div(
                class = "tbi-v2-risk-card",
                
                shiny::span(
                  class = "tbi-v2-risk-icon",
                  bsicons::bs_icon(
                    "exclamation-triangle"
                  )
                ),
                
                shiny::div(
                  shiny::strong(
                    "Risk"
                  ),
                  shiny::p(
                    risk
                  )
                )
              )
            }
          )
        )
      })
      
      output$financial_opportunities <- shiny::renderUI({
        d <- salary_data()
        th <- threshold_data()
        opportunities <- character()
        
        if (
          nrow(th) &&
          payroll_total() <
          as.numeric(
            th$first_apron[[1]]
          )
        ) {
          opportunities <- c(
            opportunities,
            "Payroll remains below the first apron, preserving additional transaction flexibility."
          )
        }
        
        tpes <- visible_tpe_count()
        
        if (
          !is.na(tpes) &&
          tpes > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              tpes,
              " active trade exception",
              if (tpes == 1) "" else "s",
              " can be evaluated as acquisition tools."
            )
          )
        }
        
        expiring <- sum(
          !is.na(
            d$free_agent_year
          ) &
            suppressWarnings(
              as.numeric(
                d$free_agent_year
              )
            ) <= 2028,
          na.rm = TRUE
        )
        
        if (expiring > 0) {
          opportunities <- c(
            opportunities,
            paste0(
              expiring,
              " near-term contract expiration",
              if (expiring == 1) "" else "s",
              " may create future flexibility."
            )
          )
        }
        
        if (!length(opportunities)) {
          opportunities <- paste(
            "Preserve optionality and evaluate lower-cost paths",
            "to improve the roster."
          )
        }
        
        shiny::tagList(
          lapply(
            opportunities,
            function(opportunity) {
              shiny::div(
                class = "tbi-v2-opportunity-card",
                
                shiny::span(
                  class = "tbi-v2-opportunity-icon",
                  bsicons::bs_icon(
                    "graph-up-arrow"
                  )
                ),
                
                shiny::div(
                  shiny::strong(
                    "Opportunity"
                  ),
                  shiny::p(
                    opportunity
                  )
                )
              )
            }
          )
        )
      })

      cap_overview_rows <- function(rows) {
        shiny::tagList(lapply(rows, function(row) {
          shiny::div(
            class = "cap-v2-overview-row",
            shiny::span(row[[1L]]),
            shiny::strong(row[[2L]])
          )
        }))
      }

      output$overview_position <- shiny::renderUI({
        th <- threshold_data()
        tax_position <- if (nrow(th)) {
          difference <- as.numeric(th$luxury_tax[[1]]) - payroll_total()
          paste(money(abs(difference)), if (difference >= 0) "below tax" else "over tax")
        } else "UNKNOWN"
        cap_overview_rows(list(
          c("Payroll", money(payroll_total())),
          c("Operating band", team_cap_status()),
          c("Tax position", tax_position)
        ))
      })

      output$overview_flexibility <- shiny::renderUI({
        th <- threshold_data()
        first <- if (nrow(th)) {
          difference <- as.numeric(th$first_apron[[1]]) - payroll_total()
          paste(money(abs(difference)), if (difference >= 0) "below first apron" else "over first apron")
        } else "UNKNOWN"
        tpes <- visible_tpe_count()
        cap_overview_rows(list(
          c("Flexibility", flexibility_status()),
          c("First-apron distance", first),
          c("Trade exceptions", cap_tpe_display_value(tpes))
        ))
      })

      output$overview_watch <- shiny::renderUI({
        d <- salary_data()
        years <- suppressWarnings(as.integer(d$free_agent_year))
        next_year <- if (any(!is.na(years))) min(years, na.rm = TRUE) else NA_integer_
        next_count <- if (is.na(next_year)) 0L else sum(years == next_year, na.rm = TRUE)
        cap_overview_rows(list(
          c("Operating posture", cap_recommendation_decision()),
          c("Next contract window", if (is.na(next_year)) "UNKNOWN" else paste(next_year, paste0("(", next_count, ")"))),
          c("Current contracts", as.character(nrow(d)))
        ))
      })

      output$market_summary <- shiny::renderUI({
        filtered <- filtered_market()
        loaded <- sum(filtered$loaded_window_fields, na.rm = TRUE)
        review <- sum(filtered$market_window_status != "CURRENT CONTRACT WINDOW", na.rm = TRUE)
        shiny::tagList(
          shiny::strong(paste(nrow(filtered), "loaded market rows")),
          shiny::span(paste(loaded, "rows with all loaded window fields")),
          shiny::span(paste(review, "rows require review"))
        )
      })

      output$market_table <- reactable::renderReactable({
        d <- filtered_market()
        shiny::validate(shiny::need(nrow(d) > 0, "No loaded free-agent rows match the active filters."))
        money_or_unknown <- function(value) {
          value <- suppressWarnings(as.numeric(value))
          if (length(value) != 1L || is.na(value)) "UNKNOWN" else money(value)
        }
        display <- data.frame(
          Player = d$player_name,
          Position = ifelse(is.na(d$primary_position), "UNKNOWN", d$primary_position),
          Age = d$player_age,
          `Current Team` = d$team_name,
          Salary = d$cap_hit,
          Contract = ifelse(is.na(d$contract_type), "UNKNOWN", d$contract_type),
          `FA Year` = d$free_agent_year,
          `FA Type` = ifelse(is.na(d$free_agent_status), "UNKNOWN", d$free_agent_status),
          Option = ifelse(is.na(d$option_type), "NONE LOADED", d$option_type),
          BIE = d$bie,
          `BIE Grade` = d$bie_grade,
          `BIE Season` = ifelse(is.na(d$bie_season), "UNKNOWN", d$bie_season),
          `BIE Confidence` = ifelse(is.na(d$bie_confidence), "UNKNOWN", d$bie_confidence),
          `Free Agency Status` = cap_market_display_status(d$market_window_status),
          `Verification Status` = cap_market_display_status(d$data_quality),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        quality_token <- function(value) {
          tone <- if (identical(value, "CURRENT / VERIFIED")) "current" else "review"
          shiny::span(class = paste("cap-v2-status-token", paste0("cap-v2-status-token-", tone)), value)
        }
        reactable::reactable(
          display,
          searchable = FALSE,
          pagination = TRUE,
          defaultPageSize = 12,
          compact = TRUE,
          highlight = TRUE,
          sortable = TRUE,
          theme = reactable::reactableTheme(
            backgroundColor = "transparent",
            color = "#e6edf6",
            borderColor = "rgba(148,163,184,.10)",
            headerStyle = list(backgroundColor = "#111824", color = "#9cabbc", fontWeight = 800),
            rowHighlightStyle = list(backgroundColor = "rgba(59,130,246,.045)")
          ),
          columns = list(
            Player = reactable::colDef(minWidth = 140),
            Position = reactable::colDef(minWidth = 76),
            Age = reactable::colDef(format = reactable::colFormat(digits = 0), width = 58),
            `Current Team` = reactable::colDef(minWidth = 145),
            Salary = reactable::colDef(cell = money_or_unknown, minWidth = 100, sortNALast = TRUE),
            Contract = reactable::colDef(minWidth = 130),
            `FA Year` = reactable::colDef(width = 75),
            `FA Type` = reactable::colDef(minWidth = 95),
            Option = reactable::colDef(minWidth = 115),
            BIE = reactable::colDef(format = reactable::colFormat(digits = 1), width = 72, sortNALast = TRUE),
            `BIE Grade` = reactable::colDef(minWidth = 88),
            `BIE Season` = reactable::colDef(minWidth = 95),
            `BIE Confidence` = reactable::colDef(minWidth = 115),
            `Free Agency Status` = reactable::colDef(cell = quality_token, minWidth = 150),
            `Verification Status` = reactable::colDef(cell = quality_token, minWidth = 170)
          )
        )
      })

      cap_recommendation_decision <- shiny::reactive({
        cap_financial_decision_state(team_cap_status())$decision
      })

      cap_recommendation_row <- function(label, value, evidence = "FACT") {
        shiny::div(
          class = "cap-v2-recommendation-row",
          shiny::span(label),
          shiny::strong(value),
          shiny::tags$small(evidence)
        )
      }

      output$cap_recommendation_posture <- shiny::renderUI({
        shiny::tagList(
          shiny::strong(class = "cap-v2-recommendation-decision", cap_recommendation_decision()),
          shiny::p(paste(selected_team(), "is operating in the", team_cap_status(), "band with", tolower(flexibility_status()), "flexibility."))
        )
      })

      output$cap_recommendation_money <- shiny::renderUI({
        th <- threshold_data()
        apron_distance <- if (nrow(th)) {
          difference <- as.numeric(th$first_apron[[1]]) - payroll_total()
          paste(money(abs(difference)), if (difference >= 0) "below first apron" else "over first apron")
        } else {
          "UNKNOWN"
        }
        shiny::tagList(
          cap_recommendation_row("Team payroll", money(payroll_total())),
          cap_recommendation_row("Operating band", team_cap_status(), "RULE RESULT"),
          cap_recommendation_row("Cap room / overage", {
            if (!nrow(th)) "UNKNOWN" else {
              difference <- as.numeric(th$salary_cap[[1]]) - payroll_total()
              paste(money(abs(difference)), if (difference >= 0) "available" else "over cap")
            }
          }, "RULE RESULT"),
          cap_recommendation_row("First-apron distance", apron_distance, "RULE RESULT"),
          cap_recommendation_row("Flexibility", flexibility_status(), "RULE RESULT")
        )
      })

      output$cap_recommendation_watch <- shiny::renderUI({
        d <- salary_data()
        market <- market_data()
        year <- cap_market_default_year(market$free_agent_year, selected_season())
        window <- market[as.character(market$free_agent_year) == year, , drop = FALSE]
        loaded <- sum(window$loaded_window_fields, na.rm = TRUE)
        review <- sum(window$market_window_status != "CURRENT CONTRACT WINDOW", na.rm = TRUE)
        cap_hits <- sort(numeric_or_zero(d$cap_hit), decreasing = TRUE)
        total <- sum(cap_hits, na.rm = TRUE)
        top3 <- if (total > 0) 100 * sum(utils::head(cap_hits, 3), na.rm = TRUE) / total else 0
        next_action <- if (team_cap_status() %in% c("Above First Apron", "Above Second Apron")) {
          "Run transaction-level CBA review before committing salary."
        } else {
          "Sequence contract decisions against the next loaded free-agent window."
        }
        shiny::tagList(
          cap_recommendation_row("Contract concentration", sprintf("Top three: %.1f%% of payroll", top3)),
          cap_recommendation_row("Free-agent window", paste(year, "—", loaded, "rows with all loaded fields,", review, "require review")),
          cap_recommendation_row("Primary risk", if (team_cap_status() %in% c("Above First Apron", "Above Second Apron")) paste(team_cap_status(), "restricts flexibility") else "Contract concentration and timing require monitoring", "RULE RESULT"),
          cap_recommendation_row("Primary opportunity", if (flexibility_status() == "Flexible") "Loaded cap room supports roster-building optionality" else "Exception and contract optionality require source verification", "RULE RESULT"),
          cap_recommendation_row("Next action", next_action, "DECISION SUPPORT")
        )
      })

      # ------------------------------------------------------
      # Contract ledger
      # ------------------------------------------------------
      
      output$salary_table <- reactable::renderReactable({
        d <- salary_data()
        
        shiny::validate(
          shiny::need(
            nrow(d) > 0,
            "No contract-year records are available for this team and season."
          )
        )
        
        th <- threshold_data()
        
        cap_value <- if (
          nrow(th)
        ) {
          th$salary_cap[[1]]
        } else {
          NA_real_
        }
        
        first_value <- if (
          nrow(th)
        ) {
          th$first_apron[[1]]
        } else {
          NA_real_
        }
        
        guaranteed <- numeric_or_zero(
          d$guaranteed_amount
        )
        
        cap_hit <- numeric_or_zero(
          d$cap_hit
        )
        
        display <- data.frame(
          Player = d$player_name,
          Position = ifelse(
            is.na(
              d$primary_position
            ),
            "—",
            d$primary_position
          ),
          `Cap Hit` = vapply(
            cap_hit,
            money,
            character(1)
          ),
          Guaranteed = vapply(
            guaranteed,
            money,
            character(1)
          ),
          `% of Cap` = if (
            !is.na(
              cap_value
            )
          ) {
            vapply(
              cap_hit,
              pct_of,
              character(1),
              threshold = cap_value
            )
          } else {
            "—"
          },
          `% of 1st Apron` = if (
            !is.na(
              first_value
            )
          ) {
            vapply(
              cap_hit,
              pct_of,
              character(1),
              threshold = first_value
            )
          } else {
            "—"
          },
          Contract = ifelse(
            is.na(
              d$contract_type
            ),
            "Not classified",
            d$contract_type
          ),
          `Contract Through` = ifelse(
            is.na(
              d$contract_end_season
            ),
            "—",
            d$contract_end_season
          ),
          `FA Year` = ifelse(
            is.na(
              d$free_agent_year
            ),
            "—",
            d$free_agent_year
          ),
          `Selected-year Option` = ifelse(
            is.na(d$option_type) | !nzchar(trimws(as.character(d$option_type))),
            "UNKNOWN",
            d$option_type
          ),
          `Data Quality` = roster_contract_quality_status(d),
          check.names = FALSE
        )
        
        reactable::reactable(
          display,
          searchable = TRUE,
          striped = FALSE,
          highlight = TRUE,
          pagination = TRUE,
          defaultPageSize = 10,
          compact = TRUE,
          theme = reactable::reactableTheme(
            backgroundColor = "transparent",
            color = "#e6edf6",
            borderColor = "rgba(148,163,184,.10)",
            headerStyle = list(
              backgroundColor = "#111824",
              color = "#7f8da0",
              fontWeight = 800
            ),
            rowHighlightStyle = list(
              backgroundColor = "rgba(59,130,246,.045)"
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Front-office readout
      # ------------------------------------------------------
      
      output$cap_signals <- shiny::renderUI({
        d <- salary_data()
        
        cap_hit <- numeric_or_zero(
          d$cap_hit
        )
        
        guaranteed <- numeric_or_zero(
          d$guaranteed_amount
        )
        
        total <- sum(
          cap_hit,
          na.rm = TRUE
        )
        
        guaranteed_total <- sum(
          pmin(
            guaranteed,
            cap_hit
          ),
          na.rm = TRUE
        )
        
        non_guaranteed <- max(
          0,
          total -
            guaranteed_total
        )
        
        top3 <- if (
          length(
            cap_hit
          )
        ) {
          sum(
            utils::head(
              cap_hit,
              3
            ),
            na.rm = TRUE
          )
        } else {
          0
        }
        
        top3_share <- if (
          total > 0
        ) {
          100 * top3 / total
        } else {
          0
        }
        
        signal_row <- function(
    label,
    value) {
          shiny::div(
            class = "tbi-signal-row",
            shiny::span(
              label
            ),
            shiny::strong(
              value
            )
          )
        }
        
        shiny::tagList(
          signal_row(
            "Operating band",
            team_cap_status()
          ),
          signal_row(
            "Flexibility",
            flexibility_status()
          ),
          signal_row(
            "Guaranteed salary",
            money(
              guaranteed_total
            )
          ),
          signal_row(
            "Non-guaranteed exposure",
            money(
              non_guaranteed
            )
          ),
          signal_row(
            "Top-three concentration",
            sprintf(
              "%.1f%%",
              top3_share
            )
          ),
          signal_row(
            "Trade exceptions",
            {
              cap_tpe_display_value(visible_tpe_count())
            }
          ),
          shiny::div(
            class = "tbi-v2-cap-scope-note",
            shiny::tags$small(
              paste(
                "Cap Intelligence is a decision-support view.",
                "Transaction-level legality remains subject to the",
                "applicable CBA rule screen."
              )
            )
          )
        )
      })
    }
  )
}
