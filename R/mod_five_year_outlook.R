# ------------------------------------------------------------
# Module: Five-Year Outlook
# Version 2.1 Executive Long-Range Planning Workspace
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Five-Year Outlook UI
#'
#' @param id Internal module ID.
#' @noRd
mod_five_year_outlook_ui <- function(id) {
  ns <- shiny::NS(id)
  
  snapshot_item <- function(
    label,
    output_id,
    icon,
    tone = "blue",
    cba_term = NULL) {
    
    rendered_label <- if (
      !is.null(cba_term) &&
      nzchar(as.character(cba_term)) &&
      exists("tbi_cba_link", mode = "function")
    ) {
      tbi_cba_link(
        term = cba_term,
        label = label,
        class = "outlook-cba-link"
      )
    } else {
      shiny::span(
        class = "tbi-v2-snapshot-label",
        label
      )
    }
    
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
        rendered_label,
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
    class = "tbi-module-page tbi-v2-outlook-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-v2-outlook-page {
          display:grid;
          gap:12px;
        }

        .outlook-v2-five-grid {
          display:grid;
          grid-template-columns:repeat(5,minmax(0,1fr));
          gap:8px;
          padding:14px;
        }

        .outlook-v2-year-card {
          position:relative;
          min-height:158px;
          padding:13px;
          overflow:hidden;
          border:1px solid rgba(148,163,184,.11);
          border-radius:10px;
          background:
            linear-gradient(145deg,rgba(19,28,42,.96),rgba(12,18,28,.98));
        }

        .outlook-v2-year-card.current {
          border-color:rgba(96,165,250,.34);
          box-shadow:inset 0 3px 0 rgba(96,165,250,.72);
        }

        .outlook-v2-year-label {
          color:#718198;
          font-size:.53rem;
          font-weight:850;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .outlook-v2-season {
          margin:4px 0 12px;
          color:#f4f7fb;
          font-size:1rem;
          font-weight:820;
        }

        .outlook-v2-year-metric {
          min-height:31px;
          padding:5px 0;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:8px;
          border-bottom:1px solid rgba(148,163,184,.075);
          color:#8594a8;
          font-size:.57rem;
        }

        .outlook-v2-year-metric:last-child {
          border-bottom:0;
        }

        .outlook-v2-year-metric strong {
          color:#edf3f8;
          font-size:.62rem;
        }

        .outlook-v2-bar {
          height:6px;
          margin-top:12px;
          overflow:hidden;
          border-radius:99px;
          background:#1c2938;
        }

        .outlook-v2-bar-fill {
          height:100%;
          border-radius:inherit;
          background:#4b9cff;
        }

        .outlook-v2-planning-grid {
          display:grid;
          grid-template-columns:minmax(0,1.15fr) minmax(300px,.85fr);
          gap:12px;
        }

        .outlook-v2-table-wrap {
          max-height:430px;
          overflow:auto;
        }

        .outlook-v2-signal-row {
          min-height:40px;
          padding:7px 0;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          border-bottom:1px solid rgba(148,163,184,.08);
          color:#8493a7;
          font-size:.63rem;
        }

        .outlook-v2-signal-row:last-child {
          border-bottom:0;
        }

        .outlook-v2-signal-row strong {
          color:#eef3f8;
          text-align:right;
        }

        .outlook-v2-rec-grid {
          padding:16px;
          display:grid;
          grid-template-columns:minmax(220px,.42fr) minmax(0,1.58fr);
          gap:18px;
          align-items:center;
        }

        .outlook-v2-rec-box {
          min-height:110px;
          padding:16px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:rgba(59,130,246,.045);
        }

        .outlook-v2-rec-box span {
          color:#718198;
          font-size:.52rem;
          font-weight:850;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .outlook-v2-rec-box strong {
          margin-top:6px;
          color:#61a8ff;
          font-size:1.25rem;
          line-height:1.15;
        }

        .outlook-v2-note {
          color:#77879b;
          font-size:.59rem;
          line-height:1.5;
        }

        .outlook-v2-trade-banner {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          padding:11px 14px;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:
            linear-gradient(
              90deg,
              rgba(59,130,246,.10),
              rgba(59,130,246,.03)
            );
        }

        .outlook-v2-trade-copy {
          min-width:0;
          color:#a9b8ca;
          font-size:.61rem;
          line-height:1.45;
        }

        .outlook-v2-trade-chip {
          display:inline-flex;
          align-items:center;
          gap:6px;
          margin-right:8px;
          padding:5px 8px;
          border-radius:999px;
          background:rgba(59,130,246,.12);
          color:#72adff;
          font-size:.52rem;
          font-weight:900;
          letter-spacing:.08em;
          white-space:nowrap;
        }

        .outlook-v2-trade-delta {
          color:#f4f8fc;
          font-size:.64rem;
          font-weight:850;
          white-space:nowrap;
        }

        .outlook-v2-trade-delta.positive {
          color:#34d399;
        }

        .outlook-v2-trade-delta.warning {
          color:#fbbf24;
        }

        .outlook-cba-link {
          color:#72adff !important;
          font-weight:800 !important;
          text-decoration:none !important;
        }

        .outlook-cba-link::after {
          content:'  ↗';
          color:#5f9fee;
          font-size:.66em;
          opacity:.78;
        }

        .outlook-cba-link:hover {
          color:#a8ceff !important;
          text-decoration:underline !important;
          text-underline-offset:2px;
        }


        @media(max-width:1100px) {
          .outlook-v2-five-grid {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }

          .outlook-v2-planning-grid {
            grid-template-columns:1fr;
          }
        }

        @media(max-width:680px) {
          .outlook-v2-five-grid,
          .outlook-v2-rec-grid {
            grid-template-columns:1fr;
          }
        }
        "
      )
    ),
    
    # --------------------------------------------------------
    # Page identity
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-module-intro",
      
      shiny::div(
        shiny::div(
          class = "tbi-page-eyebrow",
          "LONG-RANGE STRATEGY"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Five-Year Outlook"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Track committed salary, contract control, free-agency turnover,",
            "draft optionality, and long-range roster flexibility."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "PLANNING HORIZON"
        ),
        shiny::strong("5 YEARS")
      )
    ),
    
    shiny::uiOutput(
      ns("outlook_trade_scenario_banner")
    ),
    
    # --------------------------------------------------------
    # Snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot",
      `data-tbi-outlook-section` = "long-range-snapshot",
      `data-tbi-outlook-tab` = "overview",
      
      shiny::div(
        class = "tbi-v2-section-title-row",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("calendar3")
          ),
          shiny::span("LONG-RANGE SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "PLANNING VIEW"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "CURRENT PAYROLL",
          "snapshot_current_payroll",
          "cash-stack",
          "blue",
          cba_term = "Team Salary"
        ),
        
        snapshot_item(
          "YEAR-3 COMMITTED",
          "snapshot_year3_payroll",
          "cash-stack",
          "blue",
          cba_term = "Team Salary"
        ),
        
        snapshot_item(
          "2028 FA / NEAR TERM",
          "snapshot_near_term_fa",
          "people",
          "orange",
          cba_term = "Unrestricted Free Agent (UFA)"
        ),
        
        snapshot_item(
          "TEAM OPTIONS",
          "snapshot_team_options",
          "person-badge",
          "green",
          cba_term = "Team Option"
        ),
        
        snapshot_item(
          "DRAFT VALUE",
          "snapshot_draft_value",
          "graph-up-arrow",
          "purple"
        ),
        
        snapshot_item(
          "FLEXIBILITY",
          "snapshot_flexibility",
          "bullseye",
          "green"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Decision + strategy scorecard
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-main-grid",
      `data-tbi-outlook-section` = "decision-and-scorecard",
      `data-tbi-outlook-tab` = "flexibility",
      
      shiny::tags$section(
        class = "tbi-v2-decision-card",
        
        shiny::div(
          class = "tbi-v2-section-title",
          
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-warning",
            bsicons::bs_icon("exclamation-triangle")
          ),
          
          shiny::span("LONG-RANGE DECISION")
        ),
        
        shiny::uiOutput(
          ns("outlook_decision")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-scorecard-panel",
        
        shiny::div(
          class = "tbi-v2-scorecard-header",
          
          shiny::div(
            class = "tbi-v2-section-title",
            shiny::span(
              class = "tbi-v2-section-icon",
              bsicons::bs_icon("graph-up-arrow")
            ),
            shiny::span("STRATEGIC FLEXIBILITY")
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Score"),
            shiny::strong(
              shiny::textOutput(
                ns("flexibility_score"),
                inline = TRUE
              )
            ),
            shiny::span("/ 100")
          )
        ),
        
        shiny::uiOutput(
          ns("strategy_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Five-year timeline
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      `data-tbi-outlook-section` = "organizational-timeline",
      `data-tbi-outlook-tab` = "timeline",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "ORGANIZATIONAL TIMELINE"
          ),
          shiny::h3("Five-year contract and roster control")
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "LOADED COMMITMENTS"
        )
      ),
      
      shiny::uiOutput(
        ns("five_year_timeline")
      )
    ),
    
    # --------------------------------------------------------
    # Headlines / risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = paste(
        "tbi-v2-exec-bottom-grid",
        "tbi-outlook-tab-layout",
        "tbi-outlook-signals-layout"
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-headlines-panel",
        `data-tbi-outlook-section` = "outlook-headlines",
        `data-tbi-outlook-tab` = "overview",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("calendar3")
          ),
          shiny::span("OUTLOOK HEADLINES")
        ),
        
        shiny::uiOutput(
          ns("outlook_headlines")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-risks-panel",
        `data-tbi-outlook-section` = "long-range-risks",
        `data-tbi-outlook-tab` = "draft-optionality",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-danger",
            bsicons::bs_icon("exclamation-triangle")
          ),
          shiny::span("LONG-RANGE RISKS")
        ),
        
        shiny::uiOutput(
          ns("outlook_risks")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-opportunities-panel",
        `data-tbi-outlook-section` = "flexibility-opportunities",
        `data-tbi-outlook-tab` = "draft-optionality",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-success",
            bsicons::bs_icon("bullseye")
          ),
          shiny::span("FLEXIBILITY OPPORTUNITIES")
        ),
        
        shiny::uiOutput(
          ns("outlook_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Contract runway + strategic readout
    # --------------------------------------------------------
    
    shiny::div(
      class = paste(
        "outlook-v2-planning-grid",
        "tbi-outlook-tab-layout",
        "tbi-outlook-planning-layout"
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        `data-tbi-outlook-section` = "contract-runway",
        `data-tbi-outlook-tab` = "contracts-free-agency",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "CONTRACT RUNWAY"
            ),
            shiny::h3("Player control and free-agency timeline")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "ROSTER CONTROL"
          )
        ),
        
        shiny::div(
          class = "outlook-v2-table-wrap",
          reactable::reactableOutput(
            ns("contract_runway_table")
          )
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        `data-tbi-outlook-section` = "front-office-readout",
        `data-tbi-outlook-tab` = "recommendation",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "STRATEGY ENGINE"
            ),
            shiny::h3("Front-office readout")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "DECISION SUPPORT"
          )
        ),
        
        shiny::div(
          style = "padding:14px 16px;",
          shiny::uiOutput(
            ns("outlook_readout")
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Executive recommendation
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      `data-tbi-outlook-section` = "executive-recommendation",
      `data-tbi-outlook-tab` = "recommendation",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "EXECUTIVE RECOMMENDATION"
          ),
          shiny::h3("Recommended five-year posture")
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "LONG-RANGE PLAN"
        )
      ),
      
      shiny::uiOutput(
        ns("outlook_recommendation")
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Five-Year Outlook server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @noRd
mod_five_year_outlook_server <- function(
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
        value <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(value) ||
          is.na(value[[1]]) ||
          !is.finite(value[[1]])
        ) {
          return("—")
        }
        
        value <- value[[1]]
        
        if (abs(value) >= 1e9) {
          return(
            sprintf(
              "$%.2fB",
              value / 1e9
            )
          )
        }
        
        if (abs(value) >= 1e6) {
          return(
            sprintf(
              "$%.1fM",
              value / 1e6
            )
          )
        }
        
        if (abs(value) >= 1e3) {
          return(
            sprintf(
              "$%.0fK",
              value / 1e3
            )
          )
        }
        
        paste0(
          "$",
          format(
            round(value),
            big.mark = ",",
            scientific = FALSE
          )
        )
      }
      
      safe_num <- function(x, default = 0) {
        value <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(value) ||
          is.na(value[[1]]) ||
          !is.finite(value[[1]])
        ) {
          default
        } else {
          value[[1]]
        }
      }
      
      text_or <- function(x, fallback = "—") {
        if (
          is.null(x) ||
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(
            trimws(
              as.character(
                x[[1]]
              )
            )
          )
        ) {
          fallback
        } else {
          as.character(
            x[[1]]
          )
        }
      }
      
      score_clamp <- function(x) {
        max(
          0,
          min(
            100,
            safe_num(
              x,
              0
            )
          )
        )
      }
      
      season_start_year <- shiny::reactive({
        shiny::req(
          selected_season()
        )
        
        year <- suppressWarnings(
          as.integer(
            substr(
              selected_season(),
              1,
              4
            )
          )
        )
        
        if (is.na(year)) {
          as.integer(
            format(
              Sys.Date(),
              "%Y"
            )
          )
        } else {
          year
        }
      })
      
      season_label <- function(start_year) {
        paste0(
          start_year,
          "-",
          substr(
            as.character(
              start_year + 1L
            ),
            3,
            4
          )
        )
      }
      
      planning_seasons <- shiny::reactive({
        years <- season_start_year() +
          0:4
        
        data.frame(
          year = years,
          season = vapply(
            years,
            season_label,
            character(1)
          ),
          stringsAsFactors = FALSE
        )
      })
      
      signal_row <- function(
    label,
    value,
    cba_term = NULL) {
        
        rendered_label <- if (
          !is.null(cba_term) &&
          nzchar(as.character(cba_term)) &&
          exists("tbi_cba_link", mode = "function")
        ) {
          tbi_cba_link(
            term = cba_term,
            label = label,
            class = "outlook-cba-link"
          )
        } else {
          shiny::span(label)
        }
        
        shiny::div(
          class = "outlook-v2-signal-row",
          rendered_label,
          shiny::strong(value)
        )
      }
      
      score_row <- function(
    label,
    subtitle,
    icon,
    score,
    rating = NULL) {
        
        score <- score_clamp(
          score
        )
        
        tone <- if (
          score >= 75
        ) {
          "green"
        } else if (
          score >= 55
        ) {
          "blue"
        } else if (
          score >= 35
        ) {
          "orange"
        } else {
          "red"
        }
        
        rating <- rating %||%
          if (
            score >= 75
          ) {
            "Strong"
          } else if (
            score >= 55
          ) {
            "Stable"
          } else if (
            score >= 35
          ) {
            "Watch"
          } else {
            "Limited"
          }
        
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
              bsicons::bs_icon(icon)
            ),
            
            shiny::div(
              shiny::strong(label),
              shiny::tags$small(subtitle)
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
                  score,
                  "%;"
                )
              )
            )
          ),
          
          shiny::strong(
            class = "tbi-v2-score-number",
            round(score)
          ),
          
          shiny::span(
            class = "tbi-v2-score-rating",
            rating
          )
        )
      }
      
      # ------------------------------------------------------
      # Current + future contract data
      # ------------------------------------------------------
      
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
        
        selected <- as.character(
          selected_team()
        )
        
        primary_team <- as.character(
          scenario$team
        )
        
        partner_team_name <- as.character(
          scenario$partner_team
        )
        
        if (
          !selected %in%
          c(
            primary_team,
            partner_team_name
          )
        ) {
          return(NULL)
        }
        
        # Normalize the same pending transaction to the
        # organization currently being viewed.
        if (
          identical(
            selected,
            partner_team_name
          )
        ) {
          
          tmp_team <- scenario$team
          tmp_outgoing <- scenario$outgoing_players
          tmp_incoming <- scenario$incoming_players
          tmp_outgoing_salary <- scenario$outgoing_salary
          tmp_incoming_salary <- scenario$incoming_salary
          
          scenario$team <-
            scenario$partner_team
          
          scenario$partner_team <-
            tmp_team
          
          scenario$outgoing_players <-
            tmp_incoming
          
          scenario$incoming_players <-
            tmp_outgoing
          
          scenario$outgoing_salary <-
            tmp_incoming_salary
          
          scenario$incoming_salary <-
            tmp_outgoing_salary
          
          scenario$salary_delta <-
            scenario$incoming_salary -
            scenario$outgoing_salary
        }
        
        scenario
      })
      
      
      scenario_player_ids <- function(
    player_data) {
        
        if (
          is.null(player_data) ||
          !is.data.frame(player_data) ||
          !nrow(player_data) ||
          !"player_id" %in% names(player_data)
        ) {
          return(integer())
        }
        
        ids <- suppressWarnings(
          as.integer(
            player_data$player_id
          )
        )
        
        ids[
          !is.na(ids)
        ]
      }
      
      
      base_future_contracts <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        seasons <- planning_seasons()$season
        
        con <- connect_db()
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        placeholders <- paste(
          rep(
            "?",
            length(seasons)
          ),
          collapse = ","
        )
        
        query <- paste0(
          "
          SELECT
            p.player_id,
            p.player_name,
            p.primary_position,
            p.player_age,
            t.team_name,
            cy.season,
            cy.base_salary,
            cy.cap_hit,
            cy.guaranteed_amount,
            cy.option_type,
            cy.likely_incentives,
            cy.unlikely_incentives,
            cy.dead_cap,
            c.contract_type,
            c.contract_start_season,
            c.contract_end_season,
            c.total_value,
            c.guaranteed_value,
            c.free_agent_year,
            c.bird_rights
          FROM contract_years cy
          INNER JOIN players p
            ON p.player_id = cy.player_id
          INNER JOIN teams t
            ON t.team_id = cy.team_id
          LEFT JOIN contracts c
            ON c.contract_id = cy.contract_id
          WHERE t.team_name = ?
            AND cy.season IN (",
          placeholders,
          ")
          ORDER BY
            cy.season,
            COALESCE(cy.cap_hit,0) DESC,
            p.player_name
          "
        )
        
        DBI::dbGetQuery(
          con,
          query,
          params = c(
            list(
              selected_team()
            ),
            as.list(
              seasons
            )
          )
        )
      })
      
      future_contracts <- shiny::reactive({
        
        current <- base_future_contracts()
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(current)
        }
        
        outgoing_ids <- scenario_player_ids(
          scenario$outgoing_players
        )
        
        incoming_ids <- scenario_player_ids(
          scenario$incoming_players
        )
        
        preview <- current[
          !current$player_id %in%
            outgoing_ids,
          ,
          drop = FALSE
        ]
        
        if (length(incoming_ids)) {
          
          seasons <- planning_seasons()$season
          
          con <- connect_db()
          
          on.exit(
            disconnect_db(con),
            add = TRUE
          )
          
          season_placeholders <- paste(
            rep(
              "?",
              length(seasons)
            ),
            collapse = ","
          )
          
          player_placeholders <- paste(
            rep(
              "?",
              length(incoming_ids)
            ),
            collapse = ","
          )
          
          query <- paste0(
            "
            SELECT
              p.player_id,
              p.player_name,
              p.primary_position,
              p.player_age,
              t.team_name,
              cy.season,
              cy.base_salary,
              cy.cap_hit,
              cy.guaranteed_amount,
              cy.option_type,
              cy.likely_incentives,
              cy.unlikely_incentives,
              cy.dead_cap,
              c.contract_type,
              c.contract_start_season,
              c.contract_end_season,
              c.total_value,
              c.guaranteed_value,
              c.free_agent_year,
              c.bird_rights
            FROM contract_years cy
            INNER JOIN players p
              ON p.player_id = cy.player_id
            INNER JOIN teams t
              ON t.team_id = cy.team_id
            LEFT JOIN contracts c
              ON c.contract_id = cy.contract_id
            WHERE t.team_name = ?
              AND cy.season IN (",
            season_placeholders,
            ")
              AND p.player_id IN (",
            player_placeholders,
            ")
            ORDER BY
              cy.season,
              COALESCE(cy.cap_hit,0) DESC,
              p.player_name
            "
          )
          
          incoming_future <- tryCatch(
            DBI::dbGetQuery(
              con,
              query,
              params = c(
                list(
                  scenario$partner_team
                ),
                as.list(seasons),
                as.list(incoming_ids)
              )
            ),
            error = function(e) {
              data.frame()
            }
          )
          
          if (nrow(incoming_future)) {
            
            incoming_future$team_name <-
              selected_team()
            
            common <- intersect(
              names(preview),
              names(incoming_future)
            )
            
            incoming_future <-
              incoming_future[
                ,
                common,
                drop = FALSE
              ]
            
            for (
              nm in setdiff(
                names(preview),
                names(incoming_future)
              )
            ) {
              incoming_future[[nm]] <- NA
            }
            
            incoming_future <-
              incoming_future[
                ,
                names(preview),
                drop = FALSE
              ]
            
            preview <- rbind(
              preview,
              incoming_future
            )
          }
        }
        
        rownames(preview) <- NULL
        preview
      })
      
      
      base_current_roster_contracts <- shiny::reactive({
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
          SELECT DISTINCT
            p.player_id,
            p.player_name,
            p.primary_position,
            p.player_age,
            COALESCE(rh.two_way_flag,0) AS two_way_flag,
            rh.roster_status,
            cy.cap_hit,
            cy.guaranteed_amount,
            cy.option_type,
            c.contract_type,
            c.contract_end_season,
            c.total_value,
            c.guaranteed_value,
            c.free_agent_year,
            c.bird_rights
          FROM roster_history rh
          INNER JOIN players p
            ON p.player_id = rh.player_id
          INNER JOIN teams t
            ON t.team_id = rh.team_id
          LEFT JOIN contract_years cy
            ON cy.player_id = rh.player_id
            AND cy.team_id = rh.team_id
            AND cy.season = rh.season
          LEFT JOIN contracts c
            ON c.contract_id = cy.contract_id
          WHERE t.team_name = ?
            AND rh.season = ?
          ORDER BY
            COALESCE(cy.cap_hit,0) DESC,
            p.player_name
          ",
          params = list(
            selected_team(),
            selected_season()
          )
        )
      })
      
      current_roster_contracts <- shiny::reactive({
        
        current <- base_current_roster_contracts()
        scenario <- active_trade_scenario()
        
        if (
          is.null(scenario) ||
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
        
        incoming_ids <- scenario_player_ids(
          scenario$incoming_players
        )
        
        if (
          length(incoming_ids) &&
          nrow(preview)
        ) {
          
          # Fill current-season contract context for incoming players
          # from the scenario-aware future table.
          incoming_current <- future_contracts()
          
          incoming_current <- incoming_current[
            incoming_current$season ==
              selected_season() &
              incoming_current$player_id %in%
              incoming_ids,
            ,
            drop = FALSE
          ]
          
          if (nrow(incoming_current)) {
            
            match_idx <- match(
              preview$player_id,
              incoming_current$player_id
            )
            
            fill_fields <- intersect(
              c(
                "player_name",
                "primary_position",
                "player_age",
                "cap_hit",
                "guaranteed_amount",
                "option_type",
                "contract_type",
                "contract_end_season",
                "total_value",
                "guaranteed_value",
                "free_agent_year",
                "bird_rights"
              ),
              intersect(
                names(preview),
                names(incoming_current)
              )
            )
            
            for (nm in fill_fields) {
              replace_rows <- !is.na(match_idx)
              
              preview[[nm]][replace_rows] <-
                incoming_current[[nm]][
                  match_idx[replace_rows]
                ]
            }
          }
        }
        
        preview
      })
      
      
      # ------------------------------------------------------
      # Year summaries
      # ------------------------------------------------------
      
      year_summary <- shiny::reactive({
        seasons <- planning_seasons()
        future <- future_contracts()
        current <- current_roster_contracts()
        
        rows <- lapply(
          seq_len(
            nrow(seasons)
          ),
          function(i) {
            
            yr <- seasons$year[[i]]
            season <- seasons$season[[i]]
            
            contracts <- future[
              future$season == season,
              ,
              drop = FALSE
            ]
            
            committed <- if (
              nrow(contracts)
            ) {
              sum(
                suppressWarnings(
                  as.numeric(
                    contracts$cap_hit
                  )
                ),
                na.rm = TRUE
              )
            } else {
              0
            }
            
            guaranteed <- if (
              nrow(contracts)
            ) {
              sum(
                suppressWarnings(
                  as.numeric(
                    contracts$guaranteed_amount
                  )
                ),
                na.rm = TRUE
              )
            } else {
              0
            }
            
            controlled_players <- if (
              nrow(contracts)
            ) {
              length(
                unique(
                  contracts$player_id
                )
              )
            } else {
              0L
            }
            
            option_count <- if (
              nrow(contracts)
            ) {
              sum(
                !is.na(
                  contracts$option_type
                ) &
                  nzchar(
                    trimws(
                      as.character(
                        contracts$option_type
                      )
                    )
                  ),
                na.rm = TRUE
              )
            } else {
              0L
            }
            
            free_agents <- if (
              nrow(current)
            ) {
              sum(
                suppressWarnings(
                  as.numeric(
                    current$free_agent_year
                  )
                ) ==
                  yr + 1L,
                na.rm = TRUE
              )
            } else {
              0L
            }
            
            data.frame(
              year = yr,
              season = season,
              committed_salary = committed,
              guaranteed_salary = guaranteed,
              controlled_players = controlled_players,
              option_count = option_count,
              free_agents = free_agents,
              stringsAsFactors = FALSE
            )
          }
        )
        
        do.call(
          rbind,
          rows
        )
      })
      
      # ------------------------------------------------------
      # Draft value if available
      # ------------------------------------------------------
      
      draft_value_result <- shiny::reactive({
        if (
          !exists(
            "evaluate_team_draft_value",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_team_draft_value(
            team_value = selected_team(),
            year_from =
              season_start_year() + 1L,
            year_to =
              season_start_year() + 6L,
            current_year =
              season_start_year() + 1L
          ),
          error = function(e) {
            NULL
          }
        )
      })
      
      draft_net_value <- shiny::reactive({
        result <- draft_value_result()
        
        if (
          is.null(result) ||
          is.null(
            result$summary
          )
        ) {
          return(NA_real_)
        }
        
        safe_num(
          result$summary$net_portfolio_value,
          NA_real_
        )
      })
      
      # ------------------------------------------------------
      # Core calculations
      # ------------------------------------------------------
      
      base_current_payroll <- shiny::reactive({
        
        d <- base_future_contracts()
        
        current <- d[
          d$season ==
            selected_season(),
          ,
          drop = FALSE
        ]
        
        if (!nrow(current)) {
          return(0)
        }
        
        sum(
          suppressWarnings(
            as.numeric(
              current$cap_hit
            )
          ),
          na.rm = TRUE
        )
      })
      
      
      current_payroll <- shiny::reactive({
        ys <- year_summary()
        
        if (!nrow(ys)) {
          return(0)
        }
        
        safe_num(
          ys$committed_salary[[1]],
          0
        )
      })
      
      year3_payroll <- shiny::reactive({
        ys <- year_summary()
        
        if (
          nrow(ys) < 3
        ) {
          return(0)
        }
        
        safe_num(
          ys$committed_salary[[3]],
          0
        )
      })
      
      near_term_fa <- shiny::reactive({
        d <- current_roster_contracts()
        
        if (!nrow(d)) {
          return(0L)
        }
        
        threshold <-
          season_start_year() +
          2L
        
        sum(
          suppressWarnings(
            as.numeric(
              d$free_agent_year
            )
          ) <= threshold,
          na.rm = TRUE
        )
      })
      
      team_option_count <- shiny::reactive({
        d <- future_contracts()
        
        if (!nrow(d)) {
          return(0L)
        }
        
        sum(
          grepl(
            "team",
            tolower(
              ifelse(
                is.na(
                  d$option_type
                ),
                "",
                d$option_type
              )
            ),
            fixed = TRUE
          ),
          na.rm = TRUE
        )
      })
      
      flexibility_metrics <- shiny::reactive({
        ys <- year_summary()
        current <- current_roster_contracts()
        
        current_salary <- max(
          current_payroll(),
          1
        )
        
        year3_drop <- max(
          0,
          1 -
            year3_payroll() /
            current_salary
        )
        
        payroll_flex <- score_clamp(
          35 +
            year3_drop *
            65
        )
        
        expiring_count <- near_term_fa()
        
        continuity_score <- score_clamp(
          100 -
            expiring_count *
            7
        )
        
        option_score <- score_clamp(
          45 +
            team_option_count() *
            10
        )
        
        draft_value <- draft_net_value()
        
        draft_score <- if (
          is.na(draft_value)
        ) {
          50
        } else {
          score_clamp(
            draft_value /
              2.5
          )
        }
        
        current_ages <- suppressWarnings(
          as.numeric(
            current$player_age
          )
        )
        
        avg_age <- if (
          length(current_ages) &&
          !all(is.na(current_ages))
        ) {
          mean(
            current_ages,
            na.rm = TRUE
          )
        } else {
          NA_real_
        }
        
        age_score <- if (
          is.na(avg_age)
        ) {
          50
        } else if (
          avg_age <= 26.5
        ) {
          82
        } else if (
          avg_age <= 28.5
        ) {
          70
        } else if (
          avg_age <= 30.5
        ) {
          55
        } else {
          38
        }
        
        composite <- mean(
          c(
            payroll_flex,
            continuity_score,
            option_score,
            draft_score,
            age_score
          ),
          na.rm = TRUE
        )
        
        list(
          payroll_flex = payroll_flex,
          continuity = continuity_score,
          options = option_score,
          draft = draft_score,
          age = age_score,
          composite = composite,
          avg_age = avg_age,
          year3_drop = year3_drop
        )
      })
      
      flexibility_label <- shiny::reactive({
        score <- flexibility_metrics()$composite
        
        if (score >= 78) {
          "Strong"
        } else if (
          score >= 62
        ) {
          "Healthy"
        } else if (
          score >= 48
        ) {
          "Moderate"
        } else {
          "Constrained"
        }
      })
      
      # ------------------------------------------------------
      # Snapshot outputs
      # ------------------------------------------------------
      
      output$outlook_trade_scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        base_payroll <- base_current_payroll()
        proposed_payroll <- current_payroll()
        
        delta <-
          proposed_payroll -
          base_payroll
        
        delta_class <- if (
          delta < 0
        ) {
          "positive"
        } else if (
          delta > 0
        ) {
          "warning"
        } else {
          ""
        }
        
        delta_text <- if (
          abs(delta) < 1
        ) {
          "$0.0M"
        } else if (
          delta > 0
        ) {
          paste0(
            "+",
            money(delta)
          )
        } else {
          paste0(
            "-",
            money(abs(delta))
          )
        }
        
        shiny::div(
          class = "outlook-v2-trade-banner",
          
          shiny::div(
            class = "outlook-v2-trade-copy",
            
            shiny::span(
              class = "outlook-v2-trade-chip",
              bsicons::bs_icon(
                "arrow-left-right"
              ),
              "TRADE SCENARIO"
            ),
            
            paste0(
              "Five-year planning now reflects ",
              selected_team(),
              " ↔ ",
              scenario$partner_team,
              ". Incoming and outgoing contract-year commitments are included across the loaded planning horizon."
            )
          ),
          
          shiny::span(
            class = paste(
              "outlook-v2-trade-delta",
              delta_class
            ),
            paste0(
              "Year 1 ",
              delta_text
            )
          )
        )
      })
      
      
      output$snapshot_current_payroll <- shiny::renderText({
        money(
          current_payroll()
        )
      })
      
      output$snapshot_year3_payroll <- shiny::renderText({
        money(
          year3_payroll()
        )
      })
      
      output$snapshot_near_term_fa <- shiny::renderText({
        near_term_fa()
      })
      
      output$snapshot_team_options <- shiny::renderText({
        team_option_count()
      })
      
      output$snapshot_draft_value <- shiny::renderText({
        value <- draft_net_value()
        
        if (is.na(value)) {
          "Not loaded"
        } else {
          sprintf(
            "%.1f",
            value
          )
        }
      })
      
      output$snapshot_flexibility <- shiny::renderText({
        flexibility_label()
      })
      
      output$flexibility_score <- shiny::renderText({
        sprintf(
          "%.0f",
          flexibility_metrics()$composite
        )
      })
      
      # ------------------------------------------------------
      # Decision
      # ------------------------------------------------------
      
      output$outlook_decision <- shiny::renderUI({
        metrics <- flexibility_metrics()
        
        label <- if (
          metrics$composite >= 78
        ) {
          "PRESERVE OPTIONALITY"
        } else if (
          metrics$composite >= 62
        ) {
          "MANAGE PROACTIVELY"
        } else if (
          metrics$composite >= 48
        ) {
          "CREATE FLEXIBILITY"
        } else {
          "RESTRUCTURE TIMELINE"
        }
        
        explanation <- if (
          metrics$composite >= 78
        ) {
          paste(
            "The current long-range profile provides multiple paths.",
            "Avoid unnecessary future commitments that reduce transaction or roster flexibility."
          )
        } else if (
          metrics$composite >= 62
        ) {
          paste(
            "The organization retains useful flexibility,",
            "but upcoming contract decisions should be sequenced carefully."
          )
        } else if (
          metrics$composite >= 48
        ) {
          paste(
            "Future optionality is moderate.",
            "Prioritize controllable contracts, draft capital, and staggered decision windows."
          )
        } else {
          paste(
            "Long-range flexibility is materially constrained by the loaded timeline.",
            "Explore contract restructuring and asset replenishment."
          )
        }
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  metrics$composite >= 62
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
                label
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
                "FLEXIBILITY SCORE"
              ),
              shiny::strong(
                paste0(
                  round(
                    metrics$composite
                  ),
                  "%"
                )
              ),
              shiny::tags$small(
                flexibility_label()
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "YEAR-3 PAYROLL CHANGE"
              ),
              shiny::strong(
                paste0(
                  round(
                    metrics$year3_drop *
                      100
                  ),
                  "% lower"
                )
              ),
              shiny::tags$small(
                "Loaded commitments only"
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Strategy scorecard
      # ------------------------------------------------------
      
      output$strategy_scorecard <- shiny::renderUI({
        metrics <- flexibility_metrics()
        
        shiny::tagList(
          score_row(
            "Payroll Flexibility",
            "Loaded Year-3 commitment runway",
            "cash-stack",
            metrics$payroll_flex
          ),
          
          score_row(
            "Roster Continuity",
            "Near-term free-agency turnover",
            "people",
            metrics$continuity
          ),
          
          score_row(
            "Team Control",
            "Team-option flexibility",
            "person-badge",
            metrics$options
          ),
          
          score_row(
            "Draft Optionality",
            "Loaded draft portfolio value",
            "calendar3",
            metrics$draft,
            if (
              is.na(
                draft_net_value()
              )
            ) {
              "Pending"
            } else {
              NULL
            }
          ),
          
          score_row(
            "Age Curve",
            "Current roster timeline",
            "graph-up-arrow",
            metrics$age
          )
        )
      })
      
      # ------------------------------------------------------
      # Five-year timeline
      # ------------------------------------------------------
      
      output$five_year_timeline <- shiny::renderUI({
        ys <- year_summary()
        
        maximum_payroll <- max(
          c(
            ys$committed_salary,
            1
          ),
          na.rm = TRUE
        )
        
        cards <- lapply(
          seq_len(
            nrow(ys)
          ),
          function(i) {
            
            row <- ys[
              i,
              ,
              drop = FALSE
            ]
            
            width <- if (
              maximum_payroll > 0
            ) {
              100 *
                safe_num(
                  row$committed_salary,
                  0
                ) /
                maximum_payroll
            } else {
              0
            }
            
            shiny::div(
              class = paste(
                "outlook-v2-year-card",
                if (
                  i == 1
                ) {
                  "current"
                } else {
                  ""
                }
              ),
              
              shiny::div(
                class = "outlook-v2-year-label",
                if (
                  i == 1
                ) {
                  "CURRENT"
                } else {
                  paste0(
                    "YEAR ",
                    i
                  )
                }
              ),
              
              shiny::div(
                class = "outlook-v2-season",
                row$season
              ),
              
              shiny::div(
                class = "outlook-v2-year-metric",
                shiny::span(
                  "Committed salary"
                ),
                shiny::strong(
                  money(
                    row$committed_salary
                  )
                )
              ),
              
              shiny::div(
                class = "outlook-v2-year-metric",
                shiny::span(
                  "Controlled players"
                ),
                shiny::strong(
                  row$controlled_players
                )
              ),
              
              shiny::div(
                class = "outlook-v2-year-metric",
                shiny::span(
                  "Free agents"
                ),
                shiny::strong(
                  row$free_agents
                )
              ),
              
              shiny::div(
                class = "outlook-v2-year-metric",
                shiny::span(
                  "Options"
                ),
                shiny::strong(
                  row$option_count
                )
              ),
              
              shiny::div(
                class = "outlook-v2-bar",
                
                shiny::div(
                  class = "outlook-v2-bar-fill",
                  style = paste0(
                    "width:",
                    width,
                    "%;"
                  )
                )
              )
            )
          }
        )
        
        shiny::tagList(
          shiny::div(
            class = "outlook-v2-five-grid",
            cards
          ),
          
          shiny::div(
            class = "outlook-v2-note",
            style = "padding:0 14px 14px;",
            paste(
              "Future salary values represent contract-year rows currently loaded in the TBI database.",
              "Missing future salary rows are not projected or imputed."
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Headlines
      # ------------------------------------------------------
      
      output$outlook_headlines <- shiny::renderUI({
        ys <- year_summary()
        current <- current_roster_contracts()
        
        final_committed <- if (
          nrow(ys)
        ) {
          safe_num(
            utils::tail(
              ys$committed_salary,
              1
            ),
            0
          )
        } else {
          0
        }
        
        scenario <- active_trade_scenario()
        
        scenario_headline <- if (
          is.null(scenario)
        ) {
          character()
        } else {
          paste0(
            "Active trade scenario versus ",
            scenario$partner_team,
            " is included in the five-year contract view."
          )
        }
        
        headlines <- c(
          scenario_headline,
          paste0(
            nrow(current),
            " current roster records feed the long-range view."
          ),
          paste0(
            near_term_fa(),
            " players reach free agency within the near-term planning window."
          ),
          paste0(
            team_option_count(),
            " team-option year",
            if (
              team_option_count() == 1
            ) "" else "s",
            " are represented in loaded future contract data."
          ),
          paste0(
            "Year-5 loaded commitments total ",
            money(
              final_committed
            ),
            "."
          )
        )
        
        shiny::tagList(
          lapply(
            headlines[
              nzchar(
                trimws(
                  as.character(
                    headlines
                  )
                )
              )
            ],
            function(item) {
              shiny::div(
                class = "tbi-v2-headline-row",
                
                shiny::span(
                  class = "tbi-v2-headline-dot"
                ),
                
                shiny::span(
                  item
                )
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Risks
      # ------------------------------------------------------
      
      output$outlook_risks <- shiny::renderUI({
        
        metrics <- flexibility_metrics()
        risks <- character()
        
        if (
          near_term_fa() >= 6
        ) {
          risks <- c(
            risks,
            paste0(
              near_term_fa(),
              " near-term free agents create significant roster-turnover exposure."
            )
          )
        }
        
        if (
          metrics$payroll_flex < 50
        ) {
          risks <- c(
            risks,
            "Loaded future commitments provide limited payroll relief over the next three seasons."
          )
        }
        
        if (
          metrics$age < 50
        ) {
          risks <- c(
            risks,
            "The current roster age profile creates meaningful long-range age-curve risk."
          )
        }
        
        scenario <- active_trade_scenario()
        
        if (!is.null(scenario)) {
          
          payroll_delta <-
            current_payroll() -
            base_current_payroll()
          
          if (payroll_delta > 5000000) {
            risks <- c(
              risks,
              paste(
                "The active trade scenario adds",
                money(
                  payroll_delta
                ),
                "to current-season payroll and should be reviewed against the long-range flexibility objective."
              )
            )
          }
        }
        
        if (
          is.na(
            draft_net_value()
          )
        ) {
          risks <- c(
            risks,
            "Draft assets are not yet loaded, so long-range asset protection cannot be fully evaluated."
          )
        } else if (
          metrics$draft < 40
        ) {
          risks <- c(
            risks,
            "Loaded draft capital provides limited downside protection."
          )
        }
        
        if (!length(risks)) {
          risks <-
            "No major structural long-range risk is identified by the currently loaded inputs."
        }
        
        shiny::tagList(
          lapply(
            unique(
              risks
            ),
            function(risk) {
              
              shiny::div(
                class = "tbi-v2-risk-item",
                
                shiny::div(
                  class = "tbi-v2-risk-icon",
                  bsicons::bs_icon(
                    "exclamation-triangle"
                  )
                ),
                
                shiny::div(
                  shiny::strong("Risk"),
                  shiny::p(
                    risk
                  )
                )
              )
            }
          )
        )
      })
      
      
      # ------------------------------------------------------
      # Opportunities
      # ------------------------------------------------------
      
      output$outlook_opportunities <- shiny::renderUI({
        metrics <- flexibility_metrics()
        opportunities <- character()
        
        if (
          metrics$year3_drop >= .25
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              "Loaded Year-3 commitments are ",
              round(
                metrics$year3_drop *
                  100
              ),
              "% below current payroll, creating potential future flexibility."
            )
          )
        }
        
        if (
          team_option_count() > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              team_option_count(),
              " team-option year",
              if (
                team_option_count() == 1
              ) "" else "s",
              " preserve future roster control."
            )
          )
        }
        
        if (
          !is.na(
            draft_net_value()
          ) &&
          metrics$draft >= 60
        ) {
          opportunities <- c(
            opportunities,
            "Draft capital can support future acquisitions or provide downside protection."
          )
        }
        
        if (
          metrics$age >= 70
        ) {
          opportunities <- c(
            opportunities,
            "The current age profile supports a multi-year competitive window."
          )
        }
        
        if (!length(opportunities)) {
          opportunities <- paste(
            "Use upcoming contract decision points to create",
            "greater flexibility and preserve optionality."
          )
        }
        
        shiny::tagList(
          lapply(
            unique(
              opportunities
            ),
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
      
      # ------------------------------------------------------
      # Contract runway table
      # ------------------------------------------------------
      
      output$contract_runway_table <- reactable::renderReactable({
        current <- current_roster_contracts()
        
        shiny::validate(
          shiny::need(
            nrow(current) > 0,
            paste(
              "No current roster contract data is available for",
              selected_team(),
              selected_season()
            )
          )
        )
        
        season_year <- season_start_year()
        
        years_remaining <- suppressWarnings(
          as.numeric(
            current$free_agent_year
          )
        ) -
          season_year
        
        years_remaining[
          is.na(
            years_remaining
          )
        ] <- NA_real_
        
        display <- data.frame(
          Player = current$player_name,
          Position = ifelse(
            is.na(
              current$primary_position
            ),
            "—",
            current$primary_position
          ),
          Age = ifelse(
            is.na(
              current$player_age
            ),
            "—",
            current$player_age
          ),
          `Current Cap Hit` = vapply(
            current$cap_hit,
            money,
            character(1)
          ),
          `Contract Through` = ifelse(
            is.na(
              current$contract_end_season
            ),
            "—",
            current$contract_end_season
          ),
          `Years Control` = ifelse(
            is.na(
              years_remaining
            ),
            "—",
            pmax(
              0,
              years_remaining
            )
          ),
          Option = ifelse(
            is.na(
              current$option_type
            ) |
              !nzchar(
                trimws(
                  as.character(
                    current$option_type
                  )
                )
              ),
            "—",
            current$option_type
          ),
          `FA Year` = ifelse(
            is.na(
              current$free_agent_year
            ),
            "—",
            current$free_agent_year
          ),
          `Bird Rights` = ifelse(
            is.na(
              current$bird_rights
            ),
            "—",
            current$bird_rights
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        reactable::reactable(
          display,
          searchable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          striped = FALSE,
          pagination = TRUE,
          defaultPageSize = 10,
          defaultSorted = "FA Year",
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
      # Readout
      # ------------------------------------------------------
      
      output$outlook_readout <- shiny::renderUI({
        metrics <- flexibility_metrics()
        ys <- year_summary()
        
        final_year <- if (
          nrow(ys)
        ) {
          ys[
            nrow(ys),
            ,
            drop = FALSE
          ]
        } else {
          NULL
        }
        
        shiny::tagList(
          signal_row(
            "Flexibility score",
            paste0(
              round(
                metrics$composite
              ),
              " / 100"
            )
          ),
          
          signal_row(
            "Current payroll",
            money(
              current_payroll()
            ),
            cba_term = "Team Salary"
          ),
          
          signal_row(
            "Year-3 committed",
            money(
              year3_payroll()
            ),
            cba_term = "Team Salary"
          ),
          
          signal_row(
            "Near-term free agents",
            as.character(
              near_term_fa()
            ),
            cba_term = "Unrestricted Free Agent (UFA)"
          ),
          
          signal_row(
            "Team option years",
            as.character(
              team_option_count()
            ),
            cba_term = "Team Option"
          ),
          
          signal_row(
            "Average age",
            if (
              is.na(
                metrics$avg_age
              )
            ) {
              "—"
            } else {
              sprintf(
                "%.1f",
                metrics$avg_age
              )
            }
          ),
          
          signal_row(
            "Draft portfolio",
            if (
              is.na(
                draft_net_value()
              )
            ) {
              "Not loaded"
            } else {
              sprintf(
                "%.1f value",
                draft_net_value()
              )
            }
          ),
          
          signal_row(
            "Year-5 commitments",
            if (
              is.null(final_year)
            ) {
              "—"
            } else {
              money(
                final_year$committed_salary
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Recommendation
      # ------------------------------------------------------
      
      output$outlook_recommendation <- shiny::renderUI({
        
        metrics <- flexibility_metrics()
        
        label <- if (
          metrics$composite >= 78
        ) {
          "PRESERVE OPTIONALITY"
        } else if (
          metrics$composite >= 62
        ) {
          "SEQUENCE DECISIONS"
        } else if (
          metrics$composite >= 48
        ) {
          "CREATE FLEXIBILITY"
        } else {
          "RESET LONG-RANGE STRUCTURE"
        }
        
        rationale <- if (
          metrics$composite >= 78
        ) {
          paste(
            "The organization has a strong long-range flexibility profile.",
            "Protect the future decision windows that allow the front office to pivot between retention, acquisition, and development."
          )
        } else if (
          metrics$composite >= 62
        ) {
          paste(
            "The long-range structure is workable but requires disciplined sequencing.",
            "Avoid stacking major contract decisions into the same offseason and preserve controllable alternatives."
          )
        } else if (
          metrics$composite >= 48
        ) {
          paste(
            "The current timeline offers only moderate optionality.",
            "Use extensions, trades, options, and upcoming expirations to spread risk and reopen future flexibility."
          )
        } else {
          paste(
            "The loaded long-range profile is restrictive.",
            "Prioritize shorter commitment windows, controllable contracts, and asset replenishment before additional aggressive spending."
          )
        }
        
        scenario <- active_trade_scenario()
        
        if (!is.null(scenario)) {
          
          payroll_delta <-
            current_payroll() -
            base_current_payroll()
          
          scenario_sentence <- if (
            abs(payroll_delta) < 1
          ) {
            paste(
              "The active trade scenario is approximately payroll-neutral in the current season.",
              "Incoming and outgoing contract commitments are included across the loaded planning horizon."
            )
          } else if (
            payroll_delta < 0
          ) {
            paste(
              "The active trade scenario reduces current-season payroll by",
              money(
                abs(
                  payroll_delta
                )
              ),
              "and the five-year view includes the incoming and outgoing contract commitments."
            )
          } else {
            paste(
              "The active trade scenario increases current-season payroll by",
              money(
                payroll_delta
              ),
              "and the five-year view includes the incoming and outgoing contract commitments."
            )
          }
          
          rationale <- paste(
            rationale,
            scenario_sentence
          )
        }
        
        if (
          is.na(
            draft_net_value()
          )
        ) {
          rationale <- paste(
            rationale,
            "Draft-capital analysis remains incomplete until the organization's draft assets are loaded."
          )
        }
        
        cba_reference_term <- if (
          team_option_count() > 0
        ) {
          "Team Option"
        } else if (
          near_term_fa() > 0
        ) {
          "Unrestricted Free Agent (UFA)"
        } else {
          "Team Salary"
        }
        
        recommendation_label <- if (
          exists(
            "tbi_cba_link",
            mode = "function"
          )
        ) {
          tbi_cba_link(
            term = cba_reference_term,
            label = label,
            class = "outlook-cba-link"
          )
        } else {
          shiny::strong(
            label
          )
        }
        
        shiny::div(
          class = "outlook-v2-rec-grid",
          
          shiny::div(
            class = "outlook-v2-rec-box",
            
            shiny::span(
              "RECOMMENDATION"
            ),
            
            shiny::strong(
              recommendation_label
            )
          ),
          
          shiny::div(
            
            shiny::h3(
              style = "margin:0 0 7px;",
              rationale
            ),
            
            shiny::p(
              class = "outlook-v2-note",
              style = "margin:0;",
              paste(
                "This view uses contract, roster, option, free-agency, and available draft-value inputs",
                "currently stored in TBI. Future salary values are loaded commitments, not salary projections."
              )
            )
          )
        )
      })
    }
  )
}
