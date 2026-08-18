# ============================================================
# PHASE 2 STEP 13 — FINAL INTEGRATION / QA
# Roster Intelligence
# Stable checkpoint: no visual redesign in this pass.
# ============================================================

# ------------------------------------------------------------
# Module: Roster Intelligence
# Version 2.4 Executive Roster Workspace — BIE Needs + Gap Analysis
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Roster Intelligence UI
#'
#' @param id Internal module ID.
#' @noRd
mod_roster_contracts_ui <- function(id) {
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
        class = "roster-cba-link"
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
    class = "tbi-module-page tbi-v2-roster-page",
    `data-tbi-subtab-input` = ns("active_subtab"),
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-v2-roster-page .roster-cba-link {
          color:#72adff !important;
          font-weight:800 !important;
          text-decoration:none !important;
        }

        .tbi-v2-roster-page .roster-cba-link::after {
          content:'  ↗';
          color:#5f9fee;
          font-size:.66em;
          opacity:.78;
        }

        .tbi-v2-roster-page .roster-cba-link:hover {
          color:#a8ceff !important;
          text-decoration:underline !important;
          text-underline-offset:2px;
        }

        .tbi-v2-roster-page .roster-trade-scenario-banner {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          padding:10px 13px;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:linear-gradient(90deg,rgba(59,130,246,.10),rgba(59,130,246,.03));
        }

        .tbi-v2-roster-page .roster-trade-scenario-copy {
          color:#a9b8ca;
          font-size:.60rem;
          line-height:1.45;
        }

        .tbi-v2-roster-page .roster-trade-scenario-chip {
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
          "ROSTER CONSTRUCTION"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Roster Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Evaluate roster balance, contract control, positional",
            "depth, player timelines, and decision flexibility."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "ROSTER MODEL"
        ),
        shiny::strong("PV 2.0")
      )
    ),
    
    shiny::uiOutput(
      ns("roster_trade_scenario_banner")
    ),
    
    # --------------------------------------------------------
    # Executive snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot",
      
      shiny::div(
        class = "tbi-v2-section-title-row",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("people")
          ),
          shiny::span("ROSTER SNAPSHOT")
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
          "ROSTER SIZE",
          "roster_size",
          "people",
          "blue"
        ),
        
        snapshot_item(
          "STANDARD",
          "standard_contracts",
          "person-badge",
          "blue"
        ),
        
        snapshot_item(
          "TWO-WAY",
          "two_way_contracts",
          "arrow-left-right",
          "green",
          cba_term = "Two-Way Contract"
        ),
        
        snapshot_item(
          "AVG AGE",
          "average_age",
          "calendar3",
          "blue"
        ),
        
        snapshot_item(
          "PAYROLL",
          "total_payroll",
          "cash-stack",
          "orange"
        ),
        
        snapshot_item(
          "OPEN SPOTS",
          "open_roster_spots",
          "bullseye",
          "purple"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Roster decision + Position Value 2.0
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-main-grid",
      
      shiny::tags$section(
        class = "tbi-v2-decision-card",
        
        shiny::div(
          class = "tbi-v2-section-title",
          
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-warning",
            bsicons::bs_icon("exclamation-triangle")
          ),
          
          shiny::span("ROSTER DECISION")
        ),
        
        shiny::uiOutput(
          ns("roster_decision")
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
            shiny::span("POSITION VALUE 2.0")
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Roster"),
            shiny::strong(
              shiny::textOutput(
                ns("roster_composite_score"),
                inline = TRUE
              )
            ),
            shiny::span("/ 100")
          )
        ),
        
        shiny::uiOutput(
          ns("position_value_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Headlines / risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-bottom-grid",
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-headlines-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("people")
          ),
          shiny::span("ROSTER HEADLINES")
        ),
        
        shiny::uiOutput(
          ns("roster_headlines")
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
          shiny::span("KEY RISKS")
        ),
        
        shiny::uiOutput(
          ns("roster_risks")
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
          shiny::span("KEY OPPORTUNITIES")
        ),
        
        shiny::uiOutput(
          ns("roster_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Roster composition + contract control
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-cap-detail-grid",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "ROSTER COMPOSITION"
            ),
            shiny::h3("Contract and roster categories")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "CURRENT"
          )
        ),
        
        shiny::div(
          style = "padding:14px 16px;",
          shiny::uiOutput(
            ns("roster_composition")
          )
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "EXECUTIVE BRIEF"
            ),
            shiny::h3("Roster assessment")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "DECISION SUPPORT"
          )
        ),
        
        shiny::div(
          style = "padding:14px 16px;",
          shiny::uiOutput(
            ns("roster_assessment")
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # BIE Roster Decision Intelligence
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "BASKETBALL INTELLIGENCE ENGINE"
          ),
          shiny::h3(
            "Roster decision intelligence"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "BIE ROSTER"
        )
      ),
      
      shiny::div(
        style = "padding:14px 16px;",
        shiny::uiOutput(
          ns("bie_roster_decision_summary")
        )
      ),
      
      shiny::div(
        style = paste(
          "max-height:420px;",
          "overflow:auto;",
          "border-top:1px solid rgba(148,163,184,.08);"
        ),
        reactable::reactableOutput(
          ns("bie_roster_decision_table")
        )
      )
    ),
    
    # --------------------------------------------------------
    # BIE Roster Needs + Gap Analysis
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "BASKETBALL INTELLIGENCE ENGINE"
          ),
          shiny::h3(
            "Roster needs + gap analysis"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "BIE NEEDS"
        )
      ),
      
      shiny::div(
        style = "padding:14px 16px;",
        shiny::uiOutput(
          ns("bie_roster_needs_summary")
        )
      ),
      
      shiny::div(
        style = paste(
          "max-height:390px;",
          "overflow:auto;",
          "border-top:1px solid rgba(148,163,184,.08);"
        ),
        reactable::reactableOutput(
          ns("bie_roster_needs_table")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Complete roster
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "PERSONNEL"
          ),
          shiny::h3("Complete roster")
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "LIVE DATA"
        )
      ),
      
      shiny::div(
        style = "max-height:520px; overflow:auto;",
        reactable::reactableOutput(
          ns("roster_table")
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Roster Intelligence server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @noRd
mod_roster_contracts_server <- function(
    id,
    selected_team,
    selected_season,
    transaction_state = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      subtab_seen <- shiny::reactiveValues(
        overview = TRUE,
        decision = FALSE,
        needs = FALSE,
        roster = FALSE,
        `legacy-hidden` = FALSE
      )

      shiny::observeEvent(
        input$active_subtab,
        {
          tab <- as.character(input$active_subtab)
          valid_tabs <- c(
            "overview",
            "decision",
            "needs",
            "roster"
          )

          if (length(tab) == 1L && tab %in% valid_tabs) {
            subtab_seen[[tab]] <- TRUE
          }
        },
        ignoreInit = FALSE
      )

      subtab_ready <- function(tab) {
        shiny::req(isTRUE(subtab_seen[[tab]]))
        invisible(TRUE)
      }
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
      # Permanent local numeric coercion helper.
      # Roster Decision Intelligence uses this helper in several
      # render paths; keeping it inside the module prevents
      # namespace / load-order failures.
      safe_num <- function(
    x,
    fallback = NA_real_) {
        
        if (
          is.null(x) ||
          !length(x)
        ) {
          return(fallback)
        }
        
        value <- suppressWarnings(
          as.numeric(
            x[[1]]
          )
        )
        
        if (
          !length(value) ||
          !is.finite(value)
        ) {
          fallback
        } else {
          value
        }
      }
      
      money <- function(x) {
        x <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(x) ||
          is.na(x)
        ) {
          return("—")
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
      
      numeric_or_zero <- function(x) {
        x <- suppressWarnings(
          as.numeric(x)
        )
        x[is.na(x)] <- 0
        x
      }
      
      text_or <- function(x, fallback = "—") {
        x <- as.character(x)
        
        if (
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(trimws(x[[1]]))
        ) {
          fallback
        } else {
          x[[1]]
        }
      }
      
      normalize_position <- function(position) {
        value <- toupper(
          trimws(
            as.character(
              position %||% ""
            )
          )
        )
        
        if (grepl("PG", value, fixed = TRUE)) return("PG")
        if (grepl("SG", value, fixed = TRUE)) return("SG")
        if (grepl("SF", value, fixed = TRUE)) return("SF")
        if (grepl("PF", value, fixed = TRUE)) return("PF")
        if (value == "C" || grepl("CENTER", value, fixed = TRUE)) return("C")
        if (value == "G") return("PG")
        if (value == "F") return("SF")
        
        "OTHER"
      }
      
      rating_label <- function(score) {
        score <- suppressWarnings(
          as.numeric(score)
        )
        
        if (is.na(score)) return("Unavailable")
        if (score >= 90) return("Elite")
        if (score >= 80) return("Strong")
        if (score >= 70) return("Stable")
        if (score >= 60) return("Watch")
        "Needs Review"
      }
      
      score_tone <- function(score) {
        if (score >= 80) {
          "green"
        } else if (score >= 70) {
          "blue"
        } else if (score >= 60) {
          "orange"
        } else {
          "red"
        }
      }
      
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
            class = "roster-cba-link"
          )
        } else {
          shiny::span(label)
        }
        
        shiny::div(
          class = "tbi-signal-row",
          rendered_label,
          shiny::strong(value)
        )
      }
      
      roster_cba_term <- function(value, type = "contract") {
        
        value <- tolower(
          trimws(
            as.character(
              value %||% ""
            )
          )
        )
        
        if (!nzchar(value) || identical(value, "—")) {
          return(NULL)
        }
        
        if (identical(type, "category")) {
          if (grepl("two-way", value, fixed = TRUE)) {
            return("Two-Way Contract")
          }
          if (grepl("exhibit", value, fixed = TRUE)) {
            return("Exhibit 10")
          }
          return(NULL)
        }
        
        if (identical(type, "option")) {
          if (grepl("team", value, fixed = TRUE)) {
            return("Team Option")
          }
          if (grepl("player", value, fixed = TRUE)) {
            return("Player Option")
          }
          return(NULL)
        }
        
        if (identical(type, "bird")) {
          return("Bird Exception")
        }
        
        if (grepl("rookie", value, fixed = TRUE)) {
          return("Rookie Scale Contract")
        }
        
        if (grepl("exhibit", value, fixed = TRUE)) {
          return("Exhibit 10")
        }
        
        if (grepl("two-way", value, fixed = TRUE)) {
          return("Two-Way Contract")
        }
        
        NULL
      }
      
      roster_table_link <- function(value, term) {
        
        if (
          is.null(term) ||
          !nzchar(as.character(term)) ||
          !exists("tbi_cba_link", mode = "function")
        ) {
          return(shiny::span(value))
        }
        
        tbi_cba_link(
          term = term,
          label = value,
          class = "roster-cba-link"
        )
      }
      
      # ------------------------------------------------------
      # Roster + contract data
      # ------------------------------------------------------
      
      base_selected_roster <- shiny::reactive({
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
            p.player_age,
            p.birth_date,
            p.height_inches,
            p.weight_lbs,

            t.team_name,
            t.abbreviation,

            rh.roster_status,
            COALESCE(rh.two_way_flag, 0) AS two_way_flag,
            rh.jersey_number,

            c.contract_id,
            c.contract_type,
            c.contract_start_season,
            c.contract_end_season,
            c.total_value,
            c.guaranteed_value,
            c.free_agent_year,
            c.bird_rights,
            c.trade_bonus_percent,

            cy.base_salary,
            cy.cap_hit,
            cy.guaranteed_amount,
            cy.option_type,
            cy.likely_incentives,
            cy.unlikely_incentives,
            cy.dead_cap

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
            CASE
              WHEN COALESCE(rh.two_way_flag, 0) = 1 THEN 2
              ELSE 1
            END,
            COALESCE(cy.cap_hit, 0) DESC,
            p.player_name
          ",
          params = list(
            selected_team(),
            selected_season()
          )
        )
      })
      
      base_selected_roster <- shiny::bindCache(
        base_selected_roster,
        selected_team(),
        selected_season(),
        cache = "session"
      )

      
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
            as.character(scenario$scenario_type),
            "trade"
          ) ||
          !identical(
            as.character(scenario$season),
            as.character(selected_season())
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
        
        # Normalize the same pending transaction to whichever
        # organization is currently being viewed.
        if (identical(selected, partner_team_name)) {
          
          tmp_team <- scenario$team
          tmp_outgoing <- scenario$outgoing_players
          tmp_incoming <- scenario$incoming_players
          tmp_outgoing_salary <- scenario$outgoing_salary
          tmp_incoming_salary <- scenario$incoming_salary
          
          scenario$team <- scenario$partner_team
          scenario$partner_team <- tmp_team
          
          scenario$outgoing_players <- tmp_incoming
          scenario$incoming_players <- tmp_outgoing
          
          scenario$outgoing_salary <- tmp_incoming_salary
          scenario$incoming_salary <- tmp_outgoing_salary
          scenario$salary_delta <-
            scenario$incoming_salary -
            scenario$outgoing_salary
        }
        
        scenario
      })
      
      
      selected_roster <- shiny::reactive({
        
        current <- base_selected_roster()
        
        if (
          is.null(active_trade_scenario()) ||
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
        
        scenario <- active_trade_scenario()
        
        if (
          is.data.frame(scenario$incoming_players) &&
          nrow(scenario$incoming_players) &&
          "player_id" %in% names(preview)
        ) {
          incoming_ids <- suppressWarnings(
            as.integer(
              scenario$incoming_players$player_id
            )
          )
          
          incoming_rows <- preview$player_id %in%
            incoming_ids
          
          if ("team_name" %in% names(preview)) {
            preview$team_name[incoming_rows] <- selected_team()
          }
          
          if ("roster_status" %in% names(preview)) {
            preview$roster_status[incoming_rows] <- "Trade Scenario"
          }
        }
        
        preview
      })
      
      
      output$roster_trade_scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        outgoing_count <- if (
          is.data.frame(scenario$outgoing_players)
        ) {
          nrow(scenario$outgoing_players)
        } else {
          0
        }
        
        incoming_count <- if (
          is.data.frame(scenario$incoming_players)
        ) {
          nrow(scenario$incoming_players)
        } else {
          0
        }
        
        shiny::div(
          class = "roster-trade-scenario-banner",
          
          shiny::div(
            class = "roster-trade-scenario-copy",
            
            shiny::span(
              class = "roster-trade-scenario-chip",
              bsicons::bs_icon("arrow-left-right"),
              "TRADE PREVIEW"
            ),
            
            paste0(
              selected_team(),
              " proposed roster: ",
              outgoing_count,
              " outgoing / ",
              incoming_count,
              " incoming versus ",
              scenario$partner_team,
              ". Roster metrics and ledger below reflect the proposed roster."
            )
          ),
          
          shiny::span(
            class = "tbi-v2-section-status",
            "SCENARIO"
          )
        )
      })
      
      
      standard_roster <- shiny::reactive({
        d <- selected_roster()
        
        d[
          numeric_or_zero(
            d$two_way_flag
          ) == 0 &
            !grepl(
              "two-way",
              tolower(
                ifelse(
                  is.na(d$contract_type),
                  "",
                  d$contract_type
                )
              ),
              fixed = TRUE
            ),
          ,
          drop = FALSE
        ]
      })
      
      two_way_roster <- shiny::reactive({
        d <- selected_roster()
        
        d[
          numeric_or_zero(
            d$two_way_flag
          ) == 1 |
            grepl(
              "two-way",
              tolower(
                ifelse(
                  is.na(d$contract_type),
                  "",
                  d$contract_type
                )
              ),
              fixed = TRUE
            ),
          ,
          drop = FALSE
        ]
      })
      
      roster_payroll <- shiny::reactive({
        sum(
          numeric_or_zero(
            selected_roster()$cap_hit
          ),
          na.rm = TRUE
        )
      })
      
      
      # ------------------------------------------------------
      # BIE Roster Decision Intelligence
      # ------------------------------------------------------
      
      bie_roster_cache <- shiny::reactiveVal(
        list(
          key = NULL,
          evaluated = NULL,
          decision = NULL
        )
      )
      
      
      bie_roster_decision_result <- shiny::reactive({
        
        d <- selected_roster()
        
        if (
          !nrow(d) ||
          !exists(
            "evaluate_bie_roster_decisions",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        key <- if (
          exists(
            "bie_roster_signature",
            mode = "function"
          )
        ) {
          bie_roster_signature(d)
        } else {
          paste(
            selected_team(),
            selected_season(),
            nrow(d),
            paste(
              sort(
                suppressWarnings(
                  as.integer(
                    d$player_id
                  )
                )
              ),
              collapse = ","
            ),
            sep = "|"
          )
        }
        
        cached <- shiny::isolate(bie_roster_cache())
        
        if (
          !is.null(cached$key) &&
          identical(
            cached$key,
            key
          ) &&
          !is.null(cached$decision)
        ) {
          return(
            cached$decision
          )
        }
        
        evaluated <- tryCatch(
          if (
            exists(
              "bie_ensure_evaluated_players",
              mode = "function"
            )
          ) {
            bie_ensure_evaluated_players(d)
          } else {
            evaluate_bie_players(d)
          },
          error = function(e) NULL
        )
        
        if (
          is.null(evaluated) ||
          !nrow(evaluated)
        ) {
          return(NULL)
        }
        
        decision <- tryCatch(
          evaluate_bie_roster_decisions(
            roster_players =
              evaluated
          ),
          error = function(e) {
            list(
              status = "ERROR",
              confidence =
                "FOUNDATION",
              explanation =
                conditionMessage(e)
            )
          }
        )
        
        bie_roster_cache(
          list(
            key = key,
            evaluated =
              evaluated,
            decision =
              decision
          )
        )
        
        decision
      })
      
      
      output$bie_roster_decision_summary <- shiny::renderUI({
        subtab_ready("legacy-hidden")
        
        result <- bie_roster_decision_result()
        
        if (is.null(result)) {
          return(
            shiny::div(
              style =
                "color:#8c9bae; font-size:.70rem;",
              "BIE roster decision intelligence is unavailable."
            )
          )
        }
        
        if (
          !identical(
            result$status,
            "OK"
          )
        ) {
          return(
            shiny::div(
              class = "tbi-v2-risk-card",
              shiny::span(
                class =
                  "tbi-v2-risk-icon",
                bsicons::bs_icon(
                  "exclamation-triangle"
                )
              ),
              shiny::div(
                shiny::strong(
                  "BIE REVIEW"
                ),
                shiny::p(
                  result$explanation
                )
              )
            )
          )
        }
        
        metric_card <- function(
    label,
    value) {
          
          shiny::div(
            style = paste(
              "min-width:0;",
              "padding:10px 11px;",
              "border:1px solid rgba(148,163,184,.10);",
              "border-radius:9px;",
              "background:rgba(255,255,255,.015);"
            ),
            
            shiny::span(
              class =
                "tbi-v2-snapshot-label",
              label
            ),
            
            shiny::strong(
              style = paste(
                "display:block;",
                "margin-top:5px;",
                "color:#eef4fb;",
                "font-size:.80rem;",
                "line-height:1.3;"
              ),
              value
            )
          )
        }
        
        
        score_text <- if (
          is.null(
            result$roster_score
          ) ||
          !is.finite(
            safe_num(
              result$roster_score,
              NA_real_
            )
          )
        ) {
          "—"
        } else {
          sprintf(
            "%.1f / 100",
            result$roster_score
          )
        }
        
        
        age_text <- if (
          is.null(
            result$average_age
          ) ||
          !is.finite(
            safe_num(
              result$average_age,
              NA_real_
            )
          )
        ) {
          "—"
        } else {
          sprintf(
            "%.1f",
            result$average_age
          )
        }
        
        
        concentration_text <- if (
          is.null(
            result$top3_payroll_share
          ) ||
          !is.finite(
            safe_num(
              result$top3_payroll_share,
              NA_real_
            )
          )
        ) {
          "—"
        } else {
          sprintf(
            "%.1f%%",
            result$top3_payroll_share
          )
        }
        
        
        priority_items <- shiny::tagList(
          lapply(
            head(
              result$priorities,
              4
            ),
            function(item) {
              shiny::div(
                class =
                  "tbi-v2-summary-item",
                shiny::span(
                  class =
                    "tbi-v2-summary-dot tbi-v2-summary-dot-warning"
                ),
                shiny::span(item)
              )
            }
          )
        )
        
        
        strength_items <- shiny::tagList(
          lapply(
            head(
              result$strengths,
              3
            ),
            function(item) {
              shiny::div(
                class =
                  "tbi-v2-summary-item",
                shiny::span(
                  class =
                    "tbi-v2-summary-dot tbi-v2-summary-dot-success"
                ),
                shiny::span(item)
              )
            }
          )
        )
        
        
        shiny::tagList(
          
          shiny::div(
            style = paste(
              "display:grid;",
              "grid-template-columns:repeat(6,minmax(0,1fr));",
              "gap:8px;"
            ),
            
            metric_card(
              "BIE ROSTER SCORE",
              score_text
            ),
            
            metric_card(
              "CONFIDENCE",
              result$confidence
            ),
            
            metric_card(
              "STANDARD",
              result$standard_count
            ),
            
            metric_card(
              "TWO-WAY",
              result$two_way_count
            ),
            
            metric_card(
              "AVG AGE",
              age_text
            ),
            
            metric_card(
              "TOP-3 PAYROLL",
              concentration_text
            )
          ),
          
          shiny::div(
            style = paste(
              "display:grid;",
              "grid-template-columns:1fr 1fr;",
              "gap:12px;",
              "margin-top:11px;"
            ),
            
            shiny::div(
              class =
                "tbi-v2-summary-card",
              shiny::div(
                class =
                  "tbi-v2-summary-title",
                "ROSTER PRIORITIES"
              ),
              priority_items
            ),
            
            shiny::div(
              class =
                "tbi-v2-summary-card",
              shiny::div(
                class =
                  "tbi-v2-summary-title",
                "ROSTER STRENGTHS"
              ),
              strength_items
            )
          ),
          
          shiny::p(
            style = paste(
              "margin:11px 0 0;",
              "color:#8296af;",
              "font-size:.64rem;",
              "line-height:1.5;"
            ),
            result$explanation
          )
        )
      })
      
      
      bie_roster_needs_result <- shiny::reactive({
        
        decision <- bie_roster_decision_result()
        
        if (
          is.null(decision) ||
          !identical(
            decision$status,
            "OK"
          ) ||
          !exists(
            "evaluate_bie_roster_needs",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        cache <- bie_roster_cache()
        
        evaluated <- cache$evaluated
        
        if (
          is.null(evaluated) ||
          !is.data.frame(evaluated) ||
          !nrow(evaluated)
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_bie_roster_needs(
            roster_players =
              evaluated,
            roster_decision =
              decision
          ),
          error = function(e) {
            list(
              status = "ERROR",
              confidence =
                "FOUNDATION",
              explanation =
                conditionMessage(e)
            )
          }
        )
      })
      
      
      output$bie_roster_needs_summary <- shiny::renderUI({
        subtab_ready("legacy-hidden")
        
        result <- bie_roster_needs_result()
        
        if (is.null(result)) {
          return(
            shiny::div(
              style =
                "color:#8c9bae; font-size:.70rem;",
              "BIE roster-needs analysis is unavailable."
            )
          )
        }
        
        if (
          !identical(
            result$status,
            "OK"
          )
        ) {
          return(
            shiny::div(
              class = "tbi-v2-risk-card",
              shiny::span(
                class =
                  "tbi-v2-risk-icon",
                bsicons::bs_icon(
                  "exclamation-triangle"
                )
              ),
              shiny::div(
                shiny::strong(
                  "BIE REVIEW"
                ),
                shiny::p(
                  result$explanation
                )
              )
            )
          )
        }
        
        metric_card <- function(
    label,
    value,
    accent = "#eef4fb") {
          
          shiny::div(
            style = paste(
              "min-width:0;",
              "padding:10px 11px;",
              "border:1px solid rgba(148,163,184,.10);",
              "border-radius:9px;",
              "background:rgba(255,255,255,.015);"
            ),
            
            shiny::span(
              class =
                "tbi-v2-snapshot-label",
              label
            ),
            
            shiny::strong(
              style = paste0(
                "display:block;",
                "margin-top:5px;",
                "color:",
                accent,
                "; font-size:.78rem;",
                "line-height:1.35;"
              ),
              value
            )
          )
        }
        
        
        action_items <- shiny::tagList(
          lapply(
            head(
              result$recommended_actions,
              4
            ),
            function(item) {
              shiny::div(
                class =
                  "tbi-v2-summary-item",
                shiny::span(
                  class =
                    "tbi-v2-summary-dot tbi-v2-summary-dot-warning"
                ),
                shiny::span(item)
              )
            }
          )
        )
        
        
        shiny::tagList(
          
          shiny::div(
            style = paste(
              "display:grid;",
              "grid-template-columns:repeat(4,minmax(0,1fr));",
              "gap:8px;"
            ),
            
            metric_card(
              "PRIMARY NEED",
              result$primary_need,
              "#fbbf24"
            ),
            
            metric_card(
              "SECONDARY NEED",
              result$secondary_need
            ),
            
            metric_card(
              "THIRD NEED",
              result$third_need
            ),
            
            metric_card(
              "CONFIDENCE",
              result$confidence,
              "#60a5fa"
            )
          ),
          
          shiny::div(
            class = "tbi-v2-summary-card",
            style = "margin-top:11px;",
            
            shiny::div(
              class =
                "tbi-v2-summary-title",
              "RECOMMENDED FRONT-OFFICE ACTIONS"
            ),
            
            action_items
          ),
          
          shiny::p(
            style = paste(
              "margin:11px 0 0;",
              "color:#8296af;",
              "font-size:.64rem;",
              "line-height:1.5;"
            ),
            result$explanation
          )
        )
      })
      
      
      output$bie_roster_needs_table <- reactable::renderReactable({
        subtab_ready("legacy-hidden")
        
        result <- bie_roster_needs_result()
        
        if (
          is.null(result) ||
          !identical(
            result$status,
            "OK"
          ) ||
          !is.data.frame(
            result$needs
          ) ||
          !nrow(
            result$needs
          )
        ) {
          return(NULL)
        }
        
        d <- result$needs
        
        d$score_display <- sprintf(
          "%.0f",
          d$score
        )
        
        reactable::reactable(
          d[
            ,
            c(
              "need",
              "category",
              "priority",
              "score_display",
              "evidence_type",
              "evidence",
              "recommended_action"
            ),
            drop = FALSE
          ],
          
          columns = list(
            need =
              reactable::colDef(
                name = "ROSTER NEED",
                minWidth = 160
              ),
            
            category =
              reactable::colDef(
                name = "CATEGORY",
                minWidth = 120
              ),
            
            priority =
              reactable::colDef(
                name = "PRIORITY",
                width = 88
              ),
            
            score_display =
              reactable::colDef(
                name = "NEED SCORE",
                width = 90
              ),
            
            evidence_type =
              reactable::colDef(
                name = "EVIDENCE",
                width = 105
              ),
            
            evidence =
              reactable::colDef(
                name = "WHY",
                minWidth = 230
              ),
            
            recommended_action =
              reactable::colDef(
                name = "FRONT-OFFICE ACTION",
                minWidth = 240
              )
          ),
          
          bordered = FALSE,
          striped = FALSE,
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          defaultPageSize =
            nrow(d)
        )
      })
      
      
      output$bie_roster_decision_table <- reactable::renderReactable({
        subtab_ready("legacy-hidden")
        
        result <- bie_roster_decision_result()
        
        if (
          is.null(result) ||
          !identical(
            result$status,
            "OK"
          ) ||
          !is.data.frame(
            result$player_decisions
          ) ||
          !nrow(
            result$player_decisions
          )
        ) {
          return(NULL)
        }
        
        d <- result$player_decisions
        
        d$salary_display <- vapply(
          d$salary,
          money,
          character(1)
        )
        
        d$bie_display <- ifelse(
          d$performance_available &
            is.finite(
              d$bie_player_score
            ),
          sprintf(
            "%.1f",
            d$bie_player_score
          ),
          "UNRATED"
        )
        
        reactable::reactable(
          d[
            ,
            c(
              "player_name",
              "position",
              "age",
              "salary_display",
              "bie_display",
              "decision_tier",
              "recommended_action",
              "contract_end"
            ),
            drop = FALSE
          ],
          
          columns = list(
            player_name =
              reactable::colDef(
                name = "PLAYER",
                minWidth = 150
              ),
            
            position =
              reactable::colDef(
                name = "POS",
                width = 62
              ),
            
            age =
              reactable::colDef(
                name = "AGE",
                width = 62,
                format =
                  reactable::colFormat(
                    digits = 0
                  )
              ),
            
            salary_display =
              reactable::colDef(
                name = "CAP HIT",
                minWidth = 90
              ),
            
            bie_display =
              reactable::colDef(
                name = "BIE",
                width = 80
              ),
            
            decision_tier =
              reactable::colDef(
                name = "ROSTER TIER",
                minWidth = 120
              ),
            
            recommended_action =
              reactable::colDef(
                name = "BIE ACTION",
                minWidth = 170
              ),
            
            contract_end =
              reactable::colDef(
                name = "CONTROL",
                minWidth = 95
              )
          ),
          
          bordered = FALSE,
          striped = FALSE,
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          defaultPageSize =
            nrow(d)
        )
      })
      
      
      # ------------------------------------------------------
      # Position Value 2.0
      # ------------------------------------------------------
      
      position_scores <- shiny::reactive({
        d <- selected_roster()
        
        positions <- c(
          "PG",
          "SG",
          "SF",
          "PF",
          "C"
        )
        
        if (!nrow(d)) {
          return(
            data.frame(
              position = positions,
              score = rep(0, 5),
              player_count = rep(0, 5),
              salary_share = rep(0, 5),
              avg_age = rep(NA_real_, 5),
              stringsAsFactors = FALSE
            )
          )
        }
        
        d$position_group <- vapply(
          d$primary_position,
          normalize_position,
          character(1)
        )
        
        total_payroll <- max(
          1,
          roster_payroll()
        )
        
        rows <- lapply(
          positions,
          function(pos) {
            
            group <- d[
              d$position_group == pos,
              ,
              drop = FALSE
            ]
            
            count <- nrow(group)
            
            salary <- sum(
              numeric_or_zero(
                group$cap_hit
              ),
              na.rm = TRUE
            )
            
            share <- salary /
              total_payroll
            
            ages <- suppressWarnings(
              as.numeric(
                group$player_age
              )
            )
            
            avg_age <- if (
              length(ages) &&
              !all(is.na(ages))
            ) {
              mean(
                ages,
                na.rm = TRUE
              )
            } else {
              NA_real_
            }
            
            depth_score <- min(
              100,
              count * 32
            )
            
            control_bonus <- sum(
              tolower(
                ifelse(
                  is.na(
                    group$option_type
                  ),
                  "",
                  group$option_type
                )
              ) %in% c(
                "team option",
                "club option"
              ),
              na.rm = TRUE
            ) * 4
            
            two_way_bonus <- sum(
              numeric_or_zero(
                group$two_way_flag
              ) == 1,
              na.rm = TRUE
            ) * 2
            
            concentration_penalty <- if (
              share > .35
            ) {
              min(
                12,
                (share - .35) *
                  100
              )
            } else {
              0
            }
            
            age_adjustment <- if (
              is.na(avg_age)
            ) {
              0
            } else if (
              avg_age >= 23 &&
              avg_age <= 29
            ) {
              6
            } else if (
              avg_age > 32
            ) {
              -5
            } else {
              2
            }
            
            score <- max(
              0,
              min(
                100,
                28 +
                  depth_score * .55 +
                  control_bonus +
                  two_way_bonus +
                  age_adjustment -
                  concentration_penalty
              )
            )
            
            data.frame(
              position = pos,
              score = score,
              player_count = count,
              salary_share = share,
              avg_age = avg_age,
              stringsAsFactors = FALSE
            )
          }
        )
        
        do.call(
          rbind,
          rows
        )
      })
      
      roster_score <- shiny::reactive({
        scores <- position_scores()$score
        
        if (!length(scores)) {
          return(0)
        }
        
        mean(
          scores,
          na.rm = TRUE
        )
      })
      
      # ------------------------------------------------------
      # Snapshot
      # ------------------------------------------------------
      
      output$roster_size <- shiny::renderText({
        nrow(
          selected_roster()
        )
      })
      
      output$standard_contracts <- shiny::renderText({
        nrow(
          standard_roster()
        )
      })
      
      output$two_way_contracts <- shiny::renderText({
        nrow(
          two_way_roster()
        )
      })
      
      output$average_age <- shiny::renderText({
        ages <- suppressWarnings(
          as.numeric(
            selected_roster()$player_age
          )
        )
        
        if (
          !length(ages) ||
          all(is.na(ages))
        ) {
          "—"
        } else {
          sprintf(
            "%.1f",
            mean(
              ages,
              na.rm = TRUE
            )
          )
        }
      })
      
      output$total_payroll <- shiny::renderText({
        money(
          roster_payroll()
        )
      })
      
      output$open_roster_spots <- shiny::renderText({
        standard_count <- nrow(
          standard_roster()
        )
        
        max(
          0,
          15 -
            standard_count
        )
      })
      
      output$roster_composite_score <- shiny::renderText({
        sprintf(
          "%.0f",
          roster_score()
        )
      })
      
      # ------------------------------------------------------
      # Roster decision
      # ------------------------------------------------------
      
      output$roster_decision <- shiny::renderUI({
        d <- selected_roster()
        scores <- position_scores()
        standard_count <- nrow(
          standard_roster()
        )
        
        if (!nrow(d)) {
          return(
            shiny::div(
              class = "tbi-v2-decision-main",
              shiny::div(
                class = "tbi-v2-decision-symbol",
                bsicons::bs_icon(
                  "exclamation-triangle"
                )
              ),
              shiny::div(
                class = "tbi-v2-decision-copy",
                shiny::strong(
                  class = "tbi-v2-decision-word",
                  "REVIEW"
                ),
                shiny::p(
                  "No roster data is available for the selected team and season."
                )
              )
            )
          )
        }
        
        weakest_index <- which.min(
          scores$score
        )
        
        weakest_position <- scores$position[
          weakest_index
        ]
        
        weakest_score <- scores$score[
          weakest_index
        ]
        
        overall <- roster_score()
        
        decision <- if (
          overall >= 82 &&
          weakest_score >= 70
        ) {
          "BALANCED"
        } else if (
          weakest_score < 60
        ) {
          paste(
            "ADDRESS",
            weakest_position
          )
        } else if (
          standard_count >= 15
        ) {
          "OPTIMIZE"
        } else {
          "WATCH"
        }
        
        explanation <- if (
          weakest_score < 60
        ) {
          paste0(
            weakest_position,
            " is the primary roster-balance concern at ",
            round(
              weakest_score
            ),
            "/100. Review depth, contract control, and acquisition alternatives."
          )
        } else if (
          overall >= 82
        ) {
          paste(
            "Position groups are broadly stable.",
            "Preserve optionality while monitoring deadline and development opportunities."
          )
        } else {
          paste(
            "The roster is functional but not fully optimized.",
            "Compare targeted upgrades against cost and future control."
          )
        }
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  identical(
                    decision,
                    "BALANCED"
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
                "ROSTER SCORE"
              ),
              shiny::strong(
                paste0(
                  round(overall),
                  "%"
                )
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "PRIMARY WATCH"
              ),
              shiny::strong(
                weakest_position
              ),
              shiny::tags$small(
                paste0(
                  round(
                    weakest_score
                  ),
                  " / 100"
                )
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Position Value 2.0 scorecard
      # ------------------------------------------------------
      
      output$position_value_scorecard <- shiny::renderUI({
        scores <- position_scores()
        
        shiny::tagList(
          lapply(
            seq_len(
              nrow(scores)
            ),
            function(i) {
              
              score <- scores$score[[i]]
              tone <- score_tone(
                score
              )
              
              subtitle <- paste0(
                scores$player_count[[i]],
                " player",
                if (
                  scores$player_count[[i]] == 1
                ) {
                  ""
                } else {
                  "s"
                },
                " • ",
                sprintf(
                  "%.0f%% payroll share",
                  100 *
                    scores$salary_share[[i]]
                )
              )
              
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
                      "people"
                    )
                  ),
                  
                  shiny::div(
                    shiny::strong(
                      scores$position[[i]]
                    ),
                    shiny::tags$small(
                      subtitle
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
                        score,
                        "%;"
                      )
                    )
                  )
                ),
                
                shiny::strong(
                  class = "tbi-v2-score-number",
                  sprintf(
                    "%.0f",
                    score
                  )
                ),
                
                shiny::span(
                  class = "tbi-v2-score-rating",
                  rating_label(
                    score
                  )
                )
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Headlines
      # ------------------------------------------------------
      
      output$roster_headlines <- shiny::renderUI({
        d <- selected_roster()
        scores <- position_scores()
        
        expiring <- sum(
          suppressWarnings(
            as.numeric(
              d$free_agent_year
            )
          ) <= 2028,
          na.rm = TRUE
        )
        
        options <- sum(
          !is.na(
            d$option_type
          ) &
            nzchar(
              trimws(
                as.character(
                  d$option_type
                )
              )
            ),
          na.rm = TRUE
        )
        
        weakest <- scores[
          which.min(
            scores$score
          ),
          ,
          drop = FALSE
        ]
        
        headlines <- c(
          paste0(
            nrow(d),
            " players are currently represented across roster categories."
          ),
          paste0(
            nrow(
              two_way_roster()
            ),
            " two-way contract",
            if (
              nrow(
                two_way_roster()
              ) == 1
            ) {
              ""
            } else {
              "s"
            },
            " are loaded."
          ),
          paste0(
            expiring,
            " contracts reach free agency by 2028."
          ),
          paste0(
            options,
            " current contract-year option flag",
            if (options == 1) "" else "s",
            " are loaded."
          ),
          paste0(
            weakest$position[[1]],
            " is the lowest Position Value 2.0 group at ",
            round(
              weakest$score[[1]]
            ),
            "/100."
          )
        )
        
        shiny::tagList(
          lapply(
            seq_along(headlines),
            function(i) {
              
              colors <- c(
                "",
                "green",
                "orange",
                "purple",
                "orange"
              )
              
              shiny::div(
                class = "tbi-v2-headline-row",
                
                shiny::span(
                  class = paste(
                    "tbi-v2-headline-dot",
                    if (
                      nzchar(
                        colors[[i]]
                      )
                    ) {
                      paste0(
                        "tbi-v2-headline-dot-",
                        colors[[i]]
                      )
                    } else {
                      ""
                    }
                  )
                ),
                
                shiny::span(
                  headlines[[i]]
                )
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Risks
      # ------------------------------------------------------
      
      output$roster_risks <- shiny::renderUI({
        subtab_ready("needs")
        d <- selected_roster()
        scores <- position_scores()
        
        risks <- character()
        
        weak <- scores[
          scores$score < 60,
          ,
          drop = FALSE
        ]
        
        if (nrow(weak)) {
          risks <- c(
            risks,
            paste0(
              paste(
                weak$position,
                collapse = ", "
              ),
              " currently grades below 60 in Position Value 2.0."
            )
          )
        }
        
        payroll <- roster_payroll()
        
        if (payroll > 0) {
          cap_hits <- sort(
            numeric_or_zero(
              d$cap_hit
            ),
            decreasing = TRUE
          )
          
          top3 <- sum(
            utils::head(
              cap_hits,
              3
            ),
            na.rm = TRUE
          )
          
          concentration <- top3 /
            payroll
          
          if (concentration >= .55) {
            risks <- c(
              risks,
              sprintf(
                "Top-three contracts account for %.1f%% of current payroll.",
                100 *
                  concentration
              )
            )
          }
        }
        
        expiring <- sum(
          suppressWarnings(
            as.numeric(
              d$free_agent_year
            )
          ) <= 2028,
          na.rm = TRUE
        )
        
        if (expiring >= 6) {
          risks <- c(
            risks,
            paste0(
              expiring,
              " contracts reach free agency by 2028, creating meaningful near-term turnover."
            )
          )
        }
        
        if (!length(risks)) {
          risks <- "No major structural roster risk is identified by the loaded contract and depth inputs."
        }
        
        shiny::tagList(
          lapply(
            unique(
              risks
            ),
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
      
      # ------------------------------------------------------
      # Opportunities
      # ------------------------------------------------------
      
      output$roster_opportunities <- shiny::renderUI({
        subtab_ready("needs")
        d <- selected_roster()
        scores <- position_scores()
        
        opportunities <- character()
        
        open_spots <- max(
          0,
          15 -
            nrow(
              standard_roster()
            )
        )
        
        if (open_spots > 0) {
          opportunities <- c(
            opportunities,
            paste0(
              open_spots,
              " standard roster spot",
              if (open_spots == 1) "" else "s",
              " remain available in the current roster view."
            )
          )
        }
        
        strong <- scores[
          scores$score >= 80,
          ,
          drop = FALSE
        ]
        
        if (nrow(strong)) {
          opportunities <- c(
            opportunities,
            paste0(
              paste(
                strong$position,
                collapse = ", "
              ),
              " currently provides strong positional depth and optionality."
            )
          )
        }
        
        team_options <- sum(
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
        
        if (team_options > 0) {
          opportunities <- c(
            opportunities,
            paste0(
              team_options,
              " team-controlled option",
              if (team_options == 1) "" else "s",
              " can support future roster flexibility."
            )
          )
        }
        
        if (!length(opportunities)) {
          opportunities <- paste(
            "Preserve flexibility while comparing targeted upgrades",
            "against development and transaction alternatives."
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
      # Composition
      # ------------------------------------------------------
      
      output$roster_composition <- shiny::renderUI({
        d <- selected_roster()
        
        contract_types <- tolower(
          ifelse(
            is.na(
              d$contract_type
            ),
            "",
            d$contract_type
          )
        )
        
        standard <- nrow(
          standard_roster()
        )
        
        two_way <- nrow(
          two_way_roster()
        )
        
        rookie <- sum(
          grepl(
            "rookie",
            contract_types,
            fixed = TRUE
          ),
          na.rm = TRUE
        )
        
        extension <- sum(
          grepl(
            "extension",
            contract_types,
            fixed = TRUE
          ),
          na.rm = TRUE
        )
        
        exhibit <- sum(
          grepl(
            "exhibit",
            contract_types,
            fixed = TRUE
          ),
          na.rm = TRUE
        )
        
        veteran <- sum(
          grepl(
            "veteran",
            contract_types,
            fixed = TRUE
          ),
          na.rm = TRUE
        )
        
        shiny::tagList(
          signal_row(
            "Standard roster",
            as.character(
              standard
            )
          ),
          signal_row(
            "Two-way",
            as.character(
              two_way
            ),
            cba_term = "Two-Way Contract"
          ),
          signal_row(
            "Veteran contracts",
            as.character(
              veteran
            )
          ),
          signal_row(
            "Rookie / rookie scale",
            as.character(
              rookie
            ),
            cba_term = "Rookie Scale Contract"
          ),
          signal_row(
            "Extensions",
            as.character(
              extension
            ),
            cba_term = "Veteran Extension"
          ),
          signal_row(
            "Exhibit contracts",
            as.character(
              exhibit
            ),
            cba_term = "Exhibit 10"
          )
        )
      })
      
      # ------------------------------------------------------
      # Executive assessment
      # ------------------------------------------------------
      
      output$roster_assessment <- shiny::renderUI({
        subtab_ready("decision")
        d <- selected_roster()
        scores <- position_scores()
        
        strongest <- scores[
          which.max(
            scores$score
          ),
          ,
          drop = FALSE
        ]
        
        weakest <- scores[
          which.min(
            scores$score
          ),
          ,
          drop = FALSE
        ]
        
        guaranteed <- sum(
          numeric_or_zero(
            d$guaranteed_amount
          ),
          na.rm = TRUE
        )
        
        total <- roster_payroll()
        
        guarantee_share <- if (
          total > 0
        ) {
          guaranteed /
            total
        } else {
          NA_real_
        }
        
        assessment <- c(
          paste0(
            nrow(d),
            " players are represented in the selected roster view."
          ),
          paste0(
            strongest$position[[1]],
            " has the strongest Position Value 2.0 score at ",
            round(
              strongest$score[[1]]
            ),
            " / 100."
          ),
          paste0(
            weakest$position[[1]],
            " has the lowest Position Value 2.0 score at ",
            round(
              weakest$score[[1]]
            ),
            " / 100 and merits roster review."
          )
        )
        
        if (!is.na(guarantee_share)) {
          assessment <- c(
            assessment,
            sprintf(
              "%.1f%% of current roster cap hits are represented as guaranteed in the loaded contract-year data.",
              100 *
                guarantee_share
            )
          )
        }
        
        shiny::tags$ul(
          class = "roster-assessment-list",
          lapply(
            assessment,
            shiny::tags$li
          )
        )
      })
      
      # ------------------------------------------------------
      # Complete roster table
      # ------------------------------------------------------
      
      output$roster_table <- reactable::renderReactable({
        d <- selected_roster()
        
        shiny::validate(
          shiny::need(
            nrow(d) > 0,
            paste(
              "No roster data is currently available for",
              selected_team(),
              "during",
              selected_season()
            )
          )
        )
        
        cap_hits <- numeric_or_zero(
          d$cap_hit
        )
        
        total_values <- suppressWarnings(
          as.numeric(
            d$total_value
          )
        )
        
        remaining_money <- ifelse(
          is.na(
            total_values
          ),
          NA_real_,
          pmax(
            0,
            total_values -
              numeric_or_zero(
                d$base_salary
              )
          )
        )
        
        roster_category <- ifelse(
          numeric_or_zero(
            d$two_way_flag
          ) == 1 |
            grepl(
              "two-way",
              tolower(
                ifelse(
                  is.na(
                    d$contract_type
                  ),
                  "",
                  d$contract_type
                )
              ),
              fixed = TRUE
            ),
          "Two-Way",
          ifelse(
            grepl(
              "exhibit",
              tolower(
                ifelse(
                  is.na(
                    d$contract_type
                  ),
                  "",
                  d$contract_type
                )
              ),
              fixed = TRUE
            ),
            "Exhibit 10",
            "Standard"
          )
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
          Age = ifelse(
            is.na(
              d$player_age
            ),
            "—",
            as.character(
              d$player_age
            )
          ),
          `Cap Hit` = vapply(
            cap_hits,
            money,
            character(1)
          ),
          `Remaining Money` = vapply(
            remaining_money,
            money,
            character(1)
          ),
          `Contract Through` = ifelse(
            is.na(
              d$contract_end_season
            ),
            "—",
            d$contract_end_season
          ),
          Contract = ifelse(
            is.na(
              d$contract_type
            ),
            "Not classified",
            d$contract_type
          ),
          Category = roster_category,
          `Bird Rights` = ifelse(
            is.na(
              d$bird_rights
            ),
            "—",
            d$bird_rights
          ),
          Option = ifelse(
            is.na(
              d$option_type
            ) |
              !nzchar(
                trimws(
                  as.character(
                    d$option_type
                  )
                )
              ),
            "—",
            d$option_type
          ),
          `FA Year` = ifelse(
            is.na(
              d$free_agent_year
            ),
            "—",
            d$free_agent_year
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        reactable::reactable(
          display,
          searchable = TRUE,
          highlight = TRUE,
          striped = FALSE,
          compact = TRUE,
          pagination = TRUE,
          defaultPageSize = 12,
          defaultSorted = "Cap Hit",
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
          ),
          columns = list(
            Contract = reactable::colDef(
              cell = function(value) {
                roster_table_link(
                  value,
                  roster_cba_term(
                    value,
                    "contract"
                  )
                )
              }
            ),
            Category = reactable::colDef(
              cell = function(value) {
                roster_table_link(
                  value,
                  roster_cba_term(
                    value,
                    "category"
                  )
                )
              }
            ),
            `Bird Rights` = reactable::colDef(
              cell = function(value) {
                roster_table_link(
                  value,
                  roster_cba_term(
                    value,
                    "bird"
                  )
                )
              }
            ),
            Option = reactable::colDef(
              cell = function(value) {
                roster_table_link(
                  value,
                  roster_cba_term(
                    value,
                    "option"
                  )
                )
              }
            )
          )
        )
      })
    }
  )
}


# ============================================================
# ROSTER INTELLIGENCE — SAFE_NUM HEALTHCHECK
# ============================================================

roster_intelligence_safe_num_healthcheck <- function() {
  
  body_text <- paste(
    deparse(
      body(
        mod_roster_contracts_server
      )
    ),
    collapse = "\n"
  )
  
  list(
    module = "Roster Intelligence",
    status = if (
      grepl(
        "safe_num <- function",
        body_text,
        fixed = TRUE
      )
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    fix =
      "LOCAL safe_num() IS DEFINED INSIDE mod_roster_contracts_server"
  )
}
