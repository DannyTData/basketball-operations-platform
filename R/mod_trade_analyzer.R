# PHASE 15I v3 — NBA DRAFT TRADE RULES ENGINE
# PHASE 15H — DRAFT ASSETS IN TRADE INTELLIGENCE
# ============================================================
# PHASE 2 STEP 13 — FINAL INTEGRATION / QA
# Trade Intelligence — UI FREEZE PRESERVED
# Stable checkpoint: no visual redesign in this pass.
# ============================================================

# ============================================================
# TBI NBA Basketball Operations Platform
# TRADE INTELLIGENCE — UI FREEZE
#
# Frozen features:
#   - balanced organization panels
#   - expanded center Trade Hub
#   - fully dynamic organization labels
#   - CBA screening
#   - shared transaction state
#   - BIE Trade Basketball Impact
#   - lazy/cached BIE execution
# ============================================================

# ------------------------------------------------------------
# Module: Trade Intelligence
# Version 2.3 UI FREEZE — Unified Executive Trade Recommendation
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Trade Intelligence UI
#'
#' @param id Internal module ID.
#' @noRd
mod_trade_analyzer_ui <- function(id, builder_only = FALSE) {
  ns <- shiny::NS(id)
  
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
  
  ui <- shiny::div(
    class = "tbi-module-page tbi-v2-trade-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-v2-trade-page .trade-bie-panel {
          margin-top:14px;
          border:1px solid rgba(96,165,250,.18);
          border-radius:12px;
          overflow:hidden;
          background:
            linear-gradient(
              145deg,
              rgba(12,27,45,.98),
              rgba(8,18,30,.98)
            );
        }

        .tbi-v2-trade-page .trade-bie-head {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          padding:12px 15px;
          border-bottom:1px solid rgba(148,163,184,.09);
        }

        .tbi-v2-trade-page .trade-bie-title {
          display:flex;
          align-items:center;
          gap:8px;
          color:#edf4fb;
          font-size:.72rem;
          font-weight:900;
        }

        .tbi-v2-trade-page .trade-bie-chip {
          display:inline-flex;
          align-items:center;
          padding:4px 7px;
          border:1px solid rgba(96,165,250,.22);
          border-radius:999px;
          background:rgba(59,130,246,.07);
          color:#8fc0ff;
          font-size:.45rem;
          font-weight:900;
          letter-spacing:.07em;
        }

        .tbi-v2-trade-page .trade-bie-grid {
          display:grid;
          grid-template-columns:1.05fr 1fr 1fr 1fr;
        }

        .tbi-v2-trade-page .trade-bie-block {
          min-width:0;
          padding:13px 15px;
          border-right:1px solid rgba(148,163,184,.08);
        }

        .tbi-v2-trade-page .trade-bie-block:last-child {
          border-right:0;
        }

        .tbi-v2-trade-page .trade-bie-label {
          display:block;
          margin-bottom:7px;
          color:#70849e;
          font-size:.44rem;
          font-weight:900;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .trade-bie-value {
          color:#eef4fb;
          font-size:1rem;
          font-weight:900;
        }

        .tbi-v2-trade-page .trade-bie-value.positive {
          color:#34d399;
        }

        .tbi-v2-trade-page .trade-bie-value.warning {
          color:#fbbf24;
        }

        .tbi-v2-trade-page .trade-bie-sub {
          display:block;
          margin-top:4px;
          color:#8296af;
          font-size:.49rem;
          line-height:1.4;
        }

        .tbi-v2-trade-page .trade-bie-summary {
          padding:10px 15px 12px;
          border-top:1px solid rgba(148,163,184,.08);
          color:#8fa2b9;
          font-size:.52rem;
          line-height:1.5;
        }

        .tbi-v2-trade-page .trade-bie-summary strong {
          color:#60a5fa;
          font-weight:900;
        }


        .tbi-v2-trade-page .trade-bie-target-fit {
          padding:12px 15px 14px;
          border-top:1px solid rgba(148,163,184,.08);
          background:rgba(59,130,246,.018);
        }

        .tbi-v2-trade-page .trade-bie-target-head {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:10px;
          margin-bottom:9px;
        }

        .tbi-v2-trade-page .trade-bie-target-title {
          color:#dce9f8;
          font-size:.58rem;
          font-weight:900;
          letter-spacing:.06em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .trade-bie-target-need {
          color:#8fc0ff;
          font-size:.48rem;
          font-weight:850;
        }

        .tbi-v2-trade-page .trade-bie-target-grid {
          display:grid;
          grid-template-columns:
            minmax(150px,1.25fr)
            80px 92px 92px 92px
            minmax(180px,1.4fr);
          align-items:center;
        }

        .tbi-v2-trade-page .trade-bie-target-cell {
          min-width:0;
          padding:7px 8px;
          border-top:1px solid rgba(148,163,184,.06);
          color:#9fb0c5;
          font-size:.49rem;
        }

        .tbi-v2-trade-page .trade-bie-target-header {
          border-top:0;
          color:#687d97;
          font-size:.41rem;
          font-weight:900;
          letter-spacing:.06em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .trade-bie-target-player {
          color:#edf4fb;
          font-weight:850;
        }

        .tbi-v2-trade-page .trade-bie-target-score {
          color:#34d399;
          font-weight:900;
        }

        @media(max-width:900px) {
          .tbi-v2-trade-page .trade-bie-target-grid {
            grid-template-columns:
              minmax(125px,1.2fr)
              70px 80px 80px 80px
              minmax(150px,1.2fr);
            overflow-x:auto;
          }
        }

        @media(max-width:1050px) {
          .tbi-v2-trade-page .trade-bie-grid {
            grid-template-columns:1fr 1fr;
          }

          .tbi-v2-trade-page .trade-bie-block:nth-child(2) {
            border-right:0;
          }

          .tbi-v2-trade-page .trade-bie-block:nth-child(-n+2) {
            border-bottom:1px solid rgba(148,163,184,.08);
          }
        }

        @media(max-width:650px) {
          .tbi-v2-trade-page .trade-bie-grid {
            grid-template-columns:1fr;
          }

          .tbi-v2-trade-page .trade-bie-block {
            border-right:0;
            border-bottom:1px solid rgba(148,163,184,.08);
          }
        }


        /* ==================================================
           TRADE INTELLIGENCE UI FREEZE
           Balanced team panels + expanded center control hub
           ================================================== */

        .tbi-v2-trade-page .tbi-trade-workspace-grid {
          display:grid;
          grid-template-columns:
            minmax(0, 1.08fr)
            minmax(260px, .84fr)
            minmax(0, 1.08fr);
          gap:14px;
          align-items:stretch;
          margin-top:4px;
        }

        .tbi-v2-trade-page .tbi-trade-team-panel,
        .tbi-v2-trade-page .tbi-trade-center-panel {
          min-width:0;
          min-height:430px;
        }

        .tbi-v2-trade-page .tbi-trade-team-panel {
          display:flex;
          flex-direction:column;
          padding:16px;
        }

        .tbi-v2-trade-page .tbi-trade-team-head {
          display:flex;
          align-items:flex-start;
          justify-content:space-between;
          gap:12px;
          min-height:74px;
          margin-bottom:10px;
        }

        .tbi-v2-trade-page .tbi-trade-team-identity {
          min-width:0;
        }

        .tbi-v2-trade-page .tbi-trade-team-label {
          display:block;
          margin-bottom:5px;
          color:#6f849e;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .tbi-trade-team-name {
          margin:0;
          color:#f1f5f9;
          font-size:1rem;
          font-weight:900;
          line-height:1.2;
        }

        .tbi-v2-trade-page .tbi-trade-team-subtitle {
          margin:5px 0 0;
          color:#8fa5c2;
          font-size:.68rem;
          line-height:1.35;
        }

        .tbi-v2-trade-page .tbi-trade-partner-select {
          flex:0 0 190px;
          width:190px;
        }

        .tbi-v2-trade-page .tbi-trade-partner-select
        .shiny-input-container,
        .tbi-v2-trade-page .tbi-trade-partner-select
        .form-group {
          width:100% !important;
          margin:0 !important;
        }

        .tbi-v2-trade-page .tbi-trade-partner-select label {
          display:none !important;
        }

        .tbi-v2-trade-page .tbi-trade-player-scroll {
          flex:1 1 auto;
          min-height:0;
          max-height:330px;
          overflow-y:auto;
          padding-right:6px;
          border-top:1px solid rgba(148,163,184,.07);
          border-bottom:1px solid rgba(148,163,184,.07);
        }

        .tbi-v2-trade-page .tbi-trade-team-panel
        .tbi-trade-total {
          margin-top:auto;
          padding-top:13px;
        }

        .tbi-v2-trade-page .tbi-trade-center-panel {
          display:flex;
          flex-direction:column;
          justify-content:center;
          padding:16px 14px;
          border:1px solid rgba(96,165,250,.16);
          border-radius:12px;
          background:
            linear-gradient(
              180deg,
              rgba(13,26,43,.96),
              rgba(8,18,30,.98)
            );
        }

        .tbi-v2-trade-page .tbi-trade-hub-title {
          margin-bottom:14px;
          text-align:center;
          color:#60a5fa;
          font-size:.54rem;
          font-weight:900;
          letter-spacing:.10em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .tbi-trade-matchup {
          display:grid;
          grid-template-columns:minmax(0,1fr) 64px minmax(0,1fr);
          gap:10px;
          align-items:center;
          width:100%;
        }

        .tbi-v2-trade-page .tbi-trade-matchup-team {
          min-width:0;
          text-align:center;
        }

        .tbi-v2-trade-page .tbi-trade-abbr {
          width:50px;
          height:50px;
          margin:0 auto 7px;
          display:grid;
          place-items:center;
          border:1px solid rgba(59,130,246,.30);
          border-radius:50%;
          background:rgba(59,130,246,.07);
          color:#60a5fa;
          font-size:.78rem;
          font-weight:900;
        }

        .tbi-v2-trade-page
        .tbi-trade-matchup-team.partner
        .tbi-trade-abbr {
          border-color:rgba(167,139,250,.30);
          background:rgba(167,139,250,.07);
          color:#a78bfa;
        }

        .tbi-v2-trade-page .tbi-trade-matchup-name {
          display:block;
          min-height:34px;
          color:#dce8f7;
          font-size:.60rem;
          font-weight:850;
          line-height:1.35;
        }

        .tbi-v2-trade-page
        .tbi-trade-matchup-team.partner
        .tbi-trade-matchup-name {
          color:#b69cff;
        }

        .tbi-v2-trade-page .tbi-trade-path-label {
          display:block;
          margin-top:17px;
          text-align:center;
          color:#6f849d;
          font-size:.44rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .tbi-v2-trade-page .tbi-trade-path {
          display:block;
          margin:6px 0 15px;
          text-align:center;
          color:#67a9ff;
          font-size:.70rem;
          font-weight:900;
          line-height:1.45;
        }

        .tbi-v2-trade-page .tbi-trade-center-summary {
          display:grid;
          grid-template-columns:1fr 1fr;
          border:1px solid rgba(96,165,250,.12);
          border-radius:9px;
          overflow:hidden;
          background:rgba(4,13,24,.35);
        }

        .tbi-v2-trade-page .tbi-trade-center-metric {
          min-width:0;
          padding:11px 9px;
          text-align:center;
        }

        .tbi-v2-trade-page
        .tbi-trade-center-metric:nth-child(odd) {
          border-right:1px solid rgba(148,163,184,.08);
        }

        .tbi-v2-trade-page
        .tbi-trade-center-metric:nth-child(n+3) {
          border-top:1px solid rgba(148,163,184,.08);
        }

        .tbi-v2-trade-page .tbi-trade-center-metric span {
          display:block;
          color:#6f839c;
          font-size:.42rem;
          font-weight:900;
          letter-spacing:.07em;
          text-transform:uppercase;
          line-height:1.3;
        }

        .tbi-v2-trade-page .tbi-trade-center-metric strong {
          display:block;
          margin-top:5px;
          color:#eaf2fb;
          font-size:.82rem;
          font-weight:900;
        }

        .tbi-v2-trade-page .tbi-trade-center-metric.outgoing strong {
          color:#60a5fa;
        }

        .tbi-v2-trade-page .tbi-trade-center-metric.incoming strong {
          color:#a78bfa;
        }

        .tbi-v2-trade-page .tbi-trade-center-metric.delta strong {
          color:#fbbf24;
        }

        .tbi-v2-trade-page .tbi-trade-center-metric.cba {
          grid-column:1 / -1;
          border-right:0 !important;
          border-top:1px solid rgba(148,163,184,.08);
        }

        @media(max-width:1180px) {
          .tbi-v2-trade-page .tbi-trade-workspace-grid {
            grid-template-columns:minmax(0,1fr) minmax(245px,.78fr) minmax(0,1fr);
          }

          .tbi-v2-trade-page .tbi-trade-partner-select {
            flex-basis:160px;
            width:160px;
          }
        }

        @media(max-width:980px) {
          .tbi-v2-trade-page .tbi-trade-workspace-grid {
            grid-template-columns:1fr;
          }

          .tbi-v2-trade-page .tbi-trade-team-panel,
          .tbi-v2-trade-page .tbi-trade-center-panel {
            min-height:auto;
          }

          .tbi-v2-trade-page .tbi-trade-center-panel {
            order:2;
          }

          .tbi-v2-trade-page .tbi-trade-team-panel:first-child {
            order:1;
          }

          .tbi-v2-trade-page .tbi-trade-team-panel:last-child {
            order:3;
          }
        }

        @media(max-width:620px) {
          .tbi-v2-trade-page .tbi-trade-team-head {
            flex-direction:column;
            min-height:auto;
          }

          .tbi-v2-trade-page .tbi-trade-partner-select {
            width:100%;
            flex-basis:auto;
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
          "TRANSACTION STRATEGY"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Transaction Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Build a two-team transaction, screen salary matching,",
            "and surface apron and aggregation restrictions."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "CBA SCREEN"
        ),
        shiny::strong("Phase 3")
      )
    ),
    
    # --------------------------------------------------------
    # Scenario snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot",
      
      shiny::div(
        class = "tbi-v2-section-title-row",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("arrow-left-right")
          ),
          shiny::span("SCENARIO SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "LIVE SCREEN"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "OUTGOING",
          "snapshot_outgoing_salary",
          "cash-stack",
          "blue"
        ),
        
        snapshot_item(
          "INCOMING",
          "snapshot_incoming_salary",
          "cash-stack",
          "blue"
        ),
        
        snapshot_item(
          "SALARY DELTA",
          "snapshot_salary_difference",
          "graph-up-arrow",
          "orange"
        ),
        
        snapshot_item(
          "DRAFT VALUE Δ",
          "snapshot_draft_value_delta",
          "graph-up-arrow",
          "purple"
        ),

        snapshot_item(
          "DRAFT SCREEN",
          "snapshot_draft_status",
          "exclamation-triangle",
          "orange"
        ),

        snapshot_item(
          "YOUR STATUS",
          "snapshot_team_a_status",
          "currency-dollar",
          "blue"
        ),
        
        snapshot_item(
          "PARTNER STATUS",
          "snapshot_team_b_status",
          "currency-dollar",
          "purple"
        ),
        
        snapshot_item(
          "CBA RESULT",
          "snapshot_cba_result",
          "exclamation-triangle",
          "orange"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Trade builder workspace — UI FREEZE
    # Balanced organization panels + central transaction hub
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-trade-workspace-grid",
      
      # ------------------------------------------------------
      # YOUR ORGANIZATION
      # ------------------------------------------------------
      
      shiny::div(
        class = "tbi-panel tbi-framework-panel tbi-trade-team-panel",
        
        shiny::div(
          class = "tbi-trade-team-head",
          
          shiny::div(
            class = "tbi-trade-team-identity",
            
            shiny::span(
              class = "tbi-trade-team-label",
              "YOUR ORGANIZATION"
            ),
            
            shiny::h3(
              class = "tbi-trade-team-name",
              shiny::textOutput(
                ns("team_a_name"),
                inline = TRUE
              )
            ),
            
            shiny::p(
              class = "tbi-trade-team-subtitle",
              "Select outgoing players"
            )
          )
        ),
        
        shiny::div(
          class = "tbi-trade-player-scroll",
          shiny::uiOutput(
            ns("team_a_players")
          )
        ),

        shiny::p(
          style = "margin-top:14px;",
          "Select outgoing draft assets"
        ),

        shiny::div(
          style = "max-height:190px; overflow-y:auto; padding-right:6px;",
          shiny::uiOutput(
            ns("team_a_draft_assets")
          )
        ),

        shiny::uiOutput(
          ns("team_a_draft_summary")
        ),
        
        shiny::div(
          class = "tbi-trade-total",
          shiny::span(
            "Outgoing salary"
          ),
          shiny::strong(
            shiny::textOutput(
              ns("outgoing_salary"),
              inline = TRUE
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # TRADE HUB
      # ------------------------------------------------------
      
      shiny::div(
        class = "tbi-trade-center-panel",
        
        shiny::div(
          class = "tbi-trade-hub-title",
          "TRADE HUB"
        ),
        
        shiny::div(
          class = "tbi-trade-matchup",
          
          shiny::div(
            class = "tbi-trade-matchup-team",
            
            shiny::div(
              class = "tbi-trade-abbr",
              shiny::textOutput(
                ns("team_a_abbr"),
                inline = TRUE
              )
            ),
            
            shiny::strong(
              class = "tbi-trade-matchup-name",
              shiny::textOutput(
                ns("team_a_short_name"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-v2-decision-symbol",
            style = "width:64px; height:64px; flex-basis:64px;",
            bsicons::bs_icon(
              "arrow-left-right"
            )
          ),
          
          shiny::div(
            class = "tbi-trade-matchup-team partner",
            
            shiny::div(
              class = "tbi-trade-abbr",
              shiny::textOutput(
                ns("team_b_abbr"),
                inline = TRUE
              )
            ),
            
            shiny::strong(
              class = "tbi-trade-matchup-name",
              shiny::textOutput(
                ns("team_b_short_name"),
                inline = TRUE
              )
            )
          )
        ),
        
        shiny::span(
          class = "tbi-trade-path-label",
          "TRANSACTION PATH"
        ),
        
        shiny::strong(
          class = "tbi-trade-path",
          shiny::textOutput(
            ns("scenario_label"),
            inline = TRUE
          )
        ),
        
        shiny::div(
          class = "tbi-trade-center-summary",
          
          shiny::div(
            class = "tbi-trade-center-metric outgoing",
            shiny::span(
              shiny::textOutput(
                ns("center_team_a_sends_label"),
                inline = TRUE
              )
            ),
            shiny::strong(
              shiny::textOutput(
                ns("center_outgoing_salary"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-trade-center-metric incoming",
            shiny::span(
              shiny::textOutput(
                ns("center_team_b_sends_label"),
                inline = TRUE
              )
            ),
            shiny::strong(
              shiny::textOutput(
                ns("center_incoming_salary"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-trade-center-metric delta",
            shiny::span(
              "SALARY DELTA"
            ),
            shiny::strong(
              shiny::textOutput(
                ns("salary_difference"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-trade-center-metric",
            shiny::span(
              "YOUR CAP STATUS"
            ),
            shiny::strong(
              shiny::textOutput(
                ns("center_team_a_status"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-trade-center-metric cba",
            shiny::span(
              "CBA STATUS"
            ),
            shiny::strong(
              shiny::textOutput(
                ns("center_cba_result"),
                inline = TRUE
              )
            )
          )
        ),

        if (isTRUE(builder_only)) {
          shiny::div(
            class = "tbi-trade-evaluate-action",
            style = "margin-top:14px; display:grid; gap:8px;",
            shiny::actionButton(
              ns("evaluate_trade"),
              "Evaluate Trade",
              class = "btn-primary"
            ),
            shiny::uiOutput(ns("evaluate_trade_status"))
          )
        }
      ),
      
      # ------------------------------------------------------
      # TRADE PARTNER
      # ------------------------------------------------------
      
      shiny::div(
        class = "tbi-panel tbi-framework-panel tbi-trade-team-panel",
        
        shiny::div(
          class = "tbi-trade-team-head",
          
          shiny::div(
            class = "tbi-trade-team-identity",
            
            shiny::span(
              class = "tbi-trade-team-label",
              "TRADE PARTNER"
            ),
            
            shiny::h3(
              class = "tbi-trade-team-name",
              shiny::textOutput(
                ns("team_b_panel_short_name"),
                inline = TRUE
              )
            ),
            
            shiny::p(
              class = "tbi-trade-team-subtitle",
              "Select incoming players"
            )
          ),
          
          shiny::div(
            class = "tbi-trade-partner-select",
            
            shiny::selectInput(
              ns("partner_team"),
              label = NULL,
              choices = NULL
            )
          )
        ),
        
        shiny::div(
          class = "tbi-trade-player-scroll",
          shiny::uiOutput(
            ns("team_b_players")
          )
        ),

        shiny::p(
          style = "margin-top:14px;",
          "Select incoming draft assets"
        ),

        shiny::div(
          style = "max-height:190px; overflow-y:auto; padding-right:6px;",
          shiny::uiOutput(
            ns("team_b_draft_assets")
          )
        ),

        shiny::uiOutput(
          ns("team_b_draft_summary")
        ),
        
        shiny::div(
          class = "tbi-trade-total",
          shiny::span(
            "Incoming salary"
          ),
          shiny::strong(
            shiny::textOutput(
              ns("incoming_salary"),
              inline = TRUE
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Decision + CBA scorecard
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-main-grid",
      
      shiny::tags$section(
        class = "tbi-v2-decision-card",
        
        shiny::div(
          class = "tbi-v2-section-title",
          
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-warning",
            bsicons::bs_icon(
              "exclamation-triangle"
            )
          ),
          
          shiny::span(
            "TRANSACTION DECISION"
          )
        ),
        
        shiny::uiOutput(
          ns("transaction_decision")
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
              bsicons::bs_icon(
                "graph-up-arrow"
              )
            ),
            shiny::span(
              "CBA RULE SCORECARD"
            )
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Screen"),
            shiny::strong(
              shiny::textOutput(
                ns("scorecard_cba_result"),
                inline = TRUE
              )
            )
          )
        ),
        
        shiny::uiOutput(
          ns("cba_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # BIE Trade Basketball Impact
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "trade-bie-panel",
      
      shiny::div(
        class = "trade-bie-head",
        
        shiny::div(
          class = "trade-bie-title",
          bsicons::bs_icon(
            "activity"
          ),
          "BIE TRADE BASKETBALL IMPACT"
        ),
        
        shiny::span(
          class = "trade-bie-chip",
          "PHASE 2"
        )
      ),
      
      shiny::uiOutput(
        ns("bie_trade_impact")
      ),
      
      shiny::uiOutput(
        ns("bie_target_fit")
      )
    ),
    
    # --------------------------------------------------------
    # Executive recommendation
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "EXECUTIVE RECOMMENDATION"
          ),
          shiny::h3(
            "Recommended transaction action"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "DECISION"
        )
      ),
      
      shiny::div(
        style = paste(
          "padding:16px; display:grid;",
          "grid-template-columns:minmax(190px,.45fr) minmax(0,1.55fr);",
          "gap:18px; align-items:center;"
        ),
        
        shiny::uiOutput(
          ns("executive_recommendation_badge")
        ),
        
        shiny::uiOutput(
          ns("executive_recommendation_copy")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Alerts / risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-bottom-grid",
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-headlines-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon(
              "currency-dollar"
            )
          ),
          shiny::span(
            "CBA ALERTS"
          )
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
            bsicons::bs_icon(
              "exclamation-triangle"
            )
          ),
          shiny::span(
            "TRANSACTION RISKS"
          )
        ),
        
        shiny::uiOutput(
          ns("trade_risks")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-opportunities-panel",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon tbi-v2-section-icon-success",
            bsicons::bs_icon(
              "bullseye"
            )
          ),
          shiny::span(
            "TRANSACTION OPPORTUNITIES"
          )
        ),
        
        shiny::uiOutput(
          ns("trade_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Detailed CBA readout
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "CBA SCREEN RESULT"
          ),
          shiny::h3(
            "Two-team transaction readout"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "FIRST-PASS SCREEN"
        )
      ),
      
      shiny::div(
        style = "padding: 16px;",
        shiny::uiOutput(
          ns("trade_readout")
        )
      )
    )
  )

  if (isTRUE(builder_only)) {
    keep <- vapply(ui$children, function(child) {
      if (!inherits(child, "shiny.tag")) return(FALSE)
      identical(child$name, "style") || grepl("tbi-trade-workspace-grid", htmltools::tagGetAttribute(child, "class") %||% "", fixed = TRUE)
    }, logical(1))
    ui$children <- ui$children[keep]
  }

  ui
}


bind_trade_text_output_mirror <- function(
    output,
    canonical_id,
    mirror_id,
    text_value) {
  output[[canonical_id]] <- shiny::renderText({
    text_value()
  })
  output[[mirror_id]] <- shiny::renderText({
    text_value()
  })

  invisible(NULL)
}


# ============================================================
# SERVER
# ============================================================

#' Trade Intelligence server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @param builder_only Whether the module is mounted as the V2 builder-only route.
#' @noRd
mod_trade_analyzer_server <- function(
    id,
    selected_team,
    selected_season,
    transaction_state = NULL,
    builder_only = FALSE) {
  
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
      
      selected_ids <- function(x) {
        values <- suppressWarnings(
          as.integer(
            x %||% character()
          )
        )
        
        values[
          !is.na(values)
        ]
      }
      
      player_choices <- function(d) {
        if (
          is.null(d) ||
          !nrow(d)
        ) {
          return(
            stats::setNames(
              character(),
              character()
            )
          )
        }
        
        labels <- paste0(
          d$player_name,
          " — ",
          vapply(
            d$cap_hit,
            money,
            character(1)
          )
        )
        
        stats::setNames(
          as.character(
            d$player_id
          ),
          labels
        )
      }
      
      empty_players <- function() {
        data.frame(
          player_id = integer(),
          player_name = character(),
          cap_hit = numeric(),
          stringsAsFactors = FALSE
        )
      }
      
      safe_eval <- function(expr) {
        tryCatch(
          expr,
          error = function(e) {
            structure(
              list(
                message = conditionMessage(e)
              ),
              class = "tbi_trade_error"
            )
          }
        )
      }
      
      score_row <- function(
    label,
    subtitle,
    icon,
    passed,
    detail = NULL,
    review = FALSE,
    cba_term = NULL) {
        
        if (isTRUE(review)) {
          tone <- "orange"
          score <- 50
          rating <- "Review"
        } else if (isTRUE(passed)) {
          tone <- "green"
          score <- 100
          rating <- "Pass"
        } else {
          tone <- "red"
          score <- 20
          rating <- "Fail"
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
              bsicons::bs_icon(
                icon
              )
            ),
            
            shiny::div(
              shiny::strong(
                if (
                  !is.null(cba_term) &&
                  nzchar(as.character(cba_term)) &&
                  exists(
                    "tbi_cba_link",
                    mode = "function"
                  )
                ) {
                  tbi_cba_link(
                    term = cba_term,
                    label = label
                  )
                } else {
                  shiny::span(label)
                }
              ),
              shiny::tags$small(
                if (
                  is.null(detail) ||
                  !nzchar(
                    as.character(
                      detail
                    )
                  )
                ) {
                  subtitle
                } else {
                  paste0(
                    subtitle,
                    " • ",
                    detail
                  )
                }
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
            rating
          ),
          
          shiny::span(
            class = "tbi-v2-score-rating",
            if (isTRUE(review)) {
              "Manual"
            } else if (isTRUE(passed)) {
              "Cleared"
            } else {
              "Blocked"
            }
          )
        )
      }
      
      signal_row <- function(
    label,
    value,
    cba_term = NULL) {
        
        rendered_value <- if (
          !is.null(cba_term) &&
          nzchar(as.character(cba_term)) &&
          exists(
            "tbi_cba_link",
            mode = "function"
          )
        ) {
          tbi_cba_link(
            term = cba_term,
            label = value
          )
        } else {
          shiny::span(value)
        }
        
        shiny::div(
          class = "tbi-signal-row",
          shiny::span(
            label
          ),
          shiny::strong(
            rendered_value
          )
        )
      }
      
      cba_status_term <- function(status) {
        
        status <- as.character(
          status %||% ""
        )
        
        if (
          grepl(
            "Second Apron",
            status,
            fixed = TRUE
          )
        ) {
          return("Second Apron")
        }
        
        if (
          grepl(
            "First Apron",
            status,
            fixed = TRUE
          )
        ) {
          return("First Apron")
        }
        
        if (
          grepl(
            "Tax",
            status,
            ignore.case = TRUE
          )
        ) {
          return("Luxury Tax")
        }
        
        if (
          grepl(
            "Cap",
            status,
            ignore.case = TRUE
          )
        ) {
          return("Salary Cap")
        }
        
        NULL
      }
      
      # ------------------------------------------------------
      # Team choices
      # ------------------------------------------------------
      
      teams <- get_teams()
      
      shiny::observe({
        shiny::req(
          selected_team()
        )
        
        partner_names <- teams$team_name[
          teams$team_name !=
            selected_team()
        ]
        
        choices <- stats::setNames(
          partner_names,
          partner_names
        )

        choices <- c("Select organization" = "", choices)
        
        selected_value <- if (
          length(
            choices
          )
        ) {
          choices[[2]]
        } else {
          character()
        }
        
        shiny::updateSelectInput(
          session,
          "partner_team",
          choices = choices,
          selected = selected_value
        )
      })
      
      # ------------------------------------------------------
      # Player pools
      # ------------------------------------------------------
      
      team_a_pool <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        get_trade_player_pool(
          selected_team(),
          selected_season()
        )
      })
      
      team_b_pool <- shiny::reactive({
        shiny::req(
          input$partner_team,
          selected_season()
        )
        
        get_trade_player_pool(
          input$partner_team,
          selected_season()
        )
      })
      
      team_a_selected <- shiny::reactive({
        d <- team_a_pool()
        
        ids <- selected_ids(
          input$outgoing_players
        )
        
        if (!length(ids)) {
          return(
            d[
              FALSE,
              ,
              drop = FALSE
            ]
          )
        }
        
        d[
          d$player_id %in% ids,
          ,
          drop = FALSE
        ]
      })
      
      team_b_selected <- shiny::reactive({
        d <- team_b_pool()
        
        ids <- selected_ids(
          input$incoming_players
        )
        
        if (!length(ids)) {
          return(
            d[
              FALSE,
              ,
              drop = FALSE
            ]
          )
        }
        
        d[
          d$player_id %in% ids,
          ,
          drop = FALSE
        ]
      })
      
      # ------------------------------------------------------
      # Phase 15H — Draft asset pools
      # ------------------------------------------------------

      team_a_draft_pool <- shiny::reactive({

        shiny::req(
          selected_team()
        )

        tbi_trade_selectable_draft_assets(
          selected_team()
        )
      })


      team_b_draft_pool <- shiny::reactive({

        shiny::req(
          input$partner_team
        )

        tbi_trade_selectable_draft_assets(
          input$partner_team
        )
      })


      team_a_draft_selected <- shiny::reactive({

        tbi_trade_selected_draft_assets(
          team_a_draft_pool(),
          input$outgoing_draft_assets
        )
      })


      team_b_draft_selected <- shiny::reactive({

        tbi_trade_selected_draft_assets(
          team_b_draft_pool(),
          input$incoming_draft_assets
        )
      })


      draft_screen <- shiny::reactive({

        tbi_trade_draft_screen(
          outgoing_assets =
            team_a_draft_selected(),
          incoming_assets =
            team_b_draft_selected(),
          team_a =
            selected_team(),
          team_b =
            input$partner_team,
          season =
            selected_season()
        )

      })


      outgoing_salary_value <- shiny::reactive({
        sum(
          team_a_selected()$cap_hit,
          na.rm = TRUE
        )
      })
      
      incoming_salary_value <- shiny::reactive({
        sum(
          team_b_selected()$cap_hit,
          na.rm = TRUE
        )
      })
      
      
      # ------------------------------------------------------
      # BIE Trade Basketball Impact
      # ------------------------------------------------------
      
      bie_trade_current_roster <- shiny::reactive({
        
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        if (
          !exists(
            "get_depth_chart_records",
            mode = "function"
          )
        ) {
          return(
            data.frame()
          )
        }
        
        tryCatch(
          get_depth_chart_records(
            selected_team(),
            selected_season()
          ),
          error = function(e) {
            data.frame()
          }
        )
      })
      
      
      bie_trade_current_lineup <- shiny::reactive({
        
        d <- bie_trade_current_roster()
        
        if (
          !nrow(d) ||
          !exists(
            "bie_lineup_from_depth_chart",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        bie_lineup_from_depth_chart(
          d
        )
      })
      
      
      bie_trade_impact_cache <- shiny::reactiveVal(
        list(
          key = NULL,
          value = NULL
        )
      )
      
      
      bie_trade_impact_result <- shiny::reactive({
        
        # IMPORTANT: inspect the lightweight selection reactives FIRST.
        # Do not query/evaluate the current roster until a genuine
        # two-sided trade exists.
        outgoing <- team_a_selected()
        incoming <- team_b_selected()
        
        if (
          !nrow(outgoing) ||
          !nrow(incoming) ||
          !exists(
            "evaluate_bie_trade_basketball_impact",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        current <- bie_trade_current_roster()
        
        if (!nrow(current)) {
          return(NULL)
        }
        
        outgoing_ids <- if (
          "player_id" %in%
          names(outgoing)
        ) {
          sort(
            suppressWarnings(
              as.integer(
                outgoing$player_id
              )
            )
          )
        } else {
          integer()
        }
        
        incoming_ids <- if (
          "player_id" %in%
          names(incoming)
        ) {
          sort(
            suppressWarnings(
              as.integer(
                incoming$player_id
              )
            )
          )
        } else {
          integer()
        }
        
        roster_key <- if (
          exists(
            "bie_roster_signature",
            mode = "function"
          )
        ) {
          bie_roster_signature(current)
        } else {
          as.character(
            nrow(current)
          )
        }
        
        key <- paste(
          selected_team(),
          selected_season(),
          roster_key,
          paste(
            outgoing_ids,
            collapse = ","
          ),
          paste(
            incoming_ids,
            collapse = ","
          ),
          sep = "||"
        )
        
        cached <- shiny::isolate(bie_trade_impact_cache())
        
        if (
          !is.null(cached$key) &&
          identical(
            cached$key,
            key
          ) &&
          !is.null(cached$value)
        ) {
          return(
            cached$value
          )
        }
        
        result <- tryCatch(
          evaluate_bie_trade_basketball_impact(
            current_players =
              current,
            outgoing_players =
              outgoing,
            incoming_players =
              incoming,
            current_lineup =
              bie_trade_current_lineup(),
            rotation_size = 9L
          ),
          error = function(e) {
            list(
              status = "ERROR",
              confidence = "UNAVAILABLE",
              explanation =
                conditionMessage(e)
            )
          }
        )
        
        bie_trade_impact_cache(
          list(
            key = key,
            value = result
          )
        )
        
        result
      })
      
      
      unified_trade_recommendation <- shiny::reactive({
        
        screen <- trade_screen()
        
        if (
          is.null(screen) ||
          inherits(
            screen,
            "tbi_trade_error"
          ) ||
          !exists(
            "evaluate_bie_unified_trade_recommendation",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        basketball <- bie_trade_impact_result()
        
        payroll_delta <-
          incoming_salary_value() -
          outgoing_salary_value()
        
        tryCatch(
          evaluate_bie_unified_trade_recommendation(
            cba_pass =
              isTRUE(
                screen$is_trade_screen_pass
              ),
            requires_manual_review =
              isTRUE(
                screen$requires_manual_review
              ),
            basketball_result = basketball,
            payroll_delta = payroll_delta
          ),
          error = function(e) {
            NULL
          }
        )
      })
      
      
      bie_target_needs_result <- shiny::reactive({
        
        current <- bie_trade_current_roster()
        
        if (
          !nrow(current) ||
          !exists(
            "evaluate_bie_roster_needs",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_bie_roster_needs(
            roster_players =
              current
          ),
          error = function(e) NULL
        )
      })
      
      
      bie_target_fit_result <- shiny::reactive({
        
        incoming <- team_b_selected()
        
        if (
          !nrow(incoming) ||
          !exists(
            "rank_bie_acquisition_targets",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        current <- bie_trade_current_roster()
        
        if (!nrow(current)) {
          return(NULL)
        }
        
        tryCatch(
          rank_bie_acquisition_targets(
            target_players =
              incoming,
            current_roster =
              current,
            needs_result =
              bie_target_needs_result()
          ),
          error = function(e) {
            list(
              status = "ERROR",
              rankings =
                data.frame(),
              confidence =
                "FOUNDATION",
              explanation =
                conditionMessage(e)
            )
          }
        )
      })
      
      
      output$bie_target_fit <- shiny::renderUI({
        
        result <- bie_target_fit_result()
        
        if (is.null(result)) {
          return(NULL)
        }
        
        if (
          !identical(
            result$status,
            "OK"
          ) ||
          !is.data.frame(
            result$rankings
          ) ||
          !nrow(
            result$rankings
          )
        ) {
          return(
            shiny::div(
              class =
                "trade-bie-target-fit",
              shiny::span(
                class =
                  "trade-bie-target-title",
                result$explanation %||%
                  "Target Fit unavailable."
              )
            )
          )
        }
        
        d <- result$rankings
        
        format_component <- function(x) {
          
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
          
          sprintf(
            "%.0f",
            value[[1]]
          )
        }
        
        
        header <- list(
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "TARGET"
          ),
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "FIT"
          ),
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "POSITION"
          ),
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "TIMELINE"
          ),
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "COST"
          ),
          shiny::div(
            class =
              "trade-bie-target-cell trade-bie-target-header",
            "ADDRESSES"
          )
        )
        
        
        rows <- unlist(
          lapply(
            seq_len(
              nrow(d)
            ),
            function(i) {
              
              row <- d[
                i,
                ,
                drop = FALSE
              ]
              
              list(
                shiny::div(
                  class =
                    "trade-bie-target-cell trade-bie-target-player",
                  paste0(
                    "#",
                    row$rank[[1]],
                    " ",
                    row$player_name[[1]]
                  )
                ),
                
                shiny::div(
                  class =
                    "trade-bie-target-cell trade-bie-target-score",
                  sprintf(
                    "%.1f",
                    row$fit_score[[1]]
                  )
                ),
                
                shiny::div(
                  class =
                    "trade-bie-target-cell",
                  format_component(
                    row$position_fit
                  )
                ),
                
                shiny::div(
                  class =
                    "trade-bie-target-cell",
                  format_component(
                    row$timeline_fit
                  )
                ),
                
                shiny::div(
                  class =
                    "trade-bie-target-cell",
                  format_component(
                    row$contract_fit
                  )
                ),
                
                shiny::div(
                  class =
                    "trade-bie-target-cell",
                  row$addresses[[1]]
                )
              )
            }
          ),
          recursive = FALSE
        )
        
        
        needs <- bie_target_needs_result()
        
        primary_need <- if (
          !is.null(needs)
        ) {
          needs$primary_need %||%
            "Unavailable"
        } else {
          "Unavailable"
        }
        
        
        shiny::div(
          class =
            "trade-bie-target-fit",
          
          shiny::div(
            class =
              "trade-bie-target-head",
            
            shiny::span(
              class =
                "trade-bie-target-title",
              "ACQUISITION TARGET FIT"
            ),
            
            shiny::span(
              class =
                "trade-bie-target-need",
              paste0(
                "Primary need: ",
                primary_need,
                " • Confidence: ",
                result$confidence
              )
            )
          ),
          
          shiny::div(
            class =
              "trade-bie-target-grid",
            c(
              header,
              rows
            )
          ),
          
          shiny::div(
            class =
              "trade-bie-summary",
            result$explanation
          )
        )
      })
      
      
      output$bie_trade_impact <- shiny::renderUI({
        
        result <-
          bie_trade_impact_result()
        
        if (is.null(result)) {
          return(
            shiny::div(
              class = "trade-bie-summary",
              paste(
                "Select outgoing and incoming players to activate",
                "Basketball Intelligence trade-impact analysis."
              )
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
              class = "trade-bie-summary",
              result$explanation %||%
                "BIE trade-impact analysis is unavailable."
            )
          )
        }
        
        format_score <- function(x) {
          
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
          
          sprintf(
            "%.1f",
            value[[1]]
          )
        }
        
        
        format_delta <- function(x) {
          
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
          
          sprintf(
            "%+.1f",
            value[[1]]
          )
        }
        
        
        delta_class <- function(x) {
          
          value <- suppressWarnings(
            as.numeric(x)
          )
          
          if (
            !length(value) ||
            is.na(value[[1]]) ||
            !is.finite(value[[1]]) ||
            abs(value[[1]]) < 0.01
          ) {
            return("")
          }
          
          if (
            value[[1]] > 0
          ) {
            "positive"
          } else {
            "warning"
          }
        }
        
        
        shiny::tagList(
          
          shiny::div(
            class = "trade-bie-grid",
            
            shiny::div(
              class = "trade-bie-block",
              shiny::span(
                class = "trade-bie-label",
                "BASKETBALL VERDICT"
              ),
              shiny::div(
                class = "trade-bie-value",
                result$verdict
              ),
              shiny::span(
                class = "trade-bie-sub",
                paste0(
                  "Confidence: ",
                  result$confidence
                )
              )
            ),
            
            shiny::div(
              class = "trade-bie-block",
              shiny::span(
                class = "trade-bie-label",
                "STARTING FIVE"
              ),
              shiny::div(
                class = paste(
                  "trade-bie-value",
                  delta_class(
                    result$lineup_delta
                  )
                ),
                format_delta(
                  result$lineup_delta
                )
              ),
              shiny::span(
                class = "trade-bie-sub",
                paste0(
                  format_score(
                    result$current_lineup_score
                  ),
                  " → ",
                  format_score(
                    result$proposed_lineup_score
                  )
                )
              )
            ),
            
            shiny::div(
              class = "trade-bie-block",
              shiny::span(
                class = "trade-bie-label",
                "9-MAN ROTATION"
              ),
              shiny::div(
                class = paste(
                  "trade-bie-value",
                  delta_class(
                    result$rotation_delta
                  )
                ),
                format_delta(
                  result$rotation_delta
                )
              ),
              shiny::span(
                class = "trade-bie-sub",
                paste0(
                  format_score(
                    result$current_rotation_score
                  ),
                  " → ",
                  format_score(
                    result$proposed_rotation_score
                  )
                )
              )
            ),
            
            shiny::div(
              class = "trade-bie-block",
              shiny::span(
                class = "trade-bie-label",
                "ROSTER CONTINUITY"
              ),
              shiny::div(
                class = "trade-bie-value",
                paste0(
                  result$preserved_starters,
                  " / 5"
                )
              ),
              shiny::span(
                class = "trade-bie-sub",
                paste0(
                  "Starters preserved • Roster ",
                  if (
                    result$roster_count_delta > 0
                  ) {
                    "+"
                  } else {
                    ""
                  },
                  result$roster_count_delta
                )
              )
            )
          ),
          
          shiny::div(
            class = "trade-bie-summary",
            shiny::strong(
              "BIE READOUT: "
            ),
            result$explanation
          )
        )
      })
      
      
      # ------------------------------------------------------
      # Build CBA inputs
      # ------------------------------------------------------
      
      trade_screen <- shiny::reactive({
        shiny::req(
          selected_team(),
          input$partner_team,
          selected_season()
        )
        
        a_selected <- team_a_selected()
        b_selected <- team_b_selected()
        
        if (
          !nrow(a_selected) ||
          !nrow(b_selected)
        ) {
          return(NULL)
        }
        
        thresholds <- get_cap_thresholds(
          selected_season()
        )
        
        if (
          is.null(thresholds) ||
          !nrow(thresholds)
        ) {
          return(
            structure(
              list(
                message = "Cap thresholds are unavailable for the selected season."
              ),
              class = "tbi_trade_error"
            )
          )
        }
        
        safe_eval({
          team_a_input <- build_trade_team_input(
            team_value = selected_team(),
            season = selected_season(),
            outgoing_player_ids =
              a_selected$player_id,
            incoming_players =
              b_selected
          )
          
          team_b_input <- build_trade_team_input(
            team_value = input$partner_team,
            season = selected_season(),
            outgoing_player_ids =
              b_selected$player_id,
            incoming_players =
              a_selected
          )
          
          evaluate_two_team_trade(
            team_a = team_a_input,
            team_b = team_b_input,
            thresholds = thresholds
          )
        })
      })
      
      # ------------------------------------------------------
      # Shared front-office scenario publisher
      # ------------------------------------------------------
      
      publish_current_trade_scenario <- function() {
        
        if (
          is.null(transaction_state) ||
          is.null(transaction_state$publish_trade)
        ) {
          return(invisible(NULL))
        }
        
        a_selected <- team_a_selected()
        b_selected <- team_b_selected()
        
        if (
          !nrow(a_selected) ||
          !nrow(b_selected)
        ) {
          return(structure(
            list(message = "Select at least one outgoing and one incoming player before evaluating."),
            class = "tbi_trade_error"
          ))
        }
        
        screen <- trade_screen()
        
        if (
          inherits(
            screen,
            "tbi_trade_error"
          )
        ) {
          return(screen)
        }

        if (is.null(screen)) {
          return(structure(
            list(message = "The protected two-team evaluator did not return a result."),
            class = "tbi_trade_error"
          ))
        }
        
        transaction_state$publish_trade(
          team = selected_team(),
          partner_team = input$partner_team,
          season = selected_season(),
          outgoing_players = a_selected,
          incoming_players = b_selected,
          outgoing_draft_assets =
            team_a_draft_selected(),
          incoming_draft_assets =
            team_b_draft_selected(),
          draft_evaluation =
            draft_screen(),
          evaluation = screen,
          source = "Trade Intelligence"
        )
        
        invisible(TRUE)
      }

      two_team_builder_signature <- shiny::reactive({
        v2_input_signature(list(
          team = as.character(selected_team() %||% ""),
          partner_team = as.character(input$partner_team %||% ""),
          season = as.character(selected_season() %||% ""),
          outgoing_players = sort(selected_ids(input$outgoing_players)),
          incoming_players = sort(selected_ids(input$incoming_players)),
          outgoing_draft_assets = sort(selected_ids(input$outgoing_draft_assets)),
          incoming_draft_assets = sort(selected_ids(input$incoming_draft_assets))
        ))
      })

      two_team_evaluation_error <- shiny::reactiveVal(NULL)
      evaluated_builder_signature <- shiny::reactiveVal(NULL)
      evaluated_scenario_id <- shiny::reactiveVal(NULL)
      evaluated_team <- shiny::reactiveVal(NULL)
      organization_rebind_pending <- shiny::reactiveVal(FALSE)
      pending_organization_team <- shiny::reactiveVal(NULL)

      reset_evaluated_owner <- function() {
        evaluated_builder_signature(NULL)
        evaluated_scenario_id(NULL)
        evaluated_team(NULL)
        organization_rebind_pending(FALSE)
        pending_organization_team(NULL)
        invisible(TRUE)
      }

      builder_inputs_match_context <- function() {
        tryCatch(
          {
            team <- as.character(selected_team() %||% "")
            partner <- as.character(input$partner_team %||% "")
            if (!nzchar(team) || !nzchar(partner) || identical(team, partner)) {
              return(FALSE)
            }

            selection_is_valid <- function(selected, pool, id_column) {
              ids <- selected_ids(selected)
              !length(ids) || (
                is.data.frame(pool) &&
                  id_column %in% names(pool) &&
                  all(ids %in% as.character(pool[[id_column]]))
              )
            }

            selection_is_valid(
              input$outgoing_players,
              team_a_pool(),
              "player_id"
            ) &&
              selection_is_valid(
                input$incoming_players,
                team_b_pool(),
                "player_id"
              ) &&
              selection_is_valid(
                input$outgoing_draft_assets,
                team_a_draft_pool(),
                "draft_asset_id"
              ) &&
              selection_is_valid(
                input$incoming_draft_assets,
                team_b_draft_pool(),
                "draft_asset_id"
              )
          },
          error = function(...) FALSE
        )
      }

      owns_evaluated_scenario <- function(scenario) {
        isTRUE(scenario$active) &&
          identical(as.character(scenario$scenario_type), "trade") &&
          identical(
            as.character(scenario$scenario_id %||% ""),
            evaluated_scenario_id() %||% ""
          )
      }

      if (isTRUE(builder_only)) {
        shiny::observeEvent(input$evaluate_trade, {
          if (!is.null(transaction_state) && !is.null(transaction_state$clear)) {
            transaction_state$clear()
          }
          reset_evaluated_owner()
          two_team_evaluation_error(NULL)

          result <- tryCatch(
            publish_current_trade_scenario(),
            error = function(e) structure(
              list(message = conditionMessage(e)),
              class = "tbi_trade_error"
            )
          )

          if (inherits(result, "tbi_trade_error") || !isTRUE(result)) {
            message <- as.character(result$message %||% "The trade could not be evaluated.")
            two_team_evaluation_error(message[[1]])
            return(invisible(NULL))
          }

          scenario <- transaction_state$snapshot()
          evaluated_builder_signature(two_team_builder_signature())
          evaluated_scenario_id(as.character(scenario$scenario_id %||% ""))
          evaluated_team(as.character(selected_team() %||% ""))
          invisible(NULL)
        }, ignoreInit = TRUE)

        shiny::observeEvent(two_team_builder_signature(), {
          evaluated_signature <- evaluated_builder_signature()
          if (is.null(evaluated_signature)) {
            if (!is.null(two_team_evaluation_error())) two_team_evaluation_error(NULL)
            return(invisible(NULL))
          }

          scenario <- transaction_state$snapshot()
          owns_result <- owns_evaluated_scenario(scenario)
          builder_changed <- !identical(two_team_builder_signature(), evaluated_signature)

          if (owns_result && builder_changed) {
            organization_changed <- !identical(
              as.character(selected_team() %||% ""),
              evaluated_team()
            )

            if (organization_changed || isTRUE(organization_rebind_pending())) {
              current_team <- as.character(selected_team() %||% "")
              if (
                !isTRUE(organization_rebind_pending()) ||
                  !identical(pending_organization_team(), current_team)
              ) {
                organization_rebind_pending(TRUE)
                pending_organization_team(current_team)
              }

              if (
                identical(pending_organization_team(), current_team) &&
                  builder_inputs_match_context()
              ) {
                evaluated_builder_signature(two_team_builder_signature())
                evaluated_team(current_team)
                organization_rebind_pending(FALSE)
                pending_organization_team(NULL)
              }
            } else if (!is.null(transaction_state$clear)) {
              transaction_state$clear()
              reset_evaluated_owner()
            }
          }
          if (!is.null(two_team_evaluation_error())) two_team_evaluation_error(NULL)
          invisible(NULL)
        }, ignoreInit = TRUE)

        shiny::observeEvent(
          list(
            transaction_state$snapshot()$active,
            transaction_state$snapshot()$scenario_id
          ),
          {
            scenario <- transaction_state$snapshot()
            owns_result <- owns_evaluated_scenario(scenario)
            if (!owns_result) reset_evaluated_owner()
          },
          ignoreInit = TRUE
        )

        output$evaluate_trade_status <- shiny::renderUI({
          error <- two_team_evaluation_error()
          if (!is.null(error)) {
            return(shiny::div(
              class = "tbi-trade-error",
              shiny::strong("Not evaluated yet"),
              shiny::p(error)
            ))
          }
          scenario <- transaction_state$snapshot()
          evaluated <- owns_evaluated_scenario(scenario)
          if (!evaluated) {
            return(shiny::div(class = "tbi-trade-evaluate-status", "Not evaluated yet"))
          }
          shiny::div(class = "tbi-trade-evaluate-status", "Evaluated current trade")
        })
      }

      if (!isTRUE(builder_only)) {
        shiny::observeEvent(
          list(
            input$outgoing_players,
            input$incoming_players,
            input$outgoing_draft_assets,
            input$incoming_draft_assets,
            input$partner_team,
            selected_team(),
            selected_season()
          ),
          {
            if (
              length(selected_ids(input$outgoing_players)) &&
              length(selected_ids(input$incoming_players))
            ) {
              publish_current_trade_scenario()
            }
          },
          ignoreInit = TRUE
        )
      }
      
      
      # ------------------------------------------------------
      # UI player selectors
      # ------------------------------------------------------
      
      output$team_a_name <- shiny::renderText({
        selected_team()
      })
      
      output$scenario_label <- shiny::renderText({
        shiny::req(
          selected_team(),
          input$partner_team
        )
        
        paste0(
          selected_team(),
          " ↔ ",
          input$partner_team
        )
      })
      
      output$team_a_players <- shiny::renderUI({
        d <- team_a_pool()
        
        shiny::checkboxGroupInput(
          session$ns(
            "outgoing_players"
          ),
          label = NULL,
          choices = player_choices(
            d
          )
        )
      })
      
      output$team_b_players <- shiny::renderUI({
        d <- team_b_pool()
        
        shiny::checkboxGroupInput(
          session$ns(
            "incoming_players"
          ),
          label = NULL,
          choices = player_choices(
            d
          )
        )
      })
      
      # ------------------------------------------------------
      output$team_a_draft_assets <- shiny::renderUI({

        assets <- team_a_draft_pool()

        if (
          is.null(assets) ||
          !nrow(assets)
        ) {

          return(
            shiny::div(
              style = "color:#8390a2; font-size:.72rem; padding:8px 0;",
              "No selectable draft assets loaded."
            )
          )
        }

        shiny::checkboxGroupInput(
          session$ns(
            "outgoing_draft_assets"
          ),
          label = NULL,
          choices =
            tbi_trade_draft_asset_choices(
              assets
            )
        )
      })


      output$team_b_draft_assets <- shiny::renderUI({

        assets <- team_b_draft_pool()

        if (
          is.null(assets) ||
          !nrow(assets)
        ) {

          return(
            shiny::div(
              style = "color:#8390a2; font-size:.72rem; padding:8px 0;",
              "No selectable draft assets loaded."
            )
          )
        }

        shiny::checkboxGroupInput(
          session$ns(
            "incoming_draft_assets"
          ),
          label = NULL,
          choices =
            tbi_trade_draft_asset_choices(
              assets
            )
        )
      })


      draft_package_summary_ui <- function(
          assets,
          direction) {

        if (
          is.null(assets) ||
          !nrow(assets)
        ) {
          return(NULL)
        }

        value <-
          tbi_trade_draft_value(
            assets
          )

        value_text <- if (
          is.na(value)
        ) {
          "Internal value not fully loaded"
        } else {
          paste0(
            "Internal value: ",
            format(
              round(value, 1),
              trim = TRUE
            )
          )
        }

        shiny::div(
          style = paste(
            "margin:8px 0 4px; padding:8px 10px;",
            "border:1px solid rgba(148,163,184,.10);",
            "border-radius:8px; background:rgba(255,255,255,.02);"
          ),
          shiny::strong(
            paste0(
              nrow(assets),
              " draft asset",
              if (nrow(assets) == 1L) "" else "s",
              " ",
              direction
            )
          ),
          shiny::tags$small(
            style = "display:block; margin-top:3px; color:#8390a2;",
            value_text
          )
        )
      }


      output$team_a_draft_summary <- shiny::renderUI({

        draft_package_summary_ui(
          team_a_draft_selected(),
          "outgoing"
        )
      })


      output$team_b_draft_summary <- shiny::renderUI({

        draft_package_summary_ui(
          team_b_draft_selected(),
          "incoming"
        )
      })


      # Snapshot outputs
      # ------------------------------------------------------
      
      output$snapshot_outgoing_salary <- shiny::renderText({
        money(
          outgoing_salary_value()
        )
      })
      
      output$snapshot_incoming_salary <- shiny::renderText({
        money(
          incoming_salary_value()
        )
      })
      
      output$snapshot_draft_value_delta <- shiny::renderText({

        value <-
          draft_screen()$value_delta

        if (
          is.null(value) ||
          length(value) == 0L ||
          is.na(value)
        ) {
          return(
            "NOT FULLY VALUED"
          )
        }

        if (value > 0) {
          paste0(
            "+",
            format(
              round(value, 1),
              trim = TRUE
            )
          )
        } else {
          format(
            round(value, 1),
            trim = TRUE
          )
        }
      })


      output$snapshot_draft_status <- shiny::renderText({

        draft_screen()$status
      })


      output$shared_scenario_status <- shiny::renderText({
        
        if (
          is.null(transaction_state) ||
          is.null(transaction_state$snapshot)
        ) {
          return("LOCAL")
        }
        
        scenario <- transaction_state$snapshot()
        
        if (
          !isTRUE(scenario$active) ||
          !identical(
            as.character(
              scenario$source
            ),
            "Trade Intelligence"
          )
        ) {
          return("LOCAL")
        }
        
        "SHARED"
      })
      
      
      output$snapshot_salary_difference <- shiny::renderText({
        delta <-
          incoming_salary_value() -
          outgoing_salary_value()
        
        if (delta > 0) {
          paste0(
            "+",
            money(delta)
          )
        } else {
          money(delta)
        }
      })
      
      output$center_outgoing_salary <- shiny::renderText({
        money(
          outgoing_salary_value()
        )
      })
      
      output$center_incoming_salary <- shiny::renderText({
        money(
          incoming_salary_value()
        )
      })
      
      output$team_a_abbr <- shiny::renderText({
        phase13_team_abbreviation(
          teams,
          selected_team()
        )
      })

      reset_builder <- function() {
        shiny::updateSelectInput(session, "partner_team", selected = "")
        shiny::updateCheckboxGroupInput(session, "outgoing_players", selected = character())
        shiny::updateCheckboxGroupInput(session, "incoming_players", selected = character())
        shiny::updateCheckboxGroupInput(session, "outgoing_draft_assets", selected = character())
        shiny::updateCheckboxGroupInput(session, "incoming_draft_assets", selected = character())
        if (!is.null(transaction_state) && !is.null(transaction_state$clear)) transaction_state$clear()
        reset_evaluated_owner()
        two_team_evaluation_error(NULL)
        invisible(TRUE)
      }
      
      output$team_b_abbr <- shiny::renderText({
        phase13_team_abbreviation(
          teams,
          input$partner_team
        )
      })
      
      output$team_a_short_name <- shiny::renderText({
        selected_team()
      })
      
      team_b_short_name_text <- shiny::reactive({
        input$partner_team
      })

      bind_trade_text_output_mirror(
        output,
        "team_b_short_name",
        "team_b_panel_short_name",
        team_b_short_name_text
      )
      
      
      output$center_team_a_sends_label <- shiny::renderText({
        
        team <- selected_team()
        
        if (
          is.null(team) ||
          !nzchar(
            trimws(
              as.character(team)
            )
          )
        ) {
          return("YOUR TEAM SENDS")
        }
        
        paste0(
          toupper(
            as.character(team)
          ),
          " SENDS"
        )
      })
      
      
      output$center_team_b_sends_label <- shiny::renderText({
        
        team <- input$partner_team
        
        if (
          is.null(team) ||
          !nzchar(
            trimws(
              as.character(team)
            )
          )
        ) {
          return("PARTNER SENDS")
        }
        
        paste0(
          toupper(
            as.character(team)
          ),
          " SENDS"
        )
      })
      
      
      output$outgoing_salary <- shiny::renderText({
        money(
          outgoing_salary_value()
        )
      })
      
      output$incoming_salary <- shiny::renderText({
        money(
          incoming_salary_value()
        )
      })
      
      output$salary_difference <- shiny::renderText({
        delta <-
          incoming_salary_value() -
          outgoing_salary_value()
        
        if (delta > 0) {
          paste0(
            "+",
            money(delta)
          )
        } else {
          money(delta)
        }
      })
      
      snapshot_team_a_status_text <- shiny::reactive({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return("Select players")
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return("Review")
        }
        
        result$team_a$post_trade_status
      })

      bind_trade_text_output_mirror(
        output,
        "snapshot_team_a_status",
        "center_team_a_status",
        snapshot_team_a_status_text
      )
      
      output$snapshot_team_b_status <- shiny::renderText({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return("Select players")
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return("Review")
        }
        
        result$team_b$post_trade_status
      })
      
      snapshot_cba_result_text <- shiny::reactive({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return("Select players")
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return("REVIEW")
        }
        
        if (
          isTRUE(
            result$is_trade_screen_pass
          ) &&
          isTRUE(
            result$requires_manual_review
          )
        ) {
          "REVIEW"
        } else {
          result$status
        }
      })

      bind_trade_text_output_mirror(
        output,
        "snapshot_cba_result",
        "center_cba_result",
        snapshot_cba_result_text
      )
      
      output$scorecard_cba_result <- shiny::renderText({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return("—")
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return("REVIEW")
        }
        
        if (
          isTRUE(
            result$is_trade_screen_pass
          ) &&
          isTRUE(
            result$requires_manual_review
          )
        ) {
          "REVIEW"
        } else {
          result$status
        }
      })
      
      # ------------------------------------------------------
      # Transaction decision
      # ------------------------------------------------------
      
      output$transaction_decision <- shiny::renderUI({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return(
            shiny::tagList(
              shiny::div(
                class = "tbi-v2-decision-main",
                
                shiny::div(
                  class = "tbi-v2-decision-symbol",
                  bsicons::bs_icon(
                    "arrow-left-right"
                  )
                ),
                
                shiny::div(
                  class = "tbi-v2-decision-copy",
                  shiny::strong(
                    class = "tbi-v2-decision-word",
                    "BUILD"
                  ),
                  shiny::p(
                    paste(
                      "Select at least one outgoing player and one incoming",
                      "player to run the two-team CBA screen."
                    )
                  )
                )
              )
            )
          )
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
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
                  result$message
                )
              )
            )
          )
        }
        
        draft_result <-
          draft_screen()

        draft_blocked <-
          identical(
            draft_result$status,
            "BLOCK"
          )

        draft_review <-
          identical(
            draft_result$status,
            "REVIEW"
          )

        cba_pass <-
          isTRUE(
            result$is_trade_screen_pass
          )

        cba_manual <-
          isTRUE(
            result$requires_manual_review
          )

        label <- if (
          draft_blocked
        ) {

          "DO NOT PROCEED"

        } else if (
          cba_pass &&
          !cba_manual &&
          !draft_review
        ) {

          "PROCEED"

        } else if (
          cba_pass
        ) {

          "REVIEW"

        } else {

          "DO NOT PROCEED"
        }

        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  identical(
                    label,
                    "PROCEED"
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
                label
              ),
              
              shiny::p(
                paste(result$executive_summary, draft_result$summary)
              )
            )
          ),
          
          shiny::div(
            class = "tbi-v2-decision-metrics",
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "YOUR TEAM"
              ),
              shiny::strong(
                result$team_a$screen_status
              ),
              shiny::tags$small(
                result$team_a$post_trade_status
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "TRADE PARTNER"
              ),
              shiny::strong(
                result$team_b$screen_status
              ),
              shiny::tags$small(
                result$team_b$post_trade_status
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Executive recommendation
      # ------------------------------------------------------
      
      output$executive_recommendation_badge <- shiny::renderUI({
        
        screen <- trade_screen()
        unified <- unified_trade_recommendation()
        
        if (is.null(screen)) {
          label <- "BUILD SCENARIO"
          tone <- "#60a5fa"
          icon <- "arrow-left-right"
          
        } else if (
          inherits(
            screen,
            "tbi_trade_error"
          )
        ) {
          label <- "REVIEW"
          tone <- "#f59e0b"
          icon <- "exclamation-triangle"
          
        } else if (!is.null(unified)) {
          
          label <- unified$recommendation
          
          tone <- switch(
            unified$tone,
            "positive" = "#34d399",
            "danger" = "#fb7185",
            "warning" = "#f59e0b",
            "blue" = "#60a5fa",
            "#60a5fa"
          )
          
          icon <- if (
            identical(
              label,
              "ADVANCE"
            )
          ) {
            "bullseye"
          } else if (
            identical(
              label,
              "ADVANCE TO BASKETBALL REVIEW"
            )
          ) {
            "graph-up-arrow"
          } else {
            "exclamation-triangle"
          }
          
        } else {
          label <- "REVIEW"
          tone <- "#f59e0b"
          icon <- "exclamation-triangle"
        }
        
        shiny::div(
          style = paste0(
            "min-height:112px; padding:18px;",
            "display:flex; align-items:center; gap:14px;",
            "border:1px solid ", tone, "55;",
            "border-radius:12px;",
            "background:", tone, "0D;"
          ),
          
          shiny::div(
            style = paste0(
              "width:52px; height:52px; flex:0 0 52px;",
              "display:grid; place-items:center;",
              "border:1px solid ", tone, "88;",
              "border-radius:50%; color:", tone, ";"
            ),
            bsicons::bs_icon(icon)
          ),
          
          shiny::div(
            shiny::span(
              class = "tbi-v2-snapshot-label",
              "UNIFIED RECOMMENDATION"
            ),
            shiny::strong(
              style = paste0(
                "display:block; margin-top:5px;",
                "font-size:1.15rem; line-height:1.2; color:",
                tone,
                ";"
              ),
              label
            ),
            if (!is.null(unified)) {
              shiny::span(
                style = paste0(
                  "display:block; margin-top:6px;",
                  "color:#8fa4bd; font-size:.54rem;"
                ),
                paste0(
                  "Decision Score ",
                  sprintf(
                    "%.1f",
                    unified$score
                  ),
                  " / 100"
                )
              )
            }
          )
        )
      })
      
      output$executive_recommendation_copy <- shiny::renderUI({
        
        screen <- trade_screen()
        unified <- unified_trade_recommendation()
        
        if (is.null(screen)) {
          return(
            shiny::div(
              shiny::h3(
                style = "margin:0 0 6px;",
                "Build a transaction"
              ),
              shiny::p(
                style = "margin:0; color:#9aa8ba; line-height:1.55;",
                paste(
                  "Select outgoing and incoming players.",
                  "The unified recommendation will combine the CBA screen,",
                  "BIE basketball impact, and payroll direction."
                )
              )
            )
          )
        }
        
        if (
          inherits(
            screen,
            "tbi_trade_error"
          )
        ) {
          return(
            shiny::div(
              shiny::h3(
                style = "margin:0 0 6px;",
                "Resolve screen inputs"
              ),
              shiny::p(
                style = "margin:0; color:#9aa8ba; line-height:1.55;",
                screen$message
              )
            )
          )
        }
        
        if (is.null(unified)) {
          return(
            shiny::div(
              shiny::h3(
                style = "margin:0 0 6px;",
                "Recommendation unavailable"
              ),
              shiny::p(
                style = "margin:0; color:#9aa8ba; line-height:1.55;",
                "The CBA screen completed, but the unified BIE recommendation could not be generated."
              )
            )
          )
        }
        
        shiny::div(
          shiny::h3(
            style = "margin:0 0 7px;",
            unified$headline
          ),
          
          shiny::p(
            style = "margin:0; color:#9aa8ba; line-height:1.55;",
            unified$rationale
          ),
          
          shiny::div(
            style = paste(
              "display:flex; flex-wrap:wrap; gap:7px 12px;",
              "margin-top:10px; color:#7f93ad; font-size:.54rem;"
            ),
            
            shiny::span(
              paste0(
                "CBA GATE: ",
                unified$cba_gate
              )
            ),
            
            shiny::span(
              paste0(
                "BASKETBALL: ",
                unified$basketball_verdict
              )
            ),
            
            shiny::span(
              paste0(
                "CONFIDENCE: ",
                unified$basketball_confidence
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # CBA rule scorecard
      # ------------------------------------------------------
      
      output$cba_scorecard <- shiny::renderUI({
        result <- trade_screen()
        
        if (
          is.null(result) ||
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return(
            shiny::div(
              style = "padding:18px; color:#8390a2;",
              "Build a transaction to populate the CBA rule scorecard."
            )
          )
        }
        
        team_a <- result$team_a
        team_b <- result$team_b
        
        salary_match <-
          isTRUE(
            team_a$is_salary_match
          ) &&
          isTRUE(
            team_b$is_salary_match
          )
        
        apron_crossing <-
          isTRUE(
            team_a$crosses_first_apron
          ) ||
          isTRUE(
            team_a$crosses_second_apron
          ) ||
          isTRUE(
            team_b$crosses_first_apron
          ) ||
          isTRUE(
            team_b$crosses_second_apron
          )
        
        aggregation <-
          isTRUE(
            team_a$second_apron_aggregation_violation
          ) ||
          isTRUE(
            team_b$second_apron_aggregation_violation
          )
        
        manual <-
          isTRUE(
            result$requires_manual_review
          )
        
        shiny::tagList(
          score_row(
            "Salary Matching",
            "Both-team matching screen",
            "cash-stack",
            salary_match,
            paste(
              team_a$matching_rule,
              "/",
              team_b$matching_rule
            ),
            cba_term = "Salary Matching"
          ),
          
          score_row(
            "Apron Crossing",
            "Post-trade apron movement",
            "graph-up-arrow",
            !apron_crossing,
            if (apron_crossing) {
              "Threshold crossing detected"
            } else {
              "No modeled crossing"
            },
            review = apron_crossing,
            cba_term = if (
              isTRUE(team_a$crosses_second_apron) ||
              isTRUE(team_b$crosses_second_apron)
            ) {
              "Second Apron"
            } else {
              "First Apron"
            }
          ),
          
          score_row(
            "Second-Apron Aggregation",
            "Outgoing salary aggregation",
            "people",
            !aggregation,
            if (aggregation) {
              "Restriction triggered"
            } else {
              "No modeled violation"
            },
            cba_term = "Salary Aggregation"
          ),
          
          score_row(
            "Draft Asset Screen",
            "Ownership / protection / source review",
            "exclamation-triangle",
            draft_screen()$status %in%
              c("PASS", "NOT USED"),
            draft_screen()$summary,
            review =
              identical(
                draft_screen()$status,
                "REVIEW"
              )
          ),

          score_row(
            "Manual CBA Review",
            "Transaction-specific review",
            "exclamation-triangle",
            !manual,
            if (manual) {
              "Additional review required"
            } else {
              "No modeled manual flag"
            },
            review = manual
          )
        )
      })
      
      # ------------------------------------------------------
      # CBA alerts
      # ------------------------------------------------------
      
      output$cba_alerts <- shiny::renderUI({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return(
            shiny::div(
              class = "tbi-v2-headline-row",
              shiny::span(
                class = "tbi-v2-headline-dot"
              ),
              shiny::span(
                "Select players to run the CBA screen."
              )
            )
          )
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return(
            shiny::div(
              class = "tbi-v2-headline-row",
              shiny::span(
                class = "tbi-v2-headline-dot tbi-v2-headline-dot-orange"
              ),
              shiny::span(
                result$message
              )
            )
          )
        }
        
        alerts <- c(
          paste0(
            selected_team(),
            ": ",
            result$team_a$matching_rule,
            ". Maximum incoming salary ",
            money(
              result$team_a$maximum_incoming_salary
            ),
            "."
          ),
          paste0(
            input$partner_team,
            ": ",
            result$team_b$matching_rule,
            ". Maximum incoming salary ",
            money(
              result$team_b$maximum_incoming_salary
            ),
            "."
          )
        )
        
        restrictions <- unique(
          c(
            result$team_a$restriction_flags,
            result$team_b$restriction_flags
          )
        )
        
        restrictions <- restrictions[
          nzchar(
            restrictions
          )
        ]
        
        alerts <- unique(
          c(
            alerts,
            restrictions
          )
        )
        
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
      
      # ------------------------------------------------------
      # Risks
      # ------------------------------------------------------
      
      output$trade_risks <- shiny::renderUI({
        result <- trade_screen()
        
        risks <- character()
        
        if (
          is.null(result)
        ) {
          risks <- "Transaction risk will populate after player selection."
        } else if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          risks <- result$message
        } else {
          if (
            !isTRUE(
              result$team_a$is_salary_match
            )
          ) {
            risks <- c(
              risks,
              paste(
                selected_team(),
                "fails salary matching."
              )
            )
          }
          
          if (
            !isTRUE(
              result$team_b$is_salary_match
            )
          ) {
            risks <- c(
              risks,
              paste(
                input$partner_team,
                "fails salary matching."
              )
            )
          }
          
          if (
            isTRUE(
              result$team_a$second_apron_aggregation_violation
            ) ||
            isTRUE(
              result$team_b$second_apron_aggregation_violation
            )
          ) {
            risks <- c(
              risks,
              "Second-apron aggregation restriction is triggered."
            )
          }
          
          if (
            isTRUE(
              result$requires_manual_review
            )
          ) {
            risks <- c(
              risks,
              "At least one modeled apron or aggregation flag requires manual CBA review."
            )
          }
        }
        
        if (!length(risks)) {
          risks <- "No modeled salary-matching or selected apron restriction is currently blocking the scenario."
        }
        
        shiny::tagList(
          lapply(
            unique(risks),
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
      
      output$trade_opportunities <- shiny::renderUI({
        result <- trade_screen()
        
        opportunities <- character()
        
        if (
          is.null(result)
        ) {
          opportunities <- "Use the scenario builder to compare financially viable transaction structures."
        } else if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          opportunities <- "Resolve the current data or threshold issue before evaluating alternatives."
        } else {
          delta <-
            incoming_salary_value() -
            outgoing_salary_value()
          
          if (delta < 0) {
            opportunities <- c(
              opportunities,
              paste0(
                "The scenario reduces ",
                selected_team(),
                " payroll by ",
                money(
                  abs(delta)
                ),
                "."
              )
            )
          }
          
          if (
            isTRUE(
              result$is_trade_screen_pass
            )
          ) {
            opportunities <- c(
              opportunities,
              "The structure passes the current two-team salary screen and can advance to deeper basketball and CBA review."
            )
          }
          
          if (
            !isTRUE(
              result$requires_manual_review
            )
          ) {
            opportunities <- c(
              opportunities,
              "No modeled apron-crossing or second-apron aggregation flag is currently active."
            )
          }
        }
        
        if (!length(opportunities)) {
          opportunities <- "Adjust salary structure or player combination to create a cleaner transaction path."
        }
        
        shiny::tagList(
          lapply(
            unique(opportunities),
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
      # Detailed trade readout
      # ------------------------------------------------------
      
      output$trade_readout <- shiny::renderUI({
        result <- trade_screen()
        
        if (
          is.null(result)
        ) {
          return(
            shiny::div(
              style = "color:#8390a2;",
              paste(
                "Select players from both organizations.",
                "The engine will then screen salary matching,",
                "post-trade payroll bands, apron crossings,",
                "and second-apron aggregation."
              )
            )
          )
        }
        
        if (
          inherits(
            result,
            "tbi_trade_error"
          )
        ) {
          return(
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
                  "Screen unavailable"
                ),
                shiny::p(
                  result$message
                )
              )
            )
          )
        }
        
        team_panel <- function(
    team_name,
    side) {
          
          shiny::div(
            class = "tbi-panel",
            
            shiny::div(
              class = "tbi-panel-kicker",
              team_name
            ),
            
            shiny::h3(
              side$screen_status
            ),
            
            signal_row(
              "Outgoing salary",
              money(
                side$outgoing_salary
              )
            ),
            
            signal_row(
              "Incoming salary",
              money(
                side$incoming_salary
              )
            ),
            
            signal_row(
              "Maximum incoming",
              money(
                side$maximum_incoming_salary
              )
            ),
            
            signal_row(
              "Matching rule",
              side$matching_rule,
              cba_term = if (
                grepl(
                  "Second-apron",
                  as.character(
                    side$matching_rule
                  ),
                  ignore.case = TRUE
                )
              ) {
                "Second Apron"
              } else {
                "Salary Matching"
              }
            ),
            
            signal_row(
              "Pre-trade status",
              side$pre_trade_status,
              cba_term = cba_status_term(
                side$pre_trade_status
              )
            ),
            
            signal_row(
              "Post-trade status",
              side$post_trade_status,
              cba_term = cba_status_term(
                side$post_trade_status
              )
            ),
            
            signal_row(
              "Manual review",
              if (
                isTRUE(
                  side$requires_manual_review
                )
              ) {
                "Required"
              } else {
                "No modeled flag"
              }
            ),
            
            signal_row(
              "CBA reference",
              "Open salary-matching rule",
              cba_term = "Salary Matching"
            )
          )
        }
        
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(
              6,
              6
            ),
            team_panel(
              selected_team(),
              result$team_a
            ),
            team_panel(
              input$partner_team,
              result$team_b
            )
          ),
          
          shiny::div(
            style = "margin-top:14px; padding:14px 16px; border:1px solid rgba(148,163,184,.12); border-radius:10px; background:rgba(255,255,255,.018);",
            
            shiny::div(
              class = "tbi-page-eyebrow",
              "EXECUTIVE EXPLANATION"
            ),
            
            shiny::p(
              style = "margin:6px 0 8px; color:#d7dfeb; line-height:1.55;",
              result$executive_summary
            ),
            
            shiny::tags$small(
              style = "color:#7f8da0;",
              result$scope_note
            )
          )
        )
      })

      list(reset_builder = reset_builder)
    }
  )
}
