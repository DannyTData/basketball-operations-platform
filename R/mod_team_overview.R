# ------------------------------------------------------------
# Module: Team Overview
# Version 2 Executive Organization Profile
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Team Overview UI
#'
#' @param id Internal module ID.
#' @noRd
mod_team_overview_ui <- function(id) {
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
        class = "team-cba-link"
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
    class = "tbi-module-page tbi-v2-team-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-v2-team-page {
          display:grid;
          gap:12px;
        }

        .team-cba-link {
          color:#72adff !important;
          font-weight:800 !important;
          text-decoration:none !important;
        }

        .team-cba-link::after {
          content:'  ↗';
          color:#5f9fee;
          font-size:.66em;
          opacity:.78;
        }

        .team-cba-link:hover {
          color:#a8ceff !important;
          text-decoration:underline !important;
          text-underline-offset:2px;
        }


        .team-v2-profile-grid {
          display:grid;
          grid-template-columns:minmax(0,1.05fr) minmax(300px,.95fr);
          gap:12px;
        }

        .team-v2-profile-hero {
          padding:18px;
          display:grid;
          grid-template-columns:96px minmax(0,1fr);
          gap:16px;
          align-items:center;
        }

        .team-v2-mark {
          width:92px;
          height:92px;
          display:grid;
          place-items:center;
          border:1px solid rgba(96,165,250,.30);
          border-radius:22px;
          background:
            radial-gradient(circle at 35% 30%,rgba(96,165,250,.18),transparent 58%),
            #101a29;
          color:#70adff;
          font-size:1.45rem;
          font-weight:900;
          letter-spacing:.03em;
        }

        .team-v2-name {
          margin:3px 0 4px;
          color:#f5f8fc;
          font-size:1.65rem;
          font-weight:850;
          letter-spacing:-.03em;
        }

        .team-v2-subtitle {
          color:#8898ab;
          font-size:.68rem;
        }

        .team-v2-status {
          display:inline-flex;
          margin-top:10px;
          padding:5px 9px;
          border:1px solid rgba(52,211,153,.22);
          border-radius:999px;
          color:#34d399;
          background:rgba(16,185,129,.07);
          font-size:.56rem;
          font-weight:850;
          letter-spacing:.07em;
        }

        .team-v2-signal-row {
          min-height:40px;
          padding:7px 0;
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:14px;
          border-bottom:1px solid rgba(148,163,184,.08);
          color:#8796aa;
          font-size:.63rem;
        }

        .team-v2-signal-row:last-child {
          border-bottom:0;
        }

        .team-v2-signal-row strong {
          color:#eef3f8;
          text-align:right;
        }

        .team-v2-score-grid {
          display:grid;
          grid-template-columns:repeat(4,minmax(0,1fr));
          gap:8px;
          padding:14px;
        }

        .team-v2-score-card {
          min-height:92px;
          padding:11px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:9px;
          background:rgba(255,255,255,.012);
        }

        .team-v2-score-card span {
          display:block;
          color:#718198;
          font-size:.50rem;
          font-weight:850;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .team-v2-score-card strong {
          display:block;
          margin-top:6px;
          color:#edf3f8;
          font-size:1rem;
        }

        .team-v2-score-card small {
          display:block;
          margin-top:4px;
          color:#7f8fa4;
          font-size:.56rem;
          line-height:1.35;
        }

        .team-v2-table-wrap {
          max-height:455px;
          overflow:auto;
        }

        .team-v2-core-grid {
          display:grid;
          grid-template-columns:repeat(3,minmax(0,1fr));
          gap:8px;
          padding:14px;
        }

        .team-v2-core-card {
          min-height:105px;
          padding:12px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:9px;
          background:rgba(255,255,255,.012);
        }

        .team-v2-core-rank {
          color:#5fa5ff;
          font-size:.52rem;
          font-weight:850;
          letter-spacing:.08em;
        }

        .team-v2-core-card strong {
          display:block;
          margin:5px 0 3px;
          overflow:hidden;
          color:#f2f5f9;
          font-size:.78rem;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        .team-v2-core-card span {
          display:block;
          color:#8696aa;
          font-size:.58rem;
          line-height:1.4;
        }

        .team-v2-rec-grid {
          padding:16px;
          display:grid;
          grid-template-columns:minmax(220px,.42fr) minmax(0,1.58fr);
          gap:18px;
          align-items:center;
        }

        .team-v2-rec-box {
          min-height:110px;
          padding:16px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:rgba(59,130,246,.045);
        }

        .team-v2-rec-box span {
          color:#718198;
          font-size:.52rem;
          font-weight:850;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .team-v2-rec-box strong {
          margin-top:6px;
          color:#61a8ff;
          font-size:1.2rem;
          line-height:1.15;
        }

        .team-v2-note {
          color:#7f8fa4;
          font-size:.59rem;
          line-height:1.5;
        }

        .team-v2-trade-banner {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          padding:10px 13px;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:linear-gradient(
            90deg,
            rgba(59,130,246,.10),
            rgba(59,130,246,.03)
          );
        }

        .team-v2-trade-copy {
          min-width:0;
          color:#a9b8ca;
          font-size:.60rem;
          line-height:1.45;
        }

        .team-v2-trade-chip {
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

        .team-v2-trade-delta {
          color:#f4f8fc;
          font-size:.62rem;
          font-weight:850;
          white-space:nowrap;
        }

        .team-v2-trade-delta.positive {
          color:#34d399;
        }

        .team-v2-trade-delta.warning {
          color:#fbbf24;
        }

        @media(max-width:1050px) {
          .team-v2-profile-grid {
            grid-template-columns:1fr;
          }

          .team-v2-score-grid {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }
        }

        @media(max-width:700px) {
          .team-v2-profile-hero,
          .team-v2-rec-grid {
            grid-template-columns:1fr;
          }

          .team-v2-score-grid,
          .team-v2-core-grid {
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
          "ORGANIZATIONAL PROFILE"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Team Overview"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Connect competitive position, roster construction,",
            "financial posture, core personnel, and league context."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "VIEW"
        ),
        shiny::strong("ORGANIZATION")
      )
    ),
    
    shiny::uiOutput(
      ns("team_trade_scenario_banner")
    ),
    
    # --------------------------------------------------------
    # Team identity + executive readout
    # --------------------------------------------------------
    
    shiny::div(
      class = "team-v2-profile-grid",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "team-v2-profile-hero",
          
          shiny::div(
            class = "team-v2-mark",
            shiny::textOutput(
              ns("team_abbreviation"),
              inline = TRUE
            )
          ),
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "CURRENT ORGANIZATION"
            ),
            
            shiny::div(
              class = "team-v2-name",
              shiny::textOutput(
                ns("team_name"),
                inline = TRUE
              )
            ),
            
            shiny::div(
              class = "team-v2-subtitle",
              shiny::textOutput(
                ns("team_context"),
                inline = TRUE
              )
            ),
            
            shiny::span(
              class = "team-v2-status",
              shiny::textOutput(
                ns("competitive_status"),
                inline = TRUE
              )
            )
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
              "EXECUTIVE READOUT"
            ),
            shiny::h3(
              "Organization at a glance"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "CURRENT"
          )
        ),
        
        shiny::div(
          style = "padding:12px 16px;",
          shiny::uiOutput(
            ns("organization_readout")
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot",
      
      shiny::div(
        class = "tbi-v2-section-title-row",
        
        shiny::div(
          class = "tbi-v2-section-title",
          shiny::span(
            class = "tbi-v2-section-icon",
            bsicons::bs_icon("bullseye")
          ),
          shiny::span("TEAM SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "CURRENT PROFILE"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "RECORD",
          "snapshot_record",
          "bullseye",
          "blue"
        ),
        
        snapshot_item(
          "CONFERENCE",
          "snapshot_conference_rank",
          "graph-up-arrow",
          "green"
        ),
        
        snapshot_item(
          "POINT DIFF",
          "snapshot_point_diff",
          "graph-up-arrow",
          "blue"
        ),
        
        snapshot_item(
          "PAYROLL",
          "snapshot_payroll",
          "cash-stack",
          "orange",
          cba_term = "Salary Cap"
        ),
        
        snapshot_item(
          "ROSTER",
          "snapshot_roster_size",
          "people",
          "blue"
        ),
        
        snapshot_item(
          "AVG AGE",
          "snapshot_average_age",
          "calendar3",
          "purple"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Decision + organizational scorecard
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
          
          shiny::span("ORGANIZATIONAL DECISION")
        ),
        
        shiny::uiOutput(
          ns("team_decision")
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
            shiny::span("ORGANIZATIONAL SCORECARD")
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Composite"),
            shiny::strong(
              shiny::textOutput(
                ns("organization_score"),
                inline = TRUE
              )
            ),
            shiny::span("/ 100")
          )
        ),
        
        shiny::uiOutput(
          ns("organization_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Team performance profile
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "TEAM PROFILE"
          ),
          shiny::h3(
            "Competitive and roster indicators"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "TEAM LEVEL"
        )
      ),
      
      shiny::div(
        class = "team-v2-score-grid",
        
        shiny::div(
          class = "team-v2-score-card",
          shiny::span("WIN PERCENTAGE"),
          shiny::strong(
            shiny::textOutput(
              ns("profile_win_pct"),
              inline = TRUE
            )
          ),
          shiny::tags$small(
            "Current results"
          )
        ),
        
        shiny::div(
          class = "team-v2-score-card",
          shiny::span("SCORING"),
          shiny::strong(
            shiny::textOutput(
              ns("profile_scoring"),
              inline = TRUE
            )
          ),
          shiny::tags$small(
            "Points per game"
          )
        ),
        
        shiny::div(
          class = "team-v2-score-card",
          shiny::span("TOP-3 SALARY"),
          shiny::strong(
            shiny::textOutput(
              ns("profile_top3_concentration"),
              inline = TRUE
            )
          ),
          shiny::tags$small(
            "Payroll concentration"
          )
        ),
        
        shiny::div(
          class = "team-v2-score-card",
          if (
            exists("tbi_cba_link", mode = "function")
          ) {
            tbi_cba_link(
              term = "Two-Way Contract",
              label = "TWO-WAYS",
              class = "team-cba-link"
            )
          } else {
            shiny::span("TWO-WAYS")
          },
          shiny::strong(
            shiny::textOutput(
              ns("profile_two_ways"),
              inline = TRUE
            )
          ),
          shiny::tags$small(
            "Development roster"
          )
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
            bsicons::bs_icon("bullseye")
          ),
          shiny::span("TEAM HEADLINES")
        ),
        
        shiny::uiOutput(
          ns("team_headlines")
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
          ns("team_risks")
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
          ns("team_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Core players + conference table
    # --------------------------------------------------------
    
    shiny::div(
      class = "team-v2-profile-grid",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "CORE PERSONNEL"
            ),
            shiny::h3(
              "Highest current cap commitments"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "TOP 3"
          )
        ),
        
        shiny::uiOutput(
          ns("core_players")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "LEAGUE CONTEXT"
            ),
            shiny::h3(
              shiny::textOutput(
                ns("standings_heading"),
                inline = TRUE
              )
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "CONFERENCE"
          )
        ),
        
        shiny::div(
          class = "team-v2-table-wrap",
          reactable::reactableOutput(
            ns("conference_standings")
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Recommendation
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
            "Recommended organizational posture"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "DECISION SUPPORT"
        )
      ),
      
      shiny::uiOutput(
        ns("team_recommendation")
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Team Overview server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Optional reactive selected season.
#' @noRd
mod_team_overview_server <- function(
    id,
    selected_team,
    selected_season = NULL,
    transaction_state = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
      safe_num <- function(x, default = NA_real_) {
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
      
      money <- function(x) {
        value <- safe_num(
          x,
          NA_real_
        )
        
        if (is.na(value)) {
          return("—")
        }
        
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
      
      current_season <- shiny::reactive({
        if (
          !is.null(selected_season) &&
          is.function(selected_season)
        ) {
          value <- selected_season()
          
          if (
            !is.null(value) &&
            length(value) &&
            nzchar(
              as.character(
                value[[1]]
              )
            )
          ) {
            return(
              as.character(
                value[[1]]
              )
            )
          }
        }
        
        "2026-27"
      })
      
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
            class = "team-cba-link"
          )
        } else {
          shiny::span(label)
        }
        
        shiny::div(
          class = "team-v2-signal-row",
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
      # Standings DB path
      # ------------------------------------------------------
      
      standings_db_path <- function() {
        
        development_paths <- c(
          file.path(
            "inst",
            "app",
            "data",
            "basketball_ops.duckdb"
          ),
          file.path(
            "inst",
            "data",
            "basketball_ops.duckdb"
          ),
          file.path(
            "data",
            "basketball_ops.duckdb"
          )
        )
        
        existing <- development_paths[
          file.exists(
            development_paths
          )
        ]
        
        if (length(existing)) {
          return(
            existing[[1]]
          )
        }
        
        installed <- system.file(
          "app/data/basketball_ops.duckdb",
          package = utils::packageName(),
          mustWork = FALSE
        )
        
        if (
          nzchar(installed) &&
          file.exists(installed)
        ) {
          return(installed)
        }
        
        ""
      }
      
      # ------------------------------------------------------
      # Standings
      # ------------------------------------------------------
      
      standings_table <- shiny::reactive({
        shiny::req(
          selected_team()
        )
        
        path <- standings_db_path()
        
        if (
          !nzchar(path) ||
          !file.exists(path) ||
          !requireNamespace(
            "duckdb",
            quietly = TRUE
          )
        ) {
          return(NULL)
        }
        
        con <- tryCatch(
          DBI::dbConnect(
            duckdb::duckdb(),
            dbdir = path,
            read_only = TRUE
          ),
          error = function(e) NULL
        )
        
        if (is.null(con)) {
          return(NULL)
        }
        
        on.exit(
          DBI::dbDisconnect(
            con,
            shutdown = TRUE
          ),
          add = TRUE
        )
        
        result <- tryCatch(
          DBI::dbGetQuery(
            con,
            "
            SELECT
              team_name,
              wins,
              losses,
              win_pct,
              conference_rank,
              division_rank,
              points_per_game,
              point_diff,
              conference
            FROM standings
            "
          ),
          error = function(e) NULL
        )
        
        if (
          is.null(result) ||
          !nrow(result)
        ) {
          return(result)
        }
        
        numeric_fields <- c(
          "wins",
          "losses",
          "win_pct",
          "conference_rank",
          "division_rank",
          "points_per_game",
          "point_diff"
        )
        
        for (
          field in intersect(
            numeric_fields,
            names(result)
          )
        ) {
          result[[field]] <-
            suppressWarnings(
              as.numeric(
                result[[field]]
              )
            )
        }
        
        # Fill missing conference rank deterministically.
        for (
          conference_name in unique(
            result$conference
          )
        ) {
          idx <- which(
            result$conference ==
              conference_name
          )
          
          current_rank <-
            result$conference_rank[idx]
          
          if (
            length(idx) &&
            any(
              is.na(
                current_rank
              )
            )
          ) {
            win_pct <-
              result$win_pct[idx]
            
            wins <-
              result$wins[idx]
            
            point_diff <-
              result$point_diff[idx]
            
            win_pct[
              is.na(
                win_pct
              )
            ] <- -Inf
            
            wins[
              is.na(
                wins
              )
            ] <- -Inf
            
            point_diff[
              is.na(
                point_diff
              )
            ] <- -Inf
            
            ord <- order(
              -win_pct,
              -wins,
              -point_diff,
              result$team_name[idx]
            )
            
            computed <- integer(
              length(idx)
            )
            
            computed[ord] <-
              seq_along(ord)
            
            missing <- is.na(
              current_rank
            )
            
            current_rank[missing] <-
              computed[missing]
            
            result$conference_rank[idx] <-
              current_rank
          }
        }
        
        result
      })
      
      team_data <- shiny::reactive({
        standings <- standings_table()
        
        if (
          is.null(standings) ||
          !nrow(standings)
        ) {
          return(NULL)
        }
        
        row <- standings[
          standings$team_name ==
            selected_team(),
          ,
          drop = FALSE
        ]
        
        if (!nrow(row)) {
          return(NULL)
        }
        
        row[
          1,
          ,
          drop = FALSE
        ]
      })
      
      conference_data <- shiny::reactive({
        standings <- standings_table()
        team <- team_data()
        
        if (
          is.null(standings) ||
          is.null(team)
        ) {
          return(NULL)
        }
        
        result <- standings[
          standings$conference ==
            team$conference[[1]],
          c(
            "conference_rank",
            "team_name",
            "wins",
            "losses",
            "win_pct",
            "point_diff"
          ),
          drop = FALSE
        ]
        
        result[
          order(
            result$conference_rank,
            -result$wins
          ),
          ,
          drop = FALSE
        ]
      })
      
      # ------------------------------------------------------
      # Roster / contract data
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
            as.character(scenario$scenario_type),
            "trade"
          ) ||
          !identical(
            as.character(scenario$season),
            as.character(current_season())
          )
        ) {
          return(NULL)
        }
        
        selected <- as.character(selected_team())
        primary_team <- as.character(scenario$team)
        partner_team_name <- as.character(scenario$partner_team)
        
        if (
          !selected %in%
          c(primary_team, partner_team_name)
        ) {
          return(NULL)
        }
        
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
      
      
      base_roster_contracts <- shiny::reactive({
        shiny::req(
          selected_team()
        )
        
        if (
          !exists(
            "connect_db",
            mode = "function"
          )
        ) {
          return(
            data.frame()
          )
        }
        
        con <- tryCatch(
          connect_db(),
          error = function(e) NULL
        )
        
        if (is.null(con)) {
          return(
            data.frame()
          )
        }
        
        on.exit(
          try(
            disconnect_db(con),
            silent = TRUE
          ),
          add = TRUE
        )
        
        result <- tryCatch(
          DBI::dbGetQuery(
            con,
            "
            SELECT DISTINCT
              p.player_id,
              p.player_name,
              p.primary_position,
              p.player_age,
              t.team_name,
              t.abbreviation,
              COALESCE(rh.two_way_flag,0) AS two_way_flag,
              rh.roster_status,
              cy.cap_hit,
              cy.guaranteed_amount,
              cy.option_type,
              c.contract_type,
              c.contract_end_season,
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
              current_season()
            )
          ),
          error = function(e) {
            data.frame()
          }
        )
        
        result
      })
      
      roster_contracts <- shiny::reactive({
        
        current <- base_roster_contracts()
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
        
        incoming_ids <- if (
          is.data.frame(scenario$incoming_players) &&
          nrow(scenario$incoming_players) &&
          "player_id" %in%
          names(scenario$incoming_players)
        ) {
          suppressWarnings(
            as.integer(
              scenario$incoming_players$player_id
            )
          )
        } else {
          integer()
        }
        
        incoming_ids <- incoming_ids[
          !is.na(incoming_ids)
        ]
        
        if (
          length(incoming_ids) &&
          "player_id" %in% names(preview)
        ) {
          idx <- preview$player_id %in%
            incoming_ids
          
          if ("team_name" %in% names(preview)) {
            preview$team_name[idx] <- selected_team()
          }
          
          if ("roster_status" %in% names(preview)) {
            preview$roster_status[idx] <- "Trade Scenario"
          }
        }
        
        preview
      })
      
      
      output$team_trade_scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        base_data <- base_roster_contracts()
        
        base_payroll <- if (nrow(base_data)) {
          sum(
            suppressWarnings(
              as.numeric(base_data$cap_hit)
            ),
            na.rm = TRUE
          )
        } else {
          0
        }
        
        projected_payroll <- if (
          exists(
            "tbi_apply_trade_scenario_to_payroll",
            mode = "function"
          )
        ) {
          tbi_apply_trade_scenario_to_payroll(
            current_payroll = base_payroll,
            transaction_state = transaction_state,
            team_name = selected_team()
          )
        } else {
          payroll()
        }
        
        delta <-
          projected_payroll -
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
          "Payroll neutral"
        } else if (
          delta > 0
        ) {
          paste0(
            "+",
            money(delta),
            " payroll"
          )
        } else {
          paste0(
            "-",
            money(abs(delta)),
            " payroll"
          )
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
          class = "team-v2-trade-banner",
          
          shiny::div(
            class = "team-v2-trade-copy",
            
            shiny::span(
              class = "team-v2-trade-chip",
              bsicons::bs_icon("arrow-left-right"),
              "TRADE SCENARIO"
            ),
            
            paste0(
              selected_team(),
              " versus ",
              scenario$partner_team,
              ": ",
              outgoing_count,
              " out / ",
              incoming_count,
              " in. Team Overview metrics below reflect the proposed roster."
            )
          ),
          
          shiny::span(
            class = paste(
              "team-v2-trade-delta",
              delta_class
            ),
            delta_text
          )
        )
      })
      
      
      # ------------------------------------------------------
      # Team metrics
      # ------------------------------------------------------
      
      payroll <- shiny::reactive({
        d <- roster_contracts()
        
        if (!nrow(d)) {
          return(0)
        }
        
        sum(
          suppressWarnings(
            as.numeric(
              d$cap_hit
            )
          ),
          na.rm = TRUE
        )
      })
      
      average_age <- shiny::reactive({
        d <- roster_contracts()
        
        if (!nrow(d)) {
          return(NA_real_)
        }
        
        ages <- suppressWarnings(
          as.numeric(
            d$player_age
          )
        )
        
        if (
          !length(ages) ||
          all(
            is.na(
              ages
            )
          )
        ) {
          NA_real_
        } else {
          mean(
            ages,
            na.rm = TRUE
          )
        }
      })
      
      top_three_concentration <- shiny::reactive({
        d <- roster_contracts()
        total <- payroll()
        
        if (
          !nrow(d) ||
          total <= 0
        ) {
          return(NA_real_)
        }
        
        hits <- sort(
          suppressWarnings(
            as.numeric(
              d$cap_hit
            )
          ),
          decreasing = TRUE
        )
        
        hits[
          is.na(
            hits
          )
        ] <- 0
        
        sum(
          utils::head(
            hits,
            3
          )
        ) /
          total
      })
      
      near_term_free_agents <- shiny::reactive({
        d <- roster_contracts()
        
        if (!nrow(d)) {
          return(0L)
        }
        
        season_year <- suppressWarnings(
          as.integer(
            substr(
              current_season(),
              1,
              4
            )
          )
        )
        
        if (is.na(season_year)) {
          return(0L)
        }
        
        fa <- suppressWarnings(
          as.numeric(
            d$free_agent_year
          )
        )
        
        sum(
          !is.na(fa) &
            fa <=
            season_year + 2L
        )
      })
      
      team_options <- shiny::reactive({
        d <- roster_contracts()
        
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
      
      two_way_count <- shiny::reactive({
        d <- roster_contracts()
        
        if (!nrow(d)) {
          return(0L)
        }
        
        sum(
          suppressWarnings(
            as.numeric(
              d$two_way_flag
            )
          ) ==
            1,
          na.rm = TRUE
        )
      })
      
      # ------------------------------------------------------
      # Competitive assessment
      # ------------------------------------------------------
      
      competitive_status <- shiny::reactive({
        team <- team_data()
        
        if (is.null(team)) {
          return("Standings unavailable")
        }
        
        rank <- safe_num(
          team$conference_rank,
          NA_real_
        )
        
        win_pct <- safe_num(
          team$win_pct,
          NA_real_
        )
        
        diff <- safe_num(
          team$point_diff,
          NA_real_
        )
        
        if (
          !is.na(rank) &&
          rank <= 3 &&
          !is.na(win_pct) &&
          win_pct >= .600 &&
          !is.na(diff) &&
          diff >= 5
        ) {
          "CHAMPIONSHIP CONTENDER"
        } else if (
          !is.na(rank) &&
          rank <= 6
        ) {
          "PLAYOFF POSITION"
        } else if (
          !is.na(rank) &&
          rank <= 10
        ) {
          "PLAY-IN POSITION"
        } else {
          "DEVELOPMENT / LOTTERY POSITION"
        }
      })
      
      organization_metrics <- shiny::reactive({
        team <- team_data()
        
        rank <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$conference_rank,
            NA_real_
          )
        }
        
        win_pct <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$win_pct,
            NA_real_
          )
        }
        
        diff <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$point_diff,
            NA_real_
          )
        }
        
        competitive <- if (
          is.na(rank)
        ) {
          50
        } else {
          score_clamp(
            100 -
              (rank - 1) *
              6
          )
        }
        
        performance <- if (
          is.na(win_pct)
        ) {
          50
        } else {
          score_clamp(
            win_pct *
              100
          )
        }
        
        margin <- if (
          is.na(diff)
        ) {
          50
        } else {
          score_clamp(
            50 +
              diff *
              5
          )
        }
        
        concentration <-
          top_three_concentration()
        
        roster_balance <- if (
          is.na(concentration)
        ) {
          50
        } else {
          score_clamp(
            100 -
              max(
                0,
                concentration - .40
              ) *
              140
          )
        }
        
        continuity <- score_clamp(
          100 -
            near_term_free_agents() *
            7
        )
        
        age <- average_age()
        
        timeline <- if (
          is.na(age)
        ) {
          50
        } else if (
          age <= 26.5
        ) {
          84
        } else if (
          age <= 28.5
        ) {
          72
        } else if (
          age <= 30.5
        ) {
          55
        } else {
          38
        }
        
        composite <- mean(
          c(
            competitive,
            performance,
            margin,
            roster_balance,
            continuity,
            timeline
          ),
          na.rm = TRUE
        )
        
        list(
          competitive = competitive,
          performance = performance,
          margin = margin,
          roster_balance = roster_balance,
          continuity = continuity,
          timeline = timeline,
          composite = composite
        )
      })
      
      # ------------------------------------------------------
      # Identity
      # ------------------------------------------------------
      
      output$team_name <- shiny::renderText({
        selected_team()
      })
      
      output$team_abbreviation <- shiny::renderText({
        d <- roster_contracts()
        
        if (
          nrow(d) &&
          "abbreviation" %in%
          names(d)
        ) {
          return(
            text_or(
              d$abbreviation,
              substr(
                selected_team(),
                1,
                3
              )
            )
          )
        }
        
        paste(
          substr(
            strsplit(
              selected_team(),
              "\\s+"
            )[[1]],
            1,
            1
          ),
          collapse = ""
        )
      })
      
      output$team_context <- shiny::renderText({
        team <- team_data()
        
        conference <- if (
          is.null(team)
        ) {
          "Conference unavailable"
        } else {
          paste0(
            text_or(
              team$conference,
              "—"
            ),
            " Conference"
          )
        }
        
        paste(
          conference,
          current_season(),
          sep = " • "
        )
      })
      
      output$competitive_status <- shiny::renderText({
        competitive_status()
      })
      
      # ------------------------------------------------------
      # Snapshot
      # ------------------------------------------------------
      
      output$snapshot_record <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          return("—")
        }
        
        paste0(
          safe_num(
            team$wins,
            0
          ),
          "-",
          safe_num(
            team$losses,
            0
          )
        )
      })
      
      output$snapshot_conference_rank <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          return("—")
        }
        
        rank <- safe_num(
          team$conference_rank,
          NA_real_
        )
        
        if (is.na(rank)) {
          "—"
        } else {
          paste0(
            "#",
            round(rank)
          )
        }
      })
      
      output$snapshot_point_diff <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          return("—")
        }
        
        diff <- safe_num(
          team$point_diff,
          NA_real_
        )
        
        if (is.na(diff)) {
          "—"
        } else {
          sprintf(
            "%+.1f",
            diff
          )
        }
      })
      
      output$snapshot_payroll <- shiny::renderText({
        money(
          payroll()
        )
      })
      
      output$snapshot_roster_size <- shiny::renderText({
        nrow(
          roster_contracts()
        )
      })
      
      output$snapshot_average_age <- shiny::renderText({
        age <- average_age()
        
        if (is.na(age)) {
          "—"
        } else {
          sprintf(
            "%.1f",
            age
          )
        }
      })
      
      # ------------------------------------------------------
      # Readout
      # ------------------------------------------------------
      
      output$organization_readout <- shiny::renderUI({
        team <- team_data()
        
        conference_value <- if (
          is.null(team)
        ) {
          "—"
        } else {
          text_or(
            team$conference
          )
        }
        
        division_rank <- if (
          is.null(team)
        ) {
          "—"
        } else {
          rank <- safe_num(
            team$division_rank,
            NA_real_
          )
          
          if (
            is.na(rank)
          ) {
            "—"
          } else {
            paste0(
              "#",
              round(rank)
            )
          }
        }
        
        shiny::tagList(
          signal_row(
            "Competitive status",
            competitive_status()
          ),
          signal_row(
            "Conference",
            conference_value
          ),
          signal_row(
            "Division rank",
            division_rank
          ),
          signal_row(
            "Roster size",
            as.character(
              nrow(
                roster_contracts()
              )
            )
          ),
          signal_row(
            "Near-term free agents",
            as.character(
              near_term_free_agents()
            )
          ),
          signal_row(
            "Team options",
            as.character(
              team_options()
            ),
            cba_term = "Team Option"
          )
        )
      })
      
      # ------------------------------------------------------
      # Decision
      # ------------------------------------------------------
      
      output$team_decision <- shiny::renderUI({
        metrics <- organization_metrics()
        team <- team_data()
        
        rank <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$conference_rank,
            NA_real_
          )
        }
        
        label <- if (
          !is.na(rank) &&
          rank <= 3
        ) {
          "PUSH SELECTIVELY"
        } else if (
          !is.na(rank) &&
          rank <= 6
        ) {
          "UPGRADE AROUND CORE"
        } else if (
          !is.na(rank) &&
          rank <= 10
        ) {
          "RAISE THE CEILING"
        } else {
          "PRESERVE FLEXIBILITY"
        }
        
        explanation <- if (
          label ==
          "PUSH SELECTIVELY"
        ) {
          paste(
            "Competitive position supports targeted win-now moves,",
            "but premium long-term assets should only move for clear impact."
          )
        } else if (
          label ==
          "UPGRADE AROUND CORE"
        ) {
          paste(
            "The team is positioned in the playoff field.",
            "Prioritize upgrades that strengthen the current rotation without compromising future control."
          )
        } else if (
          label ==
          "RAISE THE CEILING"
        ) {
          paste(
            "The organization is within postseason range but needs a clearer path to higher-end performance.",
            "Compare targeted upgrades against development and flexibility."
          )
        } else {
          paste(
            "The current competitive position does not justify aggressive asset consolidation.",
            "Prioritize development, optionality, and controllable value."
          )
        }
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  metrics$composite >= 60
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
                "ORG SCORE"
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
                competitive_status()
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "PRIMARY WATCH"
              ),
              shiny::strong(
                if (
                  metrics$continuity <
                  metrics$roster_balance
                ) {
                  "Continuity"
                } else {
                  "Roster Balance"
                }
              ),
              shiny::tags$small(
                "Decision-support signal"
              )
            )
          )
        )
      })
      
      output$organization_score <- shiny::renderText({
        sprintf(
          "%.0f",
          organization_metrics()$composite
        )
      })
      
      # ------------------------------------------------------
      # Scorecard
      # ------------------------------------------------------
      
      output$organization_scorecard <- shiny::renderUI({
        metrics <- organization_metrics()
        
        shiny::tagList(
          score_row(
            "Competitive Position",
            "Conference standing",
            "bullseye",
            metrics$competitive
          ),
          
          score_row(
            "Current Performance",
            "Win percentage",
            "graph-up-arrow",
            metrics$performance
          ),
          
          score_row(
            "Scoring Margin",
            "Point differential",
            "graph-up-arrow",
            metrics$margin
          ),
          
          score_row(
            "Roster Balance",
            "Top-three salary concentration",
            "people",
            metrics$roster_balance
          ),
          
          score_row(
            "Roster Continuity",
            "Near-term free-agency exposure",
            "calendar3",
            metrics$continuity
          ),
          
          score_row(
            "Age Timeline",
            "Current roster age curve",
            "person-badge",
            metrics$timeline
          )
        )
      })
      
      # ------------------------------------------------------
      # Team profile
      # ------------------------------------------------------
      
      output$profile_win_pct <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          return("—")
        }
        
        pct <- safe_num(
          team$win_pct,
          NA_real_
        )
        
        if (is.na(pct)) {
          "—"
        } else {
          sprintf(
            "%.3f",
            pct
          )
        }
      })
      
      output$profile_scoring <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          return("—")
        }
        
        value <- safe_num(
          team$points_per_game,
          NA_real_
        )
        
        if (is.na(value)) {
          "—"
        } else {
          paste0(
            sprintf(
              "%.1f",
              value
            ),
            " PPG"
          )
        }
      })
      
      output$profile_top3_concentration <- shiny::renderText({
        value <-
          top_three_concentration()
        
        if (is.na(value)) {
          "—"
        } else {
          sprintf(
            "%.1f%%",
            value *
              100
          )
        }
      })
      
      output$profile_two_ways <- shiny::renderText({
        two_way_count()
      })
      
      # ------------------------------------------------------
      # Headlines
      # ------------------------------------------------------
      
      output$team_headlines <- shiny::renderUI({
        team <- team_data()
        
        point_diff <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$point_diff,
            NA_real_
          )
        }
        
        headlines <- c(
          paste0(
            competitive_status(),
            " based on the current standings profile."
          ),
          paste0(
            "Current payroll is ",
            money(
              payroll()
            ),
            " across ",
            nrow(
              roster_contracts()
            ),
            " loaded roster records."
          ),
          paste0(
            near_term_free_agents(),
            " players reach free agency within the near-term planning window."
          ),
          if (
            is.na(point_diff)
          ) {
            "Point-differential context is unavailable."
          } else {
            paste0(
              "Current scoring margin is ",
              sprintf(
                "%+.1f",
                point_diff
              ),
              " points per game."
            )
          }
        )
        
        shiny::tagList(
          lapply(
            headlines,
            function(item) {
              shiny::div(
                class = "tbi-v2-headline-row",
                shiny::span(
                  class = "tbi-v2-headline-dot"
                ),
                shiny::span(item)
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Risks
      # ------------------------------------------------------
      
      output$team_risks <- shiny::renderUI({
        metrics <- organization_metrics()
        risks <- character()
        
        if (
          near_term_free_agents() >= 6
        ) {
          risks <- c(
            risks,
            paste0(
              near_term_free_agents(),
              " near-term free agents create meaningful roster-turnover exposure."
            )
          )
        }
        
        concentration <-
          top_three_concentration()
        
        if (
          !is.na(concentration) &&
          concentration >= .60
        ) {
          risks <- c(
            risks,
            sprintf(
              "The top three cap hits account for %.1f%% of current payroll.",
              concentration *
                100
            )
          )
        }
        
        if (
          metrics$competitive < 45
        ) {
          risks <- c(
            risks,
            "Current competitive position does not support aggressive short-term asset spending."
          )
        }
        
        if (
          metrics$timeline < 50
        ) {
          risks <- c(
            risks,
            "Roster age profile creates increased long-range performance risk."
          )
        }
        
        if (!length(risks)) {
          risks <- "No major structural team-level risk is identified by the currently loaded inputs."
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
      
      output$team_opportunities <- shiny::renderUI({
        metrics <- organization_metrics()
        opportunities <- character()
        
        if (
          metrics$competitive >= 70
        ) {
          opportunities <- c(
            opportunities,
            "Competitive positioning supports targeted win-now evaluation."
          )
        }
        
        if (
          team_options() > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              team_options(),
              " team-option year",
              if (
                team_options() == 1
              ) "" else "s",
              " preserve future roster control."
            )
          )
        }
        
        if (
          metrics$timeline >= 70
        ) {
          opportunities <- c(
            opportunities,
            "The current age profile supports a multi-year competitive window."
          )
        }
        
        if (
          !is.na(
            top_three_concentration()
          ) &&
          top_three_concentration() <
          .50
        ) {
          opportunities <- c(
            opportunities,
            "Salary distribution is relatively balanced across the loaded roster."
          )
        }
        
        if (!length(opportunities)) {
          opportunities <- paste(
            "Preserve flexibility while comparing",
            "development, transaction, and contract pathways."
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
      # Core players
      # ------------------------------------------------------
      
      output$core_players <- shiny::renderUI({
        d <- roster_contracts()
        
        if (!nrow(d)) {
          return(
            shiny::div(
              style = "padding:16px; color:#7f8fa4;",
              "Roster contract data is unavailable."
            )
          )
        }
        
        hits <- suppressWarnings(
          as.numeric(
            d$cap_hit
          )
        )
        
        hits[
          is.na(
            hits
          )
        ] <- 0
        
        ord <- order(
          hits,
          decreasing = TRUE
        )
        
        top <- d[
          utils::head(
            ord,
            3
          ),
          ,
          drop = FALSE
        ]
        
        shiny::div(
          class = "team-v2-core-grid",
          
          lapply(
            seq_len(
              nrow(top)
            ),
            function(i) {
              
              shiny::div(
                class = "team-v2-core-card",
                
                shiny::div(
                  class = "team-v2-core-rank",
                  paste0(
                    "CORE ",
                    i
                  )
                ),
                
                shiny::strong(
                  text_or(
                    top$player_name[[i]]
                  )
                ),
                
                shiny::span(
                  paste(
                    text_or(
                      top$primary_position[[i]]
                    ),
                    money(
                      top$cap_hit[[i]]
                    ),
                    sep = " • "
                  )
                ),
                
                shiny::span(
                  paste(
                    text_or(
                      top$contract_type[[i]],
                      "Contract not classified"
                    ),
                    paste0(
                      "FA ",
                      text_or(
                        top$free_agent_year[[i]]
                      )
                    ),
                    sep = " • "
                  )
                )
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Conference standings
      # ------------------------------------------------------
      
      output$standings_heading <- shiny::renderText({
        team <- team_data()
        
        if (is.null(team)) {
          "Conference Standings"
        } else {
          paste(
            text_or(
              team$conference
            ),
            "Conference"
          )
        }
      })
      
      output$conference_standings <- reactable::renderReactable({
        standings <- conference_data()
        
        shiny::validate(
          shiny::need(
            !is.null(standings) &&
              nrow(standings) > 0,
            "Conference standings are unavailable."
          )
        )
        
        display <- data.frame(
          Rank = standings$conference_rank,
          Team = standings$team_name,
          W = standings$wins,
          L = standings$losses,
          PCT = sprintf(
            "%.3f",
            standings$win_pct
          ),
          DIFF = sprintf(
            "%+.1f",
            standings$point_diff
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        reactable::reactable(
          display,
          searchable = FALSE,
          pagination = FALSE,
          compact = TRUE,
          striped = FALSE,
          highlight = TRUE,
          defaultSorted = "Rank",
          rowStyle = function(index) {
            if (
              identical(
                as.character(
                  display$Team[[index]]
                ),
                as.character(
                  selected_team()
                )
              )
            ) {
              list(
                background = "rgba(59,130,246,.085)",
                fontWeight = 700
              )
            } else {
              NULL
            }
          },
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
      # Recommendation
      # ------------------------------------------------------
      
      output$team_recommendation <- shiny::renderUI({
        team <- team_data()
        metrics <- organization_metrics()
        
        rank <- if (
          is.null(team)
        ) {
          NA_real_
        } else {
          safe_num(
            team$conference_rank,
            NA_real_
          )
        }
        
        label <- if (
          !is.na(rank) &&
          rank <= 3
        ) {
          "CONTEND WITH DISCIPLINE"
        } else if (
          !is.na(rank) &&
          rank <= 6
        ) {
          "UPGRADE THE ROTATION"
        } else if (
          !is.na(rank) &&
          rank <= 10
        ) {
          "IMPROVE WITHOUT OVERCOMMITTING"
        } else {
          "DEVELOP + PRESERVE ASSETS"
        }
        
        rationale <- if (
          label ==
          "CONTEND WITH DISCIPLINE"
        ) {
          paste(
            "The current competitive profile supports championship-oriented evaluation.",
            "Prioritize acquisitions that materially improve playoff outcomes while protecting future flexibility."
          )
        } else if (
          label ==
          "UPGRADE THE ROTATION"
        ) {
          paste(
            "The team is positioned as a playoff-level organization.",
            "Target specific rotation improvements instead of broad roster churn."
          )
        } else if (
          label ==
          "IMPROVE WITHOUT OVERCOMMITTING"
        ) {
          paste(
            "The team is within postseason range but does not yet justify aggressive premium-asset spending.",
            "Seek upgrades with controllable cost and upside."
          )
        } else {
          paste(
            "The current competitive profile favors development and optionality.",
            "Prioritize player growth, controllable contracts, draft capital, and future decision flexibility."
          )
        }
        
        if (
          metrics$continuity < 50
        ) {
          rationale <- paste(
            rationale,
            "Near-term free-agency exposure should also be addressed before major long-range commitments."
          )
        }
        
        shiny::div(
          class = "team-v2-rec-grid",
          
          shiny::div(
            class = "team-v2-rec-box",
            shiny::span(
              "RECOMMENDATION"
            ),
            shiny::strong(
              label
            )
          ),
          
          shiny::div(
            shiny::h3(
              style = "margin:0 0 7px;",
              rationale
            ),
            
            shiny::p(
              class = "team-v2-note",
              style = "margin:0;",
              paste(
                "Team Overview combines current standings with loaded roster and contract information.",
                "It is a decision-support summary rather than a substitute for scouting, medical, cap, or transaction review."
              )
            )
          )
        )
      })
    }
  )
}