# ------------------------------------------------------------
# Module: Draft Intelligence
# Version 2 Executive Draft Capital Workspace
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Draft Intelligence UI
#'
#' @param id Internal module ID.
#' @noRd
mod_draft_assets_ui <- function(id) {
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
  
  shiny::div(
    class = "tbi-module-page tbi-v2-draft-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-v2-draft-page {
          display:grid;
          gap:12px;
        }

        .draft-v2-portfolio-grid {
          display:grid;
          grid-template-columns:minmax(315px,.72fr) minmax(0,1.28fr);
          gap:12px;
        }

        .draft-v2-value-hero {
          padding:18px;
          display:grid;
          grid-template-columns:105px minmax(0,1fr);
          gap:16px;
          align-items:center;
        }

        .draft-v2-score-ring {
          width:100px;
          height:100px;
          display:grid;
          place-items:center;
          border-radius:50%;
          border:2px solid rgba(96,165,250,.42);
          box-shadow:inset 0 0 0 8px rgba(59,130,246,.045);
          background:#101826;
        }

        .draft-v2-score-ring strong {
          display:block;
          color:#61a8ff;
          font-size:1.55rem;
          line-height:1;
          text-align:center;
        }

        .draft-v2-score-ring span {
          display:block;
          margin-top:4px;
          color:#718198;
          font-size:.48rem;
          font-weight:850;
          letter-spacing:.08em;
          text-align:center;
          text-transform:uppercase;
        }

        .draft-v2-grade {
          margin:4px 0;
          color:#f6f8fb;
          font-size:1.45rem;
          font-weight:850;
          letter-spacing:-.025em;
        }

        .draft-v2-summary {
          margin:5px 0 0;
          color:#9aa8ba;
          font-size:.67rem;
          line-height:1.5;
        }

        .draft-v2-year-grid {
          padding:13px 15px 15px;
          display:grid;
          grid-template-columns:repeat(5,minmax(0,1fr));
          gap:8px;
        }

        .draft-v2-year-card {
          min-height:90px;
          padding:11px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:9px;
          background:rgba(255,255,255,.012);
        }

        .draft-v2-year-card strong {
          display:block;
          color:#f3f6fa;
          font-size:.88rem;
        }

        .draft-v2-year-card span {
          display:block;
          margin-top:4px;
          color:#77879b;
          font-size:.57rem;
          line-height:1.4;
        }

        .draft-v2-year-value {
          margin-top:9px !important;
          color:#61a8ff !important;
          font-size:.74rem !important;
          font-weight:800;
        }

        .draft-v2-signal-row {
          min-height:39px;
          padding:7px 0;
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:12px;
          border-bottom:1px solid rgba(148,163,184,.08);
          color:#8796aa;
          font-size:.63rem;
        }

        .draft-v2-signal-row:last-child {
          border-bottom:0;
        }

        .draft-v2-signal-row strong {
          color:#eef3f8;
          text-align:right;
        }

        .draft-v2-message {
          display:flex;
          gap:8px;
          margin:7px 0;
          color:#c6d0dd;
          font-size:.62rem;
          line-height:1.45;
        }

        .draft-v2-dot {
          width:6px;
          height:6px;
          flex:0 0 6px;
          margin-top:5px;
          border-radius:50%;
          background:#60a5fa;
        }

        .draft-v2-dot.warning { background:#f59e0b; }
        .draft-v2-dot.danger { background:#fb7185; }
        .draft-v2-dot.success { background:#34d399; }

        .draft-v2-table-wrap {
          max-height:480px;
          overflow:auto;
        }

        .draft-v2-rec-grid {
          padding:16px;
          display:grid;
          grid-template-columns:minmax(220px,.42fr) minmax(0,1.58fr);
          gap:18px;
          align-items:center;
        }

        .draft-v2-rec-box {
          min-height:105px;
          padding:15px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:rgba(59,130,246,.045);
        }

        .draft-v2-rec-box span {
          color:#718198;
          font-size:.52rem;
          font-weight:850;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .draft-v2-rec-box strong {
          margin-top:6px;
          color:#61a8ff;
          font-size:1.2rem;
        }

        @media(max-width:1050px) {
          .draft-v2-portfolio-grid {
            grid-template-columns:1fr;
          }

          .draft-v2-year-grid {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }
        }

        @media(max-width:680px) {
          .draft-v2-value-hero,
          .draft-v2-rec-grid {
            grid-template-columns:1fr;
          }

          .draft-v2-year-grid {
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
          "ASSET STRATEGY"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Draft Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Evaluate draft capital, protection exposure, control rights,",
            "portfolio value, verification risk, and future transaction flexibility."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "DRAFT MODEL"
        ),
        shiny::strong("Phase 5")
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
            bsicons::bs_icon("calendar3")
          ),
          shiny::span("DRAFT CAPITAL SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "CURRENT PORTFOLIO"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "TOTAL ASSETS",
          "snapshot_total_assets",
          "calendar3",
          "blue"
        ),
        
        snapshot_item(
          "1ST ROUND",
          "snapshot_firsts",
          "bullseye",
          "green"
        ),
        
        snapshot_item(
          "2ND ROUND",
          "snapshot_seconds",
          "calendar3",
          "blue"
        ),
        
        snapshot_item(
          "SWAP RIGHTS",
          "snapshot_swaps",
          "arrow-left-right",
          "purple"
        ),
        
        snapshot_item(
          "NET VALUE",
          "snapshot_net_value",
          "graph-up-arrow",
          "blue"
        ),
        
        snapshot_item(
          "PORTFOLIO GRADE",
          "snapshot_grade",
          "bullseye",
          "green"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Decision + portfolio score
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
          
          shiny::span("DRAFT CAPITAL DECISION")
        ),
        
        shiny::uiOutput(
          ns("draft_decision")
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
            shiny::span("PORTFOLIO VALUE")
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Net"),
            shiny::strong(
              shiny::textOutput(
                ns("net_value_score"),
                inline = TRUE
              )
            )
          )
        ),
        
        shiny::uiOutput(
          ns("portfolio_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Portfolio intelligence
    # --------------------------------------------------------
    
    shiny::div(
      class = "draft-v2-portfolio-grid",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "PORTFOLIO STRENGTH"
            ),
            shiny::h3("Draft capital profile")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "VALUE MODEL"
          )
        ),
        
        shiny::uiOutput(
          ns("portfolio_value_hero")
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "FUTURE CONTROL"
            ),
            shiny::h3("Five-year asset timeline")
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "PLANNING WINDOW"
          )
        ),
        
        shiny::uiOutput(
          ns("asset_timeline")
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
            bsicons::bs_icon("calendar3")
          ),
          shiny::span("DRAFT HEADLINES")
        ),
        
        shiny::uiOutput(
          ns("draft_headlines")
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
          shiny::span("ASSET RISKS")
        ),
        
        shiny::uiOutput(
          ns("draft_risks")
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
          shiny::span("ASSET OPPORTUNITIES")
        ),
        
        shiny::uiOutput(
          ns("draft_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Asset ledger
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-context-panel",
      
      shiny::div(
        class = "tbi-v2-context-header",
        
        shiny::div(
          shiny::div(
            class = "tbi-page-eyebrow",
            "DRAFT ASSET LEDGER"
          ),
          shiny::h3("Controlled picks, swaps, and obligations")
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "VERIFIED + MODELED"
        )
      ),
      
      shiny::div(
        class = "draft-v2-table-wrap",
        reactable::reactableOutput(
          ns("draft_asset_table")
        )
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
          shiny::h3("Recommended draft-capital posture")
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "DECISION SUPPORT"
        )
      ),
      
      shiny::uiOutput(
        ns("draft_recommendation")
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Draft Intelligence server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Optional reactive selected season.
#' @noRd
mod_draft_assets_server <- function(
    id,
    selected_team,
    selected_season = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
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
      
      current_draft_year <- shiny::reactive({
        if (
          !is.null(selected_season) &&
          is.function(selected_season)
        ) {
          season_value <- selected_season()
          
          parsed <- suppressWarnings(
            as.integer(
              substr(
                season_value,
                1,
                4
              )
            )
          )
          
          if (!is.na(parsed)) {
            return(parsed + 1L)
          }
        }
        
        as.integer(
          format(
            Sys.Date(),
            "%Y"
          )
        )
      })
      
      message_rows <- function(
    messages,
    tone = "") {
        
        messages <- unique(
          as.character(
            messages %||% character()
          )
        )
        
        messages <- messages[
          !is.na(messages) &
            nzchar(messages)
        ]
        
        if (!length(messages)) {
          messages <- "No additional item is currently identified."
        }
        
        shiny::tagList(
          lapply(
            messages,
            function(message) {
              shiny::div(
                class = "draft-v2-message",
                shiny::span(
                  class = paste(
                    "draft-v2-dot",
                    tone
                  )
                ),
                shiny::span(
                  message
                )
              )
            }
          )
        )
      }
      
      signal_row <- function(label, value) {
        shiny::div(
          class = "draft-v2-signal-row",
          shiny::span(label),
          shiny::strong(value)
        )
      }
      
      score_row <- function(
    label,
    subtitle,
    icon,
    score,
    rating = NULL) {
        
        score <- max(
          0,
          min(
            100,
            safe_num(
              score,
              0
            )
          )
        )
        
        tone <- if (
          score >= 75
        ) {
          "green"
        } else if (
          score >= 50
        ) {
          "blue"
        } else if (
          score >= 30
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
            score >= 50
          ) {
            "Stable"
          } else if (
            score >= 30
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
      # Engine results
      # ------------------------------------------------------
      
      draft_value_result <- shiny::reactive({
        shiny::req(
          selected_team()
        )
        
        tryCatch(
          evaluate_team_draft_value(
            team_value = selected_team(),
            year_from = current_draft_year(),
            year_to = current_draft_year() + 6L,
            current_year = current_draft_year()
          ),
          error = function(e) {
            structure(
              list(
                message = conditionMessage(e)
              ),
              class = "tbi_draft_error"
            )
          }
        )
      })
      
      draft_assets <- shiny::reactive({
        result <- draft_value_result()
        
        if (
          inherits(
            result,
            "tbi_draft_error"
          )
        ) {
          return(
            data.frame()
          )
        }
        
        result$assets
      })
      
      valued_assets <- shiny::reactive({
        result <- draft_value_result()
        
        if (
          inherits(
            result,
            "tbi_draft_error"
          )
        ) {
          return(
            data.frame()
          )
        }
        
        result$valued_assets
      })
      
      draft_summary <- shiny::reactive({
        result <- draft_value_result()
        
        if (
          inherits(
            result,
            "tbi_draft_error"
          )
        ) {
          return(
            list(
              gross_asset_value = 0,
              gross_obligation_value = 0,
              net_portfolio_value = 0,
              premium_assets = 0L,
              strong_assets = 0L,
              obligations = 0L,
              review_required = 0L,
              portfolio_grade = "Review",
              executive_summary = result$message
            )
          )
        }
        
        result$summary
      })
      
      asset_summary <- shiny::reactive({
        assets <- draft_assets()
        
        tryCatch(
          summarize_draft_assets(
            assets
          ),
          error = function(e) {
            list(
              total_assets = nrow(assets),
              controlled_first_round = 0L,
              controlled_second_round = 0L,
              outgoing_obligations = 0L,
              swap_rights = 0L,
              swap_obligations = 0L,
              high_value_assets = 0L,
              verified_assets = 0L,
              review_required = 0L,
              years_of_control = 0L,
              earliest_asset_year = NA_integer_,
              latest_asset_year = NA_integer_,
              portfolio_status = "Review",
              executive_summary = conditionMessage(e)
            )
          }
        )
      })
      
      # ------------------------------------------------------
      # Snapshot
      # ------------------------------------------------------
      
      output$snapshot_total_assets <- shiny::renderText({
        asset_summary()$total_assets
      })
      
      output$snapshot_firsts <- shiny::renderText({
        asset_summary()$controlled_first_round
      })
      
      output$snapshot_seconds <- shiny::renderText({
        asset_summary()$controlled_second_round
      })
      
      output$snapshot_swaps <- shiny::renderText({
        asset_summary()$swap_rights
      })
      
      output$snapshot_net_value <- shiny::renderText({
        sprintf(
          "%.1f",
          safe_num(
            draft_summary()$net_portfolio_value,
            0
          )
        )
      })
      
      output$snapshot_grade <- shiny::renderText({
        text_or(
          draft_summary()$portfolio_grade,
          "Unrated"
        )
      })
      
      output$net_value_score <- shiny::renderText({
        sprintf(
          "%.1f",
          safe_num(
            draft_summary()$net_portfolio_value,
            0
          )
        )
      })
      
      # ------------------------------------------------------
      # Decision
      # ------------------------------------------------------
      
      output$draft_decision <- shiny::renderUI({
        result <- draft_value_result()
        summary <- draft_summary()
        assets <- asset_summary()
        
        if (
          inherits(
            result,
            "tbi_draft_error"
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
                  "REVIEW DATA"
                ),
                
                shiny::p(
                  result$message
                )
              )
            )
          )
        }
        
        value <- safe_num(
          summary$net_portfolio_value,
          0
        )
        
        review_count <- safe_num(
          summary$review_required,
          0
        )
        
        decision <- if (
          value >= 150 &&
          review_count == 0
        ) {
          "PRESERVE OPTIONALITY"
        } else if (
          value >= 75
        ) {
          "LEVERAGE SELECTIVELY"
        } else if (
          value >= 20
        ) {
          "PROTECT CORE ASSETS"
        } else {
          "BUILD DRAFT CAPITAL"
        }
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  value >= 75
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
                summary$executive_summary
              )
            )
          ),
          
          shiny::div(
            class = "tbi-v2-decision-metrics",
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "PORTFOLIO GRADE"
              ),
              shiny::strong(
                summary$portfolio_grade
              ),
              shiny::tags$small(
                paste0(
                  round(value, 1),
                  " net value"
                )
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "CONTROL WINDOW"
              ),
              shiny::strong(
                paste0(
                  safe_num(
                    assets$years_of_control,
                    0
                  ),
                  " yrs"
                )
              ),
              shiny::tags$small(
                paste0(
                  assets$controlled_first_round,
                  " firsts / ",
                  assets$controlled_second_round,
                  " seconds"
                )
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Portfolio scorecard
      # ------------------------------------------------------
      
      output$portfolio_scorecard <- shiny::renderUI({
        summary <- draft_summary()
        assets <- asset_summary()
        
        net_value <- safe_num(
          summary$net_portfolio_value,
          0
        )
        
        value_score <- min(
          100,
          max(
            0,
            net_value / 2.5
          )
        )
        
        first_score <- min(
          100,
          safe_num(
            assets$controlled_first_round,
            0
          ) *
            20
        )
        
        second_score <- min(
          100,
          safe_num(
            assets$controlled_second_round,
            0
          ) *
            12
        )
        
        verification_score <- if (
          safe_num(
            assets$total_assets,
            0
          ) > 0
        ) {
          100 *
            safe_num(
              assets$verified_assets,
              0
            ) /
            safe_num(
              assets$total_assets,
              1
            )
        } else {
          0
        }
        
        obligation_score <- max(
          0,
          100 -
            safe_num(
              assets$outgoing_obligations,
              0
            ) *
            20
        )
        
        shiny::tagList(
          score_row(
            "Net Portfolio Value",
            "Internal draft-value estimate",
            "graph-up-arrow",
            value_score,
            summary$portfolio_grade
          ),
          
          score_row(
            "First-Round Control",
            "Controlled first-round inventory",
            "bullseye",
            first_score
          ),
          
          score_row(
            "Second-Round Depth",
            "Controlled second-round inventory",
            "calendar3",
            second_score
          ),
          
          score_row(
            "Verification Quality",
            "Records marked verified",
            "person-badge",
            verification_score
          ),
          
          score_row(
            "Obligation Exposure",
            "Outgoing pick and swap burden",
            "arrow-left-right",
            obligation_score
          )
        )
      })
      
      # ------------------------------------------------------
      # Portfolio hero
      # ------------------------------------------------------
      
      output$portfolio_value_hero <- shiny::renderUI({
        summary <- draft_summary()
        assets <- asset_summary()
        
        net_value <- safe_num(
          summary$net_portfolio_value,
          0
        )
        
        shiny::tagList(
          shiny::div(
            class = "draft-v2-value-hero",
            
            shiny::div(
              class = "draft-v2-score-ring",
              shiny::div(
                shiny::strong(
                  sprintf(
                    "%.0f",
                    net_value
                  )
                ),
                shiny::span(
                  "NET VALUE"
                )
              )
            ),
            
            shiny::div(
              shiny::div(
                class = "tbi-page-eyebrow",
                "PORTFOLIO GRADE"
              ),
              
              shiny::div(
                class = "draft-v2-grade",
                summary$portfolio_grade
              ),
              
              shiny::p(
                class = "draft-v2-summary",
                summary$executive_summary
              )
            )
          ),
          
          shiny::div(
            style = "padding:0 16px 14px;",
            
            signal_row(
              "Gross asset value",
              sprintf(
                "%.1f",
                safe_num(
                  summary$gross_asset_value,
                  0
                )
              )
            ),
            
            signal_row(
              "Obligation value",
              sprintf(
                "%.1f",
                safe_num(
                  summary$gross_obligation_value,
                  0
                )
              )
            ),
            
            signal_row(
              "Premium assets",
              as.character(
                safe_num(
                  summary$premium_assets,
                  0
                )
              )
            ),
            
            signal_row(
              "Strong assets",
              as.character(
                safe_num(
                  summary$strong_assets,
                  0
                )
              )
            ),
            
            signal_row(
              "Manual review",
              as.character(
                safe_num(
                  summary$review_required,
                  0
                )
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Timeline
      # ------------------------------------------------------
      
      output$asset_timeline <- shiny::renderUI({
        valued <- valued_assets()
        assets <- draft_assets()
        
        years <- seq(
          current_draft_year(),
          current_draft_year() + 4L
        )
        
        cards <- lapply(
          years,
          function(year) {
            
            year_rows <- if (
              nrow(assets)
            ) {
              assets[
                assets$draft_year == year,
                ,
                drop = FALSE
              ]
            } else {
              data.frame()
            }
            
            valued_rows <- if (
              nrow(valued)
            ) {
              valued[
                valued$draft_year == year,
                ,
                drop = FALSE
              ]
            } else {
              data.frame()
            }
            
            firsts <- if (
              nrow(year_rows)
            ) {
              sum(
                year_rows$round == "First" &
                  year_rows$control_type %in%
                  c(
                    "Own",
                    "Incoming",
                    "Swap Right"
                  ),
                na.rm = TRUE
              )
            } else {
              0
            }
            
            seconds <- if (
              nrow(year_rows)
            ) {
              sum(
                year_rows$round == "Second" &
                  year_rows$control_type %in%
                  c(
                    "Own",
                    "Incoming",
                    "Swap Right"
                  ),
                na.rm = TRUE
              )
            } else {
              0
            }
            
            obligations <- if (
              nrow(year_rows)
            ) {
              sum(
                year_rows$control_type %in%
                  c(
                    "Outgoing",
                    "Swap Obligation"
                  ),
                na.rm = TRUE
              )
            } else {
              0
            }
            
            value <- if (
              nrow(valued_rows)
            ) {
              sum(
                valued_rows$blended_value_score,
                na.rm = TRUE
              )
            } else {
              0
            }
            
            shiny::div(
              class = "draft-v2-year-card",
              
              shiny::strong(
                year
              ),
              
              shiny::span(
                paste0(
                  firsts,
                  " 1st • ",
                  seconds,
                  " 2nd"
                )
              ),
              
              shiny::span(
                paste0(
                  obligations,
                  " obligation",
                  if (
                    obligations == 1
                  ) {
                    ""
                  } else {
                    "s"
                  }
                )
              ),
              
              shiny::span(
                class = "draft-v2-year-value",
                paste0(
                  "Value ",
                  sprintf(
                    "%.1f",
                    value
                  )
                )
              )
            )
          }
        )
        
        shiny::div(
          class = "draft-v2-year-grid",
          cards
        )
      })
      
      # ------------------------------------------------------
      # Headlines
      # ------------------------------------------------------
      
      output$draft_headlines <- shiny::renderUI({
        assets <- asset_summary()
        summary <- draft_summary()
        
        headlines <- c(
          paste0(
            assets$total_assets,
            " active draft records are loaded in the planning window."
          ),
          paste0(
            assets$controlled_first_round,
            " controlled first-round asset",
            if (
              assets$controlled_first_round == 1
            ) "" else "s",
            " are represented."
          ),
          paste0(
            assets$swap_rights,
            " swap right",
            if (
              assets$swap_rights == 1
            ) "" else "s",
            " are currently controlled."
          ),
          paste0(
            summary$review_required,
            " valued record",
            if (
              summary$review_required == 1
            ) "" else "s",
            " require manual verification."
          )
        )
        
        message_rows(
          headlines,
          ""
        )
      })
      
      # ------------------------------------------------------
      # Risks
      # ------------------------------------------------------
      
      output$draft_risks <- shiny::renderUI({
        assets <- asset_summary()
        summary <- draft_summary()
        
        risks <- character()
        
        if (
          safe_num(
            assets$outgoing_obligations,
            0
          ) > 0
        ) {
          risks <- c(
            risks,
            paste0(
              assets$outgoing_obligations,
              " outgoing draft obligation",
              if (
                assets$outgoing_obligations == 1
              ) "" else "s",
              " reduce future flexibility."
            )
          )
        }
        
        if (
          safe_num(
            assets$swap_obligations,
            0
          ) > 0
        ) {
          risks <- c(
            risks,
            paste0(
              assets$swap_obligations,
              " swap obligation",
              if (
                assets$swap_obligations == 1
              ) "" else "s",
              " create future downside exposure."
            )
          )
        }
        
        if (
          safe_num(
            summary$review_required,
            0
          ) > 0
        ) {
          risks <- c(
            risks,
            paste0(
              summary$review_required,
              " valued record",
              if (
                summary$review_required == 1
              ) "" else "s",
              " require protection or verification review before transaction use."
            )
          )
        }
        
        if (
          safe_num(
            summary$net_portfolio_value,
            0
          ) < 20
        ) {
          risks <- c(
            risks,
            "Estimated draft-capital value is limited, reducing transaction downside protection."
          )
        }
        
        if (!length(risks)) {
          risks <- "No major structural draft-capital risk is identified by the loaded portfolio."
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
      
      output$draft_opportunities <- shiny::renderUI({
        assets <- asset_summary()
        summary <- draft_summary()
        
        opportunities <- character()
        
        if (
          safe_num(
            summary$premium_assets,
            0
          ) > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              summary$premium_assets,
              " premium asset",
              if (
                summary$premium_assets == 1
              ) "" else "s",
              " can anchor high-level transaction packages."
            )
          )
        }
        
        if (
          safe_num(
            assets$swap_rights,
            0
          ) > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              assets$swap_rights,
              " swap right",
              if (
                assets$swap_rights == 1
              ) "" else "s",
              " preserve upside without requiring full pick ownership."
            )
          )
        }
        
        if (
          safe_num(
            assets$controlled_first_round,
            0
          ) >= 4
        ) {
          opportunities <- c(
            opportunities,
            "First-round inventory provides meaningful flexibility for consolidation or star-level acquisition scenarios."
          )
        }
        
        if (
          safe_num(
            summary$net_portfolio_value,
            0
          ) >= 75
        ) {
          opportunities <- c(
            opportunities,
            "The modeled portfolio provides useful transaction currency while preserving future optionality."
          )
        }
        
        if (!length(opportunities)) {
          opportunities <- "Prioritize adding or preserving controllable future draft assets before aggressive consolidation."
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
      # Asset ledger
      # ------------------------------------------------------
      
      output$draft_asset_table <- reactable::renderReactable({
        assets <- draft_assets()
        valued <- valued_assets()
        
        shiny::validate(
          shiny::need(
            nrow(assets) > 0,
            paste(
              "No active draft assets are loaded for",
              selected_team(),
              "in the current planning window."
            )
          )
        )
        
        merged <- merge(
          assets,
          valued[
            ,
            c(
              "draft_asset_id",
              "expected_slot",
              "blended_value_score",
              "value_tier",
              "requires_manual_review"
            ),
            drop = FALSE
          ],
          by = "draft_asset_id",
          all.x = TRUE,
          suffixes = c(
            "",
            "_value"
          )
        )
        
        review_col <- if (
          "requires_manual_review_value" %in%
          names(merged)
        ) {
          merged$requires_manual_review_value
        } else if (
          "requires_manual_review" %in%
          names(merged)
        ) {
          merged$requires_manual_review
        } else {
          rep(
            FALSE,
            nrow(merged)
          )
        }
        
        display <- data.frame(
          Year = merged$draft_year,
          Round = merged$round,
          Control = merged$control_type,
          `Original Team` = merged$original_team,
          Protection = ifelse(
            is.na(
              merged$protection_text
            ) |
              !nzchar(
                trimws(
                  as.character(
                    merged$protection_text
                  )
                )
              ),
            "Unprotected / Not specified",
            merged$protection_text
          ),
          `Expected Slot` = ifelse(
            is.na(
              merged$expected_slot
            ),
            "—",
            merged$expected_slot
          ),
          `Value Score` = ifelse(
            is.na(
              merged$blended_value_score
            ),
            "—",
            sprintf(
              "%.1f",
              merged$blended_value_score
            )
          ),
          Tier = ifelse(
            is.na(
              merged$value_tier
            ),
            "—",
            merged$value_tier
          ),
          Verified = ifelse(
            merged$verification_status == "Verified",
            "Yes",
            "Review"
          ),
          `Manual Review` = ifelse(
            review_col,
            "Required",
            "No"
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
          defaultSorted = "Year",
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
      
      output$draft_recommendation <- shiny::renderUI({
        summary <- draft_summary()
        assets <- asset_summary()
        
        value <- safe_num(
          summary$net_portfolio_value,
          0
        )
        
        label <- if (
          value >= 150
        ) {
          "PRESERVE + DEPLOY SELECTIVELY"
        } else if (
          value >= 75
        ) {
          "MAINTAIN FLEXIBILITY"
        } else if (
          value >= 20
        ) {
          "PROTECT CORE PICKS"
        } else {
          "ACQUIRE DRAFT CAPITAL"
        }
        
        rationale <- if (
          value >= 150
        ) {
          paste(
            "The portfolio carries substantial modeled transaction value.",
            "Avoid unnecessary dilution while preserving the ability to consolidate for a high-impact acquisition."
          )
        } else if (
          value >= 75
        ) {
          paste(
            "The portfolio provides useful transaction currency.",
            "Deploy selectively while preserving enough future control to protect against roster and competitive downside."
          )
        } else if (
          value >= 20
        ) {
          paste(
            "Draft capital is functional but not deep.",
            "Protect the strongest controlled picks and use second-round or lower-value assets before premium inventory."
          )
        } else {
          paste(
            "Draft capital provides limited transaction protection.",
            "Prioritize acquiring controllable picks and reducing outgoing obligations before aggressive asset consolidation."
          )
        }
        
        if (
          safe_num(
            summary$review_required,
            0
          ) > 0
        ) {
          rationale <- paste(
            rationale,
            paste0(
              summary$review_required,
              " asset record",
              if (
                summary$review_required == 1
              ) "" else "s",
              " should be verified before inclusion in a transaction."
            )
          )
        }
        
        shiny::div(
          class = "draft-v2-rec-grid",
          
          shiny::div(
            class = "draft-v2-rec-box",
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
              style = "margin:0; color:#8998ab; line-height:1.55;",
              paste(
                "Draft value is an internal decision-support estimate.",
                "Verified pick language, scouting context, market conditions,",
                "and transaction objectives remain controlling inputs."
              )
            )
          )
        )
      })
    }
  )
}