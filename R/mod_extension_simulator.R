# ============================================================
# PHASE 2 STEP 13 — FINAL INTEGRATION / QA
# Extension Intelligence
# Stable checkpoint: no visual redesign in this pass.
# ============================================================

# ------------------------------------------------------------
# Module: Extension Intelligence
# Version 2.2 Executive Contract Workspace — BIE Extension Value
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Extension Simulator UI
#'
#' @param id Internal module ID.
#' @noRd
mod_extension_simulator_ui <- function(id) {
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
    class = "tbi-module-page tbi-v2-extension-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .tbi-extension-cba-link {
          color:#72adff !important;
          font-weight:800 !important;
          text-decoration:none !important;
        }

        .tbi-extension-cba-link::after {
          content:'  ↗';
          color:#5f9fee;
          font-size:.66em;
          opacity:.78;
        }

        .tbi-extension-cba-link:hover {
          color:#a8ceff !important;
          text-decoration:underline !important;
          text-underline-offset:2px;
        }


        .tbi-v2-extension-page {
          display: grid;
          gap: 12px;
        }

        .ext-builder-grid {
          display: grid;
          grid-template-columns: minmax(330px,.85fr) minmax(0,1.15fr);
          gap: 12px;
        }

        .ext-form-grid {
          display: grid;
          grid-template-columns: repeat(2,minmax(0,1fr));
          gap: 10px 12px;
        }

        .ext-form-grid .form-group {
          margin-bottom: 0 !important;
        }

        .ext-form-grid label,
        .ext-player-select label {
          color: #718198 !important;
          font-size: .55rem !important;
          font-weight: 850 !important;
          letter-spacing: .08em !important;
          text-transform: uppercase;
        }

        .ext-player-select {
          margin-bottom: 12px;
        }

        .ext-builder-panel {
          padding: 16px;
        }

        .ext-builder-title {
          margin-bottom: 12px;
          color: #f3f6fa;
          font-size: 1rem;
          font-weight: 780;
        }

        .ext-player-card {
          margin-bottom: 12px;
          padding: 13px;
          display: grid;
          grid-template-columns: 52px minmax(0,1fr);
          gap: 12px;
          align-items: center;
          border: 1px solid rgba(96,165,250,.17);
          border-radius: 10px;
          background: rgba(59,130,246,.035);
        }

        .ext-player-avatar {
          width: 52px;
          height: 52px;
          display: grid;
          place-items: center;
          border: 1px solid rgba(96,165,250,.28);
          border-radius: 50%;
          background: #101b2a;
          color: #69a9ff;
          font-weight: 900;
        }

        .ext-player-card h3 {
          margin: 0 !important;
          color: #f7f9fc !important;
          font-size: 1.05rem !important;
        }

        .ext-player-card p {
          margin: 3px 0 0;
          color: #8392a6;
          font-size: .65rem;
        }

        .ext-verify-box {
          margin-top: 12px;
          padding: 12px;
          border: 1px solid rgba(148,163,184,.10);
          border-radius: 9px;
          background: rgba(255,255,255,.012);
        }

        .ext-verify-title {
          margin-bottom: 8px;
          color: #77869a;
          font-size: .55rem;
          font-weight: 850;
          letter-spacing: .08em;
          text-transform: uppercase;
        }

        .ext-verify-box .form-check {
          margin-bottom: 6px;
        }

        .ext-verify-box label {
          color: #a9b6c7 !important;
          font-size: .63rem !important;
        }

        .ext-input-note {
          margin-top: 10px;
          color: #718198;
          font-size: .58rem;
          line-height: 1.5;
        }

        .ext-summary-strip {
          display: grid;
          grid-template-columns: repeat(4,minmax(0,1fr));
          gap: 8px;
          margin-top: 12px;
        }

        .ext-summary-card {
          min-height: 68px;
          padding: 10px;
          border: 1px solid rgba(148,163,184,.10);
          border-radius: 8px;
          background: rgba(255,255,255,.012);
        }

        .ext-summary-card span {
          display: block;
          color: #718198;
          font-size: .51rem;
          font-weight: 850;
          letter-spacing: .08em;
          text-transform: uppercase;
        }

        .ext-summary-card strong {
          display: block;
          margin-top: 5px;
          color: #edf3f9;
          font-size: .84rem;
        }

        .ext-schedule-wrap {
          max-height: 305px;
          overflow: auto;
        }

        .ext-status-row {
          min-height: 39px;
          padding: 7px 0;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 14px;
          border-bottom: 1px solid rgba(148,163,184,.085);
          color: #8493a8;
          font-size: .63rem;
        }

        .ext-status-row:last-child {
          border-bottom: 0;
        }

        .ext-status-row strong {
          color: #eef3f8;
          text-align: right;
        }

        .ext-status-pass { color:#34d399 !important; }
        .ext-status-review { color:#f59e0b !important; }
        .ext-status-fail { color:#fb7185 !important; }

        .ext-message {
          display: flex;
          gap: 8px;
          margin: 7px 0;
          color: #c6d0dd;
          font-size: .62rem;
          line-height: 1.45;
        }

        .ext-message-dot {
          width: 6px;
          height: 6px;
          margin-top: 5px;
          flex: 0 0 6px;
          border-radius: 50%;
          background: #60a5fa;
        }

        .ext-message-dot.warning { background:#f59e0b; }
        .ext-message-dot.danger { background:#fb7185; }
        .ext-message-dot.success { background:#34d399; }

        @media (max-width: 1050px) {
          .ext-builder-grid {
            grid-template-columns: 1fr;
          }
        }

        @media (max-width: 720px) {
          .ext-form-grid,
          .ext-summary-strip {
            grid-template-columns: 1fr;
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
          "CONTRACT STRATEGY"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Extension Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Model extension structures, screen CBA eligibility,",
            "test starting salary and raise limits, and evaluate long-term commitment."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-v2-model-chip",
        shiny::span(
          class = "tbi-v2-model-label",
          "CBA ENGINE"
        ),
        shiny::strong("Phase 4")
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
            bsicons::bs_icon("calculator")
          ),
          shiny::span("EXTENSION SNAPSHOT")
        ),
        
        shiny::span(
          class = "tbi-v2-section-status",
          shiny::span(class = "tbi-v2-live-dot"),
          "LIVE MODEL"
        )
      ),
      
      shiny::div(
        class = "tbi-v2-snapshot-grid",
        
        snapshot_item(
          "PLAYER",
          "snapshot_player",
          "person-badge",
          "blue"
        ),
        
        snapshot_item(
          "CURRENT SALARY",
          "snapshot_current_salary",
          "cash-stack",
          "blue"
        ),
        
        snapshot_item(
          "EXTENSION TYPE",
          "snapshot_extension_type",
          "calculator",
          "purple"
        ),
        
        snapshot_item(
          "STARTING SALARY",
          "snapshot_starting_salary",
          "currency-dollar",
          "blue"
        ),
        
        snapshot_item(
          "TOTAL VALUE",
          "snapshot_total_value",
          "cash-stack",
          "green"
        ),
        
        snapshot_item(
          "CBA RESULT",
          "snapshot_result",
          "exclamation-triangle",
          "orange"
        )
      )
    ),
    
    # --------------------------------------------------------
    # Builder
    # --------------------------------------------------------
    
    shiny::div(
      class = "ext-builder-grid",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "PLAYER & ELIGIBILITY"
            ),
            shiny::h3(
              "Verified contract inputs"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "INPUTS"
          )
        ),
        
        shiny::div(
          class = "ext-builder-panel",
          
          shiny::div(
            class = "ext-player-select",
            shiny::selectInput(
              ns("player_id"),
              "Player",
              choices = NULL,
              width = "100%"
            )
          ),
          
          shiny::uiOutput(
            ns("player_card")
          ),
          
          shiny::div(
            class = "ext-form-grid",
            
            shiny::numericInput(
              ns("service_years"),
              "NBA Service Years",
              value = 4,
              min = 0,
              max = 25,
              step = 1
            ),
            
            shiny::numericInput(
              ns("remaining_years"),
              "Contract Years Remaining",
              value = 1,
              min = 0,
              max = 6,
              step = 1
            )
          ),
          
          shiny::div(
            class = "ext-verify-box",
            
            shiny::div(
              class = "ext-verify-title",
              "ELIGIBILITY FACTS TO VERIFY"
            ),
            
            shiny::checkboxInput(
              ns("is_first_round_pick"),
              "First-round draft pick",
              value = FALSE
            ),
            
            shiny::checkboxInput(
              ns("rookie_options_exercised"),
              "Required rookie-scale option years exercised",
              value = FALSE
            ),
            
            shiny::checkboxInput(
              ns("timing_window_open"),
              "Applicable extension signing window confirmed open",
              value = FALSE
            ),
            
            shiny::checkboxInput(
              ns("designated_rookie_qualified"),
              "Designated-rookie qualification confirmed",
              value = FALSE
            ),
            
            shiny::checkboxInput(
              ns("designated_veteran_qualified"),
              "Designated-veteran award qualification confirmed",
              value = FALSE
            ),
            
            shiny::checkboxInput(
              ns("original_team_requirement_met"),
              "Designated-veteran original-team requirement confirmed",
              value = FALSE
            )
          ),
          
          shiny::div(
            class = "ext-input-note",
            paste(
              "These facts are intentionally manual where the current database",
              "does not contain enough information for a definitive CBA ruling."
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
              "PROPOSAL BUILDER"
            ),
            shiny::h3(
              "Extension structure"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "SCENARIO"
          )
        ),
        
        shiny::div(
          class = "ext-builder-panel",
          
          shiny::div(
            class = "ext-form-grid",
            
            shiny::selectInput(
              ns("extension_type"),
              "Extension Type",
              choices = c(
                "Veteran Extension" = "veteran",
                "Rookie-Scale Extension" = "rookie_scale",
                "Designated Rookie" = "designated_rookie",
                "Designated Veteran" = "designated_veteran"
              ),
              selected = "veteran"
            ),
            
            shiny::selectInput(
              ns("guarantee_structure"),
              "Guarantee Structure",
              choices = c(
                "Fully guaranteed",
                "Final year team option",
                "Final year player option",
                "Partial guarantee"
              ),
              selected = "Fully guaranteed"
            ),
            
            shiny::numericInput(
              ns("starting_salary_m"),
              "Starting Salary ($M)",
              value = 20,
              min = 0,
              max = 100,
              step = 0.1
            ),
            
            shiny::numericInput(
              ns("years"),
              "Extension Years",
              value = 4,
              min = 1,
              max = 5,
              step = 1
            ),
            
            shiny::numericInput(
              ns("raise_pct"),
              "Annual Raise (%)",
              value = 8,
              min = 0,
              max = 20,
              step = 0.25
            ),
            
            shiny::textInput(
              ns("first_season"),
              "First Extension Season",
              value = "2027-28"
            )
          ),
          
          shiny::div(
            class = "ext-summary-strip",
            
            shiny::div(
              class = "ext-summary-card",
              shiny::span("MAX START"),
              shiny::strong(
                shiny::textOutput(
                  ns("builder_max_start"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              class = "ext-summary-card",
              shiny::span("MAX RAISE"),
              shiny::strong(
                shiny::textOutput(
                  ns("builder_max_raise"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              class = "ext-summary-card",
              shiny::span("MAX YEARS"),
              shiny::strong(
                shiny::textOutput(
                  ns("builder_max_years"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              class = "ext-summary-card",
              shiny::span("ROOM BELOW MAX"),
              shiny::strong(
                shiny::textOutput(
                  ns("builder_room_below_max"),
                  inline = TRUE
                )
              )
            )
          ),
          
          shiny::div(
            class = "ext-input-note",
            paste(
              "The proposal updates automatically.",
              "No offer is stored or submitted from this screen."
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
            "EXTENSION DECISION"
          )
        ),
        
        shiny::uiOutput(
          ns("extension_decision")
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
              "CBA EXTENSION SCORECARD"
            )
          ),
          
          shiny::div(
            class = "tbi-v2-composite-score",
            shiny::span("Screen"),
            shiny::strong(
              shiny::textOutput(
                ns("scorecard_result"),
                inline = TRUE
              )
            )
          )
        ),
        
        shiny::uiOutput(
          ns("extension_scorecard")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Schedule + executive readout
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
              "FINANCIAL IMPACT"
            ),
            shiny::h3(
              "Proposed extension schedule"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "MODELED"
          )
        ),
        
        shiny::div(
          class = "ext-schedule-wrap",
          reactable::reactableOutput(
            ns("extension_schedule")
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
              "CONTRACT ENGINE"
            ),
            shiny::h3(
              "Front-office readout"
            )
          ),
          
          shiny::span(
            class = "tbi-v2-context-tag",
            "DECISION SUPPORT"
          )
        ),
        
        shiny::div(
          style = "padding:14px 16px;",
          shiny::uiOutput(
            ns("contract_readout")
          )
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
              "calculator"
            )
          ),
          shiny::span(
            "CBA REVIEW ITEMS"
          )
        ),
        
        shiny::uiOutput(
          ns("extension_alerts")
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
            "CONTRACT RISKS"
          )
        ),
        
        shiny::uiOutput(
          ns("extension_risks")
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
            "NEGOTIATION OPPORTUNITIES"
          )
        ),
        
        shiny::uiOutput(
          ns("extension_opportunities")
        )
      )
    ),
    
    # --------------------------------------------------------
    # BIE Contract Value Intelligence
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
            "Extension value + timeline"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "BIE CONTRACT VALUE"
        )
      ),
      
      shiny::div(
        style = "padding:16px;",
        shiny::uiOutput(
          ns("bie_extension_value")
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
          shiny::h3(
            "Recommended contract action"
          )
        ),
        
        shiny::span(
          class = "tbi-v2-context-tag",
          "CBA + FINANCIAL + BIE"
        )
      ),
      
      shiny::div(
        style = "padding:16px;",
        shiny::uiOutput(
          ns("executive_recommendation")
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Extension Simulator server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @noRd
mod_extension_simulator_server <- function(
    id,
    selected_team,
    selected_season) {
  
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
      
      message_rows <- function(
    messages,
    tone = "warning") {
        
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
          return(
            shiny::div(
              class = "ext-message",
              shiny::span(
                class = "ext-message-dot success"
              ),
              shiny::span(
                "No additional item is currently identified."
              )
            )
          )
        }
        
        shiny::tagList(
          lapply(
            messages,
            function(message) {
              
              shiny::div(
                class = "ext-message",
                
                shiny::span(
                  class = paste(
                    "ext-message-dot",
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
      
      score_row <- function(
    label,
    subtitle,
    icon,
    state,
    detail = NULL,
    cba_term = NULL) {
        
        state <- toupper(
          as.character(
            state
          )
        )
        
        if (state == "PASS") {
          tone <- "green"
          width <- 100
          rating <- "Pass"
          status <- "Cleared"
        } else if (
          state == "REVIEW"
        ) {
          tone <- "orange"
          width <- 55
          rating <- "Review"
          status <- "Verify"
        } else {
          tone <- "red"
          width <- 20
          rating <- "Fail"
          status <- "Blocked"
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
                  exists("tbi_cba_link", mode = "function")
                ) {
                  tbi_cba_link(
                    term = cba_term,
                    label = label,
                    class = "tbi-extension-cba-link"
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
                  width,
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
            status
          )
        )
      }
      
      status_row <- function(
    label,
    value,
    tone = NULL,
    cba_term = NULL) {
        
        class_name <- if (
          is.null(tone)
        ) {
          ""
        } else {
          paste0(
            "ext-status-",
            tone
          )
        }
        
        shiny::div(
          class = "ext-status-row",
          shiny::span(label),
          shiny::strong(
            class = class_name,
            if (
              !is.null(cba_term) &&
              nzchar(as.character(cba_term)) &&
              exists("tbi_cba_link", mode = "function")
            ) {
              tbi_cba_link(
                term = cba_term,
                label = value,
                class = "tbi-extension-cba-link"
              )
            } else {
              shiny::span(value)
            }
          )
        )
      }
      
      extension_cba_term <- function(x) {
        switch(
          as.character(x %||% ""),
          "rookie_scale" = "Rookie-Scale Extension",
          "designated_rookie" = "Designated Rookie Extension",
          "veteran" = "Veteran Extension",
          "designated_veteran" = "Designated Veteran Extension",
          "Veteran Extension"
        )
      }
      
      extension_type_label <- function(x) {
        switch(
          x,
          "rookie_scale" = "Rookie Scale",
          "designated_rookie" = "Designated Rookie",
          "veteran" = "Veteran",
          "designated_veteran" = "Designated Veteran",
          "Review"
        )
      }
      
      next_season_label <- function(season) {
        year <- suppressWarnings(
          as.integer(
            substr(
              season,
              1,
              4
            )
          )
        )
        
        if (is.na(year)) {
          return("")
        }
        
        paste0(
          year + 1L,
          "-",
          substr(
            as.character(
              year + 2L
            ),
            3,
            4
          )
        )
      }
      
      # ------------------------------------------------------
      # Player pool
      # ------------------------------------------------------
      
      player_pool <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        get_extension_player_pool(
          selected_team(),
          selected_season()
        )
      })
      
      shiny::observe({
        d <- player_pool()
        
        if (!nrow(d)) {
          shiny::updateSelectInput(
            session,
            "player_id",
            choices = character(),
            selected = character()
          )
          
          return()
        }
        
        choices <- stats::setNames(
          as.character(
            d$player_id
          ),
          paste0(
            d$player_name,
            " — ",
            ifelse(
              is.na(
                d$primary_position
              ),
              "—",
              d$primary_position
            ),
            " — ",
            vapply(
              d$current_salary,
              money,
              character(1)
            )
          )
        )
        
        current <- isolate(
          input$player_id
        )
        
        selected_value <- if (
          !is.null(current) &&
          current %in% choices
        ) {
          current
        } else {
          choices[[1]]
        }
        
        shiny::updateSelectInput(
          session,
          "player_id",
          choices = choices,
          selected = selected_value
        )
      })
      
      selected_player_row <- shiny::reactive({
        d <- player_pool()
        
        shiny::req(
          nrow(d),
          input$player_id
        )
        
        player_id <- suppressWarnings(
          as.integer(
            input$player_id
          )
        )
        
        row <- d[
          d$player_id == player_id,
          ,
          drop = FALSE
        ]
        
        shiny::req(
          nrow(row)
        )
        
        row[
          1,
          ,
          drop = FALSE
        ]
      })
      
      # Default starting salary follows selected player until
      # the user changes the value manually.
      shiny::observeEvent(
        selected_player_row(),
        {
          row <- selected_player_row()
          
          salary_m <-
            safe_num(
              row$current_salary,
              0
            ) /
            1e6
          
          if (salary_m <= 0) {
            salary_m <- 1
          }
          
          shiny::updateNumericInput(
            session,
            "starting_salary_m",
            value = round(
              salary_m * 1.15,
              1
            )
          )
          
          next_season <- next_season_label(
            selected_season()
          )
          
          if (nzchar(next_season)) {
            shiny::updateTextInput(
              session,
              "first_season",
              value = next_season
            )
          }
        },
        ignoreInit = FALSE
      )
      
      # ------------------------------------------------------
      # Threshold
      # ------------------------------------------------------
      
      cap_threshold <- shiny::reactive({
        shiny::req(
          selected_season()
        )
        
        th <- get_cap_thresholds(
          selected_season()
        )
        
        if (
          is.null(th) ||
          !nrow(th)
        ) {
          return(NA_real_)
        }
        
        safe_num(
          th$salary_cap[[1]],
          NA_real_
        )
      })
      
      # ------------------------------------------------------
      # Player engine object
      # ------------------------------------------------------
      
      extension_player <- shiny::reactive({
        row <- selected_player_row()
        
        extension_player_from_row(
          player_row = row,
          service_years =
            as.integer(
              safe_num(
                input$service_years,
                0
              )
            ),
          remaining_contract_years =
            as.integer(
              safe_num(
                input$remaining_years,
                0
              )
            ),
          is_first_round_pick =
            isTRUE(
              input$is_first_round_pick
            ),
          rookie_option_years_exercised =
            isTRUE(
              input$rookie_options_exercised
            ),
          timing_window_open =
            isTRUE(
              input$timing_window_open
            ),
          designated_rookie_qualified =
            isTRUE(
              input$designated_rookie_qualified
            ),
          designated_veteran_qualified =
            isTRUE(
              input$designated_veteran_qualified
            ),
          original_team_requirement_met =
            isTRUE(
              input$original_team_requirement_met
            )
        )
      })
      
      # ------------------------------------------------------
      # Proposal + engine result
      # ------------------------------------------------------
      
      proposal <- shiny::reactive({
        shiny::req(
          input$extension_type,
          input$starting_salary_m,
          input$years,
          input$raise_pct
        )
        
        list(
          extension_type =
            input$extension_type,
          salary_cap =
            cap_threshold(),
          starting_salary =
            safe_num(
              input$starting_salary_m,
              0
            ) *
            1e6,
          years =
            as.integer(
              safe_num(
                input$years,
                1
              )
            ),
          raise_percent =
            safe_num(
              input$raise_pct,
              0
            ) /
            100,
          guarantee_structure =
            input$guarantee_structure,
          first_season =
            input$first_season
        )
      })
      
      extension_result <- shiny::reactive({
        if (
          is.na(
            cap_threshold()
          ) ||
          cap_threshold() <= 0
        ) {
          return(
            structure(
              list(
                message = "Verified salary-cap threshold data is unavailable for this season."
              ),
              class = "tbi_extension_error"
            )
          )
        }
        
        tryCatch(
          evaluate_extension_proposal(
            player = extension_player(),
            proposal = proposal()
          ),
          error = function(e) {
            structure(
              list(
                message = conditionMessage(e)
              ),
              class = "tbi_extension_error"
            )
          }
        )
      })
      
      # ------------------------------------------------------
      # BIE Extension Value Intelligence
      # ------------------------------------------------------
      
      bie_extension_roster <- shiny::reactive({
        
        d <- player_pool()
        
        if (
          !nrow(d) ||
          !exists(
            "evaluate_bie_players",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_bie_players(d),
          error = function(e) NULL
        )
      })
      
      
      bie_extension_value_result <- shiny::reactive({
        
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          ) ||
          !exists(
            "evaluate_bie_extension_value",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_bie_extension_value(
            player_row =
              selected_player_row(),
            proposal =
              proposal(),
            extension_result =
              result,
            evaluated_roster =
              bie_extension_roster()
          ),
          error = function(e) {
            list(
              status = "ERROR",
              recommendation = "REVIEW",
              confidence = "FOUNDATION",
              explanation =
                conditionMessage(e)
            )
          }
        )
      })
      
      
      output$bie_extension_value <- shiny::renderUI({
        
        value <- bie_extension_value_result()
        
        if (is.null(value)) {
          return(
            shiny::div(
              style =
                "color:#8c9bae; font-size:.72rem;",
              "BIE contract-value analysis will appear when the proposal is evaluable."
            )
          )
        }
        
        if (
          !identical(
            value$status,
            "OK"
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
                  "BIE REVIEW"
                ),
                shiny::p(
                  value$explanation
                )
              )
            )
          )
        }
        
        metric_card <- function(
    label,
    value_text) {
          
          shiny::div(
            style = paste(
              "min-width:0; padding:10px 11px;",
              "border:1px solid rgba(148,163,184,.10);",
              "border-radius:9px;",
              "background:rgba(255,255,255,.015);"
            ),
            
            shiny::span(
              class = "tbi-v2-snapshot-label",
              label
            ),
            
            shiny::strong(
              style = paste(
                "display:block; margin-top:5px;",
                "color:#eef4fb; font-size:.80rem;",
                "line-height:1.3;"
              ),
              value_text
            )
          )
        }
        
        
        player_value_text <- if (
          isTRUE(
            value$performance_available
          )
        ) {
          paste0(
            sprintf(
              "%.1f",
              value$player_score
            ),
            " • ",
            value$player_grade
          )
        } else {
          "UNRATED"
        }
        
        
        cap_share_text <- if (
          is.null(
            value$cap_share
          ) ||
          !is.finite(
            safe_num(
              value$cap_share,
              NA_real_
            )
          )
        ) {
          "—"
        } else {
          sprintf(
            "%.1f%% of cap",
            value$cap_share
          )
        }
        
        
        score_text <- if (
          is.null(
            value$decision_score
          ) ||
          !is.finite(
            safe_num(
              value$decision_score,
              NA_real_
            )
          )
        ) {
          "—"
        } else {
          sprintf(
            "%.1f / 100",
            value$decision_score
          )
        }
        
        
        tone <- if (
          grepl(
            "ADVANCE",
            value$recommendation,
            fixed = TRUE
          )
        ) {
          "#34d399"
        } else if (
          grepl(
            "HOLD|REVISE",
            value$recommendation
          )
        ) {
          "#fbbf24"
        } else {
          "#60a5fa"
        }
        
        
        shiny::tagList(
          
          shiny::div(
            style = paste(
              "display:grid;",
              "grid-template-columns:repeat(6,minmax(0,1fr));",
              "gap:8px;"
            ),
            
            metric_card(
              "BIE PLAYER VALUE",
              player_value_text
            ),
            
            metric_card(
              "AGE CURVE",
              value$age_curve
            ),
            
            metric_card(
              "TIMELINE",
              value$timeline_signal
            ),
            
            metric_card(
              "PROPOSED COST",
              cap_share_text
            ),
            
            metric_card(
              "COST BAND",
              value$cost_band
            ),
            
            metric_card(
              "VALUE ALIGNMENT",
              value$value_alignment
            )
          ),
          
          shiny::div(
            style = paste(
              "display:grid;",
              "grid-template-columns:minmax(220px,.45fr) minmax(0,1.55fr);",
              "gap:14px; align-items:center;",
              "margin-top:10px;"
            ),
            
            shiny::div(
              style = paste(
                "padding:13px 14px;",
                "border:1px solid rgba(96,165,250,.14);",
                "border-radius:9px;",
                "background:rgba(59,130,246,.035);"
              ),
              
              shiny::span(
                class =
                  "tbi-v2-snapshot-label",
                "BIE DECISION"
              ),
              
              shiny::strong(
                style = paste0(
                  "display:block; margin-top:5px;",
                  "color:",
                  tone,
                  "; font-size:.90rem;"
                ),
                value$recommendation
              ),
              
              shiny::div(
                style = paste(
                  "margin-top:5px;",
                  "color:#8da1ba;",
                  "font-size:.60rem;"
                ),
                paste0(
                  "Score ",
                  score_text,
                  " • Confidence ",
                  value$confidence,
                  " • CBA ",
                  value$cba_gate
                )
              )
            ),
            
            shiny::p(
              style = paste(
                "margin:0;",
                "color:#8c9bae;",
                "font-size:.68rem;",
                "line-height:1.55;"
              ),
              value$explanation
            )
          )
        )
      })
      
      
      # ------------------------------------------------------
      # Player card
      # ------------------------------------------------------
      
      output$player_card <- shiny::renderUI({
        p <- selected_player_row()
        
        name <- text_or(
          p$player_name,
          "Player"
        )
        
        parts <- strsplit(
          trimws(name),
          "\\s+"
        )[[1]]
        
        initials <- paste0(
          substr(
            utils::head(
              parts,
              1
            ),
            1,
            1
          ),
          substr(
            utils::tail(
              parts,
              1
            ),
            1,
            1
          )
        )
        
        shiny::div(
          class = "ext-player-card",
          
          shiny::div(
            class = "ext-player-avatar",
            initials
          ),
          
          shiny::div(
            shiny::h3(
              name
            ),
            
            shiny::p(
              paste(
                text_or(
                  p$primary_position
                ),
                paste0(
                  "Age ",
                  text_or(
                    p$player_age
                  )
                ),
                text_or(
                  p$contract_type,
                  "Contract not classified"
                ),
                sep = " • "
              )
            ),
            
            shiny::p(
              paste0(
                "Current salary ",
                money(
                  p$current_salary
                ),
                " • Contract through ",
                text_or(
                  p$contract_end_season
                )
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Snapshot
      # ------------------------------------------------------
      
      output$snapshot_player <- shiny::renderText({
        text_or(
          selected_player_row()$player_name
        )
      })
      
      output$snapshot_current_salary <- shiny::renderText({
        money(
          selected_player_row()$current_salary
        )
      })
      
      output$snapshot_extension_type <- shiny::renderText({
        extension_type_label(
          input$extension_type
        )
      })
      
      output$snapshot_starting_salary <- shiny::renderText({
        money(
          safe_num(
            input$starting_salary_m,
            0
          ) *
            1e6
        )
      })
      
      output$snapshot_total_value <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("—")
        }
        
        money(
          result$schedule_summary$total_value
        )
      })
      
      output$snapshot_result <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("REVIEW")
        }
        
        if (
          identical(
            result$status,
            "PASS_WITH_REVIEW"
          )
        ) {
          "REVIEW"
        } else {
          result$status
        }
      })
      
      output$scorecard_result <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("REVIEW")
        }
        
        if (
          isTRUE(
            result$passes_screen
          )
        ) {
          "PASS"
        } else {
          "FAIL"
        }
      })
      
      # ------------------------------------------------------
      # Builder limits
      # ------------------------------------------------------
      
      output$builder_max_start <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("—")
        }
        
        money(
          result$starting_salary_screen$maximum_starting_salary
        )
      })
      
      output$builder_max_raise <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("—")
        }
        
        sprintf(
          "%.1f%%",
          100 *
            safe_num(
              result$raise_screen$maximum_raise_percent,
              0
            )
        )
      })
      
      output$builder_max_years <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("—")
        }
        
        paste0(
          result$maximum_years,
          " yrs"
        )
      })
      
      output$builder_room_below_max <- shiny::renderText({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return("—")
        }
        
        room <- safe_num(
          result$starting_salary_screen$room_below_limit,
          0
        )
        
        if (room >= 0) {
          money(room)
        } else {
          paste0(
            money(
              abs(room)
            ),
            " over"
          )
        }
      })
      
      # ------------------------------------------------------
      # Decision
      # ------------------------------------------------------
      
      output$extension_decision <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
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
        
        decision <- if (
          !isTRUE(
            result$passes_screen
          )
        ) {
          "DO NOT ADVANCE"
        } else if (
          isTRUE(
            result$requires_manual_review
          )
        ) {
          "ADVANCE TO REVIEW"
        } else {
          "ADVANCE"
        }
        
        shiny::tagList(
          shiny::div(
            class = "tbi-v2-decision-main",
            
            shiny::div(
              class = "tbi-v2-decision-symbol",
              bsicons::bs_icon(
                if (
                  isTRUE(
                    result$passes_screen
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
                result$executive_summary
              )
            )
          ),
          
          shiny::div(
            class = "tbi-v2-decision-metrics",
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "TOTAL VALUE"
              ),
              shiny::strong(
                money(
                  result$schedule_summary$total_value
                )
              ),
              shiny::tags$small(
                paste0(
                  result$schedule_summary$years,
                  " years"
                )
              )
            ),
            
            shiny::div(
              class = "tbi-v2-decision-metric",
              shiny::span(
                "ELIGIBILITY"
              ),
              shiny::strong(
                result$eligibility$status
              ),
              shiny::tags$small(
                extension_type_label(
                  result$extension_type
                )
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Scorecard
      # ------------------------------------------------------
      
      output$extension_scorecard <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return(
            shiny::div(
              style = "padding:18px; color:#8390a2;",
              result$message
            )
          )
        }
        
        eligibility_state <- if (
          isTRUE(
            result$eligibility$eligible
          )
        ) {
          if (
            isTRUE(
              result$eligibility$requires_manual_review
            )
          ) {
            "REVIEW"
          } else {
            "PASS"
          }
        } else {
          "FAIL"
        }
        
        start_state <- if (
          isTRUE(
            result$starting_salary_screen$requested_within_limit
          )
        ) {
          "PASS"
        } else {
          "FAIL"
        }
        
        raise_state <- if (
          isTRUE(
            result$raise_screen$requested_within_limit
          )
        ) {
          "PASS"
        } else {
          "FAIL"
        }
        
        years_state <- if (
          isTRUE(
            result$years_within_limit
          )
        ) {
          "PASS"
        } else {
          "FAIL"
        }
        
        shiny::tagList(
          score_row(
            "Eligibility",
            "Extension-type eligibility screen",
            "person-badge",
            eligibility_state,
            result$eligibility$status,
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          score_row(
            "Starting Salary",
            "Requested first-year salary",
            "currency-dollar",
            start_state,
            paste0(
              money(
                proposal()$starting_salary
              ),
              " requested / ",
              money(
                result$starting_salary_screen$maximum_starting_salary
              ),
              " max"
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          score_row(
            "Annual Raise",
            "Year-over-year raise limit",
            "graph-up-arrow",
            raise_state,
            paste0(
              sprintf(
                "%.1f%%",
                proposal()$raise_percent *
                  100
              ),
              " requested"
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          score_row(
            "Contract Term",
            "Extension length",
            "calendar3",
            years_state,
            paste0(
              result$requested_years,
              " requested / ",
              result$maximum_years,
              " max"
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          score_row(
            "Manual Review",
            "Dates, awards, options, guarantees and language",
            "exclamation-triangle",
            if (
              isTRUE(
                result$requires_manual_review
              )
            ) {
              "REVIEW"
            } else {
              "PASS"
            },
            if (
              isTRUE(
                result$requires_manual_review
              )
            ) {
              "Required"
            } else {
              "No modeled flag"
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Schedule
      # ------------------------------------------------------
      
      output$extension_schedule <- reactable::renderReactable({
        result <- extension_result()
        
        shiny::validate(
          shiny::need(
            !inherits(
              result,
              "tbi_extension_error"
            ),
            if (
              inherits(
                result,
                "tbi_extension_error"
              )
            ) {
              result$message
            } else {
              "Extension schedule unavailable."
            }
          )
        )
        
        schedule <- result$schedule
        
        display <- data.frame(
          Season = schedule$season,
          Year = schedule$extension_year,
          Salary = vapply(
            schedule$salary,
            money,
            character(1)
          ),
          Raise = vapply(
            schedule$raise_percent,
            function(x) {
              if (
                is.na(
                  suppressWarnings(
                    as.numeric(x)
                  )
                )
              ) {
                return("—")
              }
              
              sprintf(
                "%.1f%%",
                safe_num(
                  x,
                  0
                ) *
                  100
              )
            },
            character(1)
          ),
          Guarantee = schedule$guarantee,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        reactable::reactable(
          display,
          pagination = FALSE,
          striped = FALSE,
          highlight = TRUE,
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
      # Readout
      # ------------------------------------------------------
      
      output$contract_readout <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return(
            message_rows(
              result$message,
              "danger"
            )
          )
        }
        
        start_tone <- if (
          isTRUE(
            result$starting_salary_screen$requested_within_limit
          )
        ) {
          "pass"
        } else {
          "fail"
        }
        
        raise_tone <- if (
          isTRUE(
            result$raise_screen$requested_within_limit
          )
        ) {
          "pass"
        } else {
          "fail"
        }
        
        shiny::tagList(
          status_row(
            "Extension type",
            extension_type_label(
              result$extension_type
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "Eligibility",
            result$eligibility$status,
            if (
              isTRUE(
                result$eligibility$eligible
              )
            ) {
              "pass"
            } else {
              "fail"
            },
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "Maximum starting salary",
            money(
              result$starting_salary_screen$maximum_starting_salary
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "Requested starting salary",
            money(
              proposal()$starting_salary
            ),
            start_tone
          ),
          
          status_row(
            "Maximum annual raise",
            sprintf(
              "%.1f%%",
              safe_num(
                result$raise_screen$maximum_raise_percent,
                0
              ) *
                100
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "Requested annual raise",
            sprintf(
              "%.1f%%",
              proposal()$raise_percent *
                100
            ),
            raise_tone
          ),
          
          status_row(
            "Maximum term",
            paste0(
              result$maximum_years,
              " years"
            ),
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "Modeled total value",
            money(
              result$schedule_summary$total_value
            )
          ),
          
          status_row(
            "Average annual value",
            money(
              result$schedule_summary$average_annual_value
            )
          ),
          
          status_row(
            "Rule set",
            result$rule_set,
            cba_term = extension_cba_term(result$extension_type)
          ),
          
          status_row(
            "CBA reference",
            "Open extension rule",
            cba_term = extension_cba_term(result$extension_type)
          )
        )
      })
      
      # ------------------------------------------------------
      # Alerts
      # ------------------------------------------------------
      
      output$extension_alerts <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return(
            message_rows(
              result$message,
              "danger"
            )
          )
        }
        
        message_rows(
          result$warnings,
          "warning"
        )
      })
      
      output$extension_risks <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
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
                  "Risk"
                ),
                shiny::p(
                  result$message
                )
              )
            )
          )
        }
        
        risks <- result$failures
        
        if (
          !length(risks)
        ) {
          room <- safe_num(
            result$starting_salary_screen$room_below_limit,
            0
          )
          
          max_start <- safe_num(
            result$starting_salary_screen$maximum_starting_salary,
            0
          )
          
          if (
            max_start > 0 &&
            room <
            0.02 *
            max_start
          ) {
            risks <- paste(
              "The proposal is near the screened maximum starting salary,",
              "increasing long-term cap concentration."
            )
          } else {
            risks <- "No modeled financial-limit failure is currently identified."
          }
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
      
      output$extension_opportunities <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          return(
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
                  "Resolve the required input before comparing contract structures."
                )
              )
            )
          )
        }
        
        opportunities <- character()
        
        room <- safe_num(
          result$starting_salary_screen$room_below_limit,
          0
        )
        
        if (
          room > 0
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              money(room),
              " remains below the screened maximum starting salary."
            )
          )
        }
        
        if (
          result$requested_years <
          result$maximum_years
        ) {
          opportunities <- c(
            opportunities,
            paste0(
              result$maximum_years -
                result$requested_years,
              " additional extension year",
              if (
                result$maximum_years -
                result$requested_years == 1
              ) {
                ""
              } else {
                "s"
              },
              " may remain available under the screened term limit."
            )
          )
        }
        
        if (
          proposal()$raise_percent <
          safe_num(
            result$raise_screen$maximum_raise_percent,
            0
          )
        ) {
          opportunities <- c(
            opportunities,
            "The requested raise is below the screened maximum, leaving negotiation room."
          )
        }
        
        if (
          !length(opportunities)
        ) {
          opportunities <- paste(
            "Compare the current structure against waiting,",
            "free-agency risk, and alternative guarantee structures."
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
      # Executive recommendation
      # ------------------------------------------------------
      
      output$executive_recommendation <- shiny::renderUI({
        result <- extension_result()
        
        if (
          inherits(
            result,
            "tbi_extension_error"
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
                  "REVIEW INPUTS"
                ),
                shiny::p(
                  result$message
                )
              )
            )
          )
        }
        
        label <- if (
          !isTRUE(
            result$passes_screen
          )
        ) {
          "REVISE PROPOSAL"
        } else if (
          safe_num(
            result$starting_salary_screen$room_below_limit,
            0
          ) <
          0.02 *
          safe_num(
            result$starting_salary_screen$maximum_starting_salary,
            0
          )
        ) {
          "ADVANCE WITH CAUTION"
        } else {
          "ADVANCE TO CONTRACT REVIEW"
        }
        
        tone <- if (
          !isTRUE(
            result$passes_screen
          )
        ) {
          "#fb7185"
        } else {
          "#34d399"
        }
        
        shiny::div(
          style = paste(
            "display:grid;",
            "grid-template-columns:minmax(220px,.45fr) minmax(0,1.55fr);",
            "gap:18px; align-items:center;"
          ),
          
          shiny::div(
            style = paste0(
              "min-height:105px; padding:16px;",
              "display:flex; align-items:center;",
              "border:1px solid ",
              tone,
              "55; border-radius:11px;",
              "background:",
              tone,
              "0D;"
            ),
            
            shiny::div(
              shiny::span(
                class = "tbi-v2-snapshot-label",
                "RECOMMENDATION"
              ),
              
              shiny::strong(
                style = paste0(
                  "display:block; margin-top:6px;",
                  "color:",
                  tone,
                  "; font-size:1.25rem;"
                ),
                label
              )
            )
          ),
          
          shiny::div(
            shiny::h3(
              style = "margin:0 0 6px;",
              result$recommendation
            ),
            
            shiny::p(
              style = "margin:0; color:#8c9bae; line-height:1.55;",
              result$scope_note
            )
          )
        )
      })
    }
  )
}