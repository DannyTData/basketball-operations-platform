# ============================================================
# PHASE 2 STEP 13 — FINAL INTEGRATION / QA
# Roster Intelligence
# Roster Intelligence stabilization workspace.
# ============================================================

# ------------------------------------------------------------
# Module: Roster Intelligence
# Version 2.4 Executive Roster Workspace — BIE Needs + Gap Analysis
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

roster_column <- function(data, name, default = NA) {
  if (is.data.frame(data) && name %in% names(data)) data[[name]] else rep(default, nrow(data))
}

roster_contract_quality_status <- function(data) {
  allowed <- c("CURRENT", "STALE", "CONFLICT", "UNKNOWN", "REQUIRES REVIEW")
  if ("contract_reconciliation_status" %in% names(data)) {
    status <- toupper(trimws(as.character(data$contract_reconciliation_status)))
    status[is.na(status) | !status %in% allowed] <- "REQUIRES REVIEW"
    return(status)
  }

  known_contract <-
    !is.na(roster_column(data, "contract_id")) |
    (!is.na(roster_column(data, "contract_type")) & nzchar(trimws(as.character(roster_column(data, "contract_type"))))) |
    !is.na(suppressWarnings(as.numeric(roster_column(data, "cap_hit")))) |
    (!is.na(roster_column(data, "contract_end_season")) & nzchar(trimws(as.character(roster_column(data, "contract_end_season"))))) |
    !is.na(suppressWarnings(as.numeric(roster_column(data, "free_agent_year"))))

  ifelse(known_contract, "REQUIRES REVIEW", "UNKNOWN")
}

roster_position_matches <- function(values, position) {
  target <- toupper(trimws(as.character(position %||% "")))
  if (!nzchar(target)) return(rep(TRUE, length(values)))
  tokens <- lapply(strsplit(toupper(as.character(values)), "[,/]", fixed = FALSE), trimws)
  available <- unique(unlist(tokens, use.names = FALSE))
  if (!target %in% available) return(rep(FALSE, length(values)))
  vapply(tokens, function(x) target %in% x, logical(1))
}

roster_filter_selection <- function(values, selected) {
  selected <- trimws(as.character(selected %||% ""))
  if (!nzchar(selected)) return("")
  values <- trimws(as.character(values))
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (selected %in% values) selected else ""
}

roster_filter_records <- function(data,
                                  player_search = "",
                                  position = "",
                                  contract_type = "",
                                  roster_status = "",
                                  free_agent_year = "",
                                  bird_rights = "") {
  stopifnot(is.data.frame(data))
  if (!nrow(data)) return(data)
  keep <- rep(TRUE, nrow(data))

  search <- tolower(trimws(as.character(player_search %||% "")))
  if (nzchar(search)) {
    keep <- keep & grepl(
      search,
      tolower(as.character(roster_column(data, "player_name", ""))),
      fixed = TRUE
    )
  }

  keep <- keep & roster_position_matches(roster_column(data, "primary_position", ""), position)

  apply_exact <- function(values, selected) {
    selected <- trimws(as.character(selected %||% ""))
    if (!nzchar(selected)) return(rep(TRUE, length(values)))
    values <- ifelse(is.na(values), "", trimws(as.character(values)))
    if (!selected %in% unique(values)) return(rep(FALSE, length(values)))
    values == selected
  }

  keep <- keep & apply_exact(roster_column(data, "contract_type", ""), contract_type)
  keep <- keep & apply_exact(roster_column(data, "roster_status", ""), roster_status)
  keep <- keep & apply_exact(roster_column(data, "free_agent_year", ""), free_agent_year)
  keep <- keep & apply_exact(roster_column(data, "bird_rights", ""), bird_rights)
  data[keep, , drop = FALSE]
}

roster_complete_table_data <- function(data) {
  stopifnot(is.data.frame(data))
  numeric_value <- function(name) suppressWarnings(as.numeric(roster_column(data, name)))
  text_value <- function(name, fallback = "—") {
    value <- roster_column(data, name)
    value <- as.character(value)
    value[is.na(value) | !nzchar(trimws(value))] <- fallback
    value
  }

  contract_type_raw <- roster_column(data, "contract_type")
  contract_type_known <- !is.na(contract_type_raw) & nzchar(trimws(as.character(contract_type_raw)))
  contract_type <- text_value("contract_type", "Not classified")
  two_way <- (!is.na(numeric_value("two_way_flag")) & numeric_value("two_way_flag") == 1) |
    grepl("two-way", tolower(contract_type), fixed = TRUE)
  exhibit <- contract_type_known & grepl("exhibit", tolower(contract_type), fixed = TRUE)
  category <- ifelse(
    two_way,
    "Two-Way",
    ifelse(exhibit, "Exhibit 10", ifelse(contract_type_known, "Standard", "UNKNOWN"))
  )
  total_value <- numeric_value("total_value")
  base_salary <- numeric_value("base_salary")
  remaining_money <- ifelse(
    is.na(total_value) | is.na(base_salary),
    NA_real_,
    pmax(0, total_value - base_salary)
  )

  data.frame(
    Player = text_value("player_name", "UNKNOWN"),
    Position = text_value("primary_position"),
    Age = numeric_value("player_age"),
    `Cap Hit` = numeric_value("cap_hit"),
    `Remaining Money` = remaining_money,
    `Contract Through` = text_value("contract_end_season"),
    Contract = contract_type,
    Category = category,
    `Roster Status` = text_value("roster_status", "UNKNOWN"),
    `Bird Rights` = text_value("bird_rights"),
    Option = text_value("option_type"),
    `FA Year` = text_value("free_agent_year"),
    `Data Quality` = roster_contract_quality_status(data),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

roster_money_label <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value)) return("—")
  paste0("$", format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE))
}

roster_complete_reactable <- function(display,
                                      money_formatter = roster_money_label,
                                      link_formatter = function(value, type) shiny::span(value)) {
  token <- function(value, tone = "neutral") {
    shiny::span(class = paste("roster-data-token", paste0("roster-data-token-", tone)), value)
  }
  linked <- function(value, type) {
    shiny::span(class = "roster-data-token roster-data-token-neutral", link_formatter(value, type))
  }
  quality_token <- function(value) {
    tone <- if (identical(value, "CURRENT")) "current" else if (value %in% c("STALE", "CONFLICT")) "warning" else "review"
    token(value, tone)
  }

  reactable::reactable(
    display,
    searchable = FALSE,
    highlight = TRUE,
    striped = FALSE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 15,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 15, 25),
    defaultSorted = "Cap Hit",
    defaultSortOrder = "desc",
    showSortIcon = TRUE,
    showSortable = TRUE,
    theme = reactable::reactableTheme(
      backgroundColor = "transparent",
      color = "#e6edf6",
      borderColor = "rgba(148,163,184,.10)",
      headerStyle = list(backgroundColor = "#111824", color = "#9cabbc", fontWeight = 800),
      rowHighlightStyle = list(backgroundColor = "rgba(59,130,246,.045)")
    ),
    columns = list(
      Player = reactable::colDef(minWidth = 150),
      Position = reactable::colDef(minWidth = 78),
      Age = reactable::colDef(format = reactable::colFormat(digits = 0), sortNALast = TRUE, width = 62),
      `Cap Hit` = reactable::colDef(cell = function(value) money_formatter(value), sortNALast = TRUE, minWidth = 105),
      `Remaining Money` = reactable::colDef(cell = function(value) money_formatter(value), sortNALast = TRUE, minWidth = 125),
      Contract = reactable::colDef(cell = function(value) linked(value, "contract"), minWidth = 145),
      Category = reactable::colDef(cell = function(value) linked(value, "category"), minWidth = 90),
      `Roster Status` = reactable::colDef(cell = function(value) token(value), minWidth = 105),
      `Bird Rights` = reactable::colDef(cell = function(value) linked(value, "bird"), minWidth = 100),
      Option = reactable::colDef(cell = function(value) linked(value, "option"), minWidth = 100),
      `Data Quality` = reactable::colDef(cell = quality_token, minWidth = 128)
    )
  )
}

roster_assessment_readout <- function(construction_assessment, control_assessment) {
  stopifnot(length(construction_assessment) >= 3L, length(control_assessment) >= 2L)

  assessment_item <- function(label, value, tone = NULL) {
    classes <- "roster-assessment-item"
    if (!is.null(tone)) classes <- paste(classes, paste0("roster-assessment-item-", tone))

    shiny::div(
      class = classes,
      shiny::tags$dt(label),
      shiny::tags$dd(value)
    )
  }

  shiny::tags$dl(
    class = "roster-assessment-readout",
    assessment_item("Current state", construction_assessment[[1]]),
    assessment_item("Strength", construction_assessment[[2]]),
    assessment_item("Primary concern", construction_assessment[[3]], "watch"),
    assessment_item("Control / flexibility", control_assessment[[1]]),
    assessment_item("Decision watch", control_assessment[[2]])
  )
}

roster_assessment_control <- function(guarantee_share) {
  if (!is.na(guarantee_share)) {
    return(c(
      sprintf(
        "%.1f%% of current roster cap hits are represented as guaranteed in the loaded contract-year data.",
        100 * guarantee_share
      ),
      "Use the loaded guarantee and option facts when comparing near-term flexibility."
    ))
  }

  c(
    "Guaranteed-money share is UNKNOWN because the loaded roster does not support a complete calculation.",
    "Verify contract guarantees before treating financial flexibility as decision-ready."
  )
}

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
    
    shiny::div(
      class = "tbi-roster-tab-target",
      `data-tbi-roster-tab` = "overview",
      shiny::uiOutput(
        ns("roster_trade_scenario_banner")
      )
    ),
    
    # --------------------------------------------------------
    # Executive snapshot
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "tbi-v2-exec-snapshot tbi-roster-tab-target",
      `data-tbi-roster-tab` = "overview",
      
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
          "DATABASE VIEW"
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
    # Overview decision + headlines
    # --------------------------------------------------------
    
    shiny::div(
      class = "roster-overview-decision-grid tbi-roster-tab-target",
      `data-tbi-roster-tab` = "overview",
      
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
      )
    ),

    # --------------------------------------------------------
    # Roster Construction
    # --------------------------------------------------------

    shiny::div(
      class = "tbi-v2-exec-main-grid tbi-roster-tab-target",
      `data-tbi-roster-tab` = "construction",
      
      shiny::tags$section(
        class = "tbi-v2-scorecard-panel tbi-roster-tab-target",
        `data-tbi-roster-tab` = "construction",
        
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
    # Risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-bottom-grid tbi-roster-tab-target",
      `data-tbi-roster-tab` = "risk",
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-risks-panel tbi-roster-tab-target",
        `data-tbi-roster-tab` = "risk",
        
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
        class = "tbi-v2-exec-list-panel tbi-v2-opportunities-panel tbi-roster-tab-target",
        `data-tbi-roster-tab` = "risk",
        
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
      class = "tbi-v2-cap-detail-grid tbi-roster-tab-target",
      `data-tbi-roster-tab` = "construction assessment",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel tbi-roster-tab-target",
        `data-tbi-roster-tab` = "construction assessment",
        
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
            "DATABASE FACTS"
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
          class = "tbi-v2-context-panel tbi-roster-tab-target",
        `data-tbi-roster-tab` = "assessment",
        
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
          class = "roster-assessment-body",
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
      class = "tbi-v2-context-panel tbi-roster-tab-target",
      `data-tbi-roster-tab` = "legacy-hidden",
      
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
        class = "tbi-data-status",
        shiny::strong("Current-data status: Requires Source Verification"),
        shiny::span("Roster and contract rows remain database-authoritative. No approved current external snapshot is loaded, so extensions, transactions, and contract changes may be stale or conflicting.")
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
      class = "tbi-v2-context-panel tbi-roster-tab-target",
      `data-tbi-roster-tab` = "legacy-hidden",
      
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
      class = "tbi-v2-context-panel tbi-roster-tab-target",
      `data-tbi-roster-tab` = "roster",
      
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
          "DATABASE FACTS"
        )
      ),
      
      shiny::div(
        class = "tbi-data-status roster-contract-quality-banner",
        shiny::strong("Contract-data status: Requires Review"),
        shiny::span(
          paste(
            "Known database facts are shown without an approved current external reconciliation.",
            "Missing contract facts remain UNKNOWN; other rows require source verification."
          )
        )
      ),

      shiny::div(
        class = "roster-filter-bar",
        shiny::div(
          class = "roster-filter-search",
          shiny::textInput(
            ns("roster_player_search"),
            "Player search",
            placeholder = "Search loaded roster"
          )
        ),
        shiny::selectInput(ns("roster_position_filter"), "Position", choices = c("All positions" = ""), selectize = FALSE),
        shiny::selectInput(ns("roster_contract_filter"), "Contract", choices = c("All contracts" = ""), selectize = FALSE),
        shiny::selectInput(ns("roster_status_filter"), "Roster status", choices = c("All statuses" = ""), selectize = FALSE),
        shiny::selectInput(ns("roster_fa_filter"), "FA year", choices = c("All FA years" = ""), selectize = FALSE),
        shiny::selectInput(ns("roster_rights_filter"), "Rights", choices = c("All rights" = ""), selectize = FALSE),
        shiny::actionButton(ns("clear_roster_filters"), "Clear filters", class = "roster-clear-filters")
      ),

      shiny::div(
        class = "roster-table-wrap",
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
        construction = FALSE,
        assessment = FALSE,
        risk = FALSE,
        roster = FALSE,
        `legacy-hidden` = FALSE
      )

      shiny::observeEvent(
        input$active_subtab,
        {
          tab <- as.character(input$active_subtab)
          valid_tabs <- c(
            "overview",
            "construction",
            "assessment",
            "risk",
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

      roster_finding_field <- function(label, value) {
        shiny::div(
          class = "roster-finding-field",
          shiny::span(class = "roster-finding-label", label),
          shiny::p(value)
        )
      }

      roster_finding_card <- function(finding, kind = c("risk", "opportunity")) {
        kind <- match.arg(kind)
        icon <- if (identical(kind, "risk")) "exclamation-triangle" else "graph-up-arrow"
        shiny::div(
          class = paste("roster-finding-card", paste0("roster-finding-card-", kind)),
          shiny::div(
            class = "roster-finding-heading",
            bsicons::bs_icon(icon),
            shiny::strong(if (identical(kind, "risk")) "Risk" else "Opportunity")
          ),
          roster_finding_field(finding$evidence_label %||% "Fact", finding$fact),
          roster_finding_field("Impact", finding$impact),
          roster_finding_field("Decision consequence", finding$decision)
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
        
        con <- connect_db(read_only = TRUE)
        
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

      roster_filter_choices <- function(values) {
        values <- trimws(as.character(values))
        sort(unique(values[!is.na(values) & nzchar(values)]), method = "radix")
      }

      roster_position_filter_choices <- function(data) {
        roster_filter_choices(unlist(
          strsplit(as.character(roster_column(data, "primary_position", "")), "[,/]"),
          use.names = FALSE
        ))
      }

      reset_roster_filters <- function() {
        shiny::updateTextInput(session, "roster_player_search", value = "")
        for (id in c(
          "roster_position_filter", "roster_contract_filter", "roster_status_filter",
          "roster_fa_filter", "roster_rights_filter"
        )) {
          shiny::updateSelectInput(session, id, selected = "")
        }
      }

      shiny::observeEvent(
        list(selected_team(), selected_season()),
        reset_roster_filters(),
        ignoreInit = TRUE
      )

      shiny::observeEvent(
        selected_roster(),
        {
          d <- selected_roster()
          position_tokens <- roster_position_filter_choices(d)

          shiny::updateSelectInput(
            session, "roster_position_filter",
            choices = c("All positions" = "", stats::setNames(position_tokens, position_tokens))
          )
          shiny::updateSelectInput(
            session, "roster_contract_filter",
            choices = {
              values <- roster_filter_choices(roster_column(d, "contract_type", ""))
              c("All contracts" = "", stats::setNames(values, values))
            }
          )
          shiny::updateSelectInput(
            session, "roster_status_filter",
            choices = {
              values <- roster_filter_choices(roster_column(d, "roster_status", ""))
              c("All statuses" = "", stats::setNames(values, values))
            }
          )
          shiny::updateSelectInput(
            session, "roster_fa_filter",
            choices = {
              values <- roster_filter_choices(roster_column(d, "free_agent_year", ""))
              c("All FA years" = "", stats::setNames(values, values))
            }
          )
          shiny::updateSelectInput(
            session, "roster_rights_filter",
            choices = {
              values <- roster_filter_choices(roster_column(d, "bird_rights", ""))
              c("All rights" = "", stats::setNames(values, values))
            }
          )
        },
        ignoreInit = FALSE
      )

      shiny::observeEvent(input$clear_roster_filters, {
        reset_roster_filters()
      })

      filtered_roster <- shiny::reactive({
        d <- selected_roster()
        roster_filter_records(
          d,
          player_search = input$roster_player_search %||% "",
          position = roster_filter_selection(
            roster_position_filter_choices(d), input$roster_position_filter
          ),
          contract_type = roster_filter_selection(
            roster_column(d, "contract_type", ""), input$roster_contract_filter
          ),
          roster_status = roster_filter_selection(
            roster_column(d, "roster_status", ""), input$roster_status_filter
          ),
          free_agent_year = roster_filter_selection(
            roster_column(d, "free_agent_year", ""), input$roster_fa_filter
          ),
          bird_rights = roster_filter_selection(
            roster_column(d, "bird_rights", ""), input$roster_rights_filter
          )
        )
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
        subtab_ready("risk")
        d <- selected_roster()
        scores <- position_scores()
        
        risks <- list()
        
        weak <- scores[
          scores$score < 60,
          ,
          drop = FALSE
        ]
        
        if (nrow(weak)) {
          risks[[length(risks) + 1L]] <- list(
            evidence_label = "MODEL OUTPUT",
            fact = paste0(
              "Position Value 2.0 indicates ",
              paste(
                weak$position,
                collapse = ", "
              ),
              " grades below 60."
            ),
            impact = "Model-indicated position-value pressure is concentrated in the identified groups.",
            decision = "Prioritize those positions when comparing development, roster, or transaction alternatives."
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
            risks[[length(risks) + 1L]] <- list(
              fact = sprintf(
                "Top-three contracts account for %.1f%% of current payroll.",
                100 *
                  concentration
              ),
              impact = "A large share of the loaded payroll is concentrated in three roster spots.",
              decision = "Test depth-retention and upgrade options against the remaining payroll flexibility."
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
          risks[[length(risks) + 1L]] <- list(
            fact = paste0(
              expiring,
              " contracts reach free agency by 2028, creating meaningful near-term turnover."
            ),
            impact = "Several roster decisions may require sequencing in the same planning window.",
            decision = "Review retention priorities before committing flexibility to secondary needs."
          )
        }
        
        if (!length(risks)) {
          risks[[1L]] <- list(
            fact = "No major structural roster risk is identified by the loaded contract and depth inputs.",
            impact = "The current database view does not surface a controlling roster imbalance.",
            decision = "Preserve flexibility and verify current source facts before making a roster move."
          )
        }
        
        shiny::tagList(
          lapply(
            risks,
            roster_finding_card,
            kind = "risk"
          )
        )
      })
      
      # ------------------------------------------------------
      # Opportunities
      # ------------------------------------------------------
      
      output$roster_opportunities <- shiny::renderUI({
        subtab_ready("risk")
        d <- selected_roster()
        scores <- position_scores()
        
        opportunities <- list()
        
        open_spots <- max(
          0,
          15 -
            nrow(
              standard_roster()
            )
        )
        
        if (open_spots > 0) {
          opportunities[[length(opportunities) + 1L]] <- list(
            fact = paste0(
              open_spots,
              " standard roster spot",
              if (open_spots == 1) "" else "s",
              " remain available in the current roster view."
            ),
            impact = "The loaded roster has room for an additional standard-contract player.",
            decision = "Compare position need and financial fit before using the available roster spot."
          )
        }
        
        strong <- scores[
          scores$score >= 80,
          ,
          drop = FALSE
        ]
        
        if (nrow(strong)) {
          opportunities[[length(opportunities) + 1L]] <- list(
            evidence_label = "MODEL OUTPUT",
            fact = paste0(
              "Position Value 2.0 indicates ",
              paste(
                strong$position,
                collapse = ", "
              ),
              " provides strong positional depth and optionality."
            ),
            impact = "Strength in those groups can support internal role coverage or transaction flexibility.",
            decision = "Protect essential depth while evaluating whether surplus value can address weaker positions."
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
          opportunities[[length(opportunities) + 1L]] <- list(
            fact = paste0(
              team_options,
              " team-controlled option",
              if (team_options == 1) "" else "s",
              " can support future roster flexibility."
            ),
            impact = "The loaded contracts include team-controlled decision points.",
            decision = "Sequence option decisions with retention, cap, and roster-slot planning."
          )
        }
        
        if (!length(opportunities)) {
          opportunities[[1L]] <- list(
            fact = "No specific roster-control opportunity is identified by the loaded inputs.",
            impact = "The database view does not support a stronger opportunity claim.",
            decision = "Preserve flexibility while comparing targeted upgrades with development alternatives."
          )
        }
        
        shiny::tagList(
          lapply(
            opportunities,
            roster_finding_card,
            kind = "opportunity"
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
        subtab_ready("assessment")
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
        
        guarantee_values <- suppressWarnings(as.numeric(d$guaranteed_amount))
        guaranteed <- sum(guarantee_values, na.rm = TRUE)
        
        total <- roster_payroll()
        
        guarantee_share <- if (
          total > 0 && length(guarantee_values) && all(!is.na(guarantee_values))
        ) {
          guaranteed /
            total
        } else {
          NA_real_
        }
        
        construction_assessment <- c(
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

        control_assessment <- roster_assessment_control(guarantee_share)

        roster_assessment_readout(construction_assessment, control_assessment)
      })
      
      # ------------------------------------------------------
      # Complete roster table
      # ------------------------------------------------------
      
      output$roster_table <- reactable::renderReactable({
        d <- filtered_roster()
        
        shiny::validate(
          shiny::need(
            nrow(d) > 0,
            paste(
              "No roster rows match the active filters for",
              selected_team(),
              "during",
              selected_season()
            )
          )
        )
        
        display <- roster_complete_table_data(d)

        roster_complete_reactable(
          display,
          money_formatter = money,
          link_formatter = function(value, type) {
            roster_table_link(value, roster_cba_term(value, type))
          }
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
