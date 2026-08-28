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

extension_ui_status_label <- function(status) {
  normalized <- toupper(v2_ui_text(status))

  switch(
    normalized,
    "ELIGIBLE_WITH_REVIEW" = "Eligible — Review Required",
    "PASS_WITH_REVIEW" = "Pass — Review Required",
    "ELIGIBLE" = "Eligible",
    "INELIGIBLE" = "Not Eligible",
    "NOT_ELIGIBLE" = "Not Eligible",
    "PASS" = "Pass",
    "REVIEW" = "Review Required",
    "FAIL" = "Fail",
    "UNKNOWN" = "Unknown",
    v2_ui_humanize(normalized)
  )
}


extension_manual_fact_ids <- function() {
  c(
    "is_first_round_pick",
    "rookie_options_exercised",
    "timing_window_open",
    "designated_rookie_qualified",
    "designated_veteran_qualified",
    "original_team_requirement_met"
  )
}


extension_manual_fact_choices <- function() {
  c(
    "Unknown" = "UNKNOWN",
    "Met" = "MET",
    "Not met" = "NOT_MET"
  )
}


extension_manual_fact_labels <- function() {
  c(
    is_first_round_pick = "first-round draft-pick status",
    rookie_options_exercised = "required rookie-scale option years",
    timing_window_open = "applicable extension signing window",
    designated_rookie_qualified = "designated-rookie qualification",
    designated_veteran_qualified = "designated-veteran award qualification",
    original_team_requirement_met = "designated-veteran original-team requirement"
  )
}


extension_manual_fact_state <- function(value) {
  if (is.logical(value) && length(value) && !is.na(value[[1]])) {
    return(if (isTRUE(value[[1]])) "MET" else "NOT_MET")
  }

  if (
    is.numeric(value) &&
    length(value) &&
    !is.na(value[[1]]) &&
    value[[1]] %in% c(0, 1)
  ) {
    return(if (value[[1]] == 1) "MET" else "NOT_MET")
  }

  if (is.null(value) || !length(value) || is.na(value[[1]])) {
    return("UNKNOWN")
  }

  normalized <- toupper(trimws(as.character(value[[1]])))

  if (!nzchar(normalized)) {
    return("UNKNOWN")
  }

  switch(
    normalized,
    "MET" = "MET",
    "TRUE" = "MET",
    "YES" = "MET",
    "NOT_MET" = "NOT_MET",
    "NOT MET" = "NOT_MET",
    "FALSE" = "NOT_MET",
    "NO" = "NOT_MET",
    "UNKNOWN"
  )
}


extension_required_manual_facts <- function(extension_type) {
  extension_type <- tolower(trimws(as.character(extension_type %||% "")))

  switch(
    extension_type,
    "veteran" = "timing_window_open",
    "rookie_scale" = c(
      "is_first_round_pick",
      "rookie_options_exercised",
      "timing_window_open"
    ),
    "designated_rookie" = c(
      "is_first_round_pick",
      "rookie_options_exercised",
      "timing_window_open",
      "designated_rookie_qualified"
    ),
    "designated_veteran" = c(
      "timing_window_open",
      "designated_veteran_qualified",
      "original_team_requirement_met"
    ),
    extension_manual_fact_ids()
  )
}


extension_manual_fact_contract <- function(values, extension_type) {
  if (!is.list(values)) {
    values <- list()
  }

  fact_ids <- extension_manual_fact_ids()
  states <- stats::setNames(
    vapply(
      fact_ids,
      function(fact_id) {
        extension_manual_fact_state(values[[fact_id]])
      },
      character(1)
    ),
    fact_ids
  )
  required <- extension_required_manual_facts(extension_type)
  unresolved <- required[states[required] == "UNKNOWN"]
  flags <- stats::setNames(rep(FALSE, length(fact_ids)), fact_ids)
  resolved_required <- setdiff(required, unresolved)

  if (length(resolved_required)) {
    flags[resolved_required] <- states[resolved_required] == "MET"
  }

  list(
    states = states,
    required = required,
    unresolved = unname(unresolved),
    flags = flags,
    resolved = !length(unresolved)
  )
}


extension_service_year_state <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1L]])) {
    return(NA_integer_)
  }

  normalized <- toupper(trimws(as.character(value[[1L]])))
  if (!nzchar(normalized) || normalized == "UNKNOWN") {
    return(NA_integer_)
  }

  numeric_value <- suppressWarnings(as.numeric(normalized))
  if (
    is.na(numeric_value) ||
    !is.finite(numeric_value) ||
    numeric_value < 0 ||
    numeric_value > 25 ||
    abs(numeric_value - round(numeric_value)) > .Machine$double.eps^0.5
  ) {
    return(NA_integer_)
  }

  as.integer(round(numeric_value))
}


extension_service_years_from_row <- function(player_row) {
  if (is.null(player_row) || (!is.data.frame(player_row) && !is.list(player_row))) {
    return(NA_integer_)
  }

  for (field in c("nba_service_years", "service_years")) {
    if (field %in% names(player_row)) {
      value <- extension_service_year_state(player_row[[field]])
      if (!is.na(value)) return(value)
    }
  }

  NA_integer_
}


extension_service_year_choices <- function() {
  c(
    "UNKNOWN" = "UNKNOWN",
    stats::setNames(as.character(0:25), as.character(0:25))
  )
}


extension_review_field_labels <- function() {
  c(
    service_years = "NBA service years",
    is_first_round_pick = "First-round draft-pick status",
    rookie_options_exercised = "Required rookie-scale option years",
    timing_window_open = "Applicable extension signing window",
    designated_rookie_qualified = "Designated-rookie qualification",
    designated_veteran_qualified = "Designated-veteran award qualification",
    original_team_requirement_met = "Designated-veteran original-team requirement"
  )
}


extension_review_reasons <- function(result) {
  if (!is.list(result)) return(character())

  if (length(result$review_reasons %||% character())) {
    return(unique(as.character(result$review_reasons)))
  }

  missing_fields <- as.character(result$missing_fields %||% character())
  if (length(missing_fields)) {
    labels <- extension_review_field_labels()
    resolved <- unname(labels[missing_fields])
    unresolved <- is.na(resolved)
    resolved[unresolved] <- vapply(
      missing_fields[unresolved],
      v2_ui_humanize,
      character(1)
    )
    return(unique(resolved))
  }

  unique(as.character(result$warnings %||% character()))
}


extension_is_known_failure <- function(result) {
  is.list(result) && (
    identical(result$status, "FAIL") ||
      inherits(result, "tbi_extension_failure")
  )
}


extension_known_eligibility_failure <- function(player, extension_type) {
  eligibility <- tryCatch(
    screen_extension_eligibility(
      player = player,
      extension_type = extension_type
    ),
    error = function(e) NULL
  )
  if (!is.list(eligibility)) return(NULL)

  failures <- unique(as.character(eligibility$failures %||% character()))
  failures <- failures[!is.na(failures) & nzchar(failures)]
  if (!length(failures)) return(NULL)

  structure(
    list(
      status = "FAIL",
      passes_screen = FALSE,
      requires_manual_review = TRUE,
      reason_code = "MODELED_ELIGIBILITY_FAILURE",
      eligibility = eligibility,
      failures = failures,
      warnings = eligibility$warnings %||% character(),
      message = paste(
        "The proposal fails the modeled eligibility screen.",
        paste(failures, collapse = " ")
      )
    ),
    class = c("tbi_extension_failure", "tbi_extension_error")
  )
}


extension_analysis_is_pending <- function(result) {
  if (!is.list(result)) return(FALSE)

  missing_fields <- as.character(result$missing_fields %||% character())

  inherits(result, "tbi_extension_review") &&
    identical(result$status, "REVIEW") &&
    isTRUE(is.na(result$passes_screen)) &&
    !extension_is_known_failure(result) &&
    any(!is.na(missing_fields) & nzchar(missing_fields))
}


extension_analysis_pending_facts <- function(result) {
  if (!extension_analysis_is_pending(result)) {
    return(data.frame(
      field = character(),
      label = character(),
      state = character(),
      stringsAsFactors = FALSE
    ))
  }

  fields <- unique(as.character(result$missing_fields %||% character()))
  fields <- fields[!is.na(fields) & nzchar(fields)]
  labels <- extension_review_field_labels()
  resolved_labels <- unname(labels[fields])
  fallback <- is.na(resolved_labels)
  resolved_labels[fallback] <- vapply(
    fields[fallback],
    v2_ui_humanize,
    character(1)
  )

  data.frame(
    field = fields,
    label = resolved_labels,
    state = ifelse(
      fields == "timing_window_open",
      "REQUIRES SOURCE VERIFICATION",
      "UNKNOWN"
    ),
    stringsAsFactors = FALSE
  )
}


extension_analysis_pending_ui <- function(result) {
  facts <- extension_analysis_pending_facts(result)
  if (!nrow(facts)) return(NULL)

  shiny::tags$section(
    class = "ext-analysis-pending",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    shiny::tags$h3("EXTENSION ANALYSIS PENDING"),
    shiny::tags$p(
      class = "ext-analysis-pending-summary",
      "The modeled extension cannot be calculated until required eligibility and timing facts are verified."
    ),
    shiny::tags$dl(
      class = "ext-analysis-pending-facts",
      lapply(seq_len(nrow(facts)), function(index) {
        shiny::div(
          class = "ext-analysis-pending-fact",
          shiny::tags$dt(facts$label[[index]]),
          shiny::tags$dd(facts$state[[index]])
        )
      })
    ),
    shiny::div(
      class = "ext-analysis-pending-next",
      shiny::tags$h4("NEXT ACTION"),
      shiny::tags$p(
        "Complete the required source verification on the Proposal tab."
      )
    )
  )
}


extension_review_reason_panel <- function(result, screen_passed = FALSE) {
  reasons <- extension_review_reasons(result)
  if (!length(reasons)) return(NULL)

  shiny::div(
    class = "ext-review-required",
    shiny::tags$h4("REVIEW REQUIRED BECAUSE"),
    shiny::tags$p(
      if (isTRUE(screen_passed)) {
        "Modeled financial/CBA screen passes, but specific source facts remain unverified."
      } else {
        "The modeled financial/CBA screen is paused until these source facts are verified."
      }
    ),
    shiny::tags$ul(
      class = "ext-recommendation-list",
      lapply(reasons, shiny::tags$li)
    )
  )
}


extension_manual_input_defaults <- function(player_row = NULL) {
  c(
    list(
      service_years = extension_service_years_from_row(player_row),
      remaining_years = 1L
    ),
    stats::setNames(
      as.list(rep("UNKNOWN", length(extension_manual_fact_ids()))),
      extension_manual_fact_ids()
    )
  )
}

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

  manual_fact_input <- function(input_id, label) {
    shiny::selectInput(
      ns(input_id),
      label,
      choices = extension_manual_fact_choices(),
      selected = "UNKNOWN",
      selectize = FALSE,
      width = "100%"
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
          --ext-space-1: 4px;
          --ext-space-2: 8px;
          --ext-space-3: 12px;
          --ext-space-4: 16px;
        }

        .ext-builder-grid {
          display: grid;
          grid-template-columns: minmax(320px,5fr) minmax(0,7fr);
          gap: var(--ext-space-3);
        }

        .ext-form-grid {
          display: grid;
          grid-template-columns: repeat(2,minmax(0,1fr));
          gap: var(--ext-space-2) var(--ext-space-3);
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
          padding: var(--ext-space-4);
        }

        .ext-builder-title {
          margin-bottom: 12px;
          color: #f3f6fa;
          font-size: 1rem;
          font-weight: 780;
        }

        .ext-player-card {
          margin-bottom: var(--ext-space-3);
          padding: var(--ext-space-3);
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
          margin-top: var(--ext-space-3);
          padding: var(--ext-space-3);
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

        .ext-verify-options {
          display: grid;
          grid-template-columns: repeat(2,minmax(0,1fr));
          gap: var(--ext-space-1) var(--ext-space-3);
        }

        .ext-verify-options .form-group {
          min-width: 0;
          margin-bottom: 0 !important;
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
          gap: var(--ext-space-2);
          margin-top: var(--ext-space-3);
        }

        .ext-summary-card {
          min-height: 60px;
          padding: var(--ext-space-2);
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
          overflow-x: auto;
        }

        .ext-financial-grid {
          grid-template-columns: minmax(0,3fr) minmax(300px,2fr) !important;
          gap: var(--ext-space-3) !important;
        }

        .ext-panel-body {
          padding: var(--ext-space-4);
        }

        .ext-status-row {
          min-height: 36px;
          padding: var(--ext-space-2) 0;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: var(--ext-space-3);
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

        .ext-cba-details {
          padding: var(--ext-space-3) var(--ext-space-4);
        }

        .ext-cba-details summary {
          color: #b9c6d6;
          font-size: .72rem;
          font-weight: 800;
          cursor: pointer;
        }

        .ext-bie-metrics {
          display: grid;
          grid-template-columns: repeat(6,minmax(0,1fr));
          gap: var(--ext-space-2);
        }

        .ext-bie-summary {
          display: grid;
          grid-template-columns: minmax(220px,2fr) minmax(0,5fr);
          gap: var(--ext-space-3);
          align-items: center;
          margin-top: var(--ext-space-3);
        }

        .ext-recommendation-brief {
          display: grid;
          grid-template-columns: minmax(220px,.8fr) minmax(0,1.2fr) minmax(0,1fr);
          gap: var(--ext-space-3);
        }

        .ext-recommendation-decision,
        .ext-recommendation-column {
          min-width: 0;
          padding: var(--ext-space-4);
        }

        .ext-recommendation-decision {
          border: 1px solid rgba(96,165,250,.20);
          border-radius: 12px;
          background: rgba(37,99,235,.07);
        }

        .ext-recommendation-column {
          border-left: 1px solid rgba(148,163,184,.10);
        }

        .ext-recommendation-column h4,
        .ext-recommendation-decision h4 {
          margin: 0 0 var(--ext-space-2);
          color: #dce6f2;
          font-size: .72rem;
        }

        .ext-recommendation-list {
          margin: 0;
          padding-left: var(--ext-space-4);
          color: #9eacbd;
          font-size: .72rem;
          line-height: 1.45;
        }

        .ext-recommendation-list li + li {
          margin-top: var(--ext-space-2);
        }

        .ext-review-required {
          margin-top: var(--ext-space-3);
          padding: var(--ext-space-3);
          border: 1px solid rgba(245,158,11,.24);
          border-radius: 9px;
          background: rgba(245,158,11,.055);
        }

        .ext-review-required h4 {
          margin: 0 0 var(--ext-space-2);
          color: #f7c86a;
          font-size: .7rem;
          letter-spacing: .04em;
        }

        .ext-review-required p {
          margin: 0 0 var(--ext-space-2);
          color: #c8d2df;
          font-size: .68rem;
          line-height: 1.45;
        }

        .ext-analysis-pending-slot {
          grid-column: 1 / -1;
        }

        .ext-tab-section-slot > section,
        .tbi-v2-snapshot-grid > .shiny-panel-conditional > .tbi-v2-snapshot-item {
          height: 100%;
        }

        .ext-analysis-pending {
          width: 100%;
          padding: var(--ext-space-4);
          border: 1px solid rgba(245,158,11,.24);
          border-radius: 12px;
          background: rgba(245,158,11,.055);
        }

        .ext-analysis-pending h3 {
          margin: 0;
          color: #f7c86a;
          font-size: .82rem;
          font-weight: 850;
          letter-spacing: .06em;
        }

        .ext-analysis-pending-summary {
          max-width: 68ch;
          margin: var(--ext-space-2) 0 var(--ext-space-4);
          color: #c8d2df;
          font-size: .72rem;
          line-height: 1.5;
        }

        .ext-analysis-pending-facts {
          margin: 0;
        }

        .ext-analysis-pending-fact {
          display: grid;
          grid-template-columns: minmax(0,1fr) auto;
          gap: var(--ext-space-3);
          padding: var(--ext-space-2) 0;
          border-bottom: 1px solid rgba(245,158,11,.14);
        }

        .ext-analysis-pending-fact dt,
        .ext-analysis-pending-fact dd {
          margin: 0;
          font-size: .68rem;
          line-height: 1.4;
        }

        .ext-analysis-pending-fact dt {
          color: #aebaca;
        }

        .ext-analysis-pending-fact dd {
          color: #f7c86a;
          font-weight: 800;
          letter-spacing: .025em;
          text-align: right;
        }

        .ext-analysis-pending-next {
          margin-top: var(--ext-space-4);
        }

        .ext-analysis-pending-next h4 {
          margin: 0 0 var(--ext-space-2);
          color: #edf3f9;
          font-size: .62rem;
          font-weight: 850;
          letter-spacing: .08em;
        }

        .ext-analysis-pending-next p {
          margin: 0;
          color: #c8d2df;
          font-size: .72rem;
          line-height: 1.5;
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

        @media (max-width: 1000px) {
          .ext-builder-grid {
            grid-template-columns: 1fr;
          }

          .ext-financial-grid,
          .ext-recommendation-brief {
            grid-template-columns: 1fr !important;
          }

          .ext-recommendation-column {
            border-left: 0;
            border-top: 1px solid rgba(148,163,184,.10);
          }

          .ext-bie-metrics {
            grid-template-columns: repeat(3,minmax(0,1fr));
          }
        }

        @media (max-width: 720px) {
          .ext-form-grid,
          .ext-verify-options {
            grid-template-columns: 1fr;
          }

          .ext-summary-strip,
          .ext-bie-metrics {
            grid-template-columns: repeat(2,minmax(0,1fr));
          }

          .ext-bie-summary {
            grid-template-columns: 1fr;
          }

          .ext-status-row {
            align-items: flex-start;
            flex-wrap: wrap;
          }

          .ext-status-row strong {
            width: 100%;
            text-align: left;
          }

          .ext-analysis-pending-fact {
            grid-template-columns: 1fr;
            gap: 2px;
          }

          .ext-analysis-pending-fact dd {
            text-align: left;
          }
        }

        @media (max-width: 480px) {
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

    shiny::div(
      class = "ext-analysis-state-flag",
      `aria-hidden` = "true",
      style = "display:none;",
      shiny::textOutput(
        ns("extension_analysis_pending_flag"),
        inline = TRUE
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
        
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'ready'",
          snapshot_item(
            "TOTAL VALUE",
            "snapshot_total_value",
            "cash-stack",
            "green"
          ),
          ns = ns
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
      class = "ext-builder-grid tbi-extension-tab-layout",
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        `data-tbi-extension-section` = "player-eligibility",
        `data-tbi-extension-tab` = "proposal",
        
        shiny::div(
          class = "tbi-v2-context-header",
          
          shiny::div(
            shiny::div(
              class = "tbi-page-eyebrow",
              "PLAYER & ELIGIBILITY"
            ),
            shiny::h3(
              "Contract inputs for verification"
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
            
            shiny::selectInput(
              ns("service_years"),
              "NBA Service Years",
              choices = extension_service_year_choices(),
              selected = "UNKNOWN"
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

          shiny::p(
            class = "ext-input-note",
            paste(
              "Service years are not loaded from the current authoritative database.",
              "Keep UNKNOWN unless an official service calculation has been verified."
            )
          ),
          
          shiny::div(
            class = "ext-verify-box",
            
            shiny::div(
              class = "ext-verify-title",
              "ELIGIBILITY FACTS TO VERIFY"
            ),
            shiny::div(
              class = "ext-verify-options",
              manual_fact_input(
                "is_first_round_pick",
                "First-round draft pick"
              ),
              manual_fact_input(
                "rookie_options_exercised",
                "Required rookie-scale option years exercised"
              ),
              manual_fact_input(
                "timing_window_open",
                "Applicable extension signing window confirmed open"
              ),
              manual_fact_input(
                "designated_rookie_qualified",
                "Designated-rookie qualification confirmed"
              ),
              manual_fact_input(
                "designated_veteran_qualified",
                "Designated-veteran award qualification confirmed"
              ),
              manual_fact_input(
                "original_team_requirement_met",
                "Designated-veteran original-team requirement confirmed"
              )
            )
          ),
          
          shiny::div(
            class = "ext-input-note",
            paste(
              "Unknown applicable facts route the screen to REVIEW and are never",
              "treated as not met. Manual values reset with player, team, or season."
            )
          )
        )
      ),
      
      shiny::tags$section(
        class = "tbi-v2-context-panel",
        `data-tbi-extension-section` = "proposal-builder",
        `data-tbi-extension-tab` = "proposal",
        
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
      class = "tbi-v2-exec-main-grid tbi-extension-tab-layout",
      
      shiny::tags$section(
        class = "tbi-v2-decision-card",
        `data-tbi-extension-section` = "extension-decision",
        `data-tbi-extension-tab` = "cba-screen",
        
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
        `data-tbi-extension-section` = "cba-extension-scorecard",
        `data-tbi-extension-tab` = "cba-screen",
        
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
      class = "tbi-v2-cap-detail-grid ext-financial-grid tbi-extension-tab-layout",

      htmltools::tagAppendAttributes(
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'pending'",
          shiny::uiOutput(ns("financial_analysis_pending")),
          ns = ns
        ),
        class = "ext-analysis-pending-slot",
        `data-tbi-extension-tab` = "financial-impact"
      ),

      htmltools::tagAppendAttributes(
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'ready'",
          shiny::tags$section(
            class = "tbi-v2-context-panel",
            shiny::div(
              class = "tbi-v2-context-header",
              shiny::div(
                shiny::div(
                  class = "tbi-page-eyebrow",
                  "FINANCIAL IMPACT"
                ),
                shiny::h3("Proposed extension schedule")
              ),
              shiny::span(
                class = "tbi-v2-context-tag",
                "MODELED"
              )
            ),
            shiny::div(
              class = "ext-schedule-wrap",
              reactable::reactableOutput(ns("extension_schedule"))
            )
          ),
          ns = ns
        ),
        class = "ext-tab-section-slot",
        `data-tbi-extension-section` = "proposed-extension-schedule",
        `data-tbi-extension-tab` = "financial-impact"
      ),

      htmltools::tagAppendAttributes(
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'ready'",
          shiny::tags$section(
            class = "tbi-v2-context-panel",
            shiny::div(
              class = "tbi-v2-context-header",
              shiny::div(
                shiny::div(
                  class = "tbi-page-eyebrow",
                  "CONTRACT ENGINE"
                ),
                shiny::h3("Front-office readout")
              ),
              shiny::span(
                class = "tbi-v2-context-tag",
                "DECISION SUPPORT"
              )
            ),
            shiny::div(
              class = "ext-panel-body",
              shiny::uiOutput(ns("contract_readout"))
            )
          ),
          ns = ns
        ),
        class = "ext-tab-section-slot",
        `data-tbi-extension-section` = "front-office-readout",
        `data-tbi-extension-tab` = "financial-impact"
      )
    ),

    # --------------------------------------------------------
    # Executive recommendation
    # --------------------------------------------------------

    htmltools::tagAppendAttributes(
      shiny::conditionalPanel(
        condition = "output.extension_analysis_pending_flag === 'pending'",
        shiny::uiOutput(ns("recommendation_analysis_pending")),
        ns = ns
      ),
      class = "ext-analysis-pending-slot",
      `data-tbi-extension-tab` = "recommendation"
    ),

    htmltools::tagAppendAttributes(
      shiny::conditionalPanel(
        condition = "output.extension_analysis_pending_flag === 'ready'",
        shiny::tags$section(
          class = "tbi-v2-context-panel",
          shiny::div(
            class = "tbi-v2-context-header",
            shiny::div(
              shiny::div(class = "tbi-page-eyebrow", "EXECUTIVE RECOMMENDATION"),
              shiny::h3("Recommended contract action")
            ),
            shiny::span(class = "tbi-v2-context-tag", "CBA + FINANCIAL + BIE")
          ),
          shiny::div(
            class = "ext-panel-body",
            shiny::uiOutput(ns("executive_recommendation"))
          )
        ),
        ns = ns
      ),
      class = "ext-tab-section-slot",
      `data-tbi-extension-section` = "recommended-contract-action",
      `data-tbi-extension-tab` = "recommendation"
    ),
    
    # --------------------------------------------------------
    # Alerts / risks / opportunities
    # --------------------------------------------------------
    
    shiny::div(
      class = "tbi-v2-exec-bottom-grid tbi-extension-tab-layout",
      
      shiny::tags$section(
        class = "tbi-v2-exec-list-panel tbi-v2-headlines-panel",
        `data-tbi-extension-section` = "cba-review-items",
        `data-tbi-extension-tab` = "cba-screen",
        
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
        
        shiny::tags$details(
          class = "ext-cba-details",
          shiny::tags$summary("View rule explanations and verification items"),
          shiny::uiOutput(ns("extension_alerts"))
        )
      ),
      
      htmltools::tagAppendAttributes(
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'ready'",
          shiny::tags$section(
            class = "tbi-v2-exec-list-panel tbi-v2-risks-panel",
            shiny::div(
              class = "tbi-v2-section-title",
              shiny::span(
                class = "tbi-v2-section-icon tbi-v2-section-icon-danger",
                bsicons::bs_icon("exclamation-triangle")
              ),
              shiny::span("CONTRACT RISKS")
            ),
            shiny::uiOutput(ns("extension_risks"))
          ),
          ns = ns
        ),
        class = "ext-tab-section-slot",
        `data-tbi-extension-section` = "contract-risks",
        `data-tbi-extension-tab` = "recommendation"
      ),

      htmltools::tagAppendAttributes(
        shiny::conditionalPanel(
          condition = "output.extension_analysis_pending_flag === 'ready'",
          shiny::tags$section(
            class = "tbi-v2-exec-list-panel tbi-v2-opportunities-panel",
            shiny::div(
              class = "tbi-v2-section-title",
              shiny::span(
                class = "tbi-v2-section-icon tbi-v2-section-icon-success",
                bsicons::bs_icon("bullseye")
              ),
              shiny::span("NEGOTIATION OPPORTUNITIES")
            ),
            shiny::uiOutput(ns("extension_opportunities"))
          ),
          ns = ns
        ),
        class = "ext-tab-section-slot",
        `data-tbi-extension-section` = "negotiation-opportunities",
        `data-tbi-extension-tab` = "recommendation"
      )
    ),
    
    # --------------------------------------------------------
    # BIE Contract Value Intelligence
    # --------------------------------------------------------
    
    htmltools::tagAppendAttributes(
      shiny::conditionalPanel(
        condition = "output.extension_analysis_pending_flag === 'ready'",
        shiny::tags$section(
          class = "tbi-v2-context-panel",
          shiny::div(
            class = "tbi-v2-context-header",
            shiny::div(
              shiny::div(
                class = "tbi-page-eyebrow",
                "BASKETBALL INTELLIGENCE ENGINE"
              ),
              shiny::h3("Extension value + timeline")
            ),
            shiny::span(
              class = "tbi-v2-context-tag",
              "BIE CONTRACT VALUE"
            )
          ),
          shiny::div(
            class = "ext-panel-body",
            shiny::tags$details(
              class = "ext-cba-details",
              shiny::tags$summary("View BIE value and timeline details"),
              shiny::uiOutput(ns("bie_extension_value"))
            )
          )
        ),
        ns = ns
      ),
      class = "ext-tab-section-slot",
      `data-tbi-extension-section` = "bie-extension-value-timeline",
      `data-tbi-extension-tab` = "recommendation"
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

      # Manual facts are intentionally session-scoped and context-bound. The
      # engine never reads the raw Shiny inputs, because those values can lag
      # behind update*Input() messages when player/team/season changes.
      manual_context_key <- shiny::reactive({
        v2_input_signature(list(
          team = as.character(selected_team() %||% ""),
          season = as.character(selected_season() %||% ""),
          player_id = as.character(input$player_id %||% "")
        ))
      })

      manual_extension_state <- shiny::reactiveVal(
        list(
          context_key = NULL,
          values = extension_manual_input_defaults()
        )
      )
      manual_context_reset_pending <- shiny::reactiveVal(FALSE)

      manual_extension_inputs <- shiny::reactive({
        snapshot <- manual_extension_state()

        if (!identical(snapshot$context_key, manual_context_key())) {
          return(extension_manual_input_defaults())
        }

        snapshot$values
      })

      shiny::observeEvent(
        manual_context_key(),
        {
          context_key <- manual_context_key()
          player_row <- tryCatch(
            selected_player_row(),
            error = function(...) NULL
          )
          values <- extension_manual_input_defaults(player_row)

          manual_context_reset_pending(TRUE)
          session$onFlushed(
            function() {
              manual_context_reset_pending(FALSE)
            },
            once = TRUE
          )

          manual_extension_state(
            list(
              context_key = context_key,
              values = values
            )
          )

          shiny::updateSelectInput(
            session,
            "service_years",
            selected = if (is.na(values$service_years)) {
              "UNKNOWN"
            } else {
              as.character(values$service_years)
            }
          )
          shiny::updateNumericInput(
            session,
            "remaining_years",
            value = values$remaining_years
          )

          for (fact_id in extension_manual_fact_ids()) {
            shiny::updateSelectInput(
              session,
              fact_id,
              selected = values[[fact_id]]
            )
          }
        },
        ignoreInit = FALSE,
        priority = 100
      )

      manual_input_observers <- lapply(
        c(
          "service_years",
          "remaining_years",
          extension_manual_fact_ids()
        ),
        function(input_id) {
          shiny::observeEvent(
            input[[input_id]],
            {
              snapshot <- manual_extension_state()
              context_key <- manual_context_key()

              if (
                isTRUE(manual_context_reset_pending()) ||
                !identical(snapshot$context_key, context_key)
              ) {
                return()
              }

              value <- if (input_id %in% extension_manual_fact_ids()) {
                extension_manual_fact_state(input[[input_id]])
              } else if (identical(input_id, "service_years")) {
                extension_service_year_state(input[[input_id]])
              } else {
                as.integer(
                  safe_num(
                    input[[input_id]],
                    extension_manual_input_defaults()[[input_id]]
                  )
                )
              }

              snapshot$values[[input_id]] <- value
              manual_extension_state(snapshot)
            },
            ignoreInit = TRUE,
            priority = 0
          )
        }
      )

      manual_fact_contract <- shiny::reactive({
        extension_manual_fact_contract(
          manual_extension_inputs(),
          input$extension_type
        )
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

      build_extension_player <- function(values,
                                         facts,
                                         assume_unknown_met = FALSE) {
        row <- selected_player_row()
        flags <- facts$flags

        if (isTRUE(assume_unknown_met) && length(facts$unresolved)) {
          flags[facts$unresolved] <- TRUE
        }

        extension_player_from_row(
          player_row = row,
          service_years = values$service_years,
          remaining_contract_years =
            as.integer(
              safe_num(
                values$remaining_years,
                0
              )
            ),
          is_first_round_pick =
            flags[["is_first_round_pick"]],
          rookie_option_years_exercised =
            flags[["rookie_options_exercised"]],
          timing_window_open =
            flags[["timing_window_open"]],
          designated_rookie_qualified =
            flags[["designated_rookie_qualified"]],
          designated_veteran_qualified =
            flags[["designated_veteran_qualified"]],
          original_team_requirement_met =
            flags[["original_team_requirement_met"]]
        )
      }

      extension_player <- shiny::reactive({
        values <- manual_extension_inputs()
        facts <- manual_fact_contract()

        shiny::req(isTRUE(facts$resolved))

        build_extension_player(values, facts)
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
        facts <- manual_fact_contract()
        values <- manual_extension_inputs()
        missing_fields <- c(
          if (is.na(values$service_years)) "service_years",
          facts$unresolved
        )

        # Unknown manual facts are treated as met only for this preflight. A
        # failure that survives that permissive assumption is definitive;
        # otherwise the unresolved facts retain the REVIEW state below.
        if (length(facts$unresolved)) {
          labels <- extension_review_field_labels()
          missing_labels <- unname(labels[missing_fields])

          preflight_player <- build_extension_player(
            values,
            facts,
            assume_unknown_met = TRUE
          )
          preflight <- extension_known_eligibility_failure(
            preflight_player,
            input$extension_type
          )
          if (is.null(preflight)) {
            preflight <- tryCatch(
              evaluate_extension_proposal(
                player = preflight_player,
                proposal = proposal()
              ),
              error = function(e) NULL
            )
          }

          if (extension_is_known_failure(preflight)) {
            failures <- unique(as.character(preflight$failures %||% character()))
            failures <- failures[!is.na(failures) & nzchar(failures)]
            failure_message <- as.character(
              preflight$message %||%
                preflight$executive_summary %||%
                "The proposal fails the modeled eligibility screen."
            )[[1L]]

            return(
              structure(
                list(
                  status = "FAIL",
                  passes_screen = FALSE,
                  requires_manual_review = TRUE,
                  reason_code = "MODELED_ELIGIBILITY_FAILURE",
                  missing_fields = missing_fields,
                  fact_states = facts$states,
                  review_reasons = missing_labels,
                  eligibility = preflight$eligibility,
                  failures = failures,
                  warnings = preflight$warnings %||% character(),
                  message = paste(
                    failure_message,
                    "Additional required source facts remain unresolved."
                  )
                ),
                class = c(
                  "tbi_extension_failure",
                  "tbi_extension_error"
                )
              )
            )
          }

          return(
            structure(
              list(
                status = "REVIEW",
                passes_screen = NA,
                requires_manual_review = TRUE,
                reason_code = if ("service_years" %in% missing_fields) {
                  "NBA_SERVICE_YEARS_UNKNOWN"
                } else {
                  "MANUAL_EXTENSION_FACTS_UNKNOWN"
                },
                missing_fields = missing_fields,
                fact_states = facts$states,
                review_reasons = missing_labels,
                message = paste(
                  "Review required before the modeled extension screen can run.",
                  "Verify the unresolved source facts listed below."
                )
              ),
              class = c(
                "tbi_extension_review",
                "tbi_extension_error"
              )
            )
          )
        }

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

      extension_analysis_pending <- shiny::reactive({
        extension_analysis_is_pending(extension_result())
      })

      output$extension_analysis_pending_flag <- shiny::renderText({
        if (extension_analysis_pending()) "pending" else "ready"
      })
      shiny::outputOptions(
        output,
        "extension_analysis_pending_flag",
        suspendWhenHidden = FALSE
      )

      output$financial_analysis_pending <- shiny::renderUI({
        if (!extension_analysis_pending()) return(NULL)
        extension_analysis_pending_ui(extension_result())
      })

      output$recommendation_analysis_pending <- shiny::renderUI({
        if (!extension_analysis_pending()) return(NULL)
        extension_analysis_pending_ui(extension_result())
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
        if (extension_analysis_pending()) return(NULL)
        
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
            class = "ext-bie-metrics",
            
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
            class = "ext-bie-summary",
            
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
                "Contract through ",
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

        if (extension_analysis_pending()) {
          return("")
        }
        
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
          return(if (extension_is_known_failure(result)) "FAIL" else "REVIEW")
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
          return(if (extension_is_known_failure(result)) "FAIL" else "REVIEW")
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
          decision_word <- if (extension_is_known_failure(result)) {
            "DO NOT ADVANCE"
          } else {
            "REVIEW REQUIRED"
          }

          return(
            shiny::tagList(
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
                    decision_word
                  ),

                  shiny::p(
                    result$message
                  )
                )
              ),
              if (!extension_is_known_failure(result)) {
                extension_review_reason_panel(result)
              }
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
          "REVIEW REQUIRED"
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
                extension_ui_status_label(result$eligibility$status)
              ),
              shiny::tags$small(
                extension_type_label(
                  result$extension_type
                )
              )
            )
          ),
          if (isTRUE(result$requires_manual_review) && isTRUE(result$passes_screen)) {
            extension_review_reason_panel(result, screen_passed = TRUE)
          }
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
            extension_ui_status_label(result$eligibility$status),
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

        if (extension_analysis_pending()) {
          return(NULL)
        }
        
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

        if (extension_analysis_pending()) {
          return(NULL)
        }
        
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
        
        shiny::tagList(
          status_row(
            "Starting salary",
            money(
              proposal()$starting_salary
            ),
            start_tone
          ),
          status_row(
            "Final-year salary",
            money(result$schedule_summary$final_salary)
          ),
          status_row(
            "Guarantee / option",
            text_or(proposal()$guarantee_structure, "UNKNOWN")
          ),
          status_row(
            "Total contract value",
            money(
              result$schedule_summary$total_value
            )
          ),
          
          status_row(
            "Average annual value",
            money(
              result$schedule_summary$average_annual_value
            )
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

        if (extension_analysis_pending()) {
          return(NULL)
        }
        
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

        if (extension_analysis_pending()) {
          return(NULL)
        }
        
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

        if (extension_analysis_pending()) {
          return(NULL)
        }
        
        if (
          inherits(
            result,
            "tbi_extension_error"
          )
        ) {
          recommendation_word <- if (extension_is_known_failure(result)) {
            "DO NOT ADVANCE"
          } else {
            "REVIEW REQUIRED"
          }

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
                  recommendation_word
                ),
                shiny::p(
                  result$message
                ),
                if (!extension_is_known_failure(result)) {
                  extension_review_reason_panel(result)
                }
              )
            )
          )
          }

          proposal_value <- proposal()
          player <- selected_player_row()
        
        label <- if (!isTRUE(result$passes_screen)) {
          "DO NOT ADVANCE"
        } else if (isTRUE(result$requires_manual_review)) {
          "REVIEW REQUIRED"
        } else {
          "ADVANCE"
        }

        eligibility <- extension_ui_status_label(result$eligibility$status)
        start_ok <- isTRUE(result$starting_salary_screen$requested_within_limit)
        raise_ok <- isTRUE(result$raise_screen$requested_within_limit)
        term_ok <- isTRUE(result$years_within_limit)
        room <- safe_num(result$starting_salary_screen$room_below_limit, 0)

        reasons <- if (length(result$failures)) {
          utils::head(unique(result$failures), 5L)
        } else {
          c(
            paste("Eligibility:", eligibility),
            paste(
              "Starting salary",
              money(proposal_value$starting_salary),
              "is within the screened",
              money(result$starting_salary_screen$maximum_starting_salary),
              "maximum."
            ),
            sprintf(
              "Annual raise %.1f%% is within the screened %.1f%% maximum.",
              proposal_value$raise_percent * 100,
              safe_num(result$raise_screen$maximum_raise_percent, 0) * 100
            ),
            paste(
              result$requested_years,
              "years produces",
              money(result$schedule_summary$total_value),
              "in modeled salary."
            ),
            "Manual contract-language and timing verification remains required."
          )
        }

        blocker <- if (length(result$failures)) {
          result$failures[[1L]]
        } else {
          "No modeled blocking rule; required verification remains open."
        }

        next_action <- if (!start_ok) {
          paste("Reduce starting salary to", money(result$starting_salary_screen$maximum_starting_salary), "or below.")
        } else if (!raise_ok) {
          sprintf("Reduce the annual raise to %.1f%% or below.", safe_num(result$raise_screen$maximum_raise_percent, 0) * 100)
        } else if (!term_ok) {
          paste("Reduce the extension term to", result$maximum_years, "years or fewer.")
        } else if (!isTRUE(result$eligibility$eligible)) {
          "Resolve the identified eligibility facts before advancing."
        } else if (isTRUE(result$requires_manual_review)) {
          "Complete the required timing, option, guarantee, and contract-language verification."
        } else {
          "Advance the proposal to contract review."
        }

        primary_risk <- if (length(result$failures)) {
          result$failures[[1L]]
        } else if (length(result$warnings)) {
          result$warnings[[1L]]
        } else {
          "No modeled financial-limit failure is currently identified."
        }

        primary_opportunity <- if (room > 0) {
          paste(money(room), "remains below the screened maximum starting salary.")
        } else {
          "No additional starting-salary room is supported by the current screen."
        }

        shiny::div(
          class = "ext-recommendation-brief",
          shiny::div(
            class = "ext-recommendation-decision",
            shiny::span(class = "tbi-v2-snapshot-label", "DECISION"),
            shiny::strong(
              class = paste(
                "tbi-v2-decision-word",
                if (!isTRUE(result$passes_screen)) {
                  "ext-status-fail"
                } else if (isTRUE(result$requires_manual_review)) {
                  "ext-status-review"
                } else {
                  "ext-status-pass"
                }
              ),
              label
            ),
            shiny::p(result$executive_summary),
            if (isTRUE(result$requires_manual_review) && isTRUE(result$passes_screen)) {
              extension_review_reason_panel(result, screen_passed = TRUE)
            },
            shiny::h4("Next action"),
            shiny::p(next_action)
          ),
          shiny::div(
            class = "ext-recommendation-column",
            shiny::h4("Why"),
            shiny::tags$ul(
              class = "ext-recommendation-list",
              lapply(reasons, shiny::tags$li)
            ),
            shiny::h4("CBA result"),
            shiny::p(blocker)
          ),
          shiny::div(
            class = "ext-recommendation-column",
            shiny::h4("Financial effect"),
            shiny::p(paste(
              money(proposal_value$starting_salary), "starting salary;",
              money(result$schedule_summary$total_value), "total;",
              text_or(proposal_value$guarantee_structure, "guarantee structure unknown")
            )),
            shiny::h4("Player / team context"),
            shiny::p(paste(
              text_or(player$player_name), "—",
              extension_type_label(result$extension_type), "extension; current salary",
              money(player$current_salary)
            )),
            shiny::h4("Primary risk"),
            shiny::p(primary_risk),
            shiny::h4("Primary opportunity"),
            shiny::p(primary_opportunity)
          )
        )
      })
    }
  )
}
