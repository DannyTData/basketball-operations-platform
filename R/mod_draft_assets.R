# ------------------------------------------------------------
# Module: Draft Intelligence
# Version 2 Executive Draft Capital Workspace
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

draft_ledger_text <- function(x, fallback = "") {
  values <- x %||% ""
  vapply(seq_along(values), function(i) draft_text(values[[i]], fallback), character(1))
}

draft_verification_reason <- function(row) {
  verified <- identical(draft_ledger_text(row$verification_status), "Verified") &&
    !isTRUE(row$requires_manual_review)

  if (verified) {
    return("Verified")
  }

  source_values <- c(row$source_name, row$source_url, row$source_date)
  if (any(!nzchar(draft_ledger_text(source_values)))) {
    return("Source provenance incomplete")
  }

  control <- draft_ledger_text(row$control_type)
  if (grepl("Swap", control, fixed = TRUE) &&
      !nzchar(draft_ledger_text(row$transaction_reference))) {
    return("Swap terms incomplete")
  }

  if (!nzchar(draft_ledger_text(row$protection_text))) {
    return("Protection language incomplete")
  }

  condition_count <- draft_integer(row$condition_count, 0L)
  conveyance_values <- c(row$conveyance_start_year, row$conveyance_end_year)
  if (is.finite(condition_count) && condition_count > 0L &&
      all(!nzchar(draft_ledger_text(conveyance_values)))) {
    return("Conveyance conditions incomplete")
  }

  "Loaded source provenance remains unverified"
}

prepare_draft_asset_ledger <- function(assets, valued) {
  if (!is.data.frame(assets) || !nrow(assets)) {
    return(data.frame())
  }

  ledger <- assets
  valued <- if (is.data.frame(valued)) valued else data.frame()
  valued_match <- if (nrow(valued) && "draft_asset_id" %in% names(valued)) {
    match(ledger$draft_asset_id, valued$draft_asset_id)
  } else {
    rep(NA_integer_, nrow(ledger))
  }

  for (field in c("expected_slot", "blended_value_score", "value_tier")) {
    ledger[[field]] <- if (field %in% names(valued)) valued[[field]][valued_match] else NA
  }

  asset_review <- if ("requires_manual_review" %in% names(ledger)) {
    as.logical(ledger$requires_manual_review)
  } else {
    rep(FALSE, nrow(ledger))
  }
  valued_review <- if ("requires_manual_review" %in% names(valued)) {
    as.logical(valued$requires_manual_review[valued_match])
  } else {
    rep(FALSE, nrow(ledger))
  }
  ledger$requires_verification <- ifelse(is.na(asset_review), FALSE, asset_review) |
    ifelse(is.na(valued_review), FALSE, valued_review) |
    draft_ledger_text(ledger$verification_status) != "Verified"

  ledger$verification_state <- ifelse(
    ledger$requires_verification,
    "REQUIRES VERIFICATION",
    "VERIFIED"
  )
  ledger$verification_reason <- vapply(
    seq_len(nrow(ledger)),
    function(i) draft_verification_reason(as.list(ledger[i, , drop = FALSE])),
    character(1)
  )
  ledger$protection_filter <- draft_ledger_text(ledger$protection_type, "Terms incomplete")
  ledger
}

filter_draft_asset_ledger <- function(ledger, filters = list()) {
  if (!is.data.frame(ledger) || !nrow(ledger)) {
    return(ledger)
  }

  value <- function(name) draft_ledger_text(filters[[name]] %||% "")[[1]]
  keep <- rep(TRUE, nrow(ledger))
  exact_filters <- c(
    year = "draft_year",
    round = "round",
    control = "control_type",
    original_team = "original_team",
    protection = "protection_filter",
    verification = "verification_state"
  )

  for (name in names(exact_filters)) {
    selected <- value(name)
    if (nzchar(selected)) {
      keep <- keep & draft_ledger_text(ledger[[exact_filters[[name]]]]) == selected
    }
  }

  search <- tolower(value("search"))
  if (nzchar(search)) {
    searchable <- do.call(
      paste,
      c(
        lapply(
          c("draft_year", "round", "control_type", "original_team", "protection_text", "verification_reason", "value_tier"),
          function(field) draft_ledger_text(ledger[[field]])
        ),
        sep = " "
      )
    )
    keep <- keep & grepl(search, tolower(searchable), fixed = TRUE)
  }

  ledger[keep, , drop = FALSE]
}

group_draft_asset_ledger <- function(ledger) {
  if (!is.data.frame(ledger) || !nrow(ledger)) {
    return(list())
  }
  years <- sort(unique(as.integer(ledger$draft_year)), method = "radix")
  stats::setNames(
    lapply(years, function(year) ledger[ledger$draft_year == year, , drop = FALSE]),
    as.character(years)
  )
}

draft_recommendation_facts <- function(ledger, summary) {
  value <- draft_number(summary$net_portfolio_value, 0)
  posture <- if (value >= 150) {
    "PRESERVE + DEPLOY SELECTIVELY"
  } else if (value >= 75) {
    "MAINTAIN FLEXIBILITY"
  } else if (value >= 20) {
    "PROTECT CORE PICKS"
  } else {
    "ACQUIRE DRAFT CAPITAL"
  }

  modeled_rows <- ledger[is.finite(suppressWarnings(as.numeric(ledger$blended_value_score))), , drop = FALSE]
  year_value <- if (nrow(modeled_rows)) {
    stats::aggregate(blended_value_score ~ draft_year, modeled_rows, sum)
  } else {
    data.frame(draft_year = integer(), blended_value_score = numeric())
  }
  strongest <- if (nrow(year_value)) year_value$draft_year[[which.max(year_value$blended_value_score)]] else NA_integer_
  constrained <- if (nrow(year_value)) year_value$draft_year[[which.min(year_value$blended_value_score)]] else NA_integer_
  review_count <- sum(ledger$requires_verification %||% logical(), na.rm = TRUE)
  obligation_rows <- ledger[ledger$control_type %in% c("Outgoing", "Swap Obligation"), , drop = FALSE]
  modeled_obligations <- obligation_rows[
    is.finite(suppressWarnings(as.numeric(obligation_rows$blended_value_score))),
    ,
    drop = FALSE
  ]
  biggest_obligation <- if (nrow(modeled_obligations)) {
    scores <- suppressWarnings(as.numeric(modeled_obligations$blended_value_score))
    obligation <- modeled_obligations[which.min(scores), , drop = FALSE]
    paste(
      obligation$draft_year,
      obligation$round,
      obligation$control_type,
      "record from",
      obligation$original_team,
      "is the largest loaded obligation by modeled value."
    )
  } else if (nrow(obligation_rows)) {
    "The largest loaded obligation is UNKNOWN because modeled value is unavailable."
  } else {
    "No outgoing obligation is loaded in the planning window."
  }

  list(
    posture = posture,
    strongest_year = if (is.na(strongest)) "No supported year" else paste(strongest, "has the strongest modeled control value."),
    constrained_year = if (is.na(constrained)) "No supported year" else paste(constrained, "is the most constrained modeled year."),
    verification = paste(
      review_count,
      if (review_count == 1L) "record requires" else "records require",
      "verification before transaction use."
    ),
    biggest_obligation = biggest_obligation,
    next_action = if (is.na(strongest)) {
      "Resolve verification gaps before assigning draft capital to a transaction."
    } else if (value >= 75) {
      paste("Preserve", strongest, "control unless a supported high-impact transaction justifies deployment.")
    } else {
      paste("Protect the strongest supported control in", strongest, "and avoid increasing exposure in", constrained, ".")
    }
  )
}

draft_error_recommendation <- function(message = NULL) {
  value <- as.character(message %||% character())
  detail <- if (length(value) && !is.na(value[[1]])) trimws(value[[1]]) else ""
  if (!nzchar(detail)) detail <- "Draft evidence is unavailable."

  list(
    posture = "REVIEW / UNKNOWN",
    strongest_year = "UNKNOWN — the Draft engine did not return supported evidence.",
    constrained_year = "UNKNOWN — the Draft engine did not return supported evidence.",
    verification = "UNKNOWN — source verification could not be evaluated.",
    biggest_obligation = "UNKNOWN — obligations could not be evaluated.",
    next_action = paste(
      "Review only. Restore the Draft engine/query and re-run before taking draft-capital action.",
      detail
    )
  )
}

draft_finding <- function(category, fact, impact, consequence) {
  list(
    category = category,
    fact = fact,
    impact = impact,
    consequence = consequence
  )
}

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

        .tbi-v2-draft-page :is(.draft-v2-overview-balance-grid,.draft-v2-portfolio-balance-grid) {
          display:grid;
          grid-template-columns:repeat(3,minmax(0,1fr)) !important;
          gap:16px;
          align-items:start;
        }

        .draft-v2-overview-balance-grid > section,
        .draft-v2-portfolio-balance-grid > section {
          min-width:0;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-card {
          padding:12px 16px 0;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-main {
          min-height:0;
          padding:16px 8px 12px;
          gap:12px;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-symbol {
          width:44px;
          height:44px;
          flex-basis:44px;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-symbol svg {
          width:20px !important;
          height:20px !important;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-word {
          margin-bottom:8px;
          font-size:1.2rem;
          letter-spacing:-.025em;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-copy p {
          font-size:.70rem;
          line-height:1.45;
        }

        .draft-v2-overview-balance-grid .tbi-v2-decision-metric {
          min-height:0;
          padding:12px 16px;
        }

        .draft-v2-overview-balance-grid .tbi-v2-score-row {
          min-height:52px;
          padding:8px 12px;
          grid-template-columns:minmax(0,1fr) auto auto;
          gap:6px 8px;
        }

        .draft-v2-overview-balance-grid .tbi-v2-score-name {
          grid-column:1;
          grid-row:1;
        }

        .draft-v2-overview-balance-grid .tbi-v2-score-meter {
          grid-column:1 / -1;
          grid-row:2;
        }

        .draft-v2-overview-balance-grid .tbi-v2-score-number {
          grid-column:2;
          grid-row:1;
        }

        .draft-v2-overview-balance-grid .tbi-v2-score-rating {
          grid-column:3;
          grid-row:1;
        }

        .draft-v2-control-summary {
          padding:8px 16px 12px;
        }

        .draft-v2-strength-summary {
          padding:16px;
          display:grid;
          grid-template-columns:auto minmax(0,1fr);
          gap:8px 16px;
          align-items:end;
        }

        .draft-v2-strength-net span,
        .draft-v2-strength-grade span,
        .draft-v2-strength-metric span {
          display:block;
          color:#718198;
          font-size:.58rem;
          font-weight:800;
        }

        .draft-v2-strength-net strong {
          display:block;
          margin-top:4px;
          color:#61a8ff;
          font-size:1.35rem;
          line-height:1;
        }

        .draft-v2-strength-grade strong {
          display:block;
          margin-top:4px;
          color:#f6f8fb;
          font-size:1rem;
        }

        .draft-v2-summary {
          grid-column:1 / -1;
          max-width:68ch;
          margin:4px 0 0;
          color:#9aa8ba;
          font-size:.68rem;
          line-height:1.45;
        }

        .draft-v2-strength-metrics {
          grid-column:1 / -1;
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:8px;
          margin-top:4px;
        }

        .draft-v2-strength-metric {
          padding:8px 0;
          border-top:1px solid rgba(148,163,184,.09);
        }

        .draft-v2-strength-metric strong {
          display:block;
          margin-top:3px;
          color:#eef3f8;
          font-size:.86rem;
        }

        .draft-v2-year-grid {
          padding:8px 16px 12px;
          display:grid;
          grid-template-columns:1fr;
          gap:0;
        }

        .draft-v2-year-card {
          min-height:0;
          padding:10px 0;
          display:grid;
          grid-template-columns:48px minmax(0,1fr) minmax(0,1fr) auto;
          gap:8px;
          align-items:center;
          border-top:1px solid rgba(148,163,184,.09);
        }

        .draft-v2-year-card:first-child {
          border-top:0;
        }

        .draft-v2-year-card strong {
          color:#f3f6fa;
          font-size:.80rem;
        }

        .draft-v2-year-card span {
          color:#8796aa;
          font-size:.65rem;
          line-height:1.35;
        }

        .draft-v2-year-value {
          color:#61a8ff !important;
          font-weight:800;
          text-align:right;
        }

        .draft-v2-signal-row {
          min-height:36px;
          padding:8px 0;
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:12px;
          border-bottom:1px solid rgba(148,163,184,.08);
          color:#8796aa;
          font-size:.66rem;
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
          min-width:0;
          padding:0 14px 14px;
        }

        .draft-v2-filter-bar {
          display:grid;
          grid-template-columns:minmax(190px,1.35fr) repeat(6,minmax(112px,1fr)) auto;
          gap:8px;
          align-items:end;
          padding:12px 14px;
          border-bottom:1px solid rgba(148,163,184,.10);
        }

        .draft-v2-filter-bar .form-group {
          min-width:0;
          margin:0;
        }

        .draft-v2-filter-bar label {
          margin-bottom:4px;
          color:#8494a8;
          font-size:.62rem;
          font-weight:800;
        }

        .draft-v2-filter-bar :is(.form-control,.selectize-input) {
          min-height:34px;
          font-size:.70rem;
        }

        .draft-v2-clear-filters {
          min-height:34px;
          padding:6px 11px;
          border:1px solid rgba(148,163,184,.22);
          border-radius:7px;
          background:rgba(51,65,85,.22);
          color:#cbd5e1;
          font-size:.68rem;
          font-weight:750;
          white-space:nowrap;
        }

        .draft-v2-ledger-summary {
          padding:10px 0;
          color:#91a0b3;
          font-size:.72rem;
        }

        .draft-v2-year-group {
          border-top:1px solid rgba(148,163,184,.12);
        }

        .draft-v2-year-group > summary {
          display:flex;
          justify-content:space-between;
          gap:12px;
          padding:11px 2px;
          color:#edf3fa;
          font-size:.78rem;
          font-weight:800;
          cursor:pointer;
        }

        .draft-v2-year-group > summary span {
          color:#8797aa;
          font-size:.68rem;
          font-weight:650;
          text-align:right;
        }

        .draft-v2-asset-row {
          display:grid;
          grid-template-columns:minmax(90px,.55fr) minmax(110px,.7fr) minmax(145px,1fr) minmax(145px,1.1fr);
          gap:8px 14px;
          padding:10px 8px;
          border-top:1px solid rgba(148,163,184,.07);
        }

        .draft-v2-asset-row > div {
          min-width:0;
        }

        .draft-v2-asset-label {
          display:block;
          margin-bottom:2px;
          color:#738399;
          font-size:.58rem;
          font-weight:800;
          text-transform:uppercase;
        }

        .draft-v2-asset-value {
          display:block;
          color:#c8d3df;
          font-size:.72rem;
          line-height:1.4;
          overflow-wrap:anywhere;
        }

        .draft-v2-asset-detail {
          grid-column:1 / -1;
        }

        .draft-v2-asset-detail summary {
          color:#8ca4c1;
          font-size:.66rem;
          cursor:pointer;
        }

        .draft-v2-asset-detail p {
          max-width:90ch;
          margin:7px 0 0;
          color:#aebccb;
          font-size:.70rem;
          line-height:1.5;
        }

        .draft-v2-empty-ledger {
          padding:20px 4px;
          color:#aab7c7;
          font-size:.76rem;
        }

        .draft-v2-finding {
          display:grid;
          grid-template-columns:minmax(112px,.34fr) minmax(0,1fr);
          gap:6px 12px;
          padding:10px 0;
          border-bottom:1px solid rgba(148,163,184,.09);
        }

        .draft-v2-finding:last-child {
          border-bottom:0;
        }

        .draft-v2-finding-category {
          grid-row:1 / span 3;
          color:#91a4bc;
          font-size:.64rem;
          font-weight:800;
        }

        .draft-v2-finding p {
          margin:0;
          color:#aebac8;
          font-size:.70rem;
          line-height:1.42;
        }

        .draft-v2-finding p strong {
          color:#77879b;
          font-size:.59rem;
          text-transform:uppercase;
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

        @media(max-width:1200px) {
          .tbi-v2-draft-page :is(.draft-v2-overview-balance-grid,.draft-v2-portfolio-balance-grid) {
            grid-template-columns:repeat(2,minmax(0,1fr)) !important;
          }

          .draft-v2-overview-balance-grid > :last-child,
          .draft-v2-portfolio-balance-grid > :last-child {
            grid-column:1 / -1;
          }

          .draft-v2-filter-bar {
            grid-template-columns:repeat(3,minmax(0,1fr));
          }
        }

        @media(max-width:900px) {
          .tbi-v2-draft-page :is(.draft-v2-overview-balance-grid,.draft-v2-portfolio-balance-grid) {
            grid-template-columns:1fr !important;
          }

          .draft-v2-overview-balance-grid > :last-child,
          .draft-v2-portfolio-balance-grid > :last-child {
            grid-column:auto;
          }
        }

        @media(max-width:680px) {
          .draft-v2-rec-grid {
            grid-template-columns:1fr;
          }

          .draft-v2-year-card {
            grid-template-columns:44px minmax(0,1fr) auto;
          }

          .draft-v2-year-obligations {
            grid-column:2;
          }

          .draft-v2-year-value {
            grid-column:3;
            grid-row:1 / span 2;
          }

          .draft-v2-filter-bar,
          .draft-v2-asset-row,
          .draft-v2-finding {
            grid-template-columns:1fr;
          }

          .draft-v2-asset-detail {
            grid-column:auto;
          }

          .draft-v2-finding-category {
            grid-row:auto;
          }

          .draft-v2-clear-filters {
            width:100%;
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
      class = "tbi-v2-exec-main-grid draft-v2-overview-balance-grid",
      
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
      ),

      shiny::tags$section(
        class = "tbi-v2-scorecard-panel draft-v2-control-panel",

        shiny::div(
          class = "tbi-v2-scorecard-header",
          shiny::div(
            class = "tbi-v2-section-title",
            shiny::span(
              class = "tbi-v2-section-icon",
              bsicons::bs_icon("shield-check")
            ),
            shiny::span("CONTROL / VERIFICATION")
          )
        ),

        shiny::uiOutput(
          ns("overview_control_summary")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Portfolio intelligence
    # --------------------------------------------------------
    
    shiny::div(
      class = "draft-v2-portfolio-grid draft-v2-portfolio-balance-grid",
      
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
              "CONTROL & OBLIGATIONS"
            ),
            shiny::h3("Portfolio commitments")
          ),

          shiny::span(
            class = "tbi-v2-context-tag",
            "CURRENT FACTS"
          )
        ),

        shiny::uiOutput(
          ns("portfolio_control_summary")
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
          "Records Requiring Verification"
        )
      ),

      shiny::div(
        class = "draft-v2-filter-bar",
        shiny::textInput(ns("draft_search"), "Search assets", placeholder = "Team, terms, tier…"),
        shiny::selectInput(ns("draft_year_filter"), "Year", choices = c("All years" = "")),
        shiny::selectInput(ns("draft_round_filter"), "Round", choices = c("All rounds" = "")),
        shiny::selectInput(ns("draft_control_filter"), "Control", choices = c("All control types" = "")),
        shiny::selectInput(ns("draft_original_team_filter"), "Original team", choices = c("All original teams" = "")),
        shiny::selectInput(ns("draft_protection_filter"), "Protection", choices = c("All protection types" = "")),
        shiny::selectInput(ns("draft_verification_filter"), "Verification", choices = c("All records" = "")),
        shiny::actionButton(
          ns("clear_draft_filters"),
          "Clear filters",
          class = "draft-v2-clear-filters"
        )
      ),
      
      shiny::div(
        class = "draft-v2-table-wrap",
        shiny::uiOutput(
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

      control_summary_ui <- function(assets, summary) {
        shiny::div(
          class = "draft-v2-control-summary",
          signal_row("Controlled first-round assets", assets$controlled_first_round),
          signal_row("Controlled second-round assets", assets$controlled_second_round),
          signal_row("Swap rights", assets$swap_rights),
          signal_row("Outgoing obligations", assets$outgoing_obligations),
          signal_row("Records Requiring Source Verification", summary$review_required)
        )
      }

      finding_row <- function(category, fact, impact, consequence) {
        field <- function(label, value) {
          shiny::p(shiny::strong(paste0(label, " · ")), value)
        }
        shiny::div(
          class = "draft-v2-finding",
          shiny::div(class = "draft-v2-finding-category", category),
          field("Fact", fact),
          field("Impact", impact),
          field("Decision consequence", consequence)
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

      draft_ledger <- shiny::reactive({
        prepare_draft_asset_ledger(draft_assets(), valued_assets())
      })

      valid_filter <- function(selected, allowed) {
        selected <- draft_ledger_text(selected %||% "")[[1]]
        if (nzchar(selected) && selected %in% draft_ledger_text(allowed)) selected else ""
      }

      active_draft_filters <- shiny::reactive({
        ledger <- draft_ledger()
        list(
          search = draft_ledger_text(input$draft_search %||% "")[[1]],
          year = valid_filter(input$draft_year_filter, ledger$draft_year),
          round = valid_filter(input$draft_round_filter, ledger$round),
          control = valid_filter(input$draft_control_filter, ledger$control_type),
          original_team = valid_filter(input$draft_original_team_filter, ledger$original_team),
          protection = valid_filter(input$draft_protection_filter, ledger$protection_filter),
          verification = valid_filter(input$draft_verification_filter, ledger$verification_state)
        )
      })

      filter_team <- shiny::reactiveVal(NULL)

      filtered_draft_assets <- shiny::reactive({
        if (!identical(filter_team(), selected_team())) {
          return(draft_ledger())
        }
        filter_draft_asset_ledger(draft_ledger(), active_draft_filters())
      })

      shiny::observeEvent(
        list(
          input$draft_search,
          input$draft_year_filter,
          input$draft_round_filter,
          input$draft_control_filter,
          input$draft_original_team_filter,
          input$draft_protection_filter,
          input$draft_verification_filter
        ),
        filter_team(selected_team()),
        ignoreInit = TRUE
      )

      update_draft_filter_choices <- function() {
        ledger <- draft_ledger()
        choices <- function(label, values) {
          values <- sort(unique(draft_ledger_text(values)), method = "radix")
          values <- values[nzchar(values)]
          c(stats::setNames("", label), stats::setNames(values, values))
        }
        shiny::updateSelectInput(session, "draft_year_filter", choices = choices("All years", ledger$draft_year))
        shiny::updateSelectInput(session, "draft_round_filter", choices = choices("All rounds", ledger$round))
        shiny::updateSelectInput(session, "draft_control_filter", choices = choices("All control types", ledger$control_type))
        shiny::updateSelectInput(session, "draft_original_team_filter", choices = choices("All original teams", ledger$original_team))
        shiny::updateSelectInput(session, "draft_protection_filter", choices = choices("All protection types", ledger$protection_filter))
        shiny::updateSelectInput(session, "draft_verification_filter", choices = choices("All records", ledger$verification_state))
      }

      reset_draft_filters <- function() {
        filter_ids <- c(
          "draft_search", "draft_year_filter", "draft_round_filter", "draft_control_filter",
          "draft_original_team_filter", "draft_protection_filter", "draft_verification_filter"
        )
        lapply(filter_ids, function(id) shiny::freezeReactiveValue(input, id))
        shiny::updateTextInput(session, "draft_search", value = "")
        for (id in setdiff(filter_ids, "draft_search")) {
          shiny::updateSelectInput(session, id, selected = "")
        }
      }

      shiny::observeEvent(draft_ledger(), update_draft_filter_choices(), ignoreInit = FALSE)
      shiny::observeEvent(selected_team(), {
        filter_team(NULL)
        reset_draft_filters()
      }, ignoreInit = TRUE)
      shiny::observeEvent(input$clear_draft_filters, {
        filter_team(NULL)
        reset_draft_filters()
      }, ignoreInit = TRUE)
      
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
                  "REVIEW / UNKNOWN"
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
            "Obligation Exposure",
            "Outgoing pick and swap burden",
            "arrow-left-right",
            obligation_score
          )
        )
      })

      control_summary <- shiny::reactive({
        control_summary_ui(asset_summary(), draft_summary())
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
        
        strength_metric <- function(label, value) {
          shiny::div(
            class = "draft-v2-strength-metric",
            shiny::span(label),
            shiny::strong(value)
          )
        }

        shiny::div(
          class = "draft-v2-strength-summary",
          shiny::div(
            class = "draft-v2-strength-net",
            shiny::span("NET VALUE"),
            shiny::strong(sprintf("%.0f", net_value))
          ),
          shiny::div(
            class = "draft-v2-strength-grade",
            shiny::span("PORTFOLIO GRADE"),
            shiny::strong(summary$portfolio_grade)
          ),
          shiny::p(
            class = "draft-v2-summary",
            summary$executive_summary
          ),
          shiny::div(
            class = "draft-v2-strength-metrics",
            strength_metric(
              "Gross asset value",
              sprintf("%.1f", safe_num(summary$gross_asset_value, 0))
            ),
            strength_metric(
              "Obligation value",
              sprintf("%.1f", safe_num(summary$gross_obligation_value, 0))
            ),
            strength_metric(
              "Premium assets",
              as.character(safe_num(summary$premium_assets, 0))
            ),
            strength_metric(
              "Strong assets",
              as.character(safe_num(summary$strong_assets, 0))
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
                class = "draft-v2-year-obligations",
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
            " require verification before transaction use."
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
        
        risks <- list()
        
        if (
          safe_num(
            assets$outgoing_obligations,
            0
          ) > 0
        ) {
          risks <- append(risks, list(draft_finding(
            "CONTROL RISKS",
            paste0(
              assets$outgoing_obligations, " outgoing draft obligation",
              if (assets$outgoing_obligations == 1) "" else "s",
              " reduce future flexibility."
            ),
            "Those obligations reduce clean control in the affected years.",
            "Do not treat the affected draft capital as fully deployable."
          )))
        }
        
        if (
          safe_num(
            assets$swap_obligations,
            0
          ) > 0
        ) {
          risks <- append(risks, list(draft_finding(
            "CONTROL RISKS",
            paste0(
              assets$swap_obligations, " swap obligation",
              if (assets$swap_obligations == 1) "" else "s",
              " create future downside exposure."
            ),
            "Those obligations reduce clean control in the affected years.",
            "Do not treat the affected draft capital as fully deployable."
          )))
        }
        
        if (
          safe_num(
            summary$review_required,
            0
          ) > 0
        ) {
          risks <- append(risks, list(draft_finding(
            "PROTECTION / CONVEYANCE RISKS",
            paste0(
              summary$review_required, " valued record",
              if (summary$review_required == 1) "" else "s",
              " require protection or verification review before transaction use."
            ),
            "The record cannot be treated as transaction-ready until the missing terms are verified.",
            "Verify the named evidence before routing the asset."
          )))
        }
        
        if (
          safe_num(
            summary$net_portfolio_value,
            0
          ) < 20
        ) {
          risks <- append(risks, list(draft_finding(
            "TRADE FLEXIBILITY",
            "Estimated draft-capital value is limited, reducing transaction downside protection.",
            "The loaded portfolio provides less downside protection in transaction planning.",
            "Do not treat the affected draft capital as fully deployable."
          )))
        }
        
        if (!length(risks)) {
          risks <- list(draft_finding(
            "TRADE FLEXIBILITY",
            "No major structural draft-capital risk is identified by the loaded portfolio.",
            "The loaded portfolio does not expose a controlling structural risk.",
            "Preserve verification discipline before deploying draft capital."
          ))
        }
        
        shiny::tagList(
          lapply(
            risks,
            function(risk) finding_row(risk$category, risk$fact, risk$impact, risk$consequence)
          )
        )
      })
      
      # ------------------------------------------------------
      # Opportunities
      # ------------------------------------------------------
      
      output$draft_opportunities <- shiny::renderUI({
        result <- draft_value_result()

        if (inherits(result, "tbi_draft_error")) {
          return(finding_row(
            "REVIEW / UNKNOWN",
            "Draft opportunity evidence is unavailable.",
            result$message,
            "Restore the Draft engine/query and re-run before taking draft-capital action."
          ))
        }

        assets <- asset_summary()
        summary <- draft_summary()
        
        opportunities <- list()
        
        if (
          safe_num(
            summary$premium_assets,
            0
          ) > 0
        ) {
          opportunities <- append(opportunities, list(draft_finding(
            "TRADE FLEXIBILITY",
            paste0(
              summary$premium_assets, " premium asset",
              if (summary$premium_assets == 1) "" else "s",
              " can anchor high-level transaction packages."
            ),
            "The supported inventory can preserve options across transaction structures.",
            "Preserve the strongest controlled years unless the return justifies deployment."
          )))
        }
        
        if (
          safe_num(
            assets$swap_rights,
            0
          ) > 0
        ) {
          opportunities <- append(opportunities, list(draft_finding(
            "TRADE FLEXIBILITY",
            paste0(
              assets$swap_rights, " swap right",
              if (assets$swap_rights == 1) "" else "s",
              " preserve upside without requiring full pick ownership."
            ),
            "The supported inventory can preserve options across transaction structures.",
            "Preserve the strongest controlled years unless the return justifies deployment."
          )))
        }
        
        if (
          safe_num(
            assets$controlled_first_round,
            0
          ) >= 4
        ) {
          opportunities <- append(opportunities, list(draft_finding(
            "ACQUISITION OPPORTUNITIES",
            "First-round inventory provides meaningful flexibility for consolidation or star-level acquisition scenarios.",
            "Additional controlled assets improve future transaction coverage.",
            "Prioritize controllable assets rather than adding conditional exposure."
          )))
        }
        
        if (
          safe_num(
            summary$net_portfolio_value,
            0
          ) >= 75
        ) {
          opportunities <- append(opportunities, list(draft_finding(
            "TRADE FLEXIBILITY",
            "The modeled portfolio provides useful transaction currency while preserving future optionality.",
            "The supported inventory can preserve options across transaction structures.",
            "Preserve the strongest controlled years unless the return justifies deployment."
          )))
        }
        
        if (!length(opportunities)) {
          opportunities <- list(draft_finding(
            "ACQUISITION OPPORTUNITIES",
            "Prioritize adding or preserving controllable future draft assets before aggressive consolidation.",
            "Additional controlled assets would improve future transaction coverage.",
            "Prioritize controllable assets rather than adding conditional exposure."
          ))
        }
        
        shiny::tagList(
          lapply(
            opportunities,
            function(opportunity) finding_row(
              opportunity$category,
              opportunity$fact,
              opportunity$impact,
              opportunity$consequence
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Asset ledger
      # ------------------------------------------------------
      
      output$draft_asset_table <- shiny::renderUI({
        ledger <- filtered_draft_assets()
        total <- nrow(draft_ledger())

        if (!nrow(ledger)) {
          return(shiny::div(
            class = "draft-v2-empty-ledger",
            paste("No draft assets match the active filters for", selected_team(), ".")
          ))
        }

        groups <- group_draft_asset_ledger(ledger)
        year_ui <- Map(
          function(year, rows, index) {
            firsts <- sum(rows$round == "First", na.rm = TRUE)
            seconds <- sum(rows$round == "Second", na.rm = TRUE)
            obligations <- sum(rows$control_type %in% c("Outgoing", "Swap Obligation"), na.rm = TRUE)
            review <- sum(rows$requires_verification, na.rm = TRUE)
            count_label <- function(value, singular) {
              paste(value, paste0(singular, if (value == 1L) "" else "s"))
            }

            asset_rows <- lapply(seq_len(nrow(rows)), function(i) {
              row <- rows[i, , drop = FALSE]
              protection <- draft_ledger_text(row$protection_text, "Protection language not specified")[[1]]
              score <- suppressWarnings(as.numeric(row$blended_value_score))
              expected_slot <- draft_integer(row$expected_slot, NA_integer_)
              value <- if (is.finite(score)) {
                sprintf("%.1f · %s", score, draft_ledger_text(row$value_tier, "Untiered")[[1]])
              } else {
                "UNKNOWN"
              }

              shiny::div(
                class = "draft-v2-asset-row",
                shiny::div(shiny::span(class = "draft-v2-asset-label", "Round"), shiny::span(class = "draft-v2-asset-value", row$round)),
                shiny::div(shiny::span(class = "draft-v2-asset-label", "Control"), shiny::span(class = "draft-v2-asset-value", row$control_type)),
                shiny::div(shiny::span(class = "draft-v2-asset-label", "Original team"), shiny::span(class = "draft-v2-asset-value", row$original_team)),
                shiny::div(shiny::span(class = "draft-v2-asset-label", "Verification"), shiny::span(class = "draft-v2-asset-value", row$verification_reason)),
                shiny::tags$details(
                  class = "draft-v2-asset-detail",
                  shiny::tags$summary("Protection, provenance & modeled value"),
                  shiny::p(protection),
                  shiny::p(
                    paste0(
                      "Source: ", draft_ledger_text(row$source_name, "UNKNOWN")[[1]],
                      " · Verification: ", row$verification_state,
                      " · Expected slot: ", if (is.na(expected_slot)) "UNKNOWN" else expected_slot,
                      " · Value: ", value
                    )
                  )
                )
              )
            })

            shiny::tags$details(
              class = "draft-v2-year-group",
              open = if (index <= 2L) NA else NULL,
              shiny::tags$summary(
                shiny::strong(year),
                shiny::span(
                  paste0(
                    count_label(nrow(rows), "record"), " · ",
                    count_label(firsts, "first"), " · ",
                    count_label(seconds, "second"), " · ",
                    count_label(obligations, "obligation"), " · ",
                    review, if (review == 1L) " requires" else " require", " verification"
                  )
                )
              ),
              asset_rows
            )
          },
          names(groups),
          groups,
          seq_along(groups)
        )

        shiny::tagList(
          shiny::div(
            class = "draft-v2-ledger-summary",
            paste0("Showing ", nrow(ledger), " of ", total, " records across ", length(groups), " year groups.")
          ),
          year_ui
        )
      })

      output$overview_control_summary <- shiny::renderUI({
        control_summary()
      })

      output$portfolio_control_summary <- shiny::renderUI({
        control_summary()
      })
      
      # ------------------------------------------------------
      # Recommendation
      # ------------------------------------------------------
      
      output$draft_recommendation <- shiny::renderUI({
        result <- draft_value_result()
        summary <- draft_summary()
        recommendation <- if (inherits(result, "tbi_draft_error")) {
          draft_error_recommendation(result$message)
        } else {
          draft_recommendation_facts(draft_ledger(), summary)
        }
        
        shiny::div(
          class = "draft-v2-rec-grid",
          
          shiny::div(
            class = "draft-v2-rec-box",
            shiny::span(
              "RECOMMENDATION"
            ),
            shiny::strong(
              recommendation$posture
            )
          ),
          
          shiny::div(
            shiny::h3(
              style = "margin:0 0 7px;",
              paste(selected_team(), "draft-capital action")
            ),
            shiny::div(
              class = "draft-v2-recommendation-facts",
              signal_row("Strongest control year", recommendation$strongest_year),
              signal_row("Most constrained year", recommendation$constrained_year),
              signal_row("Biggest obligation", recommendation$biggest_obligation),
              signal_row("Records requiring verification", recommendation$verification),
              signal_row("Next action", recommendation$next_action)
            ),
            shiny::p(
              style = "margin:9px 0 0; color:#8998ab; line-height:1.5;",
              "FACT + MODEL OUTPUT · Draft value is internal decision support; verified ownership and protection language remain controlling inputs."
            )
          )
        )
      })
    }
  )
}
