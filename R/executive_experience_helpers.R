# ============================================================
# Thompson's Basketball Intelligence
# Phase 7: Executive Experience Helpers
# ============================================================

# Reusable UI helpers for executive-facing decision support.
#
# These helpers are intentionally separate from any one Shiny module so
# Executive Dashboard, Trade Intelligence, Extension Simulator, Draft
# Intelligence, and Five-Year Outlook can present decisions consistently.
#
# Expected upstream inputs:
# - Basketball Intelligence Engine result
# - Trade / Extension / Draft scenario comparison results
# - Explicit data-quality and assumption metadata

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}


#' Convert a value to clean text
#' @noRd
executive_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) default else value
}


#' Convert a value to safe numeric
#' @noRd
executive_number <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x))
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !is.finite(value[[1]])
  ) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to logical
#' @noRd
executive_flag <- function(x, default = FALSE) {
  if (is.logical(x) && length(x) && !is.na(x[[1]])) {
    return(isTRUE(x[[1]]))
  }
  
  if (is.numeric(x) && length(x) && !is.na(x[[1]])) {
    return(x[[1]] != 0)
  }
  
  if (is.character(x) && length(x)) {
    value <- tolower(trimws(x[[1]]))
    
    if (value %in% c("true", "t", "yes", "y", "1")) return(TRUE)
    if (value %in% c("false", "f", "no", "n", "0")) return(FALSE)
  }
  
  default
}


#' Clamp executive score
#' @noRd
executive_score <- function(x) {
  value <- executive_number(x, 0)
  
  min(max(value, 0), 100)
}


#' Safe nested list lookup
#' @noRd
executive_get <- function(x, path, default = NULL) {
  if (is.null(x)) return(default)
  
  current <- x
  
  for (name in path) {
    if (!is.list(current) || !name %in% names(current)) {
      return(default)
    }
    
    current <- current[[name]]
  }
  
  if (is.null(current) || !length(current)) default else current
}


# ------------------------------------------------------------
# Status and severity mapping
# ------------------------------------------------------------

#' Normalize a status label
#' @noRd
executive_status_key <- function(x) {
  value <- tolower(
    executive_text(x, "unknown")
  )
  
  gsub(
    "[^a-z0-9]+",
    "_",
    value
  )
}


#' Return semantic severity for a status
#' @noRd
executive_status_severity <- function(status) {
  key <- executive_status_key(status)
  
  positive <- c(
    "pass",
    "eligible",
    "verified",
    "advance",
    "aggressive",
    "positive",
    "strong",
    "elite",
    "below_cap"
  )
  
  caution <- c(
    "pass_with_review",
    "eligible_with_review",
    "needs_review",
    "neutral",
    "hold_compare_alternatives",
    "advance_with_conditions",
    "tax_team",
    "above_average",
    "balanced"
  )
  
  negative <- c(
    "fail",
    "ineligible",
    "do_not_advance",
    "negative",
    "above_second_apron",
    "obligation_heavy"
  )
  
  warning <- c(
    "caution",
    "proceed_with_caution",
    "above_first_apron",
    "limited",
    "unverified"
  )
  
  if (key %in% positive) return("positive")
  if (key %in% caution) return("caution")
  if (key %in% negative) return("negative")
  if (key %in% warning) return("warning")
  
  "neutral"
}


#' Semantic class names for executive components
#' @noRd
executive_severity_class <- function(severity) {
  severity <- executive_status_key(severity)
  
  supported <- c(
    "positive",
    "caution",
    "warning",
    "negative",
    "neutral",
    "info"
  )
  
  if (!severity %in% supported) {
    severity <- "neutral"
  }
  
  paste0(
    "tbi-exec--",
    severity
  )
}


#' Format status label for display
#' @noRd
executive_display_label <- function(x) {
  value <- executive_text(x, "Unknown")
  
  value <- gsub("_", " ", value, fixed = TRUE)
  words <- strsplit(tolower(value), "\\s+")[[1]]
  
  paste(
    paste0(
      toupper(substr(words, 1, 1)),
      substr(words, 2, nchar(words))
    ),
    collapse = " "
  )
}


# ------------------------------------------------------------
# Generic UI primitives
# ------------------------------------------------------------

#' Executive status badge
#' @noRd
executive_status_badge <- function(label,
                                   severity = NULL,
                                   icon = NULL) {
  label <- executive_display_label(label)
  
  severity <- severity %||%
    executive_status_severity(label)
  
  icon_tag <- if (!is.null(icon)) {
    shiny::icon(icon)
  } else {
    NULL
  }
  
  shiny::span(
    class = paste(
      "tbi-exec-badge",
      executive_severity_class(severity)
    ),
    icon_tag,
    shiny::span(
      class = "tbi-exec-badge__label",
      label
    )
  )
}


#' Executive data provenance badge
#' @noRd
executive_data_badge <- function(status = c(
  "Verified",
  "Assumption-Based",
  "Needs Review",
  "Unavailable"
),
detail = NULL) {
  status <- match.arg(status)
  
  severity <- switch(
    status,
    "Verified" = "positive",
    "Assumption-Based" = "caution",
    "Needs Review" = "warning",
    "Unavailable" = "neutral"
  )
  
  icon <- switch(
    status,
    "Verified" = "circle-check",
    "Assumption-Based" = "triangle-exclamation",
    "Needs Review" = "magnifying-glass",
    "Unavailable" = "circle-minus"
  )
  
  badge <- executive_status_badge(
    label = status,
    severity = severity,
    icon = icon
  )
  
  if (is.null(detail) || !nzchar(executive_text(detail))) {
    return(badge)
  }
  
  shiny::tags$span(
    class = "tbi-exec-data-status",
    badge,
    shiny::tags$span(
      class = "tbi-exec-data-status__detail",
      executive_text(detail)
    )
  )
}


#' Executive metric card
#' @noRd
executive_metric_card <- function(label,
                                  value,
                                  subtitle = NULL,
                                  status = NULL,
                                  severity = NULL,
                                  icon = NULL) {
  shiny::tags$div(
    class = paste(
      "tbi-exec-metric-card",
      if (!is.null(severity)) {
        executive_severity_class(severity)
      } else {
        ""
      }
    ),
    shiny::tags$div(
      class = "tbi-exec-metric-card__header",
      shiny::tags$span(
        class = "tbi-exec-metric-card__label",
        executive_text(label)
      ),
      if (!is.null(icon)) shiny::icon(icon) else NULL
    ),
    shiny::tags$div(
      class = "tbi-exec-metric-card__value",
      executive_text(value, "—")
    ),
    if (!is.null(subtitle)) {
      shiny::tags$div(
        class = "tbi-exec-metric-card__subtitle",
        executive_text(subtitle)
      )
    } else {
      NULL
    },
    if (!is.null(status)) {
      shiny::tags$div(
        class = "tbi-exec-metric-card__status",
        executive_status_badge(
          status,
          severity = severity
        )
      )
    } else {
      NULL
    }
  )
}


#' Executive empty state
#' @noRd
executive_empty_state <- function(title = "No data available",
                                  message = "The selected organization does not have enough verified data for this view.",
                                  icon = "database") {
  shiny::tags$div(
    class = "tbi-exec-empty-state",
    shiny::tags$div(
      class = "tbi-exec-empty-state__icon",
      shiny::icon(icon)
    ),
    shiny::tags$h4(
      class = "tbi-exec-empty-state__title",
      executive_text(title)
    ),
    shiny::tags$p(
      class = "tbi-exec-empty-state__message",
      executive_text(message)
    )
  )
}


#' Executive loading state
#' @noRd
executive_loading_state <- function(message = "Loading executive intelligence…") {
  shiny::tags$div(
    class = "tbi-exec-loading-state",
    shiny::tags$div(
      class = "tbi-exec-loading-state__spinner"
    ),
    shiny::tags$span(
      class = "tbi-exec-loading-state__message",
      executive_text(message)
    )
  )
}


# ------------------------------------------------------------
# Basketball Intelligence presentation
# ------------------------------------------------------------

#' Score color severity
#' @noRd
executive_score_severity <- function(score) {
  score <- executive_score(score)
  
  if (score >= 78) return("positive")
  if (score >= 65) return("caution")
  if (score >= 50) return("neutral")
  if (score >= 35) return("warning")
  
  "negative"
}


#' Recommendation banner
#' @noRd
executive_recommendation_banner <- function(
    intelligence_result,
    title = "Executive Recommendation") {
  
  if (!is.list(intelligence_result)) {
    return(
      executive_empty_state(
        title = "Recommendation unavailable",
        message = "No Basketball Intelligence result was supplied.",
        icon = "brain"
      )
    )
  }
  
  recommendation <- executive_text(
    intelligence_result$recommendation,
    "Hold / Compare Alternatives"
  )
  
  classification <- executive_text(
    intelligence_result$classification,
    "Neutral"
  )
  
  score <- executive_score(
    intelligence_result$score
  )
  
  severity <- executive_score_severity(
    score
  )
  
  summary <- executive_text(
    intelligence_result$executive_summary,
    "No executive summary is available."
  )
  
  shiny::tags$section(
    class = paste(
      "tbi-exec-recommendation",
      executive_severity_class(severity)
    ),
    shiny::tags$div(
      class = "tbi-exec-recommendation__header",
      shiny::tags$div(
        shiny::tags$div(
          class = "tbi-exec-recommendation__eyebrow",
          executive_text(title)
        ),
        shiny::tags$h2(
          class = "tbi-exec-recommendation__title",
          recommendation
        )
      ),
      shiny::tags$div(
        class = "tbi-exec-recommendation__score",
        shiny::tags$span(
          class = "tbi-exec-recommendation__score-value",
          sprintf("%.1f", score)
        ),
        shiny::tags$span(
          class = "tbi-exec-recommendation__score-scale",
          "/100"
        )
      )
    ),
    shiny::tags$div(
      class = "tbi-exec-recommendation__meta",
      executive_status_badge(
        classification,
        severity = severity
      ),
      executive_data_badge(
        status = if (
          executive_flag(
            intelligence_result$requires_manual_review,
            FALSE
          )
        ) {
          "Needs Review"
        } else {
          "Verified"
        }
      )
    ),
    shiny::tags$p(
      class = "tbi-exec-recommendation__summary",
      summary
    )
  )
}


#' One Basketball Intelligence component card
#' @noRd
executive_factor_card <- function(label,
                                  component,
                                  icon = NULL) {
  score <- executive_score(
    component$score %||% 0
  )
  
  severity <- executive_score_severity(
    score
  )
  
  explanation <- executive_text(
    component$explanation,
    "No explanation is available."
  )
  
  shiny::tags$article(
    class = paste(
      "tbi-exec-factor-card",
      executive_severity_class(severity)
    ),
    shiny::tags$div(
      class = "tbi-exec-factor-card__header",
      shiny::tags$div(
        class = "tbi-exec-factor-card__title-group",
        if (!is.null(icon)) shiny::icon(icon) else NULL,
        shiny::tags$h4(
          class = "tbi-exec-factor-card__title",
          executive_text(label)
        )
      ),
      shiny::tags$div(
        class = "tbi-exec-factor-card__score",
        sprintf("%.1f", score)
      )
    ),
    shiny::tags$div(
      class = "tbi-exec-factor-card__meter",
      shiny::tags$div(
        class = "tbi-exec-factor-card__meter-fill",
        style = paste0(
          "width:",
          round(score, 1),
          "%;"
        )
      )
    ),
    shiny::tags$p(
      class = "tbi-exec-factor-card__explanation",
      explanation
    )
  )
}


#' Five-factor scorecard
#' @noRd
executive_intelligence_scorecard <- function(intelligence_result) {
  if (
    !is.list(intelligence_result) ||
    !is.list(intelligence_result$components)
  ) {
    return(
      executive_empty_state(
        title = "Scorecard unavailable",
        message = "The five Basketball Intelligence components are not available.",
        icon = "chart-simple"
      )
    )
  }
  
  components <- intelligence_result$components
  
  labels <- list(
    competitive_position = list(
      label = "Competitive Position",
      icon = "trophy"
    ),
    financial_flexibility = list(
      label = "Financial Flexibility",
      icon = "scale-balanced"
    ),
    roster_control = list(
      label = "Roster Control",
      icon = "users"
    ),
    draft_capital = list(
      label = "Draft Capital",
      icon = "layer-group"
    ),
    transaction_risk = list(
      label = "Transaction Profile",
      icon = "arrow-right-arrow-left"
    )
  )
  
  cards <- lapply(
    names(labels),
    function(name) {
      executive_factor_card(
        label = labels[[name]]$label,
        component = components[[name]] %||%
          list(
            score = 0,
            explanation = "Component unavailable."
          ),
        icon = labels[[name]]$icon
      )
    }
  )
  
  shiny::tags$section(
    class = "tbi-exec-scorecard",
    shiny::tags$div(
      class = "tbi-exec-section-header",
      shiny::tags$div(
        shiny::tags$div(
          class = "tbi-exec-section-header__eyebrow",
          "Decision Framework"
        ),
        shiny::tags$h3(
          class = "tbi-exec-section-header__title",
          "Basketball Intelligence Scorecard"
        )
      )
    ),
    shiny::tags$div(
      class = "tbi-exec-scorecard__grid",
      cards
    )
  )
}


# ------------------------------------------------------------
# Risk and opportunity presentation
# ------------------------------------------------------------

#' Executive callout
#' @noRd
executive_callout <- function(title,
                              message,
                              severity = "neutral",
                              icon = NULL) {
  shiny::tags$article(
    class = paste(
      "tbi-exec-callout",
      executive_severity_class(severity)
    ),
    shiny::tags$div(
      class = "tbi-exec-callout__icon",
      if (!is.null(icon)) shiny::icon(icon) else NULL
    ),
    shiny::tags$div(
      class = "tbi-exec-callout__content",
      shiny::tags$h4(
        class = "tbi-exec-callout__title",
        executive_text(title)
      ),
      shiny::tags$p(
        class = "tbi-exec-callout__message",
        executive_text(message)
      )
    )
  )
}


#' Key risk list
#' @noRd
executive_risk_panel <- function(intelligence_result,
                                 title = "Key Risks") {
  risks <- intelligence_result$key_risks %||%
    character(0)
  
  risks <- as.character(risks)
  risks <- risks[nzchar(trimws(risks))]
  
  if (!length(risks)) {
    return(
      executive_callout(
        title = "No major structural risk identified",
        message = "The supplied inputs did not identify a major structural constraint.",
        severity = "positive",
        icon = "shield"
      )
    )
  }
  
  items <- lapply(
    risks,
    function(risk) {
      shiny::tags$li(
        class = "tbi-exec-risk-panel__item",
        shiny::icon("triangle-exclamation"),
        shiny::tags$span(risk)
      )
    }
  )
  
  shiny::tags$section(
    class = "tbi-exec-risk-panel",
    shiny::tags$div(
      class = "tbi-exec-section-header",
      shiny::tags$h3(
        class = "tbi-exec-section-header__title",
        executive_text(title)
      )
    ),
    shiny::tags$ul(
      class = "tbi-exec-risk-panel__list",
      items
    )
  )
}


#' Opportunity panel
#' @noRd
executive_opportunity_panel <- function(opportunities,
                                        title = "Key Opportunities") {
  opportunities <- as.character(
    opportunities %||% character(0)
  )
  
  opportunities <- opportunities[
    nzchar(trimws(opportunities))
  ]
  
  if (!length(opportunities)) {
    return(
      executive_empty_state(
        title = "No opportunities loaded",
        message = "Opportunity callouts have not been supplied for this decision.",
        icon = "lightbulb"
      )
    )
  }
  
  cards <- lapply(
    opportunities,
    function(item) {
      executive_callout(
        title = "Opportunity",
        message = item,
        severity = "positive",
        icon = "arrow-trend-up"
      )
    }
  )
  
  shiny::tags$section(
    class = "tbi-exec-opportunity-panel",
    shiny::tags$div(
      class = "tbi-exec-section-header",
      shiny::tags$h3(
        class = "tbi-exec-section-header__title",
        executive_text(title)
      )
    ),
    shiny::tags$div(
      class = "tbi-exec-opportunity-panel__grid",
      cards
    )
  )
}


# ------------------------------------------------------------
# Scenario comparison
# ------------------------------------------------------------

#' Scenario comparison card
#' @noRd
executive_scenario_card <- function(label,
                                    decision,
                                    is_preferred = FALSE) {
  score <- executive_score(
    decision$score
  )
  
  severity <- executive_score_severity(
    score
  )
  
  shiny::tags$article(
    class = paste(
      "tbi-exec-scenario-card",
      executive_severity_class(severity),
      if (isTRUE(is_preferred)) {
        "tbi-exec-scenario-card--preferred"
      } else {
        ""
      }
    ),
    shiny::tags$div(
      class = "tbi-exec-scenario-card__header",
      shiny::tags$h4(
        class = "tbi-exec-scenario-card__title",
        executive_text(label)
      ),
      if (isTRUE(is_preferred)) {
        executive_status_badge(
          "Preferred",
          severity = "positive",
          icon = "star"
        )
      } else {
        NULL
      }
    ),
    shiny::tags$div(
      class = "tbi-exec-scenario-card__score",
      sprintf("%.1f", score)
    ),
    shiny::tags$div(
      class = "tbi-exec-scenario-card__classification",
      executive_status_badge(
        decision$classification %||%
          "Unknown",
        severity = severity
      )
    ),
    shiny::tags$p(
      class = "tbi-exec-scenario-card__summary",
      executive_text(
        decision$executive_summary,
        "No summary is available."
      )
    )
  )
}


#' Scenario comparison panel
#' @noRd
executive_scenario_comparison <- function(
    comparison_result,
    decision_a,
    decision_b) {
  
  if (
    !is.list(comparison_result) ||
    !is.list(decision_a) ||
    !is.list(decision_b)
  ) {
    return(
      executive_empty_state(
        title = "Scenario comparison unavailable",
        message = "Two complete decision scenarios are required.",
        icon = "code-compare"
      )
    )
  }
  
  label_a <- executive_text(
    comparison_result$label_a,
    "Scenario A"
  )
  
  label_b <- executive_text(
    comparison_result$label_b,
    "Scenario B"
  )
  
  preferred <- executive_text(
    comparison_result$preferred,
    "Even"
  )
  
  shiny::tags$section(
    class = "tbi-exec-scenario-comparison",
    shiny::tags$div(
      class = "tbi-exec-section-header",
      shiny::tags$div(
        shiny::tags$div(
          class = "tbi-exec-section-header__eyebrow",
          "Scenario Analysis"
        ),
        shiny::tags$h3(
          class = "tbi-exec-section-header__title",
          "Decision Comparison"
        )
      ),
      executive_status_badge(
        preferred,
        severity = if (preferred == "Even") {
          "neutral"
        } else {
          "positive"
        }
      )
    ),
    shiny::tags$p(
      class = "tbi-exec-scenario-comparison__summary",
      executive_text(
        comparison_result$executive_summary,
        "No scenario comparison summary is available."
      )
    ),
    shiny::tags$div(
      class = "tbi-exec-scenario-comparison__grid",
      executive_scenario_card(
        label = label_a,
        decision = decision_a,
        is_preferred = identical(
          preferred,
          label_a
        )
      ),
      executive_scenario_card(
        label = label_b,
        decision = decision_b,
        is_preferred = identical(
          preferred,
          label_b
        )
      )
    )
  )
}


# ------------------------------------------------------------
# Data-quality summary
# ------------------------------------------------------------

#' Executive data-quality panel
#' @noRd
executive_data_quality_panel <- function(
    verified_items = 0L,
    assumption_items = 0L,
    review_items = 0L,
    unavailable_items = 0L,
    updated_at = NULL,
    title = "Data Confidence",
    explanation = NULL) {
  
  verified_items <- max(
    as.integer(verified_items %||% 0L),
    0L
  )
  
  assumption_items <- max(
    as.integer(assumption_items %||% 0L),
    0L
  )
  
  review_items <- max(
    as.integer(review_items %||% 0L),
    0L
  )
  
  unavailable_items <- max(
    as.integer(unavailable_items %||% 0L),
    0L
  )
  
  shiny::tags$section(
    class = "tbi-exec-data-quality",
    shiny::tags$div(
      class = "tbi-exec-section-header",
      shiny::tags$h3(
        class = "tbi-exec-section-header__title",
        executive_text(title, "Data Confidence")
      ),
      if (!is.null(updated_at)) {
        shiny::tags$span(
          class = "tbi-exec-data-quality__updated",
          paste(
            "Updated",
            executive_text(updated_at)
          )
        )
      } else {
        NULL
      }
    ),
    if (!is.null(explanation)) {
      shiny::tags$p(
        class = "command-evidence-explanation",
        executive_text(explanation)
      )
    },
    shiny::tags$div(
      class = "tbi-exec-data-quality__grid",
      executive_metric_card(
        label = "Verified",
        value = verified_items,
        subtitle = "Loaded facts with verified/current evidence.",
        status = "Verified",
        severity = "positive",
        icon = "circle-check"
      ),
      executive_metric_card(
        label = "Assumption-Based",
        value = assumption_items,
        subtitle = "Outputs that depend on explicit modeled assumptions.",
        status = "Assumption-Based",
        severity = "caution",
        icon = "triangle-exclamation"
      ),
      executive_metric_card(
        label = "Needs Review",
        value = review_items,
        subtitle = "Loaded evidence that requires source or rule verification.",
        status = "Needs Review",
        severity = "warning",
        icon = "magnifying-glass"
      ),
      executive_metric_card(
        label = "Unavailable",
        value = unavailable_items,
        subtitle = "Required information not currently loaded.",
        status = "Unavailable",
        severity = "neutral",
        icon = "circle-minus"
      )
    )
  )
}


# ------------------------------------------------------------
# Complete executive decision view
# ------------------------------------------------------------

#' Complete executive decision layout
#' @noRd
executive_decision_view <- function(
    intelligence_result,
    opportunities = NULL,
    data_quality = NULL,
    comparison = NULL,
    decision_a = NULL,
    decision_b = NULL) {
  
  if (!is.list(intelligence_result)) {
    return(
      executive_empty_state(
        title = "Executive decision unavailable",
        message = "A Basketball Intelligence result is required.",
        icon = "brain"
      )
    )
  }
  
  data_quality_ui <- NULL
  
  if (is.list(data_quality)) {
    data_quality_ui <- do.call(
      executive_data_quality_panel,
      data_quality
    )
  }
  
  comparison_ui <- NULL
  
  if (
    !is.null(comparison) &&
    !is.null(decision_a) &&
    !is.null(decision_b)
  ) {
    comparison_ui <- executive_scenario_comparison(
      comparison_result = comparison,
      decision_a = decision_a,
      decision_b = decision_b
    )
  }
  
  shiny::tags$div(
    class = "tbi-executive-decision-view",
    executive_recommendation_banner(
      intelligence_result
    ),
    executive_intelligence_scorecard(
      intelligence_result
    ),
    shiny::tags$div(
      class = "tbi-executive-decision-view__two-column",
      executive_risk_panel(
        intelligence_result
      ),
      executive_opportunity_panel(
        opportunities
      )
    ),
    data_quality_ui,
    comparison_ui,
    shiny::tags$div(
      class = "tbi-executive-decision-view__scope-note",
      shiny::icon("circle-info"),
      shiny::tags$span(
        executive_text(
          intelligence_result$scope_note,
          "This output is intended for internal decision support."
        )
      )
    )
  )
}
