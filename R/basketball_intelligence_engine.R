# ============================================================
# Thompson's Basketball Intelligence
# Phase 6: Basketball Intelligence Engine
# ============================================================

# This engine converts outputs from the verified TBI engines into
# executive decision support.
#
# It is independent of Shiny and designed to work with:
# - cap_engine.R
# - trade_engine.R
# - extension_engine.R
# - draft_assets_engine.R
# - draft_value_engine.R
# - draft_simulation_engine.R
#
# IMPORTANT:
# - Recommendations are internal decision-support outputs.
# - The engine does not replace basketball judgment, medical review,
#   scouting, legal review, or final CBA interpretation.
# - Every recommendation is driven by explicit, inspectable inputs.

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}


#' Convert a value to a safe numeric scalar
#' @noRd
basketball_intel_number <- function(x, default = NA_real_) {
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


#' Convert a value to a safe integer scalar
#' @noRd
basketball_intel_integer <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))
  
  if (!length(value) || is.na(value[[1]])) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to a clean character scalar
#' @noRd
basketball_intel_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) default else value
}


#' Convert a value to a logical scalar
#' @noRd
basketball_intel_flag <- function(x, default = FALSE) {
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


#' Clamp a numeric score
#' @noRd
basketball_intel_clamp <- function(x,
                                   minimum = 0,
                                   maximum = 100) {
  x <- basketball_intel_number(x, minimum)
  
  min(max(x, minimum), maximum)
}


#' Read a nested list value safely
#' @noRd
basketball_intel_get <- function(x,
                                 path,
                                 default = NULL) {
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
# Centralized rules
# ------------------------------------------------------------

#' Default Basketball Intelligence rules
#' @noRd
basketball_intelligence_rule_defaults <- function() {
  list(
    model_label = "TBI Basketball Intelligence Model v1",
    
    weights = c(
      competitive_position = 0.25,
      financial_flexibility = 0.25,
      roster_control = 0.15,
      draft_capital = 0.20,
      transaction_risk = 0.15
    ),
    
    competitive_scores = c(
      contender = 90,
      playoff = 72,
      play_in = 55,
      rebuilding = 35,
      unknown = 50
    ),
    
    cap_status_scores = c(
      below_cap = 92,
      over_cap = 72,
      tax_team = 55,
      above_first_apron = 35,
      above_second_apron = 18,
      unknown = 50
    ),
    
    draft_portfolio_scores = c(
      elite = 90,
      strong = 78,
      above_average = 66,
      balanced = 55,
      limited = 38,
      obligation_heavy = 20,
      unrated = 50
    ),
    
    extension_status_scores = c(
      pass = 85,
      pass_with_review = 70,
      fail = 25,
      unknown = 50
    ),
    
    trade_status_scores = c(
      pass = 82,
      fail = 20,
      unknown = 50
    ),
    
    score_bands = c(
      aggressive = 78,
      positive = 65,
      neutral = 50,
      caution = 35
    ),
    
    concentration_penalties = c(
      high = 20,
      moderate = 10,
      low = 0
    ),
    
    review_penalty_per_item = 3,
    maximum_review_penalty = 15,
    
    apron_penalties = c(
      first = 12,
      second = 22
    ),
    
    recommendation_labels = c(
      aggressive = "Advance",
      positive = "Advance with Conditions",
      neutral = "Hold / Compare Alternatives",
      caution = "Proceed with Caution",
      negative = "Do Not Advance"
    )
  )
}


#' Resolve Basketball Intelligence rules
#' @noRd
resolve_basketball_intelligence_rules <- function(
    rule_overrides = NULL) {
  rules <- basketball_intelligence_rule_defaults()
  
  if (is.null(rule_overrides)) {
    return(rules)
  }
  
  if (!is.list(rule_overrides)) {
    stop(
      "rule_overrides must be a named list.",
      call. = FALSE
    )
  }
  
  unknown <- setdiff(
    names(rule_overrides),
    names(rules)
  )
  
  if (length(unknown)) {
    stop(
      paste0(
        "Unknown basketball-intelligence rule override(s): ",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  rules[names(rule_overrides)] <- rule_overrides
  
  rules
}


# ------------------------------------------------------------
# Normalization helpers
# ------------------------------------------------------------

#' Normalize a label for lookup
#' @noRd
basketball_intel_key <- function(x) {
  value <- tolower(
    basketball_intel_text(x, "unknown")
  )
  
  gsub(
    "[^a-z0-9]+",
    "_",
    value
  )
}


#' Resolve a score from a named score map
#' @noRd
basketball_intel_lookup_score <- function(label,
                                          score_map,
                                          default = 50) {
  key <- basketball_intel_key(label)
  
  if (key %in% names(score_map)) {
    return(
      basketball_intel_number(
        score_map[[key]],
        default
      )
    )
  }
  
  default
}


# ------------------------------------------------------------
# Competitive position
# ------------------------------------------------------------

#' Evaluate competitive position
#' @noRd
evaluate_competitive_position <- function(
    competitive_tier = "Unknown",
    projected_wins = NA_real_,
    playoff_probability = NA_real_,
    championship_probability = NA_real_,
    rules = basketball_intelligence_rule_defaults()) {
  
  tier_score <- basketball_intel_lookup_score(
    competitive_tier,
    rules$competitive_scores,
    default = 50
  )
  
  projected_wins <- basketball_intel_number(
    projected_wins,
    NA_real_
  )
  
  playoff_probability <- basketball_intel_number(
    playoff_probability,
    NA_real_
  )
  
  championship_probability <- basketball_intel_number(
    championship_probability,
    NA_real_
  )
  
  score_components <- c(tier_score)
  
  if (!is.na(projected_wins)) {
    wins_score <- basketball_intel_clamp(
      (projected_wins - 20) / 40 * 100
    )
    
    score_components <- c(
      score_components,
      wins_score
    )
  }
  
  if (!is.na(playoff_probability)) {
    if (playoff_probability > 1) {
      playoff_probability <- playoff_probability / 100
    }
    
    score_components <- c(
      score_components,
      basketball_intel_clamp(
        playoff_probability * 100
      )
    )
  }
  
  if (!is.na(championship_probability)) {
    if (championship_probability > 1) {
      championship_probability <-
        championship_probability / 100
    }
    
    championship_score <- basketball_intel_clamp(
      championship_probability * 250
    )
    
    score_components <- c(
      score_components,
      championship_score
    )
  }
  
  score <- mean(
    score_components,
    na.rm = TRUE
  )
  
  list(
    score = basketball_intel_clamp(score),
    competitive_tier = basketball_intel_text(
      competitive_tier,
      "Unknown"
    ),
    projected_wins = projected_wins,
    playoff_probability = playoff_probability,
    championship_probability = championship_probability,
    explanation = paste0(
      "Competitive position was evaluated using the supplied tier",
      if (!is.na(projected_wins)) {
        paste0(", projected wins of ", round(projected_wins, 1))
      } else {
        ""
      },
      if (!is.na(playoff_probability)) {
        paste0(
          ", and a ",
          round(playoff_probability * 100, 1),
          "% playoff probability"
        )
      } else {
        ""
      },
      "."
    )
  )
}


# ------------------------------------------------------------
# Financial flexibility
# ------------------------------------------------------------

#' Evaluate financial flexibility from a cap-engine result
#' @noRd
evaluate_financial_flexibility <- function(
    cap_result,
    top_three_concentration = NA_real_,
    future_committed_salary_ratio = NA_real_,
    rules = basketball_intelligence_rule_defaults()) {
  
  if (!is.list(cap_result)) {
    stop(
      "cap_result must be a cap-engine result list.",
      call. = FALSE
    )
  }
  
  operating_status <- basketball_intel_text(
    cap_result$operating_status %||%
      cap_result$status,
    "Unknown"
  )
  
  status_score <- basketball_intel_lookup_score(
    operating_status,
    rules$cap_status_scores,
    default = 50
  )
  
  top_three_concentration <- basketball_intel_number(
    top_three_concentration %||%
      cap_result$top_three_salary_concentration,
    NA_real_
  )
  
  future_committed_salary_ratio <-
    basketball_intel_number(
      future_committed_salary_ratio,
      NA_real_
    )
  
  concentration_penalty <- 0
  
  if (!is.na(top_three_concentration)) {
    if (top_three_concentration > 0.65) {
      concentration_penalty <-
        rules$concentration_penalties[["high"]]
    } else if (top_three_concentration > 0.50) {
      concentration_penalty <-
        rules$concentration_penalties[["moderate"]]
    }
  }
  
  future_commitment_penalty <- 0
  
  if (!is.na(future_committed_salary_ratio)) {
    if (future_committed_salary_ratio > 1.10) {
      future_commitment_penalty <- 15
    } else if (future_committed_salary_ratio > 0.95) {
      future_commitment_penalty <- 8
    }
  }
  
  score <- basketball_intel_clamp(
    status_score -
      concentration_penalty -
      future_commitment_penalty
  )
  
  list(
    score = score,
    operating_status = operating_status,
    concentration_penalty = concentration_penalty,
    future_commitment_penalty =
      future_commitment_penalty,
    explanation = paste0(
      "Financial flexibility reflects ",
      operating_status,
      " status",
      if (concentration_penalty > 0) {
        paste0(
          ", with a ",
          concentration_penalty,
          "-point concentration penalty"
        )
      } else {
        ""
      },
      if (future_commitment_penalty > 0) {
        paste0(
          " and a ",
          future_commitment_penalty,
          "-point future-commitment penalty"
        )
      } else {
        ""
      },
      "."
    )
  )
}


# ------------------------------------------------------------
# Roster control
# ------------------------------------------------------------

#' Evaluate roster control and optionality
#' @noRd
evaluate_roster_control <- function(
    guaranteed_roster_spots = NA_integer_,
    expiring_contracts = NA_integer_,
    team_options = NA_integer_,
    player_options = NA_integer_,
    two_way_contracts = NA_integer_,
    dead_money_ratio = NA_real_) {
  
  guaranteed_roster_spots <- basketball_intel_integer(
    guaranteed_roster_spots,
    NA_integer_
  )
  
  expiring_contracts <- basketball_intel_integer(
    expiring_contracts,
    NA_integer_
  )
  
  team_options <- basketball_intel_integer(
    team_options,
    NA_integer_
  )
  
  player_options <- basketball_intel_integer(
    player_options,
    NA_integer_
  )
  
  two_way_contracts <- basketball_intel_integer(
    two_way_contracts,
    NA_integer_
  )
  
  dead_money_ratio <- basketball_intel_number(
    dead_money_ratio,
    0
  )
  
  score <- 55
  
  if (!is.na(guaranteed_roster_spots)) {
    if (guaranteed_roster_spots <= 10) {
      score <- score + 12
    } else if (guaranteed_roster_spots >= 14) {
      score <- score - 10
    }
  }
  
  if (!is.na(expiring_contracts)) {
    score <- score + min(expiring_contracts * 2, 10)
  }
  
  if (!is.na(team_options)) {
    score <- score + min(team_options * 3, 9)
  }
  
  if (!is.na(player_options)) {
    score <- score - min(player_options * 2, 8)
  }
  
  if (!is.na(two_way_contracts)) {
    score <- score + min(two_way_contracts, 3)
  }
  
  if (!is.na(dead_money_ratio)) {
    score <- score - min(dead_money_ratio * 100, 20)
  }
  
  score <- basketball_intel_clamp(score)
  
  list(
    score = score,
    guaranteed_roster_spots =
      guaranteed_roster_spots,
    expiring_contracts = expiring_contracts,
    team_options = team_options,
    player_options = player_options,
    two_way_contracts = two_way_contracts,
    dead_money_ratio = dead_money_ratio,
    explanation = paste(
      "Roster control reflects guaranteed spots, expirings,",
      "team-controlled options, player-controlled options,",
      "two-way flexibility, and dead-money exposure."
    )
  )
}


# ------------------------------------------------------------
# Draft capital
# ------------------------------------------------------------

#' Evaluate draft capital from a valued portfolio
#' @noRd
evaluate_draft_capital <- function(
    draft_value_result,
    draft_simulation_result = NULL,
    rules = basketball_intelligence_rule_defaults()) {
  
  if (!is.list(draft_value_result)) {
    stop(
      "draft_value_result must be a draft-value result list.",
      call. = FALSE
    )
  }
  
  portfolio_grade <- basketball_intel_text(
    basketball_intel_get(
      draft_value_result,
      c("summary", "portfolio_grade"),
      "Unrated"
    ),
    "Unrated"
  )
  
  base_score <- basketball_intel_lookup_score(
    portfolio_grade,
    rules$draft_portfolio_scores,
    default = 50
  )
  
  review_required <- basketball_intel_integer(
    basketball_intel_get(
      draft_value_result,
      c("summary", "review_required"),
      0L
    ),
    0L
  )
  
  review_penalty <- min(
    review_required *
      rules$review_penalty_per_item,
    rules$maximum_review_penalty
  )
  
  uncertainty_penalty <- 0
  
  if (!is.null(draft_simulation_result)) {
    mean_value <- basketball_intel_number(
      draft_simulation_result$mean_portfolio_value,
      NA_real_
    )
    
    value_sd <- basketball_intel_number(
      draft_simulation_result$portfolio_value_sd,
      NA_real_
    )
    
    if (
      !is.na(mean_value) &&
      !is.na(value_sd) &&
      abs(mean_value) > 0
    ) {
      coefficient_of_variation <-
        abs(value_sd / mean_value)
      
      uncertainty_penalty <- min(
        coefficient_of_variation * 20,
        12
      )
    }
  }
  
  score <- basketball_intel_clamp(
    base_score -
      review_penalty -
      uncertainty_penalty
  )
  
  list(
    score = score,
    portfolio_grade = portfolio_grade,
    review_penalty = review_penalty,
    uncertainty_penalty = uncertainty_penalty,
    explanation = paste0(
      "Draft capital was graded ",
      portfolio_grade,
      if (review_penalty > 0) {
        paste0(
          ", with a ",
          round(review_penalty, 1),
          "-point verification penalty"
        )
      } else {
        ""
      },
      if (uncertainty_penalty > 0) {
        paste0(
          " and a ",
          round(uncertainty_penalty, 1),
          "-point uncertainty penalty"
        )
      } else {
        ""
      },
      "."
    )
  )
}


# ------------------------------------------------------------
# Transaction risk
# ------------------------------------------------------------

#' Evaluate transaction risk from trade and extension results
#' @noRd
evaluate_transaction_risk <- function(
    trade_result = NULL,
    extension_result = NULL,
    crosses_first_apron = FALSE,
    crosses_second_apron = FALSE,
    manual_review_items = 0L,
    rules = basketball_intelligence_rule_defaults()) {
  
  component_scores <- numeric(0)
  explanations <- character(0)
  
  if (!is.null(trade_result)) {
    trade_status <- basketball_intel_text(
      trade_result$status %||%
        trade_result$screen_status,
      "Unknown"
    )
    
    trade_score <- basketball_intel_lookup_score(
      trade_status,
      rules$trade_status_scores,
      default = 50
    )
    
    component_scores <- c(
      component_scores,
      trade_score
    )
    
    explanations <- c(
      explanations,
      paste0(
        "Trade screen: ",
        trade_status,
        "."
      )
    )
    
    crosses_first_apron <-
      basketball_intel_flag(
        trade_result$crosses_first_apron,
        crosses_first_apron
      )
    
    crosses_second_apron <-
      basketball_intel_flag(
        trade_result$crosses_second_apron,
        crosses_second_apron
      )
  }
  
  if (!is.null(extension_result)) {
    extension_status <- basketball_intel_text(
      extension_result$status,
      "Unknown"
    )
    
    extension_score <- basketball_intel_lookup_score(
      extension_status,
      rules$extension_status_scores,
      default = 50
    )
    
    component_scores <- c(
      component_scores,
      extension_score
    )
    
    explanations <- c(
      explanations,
      paste0(
        "Extension screen: ",
        extension_status,
        "."
      )
    )
  }
  
  if (!length(component_scores)) {
    component_scores <- 60
    explanations <- "No specific transaction screen supplied."
  }
  
  score <- mean(component_scores)
  
  if (basketball_intel_flag(crosses_first_apron)) {
    score <- score - rules$apron_penalties[["first"]]
    explanations <- c(
      explanations,
      "Transaction crosses the first apron."
    )
  }
  
  if (basketball_intel_flag(crosses_second_apron)) {
    score <- score - rules$apron_penalties[["second"]]
    explanations <- c(
      explanations,
      "Transaction crosses the second apron."
    )
  }
  
  manual_review_items <- basketball_intel_integer(
    manual_review_items,
    0L
  )
  
  review_penalty <- min(
    manual_review_items *
      rules$review_penalty_per_item,
    rules$maximum_review_penalty
  )
  
  score <- basketball_intel_clamp(
    score - review_penalty
  )
  
  list(
    score = score,
    review_penalty = review_penalty,
    crosses_first_apron =
      basketball_intel_flag(crosses_first_apron),
    crosses_second_apron =
      basketball_intel_flag(crosses_second_apron),
    explanation = paste(
      explanations,
      collapse = " "
    )
  )
}


# ------------------------------------------------------------
# Composite score and recommendation
# ------------------------------------------------------------

#' Classify a composite Basketball Intelligence score
#' @noRd
classify_basketball_intelligence_score <- function(
    score,
    rules = basketball_intelligence_rule_defaults()) {
  
  score <- basketball_intel_clamp(score)
  bands <- rules$score_bands
  
  if (score >= bands[["aggressive"]]) {
    return("Aggressive")
  }
  
  if (score >= bands[["positive"]]) {
    return("Positive")
  }
  
  if (score >= bands[["neutral"]]) {
    return("Neutral")
  }
  
  if (score >= bands[["caution"]]) {
    return("Caution")
  }
  
  "Negative"
}


#' Resolve recommendation label
#' @noRd
basketball_intelligence_recommendation_label <- function(
    classification,
    rules = basketball_intelligence_rule_defaults()) {
  
  key <- basketball_intel_key(
    classification
  )
  
  label <- rules$recommendation_labels[[key]]
  
  if (is.null(label)) {
    return("Hold / Compare Alternatives")
  }
  
  label
}


#' Build executive rationale
#' @noRd
build_basketball_intelligence_rationale <- function(
    components,
    classification) {
  
  component_scores <- c(
    competitive_position =
      components$competitive_position$score,
    financial_flexibility =
      components$financial_flexibility$score,
    roster_control =
      components$roster_control$score,
    draft_capital =
      components$draft_capital$score,
    transaction_risk =
      components$transaction_risk$score
  )
  
  strongest_name <- names(
    which.max(component_scores)
  )
  
  weakest_name <- names(
    which.min(component_scores)
  )
  
  labels <- c(
    competitive_position = "competitive position",
    financial_flexibility = "financial flexibility",
    roster_control = "roster control",
    draft_capital = "draft capital",
    transaction_risk = "transaction profile"
  )
  
  paste0(
    "Overall classification is ",
    tolower(classification),
    ". The strongest factor is ",
    labels[[strongest_name]],
    " at ",
    round(component_scores[[strongest_name]], 1),
    ", while the primary constraint is ",
    labels[[weakest_name]],
    " at ",
    round(component_scores[[weakest_name]], 1),
    "."
  )
}


#' Evaluate a full basketball decision
#'
#' @param inputs Named list containing competitive, cap, roster,
#'   draft-value, draft-simulation, trade, and extension inputs.
#' @param rule_overrides Optional centralized rule overrides.
#' @return Composite Basketball Intelligence result.
#' @noRd
evaluate_basketball_decision <- function(
    inputs,
    rule_overrides = NULL) {
  
  if (!is.list(inputs)) {
    stop(
      "inputs must be a named list.",
      call. = FALSE
    )
  }
  
  rules <- resolve_basketball_intelligence_rules(
    rule_overrides
  )
  
  competitive <- evaluate_competitive_position(
    competitive_tier =
      basketball_intel_get(
        inputs,
        c("competitive", "competitive_tier"),
        "Unknown"
      ),
    projected_wins =
      basketball_intel_get(
        inputs,
        c("competitive", "projected_wins"),
        NA_real_
      ),
    playoff_probability =
      basketball_intel_get(
        inputs,
        c("competitive", "playoff_probability"),
        NA_real_
      ),
    championship_probability =
      basketball_intel_get(
        inputs,
        c("competitive", "championship_probability"),
        NA_real_
      ),
    rules = rules
  )
  
  cap_result <- inputs$cap_result %||%
    list(
      operating_status = "Unknown"
    )
  
  financial <- evaluate_financial_flexibility(
    cap_result = cap_result,
    top_three_concentration =
      basketball_intel_get(
        inputs,
        c("financial", "top_three_concentration"),
        NA_real_
      ),
    future_committed_salary_ratio =
      basketball_intel_get(
        inputs,
        c("financial", "future_committed_salary_ratio"),
        NA_real_
      ),
    rules = rules
  )
  
  roster <- evaluate_roster_control(
    guaranteed_roster_spots =
      basketball_intel_get(
        inputs,
        c("roster", "guaranteed_roster_spots"),
        NA_integer_
      ),
    expiring_contracts =
      basketball_intel_get(
        inputs,
        c("roster", "expiring_contracts"),
        NA_integer_
      ),
    team_options =
      basketball_intel_get(
        inputs,
        c("roster", "team_options"),
        NA_integer_
      ),
    player_options =
      basketball_intel_get(
        inputs,
        c("roster", "player_options"),
        NA_integer_
      ),
    two_way_contracts =
      basketball_intel_get(
        inputs,
        c("roster", "two_way_contracts"),
        NA_integer_
      ),
    dead_money_ratio =
      basketball_intel_get(
        inputs,
        c("roster", "dead_money_ratio"),
        NA_real_
      )
  )
  
  draft_value_result <- inputs$draft_value_result %||%
    list(
      summary = list(
        portfolio_grade = "Unrated",
        review_required = 0L
      )
    )
  
  draft <- evaluate_draft_capital(
    draft_value_result =
      draft_value_result,
    draft_simulation_result =
      inputs$draft_simulation_result,
    rules = rules
  )
  
  transaction <- evaluate_transaction_risk(
    trade_result = inputs$trade_result,
    extension_result = inputs$extension_result,
    crosses_first_apron =
      basketball_intel_get(
        inputs,
        c("transaction", "crosses_first_apron"),
        FALSE
      ),
    crosses_second_apron =
      basketball_intel_get(
        inputs,
        c("transaction", "crosses_second_apron"),
        FALSE
      ),
    manual_review_items =
      basketball_intel_get(
        inputs,
        c("transaction", "manual_review_items"),
        0L
      ),
    rules = rules
  )
  
  components <- list(
    competitive_position = competitive,
    financial_flexibility = financial,
    roster_control = roster,
    draft_capital = draft,
    transaction_risk = transaction
  )
  
  weights <- rules$weights
  
  weight_sum <- sum(weights)
  
  if (weight_sum <= 0) {
    stop(
      "Basketball Intelligence weights must sum to a positive value.",
      call. = FALSE
    )
  }
  
  weights <- weights / weight_sum
  
  composite_score <-
    competitive$score *
    weights[["competitive_position"]] +
    financial$score *
    weights[["financial_flexibility"]] +
    roster$score *
    weights[["roster_control"]] +
    draft$score *
    weights[["draft_capital"]] +
    transaction$score *
    weights[["transaction_risk"]]
  
  composite_score <- basketball_intel_clamp(
    composite_score
  )
  
  classification <-
    classify_basketball_intelligence_score(
      composite_score,
      rules = rules
    )
  
  recommendation <-
    basketball_intelligence_recommendation_label(
      classification,
      rules = rules
    )
  
  rationale <- build_basketball_intelligence_rationale(
    components = components,
    classification = classification
  )
  
  key_risks <- character(0)
  
  if (financial$score < 45) {
    key_risks <- c(
      key_risks,
      "Financial flexibility is materially constrained."
    )
  }
  
  if (draft$score < 45) {
    key_risks <- c(
      key_risks,
      "Draft capital provides limited downside protection."
    )
  }
  
  if (transaction$score < 45) {
    key_risks <- c(
      key_risks,
      "The transaction profile contains significant restrictions or review items."
    )
  }
  
  if (competitive$score < 45) {
    key_risks <- c(
      key_risks,
      "The competitive timeline may not justify an aggressive resource commitment."
    )
  }
  
  if (!length(key_risks)) {
    key_risks <- "No major structural risk was identified by the supplied inputs."
  }
  
  executive_summary <- paste0(
    recommendation,
    ". Composite Basketball Intelligence score: ",
    round(composite_score, 1),
    "/100. ",
    rationale
  )
  
  list(
    score = composite_score,
    classification = classification,
    recommendation = recommendation,
    components = components,
    weights = weights,
    key_risks = key_risks,
    executive_summary = executive_summary,
    model_label = rules$model_label,
    scope_note = paste(
      "This recommendation is a decision-support output.",
      "It should be reviewed alongside scouting, coaching, medical,",
      "ownership, legal, and CBA analysis."
    )
  )
}


# ------------------------------------------------------------
# Scenario comparison
# ------------------------------------------------------------

#' Compare two Basketball Intelligence decisions
#' @noRd
compare_basketball_decisions <- function(
    decision_a,
    decision_b,
    label_a = "Scenario A",
    label_b = "Scenario B") {
  
  if (!is.list(decision_a) || is.null(decision_a$score)) {
    stop(
      "decision_a must be a Basketball Intelligence result.",
      call. = FALSE
    )
  }
  
  if (!is.list(decision_b) || is.null(decision_b$score)) {
    stop(
      "decision_b must be a Basketball Intelligence result.",
      call. = FALSE
    )
  }
  
  score_a <- basketball_intel_number(
    decision_a$score,
    0
  )
  
  score_b <- basketball_intel_number(
    decision_b$score,
    0
  )
  
  difference <- score_a - score_b
  
  preferred <- if (difference > 0) {
    label_a
  } else if (difference < 0) {
    label_b
  } else {
    "Even"
  }
  
  executive_summary <- if (difference == 0) {
    paste0(
      label_a,
      " and ",
      label_b,
      " have equal Basketball Intelligence scores."
    )
  } else {
    paste0(
      preferred,
      " is preferred by ",
      round(abs(difference), 1),
      " points."
    )
  }
  
  list(
    label_a = label_a,
    label_b = label_b,
    score_a = score_a,
    score_b = score_b,
    difference = difference,
    preferred = preferred,
    executive_summary = executive_summary
  )
}