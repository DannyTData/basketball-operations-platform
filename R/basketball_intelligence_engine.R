# ============================================================
# Thompson's Basketball Intelligence
# Phase 2 / BIE v2: Basketball Intelligence Engine
# PATCH: RANKED POSITION FIT + 65/25/10 LINEUP WEIGHTS
# - Position order now matters (primary/secondary/tertiary/emergency)
# - Candidate pre-ranking uses the same player/position ratio as final scoring
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

# ============================================================
# PHASE 2 — BASKETBALL LAYER
# Player Evaluation + Lineup Optimization Foundation
# ============================================================
#
# This section extends the existing executive decision engine.
# It does NOT replace evaluate_basketball_decision().
#
# Design goals:
# 1. Work today with roster/depth-chart data.
# 2. Automatically use advanced basketball metrics when loaded.
# 3. Keep every score inspectable.
# 4. Separate "player value" from "lineup fit".
# 5. Never invent missing performance data.
# 6. Allow future internal team metrics to replace public metrics
#    without changing the optimizer interface.
#
# Primary public API added in this phase:
#
#   evaluate_bie_players(players)
#   optimize_bie_starting_five(players)
#   compare_bie_lineups(lineup_a, lineup_b)
#
# Expected minimum player columns:
#   player_id, player_name, primary_position
#
# Helpful optional columns:
#   eligible_positions, depth_order, is_starter, salary/cap_hit
#
# Advanced metrics are optional and discovered automatically.
# ============================================================


# ------------------------------------------------------------
# Performance helpers
# ------------------------------------------------------------

#' Lightweight roster signature for BIE caching
#'
#' This intentionally avoids digest/hash dependencies. It creates a
#' stable key from the fields that can materially change a roster-level
#' BIE result.
#' @noRd
bie_roster_signature <- function(players) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players)
  ) {
    return("EMPTY")
  }
  
  id <- if (
    "player_id" %in%
    names(players)
  ) {
    suppressWarnings(
      as.integer(players$player_id)
    )
  } else {
    seq_len(nrow(players))
  }
  
  depth <- bie_numeric_column(
    players,
    c(
      "depth_order",
      "depth"
    )
  )
  
  starter <- bie_numeric_column(
    players,
    c(
      "is_starter",
      "starter"
    ),
    default = 0
  )
  
  salary <- bie_numeric_column(
    players,
    c(
      "cap_hit",
      "salary",
      "base_salary"
    )
  )
  
  position <- bie_text_column(
    players,
    c(
      "eligible_positions",
      "official_positions",
      "primary_position",
      "position",
      "pos"
    ),
    default = ""
  )
  
  # Include any already-computed BIE scores so downstream caches
  # invalidate when performance inputs are refreshed.
  bie_score <- if (
    "bie_player_score" %in%
    names(players)
  ) {
    suppressWarnings(
      as.numeric(players$bie_player_score)
    )
  } else {
    rep(NA_real_, nrow(players))
  }
  
  rows <- paste(
    ifelse(is.na(id), "", id),
    ifelse(is.na(depth), "", round(depth, 3)),
    ifelse(is.na(starter), "", round(starter, 3)),
    ifelse(is.na(salary), "", round(salary, 0)),
    ifelse(is.na(position), "", position),
    ifelse(is.na(bie_score), "", round(bie_score, 4)),
    sep = ":"
  )
  
  paste(
    sort(rows),
    collapse = "|"
  )
}


#' Return already-evaluated roster data when possible
#' @noRd
bie_ensure_evaluated_players <- function(players) {
  
  if (
    is.null(players) ||
    !is.data.frame(players)
  ) {
    return(players)
  }
  
  required <- c(
    "bie_player_score",
    "bie_score_source",
    "bie_metric_components"
  )
  
  if (
    all(
      required %in%
      names(players)
    )
  ) {
    return(players)
  }
  
  evaluate_bie_players(players)
}


# ------------------------------------------------------------
# Generic Phase-2 helpers
# ------------------------------------------------------------

#' Return the first matching column name
#' @noRd
bie_find_column <- function(
    data,
    candidates) {
  
  if (
    is.null(data) ||
    !is.data.frame(data)
  ) {
    return(NULL)
  }
  
  match <- candidates[
    candidates %in%
      names(data)
  ]
  
  if (!length(match)) {
    return(NULL)
  }
  
  match[[1]]
}


#' Read a numeric vector from the first available column
#' @noRd
bie_numeric_column <- function(
    data,
    candidates,
    default = NA_real_) {
  
  column <- bie_find_column(
    data,
    candidates
  )
  
  if (is.null(column)) {
    return(
      rep(
        default,
        nrow(data)
      )
    )
  }
  
  value <- suppressWarnings(
    as.numeric(
      data[[column]]
    )
  )
  
  value[
    !is.finite(value)
  ] <- NA_real_
  
  value
}


#' Read a character vector from the first available column
#' @noRd
bie_text_column <- function(
    data,
    candidates,
    default = NA_character_) {
  
  column <- bie_find_column(
    data,
    candidates
  )
  
  if (is.null(column)) {
    return(
      rep(
        default,
        nrow(data)
      )
    )
  }
  
  value <- as.character(
    data[[column]]
  )
  
  value[
    is.na(value) |
      !nzchar(
        trimws(value)
      )
  ] <- default
  
  value
}


#' Percentile score with neutral treatment for missing values
#'
#' Missing data is never treated as bad performance.
#' It receives a neutral 50 until richer data is loaded.
#' @noRd
bie_percentile_score <- function(
    x,
    higher_is_better = TRUE,
    neutral = 50) {
  
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(x)
  
  output <- rep(
    neutral,
    length(x)
  )
  
  if (sum(valid) < 2) {
    return(output)
  }
  
  ranks <- rank(
    x[valid],
    ties.method = "average",
    na.last = "keep"
  )
  
  percentile <- if (
    length(ranks) == 1
  ) {
    50
  } else {
    100 *
      (ranks - 1) /
      (length(ranks) - 1)
  }
  
  if (!higher_is_better) {
    percentile <- 100 -
      percentile
  }
  
  output[valid] <- percentile
  
  basketball_intel_clamp(
    output
  )
}


#' Weighted mean that ignores unavailable features
#' @noRd
bie_weighted_available_score <- function(
    scores,
    weights,
    neutral = 50) {
  
  if (
    is.null(scores) ||
    !is.data.frame(scores) ||
    !nrow(scores)
  ) {
    return(numeric())
  }
  
  available_names <- intersect(
    names(weights),
    names(scores)
  )
  
  if (!length(available_names)) {
    return(
      rep(
        neutral,
        nrow(scores)
      )
    )
  }
  
  result <- rep(
    NA_real_,
    nrow(scores)
  )
  
  for (
    i in seq_len(
      nrow(scores)
    )
  ) {
    
    row_scores <- suppressWarnings(
      as.numeric(
        scores[
          i,
          available_names,
          drop = TRUE
        ]
      )
    )
    
    row_weights <- as.numeric(
      weights[
        available_names
      ]
    )
    
    valid <- is.finite(
      row_scores
    ) &
      is.finite(
        row_weights
      ) &
      row_weights > 0
    
    if (!any(valid)) {
      result[[i]] <- neutral
      next
    }
    
    result[[i]] <- weighted.mean(
      row_scores[valid],
      row_weights[valid]
    )
  }
  
  basketball_intel_clamp(
    result
  )
}


# ------------------------------------------------------------
# Position normalization
# ------------------------------------------------------------

#' Normalize one basketball position label
#' @noRd
bie_normalize_position <- function(x) {
  
  value <- toupper(
    trimws(
      as.character(
        x %||% ""
      )
    )
  )
  
  aliases <- c(
    "POINT GUARD" = "PG",
    "SHOOTING GUARD" = "SG",
    "SMALL FORWARD" = "SF",
    "POWER FORWARD" = "PF",
    "CENTER" = "C",
    "G" = "G",
    "F" = "F",
    "G-F" = "G-F",
    "F-G" = "G-F",
    "F-C" = "F-C",
    "C-F" = "F-C"
  )
  
  if (value %in% names(aliases)) {
    return(
      unname(
        aliases[[value]]
      )
    )
  }
  
  value
}


#' Expand a position token into NBA lineup positions
#' @noRd
bie_expand_position_token <- function(token) {
  
  token <- bie_normalize_position(
    token
  )
  
  if (token %in% c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )) {
    return(token)
  }
  
  if (token == "G") {
    return(
      c(
        "PG",
        "SG"
      )
    )
  }
  
  if (token == "F") {
    return(
      c(
        "SF",
        "PF"
      )
    )
  }
  
  if (token == "G-F") {
    return(
      c(
        "PG",
        "SG",
        "SF"
      )
    )
  }
  
  if (token == "F-C") {
    return(
      c(
        "SF",
        "PF",
        "C"
      )
    )
  }
  
  character()
}


#' Parse ranked eligible positions for one player row
#'
#' Position order matters. Current depth-chart `position` is treated
#' as the strongest present-day assignment, followed by the ordered
#' positions loaded in `primary_position`, then broader eligibility
#' fields. Duplicates are removed while preserving first occurrence.
#' @noRd
bie_ranked_player_positions <- function(row) {
  
  if (
    is.null(row) ||
    !is.data.frame(row) ||
    !nrow(row)
  ) {
    return(character())
  }
  
  candidate_columns <- c(
    "position",
    "primary_position",
    "eligible_positions",
    "official_positions",
    "positions",
    "pos"
  )
  
  ranked <- character()
  
  for (
    column in intersect(
      candidate_columns,
      names(row)
    )
  ) {
    
    value <- as.character(
      row[[column]][[1]]
    )
    
    if (
      is.na(value) ||
      !nzchar(
        trimws(value)
      )
    ) {
      next
    }
    
    tokens <- unlist(
      strsplit(
        value,
        "[,/|;]+"
      )
    )
    
    tokens <- trimws(tokens)
    
    for (token in tokens) {
      
      expanded <- bie_expand_position_token(
        token
      )
      
      expanded <- expanded[
        expanded %in%
          c(
            "PG",
            "SG",
            "SF",
            "PF",
            "C"
          )
      ]
      
      for (pos in expanded) {
        if (!pos %in% ranked) {
          ranked <- c(
            ranked,
            pos
          )
        }
      }
    }
  }
  
  ranked
}


#' Parse all eligible positions for one player row
#' @noRd
bie_player_positions <- function(row) {
  bie_ranked_player_positions(row)
}


#' Determine primary position for one player row
#' @noRd
bie_primary_position <- function(row) {
  
  positions <- bie_ranked_player_positions(
    row
  )
  
  if (length(positions)) {
    positions[[1]]
  } else {
    NA_character_
  }
}


# ------------------------------------------------------------
# Player basketball value
# ------------------------------------------------------------

#' Default Phase-2 player-value weights
#'
#' These weights determine the relative importance of metric
#' families after each metric is converted to a roster percentile.
#'
#' They are intentionally easy to override later.
#' @noRd
bie_player_weight_defaults <- function() {
  
  c(
    impact = 0.34,
    offense = 0.20,
    defense = 0.20,
    efficiency = 0.10,
    playmaking = 0.08,
    rebounding = 0.05,
    availability = 0.03
  )
}


#' Evaluate player basketball value
#'
#' This function discovers whichever basketball metrics are
#' currently available. It never fabricates missing stats.
#'
#' @param players Player-level data frame.
#' @param weight_overrides Optional named component weights.
#' @return Original data plus BIE player scores and data-quality info.
#' @noRd
evaluate_bie_players <- function(
    players,
    weight_overrides = NULL) {
  
  if (
    is.null(players) ||
    !is.data.frame(players)
  ) {
    stop(
      "players must be a data frame.",
      call. = FALSE
    )
  }
  
  if (!nrow(players)) {
    return(players)
  }
  
  d <- players
  
  if (
    !"player_id" %in%
    names(d)
  ) {
    d$player_id <- seq_len(
      nrow(d)
    )
  }
  
  if (
    !"player_name" %in%
    names(d)
  ) {
    d$player_name <- paste0(
      "Player ",
      d$player_id
    )
  }
  
  # ----------------------------------------------------------
  # Metric discovery
  # ----------------------------------------------------------
  
  impact_raw <- list(
    epm = bie_numeric_column(
      d,
      c(
        "epm",
        "estimated_plus_minus"
      )
    ),
    darko = bie_numeric_column(
      d,
      c(
        "darko",
        "darko_dpm",
        "dpm"
      )
    ),
    lebron = bie_numeric_column(
      d,
      c(
        "lebron",
        "lebron_metric"
      )
    ),
    bpm = bie_numeric_column(
      d,
      c(
        "bpm",
        "box_plus_minus"
      )
    ),
    vorp = bie_numeric_column(
      d,
      c(
        "vorp"
      )
    ),
    ws48 = bie_numeric_column(
      d,
      c(
        "ws48",
        "ws_per_48",
        "win_shares_per_48"
      )
    )
  )
  
  offense_raw <- list(
    offensive_epm = bie_numeric_column(
      d,
      c(
        "offensive_epm",
        "o_epm",
        "oepm"
      )
    ),
    offensive_bpm = bie_numeric_column(
      d,
      c(
        "obpm",
        "offensive_bpm"
      )
    ),
    offensive_rating = bie_numeric_column(
      d,
      c(
        "offensive_rating",
        "ortg"
      )
    ),
    points_per_100 = bie_numeric_column(
      d,
      c(
        "pts_per_100",
        "points_per_100"
      )
    )
  )
  
  defense_raw <- list(
    defensive_epm = bie_numeric_column(
      d,
      c(
        "defensive_epm",
        "d_epm",
        "depm"
      )
    ),
    defensive_bpm = bie_numeric_column(
      d,
      c(
        "dbpm",
        "defensive_bpm"
      )
    ),
    defensive_rating = bie_numeric_column(
      d,
      c(
        "defensive_rating",
        "drtg"
      )
    ),
    stocks = bie_numeric_column(
      d,
      c(
        "stocks_per_100",
        "stocks"
      )
    )
  )
  
  efficiency_raw <- list(
    ts = bie_numeric_column(
      d,
      c(
        "ts_pct",
        "true_shooting_pct",
        "true_shooting"
      )
    ),
    efg = bie_numeric_column(
      d,
      c(
        "efg_pct",
        "effective_fg_pct"
      )
    ),
    three_pct = bie_numeric_column(
      d,
      c(
        "three_pct",
        "three_point_pct",
        "fg3_pct",
        "x3p_pct"
      )
    )
  )
  
  playmaking_raw <- list(
    ast_pct = bie_numeric_column(
      d,
      c(
        "ast_pct",
        "assist_pct"
      )
    ),
    assists_per_100 = bie_numeric_column(
      d,
      c(
        "ast_per_100",
        "assists_per_100"
      )
    ),
    ast_to = bie_numeric_column(
      d,
      c(
        "ast_to",
        "assist_turnover_ratio"
      )
    )
  )
  
  rebounding_raw <- list(
    trb_pct = bie_numeric_column(
      d,
      c(
        "trb_pct",
        "total_rebound_pct"
      )
    ),
    rebounds_per_100 = bie_numeric_column(
      d,
      c(
        "trb_per_100",
        "reb_per_100",
        "rebounds_per_100"
      )
    )
  )
  
  availability_raw <- list(
    minutes = bie_numeric_column(
      d,
      c(
        "minutes",
        "mp",
        "minutes_per_game",
        "mpg"
      )
    ),
    games = bie_numeric_column(
      d,
      c(
        "games",
        "g",
        "games_played"
      )
    )
  )
  
  # ----------------------------------------------------------
  # Convert raw metrics to roster-relative percentiles
  # ----------------------------------------------------------
  
  mean_available_percentiles <- function(
    metrics,
    reverse = character()) {
    
    values <- lapply(
      names(metrics),
      function(metric_name) {
        
        x <- metrics[[metric_name]]
        
        if (
          all(
            !is.finite(x)
          )
        ) {
          return(NULL)
        }
        
        bie_percentile_score(
          x,
          higher_is_better =
            !metric_name %in%
            reverse
        )
      }
    )
    
    values <- values[
      !vapply(
        values,
        is.null,
        logical(1)
      )
    ]
    
    if (!length(values)) {
      return(
        rep(
          NA_real_,
          nrow(d)
        )
      )
    }
    
    matrix_value <- do.call(
      cbind,
      values
    )
    
    rowMeans(
      matrix_value,
      na.rm = TRUE
    )
  }
  
  impact_score <- mean_available_percentiles(
    impact_raw
  )
  
  offense_score <- mean_available_percentiles(
    offense_raw
  )
  
  defense_score <- mean_available_percentiles(
    defense_raw,
    reverse = c(
      "defensive_rating"
    )
  )
  
  efficiency_score <- mean_available_percentiles(
    efficiency_raw
  )
  
  playmaking_score <- mean_available_percentiles(
    playmaking_raw
  )
  
  rebounding_score <- mean_available_percentiles(
    rebounding_raw
  )
  
  availability_score <- mean_available_percentiles(
    availability_raw
  )
  
  components <- data.frame(
    impact = impact_score,
    offense = offense_score,
    defense = defense_score,
    efficiency = efficiency_score,
    playmaking = playmaking_score,
    rebounding = rebounding_score,
    availability = availability_score
  )
  
  # ----------------------------------------------------------
  # Advanced-data score
  # ----------------------------------------------------------
  
  weights <- bie_player_weight_defaults()
  
  if (!is.null(weight_overrides)) {
    
    common <- intersect(
      names(weight_overrides),
      names(weights)
    )
    
    weights[common] <- suppressWarnings(
      as.numeric(
        weight_overrides[common]
      )
    )
  }
  
  advanced_available <- rowSums(
    is.finite(
      as.matrix(
        components
      )
    )
  )
  
  advanced_score <- rep(
    NA_real_,
    nrow(d)
  )
  
  for (
    i in seq_len(
      nrow(d)
    )
  ) {
    
    values <- as.numeric(
      components[
        i,
        ,
        drop = TRUE
      ]
    )
    
    names(values) <- names(
      components
    )
    
    valid <- is.finite(values)
    
    if (!any(valid)) {
      next
    }
    
    row_weights <- weights[
      names(values)[valid]
    ]
    
    advanced_score[[i]] <- weighted.mean(
      values[valid],
      row_weights
    )
  }
  
  # ----------------------------------------------------------
  # Depth-chart fallback
  # ----------------------------------------------------------
  
  depth_order <- bie_numeric_column(
    d,
    c(
      "depth_order",
      "depth"
    )
  )
  
  starter <- bie_numeric_column(
    d,
    c(
      "is_starter",
      "starter"
    ),
    default = 0
  )
  
  salary <- bie_numeric_column(
    d,
    c(
      "cap_hit",
      "salary",
      "base_salary"
    )
  )
  
  depth_score <- rep(
    50,
    nrow(d)
  )
  
  valid_depth <- is.finite(
    depth_order
  )
  
  if (any(valid_depth)) {
    depth_score[valid_depth] <-
      basketball_intel_clamp(
        100 -
          pmin(
            depth_order[valid_depth] - 1,
            5
          ) *
          14
      )
  }
  
  starter_bonus <- ifelse(
    is.finite(starter) &
      starter > 0,
    12,
    0
  )
  
  salary_score <- bie_percentile_score(
    salary
  )
  
  fallback_score <-
    0.70 *
    depth_score +
    0.20 *
    salary_score +
    0.10 *
    basketball_intel_clamp(
      50 +
        starter_bonus
    )
  
  # If no depth and no salary information exist,
  # use a neutral score instead of pretending to know value.
  no_fallback_data <-
    !is.finite(depth_order) &
    !is.finite(salary) &
    !(is.finite(starter) & starter > 0)
  
  fallback_score[
    no_fallback_data
  ] <- 50
  
  # ----------------------------------------------------------
  # Final player score
  # ----------------------------------------------------------
  
  has_advanced <- is.finite(
    advanced_score
  )
  
  bie_score <- ifelse(
    has_advanced,
    advanced_score,
    fallback_score
  )
  
  data_source <- ifelse(
    has_advanced,
    "PERFORMANCE_DATA",
    "DEPTH_FALLBACK"
  )
  
  d$bie_player_score <-
    basketball_intel_clamp(
      bie_score
    )
  
  d$bie_score_source <-
    data_source
  
  d$bie_metric_components <-
    advanced_available
  
  d$bie_impact_score <-
    impact_score
  
  d$bie_offense_score <-
    offense_score
  
  d$bie_defense_score <-
    defense_score
  
  d$bie_efficiency_score <-
    efficiency_score
  
  d$bie_playmaking_score <-
    playmaking_score
  
  d$bie_rebounding_score <-
    rebounding_score
  
  d$bie_availability_score <-
    availability_score
  
  d
}


# ------------------------------------------------------------
# Lineup fit
# ------------------------------------------------------------

#' Score one player's fit at one lineup position
#'
#' Ranked position fit:
#'   1st / true current primary = 100
#'   2nd / strong secondary     =  90
#'   3rd / tertiary             =  75
#'   4th / emergency            =  55
#'   5th / extreme emergency    =  40
#'   not listed                 = -Inf
#'
#' This preserves versatility without treating a fourth-listed
#' position as equivalent to a true secondary position.
#' @noRd
bie_position_fit_score <- function(
    player_row,
    lineup_position) {
  
  lineup_position <- bie_normalize_position(
    lineup_position
  )
  
  ranked_positions <-
    bie_ranked_player_positions(
      player_row
    )
  
  position_rank <- match(
    lineup_position,
    ranked_positions
  )
  
  if (is.na(position_rank)) {
    return(-Inf)
  }
  
  fit_by_rank <- c(
    100,
    90,
    75,
    55,
    40
  )
  
  if (
    position_rank <=
    length(fit_by_rank)
  ) {
    return(
      fit_by_rank[[position_rank]]
    )
  }
  
  35
}


#' Calculate role-balance bonuses for a five-man lineup
#' @noRd
bie_lineup_balance <- function(lineup) {
  
  if (
    is.null(lineup) ||
    !is.data.frame(lineup) ||
    nrow(lineup) != 5
  ) {
    return(
      list(
        score = 50,
        spacing = NA_real_,
        playmaking = NA_real_,
        defense = NA_real_,
        rebounding = NA_real_,
        explanation = "Lineup balance unavailable."
      )
    )
  }
  
  component_mean <- function(column) {
    
    if (!column %in% names(lineup)) {
      return(NA_real_)
    }
    
    value <- suppressWarnings(
      as.numeric(
        lineup[[column]]
      )
    )
    
    if (!any(is.finite(value))) {
      return(NA_real_)
    }
    
    mean(
      value[
        is.finite(value)
      ]
    )
  }
  
  spacing <- component_mean(
    "bie_efficiency_score"
  )
  
  playmaking <- component_mean(
    "bie_playmaking_score"
  )
  
  defense <- component_mean(
    "bie_defense_score"
  )
  
  rebounding <- component_mean(
    "bie_rebounding_score"
  )
  
  available <- c(
    spacing = spacing,
    playmaking = playmaking,
    defense = defense,
    rebounding = rebounding
  )
  
  valid <- is.finite(
    available
  )
  
  score <- if (
    any(valid)
  ) {
    mean(
      available[valid]
    )
  } else {
    50
  }
  
  explanation <- if (
    any(valid)
  ) {
    paste0(
      "Lineup balance uses ",
      sum(valid),
      " available basketball component",
      if (
        sum(valid) == 1
      ) "" else "s",
      "."
    )
  } else {
    paste(
      "Advanced lineup-fit data is not loaded yet;",
      "the optimizer is relying on player value and positional fit."
    )
  }
  
  list(
    score =
      basketball_intel_clamp(
        score
      ),
    spacing = spacing,
    playmaking = playmaking,
    defense = defense,
    rebounding = rebounding,
    explanation = explanation
  )
}


# ------------------------------------------------------------
# Starting-five optimization
# ------------------------------------------------------------

#' Optimize a five-man starting lineup
#'
#' The optimizer assigns one unique player to each of:
#' PG, SG, SF, PF, C.
#'
#' It uses:
#' - BIE player value
#' - official/loaded position eligibility
#' - primary-position fit
#' - optional advanced lineup-balance components
#'
#' @param players Player data frame.
#' @param candidate_limit Maximum candidates considered per position.
#' @param player_weight Weight applied to player quality.
#' @param position_weight Weight applied to position fit.
#' @param balance_weight Weight applied to lineup balance.
#' @return Named BIE lineup result.
#' @noRd
optimize_bie_starting_five <- function(
    players,
    candidate_limit = 8L,
    player_weight = 0.65,
    position_weight = 0.25,
    balance_weight = 0.10) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players)
  ) {
    return(
      list(
        status = "NO_ROSTER",
        lineup = data.frame(),
        score = NA_real_,
        explanation =
          "No roster data was supplied."
      )
    )
  }
  
  candidate_limit <- max(
    2L,
    basketball_intel_integer(
      candidate_limit,
      8L
    )
  )
  
  # Normalize lineup weights so custom callers cannot accidentally
  # change the scale of the final score. Defaults are 65 / 25 / 10.
  lineup_weights <- suppressWarnings(
    as.numeric(
      c(
        player_weight,
        position_weight,
        balance_weight
      )
    )
  )
  
  if (
    length(lineup_weights) != 3L ||
    any(!is.finite(lineup_weights)) ||
    any(lineup_weights < 0) ||
    sum(lineup_weights) <= 0
  ) {
    lineup_weights <- c(
      0.65,
      0.25,
      0.10
    )
  }
  
  lineup_weights <-
    lineup_weights /
    sum(lineup_weights)
  
  player_weight <-
    lineup_weights[[1]]
  
  position_weight <-
    lineup_weights[[2]]
  
  balance_weight <-
    lineup_weights[[3]]
  
  # Candidate pre-ranking has no lineup-balance information yet,
  # so use the SAME player-vs-position ratio as the final optimizer
  # rather than the old hard-coded 82/18 split.
  candidate_weight_total <-
    player_weight +
    position_weight
  
  candidate_player_weight <-
    player_weight /
    candidate_weight_total
  
  candidate_position_weight <-
    position_weight /
    candidate_weight_total
  
  d <- bie_ensure_evaluated_players(
    players
  )
  
  lineup_positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  # Build ranked candidate lists by lineup position.
  candidates <- lapply(
    lineup_positions,
    function(pos) {
      
      fit <- vapply(
        seq_len(
          nrow(d)
        ),
        function(i) {
          bie_position_fit_score(
            d[
              i,
              ,
              drop = FALSE
            ],
            pos
          )
        },
        numeric(1)
      )
      
      eligible <- is.finite(
        fit
      )
      
      if (!any(eligible)) {
        return(
          integer()
        )
      }
      
      score <-
        candidate_player_weight *
        d$bie_player_score +
        candidate_position_weight *
        fit
      
      idx <- which(
        eligible
      )
      
      idx <- idx[
        order(
          -score[idx],
          d$player_name[idx],
          na.last = TRUE
        )
      ]
      
      head(
        idx,
        candidate_limit
      )
    }
  )
  
  names(candidates) <-
    lineup_positions
  
  missing_positions <- lineup_positions[
    !vapply(
      candidates,
      length,
      integer(1)
    )
  ]
  
  if (length(missing_positions)) {
    return(
      list(
        status = "INCOMPLETE_POSITION_COVERAGE",
        lineup = data.frame(),
        score = NA_real_,
        missing_positions =
          missing_positions,
        explanation = paste(
          "No eligible player is available for:",
          paste(
            missing_positions,
            collapse = ", "
          )
        )
      )
    )
  }
  
  best_score <- -Inf
  best_assignment <- NULL
  evaluated <- 0L
  
  # Recursive search avoids building a huge expand.grid().
  search_assignment <- function(
    position_index,
    chosen_indices,
    assigned_positions) {
    
    if (
      position_index >
      length(
        lineup_positions
      )
    ) {
      
      evaluated <<-
        evaluated + 1L
      
      lineup <- d[
        chosen_indices,
        ,
        drop = FALSE
      ]
      
      lineup$bie_lineup_position <-
        assigned_positions
      
      player_quality <- mean(
        lineup$bie_player_score,
        na.rm = TRUE
      )
      
      position_fit <- mean(
        vapply(
          seq_len(
            nrow(lineup)
          ),
          function(i) {
            bie_position_fit_score(
              lineup[
                i,
                ,
                drop = FALSE
              ],
              lineup$bie_lineup_position[[i]]
            )
          },
          numeric(1)
        ),
        na.rm = TRUE
      )
      
      balance <- bie_lineup_balance(
        lineup
      )
      
      score <-
        player_weight *
        player_quality +
        position_weight *
        position_fit +
        balance_weight *
        balance$score
      
      score <-
        basketball_intel_clamp(
          score
        )
      
      if (score > best_score) {
        
        best_score <<-
          score
        
        best_assignment <<-
          list(
            lineup = lineup,
            player_quality =
              player_quality,
            position_fit =
              position_fit,
            balance =
              balance
          )
      }
      
      return(
        invisible(NULL)
      )
    }
    
    pos <- lineup_positions[
      position_index
    ]
    
    for (
      candidate_index in
      candidates[[pos]]
    ) {
      
      if (
        candidate_index %in%
        chosen_indices
      ) {
        next
      }
      
      search_assignment(
        position_index =
          position_index + 1L,
        chosen_indices =
          c(
            chosen_indices,
            candidate_index
          ),
        assigned_positions =
          c(
            assigned_positions,
            pos
          )
      )
    }
    
    invisible(NULL)
  }
  
  search_assignment(
    position_index = 1L,
    chosen_indices = integer(),
    assigned_positions = character()
  )
  
  if (is.null(best_assignment)) {
    
    return(
      list(
        status = "NO_LEGAL_LINEUP",
        lineup = data.frame(),
        score = NA_real_,
        evaluated_lineups =
          evaluated,
        explanation = paste(
          "Position eligibility exists individually,",
          "but no unique five-player assignment could be created."
        )
      )
    )
  }
  
  lineup <- best_assignment$lineup
  
  lineup <- lineup[
    match(
      lineup_positions,
      lineup$bie_lineup_position
    ),
    ,
    drop = FALSE
  ]
  
  rownames(lineup) <- NULL
  
  source_counts <- table(
    lineup$bie_score_source
  )
  
  performance_players <- if (
    "PERFORMANCE_DATA" %in%
    names(source_counts)
  ) {
    as.integer(
      source_counts[[
        "PERFORMANCE_DATA"
      ]]
    )
  } else {
    0L
  }
  
  confidence <- if (
    performance_players == 5L
  ) {
    "HIGH"
  } else if (
    performance_players >= 3L
  ) {
    "MODERATE"
  } else {
    "FOUNDATION"
  }
  
  explanation <- if (
    performance_players == 0L
  ) {
    paste(
      "Starting Five is optimized from roster hierarchy,",
      "position eligibility, and loaded contract/depth signals.",
      "Advanced player-performance data has not been loaded yet."
    )
  } else {
    paste0(
      "Starting Five uses performance data for ",
      performance_players,
      " of 5 players, with positional-fit and lineup-balance adjustments."
    )
  }
  
  list(
    status = "OK",
    lineup = lineup,
    score = best_score,
    player_quality =
      best_assignment$player_quality,
    position_fit =
      best_assignment$position_fit,
    balance =
      best_assignment$balance,
    evaluated_lineups =
      evaluated,
    performance_data_players =
      performance_players,
    confidence = confidence,
    explanation = explanation,
    model_label =
      "BIE Starting Five v1"
  )
}


# ------------------------------------------------------------
# Lineup comparison
# ------------------------------------------------------------

#' Compare two optimized BIE lineups
#' @noRd
compare_bie_lineups <- function(
    lineup_a,
    lineup_b,
    label_a = "Current",
    label_b = "Proposed") {
  
  score_a <- basketball_intel_number(
    lineup_a$score
  )
  
  score_b <- basketball_intel_number(
    lineup_b$score
  )
  
  delta <- if (
    is.na(score_a) ||
    is.na(score_b)
  ) {
    NA_real_
  } else {
    score_b -
      score_a
  }
  
  direction <- if (
    is.na(delta)
  ) {
    "Unavailable"
  } else if (
    delta >= 5
  ) {
    "Material Improvement"
  } else if (
    delta >= 1.5
  ) {
    "Improvement"
  } else if (
    delta > -1.5
  ) {
    "Roughly Neutral"
  } else if (
    delta > -5
  ) {
    "Decline"
  } else {
    "Material Decline"
  }
  
  list(
    label_a = label_a,
    label_b = label_b,
    score_a = score_a,
    score_b = score_b,
    delta = delta,
    direction = direction,
    lineup_a = lineup_a$lineup %||%
      data.frame(),
    lineup_b = lineup_b$lineup %||%
      data.frame(),
    explanation = if (
      is.na(delta)
    ) {
      "A valid score was not available for both lineups."
    } else {
      paste0(
        label_b,
        " lineup is ",
        if (
          delta >= 0
        ) "+" else "",
        round(
          delta,
          1
        ),
        " BIE points versus ",
        label_a,
        "."
      )
    }
  )
}




# ============================================================
# PHASE 2 — STEP 3
# Explainable Basketball Intelligence
# ============================================================

#' Safe lineup component value
#' @noRd
bie_lineup_component_value <- function(
    lineup_result,
    component,
    default = NA_real_) {
  
  if (
    is.null(lineup_result)
  ) {
    return(default)
  }
  
  if (
    component %in%
    c(
      "player_quality",
      "position_fit",
      "score"
    )
  ) {
    
    value <- lineup_result[[component]]
    
  } else {
    
    balance <- lineup_result$balance
    
    value <- if (
      !is.null(balance)
    ) {
      balance[[component]]
    } else {
      NULL
    }
  }
  
  basketball_intel_number(
    value,
    default
  )
}


#' Classify one lineup component score
#' @noRd
bie_component_band <- function(score) {
  
  score <- basketball_intel_number(
    score
  )
  
  if (is.na(score)) {
    return("Unavailable")
  }
  
  if (score >= 80) {
    return("Elite")
  }
  
  if (score >= 68) {
    return("Strong")
  }
  
  if (score >= 55) {
    return("Positive")
  }
  
  if (score >= 45) {
    return("Neutral")
  }
  
  if (score >= 32) {
    return("Concern")
  }
  
  "Major Concern"
}


#' Convert a component key to executive basketball language
#' @noRd
bie_component_label <- function(component) {
  
  labels <- c(
    player_quality = "Player Quality",
    position_fit = "Position Fit",
    spacing = "Scoring Efficiency / Spacing",
    playmaking = "Playmaking",
    defense = "Defense",
    rebounding = "Rebounding",
    score = "Overall BIE Lineup Score"
  )
  
  label <- labels[[component]]
  
  if (
    is.null(label)
  ) {
    component
  } else {
    label
  }
}


#' Build strengths and concerns for one BIE lineup
#'
#' Only components backed by loaded performance data are described
#' as basketball strengths/concerns. FOUNDATION-only lineups instead
#' explain that depth and position eligibility are driving the model.
#'
#' @noRd
explain_bie_lineup <- function(
    lineup_result) {
  
  if (
    is.null(lineup_result) ||
    !identical(
      lineup_result$status %||% "OK",
      "OK"
    )
  ) {
    return(
      list(
        available = FALSE,
        strengths = character(),
        concerns = character(),
        component_table = data.frame(),
        explanation =
          "A valid BIE lineup result is required."
      )
    )
  }
  
  confidence <- basketball_intel_text(
    lineup_result$confidence,
    "FOUNDATION"
  )
  
  performance_players <- basketball_intel_integer(
    lineup_result$performance_data_players,
    0L
  )
  
  components <- c(
    player_quality =
      bie_lineup_component_value(
        lineup_result,
        "player_quality"
      ),
    position_fit =
      bie_lineup_component_value(
        lineup_result,
        "position_fit"
      ),
    spacing =
      bie_lineup_component_value(
        lineup_result,
        "spacing"
      ),
    playmaking =
      bie_lineup_component_value(
        lineup_result,
        "playmaking"
      ),
    defense =
      bie_lineup_component_value(
        lineup_result,
        "defense"
      ),
    rebounding =
      bie_lineup_component_value(
        lineup_result,
        "rebounding"
      )
  )
  
  component_table <- data.frame(
    component = names(components),
    label = vapply(
      names(components),
      bie_component_label,
      character(1)
    ),
    score = as.numeric(
      components
    ),
    band = vapply(
      components,
      bie_component_band,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  
  strengths <- character()
  concerns <- character()
  
  # Position fit is always inspectable because it comes from
  # loaded roster eligibility rather than inferred performance.
  position_fit <- components[[
    "position_fit"
  ]]
  
  if (
    is.finite(position_fit) &&
    position_fit >= 92
  ) {
    strengths <- c(
      strengths,
      paste0(
        "Strong positional alignment (",
        round(position_fit, 1),
        ")."
      )
    )
  } else if (
    is.finite(position_fit) &&
    position_fit < 80
  ) {
    concerns <- c(
      concerns,
      paste0(
        "Lineup relies on secondary-position assignments (",
        round(position_fit, 1),
        ")."
      )
    )
  }
  
  advanced_components <- c(
    spacing = components[["spacing"]],
    playmaking =
      components[["playmaking"]],
    defense =
      components[["defense"]],
    rebounding =
      components[["rebounding"]]
  )
  
  if (performance_players > 0L) {
    
    for (
      component in names(
        advanced_components
      )
    ) {
      
      value <- advanced_components[[
        component
      ]]
      
      if (!is.finite(value)) {
        next
      }
      
      label <- bie_component_label(
        component
      )
      
      if (value >= 68) {
        strengths <- c(
          strengths,
          paste0(
            label,
            " grades as ",
            tolower(
              bie_component_band(
                value
              )
            ),
            " (",
            round(value, 1),
            ")."
          )
        )
      }
      
      if (value < 45) {
        concerns <- c(
          concerns,
          paste0(
            label,
            " is a current lineup concern (",
            round(value, 1),
            ")."
          )
        )
      }
    }
  }
  
  player_quality <- components[[
    "player_quality"
  ]]
  
  if (
    performance_players > 0L &&
    is.finite(player_quality) &&
    player_quality >= 68
  ) {
    strengths <- c(
      strengths,
      paste0(
        "Overall player-quality signal is strong (",
        round(player_quality, 1),
        ")."
      )
    )
  }
  
  if (!length(strengths)) {
    
    strengths <- if (
      identical(
        confidence,
        "FOUNDATION"
      )
    ) {
      "The recommendation maximizes the strongest available depth-chart and position-fit combination."
    } else {
      "No single loaded component currently stands out as a major strength."
    }
  }
  
  if (!length(concerns)) {
    
    concerns <- if (
      identical(
        confidence,
        "FOUNDATION"
      )
    ) {
      paste(
        "Advanced performance data is not loaded for enough players",
        "to make evidence-based spacing, playmaking, defensive, or rebounding claims."
      )
    } else {
      "No major weakness is identified by the currently loaded lineup components."
    }
  }
  
  explanation <- if (
    identical(
      confidence,
      "FOUNDATION"
    )
  ) {
    paste(
      "This is a FOUNDATION recommendation.",
      "BIE is optimizing player hierarchy and legal position coverage.",
      "Performance-based lineup claims will activate automatically",
      "when advanced player metrics are loaded."
    )
  } else {
    paste0(
      "BIE uses performance data for ",
      performance_players,
      " of 5 players and combines player quality, position fit,",
      " and available lineup-balance components."
    )
  }
  
  list(
    available = TRUE,
    confidence = confidence,
    performance_data_players =
      performance_players,
    strengths = unique(strengths),
    concerns = unique(concerns),
    component_table =
      component_table,
    explanation = explanation
  )
}


#' Compare basketball component changes between two lineup results
#'
#' @noRd
compare_bie_lineup_components <- function(
    current_result,
    recommended_result) {
  
  components <- c(
    "player_quality",
    "position_fit",
    "spacing",
    "playmaking",
    "defense",
    "rebounding"
  )
  
  current_values <- vapply(
    components,
    function(component) {
      bie_lineup_component_value(
        current_result,
        component
      )
    },
    numeric(1)
  )
  
  recommended_values <- vapply(
    components,
    function(component) {
      bie_lineup_component_value(
        recommended_result,
        component
      )
    },
    numeric(1)
  )
  
  delta <- recommended_values -
    current_values
  
  result <- data.frame(
    component = components,
    label = vapply(
      components,
      bie_component_label,
      character(1)
    ),
    current = current_values,
    recommended =
      recommended_values,
    delta = delta,
    stringsAsFactors = FALSE
  )
  
  result$available <-
    is.finite(
      result$current
    ) &
    is.finite(
      result$recommended
    )
  
  result$direction <- ifelse(
    !result$available,
    "Unavailable",
    ifelse(
      result$delta >= 3,
      "Improved",
      ifelse(
        result$delta <= -3,
        "Declined",
        "Stable"
      )
    )
  )
  
  result
}


#' Build an executive "why the lineup changed" summary
#' @noRd
explain_bie_lineup_change <- function(
    current_result,
    recommended_result) {
  
  comparison <-
    compare_bie_lineup_components(
      current_result,
      recommended_result
    )
  
  available <- comparison[
    comparison$available,
    ,
    drop = FALSE
  ]
  
  if (!nrow(available)) {
    return(
      list(
        component_table = comparison,
        biggest_improvement = NULL,
        biggest_tradeoff = NULL,
        summary = paste(
          "The recommended lineup differs because BIE found a",
          "stronger player-value / position-fit assignment.",
          "Detailed performance-component changes are unavailable",
          "until advanced player data is loaded."
        )
      )
    )
  }
  
  positive <- available[
    available$delta > 0,
    ,
    drop = FALSE
  ]
  
  negative <- available[
    available$delta < 0,
    ,
    drop = FALSE
  ]
  
  biggest_improvement <- if (
    nrow(positive)
  ) {
    positive[
      which.max(
        positive$delta
      ),
      ,
      drop = FALSE
    ]
  } else {
    NULL
  }
  
  biggest_tradeoff <- if (
    nrow(negative)
  ) {
    negative[
      which.min(
        negative$delta
      ),
      ,
      drop = FALSE
    ]
  } else {
    NULL
  }
  
  summary_parts <- character()
  
  if (
    !is.null(
      biggest_improvement
    )
  ) {
    summary_parts <- c(
      summary_parts,
      paste0(
        "Largest improvement: ",
        biggest_improvement$label[[1]],
        " ",
        sprintf(
          "%+.1f",
          biggest_improvement$delta[[1]]
        ),
        "."
      )
    )
  }
  
  if (
    !is.null(
      biggest_tradeoff
    )
  ) {
    summary_parts <- c(
      summary_parts,
      paste0(
        "Largest tradeoff: ",
        biggest_tradeoff$label[[1]],
        " ",
        sprintf(
          "%+.1f",
          biggest_tradeoff$delta[[1]]
        ),
        "."
      )
    )
  }
  
  if (!length(summary_parts)) {
    summary_parts <-
      "The two lineups grade similarly across the currently available components."
  }
  
  list(
    component_table = comparison,
    biggest_improvement =
      biggest_improvement,
    biggest_tradeoff =
      biggest_tradeoff,
    summary = paste(
      summary_parts,
      collapse = " "
    )
  )
}




# ============================================================
# PHASE 2 — STEP 4
# Player Fit Intelligence
# ============================================================

#' Convert a BIE player score to a display grade
#' @noRd
bie_player_grade <- function(score) {
  
  score <- basketball_intel_number(
    score
  )
  
  if (is.na(score)) {
    return("UNRATED")
  }
  
  if (score >= 94) return("A+")
  if (score >= 88) return("A")
  if (score >= 82) return("A-")
  if (score >= 76) return("B+")
  if (score >= 70) return("B")
  if (score >= 64) return("B-")
  if (score >= 58) return("C+")
  if (score >= 52) return("C")
  if (score >= 46) return("C-")
  if (score >= 40) return("D+")
  
  "D"
}


#' Classify player-profile confidence
#' @noRd
bie_player_profile_confidence <- function(
    metric_components) {
  
  components <- basketball_intel_integer(
    metric_components,
    0L
  )
  
  if (components >= 5L) {
    return("HIGH")
  }
  
  if (components >= 3L) {
    return("MODERATE")
  }
  
  if (components >= 1L) {
    return("LIMITED")
  }
  
  "FOUNDATION"
}


#' Age-curve label for player fit
#' @noRd
bie_player_age_curve <- function(age) {
  
  age <- basketball_intel_number(
    age
  )
  
  if (is.na(age)) {
    return("Age data unavailable")
  }
  
  if (age <= 22) {
    return("Development")
  }
  
  if (age <= 25) {
    return("Ascending")
  }
  
  if (age <= 29) {
    return("Prime")
  }
  
  if (age <= 32) {
    return("Veteran Prime")
  }
  
  "Late Veteran"
}


#' Archetype from loaded BIE performance components
#'
#' This is deliberately conservative. No performance archetype
#' is assigned when the underlying basketball metrics are absent.
#' @noRd
bie_player_archetype <- function(player_row) {
  
  if (
    is.null(player_row) ||
    !is.data.frame(player_row) ||
    !nrow(player_row)
  ) {
    return("Unavailable")
  }
  
  metric_components <- basketball_intel_integer(
    player_row$bie_metric_components,
    0L
  )
  
  if (metric_components == 0L) {
    return("Performance Archetype Pending")
  }
  
  get_score <- function(column) {
    if (!column %in% names(player_row)) {
      return(NA_real_)
    }
    
    basketball_intel_number(
      player_row[[column]][[1]]
    )
  }
  
  offense <- get_score(
    "bie_offense_score"
  )
  
  defense <- get_score(
    "bie_defense_score"
  )
  
  efficiency <- get_score(
    "bie_efficiency_score"
  )
  
  playmaking <- get_score(
    "bie_playmaking_score"
  )
  
  rebounding <- get_score(
    "bie_rebounding_score"
  )
  
  primary <- bie_primary_position(
    player_row
  )
  
  if (
    is.finite(offense) &&
    offense >= 70 &&
    is.finite(playmaking) &&
    playmaking >= 65
  ) {
    return("Primary Creator")
  }
  
  if (
    is.finite(offense) &&
    offense >= 65 &&
    is.finite(defense) &&
    defense >= 65
  ) {
    return("Two-Way Impact Player")
  }
  
  if (
    is.finite(efficiency) &&
    efficiency >= 70 &&
    primary %in% c(
      "PG",
      "SG",
      "SF",
      "PF"
    )
  ) {
    return("Efficient Scoring / Spacing Threat")
  }
  
  if (
    is.finite(defense) &&
    defense >= 70
  ) {
    return("Defensive Impact Player")
  }
  
  if (
    is.finite(rebounding) &&
    rebounding >= 70 &&
    primary %in% c(
      "PF",
      "C"
    )
  ) {
    return("Interior / Rebounding Big")
  }
  
  if (
    is.finite(playmaking) &&
    playmaking >= 70
  ) {
    return("Connector / Playmaker")
  }
  
  "Balanced Rotation Profile"
}


#' Build strengths and concerns for one player profile
#' @noRd
bie_player_strengths_concerns <- function(
    player_row) {
  
  if (
    is.null(player_row) ||
    !is.data.frame(player_row) ||
    !nrow(player_row)
  ) {
    return(
      list(
        strengths = character(),
        concerns = character()
      )
    )
  }
  
  component_map <- c(
    bie_impact_score = "Overall Impact",
    bie_offense_score = "Offense",
    bie_defense_score = "Defense",
    bie_efficiency_score = "Efficiency / Spacing",
    bie_playmaking_score = "Playmaking",
    bie_rebounding_score = "Rebounding",
    bie_availability_score = "Availability"
  )
  
  strengths <- character()
  concerns <- character()
  
  for (
    column in names(
      component_map
    )
  ) {
    
    if (!column %in% names(player_row)) {
      next
    }
    
    value <- basketball_intel_number(
      player_row[[column]][[1]]
    )
    
    if (is.na(value)) {
      next
    }
    
    label <- component_map[[column]]
    
    if (value >= 70) {
      strengths <- c(
        strengths,
        paste0(
          label,
          " is a strong team-relative signal (",
          round(value, 1),
          ")."
        )
      )
    }
    
    if (value < 40) {
      concerns <- c(
        concerns,
        paste0(
          label,
          " is below the preferred team-relative range (",
          round(value, 1),
          ")."
        )
      )
    }
  }
  
  if (!length(strengths)) {
    strengths <-
      "No performance-based strength is identified from the currently loaded metrics."
  }
  
  if (!length(concerns)) {
    concerns <-
      "No major performance concern is identified from the currently loaded metrics."
  }
  
  list(
    strengths = unique(strengths),
    concerns = unique(concerns)
  )
}


#' Evaluate one player's fit within a roster
#'
#' `roster_players` should contain the same metric columns as the
#' selected player whenever possible. Scores are roster-relative;
#' they are not league-wide grades unless league-wide comparison
#' data is supplied as the roster.
#'
#' @noRd
evaluate_bie_player_fit <- function(
    player_id,
    roster_players) {
  
  if (
    is.null(roster_players) ||
    !is.data.frame(roster_players) ||
    !nrow(roster_players)
  ) {
    return(
      list(
        status = "NO_ROSTER",
        score = NA_real_,
        grade = "UNRATED",
        confidence = "FOUNDATION",
        explanation =
          "No roster data was supplied for Player Fit Intelligence."
      )
    )
  }
  
  if (
    !"player_id" %in%
    names(roster_players)
  ) {
    return(
      list(
        status = "NO_PLAYER_ID",
        score = NA_real_,
        grade = "UNRATED",
        confidence = "FOUNDATION",
        explanation =
          "Player Fit Intelligence requires player_id."
      )
    )
  }
  
  evaluated <- if (
    "bie_player_score" %in%
    names(roster_players)
  ) {
    roster_players
  } else {
    evaluate_bie_players(
      roster_players
    )
  }
  
  selected_id <- suppressWarnings(
    as.integer(player_id)
  )
  
  row <- evaluated[
    suppressWarnings(
      as.integer(
        evaluated$player_id
      )
    ) == selected_id,
    ,
    drop = FALSE
  ]
  
  if (!nrow(row)) {
    return(
      list(
        status = "PLAYER_NOT_FOUND",
        score = NA_real_,
        grade = "UNRATED",
        confidence = "FOUNDATION",
        explanation =
          "The selected player is not present in the evaluated roster."
      )
    )
  }
  
  row <- row[
    1,
    ,
    drop = FALSE
  ]
  
  metric_components <- basketball_intel_integer(
    row$bie_metric_components,
    0L
  )
  
  confidence <-
    bie_player_profile_confidence(
      metric_components
    )
  
  performance_available <-
    metric_components > 0L &&
    identical(
      basketball_intel_text(
        row$bie_score_source,
        ""
      ),
      "PERFORMANCE_DATA"
    )
  
  score <- if (
    performance_available
  ) {
    basketball_intel_number(
      row$bie_player_score
    )
  } else {
    NA_real_
  }
  
  grade <- bie_player_grade(
    score
  )
  
  positions <- bie_player_positions(
    row
  )
  
  primary <- bie_primary_position(
    row
  )
  
  secondary <- setdiff(
    positions,
    primary
  )
  
  secondary_label <- if (
    length(secondary)
  ) {
    paste(
      secondary,
      collapse = " / "
    )
  } else {
    "None loaded"
  }
  
  age_column <- bie_find_column(
    row,
    c(
      "player_age",
      "age"
    )
  )
  
  age <- if (
    is.null(age_column)
  ) {
    NA_real_
  } else {
    basketball_intel_number(
      row[[age_column]][[1]]
    )
  }
  
  profile_notes <-
    bie_player_strengths_concerns(
      row
    )
  
  ranked <- evaluated[
    evaluated$bie_score_source ==
      "PERFORMANCE_DATA" &
      is.finite(
        suppressWarnings(
          as.numeric(
            evaluated$bie_player_score
          )
        )
      ),
    ,
    drop = FALSE
  ]
  
  roster_rank <- NA_integer_
  rated_players <- nrow(ranked)
  
  if (
    performance_available &&
    rated_players
  ) {
    
    ranked <- ranked[
      order(
        -suppressWarnings(
          as.numeric(
            ranked$bie_player_score
          )
        ),
        ranked$player_name,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
    
    roster_rank <- match(
      selected_id,
      suppressWarnings(
        as.integer(
          ranked$player_id
        )
      )
    )
  }
  
  explanation <- if (
    performance_available
  ) {
    paste0(
      "Player Fit Intelligence is using ",
      metric_components,
      " loaded basketball component",
      if (
        metric_components == 1
      ) "" else "s",
      ". The BIE score and roster rank are team-relative."
    )
  } else {
    paste(
      "Performance-grade data is not loaded for this player.",
      "BIE will show position, age-curve, roster context, and contract information,",
      "but it will not manufacture an offense, defense, or overall basketball grade."
    )
  }
  
  list(
    status = "OK",
    player_id = selected_id,
    player_name =
      basketball_intel_text(
        row$player_name,
        "Player"
      ),
    score = score,
    grade = grade,
    confidence = confidence,
    performance_available =
      performance_available,
    metric_components =
      metric_components,
    roster_rank = roster_rank,
    rated_players = rated_players,
    primary_position = primary,
    secondary_positions =
      secondary_label,
    position_versatility =
      length(positions),
    age_curve =
      bie_player_age_curve(
        age
      ),
    archetype =
      bie_player_archetype(
        row
      ),
    impact =
      basketball_intel_number(
        row$bie_impact_score
      ),
    offense =
      basketball_intel_number(
        row$bie_offense_score
      ),
    defense =
      basketball_intel_number(
        row$bie_defense_score
      ),
    efficiency =
      basketball_intel_number(
        row$bie_efficiency_score
      ),
    playmaking =
      basketball_intel_number(
        row$bie_playmaking_score
      ),
    rebounding =
      basketball_intel_number(
        row$bie_rebounding_score
      ),
    availability =
      basketball_intel_number(
        row$bie_availability_score
      ),
    strengths =
      profile_notes$strengths,
    concerns =
      profile_notes$concerns,
    explanation = explanation,
    model_label =
      "BIE Player Fit v1"
  )
}




# ============================================================
# PHASE 2 — STEP 5
# Rotation Intelligence
# ============================================================

#' Allocate 240 regulation minutes across a BIE rotation
#'
#' Minutes are a decision-support recommendation, not a coaching
#' mandate. The allocator respects starter/bench minute bands and
#' always returns a regulation total when the requested rotation is
#' feasible.
#' @noRd
allocate_bie_rotation_minutes <- function(
    rotation,
    total_minutes = 240) {
  
  if (
    is.null(rotation) ||
    !is.data.frame(rotation) ||
    !nrow(rotation)
  ) {
    return(numeric())
  }
  
  n <- nrow(rotation)
  
  starter_flag <- if (
    "bie_rotation_role" %in%
    names(rotation)
  ) {
    rotation$bie_rotation_role ==
      "STARTER"
  } else {
    seq_len(n) <= min(5L, n)
  }
  
  score <- if (
    "bie_player_score" %in%
    names(rotation)
  ) {
    suppressWarnings(
      as.numeric(
        rotation$bie_player_score
      )
    )
  } else {
    rep(50, n)
  }
  
  score[
    !is.finite(score)
  ] <- 50
  
  weights <-
    pmax(
      0.25,
      score / 100
    ) *
    ifelse(
      starter_flag,
      1.35,
      0.82
    )
  
  minimum <- ifelse(
    starter_flag,
    28,
    8
  )
  
  maximum <- ifelse(
    starter_flag,
    38,
    30
  )
  
  if (
    sum(minimum) >
    total_minutes ||
    sum(maximum) <
    total_minutes
  ) {
    # Fall back to a proportional allocation if the requested
    # rotation size makes the standard bands infeasible.
    minutes <-
      total_minutes *
      weights /
      sum(weights)
    
    return(
      round(
        minutes,
        1
      )
    )
  }
  
  minutes <- minimum
  capacity <- maximum -
    minimum
  
  remaining <-
    total_minutes -
    sum(minutes)
  
  active <- capacity > 1e-8
  
  guard <- 0L
  
  while (
    remaining > 1e-8 &&
    any(active) &&
    guard < 100L
  ) {
    
    guard <- guard + 1L
    
    active_weights <- weights
    active_weights[
      !active
    ] <- 0
    
    weight_sum <- sum(
      active_weights
    )
    
    if (
      !is.finite(weight_sum) ||
      weight_sum <= 0
    ) {
      active_weights[
        active
      ] <- 1
      
      weight_sum <- sum(
        active_weights
      )
    }
    
    proposed <-
      remaining *
      active_weights /
      weight_sum
    
    added <- pmin(
      proposed,
      capacity
    )
    
    minutes <- minutes +
      added
    
    capacity <- capacity -
      added
    
    remaining <- remaining -
      sum(added)
    
    active <- capacity > 1e-8
  }
  
  if (
    abs(
      total_minutes -
      sum(minutes)
    ) >
    1e-6
  ) {
    
    difference <-
      total_minutes -
      sum(minutes)
    
    index <- which.max(
      maximum -
        minutes
    )
    
    minutes[[index]] <-
      minutes[[index]] +
      difference
  }
  
  rounded <- round(
    minutes,
    1
  )
  
  rounding_gap <-
    total_minutes -
    sum(rounded)
  
  if (
    abs(rounding_gap) >= 0.05
  ) {
    rounded[[1]] <-
      rounded[[1]] +
      rounding_gap
  }
  
  rounded
}


#' Optimize an NBA rotation
#'
#' Starts with the optimized BIE Starting Five, then selects the
#' best available bench players using player quality, versatility,
#' and loaded depth-chart information. Advanced metrics automatically
#' strengthen the model when available.
#'
#' @noRd
optimize_bie_rotation <- function(
    players,
    rotation_size = 9L,
    total_minutes = 240L,
    approved_lineup = NULL) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players)
  ) {
    return(
      list(
        status = "NO_ROSTER",
        rotation = data.frame(),
        score = NA_real_,
        confidence = "FOUNDATION",
        explanation =
          "No roster data was supplied."
      )
    )
  }
  
  rotation_size <- basketball_intel_integer(
    rotation_size,
    9L
  )
  
  rotation_size <- max(
    5L,
    min(
      10L,
      rotation_size
    )
  )
  
  total_minutes <- basketball_intel_integer(
    total_minutes,
    240L
  )
  
  d <- bie_ensure_evaluated_players(
    players
  )
  
  if (
    nrow(d) <
    rotation_size
  ) {
    rotation_size <- nrow(d)
  }
  
  lineup_positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  # ----------------------------------------------------------
  # Starting Five source
  # ----------------------------------------------------------
  # If the application supplies an approved/working lineup,
  # Rotation Intelligence MUST preserve it. BIE then builds the
  # bench and minutes around that lineup instead of replacing it.
  # ----------------------------------------------------------
  
  use_approved <- FALSE
  starter_rows <- data.frame()
  
  if (
    !is.null(approved_lineup)
  ) {
    
    approved <- approved_lineup[
      lineup_positions
    ]
    
    approved_ids <- suppressWarnings(
      as.integer(
        approved
      )
    )
    
    valid_approved <-
      length(approved_ids) == 5L &&
      all(
        !is.na(
          approved_ids
        )
      ) &&
      length(
        unique(
          approved_ids
        )
      ) == 5L
    
    if (valid_approved) {
      
      starter_index <- match(
        approved_ids,
        suppressWarnings(
          as.integer(
            d$player_id
          )
        )
      )
      
      if (
        all(
          !is.na(
            starter_index
          )
        )
      ) {
        
        starter_rows <- d[
          starter_index,
          ,
          drop = FALSE
        ]
        
        starter_rows$
          bie_lineup_position <-
          lineup_positions
        
        use_approved <- TRUE
      }
    }
  }
  
  if (!use_approved) {
    
    starters <- optimize_bie_starting_five(
      d
    )
    
    if (
      is.null(starters) ||
      !identical(
        starters$status,
        "OK"
      )
    ) {
      return(
        list(
          status =
            "STARTING_FIVE_UNAVAILABLE",
          rotation = data.frame(),
          score = NA_real_,
          confidence = "FOUNDATION",
          explanation = if (
            !is.null(
              starters$explanation
            )
          ) {
            starters$explanation
          } else {
            "BIE could not create a legal Starting Five."
          }
        )
      )
    }
    
    starter_rows <- starters$lineup
  }
  
  starter_ids <- suppressWarnings(
    as.integer(
      starter_rows$player_id
    )
  )
  
  # ----------------------------------------------------------
  # Bench pool
  # ----------------------------------------------------------
  
  bench <- d[
    !suppressWarnings(
      as.integer(
        d$player_id
      )
    ) %in%
      starter_ids,
    ,
    drop = FALSE
  ]
  
  bench_needed <-
    max(
      0L,
      rotation_size - 5L
    )
  
  # Determine whether the roster has meaningful performance data.
  performance_count <- if (
    "bie_score_source" %in%
    names(d)
  ) {
    sum(
      d$bie_score_source ==
        "PERFORMANCE_DATA",
      na.rm = TRUE
    )
  } else {
    0L
  }
  
  foundation_mode <-
    performance_count <
    ceiling(
      rotation_size / 2
    )
  
  if (
    bench_needed > 0L &&
    nrow(bench)
  ) {
    
    versatility <- vapply(
      seq_len(
        nrow(bench)
      ),
      function(i) {
        length(
          bie_player_positions(
            bench[
              i,
              ,
              drop = FALSE
            ]
          )
        )
      },
      integer(1)
    )
    
    depth_order <- bie_numeric_column(
      bench,
      c(
        "depth_order",
        "depth"
      )
    )
    
    depth_component <- rep(
      50,
      nrow(bench)
    )
    
    valid_depth <- is.finite(
      depth_order
    )
    
    if (any(valid_depth)) {
      
      depth_component[
        valid_depth
      ] <-
        basketball_intel_clamp(
          100 -
            pmin(
              depth_order[
                valid_depth
              ] - 1,
              6
            ) *
            15
        )
    }
    
    player_score <- suppressWarnings(
      as.numeric(
        bench$bie_player_score
      )
    )
    
    player_score[
      !is.finite(
        player_score
      )
    ] <- 50
    
    versatility_score <-
      basketball_intel_clamp(
        45 +
          14 *
          pmax(
            0,
            versatility - 1
          )
      )
    
    # FOUNDATION mode intentionally trusts the existing depth chart
    # more than the fallback BIE player score.
    bench$bie_rotation_selection_score <- if (
      foundation_mode
    ) {
      0.65 *
        depth_component +
        0.20 *
        player_score +
        0.15 *
        versatility_score
    } else {
      0.68 *
        player_score +
        0.17 *
        depth_component +
        0.15 *
        versatility_score
    }
    
    bench <- bench[
      order(
        -bench$
          bie_rotation_selection_score,
        depth_order,
        bench$player_name,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
    
    bench <- head(
      bench,
      bench_needed
    )
    
  } else {
    
    bench <- bench[
      0,
      ,
      drop = FALSE
    ]
  }
  
  # ----------------------------------------------------------
  # Rotation roles / slots
  # ----------------------------------------------------------
  
  starter_rows$bie_rotation_role <-
    "STARTER"
  
  starter_rows$bie_rotation_slot <-
    starter_rows$
    bie_lineup_position
  
  if (nrow(bench)) {
    
    bench$bie_rotation_role <-
      "BENCH"
    
    bench$bie_rotation_slot <-
      vapply(
        seq_len(
          nrow(bench)
        ),
        function(i) {
          
          positions <- bie_player_positions(
            bench[
              i,
              ,
              drop = FALSE
            ]
          )
          
          if (length(positions)) {
            paste(
              positions,
              collapse = "/"
            )
          } else {
            "UTILITY"
          }
        },
        character(1)
      )
  }
  
  all_columns <- union(
    names(starter_rows),
    names(bench)
  )
  
  for (
    column in setdiff(
      all_columns,
      names(starter_rows)
    )
  ) {
    starter_rows[[column]] <- NA
  }
  
  for (
    column in setdiff(
      all_columns,
      names(bench)
    )
  ) {
    bench[[column]] <- NA
  }
  
  starter_rows <- starter_rows[
    ,
    all_columns,
    drop = FALSE
  ]
  
  bench <- bench[
    ,
    all_columns,
    drop = FALSE
  ]
  
  rotation <- rbind(
    starter_rows,
    bench
  )
  
  # ----------------------------------------------------------
  # Minutes
  # ----------------------------------------------------------
  # In FOUNDATION mode we do not pretend the fallback score can
  # distinguish 37 minutes from 31 minutes with precision.
  # Starters are therefore kept in a realistic shared band and
  # bench minutes are allocated around them.
  # ----------------------------------------------------------
  
  if (foundation_mode) {
    
    starter_n <- sum(
      rotation$bie_rotation_role ==
        "STARTER"
    )
    
    bench_n <- sum(
      rotation$bie_rotation_role ==
        "BENCH"
    )
    
    starter_total <- min(
      174,
      total_minutes -
        8 * bench_n
    )
    
    starter_total <- max(
      starter_total,
      30 * starter_n
    )
    
    bench_total <-
      total_minutes -
      starter_total
    
    starter_minutes <- rep(
      starter_total /
        starter_n,
      starter_n
    )
    
    if (bench_n > 0L) {
      
      bench_rows <- rotation[
        rotation$bie_rotation_role ==
          "BENCH",
        ,
        drop = FALSE
      ]
      
      bench_depth <- bie_numeric_column(
        bench_rows,
        c(
          "depth_order",
          "depth"
        )
      )
      
      bench_weights <- ifelse(
        is.finite(
          bench_depth
        ),
        pmax(
          0.35,
          1.25 -
            0.13 *
            pmax(
              0,
              bench_depth - 1
            )
        ),
        0.75
      )
      
      bench_minutes <-
        bench_total *
        bench_weights /
        sum(
          bench_weights
        )
      
    } else {
      
      bench_minutes <- numeric()
    }
    
    rotation$
      bie_recommended_minutes <-
      c(
        starter_minutes,
        bench_minutes
      )
    
    rotation$
      bie_recommended_minutes <-
      round(
        rotation$
          bie_recommended_minutes,
        1
      )
    
    minute_gap <-
      total_minutes -
      sum(
        rotation$
          bie_recommended_minutes
      )
    
    if (
      nrow(rotation) &&
      abs(minute_gap) >= 0.05
    ) {
      rotation$
        bie_recommended_minutes[[1]] <-
        rotation$
        bie_recommended_minutes[[1]] +
        minute_gap
    }
    
  } else {
    
    rotation$
      bie_recommended_minutes <-
      allocate_bie_rotation_minutes(
        rotation,
        total_minutes =
          total_minutes
      )
  }
  
  rotation$bie_rotation_rank <-
    seq_len(
      nrow(rotation)
    )
  
  # Preserve the approved Starting Five order.
  starter_position_order <- match(
    rotation$bie_rotation_slot,
    lineup_positions
  )
  
  starter_sort <- ifelse(
    rotation$bie_rotation_role ==
      "STARTER",
    starter_position_order,
    100 +
      rank(
        -rotation$
          bie_recommended_minutes,
        ties.method = "first"
      )
  )
  
  rotation <- rotation[
    order(
      starter_sort
    ),
    ,
    drop = FALSE
  ]
  
  rownames(rotation) <- NULL
  
  rotation_performance_players <- if (
    "bie_score_source" %in%
    names(rotation)
  ) {
    sum(
      rotation$bie_score_source ==
        "PERFORMANCE_DATA",
      na.rm = TRUE
    )
  } else {
    0L
  }
  
  confidence <- if (
    rotation_performance_players >=
    nrow(rotation) - 1L
  ) {
    "HIGH"
  } else if (
    rotation_performance_players >=
    ceiling(
      nrow(rotation) / 2
    )
  ) {
    "MODERATE"
  } else {
    "FOUNDATION"
  }
  
  player_score <- suppressWarnings(
    as.numeric(
      rotation$bie_player_score
    )
  )
  
  minute_weight <- suppressWarnings(
    as.numeric(
      rotation$
        bie_recommended_minutes
    )
  )
  
  rotation_score <- if (
    any(
      is.finite(
        player_score
      )
    )
  ) {
    weighted.mean(
      player_score,
      minute_weight,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }
  
  explanation <- if (
    use_approved &&
    identical(
      confidence,
      "FOUNDATION"
    )
  ) {
    paste(
      "Rotation Intelligence is preserving the approved Working Starting Five.",
      "BIE is selecting the bench primarily from the loaded depth chart",
      "and allocating 240 planning minutes around those starters.",
      "Advanced performance data is not yet loaded for enough players",
      "to justify replacing the coach/front-office lineup."
    )
  } else if (
    use_approved
  ) {
    paste0(
      "Rotation Intelligence preserves the approved Working Starting Five and uses performance data for ",
      rotation_performance_players,
      " of ",
      nrow(rotation),
      " rotation players when selecting the bench and allocating minutes."
    )
  } else if (
    identical(
      confidence,
      "FOUNDATION"
    )
  ) {
    paste(
      "No complete approved Starting Five was supplied, so BIE generated one.",
      "The model is operating in FOUNDATION mode and relies primarily",
      "on depth hierarchy and position eligibility."
    )
  } else {
    paste0(
      "Rotation Intelligence uses performance data for ",
      rotation_performance_players,
      " of ",
      nrow(rotation),
      " rotation players, plus position versatility and roster hierarchy."
    )
  }
  
  list(
    status = "OK",
    rotation = rotation,
    rotation_size =
      nrow(rotation),
    total_minutes =
      sum(
        rotation$
          bie_recommended_minutes
      ),
    score =
      basketball_intel_clamp(
        rotation_score
      ),
    confidence = confidence,
    performance_data_players =
      rotation_performance_players,
    starting_five_source = if (
      use_approved
    ) {
      "WORKING_LINEUP"
    } else {
      "BIE_GENERATED"
    },
    foundation_mode =
      foundation_mode,
    explanation = explanation,
    model_label =
      "BIE Rotation v1.1"
  )
}



# ============================================================
# PHASE 2 — STEP 6
# Trade Basketball Impact
# ============================================================

#' Build a named five-position lineup from depth-chart data
#' @noRd
bie_lineup_from_depth_chart <- function(players) {
  
  positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  output <- stats::setNames(
    rep(
      NA_integer_,
      length(positions)
    ),
    positions
  )
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players) ||
    !"player_id" %in% names(players)
  ) {
    return(output)
  }
  
  d <- players
  
  if (
    !"is_starter" %in%
    names(d)
  ) {
    return(output)
  }
  
  starter_flag <- suppressWarnings(
    as.integer(
      d$is_starter
    )
  )
  
  starters <- d[
    !is.na(starter_flag) &
      starter_flag > 0,
    ,
    drop = FALSE
  ]
  
  if (!nrow(starters)) {
    return(output)
  }
  
  for (
    position in positions
  ) {
    
    candidates <- starters[
      vapply(
        seq_len(
          nrow(starters)
        ),
        function(i) {
          position %in%
            bie_player_positions(
              starters[
                i,
                ,
                drop = FALSE
              ]
            )
        },
        logical(1)
      ),
      ,
      drop = FALSE
    ]
    
    if (!nrow(candidates)) {
      next
    }
    
    exact <- candidates[
      vapply(
        seq_len(
          nrow(candidates)
        ),
        function(i) {
          identical(
            bie_primary_position(
              candidates[
                i,
                ,
                drop = FALSE
              ]
            ),
            position
          )
        },
        logical(1)
      ),
      ,
      drop = FALSE
    ]
    
    chosen <- if (
      nrow(exact)
    ) {
      exact[
        1,
        ,
        drop = FALSE
      ]
    } else {
      candidates[
        1,
        ,
        drop = FALSE
      ]
    }
    
    player_id <- suppressWarnings(
      as.integer(
        chosen$player_id[[1]]
      )
    )
    
    if (
      !is.na(player_id) &&
      !player_id %in%
      output
    ) {
      output[[position]] <-
        player_id
    }
  }
  
  output
}


#' Apply incoming/outgoing trade players to a roster preview
#'
#' This helper is pure and does not write to the database.
#' Incoming players enter the preview as non-starters at deep depth
#' until lineup intelligence evaluates where they belong.
#' @noRd
bie_apply_trade_to_roster <- function(
    current_players,
    outgoing_players,
    incoming_players) {
  
  if (
    is.null(current_players) ||
    !is.data.frame(current_players)
  ) {
    return(
      data.frame()
    )
  }
  
  preview <- current_players
  
  outgoing_ids <- if (
    !is.null(outgoing_players) &&
    is.data.frame(outgoing_players) &&
    nrow(outgoing_players) &&
    "player_id" %in%
    names(outgoing_players)
  ) {
    suppressWarnings(
      as.integer(
        outgoing_players$player_id
      )
    )
  } else {
    integer()
  }
  
  outgoing_ids <-
    outgoing_ids[
      !is.na(
        outgoing_ids
      )
    ]
  
  if (
    length(outgoing_ids) &&
    "player_id" %in%
    names(preview)
  ) {
    preview <- preview[
      !suppressWarnings(
        as.integer(
          preview$player_id
        )
      ) %in%
        outgoing_ids,
      ,
      drop = FALSE
    ]
  }
  
  if (
    is.null(incoming_players) ||
    !is.data.frame(incoming_players) ||
    !nrow(incoming_players)
  ) {
    rownames(preview) <- NULL
    return(preview)
  }
  
  incoming <- incoming_players
  
  # Map common transaction fields into depth-chart language.
  if (
    !"salary" %in%
    names(incoming) &&
    "cap_hit" %in%
    names(incoming)
  ) {
    incoming$salary <-
      suppressWarnings(
        as.numeric(
          incoming$cap_hit
        )
      )
  }
  
  if (
    !"position" %in%
    names(incoming) &&
    "primary_position" %in%
    names(incoming)
  ) {
    incoming$position <-
      as.character(
        incoming$primary_position
      )
  }
  
  if (
    !"depth_order" %in%
    names(incoming)
  ) {
    incoming$depth_order <- 99L
  }
  
  if (
    !"is_starter" %in%
    names(incoming)
  ) {
    incoming$is_starter <- 0L
  }
  
  if (
    !"roster_status" %in%
    names(incoming)
  ) {
    incoming$roster_status <-
      "Trade Scenario"
  }
  
  all_columns <- union(
    names(preview),
    names(incoming)
  )
  
  for (
    column in setdiff(
      all_columns,
      names(preview)
    )
  ) {
    preview[[column]] <- NA
  }
  
  for (
    column in setdiff(
      all_columns,
      names(incoming)
    )
  ) {
    incoming[[column]] <- NA
  }
  
  preview <- preview[
    ,
    all_columns,
    drop = FALSE
  ]
  
  incoming <- incoming[
    ,
    all_columns,
    drop = FALSE
  ]
  
  combined <- rbind(
    preview,
    incoming
  )
  
  if (
    "player_id" %in%
    names(combined)
  ) {
    combined <- combined[
      !duplicated(
        suppressWarnings(
          as.integer(
            combined$player_id
          )
        )
      ),
      ,
      drop = FALSE
    ]
  }
  
  rownames(combined) <- NULL
  
  combined
}


#' Evaluate one named five-player lineup against a roster
#' @noRd
evaluate_bie_named_lineup <- function(
    players,
    lineup) {
  
  positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players) ||
    is.null(lineup)
  ) {
    return(NULL)
  }
  
  lineup <- lineup[
    positions
  ]
  
  ids <- suppressWarnings(
    as.integer(
      lineup
    )
  )
  
  if (
    length(ids) != 5L ||
    any(
      is.na(ids)
    ) ||
    length(
      unique(ids)
    ) != 5L
  ) {
    return(NULL)
  }
  
  d <- bie_ensure_evaluated_players(
    players
  )
  
  index <- match(
    ids,
    suppressWarnings(
      as.integer(
        d$player_id
      )
    )
  )
  
  if (
    any(
      is.na(index)
    )
  ) {
    return(NULL)
  }
  
  lineup_rows <- d[
    index,
    ,
    drop = FALSE
  ]
  
  lineup_rows$
    bie_lineup_position <-
    positions
  
  player_quality <- mean(
    suppressWarnings(
      as.numeric(
        lineup_rows$
          bie_player_score
      )
    ),
    na.rm = TRUE
  )
  
  position_fit <- mean(
    vapply(
      seq_len(
        nrow(lineup_rows)
      ),
      function(i) {
        bie_position_fit_score(
          lineup_rows[
            i,
            ,
            drop = FALSE
          ],
          positions[[i]]
        )
      },
      numeric(1)
    ),
    na.rm = TRUE
  )
  
  balance <- bie_lineup_balance(
    lineup_rows
  )
  
  score <-
    0.72 *
    player_quality +
    0.18 *
    position_fit +
    0.10 *
    basketball_intel_number(
      balance$score,
      50
    )
  
  performance_players <- if (
    "bie_score_source" %in%
    names(lineup_rows)
  ) {
    sum(
      lineup_rows$
        bie_score_source ==
        "PERFORMANCE_DATA",
      na.rm = TRUE
    )
  } else {
    0L
  }
  
  confidence <- if (
    performance_players == 5L
  ) {
    "HIGH"
  } else if (
    performance_players >= 3L
  ) {
    "MODERATE"
  } else {
    "FOUNDATION"
  }
  
  list(
    status = "OK",
    lineup = lineup_rows,
    score =
      basketball_intel_clamp(
        score
      ),
    player_quality =
      player_quality,
    position_fit =
      position_fit,
    balance = balance,
    performance_data_players =
      performance_players,
    confidence = confidence
  )
}


#' Rebuild only the lineup spots lost in a trade
#'
#' Existing starters who remain on the proposed roster are preserved.
#' BIE fills only vacant positions from the proposed roster.
#' @noRd
build_bie_post_trade_lineup <- function(
    proposed_players,
    current_lineup) {
  
  positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  output <- stats::setNames(
    rep(
      NA_integer_,
      5L
    ),
    positions
  )
  
  if (
    is.null(proposed_players) ||
    !is.data.frame(proposed_players) ||
    !nrow(proposed_players)
  ) {
    return(output)
  }
  
  d <- if (
    "bie_player_score" %in%
    names(proposed_players)
  ) {
    proposed_players
  } else {
    evaluate_bie_players(
      proposed_players
    )
  }
  
  current_lineup <- current_lineup[
    positions
  ]
  
  proposed_ids <- suppressWarnings(
    as.integer(
      d$player_id
    )
  )
  
  # Preserve current starters who are still on the roster.
  for (
    position in positions
  ) {
    
    player_id <- suppressWarnings(
      as.integer(
        current_lineup[[position]]
      )
    )
    
    if (
      length(player_id) &&
      !is.na(player_id) &&
      player_id %in%
      proposed_ids
    ) {
      output[[position]] <-
        player_id
    }
  }
  
  used <- output[
    !is.na(output)
  ]
  
  # Fill only vacancies.
  for (
    position in positions[
      is.na(output)
    ]
  ) {
    
    candidate_index <- which(
      !proposed_ids %in%
        used
    )
    
    if (!length(candidate_index)) {
      next
    }
    
    fit <- vapply(
      candidate_index,
      function(i) {
        bie_position_fit_score(
          d[
            i,
            ,
            drop = FALSE
          ],
          position
        )
      },
      numeric(1)
    )
    
    legal <- is.finite(
      fit
    )
    
    if (!any(legal)) {
      next
    }
    
    legal_index <-
      candidate_index[
        legal
      ]
    
    fit <-
      fit[
        legal
      ]
    
    score <-
      0.78 *
      suppressWarnings(
        as.numeric(
          d$
            bie_player_score[
              legal_index
            ]
        )
      ) +
      0.22 *
      fit
    
    best <- legal_index[
      which.max(
        score
      )
    ]
    
    player_id <- suppressWarnings(
      as.integer(
        d$player_id[[best]]
      )
    )
    
    if (!is.na(player_id)) {
      output[[position]] <-
        player_id
      
      used <- c(
        used,
        player_id
      )
    }
  }
  
  output
}


#' Evaluate the basketball impact of a proposed trade
#'
#' This compares the current approved depth-chart lineup/rotation
#' to the proposed roster while preserving unaffected starters.
#' In FOUNDATION mode, the result is explicitly a structural roster
#' comparison rather than a performance projection.
#' @noRd
evaluate_bie_trade_basketball_impact <- function(
    current_players,
    outgoing_players,
    incoming_players,
    current_lineup = NULL,
    rotation_size = 9L) {
  
  if (
    is.null(current_players) ||
    !is.data.frame(current_players) ||
    !nrow(current_players)
  ) {
    return(
      list(
        status = "NO_ROSTER",
        confidence = "FOUNDATION",
        explanation =
          "Current roster data is unavailable."
      )
    )
  }
  
  current_evaluated <-
    bie_ensure_evaluated_players(
      current_players
    )
  
  proposed_players <-
    bie_apply_trade_to_roster(
      current_players =
        current_players,
      outgoing_players =
        outgoing_players,
      incoming_players =
        incoming_players
    )
  
  proposed_evaluated <-
    bie_ensure_evaluated_players(
      proposed_players
    )
  
  if (
    is.null(current_lineup)
  ) {
    current_lineup <-
      bie_lineup_from_depth_chart(
        current_evaluated
      )
  }
  
  current_lineup <- current_lineup[
    c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C"
    )
  ]
  
  proposed_lineup <-
    build_bie_post_trade_lineup(
      proposed_players =
        proposed_evaluated,
      current_lineup =
        current_lineup
    )
  
  current_lineup_result <-
    evaluate_bie_named_lineup(
      current_evaluated,
      current_lineup
    )
  
  proposed_lineup_result <-
    evaluate_bie_named_lineup(
      proposed_evaluated,
      proposed_lineup
    )
  
  current_rotation <-
    optimize_bie_rotation(
      players =
        current_evaluated,
      rotation_size =
        rotation_size,
      total_minutes = 240L,
      approved_lineup =
        current_lineup
    )
  
  proposed_rotation <-
    optimize_bie_rotation(
      players =
        proposed_evaluated,
      rotation_size =
        rotation_size,
      total_minutes = 240L,
      approved_lineup =
        proposed_lineup
    )
  
  current_lineup_score <- if (
    is.null(
      current_lineup_result
    )
  ) {
    NA_real_
  } else {
    basketball_intel_number(
      current_lineup_result$score
    )
  }
  
  proposed_lineup_score <- if (
    is.null(
      proposed_lineup_result
    )
  ) {
    NA_real_
  } else {
    basketball_intel_number(
      proposed_lineup_result$score
    )
  }
  
  lineup_delta <- if (
    is.na(current_lineup_score) ||
    is.na(proposed_lineup_score)
  ) {
    NA_real_
  } else {
    proposed_lineup_score -
      current_lineup_score
  }
  
  current_rotation_score <- if (
    !is.null(current_rotation) &&
    identical(
      current_rotation$status,
      "OK"
    )
  ) {
    basketball_intel_number(
      current_rotation$score
    )
  } else {
    NA_real_
  }
  
  proposed_rotation_score <- if (
    !is.null(proposed_rotation) &&
    identical(
      proposed_rotation$status,
      "OK"
    )
  ) {
    basketball_intel_number(
      proposed_rotation$score
    )
  } else {
    NA_real_
  }
  
  rotation_delta <- if (
    is.na(current_rotation_score) ||
    is.na(proposed_rotation_score)
  ) {
    NA_real_
  } else {
    proposed_rotation_score -
      current_rotation_score
  }
  
  current_ids <- suppressWarnings(
    as.integer(
      current_players$player_id
    )
  )
  
  proposed_ids <- suppressWarnings(
    as.integer(
      proposed_players$player_id
    )
  )
  
  roster_count_delta <-
    length(
      unique(
        proposed_ids[
          !is.na(
            proposed_ids
          )
        ]
      )
    ) -
    length(
      unique(
        current_ids[
          !is.na(
            current_ids
          )
        ]
      )
    )
  
  preserved_starters <- sum(
    suppressWarnings(
      as.integer(
        current_lineup
      )
    ) %in%
      suppressWarnings(
        as.integer(
          proposed_lineup
        )
      ),
    na.rm = TRUE
  )
  
  performance_players <- max(
    basketball_intel_integer(
      current_rotation$
        performance_data_players,
      0L
    ),
    basketball_intel_integer(
      proposed_rotation$
        performance_data_players,
      0L
    )
  )
  
  confidence <- if (
    performance_players >= 8L
  ) {
    "HIGH"
  } else if (
    performance_players >= 5L
  ) {
    "MODERATE"
  } else {
    "FOUNDATION"
  }
  
  structural_delta <- mean(
    c(
      lineup_delta,
      rotation_delta
    ),
    na.rm = TRUE
  )
  
  if (
    !is.finite(
      structural_delta
    )
  ) {
    structural_delta <- 0
  }
  
  verdict <- if (
    structural_delta >= 5
  ) {
    "BASKETBALL UPGRADE"
  } else if (
    structural_delta >= 1.5
  ) {
    "MODEST UPGRADE"
  } else if (
    structural_delta > -1.5
  ) {
    "ROUGHLY NEUTRAL"
  } else if (
    structural_delta > -5
  ) {
    "MODEST DECLINE"
  } else {
    "BASKETBALL DECLINE"
  }
  
  explanation <- if (
    identical(
      confidence,
      "FOUNDATION"
    )
  ) {
    paste(
      "BIE is comparing roster structure, approved-starter continuity,",
      "position coverage, depth hierarchy, and the available fallback player signals.",
      "This is not yet a win, net-rating, offense, or defense projection."
    )
  } else {
    paste0(
      "BIE compares the current and proposed Starting Five plus ",
      rotation_size,
      "-man rotation using available performance data and roster-fit signals."
    )
  }
  
  list(
    status = "OK",
    confidence = confidence,
    verdict = verdict,
    structural_delta =
      structural_delta,
    current_lineup =
      current_lineup,
    proposed_lineup =
      proposed_lineup,
    current_lineup_score =
      current_lineup_score,
    proposed_lineup_score =
      proposed_lineup_score,
    lineup_delta =
      lineup_delta,
    current_rotation_score =
      current_rotation_score,
    proposed_rotation_score =
      proposed_rotation_score,
    rotation_delta =
      rotation_delta,
    roster_count_delta =
      roster_count_delta,
    preserved_starters =
      preserved_starters,
    current_rotation =
      current_rotation,
    proposed_rotation =
      proposed_rotation,
    explanation = explanation,
    model_label =
      "BIE Trade Basketball Impact v1"
  )
}


# ------------------------------------------------------------
# BIE Phase-2 model metadata
# ------------------------------------------------------------

#' Return Basketball Intelligence Engine capabilities
#' @noRd
bie_capabilities <- function() {
  
  list(
    engine = "Thompson's Basketball Intelligence",
    version = "BIE v2.0 Foundation",
    executive_decision_engine = TRUE,
    player_evaluation = TRUE,
    starting_five_optimization = TRUE,
    lineup_comparison = TRUE,
    explainable_lineup_intelligence = TRUE,
    player_fit_intelligence = TRUE,
    rotation_optimization = TRUE,
    trade_basketball_impact = TRUE,
    extension_value_model = FALSE,
    win_projection = FALSE,
    playoff_projection = FALSE,
    championship_projection = FALSE,
    notes = c(
      "Existing executive decision scoring remains active.",
      "Player scores automatically upgrade when advanced metrics are loaded.",
      "Missing performance metrics are never fabricated.",
      "Starting-five optimization respects loaded position eligibility.",
      "Lineup recommendations include inspectable strengths, concerns, and component-change explanations.",
      "Player Fit Intelligence provides team-relative player profiles when performance metrics are available.",
      "Rotation Intelligence preserves an approved Working Starting Five when supplied, then recommends the bench and regulation-minute allocation.",
      "Trade Basketball Impact compares current vs proposed Starting Five continuity, rotation quality, depth, and roster structure.",
      "Rotation and transaction basketball-impact layers are Phase-2 follow-ons."
    )
  )
}


# >>> TBI_BIE_VNEXT_LOCKED_OVERRIDE_START >>>

# ============================================================
# BIE vNEXT — LOCKED ARCHITECTURE OVERRIDE
# Generated from validated six-team calibration.
# Later definitions intentionally override legacy Phase-2 BIE.
# ============================================================

.bie_vnext_reference_cache <- new.env(parent = emptyenv())

bie_vnext_num <- function (x) 
{
    suppressWarnings(as.numeric(x))
}

bie_vnext_clamp <- function (x, low = 0, high = 100) 
{
    x <- suppressWarnings(as.numeric(x))
    pmin(high, pmax(low, x))
}

bie_vnext_coalesce_num <- function (data, candidates, default = NA_real_) 
{
    output <- rep(default, nrow(data))
    for (field in candidates) {
        if (!field %in% names(data)) {
            next
        }
        value <- suppressWarnings(as.numeric(data[[field]]))
        use <- !is.finite(output) & is.finite(value)
        output[use] <- value[use]
    }
    output
}

bie_vnext_weighted <- function (inputs, weights) 
{
    if (!length(inputs)) {
        return(numeric())
    }
    m <- do.call(cbind, lapply(inputs, function(x) {
        suppressWarnings(as.numeric(x))
    }))
    weights <- suppressWarnings(as.numeric(weights))
    output <- rep(NA_real_, nrow(m))
    for (i in seq_len(nrow(m))) {
        values <- m[i, , drop = TRUE]
        valid <- is.finite(values) & is.finite(weights) & weights > 0
        if (!any(valid)) {
            next
        }
        output[[i]] <- weighted.mean(values[valid], weights[valid])
    }
    pmin(100, pmax(0, output))
}

bie_vnext_roster_percentile <- function (x, higher_is_better = TRUE) 
{
    x <- suppressWarnings(as.numeric(x))
    output <- rep(NA_real_, length(x))
    valid <- is.finite(x)
    if (!any(valid)) {
        return(output)
    }
    if (sum(valid) == 1L) {
        output[valid] <- 50
        return(output)
    }
    ranks <- rank(x[valid], ties.method = "average")
    pct <- 100 * (ranks - 1)/(length(ranks) - 1)
    if (!higher_is_better) {
        pct <- 100 - pct
    }
    output[valid] <- pct
    output
}

bie_vnext_league_percentile <- function (value, reference) 
{
    value <- suppressWarnings(as.numeric(value))
    reference <- suppressWarnings(as.numeric(reference))
    reference <- reference[is.finite(reference)]
    if (length(value) != 1L || !is.finite(value) || !length(reference)) {
        return(NA_real_)
    }
    below <- sum(reference < value)
    equal <- sum(reference == value)
    100 * (below + 0.5 * equal)/length(reference)
}

bie_vnext_reliability <- function (games, mpg) 
{
    games <- suppressWarnings(as.numeric(games))
    mpg <- suppressWarnings(as.numeric(mpg))
    games_factor <- pmin(1, pmax(0, games/25))
    mpg_factor <- pmin(1, pmax(0, mpg/20))
    result <- rep(0, length(games))
    valid <- is.finite(games_factor) & is.finite(mpg_factor)
    result[valid] <- pmin(games_factor[valid], mpg_factor[valid])
    result
}

bie_vnext_stabilize <- function (score, reliability) 
{
    score <- suppressWarnings(as.numeric(score))
    reliability <- suppressWarnings(as.numeric(reliability))
    result <- 50 + (score - 50) * reliability
    result[!is.finite(score)] <- NA_real_
    pmin(100, pmax(0, result))
}

bie_vnext_season_vector <- function (data) 
{
    output <- rep(NA_character_, nrow(data))
    candidates <- c("performance_season_used", "performance_season", "stats_season", "advanced_season", "source_season", "season")
    for (field in intersect(candidates, names(data))) {
        value <- as.character(data[[field]])
        valid <- !is.na(value) & nzchar(trimws(value))
        use <- is.na(output) & valid
        output[use] <- trimws(value[use])
    }
    output
}

bie_vnext_get_league_reference <- function (season) 
{
    requested <- as.character(season[[1]])
    if (is.na(requested) || !nzchar(trimws(requested))) {
        return(list(available = FALSE, season = NA_character_))
    }
    requested <- trimws(requested)
    if (exists(requested, envir = .bie_vnext_reference_cache, inherits = FALSE)) {
        return(get(requested, envir = .bie_vnext_reference_cache, inherits = FALSE))
    }
    con <- tryCatch(connect_db(read_only = TRUE), error = function(e) {
        NULL
    })
    if (is.null(con)) {
        return(list(available = FALSE, season = requested))
    }
    on.exit(try(disconnect_db(con), silent = TRUE), add = TRUE)
    fetch_reference <- function(target_season) {
        league <- tryCatch(DBI::dbGetQuery(con, "\n          SELECT\n            a.player_id,\n            a.team_id,\n            a.player_impact_estimate,\n            a.net_rating,\n            a.points_per_100,\n            a.usage_rate,\n            a.free_throw_rate,\n            a.true_shooting_pct,\n            a.effective_field_goal_pct,\n            s.games_played,\n            s.minutes,\n            s.points\n          FROM player_season_advanced a\n          INNER JOIN player_season_stats s\n            ON s.player_id = a.player_id\n            AND s.team_id = a.team_id\n            AND s.season = a.season\n          WHERE a.season = ?\n          ", 
            params = list(target_season)), error = function(e) {
            data.frame()
        })
        if (!nrow(league)) {
            return(NULL)
        }
        numeric_fields <- c("player_impact_estimate", "net_rating", "points_per_100", "usage_rate", "free_throw_rate", "true_shooting_pct", "effective_field_goal_pct", "games_played", "minutes", "points")
        for (field in intersect(numeric_fields, names(league))) {
            league[[field]] <- suppressWarnings(as.numeric(league[[field]]))
        }
        league$ppg <- ifelse(is.finite(league$games_played) & league$games_played > 0 & is.finite(league$points), league$points/league$games_played, NA_real_)
        minute_order <- ifelse(is.finite(league$minutes), league$minutes, -Inf)
        league <- league[order(league$player_id, -minute_order), , drop = FALSE]
        league <- league[!duplicated(league$player_id), , drop = FALSE]
        qualified <- is.finite(league$games_played) & is.finite(league$minutes) & league$games_played >= 20 & league$minutes >= 300
        q <- league[qualified, , drop = FALSE]
        if (nrow(q) < 100) {
            return(NULL)
        }
        ts_mean <- mean(q$true_shooting_pct, na.rm = TRUE)
        ts_sd <- stats::sd(q$true_shooting_pct, na.rm = TRUE)
        efg_mean <- mean(q$effective_field_goal_pct, na.rm = TRUE)
        efg_sd <- stats::sd(q$effective_field_goal_pct, na.rm = TRUE)
        combo <- rep(NA_real_, nrow(q))
        valid_eff <- is.finite(q$true_shooting_pct) & is.finite(q$effective_field_goal_pct) & is.finite(ts_sd) & ts_sd > 0 & is.finite(efg_sd) & efg_sd > 0
        combo[valid_eff] <- 0.7 * (q$true_shooting_pct[valid_eff] - ts_mean)/ts_sd + 0.3 * (q$effective_field_goal_pct[valid_eff] - efg_mean)/efg_sd
        list(available = TRUE, season = target_season, n = nrow(q), pie = q$player_impact_estimate, net = q$net_rating, points100 = q$points_per_100, ppg = q$ppg, ftr = q$free_throw_rate, usage = q$usage_rate, ts_mean = ts_mean, ts_sd = ts_sd, efg_mean = efg_mean, efg_sd = efg_sd, combo_mean = mean(combo, na.rm = TRUE), combo_sd = stats::sd(combo, na.rm = TRUE))
    }
    available_seasons <- tryCatch(DBI::dbGetQuery(con, "\n        SELECT DISTINCT season\n        FROM player_season_advanced\n        WHERE season <= ?\n        ORDER BY season DESC\n        ", params = list(requested)), error = function(e) {
        data.frame()
    })
    candidates <- unique(c(requested, if (nrow(available_seasons) && "season" %in% names(available_seasons)) {
        as.character(available_seasons$season)
    } else {
        character()
    }))
    result <- NULL
    for (candidate in candidates) {
        candidate_result <- fetch_reference(candidate)
        if (!is.null(candidate_result) && isTRUE(candidate_result$available)) {
            result <- candidate_result
            break
        }
    }
    if (is.null(result)) {
        result <- list(available = FALSE, season = requested)
    }
    assign(requested, result, envir = .bie_vnext_reference_cache)
    result
}

evaluate_bie_players <- function (players, weight_overrides = NULL) 
{
    if (is.null(players) || !is.data.frame(players)) {
        stop("players must be a data frame.", call. = FALSE)
    }
    if (!nrow(players)) {
        return(players)
    }
    d <- players
    n <- nrow(d)
    if (!"player_id" %in% names(d)) {
        d$player_id <- seq_len(n)
    }
    if (!"player_name" %in% names(d)) {
        d$player_name <- paste0("Player ", d$player_id)
    }
    games <- bie_vnext_coalesce_num(d, c("stats_games_played", "games_played", "impact_games_played", "shooting_games_played", "playmaking_games_played", "defense_games_played", "games", "g"))
    total_minutes <- bie_vnext_coalesce_num(d, c("stats_minutes", "impact_minutes", "shooting_minutes", "playmaking_minutes", "defense_minutes", "minutes"))
    mpg <- bie_vnext_coalesce_num(d, c("minutes_per_game", "stats_minutes_per_game", "impact_minutes_per_game", "mpg"))
    need_mpg <- !is.finite(mpg) & is.finite(total_minutes) & is.finite(games) & games > 0
    mpg[need_mpg] <- total_minutes[need_mpg]/games[need_mpg]
    mpg[is.finite(mpg) & (mpg < 0 | mpg > 48)] <- NA_real_
    reliability <- bie_vnext_reliability(games, mpg)
    seasons <- bie_vnext_season_vector(d)
    points <- bie_vnext_coalesce_num(d, c("stats_points", "points"))
    ppg <- bie_vnext_coalesce_num(d, c("stats_points_per_game", "points_per_game", "ppg"))
    need_ppg <- !is.finite(ppg) & is.finite(points) & is.finite(games) & games > 0
    ppg[need_ppg] <- points[need_ppg]/games[need_ppg]
    pie <- bie_vnext_coalesce_num(d, c("advanced_player_impact_estimate", "impact_player_impact_estimate", "player_impact_estimate", "pie"))
    net_rating <- bie_vnext_coalesce_num(d, c("advanced_net_rating", "impact_net_rating", "net_rating"))
    points100 <- bie_vnext_coalesce_num(d, c("advanced_points_per_100", "points_per_100", "pts_per_100"))
    usage <- bie_vnext_coalesce_num(d, c("advanced_usage_rate", "playmaking_usage_rate", "usage_rate", "usg_pct"))
    ftr <- bie_vnext_coalesce_num(d, c("advanced_free_throw_rate", "shooting_free_throw_rate", "free_throw_rate", "ftr"))
    ts <- bie_vnext_coalesce_num(d, c("advanced_true_shooting_pct", "shooting_true_shooting_pct", "true_shooting_pct", "ts_pct"))
    efg <- bie_vnext_coalesce_num(d, c("advanced_effective_field_goal_pct", "shooting_effective_field_goal_pct", "effective_field_goal_pct", "effective_fg_pct", "efg_pct"))
    disruption <- bie_vnext_coalesce_num(d, c("defense_disruption_score", "disruption_score", "defense_disruption", "disruption"))
    interior <- bie_vnext_coalesce_num(d, c("defense_interior_impact_score", "interior_impact_score", "defense_interior_impact", "interior_impact"))
    rebound_impact <- bie_vnext_coalesce_num(d, c("defense_rebound_impact_percentile", "rebound_impact_percentile", "defense_rebound_impact_pct", "rebound_impact_pct"))
    defensive_activity <- bie_vnext_weighted(list(disruption, interior, rebound_impact), c(0.4, 0.35, 0.25))
    pie_pct <- rep(NA_real_, n)
    net_pct <- rep(NA_real_, n)
    p100_pct <- rep(NA_real_, n)
    ppg_pct <- rep(NA_real_, n)
    ftr_pct <- rep(NA_real_, n)
    usage_pct <- rep(NA_real_, n)
    efficiency_raw <- rep(NA_real_, n)
    reference_season <- rep(NA_character_, n)
    for (i in seq_len(n)) {
        if (is.na(seasons[[i]]) || !nzchar(trimws(seasons[[i]]))) {
            next
        }
        ref <- bie_vnext_get_league_reference(seasons[[i]])
        if (!isTRUE(ref$available)) {
            next
        }
        reference_season[[i]] <- ref$season
        pie_pct[[i]] <- bie_vnext_league_percentile(pie[[i]], ref$pie)
        net_pct[[i]] <- bie_vnext_league_percentile(net_rating[[i]], ref$net)
        p100_pct[[i]] <- bie_vnext_league_percentile(points100[[i]], ref$points100)
        ppg_pct[[i]] <- bie_vnext_league_percentile(ppg[[i]], ref$ppg)
        ftr_pct[[i]] <- bie_vnext_league_percentile(ftr[[i]], ref$ftr)
        usage_pct[[i]] <- bie_vnext_league_percentile(usage[[i]], ref$usage)
        if (is.finite(ts[[i]]) && is.finite(efg[[i]]) && is.finite(ref$ts_sd) && ref$ts_sd > 0 && is.finite(ref$efg_sd) && ref$efg_sd > 0 && is.finite(ref$combo_sd) && ref$combo_sd > 0) {
            ts_z <- (ts[[i]] - ref$ts_mean)/ref$ts_sd
            efg_z <- (efg[[i]] - ref$efg_mean)/ref$efg_sd
            combo <- 0.7 * ts_z + 0.3 * efg_z
            combo_z <- (combo - ref$combo_mean)/ref$combo_sd
            efficiency_raw[[i]] <- bie_vnext_clamp(50 + 12 * combo_z)
        }
    }
    impact_raw <- bie_vnext_weighted(list(pie_pct, defensive_activity, net_pct), c(0.65, 0.25, 0.1))
    offense_raw <- bie_vnext_weighted(list(p100_pct, ppg_pct, ftr_pct, usage_pct), c(0.5, 0.25, 0.15, 0.1))
    defense_proxy <- bie_vnext_coalesce_num(d, c("defense_defense_proxy_score", "defense_proxy_score", "defense_defense_proxy", "defense_proxy"))
    existing_defense <- bie_vnext_coalesce_num(d, c("impact_defensive_impact_score", "defensive_impact_score", "existing_defensive_impact"))
    defense_raw <- bie_vnext_weighted(list(defense_proxy, disruption, interior, existing_defense), c(0.3, 0.2, 0.15, 0.2))
    creation <- bie_vnext_coalesce_num(d, c("playmaking_creation_score", "creation_score"))
    passing_control <- bie_vnext_coalesce_num(d, c("playmaking_passing_control_score", "passing_control_score"))
    secondary_creation <- bie_vnext_coalesce_num(d, c("playmaking_secondary_creation_score", "secondary_creation_score"))
    ball_security <- bie_vnext_coalesce_num(d, c("playmaking_ball_security_score", "ball_security_score"))
    assist_pct <- bie_vnext_coalesce_num(d, c("advanced_assist_pct", "playmaking_assist_pct", "assist_pct", "ast_pct"))
    assists100 <- bie_vnext_coalesce_num(d, c("advanced_assists_per_100", "playmaking_assists_per_100", "assists_per_100", "ast_per_100"))
    playmaking_raw <- bie_vnext_weighted(list(creation, passing_control, secondary_creation, ball_security, bie_vnext_roster_percentile(assist_pct), bie_vnext_roster_percentile(assists100)), c(0.3, 0.25, 0.2, 0.15, 0.05, 0.05))
    rebound_score <- bie_vnext_coalesce_num(d, c("defense_rebounding_score", "rebounding_score"))
    rebound_pct <- bie_vnext_coalesce_num(d, c("advanced_rebound_pct", "defense_rebound_pct", "rebound_pct", "trb_pct", "total_rebound_pct"))
    oreb_pct <- bie_vnext_coalesce_num(d, c("advanced_offensive_rebound_pct", "defense_offensive_rebound_pct", "offensive_rebound_pct"))
    dreb_pct <- bie_vnext_coalesce_num(d, c("advanced_defensive_rebound_pct", "defense_defensive_rebound_pct", "defensive_rebound_pct"))
    rebounds100 <- bie_vnext_coalesce_num(d, c("advanced_rebounds_per_100", "defense_rebounds_per_100", "rebounds_per_100", "trb_per_100", "reb_per_100"))
    rebounding_raw <- bie_vnext_weighted(list(rebound_score, bie_vnext_roster_percentile(rebound_pct), bie_vnext_roster_percentile(oreb_pct), bie_vnext_roster_percentile(dreb_pct), bie_vnext_roster_percentile(rebounds100)), c(0.5, 0.2, 0.1, 0.1, 0.1))
    impact_score <- bie_vnext_stabilize(impact_raw, reliability)
    offense_score <- bie_vnext_stabilize(offense_raw, reliability)
    defense_score <- bie_vnext_stabilize(defense_raw, reliability)
    efficiency_score <- bie_vnext_stabilize(efficiency_raw, reliability)
    playmaking_score <- bie_vnext_stabilize(playmaking_raw, reliability)
    rebounding_score <- bie_vnext_stabilize(rebounding_raw, reliability)
    availability_score <- 100 * pmin(1, pmax(0, games/82))
    availability_score[!is.finite(games)] <- NA_real_
    if ("tbi_performance_available" %in% names(d)) {
        raw_flag <- d$tbi_performance_available
        if (is.logical(raw_flag)) {
            performance_player <- raw_flag %in% TRUE
        }
        else {
            performance_player <- tolower(trimws(as.character(raw_flag))) %in% c("true", "t", "1", "yes", "y")
        }
    }
    else {
        evidence_matrix <- cbind(pie, net_rating, points100, ppg, ts, efg, assist_pct, rebounds100)
        performance_player <- is.finite(games) & games > 0 & rowSums(is.finite(evidence_matrix)) > 0
    }
    performance_player[is.na(performance_player)] <- FALSE
    weights <- bie_player_weight_defaults()
    if (!is.null(weight_overrides)) {
        common <- intersect(names(weight_overrides), names(weights))
        weights[common] <- suppressWarnings(as.numeric(weight_overrides[common]))
    }
    weights[!is.finite(weights) | weights < 0] <- 0
    categories <- data.frame(impact = impact_score, offense = offense_score, defense = defense_score, efficiency = efficiency_score, playmaking = playmaking_score, rebounding = rebounding_score, availability = availability_score)
    final_score <- bie_vnext_weighted(lapply(categories, identity), weights[names(categories)])
    basketball_component_count <- rowSums(is.finite(as.matrix(categories[, c("impact", "offense", "defense", "efficiency", "playmaking", "rebounding"), drop = FALSE])))
    use_performance <- performance_player & basketball_component_count >= 1L & is.finite(final_score)
    depth_order <- bie_vnext_coalesce_num(d, c("depth_order", "depth"))
    starter <- bie_vnext_coalesce_num(d, c("is_starter", "starter"), default = 0)
    salary <- bie_vnext_coalesce_num(d, c("cap_hit", "salary", "base_salary"))
    depth_score <- rep(50, n)
    valid_depth <- is.finite(depth_order)
    depth_score[valid_depth] <- bie_vnext_clamp(100 - pmin(depth_order[valid_depth] - 1, 5) * 14)
    salary_score <- bie_vnext_roster_percentile(salary)
    salary_score[!is.finite(salary_score)] <- 50
    starter_bonus <- ifelse(is.finite(starter) & starter > 0, 12, 0)
    fallback_score <- 0.7 * depth_score + 0.2 * salary_score + 0.1 * bie_vnext_clamp(50 + starter_bonus)
    no_fallback_data <- !is.finite(depth_order) & !is.finite(salary) & !(is.finite(starter) & starter > 0)
    fallback_score[no_fallback_data] <- 50
    impact_score[!use_performance] <- NA_real_
    offense_score[!use_performance] <- NA_real_
    defense_score[!use_performance] <- NA_real_
    efficiency_score[!use_performance] <- NA_real_
    playmaking_score[!use_performance] <- NA_real_
    rebounding_score[!use_performance] <- NA_real_
    availability_score[!use_performance] <- NA_real_
    final_score[!use_performance] <- NA_real_
    gp_factor <- pmin(1, pmax(0, games/25))
    mpg_factor <- pmin(1, pmax(0, mpg/20))
    gp_factor[!is.finite(gp_factor)] <- 0
    mpg_factor[!is.finite(mpg_factor)] <- 0
    selection_penalty <- 4 * (1 - gp_factor) + 18 * (1 - mpg_factor)
    selection_score <- ifelse(use_performance, final_score - selection_penalty, 50)
    effective_impact_weight <- rep(NA_real_, n)
    impact_signal_share <- rep(NA_real_, n)
    final_categories <- data.frame(impact = impact_score, offense = offense_score, defense = defense_score, efficiency = efficiency_score, playmaking = playmaking_score, rebounding = rebounding_score, availability = availability_score)
    for (i in seq_len(n)) {
        if (!use_performance[[i]]) {
            next
        }
        values <- suppressWarnings(as.numeric(final_categories[i, , drop = TRUE]))
        names(values) <- names(final_categories)
        valid <- is.finite(values)
        if (!any(valid)) {
            next
        }
        row_weights <- weights[names(values)[valid]]
        row_weights <- row_weights/sum(row_weights)
        if ("impact" %in% names(row_weights)) {
            effective_impact_weight[[i]] <- 100 * row_weights[["impact"]]
            deviation <- abs(row_weights * (values[valid] - 50))
            total_deviation <- sum(deviation, na.rm = TRUE)
            if (is.finite(total_deviation) && total_deviation > 0) {
                impact_signal_share[[i]] <- 100 * deviation[["impact"]]/total_deviation
            }
        }
    }
    d$bie_structural_score <- bie_vnext_clamp(fallback_score)
    d$bie_player_score <- ifelse(use_performance, final_score, d$bie_structural_score)
    d$bie_score_source <- ifelse(use_performance, "PERFORMANCE_DATA", "DEPTH_FALLBACK")
    d$bie_metric_components <- rowSums(is.finite(as.matrix(final_categories)))
    d$bie_vnext_games <- games
    d$bie_vnext_mpg <- mpg
    d$bie_sample_reliability <- reliability
    d$bie_sample_reliability_pct <- reliability * 100
    d$bie_sample_confidence <- ifelse(reliability >= 1, "HIGH", ifelse(reliability >= 0.8, "STRONG", ifelse(reliability >= 0.6, "MODERATE", ifelse(reliability >= 0.4, "MODERATE-LOW", ifelse(reliability >= 0.2, "LOW", "VERY LOW")))))
    d$bie_reference_season <- reference_season
    d$impact_pie_league_pct <- pie_pct
    d$impact_net_league_pct <- net_pct
    d$bie_defensive_activity_score <- defensive_activity
    d$offense_pts100_league_pct <- p100_pct
    d$offense_ppg_league_pct <- ppg_pct
    d$offense_ftr_league_pct <- ftr_pct
    d$offense_usage_league_pct <- usage_pct
    d$bie_impact_score <- impact_score
    d$bie_offense_score <- offense_score
    d$bie_defense_score <- defense_score
    d$bie_efficiency_score <- efficiency_score
    d$bie_playmaking_score <- playmaking_score
    d$bie_rebounding_score <- rebounding_score
    d$bie_availability_score <- availability_score
    d$bie_vnext_player_score <- final_score
    d$bie_selection_uncertainty_penalty <- ifelse(use_performance, selection_penalty, 0)
    d$bie_selection_score <- selection_score
    d$bie_effective_impact_weight_pct <- effective_impact_weight
    d$bie_impact_signal_share_pct <- impact_signal_share
    d$bie_model_version <- "BIE vNEXT LOCKED"
    d
}

bie_ensure_evaluated_players <- function (players) 
{
    if (is.null(players) || !is.data.frame(players)) {
        return(players)
    }
    required <- c("bie_player_score", "bie_score_source", "bie_metric_components", "bie_selection_score", "bie_model_version")
    already_locked <- all(required %in% names(players)) && all(as.character(players$bie_model_version) == "BIE vNEXT LOCKED")
    if (already_locked) {
        return(players)
    }
    evaluate_bie_players(players)
}

optimize_bie_starting_five <- function (players, candidate_limit = 8L, player_weight = 0.65, position_weight = 0.25, balance_weight = 0.1, locks = NULL) 
{
    if (is.null(players) || !is.data.frame(players) || !nrow(players)) {
        return(list(status = "NO_ROSTER", lineup = data.frame(), score = NA_real_, explanation = "No roster data was supplied."))
    }
    candidate_limit <- suppressWarnings(as.integer(candidate_limit))
    if (!length(candidate_limit) || is.na(candidate_limit) || candidate_limit < 2L) {
        candidate_limit <- 8L
    }
    lineup_weights <- suppressWarnings(as.numeric(c(player_weight, position_weight, balance_weight)))
    if (length(lineup_weights) != 3L || any(!is.finite(lineup_weights)) || any(lineup_weights < 0) || sum(lineup_weights) <= 0) {
        lineup_weights <- c(0.65, 0.25, 0.1)
    }
    lineup_weights <- lineup_weights/sum(lineup_weights)
    player_weight <- lineup_weights[[1]]
    position_weight <- lineup_weights[[2]]
    balance_weight <- lineup_weights[[3]]
    candidate_total <- player_weight + position_weight
    candidate_player_weight <- player_weight/candidate_total
    candidate_position_weight <- position_weight/candidate_total
    d <- bie_ensure_evaluated_players(players)
    positions <- c("PG", "SG", "SF", "PF", "C")
    quality <- if ("bie_selection_score" %in% names(d)) {
        suppressWarnings(as.numeric(d$bie_selection_score))
    }
    else {
        suppressWarnings(as.numeric(d$bie_player_score))
    }
    if (is.null(locks) || !is.data.frame(locks) || !nrow(locks)) {
        locks <- data.frame(player_id = integer(), player_name = character(), position = character(), stringsAsFactors = FALSE)
    }
    if (nrow(locks) && !"position" %in% names(locks)) {
        stop("locks must contain a position column.", call. = FALSE)
    }
    if (nrow(locks)) {
        locks$position <- vapply(locks$position, bie_normalize_position, character(1))
        if (any(duplicated(locks$position))) {
            stop("Only one lock may be assigned to each lineup position.", call. = FALSE)
        }
        locks$resolved_index <- NA_integer_
        for (j in seq_len(nrow(locks))) {
            idx <- NA_integer_
            if ("player_id" %in% names(locks)) {
                lock_id <- suppressWarnings(as.integer(locks$player_id[[j]]))
                if (length(lock_id) && is.finite(lock_id)) {
                  idx <- match(lock_id, suppressWarnings(as.integer(d$player_id)))
                }
            }
            if (is.na(idx) && "player_name" %in% names(locks)) {
                lock_name <- as.character(locks$player_name[[j]])
                hits <- which(tolower(trimws(d$player_name)) == tolower(trimws(lock_name)))
                if (length(hits) == 1L) {
                  idx <- hits[[1]]
                }
            }
            if (is.na(idx)) {
                return(list(status = "LOCKED_PLAYER_NOT_FOUND", lineup = data.frame(), score = NA_real_))
            }
            locks$resolved_index[[j]] <- idx
        }
        if (any(duplicated(locks$resolved_index))) {
            stop("The same player cannot be locked into multiple positions.", call. = FALSE)
        }
    }
    locked_indices <- if (nrow(locks)) {
        as.integer(locks$resolved_index)
    }
    else {
        integer()
    }
    candidates <- vector("list", length(positions))
    names(candidates) <- positions
    for (pos in positions) {
        position_lock <- if (nrow(locks)) {
            locks[locks$position == pos, , drop = FALSE]
        }
        else {
            data.frame()
        }
        if (nrow(position_lock) == 1L) {
            idx <- as.integer(position_lock$resolved_index[[1]])
            fit <- bie_position_fit_score(d[idx, , drop = FALSE], pos)
            if (!is.finite(fit)) {
                return(list(status = "LOCKED_PLAYER_POSITION_ILLEGAL", lineup = data.frame(), score = NA_real_))
            }
            candidates[[pos]] <- idx
            next
        }
        fit <- vapply(seq_len(nrow(d)), function(i) {
            bie_position_fit_score(d[i, , drop = FALSE], pos)
        }, numeric(1))
        eligible <- is.finite(fit)
        if (length(locked_indices)) {
            eligible[locked_indices] <- FALSE
        }
        performance_eligible <- eligible & d$bie_score_source == "PERFORMANCE_DATA" & is.finite(quality)
        pool <- if (any(performance_eligible)) {
            performance_eligible
        }
        else {
            eligible & is.finite(quality)
        }
        if (!any(pool)) {
            candidates[[pos]] <- integer()
            next
        }
        candidate_score <- candidate_player_weight * quality + candidate_position_weight * fit
        idx <- which(pool)
        idx <- idx[order(-candidate_score[idx], d$player_name[idx], na.last = TRUE)]
        candidates[[pos]] <- head(idx, candidate_limit)
    }
    missing_positions <- positions[!vapply(candidates, length, integer(1))]
    if (length(missing_positions)) {
        return(list(status = "INCOMPLETE_POSITION_COVERAGE", lineup = data.frame(), score = NA_real_, missing_positions = missing_positions))
    }
    best_score <- -Inf
    best_assignment <- NULL
    evaluated_lineups <- 0L
    search_assignment <- function(position_index, chosen_indices, assigned_positions) {
        if (position_index > length(positions)) {
            evaluated_lineups <<- evaluated_lineups + 1L
            lineup <- d[chosen_indices, , drop = FALSE]
            lineup$bie_lineup_position <- assigned_positions
            lineup_quality <- if ("bie_selection_score" %in% names(lineup)) {
                suppressWarnings(as.numeric(lineup$bie_selection_score))
            }
            else {
                suppressWarnings(as.numeric(lineup$bie_player_score))
            }
            player_quality <- mean(lineup_quality, na.rm = TRUE)
            raw_bie_quality <- mean(suppressWarnings(as.numeric(lineup$bie_player_score)), na.rm = TRUE)
            position_fit <- mean(vapply(seq_len(nrow(lineup)), function(i) {
                bie_position_fit_score(lineup[i, , drop = FALSE], lineup$bie_lineup_position[[i]])
            }, numeric(1)), na.rm = TRUE)
            balance <- bie_lineup_balance(lineup)
            score <- player_weight * player_quality + position_weight * position_fit + balance_weight * balance$score
            score <- pmin(100, pmax(0, score))
            if (score > best_score) {
                best_score <<- score
                best_assignment <<- list(lineup = lineup, player_quality = player_quality, raw_bie_quality = raw_bie_quality, position_fit = position_fit, balance = balance)
            }
            return(invisible(NULL))
        }
        pos <- positions[[position_index]]
        for (candidate_index in candidates[[pos]]) {
            if (candidate_index %in% chosen_indices) {
                next
            }
            search_assignment(position_index = position_index + 1L, chosen_indices = c(chosen_indices, candidate_index), assigned_positions = c(assigned_positions, pos))
        }
        invisible(NULL)
    }
    search_assignment(position_index = 1L, chosen_indices = integer(), assigned_positions = character())
    if (is.null(best_assignment)) {
        return(list(status = "NO_LEGAL_LINEUP", lineup = data.frame(), score = NA_real_, evaluated_lineups = evaluated_lineups))
    }
    lineup <- best_assignment$lineup
    lineup <- lineup[match(positions, lineup$bie_lineup_position), , drop = FALSE]
    rownames(lineup) <- NULL
    source_counts <- table(lineup$bie_score_source)
    performance_players <- if ("PERFORMANCE_DATA" %in% names(source_counts)) {
        as.integer(source_counts[["PERFORMANCE_DATA"]])
    }
    else {
        0L
    }
    confidence <- if (performance_players == 5L) {
        "HIGH"
    }
    else if (performance_players >= 3L) {
        "MODERATE"
    }
    else {
        "FOUNDATION"
    }
    list(status = "OK", lineup = lineup, score = best_score, player_quality = best_assignment$player_quality, raw_bie_quality = best_assignment$raw_bie_quality, position_fit = best_assignment$position_fit, balance = best_assignment$balance, evaluated_lineups = evaluated_lineups, performance_data_players = performance_players, confidence = confidence, explanation = paste0("BIE vNEXT Starting Five: ", round(100 * player_weight), "% player quality / ", round(100 * position_weight), "% position fit / ", 
        round(100 * balance_weight), "% lineup balance. Performance-backed legal candidates take priority over fallback."), model_label = "BIE Starting Five vNEXT LOCKED")
}

bie_capabilities <- function () 
{
    list(engine = "Thompson's Basketball Intelligence", version = "BIE vNEXT — Locked Architecture", executive_decision_engine = TRUE, player_evaluation = TRUE, starting_five_optimization = TRUE, lineup_comparison = TRUE, explainable_lineup_intelligence = TRUE, player_fit_intelligence = TRUE, rotation_optimization = TRUE, trade_basketball_impact = TRUE, extension_value_model = FALSE, win_projection = FALSE, playoff_projection = FALSE, championship_projection = FALSE, notes = c("Overall BIE weights: Impact 34%, Offense 20%, Defense 20%, Efficiency 10%, Playmaking 8%, Rebounding 5%, Availability 3%.", 
        "Impact: 65% leaguewide PIE, 25% Defensive Activity, 10% leaguewide Net Rating.", "Defensive Activity: 40% Disruption, 35% Interior Impact, 25% Rebound Impact.", "Offense: 50% leaguewide PTS/100, 25% leaguewide PPG, 15% leaguewide FTr, 10% leaguewide Usage.", "Efficiency: 70% TS and 30% eFG using a qualified league-standardized reference.", "Performance samples are stabilized using GP/25 and MPG/20 reliability.", "Selection uncertainty is applied separately from the BIE player score.", "Starting Five optimization uses 65% player quality, 25% position fit, and 10% lineup balance.", 
        "Performance-backed legal candidates take priority over depth-chart fallback candidates.", "Architecture is locked pending final pre-freeze QA."))
}

# <<< TBI_BIE_VNEXT_LOCKED_OVERRIDE_END <<<


# >>> TBI_PERMANENT_STARTING_FIVE_LOCKS_START >>>

# ============================================================
# TBI — Permanent Approved Starting-Five Locks
# 2026-27 pre-freeze registry
# ============================================================

bie_starting_five_lock_registry <- function() {

  data.frame(
    season = c(
      rep('2026-27', 5),
      '2026-27',
      '2026-27',
      '2026-27',
      '2026-27'
    ),

    team_name = c(
      rep('Denver Nuggets', 5),
      'Brooklyn Nets',
      'Sacramento Kings',
      'Utah Jazz',
      'Washington Wizards'
    ),

    player_name = c(
      'Jamal Murray',
      'Christian Braun',
      'Cameron Johnson',
      'Aaron Gordon',
      'Nikola Jokic',
      'Mikel Brown Jr.',
      'Darius Acuff',
      'Darryn Peterson',
      'AJ Dybantsa'
    ),

    position = c(
      'PG',
      'SG',
      'SF',
      'PF',
      'C',
      'PG',
      'PG',
      'PG',
      'SF'
    ),

    lock_reason = c(
      rep('USER_APPROVED_STARTING_FIVE', 5),
      rep('USER_DESIGNATED_ROOKIE_STARTER', 4)
    ),

    stringsAsFactors = FALSE
  )
}

bie_resolve_roster_team_name <- function(players) {

  if (is.null(players) || !is.data.frame(players) || !nrow(players)) {
    return(NA_character_)
  }

  for (field in c('team_name', 'current_team_name', 'team')) {

    if (field %in% names(players)) {

      values <- unique(trimws(as.character(players[[field]])))
      values <- values[!is.na(values) & nzchar(values)]

      if (length(values) == 1L) {
        return(values[[1]])
      }
    }
  }

  if ('team_id' %in% names(players)) {

    ids <- unique(
      suppressWarnings(as.integer(players$team_id))
    )

    ids <- ids[is.finite(ids)]

    if (length(ids) == 1L && exists('get_team', mode = 'function')) {

      team_row <- tryCatch(
        get_team(ids[[1]]),
        error = function(e) data.frame()
      )

      if (
        is.data.frame(team_row) &&
        nrow(team_row) &&
        'team_name' %in% names(team_row)
      ) {

        value <- as.character(team_row$team_name[[1]])

        if (!is.na(value) && nzchar(trimws(value))) {
          return(trimws(value))
        }
      }
    }
  }

  NA_character_
}

bie_resolve_roster_season <- function(players) {

  if (is.null(players) || !is.data.frame(players) || !nrow(players)) {
    return(NA_character_)
  }

  for (field in c('season', 'roster_season')) {

    if (field %in% names(players)) {

      values <- unique(trimws(as.character(players[[field]])))
      values <- values[!is.na(values) & nzchar(values)]

      if (length(values) == 1L) {
        return(values[[1]])
      }
    }
  }

  NA_character_
}

bie_default_starting_five_locks <- function(players) {

  team_name <- bie_resolve_roster_team_name(players)
  season <- bie_resolve_roster_season(players)

  if (is.na(team_name) || is.na(season)) {
    return(data.frame())
  }

  registry <- bie_starting_five_lock_registry()

  specs <- registry[
    registry$team_name == team_name &
    registry$season == season,
    ,
    drop = FALSE
  ]

  if (!nrow(specs)) {
    return(data.frame())
  }

  output <- list()

  roster_names <- tolower(trimws(as.character(players$player_name)))

  for (i in seq_len(nrow(specs))) {

    idx <- which(
      roster_names ==
      tolower(trimws(specs$player_name[[i]]))
    )

    if (length(idx) != 1L) {

      warning(
        paste0(
          'Approved BIE lock could not be resolved: ',
          team_name,
          ' / ',
          specs$player_name[[i]]
        )
      )

      next
    }

    output[[length(output) + 1L]] <- data.frame(
      player_id = suppressWarnings(as.integer(players$player_id[[idx]])),
      player_name = as.character(players$player_name[[idx]]),
      position = specs$position[[i]],
      lock_reason = specs$lock_reason[[i]],
      stringsAsFactors = FALSE
    )
  }

  if (!length(output)) {
    return(data.frame())
  }

  do.call(rbind, output)
}

# Preserve validated vNEXT optimizer as the core implementation.
.bie_vnext_starting_five_core <- optimize_bie_starting_five

optimize_bie_starting_five <- function(
    players,
    candidate_limit = 8L,
    player_weight = 0.65,
    position_weight = 0.25,
    balance_weight = 0.10,
    locks = NULL) {

  resolved_locks <- locks

  if (
    is.null(resolved_locks) ||
    !is.data.frame(resolved_locks) ||
    !nrow(resolved_locks)
  ) {

    resolved_locks <- bie_default_starting_five_locks(players)
  }

  .bie_vnext_starting_five_core(
    players = players,
    candidate_limit = candidate_limit,
    player_weight = player_weight,
    position_weight = position_weight,
    balance_weight = balance_weight,
    locks = resolved_locks
  )
}

# <<< TBI_PERMANENT_STARTING_FIVE_LOCKS_END <<<




# >>> TBI_BIE_FINAL_FREEZE_STAMP_START >>>

# ============================================================
# THOMPSON'S BASKETBALL INTELLIGENCE
# BIE vNEXT — OFFICIAL ARCHITECTURE FREEZE
#
# Freeze date: 2026-08-13
#
# Final 30-team freeze QA: PASS
#
# FROZEN ARCHITECTURE:
#   Overall BIE
#     Impact        34%
#     Offense       20%
#     Defense       20%
#     Efficiency    10%
#     Playmaking     8%
#     Rebounding     5%
#     Availability   3%
#
#   Impact
#     65% PIE — leaguewide
#     25% Defensive Activity
#     10% Net Rating — leaguewide
#
#   Offense
#     50% Points / 100
#     25% PPG
#     15% Free Throw Rate
#     10% Usage
#
#   Efficiency
#     70% TS
#     30% eFG
#     league standardized
#
#   Reliability
#     GP / 25
#     MPG / 20
#     reliability = minimum of the two
#
#   Selection uncertainty
#     4 * (1 - GP factor)
#     + 18 * (1 - MPG factor)
#
#   Starting Five Optimizer
#     65% Player Quality
#     25% Position Fit
#     10% Lineup Balance
#
#   Position Fit
#     1st 100
#     2nd 90
#     3rd 75
#     4th 55
#     5th 40
#     further 35
#     unlisted -Inf
#
# POST-FREEZE PERMITTED:
#   roster updates
#   starting-lineup cleanup
#   manual/approved locks
#   rookie designations
#   position/source corrections
#   transaction updates
#   injury/availability updates
#   player/data mapping corrections
#
# POST-FREEZE REQUIRES MODEL VERSION REVIEW:
#   changing BIE weights
#   changing category formulas
#   changing reliability formula
#   changing selection penalty
#   changing optimizer weights
#   changing position-fit hierarchy
#   changing league-reference methodology
# ============================================================

bie_freeze_status <- function() {
  list(
    frozen = TRUE,
    freeze_date = '2026-08-13',
    model = 'BIE vNEXT — Locked Architecture',
    freeze_qa = 'PASS',
    teams_tested = 30L,
    optimizer = '65% quality / 25% position fit / 10% lineup balance',
    lineup_cleanup_allowed = TRUE,
    architecture_changes_require_new_version = TRUE
  )
}

# <<< TBI_BIE_FINAL_FREEZE_STAMP_END <<<


# >>> TBI_BIE_ROSTER_ENRICHMENT_COMPAT_START >>>
# Restores roster-to-performance enrichment expected by the UI.
# Frozen BIE scoring architecture is unchanged.
tbi_bie_enrich_roster <- function (roster, roster_season = NULL) 
{
    if (is.null(roster) || !is.data.frame(roster) || !nrow(roster)) {
        return(roster)
    }
    if (is.null(roster_season) || !length(roster_season) || is.na(roster_season[[1]]) || !nzchar(trimws(as.character(roster_season[[1]])))) {
        season_values <- if ("season" %in% names(roster)) {
            unique(as.character(roster$season))
        }
        else {
            character()
        }
        season_values <- season_values[!is.na(season_values) & nzchar(trimws(season_values))]
        roster_season <- if (length(season_values)) {
            season_values[[1]]
        }
        else {
            "2026-27"
        }
    }
    roster_season <- as.character(roster_season[[1]])
    con <- tryCatch(connect_db(read_only = TRUE), error = function(e) {
        connect_db()
    })
    on.exit(disconnect_db(con), add = TRUE)
    latest_performance_season <- function(player_id) {
        if (!"player_season_stats" %in% DBI::dbListTables(con)) {
            return(roster_season)
        }
        fields <- DBI::dbListFields(con, "player_season_stats")
        if (!all(c("player_id", "season") %in% fields)) {
            return(roster_season)
        }
        game_filter <- if ("games_played" %in% fields) {
            " AND COALESCE(games_played, 0) > 0 "
        }
        else {
            ""
        }
        result <- tryCatch(DBI::dbGetQuery(con, paste0("SELECT season ", "FROM player_season_stats ", "WHERE player_id = ? ", "AND season <= ? ", game_filter, "GROUP BY season ", "ORDER BY season DESC ", "LIMIT 1"), params = list(as.integer(player_id), roster_season)), error = function(e) {
            data.frame()
        })
        if (!nrow(result) || is.na(result$season[[1]])) {
            return(roster_season)
        }
        as.character(result$season[[1]])
    }
    latest_row <- function(table_name, player_id, performance_season) {
        tables <- DBI::dbListTables(con)
        if (!table_name %in% tables) {
            return(data.frame())
        }
        fields <- DBI::dbListFields(con, table_name)
        if (!"player_id" %in% fields) {
            return(data.frame())
        }
        where <- c("player_id = ?")
        params <- list(as.integer(player_id))
        if ("season" %in% fields) {
            where <- c(where, "season = ?")
            params <- c(params, list(as.character(performance_season)))
        }
        order_clause <- if ("minutes" %in% fields) {
            " ORDER BY COALESCE(minutes, 0) DESC "
        }
        else if ("updated_at" %in% fields) {
            " ORDER BY updated_at DESC "
        }
        else {
            ""
        }
        tryCatch(DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_name, " WHERE ", paste(where, collapse = " AND "), order_clause, " LIMIT 1"), params = params), error = function(e) {
            data.frame()
        })
    }
    merge_evidence <- function(base, extra, prefix) {
        if (is.null(extra) || !is.data.frame(extra) || !nrow(extra)) {
            return(base)
        }
        extra <- extra[1, , drop = FALSE]
        key_columns <- c("player_id", "team_id", "season", "source_name", "source_player_id", "source_team", "imported_at", "updated_at", "metric_version")
        evidence_columns <- setdiff(names(extra), key_columns)
        for (column in evidence_columns) {
            value <- extra[[column]][[1]]
            prefixed_name <- paste0(prefix, column)
            base[[prefixed_name]] <- value
            if (!column %in% names(base)) {
                base[[column]] <- value
            }
            else {
                current <- base[[column]]
                current_missing <- is.null(current) || !length(current) || all(is.na(current))
                value_usable <- !is.null(value) && length(value) && !all(is.na(value))
                if (current_missing && value_usable) {
                  base[[column]] <- value
                }
            }
        }
        base
    }
    evidence_tables <- c(stats = "player_season_stats", advanced = "player_season_advanced", shooting = "player_season_shooting", playmaking = "player_season_playmaking", defense = "player_season_defense_rebounding", roles = "player_season_roles", impact = "player_season_impact")
    rows <- lapply(seq_len(nrow(roster)), function(i) {
        row <- roster[i, , drop = FALSE]
        player_id <- suppressWarnings(as.integer(row$player_id[[1]]))
        performance_season <- latest_performance_season(player_id)
        stats_row <- latest_row(table_name = "player_season_stats", player_id = player_id, performance_season = performance_season)
        row$tbi_performance_available <- nrow(stats_row) > 0L
        row$performance_season_used <- if (nrow(stats_row)) {
            performance_season
        }
        else {
            NA_character_
        }
        for (evidence_name in names(evidence_tables)) {
            table_name <- unname(evidence_tables[evidence_name])
            extra <- if (identical(evidence_name, "stats")) {
                stats_row
            }
            else {
                latest_row(table_name = table_name, player_id = player_id, performance_season = performance_season)
            }
            row <- merge_evidence(base = row, extra = extra, prefix = paste0(evidence_name, "_"))
        }
        row
    })
    all_names <- unique(unlist(lapply(rows, names)))
    normalized <- lapply(rows, function(row) {
        missing <- setdiff(all_names, names(row))
        for (column in missing) {
            row[[column]] <- NA
        }
        row[, all_names, drop = FALSE]
    })
    do.call(rbind, normalized)
}
# <<< TBI_BIE_ROSTER_ENRICHMENT_COMPAT_END <<<

