# ============================================================
# Thompson's Basketball Intelligence
# Phase 5C: Draft Simulation Intelligence Engine
# ============================================================

# This engine simulates draft-asset outcomes across protection,
# conveyance, swap, and multi-year scenario paths.
#
# It is independent of Shiny and designed to work with:
# - draft_assets_engine.R
# - draft_value_engine.R
#
# IMPORTANT:
# - Simulation results are decision-support estimates.
# - Official pick language and league records remain controlling.
# - The engine does not infer missing protection chains.
# - Lottery probabilities and expected slots must be supplied or
#   explicitly assumed.

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
draft_sim_number <- function(x, default = NA_real_) {
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
draft_sim_integer <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))
  
  if (!length(value) || is.na(value[[1]])) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to a clean character scalar
#' @noRd
draft_sim_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) default else value
}


#' Convert a value to logical
#' @noRd
draft_sim_flag <- function(x, default = FALSE) {
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


#' Clamp a probability to [0, 1]
#' @noRd
draft_sim_probability <- function(x, default = NA_real_) {
  value <- draft_sim_number(x, default)
  
  if (is.na(value)) {
    return(value)
  }
  
  if (value < 0 || value > 1) {
    stop(
      "Probability values must be between 0 and 1.",
      call. = FALSE
    )
  }
  
  value
}


# ------------------------------------------------------------
# Scenario assumptions
# ------------------------------------------------------------

#' Default simulation settings
#' @noRd
draft_simulation_rule_defaults <- function() {
  list(
    model_label = "TBI Draft Simulation Model v1",
    simulation_iterations = 5000L,
    random_seed = 42L,
    current_year = as.integer(format(Sys.Date(), "%Y")),
    
    expected_slot_sd = c(
      first = 5.0,
      second = 6.0
    ),
    
    minimum_first_slot = 1L,
    maximum_first_slot = 30L,
    minimum_second_slot = 31L,
    maximum_second_slot = 60L,
    
    default_conveyance_probability = 0.70,
    default_swap_exercise_probability = 0.35,
    default_condition_resolution_probability = 0.50,
    
    best_case_quantile = 0.90,
    expected_case_quantile = 0.50,
    worst_case_quantile = 0.10
  )
}


#' Merge default simulation rules with overrides
#' @noRd
resolve_draft_simulation_rules <- function(rule_overrides = NULL) {
  rules <- draft_simulation_rule_defaults()
  
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
        "Unknown draft-simulation rule override(s): ",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  rules[names(rule_overrides)] <- rule_overrides
  
  rules
}


# ------------------------------------------------------------
# Asset and condition normalization
# ------------------------------------------------------------

#' Normalize one draft asset for simulation
#' @noRd
validate_draft_simulation_asset <- function(asset) {
  if (is.data.frame(asset)) {
    if (nrow(asset) != 1L) {
      stop(
        "asset data frame must contain exactly one row.",
        call. = FALSE
      )
    }
    
    asset <- lapply(
      as.list(asset[1, , drop = FALSE]),
      function(x) x[[1]]
    )
  }
  
  if (!is.list(asset)) {
    stop(
      "asset must be a named list or one-row data frame.",
      call. = FALSE
    )
  }
  
  required <- c(
    "draft_year",
    "round",
    "control_type"
  )
  
  missing <- setdiff(required, names(asset))
  
  if (length(missing)) {
    stop(
      paste0(
        "asset is missing required field(s): ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  round_value <- if (
    exists("normalize_draft_value_round", mode = "function")
  ) {
    normalize_draft_value_round(asset$round)
  } else {
    value <- tolower(draft_sim_text(asset$round))
    if (value %in% c("1", "1st", "first", "first round")) {
      "First"
    } else if (value %in% c("2", "2nd", "second", "second round")) {
      "Second"
    } else {
      stop("Unsupported draft round.", call. = FALSE)
    }
  }
  
  list(
    draft_asset_id = draft_sim_integer(
      asset$draft_asset_id,
      NA_integer_
    ),
    draft_year = draft_sim_integer(
      asset$draft_year,
      NA_integer_
    ),
    round = round_value,
    control_type = draft_sim_text(
      asset$control_type
    ),
    protection_type = draft_sim_text(
      asset$protection_type,
      "Unspecified"
    ),
    verification_status = draft_sim_text(
      asset$verification_status,
      "Unverified"
    ),
    strategic_value = draft_sim_text(
      asset$strategic_value,
      "Unrated"
    ),
    condition_count = draft_sim_integer(
      asset$condition_count,
      0L
    ),
    expected_slot = draft_sim_integer(
      asset$expected_slot,
      NA_integer_
    ),
    conveyance_probability = draft_sim_probability(
      asset$conveyance_probability,
      NA_real_
    ),
    swap_exercise_probability = draft_sim_probability(
      asset$swap_exercise_probability,
      NA_real_
    ),
    current_team = draft_sim_text(
      asset$current_team
    ),
    original_team = draft_sim_text(
      asset$original_team
    )
  )
}


#' Normalize a condition-chain data frame
#' @noRd
normalize_draft_conditions <- function(conditions) {
  if (is.null(conditions)) {
    return(
      data.frame(
        condition_order = integer(),
        condition_year = integer(),
        condition_text = character(),
        outcome_if_conveys = character(),
        outcome_if_not_conveyed = character(),
        converts_to_year = integer(),
        converts_to_round = character(),
        is_final_condition = integer(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  if (!is.data.frame(conditions)) {
    stop(
      "conditions must be a data frame.",
      call. = FALSE
    )
  }
  
  required <- c(
    "condition_order",
    "condition_text"
  )
  
  missing <- setdiff(required, names(conditions))
  
  if (length(missing)) {
    stop(
      paste0(
        "conditions are missing required field(s): ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  result <- conditions
  
  optional <- list(
    condition_year = NA_integer_,
    outcome_if_conveys = "",
    outcome_if_not_conveyed = "",
    converts_to_year = NA_integer_,
    converts_to_round = "",
    is_final_condition = 0L
  )
  
  for (name in names(optional)) {
    if (!name %in% names(result)) {
      result[[name]] <- optional[[name]]
    }
  }
  
  result$condition_order <- as.integer(
    result$condition_order
  )
  
  result$condition_year <- suppressWarnings(
    as.integer(result$condition_year)
  )
  
  result$converts_to_year <- suppressWarnings(
    as.integer(result$converts_to_year)
  )
  
  result$is_final_condition <- as.integer(
    result$is_final_condition
  )
  
  result <- result[
    order(result$condition_order),
    ,
    drop = FALSE
  ]
  
  rownames(result) <- NULL
  
  result
}


# ------------------------------------------------------------
# Slot simulation
# ------------------------------------------------------------

#' Simulate an expected draft slot
#' @noRd
simulate_draft_slot <- function(round_value,
                                expected_slot = NULL,
                                slot_sd = NULL,
                                rules = draft_simulation_rule_defaults()) {
  round_value <- if (
    exists("normalize_draft_value_round", mode = "function")
  ) {
    normalize_draft_value_round(round_value)
  } else {
    draft_sim_text(round_value)
  }
  
  if (round_value == "First") {
    default_slot <- 16L
    minimum_slot <- draft_sim_integer(
      rules$minimum_first_slot,
      1L
    )
    maximum_slot <- draft_sim_integer(
      rules$maximum_first_slot,
      30L
    )
    default_sd <- draft_sim_number(
      rules$expected_slot_sd[["first"]],
      5
    )
  } else {
    default_slot <- 45L
    minimum_slot <- draft_sim_integer(
      rules$minimum_second_slot,
      31L
    )
    maximum_slot <- draft_sim_integer(
      rules$maximum_second_slot,
      60L
    )
    default_sd <- draft_sim_number(
      rules$expected_slot_sd[["second"]],
      6
    )
  }
  
  expected_slot <- draft_sim_integer(
    expected_slot,
    default_slot
  )
  
  slot_sd <- draft_sim_number(
    slot_sd,
    default_sd
  )
  
  if (slot_sd < 0) {
    stop(
      "slot_sd cannot be negative.",
      call. = FALSE
    )
  }
  
  simulated <- round(
    stats::rnorm(
      1,
      mean = expected_slot,
      sd = slot_sd
    )
  )
  
  as.integer(
    min(
      max(simulated, minimum_slot),
      maximum_slot
    )
  )
}


# ------------------------------------------------------------
# Conveyance and swap logic
# ------------------------------------------------------------

#' Determine whether an asset conveys in the current simulation
#' @noRd

simulate_conveyance <- function(asset,
                                rules = draft_simulation_rule_defaults()) {
  asset <- validate_draft_simulation_asset(
    asset
  )
  
  probability <- draft_sim_probability(
    asset$conveyance_probability,
    rules$default_conveyance_probability
  )
  
  conveyed <- if (probability <= 0) {
    FALSE
  } else if (probability >= 1) {
    TRUE
  } else {
    stats::runif(1) <= probability
  }
  
  list(
    conveyed = conveyed,
    probability = probability
  )
}

#' Determine whether a swap right is exercised
#' @noRd
simulate_swap_exercise <- function(asset,
                                   rules = draft_simulation_rule_defaults()) {
  asset <- validate_draft_simulation_asset(
    asset
  )
  
  probability <- draft_sim_probability(
    asset$swap_exercise_probability,
    rules$default_swap_exercise_probability
  )
  
  exercised <- if (probability <= 0) {
    FALSE
  } else if (probability >= 1) {
    TRUE
  } else {
    stats::runif(1) <= probability
  }
  
  list(
    exercised = exercised,
    probability = probability
  )
}


#' Resolve the next condition path
#' @noRd
resolve_condition_path <- function(asset,
                                   conditions,
                                   conveyed) {
  asset <- validate_draft_simulation_asset(asset)
  conditions <- normalize_draft_conditions(conditions)
  
  if (!nrow(conditions)) {
    return(
      list(
        resolved_year = asset$draft_year,
        resolved_round = asset$round,
        terminal = TRUE,
        resolution_text = if (conveyed) {
          "Asset conveyed with no additional condition chain."
        } else {
          "Asset did not convey and no additional condition chain was loaded."
        }
      )
    )
  }
  
  current <- conditions[1, , drop = FALSE]
  
  if (isTRUE(conveyed)) {
    return(
      list(
        resolved_year = current$condition_year[[1]] %||%
          asset$draft_year,
        resolved_round = asset$round,
        terminal = TRUE,
        resolution_text = draft_sim_text(
          current$outcome_if_conveys[[1]],
          "Asset conveyed under the loaded condition."
        )
      )
    )
  }
  
  next_year <- current$converts_to_year[[1]]
  next_round <- draft_sim_text(
    current$converts_to_round[[1]],
    asset$round
  )
  
  terminal <- draft_sim_flag(
    current$is_final_condition[[1]],
    FALSE
  ) || is.na(next_year)
  
  list(
    resolved_year = if (is.na(next_year)) {
      asset$draft_year
    } else {
      next_year
    },
    resolved_round = next_round,
    terminal = terminal,
    resolution_text = draft_sim_text(
      current$outcome_if_not_conveyed[[1]],
      if (terminal) {
        "Asset did not convey and reached the terminal loaded condition."
      } else {
        "Asset did not convey and rolled to the next loaded condition."
      }
    )
  )
}


# ------------------------------------------------------------
# Single-iteration asset simulation
# ------------------------------------------------------------

#' Simulate one asset outcome
#' @noRd
simulate_draft_asset_once <- function(asset,
                                      conditions = NULL,
                                      current_year = NULL,
                                      rule_overrides = NULL) {
  rules <- resolve_draft_simulation_rules(
    rule_overrides
  )
  
  asset <- validate_draft_simulation_asset(
    asset
  )
  
  current_year <- draft_sim_integer(
    current_year,
    rules$current_year
  )
  
  simulated_slot <- simulate_draft_slot(
    round_value = asset$round,
    expected_slot = asset$expected_slot,
    rules = rules
  )
  
  control_key <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(asset$control_type)
  )
  
  conveyed <- TRUE
  swap_exercised <- FALSE
  resolution <- list(
    resolved_year = asset$draft_year,
    resolved_round = asset$round,
    terminal = TRUE,
    resolution_text = "No conditional event required."
  )
  
  if (
    control_key %in%
    c("incoming", "outgoing")
  ) {
    conveyance <- simulate_conveyance(
      asset,
      rules = rules
    )
    
    conveyed <- conveyance$conveyed
    
    resolution <- resolve_condition_path(
      asset = asset,
      conditions = conditions,
      conveyed = conveyed
    )
  }
  
  if (
    control_key %in%
    c("swap_right", "swap_obligation")
  ) {
    swap <- simulate_swap_exercise(
      asset,
      rules = rules
    )
    
    swap_exercised <- swap$exercised
  }
  
  effective_control_type <- asset$control_type
  
  if (
    control_key == "swap_right" &&
    !swap_exercised
  ) {
    effective_control_type <- "Own"
  }
  
  if (
    control_key == "swap_obligation" &&
    !swap_exercised
  ) {
    effective_control_type <- "Own"
  }
  
  simulation_asset <- list(
    draft_asset_id = asset$draft_asset_id,
    draft_year = resolution$resolved_year,
    round = resolution$resolved_round,
    control_type = effective_control_type,
    protection_type = asset$protection_type,
    verification_status = asset$verification_status,
    strategic_value = asset$strategic_value,
    condition_count = asset$condition_count,
    expected_slot = simulated_slot,
    internal_value = NA_real_
  )
  
  if (
    !conveyed &&
    isTRUE(resolution$terminal) &&
    resolution$resolved_year == asset$draft_year
  ) {
    value_result <- list(
      blended_value_score = 0,
      value_tier = "Minimal"
    )
  } else {
    if (!exists("evaluate_draft_asset_value", mode = "function")) {
      stop(
        "evaluate_draft_asset_value() is required from draft_value_engine.R.",
        call. = FALSE
      )
    }
    
    value_result <- evaluate_draft_asset_value(
      asset = simulation_asset,
      current_year = current_year
    )
  }
  
  list(
    draft_asset_id = asset$draft_asset_id,
    simulated_slot = simulated_slot,
    conveyed = conveyed,
    swap_exercised = swap_exercised,
    resolved_year = resolution$resolved_year,
    resolved_round = resolution$resolved_round,
    terminal = resolution$terminal,
    resolution_text = resolution$resolution_text,
    simulated_value = value_result$blended_value_score,
    value_tier = value_result$value_tier
  )
}


# ------------------------------------------------------------
# Monte Carlo simulation
# ------------------------------------------------------------

#' Simulate one asset across many iterations
#' @noRd
simulate_draft_asset <- function(asset,
                                 conditions = NULL,
                                 iterations = NULL,
                                 current_year = NULL,
                                 random_seed = NULL,
                                 rule_overrides = NULL) {
  rules <- resolve_draft_simulation_rules(
    rule_overrides
  )
  
  iterations <- draft_sim_integer(
    iterations,
    rules$simulation_iterations
  )
  
  if (is.na(iterations) || iterations < 100L) {
    stop(
      "iterations must be at least 100.",
      call. = FALSE
    )
  }
  
  random_seed <- draft_sim_integer(
    random_seed,
    rules$random_seed
  )
  
  set.seed(random_seed)
  
  results <- lapply(
    seq_len(iterations),
    function(i) {
      simulate_draft_asset_once(
        asset = asset,
        conditions = conditions,
        current_year = current_year,
        rule_overrides = rule_overrides
      )
    }
  )
  
  values <- vapply(
    results,
    function(x) x$simulated_value,
    numeric(1)
  )
  
  slots <- vapply(
    results,
    function(x) x$simulated_slot,
    integer(1)
  )
  
  conveyance_rate <- mean(
    vapply(
      results,
      function(x) x$conveyed,
      logical(1)
    )
  )
  
  swap_exercise_rate <- mean(
    vapply(
      results,
      function(x) x$swap_exercised,
      logical(1)
    )
  )
  
  quantiles <- stats::quantile(
    values,
    probs = c(
      rules$worst_case_quantile,
      rules$expected_case_quantile,
      rules$best_case_quantile
    ),
    names = FALSE,
    na.rm = TRUE
  )
  
  list(
    iterations = iterations,
    mean_value = mean(values),
    median_value = stats::median(values),
    value_sd = stats::sd(values),
    worst_case_value = quantiles[[1]],
    expected_case_value = quantiles[[2]],
    best_case_value = quantiles[[3]],
    mean_slot = mean(slots),
    median_slot = stats::median(slots),
    conveyance_rate = conveyance_rate,
    swap_exercise_rate = swap_exercise_rate,
    simulation_results = data.frame(
      iteration = seq_len(iterations),
      simulated_slot = slots,
      conveyed = vapply(
        results,
        function(x) x$conveyed,
        logical(1)
      ),
      swap_exercised = vapply(
        results,
        function(x) x$swap_exercised,
        logical(1)
      ),
      resolved_year = vapply(
        results,
        function(x) x$resolved_year,
        integer(1)
      ),
      resolved_round = vapply(
        results,
        function(x) x$resolved_round,
        character(1)
      ),
      simulated_value = values,
      value_tier = vapply(
        results,
        function(x) x$value_tier,
        character(1)
      ),
      stringsAsFactors = FALSE
    ),
    model_label = rules$model_label,
    scope_note = paste(
      "Simulation outcomes depend on supplied probabilities,",
      "expected slots, and loaded condition language."
    )
  )
}


# ------------------------------------------------------------
# Portfolio simulation
# ------------------------------------------------------------

#' Simulate a full portfolio
#' @noRd
simulate_draft_portfolio <- function(assets,
                                     conditions_lookup = NULL,
                                     iterations = NULL,
                                     current_year = NULL,
                                     random_seed = NULL,
                                     rule_overrides = NULL) {
  if (!is.data.frame(assets)) {
    stop(
      "assets must be a data frame.",
      call. = FALSE
    )
  }
  
  if (!nrow(assets)) {
    return(
      list(
        iterations = 0L,
        mean_portfolio_value = 0,
        median_portfolio_value = 0,
        worst_case_value = 0,
        expected_case_value = 0,
        best_case_value = 0,
        simulation_results = data.frame(),
        executive_summary =
          "No draft assets are available for simulation."
      )
    )
  }
  
  rules <- resolve_draft_simulation_rules(
    rule_overrides
  )
  
  iterations <- draft_sim_integer(
    iterations,
    rules$simulation_iterations
  )
  
  if (is.na(iterations) || iterations < 100L) {
    stop(
      "iterations must be at least 100.",
      call. = FALSE
    )
  }
  
  random_seed <- draft_sim_integer(
    random_seed,
    rules$random_seed
  )
  
  set.seed(random_seed)
  
  portfolio_values <- numeric(iterations)
  
  for (i in seq_len(iterations)) {
    iteration_value <- 0
    
    for (row_index in seq_len(nrow(assets))) {
      asset_row <- assets[row_index, , drop = FALSE]
      
      asset_id <- if (
        "draft_asset_id" %in% names(asset_row)
      ) {
        as.character(asset_row$draft_asset_id[[1]])
      } else {
        as.character(row_index)
      }
      
      conditions <- NULL
      
      if (
        !is.null(conditions_lookup) &&
        is.list(conditions_lookup) &&
        asset_id %in% names(conditions_lookup)
      ) {
        conditions <- conditions_lookup[[asset_id]]
      }
      
      result <- simulate_draft_asset_once(
        asset = asset_row,
        conditions = conditions,
        current_year = current_year,
        rule_overrides = rule_overrides
      )
      
      iteration_value <- iteration_value +
        result$simulated_value
    }
    
    portfolio_values[[i]] <- iteration_value
  }
  
  quantiles <- stats::quantile(
    portfolio_values,
    probs = c(
      rules$worst_case_quantile,
      rules$expected_case_quantile,
      rules$best_case_quantile
    ),
    names = FALSE,
    na.rm = TRUE
  )
  
  mean_value <- mean(portfolio_values)
  median_value <- stats::median(portfolio_values)
  
  executive_summary <- paste0(
    "Across ",
    iterations,
    " simulations, estimated portfolio value averaged ",
    round(mean_value, 1),
    " points. The expected-case value was ",
    round(quantiles[[2]], 1),
    ", with a simulated range from ",
    round(quantiles[[1]], 1),
    " in the downside case to ",
    round(quantiles[[3]], 1),
    " in the upside case."
  )
  
  list(
    iterations = iterations,
    mean_portfolio_value = mean_value,
    median_portfolio_value = median_value,
    portfolio_value_sd = stats::sd(portfolio_values),
    worst_case_value = quantiles[[1]],
    expected_case_value = quantiles[[2]],
    best_case_value = quantiles[[3]],
    simulation_results = data.frame(
      iteration = seq_len(iterations),
      portfolio_value = portfolio_values,
      stringsAsFactors = FALSE
    ),
    executive_summary = executive_summary,
    model_label = rules$model_label,
    scope_note = paste(
      "Portfolio simulation is an internal uncertainty estimate,",
      "not a forecast of actual draft order or conveyance."
    )
  )
}


# ------------------------------------------------------------
# Database-backed simulation
# ------------------------------------------------------------

#' Load persistent assets and condition chains for simulation
#' @noRd
get_draft_simulation_inputs <- function(team_value,
                                        year_from = NULL,
                                        year_to = NULL,
                                        db_path = NULL) {
  if (!exists("get_draft_assets", mode = "function")) {
    stop(
      "get_draft_assets() is required from draft_assets_engine.R.",
      call. = FALSE
    )
  }
  
  if (!exists("get_draft_asset_detail", mode = "function")) {
    stop(
      "get_draft_asset_detail() is required from draft_assets_engine.R.",
      call. = FALSE
    )
  }
  
  assets <- get_draft_assets(
    team_value = team_value,
    year_from = year_from,
    year_to = year_to,
    include_inactive = FALSE,
    db_path = db_path
  )
  
  conditions_lookup <- list()
  
  if (nrow(assets)) {
    for (i in seq_len(nrow(assets))) {
      asset_id <- assets$draft_asset_id[[i]]
      
      detail <- get_draft_asset_detail(
        draft_asset_id = asset_id,
        db_path = db_path
      )
      
      conditions_lookup[[as.character(asset_id)]] <-
        detail$conditions
    }
  }
  
  list(
    assets = assets,
    conditions_lookup = conditions_lookup
  )
}


#' Simulate one team's persistent draft portfolio
#' @noRd
simulate_team_draft_portfolio <- function(team_value,
                                          year_from = NULL,
                                          year_to = NULL,
                                          iterations = NULL,
                                          current_year = NULL,
                                          random_seed = NULL,
                                          db_path = NULL,
                                          rule_overrides = NULL) {
  inputs <- get_draft_simulation_inputs(
    team_value = team_value,
    year_from = year_from,
    year_to = year_to,
    db_path = db_path
  )
  
  result <- simulate_draft_portfolio(
    assets = inputs$assets,
    conditions_lookup = inputs$conditions_lookup,
    iterations = iterations,
    current_year = current_year,
    random_seed = random_seed,
    rule_overrides = rule_overrides
  )
  
  result$team <- team_value
  result$assets <- inputs$assets
  
  result
}