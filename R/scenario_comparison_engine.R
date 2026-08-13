# ============================================================
# Thompson's Basketball Intelligence
# Phase 10: Scenario Comparison Engine
# ============================================================

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

scenario_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


scenario_integer <- function(x, default = NA_integer_) {
  value <- scenario_number(x, NA_real_)
  
  if (!is.finite(value)) {
    return(default)
  }
  
  as.integer(round(value))
}


scenario_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


scenario_flag <- function(x, default = FALSE) {
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


# ------------------------------------------------------------
# Rules
# ------------------------------------------------------------

scenario_comparison_rule_defaults <- function() {
  list(
    minutes_weight = 0.20,
    balanced_lineup_weight = 0.20,
    offense_lineup_weight = 0.15,
    defense_lineup_weight = 0.15,
    closing_lineup_weight = 0.15,
    rotation_quality_weight = 0.15,
    
    strong_positive_threshold = 4,
    positive_threshold = 1,
    negative_threshold = -1,
    strong_negative_threshold = -4,
    
    model_label = "TBI_SCENARIO_v1_COMPARISON"
  )
}


resolve_scenario_comparison_rules <- function(overrides = NULL) {
  rules <- scenario_comparison_rule_defaults()
  
  if (is.null(overrides)) {
    return(rules)
  }
  
  if (!is.list(overrides)) {
    stop(
      "Scenario-comparison rule overrides must be a list.",
      call. = FALSE
    )
  }
  
  unknown <- setdiff(
    names(overrides),
    names(rules)
  )
  
  if (length(unknown) > 0L) {
    stop(
      paste0(
        "Unknown scenario-comparison rule override(s): ",
        paste(unknown, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  for (name in names(overrides)) {
    rules[[name]] <- overrides[[name]]
  }
  
  total_weight <-
    rules$minutes_weight +
    rules$balanced_lineup_weight +
    rules$offense_lineup_weight +
    rules$defense_lineup_weight +
    rules$closing_lineup_weight +
    rules$rotation_quality_weight
  
  if (!isTRUE(all.equal(total_weight, 1, tolerance = 1e-8))) {
    stop(
      "Scenario comparison weights must sum to 1.",
      call. = FALSE
    )
  }
  
  rules
}


# ------------------------------------------------------------
# Input validation
# ------------------------------------------------------------

validate_scenario_roster <- function(roster,
                                     label = "roster") {
  if (!is.data.frame(roster)) {
    stop(
      paste0(
        label,
        " must be a data frame."
      ),
      call. = FALSE
    )
  }
  
  if (nrow(roster) < 5L) {
    stop(
      paste0(
        label,
        " must contain at least five players."
      ),
      call. = FALSE
    )
  }
  
  required_any <- c(
    "player_id",
    "PLAYER_ID",
    "id"
  )
  
  if (!any(required_any %in% names(roster))) {
    stop(
      paste0(
        label,
        " must contain a player identifier column."
      ),
      call. = FALSE
    )
  }
  
  roster
}


# ------------------------------------------------------------
# Phase 8 + Phase 9 wrappers
# ------------------------------------------------------------

build_scenario_state <- function(roster,
                                 rotation_size = NULL,
                                 manual_overrides = NULL,
                                 minute_rule_overrides = NULL,
                                 lineup_rule_overrides = NULL) {
  validate_scenario_roster(
    roster,
    label = "scenario roster"
  )
  
  if (!exists(
    "build_minute_allocation",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 8 Minute Allocation Engine is not loaded.",
      call. = FALSE
    )
  }
  
  if (!exists(
    "build_lineup_optimization",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 9 Lineup Optimization Engine is not loaded.",
      call. = FALSE
    )
  }
  
  minutes <- build_minute_allocation(
    roster = roster,
    rotation_size = rotation_size,
    manual_overrides = manual_overrides,
    rule_overrides = minute_rule_overrides
  )
  
  lineups <- build_lineup_optimization(
    roster_or_allocation = minutes,
    pool_size = rotation_size,
    rule_overrides = lineup_rule_overrides
  )
  
  list(
    roster = roster,
    minutes = minutes,
    lineups = lineups
  )
}


# ------------------------------------------------------------
# Rotation quality
# ------------------------------------------------------------

scenario_rotation_quality <- function(state) {
  allocation <- state$minutes$allocation
  
  if (is.null(allocation) || nrow(allocation) == 0L) {
    return(0)
  }
  
  active <- allocation[
    allocation$recommended_minutes > 0,
    ,
    drop = FALSE
  ]
  
  if (nrow(active) == 0L) {
    return(0)
  }
  
  weights <- active$recommended_minutes
  
  if (sum(weights) <= 0) {
    weights <- rep(1, nrow(active))
  }
  
  bie <- suppressWarnings(
    as.numeric(
      active$bie_rating
    )
  )
  
  bie[!is.finite(bie)] <- 50
  
  weighted.mean(
    bie,
    weights = weights
  )
}


scenario_rotation_depth <- function(state) {
  allocation <- state$minutes$allocation
  
  if (is.null(allocation) || nrow(allocation) == 0L) {
    return(0L)
  }
  
  sum(
    allocation$recommended_minutes > 0
  )
}


# ------------------------------------------------------------
# Player minute comparison
# ------------------------------------------------------------

compare_scenario_minutes <- function(base_state,
                                     scenario_state) {
  if (!exists(
    "compare_minute_allocations",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 8 minute comparison function is not loaded.",
      call. = FALSE
    )
  }
  
  compare_minute_allocations(
    base_state$minutes,
    scenario_state$minutes
  )
}


# ------------------------------------------------------------
# Lineup comparison
# ------------------------------------------------------------

compare_scenario_lineups <- function(base_state,
                                     scenario_state) {
  if (!exists(
    "compare_lineup_optimization",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 9 lineup comparison function is not loaded.",
      call. = FALSE
    )
  }
  
  compare_lineup_optimization(
    base_state$lineups,
    scenario_state$lineups
  )
}


# ------------------------------------------------------------
# Aggregate scoring
# ------------------------------------------------------------

scenario_lineup_score <- function(state,
                                  type) {
  item <- state$lineups[[type]]
  
  if (is.null(item)) {
    return(0)
  }
  
  scenario_number(
    item$score,
    0
  )
}


scenario_minutes_signal <- function(base_state,
                                    scenario_state) {
  base_quality <- scenario_rotation_quality(
    base_state
  )
  
  scenario_quality <- scenario_rotation_quality(
    scenario_state
  )
  
  scenario_quality - base_quality
}


scenario_rotation_quality_signal <- function(base_state,
                                             scenario_state) {
  base_quality <- scenario_rotation_quality(
    base_state
  )
  
  scenario_quality <- scenario_rotation_quality(
    scenario_state
  )
  
  scenario_quality - base_quality
}


score_scenario_comparison <- function(base_state,
                                      scenario_state,
                                      rule_overrides = NULL) {
  rules <- resolve_scenario_comparison_rules(
    rule_overrides
  )
  
  balanced_change <-
    scenario_lineup_score(
      scenario_state,
      "balanced"
    ) -
    scenario_lineup_score(
      base_state,
      "balanced"
    )
  
  offense_change <-
    scenario_lineup_score(
      scenario_state,
      "offense"
    ) -
    scenario_lineup_score(
      base_state,
      "offense"
    )
  
  defense_change <-
    scenario_lineup_score(
      scenario_state,
      "defense"
    ) -
    scenario_lineup_score(
      base_state,
      "defense"
    )
  
  closing_change <-
    scenario_lineup_score(
      scenario_state,
      "closing"
    ) -
    scenario_lineup_score(
      base_state,
      "closing"
    )
  
  minutes_change <- scenario_minutes_signal(
    base_state,
    scenario_state
  )
  
  rotation_quality_change <-
    scenario_rotation_quality_signal(
      base_state,
      scenario_state
    )
  
  composite <-
    rules$minutes_weight *
    minutes_change +
    rules$balanced_lineup_weight *
    balanced_change +
    rules$offense_lineup_weight *
    offense_change +
    rules$defense_lineup_weight *
    defense_change +
    rules$closing_lineup_weight *
    closing_change +
    rules$rotation_quality_weight *
    rotation_quality_change
  
  list(
    composite_score = composite,
    minutes_change = minutes_change,
    balanced_change = balanced_change,
    offense_change = offense_change,
    defense_change = defense_change,
    closing_change = closing_change,
    rotation_quality_change =
      rotation_quality_change
  )
}


# ------------------------------------------------------------
# Recommendation logic
# ------------------------------------------------------------

scenario_recommendation_label <- function(composite_score,
                                          rules = scenario_comparison_rule_defaults()) {
  score <- scenario_number(
    composite_score,
    0
  )
  
  if (score >= rules$strong_positive_threshold) {
    return("STRONGLY FAVOR SCENARIO")
  }
  
  if (score >= rules$positive_threshold) {
    return("FAVOR SCENARIO")
  }
  
  if (score <= rules$strong_negative_threshold) {
    return("STRONGLY FAVOR BASE")
  }
  
  if (score <= rules$negative_threshold) {
    return("FAVOR BASE")
  }
  
  "NEUTRAL / CLOSE CALL"
}


build_scenario_reasons <- function(score_result,
                                   minute_comparison,
                                   lineup_comparison) {
  reasons <- character(0)
  
  if (score_result$balanced_change > 0.5) {
    reasons <- c(
      reasons,
      "Balanced lineup quality improves."
    )
  }
  
  if (score_result$offense_change > 0.5) {
    reasons <- c(
      reasons,
      "Offensive lineup quality improves."
    )
  }
  
  if (score_result$defense_change > 0.5) {
    reasons <- c(
      reasons,
      "Defensive lineup quality improves."
    )
  }
  
  if (score_result$closing_change > 0.5) {
    reasons <- c(
      reasons,
      "Closing lineup quality improves."
    )
  }
  
  if (score_result$rotation_quality_change > 0.5) {
    reasons <- c(
      reasons,
      "Minute-weighted rotation quality improves."
    )
  }
  
  if (score_result$balanced_change < -0.5) {
    reasons <- c(
      reasons,
      "Balanced lineup quality declines."
    )
  }
  
  if (score_result$offense_change < -0.5) {
    reasons <- c(
      reasons,
      "Offensive lineup quality declines."
    )
  }
  
  if (score_result$defense_change < -0.5) {
    reasons <- c(
      reasons,
      "Defensive lineup quality declines."
    )
  }
  
  if (score_result$closing_change < -0.5) {
    reasons <- c(
      reasons,
      "Closing lineup quality declines."
    )
  }
  
  if (score_result$rotation_quality_change < -0.5) {
    reasons <- c(
      reasons,
      "Minute-weighted rotation quality declines."
    )
  }
  
  if (!is.null(minute_comparison) &&
      nrow(minute_comparison) > 0L) {
    biggest <- minute_comparison[
      which.max(
        abs(
          minute_comparison$minute_change
        )
      ),
      ,
      drop = FALSE
    ]
    
    reasons <- c(
      reasons,
      paste0(
        "Largest minute change: ",
        biggest$player_name[[1]],
        " ",
        ifelse(
          biggest$minute_change[[1]] >= 0,
          "+",
          ""
        ),
        biggest$minute_change[[1]],
        " minutes."
      )
    )
  }
  
  changed_lineups <- sum(
    lineup_comparison$lineup_changed,
    na.rm = TRUE
  )
  
  if (changed_lineups > 0L) {
    reasons <- c(
      reasons,
      paste0(
        changed_lineups,
        " optimized lineup type(s) change."
      )
    )
  }
  
  unique(reasons)
}


# ------------------------------------------------------------
# Main comparison engine
# ------------------------------------------------------------

build_scenario_comparison <- function(base_roster,
                                      scenario_roster,
                                      scenario_name = "Scenario",
                                      rotation_size = NULL,
                                      base_manual_overrides = NULL,
                                      scenario_manual_overrides = NULL,
                                      minute_rule_overrides = NULL,
                                      lineup_rule_overrides = NULL,
                                      comparison_rule_overrides = NULL) {
  validate_scenario_roster(
    base_roster,
    label = "base_roster"
  )
  
  validate_scenario_roster(
    scenario_roster,
    label = "scenario_roster"
  )
  
  rules <- resolve_scenario_comparison_rules(
    comparison_rule_overrides
  )
  
  base_state <- build_scenario_state(
    roster = base_roster,
    rotation_size = rotation_size,
    manual_overrides = base_manual_overrides,
    minute_rule_overrides = minute_rule_overrides,
    lineup_rule_overrides = lineup_rule_overrides
  )
  
  scenario_state <- build_scenario_state(
    roster = scenario_roster,
    rotation_size = rotation_size,
    manual_overrides = scenario_manual_overrides,
    minute_rule_overrides = minute_rule_overrides,
    lineup_rule_overrides = lineup_rule_overrides
  )
  
  minute_comparison <- compare_scenario_minutes(
    base_state,
    scenario_state
  )
  
  lineup_comparison <- compare_scenario_lineups(
    base_state,
    scenario_state
  )
  
  score_result <- score_scenario_comparison(
    base_state,
    scenario_state,
    rule_overrides = comparison_rule_overrides
  )
  
  recommendation <- scenario_recommendation_label(
    score_result$composite_score,
    rules = rules
  )
  
  reasons <- build_scenario_reasons(
    score_result,
    minute_comparison,
    lineup_comparison
  )
  
  list(
    scenario_name = scenario_text(
      scenario_name,
      "Scenario"
    ),
    recommendation = recommendation,
    composite_score =
      score_result$composite_score,
    reasons = reasons,
    
    base_state = base_state,
    scenario_state = scenario_state,
    
    minute_comparison = minute_comparison,
    lineup_comparison = lineup_comparison,
    
    score_detail = score_result,
    
    base_rotation_quality =
      scenario_rotation_quality(
        base_state
      ),
    scenario_rotation_quality =
      scenario_rotation_quality(
        scenario_state
      ),
    
    base_rotation_size =
      scenario_rotation_depth(
        base_state
      ),
    scenario_rotation_size =
      scenario_rotation_depth(
        scenario_state
      ),
    
    model_label = rules$model_label
  )
}


# ------------------------------------------------------------
# Executive tables
# ------------------------------------------------------------

scenario_scorecard_table <- function(result) {
  if (!is.list(result) ||
      is.null(result$score_detail)) {
    stop(
      "result must be a build_scenario_comparison result.",
      call. = FALSE
    )
  }
  
  detail <- result$score_detail
  
  data.frame(
    metric = c(
      "Composite",
      "Minute-weighted rotation",
      "Balanced lineup",
      "Offensive lineup",
      "Defensive lineup",
      "Closing lineup",
      "Rotation quality"
    ),
    change = c(
      detail$composite_score,
      detail$minutes_change,
      detail$balanced_change,
      detail$offense_change,
      detail$defense_change,
      detail$closing_change,
      detail$rotation_quality_change
    ),
    stringsAsFactors = FALSE
  )
}


scenario_executive_summary <- function(result) {
  if (!is.list(result)) {
    stop(
      "result must be a build_scenario_comparison result.",
      call. = FALSE
    )
  }
  
  reasons <- result$reasons
  
  reason_text <- if (length(reasons) > 0L) {
    paste(
      reasons,
      collapse = " "
    )
  } else {
    "The scenario produces only small basketball-impact changes."
  }
  
  paste0(
    result$recommendation,
    " — ",
    result$scenario_name,
    " posts a composite basketball-impact change of ",
    round(
      result$composite_score,
      2
    ),
    ". ",
    reason_text
  )
}