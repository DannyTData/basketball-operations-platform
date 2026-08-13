# ============================================================
# Thompson's Basketball Intelligence
# Phase 5B: Draft Value Intelligence Engine
# ============================================================

# This engine estimates the strategic value of persistent draft assets.
# It is independent of Shiny and can be reused by the Draft Intelligence,
# Trade Intelligence, Executive Dashboard, and Five-Year Outlook modules.
#
# IMPORTANT:
# - Draft value is inherently uncertain and organization-specific.
# - The engine produces transparent internal estimates, not market truth.
# - Every score is decomposed into assumptions so decision-makers can see
#   how year, round, expected slot, protection, control type, and verification
#   status affect the final value.

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------

#' Null-coalescing helper
#' @noRd
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}


#' Convert a value to a safe numeric scalar
#' @noRd
draft_value_number <- function(x, default = NA_real_) {
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
draft_value_integer <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))
  
  if (!length(value) || is.na(value[[1]])) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to a clean character scalar
#' @noRd
draft_value_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) default else value
}


#' Convert a value to a logical scalar
#' @noRd
draft_value_flag <- function(x, default = FALSE) {
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


#' Clamp a numeric value to a range
#' @noRd
draft_value_clamp <- function(x, minimum = 0, maximum = 100) {
  x <- draft_value_number(x, minimum)
  
  min(max(x, minimum), maximum)
}


# ------------------------------------------------------------
# Centralized valuation settings
# ------------------------------------------------------------

#' Return default draft-value assumptions
#'
#' @return Named list of transparent valuation assumptions.
#' @noRd
draft_value_rule_defaults <- function() {
  list(
    model_label = "TBI Draft Asset Value Model v1",
    
    current_year = as.integer(
      format(
        Sys.Date(),
        "%Y"
      )
    ),
    
    first_round_slot_values = c(
      `1` = 100.0,
      `2` = 94.0,
      `3` = 89.0,
      `4` = 84.0,
      `5` = 80.0,
      `6` = 76.0,
      `7` = 72.0,
      `8` = 68.0,
      `9` = 64.0,
      `10` = 60.0,
      `11` = 56.0,
      `12` = 53.0,
      `13` = 50.0,
      `14` = 47.0,
      `15` = 44.0,
      `16` = 42.0,
      `17` = 40.0,
      `18` = 38.0,
      `19` = 36.0,
      `20` = 34.0,
      `21` = 32.0,
      `22` = 30.0,
      `23` = 28.0,
      `24` = 26.0,
      `25` = 24.0,
      `26` = 22.0,
      `27` = 20.0,
      `28` = 18.0,
      `29` = 16.0,
      `30` = 14.0
    ),
    
    second_round_slot_values = c(
      `31` = 12.0,
      `32` = 11.5,
      `33` = 11.0,
      `34` = 10.5,
      `35` = 10.0,
      `36` = 9.5,
      `37` = 9.0,
      `38` = 8.5,
      `39` = 8.0,
      `40` = 7.5,
      `41` = 7.0,
      `42` = 6.5,
      `43` = 6.0,
      `44` = 5.5,
      `45` = 5.0,
      `46` = 4.6,
      `47` = 4.2,
      `48` = 3.8,
      `49` = 3.4,
      `50` = 3.0,
      `51` = 2.7,
      `52` = 2.4,
      `53` = 2.1,
      `54` = 1.8,
      `55` = 1.5,
      `56` = 1.2,
      `57` = 1.0,
      `58` = 0.8,
      `59` = 0.6,
      `60` = 0.5
    ),
    
    default_expected_slots = c(
      first = 16L,
      second = 45L
    ),
    
    annual_discount_rate = 0.08,
    
    protection_multipliers = c(
      unprotected = 1.00,
      lottery_protected = 0.72,
      top_n_protected = 0.78,
      range_protected = 0.68,
      best_of = 0.82,
      worst_of = 0.58,
      conditional = 0.62,
      none = 1.00,
      unspecified = 0.50
    ),
    
    control_type_multipliers = c(
      own = 1.00,
      incoming = 1.00,
      outgoing = -1.00,
      swap_right = 0.45,
      swap_obligation = -0.35
    ),
    
    verification_multipliers = c(
      verified = 1.00,
      needs_review = 0.90,
      unverified = 0.80,
      superseded = 0.00
    ),
    
    strategic_value_multipliers = c(
      high = 1.10,
      medium = 1.00,
      low = 0.90,
      unrated = 1.00
    ),
    
    condition_penalty_per_step = 0.05,
    minimum_condition_multiplier = 0.60,
    
    value_tier_breaks = c(
      premium = 70,
      strong = 45,
      useful = 20,
      limited = 5
    )
  )
}


#' Merge default valuation rules with overrides
#' @noRd
resolve_draft_value_rules <- function(rule_overrides = NULL) {
  rules <- draft_value_rule_defaults()
  
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
        "Unknown draft-value rule override(s): ",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  rules[names(rule_overrides)] <- rule_overrides
  
  rules
}


# ------------------------------------------------------------
# Slot and time value
# ------------------------------------------------------------

#' Normalize a draft round
#' @noRd
normalize_draft_value_round <- function(round_value) {
  value <- tolower(
    trimws(
      draft_value_text(round_value)
    )
  )
  
  if (value %in% c("1", "1st", "first", "first round")) {
    return("First")
  }
  
  if (value %in% c("2", "2nd", "second", "second round")) {
    return("Second")
  }
  
  stop(
    paste0(
      "Unsupported draft round: ",
      draft_value_text(round_value, "<blank>"),
      "."
    ),
    call. = FALSE
  )
}


#' Return a slot value from the configured curve
#' @noRd
draft_slot_value <- function(round_value,
                             expected_slot = NULL,
                             rules = draft_value_rule_defaults()) {
  round_value <- normalize_draft_value_round(
    round_value
  )
  
  default_slot <- if (round_value == "First") {
    draft_value_integer(
      rules$default_expected_slots[["first"]],
      16L
    )
  } else {
    draft_value_integer(
      rules$default_expected_slots[["second"]],
      45L
    )
  }
  
  expected_slot <- draft_value_integer(
    expected_slot,
    default_slot
  )
  
  valid_range <- if (round_value == "First") {
    1:30
  } else {
    31:60
  }
  
  if (!expected_slot %in% valid_range) {
    stop(
      paste0(
        "expected_slot must be between ",
        min(valid_range),
        " and ",
        max(valid_range),
        " for a ",
        tolower(round_value),
        "-round pick."
      ),
      call. = FALSE
    )
  }
  
  curve <- if (round_value == "First") {
    rules$first_round_slot_values
  } else {
    rules$second_round_slot_values
  }
  
  if (is.null(curve) || !as.character(expected_slot) %in% names(curve)) {
    stop(
      "The configured slot-value curve is incomplete.",
      call. = FALSE
    )
  }
  
  list(
    round = round_value,
    expected_slot = expected_slot,
    base_slot_value = draft_value_number(
      curve[[as.character(expected_slot)]],
      NA_real_
    )
  )
}


#' Calculate the time discount for a future draft asset
#' @noRd
draft_time_discount <- function(draft_year,
                                current_year = NULL,
                                annual_discount_rate = NULL,
                                rules = draft_value_rule_defaults()) {
  draft_year <- draft_value_integer(
    draft_year,
    NA_integer_
  )
  
  current_year <- draft_value_integer(
    current_year,
    rules$current_year
  )
  
  annual_discount_rate <- draft_value_number(
    annual_discount_rate,
    rules$annual_discount_rate
  )
  
  if (is.na(draft_year) || is.na(current_year)) {
    stop(
      "draft_year and current_year are required.",
      call. = FALSE
    )
  }
  
  if (annual_discount_rate < 0 || annual_discount_rate >= 1) {
    stop(
      "annual_discount_rate must be between 0 and 1.",
      call. = FALSE
    )
  }
  
  years_out <- max(draft_year - current_year, 0L)
  multiplier <- 1 / ((1 + annual_discount_rate)^years_out)
  
  list(
    draft_year = draft_year,
    current_year = current_year,
    years_out = years_out,
    annual_discount_rate = annual_discount_rate,
    time_multiplier = multiplier
  )
}


# ------------------------------------------------------------
# Multipliers
# ------------------------------------------------------------

#' Normalize a label for rule lookup
#' @noRd
draft_value_lookup_key <- function(x) {
  value <- tolower(
    trimws(
      draft_value_text(x)
    )
  )
  
  gsub(
    "[^a-z0-9]+",
    "_",
    value
  )
}


#' Resolve a protection multiplier
#' @noRd
draft_protection_multiplier <- function(protection_type,
                                        rules = draft_value_rule_defaults()) {
  key <- draft_value_lookup_key(
    protection_type
  )
  
  aliases <- c(
    "unprotected" = "unprotected",
    "lottery_protected" = "lottery_protected",
    "top_n_protected" = "top_n_protected",
    "range_protected" = "range_protected",
    "best_of" = "best_of",
    "worst_of" = "worst_of",
    "conditional" = "conditional",
    "none" = "none",
    "unspecified" = "unspecified"
  )
  
  resolved <- aliases[[key]]
  
  if (is.null(resolved)) {
    resolved <- "unspecified"
  }
  
  multiplier <- rules$protection_multipliers[[resolved]]
  
  if (is.null(multiplier)) {
    stop(
      "The configured protection multipliers are incomplete.",
      call. = FALSE
    )
  }
  
  draft_value_number(multiplier, 0.50)
}


#' Resolve a control-type multiplier
#' @noRd
draft_control_type_multiplier <- function(control_type,
                                          rules = draft_value_rule_defaults()) {
  key <- draft_value_lookup_key(
    control_type
  )
  
  aliases <- c(
    "own" = "own",
    "incoming" = "incoming",
    "outgoing" = "outgoing",
    "swap_right" = "swap_right",
    "swap_obligation" = "swap_obligation"
  )
  
  if (!key %in% names(aliases)) {
    stop(
      paste0(
        "Unsupported control_type: ",
        draft_value_text(control_type, "<blank>"),
        "."
      ),
      call. = FALSE
    )
  }
  
  resolved <- unname(
    aliases[[key]]
  )
  
  multiplier <- rules$control_type_multipliers[[resolved]]
  
  if (is.null(multiplier)) {
    stop(
      "The configured control-type multipliers are incomplete.",
      call. = FALSE
    )
  }
  
  draft_value_number(multiplier, 0)
}


#' Resolve a verification multiplier
#' @noRd
draft_verification_multiplier <- function(verification_status,
                                          rules = draft_value_rule_defaults()) {
  key <- draft_value_lookup_key(
    verification_status
  )
  
  if (!key %in% names(rules$verification_multipliers)) {
    key <- "unverified"
  }
  
  draft_value_number(
    rules$verification_multipliers[[key]],
    0.80
  )
}


#' Resolve a strategic-value multiplier
#' @noRd
draft_strategic_multiplier <- function(strategic_value,
                                       rules = draft_value_rule_defaults()) {
  key <- draft_value_lookup_key(
    strategic_value
  )
  
  if (!key %in% names(rules$strategic_value_multipliers)) {
    key <- "unrated"
  }
  
  draft_value_number(
    rules$strategic_value_multipliers[[key]],
    1.00
  )
}


#' Calculate a condition-chain multiplier
#' @noRd
draft_condition_multiplier <- function(condition_count,
                                       rules = draft_value_rule_defaults()) {
  condition_count <- max(
    draft_value_integer(
      condition_count,
      0L
    ),
    0L
  )
  
  penalty <- draft_value_number(
    rules$condition_penalty_per_step,
    0.05
  )
  
  minimum <- draft_value_number(
    rules$minimum_condition_multiplier,
    0.60
  )
  
  max(
    1 - condition_count * penalty,
    minimum
  )
}


# ------------------------------------------------------------
# Asset validation and scoring
# ------------------------------------------------------------

#' Validate and normalize one draft asset for valuation
#' @noRd
validate_draft_value_asset <- function(asset) {
  if (is.data.frame(asset)) {
    if (nrow(asset) != 1L) {
      stop(
        "asset data frame must contain exactly one row.",
        call. = FALSE
      )
    }
    
    asset <- as.list(asset[1, , drop = FALSE])
    asset <- lapply(
      asset,
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
  
  missing <- setdiff(
    required,
    names(asset)
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "asset is missing required field(s): ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  list(
    draft_asset_id = draft_value_integer(
      asset$draft_asset_id,
      NA_integer_
    ),
    draft_year = draft_value_integer(
      asset$draft_year,
      NA_integer_
    ),
    round = normalize_draft_value_round(
      asset$round
    ),
    control_type = draft_value_text(
      asset$control_type
    ),
    protection_type = draft_value_text(
      asset$protection_type,
      "Unspecified"
    ),
    verification_status = draft_value_text(
      asset$verification_status,
      "Unverified"
    ),
    strategic_value = draft_value_text(
      asset$strategic_value,
      "Unrated"
    ),
    condition_count = draft_value_integer(
      asset$condition_count,
      0L
    ),
    expected_slot = draft_value_integer(
      asset$expected_slot,
      NA_integer_
    ),
    internal_value = draft_value_number(
      asset$internal_value,
      NA_real_
    ),
    original_team = draft_value_text(
      asset$original_team
    ),
    current_team = draft_value_text(
      asset$current_team
    ),
    notes = draft_value_text(
      asset$notes
    )
  )
}


#' Convert a numeric score to a value tier
#' @noRd
draft_value_tier <- function(score,
                             rules = draft_value_rule_defaults()) {
  score <- abs(
    draft_value_number(
      score,
      0
    )
  )
  
  breaks <- rules$value_tier_breaks
  
  if (score >= breaks[["premium"]]) {
    return("Premium")
  }
  
  if (score >= breaks[["strong"]]) {
    return("Strong")
  }
  
  if (score >= breaks[["useful"]]) {
    return("Useful")
  }
  
  if (score >= breaks[["limited"]]) {
    return("Limited")
  }
  
  "Minimal"
}


#' Estimate the value of one draft asset
#'
#' @param asset Named list or one-row data frame.
#' @param current_year Valuation year.
#' @param rule_overrides Optional rule overrides.
#' @return Transparent draft-value result.
#' @noRd
evaluate_draft_asset_value <- function(asset,
                                       current_year = NULL,
                                       rule_overrides = NULL) {
  rules <- resolve_draft_value_rules(
    rule_overrides
  )
  
  asset <- validate_draft_value_asset(
    asset
  )
  
  slot <- draft_slot_value(
    round_value = asset$round,
    expected_slot = asset$expected_slot,
    rules = rules
  )
  
  time <- draft_time_discount(
    draft_year = asset$draft_year,
    current_year = current_year %||%
      rules$current_year,
    rules = rules
  )
  
  protection_multiplier <-
    draft_protection_multiplier(
      asset$protection_type,
      rules = rules
    )
  
  control_multiplier <-
    draft_control_type_multiplier(
      asset$control_type,
      rules = rules
    )
  
  verification_multiplier <-
    draft_verification_multiplier(
      asset$verification_status,
      rules = rules
    )
  
  strategic_multiplier <-
    draft_strategic_multiplier(
      asset$strategic_value,
      rules = rules
    )
  
  condition_multiplier <-
    draft_condition_multiplier(
      asset$condition_count,
      rules = rules
    )
  
  model_score <-
    slot$base_slot_value *
    time$time_multiplier *
    protection_multiplier *
    control_multiplier *
    verification_multiplier *
    strategic_multiplier *
    condition_multiplier
  
  blended_score <- if (!is.na(asset$internal_value)) {
    0.70 * model_score +
      0.30 * (
        sign(control_multiplier) *
          abs(asset$internal_value)
      )
  } else {
    model_score
  }
  
  tier <- draft_value_tier(
    blended_score,
    rules = rules
  )
  
  direction <- if (blended_score > 0) {
    "Asset"
  } else if (blended_score < 0) {
    "Obligation"
  } else {
    "Neutral"
  }
  
  review_reasons <- character(0)
  
  if (
    asset$verification_status != "Verified"
  ) {
    review_reasons <- c(
      review_reasons,
      "Asset terms are not marked verified."
    )
  }
  
  if (
    asset$protection_type %in%
    c(
      "Conditional",
      "Best Of",
      "Worst Of",
      "Range Protected",
      "Unspecified"
    )
  ) {
    review_reasons <- c(
      review_reasons,
      "Protection or swap terms require manual interpretation."
    )
  }
  
  if (is.na(asset$expected_slot)) {
    review_reasons <- c(
      review_reasons,
      paste0(
        "Expected slot was not supplied; the model used the default ",
        slot$expected_slot,
        " slot."
      )
    )
  }
  
  explanation <- paste0(
    asset$draft_year,
    " ",
    asset$round,
    "-round ",
    tolower(asset$control_type),
    " evaluated at slot ",
    slot$expected_slot,
    ". Base value ",
    round(slot$base_slot_value, 1),
    " was adjusted for timing, protection, control, verification, strategic rating, and condition complexity."
  )
  
  list(
    draft_asset_id = asset$draft_asset_id,
    draft_year = asset$draft_year,
    round = asset$round,
    control_type = asset$control_type,
    direction = direction,
    expected_slot = slot$expected_slot,
    base_slot_value = slot$base_slot_value,
    years_out = time$years_out,
    time_multiplier = time$time_multiplier,
    protection_multiplier = protection_multiplier,
    control_multiplier = control_multiplier,
    verification_multiplier = verification_multiplier,
    strategic_multiplier = strategic_multiplier,
    condition_multiplier = condition_multiplier,
    model_score = model_score,
    internal_value = asset$internal_value,
    blended_value_score = blended_score,
    value_tier = tier,
    requires_manual_review = length(review_reasons) > 0L,
    review_reasons = unique(review_reasons),
    explanation = explanation,
    model_label = rules$model_label
  )
}


# ------------------------------------------------------------
# Portfolio valuation
# ------------------------------------------------------------

#' Evaluate all assets in a data frame
#' @noRd
evaluate_draft_portfolio_values <- function(assets,
                                            current_year = NULL,
                                            rule_overrides = NULL) {
  if (is.null(assets)) {
    assets <- data.frame()
  }
  
  if (!is.data.frame(assets)) {
    stop(
      "assets must be a data frame.",
      call. = FALSE
    )
  }
  
  if (!nrow(assets)) {
    return(
      data.frame(
        draft_asset_id = integer(),
        draft_year = integer(),
        round = character(),
        control_type = character(),
        direction = character(),
        expected_slot = integer(),
        base_slot_value = numeric(),
        model_score = numeric(),
        blended_value_score = numeric(),
        value_tier = character(),
        requires_manual_review = logical(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  results <- lapply(
    seq_len(nrow(assets)),
    function(i) {
      evaluate_draft_asset_value(
        asset = assets[i, , drop = FALSE],
        current_year = current_year,
        rule_overrides = rule_overrides
      )
    }
  )
  
  data.frame(
    draft_asset_id = vapply(
      results,
      function(x) x$draft_asset_id %||% NA_integer_,
      integer(1)
    ),
    draft_year = vapply(
      results,
      function(x) x$draft_year,
      integer(1)
    ),
    round = vapply(
      results,
      function(x) x$round,
      character(1)
    ),
    control_type = vapply(
      results,
      function(x) x$control_type,
      character(1)
    ),
    direction = vapply(
      results,
      function(x) x$direction,
      character(1)
    ),
    expected_slot = vapply(
      results,
      function(x) x$expected_slot,
      integer(1)
    ),
    base_slot_value = vapply(
      results,
      function(x) x$base_slot_value,
      numeric(1)
    ),
    time_multiplier = vapply(
      results,
      function(x) x$time_multiplier,
      numeric(1)
    ),
    protection_multiplier = vapply(
      results,
      function(x) x$protection_multiplier,
      numeric(1)
    ),
    control_multiplier = vapply(
      results,
      function(x) x$control_multiplier,
      numeric(1)
    ),
    verification_multiplier = vapply(
      results,
      function(x) x$verification_multiplier,
      numeric(1)
    ),
    strategic_multiplier = vapply(
      results,
      function(x) x$strategic_multiplier,
      numeric(1)
    ),
    condition_multiplier = vapply(
      results,
      function(x) x$condition_multiplier,
      numeric(1)
    ),
    model_score = vapply(
      results,
      function(x) x$model_score,
      numeric(1)
    ),
    internal_value = vapply(
      results,
      function(x) {
        if (is.na(x$internal_value)) {
          NA_real_
        } else {
          x$internal_value
        }
      },
      numeric(1)
    ),
    blended_value_score = vapply(
      results,
      function(x) x$blended_value_score,
      numeric(1)
    ),
    value_tier = vapply(
      results,
      function(x) x$value_tier,
      character(1)
    ),
    requires_manual_review = vapply(
      results,
      function(x) x$requires_manual_review,
      logical(1)
    ),
    explanation = vapply(
      results,
      function(x) x$explanation,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}


#' Summarize a valued draft portfolio
#' @noRd
summarize_draft_portfolio_value <- function(valued_assets) {
  if (!is.data.frame(valued_assets)) {
    stop(
      "valued_assets must be a data frame.",
      call. = FALSE
    )
  }
  
  if (!nrow(valued_assets)) {
    return(
      list(
        gross_asset_value = 0,
        gross_obligation_value = 0,
        net_portfolio_value = 0,
        premium_assets = 0L,
        strong_assets = 0L,
        obligations = 0L,
        review_required = 0L,
        portfolio_grade = "Unrated",
        executive_summary =
          "No valued draft assets are available for the selected organization."
      )
    )
  }
  
  values <- suppressWarnings(
    as.numeric(
      valued_assets$blended_value_score
    )
  )
  
  values[is.na(values)] <- 0
  
  gross_asset_value <- sum(
    values[values > 0],
    na.rm = TRUE
  )
  
  gross_obligation_value <- abs(
    sum(
      values[values < 0],
      na.rm = TRUE
    )
  )
  
  net_portfolio_value <- sum(
    values,
    na.rm = TRUE
  )
  
  premium_assets <- sum(
    valued_assets$value_tier == "Premium" &
      values > 0,
    na.rm = TRUE
  )
  
  strong_assets <- sum(
    valued_assets$value_tier == "Strong" &
      values > 0,
    na.rm = TRUE
  )
  
  obligations <- sum(
    values < 0,
    na.rm = TRUE
  )
  
  review_required <- sum(
    valued_assets$requires_manual_review,
    na.rm = TRUE
  )
  
  portfolio_grade <- if (
    net_portfolio_value >= 250
  ) {
    "Elite"
  } else if (
    net_portfolio_value >= 150
  ) {
    "Strong"
  } else if (
    net_portfolio_value >= 75
  ) {
    "Above Average"
  } else if (
    net_portfolio_value >= 20
  ) {
    "Balanced"
  } else if (
    net_portfolio_value >= 0
  ) {
    "Limited"
  } else {
    "Obligation Heavy"
  }
  
  executive_summary <- paste0(
    "Estimated net draft portfolio value is ",
    round(net_portfolio_value, 1),
    " points, including ",
    premium_assets,
    " premium asset",
    if (premium_assets == 1) "" else "s",
    " and ",
    obligations,
    " obligation",
    if (obligations == 1) "" else "s",
    ". ",
    if (review_required > 0) {
      paste0(
        review_required,
        " valued record",
        if (review_required == 1) "" else "s",
        " require manual verification."
      )
    } else {
      "All valued records are marked ready for decision-support use."
    }
  )
  
  list(
    gross_asset_value = gross_asset_value,
    gross_obligation_value = gross_obligation_value,
    net_portfolio_value = net_portfolio_value,
    premium_assets = premium_assets,
    strong_assets = strong_assets,
    obligations = obligations,
    review_required = review_required,
    portfolio_grade = portfolio_grade,
    executive_summary = executive_summary
  )
}


#' Evaluate one team's persistent draft portfolio
#' @noRd
evaluate_team_draft_value <- function(team_value,
                                      year_from = NULL,
                                      year_to = NULL,
                                      current_year = NULL,
                                      db_path = NULL,
                                      rule_overrides = NULL) {
  if (!exists("get_draft_assets", mode = "function")) {
    stop(
      "get_draft_assets() is required from draft_assets_engine.R.",
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
  
  valued_assets <- evaluate_draft_portfolio_values(
    assets = assets,
    current_year = current_year,
    rule_overrides = rule_overrides
  )
  
  summary <- summarize_draft_portfolio_value(
    valued_assets
  )
  
  list(
    team = team_value,
    assets = assets,
    valued_assets = valued_assets,
    summary = summary,
    scope_note = paste(
      "Draft value is an internal decision-support estimate.",
      "It should be reviewed alongside scouting, market conditions,",
      "transaction context, and verified pick language."
    )
  )
}


# ------------------------------------------------------------
# Portfolio comparison
# ------------------------------------------------------------

#' Compare two valued draft portfolios
#' @noRd
compare_draft_portfolios <- function(portfolio_a,
                                     portfolio_b,
                                     label_a = "Team A",
                                     label_b = "Team B") {
  if (
    !is.list(portfolio_a) ||
    is.null(portfolio_a$summary)
  ) {
    stop(
      "portfolio_a must contain a summary.",
      call. = FALSE
    )
  }
  
  if (
    !is.list(portfolio_b) ||
    is.null(portfolio_b$summary)
  ) {
    stop(
      "portfolio_b must contain a summary.",
      call. = FALSE
    )
  }
  
  value_a <- draft_value_number(
    portfolio_a$summary$net_portfolio_value,
    0
  )
  
  value_b <- draft_value_number(
    portfolio_b$summary$net_portfolio_value,
    0
  )
  
  difference <- value_a - value_b
  
  leader <- if (difference > 0) {
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
      " have equal estimated net draft value."
    )
  } else {
    paste0(
      leader,
      " holds an estimated ",
      round(abs(difference), 1),
      "-point advantage in net draft portfolio value."
    )
  }
  
  list(
    label_a = label_a,
    label_b = label_b,
    net_value_a = value_a,
    net_value_b = value_b,
    difference = difference,
    leader = leader,
    executive_summary = executive_summary
  )
}