# ============================================================
# Thompson's Basketball Intelligence
# Phase 5B: Draft Value Engine Tests
# ============================================================

testthat::test_that("draft value helpers normalize values safely", {
  testthat::expect_equal(
    draft_value_number("12.5"),
    12.5
  )
  
  testthat::expect_equal(
    draft_value_integer("2028"),
    2028L
  )
  
  testthat::expect_equal(
    draft_value_text("  Premium  "),
    "Premium"
  )
  
  testthat::expect_true(
    draft_value_flag("yes")
  )
  
  testthat::expect_false(
    draft_value_flag("no")
  )
  
  testthat::expect_equal(
    draft_value_clamp(120),
    100
  )
  
  testthat::expect_equal(
    draft_value_clamp(-5),
    0
  )
})


testthat::test_that("draft value rules resolve and reject unknown overrides", {
  rules <- resolve_draft_value_rules(
    list(
      annual_discount_rate = 0.10
    )
  )
  
  testthat::expect_equal(
    rules$annual_discount_rate,
    0.10
  )
  
  testthat::expect_error(
    resolve_draft_value_rules(
      list(
        unknown_rule = 1
      )
    ),
    "Unknown draft-value rule override"
  )
})


testthat::test_that("draft rounds normalize correctly", {
  testthat::expect_equal(
    normalize_draft_value_round("1st"),
    "First"
  )
  
  testthat::expect_equal(
    normalize_draft_value_round("second round"),
    "Second"
  )
  
  testthat::expect_error(
    normalize_draft_value_round("Third"),
    "Unsupported draft round"
  )
})


testthat::test_that("slot values use configured curves", {
  first_pick <- draft_slot_value(
    round_value = "First",
    expected_slot = 1
  )
  
  testthat::expect_equal(
    first_pick$base_slot_value,
    100
  )
  
  mid_first <- draft_slot_value(
    round_value = "First",
    expected_slot = 16
  )
  
  testthat::expect_equal(
    mid_first$base_slot_value,
    42
  )
  
  second_pick <- draft_slot_value(
    round_value = "Second",
    expected_slot = 45
  )
  
  testthat::expect_equal(
    second_pick$base_slot_value,
    5
  )
  
  testthat::expect_error(
    draft_slot_value(
      round_value = "First",
      expected_slot = 31
    ),
    "between 1 and 30"
  )
})


testthat::test_that("future draft assets are discounted over time", {
  result <- draft_time_discount(
    draft_year = 2028,
    current_year = 2026,
    annual_discount_rate = 0.10
  )
  
  testthat::expect_equal(
    result$years_out,
    2L
  )
  
  testthat::expect_equal(
    result$time_multiplier,
    1 / 1.1^2
  )
  
  current <- draft_time_discount(
    draft_year = 2025,
    current_year = 2026,
    annual_discount_rate = 0.10
  )
  
  testthat::expect_equal(
    current$years_out,
    0L
  )
  
  testthat::expect_equal(
    current$time_multiplier,
    1
  )
  
  testthat::expect_error(
    draft_time_discount(
      draft_year = 2028,
      current_year = 2026,
      annual_discount_rate = 1
    ),
    "between 0 and 1"
  )
})


testthat::test_that("valuation multipliers resolve correctly", {
  testthat::expect_equal(
    draft_protection_multiplier("Unprotected"),
    1
  )
  
  testthat::expect_equal(
    draft_protection_multiplier("Top-N Protected"),
    0.78
  )
  
  testthat::expect_equal(
    draft_control_type_multiplier("Outgoing"),
    -1
  )
  
  testthat::expect_equal(
    draft_control_type_multiplier("Swap Right"),
    0.45
  )
  
  testthat::expect_equal(
    draft_verification_multiplier("Needs Review"),
    0.90
  )
  
  testthat::expect_equal(
    draft_strategic_multiplier("High"),
    1.10
  )
  
  testthat::expect_error(
    draft_control_type_multiplier("Unknown"),
    "Unsupported control_type"
  )
})


testthat::test_that("condition multiplier declines but respects floor", {
  testthat::expect_equal(
    draft_condition_multiplier(0),
    1
  )
  
  testthat::expect_equal(
    draft_condition_multiplier(2),
    0.90
  )
  
  testthat::expect_equal(
    draft_condition_multiplier(20),
    0.60
  )
})


testthat::test_that("draft value asset validation accepts list and row", {
  asset_list <- list(
    draft_asset_id = 1,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    protection_type = "Top-N Protected",
    verification_status = "Verified",
    strategic_value = "High",
    condition_count = 1,
    expected_slot = 10,
    internal_value = 70
  )
  
  result_list <- validate_draft_value_asset(
    asset_list
  )
  
  testthat::expect_equal(
    result_list$draft_year,
    2028L
  )
  
  asset_row <- data.frame(
    draft_asset_id = 1,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    stringsAsFactors = FALSE
  )
  
  result_row <- validate_draft_value_asset(
    asset_row
  )
  
  testthat::expect_equal(
    result_row$control_type,
    "Incoming"
  )
  
  testthat::expect_error(
    validate_draft_value_asset(
      data.frame(
        draft_year = c(2028, 2029),
        round = c("First", "First"),
        control_type = c("Own", "Own")
      )
    ),
    "exactly one row"
  )
})


testthat::test_that("value tiers classify positive and negative scores by magnitude", {
  testthat::expect_equal(
    draft_value_tier(75),
    "Premium"
  )
  
  testthat::expect_equal(
    draft_value_tier(50),
    "Strong"
  )
  
  testthat::expect_equal(
    draft_value_tier(25),
    "Useful"
  )
  
  testthat::expect_equal(
    draft_value_tier(10),
    "Limited"
  )
  
  testthat::expect_equal(
    draft_value_tier(2),
    "Minimal"
  )
  
  testthat::expect_equal(
    draft_value_tier(-80),
    "Premium"
  )
})


testthat::test_that("single draft asset valuation is transparent", {
  asset <- list(
    draft_asset_id = 10,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    protection_type = "Top-N Protected",
    verification_status = "Verified",
    strategic_value = "High",
    condition_count = 1,
    expected_slot = 10,
    internal_value = NA_real_
  )
  
  result <- evaluate_draft_asset_value(
    asset = asset,
    current_year = 2026
  )
  
  expected_model <-
    60 *
    (1 / 1.08^2) *
    0.78 *
    1.00 *
    1.00 *
    1.10 *
    0.95
  
  testthat::expect_equal(
    result$model_score,
    expected_model
  )
  
  testthat::expect_equal(
    result$blended_value_score,
    expected_model
  )
  
  testthat::expect_equal(
    result$direction,
    "Asset"
  )
  
  testthat::expect_false(
    result$requires_manual_review
  )
  
  testthat::expect_match(
    result$explanation,
    "Base value"
  )
})


testthat::test_that("internal value blends with model value", {
  asset <- list(
    draft_asset_id = 11,
    draft_year = 2026,
    round = "First",
    control_type = "Own",
    protection_type = "Unprotected",
    verification_status = "Verified",
    strategic_value = "Medium",
    condition_count = 0,
    expected_slot = 16,
    internal_value = 60
  )
  
  result <- evaluate_draft_asset_value(
    asset = asset,
    current_year = 2026
  )
  
  expected <- 0.70 * 42 + 0.30 * 60
  
  testthat::expect_equal(
    result$blended_value_score,
    expected
  )
})


testthat::test_that("outgoing obligations receive negative value", {
  asset <- list(
    draft_asset_id = 12,
    draft_year = 2027,
    round = "First",
    control_type = "Outgoing",
    protection_type = "Unprotected",
    verification_status = "Verified",
    strategic_value = "Medium",
    condition_count = 0,
    expected_slot = 20
  )
  
  result <- evaluate_draft_asset_value(
    asset = asset,
    current_year = 2026
  )
  
  testthat::expect_true(
    result$blended_value_score < 0
  )
  
  testthat::expect_equal(
    result$direction,
    "Obligation"
  )
})


testthat::test_that("ambiguous assets are flagged for review", {
  asset <- list(
    draft_year = 2029,
    round = "First",
    control_type = "Incoming",
    protection_type = "Best Of",
    verification_status = "Needs Review",
    strategic_value = "Unrated",
    condition_count = 2
  )
  
  result <- evaluate_draft_asset_value(
    asset = asset,
    current_year = 2026
  )
  
  testthat::expect_true(
    result$requires_manual_review
  )
  
  testthat::expect_true(
    length(result$review_reasons) >= 2
  )
  
  testthat::expect_equal(
    result$expected_slot,
    16L
  )
})


testthat::test_that("portfolio evaluation returns one row per asset", {
  assets <- data.frame(
    draft_asset_id = c(1, 2, 3),
    draft_year = c(2027, 2028, 2029),
    round = c("First", "First", "Second"),
    control_type = c("Own", "Incoming", "Outgoing"),
    protection_type = c(
      "Unprotected",
      "Top-N Protected",
      "Unprotected"
    ),
    verification_status = c(
      "Verified",
      "Verified",
      "Verified"
    ),
    strategic_value = c(
      "High",
      "Medium",
      "Low"
    ),
    condition_count = c(0, 1, 0),
    expected_slot = c(8, 18, 45),
    internal_value = c(NA, 35, NA),
    stringsAsFactors = FALSE
  )
  
  valued <- evaluate_draft_portfolio_values(
    assets = assets,
    current_year = 2026
  )
  
  testthat::expect_equal(
    nrow(valued),
    3
  )
  
  testthat::expect_true(
    valued$blended_value_score[[1]] > 0
  )
  
  testthat::expect_true(
    valued$blended_value_score[[3]] < 0
  )
})


testthat::test_that("empty portfolio returns empty valuation frame", {
  valued <- evaluate_draft_portfolio_values(
    data.frame()
  )
  
  testthat::expect_equal(
    nrow(valued),
    0
  )
  
  testthat::expect_true(
    "blended_value_score" %in% names(valued)
  )
})


testthat::test_that("portfolio summary calculates assets and obligations", {
  valued <- data.frame(
    blended_value_score = c(
      80,
      50,
      25,
      -20
    ),
    value_tier = c(
      "Premium",
      "Strong",
      "Useful",
      "Useful"
    ),
    requires_manual_review = c(
      FALSE,
      FALSE,
      TRUE,
      FALSE
    ),
    stringsAsFactors = FALSE
  )
  
  summary <- summarize_draft_portfolio_value(
    valued
  )
  
  testthat::expect_equal(
    summary$gross_asset_value,
    155
  )
  
  testthat::expect_equal(
    summary$gross_obligation_value,
    20
  )
  
  testthat::expect_equal(
    summary$net_portfolio_value,
    135
  )
  
  testthat::expect_equal(
    summary$premium_assets,
    1
  )
  
  testthat::expect_equal(
    summary$strong_assets,
    1
  )
  
  testthat::expect_equal(
    summary$obligations,
    1
  )
  
  testthat::expect_equal(
    summary$review_required,
    1
  )
  
  testthat::expect_equal(
    summary$portfolio_grade,
    "Above Average"
  )
})


testthat::test_that("empty portfolio summary is unrated", {
  summary <- summarize_draft_portfolio_value(
    data.frame()
  )
  
  testthat::expect_equal(
    summary$net_portfolio_value,
    0
  )
  
  testthat::expect_equal(
    summary$portfolio_grade,
    "Unrated"
  )
})


testthat::test_that("draft portfolio comparison identifies leader", {
  portfolio_a <- list(
    summary = list(
      net_portfolio_value = 180
    )
  )
  
  portfolio_b <- list(
    summary = list(
      net_portfolio_value = 120
    )
  )
  
  result <- compare_draft_portfolios(
    portfolio_a = portfolio_a,
    portfolio_b = portfolio_b,
    label_a = "Boston",
    label_b = "Brooklyn"
  )
  
  testthat::expect_equal(
    result$difference,
    60
  )
  
  testthat::expect_equal(
    result$leader,
    "Boston"
  )
  
  testthat::expect_match(
    result$executive_summary,
    "60-point advantage"
  )
})


testthat::test_that("draft portfolio comparison handles ties", {
  portfolio_a <- list(
    summary = list(
      net_portfolio_value = 100
    )
  )
  
  portfolio_b <- list(
    summary = list(
      net_portfolio_value = 100
    )
  )
  
  result <- compare_draft_portfolios(
    portfolio_a = portfolio_a,
    portfolio_b = portfolio_b
  )
  
  testthat::expect_equal(
    result$leader,
    "Even"
  )
  
  testthat::expect_match(
    result$executive_summary,
    "equal estimated net draft value"
  )
})