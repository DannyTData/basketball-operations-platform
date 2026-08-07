# ============================================================
# Thompson's Basketball Intelligence
# Phase 5C: Draft Simulation Engine Tests
# ============================================================

testthat::test_that("simulation helpers normalize values safely", {
  testthat::expect_equal(
    draft_sim_number("12.5"),
    12.5
  )
  
  testthat::expect_equal(
    draft_sim_integer("2028"),
    2028L
  )
  
  testthat::expect_equal(
    draft_sim_text("  Test  "),
    "Test"
  )
  
  testthat::expect_true(
    draft_sim_flag("yes")
  )
  
  testthat::expect_false(
    draft_sim_flag("no")
  )
  
  testthat::expect_equal(
    draft_sim_probability(0.75),
    0.75
  )
  
  testthat::expect_error(
    draft_sim_probability(1.2),
    "between 0 and 1"
  )
})


testthat::test_that("simulation rules resolve and reject unknown overrides", {
  rules <- resolve_draft_simulation_rules(
    list(
      simulation_iterations = 1000L
    )
  )
  
  testthat::expect_equal(
    rules$simulation_iterations,
    1000L
  )
  
  testthat::expect_error(
    resolve_draft_simulation_rules(
      list(
        unknown_rule = 1
      )
    ),
    "Unknown draft-simulation rule override"
  )
})


testthat::test_that("simulation asset validation accepts list and row", {
  asset <- list(
    draft_asset_id = 1,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming"
  )
  
  result <- validate_draft_simulation_asset(
    asset
  )
  
  testthat::expect_equal(
    result$draft_year,
    2028L
  )
  
  row <- data.frame(
    draft_asset_id = 1,
    draft_year = 2028,
    round = "First",
    control_type = "Own",
    stringsAsFactors = FALSE
  )
  
  result_row <- validate_draft_simulation_asset(
    row
  )
  
  testthat::expect_equal(
    result_row$control_type,
    "Own"
  )
  
  testthat::expect_error(
    validate_draft_simulation_asset(
      data.frame(
        draft_year = c(2028, 2029),
        round = c("First", "First"),
        control_type = c("Own", "Own")
      )
    ),
    "exactly one row"
  )
})


testthat::test_that("condition normalization adds optional fields", {
  conditions <- data.frame(
    condition_order = c(2, 1),
    condition_text = c(
      "Second condition",
      "First condition"
    ),
    stringsAsFactors = FALSE
  )
  
  result <- normalize_draft_conditions(
    conditions
  )
  
  testthat::expect_equal(
    result$condition_order,
    c(1L, 2L)
  )
  
  testthat::expect_true(
    all(
      c(
        "condition_year",
        "outcome_if_conveys",
        "outcome_if_not_conveyed",
        "converts_to_year",
        "converts_to_round",
        "is_final_condition"
      ) %in% names(result)
    )
  )
})


testthat::test_that("draft slot simulation respects round bounds", {
  set.seed(1)
  
  first <- replicate(
    100,
    simulate_draft_slot(
      round_value = "First",
      expected_slot = 16,
      slot_sd = 10
    )
  )
  
  testthat::expect_true(
    all(first >= 1 & first <= 30)
  )
  
  second <- replicate(
    100,
    simulate_draft_slot(
      round_value = "Second",
      expected_slot = 45,
      slot_sd = 10
    )
  )
  
  testthat::expect_true(
    all(second >= 31 & second <= 60)
  )
  
  testthat::expect_error(
    simulate_draft_slot(
      round_value = "First",
      expected_slot = 16,
      slot_sd = -1
    ),
    "cannot be negative"
  )
})


testthat::test_that("conveyance simulation honors deterministic probabilities", {
  always <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    conveyance_probability = 1
  )
  
  never <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    conveyance_probability = 0
  )
  
  testthat::expect_true(
    simulate_conveyance(always)$conveyed
  )
  
  testthat::expect_false(
    simulate_conveyance(never)$conveyed
  )
})


testthat::test_that("swap simulation honors deterministic probabilities", {
  always <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Swap Right",
    swap_exercise_probability = 1
  )
  
  never <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Swap Right",
    swap_exercise_probability = 0
  )
  
  testthat::expect_true(
    simulate_swap_exercise(always)$exercised
  )
  
  testthat::expect_false(
    simulate_swap_exercise(never)$exercised
  )
})


testthat::test_that("condition path resolves conveyance and rollover", {
  asset <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Incoming"
  )
  
  conditions <- data.frame(
    condition_order = 1L,
    condition_year = 2028L,
    condition_text = "Top-10 protected",
    outcome_if_conveys = "Conveys in 2028",
    outcome_if_not_conveyed = "Rolls to 2029",
    converts_to_year = 2029L,
    converts_to_round = "First",
    is_final_condition = 0L,
    stringsAsFactors = FALSE
  )
  
  conveyed <- resolve_condition_path(
    asset = asset,
    conditions = conditions,
    conveyed = TRUE
  )
  
  testthat::expect_true(
    conveyed$terminal
  )
  
  testthat::expect_equal(
    conveyed$resolved_year,
    2028L
  )
  
  rolled <- resolve_condition_path(
    asset = asset,
    conditions = conditions,
    conveyed = FALSE
  )
  
  testthat::expect_false(
    rolled$terminal
  )
  
  testthat::expect_equal(
    rolled$resolved_year,
    2029L
  )
})


testthat::test_that("single asset simulation returns a valued result", {
  asset <- list(
    draft_asset_id = 1,
    draft_year = 2028,
    round = "First",
    control_type = "Own",
    protection_type = "Unprotected",
    verification_status = "Verified",
    strategic_value = "High",
    condition_count = 0,
    expected_slot = 10
  )
  
  set.seed(42)
  
  result <- simulate_draft_asset_once(
    asset = asset,
    current_year = 2026
  )
  
  testthat::expect_equal(
    result$draft_asset_id,
    1L
  )
  
  testthat::expect_true(
    result$simulated_slot >= 1 &&
      result$simulated_slot <= 30
  )
  
  testthat::expect_true(
    is.numeric(result$simulated_value)
  )
})


testthat::test_that("non-conveyed terminal asset may resolve to zero", {
  asset <- list(
    draft_asset_id = 2,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    protection_type = "Top-N Protected",
    verification_status = "Verified",
    strategic_value = "Medium",
    condition_count = 1,
    expected_slot = 10,
    conveyance_probability = 0
  )
  
  conditions <- data.frame(
    condition_order = 1L,
    condition_year = 2028L,
    condition_text = "Final condition",
    outcome_if_not_conveyed = "Asset extinguishes",
    converts_to_year = NA_integer_,
    converts_to_round = "",
    is_final_condition = 1L,
    stringsAsFactors = FALSE
  )
  
  result <- simulate_draft_asset_once(
    asset = asset,
    conditions = conditions,
    current_year = 2026
  )
  
  testthat::expect_false(
    result$conveyed
  )
  
  testthat::expect_equal(
    result$simulated_value,
    0
  )
})


testthat::test_that("asset Monte Carlo simulation is reproducible", {
  asset <- list(
    draft_asset_id = 3,
    draft_year = 2028,
    round = "First",
    control_type = "Incoming",
    protection_type = "Top-N Protected",
    verification_status = "Verified",
    strategic_value = "Medium",
    condition_count = 0,
    expected_slot = 16,
    conveyance_probability = 0.70
  )
  
  result_a <- simulate_draft_asset(
    asset = asset,
    iterations = 250,
    current_year = 2026,
    random_seed = 123
  )
  
  result_b <- simulate_draft_asset(
    asset = asset,
    iterations = 250,
    current_year = 2026,
    random_seed = 123
  )
  
  testthat::expect_equal(
    result_a$mean_value,
    result_b$mean_value
  )
  
  testthat::expect_equal(
    result_a$conveyance_rate,
    result_b$conveyance_rate
  )
  
  testthat::expect_equal(
    nrow(result_a$simulation_results),
    250
  )
})


testthat::test_that("asset simulation validates iteration minimum", {
  asset <- list(
    draft_year = 2028,
    round = "First",
    control_type = "Own"
  )
  
  testthat::expect_error(
    simulate_draft_asset(
      asset = asset,
      iterations = 50
    ),
    "at least 100"
  )
})


testthat::test_that("portfolio simulation aggregates asset values", {
  assets <- data.frame(
    draft_asset_id = c(1, 2),
    draft_year = c(2028, 2029),
    round = c("First", "Second"),
    control_type = c("Own", "Outgoing"),
    protection_type = c(
      "Unprotected",
      "Unprotected"
    ),
    verification_status = c(
      "Verified",
      "Verified"
    ),
    strategic_value = c(
      "High",
      "Low"
    ),
    condition_count = c(0, 0),
    expected_slot = c(10, 45),
    stringsAsFactors = FALSE
  )
  
  result <- simulate_draft_portfolio(
    assets = assets,
    iterations = 200,
    current_year = 2026,
    random_seed = 123
  )
  
  testthat::expect_equal(
    result$iterations,
    200
  )
  
  testthat::expect_equal(
    nrow(result$simulation_results),
    200
  )
  
  testthat::expect_true(
    is.numeric(result$mean_portfolio_value)
  )
  
  testthat::expect_match(
    result$executive_summary,
    "Across 200 simulations"
  )
})


testthat::test_that("empty portfolio simulation returns zero summary", {
  result <- simulate_draft_portfolio(
    assets = data.frame()
  )
  
  testthat::expect_equal(
    result$iterations,
    0L
  )
  
  testthat::expect_equal(
    result$mean_portfolio_value,
    0
  )
  
  testthat::expect_match(
    result$executive_summary,
    "No draft assets"
  )
})


testthat::test_that("portfolio simulation validates iterations", {
  assets <- data.frame(
    draft_year = 2028,
    round = "First",
    control_type = "Own",
    stringsAsFactors = FALSE
  )
  
  testthat::expect_error(
    simulate_draft_portfolio(
      assets = assets,
      iterations = 25
    ),
    "at least 100"
  )
})