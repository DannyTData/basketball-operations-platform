# ============================================================
# Thompson's Basketball Intelligence
# Phase 4: Extension Engine Tests
# ============================================================

testthat::test_that("extension helpers normalize values safely", {
  testthat::expect_equal(extension_number("12500000"), 12500000)
  testthat::expect_equal(extension_integer(4.2), 4L)
  testthat::expect_true(extension_flag("yes"))
  testthat::expect_false(extension_flag("no"))
  testthat::expect_equal(extension_money(12500000), "$12.5M")
})


testthat::test_that("extension types normalize correctly", {
  testthat::expect_equal(
    normalize_extension_type("rookie scale"),
    "rookie_scale"
  )
  
  testthat::expect_equal(
    normalize_extension_type("designated veteran"),
    "designated_veteran"
  )
  
  testthat::expect_error(
    normalize_extension_type("two-way"),
    "Unsupported extension_type"
  )
})


testthat::test_that("maximum salary percentage follows service tiers", {
  rules <- extension_rule_defaults()
  
  testthat::expect_equal(
    maximum_salary_percentage(3, rules = rules),
    0.25
  )
  
  testthat::expect_equal(
    maximum_salary_percentage(8, rules = rules),
    0.30
  )
  
  testthat::expect_equal(
    maximum_salary_percentage(10, rules = rules),
    0.35
  )
  
  testthat::expect_equal(
    maximum_salary_percentage(
      4,
      designated_rookie_qualified = TRUE,
      rules = rules
    ),
    0.30
  )
  
  testthat::expect_equal(
    maximum_salary_percentage(
      8,
      designated_veteran_qualified = TRUE,
      rules = rules
    ),
    0.35
  )
})


testthat::test_that("maximum player salary is calculated correctly", {
  result <- calculate_maximum_player_salary(
    salary_cap = 140000000,
    service_years = 5
  )
  
  testthat::expect_equal(result$maximum_percentage, 0.25)
  testthat::expect_equal(result$maximum_salary, 35000000)
  
  testthat::expect_error(
    calculate_maximum_player_salary(
      salary_cap = 0,
      service_years = 5
    ),
    "positive salary_cap"
  )
})


testthat::test_that("rookie-scale eligibility screen passes with complete facts", {
  player <- list(
    service_years = 3,
    contract_type = "Rookie Scale",
    remaining_contract_years = 1,
    is_first_round_pick = TRUE,
    rookie_option_years_exercised = TRUE,
    timing_window_open = TRUE,
    designated_rookie_qualified = FALSE,
    designated_veteran_qualified = FALSE,
    original_team_requirement_met = FALSE,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
  
  result <- screen_extension_eligibility(
    player = player,
    extension_type = "rookie_scale"
  )
  
  testthat::expect_true(result$eligible)
  testthat::expect_equal(result$status, "ELIGIBLE")
  testthat::expect_length(result$failures, 0)
})


testthat::test_that("rookie-scale eligibility fails without required facts", {
  player <- list(
    service_years = 2,
    contract_type = "Standard",
    remaining_contract_years = 1,
    is_first_round_pick = FALSE,
    rookie_option_years_exercised = FALSE,
    timing_window_open = FALSE,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
  
  result <- screen_extension_eligibility(
    player = player,
    extension_type = "rookie_scale"
  )
  
  testthat::expect_false(result$eligible)
  testthat::expect_equal(result$status, "INELIGIBLE")
  testthat::expect_true(length(result$failures) >= 3)
})


testthat::test_that("designated veteran requires award and original-team facts", {
  player <- list(
    service_years = 8,
    remaining_contract_years = 1,
    timing_window_open = TRUE,
    designated_veteran_qualified = FALSE,
    original_team_requirement_met = FALSE,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
  
  result <- screen_extension_eligibility(
    player = player,
    extension_type = "designated_veteran"
  )
  
  testthat::expect_false(result$eligible)
  testthat::expect_true(
    any(grepl("award qualification", result$failures))
  )
  testthat::expect_true(
    any(grepl("original-team requirement", result$failures))
  )
})


testthat::test_that("veteran starting salary is capped by rule limit", {
  result <- calculate_extension_starting_salary_limit(
    extension_type = "veteran",
    salary_cap = 140000000,
    service_years = 6,
    current_salary = 20000000,
    next_season_salary = 21000000,
    requested_starting_salary = 30000000
  )
  
  testthat::expect_equal(
    result$maximum_player_salary,
    35000000
  )
  
  testthat::expect_equal(
    result$maximum_starting_salary,
    29400000
  )
  
  testthat::expect_false(
    result$requested_within_limit
  )
  
  testthat::expect_equal(
    result$excess_over_limit,
    600000
  )
})


testthat::test_that("rookie starting salary uses applicable max tier", {
  result <- calculate_extension_starting_salary_limit(
    extension_type = "rookie_scale",
    salary_cap = 140000000,
    service_years = 3,
    current_salary = 10000000,
    requested_starting_salary = 34000000
  )
  
  testthat::expect_equal(
    result$maximum_starting_salary,
    35000000
  )
  
  testthat::expect_true(
    result$requested_within_limit
  )
})


testthat::test_that("raise limits distinguish Bird and non-Bird structures", {
  bird <- maximum_extension_raise_percent(
    has_bird_rights = TRUE,
    requested_raise_percent = 0.08
  )
  
  non_bird <- maximum_extension_raise_percent(
    has_bird_rights = FALSE,
    requested_raise_percent = 0.06
  )
  
  testthat::expect_equal(
    bird$maximum_raise_percent,
    0.08
  )
  
  testthat::expect_true(
    bird$requested_within_limit
  )
  
  testthat::expect_equal(
    non_bird$maximum_raise_percent,
    0.05
  )
  
  testthat::expect_false(
    non_bird$requested_within_limit
  )
})


testthat::test_that("extension schedule builds correct salary progression", {
  schedule <- build_extension_schedule(
    starting_salary = 30000000,
    years = 4,
    raise_percent = 0.08,
    guarantee_structure = "Final year player option",
    first_season = "2027-28"
  )
  
  testthat::expect_equal(nrow(schedule), 4)
  testthat::expect_equal(schedule$salary[[1]], 30000000)
  testthat::expect_equal(schedule$salary[[2]], 32400000)
  testthat::expect_equal(
    schedule$guarantee[[4]],
    "Player option"
  )
  testthat::expect_equal(
    schedule$season[[1]],
    "2027-28"
  )
  testthat::expect_equal(
    schedule$season[[4]],
    "2030-31"
  )
})


testthat::test_that("extension schedule validates years and raise inputs", {
  testthat::expect_error(
    build_extension_schedule(
      starting_salary = 30000000,
      years = 0,
      raise_percent = 0.08
    ),
    "between 1 and 5"
  )
  
  testthat::expect_error(
    build_extension_schedule(
      starting_salary = 30000000,
      years = 4,
      raise_percent = 1.5
    ),
    "between -1 and 1"
  )
})


testthat::test_that("schedule summary returns total value and AAV", {
  schedule <- build_extension_schedule(
    starting_salary = 30000000,
    years = 2,
    raise_percent = 0.10
  )
  
  summary <- summarize_extension_schedule(schedule)
  
  testthat::expect_equal(summary$years, 2)
  testthat::expect_equal(summary$total_value, 63000000)
  testthat::expect_equal(summary$average_annual_value, 31500000)
  testthat::expect_equal(summary$final_salary, 33000000)
})


testthat::test_that("valid veteran proposal passes with manual review", {
  player <- list(
    player_name = "Veteran Player",
    service_years = 6,
    current_salary = 20000000,
    next_season_salary = 21000000,
    remaining_contract_years = 1,
    contract_type = "Veteran",
    has_bird_rights = TRUE,
    timing_window_open = TRUE,
    designated_rookie_qualified = FALSE,
    designated_veteran_qualified = FALSE,
    original_team_requirement_met = FALSE,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
  
  proposal <- list(
    extension_type = "veteran",
    salary_cap = 140000000,
    starting_salary = 28000000,
    years = 4,
    raise_percent = 0.08,
    guarantee_structure = "Fully guaranteed",
    first_season = "2027-28"
  )
  
  result <- evaluate_extension_proposal(
    player = player,
    proposal = proposal
  )
  
  testthat::expect_true(result$passes_screen)
  testthat::expect_equal(result$status, "PASS_WITH_REVIEW")
  testthat::expect_equal(result$maximum_years, 4)
  testthat::expect_equal(result$schedule_summary$years, 4)
  testthat::expect_match(
    result$executive_summary,
    "passes the current financial and eligibility screen"
  )
})


testthat::test_that("proposal fails when salary raise and years exceed limits", {
  player <- list(
    player_name = "Veteran Player",
    service_years = 6,
    current_salary = 20000000,
    next_season_salary = 21000000,
    remaining_contract_years = 1,
    contract_type = "Veteran",
    has_bird_rights = FALSE,
    timing_window_open = TRUE,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
  
  proposal <- list(
    extension_type = "veteran",
    salary_cap = 140000000,
    starting_salary = 32000000,
    years = 5,
    raise_percent = 0.08,
    guarantee_structure = "Fully guaranteed"
  )
  
  result <- evaluate_extension_proposal(
    player = player,
    proposal = proposal
  )
  
  testthat::expect_false(result$passes_screen)
  testthat::expect_equal(result$status, "FAIL")
  testthat::expect_true(
    any(grepl("starting salary exceeds", result$failures))
  )
  testthat::expect_true(
    any(grepl("annual raise exceeds", result$failures))
  )
  testthat::expect_true(
    any(grepl("term exceeds", result$failures))
  )
})


testthat::test_that("proposal requires all required fields", {
  player <- list(
    service_years = 6,
    current_salary = 20000000
  )
  
  proposal <- list(
    extension_type = "veteran",
    salary_cap = 140000000
  )
  
  testthat::expect_error(
    evaluate_extension_proposal(
      player = player,
      proposal = proposal
    ),
    "missing required field"
  )
})


testthat::test_that("player row converts conservatively", {
  row <- data.frame(
    player_id = 1,
    player_name = "Test Player",
    current_salary = 12000000,
    contract_type = "Rookie Scale",
    bird_rights = "Bird",
    stringsAsFactors = FALSE
  )
  
  result <- extension_player_from_row(
    player_row = row,
    service_years = 3,
    remaining_contract_years = 1,
    is_first_round_pick = TRUE,
    rookie_option_years_exercised = TRUE,
    timing_window_open = TRUE
  )
  
  testthat::expect_equal(result$player_id, 1)
  testthat::expect_equal(result$current_salary, 12000000)
  testthat::expect_true(result$has_bird_rights)
  testthat::expect_true(result$is_first_round_pick)
  testthat::expect_true(result$rookie_option_years_exercised)
})


testthat::test_that("rule overrides reject unknown settings", {
  testthat::expect_error(
    resolve_extension_rules(
      list(unknown_rule = 1)
    ),
    "Unknown extension rule override"
  )
  
  result <- resolve_extension_rules(
    list(veteran_prior_salary_multiplier = 1.50)
  )
  
  testthat::expect_equal(
    result$veteran_prior_salary_multiplier,
    1.50
  )
})