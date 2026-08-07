# ============================================================
# Thompson's Basketball Intelligence
# Phase 3: Trade Engine Tests
# ============================================================

testthat::test_that("trade salary helpers handle numeric input safely", {
  testthat::expect_equal(trade_number("12500000"), 12500000)
  testthat::expect_equal(trade_number(NA, default = 7), 7)
  testthat::expect_equal(trade_salary_sum(c(1000000, NA, 2500000)), 3500000)
  testthat::expect_equal(trade_money(12500000), "$12,500,000")
})


testthat::test_that("trade team status resolves every threshold band", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  testthat::expect_equal(
    resolve_trade_team_status(130000000, thresholds)$status,
    "Below Cap"
  )
  
  testthat::expect_equal(
    resolve_trade_team_status(150000000, thresholds)$status,
    "Over Cap"
  )
  
  testthat::expect_equal(
    resolve_trade_team_status(172000000, thresholds)$status,
    "Tax Team"
  )
  
  testthat::expect_equal(
    resolve_trade_team_status(180000000, thresholds)$status,
    "Above First Apron"
  )
  
  testthat::expect_equal(
    resolve_trade_team_status(191000000, thresholds)$status,
    "Above Second Apron"
  )
})


testthat::test_that("trade thresholds must be complete and ordered", {
  incomplete <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = NA_real_,
    second_apron = 189000000
  )
  
  testthat::expect_error(
    resolve_trade_team_status(150000000, incomplete),
    "thresholds are required"
  )
  
  unordered <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 180000000,
    first_apron = 175000000,
    second_apron = 189000000
  )
  
  testthat::expect_error(
    resolve_trade_team_status(150000000, unordered),
    "ascending order"
  )
})


testthat::test_that("standard non-tax matching bands calculate correctly", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  low <- calculate_maximum_incoming_salary(
    outgoing_salary = 5000000,
    team_salary = 150000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(low$rule_code, "STANDARD_LOW")
  testthat::expect_equal(low$maximum_incoming_salary, 10250000)
  
  middle <- calculate_maximum_incoming_salary(
    outgoing_salary = 20000000,
    team_salary = 150000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(middle$rule_code, "STANDARD_MIDDLE")
  testthat::expect_equal(middle$maximum_incoming_salary, 27500000)
  
  high <- calculate_maximum_incoming_salary(
    outgoing_salary = 32000000,
    team_salary = 150000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(high$rule_code, "STANDARD_HIGH")
  testthat::expect_equal(high$maximum_incoming_salary, 40250000)
})


testthat::test_that("tax and apron matching rules are applied", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  tax_team <- calculate_maximum_incoming_salary(
    outgoing_salary = 20000000,
    team_salary = 172000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(tax_team$rule_code, "TAX_125")
  testthat::expect_equal(tax_team$maximum_incoming_salary, 25250000)
  
  first_apron <- calculate_maximum_incoming_salary(
    outgoing_salary = 20000000,
    team_salary = 180000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(first_apron$rule_code, "FIRST_APRON_110")
  testthat::expect_equal(first_apron$maximum_incoming_salary, 22250000)
  
  second_apron <- calculate_maximum_incoming_salary(
    outgoing_salary = 20000000,
    team_salary = 191000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(second_apron$rule_code, "SECOND_APRON_100")
  testthat::expect_equal(second_apron$maximum_incoming_salary, 20000000)
})


testthat::test_that("below-cap teams may absorb salary into cap room", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  room_team <- calculate_maximum_incoming_salary(
    outgoing_salary = 5000000,
    team_salary = 130000000,
    thresholds = thresholds
  )
  
  testthat::expect_equal(room_team$rule_code, "CAP_ROOM")
  testthat::expect_equal(room_team$cap_room_used, 10000000)
  testthat::expect_equal(room_team$maximum_incoming_salary, 15000000)
})


testthat::test_that("team trade evaluation returns pass and fail results", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  passing <- evaluate_team_trade(
    outgoing_salary = 20000000,
    incoming_salary = 25000000,
    team_salary = 150000000,
    thresholds = thresholds
  )
  
  testthat::expect_true(passing$is_salary_match)
  testthat::expect_equal(passing$status, "PASS")
  testthat::expect_equal(passing$post_trade_salary, 155000000)
  testthat::expect_true(passing$room_to_matching_limit >= 0)
  
  failing <- evaluate_team_trade(
    outgoing_salary = 20000000,
    incoming_salary = 28000000,
    team_salary = 150000000,
    thresholds = thresholds
  )
  
  testthat::expect_false(failing$is_salary_match)
  testthat::expect_equal(failing$status, "FAIL")
  testthat::expect_true(failing$room_to_matching_limit < 0)
})


testthat::test_that("trade evaluation detects apron crossings", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  result <- evaluate_team_trade(
    outgoing_salary = 10000000,
    incoming_salary = 19000000,
    team_salary = 170000000,
    thresholds = thresholds,
    rule_overrides = list(
      tax_team_multiplier = 2,
      tax_team_addition = 0
    )
  )
  
  testthat::expect_true(result$crosses_first_apron)
  testthat::expect_false(result$crosses_second_apron)
  testthat::expect_equal(result$post_trade_status, "Above First Apron")
})


testthat::test_that("trade-side salary accepts supported salary columns", {
  testthat::expect_equal(
    trade_side_salary(data.frame(cap_hit = c(10000000, 5000000))),
    15000000
  )
  
  testthat::expect_equal(
    trade_side_salary(data.frame(trade_salary = c(8000000, 2000000))),
    10000000
  )
  
  testthat::expect_error(
    trade_side_salary(data.frame(player_name = "Player A")),
    "must contain"
  )
})


testthat::test_that("second-apron aggregation is flagged", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  outgoing <- data.frame(
    player_name = c("Player A", "Player B"),
    cap_hit = c(10000000, 8000000)
  )
  
  incoming <- data.frame(
    player_name = "Player C",
    cap_hit = 18000000
  )
  
  result <- evaluate_trade_side(
    outgoing_players = outgoing,
    incoming_players = incoming,
    team_salary = 191000000,
    thresholds = thresholds
  )
  
  testthat::expect_true(result$second_apron_aggregation_violation)
  testthat::expect_false(result$is_screen_pass)
  testthat::expect_equal(result$screen_status, "FAIL")
  testthat::expect_true(result$requires_manual_review)
})


testthat::test_that("two-team trade requires complete team inputs", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  incomplete_team <- list(
    team_name = "Team A",
    team_salary = 150000000
  )
  
  complete_team <- list(
    team_name = "Team B",
    team_salary = 150000000,
    outgoing_players = data.frame(cap_hit = 10000000),
    incoming_players = data.frame(cap_hit = 10000000)
  )
  
  testthat::expect_error(
    evaluate_two_team_trade(
      team_a = incomplete_team,
      team_b = complete_team,
      thresholds = thresholds
    ),
    "team_a is missing"
  )
})


testthat::test_that("two-team trade passes when both sides pass", {
  thresholds <- data.frame(
    salary_cap = 140000000,
    luxury_tax = 170000000,
    first_apron = 178000000,
    second_apron = 189000000
  )
  
  team_a <- list(
    team_name = "Team A",
    team_salary = 150000000,
    available_cap_room = 0,
    outgoing_players = data.frame(
      player_name = "Player A",
      cap_hit = 20000000
    ),
    incoming_players = data.frame(
      player_name = "Player B",
      cap_hit = 18000000
    )
  )
  
  team_b <- list(
    team_name = "Team B",
    team_salary = 150000000,
    available_cap_room = 0,
    outgoing_players = data.frame(
      player_name = "Player B",
      cap_hit = 18000000
    ),
    incoming_players = data.frame(
      player_name = "Player A",
      cap_hit = 20000000
    )
  )
  
  result <- evaluate_two_team_trade(
    team_a = team_a,
    team_b = team_b,
    thresholds = thresholds
  )
  
  testthat::expect_true(result$is_trade_screen_pass)
  testthat::expect_equal(result$status, "PASS")
  testthat::expect_match(
    result$executive_summary,
    "passes the current salary-matching screen"
  )
  testthat::expect_match(
    result$scope_note,
    "not a final league-office transaction determination"
  )
})