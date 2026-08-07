# ============================================================
# Thompson's Basketball Intelligence
# Phase 6: Basketball Intelligence Engine Tests
# ============================================================

testthat::test_that("basketball intelligence helpers normalize values safely", {
  testthat::expect_equal(
    basketball_intel_number("12.5"),
    12.5
  )
  
  testthat::expect_equal(
    basketball_intel_integer("8"),
    8L
  )
  
  testthat::expect_equal(
    basketball_intel_text("  Contender  "),
    "Contender"
  )
  
  testthat::expect_true(
    basketball_intel_flag("yes")
  )
  
  testthat::expect_false(
    basketball_intel_flag("no")
  )
  
  testthat::expect_equal(
    basketball_intel_clamp(120),
    100
  )
  
  testthat::expect_equal(
    basketball_intel_clamp(-5),
    0
  )
})


testthat::test_that("nested values are read safely", {
  x <- list(
    a = list(
      b = 10
    )
  )
  
  testthat::expect_equal(
    basketball_intel_get(x, c("a", "b"), 0),
    10
  )
  
  testthat::expect_equal(
    basketball_intel_get(x, c("a", "missing"), 7),
    7
  )
})


testthat::test_that("rules resolve and reject unknown overrides", {
  rules <- resolve_basketball_intelligence_rules(
    list(
      review_penalty_per_item = 4
    )
  )
  
  testthat::expect_equal(
    rules$review_penalty_per_item,
    4
  )
  
  testthat::expect_error(
    resolve_basketball_intelligence_rules(
      list(
        unknown_rule = 1
      )
    ),
    "Unknown basketball-intelligence rule override"
  )
})


testthat::test_that("competitive position evaluates supplied inputs", {
  result <- evaluate_competitive_position(
    competitive_tier = "Contender",
    projected_wins = 52,
    playoff_probability = 0.90,
    championship_probability = 0.15
  )
  
  testthat::expect_true(
    result$score > 70
  )
  
  testthat::expect_equal(
    result$competitive_tier,
    "Contender"
  )
  
  testthat::expect_match(
    result$explanation,
    "projected wins"
  )
})


testthat::test_that("competitive probabilities accept percentages", {
  decimal <- evaluate_competitive_position(
    competitive_tier = "Playoff",
    playoff_probability = 0.75
  )
  
  percent <- evaluate_competitive_position(
    competitive_tier = "Playoff",
    playoff_probability = 75
  )
  
  testthat::expect_equal(
    decimal$score,
    percent$score
  )
})


testthat::test_that("financial flexibility reflects cap status", {
  below_cap <- evaluate_financial_flexibility(
    cap_result = list(
      operating_status = "Below Cap"
    )
  )
  
  second_apron <- evaluate_financial_flexibility(
    cap_result = list(
      operating_status = "Above Second Apron"
    )
  )
  
  testthat::expect_true(
    below_cap$score > second_apron$score
  )
  
  testthat::expect_equal(
    below_cap$operating_status,
    "Below Cap"
  )
})


testthat::test_that("financial concentration and commitments create penalties", {
  result <- evaluate_financial_flexibility(
    cap_result = list(
      operating_status = "Over Cap"
    ),
    top_three_concentration = 0.70,
    future_committed_salary_ratio = 1.15
  )
  
  testthat::expect_equal(
    result$concentration_penalty,
    20
  )
  
  testthat::expect_equal(
    result$future_commitment_penalty,
    15
  )
  
  testthat::expect_equal(
    result$score,
    37
  )
})


testthat::test_that("financial flexibility requires a list", {
  testthat::expect_error(
    evaluate_financial_flexibility(
      cap_result = "Over Cap"
    ),
    "must be a cap-engine result list"
  )
})


testthat::test_that("roster control rewards optionality", {
  flexible <- evaluate_roster_control(
    guaranteed_roster_spots = 9,
    expiring_contracts = 4,
    team_options = 2,
    player_options = 0,
    two_way_contracts = 3,
    dead_money_ratio = 0
  )
  
  constrained <- evaluate_roster_control(
    guaranteed_roster_spots = 15,
    expiring_contracts = 0,
    team_options = 0,
    player_options = 3,
    two_way_contracts = 0,
    dead_money_ratio = 0.12
  )
  
  testthat::expect_true(
    flexible$score > constrained$score
  )
  
  testthat::expect_true(
    flexible$score >= 70
  )
})


testthat::test_that("draft capital uses portfolio grade and penalties", {
  draft_value_result <- list(
    summary = list(
      portfolio_grade = "Strong",
      review_required = 2
    )
  )
  
  simulation_result <- list(
    mean_portfolio_value = 100,
    portfolio_value_sd = 20
  )
  
  result <- evaluate_draft_capital(
    draft_value_result = draft_value_result,
    draft_simulation_result = simulation_result
  )
  
  testthat::expect_equal(
    result$portfolio_grade,
    "Strong"
  )
  
  testthat::expect_equal(
    result$review_penalty,
    6
  )
  
  testthat::expect_equal(
    result$uncertainty_penalty,
    4
  )
  
  testthat::expect_equal(
    result$score,
    68
  )
})


testthat::test_that("transaction risk scores trade and extension results", {
  result <- evaluate_transaction_risk(
    trade_result = list(
      status = "PASS",
      crosses_first_apron = FALSE,
      crosses_second_apron = FALSE
    ),
    extension_result = list(
      status = "PASS_WITH_REVIEW"
    ),
    manual_review_items = 1
  )
  
  testthat::expect_equal(
    result$review_penalty,
    3
  )
  
  testthat::expect_true(
    result$score > 70
  )
  
  testthat::expect_match(
    result$explanation,
    "Trade screen"
  )
})


testthat::test_that("apron crossings reduce transaction score", {
  base <- evaluate_transaction_risk(
    trade_result = list(
      status = "PASS"
    )
  )
  
  first_apron <- evaluate_transaction_risk(
    trade_result = list(
      status = "PASS",
      crosses_first_apron = TRUE
    )
  )
  
  second_apron <- evaluate_transaction_risk(
    trade_result = list(
      status = "PASS",
      crosses_second_apron = TRUE
    )
  )
  
  testthat::expect_true(
    base$score > first_apron$score
  )
  
  testthat::expect_true(
    first_apron$score > second_apron$score
  )
})


testthat::test_that("transaction risk handles no supplied transaction", {
  result <- evaluate_transaction_risk()
  
  testthat::expect_equal(
    result$score,
    60
  )
  
  testthat::expect_match(
    result$explanation,
    "No specific transaction"
  )
})


testthat::test_that("score classifications follow configured bands", {
  testthat::expect_equal(
    classify_basketball_intelligence_score(85),
    "Aggressive"
  )
  
  testthat::expect_equal(
    classify_basketball_intelligence_score(70),
    "Positive"
  )
  
  testthat::expect_equal(
    classify_basketball_intelligence_score(55),
    "Neutral"
  )
  
  testthat::expect_equal(
    classify_basketball_intelligence_score(40),
    "Caution"
  )
  
  testthat::expect_equal(
    classify_basketball_intelligence_score(20),
    "Negative"
  )
})


testthat::test_that("recommendation labels map to classifications", {
  testthat::expect_equal(
    basketball_intelligence_recommendation_label(
      "Aggressive"
    ),
    "Advance"
  )
  
  testthat::expect_equal(
    basketball_intelligence_recommendation_label(
      "Negative"
    ),
    "Do Not Advance"
  )
})


testthat::test_that("executive rationale identifies strongest and weakest areas", {
  components <- list(
    competitive_position = list(score = 90),
    financial_flexibility = list(score = 40),
    roster_control = list(score = 60),
    draft_capital = list(score = 70),
    transaction_risk = list(score = 80)
  )
  
  result <- build_basketball_intelligence_rationale(
    components = components,
    classification = "Positive"
  )
  
  testthat::expect_match(
    result,
    "competitive position"
  )
  
  testthat::expect_match(
    result,
    "financial flexibility"
  )
})


testthat::test_that("complete basketball decision returns composite output", {
  inputs <- list(
    competitive = list(
      competitive_tier = "Contender",
      projected_wins = 52,
      playoff_probability = 0.90,
      championship_probability = 0.12
    ),
    
    cap_result = list(
      operating_status = "Tax Team",
      top_three_salary_concentration = 0.55
    ),
    
    financial = list(
      future_committed_salary_ratio = 0.90
    ),
    
    roster = list(
      guaranteed_roster_spots = 11,
      expiring_contracts = 3,
      team_options = 2,
      player_options = 1,
      two_way_contracts = 3,
      dead_money_ratio = 0.01
    ),
    
    draft_value_result = list(
      summary = list(
        portfolio_grade = "Strong",
        review_required = 0
      )
    ),
    
    draft_simulation_result = list(
      mean_portfolio_value = 160,
      portfolio_value_sd = 16
    ),
    
    trade_result = list(
      status = "PASS",
      crosses_first_apron = FALSE,
      crosses_second_apron = FALSE
    ),
    
    extension_result = list(
      status = "PASS_WITH_REVIEW"
    ),
    
    transaction = list(
      manual_review_items = 1
    )
  )
  
  result <- evaluate_basketball_decision(
    inputs = inputs
  )
  
  testthat::expect_true(
    result$score >= 0 &&
      result$score <= 100
  )
  
  testthat::expect_true(
    result$classification %in%
      c(
        "Aggressive",
        "Positive",
        "Neutral",
        "Caution",
        "Negative"
      )
  )
  
  testthat::expect_true(
    nzchar(result$recommendation)
  )
  
  testthat::expect_equal(
    length(result$components),
    5
  )
  
  testthat::expect_match(
    result$executive_summary,
    "Composite Basketball Intelligence score"
  )
})


testthat::test_that("basketball decision generates structural risks", {
  inputs <- list(
    competitive = list(
      competitive_tier = "Rebuilding"
    ),
    
    cap_result = list(
      operating_status = "Above Second Apron"
    ),
    
    roster = list(
      guaranteed_roster_spots = 15,
      player_options = 4,
      dead_money_ratio = 0.15
    ),
    
    draft_value_result = list(
      summary = list(
        portfolio_grade = "Obligation Heavy",
        review_required = 3
      )
    ),
    
    trade_result = list(
      status = "FAIL",
      crosses_second_apron = TRUE
    ),
    
    transaction = list(
      manual_review_items = 3
    )
  )
  
  result <- evaluate_basketball_decision(
    inputs = inputs
  )
  
  testthat::expect_true(
    result$score < 50
  )
  
  testthat::expect_true(
    result$classification %in%
      c("Caution", "Negative")
  )
  
  testthat::expect_true(
    length(result$key_risks) >= 3
  )
})


testthat::test_that("basketball decision requires a list", {
  testthat::expect_error(
    evaluate_basketball_decision(
      inputs = "invalid"
    ),
    "inputs must be a named list"
  )
})


testthat::test_that("decision comparison identifies preferred scenario", {
  decision_a <- list(
    score = 76
  )
  
  decision_b <- list(
    score = 64
  )
  
  result <- compare_basketball_decisions(
    decision_a = decision_a,
    decision_b = decision_b,
    label_a = "Trade Now",
    label_b = "Hold"
  )
  
  testthat::expect_equal(
    result$difference,
    12
  )
  
  testthat::expect_equal(
    result$preferred,
    "Trade Now"
  )
  
  testthat::expect_match(
    result$executive_summary,
    "preferred by 12 points"
  )
})


testthat::test_that("decision comparison handles ties", {
  decision_a <- list(score = 70)
  decision_b <- list(score = 70)
  
  result <- compare_basketball_decisions(
    decision_a = decision_a,
    decision_b = decision_b
  )
  
  testthat::expect_equal(
    result$preferred,
    "Even"
  )
  
  testthat::expect_match(
    result$executive_summary,
    "equal Basketball Intelligence scores"
  )
})