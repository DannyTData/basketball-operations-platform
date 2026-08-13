# ============================================================
# Thompson's Basketball Intelligence
# Phase 9 Tests: Lineup Optimization Engine
# ============================================================

create_lineup_test_pool <- function() {
  data.frame(
    player_id = 1:10,
    player_name = c(
      "Lead Guard",
      "Scoring Guard",
      "Two Way Wing",
      "Stretch Forward",
      "Rim Center",
      "Bench Creator",
      "Bench Shooter",
      "Bench Defender",
      "Bench Big",
      "Utility Wing"
    ),
    position = c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C",
      "PG",
      "SG",
      "SF",
      "C",
      "PF"
    ),
    availability_status = rep(
      "AVAILABLE",
      10
    ),
    recommended_minutes = c(
      35, 34, 33, 31, 30,
      25, 20, 15, 10, 7
    ),
    bie_rating = c(
      90, 84, 86, 80, 82,
      78, 72, 76, 70, 68
    ),
    offensive_impact = c(
      92, 90, 82, 86, 72,
      84, 86, 68, 64, 72
    ),
    defensive_impact = c(
      78, 70, 90, 82, 92,
      68, 60, 91, 86, 80
    ),
    creation_score = c(
      96, 84, 76, 64, 42,
      90, 66, 50, 35, 58
    ),
    spacing_score = c(
      88, 94, 82, 92, 45,
      78, 96, 70, 38, 80
    ),
    rebounding_score = c(
      45, 42, 70, 78, 96,
      40, 35, 66, 92, 74
    ),
    stringsAsFactors = FALSE
  )
}


testthat::test_that("lineup rules resolve correctly", {
  rules <- lineup_optimization_rule_defaults()
  
  testthat::expect_equal(
    rules$model_label,
    "TBI_LINEUP_v1_OPTIMIZATION"
  )
  
  testthat::expect_error(
    resolve_lineup_optimization_rules(
      list(
        unknown_rule = 1
      )
    ),
    "Unknown lineup-optimization rule"
  )
})


testthat::test_that("positions normalize to expected groups", {
  testthat::expect_equal(
    normalize_lineup_position("Point Guard"),
    "PG"
  )
  
  testthat::expect_equal(
    normalize_lineup_position("PF"),
    "PF"
  )
  
  testthat::expect_equal(
    position_group("C"),
    "BIG"
  )
})


testthat::test_that("balanced position evaluation works", {
  result <- position_balance_evaluation(
    c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C"
    )
  )
  
  testthat::expect_true(
    result$balanced
  )
  
  testthat::expect_false(
    result$review_required
  )
})


testthat::test_that("candidate pool keeps available players", {
  pool <- create_lineup_test_pool()
  
  pool$availability_status[[10]] <- "OUT"
  
  result <- get_lineup_candidate_pool(
    pool,
    pool_size = 10
  )
  
  testthat::expect_equal(
    nrow(result),
    9
  )
  
  testthat::expect_false(
    any(
      result$availability_status == "OUT"
    )
  )
})


testthat::test_that("five-player lineup produces a score", {
  pool <- create_lineup_test_pool()
  
  result <- score_lineup(
    pool[1:5, ],
    lineup_type = "balanced"
  )
  
  testthat::expect_true(
    result$score > 0
  )
  
  testthat::expect_true(
    result$position_balance$balanced
  )
})


testthat::test_that("offense optimizer favors offensive talent", {
  pool <- create_lineup_test_pool()
  
  result <- optimize_lineup_type(
    pool,
    lineup_type = "offense",
    pool_size = 10
  )
  
  testthat::expect_equal(
    length(result$players),
    5
  )
  
  testthat::expect_true(
    "Lead Guard" %in%
      result$players
  )
  
  testthat::expect_true(
    "Scoring Guard" %in%
      result$players
  )
})


testthat::test_that("defense optimizer includes elite defenders", {
  pool <- create_lineup_test_pool()
  
  result <- optimize_lineup_type(
    pool,
    lineup_type = "defense",
    pool_size = 10
  )
  
  testthat::expect_true(
    "Two Way Wing" %in%
      result$players
  )
  
  testthat::expect_true(
    "Rim Center" %in%
      result$players
  )
})


testthat::test_that("full optimization returns four lineup types", {
  pool <- create_lineup_test_pool()
  
  result <- build_lineup_optimization(
    pool,
    pool_size = 10
  )
  
  testthat::expect_true(
    all(
      c(
        "balanced",
        "offense",
        "defense",
        "closing"
      ) %in%
        names(result)
    )
  )
  
  testthat::expect_equal(
    result$model_label,
    "TBI_LINEUP_v1_OPTIMIZATION"
  )
})


testthat::test_that("optimization table contains four rows", {
  pool <- create_lineup_test_pool()
  
  result <- build_lineup_optimization(
    pool,
    pool_size = 10
  )
  
  table <- lineup_optimization_table(
    result
  )
  
  testthat::expect_equal(
    nrow(table),
    4
  )
  
  testthat::expect_true(
    all(
      c(
        "score",
        "players",
        "offense",
        "defense"
      ) %in%
        names(table)
    )
  )
})


testthat::test_that("scenario comparison detects lineup changes", {
  base_pool <- create_lineup_test_pool()
  
  scenario_pool <- base_pool
  
  scenario_pool$offensive_impact[
    scenario_pool$player_name ==
      "Bench Shooter"
  ] <- 100
  
  scenario_pool$bie_rating[
    scenario_pool$player_name ==
      "Bench Shooter"
  ] <- 96
  
  scenario_pool$recommended_minutes[
    scenario_pool$player_name ==
      "Bench Shooter"
  ] <- 32
  
  base <- build_lineup_optimization(
    base_pool,
    pool_size = 10
  )
  
  scenario <- build_lineup_optimization(
    scenario_pool,
    pool_size = 10
  )
  
  comparison <- compare_lineup_optimization(
    base,
    scenario
  )
  
  testthat::expect_equal(
    nrow(comparison),
    4
  )
  
  testthat::expect_true(
    any(
      comparison$lineup_changed |
        abs(comparison$score_change) > 0
    )
  )
})