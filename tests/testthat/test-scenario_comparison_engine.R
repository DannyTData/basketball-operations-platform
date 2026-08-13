# ============================================================
# Thompson's Basketball Intelligence
# Phase 10 Tests: Scenario Comparison Engine
# ============================================================

create_scenario_test_roster <- function() {
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
      "PG", "SG", "SF", "PF", "C",
      "PG", "SG", "SF", "C", "PF"
    ),
    depth_order = c(
      1, 1, 1, 1, 1,
      2, 2, 2, 2, 2
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 5)
    ),
    availability_status =
      rep("AVAILABLE", 10),
    bie_rating = c(
      90, 84, 86, 80, 82,
      78, 72, 76, 70, 68
    ),
    projected_bie_rating = c(
      91, 85, 87, 81, 83,
      79, 73, 77, 71, 69
    ),
    impact_score = c(
      90, 83, 88, 80, 84,
      78, 72, 77, 71, 69
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
    primary_role = c(
      "Creator",
      "Scorer",
      "Two-Way",
      "Connector",
      "Rim",
      "Creator",
      "Spacer",
      "Defender",
      "Rebounder",
      "Connector"
    ),
    archetype = "",
    impact_tier = c(
      rep("Starter", 5),
      rep("Rotation", 5)
    ),
    stringsAsFactors = FALSE
  )
}


testthat::test_that("scenario rules sum to one", {
  rules <- scenario_comparison_rule_defaults()
  
  total <-
    rules$minutes_weight +
    rules$balanced_lineup_weight +
    rules$offense_lineup_weight +
    rules$defense_lineup_weight +
    rules$closing_lineup_weight +
    rules$rotation_quality_weight
  
  testthat::expect_equal(
    total,
    1
  )
})


testthat::test_that("invalid scenario roster is rejected", {
  testthat::expect_error(
    validate_scenario_roster(
      data.frame(
        x = 1:4
      )
    ),
    "at least five"
  )
})


testthat::test_that("scenario state builds Phase 8 and Phase 9 outputs", {
  roster <- create_scenario_test_roster()
  
  state <- build_scenario_state(
    roster,
    rotation_size = 10
  )
  
  testthat::expect_true(
    is.list(state$minutes)
  )
  
  testthat::expect_true(
    is.list(state$lineups)
  )
  
  testthat::expect_equal(
    sum(
      state$minutes$allocation$
        recommended_minutes
    ),
    240
  )
})


testthat::test_that("identical rosters produce neutral score", {
  roster <- create_scenario_test_roster()
  
  result <- build_scenario_comparison(
    base_roster = roster,
    scenario_roster = roster,
    scenario_name = "No Change",
    rotation_size = 10
  )
  
  testthat::expect_equal(
    result$composite_score,
    0,
    tolerance = 1e-8
  )
  
  testthat::expect_equal(
    result$recommendation,
    "NEUTRAL / CLOSE CALL"
  )
})


testthat::test_that("upgraded player improves scenario score", {
  base <- create_scenario_test_roster()
  scenario <- base
  
  hit <- which(
    scenario$player_name ==
      "Bench Shooter"
  )
  
  scenario$bie_rating[hit] <- 96
  scenario$projected_bie_rating[hit] <- 97
  scenario$impact_score[hit] <- 96
  scenario$offensive_impact[hit] <- 100
  scenario$defensive_impact[hit] <- 82
  scenario$creation_score[hit] <- 88
  scenario$spacing_score[hit] <- 100
  scenario$rebounding_score[hit] <- 55
  
  result <- build_scenario_comparison(
    base_roster = base,
    scenario_roster = scenario,
    scenario_name = "Add Elite Shooter",
    rotation_size = 10
  )
  
  testthat::expect_gt(
    result$composite_score,
    0
  )
  
  testthat::expect_true(
    result$recommendation %in%
      c(
        "FAVOR SCENARIO",
        "STRONGLY FAVOR SCENARIO",
        "NEUTRAL / CLOSE CALL"
      )
  )
})


testthat::test_that("minute comparison returns player changes", {
  base <- create_scenario_test_roster()
  scenario <- base
  
  scenario$availability_status[
    scenario$player_name ==
      "Scoring Guard"
  ] <- "OUT"
  
  result <- build_scenario_comparison(
    base_roster = base,
    scenario_roster = scenario,
    scenario_name = "Guard Unavailable",
    rotation_size = 9
  )
  
  testthat::expect_true(
    nrow(
      result$minute_comparison
    ) > 0
  )
  
  testthat::expect_true(
    any(
      abs(
        result$minute_comparison$
          minute_change
      ) > 0
    )
  )
})


testthat::test_that("lineup comparison returns four lineup types", {
  base <- create_scenario_test_roster()
  scenario <- base
  
  scenario$defensive_impact[
    scenario$player_name ==
      "Bench Defender"
  ] <- 100
  
  scenario$bie_rating[
    scenario$player_name ==
      "Bench Defender"
  ] <- 95
  
  result <- build_scenario_comparison(
    base_roster = base,
    scenario_roster = scenario,
    scenario_name = "Defense Upgrade",
    rotation_size = 10
  )
  
  testthat::expect_equal(
    nrow(
      result$lineup_comparison
    ),
    4
  )
})


testthat::test_that("scorecard table is generated", {
  roster <- create_scenario_test_roster()
  
  result <- build_scenario_comparison(
    base_roster = roster,
    scenario_roster = roster,
    scenario_name = "No Change",
    rotation_size = 10
  )
  
  table <- scenario_scorecard_table(
    result
  )
  
  testthat::expect_equal(
    nrow(table),
    7
  )
  
  testthat::expect_true(
    all(
      c(
        "metric",
        "change"
      ) %in%
        names(table)
    )
  )
})


testthat::test_that("executive summary contains recommendation", {
  roster <- create_scenario_test_roster()
  
  result <- build_scenario_comparison(
    base_roster = roster,
    scenario_roster = roster,
    scenario_name = "No Change",
    rotation_size = 10
  )
  
  summary <- scenario_executive_summary(
    result
  )
  
  testthat::expect_match(
    summary,
    "NEUTRAL / CLOSE CALL"
  )
  
  testthat::expect_match(
    summary,
    "No Change"
  )
})