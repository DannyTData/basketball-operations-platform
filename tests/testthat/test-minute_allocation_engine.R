# ============================================================
# Thompson's Basketball Intelligence
# Phase 8 Tests: Minute Allocation Engine
# ============================================================

testthat::test_that("minute helpers normalize values", {
  testthat::expect_equal(
    minute_number("12.5"),
    12.5
  )
  
  testthat::expect_equal(
    minute_integer("13"),
    13L
  )
  
  testthat::expect_true(
    minute_flag("yes")
  )
  
  testthat::expect_equal(
    normalize_availability_status("Out"),
    "OUT"
  )
})


testthat::test_that("default scoring weights sum to one", {
  rules <- minute_allocation_rule_defaults()
  
  total <-
    rules$bie_weight +
    rules$projected_bie_weight +
    rules$impact_weight +
    rules$role_weight +
    rules$depth_weight +
    rules$starter_weight +
    rules$availability_weight
  
  testthat::expect_equal(
    total,
    1
  )
})


testthat::test_that("roster preparation supports TBI-style fields", {
  roster <- data.frame(
    player_id = 1:2,
    player_name = c("Alpha", "Beta"),
    position = c("PG", "SG"),
    depth_order = c(1, 2),
    is_starter = c(1, 0),
    latest_bie_rating = c(85, 70),
    projected_bie_rating = c(87, 72),
    stringsAsFactors = FALSE
  )
  
  result <- prepare_minute_allocation_roster(
    roster
  )
  
  testthat::expect_equal(
    nrow(result),
    2
  )
  
  testthat::expect_equal(
    result$bie_rating[[1]],
    85
  )
  
  testthat::expect_true(
    result$is_starter[[1]]
  )
})


testthat::test_that("out players receive zero priority", {
  player <- list(
    bie_rating = 95,
    projected_bie_rating = 96,
    impact_score = 95,
    primary_role = "Creator",
    archetype = "Primary Offensive Engine",
    impact_tier = "Elite",
    depth_order = 1,
    is_starter = TRUE,
    availability_status = "Out"
  )
  
  testthat::expect_equal(
    calculate_player_minute_priority(player),
    0
  )
})


testthat::test_that("minute allocation totals exactly 240", {
  roster <- data.frame(
    player_id = 1:12,
    player_name = paste("Player", 1:12),
    position = rep(
      c("PG", "SG", "SF", "PF", "C", "G"),
      2
    ),
    depth_order = c(
      1, 1, 1, 1, 1,
      2, 2, 2, 2, 2,
      3, 3
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 7)
    ),
    availability_status = rep("Available", 12),
    bie_rating = c(
      88, 85, 82, 80, 78,
      74, 72, 70, 68, 66,
      60, 58
    ),
    projected_bie_rating = c(
      89, 86, 83, 81, 79,
      75, 73, 71, 69, 67,
      61, 59
    ),
    impact_score = c(
      90, 84, 82, 79, 78,
      73, 72, 69, 68, 66,
      59, 57
    ),
    primary_role = c(
      "Creator",
      "Scorer",
      "Two-Way",
      "Connector",
      "Rim",
      "Scorer",
      "Defender",
      "Connector",
      "Spacer",
      "Rebounder",
      "Depth",
      "Depth"
    ),
    archetype = rep("", 12),
    impact_tier = c(
      rep("Starter", 5),
      rep("Rotation", 5),
      rep("Depth", 2)
    ),
    stringsAsFactors = FALSE
  )
  
  result <- build_minute_allocation(
    roster,
    rotation_size = 10
  )
  
  testthat::expect_equal(
    sum(result$allocation$recommended_minutes),
    240
  )
  
  testthat::expect_equal(
    result$summary$total_minutes,
    240
  )
  
  testthat::expect_equal(
    result$summary$rotation_size,
    10
  )
})


testthat::test_that("five starters remain in the recommended rotation", {
  roster <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    position = rep(
      c("PG", "SG", "SF", "PF", "C"),
      2
    ),
    depth_order = c(
      rep(1, 5),
      rep(2, 5)
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 5)
    ),
    availability_status = rep("Available", 10),
    bie_rating = seq(80, 62, length.out = 10),
    projected_bie_rating = seq(81, 63, length.out = 10),
    impact_score = seq(82, 64, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  result <- build_minute_allocation(
    roster,
    rotation_size = 10
  )
  
  starters <- result$allocation[
    result$allocation$is_starter,
    ,
    drop = FALSE
  ]
  
  testthat::expect_true(
    all(starters$recommended_minutes >= 26)
  )
})


testthat::test_that("limited player maximum is respected", {
  roster <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    position = rep(
      c("PG", "SG", "SF", "PF", "C"),
      2
    ),
    depth_order = c(
      rep(1, 5),
      rep(2, 5)
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 5)
    ),
    availability_status = c(
      "Limited",
      rep("Available", 9)
    ),
    bie_rating = seq(95, 60, length.out = 10),
    projected_bie_rating = seq(96, 61, length.out = 10),
    impact_score = seq(95, 60, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  result <- build_minute_allocation(
    roster,
    rotation_size = 10
  )
  
  limited <- result$allocation[
    result$allocation$player_id == 1,
    ,
    drop = FALSE
  ]
  
  testthat::expect_lte(
    limited$recommended_minutes[[1]],
    24
  )
})


testthat::test_that("manual overrides are honored", {
  roster <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    position = rep(
      c("PG", "SG", "SF", "PF", "C"),
      2
    ),
    depth_order = c(
      rep(1, 5),
      rep(2, 5)
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 5)
    ),
    availability_status = rep("Available", 10),
    bie_rating = seq(88, 70, length.out = 10),
    projected_bie_rating = seq(89, 71, length.out = 10),
    impact_score = seq(87, 69, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  override <- data.frame(
    player_id = 1,
    minutes = 36
  )
  
  result <- build_minute_allocation(
    roster,
    rotation_size = 10,
    manual_overrides = override
  )
  
  player <- result$allocation[
    result$allocation$player_id == 1,
    ,
    drop = FALSE
  ]
  
  testthat::expect_equal(
    player$recommended_minutes[[1]],
    36
  )
  
  testthat::expect_equal(
    sum(result$allocation$recommended_minutes),
    240
  )
})


testthat::test_that("allocation comparison calculates minute changes", {
  base <- data.frame(
    player_id = 1:2,
    player_name = c("A", "B"),
    recommended_minutes = c(30, 20)
  )
  
  scenario <- data.frame(
    player_id = 1:2,
    player_name = c("A", "B"),
    recommended_minutes = c(25, 25)
  )
  
  result <- compare_minute_allocations(
    base,
    scenario
  )
  
  a <- result[
    result$player_id == 1,
    ,
    drop = FALSE
  ]
  
  b <- result[
    result$player_id == 2,
    ,
    drop = FALSE
  ]
  
  testthat::expect_equal(
    a$minute_change[[1]],
    -5
  )
  
  testthat::expect_equal(
    b$minute_change[[1]],
    5
  )
})