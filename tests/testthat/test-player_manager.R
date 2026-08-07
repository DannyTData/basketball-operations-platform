testthat::test_that("player manager validates positions", {
  testthat::expect_equal(player_manager_clean_positions(c("pg", " SF ", "bad")), c("PG", "SF"))
  testthat::expect_true(all(player_manager_valid_positions() %in% c("PG", "SG", "SF", "PF", "C")))
})

testthat::test_that("position value stays in range", {
  roster <- data.frame(player_id = integer(), age = numeric(), salary = numeric())
  depth <- data.frame(player_id = integer(), position = character())
  result <- position_value_v2("PG", roster, depth)
  testthat::expect_gte(result$score, 0)
  testthat::expect_lte(result$score, 100)
})
