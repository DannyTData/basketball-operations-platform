testthat::test_that("player manager validates positions", {
  testthat::expect_equal(player_manager_clean_positions(c("pg", " SF ", "bad")), c("PG", "SF"))
  testthat::expect_true(all(player_manager_valid_positions() %in% c("PG", "SG", "SF", "PF", "C")))
})
