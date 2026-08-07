test_that("cap engine classifies each major operating band", {
  thresholds <- data.frame(
    salary_cap = 100,
    luxury_tax = 120,
    first_apron = 130,
    second_apron = 140,
    minimum_team_salary = 90
  )

  result_for <- function(value) {
    calculate_team_cap_position(data.frame(cap_hit = value), thresholds)
  }

  expect_equal(result_for(90)$status, "Below Cap")
  expect_equal(result_for(110)$status, "Over Cap")
  expect_equal(result_for(125)$status, "Tax Team")
  expect_equal(result_for(135)$status, "Above First Apron")
  expect_equal(result_for(145)$status, "Above Second Apron")
})

test_that("cap engine calculates room and overage correctly", {
  thresholds <- data.frame(
    salary_cap = 100,
    luxury_tax = 120,
    first_apron = 130,
    second_apron = 140,
    minimum_team_salary = 90
  )
  result <- calculate_team_cap_position(
    data.frame(cap_hit = c(50, 35, 20), guaranteed_amount = c(50, 25, 10)),
    thresholds
  )

  expect_equal(result$team_salary, 105)
  expect_equal(result$cap_room, 0)
  expect_equal(result$over_cap_by, 5)
  expect_equal(result$tax_room, 15)
  expect_equal(result$guaranteed_salary, 85)
  expect_equal(result$non_guaranteed_exposure, 20)
})

test_that("two-way rows are excluded by default", {
  thresholds <- data.frame(
    salary_cap = 100,
    luxury_tax = 120,
    first_apron = 130,
    second_apron = 140,
    minimum_team_salary = 90
  )
  contracts <- data.frame(cap_hit = c(95, 10), two_way_flag = c(0, 1))

  expect_equal(calculate_team_cap_position(contracts, thresholds)$team_salary, 95)
  expect_equal(calculate_team_cap_position(contracts, thresholds, include_two_way = TRUE)$team_salary, 105)
})

test_that("additional team salary is explicit and affects classification", {
  thresholds <- data.frame(
    salary_cap = 100,
    luxury_tax = 120,
    first_apron = 130,
    second_apron = 140,
    minimum_team_salary = 90
  )
  result <- calculate_team_cap_position(
    data.frame(cap_hit = 95),
    thresholds,
    additional_team_salary = 30
  )

  expect_equal(result$active_salary, 95)
  expect_equal(result$additional_team_salary, 30)
  expect_equal(result$team_salary, 125)
  expect_equal(result$status, "Tax Team")
})

test_that("invalid thresholds fail loudly", {
  expect_error(
    calculate_team_cap_position(
      data.frame(cap_hit = 100),
      data.frame(salary_cap = 100, luxury_tax = 90, first_apron = 130, second_apron = 140)
    ),
    "ascending order"
  )
})

test_that("database-backed cap summary uses loaded team data", {
  result <- get_team_cap_summary("Boston Celtics", "2026-27")

  expect_equal(result$team, "Boston Celtics")
  expect_equal(result$season, "2026-27")
  expect_true(result$contract_count > 0)
  expect_true(result$team_salary > 0)
  expect_true(result$status %in% c(
    "Below Cap", "Over Cap", "Tax Team", "Above First Apron", "Above Second Apron"
  ))
})
