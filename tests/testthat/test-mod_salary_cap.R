test_that("salary cap module UI is available", {
  ui <- mod_salary_cap_ui(id = "test")
  golem::expect_shinytag(ui)
  expect_true(all(c("id") %in% names(formals(mod_salary_cap_ui))))
})

test_that("salary cap module server keeps required shared filters", {
  expect_true(all(
    c("id", "selected_team", "selected_season") %in%
      names(formals(mod_salary_cap_server))
  ))
})
