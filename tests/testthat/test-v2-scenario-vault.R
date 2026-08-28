testthat::test_that("feedback Scenario Vault blocks only Trade-local scenarios", {
  message <- "Multi-team scenario save/restore is not supported in V2 feedback."

  baseline <- tbi_feedback_scenario_vault_policy(list(active = FALSE))
  testthat::expect_true(baseline$supported)
  testthat::expect_identical(baseline$scenario_scope, "BASELINE")

  shared <- tbi_feedback_scenario_vault_policy(list(
    active = TRUE,
    scenario_type = "trade",
    scenario_scope = "SHARED_SUPPORTED"
  ))
  testthat::expect_true(shared$supported)
  testthat::expect_identical(shared$code, "SCENARIO_VAULT_SUPPORTED")
  testthat::expect_null(shared$message)

  trade_local <- tbi_feedback_scenario_vault_policy(list(
    active = TRUE,
    scenario_type = "v2_multiteam_trade",
    scenario_scope = "TRADE_LOCAL"
  ))
  testthat::expect_false(trade_local$supported)
  testthat::expect_identical(
    trade_local$code,
    "TRADE_LOCAL_VAULT_UNSUPPORTED"
  )
  testthat::expect_identical(trade_local$scenario_scope, "TRADE_LOCAL")
  testthat::expect_identical(trade_local$message, message)

  inferred_trade_local <- tbi_feedback_scenario_vault_policy(list(
    active = TRUE,
    scenario_type = "v2_multiteam_trade"
  ))
  testthat::expect_false(inferred_trade_local$supported)

  local_development <- tbi_feedback_scenario_vault_policy(
    list(
      active = TRUE,
      scenario_type = "v2_multiteam_trade",
      scenario_scope = "TRADE_LOCAL"
    ),
    feedback_mode = FALSE
  )
  testthat::expect_true(local_development$supported)
})

testthat::test_that("feedback chrome initializes the Scenario Vault fail closed", {
  rendered <- htmltools::renderTags(tbi_demo_chrome(
    expiration_disabled = FALSE,
    status_label = "FEEDBACK / NON-AUTHORITATIVE",
    feedback_mode = TRUE
  ))
  html <- paste(rendered$head, rendered$html, collapse = "\n")

  testthat::expect_match(
    html,
    "window.TBI_FEEDBACK_MODE = true",
    fixed = TRUE
  )
  testthat::expect_match(html, "tbi-demo-vault-notice", fixed = TRUE)
  testthat::expect_match(html, 'aria-live="polite"', fixed = TRUE)
})

testthat::test_that("feedback Scenario Vault uses canonical server policy", {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  server <- paste(
    readLines(file.path(root, "R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_demo.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  testthat::expect_match(
    server,
    '"tbi-demo-scenario-vault-policy"',
    fixed = TRUE
  )
  testthat::expect_match(
    server,
    "tbi_feedback_scenario_vault_policy(",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "if (vaultActionBlocked())",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "if (vaultActionBlocked(item))",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "scenarioScope: state.scenarioScope",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "Multi-team scenario save/restore is not supported in V2 feedback.",
    fixed = TRUE
  )
})
