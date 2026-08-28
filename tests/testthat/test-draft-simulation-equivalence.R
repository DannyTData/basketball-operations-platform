# ============================================================
# Command Center draft simulation characterization/equivalence
# ============================================================

tbi_draft_equivalence_teams <- c(
  "New York Knicks",
  "Brooklyn Nets",
  "Memphis Grizzlies",
  "Phoenix Suns"
)

testthat::test_that("current draft simulation inputs match the frozen reference", {
  tbi_test_draft_with_read_only_db(function() {
    for (team in tbi_draft_equivalence_teams) {
      expected <- tbi_test_get_draft_simulation_inputs_reference(team)
      actual <- get_draft_simulation_inputs(team)

      testthat::expect_identical(
        actual,
        expected,
        info = team
      )
    }
  })
})

testthat::test_that("year-filtered draft simulation inputs retain exact scope and order", {
  tbi_test_draft_with_read_only_db(function() {
    expected <- tbi_test_get_draft_simulation_inputs_reference(
      team_value = "Memphis Grizzlies",
      year_from = 2028L,
      year_to = 2031L
    )
    actual <- get_draft_simulation_inputs(
      team_value = "Memphis Grizzlies",
      year_from = 2028L,
      year_to = 2031L
    )

    testthat::expect_identical(actual, expected)
  })
})

testthat::test_that("250-iteration portfolio results and RNG state match the frozen reference", {
  tbi_test_draft_with_read_only_db(function() {
    for (team in tbi_draft_equivalence_teams) {
      inputs <- tbi_test_get_draft_simulation_inputs_reference(team)

      set.seed(9182)
      expected <- tbi_test_simulate_draft_portfolio_reference(
        assets = inputs$assets,
        conditions_lookup = inputs$conditions_lookup,
        iterations = 250L,
        current_year = 2026L,
        random_seed = 42L
      )
      expected_rng <- .Random.seed

      set.seed(9182)
      actual <- simulate_draft_portfolio(
        assets = inputs$assets,
        conditions_lookup = inputs$conditions_lookup,
        iterations = 250L,
        current_year = 2026L,
        random_seed = 42L
      )
      actual_rng <- .Random.seed

      tbi_test_expect_draft_simulation_identical(
        actual,
        expected
      )
      testthat::expect_identical(
        actual_rng,
        expected_rng,
        info = team
      )
      testthat::expect_identical(actual$iterations, 250L)
    }
  })
})

testthat::test_that("team simulation wrapper preserves its complete output contract", {
  tbi_test_draft_with_read_only_db(function() {
    for (team in tbi_draft_equivalence_teams) {
      inputs <- tbi_test_get_draft_simulation_inputs_reference(team)
      expected <- tbi_test_simulate_draft_portfolio_reference(
        assets = inputs$assets,
        conditions_lookup = inputs$conditions_lookup,
        iterations = 250L,
        current_year = 2026L,
        random_seed = 31415L
      )
      expected$team <- team
      expected$assets <- inputs$assets

      actual <- simulate_team_draft_portfolio(
        team_value = team,
        iterations = 250L,
        current_year = 2026L,
        random_seed = 31415L
      )

      tbi_test_expect_draft_simulation_identical(
        actual,
        expected
      )
    }
  })
})

testthat::test_that("custom simulation rules preserve exact RNG draws and distributions", {
  tbi_test_draft_with_read_only_db(function() {
    inputs <- tbi_test_get_draft_simulation_inputs_reference(
      "New York Knicks"
    )
    overrides <- list(
      expected_slot_sd = c(first = 3.5, second = 7.25),
      default_conveyance_probability = 0.55,
      default_swap_exercise_probability = 0.25
    )

    expected <- tbi_test_simulate_draft_portfolio_reference(
      assets = inputs$assets,
      conditions_lookup = inputs$conditions_lookup,
      iterations = 250L,
      current_year = 2026L,
      random_seed = 2718L,
      rule_overrides = overrides
    )
    expected_rng <- .Random.seed
    actual <- simulate_draft_portfolio(
      assets = inputs$assets,
      conditions_lookup = inputs$conditions_lookup,
      iterations = 250L,
      current_year = 2026L,
      random_seed = 2718L,
      rule_overrides = overrides
    )
    actual_rng <- .Random.seed

    tbi_test_expect_draft_simulation_identical(actual, expected)
    testthat::expect_identical(actual_rng, expected_rng)
  })
})

testthat::test_that("real draft-capital profiles retain value, grade, control, and review summaries", {
  expected <- list(
    "New York Knicks" = list(
      assets = 19L,
      gross = 94.21541,
      obligations_value = 89.74328,
      net = 4.472129,
      obligations = 6L,
      review = 19L,
      grade = "Limited",
      controls = c(Incoming = 7L, Outgoing = 6L, Own = 6L)
    ),
    "Brooklyn Nets" = list(
      assets = 30L,
      gross = 301.963,
      obligations_value = 10.88889,
      net = 291.0741,
      obligations = 1L,
      review = 30L,
      grade = "Elite",
      controls = c(Incoming = 17L, Own = 12L, `Swap Obligation` = 1L)
    ),
    "Memphis Grizzlies" = list(
      assets = 25L,
      gross = 292.6561,
      obligations_value = 12.30767,
      net = 280.3484,
      obligations = 4L,
      review = 25L,
      grade = "Elite",
      controls = c(Incoming = 10L, Outgoing = 4L, Own = 10L, `Swap Right` = 1L)
    ),
    "Phoenix Suns" = list(
      assets = 7L,
      gross = 21.1737,
      obligations_value = 81.81561,
      net = -60.64191,
      obligations = 6L,
      review = 7L,
      grade = "Obligation Heavy",
      controls = c(Outgoing = 6L, Own = 1L)
    )
  )

  tbi_test_draft_with_read_only_db(function() {
    for (team in names(expected)) {
      contract <- expected[[team]]
      result <- evaluate_team_draft_value(
        team_value = team,
        current_year = 2026L
      )
      summary <- result$summary
      controls <- table(result$assets$control_type)

      testthat::expect_identical(nrow(result$assets), contract$assets)
      testthat::expect_equal(summary$gross_asset_value, contract$gross, tolerance = 1e-6)
      testthat::expect_equal(summary$gross_obligation_value, contract$obligations_value, tolerance = 1e-6)
      testthat::expect_equal(summary$net_portfolio_value, contract$net, tolerance = 1e-6)
      testthat::expect_identical(summary$obligations, contract$obligations)
      testthat::expect_identical(summary$review_required, contract$review)
      testthat::expect_identical(summary$portfolio_grade, contract$grade)
      testthat::expect_identical(
        as.integer(controls[names(contract$controls)]),
        as.integer(contract$controls)
      )
      testthat::expect_identical(
        result$valued_assets$requires_manual_review,
        rep(TRUE, contract$assets)
      )
    }
  })
})

testthat::test_that("the frozen New York input path records its hardened read-only DB call pattern", {
  tbi_test_draft_with_read_only_db(function() {
    reference <- tbi_test_count_draft_db_calls(function() {
      tbi_test_get_draft_simulation_inputs_reference(
        "New York Knicks"
      )
    })

    testthat::expect_identical(
      reference$counts,
      c(
        queries = 59L,
        executes = 40L,
        table_lists = 20L,
        field_lists = 0L
      )
    )
  })
})

testthat::test_that("the optimized New York input path uses one read-only batch", {
  tbi_test_draft_with_read_only_db(function() {
    optimized <- tbi_test_count_draft_db_calls(function() {
      get_draft_simulation_inputs("New York Knicks")
    })

    testthat::expect_identical(
      optimized$counts,
      c(
        queries = 3L,
        executes = 2L,
        table_lists = 1L,
        field_lists = 0L
      )
    )
  })
})
