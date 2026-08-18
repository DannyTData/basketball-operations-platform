# Frozen Stage B reference for the pre-optimization draft simulation path.
# Keep this test-only implementation unchanged while production is optimized.

tbi_test_draft_db_path <- function() {
  resolve_tbi_db_path(
    file.path("inst", "database", "tbi.sqlite")
  )
}

tbi_test_draft_db_hash <- function(
    db_path = tbi_test_draft_db_path()) {
  unname(tools::md5sum(db_path)[[1]])
}

tbi_test_draft_with_read_only_db <- function(
    code,
    db_path = tbi_test_draft_db_path()) {
  before <- tbi_test_draft_db_hash(db_path)
  production_connect_db <- get(
    "connect_db",
    envir = asNamespace("basketballops")
  )

  testthat::local_mocked_bindings(
    connect_db = function(db_path = NULL, read_only = FALSE) {
      production_connect_db(
        db_path = db_path,
        read_only = TRUE
      )
    },
    .package = "basketballops"
  )

  value <- code()
  after <- tbi_test_draft_db_hash(db_path)

  if (!identical(before, after)) {
    stop(
      "The authoritative TBI database changed during a read-only draft-simulation test.",
      call. = FALSE
    )
  }

  value
}

tbi_test_count_draft_db_calls <- function(code) {
  counts <- new.env(parent = emptyenv())
  counts$queries <- 0L
  counts$executes <- 0L
  counts$table_lists <- 0L
  counts$field_lists <- 0L
  production_db_get_query <- get(
    "dbGetQuery",
    envir = asNamespace("DBI")
  )
  production_db_execute <- get(
    "dbExecute",
    envir = asNamespace("DBI")
  )
  production_db_list_tables <- get(
    "dbListTables",
    envir = asNamespace("DBI")
  )
  production_db_list_fields <- get(
    "dbListFields",
    envir = asNamespace("DBI")
  )

  testthat::local_mocked_bindings(
    dbGetQuery = function(...) {
      counts$queries <- counts$queries + 1L
      production_db_get_query(...)
    },
    dbExecute = function(...) {
      counts$executes <- counts$executes + 1L
      production_db_execute(...)
    },
    dbListTables = function(...) {
      counts$table_lists <- counts$table_lists + 1L
      production_db_list_tables(...)
    },
    dbListFields = function(...) {
      counts$field_lists <- counts$field_lists + 1L
      production_db_list_fields(...)
    },
    .package = "DBI"
  )

  value <- code()

  list(
    value = value,
    counts = c(
      queries = counts$queries,
      executes = counts$executes,
      table_lists = counts$table_lists,
      field_lists = counts$field_lists
    )
  )
}

tbi_test_get_draft_simulation_inputs_reference <- function(
    team_value,
    year_from = NULL,
    year_to = NULL,
    db_path = NULL) {
  assets <- get_draft_assets(
    team_value = team_value,
    year_from = year_from,
    year_to = year_to,
    include_inactive = FALSE,
    db_path = db_path
  )

  conditions_lookup <- list()

  if (nrow(assets)) {
    for (i in seq_len(nrow(assets))) {
      asset_id <- assets$draft_asset_id[[i]]
      detail <- get_draft_asset_detail(
        draft_asset_id = asset_id,
        db_path = db_path
      )

      conditions_lookup[[as.character(asset_id)]] <-
        detail$conditions
    }
  }

  list(
    assets = assets,
    conditions_lookup = conditions_lookup
  )
}

tbi_test_simulate_draft_portfolio_reference <- function(
    assets,
    conditions_lookup = NULL,
    iterations = NULL,
    current_year = NULL,
    random_seed = NULL,
    rule_overrides = NULL) {
  if (!is.data.frame(assets)) {
    stop("assets must be a data frame.", call. = FALSE)
  }

  if (!nrow(assets)) {
    return(
      list(
        iterations = 0L,
        mean_portfolio_value = 0,
        median_portfolio_value = 0,
        worst_case_value = 0,
        expected_case_value = 0,
        best_case_value = 0,
        simulation_results = data.frame(),
        executive_summary =
          "No draft assets are available for simulation."
      )
    )
  }

  rules <- resolve_draft_simulation_rules(rule_overrides)
  iterations <- draft_sim_integer(
    iterations,
    rules$simulation_iterations
  )

  if (is.na(iterations) || iterations < 100L) {
    stop("iterations must be at least 100.", call. = FALSE)
  }

  random_seed <- draft_sim_integer(
    random_seed,
    rules$random_seed
  )

  set.seed(random_seed)
  portfolio_values <- numeric(iterations)

  for (i in seq_len(iterations)) {
    iteration_value <- 0

    for (row_index in seq_len(nrow(assets))) {
      asset_row <- assets[row_index, , drop = FALSE]
      asset_id <- if (
        "draft_asset_id" %in% names(asset_row)
      ) {
        as.character(asset_row$draft_asset_id[[1]])
      } else {
        as.character(row_index)
      }
      conditions <- NULL

      if (
        !is.null(conditions_lookup) &&
        is.list(conditions_lookup) &&
        asset_id %in% names(conditions_lookup)
      ) {
        conditions <- conditions_lookup[[asset_id]]
      }

      result <- simulate_draft_asset_once(
        asset = asset_row,
        conditions = conditions,
        current_year = current_year,
        rule_overrides = rule_overrides
      )

      iteration_value <- iteration_value +
        result$simulated_value
    }

    portfolio_values[[i]] <- iteration_value
  }

  quantiles <- stats::quantile(
    portfolio_values,
    probs = c(
      rules$worst_case_quantile,
      rules$expected_case_quantile,
      rules$best_case_quantile
    ),
    names = FALSE,
    na.rm = TRUE
  )
  mean_value <- mean(portfolio_values)
  median_value <- stats::median(portfolio_values)
  executive_summary <- paste0(
    "Across ",
    iterations,
    " simulations, estimated portfolio value averaged ",
    round(mean_value, 1),
    " points. The expected-case value was ",
    round(quantiles[[2]], 1),
    ", with a simulated range from ",
    round(quantiles[[1]], 1),
    " in the downside case to ",
    round(quantiles[[3]], 1),
    " in the upside case."
  )

  list(
    iterations = iterations,
    mean_portfolio_value = mean_value,
    median_portfolio_value = median_value,
    portfolio_value_sd = stats::sd(portfolio_values),
    worst_case_value = quantiles[[1]],
    expected_case_value = quantiles[[2]],
    best_case_value = quantiles[[3]],
    simulation_results = data.frame(
      iteration = seq_len(iterations),
      portfolio_value = portfolio_values,
      stringsAsFactors = FALSE
    ),
    executive_summary = executive_summary,
    model_label = rules$model_label,
    scope_note = paste(
      "Portfolio simulation is an internal uncertainty estimate,",
      "not a forecast of actual draft order or conveyance."
    )
  )
}

tbi_test_expect_draft_simulation_identical <- function(
    actual,
    expected) {
  testthat::expect_identical(names(actual), names(expected))
  testthat::expect_identical(actual, expected)

  if (
    is.list(expected) &&
    "simulation_results" %in% names(expected) &&
    is.data.frame(expected$simulation_results)
  ) {
    testthat::expect_identical(
      vapply(actual$simulation_results, typeof, character(1)),
      vapply(expected$simulation_results, typeof, character(1))
    )
    testthat::expect_identical(
      is.na(actual$simulation_results),
      is.na(expected$simulation_results)
    )
  }
}
