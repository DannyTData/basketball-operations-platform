# Frozen Stage B characterization oracle for Player Management BIE assembly.
#
# This helper deliberately preserves the current per-player N+1 behavior. It is
# test-only so a later batched implementation can be compared with the exact
# legacy contract without keeping the old query path in production.

TBI_PM_BIE_EQUIVALENCE_TOLERANCE <- 1e-12

tbi_test_pm_db_path <- function() {
  resolve_tbi_db_path(file.path("inst", "database", "tbi.sqlite"))
}

tbi_test_pm_db_hash <- function(db_path = tbi_test_pm_db_path()) {
  unname(tools::md5sum(db_path)[[1]])
}

tbi_test_pm_with_read_only_connection <- function(code, db_path = tbi_test_pm_db_path()) {
  before <- tbi_test_pm_db_hash(db_path)
  con <- connect_db(db_path = db_path, read_only = TRUE)

  on.exit(
    {
      disconnect_db(con)
      after <- tbi_test_pm_db_hash(db_path)

      if (!identical(before, after)) {
        stop("The authoritative TBI database changed during a read-only equivalence test.", call. = FALSE)
      }
    },
    add = TRUE
  )

  code(con)
}

tbi_test_pm_count_dbi_calls <- function(code) {
  counts <- new.env(parent = emptyenv())
  counts$queries <- 0L
  counts$table_lists <- 0L
  counts$field_lists <- 0L
  production_db_get_query <- get("dbGetQuery", envir = asNamespace("DBI"))
  production_db_list_tables <- get("dbListTables", envir = asNamespace("DBI"))
  production_db_list_fields <- get("dbListFields", envir = asNamespace("DBI"))

  testthat::local_mocked_bindings(
    dbGetQuery = function(...) {
      counts$queries <- counts$queries + 1L
      production_db_get_query(...)
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
      table_lists = counts$table_lists,
      field_lists = counts$field_lists
    )
  )
}

tbi_test_pm_latest_performance_season_reference <- function(
    con,
    player_id,
    roster_season) {
  if (!"player_season_stats" %in% DBI::dbListTables(con)) {
    return(as.character(roster_season))
  }

  fields <- DBI::dbListFields(con, "player_season_stats")

  if (!"player_id" %in% fields || !"season" %in% fields) {
    return(as.character(roster_season))
  }

  game_filter <- if ("games_played" %in% fields) {
    "AND COALESCE(games_played, 0) > 0"
  } else {
    ""
  }

  result <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "
        SELECT season
        FROM player_season_stats
        WHERE player_id = ?
          AND season <= ?
        ",
        game_filter,
        "
        GROUP BY season
        ORDER BY season DESC
        LIMIT 1
        "
      ),
      params = list(
        as.integer(player_id),
        as.character(roster_season)
      )
    ),
    error = function(e) data.frame()
  )

  if (!nrow(result) || is.na(result$season[[1]])) {
    return(as.character(roster_season))
  }

  as.character(result$season[[1]])
}

tbi_test_pm_latest_row_reference <- function(
    con,
    table_name,
    player_id,
    performance_season) {
  if (!table_name %in% DBI::dbListTables(con)) {
    return(data.frame())
  }

  fields <- DBI::dbListFields(con, table_name)

  if (!"player_id" %in% fields) {
    return(data.frame())
  }

  where <- "player_id = ?"
  params <- list(as.integer(player_id))

  if ("season" %in% fields) {
    where <- c(where, "season = ?")
    params <- c(params, list(as.character(performance_season)))
  }

  order_clause <- if ("minutes" %in% fields) {
    " ORDER BY COALESCE(minutes, 0) DESC "
  } else if ("updated_at" %in% fields) {
    " ORDER BY updated_at DESC "
  } else {
    ""
  }

  tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM ",
        table_name,
        " WHERE ",
        paste(where, collapse = " AND "),
        order_clause,
        " LIMIT 1"
      ),
      params = params
    ),
    error = function(e) data.frame()
  )
}

tbi_test_pm_merge_evidence_row_reference <- function(base, extra, prefix = NULL) {
  if (is.null(extra) || !is.data.frame(extra) || !nrow(extra)) {
    return(base)
  }

  extra <- extra[1, , drop = FALSE]
  key_columns <- c(
    "player_id",
    "team_id",
    "season",
    "source_name",
    "source_player_id",
    "imported_at",
    "updated_at",
    "metric_version"
  )

  for (column in setdiff(names(extra), key_columns)) {
    target <- column

    if (target %in% names(base) && !is.null(prefix)) {
      target <- paste0(prefix, column)
    }

    if (!target %in% names(base)) {
      base[[target]] <- extra[[column]][[1]]
    } else {
      current <- base[[target]][[1]]
      replacement <- extra[[column]][[1]]

      if (
        (is.null(current) || !length(current) || is.na(current)) &&
          !is.null(replacement) &&
          length(replacement) &&
          !is.na(replacement)
      ) {
        base[[target]][[1]] <- replacement
      }
    }
  }

  base
}

tbi_test_pm_team_data_reference <- function(pool, roster_season, con) {
  if (!nrow(pool)) {
    return(pool)
  }

  rows <- lapply(
    seq_len(nrow(pool)),
    function(i) {
      roster_row <- pool[i, , drop = FALSE]
      player_id <- suppressWarnings(as.integer(roster_row$player_id[[1]]))
      performance_season <- tbi_test_pm_latest_performance_season_reference(
        con = con,
        player_id = player_id,
        roster_season = roster_season
      )

      stats <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_stats",
        player_id,
        performance_season
      )
      impact <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_impact",
        player_id,
        performance_season
      )
      roles <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_roles",
        player_id,
        performance_season
      )
      shooting <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_shooting",
        player_id,
        performance_season
      )
      playmaking <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_playmaking",
        player_id,
        performance_season
      )
      defense <- tbi_test_pm_latest_row_reference(
        con,
        "player_season_defense_rebounding",
        player_id,
        performance_season
      )

      row <- roster_row
      row <- tbi_test_pm_merge_evidence_row_reference(row, stats, "stats_")
      row <- tbi_test_pm_merge_evidence_row_reference(row, shooting, "shooting_")
      row <- tbi_test_pm_merge_evidence_row_reference(row, playmaking, "playmaking_")
      row <- tbi_test_pm_merge_evidence_row_reference(row, defense, "defense_")
      row <- tbi_test_pm_merge_evidence_row_reference(row, roles, "roles_")
      row <- tbi_test_pm_merge_evidence_row_reference(row, impact, "impact_")

      get_first_numeric <- function(candidates) {
        for (name in candidates) {
          if (name %in% names(row)) {
            value <- suppressWarnings(as.numeric(row[[name]][[1]]))

            if (length(value) && is.finite(value)) {
              return(value)
            }
          }
        }

        NA_real_
      }

      rating <- get_first_numeric(c(
        "bie_performance_rating",
        "impact_bie_performance_rating"
      ))
      all_around <- get_first_numeric(c(
        "all_around_impact_score",
        "impact_all_around_impact_score"
      ))
      offense <- get_first_numeric(c(
        "offensive_impact_score",
        "impact_offensive_impact_score"
      ))
      defense_score <- get_first_numeric(c(
        "defensive_impact_score",
        "impact_defensive_impact_score"
      ))
      shooting_score <- get_first_numeric(c(
        "shooting_component",
        "impact_shooting_component",
        "shooting_efficiency_score",
        "shooting_shooting_efficiency_score"
      ))
      playmaking_score <- get_first_numeric(c(
        "creation_component",
        "impact_creation_component",
        "creation_score",
        "playmaking_creation_score"
      ))
      rebounding_score <- get_first_numeric(c(
        "rebounding_component",
        "impact_rebounding_component",
        "rebounding_score",
        "defense_rebounding_score"
      ))

      row$bie_player_score <- rating
      row$bie_impact_score <- all_around
      row$bie_offense_score <- offense
      row$bie_defense_score <- defense_score
      row$bie_efficiency_score <- shooting_score
      row$bie_playmaking_score <- playmaking_score
      row$bie_rebounding_score <- rebounding_score

      components <- c(
        all_around,
        offense,
        defense_score,
        shooting_score,
        playmaking_score,
        rebounding_score
      )

      row$bie_metric_components <- sum(is.finite(components))
      row$bie_score_source <- if (is.finite(rating)) {
        "PERFORMANCE_DATA"
      } else {
        "FOUNDATION"
      }
      row$performance_season_used <- performance_season
      row
    }
  )

  all_names <- unique(unlist(lapply(rows, names)))
  normalized <- lapply(
    rows,
    function(row) {
      missing <- setdiff(all_names, names(row))

      for (name in missing) {
        row[[name]] <- NA
      }

      row[, all_names, drop = FALSE]
    }
  )

  do.call(rbind, normalized)
}

tbi_test_pm_reference_for_pool <- function(
    pool,
    roster_season = "2026-27",
    db_path = tbi_test_pm_db_path()) {
  tbi_test_pm_with_read_only_connection(
    function(con) {
      tbi_test_pm_team_data_reference(pool, roster_season, con)
    },
    db_path = db_path
  )
}

tbi_test_pm_batched_for_pool <- function(
    pool,
    roster_season = "2026-27",
    db_path = tbi_test_pm_db_path()) {
  tbi_test_pm_with_read_only_connection(
    function(con) {
      player_manager_batched_team_data(pool, roster_season, con)
    },
    db_path = db_path
  )
}

tbi_test_pm_base_pool_reference <- function(
    team,
    season = "2026-27",
    db_path = tbi_test_pm_db_path()) {
  tbi_test_pm_with_read_only_connection(
    function(con) {
      result <- DBI::dbGetQuery(
        con,
        "
        SELECT DISTINCT
          p.player_id,
          p.player_name,
          p.primary_position,
          p.player_age,
          p.height_inches,
          p.weight_lbs,
          t.team_name,
          t.abbreviation,
          rh.roster_status,
          COALESCE(rh.two_way_flag, 0) AS two_way_flag,
          rh.jersey_number,
          cy.cap_hit,
          cy.base_salary,
          cy.guaranteed_amount,
          cy.option_type,
          c.contract_type,
          c.contract_start_season,
          c.contract_end_season,
          c.total_value,
          c.guaranteed_value,
          c.free_agent_year,
          c.bird_rights,
          c.trade_bonus_percent
        FROM roster_history rh
        INNER JOIN players p ON p.player_id = rh.player_id
        INNER JOIN teams t ON t.team_id = rh.team_id
        LEFT JOIN contract_years cy
          ON cy.player_id = rh.player_id
          AND cy.team_id = rh.team_id
          AND cy.season = rh.season
        LEFT JOIN contracts c ON c.contract_id = cy.contract_id
        WHERE t.team_name = ?
          AND rh.season = ?
        ORDER BY
          COALESCE(cy.cap_hit, 0) DESC,
          p.player_name
        ",
        params = list(team, season)
      )

      if (nrow(result)) {
        result$transaction_role <- "CURRENT"
      }

      result
    },
    db_path = db_path
  )
}

tbi_test_pm_capture_module <- function(
    team,
    season = "2026-27",
    transaction_state = NULL,
    include_data = TRUE,
    db_path = tbi_test_pm_db_path()) {
  before <- tbi_test_pm_db_hash(db_path)
  captured <- new.env(parent = emptyenv())
  selected_team <- shiny::reactiveVal(team)
  selected_season <- shiny::reactiveVal(season)
  production_connect_db <- get("connect_db", envir = asNamespace("basketballops"))

  # Force every connection opened by the live module down the read-only path,
  # including its otherwise read-only SELECTs whose legacy call omits the flag.
  testthat::local_mocked_bindings(
    connect_db = function(db_path = NULL, read_only = FALSE) {
      production_connect_db(db_path = db_path, read_only = TRUE)
    },
    .package = "basketballops"
  )

  shiny::testServer(
    mod_player_manager_server,
    args = list(
      selected_team = selected_team,
      selected_season = selected_season,
      transaction_state = transaction_state
    ),
    {
      session$flushReact()
      captured$pool <- player_pool()

      if (isTRUE(include_data)) {
        captured$data <- bie_team_player_data()
      }
    }
  )

  after <- tbi_test_pm_db_hash(db_path)

  if (!identical(before, after)) {
    stop("The authoritative TBI database changed during a module equivalence test.", call. = FALSE)
  }

  as.list(captured)
}

tbi_test_expect_pm_bie_equivalent <- function(
    actual,
    expected,
    tolerance = TBI_PM_BIE_EQUIVALENCE_TOLERANCE) {
  testthat::expect_identical(dim(actual), dim(expected))
  testthat::expect_identical(names(actual), names(expected))
  testthat::expect_identical(rownames(actual), rownames(expected))
  testthat::expect_identical(
    vapply(actual, typeof, character(1)),
    vapply(expected, typeof, character(1))
  )
  testthat::expect_identical(
    vapply(actual, function(column) paste(class(column), collapse = "/"), character(1)),
    vapply(expected, function(column) paste(class(column), collapse = "/"), character(1))
  )
  testthat::expect_identical(is.na(actual), is.na(expected))

  for (name in names(expected)) {
    if (is.numeric(expected[[name]])) {
      testthat::expect_equal(
        actual[[name]],
        expected[[name]],
        tolerance = tolerance
      )
    } else {
      testthat::expect_identical(actual[[name]], expected[[name]])
    }
  }

  invisible(TRUE)
}

tbi_test_pm_capture_and_compare <- function(
    team,
    season = "2026-27",
    transaction_state = NULL) {
  captured <- tbi_test_pm_capture_module(
    team = team,
    season = season,
    transaction_state = transaction_state
  )
  expected <- tbi_test_pm_reference_for_pool(
    captured$pool,
    roster_season = season
  )

  tbi_test_expect_pm_bie_equivalent(captured$data, expected)
  captured$reference <- expected
  captured
}
