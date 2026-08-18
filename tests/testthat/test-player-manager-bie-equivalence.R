testthat::test_that("New York reference captures the veteran-heavy current roster", {
  captured <- tbi_test_pm_capture_and_compare("New York Knicks")
  result <- captured$data

  testthat::expect_identical(nrow(result), 13L)
  testthat::expect_true(all(result$bie_score_source == "PERFORMANCE_DATA"))
  testthat::expect_true(all(is.finite(result$bie_player_score)))
  testthat::expect_identical(result$player_id, captured$pool$player_id)
  testthat::expect_identical(result$player_name, captured$pool$player_name)
  testthat::expect_true(all(result$transaction_role == "CURRENT"))
})

testthat::test_that("Oklahoma City reference preserves fallback rows and order", {
  captured <- tbi_test_pm_capture_and_compare("Oklahoma City Thunder")
  result <- captured$data
  fallback <- result$bie_score_source == "FOUNDATION"
  score_columns <- c(
    "bie_player_score",
    "bie_impact_score",
    "bie_offense_score",
    "bie_defense_score",
    "bie_efficiency_score",
    "bie_playmaking_score",
    "bie_rebounding_score"
  )

  testthat::expect_identical(nrow(result), 20L)
  testthat::expect_identical(sum(fallback), 8L)
  testthat::expect_true(all(is.na(result[fallback, score_columns, drop = FALSE])))
  testthat::expect_true(all(result$bie_metric_components[fallback] == 0L))
  testthat::expect_true(all(result$performance_season_used[fallback] == "2026-27"))
  testthat::expect_true(all(is.na(result$primary_role[fallback])))
  testthat::expect_true(all(is.na(result$archetype[fallback])))
  testthat::expect_identical(result$player_id, captured$pool$player_id)
  testthat::expect_identical(result$player_name, captured$pool$player_name)
  testthat::expect_true(all(result$transaction_role == "CURRENT"))
})

testthat::test_that("Memphis reference preserves multi-position text verbatim", {
  captured <- tbi_test_pm_capture_and_compare("Memphis Grizzlies")
  result <- captured$data
  multi_position <- grepl(",", captured$pool$primary_position, fixed = TRUE)

  testthat::expect_identical(nrow(result), 22L)
  testthat::expect_identical(sum(multi_position, na.rm = TRUE), 15L)
  testthat::expect_identical(result$primary_position, captured$pool$primary_position)
  testthat::expect_identical(
    result$primary_position[multi_position],
    captured$pool$primary_position[multi_position]
  )
  testthat::expect_false("secondary_position" %in% names(result))
})

testthat::test_that("Houston reference protects Tristen Newton's current and historical rows", {
  captured <- tbi_test_pm_capture_and_compare("Houston Rockets")
  result <- captured$data
  tristen <- result[result$player_name == "Tristen Newton", , drop = FALSE]

  testthat::expect_identical(nrow(tristen), 1L)
  testthat::expect_identical(tristen$performance_season_used[[1]], "2025-26")
  testthat::expect_identical(tristen$source_team[[1]], "HOU")
  testthat::expect_false(any(c(
    "team_id",
    "stats_team_id",
    "shooting_team_id",
    "playmaking_team_id",
    "defense_team_id",
    "roles_team_id",
    "impact_team_id"
  ) %in% names(tristen)))

  tbi_test_pm_with_read_only_connection(function(con) {
    tied <- DBI::dbGetQuery(
      con,
      "
      SELECT team_id, minutes
      FROM player_season_stats
      WHERE player_id = ?
        AND season = ?
        AND COALESCE(minutes, 0) = (
          SELECT MAX(COALESCE(minutes, 0))
          FROM player_season_stats
          WHERE player_id = ?
            AND season = ?
        )
      ORDER BY team_id
      ",
      params = list(550L, "2024-25", 550L, "2024-25")
    )

    testthat::expect_identical(tied$team_id, c(12L, 18L))
    testthat::expect_equal(tied$minutes, c(8, 8), tolerance = 0)

    evidence_tables <- c(
      "player_season_stats",
      "player_season_shooting",
      "player_season_playmaking",
      "player_season_defense_rebounding",
      "player_season_roles",
      "player_season_impact"
    )
    selected_team_ids <- vapply(
      evidence_tables,
      function(table_name) {
        row <- tbi_test_pm_latest_row_reference(
          con,
          table_name,
          player_id = 550L,
          performance_season = "2024-25"
        )
        as.integer(row$team_id[[1]])
      },
      integer(1)
    )

    testthat::expect_identical(unname(selected_team_ids), rep(12L, 6L))

    historical_rows <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM player_season_stats
      WHERE player_id = ?
        AND season = ?
      ORDER BY team_id
      ",
      params = list(550L, "2024-25")
    )
    testthat::expect_gte(nrow(historical_rows), 2L)

    updated_at_rows <- historical_rows[
      ,
      setdiff(names(historical_rows), "minutes"),
      drop = FALSE
    ]
    updated_order <- order(
      as.character(updated_at_rows$updated_at),
      decreasing = TRUE,
      na.last = TRUE,
      method = "radix"
    )[[1]]
    selected_by_update <- player_manager_batched_latest_row(
      updated_at_rows,
      names(updated_at_rows),
      player_id = 550L,
      performance_season = "2024-25"
    )
    tbi_test_expect_pm_bie_equivalent(
      selected_by_update,
      updated_at_rows[updated_order, , drop = FALSE],
      tolerance = 0
    )

    database_order_rows <- updated_at_rows[
      ,
      setdiff(names(updated_at_rows), "updated_at"),
      drop = FALSE
    ]
    selected_by_database_order <- player_manager_batched_latest_row(
      database_order_rows,
      names(database_order_rows),
      player_id = 550L,
      performance_season = "2024-25"
    )
    tbi_test_expect_pm_bie_equivalent(
      selected_by_database_order,
      database_order_rows[1, , drop = FALSE],
      tolerance = 0
    )
  })

  historical_pool <- captured$pool[
    captured$pool$player_name == "Tristen Newton",
    ,
    drop = FALSE
  ]
  historical <- tbi_test_pm_reference_for_pool(
    historical_pool,
    roster_season = "2024-25"
  )
  historical_batched <- tbi_test_pm_batched_for_pool(
    historical_pool,
    roster_season = "2024-25"
  )
  tbi_test_expect_pm_bie_equivalent(historical_batched, historical)
  testthat::expect_identical(historical_pool$abbreviation[[1]], "HOU")
  testthat::expect_identical(historical$performance_season_used[[1]], "2024-25")
  testthat::expect_identical(historical$source_team[[1]], "IND")
})

testthat::test_that("trade scenario pool preserves removal, append, and final ordering", {
  knicks <- tbi_test_pm_base_pool_reference("New York Knicks")
  nets <- tbi_test_pm_base_pool_reference("Brooklyn Nets")
  outgoing <- knicks[1, , drop = FALSE]
  incoming <- nets[!nets$player_id %in% knicks$player_id, , drop = FALSE][1, , drop = FALSE]
  scenario <- list(
    active = TRUE,
    scenario_type = "trade",
    season = "2026-27",
    team = "New York Knicks",
    partner_team = "Brooklyn Nets",
    outgoing_players = outgoing[, "player_id", drop = FALSE],
    incoming_players = incoming[, "player_id", drop = FALSE],
    outgoing_salary = as.numeric(outgoing$cap_hit[[1]]),
    incoming_salary = as.numeric(incoming$cap_hit[[1]]),
    salary_delta = as.numeric(incoming$cap_hit[[1]]) - as.numeric(outgoing$cap_hit[[1]])
  )
  transaction_state <- list(snapshot = shiny::reactiveVal(scenario))
  captured <- tbi_test_pm_capture_and_compare(
    "New York Knicks",
    transaction_state = transaction_state
  )

  expected_pool <- knicks[knicks$player_id != outgoing$player_id[[1]], , drop = FALSE]
  expected_pool$transaction_role <- "CURRENT"
  incoming$team_name <- "New York Knicks"
  incoming$transaction_role <- "TRADE IN"
  expected_pool <- rbind(expected_pool, incoming[, names(expected_pool), drop = FALSE])
  cap_sort <- suppressWarnings(as.numeric(expected_pool$cap_hit))
  cap_sort[is.na(cap_sort)] <- 0
  expected_pool <- expected_pool[
    order(-cap_sort, expected_pool$player_name),
    ,
    drop = FALSE
  ]
  rownames(expected_pool) <- NULL

  tbi_test_expect_pm_bie_equivalent(captured$pool, expected_pool, tolerance = 0)
  testthat::expect_false(outgoing$player_id[[1]] %in% captured$pool$player_id)
  testthat::expect_identical(
    captured$pool$transaction_role[captured$pool$player_id == incoming$player_id[[1]]],
    "TRADE IN"
  )
  testthat::expect_identical(
    captured$pool$player_id,
    expected_pool$player_id
  )
  testthat::expect_identical(
    captured$data$transaction_role,
    expected_pool$transaction_role
  )
})

testthat::test_that("reference preserves duplicate pool rows exactly", {
  pool <- tbi_test_pm_base_pool_reference("New York Knicks")[1:2, , drop = FALSE]
  duplicated_pool <- rbind(
    pool[1, , drop = FALSE],
    pool[1, , drop = FALSE],
    pool[2, , drop = FALSE]
  )
  rownames(duplicated_pool) <- NULL
  result <- tbi_test_pm_reference_for_pool(duplicated_pool)
  batched <- tbi_test_pm_batched_for_pool(duplicated_pool)

  tbi_test_expect_pm_bie_equivalent(batched, result)
  testthat::expect_identical(nrow(result), 3L)
  testthat::expect_identical(
    result$player_id,
    c(pool$player_id[[1]], pool$player_id[[1]], pool$player_id[[2]])
  )
  testthat::expect_identical(as.list(result[1, ]), as.list(result[2, ]))
})

testthat::test_that("season selection remains league-wide with exact roster fallback", {
  tbi_test_pm_with_read_only_connection(function(con) {
    tristen_season <- tbi_test_pm_latest_performance_season_reference(
      con,
      player_id = 550L,
      roster_season = "2024-25"
    )
    tristen_stats <- tbi_test_pm_latest_row_reference(
      con,
      "player_season_stats",
      player_id = 550L,
      performance_season = tristen_season
    )

    testthat::expect_identical(tristen_season, "2024-25")
    testthat::expect_identical(as.integer(tristen_stats$team_id[[1]]), 12L)
    testthat::expect_identical(tristen_stats$source_team[[1]], "IND")

    metadata <- player_manager_batched_metadata(
      con,
      c(
        "player_season_stats",
        "player_season_shooting",
        "player_season_playmaking",
        "player_season_defense_rebounding",
        "player_season_roles",
        "player_season_impact"
      )
    )
    testthat::expect_identical(
      player_manager_batched_performance_seasons(
        con,
        player_ids = 550L,
        roster_season = "2024-25",
        metadata = metadata
      ),
      tristen_season
    )

    fallback_player <- DBI::dbGetQuery(
      con,
      "
      SELECT rh.player_id
      FROM roster_history rh
      INNER JOIN teams t ON t.team_id = rh.team_id
      WHERE t.team_name = ?
        AND rh.season = ?
        AND NOT EXISTS (
          SELECT 1
          FROM player_season_stats s
          WHERE s.player_id = rh.player_id
            AND s.season <= rh.season
            AND COALESCE(s.games_played, 0) > 0
        )
      ORDER BY rh.player_id
      LIMIT 1
      ",
      params = list("Oklahoma City Thunder", "2026-27")
    )
    fallback_id <- as.integer(fallback_player$player_id[[1]])
    fallback_season <- tbi_test_pm_latest_performance_season_reference(
      con,
      player_id = fallback_id,
      roster_season = "2026-27"
    )
    fallback_stats <- tbi_test_pm_latest_row_reference(
      con,
      "player_season_stats",
      player_id = fallback_id,
      performance_season = fallback_season
    )

    testthat::expect_identical(fallback_season, "2026-27")
    testthat::expect_identical(nrow(fallback_stats), 0L)
    testthat::expect_identical(
      player_manager_batched_performance_seasons(
        con,
        player_ids = fallback_id,
        roster_season = "2026-27",
        metadata = metadata
      ),
      fallback_season
    )
    testthat::expect_identical(
      nrow(tbi_test_pm_latest_row_reference(
        con,
        "player_season_table_that_does_not_exist",
        player_id = fallback_id,
        performance_season = fallback_season
      )),
      0L
    )
  })
})

testthat::test_that("real zero-game historical evidence loads after roster fallback", {
  tbi_test_pm_with_read_only_connection(function(con) {
    player <- DBI::dbGetQuery(
      con,
      "
      SELECT
        p.player_id,
        p.player_name,
        p.primary_position,
        p.player_age,
        p.height_inches,
        p.weight_lbs,
        t.team_name,
        t.abbreviation
      FROM players p
      INNER JOIN player_season_stats s ON s.player_id = p.player_id
      INNER JOIN teams t ON t.team_id = s.team_id
      WHERE p.player_name = ?
        AND s.season = ?
      LIMIT 1
      ",
      params = list("Gabe McGlothan", "2025-26")
    )
    player$transaction_role <- "CURRENT"

    qualifying <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS qualifying_rows
      FROM player_season_stats
      WHERE player_id = ?
        AND season <= ?
        AND COALESCE(games_played, 0) > 0
      ",
      params = list(as.integer(player$player_id[[1]]), "2025-26")
    )
    performance_season <- tbi_test_pm_latest_performance_season_reference(
      con,
      player_id = player$player_id[[1]],
      roster_season = "2025-26"
    )
    result <- tbi_test_pm_team_data_reference(player, "2025-26", con)
    batched <- player_manager_batched_team_data(player, "2025-26", con)
    impact <- tbi_test_pm_latest_row_reference(
      con,
      "player_season_impact",
      player_id = player$player_id[[1]],
      performance_season = performance_season
    )

    tbi_test_expect_pm_bie_equivalent(batched, result)
    testthat::expect_identical(as.integer(qualifying$qualifying_rows[[1]]), 0L)
    testthat::expect_identical(performance_season, "2025-26")
    testthat::expect_identical(result$games_played[[1]], 0L)
    testthat::expect_identical(result$performance_season_used[[1]], "2025-26")
    testthat::expect_identical(result$bie_score_source[[1]], "PERFORMANCE_DATA")
    testthat::expect_equal(
      result$bie_player_score[[1]],
      impact$bie_performance_rating[[1]],
      tolerance = TBI_PM_BIE_EQUIVALENCE_TOLERANCE
    )
    testthat::expect_identical(result$bie_metric_components[[1]], 3L)
    testthat::expect_true(is.na(result$bie_defense_score[[1]]))
    testthat::expect_true(is.na(result$bie_playmaking_score[[1]]))
    testthat::expect_true(is.na(result$bie_rebounding_score[[1]]))
    testthat::expect_true(is.finite(result$bie_impact_score[[1]]))
    testthat::expect_true(is.finite(result$bie_offense_score[[1]]))
    testthat::expect_true(is.finite(result$bie_efficiency_score[[1]]))
    testthat::expect_identical(result$primary_role[[1]], "CONNECTOR")
    testthat::expect_identical(result$archetype[[1]], "ROLE PLAYER / MIXED PROFILE")
  })
})

testthat::test_that("evidence merge order and collision prefixes remain exact", {
  pool <- tbi_test_pm_base_pool_reference("New York Knicks")[1, , drop = FALSE]

  tbi_test_pm_with_read_only_connection(function(con) {
    player_id <- as.integer(pool$player_id[[1]])
    performance_season <- tbi_test_pm_latest_performance_season_reference(
      con,
      player_id,
      "2026-27"
    )
    tables <- c(
      stats = "player_season_stats",
      shooting = "player_season_shooting",
      playmaking = "player_season_playmaking",
      defense = "player_season_defense_rebounding",
      roles = "player_season_roles",
      impact = "player_season_impact"
    )
    evidence <- lapply(
      tables,
      function(table_name) {
        tbi_test_pm_latest_row_reference(
          con,
          table_name,
          player_id,
          performance_season
        )
      }
    )
    result <- tbi_test_pm_team_data_reference(pool, "2026-27", con)
    batched <- player_manager_batched_team_data(pool, "2026-27", con)

    tbi_test_expect_pm_bie_equivalent(batched, result)
    ordered_markers <- match(
      c(
        "games_played",
        "shooting_games_played",
        "playmaking_games_played",
        "defense_games_played",
        "roles_games_played",
        "impact_games_played"
      ),
      names(result)
    )
    testthat::expect_true(all(diff(ordered_markers) > 0))
    testthat::expect_equal(result$minutes[[1]], evidence$stats$minutes[[1]], tolerance = 0)
    testthat::expect_equal(
      result$shooting_minutes[[1]],
      evidence$shooting$minutes[[1]],
      tolerance = 0
    )
    testthat::expect_equal(
      result$playmaking_minutes[[1]],
      evidence$playmaking$minutes[[1]],
      tolerance = 0
    )
    testthat::expect_equal(
      result$defense_minutes[[1]],
      evidence$defense$minutes[[1]],
      tolerance = 0
    )
    testthat::expect_equal(result$roles_minutes[[1]], evidence$roles$minutes[[1]], tolerance = 0)
    testthat::expect_equal(result$impact_minutes[[1]], evidence$impact$minutes[[1]], tolerance = 0)
    testthat::expect_identical(result$primary_role[[1]], evidence$roles$primary_role[[1]])
    testthat::expect_identical(result$impact_primary_role[[1]], evidence$impact$primary_role[[1]])
    testthat::expect_identical(result$archetype[[1]], evidence$roles$archetype[[1]])
    testthat::expect_identical(result$impact_archetype[[1]], evidence$impact$archetype[[1]])
    testthat::expect_identical(result$evidence_count[[1]], evidence$roles$evidence_count[[1]])
    testthat::expect_identical(
      result$impact_evidence_count[[1]],
      evidence$impact$evidence_count[[1]]
    )
    testthat::expect_false(any(c(
      "team_id",
      "stats_team_id",
      "shooting_team_id",
      "playmaking_team_id",
      "defense_team_id",
      "roles_team_id",
      "impact_team_id"
    ) %in% names(result)))
  })
})

testthat::test_that("first-row evidence availability controls union column order", {
  pool <- tbi_test_pm_base_pool_reference("Oklahoma City Thunder")
  full <- tbi_test_pm_reference_for_pool(pool)
  fallback_index <- which(full$bie_score_source == "FOUNDATION")[[1]]
  evidence_index <- which(full$bie_score_source == "PERFORMANCE_DATA")[[1]]
  fallback_first_pool <- rbind(
    pool[fallback_index, , drop = FALSE],
    pool[evidence_index, , drop = FALSE]
  )
  evidence_first_pool <- fallback_first_pool[2:1, , drop = FALSE]
  rownames(fallback_first_pool) <- NULL
  rownames(evidence_first_pool) <- NULL

  fallback_first <- tbi_test_pm_reference_for_pool(fallback_first_pool)
  evidence_first <- tbi_test_pm_reference_for_pool(evidence_first_pool)
  fallback_first_batched <- tbi_test_pm_batched_for_pool(fallback_first_pool)
  evidence_first_batched <- tbi_test_pm_batched_for_pool(evidence_first_pool)

  tbi_test_expect_pm_bie_equivalent(fallback_first_batched, fallback_first)
  tbi_test_expect_pm_bie_equivalent(evidence_first_batched, evidence_first)
  testthat::expect_setequal(names(fallback_first), names(evidence_first))
  testthat::expect_false(identical(names(fallback_first), names(evidence_first)))
  testthat::expect_lt(
    match("bie_player_score", names(fallback_first)),
    match("games_played", names(fallback_first))
  )
  testthat::expect_lt(
    match("games_played", names(evidence_first)),
    match("bie_player_score", names(evidence_first))
  )
})

testthat::test_that("empty pool is returned unchanged", {
  empty_pool <- tbi_test_pm_base_pool_reference("New York Knicks")[0, , drop = FALSE]
  result <- tbi_test_pm_reference_for_pool(empty_pool)
  batched <- tbi_test_pm_batched_for_pool(empty_pool)

  tbi_test_expect_pm_bie_equivalent(batched, result)
  testthat::expect_identical(result, empty_pool)
})

testthat::test_that("15-player roster uses seven batched queries and cached metadata", {
  pool <- tbi_test_pm_base_pool_reference("Oklahoma City Thunder")[1:15, , drop = FALSE]

  tbi_test_pm_with_read_only_connection(function(con) {
    reference <- tbi_test_pm_count_dbi_calls(function() {
      tbi_test_pm_team_data_reference(pool, "2026-27", con)
    })
    batched <- tbi_test_pm_count_dbi_calls(function() {
      player_manager_batched_team_data(pool, "2026-27", con)
    })

    tbi_test_expect_pm_bie_equivalent(batched$value, reference$value)
    testthat::expect_identical(
      reference$counts,
      c(queries = 105L, table_lists = 105L, field_lists = 105L)
    )
    testthat::expect_identical(
      batched$counts,
      c(queries = 7L, table_lists = 1L, field_lists = 6L)
    )
  })
})
