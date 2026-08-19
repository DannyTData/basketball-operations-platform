# ============================================================
# V1 rotation foundation characterization
# ============================================================

v1_characterization_roster <- function(n = 12L) {
  positions <- c("PG", "SG", "SF", "PF", "C", "G", "F", "C")

  data.frame(
    player_id = seq_len(n),
    player_name = sprintf("Player %02d", seq_len(n)),
    position = rep(positions, length.out = n),
    depth_order = rep(c(1L, 2L, 3L), length.out = n),
    is_starter = seq_len(n) <= 5L,
    availability_status = "AVAILABLE",
    bie_rating = seq(90, 60, length.out = n),
    projected_bie_rating = seq(91, 61, length.out = n),
    impact_score = seq(89, 59, length.out = n),
    recommended_minutes = rev(seq_len(n)),
    offensive_impact = seq(92, 62, length.out = n),
    defensive_impact = seq(88, 58, length.out = n),
    creation_score = seq(87, 57, length.out = n),
    spacing_score = seq(86, 56, length.out = n),
    rebounding_score = seq(85, 55, length.out = n),
    primary_role = "",
    archetype = "",
    impact_tier = "",
    stringsAsFactors = FALSE
  )
}


testthat::test_that(
  "active minute preparation freezes schema values rookie behavior and reads",
  {
    tbi_test_draft_with_read_only_db(function() {
      roster <- v1_characterization_roster(2L)
      roster$player_id <- c(900001L, 900002L)
      roster$player_name <- c("Designated Rookie", "Veteran")
      roster$is_starter <- c(TRUE, FALSE)
      roster$tbi_performance_available <- c(FALSE, TRUE)
      roster$bie_offense_score <- c(77, NA)

      captured <- tbi_test_count_draft_db_calls(function() {
        prepare_minute_allocation_roster(roster)
      })
      result <- captured$value

      testthat::expect_identical(
        names(result),
        c(
          "player_id", "player_name", "position", "depth_order",
          "is_starter", "availability_status", "bie_rating",
          "projected_bie_rating", "impact_score", "primary_role",
          "archetype", "impact_tier", "current_minutes",
          "offensive_impact", "defensive_impact", "creation_score",
          "spacing_score", "rebounding_score", "is_preseason_rookie",
          "tbi_prior_nba_games", "tbi_contract_start_season",
          "tbi_rookie_definition"
        )
      )
      testthat::expect_identical(result$player_id, c(900001L, 900002L))
      testthat::expect_identical(result$player_name, c("Designated Rookie", "Veteran"))
      testthat::expect_identical(result$is_starter, c(TRUE, FALSE))
      testthat::expect_equal(result$offensive_impact, c(77, result$bie_rating[[2]]))
      testthat::expect_identical(result$is_preseason_rookie, c(TRUE, TRUE))
      testthat::expect_identical(
        result$tbi_rookie_definition,
        c("ZERO_PRIOR_NBA_GAMES", "ZERO_PRIOR_NBA_GAMES")
      )
      testthat::expect_identical(
        captured$counts,
        c(queries = 4L, executes = 2L, table_lists = 1L, field_lists = 6L)
      )
    })
  }
)


testthat::test_that(
  "active lineup preparation freezes schema values ordering and rookie passthrough",
  {
    roster <- v1_characterization_roster(7L)
    roster$is_preseason_rookie <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
    roster$is_starter[[5]] <- TRUE
    roster$is_starter[[6]] <- FALSE
    roster$availability_status[[7]] <- "OUT"
    roster$recommended_minutes <- c(30, 30, 28, 26, 10, 40, 35)
    roster$bie_rating <- c(80, 80, 78, 76, 60, 99, 95)

    pool <- prepare_lineup_player_pool(roster)

    testthat::expect_identical(
      names(pool),
      c(
        "player_id", "player_name", "position", "availability_status",
        "recommended_minutes", "bie_rating", "offensive_impact",
        "defensive_impact", "creation_score", "spacing_score",
        "rebounding_score", "is_starter", "is_preseason_rookie"
      )
    )
    testthat::expect_identical(pool$player_id, roster$player_id)
    testthat::expect_true(pool$is_starter[[5]])
    testthat::expect_true(pool$is_preseason_rookie[[6]])

    db_calls <- tbi_test_draft_with_read_only_db(function() {
      tbi_test_count_draft_db_calls(function() {
        get_lineup_candidate_pool(roster, pool_size = 6L)
      })
    })
    testthat::expect_identical(
      db_calls$counts,
      c(queries = 1L, executes = 2L, table_lists = 1L, field_lists = 1L)
    )

    testthat::local_mocked_bindings(
      tbi_preseason_rookie_gate_active = function(season = "2026-27") TRUE,
      .package = "basketballops"
    )
    candidates <- get_lineup_candidate_pool(roster, pool_size = 6L)

    testthat::expect_false(6L %in% candidates$player_id)
    testthat::expect_false(7L %in% candidates$player_id)
    testthat::expect_true(5L %in% candidates$player_id)
    testthat::expect_identical(
      candidates$player_id[1:2],
      c(1L, 2L)
    )

  }
)


testthat::test_that(
  "approved lineup behavior is frozen for valid and malformed lock sets",
  {
    roster <- v1_characterization_roster(10L)
    original <- roster$is_starter
    valid <- c(PG = 6L, SG = 2L, SF = 3L, PF = 4L, C = 5L)

    applied <- phase11_apply_approved_lineup(roster, valid)
    testthat::expect_identical(applied$player_id[applied$is_starter], c(2L, 3L, 4L, 5L, 6L))
    testthat::expect_identical(applied$position[applied$player_id == 6L], "PG")

    unavailable <- roster
    unavailable$availability_status[unavailable$player_id == 6L] <- "OUT"
    unavailable_applied <- phase11_apply_approved_lineup(unavailable, valid)
    testthat::expect_true(
      unavailable_applied$is_starter[unavailable_applied$player_id == 6L]
    )
    testthat::expect_identical(
      unavailable_applied$availability_status[unavailable_applied$player_id == 6L],
      "OUT"
    )

    malformed <- list(
      missing_position = c(PG = 6L, SG = 2L, SF = 3L, PF = 4L),
      duplicate_player = c(PG = 1L, SG = 1L, SF = 3L, PF = 4L, C = 5L),
      unmatched_player = c(PG = 999L, SG = 2L, SF = 3L, PF = 4L, C = 5L),
      partial_match = c(PG = 6L, SG = 999L, SF = 3L, PF = 4L, C = 5L)
    )

    for (locks in malformed) {
      result <- phase11_apply_approved_lineup(roster, locks)
      if (length(locks) != 5L || length(unique(unname(locks))) != 5L) {
        testthat::expect_identical(result$is_starter, original)
      } else {
        testthat::expect_identical(sum(result$is_starter), 4L)
      }
    }

    roster$position[roster$player_id == 7L] <- "PG/SG"
    multi_position <- valid
    multi_position[["PG"]] <- 7L
    result <- phase11_apply_approved_lineup(roster, multi_position)
    testthat::expect_identical(result$position[result$player_id == 7L], "PG")
  }
)


testthat::test_that(
  "rotation selection freezes ties NAs availability shortfalls and sizes",
  {
    roster <- v1_characterization_roster(12L)
    roster$is_starter <- FALSE
    roster$depth_order <- 2L
    roster$minute_priority <- c(80, 80, NA, 75, 74, 73, 72, 71, 70, 69, 68, 67)
    roster$player_name[1:2] <- c("Beta", "Alpha")
    roster$availability_status[c(4L, 12L)] <- "OUT"

    selected <- select_rotation_players(roster, rotation_size = 8L)
    ranked_ids <- selected$player_id[order(selected$rotation_rank, na.last = NA)]
    testthat::expect_identical(ranked_ids[1:2], c(2L, 1L))
    testthat::expect_false(any(selected$in_rotation[c(4L, 12L)]))
    testthat::expect_false(selected$in_rotation[[3]])

    for (size in 8:12) {
      result <- select_rotation_players(
        transform(
          v1_characterization_roster(12L),
          minute_priority = seq(90, 79)
        ),
        rotation_size = size
      )
      testthat::expect_identical(sum(result$in_rotation), as.integer(size))
    }

    short <- v1_characterization_roster(8L)
    short$minute_priority <- seq(90, 83)
    short$availability_status[7:8] <- "OUT"
    short_result <- select_rotation_players(short, rotation_size = 12L)
    testthat::expect_identical(sum(short_result$in_rotation), 6L)
  }
)


testthat::test_that(
  "unavailable starters and V1 signatures caches and freeze status remain characterized",
  {
    roster <- v1_characterization_roster(10L)
    roster$availability_status[[1]] <- "OUT"
    scored <- score_minute_allocation_roster(roster)
    selected <- select_rotation_players(scored, rotation_size = 10L)

    testthat::expect_false(selected$in_rotation[selected$player_id == 1L])
    testthat::expect_identical(
      phase13_roster_signature(roster),
      phase13_roster_signature(roster[nrow(roster):1, , drop = FALSE])
    )
    changed <- roster
    changed$availability_status[[2]] <- "LIMITED"
    testthat::expect_false(
      identical(phase13_roster_signature(roster), phase13_roster_signature(changed))
    )

    contextual <- roster
    contextual$team_name <- "Boston Celtics"
    contextual$season <- "2026-27"
    other_context <- contextual
    other_context$team_name <- "Denver Nuggets"
    other_context$season <- "2027-28"
    testthat::expect_identical(
      phase13_roster_signature(contextual),
      phase13_roster_signature(other_context)
    )

    lineup <- c(PG = 1L, SG = 2L, SF = 3L, PF = 4L, C = 5L)
    changed_lineup <- lineup
    changed_lineup[["PG"]] <- 6L
    testthat::expect_false(identical(
      phase13_lineup_signature(lineup),
      phase13_lineup_signature(changed_lineup)
    ))
    key_10 <- paste(
      phase13_roster_signature(roster), phase13_lineup_signature(lineup), 10L,
      sep = "||"
    )
    key_11 <- paste(
      phase13_roster_signature(roster), phase13_lineup_signature(lineup), 11L,
      sep = "||"
    )
    testthat::expect_false(identical(key_10, key_11))

    scenario <- list(
      team = "Boston Celtics",
      partner_team = "Denver Nuggets",
      outgoing_players = data.frame(player_id = 1L),
      incoming_players = data.frame(player_id = 20L),
      outgoing_salary = 10,
      incoming_salary = 11
    )
    changed_scenario <- scenario
    changed_scenario$incoming_players <- data.frame(player_id = 21L)
    testthat::expect_false(identical(
      phase13_trade_signature(scenario),
      phase13_trade_signature(changed_scenario)
    ))

    cache <- phase13_cache_store(phase13_cache_new(), "key", list(value = 1L))
    testthat::expect_identical(phase13_cache_get(cache, "key"), list(value = 1L))
    testthat::expect_null(phase13_cache_get(cache, "different"))
    testthat::expect_true(isTRUE(bie_freeze_status()$frozen))
  }
)
