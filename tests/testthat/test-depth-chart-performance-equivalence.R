# ============================================================
# Depth Chart loading performance characterization/equivalence
# ============================================================

tbi_depth_chart_equivalence_teams <- c(
  "Boston Celtics",
  "Denver Nuggets",
  "Brooklyn Nets",
  "Sacramento Kings",
  "Memphis Grizzlies",
  "Utah Jazz",
  "Washington Wizards"
)


testthat::test_that(
  "batched Depth Chart enrichment matches the frozen BIE compatibility path",
  {
    tbi_test_draft_with_read_only_db(function() {
      for (team in tbi_depth_chart_equivalence_teams) {
        roster <- tbi_test_depth_chart_roster(team)
        expected <- tbi_test_depth_chart_enrichment_reference(roster)
        actual <- depth_chart_batched_bie_enrich_roster(roster)

        testthat::expect_identical(
          actual,
          expected,
          info = team
        )
      }
    })
  }
)


testthat::test_that(
  "batched official positions match every per-player reference result",
  {
    tbi_test_draft_with_read_only_db(function() {
      for (team in tbi_depth_chart_equivalence_teams) {
        roster <- tbi_test_depth_chart_roster(team)
        actual <- depth_chart_batched_official_positions(roster)

        for (i in seq_len(nrow(roster))) {
          player_id <- as.integer(roster$player_id[[i]])
          expected <- get_player_eligible_positions(
            player_id = player_id,
            primary_position = roster$primary_position[[i]]
          )

          testthat::expect_identical(
            actual[[as.character(player_id)]],
            expected,
            info = paste(team, roster$player_name[[i]])
          )
        }
      }
    })
  }
)


testthat::test_that(
  "batched official positions use one roster query and no schema reads",
  {
    tbi_test_draft_with_read_only_db(function() {
      roster <- tbi_test_depth_chart_roster("Boston Celtics")
      measured <- tbi_test_count_draft_db_calls(function() {
        depth_chart_batched_official_positions(roster)
      })

      testthat::expect_identical(
        measured$counts,
        c(
          queries = 1L,
          executes = 2L,
          table_lists = 0L,
          field_lists = 0L
        )
      )
    })
  }
)


testthat::test_that(
  "compact optimizer input rehydrates the exact frozen BIE result",
  {
    tbi_test_draft_with_read_only_db(function() {
      for (team in tbi_depth_chart_equivalence_teams) {
        roster <- tbi_test_depth_chart_roster(team)
        enriched <- tbi_test_depth_chart_enrichment_reference(roster)
        players <- evaluate_bie_players(enriched)

        expected <- tbi_test_depth_chart_optimizer_reference(players)
        actual <- depth_chart_optimize_bie_starting_five(players)

        testthat::expect_identical(
          actual,
          expected,
          info = team
        )
      }
    })
  }
)


testthat::test_that(
  "Depth Chart performance helpers preserve empty inputs",
  {
    empty <- data.frame()

    testthat::expect_identical(
      depth_chart_batched_bie_enrich_roster(empty),
      tbi_test_depth_chart_enrichment_reference(empty)
    )
    testthat::expect_identical(
      depth_chart_optimize_bie_starting_five(empty),
      tbi_test_depth_chart_optimizer_reference(empty)
    )
  }
)


testthat::test_that(
  "batched enrichment removes the Boston per-player query pattern",
  {
    tbi_test_draft_with_read_only_db(function() {
      roster <- tbi_test_depth_chart_roster("Boston Celtics")

      reference <- tbi_test_count_draft_db_calls(function() {
        tbi_test_depth_chart_enrichment_reference(roster)
      })
      optimized <- tbi_test_count_draft_db_calls(function() {
        depth_chart_batched_bie_enrich_roster(roster)
      })

      testthat::expect_identical(
        reference$counts,
        c(
          queries = 144L,
          executes = 2L,
          table_lists = 144L,
          field_lists = 144L
        )
      )
      testthat::expect_identical(
        optimized$counts,
        c(
          queries = 8L,
          executes = 2L,
          table_lists = 1L,
          field_lists = 7L
        )
      )
    })
  }
)


testthat::test_that(
  "compact optimization preserves locks, scores, classes, and NA placement",
  {
    tbi_test_draft_with_read_only_db(function() {
      for (team in c(
        "Denver Nuggets",
        "Brooklyn Nets",
        "Sacramento Kings",
        "Utah Jazz",
        "Washington Wizards"
      )) {
        roster <- tbi_test_depth_chart_roster(team)
        players <- evaluate_bie_players(
          tbi_test_depth_chart_enrichment_reference(roster)
        )
        expected <- tbi_test_depth_chart_optimizer_reference(players)
        actual <- depth_chart_optimize_bie_starting_five(players)

        testthat::expect_identical(names(actual), names(expected))
        testthat::expect_identical(actual$lineup, expected$lineup)
        testthat::expect_identical(
          vapply(actual$lineup, typeof, character(1)),
          vapply(expected$lineup, typeof, character(1))
        )
        testthat::expect_identical(
          is.na(actual$lineup),
          is.na(expected$lineup)
        )
        testthat::expect_identical(actual$score, expected$score)
        testthat::expect_identical(
          actual$evaluated_lineups,
          expected$evaluated_lineups
        )
      }
    })
  }
)
