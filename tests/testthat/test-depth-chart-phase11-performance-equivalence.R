# ============================================================
# Depth Chart Phase 11 prepared-candidate equivalence
# ============================================================

depth_chart_test_approved_lineup <- function(roster) {
  positions <- c("PG", "SG", "SF", "PF", "C")
  stats::setNames(
    vapply(
      positions,
      function(position) {
        rows <- roster[
          roster$position == position & roster$is_starter == 1L,
          ,
          drop = FALSE
        ]
        if (nrow(rows)) as.integer(rows$player_id[[1]]) else NA_integer_
      },
      integer(1)
    ),
    positions
  )
}


depth_chart_test_phase11_rotation <- function(
    team,
    rotation_size = 9L) {
  roster <- tbi_test_depth_chart_roster(team)
  players <- evaluate_bie_players(
    depth_chart_batched_bie_enrich_roster(roster)
  )
  build_phase11_rotation_result(
    players = players,
    rotation_size = rotation_size,
    total_minutes = 240L,
    approved_lineup = depth_chart_test_approved_lineup(roster)
  )
}


testthat::test_that(
  "prepared Phase 11 candidates preserve every lineup result exactly",
  {
    tbi_test_draft_with_read_only_db(function() {
      for (team in c(
        "Boston Celtics",
        "Denver Nuggets",
        "Memphis Grizzlies",
        "Brooklyn Nets",
        "Sacramento Kings",
        "Utah Jazz",
        "Washington Wizards"
      )) {
        for (rotation_size in c(8L, 9L, 10L)) {
          rotation <- depth_chart_test_phase11_rotation(
            team,
            rotation_size
          )
          expected <- build_phase11_lineup_result(
            rotation,
            pool_size = rotation$rotation_size
          )
          actual <- depth_chart_build_phase11_lineup_result(
            rotation,
            pool_size = rotation$rotation_size
          )

          testthat::expect_identical(
            actual,
            expected,
            info = paste(team, rotation_size)
          )
        }
      }
    })
  }
)


testthat::test_that(
  "prepared Phase 11 candidates reuse the immutable pool and rookie gate",
  {
    tbi_test_draft_with_read_only_db(function() {
      rotation <- depth_chart_test_phase11_rotation("Boston Celtics", 9L)
      reference <- tbi_test_count_draft_db_calls(function() {
        build_phase11_lineup_result(
          rotation,
          pool_size = rotation$rotation_size
        )
      })
      optimized <- tbi_test_count_draft_db_calls(function() {
        depth_chart_build_phase11_lineup_result(
          rotation,
          pool_size = rotation$rotation_size
        )
      })

      testthat::expect_identical(
        reference$counts,
        c(
          queries = 4L,
          executes = 8L,
          table_lists = 4L,
          field_lists = 4L
        )
      )
      testthat::expect_identical(
        optimized$counts,
        c(
          queries = 1L,
          executes = 2L,
          table_lists = 1L,
          field_lists = 1L
        )
      )
    })
  }
)
