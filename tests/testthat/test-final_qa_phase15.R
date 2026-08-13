# ============================================================
# Phase 15 Tests: Final 30-Team QA + Freeze Readiness
# ============================================================

phase15_synthetic_roster <- function() {
  data.frame(
    player_id = 1:10,
    player_name = paste(
      "QA Player",
      1:10
    ),
    position = rep(
      c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      ),
      2
    ),
    depth_order = c(
      rep(1, 5),
      rep(2, 5)
    ),
    is_starter = c(
      rep(1L, 5),
      rep(0L, 5)
    ),
    availability_status =
      rep(
        "AVAILABLE",
        10
      ),
    bie_rating =
      seq(
        90,
        68,
        length.out = 10
      ),
    projected_bie_rating =
      seq(
        91,
        69,
        length.out = 10
      ),
    impact_score =
      seq(
        90,
        68,
        length.out = 10
      ),
    offensive_impact =
      seq(
        92,
        70,
        length.out = 10
      ),
    defensive_impact =
      seq(
        90,
        68,
        length.out = 10
      ),
    creation_score =
      seq(
        94,
        60,
        length.out = 10
      ),
    spacing_score =
      seq(
        91,
        62,
        length.out = 10
      ),
    rebounding_score =
      seq(
        82,
        64,
        length.out = 10
      ),
    stringsAsFactors = FALSE
  )
}


testthat::test_that(
  "expected lineup profiles are fixed",
  {
    testthat::expect_equal(
      phase15_expected_lineup_types(),
      c(
        "BALANCED",
        "OFFENSE",
        "DEFENSE",
        "CLOSING"
      )
    )
  }
)


testthat::test_that(
  "synthetic Phase 8 and Phase 9 stack satisfies final invariants",
  {
    roster <-
      phase15_synthetic_roster()
    
    minutes <-
      build_minute_allocation(
        roster = roster,
        rotation_size = 10L,
        total_minutes = 240L
      )
    
    testthat::expect_equal(
      sum(
        minutes$allocation$
          recommended_minutes
      ),
      240
    )
    
    lineups <-
      build_lineup_optimization(
        roster_or_allocation =
          minutes,
        pool_size = 10L
      )
    
    types <-
      phase15_lineup_types(
        lineups
      )
    
    testthat::expect_true(
      all(
        phase15_expected_lineup_types() %in%
          types
      )
    )
  }
)


testthat::test_that(
  "freeze manifest refuses failed QA",
  {
    failed <- list(
      summary = list(
        overall_pass = FALSE
      )
    )
    
    testthat::expect_error(
      phase15_freeze_manifest(
        failed
      ),
      "cannot be frozen"
    )
  }
)


testthat::test_that(
  "freeze manifest accepts clean QA",
  {
    clean <- list(
      summary = list(
        overall_pass = TRUE,
        season = "2026-27",
        teams_tested = 30L,
        teams_passed = 30L
      )
    )
    
    manifest <-
      phase15_freeze_manifest(
        clean
      )
    
    testthat::expect_equal(
      manifest$status,
      "FROZEN"
    )
    
    testthat::expect_equal(
      manifest$teams_verified,
      30L
    )
  }
)