# ============================================================
# Phase 11 Tests: Roster Intelligence Integration
# ============================================================

create_phase11_test_roster <- function() {
  data.frame(
    player_id = 1:10,
    player_name = paste(
      "Player",
      1:10
    ),
    position = c(
      "PG", "SG", "SF", "PF", "C",
      "PG", "SG", "SF", "PF", "C"
    ),
    depth_order = c(
      rep(1, 5),
      rep(2, 5)
    ),
    is_starter = c(
      rep(TRUE, 5),
      rep(FALSE, 5)
    ),
    availability_status =
      rep("AVAILABLE", 10),
    bie_rating = c(
      90, 86, 84, 82, 80,
      76, 74, 72, 70, 68
    ),
    projected_bie_rating = c(
      91, 87, 85, 83, 81,
      77, 75, 73, 71, 69
    ),
    impact_score = c(
      90, 85, 84, 82, 81,
      76, 74, 72, 70, 68
    ),
    offensive_impact = c(
      94, 90, 84, 82, 74,
      82, 80, 70, 68, 64
    ),
    defensive_impact = c(
      76, 72, 90, 84, 94,
      70, 68, 88, 80, 86
    ),
    creation_score = c(
      96, 84, 74, 62, 40,
      88, 66, 50, 56, 34
    ),
    spacing_score = c(
      88, 94, 84, 90, 42,
      76, 92, 72, 78, 36
    ),
    rebounding_score = c(
      42, 40, 72, 80, 96,
      38, 35, 68, 76, 92
    ),
    primary_role = c(
      "Creator", "Scorer", "Two-Way",
      "Connector", "Rim",
      "Creator", "Spacer", "Defender",
      "Connector", "Rebounder"
    ),
    archetype = "",
    impact_tier = c(
      rep("Starter", 5),
      rep("Rotation", 5)
    ),
    stringsAsFactors = FALSE
  )
}


testthat::test_that(
  "approved lineup becomes starter source",
  {
    roster <- create_phase11_test_roster()
    
    approved <- c(
      PG = 6,
      SG = 2,
      SF = 3,
      PF = 4,
      C = 5
    )
    
    result <-
      phase11_apply_approved_lineup(
        roster,
        approved
      )
    
    testthat::expect_true(
      result$is_starter[
        result$player_id == 6
      ]
    )
    
    testthat::expect_false(
      result$is_starter[
        result$player_id == 1
      ]
    )
  }
)


testthat::test_that(
  "Phase 11 rotation adapter totals 240",
  {
    roster <- create_phase11_test_roster()
    
    approved <- c(
      PG = 1,
      SG = 2,
      SF = 3,
      PF = 4,
      C = 5
    )
    
    result <-
      build_phase11_rotation_result(
        roster,
        rotation_size = 9,
        approved_lineup = approved
      )
    
    testthat::expect_equal(
      result$status,
      "OK"
    )
    
    testthat::expect_equal(
      result$total_minutes,
      240
    )
    
    testthat::expect_equal(
      result$rotation_size,
      9
    )
    
    testthat::expect_true(
      all(
        c(
          "bie_rotation_role",
          "bie_rotation_slot",
          "bie_recommended_minutes"
        ) %in%
          names(result$rotation)
      )
    )
  }
)


testthat::test_that(
  "Phase 9 optimization builds from Phase 8 adapter",
  {
    roster <- create_phase11_test_roster()
    
    rotation <-
      build_phase11_rotation_result(
        roster,
        rotation_size = 9
      )
    
    lineups <-
      build_phase11_lineup_result(
        rotation
      )
    
    testthat::expect_true(
      is.list(lineups)
    )
    
    testthat::expect_equal(
      length(
        lineups$closing$players
      ),
      5
    )
  }
)