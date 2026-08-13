# ============================================================
# Phase 12 Tests: Scenario Comparison UI Integration
# ============================================================

phase12_test_roster <- function() {
  data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    position = c(
      "PG","SG","SF","PF","C",
      "PG","SG","SF","PF","C"
    ),
    depth_order = c(
      rep(1,5),
      rep(2,5)
    ),
    is_starter = c(
      rep(TRUE,5),
      rep(FALSE,5)
    ),
    availability_status =
      rep("AVAILABLE",10),
    bie_rating = c(
      90,86,84,82,80,
      76,74,72,70,68
    ),
    projected_bie_rating = c(
      91,87,85,83,81,
      77,75,73,71,69
    ),
    impact_score = c(
      90,85,84,82,81,
      76,74,72,70,68
    ),
    offensive_impact = c(
      94,90,84,82,74,
      82,80,70,68,64
    ),
    defensive_impact = c(
      76,72,90,84,94,
      70,68,88,80,86
    ),
    creation_score = c(
      96,84,74,62,40,
      88,66,50,56,34
    ),
    spacing_score = c(
      88,94,84,90,42,
      76,92,72,78,36
    ),
    rebounding_score = c(
      42,40,72,80,96,
      38,35,68,76,92
    ),
    primary_role = c(
      "Creator","Scorer","Two-Way",
      "Connector","Rim",
      "Creator","Spacer","Defender",
      "Connector","Rebounder"
    ),
    archetype = "",
    impact_tier = c(
      rep("Starter",5),
      rep("Rotation",5)
    ),
    stringsAsFactors = FALSE
  )
}


testthat::test_that(
  "change text adds positive sign",
  {
    testthat::expect_equal(
      phase12_change_text(2.4),
      "+2.4"
    )
    
    testthat::expect_equal(
      phase12_change_text(-1.2),
      "-1.2"
    )
  }
)


testthat::test_that(
  "recommendation classes resolve",
  {
    testthat::expect_equal(
      phase12_recommendation_class(
        "FAVOR SCENARIO"
      ),
      "positive"
    )
    
    testthat::expect_equal(
      phase12_recommendation_class(
        "FAVOR BASE"
      ),
      "negative"
    )
    
    testthat::expect_equal(
      phase12_recommendation_class(
        "NEUTRAL / CLOSE CALL"
      ),
      "neutral"
    )
  }
)


testthat::test_that(
  "Phase 10 result can render Phase 12 panel",
  {
    base <- phase12_test_roster()
    scenario <- base
    
    scenario$bie_rating[[7]] <- 96
    scenario$projected_bie_rating[[7]] <- 97
    scenario$impact_score[[7]] <- 96
    scenario$offensive_impact[[7]] <- 100
    scenario$spacing_score[[7]] <- 100
    
    result <- build_scenario_comparison(
      base_roster = base,
      scenario_roster = scenario,
      scenario_name = "Test Trade",
      rotation_size = 9
    )
    
    panel <- build_phase12_scenario_panel(
      result,
      scenario = list(
        partner_team = "Test Partner"
      )
    )
    
    testthat::expect_s3_class(
      panel,
      "shiny.tag"
    )
  }
)