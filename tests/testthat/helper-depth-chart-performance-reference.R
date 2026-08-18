# Frozen references for Depth Chart performance equivalence.
# The immutable BIE functions remain the behavior source of truth.

tbi_test_depth_chart_roster <- function(
    team,
    season = "2026-27") {
  get_depth_chart_records(
    team_value = team,
    season = season
  )
}


tbi_test_depth_chart_enrichment_reference <- function(
    roster,
    roster_season = NULL) {
  tbi_bie_enrich_roster(
    roster = roster,
    roster_season = roster_season
  )
}


tbi_test_depth_chart_optimizer_reference <- function(players) {
  optimize_bie_starting_five(players)
}
