library(hoopR)
library(dplyr)
library(readr)
library(DBI)
library(duckdb)
library(janitor)

season_value <- "2025-26"
database_path <- "data/basketball_ops.duckdb"

message("Requesting NBA team data...")

# Get team stats from the NBA API
api_result <- hoopR::nba_leaguedashteamstats(
  season = season_value,
  season_type = "Regular Season",
  per_mode = "PerGame"
)

# Handle different hoopR return formats
if (is.data.frame(api_result)) {
  team_stats <- api_result
} else {
  data_frame_locations <- which(
    vapply(api_result, is.data.frame, logical(1))
  )
  
  if (length(data_frame_locations) == 0) {
    stop("The NBA request returned no data frame.")
  }
  
  team_stats <- api_result[[data_frame_locations[1]]]
}

team_stats <- team_stats |>
  janitor::clean_names() |>
  dplyr::mutate(
    team_name = dplyr::recode(
      team_name,
      "LA Clippers" = "Los Angeles Clippers"
    )
  )

# Load master team table
teams_master <- readr::read_csv(
  "data/teams.csv",
  show_col_types = FALSE
)

# Build standings table
standings <- team_stats |>
  transmute(
    nba_team_id = as.character(team_id),
    team_name,
    games_played = gp,
    wins = w,
    losses = l,
    win_pct = w_pct,
    points_per_game = pts,
    point_diff = plus_minus
  ) |>
  left_join(
    teams_master,
    by = "team_name"
  ) |>
  group_by(conference) |>
  mutate(
    conference_rank = min_rank(desc(win_pct))
  ) |>
  ungroup() |>
  group_by(division) |>
  mutate(
    division_rank = min_rank(desc(win_pct))
  ) |>
  ungroup() |>
  mutate(
    season = season_value,
    updated_at = Sys.time()
  ) |>
  select(
    season,
    team_id,
    nba_team_id,
    team_name,
    abbreviation,
    conference,
    division,
    games_played,
    wins,
    losses,
    win_pct,
    conference_rank,
    division_rank,
    points_per_game,
    point_diff,
    updated_at
  ) |>
  arrange(conference, conference_rank)

# Check that all 30 teams matched
if (any(is.na(standings$team_id))) {
  stop("One or more teams did not match the master table.")
}

# Connect to DuckDB
con <- dbConnect(
  duckdb(),
  dbdir = database_path
)

# Write standings table
dbWriteTable(
  con,
  "standings",
  standings,
  overwrite = TRUE
)

# Verify
print(
  dbGetQuery(
    con,
    "
    SELECT
      team_name,
      wins,
      losses,
      conference_rank
    FROM standings
    ORDER BY conference, conference_rank
    "
  )
)

dbDisconnect(
  con,
  shutdown = TRUE
)

message("Standings table updated successfully!")