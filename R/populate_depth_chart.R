library(DBI)
library(RSQLite)
library(dplyr)

con <- dbConnect(
  RSQLite::SQLite(),
  "inst/database/tbi.sqlite"
)

current_season <- "2026-27"

# Create the table if it does not already exist
dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS depth_chart (
      depth_chart_id INTEGER PRIMARY KEY AUTOINCREMENT,
      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,
      position TEXT NOT NULL,
      depth_order INTEGER NOT NULL,
      is_starter INTEGER NOT NULL DEFAULT 0,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,

      UNIQUE(player_id, team_id, season),

      FOREIGN KEY(player_id) REFERENCES players(player_id),
      FOREIGN KEY(team_id) REFERENCES teams(team_id)
  );
  "
)

# Pull the current active roster
current_roster <- dbGetQuery(
  con,
  "
  SELECT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      rh.team_id,
      t.team_name,
      rh.season,
      rh.roster_status,
      rh.two_way_flag,
      COALESCE(cy.cap_hit, 0) AS cap_hit
  FROM roster_history rh
  INNER JOIN players p
      ON rh.player_id = p.player_id
  INNER JOIN teams t
      ON rh.team_id = t.team_id
  LEFT JOIN contract_years cy
      ON rh.player_id = cy.player_id
      AND rh.team_id = cy.team_id
      AND rh.season = cy.season
  WHERE rh.season = ?
      AND rh.roster_status IN ('Active', 'Qualifying Offer')
      AND rh.end_date IS NULL
  ",
  params = list(current_season)
)

# Use the first listed position as the player's depth-chart position
depth_chart_seed <- current_roster %>%
  mutate(
    planning_status = as.integer(
      roster_status == "Qualifying Offer"
    ),
    position = trimws(
      sub(
        ",.*$",
        "",
        primary_position
      )
    ),
    position = case_when(
      position %in% c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      ) ~ position,
      TRUE ~ "UTIL"
    )
  ) %>%
  group_by(
    team_id,
    season,
    position
  ) %>%
  arrange(
    planning_status,
    two_way_flag,
    desc(cap_hit),
    player_name,
    .by_group = TRUE
  ) %>%
  mutate(
    depth_order = row_number(),
    is_starter = as.integer(
      depth_order == 1 &
        planning_status == 0
    )
  ) %>%
  ungroup() %>%
  select(
    player_id,
    team_id,
    season,
    position,
    depth_order,
    is_starter
  )

# Clear only the season being rebuilt
dbExecute(
  con,
  "DELETE FROM depth_chart WHERE season = ?",
  params = list(current_season)
)

# Insert the initial depth chart
dbWriteTable(
  con,
  "depth_chart",
  depth_chart_seed,
  append = TRUE,
  row.names = FALSE
)

cat(
  "Depth chart populated for",
  n_distinct(depth_chart_seed$team_id),
  "teams and",
  nrow(depth_chart_seed),
  "players.\n"
)

dbDisconnect(con)