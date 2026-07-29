library(DBI)
library(RSQLite)

setup_rotation_optimizer <- function() {
  
  db_path <- normalizePath(
    "inst/database/tbi.sqlite",
    mustWork = TRUE
  )
  
  message("Using database: ", db_path)
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    db_path
  )
  
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  
  if (!DBI::dbIsValid(con)) {
    stop("Could not establish a valid SQLite connection.")
  }
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS player_positions (
        player_id INTEGER NOT NULL,
        position TEXT NOT NULL,
        eligibility_rank INTEGER NOT NULL DEFAULT 1,
        is_primary INTEGER NOT NULL DEFAULT 0,

        PRIMARY KEY (player_id, position),

        FOREIGN KEY (player_id)
            REFERENCES players(player_id)
    )
    "
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS player_season_stats (
        player_id INTEGER NOT NULL,
        team_id INTEGER NOT NULL,
        season TEXT NOT NULL,

        games_played INTEGER,
        games_started INTEGER,
        minutes_per_game REAL,
        points_per_game REAL,
        rebounds_per_game REAL,
        assists_per_game REAL,
        true_shooting_pct REAL,
        usage_pct REAL,
        bpm REAL,
        win_shares REAL,
        availability_pct REAL,

        PRIMARY KEY (
            player_id,
            team_id,
            season
        ),

        FOREIGN KEY (player_id)
            REFERENCES players(player_id),

        FOREIGN KEY (team_id)
            REFERENCES teams(team_id)
    )
    "
  )
  
  message("Rotation optimizer tables created successfully.")
}

setup_rotation_optimizer()