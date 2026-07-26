# ============================================================
# Thompson Basketball Intelligence
# Database Functions
# ============================================================

# ------------------------------------------------------------
# Connect to database
# ------------------------------------------------------------

connect_db <- function() {
  
  db_path <- file.path(
    "inst",
    "database",
    "tbi.sqlite"
  )
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    db_path
  )
  
  DBI::dbExecute(
    con,
    "PRAGMA foreign_keys = ON;"
  )
  
  con
}


disconnect_db <- function(con) {
  
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  
  invisible(NULL)
}

# ------------------------------------------------------------
# Get all NBA teams
# ------------------------------------------------------------

get_teams <- function() {
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  teams <- DBI::dbReadTable(
    con,
    "teams"
  )
  
  teams <- teams[
    order(teams$team_name),
    ,
    drop = FALSE
  ]
  
  rownames(teams) <- NULL
  
  teams
}
# ------------------------------------------------------------
# Get one NBA team
# ------------------------------------------------------------

get_team <- function(team_value) {
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  team <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name,
      abbreviation,
      conference,
      division,
      is_active
    FROM teams
    WHERE team_id = ?
       OR abbreviation = ?
       OR team_name = ?
    LIMIT 1
    ",
    params = list(
      team_value,
      team_value,
      team_value
    )
  )
  
  team
}
# ------------------------------------------------------------
# Get current roster for a team
# ------------------------------------------------------------

get_roster <- function(team_value, season = NULL) {
  
  con <- connect_db()
  
  on.exit(disconnect_db(con), add = TRUE)
  
  if (is.null(season)) {
    
    season_query <- ""
    params <- list(team_value, team_value, team_value)
    
  } else {
    
    season_query <- "AND rh.season = ?"
    params <- list(team_value, team_value, team_value, season)
    
  }
  
  sql <- paste0(
    "
    SELECT

      p.player_id,
      p.player_name,
      p.primary_position,
      p.height_inches,
      p.weight_lbs,

      rh.season,
      rh.roster_status,
      rh.jersey_number,
      rh.two_way_flag

    FROM roster_history rh

    JOIN players p
      ON rh.player_id = p.player_id

    JOIN teams t
      ON rh.team_id = t.team_id

    WHERE

      (
        t.team_name = ?
        OR t.abbreviation = ?
        OR t.team_id = ?
      )

    ",
    season_query,
    "

    ORDER BY
      p.player_name
    "
  )
  
  DBI::dbGetQuery(
    con,
    sql,
    params = params
  )
  
}