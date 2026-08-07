# ============================================================
# Thompson Basketball Intelligence
# Import Rosters
# ============================================================

source("R/database.R")

con <- connect_db()

cat("Connected to roster database.\n")

player_count <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_players FROM players"
)

roster_count <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_roster_rows FROM roster_history"
)

cat("Players currently loaded:", player_count$total_players, "\n")
cat("Roster rows currently loaded:", roster_count$total_roster_rows, "\n")

disconnect_db(con)