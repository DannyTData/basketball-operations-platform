# ============================================================
# Thompson's Basketball Intelligence
# Depth Chart Database Maintenance
#
# Phase 15J safety repair:
# This file DEFINES a maintenance function only.
# It must never modify SQLite merely because the package loads.
# ============================================================


#' Create the canonical depth-chart table
#'
#' Internal database-maintenance helper.
#' Nothing executes until this function is explicitly called.
#'
#' @param db_path Optional explicit database path.
#' @return Invisibly returns TRUE.
#' @noRd
tbi_create_depth_chart_table <- function(db_path = NULL) {

  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )

  on.exit(
    disconnect_db(con),
    add = TRUE
  )

  DBI::dbExecute(
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

      FOREIGN KEY(player_id)
        REFERENCES players(player_id),

      FOREIGN KEY(team_id)
        REFERENCES teams(team_id)
    );
    "
  )

  invisible(TRUE)
}
