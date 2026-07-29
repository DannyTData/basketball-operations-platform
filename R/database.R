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
# ------------------------------------------------------------
# Resolve the packaged SQLite database path
# ------------------------------------------------------------
resolve_tbi_db_path <- function(db_path = file.path("inst", "database", "tbi.sqlite")) {
  if (!is.null(db_path) && nzchar(db_path) && file.exists(db_path)) {
    return(normalizePath(db_path, winslash = "/", mustWork = TRUE))
  }

  golem_path <- tryCatch(
    app_sys("database/tbi.sqlite"),
    error = function(e) ""
  )

  if (nzchar(golem_path) && file.exists(golem_path)) {
    return(golem_path)
  }

  installed_path <- system.file(
    "database",
    "tbi.sqlite",
    package = utils::packageName()
  )

  if (nzchar(installed_path) && file.exists(installed_path)) {
    return(installed_path)
  }

  stop("TBI database not found. Expected inst/database/tbi.sqlite.", call. = FALSE)
}

# ------------------------------------------------------------
# Normalize a roster/depth-chart position to the five NBA slots
# ------------------------------------------------------------
normalize_depth_position <- function(position) {
  position <- toupper(trimws(ifelse(is.na(position), "", as.character(position))))

  if (grepl("PG", position, fixed = TRUE) || identical(position, "G")) return("PG")
  if (grepl("SG", position, fixed = TRUE)) return("SG")
  if (grepl("SF", position, fixed = TRUE) || identical(position, "F")) return("SF")
  if (grepl("PF", position, fixed = TRUE)) return("PF")
  if (identical(position, "C") || grepl("CENTER", position, fixed = TRUE)) return("C")

  "OTHER"
}

# ------------------------------------------------------------
# Create support tables used by the application
# ------------------------------------------------------------
ensure_tbi_support_tables <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS depth_chart_overrides (
      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,
      position TEXT NOT NULL,
      depth_order INTEGER NOT NULL DEFAULT 1,
      is_starter INTEGER NOT NULL DEFAULT 0,
      notes TEXT,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (player_id, team_id, season),
      FOREIGN KEY(player_id) REFERENCES players(player_id),
      FOREIGN KEY(team_id) REFERENCES teams(team_id)
    )
    "
  )
  invisible(TRUE)
}

# ------------------------------------------------------------
# Shared source of truth for both depth-chart displays
# ------------------------------------------------------------
get_depth_chart_records <- function(team_value, season, db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(disconnect_db(con), add = TRUE)
  ensure_tbi_support_tables(con)

  sql <- "
    WITH contract_data AS (
      SELECT
        cy.player_id,
        cy.team_id,
        cy.season,
        MAX(COALESCE(cy.cap_hit, cy.base_salary, 0)) AS salary,
        GROUP_CONCAT(
          DISTINCT COALESCE(
            NULLIF(TRIM(c.contract_type), ''),
            NULLIF(TRIM(cy.option_type), ''),
            'Standard'
          )
        ) AS contract_type
      FROM contract_years cy
      LEFT JOIN contracts c ON cy.contract_id = c.contract_id
      GROUP BY cy.player_id, cy.team_id, cy.season
    )
    SELECT
      dc.depth_chart_id,
      dc.player_id,
      dc.team_id,
      dc.season,
      COALESCE(dco.position, dc.position) AS position,
      COALESCE(dco.depth_order, dc.depth_order) AS depth_order,
      COALESCE(dco.is_starter, dc.is_starter) AS is_starter,
      CASE WHEN dco.player_id IS NULL THEN 0 ELSE 1 END AS has_override,
      dco.notes AS override_notes,
      p.player_name,
      p.primary_position,
      p.height_inches,
      p.weight_lbs,
      p.player_age,
      cd.salary,
      COALESCE(cd.contract_type, 'Not loaded') AS contract_type,
      COALESCE(rh.roster_status, 'Active') AS roster_status,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag
    FROM depth_chart dc
    INNER JOIN players p ON dc.player_id = p.player_id
    INNER JOIN teams t ON dc.team_id = t.team_id
    LEFT JOIN depth_chart_overrides dco
      ON dc.player_id = dco.player_id
      AND dc.team_id = dco.team_id
      AND dc.season = dco.season
    LEFT JOIN contract_data cd
      ON dc.player_id = cd.player_id
      AND dc.team_id = cd.team_id
      AND dc.season = cd.season
    LEFT JOIN roster_history rh
      ON dc.player_id = rh.player_id
      AND dc.team_id = rh.team_id
      AND dc.season = rh.season
    WHERE (t.team_name = ? OR t.abbreviation = ? OR CAST(t.team_id AS TEXT) = CAST(? AS TEXT))
      AND dc.season = ?
  "

  result <- DBI::dbGetQuery(
    con,
    sql,
    params = list(team_value, team_value, team_value, season)
  )

  if (nrow(result) == 0) return(result)

  result$position <- vapply(result$position, normalize_depth_position, character(1))
  result$is_starter <- as.integer(result$is_starter)
  result$depth_order <- as.integer(result$depth_order)
  result$has_override <- as.integer(result$has_override)
  result$salary[is.na(result$salary)] <- 0

  positions <- c("PG", "SG", "SF", "PF", "C", "OTHER")
  ranked <- lapply(positions, function(pos) {
    rows <- result[result$position == pos, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)

    manual_starter <- rows$has_override == 1L & rows$is_starter == 1L
    order_index <- order(
      -as.integer(manual_starter),
      -rows$is_starter,
      ifelse(rows$has_override == 1L, rows$depth_order, 999L),
      rows$depth_order,
      -rows$salary,
      rows$player_name
    )
    rows <- rows[order_index, , drop = FALSE]

    # Exactly one starter per position. A manual starter takes priority;
    # otherwise the existing first-ranked player remains the starter.
    rows$depth_order <- seq_len(nrow(rows))
    rows$is_starter <- 0L
    rows$is_starter[[1]] <- 1L
    rows
  })

  result <- do.call(rbind, ranked[!vapply(ranked, is.null, logical(1))])
  rownames(result) <- NULL
  result
}



# ------------------------------------------------------------
# Eligible positions for editable depth charts
# ------------------------------------------------------------
get_player_eligible_positions <- function(player_id, primary_position = NULL, db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(disconnect_db(con), add = TRUE)
  ensure_tbi_support_tables(con)

  explicit <- DBI::dbGetQuery(
    con,
    "SELECT position FROM player_positions WHERE player_id = ? ORDER BY eligibility_rank, position",
    params = list(as.integer(player_id))
  )
  eligible <- unique(vapply(explicit$position, normalize_depth_position, character(1)))
  eligible <- eligible[eligible %in% c("PG", "SG", "SF", "PF", "C")]

  if (!length(eligible)) {
    position <- toupper(trimws(as.character(primary_position %||% "")))
    tokens <- unlist(strsplit(gsub("[^A-Z/\\-]", "", position), "[/\\-]"))
    tokens <- unique(vapply(tokens[nzchar(tokens)], normalize_depth_position, character(1)))
    tokens <- tokens[tokens %in% c("PG", "SG", "SF", "PF", "C")]

    # Flexible fallback for generic guard/forward labels.
    if (grepl("G", position, fixed = TRUE) && !length(tokens)) tokens <- c("PG", "SG")
    if (grepl("F", position, fixed = TRUE) && !length(tokens)) tokens <- c("SF", "PF")
    eligible <- tokens
  }

  if (!length(eligible)) eligible <- normalize_depth_position(primary_position)
  eligible <- eligible[eligible %in% c("PG", "SG", "SF", "PF", "C")]
  if (!length(eligible)) eligible <- c("PG", "SG", "SF", "PF", "C")
  unique(eligible)
}

save_depth_chart_override <- function(player_id, team_value, season, position,
                                      depth_order = 1L, is_starter = FALSE,
                                      db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(disconnect_db(con), add = TRUE)
  ensure_tbi_support_tables(con)

  team <- DBI::dbGetQuery(
    con,
    "SELECT team_id FROM teams WHERE team_name = ? OR abbreviation = ? OR CAST(team_id AS TEXT) = CAST(? AS TEXT) LIMIT 1",
    params = list(team_value, team_value, team_value)
  )
  if (!nrow(team)) stop("Selected team was not found.", call. = FALSE)

  position <- normalize_depth_position(position)
  if (!position %in% c("PG", "SG", "SF", "PF", "C")) {
    stop("Choose an eligible basketball position.", call. = FALSE)
  }

  DBI::dbExecute(
    con,
    "INSERT INTO depth_chart_overrides
       (player_id, team_id, season, position, depth_order, is_starter, notes, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, 'User depth-chart edit', CURRENT_TIMESTAMP)
     ON CONFLICT(player_id, team_id, season) DO UPDATE SET
       position = excluded.position,
       depth_order = excluded.depth_order,
       is_starter = excluded.is_starter,
       notes = excluded.notes,
       updated_at = CURRENT_TIMESTAMP",
    params = list(
      as.integer(player_id), as.integer(team$team_id[[1]]), season, position,
      max(1L, as.integer(depth_order)), as.integer(isTRUE(is_starter))
    )
  )
  invisible(TRUE)
}

reset_depth_chart_override <- function(player_id, team_value, season, db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(disconnect_db(con), add = TRUE)
  ensure_tbi_support_tables(con)
  DBI::dbExecute(
    con,
    "DELETE FROM depth_chart_overrides
     WHERE player_id = ?
       AND season = ?
       AND team_id IN (
         SELECT team_id FROM teams
         WHERE team_name = ? OR abbreviation = ? OR CAST(team_id AS TEXT) = CAST(? AS TEXT)
       )",
    params = list(as.integer(player_id), season, team_value, team_value, team_value)
  )
  invisible(TRUE)
}

get_cap_thresholds <- function(season, db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(disconnect_db(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT season, salary_cap, luxury_tax, first_apron, second_apron, minimum_team_salary
     FROM cap_thresholds WHERE season = ? LIMIT 1",
    params = list(season)
  )
}

# Local null-coalescing helper used by database functions.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
