# ============================================================
# Thompson Basketball Intelligence
# Database Functions
# ============================================================

#' Resolve the authoritative TBI SQLite database
#'
#' The packaged database at inst/database/tbi.sqlite is the single source of
#' truth in development. Installed applications resolve the same file through
#' the package installation directory.
#'
#' @param db_path Optional explicit path, primarily for tests and maintenance.
#' @return A normalized database path.
#' @noRd
resolve_tbi_db_path <- function(db_path = NULL) {
  domain <- tryCatch(
    shiny::getDefaultReactiveDomain(),
    error = function(e) NULL
  )
  feedback_session <- !is.null(domain) &&
    !is.null(domain$userData) &&
    isTRUE(domain$userData$tbi_feedback_mode)

  if (feedback_session) {
    feedback_path <- as.character(domain$userData$tbi_feedback_db_path %||% "")
    if (
      length(feedback_path) != 1L ||
      is.na(feedback_path) ||
      !nzchar(trimws(feedback_path)) ||
      !file.exists(feedback_path)
    ) {
      stop(
        "Feedback database isolation is active, but this session has no valid disposable database.",
        call. = FALSE
      )
    }
    return(normalizePath(feedback_path, winslash = "/", mustWork = TRUE))
  }

  feedback_process <- tolower(trimws(Sys.getenv("TBI_FEEDBACK_MODE", "false"))) %in%
    c("1", "true", "yes", "on")
  if (feedback_process) {
    stop(
      "Feedback database access requires an active isolated Shiny session; authoritative fallback is disabled.",
      call. = FALSE
    )
  }

  # TBI_PHASE15J_WEB_DB_OVERRIDE
  demo_mode <- tolower(trimws(Sys.getenv("TBI_DEMO_MODE", "false"))) %in% c("1", "true", "yes", "on")
  demo_override <- trimws(Sys.getenv("TBI_DB_OVERRIDE", ""))

  # Isolated tests may supply their own unique database beneath tempdir().
  # The demo override still dominates repository, installed, or other paths.
  if (
    isTRUE(demo_mode) &&
    !is.null(db_path) &&
    length(db_path) == 1L &&
    !is.na(db_path) &&
    file.exists(db_path)
  ) {
    explicit_path <- normalizePath(db_path, winslash = "/", mustWork = TRUE)
    temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    if (startsWith(tolower(explicit_path), paste0(tolower(temp_root), "/"))) {
      return(explicit_path)
    }
  }

  if (
    isTRUE(demo_mode) &&
    nzchar(demo_override) &&
    file.exists(demo_override)
  ) {
    return(
      normalizePath(
        demo_override,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  candidates <- character()
  
  if (!is.null(db_path) && length(db_path) == 1L && nzchar(db_path)) {
    candidates <- c(candidates, db_path)
  }
  
  candidates <- c(
    candidates,
    file.path("inst", "database", "tbi.sqlite")
  )
  
  if (exists("app_sys", mode = "function")) {
    golem_path <- tryCatch(app_sys("database", "tbi.sqlite"), error = function(e) "")
    if (nzchar(golem_path)) candidates <- c(candidates, golem_path)
  }
  
  package_name <- tryCatch(utils::packageName(), error = function(e) "")
  if (!is.null(package_name) && nzchar(package_name)) {
    installed_path <- system.file("database", "tbi.sqlite", package = package_name)
    if (nzchar(installed_path)) candidates <- c(candidates, installed_path)
  }
  
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]
  
  if (!length(existing)) {
    stop(
      paste0(
        "TBI database not found. Expected the authoritative database at ",
        "inst/database/tbi.sqlite."
      ),
      call. = FALSE
    )
  }
  
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

#' Resolve the feedback source database without fallback from an explicit path.
#' @noRd
tbi_resolve_feedback_source_db <- function(source_db = NULL) {
  if (!is.null(source_db)) {
    source_db <- as.character(source_db)
    if (
      length(source_db) != 1L ||
      is.na(source_db) ||
      !nzchar(trimws(source_db)) ||
      !file.exists(source_db)
    ) {
      stop("The explicit feedback source database does not exist.", call. = FALSE)
    }
    return(normalizePath(source_db, winslash = "/", mustWork = TRUE))
  }

  candidates <- file.path("inst", "database", "tbi.sqlite")
  if (exists("app_sys", mode = "function")) {
    installed <- tryCatch(app_sys("database", "tbi.sqlite"), error = function(e) "")
    if (nzchar(installed)) candidates <- c(candidates, installed)
  }
  package_name <- tryCatch(utils::packageName(), error = function(e) "")
  if (!is.null(package_name) && nzchar(package_name)) {
    installed <- system.file("database", "tbi.sqlite", package = package_name)
    if (nzchar(installed)) candidates <- c(candidates, installed)
  }
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("The feedback source database could not be resolved safely.", call. = FALSE)
  }
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

#' Connect to the TBI SQLite database
#'
#' @param db_path Optional explicit database path.
#' @param read_only Open the database in read-only mode.
#' @return A DBI connection with foreign keys enabled and a busy timeout.
#' @noRd
connect_db <- function(db_path = NULL, read_only = FALSE) {
  path <- resolve_tbi_db_path(db_path)
  
  flags <- if (isTRUE(read_only)) {
    RSQLite::SQLITE_RO
  } else {
    RSQLite::SQLITE_RWC
  }
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = path,
    flags = flags
  )
  
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  
  con
}

#' Disconnect safely from the TBI database
#'
#' @param con A DBI connection.
#' @return Invisibly returns NULL.
#' @noRd
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
  
  con <- connect_db(read_only = TRUE)
  
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
  
  con <- connect_db(read_only = TRUE)
  
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
  
  con <- connect_db(read_only = TRUE)
  
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
      is_position_override INTEGER NOT NULL DEFAULT 0,
      position_override_reason TEXT,
      position_override_updated_at TEXT,
      notes TEXT,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (player_id, team_id, season),
      FOREIGN KEY(player_id) REFERENCES players(player_id),
      FOREIGN KEY(team_id) REFERENCES teams(team_id)
    )
    "
  )
  
  existing_columns <- DBI::dbGetQuery(
    con,
    "PRAGMA table_info(depth_chart_overrides)"
  )$name
  
  migrations <- list(
    is_position_override =
      "ALTER TABLE depth_chart_overrides ADD COLUMN is_position_override INTEGER NOT NULL DEFAULT 0",
    position_override_reason =
      "ALTER TABLE depth_chart_overrides ADD COLUMN position_override_reason TEXT",
    position_override_updated_at =
      "ALTER TABLE depth_chart_overrides ADD COLUMN position_override_updated_at TEXT"
  )
  
  for (column_name in names(migrations)) {
    if (!column_name %in% existing_columns) {
      DBI::dbExecute(
        con,
        migrations[[column_name]]
      )
    }
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# Shared source of truth for both depth-chart displays
# ------------------------------------------------------------
get_depth_chart_records <- function(team_value, season, db_path = NULL) {
  con <- connect_db(db_path = db_path, read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)
  
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
      COALESCE(dco.is_position_override, 0) AS is_position_override,
      dco.position_override_reason,
      dco.position_override_updated_at,
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
  result$is_position_override <- as.integer(result$is_position_override)
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
  con <- connect_db(db_path = db_path, read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)
  
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

save_depth_chart_override <- function(
    player_id,
    team_value,
    season,
    position,
    depth_order = 1L,
    is_starter = FALSE,
    allow_position_override = FALSE,
    position_override_reason = NULL,
    db_path = NULL) {
  
  path <- resolve_tbi_db_path(
    db_path %||%
      file.path(
        "inst",
        "database",
        "tbi.sqlite"
      )
  )
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = path
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_tbi_support_tables(con)
  
  team <- DBI::dbGetQuery(
    con,
    "
    SELECT team_id
    FROM teams
    WHERE team_name = ?
       OR abbreviation = ?
       OR CAST(team_id AS TEXT) = CAST(? AS TEXT)
    LIMIT 1
    ",
    params = list(
      team_value,
      team_value,
      team_value
    )
  )
  
  if (!nrow(team)) {
    stop(
      "Selected team was not found.",
      call. = FALSE
    )
  }
  
  position <- normalize_depth_position(
    position
  )
  
  valid_positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  if (!position %in% valid_positions) {
    stop(
      "Choose a valid basketball position.",
      call. = FALSE
    )
  }
  
  player <- DBI::dbGetQuery(
    con,
    "
    SELECT primary_position
    FROM players
    WHERE player_id = ?
    LIMIT 1
    ",
    params = list(
      as.integer(player_id)
    )
  )
  
  if (!nrow(player)) {
    stop(
      "Selected player was not found.",
      call. = FALSE
    )
  }
  
  explicit <- DBI::dbGetQuery(
    con,
    "
    SELECT position
    FROM player_positions
    WHERE player_id = ?
    ORDER BY eligibility_rank, position
    ",
    params = list(
      as.integer(player_id)
    )
  )
  
  eligible <- unique(
    vapply(
      explicit$position,
      normalize_depth_position,
      character(1)
    )
  )
  
  eligible <- eligible[
    eligible %in% valid_positions
  ]
  
  if (!length(eligible)) {
    primary_position <- toupper(
      trimws(
        as.character(
          player$primary_position[[1]] %||%
            ""
        )
      )
    )
    
    tokens <- unlist(
      strsplit(
        gsub(
          "[^A-Z,;/|\\-]",
          "",
          primary_position
        ),
        "[,;/|\\-]+"
      )
    )
    
    tokens <- unique(
      vapply(
        tokens[nzchar(tokens)],
        normalize_depth_position,
        character(1)
      )
    )
    
    eligible <- tokens[
      tokens %in% valid_positions
    ]
  }
  
  if (!length(eligible)) {
    fallback <- normalize_depth_position(
      player$primary_position[[1]]
    )
    
    eligible <- fallback[
      fallback %in% valid_positions
    ]
  }
  
  is_position_override <-
    !position %in% eligible
  
  if (
    is_position_override &&
    !isTRUE(allow_position_override)
  ) {
    stop(
      paste0(
        position,
        " is not listed as an eligible position for this player. ",
        "Enable Lineup Assignment Override to continue."
      ),
      call. = FALSE
    )
  }
  
  reason <- trimws(
    as.character(
      position_override_reason %||%
        ""
    )
  )
  
  if (
    is_position_override &&
    !nzchar(reason)
  ) {
    stop(
      "Enter a reason for the out-of-position lineup assignment.",
      call. = FALSE
    )
  }
  
  if (!is_position_override) {
    reason <- NA_character_
  }
  
  notes <- if (is_position_override) {
    "Lineup assignment override"
  } else {
    "User depth-chart edit"
  }
  
  override_updated_at <- if (is_position_override) {
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  } else {
    NA_character_
  }
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO depth_chart_overrides (
      player_id,
      team_id,
      season,
      position,
      depth_order,
      is_starter,
      is_position_override,
      position_override_reason,
      position_override_updated_at,
      notes,
      updated_at
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ON CONFLICT(player_id, team_id, season)
    DO UPDATE SET
      position = excluded.position,
      depth_order = excluded.depth_order,
      is_starter = excluded.is_starter,
      is_position_override = excluded.is_position_override,
      position_override_reason = excluded.position_override_reason,
      position_override_updated_at =
        excluded.position_override_updated_at,
      notes = excluded.notes,
      updated_at = CURRENT_TIMESTAMP
    ",
    params = list(
      as.integer(player_id),
      as.integer(team$team_id[[1]]),
      season,
      position,
      max(
        1L,
        as.integer(depth_order)
      ),
      as.integer(
        isTRUE(is_starter)
      ),
      as.integer(
        is_position_override
      ),
      reason,
      override_updated_at,
      notes
    )
  )
  
  invisible(
    list(
      saved = TRUE,
      assigned_position = position,
      eligible_positions = eligible,
      is_position_override =
        is_position_override,
      position_override_reason = reason
    )
  )
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
  con <- connect_db(db_path = db_path, read_only = TRUE)
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
