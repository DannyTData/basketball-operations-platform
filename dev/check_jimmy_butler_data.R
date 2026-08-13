# ============================================================
# Thompson's Basketball Intelligence
# Jimmy Butler Data / Identity Diagnostic — SAFE CONNECTION
#
# This script opens inst/database/tbi.sqlite directly in read-only
# mode and DOES NOT modify the database.
# ============================================================

required_packages <- c(
  "DBI",
  "RSQLite"
)

missing <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing)) {
  stop(
    paste0(
      "Install required packages first: ",
      paste(missing, collapse = ", ")
    ),
    call. = FALSE
  )
}

db_path <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

if (!file.exists(db_path)) {
  stop(
    paste0(
      "Database not found at ",
      db_path
    ),
    call. = FALSE
  )
}

db_path <- normalizePath(
  db_path,
  winslash = "/",
  mustWork = TRUE
)

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path,
  flags = RSQLite::SQLITE_RO
)

on.exit(
  {
    if (
      !is.null(con) &&
      DBI::dbIsValid(con)
    ) {
      DBI::dbDisconnect(con)
    }
  },
  add = TRUE
)

DBI::dbExecute(
  con,
  "PRAGMA busy_timeout = 5000;"
)

cat("\n")
cat("============================================================\n")
cat("TBI PLAYER DATA DIAGNOSTIC — JIMMY BUTLER\n")
cat("============================================================\n")
cat("Database: ", db_path, "\n\n", sep = "")

tables <- DBI::dbListTables(con)

quote_ident <- function(x) {
  as.character(
    DBI::dbQuoteIdentifier(
      con,
      x
    )
  )
}

fields <- function(table) {
  if (!table %in% tables) {
    return(character())
  }
  
  DBI::dbListFields(
    con,
    table
  )
}

print_df <- function(x) {
  if (
    is.null(x) ||
    !is.data.frame(x) ||
    !nrow(x)
  ) {
    cat("NONE\n")
    return(invisible(NULL))
  }
  
  print(
    x,
    row.names = FALSE
  )
  
  invisible(NULL)
}

query_by_player <- function(table,
                            player_id) {
  if (!table %in% tables) {
    return(
      list(
        status = "TABLE MISSING",
        data = data.frame()
      )
    )
  }
  
  cols <- fields(table)
  
  if (!"player_id" %in% cols) {
    return(
      list(
        status = "NO player_id FIELD",
        data = data.frame()
      )
    )
  }
  
  order_cols <- intersect(
    c(
      "season",
      "latest_season",
      "team_id",
      "latest_team_id"
    ),
    cols
  )
  
  order_sql <- if (length(order_cols)) {
    paste0(
      " ORDER BY ",
      paste(
        vapply(
          order_cols,
          quote_ident,
          character(1)
        ),
        collapse = ", "
      ),
      " DESC"
    )
  } else {
    ""
  }
  
  x <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      quote_ident(table),
      " WHERE player_id = ?",
      order_sql
    ),
    params = list(
      player_id
    )
  )
  
  list(
    status = if (nrow(x)) {
      "FOUND"
    } else {
      "NO ROW"
    },
    data = x
  )
}

compact <- function(x) {
  if (
    is.null(x) ||
    !is.data.frame(x) ||
    !nrow(x)
  ) {
    return(x)
  }
  
  desired <- c(
    "player_id",
    "player_name",
    "team_id",
    "latest_team_id",
    "season",
    "latest_season",
    "source_name",
    "source_player_id",
    "games_played",
    "minutes",
    "official_possessions",
    "estimated_possessions",
    "points_per_game",
    "points_per_100",
    "true_shooting_pct",
    "usage_rate",
    "assist_pct",
    "rebound_pct",
    "offensive_rating",
    "defensive_rating",
    "net_rating",
    "player_impact_estimate",
    "shooting_efficiency_score",
    "spacing_score",
    "creation_score",
    "defense_proxy_score",
    "rebounding_score",
    "primary_role",
    "role_confidence",
    "bie_player_score",
    "impact_score",
    "projected_bie_rating",
    "projected_bie_1y",
    "projected_bie_3y",
    "projection_confidence"
  )
  
  keep <- intersect(
    desired,
    names(x)
  )
  
  if (!length(keep)) {
    keep <- utils::head(
      names(x),
      12
    )
  }
  
  x[, keep, drop = FALSE]
}

# ------------------------------------------------------------
# 1. Canonical player row
# ------------------------------------------------------------

cat("[1] CANONICAL PLAYER RECORD\n")
cat("------------------------------------------------------------\n")

if (!"players" %in% tables) {
  stop(
    "players table is missing.",
    call. = FALSE
  )
}

player_cols <- fields("players")

if (!"player_name" %in% player_cols) {
  stop(
    "players.player_name is missing.",
    call. = FALSE
  )
}

jimmy_candidates <- DBI::dbGetQuery(
  con,
  "
    SELECT *
    FROM players
    WHERE LOWER(player_name) LIKE LOWER(?)
    ORDER BY player_id
  ",
  params = list(
    "%Jimmy%Butler%"
  )
)

print_df(
  jimmy_candidates
)

cat("\n")

if (!nrow(jimmy_candidates)) {
  cat("JIMMY BUTLER DATA STATUS: REVIEW REQUIRED\n")
  stop(
    "Jimmy Butler is not present in players.",
    call. = FALSE
  )
}

selected <- jimmy_candidates[
  1,
  ,
  drop = FALSE
]

if (
  "nba_player_id" %in%
  names(
    jimmy_candidates
  )
) {
  known_hit <- which(
    as.character(
      jimmy_candidates$
        nba_player_id
    ) == "202710"
  )
  
  if (length(known_hit)) {
    selected <- jimmy_candidates[
      known_hit[[1]],
      ,
      drop = FALSE
    ]
  }
}

jimmy_id <- as.integer(
  selected$player_id[[1]]
)

jimmy_name <- as.character(
  selected$player_name[[1]]
)

cat(
  "Selected: ",
  jimmy_name,
  " | internal player_id=",
  jimmy_id,
  "\n\n",
  sep = ""
)

# ------------------------------------------------------------
# 2. Identity map
# ------------------------------------------------------------

cat("[2] NBA IDENTITY CHECK\n")
cat("------------------------------------------------------------\n")

identity <- data.frame()

if (
  "external_player_identity" %in%
  tables
) {
  id_cols <- fields(
    "external_player_identity"
  )
  
  if (
    all(
      c(
        "player_id",
        "source_player_id"
      ) %in%
      id_cols
    )
  ) {
    identity <- DBI::dbGetQuery(
      con,
      "
        SELECT *
        FROM external_player_identity
        WHERE player_id = ?
           OR source_player_id = ?
        ORDER BY source_name, source_player_id
      ",
      params = list(
        jimmy_id,
        "202710"
      )
    )
  }
  
  print_df(
    identity
  )
} else {
  cat(
    "TABLE MISSING: external_player_identity\n"
  )
}

nba_id_in_players <- FALSE

if (
  "nba_player_id" %in%
  names(selected)
) {
  nba_id_in_players <- identical(
    as.character(
      selected$nba_player_id[[1]]
    ),
    "202710"
  )
}

nba_id_in_map <- FALSE

if (
  nrow(identity) &&
  "source_player_id" %in%
  names(identity)
) {
  nba_id_in_map <- any(
    as.character(
      identity$source_player_id
    ) == "202710"
  )
}

nba_identity_pass <-
  nba_id_in_players ||
  nba_id_in_map

cat(
  "\nNBA ID 202710 mapped: ",
  if (
    nba_identity_pass
  ) "YES" else "NO",
  "\n\n",
  sep = ""
)

# ------------------------------------------------------------
# 3. Phase-3 tables
# ------------------------------------------------------------

phase3_tables <- c(
  "player_season_stats",
  "player_season_advanced",
  "player_season_shooting",
  "player_season_playmaking",
  "player_season_defense_rebounding",
  "player_season_roles",
  "player_season_impact",
  "player_projection_intelligence"
)

cat("[3] PHASE-3 TABLE CHECK\n")
cat("------------------------------------------------------------\n\n")

audit <- vector(
  "list",
  length(
    phase3_tables
  )
)

for (
  i in seq_along(
    phase3_tables
  )
) {
  table <- phase3_tables[[i]]
  
  q <- query_by_player(
    table,
    jimmy_id
  )
  
  season_text <- ""
  
  if (
    nrow(q$data) &&
    "season" %in%
    names(q$data)
  ) {
    season_text <- paste(
      unique(
        as.character(
          q$data$season
        )
      ),
      collapse = " | "
    )
  }
  
  if (
    nrow(q$data) &&
    !nzchar(
      season_text
    ) &&
    "latest_season" %in%
    names(q$data)
  ) {
    season_text <- paste(
      unique(
        as.character(
          q$data$
            latest_season
        )
      ),
      collapse = " | "
    )
  }
  
  audit[[i]] <- data.frame(
    table = table,
    status = q$status,
    rows = nrow(q$data),
    seasons = season_text,
    stringsAsFactors = FALSE
  )
  
  cat(
    table,
    ": ",
    q$status,
    " | rows=",
    nrow(q$data),
    "\n",
    sep = ""
  )
  
  if (nrow(q$data)) {
    print_df(
      compact(
        q$data
      )
    )
  }
  
  cat("\n")
}

audit <- do.call(
  rbind,
  audit
)

# ------------------------------------------------------------
# 4. Explicit 2025-26 check
# ------------------------------------------------------------

target_season <- "2025-26"

cat("[4] EXPLICIT 2025-26 PERFORMANCE CHECK\n")
cat("------------------------------------------------------------\n")

core_tables <- c(
  "player_season_stats",
  "player_season_advanced",
  "player_season_shooting",
  "player_season_playmaking",
  "player_season_defense_rebounding",
  "player_season_roles",
  "player_season_impact"
)

season_rows <- vector(
  "list",
  length(core_tables)
)

for (
  i in seq_along(
    core_tables
  )
) {
  table <- core_tables[[i]]
  
  if (!table %in% tables) {
    season_rows[[i]] <- data.frame(
      table = table,
      rows_2025_26 = 0L,
      present = FALSE,
      stringsAsFactors = FALSE
    )
    
    next
  }
  
  table_cols <- fields(
    table
  )
  
  if (
    !all(
      c(
        "player_id",
        "season"
      ) %in%
      table_cols
    )
  ) {
    n <- 0L
  } else {
    n <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n FROM ",
        quote_ident(table),
        " WHERE player_id = ? ",
        "AND season = ?"
      ),
      params = list(
        jimmy_id,
        target_season
      )
    )$n[[1]]
  }
  
  season_rows[[i]] <- data.frame(
    table = table,
    rows_2025_26 =
      as.integer(n),
    present =
      as.integer(n) >
      0L,
    stringsAsFactors = FALSE
  )
}

season_audit <- do.call(
  rbind,
  season_rows
)

print(
  season_audit,
  row.names = FALSE
)

cat("\n")

# ------------------------------------------------------------
# 5. Targeted advanced values
# ------------------------------------------------------------

cat("[5] 2025-26 ADVANCED VALUES\n")
cat("------------------------------------------------------------\n")

advanced_values <- data.frame()

if (
  "player_season_advanced" %in%
  tables
) {
  adv_cols <- fields(
    "player_season_advanced"
  )
  
  desired_adv <- intersect(
    c(
      "player_id",
      "team_id",
      "season",
      "source_player_id",
      "source_name",
      "usage_rate",
      "assist_pct",
      "rebound_pct",
      "offensive_rating",
      "defensive_rating",
      "net_rating",
      "official_possessions",
      "estimated_possessions",
      "player_impact_estimate"
    ),
    adv_cols
  )
  
  advanced_values <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      paste(
        vapply(
          desired_adv,
          quote_ident,
          character(1)
        ),
        collapse = ", "
      ),
      " FROM player_season_advanced ",
      "WHERE player_id = ? AND season = ? ",
      "ORDER BY team_id"
    ),
    params = list(
      jimmy_id,
      target_season
    )
  )
}

print_df(
  advanced_values
)

cat("\n")

# ------------------------------------------------------------
# 6. Final diagnostic
# ------------------------------------------------------------

core_complete <- all(
  season_audit$present
)

advanced_has_data <- FALSE

if (nrow(advanced_values)) {
  candidate_metric_cols <- intersect(
    c(
      "usage_rate",
      "assist_pct",
      "rebound_pct",
      "offensive_rating",
      "defensive_rating",
      "net_rating",
      "official_possessions",
      "estimated_possessions",
      "player_impact_estimate"
    ),
    names(
      advanced_values
    )
  )
  
  if (length(candidate_metric_cols)) {
    advanced_has_data <- any(
      vapply(
        candidate_metric_cols,
        function(nm) {
          values <- suppressWarnings(
            as.numeric(
              advanced_values[[nm]]
            )
          )
          
          any(
            is.finite(
              values
            )
          )
        },
        logical(1)
      )
    )
  }
}

cat("[6] SUMMARY\n")
cat("------------------------------------------------------------\n")
cat(
  "Canonical Jimmy Butler record: YES\n"
)
cat(
  "NBA ID 202710 mapped: ",
  if (
    nba_identity_pass
  ) "YES" else "NO",
  "\n",
  sep = ""
)
cat(
  "2025-26 rows in every core Phase-3 table: ",
  if (
    core_complete
  ) "YES" else "NO",
  "\n",
  sep = ""
)
cat(
  "2025-26 advanced metrics contain numeric data: ",
  if (
    advanced_has_data
  ) "YES" else "NO",
  "\n",
  sep = ""
)

missing_core <- season_audit$table[
  !season_audit$present
]

if (length(missing_core)) {
  cat(
    "Missing 2025-26 table(s): ",
    paste(
      missing_core,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
}

cat("\n")
cat("============================================================\n")

if (
  nba_identity_pass &&
  core_complete &&
  advanced_has_data
) {
  cat("JIMMY BUTLER DATA STATUS: PASS\n")
  cat("Jimmy Butler's identity and 2025-26 performance chain are populated.\n")
} else {
  cat("JIMMY BUTLER DATA STATUS: REVIEW REQUIRED\n")
  cat("Do NOT freeze NBA v1.0 yet.\n")
}

cat("============================================================\n")