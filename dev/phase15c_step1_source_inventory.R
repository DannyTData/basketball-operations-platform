# ============================================================
# TBI — Phase 15C Step 1
# Source-Backed 2025-26 Missing Player Inventory
#
# PURPOSE
#   For the current 2026-27 depth-chart players who do not have
#   a 2025-26 row in TBI, check the official project source
#   pipeline (hoopR / SportsDataverse NBA player box scores)
#   to determine who actually appeared in 2025-26.
#
# IMPORTANT
#   READ-ONLY. This script DOES NOT modify tbi.sqlite.
#   It creates the verified import queue for the next controlled
#   backfill step.
#
# SOURCE
#   hoopR::load_nba_player_box(seasons = 2026)
#
# OUTPUTS
#   qa/phase15c_missing_players_source_inventory.csv
#   qa/phase15c_players_requiring_backfill.csv
#   qa/phase15c_expected_no_2025_26_nba_stats.csv
#   qa/phase15c_source_name_ambiguities.csv
#   qa/phase15c_source_player_box_columns.csv
# ============================================================

required_pkgs <- c(
  "DBI",
  "RSQLite",
  "hoopR"
)

missing_pkgs <- required_pkgs[
  !vapply(
    required_pkgs,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_pkgs)) {
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall with:\n",
      "install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

db_path <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

audit_path <- file.path(
  "qa",
  "player_data_phase15b_full_audit.csv"
)

if (!file.exists(db_path)) {
  stop(
    "Database not found at inst/database/tbi.sqlite",
    call. = FALSE
  )
}

if (!file.exists(audit_path)) {
  stop(
    paste0(
      "Phase 15B audit file not found:\n",
      audit_path,
      "\nRun dev/audit_player_data_phase15b_refined.R first."
    ),
    call. = FALSE
  )
}

dir.create(
  "qa",
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

normalize_player_name <- function(x) {
  
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x[is.na(x)] <- ""
  
  x <- tolower(x)
  
  # Remove punctuation.
  x <- gsub(
    "[^a-z0-9 ]+",
    " ",
    x
  )
  
  # Remove common suffixes for matching only.
  x <- gsub(
    "\\b(jr|sr|ii|iii|iv|v)\\b",
    " ",
    x
  )
  
  x <- gsub(
    "\\s+",
    " ",
    x
  )
  
  trimws(x)
}

first_existing_col <- function(df, candidates) {
  
  hit <- candidates[
    candidates %in% names(df)
  ]
  
  if (!length(hit)) {
    return(NA_character_)
  }
  
  hit[[1]]
}

coalesce_character <- function(df, cols) {
  
  cols <- cols[
    cols %in% names(df)
  ]
  
  if (!length(cols)) {
    return(
      rep(
        NA_character_,
        nrow(df)
      )
    )
  }
  
  out <- rep(
    NA_character_,
    nrow(df)
  )
  
  for (col in cols) {
    
    val <- as.character(
      df[[col]]
    )
    
    use <- (
      is.na(out) |
        !nzchar(trimws(out))
    ) &
      !is.na(val) &
      nzchar(trimws(val))
    
    out[use] <- val[use]
  }
  
  out
}

safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

# ------------------------------------------------------------
# Load Phase 15B missing-player population
# ------------------------------------------------------------

audit <- utils::read.csv(
  audit_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

needed_cols <- c(
  "player_id",
  "player_name",
  "team_id",
  "team_name",
  "review_class"
)

missing_audit_cols <- setdiff(
  needed_cols,
  names(audit)
)

if (length(missing_audit_cols)) {
  stop(
    paste0(
      "Phase 15B audit is missing required column(s): ",
      paste(
        missing_audit_cols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

missing_players <- audit[
  audit$review_class %in% c(
    "NO NBA PERFORMANCE DATA ON THIS ID",
    "NO TARGET-SEASON ROW — HAS OTHER NBA HISTORY"
  ),
  ,
  drop = FALSE
]

missing_players <- missing_players[
  order(
    missing_players$team_name,
    missing_players$player_name
  ),
  ,
  drop = FALSE
]

missing_players$normalized_name <-
  normalize_player_name(
    missing_players$player_name
  )

cat("\n")
cat("============================================================\n")
cat("TBI PHASE 15C — 2025-26 SOURCE INVENTORY\n")
cat("============================================================\n")
cat(
  "Current missing-player population: ",
  nrow(missing_players),
  "\n",
  sep = ""
)

if (nrow(missing_players) == 0L) {
  cat("Nothing to inventory.\n")
  cat("============================================================\n")
  quit(
    save = "no",
    status = 0
  )
}

# ------------------------------------------------------------
# Pull 2025-26 NBA player box scores
#
# SportsDataverse season key 2026 corresponds to the NBA
# season ending in 2026 (2025-26).
# ------------------------------------------------------------

cat("\n[1/6] Loading hoopR 2025-26 NBA player box scores...\n")

source_box <- tryCatch(
  {
    hoopR::load_nba_player_box(
      seasons = 2026
    )
  },
  error = function(e) {
    stop(
      paste0(
        "hoopR player-box download failed:\n",
        conditionMessage(e),
        "\n\nNo database changes were made."
      ),
      call. = FALSE
    )
  }
)

source_box <- as.data.frame(
  source_box,
  stringsAsFactors = FALSE
)

if (!nrow(source_box)) {
  stop(
    "hoopR returned zero player-box rows for season 2026.",
    call. = FALSE
  )
}

cat(
  "Source rows loaded: ",
  nrow(source_box),
  "\n",
  sep = ""
)

# Save the source schema so the controlled write script can be
# built against the exact installed hoopR data shape.
utils::write.csv(
  data.frame(
    column_name = names(source_box),
    stringsAsFactors = FALSE
  ),
  file.path(
    "qa",
    "phase15c_source_player_box_columns.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# Discover source identity columns dynamically
# ------------------------------------------------------------

name_col <- first_existing_col(
  source_box,
  c(
    "athlete_display_name",
    "athlete_name",
    "player_name",
    "display_name",
    "name",
    "athlete_full_name"
  )
)

id_col <- first_existing_col(
  source_box,
  c(
    "athlete_id",
    "player_id",
    "espn_player_id",
    "athlete_uid"
  )
)

team_id_col <- first_existing_col(
  source_box,
  c(
    "team_id",
    "team_espn_id",
    "team_uid"
  )
)

team_name_col <- first_existing_col(
  source_box,
  c(
    "team_display_name",
    "team_name",
    "team_location",
    "team_abbreviation"
  )
)

game_id_col <- first_existing_col(
  source_box,
  c(
    "game_id",
    "id"
  )
)

if (is.na(name_col)) {
  
  stop(
    paste0(
      "Could not locate a player-name field in hoopR source.\n",
      "Source columns were written to:\n",
      "qa/phase15c_source_player_box_columns.csv"
    ),
    call. = FALSE
  )
}

cat(
  "[2/6] Source player-name column: ",
  name_col,
  "\n",
  sep = ""
)

if (!is.na(id_col)) {
  cat(
    "      Source player-ID column: ",
    id_col,
    "\n",
    sep = ""
  )
} else {
  cat(
    "      Source player-ID column: NOT FOUND — matching by name only\n"
  )
}

# ------------------------------------------------------------
# Build clean source identity table
# ------------------------------------------------------------

source_box$.source_player_name <-
  as.character(
    source_box[[name_col]]
  )

source_box$.normalized_name <-
  normalize_player_name(
    source_box$.source_player_name
  )

if (!is.na(id_col)) {
  source_box$.source_player_id <-
    as.character(
      source_box[[id_col]]
    )
} else {
  source_box$.source_player_id <-
    NA_character_
}

if (!is.na(team_id_col)) {
  source_box$.source_team_id <-
    as.character(
      source_box[[team_id_col]]
    )
} else {
  source_box$.source_team_id <-
    NA_character_
}

if (!is.na(team_name_col)) {
  source_box$.source_team_name <-
    as.character(
      source_box[[team_name_col]]
    )
} else {
  source_box$.source_team_name <-
    NA_character_
}

if (!is.na(game_id_col)) {
  source_box$.source_game_id <-
    as.character(
      source_box[[game_id_col]]
    )
} else {
  source_box$.source_game_id <-
    NA_character_
}

source_box <- source_box[
  !is.na(source_box$.source_player_name) &
    nzchar(
      trimws(
        source_box$.source_player_name
      )
    ),
  ,
  drop = FALSE
]

# ------------------------------------------------------------
# Summarize 2025-26 source appearances per normalized player
# ------------------------------------------------------------

source_keys <- unique(
  source_box[
    ,
    c(
      ".normalized_name",
      ".source_player_id",
      ".source_player_name"
    ),
    drop = FALSE
  ]
)

# Count unique source IDs/names for ambiguity detection.
key_split <- split(
  source_keys,
  source_keys$.normalized_name
)

identity_summary <- do.call(
  rbind,
  lapply(
    names(key_split),
    function(key) {
      
      x <- key_split[[key]]
      
      ids <- unique(
        x$.source_player_id[
          !is.na(x$.source_player_id) &
            nzchar(x$.source_player_id)
        ]
      )
      
      names_seen <- unique(
        x$.source_player_name[
          !is.na(x$.source_player_name) &
            nzchar(x$.source_player_name)
        ]
      )
      
      data.frame(
        normalized_name = key,
        source_identity_count =
          if (length(ids)) length(ids)
        else length(names_seen),
        source_player_ids =
          paste(ids, collapse = " | "),
        source_player_names =
          paste(names_seen, collapse = " | "),
        stringsAsFactors = FALSE
      )
    }
  )
)

row.names(identity_summary) <- NULL

# Appearance/game count.
appearance_summary <- do.call(
  rbind,
  lapply(
    split(
      source_box,
      source_box$.normalized_name
    ),
    function(x) {
      
      game_count <- if (
        any(
          !is.na(x$.source_game_id) &
          nzchar(x$.source_game_id)
        )
      ) {
        length(
          unique(
            x$.source_game_id[
              !is.na(x$.source_game_id) &
                nzchar(x$.source_game_id)
            ]
          )
        )
      } else {
        nrow(x)
      }
      
      data.frame(
        normalized_name =
          x$.normalized_name[[1]],
        source_box_rows =
          nrow(x),
        source_games =
          game_count,
        source_teams =
          paste(
            sort(
              unique(
                x$.source_team_name[
                  !is.na(
                    x$.source_team_name
                  ) &
                    nzchar(
                      x$.source_team_name
                    )
                ]
              )
            ),
            collapse = " | "
          ),
        stringsAsFactors = FALSE
      )
    }
  )
)

row.names(appearance_summary) <- NULL

source_summary <- merge(
  identity_summary,
  appearance_summary,
  by = "normalized_name",
  all = TRUE
)

# ------------------------------------------------------------
# Match the 131 TBI players to 2025-26 source
# ------------------------------------------------------------

cat("\n[3/6] Matching TBI players to 2025-26 source...\n")

inventory <- merge(
  missing_players,
  source_summary,
  by = "normalized_name",
  all.x = TRUE,
  sort = FALSE
)

# Restore stable team/player ordering.
inventory <- inventory[
  order(
    inventory$team_name,
    inventory$player_name
  ),
  ,
  drop = FALSE
]

inventory$source_match <-
  !is.na(
    inventory$source_box_rows
  ) &
  inventory$source_box_rows > 0

inventory$source_ambiguous <-
  !is.na(
    inventory$source_identity_count
  ) &
  inventory$source_identity_count > 1

inventory$phase15c_status <- ifelse(
  inventory$source_ambiguous,
  "SOURCE IDENTITY AMBIGUOUS — REVIEW",
  ifelse(
    inventory$source_match,
    "PLAYED 2025-26 — BACKFILL REQUIRED",
    "NO 2025-26 SOURCE MATCH — EXPECTED/REVIEW"
  )
)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

backfill_queue <- inventory[
  inventory$phase15c_status ==
    "PLAYED 2025-26 — BACKFILL REQUIRED",
  ,
  drop = FALSE
]

expected_or_review <- inventory[
  inventory$phase15c_status ==
    "NO 2025-26 SOURCE MATCH — EXPECTED/REVIEW",
  ,
  drop = FALSE
]

ambiguities <- inventory[
  inventory$phase15c_status ==
    "SOURCE IDENTITY AMBIGUOUS — REVIEW",
  ,
  drop = FALSE
]

utils::write.csv(
  inventory,
  file.path(
    "qa",
    "phase15c_missing_players_source_inventory.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  backfill_queue,
  file.path(
    "qa",
    "phase15c_players_requiring_backfill.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  expected_or_review,
  file.path(
    "qa",
    "phase15c_expected_no_2025_26_nba_stats.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  ambiguities,
  file.path(
    "qa",
    "phase15c_source_name_ambiguities.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# Console report
# ------------------------------------------------------------

cat("[4/6] Inventory complete.\n\n")

cat(
  "PLAYED 2025-26 — BACKFILL REQUIRED: ",
  nrow(backfill_queue),
  "\n",
  sep = ""
)

cat(
  "NO 2025-26 SOURCE MATCH: ",
  nrow(expected_or_review),
  "\n",
  sep = ""
)

cat(
  "SOURCE IDENTITY AMBIGUITIES: ",
  nrow(ambiguities),
  "\n\n",
  sep = ""
)

cat("[5/6] PLAYERS REQUIRING BACKFILL\n")

if (nrow(backfill_queue)) {
  
  display_cols <- c(
    "player_id",
    "player_name",
    "team_name",
    "source_player_ids",
    "source_player_names",
    "source_games",
    "source_teams"
  )
  
  display_cols <- display_cols[
    display_cols %in%
      names(backfill_queue)
  ]
  
  print(
    backfill_queue[
      ,
      display_cols,
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  cat("NONE\n")
}

cat("\n")

if (nrow(ambiguities)) {
  
  cat("SOURCE IDENTITY AMBIGUITIES\n")
  
  display_cols <- c(
    "player_id",
    "player_name",
    "team_name",
    "source_player_ids",
    "source_player_names"
  )
  
  display_cols <- display_cols[
    display_cols %in%
      names(ambiguities)
  ]
  
  print(
    ambiguities[
      ,
      display_cols,
      drop = FALSE
    ],
    row.names = FALSE
  )
  
  cat("\n")
}

cat("[6/6] QA files written to qa/\n\n")

cat("============================================================\n")

if (nrow(ambiguities) > 0L) {
  
  cat("PHASE 15C STEP 1 STATUS: REVIEW REQUIRED\n")
  cat(
    nrow(ambiguities),
    " ambiguous source identity match(es) must be resolved before backfill.\n",
    sep = ""
  )
  
} else {
  
  cat("PHASE 15C STEP 1 STATUS: PASS\n")
  cat(
    nrow(backfill_queue),
    " player(s) verified from 2025-26 source and queued for backfill.\n",
    sep = ""
  )
  cat(
    nrow(expected_or_review),
    " player(s) had no 2025-26 source match and were NOT fabricated.\n",
    sep = ""
  )
  cat(
    "Next step: controlled database backfill for the verified queue.\n"
  )
}

cat("============================================================\n")