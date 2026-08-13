# ============================================================
# PHASE 3 STEP 3.2 — SQLITE COMMIT FIX
# Identity resolution preserved; stats upserts are SQLite-safe.
# ============================================================

# ============================================================
# TBI NBA Basketball Operations Platform
# PATCH: REGULAR-SEASON ONLY + TRUE APPEARANCE GP
# - hoopR season_type == 2 only
# - NBA Cup Championship excluded via hoopR schedule metadata
# - postseason/preseason/offseason excluded before aggregation
# - DNP rows do not count as games played
# PHASE 3 — STEP 3
# Real NBA Data Ingestion + Validation
#
# Source:
#   hoopR::load_nba_player_box()
#
# First target season:
#   hoopR season year: 2026
#   TBI season label: 2025-26
#
# Workflow:
#   1. Download NBA player box scores
#   2. Standardize source columns
#   3. Aggregate game rows -> player/team/season
#   4. Map source teams -> TBI team_id
#   5. Map source players -> TBI player_id
#   6. Produce validation report
#   7. Preview first (commit = FALSE)
#   8. Commit only after validation is clean
#   9. Build Step-2 derived metrics automatically
# ============================================================


# ------------------------------------------------------------
# Package requirement
# ------------------------------------------------------------

phase3_step3_require_packages <- function() {
  
  required <- c(
    "hoopR",
    "dplyr",
    "stringr",
    "DBI"
  )
  
  missing <- required[
    !vapply(
      required,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  
  if (length(missing)) {
    stop(
      paste0(
        "Install required packages first: ",
        paste(
          missing,
          collapse = ", "
        ),
        "\n\nRun:\ninstall.packages(c(",
        paste0(
          '"',
          missing,
          '"',
          collapse = ", "
        ),
        "))"
      )
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Generic source-column resolver
# ------------------------------------------------------------

p3s3_first_column <- function(
    df,
    candidates,
    default = NA) {
  
  hit <- candidates[
    candidates %in%
      names(df)
  ]
  
  if (!length(hit)) {
    return(
      rep(
        default,
        nrow(df)
      )
    )
  }
  
  df[[hit[[1]]]]
}


# ------------------------------------------------------------
# Normalize names for matching
# ------------------------------------------------------------

p3s3_normalize_name <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_to_lower(x)
  
  x <- stringr::str_replace_all(
    x,
    "[’‘`]",
    "'"
  )
  
  x <- stringr::str_replace_all(
    x,
    "[^a-z0-9]",
    ""
  )
  
  x
}


# ------------------------------------------------------------
# Normalize team abbreviation
# ------------------------------------------------------------

p3s3_normalize_team_abbr <- function(x) {
  
  x <- toupper(
    trimws(
      as.character(x)
    )
  )
  
  # Common historical/source variants.
  x[x == "BRK"] <- "BKN"
  x[x == "CHO"] <- "CHA"
  x[x == "PHO"] <- "PHX"
  x[x == "GS"] <- "GSW"
  x[x == "NO"] <- "NOP"
  x[x == "NY"] <- "NYK"
  x[x == "SA"] <- "SAS"
  
  x
}


# ------------------------------------------------------------
# Parse minutes
# ------------------------------------------------------------

p3s3_parse_minutes <- function(x) {
  
  if (is.numeric(x)) {
    return(
      suppressWarnings(
        as.numeric(x)
      )
    )
  }
  
  x <- as.character(x)
  
  output <- suppressWarnings(
    as.numeric(x)
  )
  
  clock <- grepl(
    "^[0-9]+:[0-9]+$",
    x
  )
  
  if (any(clock, na.rm = TRUE)) {
    
    pieces <- strsplit(
      x[clock],
      ":",
      fixed = TRUE
    )
    
    output[clock] <- vapply(
      pieces,
      function(p) {
        
        minutes <- suppressWarnings(
          as.numeric(p[[1]])
        )
        
        seconds <- suppressWarnings(
          as.numeric(p[[2]])
        )
        
        if (
          is.na(minutes) ||
          is.na(seconds)
        ) {
          return(NA_real_)
        }
        
        minutes +
          seconds / 60
      },
      numeric(1)
    )
  }
  
  output
}


# ------------------------------------------------------------
# Download hoopR NBA player box data
# ------------------------------------------------------------

download_phase3_nba_player_box <- function(
    season_year = 2026) {
  
  phase3_step3_require_packages()
  
  message(
    "Downloading hoopR NBA player box data for season year ",
    season_year,
    "..."
  )
  
  data <- hoopR::load_nba_player_box(
    seasons = season_year
  )
  
  if (
    is.null(data) ||
    !is.data.frame(data) ||
    !nrow(data)
  ) {
    stop(
      "hoopR returned no NBA player box data."
    )
  }
  
  data
}


# ------------------------------------------------------------
# Inspect source shape
# ------------------------------------------------------------

inspect_phase3_nba_source <- function(
    raw_data) {
  
  if (
    is.null(raw_data) ||
    !is.data.frame(raw_data)
  ) {
    stop(
      "raw_data must be a data.frame."
    )
  }
  
  list(
    rows = nrow(raw_data),
    columns = ncol(raw_data),
    names = names(raw_data),
    sample =
      utils::head(
        raw_data,
        5
      )
  )
}


# ------------------------------------------------------------
# Standardize hoopR source rows
# ------------------------------------------------------------

standardize_hoopr_nba_player_box <- function(
    raw_data,
    season_label = "2025-26") {
  
  phase3_step3_require_packages()
  
  d <- raw_data
  
  player_name <-
    p3s3_first_column(
      d,
      c(
        "athlete_display_name",
        "athlete_name",
        "player_display_name",
        "player_name",
        "name"
      )
    )
  
  source_player_id <-
    p3s3_first_column(
      d,
      c(
        "athlete_id",
        "player_id",
        "person_id"
      )
    )
  
  team_abbreviation <-
    p3s3_first_column(
      d,
      c(
        "team_abbreviation",
        "team_short_display_name",
        "team_abbrev",
        "team"
      )
    )
  
  team_name <-
    p3s3_first_column(
      d,
      c(
        "team_display_name",
        "team_name",
        "team_location_name"
      )
    )
  
  game_id <-
    p3s3_first_column(
      d,
      c(
        "game_id",
        "id",
        "event_id"
      )
    )
  
  # hoopR documents season_type as:
  #   1 = preseason
  #   2 = regular season
  #   3 = postseason
  #   4 = offseason
  # Preserve it on every standardized row so the regular-season
  # filter can happen BEFORE player-season aggregation.
  season_type_source <-
    p3s3_first_column(
      d,
      c(
        "season_type",
        "season_type_id",
        "season_type_code"
      )
    )
  
  game_date_source <-
    p3s3_first_column(
      d,
      c(
        "game_date",
        "date",
        "game_date_time"
      )
    )
  
  did_not_play_source <-
    p3s3_first_column(
      d,
      c(
        "did_not_play",
        "dnp",
        "did_not_play_flag"
      ),
      default = FALSE
    )
  
  did_not_play_flag <- if (
    is.logical(did_not_play_source)
  ) {
    did_not_play_source
  } else {
    toupper(
      trimws(
        as.character(
          did_not_play_source
        )
      )
    ) %in%
      c(
        "TRUE",
        "T",
        "1",
        "YES",
        "Y",
        "DNP",
        "DID NOT PLAY"
      )
  }
  
  did_not_play_flag[
    is.na(did_not_play_flag)
  ] <- FALSE
  
  starter_source <-
    p3s3_first_column(
      d,
      c(
        "starter",
        "starter_flag",
        "is_starter"
      ),
      default = FALSE
    )
  
  starter_flag <- if (
    is.logical(starter_source)
  ) {
    starter_source
  } else {
    toupper(
      as.character(
        starter_source
      )
    ) %in%
      c(
        "TRUE",
        "T",
        "1",
        "YES",
        "Y",
        "STARTER"
      )
  }
  
  minutes <-
    p3s3_parse_minutes(
      p3s3_first_column(
        d,
        c(
          "minutes",
          "minutes_played",
          "min"
        )
      )
    )
  
  points <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "points",
          "pts"
        )
      )
    )
  )
  
  offensive_rebounds <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "offensive_rebounds",
          "offensive_rebound",
          "oreb"
        )
      )
    )
  )
  
  defensive_rebounds <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "defensive_rebounds",
          "defensive_rebound",
          "dreb"
        )
      )
    )
  )
  
  rebounds <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "rebounds",
          "total_rebounds",
          "reb"
        )
      )
    )
  )
  
  assists <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "assists",
          "ast"
        )
      )
    )
  )
  
  steals <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "steals",
          "stl"
        )
      )
    )
  )
  
  blocks <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "blocks",
          "blk"
        )
      )
    )
  )
  
  turnovers <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "turnovers",
          "turnover",
          "tov"
        )
      )
    )
  )
  
  fgm <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "field_goals_made",
          "field_goals_made_field_goals_attempted",
          "fgm"
        )
      )
    )
  )
  
  fga <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "field_goals_attempted",
          "fga"
        )
      )
    )
  )
  
  three_pm <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "three_point_field_goals_made",
          "three_pointers_made",
          "three_point_made",
          "fg3m"
        )
      )
    )
  )
  
  three_pa <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "three_point_field_goals_attempted",
          "three_pointers_attempted",
          "three_point_attempted",
          "fg3a"
        )
      )
    )
  )
  
  ftm <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "free_throws_made",
          "ftm"
        )
      )
    )
  )
  
  fta <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "free_throws_attempted",
          "fta"
        )
      )
    )
  )
  
  personal_fouls <- suppressWarnings(
    as.numeric(
      p3s3_first_column(
        d,
        c(
          "fouls",
          "personal_fouls",
          "pf"
        )
      )
    )
  )
  
  out <- data.frame(
    source_player_id =
      as.character(
        source_player_id
      ),
    source_player_name =
      as.character(
        player_name
      ),
    source_team_abbr =
      p3s3_normalize_team_abbr(
        team_abbreviation
      ),
    source_team_name =
      as.character(
        team_name
      ),
    source_game_id =
      as.character(
        game_id
      ),
    source_game_date =
      as.character(
        game_date_source
      ),
    source_season_type =
      as.character(
        season_type_source
      ),
    season =
      as.character(
        season_label
      ),
    did_not_play =
      as.logical(
        did_not_play_flag
      ),
    is_starter =
      starter_flag,
    minutes = minutes,
    points = points,
    offensive_rebounds =
      offensive_rebounds,
    defensive_rebounds =
      defensive_rebounds,
    rebounds = rebounds,
    assists = assists,
    steals = steals,
    blocks = blocks,
    turnovers = turnovers,
    field_goals_made =
      fgm,
    field_goals_attempted =
      fga,
    three_pointers_made =
      three_pm,
    three_pointers_attempted =
      three_pa,
    free_throws_made =
      ftm,
    free_throws_attempted =
      fta,
    personal_fouls =
      personal_fouls,
    stringsAsFactors = FALSE
  )
  
  # Remove rows with no identifiable player name.
  out <- out[
    !is.na(out$source_player_name) &
      nzchar(
        trimws(
          out$source_player_name
        )
      ),
    ,
    drop = FALSE
  ]
  
  rownames(out) <- NULL
  
  out
}


# ------------------------------------------------------------
# Aggregate game-level rows to player/team/season
# ------------------------------------------------------------

aggregate_phase3_player_season <- function(
    standardized_rows) {
  
  phase3_step3_require_packages()
  
  d <- standardized_rows
  
  if (!nrow(d)) {
    return(
      data.frame()
    )
  }
  
  d |>
    dplyr::group_by(
      .data$source_player_id,
      .data$source_player_name,
      .data$source_team_abbr,
      .data$source_team_name,
      .data$season
    ) |>
    dplyr::summarise(
      # Count a game only when the player actually appeared.
      # hoopR player-box data can contain DNP rows; those rows must not
      # inflate Games Played, MPG, availability, or sample reliability.
      games_played =
        dplyr::n_distinct(
          .data$source_game_id[
            !is.na(
              .data$source_game_id
            ) &
              !.data$did_not_play
          ]
        ),
      games_started =
        sum(
          .data$is_starter,
          na.rm = TRUE
        ),
      minutes =
        sum(
          .data$minutes,
          na.rm = TRUE
        ),
      points =
        sum(
          .data$points,
          na.rm = TRUE
        ),
      offensive_rebounds =
        sum(
          .data$offensive_rebounds,
          na.rm = TRUE
        ),
      defensive_rebounds =
        sum(
          .data$defensive_rebounds,
          na.rm = TRUE
        ),
      rebounds =
        sum(
          .data$rebounds,
          na.rm = TRUE
        ),
      assists =
        sum(
          .data$assists,
          na.rm = TRUE
        ),
      steals =
        sum(
          .data$steals,
          na.rm = TRUE
        ),
      blocks =
        sum(
          .data$blocks,
          na.rm = TRUE
        ),
      turnovers =
        sum(
          .data$turnovers,
          na.rm = TRUE
        ),
      field_goals_made =
        sum(
          .data$field_goals_made,
          na.rm = TRUE
        ),
      field_goals_attempted =
        sum(
          .data$field_goals_attempted,
          na.rm = TRUE
        ),
      three_pointers_made =
        sum(
          .data$three_pointers_made,
          na.rm = TRUE
        ),
      three_pointers_attempted =
        sum(
          .data$three_pointers_attempted,
          na.rm = TRUE
        ),
      free_throws_made =
        sum(
          .data$free_throws_made,
          na.rm = TRUE
        ),
      free_throws_attempted =
        sum(
          .data$free_throws_attempted,
          na.rm = TRUE
        ),
      personal_fouls =
        sum(
          .data$personal_fouls,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) |>
    as.data.frame()
}


# ------------------------------------------------------------
# Read TBI team map
# ------------------------------------------------------------

get_phase3_team_map <- function(
    con) {
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name,
      abbreviation
    FROM teams
    "
  )
  
  teams$abbr_key <-
    p3s3_normalize_team_abbr(
      teams$abbreviation
    )
  
  teams$name_key <-
    p3s3_normalize_name(
      teams$team_name
    )
  
  teams
}


# ------------------------------------------------------------
# Read TBI player map
# ------------------------------------------------------------

get_phase3_player_map <- function(
    con) {
  
  players <- DBI::dbGetQuery(
    con,
    "
    SELECT
      player_id,
      player_name,
      primary_position,
      player_age
    FROM players
    "
  )
  
  players$name_key <-
    p3s3_normalize_name(
      players$player_name
    )
  
  players
}


# ------------------------------------------------------------
# Map source teams
# ------------------------------------------------------------

map_phase3_teams <- function(
    aggregated,
    team_map) {
  
  d <- aggregated
  
  d$abbr_key <-
    p3s3_normalize_team_abbr(
      d$source_team_abbr
    )
  
  d$name_key <-
    p3s3_normalize_name(
      d$source_team_name
    )
  
  abbr_match <- match(
    d$abbr_key,
    team_map$abbr_key
  )
  
  name_match <- match(
    d$name_key,
    team_map$name_key
  )
  
  resolved_index <- ifelse(
    !is.na(abbr_match),
    abbr_match,
    name_match
  )
  
  d$team_id <-
    team_map$team_id[
      resolved_index
    ]
  
  d$tbi_team_name <-
    team_map$team_name[
      resolved_index
    ]
  
  d
}


# ------------------------------------------------------------
# Map source players
# ------------------------------------------------------------

map_phase3_players <- function(
    team_mapped,
    player_map) {
  
  d <- team_mapped
  
  d$player_name_key <-
    p3s3_normalize_name(
      d$source_player_name
    )
  
  player_index <- match(
    d$player_name_key,
    player_map$name_key
  )
  
  d$player_id <-
    player_map$player_id[
      player_index
    ]
  
  d$tbi_player_name <-
    player_map$player_name[
      player_index
    ]
  
  d
}


# ------------------------------------------------------------
# Build Step-1 import frame
# ------------------------------------------------------------

build_phase3_step1_import_frame <- function(
    mapped_data) {
  
  d <- mapped_data
  
  data.frame(
    player_id =
      suppressWarnings(
        as.integer(
          d$player_id
        )
      ),
    team_id =
      suppressWarnings(
        as.integer(
          d$team_id
        )
      ),
    season =
      as.character(
        d$season
      ),
    games_played =
      suppressWarnings(
        as.integer(
          d$games_played
        )
      ),
    games_started =
      suppressWarnings(
        as.integer(
          d$games_started
        )
      ),
    minutes =
      as.numeric(
        d$minutes
      ),
    points =
      as.numeric(
        d$points
      ),
    offensive_rebounds =
      as.numeric(
        d$offensive_rebounds
      ),
    defensive_rebounds =
      as.numeric(
        d$defensive_rebounds
      ),
    rebounds =
      as.numeric(
        d$rebounds
      ),
    assists =
      as.numeric(
        d$assists
      ),
    steals =
      as.numeric(
        d$steals
      ),
    blocks =
      as.numeric(
        d$blocks
      ),
    turnovers =
      as.numeric(
        d$turnovers
      ),
    field_goals_made =
      as.numeric(
        d$field_goals_made
      ),
    field_goals_attempted =
      as.numeric(
        d$field_goals_attempted
      ),
    three_pointers_made =
      as.numeric(
        d$three_pointers_made
      ),
    three_pointers_attempted =
      as.numeric(
        d$three_pointers_attempted
      ),
    free_throws_made =
      as.numeric(
        d$free_throws_made
      ),
    free_throws_attempted =
      as.numeric(
        d$free_throws_attempted
      ),
    personal_fouls =
      as.numeric(
        d$personal_fouls
      ),
    source_name =
      "hoopR",
    source_player_id =
      as.character(
        d$source_player_id
      ),
    source_team =
      as.character(
        d$source_team_abbr
      ),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# Validation report
# ------------------------------------------------------------

validate_phase3_ingestion <- function(
    mapped_data) {
  
  d <- mapped_data
  
  unmatched_players <- unique(
    d[
      is.na(d$player_id),
      c(
        "source_player_id",
        "source_player_name",
        "source_team_abbr",
        "season"
      ),
      drop = FALSE
    ]
  )
  
  unmatched_teams <- unique(
    d[
      is.na(d$team_id),
      c(
        "source_team_abbr",
        "source_team_name",
        "season"
      ),
      drop = FALSE
    ]
  )
  
  matched <- d[
    !is.na(d$player_id) &
      !is.na(d$team_id),
    ,
    drop = FALSE
  ]
  
  duplicate_key <- duplicated(
    paste(
      matched$player_id,
      matched$team_id,
      matched$season,
      sep = "|"
    )
  )
  
  duplicate_rows <- matched[
    duplicate_key,
    ,
    drop = FALSE
  ]
  
  list(
    source_rows =
      nrow(d),
    matched_rows =
      nrow(matched),
    unmatched_player_count =
      nrow(unmatched_players),
    unmatched_team_count =
      nrow(unmatched_teams),
    duplicate_key_count =
      nrow(duplicate_rows),
    match_rate = if (
      nrow(d) > 0
    ) {
      nrow(matched) /
        nrow(d)
    } else {
      NA_real_
    },
    unmatched_players =
      unmatched_players,
    unmatched_teams =
      unmatched_teams,
    duplicate_rows =
      duplicate_rows,
    ready_to_commit =
      nrow(unmatched_teams) == 0 &&
      nrow(duplicate_rows) == 0
  )
}


# ------------------------------------------------------------
# Main Step-3 ingestion runner
# ------------------------------------------------------------

run_phase3_step3_ingestion <- function(
    season_year = 2026,
    season_label = "2025-26",
    commit = FALSE) {
  
  phase3_step3_require_packages()
  
  if (
    !exists(
      "connect_db",
      mode = "function"
    )
  ) {
    stop(
      "connect_db() is not loaded."
    )
  }
  
  if (
    !exists(
      "upsert_player_season_stats",
      mode = "function"
    )
  ) {
    stop(
      paste(
        "Phase 3 Step 1 is not loaded.",
        "Expected upsert_player_season_stats()."
      )
    )
  }
  
  raw_data <-
    download_phase3_nba_player_box(
      season_year =
        season_year
    )
  
  standardized <-
    standardize_hoopr_nba_player_box(
      raw_data =
        raw_data,
      season_label =
        season_label
    )
  
  aggregated <-
    aggregate_phase3_player_season(
      standardized
    )
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  team_map <-
    get_phase3_team_map(
      con
    )
  
  player_map <-
    get_phase3_player_map(
      con
    )
  
  mapped <-
    map_phase3_teams(
      aggregated =
        aggregated,
      team_map =
        team_map
    )
  
  mapped <-
    map_phase3_players(
      team_mapped =
        mapped,
      player_map =
        player_map
    )
  
  validation <-
    validate_phase3_ingestion(
      mapped
    )
  
  matched <- mapped[
    !is.na(mapped$player_id) &
      !is.na(mapped$team_id),
    ,
    drop = FALSE
  ]
  
  import_frame <-
    build_phase3_step1_import_frame(
      matched
    )
  
  result <- list(
    season_year =
      season_year,
    season_label =
      season_label,
    commit =
      commit,
    raw_rows =
      nrow(raw_data),
    standardized_rows =
      nrow(standardized),
    aggregated_rows =
      nrow(aggregated),
    matched_rows =
      nrow(matched),
    validation =
      validation,
    import_preview =
      import_frame
  )
  
  if (!isTRUE(commit)) {
    
    result$status <-
      "PREVIEW ONLY — DATABASE NOT CHANGED"
    
    return(result)
  }
  
  if (
    validation$unmatched_team_count > 0
  ) {
    stop(
      "Commit blocked: unmatched teams remain. Review validation$unmatched_teams."
    )
  }
  
  if (
    validation$duplicate_key_count > 0
  ) {
    stop(
      "Commit blocked: duplicate player/team/season keys remain."
    )
  }
  
  if (!nrow(import_frame)) {
    stop(
      "Commit blocked: no matched player rows are available."
    )
  }
  
  step1_result <-
    upsert_player_season_stats(
      import_frame,
      con = con
    )
  
  step2_result <- NULL
  
  if (
    exists(
      "build_advanced_metrics_from_player_stats",
      mode = "function"
    )
  ) {
    
    step2_result <-
      build_advanced_metrics_from_player_stats(
        season =
          season_label,
        con = con
      )
  }
  
  result$status <-
    "COMMITTED"
  
  result$step1_result <-
    step1_result
  
  result$step2_result <-
    step2_result
  
  result
}


# ------------------------------------------------------------
# Console validation summary
# ------------------------------------------------------------

print_phase3_step3_report <- function(result) {
  
  if (
    is.null(result) ||
    !is.list(result)
  ) {
    stop(
      "Provide the result from run_phase3_step3_ingestion()."
    )
  }
  
  validation <- result$validation
  
  cat(
    "\n",
    "============================================\n",
    "PHASE 3 STEP 3 — NBA DATA INGESTION REPORT\n",
    "============================================\n",
    sep = ""
  )
  
  cat(
    "Season: ",
    result$season_label,
    "\n",
    sep = ""
  )
  
  cat(
    "Status: ",
    result$status,
    "\n\n",
    sep = ""
  )
  
  cat(
    "Raw game/player rows: ",
    result$raw_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Aggregated player/team rows: ",
    result$aggregated_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Matched rows: ",
    validation$matched_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Match rate: ",
    sprintf(
      "%.1f%%",
      100 *
        validation$match_rate
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Unmatched players: ",
    validation$unmatched_player_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Unmatched teams: ",
    validation$unmatched_team_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Duplicate keys: ",
    validation$duplicate_key_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Ready to commit: ",
    validation$ready_to_commit,
    "\n",
    sep = ""
  )
  
  invisible(result)
}


# ------------------------------------------------------------
# Step 3 post-load health check
# ------------------------------------------------------------

phase3_step3_healthcheck <- function(
    season_label = "2025-26") {
  
  if (
    !exists(
      "connect_db",
      mode = "function"
    )
  ) {
    return(
      list(
        status = "REVIEW",
        issue =
          "connect_db() is unavailable."
      )
    )
  }
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  tables <- DBI::dbListTables(
    con
  )
  
  step1_exists <-
    "player_season_stats" %in%
    tables
  
  step2_exists <-
    "player_season_advanced" %in%
    tables
  
  step1_rows <- if (
    step1_exists
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_stats
      WHERE season = ?
      ",
      params = list(
        season_label
      )
    )$n[[1]]
  } else {
    0L
  }
  
  step2_rows <- if (
    step2_exists
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_advanced
      WHERE season = ?
      ",
      params = list(
        season_label
      )
    )$n[[1]]
  } else {
    0L
  }
  
  list(
    phase = "Phase 3",
    step =
      "Step 3 — Real NBA Data Ingestion + Validation",
    season =
      season_label,
    status = if (
      step1_rows > 0
    ) {
      "READY"
    } else {
      "AWAITING COMMIT"
    },
    player_season_stats_rows =
      step1_rows,
    player_season_advanced_rows =
      step2_rows,
    advanced_metrics_status = if (
      step2_rows > 0
    ) {
      "DERIVED"
    } else {
      "NOT YET BUILT"
    }
  )
}

# ============================================================
# PHASE 3 — STEP 3.1
# NBA Identity Resolution + Roster Synchronization
# ============================================================

# ------------------------------------------------------------
# Permanent external player identity map
# ------------------------------------------------------------

create_external_player_identity_table <- function(
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS external_player_identity (

      source_name TEXT NOT NULL,
      source_player_id TEXT NOT NULL,
      player_id INTEGER NOT NULL,

      source_player_name TEXT,
      first_seen_season TEXT,
      last_seen_season TEXT,

      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP,

      PRIMARY KEY (
        source_name,
        source_player_id
      )
    )
    "
  )
  
  try(
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_external_player_identity_player
      ON external_player_identity(player_id)
      "
    ),
    silent = TRUE
  )
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Regular-season row controls
# ------------------------------------------------------------

# hoopR / ESPN season_type mapping:
#   1 = preseason
#   2 = regular season
#   3 = postseason
#   4 = offseason
#
# Core TBI player-season statistics MUST use regular-season rows only.
# Playoff data can be modeled separately later; it must not inflate
# regular-season GP, MPG, availability, sample reliability, or BIE.

p3s31_is_regular_season_type <- function(x) {
  
  raw <- toupper(
    trimws(
      as.character(x)
    )
  )
  
  numeric_type <- suppressWarnings(
    as.integer(raw)
  )
  
  regular <-
    numeric_type == 2L |
    raw %in%
    c(
      "REGULAR SEASON",
      "REGULAR",
      "REG",
      "2"
    )
  
  regular[
    is.na(regular)
  ] <- FALSE
  
  regular
}


p3s31_excluded_team_abbr <- function() {
  
  c(
    "WORLD",
    "STARS",
    "STRIPES"
  )
}


p3s31_excluded_team_names <- function() {
  
  c(
    "WORLD",
    "TEAM STARS",
    "TEAM STRIPES"
  )
}


# ------------------------------------------------------------
# NBA Cup Championship exclusion
# ------------------------------------------------------------
#
# hoopR / ESPN can classify the NBA Cup Championship with
# season_type == 2 even though the NBA explicitly excludes that
# game from regular-season standings and player statistics.
#
# We therefore identify the Cup Championship from the hoopR
# schedule metadata and exclude its ESPN game_id before
# player-season aggregation.
#
# This is schedule-driven and season-agnostic; no hard-coded
# teams, dates, or game IDs are required.

p3s31_nba_cup_championship_game_ids <- function(
    season_year) {
  
  season_year <- suppressWarnings(
    as.integer(
      season_year
    )
  )
  
  if (
    !is.finite(
      season_year
    )
  ) {
    return(
      character()
    )
  }
  
  schedule <- tryCatch(
    hoopR::load_nba_schedule(
      seasons =
        season_year
    ),
    error = function(e) {
      warning(
        paste(
          "Unable to load hoopR NBA schedule for NBA Cup Championship filtering:",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      
      data.frame()
    }
  )
  
  if (
    is.null(schedule) ||
    !is.data.frame(schedule) ||
    !nrow(schedule)
  ) {
    return(
      character()
    )
  }
  
  headline <- if (
    "notes_headline" %in%
    names(schedule)
  ) {
    toupper(
      trimws(
        as.character(
          schedule$notes_headline
        )
      )
    )
  } else {
    rep(
      "",
      nrow(schedule)
    )
  }
  
  note_type <- if (
    "notes_type" %in%
    names(schedule)
  ) {
    toupper(
      trimws(
        as.character(
          schedule$notes_type
        )
      )
    )
  } else {
    rep(
      "",
      nrow(schedule)
    )
  }
  
  note_text <- paste(
    headline,
    note_type
  )
  
  is_cup <-
    grepl(
      "NBA CUP|EMIRATES.*CUP|IN-SEASON TOURNAMENT",
      note_text
    )
  
  # IMPORTANT: FINAL must be matched as a standalone word.
  # A plain "FINAL" regex also matches QUARTERFINAL and SEMIFINAL,
  # which would incorrectly remove Cup games that DO count in the
  # NBA regular season. Only the championship game is excluded.
  is_championship <-
    grepl(
      "\\bCHAMPIONSHIP\\b|\\bFINAL\\b",
      note_text,
      perl = TRUE
    )
  
  is_earlier_cup_round <-
    grepl(
      "\\bQUARTERFINAL\\b|\\bSEMIFINAL\\b|\\bQUARTER-FINAL\\b|\\bSEMI-FINAL\\b",
      note_text,
      perl = TRUE
    )
  
  keep <-
    is_cup &
    is_championship &
    !is_earlier_cup_round
  
  keep[
    is.na(
      keep
    )
  ] <- FALSE
  
  if (!any(keep)) {
    return(
      character()
    )
  }
  
  id_column <- intersect(
    c(
      "id",
      "game_id",
      "event_id"
    ),
    names(schedule)
  )
  
  if (!length(id_column)) {
    warning(
      paste(
        "NBA Cup Championship schedule row was found,",
        "but no schedule game-id column is available."
      ),
      call. = FALSE
    )
    
    return(
      character()
    )
  }
  
  ids <- unique(
    as.character(
      schedule[
        keep,
        id_column[[1]],
        drop = TRUE
      ]
    )
  )
  
  ids[
    !is.na(ids) &
      nzchar(
        trimws(ids)
      )
  ]
}


filter_phase3_regular_nba_rows <- function(
    df,
    excluded_game_ids = character()) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df)
  ) {
    return(df)
  }
  
  if (
    !"source_season_type" %in%
    names(df)
  ) {
    stop(
      paste(
        "Regular-season filtering cannot be verified because",
        "source_season_type is missing from standardized hoopR rows."
      ),
      call. = FALSE
    )
  }
  
  regular_season <-
    p3s31_is_regular_season_type(
      df$source_season_type
    )
  
  if (!any(regular_season)) {
    stop(
      paste(
        "No regular-season rows were identified.",
        "Expected hoopR season_type == 2 for NBA regular-season games."
      ),
      call. = FALSE
    )
  }
  
  abbr <- toupper(
    trimws(
      as.character(
        df$source_team_abbr
      )
    )
  )
  
  team_name <- toupper(
    trimws(
      as.character(
        df$source_team_name
      )
    )
  )
  
  nba_team_row <-
    !abbr %in%
    p3s31_excluded_team_abbr() &
    !team_name %in%
    p3s31_excluded_team_names()
  
  excluded_game_ids <- unique(
    as.character(
      excluded_game_ids
    )
  )
  
  source_game_id <- if (
    "source_game_id" %in%
    names(df)
  ) {
    as.character(
      df$source_game_id
    )
  } else {
    rep(
      NA_character_,
      nrow(df)
    )
  }
  
  excluded_game <-
    source_game_id %in%
    excluded_game_ids
  
  excluded_game[
    is.na(
      excluded_game
    )
  ] <- FALSE
  
  keep <-
    regular_season &
    nba_team_row &
    !excluded_game
  
  df[
    keep,
    ,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# Expanded team abbreviation normalization
# ------------------------------------------------------------

p3s3_normalize_team_abbr <- function(x) {
  
  x <- toupper(
    trimws(
      as.character(x)
    )
  )
  
  aliases <- c(
    "BRK" = "BKN",
    "CHO" = "CHA",
    "PHO" = "PHX",
    "GS" = "GSW",
    "NO" = "NOP",
    "NY" = "NYK",
    "SA" = "SAS",
    "UTAH" = "UTA",
    "WSH" = "WAS"
  )
  
  matched <- x %in%
    names(aliases)
  
  x[matched] <-
    unname(
      aliases[
        x[matched]
      ]
    )
  
  x
}


# ------------------------------------------------------------
# External identity lookup
# ------------------------------------------------------------

get_external_player_identity_map <- function(
    con,
    source_name = "hoopR") {
  
  create_external_player_identity_table(
    con
  )
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      source_name,
      source_player_id,
      player_id,
      source_player_name,
      first_seen_season,
      last_seen_season
    FROM external_player_identity
    WHERE source_name = ?
    ",
    params = list(
      source_name
    )
  )
}


# ------------------------------------------------------------
# Upsert external source identity
# ------------------------------------------------------------

upsert_external_player_identity <- function(
    con,
    source_name,
    source_player_id,
    player_id,
    source_player_name = NA_character_,
    season = NA_character_) {
  
  create_external_player_identity_table(
    con
  )
  
  source_name <- as.character(
    source_name
  )
  
  source_player_id <- as.character(
    source_player_id
  )
  
  player_id <- suppressWarnings(
    as.integer(
      player_id
    )
  )
  
  source_player_name <- as.character(
    source_player_name
  )
  
  season <- as.character(
    season
  )
  
  existing <- DBI::dbGetQuery(
    con,
    "
    SELECT
      source_name,
      source_player_id
    FROM external_player_identity
    WHERE source_name = ?
      AND source_player_id = ?
    ",
    params = list(
      source_name,
      source_player_id
    )
  )
  
  if (nrow(existing)) {
    
    DBI::dbExecute(
      con,
      "
      UPDATE external_player_identity
      SET
        player_id = ?,
        source_player_name = ?,
        last_seen_season = ?,
        updated_at = CURRENT_TIMESTAMP
      WHERE source_name = ?
        AND source_player_id = ?
      ",
      params = list(
        player_id,
        source_player_name,
        season,
        source_name,
        source_player_id
      )
    )
    
  } else {
    
    DBI::dbExecute(
      con,
      "
      INSERT INTO external_player_identity (
        source_name,
        source_player_id,
        player_id,
        source_player_name,
        first_seen_season,
        last_seen_season
      )
      VALUES (?, ?, ?, ?, ?, ?)
      ",
      params = list(
        source_name,
        source_player_id,
        player_id,
        source_player_name,
        season,
        season
      )
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Next canonical TBI player_id
# ------------------------------------------------------------

next_tbi_player_id <- function(con) {
  
  value <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(MAX(player_id), 0) + 1 AS next_id
    FROM players
    "
  )$next_id[[1]]
  
  suppressWarnings(
    as.integer(value)
  )
}


# ------------------------------------------------------------
# Safely create missing canonical player
# ------------------------------------------------------------

insert_missing_tbi_player <- function(
    con,
    player_name,
    source_player_id = NA_character_,
    source_name = "hoopR",
    season = NA_character_) {
  
  player_name <- trimws(
    as.character(
      player_name
    )
  )
  
  if (
    is.na(player_name) ||
    !nzchar(player_name)
  ) {
    stop(
      "Cannot insert a player with a blank name."
    )
  }
  
  normalized <- p3s3_normalize_name(
    player_name
  )
  
  existing <- DBI::dbGetQuery(
    con,
    "
    SELECT
      player_id,
      player_name
    FROM players
    "
  )
  
  if (nrow(existing)) {
    
    existing$key <-
      p3s3_normalize_name(
        existing$player_name
      )
    
    hit <- existing[
      existing$key == normalized,
      ,
      drop = FALSE
    ]
    
    if (nrow(hit)) {
      
      player_id <-
        suppressWarnings(
          as.integer(
            hit$player_id[[1]]
          )
        )
      
      upsert_external_player_identity(
        con = con,
        source_name =
          source_name,
        source_player_id =
          source_player_id,
        player_id =
          player_id,
        source_player_name =
          player_name,
        season =
          season
      )
      
      return(
        invisible(
          list(
            player_id =
              player_id,
            created = FALSE,
            matched_existing =
              TRUE
          )
        )
      )
    }
  }
  
  player_id <-
    next_tbi_player_id(
      con
    )
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO players (
      player_id,
      player_name,
      birth_date,
      height_inches,
      weight_lbs,
      primary_position,
      nba_player_id,
      is_active,
      created_at,
      updated_at,
      player_age
    )
    VALUES (
      ?,
      ?,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      1,
      CURRENT_TIMESTAMP,
      CURRENT_TIMESTAMP,
      NULL
    )
    ",
    params = list(
      player_id,
      player_name
    )
  )
  
  upsert_external_player_identity(
    con = con,
    source_name =
      source_name,
    source_player_id =
      source_player_id,
    player_id =
      player_id,
    source_player_name =
      player_name,
    season =
      season
  )
  
  invisible(
    list(
      player_id =
        player_id,
      created = TRUE,
      matched_existing =
        FALSE
    )
  )
}


# ------------------------------------------------------------
# Enhanced player mapping
#
# Resolution order:
#   1. permanent external source ID
#   2. exact normalized canonical name
#   3. unresolved for synchronization / review
# ------------------------------------------------------------

map_phase3_players <- function(
    team_mapped,
    player_map,
    con = NULL,
    source_name = "hoopR") {
  
  d <- team_mapped
  
  d$player_name_key <-
    p3s3_normalize_name(
      d$source_player_name
    )
  
  d$player_id <- NA_integer_
  d$tbi_player_name <- NA_character_
  d$identity_method <- NA_character_
  
  # ----------------------------------------------------------
  # 1. External-ID mapping
  # ----------------------------------------------------------
  
  if (!is.null(con)) {
    
    external <- get_external_player_identity_map(
      con,
      source_name =
        source_name
    )
    
    if (nrow(external)) {
      
      source_index <- match(
        as.character(
          d$source_player_id
        ),
        as.character(
          external$source_player_id
        )
      )
      
      matched_external <-
        !is.na(source_index)
      
      d$player_id[
        matched_external
      ] <-
        suppressWarnings(
          as.integer(
            external$player_id[
              source_index[
                matched_external
              ]
            ]
          )
        )
      
      d$identity_method[
        matched_external
      ] <- "EXTERNAL_ID"
    }
  }
  
  # ----------------------------------------------------------
  # 2. Normalized canonical name
  # ----------------------------------------------------------
  
  unresolved <- is.na(
    d$player_id
  )
  
  if (
    any(unresolved) &&
    nrow(player_map)
  ) {
    
    player_index <- match(
      d$player_name_key[
        unresolved
      ],
      player_map$name_key
    )
    
    resolved_names <-
      !is.na(player_index)
    
    rows <- which(
      unresolved
    )[resolved_names]
    
    d$player_id[rows] <-
      suppressWarnings(
        as.integer(
          player_map$player_id[
            player_index[
              resolved_names
            ]
          ]
        )
      )
    
    d$tbi_player_name[rows] <-
      as.character(
        player_map$player_name[
          player_index[
            resolved_names
          ]
        ]
      )
    
    d$identity_method[rows] <-
      "NORMALIZED_NAME"
  }
  
  # Fill canonical name from current player map for external ID rows.
  if (nrow(player_map)) {
    
    canonical_index <- match(
      d$player_id,
      player_map$player_id
    )
    
    fill_name <-
      !is.na(canonical_index)
    
    d$tbi_player_name[
      fill_name
    ] <-
      player_map$player_name[
        canonical_index[
          fill_name
        ]
      ]
  }
  
  d
}


# ------------------------------------------------------------
# Preview missing-player synchronization
# ------------------------------------------------------------

preview_phase3_player_sync <- function(
    mapped_data) {
  
  unresolved <- mapped_data[
    is.na(
      mapped_data$player_id
    ) &
      !is.na(
        mapped_data$team_id
      ),
    ,
    drop = FALSE
  ]
  
  if (!nrow(unresolved)) {
    return(
      data.frame()
    )
  }
  
  unique(
    unresolved[
      ,
      c(
        "source_player_id",
        "source_player_name",
        "source_team_abbr",
        "season"
      ),
      drop = FALSE
    ]
  )
}


# ------------------------------------------------------------
# Synchronize legitimate missing players
#
# This changes ONLY the players table + external identity map.
# It does NOT load player statistics.
# ------------------------------------------------------------

sync_phase3_missing_players <- function(
    mapped_data,
    con,
    source_name = "hoopR") {
  
  missing <- preview_phase3_player_sync(
    mapped_data
  )
  
  if (!nrow(missing)) {
    
    return(
      list(
        created_players = 0L,
        linked_players = 0L,
        records = data.frame()
      )
    )
  }
  
  # One canonical player per unique external player ID.
  missing <- missing[
    !duplicated(
      missing$source_player_id
    ),
    ,
    drop = FALSE
  ]
  
  results <- lapply(
    seq_len(
      nrow(missing)
    ),
    function(i) {
      
      row <- missing[
        i,
        ,
        drop = FALSE
      ]
      
      inserted <- insert_missing_tbi_player(
        con = con,
        player_name =
          row$source_player_name[[1]],
        source_player_id =
          row$source_player_id[[1]],
        source_name =
          source_name,
        season =
          row$season[[1]]
      )
      
      data.frame(
        source_player_id =
          row$source_player_id[[1]],
        source_player_name =
          row$source_player_name[[1]],
        player_id =
          inserted$player_id,
        created =
          isTRUE(
            inserted$created
          ),
        stringsAsFactors = FALSE
      )
    }
  )
  
  records <- do.call(
    rbind,
    results
  )
  
  list(
    created_players =
      sum(
        records$created,
        na.rm = TRUE
      ),
    linked_players =
      sum(
        !records$created,
        na.rm = TRUE
      ),
    records =
      records
  )
}


# ------------------------------------------------------------
# Store identity links for already-matched players
# ------------------------------------------------------------

persist_phase3_identity_links <- function(
    mapped_data,
    con,
    source_name = "hoopR") {
  
  matched <- mapped_data[
    !is.na(
      mapped_data$player_id
    ) &
      !is.na(
        mapped_data$source_player_id
      ),
    ,
    drop = FALSE
  ]
  
  if (!nrow(matched)) {
    return(
      invisible(0L)
    )
  }
  
  matched <- matched[
    !duplicated(
      matched$source_player_id
    ),
    ,
    drop = FALSE
  ]
  
  for (
    i in seq_len(
      nrow(matched)
    )
  ) {
    
    row <- matched[
      i,
      ,
      drop = FALSE
    ]
    
    upsert_external_player_identity(
      con = con,
      source_name =
        source_name,
      source_player_id =
        row$source_player_id[[1]],
      player_id =
        row$player_id[[1]],
      source_player_name =
        row$source_player_name[[1]],
      season =
        row$season[[1]]
    )
  }
  
  invisible(
    nrow(matched)
  )
}


# ------------------------------------------------------------
# Step 3.1 enhanced validation
# ------------------------------------------------------------

validate_phase3_ingestion_v31 <- function(
    mapped_data,
    excluded_rows = data.frame()) {
  
  validation <-
    validate_phase3_ingestion(
      mapped_data
    )
  
  validation$excluded_event_rows <-
    nrow(
      excluded_rows
    )
  
  validation$unmatched_players_unique <-
    if (
      nrow(
        validation$unmatched_players
      )
    ) {
      length(
        unique(
          validation$
            unmatched_players$
            source_player_id
        )
      )
    } else {
      0L
    }
  
  validation$ready_to_commit <-
    validation$unmatched_team_count == 0 &&
    validation$unmatched_player_count == 0 &&
    validation$duplicate_key_count == 0
  
  validation
}


# ------------------------------------------------------------
# MAIN STEP 3.1 RUNNER
#
# Mode options:
#   "preview"    = no database changes
#   "sync"       = add/link missing players only; stats untouched
#   "commit"     = sync identities and load stats
# ------------------------------------------------------------

run_phase3_step31_ingestion <- function(
    season_year = 2026,
    season_label = "2025-26",
    mode = c(
      "preview",
      "sync",
      "commit"
    )) {
  
  mode <- match.arg(
    mode
  )
  
  phase3_step3_require_packages()
  
  raw_data <-
    download_phase3_nba_player_box(
      season_year =
        season_year
    )
  
  standardized_all <-
    standardize_hoopr_nba_player_box(
      raw_data =
        raw_data,
      season_label =
        season_label
    )
  
  cup_championship_game_ids <-
    p3s31_nba_cup_championship_game_ids(
      season_year =
        season_year
    )
  
  regular_season_flag <-
    p3s31_is_regular_season_type(
      standardized_all$
        source_season_type
    )
  
  cup_championship_flag <-
    as.character(
      standardized_all$
        source_game_id
    ) %in%
    cup_championship_game_ids
  
  cup_championship_flag[
    is.na(
      cup_championship_flag
    )
  ] <- FALSE
  
  event_flag <-
    toupper(
      trimws(
        standardized_all$
          source_team_abbr
      )
    ) %in%
    p3s31_excluded_team_abbr() |
    toupper(
      trimws(
        standardized_all$
          source_team_name
      )
    ) %in%
    p3s31_excluded_team_names()
  
  excluded_nonregular_rows <-
    standardized_all[
      !regular_season_flag,
      ,
      drop = FALSE
    ]
  
  excluded_event_rows <-
    standardized_all[
      regular_season_flag &
        (
          event_flag |
            cup_championship_flag
        ),
      ,
      drop = FALSE
    ]
  
  excluded_nba_cup_championship_rows <-
    standardized_all[
      regular_season_flag &
        cup_championship_flag,
      ,
      drop = FALSE
    ]
  
  # Kept for backward-compatible validation reporting.
  excluded_rows <-
    standardized_all[
      !regular_season_flag |
        event_flag |
        cup_championship_flag,
      ,
      drop = FALSE
    ]
  
  standardized <-
    filter_phase3_regular_nba_rows(
      standardized_all,
      excluded_game_ids =
        cup_championship_game_ids
    )
  
  aggregated <-
    aggregate_phase3_player_season(
      standardized
    )
  
  # ----------------------------------------------------------
  # HARD REGULAR-SEASON SANITY CHECKS
  # ----------------------------------------------------------
  
  team_rows_over_82 <-
    aggregated[
      is.finite(
        suppressWarnings(
          as.numeric(
            aggregated$games_played
          )
        )
      ) &
        suppressWarnings(
          as.numeric(
            aggregated$games_played
          )
        ) > 82,
      ,
      drop = FALSE
    ]
  
  player_game_totals <-
    aggregated |>
    dplyr::group_by(
      .data$source_player_id,
      .data$source_player_name,
      .data$season
    ) |>
    dplyr::summarise(
      games_played =
        sum(
          .data$games_played,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  # Player totals across multiple teams are informational only.
  # A traded player can legitimately exceed 82 official regular-season
  # appearances because the two team schedules do not have to align.
  player_rows_over_82 <-
    player_game_totals[
      player_game_totals$games_played > 82,
      ,
      drop = FALSE
    ]
  
  # A single player/team row above 82 is not valid for an 82-game
  # regular season and therefore remains a hard stop.
  if (
    nrow(team_rows_over_82)
  ) {
    stop(
      paste(
        "Regular-season GP validation failed:",
        "at least one player/team row exceeds 82 games after",
        "season_type, NBA Cup Championship, event, and DNP filtering.",
        "Do not commit until the source rows are reviewed."
      ),
      call. = FALSE
    )
  }
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  create_external_player_identity_table(
    con
  )
  
  team_map <-
    get_phase3_team_map(
      con
    )
  
  player_map <-
    get_phase3_player_map(
      con
    )
  
  mapped <-
    map_phase3_teams(
      aggregated =
        aggregated,
      team_map =
        team_map
    )
  
  mapped <-
    map_phase3_players(
      team_mapped =
        mapped,
      player_map =
        player_map,
      con =
        con,
      source_name =
        "hoopR"
    )
  
  initial_validation <-
    validate_phase3_ingestion_v31(
      mapped,
      excluded_rows =
        excluded_rows
    )
  
  result <- list(
    season_year =
      season_year,
    season_label =
      season_label,
    mode =
      mode,
    raw_rows =
      nrow(raw_data),
    regular_source_rows =
      nrow(standardized),
    excluded_nonregular_rows =
      nrow(excluded_nonregular_rows),
    excluded_event_rows =
      nrow(excluded_event_rows),
    excluded_nba_cup_championship_rows =
      nrow(
        excluded_nba_cup_championship_rows
      ),
    excluded_nba_cup_championship_game_ids =
      cup_championship_game_ids,
    max_team_games_played =
      if (nrow(aggregated)) {
        max(
          aggregated$games_played,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    max_player_games_played =
      if (nrow(player_game_totals)) {
        max(
          player_game_totals$games_played,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    aggregated_regular_rows =
      nrow(aggregated),
    initial_validation =
      initial_validation,
    missing_player_preview =
      preview_phase3_player_sync(
        mapped
      )
  )
  
  # ----------------------------------------------------------
  # PREVIEW MODE
  # ----------------------------------------------------------
  
  if (
    identical(
      mode,
      "preview"
    )
  ) {
    
    result$status <-
      "PREVIEW ONLY — DATABASE NOT CHANGED"
    
    result$validation <-
      initial_validation
    
    result$mapped_preview <-
      mapped
    
    return(result)
  }
  
  # ----------------------------------------------------------
  # Persist known identity links first
  # ----------------------------------------------------------
  
  linked_existing <-
    persist_phase3_identity_links(
      mapped_data =
        mapped,
      con =
        con,
      source_name =
        "hoopR"
    )
  
  # ----------------------------------------------------------
  # Add genuinely missing canonical players
  # ----------------------------------------------------------
  
  sync_result <-
    sync_phase3_missing_players(
      mapped_data =
        mapped,
      con =
        con,
      source_name =
        "hoopR"
    )
  
  # Rebuild maps after synchronization.
  player_map <-
    get_phase3_player_map(
      con
    )
  
  mapped <-
    map_phase3_players(
      team_mapped =
        mapped,
      player_map =
        player_map,
      con =
        con,
      source_name =
        "hoopR"
    )
  
  final_validation <-
    validate_phase3_ingestion_v31(
      mapped,
      excluded_rows =
        excluded_rows
    )
  
  result$identity_links_persisted <-
    linked_existing
  
  result$sync_result <-
    sync_result
  
  result$validation <-
    final_validation
  
  # ----------------------------------------------------------
  # SYNC MODE: stop before stats
  # ----------------------------------------------------------
  
  if (
    identical(
      mode,
      "sync"
    )
  ) {
    
    result$status <-
      "IDENTITIES SYNCHRONIZED — STATS NOT COMMITTED"
    
    return(result)
  }
  
  # ----------------------------------------------------------
  # COMMIT MODE
  # ----------------------------------------------------------
  
  if (
    !isTRUE(
      final_validation$
      ready_to_commit
    )
  ) {
    
    stop(
      paste(
        "Commit blocked.",
        "Final validation is not clean.",
        "Review result$validation before loading stats."
      )
    )
  }
  
  matched <- mapped[
    !is.na(
      mapped$player_id
    ) &
      !is.na(
        mapped$team_id
      ),
    ,
    drop = FALSE
  ]
  
  import_frame <-
    build_phase3_step1_import_frame(
      matched
    )
  
  step1_result <-
    upsert_player_season_stats(
      import_frame,
      con = con
    )
  
  step2_result <- NULL
  
  if (
    exists(
      "build_advanced_metrics_from_player_stats",
      mode = "function"
    )
  ) {
    
    step2_result <-
      build_advanced_metrics_from_player_stats(
        season =
          season_label,
        con =
          con
      )
  }
  
  result$status <-
    "COMMITTED"
  
  result$step1_result <-
    step1_result
  
  result$step2_result <-
    step2_result
  
  result$import_rows <-
    nrow(
      import_frame
    )
  
  result
}


# ------------------------------------------------------------
# Step 3.1 console report
# ------------------------------------------------------------

print_phase3_step31_report <- function(
    result) {
  
  validation <- result$validation
  
  cat(
    "\n",
    "====================================================\n",
    "PHASE 3 STEP 3.1 — IDENTITY RESOLUTION REPORT\n",
    "====================================================\n",
    sep = ""
  )
  
  cat(
    "Season: ",
    result$season_label,
    "\n",
    sep = ""
  )
  
  cat(
    "Mode: ",
    result$mode,
    "\n",
    sep = ""
  )
  
  cat(
    "Status: ",
    result$status,
    "\n\n",
    sep = ""
  )
  
  cat(
    "Raw rows: ",
    result$raw_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Excluded non-regular-season rows: ",
    result$excluded_nonregular_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Excluded event rows: ",
    result$excluded_event_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Excluded NBA Cup Championship rows: ",
    if (
      is.null(
        result$excluded_nba_cup_championship_rows
      )
    ) {
      0L
    } else {
      result$excluded_nba_cup_championship_rows
    },
    "\n",
    sep = ""
  )
  
  cup_ids <- if (
    is.null(
      result$excluded_nba_cup_championship_game_ids
    )
  ) {
    character()
  } else {
    result$excluded_nba_cup_championship_game_ids
  }
  
  cat(
    "Excluded NBA Cup Championship game IDs: ",
    if (length(cup_ids)) {
      paste(
        cup_ids,
        collapse = ", "
      )
    } else {
      "NONE FOUND"
    },
    "\n",
    sep = ""
  )
  
  cat(
    "Regular-season source rows retained: ",
    result$regular_source_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Max team-row GP: ",
    result$max_team_games_played,
    "\n",
    sep = ""
  )
  
  cat(
    "Max player-season GP across teams (informational; trades can exceed 82): ",
    result$max_player_games_played,
    "\n",
    sep = ""
  )
  
  cat(
    "Regular NBA player/team rows: ",
    result$aggregated_regular_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Matched rows: ",
    validation$matched_rows,
    "\n",
    sep = ""
  )
  
  cat(
    "Match rate: ",
    sprintf(
      "%.1f%%",
      100 *
        validation$match_rate
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Unmatched players: ",
    validation$unmatched_player_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Unmatched teams: ",
    validation$unmatched_team_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Duplicate keys: ",
    validation$duplicate_key_count,
    "\n",
    sep = ""
  )
  
  if (
    !is.null(
      result$sync_result
    )
  ) {
    
    cat(
      "New TBI players created: ",
      result$sync_result$
        created_players,
      "\n",
      sep = ""
    )
    
    cat(
      "Existing players linked: ",
      result$sync_result$
        linked_players,
      "\n",
      sep = ""
    )
  }
  
  cat(
    "Ready to commit: ",
    validation$ready_to_commit,
    "\n",
    sep = ""
  )
  
  invisible(
    result
  )
}


# ------------------------------------------------------------
# Step 3.1 identity health check
# ------------------------------------------------------------

phase3_step31_healthcheck <- function() {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  tables <- DBI::dbListTables(
    con
  )
  
  identity_exists <-
    "external_player_identity" %in%
    tables
  
  identity_rows <- if (
    identity_exists
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM external_player_identity
      "
    )$n[[1]]
  } else {
    0L
  }
  
  player_rows <-
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM players
      "
    )$n[[1]]
  
  list(
    phase = "Phase 3",
    step =
      "Step 3.1 — NBA Identity Resolution + Roster Synchronization",
    status = if (
      identity_exists
    ) {
      "READY"
    } else {
      "AWAITING FIRST SYNC"
    },
    players_table_rows =
      player_rows,
    external_identity_rows =
      identity_rows
  )
}

# ------------------------------------------------------------
# Phase 3 SQLite commit preflight
# ------------------------------------------------------------

phase3_sqlite_commit_preflight <- function() {
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  required <- c(
    "upsert_player_season_stats",
    "upsert_player_advanced_metrics",
    "run_phase3_step31_ingestion"
  )
  
  available <- vapply(
    required,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )
  
  list(
    status = if (
      all(available)
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    connection_class =
      class(con),
    required_functions =
      available,
    merge_sql_required =
      FALSE,
    commit_method =
      "SQLITE TRANSACTIONAL DELETE + INSERT"
  )
}