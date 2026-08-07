# ============================================================
# Thompson's Basketball Intelligence
# Player Management helpers
# ============================================================

player_manager_valid_positions <- function() {
  c("PG", "SG", "SF", "PF", "C")
}

player_manager_clean_positions <- function(x) {
  values <- toupper(trimws(as.character(x %||% character(0))))
  values <- values[!is.na(values) & nzchar(values)]
  unique(values[values %in% player_manager_valid_positions()])
}

get_player_manager_pool <- function(team_value, season, db_path = NULL) {
  path <- resolve_tbi_db_path(
    db_path %||% file.path("inst", "database", "tbi.sqlite")
  )
  con <- connect_db(db_path = path, read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)

  DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_id,
      p.player_name,
      p.player_age,
      p.birth_date,
      p.height_inches,
      p.weight_lbs,
      p.primary_position,
      p.is_active,
      rh.team_id,
      t.team_name,
      t.abbreviation,
      rh.season,
      rh.roster_status,
      rh.two_way_flag,
      rh.jersey_number,
      GROUP_CONCAT(pp.position, ', ') AS eligible_positions,
      MAX(COALESCE(cy.cap_hit, cy.base_salary, 0)) AS current_salary,
      MAX(c.contract_type) AS contract_type,
      MAX(c.contract_end_season) AS contract_end_season,
      MAX(c.notes) AS contract_notes
    FROM roster_history rh
    INNER JOIN players p ON p.player_id = rh.player_id
    INNER JOIN teams t ON t.team_id = rh.team_id
    LEFT JOIN player_positions pp ON pp.player_id = p.player_id
    LEFT JOIN contract_years cy
      ON cy.player_id = p.player_id
      AND cy.team_id = rh.team_id
      AND cy.season = rh.season
    LEFT JOIN contracts c ON c.contract_id = cy.contract_id
    WHERE
      (t.team_name = ? OR t.abbreviation = ? OR CAST(t.team_id AS TEXT) = CAST(? AS TEXT))
      AND rh.season = ?
      AND rh.end_date IS NULL
    GROUP BY
      p.player_id, p.player_name, p.player_age, p.birth_date,
      p.height_inches, p.weight_lbs, p.primary_position, p.is_active,
      rh.team_id, t.team_name, t.abbreviation, rh.season,
      rh.roster_status, rh.two_way_flag, rh.jersey_number
    ORDER BY p.player_name
    ",
    params = list(team_value, team_value, team_value, season)
  )
}

get_player_manager_record <- function(player_id, team_value, season, db_path = NULL) {
  pool <- get_player_manager_pool(team_value, season, db_path = db_path)
  record <- pool[pool$player_id == as.integer(player_id), , drop = FALSE]
  if (!nrow(record)) return(NULL)
  record[1, , drop = FALSE]
}

save_player_manager_record <- function(
    player_id,
    team_value,
    season,
    player_name,
    player_age = NA_integer_,
    birth_date = NULL,
    height_inches = NA_real_,
    weight_lbs = NA_real_,
    primary_position,
    eligible_positions,
    roster_status = "Active",
    two_way_flag = FALSE,
    jersey_number = NULL,
    is_active = TRUE,
    db_path = NULL) {

  path <- resolve_tbi_db_path(
    db_path %||% file.path("inst", "database", "tbi.sqlite")
  )
  con <- connect_db(db_path = path, read_only = FALSE)
  on.exit(disconnect_db(con), add = TRUE)

  player_id <- as.integer(player_id)
  player_name <- trimws(as.character(player_name %||% ""))
  primary_position <- toupper(trimws(as.character(primary_position %||% "")))
  eligible_positions <- player_manager_clean_positions(eligible_positions)
  roster_status <- trimws(as.character(roster_status %||% "Active"))
  jersey_number <- trimws(as.character(jersey_number %||% ""))
  birth_date <- trimws(as.character(birth_date %||% ""))

  if (!nzchar(player_name)) stop("Player name is required.", call. = FALSE)
  if (!primary_position %in% player_manager_valid_positions()) {
    stop("Choose a valid primary position.", call. = FALSE)
  }
  if (!length(eligible_positions)) eligible_positions <- primary_position
  if (!primary_position %in% eligible_positions) {
    eligible_positions <- c(primary_position, eligible_positions)
  }

  age <- suppressWarnings(as.integer(player_age))
  height <- suppressWarnings(as.numeric(height_inches))
  weight <- suppressWarnings(as.numeric(weight_lbs))
  if (!is.na(age) && (age < 18 || age > 50)) stop("Age must be between 18 and 50.", call. = FALSE)
  if (!is.na(height) && (height < 60 || height > 96)) stop("Height must be between 60 and 96 inches.", call. = FALSE)
  if (!is.na(weight) && (weight < 130 || weight > 400)) stop("Weight must be between 130 and 400 pounds.", call. = FALSE)

  team <- DBI::dbGetQuery(
    con,
    "SELECT team_id FROM teams WHERE team_name = ? OR abbreviation = ? OR CAST(team_id AS TEXT) = CAST(? AS TEXT) LIMIT 1",
    params = list(team_value, team_value, team_value)
  )
  if (!nrow(team)) stop("Selected team was not found.", call. = FALSE)
  team_id <- as.integer(team$team_id[[1]])

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(
      con,
      "
      UPDATE players
      SET player_name = ?, player_age = ?, birth_date = ?, height_inches = ?,
          weight_lbs = ?, primary_position = ?, is_active = ?, updated_at = CURRENT_TIMESTAMP
      WHERE player_id = ?
      ",
      params = list(
        player_name,
        if (is.na(age)) NA_integer_ else age,
        if (nzchar(birth_date)) birth_date else NA_character_,
        if (is.na(height)) NA_real_ else height,
        if (is.na(weight)) NA_real_ else weight,
        primary_position,
        as.integer(isTRUE(is_active)),
        player_id
      )
    )

    DBI::dbExecute(con, "DELETE FROM player_positions WHERE player_id = ?", params = list(player_id))
    for (i in seq_along(eligible_positions)) {
      DBI::dbExecute(
        con,
        "INSERT INTO player_positions (player_id, position, eligibility_rank, is_primary) VALUES (?, ?, ?, ?)",
        params = list(player_id, eligible_positions[[i]], as.integer(i), as.integer(eligible_positions[[i]] == primary_position))
      )
    }

    DBI::dbExecute(
      con,
      "
      UPDATE roster_history
      SET roster_status = ?, two_way_flag = ?, jersey_number = ?
      WHERE player_id = ? AND team_id = ? AND season = ? AND end_date IS NULL
      ",
      params = list(
        roster_status,
        as.integer(isTRUE(two_way_flag)),
        if (nzchar(jersey_number)) jersey_number else NA_character_,
        player_id,
        team_id,
        season
      )
    )
  })

  invisible(list(saved = TRUE, player_id = player_id))
}

player_manager_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  value <- trimws(as.character(x[[1]]))
  if (!nzchar(value)) default else value
}

player_manager_money <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value) || is.na(value[[1]])) return("—")
  value <- value[[1]]
  if (abs(value) >= 1e6) sprintf("$%.1fM", value / 1e6) else paste0("$", format(round(value), big.mark = ",", scientific = FALSE))
}

player_manager_height <- function(x) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (!length(value) || is.na(value[[1]])) return("—")
  paste0(value[[1]] %/% 12, "'", value[[1]] %% 12, '"')
}
