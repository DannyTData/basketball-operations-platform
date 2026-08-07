# ============================================================
# Thompson's Basketball Intelligence
# Phase 1: Salary Cap Calculation Engine
# ============================================================

#' Convert a value to a safe numeric scalar
#' @noRd
cap_number <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value) || is.na(value[[1]]) || !is.finite(value[[1]])) default else value[[1]]
}

#' Calculate a team's cap position from contract-year commitments
#'
#' This function is intentionally independent of Shiny and the database so the
#' same rules can be tested and reused by the executive, salary-cap, trade, and
#' long-range planning modules.
#'
#' @param contract_years Data frame containing at least `cap_hit`. Optional
#'   fields include `guaranteed_amount`, `likely_incentives`, `dead_cap`, and
#'   `two_way_flag`.
#' @param thresholds One-row data frame or named list containing salary_cap,
#'   luxury_tax, first_apron, second_apron, and minimum_team_salary.
#' @param include_two_way Whether two-way rows should be included in team salary.
#' @param additional_team_salary Explicit additional team salary not represented
#'   in the contract-year rows, such as a manually supplied cap hold or roster
#'   charge. Defaults to zero and is surfaced separately in the result.
#' @return A named list describing the team's current cap position.
#' @noRd
calculate_team_cap_position <- function(contract_years,
                                        thresholds,
                                        include_two_way = FALSE,
                                        additional_team_salary = 0) {
  if (is.null(contract_years)) contract_years <- data.frame()
  if (!is.data.frame(contract_years)) {
    stop("contract_years must be a data frame.", call. = FALSE)
  }
  if (!"cap_hit" %in% names(contract_years)) {
    stop("contract_years must contain a cap_hit column.", call. = FALSE)
  }

  threshold_value <- function(name) {
    if (is.data.frame(thresholds)) {
      if (!nrow(thresholds) || !name %in% names(thresholds)) return(NA_real_)
      return(cap_number(thresholds[[name]][[1]], NA_real_))
    }
    if (is.list(thresholds) && name %in% names(thresholds)) {
      return(cap_number(thresholds[[name]], NA_real_))
    }
    NA_real_
  }

  salary_cap <- threshold_value("salary_cap")
  luxury_tax <- threshold_value("luxury_tax")
  first_apron <- threshold_value("first_apron")
  second_apron <- threshold_value("second_apron")
  minimum_team_salary <- threshold_value("minimum_team_salary")

  required_thresholds <- c(
    salary_cap = salary_cap,
    luxury_tax = luxury_tax,
    first_apron = first_apron,
    second_apron = second_apron
  )
  if (any(is.na(required_thresholds) | required_thresholds <= 0)) {
    stop("Salary cap, tax, first-apron, and second-apron thresholds are required.", call. = FALSE)
  }
  if (!(salary_cap <= luxury_tax && luxury_tax <= first_apron && first_apron <= second_apron)) {
    stop("Cap thresholds are not in ascending order.", call. = FALSE)
  }

  rows <- contract_years
  if (!include_two_way && "two_way_flag" %in% names(rows)) {
    two_way <- suppressWarnings(as.integer(rows$two_way_flag))
    two_way[is.na(two_way)] <- 0L
    rows <- rows[two_way != 1L, , drop = FALSE]
  }

  safe_sum <- function(column) {
    if (!column %in% names(rows)) return(0)
    sum(suppressWarnings(as.numeric(rows[[column]])), na.rm = TRUE)
  }

  active_salary <- safe_sum("cap_hit")
  guaranteed_salary <- safe_sum("guaranteed_amount")
  likely_incentives <- safe_sum("likely_incentives")
  dead_cap <- safe_sum("dead_cap")
  additional_team_salary <- max(0, cap_number(additional_team_salary, 0))
  team_salary <- active_salary + additional_team_salary

  distance <- function(threshold) threshold - team_salary
  over_by <- function(threshold) max(team_salary - threshold, 0)
  room_below <- function(threshold) max(threshold - team_salary, 0)

  status <- if (team_salary >= second_apron) {
    "Above Second Apron"
  } else if (team_salary >= first_apron) {
    "Above First Apron"
  } else if (team_salary >= luxury_tax) {
    "Tax Team"
  } else if (team_salary >= salary_cap) {
    "Over Cap"
  } else {
    "Below Cap"
  }

  status_level <- switch(
    status,
    "Below Cap" = "favorable",
    "Over Cap" = "neutral",
    "Tax Team" = "caution",
    "Above First Apron" = "restricted",
    "Above Second Apron" = "severely_restricted"
  )

  contract_count <- nrow(rows)
  cap_hits <- suppressWarnings(as.numeric(rows$cap_hit))
  cap_hits[is.na(cap_hits)] <- 0
  top3_salary <- sum(utils::head(sort(cap_hits, decreasing = TRUE), 3), na.rm = TRUE)

  list(
    team_salary = team_salary,
    active_salary = active_salary,
    guaranteed_salary = guaranteed_salary,
    non_guaranteed_exposure = max(active_salary - guaranteed_salary, 0),
    likely_incentives = likely_incentives,
    dead_cap = dead_cap,
    additional_team_salary = additional_team_salary,
    contract_count = contract_count,
    top3_salary = top3_salary,
    top3_share = if (team_salary > 0) top3_salary / team_salary else 0,
    salary_cap = salary_cap,
    luxury_tax = luxury_tax,
    first_apron = first_apron,
    second_apron = second_apron,
    minimum_team_salary = minimum_team_salary,
    cap_room = room_below(salary_cap),
    over_cap_by = over_by(salary_cap),
    tax_room = room_below(luxury_tax),
    over_tax_by = over_by(luxury_tax),
    first_apron_room = room_below(first_apron),
    over_first_apron_by = over_by(first_apron),
    second_apron_room = room_below(second_apron),
    over_second_apron_by = over_by(second_apron),
    minimum_salary_shortfall = if (is.na(minimum_team_salary)) 0 else room_below(minimum_team_salary),
    cap_distance = distance(salary_cap),
    tax_distance = distance(luxury_tax),
    first_apron_distance = distance(first_apron),
    second_apron_distance = distance(second_apron),
    is_over_cap = team_salary >= salary_cap,
    is_tax_team = team_salary >= luxury_tax,
    is_above_first_apron = team_salary >= first_apron,
    is_above_second_apron = team_salary >= second_apron,
    status = status,
    status_level = status_level,
    assumptions = c(
      "Team salary uses loaded contract-year cap hits.",
      "Two-way rows are excluded unless include_two_way is TRUE.",
      "Cap holds and incomplete-roster charges are excluded unless supplied as additional_team_salary."
    )
  )
}

#' Load contract-year inputs used by the cap engine
#' @noRd
get_team_cap_inputs <- function(team_value, season, db_path = NULL) {
  path <- resolve_tbi_db_path(db_path %||% file.path("inst", "database", "tbi.sqlite"))
  con <- connect_db(db_path = path, read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)

  DBI::dbGetQuery(
    con,
    "
    SELECT
      cy.contract_year_id,
      cy.player_id,
      cy.team_id,
      cy.season,
      cy.base_salary,
      cy.cap_hit,
      cy.guaranteed_amount,
      cy.option_type,
      cy.likely_incentives,
      cy.unlikely_incentives,
      cy.dead_cap,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag,
      p.player_name,
      p.primary_position,
      c.contract_type,
      c.contract_end_season,
      c.free_agent_year,
      c.bird_rights
    FROM contract_years cy
    INNER JOIN players p ON p.player_id = cy.player_id
    INNER JOIN teams t ON t.team_id = cy.team_id
    LEFT JOIN contracts c ON c.contract_id = cy.contract_id
    LEFT JOIN roster_history rh
      ON rh.player_id = cy.player_id
      AND rh.team_id = cy.team_id
      AND rh.season = cy.season
    WHERE (t.team_name = ? OR t.abbreviation = ? OR CAST(t.team_id AS TEXT) = CAST(? AS TEXT))
      AND cy.season = ?
    ORDER BY cy.cap_hit DESC, p.player_name
    ",
    params = list(team_value, team_value, team_value, season)
  )
}

#' Calculate a database-backed team cap summary
#' @noRd
get_team_cap_summary <- function(team_value, season, db_path = NULL,
                                 include_two_way = FALSE,
                                 additional_team_salary = 0) {
  contracts <- get_team_cap_inputs(team_value, season, db_path = db_path)
  thresholds <- get_cap_thresholds(season, db_path = db_path)
  if (!nrow(thresholds)) {
    stop(sprintf("Cap thresholds are not available for %s.", season), call. = FALSE)
  }

  summary <- calculate_team_cap_position(
    contract_years = contracts,
    thresholds = thresholds,
    include_two_way = include_two_way,
    additional_team_salary = additional_team_salary
  )
  summary$team <- team_value
  summary$season <- season
  summary$contracts <- contracts
  summary$thresholds <- thresholds
  summary
}
