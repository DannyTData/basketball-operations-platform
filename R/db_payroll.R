# ------------------------------------------------------------
# TBI Payroll Database Helpers
# ------------------------------------------------------------


#' Connect to the TBI SQLite database
#'
#' @noRd
tbi_db_connect <- function() {
  
  database_path <- system.file(
    "database/tbi.sqlite",
    package = "basketballops",
    mustWork = TRUE
  )
  
  DBI::dbConnect(
    RSQLite::SQLite(),
    database_path
  )
}


#' Get payroll summary for one team
#'
#' @param team_name NBA team name.
#' @param season NBA season.
#'
#' @noRd
get_team_payroll <- function(team_name,
                             season = "2026-27") {
  
  con <- tbi_db_connect()
  
  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
        t.team_name,
        COUNT(cy.player_id) AS contracts,
        SUM(cy.cap_hit) AS payroll,
        SUM(cy.cap_hit) AS cap_hit
    FROM contract_years cy
    JOIN teams t
      ON cy.team_id = t.team_id
    WHERE
        t.team_name = ?
        AND cy.season = ?
    GROUP BY
        t.team_name;
    ",
    params = list(
      team_name,
      season
    )
  )
}


#' Get league-wide payroll rankings
#'
#' @param season NBA season.
#'
#' @noRd
get_payroll_rankings <- function(season = "2026-27") {
  
  con <- tbi_db_connect()
  
  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )
  
  rankings <- DBI::dbGetQuery(
    con,
    "
    SELECT
        t.team_name,
        COUNT(cy.player_id) AS contracts,
        SUM(cy.cap_hit) AS payroll,
        SUM(cy.cap_hit) AS cap_hit
    FROM contract_years cy
    JOIN teams t
      ON cy.team_id = t.team_id
    WHERE
        cy.season = ?
    GROUP BY
        t.team_id,
        t.team_name
    ORDER BY
        payroll DESC;
    ",
    params = list(season)
  )
  
  rankings$rank <- seq_len(
    nrow(rankings)
  )
  
  rankings
}


#' Get the highest-paid player for one team
#'
#' @param team_name NBA team name.
#' @param season NBA season.
#'
#' @noRd
get_highest_paid_player <- function(team_name,
                                    season = "2026-27") {
  
  con <- tbi_db_connect()
  
  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
        p.player_name,
        cy.cap_hit
    FROM contract_years cy
    JOIN players p
      ON cy.player_id = p.player_id
    JOIN teams t
      ON cy.team_id = t.team_id
    WHERE
        t.team_name = ?
        AND cy.season = ?
    ORDER BY
        cy.cap_hit DESC
    LIMIT 1;
    ",
    params = list(
      team_name,
      season
    )
  )
}


#' Get one team's payroll rank
#'
#' @param team_name NBA team name.
#' @param season NBA season.
#'
#' @noRd
get_team_payroll_rank <- function(team_name,
                                  season = "2026-27") {
  
  rankings <- get_payroll_rankings(
    season = season
  )
  
  if (
    is.null(rankings) ||
    nrow(rankings) == 0
  ) {
    return(NA_integer_)
  }
  
  team_row <- rankings[
    rankings$team_name == team_name,
    ,
    drop = FALSE
  ]
  
  if (nrow(team_row) == 0) {
    return(NA_integer_)
  }
  
  as.integer(
    team_row$rank[[1]]
  )
}