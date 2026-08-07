# ============================================================
# Thompson's Basketball Intelligence
# Phase 3: Trade Salary-Matching Engine
# ============================================================

# This engine provides a reusable first-pass CBA salary-matching screen.
# It is intentionally independent of Shiny. It does not yet attempt to resolve
# every transaction-specific restriction (for example sign-and-trade timing,
# recently signed/traded restrictions, Base Year Compensation, poison-pill
# treatment, trade kickers, or use of a specific traded-player exception).

#' Convert a value to a safe numeric scalar
#' @noRd
trade_number <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(x))
  if (!length(value) || is.na(value[[1]]) || !is.finite(value[[1]])) {
    default
  } else {
    value[[1]]
  }
}

#' Sum salary values safely
#' @noRd
trade_salary_sum <- function(x) {
  if (is.null(x) || !length(x)) return(0)
  sum(suppressWarnings(as.numeric(x)), na.rm = TRUE)
}

#' Format a salary value for explanations
#' @noRd
trade_money <- function(x) {
  x <- trade_number(x, 0)
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

#' Resolve the cap status used by trade matching
#'
#' @param team_salary Team salary before the proposed trade.
#' @param thresholds One-row data frame or named list with salary_cap,
#'   luxury_tax, first_apron, and second_apron.
#' @return A named list describing the team's pre-trade salary status.
#' @noRd
resolve_trade_team_status <- function(team_salary, thresholds) {
  threshold_value <- function(name) {
    if (is.data.frame(thresholds)) {
      if (!nrow(thresholds) || !name %in% names(thresholds)) return(NA_real_)
      return(trade_number(thresholds[[name]][[1]], NA_real_))
    }
    
    if (is.list(thresholds) && name %in% names(thresholds)) {
      return(trade_number(thresholds[[name]], NA_real_))
    }
    
    NA_real_
  }
  
  salary_cap <- threshold_value("salary_cap")
  luxury_tax <- threshold_value("luxury_tax")
  first_apron <- threshold_value("first_apron")
  second_apron <- threshold_value("second_apron")
  
  required <- c(
    salary_cap = salary_cap,
    luxury_tax = luxury_tax,
    first_apron = first_apron,
    second_apron = second_apron
  )
  
  if (any(is.na(required) | required <= 0)) {
    stop(
      "Salary cap, tax, first-apron, and second-apron thresholds are required.",
      call. = FALSE
    )
  }
  
  if (!(salary_cap <= luxury_tax &&
        luxury_tax <= first_apron &&
        first_apron <= second_apron)) {
    stop("Cap thresholds are not in ascending order.", call. = FALSE)
  }
  
  team_salary <- max(0, trade_number(team_salary, 0))
  
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
  
  list(
    team_salary = team_salary,
    salary_cap = salary_cap,
    luxury_tax = luxury_tax,
    first_apron = first_apron,
    second_apron = second_apron,
    status = status,
    is_below_cap = team_salary < salary_cap,
    is_tax_team = team_salary >= luxury_tax,
    is_above_first_apron = team_salary >= first_apron,
    is_above_second_apron = team_salary >= second_apron
  )
}

#' Calculate maximum incoming salary under the engine's matching screen
#'
#' @param outgoing_salary Salary sent out by the team.
#' @param team_salary Team salary before the proposed trade.
#' @param thresholds Current season cap thresholds.
#' @param available_cap_room Optional cap room that may be used to absorb salary.
#'   When NULL, it is derived from salary_cap minus team_salary.
#' @param rule_overrides Optional named list for testing or future CBA updates.
#' @return A named list containing the matching rule and maximum incoming salary.
#' @noRd
calculate_maximum_incoming_salary <- function(outgoing_salary,
                                              team_salary,
                                              thresholds,
                                              available_cap_room = NULL,
                                              rule_overrides = list()) {
  outgoing_salary <- max(0, trade_number(outgoing_salary, 0))
  status <- resolve_trade_team_status(team_salary, thresholds)
  
  rules <- modifyList(
    list(
      low_band_ceiling = 7500000,
      middle_band_ceiling = 29000000,
      low_band_multiplier = 2,
      low_band_addition = 250000,
      middle_band_addition = 7500000,
      high_band_multiplier = 1.25,
      high_band_addition = 250000,
      tax_team_multiplier = 1.25,
      tax_team_addition = 250000,
      first_apron_multiplier = 1.10,
      first_apron_addition = 250000,
      second_apron_multiplier = 1,
      second_apron_addition = 0
    ),
    rule_overrides
  )
  
  cap_room <- if (is.null(available_cap_room)) {
    max(status$salary_cap - status$team_salary, 0)
  } else {
    max(0, trade_number(available_cap_room, 0))
  }
  
  if (status$is_above_second_apron) {
    maximum <- outgoing_salary * rules$second_apron_multiplier +
      rules$second_apron_addition
    rule_name <- "Second-apron dollar-for-dollar matching"
    rule_code <- "SECOND_APRON_100"
    
  } else if (status$is_above_first_apron) {
    maximum <- outgoing_salary * rules$first_apron_multiplier +
      rules$first_apron_addition
    rule_name <- "First-apron restricted matching"
    rule_code <- "FIRST_APRON_110"
    
  } else if (status$is_tax_team) {
    maximum <- outgoing_salary * rules$tax_team_multiplier +
      rules$tax_team_addition
    rule_name <- "Tax-team matching"
    rule_code <- "TAX_125"
    
  } else if (status$is_below_cap && cap_room > 0) {
    # A room team may absorb salary into available cap room. The outgoing
    # salary is added because sending salary creates additional room.
    maximum <- outgoing_salary + cap_room
    rule_name <- "Cap-room absorption"
    rule_code <- "CAP_ROOM"
    
  } else if (outgoing_salary <= rules$low_band_ceiling) {
    maximum <- outgoing_salary * rules$low_band_multiplier +
      rules$low_band_addition
    rule_name <- "Standard low outgoing-salary band"
    rule_code <- "STANDARD_LOW"
    
  } else if (outgoing_salary <= rules$middle_band_ceiling) {
    maximum <- outgoing_salary + rules$middle_band_addition
    rule_name <- "Standard middle outgoing-salary band"
    rule_code <- "STANDARD_MIDDLE"
    
  } else {
    maximum <- outgoing_salary * rules$high_band_multiplier +
      rules$high_band_addition
    rule_name <- "Standard high outgoing-salary band"
    rule_code <- "STANDARD_HIGH"
  }
  
  list(
    maximum_incoming_salary = max(0, maximum),
    outgoing_salary = outgoing_salary,
    cap_room_used = if (identical(rule_code, "CAP_ROOM")) cap_room else 0,
    rule_name = rule_name,
    rule_code = rule_code,
    pre_trade_status = status$status,
    rules = rules
  )
}

#' Evaluate one team's side of a proposed trade
#'
#' @param outgoing_salary Total matching salary sent out by the team.
#' @param incoming_salary Total matching salary received by the team.
#' @param team_salary Team salary before the proposed trade.
#' @param thresholds Current season cap thresholds.
#' @param available_cap_room Optional cap room available before the trade.
#' @param rule_overrides Optional matching-rule overrides.
#' @param tolerance Small rounding tolerance in dollars.
#' @return A named list containing the matching result and payroll impact.
#' @noRd
evaluate_team_trade <- function(outgoing_salary,
                                incoming_salary,
                                team_salary,
                                thresholds,
                                available_cap_room = NULL,
                                rule_overrides = list(),
                                tolerance = 1) {
  outgoing_salary <- max(0, trade_number(outgoing_salary, 0))
  incoming_salary <- max(0, trade_number(incoming_salary, 0))
  team_salary <- max(0, trade_number(team_salary, 0))
  
  matching <- calculate_maximum_incoming_salary(
    outgoing_salary = outgoing_salary,
    team_salary = team_salary,
    thresholds = thresholds,
    available_cap_room = available_cap_room,
    rule_overrides = rule_overrides
  )
  
  maximum <- matching$maximum_incoming_salary
  salary_matching_pass <- incoming_salary <= maximum + tolerance
  post_trade_salary <- max(0, team_salary - outgoing_salary + incoming_salary)
  post_status <- resolve_trade_team_status(post_trade_salary, thresholds)
  
  # Second-apron teams may not aggregate multiple player salaries in a trade.
  # Player-count information is evaluated separately by evaluate_trade_side().
  result_status <- if (salary_matching_pass) "PASS" else "FAIL"
  difference_to_limit <- maximum - incoming_salary
  
  explanation <- if (salary_matching_pass) {
    paste0(
      "Salary matching passes under ", matching$rule_name, ". ",
      "The team may receive up to ", trade_money(maximum),
      " and is receiving ", trade_money(incoming_salary), "."
    )
  } else {
    paste0(
      "Salary matching fails under ", matching$rule_name, ". ",
      "Incoming salary exceeds the calculated limit by ",
      trade_money(abs(difference_to_limit)), "."
    )
  }
  
  list(
    status = result_status,
    is_salary_match = salary_matching_pass,
    outgoing_salary = outgoing_salary,
    incoming_salary = incoming_salary,
    salary_delta = incoming_salary - outgoing_salary,
    maximum_incoming_salary = maximum,
    room_to_matching_limit = difference_to_limit,
    matching_rule = matching$rule_name,
    matching_rule_code = matching$rule_code,
    cap_room_used = matching$cap_room_used,
    pre_trade_salary = team_salary,
    post_trade_salary = post_trade_salary,
    pre_trade_status = matching$pre_trade_status,
    post_trade_status = post_status$status,
    becomes_tax_team = !resolve_trade_team_status(team_salary, thresholds)$is_tax_team &&
      post_status$is_tax_team,
    crosses_first_apron = !resolve_trade_team_status(team_salary, thresholds)$is_above_first_apron &&
      post_status$is_above_first_apron,
    crosses_second_apron = !resolve_trade_team_status(team_salary, thresholds)$is_above_second_apron &&
      post_status$is_above_second_apron,
    explanation = explanation
  )
}

#' Extract player salary from a trade-side data frame
#' @noRd
trade_side_salary <- function(players) {
  if (is.null(players) || !nrow(players)) return(0)
  
  salary_column <- intersect(
    c("trade_salary", "matching_salary", "cap_hit", "salary", "base_salary"),
    names(players)
  )
  
  if (!length(salary_column)) {
    stop(
      "Trade player data must contain trade_salary, matching_salary, cap_hit, salary, or base_salary.",
      call. = FALSE
    )
  }
  
  trade_salary_sum(players[[salary_column[[1]]]])
}

#' Evaluate one team using selected outgoing and incoming player rows
#'
#' @param outgoing_players Data frame of players sent out.
#' @param incoming_players Data frame of players received.
#' @param team_salary Team salary before the proposed trade.
#' @param thresholds Current season cap thresholds.
#' @param available_cap_room Optional available cap room.
#' @param rule_overrides Optional matching-rule overrides.
#' @return A named list containing salary matching and restriction flags.
#' @noRd
evaluate_trade_side <- function(outgoing_players,
                                incoming_players,
                                team_salary,
                                thresholds,
                                available_cap_room = NULL,
                                rule_overrides = list()) {
  outgoing_count <- if (is.null(outgoing_players)) 0L else nrow(outgoing_players)
  incoming_count <- if (is.null(incoming_players)) 0L else nrow(incoming_players)
  
  result <- evaluate_team_trade(
    outgoing_salary = trade_side_salary(outgoing_players),
    incoming_salary = trade_side_salary(incoming_players),
    team_salary = team_salary,
    thresholds = thresholds,
    available_cap_room = available_cap_room,
    rule_overrides = rule_overrides
  )
  
  second_apron_aggregation_violation <-
    identical(result$pre_trade_status, "Above Second Apron") && outgoing_count > 1L
  
  restrictions <- character()
  
  if (second_apron_aggregation_violation) {
    restrictions <- c(
      restrictions,
      "Second-apron teams cannot aggregate multiple outgoing player salaries in this screening model."
    )
  }
  
  if (result$crosses_first_apron) {
    restrictions <- c(
      restrictions,
      "The transaction moves the team above the first apron; hard-cap and exception consequences require review."
    )
  }
  
  if (result$crosses_second_apron) {
    restrictions <- c(
      restrictions,
      "The transaction moves the team above the second apron; additional roster-building restrictions require review."
    )
  }
  
  result$outgoing_player_count <- outgoing_count
  result$incoming_player_count <- incoming_count
  result$second_apron_aggregation_violation <- second_apron_aggregation_violation
  result$restriction_flags <- restrictions
  result$requires_manual_review <- length(restrictions) > 0
  result$is_screen_pass <- result$is_salary_match && !second_apron_aggregation_violation
  result$screen_status <- if (result$is_screen_pass) "PASS" else "FAIL"
  result
}

#' Evaluate both sides of a two-team trade
#'
#' @param team_a Named list containing team_name, team_salary, outgoing_players,
#'   incoming_players, and optional available_cap_room.
#' @param team_b Same structure as team_a.
#' @param thresholds Current-season thresholds.
#' @param rule_overrides Optional matching-rule overrides.
#' @return A named list with both team results and an overall screening result.
#' @noRd
evaluate_two_team_trade <- function(team_a,
                                    team_b,
                                    thresholds,
                                    rule_overrides = list()) {
  required <- c("team_name", "team_salary", "outgoing_players", "incoming_players")
  
  missing_a <- setdiff(required, names(team_a))
  missing_b <- setdiff(required, names(team_b))
  
  if (length(missing_a)) {
    stop(
      paste("team_a is missing:", paste(missing_a, collapse = ", ")),
      call. = FALSE
    )
  }
  
  if (length(missing_b)) {
    stop(
      paste("team_b is missing:", paste(missing_b, collapse = ", ")),
      call. = FALSE
    )
  }
  
  result_a <- evaluate_trade_side(
    outgoing_players = team_a$outgoing_players,
    incoming_players = team_a$incoming_players,
    team_salary = team_a$team_salary,
    thresholds = thresholds,
    available_cap_room = team_a$available_cap_room %||% NULL,
    rule_overrides = rule_overrides
  )
  
  result_b <- evaluate_trade_side(
    outgoing_players = team_b$outgoing_players,
    incoming_players = team_b$incoming_players,
    team_salary = team_b$team_salary,
    thresholds = thresholds,
    available_cap_room = team_b$available_cap_room %||% NULL,
    rule_overrides = rule_overrides
  )
  
  overall_pass <- isTRUE(result_a$is_screen_pass) && isTRUE(result_b$is_screen_pass)
  manual_review <- isTRUE(result_a$requires_manual_review) ||
    isTRUE(result_b$requires_manual_review)
  
  list(
    status = if (overall_pass) "PASS" else "FAIL",
    is_trade_screen_pass = overall_pass,
    requires_manual_review = manual_review,
    team_a_name = team_a$team_name,
    team_b_name = team_b$team_name,
    team_a = result_a,
    team_b = result_b,
    executive_summary = build_trade_executive_summary(
      team_a_name = team_a$team_name,
      team_b_name = team_b$team_name,
      team_a_result = result_a,
      team_b_result = result_b
    ),
    scope_note = paste(
      "This result is a salary-matching and selected apron-restriction screen,",
      "not a final league-office transaction determination."
    )
  )
}

#' Build an executive-facing trade summary
#' @noRd
build_trade_executive_summary <- function(team_a_name,
                                          team_b_name,
                                          team_a_result,
                                          team_b_result) {
  both_pass <- isTRUE(team_a_result$is_screen_pass) &&
    isTRUE(team_b_result$is_screen_pass)
  
  opening <- if (both_pass) {
    "The proposed transaction passes the current salary-matching screen for both teams."
  } else {
    "The proposed transaction does not pass the current salary-matching screen for both teams."
  }
  
  team_sentence <- function(team_name, result) {
    paste0(
      team_name, ": ", result$screen_status,
      "; payroll moves from ", trade_money(result$pre_trade_salary),
      " to ", trade_money(result$post_trade_salary),
      "; status moves from ", result$pre_trade_status,
      " to ", result$post_trade_status, "."
    )
  }
  
  review_note <- if (isTRUE(team_a_result$requires_manual_review) ||
                     isTRUE(team_b_result$requires_manual_review)) {
    paste(
      "At least one apron or aggregation flag requires additional CBA review",
      "before the transaction should be described as fully legal."
    )
  } else {
    paste(
      "No modeled apron-crossing or second-apron aggregation flag was identified,",
      "but transaction-specific restrictions still require final review."
    )
  }
  
  paste(
    opening,
    team_sentence(team_a_name, team_a_result),
    team_sentence(team_b_name, team_b_result),
    review_note
  )
}

#' Load trade-eligible player salary rows for one team
#'
#' @param team_value Team name, abbreviation, or team ID.
#' @param season NBA season.
#' @param db_path Optional database path.
#' @return Player and contract-year salary rows.
#' @noRd
get_trade_player_pool <- function(team_value, season, db_path = NULL) {
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
      p.primary_position,
      cy.contract_year_id,
      cy.contract_id,
      cy.team_id,
      cy.season,
      cy.base_salary,
      cy.cap_hit,
      cy.guaranteed_amount,
      cy.option_type,
      cy.likely_incentives,
      cy.unlikely_incentives,
      c.contract_type,
      c.contract_end_season,
      COALESCE(rh.roster_status, 'Active') AS roster_status,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag
    FROM contract_years cy
    INNER JOIN players p
      ON p.player_id = cy.player_id
    INNER JOIN teams t
      ON t.team_id = cy.team_id
    LEFT JOIN contracts c
      ON c.contract_id = cy.contract_id
    LEFT JOIN roster_history rh
      ON rh.player_id = cy.player_id
      AND rh.team_id = cy.team_id
      AND rh.season = cy.season
    WHERE
      (t.team_name = ? OR t.abbreviation = ? OR CAST(t.team_id AS TEXT) = CAST(? AS TEXT))
      AND cy.season = ?
    ORDER BY cy.cap_hit DESC, p.player_name
    ",
    params = list(team_value, team_value, team_value, season)
  )
}

#' Build the database-backed input for one side of a trade
#'
#' @param team_value Team name, abbreviation, or team ID.
#' @param season NBA season.
#' @param outgoing_player_ids Player IDs selected to leave the team.
#' @param incoming_players Data frame of incoming players from the other team.
#' @param db_path Optional database path.
#' @return A named list accepted by evaluate_two_team_trade().
#' @noRd
build_trade_team_input <- function(team_value,
                                   season,
                                   outgoing_player_ids,
                                   incoming_players,
                                   db_path = NULL) {
  pool <- get_trade_player_pool(team_value, season, db_path = db_path)
  cap_summary <- get_team_cap_summary(team_value, season, db_path = db_path)
  
  outgoing_ids <- suppressWarnings(as.integer(outgoing_player_ids))
  outgoing_ids <- outgoing_ids[!is.na(outgoing_ids)]
  
  outgoing_players <- pool[
    pool$player_id %in% outgoing_ids,
    ,
    drop = FALSE
  ]
  
  list(
    team_name = team_value,
    team_salary = cap_summary$team_salary,
    available_cap_room = cap_summary$cap_room,
    outgoing_players = outgoing_players,
    incoming_players = incoming_players,
    cap_summary = cap_summary
  )
}