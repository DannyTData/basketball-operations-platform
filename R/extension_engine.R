# ============================================================
# Thompson's Basketball Intelligence
# Phase 4: Contract and Extension Intelligence Engine
# ============================================================

# This file provides a reusable, testable extension-screening engine.
# It is intentionally independent of Shiny and the database.
#
# IMPORTANT:
# - This is a decision-support screen, not a final league-office ruling.
# - Transaction timing, award qualification, contract language, option status,
#   renegotiation rules, Over-38 treatment, and other player-specific facts
#   still require verified inputs.
# - Rule values are centralized in extension_rule_defaults() so future CBA
#   updates can be changed in one place.

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------

#' Convert a value to a safe numeric scalar
#' @noRd
extension_number <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(x))
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !is.finite(value[[1]])
  ) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to a safe whole-number scalar
#' @noRd
extension_integer <- function(x, default = 0L) {
  value <- extension_number(x, default)
  
  as.integer(round(value))
}


#' Convert a value to a logical scalar
#' @noRd
extension_flag <- function(x, default = FALSE) {
  if (is.logical(x) && length(x) && !is.na(x[[1]])) {
    return(isTRUE(x[[1]]))
  }
  
  if (is.numeric(x) && length(x) && !is.na(x[[1]])) {
    return(x[[1]] != 0)
  }
  
  if (is.character(x) && length(x)) {
    value <- tolower(trimws(x[[1]]))
    
    if (value %in% c("true", "t", "yes", "y", "1")) return(TRUE)
    if (value %in% c("false", "f", "no", "n", "0")) return(FALSE)
  }
  
  default
}


#' Return a readable dollar value
#' @noRd
extension_money <- function(x, digits = 1) {
  x <- extension_number(x, 0)
  
  if (abs(x) >= 1e9) {
    return(sprintf(paste0("$%.", digits, "fB"), x / 1e9))
  }
  
  if (abs(x) >= 1e6) {
    return(sprintf(paste0("$%.", digits, "fM"), x / 1e6))
  }
  
  paste0(
    "$",
    format(
      round(x),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}


#' Return the first available value from a named list
#' @noRd
extension_value <- function(x, name, default = NULL) {
  if (is.null(x) || !is.list(x) || !name %in% names(x)) {
    return(default)
  }
  
  value <- x[[name]]
  
  if (is.null(value) || !length(value)) default else value
}


# ------------------------------------------------------------
# Centralized rule settings
# ------------------------------------------------------------

#' Default extension-screening rules
#'
#' These values are centralized so the application can update one rule object
#' rather than hard-code percentages throughout the UI.
#'
#' @return Named list of extension rule settings.
#' @noRd
extension_rule_defaults <- function() {
  list(
    cba_label = "2023 NBA-NBPA CBA screening rules",
    
    max_salary_percentages = c(
      service_0_6 = 0.25,
      service_7_9 = 0.30,
      service_10_plus = 0.35
    ),
    
    standard_raise_percent = 0.05,
    bird_raise_percent = 0.08,
    
    rookie_standard_max_years = 4L,
    rookie_designated_max_years = 5L,
    veteran_standard_max_years = 4L,
    
    rookie_extension_service_year = 3L,
    veteran_minimum_service_years = 4L,
    
    veteran_prior_salary_multiplier = 1.40,
    
    designated_rookie_max_percent = 0.30,
    designated_veteran_max_percent = 0.35,
    
    require_manual_timing_review = TRUE,
    require_manual_award_review = TRUE
  )
}


#' Merge default rules with optional overrides
#' @noRd
resolve_extension_rules <- function(rule_overrides = NULL) {
  rules <- extension_rule_defaults()
  
  if (!is.null(rule_overrides)) {
    if (!is.list(rule_overrides)) {
      stop("rule_overrides must be a named list.", call. = FALSE)
    }
    
    unknown <- setdiff(names(rule_overrides), names(rules))
    
    if (length(unknown)) {
      stop(
        paste0(
          "Unknown extension rule override(s): ",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    rules[names(rule_overrides)] <- rule_overrides
  }
  
  rules
}


# ------------------------------------------------------------
# Maximum salary logic
# ------------------------------------------------------------

#' Determine the standard maximum-salary percentage by service
#' @noRd
maximum_salary_percentage <- function(service_years,
                                      designated_rookie_qualified = FALSE,
                                      designated_veteran_qualified = FALSE,
                                      rules = extension_rule_defaults()) {
  service_years <- extension_integer(service_years, 0L)
  
  if (service_years < 0L) {
    stop("service_years cannot be negative.", call. = FALSE)
  }
  
  if (extension_flag(designated_veteran_qualified)) {
    return(extension_number(rules$designated_veteran_max_percent, 0.35))
  }
  
  if (extension_flag(designated_rookie_qualified)) {
    return(extension_number(rules$designated_rookie_max_percent, 0.30))
  }
  
  percentages <- rules$max_salary_percentages
  
  if (is.null(percentages) || length(percentages) < 3L) {
    stop("Maximum-salary percentages are missing from the rule set.", call. = FALSE)
  }
  
  if (service_years >= 10L) {
    return(extension_number(percentages[["service_10_plus"]], 0.35))
  }
  
  if (service_years >= 7L) {
    return(extension_number(percentages[["service_7_9"]], 0.30))
  }
  
  extension_number(percentages[["service_0_6"]], 0.25)
}


#' Calculate the applicable maximum player salary
#' @noRd
calculate_maximum_player_salary <- function(salary_cap,
                                            service_years,
                                            designated_rookie_qualified = FALSE,
                                            designated_veteran_qualified = FALSE,
                                            rules = extension_rule_defaults()) {
  salary_cap <- extension_number(salary_cap, NA_real_)
  
  if (is.na(salary_cap) || salary_cap <= 0) {
    stop("A positive salary_cap is required.", call. = FALSE)
  }
  
  percentage <- maximum_salary_percentage(
    service_years = service_years,
    designated_rookie_qualified = designated_rookie_qualified,
    designated_veteran_qualified = designated_veteran_qualified,
    rules = rules
  )
  
  list(
    salary_cap = salary_cap,
    maximum_percentage = percentage,
    maximum_salary = salary_cap * percentage
  )
}


# ------------------------------------------------------------
# Extension type and eligibility screening
# ------------------------------------------------------------

#' Normalize an extension type
#' @noRd
normalize_extension_type <- function(extension_type) {
  value <- tolower(trimws(as.character(extension_type %||% "")))
  
  aliases <- c(
    "rookie" = "rookie_scale",
    "rookie scale" = "rookie_scale",
    "rookie_scale" = "rookie_scale",
    "designated rookie" = "designated_rookie",
    "designated_rookie" = "designated_rookie",
    "veteran" = "veteran",
    "veteran extension" = "veteran",
    "veteran_extension" = "veteran",
    "designated veteran" = "designated_veteran",
    "designated_veteran" = "designated_veteran"
  )
  
  resolved <- unname(aliases[value])
  
  if (!length(resolved) || is.na(resolved)) {
    stop(
      paste0(
        "Unsupported extension_type: ",
        as.character(extension_type),
        ". Use rookie_scale, designated_rookie, veteran, or designated_veteran."
      ),
      call. = FALSE
    )
  }
  
  resolved
}


#' Screen whether a player appears eligible for an extension type
#'
#' @param player Named list of player and contract facts.
#' @param extension_type Requested extension type.
#' @param rules Extension rules.
#' @return Named eligibility result.
#' @noRd
screen_extension_eligibility <- function(player,
                                         extension_type,
                                         rules = extension_rule_defaults()) {
  if (!is.list(player)) {
    stop("player must be a named list.", call. = FALSE)
  }
  
  extension_type <- normalize_extension_type(extension_type)
  
  service_years <- extension_integer(
    extension_value(player, "service_years", 0L),
    0L
  )
  
  contract_type <- tolower(
    trimws(
      as.character(
        extension_value(player, "contract_type", "")
      )
    )
  )
  
  remaining_years <- extension_integer(
    extension_value(player, "remaining_contract_years", 0L),
    0L
  )
  
  is_first_round_pick <- extension_flag(
    extension_value(player, "is_first_round_pick", FALSE)
  )
  
  rookie_option_years_exercised <- extension_flag(
    extension_value(player, "rookie_option_years_exercised", FALSE)
  )
  
  designated_rookie_qualified <- extension_flag(
    extension_value(player, "designated_rookie_qualified", FALSE)
  )
  
  designated_veteran_qualified <- extension_flag(
    extension_value(player, "designated_veteran_qualified", FALSE)
  )
  
  original_team_requirement_met <- extension_flag(
    extension_value(player, "original_team_requirement_met", FALSE)
  )
  
  timing_window_open <- extension_flag(
    extension_value(player, "timing_window_open", FALSE)
  )
  
  has_eto_exercised <- extension_flag(
    extension_value(player, "eto_exercised", FALSE)
  )
  
  contract_allows_extension <- extension_flag(
    extension_value(player, "contract_allows_extension", TRUE),
    TRUE
  )
  
  failures <- character(0)
  warnings <- character(0)
  
  if (service_years < 0L) {
    failures <- c(failures, "Service years cannot be negative.")
  }
  
  if (remaining_years < 0L) {
    failures <- c(failures, "Remaining contract years cannot be negative.")
  }
  
  if (has_eto_exercised) {
    failures <- c(
      failures,
      "The supplied facts indicate an early termination option was exercised."
    )
  }
  
  if (!contract_allows_extension) {
    failures <- c(
      failures,
      "The supplied contract facts indicate that this contract is not extendable."
    )
  }
  
  if (extension_type %in% c("rookie_scale", "designated_rookie")) {
    if (!is_first_round_pick) {
      failures <- c(
        failures,
        "Rookie-scale extension screening requires a first-round draft pick."
      )
    }
    
    if (
      service_years !=
      extension_integer(rules$rookie_extension_service_year, 3L)
    ) {
      failures <- c(
        failures,
        paste0(
          "The player must be in the rookie-scale extension service-year window supplied by the rule set."
        )
      )
    }
    
    if (!rookie_option_years_exercised) {
      failures <- c(
        failures,
        "The supplied facts do not confirm that the relevant rookie-scale option years were exercised."
      )
    }
    
    if (
      nzchar(contract_type) &&
      !grepl("rookie", contract_type, fixed = TRUE)
    ) {
      warnings <- c(
        warnings,
        "The loaded contract_type does not clearly identify a rookie-scale contract."
      )
    }
  }
  
  if (extension_type %in% c("veteran", "designated_veteran")) {
    if (
      service_years <
      extension_integer(rules$veteran_minimum_service_years, 4L)
    ) {
      failures <- c(
        failures,
        "The player does not meet the veteran-extension service threshold supplied by the rule set."
      )
    }
    
    if (remaining_years < 1L) {
      failures <- c(
        failures,
        "At least one remaining contract year is required for this extension screen."
      )
    }
  }
  
  if (
    extension_type == "designated_rookie" &&
    !designated_rookie_qualified
  ) {
    failures <- c(
      failures,
      "Designated-rookie qualification has not been confirmed."
    )
  }
  
  if (extension_type == "designated_veteran") {
    if (!designated_veteran_qualified) {
      failures <- c(
        failures,
        "Designated-veteran award qualification has not been confirmed."
      )
    }
    
    if (!original_team_requirement_met) {
      failures <- c(
        failures,
        "The designated-veteran original-team requirement has not been confirmed."
      )
    }
  }
  
  if (!timing_window_open) {
    warnings <- c(
      warnings,
      "The applicable signing window has not been confirmed as open."
    )
  }
  
  eligible <- !length(failures)
  
  status <- if (eligible && !length(warnings)) {
    "ELIGIBLE"
  } else if (eligible) {
    "ELIGIBLE_WITH_REVIEW"
  } else {
    "INELIGIBLE"
  }
  
  list(
    extension_type = extension_type,
    eligible = eligible,
    status = status,
    service_years = service_years,
    remaining_contract_years = remaining_years,
    failures = unique(failures),
    warnings = unique(warnings),
    requires_manual_review = length(warnings) > 0L,
    scope_note = paste(
      "This is an extension eligibility screen.",
      "Official eligibility still depends on verified contract language, dates,",
      "award results, service calculations, and league interpretation."
    )
  )
}


# ------------------------------------------------------------
# Starting salary and raise logic
# ------------------------------------------------------------

#' Determine the maximum starting salary for an extension screen
#' @noRd
calculate_extension_starting_salary_limit <- function(
    extension_type,
    salary_cap,
    service_years,
    current_salary,
    next_season_salary = NULL,
    designated_rookie_qualified = FALSE,
    designated_veteran_qualified = FALSE,
    requested_starting_salary = NULL,
    rules = extension_rule_defaults()) {
  
  extension_type <- normalize_extension_type(extension_type)
  
  salary_cap <- extension_number(salary_cap, NA_real_)
  current_salary <- extension_number(current_salary, NA_real_)
  
  if (is.na(salary_cap) || salary_cap <= 0) {
    stop("A positive salary_cap is required.", call. = FALSE)
  }
  
  if (is.na(current_salary) || current_salary < 0) {
    stop("A non-negative current_salary is required.", call. = FALSE)
  }
  
  next_season_salary <- extension_number(
    next_season_salary,
    current_salary
  )
  
  max_salary <- calculate_maximum_player_salary(
    salary_cap = salary_cap,
    service_years = service_years,
    designated_rookie_qualified = designated_rookie_qualified,
    designated_veteran_qualified = designated_veteran_qualified,
    rules = rules
  )
  
  prior_salary_basis <- max(current_salary, next_season_salary, na.rm = TRUE)
  
  if (extension_type %in% c("rookie_scale", "designated_rookie")) {
    rule_limit <- max_salary$maximum_salary
    rule_code <- if (
      extension_type == "designated_rookie"
    ) {
      "DESIGNATED_ROOKIE_MAX"
    } else {
      "ROOKIE_SCALE_MAX"
    }
    
    explanation <- paste0(
      "Starting salary is screened against ",
      sprintf("%.0f%%", max_salary$maximum_percentage * 100),
      " of the supplied salary cap."
    )
  } else {
    prior_salary_limit <-
      prior_salary_basis *
      extension_number(
        rules$veteran_prior_salary_multiplier,
        1.40
      )
    
    rule_limit <- min(
      max_salary$maximum_salary,
      prior_salary_limit
    )
    
    rule_code <- if (
      extension_type == "designated_veteran"
    ) {
      "DESIGNATED_VETERAN_MAX"
    } else {
      "VETERAN_PRIOR_SALARY_LIMIT"
    }
    
    explanation <- paste0(
      "Starting salary is screened against the lower of the applicable maximum salary and ",
      sprintf(
        "%.0f%%",
        extension_number(
          rules$veteran_prior_salary_multiplier,
          1.40
        ) * 100
      ),
      " of the supplied prior-salary basis."
    )
  }
  
  requested <- extension_number(
    requested_starting_salary,
    rule_limit
  )
  
  list(
    extension_type = extension_type,
    salary_cap = salary_cap,
    service_years = extension_integer(service_years, 0L),
    current_salary = current_salary,
    next_season_salary = next_season_salary,
    prior_salary_basis = prior_salary_basis,
    maximum_salary_percentage = max_salary$maximum_percentage,
    maximum_player_salary = max_salary$maximum_salary,
    maximum_starting_salary = rule_limit,
    requested_starting_salary = requested,
    requested_within_limit = requested <= rule_limit + 0.01,
    excess_over_limit = max(requested - rule_limit, 0),
    room_below_limit = max(rule_limit - requested, 0),
    rule_code = rule_code,
    explanation = explanation
  )
}


#' Determine the maximum allowed annual raise for an extension type
#' @noRd
maximum_extension_raise_percent <- function(
    has_bird_rights = TRUE,
    requested_raise_percent = NULL,
    rules = extension_rule_defaults()) {
  
  limit <- if (extension_flag(has_bird_rights, TRUE)) {
    extension_number(rules$bird_raise_percent, 0.08)
  } else {
    extension_number(rules$standard_raise_percent, 0.05)
  }
  
  requested <- extension_number(
    requested_raise_percent,
    limit
  )
  
  list(
    maximum_raise_percent = limit,
    requested_raise_percent = requested,
    requested_within_limit = requested <= limit + 1e-10,
    excess_raise_percent = max(requested - limit, 0)
  )
}


# ------------------------------------------------------------
# Contract schedule
# ------------------------------------------------------------

#' Build a year-by-year extension schedule
#' @noRd
build_extension_schedule <- function(starting_salary,
                                     years,
                                     raise_percent = 0.08,
                                     guarantee_structure = "Fully guaranteed",
                                     first_season = NULL) {
  starting_salary <- extension_number(starting_salary, NA_real_)
  years <- extension_integer(years, NA_integer_)
  raise_percent <- extension_number(raise_percent, NA_real_)
  
  if (is.na(starting_salary) || starting_salary < 0) {
    stop("starting_salary must be non-negative.", call. = FALSE)
  }
  
  if (is.na(years) || years < 1L || years > 5L) {
    stop("years must be between 1 and 5.", call. = FALSE)
  }
  
  if (is.na(raise_percent) || raise_percent < -1 || raise_percent > 1) {
    stop(
      "raise_percent must be supplied as a decimal between -1 and 1.",
      call. = FALSE
    )
  }
  
  guarantee_structure <- as.character(
    guarantee_structure %||% "Fully guaranteed"
  )
  
  year_number <- seq_len(years)
  
  salary <- starting_salary *
    (1 + raise_percent)^(year_number - 1L)
  
  guarantee_label <- rep("Guaranteed", years)
  
  if (
    identical(
      guarantee_structure,
      "Final year team option"
    )
  ) {
    guarantee_label[[years]] <- "Team option"
  } else if (
    identical(
      guarantee_structure,
      "Final year player option"
    )
  ) {
    guarantee_label[[years]] <- "Player option"
  } else if (
    identical(
      guarantee_structure,
      "Partial guarantee"
    )
  ) {
    guarantee_label[[years]] <- "Partial guarantee"
  }
  
  season <- if (is.null(first_season) || !nzchar(first_season)) {
    paste("Extension Year", year_number)
  } else {
    start_year <- suppressWarnings(
      as.integer(substr(first_season, 1, 4))
    )
    
    if (is.na(start_year)) {
      paste("Extension Year", year_number)
    } else {
      sprintf(
        "%d-%02d",
        start_year + year_number - 1L,
        (start_year + year_number) %% 100
      )
    }
  }
  
  data.frame(
    extension_year = year_number,
    season = season,
    salary = salary,
    raise_amount = c(NA_real_, diff(salary)),
    raise_percent = c(NA_real_, rep(raise_percent, years - 1L)),
    guarantee = guarantee_label,
    cumulative_value = cumsum(salary),
    stringsAsFactors = FALSE
  )
}


#' Summarize an extension schedule
#' @noRd
summarize_extension_schedule <- function(schedule) {
  if (!is.data.frame(schedule) || !nrow(schedule)) {
    stop("schedule must be a non-empty data frame.", call. = FALSE)
  }
  
  required <- c("salary", "extension_year")
  
  if (!all(required %in% names(schedule))) {
    stop(
      "schedule must contain salary and extension_year columns.",
      call. = FALSE
    )
  }
  
  salaries <- suppressWarnings(as.numeric(schedule$salary))
  
  if (any(is.na(salaries))) {
    stop("schedule contains invalid salary values.", call. = FALSE)
  }
  
  total_value <- sum(salaries)
  
  list(
    years = nrow(schedule),
    starting_salary = salaries[[1]],
    final_salary = utils::tail(salaries, 1),
    total_value = total_value,
    average_annual_value = total_value / nrow(schedule),
    schedule = schedule
  )
}


# ------------------------------------------------------------
# Full extension evaluation
# ------------------------------------------------------------

#' Evaluate a proposed extension
#'
#' @param player Named list of player/contract facts.
#' @param proposal Named list containing extension_type, salary_cap,
#'   starting_salary, years, raise_percent, and optional structure fields.
#' @param rule_overrides Optional named list passed to resolve_extension_rules().
#' @return Complete extension-screening result.
#' @noRd
evaluate_extension_proposal <- function(player,
                                        proposal,
                                        rule_overrides = NULL) {
  if (!is.list(player)) {
    stop("player must be a named list.", call. = FALSE)
  }
  
  if (!is.list(proposal)) {
    stop("proposal must be a named list.", call. = FALSE)
  }
  
  required_proposal_fields <- c(
    "extension_type",
    "salary_cap",
    "starting_salary",
    "years",
    "raise_percent"
  )
  
  missing_fields <- setdiff(
    required_proposal_fields,
    names(proposal)
  )
  
  if (length(missing_fields)) {
    stop(
      paste0(
        "proposal is missing required field(s): ",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  rules <- resolve_extension_rules(rule_overrides)
  extension_type <- normalize_extension_type(
    proposal$extension_type
  )
  
  eligibility <- screen_extension_eligibility(
    player = player,
    extension_type = extension_type,
    rules = rules
  )
  
  service_years <- extension_integer(
    extension_value(player, "service_years", 0L),
    0L
  )
  
  current_salary <- extension_number(
    extension_value(player, "current_salary", NA_real_),
    NA_real_
  )
  
  next_season_salary <- extension_number(
    extension_value(
      player,
      "next_season_salary",
      current_salary
    ),
    current_salary
  )
  
  designated_rookie_qualified <- extension_flag(
    extension_value(
      player,
      "designated_rookie_qualified",
      FALSE
    )
  )
  
  designated_veteran_qualified <- extension_flag(
    extension_value(
      player,
      "designated_veteran_qualified",
      FALSE
    )
  )
  
  starting_limit <- calculate_extension_starting_salary_limit(
    extension_type = extension_type,
    salary_cap = proposal$salary_cap,
    service_years = service_years,
    current_salary = current_salary,
    next_season_salary = next_season_salary,
    designated_rookie_qualified =
      designated_rookie_qualified,
    designated_veteran_qualified =
      designated_veteran_qualified,
    requested_starting_salary =
      proposal$starting_salary,
    rules = rules
  )
  
  has_bird_rights <- extension_flag(
    extension_value(
      player,
      "has_bird_rights",
      TRUE
    ),
    TRUE
  )
  
  raise_screen <- maximum_extension_raise_percent(
    has_bird_rights = has_bird_rights,
    requested_raise_percent = proposal$raise_percent,
    rules = rules
  )
  
  requested_years <- extension_integer(
    proposal$years,
    NA_integer_
  )
  
  maximum_years <- switch(
    extension_type,
    "rookie_scale" =
      extension_integer(
        rules$rookie_standard_max_years,
        4L
      ),
    "designated_rookie" =
      extension_integer(
        rules$rookie_designated_max_years,
        5L
      ),
    "veteran" =
      extension_integer(
        rules$veteran_standard_max_years,
        4L
      ),
    "designated_veteran" =
      extension_integer(
        rules$veteran_standard_max_years,
        4L
      )
  )
  
  years_within_limit <-
    !is.na(requested_years) &&
    requested_years >= 1L &&
    requested_years <= maximum_years
  
  schedule <- build_extension_schedule(
    starting_salary =
      extension_number(proposal$starting_salary, 0),
    years = max(1L, min(requested_years, 5L)),
    raise_percent =
      extension_number(proposal$raise_percent, 0),
    guarantee_structure =
      extension_value(
        proposal,
        "guarantee_structure",
        "Fully guaranteed"
      ),
    first_season =
      extension_value(
        proposal,
        "first_season",
        NULL
      )
  )
  
  schedule_summary <- summarize_extension_schedule(schedule)
  
  failures <- c(
    eligibility$failures,
    if (!starting_limit$requested_within_limit) {
      paste0(
        "Requested starting salary exceeds the screened limit by ",
        extension_money(
          starting_limit$excess_over_limit
        ),
        "."
      )
    },
    if (!raise_screen$requested_within_limit) {
      paste0(
        "Requested annual raise exceeds the screened limit by ",
        sprintf(
          "%.2f percentage points.",
          raise_screen$excess_raise_percent * 100
        )
      )
    },
    if (!years_within_limit) {
      paste0(
        "Requested term exceeds the screened ",
        maximum_years,
        "-year limit for this extension type."
      )
    }
  )
  
  failures <- unique(failures[nzchar(failures)])
  
  warnings <- unique(
    c(
      eligibility$warnings,
      "Confirm official signing dates and extension window.",
      "Confirm award-based qualification when a designated extension is used.",
      "Confirm option, guarantee, bonus, and trade-kicker language before approval."
    )
  )
  
  passes_screen <- !length(failures)
  
  status <- if (passes_screen && !length(warnings)) {
    "PASS"
  } else if (passes_screen) {
    "PASS_WITH_REVIEW"
  } else {
    "FAIL"
  }
  
  executive_summary <- if (!passes_screen) {
    paste0(
      "The proposed ",
      gsub("_", " ", extension_type),
      " extension does not pass the current screen. ",
      paste(failures, collapse = " ")
    )
  } else {
    paste0(
      "The proposed ",
      gsub("_", " ", extension_type),
      " structure passes the current financial and eligibility screen at ",
      extension_money(schedule_summary$total_value),
      " over ",
      schedule_summary$years,
      " years. Manual contract and timing review remains required."
    )
  }
  
  recommendation <- if (!passes_screen) {
    "Revise the proposal or verify the missing eligibility facts before advancing."
  } else if (
    starting_limit$room_below_limit <
    0.02 * starting_limit$maximum_starting_salary
  ) {
    "The proposal is near the screened maximum. Review long-term cap concentration and negotiation alternatives."
  } else {
    "The proposal is below the screened maximum. Compare this structure with waiting, free-agency risk, and future cap growth."
  }
  
  list(
    status = status,
    passes_screen = passes_screen,
    requires_manual_review = TRUE,
    extension_type = extension_type,
    eligibility = eligibility,
    starting_salary_screen = starting_limit,
    raise_screen = raise_screen,
    requested_years = requested_years,
    maximum_years = maximum_years,
    years_within_limit = years_within_limit,
    schedule = schedule,
    schedule_summary = schedule_summary,
    failures = failures,
    warnings = warnings,
    executive_summary = executive_summary,
    recommendation = recommendation,
    scope_note = paste(
      "This result is a contract-extension decision-support screen,",
      "not a final NBA or league-office determination."
    ),
    rule_set = rules$cba_label
  )
}


# ------------------------------------------------------------
# Database-backed player inputs
# ------------------------------------------------------------

#' Load player and contract facts for the Extension Simulator
#'
#' The current database does not contain every fact required for a definitive
#' CBA eligibility ruling. Missing facts are returned as explicit FALSE/NA
#' values so the UI can request or disclose them rather than assume them.
#'
#' @noRd
get_extension_player_pool <- function(team_value,
                                      season,
                                      db_path = NULL) {
  path <- resolve_tbi_db_path(
    db_path %||%
      file.path(
        "inst",
        "database",
        "tbi.sqlite"
      )
  )
  
  con <- connect_db(
    db_path = path,
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_id,
      t.team_name,
      cy.contract_id,
      cy.season,
      COALESCE(cy.cap_hit, cy.base_salary, 0) AS current_salary,
      cy.base_salary,
      cy.cap_hit,
      cy.guaranteed_amount,
      cy.option_type,
      c.contract_type,
      c.contract_start_season,
      c.contract_end_season,
      c.free_agent_year,
      c.bird_rights,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag
    FROM roster_history rh
    INNER JOIN players p
      ON p.player_id = rh.player_id
    INNER JOIN teams t
      ON t.team_id = rh.team_id
    LEFT JOIN contract_years cy
      ON cy.player_id = rh.player_id
      AND cy.team_id = rh.team_id
      AND cy.season = rh.season
    LEFT JOIN contracts c
      ON c.contract_id = cy.contract_id
    WHERE
      (
        t.team_name = ?
        OR t.abbreviation = ?
        OR CAST(t.team_id AS TEXT) =
           CAST(? AS TEXT)
      )
      AND rh.season = ?
    ORDER BY
      current_salary DESC,
      p.player_name
    ",
    params = list(
      team_value,
      team_value,
      team_value,
      season
    )
  )
}


#' Build a conservative player input object from one database row
#' @noRd
extension_player_from_row <- function(player_row,
                                      service_years = NA_integer_,
                                      remaining_contract_years = NA_integer_,
                                      is_first_round_pick = FALSE,
                                      rookie_option_years_exercised = FALSE,
                                      timing_window_open = FALSE,
                                      designated_rookie_qualified = FALSE,
                                      designated_veteran_qualified = FALSE,
                                      original_team_requirement_met = FALSE) {
  if (!is.data.frame(player_row) || nrow(player_row) != 1L) {
    stop(
      "player_row must be a one-row data frame.",
      call. = FALSE
    )
  }
  
  bird_rights <- if ("bird_rights" %in% names(player_row)) {
    value <- tolower(
      trimws(
        as.character(
          player_row$bird_rights[[1]]
        )
      )
    )
    
    nzchar(value) &&
      !value %in% c(
        "none",
        "no",
        "not loaded",
        "na"
      )
  } else {
    FALSE
  }
  
  list(
    player_id = player_row$player_id[[1]],
    player_name = player_row$player_name[[1]],
    service_years = service_years,
    current_salary = extension_number(
      player_row$current_salary[[1]],
      0
    ),
    next_season_salary = extension_number(
      player_row$current_salary[[1]],
      0
    ),
    remaining_contract_years =
      remaining_contract_years,
    contract_type = if (
      "contract_type" %in% names(player_row)
    ) {
      player_row$contract_type[[1]]
    } else {
      ""
    },
    has_bird_rights = bird_rights,
    is_first_round_pick = is_first_round_pick,
    rookie_option_years_exercised =
      rookie_option_years_exercised,
    timing_window_open = timing_window_open,
    designated_rookie_qualified =
      designated_rookie_qualified,
    designated_veteran_qualified =
      designated_veteran_qualified,
    original_team_requirement_met =
      original_team_requirement_met,
    eto_exercised = FALSE,
    contract_allows_extension = TRUE
  )
}


# Local null-coalescing helper when this file is sourced independently.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}