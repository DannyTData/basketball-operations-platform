# PHASE 15H — DRAFT ASSETS IN SHARED TRADE STATE
# ------------------------------------------------------------
# Shared Transaction / Scenario State
# Thompson Basketball Intelligence
# ------------------------------------------------------------

#' Create the shared front-office scenario state.
#'
#' The object is intentionally session-scoped. It stores a proposed
#' transaction without writing anything to the database.
#'
#' @noRd
tbi_transaction_state <- function() {
  
  state <- shiny::reactiveValues(
    active = FALSE,
    scenario_id = NULL,
    source = NULL,
    scenario_type = NULL,
    team = NULL,
    partner_team = NULL,
    season = NULL,
    outgoing_players = NULL,
    incoming_players = NULL,
    outgoing_draft_assets = NULL,
    incoming_draft_assets = NULL,
    outgoing_draft_value = 0,
    incoming_draft_value = 0,
    draft_value_delta = 0,
    draft_screen_status = "NOT USED",
    draft_evaluation = NULL,
    outgoing_salary = 0,
    incoming_salary = 0,
    salary_delta = 0,
    evaluation = NULL,
    created_at = NULL,
    updated_at = NULL
  )
  
  normalize_players <- function(x) {
    
    if (
      is.null(x) ||
      !is.data.frame(x)
    ) {
      return(
        data.frame()
      )
    }
    
    x
  }
  
  snapshot <- shiny::reactive({
    
    list(
      active = isTRUE(state$active),
      scenario_id = state$scenario_id,
      source = state$source,
      scenario_type = state$scenario_type,
      team = state$team,
      partner_team = state$partner_team,
      season = state$season,
      outgoing_players = normalize_players(
        state$outgoing_players
      ),
      incoming_players = normalize_players(
        state$incoming_players
      ),
      outgoing_draft_assets = normalize_players(
        state$outgoing_draft_assets
      ),
      incoming_draft_assets = normalize_players(
        state$incoming_draft_assets
      ),
      outgoing_draft_value = suppressWarnings(
        as.numeric(
          state$outgoing_draft_value
        )
      ),
      incoming_draft_value = suppressWarnings(
        as.numeric(
          state$incoming_draft_value
        )
      ),
      draft_value_delta = suppressWarnings(
        as.numeric(
          state$draft_value_delta
        )
      ),
      draft_screen_status = as.character(
        state$draft_screen_status
      ),
      draft_evaluation = state$draft_evaluation,
      outgoing_salary = suppressWarnings(
        as.numeric(
          state$outgoing_salary %||% 0
        )
      ),
      incoming_salary = suppressWarnings(
        as.numeric(
          state$incoming_salary %||% 0
        )
      ),
      salary_delta = suppressWarnings(
        as.numeric(
          state$salary_delta %||% 0
        )
      ),
      evaluation = state$evaluation,
      created_at = state$created_at,
      updated_at = state$updated_at
    )
  })
  
  publish_trade <- function(
    team,
    partner_team,
    season,
    outgoing_players,
    incoming_players,
      outgoing_draft_assets = data.frame(),
      incoming_draft_assets = data.frame(),
      draft_evaluation = NULL,
    evaluation = NULL,
    source = "Trade Intelligence") {
    
    outgoing_players <- normalize_players(
      outgoing_players
    )
    
    incoming_players <- normalize_players(
      incoming_players
    )

    outgoing_draft_assets <- normalize_players(
      outgoing_draft_assets
    )

    incoming_draft_assets <- normalize_players(
      incoming_draft_assets
    )
    
    outgoing_salary <- if (
      nrow(outgoing_players) &&
      "cap_hit" %in% names(outgoing_players)
    ) {
      sum(
        suppressWarnings(
          as.numeric(
            outgoing_players$cap_hit
          )
        ),
        na.rm = TRUE
      )
    } else {
      0
    }
    
    incoming_salary <- if (
      nrow(incoming_players) &&
      "cap_hit" %in% names(incoming_players)
    ) {
      sum(
        suppressWarnings(
          as.numeric(
            incoming_players$cap_hit
          )
        ),
        na.rm = TRUE
      )
    } else {
      0
    }
    
    draft_value_total <- function(x) {

      if (!nrow(x)) {
        return(0)
      }

      if (!"internal_value" %in% names(x)) {
        return(NA_real_)
      }

      values <- suppressWarnings(
        as.numeric(
          x$internal_value
        )
      )

      if (
        length(values) != nrow(x) ||
        any(is.na(values))
      ) {
        return(NA_real_)
      }

      sum(values)
    }

    outgoing_draft_value <-
      draft_value_total(
        outgoing_draft_assets
      )

    incoming_draft_value <-
      draft_value_total(
        incoming_draft_assets
      )

    draft_value_delta <- if (
      is.na(outgoing_draft_value) ||
      is.na(incoming_draft_value)
    ) {
      NA_real_
    } else {
      incoming_draft_value -
        outgoing_draft_value
    }

    draft_screen_status <- if (
      !is.null(draft_evaluation) &&
      !is.null(draft_evaluation$status)
    ) {
      as.character(
        draft_evaluation$status
      )
    } else {
      "NOT USED"
    }

    now <- Sys.time()
    
    if (
      is.null(state$scenario_id) ||
      !nzchar(
        as.character(
          state$scenario_id
        )
      )
    ) {
      state$scenario_id <- paste0(
        "trade-",
        format(
          now,
          "%Y%m%d-%H%M%S"
        ),
        "-",
        sample(
          1000:9999,
          1
        )
      )
      
      state$created_at <- now
    }
    
    state$active <- TRUE
    state$source <- source
    state$scenario_type <- "trade"
    state$team <- as.character(team)
    state$partner_team <- as.character(partner_team)
    state$season <- as.character(season)
    state$outgoing_players <- outgoing_players
    state$incoming_players <- incoming_players
    state$outgoing_draft_assets <-
      outgoing_draft_assets
    state$incoming_draft_assets <-
      incoming_draft_assets
    state$outgoing_draft_value <-
      outgoing_draft_value
    state$incoming_draft_value <-
      incoming_draft_value
    state$draft_value_delta <-
      draft_value_delta
    state$draft_screen_status <-
      draft_screen_status
    state$draft_evaluation <-
      draft_evaluation
    state$outgoing_salary <- outgoing_salary
    state$incoming_salary <- incoming_salary
    state$salary_delta <-
      incoming_salary -
      outgoing_salary
    state$evaluation <- evaluation
    state$updated_at <- now
    
    invisible(
      snapshot()
    )
  }
  
  clear <- function() {
    
    state$active <- FALSE
    state$scenario_id <- NULL
    state$source <- NULL
    state$scenario_type <- NULL
    state$team <- NULL
    state$partner_team <- NULL
    state$season <- NULL
    state$outgoing_players <- NULL
    state$incoming_players <- NULL
    state$outgoing_draft_assets <- NULL
    state$incoming_draft_assets <- NULL
    state$outgoing_draft_value <- 0
    state$incoming_draft_value <- 0
    state$draft_value_delta <- 0
    state$draft_screen_status <- "NOT USED"
    state$draft_evaluation <- NULL
    state$outgoing_salary <- 0
    state$incoming_salary <- 0
    state$salary_delta <- 0
    state$evaluation <- NULL
    state$created_at <- NULL
    state$updated_at <- NULL
    
    invisible(TRUE)
  }
  
  list(
    state = state,
    snapshot = snapshot,
    publish_trade = publish_trade,
    clear = clear
  )
}


#' Return TRUE when a shared scenario is active.
#'
#' @param transaction_state Object returned by tbi_transaction_state().
#' @noRd
tbi_scenario_active <- function(
    transaction_state) {
  
  if (
    is.null(transaction_state) ||
    is.null(transaction_state$snapshot)
  ) {
    return(FALSE)
  }
  
  snapshot <- transaction_state$snapshot()
  
  isTRUE(
    snapshot$active
  )
}


#' Apply a trade scenario to one team's roster data frame.
#'
#' This is a pure preview helper. It never writes to the database.
#'
#' @param roster Current roster data frame.
#' @param transaction_state Shared transaction state.
#' @param team_name Team whose proposed roster should be returned.
#' @noRd
tbi_apply_trade_scenario_to_roster <- function(
    roster,
    transaction_state,
    team_name) {
  
  if (
    is.null(roster) ||
    !is.data.frame(roster) ||
    is.null(transaction_state) ||
    is.null(transaction_state$snapshot)
  ) {
    return(roster)
  }
  
  scenario <- transaction_state$snapshot()
  
  if (
    !isTRUE(scenario$active) ||
    !identical(
      as.character(scenario$scenario_type),
      "trade"
    )
  ) {
    return(roster)
  }
  
  team_name <- as.character(
    team_name
  )
  
  outgoing <- data.frame()
  incoming <- data.frame()
  
  if (
    identical(
      team_name,
      as.character(scenario$team)
    )
  ) {
    outgoing <- scenario$outgoing_players
    incoming <- scenario$incoming_players
  } else if (
    identical(
      team_name,
      as.character(scenario$partner_team)
    )
  ) {
    outgoing <- scenario$incoming_players
    incoming <- scenario$outgoing_players
  } else {
    return(roster)
  }
  
  preview <- roster
  
  if (
    nrow(outgoing) &&
    "player_id" %in% names(preview) &&
    "player_id" %in% names(outgoing)
  ) {
    preview <- preview[
      !preview$player_id %in%
        outgoing$player_id,
      ,
      drop = FALSE
    ]
  }
  
  if (nrow(incoming)) {
    
    common <- intersect(
      names(preview),
      names(incoming)
    )
    
    if (length(common)) {
      
      incoming_add <- incoming[
        ,
        common,
        drop = FALSE
      ]
      
      missing_preview <- setdiff(
        names(preview),
        names(incoming_add)
      )
      
      for (nm in missing_preview) {
        incoming_add[[nm]] <- NA
      }
      
      incoming_add <- incoming_add[
        ,
        names(preview),
        drop = FALSE
      ]
      
      preview <- rbind(
        preview,
        incoming_add
      )
    }
  }
  
  rownames(preview) <- NULL
  
  preview
}


#' Apply the active scenario to a payroll total.
#'
#' @param current_payroll Numeric current payroll.
#' @param transaction_state Shared transaction state.
#' @param team_name Team whose proposed payroll should be returned.
#' @noRd
tbi_apply_trade_scenario_to_payroll <- function(
    current_payroll,
    transaction_state,
    team_name) {
  
  current_payroll <- suppressWarnings(
    as.numeric(
      current_payroll
    )
  )
  
  if (
    !length(current_payroll) ||
    is.na(current_payroll)
  ) {
    current_payroll <- 0
  }
  
  if (
    is.null(transaction_state) ||
    is.null(transaction_state$snapshot)
  ) {
    return(current_payroll)
  }
  
  scenario <- transaction_state$snapshot()
  
  if (
    !isTRUE(scenario$active) ||
    !identical(
      as.character(scenario$scenario_type),
      "trade"
    )
  ) {
    return(current_payroll)
  }
  
  if (
    identical(
      as.character(team_name),
      as.character(scenario$team)
    )
  ) {
    return(
      current_payroll +
        scenario$salary_delta
    )
  }
  
  if (
    identical(
      as.character(team_name),
      as.character(scenario$partner_team)
    )
  ) {
    return(
      current_payroll -
        scenario$salary_delta
    )
  }
  
  current_payroll
}
