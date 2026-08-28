# PHASE 15H — DRAFT ASSETS IN SHARED TRADE STATE
# ------------------------------------------------------------
# Shared Transaction / Scenario State
# Thompson Basketball Intelligence
# ------------------------------------------------------------

tbi_scenario_scope_value <- function(scenario) {
  if (!is.list(scenario) || !isTRUE(scenario$active)) {
    return(NULL)
  }

  explicit_scope <- as.character(scenario$scenario_scope %||% "")
  if (length(explicit_scope) && explicit_scope[[1]] %in% c("SHARED_SUPPORTED", "TRADE_LOCAL")) {
    return(explicit_scope[[1]])
  }

  if (identical(as.character(scenario$scenario_type), "v2_multiteam_trade")) {
    return("TRADE_LOCAL")
  }

  if (identical(as.character(scenario$scenario_type), "trade")) {
    return("SHARED_SUPPORTED")
  }

  NULL
}

tbi_scenario_is_shared_supported <- function(scenario) {
  identical(tbi_scenario_scope_value(scenario), "SHARED_SUPPORTED")
}

tbi_scenario_is_trade_local <- function(scenario) {
  identical(tbi_scenario_scope_value(scenario), "TRADE_LOCAL")
}

tbi_scenario_matches_season <- function(scenario, selected_season) {
  if (!is.list(scenario) || !isTRUE(scenario$active)) {
    return(TRUE)
  }

  selected_season <- trimws(as.character(selected_season %||% ""))
  scenario_season <- trimws(as.character(
    scenario$season %||% scenario$v2_transaction_graph$season %||% ""
  ))
  length(selected_season) == 1L &&
    !is.na(selected_season) &&
    nzchar(selected_season) &&
    length(scenario_season) == 1L &&
    !is.na(scenario_season) &&
    nzchar(scenario_season) &&
    identical(scenario_season, selected_season)
}

#' Authorize an authoritative write from the current scenario context.
#'
#' Exploratory scenario state is session-scoped.  No active scenario may
#' silently persist its derived state to the authoritative database.  The
#' supported two-team flow retains its working-preview behavior, while all
#' other active envelopes fail closed as unsupported.
#'
#' @noRd
tbi_authorize_official_write <- function(
    scenario = NULL,
    operation = "Official write") {
  operation <- as.character(operation %||% "Official write")
  operation <- if (
    length(operation) &&
      !is.na(operation[[1]]) &&
      nzchar(trimws(operation[[1]]))
  ) {
    trimws(operation[[1]])
  } else {
    "Official write"
  }

  has_canonical_active <- is.list(scenario) &&
    length(scenario$active) == 1L &&
    is.logical(scenario$active) &&
    !is.na(scenario$active)
  state_unavailable <- !is.null(scenario) && (
    !has_canonical_active || isTRUE(scenario$state_unavailable)
  )
  scenario_scope <- tbi_scenario_scope_value(scenario)

  if (state_unavailable) {
    return(list(
      allowed = FALSE,
      ok = FALSE,
      status = "BLOCKED",
      code = "SCENARIO_STATE_UNAVAILABLE",
      operation = operation,
      scenario_scope = scenario_scope,
      message = paste(
        "REVIEW / BLOCKED: Transaction scenario state is unavailable or invalid.",
        operation,
        "cannot modify the authoritative depth chart until scenario state is restored.",
        "No official records were changed."
      )
    ))
  }

  active <- isTRUE(scenario$active)

  if (!active) {
    return(list(
      allowed = TRUE,
      ok = TRUE,
      status = "PASS",
      code = "OFFICIAL_WRITE_ALLOWED",
      operation = operation,
      scenario_scope = NULL,
      message = paste(operation, "is authorized from the baseline state.")
    ))
  }

  if (identical(scenario_scope, "SHARED_SUPPORTED")) {
    return(list(
      allowed = FALSE,
      ok = FALSE,
      status = "BLOCKED",
      code = "ACTIVE_SCENARIO_WRITE_BLOCKED",
      operation = operation,
      scenario_scope = scenario_scope,
      message = paste(
        "BLOCKED:", operation,
        "is unavailable while a supported scenario preview is active.",
        "No official records were changed."
      )
    ))
  }

  scope_label <- if (identical(scenario_scope, "TRADE_LOCAL")) {
    "TRADE-LOCAL"
  } else {
    "unsupported or unknown"
  }

  list(
    allowed = FALSE,
    ok = FALSE,
    status = "BLOCKED",
    code = "UNSUPPORTED_ACTIVE_SCENARIO",
    operation = operation,
    scenario_scope = scenario_scope,
    message = paste(
      "REVIEW / BLOCKED:", operation,
      "cannot modify the authoritative depth chart while a",
      scope_label,
      "scenario is active.",
      "Clear or reset the scenario before saving official records.",
      "No official records were changed."
    )
  )
}

#' Create the shared front-office scenario state.
#'
#' The object is intentionally session-scoped. It stores a proposed
#' transaction without writing anything to the database.
#'
#' @noRd
tbi_transaction_state <- function() {

  canonical_trade_rows <- function(x) {
    x <- if (is.data.frame(x)) x else data.frame()
    columns <- sort(names(x), method = "radix")
    x <- x[, columns, drop = FALSE]

    if (nrow(x)) {
      row_signatures <- vapply(
        seq_len(nrow(x)),
        function(index) v2_canonical_text(as.list(x[index, , drop = FALSE])),
        character(1)
      )
      x <- x[order(row_signatures, method = "radix"), , drop = FALSE]
    }

    rownames(x) <- NULL
    x
  }

  two_team_transaction_identity <- function(team,
                                             partner_team,
                                             season,
                                             outgoing_players,
                                             incoming_players,
                                             outgoing_draft_assets,
                                             incoming_draft_assets) {
    body <- list(
      scenario_type = "trade",
      team = as.character(team),
      partner_team = as.character(partner_team),
      season = as.character(season),
      outgoing_players = canonical_trade_rows(outgoing_players),
      incoming_players = canonical_trade_rows(incoming_players),
      outgoing_draft_assets = canonical_trade_rows(outgoing_draft_assets),
      incoming_draft_assets = canonical_trade_rows(incoming_draft_assets)
    )

    list(
      signature = v2_input_signature(body),
      scenario_id = v2_state_id("trade", body)
    )
  }
  
  state <- shiny::reactiveValues(
    active = FALSE,
    scenario_id = NULL,
    source = NULL,
    scenario_type = NULL,
    scenario_scope = NULL,
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
    transaction_signature = NULL,
    evaluation_signature = NULL,
    v2_transaction_graph = NULL,
    v2_transaction_evaluation = NULL,
    v2_exception_scenario = NULL,
    v2_organizational_impact = NULL,
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
      scenario_scope = state$scenario_scope,
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
      transaction_signature = state$transaction_signature,
      evaluation_signature = state$evaluation_signature,
      v2_transaction_graph = state$v2_transaction_graph,
      v2_transaction_evaluation = state$v2_transaction_evaluation,
      v2_exception_scenario = state$v2_exception_scenario,
      v2_organizational_impact = state$v2_organizational_impact,
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

    identity <- two_team_transaction_identity(
      team = team,
      partner_team = partner_team,
      season = season,
      outgoing_players = outgoing_players,
      incoming_players = incoming_players,
      outgoing_draft_assets = outgoing_draft_assets,
      incoming_draft_assets = incoming_draft_assets
    )
    now <- Sys.time()
    identity_changed <- !identical(
      state$transaction_signature,
      identity$signature
    )

    if (
      identity_changed ||
      is.null(state$scenario_id) ||
      is.null(state$created_at)
    ) {
      state$scenario_id <- identity$scenario_id
      state$created_at <- now
    }
    
    state$active <- TRUE
    state$source <- source
    state$scenario_type <- "trade"
    state$scenario_scope <- "SHARED_SUPPORTED"
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
    state$transaction_signature <- identity$signature
    state$evaluation_signature <- if (is.null(evaluation)) {
      NULL
    } else {
      identity$signature
    }
    state$v2_transaction_graph <- NULL
    state$v2_transaction_evaluation <- NULL
    state$v2_exception_scenario <- NULL
    state$v2_organizational_impact <- NULL
    state$updated_at <- now
    
    invisible(
      snapshot()
    )
  }

  publish_v2_transaction <- function(graph,
                                     evaluation,
                                     organizational_impact,
                                     exception_scenario = NULL,
                                     source = "V2 Trade Intelligence") {
    if (!is.list(graph) ||
        !identical(graph$contract_type, "tbi-v2-transaction-graph")) {
      stop("graph must be a V2 transaction graph.", call. = FALSE)
    }
    if (!is.list(evaluation) ||
        !identical(evaluation$contract_type, "tbi-v2-transaction-evaluation")) {
      stop("evaluation must be a V2 transaction evaluation.", call. = FALSE)
    }
    if (!is.list(organizational_impact) ||
        !identical(organizational_impact$contract_type, "tbi-v2-organizational-impact")) {
      stop("organizational_impact must be a V2 organizational impact.", call. = FALSE)
    }

    signature_error <- function(detail) {
      clear()
      stop(
        paste("V2 transaction signatures are invalid or cross-wired:", detail),
        call. = FALSE
      )
    }

    graph_body <- list(
      transaction_id = graph$transaction_id,
      season = graph$season %||% NULL,
      teams = graph$teams,
      player_routes = graph$player_routes,
      asset_routes = graph$asset_routes,
      cash_routes = graph$cash_routes,
      exception_routes = graph$exception_routes,
      mechanisms = graph$mechanisms
    )
    expected_graph_signature <- v2_input_signature(graph_body)
    expected_evaluation_signature <- v2_input_signature(list(
      graph = graph$signature,
      team_results = evaluation$team_results
    ))
    expected_impact_signature <- v2_input_signature(list(
      graph = graph$signature,
      evaluation = evaluation$signature
    ))

    if (!identical(graph$transaction_id, evaluation$transaction_id) ||
        !identical(graph$transaction_id, organizational_impact$transaction_id)) {
      signature_error("contract transaction IDs do not match.")
    }
    if (!identical(graph$signature, expected_graph_signature)) {
      signature_error("the graph signature does not match its canonical content.")
    }
    if (!identical(evaluation$signature, expected_evaluation_signature)) {
      signature_error("the evaluation signature does not match the graph and team results.")
    }
    if (!identical(organizational_impact$signature, expected_impact_signature)) {
      signature_error("the impact signature does not match the graph and evaluation.")
    }

    if (!is.null(exception_scenario)) {
      if (!is.list(exception_scenario)) {
        signature_error("the exception scenario is missing its signed ledger.")
      }
      ledger <- exception_scenario$scenario_ledger
      if (!is.list(ledger)) {
        signature_error("the exception scenario is missing its signed ledger.")
      }
      ledger_body <- ledger
      ledger_body$signature <- NULL
      expected_ledger_signature <- v2_input_signature(ledger_body)
      if (
        !identical(ledger$transaction_id, graph$transaction_id) ||
        !identical(ledger$transaction_signature, graph$signature) ||
        !identical(ledger$signature, expected_ledger_signature)
      ) {
        signature_error("the exception ledger does not match the transaction graph.")
      }
    }

    now <- Sys.time()
    identity_changed <- !identical(state$transaction_signature, graph$signature)
    state$active <- TRUE
    state$scenario_id <- graph$transaction_id
    state$source <- as.character(source)[[1]]
    state$scenario_type <- "v2_multiteam_trade"
    state$scenario_scope <- "TRADE_LOCAL"
    state$season <- graph$season %||% NULL
    state$transaction_signature <- graph$signature
    state$evaluation_signature <- evaluation$signature
    state$v2_transaction_graph <- graph
    state$v2_transaction_evaluation <- evaluation
    state$v2_exception_scenario <- exception_scenario
    state$v2_organizational_impact <- organizational_impact
    if (identity_changed || is.null(state$created_at)) state$created_at <- now
    state$updated_at <- now
    invisible(snapshot())
  }

  set_v2_exception_scenario <- function(exception_scenario) {
    if (!isTRUE(state$active)) {
      stop("An active transaction is required before attaching exception state.", call. = FALSE)
    }
    if (!is.null(exception_scenario) && !is.list(exception_scenario)) {
      stop("exception_scenario must be a governed scenario result or NULL.", call. = FALSE)
    }
    state$v2_exception_scenario <- exception_scenario
    state$updated_at <- Sys.time()
    invisible(snapshot())
  }
  
  clear <- function() {
    
    state$active <- FALSE
    state$scenario_id <- NULL
    state$source <- NULL
    state$scenario_type <- NULL
    state$scenario_scope <- NULL
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
    state$transaction_signature <- NULL
    state$evaluation_signature <- NULL
    state$v2_transaction_graph <- NULL
    state$v2_transaction_evaluation <- NULL
    state$v2_exception_scenario <- NULL
    state$v2_organizational_impact <- NULL
    state$created_at <- NULL
    state$updated_at <- NULL
    
    invisible(TRUE)
  }
  
  list(
    state = state,
    snapshot = snapshot,
    publish_trade = publish_trade,
    publish_v2_transaction = publish_v2_transaction,
    set_v2_exception_scenario = set_v2_exception_scenario,
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
  
  isTRUE(snapshot$active) && tbi_scenario_is_shared_supported(snapshot)
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
    !tbi_scenario_is_shared_supported(scenario) ||
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
    !tbi_scenario_is_shared_supported(scenario) ||
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
