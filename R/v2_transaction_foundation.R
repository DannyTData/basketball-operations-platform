# ============================================================
# TBI V2 governed transaction foundation
#
# This layer is intentionally additive and session/staging only. It does not
# replace the protected V1 transaction evaluator and never writes a database.
# ============================================================

v2_tx_scalar <- function(value, field) {
  value <- trimws(as.character(value %||% ""))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(field, " must be a non-empty scalar string.", call. = FALSE)
  }
  value
}

v2_tx_status <- function(value, allowed, field) {
  value <- toupper(v2_tx_scalar(value, field))
  if (!value %in% allowed) stop(field, " is not supported.", call. = FALSE)
  value
}

v2_tx_routes <- function(routes, type = "player") {
  if (is.null(routes)) routes <- data.frame()
  if (!is.data.frame(routes)) stop(type, " routes must be a data frame.", call. = FALSE)
  if (!nrow(routes)) return(routes)
  required <- c("route_id", paste0(type, "_id"), "from_team_id", "to_team_id")
  if (!all(required %in% names(routes))) {
    stop(type, " routes are missing required fields.", call. = FALSE)
  }
  for (field in required) routes[[field]] <- trimws(as.character(routes[[field]]))
  order_index <- order(routes$route_id, method = "radix")
  rownames(routes) <- NULL
  routes[order_index, , drop = FALSE]
}

#' Construct a governed transaction fact event.
#' @noRd
new_v2_transaction_event <- function(event_id,
                                     effective_date,
                                     source,
                                     source_version,
                                     verification_status,
                                     teams,
                                     player_routes = data.frame(),
                                     draft_asset_routes = data.frame(),
                                     contract_actions = list(),
                                     cash_actions = list(),
                                     exception_actions = list(),
                                     notes = NULL,
                                     missing_facts = character()) {
  event_id <- v2_tx_scalar(event_id, "event_id")
  effective_date <- v2_iso_date(effective_date, "effective_date", required = TRUE)
  source <- v2_tx_scalar(source, "source")
  source_version <- v2_tx_scalar(source_version, "source_version")
  verification_status <- v2_tx_status(
    verification_status, c("VERIFIED", "UNVERIFIED", "REQUIRES REVIEW"), "verification_status"
  )
  teams <- sort(unique(trimws(as.character(teams))), method = "radix")
  if (length(teams) < 2L || length(teams) > 4L || any(!nzchar(teams))) {
    stop("teams must contain two to four unique team identifiers.", call. = FALSE)
  }
  player_routes <- v2_tx_routes(player_routes, "player")
  draft_asset_routes <- v2_tx_routes(draft_asset_routes, "asset")
  validate_event_routes <- function(routes, identity, label) {
    if (!nrow(routes)) return(invisible(TRUE))
    if (any(!routes$from_team_id %in% teams) || any(!routes$to_team_id %in% teams) ||
        any(routes$from_team_id == routes$to_team_id)) {
      stop(label, " routes must use explicit distinct participating teams.", call. = FALSE)
    }
    if (anyDuplicated(routes$route_id) || anyDuplicated(routes[[identity]])) {
      stop(label, " routes cannot duplicate a route or routed identity.", call. = FALSE)
    }
    invisible(TRUE)
  }
  validate_event_routes(player_routes, "player_id", "player")
  validate_event_routes(draft_asset_routes, "asset_id", "asset")
  body <- list(
    event_id = event_id, event_version = "1.0.0", effective_date = effective_date,
    source = source, source_version = source_version,
    verification_status = verification_status, teams = teams,
    player_routes = player_routes, draft_asset_routes = draft_asset_routes,
    contract_actions = contract_actions, cash_actions = cash_actions,
    exception_actions = exception_actions, notes = as.character(notes %||% "")[[1]],
    missing_facts = sort(unique(as.character(missing_facts)), method = "radix")
  )
  c(list(contract_type = "tbi-v2-transaction-event"), body,
    list(signature = v2_input_signature(body)))
}

#' Stage a roster refresh preview without authoritative mutation.
#' @noRd
stage_v2_transaction_refresh <- function(event, roster) {
  if (!is.list(event) || !identical(event$contract_type, "tbi-v2-transaction-event")) {
    stop("event must be a V2 transaction event.", call. = FALSE)
  }
  if (!is.data.frame(roster) || !all(c("player_id", "team_id") %in% names(roster))) {
    stop("roster must contain player_id and team_id.", call. = FALSE)
  }
  after <- roster
  after$player_id <- as.character(after$player_id)
  after$team_id <- as.character(after$team_id)
  findings <- list()
  routes <- event$player_routes
  if (nrow(routes)) {
    for (i in seq_len(nrow(routes))) {
      index <- which(after$player_id == routes$player_id[[i]])
      if (length(index) != 1L || !identical(after$team_id[[index]], routes$from_team_id[[i]])) {
        findings[[length(findings) + 1L]] <- new_v2_validation_finding(
          "REFRESH_SOURCE_OWNERSHIP_CONFLICT", "FAIL",
          paste("Roster does not verify source ownership for route", routes$route_id[[i]]), TRUE,
          path = paste0("player_routes/", routes$route_id[[i]])
        )
      } else {
        after$team_id[[index]] <- routes$to_team_id[[i]]
      }
    }
  }
  if (!identical(event$verification_status, "VERIFIED") || length(event$missing_facts)) {
    findings[[length(findings) + 1L]] <- new_v2_validation_finding(
      "REFRESH_FACTS_REQUIRE_REVIEW", "REVIEW",
      "The event is staged but its source facts are not fully verified.", FALSE,
      missing_fields = event$missing_facts
    )
  }
  validation <- aggregate_v2_validation(findings)
  state <- if (validation$status == "FAIL") "REQUIRES REVIEW" else if (
    validation$status == "REVIEW"
  ) "REQUIRES REVIEW" else "READY TO APPLY"
  list(
    contract_type = "tbi-v2-transaction-refresh-preview", contract_version = "1.0.0",
    mode = "DRY RUN", state = state, authoritative_apply = FALSE,
    event = event, roster_before = roster, roster_after = after,
    validation = validation,
    invalidations = c("DEPTH_CHART", "STARTER_LOCKS", "ROTATION", "MINUTES",
      "LINEUP_PORTFOLIO", "CAP", "DRAFT", "FIVE_YEAR", "COMMAND_CENTER"),
    signature = v2_input_signature(list(event = event$signature, roster_after = after))
  )
}

#' Create a governed team exception ledger.
#' @noRd
new_v2_team_exception_ledger <- function(entries = data.frame()) {
  required <- c(
    "team_id", "season", "exception_id", "exception_type", "original_amount",
    "remaining_amount", "creation_transaction", "creation_date", "expiration_date",
    "status", "source", "source_version", "verification_status", "use_restrictions"
  )
  if (is.data.frame(entries) && !nrow(entries) && !length(names(entries))) {
    entries <- setNames(
      as.data.frame(matrix(nrow = 0L, ncol = length(required)), stringsAsFactors = FALSE),
      required
    )
  }
  if (!is.data.frame(entries) || !all(required %in% names(entries))) {
    stop("entries are missing required exception-ledger fields.", call. = FALSE)
  }
  entries <- entries[, required, drop = FALSE]
  if (anyDuplicated(entries$exception_id)) stop("exception_id must be unique.", call. = FALSE)
  entries$exception_type <- toupper(as.character(entries$exception_type))
  if (any(entries$exception_type != "TRADED_PLAYER_EXCEPTION")) {
    stop("Only TRADED_PLAYER_EXCEPTION is supported in version 1.0.0.", call. = FALSE)
  }
  entries$original_amount <- suppressWarnings(as.numeric(entries$original_amount))
  entries$remaining_amount <- suppressWarnings(as.numeric(entries$remaining_amount))
  if (any(!is.finite(entries$original_amount)) || any(!is.finite(entries$remaining_amount)) ||
      any(entries$original_amount < 0) || any(entries$remaining_amount < 0) ||
      any(entries$remaining_amount > entries$original_amount)) {
    stop("Exception amounts must be finite and remaining cannot exceed original.", call. = FALSE)
  }
  entries$status <- toupper(as.character(entries$status))
  if (any(!entries$status %in% c("ACTIVE", "CONSUMED", "EXPIRED", "UNKNOWN"))) {
    stop("Exception status is not supported.", call. = FALSE)
  }
  entries$verification_status <- toupper(as.character(entries$verification_status))
  if (any(!entries$verification_status %in% c("VERIFIED", "REQUIRES SOURCE VERIFICATION"))) {
    stop("Exception verification status is not supported.", call. = FALSE)
  }
  for (i in seq_len(nrow(entries))) {
    entries$creation_date[[i]] <- v2_iso_date(entries$creation_date[[i]], "creation_date", TRUE)
    entries$expiration_date[[i]] <- v2_iso_date(entries$expiration_date[[i]], "expiration_date", TRUE)
    if (entries$expiration_date[[i]] < entries$creation_date[[i]]) {
      stop("expiration_date cannot precede creation_date.", call. = FALSE)
    }
  }
  entries <- entries[order(entries$team_id, entries$expiration_date, entries$exception_id,
    method = "radix"), , drop = FALSE]
  rownames(entries) <- NULL
  list(
    contract_type = "tbi-v2-team-exception-ledger", contract_version = "1.0.0",
    entries = entries, usage_history = data.frame(),
    signature = v2_input_signature(entries), authoritative = TRUE
  )
}

#' Apply user-scenario exception use to an isolated copy.
#' @noRd
apply_v2_scenario_exceptions <- function(authoritative_ledger,
                                         usage_requests = data.frame(),
                                         as_of_date,
                                         creation_facts = data.frame(),
                                         transaction_graph = NULL) {
  if (!is.list(authoritative_ledger) ||
      !identical(authoritative_ledger$contract_type, "tbi-v2-team-exception-ledger")) {
    stop("authoritative_ledger must be a V2 team exception ledger.", call. = FALSE)
  }
  as_of_date <- v2_iso_date(as_of_date, "as_of_date", TRUE)
  required <- c("route_id", "team_id", "exception_id", "amount")
  if (!is.data.frame(usage_requests) || (nrow(usage_requests) && !all(required %in% names(usage_requests)))) {
    stop("usage_requests are missing required fields.", call. = FALSE)
  }
  if (nrow(usage_requests) && anyDuplicated(as.character(usage_requests$route_id))) {
    stop("Each exception usage request must have a unique route_id.", call. = FALSE)
  }
  entries <- authoritative_ledger$entries
  history <- data.frame(route_id = character(), team_id = character(), exception_id = character(),
    amount_used = numeric(), amount_remaining = numeric(), stringsAsFactors = FALSE)
  findings <- list()
  graph_valid <- is.list(transaction_graph) &&
    identical(transaction_graph$contract_type, "tbi-v2-transaction-graph")
  add_finding <- function(code, status, explanation, blocked = FALSE, path = NULL) {
    findings[[length(findings) + 1L]] <<- new_v2_validation_finding(
      code,
      status,
      explanation,
      blocked,
      path = path
    )
  }
  if (nrow(usage_requests)) for (i in seq_len(nrow(usage_requests))) {
    path <- paste0("usage_requests/", i)
    amount <- suppressWarnings(as.numeric(usage_requests$amount[[i]]))
    requested_team <- as.character(usage_requests$team_id[[i]])
    requested_exception <- as.character(usage_requests$exception_id[[i]])
    requested_route <- as.character(usage_requests$route_id[[i]])
    idx <- which(entries$exception_id == requested_exception &
      entries$team_id == requested_team)

    if (length(idx) != 1L) {
      add_finding(
        "TPE_NOT_AVAILABLE", "FAIL",
        "The requested exception and owner do not identify one ledger entry.",
        TRUE, path
      )
      next
    }
    if (!graph_valid) {
      add_finding(
        "TPE_TRANSACTION_REQUIRED", "FAIL",
        "TPE use requires the governed transaction graph that contains its incoming player route.",
        TRUE, path
      )
      next
    }

    owner <- as.character(entries$team_id[[idx]])
    if (!owner %in% as.character(transaction_graph$teams)) {
      add_finding(
        "TPE_OWNER_NOT_PARTICIPANT", "FAIL",
        "The TPE owner is not a participant in this transaction.",
        TRUE, path
      )
      next
    }

    graph_season <- trimws(as.character(transaction_graph$season %||% ""))
    entry_season <- trimws(as.character(entries$season[[idx]] %||% ""))
    if (
      length(graph_season) != 1L ||
      is.na(graph_season) ||
      !nzchar(graph_season) ||
      length(entry_season) != 1L ||
      is.na(entry_season) ||
      !identical(entry_season, graph_season)
    ) {
      add_finding(
        "TPE_SEASON_MISMATCH", "FAIL",
        "The TPE season must exactly match the governed transaction season.",
        TRUE, path
      )
      next
    }

    player_routes <- transaction_graph$player_routes
    asset_routes <- transaction_graph$asset_routes
    player_match <- if (is.data.frame(player_routes) && nrow(player_routes)) {
      which(as.character(player_routes$route_id) == requested_route)
    } else {
      integer()
    }
    asset_match <- if (is.data.frame(asset_routes) && nrow(asset_routes)) {
      which(as.character(asset_routes$route_id) == requested_route)
    } else {
      integer()
    }
    if (length(player_match) != 1L) {
      if (length(asset_match)) {
        add_finding(
          "TPE_ASSET_ONLY_USE", "FAIL",
          "A TPE cannot be consumed by an asset-only route; an incoming player salary route is required.",
          TRUE, path
        )
      } else {
        add_finding(
          "TPE_PLAYER_ROUTE_REQUIRED", "FAIL",
          "The TPE request does not identify one concrete incoming player route.",
          TRUE, path
        )
      }
      next
    }

    route <- player_routes[player_match, , drop = FALSE]
    from_team <- as.character(route$from_team_id[[1]])
    to_team <- as.character(route$to_team_id[[1]])
    if (
      !from_team %in% as.character(transaction_graph$teams) ||
      !to_team %in% as.character(transaction_graph$teams) ||
      identical(from_team, to_team) ||
      !identical(to_team, owner)
    ) {
      add_finding(
        "TPE_RECEIVER_MISMATCH", "FAIL",
        "The incoming player destination must be the participating TPE owner.",
        TRUE, path
      )
      next
    }

    route_salary <- if ("salary" %in% names(route)) {
      suppressWarnings(as.numeric(route$salary[[1]]))
    } else {
      NA_real_
    }
    if (!is.finite(route_salary) || route_salary <= 0) {
      add_finding(
        "TPE_INCOMING_SALARY_MISSING", "FAIL",
        "The incoming player route requires a finite positive supported salary.",
        TRUE, path
      )
      next
    }
    if (
      !is.finite(amount) ||
      amount <= 0 ||
      abs(amount - route_salary) > max(0.01, abs(route_salary) * 1e-9)
    ) {
      add_finding(
        "TPE_AMOUNT_ROUTE_MISMATCH", "FAIL",
        "The requested TPE amount must equal the supported incoming player salary.",
        TRUE, path
      )
      next
    }
    if (!identical(entries$status[[idx]], "ACTIVE")) {
      add_finding(
        "TPE_NOT_ACTIVE", "FAIL",
        "The selected exception is not active.",
        TRUE, path
      )
      next
    }
    if (!identical(entries$verification_status[[idx]], "VERIFIED")) {
      add_finding(
        "TPE_NOT_VERIFIED", "FAIL",
        "The selected exception is not supported by verified source facts.",
        TRUE, path
      )
      next
    }
    if (
      entries$creation_date[[idx]] > as_of_date ||
      entries$expiration_date[[idx]] < as_of_date
    ) {
      add_finding(
        "TPE_EXPIRED", "FAIL",
        "The selected exception is outside its active date window.",
        TRUE, path
      )
      next
    }
    if (amount > entries$remaining_amount[[idx]]) {
      add_finding(
        "TPE_INSUFFICIENT", "FAIL",
        "The selected exception has insufficient remaining amount for this incoming salary.",
        TRUE, path
      )
      next
    }

    restrictions <- toupper(trimws(as.character(entries$use_restrictions[[idx]] %||% "")))
    if (!nzchar(restrictions) || restrictions %in% c(
      "UNKNOWN", "REQUIRES REVIEW", "REQUIRES SOURCE VERIFICATION"
    )) {
      add_finding(
        "TPE_RESTRICTIONS_UNKNOWN", "REVIEW",
        "Controlling TPE use restrictions require source verification before approval.",
        FALSE, path
      )
    }

    entries$remaining_amount[[idx]] <- entries$remaining_amount[[idx]] - amount
    if (entries$remaining_amount[[idx]] == 0) entries$status[[idx]] <- "CONSUMED"
    history <- rbind(history, data.frame(
      route_id = as.character(usage_requests$route_id[[i]]),
      team_id = entries$team_id[[idx]], exception_id = entries$exception_id[[idx]],
      amount_used = amount, amount_remaining = entries$remaining_amount[[idx]],
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(creation_facts)) {
    created <- new_v2_team_exception_ledger(creation_facts)$entries
    if (any(created$verification_status != "VERIFIED")) {
      findings[[length(findings) + 1L]] <- new_v2_validation_finding(
        "TPE_CREATION_REQUIRES_SOURCE", "REVIEW",
        "A scenario exception can be created only from verified rule facts."
      )
    } else if (any(created$exception_id %in% entries$exception_id)) {
      findings[[length(findings) + 1L]] <- new_v2_validation_finding(
        "TPE_CREATION_DUPLICATE_ID", "FAIL",
        "A scenario exception cannot reuse an authoritative exception_id.", TRUE
      )
    } else entries <- rbind(entries, created)
  }
  validation <- aggregate_v2_validation(findings)
  scenario <- list(
    contract_type = "tbi-v2-scenario-exception-ledger", contract_version = "1.0.0",
    entries = entries, usage_history = history, authoritative = FALSE,
    source_signature = authoritative_ledger$signature,
    transaction_id = if (graph_valid) transaction_graph$transaction_id else NULL,
    transaction_signature = if (graph_valid) transaction_graph$signature else NULL
  )
  scenario$signature <- v2_input_signature(scenario)
  list(status = validation$status, is_blocked = validation$is_blocked,
    scenario_ledger = scenario, validation = validation)
}

#' Normalize a deterministic two-to-four-team transaction graph.
#' @noRd
normalize_transaction_graph <- function(transaction_id,
                                         teams,
                                         player_routes = data.frame(),
                                         asset_routes = data.frame(),
                                         cash_routes = data.frame(),
                                         exception_routes = data.frame(),
                                         mechanisms = data.frame(),
                                         season = NULL) {
  transaction_id <- v2_tx_scalar(transaction_id, "transaction_id")
  season <- if (is.null(season) || !length(season)) {
    NULL
  } else {
    v2_tx_scalar(season, "season")
  }
  teams <- sort(unique(trimws(as.character(teams))), method = "radix")
  if (length(teams) < 2L || length(teams) > 4L || any(!nzchar(teams))) {
    stop("teams must contain two to four unique identifiers.", call. = FALSE)
  }
  player_routes <- v2_tx_routes(player_routes, "player")
  asset_routes <- v2_tx_routes(asset_routes, "asset")
  sort_optional <- function(x) {
    if (is.null(x)) return(data.frame())
    if (!is.data.frame(x)) stop("Optional routes must be data frames.", call. = FALSE)
    if (nrow(x) && "route_id" %in% names(x)) x <- x[order(x$route_id, method = "radix"), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  body <- list(transaction_id = transaction_id, season = season, teams = teams,
    player_routes = player_routes, asset_routes = asset_routes,
    cash_routes = sort_optional(cash_routes), exception_routes = sort_optional(exception_routes),
    mechanisms = sort_optional(mechanisms))
  c(list(contract_type = "tbi-v2-transaction-graph", contract_version = "1.0.0"),
    body, list(signature = v2_input_signature(body)))
}

#' Validate global transaction-route invariants.
#' @noRd
validate_transaction_routes <- function(graph, ownership = NULL) {
  if (!is.list(graph) || !identical(graph$contract_type, "tbi-v2-transaction-graph")) {
    stop("graph must be a V2 transaction graph.", call. = FALSE)
  }
  findings <- list()
  check_routes <- function(routes, identity, label) {
    if (!nrow(routes)) return()
    if (any(!routes$from_team_id %in% graph$teams) || any(!routes$to_team_id %in% graph$teams) ||
        any(routes$from_team_id == routes$to_team_id)) {
      findings[[length(findings) + 1L]] <<- new_v2_validation_finding(
        paste0("INVALID_", label, "_ROUTE"), "FAIL",
        paste(label, "routes must use explicit distinct participating teams."), TRUE
      )
    }
    if (anyDuplicated(routes[[identity]]) || anyDuplicated(routes$route_id)) {
      findings[[length(findings) + 1L]] <<- new_v2_validation_finding(
        paste0("DUPLICATE_", label, "_ROUTE"), "FAIL",
        paste("A", tolower(label), "or route cannot be routed twice."), TRUE
      )
    }
  }
  check_routes(graph$player_routes, "player_id", "PLAYER")
  check_routes(graph$asset_routes, "asset_id", "ASSET")
  if (!is.null(ownership) && is.data.frame(ownership) && nrow(graph$player_routes)) {
    for (i in seq_len(nrow(graph$player_routes))) {
      owned <- ownership$team_id[match(graph$player_routes$player_id[[i]], ownership$player_id)]
      if (!length(owned) || is.na(owned) || owned != graph$player_routes$from_team_id[[i]]) {
        findings[[length(findings) + 1L]] <- new_v2_validation_finding(
          "PLAYER_OWNERSHIP_INVALID", "FAIL", "Source ownership is not verified.", TRUE,
          path = paste0("player_routes/", graph$player_routes$route_id[[i]])
        )
      }
    }
  }
  aggregate_v2_validation(findings)
}

v2_transaction_rule_ids <- function() c(
  "salary_matching", "cap_room_acquisition", "first_apron", "second_apron",
  "aggregation", "hard_cap", "traded_player_exception", "byc", "poison_pill",
  "trade_bonus", "recently_signed", "recently_traded", "sign_and_trade",
  "reacquisition", "roster_size", "cash", "draft_ownership", "stepien",
  "pick_protections", "pick_swaps", "conditional_picks", "conveyance", "timing"
)

#' Evaluate one team's source-backed rule facts; unknown rules remain REVIEW.
#' @noRd
evaluate_transaction_team <- function(team_id, graph, rule_facts = list()) {
  findings <- lapply(v2_transaction_rule_ids(), function(rule_id) {
    fact <- rule_facts[[rule_id]]
    if (is.null(fact)) {
      return(list(
        rule_id = rule_id, rule_version = "1.0.0", team = team_id,
        transaction = graph$transaction_id, status = "REVIEW", is_blocked = FALSE,
        controlling_facts = list(), missing_facts = rule_id,
        source = NULL, source_reference = NULL, effective_date = NULL,
        explanation = "Rule evidence is not available; source verification is required."
      ))
    }
    status <- v2_validation_status(fact$status %||% "REVIEW")
    blocked <- isTRUE(fact$is_blocked)
    if (blocked && status != "FAIL") stop("A blocking rule finding must FAIL.", call. = FALSE)
    list(
      rule_id = rule_id, rule_version = as.character(fact$rule_version %||% "1.0.0")[[1]],
      team = team_id, transaction = graph$transaction_id, status = status,
      is_blocked = blocked, controlling_facts = fact$controlling_facts %||% list(),
      missing_facts = fact$missing_facts %||% character(), source = fact$source %||% NULL,
      source_reference = fact$source_reference %||% NULL,
      effective_date = fact$effective_date %||% NULL,
      explanation = as.character(fact$explanation %||% "Rule fact supplied.")[[1]]
    )
  })
  statuses <- vapply(findings, `[[`, character(1), "status")
  blocked <- any(vapply(findings, function(x) isTRUE(x$is_blocked), logical(1)))
  status <- if (any(statuses == "FAIL")) "FAIL" else if (any(statuses == "REVIEW")) "REVIEW" else "PASS"
  list(team_id = team_id, status = status, is_blocked = blocked, findings = findings)
}

#' Evaluate all organizations without widening unsupported legality.
#' @noRd
evaluate_multiteam_transaction <- function(graph, rule_facts = list()) {
  route_validation <- validate_transaction_routes(graph)
  team_results <- setNames(lapply(graph$teams, function(team) {
    evaluate_transaction_team(team, graph, rule_facts[[team]] %||% list())
  }), graph$teams)
  team_statuses <- vapply(team_results, `[[`, character(1), "status")
  status <- if (route_validation$status == "FAIL" || any(team_statuses == "FAIL")) "FAIL" else if (
    route_validation$status == "REVIEW" || any(team_statuses == "REVIEW")
  ) "REVIEW" else "PASS"
  blocked <- route_validation$is_blocked || any(vapply(team_results, function(x) x$is_blocked, logical(1)))
  list(
    contract_type = "tbi-v2-transaction-evaluation", contract_version = "1.0.0",
    transaction_id = graph$transaction_id, status = status, is_blocked = blocked,
    route_validation = route_validation, team_results = team_results,
    signature = v2_input_signature(list(graph = graph$signature, team_results = team_results))
  )
}

#' Build a cross-domain scenario impact envelope.
#' @noRd
build_v2_organizational_impact <- function(graph, evaluation) {
  if (!is.list(evaluation) || !identical(evaluation$contract_type, "tbi-v2-transaction-evaluation")) {
    stop("evaluation must be a V2 transaction evaluation.", call. = FALSE)
  }
  team_impacts <- setNames(lapply(graph$teams, function(team) {
    routes <- graph$player_routes
    incoming <- routes[routes$to_team_id == team, , drop = FALSE]
    outgoing <- routes[routes$from_team_id == team, , drop = FALSE]
    salary <- function(x) if (nrow(x) && "salary" %in% names(x)) sum(as.numeric(x$salary), na.rm = TRUE) else NA_real_
    list(
      roster_delta = nrow(incoming) - nrow(outgoing),
      incoming_player_ids = incoming$player_id, outgoing_player_ids = outgoing$player_id,
      payroll_delta = salary(incoming) - salary(outgoing),
      basketball = "REQUIRES RECALCULATION", cap = "REQUIRES RECALCULATION",
      draft = if (nrow(graph$asset_routes)) "REQUIRES RECALCULATION" else "NO ROUTED ASSET",
      five_year = "REQUIRES RECALCULATION",
      cba = evaluation$team_results[[team]]$status
    )
  }), graph$teams)
  recommendation <- if (evaluation$status == "FAIL" || evaluation$is_blocked) {
    "DO NOT PROCEED"
  } else if (evaluation$status == "REVIEW") "PROCEED WITH REVIEW" else "PROCEED"
  list(
    contract_type = "tbi-v2-organizational-impact", contract_version = "1.0.0",
    transaction_id = graph$transaction_id, team_impacts = team_impacts,
    executive_recommendation = recommendation,
    evidence_classes = c("FACT", "RULE", "MODEL OUTPUT", "ASSUMPTION", "UNKNOWN", "USER OVERRIDE"),
    cba_controls_recommendation = TRUE,
    signature = v2_input_signature(list(graph = graph$signature, evaluation = evaluation$signature))
  )
}

#' Reconcile staged current-roster facts without writing authority.
#' @noRd
reconcile_v2_current_rosters <- function(authoritative, external, source, source_version) {
  required <- c("player_id", "team_id")
  if (!is.data.frame(authoritative) || !is.data.frame(external) ||
      !all(required %in% names(authoritative)) || !all(required %in% names(external))) {
    stop("Both roster inputs require player_id and team_id.", call. = FALSE)
  }
  source <- v2_tx_scalar(source, "source")
  source_version <- v2_tx_scalar(source_version, "source_version")
  ids <- sort(unique(c(as.character(authoritative$player_id), as.character(external$player_id))), method = "radix")
  rows <- lapply(ids, function(id) {
    a <- authoritative$team_id[match(id, as.character(authoritative$player_id))]
    e <- external$team_id[match(id, as.character(external$player_id))]
    a <- if (!length(a)) NA_character_ else as.character(a[[1]])
    e <- if (!length(e)) NA_character_ else as.character(e[[1]])
    status <- if (is.na(a) || is.na(e)) {
      if (is.na(a) && !is.na(e)) "STALE" else "UNKNOWN"
    } else if (identical(a, e)) "CURRENT" else "CONFLICT"
    data.frame(player_id = id, authoritative_team_id = a, external_team_id = e,
      status = status, stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows)
  list(
    contract_type = "tbi-v2-current-data-reconciliation", contract_version = "1.0.0",
    source = source, source_version = source_version, rows = rows,
    authoritative_apply = FALSE,
    overall_status = if (all(rows$status == "CURRENT")) "CURRENT" else "REQUIRES REVIEW",
    signature = v2_input_signature(rows)
  )
}

#' Governed NBA team-media identity registry.
#'
#' URLs remain absent until an asset source/license is explicitly approved.
#' @noRd
v2_team_media_registry <- function() {
  abbreviations <- c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
    "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
    "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
  names <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
    "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
    "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
    "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
    "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
    "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
    "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
    "Utah Jazz", "Washington Wizards")
  data.frame(
    canonical_team_name = names, official_abbreviation = abbreviations,
    asset_url = NA_character_, source = NA_character_, source_version = NA_character_,
    verification_status = "REQUIRES SOURCE VERIFICATION", stringsAsFactors = FALSE
  )
}

#' Resolve a player headshot only from a supplied verified ID registry.
#' @noRd
v2_player_headshot_record <- function(player_id, registry = data.frame()) {
  player_id <- trimws(as.character(player_id %||% ""))
  required <- c("player_id", "asset_url", "source", "source_version", "verification_status")
  if (length(player_id) != 1L || !nzchar(player_id) || !is.data.frame(registry) ||
      !all(required %in% names(registry))) return(NULL)
  row <- registry[as.character(registry$player_id) == player_id &
      registry$verification_status == "VERIFIED", , drop = FALSE]
  if (nrow(row) != 1L || !nzchar(as.character(row$asset_url[[1]]))) return(NULL)
  as.list(row[1, , drop = FALSE])
}
