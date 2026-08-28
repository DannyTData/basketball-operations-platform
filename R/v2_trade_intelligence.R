# V2 Trade Intelligence workbench. The protected V1 two-team builder is
# mounted inside this shell; all added state remains session-scoped.

v2_trade_money <- function(value) {
  value <- suppressWarnings(as.numeric(value %||% 0))
  if (!length(value) || !is.finite(value[[1]])) return("UNKNOWN")
  amount <- value[[1]]
  if (abs(amount) >= 1e9) return(sprintf("$%.1fB", amount / 1e9))
  if (abs(amount) >= 1e6) return(sprintf("$%.1fM", amount / 1e6))
  if (abs(amount) >= 1e3) return(sprintf("$%.1fK", amount / 1e3))
  sprintf("$%.0f", amount)
}

v2_trade_empty <- function(title, copy) {
  shiny::div(class = "tbi-trade-empty", shiny::strong(title), shiny::p(copy))
}

v2_trade_test_mode_enabled <- function(value = Sys.getenv("TBI_ENABLE_TPE_TEST_MODE", "")) {
  domain <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (
    (!is.null(domain) && !is.null(domain$userData) && isTRUE(domain$userData$tbi_feedback_mode)) ||
    tolower(trimws(Sys.getenv("TBI_FEEDBACK_MODE", "false"))) %in% c("1", "true", "yes", "on")
  ) {
    return(FALSE)
  }
  tolower(trimws(as.character(value %||% ""))[[1]]) %in% c("1", "true", "yes", "on")
}

v2_trade_canonical_result <- function(scenario) {
  if (!is.list(scenario) || !isTRUE(scenario$active)) return(NULL)

  scenario_type <- as.character(scenario$scenario_type %||% "")
  scenario_id <- as.character(scenario$scenario_id %||% "")
  if (length(scenario_type) != 1L || length(scenario_id) != 1L || !nzchar(scenario_id)) {
    return(NULL)
  }

  if (identical(scenario_type, "trade")) {
    evaluation <- scenario$evaluation
    transaction_signature <- as.character(scenario$transaction_signature %||% "")
    evaluation_signature <- as.character(scenario$evaluation_signature %||% "")
    team <- as.character(scenario$team %||% "")
    partner <- as.character(scenario$partner_team %||% "")
    if (
      !is.list(evaluation) ||
      length(transaction_signature) != 1L ||
      !nzchar(transaction_signature) ||
      !identical(transaction_signature, evaluation_signature) ||
      !identical(as.character(evaluation$team_a_name %||% ""), team) ||
      !identical(as.character(evaluation$team_b_name %||% ""), partner)
    ) {
      return(NULL)
    }
    return(list(kind = "two_team", scenario = scenario))
  }

  if (!identical(scenario_type, "v2_multiteam_trade")) return(NULL)

  graph <- scenario$v2_transaction_graph
  evaluation <- scenario$v2_transaction_evaluation
  impact <- scenario$v2_organizational_impact
  if (!is.list(graph) || !is.list(evaluation) || !is.list(impact)) return(NULL)

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
  ids_match <- identical(scenario_id, as.character(graph$transaction_id %||% "")) &&
    identical(graph$transaction_id, evaluation$transaction_id) &&
    identical(graph$transaction_id, impact$transaction_id)
  signatures_match <- identical(graph$signature, expected_graph_signature) &&
    identical(evaluation$signature, expected_evaluation_signature) &&
    identical(impact$signature, expected_impact_signature) &&
    identical(scenario$transaction_signature, graph$signature) &&
    identical(scenario$evaluation_signature, evaluation$signature)
  if (!ids_match || !signatures_match) return(NULL)

  exceptions <- scenario$v2_exception_scenario
  if (!is.null(exceptions)) {
    ledger <- exceptions$scenario_ledger
    if (!is.list(ledger)) return(NULL)
    ledger_body <- ledger
    ledger_body$signature <- NULL
    if (
      !identical(ledger$transaction_id, graph$transaction_id) ||
      !identical(ledger$transaction_signature, graph$signature) ||
      !identical(ledger$signature, v2_input_signature(ledger_body))
    ) {
      return(NULL)
    }
  }

  list(
    kind = "multi_team",
    scenario = scenario,
    graph = graph,
    evaluation = evaluation,
    impact = impact,
    exceptions = exceptions
  )
}

v2_trade_player_slots_per_team <- 5L
v2_trade_max_teams <- 4L
v2_trade_max_player_slots <- v2_trade_player_slots_per_team * v2_trade_max_teams

v2_trade_multi_mode <- function(value) {
  if (
    is.null(value) ||
    !is.atomic(value) ||
    is.object(value) ||
    length(value) != 1L ||
    is.na(value[[1L]])
  ) {
    return(NA_integer_)
  }

  normalized <- trimws(as.character(value[[1L]]))
  if (!normalized %in% c("3", "4")) {
    return(NA_integer_)
  }

  as.integer(normalized)
}

v2_trade_player_slot_indices <- function(team_index) {
  team_index <- as.integer(team_index)
  if (length(team_index) != 1L || is.na(team_index) || team_index < 1L || team_index > v2_trade_max_teams) {
    stop("team_index must identify one of the four transaction teams.", call. = FALSE)
  }
  start <- (team_index - 1L) * v2_trade_player_slots_per_team + 1L
  seq.int(start, length.out = v2_trade_player_slots_per_team)
}

v2_trade_route_source <- function(kind, route_index, teams) {
  route_index <- as.integer(route_index)
  team_index <- if (identical(kind, "player")) ceiling(route_index / v2_trade_player_slots_per_team) else route_index
  if (length(team_index) != 1L || is.na(team_index) || team_index < 1L || team_index > length(teams)) "" else teams[[team_index]]
}

v2_trade_destination_choices <- function(source_team, teams) {
  setdiff(as.character(teams), as.character(source_team))
}

v2_trade_evidence_statement <- function(evidence_class, text) {
  paste0("[", evidence_class, "] ", text)
}

v2_trade_asset_labels <- function(x) {
  if (!is.data.frame(x) || !nrow(x)) return(character())
  column <- intersect(c("draft_asset_id", "asset_id", "asset_name", "description", "pick_description"), names(x))
  if (!length(column)) return(paste(nrow(x), "routed draft asset(s)"))
  trimws(as.character(x[[column[[1]]]]))
}

v2_trade_exception_recommendation <- function(exception) {
  if (is.null(exception)) {
    return(list(summary = v2_trade_evidence_statement("FACT", "No TPE used."), reasons = character()))
  }
  if (inherits(exception, "tbi_trade_ui_error")) {
    return(list(
      summary = v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", exception$message),
      reasons = v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", exception$message)
    ))
  }
  ledger <- exception$scenario_ledger
  if (!is.list(ledger)) {
    return(list(
      summary = v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", "Scenario TPE ledger is missing from canonical transaction state."),
      reasons = v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", "Scenario TPE ledger is missing from canonical transaction state.")
    ))
  }
  usage <- ledger$usage_history
  entries <- ledger$entries
  lines <- character()
  if (is.data.frame(usage) && nrow(usage)) {
    lines <- vapply(seq_len(nrow(usage)), function(i) {
      v2_trade_evidence_statement("FACT", paste0(
        usage$exception_id[[i]], ": ", v2_trade_money(usage$amount_used[[i]]),
        " used; ", v2_trade_money(usage$amount_remaining[[i]]), " remaining."
      ))
    }, character(1))
  }
  if (is.data.frame(entries) && nrow(entries) && "use_restrictions" %in% names(entries)) {
    created <- grepl("DEVELOPMENT / SCENARIO ONLY", as.character(entries$use_restrictions), fixed = TRUE)
    if (any(created)) {
      created_lines <- vapply(which(created), function(i) {
        v2_trade_evidence_statement("FACT", paste0(
          "TPE created: ", entries$exception_id[[i]], " for ", v2_trade_money(entries$remaining_amount[[i]]),
          " remaining; DEVELOPMENT / SCENARIO ONLY."
        ))
      }, character(1))
      lines <- c(lines, created_lines)
    }
  }
  if (!length(lines)) lines <- v2_trade_evidence_statement("FACT", "No TPE used.")
  status_line <- v2_trade_evidence_statement("RULE RESULT", paste0(
    "Scenario TPE evaluation: ", as.character(exception$status %||% "REVIEW")[[1]], "."
  ))
  list(summary = c(status_line, lines), reasons = if (!is.null(exception$status)) status_line else character())
}

v2_trade_controlling_decision <- function(scenario) {
  result <- function(status, code, basis) {
    status <- as.character(status)[[1]]
    list(
      status = status,
      code = code,
      is_blocked = identical(status, "FAIL"),
      decision = if (identical(status, "FAIL")) {
        "DO NOT PROCEED"
      } else if (identical(status, "REVIEW")) {
        "REVIEW REQUIRED"
      } else {
        "PROCEED"
      },
      basis = basis
    )
  }

  if (!is.list(scenario) || !isTRUE(scenario$active)) {
    return(result(
      "REVIEW",
      "NO_ACTIVE_TRANSACTION",
      "No active transaction is available for a controlling decision."
    ))
  }

  status_value <- function(value) {
    value <- toupper(trimws(as.character(value %||% "")))
    if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) {
      return("UNKNOWN")
    }
    value <- value[[1]]
    if (identical(value, "BLOCK")) "FAIL" else if (
      value %in% c("NOT USED", "NOT_USED")
    ) "PASS" else value
  }
  is_failure <- function(value) status_value(value) == "FAIL"
  is_review <- function(value) status_value(value) %in% c(
    "REVIEW", "UNKNOWN", "ERROR", "MISSING"
  )

  exception <- scenario$v2_exception_scenario
  exception_failed <- is.list(exception) && (
    is_failure(exception$status) || isTRUE(exception$is_blocked)
  )
  exception_review <- inherits(exception, "tbi_trade_ui_error") || (
    is.list(exception) && is_review(exception$status)
  )

  multi_evaluation <- scenario$v2_transaction_evaluation
  if (!is.null(multi_evaluation)) {
    team_results <- multi_evaluation$team_results
    findings <- if (is.list(team_results)) {
      unlist(lapply(team_results, function(x) x$findings %||% list()), recursive = FALSE)
    } else {
      list()
    }
    finding_status <- function(x) status_value(x$status %||% "UNKNOWN")
    finding_failed <- vapply(
      findings,
      function(x) finding_status(x) == "FAIL" ||
        isTRUE(x$is_blocked) ||
        isTRUE(x$is_blocking),
      logical(1)
    )
    finding_review <- vapply(
      findings,
      function(x) finding_status(x) == "REVIEW",
      logical(1)
    )
    rule_ids <- vapply(
      findings,
      function(x) as.character(x$rule_id %||% "")[[1]],
      character(1)
    )
    draft_rules <- c(
      "draft_ownership", "stepien", "pick_protections", "pick_swaps",
      "conditional_picks", "conveyance"
    )
    draft_failed <- any(finding_failed & rule_ids %in% draft_rules)
    cba_failed <- exception_failed || any(finding_failed & !rule_ids %in% draft_rules)
    route_status <- status_value(multi_evaluation$route_validation$status %||% "UNKNOWN")
    other_failed <- is_failure(route_status) || (
      (is_failure(multi_evaluation$status) || isTRUE(multi_evaluation$is_blocked)) &&
        !cba_failed && !draft_failed
    )

    if (cba_failed) {
      return(result(
        "FAIL",
        "CBA_FAIL",
        "A blocking CBA result controls this transaction; basketball and financial output cannot override legality."
      ))
    }
    if (draft_failed) {
      return(result(
        "FAIL",
        "PROTECTED_DRAFT_BLOCK",
        "A protected Draft BLOCK controls this transaction; salary or basketball PASS cannot override the asset blocker."
      ))
    }
    if (other_failed) {
      return(result(
        "FAIL",
        "TRANSACTION_FAIL",
        "A blocking transaction failure controls this transaction."
      ))
    }
    if (
      is_review(multi_evaluation$status) ||
      identical(route_status, "REVIEW") ||
      any(finding_review) ||
      exception_review
    ) {
      return(result(
        "REVIEW",
        "CONTROLLING_EVIDENCE_REVIEW",
        "Controlling transaction evidence is missing or requires source verification."
      ))
    }
    return(result(
      "PASS",
      "TRANSACTION_PASS",
      "All available controlling transaction and Draft screens pass."
    ))
  }

  evaluation <- scenario$evaluation
  evaluation_status <- if (is.list(evaluation)) {
    status_value(evaluation$status %||% evaluation$overall_status %||% "UNKNOWN")
  } else {
    "UNKNOWN"
  }
  draft <- scenario$draft_evaluation
  draft_status <- if (is.list(draft)) {
    status_value(draft$status %||% "UNKNOWN")
  } else {
    status_value(scenario$draft_screen_status %||% "UNKNOWN")
  }
  has_draft_assets <- any(vapply(
    list(scenario$outgoing_draft_assets, scenario$incoming_draft_assets),
    function(x) is.data.frame(x) && nrow(x) > 0L,
    logical(1)
  ))

  protected_sides <- if (is.list(evaluation)) {
    Filter(Negate(is.null), list(evaluation$team_a, evaluation$team_b))
  } else {
    list()
  }
  protected_screen_failed <- any(vapply(
    protected_sides,
    function(x) !is.null(x$is_screen_pass) && !isTRUE(x$is_screen_pass),
    logical(1)
  ))
  cba_failed <- exception_failed ||
    is_failure(evaluation_status) ||
    protected_screen_failed
  draft_failed <- is_failure(draft_status) || (is.list(draft) && isTRUE(draft$blocked))
  other_failed <- is.list(evaluation) && isTRUE(evaluation$is_blocked) &&
    !cba_failed

  if (cba_failed) {
    return(result(
      "FAIL",
      "CBA_FAIL",
      "A blocking CBA result controls this transaction; basketball and financial output cannot override legality."
    ))
  }
  if (draft_failed) {
    return(result(
      "FAIL",
      "PROTECTED_DRAFT_BLOCK",
      "A protected Draft BLOCK controls this transaction; salary or basketball PASS cannot override the asset blocker."
    ))
  }
  if (other_failed) {
    return(result(
      "FAIL",
      "TRANSACTION_FAIL",
      "A blocking transaction failure controls this transaction."
    ))
  }
  if (
    is_review(evaluation_status) ||
    isTRUE(evaluation$requires_manual_review) ||
    inherits(draft, "tbi_trade_error") ||
    (has_draft_assets && is_review(draft_status)) ||
    identical(draft_status, "REVIEW") ||
    exception_review
  ) {
    return(result(
      "REVIEW",
      "CONTROLLING_EVIDENCE_REVIEW",
      "Controlling transaction evidence is missing or requires source verification."
    ))
  }

  result(
    "PASS",
    "TRANSACTION_PASS",
    "All available controlling transaction and Draft screens pass."
  )
}

v2_trade_recommendation_model <- function(scenario) {
  if (!is.list(scenario) || !isTRUE(scenario$active)) return(NULL)
  controlling_decision <- v2_trade_controlling_decision(scenario)
  exception <- v2_trade_exception_recommendation(scenario$v2_exception_scenario)
  exception_failed <- is.list(scenario$v2_exception_scenario) && identical(scenario$v2_exception_scenario$status, "FAIL")
  basketball_missing <- v2_trade_evidence_statement(
    "UNKNOWN / MISSING EVIDENCE",
    "Basketball recalculation is unavailable: canonical transaction state contains no governed BIE or rotation impact for this trade."
  )

  if (!is.null(scenario$v2_transaction_evaluation)) {
    evaluation <- scenario$v2_transaction_evaluation
    graph <- scenario$v2_transaction_graph
    impact <- scenario$v2_organizational_impact
    decision <- controlling_decision$decision
    findings <- unlist(lapply(evaluation$team_results, `[[`, "findings"), recursive = FALSE)
    finding_line <- function(x) {
      missing <- paste(as.character(x$missing_facts %||% character()), collapse = ", ")
      suffix <- if (nzchar(missing)) paste0(" Missing: ", missing, ".") else ""
      v2_trade_evidence_statement("RULE RESULT", paste0(
        x$team, " - ", x$rule_id, " - ", x$status,
        if (isTRUE(x$is_blocked)) " - BLOCKING" else "", ": ", x$explanation, suffix
      ))
    }
    statuses <- vapply(findings, function(x) as.character(x$status %||% "REVIEW")[[1]], character(1))
    blocked <- vapply(findings, function(x) isTRUE(x$is_blocked), logical(1))
    order_index <- order(!blocked, match(statuses, c("FAIL", "REVIEW", "PASS")), seq_along(findings), method = "radix")
    findings <- findings[order_index]
    cba_all <- vapply(findings, finding_line, character(1))
    cba_summary <- c(
      v2_trade_evidence_statement("RULE RESULT", paste0("Overall transaction evaluation: ", evaluation$status, ".")),
      utils::head(cba_all, 5L)
    )
    reasons <- unique(c(cba_summary, exception$reasons))
    reasons <- utils::head(reasons, 5L)
    if (length(reasons) < 3L && is.list(impact) && length(impact$team_impacts)) {
      first_team <- names(impact$team_impacts)[[1]]
      first_impact <- impact$team_impacts[[first_team]]
      reasons <- c(reasons, v2_trade_evidence_statement("FACT", paste0(
        first_team, " payroll delta is ", v2_trade_money(first_impact$payroll_delta),
        " with roster delta ", first_impact$roster_delta, "."
      )))
    }
    financial <- if (is.list(impact) && length(impact$team_impacts)) {
      vapply(names(impact$team_impacts), function(team) {
        item <- impact$team_impacts[[team]]
        v2_trade_evidence_statement("FACT", paste0(
          team, ": payroll delta ", v2_trade_money(item$payroll_delta), "; roster delta ", item$roster_delta, "."
        ))
      }, character(1))
    } else v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", "Organizational payroll impact is missing from canonical transaction state.")
    assets <- if (is.list(graph) && is.data.frame(graph$asset_routes)) graph$asset_routes else data.frame()
    draft <- if (!nrow(assets)) {
      v2_trade_evidence_statement("FACT", "No routed draft assets.")
    } else vapply(seq_len(nrow(assets)), function(i) {
      v2_trade_evidence_statement("FACT", paste0(
        assets$asset_id[[i]], " routed from ", assets$from_team_id[[i]], " to ", assets$to_team_id[[i]], "."
      ))
    }, character(1))
    controlling <- if (length(findings)) findings[[1]] else NULL
    next_action <- if (!is.null(controlling) && (identical(controlling$status, "FAIL") || identical(controlling$status, "REVIEW"))) {
      missing <- paste(as.character(controlling$missing_facts %||% character()), collapse = ", ")
      if (nzchar(missing)) {
        v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", paste0(
          "Verify ", missing, " for ", controlling$team, " before approval: ", controlling$explanation
        ))
      } else v2_trade_evidence_statement("RULE RESULT", paste0(
        "Resolve ", controlling$rule_id, " for ", controlling$team, ": ", controlling$explanation
      ))
    } else v2_trade_evidence_statement("RULE RESULT", "No transaction change is required by the evaluated transaction rules.")
    return(list(
      decision = decision,
      decision_basis = v2_trade_evidence_statement("RULE RESULT", controlling_decision$basis),
      reasons = reasons, cba = cba_summary, cba_all = cba_all,
      financial = financial, tpe = exception$summary, draft = draft,
      basketball = basketball_missing, next_action = next_action
    ))
  }

  evaluation <- scenario$evaluation
  financial <- c(
    v2_trade_evidence_statement("FACT", paste0(
      scenario$team, ": Salary sent ", v2_trade_money(scenario$outgoing_salary),
      "; salary received ", v2_trade_money(scenario$incoming_salary),
      "; payroll delta ", v2_trade_money(scenario$salary_delta), "."
    )),
    v2_trade_evidence_statement("FACT", paste0(
      scenario$partner_team, ": Salary sent ", v2_trade_money(scenario$incoming_salary),
      "; salary received ", v2_trade_money(scenario$outgoing_salary),
      "; payroll delta ", v2_trade_money(-scenario$salary_delta), "."
    ))
  )
  outgoing_assets <- v2_trade_asset_labels(scenario$outgoing_draft_assets)
  incoming_assets <- v2_trade_asset_labels(scenario$incoming_draft_assets)
  draft <- if (!length(outgoing_assets) && !length(incoming_assets)) {
    v2_trade_evidence_statement("FACT", "No routed draft assets.")
  } else {
    c(
      if (length(outgoing_assets)) v2_trade_evidence_statement("FACT", paste0(scenario$team, " sends: ", paste(outgoing_assets, collapse = ", "), ".")),
      if (length(incoming_assets)) v2_trade_evidence_statement("FACT", paste0(scenario$team, " receives: ", paste(incoming_assets, collapse = ", "), ".")),
      v2_trade_evidence_statement("RULE RESULT", paste0("Draft screen status: ", scenario$draft_screen_status, "."))
    )
  }
  if (is.null(evaluation)) {
    missing <- v2_trade_evidence_statement(
      "UNKNOWN / MISSING EVIDENCE",
      "Protected two-team evaluation is missing from canonical transaction state; salary matching and CBA findings cannot be calculated."
    )
    return(list(
      decision = controlling_decision$decision,
      decision_basis = v2_trade_evidence_statement("RULE RESULT", controlling_decision$basis),
      reasons = unique(c(missing, exception$reasons, financial[[1]])),
      cba = missing, cba_all = missing, financial = financial, tpe = exception$summary,
      draft = draft, basketball = basketball_missing,
      next_action = v2_trade_evidence_statement("UNKNOWN / MISSING EVIDENCE", "Run the protected two-team evaluator for the active trade before approval.")
    ))
  }

  sides <- list(evaluation$team_a, evaluation$team_b)
  team_names <- c(evaluation$team_a_name, evaluation$team_b_name)
  side_lines <- vapply(seq_along(sides), function(i) {
    side <- sides[[i]]
    v2_trade_evidence_statement("RULE RESULT", paste0(
      team_names[[i]], " salary matching: ", side$screen_status,
      if (!isTRUE(side$is_screen_pass)) " - BLOCKING" else "", " - ", side$explanation
    ))
  }, character(1))
  cba <- unlist(lapply(seq_along(sides), function(i) {
    side <- sides[[i]]
    c(
      v2_trade_evidence_statement("RULE RESULT", paste0(
        team_names[[i]], " - ", side$screen_status,
        if (!isTRUE(side$is_screen_pass)) " - BLOCKING" else "", " - ", side$matching_rule,
        "; receives ", v2_trade_money(side$incoming_salary),
        " with maximum ", v2_trade_money(side$maximum_incoming_salary), "."
      )),
      if (isTRUE(side$second_apron_aggregation_violation)) v2_trade_evidence_statement(
        "RULE RESULT", paste0(team_names[[i]], " - FAIL - BLOCKING: Second-apron aggregation restriction is triggered.")
      ),
      if (length(side$restriction_flags)) vapply(side$restriction_flags, function(flag) {
        v2_trade_evidence_statement("RULE RESULT", paste0(team_names[[i]], " - REVIEW: ", flag))
      }, character(1))
    )
  }), use.names = FALSE)
  status <- as.character(evaluation$status %||% evaluation$overall_status %||% "REVIEW")[[1]]
  decision <- controlling_decision$decision
  reasons <- unique(c(
    side_lines,
    v2_trade_evidence_statement("FACT", paste0(
      scenario$team, " payroll changes by ", v2_trade_money(scenario$salary_delta), "."
    )),
    exception$reasons
  ))
  failing <- which(vapply(sides, function(side) !isTRUE(side$is_screen_pass), logical(1)))
  reviewing <- which(vapply(sides, function(side) isTRUE(side$requires_manual_review), logical(1)))
  next_action <- if (identical(controlling_decision$code, "PROTECTED_DRAFT_BLOCK")) {
    v2_trade_evidence_statement(
      "RULE RESULT",
      paste0("Resolve the protected Draft BLOCK before approval: ", scenario$draft_evaluation$summary %||% "draft asset legality is blocked.")
    )
  } else if (length(failing)) {
    i <- failing[[1]]
    side <- sides[[i]]
    v2_trade_evidence_statement("RULE RESULT", paste0(
      "Reduce incoming salary for ", team_names[[i]], " to ", v2_trade_money(side$maximum_incoming_salary),
      " or less under ", side$matching_rule, "."
    ))
  } else if (length(reviewing)) {
    i <- reviewing[[1]]
    v2_trade_evidence_statement("RULE RESULT", paste0(team_names[[i]], ": ", sides[[i]]$restriction_flags[[1]]))
  } else v2_trade_evidence_statement("RULE RESULT", "No transaction change is required by the evaluated salary-matching screen.")
  list(
    decision = decision,
    decision_basis = v2_trade_evidence_statement("RULE RESULT", controlling_decision$basis),
    reasons = utils::head(reasons, 5L), cba = cba, cba_all = cba,
    financial = financial, tpe = exception$summary, draft = draft,
    basketball = basketball_missing, next_action = next_action
  )
}

v2_trade_development_tpe_ledger <- function(team, expired = FALSE) {
  team <- v2_tx_scalar(team, "team")
  new_v2_team_exception_ledger(data.frame(
    team_id = team, season = "2026-27", exception_id = "development-tpe-12m",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 12000000,
    remaining_amount = 12000000, creation_transaction = "DEVELOPMENT REFERENCE TRANSACTION",
    creation_date = "2026-07-01", expiration_date = if (isTRUE(expired)) "2026-08-01" else "2027-08-20",
    status = "ACTIVE", source = "DEVELOPMENT FIXTURE", source_version = "1.0.0-test",
    verification_status = "VERIFIED", use_restrictions = "TEST ONLY",
    stringsAsFactors = FALSE
  ))
}

v2_trade_development_creation_fact <- function(team) {
  data.frame(
    team_id = v2_tx_scalar(team, "team"), season = "2026-27", exception_id = "development-created-tpe-6m",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 6000000,
    remaining_amount = 6000000, creation_transaction = "DEVELOPMENT SYNTHETIC TRADE",
    creation_date = "2026-08-20", expiration_date = "2027-08-20", status = "ACTIVE",
    source = "DEVELOPMENT FIXTURE", source_version = "1.0.0-test",
    verification_status = "VERIFIED", use_restrictions = "TEST ONLY · DEVELOPMENT / SCENARIO ONLY",
    stringsAsFactors = FALSE
  )
}

v2_trade_two_team_scenario_graph <- function(scenario) {
  if (
    !is.list(scenario) ||
    !isTRUE(scenario$active) ||
    !identical(as.character(scenario$scenario_type), "trade")
  ) {
    stop("An active two-team scenario is required.", call. = FALSE)
  }
  route_rows <- function(players, from_team, to_team, prefix) {
    if (!is.data.frame(players) || !nrow(players)) return(data.frame())
    salary <- if ("cap_hit" %in% names(players)) {
      suppressWarnings(as.numeric(players$cap_hit))
    } else {
      rep(NA_real_, nrow(players))
    }
    data.frame(
      route_id = paste0(prefix, seq_len(nrow(players))),
      player_id = as.character(players$player_id),
      from_team_id = from_team,
      to_team_id = to_team,
      salary = salary,
      stringsAsFactors = FALSE
    )
  }
  outgoing <- route_rows(
    scenario$outgoing_players,
    scenario$team,
    scenario$partner_team,
    "two-team-outgoing-"
  )
  incoming <- route_rows(
    scenario$incoming_players,
    scenario$partner_team,
    scenario$team,
    "two-team-incoming-"
  )
  routes <- Filter(function(x) is.data.frame(x) && nrow(x), list(outgoing, incoming))
  routes <- if (length(routes)) do.call(rbind, routes) else data.frame()
  normalize_transaction_graph(
    transaction_id = as.character(scenario$scenario_id %||% "two-team-scenario")[[1]],
    teams = c(scenario$team, scenario$partner_team),
    player_routes = routes,
    season = scenario$season
  )
}

v2_trade_tpe_usage_request <- function(ledger, exception_id, amount, graph) {
  exception_id <- as.character(exception_id %||% "")[[1]]
  amount <- suppressWarnings(as.numeric(amount %||% 0))
  entries <- ledger$entries
  idx <- which(as.character(entries$exception_id) == exception_id)
  if (!length(idx)) return(data.frame())

  owner <- as.character(entries$team_id[[idx[[1]]]])
  routes <- graph$player_routes
  matching <- if (
    length(idx) == 1L &&
    is.data.frame(routes) &&
    nrow(routes) &&
    "salary" %in% names(routes) &&
    is.finite(amount) &&
    amount > 0
  ) {
    salary <- suppressWarnings(as.numeric(routes$salary))
    which(
      as.character(routes$to_team_id) == owner &
      is.finite(salary) &
      abs(salary - amount) <= pmax(0.01, abs(salary) * 1e-9)
    )
  } else {
    integer()
  }
  route_id <- if (length(matching) == 1L) {
    as.character(routes$route_id[[matching]])
  } else {
    paste0("unbound-tpe-", exception_id)
  }
  data.frame(
    route_id = route_id,
    team_id = owner,
    exception_id = exception_id,
    amount = amount,
    stringsAsFactors = FALSE
  )
}

v2_trade_intelligence_ui <- function(id, legacy_builder) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "tbi-trade-workbench",
    bslib::navset_card_tab(
    id = ns("trade_view"),
    bslib::nav_panel(
      "Trade Summary",
      shiny::uiOutput(ns("scenario_summary"))
    ),
    bslib::nav_panel(
      "2-Team Trade",
      shiny::div(
        class = "tbi-trade-builder-intro",
        shiny::div(shiny::strong("Two-team transaction"), shiny::span("Select the organization from the global selector, then choose its partner, players, and controlled draft assets below.")),
        shiny::actionButton(ns("reset_two_team"), "Clear Trade", class = "btn-outline-secondary")
      ),
      shiny::uiOutput(ns("development_tpe_workbench")),
      legacy_builder
    ),
    bslib::nav_panel(
      "Multi-Team Trade",
      shiny::div(
        class = "tbi-multiteam-workbench",
        shiny::div(
          class = "tbi-multiteam-toolbar",
          shiny::radioButtons(ns("multi_mode"), "Transaction size", choices = c("3 Teams" = 3L, "4 Teams" = 4L), selected = 3L, inline = TRUE),
          shiny::actionButton(ns("evaluate_multi"), "Evaluate multi-team trade", class = "btn-primary"),
          shiny::actionButton(ns("reset_multi"), "Reset Scenario", class = "btn-outline-secondary")
        ),
        shiny::uiOutput(ns("multi_team_selectors")),
        shiny::uiOutput(ns("multi_team_workspaces")),
        shiny::uiOutput(ns("tpe_workspace")),
        shiny::uiOutput(ns("multi_team_preview"))
      )
    ),
    bslib::nav_panel("Evaluation", shiny::uiOutput(ns("evaluation_view"))),
    bslib::nav_panel("Recommendation", shiny::uiOutput(ns("recommendation_view")))
    )
  )
}

v2_trade_intelligence_server <- function(id,
                                         selected_team,
                                         selected_season,
                                         transaction_state,
                                         authoritative_exception_ledger = new_v2_team_exception_ledger(),
                                         reset_two_team_builder = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    team_names <- sort(unique(as.character(get_teams()$team_name)), method = "radix")
    local_error <- shiny::reactiveVal(NULL)
    development_tpe_scenario <- shiny::reactiveVal(NULL)
    evaluated_builder_signature <- shiny::reactiveVal(NULL)
    evaluated_transaction_signature <- shiny::reactiveVal(NULL)
    evaluation_sequence <- shiny::reactiveVal(0L)

    output$development_tpe_workbench <- shiny::renderUI({
      if (!v2_trade_test_mode_enabled()) return(NULL)
      shiny::tags$section(
        class = "tbi-development-tpe-workbench",
        shiny::div(class = "tbi-development-tpe-head",
          shiny::div(shiny::strong("Development TPE Test Mode"), shiny::span("TEST ONLY · IN-MEMORY · NEVER WRITTEN TO DATABASE")),
          v2_ui_status_chip("REVIEW")),
        shiny::div(class = "tbi-development-tpe-controls",
          shiny::selectInput(session$ns("development_tpe_team"), "Fixture team", choices = team_names,
            selected = if (selected_team() %in% team_names) selected_team() else team_names[[1]]),
          shiny::radioButtons(session$ns("development_tpe_state"), "Fixture state",
            choices = c("Active · expires 2027-08-20" = "ACTIVE", "Expired · expired 2026-08-01" = "EXPIRED"), selected = "ACTIVE"),
          shiny::numericInput(session$ns("development_tpe_amount"), "Amount to use", value = 5000000, min = 0, step = 100000),
          shiny::actionButton(session$ns("development_tpe_apply"), "Use Test TPE", class = "btn-primary"),
          shiny::actionButton(session$ns("development_tpe_create"), "Run TPE Creation Fixture", class = "btn-secondary"),
          shiny::actionButton(session$ns("development_tpe_reset"), "Reset Test TPE", class = "btn-outline-secondary")
        ),
        shiny::uiOutput(session$ns("development_tpe_ledger"))
      )
    })

    output$tpe_workspace <- shiny::renderUI({
      if (!v2_trade_test_mode_enabled()) return(NULL)
      shiny::div(
        class = "tbi-route-section tbi-tpe-workspace",
        shiny::div(
          class = "tbi-route-heading",
          shiny::h3("Verified TPE inventory"),
          shiny::p("Exception use is explicit and applies only to the session scenario ledger.")
        ),
        shiny::uiOutput(session$ns("tpe_inventory")),
        shiny::uiOutput(session$ns("tpe_controls"))
      )
    })

    development_authority <- shiny::reactive({
      shiny::req(v2_trade_test_mode_enabled())
      team <- as.character(input$development_tpe_team %||% selected_team())
      v2_trade_development_tpe_ledger(team, identical(input$development_tpe_state, "EXPIRED"))
    })

    shiny::observeEvent(input$development_tpe_apply, {
      authority <- development_authority()
      active <- transaction_state$snapshot()
      if (!isTRUE(active$active) || !identical(active$scenario_type, "trade")) {
        development_tpe_scenario(structure(list(message = "Build an active 2-team transaction before selecting the development TPE."), class = "tbi_trade_ui_error"))
        return()
      }
      if (!authority$entries$team_id[[1]] %in% c(active$team, active$partner_team)) {
        development_tpe_scenario(structure(list(message = "The fixture team must be one of the active transaction participants."), class = "tbi_trade_ui_error"))
        return()
      }
      amount <- suppressWarnings(as.numeric(input$development_tpe_amount %||% 0))
      graph <- v2_trade_two_team_scenario_graph(active)
      usage <- v2_trade_tpe_usage_request(
        authority,
        authority$entries$exception_id[[1]],
        amount,
        graph
      )
      result <- apply_v2_scenario_exceptions(
        authority,
        usage,
        "2026-08-20",
        transaction_graph = graph
      )
      development_tpe_scenario(result)
      transaction_state$set_v2_exception_scenario(result)
    })
    shiny::observeEvent(input$development_tpe_create, {
      authority <- development_authority()
      active <- transaction_state$snapshot()
      if (!isTRUE(active$active) || !identical(active$scenario_type, "trade")) {
        development_tpe_scenario(structure(list(message = "Build an active 2-team transaction before running the creation fixture."), class = "tbi_trade_ui_error"))
        return()
      }
      if (!authority$entries$team_id[[1]] %in% c(active$team, active$partner_team)) {
        development_tpe_scenario(structure(list(message = "The fixture team must be one of the active transaction participants."), class = "tbi_trade_ui_error"))
        return()
      }
      result <- apply_v2_scenario_exceptions(
        authority, data.frame(), "2026-08-20",
        creation_facts = v2_trade_development_creation_fact(authority$entries$team_id[[1]])
      )
      development_tpe_scenario(result)
      transaction_state$set_v2_exception_scenario(result)
    })
    shiny::observeEvent(input$development_tpe_reset, {
      development_tpe_scenario(NULL)
      active <- transaction_state$snapshot()
      if (isTRUE(active$active)) transaction_state$set_v2_exception_scenario(NULL)
      shiny::updateRadioButtons(session, "development_tpe_state", selected = "ACTIVE")
      shiny::updateNumericInput(session, "development_tpe_amount", value = 5000000)
    })
    shiny::observeEvent(list(input$development_tpe_team, input$development_tpe_state), {
      development_tpe_scenario(NULL)
      active <- transaction_state$snapshot()
      if (isTRUE(active$active) && !is.null(active$v2_exception_scenario)) transaction_state$set_v2_exception_scenario(NULL)
    }, ignoreInit = TRUE)

    output$development_tpe_ledger <- shiny::renderUI({
      authority <- development_authority()
      scenario <- development_tpe_scenario()
      render_entry <- function(row, label) shiny::div(
        class = "tbi-development-ledger-row",
        shiny::div(shiny::strong(label), shiny::span(row$exception_id[[1]])),
        shiny::span(paste("Team", row$team_id[[1]])),
        shiny::span(paste("Original", v2_trade_money(row$original_amount[[1]]))),
        shiny::span(paste("Remaining", v2_trade_money(row$remaining_amount[[1]]))),
        shiny::span(paste("Expires", row$expiration_date[[1]])),
        shiny::span("Verification · TEST ONLY"),
        shiny::tags$small(paste(row$source[[1]], "·", row$creation_transaction[[1]]))
      )
      before <- render_entry(authority$entries[1, , drop = FALSE], "Authoritative fixture baseline")
      if (inherits(scenario, "tbi_trade_ui_error")) return(shiny::div(class = "tbi-development-ledger", before,
        shiny::div(class = "tbi-trade-error", shiny::strong("Active trade required"), shiny::p(scenario$message))))
      if (is.null(scenario)) return(shiny::div(class = "tbi-development-ledger", before,
        shiny::p(class = "tbi-development-ledger-empty", "No scenario exception changes. Use, create, or reset the test fixture.")))
      after_rows <- lapply(seq_len(nrow(scenario$scenario_ledger$entries)), function(i) {
        row <- scenario$scenario_ledger$entries[i, , drop = FALSE]
        created <- row$exception_id[[1]] != authority$entries$exception_id[[1]]
        render_entry(row, if (created) "DEVELOPMENT / SCENARIO ONLY · Created TPE" else "Scenario after use")
      })
      shiny::div(class = "tbi-development-ledger", before,
        shiny::div(class = "tbi-development-ledger-result", v2_ui_status_chip(scenario$status, scenario$is_blocked),
          shiny::span(if (scenario$status == "FAIL") "Requested use is expired or exceeds the available test balance." else "Scenario ledger updated; fixture baseline remains unchanged.")),
        after_rows)
    })

    multi_team_mode <- shiny::reactive({
      v2_trade_multi_mode(input$multi_mode)
    })
    selected_multi_teams <- shiny::reactive({
      count <- multi_team_mode()
      if (is.na(count)) return(character())
      values <- vapply(seq_len(count), function(i) as.character(input[[paste0("multi_team_", i)]] %||% ""), character(1))
      values[nzchar(values)]
    })
    multi_player_pools <- shiny::reactive({
      teams <- selected_multi_teams()
      stats::setNames(lapply(teams, get_trade_player_pool, season = selected_season()), teams)
    })
    multi_asset_pools <- shiny::reactive({
      teams <- selected_multi_teams()
      stats::setNames(lapply(teams, tbi_trade_selectable_draft_assets), teams)
    })

    output$multi_team_selectors <- shiny::renderUI({
      count <- multi_team_mode()
      if (is.na(count)) {
        return(v2_trade_empty(
          "Invalid transaction size",
          "Transaction size must be 3 or 4 teams."
        ))
      }
      shiny::div(
        class = "tbi-multiteam-selectors",
        lapply(seq_len(count), function(i) {
          current <- as.character(input[[paste0("multi_team_", i)]] %||% "")
          selected <- if (current %in% team_names) {
            current
          } else if (i == 1L && selected_team() %in% team_names) {
            selected_team()
          } else {
            ""
          }
          shiny::selectInput(
            session$ns(paste0("multi_team_", i)), paste("Team", i),
            choices = c("Select organization" = "", team_names),
            selected = selected
          )
        })
      )
    })

    route_row <- function(i, kind, source_team, teams) {
      prefix <- paste0(kind, "_route_", i)
      player_number <- ((i - 1L) %% v2_trade_player_slots_per_team) + 1L
      destination_choices <- v2_trade_destination_choices(source_team, teams)
      current_destination <- as.character(input[[paste0(prefix, "_to")]] %||% "")
      shiny::div(
        class = paste("tbi-route-row", paste0("tbi-", kind, "-route-row")),
        shiny::tags$input(type = "hidden", id = session$ns(paste0(prefix, "_from")), value = source_team),
        shiny::uiOutput(session$ns(paste0(prefix, "_identity_ui"))),
        shiny::selectInput(
          session$ns(paste0(prefix, "_to")), "Destination",
          choices = c("Select destination" = "", destination_choices),
          selected = if (current_destination %in% destination_choices) current_destination else ""
        ),
        if (kind == "player") shiny::actionButton(session$ns(paste0(prefix, "_clear")), "Clear", title = paste("Clear Player", player_number), class = "btn-outline-secondary tbi-route-clear") else NULL
      )
    }

    output$multi_team_workspaces <- shiny::renderUI({
      teams <- selected_multi_teams()
      expected <- multi_team_mode()
      if (is.na(expected)) {
        return(v2_trade_empty(
          "Invalid transaction size",
          "Transaction size must be 3 or 4 teams."
        ))
      }
      if (length(teams) != expected || anyDuplicated(teams)) {
        return(v2_trade_empty("Select distinct teams", "Choose every participating organization to build team-owned outgoing packages."))
      }
      shiny::div(class = "tbi-team-route-workspaces", lapply(seq_along(teams), function(team_i) {
        source_team <- teams[[team_i]]
        player_slots <- v2_trade_player_slot_indices(team_i)
        shiny::tags$section(class = "tbi-team-route-workspace",
          shiny::div(class = "tbi-team-route-head", shiny::h3(source_team), shiny::span("Outgoing package")),
          shiny::div(class = "tbi-team-route-group", shiny::h4("Players being sent"),
            lapply(player_slots, route_row, kind = "player", source_team = source_team, teams = teams)),
          shiny::div(class = "tbi-team-route-group", shiny::h4("Draft assets being sent"),
            route_row(team_i, "asset", source_team, teams))
        )
      }))
    })

    evaluation_exception_ledger <- shiny::reactive({
      entries <- authoritative_exception_ledger$entries
      if (v2_trade_test_mode_enabled()) {
        fixture <- development_authority()$entries
        entries <- rbind(entries, fixture[, names(entries), drop = FALSE])
      }
      new_v2_team_exception_ledger(entries)
    })
    verified_tpes <- shiny::reactive({
      entries <- evaluation_exception_ledger()$entries
      participants <- selected_multi_teams()
      entries[
        entries$verification_status == "VERIFIED" &
          entries$status == "ACTIVE" &
          entries$team_id %in% participants &
          entries$season == selected_season(),
        ,
        drop = FALSE
      ]
    })
    output$tpe_inventory <- shiny::renderUI({
      entries <- verified_tpes()
      if (!nrow(entries)) return(v2_trade_empty("No verified TPE currently loaded", "This does not assert that no real-world exception exists; approved amount, date, and source facts are not loaded."))
      shiny::div(class = "tbi-tpe-inventory", lapply(seq_len(nrow(entries)), function(i) shiny::div(
        class = "tbi-tpe-row", shiny::strong(entries$exception_id[[i]]),
        shiny::span(paste("Original", v2_trade_money(entries$original_amount[[i]]))),
        shiny::span(paste("Remaining", v2_trade_money(entries$remaining_amount[[i]]))),
        shiny::span(paste("Expires", entries$expiration_date[[i]])),
        shiny::tags$small(paste(entries$creation_transaction[[i]], "·", entries$source[[i]], "· VERIFIED"))
      )))
    })
    output$tpe_controls <- shiny::renderUI({
      entries <- verified_tpes()
      if (!nrow(entries)) return(NULL)
      choices <- stats::setNames(entries$exception_id, paste(entries$team_id, entries$exception_id, vapply(entries$remaining_amount, v2_trade_money, character(1)), sep = " · "))
      shiny::div(class = "tbi-tpe-controls",
        shiny::selectInput(session$ns("tpe_use_id"), "Use exception", choices = c("Do not use" = "", choices)),
        shiny::numericInput(session$ns("tpe_use_amount"), "Amount", value = 0, min = 0, step = 100000))
    })

    for (kind in c("player", "asset")) {
      maximum <- if (kind == "player") v2_trade_max_player_slots else v2_trade_max_teams
      for (i in seq_len(maximum)) local({
        route_kind <- kind
        route_i <- i
        output_id <- paste0(route_kind, "_route_", route_i, "_identity_ui")
        output[[output_id]] <- shiny::renderUI({
          from <- v2_trade_route_source(route_kind, route_i, selected_multi_teams())
          current <- as.character(input[[paste0(route_kind, "_route_", route_i, "_id")]] %||% "")
          if (is.null(from) || !nzchar(from)) return(shiny::selectInput(session$ns(paste0(route_kind, "_route_", route_i, "_id")), if (route_kind == "player") "Player" else "Draft asset", choices = character()))
          if (route_kind == "player") {
            pool <- multi_player_pools()[[from]]
            other_ids <- unlist(lapply(setdiff(seq_len(v2_trade_max_player_slots), route_i), function(j) {
              as.character(input[[paste0("player_route_", j, "_id")]] %||% "")
            }), use.names = FALSE)
            pool <- pool[!as.character(pool$player_id) %in% other_ids[nzchar(other_ids)], , drop = FALSE]
            labels <- paste(pool$player_name, pool$primary_position, vapply(pool$cap_hit, v2_trade_money, character(1)), sep = " · ")
            choices <- stats::setNames(as.character(pool$player_id), labels)
          } else {
            pool <- multi_asset_pools()[[from]]
            choices <- tbi_trade_draft_asset_choices(pool)
          }
          label <- if (route_kind == "player") paste("Player", ((route_i - 1L) %% v2_trade_player_slots_per_team) + 1L) else "Draft asset"
          selected <- if (current %in% unname(choices)) current else ""
          shiny::selectInput(session$ns(paste0(route_kind, "_route_", route_i, "_id")), label, choices = c("Optional" = "", choices), selected = selected)
        })
      })
    }

    for (i in seq_len(v2_trade_max_player_slots)) local({
      route_i <- i
      shiny::observeEvent(input[[paste0("player_route_", route_i, "_clear")]], {
        shiny::updateSelectInput(session, paste0("player_route_", route_i, "_id"), selected = "")
      }, ignoreInit = TRUE)
    })

    build_routes <- function(kind, maximum, teams) {
      rows <- lapply(seq_len(maximum), function(i) {
        from <- v2_trade_route_source(kind, i, teams)
        to <- as.character(input[[paste0(kind, "_route_", i, "_to")]] %||% "")
        identity <- as.character(input[[paste0(kind, "_route_", i, "_id")]] %||% "")
        if (!nzchar(from) || !nzchar(to) || !nzchar(identity) || !from %in% teams || !to %in% teams || identical(from, to)) return(NULL)
        row <- data.frame(route_id = paste0(substr(kind, 1L, 1L), i), identity = identity,
          from_team_id = from, to_team_id = to, stringsAsFactors = FALSE)
        if (kind == "player") {
          pool <- multi_player_pools()[[from]]
          if (!identity %in% as.character(pool$player_id)) return(NULL)
          row$salary <- pool$cap_hit[match(identity, as.character(pool$player_id))]
        } else {
          pool <- multi_asset_pools()[[from]]
          if (!identity %in% as.character(pool$draft_asset_id)) return(NULL)
        }
        names(row)[[2]] <- paste0(if (kind == "player") "player" else "asset", "_id")
        row
      })
      rows <- Filter(Negate(is.null), rows)
      result <- if (!length(rows)) data.frame() else do.call(rbind, rows)
      if (kind == "player" && nrow(result) && anyDuplicated(as.character(result$player_id))) {
        stop("A player cannot be selected in more than one outgoing slot.", call. = FALSE)
      }
      result
    }

    multi_builder_signature <- shiny::reactive({
      route_values <- function(kind, maximum, suffix) {
        vapply(seq_len(maximum), function(i) {
          as.character(input[[paste0(kind, "_route_", i, suffix)]] %||% "")
        }, character(1))
      }
      v2_input_signature(list(
        mode = if (is.na(multi_team_mode())) {
          "INVALID"
        } else {
          as.character(multi_team_mode())
        },
        teams = vapply(seq_len(v2_trade_max_teams), function(i) {
          as.character(input[[paste0("multi_team_", i)]] %||% "")
        }, character(1)),
        player_ids = route_values("player", v2_trade_max_player_slots, "_id"),
        player_destinations = route_values("player", v2_trade_max_player_slots, "_to"),
        asset_ids = route_values("asset", v2_trade_max_teams, "_id"),
        asset_destinations = route_values("asset", v2_trade_max_teams, "_to"),
        tpe_id = as.character(input$tpe_use_id %||% ""),
        tpe_amount = suppressWarnings(as.numeric(input$tpe_use_amount %||% 0))
      ))
    })

    evaluate_multi <- function() {
      teams <- selected_multi_teams()
      count <- multi_team_mode()
      if (is.na(count)) {
        stop("Transaction size must be 3 or 4 teams.", call. = FALSE)
      }
      if (length(teams) != count || anyDuplicated(teams)) stop("Select distinct organizations for every participant.", call. = FALSE)
      players <- build_routes("player", v2_trade_max_player_slots, teams)
      assets <- build_routes("asset", v2_trade_max_teams, teams)
      if (!nrow(players) && !nrow(assets)) stop("Route at least one actual player or controlled draft asset.", call. = FALSE)
      next_sequence <- evaluation_sequence() + 1L
      evaluation_sequence(next_sequence)
      graph <- normalize_transaction_graph(
        paste0("v2-trade-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", next_sequence),
        teams,
        players,
        assets,
        season = selected_season()
      )
      evaluation <- evaluate_multiteam_transaction(graph)
      tpe_id <- as.character(input$tpe_use_id %||% "")
      tpe_amount <- suppressWarnings(as.numeric(input$tpe_use_amount %||% 0))
      usage <- data.frame()
      if (nzchar(tpe_id) && is.finite(tpe_amount) && tpe_amount > 0) {
        usage <- v2_trade_tpe_usage_request(
          evaluation_exception_ledger(),
          tpe_id,
          tpe_amount,
          graph
        )
      }
      exceptions <- apply_v2_scenario_exceptions(
        evaluation_exception_ledger(),
        usage,
        as.character(Sys.Date()),
        transaction_graph = graph
      )
      transaction_state$clear()
      if (exceptions$status == "FAIL") {
        requested_team <- if (nrow(usage)) usage$team_id[[1]] else ""
        failed_team <- if (requested_team %in% names(evaluation$team_results)) {
          requested_team
        } else {
          graph$teams[[1]]
        }
        evaluation$team_results[[failed_team]]$status <- "FAIL"
        evaluation$team_results[[failed_team]]$is_blocked <- TRUE
        evaluation$team_results[[failed_team]]$findings[[length(evaluation$team_results[[failed_team]]$findings) + 1L]] <- list(
          rule_id = "traded_player_exception", rule_version = "1.0.0", team = failed_team,
          transaction = graph$transaction_id, status = "FAIL", is_blocked = TRUE,
          controlling_facts = list(), missing_facts = character(), source = NULL,
          source_reference = NULL, effective_date = NULL,
          explanation = exceptions$validation$findings[[1]]$explanation$message %||%
            "The selected verified exception is expired, incompatible, or insufficient."
        )
        evaluation$status <- "FAIL"
        evaluation$is_blocked <- TRUE
        evaluation$signature <- v2_input_signature(list(graph = graph$signature, team_results = evaluation$team_results))
      }
      impact <- build_v2_organizational_impact(graph, evaluation)
      transaction_state$publish_v2_transaction(graph, evaluation, impact, exceptions)
      list(graph = graph, evaluation = evaluation, impact = impact, exceptions = exceptions)
    }

    invalidate_multi_result <- function(clear_any = FALSE) {
      scenario <- transaction_state$snapshot()
      should_clear <- isTRUE(scenario$active) && (
        isTRUE(clear_any) ||
          identical(as.character(scenario$scenario_type), "v2_multiteam_trade")
      )
      if (should_clear) {
        transaction_state$clear()
      }
      local_error(NULL)
      evaluated_builder_signature(NULL)
      evaluated_transaction_signature(NULL)
      invisible(TRUE)
    }

    shiny::observeEvent(input$evaluate_multi, {
      invalidate_multi_result(clear_any = TRUE)
      value <- tryCatch(evaluate_multi(), error = function(e) structure(list(message = conditionMessage(e)), class = "tbi_trade_ui_error"))
      if (inherits(value, "tbi_trade_ui_error")) {
        local_error(value)
      } else {
        evaluated_builder_signature(multi_builder_signature())
        evaluated_transaction_signature(value$graph$signature)
        local_error(NULL)
      }
    })

    canonical_result <- shiny::reactive({
      v2_trade_canonical_result(transaction_state$snapshot())
    })

    canonical_multi_result <- shiny::reactive({
      result <- canonical_result()
      if (is.null(result) || !identical(result$kind, "multi_team")) return(NULL)
      result
    })

    canonical_transaction_identity <- shiny::reactive({
      scenario <- transaction_state$snapshot()
      v2_input_signature(list(
        scenario_type = as.character(scenario$scenario_type %||% ""),
        scenario_id = as.character(scenario$scenario_id %||% ""),
        graph_signature = as.character(
          scenario$v2_transaction_graph$signature %||% ""
        )
      ))
    })

    shiny::observeEvent(
      canonical_transaction_identity(),
      {
        local_error(NULL)
        value <- canonical_multi_result()
        if (is.null(value) || !identical(value$graph$signature, evaluated_transaction_signature())) {
          evaluated_builder_signature(NULL)
          evaluated_transaction_signature(NULL)
        }
      },
      ignoreInit = TRUE
    )

    clear_scenario <- function() {
      transaction_state$clear()
      local_error(NULL)
      development_tpe_scenario(NULL)
      evaluated_builder_signature(NULL)
      evaluated_transaction_signature(NULL)
      if (is.function(reset_two_team_builder)) reset_two_team_builder()
      shiny::updateRadioButtons(session, "multi_mode", selected = 3L)
      for (i in seq_len(4L)) shiny::updateSelectInput(session, paste0("multi_team_", i), selected = if (i == 1L) selected_team() else "")
      for (kind in c("player", "asset")) {
        maximum <- if (kind == "player") v2_trade_max_player_slots else v2_trade_max_teams
        for (i in seq_len(maximum)) {
          shiny::updateSelectInput(session, paste0(kind, "_route_", i, "_id"), selected = "")
          shiny::updateSelectInput(session, paste0(kind, "_route_", i, "_to"), selected = "")
        }
      }
      shiny::updateSelectInput(session, "tpe_use_id", selected = "")
      shiny::updateNumericInput(session, "tpe_use_amount", value = 0)
      shiny::updateRadioButtons(session, "development_tpe_state", selected = "ACTIVE")
      shiny::updateNumericInput(session, "development_tpe_amount", value = 5000000)
      invisible(TRUE)
    }
    shiny::observeEvent(input$reset_multi, clear_scenario())
    shiny::observeEvent(input$reset_two_team, clear_scenario())

    # Builder edits invalidate only the result they produced. Compatible inputs remain mounted.
    shiny::observeEvent(multi_builder_signature(), {
      current_result <- canonical_multi_result()
      owns_result <- !is.null(current_result) &&
        identical(current_result$graph$signature, evaluated_transaction_signature())
      builder_changed <- !is.null(evaluated_builder_signature()) &&
        !identical(multi_builder_signature(), evaluated_builder_signature())
      if (owns_result && builder_changed) {
        invalidate_multi_result()
      } else if (!is.null(local_error())) {
        local_error(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(selected_season(), {
      scenario <- transaction_state$snapshot()
      if (!tbi_scenario_matches_season(scenario, selected_season())) {
        transaction_state$clear()
      }
      local_error(NULL)
      evaluated_builder_signature(NULL)
      evaluated_transaction_signature(NULL)
      for (i in seq_len(v2_trade_max_player_slots)) {
        shiny::updateSelectInput(session, paste0("player_route_", i, "_id"), selected = "")
      }
      shiny::updateSelectInput(session, "tpe_use_id", selected = "")
      shiny::updateNumericInput(session, "tpe_use_amount", value = 0)
      development_tpe_scenario(NULL)
      if (is.function(reset_two_team_builder)) reset_two_team_builder()
    }, ignoreInit = TRUE)

    shiny::observeEvent(transaction_state$snapshot()$active, {
      if (!isTRUE(transaction_state$snapshot()$active)) {
        local_error(NULL)
        development_tpe_scenario(NULL)
      }
    }, ignoreInit = TRUE)

    output$multi_team_preview <- shiny::renderUI({
      error <- local_error()
      if (inherits(error, "tbi_trade_ui_error")) return(shiny::div(class = "tbi-trade-error", shiny::strong("Cannot evaluate transaction"), shiny::p(error$message)))
      value <- canonical_multi_result()
      if (is.null(value)) return(v2_trade_empty("No multi-team result", "Select distinct teams, route actual players or assets, then evaluate."))
      usage_history <- if (
        is.list(value$exceptions) &&
        is.list(value$exceptions$scenario_ledger) &&
        is.data.frame(value$exceptions$scenario_ledger$usage_history)
      ) {
        value$exceptions$scenario_ledger$usage_history
      } else {
        data.frame()
      }
      rows <- lapply(value$graph$teams, function(team) {
        impact <- value$impact$team_impacts[[team]]
        result <- value$evaluation$team_results[[team]]
        shiny::div(class = "tbi-team-consequence", shiny::div(shiny::strong(team), v2_ui_status_chip(result$status, result$is_blocked)),
          shiny::span(paste("Salary delta", v2_trade_money(impact$payroll_delta))),
          shiny::span(paste("Roster delta", impact$roster_delta)),
          shiny::span(paste("Draft", v2_ui_humanize(impact$draft))),
          shiny::span(if (nrow(usage_history) && any(usage_history$team_id == team)) "TPE used" else "TPE not used"))
      })
      shiny::div(class = "tbi-team-consequence-grid", rows)
    })

    scenario_exception_ui <- function(exception_scenario) {
      if (!v2_trade_test_mode_enabled()) return(NULL)
      if (is.null(exception_scenario) || !is.list(exception_scenario) || is.null(exception_scenario$scenario_ledger)) return(NULL)
      ledger <- exception_scenario$scenario_ledger
      usage <- ledger$usage_history
      entries <- ledger$entries
      shiny::div(class = "tbi-scenario-exception-summary",
        shiny::div(class = "tbi-route-heading", shiny::h3("Scenario Exception Ledger"),
          v2_ui_status_chip(exception_scenario$status, exception_scenario$is_blocked)),
        if (nrow(usage)) shiny::p(paste("Used", v2_trade_money(sum(usage$amount_used)), "· Remaining", v2_trade_money(tail(usage$amount_remaining, 1)))) else NULL,
        lapply(seq_len(nrow(entries)), function(i) {
          created <- grepl("DEVELOPMENT / SCENARIO ONLY", entries$use_restrictions[[i]], fixed = TRUE)
          shiny::div(class = "tbi-tpe-row",
            shiny::strong(if (created) "DEVELOPMENT / SCENARIO ONLY · Created TPE" else entries$exception_id[[i]]),
            shiny::span(entries$team_id[[i]]),
            shiny::span(paste("Original", v2_trade_money(entries$original_amount[[i]]))),
            shiny::span(paste("Remaining", v2_trade_money(entries$remaining_amount[[i]]))),
            shiny::span("TEST ONLY"))
        }))
    }

    output$scenario_summary <- shiny::renderUI({
      result <- canonical_result()
      if (is.null(result)) return(v2_trade_empty("Not evaluated yet", "Build and evaluate a transaction first."))
      scenario <- result$scenario
      controlling <- v2_trade_controlling_decision(scenario)
      if (!is.null(scenario$v2_transaction_graph)) {
        graph <- scenario$v2_transaction_graph
        player_count <- nrow(graph$player_routes)
        asset_count <- nrow(graph$asset_routes)
        return(shiny::div(class = "tbi-trade-summary",
          shiny::div(class = "tbi-trade-summary-head", shiny::div(shiny::strong(paste(length(graph$teams), "organizations")), shiny::span(paste(graph$teams, collapse = " · "))), v2_ui_status_chip(controlling$status, controlling$is_blocked)),
          shiny::div(class = "tbi-trade-flow-metrics",
            v2_ui_metric("Players routed", player_count), v2_ui_metric("Assets routed", asset_count),
            v2_ui_metric("Recommendation", controlling$decision),
            v2_ui_metric("Scope", "TRADE-LOCAL", "Not propagated to downstream pages")),
          shiny::p("Salary, roster, draft, exception, and CBA consequences are shown in Evaluation. Unsupported rule facts remain Review Required."),
          scenario_exception_ui(scenario$v2_exception_scenario)))
      }
      shiny::div(class = "tbi-trade-summary",
        shiny::div(class = "tbi-trade-summary-head", shiny::div(shiny::strong("Two-team scenario"), shiny::span(paste(scenario$team, "↔", scenario$partner_team))), v2_ui_status_chip(controlling$status, controlling$is_blocked)),
        shiny::div(class = "tbi-trade-flow-metrics", v2_ui_metric("Salary sent", v2_trade_money(scenario$outgoing_salary)), v2_ui_metric("Salary received", v2_trade_money(scenario$incoming_salary)), v2_ui_metric("Salary delta", v2_trade_money(scenario$salary_delta)), v2_ui_metric("Draft screen", scenario$draft_screen_status), v2_ui_metric("Recommendation", controlling$decision)),
        scenario_exception_ui(scenario$v2_exception_scenario))
    })

    output$evaluation_view <- shiny::renderUI({
      result <- canonical_result()
      if (is.null(result)) return(v2_trade_empty("Not evaluated yet", "Build and evaluate a transaction first."))
      scenario <- result$scenario
      controlling <- v2_trade_controlling_decision(scenario)
      evaluation <- scenario$v2_transaction_evaluation
      if (is.null(evaluation)) {
        exception <- scenario$v2_exception_scenario
        return(shiny::div(class = "tbi-trade-evaluation",
          shiny::div(class = "tbi-trade-decision", shiny::span("2-TEAM RESULT"), shiny::h2(controlling$decision), shiny::p(controlling$basis), v2_ui_status_chip(controlling$status, controlling$is_blocked)),
          shiny::div(class = "tbi-two-team-evaluation-grid",
            shiny::div(shiny::strong(scenario$team), shiny::span(paste("Salary sent", v2_trade_money(scenario$outgoing_salary))), shiny::span(paste("Salary received", v2_trade_money(scenario$incoming_salary)))),
            shiny::div(shiny::strong(scenario$partner_team), shiny::span(paste("Salary sent", v2_trade_money(scenario$incoming_salary))), shiny::span(paste("Salary received", v2_trade_money(scenario$outgoing_salary)))),
            shiny::div(shiny::strong("TPE mechanism"), shiny::span(if (is.null(exception)) "Not selected" else exception$status), shiny::span(if (!is.null(exception) && isTRUE(exception$is_blocked)) "Blocking failure" else "Scenario only"))),
          scenario_exception_ui(exception),
          shiny::tags$details(shiny::tags$summary("Protected V1 evaluation detail"), shiny::p("Salary matching, apron, aggregation, draft, and BIE findings remain governed by the protected two-team evaluator."))))
      }
      all_findings <- unlist(lapply(evaluation$team_results, `[[`, "findings"), recursive = FALSE)
      priority <- c("salary_matching", "first_apron", "second_apron", "aggregation", "traded_player_exception", "roster_size", "draft_ownership")
      rows <- lapply(priority, function(rule) {
        matches <- Filter(function(x) identical(x$rule_id, rule), all_findings)
        status <- if (any(vapply(matches, `[[`, character(1), "status") == "FAIL")) "FAIL" else if (any(vapply(matches, `[[`, character(1), "status") == "REVIEW")) "REVIEW" else "PASS"
        shiny::div(class = "tbi-rule-finding", shiny::strong(v2_ui_humanize(rule)), v2_ui_status_chip(status))
      })
      shiny::div(class = "tbi-trade-evaluation",
        shiny::div(class = "tbi-trade-decision", shiny::span("PRIMARY RESULT"), shiny::h2(controlling$decision), shiny::p(controlling$basis), v2_ui_status_chip(controlling$status, controlling$is_blocked)),
        shiny::div(class = "tbi-rule-findings", rows),
        shiny::tags$details(shiny::tags$summary("Detailed source-verification findings"), lapply(all_findings, function(x) shiny::p(shiny::strong(paste(v2_ui_humanize(x$rule_id), "—", x$status)), x$explanation))))
    })

    output$recommendation_view <- shiny::renderUI({
      result <- canonical_result()
      if (is.null(result)) return(v2_trade_empty("Not evaluated yet", "Build and evaluate a transaction first."))
      scenario <- result$scenario
      recommendation <- v2_trade_recommendation_model(scenario)
      evidence_list <- function(items) shiny::tags$ul(lapply(items, shiny::tags$li))
      shiny::div(class = "tbi-trade-recommendation",
        shiny::div(class = "tbi-trade-decision", shiny::span("EXECUTIVE DECISION"), shiny::h2(recommendation$decision), shiny::p(recommendation$decision_basis)),
        shiny::div(class = "tbi-recommendation-columns",
          shiny::tags$section(shiny::h3("Why"), evidence_list(recommendation$reasons)),
          shiny::tags$section(shiny::h3("CBA"), evidence_list(recommendation$cba),
            if (length(recommendation$cba_all) > length(recommendation$cba)) shiny::tags$details(
              shiny::tags$summary("All evaluated rule findings"), evidence_list(recommendation$cba_all)
            )),
          shiny::tags$section(shiny::h3("Financial"), evidence_list(recommendation$financial)),
          shiny::tags$section(shiny::h3("TPE"), evidence_list(recommendation$tpe)),
          shiny::tags$section(shiny::h3("Draft"), evidence_list(recommendation$draft)),
          shiny::tags$section(shiny::h3("Basketball"), shiny::p(recommendation$basketball)),
          shiny::tags$section(shiny::h3("Next action"), shiny::p(recommendation$next_action))))
    })
  })
}
