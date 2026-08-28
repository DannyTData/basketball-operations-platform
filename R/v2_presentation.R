# Phase 3 development-visible V2 presentation helpers.
# These helpers are read-only: they render shadow contracts and never persist them.

v2_ui_text <- function(value, fallback = "UNKNOWN") {
  value <- as.character(value %||% "")
  if (!length(value) || is.na(value[[1]]) || !nzchar(trimws(value[[1]]))) fallback else value[[1]]
}

v2_ui_concise_note <- function(value, fallback, limit = 140L) {
  value <- trimws(v2_ui_text(value, fallback))
  first_sentence <- strsplit(value, "(?<=[.!?])\\s+", perl = TRUE)[[1]][[1]]
  if (nchar(first_sentence) > limit) {
    paste0(substr(first_sentence, 1L, limit - 3L), "…")
  } else {
    first_sentence
  }
}

v2_ui_humanize <- function(value) {
  value <- v2_ui_text(value)
  known <- c(
    ELIGIBLE_WITH_REVIEW = "Eligible — Review Required",
    UNKNOWN_ROLES = "Role Evidence Missing",
    MANUAL_REVIEW = "Source Verification Required",
    REQUIRES_REVIEW = "Review Required",
    REQUIRES_SOURCE_VERIFICATION = "Source Verification Required"
  )
  if (value %in% names(known)) return(unname(known[[value]]))
  if (value %in% c("PASS", "REVIEW", "FAIL", "UNKNOWN")) return(value)
  tools::toTitleCase(tolower(gsub("_", " ", value, fixed = TRUE)))
}

v2_ui_status_class <- function(status) {
  status <- toupper(v2_ui_text(status))
  if (status == "PASS") "pass" else if (status %in% c("FAIL", "ERROR")) "fail" else "review"
}

v2_ui_initials <- function(name) {
  words <- strsplit(trimws(v2_ui_text(name, "?")), "\\s+")[[1]]
  paste0(substr(head(words, 2L), 1L, 1L), collapse = "")
}

v2_ui_provenance_chip <- function(label, kind = "model") {
  shiny::span(class = paste("tbi-p3-provenance", kind), label)
}

v2_ui_status_chip <- function(status, blocked = FALSE) {
  status <- v2_ui_text(status)
  shiny::span(
    class = paste("tbi-p3-status", v2_ui_status_class(status)),
    shiny::span(class = "tbi-p3-status-dot"),
    if (isTRUE(blocked)) paste(v2_ui_humanize(status), "· BLOCKED") else v2_ui_humanize(status)
  )
}

v2_ui_player_token <- function(player_id,
                               player_name,
                               detail = NULL,
                               compact = FALSE,
                               media_registry = data.frame()) {
  media <- v2_player_headshot_record(player_id, media_registry)
  avatar <- if (is.null(media)) {
    shiny::div(
      class = "tbi-p3-avatar",
      role = "img",
      `aria-label` = paste("Fallback initials for", v2_ui_text(player_name)),
      v2_ui_initials(player_name)
    )
  } else {
    shiny::div(
      class = "tbi-p3-avatar has-image",
      shiny::tags$img(
        src = media$asset_url,
        alt = paste(v2_ui_text(player_name), "headshot"),
        loading = "lazy",
        decoding = "async"
      )
    )
  }
  shiny::div(
    class = paste("tbi-p3-player", if (compact) "compact" else ""),
    avatar,
    shiny::div(
      class = "tbi-p3-player-copy",
      shiny::strong(v2_ui_text(player_name)),
      shiny::span(v2_ui_text(detail, paste("Player", player_id)))
    )
  )
}

v2_ui_team_mark <- function(team, media_registry = v2_team_media_registry()) {
  team <- v2_ui_text(team, "TBI")
  abbreviation <- paste0(substr(gsub("[^A-Za-z]", "", team), 1L, 3L))
  media <- media_registry[
    (media_registry$canonical_team_name == team |
      media_registry$official_abbreviation == toupper(team)) &
      media_registry$verification_status == "VERIFIED",
    , drop = FALSE
  ]
  if (nrow(media) == 1L && nzchar(v2_ui_text(media$asset_url[[1]], ""))) {
    return(shiny::div(
      class = "tbi-p3-team-mark has-image",
      shiny::tags$img(
        src = media$asset_url[[1]], alt = paste(team, "logo"),
        loading = "lazy", decoding = "async"
      )
    ))
  }
  shiny::div(
    class = "tbi-p3-team-mark",
    role = "img",
    `aria-label` = paste(team, "team abbreviation fallback"),
    toupper(abbreviation)
  )
}

v2_ui_transaction_foundation_banner <- function(id = NULL) {
  ns <- if (is.null(id)) identity else shiny::NS(id)
  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-transaction-foundation tbi-p3-transaction-workspace",
    shiny::div(
      class = "tbi-p3-panel-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "V2 TRANSACTION FOUNDATION"),
        shiny::h2("Multi-Team Scenario Workspace"),
        shiny::p("Governed scenario contracts available for a 2–4 team route graph with per-team CBA, roster, cap, draft, and exception consequences. Unsupported CBA rules remain REVIEW; the protected V1 two-team UI remains authoritative.")
      ),
      v2_ui_status_chip("REVIEW")
    ),
    shiny::div(
      class = "tbi-p3-transaction-controls",
      shiny::selectInput(
        ns("v2_team_count"), "Participating teams",
        choices = c("2 teams" = 2L, "3 teams" = 3L, "4 teams" = 4L),
        selected = 2L, width = "100%"
      ),
      shiny::uiOutput(ns("v2_additional_teams")),
      shiny::div(
        class = "tbi-p3-exception-inventory",
        shiny::h3("Verified TPE Inventory"),
        shiny::uiOutput(ns("v2_exception_inventory")),
        shiny::uiOutput(ns("v2_exception_controls"))
      )
    ),
    shiny::div(
      class = "tbi-p3-route-workspace",
      shiny::div(
        shiny::h3("Explicit Routes"),
        shiny::p("Route each player or draft asset once to an explicit receiving organization. Stable IDs are required; names alone are not authoritative.")
      ),
      shiny::uiOutput(ns("v2_route_editor"))
    ),
    shiny::div(
      class = "tbi-p3-transaction-actions",
      shiny::actionButton(ns("v2_evaluate_transaction"), "Evaluate V2 Scenario", class = "btn-primary"),
      shiny::actionButton(ns("v2_reset_transaction"), "Reset V2 Scenario", class = "btn-secondary")
    ),
    shiny::uiOutput(ns("v2_transaction_result"))
  )
}

v2_build_ui_transaction_graph <- function(transaction_id, teams, route_rows = list(), season = NULL) {
  route_rows <- Filter(function(row) {
    nzchar(trimws(as.character(row$identity %||% "")))
  }, route_rows)
  player_rows <- Filter(function(row) identical(toupper(row$kind %||% ""), "PLAYER"), route_rows)
  asset_rows <- Filter(function(row) identical(toupper(row$kind %||% ""), "ASSET"), route_rows)
  make_routes <- function(rows, identity_name) {
    if (!length(rows)) return(data.frame())
    out <- data.frame(
      route_id = paste0(tolower(substr(identity_name, 1L, 1L)), seq_along(rows)),
      identity = vapply(rows, function(row) trimws(as.character(row$identity)), character(1)),
      from_team_id = vapply(rows, function(row) as.character(row$from), character(1)),
      to_team_id = vapply(rows, function(row) as.character(row$to), character(1)),
      stringsAsFactors = FALSE
    )
    names(out)[[2]] <- identity_name
    out
  }
  normalize_transaction_graph(
    transaction_id = transaction_id,
    teams = teams,
    player_routes = make_routes(player_rows, "player_id"),
    asset_routes = make_routes(asset_rows, "asset_id"),
    season = season
  )
}

v2_transaction_workspace_server <- function(id,
                                             selected_team,
                                             transaction_state,
                                             authoritative_exception_ledger = new_v2_team_exception_ledger(),
                                             selected_season = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    teams <- get_teams()
    team_names <- sort(unique(as.character(teams$team_name)), method = "radix")

    participant_names <- shiny::reactive({
      count <- suppressWarnings(as.integer(input$v2_team_count %||% 2L))
      values <- c(selected_team(), input$partner_team)
      if (count >= 3L) values <- c(values, input$v2_team_3)
      if (count >= 4L) values <- c(values, input$v2_team_4)
      unique(values[!is.na(values) & nzchar(trimws(values))])
    })

    output$v2_additional_teams <- shiny::renderUI({
      count <- suppressWarnings(as.integer(input$v2_team_count %||% 2L))
      choices <- team_names[team_names != selected_team()]
      shiny::div(
        class = "tbi-p3-additional-teams",
        if (count >= 3L) shiny::selectInput(session$ns("v2_team_3"), "Team 3", choices = c("Select Team" = "", choices)),
        if (count >= 4L) shiny::selectInput(session$ns("v2_team_4"), "Team 4", choices = c("Select Team" = "", choices))
      )
    })

    output$v2_exception_inventory <- shiny::renderUI({
      entries <- authoritative_exception_ledger$entries
      verified <- entries[
        entries$verification_status == "VERIFIED" & entries$status == "ACTIVE",
        , drop = FALSE
      ]
      if (nrow(verified)) {
        return(shiny::div(
          class = "tbi-p3-exception-list",
          lapply(seq_len(nrow(verified)), function(i) shiny::div(
            class = "tbi-p3-exception-row",
            shiny::strong(verified$exception_id[[i]]),
            shiny::span(paste(verified$team_id[[i]], "·", v2_trade_money(verified$remaining_amount[[i]]), "remaining")),
            shiny::small(paste("Expires", verified$expiration_date[[i]], "·", verified$source[[i]]))
          ))
        ))
      }
      shiny::div(
        class = "tbi-p3-empty-inline",
        shiny::strong("No verified TPE loaded"),
        shiny::span("Authoritative exception amounts, creation dates, and expiration dates require a verified source. No scenario use is inferred.")
      )
    })

    output$v2_exception_controls <- shiny::renderUI({
      entries <- authoritative_exception_ledger$entries
      verified <- entries[
        entries$verification_status == "VERIFIED" & entries$status == "ACTIVE",
        , drop = FALSE
      ]
      if (!nrow(verified)) return(NULL)
      choices <- stats::setNames(
        verified$exception_id,
        paste(
          verified$team_id,
          verified$exception_id,
          vapply(verified$remaining_amount, v2_trade_money, character(1)),
          sep = " · "
        )
      )
      shiny::div(
        class = "tbi-p3-exception-controls",
        shiny::selectInput(session$ns("v2_exception_id"), "Exception to use", choices = choices),
        shiny::numericInput(session$ns("v2_exception_amount"), "Amount used", value = 0, min = 0, step = 100000)
      )
    })

    output$v2_route_editor <- shiny::renderUI({
      participants <- participant_names()
      if (length(participants) < 2L) {
        return(shiny::div(class = "tbi-p3-empty-inline", "Select at least two distinct organizations."))
      }
      shiny::div(
        class = "tbi-p3-route-grid",
        lapply(seq_len(4L), function(i) {
          shiny::div(
            class = "tbi-p3-route-row",
            shiny::selectInput(session$ns(paste0("v2_route_kind_", i)), paste("Route", i), c("Player" = "PLAYER", "Draft asset" = "ASSET")),
            shiny::textInput(session$ns(paste0("v2_route_identity_", i)), "Stable ID", placeholder = "Optional"),
            shiny::selectInput(session$ns(paste0("v2_route_from_", i)), "From", participants),
            shiny::selectInput(session$ns(paste0("v2_route_to_", i)), "To", rev(participants))
          )
        })
      )
    })

    result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$v2_evaluate_transaction, {
      participants <- participant_names()
      shiny::validate(shiny::need(length(participants) == as.integer(input$v2_team_count), "Select distinct organizations for every participant."))
      rows <- lapply(seq_len(4L), function(i) list(
        kind = input[[paste0("v2_route_kind_", i)]],
        identity = input[[paste0("v2_route_identity_", i)]],
        from = input[[paste0("v2_route_from_", i)]],
        to = input[[paste0("v2_route_to_", i)]]
      ))
      graph <- v2_build_ui_transaction_graph(
        paste0("v2-ui-", format(Sys.time(), "%Y%m%d%H%M%S")),
        participants,
        rows,
        season = selected_season()
      )
      evaluation <- evaluate_multiteam_transaction(graph)
      impact <- build_v2_organizational_impact(graph, evaluation)
      entries <- authoritative_exception_ledger$entries
      exception_id <- as.character(input$v2_exception_id %||% "")
      amount <- suppressWarnings(as.numeric(input$v2_exception_amount %||% 0))
      usage <- data.frame()
      if (nzchar(exception_id) && is.finite(amount) && amount > 0) {
        idx <- which(entries$exception_id == exception_id)
        if (length(idx) == 1L) usage <- data.frame(
          route_id = paste0("tpe-use-", exception_id),
          team_id = entries$team_id[[idx]],
          exception_id = exception_id,
          amount = amount,
          stringsAsFactors = FALSE
        )
      }
      exception_scenario <- apply_v2_scenario_exceptions(
        authoritative_exception_ledger,
        usage,
        as.character(Sys.Date()),
        transaction_graph = graph
      )
      transaction_state$publish_v2_transaction(graph, evaluation, impact, exception_scenario)
      result(list(graph = graph, evaluation = evaluation, impact = impact, exceptions = exception_scenario))
    })

    shiny::observeEvent(input$v2_reset_transaction, {
      transaction_state$clear()
      result(NULL)
    })

    output$v2_transaction_result <- shiny::renderUI({
      value <- result()
      if (is.null(value)) return(shiny::div(class = "tbi-p3-empty-inline", "No V2 multi-team scenario evaluated."))
      team_rows <- lapply(value$evaluation$team_results, function(team_result) {
        impact <- value$impact$team_impacts[[team_result$team_id]]
        shiny::div(
          class = "tbi-p3-team-result",
          shiny::strong(team_result$team_id),
          v2_ui_status_chip(team_result$status, team_result$is_blocked),
          shiny::span(paste("Roster delta", impact$roster_delta)),
          shiny::span(paste("CBA", impact$cba)),
          shiny::span("Salary and rule facts: source verification required")
        )
      })
      shiny::div(
        class = "tbi-p3-transaction-result",
        shiny::div(
          class = "tbi-p3-result-head",
          shiny::div(shiny::span("EXECUTIVE RECOMMENDATION"), shiny::strong(value$impact$executive_recommendation)),
          v2_ui_status_chip(value$evaluation$status, value$evaluation$is_blocked)
        ),
        shiny::div(class = "tbi-p3-team-results", team_rows),
        shiny::p(if (nrow(value$exceptions$scenario_ledger$usage_history)) {
          used <- value$exceptions$scenario_ledger$usage_history[1L, , drop = FALSE]
          paste("TPE impact:", v2_trade_money(used$amount_used[[1]]), "used from", used$exception_id[[1]],
                "with", v2_trade_money(used$amount_remaining[[1]]), "remaining. Authoritative ledger unchanged.")
        } else {
          "TPE impact: no verified exception was used. Scenario ledger remains isolated and the authoritative ledger is unchanged."
        })
      )
    })
  })
}

v2_ui_global_context <- function(shadow, scenario = NULL, team = NULL) {
  active <- tbi_scenario_is_shared_supported(scenario)
  status <- shadow$phase2_diagnostics$status %||% shadow$execution_status %||% "LOADING"
  shiny::div(
    class = "tbi-p3-global-context",
    shiny::div(
      class = "tbi-p3-global-identity",
      v2_ui_team_mark(team),
      shiny::div(
        shiny::strong("V2 INTELLIGENCE · DEVELOPMENT / NON-AUTHORITATIVE"),
        shiny::span("Read-only shadow evidence. V1 remains the authoritative save path.")
      )
    ),
    shiny::div(
      class = "tbi-p3-global-signals",
      v2_ui_status_chip(status, shadow$phase2_diagnostics$is_blocked %||% FALSE),
      v2_ui_provenance_chip("MODEL OUTPUT", "model"),
      v2_ui_provenance_chip(if (active) "SCENARIO ACTIVE" else "BASELINE", if (active) "override" else "fact")
    )
  )
}

v2_ui_metric <- function(label, value, note = NULL, tone = NULL) {
  shiny::div(
    class = paste("tbi-p3-metric", tone %||% ""),
    shiny::span(label),
    shiny::strong(v2_ui_text(value)),
    if (!is.null(note)) shiny::tags$small(note)
  )
}

v2_ui_page_lens <- function(title, description, domains) {
  shiny::div(
    class = "tbi-p3-page-lens",
    shiny::div(
      shiny::span(class = "tbi-p3-kicker", "DECISION LENS"),
      shiny::strong(title),
      shiny::tags$small(description)
    ),
    shiny::div(
      class = "tbi-p3-page-lens-domains",
      lapply(domains, function(domain) v2_ui_provenance_chip(domain[[1]], domain[[2]]))
    )
  )
}

v2_ui_shadow_ready <- function(shadow) {
  is.list(shadow) && identical(shadow$execution_status, "COMPLETED") &&
    is.data.frame(shadow$minute_ledger$ledger) && nrow(shadow$minute_ledger$ledger) > 0L
}

v2_ui_depth_state_issue <- function(shadow) {
  if (!is.list(shadow)) {
    return(list(
      status = "REVIEW",
      is_blocked = FALSE,
      title = "No V2 rotation state received",
      detail = "Select a team and season so the development rotation can be built."
    ))
  }

  execution_status <- toupper(v2_ui_text(shadow$execution_status, "LOADING"))
  diagnostics <- shadow$phase2_diagnostics %||% list()

  if (execution_status == "DISABLED") {
    return(list(
      status = "REVIEW",
      is_blocked = FALSE,
      title = "V2 rotation is disabled",
      detail = "Enable the V2 shadow rotation route to compare a non-authoritative plan."
    ))
  }

  if (execution_status == "ERROR") {
    detail <- diagnostics$error$message %||% shadow$error$message %||%
      "The V2 shadow plan failed before a rotation could be rendered."
    return(list(
      status = "FAIL",
      is_blocked = TRUE,
      title = "V2 rotation could not be built",
      detail = v2_ui_text(detail)
    ))
  }

  if (execution_status != "COMPLETED") {
    return(list(
      status = "REVIEW",
      is_blocked = FALSE,
      title = "V2 rotation is preparing",
      detail = "The authoritative V1 Depth Chart remains available while the shadow plan loads."
    ))
  }

  if (!is.data.frame(shadow$rotation_10$members) || !nrow(shadow$rotation_10$members)) {
    return(list(
      status = diagnostics$status %||% "REVIEW",
      is_blocked = isTRUE(diagnostics$is_blocked),
      title = "Rotation membership is unavailable",
      detail = "The starter or rotation contract produced no governed player membership. Review roster completeness and validation findings."
    ))
  }

  if (!is.data.frame(shadow$minute_ledger$ledger) || !nrow(shadow$minute_ledger$ledger)) {
    detail <- diagnostics$error$message %||%
      "Rotation membership exists, but the governed minute ledger did not produce player minutes."
    return(list(
      status = diagnostics$status %||% "REVIEW",
      is_blocked = isTRUE(diagnostics$is_blocked),
      title = "Minute ledger is unavailable",
      detail = v2_ui_text(detail)
    ))
  }

  NULL
}

v2_ui_depth_evidence <- function(shadow) {
  findings <- unique(vapply(
    Filter(
      function(x) identical(x$status, "REVIEW") || identical(x$status, "FAIL"),
      shadow$validation_findings %||% list()
    ),
    function(x) v2_ui_text(x$code),
    character(1)
  ))
  shiny::tags$details(
    class = "tbi-p3-evidence",
    shiny::tags$summary("Evidence, provenance, and human review"),
    shiny::div(
      v2_ui_provenance_chip("FACT", "fact"),
      v2_ui_provenance_chip("MODEL OUTPUT", "model"),
      v2_ui_provenance_chip("UNKNOWN", "unknown")
    ),
    shiny::p("V2 is development-visible and non-authoritative. Missing role or availability evidence remains UNKNOWN. Lineup synergy and clutch evidence are not asserted."),
    shiny::p(if (length(findings)) {
      paste(
        "Validation findings:",
        paste(vapply(findings, v2_ui_humanize, character(1)), collapse = " · ")
      )
    } else {
      "No validation findings were returned."
    })
  )
}

v2_ui_depth_player_names <- function(shadow) {
  rotation <- shadow$rotation_10$members
  stats::setNames(
    as.character(rotation$player_name),
    as.character(rotation$player_id)
  )
}

v2_ui_depth_rotation <- function(shadow) {
  issue <- v2_ui_depth_state_issue(shadow)
  if (!is.null(issue)) {
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "rotation",
      v2_ui_status_chip(issue$status, issue$is_blocked),
      shiny::strong(issue$title),
      shiny::span(issue$detail)
    ))
  }

  rotation <- shadow$minute_ledger$ledger
  starter_flag <- as.logical(rotation$is_starter)
  starter_flag[is.na(starter_flag)] <- FALSE
  starters <- rotation[starter_flag, , drop = FALSE]
  bench <- rotation[!starter_flag, , drop = FALSE]
  sixth <- bench[bench$rotation_role == "SIXTH_MAN", , drop = FALSE]

  player_nodes <- function(rows) {
    lapply(seq_len(nrow(rows)), function(i) {
      role <- v2_ui_humanize(rows$rotation_role[[i]])
      detail <- paste0(
        role, " · ", rows$assigned_minutes[[i]], " min · ",
        v2_ui_text(rows$availability_status[[i]])
      )
      v2_ui_player_token(rows$player_id[[i]], rows$player_name[[i]], detail, TRUE)
    })
  }

  total_minutes <- suppressWarnings(as.integer(shadow$minute_ledger$total_assigned_minutes))
  exact_minutes <- length(total_minutes) == 1L && !is.na(total_minutes) && total_minutes == 240L
  status <- shadow$phase2_diagnostics$status %||% shadow$minute_ledger$status %||% "REVIEW"

  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-depth tbi-depth-v2-view",
    `data-tbi-v2-depth-view` = "rotation",
    shiny::div(
      class = "tbi-p3-panel-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "PHASE 2 · ROTATION + MINUTES"),
        shiny::h2("Who Plays and How Much?"),
        shiny::p("The governed 10/11-player shadow rotation and exact individual minute ledger.")
      ),
      v2_ui_status_chip(status, shadow$phase2_diagnostics$is_blocked %||% FALSE)
    ),
    shiny::div(
      class = "tbi-p3-metric-grid",
      v2_ui_metric("Rotation", paste0(nrow(rotation), " players"), paste0(nrow(starters), " starters · ", nrow(bench), " bench")),
      v2_ui_metric("Sixth man", if (nrow(sixth)) sixth$player_name[[1]] else "UNKNOWN", "MODEL OUTPUT"),
      v2_ui_metric("Minutes", paste0(total_minutes, " / 240"), if (exact_minutes) "Exact reconciliation" else "REVIEW"),
      v2_ui_metric("Availability", if (all(rotation$availability_status == "AVAILABLE")) "VERIFIED" else "REVIEW", "UNKNOWN remains explicit")
    ),
    shiny::div(
      class = "tbi-p3-rotation-groups",
      shiny::div(class = "tbi-p3-subpanel", shiny::h3("Starting Five"), shiny::div(class = "tbi-p3-player-grid", player_nodes(starters))),
      shiny::div(class = "tbi-p3-subpanel", shiny::h3("Bench Rotation"), shiny::div(class = "tbi-p3-player-grid", player_nodes(bench)))
    ),
    v2_ui_depth_evidence(shadow)
  )
}

v2_ui_depth_lineups <- function(shadow, lineup_working_ui = NULL) {
  issue <- v2_ui_depth_state_issue(shadow)
  if (!is.null(issue)) {
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "lineup",
      v2_ui_status_chip(issue$status, issue$is_blocked),
      shiny::strong(issue$title),
      shiny::span(issue$detail)
    ))
  }

  lineups <- shadow$lineup_portfolio$lineups
  if (!is.data.frame(lineups) || !nrow(lineups)) {
    detail <- shadow$phase2_diagnostics$error$message %||%
      "The Phase 2 portfolio emitted no legal lineup groups. Review rotation, minute, stagger, and verified position-eligibility findings."
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "lineup",
      v2_ui_status_chip(shadow$lineup_portfolio$status %||% "REVIEW", shadow$lineup_portfolio$is_blocked %||% FALSE),
      shiny::strong("Lineup portfolio is unavailable"),
      shiny::span(v2_ui_text(detail))
    ))
  }

  player_names <- v2_ui_depth_player_names(shadow)
  type_labels <- c(
    BASE = "Starting Lineup",
    BENCH_BRIDGE = "Bench Bridge",
    OFFENSE = "Offense Lineup",
    DEFENSE = "Defense Lineup",
    SMALL_BALL = "Small-Ball Lineup",
    CLOSING = "Closing Lineup",
    BIE_WORKING = "BIE Recommended / Working Lineup"
  )
  lineup_card <- function(i) {
    type <- as.character(lineups$lineup_type[[i]])
    ids <- as.character(lineups$player_ids[[i]])
    names <- unname(player_names[ids])
    names[is.na(names) | !nzchar(names)] <- paste("Player", ids[is.na(names) | !nzchar(names)])
    explanation <- v2_ui_text(lineups$explanation[[i]], "No lineup explanation was returned.")
    reliability <- v2_ui_text(lineups$reliability[[i]], "UNKNOWN")
    shiny::div(
      class = "tbi-p3-lineup-card",
      `data-lineup-type` = type,
      shiny::div(
        class = "tbi-p3-lineup-head",
        shiny::strong(v2_ui_text(unname(type_labels[type]), v2_ui_humanize(type))),
        v2_ui_status_chip(lineups$legality_status[[i]])
      ),
      shiny::div(
        class = "tbi-p3-lineup-members",
        lapply(names, shiny::span)
      ),
      shiny::tags$small(
        class = "tbi-p3-lineup-note",
        v2_ui_concise_note(explanation, "No lineup explanation was returned.")
      ),
      shiny::tags$details(
        class = "tbi-p3-lineup-details",
        shiny::tags$summary("View details"),
        shiny::p(explanation),
        shiny::div(
          class = "tbi-p3-lineup-detail-provenance",
          v2_ui_provenance_chip(reliability, "model")
        )
      )
    )
  }

  missing_card <- function(type, reason) {
    shiny::div(
      class = "tbi-p3-lineup-card tbi-p3-lineup-unavailable",
      `data-lineup-type` = type,
      shiny::div(
        class = "tbi-p3-lineup-head",
        shiny::strong(v2_ui_text(unname(type_labels[type]), v2_ui_humanize(type))),
        v2_ui_status_chip("REVIEW")
      ),
      shiny::span(class = "tbi-p3-lineup-missing", reason)
    )
  }

  cards_for_type <- function(type, missing_reason) {
    indices <- which(lineups$lineup_type == type)
    if (length(indices)) {
      lapply(indices, lineup_card)
    } else {
      list(missing_card(type, missing_reason))
    }
  }

  situational_types <- c("OFFENSE", "DEFENSE", "SMALL_BALL", "BENCH_BRIDGE")
  situational_reasons <- c(
    OFFENSE = "No verified offense lineup was emitted.",
    DEFENSE = "No verified defense lineup was emitted.",
    SMALL_BALL = "No verified small-ball lineup was emitted.",
    BENCH_BRIDGE = "No verified bench-bridge lineup was emitted."
  )

  working_card <- if (is.null(lineup_working_ui)) {
    missing_card(
      "BIE_WORKING",
      "Working-lineup state is available only inside the interactive Depth Chart module."
    )
  } else {
    lineup_working_ui
  }

  status <- shadow$lineup_portfolio$status %||% shadow$phase2_diagnostics$status %||% "REVIEW"

  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-depth tbi-p3-lineup-portfolio tbi-depth-v2-view",
    `data-tbi-v2-depth-view` = "lineup",
    shiny::div(
      class = "tbi-p3-panel-head tbi-p3-lineup-portfolio-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "PHASE 2 · LINEUP PORTFOLIO"),
        shiny::h2("Lineup Portfolio"),
        shiny::p("The working five and governed situation groups, with evidence available on request.")
      ),
      v2_ui_status_chip(status, shadow$lineup_portfolio$is_blocked %||% FALSE)
    ),
    shiny::div(
      class = "tbi-p3-lineup-row tbi-p3-lineup-row-top",
      cards_for_type("BASE", "The governed portfolio did not emit a starting lineup."),
      working_card,
      cards_for_type("CLOSING", "The governed portfolio did not emit a closing lineup.")
    ),
    shiny::div(
      class = "tbi-p3-situational-lineups",
      `data-tbi-situational-active` = "OFFENSE",
      shiny::div(
        class = "tbi-p3-situational-head",
        shiny::strong("Situational Lineups"),
        shiny::div(
          class = "tbi-p3-situational-controls",
          lapply(situational_types, function(type) {
            shiny::tags$button(
              type = "button",
              class = "tbi-p3-situational-selector",
              `data-situational-lineup` = type,
              `aria-pressed` = if (identical(type, "OFFENSE")) "true" else "false",
              v2_ui_text(unname(type_labels[type]), v2_ui_humanize(type))
            )
          })
        )
      ),
      shiny::div(
        class = "tbi-p3-situational-detail",
        lapply(situational_types, function(type) {
          cards_for_type(type, unname(situational_reasons[type]))
        })
      )
    ),
    v2_ui_depth_evidence(shadow)
  )
}

v2_ui_depth_staggering <- function(shadow) {
  issue <- v2_ui_depth_state_issue(shadow)
  if (!is.null(issue)) {
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "staggering",
      v2_ui_status_chip(issue$status, issue$is_blocked),
      shiny::strong(issue$title),
      shiny::span(issue$detail)
    ))
  }

  segments <- shadow$stagger_plan$segments
  if (!is.data.frame(segments) || !nrow(segments)) {
    detail <- shadow$phase2_diagnostics$error$message %||%
      "The minute ledger exists, but no governed game segments were emitted. Review exact-minute and lineup-feasibility findings."
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "staggering",
      v2_ui_status_chip(shadow$stagger_plan$status %||% "REVIEW", shadow$stagger_plan$is_blocked %||% FALSE),
      shiny::strong("Staggering plan is unavailable"),
      shiny::span(v2_ui_text(detail))
    ))
  }

  player_names <- v2_ui_depth_player_names(shadow)
  substitutions <- shadow$stagger_plan$substitution_events

  segment_row <- function(i) {
    ids <- as.character(segments$player_ids[[i]])
    names <- unname(player_names[ids])
    names[is.na(names) | !nzchar(names)] <- paste("Player", ids[is.na(names) | !nzchar(names)])
    starter_overlap <- segments$starter_count[[i]]
    shiny::div(
      class = "tbi-p3-game-segment",
      shiny::strong(paste0(segments$start_clock[[i]], "–", segments$end_clock[[i]])),
      shiny::span(class = "tbi-p3-game-segment-players", paste(names, collapse = " · ")),
      shiny::tags$small(paste0("Starter overlap: ", starter_overlap, "/5"))
    )
  }

  quarter_card <- function(period) {
    indices <- which(as.integer(segments$period) == period)
    event_indices <- if (is.data.frame(substitutions) && "period" %in% names(substitutions)) {
      which(as.integer(substitutions$period) == period)
    } else {
      integer()
    }
    substitution_nodes <- if (length(event_indices)) {
      lapply(event_indices, function(i) {
        out_id <- as.character(substitutions$player_out[[i]])
        in_id <- as.character(substitutions$player_in[[i]])
        out_name <- v2_ui_text(unname(player_names[out_id]), paste("Player", out_id))
        in_name <- v2_ui_text(unname(player_names[in_id]), paste("Player", in_id))
        shiny::span(
          class = "tbi-p3-substitution-event",
          paste0(substitutions$clock[[i]], " · ", out_name, " out / ", in_name, " in")
        )
      })
    } else {
      shiny::span(class = "tbi-p3-substitution-empty", "No substitution events emitted.")
    }
    shiny::div(
      class = "tbi-p3-quarter-card",
      shiny::div(
        class = "tbi-p3-quarter-head",
        shiny::h3(paste0("Q", period)),
        shiny::span(if (length(indices)) paste(length(indices), "segments") else "No segments")
      ),
      if (length(indices)) {
        lapply(indices, segment_row)
      } else {
        shiny::div(
          class = "tbi-p3-lineup-unavailable",
          v2_ui_status_chip("REVIEW"),
          paste0("Q", period, " unavailable: the stagger contract emitted no segments for this quarter.")
        )
      },
      shiny::div(
        class = "tbi-p3-substitution-flow",
        shiny::strong("Substitution sequence"),
        substitution_nodes
      )
    )
  }

  status <- shadow$stagger_plan$status %||% shadow$phase2_diagnostics$status %||% "REVIEW"

  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-depth tbi-depth-v2-view",
    `data-tbi-v2-depth-view` = "staggering",
    shiny::div(
      class = "tbi-p3-panel-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "PHASE 2 · STAGGERING"),
        shiny::h2("Staggering"),
        shiny::p("Quarter-by-quarter on-court groups, substitution sequence, and starter overlap.")
      ),
      v2_ui_status_chip(status, shadow$stagger_plan$is_blocked %||% FALSE)
    ),
    shiny::div(class = "tbi-p3-quarter-grid", lapply(1:4, quarter_card)),
    v2_ui_depth_evidence(shadow)
  )
}

v2_ui_depth_gameplan <- function(shadow) {
  issue <- v2_ui_depth_state_issue(shadow)
  if (!is.null(issue)) {
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "gameplan",
      v2_ui_status_chip(issue$status, issue$is_blocked),
      shiny::strong(issue$title),
      shiny::span(issue$detail)
    ))
  }

  segments <- shadow$stagger_plan$segments
  if (!is.data.frame(segments) || !nrow(segments)) {
    detail <- shadow$phase2_diagnostics$error$message %||%
      "The minute ledger exists, but no governed game segments were emitted. Review exact-minute and lineup-feasibility findings."
    return(shiny::tags$section(
      class = "tbi-p3-panel tbi-p3-empty tbi-depth-v2-view",
      `data-tbi-v2-depth-view` = "gameplan",
      v2_ui_status_chip(shadow$stagger_plan$status %||% "REVIEW", shadow$stagger_plan$is_blocked %||% FALSE),
      shiny::strong("Game plan is unavailable"),
      shiny::span(v2_ui_text(detail))
    ))
  }

  rotation <- shadow$rotation_10$members
  player_names <- v2_ui_depth_player_names(shadow)
  sixth_rows <- rotation[rotation$rotation_role == "SIXTH_MAN", , drop = FALSE]
  sixth_id <- if (nrow(sixth_rows)) as.character(sixth_rows$player_id[[1]]) else NA_character_
  total_game_minutes <- suppressWarnings(sum(as.numeric(segments$duration), na.rm = TRUE))
  total_player_minutes <- suppressWarnings(as.integer(
    shadow$stagger_plan$total_player_minutes %||% shadow$minute_ledger$total_assigned_minutes
  ))

  sixth_entry <- "UNKNOWN"
  if (!is.na(sixth_id)) {
    entry <- which(vapply(segments$player_ids, function(ids) sixth_id %in% as.character(ids), logical(1)))
    if (length(entry)) {
      i <- entry[[1]]
      sixth_entry <- paste0("Q", segments$period[[i]], " · ", segments$start_clock[[i]], "–", segments$end_clock[[i]])
    }
  }

  coverage_summary <- function(field) {
    if (!field %in% names(segments)) return("UNKNOWN")
    values <- unique(vapply(segments[[field]], v2_ui_text, character(1)))
    paste(values, collapse = " / ")
  }

  starter_counts <- suppressWarnings(as.integer(segments$starter_count))
  valid_starter_counts <- starter_counts[!is.na(starter_counts)]
  minimum_starters <- if (length(valid_starter_counts)) min(valid_starter_counts) else NA_integer_
  rest_indices <- if (is.na(minimum_starters)) integer() else which(starter_counts == minimum_starters)
  rest_windows <- if (length(rest_indices)) {
    labels <- vapply(head(rest_indices, 2L), function(i) {
      paste0("Q", segments$period[[i]], " ", segments$start_clock[[i]], "–", segments$end_clock[[i]])
    }, character(1))
    paste(labels, collapse = " · ")
  } else {
    "UNKNOWN"
  }

  closing <- shadow$lineup_portfolio$lineups
  closing <- if (is.data.frame(closing)) closing[closing$lineup_type == "CLOSING", , drop = FALSE] else data.frame()
  closing_names <- if (nrow(closing)) {
    ids <- as.character(closing$player_ids[[1]])
    names <- unname(player_names[ids])
    names[is.na(names) | !nzchar(names)] <- paste("Player", ids[is.na(names) | !nzchar(names)])
    paste(names, collapse = " · ")
  } else {
    "No verified closing group"
  }

  overlap_range <- if (length(valid_starter_counts)) {
    paste0(min(valid_starter_counts), "–", max(valid_starter_counts), " of 5 starters")
  } else {
    "Starter overlap UNKNOWN"
  }
  flow_summary <- paste0(
    nrow(segments), " governed segments · ", overlap_range,
    " · sixth man enters ", sixth_entry, "."
  )
  status <- shadow$stagger_plan$status %||% shadow$phase2_diagnostics$status %||% "REVIEW"

  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-depth tbi-depth-v2-view tbi-p3-gameplan-summary",
    `data-tbi-v2-depth-view` = "gameplan",
    shiny::div(
      class = "tbi-p3-panel-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "PHASE 2 · GAME PLAN"),
        shiny::h2("48-Minute Game Plan"),
        shiny::p("The existing rotation plan summarized for coverage, rest, and closing decisions.")
      ),
      v2_ui_status_chip(status, shadow$stagger_plan$is_blocked %||% FALSE)
    ),
    shiny::div(
      class = "tbi-p3-gameplan-metrics",
      v2_ui_metric("Game length", "48 minutes", paste0(total_game_minutes, " / 48 represented")),
      v2_ui_metric("Team minutes", paste0(total_player_minutes, " / 240"), "Exact player exposure"),
      v2_ui_metric("Sixth-man entry", sixth_entry, if (nrow(sixth_rows)) sixth_rows$player_name[[1]] else "Role evidence unavailable"),
      v2_ui_metric("Closing group", if (nrow(closing)) "AVAILABLE" else "REVIEW", closing_names),
      v2_ui_metric("Creator coverage", coverage_summary("creator_coverage_status"), "Across governed segments"),
      v2_ui_metric("Center coverage", coverage_summary("big_center_coverage_status"), "Across governed segments"),
      v2_ui_metric("Key rest windows", rest_windows, if (is.na(minimum_starters)) "Overlap unavailable" else paste0(minimum_starters, "/5 starter overlap")),
      v2_ui_metric("Rotation flow", nrow(segments), "Governed substitution segments")
    ),
    shiny::div(
      class = "tbi-p3-gameplan-flow",
      shiny::strong("Rotation-flow summary"),
      shiny::span(flow_summary)
    ),
    v2_ui_depth_evidence(shadow)
  )
}

v2_ui_depth_intelligence <- function(shadow, lineup_working_ui = NULL) {
  shiny::div(
    class = "tbi-depth-v2-views",
    v2_ui_depth_rotation(shadow),
    v2_ui_depth_lineups(shadow, lineup_working_ui),
    v2_ui_depth_staggering(shadow),
    v2_ui_depth_gameplan(shadow)
  )
}

v2_ui_command_intelligence <- function(shadow, scenario = NULL) {
  if (!v2_ui_shadow_ready(shadow)) return(NULL)
  rotation <- shadow$rotation_10$members
  lineups <- shadow$lineup_portfolio$lineups
  findings <- unique(vapply(
    Filter(function(x) identical(x$status, "REVIEW"), shadow$validation_findings),
    function(x) v2_ui_text(x$code), character(1)
  ))
  risks <- head(findings, 5L)
  active <- tbi_scenario_is_shared_supported(scenario)
  scenario_impact <- if (active && is.list(scenario$v2_organizational_impact)) {
    scenario$v2_organizational_impact
  } else NULL
  scenario_recommendation <- if (!is.null(scenario_impact)) {
    v2_ui_text(scenario_impact$executive_recommendation, "PROCEED WITH REVIEW")
  } else NULL
  executive_recommendation <- if (!is.null(scenario_recommendation)) {
    scenario_recommendation
  } else if (shadow$phase2_diagnostics$is_blocked) {
    "Resolve blocking evidence before action"
  } else {
    "Advance to human review; do not treat shadow output as approval"
  }
  shiny::tags$section(
    class = "tbi-p3-panel tbi-p3-command",
    shiny::div(
      class = "tbi-p3-panel-head",
      shiny::div(
        shiny::span(class = "tbi-p3-kicker", "V2 SHADOW · DEVELOPMENT"),
        shiny::h2("Basketball Plan"),
        shiny::p("Rotation, minute, lineup, and evidence status supporting the canonical executive posture above.")
      ),
      v2_ui_status_chip(shadow$phase2_diagnostics$status, shadow$phase2_diagnostics$is_blocked)
    ),
    shiny::div(
      class = "tbi-p3-command-grid",
      shiny::div(
        class = "tbi-p3-command-primary",
        shiny::span("BASKETBALL REVIEW"),
        shiny::strong(executive_recommendation),
        shiny::p("CBA FAIL remains controlling. Basketball output cannot override transaction legality."),
        shiny::div(
          class = "tbi-p3-provenance-row",
          v2_ui_provenance_chip("MODEL OUTPUT", "model"),
          v2_ui_provenance_chip("HUMAN REVIEW REQUIRED", "unknown"),
          if (active) v2_ui_provenance_chip("USER SCENARIO", "override")
        )
      ),
      shiny::div(
        class = "tbi-p3-risk-list",
        shiny::span("KEY RISKS"),
        if (length(risks)) {
          lapply(risks, function(risk) shiny::div(shiny::span("!"), v2_ui_humanize(risk)))
        } else {
          shiny::div(shiny::span("!"), "No V2 review findings")
        }
      )
    ),
    shiny::div(
      class = "tbi-p3-metric-grid six",
      v2_ui_metric("Rotation", nrow(rotation), "MODEL OUTPUT"),
      v2_ui_metric("Starters", sum(rotation$is_starter), "Approved state"),
      v2_ui_metric("Minute plan", paste0(shadow$minute_ledger$total_assigned_minutes, " / 240"), shadow$minute_ledger$status),
      v2_ui_metric("Lineups", nrow(lineups), "Legal portfolio"),
      v2_ui_metric("Closing", if (any(lineups$lineup_type == "CLOSING")) "LEGAL" else "REVIEW"),
      v2_ui_metric(
        "Scenario",
        if (!is.null(scenario_recommendation)) scenario_recommendation else if (active) "ACTIVE" else "BASELINE",
        if (active) "Session scoped" else "No active delta"
      )
    )
  )
}
