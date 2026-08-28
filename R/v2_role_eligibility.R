# ============================================================
# TBI V2 Phase 1D verified role-eligibility adapter
# ============================================================

v2_role_policy <- function() {
  list(
    policy_type = "tbi-v2-role-evidence-policy",
    contract_version = "1.0.0",
    supported_roles = c(
      "POSITION_PG", "POSITION_SG", "POSITION_SF", "POSITION_PF", "POSITION_C",
      "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR",
      "BACKUP_PG", "BACKUP_C"
    ),
    eligibility = c("ELIGIBLE", "NOT_ELIGIBLE", "UNKNOWN"),
    evidence_classes = c(
      "AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION", "MODEL_EVIDENCE", "UNKNOWN"
    ),
    verification_statuses = c("VERIFIED", "DERIVED_VERIFIED", "UNVERIFIED", "MISSING"),
    precedence = c(
      "MANUAL_VERIFIED", "AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION",
      "MODEL_EVIDENCE", "UNKNOWN"
    ),
    canonical_positions = c("PG", "SG", "SF", "PF", "C"),
    generic_position_implies_backup_role = FALSE,
    model_evidence_is_authoritative = FALSE,
    absence_implies_not_eligible = FALSE
  )
}


v2_role_evidence_sources <- function() {
  data.frame(
    source = c(
      "MANUAL_VERIFIED", "APPROVED_POSITION_OVERRIDE", "DEPTH_CHART_POSITION",
      "OFFICIAL_PRIMARY_POSITION", "VERIFIED_MULTI_POSITION_INPUT",
      "PLAYER_POSITIONS_TABLE", "MODEL_OR_BIE_EVIDENCE", "GENERIC_POSITION_STRING",
      "BACKUP_ROLE_SOURCE", "PLAYMAKING_MODEL_EVIDENCE",
      "DEFENSE_REBOUNDING_MODEL_EVIDENCE"
    ),
    evidence_class = c(
      rep("AUTHORITATIVE_FACT", 5L), "UNKNOWN", "MODEL_EVIDENCE", "UNKNOWN", "UNKNOWN",
      "MODEL_EVIDENCE", "MODEL_EVIDENCE"
    ),
    derivation_rule = c(
      "Use the explicit player/team/season/role/eligibility record after provenance validation.",
      "Verify only the exact approved slot when the override flag and reason are present.",
      "Verify only an exact canonical stored depth-chart slot; absence proves nothing.",
      "Verify only an exact canonical primary position; compound or generic strings remain unknown.",
      "Verify only listed slots when source, version, and VERIFIED status are supplied explicitly.",
      "Current schema lacks approved provenance and verification metadata; rows remain unknown.",
      "May support explanation or ranking but cannot establish role eligibility.",
      "Never expand G, F, height, name, or arbitrary compound text into verified eligibility.",
      "No current authoritative BACKUP_PG or BACKUP_C source exists; use manual verified evidence.",
      "Creation roles and scores remain review evidence and do not establish creator or handler eligibility.",
      "Defensive roles and interior scores remain review evidence and do not establish rim-protector eligibility."
    ),
    can_establish_eligible = c(TRUE, TRUE, TRUE, TRUE, TRUE, rep(FALSE, 6L)),
    can_establish_not_eligible = c(TRUE, rep(FALSE, 10L)),
    stringsAsFactors = FALSE
  )
}


validate_v2_role_policy <- function(policy) {
  expected <- v2_role_policy()
  required <- names(expected)
  if (!is.list(policy) || !all(required %in% names(policy))) {
    stop("policy is missing required Phase 1D role-evidence fields.", call. = FALSE)
  }
  if (any(vapply(required, function(name) {
    !identical(policy[[name]], expected[[name]])
  }, logical(1)))) {
    stop("policy weakens the approved Phase 1D evidence contract.", call. = FALSE)
  }
  policy
}


v2_role_scalar_text <- function(x, field) {
  value <- trimws(as.character(x %||% ""))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(field, " must be one non-empty value.", call. = FALSE)
  }
  value
}


v2_role_exact_position <- function(x, policy = v2_role_policy()) {
  value <- toupper(trimws(as.character(x %||% "")))
  if (length(value) != 1L || is.na(value) || !value %in% policy$canonical_positions) {
    return(NA_character_)
  }
  value
}


v2_role_manual_records <- function(manual_evidence, roster_ids, team_id, season,
                                   policy = v2_role_policy()) {
  if (is.null(manual_evidence)) return(list())
  if (!is.data.frame(manual_evidence)) {
    stop("manual_evidence must be a data frame.", call. = FALSE)
  }
  required <- c(
    "player_id", "team_id", "season", "role", "eligibility", "source",
    "author_source_note", "reason"
  )
  missing <- setdiff(required, names(manual_evidence))
  if (length(missing)) {
    stop("manual_evidence is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(manual_evidence)) return(list())

  normalized <- manual_evidence
  normalized$player_id <- suppressWarnings(as.integer(normalized$player_id))
  normalized$team_id <- trimws(as.character(normalized$team_id))
  normalized$season <- trimws(as.character(normalized$season))
  normalized$role <- toupper(trimws(as.character(normalized$role)))
  normalized$eligibility <- toupper(trimws(as.character(normalized$eligibility)))
  normalized$source <- toupper(trimws(as.character(normalized$source)))
  normalized$author_source_note <- trimws(as.character(normalized$author_source_note))
  normalized$reason <- trimws(as.character(normalized$reason))

  phase1e_roles <- c(
    "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR"
  )
  phase1e_rows <- normalized$role %in% phase1e_roles
  phase1e_required <- c(
    "source_version", "verification_status", "verified_by", "effective_date",
    "evidence_version"
  )
  if (any(phase1e_rows)) {
    phase1e_missing <- setdiff(phase1e_required, names(normalized))
    if (length(phase1e_missing)) {
      stop("Phase 1E manual role evidence is missing: ",
        paste(phase1e_missing, collapse = ", "), call. = FALSE)
    }
    for (field in phase1e_required) {
      values <- trimws(as.character(normalized[[field]][phase1e_rows]))
      if (any(is.na(values) | !nzchar(values))) {
        stop("Phase 1E manual role evidence requires ", field, ".", call. = FALSE)
      }
    }
    statuses <- toupper(trimws(as.character(
      normalized$verification_status[phase1e_rows]
    )))
    if (any(statuses != "VERIFIED")) {
      stop("Phase 1E manual role evidence must be VERIFIED.", call. = FALSE)
    }
    invisible(lapply(normalized$effective_date[phase1e_rows], function(value) {
      v2_availability_date(value, "effective_date", required = TRUE)
    }))
  }

  if (any(is.na(normalized$player_id))) stop("manual player_id is malformed.", call. = FALSE)
  if (any(!normalized$player_id %in% roster_ids)) {
    stop("manual role evidence references a player outside roster.", call. = FALSE)
  }
  if (any(normalized$team_id != as.character(team_id))) {
    stop("manual role evidence has the wrong team.", call. = FALSE)
  }
  if (any(normalized$season != season)) {
    stop("manual role evidence has the wrong season.", call. = FALSE)
  }
  if (any(!normalized$role %in% policy$supported_roles)) {
    stop("manual role evidence contains an unsupported role.", call. = FALSE)
  }
  if (any(!normalized$eligibility %in% policy$eligibility)) {
    stop("manual role evidence contains an invalid eligibility.", call. = FALSE)
  }
  if (any(normalized$source != "MANUAL_VERIFIED")) {
    stop("manual role evidence source must be MANUAL_VERIFIED.", call. = FALSE)
  }
  if (any(!nzchar(normalized$author_source_note))) {
    stop("manual role evidence requires an author/source note.", call. = FALSE)
  }
  if (any(!nzchar(normalized$reason))) {
    stop("manual role evidence requires a reason.", call. = FALSE)
  }

  key <- paste(normalized$player_id, normalized$role, sep = "::")
  duplicate_keys <- unique(key[duplicated(key) | duplicated(key, fromLast = TRUE)])
  if (length(duplicate_keys)) {
    conflicting <- vapply(duplicate_keys, function(item) {
      length(unique(normalized$eligibility[key == item])) > 1L
    }, logical(1))
    if (any(conflicting)) stop("manual role evidence contains conflicting records.", call. = FALSE)
    stop("manual role evidence contains duplicate player-role records.", call. = FALSE)
  }

  order_index <- order(normalized$player_id, match(normalized$role, policy$supported_roles), method = "radix")
  normalized <- normalized[order_index, , drop = FALSE]
  lapply(seq_len(nrow(normalized)), function(i) {
    row <- normalized[i, , drop = FALSE]
    new_v2_role_eligibility(
      player_id = row$player_id[[1]],
      team_id = team_id,
      season = season,
      role = row$role[[1]],
      eligibility = row$eligibility[[1]],
      evidence_status = if (row$eligibility[[1]] == "UNKNOWN") "UNKNOWN" else "VERIFIED",
      evidence_source = "MANUAL_VERIFIED",
      evidence_class = if (row$eligibility[[1]] == "UNKNOWN") "UNKNOWN" else "AUTHORITATIVE_FACT",
      source_field = "manual_evidence.eligibility",
      source_version = if ("source_version" %in% names(row) &&
        nzchar(trimws(as.character(row$source_version[[1]])))) {
        as.character(row$source_version[[1]])
      } else {
        policy$contract_version
      },
      verification_status = if (row$eligibility[[1]] == "UNKNOWN") "UNVERIFIED" else "VERIFIED",
      evidence_fields = c("eligibility", "author_source_note", "reason"),
      missing_fields = if (row$eligibility[[1]] == "UNKNOWN") "verified_role_eligibility" else character(),
      reason_codes = c("MANUAL_VERIFIED_ROLE_EVIDENCE", paste0(row$role[[1]], "_", row$eligibility[[1]])),
      explanation = paste(row$reason[[1]], "Source:", row$author_source_note[[1]]),
      verified_by = if ("verified_by" %in% names(row)) row$verified_by[[1]] else NULL,
      effective_date = if ("effective_date" %in% names(row)) row$effective_date[[1]] else NULL,
      expiration_date = if ("expiration_date" %in% names(row)) row$expiration_date[[1]] else NULL,
      evidence_version = if ("evidence_version" %in% names(row)) {
        row$evidence_version[[1]]
      } else {
        policy$contract_version
      }
    )
  })
}


v2_role_verified_positions <- function(roster, index, policy = v2_role_policy()) {
  if (!"verified_positions" %in% names(roster)) return(character())
  required <- c(
    "verified_positions_source", "verified_positions_source_version",
    "verified_positions_verification_status"
  )
  if (!all(required %in% names(roster))) return(character())
  source <- trimws(as.character(roster$verified_positions_source[[index]] %||% ""))
  version <- trimws(as.character(roster$verified_positions_source_version[[index]] %||% ""))
  status <- toupper(trimws(as.character(
    roster$verified_positions_verification_status[[index]] %||% ""
  )))
  if (is.na(source) || is.na(version) || is.na(status) ||
      !nzchar(source) || !nzchar(version) || status != "VERIFIED") return(character())
  value <- roster$verified_positions[[index]]
  if (is.null(value) || !length(value)) return(character())
  value <- unique(toupper(trimws(as.character(value))))
  value[value %in% policy$canonical_positions]
}


v2_role_position_record <- function(roster, index, role, team_id, season,
                                    policy = v2_role_policy()) {
  player_id <- roster$player_id[[index]]
  target <- sub("^POSITION_", "", role)
  assigned_field <- if ("source_position" %in% names(roster)) "source_position" else "position"
  assigned <- if (assigned_field %in% names(roster)) {
    v2_role_exact_position(roster[[assigned_field]][[index]], policy)
  } else {
    NA_character_
  }
  primary <- if ("primary_position" %in% names(roster)) {
    v2_role_exact_position(roster$primary_position[[index]], policy)
  } else {
    NA_character_
  }
  verified_positions <- v2_role_verified_positions(roster, index, policy)
  is_override <- "is_position_override" %in% names(roster) &&
    isTRUE(suppressWarnings(as.integer(roster$is_position_override[[index]])) == 1L)
  override_reason <- if ("position_override_reason" %in% names(roster)) {
    trimws(as.character(roster$position_override_reason[[index]] %||% ""))
  } else {
    ""
  }
  if (is.na(override_reason)) override_reason <- ""

  evidence <- NULL
  if (is_override && nzchar(override_reason) && identical(assigned, target)) {
    evidence <- list(
      source = "APPROVED_POSITION_OVERRIDE",
      source_field = "depth_chart_overrides.position",
      source_version = "depth-chart-override-v1",
      evidence_class = "AUTHORITATIVE_FACT",
      verification_status = "VERIFIED",
      reason = "APPROVED_POSITION_ASSIGNMENT",
      explanation = paste("Approved position override:", override_reason)
    )
  } else if (target %in% verified_positions) {
    evidence <- list(
      source = as.character(roster$verified_positions_source[[index]]),
      source_field = "verified_positions",
      source_version = as.character(roster$verified_positions_source_version[[index]]),
      evidence_class = "AUTHORITATIVE_FACT",
      verification_status = "VERIFIED",
      reason = "VERIFIED_MULTI_POSITION_ELIGIBILITY",
      explanation = "The explicit verified-position ledger includes this position."
    )
  } else if (!is_override && identical(assigned, target)) {
    evidence <- list(
      source = "DEPTH_CHART_POSITION",
      source_field = assigned_field,
      source_version = "depth-chart-v1",
      evidence_class = "AUTHORITATIVE_FACT",
      verification_status = "VERIFIED",
      reason = "STORED_POSITION_ASSIGNMENT",
      explanation = "The authoritative depth chart explicitly assigns this exact position."
    )
  } else if (identical(primary, target)) {
    evidence <- list(
      source = "OFFICIAL_PRIMARY_POSITION",
      source_field = "players.primary_position",
      source_version = "players-v1",
      evidence_class = "AUTHORITATIVE_FACT",
      verification_status = "VERIFIED",
      reason = "EXACT_PRIMARY_POSITION",
      explanation = "The authoritative player record stores this exact primary position."
    )
  }

  if (is.null(evidence)) {
    return(new_v2_role_eligibility(
      player_id = player_id, team_id = team_id, season = season, role = role,
      missing_fields = "verified_position_eligibility",
      evidence_class = "UNKNOWN", verification_status = "MISSING",
      reason_codes = "POSITION_ELIGIBILITY_UNKNOWN",
      explanation = "No approved exact-position or verified multi-position evidence supports this assignment."
    ))
  }
  new_v2_role_eligibility(
    player_id = player_id, team_id = team_id, season = season, role = role,
    eligibility = "ELIGIBLE", evidence_status = "VERIFIED",
    evidence_source = evidence$source, evidence_class = evidence$evidence_class,
    source_field = evidence$source_field, source_version = evidence$source_version,
    verification_status = evidence$verification_status,
    evidence_fields = evidence$source_field, reason_codes = evidence$reason,
    explanation = evidence$explanation
  )
}


v2_role_unknown_record <- function(player_id, team_id, season, role) {
  field <- paste0(tolower(role), "_eligibility")
  new_v2_role_eligibility(
    player_id = player_id, team_id = team_id, season = season, role = role,
    missing_fields = field, evidence_class = "UNKNOWN", verification_status = "MISSING",
    reason_codes = paste0(role, "_EVIDENCE_MISSING"),
    explanation = paste(role, "requires explicit verified evidence; no inference was made.")
  )
}


build_v2_role_eligibility_ledger <- function(roster,
                                             team_id,
                                             season,
                                             manual_evidence = NULL,
                                             policy = v2_role_policy()) {
  policy <- validate_v2_role_policy(policy)
  if (!is.data.frame(roster)) stop("roster must be a data frame.", call. = FALSE)
  if (!all(c("player_id", "player_name") %in% names(roster))) {
    stop("roster must contain player_id and player_name.", call. = FALSE)
  }
  team_id <- v2_role_scalar_text(team_id, "team_id")
  season <- v2_role_scalar_text(season, "season")
  normalized <- roster
  normalized$player_id <- suppressWarnings(as.integer(normalized$player_id))
  if (any(is.na(normalized$player_id)) || anyDuplicated(normalized$player_id)) {
    stop("roster player_id values must be known and unique.", call. = FALSE)
  }
  if ("team_id" %in% names(normalized)) {
    roster_teams <- unique(trimws(as.character(normalized$team_id)))
    roster_teams <- roster_teams[!is.na(roster_teams) & nzchar(roster_teams)]
    if (length(roster_teams) && !identical(roster_teams, team_id)) {
      stop("roster has the wrong team for the requested role ledger.", call. = FALSE)
    }
  }
  if ("season" %in% names(normalized)) {
    roster_seasons <- unique(trimws(as.character(normalized$season)))
    roster_seasons <- roster_seasons[!is.na(roster_seasons) & nzchar(roster_seasons)]
    if (length(roster_seasons) && !identical(roster_seasons, season)) {
      stop("roster has the wrong season for the requested role ledger.", call. = FALSE)
    }
  }
  normalized <- normalized[order(normalized$player_id, method = "radix"), , drop = FALSE]
  manual <- v2_role_manual_records(
    manual_evidence, normalized$player_id, team_id, season, policy
  )
  manual_keys <- vapply(manual, function(x) paste(x$player_id, x$role, sep = "::"), character(1))

  records <- list()
  for (i in seq_len(nrow(normalized))) {
    for (role in policy$supported_roles) {
      key <- paste(normalized$player_id[[i]], role, sep = "::")
      manual_index <- match(key, manual_keys)
      record <- if (!is.na(manual_index)) {
        manual[[manual_index]]
      } else if (grepl("^POSITION_", role)) {
        v2_role_position_record(normalized, i, role, team_id, season, policy)
      } else if (length(v2_role_model_fields(role)) > 0L) {
        v2_role_model_record(normalized, i, role, team_id, season)
      } else {
        v2_role_unknown_record(normalized$player_id[[i]], team_id, season, role)
      }
      records[[length(records) + 1L]] <- record
    }
  }
  keys <- vapply(records, function(x) paste(x$player_id, x$role, sep = "::"), character(1))
  if (anyDuplicated(keys)) stop("ledger contains duplicate player-role records.", call. = FALSE)
  identity <- list(
    contract_version = policy$contract_version,
    team_id = team_id,
    season = season,
    policy_signature = v2_input_signature(policy),
    records = records
  )
  list(
    contract_type = "tbi-v2-role-eligibility-ledger",
    contract_version = policy$contract_version,
    team_id = team_id,
    season = season,
    evidence_precedence = policy$precedence,
    records = records,
    input_signature = v2_input_signature(identity),
    status = if (any(vapply(records, function(x) x$eligibility == "UNKNOWN", logical(1)))) {
      "REVIEW"
    } else {
      "PASS"
    },
    is_blocked = FALSE,
    conflicting_count = 0L,
    malformed_count = 0L
  )
}


v2_role_record <- function(ledger, player_id, role) {
  if (!is.list(ledger) || !identical(ledger$contract_type, "tbi-v2-role-eligibility-ledger")) {
    stop("ledger must be a Phase 1D role-eligibility ledger.", call. = FALSE)
  }
  player_id <- suppressWarnings(as.integer(player_id))
  role <- toupper(trimws(as.character(role)))
  matches <- Filter(
    function(x) identical(x$player_id, player_id) && identical(x$role, role),
    ledger$records
  )
  if (length(matches) != 1L) stop("ledger does not contain exactly one requested record.", call. = FALSE)
  matches[[1]]
}


v2_role_position_assignment <- function(
    player_ids,
    position_index,
    positions = v2_role_policy()$canonical_positions) {
  ids <- suppressWarnings(as.integer(player_ids))
  positions <- as.character(positions)
  if (length(ids) != length(positions) || any(is.na(ids)) || anyDuplicated(ids)) {
    return(NULL)
  }

  eligible <- position_index[as.character(ids)]
  assign_one <- function(remaining_positions, remaining_ids, assigned) {
    if (!length(remaining_positions)) return(assigned)
    counts <- vapply(remaining_positions, function(position) {
      sum(vapply(remaining_ids, function(id) {
        position %in% eligible[[as.character(id)]]
      }, logical(1)))
    }, integer(1))
    if (any(counts == 0L)) return(NULL)

    position <- remaining_positions[
      order(counts, match(remaining_positions, positions))
    ][[1]]
    candidates <- sort(remaining_ids[vapply(remaining_ids, function(id) {
      position %in% eligible[[as.character(id)]]
    }, logical(1))])
    for (id in candidates) {
      result <- assign_one(
        setdiff(remaining_positions, position),
        setdiff(remaining_ids, id),
        c(assigned, stats::setNames(position, as.character(id)))
      )
      if (!is.null(result)) return(result)
    }
    NULL
  }

  assignment <- assign_one(positions, sort(ids), character())
  if (is.null(assignment)) return(NULL)
  assignment <- assignment[as.character(ids)]
  if (exists("position_balance_evaluation", mode = "function")) {
    protected <- position_balance_evaluation(unname(assignment))
    if (!isTRUE(protected$balanced) || isTRUE(protected$review_required)) {
      return(NULL)
    }
  }
  assignment
}


v2_rotation_role_records <- function(ledger) {
  Filter(function(x) x$role %in% c("BACKUP_PG", "BACKUP_C"), ledger$records)
}


summarize_v2_role_completeness <- function(ledger, roster = NULL) {
  records <- ledger$records
  position <- Filter(function(x) grepl("^POSITION_", x$role), records)
  backup_pg <- Filter(function(x) identical(x$role, "BACKUP_PG"), records)
  backup_c <- Filter(function(x) identical(x$role, "BACKUP_C"), records)
  primary_creator <- Filter(function(x) identical(x$role, "PRIMARY_CREATOR"), records)
  secondary_creator <- Filter(function(x) identical(x$role, "SECONDARY_CREATOR"), records)
  ball_handler <- Filter(function(x) identical(x$role, "BALL_HANDLER"), records)
  rim_protector <- Filter(function(x) identical(x$role, "RIM_PROTECTOR"), records)
  eligible_count <- function(values) sum(vapply(
    values, function(x) identical(x$eligibility, "ELIGIBLE") &&
      x$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED"), logical(1)
  ))
  verified_pg <- eligible_count(backup_pg)
  verified_c <- eligible_count(backup_c)
  verified_primary_creator <- eligible_count(primary_creator)
  verified_secondary_creator <- eligible_count(secondary_creator)
  verified_ball_handler <- eligible_count(ball_handler)
  verified_rim_protector <- eligible_count(rim_protector)
  position_by_player <- split(position, vapply(position, `[[`, integer(1), "player_id"))
  player_has_verified_position <- vapply(position_by_player, function(values) {
    eligible_count(values) > 0L
  }, logical(1))
  verified_position_players <- sum(player_has_verified_position)
  unknown_position_players <- sum(!player_has_verified_position)
  unknown_position_records <- sum(vapply(
    position, function(x) x$eligibility == "UNKNOWN", logical(1)
  ))
  all_roles <- c(
    backup_pg, backup_c, primary_creator, secondary_creator, ball_handler, rim_protector
  )
  unknown_roles <- sum(vapply(all_roles, function(x) x$eligibility == "UNKNOWN", logical(1)))
  coverage <- if (verified_pg > 0L && verified_c > 0L) "PASS" else "REVIEW"
  list(
    team_id = ledger$team_id,
    season = ledger$season,
    roster_players = length(unique(vapply(records, `[[`, integer(1), "player_id"))),
    verified_position_eligibility = as.integer(verified_position_players),
    unknown_position_eligibility = as.integer(unknown_position_players),
    unknown_position_records = as.integer(unknown_position_records),
    verified_backup_pg_candidates = as.integer(verified_pg),
    verified_backup_c_candidates = as.integer(verified_c),
    verified_primary_creator_candidates = as.integer(verified_primary_creator),
    verified_secondary_creator_candidates = as.integer(verified_secondary_creator),
    verified_ball_handler_candidates = as.integer(verified_ball_handler),
    verified_rim_protector_candidates = as.integer(verified_rim_protector),
    missing_backup_pg_evidence = verified_pg == 0L,
    missing_backup_c_evidence = verified_c == 0L,
    unknown_role_records = as.integer(unknown_roles),
    conflicting_count = as.integer(ledger$conflicting_count %||% 0L),
    malformed_count = as.integer(ledger$malformed_count %||% 0L),
    team_role_coverage_status = coverage,
    reason_codes = c(
      if (verified_pg == 0L) "BACKUP_PG_EVIDENCE_MISSING",
      if (verified_c == 0L) "BACKUP_C_EVIDENCE_MISSING"
    ),
    input_signature = v2_input_signature(ledger$input_signature)
  )
}


v2_role_authoritative_roster_snapshot <- function(team, season, db_path = NULL) {
  team <- v2_role_scalar_text(team, "team")
  season <- v2_role_scalar_text(season, "season")
  con <- connect_db(db_path = db_path, read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)
  roster <- DBI::dbGetQuery(
    con,
    "
      WITH selected_team AS (
        SELECT team_id
        FROM teams
        WHERE team_name = ? OR abbreviation = ? OR CAST(team_id AS TEXT) = CAST(? AS TEXT)
        LIMIT 1
      ),
      contract_data AS (
        SELECT
          cy.player_id,
          cy.team_id,
          cy.season,
          MAX(COALESCE(cy.cap_hit, cy.base_salary, 0)) AS salary
        FROM contract_years cy
        WHERE cy.team_id = (SELECT team_id FROM selected_team)
          AND cy.season = ?
        GROUP BY cy.player_id, cy.team_id, cy.season
      )
      SELECT
        dc.player_id,
        dc.team_id,
        dc.season,
        p.player_name,
        t.abbreviation AS team_abbreviation,
        CASE WHEN dco.player_id IS NULL THEN dc.position ELSE dco.position END AS source_position,
        p.primary_position,
        CASE WHEN dco.player_id IS NULL THEN dc.depth_order ELSE dco.depth_order END AS depth_order,
        CASE WHEN dco.player_id IS NULL THEN dc.is_starter ELSE dco.is_starter END AS is_starter,
        CASE WHEN dco.player_id IS NULL THEN 0 ELSE 1 END AS has_override,
        COALESCE(dco.is_position_override, 0) AS is_position_override,
        dco.position_override_reason,
        dco.position_override_updated_at,
        COALESCE(cd.salary, 0) AS salary,
        COALESCE(rh.roster_status, 'Active') AS roster_status,
        COALESCE(rh.two_way_flag, 0) AS two_way_flag
      FROM depth_chart dc
      INNER JOIN players p ON p.player_id = dc.player_id
      INNER JOIN teams t ON t.team_id = dc.team_id
      LEFT JOIN depth_chart_overrides dco
        ON dco.player_id = dc.player_id
       AND dco.team_id = dc.team_id
       AND dco.season = dc.season
      LEFT JOIN roster_history rh
        ON rh.player_id = dc.player_id
       AND rh.team_id = dc.team_id
       AND rh.season = dc.season
      LEFT JOIN contract_data cd
        ON cd.player_id = dc.player_id
       AND cd.team_id = dc.team_id
       AND cd.season = dc.season
      WHERE (t.team_name = ? OR t.abbreviation = ? OR CAST(t.team_id AS TEXT) = CAST(? AS TEXT))
        AND dc.season = ?
      ORDER BY dc.player_id
    ",
    params = list(team, team, team, season, team, team, team, season)
  )
  if (!nrow(roster)) return(roster)
  roster$position <- vapply(roster$source_position, normalize_depth_position, character(1))
  positions <- c("PG", "SG", "SF", "PF", "C", "OTHER")
  ranked <- lapply(positions, function(position) {
    rows <- roster[roster$position == position, , drop = FALSE]
    if (!nrow(rows)) return(NULL)
    manual_starter <- rows$has_override == 1L & rows$is_starter == 1L
    conflict_ids <- suppressWarnings(as.integer(rows$player_id[manual_starter]))
    rows$approved_lock_conflict <- length(conflict_ids) > 1L
    rows$approved_lock_conflict_player_ids <- I(rep(list(conflict_ids), nrow(rows)))
    order_index <- order(
      -as.integer(manual_starter),
      -suppressWarnings(as.integer(rows$is_starter)),
      ifelse(rows$has_override == 1L, rows$depth_order, 999L),
      rows$depth_order,
      -rows$salary,
      rows$player_name,
      method = "radix"
    )
    rows <- rows[order_index, , drop = FALSE]
    rows$depth_order <- seq_len(nrow(rows))
    rows$is_starter <- 0L
    rows$is_starter[[1]] <- 1L
    rows
  })
  roster <- do.call(rbind, ranked[!vapply(ranked, is.null, logical(1))])
  rownames(roster) <- NULL
  player_ids <- unique(suppressWarnings(as.integer(roster$player_id)))
  roster$verified_positions <- I(rep(list(character()), length(player_ids)))
  roster$verified_positions_source <- NA_character_
  roster$verified_positions_source_version <- NA_character_
  roster$verified_positions_verification_status <- "MISSING"
  roster$availability_status <- "UNKNOWN"
  roster
}


summarize_v2_role_league <- function(team_results) {
  if (!is.data.frame(team_results)) stop("team_results must be a data frame.", call. = FALSE)
  statuses <- c("PASS", "REVIEW", "FAIL")
  count_status <- function(column, status) sum(team_results[[column]] == status, na.rm = TRUE)
  data.frame(
    metric = c(
      "teams", paste0("starter_", tolower(statuses)),
      paste0("rotation_10_", tolower(statuses)), paste0("rotation_11_", tolower(statuses)),
      "verified_availability", "unknown_availability",
      "verified_backup_pg_candidates", "verified_backup_c_candidates",
      "verified_primary_creator_candidates", "verified_secondary_creator_candidates",
      "verified_ball_handler_candidates", "verified_rim_protector_candidates",
      "unknown_role_records"
    ),
    value = c(
      nrow(team_results),
      vapply(statuses, function(x) count_status("starter_status", x), integer(1)),
      vapply(statuses, function(x) count_status("rotation_10_status", x), integer(1)),
      vapply(statuses, function(x) count_status("rotation_11_status", x), integer(1)),
      sum(team_results$verified_availability_count, na.rm = TRUE),
      sum(team_results$unknown_availability_count, na.rm = TRUE),
      sum(team_results$verified_backup_pg_count, na.rm = TRUE),
      sum(team_results$verified_backup_c_count, na.rm = TRUE),
      sum(team_results$verified_primary_creator_count, na.rm = TRUE),
      sum(team_results$verified_secondary_creator_count, na.rm = TRUE),
      sum(team_results$verified_ball_handler_count, na.rm = TRUE),
      sum(team_results$verified_rim_protector_count, na.rm = TRUE),
      sum(team_results$unknown_role_count, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}
