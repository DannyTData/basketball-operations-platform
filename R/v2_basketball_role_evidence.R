# ============================================================
# TBI V2 Phase 1E basketball-role evidence adapters
# ============================================================

v2_role_model_fields <- function(role) {
  switch(
    role,
    PRIMARY_CREATOR = c(
      "creation_role", "creation_score", "primary_role", "playmaking_confidence"
    ),
    SECONDARY_CREATOR = c(
      "creation_role", "secondary_creation_score", "secondary_role",
      "playmaking_confidence"
    ),
    BALL_HANDLER = c(
      "creation_role", "creation_score", "passing_control_score",
      "ball_security_score", "playmaking_confidence"
    ),
    RIM_PROTECTOR = c(
      "defensive_role", "interior_impact_score", "block_impact_percentile",
      "defense_confidence"
    ),
    character()
  )
}


v2_role_model_source <- function(role) {
  if (role %in% c("PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER")) {
    return("PLAYMAKING_MODEL_EVIDENCE")
  }
  if (identical(role, "RIM_PROTECTOR")) return("DEFENSE_REBOUNDING_MODEL_EVIDENCE")
  NULL
}


v2_role_model_record <- function(roster, index, role, team_id, season) {
  fields <- intersect(v2_role_model_fields(role), names(roster))
  present <- fields[vapply(fields, function(field) {
    value <- roster[[field]][[index]]
    length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))
  }, logical(1))]
  if (!length(present)) {
    return(v2_role_unknown_record(roster$player_id[[index]], team_id, season, role))
  }
  source_version_fields <- intersect(
    c("role_metric_version", "playmaking_metric_version", "defense_metric_version",
      "metric_version", "model_version"),
    names(roster)
  )
  versions <- unique(unlist(lapply(source_version_fields, function(field) {
    value <- trimws(as.character(roster[[field]][[index]] %||% ""))
    if (length(value) == 1L && !is.na(value) && nzchar(value)) value else character()
  }), use.names = FALSE))
  new_v2_role_eligibility(
    player_id = roster$player_id[[index]], team_id = team_id, season = season,
    role = role, eligibility = "UNKNOWN", evidence_status = "UNKNOWN",
    evidence_source = v2_role_model_source(role),
    evidence_fields = present,
    missing_fields = paste0(tolower(role), "_verified_eligibility"),
    evidence_class = "MODEL_EVIDENCE",
    source_field = paste(present, collapse = "|"),
    source_version = if (length(versions)) paste(versions, collapse = "|") else
      "UNVERSIONED_MODEL_EVIDENCE",
    verification_status = "UNVERIFIED",
    reason_codes = c("MODEL_EVIDENCE_ONLY", paste0(role, "_ELIGIBILITY_UNKNOWN")),
    explanation = paste(
      "Existing model fields support review for", role,
      "but do not establish verified eligibility."
    )
  )
}


v2_evidence_diagnostic_row <- function(evidence_type,
                                       records,
                                       require_all = FALSE,
                                       aggregate_position = FALSE) {
  verified_record <- function(record) {
    record$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED") &&
      record$eligibility %in% c("ELIGIBLE", "NOT_ELIGIBLE")
  }
  eligible_record <- function(record) {
    verified_record(record) && identical(record$eligibility, "ELIGIBLE")
  }
  if (aggregate_position) {
    groups <- split(records, vapply(records, `[[`, integer(1), "player_id"))
    verified <- sum(vapply(groups, function(group) any(vapply(
      group, eligible_record, logical(1)
    )), logical(1)))
    unknown <- length(groups) - verified
    eligible <- verified
    total <- length(groups)
  } else {
    verified <- sum(vapply(records, verified_record, logical(1)))
    unknown <- sum(vapply(records, function(record) {
      identical(record$eligibility, "UNKNOWN")
    }, logical(1)))
    eligible <- sum(vapply(records, eligible_record, logical(1)))
    total <- length(records)
  }
  coverage <- if (require_all) {
    if (verified == total && unknown == 0L) "PASS" else "REVIEW"
  } else if (eligible > 0L) {
    "PASS"
  } else if (verified == total && total > 0L && unknown == 0L) {
    "FAIL"
  } else {
    "REVIEW"
  }
  data.frame(
    evidence_type = evidence_type,
    verified_count = as.integer(verified),
    unknown_count = as.integer(unknown),
    conflicting_count = 0L,
    malformed_count = 0L,
    coverage_status = coverage,
    stringsAsFactors = FALSE
  )
}


summarize_v2_evidence_completeness <- function(role_ledger, availability_ledger) {
  availability <- summarize_v2_availability_completeness(availability_ledger)
  availability_row <- data.frame(
    evidence_type = "AVAILABILITY",
    verified_count = availability$verified_count,
    unknown_count = availability$unknown_count,
    conflicting_count = availability$conflicting_count,
    malformed_count = availability$malformed_count,
    coverage_status = availability$coverage_status,
    stringsAsFactors = FALSE
  )
  role_types <- c(
    "POSITION_ELIGIBILITY", "BACKUP_PG", "BACKUP_C", "PRIMARY_CREATOR",
    "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR"
  )
  role_rows <- lapply(role_types, function(role) {
    records <- if (role == "POSITION_ELIGIBILITY") {
      Filter(function(record) grepl("^POSITION_", record$role), role_ledger$records)
    } else {
      Filter(function(record) identical(record$role, role), role_ledger$records)
    }
    v2_evidence_diagnostic_row(
      role, records,
      aggregate_position = identical(role, "POSITION_ELIGIBILITY")
    )
  })
  do.call(rbind, c(list(availability_row), role_rows))
}
