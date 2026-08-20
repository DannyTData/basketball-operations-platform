# ============================================================
# TBI V2 shared contract primitives
# ============================================================

v2_rotation_metadata <- function() {
  list(
    model_name = "tbi-rotation",
    model_version = "2.0.0-phase1"
  )
}


v2_validation_statuses <- function() {
  c("PASS", "REVIEW", "FAIL")
}


v2_validation_status <- function(status) {
  value <- toupper(trimws(as.character(status %||% "")))

  if (length(value) != 1L || is.na(value) ||
      !value %in% v2_validation_statuses()) {
    stop("status must be PASS, REVIEW, or FAIL.", call. = FALSE)
  }

  value
}


new_v2_explanation <- function(reason_codes = character(),
                               message = "",
                               evidence_fields = character(),
                               missing_fields = character()) {
  list(
    reason_codes = unique(as.character(reason_codes)),
    message = as.character(message %||% "")[[1]],
    evidence_fields = unique(as.character(evidence_fields)),
    missing_fields = unique(as.character(missing_fields))
  )
}


new_v2_validation_finding <- function(code,
                                      status,
                                      message,
                                      is_blocking = FALSE,
                                      path = NULL,
                                      evidence_fields = character(),
                                      missing_fields = character()) {
  code <- trimws(as.character(code %||% ""))
  message <- trimws(as.character(message %||% ""))

  if (length(code) != 1L || is.na(code) || !nzchar(code)) {
    stop("code must be a non-empty scalar string.", call. = FALSE)
  }
  if (length(message) != 1L || is.na(message) || !nzchar(message)) {
    stop("message must be a non-empty scalar string.", call. = FALSE)
  }

  status <- v2_validation_status(status)
  is_blocking <- isTRUE(is_blocking)

  if (is_blocking && status != "FAIL") {
    stop("A blocking finding must use status FAIL.", call. = FALSE)
  }

  list(
    code = code,
    status = status,
    is_blocking = is_blocking,
    path = if (is.null(path)) NULL else as.character(path)[[1]],
    explanation = new_v2_explanation(
      reason_codes = code,
      message = message,
      evidence_fields = evidence_fields,
      missing_fields = missing_fields
    )
  )
}


aggregate_v2_validation <- function(findings = list()) {
  if (is.null(findings)) {
    findings <- list()
  }
  if (!is.list(findings)) {
    stop("findings must be a list.", call. = FALSE)
  }
  if (!length(findings)) {
    return(list(status = "PASS", is_blocked = FALSE, findings = list()))
  }

  statuses <- vapply(
    findings,
    function(finding) {
      if (!is.list(finding) || is.null(finding$status)) {
        stop("Each finding must be a V2 validation finding.", call. = FALSE)
      }
      v2_validation_status(finding$status)
    },
    character(1)
  )
  severity <- match(statuses, v2_validation_statuses())
  blocked <- any(vapply(findings, function(x) isTRUE(x$is_blocking), logical(1)))

  list(
    status = v2_validation_statuses()[[max(severity)]],
    is_blocked = blocked,
    findings = findings
  )
}


v2_rotation_policy <- function() {
  list(
    policy_type = "tbi-v2-rotation-policy",
    policy_version = "1.0.0",
    canonical_statuses = v2_validation_statuses(),
    rotation_sizes = c(10L, 11L),
    starter_positions = c("PG", "SG", "SF", "PF", "C"),
    role_types = c("BACKUP_PG", "BACKUP_C"),
    preseason_rookie_gate_active = TRUE,
    unknown_role_coverage_requires_review = TRUE,
    approved_lock_conflicts_are_blocking = TRUE,
    infer_roles_from_generic_position = FALSE,
    infer_roles_from_player_name = FALSE,
    infer_roles_from_bie = FALSE
  )
}


validate_v2_rotation_policy <- function(policy) {
  required <- c(
    "canonical_statuses", "rotation_sizes", "starter_positions", "role_types",
    "preseason_rookie_gate_active",
    "unknown_role_coverage_requires_review",
    "approved_lock_conflicts_are_blocking"
  )
  if (!is.list(policy) || !all(required %in% names(policy))) {
    stop("policy is missing required V2 rotation fields.", call. = FALSE)
  }
  if (!identical(policy$canonical_statuses, v2_validation_statuses()) ||
      !identical(as.integer(policy$rotation_sizes), c(10L, 11L)) ||
      !identical(as.character(policy$starter_positions), c("PG", "SG", "SF", "PF", "C")) ||
      !identical(as.character(policy$role_types), c("BACKUP_PG", "BACKUP_C"))) {
    stop("policy changes a fixed Phase 1 contract vocabulary.", call. = FALSE)
  }
  if (!isTRUE(policy$unknown_role_coverage_requires_review) ||
      !isTRUE(policy$approved_lock_conflicts_are_blocking) ||
      !is.logical(policy$preseason_rookie_gate_active) ||
      length(policy$preseason_rookie_gate_active) != 1L ||
      is.na(policy$preseason_rookie_gate_active)) {
    stop("policy weakens required V2 safety behavior.", call. = FALSE)
  }
  policy
}


v2_canonical_text <- function(x) {
  encode <- function(value) {
    value <- enc2utf8(as.character(value))
    paste0(nchar(value, type = "bytes"), ":", value)
  }

  if (is.null(x)) {
    return("NULL")
  }
  if (is.data.frame(x)) {
    columns <- sort(names(x), method = "radix")
    rows <- if (nrow(x)) {
      vapply(
        seq_len(nrow(x)),
        function(i) v2_canonical_text(as.list(x[i, columns, drop = FALSE])),
        character(1)
      )
    } else {
      character()
    }
    return(paste0("DF[", paste(encode(columns), collapse = ","), "]{", paste(rows, collapse = ";"), "}"))
  }
  if (is.list(x)) {
    item_names <- names(x)
    if (!is.null(item_names) && all(nzchar(item_names))) {
      order_index <- order(item_names, method = "radix")
      return(paste0(
        "L{",
        paste(
          paste0(encode(item_names[order_index]), "=", vapply(x[order_index], v2_canonical_text, character(1))),
          collapse = ";"
        ),
        "}"
      ))
    }
    return(paste0("L[", paste(vapply(x, v2_canonical_text, character(1)), collapse = ";"), "]"))
  }

  type <- typeof(x)
  values <- vapply(
    seq_along(x),
    function(i) {
      if (is.na(x[[i]])) "<NA>" else encode(x[[i]])
    },
    character(1)
  )
  paste0(type, "[", paste(values, collapse = ","), "]")
}


v2_text_hash <- function(text) {
  bytes <- utf8ToInt(enc2utf8(text))
  modulus <- 2147483629
  hash_a <- 17
  hash_b <- 29

  for (byte in bytes) {
    hash_a <- (hash_a * 131 + byte + 1) %% modulus
    hash_b <- (hash_b * 137 + byte + 7) %% modulus
  }

  paste0(sprintf("%010.0f", hash_a), sprintf("%010.0f", hash_b))
}


v2_input_signature <- function(x) {
  paste0("v2sig1-", v2_text_hash(v2_canonical_text(x)))
}


v2_state_id <- function(prefix, input) {
  prefix <- tolower(trimws(as.character(prefix %||% "")))
  if (length(prefix) != 1L || is.na(prefix) ||
      !grepl("^[a-z][a-z0-9-]*$", prefix)) {
    stop("prefix must be a lowercase identifier.", call. = FALSE)
  }
  paste0(prefix, "-", sub("^v2sig1-", "", v2_input_signature(input)))
}


v2_iso_date <- function(value, field, required = FALSE) {
  text <- trimws(as.character(value %||% ""))
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    if (required) stop(field, " must be an ISO date.", call. = FALSE)
    return(NULL)
  }
  parsed <- suppressWarnings(as.Date(text, format = "%Y-%m-%d"))
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != text) {
    stop(field, " must be an ISO date.", call. = FALSE)
  }
  text
}


new_v2_role_eligibility <- function(player_id,
                                    role,
                                    eligibility = "UNKNOWN",
                                    evidence_status = "UNKNOWN",
                                    evidence_source = NULL,
                                    evidence_fields = character(),
                                    missing_fields = character(),
                                    team_id = NULL,
                                    season = NULL,
                                    evidence_class = NULL,
                                    source_field = NULL,
                                    source_version = NULL,
                                    verification_status = NULL,
                                    reason_codes = character(),
                                    explanation = NULL,
                                    verified_by = NULL,
                                    effective_date = NULL,
                                    expiration_date = NULL,
                                    evidence_version = "1.0.0") {
  role <- toupper(trimws(as.character(role %||% "")))
  eligibility <- toupper(trimws(as.character(eligibility %||% "UNKNOWN")))
  evidence_status <- toupper(trimws(as.character(evidence_status %||% "UNKNOWN")))
  normalized_player_id <- suppressWarnings(as.integer(player_id))
  role_policy <- v2_role_policy()

  if (length(normalized_player_id) != 1L || is.na(normalized_player_id)) {
    stop("player_id must be one known integer ID.", call. = FALSE)
  }

  if (!role %in% role_policy$supported_roles) {
    stop("role is outside the Phase 1D role vocabulary.", call. = FALSE)
  }
  if (!eligibility %in% role_policy$eligibility) {
    stop("eligibility must be ELIGIBLE, NOT_ELIGIBLE, or UNKNOWN.", call. = FALSE)
  }
  if (!evidence_status %in% c("VERIFIED", "UNKNOWN")) {
    stop("evidence_status must be VERIFIED or UNKNOWN.", call. = FALSE)
  }
  if (eligibility != "UNKNOWN" && evidence_status != "VERIFIED") {
    stop("Known role eligibility requires verified evidence.", call. = FALSE)
  }
  if (evidence_status == "VERIFIED" &&
      (is.null(evidence_source) || !nzchar(trimws(as.character(evidence_source)[[1]])) ||
       !length(evidence_fields))) {
    stop(
      "Verified role eligibility requires an evidence source and evidence fields.",
      call. = FALSE
    )
  }
  if (eligibility == "UNKNOWN") {
    evidence_status <- "UNKNOWN"
  }

  verification_status <- toupper(trimws(as.character(
    verification_status %||% if (evidence_status == "VERIFIED") "VERIFIED" else "MISSING"
  )))
  if (length(verification_status) != 1L || is.na(verification_status) ||
      !verification_status %in% role_policy$verification_statuses) {
    stop(
      "verification_status must be VERIFIED, DERIVED_VERIFIED, UNVERIFIED, or MISSING.",
      call. = FALSE
    )
  }
  if (eligibility != "UNKNOWN" &&
      !verification_status %in% c("VERIFIED", "DERIVED_VERIFIED")) {
    stop("Known role eligibility requires verified evidence.", call. = FALSE)
  }
  if (eligibility == "UNKNOWN" &&
      verification_status %in% c("VERIFIED", "DERIVED_VERIFIED")) {
    stop("Unknown role eligibility cannot claim verified evidence.", call. = FALSE)
  }

  evidence_class <- toupper(trimws(as.character(evidence_class %||%
    if (evidence_status == "VERIFIED") "AUTHORITATIVE_FACT" else "UNKNOWN")))
  if (length(evidence_class) != 1L || is.na(evidence_class) ||
      !evidence_class %in% role_policy$evidence_classes) {
    stop("evidence_class is outside the Phase 1D vocabulary.", call. = FALSE)
  }
  if (eligibility != "UNKNOWN" && evidence_class %in% c("MODEL_EVIDENCE", "UNKNOWN")) {
    stop("Model or unknown evidence cannot establish role eligibility.", call. = FALSE)
  }

  phase1e_role <- role %in% c(
    "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR"
  )
  evidence_version <- v2_role_scalar_text(evidence_version, "evidence_version")
  effective_date <- v2_iso_date(
    effective_date, "effective_date", required = phase1e_role && eligibility != "UNKNOWN"
  )
  expiration_date <- v2_iso_date(expiration_date, "expiration_date")
  if (!is.null(effective_date) && !is.null(expiration_date) &&
      expiration_date < effective_date) {
    stop("role evidence effective window is invalid.", call. = FALSE)
  }
  if (phase1e_role && eligibility != "UNKNOWN") {
    v2_role_scalar_text(source_version, "source_version")
    v2_role_scalar_text(verified_by, "verified_by")
  }

  validation <- if (eligibility == "UNKNOWN") {
    aggregate_v2_validation(list(new_v2_validation_finding(
      code = "ROLE_ELIGIBILITY_UNKNOWN",
      status = "REVIEW",
      message = paste(role, "eligibility evidence is unknown."),
      missing_fields = missing_fields
    )))
  } else {
    aggregate_v2_validation()
  }

  contract <- list(
    contract_type = "tbi-v2-role-eligibility",
    contract_version = "1.0.0",
    player_id = normalized_player_id[[1]],
    team_id = team_id,
    season = season,
    role = role,
    eligibility = eligibility,
    evidence_class = evidence_class,
    source = if (is.null(evidence_source)) NULL else as.character(evidence_source)[[1]],
    source_field = if (is.null(source_field)) NULL else as.character(source_field)[[1]],
    source_version = if (is.null(source_version)) NULL else as.character(source_version)[[1]],
    evidence_version = evidence_version,
    verified_by = if (is.null(verified_by)) NULL else as.character(verified_by)[[1]],
    effective_date = effective_date,
    expiration_date = expiration_date,
    verification_status = verification_status,
    evidence_status = evidence_status,
    evidence_source = if (is.null(evidence_source)) NULL else as.character(evidence_source)[[1]],
    evidence_fields = unique(as.character(evidence_fields)),
    missing_fields = unique(as.character(missing_fields)),
    reason_codes = unique(as.character(reason_codes)),
    explanation = as.character(explanation %||% if (eligibility == "UNKNOWN") {
      paste(role, "eligibility remains unknown because approved evidence is missing.")
    } else {
      paste(role, "eligibility is supported by approved evidence.")
    })[[1]],
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation
  )
  contract$input_signature <- v2_input_signature(contract[setdiff(
    names(contract), c("input_signature", "status", "is_blocked", "validation")
  )])
  contract
}
