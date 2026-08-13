# ============================================================
# Thompson's Basketball Intelligence
# Phase 5A: Draft Assets Intelligence Engine
# ============================================================

# This engine manages persistent draft-pick ownership, protections,
# swaps, conveyance conditions, outgoing obligations, verification
# metadata, and portfolio summaries.
#
# It is deliberately independent of Shiny. The Draft Intelligence
# module should call these functions rather than maintain session-only
# reactive data.
#
# IMPORTANT:
# Draft-pick language can be complex. This engine stores and classifies
# verified terms but does not silently infer missing legal language.
# Any asset with incomplete or unverified terms is flagged for review.

# ------------------------------------------------------------
# Safe helpers
# ------------------------------------------------------------

#' Null-coalescing helper
#' @noRd
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}


#' Convert a value to a safe character scalar
#' @noRd
draft_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) default else value
}


#' Convert a value to a safe integer scalar
#' @noRd
draft_integer <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(round(as.numeric(x))))
  
  if (!length(value) || is.na(value[[1]])) {
    return(default)
  }
  
  value[[1]]
}


#' Convert a value to a safe numeric scalar
#' @noRd
draft_number <- function(x, default = NA_real_) {
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


#' Convert a value to a logical scalar
#' @noRd
draft_flag <- function(x, default = FALSE) {
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


#' Return a normalized current timestamp
#' @noRd
draft_timestamp <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
}


# ------------------------------------------------------------
# Controlled vocabularies
# ------------------------------------------------------------

#' Supported draft-asset control types
#' @noRd
draft_control_types <- function() {
  c(
    "Own",
    "Incoming",
    "Outgoing",
    "Swap Right",
    "Swap Obligation"
  )
}


#' Supported draft-pick rounds
#' @noRd
draft_rounds <- function() {
  c("First", "Second")
}


#' Supported strategic-value labels
#' @noRd
draft_value_tiers <- function() {
  c("High", "Medium", "Low", "Unrated")
}


#' Supported asset verification statuses
#' @noRd
draft_verification_statuses <- function() {
  c(
    "Verified",
    "Needs Review",
    "Unverified",
    "Superseded"
  )
}


#' Supported protection classifications
#' @noRd
draft_protection_types <- function() {
  c(
    "Unprotected",
    "Lottery Protected",
    "Top-N Protected",
    "Range Protected",
    "Best Of",
    "Worst Of",
    "Conditional",
    "None",
    "Unspecified"
  )
}


#' Normalize a control type
#' @noRd
normalize_draft_control_type <- function(x) {
  value <- tolower(
    gsub(
      "[-_]+",
      " ",
      draft_text(x)
    )
  )
  
  aliases <- c(
    "own" = "Own",
    "owned" = "Own",
    "incoming" = "Incoming",
    "acquired" = "Incoming",
    "outgoing" = "Outgoing",
    "owed" = "Outgoing",
    "swap right" = "Swap Right",
    "swap rights" = "Swap Right",
    "swap obligation" = "Swap Obligation",
    "swap obligations" = "Swap Obligation"
  )
  
  if (!value %in% names(aliases)) {
    stop(
      paste0(
        "Unsupported control_type: ",
        draft_text(x, "<blank>"),
        "."
      ),
      call. = FALSE
    )
  }
  
  unname(aliases[[value]])
}


#' Normalize a draft round
#' @noRd
normalize_draft_round <- function(x) {
  value <- tolower(draft_text(x))
  
  if (value %in% c("1", "1st", "first", "first round")) {
    return("First")
  }
  
  if (value %in% c("2", "2nd", "second", "second round")) {
    return("Second")
  }
  
  stop(
    paste0(
      "Unsupported round: ",
      draft_text(x, "<blank>"),
      "."
    ),
    call. = FALSE
  )
}


#' Normalize a strategic-value tier
#' @noRd
normalize_draft_value_tier <- function(x) {
  value <- tools::toTitleCase(
    tolower(
      draft_text(x, "Unrated")
    )
  )
  
  if (!value %in% draft_value_tiers()) {
    stop(
      paste0(
        "Unsupported strategic_value: ",
        value,
        "."
      ),
      call. = FALSE
    )
  }
  
  value
}


#' Normalize a verification status
#' @noRd
normalize_draft_verification_status <- function(x) {
  value <- tools::toTitleCase(
    tolower(
      draft_text(x, "Unverified")
    )
  )
  
  if (!value %in% draft_verification_statuses()) {
    stop(
      paste0(
        "Unsupported verification_status: ",
        value,
        "."
      ),
      call. = FALSE
    )
  }
  
  value
}


# ------------------------------------------------------------
# Protection classification
# ------------------------------------------------------------

#' Classify human-readable protection language
#'
#' This classifier is intentionally conservative. The original language is
#' always preserved and should remain the source of truth.
#'
#' @noRd
classify_draft_protection <- function(protection_text) {
  raw <- draft_text(protection_text, "Unspecified")
  value <- tolower(raw)
  
  protection_type <- "Conditional"
  protected_through_pick <- NA_integer_
  protected_from_pick <- NA_integer_
  requires_manual_review <- FALSE
  
  if (
    value %in% c(
      "unprotected",
      "no protection",
      "none"
    )
  ) {
    protection_type <- "Unprotected"
  } else if (
    value %in% c(
      "unspecified",
      "unknown",
      "tbd"
    )
  ) {
    protection_type <- "Unspecified"
    requires_manual_review <- TRUE
  } else if (grepl("lottery", value)) {
    protection_type <- "Lottery Protected"
    protected_through_pick <- 14L
  } else if (grepl("best of", value, fixed = TRUE)) {
    protection_type <- "Best Of"
    requires_manual_review <- TRUE
  } else if (grepl("worst of", value, fixed = TRUE)) {
    protection_type <- "Worst Of"
    requires_manual_review <- TRUE
  } else {
    top_match <- regexec(
      "top[ -]?([0-9]{1,2})",
      value
    )
    top_parts <- regmatches(value, top_match)[[1]]
    
    range_match <- regexec(
      "([0-9]{1,2})[ -]?(through|to|-)[ -]?([0-9]{1,2})",
      value
    )
    range_parts <- regmatches(value, range_match)[[1]]
    
    if (length(top_parts) >= 2L) {
      protection_type <- "Top-N Protected"
      protected_through_pick <- as.integer(top_parts[[2]])
    } else if (length(range_parts) >= 4L) {
      protection_type <- "Range Protected"
      protected_from_pick <- as.integer(range_parts[[2]])
      protected_through_pick <- as.integer(range_parts[[4]])
      requires_manual_review <- TRUE
    } else {
      protection_type <- "Conditional"
      requires_manual_review <- TRUE
    }
  }
  
  list(
    protection_text = raw,
    protection_type = protection_type,
    protected_from_pick = protected_from_pick,
    protected_through_pick = protected_through_pick,
    requires_manual_review = requires_manual_review
  )
}


# ------------------------------------------------------------
# Database schema
# ------------------------------------------------------------

#' Create persistent draft-asset support tables
#' @noRd
ensure_draft_asset_tables <- function(con) {
  if (is.null(con) || !DBI::dbIsValid(con)) {
    stop(
      "A valid database connection is required.",
      call. = FALSE
    )
  }
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS draft_assets (
      draft_asset_id INTEGER PRIMARY KEY AUTOINCREMENT,
      draft_year INTEGER NOT NULL,
      round TEXT NOT NULL
        CHECK (round IN ('First', 'Second')),
      original_team_id INTEGER NOT NULL,
      current_team_id INTEGER NOT NULL,
      control_type TEXT NOT NULL
        CHECK (
          control_type IN (
            'Own',
            'Incoming',
            'Outgoing',
            'Swap Right',
            'Swap Obligation'
          )
        ),
      counterparty_team_id INTEGER,
      protection_text TEXT NOT NULL DEFAULT 'Unspecified',
      protection_type TEXT NOT NULL DEFAULT 'Unspecified',
      protected_from_pick INTEGER,
      protected_through_pick INTEGER,
      conveyance_start_year INTEGER,
      conveyance_end_year INTEGER,
      converts_to_round TEXT,
      swap_priority TEXT,
      strategic_value TEXT NOT NULL DEFAULT 'Unrated'
        CHECK (
          strategic_value IN (
            'High',
            'Medium',
            'Low',
            'Unrated'
          )
        ),
      internal_value REAL,
      transaction_reference TEXT,
      source_name TEXT,
      source_url TEXT,
      source_date TEXT,
      verification_status TEXT NOT NULL DEFAULT 'Unverified'
        CHECK (
          verification_status IN (
            'Verified',
            'Needs Review',
            'Unverified',
            'Superseded'
          )
        ),
      notes TEXT,
      is_active INTEGER NOT NULL DEFAULT 1
        CHECK (is_active IN (0, 1)),
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(original_team_id) REFERENCES teams(team_id),
      FOREIGN KEY(current_team_id) REFERENCES teams(team_id),
      FOREIGN KEY(counterparty_team_id) REFERENCES teams(team_id),
      CHECK (draft_year BETWEEN 2000 AND 2100),
      CHECK (
        protected_from_pick IS NULL OR
        protected_from_pick BETWEEN 1 AND 60
      ),
      CHECK (
        protected_through_pick IS NULL OR
        protected_through_pick BETWEEN 1 AND 60
      ),
      CHECK (
        internal_value IS NULL OR
        internal_value >= 0
      )
    );
    "
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS draft_asset_conditions (
      draft_condition_id INTEGER PRIMARY KEY AUTOINCREMENT,
      draft_asset_id INTEGER NOT NULL,
      condition_order INTEGER NOT NULL DEFAULT 1,
      condition_year INTEGER,
      condition_text TEXT NOT NULL,
      outcome_if_conveys TEXT,
      outcome_if_not_conveyed TEXT,
      converts_to_year INTEGER,
      converts_to_round TEXT,
      is_final_condition INTEGER NOT NULL DEFAULT 0
        CHECK (is_final_condition IN (0, 1)),
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(draft_asset_id)
        REFERENCES draft_assets(draft_asset_id)
        ON DELETE CASCADE,
      UNIQUE(draft_asset_id, condition_order)
    );
    "
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS draft_asset_audit (
      draft_audit_id INTEGER PRIMARY KEY AUTOINCREMENT,
      draft_asset_id INTEGER,
      action TEXT NOT NULL,
      changed_by TEXT,
      change_summary TEXT,
      changed_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(draft_asset_id)
        REFERENCES draft_assets(draft_asset_id)
        ON DELETE SET NULL
    );
    "
  )
  
  indexes <- c(
    "
    CREATE INDEX IF NOT EXISTS idx_draft_assets_current_team_year
    ON draft_assets(current_team_id, draft_year);
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_draft_assets_original_team_year
    ON draft_assets(original_team_id, draft_year);
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_draft_assets_control_type
    ON draft_assets(control_type);
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_draft_assets_active
    ON draft_assets(is_active);
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_draft_conditions_asset
    ON draft_asset_conditions(draft_asset_id, condition_order);
    "
  )
  
  invisible(
    lapply(
      indexes,
      function(sql) DBI::dbExecute(con, sql)
    )
  )
  
  invisible(TRUE)
}


#' Ensure draft tables using the packaged TBI database
#' @noRd
initialize_draft_assets_database <- function(db_path = NULL) {
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Team resolution
# ------------------------------------------------------------

#' Resolve one team identifier
#' @noRd
resolve_draft_team <- function(con, team_value) {
  value <- draft_text(team_value)
  
  if (!nzchar(value)) {
    stop("A team identifier is required.", call. = FALSE)
  }
  
  result <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name,
      abbreviation
    FROM teams
    WHERE
      team_name = ?
      OR abbreviation = ?
      OR CAST(team_id AS TEXT) = CAST(? AS TEXT)
    LIMIT 1;
    ",
    params = list(value, value, value)
  )
  
  if (!nrow(result)) {
    stop(
      paste0(
        "Team not found: ",
        value,
        "."
      ),
      call. = FALSE
    )
  }
  
  result[1, , drop = FALSE]
}


#' Resolve optional counterparty team
#' @noRd
resolve_optional_draft_team_id <- function(con, team_value) {
  value <- draft_text(team_value)
  
  if (!nzchar(value) || value %in% c("—", "-", "None")) {
    return(NA_integer_)
  }
  
  as.integer(
    resolve_draft_team(
      con,
      value
    )$team_id[[1]]
  )
}


# ------------------------------------------------------------
# Asset validation
# ------------------------------------------------------------

#' Validate and normalize a draft asset input
#' @noRd
validate_draft_asset <- function(asset) {
  if (!is.list(asset)) {
    stop("asset must be a named list.", call. = FALSE)
  }
  
  required <- c(
    "draft_year",
    "round",
    "original_team",
    "current_team",
    "control_type"
  )
  
  missing_fields <- setdiff(
    required,
    names(asset)
  )
  
  if (length(missing_fields)) {
    stop(
      paste0(
        "asset is missing required field(s): ",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  draft_year <- draft_integer(asset$draft_year)
  
  if (
    is.na(draft_year) ||
    draft_year < 2000L ||
    draft_year > 2100L
  ) {
    stop(
      "draft_year must be between 2000 and 2100.",
      call. = FALSE
    )
  }
  
  round <- normalize_draft_round(asset$round)
  control_type <- normalize_draft_control_type(
    asset$control_type
  )
  
  protection <- classify_draft_protection(
    asset$protection_text %||%
      asset$protection %||%
      "Unspecified"
  )
  
  strategic_value <- normalize_draft_value_tier(
    asset$strategic_value %||% "Unrated"
  )
  
  verification_status <-
    normalize_draft_verification_status(
      asset$verification_status %||%
        if (protection$requires_manual_review) {
          "Needs Review"
        } else {
          "Unverified"
        }
    )
  
  internal_value <- draft_number(
    asset$internal_value,
    NA_real_
  )
  
  if (!is.na(internal_value) && internal_value < 0) {
    stop(
      "internal_value cannot be negative.",
      call. = FALSE
    )
  }
  
  conveyance_start_year <- draft_integer(
    asset$conveyance_start_year,
    NA_integer_
  )
  
  conveyance_end_year <- draft_integer(
    asset$conveyance_end_year,
    NA_integer_
  )
  
  if (
    !is.na(conveyance_start_year) &&
    !is.na(conveyance_end_year) &&
    conveyance_end_year < conveyance_start_year
  ) {
    stop(
      "conveyance_end_year cannot precede conveyance_start_year.",
      call. = FALSE
    )
  }
  
  converts_to_round <- draft_text(
    asset$converts_to_round
  )
  
  if (nzchar(converts_to_round)) {
    converts_to_round <- normalize_draft_round(
      converts_to_round
    )
  } else {
    converts_to_round <- NA_character_
  }
  
  list(
    draft_year = draft_year,
    round = round,
    original_team = draft_text(
      asset$original_team
    ),
    current_team = draft_text(
      asset$current_team
    ),
    control_type = control_type,
    counterparty = draft_text(
      asset$counterparty
    ),
    protection_text =
      protection$protection_text,
    protection_type =
      protection$protection_type,
    protected_from_pick =
      protection$protected_from_pick,
    protected_through_pick =
      protection$protected_through_pick,
    conveyance_start_year =
      conveyance_start_year,
    conveyance_end_year =
      conveyance_end_year,
    converts_to_round =
      converts_to_round,
    swap_priority = draft_text(
      asset$swap_priority
    ),
    strategic_value =
      strategic_value,
    internal_value =
      internal_value,
    transaction_reference = draft_text(
      asset$transaction_reference
    ),
    source_name = draft_text(
      asset$source_name
    ),
    source_url = draft_text(
      asset$source_url
    ),
    source_date = draft_text(
      asset$source_date
    ),
    verification_status =
      verification_status,
    notes = draft_text(
      asset$notes
    ),
    is_active = as.integer(
      draft_flag(
        asset$is_active,
        TRUE
      )
    ),
    requires_manual_review =
      protection$requires_manual_review ||
      verification_status != "Verified"
  )
}


# ------------------------------------------------------------
# CRUD operations
# ------------------------------------------------------------

#' Insert a persistent draft asset
#' @noRd
save_draft_asset <- function(asset,
                             db_path = NULL,
                             changed_by = "TBI User") {
  normalized <- validate_draft_asset(asset)
  
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  original_team <- resolve_draft_team(
    con,
    normalized$original_team
  )
  
  current_team <- resolve_draft_team(
    con,
    normalized$current_team
  )
  
  counterparty_team_id <-
    resolve_optional_draft_team_id(
      con,
      normalized$counterparty
    )
  
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbExecute(
        con,
        "
        INSERT INTO draft_assets (
          draft_year,
          round,
          original_team_id,
          current_team_id,
          control_type,
          counterparty_team_id,
          protection_text,
          protection_type,
          protected_from_pick,
          protected_through_pick,
          conveyance_start_year,
          conveyance_end_year,
          converts_to_round,
          swap_priority,
          strategic_value,
          internal_value,
          transaction_reference,
          source_name,
          source_url,
          source_date,
          verification_status,
          notes,
          is_active,
          created_at,
          updated_at
        )
        VALUES (
          ?, ?, ?, ?, ?, ?,
          ?, ?, ?, ?, ?, ?,
          ?, ?, ?, ?, ?, ?,
          ?, ?, ?, ?, ?,
          CURRENT_TIMESTAMP,
          CURRENT_TIMESTAMP
        );
        ",
        params = list(
          normalized$draft_year,
          normalized$round,
          as.integer(original_team$team_id[[1]]),
          as.integer(current_team$team_id[[1]]),
          normalized$control_type,
          counterparty_team_id,
          normalized$protection_text,
          normalized$protection_type,
          normalized$protected_from_pick,
          normalized$protected_through_pick,
          normalized$conveyance_start_year,
          normalized$conveyance_end_year,
          normalized$converts_to_round,
          normalized$swap_priority,
          normalized$strategic_value,
          normalized$internal_value,
          normalized$transaction_reference,
          normalized$source_name,
          normalized$source_url,
          normalized$source_date,
          normalized$verification_status,
          normalized$notes,
          normalized$is_active
        )
      )
      
      draft_asset_id <- DBI::dbGetQuery(
        con,
        "SELECT last_insert_rowid() AS draft_asset_id;"
      )$draft_asset_id[[1]]
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO draft_asset_audit (
          draft_asset_id,
          action,
          changed_by,
          change_summary
        )
        VALUES (?, 'CREATE', ?, ?);
        ",
        params = list(
          draft_asset_id,
          draft_text(changed_by, "TBI User"),
          paste0(
            normalized$draft_year,
            " ",
            normalized$round,
            " - ",
            normalized$control_type
          )
        )
      )
      
      as.integer(draft_asset_id)
    }
  )
}


#' Update a persistent draft asset
#' @noRd
update_draft_asset <- function(draft_asset_id,
                               asset,
                               db_path = NULL,
                               changed_by = "TBI User") {
  draft_asset_id <- draft_integer(
    draft_asset_id
  )
  
  if (is.na(draft_asset_id) || draft_asset_id < 1L) {
    stop(
      "A valid draft_asset_id is required.",
      call. = FALSE
    )
  }
  
  normalized <- validate_draft_asset(asset)
  
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  existing <- DBI::dbGetQuery(
    con,
    "
    SELECT draft_asset_id
    FROM draft_assets
    WHERE draft_asset_id = ?;
    ",
    params = list(draft_asset_id)
  )
  
  if (!nrow(existing)) {
    stop(
      paste0(
        "Draft asset not found: ",
        draft_asset_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  original_team <- resolve_draft_team(
    con,
    normalized$original_team
  )
  
  current_team <- resolve_draft_team(
    con,
    normalized$current_team
  )
  
  counterparty_team_id <-
    resolve_optional_draft_team_id(
      con,
      normalized$counterparty
    )
  
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbExecute(
        con,
        "
        UPDATE draft_assets
        SET
          draft_year = ?,
          round = ?,
          original_team_id = ?,
          current_team_id = ?,
          control_type = ?,
          counterparty_team_id = ?,
          protection_text = ?,
          protection_type = ?,
          protected_from_pick = ?,
          protected_through_pick = ?,
          conveyance_start_year = ?,
          conveyance_end_year = ?,
          converts_to_round = ?,
          swap_priority = ?,
          strategic_value = ?,
          internal_value = ?,
          transaction_reference = ?,
          source_name = ?,
          source_url = ?,
          source_date = ?,
          verification_status = ?,
          notes = ?,
          is_active = ?,
          updated_at = CURRENT_TIMESTAMP
        WHERE draft_asset_id = ?;
        ",
        params = list(
          normalized$draft_year,
          normalized$round,
          as.integer(original_team$team_id[[1]]),
          as.integer(current_team$team_id[[1]]),
          normalized$control_type,
          counterparty_team_id,
          normalized$protection_text,
          normalized$protection_type,
          normalized$protected_from_pick,
          normalized$protected_through_pick,
          normalized$conveyance_start_year,
          normalized$conveyance_end_year,
          normalized$converts_to_round,
          normalized$swap_priority,
          normalized$strategic_value,
          normalized$internal_value,
          normalized$transaction_reference,
          normalized$source_name,
          normalized$source_url,
          normalized$source_date,
          normalized$verification_status,
          normalized$notes,
          normalized$is_active,
          draft_asset_id
        )
      )
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO draft_asset_audit (
          draft_asset_id,
          action,
          changed_by,
          change_summary
        )
        VALUES (?, 'UPDATE', ?, ?);
        ",
        params = list(
          draft_asset_id,
          draft_text(changed_by, "TBI User"),
          "Draft asset record updated."
        )
      )
    }
  )
  
  invisible(TRUE)
}


#' Soft-delete a draft asset
#' @noRd
archive_draft_asset <- function(draft_asset_id,
                                db_path = NULL,
                                changed_by = "TBI User") {
  draft_asset_id <- draft_integer(
    draft_asset_id
  )
  
  if (is.na(draft_asset_id) || draft_asset_id < 1L) {
    stop(
      "A valid draft_asset_id is required.",
      call. = FALSE
    )
  }
  
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  affected <- DBI::dbExecute(
    con,
    "
    UPDATE draft_assets
    SET
      is_active = 0,
      updated_at = CURRENT_TIMESTAMP
    WHERE draft_asset_id = ?;
    ",
    params = list(draft_asset_id)
  )
  
  if (affected != 1L) {
    stop(
      paste0(
        "Draft asset not found: ",
        draft_asset_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO draft_asset_audit (
      draft_asset_id,
      action,
      changed_by,
      change_summary
    )
    VALUES (?, 'ARCHIVE', ?, 'Draft asset archived.');
    ",
    params = list(
      draft_asset_id,
      draft_text(changed_by, "TBI User")
    )
  )
  
  invisible(TRUE)
}


#' Add a conveyance or protection condition
#' @noRd
save_draft_asset_condition <- function(
    draft_asset_id,
    condition_text,
    condition_order = 1L,
    condition_year = NA_integer_,
    outcome_if_conveys = "",
    outcome_if_not_conveyed = "",
    converts_to_year = NA_integer_,
    converts_to_round = "",
    is_final_condition = FALSE,
    db_path = NULL) {
  
  draft_asset_id <- draft_integer(
    draft_asset_id
  )
  
  condition_order <- draft_integer(
    condition_order,
    1L
  )
  
  condition_text <- draft_text(
    condition_text
  )
  
  if (is.na(draft_asset_id) || draft_asset_id < 1L) {
    stop(
      "A valid draft_asset_id is required.",
      call. = FALSE
    )
  }
  
  if (!nzchar(condition_text)) {
    stop(
      "condition_text is required.",
      call. = FALSE
    )
  }
  
  normalized_conversion_round <-
    draft_text(converts_to_round)
  
  if (nzchar(normalized_conversion_round)) {
    normalized_conversion_round <-
      normalize_draft_round(
        normalized_conversion_round
      )
  } else {
    normalized_conversion_round <- NA_character_
  }
  
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  asset_exists <- DBI::dbGetQuery(
    con,
    "
    SELECT 1 AS found
    FROM draft_assets
    WHERE draft_asset_id = ?
    LIMIT 1;
    ",
    params = list(draft_asset_id)
  )
  
  if (!nrow(asset_exists)) {
    stop(
      paste0(
        "Draft asset not found: ",
        draft_asset_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO draft_asset_conditions (
      draft_asset_id,
      condition_order,
      condition_year,
      condition_text,
      outcome_if_conveys,
      outcome_if_not_conveyed,
      converts_to_year,
      converts_to_round,
      is_final_condition,
      created_at,
      updated_at
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
    ON CONFLICT(draft_asset_id, condition_order)
    DO UPDATE SET
      condition_year = excluded.condition_year,
      condition_text = excluded.condition_text,
      outcome_if_conveys = excluded.outcome_if_conveys,
      outcome_if_not_conveyed = excluded.outcome_if_not_conveyed,
      converts_to_year = excluded.converts_to_year,
      converts_to_round = excluded.converts_to_round,
      is_final_condition = excluded.is_final_condition,
      updated_at = CURRENT_TIMESTAMP;
    ",
    params = list(
      draft_asset_id,
      max(1L, condition_order),
      draft_integer(
        condition_year,
        NA_integer_
      ),
      condition_text,
      draft_text(outcome_if_conveys),
      draft_text(outcome_if_not_conveyed),
      draft_integer(
        converts_to_year,
        NA_integer_
      ),
      normalized_conversion_round,
      as.integer(
        draft_flag(
          is_final_condition,
          FALSE
        )
      )
    )
  )
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Retrieval
# ------------------------------------------------------------

#' Retrieve draft assets for one organization
#' @noRd
get_draft_assets <- function (team_value, year_from = NULL, year_to = NULL, include_inactive = FALSE, db_path = NULL) 
{
    con <- connect_db(db_path = db_path, read_only = FALSE)
    on.exit(disconnect_db(con), add = TRUE)
    ensure_draft_asset_tables(con)
    team <- resolve_draft_team(con, team_value)
    team_id <- as.integer(team$team_id[[1]])
    filters <- c("\n    (\n      da.current_team_id = ?\n      OR da.original_team_id = ?\n    )\n    ")
    params <- list(team_id, team_id)
    if (!isTRUE(include_inactive)) {
        filters <- c(filters, "da.is_active = 1")
    }
    if (!is.null(year_from)) {
        filters <- c(filters, "da.draft_year >= ?")
        params <- c(params, list(draft_integer(year_from)))
    }
    if (!is.null(year_to)) {
        filters <- c(filters, "da.draft_year <= ?")
        params <- c(params, list(draft_integer(year_to)))
    }
    sql <- paste0("\n    SELECT\n      da.draft_asset_id,\n      da.draft_year,\n      da.round,\n\n      da.original_team_id AS perspective_original_team_id,\n      da.current_team_id AS perspective_current_team_id,\n\n      da.control_type,\n\n      ot.team_name AS original_team,\n      ct.team_name AS current_team,\n      cp.team_name AS counterparty,\n\n      da.protection_text,\n      da.protection_type,\n      da.protected_from_pick,\n      da.protected_through_pick,\n\n      da.conveyance_start_year,\n      da.conveyance_end_year,\n      da.converts_to_round,\n\n      da.swap_priority,\n      da.strategic_value,\n      da.internal_value,\n\n      da.transaction_reference,\n      da.source_name,\n      da.source_url,\n      da.source_date,\n\n      da.verification_status,\n      da.notes,\n      da.is_active,\n\n      da.created_at,\n      da.updated_at,\n\n      COUNT(dac.draft_condition_id) AS condition_count\n\n    FROM draft_assets da\n\n    INNER JOIN teams ot\n      ON ot.team_id = da.original_team_id\n\n    INNER JOIN teams ct\n      ON ct.team_id = da.current_team_id\n\n    LEFT JOIN teams cp\n      ON cp.team_id = da.counterparty_team_id\n\n    LEFT JOIN draft_asset_conditions dac\n      ON dac.draft_asset_id = da.draft_asset_id\n\n    WHERE ", 
        paste(filters, collapse = " AND "), "\n\n    GROUP BY\n      da.draft_asset_id\n\n    ORDER BY\n      da.draft_year,\n\n      CASE da.round\n        WHEN 'First' THEN 1\n        ELSE 2\n      END,\n\n      da.control_type,\n      ot.team_name;\n    ")
    result <- DBI::dbGetQuery(con, sql, params = params)
    if (nrow(result)) {
        stored_control_type <- result$control_type
        original_id <- as.integer(result$perspective_original_team_id)
        current_id <- as.integer(result$perspective_current_team_id)
        result$control_type <- vapply(seq_len(nrow(result)), function(i) {
            stored <- stored_control_type[[i]]
            original <- original_id[[i]]
            current <- current_id[[i]]
            if (original == team_id && current == team_id) {
                return(stored)
            }
            if (current == team_id && original != team_id) {
                if (stored %in% c("Swap Right", "Swap Obligation")) {
                  return("Swap Right")
                }
                return("Incoming")
            }
            if (original == team_id && current != team_id) {
                if (stored %in% c("Swap Right", "Swap Obligation")) {
                  return("Swap Obligation")
                }
                return("Outgoing")
            }
            stored
        }, character(1))
        result$perspective_original_team_id <- NULL
        result$perspective_current_team_id <- NULL
        result$requires_manual_review <- result$verification_status != "Verified" | result$protection_type %in% c("Conditional", "Best Of", "Worst Of", "Range Protected", "Unspecified")
    }
    result
}


#' Retrieve one draft asset and its condition chain
#' @noRd
get_draft_asset_detail <- function(draft_asset_id,
                                   db_path = NULL) {
  draft_asset_id <- draft_integer(
    draft_asset_id
  )
  
  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_draft_asset_tables(con)
  
  asset <- DBI::dbGetQuery(
    con,
    "
    SELECT
      da.*,
      ot.team_name AS original_team,
      ct.team_name AS current_team,
      cp.team_name AS counterparty
    FROM draft_assets da
    INNER JOIN teams ot
      ON ot.team_id = da.original_team_id
    INNER JOIN teams ct
      ON ct.team_id = da.current_team_id
    LEFT JOIN teams cp
      ON cp.team_id = da.counterparty_team_id
    WHERE da.draft_asset_id = ?;
    ",
    params = list(draft_asset_id)
  )
  
  if (!nrow(asset)) {
    stop(
      paste0(
        "Draft asset not found: ",
        draft_asset_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  conditions <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM draft_asset_conditions
    WHERE draft_asset_id = ?
    ORDER BY condition_order;
    ",
    params = list(draft_asset_id)
  )
  
  audit <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM draft_asset_audit
    WHERE draft_asset_id = ?
    ORDER BY changed_at DESC, draft_audit_id DESC;
    ",
    params = list(draft_asset_id)
  )
  
  list(
    asset = asset,
    conditions = conditions,
    audit = audit
  )
}


# ------------------------------------------------------------
# Portfolio intelligence
# ------------------------------------------------------------

#' Summarize one organization's draft portfolio
#' @noRd
summarize_draft_assets <- function(assets) {
  if (is.null(assets)) {
    assets <- data.frame()
  }
  
  if (!is.data.frame(assets)) {
    stop(
      "assets must be a data frame.",
      call. = FALSE
    )
  }
  
  if (!nrow(assets)) {
    return(
      list(
        total_assets = 0L,
        controlled_first_round = 0L,
        controlled_second_round = 0L,
        outgoing_obligations = 0L,
        swap_rights = 0L,
        swap_obligations = 0L,
        high_value_assets = 0L,
        verified_assets = 0L,
        review_required = 0L,
        years_of_control = 0L,
        earliest_asset_year = NA_integer_,
        latest_asset_year = NA_integer_,
        portfolio_status = "No Assets Loaded",
        executive_summary =
          "No persistent draft assets are loaded for the selected organization."
      )
    )
  }
  
  controlled_types <- c(
    "Own",
    "Incoming",
    "Swap Right"
  )
  
  total_assets <- nrow(assets)
  
  controlled_first_round <- sum(
    assets$round == "First" &
      assets$control_type %in% controlled_types,
    na.rm = TRUE
  )
  
  controlled_second_round <- sum(
    assets$round == "Second" &
      assets$control_type %in% controlled_types,
    na.rm = TRUE
  )
  
  outgoing_obligations <- sum(
    assets$control_type %in%
      c(
        "Outgoing",
        "Swap Obligation"
      ),
    na.rm = TRUE
  )
  
  swap_rights <- sum(
    assets$control_type == "Swap Right",
    na.rm = TRUE
  )
  
  swap_obligations <- sum(
    assets$control_type == "Swap Obligation",
    na.rm = TRUE
  )
  
  high_value_assets <- sum(
    assets$strategic_value == "High",
    na.rm = TRUE
  )
  
  verified_assets <- sum(
    assets$verification_status == "Verified",
    na.rm = TRUE
  )
  
  review_required <- if (
    "requires_manual_review" %in% names(assets)
  ) {
    sum(
      assets$requires_manual_review,
      na.rm = TRUE
    )
  } else {
    sum(
      assets$verification_status != "Verified",
      na.rm = TRUE
    )
  }
  
  years <- sort(
    unique(
      assets$draft_year[
        assets$control_type %in% controlled_types
      ]
    )
  )
  
  portfolio_status <- if (
    review_required > 0
  ) {
    "Needs Verification"
  } else if (
    controlled_first_round >= 5
  ) {
    "Strong First-Round Control"
  } else if (
    outgoing_obligations > controlled_first_round
  ) {
    "Obligation Heavy"
  } else {
    "Balanced"
  }
  
  executive_summary <- paste0(
    "The organization controls ",
    controlled_first_round,
    " first-round asset",
    if (controlled_first_round == 1) "" else "s",
    " and ",
    controlled_second_round,
    " second-round asset",
    if (controlled_second_round == 1) "" else "s",
    ". It carries ",
    outgoing_obligations,
    " outgoing obligation",
    if (outgoing_obligations == 1) "" else "s",
    ". ",
    if (review_required > 0) {
      paste0(
        review_required,
        " asset",
        if (review_required == 1) "" else "s",
        " require verification before transaction use."
      )
    } else {
      "All loaded asset records are marked verified."
    }
  )
  
  list(
    total_assets = total_assets,
    controlled_first_round =
      controlled_first_round,
    controlled_second_round =
      controlled_second_round,
    outgoing_obligations =
      outgoing_obligations,
    swap_rights = swap_rights,
    swap_obligations = swap_obligations,
    high_value_assets = high_value_assets,
    verified_assets = verified_assets,
    review_required = review_required,
    years_of_control = length(years),
    earliest_asset_year = if (length(years)) {
      min(years)
    } else {
      NA_integer_
    },
    latest_asset_year = if (length(years)) {
      max(years)
    } else {
      NA_integer_
    },
    portfolio_status = portfolio_status,
    executive_summary = executive_summary
  )
}


#' Build a team-level draft portfolio directly from the database
#' @noRd
evaluate_draft_asset_portfolio <- function(
    team_value,
    year_from = NULL,
    year_to = NULL,
    db_path = NULL) {
  
  assets <- get_draft_assets(
    team_value = team_value,
    year_from = year_from,
    year_to = year_to,
    include_inactive = FALSE,
    db_path = db_path
  )
  
  summary <- summarize_draft_assets(
    assets
  )
  
  list(
    team = team_value,
    assets = assets,
    summary = summary,
    scope_note = paste(
      "Draft-asset records are decision-support data.",
      "Official transaction documents and league records remain controlling."
    )
  )
}
