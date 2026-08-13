# ============================================================
# TBI — Jimmy Butler Identity Merge Preflight
# Canonical target: 734 = Jimmy Butler III
# Legacy source:    275 = Jimmy Butler
#
# READ-ONLY. Makes no changes.
# ============================================================

library(DBI)
library(RSQLite)

db_path <- file.path("inst", "database", "tbi.sqlite")

if (!file.exists(db_path)) {
  stop("Database not found at inst/database/tbi.sqlite", call. = FALSE)
}

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)


source_id <- 275L
target_id <- 734L

cat("\n============================================================\n")
cat("JIMMY BUTLER IDENTITY MERGE PREFLIGHT\n")
cat("SOURCE 275 -> TARGET 734\n")
cat("============================================================\n\n")

players <- DBI::dbGetQuery(
  db,
  "
  SELECT *
  FROM players
  WHERE player_id IN (?, ?)
  ORDER BY player_id
  ",
  params = list(source_id, target_id)
)

cat("[1] PLAYER RECORDS\n")
print(players, row.names = FALSE)
cat("\n")

if (nrow(players) != 2L) {
  stop("Expected both player_id 275 and 734 to exist.", call. = FALSE)
}

tables <- DBI::dbListTables(db)

# Find every table containing player_id.
player_tables <- tables[
  vapply(
    tables,
    function(tbl) {
      "player_id" %in% DBI::dbListFields(db, tbl)
    },
    logical(1)
  )
]

cat("[2] TABLES CONTAINING player_id\n")
cat(paste(player_tables, collapse = "\n"), "\n\n")

# Utility for primary key columns.
pk_columns <- function(tbl) {
  info <- DBI::dbGetQuery(
    db,
    paste0("PRAGMA table_info(", DBI::dbQuoteIdentifier(db, tbl), ")")
  )
  info$name[info$pk > 0]
}

summary_rows <- list()
collision_rows <- list()

for (tbl in player_tables) {

  quoted_tbl <- as.character(DBI::dbQuoteIdentifier(db, tbl))

  source_n <- DBI::dbGetQuery(
    db,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quoted_tbl,
      " WHERE player_id = ?"
    ),
    params = list(source_id)
  )$n[[1]]

  target_n <- DBI::dbGetQuery(
    db,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quoted_tbl,
      " WHERE player_id = ?"
    ),
    params = list(target_id)
  )$n[[1]]

  pks <- pk_columns(tbl)

  collision_count <- 0L
  collision_detail <- ""

  # If the table has a composite PK including player_id, check whether
  # replacing 275 with 734 would duplicate an existing target key.
  if (
    length(pks) > 1L &&
    "player_id" %in% pks &&
    source_n > 0 &&
    target_n > 0
  ) {
    other_pks <- setdiff(pks, "player_id")

    if (length(other_pks)) {

      join_conditions <- paste0(
        "s.",
        vapply(other_pks, function(x) as.character(DBI::dbQuoteIdentifier(db, x)), character(1)),
        " = t.",
        vapply(other_pks, function(x) as.character(DBI::dbQuoteIdentifier(db, x)), character(1)),
        collapse = " AND "
      )

      sql <- paste0(
        "SELECT COUNT(*) AS n FROM ",
        quoted_tbl,
        " s INNER JOIN ",
        quoted_tbl,
        " t ON ",
        join_conditions,
        " WHERE s.player_id = ? AND t.player_id = ?"
      )

      collision_count <- DBI::dbGetQuery(
        db,
        sql,
        params = list(source_id, target_id)
      )$n[[1]]

      if (collision_count > 0) {
        collision_detail <- paste(other_pks, collapse = ", ")
      }
    }
  }

  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    table = tbl,
    source_275_rows = as.integer(source_n),
    target_734_rows = as.integer(target_n),
    pk_columns = paste(pks, collapse = ", "),
    collision_count = as.integer(collision_count),
    stringsAsFactors = FALSE
  )

  if (collision_count > 0) {
    collision_rows[[length(collision_rows) + 1L]] <- data.frame(
      table = tbl,
      collision_count = as.integer(collision_count),
      matching_key_columns = collision_detail,
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- do.call(rbind, summary_rows)

cat("[3] TABLE-BY-TABLE REFERENCE SUMMARY\n")
print(summary_df, row.names = FALSE)
cat("\n")

if (length(collision_rows)) {
  collision_df <- do.call(rbind, collision_rows)
} else {
  collision_df <- data.frame()
}

cat("[4] PRIMARY-KEY COLLISION CHECK\n")

if (nrow(collision_df)) {
  print(collision_df, row.names = FALSE)
  cat("\nPREFLIGHT STATUS: REVIEW REQUIRED\n")
  cat("Do NOT run the merge yet.\n")
} else {
  cat("No composite-primary-key collisions detected.\n")
  cat("\nPREFLIGHT STATUS: PASS\n")
  cat("Safe to proceed to controlled merge script.\n")
}

cat("============================================================\n")

# ------------------------------------------------------------
# Close database connection explicitly
# ------------------------------------------------------------
if (!is.null(db) && DBI::dbIsValid(db)) {
  DBI::dbDisconnect(db)
}

