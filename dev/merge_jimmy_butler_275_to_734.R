# ============================================================
# TBI — Jimmy Butler Identity Merge
#
# Canonical target:
#   734 = Jimmy Butler III
#
# Legacy source:
#   275 = Jimmy Butler
#
# This script:
#   1. Creates a timestamped SQLite backup
#   2. Uses a transaction
#   3. Copies useful bio fields from 275 -> 734 when target is blank
#   4. Moves every player_id reference 275 -> 734
#   5. Remaps external_player_identity to 734
#   6. Deletes legacy player row 275
#   7. Commits only if all checks pass
#
# Run ONLY after preflight PASS.
# ============================================================

library(DBI)
library(RSQLite)

db_path <- file.path("inst", "database", "tbi.sqlite")

if (!file.exists(db_path)) {
  stop("Database not found.", call. = FALSE)
}

source_id <- 275L
target_id <- 734L

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- file.path(
  "inst",
  "database",
  paste0("tbi_before_jimmy_merge_", timestamp, ".sqlite")
)

ok <- file.copy(
  db_path,
  backup_path,
  overwrite = FALSE
)

if (!isTRUE(ok)) {
  stop("Could not create database backup. Merge aborted.", call. = FALSE)
}

cat("Backup created:\n", backup_path, "\n\n", sep = "")

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)


DBI::dbExecute(db, "PRAGMA foreign_keys = OFF;")
DBI::dbBegin(db)

success <- FALSE

tryCatch(
  {
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
    
    if (nrow(players) != 2L) {
      stop("Expected both source and target player rows.")
    }
    
    src <- players[players$player_id == source_id, , drop = FALSE]
    tgt <- players[players$player_id == target_id, , drop = FALSE]
    
    # ----------------------------------------------------------
    # Preserve preferred display identity from 734, fill bio gaps
    # ----------------------------------------------------------
    
    fill_fields <- intersect(
      c(
        "birth_date",
        "height_inches",
        "weight_lbs",
        "primary_position",
        "nba_player_id",
        "player_age"
      ),
      names(players)
    )
    
    updates <- list()
    
    for (field in fill_fields) {
      src_val <- src[[field]][[1]]
      tgt_val <- tgt[[field]][[1]]
      
      target_missing <- is.na(tgt_val) ||
        (is.character(tgt_val) && !nzchar(trimws(tgt_val)))
      
      source_available <- !is.na(src_val) &&
        !(is.character(src_val) && !nzchar(trimws(src_val)))
      
      if (target_missing && source_available) {
        updates[[field]] <- src_val
      }
    }
    
    # Explicitly ensure canonical display name.
    updates[["player_name"]] <- "Jimmy Butler III"
    
    if (length(updates)) {
      set_clause <- paste0(
        names(updates),
        " = ?",
        collapse = ", "
      )
      
      DBI::dbExecute(
        db,
        paste0(
          "UPDATE players SET ",
          set_clause,
          ", updated_at = CURRENT_TIMESTAMP ",
          "WHERE player_id = ?"
        ),
        params = c(
          unname(updates),
          list(target_id)
        )
      )
    }
    
    # ----------------------------------------------------------
    # Find all tables with player_id and move references
    # ----------------------------------------------------------
    
    tables <- DBI::dbListTables(db)
    
    player_tables <- setdiff(
      tables[
        vapply(
          tables,
          function(tbl) {
            "player_id" %in% DBI::dbListFields(db, tbl)
          },
          logical(1)
        )
      ],
      "players"
    )
    
    for (tbl in player_tables) {
      
      quoted_tbl <- as.character(
        DBI::dbQuoteIdentifier(db, tbl)
      )
      
      source_n <- DBI::dbGetQuery(
        db,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          quoted_tbl,
          " WHERE player_id = ?"
        ),
        params = list(source_id)
      )$n[[1]]
      
      if (source_n == 0L) next
      
      # Re-run duplicate-key protection using PK metadata.
      info <- DBI::dbGetQuery(
        db,
        paste0(
          "PRAGMA table_info(",
          quoted_tbl,
          ")"
        )
      )
      
      pks <- info$name[info$pk > 0]
      
      if (
        length(pks) > 1L &&
        "player_id" %in% pks
      ) {
        other_pks <- setdiff(
          pks,
          "player_id"
        )
        
        if (length(other_pks)) {
          join_conditions <- paste0(
            "s.",
            vapply(other_pks, function(x) as.character(DBI::dbQuoteIdentifier(db, x)), character(1)),
            " = t.",
            vapply(other_pks, function(x) as.character(DBI::dbQuoteIdentifier(db, x)), character(1)),
            collapse = " AND "
          )
          
          collision_n <- DBI::dbGetQuery(
            db,
            paste0(
              "SELECT COUNT(*) AS n FROM ",
              quoted_tbl,
              " s INNER JOIN ",
              quoted_tbl,
              " t ON ",
              join_conditions,
              " WHERE s.player_id = ? AND t.player_id = ?"
            ),
            params = list(
              source_id,
              target_id
            )
          )$n[[1]]
          
          if (collision_n > 0L) {
            stop(
              paste0(
                "Collision detected in table ",
                tbl,
                ". Merge aborted."
              )
            )
          }
        }
      }
      
      changed <- DBI::dbExecute(
        db,
        paste0(
          "UPDATE ",
          quoted_tbl,
          " SET player_id = ? ",
          "WHERE player_id = ?"
        ),
        params = list(
          target_id,
          source_id
        )
      )
      
      cat(
        tbl,
        ": moved ",
        changed,
        " row(s)\n",
        sep = ""
      )
    }
    
    # ----------------------------------------------------------
    # Remove legacy player row
    # ----------------------------------------------------------
    
    remaining_refs <- 0L
    
    for (tbl in setdiff(player_tables, "players")) {
      quoted_tbl <- as.character(
        DBI::dbQuoteIdentifier(db, tbl)
      )
      
      n <- DBI::dbGetQuery(
        db,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          quoted_tbl,
          " WHERE player_id = ?"
        ),
        params = list(source_id)
      )$n[[1]]
      
      remaining_refs <- remaining_refs + as.integer(n)
    }
    
    if (remaining_refs > 0L) {
      stop(
        paste0(
          "Legacy player 275 still has ",
          remaining_refs,
          " references. Merge aborted."
        )
      )
    }
    
    DBI::dbExecute(
      db,
      "DELETE FROM players WHERE player_id = ?",
      params = list(source_id)
    )
    
    # ----------------------------------------------------------
    # Verification before commit
    # ----------------------------------------------------------
    
    canonical <- DBI::dbGetQuery(
      db,
      "
      SELECT *
      FROM players
      WHERE player_id = ?
      ",
      params = list(target_id)
    )
    
    if (nrow(canonical) != 1L) {
      stop("Canonical player 734 missing after merge.")
    }
    
    if (!identical(
      as.character(canonical$player_name[[1]]),
      "Jimmy Butler III"
    )) {
      stop("Canonical display name is not Jimmy Butler III.")
    }
    
    DBI::dbCommit(db)
    success <- TRUE
    
    cat("\n============================================================\n")
    cat("JIMMY BUTLER MERGE: PASS\n")
    cat("Canonical player is now 734 — Jimmy Butler III\n")
    cat("Backup preserved at:\n", backup_path, "\n", sep = "")
    cat("============================================================\n")
  },
  error = function(e) {
    if (DBI::dbIsValid(db)) {
      try(DBI::dbRollback(db), silent = TRUE)
    }
    
    cat("\n============================================================\n")
    cat("JIMMY BUTLER MERGE: FAIL\n")
    cat(conditionMessage(e), "\n")
    cat("Database transaction rolled back.\n")
    cat("Backup remains at:\n", backup_path, "\n", sep = "")
    cat("============================================================\n")
    
    stop(e)
  }
)

if (success) {
  DBI::dbExecute(db, "PRAGMA foreign_keys = ON;")
}

# ------------------------------------------------------------
# Close database connection explicitly
# ------------------------------------------------------------
if (!is.null(db) && DBI::dbIsValid(db)) {
  DBI::dbDisconnect(db)
}