# ============================================================
# TBI — Phase 15B Duplicate Identity Batch Merge
#
# Run ONLY after:
#   preflight_phase15b_duplicate_merges.R
# returns PREFLIGHT STATUS: PASS
#
# Canonical IDs are the CURRENT 2026-27 roster IDs.
#
# Creates a full SQLite backup before any write.
# Uses one transaction for all 10 identity merges.
# ============================================================

library(DBI)
library(RSQLite)

db_path <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

if (!file.exists(db_path)) {
  stop(
    "Database not found.",
    call. = FALSE
  )
}

merges <- data.frame(
  target_id = c(
    412L, 527L, 411L, 115L, 532L,
    22L, 180L, 316L, 449L, 431L
  ),
  target_name = c(
    "Moussa Diabaté",
    "Tidjane Salaün",
    "Moussa Cissé",
    "DaRon Holmes",
    "Tolu Smith III",
    "Alperen Şengün",
    "GG Jackson II",
    "Karlo Matković",
    "Pacôme Dadiet",
    "Nikola Topić"
  ),
  source_id = c(
    680L, 730L, 706L, 683L, 655L,
    717L, 723L, 721L, 731L, 727L
  ),
  source_name = c(
    "Moussa Diabate",
    "Tidjane Salaun",
    "Moussa Cisse",
    "DaRon Holmes II",
    "Tolu Smith",
    "Alperen Sengun",
    "GG Jackson",
    "Karlo Matkovic",
    "Pacome Dadiet",
    "Nikola Topic"
  ),
  stringsAsFactors = FALSE
)

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

backup_path <- file.path(
  "inst",
  "database",
  paste0(
    "tbi_before_phase15b_identity_merge_",
    timestamp,
    ".sqlite"
  )
)

backup_ok <- file.copy(
  db_path,
  backup_path,
  overwrite = FALSE
)

if (!isTRUE(backup_ok)) {
  stop(
    "Database backup failed. Merge aborted.",
    call. = FALSE
  )
}

cat(
  "Backup created:\n",
  backup_path,
  "\n\n",
  sep = ""
)

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)

DBI::dbExecute(
  db,
  "PRAGMA foreign_keys = OFF;"
)

DBI::dbBegin(
  db
)

merge_ok <- FALSE

tryCatch(
  {
    
    tables <- DBI::dbListTables(
      db
    )
    
    player_tables <- setdiff(
      tables[
        vapply(
          tables,
          function(tbl) {
            "player_id" %in%
              DBI::dbListFields(
                db,
                tbl
              )
          },
          logical(1)
        )
      ],
      "players"
    )
    
    for (m in seq_len(nrow(merges))) {
      
      source_id <- merges$source_id[[m]]
      target_id <- merges$target_id[[m]]
      
      cat(
        "\n------------------------------------------------------------\n"
      )
      cat(
        source_id,
        " ",
        merges$source_name[[m]],
        " -> ",
        target_id,
        " ",
        merges$target_name[[m]],
        "\n",
        sep = ""
      )
      cat(
        "------------------------------------------------------------\n"
      )
      
      players <- DBI::dbGetQuery(
        db,
        "
        SELECT *
        FROM players
        WHERE player_id IN (?, ?)
        ORDER BY player_id
        ",
        params = list(
          source_id,
          target_id
        )
      )
      
      if (nrow(players) != 2L) {
        stop(
          paste0(
            "Expected both player rows for ",
            source_id,
            " -> ",
            target_id
          )
        )
      }
      
      src <- players[
        players$player_id ==
          source_id,
        ,
        drop = FALSE
      ]
      
      tgt <- players[
        players$player_id ==
          target_id,
        ,
        drop = FALSE
      ]
      
      # ------------------------------------------------------
      # Fill missing canonical bio fields from analytics ID.
      # Never overwrite current roster identity/display name.
      # ------------------------------------------------------
      
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
        
        target_missing <-
          is.na(tgt_val) ||
          (
            is.character(tgt_val) &&
              !nzchar(
                trimws(tgt_val)
              )
          )
        
        source_available <-
          !is.na(src_val) &&
          !(
            is.character(src_val) &&
              !nzchar(
                trimws(src_val)
              )
          )
        
        if (
          target_missing &&
          source_available
        ) {
          updates[[field]] <-
            src_val
        }
      }
      
      # Keep canonical 2026-27 display name.
      updates[["player_name"]] <-
        merges$target_name[[m]]
      
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
      
      # ------------------------------------------------------
      # Move every database reference source -> target.
      # ------------------------------------------------------
      
      for (tbl in player_tables) {
        
        qtbl <- as.character(
          DBI::dbQuoteIdentifier(
            db,
            tbl
          )
        )
        
        source_n <- DBI::dbGetQuery(
          db,
          paste0(
            "SELECT COUNT(*) AS n FROM ",
            qtbl,
            " WHERE player_id = ?"
          ),
          params = list(
            source_id
          )
        )$n[[1]]
        
        if (source_n == 0L) {
          next
        }
        
        changed <- DBI::dbExecute(
          db,
          paste0(
            "UPDATE ",
            qtbl,
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
      
      # ------------------------------------------------------
      # Confirm no old references remain.
      # ------------------------------------------------------
      
      remaining <- 0L
      
      for (tbl in player_tables) {
        
        qtbl <- as.character(
          DBI::dbQuoteIdentifier(
            db,
            tbl
          )
        )
        
        remaining <- remaining +
          as.integer(
            DBI::dbGetQuery(
              db,
              paste0(
                "SELECT COUNT(*) AS n FROM ",
                qtbl,
                " WHERE player_id = ?"
              ),
              params = list(
                source_id
              )
            )$n[[1]]
          )
      }
      
      if (remaining > 0L) {
        stop(
          paste0(
            "Source ID ",
            source_id,
            " still has ",
            remaining,
            " references."
          )
        )
      }
      
      # Delete duplicate canonical-player row.
      DBI::dbExecute(
        db,
        "
        DELETE FROM players
        WHERE player_id = ?
        ",
        params = list(
          source_id
        )
      )
      
      check_target <- DBI::dbGetQuery(
        db,
        "
        SELECT
          player_id,
          player_name
        FROM players
        WHERE player_id = ?
        ",
        params = list(
          target_id
        )
      )
      
      if (
        nrow(check_target) != 1L ||
        !identical(
          as.character(
            check_target$
            player_name[[1]]
          ),
          merges$target_name[[m]]
        )
      ) {
        stop(
          paste0(
            "Canonical validation failed for target ",
            target_id
          )
        )
      }
    }
    
    DBI::dbCommit(
      db
    )
    
    merge_ok <- TRUE
    
    cat("\n")
    cat("============================================================\n")
    cat("PHASE 15B DUPLICATE IDENTITY MERGE: PASS\n")
    cat("10/10 duplicate identities merged successfully.\n")
    cat(
      "Backup preserved at:\n",
      backup_path,
      "\n",
      sep = ""
    )
    cat("============================================================\n")
  },
  error = function(e) {
    
    if (
      !is.null(db) &&
      DBI::dbIsValid(db)
    ) {
      try(
        DBI::dbRollback(
          db
        ),
        silent = TRUE
      )
    }
    
    cat("\n")
    cat("============================================================\n")
    cat("PHASE 15B DUPLICATE IDENTITY MERGE: FAIL\n")
    cat(
      conditionMessage(e),
      "\n"
    )
    cat(
      "Transaction rolled back.\n"
    )
    cat(
      "Backup remains at:\n",
      backup_path,
      "\n",
      sep = ""
    )
    cat("============================================================\n")
    
    stop(e)
  }
)

if (
  !is.null(db) &&
  DBI::dbIsValid(db)
) {
  if (merge_ok) {
    DBI::dbExecute(
      db,
      "PRAGMA foreign_keys = ON;"
    )
  }
  
  DBI::dbDisconnect(
    db
  )
}