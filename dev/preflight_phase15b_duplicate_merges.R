# ============================================================
# TBI — Phase 15B Duplicate Identity Batch Preflight
#
# READ-ONLY
#
# Keeps CURRENT 2026-27 roster IDs as canonical:
#
#   412 Moussa Diabaté      <- 680 Moussa Diabate
#   527 Tidjane Salaün      <- 730 Tidjane Salaun
#   411 Moussa Cissé        <- 706 Moussa Cisse
#   115 DaRon Holmes        <- 683 DaRon Holmes II
#   532 Tolu Smith III      <- 655 Tolu Smith
#    22 Alperen Şengün      <- 717 Alperen Sengun
#   180 GG Jackson II       <- 723 GG Jackson
#   316 Karlo Matković      <- 721 Karlo Matkovic
#   449 Pacôme Dadiet       <- 731 Pacome Dadiet
#   431 Nikola Topić        <- 727 Nikola Topic
#
# Purpose:
#   Confirm all source -> target merges can occur without
#   primary-key collisions.
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

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)

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

tables <- DBI::dbListTables(db)

player_tables <- tables[
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
]

pk_columns <- function(tbl) {
  
  info <- DBI::dbGetQuery(
    db,
    paste0(
      "PRAGMA table_info(",
      DBI::dbQuoteIdentifier(
        db,
        tbl
      ),
      ")"
    )
  )
  
  info$name[
    info$pk > 0
  ]
}

cat("\n")
cat("============================================================\n")
cat("TBI PHASE 15B — DUPLICATE IDENTITY BATCH PREFLIGHT\n")
cat("============================================================\n\n")

all_results <- list()
all_collisions <- list()

for (m in seq_len(nrow(merges))) {
  
  target_id <- merges$target_id[[m]]
  source_id <- merges$source_id[[m]]
  
  cat(
    "\n------------------------------------------------------------\n"
  )
  cat(
    source_id,
    " ",
    merges$source_name[[m]],
    "  ->  ",
    target_id,
    " ",
    merges$target_name[[m]],
    "\n",
    sep = ""
  )
  cat(
    "------------------------------------------------------------\n"
  )
  
  player_rows <- DBI::dbGetQuery(
    db,
    "
    SELECT
      player_id,
      player_name,
      height_inches,
      weight_lbs,
      primary_position,
      nba_player_id,
      player_age
    FROM players
    WHERE player_id IN (?, ?)
    ORDER BY player_id
    ",
    params = list(
      target_id,
      source_id
    )
  )
  
  print(
    player_rows,
    row.names = FALSE
  )
  
  if (nrow(player_rows) != 2L) {
    stop(
      paste0(
        "Expected both player IDs for merge ",
        source_id,
        " -> ",
        target_id
      ),
      call. = FALSE
    )
  }
  
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
    
    target_n <- DBI::dbGetQuery(
      db,
      paste0(
        "SELECT COUNT(*) AS n FROM ",
        qtbl,
        " WHERE player_id = ?"
      ),
      params = list(
        target_id
      )
    )$n[[1]]
    
    pks <- pk_columns(
      tbl
    )
    
    collision_n <- 0L
    
    if (
      length(pks) > 1L &&
      "player_id" %in% pks &&
      source_n > 0L &&
      target_n > 0L
    ) {
      
      other_pks <- setdiff(
        pks,
        "player_id"
      )
      
      if (length(other_pks)) {
        
        joins <- paste0(
          "s.",
          vapply(
            other_pks,
            function(x) {
              as.character(
                DBI::dbQuoteIdentifier(
                  db,
                  x
                )
              )
            },
            character(1)
          ),
          " = t.",
          vapply(
            other_pks,
            function(x) {
              as.character(
                DBI::dbQuoteIdentifier(
                  db,
                  x
                )
              )
            },
            character(1)
          ),
          collapse = " AND "
        )
        
        collision_n <- DBI::dbGetQuery(
          db,
          paste0(
            "SELECT COUNT(*) AS n FROM ",
            qtbl,
            " s INNER JOIN ",
            qtbl,
            " t ON ",
            joins,
            " WHERE s.player_id = ? ",
            "AND t.player_id = ?"
          ),
          params = list(
            source_id,
            target_id
          )
        )$n[[1]]
      }
    }
    
    all_results[[
      length(all_results) + 1L
    ]] <- data.frame(
      source_id = source_id,
      source_name =
        merges$source_name[[m]],
      target_id = target_id,
      target_name =
        merges$target_name[[m]],
      table = tbl,
      source_rows =
        as.integer(source_n),
      target_rows =
        as.integer(target_n),
      pk_columns =
        paste(
          pks,
          collapse = ", "
        ),
      collision_count =
        as.integer(collision_n),
      stringsAsFactors = FALSE
    )
    
    if (collision_n > 0L) {
      
      all_collisions[[
        length(all_collisions) + 1L
      ]] <- data.frame(
        source_id = source_id,
        target_id = target_id,
        table = tbl,
        collision_count =
          as.integer(
            collision_n
          ),
        stringsAsFactors = FALSE
      )
    }
  }
}

results <- do.call(
  rbind,
  all_results
)

dir.create(
  "qa",
  showWarnings = FALSE
)

utils::write.csv(
  results,
  file.path(
    "qa",
    "phase15b_duplicate_merge_preflight.csv"
  ),
  row.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat("PREFLIGHT SUMMARY\n")
cat("============================================================\n")

source_totals <- aggregate(
  source_rows ~
    source_id +
    source_name +
    target_id +
    target_name,
  data = results,
  FUN = sum
)

print(
  source_totals,
  row.names = FALSE
)

cat("\n")

if (length(all_collisions)) {
  
  collisions <- do.call(
    rbind,
    all_collisions
  )
  
  print(
    collisions,
    row.names = FALSE
  )
  
  cat("\n")
  cat(
    "PREFLIGHT STATUS: REVIEW REQUIRED\n"
  )
  cat(
    "Do NOT run the batch merge.\n"
  )
  
} else {
  
  cat(
    "No primary-key collisions detected across all 10 merges.\n"
  )
  cat(
    "PREFLIGHT STATUS: PASS\n"
  )
  cat(
    "Safe to proceed to controlled batch merge.\n"
  )
}

cat("============================================================\n")

if (
  !is.null(db) &&
  DBI::dbIsValid(db)
) {
  DBI::dbDisconnect(db)
}