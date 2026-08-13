# ============================================================
# Thompson's Basketball Intelligence
# Phase 11: Roster Intelligence Integration
# Bridges Phase 8 + Phase 9 into the existing depth-chart UI.
# ============================================================

phase11_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


phase11_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


phase11_apply_approved_lineup <- function(players,
                                          approved_lineup = NULL) {
  if (!is.data.frame(players)) {
    stop("players must be a data frame.", call. = FALSE)
  }
  
  if (nrow(players) == 0L) {
    return(players)
  }
  
  if (!"player_id" %in% names(players)) {
    stop("players must contain player_id.", call. = FALSE)
  }
  
  if (!"is_starter" %in% names(players)) {
    players$is_starter <- FALSE
  }
  
  if (!"position" %in% names(players)) {
    players$position <- ""
  }
  
  if (is.null(approved_lineup)) {
    return(players)
  }
  
  positions <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  approved <- approved_lineup[
    intersect(
      positions,
      names(approved_lineup)
    )
  ]
  
  approved_ids <- suppressWarnings(
    as.integer(
      unlist(
        approved,
        use.names = FALSE
      )
    )
  )
  
  approved_positions <- names(approved)
  
  valid <- !is.na(approved_ids)
  
  approved_ids <- approved_ids[valid]
  approved_positions <- approved_positions[valid]
  
  if (length(approved_ids) != 5L ||
      length(unique(approved_ids)) != 5L) {
    return(players)
  }
  
  players$is_starter <- FALSE
  
  for (i in seq_along(approved_ids)) {
    hit <- which(
      suppressWarnings(
        as.integer(
          players$player_id
        )
      ) == approved_ids[[i]]
    )
    
    if (length(hit) == 1L) {
      players$is_starter[[hit]] <- TRUE
      players$position[[hit]] <- approved_positions[[i]]
    }
  }
  
  players
}


phase11_rotation_explanation <- function(allocation) {
  if (is.null(allocation) ||
      !is.data.frame(allocation) ||
      nrow(allocation) == 0L) {
    return(
      "Phase 8 could not create a regulation rotation."
    )
  }
  
  active <- allocation[
    allocation$recommended_minutes > 0,
    ,
    drop = FALSE
  ]
  
  if (nrow(active) == 0L) {
    return(
      "Phase 8 did not allocate regulation minutes."
    )
  }
  
  top <- active[
    order(
      -active$recommended_minutes,
      active$player_name
    ),
    ,
    drop = FALSE
  ]
  
  top <- head(top, 3)
  
  top_text <- paste0(
    top$player_name,
    " ",
    top$recommended_minutes,
    " MIN",
    collapse = " • "
  )
  
  paste0(
    "Phase 8 allocates the full 240 regulation minutes using ",
    "BIE/impact, role value, depth-chart priority, projection signal, ",
    "starter status and availability. Top workload: ",
    top_text,
    "."
  )
}


build_phase11_rotation_result <- function(players,
                                          rotation_size = 9L,
                                          total_minutes = 240L,
                                          approved_lineup = NULL) {
  if (!exists(
    "build_minute_allocation",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 8 Minute Allocation Engine is not loaded.",
      call. = FALSE
    )
  }
  
  d <- phase11_apply_approved_lineup(
    players,
    approved_lineup = approved_lineup
  )
  
  result <- build_minute_allocation(
    roster = d,
    rotation_size = rotation_size,
    total_minutes = total_minutes
  )
  
  rotation <- result$allocation
  
  if (nrow(rotation) == 0L) {
    return(
      list(
        status = "NO_ROTATION",
        rotation = data.frame(),
        rotation_size = 0L,
        total_minutes = 0,
        score = NA_real_,
        confidence = "FOUNDATION",
        starting_five_source = if (
          is.null(approved_lineup)
        ) {
          "DEPTH CHART"
        } else {
          "WORKING LINEUP"
        },
        explanation =
          "Phase 8 returned no rotation rows.",
        phase8_result = result
      )
    )
  }
  
  rotation$bie_rotation_role <- ifelse(
    rotation$is_starter,
    "STARTER",
    "BENCH"
  )
  
  rotation$bie_rotation_slot <- ifelse(
    rotation$is_starter,
    rotation$position,
    paste0(
      "R",
      ifelse(
        is.na(rotation$rotation_rank),
        "",
        rotation$rotation_rank
      )
    )
  )
  
  rotation$bie_recommended_minutes <-
    rotation$recommended_minutes
  
  active <- rotation[
    rotation$recommended_minutes > 0,
    ,
    drop = FALSE
  ]
  
  score <- if (nrow(active) > 0L) {
    stats::weighted.mean(
      active$minute_priority,
      w = pmax(
        active$recommended_minutes,
        1
      )
    )
  } else {
    NA_real_
  }
  
  list(
    status = "OK",
    rotation = rotation,
    rotation_size =
      sum(
        rotation$recommended_minutes > 0
      ),
    total_minutes =
      sum(
        rotation$recommended_minutes
      ),
    score = score,
    confidence = "MODEL",
    starting_five_source = if (
      is.null(approved_lineup)
    ) {
      "DEPTH CHART"
    } else {
      "WORKING LINEUP"
    },
    explanation =
      phase11_rotation_explanation(
        rotation
      ),
    phase8_result = result
  )
}


build_phase11_lineup_result <- function(rotation_result,
                                        pool_size = NULL) {
  if (!exists(
    "build_lineup_optimization",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "Phase 9 Lineup Optimization Engine is not loaded.",
      call. = FALSE
    )
  }
  
  if (is.null(rotation_result) ||
      !identical(
        rotation_result$status,
        "OK"
      )) {
    return(NULL)
  }
  
  phase8_result <-
    rotation_result$phase8_result
  
  if (is.null(pool_size)) {
    pool_size <-
      rotation_result$rotation_size
  }
  
  build_lineup_optimization(
    roster_or_allocation =
      phase8_result,
    pool_size = pool_size
  )
}


phase11_lineup_card <- function(label,
                                item) {
  if (!requireNamespace(
    "shiny",
    quietly = TRUE
  )) {
    stop(
      "The shiny package is required to render Phase 11 UI.",
      call. = FALSE
    )
  }
  
  if (is.null(item)) {
    return(NULL)
  }
  
  shiny::div(
    class = "depth-phase11-lineup-card",
    
    shiny::div(
      class = "depth-phase11-lineup-head",
      
      shiny::span(
        class = "depth-phase11-lineup-label",
        label
      ),
      
      shiny::strong(
        class = "depth-phase11-lineup-score",
        sprintf(
          "%.1f",
          phase11_number(
            item$score,
            0
          )
        )
      )
    ),
    
    shiny::div(
      class = "depth-phase11-lineup-players",
      paste(
        item$players,
        collapse = " • "
      )
    ),
    
    shiny::div(
      class = "depth-phase11-lineup-metrics",
      
      shiny::span(
        paste0(
          "OFF ",
          sprintf(
            "%.1f",
            phase11_number(
              item$offense,
              0
            )
          )
        )
      ),
      
      shiny::span(
        paste0(
          "DEF ",
          sprintf(
            "%.1f",
            phase11_number(
              item$defense,
              0
            )
          )
        )
      ),
      
      shiny::span(
        paste0(
          "BIE ",
          sprintf(
            "%.1f",
            phase11_number(
              item$bie,
              0
            )
          )
        )
      )
    )
  )
}


build_phase11_lineup_panel <- function(lineup_result) {
  if (!requireNamespace(
    "shiny",
    quietly = TRUE
  )) {
    stop(
      "The shiny package is required to render Phase 11 UI.",
      call. = FALSE
    )
  }
  
  if (is.null(lineup_result)) {
    return(
      shiny::div(
        class = "depth-phase11-empty",
        "Phase 9 lineup optimization is unavailable."
      )
    )
  }
  
  shiny::div(
    class = "depth-phase11-shell",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .depth-phase11-shell{
          margin-top:14px;
          padding:14px;
          border:1px solid rgba(107,163,255,.18);
          border-radius:14px;
          background:rgba(8,18,34,.64);
        }
        .depth-phase11-kicker{
          display:block;
          margin-bottom:3px;
          font-size:10px;
          letter-spacing:.12em;
          font-weight:800;
          color:#7da7dc;
        }
        .depth-phase11-title{
          display:block;
          margin-bottom:12px;
          font-size:14px;
          color:#f4f8ff;
        }
        .depth-phase11-grid{
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:10px;
        }
        .depth-phase11-lineup-card{
          min-width:0;
          padding:11px 12px;
          border:1px solid rgba(130,173,229,.14);
          border-radius:11px;
          background:rgba(13,28,50,.72);
        }
        .depth-phase11-lineup-head{
          display:flex;
          justify-content:space-between;
          gap:8px;
          align-items:center;
          margin-bottom:7px;
        }
        .depth-phase11-lineup-label{
          font-size:10px;
          font-weight:800;
          letter-spacing:.08em;
          color:#9ebde3;
        }
        .depth-phase11-lineup-score{
          font-size:16px;
          color:#eaf3ff;
        }
        .depth-phase11-lineup-players{
          font-size:12px;
          line-height:1.45;
          color:#f5f8fc;
        }
        .depth-phase11-lineup-metrics{
          display:flex;
          flex-wrap:wrap;
          gap:8px;
          margin-top:8px;
          font-size:10px;
          color:#8fb0d6;
        }
        .depth-phase11-empty{
          margin-top:12px;
          padding:12px;
          border-radius:10px;
          background:rgba(11,25,45,.55);
          color:#8fa6c4;
          font-size:12px;
        }
        @media (max-width:1100px){
          .depth-phase11-grid{
            grid-template-columns:1fr;
          }
        }
        "
      )
    ),
    
    shiny::span(
      class = "depth-phase11-kicker",
      "PHASE 9 • LINEUP OPTIMIZATION"
    ),
    
    shiny::strong(
      class = "depth-phase11-title",
      "Optimized Five-Man Profiles"
    ),
    
    shiny::div(
      class = "depth-phase11-grid",
      
      phase11_lineup_card(
        "BALANCED",
        lineup_result$balanced
      ),
      
      phase11_lineup_card(
        "OFFENSE",
        lineup_result$offense
      ),
      
      phase11_lineup_card(
        "DEFENSE",
        lineup_result$defense
      ),
      
      phase11_lineup_card(
        "CLOSING",
        lineup_result$closing
      )
    )
  )
}

# >>> TBI_V1_OBJECTIVE_LINEUP_CARD_START >>>
# Objective-specific Phase 9 metric presentation.
phase11_lineup_card <- function (label, item) 
{
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("The shiny package is required.", call. = FALSE)
    }
    if (is.null(item)) {
        return(NULL)
    }
    type <- toupper(trimws(as.character(label)))
    metrics <- switch(type, OFFENSE = list(list(label = "OFF", value = item$offense), list(label = "CRE", value = item$creation), list(label = "SPACE", value = item$spacing)), DEFENSE = list(list(label = "DEF", value = item$defense), list(label = "REB", value = item$rebounding), list(label = "BIE", value = item$bie)), CLOSING = list(list(label = "BIE", value = item$bie), list(label = "CRE", value = item$creation), list(label = "DEF", value = item$defense)), list(list(label = "OFF", value = item$offense), 
        list(label = "DEF", value = item$defense), list(label = "BIE", value = item$bie)))
    metric_nodes <- lapply(metrics, function(metric) {
        shiny::span(paste0(metric$label, " ", sprintf("%.1f", phase11_number(metric$value, 0))))
    })
    shiny::div(class = "depth-phase11-lineup-card", shiny::div(class = "depth-phase11-lineup-head", shiny::span(class = "depth-phase11-lineup-label", label), shiny::strong(class = "depth-phase11-lineup-score", sprintf("%.1f", phase11_number(item$score, 0)))), shiny::div(class = "depth-phase11-lineup-players", paste(item$players, collapse = " • ")), shiny::div(class = "depth-phase11-lineup-metrics", metric_nodes))
}
# <<< TBI_V1_OBJECTIVE_LINEUP_CARD_END <<<


# >>> TBI_V1_OBJECTIVE_CARD_START >>>
# V1.0 objective-specific lineup-card metrics.
phase11_lineup_card <- function (label, item) 
{
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("The shiny package is required.", call. = FALSE)
    }
    if (is.null(item)) {
        return(NULL)
    }
    type <- toupper(trimws(as.character(label)))
    metrics <- switch(type, OFFENSE = list(list(label = "OFF", value = item$offense), list(label = "CRE", value = item$creation), list(label = "SPACE", value = item$spacing)), DEFENSE = list(list(label = "DEF", value = item$defense), list(label = "REB", value = item$rebounding), list(label = "BIE", value = item$bie)), CLOSING = list(list(label = "BIE", value = item$bie), list(label = "CRE", value = item$creation), list(label = "DEF", value = item$defense)), list(list(label = "OFF", value = item$offense), 
        list(label = "DEF", value = item$defense), list(label = "BIE", value = item$bie)))
    metric_nodes <- lapply(metrics, function(metric) {
        shiny::span(paste0(metric$label, " ", sprintf("%.1f", phase11_number(metric$value, 0))))
    })
    shiny::div(class = "depth-phase11-lineup-card", shiny::div(class = "depth-phase11-lineup-head", shiny::span(class = "depth-phase11-lineup-label", label), shiny::strong(class = "depth-phase11-lineup-score", sprintf("%.1f", phase11_number(item$score, 0)))), shiny::div(class = "depth-phase11-lineup-players", paste(item$players, collapse = " • ")), shiny::div(class = "depth-phase11-lineup-metrics", metric_nodes))
}
# <<< TBI_V1_OBJECTIVE_CARD_END <<<

