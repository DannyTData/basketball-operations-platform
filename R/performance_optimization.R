# ============================================================
# Thompson's Basketball Intelligence
# Phase 13: Performance + Loading Optimization
# ============================================================

phase13_scalar_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


phase13_scalar_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


phase13_roster_signature <- function(d) {
  if (is.null(d) ||
      !is.data.frame(d) ||
      nrow(d) == 0L) {
    return("EMPTY")
  }
  
  preferred <- c(
    "player_id",
    "player_name",
    "position",
    "depth_order",
    "is_starter",
    "availability_status",
    "bie_rating",
    "projected_bie_rating",
    "impact_score",
    "offensive_impact",
    "defensive_impact",
    "creation_score",
    "spacing_score",
    "rebounding_score"
  )
  
  cols <- intersect(
    preferred,
    names(d)
  )
  
  if (length(cols) == 0L) {
    return(
      paste0(
        "ROWS:",
        nrow(d),
        "|COLS:",
        ncol(d)
      )
    )
  }
  
  x <- d[, cols, drop = FALSE]
  
  if ("player_id" %in% names(x)) {
    ord <- order(
      suppressWarnings(
        as.integer(x$player_id)
      ),
      na.last = TRUE
    )
    
    x <- x[ord, , drop = FALSE]
  }
  
  values <- vapply(
    names(x),
    function(nm) {
      paste(
        nm,
        paste(
          ifelse(
            is.na(x[[nm]]),
            "<NA>",
            as.character(x[[nm]])
          ),
          collapse = ","
        ),
        sep = "="
      )
    },
    character(1)
  )
  
  paste(
    nrow(x),
    paste(values, collapse = "||"),
    sep = "::"
  )
}


phase13_lineup_signature <- function(lineup) {
  if (is.null(lineup)) {
    return("NO_LINEUP")
  }
  
  if (is.list(lineup)) {
    lineup <- unlist(
      lineup,
      use.names = TRUE
    )
  }
  
  paste(
    names(lineup),
    suppressWarnings(
      as.integer(lineup)
    ),
    sep = "=",
    collapse = "|"
  )
}


phase13_trade_signature <- function(scenario) {
  if (is.null(scenario)) {
    return("NO_SCENARIO")
  }
  
  ids <- function(x) {
    if (is.null(x) ||
        !is.data.frame(x) ||
        nrow(x) == 0L ||
        !"player_id" %in% names(x)) {
      return("")
    }
    
    paste(
      sort(
        unique(
          suppressWarnings(
            as.integer(
              x$player_id
            )
          )
        ),
        na.last = TRUE
      ),
      collapse = ","
    )
  }
  
  paste(
    phase13_scalar_text(
      scenario$team,
      ""
    ),
    phase13_scalar_text(
      scenario$partner_team,
      ""
    ),
    ids(
      scenario$outgoing_players
    ),
    ids(
      scenario$incoming_players
    ),
    phase13_scalar_number(
      scenario$outgoing_salary,
      0
    ),
    phase13_scalar_number(
      scenario$incoming_salary,
      0
    ),
    sep = "||"
  )
}


phase13_cache_new <- function() {
  list(
    key = NULL,
    value = NULL,
    hits = 0L,
    misses = 0L
  )
}


phase13_cache_get <- function(cache,
                              key) {
  if (is.null(cache) ||
      is.null(cache$key) ||
      !identical(
        cache$key,
        key
      ) ||
      is.null(cache$value)) {
    return(NULL)
  }
  
  cache$value
}


phase13_cache_store <- function(cache,
                                key,
                                value) {
  hits <- if (
    !is.null(cache$hits)
  ) {
    as.integer(cache$hits)
  } else {
    0L
  }
  
  misses <- if (
    !is.null(cache$misses)
  ) {
    as.integer(cache$misses)
  } else {
    0L
  }
  
  list(
    key = key,
    value = value,
    hits = hits,
    misses = misses + 1L
  )
}


phase13_cache_hit <- function(cache) {
  cache$hits <- as.integer(
    cache$hits %||% 0L
  ) + 1L
  
  cache
}


phase13_performance_status <- function(cache_list) {
  if (is.null(cache_list) ||
      !length(cache_list)) {
    return(
      list(
        hits = 0L,
        misses = 0L,
        hit_rate = 0
      )
    )
  }
  
  hits <- sum(
    vapply(
      cache_list,
      function(x) {
        as.integer(
          x$hits %||% 0L
        )
      },
      integer(1)
    )
  )
  
  misses <- sum(
    vapply(
      cache_list,
      function(x) {
        as.integer(
          x$misses %||% 0L
        )
      },
      integer(1)
    )
  )
  
  total <- hits + misses
  
  list(
    hits = hits,
    misses = misses,
    hit_rate = if (
      total > 0
    ) {
      hits / total
    } else {
      0
    }
  )
}