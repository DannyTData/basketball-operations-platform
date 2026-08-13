# ============================================================
# Thompson's Basketball Intelligence
# Phase 8: Minute Allocation + Rotation Intelligence Engine
# ============================================================

# ------------------------------------------------------------
# Scalar helpers
# ------------------------------------------------------------

minute_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


minute_integer <- function(x, default = NA_integer_) {
  value <- minute_number(x, default = NA_real_)
  
  if (!is.finite(value)) {
    return(default)
  }
  
  as.integer(round(value))
}


minute_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


minute_flag <- function(x, default = FALSE) {
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


minute_clamp <- function(x, lower, upper) {
  min(max(x, lower), upper)
}


minute_first_existing <- function(data, candidates, default = NA) {
  hit <- candidates[candidates %in% names(data)]
  
  if (length(hit) == 0L) {
    return(rep(default, nrow(data)))
  }
  
  data[[hit[[1]]]]
}


# ------------------------------------------------------------
# Rules
# ------------------------------------------------------------

minute_allocation_rule_defaults <- function() {
  list(
    regulation_minutes = 240,
    default_rotation_size = 10L,
    minimum_rotation_size = 8L,
    maximum_rotation_size = 12L,
    
    starter_min = 26,
    starter_max = 38,
    bench_min = 8,
    bench_max = 30,
    fringe_min = 0,
    fringe_max = 14,
    
    bie_weight = 0.34,
    projected_bie_weight = 0.16,
    impact_weight = 0.14,
    role_weight = 0.10,
    depth_weight = 0.10,
    starter_weight = 0.08,
    availability_weight = 0.08,
    
    current_minutes_anchor_weight = 0.20,
    
    model_label = "TBI_MINUTES_v1_ROTATION_INTELLIGENCE"
  )
}


resolve_minute_allocation_rules <- function(overrides = NULL) {
  rules <- minute_allocation_rule_defaults()
  
  if (is.null(overrides)) {
    return(rules)
  }
  
  if (!is.list(overrides)) {
    stop("Minute-allocation rule overrides must be a list.", call. = FALSE)
  }
  
  unknown <- setdiff(names(overrides), names(rules))
  
  if (length(unknown) > 0L) {
    stop(
      paste0(
        "Unknown minute-allocation rule override(s): ",
        paste(unknown, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  for (name in names(overrides)) {
    rules[[name]] <- overrides[[name]]
  }
  
  total_weight <-
    rules$bie_weight +
    rules$projected_bie_weight +
    rules$impact_weight +
    rules$role_weight +
    rules$depth_weight +
    rules$starter_weight +
    rules$availability_weight
  
  if (!isTRUE(all.equal(total_weight, 1, tolerance = 1e-8))) {
    stop(
      "Minute-allocation scoring weights must sum to 1.",
      call. = FALSE
    )
  }
  
  if (
    rules$minimum_rotation_size < 5 ||
    rules$maximum_rotation_size < rules$minimum_rotation_size
  ) {
    stop("Rotation-size rules are invalid.", call. = FALSE)
  }
  
  rules
}


# ------------------------------------------------------------
# Normalization
# ------------------------------------------------------------

normalize_availability_status <- function(x) {
  value <- tolower(minute_text(x, "available"))
  
  if (value %in% c("available", "active", "healthy", "probable")) {
    return("AVAILABLE")
  }
  
  if (value %in% c("questionable", "limited", "minutes restriction", "restricted")) {
    return("LIMITED")
  }
  
  if (value %in% c("out", "inactive", "injured", "suspended", "unavailable")) {
    return("OUT")
  }
  
  "AVAILABLE"
}


availability_score <- function(status) {
  status <- normalize_availability_status(status)
  
  switch(
    status,
    "AVAILABLE" = 100,
    "LIMITED" = 55,
    "OUT" = 0,
    100
  )
}


role_priority_score <- function(primary_role = "",
                                archetype = "",
                                impact_tier = "") {
  text <- toupper(
    paste(
      minute_text(primary_role),
      minute_text(archetype),
      minute_text(impact_tier)
    )
  )
  
  score <- 50
  
  if (grepl("PRIMARY|ENGINE|STAR|ELITE", text)) score <- score + 30
  if (grepl("CREATOR|SCORER|TWO-WAY|STARTER", text)) score <- score + 15
  if (grepl("CONNECTOR|SPACER|DEFENDER|RIM|REBOUNDER", text)) score <- score + 8
  if (grepl("DEPTH|FRINGE|DEVELOPMENT", text)) score <- score - 15
  
  minute_clamp(score, 0, 100)
}


depth_priority_score <- function(depth_order) {
  depth <- minute_integer(depth_order, 4L)
  
  if (is.na(depth) || depth < 1L) {
    depth <- 4L
  }
  
  switch(
    as.character(min(depth, 5L)),
    "1" = 100,
    "2" = 78,
    "3" = 56,
    "4" = 35,
    "5" = 20,
    20
  )
}


# ------------------------------------------------------------
# Roster preparation
# ------------------------------------------------------------

prepare_minute_allocation_roster <- function(roster) {
  if (is.null(roster)) {
    roster <- data.frame()
  }
  
  if (!is.data.frame(roster)) {
    stop("roster must be a data frame.", call. = FALSE)
  }
  
  if (nrow(roster) == 0L) {
    return(
      data.frame(
        player_id = integer(),
        player_name = character(),
        position = character(),
        depth_order = integer(),
        is_starter = logical(),
        availability_status = character(),
        bie_rating = numeric(),
        projected_bie_rating = numeric(),
        impact_score = numeric(),
        primary_role = character(),
        archetype = character(),
        impact_tier = character(),
        current_minutes = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  player_id <- minute_first_existing(
    roster,
    c("player_id", "PLAYER_ID", "id"),
    default = NA_integer_
  )
  
  player_name <- minute_first_existing(
    roster,
    c("player_name", "PLAYER_NAME", "name"),
    default = ""
  )
  
  position <- minute_first_existing(
    roster,
    c("position", "assigned_position", "Pos", "pos"),
    default = ""
  )
  
  depth_order <- minute_first_existing(
    roster,
    c("depth_order", "depth", "rotation_order"),
    default = 4L
  )
  
  is_starter <- minute_first_existing(
    roster,
    c("is_starter", "starter", "starting"),
    default = FALSE
  )
  
  availability <- minute_first_existing(
    roster,
    c(
      "availability_status",
      "availability",
      "status",
      "planning_status"
    ),
    default = "Available"
  )
  
  bie <- minute_first_existing(
    roster,
    c(
      "bie_player_score",
      "bie_vnext_player_score",
      "bie_selection_score",
      "bie_rating",
      "latest_bie_rating",
      "player_bie_rating",
      "BIE"
    ),
    default = 50
  )
  
  projected_bie <- minute_first_existing(
    roster,
    c(
      "projected_bie_rating",
      "projected_bie_1y",
      "future_bie"
    ),
    default = NA_real_
  )
  
  impact <- minute_first_existing(
    roster,
    c(
      "bie_impact_score",
      "impact_score",
      "all_around_impact",
      "overall_impact",
      "impact_rating"
    ),
    default = NA_real_
  )
  
  primary_role <- minute_first_existing(
    roster,
    c("primary_role", "role", "role_primary"),
    default = ""
  )
  
  archetype <- minute_first_existing(
    roster,
    c("archetype", "player_archetype"),
    default = ""
  )
  
  impact_tier <- minute_first_existing(
    roster,
    c("impact_tier", "bie_tier"),
    default = ""
  )
  
  current_minutes <- minute_first_existing(
    roster,
    c(
      "current_minutes",
      "minutes_per_game",
      "mpg",
      "MPG"
    ),
    default = NA_real_
  )
  
  out <- data.frame(
    player_id = suppressWarnings(as.integer(player_id)),
    player_name = as.character(player_name),
    position = as.character(position),
    depth_order = suppressWarnings(as.integer(depth_order)),
    is_starter = vapply(
      seq_len(nrow(roster)),
      function(i) minute_flag(is_starter[i], FALSE),
      logical(1)
    ),
    availability_status = vapply(
      seq_len(nrow(roster)),
      function(i) normalize_availability_status(availability[i]),
      character(1)
    ),
    bie_rating = suppressWarnings(as.numeric(bie)),
    projected_bie_rating = suppressWarnings(as.numeric(projected_bie)),
    impact_score = suppressWarnings(as.numeric(impact)),
    primary_role = as.character(primary_role),
    archetype = as.character(archetype),
    impact_tier = as.character(impact_tier),
    current_minutes = suppressWarnings(as.numeric(current_minutes)),
    stringsAsFactors = FALSE
  )
  
  out$player_name[is.na(out$player_name)] <- ""
  out$position[is.na(out$position)] <- ""
  
  out$bie_rating[!is.finite(out$bie_rating)] <- 50
  
  projected_missing <- !is.finite(out$projected_bie_rating)
  out$projected_bie_rating[projected_missing] <-
    out$bie_rating[projected_missing]
  
  impact_missing <- !is.finite(out$impact_score)
  out$impact_score[impact_missing] <-
    out$bie_rating[impact_missing]
  
  out$current_minutes[!is.finite(out$current_minutes)] <- NA_real_
  
  out
}


# ------------------------------------------------------------
# Player minute priority
# ------------------------------------------------------------

calculate_player_minute_priority <- function(player,
                                             rules = minute_allocation_rule_defaults()) {
  if (is.data.frame(player)) {
    if (nrow(player) != 1L) {
      stop("player data frame must contain exactly one row.", call. = FALSE)
    }
    
    player <- as.list(player[1, , drop = FALSE])
  }
  
  if (!is.list(player)) {
    stop("player must be a named list or one-row data frame.", call. = FALSE)
  }
  
  bie <- minute_clamp(
    minute_number(player$bie_rating, 50),
    0,
    100
  )
  
  projected <- minute_clamp(
    minute_number(player$projected_bie_rating, bie),
    0,
    100
  )
  
  impact <- minute_clamp(
    minute_number(player$impact_score, bie),
    0,
    100
  )
  
  role <- role_priority_score(
    player$primary_role,
    player$archetype,
    player$impact_tier
  )
  
  depth <- depth_priority_score(
    player$depth_order
  )
  
  starter <- if (minute_flag(player$is_starter, FALSE)) 100 else 45
  
  availability <- availability_score(
    player$availability_status
  )
  
  score <-
    rules$bie_weight * bie +
    rules$projected_bie_weight * projected +
    rules$impact_weight * impact +
    rules$role_weight * role +
    rules$depth_weight * depth +
    rules$starter_weight * starter +
    rules$availability_weight * availability
  
  if (
    normalize_availability_status(
      player$availability_status
    ) == "OUT"
  ) {
    score <- 0
  }
  
  minute_clamp(score, 0, 100)
}


score_minute_allocation_roster <- function(roster,
                                           rule_overrides = NULL) {
  rules <- resolve_minute_allocation_rules(rule_overrides)
  roster <- prepare_minute_allocation_roster(roster)
  
  if (nrow(roster) == 0L) {
    roster$minute_priority <- numeric()
    roster$role_priority <- numeric()
    roster$depth_priority <- numeric()
    return(roster)
  }
  
  roster$role_priority <- vapply(
    seq_len(nrow(roster)),
    function(i) {
      role_priority_score(
        roster$primary_role[[i]],
        roster$archetype[[i]],
        roster$impact_tier[[i]]
      )
    },
    numeric(1)
  )
  
  roster$depth_priority <- vapply(
    roster$depth_order,
    depth_priority_score,
    numeric(1)
  )
  
  roster$minute_priority <- vapply(
    seq_len(nrow(roster)),
    function(i) {
      calculate_player_minute_priority(
        roster[i, , drop = FALSE],
        rules = rules
      )
    },
    numeric(1)
  )
  
  roster
}


# ------------------------------------------------------------
# Rotation selection
# ------------------------------------------------------------

select_rotation_players <- function(scored_roster,
                                    rotation_size = NULL,
                                    rule_overrides = NULL) {
  rules <- resolve_minute_allocation_rules(rule_overrides)
  
  if (!is.data.frame(scored_roster)) {
    stop("scored_roster must be a data frame.", call. = FALSE)
  }
  
  if (nrow(scored_roster) == 0L) {
    scored_roster$in_rotation <- logical()
    scored_roster$rotation_rank <- integer()
    return(scored_roster)
  }
  
  if (!"minute_priority" %in% names(scored_roster)) {
    scored_roster <- score_minute_allocation_roster(
      scored_roster,
      rule_overrides = rule_overrides
    )
  }
  
  rotation_size <- minute_integer(
    rotation_size,
    rules$default_rotation_size
  )
  
  rotation_size <- minute_clamp(
    rotation_size,
    rules$minimum_rotation_size,
    rules$maximum_rotation_size
  )
  
  available_idx <- which(
    scored_roster$availability_status != "OUT"
  )
  
  actual_size <- min(
    rotation_size,
    length(available_idx)
  )
  
  ordering <- order(
    -as.integer(scored_roster$is_starter),
    scored_roster$depth_order,
    -scored_roster$minute_priority,
    scored_roster$player_name,
    na.last = TRUE
  )
  
  eligible_order <- ordering[
    ordering %in% available_idx
  ]
  
  selected <- head(
    eligible_order,
    actual_size
  )
  
  scored_roster$in_rotation <- FALSE
  scored_roster$in_rotation[selected] <- TRUE
  
  rotation_rank <- rep(NA_integer_, nrow(scored_roster))
  rotation_rank[selected] <- seq_along(selected)
  
  scored_roster$rotation_rank <- rotation_rank
  
  scored_roster
}


# ------------------------------------------------------------
# Minute boundaries
# ------------------------------------------------------------

player_minute_bounds <- function(player,
                                 rules = minute_allocation_rule_defaults()) {
  status <- normalize_availability_status(
    player$availability_status
  )
  
  if (status == "OUT") {
    return(c(min = 0, max = 0))
  }
  
  in_rotation <- minute_flag(
    player$in_rotation,
    FALSE
  )
  
  if (!in_rotation) {
    return(
      c(
        min = rules$fringe_min,
        max = rules$fringe_max
      )
    )
  }
  
  if (minute_flag(player$is_starter, FALSE)) {
    max_value <- rules$starter_max
    
    if (status == "LIMITED") {
      max_value <- min(max_value, 24)
    }
    
    return(
      c(
        min = min(rules$starter_min, max_value),
        max = max_value
      )
    )
  }
  
  max_value <- rules$bench_max
  
  if (status == "LIMITED") {
    max_value <- min(max_value, 18)
  }
  
  c(
    min = min(rules$bench_min, max_value),
    max = max_value
  )
}


# ------------------------------------------------------------
# Constrained allocation
# ------------------------------------------------------------

allocate_minutes_with_bounds <- function(weights,
                                         lower,
                                         upper,
                                         total_minutes) {
  weights <- pmax(as.numeric(weights), 0)
  lower <- pmax(as.numeric(lower), 0)
  upper <- pmax(as.numeric(upper), lower)
  
  if (
    length(weights) != length(lower) ||
    length(weights) != length(upper)
  ) {
    stop("weights, lower, and upper must have equal length.", call. = FALSE)
  }
  
  if (sum(lower) - total_minutes > 1e-8) {
    stop(
      "Minimum minute constraints exceed available regulation minutes.",
      call. = FALSE
    )
  }
  
  if (sum(upper) + 1e-8 < total_minutes) {
    stop(
      "Maximum minute constraints cannot reach available regulation minutes.",
      call. = FALSE
    )
  }
  
  allocation <- lower
  remaining <- total_minutes - sum(allocation)
  
  active <- which(upper - allocation > 1e-8)
  
  guard <- 0L
  
  while (remaining > 1e-8 && length(active) > 0L) {
    guard <- guard + 1L
    
    if (guard > 1000L) {
      stop("Minute allocation did not converge.", call. = FALSE)
    }
    
    active_weights <- weights[active]
    
    if (sum(active_weights) <= 0) {
      active_weights <- rep(1, length(active))
    }
    
    proposed <-
      remaining *
      active_weights /
      sum(active_weights)
    
    capacity <- upper[active] - allocation[active]
    added <- pmin(proposed, capacity)
    
    allocation[active] <- allocation[active] + added
    
    new_remaining <- total_minutes - sum(allocation)
    
    if (abs(new_remaining - remaining) < 1e-10) {
      break
    }
    
    remaining <- new_remaining
    
    active <- active[
      upper[active] - allocation[active] > 1e-8
    ]
  }
  
  if (remaining > 1e-6) {
    for (idx in order(upper - allocation, decreasing = TRUE)) {
      if (remaining <= 1e-8) break
      
      add <- min(
        remaining,
        upper[idx] - allocation[idx]
      )
      
      if (add > 0) {
        allocation[idx] <- allocation[idx] + add
        remaining <- remaining - add
      }
    }
  }
  
  allocation
}


round_minutes_to_team_total <- function(minutes,
                                        target_total = 240L) {
  raw <- pmax(as.numeric(minutes), 0)
  floored <- floor(raw)
  remainder <- as.integer(target_total - sum(floored))
  
  if (remainder > 0L) {
    fractions <- raw - floored
    order_idx <- order(
      fractions,
      decreasing = TRUE
    )
    
    for (idx in head(order_idx, remainder)) {
      floored[idx] <- floored[idx] + 1
    }
  }
  
  if (remainder < 0L) {
    fractions <- raw - floored
    order_idx <- order(
      fractions,
      decreasing = FALSE
    )
    
    needed <- abs(remainder)
    
    for (idx in order_idx) {
      if (needed <= 0L) break
      
      if (floored[idx] > 0L) {
        floored[idx] <- floored[idx] - 1
        needed <- needed - 1L
      }
    }
  }
  
  as.integer(floored)
}


# ------------------------------------------------------------
# Main allocation engine
# ------------------------------------------------------------

build_minute_allocation <- function(roster,
                                    rotation_size = NULL,
                                    total_minutes = NULL,
                                    manual_overrides = NULL,
                                    rule_overrides = NULL) {
  rules <- resolve_minute_allocation_rules(rule_overrides)
  
  total_minutes <- minute_integer(
    total_minutes,
    rules$regulation_minutes
  )
  
  if (is.na(total_minutes) || total_minutes <= 0) {
    stop("total_minutes must be positive.", call. = FALSE)
  }
  
  scored <- score_minute_allocation_roster(
    roster,
    rule_overrides = rule_overrides
  )
  
  if (nrow(scored) == 0L) {
    scored$recommended_minutes <- integer()
    scored$minute_share <- numeric()
    scored$minute_delta <- numeric()
    scored$allocation_reason <- character()
    
    return(
      list(
        allocation = scored,
        summary = summarize_minute_allocation(scored),
        total_minutes = total_minutes,
        model_label = rules$model_label
      )
    )
  }
  
  selected <- select_rotation_players(
    scored,
    rotation_size = rotation_size,
    rule_overrides = rule_overrides
  )
  
  bounds <- t(
    vapply(
      seq_len(nrow(selected)),
      function(i) {
        player_minute_bounds(
          selected[i, , drop = FALSE],
          rules = rules
        )
      },
      numeric(2)
    )
  )
  
  lower <- bounds[, "min"]
  upper <- bounds[, "max"]
  
  # Players outside the selected rotation receive zero by default.
  lower[!selected$in_rotation] <- 0
  upper[!selected$in_rotation] <- 0
  
  weights <- pmax(
    selected$minute_priority,
    1
  )
  
  # Anchor toward known current minutes when available without overriding
  # basketball intelligence.
  current_anchor <- selected$current_minutes
  has_anchor <- is.finite(current_anchor)
  
  if (any(has_anchor)) {
    normalized_anchor <- rep(0, nrow(selected))
    
    max_anchor <- max(
      current_anchor[has_anchor],
      na.rm = TRUE
    )
    
    if (is.finite(max_anchor) && max_anchor > 0) {
      normalized_anchor[has_anchor] <-
        current_anchor[has_anchor] / max_anchor * 100
    }
    
    weights <-
      (1 - rules$current_minutes_anchor_weight) * weights +
      rules$current_minutes_anchor_weight * normalized_anchor
  }
  
  # Manual overrides are optional and only affect named player IDs.
  override_minutes <- rep(NA_real_, nrow(selected))
  
  if (!is.null(manual_overrides)) {
    if (!is.data.frame(manual_overrides)) {
      stop(
        "manual_overrides must be a data frame with player_id and minutes.",
        call. = FALSE
      )
    }
    
    required <- c("player_id", "minutes")
    
    if (!all(required %in% names(manual_overrides))) {
      stop(
        "manual_overrides must contain player_id and minutes.",
        call. = FALSE
      )
    }
    
    for (i in seq_len(nrow(manual_overrides))) {
      pid <- suppressWarnings(
        as.integer(manual_overrides$player_id[[i]])
      )
      
      mins <- suppressWarnings(
        as.numeric(manual_overrides$minutes[[i]])
      )
      
      hit <- which(
        !is.na(selected$player_id) &
          selected$player_id == pid
      )
      
      if (length(hit) == 1L && is.finite(mins)) {
        override_minutes[hit] <- mins
      }
    }
  }
  
  fixed <- which(is.finite(override_minutes))
  
  if (length(fixed) > 0L) {
    for (idx in fixed) {
      if (override_minutes[idx] < 0) {
        stop("Manual minute overrides cannot be negative.", call. = FALSE)
      }
      
      if (override_minutes[idx] > upper[idx] + 1e-8) {
        stop(
          paste0(
            "Manual minute override exceeds maximum for ",
            selected$player_name[[idx]],
            "."
          ),
          call. = FALSE
        )
      }
      
      lower[idx] <- override_minutes[idx]
      upper[idx] <- override_minutes[idx]
    }
  }
  
  raw_minutes <- allocate_minutes_with_bounds(
    weights = weights,
    lower = lower,
    upper = upper,
    total_minutes = total_minutes
  )
  
  recommended <- round_minutes_to_team_total(
    raw_minutes,
    target_total = total_minutes
  )
  
  selected$recommended_minutes <- recommended
  
  selected$minute_share <-
    selected$recommended_minutes /
    total_minutes
  
  selected$minute_delta <- ifelse(
    is.finite(selected$current_minutes),
    selected$recommended_minutes -
      selected$current_minutes,
    NA_real_
  )
  
  selected$allocation_reason <- vapply(
    seq_len(nrow(selected)),
    function(i) {
      status <- selected$availability_status[[i]]
      
      if (status == "OUT") {
        return("Unavailable — no regulation minutes allocated.")
      }
      
      if (!selected$in_rotation[[i]]) {
        return("Outside recommended rotation.")
      }
      
      starter_text <- if (selected$is_starter[[i]]) {
        "starter"
      } else {
        "rotation player"
      }
      
      paste0(
        "Recommended as a ",
        starter_text,
        " based on BIE/impact, role value, depth-chart priority, ",
        "projection signal, and availability."
      )
    },
    character(1)
  )
  
  selected <- selected[
    order(
      -selected$recommended_minutes,
      selected$rotation_rank,
      selected$player_name,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  rownames(selected) <- NULL
  
  list(
    allocation = selected,
    summary = summarize_minute_allocation(selected),
    total_minutes = total_minutes,
    model_label = rules$model_label
  )
}


# ------------------------------------------------------------
# Summary + comparison
# ------------------------------------------------------------

summarize_minute_allocation <- function(allocation) {
  if (is.null(allocation) || nrow(allocation) == 0L) {
    return(
      list(
        rotation_size = 0L,
        total_minutes = 0L,
        starter_minutes = 0L,
        bench_minutes = 0L,
        top_player = NA_character_,
        top_player_minutes = 0L,
        limited_players = 0L,
        unavailable_players = 0L
      )
    )
  }
  
  in_rotation <- allocation$recommended_minutes > 0
  
  top_idx <- if (any(in_rotation)) {
    which.max(allocation$recommended_minutes)
  } else {
    NA_integer_
  }
  
  list(
    rotation_size = sum(in_rotation),
    total_minutes = sum(allocation$recommended_minutes),
    starter_minutes = sum(
      allocation$recommended_minutes[
        allocation$is_starter
      ]
    ),
    bench_minutes = sum(
      allocation$recommended_minutes[
        !allocation$is_starter
      ]
    ),
    top_player = if (!is.na(top_idx)) {
      allocation$player_name[[top_idx]]
    } else {
      NA_character_
    },
    top_player_minutes = if (!is.na(top_idx)) {
      allocation$recommended_minutes[[top_idx]]
    } else {
      0L
    },
    limited_players = sum(
      allocation$availability_status == "LIMITED"
    ),
    unavailable_players = sum(
      allocation$availability_status == "OUT"
    )
  )
}


compare_minute_allocations <- function(base_allocation,
                                       scenario_allocation) {
  extract <- function(x, prefix) {
    if (is.list(x) && "allocation" %in% names(x)) {
      x <- x$allocation
    }
    
    if (!is.data.frame(x)) {
      stop("Allocations must be data frames or build_minute_allocation results.", call. = FALSE)
    }
    
    required <- c(
      "player_id",
      "player_name",
      "recommended_minutes"
    )
    
    if (!all(required %in% names(x))) {
      stop(
        "Allocation data is missing required columns.",
        call. = FALSE
      )
    }
    
    out <- x[, required, drop = FALSE]
    
    names(out)[names(out) == "recommended_minutes"] <-
      paste0(prefix, "_minutes")
    
    out
  }
  
  base <- extract(base_allocation, "base")
  scenario <- extract(scenario_allocation, "scenario")
  
  merged <- merge(
    base,
    scenario,
    by = c("player_id", "player_name"),
    all = TRUE
  )
  
  merged$base_minutes[is.na(merged$base_minutes)] <- 0
  merged$scenario_minutes[is.na(merged$scenario_minutes)] <- 0
  
  merged$minute_change <-
    merged$scenario_minutes -
    merged$base_minutes
  
  merged <- merged[
    order(
      -abs(merged$minute_change),
      merged$player_name
    ),
    ,
    drop = FALSE
  ]
  
  rownames(merged) <- NULL
  
  merged
}

# TBI_VNEXT_PHASE8_FIELD_COMPAT

# >>> TBI_PHASE8_COMPONENT_PASSTHROUGH_START >>>
# Preserve frozen BIE components through Phase 8.
# This is a compatibility/data-passthrough layer only.
# BIE scoring architecture is unchanged.
tbi_phase8_prepare_base <- prepare_minute_allocation_roster

prepare_minute_allocation_roster <- function (roster) 
{
    original <- roster
    out <- tbi_phase8_prepare_base(roster)
    if (!is.data.frame(original) || !is.data.frame(out) || !nrow(out) || nrow(original) != nrow(out)) {
        return(out)
    }
    metric_from <- function(candidates, fallback) {
        fallback <- suppressWarnings(as.numeric(fallback))
        for (nm in candidates) {
            if (!nm %in% names(original)) {
                next
            }
            value <- suppressWarnings(as.numeric(original[[nm]]))
            if (length(value) != nrow(out) || !any(is.finite(value))) {
                next
            }
            missing <- !is.finite(value)
            value[missing] <- fallback[missing]
            value <- pmin(pmax(value, 0), 100)
            return(value)
        }
        fallback
    }
    out$offensive_impact <- metric_from(c("bie_offense_score", "offensive_impact", "offensive_impact_score", "offense_score"), out$bie_rating)
    out$defensive_impact <- metric_from(c("bie_defense_score", "defensive_impact", "defensive_impact_score", "defense_score"), out$bie_rating)
    out$creation_score <- metric_from(c("bie_playmaking_score", "bie_creation_score", "creation_score", "playmaking_score"), out$offensive_impact)
    out$spacing_score <- metric_from(c("bie_spacing_score", "bie_shooting_score", "spacing_score", "shooting_spacing_score", "shooting_score"), out$offensive_impact)
    out$rebounding_score <- metric_from(c("bie_rebounding_score", "rebounding_score", "rebound_score"), out$defensive_impact)
    out
}
# <<< TBI_PHASE8_COMPONENT_PASSTHROUGH_END <<<


# >>> TBI_V1_PRESEASON_PHASE8_START >>>
# V1.0 preseason metadata/component passthrough.
tbi_v1_flag <- function (x) 
{
    if (is.null(x) || !length(x)) {
        return(FALSE)
    }
    value <- x[[1]]
    if (is.logical(value)) {
        return(isTRUE(value))
    }
    if (is.numeric(value)) {
        return(is.finite(value) && value != 0)
    }
    text <- tolower(trimws(as.character(value)))
    text %in% c("1", "true", "yes", "y", "rookie")
}

tbi_v1_minute_base <- prepare_minute_allocation_roster

prepare_minute_allocation_roster <- function (roster) 
{
    original <- roster
    out <- tbi_v1_minute_base(roster)
    if (!is.data.frame(original) || !is.data.frame(out) || !nrow(out) || nrow(original) != nrow(out)) {
        return(out)
    }
    metric_from <- function(candidates, fallback) {
        fallback <- suppressWarnings(as.numeric(fallback))
        for (nm in candidates) {
            if (!nm %in% names(original)) {
                next
            }
            value <- suppressWarnings(as.numeric(original[[nm]]))
            if (length(value) != nrow(out) || !any(is.finite(value))) {
                next
            }
            missing <- !is.finite(value)
            value[missing] <- fallback[missing]
            return(pmin(pmax(value, 0), 100))
        }
        fallback
    }
    out$offensive_impact <- metric_from(c("bie_offense_score", "offensive_impact", "offensive_impact_score", "offense_score"), out$bie_rating)
    out$defensive_impact <- metric_from(c("bie_defense_score", "defensive_impact", "defensive_impact_score", "defense_score"), out$bie_rating)
    out$creation_score <- metric_from(c("bie_playmaking_score", "bie_creation_score", "creation_score", "playmaking_score"), out$offensive_impact)
    out$spacing_score <- metric_from(c("bie_spacing_score", "bie_shooting_score", "spacing_score", "shooting_score"), out$offensive_impact)
    out$rebounding_score <- metric_from(c("bie_rebounding_score", "rebounding_score", "rebound_score"), out$defensive_impact)
    rookie <- rep(FALSE, nrow(out))
    if ("is_rookie" %in% names(original)) {
        rookie <- vapply(seq_len(nrow(original)), function(i) {
            tbi_v1_flag(original$is_rookie[i])
        }, logical(1))
    }
    if ("contract_type" %in% names(original)) {
        contract_rookie <- grepl("rookie", tolower(ifelse(is.na(original$contract_type), "", as.character(original$contract_type))), fixed = TRUE)
        rookie <- rookie | contract_rookie
    }
    if ("tbi_performance_available" %in% names(original)) {
        performance_available <- vapply(seq_len(nrow(original)), function(i) {
            tbi_v1_flag(original$tbi_performance_available[i])
        }, logical(1))
        rookie <- rookie | !performance_available
    }
    out$is_preseason_rookie <- as.logical(rookie)
    out$is_starter <- vapply(seq_len(nrow(out)), function(i) {
        tbi_v1_flag(out$is_starter[i])
    }, logical(1))
    out
}
# <<< TBI_V1_PRESEASON_PHASE8_END <<<


# >>> TBI_V1_STRICT_FIRST_YEAR_ROOKIE_START >>>
# Strict first-year rookie classification.
# Replaces broad preseason rookie/pending classification.
# BIE architecture is unchanged.

tbi_v1_strict_rookie_base <- prepare_minute_allocation_roster

prepare_minute_allocation_roster <- function (roster) 
{
    original <- roster
    out <- tbi_v1_strict_rookie_base(roster)
    if (!is.data.frame(original) || !is.data.frame(out) || !nrow(out) || nrow(original) != nrow(out)) {
        return(out)
    }
    roster_season <- "2026-27"
    if ("season" %in% names(original)) {
        seasons <- unique(as.character(original$season))
        seasons <- seasons[!is.na(seasons) & nzchar(trimws(seasons))]
        if (length(seasons)) {
            roster_season <- seasons[[1]]
        }
    }
    con <- tryCatch(connect_db(read_only = TRUE), error = function(e) {
        connect_db()
    })
    on.exit(disconnect_db(con), add = TRUE)
    tables <- DBI::dbListTables(con)
    strict_rookie <- rep(FALSE, nrow(out))
    contract_start_used <- rep(NA_character_, nrow(out))
    prior_games_used <- rep(NA_real_, nrow(out))
    for (i in seq_len(nrow(out))) {
        player_id <- suppressWarnings(as.integer(original$player_id[[i]]))
        if (!is.finite(player_id)) {
            next
        }
        prior_games <- 0
        if ("player_season_stats" %in% tables) {
            stats_fields <- DBI::dbListFields(con, "player_season_stats")
            if (all(c("player_id", "season") %in% stats_fields)) {
                game_expression <- if ("games_played" %in% stats_fields) {
                  "COALESCE(SUM(games_played), 0)"
                }
                else {
                  "COUNT(*)"
                }
                prior <- tryCatch(DBI::dbGetQuery(con, paste0("SELECT ", game_expression, " AS prior_games ", "FROM player_season_stats ", "WHERE player_id = ? ", "AND season < ?"), params = list(player_id, roster_season)), error = function(e) {
                  data.frame(prior_games = 0)
                })
                if (nrow(prior)) {
                  prior_games <- suppressWarnings(as.numeric(prior$prior_games[[1]]))
                  if (!is.finite(prior_games)) {
                    prior_games <- 0
                  }
                }
            }
        }
        prior_games_used[[i]] <- prior_games
        contract_start <- NA_character_
        if (all(c("contract_years", "contracts") %in% tables)) {
            contract_fields <- DBI::dbListFields(con, "contracts")
            year_fields <- DBI::dbListFields(con, "contract_years")
            if ("contract_start_season" %in% contract_fields && all(c("player_id", "season", "contract_id") %in% year_fields)) {
                contract_result <- tryCatch(DBI::dbGetQuery(con, "\n              SELECT\n                c.contract_start_season\n              FROM contract_years cy\n\n              INNER JOIN contracts c\n                ON c.contract_id = cy.contract_id\n\n              WHERE cy.player_id = ?\n                AND cy.season = ?\n                AND c.contract_start_season IS NOT NULL\n\n              ORDER BY\n                c.contract_start_season DESC\n\n              LIMIT 1\n              ", 
                  params = list(player_id, roster_season)), error = function(e) {
                  data.frame()
                })
                if (nrow(contract_result)) {
                  contract_start <- as.character(contract_result$contract_start_season[[1]])
                }
            }
        }
        contract_start_used[[i]] <- contract_start
        strict_rookie[[i]] <- isTRUE(prior_games == 0) && !is.na(contract_start) && identical(trimws(contract_start), trimws(roster_season))
    }
    out$is_preseason_rookie <- strict_rookie
    out$tbi_prior_nba_games <- prior_games_used
    out$tbi_contract_start_season <- contract_start_used
    out$tbi_rookie_definition <- ifelse(strict_rookie, "FIRST_YEAR_ROOKIE_CONFIRMED", "VETERAN_OR_NOT_CONFIRMED_ROOKIE")
    out
}
# <<< TBI_V1_STRICT_FIRST_YEAR_ROOKIE_END <<<


# >>> TBI_V1_FINAL_ZERO_GAME_ROOKIE_START >>>
# Final V1.0 preseason rookie classification.
# Zero prior NBA games = first-year rookie.
# Starting rookies remain eligible downstream.
tbi_v1_final_rookie_base <- prepare_minute_allocation_roster

prepare_minute_allocation_roster <- function (roster) 
{
    out <- tbi_v1_final_rookie_base(roster)
    if (!is.data.frame(out) || !nrow(out)) {
        return(out)
    }
    if (!"tbi_prior_nba_games" %in% names(out)) {
        stop(paste("tbi_prior_nba_games is missing.", "Final rookie classification cannot run safely."), call. = FALSE)
    }
    prior_games <- suppressWarnings(as.numeric(out$tbi_prior_nba_games))
    out$is_preseason_rookie <- is.finite(prior_games) & prior_games == 0
    out$tbi_rookie_definition <- ifelse(out$is_preseason_rookie, "ZERO_PRIOR_NBA_GAMES", "PRIOR_NBA_EXPERIENCE")
    out
}
# <<< TBI_V1_FINAL_ZERO_GAME_ROOKIE_END <<<

