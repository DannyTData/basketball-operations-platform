# ============================================================
# Thompson's Basketball Intelligence
# Phase 9: Lineup Optimization Engine
# ============================================================

# ------------------------------------------------------------
# Scalar helpers
# ------------------------------------------------------------

lineup_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


lineup_integer <- function(x, default = NA_integer_) {
  value <- lineup_number(x, NA_real_)
  
  if (!is.finite(value)) {
    return(default)
  }
  
  as.integer(round(value))
}


lineup_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


lineup_flag <- function(x, default = FALSE) {
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


lineup_clamp <- function(x, lower = 0, upper = 100) {
  min(max(x, lower), upper)
}


lineup_first_existing <- function(data, candidates, default = NA) {
  hit <- candidates[candidates %in% names(data)]
  
  if (length(hit) == 0L) {
    return(rep(default, nrow(data)))
  }
  
  data[[hit[[1]]]]
}


# ------------------------------------------------------------
# Rules
# ------------------------------------------------------------

lineup_optimization_rule_defaults <- function() {
  list(
    minimum_pool_size = 5L,
    maximum_pool_size = 12L,
    maximum_candidate_lineups = 792L,
    
    balanced_offense_weight = 0.28,
    balanced_defense_weight = 0.26,
    balanced_bie_weight = 0.20,
    balanced_creation_weight = 0.10,
    balanced_spacing_weight = 0.08,
    balanced_rebounding_weight = 0.08,
    
    offense_offense_weight = 0.34,
    offense_bie_weight = 0.20,
    offense_creation_weight = 0.18,
    offense_spacing_weight = 0.18,
    offense_defense_weight = 0.05,
    offense_rebounding_weight = 0.05,
    
    defense_defense_weight = 0.38,
    defense_bie_weight = 0.18,
    defense_rebounding_weight = 0.18,
    defense_offense_weight = 0.10,
    defense_creation_weight = 0.08,
    defense_spacing_weight = 0.08,
    
    closing_bie_weight = 0.24,
    closing_offense_weight = 0.20,
    closing_defense_weight = 0.20,
    closing_creation_weight = 0.14,
    closing_spacing_weight = 0.10,
    closing_rebounding_weight = 0.07,
    closing_minutes_weight = 0.05,
    
    position_balance_bonus = 4,
    position_review_penalty = 4,
    unavailable_penalty = 100,
    
    model_label = "TBI_LINEUP_v1_OPTIMIZATION"
  )
}


resolve_lineup_optimization_rules <- function(overrides = NULL) {
  rules <- lineup_optimization_rule_defaults()
  
  if (is.null(overrides)) {
    return(rules)
  }
  
  if (!is.list(overrides)) {
    stop("Lineup-optimization rule overrides must be a list.", call. = FALSE)
  }
  
  unknown <- setdiff(names(overrides), names(rules))
  
  if (length(unknown) > 0L) {
    stop(
      paste0(
        "Unknown lineup-optimization rule override(s): ",
        paste(unknown, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  for (name in names(overrides)) {
    rules[[name]] <- overrides[[name]]
  }
  
  check_weights <- function(values, label) {
    total <- sum(
      unlist(
        rules[values],
        use.names = FALSE
      )
    )
    
    if (!isTRUE(all.equal(total, 1, tolerance = 1e-8))) {
      stop(
        paste0(
          label,
          " lineup weights must sum to 1."
        ),
        call. = FALSE
      )
    }
  }
  
  check_weights(
    c(
      "balanced_offense_weight",
      "balanced_defense_weight",
      "balanced_bie_weight",
      "balanced_creation_weight",
      "balanced_spacing_weight",
      "balanced_rebounding_weight"
    ),
    "Balanced"
  )
  
  check_weights(
    c(
      "offense_offense_weight",
      "offense_bie_weight",
      "offense_creation_weight",
      "offense_spacing_weight",
      "offense_defense_weight",
      "offense_rebounding_weight"
    ),
    "Offensive"
  )
  
  check_weights(
    c(
      "defense_defense_weight",
      "defense_bie_weight",
      "defense_rebounding_weight",
      "defense_offense_weight",
      "defense_creation_weight",
      "defense_spacing_weight"
    ),
    "Defensive"
  )
  
  check_weights(
    c(
      "closing_bie_weight",
      "closing_offense_weight",
      "closing_defense_weight",
      "closing_creation_weight",
      "closing_spacing_weight",
      "closing_rebounding_weight",
      "closing_minutes_weight"
    ),
    "Closing"
  )
  
  rules
}


# ------------------------------------------------------------
# Normalization
# ------------------------------------------------------------

normalize_lineup_position <- function(x) {
  value <- toupper(
    gsub(
      "[^A-Z]",
      "",
      lineup_text(x)
    )
  )
  
  if (!nzchar(value)) {
    return("UTIL")
  }
  
  aliases <- c(
    "PG" = "PG",
    "POINTGUARD" = "PG",
    "G" = "G",
    "GUARD" = "G",
    "SG" = "SG",
    "SHOOTINGGUARD" = "SG",
    "SF" = "SF",
    "SMALLFORWARD" = "SF",
    "F" = "F",
    "FORWARD" = "F",
    "PF" = "PF",
    "POWERFORWARD" = "PF",
    "C" = "C",
    "CENTER" = "C",
    "FC" = "FC",
    "CF" = "FC",
    "GF" = "GF",
    "FG" = "GF"
  )
  
  if (!value %in% names(aliases)) {
    return("UTIL")
  }
  
  unname(aliases[[value]])
}


position_group <- function(position) {
  position <- normalize_lineup_position(position)
  
  if (position %in% c("PG", "SG", "G")) return("GUARD")
  if (position %in% c("SF", "PF", "F")) return("WING")
  if (position %in% c("C", "FC")) return("BIG")
  if (position %in% c("GF")) return("WING")
  
  "UTIL"
}


position_balance_evaluation <- function(positions) {
  groups <- vapply(
    positions,
    position_group,
    character(1)
  )
  
  guards <- sum(groups == "GUARD")
  wings <- sum(groups == "WING")
  bigs <- sum(groups == "BIG")
  utils <- sum(groups == "UTIL")
  
  balanced <-
    guards >= 1L &&
    wings >= 1L &&
    bigs >= 1L
  
  review_required <-
    guards == 0L ||
    bigs == 0L ||
    utils >= 2L
  
  list(
    guards = guards,
    wings = wings,
    bigs = bigs,
    utility = utils,
    balanced = balanced,
    review_required = review_required
  )
}


# ------------------------------------------------------------
# Player preparation
# ------------------------------------------------------------

prepare_lineup_player_pool <- function(roster_or_allocation) {
  if (is.list(roster_or_allocation) &&
      "allocation" %in% names(roster_or_allocation)) {
    roster_or_allocation <- roster_or_allocation$allocation
  }
  
  if (is.null(roster_or_allocation)) {
    roster_or_allocation <- data.frame()
  }
  
  if (!is.data.frame(roster_or_allocation)) {
    stop(
      "roster_or_allocation must be a data frame or Phase 8 allocation result.",
      call. = FALSE
    )
  }
  
  if (nrow(roster_or_allocation) == 0L) {
    return(
      data.frame(
        player_id = integer(),
        player_name = character(),
        position = character(),
        availability_status = character(),
        recommended_minutes = numeric(),
        bie_rating = numeric(),
        offensive_impact = numeric(),
        defensive_impact = numeric(),
        creation_score = numeric(),
        spacing_score = numeric(),
        rebounding_score = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  player_id <- lineup_first_existing(
    roster_or_allocation,
    c("player_id", "PLAYER_ID", "id"),
    default = NA_integer_
  )
  
  player_name <- lineup_first_existing(
    roster_or_allocation,
    c("player_name", "PLAYER_NAME", "name"),
    default = ""
  )
  
  position <- lineup_first_existing(
    roster_or_allocation,
    c("position", "assigned_position", "Pos", "pos"),
    default = "UTIL"
  )
  
  availability <- lineup_first_existing(
    roster_or_allocation,
    c(
      "availability_status",
      "availability",
      "status"
    ),
    default = "AVAILABLE"
  )
  
  recommended_minutes <- lineup_first_existing(
    roster_or_allocation,
    c(
      "recommended_minutes",
      "current_minutes",
      "minutes_per_game",
      "mpg"
    ),
    default = 0
  )
  
  bie <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_player_score",
      "bie_vnext_player_score",
      "bie_selection_score",
      "bie_rating",
      "latest_bie_rating",
      "projected_bie_rating",
      "BIE"
    ),
    default = 50
  )
  
  offense <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_offense_score",
      "offensive_impact",
      "offensive_impact_score",
      "offense_score",
      "offensive_rating",
      "offensive_value"
    ),
    default = NA_real_
  )
  
  defense <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_defense_score",
      "defensive_impact",
      "defensive_impact_score",
      "defense_score",
      "defensive_rating",
      "defensive_value"
    ),
    default = NA_real_
  )
  
  creation <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_playmaking_score",
      "bie_creation_score",
      "creation_score",
      "creation_rating",
      "playmaking_score",
      "playmaking_rating"
    ),
    default = NA_real_
  )
  
  spacing <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_spacing_score",
      "spacing_score",
      "shooting_score",
      "shooting_spacing_score",
      "shooting_rating"
    ),
    default = NA_real_
  )
  
  rebounding <- lineup_first_existing(
    roster_or_allocation,
    c(
      "bie_rebounding_score",
      "rebounding_score",
      "rebounding_rating",
      "rebound_score"
    ),
    default = NA_real_
  )
  
  out <- data.frame(
    player_id = suppressWarnings(as.integer(player_id)),
    player_name = as.character(player_name),
    position = vapply(
      position,
      normalize_lineup_position,
      character(1)
    ),
    availability_status = toupper(
      as.character(availability)
    ),
    recommended_minutes = suppressWarnings(
      as.numeric(recommended_minutes)
    ),
    bie_rating = suppressWarnings(
      as.numeric(bie)
    ),
    offensive_impact = suppressWarnings(
      as.numeric(offense)
    ),
    defensive_impact = suppressWarnings(
      as.numeric(defense)
    ),
    creation_score = suppressWarnings(
      as.numeric(creation)
    ),
    spacing_score = suppressWarnings(
      as.numeric(spacing)
    ),
    rebounding_score = suppressWarnings(
      as.numeric(rebounding)
    ),
    stringsAsFactors = FALSE
  )
  
  out$recommended_minutes[
    !is.finite(out$recommended_minutes)
  ] <- 0
  
  out$bie_rating[
    !is.finite(out$bie_rating)
  ] <- 50
  
  fill_metric <- function(metric, fallback) {
    metric[!is.finite(metric)] <- fallback[!is.finite(metric)]
    metric
  }
  
  out$offensive_impact <- fill_metric(
    out$offensive_impact,
    out$bie_rating
  )
  
  out$defensive_impact <- fill_metric(
    out$defensive_impact,
    out$bie_rating
  )
  
  out$creation_score <- fill_metric(
    out$creation_score,
    out$offensive_impact
  )
  
  out$spacing_score <- fill_metric(
    out$spacing_score,
    out$offensive_impact
  )
  
  out$rebounding_score <- fill_metric(
    out$rebounding_score,
    out$defensive_impact
  )
  
  numeric_fields <- c(
    "bie_rating",
    "offensive_impact",
    "defensive_impact",
    "creation_score",
    "spacing_score",
    "rebounding_score"
  )
  
  for (field in numeric_fields) {
    out[[field]] <- pmin(
      pmax(out[[field]], 0),
      100
    )
  }
  
  out
}


# ------------------------------------------------------------
# Candidate lineups
# ------------------------------------------------------------

get_lineup_candidate_pool <- function(roster_or_allocation,
                                      pool_size = NULL,
                                      rule_overrides = NULL) {
  rules <- resolve_lineup_optimization_rules(rule_overrides)
  pool <- prepare_lineup_player_pool(roster_or_allocation)
  
  if (nrow(pool) == 0L) {
    return(pool)
  }
  
  eligible <- pool[
    pool$availability_status != "OUT",
    ,
    drop = FALSE
  ]
  
  if (nrow(eligible) < 5L) {
    stop(
      "At least five available players are required for lineup optimization.",
      call. = FALSE
    )
  }
  
  default_size <- min(
    rules$maximum_pool_size,
    nrow(eligible)
  )
  
  pool_size <- lineup_integer(
    pool_size,
    default_size
  )
  
  pool_size <- min(
    max(pool_size, rules$minimum_pool_size),
    rules$maximum_pool_size,
    nrow(eligible)
  )
  
  ordering <- order(
    -eligible$recommended_minutes,
    -eligible$bie_rating,
    eligible$player_name
  )
  
  eligible <- eligible[
    head(ordering, pool_size),
    ,
    drop = FALSE
  ]
  
  rownames(eligible) <- NULL
  
  eligible
}


enumerate_lineup_candidates <- function(player_pool,
                                        rule_overrides = NULL) {
  rules <- resolve_lineup_optimization_rules(rule_overrides)
  
  if (!is.data.frame(player_pool)) {
    stop("player_pool must be a data frame.", call. = FALSE)
  }
  
  if (nrow(player_pool) < 5L) {
    stop(
      "At least five players are required to enumerate lineups.",
      call. = FALSE
    )
  }
  
  combos <- utils::combn(
    seq_len(nrow(player_pool)),
    5
  )
  
  if (ncol(combos) > rules$maximum_candidate_lineups) {
    combos <- combos[
      ,
      seq_len(rules$maximum_candidate_lineups),
      drop = FALSE
    ]
  }
  
  lapply(
    seq_len(ncol(combos)),
    function(i) {
      player_pool[
        combos[, i],
        ,
        drop = FALSE
      ]
    }
  )
}


# ------------------------------------------------------------
# Lineup scoring
# ------------------------------------------------------------

lineup_metric_mean <- function(lineup, field) {
  if (!field %in% names(lineup)) {
    return(50)
  }
  
  values <- suppressWarnings(
    as.numeric(lineup[[field]])
  )
  
  values <- values[
    is.finite(values)
  ]
  
  if (length(values) == 0L) {
    return(50)
  }
  
  mean(values)
}


lineup_minutes_score <- function(lineup) {
  values <- suppressWarnings(
    as.numeric(
      lineup$recommended_minutes
    )
  )
  
  values[!is.finite(values)] <- 0
  
  max_minutes <- max(values, 0)
  
  if (max_minutes <= 0) {
    return(50)
  }
  
  mean(
    pmin(
      pmax(
        values / max_minutes * 100,
        0
      ),
      100
    )
  )
}


score_lineup <- function(lineup,
                         lineup_type = c(
                           "balanced",
                           "offense",
                           "defense",
                           "closing"
                         ),
                         rule_overrides = NULL) {
  rules <- resolve_lineup_optimization_rules(rule_overrides)
  
  lineup_type <- match.arg(lineup_type)
  
  if (!is.data.frame(lineup) || nrow(lineup) != 5L) {
    stop(
      "lineup must be a five-row data frame.",
      call. = FALSE
    )
  }
  
  bie <- lineup_metric_mean(
    lineup,
    "bie_rating"
  )
  
  offense <- lineup_metric_mean(
    lineup,
    "offensive_impact"
  )
  
  defense <- lineup_metric_mean(
    lineup,
    "defensive_impact"
  )
  
  creation <- lineup_metric_mean(
    lineup,
    "creation_score"
  )
  
  spacing <- lineup_metric_mean(
    lineup,
    "spacing_score"
  )
  
  rebounding <- lineup_metric_mean(
    lineup,
    "rebounding_score"
  )
  
  minutes <- lineup_minutes_score(lineup)
  
  score <- switch(
    lineup_type,
    
    "balanced" =
      rules$balanced_offense_weight * offense +
      rules$balanced_defense_weight * defense +
      rules$balanced_bie_weight * bie +
      rules$balanced_creation_weight * creation +
      rules$balanced_spacing_weight * spacing +
      rules$balanced_rebounding_weight * rebounding,
    
    "offense" =
      rules$offense_offense_weight * offense +
      rules$offense_bie_weight * bie +
      rules$offense_creation_weight * creation +
      rules$offense_spacing_weight * spacing +
      rules$offense_defense_weight * defense +
      rules$offense_rebounding_weight * rebounding,
    
    "defense" =
      rules$defense_defense_weight * defense +
      rules$defense_bie_weight * bie +
      rules$defense_rebounding_weight * rebounding +
      rules$defense_offense_weight * offense +
      rules$defense_creation_weight * creation +
      rules$defense_spacing_weight * spacing,
    
    "closing" =
      rules$closing_bie_weight * bie +
      rules$closing_offense_weight * offense +
      rules$closing_defense_weight * defense +
      rules$closing_creation_weight * creation +
      rules$closing_spacing_weight * spacing +
      rules$closing_rebounding_weight * rebounding +
      rules$closing_minutes_weight * minutes
  )
  
  balance <- position_balance_evaluation(
    lineup$position
  )
  
  if (isTRUE(balance$balanced)) {
    score <- score +
      rules$position_balance_bonus
  }
  
  if (isTRUE(balance$review_required)) {
    score <- score -
      rules$position_review_penalty
  }
  
  unavailable <- any(
    lineup$availability_status == "OUT"
  )
  
  if (unavailable) {
    score <- score -
      rules$unavailable_penalty
  }
  
  list(
    score = lineup_clamp(score, 0, 100),
    lineup_type = lineup_type,
    bie = bie,
    offense = offense,
    defense = defense,
    creation = creation,
    spacing = spacing,
    rebounding = rebounding,
    minutes = minutes,
    position_balance = balance,
    unavailable = unavailable
  )
}


lineup_key <- function(lineup) {
  ids <- lineup$player_id
  
  if (all(!is.na(ids))) {
    return(
      paste(
        sort(ids),
        collapse = "-"
      )
    )
  }
  
  paste(
    sort(lineup$player_name),
    collapse = " | "
  )
}


summarize_lineup <- function(lineup,
                             score_result) {
  players <- paste(
    lineup$player_name,
    collapse = ", "
  )
  
  balance <- score_result$position_balance
  
  explanation <- paste0(
    tools::toTitleCase(
      score_result$lineup_type
    ),
    " lineup score ",
    round(score_result$score, 1),
    ". BIE ",
    round(score_result$bie, 1),
    ", offense ",
    round(score_result$offense, 1),
    ", defense ",
    round(score_result$defense, 1),
    ", creation ",
    round(score_result$creation, 1),
    ", spacing ",
    round(score_result$spacing, 1),
    ", rebounding ",
    round(score_result$rebounding, 1),
    "."
  )
  
  if (!isTRUE(balance$balanced)) {
    explanation <- paste(
      explanation,
      "Position balance requires review."
    )
  }
  
  list(
    lineup_key = lineup_key(lineup),
    lineup_type = score_result$lineup_type,
    score = score_result$score,
    players = lineup$player_name,
    player_ids = lineup$player_id,
    positions = lineup$position,
    recommended_minutes = lineup$recommended_minutes,
    bie = score_result$bie,
    offense = score_result$offense,
    defense = score_result$defense,
    creation = score_result$creation,
    spacing = score_result$spacing,
    rebounding = score_result$rebounding,
    position_balance = balance,
    requires_position_review = balance$review_required,
    explanation = explanation
  )
}


# ------------------------------------------------------------
# Optimizers
# ------------------------------------------------------------

optimize_lineup_type <- function(roster_or_allocation,
                                 lineup_type = c(
                                   "balanced",
                                   "offense",
                                   "defense",
                                   "closing"
                                 ),
                                 pool_size = NULL,
                                 rule_overrides = NULL) {
  lineup_type <- match.arg(lineup_type)
  
  pool <- get_lineup_candidate_pool(
    roster_or_allocation,
    pool_size = pool_size,
    rule_overrides = rule_overrides
  )
  
  candidates <- enumerate_lineup_candidates(
    pool,
    rule_overrides = rule_overrides
  )
  
  scores <- vapply(
    candidates,
    function(candidate) {
      score_lineup(
        candidate,
        lineup_type = lineup_type,
        rule_overrides = rule_overrides
      )$score
    },
    numeric(1)
  )
  
  best_index <- which.max(scores)
  
  best_lineup <- candidates[[best_index]]
  
  best_score <- score_lineup(
    best_lineup,
    lineup_type = lineup_type,
    rule_overrides = rule_overrides
  )
  
  summary <- summarize_lineup(
    best_lineup,
    best_score
  )
  
  summary$candidate_count <-
    length(candidates)
  
  summary
}


build_lineup_optimization <- function(roster_or_allocation,
                                      pool_size = NULL,
                                      rule_overrides = NULL) {
  rules <- resolve_lineup_optimization_rules(rule_overrides)
  
  balanced <- optimize_lineup_type(
    roster_or_allocation,
    lineup_type = "balanced",
    pool_size = pool_size,
    rule_overrides = rule_overrides
  )
  
  offense <- optimize_lineup_type(
    roster_or_allocation,
    lineup_type = "offense",
    pool_size = pool_size,
    rule_overrides = rule_overrides
  )
  
  defense <- optimize_lineup_type(
    roster_or_allocation,
    lineup_type = "defense",
    pool_size = pool_size,
    rule_overrides = rule_overrides
  )
  
  closing <- optimize_lineup_type(
    roster_or_allocation,
    lineup_type = "closing",
    pool_size = pool_size,
    rule_overrides = rule_overrides
  )
  
  list(
    balanced = balanced,
    offense = offense,
    defense = defense,
    closing = closing,
    model_label = rules$model_label
  )
}


lineup_optimization_table <- function(result) {
  if (!is.list(result)) {
    stop(
      "result must be a build_lineup_optimization result.",
      call. = FALSE
    )
  }
  
  types <- c(
    "balanced",
    "offense",
    "defense",
    "closing"
  )
  
  rows <- lapply(
    types,
    function(type) {
      item <- result[[type]]
      
      data.frame(
        lineup_type = tools::toTitleCase(type),
        score = round(
          item$score,
          1
        ),
        players = paste(
          item$players,
          collapse = " | "
        ),
        bie = round(item$bie, 1),
        offense = round(item$offense, 1),
        defense = round(item$defense, 1),
        creation = round(item$creation, 1),
        spacing = round(item$spacing, 1),
        rebounding = round(item$rebounding, 1),
        requires_position_review =
          item$requires_position_review,
        stringsAsFactors = FALSE
      )
    }
  )
  
  do.call(
    rbind,
    rows
  )
}


compare_lineup_optimization <- function(base_result,
                                        scenario_result) {
  base <- lineup_optimization_table(
    base_result
  )
  
  scenario <- lineup_optimization_table(
    scenario_result
  )
  
  merged <- merge(
    base,
    scenario,
    by = "lineup_type",
    suffixes = c(
      "_base",
      "_scenario"
    ),
    all = TRUE
  )
  
  merged$score_change <-
    merged$score_scenario -
    merged$score_base
  
  merged$lineup_changed <-
    merged$players_base !=
    merged$players_scenario
  
  merged
}

# TBI_VNEXT_PHASE9_FIELD_COMPAT

# >>> TBI_V1_PRESEASON_PHASE9_START >>>
# V1.0 preseason candidate eligibility.
tbi_preseason_rookie_gate_active <- function (season = "2026-27") 
{
    con <- tryCatch(connect_db(read_only = TRUE), error = function(e) {
        connect_db()
    })
    on.exit(disconnect_db(con), add = TRUE)
    if (!"player_season_stats" %in% DBI::dbListTables(con)) {
        return(TRUE)
    }
    fields <- DBI::dbListFields(con, "player_season_stats")
    if (!"season" %in% fields) {
        return(TRUE)
    }
    game_filter <- if ("games_played" %in% fields) {
        " AND COALESCE(games_played, 0) > 0 "
    }
    else {
        ""
    }
    result <- tryCatch(DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n ", "FROM player_season_stats ", "WHERE season = ? ", game_filter), params = list(season)), error = function(e) {
        data.frame(n = 0L)
    })
    if (!nrow(result)) {
        return(TRUE)
    }
    rows <- suppressWarnings(as.integer(result$n[[1]]))
    is.na(rows) || rows == 0L
}

tbi_v1_lineup_pool_base <- prepare_lineup_player_pool

prepare_lineup_player_pool <- function (roster_or_allocation) 
{
    raw <- if (is.list(roster_or_allocation) && "allocation" %in% names(roster_or_allocation)) {
        roster_or_allocation$allocation
    }
    else {
        roster_or_allocation
    }
    out <- tbi_v1_lineup_pool_base(roster_or_allocation)
    if (!is.data.frame(raw) || !is.data.frame(out) || !nrow(raw) || !nrow(out)) {
        out$is_starter <- rep(FALSE, nrow(out))
        out$is_preseason_rookie <- rep(FALSE, nrow(out))
        return(out)
    }
    raw_id <- suppressWarnings(as.integer(raw$player_id))
    out_id <- suppressWarnings(as.integer(out$player_id))
    match_index <- match(out_id, raw_id)
    starter_raw <- if ("is_starter" %in% names(raw)) {
        vapply(seq_len(nrow(raw)), function(i) {
            tbi_v1_flag(raw$is_starter[i])
        }, logical(1))
    }
    else {
        rep(FALSE, nrow(raw))
    }
    rookie_raw <- if ("is_preseason_rookie" %in% names(raw)) {
        vapply(seq_len(nrow(raw)), function(i) {
            tbi_v1_flag(raw$is_preseason_rookie[i])
        }, logical(1))
    }
    else {
        rep(FALSE, nrow(raw))
    }
    out$is_starter <- FALSE
    out$is_preseason_rookie <- FALSE
    valid <- !is.na(match_index)
    out$is_starter[valid] <- starter_raw[match_index[valid]]
    out$is_preseason_rookie[valid] <- rookie_raw[match_index[valid]]
    out
}

get_lineup_candidate_pool <- function (roster_or_allocation, pool_size = NULL, rule_overrides = NULL) 
{
    rules <- resolve_lineup_optimization_rules(rule_overrides)
    pool <- prepare_lineup_player_pool(roster_or_allocation)
    if (!nrow(pool)) {
        return(pool)
    }
    eligible <- pool[pool$availability_status != "OUT", , drop = FALSE]
    preseason_active <- tbi_preseason_rookie_gate_active("2026-27")
    if (isTRUE(preseason_active)) {
        eligible <- eligible[!(eligible$is_preseason_rookie & !eligible$is_starter), , drop = FALSE]
    }
    if (nrow(eligible) < 5L) {
        stop(paste("Fewer than five eligible players remain", "after preseason rookie gating."), call. = FALSE)
    }
    default_size <- min(rules$maximum_pool_size, nrow(eligible))
    pool_size <- lineup_integer(pool_size, default_size)
    pool_size <- min(max(pool_size, rules$minimum_pool_size), rules$maximum_pool_size, nrow(eligible))
    ordering <- order(-as.integer(eligible$is_starter), -eligible$recommended_minutes, -eligible$bie_rating, eligible$player_name)
    eligible <- eligible[head(ordering, pool_size), , drop = FALSE]
    rownames(eligible) <- NULL
    eligible
}
# <<< TBI_V1_PRESEASON_PHASE9_END <<<

