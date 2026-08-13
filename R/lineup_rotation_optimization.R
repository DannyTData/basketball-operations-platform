# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 9
# Lineup + Rotation Optimization
#
# Purpose:
#   Use the real Phase-3 player impact / role intelligence to
#   recommend lineups and a rotation.
#
# Outputs:
#   - Best Starting Five
#   - Best Closing Five
#   - Best Offensive Five
#   - Best Defensive Five
#   - Best Balanced Five
#   - Regulation Rotation (240 minutes)
#
# Inputs:
#   - player_season_impact
#   - player_season_roles
#   - players
#   - roster_history / teams when available
#
# Safeguards:
#   - respects listed primary positions
#   - uses only loaded evidence
#   - no opponent-specific matchup claims yet
#   - no tracking-specific lineup claims yet
# ============================================================


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s9_num <- function(
    x,
    default = NA_real_) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !is.finite(value[[1]])
  ) {
    return(default)
  }
  
  value[[1]]
}


p3s9_text <- function(
    x,
    default = "") {
  
  value <- as.character(x)
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !nzchar(
      trimws(
        value[[1]]
      )
    )
  ) {
    return(default)
  }
  
  trimws(
    value[[1]]
  )
}


p3s9_weighted_mean <- function(
    values,
    weights) {
  
  values <- suppressWarnings(
    as.numeric(values)
  )
  
  weights <- suppressWarnings(
    as.numeric(weights)
  )
  
  valid <-
    is.finite(values) &
    is.finite(weights) &
    weights > 0
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  sum(
    values[valid] *
      weights[valid]
  ) /
    sum(
      weights[valid]
    )
}


# ------------------------------------------------------------
# Position normalization
# ------------------------------------------------------------

p3s9_normalize_position <- function(position) {
  
  position <- toupper(
    trimws(
      as.character(
        position
      )
    )
  )
  
  if (!length(position)) {
    return("")
  }
  
  position <- position[[1]]
  
  aliases <- c(
    "POINT GUARD" = "PG",
    "SHOOTING GUARD" = "SG",
    "SMALL FORWARD" = "SF",
    "POWER FORWARD" = "PF",
    "CENTER" = "C",
    "G" = "G",
    "F" = "F"
  )
  
  if (
    position %in%
    names(aliases)
  ) {
    return(
      aliases[[position]]
    )
  }
  
  position
}


p3s9_position_eligibility <- function(position) {
  
  p <- p3s9_normalize_position(
    position
  )
  
  if (
    is.na(p) ||
    !nzchar(p)
  ) {
    return(
      c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      )
    )
  }
  
  if (p == "PG") {
    return(
      c(
        "PG",
        "SG"
      )
    )
  }
  
  if (p == "SG") {
    return(
      c(
        "SG",
        "PG",
        "SF"
      )
    )
  }
  
  if (p == "SF") {
    return(
      c(
        "SF",
        "SG",
        "PF"
      )
    )
  }
  
  if (p == "PF") {
    return(
      c(
        "PF",
        "SF",
        "C"
      )
    )
  }
  
  if (p == "C") {
    return(
      c(
        "C",
        "PF"
      )
    )
  }
  
  if (p == "G") {
    return(
      c(
        "PG",
        "SG"
      )
    )
  }
  
  if (p == "F") {
    return(
      c(
        "SF",
        "PF"
      )
    )
  }
  
  c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
}


# ------------------------------------------------------------
# Read current team-season player impact
# ------------------------------------------------------------

# ------------------------------------------------------------
# Resolve requested team to canonical team_id
# ------------------------------------------------------------

p3s9_normalize_team_text <- function(x) {
  
  x <- toupper(
    trimws(
      as.character(x)
    )
  )
  
  x <- gsub(
    "[^A-Z0-9]",
    "",
    x
  )
  
  x
}


resolve_phase3_team <- function(
    team,
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name,
      abbreviation
    FROM teams
    ORDER BY team_name
    "
  )
  
  if (!nrow(teams)) {
    stop(
      "The teams table is empty."
    )
  }
  
  query_key <-
    p3s9_normalize_team_text(
      team
    )
  
  name_key <-
    p3s9_normalize_team_text(
      teams$team_name
    )
  
  abbr_key <-
    p3s9_normalize_team_text(
      teams$abbreviation
    )
  
  # Common NBA city / franchise aliases.
  aliases <- c(
    "ATLANTAHAWKS" = "ATL",
    "BOSTONCELTICS" = "BOS",
    "BROOKLYNNETS" = "BKN",
    "CHARLOTTEHORNETS" = "CHA",
    "CHICAGOBULLS" = "CHI",
    "CLEVELANDCAVALIERS" = "CLE",
    "DALLASMAVERICKS" = "DAL",
    "DENVERNUGGETS" = "DEN",
    "DETROITPISTONS" = "DET",
    "GOLDENSTATEWARRIORS" = "GSW",
    "HOUSTONROCKETS" = "HOU",
    "INDIANAPACERS" = "IND",
    "LACLIPPERS" = "LAC",
    "LOSANGELESCLIPPERS" = "LAC",
    "LALAKERS" = "LAL",
    "LOSANGELESLAKERS" = "LAL",
    "MEMPHISGRIZZLIES" = "MEM",
    "MIAMIHEAT" = "MIA",
    "MILWAUKEEBUCKS" = "MIL",
    "MINNESOTATIMBERWOLVES" = "MIN",
    "NEWORLEANSPELICANS" = "NOP",
    "NEWYORKKNICKS" = "NYK",
    "OKLAHOMACITYTHUNDER" = "OKC",
    "ORLANDOMAGIC" = "ORL",
    "PHILADELPHIA76ERS" = "PHI",
    "PHOENIXSUNS" = "PHX",
    "PORTLANDTRAILBLAZERS" = "POR",
    "SACRAMENTOKINGS" = "SAC",
    "SANANTONIOSPURS" = "SAS",
    "TORONTORAPTORS" = "TOR",
    "UTAHJAZZ" = "UTA",
    "WASHINGTONWIZARDS" = "WAS"
  )
  
  if (
    query_key %in%
    names(aliases)
  ) {
    
    alias_abbr <-
      unname(
        aliases[[query_key]]
      )
    
    query_key <-
      p3s9_normalize_team_text(
        alias_abbr
      )
  }
  
  exact <- which(
    name_key == query_key |
      abbr_key == query_key
  )
  
  if (length(exact)) {
    
    row <- teams[
      exact[[1]],
      ,
      drop = FALSE
    ]
    
    return(
      list(
        team_id =
          as.integer(
            row$team_id[[1]]
          ),
        team_name =
          as.character(
            row$team_name[[1]]
          ),
        abbreviation =
          as.character(
            row$abbreviation[[1]]
          ),
        match_method =
          "EXACT / ALIAS"
      )
    )
  }
  
  # Safe partial fallback for city/franchise-name differences.
  partial <- which(
    grepl(
      query_key,
      name_key,
      fixed = TRUE
    ) |
      grepl(
        name_key,
        query_key,
        fixed = TRUE
      )
  )
  
  if (length(partial) == 1L) {
    
    row <- teams[
      partial[[1]],
      ,
      drop = FALSE
    ]
    
    return(
      list(
        team_id =
          as.integer(
            row$team_id[[1]]
          ),
        team_name =
          as.character(
            row$team_name[[1]]
          ),
        abbreviation =
          as.character(
            row$abbreviation[[1]]
          ),
        match_method =
          "PARTIAL"
      )
    )
  }
  
  stop(
    paste0(
      "Could not uniquely resolve team '",
      team,
      "'. Available teams include: ",
      paste(
        utils::head(
          teams$team_name,
          10
        ),
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# Read current team-season player impact
# ------------------------------------------------------------

get_phase3_lineup_source <- function(
    team_name,
    season,
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  tables <- DBI::dbListTables(
    con
  )
  
  required <- c(
    "player_season_impact",
    "player_season_roles",
    "players",
    "teams"
  )
  
  missing <- setdiff(
    required,
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Step 9 is missing required tables: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  resolved_team <-
    resolve_phase3_team(
      team =
        team_name,
      con =
        con
    )
  
  roster_join <- if (
    "roster_history" %in%
    tables
  ) {
    "
    LEFT JOIN roster_history rh
      ON rh.player_id = impact.player_id
      AND rh.team_id = impact.team_id
      AND rh.season = impact.season
    "
  } else {
    ""
  }
  
  roster_fields <- if (
    "roster_history" %in%
    tables
  ) {
    "
      rh.roster_status,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag,
    "
  } else {
    "
      NULL AS roster_status,
      0 AS two_way_flag,
    "
  }
  
  sql <- paste0(
    "
    SELECT
      impact.player_id,
      impact.team_id,
      impact.season,
      p.player_name,
      p.primary_position,
      p.player_age,

      impact.games_played,
      impact.minutes,
      impact.minutes_per_game,

      impact.shooting_component,
      impact.creation_component,
      impact.defense_component,
      impact.rebounding_component,
      impact.role_component,
      impact.availability_component,
      impact.advanced_impact_component,

      impact.offensive_impact_score,
      impact.defensive_impact_score,
      impact.all_around_impact_score,

      impact.bie_performance_rating,
      impact.bie_performance_percentile,
      impact.impact_tier,
      impact.impact_confidence,

      role.primary_role,
      role.secondary_role,
      role.tertiary_role,
      role.archetype,
      role.role_family,

      role.spacing_role_score,
      role.creation_role_score,
      role.connector_role_score,
      role.defense_role_score,
      role.rebounding_role_score,
      role.interior_role_score,

      ",
    roster_fields,
    "

      t.team_name,
      t.abbreviation

    FROM player_season_impact impact

    INNER JOIN players p
      ON p.player_id = impact.player_id

    INNER JOIN teams t
      ON t.team_id = impact.team_id

    LEFT JOIN player_season_roles role
      ON role.player_id = impact.player_id
      AND role.team_id = impact.team_id
      AND role.season = impact.season

    ",
    roster_join,
    "

    WHERE impact.team_id = ?
      AND impact.season = ?

    ORDER BY
      impact.bie_performance_rating DESC,
      impact.minutes_per_game DESC,
      p.player_name
    "
  )
  
  result <- DBI::dbGetQuery(
    con,
    sql,
    params = list(
      resolved_team$team_id,
      as.character(
        season
      )
    )
  )
  
  attr(
    result,
    "resolved_team"
  ) <- resolved_team
  
  result
}


# ------------------------------------------------------------
# Player objective scores
# ------------------------------------------------------------

derive_p3s9_player_objectives <- function(
    players) {
  
  d <- players
  
  n <- nrow(d)
  
  d$starting_value <- rep(
    NA_real_,
    n
  )
  
  d$closing_value <- rep(
    NA_real_,
    n
  )
  
  d$offensive_value <- rep(
    NA_real_,
    n
  )
  
  d$defensive_value <- rep(
    NA_real_,
    n
  )
  
  d$balanced_value <- rep(
    NA_real_,
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    d$starting_value[[i]] <-
      p3s9_weighted_mean(
        c(
          d$bie_performance_rating[[i]],
          d$all_around_impact_score[[i]],
          d$availability_component[[i]]
        ),
        c(
          0.56,
          0.30,
          0.14
        )
      )
    
    d$closing_value[[i]] <-
      p3s9_weighted_mean(
        c(
          d$bie_performance_rating[[i]],
          d$offensive_impact_score[[i]],
          d$defensive_impact_score[[i]],
          d$creation_role_score[[i]],
          d$spacing_role_score[[i]]
        ),
        c(
          0.36,
          0.21,
          0.18,
          0.14,
          0.11
        )
      )
    
    d$offensive_value[[i]] <-
      p3s9_weighted_mean(
        c(
          d$offensive_impact_score[[i]],
          d$shooting_component[[i]],
          d$creation_component[[i]],
          d$spacing_role_score[[i]]
        ),
        c(
          0.42,
          0.22,
          0.24,
          0.12
        )
      )
    
    d$defensive_value[[i]] <-
      p3s9_weighted_mean(
        c(
          d$defensive_impact_score[[i]],
          d$defense_component[[i]],
          d$rebounding_component[[i]],
          d$interior_role_score[[i]]
        ),
        c(
          0.44,
          0.30,
          0.16,
          0.10
        )
      )
    
    d$balanced_value[[i]] <-
      p3s9_weighted_mean(
        c(
          d$bie_performance_rating[[i]],
          d$offensive_impact_score[[i]],
          d$defensive_impact_score[[i]],
          d$role_component[[i]]
        ),
        c(
          0.34,
          0.25,
          0.25,
          0.16
        )
      )
  }
  
  d
}


# ------------------------------------------------------------
# Enumerate valid five-man lineups
# ------------------------------------------------------------

p3s9_valid_lineups <- function(
    players,
    max_candidates = 12L) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    nrow(players) < 5
  ) {
    return(
      list()
    )
  }
  
  d <- players[
    order(
      players$bie_performance_rating,
      decreasing = TRUE,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  d <- utils::head(
    d,
    max_candidates
  )
  
  combos <- utils::combn(
    seq_len(
      nrow(d)
    ),
    5,
    simplify = FALSE
  )
  
  lapply(
    combos,
    function(idx) {
      d[
        idx,
        ,
        drop = FALSE
      ]
    }
  )
}


# ------------------------------------------------------------
# Position feasibility for a five
# ------------------------------------------------------------

p3s9_lineup_position_feasible <- function(
    lineup) {
  
  slots <- c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
  
  if (
    is.null(lineup) ||
    !is.data.frame(lineup) ||
    nrow(lineup) != 5
  ) {
    return(FALSE)
  }
  
  eligible <- lapply(
    lineup$primary_position,
    p3s9_position_eligibility
  )
  
  # Small brute-force assignment over 5! = 120 permutations.
  permute <- function(x) {
    if (length(x) == 1) {
      return(
        list(x)
      )
    }
    
    out <- list()
    
    for (
      i in seq_along(x)
    ) {
      
      rest <- x[-i]
      
      sub <- Recall(
        rest
      )
      
      out <- c(
        out,
        lapply(
          sub,
          function(y) {
            c(
              x[[i]],
              y
            )
          }
        )
      )
    }
    
    out
  }
  
  for (
    assignment in permute(
      slots
    )
  ) {
    
    ok <- TRUE
    
    for (
      i in seq_len(5)
    ) {
      
      if (
        !assignment[[i]] %in%
        eligible[[i]]
      ) {
        ok <- FALSE
        break
      }
    }
    
    if (ok) {
      return(TRUE)
    }
  }
  
  FALSE
}


# ------------------------------------------------------------
# Lineup balance bonuses
# ------------------------------------------------------------

p3s9_lineup_balance_bonus <- function(
    lineup) {
  
  spacing <- mean(
    suppressWarnings(
      as.numeric(
        lineup$spacing_role_score
      )
    ),
    na.rm = TRUE
  )
  
  creation <- mean(
    suppressWarnings(
      as.numeric(
        lineup$creation_role_score
      )
    ),
    na.rm = TRUE
  )
  
  defense <- mean(
    suppressWarnings(
      as.numeric(
        lineup$defense_role_score
      )
    ),
    na.rm = TRUE
  )
  
  rebounding <- mean(
    suppressWarnings(
      as.numeric(
        lineup$rebounding_role_score
      )
    ),
    na.rm = TRUE
  )
  
  connector <- mean(
    suppressWarnings(
      as.numeric(
        lineup$connector_role_score
      )
    ),
    na.rm = TRUE
  )
  
  values <- c(
    spacing,
    creation,
    defense,
    rebounding,
    connector
  )
  
  values[
    !is.finite(values)
  ] <- 50
  
  # Reward balance; punish a fatal weakness.
  mean_component <- mean(
    values
  )
  
  weakest <- min(
    values
  )
  
  bonus <-
    0.65 *
    mean_component +
    0.35 *
    weakest
  
  bonus
}


# ------------------------------------------------------------
# Score a lineup for objective
# ------------------------------------------------------------

p3s9_score_lineup <- function(
    lineup,
    objective = c(
      "starting",
      "closing",
      "offense",
      "defense",
      "balanced"
    )) {
  
  objective <- match.arg(
    objective
  )
  
  value_column <- switch(
    objective,
    starting = "starting_value",
    closing = "closing_value",
    offense = "offensive_value",
    defense = "defensive_value",
    balanced = "balanced_value"
  )
  
  player_score <- mean(
    suppressWarnings(
      as.numeric(
        lineup[[value_column]]
      )
    ),
    na.rm = TRUE
  )
  
  if (!is.finite(player_score)) {
    player_score <- 0
  }
  
  balance_bonus <-
    p3s9_lineup_balance_bonus(
      lineup
    )
  
  # Objective-specific team composition emphasis.
  spacing <- mean(
    lineup$spacing_role_score,
    na.rm = TRUE
  )
  
  creation <- mean(
    lineup$creation_role_score,
    na.rm = TRUE
  )
  
  defense <- mean(
    lineup$defense_role_score,
    na.rm = TRUE
  )
  
  rebounding <- mean(
    lineup$rebounding_role_score,
    na.rm = TRUE
  )
  
  replace_na <- function(x) {
    if (
      length(x) == 0 ||
      !is.finite(x)
    ) {
      50
    } else {
      x
    }
  }
  
  spacing <- replace_na(
    spacing
  )
  
  creation <- replace_na(
    creation
  )
  
  defense <- replace_na(
    defense
  )
  
  rebounding <- replace_na(
    rebounding
  )
  
  composition <- switch(
    objective,
    starting =
      p3s9_weighted_mean(
        c(
          balance_bonus,
          spacing,
          creation,
          defense,
          rebounding
        ),
        c(
          .36,
          .16,
          .16,
          .18,
          .14
        )
      ),
    closing =
      p3s9_weighted_mean(
        c(
          balance_bonus,
          spacing,
          creation,
          defense
        ),
        c(
          .28,
          .24,
          .26,
          .22
        )
      ),
    offense =
      p3s9_weighted_mean(
        c(
          spacing,
          creation,
          balance_bonus
        ),
        c(
          .38,
          .40,
          .22
        )
      ),
    defense =
      p3s9_weighted_mean(
        c(
          defense,
          rebounding,
          balance_bonus
        ),
        c(
          .48,
          .28,
          .24
        )
      ),
    balanced =
      balance_bonus
  )
  
  p3s9_weighted_mean(
    c(
      player_score,
      composition
    ),
    c(
      0.78,
      0.22
    )
  )
}


# ------------------------------------------------------------
# Optimize one lineup objective
# ------------------------------------------------------------

optimize_phase3_lineup <- function(
    players,
    objective = c(
      "starting",
      "closing",
      "offense",
      "defense",
      "balanced"
    ),
    max_candidates = 12L) {
  
  objective <- match.arg(
    objective
  )
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    nrow(players) < 5
  ) {
    return(
      list(
        status =
          "INSUFFICIENT PLAYERS",
        lineup =
          data.frame(),
        score =
          NA_real_
      )
    )
  }
  
  candidates <-
    p3s9_valid_lineups(
      players,
      max_candidates =
        max_candidates
    )
  
  if (!length(candidates)) {
    return(
      list(
        status =
          "NO CANDIDATES",
        lineup =
          data.frame(),
        score =
          NA_real_
      )
    )
  }
  
  scored <- lapply(
    candidates,
    function(lineup) {
      
      feasible <-
        p3s9_lineup_position_feasible(
          lineup
        )
      
      score <- if (
        feasible
      ) {
        p3s9_score_lineup(
          lineup,
          objective =
            objective
        )
      } else {
        -Inf
      }
      
      list(
        lineup =
          lineup,
        feasible =
          feasible,
        score =
          score
      )
    }
  )
  
  scores <- vapply(
    scored,
    function(x) {
      x$score
    },
    numeric(1)
  )
  
  if (
    !any(
      is.finite(
        scores
      )
    )
  ) {
    return(
      list(
        status =
          "NO POSITION-FEASIBLE LINEUP",
        lineup =
          data.frame(),
        score =
          NA_real_
      )
    )
  }
  
  best <- which.max(
    scores
  )
  
  result <- scored[[best]]
  
  result$status <- "OK"
  result$objective <- objective
  
  result
}


# ------------------------------------------------------------
# Rotation minute allocation
# ------------------------------------------------------------

build_phase3_rotation <- function(
    players,
    rotation_size = 10L,
    total_minutes = 240) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players)
  ) {
    return(
      data.frame()
    )
  }
  
  rotation_size <- min(
    as.integer(
      rotation_size
    ),
    nrow(players)
  )
  
  d <- players[
    order(
      players$bie_performance_rating,
      players$minutes_per_game,
      decreasing = TRUE,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  d <- utils::head(
    d,
    rotation_size
  )
  
  score <- suppressWarnings(
    as.numeric(
      d$bie_performance_rating
    )
  )
  
  historical_minutes <- suppressWarnings(
    as.numeric(
      d$minutes_per_game
    )
  )
  
  score[
    !is.finite(score)
  ] <- 40
  
  historical_minutes[
    !is.finite(
      historical_minutes
    )
  ] <- 12
  
  # Blend performance with demonstrated workload.
  workload_signal <-
    0.68 *
    score +
    0.32 *
    pmin(
      100,
      historical_minutes /
        36 *
        100
    )
  
  # Convert to practical NBA rotation minutes.
  min_floor <- ifelse(
    seq_len(
      nrow(d)
    ) <= 5,
    24,
    8
  )
  
  raw_minutes <-
    min_floor +
    (
      workload_signal /
        sum(
          workload_signal
        )
    ) *
    (
      total_minutes -
        sum(
          min_floor
        )
    )
  
  raw_minutes <- pmin(
    raw_minutes,
    38
  )
  
  # Rebalance exactly to 240.
  difference <-
    total_minutes -
    sum(
      raw_minutes
    )
  
  if (
    abs(difference) >
    0.01
  ) {
    
    order_idx <- order(
      workload_signal,
      decreasing = TRUE
    )
    
    for (
      idx in order_idx
    ) {
      
      if (
        abs(difference) <=
        0.01
      ) {
        break
      }
      
      room <- if (
        difference > 0
      ) {
        40 -
          raw_minutes[[idx]]
      } else {
        raw_minutes[[idx]] -
          6
      }
      
      change <- sign(
        difference
      ) *
        min(
          abs(
            difference
          ),
          abs(
            room
          )
        )
      
      raw_minutes[[idx]] <-
        raw_minutes[[idx]] +
        change
      
      difference <-
        total_minutes -
        sum(
          raw_minutes
        )
    }
  }
  
  d$recommended_minutes <-
    round(
      raw_minutes,
      1
    )
  
  d$rotation_rank <-
    seq_len(
      nrow(d)
    )
  
  keep <- c(
    "rotation_rank",
    "player_id",
    "player_name",
    "primary_position",
    "recommended_minutes",
    "bie_performance_rating",
    "impact_tier",
    "impact_confidence",
    "primary_role",
    "archetype",
    "offensive_impact_score",
    "defensive_impact_score"
  )
  
  d[
    ,
    intersect(
      keep,
      names(d)
    ),
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# Full Step-9 team optimization
# ------------------------------------------------------------

run_phase3_lineup_rotation_optimization <- function(
    team_name,
    season = "2025-26",
    max_candidates = 12L,
    rotation_size = 10L,
    con = NULL) {
  
  players <-
    get_phase3_lineup_source(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  if (!nrow(players)) {
    
    return(
      list(
        status =
          "NO TEAM PERFORMANCE DATA",
        team =
          team_name,
        season =
          season
      )
    )
  }
  
  players <-
    derive_p3s9_player_objectives(
      players
    )
  
  starting <-
    optimize_phase3_lineup(
      players,
      objective =
        "starting",
      max_candidates =
        max_candidates
    )
  
  closing <-
    optimize_phase3_lineup(
      players,
      objective =
        "closing",
      max_candidates =
        max_candidates
    )
  
  offense <-
    optimize_phase3_lineup(
      players,
      objective =
        "offense",
      max_candidates =
        max_candidates
    )
  
  defense <-
    optimize_phase3_lineup(
      players,
      objective =
        "defense",
      max_candidates =
        max_candidates
    )
  
  balanced <-
    optimize_phase3_lineup(
      players,
      objective =
        "balanced",
      max_candidates =
        max_candidates
    )
  
  rotation <-
    build_phase3_rotation(
      players,
      rotation_size =
        rotation_size,
      total_minutes =
        240
    )
  
  list(
    status =
      "OK",
    team =
      team_name,
    season =
      season,
    player_pool =
      players,
    starting_five =
      starting,
    closing_five =
      closing,
    offensive_five =
      offense,
    defensive_five =
      defense,
    balanced_five =
      balanced,
    rotation =
      rotation,
    matchup_scope =
      "TEAM-INTERNAL OPTIMIZATION",
    opponent_specific_status =
      "NOT ACTIVE — OPPONENT MATCHUP DATA NOT YET LOADED"
  )
}


# ------------------------------------------------------------
# Compact lineup display
# ------------------------------------------------------------

phase3_step9_lineup_table <- function(
    optimization_result,
    type = c(
      "starting",
      "closing",
      "offense",
      "defense",
      "balanced"
    )) {
  
  type <- match.arg(
    type
  )
  
  if (
    is.null(
      optimization_result
    ) ||
    !identical(
      optimization_result$status,
      "OK"
    )
  ) {
    return(
      data.frame()
    )
  }
  
  object <- switch(
    type,
    starting =
      optimization_result$
      starting_five,
    closing =
      optimization_result$
      closing_five,
    offense =
      optimization_result$
      offensive_five,
    defense =
      optimization_result$
      defensive_five,
    balanced =
      optimization_result$
      balanced_five
  )
  
  if (
    is.null(
      object$lineup
    ) ||
    !nrow(
      object$lineup
    )
  ) {
    return(
      data.frame()
    )
  }
  
  d <- object$lineup
  
  keep <- c(
    "player_name",
    "primary_position",
    "bie_performance_rating",
    "impact_tier",
    "primary_role",
    "archetype",
    "offensive_impact_score",
    "defensive_impact_score",
    "spacing_role_score",
    "creation_role_score"
  )
  
  d <- d[
    ,
    intersect(
      keep,
      names(d)
    ),
    drop = FALSE
  ]
  
  attr(
    d,
    "lineup_score"
  ) <- object$score
  
  d
}


# ------------------------------------------------------------
# Step 9 diagnostic
# ------------------------------------------------------------

phase3_step9_diagnostic <- function(
    team_name,
    season = "2025-26") {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  resolved <-
    resolve_phase3_team(
      team =
        team_name,
      con =
        con
    )
  
  impact_rows <-
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_impact
      WHERE team_id = ?
        AND season = ?
      ",
      params = list(
        resolved$team_id,
        season
      )
    )$n[[1]]
  
  role_rows <-
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_roles
      WHERE team_id = ?
        AND season = ?
      ",
      params = list(
        resolved$team_id,
        season
      )
    )$n[[1]]
  
  list(
    requested_team =
      team_name,
    resolved_team_name =
      resolved$team_name,
    abbreviation =
      resolved$abbreviation,
    team_id =
      resolved$team_id,
    match_method =
      resolved$match_method,
    season =
      season,
    impact_rows =
      impact_rows,
    role_rows =
      role_rows,
    status = if (
      impact_rows >= 5
    ) {
      "READY FOR OPTIMIZATION"
    } else {
      "INSUFFICIENT TEAM IMPACT ROWS"
    }
  )
}


# ------------------------------------------------------------
# Step 9 health check
# ------------------------------------------------------------

phase3_step9_healthcheck <- function(
    team_name,
    season = "2025-26") {
  
  result <-
    run_phase3_lineup_rotation_optimization(
      team_name =
        team_name,
      season =
        season
    )
  
  if (
    !identical(
      result$status,
      "OK"
    )
  ) {
    
    return(
      list(
        phase =
          "Phase 3",
        step =
          "Step 9 — Lineup + Rotation Optimization",
        status =
          "REVIEW",
        issue =
          result$status
      )
    )
  }
  
  lineup_ready <- c(
    starting =
      identical(
        result$
          starting_five$
          status,
        "OK"
      ),
    closing =
      identical(
        result$
          closing_five$
          status,
        "OK"
      ),
    offense =
      identical(
        result$
          offensive_five$
          status,
        "OK"
      ),
    defense =
      identical(
        result$
          defensive_five$
          status,
        "OK"
      ),
    balanced =
      identical(
        result$
          balanced_five$
          status,
        "OK"
      )
  )
  
  rotation_minutes <- if (
    nrow(
      result$rotation
    )
  ) {
    sum(
      result$
        rotation$
        recommended_minutes
    )
  } else {
    0
  }
  
  list(
    phase =
      "Phase 3",
    step =
      "Step 9 — Lineup + Rotation Optimization",
    status = if (
      all(
        lineup_ready
      ) &&
      abs(
        rotation_minutes -
        240
      ) <=
      1
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    team =
      team_name,
    season =
      season,
    lineups_ready =
      lineup_ready,
    rotation_players =
      nrow(
        result$rotation
      ),
    rotation_minutes =
      rotation_minutes,
    matchup_scope =
      result$matchup_scope,
    opponent_specific_status =
      result$opponent_specific_status
  )
}


# ============================================================
# PHASE 3.1 OVERRIDE — CURRENT ROSTER / PRIOR PERFORMANCE
# ============================================================

get_phase3_lineup_source <- function(
    team_name,
    season,
    con = NULL,
    current_roster_season = "2026-27") {
  
  if (
    !exists(
      "get_tbi_current_roster_evaluation_context",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Current roster evaluation context is not loaded."
    )
  }
  
  players <- get_tbi_current_roster_evaluation_context(
    team_name = team_name,
    current_roster_season = current_roster_season,
    performance_season = season,
    con = con
  )
  
  if (!nrow(players)) {
    return(players)
  }
  
  # The optimizer works on the CURRENT roster.
  # Performance fields may come from the prior team.
  players$team_name <- players$current_team_name
  players$abbreviation <- players$current_team_abbreviation
  players$team_id <- players$current_team_id
  
  players
}


run_phase3_lineup_rotation_optimization <- function(
    team_name,
    season = "2025-26",
    max_candidates = 12L,
    rotation_size = 10L,
    con = NULL,
    current_roster_season = "2026-27") {
  
  players <- get_phase3_lineup_source(
    team_name = team_name,
    season = season,
    con = con,
    current_roster_season = current_roster_season
  )
  
  if (!nrow(players)) {
    return(
      list(
        status = "NO CURRENT ROSTER DATA",
        team = team_name,
        season = season,
        current_roster_season = current_roster_season
      )
    )
  }
  
  players <- derive_p3s9_player_objectives(players)
  
  starting <- optimize_phase3_lineup(
    players,
    objective = "starting",
    max_candidates = max_candidates
  )
  
  closing <- optimize_phase3_lineup(
    players,
    objective = "closing",
    max_candidates = max_candidates
  )
  
  offense <- optimize_phase3_lineup(
    players,
    objective = "offense",
    max_candidates = max_candidates
  )
  
  defense <- optimize_phase3_lineup(
    players,
    objective = "defense",
    max_candidates = max_candidates
  )
  
  balanced <- optimize_phase3_lineup(
    players,
    objective = "balanced",
    max_candidates = max_candidates
  )
  
  rotation <- build_phase3_rotation(
    players,
    rotation_size = rotation_size,
    total_minutes = 240
  )
  
  list(
    status = "OK",
    team = unique(players$current_team_name)[[1]],
    season = season,
    current_roster_season =
      unique(players$current_roster_season)[[1]],
    player_pool = players,
    starting_five = starting,
    closing_five = closing,
    offensive_five = offense,
    defensive_five = defense,
    balanced_five = balanced,
    rotation = rotation,
    matchup_scope = "CURRENT-ROSTER TEAM-INTERNAL OPTIMIZATION",
    evidence_scope =
      "PRIOR-SEASON NBA PERFORMANCE WHERE AVAILABLE",
    rookie_scope =
      "ROOKIES WITHOUT NBA EVIDENCE REMAIN UNRATED",
    opponent_specific_status =
      "NOT ACTIVE — OPPONENT MATCHUP DATA NOT YET LOADED"
  )
}


phase3_step9_healthcheck <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  result <- run_phase3_lineup_rotation_optimization(
    team_name = team_name,
    season = season,
    current_roster_season = current_roster_season
  )
  
  if (!identical(result$status, "OK")) {
    return(
      list(
        phase = "Phase 3.1",
        step = "Lineup + Rotation — Current Roster Context",
        status = "REVIEW",
        issue = result$status
      )
    )
  }
  
  lineup_ready <- c(
    starting = identical(result$starting_five$status, "OK"),
    closing = identical(result$closing_five$status, "OK"),
    offense = identical(result$offensive_five$status, "OK"),
    defense = identical(result$defensive_five$status, "OK"),
    balanced = identical(result$balanced_five$status, "OK")
  )
  
  rotation_minutes <- if (nrow(result$rotation)) {
    sum(result$rotation$recommended_minutes)
  } else {
    0
  }
  
  list(
    phase = "Phase 3.1",
    step = "Lineup + Rotation — Current Roster Context",
    status = if (
      all(lineup_ready) &&
      abs(rotation_minutes - 240) <= 1
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    team = result$team,
    current_roster_season = result$current_roster_season,
    performance_season = season,
    current_roster_players = nrow(result$player_pool),
    prior_nba_evidence_players =
      sum(result$player_pool$evaluation_source == "PRIOR-SEASON NBA"),
    rookie_or_pending_players =
      sum(result$player_pool$evaluation_source != "PRIOR-SEASON NBA"),
    lineups_ready = lineup_ready,
    rotation_players = nrow(result$rotation),
    rotation_minutes = rotation_minutes
  )
}