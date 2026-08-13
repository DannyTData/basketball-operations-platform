# ============================================================
# THOMPSON'S BASKETBALL INTELLIGENCE
# PHASE 3.3 — FINAL MODEL UPGRADE LAYER
# File: R/zzz_phase33_final_model_upgrade.R
#
# Requires the existing Phase 3.2/3.3 engine files to remain in R/.
# This file loads last and permanently overrides the calibrated
# Step 6 / Step 8 functions while adding five-season projections
# and a one-command rebuild runner.
# ============================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

p33_num <- function(x) suppressWarnings(as.numeric(x))

p33_pct <- function(x, higher_is_better = TRUE) {
  x <- p33_num(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  r <- rank(x[ok], ties.method = "average", na.last = "keep")
  p <- if (sum(ok) == 1) 50 else 100 * (r - 1) / (sum(ok) - 1)
  if (!higher_is_better) p <- 100 - p
  out[ok] <- p
  out
}

p33_wmean <- function(values, weights) {
  values <- p33_num(values); weights <- p33_num(weights)
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) return(NA_real_)
  sum(values[ok] * weights[ok]) / sum(weights[ok])
}

p33_clip <- function(x, lo = 0, hi = 100) pmax(lo, pmin(hi, x))

# ------------------------------------------------------------
# STEP 6 V4 — OFFICIAL DEFENSE + REBOUNDING CONTEXT
# ------------------------------------------------------------
# Capture the existing V3 derivation and improve its scoring.
.p33_step6_v3 <- derive_phase3_defense_rebounding_intelligence

derive_phase3_defense_rebounding_intelligence <- function(season_data) {
  d <- .p33_step6_v3(season_data)
  if (!is.data.frame(d) || !nrow(d)) return(d)
  
  # Official NBA advanced percentages are direct rebounding evidence.
  reb_pctile  <- p33_pct(d$rebound_pct)
  oreb_pctile <- p33_pct(d$offensive_rebound_pct)
  dreb_pctile <- p33_pct(d$defensive_rebound_pct)
  
  # Existing per-100 rebounding remains supporting evidence.
  per100_reb <- p33_pct(d$rebounds_per_100)
  per100_orb <- p33_pct(d$offensive_rebounds_per_100)
  per100_drb <- p33_pct(d$defensive_rebounds_per_100)
  
  # Rebuild rebounding score with official share metrics leading.
  for (i in seq_len(nrow(d))) {
    raw_reb <- p33_wmean(
      c(reb_pctile[i], dreb_pctile[i], oreb_pctile[i],
        per100_reb[i], per100_drb[i], per100_orb[i]),
      c(.34, .26, .12, .14, .10, .04)
    )
    if (is.finite(raw_reb)) d$rebounding_score[i] <- raw_reb
    
    # Defensive rating is contextual/team-influenced, so it is important
    # but never allowed to dominate individual defensive evaluation.
    drtg <- d$defensive_rating_percentile[i]
    dbpm <- d$defensive_bpm_percentile[i]
    disruption <- d$disruption_score[i]
    interior <- d$interior_impact_score[i]
    
    raw_def <- p33_wmean(
      c(drtg, dbpm, disruption, d$rebounding_score[i], interior),
      c(.30, .22, .20, .16, .12)
    )
    
    if (is.finite(raw_def)) d$defense_proxy_score[i] <- p33_clip(raw_def)
  }
  
  d$metric_version <- "P3S6_v4_OFFICIAL_DEF_REB_CONTEXT"
  d
}

phase3_step6_v4_healthcheck <- function(season = "2025-26") {
  s <- get_phase3_defense_rebounding_source(season = season)
  d <- derive_phase3_defense_rebounding_intelligence(s)
  nfinite <- function(x) sum(is.finite(p33_num(x)))
  list(
    phase = "Phase 3.3",
    step = "Step 6 — Official Defense + Rebounding V4",
    status = if (nrow(d) > 0 && nfinite(s$defensive_rating) > 0 &&
                 nfinite(s$rebound_pct) > 0) "READY FOR REBUILD" else "REVIEW",
    rows = nrow(d),
    defensive_rating_rows = nfinite(s$defensive_rating),
    rebound_pct_rows = nfinite(s$rebound_pct),
    defensive_rebound_pct_rows = nfinite(s$defensive_rebound_pct),
    offensive_rebound_pct_rows = nfinite(s$offensive_rebound_pct),
    metric_version = "P3S6_v4_OFFICIAL_DEF_REB_CONTEXT",
    rule = "OFFICIAL DEF RTG IS CONTEXT; OFFICIAL REB%/DREB%/OREB% LEAD REBOUNDING; BOX SCORE ACTIVITY SUPPORTS"
  )
}

# ------------------------------------------------------------
# STEP 8 V3 — OFFICIAL OFF/DEF/NET CONTEXT, NO HEAVY DOUBLE COUNT
# ------------------------------------------------------------
.p33_impact_source_v22 <- get_phase3_player_impact_source
.p33_impact_derive_v22 <- derive_phase3_player_impact

get_phase3_player_impact_source <- function(season, con = NULL) {
  d <- .p33_impact_source_v22(season = season, con = con)
  if (!is.data.frame(d) || !nrow(d)) return(d)
  
  owns <- is.null(con)
  if (owns) con <- connect_db(read_only = TRUE)
  if (owns) on.exit(disconnect_db(con), add = TRUE)
  
  if (!"player_season_advanced" %in% DBI::dbListTables(con)) return(d)
  a <- DBI::dbGetQuery(con, paste0(
    "SELECT player_id, team_id, season, usage_rate, assist_pct, ",
    "offensive_rating, defensive_rating, net_rating, official_possessions ",
    "FROM player_season_advanced WHERE season = ?"),
    params = list(as.character(season)))
  
  # Avoid duplicate net_rating if the old source already carried it.
  add_cols <- setdiff(names(a), names(d))
  keys <- c("player_id", "team_id", "season")
  if (length(add_cols)) {
    d <- merge(d, a[, c(keys, add_cols), drop = FALSE], by = keys,
               all.x = TRUE, sort = FALSE)
  }
  d
}

derive_phase3_player_impact <- function(source_data) {
  d <- .p33_impact_derive_v22(source_data)
  if (!is.data.frame(d) || !nrow(d)) return(d)
  
  keys <- c("player_id", "team_id", "season")
  idx <- match(
    paste(d$player_id, d$team_id, d$season, sep = "|"),
    paste(source_data$player_id, source_data$team_id, source_data$season, sep = "|")
  )
  
  off <- if ("offensive_rating" %in% names(source_data)) p33_num(source_data$offensive_rating[idx]) else rep(NA_real_, nrow(d))
  net <- if ("net_rating" %in% names(source_data)) p33_num(source_data$net_rating[idx]) else rep(NA_real_, nrow(d))
  poss <- if ("official_possessions" %in% names(source_data)) p33_num(source_data$official_possessions[idx]) else rep(NA_real_, nrow(d))
  
  offp <- p33_pct(off)
  netp <- p33_pct(net)
  
  # Small official context modifier only. Skill/production/Step-6 defense
  # remain the core BIE signal. This prevents OFF/DEF/NET from being
  # counted again as if they were independent box-score skills.
  context <- mapply(function(o, n) p33_wmean(c(o, n), c(.45, .55)), offp, netp)
  modifier <- ifelse(is.finite(context), (context - 50) * 0.10, 0)  # max ~ +/-5
  
  if ("bie_performance_rating" %in% names(d)) {
    d$bie_performance_rating <- p33_clip(p33_num(d$bie_performance_rating) + modifier)
    
    # Recompute league percentile after the context adjustment.
    gp <- if ("games_played" %in% names(d)) p33_num(d$games_played) else rep(NA_real_, nrow(d))
    mp <- if ("minutes_per_game" %in% names(d)) p33_num(d$minutes_per_game) else rep(NA_real_, nrow(d))
    qual <- is.finite(gp) & is.finite(mp) & gp >= 20 & mp >= 10
    pct <- rep(NA_real_, nrow(d))
    if (any(qual)) pct[qual] <- p33_pct(d$bie_performance_rating[qual])
    d$bie_performance_percentile <- pct
  }
  
  d$model_version <- "P3S8_v3_OFFICIAL_CONTEXT"
  d
}

phase3_step8_v3_healthcheck <- function(season = "2025-26") {
  s <- get_phase3_player_impact_source(season = season)
  d <- derive_phase3_player_impact(s)
  nfinite <- function(x) sum(is.finite(p33_num(x)))
  list(
    phase = "Phase 3.3",
    step = "Step 8 — BIE Official Context V3",
    status = if (nrow(d) > 0 && nfinite(d$bie_performance_rating) > 0 &&
                 nfinite(s$net_rating) > 0) "READY FOR REBUILD" else "REVIEW",
    rows = nrow(d),
    finite_ratings = nfinite(d$bie_performance_rating),
    offensive_rating_rows = if ("offensive_rating" %in% names(s)) nfinite(s$offensive_rating) else 0L,
    net_rating_rows = nfinite(s$net_rating),
    model_version = "P3S8_v3_OFFICIAL_CONTEXT",
    rule = "OFF/NET CONTEXT IS A SMALL MODIFIER; STEP-6 DEFENSE IS NOT DOUBLE-COUNTED"
  )
}

# ------------------------------------------------------------
# FIVE-SEASON TREND + PROJECTION ENGINE
# ------------------------------------------------------------
phase33_season_index <- function(season) suppressWarnings(as.integer(substr(season, 1, 4)))

phase34_age_year_effect <- function(
    projected_age,
    baseline) {
  
  age <- suppressWarnings(
    as.numeric(
      projected_age
    )
  )
  
  base <- suppressWarnings(
    as.numeric(
      baseline
    )
  )
  
  if (!is.finite(age)) {
    return(0)
  }
  
  # Young players get development opportunity, but the boost
  # shrinks as the player approaches the top of the scale.
  headroom <- if (
    is.finite(base)
  ) {
    max(
      0.25,
      min(
        1,
        (100 - base) /
          45
      )
    )
  } else {
    0.50
  }
  
  if (age <= 21) {
    return(
      2.6 *
        headroom
    )
  }
  
  if (age <= 24) {
    return(
      1.9 *
        headroom
    )
  }
  
  if (age <= 27) {
    return(
      1.0 *
        headroom
    )
  }
  
  if (age <= 30) {
    return(0)
  }
  
  if (age <= 32) {
    return(-0.7)
  }
  
  if (age <= 34) {
    return(-1.4)
  }
  
  if (age <= 36) {
    return(-2.3)
  }
  
  if (age <= 38) {
    return(-3.3)
  }
  
  if (age <= 40) {
    return(-4.8)
  }
  
  # Extreme-age seasons need a much stronger risk adjustment.
  -6.5
}


phase34_age_curve_adjustment <- function(
    current_age,
    years_forward,
    baseline) {
  
  age <- suppressWarnings(
    as.numeric(
      current_age
    )
  )
  
  years_forward <- suppressWarnings(
    as.integer(
      years_forward
    )
  )
  
  if (
    !is.finite(age) ||
    !is.finite(years_forward) ||
    years_forward <= 0
  ) {
    return(0)
  }
  
  effects <- vapply(
    seq_len(
      years_forward
    ),
    function(y) {
      phase34_age_year_effect(
        projected_age =
          age + y,
        baseline =
          baseline
      )
    },
    numeric(1)
  )
  
  sum(
    effects,
    na.rm = TRUE
  )
}


phase34_projection_trajectory <- function(
    current_age,
    one_year,
    three_year,
    annual_trend) {
  
  age <- suppressWarnings(
    as.numeric(
      current_age
    )
  )
  
  one_year <- suppressWarnings(
    as.numeric(
      one_year
    )
  )
  
  three_year <- suppressWarnings(
    as.numeric(
      three_year
    )
  )
  
  trend <- suppressWarnings(
    as.numeric(
      annual_trend
    )
  )
  
  if (
    is.finite(age) &&
    age <= 25 &&
    is.finite(three_year) &&
    is.finite(one_year) &&
    three_year >=
    one_year + 1
  ) {
    return(
      "PRIME ASCENT"
    )
  }
  
  if (
    is.finite(age) &&
    age >= 40
  ) {
    return(
      "LATE-CAREER DECLINE RISK"
    )
  }
  
  if (
    is.finite(three_year) &&
    is.finite(one_year) &&
    three_year <=
    one_year - 5
  ) {
    return(
      "DECLINING"
    )
  }
  
  if (
    is.finite(three_year) &&
    is.finite(one_year) &&
    three_year <=
    one_year - 2
  ) {
    return(
      "EARLY DECLINE"
    )
  }
  
  if (
    is.finite(three_year) &&
    is.finite(one_year) &&
    three_year >=
    one_year + 2
  ) {
    return(
      "RISING"
    )
  }
  
  if (
    is.finite(trend) &&
    abs(trend) >= 4
  ) {
    return(
      "HIGH-VARIANCE"
    )
  }
  
  "STABLE"
}


build_phase33_player_projections <- function(
    latest_season = "2025-26",
    seasons_back = 5,
    con = NULL) {
  
  owns <- is.null(
    con
  )
  
  if (owns) {
    con <- connect_db()
  }
  
  if (owns) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  if (
    !"player_season_impact" %in%
    DBI::dbListTables(con)
  ) {
    stop(
      paste(
        "player_season_impact does not exist.",
        "Rebuild Steps 4-8 first."
      )
    )
  }
  
  latest_start <-
    phase33_season_index(
      latest_season
    )
  
  seasons <- sprintf(
    "%d-%02d",
    (
      latest_start -
        seasons_back +
        1
    ):
      latest_start,
    (
      (
        latest_start -
          seasons_back +
          2
      ):
        (
          latest_start +
            1
        )
    ) %%
      100
  )
  
  qmarks <- paste(
    rep(
      "?",
      length(
        seasons
      )
    ),
    collapse = ","
  )
  
  sql <- paste0(
    "SELECT ",
    "i.player_id, ",
    "i.team_id, ",
    "i.season, ",
    "i.games_played, ",
    "i.minutes_per_game, ",
    "i.bie_performance_rating, ",
    "i.bie_performance_percentile, ",
    "i.impact_tier, ",
    "p.player_name, ",
    "p.player_age ",
    "FROM player_season_impact i ",
    "LEFT JOIN players p ",
    "ON p.player_id = i.player_id ",
    "WHERE i.season IN (",
    qmarks,
    ")"
  )
  
  h <- DBI::dbGetQuery(
    con,
    sql,
    params =
      as.list(
        seasons
      )
  )
  
  if (!nrow(h)) {
    return(
      invisible(
        list(
          status =
            "NO HISTORY",
          rows_processed =
            0L
        )
      )
    )
  }
  
  h$season_index <-
    phase33_season_index(
      h$season
    )
  
  ids <- unique(
    h$player_id
  )
  
  out <- lapply(
    ids,
    function(pid) {
      
      x <- h[
        h$player_id ==
          pid &
          is.finite(
            p33_num(
              h$bie_performance_rating
            )
          ),
        ,
        drop = FALSE
      ]
      
      x <- x[
        order(
          x$season_index
        ),
        ,
        drop = FALSE
      ]
      
      if (!nrow(x)) {
        return(NULL)
      }
      
      r <- p33_num(
        x$bie_performance_rating
      )
      
      gp <- p33_num(
        x$games_played
      )
      
      mpg <- p33_num(
        x$minutes_per_game
      )
      
      recency <- seq_along(
        r
      )
      
      availability <- pmin(
        1,
        ifelse(
          is.finite(gp),
          gp /
            65,
          0.5
        )
      )
      
      role_weight <- pmin(
        1,
        ifelse(
          is.finite(mpg),
          mpg /
            28,
          0.5
        )
      )
      
      w <-
        recency *
        (
          0.65 +
            0.35 *
            availability
        ) *
        (
          0.70 +
            0.30 *
            role_weight
        )
      
      baseline <- weighted.mean(
        r,
        w,
        na.rm = TRUE
      )
      
      slope <- 0
      
      if (
        length(r) >= 2 &&
        length(
          unique(
            x$season_index
          )
        ) >= 2
      ) {
        
        fit <- try(
          stats::lm(
            r ~
              x$season_index,
            weights = w
          ),
          silent = TRUE
        )
        
        if (
          !inherits(
            fit,
            "try-error"
          )
        ) {
          slope <- unname(
            stats::coef(
              fit
            )[2] %||%
              0
          )
        }
        
        if (!is.finite(slope)) {
          slope <- 0
        }
      }
      
      latest <- x[
        nrow(x),
        ,
        drop = FALSE
      ]
      
      current_age <- suppressWarnings(
        as.numeric(
          latest$player_age[[1]]
        )
      )
      
      # Historical trend is evidence, but is intentionally
      # damped so a hot/cold prior season cannot dominate.
      trend_1y <- max(
        -4,
        min(
          4,
          slope *
            0.45
        )
      )
      
      trend_3y <- max(
        -7,
        min(
          7,
          slope *
            0.75
        )
      )
      
      age_1y <-
        phase34_age_curve_adjustment(
          current_age =
            current_age,
          years_forward =
            1,
          baseline =
            baseline
        )
      
      age_3y <-
        phase34_age_curve_adjustment(
          current_age =
            current_age,
          years_forward =
            3,
          baseline =
            baseline
        )
      
      projection_1y <- p33_clip(
        baseline +
          trend_1y +
          age_1y
      )
      
      projection_3y <- p33_clip(
        baseline +
          trend_3y +
          age_3y
      )
      
      trajectory <-
        phase34_projection_trajectory(
          current_age =
            current_age,
          one_year =
            projection_1y,
          three_year =
            projection_3y,
          annual_trend =
            slope
        )
      
      confidence <- if (
        nrow(x) >= 4 &&
        is.finite(
          current_age
        ) &&
        current_age < 38
      ) {
        "HIGH"
      } else if (
        nrow(x) >= 2
      ) {
        "MODERATE"
      } else {
        "LIMITED"
      }
      
      # 40+ forecasts carry structural uncertainty even with
      # a long performance history.
      if (
        is.finite(
          current_age
        ) &&
        current_age >= 40
      ) {
        confidence <-
          "MODERATE"
      }
      
      data.frame(
        player_id =
          pid,
        
        player_name =
          latest$player_name[[1]],
        
        latest_team_id =
          latest$team_id[[1]],
        
        latest_season =
          latest$season[[1]],
        
        current_age =
          current_age,
        
        seasons_used =
          nrow(x),
        
        latest_bie_rating =
          r[
            length(r)
          ],
        
        weighted_baseline =
          baseline,
        
        annual_trend =
          slope,
        
        age_curve_1y =
          age_1y,
        
        age_curve_3y =
          age_3y,
        
        projected_bie_1y =
          projection_1y,
        
        projected_bie_3y =
          projection_3y,
        
        # Compatibility field used by existing TBI code.
        projected_bie_rating =
          projection_1y,
        
        projection_trajectory =
          trajectory,
        
        projection_confidence =
          confidence,
        
        projection_version =
          "P3P_v2_AGE_CURVE_FIVE_SEASON",
        
        stringsAsFactors = FALSE
      )
    }
  )
  
  out <- do.call(
    rbind,
    out
  )
  
  DBI::dbExecute(
    con,
    "
    DROP TABLE IF EXISTS player_projection_intelligence
    "
  )
  
  DBI::dbWriteTable(
    con,
    "player_projection_intelligence",
    out,
    overwrite = TRUE
  )
  
  invisible(
    list(
      status =
        "AGE-AWARE PROJECTIONS BUILT",
      
      rows_processed =
        nrow(out),
      
      seasons =
        seasons,
      
      model =
        "P3P_v2_AGE_CURVE_FIVE_SEASON"
    )
  )
}


phase33_projection_healthcheck <- function(con = NULL) {
  owns <- is.null(con)
  if (owns) con <- connect_db(read_only=TRUE)
  if (owns) on.exit(disconnect_db(con), add=TRUE)
  if (!"player_projection_intelligence" %in% DBI::dbListTables(con))
    return(list(status="REVIEW", explanation="Projection table not built."))
  d <- DBI::dbGetQuery(con, "SELECT * FROM player_projection_intelligence")
  list(
    phase="Phase 3.3",
    step="Five-Season Player Projection Intelligence",
    status=if(nrow(d)>0 && all(is.finite(p33_num(d$projected_bie_rating)))) "READY" else "REVIEW",
    players=nrow(d),
    high_confidence=sum(d$projection_confidence=="HIGH", na.rm=TRUE),
    moderate_confidence=sum(d$projection_confidence=="MODERATE", na.rm=TRUE),
    limited_confidence=sum(d$projection_confidence=="LIMITED", na.rm=TRUE),
    model_version="P3P_v1_FIVE_SEASON_DAMPED"
  )
}

# ------------------------------------------------------------
# ONE-COMMAND PHASE 3.3 REBUILD
# ------------------------------------------------------------
run_phase33_full_rebuild <- function(
    current_season = "2025-26",
    history_seasons = c("2021-22","2022-23","2023-24","2024-25","2025-26")) {
  
  results <- list()
  
  for (s in history_seasons) {
    message("[Phase 3.3] Rebuilding ", s, " ...")
    results[[s]] <- list(
      step5 = tryCatch(build_phase3_playmaking_intelligence(s), error=function(e) list(status="ERROR", explanation=conditionMessage(e))),
      step6 = tryCatch(build_phase3_defense_rebounding_intelligence(s), error=function(e) list(status="ERROR", explanation=conditionMessage(e))),
      step7 = tryCatch(build_phase3_player_roles(s), error=function(e) list(status="ERROR", explanation=conditionMessage(e))),
      step8 = tryCatch(build_phase3_player_impact(s), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
    )
  }
  
  projection <- tryCatch(
    build_phase33_player_projections(latest_season=current_season, seasons_back=length(history_seasons)),
    error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  
  hc5 <- tryCatch(phase3_step5_v3_healthcheck(current_season), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  hc6 <- tryCatch(phase3_step6_v4_healthcheck(current_season), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  hc7 <- tryCatch(phase3_step7_v2_healthcheck(current_season), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  hc8 <- tryCatch(phase3_step8_v3_healthcheck(current_season), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  hcp <- tryCatch(phase33_projection_healthcheck(), error=function(e) list(status="ERROR", explanation=conditionMessage(e)))
  
  statuses <- c(step5=hc5$status, step6=hc6$status, step7=hc7$status, step8=hc8$status, projections=hcp$status)
  ok <- all(grepl("^READY", statuses))
  
  summary <- data.frame(
    component=names(statuses), status=unname(statuses), stringsAsFactors=FALSE)
  
  list(
    phase="Phase 3.3",
    status=if(ok) "READY FOR TBI" else "REVIEW",
    seasons=history_seasons,
    summary=summary,
    healthchecks=list(step5=hc5,step6=hc6,step7=hc7,step8=hc8,projections=hcp),
    rebuild_results=results,
    projection_result=projection
  )
}


# ============================================================
# PHASE 3.4 — CONTROLLED ADVANCED-DATA REPAIR + REBUILD
# ============================================================

phase34_advanced_completeness <- function(
    season = "2025-26",
    con = NULL) {
  
  owns <- is.null(
    con
  )
  
  if (owns) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  d <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS rows,
      SUM(CASE WHEN usage_rate IS NOT NULL THEN 1 ELSE 0 END) AS usage_rows,
      SUM(CASE WHEN assist_pct IS NOT NULL THEN 1 ELSE 0 END) AS assist_pct_rows,
      SUM(CASE WHEN rebound_pct IS NOT NULL THEN 1 ELSE 0 END) AS rebound_pct_rows,
      SUM(CASE WHEN offensive_rating IS NOT NULL THEN 1 ELSE 0 END) AS off_rows,
      SUM(CASE WHEN defensive_rating IS NOT NULL THEN 1 ELSE 0 END) AS def_rows,
      SUM(CASE WHEN net_rating IS NOT NULL THEN 1 ELSE 0 END) AS net_rows,
      SUM(CASE WHEN official_possessions IS NOT NULL THEN 1 ELSE 0 END) AS poss_rows,
      SUM(CASE WHEN player_impact_estimate IS NOT NULL THEN 1 ELSE 0 END) AS pie_rows
    FROM player_season_advanced
    WHERE season = ?
    ",
    params =
      list(
        as.character(
          season
        )
      )
  )
  
  fields <- c(
    "usage_rows",
    "assist_pct_rows",
    "rebound_pct_rows",
    "off_rows",
    "def_rows",
    "net_rows",
    "poss_rows",
    "pie_rows"
  )
  
  counts <- suppressWarnings(
    as.numeric(
      d[
        1,
        fields
      ]
    )
  )
  
  list(
    phase =
      "Phase 3.4",
    
    season =
      season,
    
    status = if (
      d$rows[[1]] > 0 &&
      min(
        counts,
        na.rm = TRUE
      ) >=
      d$rows[[1]] -
      2
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    
    counts =
      d,
    
    note =
      "A tiny number of legitimate source-null rows may remain; any material gap should be reviewed."
  )
}


phase34_luka_check <- function(
    season = "2025-26") {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_name,
      a.player_id,
      a.team_id,
      a.season,
      a.usage_rate,
      a.assist_pct,
      a.offensive_rebound_pct,
      a.defensive_rebound_pct,
      a.rebound_pct,
      a.offensive_rating,
      a.defensive_rating,
      a.net_rating,
      a.official_possessions,
      a.player_impact_estimate,
      a.source_player_id,
      a.metric_version
    FROM player_season_advanced a
    INNER JOIN players p
      ON p.player_id = a.player_id
    WHERE a.season = ?
      AND LOWER(p.player_name) LIKE '%luka doncic%'
    ORDER BY a.team_id
    ",
    params =
      list(
        as.character(
          season
        )
      )
  )
}


run_phase34_complete_correction <- function(
    current_season = "2025-26",
    history_seasons = c(
      "2021-22",
      "2022-23",
      "2023-24",
      "2024-25",
      "2025-26"
    )) {
  
  if (
    !requireNamespace(
      "hoopR",
      quietly = TRUE
    )
  ) {
    stop(
      "Package 'hoopR' is required."
    )
  }
  
  results <- list()
  
  for (s in history_seasons) {
    
    message(
      "[Phase 3.4] ",
      s,
      " — downloading official NBA Advanced..."
    )
    
    official <- hoopR::nba_leaguedashplayerstats(
      season = s,
      measure_type = "Advanced",
      per_mode = "PerGame"
    )
    
    message(
      "[Phase 3.4] ",
      s,
      " — rebuilding Step 2 official advanced..."
    )
    
    step2 <- build_advanced_metrics_from_player_stats(
      season = s,
      official_advanced = official
    )
    
    message(
      "[Phase 3.4] ",
      s,
      " — rebuilding Steps 4-8..."
    )
    
    step4 <- tryCatch(
      build_phase3_shooting_intelligence(
        s
      ),
      error =
        function(e) {
          list(
            status =
              "ERROR",
            explanation =
              conditionMessage(e)
          )
        }
    )
    
    step5 <- tryCatch(
      build_phase3_playmaking_intelligence(
        s
      ),
      error =
        function(e) {
          list(
            status =
              "ERROR",
            explanation =
              conditionMessage(e)
          )
        }
    )
    
    step6 <- tryCatch(
      build_phase3_defense_rebounding_intelligence(
        s
      ),
      error =
        function(e) {
          list(
            status =
              "ERROR",
            explanation =
              conditionMessage(e)
          )
        }
    )
    
    step7 <- tryCatch(
      build_phase3_player_roles(
        s
      ),
      error =
        function(e) {
          list(
            status =
              "ERROR",
            explanation =
              conditionMessage(e)
          )
        }
    )
    
    step8 <- tryCatch(
      build_phase3_player_impact(
        s
      ),
      error =
        function(e) {
          list(
            status =
              "ERROR",
            explanation =
              conditionMessage(e)
          )
        }
    )
    
    results[[s]] <- list(
      step2 =
        step2,
      step4 =
        step4,
      step5 =
        step5,
      step6 =
        step6,
      step7 =
        step7,
      step8 =
        step8
    )
  }
  
  message(
    "[Phase 3.4] Building age-aware five-season projections..."
  )
  
  projection <-
    build_phase33_player_projections(
      latest_season =
        current_season,
      seasons_back =
        length(
          history_seasons
        )
    )
  
  completeness <-
    phase34_advanced_completeness(
      current_season
    )
  
  projection_hc <-
    phase33_projection_healthcheck()
  
  luka <-
    phase34_luka_check(
      current_season
    )
  
  overall <- if (
    identical(
      completeness$status,
      "READY"
    ) &&
    grepl(
      "^READY",
      projection_hc$status
    )
  ) {
    "READY FOR TBI REVIEW"
  } else {
    "REVIEW"
  }
  
  list(
    phase =
      "Phase 3.4",
    
    status =
      overall,
    
    advanced_completeness =
      completeness,
    
    projection_healthcheck =
      projection_hc,
    
    luka_check =
      luka,
    
    rebuild_results =
      results,
    
    projection_result =
      projection
  )
}
