tbi_league_healthcheck <- function(
    current_roster_season = "2026-27",
    performance_season = "2025-26") {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name
    FROM teams
    ORDER BY team_name
    "
  )
  
  roster <- DBI::dbGetQuery(
    con,
    "
    SELECT
      rh.team_id,
      rh.player_id,
      rh.season,
      p.player_name
    FROM roster_history rh
    INNER JOIN players p
      ON p.player_id = rh.player_id
    WHERE rh.season = ?
    ",
    params = list(
      current_roster_season
    )
  )
  
  stats <- DBI::dbGetQuery(
    con,
    "
    SELECT DISTINCT
      player_id,
      team_id,
      season
    FROM player_season_stats
    WHERE season = ?
    ",
    params = list(
      performance_season
    )
  )
  
  impact <- DBI::dbGetQuery(
    con,
    "
    SELECT DISTINCT
      player_id,
      team_id,
      season,
      bie_performance_rating,
      impact_confidence
    FROM player_season_impact
    WHERE season = ?
    ",
    params = list(
      performance_season
    )
  )
  
  team_results <- lapply(
    seq_len(
      nrow(teams)
    ),
    function(i) {
      
      team_id <-
        teams$team_id[[i]]
      
      team_name <-
        teams$team_name[[i]]
      
      team_roster <- roster[
        roster$team_id == team_id,
        ,
        drop = FALSE
      ]
      
      roster_players <-
        nrow(team_roster)
      
      prior_stats <- stats[
        stats$player_id %in%
          team_roster$player_id,
        ,
        drop = FALSE
      ]
      
      prior_impact <- impact[
        impact$player_id %in%
          team_roster$player_id,
        ,
        drop = FALSE
      ]
      
      veterans_with_stats <-
        length(
          unique(
            prior_stats$player_id
          )
        )
      
      players_with_impact <-
        length(
          unique(
            prior_impact$player_id[
              is.finite(
                suppressWarnings(
                  as.numeric(
                    prior_impact$
                      bie_performance_rating
                  )
                )
              )
            ]
          )
        )
      
      rookies_or_pending <-
        max(
          roster_players -
            veterans_with_stats,
          0
        )
      
      missing_impact <-
        max(
          veterans_with_stats -
            players_with_impact,
          0
        )
      
      status <- if (
        roster_players > 0 &&
        missing_impact == 0
      ) {
        "READY"
      } else {
        "REVIEW"
      }
      
      data.frame(
        team_id =
          team_id,
        
        team_name =
          team_name,
        
        roster_players =
          roster_players,
        
        veterans_with_prior_stats =
          veterans_with_stats,
        
        rookies_or_pending =
          rookies_or_pending,
        
        players_with_bie =
          players_with_impact,
        
        missing_bie =
          missing_impact,
        
        status =
          status,
        
        stringsAsFactors =
          FALSE
      )
    }
  )
  
  league <- do.call(
    rbind,
    team_results
  )
  
  league <- league[
    order(
      league$team_name
    ),
    ,
    drop = FALSE
  ]
  
  attr(
    league,
    "summary"
  ) <- list(
    teams =
      nrow(league),
    
    ready =
      sum(
        league$status ==
          "READY"
      ),
    
    review =
      sum(
        league$status ==
          "REVIEW"
      ),
    
    roster_players =
      sum(
        league$roster_players
      ),
    
    veterans_with_prior_stats =
      sum(
        league$
          veterans_with_prior_stats
      ),
    
    rookies_or_pending =
      sum(
        league$
          rookies_or_pending
      ),
    
    missing_bie =
      sum(
        league$missing_bie
      )
  )
  
  league
}