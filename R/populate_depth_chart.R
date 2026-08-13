# ============================================================
# Thompson's Basketball Intelligence
# Depth Chart Population Maintenance
#
# Phase 15J safety repair:
# This file DEFINES a maintenance function only.
# It must never rebuild depth_chart during package loading.
# ============================================================


#' Rebuild the depth chart for one season
#'
#' This is an explicit maintenance action.
#' It does not run during devtools::load_all().
#'
#' @param current_season Season to rebuild.
#' @param db_path Optional explicit database path.
#' @return Invisibly returns the populated depth-chart rows.
#' @noRd
tbi_populate_depth_chart <- function(
    current_season = "2026-27",
    db_path = NULL) {

  if (
    !length(current_season) ||
    is.na(current_season[[1]]) ||
    !nzchar(trimws(as.character(current_season[[1]])))
  ) {
    stop(
      "current_season must be a non-empty season value.",
      call. = FALSE
    )
  }

  current_season <- trimws(
    as.character(current_season[[1]])
  )


  # Ensure canonical table exists.
  tbi_create_depth_chart_table(
    db_path = db_path
  )


  con <- connect_db(
    db_path = db_path,
    read_only = FALSE
  )

  on.exit(
    disconnect_db(con),
    add = TRUE
  )


  current_roster <- DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      rh.team_id,
      t.team_name,
      rh.season,
      rh.two_way_flag,
      COALESCE(cy.cap_hit, 0) AS cap_hit

    FROM roster_history rh

    INNER JOIN players p
      ON rh.player_id = p.player_id

    INNER JOIN teams t
      ON rh.team_id = t.team_id

    LEFT JOIN contract_years cy
      ON rh.player_id = cy.player_id
      AND rh.team_id = cy.team_id
      AND rh.season = cy.season

    WHERE
      rh.season = ?
      AND rh.roster_status = 'Active'
      AND rh.end_date IS NULL
    ",
    params = list(
      current_season
    )
  )


  if (!nrow(current_roster)) {
    stop(
      paste0(
        "No active roster rows found for season ",
        current_season,
        "."
      ),
      call. = FALSE
    )
  }


  depth_chart_seed <- dplyr::mutate(
    current_roster,

    position = trimws(
      sub(
        ",.*$",
        "",
        primary_position
      )
    ),

    position = dplyr::case_when(
      position %in% c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      ) ~ position,

      TRUE ~ "UTIL"
    )
  )


  depth_chart_seed <- dplyr::group_by(
    depth_chart_seed,
    team_id,
    season,
    position
  )


  depth_chart_seed <- dplyr::arrange(
    depth_chart_seed,
    two_way_flag,
    dplyr::desc(cap_hit),
    player_name,
    .by_group = TRUE
  )


  depth_chart_seed <- dplyr::mutate(
    depth_chart_seed,
    depth_order = dplyr::row_number(),
    is_starter = as.integer(
      depth_order == 1L
    )
  )


  depth_chart_seed <- dplyr::ungroup(
    depth_chart_seed
  )


  depth_chart_seed <- dplyr::select(
    depth_chart_seed,
    player_id,
    team_id,
    season,
    position,
    depth_order,
    is_starter
  )


  DBI::dbWithTransaction(
    con,
    {

      DBI::dbExecute(
        con,
        "DELETE FROM depth_chart WHERE season = ?",
        params = list(
          current_season
        )
      )


      DBI::dbWriteTable(
        con,
        "depth_chart",
        depth_chart_seed,
        append = TRUE,
        row.names = FALSE
      )
    }
  )


  message(
    "Depth chart populated for ",
    dplyr::n_distinct(
      depth_chart_seed$team_id
    ),
    " teams and ",
    nrow(depth_chart_seed),
    " players."
  )


  invisible(
    depth_chart_seed
  )
}
