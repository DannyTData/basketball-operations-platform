# ============================================================
# TBI DRAFT TRADE RULES ENGINE
# NBA v1.0 — PHASE 15I v3
# Seven-year horizon / Stepien / protection / swap / apron
# Decision-support layer — no database writes
# ============================================================

tbi_trade_selectable_draft_assets <-
function(
        team_value,
        db_path = NULL) {
    
    assets <-
        get_draft_assets(
            team_value = team_value,
            include_inactive = FALSE,
            db_path = db_path
        )
    
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(
            data.frame()
        )
    }
    
    
    result <-
        assets[
            assets$control_type %in%
                c(
                    "Own",
                    "Incoming",
                    "Swap Right"
                ),
            ,
            drop = FALSE
        ]
    
    
    rownames(result) <- NULL
    
    result
}
tbi_trade_draft_asset_choices <-
function(
        assets) {
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(
            stats::setNames(
                character(),
                character()
            )
        )
    }
    
    
    char <- function(x) {
        
        x <- as.character(x)
        
        x[
            is.na(x)
        ] <- ""
        
        trimws(x)
    }
    
    
    round_label <-
        ifelse(
            char(
                assets$round
            ) ==
                "First",
            "1st",
            "2nd"
        )
    
    
    labels <-
        paste0(
            assets$draft_year,
            " ",
            round_label,
            " • ",
            char(
                assets$original_team
            ),
            " • ",
            char(
                assets$control_type
            )
        )
    
    
    protection <- if (
        "protection_type" %in%
        names(assets)
    ) {
        
        char(
            assets$protection_type
        )
        
    } else {
        
        rep(
            "",
            nrow(assets)
        )
    }
    
    
    show_protection <-
        nzchar(protection) &
        !tolower(protection) %in%
        c(
            "none",
            "unprotected",
            "not protected"
        )
    
    
    labels[
        show_protection
    ] <-
        paste0(
            labels[
                show_protection
            ],
            " • ",
            protection[
                show_protection
            ]
        )
    
    
    review <- if (
        "requires_manual_review" %in%
        names(assets)
    ) {
        
        suppressWarnings(
            as.logical(
                assets$requires_manual_review
            )
        )
        
    } else {
        
        rep(
            FALSE,
            nrow(assets)
        )
    }
    
    
    review[
        is.na(review)
    ] <- TRUE
    
    
    labels[
        review
    ] <-
        paste0(
            labels[
                review
            ],
            " • REVIEW"
        )
    
    
    stats::setNames(
        as.character(
            assets$draft_asset_id
        ),
        labels
    )
}
tbi_trade_selected_draft_assets <-
function(
        assets,
        selected_ids) {
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(
            data.frame()
        )
    }
    
    
    ids <-
        suppressWarnings(
            as.integer(
                selected_ids
            )
        )
    
    
    ids <-
        ids[
            !is.na(ids)
        ]
    
    
    if (!length(ids)) {
        
        return(
            assets[
                FALSE,
                ,
                drop = FALSE
            ]
        )
    }
    
    
    result <-
        assets[
            assets$draft_asset_id %in%
                ids,
            ,
            drop = FALSE
        ]
    
    
    rownames(result) <- NULL
    
    result
}
tbi_trade_draft_value <-
function(
        assets) {
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(0)
    }
    
    
    if (
        !"internal_value" %in%
        names(assets)
    ) {
        
        return(
            NA_real_
        )
    }
    
    
    values <-
        suppressWarnings(
            as.numeric(
                assets$internal_value
            )
        )
    
    
    if (
        length(values) !=
        nrow(assets) ||
        any(
            is.na(values)
        )
    ) {
        
        return(
            NA_real_
        )
    }
    
    
    sum(values)
}
tbi_trade_season_start_year <-
function(
        season) {
    
    season <-
        as.character(
            season
        )
    
    
    if (
        !length(season) ||
        is.na(season[[1]]) ||
        !grepl(
            "^[0-9]{4}",
            season[[1]]
        )
    ) {
        
        return(
            NA_integer_
        )
    }
    
    
    suppressWarnings(
        as.integer(
            substr(
                season[[1]],
                1L,
                4L
            )
        )
    )
}
tbi_trade_draft_data_window <-
function(
        db_path = NULL) {
    
    con <-
        connect_db(
            db_path = db_path,
            read_only = TRUE
        )
    
    
    on.exit(
        disconnect_db(con),
        add = TRUE
    )
    
    
    result <-
        DBI::dbGetQuery(
            con,
            "
      SELECT
        MIN(draft_year) AS min_year,
        MAX(draft_year) AS max_year
      FROM draft_assets
      WHERE is_active = 1;
      "
        )
    
    
    if (!nrow(result)) {
        
        return(
            c(
                min_year = NA_integer_,
                max_year = NA_integer_
            )
        )
    }
    
    
    c(
        min_year =
            suppressWarnings(
                as.integer(
                    result$min_year[[1]]
                )
            ),
        max_year =
            suppressWarnings(
                as.integer(
                    result$max_year[[1]]
                )
            )
    )
}
tbi_trade_draft_asset_complexity <-
function(
        assets) {
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(
            logical()
        )
    }
    
    
    review <-
        rep(
            FALSE,
            nrow(assets)
        )
    
    
    if (
        "requires_manual_review" %in%
        names(assets)
    ) {
        
        x <-
            suppressWarnings(
                as.logical(
                    assets$requires_manual_review
                )
            )
        
        x[is.na(x)] <- TRUE
        
        review <-
            review |
            x
    }
    
    
    if (
        "verification_status" %in%
        names(assets)
    ) {
        
        x <-
            trimws(
                as.character(
                    assets$verification_status
                )
            )
        
        review <-
            review |
            is.na(x) |
            x != "Verified"
    }
    
    
    if (
        "condition_count" %in%
        names(assets)
    ) {
        
        x <-
            suppressWarnings(
                as.numeric(
                    assets$condition_count
                )
            )
        
        x[is.na(x)] <- 0
        
        review <-
            review |
            x > 0
    }
    
    
    if (
        "control_type" %in%
        names(assets)
    ) {
        
        review <-
            review |
            as.character(
                assets$control_type
            ) ==
            "Swap Right"
    }
    
    
    if (
        "protection_type" %in%
        names(assets)
    ) {
        
        x <-
            trimws(
                as.character(
                    assets$protection_type
                )
            )
        
        review <-
            review |
            x %in%
            c(
                "Conditional",
                "Best Of",
                "Worst Of",
                "Range Protected",
                "Unspecified"
            )
    }
    
    
    if (
        all(
            c(
                "draft_year",
                "conveyance_end_year"
            ) %in%
            names(assets)
        )
    ) {
        
        draft_year <-
            suppressWarnings(
                as.integer(
                    assets$draft_year
                )
            )
        
        conveyance_end <-
            suppressWarnings(
                as.integer(
                    assets$conveyance_end_year
                )
            )
        
        review <-
            review |
            (
                !is.na(conveyance_end) &
                    !is.na(draft_year) &
                    conveyance_end > draft_year
            )
    }
    
    
    review
}
tbi_trade_underlying_pick_key <-
function(
        assets) {
    
    if (
        is.null(assets) ||
        !is.data.frame(assets) ||
        !nrow(assets)
    ) {
        
        return(
            character()
        )
    }
    
    
    original_team <- if (
        "original_team" %in%
        names(assets)
    ) {
        
        as.character(
            assets$original_team
        )
        
    } else {
        
        rep(
            "",
            nrow(assets)
        )
    }
    
    
    paste(
        suppressWarnings(
            as.integer(
                assets$draft_year
            )
        ),
        as.character(
            assets$round
        ),
        original_team,
        sep = "|"
    )
}
tbi_trade_stepien_pair_result <-
function(
        left_status,
        right_status,
        transaction_touches_pair = TRUE) {
    
    left_status <-
        as.character(
            left_status
        )
    
    right_status <-
        as.character(
            right_status
        )
    
    
    if (
        identical(
            left_status,
            "NONE"
        ) &&
        identical(
            right_status,
            "NONE"
        )
    ) {
        
        return(
            "BLOCK"
        )
    }
    
    
    statuses <-
        c(
            left_status,
            right_status
        )
    
    
    if (
        isTRUE(
            transaction_touches_pair
        ) &&
        any(
            statuses == "NONE"
        ) &&
        any(
            statuses %in%
            c(
                "UNKNOWN",
                "UNCERTAIN"
            )
        )
    ) {
        
        return(
            "REVIEW"
        )
    }
    
    
    "PASS"
}
tbi_trade_team_draft_rules <-
function(
        team_name,
        sent_assets,
        received_assets = data.frame(),
        season,
        db_path = NULL) {
    
    if (
        is.null(sent_assets) ||
        !is.data.frame(sent_assets)
    ) {
        
        sent_assets <-
            data.frame()
    }
    
    
    if (
        is.null(received_assets) ||
        !is.data.frame(received_assets)
    ) {
        
        received_assets <-
            data.frame()
    }
    
    
    if (!nrow(sent_assets)) {
        
        return(
            list(
                team = team_name,
                status = "NOT USED",
                blocked = FALSE,
                requires_manual_review = FALSE,
                issues = character(),
                year_states = data.frame(),
                horizon_start = NA_integer_,
                horizon_end = NA_integer_
            )
        )
    }
    
    
    blocks <-
        character()
    
    reviews <-
        character()
    
    
    season_start <-
        tbi_trade_season_start_year(
            season
        )
    
    
    if (is.na(season_start)) {
        
        return(
            list(
                team = team_name,
                status = "REVIEW",
                blocked = FALSE,
                requires_manual_review = TRUE,
                issues =
                    "Selected season could not be converted into a draft-trade horizon.",
                year_states = data.frame(),
                horizon_start = NA_integer_,
                horizon_end = NA_integer_
            )
        )
    }
    
    
    first_future_draft <-
        season_start + 1L
    
    final_tradeable_draft <-
        season_start + 7L
    
    
    selected_years <-
        suppressWarnings(
            as.integer(
                sent_assets$draft_year
            )
        )
    
    
    # ----------------------------------------------------------
    # Seven-year future-pick horizon
    # ----------------------------------------------------------
    
    outside_horizon <-
        is.na(selected_years) |
        selected_years <
        first_future_draft |
        selected_years >
        final_tradeable_draft
    
    
    if (
        any(
            outside_horizon,
            na.rm = TRUE
        )
    ) {
        
        bad_years <-
            unique(
                selected_years[
                    outside_horizon &
                        !is.na(selected_years)
                ]
            )
        
        
        bad_text <-
            if (length(bad_years)) {
                
                paste(
                    bad_years,
                    collapse = ", "
                )
                
            } else {
                
                "invalid year"
            }
        
        
        blocks <-
            c(
                blocks,
                paste0(
                    "Future-pick horizon violation: ",
                    bad_text,
                    " is outside the modeled ",
                    first_future_draft,
                    "-",
                    final_tradeable_draft,
                    " transaction window."
                )
            )
    }
    
    
    # ----------------------------------------------------------
    # Ownership / control
    # ----------------------------------------------------------
    
    if (
        "current_team" %in%
        names(sent_assets)
    ) {
        
        holder <-
            trimws(
                as.character(
                    sent_assets$current_team
                )
            )
        
        wrong_holder <-
            !is.na(holder) &
            nzchar(holder) &
            holder !=
            as.character(
                team_name
            )
        
        
        if (
            any(
                wrong_holder,
                na.rm = TRUE
            )
        ) {
            
            blocks <-
                c(
                    blocks,
                    "At least one selected draft asset is not currently controlled by the sending organization."
                )
        }
    }
    
    
    control <-
        as.character(
            sent_assets$control_type
        )
    
    
    bad_control <-
        !control %in%
        c(
            "Own",
            "Incoming",
            "Swap Right"
        )
    
    
    if (
        any(
            bad_control,
            na.rm = TRUE
        )
    ) {
        
        blocks <-
            c(
                blocks,
                "At least one selected draft asset is an obligation rather than a controlled asset."
            )
    }
    
    
    # ----------------------------------------------------------
    # Protection / swap / source complexity
    # ----------------------------------------------------------
    
    sent_complex <-
        tbi_trade_draft_asset_complexity(
            sent_assets
        )
    
    
    if (
        any(
            sent_complex,
            na.rm = TRUE
        )
    ) {
        
        reviews <-
            c(
                reviews,
                "One or more outgoing draft assets contain protection, conveyance, swap, or verification complexity."
            )
    }
    
    
    if (
        any(
            control == "Swap Right",
            na.rm = TRUE
        )
    ) {
        
        reviews <-
            c(
                reviews,
                "A selected swap right requires manual review of the underlying swap hierarchy."
            )
    }
    
    
    # ----------------------------------------------------------
    # Second-apron frozen-pick guardrail
    #
    # We do NOT invent historical apron status.
    #
    # Deep-horizon own firsts receive REVIEW so league/CBA
    # status can be confirmed before the transaction is treated
    # as clear.
    # ----------------------------------------------------------
    
    own_first <-
        as.character(
            sent_assets$round
        ) ==
        "First" &
        control ==
        "Own"
    
    
    deepest_horizon_own_first <-
        own_first &
        selected_years ==
        final_tradeable_draft
    
    
    if (
        any(
            deepest_horizon_own_first,
            na.rm = TRUE
        )
    ) {
        
        reviews <-
            c(
                reviews,
                paste0(
                    "Second-apron frozen-pick review: confirm the league status of the ",
                    final_tradeable_draft,
                    " own first-round pick before treating it as freely tradeable."
                )
            )
    }
    
    
    # ----------------------------------------------------------
    # Load complete organization portfolio
    # ----------------------------------------------------------
    
    portfolio <-
        tryCatch(
            
            get_draft_assets(
                team_value = team_name,
                include_inactive = FALSE,
                db_path = db_path
            ),
            
            error = function(e) {
                
                NULL
            }
        )
    
    
    if (
        is.null(portfolio) ||
        !is.data.frame(portfolio)
    ) {
        
        reviews <-
            c(
                reviews,
                "The full organization draft portfolio could not be loaded for Stepien screening."
            )
        
        
        final_status <-
            if (length(blocks)) {
                "BLOCK"
            } else {
                "REVIEW"
            }
        
        
        return(
            list(
                team = team_name,
                status = final_status,
                blocked =
                    identical(
                        final_status,
                        "BLOCK"
                    ),
                requires_manual_review =
                    identical(
                        final_status,
                        "REVIEW"
                    ),
                issues =
                    unique(
                        c(
                            blocks,
                            reviews
                        )
                    ),
                year_states =
                    data.frame(),
                horizon_start =
                    first_future_draft,
                horizon_end =
                    final_tradeable_draft
            )
        )
    }
    
    
    # ----------------------------------------------------------
    # Remove picks being sent
    # ----------------------------------------------------------
    
    sent_ids <-
        suppressWarnings(
            as.integer(
                sent_assets$draft_asset_id
            )
        )
    
    sent_ids <-
        sent_ids[
            !is.na(sent_ids)
        ]
    
    
    remaining <-
        portfolio[
            !portfolio$draft_asset_id %in%
                sent_ids,
            ,
            drop = FALSE
        ]
    
    
    # ----------------------------------------------------------
    # Corrected data-window indexing
    # ----------------------------------------------------------
    
    data_window <-
        tbi_trade_draft_data_window(
            db_path = db_path
        )
    
    
    window_min <-
        suppressWarnings(
            as.integer(
                data_window[["min_year"]]
            )
        )
    
    
    window_max <-
        suppressWarnings(
            as.integer(
                data_window[["max_year"]]
            )
        )
    
    
    horizon_years <-
        seq.int(
            first_future_draft,
            final_tradeable_draft
        )
    
    
    states <-
        rep(
            "UNKNOWN",
            length(horizon_years)
        )
    
    
    touches <-
        rep(
            FALSE,
            length(horizon_years)
        )
    
    
    # ----------------------------------------------------------
    # Determine post-trade first-round control by year
    # ----------------------------------------------------------
    
    for (
        i in seq_along(
            horizon_years
        )
    ) {
        
        draft_year <-
            horizon_years[[i]]
        
        
        retained_firsts <-
            remaining[
                as.character(
                    remaining$round
                ) ==
                    "First" &
                    suppressWarnings(
                        as.integer(
                            remaining$draft_year
                        )
                    ) ==
                    draft_year &
                    as.character(
                        remaining$control_type
                    ) %in%
                    c(
                        "Own",
                        "Incoming",
                        "Swap Right"
                    ),
                ,
                drop = FALSE
            ]
        
        
        if (
            nrow(received_assets) &&
            all(
                c(
                    "round",
                    "draft_year"
                ) %in%
                names(received_assets)
            )
        ) {
            
            received_firsts <-
                received_assets[
                    as.character(
                        received_assets$round
                    ) ==
                        "First" &
                        suppressWarnings(
                            as.integer(
                                received_assets$draft_year
                            )
                        ) ==
                        draft_year,
                    ,
                    drop = FALSE
                ]
            
        } else {
            
            received_firsts <-
                data.frame()
        }
        
        
        retained_complex <-
            tbi_trade_draft_asset_complexity(
                retained_firsts
            )
        
        
        received_complex <-
            tbi_trade_draft_asset_complexity(
                received_firsts
            )
        
        
        retained_clean <-
            if (nrow(retained_firsts)) {
                
                !retained_complex &
                    as.character(
                        retained_firsts$control_type
                    ) %in%
                    c(
                        "Own",
                        "Incoming"
                    )
                
            } else {
                
                logical()
            }
        
        
        received_clean <-
            if (nrow(received_firsts)) {
                
                !received_complex &
                    as.character(
                        received_firsts$control_type
                    ) %in%
                    c(
                        "Own",
                        "Incoming"
                    )
                
            } else {
                
                logical()
            }
        
        
        has_clean_first <-
            any(
                retained_clean,
                na.rm = TRUE
            ) ||
            any(
                received_clean,
                na.rm = TRUE
            )
        
        
        has_uncertain_first <-
            nrow(retained_firsts) > 0L ||
            nrow(received_firsts) > 0L
        
        
        sent_this_year <-
            sent_assets[
                as.character(
                    sent_assets$round
                ) ==
                    "First" &
                    selected_years ==
                    draft_year,
                ,
                drop = FALSE
            ]
        
        
        sent_this_complex <-
            tbi_trade_draft_asset_complexity(
                sent_this_year
            )
        
        
        definitely_sent_first <-
            nrow(sent_this_year) > 0L &&
            any(
                !sent_this_complex &
                    as.character(
                        sent_this_year$control_type
                    ) %in%
                    c(
                        "Own",
                        "Incoming"
                    ),
                na.rm = TRUE
            )
        
        
        complex_sent_first <-
            nrow(sent_this_year) > 0L &&
            any(
                sent_this_complex,
                na.rm = TRUE
            )
        
        
        touches[[i]] <-
            nrow(sent_this_year) > 0L
        
        
        if (has_clean_first) {
            
            states[[i]] <-
                "SAFE"
            
        } else if (
            has_uncertain_first ||
            complex_sent_first
        ) {
            
            states[[i]] <-
                "UNCERTAIN"
            
        } else if (
            definitely_sent_first
        ) {
            
            states[[i]] <-
                "NONE"
            
        } else if (
            !is.na(window_min) &&
            !is.na(window_max) &&
            draft_year >= window_min &&
            draft_year <= window_max
        ) {
            
            states[[i]] <-
                "NONE"
            
        } else {
            
            states[[i]] <-
                "UNKNOWN"
        }
    }
    
    
    # ----------------------------------------------------------
    # Protected / conditional conveyance window
    # ----------------------------------------------------------
    
    sent_firsts <-
        sent_assets[
            as.character(
                sent_assets$round
            ) ==
                "First",
            ,
            drop = FALSE
        ]
    
    
    if (nrow(sent_firsts)) {
        
        first_complex <-
            tbi_trade_draft_asset_complexity(
                sent_firsts
            )
        
        
        for (
            j in seq_len(
                nrow(sent_firsts)
            )
        ) {
            
            if (
                !isTRUE(
                    first_complex[[j]]
                )
            ) {
                
                next
            }
            
            
            begin_year <-
                suppressWarnings(
                    as.integer(
                        sent_firsts$draft_year[[j]]
                    )
                )
            
            
            if (is.na(begin_year)) {
                
                next
            }
            
            
            end_year <-
                if (
                    "conveyance_end_year" %in%
                    names(sent_firsts)
                ) {
                    
                    suppressWarnings(
                        as.integer(
                            sent_firsts$conveyance_end_year[[j]]
                        )
                    )
                    
                } else {
                    
                    NA_integer_
                }
            
            
            if (
                is.na(end_year) ||
                end_year < begin_year
            ) {
                
                end_year <-
                    begin_year
            }
            
            
            possible_years <-
                intersect(
                    seq.int(
                        begin_year,
                        end_year
                    ),
                    horizon_years
                )
            
            
            for (
                possible_year in possible_years
            ) {
                
                index <-
                    match(
                        possible_year,
                        horizon_years
                    )
                
                
                if (is.na(index)) {
                    
                    next
                }
                
                
                touches[[index]] <-
                    TRUE
                
                
                if (
                    states[[index]] !=
                    "SAFE"
                ) {
                    
                    states[[index]] <-
                        "UNCERTAIN"
                }
            }
        }
    }
    
    
    year_states <-
        data.frame(
            draft_year =
                horizon_years,
            first_round_state =
                states,
            transaction_touches_year =
                touches,
            stringsAsFactors =
                FALSE
        )
    
    
    # ----------------------------------------------------------
    # Stepien Rule
    # ----------------------------------------------------------
    
    if (
        length(horizon_years) >=
        2L
    ) {
        
        for (
            i in seq_len(
                length(horizon_years) - 1L
            )
        ) {
            
            left_year <-
                horizon_years[[i]]
            
            right_year <-
                horizon_years[[i + 1L]]
            
            
            pair_touched <-
                touches[[i]] ||
                touches[[i + 1L]]
            
            
            pair_result <-
                tbi_trade_stepien_pair_result(
                    states[[i]],
                    states[[i + 1L]],
                    transaction_touches_pair =
                        pair_touched
                )
            
            
            if (
                identical(
                    pair_result,
                    "BLOCK"
                ) &&
                pair_touched
            ) {
                
                blocks <-
                    c(
                        blocks,
                        paste0(
                            "Stepien Rule violation: the modeled transaction would leave ",
                            team_name,
                            " without a controlled first-round pick in both the ",
                            left_year,
                            " and ",
                            right_year,
                            " NBA Drafts."
                        )
                    )
                
            } else if (
                identical(
                    pair_result,
                    "REVIEW"
                )
            ) {
                
                reviews <-
                    c(
                        reviews,
                        paste0(
                            "Stepien review required across the ",
                            left_year,
                            " and ",
                            right_year,
                            " drafts because at least one adjacent first-round asset is conditional or unresolved."
                        )
                    )
            }
        }
    }
    
    
    # ----------------------------------------------------------
    # Explicit protection / conveyance review
    # ----------------------------------------------------------
    
    if (
        nrow(sent_firsts) &&
        any(
            tbi_trade_draft_asset_complexity(
                sent_firsts
            ),
            na.rm = TRUE
        )
    ) {
        
        reviews <-
            c(
                reviews,
                paste(
                    "Protected or conditional outgoing first-round picks must be",
                    "checked across every possible conveyance season before another",
                    "first-round pick is treated as freely tradeable."
                )
            )
    }
    
    
    blocks <-
        unique(
            blocks
        )
    
    reviews <-
        unique(
            reviews
        )
    
    
    status <-
        if (length(blocks)) {
            
            "BLOCK"
            
        } else if (length(reviews)) {
            
            "REVIEW"
            
        } else {
            
            "PASS"
        }
    
    
    list(
        team = team_name,
        status = status,
        blocked =
            identical(
                status,
                "BLOCK"
            ),
        requires_manual_review =
            identical(
                status,
                "REVIEW"
            ),
        issues =
            c(
                blocks,
                reviews
            ),
        year_states =
            year_states,
        horizon_start =
            first_future_draft,
        horizon_end =
            final_tradeable_draft
    )
}
tbi_trade_draft_screen <-
function(
        outgoing_assets,
        incoming_assets,
        team_a = NULL,
        team_b = NULL,
        season = NULL,
        db_path = NULL) {
    
    normalize_assets <- function(x) {
        
        if (
            is.null(x) ||
            !is.data.frame(x)
        ) {
            
            return(
                data.frame()
            )
        }
        
        x
    }
    
    
    outgoing_assets <-
        normalize_assets(
            outgoing_assets
        )
    
    incoming_assets <-
        normalize_assets(
            incoming_assets
        )
    
    
    outgoing_count <-
        nrow(
            outgoing_assets
        )
    
    incoming_count <-
        nrow(
            incoming_assets
        )
    
    
    if (
        outgoing_count == 0L &&
        incoming_count == 0L
    ) {
        
        return(
            list(
                status = "NOT USED",
                requires_manual_review = FALSE,
                blocked = FALSE,
                issues = character(),
                outgoing_count = 0L,
                incoming_count = 0L,
                outgoing_value = 0,
                incoming_value = 0,
                value_delta = 0,
                team_a_rules = NULL,
                team_b_rules = NULL,
                summary =
                    "No draft assets are included in the transaction."
            )
        )
    }
    
    
    outgoing_value <-
        tbi_trade_draft_value(
            outgoing_assets
        )
    
    incoming_value <-
        tbi_trade_draft_value(
            incoming_assets
        )
    
    
    value_delta <-
        if (
            is.na(outgoing_value) ||
            is.na(incoming_value)
        ) {
            
            NA_real_
            
        } else {
            
            incoming_value -
                outgoing_value
        }
    
    
    blocks <-
        character()
    
    reviews <-
        character()
    
    
    # ----------------------------------------------------------
    # Combine asset frames safely
    # ----------------------------------------------------------
    
    if (
        outgoing_count &&
        incoming_count
    ) {
        
        columns <-
            union(
                names(outgoing_assets),
                names(incoming_assets)
            )
        
        
        normalize_columns <- function(
        x,
        columns) {
            
            missing <-
                setdiff(
                    columns,
                    names(x)
                )
            
            
            for (nm in missing) {
                
                x[[nm]] <- NA
            }
            
            
            x[
                ,
                columns,
                drop = FALSE
            ]
        }
        
        
        all_assets <-
            rbind(
                normalize_columns(
                    outgoing_assets,
                    columns
                ),
                normalize_columns(
                    incoming_assets,
                    columns
                )
            )
        
    } else if (outgoing_count) {
        
        all_assets <-
            outgoing_assets
        
    } else {
        
        all_assets <-
            incoming_assets
    }
    
    
    # ----------------------------------------------------------
    # Same database asset twice
    # ----------------------------------------------------------
    
    ids <-
        suppressWarnings(
            as.integer(
                all_assets$draft_asset_id
            )
        )
    
    
    if (
        any(
            duplicated(
                ids[
                    !is.na(ids)
                ]
            )
        )
    ) {
        
        blocks <-
            c(
                blocks,
                "The same draft asset appears on both sides of the proposed transaction."
            )
    }
    
    
    # ----------------------------------------------------------
    # Same physical pick twice
    # ----------------------------------------------------------
    
    physical_keys <-
        tbi_trade_underlying_pick_key(
            all_assets
        )
    
    
    duplicate_physical <-
        physical_keys[
            duplicated(
                physical_keys
            )
        ]
    
    
    if (length(duplicate_physical)) {
        
        blocks <-
            c(
                blocks,
                paste0(
                    "Duplicate underlying physical pick detected: ",
                    paste(
                        unique(
                            duplicate_physical
                        ),
                        collapse = ", "
                    ),
                    "."
                )
            )
    }
    
    
    # ----------------------------------------------------------
    # Source / network review
    # ----------------------------------------------------------
    
    selected_complex <-
        tbi_trade_draft_asset_complexity(
            all_assets
        )
    
    
    if (
        any(
            selected_complex,
            na.rm = TRUE
        )
    ) {
        
        reviews <-
            c(
                reviews,
                "The package contains at least one draft asset with source, protection, conveyance, or swap complexity."
            )
    }
    
    
    # ----------------------------------------------------------
    # Infer team IDs if necessary
    # ----------------------------------------------------------
    
    if (
        (
            is.null(team_a) ||
            !length(team_a) ||
            is.na(team_a) ||
            !nzchar(
                as.character(
                    team_a
                )
            )
        ) &&
        outgoing_count &&
        "current_team" %in%
        names(outgoing_assets)
    ) {
        
        team_a <-
            as.character(
                outgoing_assets$current_team[[1]]
            )
    }
    
    
    if (
        (
            is.null(team_b) ||
            !length(team_b) ||
            is.na(team_b) ||
            !nzchar(
                as.character(
                    team_b
                )
            )
        ) &&
        incoming_count &&
        "current_team" %in%
        names(incoming_assets)
    ) {
        
        team_b <-
            as.character(
                incoming_assets$current_team[[1]]
            )
    }
    
    
    # ----------------------------------------------------------
    # Team A rule evaluation
    # ----------------------------------------------------------
    
    team_a_rules <-
        if (
            outgoing_count &&
            !is.null(team_a) &&
            length(team_a) &&
            !is.na(team_a) &&
            nzchar(
                as.character(
                    team_a
                )
            )
        ) {
            
            tbi_trade_team_draft_rules(
                team_name =
                    as.character(
                        team_a
                    ),
                sent_assets =
                    outgoing_assets,
                received_assets =
                    incoming_assets,
                season =
                    season,
                db_path =
                    db_path
            )
            
        } else {
            
            NULL
        }
    
    
    # ----------------------------------------------------------
    # Team B rule evaluation
    # ----------------------------------------------------------
    
    team_b_rules <-
        if (
            incoming_count &&
            !is.null(team_b) &&
            length(team_b) &&
            !is.na(team_b) &&
            nzchar(
                as.character(
                    team_b
                )
            )
        ) {
            
            tbi_trade_team_draft_rules(
                team_name =
                    as.character(
                        team_b
                    ),
                sent_assets =
                    incoming_assets,
                received_assets =
                    outgoing_assets,
                season =
                    season,
                db_path =
                    db_path
            )
            
        } else {
            
            NULL
        }
    
    
    rule_results <-
        Filter(
            Negate(
                is.null
            ),
            list(
                team_a_rules,
                team_b_rules
            )
        )
    
    
    for (result in rule_results) {
        
        if (
            identical(
                result$status,
                "BLOCK"
            )
        ) {
            
            blocks <-
                c(
                    blocks,
                    result$issues
                )
            
        } else if (
            identical(
                result$status,
                "REVIEW"
            )
        ) {
            
            reviews <-
                c(
                    reviews,
                    result$issues
                )
        }
    }
    
    
    blocks <-
        unique(
            blocks
        )
    
    reviews <-
        unique(
            reviews
        )
    
    
    status <-
        if (length(blocks)) {
            
            "BLOCK"
            
        } else if (length(reviews)) {
            
            "REVIEW"
            
        } else {
            
            "PASS"
        }
    
    
    issues <-
        unique(
            c(
                blocks,
                reviews
            )
        )
    
    
    detail <-
        if (length(issues)) {
            
            paste(
                issues,
                collapse = " "
            )
            
        } else {
            
            paste(
                "The modeled package clears the seven-year horizon and",
                "Stepien first-round-control screen with no loaded draft",
                "asset complexity requiring manual review."
            )
        }
    
    
    list(
        status = status,
        requires_manual_review =
            identical(
                status,
                "REVIEW"
            ),
        blocked =
            identical(
                status,
                "BLOCK"
            ),
        issues = issues,
        outgoing_count =
            outgoing_count,
        incoming_count =
            incoming_count,
        outgoing_value =
            outgoing_value,
        incoming_value =
            incoming_value,
        value_delta =
            value_delta,
        team_a_rules =
            team_a_rules,
        team_b_rules =
            team_b_rules,
        summary =
            paste0(
                "Draft package: ",
                outgoing_count,
                " outgoing / ",
                incoming_count,
                " incoming. Draft rules result: ",
                status,
                ". ",
                detail
            )
    )
}
