# ============================================================
# Phase 13 Tests: Performance + Loading Optimization
# ============================================================

testthat::test_that(
  "roster signature is stable to row order",
  {
    d <- data.frame(
      player_id = c(2, 1),
      player_name = c("B", "A"),
      position = c("SG", "PG"),
      stringsAsFactors = FALSE
    )
    
    testthat::expect_identical(
      phase13_roster_signature(d),
      phase13_roster_signature(
        d[c(2, 1), , drop = FALSE]
      )
    )
  }
)


testthat::test_that(
  "roster signature changes with basketball inputs",
  {
    d <- data.frame(
      player_id = 1:2,
      player_name = c("A", "B"),
      bie_rating = c(80, 70)
    )
    
    d2 <- d
    d2$bie_rating[[2]] <- 75
    
    testthat::expect_false(
      identical(
        phase13_roster_signature(d),
        phase13_roster_signature(d2)
      )
    )
  }
)


testthat::test_that(
  "trade signature tracks incoming and outgoing players",
  {
    s1 <- list(
      team = "A",
      partner_team = "B",
      outgoing_players =
        data.frame(player_id = 1),
      incoming_players =
        data.frame(player_id = 2),
      outgoing_salary = 10,
      incoming_salary = 12
    )
    
    s2 <- s1
    s2$incoming_players <-
      data.frame(player_id = 3)
    
    testthat::expect_false(
      identical(
        phase13_trade_signature(s1),
        phase13_trade_signature(s2)
      )
    )
  }
)


testthat::test_that(
  "cache stores and retrieves exact key",
  {
    cache <- phase13_cache_new()
    
    cache <- phase13_cache_store(
      cache,
      "abc",
      list(value = 1)
    )
    
    testthat::expect_equal(
      phase13_cache_get(
        cache,
        "abc"
      )$value,
      1
    )
    
    testthat::expect_null(
      phase13_cache_get(
        cache,
        "xyz"
      )
    )
  }
)


testthat::test_that(
  "performance status calculates hit rate",
  {
    a <- phase13_cache_new()
    b <- phase13_cache_new()
    
    a$hits <- 3L
    a$misses <- 1L
    b$hits <- 1L
    b$misses <- 1L
    
    status <-
      phase13_performance_status(
        list(a, b)
      )
    
    testthat::expect_equal(
      status$hits,
      4L
    )
    
    testthat::expect_equal(
      status$misses,
      2L
    )
    
    testthat::expect_equal(
      status$hit_rate,
      4 / 6
    )
  }
)