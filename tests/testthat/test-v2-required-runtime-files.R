testthat::test_that("required V2 runtime files remain present and wired", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  required <- c(
    "R/v2_presentation.R",
    "R/v2_trade_intelligence.R",
    "R/v2_transaction_foundation.R",
    "inst/app/www/tbi_phase3.css"
  )

  testthat::expect_true(all(file.exists(file.path(root, required))))

  ui <- paste(readLines(file.path(root, "R", "app_ui.R"), warn = FALSE), collapse = "\n")
  server <- paste(readLines(file.path(root, "R", "app_server.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(ui, 'app_sys("app/www/tbi_phase3.css")', fixed = TRUE)
  testthat::expect_match(ui, "v2_trade_intelligence_ui(", fixed = TRUE)
  testthat::expect_match(server, "v2_ui_global_context(", fixed = TRUE)
  testthat::expect_true(exists("normalize_transaction_graph", mode = "function"))
  testthat::expect_true(exists("v2_ui_global_context", mode = "function"))
  testthat::expect_true(exists("v2_trade_intelligence_ui", mode = "function"))
})

testthat::test_that("direct V2 runtime dependencies exclude display-only scales", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  imports <- trimws(unlist(strsplit(description[[1L, "Imports"]], ",", fixed = TRUE)))
  imports <- sub("\\s*\\(.*$", "", imports)
  testthat::expect_true("htmltools" %in% imports)
  testthat::expect_false("scales" %in% imports)

  runtime_files <- list.files(
    file.path(root, "R"),
    pattern = "[.]R$",
    full.names = TRUE
  )
  runtime_source <- paste(
    unlist(lapply(runtime_files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
  testthat::expect_false(grepl("scales::", runtime_source, fixed = TRUE))

  lock <- paste(readLines(file.path(root, "renv.lock"), warn = FALSE), collapse = "\n")
  testthat::expect_match(lock, '(?m)^    "htmltools": \\{', perl = TRUE)
  testthat::expect_false(grepl('(?m)^    "scales": \\{', lock, perl = TRUE))
})
