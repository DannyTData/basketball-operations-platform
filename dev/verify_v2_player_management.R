cat("\n============================================================\n")
cat("VERSION 2.0 PLAYER MANAGEMENT VERIFICATION\n")
cat("============================================================\n")

devtools::load_all(quiet = FALSE)
required <- c(
  "mod_player_manager_ui", "mod_player_manager_server",
  "get_player_manager_pool", "save_player_manager_record",
  "position_value_v2"
)
missing <- required[!vapply(required, exists, logical(1), mode = "function")]
if (length(missing)) stop("Missing functions: ", paste(missing, collapse = ", "))

testthat::test_file("tests/testthat/test-player_manager.R")
cat("VERSION 2.0 PLAYER MANAGEMENT STATUS: PASS\n")
