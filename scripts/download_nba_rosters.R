# ============================================================
# Thompson Basketball Intelligence
# Download Current NBA Rosters from NBA.com
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)

nba_url <- "https://www.nba.com/players"

output_directory <- file.path("inst", "extdata")
output_file <- file.path(
  output_directory,
  "nba_rosters_raw.csv"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("Reading NBA.com league roster...\n")

page <- read_html(nba_url)

tables <- html_table(
  page,
  fill = TRUE
)

cat("Tables found:", length(tables), "\n")

if (length(tables) == 0) {
  stop("No tables were found on the NBA.com players page.")
}

# Find the table containing Player and Team columns
table_match <- which(
  vapply(
    tables,
    function(x) {
      all(c("Player", "Team") %in% names(x))
    },
    logical(1)
  )
)

if (length(table_match) == 0) {
  stop(
    paste(
      "The NBA roster table was not found.",
      "NBA.com may have changed the page structure."
    )
  )
}

nba_rosters <- tables[[table_match[1]]] |>
  rename_with(
    ~ str_to_lower(
      str_replace_all(.x, "[^A-Za-z0-9]+", "_")
    )
  ) |>
  rename_with(
    ~ str_remove(.x, "_$")
  ) |>
  mutate(
    player = str_squish(player),
    team = str_squish(team),
    number = str_squish(as.character(number)),
    position = str_squish(position),
    height = str_squish(height),
    weight = str_squish(weight),
    downloaded_at = as.character(Sys.time())
  ) |>
  filter(
    !is.na(player),
    player != "",
    !is.na(team),
    team != ""
  ) |>
  distinct(
    player,
    team,
    .keep_all = TRUE
  )

write_csv(
  nba_rosters,
  output_file,
  na = ""
)

cat("\n=====================================\n")
cat("NBA roster download complete!\n")
cat("Players returned:", nrow(nba_rosters), "\n")
cat(
  "Teams returned:",
  n_distinct(nba_rosters$team),
  "\n"
)
cat("Saved to:", output_file, "\n")
cat("=====================================\n")