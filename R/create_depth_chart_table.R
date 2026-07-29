library(DBI)
library(RSQLite)

db <- dbConnect(
  SQLite(),
  "inst/database/tbi.sqlite"
)

dbExecute(
  db,
  "
CREATE TABLE IF NOT EXISTS depth_chart (

team_name TEXT NOT NULL,

player_name TEXT NOT NULL,

position TEXT NOT NULL,

depth_order INTEGER NOT NULL,

is_starter INTEGER DEFAULT 0,

updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

PRIMARY KEY(team_name, player_name)

);
"
)

dbDisconnect(db)

message("Depth chart table created.")