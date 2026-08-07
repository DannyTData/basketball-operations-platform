library(DBI)
library(RSQLite)

# Create database folder if it doesn't exist
if (!dir.exists("inst/database")) {
  dir.create("inst/database", recursive = TRUE)
}

# Database path
db_path <- "inst/database/tbi.sqlite"

# Connect to SQLite
con <- dbConnect(SQLite(), db_path)

# Turn on foreign keys
dbExecute(con, "PRAGMA foreign_keys = ON;")

cat("Connected to:", db_path, "\n")

# ============================================================
# Thompson Basketball Intelligence
# Database Initialization
# ============================================================

library(DBI)
library(RSQLite)

# ------------------------------------------------------------
# Create database folder
# ------------------------------------------------------------

db_folder <- "inst/database"

if (!dir.exists(db_folder)) {
  dir.create(db_folder, recursive = TRUE)
}

db_path <- file.path(db_folder, "tbi.sqlite")

# ------------------------------------------------------------
# Connect
# ------------------------------------------------------------

con <- dbConnect(
  SQLite(),
  db_path
)

dbExecute(con, "PRAGMA foreign_keys = ON;")

cat("=====================================\n")
cat("Connected Successfully!\n")
cat("Database:", db_path, "\n")
cat("=====================================\n")
# ============================================================
# Create core database tables
# ============================================================

# ------------------------------------------------------------
# Data sources
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS data_sources (
    source_id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_name TEXT NOT NULL,
    source_reference TEXT,
    access_method TEXT NOT NULL DEFAULT 'Manual',
    usage_notes TEXT,
    checked_at TEXT,
    verification_status TEXT NOT NULL DEFAULT 'Unverified'
  );
  "
)

# ------------------------------------------------------------
# Teams
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS teams (
    team_id INTEGER PRIMARY KEY,
    team_name TEXT NOT NULL UNIQUE,
    abbreviation TEXT NOT NULL UNIQUE,
    conference TEXT NOT NULL,
    division TEXT NOT NULL,
    is_active INTEGER NOT NULL DEFAULT 1
  );
  "
)

# ------------------------------------------------------------
# Players
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS players (
    player_id INTEGER PRIMARY KEY AUTOINCREMENT,
    player_name TEXT NOT NULL,
    birth_date TEXT,
    height_inches REAL,
    weight_lbs REAL,
    primary_position TEXT,
    nba_player_id TEXT,
    is_active INTEGER NOT NULL DEFAULT 1,
    created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
  );
  "
)

# ------------------------------------------------------------
# Roster history
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS roster_history (
    roster_id INTEGER PRIMARY KEY AUTOINCREMENT,
    player_id INTEGER NOT NULL,
    team_id INTEGER NOT NULL,
    season TEXT NOT NULL,
    roster_status TEXT NOT NULL DEFAULT 'Active',
    start_date TEXT NOT NULL,
    end_date TEXT,
    two_way_flag INTEGER NOT NULL DEFAULT 0,
    jersey_number TEXT,
    source_id INTEGER,
    created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY(player_id)
      REFERENCES players(player_id),

    FOREIGN KEY(team_id)
      REFERENCES teams(team_id),

    FOREIGN KEY(source_id)
      REFERENCES data_sources(source_id)
  );
  "
)

# ------------------------------------------------------------
# Contracts
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS contracts (
    contract_id INTEGER PRIMARY KEY AUTOINCREMENT,
    player_id INTEGER NOT NULL,
    contract_start_season TEXT,
    contract_end_season TEXT,
    contract_type TEXT,
    total_value REAL,
    guaranteed_value REAL,
    free_agent_year INTEGER,
    bird_rights TEXT,
    trade_bonus_percent REAL,
    notes TEXT,
    source_id INTEGER,
    created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY(player_id)
      REFERENCES players(player_id),

    FOREIGN KEY(source_id)
      REFERENCES data_sources(source_id)
  );
  "
)

# ------------------------------------------------------------
# Contract years
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS contract_years (
    contract_year_id INTEGER PRIMARY KEY AUTOINCREMENT,
    contract_id INTEGER NOT NULL,
    player_id INTEGER NOT NULL,
    team_id INTEGER NOT NULL,
    season TEXT NOT NULL,
    base_salary REAL NOT NULL DEFAULT 0,
    cap_hit REAL NOT NULL DEFAULT 0,
    guaranteed_amount REAL NOT NULL DEFAULT 0,
    option_type TEXT,
    likely_incentives REAL NOT NULL DEFAULT 0,
    unlikely_incentives REAL NOT NULL DEFAULT 0,
    dead_cap REAL NOT NULL DEFAULT 0,
    source_id INTEGER,
    verified_at TEXT,

    FOREIGN KEY(contract_id)
      REFERENCES contracts(contract_id),

    FOREIGN KEY(player_id)
      REFERENCES players(player_id),

    FOREIGN KEY(team_id)
      REFERENCES teams(team_id),

    FOREIGN KEY(source_id)
      REFERENCES data_sources(source_id),

    UNIQUE(contract_id, season)
  );
  "
)

# ------------------------------------------------------------
# Transactions
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS transactions (
    transaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
    transaction_date TEXT NOT NULL,
    transaction_type TEXT NOT NULL,
    player_id INTEGER,
    from_team_id INTEGER,
    to_team_id INTEGER,
    season TEXT,
    transaction_details TEXT,
    source_id INTEGER,
    created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY(player_id)
      REFERENCES players(player_id),

    FOREIGN KEY(from_team_id)
      REFERENCES teams(team_id),

    FOREIGN KEY(to_team_id)
      REFERENCES teams(team_id),

    FOREIGN KEY(source_id)
      REFERENCES data_sources(source_id)
  );
  "
)

# ------------------------------------------------------------
# Cap thresholds
# ------------------------------------------------------------

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS cap_thresholds (
    season TEXT PRIMARY KEY,
    salary_cap REAL NOT NULL,
    luxury_tax REAL NOT NULL,
    first_apron REAL,
    second_apron REAL,
    minimum_team_salary REAL,
    source_id INTEGER,
    verified_at TEXT,

    FOREIGN KEY(source_id)
      REFERENCES data_sources(source_id)
  );
  "
)

cat("Core tables created successfully!\n")

# ============================================================
# Insert NBA Teams
# ============================================================

nba_teams <- data.frame(
  team_id = 1:30,
  
  team_name = c(
    "Atlanta Hawks",
    "Boston Celtics",
    "Brooklyn Nets",
    "Charlotte Hornets",
    "Chicago Bulls",
    "Cleveland Cavaliers",
    "Dallas Mavericks",
    "Denver Nuggets",
    "Detroit Pistons",
    "Golden State Warriors",
    "Houston Rockets",
    "Indiana Pacers",
    "Los Angeles Clippers",
    "Los Angeles Lakers",
    "Memphis Grizzlies",
    "Miami Heat",
    "Milwaukee Bucks",
    "Minnesota Timberwolves",
    "New Orleans Pelicans",
    "New York Knicks",
    "Oklahoma City Thunder",
    "Orlando Magic",
    "Philadelphia 76ers",
    "Phoenix Suns",
    "Portland Trail Blazers",
    "Sacramento Kings",
    "San Antonio Spurs",
    "Toronto Raptors",
    "Utah Jazz",
    "Washington Wizards"
  ),
  
  abbreviation = c(
    "ATL","BOS","BKN","CHA","CHI",
    "CLE","DAL","DEN","DET","GSW",
    "HOU","IND","LAC","LAL","MEM",
    "MIA","MIL","MIN","NOP","NYK",
    "OKC","ORL","PHI","PHX","POR",
    "SAC","SAS","TOR","UTA","WAS"
  ),
  
  conference = c(
    "Eastern","Eastern","Eastern","Eastern","Eastern",
    "Eastern","Western","Western","Eastern","Western",
    "Western","Eastern","Western","Western","Western",
    "Eastern","Eastern","Western","Western","Eastern",
    "Western","Eastern","Eastern","Western","Western",
    "Western","Western","Eastern","Western","Eastern"
  ),
  
  division = c(
    "Southeast",
    "Atlantic",
    "Atlantic",
    "Southeast",
    "Central",
    "Central",
    "Southwest",
    "Northwest",
    "Central",
    "Pacific",
    "Southwest",
    "Central",
    "Pacific",
    "Pacific",
    "Southwest",
    "Southeast",
    "Central",
    "Northwest",
    "Southwest",
    "Atlantic",
    "Northwest",
    "Southeast",
    "Atlantic",
    "Pacific",
    "Northwest",
    "Pacific",
    "Southwest",
    "Atlantic",
    "Northwest",
    "Southeast"
  ),
  
  is_active = 1
)

dbWriteTable(
  con,
  "teams",
  nba_teams,
  append = TRUE,
  row.names = FALSE
)

cat(nrow(nba_teams), "NBA teams loaded!\n")

dbDisconnect(con)

cat("Database connection closed.\n")