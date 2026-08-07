# Create and verify the recommended indexes for the authoritative TBI database.
# Run from the basketballops project root:
# source("dev/create_database_indexes.R")

source(file.path("R", "database.R"))
source(file.path("R", "db_validation.R"))

con <- connect_db()
on.exit(disconnect_db(con), add = TRUE)
create_tbi_indexes(con)

cat("Recommended TBI database indexes created successfully.\n")
