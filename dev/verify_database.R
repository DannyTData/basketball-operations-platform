# Validate the authoritative TBI database.
# Run from the basketballops project root:
# source("dev/verify_database.R")

source(file.path("R", "database.R"))
source(file.path("R", "db_validation.R"))

report <- validate_tbi_database(create_indexes = FALSE)
print_tbi_database_report(report)

if (!isTRUE(report$passed)) {
  stop(
    "TBI database verification found one or more issues. Review the report above.",
    call. = FALSE
  )
}
