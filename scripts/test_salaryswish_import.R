# scripts/test_salaryswish_import.R

# Load the functions stored in the R folder
source("R/salaryswish.R")
source("R/clean_salaryswish.R")
source("R/import_salaryswish.R")

# Download and clean all SalarySwish pages
players <- update_salaryswish_data()

# Confirm the import worked
print(nrow(players))
print(names(players))

# Inspect the cleaned data
dplyr::glimpse(players)
View(players)