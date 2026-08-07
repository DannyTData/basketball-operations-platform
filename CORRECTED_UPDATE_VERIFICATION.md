# Corrected Update Verification

This build was created from the uploaded **Depth Chart Polished** master.

## Verified in the packaged files

- `R/database.R`: shared depth-chart override logic is active.
- `R/mod_roster_contracts.R`: roster grouping, salary sorting, years left, and money remaining are active.
- `R/mod_extension_simulator.R`: Year 1 salary updates when the selected player changes.
- `R/mod_executive_dashboard.R`: missing DuckDB no longer causes repeated dashboard errors.
- `inst/database/tbi.sqlite`: 588 players, 574 roster rows, 574 depth-chart rows, and 6 manual overrides.
- Jalen Duren and Peyton Watson are included as qualifying-offer planning rows without invented salary values.

## Runtime note

The edited project was structurally and database-validated in the build environment. Final Shiny runtime validation must be completed in RStudio with `golem::run_dev()`.
