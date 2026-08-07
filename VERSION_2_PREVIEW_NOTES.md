# Basketball Operations Platform v2.0 Preview

This upgraded working copy includes:

- Compact desktop-style workspace with internal page scrolling
- Compact Trade Intelligence player lists
- Player Management navigation page
- Player Profile v1
- Safe player editing for name, age, birth date, height, weight, primary and eligible positions, roster status, jersey number, two-way status, and active status
- Position Value 2.0, replacing the roster-count-only score
- Existing CBA Trade Intelligence, lineup overrides, qualifying-offer logic, engines, modules, and verification scripts preserved
- New Player Management tests and verification script

## Verify in RStudio

```r
devtools::load_all()
source("dev/verify_v2_player_management.R")
run_app()
```

## Position Value 2.0 scope

The score is a transparent roster-construction score based on currently available platform data: assigned coverage, depth, age timeline, contract balance, and positional flexibility. It does not claim to measure on-court player quality until a reliable performance-statistics feed is connected.
