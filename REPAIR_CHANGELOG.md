# Thompson Basketball Intelligence — Repair Summary

## Repair scope completed

### Roster Intelligence
- Rebuilt `R/mod_depth_chart.R` to remove the duplicated/incomplete SQL and reduce the module to one coherent implementation.
- Added a single shared depth-chart query in `R/database.R`.
- Updated `R/mod_roster_contracts.R` so its depth chart uses the shared database source instead of maintaining a second SQL implementation.
- Standardized SQLite path resolution and safe connection cleanup.
- Preserved the player intelligence panel, roster cards, depth ordering, starter/reserve handling, and empty-state behavior.

### Executive Dashboard
- Added a development/package-safe DuckDB path resolver so the dashboard can locate its standings database both in RStudio and when installed.
- Removed a duplicated payroll-tier condition.
- Corrected the decision-outlook logic so recommendations use payroll rank rather than comparing against status labels that could never occur.
- Preserved the dashboard cards, conference table, team selection, and payroll functions.

### Database layer
- Added `resolve_tbi_db_path()` for one consistent SQLite location strategy.
- Added `get_depth_chart_records()` as the source of truth for both depth-chart displays.
- Added position normalization and safe connection shutdown.

## Files changed
- `R/database.R`
- `R/mod_depth_chart.R`
- `R/mod_roster_contracts.R`
- `R/mod_executive_dashboard.R`

## First launch
1. Open `basketball-operations-platform.Rproj` in RStudio.
2. Run `renv::restore()` only if R reports missing packages.
3. Run `golem::run_dev()` or source `dev/run_dev.R`.
4. Test Executive Dashboard and Roster Intelligence first.

The project database contains populated teams, players, roster history, contracts, contract years, and depth-chart records. Cap-threshold and transaction tables remain available for future feature population.

## Interview framework pass
- Replaced Salary Cap placeholder with a live payroll/contract ledger and flexibility signals.
- Replaced Trade placeholder with an interactive two-team salary scenario framework.
- Replaced Draft placeholder with a working session-based draft asset register.
- Passed shared organization and season filters into all three modules.
- Replaced unsupported Bootstrap icon names used by the repaired modules.

## Lightweight Functionality Pass

Added interview-ready minimum viable functionality to the three remaining placeholder modules:

- `R/mod_team_overview.R`: live roster count, payroll, average age, highest-paid player, team identity, organizational direction, executive summary, and top salary commitments.
- `R/mod_extension_simulator.R`: live player selector, years, starting salary, annual raise, guarantee structure, total value, average salary, final-year salary, and year-by-year schedule.
- `R/mod_five_year_outlook.R`: current payroll, contract-control horizon, free-agent wave, five-season estimated commitments, control-rate table, and competitive-window signals.
- `R/app_server.R`: passes the shared selected team and season into all three modules.
- `inst/app/www/tbi.css`: adds callout and five-year outlook presentation styles.

These are intentionally framework-level tools. Advanced CBA eligibility rules, verified future cap thresholds, draft-capital projections, and performance forecasting remain future development layers.

## Corrected roster, depth-chart, and contract update

- Added a persistent `depth_chart_overrides` table and applied overrides for LeBron James (PF starter), Julius Randle (PF starter), Ausar Thompson (SF starter), CJ McCollum (SG starter), and Paul George (SF rotation).
- Added Jalen Duren to Detroit and Peyton Watson to Denver as qualifying-offer planning rows. No salary was fabricated for either player.
- Roster Intelligence now groups standard, two-way, Exhibit 10, qualifying-offer, and restricted-free-agent personnel; standard contracts are sorted by salary.
- Added years remaining and money remaining. Values are labeled `est.` when future annual contract rows are not loaded.
- Extension Simulator now updates Year 1 salary whenever the selected player changes.
- Executive Dashboard now fails gracefully when DuckDB is unavailable rather than producing repeated red runtime errors.
- Replaced unsupported Bootstrap icon names in the updated modules.

## 2026-07-28 — Editable Depth Chart and Cap Threshold Update
- Added computed conference-rank fallback when the standings feed leaves rank blank.
- Added user-editable depth-chart assignments with eligible-position controls, starter status, depth order, persistent SQLite overrides, and reset support.
- Added visible QO/RFA/2W planning-roster labels to depth-chart cards.
- Added official 2025-26 and 2026-27 salary cap, luxury-tax, first-apron, second-apron, and minimum-team-salary thresholds.
- Added team payroll threshold cards and player salary percentages of the cap and apron levels.
