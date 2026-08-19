# Thompson's Basketball Intelligence Architecture

## Product mission

TBI is a session-oriented R/Shiny decision-support application for NBA
basketball operations. It combines roster, player, lineup, salary-cap, trade,
draft, contract, and long-range planning evidence without writing scenario
work back to the production database.

## Runtime layers

1. `R/app_ui.R` owns the application shell, top navigation, global team/season
   controls, and the module mount points.
2. `R/app_server.R` owns selected team/season state, navigation, CBA deep links,
   and the session-scoped transaction state.
3. `R/mod_*.R` files own module UI and server orchestration.
4. Pure engines own reusable calculations. Examples include
   `trade_engine.R`, `minute_allocation_engine.R`,
   `lineup_optimization_engine.R`, `draft_assets_engine.R`, and
   `draft_simulation_engine.R`.
5. Database helpers use `connect_db(read_only = TRUE)` for application reads.
   The SQLite database is the source of truth; scenario state remains in the
   Shiny session.
6. `inst/app/www/tbi_ux_foundation.js` coordinates all approved internal
   sub-tabs through one shared `MutationObserver`. Its paired stylesheet owns
   the common responsive visual language.

## Session state flow

```text
selected team + selected season
              |
              +--> roster / contracts / payroll / draft assets
              |
              +--> player and team evidence --> BIE readouts
              |
              +--> session transaction scenario
                         |
                         +--> roster/payroll previews
                         +--> trade/CBA screen
                         +--> scenario comparison
```

The transaction object in `R/transaction_state.R` is session-scoped. It stores
a two-team proposal, selected players and draft assets, salary deltas, and
evaluation results. It never writes to the database.

## Module map

| Workspace | Primary responsibility | Principal implementation |
|---|---|---|
| Command Center | Executive decision summary and cross-domain scorecards | `R/mod_executive_dashboard.R` |
| Team Overview | Team context, profile, risks, and recommendation | `R/mod_team_overview.R` |
| Roster Intelligence | Roster construction and contract inventory | `R/mod_roster_contracts.R` |
| Depth Chart | Approved starters, depth, rotation, minutes, and lineup views | `R/mod_depth_chart.R` |
| Player Management | Player evidence, value, development, contract, and recommendation | `R/mod_player_manager.R` |
| Cap Intelligence | Payroll, cap thresholds, exceptions, and flexibility | `R/mod_salary_cap.R` |
| Extension Simulator | Extension eligibility, proposal, CBA screen, and financial schedule | `R/mod_extension_simulator.R` |
| Trade Intelligence | Two-team proposal, salary screen, draft screen, BIE impact, and recommendation | `R/mod_trade_analyzer.R` |
| Draft Intelligence | Draft portfolio, controls, risk, timeline, and simulation | `R/mod_draft_assets.R` |
| Five-Year Outlook | Multi-year contracts, salary, draft control, flexibility, and posture | `R/mod_five_year_outlook.R` |
| CBA Info Hub | Searchable terminology, references, aliases, and contextual deep links | `R/mod_cba_glossary.R` |

## Frozen basketball architecture

TBI V1 basketball evaluation behavior is frozen. In particular:

- `bie_freeze_status()` must report `frozen = TRUE`.
- BIE formulas, weights, and reliability rules may not change in-place.
- the starting-five optimizer remains 65% player quality, 25% position fit,
  and 10% lineup balance;
- approved starter locks, lineup legality, candidate order, the preseason
  rookie gate, and database source-of-truth behavior remain authoritative.

Any basketball-architecture change requires a new model version and a frozen
reference/equivalence suite before production routing changes.

## UI and performance architecture

Internal sub-tabs only reorganize mounted content. They preserve existing
Shiny IDs and keep stateful controls mounted. Server-visible tab state may gate
heavy, read-only outputs, but it must never gate required starter, lineup,
rotation-save, trade-selection, CBA, or scenario synchronization.

Stable session reads may be cached when the complete invalidation key is known.
Cross-session persistent caches are not part of the current architecture.

## Deployment notes

- Run with the repository's `renv` library synchronized to the deployed R
  runtime.
- Configure the database path outside code and give the app read access.
- Do not expose database backups, QA exports, tokens, or local environment
  files in the deployed bundle.
- Parse changed R files, run `devtools::load_all()`, focused characterization
  tests, the shared UX tests, and a disposable-database browser smoke test.
- Confirm asset fingerprints change whenever shared CSS or JavaScript changes.
- Confirm `bie_freeze_status()` before release.

The current release-hardening evidence and manual checklist are recorded in
`docs/release-hardening-2026-08-18.md`.
