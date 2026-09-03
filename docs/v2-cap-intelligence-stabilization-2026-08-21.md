# TBI V2 Cap Intelligence Stabilization — 2026-08-21

## Status

CAP INTELLIGENCE READY FOR MANUAL QA

Cap Intelligence now presents six distinct financial workspaces without changing the protected cap engine, BIE formulas, transaction rules, database, or other top-level pages. The approved Free Agent Market is a read-only projection of loaded contract-year, player, team, and stored BIE data.

## Files changed

- `R/mod_salary_cap.R` — explicit six-workspace UI, loaded-data market helpers/query/filters, selected-team executive summaries, contract quality display, and financial recommendation presentation.
- `inst/app/www/tbi_ux_foundation.js` — Cap-only six-tab routing, explicit target support, session restoration, ARIA state, and shared Shiny subtab notification.
- `inst/app/www/tbi_ux_foundation.css` — Cap-only density, market toolbar/table, responsive grids, dark status tokens, and recommendation styling.
- `tests/testthat/test-v2-cap-intelligence-stabilization.R` — proof-first market, filter/sort, evidence-quality, UI contract, live module, and team-switch coverage.
- `docs/v2-cap-intelligence-stabilization-2026-08-21.md` — this completion record.

## Overview result

Overview combines the existing financial snapshot with three compact panels: current money position, apron/flexibility, and team-building watch. Every value is derived from existing payroll, threshold, exception, contract, and operating-status reactives.

## Decision & Thresholds result

The existing cap/tax/first-apron/second-apron scorecard and financial operating decision remain authoritative. The composition is compacted into a balanced decision/threshold workspace; no new transaction permission or CBA rule is inferred.

## Contracts & Commitments result

The existing team contract ledger and front-office readout remain table-led. Its option column is explicitly labeled as the selected-year option. Contract rows use the existing governed contract-quality helper, so known but unreconciled facts show `REQUIRES REVIEW` rather than being presented as current.

## Free Agent Market result

The market reads the selected season's league-wide contract-year population. It aggregates option facts across each full loaded contract and joins only the governed prior performance season's canonical BIE row, ordered by minutes, games, then team ID exactly as the existing roster evidence selector does. The table displays BIE season and confidence. It does not scrape, predict a contract, calculate market value, or mutate data.

Loaded 2026-27 market facts:

- 572 player rows
- free-agent years: 2027, 2028, 2029, 2030, 2031
- 195 rows have every loaded contract-window field and no loaded option condition
- 225 contracts contain a loaded option in the selected or a future contract year and remain conditional / `REVIEW`
- 168 rows have at least one incomplete contract-window field and remain `UNKNOWN` / `REVIEW` (this count can overlap the option population)
- all known-but-unreconciled rows remain `REQUIRES REVIEW`; field completeness is never presented as verification

The table shows player, position, age, current team, salary, contract, FA year/type, contract option, BIE, BIE grade/season/confidence, market status, and data quality. No league/position rank is shown because the task did not require a new comparison model and the loaded contract reconciliation does not establish a decision-ready ranking population.

## Filters and sorts tested

- two distinct loaded FA years
- Center token filtering, including multi-position `PF, C` rows
- loaded FA type
- loaded BIE grade
- player search
- current-team filter
- combined year + position + FA type
- BIE numeric descending
- salary numeric descending
- player alphabetical
- clear/default projection restores the unfiltered helper result

The current-team filter is explicit. Switching the selected Cap team does not implicitly narrow the league market.

## BIE context result

BIE is read from `player_season_impact.bie_performance_rating` for the governed prior performance season only (`2025-26` for the `2026-27` market). Multi-team evidence uses the existing canonical minutes/games/team ordering; five players without same-season evidence remain `UNKNOWN` rather than receiving stale `2024-25` values. Grade uses the existing frozen `bie_player_grade()` presentation contract. The full BIE equivalence suite passed. No BIE formula, weight, grade boundary, reliability rule, or player projection changed.

## Data-quality handling

- missing FA year/status, contract end, or verification timestamp -> `UNKNOWN` and market `REVIEW`
- option-dependent contract window, including a future-year option on the same contract -> `REQUIRES SOURCE VERIFICATION` and market `REVIEW`
- all loaded fields with no option -> the existing contract reconciliation status, which remains `REQUIRES REVIEW` unless a governed `CURRENT` status is supplied
- missing salary or BIE remains `UNKNOWN`; it is not converted to zero

`verified_at` is not treated as proof that a contract is current.

## Risks & Flexibility result

Existing CBA alerts, financial risks, and flexibility opportunities remain on one dedicated workspace. No new risk threshold or transaction rule was added.

## Recommendation result

Recommendation is presentation-only and consumes the same selected-team payroll, threshold, operating-band, flexibility, contract, exception, and market-window reactives as the other Cap workspaces. Its posture now reuses the exact `RESTRICTED` / `CAUTION` / `MANAGE` / `FLEXIBLE` decision mapping shown in Decision & Thresholds. Unsupported exception or option availability is labeled as requiring source verification. It does not create a new recommendation model.

## Team-switch result

The live module test switched Boston -> Oklahoma City -> Charlotte -> Cleveland. Team payroll/contracts/recommendation refreshed for every team. The league-wide market retained the same 572 player IDs and active filters remained market-scoped.

## Automated verification

- changed R parse: PASS
- `devtools::load_all()`: PASS
- proof-first focused Cap suite: PASS (red on missing market/UI seams before implementation; green after implementation)
- `test-v2-cap-intelligence-stabilization.R`: PASS
- `test-mod_salary_cap.R`: PASS
- `test-cap_engine.R`: PASS
- `test-mod_roster_contracts.R`: PASS
- `test-player-manager-bie-equivalence.R`: PASS
- `test-ux-foundation-assets.R`: PASS with existing sass-cache warnings only
- `test-database.R`: PASS
- Impeccable post-change layout detector: `[]`
- live Shiny startup: PASS at `http://127.0.0.1:4387/`
- HTTP smoke: 200, Shiny payload present
- JavaScript syntax check: PASS
- canonical market query check: PASS (572 distinct players, 225 contract options, 449 same-season BIE rows, no mixed BIE season)
- `git diff --check`: PASS (line-ending warnings only)
- staging index: empty

## Database safety

- SHA-256 before: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- SHA-256 after: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- size after: 11,296,768 bytes
- result: unchanged

## Remaining manual visual checks

Trusted screenshot/control automation was unavailable because the configured in-app browser service could not resolve from an approved trusted code path. No screenshot verification is claimed. Manual QA should verify:

1. Overview balance at 1920 and 1440.
2. All six Cap tabs, Arrow/Home/End keyboard navigation, persisted tab restoration, and filter preservation after a tab round-trip.
3. Free Agent Market year/position/type/team/search controls and Clear filters.
4. Contained table scrolling with no page-level overflow.
5. Recommendation team-specific refresh across the four tested teams.
6. Toolbar stacking near 900 and 390.
7. Mobile subtab rail, status-token wrapping, and table usability.

No other Cap feature or top-level page was started.
