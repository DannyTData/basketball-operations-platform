# TBI V2 Depth Chart Stabilization — 2026-08-21

## Status

FUNCTIONALLY READY

Depth Chart now has four distinct functional views: Depth Chart, Rotation,
Lineups, and Staggering / Game Plan. Valid V2 state renders actual players and
minutes; unavailable state names the failed dependency. V1 remains authoritative
and V2 remains development-visible and non-authoritative.

## Scenario cleanup

Root cause: Depth Chart's active-trade eligibility and partner-perspective logic
was embedded inside a Shiny reactive and had no mounted publication-to-reset
regression. That allowed stale presentation regressions to go undetected even
when the shared transaction state had been cleared.

Fix:

- `depth_chart_active_trade_scenario()` is the one pure resolver for active,
  matching-season Trade state.
- Partner-team perspective reverses teams, outgoing/incoming players, salaries,
  and salary delta deterministically.
- Incoming preview players are normalized to the selected team's canonical
  `team_id`; the V2 shadow never receives a mixed-team preview roster.
- All Depth Chart scenario banner, roster, starter, rotation, minute, lineup,
  and stagger state now invalidates from the canonical transaction snapshot.

Reset verification: the mounted module published a Boston-Charlotte preview,
showed the Trade banner and preview roster, remained `COMPLETED` in V2, then
cleared the transaction. The banner disappeared and roster, rotation members,
minute ledger, lineup portfolio, and stagger segments exactly matched their
pre-scenario baselines without a browser refresh.

## Depth Chart

The authoritative depth board remains isolated from the V2 plan views. Existing
PG/SG/SF/PF/C placement, starter locks, edit/save behavior, rookie handling, and
V1 logic were not changed. The V2 workspace is mounted separately and is hidden
when the Depth Chart subtab is active.

## Rotation

Root cause of the prior blank state: the V2 shadow was computed and returned by
the server, but the four-subtab client shell did not have a dedicated mounted
Depth Chart `uiOutput` rendering the Phase 2 plan. Switching to Rotation could
therefore hide the legacy shell without revealing meaningful V2 content.

Current result: Rotation consumes the existing `rotation_10` and governed minute
ledger. It renders Starting Five, ordered Bench Rotation, sixth man, actual
player names, individual minutes, availability, evidence status, and exact team
minutes. A downstream lineup/stagger error no longer hides valid upstream
rotation/minute output.

Tested teams: Boston Celtics, Charlotte Hornets, Cleveland Cavaliers, and
Portland Trail Blazers for season 2026-27.

240/240 result: all four mounted-team runs returned 10 rotation players and
exactly 240 assigned minutes.

## Player detail

Root cause of layering: the player rail and adjacent positioned board/court
surfaces lacked an explicit contained stacking relationship. The rail could be
painted beneath a neighboring Depth Chart surface.

Fix: the board/court and player rail remain separate grid columns at desktop;
the rail has a local isolated stacking context with a low `z-index` of 2 while
the board uses 1. At narrower widths the grid becomes separate rows. No extreme
global z-index was added. A compact Clear player detail control resets selection.
Mounted-server tests prove selecting a player updates the visible profile header
and clearing it returns to the explicit Select a player state.

## Lineups

Lineups now reads the existing Phase 2 lineup portfolio and renders separate
Starting Lineup, Specialized Lineups, and Closing Lineup groups with actual
player names, legality, reliability, explanation, and governed evidence state.
No new lineup model or unsupported synergy was introduced. Missing categories
render exact REVIEW explanations. A lineup-builder failure now leaves valid
Rotation/minutes visible and reports the builder error only in the affected view.

## Staggering / Game Plan

The existing stagger contract is presented as Q1-Q4 quarter cards with readable
clock segments, actual player names, starter overlap, creator coverage, center
coverage, sixth-man entry context, 48 game minutes, and 240 team minutes. Raw
player IDs are not the primary presentation. Metadata uses a readable helper
scale and mobile rows wrap rather than creating a page-wide scrollbar.

Runtime result: all four tested teams emitted periods 1-4, roster-contained
player IDs, and exact 240 player-minutes.

## Team switching

Mounted Shiny verification switched Boston -> Charlotte -> Cleveland -> Portland.
For each team the roster exceeded 10 players, selection belonged to the current
roster, the shadow was `COMPLETED`, rotation contained 10 current-team players,
minutes totaled 240, lineup IDs belonged to the current roster, stagger IDs
belonged to the current roster, and periods 1-4 were present. No prior-team
player or scenario state leaked.

## Empty states

Verified states include disabled/preparing state, top-level execution failure,
missing rotation membership, missing minute ledger, missing lineup portfolio,
missing stagger plan, and partial Phase 2 failure. Each state names the failed
dependency and preserves PASS/REVIEW/FAIL plus independent blocked state. A
partial lineup failure was explicitly tested to preserve the valid 240-minute
Rotation output.

## Tests

All changed R files parsed and `devtools::load_all()` passed.

Focused final results:

| Suite | Passed | Failed | Errors | Warnings | Skipped |
|---|---:|---:|---:|---:|---:|
| `test-v2-depth-chart-stabilization.R` | 126 | 0 | 0 | 1 | 0 |
| `test-v2-phase3-presentation.R` | 62 | 0 | 0 | 4 | 0 |

The single focused warning is the local Shiny package build-version warning.
The four presentation warnings are the existing Windows Sass-cache fallback.

Counted regression results:

| Suite | Passed | Failed | Errors |
|---|---:|---:|---:|
| `test-v2-rotation-contracts.R` | 49 | 0 | 0 |
| `test-v2-rotation-engine.R` | 73 | 0 | 0 |
| `test-v2-rotation-shadow.R` | 37 | 0 | 0 |
| `test-v2-phase1e-evidence.R` | 65 | 0 | 0 |
| `test-v2-role-eligibility.R` | 70 | 0 | 0 |
| `test-v2-role-all-teams.R` | 45 | 0 | 0 |
| `test-v2-minute-ledger.R` | 46 | 0 | 0 |
| `test-v2-staggering.R` | 39 | 0 | 0 |
| `test-v2-lineup-portfolio.R` | 32 | 0 | 0 |
| `test-v2-phase2-all-teams.R` | 36 | 0 | 0 |
| `test-scenario_comparison_engine.R` | 16 | 0 | 0 |
| `test-scenario_comparison_integration.R` | 6 | 0 | 0 |
| `test-minute_allocation_engine.R` | 18 | 0 | 0 |
| `test-lineup_optimization_engine.R` | 22 | 0 | 0 |
| `test-v1-rotation-characterization.R` | 45 | 0 | 0 |
| `test-ux-foundation-assets.R` | 92 | 0 | 0 |
| `test-depth-chart-performance-equivalence.R` | 186 | 0 | 0 |
| `test-depth-chart-phase11-performance-equivalence.R` | 23 | 0 | 0 |

Counted totals: 1,088 passing assertions, zero failures, zero errors, nine
environment/cache warnings, and zero skips. Earlier focused execution in this
same task also passed the Phase 1/2 combined suites, output-ID checks, Trade
scenario reset checks, and protected V1 regressions before the final counted
sweep.

No obvious performance regression was introduced. The three V2 views render
from one session shadow state and client subtab changes do not trigger new DB
reads, BIE evaluation, rotation construction, or lineup construction.

## Database

- Before SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- After SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- Expected SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- Result: unchanged; no database mutation occurred.

## Browser/manual QA

Automated:

- The actual app started locally with the development demo bypass and returned
  HTTP 200 at `http://127.0.0.1:8765`.
- Mounted Shiny module tests exercised real database rosters, scenario
  publication/reset, four-team switching, selected-player update/clear, and all
  Phase 2 plan contracts.
- Rendered DOM checks verified distinct Rotation, Lineups, and Game Plan views,
  player names, minutes, evidence, Q1-Q4, responsive contracts, and precise
  empty states.

Not visually automated: the trusted in-app browser client could not start
because its RPC dependency was outside the configured trusted code path. No
screenshots or computed viewport bounds are claimed. Manual inspection remains
for 1920, 1440, approximately 900, and approximately 390 pixels.

## Files changed

Files changed by this Depth Chart stabilization:

- `R/mod_depth_chart.R`
- `R/v2_presentation.R`
- `inst/app/www/tbi_phase3.css`
- `tests/testthat/test-v2-depth-chart-stabilization.R`
- `tests/testthat/test-v2-phase3-presentation.R`
- `docs/v2-depth-chart-stabilization-2026-08-21.md`

No file was deleted. No file is staged.

Current branch: `v2-elite-development`

Current HEAD: `e39b1a752243d7969e32c627bb7e22b70947a247`

Current modified files in the approved shared working tree:

- `R/app_server.R`
- `R/app_ui.R`
- `R/mod_depth_chart.R`
- `R/mod_draft_assets.R`
- `R/mod_executive_dashboard.R`
- `R/mod_five_year_outlook.R`
- `R/mod_player_manager.R`
- `R/mod_roster_contracts.R`
- `R/mod_trade_analyzer.R`
- `R/transaction_state.R`
- `R/web_demo_security.R`
- `app.R`
- `inst/app/www/tbi_demo.css`
- `inst/app/www/tbi_demo.js`
- `inst/app/www/tbi_ux_foundation.js`

Current new/untracked paths in the approved shared working tree:

- ` README_FIRST.txt`
- ` TBI_REPAIR_2025_26_REGULAR_SEASON_AND_POSITION_VALIDATION.R`
- `.context/`
- `.impeccable/`
- `R/v2_presentation.R`
- `R/v2_trade_intelligence.R`
- `R/v2_transaction_foundation.R`
- `docs/v2-depth-chart-stabilization-2026-08-21.md`
- `docs/v2-feedback-candidate-overnight-2026-08-20.md`
- `docs/v2-final-manual-audit-corrections-2026-08-20.md`
- `docs/v2-phase3-overnight-completion-2026-08-20.md`
- `docs/v2-phase3k-page-equality-2026-08-20.md`
- `docs/v2-trade-intelligence-repair-2026-08-20.md`
- `inst/app/www/tbi_phase3.css`
- `reference/`
- `tests/testthat/test-v2-depth-chart-stabilization.R`
- `tests/testthat/test-v2-feedback-candidate-workflows.R`
- `tests/testthat/test-v2-final-manual-audit-corrections.R`
- `tests/testthat/test-v2-phase3-presentation.R`
- `tests/testthat/test-v2-trade-intelligence-repair.R`
- `tests/testthat/test-v2-transaction-foundation.R`
- `tests/testthat/test-web-demo-expiration-bypass.R`

Deleted files: none.

Staged files: none.

`git diff --check`: passed with exit code 0. Git emitted only the existing
LF-to-CRLF working-copy notices.

## Morning manual checklist

1. Select one team and inspect Depth Chart plus the contained player detail.
2. Open Rotation and verify real players, bench order, sixth man, and 240/240.
3. Open Lineups and inspect starting, specialized, and closing groups.
4. Open Staggering / Game Plan and inspect Q1-Q4 at desktop and mobile width.
5. Switch Boston -> Charlotte -> Cleveland -> Portland and watch every subtab.
6. Publish a safe Trade preview, then reset it and confirm the banner and all deltas disappear.
