# TBI V2 Depth Chart Layout QA — 2026-08-21

## Executive result

The Depth Chart layout is ready for manual QA. The implementation, live Shiny startup/HTTP smoke, runtime-oriented state tests, responsive layout contracts, protected basketball regressions, database safety check, and Git whitespace check passed.

Trusted automated screenshots were unavailable because the bundled browser service could not resolve one of its runtime dependencies from a configured trusted code path. No screenshot-based visual verification is claimed. The exact desktop, tablet, and mobile checks that remain are listed below.

## Layout result

| Area | Result | Evidence |
|---|---|---|
| Subtab placement | Complete | Depth Chart, Rotation, Lineups, and Staggering / Game Plan are placed immediately below the module introduction. |
| Desktop composition | Complete | The Depth Chart workspace uses an approximately 62/38 chart-to-inspector split. |
| Positional chart | Complete | Player cards and positional columns are more compact while retaining existing basketball/status fields. |
| Selected-player workspace | Complete | Player detail, assignment controls, and the compact current-five court share the right workspace. |
| Empty selection | Complete | The panel displays: “Select a player to inspect or edit their depth-chart assignment.” |
| Court placement | Complete | The compact current-five view remains on Depth Chart; the full lineup editor remains available on Lineups. |
| Rotation | Preserved | Existing Rotation content and behavior were not redesigned. |
| Lineups | Preserved | Existing Lineups content and controls remain mounted and full width. |
| Game Plan | Complete | The view is framed as “48-Minute Game Plan” and explains the 240-minute, on-court-group, starter-overlap, creator, and center-coverage model. |
| Responsive contract | Complete | At 900px and below, the chart, player detail, and court stack in that order; smaller mobile sizing is applied at 650px. |

## Files changed for this pass

- `R/mod_depth_chart.R` — reorganized the existing Depth Chart markup into the chart and right-side detail/court workspace; preserved existing IDs and server behavior.
- `R/v2_presentation.R` — clarified the Game Plan heading and governed description.
- `inst/app/www/tbi_ux_foundation.js` — placed the existing subtab navigation beneath the module introduction and kept the proper active workspace mounted.
- `inst/app/www/tbi_phase3.css` — consolidated the Depth Chart layout owner, compacted spacing/cards/court, and added the responsive stack.
- `tests/testthat/test-v2-depth-chart-stabilization.R` — added runtime-oriented selection, team switching, layout-contract, and staggering-invariant coverage.
- `tests/testthat/test-v2-phase3-presentation.R` — updated presentation characterization for the approved layout and Game Plan framing.
- `docs/v2-depth-chart-layout-qa-2026-08-21.md` — this completion report.

No files were deleted. No changes were staged.

## Desktop composition

The primary workspace now allocates roughly 62.3% to the positional depth chart and 37.7% to the right-side decision workspace. The positional board uses reduced header, card, and grid spacing without reducing the information shown. The inspector appears first in the right column, followed by a compact current-starting-five court. The Depth Chart view suppresses full lineup-analysis internals that belong to the Lineups tab; it does not delete or change those controls.

## Selected-player behavior

Runtime-oriented Shiny tests selected three distinct real roster players, confirmed each selection reached the inspector, and cleared the selection. Clearing produces the required empty state without retaining stale player details. Team switching was exercised consecutively for Boston, Brooklyn, and Cleveland to verify selection/state isolation.

## Court placement and preserved views

The Depth Chart tab contains only the compact current-five court needed for immediate context. Rotation retains its existing governed rotation/minute presentation. Lineups retains the full existing lineup editor and analysis. The tab-routing script moves navigation without recreating Shiny inputs or outputs, preserving mounted state and existing IDs.

## Game Plan and staggering invariants

The Game Plan presentation now clearly describes the 48-minute plan and its relationship to the selected rotation and exact 240 player-minutes. Tests across Charlotte, Portland, Boston, Brooklyn, and Cleveland confirmed:

- exactly 240 total player-minutes;
- exactly five unique players in every segment;
- every segment player belongs to the governed rotation;
- segment-duration totals reconcile exactly to every player's minute-ledger assignment.

The visually repetitive starter-overlap pattern was inspected as a presentation concern. Because the governed reconciliation and membership invariants pass, no basketball or staggering model logic was rewritten.

## Responsive result

The CSS/DOM contract was verified for wide desktop, 1440px desktop, 900px tablet, and compact mobile behavior. At 900px and below, the positional chart is followed by player detail and then the court. At 650px and below, cards, avatars, and court slots use compact dimensions. Static checks found no arbitrary z-index workaround or conflicting duplicate shell owner after consolidation.

Live screenshot and computed-geometry capture remains manual because the trusted browser runtime failed before browser control initialized. HTTP startup succeeded, so this is a visual-evidence limitation rather than an application startup failure.

## Automated and runtime verification

- Changed R files parsed successfully.
- `devtools::load_all(quiet = TRUE)` passed.
- JavaScript syntax validation passed.
- Live Shiny startup returned HTTP 200 with the local demo-expiration bypass enabled.
- Focused Depth Chart stabilization and Phase 3 presentation suites passed.
- Phase 2 rotation shadow, rotation engine/contracts, minute ledger, staggering, and lineup portfolio suites passed.
- Scenario comparison engine and integration suites passed.
- Protected V1 minute allocation, lineup optimization, rotation characterization/frozen-BIE assertion, and Depth Chart performance-equivalence suites passed.
- Fifteen focused/protected test files completed without failures or errors.

Non-failing warnings were limited to the installed Shiny build-version notice and Sass cache fallback to a temporary directory. No runtime application error was observed during startup/HTTP smoke.

## Database and Git safety

- Branch: `v2-elite-development`
- HEAD: `e39b1a752243d7969e32c627bb7e22b70947a247`
- Database: `inst/database/tbi.sqlite`
- SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- Size: `11296768` bytes
- Database modified timestamp: `2026-08-19T16:49:56.5939000-04:00`
- Database hash matches the required pre-work value.
- Staging index: empty
- Deleted files: none
- `git diff --check`: passed (line-ending notices only)

## Review outcome

Targeted correctness, testing, reuse, quality, and efficiency reviews were applied only to the permitted Depth Chart files. Duplicate CSS ownership introduced during the pass was consolidated; no behavior-preserving efficiency change was warranted. The repository-wide review harness was not used because the approved working tree contains unrelated uncommitted V2 work that is outside this task boundary.

## Manual visual review still required

1. At 1440px desktop, confirm the chart, player inspector, and court read comfortably without overlap or excessive vertical space.
2. At 1920px or wider, confirm the 62/38 composition uses the canvas without stretching cards excessively.
3. At approximately 900px, confirm the chart → detail → court stack and all assignment controls remain reachable.
4. At approximately 390px, confirm there is no horizontal overflow and court/player labels remain readable.
5. Click all four subtabs and verify Rotation and Lineups retain their prior content and controls.
6. Select at least three players, then clear selection and confirm the required empty-state copy appears immediately.
7. Switch Boston → Brooklyn → Cleveland and confirm no selected-player state leaks between teams.
8. Inspect the 48-Minute Game Plan visually; repeated starter-overlap counts are permitted when the verified segment/minute invariants remain correct.

