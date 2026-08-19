# Release Hardening Report - 2026-08-18

## Scope and safety

This audit used branch `release-1.9-demo-baseline` and a disposable copy of
`inst/database/tbi.sqlite`. No database write was performed. The BIE engine,
ingestion, starter locks, lineup legality, rookie eligibility, CBA transaction
rules, and transaction state were not changed.

## Desktop runtime QA

A live Shiny session exercised every top-level workspace and every currently
rendered internal sub-tab. The rendered DOM contained 396 IDs and no duplicate
IDs. All 42 sub-tabs activated successfully, with no visible Shiny error and no
page-level horizontal overflow at the desktop viewport.

| Workspace | Internal tabs exercised | Browser navigation settle* | Result |
|---|---:|---:|---|
| Command Center | 5 | 0.411s | Pass |
| Team Overview | 6 | 0.548s | Pass |
| Roster Intelligence | 4 | 0.414s | Pass |
| Depth Chart | 3 | 0.434s | Pass |
| Player Management | 5 | 1.371s | Pass; slowest warm navigation |
| Cap Intelligence | 4 | 0.428s | Pass |
| Extension Simulator | 4 | 0.418s | Pass |
| Trade Intelligence | 4 | 0.422s | Pass |
| Draft Intelligence | 5 | 0.412s | Pass |
| Five-Year Outlook | 6 | 0.429s | Pass |
| CBA Info Hub | n/a | 0.427s | Pass |

\* Click-to-active-page with no visible recalculating output in an already
connected QA session. These numbers are end-to-end browser settle diagnostics,
not isolated cold server benchmarks.

Empty scenario-banner output nodes were confirmed to be intentional: their
renderers return `NULL` when no session trade scenario is active. No production
output was silently suppressed.

The source and regression suite confirm exactly one global
`MutationObserver`, owned by `tbi_ux_foundation.js`. The CBA module uses a
root-scoped event listener; CBA cross-module links use the existing shared
document click coordinator.

## Responsive QA

Static responsive contracts exist for all approved internal sub-tab rails and
for the CBA index/workspace split. The CBA layout becomes a drawer at 1000px,
its detail grid becomes one column at 1250px, and the mobile drawer has a scrim,
close control, Escape handling, and reduced-motion treatment.

The available in-app browser session did not expose deterministic viewport
emulation. Tablet and phone behavior therefore remains a manual release check;
it is not represented as live-verified in this report.

## Performance evidence

### Command Center draft simulation

The current read-only three-run harness retained 250 iterations and produced:

- simulation median: 0.689s;
- Command Center bundle cold median: 0.795s;
- warm median: 0.00014s;
- invariant simulation input calls: 3 queries, 2 executes, 1 table-list call;
- database hash unchanged.

The committed optimization record measured 0.640s for the Command Center
bundle and 0.566s for the simulation. The current rerun is slower but remains
within ordinary machine/runtime variance and is still more than an order of
magnitude faster than the 11.23s baseline.

### Depth Chart

The committed same-harness best is 0.886s full cold, 0.054s core board, and
0.009s warm with 49 queries. Two current clean reruns measured 1.603s and
1.354s full cold. The second rerun measured:

- core board: 0.080s;
- warm: 0.013s;
- progressive output sum: 1.054s;
- 49 queries, 11 executes, 3 table-list calls, 62 field-list calls;
- enrichment: 0.360s;
- starting-five optimizer: 0.250s;
- rotation: 0.170s;
- BIE lineup summary: 0.510s;
- Phase 11 lineup panel: 0.334s;
- equivalence failures: 0;
- BIE frozen: true;
- database hash unchanged.

The current environment does not reproduce the sub-second full-page median.
The core board is still fast and all equivalence gates pass. The remaining
latency is concentrated in the read-only downstream BIE lineup summary and
Phase 11 panel; no basketball logic was changed to chase the timing.

### Player Management

The Stage C characterization suite confirms the 15-player path changed from
105 row-level queries plus 105 table and 105 field metadata calls to 7 batched
queries, 1 table-list call, and 6 field-list calls. The frozen reference, row
order, duplicate behavior, tie precedence, dynamic columns, NA placement, and
fallback semantics all pass.

## Automated checks

Passing checks include:

- parse of `R/mod_cba_glossary.R`;
- `devtools::load_all()`;
- focused CBA glossary/deep-link tests;
- shared UX asset and single-observer tests;
- BIE, cap, database, Depth Chart equivalence, draft simulation equivalence,
  draft engines, extension engine, lineup, minute allocation, scenario,
  trade-engine, trade-output-ID, Player Management batched/reference, and UI
  regression suites.

The full suite reached its ten-problem reporting cap. The observed items are
not regressions introduced by this work:

| Count | Classification | Evidence |
|---:|---|---|
| 8 | Stale legacy UI contract | Module tests expect class `shiny.tag.list`; the active module UIs intentionally return one root `shiny.tag`, and live rendering passes. |
| 1 | Stale performance assertion | `test-performance_optimization.R` expects no `bindCache()` even though the reviewed Stage 2 session-cache implementation intentionally uses it. |
| 1 | Stale/missing legacy helper | `test-player_manager.R` calls undefined `position_value_v2()`; unrelated to the current batched Player Management implementation. |
| 1 warning | Environment/dependency | `shiny` was built under R 4.5.3 while the available runtime is R 4.5.2. |
| 4 warnings | Environment/locale | `LC_COLLATE`, `LC_CTYPE`, `LC_MONETARY`, and `LC_TIME` could not be set to `C.UTF-8` on Windows. |

`renv` also reports that the project is out of sync. The library should be
reconciled in the target release environment before deployment; tests must not
be changed merely to hide these warnings.

## Known limitations

- The trade salary engine explicitly identifies itself as a first-pass screen,
  not a final league-office legality determination.
- Several transaction-specific rules are unimplemented; see
  `docs/trade-cba-multiteam-readiness.md`.
- The CBA Hub is decision-support content, not legal advice. Ten concepts are
  explicitly marked as requiring source verification.
- Tablet/mobile responsive behavior needs manual viewport verification.
- Current Depth Chart cold performance is hardware/runtime sensitive and did
  not reproduce the stored sub-second median in this run.

## Repository hygiene recommendations

No file was deleted. Recommended follow-up:

1. Review and archive or remove 500 ignored `qa/` files (77.22 MB) once their
   evidentiary retention period is defined.
2. Review 13 ignored `inst/database/backups/` files (75.85 MB) and numerous
   `inst/database/tbi_before_*.sqlite` snapshots. Keep recovery copies outside
   the deployable application tree.
3. Deduplicate the repeated lower half of `.gitignore`; every TBI generated-file
   pattern currently appears twice.
4. Keep `.context/`, `.impeccable/`, `agents.md`, and the two protected repair
   files local unless the team intentionally adopts them as tracked artifacts.
5. Treat `inst/database/tbi.sqlite` as the sole deployable database source of
   truth and verify that backup paths cannot be served as static assets.

## Manual RStudio and browser checklist

1. Start a clean R 4.5.3 session, run `renv::status()`, restore only from the
   reviewed lockfile, then run `devtools::load_all()`.
2. Confirm `bie_freeze_status()` returns `frozen = TRUE`.
3. Run the focused CBA, shared UX, Depth Chart, draft simulation, Player
   Management equivalence, trade engine, and transaction scenario tests.
4. Start Shiny against a copied database and open all 11 top-level pages.
5. At 1440px or wider, click every internal tab and confirm no blank card,
   duplicate card, horizontal overflow, or browser console error.
6. At approximately 900px, verify tab rails remain readable, multi-column cards
   stack, and the CBA Knowledge Index opens as a drawer.
7. At approximately 390px, verify horizontal tab scrolling, no page overflow,
   the CBA drawer scrim/Close/Escape behavior, and readable rule cards.
8. From Command Center, click a CBA term link and confirm the CBA page opens on
   the requested rule even when a different search filter is active.
9. In the CBA workspace, click an impacted module and confirm navigation without
   losing selected team or season.
10. Build a valid two-sided trade, confirm both-team salary screens, draft
    screen, BIE impact, recommendation, and downstream scenario banners; clear
    it and confirm all banners disappear.
11. In Depth Chart, verify approved starters on first paint, saved lineup state,
    rotation sizes 9 and 10, exact 240 minutes, and return-team cache behavior.
12. Re-run the Depth Chart benchmark on the release hardware. Treat <1.0s as a
    release performance goal, not as proven by this Windows R 4.5.2 run.
