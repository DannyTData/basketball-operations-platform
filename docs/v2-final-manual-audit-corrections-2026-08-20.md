# TBI V2 Final Manual-Audit Corrections — 2026-08-20

## Executive result

**READY WITH KNOWN DATA GAPS**

The specific release-blocking workflow defects from the manual audit are corrected: startup is neutral, roster currency sorting is numeric, Draft and Five-Year tables expose working native filters, Depth Chart has decision-specific views, and Trade Intelligence exposes governed 2/3/4-team routing plus an isolated verified-TPE usage path. Shared density, readability, status, overlay, and responsive rules were tightened without changing V1 basketball, CBA authority, save behavior, or the production database.

This is not a claim that current external NBA facts are complete. The authoritative exception ledger has no approved live TPE source, the player table has no jersey-number field, and no approved external snapshot was available to resolve potentially stale contract/roster facts. The UI labels these as source-verification gaps.

## Page results

| Page | Corrections in this pass | Manual QA focus |
|---|---|---|
| Command Center | Shared decision lens, compact global V2 context, denser top-aligned cards, readable risk language | Distinct tab purposes and above-fold composition |
| Team Overview | Shared width/grid/card rhythm and top-aligned content | Three-zone balance and compact league context |
| Roster Intelligence | Numeric currency columns; Cap Hit defaults descending; current-data status; compact grids | Sort/search persistence after team and tab changes |
| Depth Chart | Depth Chart, Rotation, Lineups, Staggering, Game Plan views; Player Intelligence removed as a major view; named/initialed timeline and legend | Starter editing remains V1-authoritative; all V2 view compositions |
| Player Management | Balanced shared card rules; explicit Jersey = UNKNOWN because no authoritative field exists | Profile density and small-screen identity layout |
| Cap Intelligence | Shared dense cards, typography, dark chips, and top alignment | Threshold/readout story and table containment |
| Extension Simulator | Shared workstation density and human-readable statuses | Proposal/CBA/financial/recommendation flow |
| Trade Intelligence | Actual 2/3/4-team controls, explicit player/asset routes, per-team results, TPE inventory/use control, isolated scenario reset | Full user workflow and complex route ergonomics |
| Draft Intelligence | Working native filters, grouping by year, `Records Requiring Verification` language | Filtering and year-group readability |
| Five-Year Outlook | Working native contract filters and denser shared layout | Long-table filtering and multi-year story |
| CBA Info Hub | Optional empty sections collapse; fixed/min-height pressure removed; content reflows upward | Terminology changes, drawer, related rules, mobile |

## Functional bugs fixed

- Removed accidental Boston startup selection. A new session starts at `Select Team`; a selection is restored only from same-browser session storage after Shiny connects.
- Preserved numeric `Age`, `Cap Hit`, and `Remaining Money` values through Reactable and formatted only at the cell layer. Default Cap Hit ordering is true numeric descending with `NA` last.
- Enabled Reactable filtering for Draft Asset Ledger and Five-Year Contracts & Free Agency; Draft is grouped by year.
- Replaced ambiguous Draft `Manual Review` wording with a source/protection verification explanation.
- Replaced raw status tokens with human-readable presentation labels while retaining canonical values internally.
- Split the overloaded Depth Chart presentation into decision-specific views and made the stagger map identify people and coverage meaningfully.
- Reduced Saved Scenarios and Private Demo controls so they no longer dominate or cover the workspace.
- Removed fixed-height behavior that created empty CBA Rule Intelligence bands.

## Trade, multi-team, and TPE status

- The actual Trade Intelligence page exposes 2-, 3-, and 4-team selection.
- Up to four explicit player/draft-asset routes can identify stable IDs, source team, and receiving team.
- Evaluation uses the versioned V2 multi-team graph/evaluator and publishes per-team results plus organizational impact into the existing session scenario state.
- Reset clears the scenario and restores the authoritative baseline.
- Verified TPE rows render ID, team, remaining amount, expiration, source, and verification-backed controls.
- Partial/full/insufficient/expired behavior remains governed by `apply_v2_scenario_exceptions()`; focused tests cover partial/full/invalid behavior.
- A verified-fixture runtime test consumed $5M of a $12M TPE in the scenario ledger while the authoritative ledger remained $12M.
- The live authoritative ledger is intentionally empty. The UI shows `No verified TPE loaded`; it does not infer Charlotte or any other team’s amount/date.
- TPE creation is supported by the backend only from verified creation facts and appears in the scenario ledger. No UI-created TPE is fabricated when supported CBA creation facts are absent.
- Cash and exception route editing are not exposed because a sufficiently verified UI rule/mechanism contract is not yet available; unsupported rules remain REVIEW.

## Default-team behavior

Priority is now: browser-session selection when present, otherwise neutral `Select Team`. There is no first-row/database-order fallback and no hard-coded Boston selection. The restore occurs after `shiny:connected`, preventing initialization order from overwriting the neutral state.

## Global visual corrections

- Shared cards use content-sized grid rows, reduced minimum heights, consistent padding/radius/borders, and top alignment.
- Page grids use the available workspace and avoid stretch-created empty bands.
- Supporting copy, risk text, captions, and metadata have a readable floor without shrinking desktop typography.
- Former white pills are rethemed as dark tokens with consistent border/contrast behavior.
- Status/provenance text is consistent for PASS, REVIEW, FAIL, UNKNOWN, FACT, MODEL OUTPUT, and source verification.
- Saved Scenarios is a compact bottom-right utility; Private Demo is a compact bottom-left status and reflows on mobile.
- Transaction controls stack at smaller breakpoints; dense tables retain contained overflow.

## Current-data reconciliation findings

| Domain | Status | Finding |
|---|---|---|
| Production roster/contracts | REQUIRES REVIEW | Database remains authoritative, but no approved current external snapshot was available to resolve suspected stale facts. |
| Jersey number | UNKNOWN | `players` schema has no jersey-number field; UI reports UNKNOWN. |
| Team TPE inventory | REQUIRES REVIEW | No approved source-backed live ledger entries are present. |
| Player/team images | REQUIRES REVIEW | Governed fallbacks remain active where verified assets are unavailable. |
| Database integrity | CURRENT | Hash, size, and timestamp match the pre-run baseline. |

No player-specific correction was hard-coded and no authoritative data was written.

## Responsive and runtime findings

Static responsive rules cover 2560, 1920, 1440, approximately 900, and approximately 390 widths. The live Shiny HTTP smoke returned 200 with a neutral team selector, multi-team control, TPE inventory, and route editor present; no demo-expired overlay was emitted. Runtime `testServer` checks validated neutral startup, transaction publish/reset, and TPE isolation.

Automated screenshot capture was unavailable through the trusted browser runtime, so pixel-level visual approval at the five required viewport sizes remains manual. No screenshot verification is claimed.

## Verification

- Changed R files parse successfully.
- `devtools::load_all()` succeeds.
- Full repository suite executed through all files. It initially reported three Phase 3 copy assertions after the Trade banner became interactive; the protected wording was restored without removing functionality.
- Final affected suites: Phase 3 presentation green; final manual-audit corrections green; V2 transaction foundation green.
- The full run showed protected BIE, cap, CBA, database, V1 rotation/minute/lineup, depth equivalence, player equivalence, scenario, Phase 1, Phase 2, all-team, transaction, and demo-security suites green. Only non-failing local package/cache warnings were emitted.
- Focused transaction matrix: 42 transaction-foundation assertions and 22 feedback-workflow assertions green.
- Live HTTP smoke: status 200, 586,436-byte UI response, neutral team present, multi-team/TPE/route controls present, expired overlay absent.
- `git diff --check`: PASS (Git emitted informational LF-to-CRLF working-copy warnings only).

## Database safety

- Before SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- After SHA-256: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- Size: `11,296,768` bytes before and after
- Modified: `2026-08-19 16:49:56` before and after

## Remaining blockers / known data gaps

These are data or manual-visual gaps, not known runtime blockers:

1. Approved live TPE amounts, creation dates, expiration dates, and restrictions are not loaded.
2. Jersey numbers are absent from the authoritative schema.
3. Suspected stale roster/contract facts require an approved current-data source and governed reconciliation run.
4. Automated screenshots were unavailable; all viewport compositions require human confirmation.
5. Cash routes and verified TPE-creation mechanisms remain REVIEW-only until their controlling facts/rules are sourced.

## Exact next manual QA checklist

1. At 1920×1080, open a fresh browser session and confirm `Select Team`, then select a team and refresh once to confirm session restore.
2. On Command Center, inspect every internal tab for distinct purpose and confirm Saved Scenarios/Private Demo do not overlap content.
3. On Team Overview, inspect Overview, Scorecard, Profile, and Personnel at 2560×1440 and 1440×900 for balanced columns.
4. On Roster Intelligence, confirm Cap Hit starts descending; test Age, Contract Through, FA Year, search + sort, team change, and tab revisit.
5. On Depth Chart, exercise Depth Chart, Rotation, Lineups, Staggering, and Game Plan; confirm starter editing and V1 authority remain intact.
6. On Player Management, inspect profiles with and without images and confirm Jersey displays UNKNOWN without layout shift.
7. On Cap Intelligence and Extension Simulator, verify dark chips, threshold readability, and contained tables at 900 and 390 widths.
8. On Trade Intelligence, evaluate and reset 2-, 3-, and 4-team player/asset routes; verify per-team and overall REVIEW/FAIL behavior.
9. Load a verified synthetic TPE fixture in a disposable session; test partial, full, insufficient, and expired use and confirm authoritative remaining amount never changes.
10. On Draft Asset Ledger, filter text and expand year groups; confirm verification language and no page-level horizontal overflow.
11. On Five-Year Contracts, test filtering after team changes and confirm tables stay contained on mobile.
12. In CBA Hub, switch among terms with sparse/rich optional content; verify sections reflow, drawer/navigation remain usable, and no empty center band appears.
13. Repeat all 11 pages at 2560×1440, 1920×1080, 1440×900, ~900, and ~390; capture any clipping, overlap, or accidental blank bands.

## Git state

- Branch: `v2-elite-development`
- HEAD: `e39b1a752243d7969e32c627bb7e22b70947a247`
- No files were staged, committed, pushed, merged, reset, or deleted by this pass.
- Existing approved dirty-tree work was preserved.
