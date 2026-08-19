# TBI Overnight Master Report - 2026-08-19

## Outcome

All five requested projects were completed in order. Production code changes
are limited to the CBA Info Hub and its previously missing shared deep-link
wiring. Projects 3-5 are design/audit only. No commit or push was made, no
database content was changed, and protected basketball/CBA transaction logic
was not modified.

## Files changed

| File | Reason |
|---|---|
| `R/mod_cba_glossary.R` | Redesign CBA Hub as a collapsible index plus wide rule workspace; add canonical terms, aliases, source metadata, search helpers, responsive drawer, and selection-persistence fix |
| `inst/app/www/tbi_ux_foundation.js` | Wire existing `data-cba-term-link` and `data-cba-module-link` elements to the existing Shiny deep-link inputs through the one shared UX coordinator |
| `tests/testthat/test-cba-glossary.R` | Characterize canonical uniqueness, aliases, search, IDs, responsive structure, deep links, and selected-rule persistence |
| `docs/architecture.md` | Replace the minimal architecture note with runtime layers, module map, state flow, BIE freeze, cache/UI rules, and deployment notes |
| `docs/release-hardening-2026-08-18.md` | Record page QA, performance, test classification, limitations, repository hygiene, and manual release checklist |
| `docs/v2-rotation-engine-roadmap.md` | Project 3 audit and phased V2 rotation design |
| `docs/trade-cba-multiteam-readiness.md` | Project 4 rule coverage matrix and generalized 2-4 team architecture |
| `docs/v2-v3-product-roadmap.md` | Project 5 ranked top-ten product roadmap |
| `docs/overnight-master-report-2026-08-19.md` | Consolidated morning report |

## Project 1 - CBA Info Hub

### Architecture delivered

- Left: searchable Knowledge Index grouped by supported category, collapsible
  to 56px on desktop and presented as a scrim-backed drawer at <=1000px.
- Right: wider Rule Intelligence Workspace with definition, practical impact,
  example, affected areas, related rules, aliases, TBI module impact, source
  reference, verification status, recently viewed, and favorites.
- Detail content uses a two-column information grid on wide screens and one
  column below 1250px.
- All existing Shiny IDs remain. The category selector now derives its choices
  from the glossary.
- The module uses root-scoped click/keyboard handling. The global observer
  count remains exactly one.

### Verified defects fixed

1. Cross-module CBA buttons rendered `data-cba-term-link` attributes, but no
   client handler ever sent `cba_open_term`; the links were inert. The existing
   shared coordinator now sends event-priority requests for term and impacted
   module links.
2. Filtering the index automatically selected its first result. An active
   search could therefore overwrite a requested deep-linked rule. Filtering
   now changes only the index; explicit card/history/related/deep-link actions
   own the selected detail.

Live verification proved Command Center -> Luxury Tax -> Cap Intelligence
navigation. The CBA page opened with Luxury Tax selected, the requested content
rendered, and no duplicate DOM ID appeared.

### Knowledge expansion

The glossary increased from 38 to 66 unique canonical terms. Canonical terms
and aliases resolve through 174 normalized lookup keys without collisions.
Every original row remains first and in its original order.

Added canonical concepts and aliases:

- Tax Apron Terminology (`Tax Apron`, `Apron`)
- Early Termination Option (`ETO`, `Early Termination`)
- Mid-Level Exception (`MLE`, `Midlevel Exception`)
- Disabled Player Exception (`DPE`, `Disabled Player`)
- Poison Pill Provision (`Poison Pill`, `PPP`)
- Minimum Team Salary / Salary Floor (`Salary Floor`, `Minimum Team Salary`,
  `Team Salary Floor`)
- Guaranteed Salary (`Fully Guaranteed`, `Guarantee`)
- Non-Guaranteed Salary (`Unguaranteed Salary`, `Non Guaranteed Salary`)
- Partial Guarantee (`Partially Guaranteed`, `Partial Guarantee Amount`)
- Roster Charge (`Incomplete Roster Charge`, `Minimum Roster Charge`)
- Supermax / Designated Veteran Terminology (`Supermax`, `Super Max`)
- Maximum Salary (`Max Salary`, `Maximum Contract`)
- Maximum Extension (`Max Extension`)
- Extension Eligibility (`Extension Eligible`, `Eligibility Window`)
- Over-38 Rule (`Over 38 Rule`, `Over-38`)
- Cash Considerations (`Cash in Trade`, `Trade Cash`)
- Draft Rights (`Player Draft Rights`, `NBA Draft Rights`)
- First-Round Pick (`First Round Pick`, `FRP`, `1st-Round Pick`)
- Second-Round Pick (`Second Round Pick`, `SRP`, `2nd-Round Pick`)
- Pick Protection (`Protected Pick`, `Draft Protection`)
- Pick Swap (`Swap Rights`, `Draft Pick Swap`)
- Stepien Rule (`Ted Stepien Rule`, `Stepien`)
- Seven-Year Rule (`Seven Year Rule`, `7-Year Rule`)
- Conveyance (`Pick Conveyance`, `Conveys`)
- Pick Obligation (`Draft Obligation`, `Owed Pick`)
- Reacquisition Restrictions (`Reacquisition Rule`, `Reacquiring a Player`)
- Recently Traded Restriction (`Recently Traded Player`,
  `Trade Waiting Period`)
- Recently Signed Restriction (`Recently Signed Player`,
  `Signing Trade Restriction`)

All 38 original terms also received practical aliases where supported,
including Bird Rights, Early/Non-Bird Rights, TPE, BYC, MLE families, RFA/UFA,
QO, trade kicker, option, waiver, rookie-extension, and two-way shorthand.

### Requires source verification

No precise legal interpretation was invented. These ten summaries are visibly
marked `Requires source verification`:

- Tax Apron Terminology
- Poison Pill Provision
- Supermax / Designated Veteran Terminology
- Over-38 Rule
- Cash Considerations
- Stepien Rule
- Seven-Year Rule
- Reacquisition Restrictions
- Recently Traded Restriction
- Recently Signed Restriction

The remaining entries are labeled supported summaries and still instruct the
user to confirm the governing agreement and official league guidance before
execution.

## Project 2 - Whole-platform QA and release hardening

### Page-by-page result

All 11 top-nav pages opened. All 42 internal sub-tabs activated. Every page had
its expected header, no visible Shiny error, and zero desktop horizontal
overflow. The final rendered DOM had 396 IDs and zero duplicates.

| Page | Status |
|---|---|
| Command Center | Pass - 5 tabs |
| Team Overview | Pass - 6 tabs |
| Roster Intelligence | Pass - 4 tabs |
| Depth Chart | Pass - 3 tabs |
| Player Management | Pass - 5 tabs |
| Cap Intelligence | Pass - 4 tabs |
| Extension Simulator | Pass - 4 tabs |
| Trade Intelligence | Pass - 4 tabs |
| Draft Intelligence | Pass - 5 tabs |
| Five-Year Outlook | Pass - 6 tabs |
| CBA Info Hub | Pass - responsive knowledge workspace |

Scenario-banner nodes that were empty had intentional `return(NULL)` behavior
when there was no active trade. No missing section or unbound visible output was
found. Tablet/mobile CSS contracts passed static tests; live device emulation
was unavailable, so responsive browser verification remains manual.

### Performance

- Command Center current cold server median: 0.795s; warm 0.00014s; 250 draft
  iterations unchanged. Stored optimized result: 0.640s.
- Draft simulation current median: 0.689s; three invariant data queries.
- Depth Chart current clean rerun: core 0.080s, full cold 1.354s, warm 0.013s,
  49 queries. Stored same-branch best: 0.886s. Equivalence failures 0, database
  hash unchanged, BIE frozen. The remaining current-run cost is BIE lineup
  summary (0.510s) and Phase 11 lineup panel (0.334s).
- Player Management 15-player evidence assembly: 105 queries/105 table checks/
  105 field checks in the frozen reference versus 7/1/6 in production, with
  strict equivalence passing.
- Warm browser navigation was approximately 0.4-0.55s for most pages; Player
  Management was slowest at 1.371s.

The current Windows R 4.5.2 run did not reproduce the Depth Chart sub-second
full-page goal. No lineup/BIE logic was altered to force it.

### Test results and classification

Focused CBA and shared UX suites pass. The full run passed all core engines,
database, BIE, Depth Chart, draft simulation, Player Management equivalence,
scenario, trade, output-ID, and current UX coverage before reaching the
testthat ten-problem display cap.

- 8 stale UI assertions expect `shiny.tag.list` although active module UIs
  intentionally return one root `shiny.tag` and render successfully.
- 1 stale performance assertion rejects the intentional reviewed Stage 2
  `bindCache()` implementation.
- 1 stale Player Management test calls missing legacy helper
  `position_value_v2()`.
- Environment warnings: locale `C.UTF-8` unavailable; `shiny` built under R
  4.5.3 but runtime is R 4.5.2; `renv` reports out-of-sync.

Tests were not edited simply to make the suite green.

### Repository hygiene

No deletion was performed. Review candidates:

- `qa/`: 500 ignored files, 77.22 MB;
- `inst/database/backups/`: 13 ignored files, 75.85 MB;
- additional `inst/database/tbi_before_*.sqlite` recovery snapshots;
- duplicated lower half of `.gitignore` (all generated-file patterns appear
  twice);
- local `.context/`, `.impeccable/`, `agents.md`, and protected repair files.

## Project 3 - V2 rotation architecture

The current stack already has exact 240-minute allocation, 8-12 player
selection, four optimized lineup types, availability normalization, approved
lineups, Phase 11 rotation/lineup logic, Phase 12 scenario comparison, and
session caches. It lacks one canonical rotation plan, sixth-man and backup
PG/C roles, game-long lineup/minute reconciliation, substitution sequencing,
and a situation override hierarchy.

The recommended new version is:

```text
Starter -> Rotation -> Minutes -> Lineup portfolio -> Situation derivation
```

Approved locks and legality precede model ranking. Situations derive immutable
plans rather than mutate base state. Estimated phased effort is 19-29
engineer-weeks plus new data work. Full contracts, cache keys, override order,
30-team regression requirements, UI surfaces, and phase gates are in
`docs/v2-rotation-engine-roadmap.md`.

## Project 4 - Trade/CBA and multi-team readiness

The current engine is production-useful as a stated two-team screen, not a full
legality determination. It partially handles salary matching, selected apron
bands/crossings, second-apron aggregation, the seven-year horizon, Stepien
control states, protection/conveyance review, and draft duplicate prevention.

Missing from transaction validation: TPE usage, hard-cap ledger, sign-and-trade,
recently signed/traded, BYC, poison pill, trade kicker, minimum salary, roster
size, reacquisition, cash considerations, and complete swap/condition routing.

Three-team and four-team transactions are not production-ready. The recommended
future architecture uses explicit player/asset/cash route graphs, a result per
organization, and one overall PASS/REVIEW/FAIL. Basketball/BIE evaluation stays
downstream and can never override CBA FAIL. Full matrix, readiness gates, state
model, pipeline, UI architecture, and 32-51 week estimate are in
`docs/trade-cba-multiteam-readiness.md`.

## Project 5 - Prioritized V2/V3 roadmap

1. V2 rotation core - immediate next
2. two-team transaction-rule foundation - immediate next
3. injury/availability scenarios - near-term V2
4. minutes and staggering - near-term V2
5. lineup portfolio and closing intelligence - near-term V2
6. contract decision calendar/sequencing - later V2
7. generalized multi-team trades - later V2
8. draft scenario decision lab - later V2
9. roster-construction constraint solver - V3
10. predictive decision simulator - V3

Use cases, dependencies, effort, technical risk, basketball/CBA risk, and
release gates are in `docs/v2-v3-product-roadmap.md`.

## Unresolved risks

- Ten CBA glossary concepts need source verification.
- Multiple trade legality gaps prevent safe multi-team production use.
- The release R library/runtime is not synchronized.
- Current hardware did not reproduce the stored Depth Chart sub-second full
  cold median.
- Tablet/mobile behavior requires final manual viewport checks.
- Stale legacy tests reduce signal until deliberately reconciled with the
  approved current contracts.

## Required morning checks

Use the 12-step checklist in `docs/release-hardening-2026-08-18.md`. The highest
value checks are: synchronize R/`renv`; confirm BIE frozen; verify desktop,
tablet, and phone layouts; exercise CBA deep links with a conflicting active
search; run a valid two-sided trade through salary/draft/BIE/scenario outputs;
verify approved Depth Chart starters and exact 240 minutes; and rerun the Depth
Chart benchmark on release hardware.

## Frozen behavior confirmation

`bie_freeze_status()` reports:

- `frozen = TRUE`
- model `BIE vNEXT - Locked Architecture`
- QA `PASS`
- 30 teams tested
- optimizer `65% quality / 25% position fit / 10% lineup balance`

BIE formulas/weights/reliability, starter locks, lineup legality, candidate
ordering, rookie gate, rotation logic, CBA transaction rules, scenario logic,
database source of truth, and database contents were not changed.
