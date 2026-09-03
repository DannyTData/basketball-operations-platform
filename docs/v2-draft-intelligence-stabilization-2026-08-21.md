# V2 Draft Intelligence Stabilization — 2026-08-21

## Status

**DRAFT INTELLIGENCE READY FOR MANUAL QA**

Draft Intelligence now presents the existing draft-capital data as a compact, five-subtab asset-management workspace. The server-owned Asset Ledger filters, year grouping, verification explanations, team switching, and evidence-specific recommendation passed focused runtime coverage against the loaded application database. No draft model, valuation formula, projection, transaction rule, or database content changed.

## Files changed

- `R/mod_draft_assets.R` — Draft-only presentation helpers, governed ledger filtering and grouping, verification explanations, structured findings, and evidence-specific recommendation output.
- `tests/testthat/test-v2-draft-intelligence-stabilization.R` — proof-first functional and live-module coverage for the stabilization acceptance paths.
- `tests/testthat/test-v2-final-manual-audit-corrections.R` — replaced stale Reactable implementation assertions with stronger checks for the server-driven Draft filters and expandable year groups. Five-Year assertions were not changed.
- `docs/v2-draft-intelligence-stabilization-2026-08-21.md` — this completion report.

No shared CSS/JavaScript file and no other top-level page was changed for this stabilization.

## Overview result

The existing snapshot remains the compact executive entry point. Real-team runtime coverage confirmed that total records, controlled firsts/seconds, swap rights, obligation exposure, and the existing modeled portfolio position populate from the selected team.

## Portfolio & Timeline result

The existing five-year portfolio and timeline calculations are unchanged. Runtime switching across Boston, Minnesota, Atlanta, and Charlotte confirmed that the visible portfolio and timeline output changes with each selected team and does not retain the prior team's rendered state.

## Filter result

The Asset Ledger now uses explicit server-owned filters for:

- year
- round
- control type
- original team
- protection type
- verification state
- text search

Single filters, two-filter combinations, search plus verification, and clear-to-full-ledger behavior passed. A team-bound filter epoch prevents stale search or exact-filter values from being applied while team-change reset messages are in flight. Empty result sets display a selected-team-specific explanation.

## Year-group result

Filtered records are grouped once by ordered draft year. The first two available years are initially expanded and all other years remain user-expandable. Tests confirm stable year ordering, filter-preserving grouping, and exactly one appearance of every filtered asset ID across all groups.

## Verification-language result

User-facing `Manual Review` language in the Draft workspace was replaced with `Records Requiring Verification`. Each ledger record now reports the first supported reason available from its loaded facts:

- source provenance incomplete
- swap terms incomplete
- protection language incomplete
- conveyance conditions incomplete
- loaded provenance remains unverified
- verified

Missing facts remain explicit. The implementation does not infer ownership, protection, conveyance, or source facts.

## Risks & Opportunities result

The existing findings are now rendered as structured `FACT`, `IMPACT`, and `DECISION CONSEQUENCE` rows. Categories are assigned when the existing finding is constructed rather than inferred later from its prose. This preserves the existing thresholds and findings while making control risk, protection/conveyance risk, trade flexibility, and acquisition opportunity easier to scan.

## Recommendation result

The recommendation still uses the existing net-portfolio thresholds. It now explains that posture with the selected team's supported outputs:

- strongest modeled control year
- most constrained modeled year
- largest loaded modeled obligation
- number of records requiring verification
- a specific preservation or acquisition action

When modeled year or obligation values are unavailable, the recommendation states that the result is unsupported or `UNKNOWN`; it does not rank missing values as zero.

## Team-switch result

Live Shiny module coverage exercised:

`Boston Celtics -> Minnesota Timberwolves -> Atlanta Hawks -> Charlotte Hornets`

The test switched teams while a Boston search and original-team filter were active. Each new team restored its complete ledger, changed the asset IDs, portfolio output, timeline output, and recommendation, and displayed the newly selected team in the recommendation. No prior-team filter was applied to the next team.

## Runtime verification

- Started the real Shiny application from the repository with the existing local dependency library and development demo-expiration bypass.
- HTTP root returned `200` and served the Draft Intelligence page, the new filter controls, and the verification terminology.
- Live `shiny::testServer()` coverage exercised Tests A–H against the loaded database-backed module state: overview, portfolio/timeline, filters, combined filters and clear, multiple year groups/no duplicate IDs, active-filter team switching, selected-team recommendation, and exact UNKNOWN/verification reasons.
- Trusted interactive browser capture could not initialize because the bundled browser service dependency was rejected outside its configured trusted code path. No screenshot or click-level visual claim is made.

## Tests

All executed tests passed after the stabilization:

- changed R parse — PASS
- `devtools::load_all()` — PASS
- `test-v2-draft-intelligence-stabilization.R` — PASS, 69 expectations; one package build-version warning only
- `test-mod_draft_assets.R` — PASS
- `test-draft_assets_engine.R` — PASS
- `test-draft_value_engine.R` — PASS
- `test-draft_simulation_engine.R` — PASS
- `test-draft-simulation-equivalence.R` — PASS
- `test-v2-phase3-presentation.R` — PASS; Sass cache fell back to a temporary directory
- `test-v2-final-manual-audit-corrections.R` — PASS; one package build-version warning only
- `test-v2-feedback-candidate-workflows.R` — PASS
- `test-scenario_comparison_engine.R` — PASS
- `test-scenario_comparison_integration.R` — PASS
- `test-basketball_intelligence_engine.R` — PASS
- application startup and HTTP smoke — PASS (`200`)

The stale Draft assertion in `test-v2-final-manual-audit-corrections.R` required Reactable's `filterable` and `groupBy` source strings. That implementation was intentionally replaced by stronger server-driven filters and native expandable year groups. Its updated assertions now protect the actual filter control IDs and year-group surface; the dedicated stabilization suite protects behavior.

## Database safety

Authoritative database:

- Path: `inst/database/tbi.sqlite`
- SHA-256 before: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- SHA-256 after: `1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2`
- Result: unchanged

## Manual visual checks remaining

Because trusted screenshot automation was unavailable, manually verify these Draft-only presentation details:

1. Overview density and line wrapping at 1920 and 1440 desktop widths.
2. All five Draft subtabs change without duplicated or retained content.
3. Asset Ledger filter layout at 1920, 1440, approximately 900, and approximately 390 pixels.
4. Expand and collapse at least two year groups after applying a combined filter.
5. Open one asset's protection/provenance detail and confirm long source/protection text wraps cleanly.
6. Repeat Boston -> Minnesota -> Atlanta -> Charlotte in the browser with filters active.
7. Confirm the Risks & Opportunities FACT/IMPACT/DECISION rows remain readable on mobile.
8. Confirm Recommendation rows do not clip long team/obligation descriptions.

No nested scrolling was introduced. The Asset Ledger uses normal page flow and responsive single-column fallbacks.
