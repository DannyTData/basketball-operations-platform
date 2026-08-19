# Trade/CBA Rule Coverage and Multi-Team Readiness

## Executive assessment

The current production Trade Intelligence workspace is a useful two-team
salary-matching and selected apron/draft screen. It is not yet a complete
transaction-legality engine. Three- and four-team production routing should
not begin until the missing per-team player/contract restrictions, hard-cap
ledger, TPE treatment, and generalized asset routing are source-verified and
covered by strict two-team equivalence tests.

No transaction rule was changed during this audit.

## Current two-team architecture

1. `mod_trade_analyzer_server()` loads one selected team and one partner team,
   keeps outgoing/incoming player and draft selections in Shiny inputs, and
   computes BIE fit/impact and the recommendation.
2. `build_trade_team_input()` loads a team's salary, selected outgoing rows,
   and the opposite side's selected players.
3. `evaluate_two_team_trade()` calls `evaluate_trade_side()` independently for
   both teams.
4. `calculate_maximum_incoming_salary()` applies the current cap-status salary
   bands; `evaluate_trade_side()` adds second-apron aggregation and threshold
   crossing flags.
5. `tbi_trade_draft_screen()` checks asset identity, physical-pick duplication,
   ownership/control, the modeled seven-year horizon, adjacent first-round
   control, and complex protection/conveyance review.
6. `tbi_transaction_state()` publishes a session-only two-team scenario. Other
   modules preview roster/payroll changes from that object without database
   writes.

The scope comment in `trade_engine.R` correctly states that the result is a
first-pass screen and explicitly lists several unimplemented restrictions.

## Rule coverage matrix

| Rule | Coverage | Current evidence | Work required before multi-team |
|---|---|---|---|
| Salary matching | Partially implemented | Both teams are evaluated independently with cap-room, standard, tax, first-apron, and second-apron bands | Verify season-specific formulas/threshold applicability and matching-salary inputs against an approved source; add effective-date fixtures |
| Traded Player Exception | UI/text only | CBA concept exists; no TPE selection, balance, expiry, or consumption in transaction validation | Build a source-backed TPE ledger and per-route acquisition mechanism |
| Aggregation | Partially implemented | Pre-trade second-apron team with more than one outgoing player is blocked | Verify full aggregation definition, timing, route interactions, and all apron cases |
| First-apron restrictions | Partially implemented | Status and crossing are calculated; crossing becomes manual review | Model the actual prohibited mechanisms and resulting hard-cap trigger/ceiling |
| Second-apron restrictions | Partially implemented | Dollar-for-dollar matching, outgoing count aggregation flag, threshold crossing | Model all applicable mechanisms, timing, cash/pick constraints, and post-transaction effects |
| Hard cap | Missing from transaction validation | Crossing text advises review; no hard-cap state, trigger, or ceiling ledger | Source-verify trigger events and enforce the correct apron ceiling over full post-transaction team salary |
| Sign-and-trade | Missing from transaction validation | Explicitly listed as out of scope in `trade_engine.R` | Add transaction type, player eligibility/timing, receiving-team restrictions, and hard-cap result |
| Recently signed player restriction | Missing from transaction validation | CBA Hub concept only; exact rule marked for source verification | Add source-backed eligibility dates and contract-path conditions |
| Recently traded player restriction | Missing from transaction validation | Explicitly out of scope | Add acquisition date, aggregation/timing rules, and route-aware validation |
| Base Year Compensation | Missing from transaction validation | Explicitly out of scope | Calculate sending/receiving matching salary from contract context, not headline cap hit |
| Poison-pill treatment | Missing from transaction validation | CBA Hub concept; exact treatment marked for source verification | Source-verify rookie-extension timing and asymmetric salary values |
| Trade bonus/kicker | Missing from transaction validation | CBA term exists; no bonus allocation or waiver treatment in matching salary | Add contract clause data and source-backed allocation calculation |
| Minimum salary treatment | Missing from transaction validation | No trade-specific minimum-salary rule | Source-verify whether/how applicable contracts affect matching and roster charges |
| Roster-size consequences | Missing from transaction validation | Player counts are used only for aggregation | Validate standard/two-way roster limits, temporary under/over states, and required follow-up |
| Seven-year future-pick horizon | Partially implemented | Blocks selected draft years outside `season start + 1` through `+7` | Verify calendar/effective date and rights-versus-pick distinctions |
| Stepien Rule | Partially implemented | Adjacent-year first-round control states produce PASS/REVIEW/BLOCK | Expand alternate-pick control, conditional networks, reacquisition, and league-approved exceptions; source verify edge cases |
| Draft-pick protection | Partially implemented | Protection is classified; complex outgoing firsts force review | Evaluate every rollover/fallback path rather than treating all complexity as generic review |
| Conveyance | Partially implemented | Possible conveyance years mark control states uncertain | Build a condition graph and prove mutually exclusive outcomes across routed assets |
| Pick swaps | UI/text/review only | Swap complexity is identified; no full best/worst-of or routed swap evaluation | Model swap participants, priority, exercise conditions, and underlying physical pick |
| Duplicate asset prevention | Fully implemented for current two-team package | Duplicate database asset IDs and duplicate physical-pick keys block | Generalize uniqueness across every route and derivative right in a transaction graph |
| Duplicate player prevention | Implicit for current two-team pools | Team pools are separate; no generalized transaction-level identity validator | Add one global player-origin and single-destination constraint |
| Reacquisition restriction | Missing; requires source verification | CBA Hub concept only | Source-verify player history, time window, waiver/trade path, and exceptions |
| Cash considerations | Missing; requires source verification | CBA Hub concept only | Add annual team ledger, direction, permitted use, and apron restrictions if supported |

“Fully implemented” above means within the current modeled two-team scope; it
does not mean a league-office legality opinion.

## Readiness gates

### Required before safe three-team trades

- generalized route graph and global duplicate-player/asset validation;
- per-team salary matching from all incoming and outgoing routes;
- source-backed matching salary, BYC, poison pill, kicker, recently signed, and
  recently traded treatment;
- TPE creation/use/expiry model;
- first/second-apron prohibited mechanisms and hard-cap ledger;
- roster-size consequences;
- draft condition graph, swaps, and Stepien result for every organization;
- strict equivalence proving every existing two-team fixture is unchanged.

### Additional required before safe four-team trades

- cycle and fan-out routing tests;
- deterministic resolution of multiple TPE/acquisition mechanisms per team;
- performance protection for the larger route/condition search;
- conflict diagnostics that identify the exact team, route, player/asset, and
  rule causing FAIL/REVIEW;
- much larger combinatorial fixture matrix and cancel/reorder state tests.

Current readiness: two-team decision-support screen **usable with stated manual
review scope**; three-team **not production-ready**; four-team **not
production-ready**.

## Proposed generalized 2-4 team model

```text
transaction
  transaction_id, model_version, season, effective_date, status
  teams[]
    team_id, pre_salary, cap/apron status, roster state, TPE ledger
  player_routes[]
    player_id, from_team_id, to_team_id, contract snapshot,
    sending_salary, receiving_salary, restriction facts
  asset_routes[]
    asset_right_id, underlying_pick_id, from_team_id, to_team_id,
    protection/condition graph, verification status
  cash_routes[]
    from_team_id, to_team_id, amount, annual-ledger key
  mechanisms[]
    team_id, route_id, type (matching/TPE/cap room/sign-and-trade), source
  team_results[]
    salary result, apron/hard-cap result, roster result, draft result,
    rule findings[]
  overall_result
    PASS | REVIEW | FAIL, blocking findings[], review findings[]
```

Invariants:

- 2-4 distinct organizations;
- every route has one valid origin and one different destination;
- each player appears once and has one destination;
- each physical pick/right is uniquely routed unless it is an explicitly
  modeled derivative right;
- incoming/outgoing salary and roster counts reconcile by organization;
- no asset disappears or is created by UI ordering;
- team results are independent and overall FAIL if any team fails;
- REVIEW never becomes PASS because another team passes.

## Evaluation pipeline

1. Normalize and validate transaction graph.
2. Snapshot read-only team, contract, restriction, draft, and cap facts once.
3. Resolve matching salary for each player/side.
4. Assign each acquisition to cap room, salary matching, or a valid TPE.
5. Evaluate salary matching per organization.
6. Evaluate apron restrictions and hard-cap triggers/ceiling per organization.
7. Evaluate player timing/contract restrictions.
8. Evaluate roster counts.
9. Evaluate draft ownership, conditions, swaps, horizon, Stepien, and duplicate
   physical assets.
10. Aggregate findings to team and overall PASS/REVIEW/FAIL.

Unknown required evidence returns REVIEW or FAIL according to the verified rule
contract; it must never default to PASS.

## BIE strategy

BIE remains downstream and advisory. For each team, construct its post-trade
roster from the route graph and run the frozen evaluator/lineup/rotation paths.
Report player value sent/received, team BIE delta, role/position gaps, approved
starter impacts, and confidence. BIE cannot override a CBA FAIL and does not
participate in transaction legality.

## UI architecture

Keep the current two-team workspace as the default. A future feature-flagged
multi-team builder should use:

- a 2-4 team rail with one card per organization;
- route rows showing explicit From and To organizations;
- independent player, pick/right, and cash ledgers;
- per-team CBA status pinned above overall status;
- a route-level issue drawer linked from each finding;
- a summary matrix for salary, roster count, draft control, and BIE delta.

Selections must use stable route IDs, not positional team-A/team-B arrays. The
current shared transaction state should remain untouched until an adapter can
publish a two-team-compatible view for downstream modules.

## Phased roadmap and effort

| Phase | Deliverable | Effort |
|---|---|---:|
| 0. Source pack | Approved rule citations, effective dates, data availability, fixture catalogue | 2-4 weeks legal/CBA + engineering |
| 1. Two-team characterization | Freeze all current salary/draft/BIE/scenario outputs and route-order behavior | 2-3 engineer-weeks |
| 2. Rule facts and ledgers | Matching-salary facts, timing restrictions, TPE, hard cap, roster and cash ledgers | 6-10 engineer-weeks |
| 3. General transaction graph | Pure 2-4 team state, normalization, duplicate and routing validation | 4-6 engineer-weeks |
| 4. Per-team CBA evaluator | Source-backed team pipelines and overall result aggregation | 6-10 engineer-weeks |
| 5. Draft condition graph | Protections, conveyance, swaps, Stepien across all routes | 5-8 engineer-weeks |
| 6. Feature-flagged UI/BIE | Multi-team builder, diagnostics, downstream adapter, frozen BIE readouts | 5-8 engineer-weeks |
| 7. QA/release | 2/3/4-team fixtures, state/reorder/cancel/performance/browser tests | 4-6 engineer-weeks |

Total: approximately 32-51 engineer-weeks plus CBA/legal review and any data
acquisition. The first production milestone should be a more complete two-team
legality engine, not a wider UI over incomplete rules.
