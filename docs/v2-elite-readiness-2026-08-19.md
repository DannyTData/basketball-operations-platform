# TBI V2 Elite Readiness — Unattended Sprint

Date: 2026-08-19  
Branch audited: `v2-elite-development`  
Scope: read-only audit, characterization, architecture, and readiness planning after Phase 1C

## Executive assessment

Phase 1A, 1B, and 1C are checkpointed in commits `4b555ba`, `62c712d`, and
`3fb2b77`. The Phase 1C focused gate passes: `devtools::load_all()` succeeded
and 204 assertions passed across V1 rotation characterization, V2 contracts,
the V2 rotation engine, and V2 shadow routing. V2 remains comparison-only and
the V1 path does not execute the shadow engine.

TBI is ready to continue V2 foundation work, but it is not ready for an
authoritative visible V2 decision router. The strongest current assets are the
frozen V1 basketball baseline, versioned Phase 1 contracts, deterministic
rotation core, exact-240 V1 minute allocator, isolated lineup optimizers,
session-scoped scenarios, and substantial historical model evidence. The
trust-limiting gaps are authoritative role eligibility, current availability,
source-versioned transaction rules, transaction-specific contract facts,
verified decision dates, and reconciled game-long lineup exposure.

The next implementation should be **Phase 1D — Verified Role Eligibility
Adapter**, not production staggering or a multi-team UI. It should convert only
approved facts into `ELIGIBLE`/`NOT_ELIGIBLE`, retain model signals as evidence,
and leave everything else `UNKNOWN`.

---

## V2-ROLE-DATA-READINESS

### Read-only inventory

The application database contains 30 active teams and one 2026–27 roster/depth
snapshot with 575 rows. Historical 2025–26 role, playmaking, defense/rebounding,
shooting, impact, advanced, and box-score tables each contain 673 rows across
all 30 teams. The 2026–27 roster-to-2025–26 evidence match ranges from 60.0% to
100.0% by team; unmatched players must remain unknown. `player_positions` is
empty. `transactions` is empty. The only row in `data_sources` is SalarySwish,
marked `Imported`, not a verified basketball-role or medical source.

| Candidate input | Class | Source / fields | Scope and completeness | Provenance / reliability | Safe eligibility use | Recommended label |
|---|---|---|---|---|---|---|
| Official roster position | A when sourced; otherwise incomplete | `players.primary_position`, `depth_chart.position`; 2026–27, 575 rows/30 teams | Position is complete in `depth_chart`; row-level source is absent | Roster/depth fact, but not role proof | May constrain position legality; cannot prove backup PG/C | `DATABASE_POSITION_FACT` |
| Approved position assignment | A | `depth_chart_overrides.position`, `is_position_override`, reason/timestamp; Depth Chart approved lineup state | 203 override rows; no universal multi-position ledger | User-approved/session or DB override | Can prove a specific approved assignment; absence cannot prove ineligibility | `APPROVED_POSITION_OVERRIDE` |
| Multi-position eligibility | D currently | `player_positions` schema exists but has 0 rows | 0 players | No usable provenance | Neither eligible nor not eligible | `UNKNOWN_POSITION_ELIGIBILITY` |
| Depth order / starter | A for current stored plan | `depth_chart.depth_order`, `is_starter`; overrides | 575 rows/30 teams | Stored planning fact, not skill evidence | May order candidates; cannot establish backup role | `DEPTH_CHART_FACT` |
| Primary/secondary creator | C | `player_season_playmaking.creation_role`, creation/secondary scores, confidence; `player_season_roles` | All 30 teams in 2025–26; current-roster match 60–100% | Deterministic/model evaluation from historical performance | Can support/rank a proposal; cannot alone produce authoritative role eligibility | `MODEL_PLAYMAKING_EVIDENCE_V<metric_version>` |
| Ball handler | C/D | Creation role, assist/usage/turnover measures | Same historical coverage; no approved handler flag | Model proxy, not direct assignment | Review evidence only | `MODEL_BALL_HANDLER_PROXY` |
| Spacing/shooting | C | `player_season_shooting.spacing_score`, tier, reliability, confidence | 673/673 populated in 2025–26; all teams | Model/evaluation evidence with metric version/source name | Can score and explain; not a hard role fact | `MODEL_SPACING_EVIDENCE_V<metric_version>` |
| Defensive role | C | `player_season_defense_rebounding.defensive_role`, scores/confidence | 673 rows/30 teams; includes context-needed categories | Model/evaluation evidence | Can rank; cannot establish an approved defensive assignment | `MODEL_DEFENSIVE_ROLE_EVIDENCE_V<metric_version>` |
| Rim protection | C/D | Blocks, block percentile, `interior_impact_score`, defensive role | Historical model coverage; no approved rim-protector field | Proxy/model output | Review evidence only; never infer backup C | `MODEL_RIM_PROTECTION_PROXY` |
| Backup PG | D | No authoritative field or approved eligibility ledger | Universal gap | Generic PG position and playmaking scores are insufficient | Must remain `UNKNOWN` | `UNKNOWN_BACKUP_PG_ELIGIBILITY` |
| Backup C | D | No authoritative field or approved eligibility ledger | Universal gap | Generic C position and interior metrics are insufficient | Must remain `UNKNOWN` | `UNKNOWN_BACKUP_C_ELIGIBILITY` |
| Current availability | D | No authoritative availability table/feed | Universal gap | `roster_status` and historical `availability_pct` are not current medical status; 2025–26 `availability_pct` is null | Must remain `UNKNOWN` unless explicitly supplied | `UNKNOWN_AVAILABILITY` |
| Manual availability scenario | A within scenario scope | Phase 1 V2 contract input / future scenario contract | Only players explicitly supplied | User override, not medical fact | Can drive scenario behavior, clearly labeled | `USER_AVAILABILITY_OVERRIDE` |

### All-30-team summary

All 30 teams have 2026–27 roster and depth rows and 2025–26 model tables. The
current-roster match to each historical model family is identical by team and
ranges from 60.0% (OKC) to 100.0% (NYK). This is useful for evaluation but not
enough to turn universal role coverage from `REVIEW` to `PASS`, because the
missing fact is authority, not merely row count. No team has database-backed
current availability or populated authoritative multi-position eligibility.

### Smallest trustworthy role contract

Extend, do not replace, `tbi-v2-role-eligibility` with a versioned evidence
record:

```text
role_eligibility_v2
  player_id, team_id, season, role
  eligibility: ELIGIBLE | NOT_ELIGIBLE | UNKNOWN
  fact_type: APPROVED_ASSIGNMENT | VERIFIED_SCOUTING | VERIFIED_POSITION | UNKNOWN
  effective_from, effective_to
  source_id, source_reference, verified_at, verified_by
  evidence_fields[], model_support[]
  validation: PASS | REVIEW | FAIL
```

Only affirmative and negative facts from an approved source may produce
`ELIGIBLE` or `NOT_ELIGIBLE`. Model evidence may support an explanation but may
not cross that boundary. Absence of a record produces `UNKNOWN`, never
`NOT_ELIGIBLE`. This one contract, populated first for backup PG and backup C,
is the smallest safe step away from universal `REVIEW`.

---

## V2-TRANSACTION-RULE-FOUNDATION-AUDIT

### Current foundation

`trade_engine.R` is explicitly a first-pass two-team salary screen.
`evaluate_two_team_trade()` evaluates both organizations; salary bands,
cap-room absorption, threshold crossings, and one second-apron aggregation
check are covered. `tbi_trade_draft_screen()` adds ownership/control,
duplication, horizon, Stepien-control, and complexity review. The transaction
state is session-only and downstream roster/payroll previews do not write to
the database.

| Rule/mechanism | Current implementation / location | Required facts | Provenance gap | Expected result | Complexity / risk | Sequence |
|---|---|---|---|---|---|---:|
| Salary matching | Partial: `calculate_maximum_incoming_salary()`, `evaluate_team_trade()` | Matching salary, team salary, thresholds, date | Formula/effective-date source version | PASS within verified limit; FAIL over; REVIEW if facts/source missing | M / very high CBA | 2 |
| Cap-room acquisition | Partial in maximum-incoming calculation | Cap room, roster charges, timing | Room calculation source contract | PASS/FAIL when complete; REVIEW otherwise | M / high | 3 |
| First apron | Status/crossing only | Pre/post salary, mechanisms, date | Prohibited-mechanism and trigger sources | Crossing/restriction unknown => REVIEW; verified violation => FAIL | L / very high | 5 |
| Second apron | Dollar-for-dollar and multi-player outgoing aggregation check | Salary, aggregation facts, mechanisms | Full restriction/effective-date pack | Verified violation FAIL; incomplete rule set REVIEW | L / very high | 5 |
| Aggregation | Partial second-apron outgoing-count proxy | Player matching salary, aggregation groups/timing | Definition and route/timing rules | FAIL only on verified prohibited case; otherwise REVIEW | L / very high | 4 |
| Hard-cap triggers | Missing | Triggering mechanism/event/date | Entire source-versioned trigger ledger | `REQUIRES SOURCE VERIFICATION`; REVIEW | L / very high | 5 |
| Hard-cap ceiling | Missing | Active ceiling, full team salary, charges | Entire ledger/source pack | Verified excess FAIL; missing facts REVIEW | L / very high | 5 |
| Traded player exceptions | Text/UI only | TPE amount, creation, expiry, use, route | No TPE ledger | `REQUIRES SOURCE VERIFICATION`; REVIEW | L / very high | 6 |
| BYC | Missing | Contract path, prior/new salary, date | Contract fields and rule source | REVIEW until source/facts; verified mismatch can FAIL | M / very high | 4 |
| Poison pill | Missing | Extension facts, effective period, salary treatment | Start dates are entirely missing in current contracts | `REQUIRES SOURCE VERIFICATION`; REVIEW | M / very high | 4 |
| Trade bonus/kicker | Missing | Clause %, waiver, remaining allocation | `trade_bonus_percent` populated for 0/602 contracts | REVIEW; verified calculation feeds salary | M / high | 4 |
| Recently signed | Missing | Signing date/type, restriction end | Contract start season missing for 602/602; no signing date | `REQUIRES SOURCE VERIFICATION`; REVIEW | M / very high | 4 |
| Recently traded | Missing | Acquisition transaction/date, aggregation status | `transactions` has 0 rows | `REQUIRES SOURCE VERIFICATION`; REVIEW | M / very high | 4 |
| Sign-and-trade | Missing | transaction type, FA/signing facts, recipient state | No authoritative event facts/source pack | Verified violation FAIL; otherwise REVIEW | L / very high | 6 |
| Reacquisition | Missing | player transaction/waiver history and date | `transactions` empty | `REQUIRES SOURCE VERIFICATION`; REVIEW | M / very high | 6 |
| Roster limits | Missing in transaction evaluator | Standard/two-way counts, temporary rules/date | Rule/effective-date pack | Verified excess FAIL or REVIEW if cure permitted; missing rules REVIEW | M / high | 7 |
| Cash considerations | Missing | direction, amount, annual ledger, date | No cash ledger or verified rule | `REQUIRES SOURCE VERIFICATION`; REVIEW | M / high | 7 |
| Draft ownership | Partial: draft screen | Current control/right, source, date | Active assets have URLs but 0 `VERIFIED` statuses | Unknown ownership FAIL for routing; uncertain control REVIEW | M / very high | 8 |
| Pick protections | Partial classification/review | Full protection paths | Condition graph not fully resolved | Complex/unknown REVIEW; invalid route FAIL | L / high | 8 |
| Pick swaps | Identified, not fully evaluated | Participants, priority, physical pick, conditions | Nine active swap rows; no verified resolved network | REVIEW until resolved | L / high | 8 |
| Stepien | Partial adjacent-control states | All possible first-round control outcomes | Source verification and alternate-control edges | Verified loss of control FAIL; uncertainty REVIEW | L / very high | 8 |
| Conditional picks/conveyance | Partial generic review | Mutually exclusive condition graph | 52 networks; all require manual review; none resolved/verified | REVIEW until all paths resolved | XL / high | 8 |
| Transaction timing | Not modeled as required input | Exact transaction date and rule versions | No scenario effective date contract | Missing date => REVIEW for date-sensitive rules | M / very high | 1 |

Contract data reinforces the risk: 602 contracts have zero populated trade
bonus percentages and zero populated start seasons; only 406 have free-agent
status. Of 1,165 contract-year rows, 593 lack `source_id` and `verified_at`.
No unsupported item above may advance beyond `REQUIRES SOURCE VERIFICATION`.

### Safest incremental evaluator path

1. Require `effective_date`, league year, rule-pack version, and fact snapshot.
2. Freeze current two-team outputs as `trade-screen-v1` equivalence fixtures.
3. Add matching-salary fact resolution (ordinary, BYC, poison pill, kicker).
4. Add player timing restrictions with authoritative event dates.
5. Add apron/hard-cap state and ledgers.
6. Add TPE and sign-and-trade mechanisms.
7. Add roster/cash consequences.
8. Add verified draft condition graph and Stepien evaluation.
9. Expose `transaction-evaluator-v2` behind a feature flag; never silently
   reinterpret V1 results.

---

## V2-PHASE-2A-MINUTES-STAGGERING

### Characterized V1 behavior

`build_minute_allocation()` selects 8–12 players, preserves starters when
available, assigns bounded minutes, rounds to exactly 240, honors manual
overrides, gives `OUT` players zero priority/minutes, and applies lower bounds
and caps for starters/bench/limited players. Tests cover exact 240, starter
retention, limited caps, manual overrides, and comparisons. Phase 1A also
freezes active schema, DB-call counts, rookie behavior, ties, NA handling,
short rosters, and signatures. Identical inputs are deterministic.

Important limitations: V1 normalizes unrecognized availability to
`AVAILABLE`; V2 correctly normalizes it to `UNKNOWN`. V1 has several later
compatibility redefinitions of `prepare_minute_allocation_roster()` and the
lineup equivalent; they are protected characterization targets and must not be
refactored during Phase 2A. Current caches use roster/lineup/size and related
signatures but no canonical minute-ledger or stagger-policy signature.

### Target pipeline and APIs

```text
Rotation -> Minute Ledger -> Game Segments -> Substitution Events
         -> Coverage Validation -> reconciled 240 team minutes
```

- `allocate_v2_minutes(rotation, starter_state, policy, manual_overrides,
  availability_scenario)` returns a new ledger, initially through a V1 adapter.
- `build_v2_stagger_plan(minute_ledger, rotation, role_eligibility, policy)`
  builds deterministic regulation segments and events.
- `validate_v2_minute_ledger(ledger)` checks identities, bounds, individual
  totals, overrides, availability, and exact team total.
- `validate_v2_segment_coverage(segments, ledger, role_eligibility)` checks
  five-player uniqueness, clocks, exposure reconciliation, and role coverage.

### Versioned contracts

**Minute Ledger:** `ledger_id`, model version, plan/scenario IDs, player ID,
target/assigned minutes, minimum/maximum, rotation/starter flags, availability,
source, override provenance, fact/model evidence, missing evidence, reasons,
and row/aggregate validation.

**Game Segment:** segment ID, period, start/end clock, duration, five player
IDs, creator coverage, primary/secondary handler coverage, big/center coverage,
lineup ID, lineage, evidence, missing evidence, and validation.

**Substitution Event:** event ID, period/game clock, player in/out, reason,
resulting five-player unit, source/provenance, base event ID, and validation.

### Policy behavior

Stagger primary and secondary handlers where verified, then starting-big and
backup-center coverage; build bench bridges; flag all-bench exposure; and make
the closing transition explicit. Unknown creator/handler/center evidence
creates `REVIEW`, not invented coverage. Every segment contributes duration to
five players; summed segment exposure must equal both each player's assigned
minutes and 240 team minutes. Infeasible bounds or coverage produce
`FAIL/is_blocked=TRUE`; missing role evidence produces
`REVIEW/is_blocked=FALSE` unless it makes construction impossible.

---

## V2-LINEUP-PORTFOLIO-READINESS

### Current reusable capability and gaps

`build_lineup_optimization()` produces balanced, offense, defense, and closing
groups from a candidate pool. It enforces five unique players, excludes `OUT`,
uses position-balance review, scores player metrics, preserves stable candidate
behavior, and protects the preseason rookie gate. It optimizes isolated groups;
it does not reconcile group exposure to player minutes or game segments.

| Lineup type | Reusable inputs | Missing evidence / dependencies | Required validation |
|---|---|---|---|
| Base | Approved starters, V1 legality, BIE/model metrics | Versioned rotation/minute lineage | Locks, five unique, assigned-position legality |
| Bench bridge | Rotation bench/order, minute targets | Verified creator and big coverage | Segment coverage and exposure reconciliation |
| Offensive | Offensive impact, creation, spacing | Pair/group evidence and reliability | Legality plus evidence-qualified explanation |
| Defensive | Defensive impact/rebounding | Matchup assignments, rim role authority | Legality; unknown matchup remains REVIEW |
| Small-ball | Position normalization, spacing | Approved switch/small-ball eligibility, opponent context | No generic-position inference; REVIEW if unsupported |
| Closing | Existing closing objective | Situation, verified roles, minutes, reliability | Base-versus-closing lineage and CBA-independent recommendation |

TBI has player-level on-court and on/off metrics in
`player_season_advanced`, but no trustworthy pair, three-player, or five-player
evidence store, lineup sample-size ledger, or portfolio reliability threshold.
On/off is player-level model evidence, not synergy. Pair/group synergy is
therefore `UNKNOWN`.

### APIs and contract

- `build_v2_lineup_portfolio(rotation, minute_ledger, eligibility,
  objectives, policy)` creates candidate situation units.
- `reconcile_lineup_exposure(portfolio, segments, minute_ledger)` proves
  group-to-player minute equality and reports residuals.
- `select_v2_closing_lineup(portfolio, situation, policy)` selects only among
  legal/reliable candidates and records missing evidence.

`lineup_portfolio_v2` contains lineup ID, player IDs, assigned positions,
objective, target/actual minutes, legality, score components, reliability,
fact/model evidence, missing evidence, reasons, validation, and base/situation
lineage. Reliability is never implied by a score; absent samples remain
`UNKNOWN`/`REVIEW`.

---

## V2-INJURY-AVAILABILITY-READINESS

### Known versus unknown

TBI's engines recognize `AVAILABLE`, `LIMITED`, and `OUT`; Phase 1 V2 also
preserves `UNKNOWN`. Tests characterize zero minutes for `OUT`, caps for
`LIMITED`, and blocked locked-starter conflicts. The production database does
not contain authoritative current injury status, expected return, minute
restriction, workload directive, medical clearance, injury duration, rest, or
back-to-back status. Historical games/minutes and roster status cannot supply
those facts.

### Scenario architecture

```text
Base Rotation Plan -> Availability Scenario -> Replacement Starter Proposal
-> Derived Rotation -> Derived Minutes -> Derived Lineups
-> Organizational Delta
```

`availability_scenario_v2` contains scenario/base-plan IDs, player ID, status,
restriction type, explicit minute ceiling only when supplied, effective
window, source/provenance, verified fields, unknown fields, findings, canonical
status, and independent `is_blocked`.

| Condition | Required behavior |
|---|---|
| Locked starter OUT | Preserve lock in base; `REVIEW` or `FAIL` per contract with `is_blocked=TRUE`; separate replacement proposal |
| Locked starter LIMITED | Preserve lock; apply only explicit ceiling; REVIEW if ceiling/clearance unknown |
| Unlocked starter OUT | Derive replacement proposal; zero minutes; validate legality/coverage |
| Sixth man OUT | Re-rank bench deterministically; explain role/minute delta |
| Backup PG/C OUT | REVIEW coverage unless another player has verified eligibility |
| Multiple unavailable | Derive immutably; FAIL/blocked if rotation or 240-minute plan infeasible |
| Unknown medical facts | Keep fields UNKNOWN; never estimate return/clearance/restriction |

---

## V2-CONTRACT-CALENDAR-READINESS

### Supportable facts and gaps

Current modules can display salary/cap hits, guarantee amounts when present,
option labels, contract end/free-agent year, free-agent status, signing
exception, thresholds, and draft asset years. They cannot safely generate a
deadline calendar from season labels alone.

| Event | Current fact availability | Authoritative date / rule status | Consequences / gap |
|---|---|---|---|
| Team/player options | 282 contract-year option rows | No exercise deadline field | Calendar event remains REVIEW |
| Qualifying offers | FA status/contract type may hint | No QO amount/deadline/eligibility fact | `REQUIRES SOURCE VERIFICATION` |
| Guarantee/partial guarantee | 911/1,165 rows have guarantee amount | No guarantee date or schedule | Amount usable; decision date UNKNOWN |
| Extension eligibility | Simulator models scenarios | No authoritative eligibility-window ledger | Rule/date must be sourced/versioned |
| Rookie/veteran windows | Glossary/UI knowledge | No exact verified dates | UNKNOWN |
| Free agency | 406/602 contracts have FA status; free-agent year exists | No event date | Year-level planning only |
| Trade eligibility | No signed/traded dates; transactions table empty | Unsupported | UNKNOWN/REVIEW |
| Draft decisions | Asset year/control/conditions exist | Decision deadlines not modeled | UNKNOWN; dependency graph only |
| Exception consequences | Cap module displays exception context | No event-ledger effects | Needs transaction rule foundation |
| Roster deadlines | No authoritative date table | Unsupported | UNKNOWN |

### APIs and event contract

- `build_contract_event_snapshot(team, season, as_of_date, source_pack)`
  snapshots facts without inventing dates.
- `build_decision_calendar(snapshot, rule_pack)` emits verified events and
  explicit missing-date findings.
- `sequence_decision_events(events, scenario)` topologically orders effects and
  detects cycles/unknown dependencies.

`contract_event_v2` contains event ID/type, team/player/asset IDs, effective
and deadline dates, date precision, status, governing rule ID/version,
fact/source snapshot, verification state, dependencies, cap/roster/draft/CBA
consequences, unknown fields, validation, and scenario lineage.

Sequence example: option exercise changes guaranteed payroll -> changes apron
and cap room -> changes extension or transaction mechanism availability ->
changes Five-Year Outlook. Persistent alerts remain out of scope.

---

## V2-DRAFT-DECISION-LAB-READINESS

The draft stack has an asset inventory, protection classification, condition
tables, network tables, portfolio valuation, deterministic simulation with
frozen RNG behavior, conveyance/swap simulation, transaction screening, and
Five-Year/roster surfaces. Of 352 active assets, none is marked `VERIFIED`; 121
have transaction references. All 52 modeled right networks require manual
review and none is marked resolved/verified.

| Branch | Reusable evidence | Missing/dependency | Required uncertainty treatment |
|---|---|---|---|
| KEEP | Asset/control data, valuation, frozen simulation | Prospect/slot outcome evidence, roster need authority | Distribution/model output, never promised outcome |
| TRADE | Trade draft screen, asset value | Complete transaction evaluator and verified ownership | CBA/draft FAIL dominates |
| MOVE BACK | Asset routing/simulation concepts | Actual offer, rights graph, roster slots | Scenario assumption explicitly labeled |
| DEFER/FUTURE VALUE | Future assets, Five-Year view | Verified conveyance/control and discount policy | Model range plus verification gaps |

Pipeline: baseline portfolio -> decision branch -> transaction/CBA -> roster ->
cap -> rotation -> Five-Year -> comparison.

Proposed APIs are `build_draft_decision_scenario()`,
`enumerate_draft_options()`, `evaluate_draft_decision_branch()`, and
`compare_draft_decisions()`. The versioned branch contract includes scenario,
base portfolio, branch type, routed assets, assumptions, RNG seed/version,
transaction result, cap/roster/rotation/Five-Year deltas, evidence, uncertainty,
missing facts, validation, and explanation log. Frozen RNG and current fixtures
remain equivalence references. No prospect outcome may be fabricated.

---

## V2-SCENARIO-STATE-ARCHITECTURE

### Current state classification

| State | Classification | Finding |
|---|---|---|
| SQLite roster/contracts/cap/draft/model tables | Canonical database facts | Read-only application source of truth |
| Selected team/season | Mutable session context | Shared by app server; must be part of signatures |
| Approved starters/depth overrides | Canonical approved plan facts plus mutable UI state | Must not be silently replaced |
| `tbi_transaction_state()` | Mutable session-only scenario | Two-team shape; safe through an adapter |
| Depth Chart reactives/scenario lineup | Mutable duplicated/module-local | `REQUIRES ADAPTER` |
| Phase 11 minute/lineup outputs | Derived/cache-only | `SAFE` as V1 reference; not canonical V2 state |
| Phase 12 scenario comparison | Derived | `SAFE` consumer after adapter |
| Phase 13 caches | Cache-only/session | `REQUIRES ADAPTER` for full V2 signature |
| Extension scenarios | Session-local derived proposals | `REQUIRES ADAPTER` |
| Draft simulation state | Derived with frozen RNG | `SAFE` if seed/model retained |
| Phase 1 V2 starter/rotation/shadow state | Versioned immutable-like derived state | `SAFE` foundation |
| Production-state migrations | Not authorized | `REQUIRES FUTURE MIGRATION` |

### Canonical envelope

```text
Database Facts + Team/Season + Approved Locks + Transaction Facts
+ Availability Facts + Manual Overrides
-> versioned_decision_scenario_v2
-> Rotation | Minutes | Lineups | Cap | CBA | Draft | Five-Year
-> Organizational Impact
```

The envelope contains scenario ID/version, parent ID, team/season, created-at,
type, input signatures, immutable fact-snapshot references, transaction facts,
availability facts, manual overrides, model/rule/contract versions, validation,
provenance, and typed explanation log. A base scenario is immutable. Every
change creates a derived child referencing its parent and delta; no downstream
engine mutates upstream facts or cached base output in place.

Potential mutation paths in transaction and Depth Chart reactive values are
safe for V1 but require adapters before becoming V2 inputs. Database-writing
admin/depth override helpers require future migration review and are not part
of scenario evaluation.

---

## V2-ORGANIZATIONAL-IMPACT-ARCHITECTURE

Current recommendations are module-local composites: Command Center and Team
Overview summarize signals; roster/player modules use BIE and role/model
outputs; Trade combines salary/draft/BIE screens; scenario comparison reports
rotation/minute/lineup deltas; Cap, Extension, Draft, Five-Year, and CBA expose
domain views. There is no canonical cross-domain recommendation contract.

`organizational_impact_v2` should contain:

- identity: impact ID, scenario/base IDs, team, season, model/rule versions;
- basketball: starter, rotation, minute, lineup, BIE, and verified coverage
  deltas;
- financial: payroll, cap room, tax, apron, and flexibility deltas;
- CBA: PASS/REVIEW/FAIL, `is_blocked`, triggered rules/restrictions/provenance;
- assets: draft capital, control, obligations, and uncertainty;
- multi-year: contract obligations, flexibility, constraints;
- risk: missing evidence, model uncertainty, dependencies, verification gaps;
- recommendation: `PROCEED`, `PROCEED_WITH_REVIEW`, or `DO_NOT_PROCEED`;
- typed explanations and validation.

Every explanation item carries exactly one epistemic label: `FACT`, `RULE`,
`MODEL OUTPUT`, `ASSUMPTION`, `UNKNOWN`, or `USER OVERRIDE`. Aggregation order
is hard constraints/CBA first, then data completeness, then basketball and
financial evaluation. Any CBA `FAIL` forces `DO_NOT_PROCEED`, regardless of
BIE or other upside. `REVIEW` cannot be upgraded to PASS by a model score.

---

## V2-RELEASE-HARDENING-PLAN

### Performance finding

The previously recorded 0.510s value is the V1 BIE lineup summary within a
Depth Chart cold run, not a proven V2 shadow overhead measurement. Phase 1C
shadow work adds representative comparison-only cost, while strict all-team
validation multiplies roster reads, two rotation sizes, contract validation,
hash/signature work, and deterministic reruns across 30 teams. A dedicated
profiler harness is required before attributing the 0.51s figure to shadow
routing.

User-session work should evaluate one selected team, cache immutable inputs by
complete signature, isolate shadow exceptions, and impose a small latency
budget. QA work may validate all 30 teams, adverse fixtures, repeatability,
database hashes, and V1 equivalence outside the request path. Basketball logic
must not be weakened to make the harness faster.

| Release item | Classification | Exit evidence |
|---|---|---|
| R runtime/`renv` synchronization | BLOCKING BEFORE V2 VISIBLE | Clean restore/status on release runtime |
| Phase 1C and V1 regression suites | BLOCKING | Green focused and protected suites |
| BIE freeze / 65-25-10 / rookie/locks | BLOCKING | Explicit freeze/equivalence report |
| Feature flag defaults to V1; rollback | BLOCKING | Route tests and operational rollback drill |
| Authoritative role/availability missing-data behavior | BLOCKING | All-team `UNKNOWN`/REVIEW fixtures |
| Database hash and read-only verification | BLOCKING | Before/after hash and read-only connections |
| Error isolation | BLOCKING | V2 exception leaves V1 rendered/state intact |
| Shiny ID/navigation/team/season/state preservation | BLOCKING | Static + browser regression |
| Cache invalidation/signatures | BLOCKING | Team/season/roster/locks/scenario/policy/version tests |
| Contract/model/rule version display | BLOCKING | Visible version and provenance QA |
| CBA FAIL dominance | BLOCKING | Cross-domain aggregation tests |
| User-session profiling | RECOMMENDED | Cold/warm selected-team budget |
| QA harness profiling/sharding | RECOMMENDED | Per-stage timings, deterministic shards |
| Desktop/tablet/mobile browser QA | BLOCKING | Real viewport evidence |
| Accessibility keyboard/focus/status semantics | BLOCKING | Automated and manual audit |
| Diagnostics/logging | RECOMMENDED | Structured, no medical/private fabrication |
| Stale legacy test reconciliation | RECOMMENDED | Approved contract decisions, not weakened tests |
| Persistent cross-session cache | CAN DEFER | Requires separate privacy/invalidation approval |
| Production multi-team UI | CAN DEFER | Rule/data foundation incomplete |

Profile separately: data snapshot, adapter construction, starter validation,
10/11 rotation builds, explanation assembly, signatures, and repeat run. For
all-team QA record median/p95 per team and stage, DB call counts, and hash.

---

## V2-MULTITEAM-TRADE-READINESS

Multi-team trading is not ready for implementation. The two-team engine remains
the equivalence reference until the transaction foundation is source-complete.

### General transaction graph

The versioned graph contains 2–4 team nodes; player, pick/right, swap, and cash
routes; acquisition mechanisms; immutable contract/team/asset snapshots;
per-team incoming/outgoing ledgers; team results; and one overall result.
It supports chains, cycles, and fan-out while enforcing global player and asset
uniqueness.

- `normalize_transaction_graph()` canonicalizes IDs/order without changing
  economics.
- `validate_transaction_routes()` checks origins, destinations, ownership,
  uniqueness, conservation, and route completeness.
- `snapshot_transaction_facts()` reads all facts once for an effective date.
- `evaluate_transaction_team()` applies salary, mechanisms, apron/hard cap,
  player restrictions, roster, cash, and draft rules independently.
- `evaluate_multiteam_transaction()` aggregates team results; one team FAIL
  makes the overall result FAIL and one unresolved REVIEW prevents PASS.

Required tests: two-team equivalence; three-team chain and circular route;
four-team transaction; player twice; pick/right twice; invalid ownership;
one-team CBA FAIL; one-team REVIEW; route reorder stability; cancellation;
salary and roster reconciliation; cash conservation; cycles/fan-out; and
database immutability.

---

# Final master report — V2 Elite Readiness

## Master dependency graph

```text
Source/version governance + canonical fact snapshots
  ├─ Verified role/position/availability contracts
  │    └─ Rotation 1D -> Minutes/Staggering -> Lineup Portfolio
  │         └─ Injury Scenarios ─┐
  ├─ Transaction effective date + contract restriction facts
  │    └─ Two-team Rule Foundation -> Draft Condition Graph
  │         └─ General Transaction Graph -> 3-team -> 4-team
  ├─ Contract event facts/rules -> Decision Calendar
  └─ Verified draft rights + frozen RNG -> Draft Decision Lab

All domain results -> Canonical Scenario Envelope
                   -> Organizational Impact
                   -> Visible V2 router and release hardening
```

## Master data-gap matrix

| Rank | Gap | Impact | Consequence |
|---:|---|---|---|
| 1 | Authoritative current availability/medical directives | Critical | Cannot safely derive injury rotations or minute caps |
| 2 | Verified backup PG/C and multi-position eligibility | Critical | Rotation Core remains universal REVIEW |
| 3 | Transaction dates/history and signing/acquisition dates | Critical | Timing, aggregation, reacquisition rules unsupported |
| 4 | Contract matching-salary facts (BYC/PPP/kicker) | Critical | Salary screen cannot become legality evaluator |
| 5 | Source-versioned CBA rule/effective-date pack | Critical | Unsupported interpretations cannot PASS |
| 6 | Hard-cap/TPE/cash ledgers | High | Apron/mechanism evaluation incomplete |
| 7 | Verified draft ownership/condition/swap graph | High | Stepien and routed asset conclusions remain REVIEW |
| 8 | Pair/3/5-player exposure and sample reliability | High | Portfolio/closing synergy remains UNKNOWN |
| 9 | Contract decision dates/windows | High | Calendar cannot be authoritative |
| 10 | Current-roster historical model gaps (0–40% by team) | Medium | Model ranking/explanation incomplete, especially new players |

## CBA/source-verification gaps

Priority source packs are: transaction effective-date/version rules; matching
salary bands and cap-room treatment; BYC/poison-pill/kicker; recently
signed/traded and reacquisition; apron restrictions, aggregation, hard-cap
triggers/ceiling; TPE and sign-and-trade; roster/cash; Stepien, swaps,
protections, conditional conveyance, and seven-year timing. Until verified,
each is `REQUIRES SOURCE VERIFICATION` and cannot create PASS.

## Technical debt boundaries

**Must not refactor yet:** frozen BIE internals; 65/25/10 optimizer; approved
locks; candidate ordering; rookie gate; active duplicate compatibility
definitions in minute/lineup preparation; two-team transaction state/engine;
current scenario behavior; Shiny IDs/mounting; database source-of-truth paths.

**Safe during V2 isolation:** new versioned adapters/contracts; pure validators;
immutable scenario envelope; explicit signatures; QA profilers; fact/source
ledgers in new schemas after approval; feature-flag diagnostics; stale-test
reconciliation only after current behavior is explicitly approved.

## Exact development sequence after Phase 1C

1. Phase 1D verified role eligibility adapter and source contract.
2. Phase 1E authoritative availability scenario input contract.
3. Phase 2A minute-ledger adapter and validation (no staggering production).
4. Phase 2B segment/stagger planner behind shadow flag.
5. Phase 2C lineup portfolio/exposure reconciliation.
6. Transaction Foundation 0: effective date, source pack, fact snapshot.
7. Transaction Foundation 1: matching salary and player restrictions.
8. Scenario envelope and Organizational Impact aggregation.
9. Contract calendar and draft decision lab foundations.
10. Only then general transaction graph and multi-team progression.

## Next ten implementation milestones

| # | Milestone | Objective / dependencies | Likely files | Effort | Tech risk | Basketball/CBA risk | Exit gate |
|---:|---|---|---|---|---|---|---|
| 1 | Phase 1D — Verified Role Eligibility Adapter | Populate contract from approved facts only; Phase 1C | new `R/v2_role_evidence.R`, V2 contracts/state/tests | MEDIUM | Medium | High | All teams deterministic; unknowns preserved; no generic-position inference |
| 2 | Phase 1E — Availability Scenario Contract | Explicit scenario-only status/restrictions; milestone 1 | new V2 availability/state/tests | MEDIUM | Medium | Very high | Locked conflicts blocked; no medical inference |
| 3 | Phase 2A — Minute Ledger Adapter | Wrap characterized V1 allocator; milestones 1–2 | new `R/v2_minutes.R`, minute tests | MEDIUM | Medium | High | Exact 240, immutable inputs, V1 equivalence |
| 4 | Phase 2B — Shadow Stagger Planner | Legal segments/events and coverage; milestone 3 | new stagger/segment engine/tests | LARGE | High | High | Segment and individual minutes reconcile exactly |
| 5 | Phase 2C — Lineup Portfolio | Reconcile base/bridge/offense/defense/closing; milestone 4 | new portfolio engine/tests | LARGE | High | High | Legal five-player groups; UNKNOWN synergy; exposure equality |
| 6 | Transaction Foundation 0 | Effective date, rule/source versions, fact snapshot | new transaction contracts/source registry/tests | MEDIUM | Medium | Very high | Missing date/source produces REVIEW, never PASS |
| 7 | Transaction Foundation 1 | Matching salary, BYC/PPP/kicker/timing | new v2 evaluator/adapters; trade tests | VERY LARGE | High | Very high | Source-backed fixture matrix; V1 unchanged |
| 8 | Canonical Scenario Envelope | Immutable base/derived cross-domain state | new scenario contracts/adapters/tests | LARGE | High | High | No upstream mutation; complete signatures |
| 9 | Organizational Impact v1 | Typed evidence and fail-dominant recommendation | new impact engine; module adapters/tests | LARGE | High | Very high | CBA FAIL always DO_NOT_PROCEED |
| 10 | Release Candidate Shadow Gate | Performance, browser, accessibility, rollback | app routing/tests/docs/harness | LARGE | High | High | All blocking release items green; visible flag still approval-gated |

The strongest “real NBA front-office system” milestones are the game-long
Minutes/Staggering plan, the reconciled Lineup Portfolio with closing lineage,
and the Organizational Impact report that joins basketball, finance, CBA,
assets, and multi-year consequences without hiding uncertainty.

## Trust blockers and premature features

Serious decision support is currently prevented by absent authoritative
availability and role eligibility, incomplete/source-unversioned CBA legality,
missing transaction dates and matching-salary facts, unresolved draft rights,
and lack of portfolio exposure reconciliation. The product can support
screening and scenario exploration when these limitations are explicit; it
cannot safely present league-office legality, medical certainty, or lineup
synergy.

Do not build visually impressive multi-team trade canvases, medical-return
timelines, “AI chemistry” heatmaps, precise closing-lineup win projections,
automated contract deadline alerts, or prospect career forecasts yet. Their
foundations are insufficient and visual polish would amplify false confidence.

## Roadmap: NOW / NEXT / LATER V2 / V3

**NOW:** role eligibility adapter; availability contract; minute ledger;
transaction source/date/fact foundation; release environment reconciliation.

**NEXT:** shadow staggering; lineup portfolio; transaction matching-salary and
timing rules; immutable scenario envelope; Organizational Impact.

**LATER V2:** contract calendar; draft decision lab; verified draft condition
graph; general transaction graph; three-team then four-team UI.

**V3:** constraint-based roster construction and calibrated predictive
simulation after historical validation/model governance.

### Comparison with `docs/v2-v3-product-roadmap.md`

The audit confirms the roadmap's foundation-first direction, frozen V1
rollback, Rotation Core priority, transaction-rule priority, injury/minutes/
lineup dependency chain, and deferral of multi-team and predictive work. It
changes one ordering detail: the roadmap treats V2 Rotation Core as the next
feature, but Phase 1C proves the engine exists and the true next gate is the
verified role-eligibility adapter. It also separates availability contract
governance from the richer injury product and moves the canonical scenario
envelope/Organizational Impact earlier, before later decision labs. Newly
discovered facts include an empty `player_positions` table, empty transaction
history, no contract start seasons or trade-bonus percentages, 593 unsourced
contract-year rows, and zero verified/resolved active draft networks.

## Recommended next Codex assignment

**Phase name:** TBI V2 Phase 1D — Verified Role Eligibility Adapter.

**Scope:** audit-approved, read-only adapter and versioned contract for backup
PG, backup C, and approved multi-position eligibility; support direct approved
facts and optional historical model support; all other values remain UNKNOWN;
run all-30-team shadow characterization.

**Dependencies:** Phase 1C commits/tests; approved source and provenance policy;
explicit decision on whether Depth Chart overrides count as authoritative only
for the assigned position or for broader eligibility.

**Safety boundaries:** no V1/BIE/minute/lineup changes; no DB mutation; no
generic-position role inference; no availability inference; no visible V2
routing; session/read-only facts only; PASS/REVIEW/FAIL plus independent
`is_blocked`.

**Exit gates:** new contract validation tests; affirmative/negative/unknown
fixtures; source/provenance rejection tests; all-30 deterministic shadow run;
V1 characterization green; Phase 1C green; BIE frozen; DB hash unchanged;
`devtools::load_all()` and `git diff --check` pass.

---

## Safety verification

This sprint added only this documentation artifact. No production R files,
tests, or database contents were intentionally changed. No commit, push, or
merge was performed. Final command evidence is reported in the completion
message accompanying this document.
