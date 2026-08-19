# V2 Rotation Engine Architecture Roadmap

## Decision

V2 should be a new, versioned layer built around:

```text
Starter layer -> Rotation layer -> Minutes layer -> Lineup combinations -> Situations
```

It must consume the frozen V1 BIE player evaluations and approved starter
locks; it must not alter BIE formulas, the 65/25/10 starting-five optimizer,
lineup legality, or the rookie gate.

## Current repository map

| Current area | Functions/state | What already works | V2 gap |
|---|---|---|---|
| Approved starters | `mod_depth_chart_server()`, `baseline_lineup()`, `active_lineup()`, `scenario_lineup`, `depth_chart_optimize_bie_starting_five()` | Five-position lineup, drag/drop scenario, approved locks, frozen 65/25/10 search | No explicit versioned starter contract shared with downstream layers |
| Rotation and minutes | `build_minute_allocation()`, `select_rotation_players()`, `player_minute_bounds()`, `build_phase11_rotation_result()` | 8-12 player support, exact 240 minutes, starter minimums, availability, manual overrides, 9/10-man UI | No named sixth man, backup-PG/C coverage, stagger plan, or 11-man product flow |
| Lineup combinations | `build_lineup_optimization()`, `optimize_lineup_type()`, `depth_chart_build_phase11_lineup_result()` | Balanced/offense/defense/closing five-man optimization and position review | Optimizes isolated groups, not a game-long set of combinations constrained by minutes/staggering |
| Situational comparison | `build_scenario_comparison()`, Phase 12 caches | Base-versus-transaction roster, minutes, rotation, and lineup changes | No foul trouble, rest, back-to-back, development, matchup, injury-duration, or playoff policy hierarchy |
| State and caching | Depth Chart reactive values plus Phase 13 caches | Roster signatures avoid repeat BIE and lineup work | State is module-local; no canonical rotation-plan ID/version or override ledger |

Technical debt to address during V2 isolation: `prepare_minute_allocation_roster()`
and `prepare_lineup_player_pool()` are redefined by later compatibility blocks.
The active definitions must first be captured by characterization tests; do not
clean them up as an unprotected refactor.

## Available inputs

The current app can supply player ID/name, official and assigned position,
depth order, starter flag, availability status, BIE rating, projected BIE,
impact, role/archetype, current/recommended minutes, contract/roster status,
approved lineup, transaction scenario, team, and season.

Inputs not yet modeled consistently include:

- possession-level substitution history and time-of-game availability;
- on/off and pair/group synergy with reliability;
- foul rate and foul-trouble policy;
- injury type, expected return, workload restriction, and medical clearance;
- schedule density, travel, back-to-back, and rest directives;
- matchup assignments and opponent lineup archetypes;
- explicit ball-handler, backup point guard, rim-protection, and backup center
  eligibility;
- development priority and organizational minute floor;
- playoff series/context, game state, and overtime policy.

Missing inputs must remain `unknown` and generate review flags. V2 must never
fabricate them from player names, positions, or generic defaults.

## Layer contracts

### 1. Starter layer

Proposed function:

```r
build_v2_starter_state(roster, approved_locks, unavailable_ids,
                       scenario_id = NULL)
```

Output: one row per PG/SG/SF/PF/C with `player_id`, `position`, `lock_source`,
`is_available`, `legality_status`, and explanation codes. Approved locks are
authoritative. If a locked player is unavailable, return `REVIEW/BLOCKED` and a
replacement proposal; never silently discard the lock.

### 2. Rotation layer

Proposed function:

```r
build_v2_rotation(roster, starter_state, rotation_size,
                  role_requirements, availability, policy)
```

Output: exactly 10 or 11 selected players (with a separately versioned option
to retain legacy 9-player support), five starters, ordered bench, sixth man,
backup PG, backup C, inclusion/exclusion reasons, and unmet-coverage flags.

Deterministic requirements: availability exclusion, approved-starter inclusion,
exact size, no duplicate player, rookie eligibility, and minimum positional
coverage. Model-based choice may rank otherwise legal candidates using frozen
BIE outputs, role evidence, and depth evidence.

### 3. Minutes layer

Proposed function:

```r
allocate_v2_minutes(rotation, starter_state, policy,
                    manual_overrides = NULL, total = 240L)
```

Output: integer minutes summing to exactly 240, individual bounds, minute-share
reasons, override provenance, and coverage feasibility. It should reuse the
current bounded allocation mechanics behind a V2 adapter until V2 behavior is
fully characterized.

### 4. Lineup-combination layer

Proposed function:

```r
build_v2_lineup_portfolio(rotation, minutes, eligibility,
                          lineup_objectives, policy)
```

Output: legal five-man units with IDs, members, assigned positions, objective,
target minutes, score components, coverage, and review flags. The sum of group
exposure must reconcile with individual minutes. Each group contains five
unique players and passes the existing legality function.

### 5. Situation layer

Proposed function:

```r
apply_v2_rotation_situation(base_plan, situation, constraints, policy)
```

Output: a derived plan plus a structured delta from base. Supported situation
types should be introduced one at a time: injury/unavailable, foul trouble,
back-to-back/rest, development, matchup, playoff tightening, and closing.

## Override hierarchy

Highest priority wins:

1. hard legality and availability;
2. approved starter locks (or explicit conflict requiring human resolution);
3. manual front-office/coaching override with author and reason;
4. transaction/injury scenario state;
5. schedule and playoff policy;
6. matchup/foul/development situation policy;
7. deterministic role and position coverage;
8. model ranking and optimization tie-breaks;
9. stable database/player ordering.

Every override must record `source`, `reason_code`, `created_at`, input plan ID,
and affected player/lineup. No layer should mutate an upstream plan in place.

## State model

```text
rotation_plan
  plan_id, model_version, team_id, season, roster_signature, scenario_id
  starter_state
  rotation_members
  minute_allocations
  lineup_portfolio
  situation_overrides[]
  explanation_log[]
  validation_result
```

Injuries and transactions belong in the session scenario envelope, upstream of
the rotation plan. Situation overrides belong in the derived plan and reference
the scenario version. A changed roster/scenario creates a new plan; it must not
partially mutate an older cached result.

## Cache strategy

Use bounded, session-only caches keyed by:

```text
model version + team + season + roster signature + approved locks
+ availability signature + scenario signature + policy signature
+ rotation size + manual override signature
```

Cache each layer independently so a closing-lineup policy change does not
re-evaluate player BIE evidence. Never use cross-session persistent caches until
invalidation, privacy, and deployment behavior are explicitly approved.

## Explainability contract

Each selected player, minute assignment, lineup, and override needs:

- a stable reason code;
- plain-language explanation;
- evidence fields used and fields missing;
- hard constraints passed/failed;
- model score components without altering BIE internals;
- provenance for approved locks and manual changes;
- base-versus-situation delta.

## Required regression matrix

Run all 30 teams across normal and adverse fixtures. Required assertions:

- exactly five legal starters and all approved locks preserved;
- exact 10- or 11-player rotation as requested;
- exact 240 integer minutes;
- no duplicate player in a lineup;
- all lineup assignments legal;
- backup PG and backup C coverage, or explicit `REVIEW` when evidence is absent;
- legal closing lineup;
- unavailable players receive zero minutes;
- transaction, injury, and team-switch state remain consistent;
- deterministic results for identical inputs and stable tie ordering;
- base plan unchanged when deriving a situation plan;
- V1 outputs unchanged when V2 routing is disabled.

Characterization fixtures must include multi-position players, sparse evidence,
rookies at the preseason gate, locked unavailable starters, 10- and 11-man
rotations, development minutes, playoff tightening, and infeasible coverage.

## UI surfaces

Keep the current Depth Chart as the authoritative V1 surface. Add V2 behind an
explicit model-version feature flag:

- Rotation Plan: starters, 10/11-man membership, sixth man, backup PG/C;
- Minutes & Staggering: minute ledger and coverage timeline;
- Lineup Portfolio: base, bench bridge, offense, defense, and closing units;
- Situations: injury, foul, rest, development, matchup, playoff scenario;
- Explain: constraints, evidence, reasons, and deltas.

Inputs must stay mounted when switching views. Saving a V2 plan must not call
the existing starter/rotation save path until an explicit migration contract is
approved.

## Phased delivery and effort

| Phase | Deliverable | Estimated effort | Exit gate |
|---|---|---:|---|
| 1. Rotation core | Versioned starter adapter, exact 10/11-man selection, sixth man, backup PG/C, validation/explanations | 3-4 engineer-weeks | 30-team exact-size/lock/coverage suite; V1 unchanged |
| 2. Minutes and staggering | 240-minute allocation, bench bridges, starter staggering, substitution sequence prototype | 4-6 engineer-weeks | Exact minutes, feasible exposures, stable deterministic sequence |
| 3. Lineup combinations | Reconciled lineup portfolio and legal closing units | 4-6 engineer-weeks | Every lineup legal/unique; player/group minutes reconcile |
| 4. Situational logic | Injury, foul, rest, development, matchup, playoff derivations | 5-8 engineer-weeks | Override hierarchy and scenario consistency across all fixtures |
| 5. UI and QA | Feature-flagged coaching/front-office workspace, explainability, browser/performance QA | 3-5 engineer-weeks | Accessibility, responsive, state-preservation, performance, rollback |

Total: approximately 19-29 engineer-weeks, excluding collection/validation of
new medical, schedule, matchup, and substitution data.
