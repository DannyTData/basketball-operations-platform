# TBI V2/V3 Product Roadmap

## Prioritization principles

1. Complete rule/data foundations before widening transaction UI.
2. Keep frozen V1 BIE and approved lineup behavior available as a rollback.
3. Prefer explainable decision workflows over unsupported precision.
4. Version new basketball models and protect them with all-team fixtures.

## Top ten opportunities

| Priority | Feature | Front-office use case and user value | Current gap | Dependencies | Effort | Technical risk | Basketball/CBA risk | Release |
|---:|---|---|---|---|---:|---|---|---|
| 1 | V2 rotation core | Set an explainable 10/11-man rotation with sixth man and backup PG/C coverage; replaces manual reconciliation across depth/minutes/lineups | Current tools choose rotation/minutes and isolated lineups but do not produce one authoritative plan | Versioned starter contract, role/position eligibility, all-team fixtures | 3-4 weeks | Medium | High; must preserve locks, legality, rookie gate | Immediate next |
| 2 | Transaction rule foundation | Determine whether a two-team deal can advance before basketball evaluation; reduces false confidence and manual cross-checking | Salary/apron/draft screen omits multiple contract/timing/TPE/hard-cap rules | Approved CBA source pack, contract facts, rule ledgers, strict two-team reference | 10-16 weeks in increments | High | Very high | Immediate next |
| 3 | Injury and availability scenarios | See replacement starters, rotation/minute redistribution, payroll/roster effects, and decision risk for an absence | Only coarse availability exists; no durable injury scenario or expected return/workload state | V2 rotation layers, medical-data governance, scenario versioning | 5-8 weeks | High | High; avoid medical inference and silent lock overrides | Near-term V2 |
| 4 | Minutes and staggering planner | Build a 240-minute plan, stagger creators/bigs, and identify coverage gaps by game segment | Minute totals exist; no substitution sequence or simultaneous-on-court reconciliation | V2 rotation core, lineup portfolio, substitution data/constraints | 4-6 weeks | High | High | Near-term V2 |
| 5 | Lineup portfolio and closing intelligence | Compare base, bench bridge, offense, defense, and closing groups while reconciling player minutes | Four optimized lineups are isolated and not tied to a game plan | Rotation/minutes plan, position legality, reliable lineup/pair evidence | 4-6 weeks | High | High; sparse samples/reliability | Near-term V2 |
| 6 | Contract decision calendar and sequencing | Coordinate options, guarantees, extension windows, exceptions, draft decisions, and trade timing | Five-Year/Extension/Cap show facts but not one dependency-aware action calendar | Verified date/rule data, transaction rule foundation, notification model | 5-8 weeks | Medium | Very high for dates/eligibility | Later V2 |
| 7 | Multi-team trade architecture | Route players and draft rights independently across 3-4 teams with a result per team | State and UI are team-A/team-B and current legality coverage is incomplete | Completed transaction rule foundation, generalized graph, draft condition graph | 18-27 additional weeks | Very high | Very high | Later V2 |
| 8 | Draft scenario decision lab | Compare keep/trade/defer/swap outcomes, portfolio concentration, and roster/cap fit | Current simulation evaluates owned portfolio but not decision branches integrated with roster timelines | Verified pick graph, prospect/slot distributions, frozen RNG fixtures, cap/roster integration | 8-12 weeks | High | Medium; assumptions must be explicit | Later V2 |
| 9 | Roster-construction constraint solver | Find feasible roster moves that satisfy position/role, payroll, asset, age, and control goals | Current modules evaluate one proposal at a time | Complete rule engine, versioned roster objectives, explainable search, performance budget | 12-18 weeks | Very high | Very high; recommendation guardrails | V3 |
| 10 | Predictive decision simulator | Compare multi-year distributions for moves under performance, availability, cap, contract, and draft uncertainty | Current forecasts/simulations are narrow and mostly deterministic outside draft simulation | Calibrated outcomes, historical validation, scenario engine, model governance, uncertainty UI | 16-24+ weeks | Very high | Very high; avoid false precision | V3 |

## Delivery sequence

### Immediate next

- Freeze source packs and references for V2 rotation and the two-team rule
  engine.
- Build the versioned rotation core behind a feature flag.
- Close transaction-rule gaps in source-supported increments while proving
  existing two-team outputs unchanged until each explicit version cutover.
- Reconcile the release R/`renv` environment and stale test contracts.

### Near-term V2

- Injury/availability scenario envelope.
- Minute staggering and legal lineup portfolio.
- Closing-lineup explanations and base-versus-situation comparison.

### Later V2

- Contract/action sequencing calendar.
- Generalized transaction graph, then three-team, then four-team UI.
- Draft decision lab integrated with cap and roster timelines.

### V3

- Constraint-based roster-construction search.
- Calibrated predictive decision simulation with explicit uncertainty and model
  governance.

## Product gates

No V2/V3 feature should ship without:

- a versioned external contract and rollback path;
- explicit missing-data behavior;
- frozen representative and all-team characterization where relevant;
- database read/write classification;
- plain-language explanations and source provenance;
- accessibility, responsive, state-preservation, and performance budgets;
- confirmation that a basketball recommendation never overrides a CBA FAIL.
