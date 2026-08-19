# Thompson's Basketball Intelligence — Codex Instructions

## Product

This repository contains Thompson's Basketball Intelligence (TBI), an NBA
basketball-operations decision-support platform built primarily in R/Shiny.

Treat the current V1/release behavior as a protected production baseline.

## Required Reading

Before making architectural or V2 basketball changes, read:

- docs/architecture.md
- docs/v2-v3-product-roadmap.md
- docs/v2-rotation-engine-roadmap.md
- docs/trade-cba-multiteam-readiness.md
- docs/release-hardening-2026-08-18.md
- relevant existing tests

## Protected V1 Basketball Behavior

Do not modify existing V1 behavior unless explicitly approved as a versioned
replacement.

Protected behavior includes:

- frozen BIE formulas, weights, grades, and reliability behavior
- 65/25/10 starting-five optimizer
- approved/manual starter locks
- lineup legality rules
- candidate ordering behavior
- preseason rookie eligibility gate
- database source-of-truth behavior
- existing V1 transaction and scenario behavior
- existing production CBA transaction rules

New V2 basketball behavior must be versioned and must not silently replace V1.

## Starter Rules

Approved starter locks are authoritative.

If a locked starter conflicts with availability or legality:

- do not silently remove the lock
- return a REVIEW/BLOCKED state
- explain the conflict
- provide a replacement proposal separately

User-designated rookie starters must not be benched merely because NBA
performance data is unavailable.

## Data Integrity

Never fabricate:

- player performance
- role eligibility
- injury information
- return dates
- medical clearance
- workload restrictions
- matchup evidence
- contract facts
- transaction dates
- CBA rules
- draft obligations
- lineup evidence

Unknown information must remain unknown.

When missing information affects a decision, return an explicit REVIEW or
missing-data flag.

## CBA Safety

Basketball recommendations never override a CBA FAIL.

Transaction evaluation should use explicit:

- PASS
- FAIL
- REVIEW

states with plain-language explanation and source provenance.

Do not invent legal interpretations to make a scenario pass.

## V2 Architecture Direction

V2 basketball architecture should follow:

Starter State
-> Rotation
-> Minutes
-> Lineup Portfolio
-> Situations
-> Organizational Impact

New state should be versioned and scenario-aware.

Prefer adapters and new contracts around stable V1 engines before refactoring
working V1 internals.

## Database and State

Application database reads remain source-of-truth.

Do not modify production database contents unless explicitly instructed.

Scenario and exploratory planning state should remain session-scoped unless a
new persistence contract is explicitly approved.

Do not introduce cross-session persistent caches without explicit approval.

## Shiny and UI Protection

Preserve existing Shiny input/output IDs unless a migration is explicitly
approved.

Do not break:

- top navigation
- CBA deep links
- internal sub-tabs
- responsive/mobile behavior
- selected team and season state
- existing scenario state

Inputs required for stateful behavior should remain mounted when switching
views.

## Testing Standard

For meaningful production changes:

1. Parse changed R files.
2. Run devtools::load_all().
3. Run relevant focused tests.
4. Run characterization and regression tests for protected behavior.
5. Run git diff --check.
6. Report failures accurately instead of weakening tests merely to get green.
7. Confirm V1 behavior remains unchanged when V2 routing is disabled.

For V2 basketball work, use representative fixtures and all-30-team validation
where applicable.

## Refactoring

Inspect before editing.

Add characterization coverage before risky refactors.

Avoid broad cleanup while implementing a focused feature.

Do not modify frozen logic simply because another implementation appears
cleaner.

Prefer small, reviewable, modular changes.

## Git Safety

Do not commit unless explicitly instructed.

Do not push unless explicitly instructed.

Do not merge branches unless explicitly instructed.

Before proposing a commit, report:

- files changed
- purpose of each change
- tests run
- test results
- known limitations
- git diff --check result

## V2 Execution Policy

Do not implement the entire V2 roadmap at once.

Work phase-by-phase.

For Phase 0 or audit tasks:

- inspect only
- produce implementation plans and matrices
- do not modify production code
- do not modify tests
- do not modify database contents
- do not commit
- do not push
- stop for approval before implementation

For implementation phases, stop at the requested phase boundary and return a
completion report before continuing.
