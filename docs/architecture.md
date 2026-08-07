# Basketball Operations Platform Architecture

## Product Mission

Help basketball organizations make faster, clearer, and better-informed
roster, financial, and strategic decisions.

## Audience Layers

### Executive View
Answers the most important questions in approximately 30 seconds.

### Basketball Operations View
Shows scenarios, tradeoffs, risks, and available options.

### Technical View
Shows calculations, assumptions, CBA logic, and source details.

## Core Architecture

Selected Team
    ↓
Roster
Contracts
Payroll
Draft Capital
Trade Assets
Projections
Scenarios

The application must never hard-code one demonstration team into its
calculation or interface logic.

## Page Standard

Every major page should explain:

1. What is happening?
2. Why does it matter?
3. What is the organizational impact?
4. What options are available?
5. How was the result calculated?

## Development Standard

All CBA and financial calculations should be separated from interface code
and covered by automated tests.
