# Update Verification

This build was rebuilt from the uploaded `Corrected_Roster_Depth_Update` master.

## Implemented
- Conference rank fallback is computed from win percentage, wins, point differential, and team name when the standings feed has blank ranks.
- Depth Chart includes persistent player edits for eligible position, depth order, and starter status.
- Reset removes a player's manual override and restores generated logic.
- Qualifying-offer, restricted-free-agent, and two-way labels are visible on planning depth-chart cards.
- Official 2025-26 and 2026-27 cap, tax, first-apron, second-apron, and minimum-payroll thresholds are loaded in SQLite.
- Salary Cap Intelligence shows payroll relative to each threshold and each player's salary as a percentage of cap and apron levels.

## Runtime note
R is not available in the build environment, so the final Shiny launch must be tested in RStudio.
