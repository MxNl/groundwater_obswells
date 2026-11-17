# 10. Fixed scenarios from guidelines and expert choices

- Status: accepted
- Date: 2026-11-04 (National and EU: 2026-11-17)
- Code: `_targets.R` (`scenarios_fixed`)

## Context

Several definitions of a suitable well are in use. They are compared side
by side rather than choosing one.

## Decision

| Scenario | Mean depth | Max. depth | Screen top | Screen bottom | Length |
|---|---|---|---|---|---|
| National (2020) | 1-5 m | < 10 m | 0-3 m below the water table | <= 4 m below | 1-2 m |
| FARM tolerant | 1-5 m | < 10 m | 0-3 m below the water table | - | <= 4 m |
| EU (EFSA 2023) | 1-5 m | < 10 m | - | 1-2 m below | 1-2 m |
| FARM strict | 1-5 m | < 10 m | 0-2 m below the water table | - | <= 2 m |
| Very shallow | 0.5-3 m | < 5 m | 0-1 m below the water table | - | <= 2 m |
| Screen at the water table | 1-5 m | < 10 m | above the water table | - | <= 3 m |

All also require "not falling dry" (ADR 9). The order of the table is the
order in the plots.

- National (2020), "screen top 0-2 m, at most 3 m": 3 m is the limit; with 2
  m the bottom limit of 4 m would always hold.
- EU (EFSA 2023) has no rule for the top; bottom and length place it between
  1 m above and 1 m below the water table.
- "Depth 1 to 5 m or maximum 10 m" is read as mean 1-5 m and maximum below
  10 m.

## Consequences

- FARM strict lies within FARM tolerant; other scenarios overlap partly. For
  counts without double counting, each well is assigned to the most
  selective scenario it meets (ADR 12).
