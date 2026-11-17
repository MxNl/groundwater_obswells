# 12. Each well counted once, in the most selective scenario it meets

- Status: accepted (supersedes "first scenario of the list", 2026-11-04)
- Date: 2026-11-17
- Code: `R/scenarios.R` (`assign_scenario_groups()`), combined Sankey
  diagram

## Context

Scenarios overlap. To show how the usable wells split up, each well must be
counted once. The rule was "the first scenario in the list it meets"; with
the new order (FARM tolerant before FARM strict) FARM strict would get no
wells.

## Decision

- Each usable well goes to the scenario with the fewest selected wells among
  those it meets (ties: the earlier in the list), or to "none".

## Consequences

- A scenario within another keeps its own group, whatever the order of the
  list; the display order is free.
- The groups change when the numbers of selected wells change order.
