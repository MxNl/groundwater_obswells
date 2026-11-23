# 25. National (2020): screens up to 4 m long

- Status: accepted (amends ADR 10)
- Date: 2026-11-23
- Code: `_targets.R` (`scenarios_fixed`, row `national_2020`:
  `screen_length_max` 4)

## Context

The National (2020) scenario was set up with screens 1-2 m long. The
criteria allow screens up to 4 m.

## Decision

- National (2020): screen length 1-4 m (was 1-2 m). The other thresholds
  stay: mean depth 1-5 m, maximum depth < 10 m, screen top 0-3 m and screen
  bottom at most 4 m below the mean water table.

## Consequences

- More wells meet the scenario. The bottom limit now matters more: screen
  top depth below the water table plus screen length must stay within 4 m,
  so a 4 m screen must start right at the water table.
- The reasoning in ADR 10 for reading "at most 3 m" as the limit of the
  screen top (with 2 m screens the bottom limit would always hold) no
  longer applies; the reading itself is unchanged.
