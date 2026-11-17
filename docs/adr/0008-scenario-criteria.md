# 8. Scenarios: five criteria applied one after the other

- Status: accepted
- Date: 2026-11-04 (screen bottom and minimum length: 2026-11-17)
- Code: `R/scenarios.R` (`scenario_checks()`, `evaluate_scenarios()`,
  `count_scenario_steps()`); settings `scenarios_fixed`

## Context

The wells are to be selected for monitoring shallow groundwater, where
substances from the surface arrive with little delay. Different guidelines
define this differently; the selection should be traceable step by step.

## Decision

Each scenario filters the usable wells by five criteria, in m below ground,
in this order:

1. mean depth between `dtw_mean_min` and `dtw_mean_max`;
2. maximum depth (`dtw_deepest`) below `dtw_max`;
3. screen position relative to the mean water table: the top by
   `screen_rule` (`below`: 0 to `top_below_max` m below it; `spans`: above
   it; `none`: no rule) and, where given, the bottom `bot_below_min` to
   `bot_below_max` m below it;
4. not falling dry (ADR 9);
5. screen length above 0, from `screen_length_min` to `screen_length_max`.

Empty limits (NA) mean no limit.

## Consequences

- The wells left after each criterion are counted per scenario, so the
  figures show where wells drop out.
- The order matters only for these counts, not for the selected wells.
- The screen position refers to the mean water table, not to its seasonal
  range.
