# 11. "Even coverage": a scenario fitted to cover Germany evenly

- Status: accepted
- Date: 2026-11-07
- Code: `R/scenarios.R` (`optimise_coverage_scenario()`); settings
  `coverage_settings` (`cell_km` 50, `keep_share` 0.8, `dtw_mean_min` 0,
  grids `dtw_mean_max` 1-15 m by 0.5, `dtw_max_extra` 1-8 m,
  `top_below_max` 1-5 m, `screen_length_max` 1-6 m, weights 1)

## Context

The fixed scenarios select almost no wells in the south, where the
groundwater is deeper. A network should cover the whole country while
staying as shallow and strict as possible.

## Decision

- Germany is divided into cells of `cell_km`; coverage is the number of
  cells with at least one selected well.
- All combinations of the four thresholds on their grids are evaluated
  (maximum depth = mean depth limit + `dtw_max_extra`; screen top below the
  water table; not falling dry).
- Reference: the coverage of the loosest combination. Of the combinations
  reaching at least `keep_share` of it, the strictest wins: each threshold
  scaled 0 (strict) to 1 (loose) on its grid, weighted and summed, the
  smallest sum.

## Consequences

- The thresholds follow the data and change when wells are added or the
  wells analysed change (e.g. the switch to FARM, ADR 1).
- Weights allow keeping one threshold stricter at the cost of the others.

## Alternatives considered

- Thresholds per region: more even, but not one scenario for all of
  Germany.
