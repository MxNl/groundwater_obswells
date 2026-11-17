# 7. Depth to groundwater: mean, maximum and annual low

- Status: accepted
- Date: 2026-11-04
- Code: `R/wells.R` (`gwl_window_stats_sql()`, `assess_well_dtw()`)

## Context

The levels are elevations (m a.s.l.), the screen depths are depths below
ground. The scenarios compare both.

## Decision

- Depth below ground = ground elevation - level, for the mean
  (`dtw_mean`), the seasonal mean, the shallowest and the deepest monthly
  mean in the mean window (`dtw_deepest`, the "maximum depth") and the annual
  low (`dtw_annual_low`).
- Annual low: the lowest monthly mean of each year of the mean window,
  averaged over the years with at least 6 months of data (all years if none
  has 6).
- The ground elevation is required: without it there is no depth.

## Consequences

- An error in the ground elevation moves all depths of the well by the same
  amount; the elevation must be on the same height datum as the levels.
- The maximum depth is the deepest monthly mean, not the deepest single
  reading.

## Alternatives considered

- Levels as depth below the measuring point: would need only the stick-up,
  but the levels are stored as elevations.
