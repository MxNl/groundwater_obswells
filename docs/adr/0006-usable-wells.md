# 6. Usable wells: complete metadata and data in every season

- Status: accepted
- Date: 2026-11-04
- Code: `R/wells.R` (`assess_well_dtw()`, `gwl_window_stats_sql()`);
  settings `min_years_per_season` (5)

## Context

A mean depth is only meaningful if the data cover the annual cycle; wells
measured only in summer would look deeper than they are. The scenarios also
need the screen depths.

## Decision

- A well is usable if it has a mean depth (ground elevation and valid levels),
  a screen top and bottom, data in the recent window, and each season (DJF,
  MAM, JJA, SON; December in its own calendar year) has data in at least
  `min_years_per_season` years of the recent window.
- The steps are counted in this order: ground elevation, valid levels, screen
  depths, recent data, seasons (flow chart, Sankey diagram, drop-out per
  state).

## Consequences

- Wells with a seasonal bias or short series drop out.
- Missing screen depths are the main loss in the south (BY, BW) in gw_base.

## Alternatives considered

- A minimum number of measurements: does not detect a seasonal bias.
- A coverage share of the months: penalises monthly manual readings with
  single gaps more than needed.
