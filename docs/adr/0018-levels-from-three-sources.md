# 18. Groundwater levels from three sources by priority

- Status: accepted (supersedes the levels part of ADR 1); the choice
  amended by ADR 23
- Date: 2026-11-18
- Code: `R/levels.R` (`assign_level_sources()`, `link_state_levels()`,
  `state_monthly()`, `combine_monthly()`, `combine_year_seasons()`,
  `fetch_gwbase_level_counts()`); settings `level_config`
  (`neighbour_radius_m` 10)

## Context

With levels from gw_base only, a large share of the FARM wells dropped out
for lack of levels: their IDs could not be linked, or the linked gw_base well
has no series. The states delivered level series for many FARM wells
directly, and some wells have a gw_base well with levels right next to them.

## Decision

Each FARM well takes its level series from the first source with data (at
least one level inside `gwl_valid_range`):

1. the state level series (state deliveries and MENTOR, ADR 21), linked by
   state and ID with the mapping (ADR 17);
2. the linked gw_base well;
3. the nearest gw_base well with levels within `neighbour_radius_m` (10 m)
   of the well's coordinates.

- The chosen series counts as the well's own: the depths use the well's own
  ground elevation, and the monthly means are stored under its `well_id`.
- The state series get the same checks as the gw_base series on the server
  (empty, valid level range, valid depth; ADR 3), computed in R. Where only a
  depth was delivered, the level is ground elevation minus the depth (below
  ground) or the FARM measuring point minus the depth (below the measuring
  point, RP and the BW Abstich).
- The gw_base server queries read only the gw_base wells chosen in 2 and 3,
  with the ground elevation of the FARM well they serve.

## Consequences

- Wells drop out at "no valid groundwater levels" only if no source has
  data; the filling figure shows per state which source gives the levels,
  and `well_dtw$level_source` per well.
- A gw_base well within 10 m stands for the FARM well; flag "levels of a
  nearby gw_base well". If two FARM wells share one gw_base well with
  different ground elevations, the server check of implausible depths uses
  the first.
- The threshold figures of error codes and implied depths still show the
  gw_base series only; the season figures include the state series.

## Alternatives considered

- gw_base first, state series as a fallback: gw_base was the weaker source
  for many FARM wells (missing links and series).
- Combining all sources per well: overlapping series from different
  deliveries could differ in reference and processing.
