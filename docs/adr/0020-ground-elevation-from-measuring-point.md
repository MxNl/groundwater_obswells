# 20. Missing ground elevations estimated from the measuring point

- Status: accepted (extends ADR 1)
- Date: 2026-11-18
- Code: `R/farm.R` (`estimate_elev_from_mp()`, `farm_stickup()`); settings
  `farm_config$stickup_min_pairs` (10)

## Context

Without a ground elevation there is no depth to groundwater (ADR 7). Many
FARM wells lack it in FARM and in gw_base, but FARM gives the height of the
measuring point (top of the casing), which lies a fairly constant distance
(stick-up) above ground within a state.

## Decision

- Where the ground elevation is still missing after the filling from gw_base,
  it is the FARM measuring point height minus the median stick-up of the
  state.
- The stick-up per state is the median of measuring point minus ground
  elevation of the state's FARM wells with both values in FARM; the median of
  Germany where a state has fewer than `stickup_min_pairs` such wells
  (`output/farm_stickup.csv`).
- The value is marked "estimated from measuring point"
  (`elev_gs_from`, `elev_gs_stickup`), with a colour of its own in the
  filling figure.

## Consequences

- More wells get a depth. Each estimated depth is off by the difference
  between the well's real stick-up and the state median (typically some
  decimetres, more for wells with buried or tall heads).

## Alternatives considered

- The ground elevation of the state deliveries (in the script's output):
  another source, not used yet.
- A DEM: the meer DEM disagreed by tens of metres with all sources (ADR 16).
