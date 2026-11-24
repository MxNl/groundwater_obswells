# 1. FARM master data as the wells analysed, filled from gw_base

- Status: accepted; the levels part superseded by ADR 18, ground elevation
  extended by ADR 20; NI records replaced by the NI master data (ADR 27)
- Date: 2026-11-17
- Code: `R/farm.R` (`read_farm_meta()`, `fill_farm_meta()`,
  `plot_farm_filling()`); settings `farm_config` (file
  `cache/check_farm/stammdaten_geo.csv`, column names), `well_config`
  (gw_base metadata table `tbl_meta_well_import`)

## Context

The wells were first taken from the gw_base metadata
(`tbl_meta_well_import`). The FARM master data hold the wells of the FARM
monitoring network with their own ground elevations, screen depths and
coordinates; the comparison of the sources (ADR 16) showed gaps in both. The
analysis is meant for the FARM wells, while the groundwater levels exist only
in gw_base (`tbl_gwl_raw`).

## Decision

- The wells analysed are the records of the FARM master data.
- Where FARM has no ground elevation, screen top, screen bottom or
  coordinates, the value of the same well in gw_base is used. The wells are
  linked by state and ID (ADR 2).
- Each value keeps its origin (`elev_gs_from`, `screen_top_from`,
  `screen_bot_from`, `coords_from`: FARM, gw_base, missing in both, missing
  and the well not found in gw_base).
- FARM coordinates are transformed to EPSG 25832; EPSG codes PROJ does not
  know (such as 0) are guessed from the coordinate ranges.
- The levels are read from gw_base under the gw_base ID of the linked well;
  FARM wells not found in gw_base have no levels.

## Consequences

- `well_meta` has one row per FARM record; `plots/farm_filling.png` and
  `output/farm_filling.csv` show per field and state how many values come
  from FARM, were filled or stay missing.
- Wells whose ID cannot be linked drop out at "no ground elevation" or "no
  valid levels". In several states (NI, MV, BY, ST, partly HE) FARM and
  gw_base use different ID systems, so most of their wells are not linked
  yet (ADR 2).
- FARM values take precedence also where gw_base differs; the differences
  are not resolved, only shown by the source check (ADR 16).

## Alternatives considered

- gw_base as the base, filled from FARM: covers more wells, but not the
  network the analysis is for.
- Only complete FARM records: loses the values gw_base can add (in the
  first run about 1,950 ground elevations and 1,590 coordinates).
