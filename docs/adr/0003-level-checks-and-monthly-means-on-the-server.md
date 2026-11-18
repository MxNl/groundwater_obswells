# 3. Checks of single levels and monthly means on SQL Server

- Status: accepted; for the gw_base series (state series: ADR 18)
- Date: 2026-11-04 (filled ground elevations: 2026-11-17)
- Code: `R/wells.R` (`checked_measurements_cte()`, `gwl_monthly_sql()`),
  `R/db.R` (`db_query_wells()`); settings `gwl_valid_range` (-100 to 3,000 m
  a.s.l.), `dtw_valid_range` (-2 to 150 m below ground)

## Context

`tbl_gwl_raw` holds the levels of all wells (m a.s.l.), from manual
readings to dense logger series, with error codes such as -999 and some
implausible values. Reading the whole table into R is slow; the analysis
needs per well only the recent years.

## Decision

- Each measurement of the analysis wells is checked on the server: empty
  (code 1), level outside `gwl_valid_range` (2), depth below the (filled)
  ground elevation outside `dtw_valid_range` (3), else valid (0).
- Only valid measurements are averaged per well and month; the counts per
  check come back per well (`n_obs_empty`, `n_obs_out_of_range`,
  `n_obs_implausible_dtw`).
- The analysis wells and their ground elevations, which may come from FARM,
  are written to a temporary table `#well_elev` on the connection of the
  query and dropped afterwards; only their levels are read.

## Consequences

- One scan of the level table per query; only monthly means of the window
  come back.
- Monthly means keep dense logger periods from outweighing monthly manual
  readings.
- Wells without a ground elevation get no depth check on the server (the
  comparison is empty); they drop out later for lack of a depth.
- The temporary table needs no rights beyond reading (tempdb); IDs are
  compared in the database collation.

## Alternatives considered

- Reading all levels into R or DuckDB: simpler queries, but slow and large.
- Error codes as a fixed list: the range check catches all codes outside
  plausible levels; a list of candidate codes is only checked in a figure
  (ADR 15).
