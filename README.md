# groundwater_obswells

Analyses how suitable groundwater observation wells are, using their metadata
and time series from a Microsoft SQL Server database.

## Setup

1. Install the R packages and the ODBC driver
   ([ODBC Driver 18 for SQL Server](https://learn.microsoft.com/sql/connect/odbc/download-odbc-driver-for-sql-server)):

   ```r
   install.packages(c(
     "targets", "odbc", "pool", "duckdb", "DBI", "dplyr", "tibble", "tidyr", "keyring",
     "sf", "ggplot2", "ggridges", "patchwork", "marquee", "scales", "ragg",
     "systemfonts", "rnaturalearthdata", "withr", "readxl"
   ))
   ```

2. Create `.Renviron` in the project folder with the connection details.
   `.Renviron` is git-ignored, so never commit these anywhere else.

   ```
   DB_SERVER=<server>
   DB_PORT=<port>
   DB_NAME=<database name>
   DB_USER=<user>
   # optional
   # DB_DRIVER=ODBC Driver 18 for SQL Server
   # DB_TRUST_SERVER_CERTIFICATE=yes
   # DB_SCHEMAS=dbo
   ```

   `DB_PORT` can be left out for the default port 1433. Set
   `DB_TRUST_SERVER_CERTIFICATE=yes` if the connection fails with a
   certificate error (common for servers inside a network). `DB_SCHEMAS`
   limits the catalogue to these schemas (comma-separated).
3. Restart R in the project folder, so it reads `.Renviron`.
4. Store the password once in the system keyring (Windows Credential Manager,
   macOS Keychain, Secret Service on Linux); this prompts for it:

   ```r
   targets::tar_source("R")
   set_db_password()
   ```

   `DB_PASSWORD` in `.Renviron` works as well, the keyring is safer.

## Run

```r
targets::tar_make()
targets::tar_read(db_table_overview)   # one row per table
targets::tar_read(db_columns)          # one row per column

targets::tar_read(well_availability)   # how far the data go, in numbers
targets::tar_read(gwl_checks)          # measurements removed by the checks
targets::tar_read(well_dtw)            # one row per well
targets::tar_read(scenario_steps)      # wells per scenario after each criterion
```

The figures are written to `plots/`: `flowchart.png`,
`sankey_wells.png` (where the wells drop out on the way to the usable
wells), `sankey_scenarios.png` (the same for the criteria of each scenario),
`sankey_combined.png` (preprocessing and scenarios in one diagram; each
usable well is counted once, in the first scenario of the list it meets),
`missing_screen_depths.png` (where screen depths are missing, per state),
`scenario_optimisation.png` (the cells covered by the "Even coverage"
scenario and how its thresholds were fitted),
`method_flowchart.png` (all checking and filtering steps in short words,
with the thresholds from the settings),
`state_attrition.png` (per state: wells lost at each step, usable wells and
wells per scenario) and `diagnostics/wells_NN.png` (diagnostic sheets of a
random sample of wells: monthly depths, removed spikes, mean window, mean,
annual low and maximum depth, screen, and the result of each scenario;
sample size and seed in `diagnostic_settings`).

`plots/thresholds/` helps to choose the preprocessing settings from the
data (ranges in `threshold_settings`):

- `recent_window.png`: usable wells for each recent window (3-20 years) and
  minimum number of years with data per season, and how many wells have
  data within the last N years
- `mean_window.png`: how much the mean level changes with the number of most
  recent years behind it, and how many wells have that many years
- `spikes.png`: share of monthly means removed and wells affected for each
  factor of the MAD and minimum deviation, with and without the neighbour
  condition
- `valid_ranges.png`: the implied depth of all measurements with the
  plausibility limits, how often common error codes occur (`candidate_codes`;
  grey ones lie inside the valid range and are kept as levels), and the
  removed values found in the most wells
and one `scenario_<id>.png` per scenario.

## Depth to groundwater

`well_dtw` has one row per well, from `tbl_meta_well_import` and
`tbl_gwl_raw`. Table and column names, thresholds and criteria are set in
`well_config` in `_targets.R`.

How the mean depth is calculated:

1. **Checks of single measurements** (on the server): empty values, levels
   outside `gwl_valid_range` (error codes such as -999) and depths below
   ground outside `dtw_valid_range` are left out and counted per well
   (`n_obs_empty`, `n_obs_out_of_range`, `n_obs_implausible_dtw`).
2. **Monthly means** of the valid measurements, so dense logger periods do
   not outweigh monthly manual readings.
3. **Recent window**: the last `recent_years` (10) years up to the current
   month (`reference_date`, NULL: today).
4. **Spikes**: monthly means further than `max(spike_mad * MAD,
   spike_min_dev)` from the well's median in the recent window are removed
   (`n_months_spike`). With `spike_neighbours` (default) only if they lie
   that far from both neighbouring months too: single outliers are removed,
   real events lasting some months (wet winters, droughts) stay.
5. **Mean window**: the `window_years` (5) most recent calendar years with
   data in the recent window, not necessarily consecutive, so the mean
   focuses on the latest data. `gwl_mean` is the mean of their monthly means;
   `gwl_mean_seasonal` averages each calendar month first and then the 12
   months, so a season with more gaps does not bias it (only when all 12
   calendar months have data).
6. **Depth below ground**: `dtw_mean = elev_gs - gwl_mean` (and
   `dtw_mean_seasonal`); `dtw_shallowest` and `dtw_deepest` from the highest
   and lowest monthly mean. Only wells with `elev_gs` get a depth.

How reliable the mean is: `min_years_per_season` (in how many years of the
recent window the season with the fewest years has data), `seasons`,
`coverage_recent` (share of the recent window's months with data),
`max_gap_months`, and `first_year_window`/`last_year_window`/
`n_years_window` (the years behind the mean).

`usable` is TRUE for wells with a depth, screen depths, valid data in the
recent window and each season (DJF, MAM, JJA, SON) with data in at least
`min_years_per_season` (5) of its years.
`water_table_vs_screen` tells whether the mean water table is above, within
or below the screen, and `flags` lists the problems found.

`well_availability` counts the wells left after each criterion, `gwl_checks`
the measurements removed by the checks.

Steps 1-2 run in SQL Server (one scan of the table), steps 3-5 in an
in-memory DuckDB.

## Scenarios

The scenarios (`scenarios_fixed` and the optimised one in `_targets.R`)
select usable wells with shallow groundwater, where substances from the
surface show up with a short delay. Each applies five criteria, in m below
ground surface, in this order:

1. mean depth to groundwater between `dtw_mean_min` and `dtw_mean_max`
2. maximum depth to groundwater (deepest monthly mean of the mean window)
   less than `dtw_max`
3. screen top position (`screen_rule`): `below` = 0 to `top_below_max` m
   below the mean water table; `spans` = above it, so the screen reaches
   across the water table
4. not falling dry: the screen bottom is deeper than the typical annual low
   (`dtw_annual_low` = ground surface minus the lowest monthly mean level of
   each year of the mean window, averaged over the years)
5. screen length up to `screen_length_max`

| Scenario | Mean depth | Max depth | Screen top | Length |
|---|---|---|---|---|
| FARM strict | 1-5 m | < 10 m | 0-2 m below the water table | <= 2 m |
| FARM tolerant | 1-5 m | < 10 m | 0-3 m below the water table | <= 4 m |
| Very shallow | 0.5-3 m | < 5 m | 0-1 m below the water table | <= 2 m |
| Screen at the water table | 1-5 m | < 10 m | above the water table | <= 3 m |
| Even coverage | fitted | fitted | fitted | fitted |

In all scenarios the screen bottom must be deeper than the annual low.

"Even coverage" is fitted to the data (`coverage_settings`) so its wells
are spread across all of Germany, also the south with its deeper
groundwater, while staying as shallow and strict as possible. Germany is
divided into cells of `cell_km` (50 km); of all threshold combinations on the
grids (mean depth limit 1-15 m, maximum depth = limit + 1-8 m, screen top
1-5 m below the water table, screen length 1-6 m) whose wells cover at least
`keep_share` (80 %) of the cells the loosest combination covers, the
strictest one wins: each threshold is scaled from 0 (strictest) to 1
(loosest) on its grid and the weighted sum (`weights`, default 1 each) is
the smallest. A higher `keep_share` covers more cells, mostly in the south,
with looser thresholds. `scenario_optimisation.png` maps the covered cells
and shows for each threshold how many cells a stricter or looser value
would cover.

Each scenario figure shows the selected wells on a map of Germany, their
number with the wells left after each criterion, and the distribution of
the four properties by depth, depth downwards (usable wells in grey,
selected wells in colour). The
flow chart shows the steps from the metadata to the usable wells and, per
scenario, its four criteria with the wells left after each. The Sankey
diagrams show the same as flows: the band is the number of wells left, the
branches the wells dropped at each step. Flow chart and Sankey diagrams are
drawn with plain ggplot2 (no extension package). The figures are sized for
normal screens (11 inches wide). Font (Noto Sans Display, downloaded from Google Fonts if
missing), colours and captions follow the agriculture_crops plots; the
settings are in `plot_config`.

Coordinates: the columns are found by name (x/y, rw/hw, lon/lat, ...) and
their coordinate system is guessed from the value ranges per well (WGS84,
UTM 32/33 with zone prefix, Gauss-Krüger, plain UTM taken as zone 32). Set
`x`, `y` and `crs` in `well_config` if this goes wrong; the guess is
reported when the pipeline runs.

Borders: the Natural Earth state borders are downloaded once into
`cache/naturalearth/`. Where the download is blocked (proxy), put
`ne_10m_admin_1_states_provinces.zip` from naturalearthdata.com there by
hand, or copy the unpacked shapefile (`ne_10m_admin_1_states_provinces.*`)
from another project's cache; until then the maps show the country outline
from rnaturalearthdata.

## When the data are read again

The database changes outside the pipeline, so on every run it reads the
catalogue (row counts of the tables) and checksums of the metadata columns
used. The queries rerun when:

- rows are added or deleted (row counts),
- values in the metadata change (checksum over well ID, ground elevation,
  screen depths and coordinates): the metadata query reruns; if a ground
  elevation changed, the level queries too, as they check the depth below
  ground,
- the settings they use change (`query_spec`, `threshold_query_settings`),
- a new month starts, as the recent window moves on: the target
  `reference_month` is checked on every run and changes only then (fix the
  window with `reference_date` in `well_config`).

Corrected levels in `tbl_gwl_raw` with the same number of rows are read at
the next new month at the latest; to read them at once, run
`targets::tar_invalidate(starts_with("gwl_"))` before `tar_make()`.

## How it works

`R/db.R` connects through ODBC with a connection pool (pool package), so all
targets of a run share their connections. The catalogue queries (`R/catalog.R`) read
only SQL Server's catalogue views, so no table is scanned. The groundwater
levels (`R/wells.R`) are checked and averaged per month on the server, so
only the monthly means of the window come back; DuckDB does the rest. Results come back
as tibbles for dplyr.

## Source check (temporary)

Compares the well metadata of gw_base with three other sources and these
among each other, kept apart from the rest (targets `check_*`,
`R/source_check.R`, settings in `check_settings`), to be removed when done.
Needs the package readxl.

- Inputs in `cache/`: `check_farm/stammdaten_geo.csv` (FARM),
  `check_meer/FARM_*_hoehe_abweichung.*` (meer, with a DEM elevation) and
  `check_original-laender/*Stammdaten*` (original data of the states).
  Separator, decimal comma and encoding (UTF-8 or Windows-1252) are
  detected; all values are read as text first, so IDs keep their leading
  zeros.
- Columns: `check_settings$mapping` names, per source, the ID column and the
  columns of ground elevation, measuring point, screen top and bottom (a
  field left out is not compared; entries such as `laender_ST` apply to one
  state's file). Case, umlauts, spaces and signs in the names do not matter.
  `output/source_check/column_mapping.csv` shows which column was found.
- IDs: the wells of all sources are linked by state and ID. A state prefix
  (`HE_`, `HE-`), spaces, punctuation and leading zeros are removed first,
  so `HE_00001000`, `1000` and `HE-1000` (in Hessen) are the same well. The
  state comes from the file name, a state column or the ID prefix. As a
  check, every gw_base column with text or whole numbers is also tried
  (`id_matching.csv`, `plots/source_check/id_matching.png`, with the
  distances between the coordinates of linked wells). If the mapped ID
  links less than half of a file's wells and another column clearly more,
  that column is used (`id_auto`, noted in `id_pairs.csv`); a column can
  also be set per file in `id_pairs`. For the distances, EPSG codes that
  PROJ does not know (such as 0) count as missing, and the CRS is guessed
  from the coordinates; `id_pairs.csv` counts these wells and lists the
  codes.
- Do the IDs match? `plots/source_check/id_overlap.png` shows per pair of
  sources and state the wells in both, only in the left and only in the
  right source (`id_overlap.csv`).
- The wells of the meer files: target `check_meer` (`tar_read(check_meer)`,
  also `meer_wells.csv`) has one row per meer well with the IDs, the values
  of gw_base, meer, FARM, the states and the meer DEM side by side
  (`elev_gs_farm`, ...), the differences to gw_base
  (`elev_gs_farm_minus_gw_base`, ...) and the distances of the coordinates
  to gw_base. `meer_differences.png` shows per field, source and state how
  many meer wells differ from gw_base by how much, `meer_map.png` where
  (ground elevation), `meer_large_differences.csv` the differences above
  `report_threshold_m`. `values.csv` has the values of all wells of all
  sources side by side.

## Planned

- Ground surface elevation: where `elev_gs` is missing, estimate it from
  `elev_mp` minus the median stick-up, flagged as estimated (about 1,100
  wells in NW lack `elev_gs`).
- Estimated screens from the total well depth, flagged as estimated, if
  the metadata have such a column (screen depths are missing for most wells
  in BY, BW and ST).
