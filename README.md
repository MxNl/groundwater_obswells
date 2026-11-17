# groundwater_obswells

Analyses how suitable groundwater observation wells are. The wells are those
of the FARM master data (`cache/check_farm/stammdaten_geo.csv`), with gaps
filled from gw_base; the groundwater levels come from gw_base, a Microsoft SQL
Server database.

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

targets::tar_read(well_meta)           # FARM wells, filled from gw_base
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

## Wells: FARM master data, filled from gw_base

The wells analysed are the records of the FARM master data
(`farm_config$file`; column names in `farm_config$columns`, case, umlauts,
spaces and signs do not matter). Each FARM well is linked to gw_base
(`tbl_meta_well_import`) by state and ID: the state from the state column
(codes or names) or an ID prefix, the IDs with a state prefix such as `HE_`,
all signs other than letters and digits and leading zeros removed
(`R/link.R`). Where FARM has no ground elevation, screen top or bottom or
coordinates, the value of the linked gw_base well is used.

`well_meta` has one row per FARM record: `well_id` (the gw_base ID, under
which the levels are read; `<state>_<FARM ID>` for wells not found in
gw_base), `farm_id`, `state`, `in_gw_base`, the values used (coordinates in
EPSG 25832; FARM codes PROJ does not know, such as 0, are guessed from the
coordinates) and per field where the value comes from (`elev_gs_from`,
`screen_top_from`, `screen_bot_from`, `coords_from`: FARM, gw_base, missing
in both, or missing and the well not found in gw_base).
`plots/farm_filling.png` and `output/farm_filling.csv` show per field and
state how many values come from FARM, were filled or stay missing;
`output/well_meta_filled.csv` holds the table.

The groundwater levels come from gw_base (`tbl_gwl_raw`), so FARM wells not
found there have none (flag "not in gw_base (no levels)"). Only the levels
of the linked wells are read.

## Depth to groundwater

`well_dtw` has one row per FARM well, from `well_meta` and `tbl_gwl_raw`.
Table and column names, thresholds and criteria are set in `well_config` in
`_targets.R`.

How the mean depth is calculated:

1. **Checks of single measurements** (on the server): empty values, levels
   outside `gwl_valid_range` (error codes such as -999) and depths below
   ground outside `dtw_valid_range` (with the filled ground elevation) are
   left out and counted per well
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
3. screen position: the top by `screen_rule` (`below` = 0 to
   `top_below_max` m below the mean water table; `spans` = above it, so the
   screen reaches across the water table; `none` = no rule for the top),
   and where given the bottom `bot_below_min` to `bot_below_max` m below the
   mean water table
4. not falling dry: the screen bottom is deeper than the typical annual low
   (`dtw_annual_low` = ground surface minus the lowest monthly mean level of
   each year of the mean window, averaged over the years)
5. screen length from `screen_length_min` (where given) up to
   `screen_length_max`

Empty values (NA) mean no limit.

| Scenario | Mean depth | Max depth | Screen top | Screen bottom | Length |
|---|---|---|---|---|---|
| FARM strict | 1-5 m | < 10 m | 0-2 m below the water table | - | <= 2 m |
| FARM tolerant | 1-5 m | < 10 m | 0-3 m below the water table | - | <= 4 m |
| Very shallow | 0.5-3 m | < 5 m | 0-1 m below the water table | - | <= 2 m |
| Screen at the water table | 1-5 m | < 10 m | above the water table | - | <= 3 m |
| National (2020) | 1-5 m | < 10 m | 0-3 m below the water table | <= 4 m below the water table | 1-2 m |
| EU (EFSA 2023) | 1-5 m | < 10 m | - | 1-2 m below the water table | 1-2 m |
| Even coverage | fitted | fitted | fitted | - | fitted |

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
catalogue (row counts of the tables) and a checksum of the gw_base metadata
columns used. The queries rerun when:

- rows are added or deleted (row counts),
- values in the gw_base metadata change (checksum over well ID, ground
  elevation, screen depths and coordinates): the metadata query reruns,
- the FARM file changes,
- the analysis wells or their (filled) ground elevations change: the level
  queries rerun, as they read only these wells and check the depth below
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
only the monthly means of the window come back; DuckDB does the rest. The
level queries need the analysis wells and their ground elevations, which
may come from FARM: `db_query_wells()` (`R/db.R`) writes them to a temporary
table `#well_elev` on the connection the query runs on (tempdb, no rights
beyond reading needed) and drops it afterwards. Results come back
as tibbles for dplyr.

## Source check (temporary)

Compares the well metadata of gw_base with three other sources and these
among each other, kept apart from the rest (targets `check_*`,
`R/source_check.R`, settings in `check_settings`), to be removed when done:
the analysis does not need it (the shared reading and linking functions are
in `R/link.R`). Needs the package readxl.

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
  (`HE_`, `HE-`), all signs other than letters and digits (spaces, `-`,
  `/`, `.`) and leading zeros are removed first: `HE_00001000`, `00001000`
  and `1000` (in Hessen) are the same well. `id_examples.csv` lists per
  source and state IDs found only in the source or only in gw_base, as
  written and as compared, to see why IDs do not match. The state comes from the file name, a state column (codes
  or names, also within longer names such as "Freie und Hansestadt Hamburg";
  values not recognised are reported as a warning) or the ID prefix. As a
  check, every gw_base column with text or whole numbers is also tried
  (`id_matching.csv`). If the mapped ID links less than half of a file's
  wells and another column clearly more, that column is used (`id_auto`,
  noted in `id_pairs.csv`); a column can also be set per file in
  `id_pairs`. For the distances between the coordinates of linked wells
  (`id_pairs.csv`, `check_meer`), EPSG codes that PROJ does not know (such
  as 0) count as missing, and the CRS is guessed from the coordinates.
- Do the IDs match? `plots/source_check/id_overlap.png` shows per pair of
  sources and state the wells in both, only in the left and only in the
  right source (`id_overlap.csv`); `id_overlap_meer.png` the share of the
  meer wells found in gw_base, FARM and the state data.
- The wells of the meer files: target `check_meer` (`tar_read(check_meer)`,
  also `meer_wells.csv`) has one row per meer well with the IDs, the values
  of gw_base, meer, FARM, the states and the meer DEM side by side
  (`elev_gs_farm`, ...), the differences to gw_base
  (`elev_gs_farm_minus_gw_base`, ...) and the distances of the coordinates
  to gw_base. `meer_differences.png` shows per field, source and state how
  many meer wells differ from the state data (the reference,
  `meer_reference`, since gw_base does not contain all wells) by how much,
  `meer_map.png` where (ground elevation), `meer_large_differences.csv` the
  differences above `report_threshold_m`. Target `check_meer_elevation`
  (written by target `check_meer_elevation_csv` to
  `output/source_check/meer_elevation.csv`): the meer wells with the ground elevation and
  measuring point of every source side by side and the difference between
  every pair of sources (`elev_gs_farm_minus_gw_base`,
  `elev_gs_dem_minus_meer`, ...). `values.csv` has the values of all wells of all
  sources side by side.

## Planned

- Ground surface elevation: where `elev_gs` is still missing after the
  filling, estimate it from the measuring point height of FARM (`elev_mp` in
  `well_meta`) minus the median stick-up, flagged as estimated.
- Estimated screens from the final depth, flagged as estimated: FARM has a
  column `endteufe__m_u__ansatzpunkt_` (screen depths are missing for most
  wells in BY, BW and ST).
