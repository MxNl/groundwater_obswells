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
     "systemfonts", "rnaturalearthdata"
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

The figures are written to `plots/`: `flowchart.png` and one
`scenario_<id>.png` per scenario.

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
3. **Window**: the last `window_years` * 12 months up to the well's last
   month with valid data.
4. **Spikes**: monthly means further than `max(spike_mad * MAD,
   spike_min_dev)` from the well's median in the window are removed
   (`n_months_spike`).
5. **Means**: `gwl_mean` is the mean of the monthly means;
   `gwl_mean_seasonal` averages each calendar month first and then the 12
   months, so a season with more gaps does not bias it (only when all 12
   calendar months have data).
6. **Depth below ground**: `dtw_mean = elev_gs - gwl_mean` (and
   `dtw_mean_seasonal`); `dtw_shallowest` and `dtw_deepest` from the highest
   and lowest monthly mean. Only wells with `elev_gs` get a depth.

How reliable the mean is: `coverage_window` (share of the window's months
with data), `max_gap_months` (longest run of months without data, including
the start of the window), `n_calendar_months`, `n_seasons` and `seasons`.

`usable` is TRUE for wells with a depth, screen depths, valid data within
the last `recent_years` years, at least `min_coverage` of the window covered
and, with `require_all_seasons`, data in all four seasons.
`water_table_vs_screen` tells whether the mean water table is above, within
or below the screen, and `flags` lists the problems found.

`well_availability` counts the wells left after each criterion, `gwl_checks`
the measurements removed by the checks.

Steps 1-2 run in SQL Server (one scan of the table), steps 3-5 in an
in-memory DuckDB.

## Scenarios

The scenarios (`scenarios` in `_targets.R`) select usable wells with
shallow groundwater, where substances from the surface show up with a short
delay. Each filters by four properties, in m below ground surface:

1. mean depth to groundwater between `dtw_mean_min` and `dtw_mean_max`
2. maximum depth to groundwater (deepest monthly mean in the window) less
   than `dtw_max`
3. screen position (`screen_rule`):
   - `below`: the screen top lies below the mean water table, at most
     `top_below_max` m deeper, and the screen bottom is deeper than the
     typical annual low (`dtw_annual_low`, deepest month of each year
     averaged), so the well does not fall dry in the annual cycle
   - `spans`: the screen reaches across the mean water table, its bottom
     deeper than the annual low
4. screen length up to `screen_length_max`

| Scenario | Mean depth | Max depth | Screen | Length |
|---|---|---|---|---|
| FARM strict | 1-5 m | < 10 m | top 0-2 m below the water table | <= 2 m |
| FARM tolerant | 1-5 m | < 10 m | top 0-3 m below the water table | <= 4 m |
| Very shallow | 0.5-3 m | < 5 m | top 0-1 m below the water table | <= 2 m |
| Screen at the water table | 1-5 m | < 10 m | across the water table | <= 3 m |
| Extended depth | 1-8 m | < 12 m | top 0-3 m below the water table | <= 4 m |

In all scenarios the screen bottom must be deeper than the annual low.

Each scenario figure shows the selected wells on a map of Germany, their
number with the wells left after each criterion, and the distribution of
the four properties (usable wells in grey, selected wells in colour). The
flow chart shows the steps from the metadata to the usable wells and the
scenarios. Font (Noto Sans Display, downloaded from Google Fonts if
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
hand; until then the maps show the country outline from rnaturalearthdata.

## How it works

`R/db.R` connects through ODBC with a connection pool (pool package), so all
targets of a run share their connections. The catalogue queries (`R/catalog.R`) read
only SQL Server's catalogue views, so no table is scanned. The groundwater
levels (`R/wells.R`) are checked and averaged per month on the server, so
only the monthly means of the window come back; DuckDB does the rest. Results come back
as tibbles for dplyr.
