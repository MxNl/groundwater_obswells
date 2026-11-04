# groundwater_obswells

Analyses how suitable groundwater observation wells are, using their metadata
and time series from a Microsoft SQL Server database.

## Setup

1. Install the R packages and the ODBC driver
   ([ODBC Driver 18 for SQL Server](https://learn.microsoft.com/sql/connect/odbc/download-odbc-driver-for-sql-server)):

   ```r
   install.packages(c("targets", "odbc", "pool", "DBI", "dplyr", "tibble", "keyring"))
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
targets::tar_read(well_dtw)            # one row per well
```

## Depth to groundwater

`well_dtw` has one row per well, from `tbl_meta_well_import` and
`tbl_gwl_raw` (table and column names are set in `well_config` in
`_targets.R`):

- `screen_top`, `screen_bot`: screen depths below ground surface, as in the
  metadata.
- `gwl_mean`: mean groundwater level (m a.s.l.) over the last
  `window_years` (default 5) years before the well's last measurement.
  The levels are averaged per month first, so dense logger periods do not
  outweigh monthly manual readings; `n_months_window` gives the number of
  months behind the mean.
- `dtw_mean`: mean depth to groundwater below ground surface,
  `elev_gs - gwl_mean`. Without `elev_gs`, the ground surface is estimated
  from `elev_mp` minus the median stick-up (`elev_mp - elev_gs`) of all wells
  with both; `dtw_method` says which was used. `dtw_shallowest` and
  `dtw_deepest` come from the highest and lowest monthly mean.
- `water_table_vs_screen`: whether the mean water table is above, within or
  below the screen.
- `flags`: data problems, e.g. implausible stick-up, swapped screen depths,
  mean level above ground, few months of data, no recent data.

## How it works

`R/db.R` connects through ODBC with a connection pool (pool package), so all
targets of a run share their connections. The catalogue queries (`R/catalog.R`) read
only SQL Server's catalogue views, so no table is scanned. The groundwater
levels (`R/wells.R`) are aggregated on the server, so only one row per well
comes back to R. Results come back
as tibbles for dplyr.
