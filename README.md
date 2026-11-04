# groundwater_obswells

Analyses how suitable groundwater observation wells are, using their metadata
and time series from a PostgreSQL database.

## Setup

1. Install the R packages:

   ```r
   install.packages(c("targets", "RPostgres", "DBI", "dplyr", "tibble", "keyring"))
   ```

2. Copy `.Renviron.example` to `.Renviron` and fill in server, port, database
   and user. `.Renviron` is git-ignored, so never commit these anywhere else.
3. Restart R in the project folder, so it reads `.Renviron`.
4. Store the password once in the system keyring (Windows Credential Manager,
   macOS Keychain, Secret Service on Linux); this prompts for it:

   ```r
   targets::tar_source("R")
   set_pg_password()
   ```

   `PGPASSWORD` in `.Renviron` or a `~/.pgpass` file work as well.

## Run

```r
targets::tar_make()
targets::tar_read(db_table_overview)   # one row per table
targets::tar_read(db_columns)          # one row per column
```

## How it works

RPostgres connects to the database with a read-only session (`R/db.R`).
The catalogue queries (`R/catalog.R`) read only PostgreSQL's system
catalogue, so no table is scanned. Results come back as tibbles for dplyr.
