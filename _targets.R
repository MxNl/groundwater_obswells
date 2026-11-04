# Pipeline: run with targets::tar_make(), inspect with targets::tar_read(<name>).

library(targets)

tar_option_set(
  packages = c("DBI", "odbc", "pool", "duckdb", "dplyr", "tibble")
)

tar_source("R")

list(
  # Database catalogue ----------------------------------------------------------
  # The database changes outside the pipeline, so its catalogue is read on
  # every run. It is cheap (system catalogue only) and the targets below only
  # rebuild when it actually changed (including the tables' row counts).
  tar_target(
    db_catalog,
    fetch_catalog(),
    cue = tar_cue(mode = "always")
  ),
  tar_target(db_tables, db_catalog$tables),
  tar_target(db_columns, db_catalog$columns),
  tar_target(db_table_overview, summarise_tables(db_tables, db_columns)),

  # Depth to groundwater per well -----------------------------------------------
  tar_target(
    well_config,
    list(
      meta_table = "tbl_meta_well_import",
      meta_id = "well_id",
      elev_gs = "elev_gs",       # elevation of the ground surface, m a.s.l.
      screen_top = "screen_top", # depth of the screen top, m below ground
      screen_bot = "screen_bot", # depth of the screen bottom, m below ground
      gwl_table = "tbl_gwl_raw",
      gwl_id = "well_id",
      gwl_date = NULL,           # NULL: the one date column of gwl_table
      gwl_level = "gwl",         # groundwater level, m a.s.l.

      # checks of single measurements (on the server)
      gwl_valid_range = c(-100, 3000), # m a.s.l.; outside: error codes like -999
      dtw_valid_range = c(-5, 300),    # m below ground; outside: implausible

      # mean over the last window_years * 12 months up to the last valid month
      window_years = 5,
      # monthly means further than max(spike_mad * MAD, spike_min_dev) m from
      # the well's median in the window are removed as spikes
      spike_mad = 5,
      spike_min_dev = 1,

      # criteria for "usable"
      recent_years = 10,         # last valid month within the last 10 years
      min_coverage = 0.5,        # at least 50 % of the window's months with data
      require_all_seasons = TRUE # data in all four seasons in the window
    )
  ),
  tar_target(well_spec, resolve_well_spec(well_config, db_columns, db_tables)),
  tar_target(well_meta, fetch_well_meta(well_spec)),
  tar_target(gwl_monthly, fetch_gwl_monthly(well_spec)),
  tar_target(gwl_stats, summarise_gwl_monthly(gwl_monthly, well_spec)),
  tar_target(well_dtw, assess_well_dtw(well_meta, gwl_stats, well_spec)),
  tar_target(well_availability, summarise_well_availability(well_dtw, well_spec)),
  tar_target(gwl_checks, summarise_gwl_checks(well_dtw))
)
