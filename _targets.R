# Pipeline: run with targets::tar_make(), inspect with targets::tar_read(<name>).

library(targets)

tar_option_set(
  packages = c("DBI", "odbc", "pool", "dplyr", "tibble")
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
      elev_mp = "elev_mp",       # elevation of the measuring point, m a.s.l.
      elev_gs = "elev_gs",       # elevation of the ground surface, m a.s.l.
      screen_top = "screen_top", # depth of the screen top, m below ground
      screen_bot = "screen_bot", # depth of the screen bottom, m below ground
      gwl_table = "tbl_gwl_raw",
      gwl_id = "well_id",
      gwl_date = NULL,           # NULL: the one date column of gwl_table
      gwl_level = "gwl",         # groundwater level, m a.s.l.
      window_years = 5,          # mean over the last years before the last measurement
      recent_years = 10          # "recent" = data within the last years
    )
  ),
  tar_target(well_spec, resolve_well_spec(well_config, db_columns, db_tables)),
  tar_target(well_meta, fetch_well_meta(well_spec)),
  tar_target(gwl_stats, fetch_gwl_stats(well_spec)),
  tar_target(
    well_dtw,
    assess_well_dtw(well_meta, gwl_stats, recent_years = well_config$recent_years)
  ),
  tar_target(
    well_availability,
    summarise_well_availability(well_dtw, recent_years = well_config$recent_years)
  )
)
