# Pipeline: run with targets::tar_make(), inspect with targets::tar_read(<name>).

library(targets)

tar_option_set(
  packages = c(
    "DBI", "odbc", "pool", "duckdb", "dplyr", "tibble", "tidyr",
    "sf", "ggplot2", "ggridges", "patchwork", "scales", "ragg", "systemfonts"
  )
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
      x = NULL,                  # coordinate columns of meta_table; NULL: found
      y = NULL,                  #   by their names (x/y, rw/hw, lon/lat, ...)
      crs = NULL,                # EPSG code of x/y; NULL: guessed per well

      # checks of single measurements (on the server)
      gwl_valid_range = c(-100, 3000), # m a.s.l.; outside: error codes like -999
      dtw_valid_range = c(-2, 150),    # m below ground; outside: implausible

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
  tar_target(gwl_checks, summarise_gwl_checks(well_dtw)),

  # Scenarios -----------------------------------------------------------------------
  # Each scenario filters the usable wells by four properties (m below
  # ground): mean depth to groundwater between dtw_mean_min and dtw_mean_max,
  # maximum depth below dtw_max, the screen position and the screen length
  # up to screen_length_max. Screen position (screen_rule):
  #   "below": screen top below the water table, at most top_below_max m below
  #            the mean water table and not above the typical annual low, so
  #            the screen does not fall dry in the annual cycle
  #   "spans": screen across the mean water table, its bottom below the annual
  #            low (top_below_max not used)
  tar_target(
    scenarios,
    tibble::tribble(
      ~id, ~name, ~dtw_mean_min, ~dtw_mean_max, ~dtw_max, ~screen_rule, ~top_below_max, ~screen_length_max, ~colour, ~purpose,
      "farm_strict", "FARM strict", 1, 5, 10, "below", 2, 2, "#E4572E",
        "Shallow groundwater and a short screen just below the water table (FARM, strict).",
      "farm_tolerant", "FARM tolerant", 1, 5, 10, "below", 3, 4, "#F0A202",
        "As FARM strict, but with the screen top up to 3 m below the water table and screens up to 4 m (FARM, tolerant).",
      "very_shallow", "Very shallow", 0.5, 3, 5, "below", 1, 2, "#1B998B",
        "Groundwater within 3 m of the surface, which seepage water reaches fastest, and a short screen right below the water table.",
      "screen_at_water_table", "Screen at the water table", 1, 5, 10, "spans", NA, 3, "#2E86AB",
        "Screens across the water table sample the uppermost, youngest groundwater, where substances from the surface arrive first.",
      "extended_depth", "Extended depth", 1, 8, 12, "below", 3, 4, "#6A4C93",
        "As FARM tolerant, but also groundwater 5 to 8 m deep: longer delays, but more wells."
    )
  ),
  tar_target(
    plot_config,
    list(
      font = "Noto Sans Display",  # BGR corporate design, from Google Fonts if missing
      dir = "plots",
      distribution_max_m = 16,     # x axis of the distributions, m below ground
      caption = paste(
        "Data: groundwater observation wells (tbl_meta_well_import, tbl_gwl_raw).",
        "Borders: Natural Earth."
      )
    )
  ),
  tar_target(well_points, wells_to_points(well_dtw, crs = well_config$crs)),
  tar_target(germany, read_germany()),
  tar_target(scenario_wells, evaluate_scenarios(well_dtw, scenarios)),
  tar_target(scenario_steps, count_scenario_steps(scenario_wells, scenarios)),
  tar_target(
    scenario_plots,
    plot_scenarios(scenarios, well_points, scenario_wells, scenario_steps, germany, plot_config),
    format = "file"
  ),
  tar_target(
    flowchart_plot,
    plot_flowchart(well_dtw, scenarios, scenario_steps, well_spec, plot_config),
    format = "file"
  )
)
