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
      x = "x_25832",             # coordinate columns of meta_table; NULL: found
      y = "y_25832",             #   by their names (x/y, rw/hw, lon/lat, ...)
      crs = 25832,               # EPSG code of x/y; NULL: guessed per well

      # checks of single measurements (on the server)
      gwl_valid_range = c(-100, 3000), # m a.s.l.; outside: error codes like -999
      dtw_valid_range = c(-2, 150),    # m below ground; outside: implausible

      # recent window: the last recent_years up to the reference month
      # (reference_date, NULL: today); monthly means further than
      # max(spike_mad * MAD, spike_min_dev) m from the well's median there are
      # removed as spikes
      recent_years = 10,
      reference_date = NULL,
      spike_mad = 5,
      spike_min_dev = 1,
      # TRUE: a spike must also lie that far from both neighbouring months,
      # so real events lasting some months (wet winters, droughts) stay
      spike_neighbours = TRUE,
      # usable: each season (DJF, MAM, JJA, SON) has data in at least this
      # many years of the recent window
      min_years_per_season = 5,
      # mean, annual low etc. over the window_years most recent years with data
      # in the recent window (not necessarily consecutive)
      window_years = 5
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
  # Each scenario filters the usable wells by five criteria (m below
  # ground), in this order: mean depth to groundwater between dtw_mean_min
  # and dtw_mean_max; maximum depth below dtw_max; screen top position
  # (screen_rule "below": 0 to top_below_max m below the mean water table;
  # "spans": above it, so the screen reaches across); not falling dry (screen
  # bottom deeper than the typical annual low); screen length up to
  # screen_length_max.
  tar_target(
    scenarios_fixed,
    tibble::tribble(
      ~id, ~name, ~dtw_mean_min, ~dtw_mean_max, ~dtw_max, ~screen_rule, ~top_below_max, ~screen_length_max, ~colour, ~purpose,
      "farm_strict", "FARM strict", 1, 5, 10, "below", 2, 2, "#E4572E",
        "Shallow groundwater and a short screen just below the water table (FARM, strict).",
      "farm_tolerant", "FARM tolerant", 1, 5, 10, "below", 3, 4, "#F0A202",
        "As FARM strict, but with the screen top up to 3 m below the water table and screens up to 4 m (FARM, tolerant).",
      "very_shallow", "Very shallow", 0.5, 3, 5, "below", 1, 2, "#1B998B",
        "Groundwater within 3 m of the surface, which seepage water reaches fastest, and a short screen right below the water table.",
      "screen_at_water_table", "Screen at the water table", 1, 5, 10, "spans", NA, 3, "#2E86AB",
        "Screens across the water table sample the uppermost, youngest groundwater, where substances from the surface arrive first."
    )
  ),
  # Scenario fitted to the data for wells across all of Germany, also in the
  # south: Germany is divided into cells of cell_km; of all threshold
  # combinations on the grids whose wells cover at least keep_share of the
  # cells the loosest combination covers, the strictest (each threshold
  # scaled 0..1 on its grid, weighted, the smallest sum). Maximum depth =
  # dtw_mean_max + dtw_max_extra. Screen top below the water table and "not
  # falling dry" as in the other scenarios.
  tar_target(
    coverage_settings,
    list(
      id = "even_coverage", name = "Even coverage", colour = "#6A4C93",
      cell_km = 50, dtw_mean_min = 0, keep_share = 0.8,
      grids = list(
        dtw_mean_max = seq(1, 15, by = 0.5),
        dtw_max_extra = 1:8,
        top_below_max = 1:5,
        screen_length_max = 1:6
      ),
      # weight of each threshold when choosing the strictest combination:
      # higher = loosening it costs more, so it stays stricter
      weights = list(dtw_mean_max = 1, dtw_max_extra = 1, top_below_max = 1, screen_length_max = 1)
    )
  ),
  tar_target(scenario_optimisation, optimise_coverage_scenario(well_points, coverage_settings)),
  tar_target(scenarios, dplyr::bind_rows(scenarios_fixed, scenario_optimisation$scenario)),
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
  # the cache folders are listed on every run, so borders and plots are
  # updated when files are added there (state borders, fonts)
  tar_target(naturalearth_files, list_naturalearth_files(), cue = tar_cue(mode = "always")),
  tar_target(germany, read_germany(files = naturalearth_files)),
  tar_target(plot_font, setup_plot_font(plot_config$font), cue = tar_cue(mode = "always")),
  tar_target(scenario_wells, evaluate_scenarios(well_dtw, scenarios)),
  tar_target(scenario_steps, count_scenario_steps(scenario_wells, scenarios)),
  tar_target(
    scenario_plots,
    plot_scenarios(scenarios, well_points, scenario_wells, scenario_steps, germany, plot_config, plot_font),
    format = "file"
  ),
  tar_target(
    sankey_plots,
    plot_sankeys(well_dtw, scenarios, scenario_steps, well_spec, plot_config, plot_font),
    format = "file"
  ),
  tar_target(
    optimisation_plot,
    plot_coverage_optimisation(scenario_optimisation, germany, plot_config, plot_font),
    format = "file"
  ),
  tar_target(
    screen_map_plot,
    plot_missing_screens(well_points, germany, plot_config, plot_font),
    format = "file"
  ),
  tar_target(
    sankey_combined_plot,
    plot_sankey_combined(well_dtw, scenarios, scenario_wells, scenario_steps, well_spec, plot_config, plot_font),
    format = "file"
  ),
  # Drop-out per state (prefix of the well ID)
  tar_target(state_drop_out, state_attrition(well_dtw, scenario_wells, scenarios, well_spec)),
  tar_target(
    state_drop_out_plot,
    plot_state_attrition(state_drop_out, scenarios, well_spec, plot_config, plot_font),
    format = "file"
  ),
  # Diagnostic sheets: n_per_group random wells from each group (selected by
  # a scenario, usable but not selected, not usable), per_page per page;
  # change seed for another sample
  tar_target(diagnostic_settings, list(n_per_group = 8, per_page = 4, seed = 1)),
  tar_target(
    diagnostic_plots,
    plot_well_diagnostics(
      well_dtw, gwl_monthly, scenario_wells, scenarios, well_spec,
      diagnostic_settings, plot_config, plot_font
    ),
    format = "file"
  ),
  # Figures for choosing the thresholds (plots/thresholds/): the recent window
  # and years per season over history_years, the years for the mean, the
  # spike rule for each factor and minimum deviation, the plausibility limits,
  # the candidate_codes (error codes) and the top_values removed values
  tar_target(
    threshold_settings,
    list(
      history_years = 30, recent_years = 3:20,
      spike_mad = c(3, 4, 5, 6, 8, 10), spike_min_dev = c(0.5, 1, 2),
      top_values = 15,
      candidate_codes = c(0, -1, -9, -99, -999, -9999, 99, 999, 9999)
    )
  ),
  tar_target(gwl_year_seasons, fetch_year_seasons(well_spec, threshold_settings)),
  tar_target(gwl_depth_histogram, fetch_depth_histogram(well_spec)),
  tar_target(gwl_candidate_codes, fetch_candidate_codes(well_spec, threshold_settings)),
  tar_target(gwl_removed_values, fetch_removed_values(well_spec, threshold_settings)),
  tar_target(
    threshold_plots,
    plot_threshold_checks(
      gwl_year_seasons, gwl_depth_histogram, gwl_candidate_codes, gwl_removed_values, gwl_monthly,
      well_dtw, well_spec, threshold_settings, plot_config, plot_font
    ),
    format = "file"
  ),
  tar_target(
    method_flowchart_plot,
    plot_method_flowchart(well_spec, coverage_settings, plot_config, plot_font),
    format = "file"
  ),
  tar_target(
    flowchart_plot,
    plot_flowchart(well_dtw, scenarios, scenario_steps, well_spec, plot_config, plot_font),
    format = "file"
  )
)
