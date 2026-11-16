# Depth to groundwater per well --------------------------------------------------
#
# For every well: the screen depths from the metadata and the mean depth to
# the groundwater table below ground surface (dtw), from the groundwater
# levels (gwl, m above sea level) of the last years with data:
#
#   1. On the server, each measurement is checked: empty, outside the valid
#      level range (error codes such as -999) or an implausible depth below
#      ground. Only valid measurements are averaged per month.
#   2. Locally, in DuckDB: the recent window is the last `recent_years` up
#      to the current month (reference_date); monthly means far from the
#      well's median there (spikes) are removed. The mean window is the
#      `window_years` most recent calendar years with data in it.
#   3. The mean of the monthly means is the mean level, so dense logger
#      periods do not outweigh monthly manual readings. The seasonal mean
#      averages each calendar month over the years first, so every month
#      counts the same even if some are missing more often.
#   4. dtw = elev_gs - mean level. A well is usable if each season has data
#      in at least `min_years_per_season` years of the recent window.

sql_ident <- function(x) {
  paste0("[", gsub("]", "]]", x, fixed = TRUE), "]")
}

date_types <- c("date", "datetime", "datetime2", "smalldatetime", "datetimeoffset")

# Checks the configured tables and columns against the catalogue and fills in
# what is not configured. Includes the row counts of both tables, so the
# targets that read them rerun when data are added.
resolve_well_spec <- function(config, columns, tables) {
  find_table <- function(name) {
    hit <- dplyr::distinct(dplyr::filter(columns, table_name == name), table_schema, table_name)
    if (nrow(hit) != 1) {
      stop(
        "Table ", name, " found ", nrow(hit), " times in the catalogue, expected once.",
        " Set DB_SCHEMAS to the schema that holds it.",
        call. = FALSE
      )
    }
    hit$table_schema
  }
  check_columns <- function(schema, table, needed) {
    have <- columns$column_name[columns$table_schema == schema & columns$table_name == table]
    missing <- setdiff(needed, have)
    if (length(missing) > 0) {
      stop(
        "Column(s) ", paste(missing, collapse = ", "), " not found in ", table,
        ". It has: ", paste(have, collapse = ", "),
        call. = FALSE
      )
    }
  }

  meta_schema <- find_table(config$meta_table)
  meta_cols <- columns$column_name[
    columns$table_schema == meta_schema & columns$table_name == config$meta_table
  ]
  x_col <- config$x %||% find_coordinate_column(meta_cols, "x")
  y_col <- config$y %||% find_coordinate_column(meta_cols, "y")
  if (is.null(x_col) || is.null(y_col)) {
    stop(
      "Could not find the coordinate columns of ", config$meta_table,
      ". Set x and y in well_config (_targets.R). Columns: ",
      paste(meta_cols, collapse = ", "),
      call. = FALSE
    )
  }
  gwl_schema <- find_table(config$gwl_table)
  check_columns(
    meta_schema, config$meta_table,
    c(config$meta_id, config$elev_gs, config$screen_top, config$screen_bot, x_col, y_col)
  )

  date_col <- config$gwl_date
  if (is.null(date_col)) {
    gwl_cols <- dplyr::filter(
      columns, table_schema == gwl_schema, table_name == config$gwl_table
    )
    date_col <- gwl_cols$column_name[sub("\\(.*$", "", gwl_cols$data_type) %in% date_types]
    if (length(date_col) != 1) {
      stop(
        "Could not pick the date column of ", config$gwl_table, ": found ",
        if (length(date_col) == 0) "none" else paste(date_col, collapse = ", "),
        ". Set gwl_date in well_config (_targets.R).",
        call. = FALSE
      )
    }
  }
  check_columns(gwl_schema, config$gwl_table, c(config$gwl_id, date_col, config$gwl_level))

  n_rows <- function(schema, table) {
    tables$n_rows[tables$table_schema == schema & tables$table_name == table]
  }
  spec <- config
  spec$meta_schema <- meta_schema
  spec$gwl_schema <- gwl_schema
  spec$gwl_date <- date_col
  spec$meta_n_rows <- n_rows(meta_schema, config$meta_table)
  spec$gwl_n_rows <- n_rows(gwl_schema, config$gwl_table)
  spec$x <- x_col
  spec$y <- y_col
  # month the recent window ends with (as year * 12 + month - 1); with
  # reference_date NULL the current month, so the window moves on monthly
  reference <- as.Date(config$reference_date %||% Sys.Date())
  spec$reference_month <- as.integer(format(reference, "%Y")) * 12L +
    as.integer(format(reference, "%m")) - 1L
  spec
}

# Coordinate column by its name: exact names first, then name prefixes
# (e.g. "x_utm", "lon_wgs84").
find_coordinate_column <- function(columns, axis) {
  names <- if (axis == "x") {
    c("x", "xcoord", "x_coord", "coord_x", "rw", "rechtswert", "ostwert",
      "east", "easting", "utm_x", "lon", "lng", "long", "longitude")
  } else {
    c("y", "ycoord", "y_coord", "coord_y", "hw", "hochwert", "nordwert",
      "north", "northing", "utm_y", "lat", "latitude")
  }
  lower <- tolower(columns)
  hit <- columns[lower %in% names]
  if (length(hit) == 0) {
    prefix <- paste0("^(", paste(names, collapse = "|"), ")[_ .]")
    hit <- columns[grepl(prefix, lower)]
  }
  if (length(hit) == 0) NULL else hit[1]
}

# The metadata columns needed, one row per metadata record.
fetch_well_meta <- function(spec) {
  sql <- sprintf(
    "SELECT CAST(%s AS nvarchar(100)) AS well_id,
            CAST(%s AS float) AS elev_gs,
            CAST(%s AS float) AS screen_top,
            CAST(%s AS float) AS screen_bot,
            CAST(%s AS float) AS x,
            CAST(%s AS float) AS y
     FROM %s.%s",
    sql_ident(spec$meta_id), sql_ident(spec$elev_gs),
    sql_ident(spec$screen_top), sql_ident(spec$screen_bot),
    sql_ident(spec$x), sql_ident(spec$y),
    sql_ident(spec$meta_schema), sql_ident(spec$meta_table)
  )
  db_query(sql)
}

sql_number <- function(x) {
  format(x, scientific = FALSE, trim = TRUE)
}

# One scan of the groundwater level table. Returns the monthly means of the
# valid measurements within the window, and for every well the counts of
# measurements by check result. Wells without any valid measurement come
# back as one row with an empty month.
gwl_monthly_sql <- function(spec) {
  sprintf(
    "WITH gs AS (
  SELECT CAST(%1$s AS nvarchar(100)) AS well_id,
         MAX(CAST(%2$s AS float)) AS elev_gs
  FROM %3$s.%4$s
  GROUP BY CAST(%1$s AS nvarchar(100))
),
checked AS (
  SELECT r.well_id,
         YEAR(r.obs_date) * 12 + MONTH(r.obs_date) - 1 AS month_index,
         r.gwl,
         CASE
           WHEN r.gwl IS NULL THEN 1
           WHEN r.gwl < %10$s OR r.gwl > %11$s THEN 2
           WHEN gs.elev_gs - r.gwl < %12$s OR gs.elev_gs - r.gwl > %13$s THEN 3
           ELSE 0
         END AS check_code
  FROM (
    SELECT CAST(%5$s AS nvarchar(100)) AS well_id,
           CAST(%6$s AS date) AS obs_date,
           CAST(%7$s AS float) AS gwl
    FROM %8$s.%9$s
    WHERE %5$s IS NOT NULL AND %6$s IS NOT NULL
  ) AS r
  LEFT JOIN gs ON gs.well_id = r.well_id
),
monthly AS (
  SELECT well_id,
         month_index,
         SUM(CASE WHEN check_code = 0 THEN 1 ELSE 0 END) AS n_valid,
         SUM(CASE WHEN check_code = 1 THEN 1 ELSE 0 END) AS n_empty,
         SUM(CASE WHEN check_code = 2 THEN 1 ELSE 0 END) AS n_out_of_range,
         SUM(CASE WHEN check_code = 3 THEN 1 ELSE 0 END) AS n_implausible_dtw,
         AVG(CASE WHEN check_code = 0 THEN gwl END) AS gwl
  FROM checked
  GROUP BY well_id, month_index
),
per_well AS (
  SELECT m.*,
         SUM(n_valid) OVER (PARTITION BY well_id) AS n_obs_valid,
         SUM(n_empty) OVER (PARTITION BY well_id) AS n_obs_empty,
         SUM(n_out_of_range) OVER (PARTITION BY well_id) AS n_obs_out_of_range,
         SUM(n_implausible_dtw) OVER (PARTITION BY well_id) AS n_obs_implausible_dtw,
         MIN(CASE WHEN n_valid > 0 THEN month_index END)
           OVER (PARTITION BY well_id) AS first_month,
         MAX(CASE WHEN n_valid > 0 THEN month_index END)
           OVER (PARTITION BY well_id) AS last_month,
         ROW_NUMBER() OVER (PARTITION BY well_id ORDER BY month_index DESC) AS rn
  FROM monthly AS m
)
SELECT well_id,
       CASE WHEN last_month IS NULL THEN NULL ELSE month_index END AS month_index,
       CASE WHEN last_month IS NULL THEN NULL ELSE n_valid END AS n_obs,
       gwl,
       CAST(n_obs_valid AS float) AS n_obs_valid,
       CAST(n_obs_empty AS float) AS n_obs_empty,
       CAST(n_obs_out_of_range AS float) AS n_obs_out_of_range,
       CAST(n_obs_implausible_dtw AS float) AS n_obs_implausible_dtw,
       first_month,
       last_month
FROM per_well
WHERE (n_valid > 0 AND month_index > %14$d - 12 * %15$d)
   OR rn = 1",
    sql_ident(spec$meta_id), sql_ident(spec$elev_gs),
    sql_ident(spec$meta_schema), sql_ident(spec$meta_table),
    sql_ident(spec$gwl_id), sql_ident(spec$gwl_date), sql_ident(spec$gwl_level),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table),
    sql_number(spec$gwl_valid_range[1]), sql_number(spec$gwl_valid_range[2]),
    sql_number(spec$dtw_valid_range[1]), sql_number(spec$dtw_valid_range[2]),
    as.integer(spec$reference_month), as.integer(spec$recent_years)
  )
}

# Monthly means in the window plus check counts per well, see
# gwl_monthly_sql(). About one row per well and month in the window.
fetch_gwl_monthly <- function(spec) {
  db_query(gwl_monthly_sql(spec))
}

month_date <- function(month_index) {
  out <- rep(as.Date(NA), length(month_index))
  ok <- !is.na(month_index)
  out[ok] <- as.Date(sprintf("%04d-%02d-01", month_index[ok] %/% 12, month_index[ok] %% 12 + 1))
  out
}

# Per well statistics of the monthly means, computed in an in-memory DuckDB
# (fast for millions of well-months):
#   - recent window: the last recent_years * 12 months up to the reference
#     month; spikes there removed: monthly means further than
#     max(spike_mad * MAD, spike_min_dev) from the well's median (wells with
#     >= 3 months) and, with spike_neighbours, also that far from both
#     neighbouring months with data, so single outliers but no real events
#   - seasons: in how many years of the recent window each season (DJF, MAM,
#     JJA, SON) has data; min_years_per_season is the smallest of the four
#     (0 if a season never has data)
#   - mean window: the window_years most recent calendar years with data in
#     the recent window (not necessarily consecutive): mean, sd, min and max
#     of their monthly means, the seasonal mean (mean of the 12 calendar-month
#     means, if all 12 have data), and the annual low (lowest monthly mean of
#     each year, averaged over the years with >= 6 months, else all years)
#   - coverage_recent: share of the recent window's months with data;
#     max_gap_months: longest run of months without data in it
gwl_window_stats_sql <- function(spec) {
  sprintf(
    "WITH m AS (
  SELECT well_id, CAST(month_index AS INTEGER) AS month_index, n_obs, gwl
  FROM monthly
  WHERE month_index IS NOT NULL AND gwl IS NOT NULL
    AND month_index > %3$d - %4$d
    AND month_index <= %3$d
),
centre AS (
  SELECT well_id, median(gwl) AS med, count(*) AS n
  FROM m GROUP BY well_id
),
spread AS (
  SELECT m.well_id, 1.4826 * median(abs(m.gwl - c.med)) AS mad
  FROM m JOIN centre AS c USING (well_id) GROUP BY m.well_id
),
-- deviation from the median, and from the previous and next month with data
compared AS (
  SELECT m.*,
         c.n,
         abs(m.gwl - c.med) AS dev,
         greatest(%1$s * s.mad, %2$s) AS threshold,
         abs(m.gwl - lag(m.gwl) OVER (PARTITION BY m.well_id ORDER BY m.month_index)) AS dev_prev,
         abs(m.gwl - lead(m.gwl) OVER (PARTITION BY m.well_id ORDER BY m.month_index)) AS dev_next
  FROM m
  JOIN centre AS c USING (well_id)
  JOIN spread AS s USING (well_id)
),
-- a spike deviates from the median by more than the threshold and, with
-- spike_neighbours, also from both neighbouring months (a missing neighbour
-- does not count): a single outlier, not a real event lasting some months
marked AS (
  SELECT well_id, month_index, n_obs, gwl,
         n >= 3 AND dev > threshold
           AND (NOT %6$s OR ((dev_prev IS NULL OR dev_prev > threshold)
                             AND (dev_next IS NULL OR dev_next > threshold))) AS spike
  FROM compared
),
valid AS (
  SELECT *,
         month_index // 12 AS obs_year,
         month_index %% 12 + 1 AS cal_month,
         CASE
           WHEN month_index %% 12 + 1 IN (12, 1, 2) THEN 'DJF'
           WHEN month_index %% 12 + 1 IN (3, 4, 5) THEN 'MAM'
           WHEN month_index %% 12 + 1 IN (6, 7, 8) THEN 'JJA'
           ELSE 'SON'
         END AS season,
         month_index - coalesce(
           lag(month_index) OVER (PARTITION BY well_id ORDER BY month_index),
           %3$d - %4$d
         ) - 1 AS gap
  FROM marked
  WHERE NOT spike
),
recent AS (
  SELECT well_id,
         CAST(count(*) AS DOUBLE) AS n_months_recent,
         CAST(count(*) AS DOUBLE) / %4$d AS coverage_recent,
         -- months without data after the last month with data count as gap too
         CAST(greatest(max(gap), %3$d - max(month_index)) AS DOUBLE) AS max_gap_months,
         CAST(count(DISTINCT obs_year) AS DOUBLE) AS n_years_recent
  FROM valid GROUP BY well_id
),
season_years AS (
  SELECT well_id, season, count(DISTINCT obs_year) AS n_years
  FROM valid GROUP BY well_id, season
),
season_cover AS (
  SELECT well_id,
         CAST(CASE WHEN count(*) = 4 THEN min(n_years) ELSE 0 END AS DOUBLE) AS min_years_per_season,
         CAST(count(*) AS DOUBLE) AS n_seasons,
         concat_ws(',',
           CASE WHEN bool_or(season = 'DJF') THEN 'DJF' END,
           CASE WHEN bool_or(season = 'MAM') THEN 'MAM' END,
           CASE WHEN bool_or(season = 'JJA') THEN 'JJA' END,
           CASE WHEN bool_or(season = 'SON') THEN 'SON' END
         ) AS seasons
  FROM season_years GROUP BY well_id
),
ranked_years AS (
  SELECT well_id, obs_year,
         row_number() OVER (PARTITION BY well_id ORDER BY obs_year DESC) AS year_rank
  FROM (SELECT DISTINCT well_id, obs_year FROM valid)
),
kept AS (
  SELECT v.*
  FROM valid AS v
  JOIN ranked_years AS r USING (well_id, obs_year)
  WHERE r.year_rank <= %5$d
),
annual AS (
  SELECT well_id, obs_year, min(gwl) AS gwl_low, count(*) AS n
  FROM kept GROUP BY well_id, obs_year
),
annual_low AS (
  SELECT well_id,
         coalesce(avg(gwl_low) FILTER (WHERE n >= 6), avg(gwl_low)) AS gwl_annual_low
  FROM annual GROUP BY well_id
),
seasonal AS (
  SELECT well_id,
         CASE WHEN count(*) = 12 THEN avg(gwl) END AS gwl_mean_seasonal
  FROM (SELECT well_id, cal_month, avg(gwl) AS gwl FROM kept GROUP BY well_id, cal_month)
  GROUP BY well_id
),
window_stats AS (
  SELECT well_id,
         CAST(sum(n_obs) AS DOUBLE) AS n_obs_window,
         CAST(count(*) AS DOUBLE) AS n_months_window,
         CAST(count(DISTINCT obs_year) AS DOUBLE) AS n_years_window,
         CAST(min(obs_year) AS DOUBLE) AS first_year_window,
         CAST(max(obs_year) AS DOUBLE) AS last_year_window,
         CAST(count(DISTINCT cal_month) AS DOUBLE) AS n_calendar_months,
         avg(gwl) AS gwl_mean,
         stddev_samp(gwl) AS gwl_sd,
         min(gwl) AS gwl_min,
         max(gwl) AS gwl_max
  FROM kept GROUP BY well_id
),
spikes AS (
  SELECT well_id, CAST(sum(CAST(spike AS INTEGER)) AS DOUBLE) AS n_months_spike
  FROM marked GROUP BY well_id
),
counts AS (
  SELECT DISTINCT well_id, n_obs_valid, n_obs_empty, n_obs_out_of_range,
         n_obs_implausible_dtw, first_month, last_month
  FROM monthly
)
SELECT counts.*,
       spikes.n_months_spike,
       recent.* EXCLUDE (well_id),
       season_cover.* EXCLUDE (well_id),
       window_stats.* EXCLUDE (well_id),
       seasonal.gwl_mean_seasonal,
       annual_low.gwl_annual_low
FROM counts
LEFT JOIN spikes USING (well_id)
LEFT JOIN recent USING (well_id)
LEFT JOIN season_cover USING (well_id)
LEFT JOIN window_stats USING (well_id)
LEFT JOIN seasonal USING (well_id)
LEFT JOIN annual_low USING (well_id)
ORDER BY well_id",
    sql_number(spec$spike_mad), sql_number(spec$spike_min_dev),
    as.integer(spec$reference_month), as.integer(12 * spec$recent_years),
    as.integer(spec$window_years),
    if (isTRUE(spec$spike_neighbours %||% TRUE)) "TRUE" else "FALSE"
  )
}

summarise_gwl_monthly <- function(monthly, spec) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "monthly", monthly)
  DBI::dbGetQuery(con, gwl_window_stats_sql(spec)) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      first_valid_month = month_date(first_month),
      last_valid_month = month_date(last_month),
      .keep = "unused"
    )
}

# Combines a named list of logical conditions into one text per row, e.g.
# "no screen depths; low coverage". NA where none holds.
combine_flags <- function(conds) {
  hits <- lapply(conds, function(x) !is.na(x) & x)
  out <- rep(NA_character_, length(hits[[1]]))
  for (name in names(hits)) {
    hit <- hits[[name]]
    out[hit] <- ifelse(is.na(out[hit]), name, paste(out[hit], name, sep = "; "))
  }
  out
}

# One row per well (in the metadata, in the time series or both): screen
# depths, mean depth to groundwater below ground surface, data quality and
# whether the well is usable by the criteria in the config.
assess_well_dtw <- function(meta, gwl_stats, spec) {
  meta <- meta |>
    dplyr::mutate(n_meta_records = dplyr::n(), .by = well_id) |>
    dplyr::distinct(well_id, .keep_all = TRUE)

  dplyr::full_join(
    dplyr::mutate(meta, in_meta = TRUE),
    dplyr::mutate(gwl_stats, in_gwl = TRUE),
    by = "well_id"
  ) |>
    dplyr::mutate(
      in_meta = !is.na(in_meta),
      in_gwl = !is.na(in_gwl),
      # depth below ground surface, m (positive = below ground)
      dtw_mean = elev_gs - gwl_mean,
      dtw_mean_seasonal = elev_gs - gwl_mean_seasonal,
      dtw_shallowest = elev_gs - gwl_max,
      dtw_deepest = elev_gs - gwl_min,
      # typical depth at the annual low (deepest month of a year, averaged)
      dtw_annual_low = elev_gs - gwl_annual_low,
      screen_length = screen_bot - screen_top,
      water_table_vs_screen = dplyr::case_when(
        is.na(dtw_mean) | is.na(screen_top) | is.na(screen_bot) ~ NA_character_,
        dtw_mean < pmin(screen_top, screen_bot) ~ "above screen",
        dtw_mean <= pmax(screen_top, screen_bot) ~ "within screen",
        .default = "below screen"
      ),
      # valid data in the recent window, and each season in enough of its years
      recent = dplyr::coalesce(n_months_recent, 0) > 0,
      seasons_ok = dplyr::coalesce(min_years_per_season, 0) >= spec$min_years_per_season,
      usable = !is.na(dtw_mean) &
        !is.na(screen_top) & !is.na(screen_bot) &
        recent & seasons_ok,
      flags = combine_flags(stats::setNames(
        list(
          !in_meta,
          in_meta & !in_gwl,
          in_gwl & dplyr::coalesce(n_obs_valid, 0) == 0,
          in_meta & is.na(elev_gs),
          in_meta & (is.na(screen_top) | is.na(screen_bot)),
          n_meta_records > 1,
          screen_top > screen_bot,
          screen_top < 0 | screen_bot < 0,
          n_obs_out_of_range > 0,
          n_obs_implausible_dtw > 0,
          n_months_spike > 0,
          dtw_mean < 0,
          !recent,
          recent & !seasons_ok
        ),
        c(
          "no metadata",
          "no groundwater levels",
          "no valid groundwater levels",
          "no elev_gs",
          "no screen depths",
          "duplicate metadata records",
          "screen_top deeper than screen_bot",
          "negative screen depth",
          "levels out of valid range removed",
          "implausible depths removed",
          "spikes removed",
          "mean level above ground",
          paste("no data in the last", spec$recent_years, "years"),
          paste("a season in fewer than", spec$min_years_per_season, "years")
        )
      ))
    ) |>
    dplyr::select(
      well_id, usable, in_meta, in_gwl,
      x, y, elev_gs, screen_top, screen_bot, screen_length,
      dtw_mean, dtw_mean_seasonal, dtw_shallowest, dtw_deepest, dtw_annual_low,
      water_table_vs_screen,
      first_valid_month, last_valid_month, recent, seasons_ok,
      n_months_recent, coverage_recent, max_gap_months, n_years_recent,
      min_years_per_season, n_seasons, seasons,
      first_year_window, last_year_window, n_years_window, n_months_window,
      n_obs_window, n_calendar_months,
      gwl_mean, gwl_mean_seasonal, gwl_annual_low, gwl_sd, gwl_min, gwl_max,
      n_obs_valid, n_obs_empty, n_obs_out_of_range, n_obs_implausible_dtw, n_months_spike,
      flags
    ) |>
    dplyr::arrange(well_id)
}

# How far the data go: wells left after each criterion, as numbers and
# shares of the metadata wells. Each "+" line adds a criterion to the
# line above.
summarise_well_availability <- function(wells, spec) {
  m <- dplyr::filter(wells, in_meta)
  has_gs <- !is.na(m$elev_gs)
  has_screen <- !is.na(m$screen_top) & !is.na(m$screen_bot)
  has_gwl <- dplyr::coalesce(m$n_obs_valid, 0) > 0
  recent <- m$recent %in% TRUE
  seasons <- m$seasons_ok %in% TRUE
  has_dtw <- !is.na(m$dtw_mean)

  items <- c(
    "wells in metadata" = nrow(m),
    "... with elev_gs" = sum(has_gs),
    "... with screen_top and screen_bot" = sum(has_screen),
    "... with groundwater levels" = sum(m$in_gwl),
    "... with valid groundwater levels" = sum(has_gwl),
    "... with elev_gs and valid levels" = sum(has_gs & has_gwl),
    "... + screen depths" = sum(has_gs & has_gwl & has_screen),
    "... + RECENT" = sum(has_gs & has_screen & recent),
    "... + SEASONS = usable" = sum(has_dtw & has_screen & recent & seasons),
    "wells with groundwater levels but no metadata" = sum(!wells$in_meta)
  )
  names(items) <- names(items) |>
    sub("RECENT", paste("data in the last", spec$recent_years, "years"), x = _, fixed = TRUE) |>
    sub("SEASONS", paste("each season in >=", spec$min_years_per_season, "of these years"), x = _, fixed = TRUE)

  tibble::tibble(
    item = names(items),
    n_wells = unname(items),
    pct_of_meta = round(100 * unname(items) / max(nrow(m), 1), 1)
  ) |>
    dplyr::mutate(pct_of_meta = dplyr::if_else(dplyr::row_number() == dplyr::n(), NA, pct_of_meta))
}

# Measurements removed by the checks, summed over all wells.
summarise_gwl_checks <- function(wells) {
  sums <- colSums(
    wells[c("n_obs_valid", "n_obs_empty", "n_obs_out_of_range", "n_obs_implausible_dtw")],
    na.rm = TRUE
  )
  wells_with <- colSums(
    wells[c("n_obs_valid", "n_obs_empty", "n_obs_out_of_range", "n_obs_implausible_dtw")] > 0,
    na.rm = TRUE
  )
  tibble::tibble(
    check = c("valid", "empty gwl", "gwl out of valid range", "implausible depth below ground"),
    n_measurements = unname(sums),
    pct_measurements = round(100 * unname(sums) / sum(sums), 2),
    n_wells_affected = unname(wells_with)
  ) |>
    dplyr::bind_rows(tibble::tibble(
      check = "monthly means removed as spikes",
      n_measurements = NA,
      pct_measurements = NA,
      n_wells_affected = sum(wells$n_months_spike > 0, na.rm = TRUE)
    ))
}
