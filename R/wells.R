# Depth to groundwater per well --------------------------------------------------
#
# For every well: the screen depths from the metadata and the mean depth to
# the groundwater table below ground surface (dtw), from the groundwater
# levels (gwl, m above sea level) of the last years with data:
#
#   1. On the server, each measurement is checked: empty, outside the valid
#      level range (error codes such as -999) or an implausible depth below
#      ground. Only valid measurements are averaged per month.
#   2. The window is the last `window_years` * 12 calendar months up to the
#      well's last month with valid data. Locally, in DuckDB, monthly means
#      far from the well's median in the window (spikes) are removed.
#   3. The mean of the monthly means is the mean level, so dense logger
#      periods do not outweigh monthly manual readings. The seasonal mean
#      averages each calendar month over the years first, so every month
#      counts the same even if some are missing more often.
#   4. dtw = elev_gs - mean level. Coverage, the longest gap and the
#      seasons covered show how reliable that mean is.

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
  gwl_schema <- find_table(config$gwl_table)
  check_columns(
    meta_schema, config$meta_table,
    c(config$meta_id, config$elev_gs, config$screen_top, config$screen_bot)
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
  spec
}

# The metadata columns needed, one row per metadata record.
fetch_well_meta <- function(spec) {
  sql <- sprintf(
    "SELECT CAST(%s AS nvarchar(100)) AS well_id,
            CAST(%s AS float) AS elev_gs,
            CAST(%s AS float) AS screen_top,
            CAST(%s AS float) AS screen_bot
     FROM %s.%s",
    sql_ident(spec$meta_id), sql_ident(spec$elev_gs),
    sql_ident(spec$screen_top), sql_ident(spec$screen_bot),
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
WHERE (last_month IS NOT NULL AND n_valid > 0
       AND month_index > last_month - 12 * %14$d)
   OR (last_month IS NULL AND rn = 1)",
    sql_ident(spec$meta_id), sql_ident(spec$elev_gs),
    sql_ident(spec$meta_schema), sql_ident(spec$meta_table),
    sql_ident(spec$gwl_id), sql_ident(spec$gwl_date), sql_ident(spec$gwl_level),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table),
    sql_number(spec$gwl_valid_range[1]), sql_number(spec$gwl_valid_range[2]),
    sql_number(spec$dtw_valid_range[1]), sql_number(spec$dtw_valid_range[2]),
    as.integer(spec$window_years)
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

# Per well statistics of the monthly means in the window, computed in an
# in-memory DuckDB (fast for millions of well-months):
#   - spikes: monthly means further than max(spike_mad * MAD, spike_min_dev)
#     from the well's median in the window (wells with >= 3 months), removed
#   - coverage, longest gap (including the start of the window), calendar
#     months and seasons covered
#   - mean, sd, min and max of the monthly means, and the seasonal mean: mean
#     of the 12 calendar-month means, only if all 12 calendar months have data
gwl_window_stats_sql <- function(spec) {
  sprintf(
    "WITH m AS (
  SELECT well_id, CAST(month_index AS INTEGER) AS month_index,
         n_obs, gwl, CAST(last_month AS INTEGER) AS last_month
  FROM monthly
  WHERE month_index IS NOT NULL AND gwl IS NOT NULL
),
centre AS (
  SELECT well_id, median(gwl) AS med, count(*) AS n
  FROM m GROUP BY well_id
),
spread AS (
  SELECT m.well_id, 1.4826 * median(abs(m.gwl - c.med)) AS mad
  FROM m JOIN centre AS c USING (well_id) GROUP BY m.well_id
),
marked AS (
  SELECT m.*,
         c.n >= 3 AND abs(m.gwl - c.med) > greatest(%1$s * s.mad, %2$s) AS spike
  FROM m
  JOIN centre AS c USING (well_id)
  JOIN spread AS s USING (well_id)
),
kept AS (
  SELECT *,
         month_index %% 12 + 1 AS cal_month,
         CASE
           WHEN month_index %% 12 + 1 IN (12, 1, 2) THEN 'DJF'
           WHEN month_index %% 12 + 1 IN (3, 4, 5) THEN 'MAM'
           WHEN month_index %% 12 + 1 IN (6, 7, 8) THEN 'JJA'
           ELSE 'SON'
         END AS season,
         month_index - coalesce(
           lag(month_index) OVER (PARTITION BY well_id ORDER BY month_index),
           last_month - %3$d
         ) - 1 AS gap
  FROM marked
  WHERE NOT spike
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
         CAST(max(gap) AS DOUBLE) AS max_gap_months,
         CAST(count(DISTINCT cal_month) AS DOUBLE) AS n_calendar_months,
         CAST(count(DISTINCT season) AS DOUBLE) AS n_seasons,
         concat_ws(',',
           CASE WHEN bool_or(season = 'DJF') THEN 'DJF' END,
           CASE WHEN bool_or(season = 'MAM') THEN 'MAM' END,
           CASE WHEN bool_or(season = 'JJA') THEN 'JJA' END,
           CASE WHEN bool_or(season = 'SON') THEN 'SON' END
         ) AS seasons,
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
       window_stats.* EXCLUDE (well_id),
       window_stats.n_months_window / %3$d AS coverage_window,
       seasonal.gwl_mean_seasonal
FROM counts
LEFT JOIN spikes USING (well_id)
LEFT JOIN window_stats USING (well_id)
LEFT JOIN seasonal USING (well_id)
ORDER BY well_id",
    sql_number(spec$spike_mad), sql_number(spec$spike_min_dev),
    as.integer(12 * spec$window_years)
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

  recent_from <- as.numeric(format(Sys.Date(), "%Y")) - spec$recent_years
  seasons_needed <- if (isTRUE(spec$require_all_seasons)) 4 else 0

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
      water_table_vs_screen = dplyr::case_when(
        is.na(dtw_mean) | is.na(screen_top) | is.na(screen_bot) ~ NA_character_,
        dtw_mean < pmin(screen_top, screen_bot) ~ "above screen",
        dtw_mean <= pmax(screen_top, screen_bot) ~ "within screen",
        .default = "below screen"
      ),
      recent = as.numeric(format(last_valid_month, "%Y")) >= recent_from,
      usable = !is.na(dtw_mean) &
        !is.na(screen_top) & !is.na(screen_bot) &
        recent %in% TRUE &
        coverage_window >= spec$min_coverage &
        n_seasons >= seasons_needed,
      usable = usable %in% TRUE,
      flags = combine_flags(stats::setNames(
        list(
          !in_meta,
          in_meta & !in_gwl,
          in_gwl & is.na(gwl_mean),
          in_meta & is.na(elev_gs),
          in_meta & (is.na(screen_top) | is.na(screen_bot)),
          n_meta_records > 1,
          screen_top > screen_bot,
          screen_top < 0 | screen_bot < 0,
          n_obs_out_of_range > 0,
          n_obs_implausible_dtw > 0,
          n_months_spike > 0,
          dtw_mean < 0,
          coverage_window < spec$min_coverage,
          n_seasons < 4,
          !recent
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
          "low coverage",
          "season(s) missing",
          paste("no data in the last", spec$recent_years, "years")
        )
      ))
    ) |>
    dplyr::select(
      well_id, usable, in_meta, in_gwl,
      elev_gs, screen_top, screen_bot,
      dtw_mean, dtw_mean_seasonal, dtw_shallowest, dtw_deepest, water_table_vs_screen,
      first_valid_month, last_valid_month, recent,
      n_obs_window, n_months_window, coverage_window, max_gap_months,
      n_calendar_months, n_seasons, seasons,
      gwl_mean, gwl_mean_seasonal, gwl_sd, gwl_min, gwl_max,
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
  has_dtw <- !is.na(m$dtw_mean)
  recent <- m$recent %in% TRUE
  covered <- dplyr::coalesce(m$coverage_window, 0) >= spec$min_coverage
  seasons <- dplyr::coalesce(m$n_seasons, 0) >= if (isTRUE(spec$require_all_seasons)) 4 else 0

  items <- c(
    "wells in metadata" = nrow(m),
    "... with elev_gs" = sum(has_gs),
    "... with screen_top and screen_bot" = sum(has_screen),
    "... with groundwater levels" = sum(m$in_gwl),
    "... with valid groundwater levels" = sum(!is.na(m$gwl_mean)),
    "... with mean depth to groundwater (dtw)" = sum(has_dtw),
    "... + screen depths" = sum(has_dtw & has_screen),
    "... + RECENT" = sum(has_dtw & has_screen & recent),
    "... + COVERAGE" = sum(has_dtw & has_screen & recent & covered),
    "... + SEASONS = usable" = sum(has_dtw & has_screen & recent & covered & seasons),
    "wells with groundwater levels but no metadata" = sum(!wells$in_meta)
  )
  names(items) <- names(items) |>
    sub("RECENT", paste("data in the last", spec$recent_years, "years"), x = _, fixed = TRUE) |>
    sub("COVERAGE", paste0("coverage >= ", 100 * spec$min_coverage, "% of the window"), x = _, fixed = TRUE) |>
    sub("SEASONS", if (isTRUE(spec$require_all_seasons)) "all four seasons" else "no season criterion", x = _, fixed = TRUE)

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
