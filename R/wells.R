# Depth to groundwater per well --------------------------------------------------
#
# For every well: the screen depths from the metadata and the mean depth to
# the groundwater table below ground surface, from the groundwater levels
# (m above sea level) of the last years with data:
#
#   1. The levels are averaged per month on the server, over the last
#      `window_years` * 12 calendar months up to the month of the well's last
#      measurement. Monthly means
#      first, so periods with dense logger data do not outweigh periods with
#      monthly manual readings.
#   2. The mean of these monthly means is the mean groundwater level.
#   3. Depth below ground surface = elev_gs - mean level. Where elev_gs is
#      missing but elev_mp is given, the ground surface is estimated as
#      elev_mp minus the median stick-up (elev_mp - elev_gs) of the wells
#      that have both; dtw_method says which way was used.

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
    c(config$meta_id, config$elev_mp, config$elev_gs, config$screen_top, config$screen_bot)
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
            CAST(%s AS float) AS elev_mp,
            CAST(%s AS float) AS elev_gs,
            CAST(%s AS float) AS screen_top,
            CAST(%s AS float) AS screen_bot
     FROM %s.%s",
    sql_ident(spec$meta_id), sql_ident(spec$elev_mp), sql_ident(spec$elev_gs),
    sql_ident(spec$screen_top), sql_ident(spec$screen_bot),
    sql_ident(spec$meta_schema), sql_ident(spec$meta_table)
  )
  db_query(sql)
}

gwl_stats_sql <- function(spec) {
  sprintf(
    "WITH ts AS (
  SELECT CAST(%1$s AS nvarchar(100)) AS well_id,
         CAST(%2$s AS date) AS obs_date,
         CAST(%3$s AS float) AS gwl
  FROM %4$s.%5$s
  WHERE %1$s IS NOT NULL AND %2$s IS NOT NULL AND %3$s IS NOT NULL
),
span AS (
  SELECT well_id,
         MIN(obs_date) AS first_date,
         MAX(obs_date) AS last_date,
         COUNT_BIG(*) AS n_obs_total
  FROM ts
  GROUP BY well_id
),
monthly AS (
  SELECT ts.well_id,
         YEAR(ts.obs_date) AS obs_year,
         MONTH(ts.obs_date) AS obs_month,
         AVG(ts.gwl) AS gwl,
         COUNT_BIG(*) AS n_obs
  FROM ts
  JOIN span ON span.well_id = ts.well_id
  -- the last window_years * 12 calendar months, including the last month
  WHERE ts.obs_date >= DATEADD(
    month, -(12 * %6$d - 1),
    DATEFROMPARTS(YEAR(span.last_date), MONTH(span.last_date), 1)
  )
  GROUP BY ts.well_id, YEAR(ts.obs_date), MONTH(ts.obs_date)
)
SELECT span.well_id,
       span.first_date,
       span.last_date,
       CAST(span.n_obs_total AS float) AS n_obs_total,
       CAST(SUM(m.n_obs) AS float) AS n_obs_window,
       CAST(COUNT_BIG(*) AS float) AS n_months_window,
       CAST(COUNT_BIG(DISTINCT m.obs_year) AS float) AS n_years_window,
       AVG(m.gwl) AS gwl_mean,
       STDEV(m.gwl) AS gwl_sd,
       MIN(m.gwl) AS gwl_min,
       MAX(m.gwl) AS gwl_max
FROM span
JOIN monthly AS m ON m.well_id = span.well_id
GROUP BY span.well_id, span.first_date, span.last_date, span.n_obs_total",
    sql_ident(spec$gwl_id), sql_ident(spec$gwl_date), sql_ident(spec$gwl_level),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table),
    as.integer(spec$window_years)
  )
}

# One row per well with groundwater levels: record span and the statistics of
# the monthly mean levels in the last `window_years` years. Computed entirely
# on the server, only one row per well is transferred.
fetch_gwl_stats <- function(spec) {
  db_query(gwl_stats_sql(spec)) |>
    dplyr::mutate(
      first_date = as.Date(first_date),
      last_date = as.Date(last_date)
    )
}

# Combines a named list of logical conditions into one text per row, e.g.
# "stick-up implausible; negative screen depth". NA where none holds.
combine_flags <- function(conds) {
  hits <- lapply(conds, function(x) !is.na(x) & x)
  out <- rep(NA_character_, length(hits[[1]]))
  for (name in names(hits)) {
    hit <- hits[[name]]
    out[hit] <- ifelse(is.na(out[hit]), name, paste(out[hit], name, sep = "; "))
  }
  out
}

# Plausible range of the stick-up (elev_mp - elev_gs) in m, for the median
# used to estimate missing ground surface elevations and for the flags.
stickup_range <- c(-0.5, 3)

# One row per well (in the metadata, in the time series or both): screen
# depths, mean depth to groundwater below ground surface and quality flags.
assess_well_dtw <- function(meta, gwl_stats, recent_years = 10) {
  meta <- meta |>
    dplyr::mutate(n_meta_records = dplyr::n(), .by = well_id) |>
    dplyr::distinct(well_id, .keep_all = TRUE)

  stickup <- meta$elev_mp - meta$elev_gs
  stickup <- stickup[!is.na(stickup) & stickup >= stickup_range[1] & stickup <= stickup_range[2]]
  stickup_median <- if (length(stickup) > 0) stats::median(stickup) else NA_real_

  recent_from <- as.numeric(format(Sys.Date(), "%Y")) - recent_years

  if (!is.na(stickup_median)) {
    message("Median stick-up (elev_mp - elev_gs): ", round(stickup_median, 2), " m")
  }

  dplyr::full_join(
    dplyr::mutate(meta, in_meta = TRUE),
    dplyr::mutate(gwl_stats, in_gwl = TRUE),
    by = "well_id"
  ) |>
    dplyr::mutate(
      in_meta = !is.na(in_meta),
      in_gwl = !is.na(in_gwl),
      stickup = elev_mp - elev_gs,
      elev_gs_used = dplyr::coalesce(elev_gs, elev_mp - stickup_median),
      dtw_method = dplyr::case_when(
        is.na(gwl_mean) ~ NA_character_,
        !is.na(elev_gs) ~ "elev_gs",
        !is.na(elev_mp) & !is.na(stickup_median) ~ "elev_mp - median stick-up"
      ),
      # depth below ground surface, m (positive = below ground)
      dtw_mean = elev_gs_used - gwl_mean,
      dtw_shallowest = elev_gs_used - gwl_max,
      dtw_deepest = elev_gs_used - gwl_min,
      water_table_vs_screen = dplyr::case_when(
        is.na(dtw_mean) | is.na(screen_top) | is.na(screen_bot) ~ NA_character_,
        dtw_mean < pmin(screen_top, screen_bot) ~ "above screen",
        dtw_mean <= pmax(screen_top, screen_bot) ~ "within screen",
        .default = "below screen"
      ),
      flags = combine_flags(stats::setNames(
        list(
          !in_meta,
          in_meta & !in_gwl,
          n_meta_records > 1,
          stickup < stickup_range[1] | stickup > stickup_range[2],
          screen_top > screen_bot,
          screen_top < 0 | screen_bot < 0,
          dtw_mean < 0,
          n_months_window < 12,
          as.numeric(format(last_date, "%Y")) < recent_from
        ),
        c(
          "no metadata",
          "no groundwater levels",
          "duplicate metadata records",
          "stick-up implausible",
          "screen_top deeper than screen_bot",
          "negative screen depth",
          "mean level above ground",
          "fewer than 12 months in window",
          paste("no data in the last", recent_years, "years")
        )
      ))
    ) |>
    dplyr::select(
      well_id, in_meta, in_gwl,
      elev_mp, elev_gs, stickup, elev_gs_used, screen_top, screen_bot,
      first_date, last_date, n_obs_total, n_obs_window, n_months_window, n_years_window,
      gwl_mean, gwl_sd, gwl_min, gwl_max,
      dtw_method, dtw_mean, dtw_shallowest, dtw_deepest,
      water_table_vs_screen, flags
    ) |>
    dplyr::arrange(well_id)
}

# How far the data go: counts of wells for each step towards a mean depth to
# groundwater and screen depths, as numbers and shares of the metadata wells.
summarise_well_availability <- function(wells, recent_years = 10) {
  m <- dplyr::filter(wells, in_meta)
  recent_from <- as.numeric(format(Sys.Date(), "%Y")) - recent_years
  has_dtw <- !is.na(m$dtw_mean)
  n_months <- dplyr::coalesce(m$n_months_window, 0)
  has_screen <- !is.na(m$screen_top) & !is.na(m$screen_bot)
  recent <- !is.na(m$last_date) & as.numeric(format(m$last_date, "%Y")) >= recent_from

  recent_label <- paste("data in the last", recent_years, "years")
  items <- c(
    "wells in metadata" = nrow(m),
    "... with duplicate metadata records" = sum(grepl("duplicate", m$flags)),
    "... with elev_gs" = sum(!is.na(m$elev_gs)),
    "... with elev_mp" = sum(!is.na(m$elev_mp)),
    "... with elev_gs and elev_mp" = sum(!is.na(m$elev_gs) & !is.na(m$elev_mp)),
    "... with neither elev_gs nor elev_mp" = sum(is.na(m$elev_gs) & is.na(m$elev_mp)),
    "... with screen_top and screen_bot" = sum(has_screen),
    "... with groundwater levels" = sum(m$in_gwl),
    "... with mean depth to groundwater (dtw)" = sum(has_dtw),
    "... dtw from elev_gs" = sum(m$dtw_method %in% "elev_gs"),
    "... dtw from elev_mp - median stick-up" = sum(m$dtw_method %in% "elev_mp - median stick-up"),
    "... dtw and screen depths" = sum(has_dtw & has_screen),
    "... dtw, screen depths and RECENT" = sum(has_dtw & has_screen & recent),
    "... as above, at least 12 months in window" = sum(has_dtw & has_screen & recent & n_months >= 12),
    "wells with groundwater levels but no metadata" = sum(!wells$in_meta)
  )
  names(items) <- sub("RECENT", recent_label, names(items), fixed = TRUE)
  tibble::tibble(
    item = names(items),
    n_wells = unname(items),
    pct_of_meta = round(100 * unname(items) / max(nrow(m), 1), 1)
  ) |>
    dplyr::mutate(pct_of_meta = dplyr::if_else(dplyr::row_number() == dplyr::n(), NA, pct_of_meta))
}
