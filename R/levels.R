# Groundwater levels of the analysis wells: three sources by priority ---------------
#
# For every FARM well the level series is taken from the first source with
# data:
#   1. the state level series (state deliveries and MENTOR, built in the
#      pipeline, R/state_levels.R), linked by state and ID with the ID
#      columns of the mapping table (source "state_levels");
#   2. the linked gw_base well (tbl_gwl_raw);
#   3. the nearest gw_base well with levels within level_config$neighbour_radius_m
#      of the well's coordinates.
# "Data" means at least one level inside gwl_valid_range. The levels of the
# chosen series count as the well's own: the depths use the well's own ground
# elevation. The gw_base levels are checked and averaged per month on the
# server (R/wells.R), the state series here with the same checks.

# The state level series (build_state_levels(), R/state_levels.R) in the
# form used here: key "state ID" with the column of the mapping (source
# state_levels, other_column: messstellen_id or id_alt), date, level; where
# only a depth was delivered, the depth and its reference (GOK: below
# ground; Messpunkt: below the measuring point).
prepare_state_levels <- function(d, mapping) {
  state <- norm_state(d$state)
  m <- id_mapping_for(mapping, "state_levels", state)
  ids <- rep(NA_character_, nrow(d))
  for (col in unique(stats::na.omit(m$other_column))) {
    rows <- which(m$other_column %in% col & m$link == "id")
    if (length(rows) > 0 && col %in% names(d)) ids[rows] <- as.character(d[[col]][rows])
  }
  tibble::tibble(
    key = as.character(state_id_key(state, ids)),
    obs_date = as.Date(d$datum),
    gwl = as.numeric(d$gwl_m_nhn),
    depth = as.numeric(d$depth_m),
    depth_ref = as.character(d$depth_ref),
    source = as.character(d$source)
  ) |>
    dplyr::filter(!is.na(key), !is.na(obs_date))
}

# The state series of the analysis wells: one row per well and measurement,
# the level in m a.s.l. Where only a depth was delivered: level = ground
# elevation - depth (below ground) or measuring point - depth (below the
# measuring point; the FARM measuring point height).
link_state_levels <- function(levels, meta) {
  wells <- meta |>
    dplyr::filter(!is.na(state_levels_key)) |>
    dplyr::distinct(well_id, .keep_all = TRUE) |>
    dplyr::select(well_id, key = state_levels_key, elev_gs, elev_mp)
  dplyr::inner_join(levels, wells, by = "key", relationship = "many-to-many") |>
    dplyr::mutate(
      gwl = dplyr::coalesce(
        gwl,
        dplyr::case_when(
          depth_ref %in% c("GOK", NA) ~ elev_gs - depth,
          depth_ref == "Messpunkt" ~ elev_mp - depth,
          .default = NA_real_
        )
      )
    ) |>
    dplyr::select(well_id, obs_date, gwl, elev_gs, source)
}

# Number of levels inside the valid range per gw_base well (all wells), to
# know which have data. One scan of the level table on the server.
gwbase_level_counts_sql <- function(spec) {
  sprintf(
    "SELECT CAST(%1$s AS nvarchar(100)) AS well_id, COUNT_BIG(*) AS n_valid
FROM %3$s.%4$s
WHERE %2$s IS NOT NULL AND CAST(%2$s AS float) BETWEEN %5$s AND %6$s
GROUP BY CAST(%1$s AS nvarchar(100))",
    sql_ident(spec$gwl_id), sql_ident(spec$gwl_level),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table),
    sql_number(spec$gwl_valid_range[1]), sql_number(spec$gwl_valid_range[2])
  )
}

# The part of the spec the count query uses (it reruns when the level table
# or the valid range changes).
level_count_spec <- function(spec) {
  spec[c("gwl_schema", "gwl_table", "gwl_id", "gwl_level", "gwl_valid_range", "gwl_n_rows")]
}

fetch_gwbase_level_counts <- function(spec) {
  db_query(gwbase_level_counts_sql(spec))
}

# The level series of every analysis well: level_source ("state data",
# "gw_base", "gw_base well within <r> m", "none"), level_id (the gw_base
# well whose levels are used; NA for state data and none) and
# level_distance_m (distance to that well, for the neighbour).
assign_level_sources <- function(meta, state_series, gw_counts, gwbase, crs = NULL,
                                 radius_m = 10, valid_range = c(-Inf, Inf)) {
  wells <- dplyr::distinct(meta, well_id, .keep_all = TRUE)
  with_state <- state_series |>
    dplyr::filter(!is.na(gwl), gwl >= valid_range[1], gwl <= valid_range[2]) |>
    dplyr::distinct(well_id)
  gw_with_data <- gw_counts$well_id[gw_counts$n_valid > 0]
  neighbour_label <- paste0("gw_base well within ", radius_m, " m")

  out <- tibble::tibble(
    well_id = wells$well_id,
    level_source = dplyr::case_when(
      wells$well_id %in% with_state$well_id ~ "state data",
      wells$in_gw_base & wells$well_id %in% gw_with_data ~ "gw_base",
      .default = NA_character_
    ),
    level_id = ifelse(level_source %in% "gw_base", wells$well_id, NA_character_),
    level_distance_m = ifelse(level_source %in% "gw_base", 0, NA_real_)
  )

  # third option: the nearest gw_base well with levels within radius_m
  todo <- which(is.na(out$level_source) & !is.na(wells$x) & !is.na(wells$y))
  xy <- coords_to_25832(gwbase$x, gwbase$y, crs %||% NA)
  cand <- tibble::tibble(well_id = gwbase$well_id, x = xy$x, y = xy$y) |>
    dplyr::filter(well_id %in% gw_with_data, !is.na(x), !is.na(y)) |>
    dplyr::distinct(well_id, .keep_all = TRUE)
  if (length(todo) > 0 && nrow(cand) > 0) {
    from <- sf::st_as_sf(wells[todo, c("x", "y")], coords = c("x", "y"), crs = 25832)
    to <- sf::st_as_sf(cand, coords = c("x", "y"), crs = 25832)
    nearest <- sf::st_nearest_feature(from, to)
    dist <- sqrt((wells$x[todo] - cand$x[nearest])^2 + (wells$y[todo] - cand$y[nearest])^2)
    near <- dist <= radius_m
    out$level_source[todo[near]] <- neighbour_label
    out$level_id[todo[near]] <- cand$well_id[nearest[near]]
    out$level_distance_m[todo[near]] <- dist[near]
  }
  out$level_source <- dplyr::coalesce(out$level_source, "none")
  out
}

# The gw_base wells whose levels are read on the server, with the ground
# elevation of the analysis well they serve (for the depth check): one row
# per gw_base well (the first analysis well where several share one).
well_elevations <- function(meta, sources) {
  sources |>
    dplyr::filter(!is.na(level_id)) |>
    dplyr::left_join(dplyr::distinct(meta, well_id, .keep_all = TRUE) |> dplyr::select(well_id, elev_gs),
                     by = "well_id") |>
    dplyr::distinct(level_id, .keep_all = TRUE) |>
    dplyr::transmute(well_id = level_id, elev_gs) |>
    dplyr::arrange(well_id)
}

# The checks of single measurements (as on the server, checked_measurements_cte()):
# 0 valid, 1 empty, 2 out of the valid level range, 3 implausible depth.
check_levels <- function(gwl, elev_gs, spec) {
  depth <- elev_gs - gwl
  dplyr::case_when(
    is.na(gwl) ~ 1L,
    gwl < spec$gwl_valid_range[1] | gwl > spec$gwl_valid_range[2] ~ 2L,
    !is.na(depth) & (depth < spec$dtw_valid_range[1] | depth > spec$dtw_valid_range[2]) ~ 3L,
    .default = 0L
  )
}

# Monthly means of the state series in the form of gwl_monthly_sql(): the
# valid levels averaged per well and month within the recent window, the
# check counts per well, and for wells without a valid level one row with an
# empty month.
state_monthly <- function(series, spec) {
  if (nrow(series) == 0) {
    return(tibble::tibble(
      well_id = character(), month_index = numeric(), n_obs = numeric(), gwl = numeric(),
      n_obs_valid = numeric(), n_obs_empty = numeric(), n_obs_out_of_range = numeric(),
      n_obs_implausible_dtw = numeric(), first_month = numeric(), last_month = numeric()
    ))
  }
  monthly <- series |>
    dplyr::mutate(
      month_index = as.integer(format(obs_date, "%Y")) * 12L + as.integer(format(obs_date, "%m")) - 1L,
      check_code = check_levels(gwl, elev_gs, spec)
    ) |>
    dplyr::summarise(
      n_valid = sum(check_code == 0L), n_empty = sum(check_code == 1L),
      n_out_of_range = sum(check_code == 2L), n_implausible_dtw = sum(check_code == 3L),
      gwl = if (any(check_code == 0L)) mean(gwl[check_code == 0L]) else NA_real_,
      .by = c(well_id, month_index)
    ) |>
    dplyr::mutate(
      n_obs_valid = sum(n_valid), n_obs_empty = sum(n_empty),
      n_obs_out_of_range = sum(n_out_of_range), n_obs_implausible_dtw = sum(n_implausible_dtw),
      first_month = suppressWarnings(min(month_index[n_valid > 0])),
      last_month = suppressWarnings(max(month_index[n_valid > 0])),
      latest = month_index == max(month_index),
      .by = well_id
    ) |>
    dplyr::mutate(
      first_month = ifelse(is.finite(first_month), first_month, NA_real_),
      last_month = ifelse(is.finite(last_month), last_month, NA_real_)
    )
  window_start <- spec$reference_month - 12L * spec$recent_years
  monthly |>
    dplyr::filter((n_valid > 0 & month_index > window_start) | latest) |>
    dplyr::mutate(
      month_index = ifelse(is.na(last_month), NA_real_, month_index),
      n_obs = ifelse(is.na(last_month), NA_real_, n_valid)
    ) |>
    dplyr::distinct() |>
    dplyr::transmute(
      well_id, month_index = as.numeric(month_index), n_obs = as.numeric(n_obs), gwl,
      n_obs_valid = as.numeric(n_obs_valid), n_obs_empty = as.numeric(n_obs_empty),
      n_obs_out_of_range = as.numeric(n_obs_out_of_range),
      n_obs_implausible_dtw = as.numeric(n_obs_implausible_dtw),
      first_month = as.numeric(first_month), last_month = as.numeric(last_month)
    )
}

# The monthly means of all analysis wells under their own well_id: gw_base
# series (fetched under the gw_base well, level_id) and state series.
combine_monthly <- function(gwbase_monthly, state_monthly, sources) {
  gw <- sources |>
    dplyr::filter(!is.na(level_id)) |>
    dplyr::select(well_id, level_id) |>
    dplyr::inner_join(dplyr::rename(gwbase_monthly, level_id = well_id), by = "level_id",
                      relationship = "many-to-many") |>
    dplyr::select(-level_id)
  st <- dplyr::filter(state_monthly, well_id %in% sources$well_id[sources$level_source == "state data"])
  dplyr::bind_rows(gw, st) |>
    dplyr::arrange(well_id, month_index)
}

# Valid months per season and year (as year_seasons_sql()) of the state
# series, and those of the gw_base series under the analysis well's own ID.
combine_year_seasons <- function(gwbase_seasons, series, sources, spec, settings) {
  gw <- sources |>
    dplyr::filter(!is.na(level_id)) |>
    dplyr::select(well_id, level_id) |>
    dplyr::inner_join(dplyr::rename(gwbase_seasons, level_id = well_id), by = "level_id",
                      relationship = "many-to-many") |>
    dplyr::select(-level_id)
  first_year <- spec$reference_month %/% 12 - settings$history_years + 1
  st <- series |>
    dplyr::filter(well_id %in% sources$well_id[sources$level_source == "state data"]) |>
    dplyr::mutate(
      check_code = check_levels(gwl, elev_gs, spec),
      obs_year = as.integer(format(obs_date, "%Y")),
      month = as.integer(format(obs_date, "%m"))
    ) |>
    dplyr::filter(check_code == 0L, obs_year >= first_year) |>
    dplyr::summarise(
      n_djf = dplyr::n_distinct(month[month %in% c(12, 1, 2)]),
      n_mam = dplyr::n_distinct(month[month %in% 3:5]),
      n_jja = dplyr::n_distinct(month[month %in% 6:8]),
      n_son = dplyr::n_distinct(month[month %in% 9:11]),
      .by = c(well_id, obs_year)
    )
  dplyr::bind_rows(gw, st)
}

# The analysis table with the level source of each well.
add_level_sources <- function(meta, sources) {
  dplyr::left_join(meta, sources, by = "well_id")
}
