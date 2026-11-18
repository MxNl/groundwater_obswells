# Groundwater levels of the analysis wells: three sources by priority ---------------
#
# For every FARM well the level series is taken from the first source with
# data:
#   1. the state level series (state deliveries, built in the
#      pipeline, R/state_levels.R), linked by state and ID with the ID
#      columns of the mapping table (source "state_levels");
#   2. the linked gw_base well (tbl_gwl_raw);
#   3. the nearest gw_base well with levels within level_config$neighbour_radius_m
#      of the well's coordinates.
# Of these, the first that is usable over time is used (data in the recent
# window, every season in enough years); if none is, the first with at
# least one level inside gwl_valid_range (choose_level_sources()). The levels of the
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

# The candidate level series of every analysis well, in the order of
# priority: 1 the state series (if it has at least one level inside
# valid_range), 2 the linked gw_base well (if it has such levels), 3 the
# nearest other gw_base well with such levels within radius_m. Columns
# well_id, priority, level_source, level_id (gw_base well; NA for state
# data), level_distance_m, cand_id (the series' ID in the candidate tables).
level_candidates <- function(meta, state_series, gw_counts, gwbase, crs = NULL,
                             radius_m = 10, valid_range = c(-Inf, Inf)) {
  wells <- dplyr::distinct(meta, well_id, .keep_all = TRUE)
  with_state <- state_series |>
    dplyr::filter(!is.na(gwl), gwl >= valid_range[1], gwl <= valid_range[2]) |>
    dplyr::distinct(well_id)
  gw_with_data <- gw_counts$well_id[gw_counts$n_valid > 0]
  state <- tibble::tibble(well_id = intersect(wells$well_id, with_state$well_id), priority = 1L,
                          level_source = "state data", level_id = NA_character_, level_distance_m = NA_real_)
  own <- wells |>
    dplyr::filter(in_gw_base, well_id %in% gw_with_data) |>
    dplyr::transmute(well_id, priority = 2L, level_source = "gw_base", level_id = well_id, level_distance_m = 0)
  # the nearest other gw_base well with levels within radius_m
  near <- NULL
  xy <- coords_to_25832(gwbase$x, gwbase$y, crs %||% NA)
  cand <- tibble::tibble(well_id = gwbase$well_id, x = xy$x, y = xy$y) |>
    dplyr::filter(well_id %in% gw_with_data, !is.na(x), !is.na(y)) |>
    dplyr::distinct(well_id, .keep_all = TRUE)
  located <- which(!is.na(wells$x) & !is.na(wells$y))
  if (length(located) > 0 && nrow(cand) > 0) {
    from <- sf::st_as_sf(wells[located, c("x", "y")], coords = c("x", "y"), crs = 25832)
    to <- sf::st_as_sf(cand, coords = c("x", "y"), crs = 25832)
    hits <- sf::st_is_within_distance(from, to, dist = radius_m)
    near <- dplyr::bind_rows(lapply(seq_along(located), function(k) {
      h <- hits[[k]]
      h <- h[cand$well_id[h] != wells$well_id[located[k]]]
      if (length(h) == 0) return(NULL)
      d <- sqrt((wells$x[located[k]] - cand$x[h])^2 + (wells$y[located[k]] - cand$y[h])^2)
      tibble::tibble(well_id = wells$well_id[located[k]], level_id = cand$well_id[h[which.min(d)]],
                     level_distance_m = min(d))
    }))
    if (nrow(near) > 0) {
      near <- dplyr::mutate(near, priority = 3L, level_source = paste0("gw_base well within ", radius_m, " m"))
    }
  }
  dplyr::bind_rows(state, own, near) |>
    dplyr::mutate(cand_id = ifelse(level_source == "state data", paste0("state:", well_id), level_id)) |>
    dplyr::arrange(well_id, priority)
}

# The gw_base wells whose levels are read on the server (all gw_base
# candidates), with the ground elevation of the analysis well they serve
# (for the depth check): one row per gw_base well (the first analysis well
# where several share one).
well_elevations <- function(meta, candidates) {
  candidates |>
    dplyr::filter(!is.na(level_id)) |>
    dplyr::left_join(dplyr::distinct(meta, well_id, .keep_all = TRUE) |> dplyr::select(well_id, elev_gs),
                     by = "well_id") |>
    dplyr::distinct(level_id, .keep_all = TRUE) |>
    dplyr::transmute(well_id = level_id, elev_gs) |>
    dplyr::arrange(well_id)
}

# The monthly means of every candidate series under its cand_id (as
# well_id, so the window statistics can be computed for all of them).
candidate_monthly <- function(gwbase_monthly, state_monthly, candidates) {
  st <- state_monthly |>
    dplyr::filter(well_id %in% candidates$well_id[candidates$level_source == "state data"]) |>
    dplyr::mutate(well_id = paste0("state:", well_id))
  gw <- dplyr::filter(gwbase_monthly, well_id %in% candidates$cand_id)
  dplyr::bind_rows(gw, st)
}

# The series used per well: of its candidates, the first in the order of
# priority that is usable over time (data in the recent window and every
# season in at least min_years_per_season years); if none is, the first
# with valid levels; else none. level_choice says whether a series of
# higher priority was passed over. Returns well_id, level_source, level_id,
# level_distance_m, level_choice for every well of `meta`.
choose_level_sources <- function(meta, candidates, stats, spec) {
  scored <- candidates |>
    dplyr::left_join(dplyr::select(stats, cand_id = well_id, n_obs_valid, n_months_recent, min_years_per_season),
                     by = "cand_id") |>
    dplyr::mutate(
      valid = dplyr::coalesce(n_obs_valid, 0) > 0,
      usable = valid & dplyr::coalesce(n_months_recent, 0) > 0 &
        dplyr::coalesce(min_years_per_season, 0) >= spec$min_years_per_season
    )
  chosen <- scored |>
    dplyr::filter(valid) |>
    dplyr::arrange(well_id, dplyr::desc(usable), priority) |>
    dplyr::mutate(first_priority = min(priority), .by = well_id) |>
    dplyr::slice_head(n = 1, by = well_id) |>
    dplyr::mutate(level_choice = dplyr::case_when(
      priority == first_priority ~ "first source with data",
      usable ~ "higher-priority series not usable over time",
      .default = "first source with data"
    ))
  tibble::tibble(well_id = unique(meta$well_id)) |>
    dplyr::left_join(dplyr::select(chosen, well_id, level_source, level_id, level_distance_m, level_choice),
                     by = "well_id") |>
    dplyr::mutate(level_source = dplyr::coalesce(level_source, "none"))
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
