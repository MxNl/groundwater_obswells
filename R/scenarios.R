# Scenarios for selecting wells --------------------------------------------------
#
# A scenario filters the usable wells (see assess_well_dtw()) by five
# criteria, all depths in m below ground surface, applied in this order:
#   1. dtw_mean: mean depth to groundwater between dtw_mean_min and
#      dtw_mean_max
#   2. dtw_max: maximum depth to groundwater (dtw_deepest, deepest monthly
#      mean in the mean window) less than dtw_max
#   3. screen_top: position of the screen top relative to the mean water
#      table, by screen_rule:
#      - "below": below the mean water table, at most top_below_max m deeper
#        (dtw_mean <= screen_top <= dtw_mean + top_below_max)
#      - "spans": above the mean water table (screen_top <= dtw_mean), so the
#        screen reaches across it
#   4. not_dry: the screen bottom is deeper than the typical annual low
#      (screen_bot >= dtw_annual_low), so the well does not fall dry in the
#      annual cycle. dtw_annual_low = elev_gs - the lowest monthly mean level
#      of each year of the mean window, averaged over the years
#   5. screen_length: screen length (screen_bot - screen_top) above 0 and up
#      to screen_length_max

`%||%` <- function(x, y) if (is.null(x)) y else x

scenario_criteria <- c(
  dtw_mean = "mean depth to groundwater",
  dtw_max = "maximum depth to groundwater",
  screen_top = "screen top position",
  not_dry = "screen bottom below the annual low",
  screen_length = "screen length"
)

# Whether each well meets each criterion of scenario `s` (one row of the
# scenarios table); `base` are the usable wells.
scenario_checks <- function(base, s) {
  screen_top <- switch(
    s$screen_rule,
    below = base$screen_top >= base$dtw_mean &
      base$screen_top <= base$dtw_mean + s$top_below_max,
    spans = base$screen_top <= base$dtw_mean,
    stop("Unknown screen_rule: ", s$screen_rule, call. = FALSE)
  )
  tibble::tibble(
    dtw_mean = base$dtw_mean >= s$dtw_mean_min & base$dtw_mean <= s$dtw_mean_max,
    dtw_max = base$dtw_deepest < s$dtw_max,
    screen_top = screen_top,
    not_dry = base$screen_bot >= base$dtw_annual_low,
    screen_length = base$screen_length > 0 & base$screen_length <= s$screen_length_max
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) x %in% TRUE))
}

# One row per usable well and scenario: whether it meets each criterion and
# all of them (selected).
evaluate_scenarios <- function(wells, scenarios) {
  base <- dplyr::filter(wells, usable)
  out <- lapply(seq_len(nrow(scenarios)), function(i) {
    s <- scenarios[i, ]
    checks <- scenario_checks(base, s)
    tibble::tibble(scenario_id = s$id, well_id = base$well_id) |>
      dplyr::bind_cols(checks) |>
      dplyr::mutate(selected = dtw_mean & dtw_max & screen_top & not_dry & screen_length)
  })
  dplyr::bind_rows(out)
}

# Wells left after each criterion, applied one after the other, per scenario.
count_scenario_steps <- function(scenario_wells, scenarios) {
  # cumulative criteria first: summarise() would see the sums under the
  # same names
  scenario_wells |>
    dplyr::mutate(
      step_1 = dtw_mean,
      step_2 = step_1 & dtw_max,
      step_3 = step_2 & screen_top,
      step_4 = step_3 & not_dry,
      step_5 = selected
    ) |>
    dplyr::summarise(
      usable = dplyr::n(),
      dtw_mean = sum(step_1),
      dtw_max = sum(step_2),
      screen_top = sum(step_3),
      not_dry = sum(step_4),
      screen_length = sum(step_5),
      .by = scenario_id
    ) |>
    dplyr::left_join(dplyr::select(scenarios, id, name), by = c(scenario_id = "id")) |>
    dplyr::relocate(name, .after = scenario_id)
}

# Coordinates --------------------------------------------------------------------

# Guesses the CRS of each well's coordinates from their ranges (German
# systems): WGS84 lon/lat, UTM 32N/33N with zone prefix (32xxxxxx), Gauss-
# Krueger zones 2-5 (first digit of the easting), and plain 6-digit UTM
# eastings, assumed to be zone 32N. Returns EPSG codes, NA where none fits.
guess_crs <- function(x, y) {
  dplyr::case_when(
    is.na(x) | is.na(y) ~ NA_integer_,
    abs(x) <= 180 & abs(y) <= 90 ~ 4326L,
    x >= 32e6 & x < 33e6 & y >= 5e6 & y < 6.2e6 ~ 25832L,
    x >= 33e6 & x < 34e6 & y >= 5e6 & y < 6.2e6 ~ 25833L,
    x >= 2e6 & x < 6e6 & y >= 5e6 & y < 6.2e6 ~ 31466L + as.integer(x %/% 1e6) - 2L,
    x >= 1e5 & x < 1e6 & y >= 5e6 & y < 6.2e6 ~ 25832L,
    .default = NA_integer_
  )
}

# The wells as points in `crs_target`. With `crs` NULL, the CRS of each well
# is guessed (guess_crs()) and reported; wells without coordinates are
# dropped. Columns crs_source (EPSG) and X, Y (in crs_target).
wells_to_points <- function(wells, crs = NULL, crs_target = 25832) {
  wells <- dplyr::filter(wells, !is.na(x), !is.na(y))
  wells$crs_source <- if (is.null(crs)) guess_crs(wells$x, wells$y) else as.integer(crs)
  if (is.null(crs)) {
    guessed <- table(wells$crs_source, useNA = "ifany")
    message(
      "Coordinate systems guessed from the value ranges (EPSG: wells): ",
      paste(names(guessed), guessed, sep = ": ", collapse = ", "),
      ". Set crs in well_config if this is wrong."
    )
  }
  wells <- dplyr::filter(wells, !is.na(crs_source))
  # zone prefix of UTM eastings (32xxxxxx) removed
  prefixed <- wells$x >= 32e6 & wells$x < 34e6
  wells$x[prefixed] <- wells$x[prefixed] %% 1e6

  parts <- lapply(split(wells, wells$crs_source), function(part) {
    points <- sf::st_as_sf(part, coords = c("x", "y"), crs = part$crs_source[1], remove = FALSE)
    sf::st_transform(points, crs_target)
  })
  points <- do.call(rbind, unname(parts))
  xy <- sf::st_coordinates(points)
  points$X <- xy[, 1]
  points$Y <- xy[, 2]
  sf::st_drop_geometry(points) |>
    tibble::as_tibble() |>
    dplyr::arrange(well_id)
}

# One scenario per usable well, so the wells can be split into groups that do
# not overlap (scenarios do overlap: FARM strict is part of FARM tolerant):
# the first scenario in the order of the scenarios table that the well
# meets, or "none". Returns well_id, scenario_id and the group label.
assign_scenario_groups <- function(scenario_wells, scenarios) {
  order <- match(scenario_wells$scenario_id, scenarios$id)
  first <- scenario_wells |>
    dplyr::mutate(order = order) |>
    dplyr::filter(selected) |>
    dplyr::slice_min(order, n = 1, by = well_id, with_ties = FALSE)
  dplyr::distinct(scenario_wells, well_id) |>
    dplyr::left_join(dplyr::select(first, well_id, scenario_id), by = "well_id") |>
    dplyr::mutate(scenario_id = dplyr::coalesce(scenario_id, "none"))
}

# Scenario for an even coverage of Germany ----------------------------------------

# Grid cell of each point (X, Y in m, EPSG 25832), as one integer id.
grid_cell <- function(x, y, cell_m) {
  col <- floor(x / cell_m)
  row <- floor(y / cell_m)
  as.integer(row * 10000 + col)
}

# A scenario fitted to the data so the selected wells cover Germany as evenly
# as possible, also the south with its deeper groundwater, while staying as
# shallow and strict as possible. Germany is divided into square cells of
# cell_km; "coverage" is the number of cells with at least one selected well.
# Each of the four thresholds has a grid from strict to loose
# (settings$grids: dtw_mean_max, dtw_max_extra (maximum depth = dtw_mean_max
# + dtw_max_extra), top_below_max, screen_length_max). The reference is the
# coverage of the loosest combination; candidates are all combinations that
# reach at least keep_share of it; of those, the strictest wins: each
# threshold scaled to 0 (strictest) .. 1 (loosest) on its grid, weighted
# (settings$weights, default 1 each) and summed, the smallest sum (ties: more
# cells, then more wells). Screen top below the water table and "not falling
# dry" stay as in the other scenarios. `points` are the wells with X, Y
# (well_points). Returns the scenario (one row as in the scenarios table),
# the profiles (cells and wells along each grid, the other thresholds at the
# chosen values), the cells and the reference.
optimise_coverage_scenario <- function(points, settings) {
  base <- dplyr::filter(points, usable)
  cell_m <- settings$cell_km * 1000
  g <- lapply(settings$grids, sort)
  # criteria that do not depend on the thresholds
  fixed <- base$dtw_mean >= settings$dtw_mean_min &
    base$screen_top >= base$dtw_mean &
    base$screen_bot >= base$dtw_annual_low &
    base$screen_length > 0
  base <- base[fixed %in% TRUE, ]
  cell <- grid_cell(base$X, base$Y, cell_m)
  top_rel <- base$screen_top - base$dtw_mean

  select <- function(l, e, t, s) {
    base$dtw_mean <= l & base$dtw_deepest < l + e & top_rel <= t & base$screen_length <= s
  }
  measure <- function(l, e, t, s) {
    keep <- select(l, e, t, s) %in% TRUE
    c(cells = length(unique(cell[keep])), wells = sum(keep))
  }
  combos <- expand.grid(
    dtw_mean_max = g$dtw_mean_max, dtw_max_extra = g$dtw_max_extra,
    top_below_max = g$top_below_max, screen_length_max = g$screen_length_max
  )
  m <- mapply(measure, combos$dtw_mean_max, combos$dtw_max_extra,
              combos$top_below_max, combos$screen_length_max)
  combos$cells <- m["cells", ]
  combos$wells <- m["wells", ]

  scaled <- function(x, grid) (match(x, grid) - 1) / max(length(grid) - 1, 1)
  w <- utils::modifyList(
    list(dtw_mean_max = 1, dtw_max_extra = 1, top_below_max = 1, screen_length_max = 1),
    as.list(settings$weights %||% list())
  )
  combos$looseness <- w$dtw_mean_max * scaled(combos$dtw_mean_max, g$dtw_mean_max) +
    w$dtw_max_extra * scaled(combos$dtw_max_extra, g$dtw_max_extra) +
    w$top_below_max * scaled(combos$top_below_max, g$top_below_max) +
    w$screen_length_max * scaled(combos$screen_length_max, g$screen_length_max)

  loosest <- measure(max(g$dtw_mean_max), max(g$dtw_max_extra), max(g$top_below_max), max(g$screen_length_max))
  cells_ref <- max(loosest[["cells"]], 1)
  candidates <- combos[combos$cells >= settings$keep_share * cells_ref, ]
  best <- candidates[order(candidates$looseness, -candidates$cells, -candidates$wells), ][1, ]

  # profiles: each threshold along its grid, the others at the chosen values
  profile <- function(name) {
    args <- best[c("dtw_mean_max", "dtw_max_extra", "top_below_max", "screen_length_max")]
    res <- vapply(g[[name]], function(v) {
      args[[name]] <- v
      measure(args$dtw_mean_max, args$dtw_max_extra, args$top_below_max, args$screen_length_max)
    }, numeric(2))
    tibble::tibble(
      threshold = name, value = g[[name]], cells = res["cells", ], wells = res["wells", ],
      chosen = g[[name]] == best[[name]]
    )
  }
  profiles <- dplyr::bind_rows(lapply(names(g), profile))

  # cells: all cells with usable wells (before the fixed criteria) and
  # whether the chosen thresholds cover them
  all_usable <- dplyr::filter(points, usable)
  chosen_keep <- select(best$dtw_mean_max, best$dtw_max_extra, best$top_below_max, best$screen_length_max) %in% TRUE
  cells <- tibble::tibble(cell = grid_cell(all_usable$X, all_usable$Y, cell_m)) |>
    dplyr::count(cell, name = "usable") |>
    dplyr::left_join(
      tibble::tibble(cell = cell[chosen_keep]) |> dplyr::count(cell, name = "selected"),
      by = "cell"
    ) |>
    dplyr::mutate(
      selected = dplyr::coalesce(selected, 0L),
      xmin = (cell %% 10000) * cell_m, ymin = (cell %/% 10000) * cell_m,
      xmax = xmin + cell_m, ymax = ymin + cell_m
    )

  num <- function(x) format(x, decimal.mark = ".", trim = TRUE)
  scenario <- tibble::tibble(
    id = settings$id,
    name = settings$name,
    dtw_mean_min = settings$dtw_mean_min,
    dtw_mean_max = best$dtw_mean_max,
    dtw_max = best$dtw_mean_max + best$dtw_max_extra,
    screen_rule = "below",
    top_below_max = best$top_below_max,
    screen_length_max = best$screen_length_max,
    colour = settings$colour,
    purpose = paste0(
      "Fitted to the data for wells across all of Germany: the strictest ",
      "thresholds whose wells still cover ", round(100 * best$cells / cells_ref),
      " % of the ", settings$cell_km, " km cells the loosest thresholds cover ",
      "(mean depth up to ", num(best$dtw_mean_max), " m, maximum depth < ",
      num(best$dtw_mean_max + best$dtw_max_extra), " m, screen top up to ",
      num(best$top_below_max), " m below the water table, screen up to ",
      num(best$screen_length_max), " m)."
    )
  )
  list(
    scenario = scenario, profiles = profiles, cells = cells,
    combos = tibble::as_tibble(combos), cells_ref = cells_ref,
    cells_chosen = best$cells, wells_chosen = best$wells,
    keep_share = settings$keep_share, cell_km = settings$cell_km
  )
}
