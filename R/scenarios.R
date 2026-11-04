# Scenarios for selecting wells --------------------------------------------------
#
# A scenario filters the usable wells (see assess_well_dtw()) by four
# properties, all in m below ground surface:
#   1. mean depth to groundwater (dtw_mean) between dtw_mean_min and
#      dtw_mean_max
#   2. maximum depth to groundwater (dtw_deepest, deepest monthly mean in the
#      window) below dtw_max
#   3. position of the screen relative to the water table, by screen_rule:
#      - "below": the screen top lies below the mean water table, at most
#        top_below_max m deeper (dtw_mean <= screen_top <= dtw_mean +
#        top_below_max), and the screen bottom is deeper than the typical
#        annual low (screen_bot >= dtw_annual_low), so the well does not fall
#        dry in the annual cycle
#      - "spans": the screen reaches across the mean water table (top above,
#        bottom below it), and its bottom is deeper than the annual low, so
#        the well does not fall dry
#   4. screen length (screen_bot - screen_top) above 0 and up to
#      screen_length_max

`%||%` <- function(x, y) if (is.null(x)) y else x

scenario_criteria <- c(
  dtw_mean = "mean depth to groundwater",
  dtw_max = "maximum depth to groundwater",
  screen_position = "screen position",
  screen_length = "screen length"
)

# One row per usable well and scenario: whether it meets each criterion and
# all of them (selected).
evaluate_scenarios <- function(wells, scenarios) {
  base <- dplyr::filter(wells, usable)
  out <- lapply(seq_len(nrow(scenarios)), function(i) {
    s <- scenarios[i, ]
    screen_position <- switch(
      s$screen_rule,
      below = base$screen_top >= base$dtw_mean &
        base$screen_top <= base$dtw_mean + s$top_below_max &
        base$screen_bot >= base$dtw_annual_low,
      spans = base$screen_top <= base$dtw_mean &
        base$screen_bot >= base$dtw_annual_low,
      stop("Unknown screen_rule: ", s$screen_rule, call. = FALSE)
    )
    tibble::tibble(
      scenario_id = s$id,
      well_id = base$well_id,
      dtw_mean = base$dtw_mean >= s$dtw_mean_min & base$dtw_mean <= s$dtw_mean_max,
      dtw_max = base$dtw_deepest < s$dtw_max,
      screen_position = screen_position,
      screen_length = base$screen_length > 0 & base$screen_length <= s$screen_length_max
    ) |>
      dplyr::mutate(
        dplyr::across(dtw_mean:screen_length, \(x) x %in% TRUE),
        selected = dtw_mean & dtw_max & screen_position & screen_length
      )
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
      step_3 = step_2 & screen_position,
      step_4 = selected
    ) |>
    dplyr::summarise(
      usable = dplyr::n(),
      dtw_mean = sum(step_1),
      dtw_max = sum(step_2),
      screen_position = sum(step_3),
      screen_length = sum(step_4),
      .by = scenario_id
    ) |>
    dplyr::left_join(dplyr::select(scenarios, id, name), by = c(scenario_id = "id")) |>
    dplyr::relocate(name, .after = scenario_id)
}

# The criteria of a scenario in words, one line per criterion.
describe_scenario <- function(s) {
  m <- function(x) format(x, decimal.mark = ".", trim = TRUE)
  screen <- switch(
    s$screen_rule,
    below = paste0(
      "Screen top 0\u2013", m(s$top_below_max),
      " m below the mean water table, bottom deeper than the annual low"
    ),
    spans = "Screen across the mean water table, bottom deeper than the annual low"
  )
  c(
    dtw_mean = paste0("Mean depth to groundwater ", m(s$dtw_mean_min), "\u2013", m(s$dtw_mean_max), " m"),
    dtw_max = paste0("Maximum depth to groundwater less than ", m(s$dtw_max), " m"),
    screen_position = screen,
    screen_length = paste0("Screen length up to ", m(s$screen_length_max), " m")
  )
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
