# Figures for choosing the thresholds -------------------------------------------------
#
# How the number of usable wells and the results depend on the settings of
# the preprocessing, to choose them from the data: the recent window and the
# years per season, the years behind the mean, the spike rule, and the
# plausibility limits and error codes (a list of candidate codes and the
# values among the removed measurements). Written to plots/thresholds/.

# The measurements with their checks, as in gwl_monthly_sql(): "gs" (ground
# elevation per well) and "checked" (well_id, obs_date, gwl, elev_gs,
# check_code: 0 valid, 1 empty, 2 out of range, 3 implausible depth).
checked_measurements_cte <- function(spec) {
  sprintf(
    "WITH gs AS (
  SELECT CAST(%1$s AS nvarchar(100)) AS well_id,
         MAX(CAST(%2$s AS float)) AS elev_gs
  FROM %3$s.%4$s
  GROUP BY CAST(%1$s AS nvarchar(100))
),
checked AS (
  SELECT r.well_id, r.obs_date, r.gwl, gs.elev_gs,
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
)",
    sql_ident(spec$meta_id), sql_ident(spec$elev_gs),
    sql_ident(spec$meta_schema), sql_ident(spec$meta_table),
    sql_ident(spec$gwl_id), sql_ident(spec$gwl_date), sql_ident(spec$gwl_level),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table),
    sql_number(spec$gwl_valid_range[1]), sql_number(spec$gwl_valid_range[2]),
    sql_number(spec$dtw_valid_range[1]), sql_number(spec$dtw_valid_range[2])
  )
}

# Per well and calendar year of the last history_years: in how many months
# of each season (DJF, MAM, JJA, SON; December in its own calendar year)
# there are valid measurements.
year_seasons_sql <- function(spec, settings) {
  first_year <- spec$reference_month %/% 12 - settings$history_years + 1
  paste0(
    checked_measurements_cte(spec),
    sprintf("
SELECT well_id,
       YEAR(obs_date) AS obs_year,
       COUNT(DISTINCT CASE WHEN MONTH(obs_date) IN (12, 1, 2) THEN MONTH(obs_date) END) AS n_djf,
       COUNT(DISTINCT CASE WHEN MONTH(obs_date) IN (3, 4, 5) THEN MONTH(obs_date) END) AS n_mam,
       COUNT(DISTINCT CASE WHEN MONTH(obs_date) IN (6, 7, 8) THEN MONTH(obs_date) END) AS n_jja,
       COUNT(DISTINCT CASE WHEN MONTH(obs_date) IN (9, 10, 11) THEN MONTH(obs_date) END) AS n_son
FROM checked
WHERE check_code = 0 AND obs_date >= DATEFROMPARTS(%d, 1, 1)
GROUP BY well_id, YEAR(obs_date)", as.integer(first_year))
  )
}

# Number of measurements per 1 m class of the implied depth below ground
# (ground elevation - level), all non-empty levels of wells with a ground
# elevation; below -50 m and above 400 m pooled.
depth_histogram_sql <- function(spec) {
  paste0(
    checked_measurements_cte(spec),
    "
SELECT depth_bin, COUNT_BIG(*) AS n
FROM (
  SELECT CASE
           WHEN elev_gs - gwl < -50 THEN -51
           WHEN elev_gs - gwl >= 400 THEN 400
           ELSE FLOOR(elev_gs - gwl)
         END AS depth_bin
  FROM checked
  WHERE gwl IS NOT NULL AND elev_gs IS NOT NULL
) AS d
GROUP BY depth_bin"
  )
}

fetch_year_seasons <- function(spec, settings) db_query(year_seasons_sql(spec, settings))
fetch_depth_histogram <- function(spec) db_query(depth_histogram_sql(spec))
# Candidate error codes: how often each value of settings$candidate_codes
# occurs as a level, and in how many wells. All candidates are listed, also
# those that do not occur.
candidate_codes_sql <- function(spec, settings) {
  codes <- paste(sql_number(settings$candidate_codes), collapse = ", ")
  sprintf(
    "SELECT CAST(%1$s AS float) AS value,
       COUNT_BIG(*) AS n,
       COUNT(DISTINCT CAST(%2$s AS nvarchar(100))) AS n_wells
FROM %3$s.%4$s
WHERE CAST(%1$s AS float) IN (%5$s)
GROUP BY CAST(%1$s AS float)",
    sql_ident(spec$gwl_level), sql_ident(spec$gwl_id),
    sql_ident(spec$gwl_schema), sql_ident(spec$gwl_table), codes
  )
}

# The values among the removed measurements (outside the valid range or an
# implausible depth) that occur in the most wells.
removed_values_sql <- function(spec, settings) {
  paste0(
    checked_measurements_cte(spec),
    sprintf("
SELECT TOP (%d) value, n, n_wells, check_code
FROM (
  SELECT gwl AS value, check_code,
         COUNT_BIG(*) AS n,
         COUNT(DISTINCT well_id) AS n_wells
  FROM checked
  WHERE check_code IN (2, 3)
  GROUP BY gwl, check_code
) AS v
ORDER BY n_wells DESC, n DESC", as.integer(settings$top_values))
  )
}

fetch_candidate_codes <- function(spec, settings) {
  found <- db_query(candidate_codes_sql(spec, settings))
  tibble::tibble(value = settings$candidate_codes) |>
    dplyr::left_join(found, by = "value") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0), n_wells = dplyr::coalesce(n_wells, 0))
}
fetch_removed_values <- function(spec, settings) db_query(removed_values_sql(spec, settings))

# Calculations ------------------------------------------------------------------------

# Usable wells for each recent window (years) and minimum number of years
# per season, among the wells with complete metadata (ground elevation,
# screen top and bottom). Approximation of the pipeline's rule on calendar
# years: the window is the last `years` calendar years up to the reference
# year.
recent_window_grid <- function(year_seasons, wells, spec, settings) {
  complete <- wells$well_id[
    wells$in_meta & !is.na(wells$elev_gs) & !is.na(wells$screen_top) & !is.na(wells$screen_bot)
  ]
  ys <- dplyr::filter(year_seasons, well_id %in% complete)
  ref_year <- spec$reference_month %/% 12
  out <- lapply(settings$recent_years, function(r) {
    min_years <- ys |>
      dplyr::filter(obs_year > ref_year - r) |>
      dplyr::summarise(
        djf = sum(n_djf > 0), mam = sum(n_mam > 0), jja = sum(n_jja > 0), son = sum(n_son > 0),
        .by = well_id
      ) |>
      dplyr::mutate(min_years = pmin(djf, mam, jja, son))
    tibble::tibble(
      recent_years = r,
      min_years_per_season = seq_len(r),
      wells = vapply(seq_len(r), function(m) sum(min_years$min_years >= m), numeric(1))
    )
  })
  dplyr::bind_rows(out)
}

# Wells (with complete metadata) whose last year with valid data is within
# the last N years.
last_year_curve <- function(year_seasons, wells, spec, settings) {
  complete <- wells$well_id[
    wells$in_meta & !is.na(wells$elev_gs) & !is.na(wells$screen_top) & !is.na(wells$screen_bot)
  ]
  last <- year_seasons |>
    dplyr::filter(well_id %in% complete) |>
    dplyr::summarise(last_year = max(obs_year), .by = well_id)
  ref_year <- spec$reference_month %/% 12
  tibble::tibble(
    years_back = seq_len(settings$history_years),
    wells = vapply(
      seq_len(settings$history_years),
      function(n) sum(last$last_year > ref_year - n), numeric(1)
    )
  )
}

# How the mean level changes with the number of most recent years behind it:
# per well the mean of the monthly means of the N most recent years with
# data, compared with the mean over all years of the recent window.
mean_window_change <- function(monthly, spec) {
  start <- spec$reference_month - 12 * spec$recent_years
  per_year <- monthly |>
    dplyr::filter(!is.na(month_index), !is.na(gwl), month_index > start, month_index <= spec$reference_month) |>
    dplyr::mutate(year = month_index %/% 12) |>
    dplyr::summarise(sum = sum(gwl), n = dplyr::n(), .by = c(well_id, year)) |>
    dplyr::arrange(well_id, dplyr::desc(year)) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      mean_n = cumsum(sum) / cumsum(n),
      mean_all = sum(sum) / sum(n),
      .by = well_id
    )
  per_year |>
    dplyr::mutate(change = abs(mean_n - mean_all)) |>
    dplyr::summarise(
      wells = dplyr::n(),
      median = stats::median(change),
      p90 = stats::quantile(change, 0.9),
      .by = rank
    ) |>
    dplyr::rename(years = rank)
}

# Share of monthly means removed as spikes and wells affected, for each
# factor of the MAD and each minimum deviation, with and without the
# neighbour condition.
spike_sensitivity <- function(monthly, spec, settings) {
  start <- spec$reference_month - 12 * spec$recent_years
  m <- monthly |>
    dplyr::filter(!is.na(month_index), !is.na(gwl), month_index > start, month_index <= spec$reference_month) |>
    dplyr::arrange(well_id, month_index) |>
    dplyr::mutate(
      med = stats::median(gwl),
      n_months = dplyr::n(),
      dev_prev = abs(gwl - dplyr::lag(gwl)),
      dev_next = abs(gwl - dplyr::lead(gwl)),
      .by = well_id
    ) |>
    dplyr::mutate(dev = abs(gwl - med)) |>
    dplyr::mutate(mad = 1.4826 * stats::median(dev), .by = well_id)
  grid <- expand.grid(
    spike_mad = settings$spike_mad, spike_min_dev = settings$spike_min_dev,
    neighbours = c(TRUE, FALSE)
  )
  grid$share_removed <- NA_real_
  grid$wells_affected <- NA_real_
  for (i in seq_len(nrow(grid))) {
    threshold <- pmax(grid$spike_mad[i] * m$mad, grid$spike_min_dev[i])
    spike <- m$n_months >= 3 & m$dev > threshold
    if (grid$neighbours[i]) {
      spike <- spike & (is.na(m$dev_prev) | m$dev_prev > threshold) &
        (is.na(m$dev_next) | m$dev_next > threshold)
    }
    grid$share_removed[i] <- mean(spike)
    grid$wells_affected[i] <- length(unique(m$well_id[spike]))
  }
  tibble::as_tibble(grid)
}

# Figures -------------------------------------------------------------------------------

threshold_dir <- function(plot_config) {
  dir <- file.path(plot_config$dir, "thresholds")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

plot_recent_window <- function(grid, curve, spec, plot_config, font) {
  colour <- "#E4572E"
  current <- dplyr::filter(
    grid, recent_years == spec$recent_years, min_years_per_season == spec$min_years_per_season
  )
  heat <- ggplot2::ggplot(grid, ggplot2::aes(min_years_per_season, recent_years)) +
    ggplot2::geom_tile(ggplot2::aes(fill = wells), colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_n(wells), colour = wells > max(wells) * 0.55),
      family = font, size = 2.1
    ) +
    ggplot2::geom_tile(data = current, fill = NA, colour = colour, linewidth = 1) +
    ggplot2::scale_fill_gradient(low = "grey97", high = "grey25", guide = "none") +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"), guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(1, max(grid$recent_years), 1), expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(breaks = unique(grid$recent_years), expand = ggplot2::expansion(0)) +
    ggplot2::labs(
      title = "Usable wells by window and years per season",
      x = "Each season with data in at least ... years", y = "Recent window (years)"
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
    )

  curve_plot <- ggplot2::ggplot(curve, ggplot2::aes(years_back, wells)) +
    ggplot2::geom_vline(xintercept = spec$recent_years, colour = colour, linewidth = 0.5, linetype = "22") +
    ggplot2::geom_line(colour = "grey45", linewidth = 0.7) +
    ggplot2::geom_point(colour = "grey45", size = 1.2) +
    ggplot2::scale_y_continuous(labels = label_n, limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(
      title = "Wells with data in the last N years",
      x = "N years back", y = "Wells"
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0))

  figure <- patchwork::wrap_plots(heat, curve_plot, widths = c(1.6, 1)) +
    patchwork::plot_annotation(
      title = "Recent window and years per season",
      subtitle = paste0(
        "Wells with complete metadata (ground elevation, screen top and bottom). ",
        "Left: usable wells for each window and minimum number of years with data ",
        "per season; the ", colour_word("current setting", colour), " framed (",
        spec$recent_years, " years, ", spec$min_years_per_season, " per season). ",
        "Right: wells whose last data lie within the last N years. Calendar years, ",
        "so the numbers can differ slightly from the pipeline's monthly window."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font)
    )
  save_figure(figure, file.path(threshold_dir(plot_config), "recent_window.png"), width = figure_width, height = 7.5)
}

plot_mean_window <- function(change, spec, plot_config, font) {
  colour <- "#E4572E"
  long <- tidyr::pivot_longer(change, c(median, p90), names_to = "stat", values_to = "change")
  p_change <- ggplot2::ggplot(long, ggplot2::aes(years, change, linetype = stat)) +
    ggplot2::geom_vline(xintercept = spec$window_years, colour = colour, linewidth = 0.5, linetype = "22") +
    ggplot2::geom_line(colour = "grey35", linewidth = 0.7) +
    ggplot2::geom_point(colour = "grey35", size = 1.2) +
    ggplot2::scale_linetype_manual(
      values = c(median = "solid", p90 = "22"),
      labels = c(median = "median of the wells", p90 = "90 % of the wells below"), name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = change$years) +
    ggplot2::scale_y_continuous(labels = function(y) paste(y, "m"), limits = c(0, NA)) +
    ggplot2::labs(
      title = "Change of the mean level",
      x = "Mean from the N most recent years with data", y = "Difference to the mean of all years"
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom", legend.justification = "left",
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
    )
  p_wells <- ggplot2::ggplot(change, ggplot2::aes(years, wells)) +
    ggplot2::geom_vline(xintercept = spec$window_years, colour = colour, linewidth = 0.5, linetype = "22") +
    ggplot2::geom_col(fill = "grey70", width = 0.7) +
    ggplot2::scale_x_continuous(breaks = change$years) +
    ggplot2::scale_y_continuous(labels = label_n, expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(title = "Wells with at least N years of data", x = "N years", y = "Wells") +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0))

  figure <- patchwork::wrap_plots(p_change, p_wells, widths = c(1.5, 1)) +
    patchwork::plot_annotation(
      title = "How many years for the mean?",
      subtitle = paste0(
        "Left: how far the mean level from the N most recent years with data lies from ",
        "the mean of all years in the recent window (", spec$recent_years, " years). ",
        "Where the curves flatten, more years change little. Right: wells that have ",
        "at least N years. Dashed: the ", colour_word("current setting", colour),
        " (", spec$window_years, " years)."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font)
    )
  save_figure(figure, file.path(threshold_dir(plot_config), "mean_window.png"), width = figure_width, height = 6.5)
}

plot_spike_sensitivity <- function(spikes, spec, plot_config, font) {
  colour <- "#E4572E"
  rule_colours <- c("median only" = "grey60", "median and both neighbours" = "#2E86AB")
  neighbours_now <- isTRUE(spec$spike_neighbours %||% TRUE)
  spikes <- spikes |>
    dplyr::mutate(
      min_dev = factor(paste0("at least ", spike_min_dev, " m")),
      rule = factor(
        ifelse(neighbours, "median and both neighbours", "median only"),
        levels = names(rule_colours)
      ),
      line = interaction(rule, min_dev)
    )
  current <- dplyr::filter(
    spikes, spike_mad == spec$spike_mad, spike_min_dev == spec$spike_min_dev,
    neighbours == neighbours_now
  )
  base <- function(y, title, label_fun) {
    d <- dplyr::mutate(spikes, value = .subset2(spikes, y))
    d_current <- dplyr::mutate(current, value = .subset2(current, y))
    ggplot2::ggplot(d, ggplot2::aes(spike_mad, value, colour = rule, linetype = min_dev, group = line)) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.2) +
      ggplot2::geom_point(data = d_current, colour = colour, size = 3) +
      ggplot2::scale_colour_manual(values = rule_colours, name = "Spike if far from") +
      ggplot2::scale_x_continuous(breaks = unique(spikes$spike_mad), labels = function(x) paste0(x, " \u00d7")) +
      ggplot2::scale_y_continuous(labels = label_fun, limits = c(0, NA)) +
      ggplot2::scale_linetype_discrete(name = "Minimum deviation") +
      ggplot2::labs(title = title, x = "Factor of the MAD", y = NULL) +
      theme_charts(font, base_size = 10) +
      ggplot2::theme(
        legend.position = "bottom", legend.justification = "left", legend.box = "vertical",
        plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
      )
  }
  figure <- patchwork::wrap_plots(
    base("share_removed", "Monthly means removed", scales::label_percent()),
    base("wells_affected", "Wells with at least one removed month", label_n),
    guides = "collect"
  ) & ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
  figure <- figure +
    patchwork::plot_annotation(
      title = "How strict should the spike rule be?",
      subtitle = paste0(
        "A monthly mean is removed if it lies further than factor \u00d7 MAD (and at least ",
        "the minimum deviation) from the well's median in the recent window; with the ",
        "neighbour condition (blue) only if it lies that far from both neighbouring ",
        "months too, so real events lasting some months stay. The ",
        colour_word("current setting", colour), ": ", spec$spike_mad,
        " \u00d7 MAD, at least ", spec$spike_min_dev, " m, ",
        if (neighbours_now) "with" else "without", " the neighbour condition."
      ),
      caption = plot_config$caption,
      # the collected legend below the charts (the annotation theme sets it)
      theme = theme_maps(font) +
        ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.justification = "left")
    )
  save_figure(figure, file.path(threshold_dir(plot_config), "spikes.png"), width = figure_width, height = 6.5)
}

plot_valid_ranges <- function(depth_hist, codes, removed, spec, plot_config, font) {
  colour <- "#E4572E"
  limits <- spec$dtw_valid_range
  hist <- ggplot2::ggplot(depth_hist) +
    ggplot2::annotate(
      "rect", xmin = limits[1], xmax = limits[2], ymin = 0.8, ymax = Inf,
      fill = colour, alpha = 0.08
    ) +
    # bars from just below 1, so classes with one measurement show on the log scale
    ggplot2::geom_rect(
      ggplot2::aes(xmin = depth_bin, xmax = depth_bin + 1, ymin = 0.8, ymax = n),
      fill = "grey45"
    ) +
    ggplot2::geom_vline(xintercept = limits, colour = colour, linewidth = 0.5, linetype = "22") +
    ggplot2::scale_y_log10(labels = label_n, expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::scale_x_continuous(labels = function(x) paste(x, "m")) +
    ggplot2::labs(
      title = "Implied depth of all measurements",
      x = "Ground elevation \u2212 level (m below ground; outer bars: beyond \u221250 / 400 m)",
      y = "Measurements (log scale)"
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0))

  # values in a single well are no codes, only repeated readings
  value_text <- function(x) sub("^-", "\u2212", format(round(x, 2), trim = TRUE, drop0trailing = TRUE))
  # candidate codes: kept as levels (inside the valid range, grey) or removed (red)
  codes <- codes |>
    dplyr::mutate(
      removed = value < spec$gwl_valid_range[1] | value > spec$gwl_valid_range[2],
      label = factor(value_text(value), levels = rev(value_text(value)))
    )
  code_bars <- ggplot2::ggplot(codes, ggplot2::aes(n_wells, label, fill = removed)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(
        n == 0, "not found",
        paste0(label_n(n_wells), " wells, ", label_n(n), " values", ifelse(removed, "", " \u2013 kept!"))
      )),
      hjust = -0.05, family = font, size = 2.5, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = colour, `FALSE` = "grey55"), guide = "none") +
    ggplot2::scale_x_continuous(labels = label_n, limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.9))) +
    ggplot2::labs(title = "Candidate error codes", x = "Wells with this value", y = "Value (m a.s.l.)") +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
    )

  # values among the removed measurements found in several wells
  removed <- removed |>
    dplyr::filter(n_wells >= 2) |>
    dplyr::mutate(
      reason = ifelse(check_code == 2, "outside the valid range", "implausible depth"),
      text = paste0(value_text(value), ifelse(check_code == 3, " *", "")),
      label = factor(text, levels = rev(unique(text)))
    )
  if (nrow(removed) == 0) {
    removed <- tibble::tibble(n_wells = 0, n = 0, label = factor("none"), reason = "outside the valid range")
  }
  removed_bars <- ggplot2::ggplot(removed, ggplot2::aes(n_wells, label)) +
    ggplot2::geom_col(width = 0.7, fill = colour, alpha = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(label_n(n_wells), " wells, ", label_n(n), " values")),
      hjust = -0.05, family = font, size = 2.5, colour = "grey30"
    ) +
    ggplot2::scale_x_continuous(labels = label_n, limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.9))) +
    ggplot2::labs(
      title = "Removed values found in several wells",
      x = "Wells with this value", y = "Value (m a.s.l.; * implausible depth)"
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
    )

  figure <- patchwork::wrap_plots(
    hist, patchwork::wrap_plots(code_bars, removed_bars, ncol = 1, heights = c(1, 1.6)),
    widths = c(1.3, 1)
  ) +
    patchwork::plot_annotation(
      title = "Plausibility limits and error codes",
      subtitle = paste0(
        "Left: all measurements by their implied depth below ground, the ",
        colour_word("plausible range", colour), " (", limits[1], " to ", limits[2],
        " m) shaded; measurements outside are removed. Right, top: common error codes ",
        "and how often they occur; ", colour_word("red", colour), " ones lie outside the ",
        "valid range (", spec$gwl_valid_range[1], " to ", spec$gwl_valid_range[2],
        " m a.s.l.) and are removed, grey ones are kept as levels. Right, bottom: the ",
        "removed values found in the most wells."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font)
    )
  save_figure(figure, file.path(threshold_dir(plot_config), "valid_ranges.png"), width = figure_width, height = 7)
}

# All threshold figures; returns their paths.
plot_threshold_checks <- function(year_seasons, depth_hist, codes, removed, monthly, wells, spec,
                                  settings, plot_config,
                                  font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  c(
    plot_recent_window(
      recent_window_grid(year_seasons, wells, spec, settings),
      last_year_curve(year_seasons, wells, spec, settings),
      spec, plot_config, font
    ),
    plot_mean_window(mean_window_change(monthly, spec), spec, plot_config, font),
    plot_spike_sensitivity(spike_sensitivity(monthly, spec, settings), spec, plot_config, font),
    plot_valid_ranges(depth_hist, codes, removed, spec, plot_config, font)
  )
}
