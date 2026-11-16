# Diagnostic sheets for single wells ------------------------------------------------
#
# For a random sample of wells, one panel per well with its monthly
# groundwater levels as depth below ground in the recent window, the removed
# spikes, the years used for the mean, the mean, annual low and maximum depth,
# the screen, and the result of each scenario. To check by eye that the rules
# do what they should. Wells are sampled in three groups: selected by at
# least one scenario, usable but not selected, not usable (with ground
# elevation and valid levels, so a depth can be drawn).

# The sample: n_per_group wells per group, reproducible with seed.
sample_diagnostic_wells <- function(wells, scenario_wells, settings) {
  selected_ids <- unique(scenario_wells$well_id[scenario_wells$selected])
  groups <- wells |>
    dplyr::filter(in_meta, !is.na(elev_gs), dplyr::coalesce(n_obs_valid, 0) > 0) |>
    dplyr::mutate(group = dplyr::case_when(
      well_id %in% selected_ids ~ "selected by a scenario",
      usable ~ "usable, not selected",
      .default = "not usable"
    ))
  withr::with_seed(settings$seed, {
    groups |>
      dplyr::slice_sample(n = settings$n_per_group, by = group) |>
      dplyr::mutate(group = factor(group, levels = c(
        "selected by a scenario", "usable, not selected", "not usable"
      ))) |>
      dplyr::arrange(group, well_id)
  })
}

# Result of each scenario for one well, in words: "yes" or the first
# criterion it fails.
scenario_results <- function(well_id, scenario_wells, scenarios) {
  criteria <- c(
    dtw_mean = "mean depth", dtw_max = "maximum depth", screen_top = "screen top",
    not_dry = "falls dry", screen_length = "screen length"
  )
  rows <- scenario_wells[scenario_wells$well_id == well_id, ]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  res <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    name <- scenarios$name[scenarios$id == r$scenario_id]
    if (r$selected) {
      paste0(name, ": yes")
    } else {
      failed <- names(criteria)[!unlist(r[names(criteria)])][1]
      paste0(name, ": no (", criteria[[failed]], ")")
    }
  }, character(1))
  paste(res, collapse = " | ")
}

# Spikes among the monthly means of one well (in time order), the same rule
# as the DuckDB statistics: further than max(k * MAD, min_dev) from the
# median and, with neighbours, also from both neighbouring months.
mark_spikes <- function(gwl, k, min_dev, neighbours = TRUE) {
  n <- length(gwl)
  if (n < 3) {
    return(rep(FALSE, n))
  }
  med <- stats::median(gwl)
  threshold <- max(k * 1.4826 * stats::median(abs(gwl - med)), min_dev)
  spike <- abs(gwl - med) > threshold
  if (neighbours) {
    dev_prev <- abs(gwl - c(NA, gwl[-n]))
    dev_next <- abs(gwl - c(gwl[-1], NA))
    spike <- spike & (is.na(dev_prev) | dev_prev > threshold) & (is.na(dev_next) | dev_next > threshold)
  }
  spike
}

# Monthly depths of one well in the recent window, with the spikes marked as
# by the DuckDB statistics and the years used for the mean.
diagnostic_series <- function(monthly, well, spec) {
  start <- spec$reference_month - 12 * spec$recent_years
  d <- monthly[monthly$well_id == well$well_id & !is.na(monthly$month_index) & !is.na(monthly$gwl), ]
  d <- d[d$month_index > start & d$month_index <= spec$reference_month, ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d <- d[order(d$month_index), ]
  d$spike <- mark_spikes(d$gwl, spec$spike_mad, spec$spike_min_dev, spec$spike_neighbours %||% TRUE)
  d$year <- d$month_index %/% 12
  years <- sort(unique(d$year[!d$spike]), decreasing = TRUE)
  d$mean_window <- d$year %in% utils::head(years, spec$window_years)
  d$date <- month_date(d$month_index)
  d$depth <- well$elev_gs - d$gwl
  d
}

diagnostic_panel <- function(well, monthly, scenario_wells, scenarios, spec, font) {
  d <- diagnostic_series(monthly, well, spec)
  start <- month_date(spec$reference_month - 12 * spec$recent_years + 1)
  end <- month_date(spec$reference_month)
  screen_x <- c(end + 120, end + 300)

  title <- paste0(well$well_id, "  (", well$group, ")")
  lines <- tibble::tibble(
    what = c("mean", "annual low", "maximum"),
    y = c(well$dtw_mean, well$dtw_annual_low, well$dtw_deepest),
    linetype = c("solid", "22", "12"),
    colour = c("#2E86AB", "#2E86AB", "#83053C")
  ) |>
    dplyr::filter(!is.na(y))
  depths <- if (nrow(lines) > 0) {
    paste0(paste0(lines$what, " ", format(round(lines$y, 1), nsmall = 1, trim = TRUE), " m"), collapse = " \u00b7 ")
  } else {
    "no depth in the recent window"
  }
  results <- scenario_results(well$well_id, scenario_wells, scenarios)
  status <- if (isTRUE(well$usable)) {
    paste0("Usable. ", results)
  } else {
    paste0("Not usable: ", well$flags %||% "")
  }
  subtitle <- paste0(
    "Depth: ", depths, "\n",
    paste(strwrap(status, 95), collapse = "\n")
  )
  y_values <- c(0, d$depth, lines$y, well$screen_top, well$screen_bot)

  p <- ggplot2::ggplot()
  if (!is.null(d) && any(d$mean_window)) {
    years <- unique(d$year[d$mean_window])
    p <- p + ggplot2::geom_rect(
      ggplot2::aes(xmin = as.Date(paste0(year, "-01-01")), xmax = as.Date(paste0(year, "-12-31")),
                   ymin = -Inf, ymax = Inf),
      data = tibble::tibble(year = years), fill = "grey93"
    )
  }
  p <- p +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = y, linetype = linetype, colour = colour),
      data = lines, linewidth = 0.5
    ) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_colour_identity()
  if (!is.null(d)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(date, depth), data = d[!d$spike, ], colour = "grey55", linewidth = 0.3) +
      ggplot2::geom_point(ggplot2::aes(date, depth), data = d[!d$spike, ], colour = "grey25", size = 0.5) +
      ggplot2::geom_point(ggplot2::aes(date, depth), data = d[d$spike, ], colour = "#E4572E", shape = 4, size = 1.6, stroke = 0.7)
  }
  if (!is.na(well$screen_top) && !is.na(well$screen_bot)) {
    p <- p +
      ggplot2::annotate(
        "rect", xmin = screen_x[1], xmax = screen_x[2],
        ymin = well$screen_top, ymax = well$screen_bot, fill = "#F0A202", alpha = 0.8
      ) +
      ggplot2::annotate(
        "text", x = mean(screen_x), y = well$screen_top, label = "screen", vjust = 1.4,
        family = font, size = 2.2, colour = "grey20", angle = 90, hjust = 1
      )
  }
  p +
    ggplot2::scale_y_reverse(labels = function(y) paste(y, "m")) +
    ggplot2::scale_x_date(limits = c(start, screen_x[2] + 30), date_labels = "%Y") +
    ggplot2::coord_cartesian(ylim = rev(range(y_values, na.rm = TRUE)) + c(0.3, -0.3)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "m below ground") +
    theme_charts(font, base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = "grey30", size = ggplot2::rel(0.8), hjust = 0, lineheight = 1.05),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )
}

# All sheets: per_page panels per page, as PNG pages in plots/diagnostics/.
# Returns the paths.
plot_well_diagnostics <- function(wells, monthly, scenario_wells, scenarios, spec,
                                  settings, plot_config,
                                  font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  sample <- sample_diagnostic_wells(wells, scenario_wells, settings)
  dir <- file.path(plot_config$dir, "diagnostics")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(dir, "^wells_[0-9]+\\.png$", full.names = TRUE))

  panels <- lapply(seq_len(nrow(sample)), function(i) {
    diagnostic_panel(sample[i, ], monthly, scenario_wells, scenarios, spec, font)
  })
  pages <- split(seq_along(panels), ceiling(seq_along(panels) / settings$per_page))
  vapply(seq_along(pages), function(k) {
    page <- patchwork::wrap_plots(panels[pages[[k]]], ncol = 2) +
      patchwork::plot_annotation(
        title = paste0("Diagnostic sheets: wells ", min(pages[[k]]), "\u2013", max(pages[[k]]), " of ", length(panels)),
        subtitle = paste0(
          "Monthly depth below ground in the last ", spec$recent_years, " years (dots; ",
          "red crosses: removed spikes), years used for the mean shaded, mean depth ",
          "(blue line), annual low (blue dashed), maximum depth (dark red dotted), ",
          "screen (orange bar on the right)."
        ),
        caption = plot_config$caption,
        theme = theme_maps(font)
      )
    save_figure(page, file.path(dir, sprintf("wells_%02d.png", k)), width = figure_width, height = 8.5)
  }, character(1))
}
