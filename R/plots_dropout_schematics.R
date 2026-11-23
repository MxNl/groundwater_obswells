# Drop-out map and scenario schematics -------------------------------------------------

# The step at which each well drops out on the way to the usable wells (the
# steps of the flow chart, stage_flags()), or "usable".
dropout_step <- function(wells, spec) {
  f <- stage_flags(wells)
  steps <- dropout_step_labels(spec)
  dplyr::case_when(
    !f$has_gs ~ steps[1],
    !f$has_gwl ~ steps[2],
    !f$has_screen ~ steps[3],
    !f$recent ~ steps[4],
    !f$usable ~ steps[5],
    .default = steps[6]
  )
}

dropout_step_labels <- function(spec) {
  c(
    "No ground elevation",
    "No valid groundwater levels",
    "No screen depths",
    paste("No data in the last", spec$recent_years, "years"),
    paste("A season in fewer than", spec$min_years_per_season, "years"),
    "Usable wells"
  )
}

# One map per step: the wells dropping out there (and the usable wells) in
# colour, all FARM wells with coordinates in light grey behind them. Counts
# in the panel titles include the wells without coordinates. Wells with
# coordinates outside Germany are errors in the coordinates and are dropped
# from the map (they would stretch its extent and shrink the panels).
plot_dropout_map <- function(wells, points, germany, spec, plot_config,
                             font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  labels <- dropout_step_labels(spec)
  counts <- table(factor(dropout_step(dplyr::filter(wells, in_meta), spec), levels = labels))
  located <- table(factor(dropout_step(points, spec), levels = labels))
  inside <- sf::st_as_sf(points, coords = c("X", "Y"), crs = sf::st_crs(germany)) |>
    sf::st_intersects(sf::st_union(germany), sparse = FALSE)
  points <- points[inside[, 1], ]
  title_of <- function(l) {
    n <- counts[[l]]
    missing <- n - located[[l]]
    paste0(l, ": ", label_n(n), if (missing > 0) paste0(" (", label_n(missing), " without coordinates)") else "")
  }
  panels <- stats::setNames(vapply(labels, title_of, character(1)), labels)
  pts <- points |>
    dplyr::mutate(step = factor(panels[dropout_step(points, spec)], levels = panels))
  background <- dplyr::select(pts, X, Y)
  colours <- stats::setNames(c(rep("#E4572E", 5), "#1B998B"), panels)
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = germany, fill = "grey96", colour = "grey70", linewidth = 0.2) +
    ggplot2::geom_point(ggplot2::aes(X, Y), data = background, colour = "grey82", size = 0.25, stroke = 0) +
    ggplot2::geom_point(ggplot2::aes(X, Y, colour = step), data = pts, size = 0.55, stroke = 0, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(step), ncol = 3, drop = FALSE,
                        labeller = ggplot2::labeller(step = ggplot2::label_wrap_gen(34))) +
    ggplot2::coord_sf(datum = NA, crs = sf::st_crs(germany)) +
    theme_maps(font, base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85), margin = ggplot2::margin(b = 4)))
  figure <- patchwork::wrap_plots(map) +
    patchwork::plot_annotation(
      title = "Where the wells drop out",
      subtitle = paste0(
        "The FARM wells at the step where they ", colour_word("drop out", "#E4572E"),
        " on the way to the ", colour_word("usable wells", "#1B998B"),
        ", in the order of the flow chart; grey: all FARM wells with coordinates. ",
        "A well appears once, at its first failed step."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font)
    )
  save_figure(figure, file.path(plot_config$dir, "dropout_map.png"), width = figure_width, height = 10.5)
}

# The example well of a scenario schematic: water table w (m below ground)
# and a screen that meets the scenario's rules.
schematic_example <- function(s) {
  num <- function(name) scenario_value(s, name)
  w <- s$dtw_mean_min + min(1.5, (s$dtw_mean_max - s$dtw_mean_min) / 2)
  len_min <- dplyr::coalesce(num("screen_length_min"), 0.5)
  len <- min(s$screen_length_max, max(len_min, 1.5))
  bot_min <- num("bot_below_min")
  bot_max <- num("bot_below_max")
  if (s$screen_rule == "none") {
    bottom <- w + mean(c(dplyr::coalesce(bot_min, 0), dplyr::coalesce(bot_max, 2)))
    top <- bottom - len
  } else {
    top <- if (s$screen_rule == "spans") w - 0.5 else w + min(0.5, s$top_below_max / 2)
    if (!is.na(bot_max)) len <- max(len_min, min(len, w + bot_max - top))
    bottom <- top + len
  }
  list(w = w, top = top, bottom = bottom)
}

# The spans (arrows) of one scenario: x position, from and to depth, label.
schematic_spans <- function(s, ex) {
  m <- function(x) format(x, decimal.mark = ".", trim = TRUE)
  num <- function(name) scenario_value(s, name)
  out <- list(
    list(x = 0.9, from = s$dtw_mean_min, to = s$dtw_mean_max,
         label = paste0("mean water table ", m(s$dtw_mean_min), "\u2013", m(s$dtw_mean_max), " m")),
    list(x = 2.0, from = 0, to = s$dtw_max, label = paste0("deepest month < ", m(s$dtw_max), " m"))
  )
  if (s$screen_rule == "below") {
    out[[length(out) + 1]] <- list(x = 6.4, from = ex$w, to = ex$w + s$top_below_max,
                                   label = paste0("screen top 0\u2013", m(s$top_below_max), " m below"))
  } else if (s$screen_rule == "spans") {
    out[[length(out) + 1]] <- list(x = 6.4, from = 0, to = ex$w, label = "screen top above water table")
  }
  bot_min <- num("bot_below_min")
  bot_max <- num("bot_below_max")
  if (!is.na(bot_min) || !is.na(bot_max)) {
    lo <- ex$w + dplyr::coalesce(bot_min, 0)
    hi <- ex$w + dplyr::coalesce(bot_max, s$dtw_max)
    lab <- if (is.na(bot_min)) paste0("bottom \u2264 ", m(bot_max), " m below") else
      paste0("bottom ", m(bot_min), "\u2013", m(bot_max), " m below")
    out[[length(out) + 1]] <- list(x = 7.6, from = lo, to = hi, label = lab)
  }
  len_min <- num("screen_length_min")
  lab <- if (is.na(len_min)) paste0("length \u2264 ", m(s$screen_length_max), " m") else
    paste0("length ", m(len_min), "\u2013", m(s$screen_length_max), " m")
  out[[length(out) + 1]] <- list(x = 8.8, from = ex$top, to = ex$bottom, label = lab)
  dplyr::bind_rows(lapply(out, tibble::as_tibble))
}

# One schematic 1-D well profile per scenario of scenarios_fixed: ground
# surface, casing, screen and an example water table, with the thresholds as
# arrow spans. The limits of the screen are relative to the mean water table,
# so they are drawn from the example water table.
plot_scenario_schematics <- function(scenarios, plot_config, font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  depth_max <- max(scenarios$dtw_max) + 1
  parts <- lapply(seq_len(nrow(scenarios)), function(i) {
    s <- scenarios[i, ]
    ex <- schematic_example(s)
    list(
      well = tibble::tibble(name = s$name, colour = s$colour, w = ex$w, top = ex$top, bottom = ex$bottom),
      spans = dplyr::mutate(schematic_spans(s, ex), name = s$name, colour = s$colour)
    )
  })
  well <- dplyr::bind_rows(lapply(parts, `[[`, "well"))
  spans <- dplyr::bind_rows(lapply(parts, `[[`, "spans"))
  # labels on two lines, so they stay about as long as short arrows
  spans$label <- vapply(spans$label, function(l) paste(strwrap(l, 17), collapse = "\n"), character(1))
  lv <- scenarios$name
  well$name <- factor(well$name, levels = lv)
  spans$name <- factor(spans$name, levels = lv)
  casing_bottom <- pmax(well$bottom + 0.6, well$w + 1)
  p <- ggplot2::ggplot() +
    # soil, saturated zone below the example water table
    ggplot2::annotate("rect", xmin = -0.2, xmax = 10.6, ymin = 0, ymax = depth_max, fill = "#f3ece1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = -0.2, xmax = 10.6, ymin = w, ymax = depth_max), data = well,
                       fill = "#dceaf5") +
    ggplot2::geom_hline(yintercept = 0, colour = "#8a6d3b", linewidth = 0.6) +
    ggplot2::geom_segment(ggplot2::aes(x = -0.2, xend = 10.6, y = w, yend = w), data = well,
                          colour = "#2E86AB", linewidth = 0.5, linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(x = 3.3, y = w - 0.22), data = well, shape = 25, size = 2.2,
                        fill = "#2E86AB", colour = "#2E86AB") +
    # casing and screen
    ggplot2::geom_rect(ggplot2::aes(xmin = 4.6, xmax = 5.4, ymin = -0.5, ymax = casing_bottom), data = well,
                       fill = "white", colour = "grey40", linewidth = 0.35) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 4.6, xmax = 5.4, ymin = top, ymax = bottom, fill = colour),
                       data = well, colour = "grey30", linewidth = 0.35, alpha = 0.85) +
    ggplot2::scale_fill_identity() +
    # threshold spans
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = x, y = from, yend = to, colour = colour), data = spans,
      linewidth = 0.55, arrow = grid::arrow(length = grid::unit(1.6, "mm"), ends = "both", type = "closed")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x - 0.5 - 0.1 * grepl("\n", label, fixed = TRUE), y = (from + to) / 2, label = label), data = spans, angle = 90,
      family = font, size = 2.7, colour = "grey20", lineheight = 0.9
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_reverse(
      breaks = seq(0, depth_max, by = 2), labels = function(x) paste(x, "m"),
      expand = ggplot2::expansion(c(0.02, 0))
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.2, 10.6), expand = ggplot2::expansion(0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::facet_wrap(ggplot2::vars(name), ncol = 3) +
    ggplot2::labs(x = NULL, y = "Depth below ground") +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"), panel.spacing.x = grid::unit(1.2, "lines"),
      panel.spacing.y = grid::unit(1.2, "lines")
    )
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "The scenarios at a well",
      subtitle = paste0(
        "Each scenario at a schematic well: ground surface, casing and screen (coloured), an example ",
        "mean water table (", colour_word("dashed", "#2E86AB"), "; saturated zone in blue). Arrows: the ",
        "allowed ranges. Mean water table and deepest month are depths below ground; the screen ",
        "limits are relative to the mean water table and move with it. All scenarios also require ",
        "the screen bottom below the average annual low (not falling dry)."
      ),
      caption = "Thresholds: scenarios_fixed in _targets.R.",
      theme = theme_maps(font)
    )
  rows <- ceiling(nrow(scenarios) / 3)
  save_figure(figure, file.path(plot_config$dir, "scenario_schematics.png"), width = figure_width,
              height = 2.6 + 4.6 * rows)
}
