# Figures ------------------------------------------------------------------------
#
# Sized for normal screens (about 11 inches wide). Special characters are
# written as \u escapes, so the code reads the same in every encoding
# (Windows): u2013 en dash, u2212 minus, u2264 less or equal, u2265
# greater or equal (each with a backslash before).

label_n <- function(x) {
  scales::label_comma()(x)
}

# Figure sizes in inches
figure_width <- 11

# The four properties of the scenarios, as rows of the distribution chart.
scenario_properties <- c(
  dtw_mean = "Mean depth to\ngroundwater",
  dtw_deepest = "Maximum depth\nto groundwater",
  screen_top = "Depth of the\nscreen top",
  screen_length = "Screen length"
)

# Allowed range of a property in a scenario. The screen top lies relative to
# the mean water table, so its range in m below ground is the one the mean
# depth and the screen rule allow together: "below" from the smallest mean
# depth to the largest plus top_below_max, "spans" from 0 to the largest
# mean depth.
scenario_ranges <- function(s) {
  top <- if (s$screen_rule == "below") {
    c(s$dtw_mean_min, s$dtw_mean_max + s$top_below_max)
  } else {
    c(0, s$dtw_mean_max)
  }
  tibble::tibble(
    property = factor(names(scenario_properties), levels = rev(names(scenario_properties))),
    from = c(s$dtw_mean_min, 0, top[1], 0),
    to = c(s$dtw_mean_max, s$dtw_max, top[2], s$screen_length_max)
  )
}

# The five criteria of a scenario in short words, for boxes and lists.
scenario_step_labels <- function(s, sep = "\n") {
  m <- function(x) format(x, decimal.mark = ".", trim = TRUE)
  screen <- if (s$screen_rule == "below") {
    paste0("Screen top 0\u2013", m(s$top_below_max), " m", sep, "below the water table")
  } else {
    paste0("Screen top above", sep, "the water table")
  }
  c(
    dtw_mean = paste0("Mean depth", sep, m(s$dtw_mean_min), "\u2013", m(s$dtw_mean_max), " m"),
    dtw_max = paste0("Maximum depth", sep, "< ", m(s$dtw_max), " m"),
    screen_top = screen,
    not_dry = paste0("Not falling dry: bottom", sep, "below the annual low"),
    screen_length = paste0("Screen length", sep, "\u2264 ", m(s$screen_length_max), " m")
  )
}

scenario_step_counts <- function(st) {
  c(st$dtw_mean, st$dtw_max, st$screen_top, st$not_dry, st$screen_length)
}

# The steps from the metadata to the usable wells: label, wells left, wells
# dropped at the step and why. Used by the flow chart and the Sankey diagram.
well_stages <- function(wells, spec) {
  m <- dplyr::filter(wells, in_meta)
  has_gs <- !is.na(m$elev_gs)
  has_gwl <- has_gs & dplyr::coalesce(m$n_obs_valid, 0) > 0
  has_screen <- has_gwl & !is.na(m$screen_top) & !is.na(m$screen_bot)
  recent <- has_screen & m$recent %in% TRUE
  usable <- recent & m$usable

  tibble::tibble(
    label = c(
      "Wells in the metadata",
      "with ground surface elevation",
      "with valid groundwater levels",
      "with screen depths",
      paste("with data in the last", spec$recent_years, "years"),
      paste0("each season in \u2265 ", spec$min_years_per_season, " of these years")
    ),
    short = c(
      "Metadata", "Ground\nelevation", "Valid\nlevels", "Screen\ndepths",
      "Recent\ndata", "Usable\nwells"
    ),
    n = c(nrow(m), sum(has_gs), sum(has_gwl), sum(has_screen), sum(recent), sum(usable)),
    reason = c(
      NA,
      "no ground surface elevation",
      "no valid groundwater levels",
      "no screen depths",
      paste("no data in the last", spec$recent_years, "years"),
      paste("a season in <", spec$min_years_per_season, "years")
    )
  ) |>
    dplyr::mutate(dropped = dplyr::lag(n) - n)
}

# Scenario figure ---------------------------------------------------------------

# One figure per scenario: map of the selected wells among all usable wells,
# their number with the wells left after each criterion, and the
# distribution of the four properties (all usable wells in grey, the
# selected in the scenario colour). Returns the path of the PNG.
# `font`: the font family from setup_plot_font() (target plot_font, so the
# plots are redrawn when the font becomes available); registered again here,
# as the plot may be drawn in another R session.
plot_scenario <- function(s, points, scenario_wells, steps, germany, plot_config, font) {
  setup_plot_font(plot_config$font)
  colour <- s$colour
  selected_ids <- scenario_wells$well_id[scenario_wells$scenario_id == s$id & scenario_wells$selected]
  usable <- dplyr::filter(points, usable) |>
    dplyr::mutate(selected = well_id %in% selected_ids) |>
    dplyr::arrange(selected)
  step <- steps[steps$scenario_id == s$id, ]
  n_usable <- step$usable
  n_selected <- step$screen_length

  # map
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = germany, fill = "grey95", colour = "grey70", linewidth = 0.2) +
    ggplot2::geom_point(
      ggplot2::aes(X, Y),
      data = dplyr::filter(usable, !selected),
      colour = "grey70", size = 0.3, stroke = 0
    ) +
    ggplot2::geom_point(
      ggplot2::aes(X, Y),
      data = dplyr::filter(usable, selected),
      colour = colour, size = 0.9, stroke = 0
    ) +
    ggplot2::coord_sf(datum = NA, crs = sf::st_crs(germany)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 0))

  # number of wells and the wells left after each criterion
  criteria <- scenario_step_labels(s, sep = " ")
  counts <- tibble::tibble(
    label = c("Usable wells", unname(criteria)),
    n = c(n_usable, scenario_step_counts(step)),
    y = -seq_along(label)
  )
  numbers <- ggplot2::ggplot(counts) +
    ggplot2::annotate(
      "text", x = 0, y = 1.5, label = label_n(n_selected), hjust = 0, vjust = 0,
      family = font, fontface = "bold", size = 13, colour = colour
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 1.1, hjust = 0, vjust = 1, family = font, size = 4,
      colour = "grey25",
      label = paste0(
        "wells, ", scales::label_percent(accuracy = 0.1)(n_selected / max(n_usable, 1)),
        " of the ", label_n(n_usable), " usable wells"
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0, y = y, label = label),
      hjust = 0, family = font, size = 3.3, colour = "grey35"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = y, label = label_n(n)),
      hjust = 1, family = font, size = 3.3, colour = "grey15", fontface = "bold"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 1, y = y - 0.5, yend = y - 0.5),
      colour = "grey85", linewidth = 0.3
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(limits = c(-nrow(counts) - 0.6, 3.2), expand = ggplot2::expansion(0)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 6, 10))

  # distributions of the four properties
  long <- function(data) {
    tidyr::pivot_longer(
      data, dplyr::all_of(names(scenario_properties)),
      names_to = "property", values_to = "value"
    ) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(property = factor(property, levels = rev(names(scenario_properties))))
  }
  all_long <- long(usable)
  sel_long <- long(dplyr::filter(usable, selected))
  # densities need some wells per property
  sel_dens <- dplyr::filter(sel_long, dplyr::n() >= 3, .by = property)
  ranges <- dplyr::filter(scenario_ranges(s), !is.na(from))
  x_max <- plot_config$distribution_max_m

  ridges <- ggplot2::ggplot(all_long, ggplot2::aes(value, property)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = from, xmax = pmin(to, x_max), ymin = as.numeric(property),
        ymax = as.numeric(property) + 0.92
      ),
      data = ranges, inherit.aes = FALSE,
      fill = colour, alpha = 0.12
    ) +
    ggridges::geom_density_ridges(
      ggplot2::aes(height = ggplot2::after_stat(ndensity)),
      stat = "density_ridges", scale = 0.9, fill = "grey88", colour = "grey60",
      linewidth = 0.3, rel_min_height = 0.002, bandwidth = 0.3
    ) +
    ggridges::geom_density_ridges(
      ggplot2::aes(height = ggplot2::after_stat(ndensity)),
      data = sel_dens, stat = "density_ridges", scale = 0.9,
      fill = colour, colour = "white", alpha = 0.75,
      linewidth = 0.4, rel_min_height = 0.002, bandwidth = 0.3
    ) +
    # the selected wells as ticks below their ridge
    ggplot2::geom_point(
      ggplot2::aes(value, as.numeric(property) - 0.06),
      data = sel_long, shape = "|", size = 1.6, colour = colour, alpha = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, x_max, 4), expand = ggplot2::expansion(0),
      labels = function(x) paste(x, "m")
    ) +
    ggplot2::scale_y_discrete(
      labels = scenario_properties, expand = ggplot2::expansion(add = c(0.25, 1))
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(x = "m below ground surface", y = NULL) +
    theme_charts(font, base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(vjust = 0, hjust = 1, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(colour = "grey35", size = ggplot2::rel(0.85), margin = ggplot2::margin(t = 6)),
      plot.margin = ggplot2::margin(4, 10, 0, 10)
    )

  subtitle <- paste0(
    s$purpose, " Map: the ", colour_word("selected wells", colour),
    " among all usable wells (grey). Right: wells left after each criterion ",
    "and the distribution of the four properties (grey: usable wells; ",
    "shaded: allowed range, for the screen top the range the mean depth and ",
    "the screen rule allow together)."
  )

  figure <- patchwork::wrap_elements(full = map) + numbers + ridges +
    patchwork::plot_layout(design = "AB\nAC", widths = c(1, 1.1), heights = c(0.6, 1)) +
    patchwork::plot_annotation(
      title = paste0(s$name, ": ", label_n(n_selected), " wells"),
      subtitle = subtitle,
      caption = plot_config$caption,
      theme = theme_maps(font) +
        ggplot2::theme(plot.subtitle = element_subtitle(margin = ggplot2::margin(t = 4, b = 8)))
    )

  save_figure(
    figure,
    file.path(plot_config$dir, paste0("scenario_", s$id, ".png")),
    width = figure_width, height = 8
  )
}

plot_scenarios <- function(scenarios, points, scenario_wells, steps, germany, plot_config,
                           font = setup_plot_font(plot_config$font)) {
  vapply(
    seq_len(nrow(scenarios)),
    function(i) plot_scenario(scenarios[i, ], points, scenario_wells, steps, germany, plot_config, font),
    character(1)
  )
}

# Flow chart ----------------------------------------------------------------------

# The steps from the metadata to the usable wells (wells left and what drops
# out), then per scenario its four criteria as boxes and the selected wells
# as the final box. Without subtitle: the boxes say what happens.
plot_flowchart <- function(wells, scenarios, steps, spec, plot_config,
                           font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  chain <- well_stages(wells, spec)
  n_chain <- nrow(chain)
  chain$y <- -(seq_len(n_chain) - 1) * 0.95
  chain$detail <- c(
    "", "", "error codes, implausible depths and spikes removed",
    rep("", n_chain - 4), "= usable wells"
  )

  box_w <- 5
  box_h <- 0.62
  n_sc <- nrow(scenarios)
  sc_w <- 2.05
  sc_gap <- 0.17
  sc_x <- (seq_len(n_sc) - (n_sc + 1) / 2) * (sc_w + sc_gap)
  bus_y <- min(chain$y) - box_h / 2 - 0.45
  crit_h <- 0.95
  crit_step <- 1.2
  crit_y0 <- bus_y - 0.45 - crit_h / 2

  # criterion boxes: one row per scenario and criterion
  crit <- lapply(seq_len(n_sc), function(i) {
    s <- scenarios[i, ]
    st <- steps[steps$scenario_id == s$id, ]
    tibble::tibble(
      id = s$id,
      colour = s$colour,
      x = sc_x[i],
      k = 1:5,
      y = crit_y0 - (0:4) * crit_step,
      label = unname(scenario_step_labels(s)),
      n = scenario_step_counts(st)
    )
  }) |>
    dplyr::bind_rows()
  leaf_y <- crit_y0 - 5 * crit_step - 0.1
  leaf_h <- 1.05
  leaves <- tibble::tibble(
    x = sc_x,
    y = leaf_y,
    name = gsub("Screen at the water table", "Screen at the\nwater table", scenarios$name, fixed = TRUE),
    colour = scenarios$colour,
    n = steps$screen_length[match(scenarios$id, steps$scenario_id)]
  )

  arrow <- grid::arrow(length = grid::unit(1.8, "mm"), type = "closed")
  line_col <- "grey55"

  p <- ggplot2::ggplot() +
    # chain: arrows, what drops out, boxes
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 0, y = y + 0.95 - box_h / 2, yend = y + box_h / 2),
      data = chain[-1, ], colour = line_col, linewidth = 0.4, arrow = arrow
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0.18, y = y + 0.475, label = paste0("\u2212 ", label_n(dropped), " ", reason)),
      data = chain[-1, ], hjust = 0, family = font, size = 3, colour = "grey45",
      fontface = "italic"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -box_w / 2, xmax = box_w / 2, ymin = y - box_h / 2, ymax = y + box_h / 2),
      data = chain, fill = c(rep("grey96", n_chain - 1), "grey85"),
      colour = c(rep("grey60", n_chain - 1), "grey30"), linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = -box_w / 2 + 0.15, y = y + ifelse(detail == "", 0, 0.11), label = label),
      data = chain, hjust = 0, family = font, size = 3.6, colour = "grey15",
      fontface = ifelse(seq_len(n_chain) == 1, "bold", "plain")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = -box_w / 2 + 0.15, y = y - 0.15, label = detail),
      data = dplyr::filter(chain, detail != ""), hjust = 0, family = font,
      size = 2.7, colour = "grey35"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = box_w / 2 - 0.15, y = y, label = label_n(n)),
      data = chain, hjust = 1, family = font, size = 3.9, colour = "grey15",
      fontface = "bold"
    ) +
    # branches to the scenarios
    ggplot2::annotate(
      "segment", x = 0, xend = 0, y = min(chain$y) - box_h / 2, yend = bus_y,
      colour = line_col, linewidth = 0.4
    ) +
    ggplot2::annotate(
      "segment", x = min(sc_x), xend = max(sc_x), y = bus_y, yend = bus_y,
      colour = line_col, linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = x, y = bus_y, yend = crit_y0 + crit_h / 2),
      data = leaves, colour = line_col, linewidth = 0.4, arrow = arrow
    ) +
    # criterion boxes and arrows between them
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = x, y = y - crit_h / 2, yend = y - crit_step + crit_h / 2),
      data = dplyr::filter(crit, k < 5), colour = line_col, linewidth = 0.4, arrow = arrow
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = x, y = y - crit_h / 2, yend = leaf_y + leaf_h / 2),
      data = dplyr::filter(crit, k == 5), colour = line_col, linewidth = 0.4, arrow = arrow
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = x - sc_w / 2, xmax = x + sc_w / 2, ymin = y - crit_h / 2, ymax = y + crit_h / 2),
      data = crit, fill = "white", colour = crit$colour, linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = y + 0.1, label = label),
      data = crit, family = font, size = 2.6, colour = "grey30", lineheight = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = y - crit_h / 2 + 0.14, label = label_n(n)),
      data = crit, family = font, size = 3, colour = "grey15", fontface = "bold"
    ) +
    # final boxes: the selected wells
    ggplot2::geom_rect(
      ggplot2::aes(xmin = x - sc_w / 2, xmax = x + sc_w / 2, ymin = y - leaf_h / 2, ymax = y + leaf_h / 2),
      data = leaves, fill = scales::alpha(leaves$colour, 0.15), colour = leaves$colour, linewidth = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = y + 0.2, label = name),
      data = leaves, family = font, size = 3.2, fontface = "bold", colour = "grey15",
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = y - 0.28, label = label_n(n)),
      data = leaves, family = font, size = 6, fontface = "bold", colour = leaves$colour
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(sc_x) - sc_w / 2, max(sc_x) + sc_w / 2),
      ylim = c(leaf_y - leaf_h / 2, max(chain$y) + box_h / 2),
      expand = FALSE, clip = "off"
    ) +
    ggplot2::labs(
      title = "From the well database to the scenarios",
      caption = plot_config$caption
    ) +
    theme_maps(font) +
    ggplot2::theme(plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 14)))

  save_figure(p, file.path(plot_config$dir, "flowchart.png"), width = figure_width, height = 12)
}

# Sankey diagrams -------------------------------------------------------------------

# Polygons of a Sankey diagram of drop-outs: a main band from left to right
# whose height is the number of wells left, and at each step a branch that
# bends down with the wells dropped there. `stages` needs columns short (stage
# label), n (wells left) and reason (why dropped, NA for the first).
sankey_parts <- function(stages, gap = 0.12, bend = 0.45) {
  k <- nrow(stages)
  h <- stages$n / stages$n[1] # band heights relative to the first stage
  band <- tibble::tibble(
    xmin = c(-0.3, seq_len(k - 1) - 0.3),
    xmax = c(seq_len(k - 1) - 0.3, k - 1 + 0.35),
    ymin = -h, ymax = 0
  )
  # sigmoid from (x0, y0) to (x1, y1)
  curve <- function(x0, x1, y0, y1, n = 30) {
    t <- seq(0, 1, length.out = n)
    list(x = x0 + (x1 - x0) * t, y = y0 + (y1 - y0) * (3 * t^2 - 2 * t^3))
  }
  dropped <- c(NA, stages$n[-k] - stages$n[-1])
  steps <- which(dropped > 0)
  branches <- lapply(steps, function(i) {
    top <- -h[i]
    bottom <- -h[i - 1]
    x0 <- i - 1 - 0.3
    x1 <- x0 + bend
    drop <- gap + 0.04 * (i %% 2) # alternate the depth so labels do not touch
    upper <- curve(x0, x1, top, top - drop)
    lower <- curve(x0, x1, bottom, bottom - drop)
    tibble::tibble(
      step = i,
      x = c(upper$x, x1 + 0.12, x1 + 0.12, rev(lower$x)),
      y = c(upper$y, top - drop, bottom - drop, rev(lower$y))
    )
  }) |>
    dplyr::bind_rows()
  wrap <- function(x) vapply(x, function(t) paste(strwrap(t, 18), collapse = "\n"), character(1))
  labels <- tibble::tibble(
    step = steps,
    x = steps - 1 - 0.3 + bend + 0.06,
    y = -h[steps - 1] - gap - 0.04 * (steps %% 2) - 0.03,
    label = paste0("\u2212", label_n(dropped[steps]), "\n", wrap(stages$reason[steps]))
  )
  stage_labels <- tibble::tibble(
    x = (band$xmin + band$xmax) / 2,
    label = paste0(stages$short, "\n", label_n(stages$n))
  )
  list(
    band = band, branches = branches, labels = labels, stage_labels = stage_labels,
    ylim = c(-max(h[-k] + gap + 0.04) - 0.42, 0.34),
    # room on the right for the label of the last branch
    xlim = c(-0.55, k - 1 + 0.75)
  )
}

sankey_plot <- function(stages, colour, font, title = NULL, drop_colour = "grey70",
                        label_size = 2.9) {
  parts <- sankey_parts(stages)
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(x, y, group = step),
      data = parts$branches, fill = drop_colour, colour = drop_colour, linewidth = 0.2
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = parts$band, fill = colour, colour = colour, linewidth = 0.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x, 0.04, label = label),
      data = parts$stage_labels, vjust = 0, family = font, size = label_size + 0.3,
      colour = "grey15", lineheight = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = label),
      data = parts$labels, vjust = 1, hjust = 0.5, family = font, size = label_size,
      colour = "grey40", lineheight = 0.95
    ) +
    ggplot2::coord_cartesian(xlim = parts$xlim, ylim = parts$ylim, expand = FALSE, clip = "off") +
    ggplot2::labs(title = title) +
    theme_maps(font) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(0.95), hjust = 0, colour = colour,
        margin = ggplot2::margin(b = 2)
      )
    )
}

# Sankey diagram from the metadata to the usable wells, and one per scenario
# from the usable wells through its four criteria. Returns both paths.
plot_sankeys <- function(wells, scenarios, steps, spec, plot_config,
                         font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  stages <- well_stages(wells, spec)
  overview <- sankey_plot(stages, "grey45", font, label_size = 3.1) +
    ggplot2::labs(
      title = "Where the wells drop out",
      subtitle = "From the metadata to the usable wells: the band shows the wells left, the grey branches the wells dropped at each step.",
      caption = plot_config$caption
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.4), hjust = 0.5, colour = "black")
    )
  path_overview <- save_figure(
    overview, file.path(plot_config$dir, "sankey_wells.png"),
    width = figure_width, height = 6
  )

  per_scenario <- lapply(seq_len(nrow(scenarios)), function(i) {
    s <- scenarios[i, ]
    st <- steps[steps$scenario_id == s$id, ]
    lab <- scenario_step_labels(s, sep = " ")
    sc_stages <- tibble::tibble(
      short = c("Usable\nwells", "Mean\ndepth", "Maximum\ndepth", "Screen\ntop", "Not\ndry", "Screen\nlength"),
      n = c(st$usable, scenario_step_counts(st)),
      reason = c(
        NA,
        paste0("mean not ", sub("Mean depth ", "", lab[["dtw_mean"]])),
        paste0("maximum \u2265 ", s$dtw_max, " m"),
        if (s$screen_rule == "below") "screen top not right below the water table" else "screen top not above the water table",
        "falls dry at the annual low",
        paste0("screen > ", s$screen_length_max, " m")
      )
    )
    sankey_plot(sc_stages, s$colour, font, title = paste0(s$name, ": ", label_n(st$screen_length), " wells"),
                label_size = 2.6)
  })
  # a free panel explains how to read the diagrams
  if (length(per_scenario) %% 2 == 1) {
    key <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text", x = 0, y = 0.5, hjust = 0, family = font, size = 3.4, colour = "grey30",
        lineheight = 1.15,
        label = paste(
          "How to read: from left to right, each scenario",
          "applies its five criteria to the usable wells.",
          "The coloured band shows the wells left after",
          "each criterion, the grey branches the wells",
          "dropped there. The last number is the number",
          "of selected wells.",
          sep = "\n"
        )
      ) +
      ggplot2::xlim(0, 1) +
      theme_maps(font)
    per_scenario <- c(per_scenario, list(key))
  }
  scenarios_plot <- patchwork::wrap_plots(per_scenario, ncol = 2) +
    patchwork::plot_annotation(
      title = "Where the usable wells drop out in each scenario",
      caption = plot_config$caption,
      theme = theme_maps(font)
    )
  path_scenarios <- save_figure(
    scenarios_plot, file.path(plot_config$dir, "sankey_scenarios.png"),
    width = figure_width, height = 10
  )
  c(path_overview, path_scenarios)
}

# Combined Sankey diagram ------------------------------------------------------------

# One Sankey diagram from the metadata through the preprocessing to the
# scenarios. Scenarios overlap (FARM strict is part of FARM tolerant), so at
# the end every usable well goes to one group only: the first scenario in
# the order of the scenarios table it meets, or none. The labels give the
# wells of the group and, where earlier scenarios took some of its wells,
# the scenario's total.
plot_sankey_combined <- function(wells, scenarios, scenario_wells, steps, spec, plot_config,
                                 font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  stages <- well_stages(wells, spec)
  parts <- sankey_parts(stages)
  k <- nrow(stages)
  n0 <- stages$n[1]

  groups <- assign_scenario_groups(scenario_wells, scenarios) |>
    dplyr::count(scenario_id, name = "n_group")
  group_tbl <- tibble::tibble(
    scenario_id = c(scenarios$id, "none"),
    name = c(scenarios$name, "No scenario"),
    colour = c(scenarios$colour, "grey80"),
    text_colour = c(scenarios$colour, "grey45"),
    total = c(steps$screen_length[match(scenarios$id, steps$scenario_id)], NA)
  ) |>
    dplyr::left_join(groups, by = "scenario_id") |>
    dplyr::mutate(n_group = dplyr::coalesce(n_group, 0L)) |>
    dplyr::filter(n_group > 0)

  # stacked groups at the end of the band, separated by gaps
  gap <- 0.025
  h <- group_tbl$n_group / n0
  src_top <- -c(0, cumsum(h)[-length(h)])
  dst_top <- src_top - gap * (seq_along(h) - 1)
  x0 <- k - 1 + 0.35
  x1 <- x0 + 0.9
  x2 <- x1 + 0.25
  curve <- function(xa, xb, ya, yb, n = 30) {
    t <- seq(0, 1, length.out = n)
    list(x = xa + (xb - xa) * t, y = ya + (yb - ya) * (3 * t^2 - 2 * t^3))
  }
  split <- lapply(seq_along(h), function(i) {
    upper <- curve(x0, x1, src_top[i], dst_top[i])
    lower <- curve(x0, x1, src_top[i] - h[i], dst_top[i] - h[i])
    tibble::tibble(
      group = i,
      x = c(upper$x, x2, x2, rev(lower$x)),
      y = c(upper$y, dst_top[i], dst_top[i] - h[i], rev(lower$y))
    )
  }) |>
    dplyr::bind_rows()

  # labels right of the groups, pushed apart where groups are thin
  centre <- dst_top - h / 2
  min_step <- 0.075
  label_y <- centre
  for (i in seq_along(label_y)[-1]) {
    label_y[i] <- min(label_y[i], label_y[i - 1] - min_step)
  }
  earlier <- !is.na(group_tbl$total) & group_tbl$total > group_tbl$n_group
  group_labels <- group_tbl |>
    dplyr::mutate(
      x = x2 + 0.12,
      y = label_y,
      y_line = centre,
      label = dplyr::case_when(
        scenario_id == "none" ~ paste0(name, ": ", label_n(n_group)),
        earlier ~ paste0(name, ": ", label_n(n_group), " more (", label_n(total), " in total)"),
        .default = paste0(name, ": ", label_n(n_group))
      )
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(x, y, group = step),
      data = parts$branches, fill = "grey75", colour = "grey75", linewidth = 0.2
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = parts$band, fill = "grey45", colour = "grey45", linewidth = 0.2
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(x, y, group = group),
      data = split, fill = group_tbl$colour[split$group],
      colour = group_tbl$colour[split$group], linewidth = 0.2
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x2 + 0.02, xend = x - 0.03, y = y_line, yend = y),
      data = group_labels, colour = "grey60", linewidth = 0.25
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = label),
      data = group_labels, hjust = 0, family = font, size = 3.2, colour = group_labels$text_colour,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x, 0.04, label = label),
      data = parts$stage_labels, vjust = 0, family = font, size = 3.2,
      colour = "grey15", lineheight = 0.95
    ) +
    ggplot2::annotate(
      "text", x = (x0 + x2) / 2, y = 0.04, vjust = 0, family = font, size = 3.2,
      colour = "grey15", lineheight = 0.95, label = "Scenario\n"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x, y, label = label),
      data = parts$labels, vjust = 1, family = font, size = 2.9,
      colour = "grey40", lineheight = 0.95
    ) +
    ggplot2::coord_cartesian(
      xlim = c(-0.55, x2 + 3.3),
      ylim = c(min(parts$ylim[1], min(dst_top - h) - 0.05), parts$ylim[2]),
      expand = FALSE, clip = "off"
    ) +
    ggplot2::labs(
      title = "From the well database to the scenarios",
      subtitle = paste0(
        "The band shows the wells left after each step, the grey branches ",
        "the wells dropped. At the end, each usable well is counted once, in ",
        "the first scenario it meets (in the order of the list)."
      ),
      caption = plot_config$caption
    ) +
    theme_maps(font)

  save_figure(
    p, file.path(plot_config$dir, "sankey_combined.png"),
    width = figure_width, height = 6.5
  )
}

# Even coverage scenario ------------------------------------------------------------

optimised_threshold_labels <- c(
  dtw_mean_max = "Limit of the mean depth (m)",
  dtw_max_extra = "Maximum depth: limit + ... (m)",
  top_below_max = "Screen top: up to ... m\nbelow the water table",
  screen_length_max = "Screen length up to ... (m)"
)

# The even coverage scenario: map of the cells with usable wells, covered or
# not by the selected wells, and for each of the four thresholds the covered
# cells along its grid, the other thresholds at the chosen values.
plot_coverage_optimisation <- function(optimisation, germany, plot_config,
                                       font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  s <- optimisation$scenario
  cells <- optimisation$cells |>
    dplyr::mutate(covered = selected > 0)

  map <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = dplyr::filter(cells, !covered), fill = "grey88", colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = dplyr::filter(cells, covered), fill = s$colour, alpha = 0.75,
      colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes((xmin + xmax) / 2, (ymin + ymax) / 2, label = selected),
      data = dplyr::filter(cells, covered), family = font, size = 2.3, colour = "white"
    ) +
    ggplot2::geom_sf(data = germany, fill = NA, colour = "grey40", linewidth = 0.25) +
    ggplot2::coord_sf(datum = NA, crs = sf::st_crs(germany)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 0))

  target <- optimisation$keep_share * optimisation$cells_ref
  profiles <- optimisation$profiles |>
    dplyr::mutate(threshold = factor(
      optimised_threshold_labels[threshold], levels = optimised_threshold_labels
    ))
  chosen <- dplyr::filter(profiles, chosen)
  curves <- ggplot2::ggplot(profiles, ggplot2::aes(value, cells)) +
    ggplot2::geom_hline(yintercept = target, colour = "grey60", linewidth = 0.3, linetype = "22") +
    ggplot2::geom_line(colour = "grey55", linewidth = 0.6) +
    ggplot2::geom_point(colour = "grey55", size = 1.1) +
    ggplot2::geom_point(data = chosen, colour = s$colour, size = 2.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      data = chosen, vjust = -1, family = font, size = 3.3, colour = s$colour, fontface = "bold"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(threshold), scales = "free_x", ncol = 2) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.2))) +
    ggplot2::labs(x = NULL, y = paste0("Covered ", optimisation$cell_km, " km cells")) +
    theme_charts(font, base_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, lineheight = 1),
      panel.spacing = grid::unit(1, "lines"),
      plot.margin = ggplot2::margin(10, 10, 0, 10)
    )

  figure <- patchwork::wrap_elements(full = map) + curves +
    patchwork::plot_layout(widths = c(1, 1.15)) +
    patchwork::plot_annotation(
      title = paste0(
        s$name, ": ", label_n(optimisation$wells_chosen), " wells in ",
        optimisation$cells_chosen, " of ", nrow(cells), " cells"
      ),
      subtitle = paste0(
        "Left: ", optimisation$cell_km, " km cells with usable wells, ",
        colour_word("covered", s$colour), " by the scenario (number of wells) or ",
        "not (grey). Right: the strictest thresholds that keep ",
        100 * optimisation$keep_share, " % of the cells the loosest ones cover ",
        "(dashed line); each panel varies one threshold, the others at the chosen values."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font) +
        ggplot2::theme(plot.subtitle = element_subtitle(margin = ggplot2::margin(t = 4, b = 8)))
    )

  save_figure(figure, file.path(plot_config$dir, "scenario_optimisation.png"), width = figure_width, height = 7.5)
}

# Missing screen depths -----------------------------------------------------------------

# Where screen depths are missing: map of the wells that have the ground
# elevation and valid groundwater levels (the step before the screen
# depths), coloured by which screen depth is missing, and the share per
# state (prefix of the well ID, e.g. BB_).
plot_missing_screens <- function(points, germany, plot_config,
                                 font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  categories <- c(
    "both missing" = "#E4572E",
    "screen top missing" = "#F0A202",
    "screen bottom missing" = "#2E86AB",
    "complete" = "grey72"
  )
  base <- points |>
    dplyr::filter(in_meta, !is.na(elev_gs), dplyr::coalesce(n_obs_valid, 0) > 0) |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(screen_top) & is.na(screen_bot) ~ "both missing",
        is.na(screen_top) ~ "screen top missing",
        is.na(screen_bot) ~ "screen bottom missing",
        .default = "complete"
      ),
      status = factor(status, levels = names(categories)),
      state = ifelse(grepl("_", well_id), sub("_.*$", "", well_id), "other")
    )
  n_missing <- sum(base$status != "complete")

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = germany, fill = "grey95", colour = "grey70", linewidth = 0.2) +
    ggplot2::geom_point(
      ggplot2::aes(X, Y),
      data = dplyr::filter(base, status == "complete"),
      colour = categories[["complete"]], size = 0.3, stroke = 0
    ) +
    ggplot2::geom_point(
      ggplot2::aes(X, Y, colour = status),
      data = dplyr::filter(base, status != "complete"),
      size = 0.6, stroke = 0
    ) +
    ggplot2::scale_colour_manual(values = categories, guide = "none") +
    ggplot2::coord_sf(datum = NA, crs = sf::st_crs(germany)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 0))

  by_state <- base |>
    dplyr::count(state, status) |>
    dplyr::mutate(share = n / sum(n), n_state = sum(n), .by = state) |>
    dplyr::mutate(
      missing_share = sum(share[status != "complete"]),
      .by = state
    ) |>
    dplyr::mutate(state = stats::reorder(state, missing_share))
  totals <- dplyr::distinct(by_state, state, n_state, missing_share)

  bars <- ggplot2::ggplot(by_state, ggplot2::aes(share, state, fill = status)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.02, y = state, label = paste0(round(100 * missing_share), " % of ", label_n(n_state))),
      data = totals, inherit.aes = FALSE, hjust = 0, family = font, size = 3.1, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(values = categories, name = NULL) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(), breaks = seq(0, 1, 0.25),
      expand = ggplot2::expansion(c(0, 0)), limits = c(0, 1.32)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Share of the wells", y = "State (prefix of the well ID)") +
    theme_charts(font, base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "left",
      plot.margin = ggplot2::margin(10, 10, 0, 10)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))

  figure <- patchwork::wrap_elements(full = map) + bars +
    patchwork::plot_layout(widths = c(1, 1)) +
    patchwork::plot_annotation(
      title = paste0("Missing screen depths: ", label_n(n_missing), " wells"),
      subtitle = paste0(
        "Wells with ground surface elevation and valid groundwater levels, ",
        "the step before the screen depths: ", label_n(n_missing), " of ",
        label_n(nrow(base)), " (", round(100 * n_missing / max(nrow(base), 1)),
        " %) lack the screen top, the bottom or both. Right: share per state ",
        "and the number of wells there."
      ),
      caption = plot_config$caption,
      theme = theme_maps(font) +
        ggplot2::theme(plot.subtitle = element_subtitle(margin = ggplot2::margin(t = 4, b = 8)))
    )
  save_figure(figure, file.path(plot_config$dir, "missing_screen_depths.png"), width = figure_width, height = 7.5)
}
