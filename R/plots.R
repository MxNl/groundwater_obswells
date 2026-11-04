# Figures ------------------------------------------------------------------------

label_n <- function(x) {
  scales::label_comma()(x)
}

# The four properties of the scenarios, as rows of the distribution chart.
scenario_properties <- c(
  dtw_mean = "Mean depth to\ngroundwater",
  dtw_deepest = "Maximum depth to\ngroundwater",
  screen_top = "Depth of the\nscreen top",
  screen_length = "Screen length"
)

# Allowed range of a property in a scenario, NA where the criterion is not a
# fixed range (the screen top lies relative to the water table).
scenario_ranges <- function(s) {
  tibble::tibble(
    property = factor(names(scenario_properties), levels = rev(names(scenario_properties))),
    from = c(s$dtw_mean_min, 0, NA, 0),
    to = c(s$dtw_mean_max, s$dtw_max, NA, s$screen_length_max)
  )
}

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
      colour = "grey72", size = 0.35, stroke = 0
    ) +
    ggplot2::geom_point(
      ggplot2::aes(X, Y),
      data = dplyr::filter(usable, selected),
      colour = colour, size = 1.1, stroke = 0
    ) +
    ggplot2::coord_sf(datum = NA, crs = sf::st_crs(germany)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 0, 0))

  # number of wells and the wells left after each criterion
  criteria <- describe_scenario(s)
  counts <- tibble::tibble(
    label = c("Usable wells", unname(criteria)),
    n = c(n_usable, step$dtw_mean, step$dtw_max, step$screen_position, step$screen_length),
    y = -seq_along(label)
  )
  numbers <- ggplot2::ggplot(counts) +
    ggplot2::annotate(
      "text", x = 0, y = 1.6, label = label_n(n_selected), hjust = 0, vjust = 0,
      family = font, fontface = "bold", size = 18, colour = colour
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 1.15, hjust = 0, vjust = 1, family = font, size = 4.6,
      colour = "grey25", lineheight = 1.1,
      label = paste0(
        "wells, ", scales::label_percent(accuracy = 0.1)(n_selected / max(n_usable, 1)),
        " of the ", label_n(n_usable), " usable wells"
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0, y = y, label = label),
      hjust = 0, family = font, size = 3.7, colour = "grey35"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = y, label = label_n(n)),
      hjust = 1, family = font, size = 3.7, colour = "grey15", fontface = "bold"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 1, y = y - 0.5, yend = y - 0.5),
      colour = "grey85", linewidth = 0.3
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(limits = c(-nrow(counts) - 0.6, 3.4), expand = ggplot2::expansion(0)) +
    theme_maps(font) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 20, 10, 20))

  # distributions of the four properties
  long <- function(data, group) {
    tidyr::pivot_longer(
      data, dplyr::all_of(names(scenario_properties)),
      names_to = "property", values_to = "value"
    ) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(
        property = factor(property, levels = rev(names(scenario_properties))),
        group = group
      )
  }
  all_long <- long(usable, "usable")
  sel_long <- long(dplyr::filter(usable, selected), "selected")
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
      data = sel_long, shape = "|", size = 2, colour = colour, alpha = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, x_max, 2), expand = ggplot2::expansion(0),
      labels = function(x) paste(x, "m")
    ) +
    ggplot2::scale_y_discrete(
      labels = scenario_properties, expand = ggplot2::expansion(add = c(0.25, 1))
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(
      x = paste0("m below ground surface (shown up to ", x_max, " m)"),
      y = NULL
    ) +
    theme_charts(font) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(vjust = 0, hjust = 1, lineheight = 1.05),
      axis.title.x = ggplot2::element_text(colour = "grey35", size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(10, 20, 0, 20)
    )

  subtitle <- paste0(
    s$purpose, " Of the usable wells, the scenario keeps those that meet all ",
    "four criteria: ", paste(tolower(substring(criteria, 1, 1)), substring(criteria, 2), sep = "", collapse = "; "),
    ". On the map: the ", colour_word("selected wells", colour), " among all ",
    "usable wells (grey). On the right: the wells left after each criterion and ",
    "the distribution of the four properties, usable wells in grey and the ",
    colour_word("selected wells", colour), " in colour (each curve scaled to ",
    "its own maximum; ticks: single wells; shaded: the allowed range)."
  )

  figure <- patchwork::wrap_elements(full = map) + numbers + ridges +
    patchwork::plot_layout(design = "AB\nAC", widths = c(1.15, 1), heights = c(0.62, 1)) +
    patchwork::plot_annotation(
      title = paste0(s$name, ": ", label_n(n_selected), " wells"),
      subtitle = subtitle,
      caption = plot_config$caption,
      theme = theme_maps(font)
    )

  save_figure(
    figure,
    file.path(plot_config$dir, paste0("scenario_", s$id, ".png")),
    width = 16, height = 11.5
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

# Flow chart of the analysis: the wells left after each step from the
# metadata to the usable wells, what drops out, and the scenarios.
plot_flowchart <- function(wells, scenarios, steps, spec, plot_config,
                           font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  m <- dplyr::filter(wells, in_meta)
  has_gs <- !is.na(m$elev_gs)
  has_gwl <- has_gs & !is.na(m$gwl_mean)
  has_screen <- has_gwl & !is.na(m$screen_top) & !is.na(m$screen_bot)
  recent <- has_screen & m$recent %in% TRUE
  covered <- recent & dplyr::coalesce(m$coverage_window, 0) >= spec$min_coverage
  usable <- covered & m$usable

  chain <- tibble::tibble(
    label = c(
      "Observation wells in the metadata",
      "with the elevation of the ground surface",
      "with valid groundwater levels",
      "with screen depths",
      paste("with data in the last", spec$recent_years, "years"),
      paste0("at least ", 100 * spec$min_coverage, " % of the months covered"),
      "Usable wells"
    ),
    detail = c(
      "",
      "",
      "error codes, implausible depths and spikes removed",
      "",
      "",
      "",
      if (isTRUE(spec$require_all_seasons)) "and data in all four seasons" else ""
    ),
    n = c(nrow(m), sum(has_gs), sum(has_gwl), sum(has_screen), sum(recent), sum(covered), sum(usable)),
    y = -(seq_along(label) - 1) * 1.25
  ) |>
    dplyr::mutate(
      dropped = dplyr::lag(n) - n,
      reason = c(
        NA,
        "without ground surface elevation",
        "without valid groundwater levels",
        "without screen depths",
        "without recent data",
        "with too many gaps",
        if (isTRUE(spec$require_all_seasons)) "with a season missing" else "other"
      )
    )

  box_w <- 6.2
  box_h <- 0.82
  n_sc <- nrow(scenarios)
  sc_w <- 2.75
  sc_gap <- 0.3
  sc_x <- (seq_len(n_sc) - (n_sc + 1) / 2) * (sc_w + sc_gap)
  sc_y <- min(chain$y) - 2.6
  sc_h <- 1.9
  bus_y <- min(chain$y) - box_h / 2 - 0.55

  sc <- scenarios |>
    dplyr::mutate(x = sc_x) |>
    dplyr::left_join(steps, by = c(id = "scenario_id"), suffix = c("", "_steps")) |>
    dplyr::mutate(
      short = vapply(seq_len(dplyr::n()), function(i) {
        d <- describe_scenario(scenarios[i, ])
        rule <- if (scenarios$screen_rule[i] == "below") {
          paste0("screen top 0\u2013", scenarios$top_below_max[i], " m below the\nwater table, bottom deeper\nthan the annual low")
        } else {
          "screen across the water\ntable, bottom deeper\nthan the annual low"
        }
        paste0(
          "mean ", scenarios$dtw_mean_min[i], "–", scenarios$dtw_mean_max[i],
          " m, max < ", scenarios$dtw_max[i], " m\n", rule,
          "\nscreen ≤ ", scenarios$screen_length_max[i], " m"
        )
      }, character(1))
    )

  arrow <- grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")

  p <- ggplot2::ggplot() +
    # arrows of the chain and what drops out
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 0, y = y + 1.25 - box_h / 2, yend = y + box_h / 2),
      data = chain[-1, ], colour = "grey55", linewidth = 0.4, arrow = arrow
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0.25, y = y + 0.625,
        label = paste0("− ", label_n(dropped), " ", reason)
      ),
      data = chain[-1, ], hjust = 0, family = font, size = 3.5, colour = "grey50",
      fontface = "italic"
    ) +
    # boxes of the chain
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -box_w / 2, xmax = box_w / 2, ymin = y - box_h / 2, ymax = y + box_h / 2),
      data = chain, fill = "grey96", colour = "grey60", linewidth = 0.3
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -box_w / 2, xmax = box_w / 2, ymin = y - box_h / 2, ymax = y + box_h / 2),
      data = chain[nrow(chain), ], fill = "grey85", colour = "grey35", linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = -box_w / 2 + 0.2, y = y + ifelse(detail == "", 0, 0.15),
        label = label
      ),
      data = chain, hjust = 0, family = font, size = 4.3, colour = "grey15",
      fontface = ifelse(seq_len(nrow(chain)) %in% c(1, nrow(chain)), "bold", "plain")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = -box_w / 2 + 0.2, y = y - 0.2, label = detail),
      data = dplyr::filter(chain, detail != ""), hjust = 0, family = font,
      size = 2.9, colour = "grey40", lineheight = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = box_w / 2 - 0.2, y = y, label = label_n(n)),
      data = chain, hjust = 1, family = font, size = 4.6, colour = "grey15",
      fontface = "bold"
    ) +
    # branches to the scenarios
    ggplot2::annotate(
      "segment", x = 0, xend = 0, y = min(chain$y) - box_h / 2, yend = bus_y,
      colour = "grey55", linewidth = 0.4
    ) +
    ggplot2::annotate(
      "segment", x = min(sc_x), xend = max(sc_x), y = bus_y, yend = bus_y,
      colour = "grey55", linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = x, y = bus_y, yend = sc_y + sc_h / 2),
      data = sc, colour = "grey55", linewidth = 0.4, arrow = arrow
    ) +
    ggplot2::annotate(
      "text", x = max(sc_x) + sc_w / 2, y = bus_y + 0.22, hjust = 1,
      label = "Scenarios: each one filters the usable wells by four criteria",
      family = font, size = 3.5, colour = "grey50", fontface = "italic"
    ) +
    # scenario boxes
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = x - sc_w / 2, xmax = x + sc_w / 2,
        ymin = sc_y - sc_h / 2, ymax = sc_y + sc_h / 2
      ),
      data = sc, fill = scales::alpha(sc$colour, 0.12), colour = sc$colour, linewidth = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = sc_y + sc_h / 2 - 0.25, label = name),
      data = sc, family = font, size = 4, fontface = "bold", colour = "grey15"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = sc_y + sc_h / 2 - 0.72, label = label_n(screen_length)),
      data = sc, family = font, size = 7, fontface = "bold", colour = sc$colour
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x, y = sc_y - 0.42, label = short),
      data = sc, family = font, size = 2.9, colour = "grey35", lineheight = 1
    ) +
    ggplot2::coord_cartesian(
      xlim = range(c(sc_x - sc_w / 2, sc_x + sc_w / 2, -box_w / 2, box_w / 2 + 4)),
      expand = TRUE, clip = "off"
    ) +
    ggplot2::labs(
      title = "From the well database to the scenarios",
      subtitle = paste0(
        "Each box shows the wells left after a step. The mean depth to ",
        "groundwater is the ground surface elevation minus the mean of the ",
        "monthly groundwater levels in the last ", spec$window_years,
        " years with data. Usable wells have screen depths, recent data and ",
        "enough months covered; the scenarios select wells with shallow ",
        "groundwater and a screen close below the water table."
      ),
      caption = plot_config$caption
    ) +
    theme_maps(font)

  save_figure(p, file.path(plot_config$dir, "flowchart.png"), width = 16, height = 13)
}
