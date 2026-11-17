# Drop-out per state ------------------------------------------------------------------
#
# Where the wells of each state drop out on the way to the usable wells, and
# how many each scenario selects. The state is the prefix of the well ID
# (e.g. BB_25470023 -> BB); IDs without prefix count as "other".

well_state <- function(well_id) {
  ifelse(grepl("_", well_id, fixed = TRUE), sub("_.*$", "", well_id), "other")
}

# The steps from the metadata to the usable wells, as logical columns that
# hold for the wells still in after each step (the same steps as
# well_stages()).
stage_flags <- function(m) {
  has_gs <- !is.na(m$elev_gs)
  has_gwl <- has_gs & dplyr::coalesce(m$n_obs_valid, 0) > 0
  has_screen <- has_gwl & !is.na(m$screen_top) & !is.na(m$screen_bot)
  recent <- has_screen & m$recent %in% TRUE
  usable <- recent & m$usable %in% TRUE
  tibble::tibble(
    has_gs = has_gs, has_gwl = has_gwl, has_screen = has_screen,
    recent = recent, usable = usable
  )
}

# One row per state (and "Germany" for all): wells in the metadata, wells
# lost at each step, usable wells and wells selected per scenario.
state_attrition <- function(wells, scenario_wells, scenarios, spec) {
  m <- dplyr::filter(wells, in_meta)
  flags <- stage_flags(m)
  per_well <- tibble::tibble(state = well_state(m$well_id), well_id = m$well_id) |>
    dplyr::bind_cols(flags)
  selected <- scenario_wells |>
    dplyr::filter(selected) |>
    dplyr::count(well_id, scenario_id) |>
    tidyr::pivot_wider(names_from = scenario_id, values_from = n, values_fill = 0)
  per_well <- dplyr::left_join(per_well, selected, by = "well_id")
  for (id in scenarios$id) {
    per_well[[id]] <- dplyr::coalesce(per_well[[id]], 0L) > 0
  }

  summarise_group <- function(d) {
    out <- tibble::tibble(
      n_meta = nrow(d),
      lost_gs = sum(!d$has_gs),
      lost_gwl = sum(d$has_gs & !d$has_gwl),
      lost_screen = sum(d$has_gwl & !d$has_screen),
      lost_recent = sum(d$has_screen & !d$recent),
      lost_seasons = sum(d$recent & !d$usable),
      usable = sum(d$usable)
    )
    for (id in scenarios$id) out[[id]] <- sum(d[[id]])
    out
  }
  by_state <- per_well |>
    dplyr::group_by(state) |>
    dplyr::group_modify(~ summarise_group(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(n_meta))
  dplyr::bind_rows(by_state, dplyr::mutate(summarise_group(per_well), state = "Germany"))
}

# Heat map: per state the share of its wells lost at each step (red), and the
# usable and selected wells (grey and scenario colours), numbers in the cells.
plot_state_attrition <- function(attrition, scenarios, spec, plot_config,
                                 font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  steps <- c(
    lost_gs = "no ground\nelevation",
    lost_gwl = "no valid\nlevels",
    lost_screen = "no screen\ndepths",
    lost_recent = paste0("no data in\nlast ", spec$recent_years, " years"),
    lost_seasons = paste0("season in\n< ", spec$min_years_per_season, " years"),
    usable = "usable\nwells"
  )
  sc_names <- stats::setNames(gsub(" ", "\n", scenarios$name, fixed = TRUE), scenarios$id)
  sc_names <- gsub("\nat\nthe\nwater\ntable", "\nat the water\ntable", sc_names, fixed = TRUE)
  columns <- c(steps, sc_names)

  states <- attrition$state
  long <- attrition |>
    tidyr::pivot_longer(dplyr::all_of(names(columns)), names_to = "column", values_to = "n") |>
    dplyr::mutate(
      share = n / pmax(n_meta, 1),
      group = dplyr::case_when(
        startsWith(column, "lost_") ~ "Lost at each step",
        column == "usable" ~ "Left",
        .default = "Selected per scenario"
      ),
      group = factor(group, levels = c("Lost at each step", "Left", "Selected per scenario")),
      column = factor(column, levels = names(columns)),
      state_label = factor(
        paste0(state, "  (", label_n(n_meta), ")"),
        levels = rev(paste0(states, "  (", label_n(attrition$n_meta), ")"))
      ),
      fill = dplyr::case_when(
        group == "Lost at each step" ~ scales::col_numeric(
          c("white", "#E4572E"), domain = c(0, 1)
        )(pmin(share * 2, 1)),
        group == "Left" ~ scales::col_numeric(c("white", "grey45"), domain = c(0, 1))(share),
        .default = scales::alpha(
          scenarios$colour[match(as.character(column), scenarios$id)],
          0.1 + 0.9 * pmin(share * 4, 1)
        )
      ),
      text_colour = ifelse(
        (group == "Lost at each step" & share > 0.3) | (group == "Left" & share > 0.55) |
          (group == "Selected per scenario" & share > 0.15),
        "white", "grey15"
      ),
      label = ifelse(n == 0, "\u2013", paste0(label_n(n), "\n", round(100 * share), " %"))
    )

  p <- ggplot2::ggplot(long, ggplot2::aes(column, state_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill), colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, colour = text_colour),
      family = font, size = 2.6, lineheight = 0.9
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(labels = columns, position = "top") +
    ggplot2::facet_grid(cols = ggplot2::vars(group), scales = "free_x", space = "free_x") +
    ggplot2::labs(
      title = "Where the wells drop out, per state",
      subtitle = paste0(
        "Per state (prefix of the well ID, FARM wells in brackets): ",
        "wells lost at each step towards the usable wells, the usable wells, and ",
        "the wells each scenario selects, as numbers and shares of the state's ",
        "FARM wells."
      ),
      x = NULL, y = NULL,
      caption = plot_config$caption
    ) +
    theme_charts(font, base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1)),
      axis.text.x.top = ggplot2::element_text(lineheight = 0.95, size = ggplot2::rel(0.85), vjust = 0),
      axis.text.y = ggplot2::element_text(colour = "grey15"),
      panel.spacing.x = grid::unit(0.8, "lines")
    )

  height <- 2.6 + 0.42 * nrow(attrition)
  save_figure(p, file.path(plot_config$dir, "state_attrition.png"), width = figure_width, height = height)
}
