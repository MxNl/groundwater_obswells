# Plot setup: font, themes, borders ----------------------------------------------
#
# The same design as the agriculture_crops plots: Noto Sans Display (BGR
# corporate design), bold centred title, grey justified subtitle in marquee
# markdown ({#E4572E **word**} for a coloured word), small grey caption,
# Natural Earth borders.

# Registers the plot font. A font that is not installed is downloaded from
# Google Fonts into cache/fonts/ (and registered from there in later
# sessions); without it the system's sans-serif font is used. Returns the
# font family to use.
setup_plot_font <- function(font) {
  cached <- list.files("cache/fonts", "\\.(ttf|otf)$", full.names = TRUE, recursive = TRUE)
  if (length(cached) > 0 && exists("add_fonts", asNamespace("systemfonts"))) {
    systemfonts::add_fonts(cached)
  }
  if (font %in% systemfonts::system_fonts()$family) {
    return(font)
  }
  if (exists("require_font", asNamespace("systemfonts"))) {
    dir.create("cache/fonts", showWarnings = FALSE, recursive = TRUE)
    ok <- systemfonts::require_font(
      font,
      fallback = "sans", dir = "cache/fonts", error = FALSE, verbose = FALSE
    )
    if (isTRUE(ok) || font %in% systemfonts::registry_fonts()$family) {
      return(font)
    }
  }
  warning("Font ", font, " not available, using sans.", call. = FALSE)
  "sans"
}

# a word in bold and a colour (marquee markdown)
colour_word <- function(word, colour) {
  paste0("{", colour, " **", word, "**}")
}

# subtitle as marquee markdown, justified, wrapped to the plot width by
# save_figure(); plain text if marquee is not installed
element_subtitle <- function(margin = ggplot2::margin(t = 6, b = 16), ...) {
  if (!requireNamespace("marquee", quietly = TRUE)) {
    return(ggplot2::element_text(colour = "grey35", lineheight = 1.2, margin = margin, ...))
  }
  # line height in the style: marquee ignores the element's (default 1.6)
  style <- marquee::classic_style(align = "center", lineheight = 1.2, margin = marquee::trbl(0))
  style <- marquee::modify_style(style, "p", align = "justified-center")
  marquee::element_marquee(
    colour = "grey35",
    width = grid::unit(1, "npc"),
    margin = margin,
    style = style,
    ...
  )
}

theme_maps <- function(font, base_size = 14) {
  ggplot2::theme_void(base_size = base_size, base_family = font) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.4), hjust = 0.5),
      plot.subtitle = element_subtitle(),
      plot.caption = ggplot2::element_text(
        colour = "grey50", size = ggplot2::rel(0.7), margin = ggplot2::margin(t = 14)
      ),
      plot.margin = ggplot2::margin(10, 15, 10, 15),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

theme_charts <- function(font, base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size, base_family = font) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.4), hjust = 0.5),
      plot.subtitle = element_subtitle(),
      plot.caption = ggplot2::element_text(
        colour = "grey50", size = ggplot2::rel(0.7), margin = ggplot2::margin(t = 18)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 20, 15, 20),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

# Saves a ggplot or patchwork as PNG and returns the path (for targets with
# format = "file"). Width and height in inches. Marquee subtitles are wrapped
# to the width of the figure within its margins: their height is measured
# before the layout, so a relative width would wrap them into more lines than
# measured.
save_figure <- function(plot, path, width, height) {
  fit_subtitle <- function(theme) {
    subtitle <- theme$plot.subtitle
    if (!inherits(subtitle, "element_marquee")) {
      return(theme)
    }
    margin <- theme$plot.margin %||% ggplot2::theme_grey()$plot.margin
    subtitle$width <- grid::unit(width, "in") - margin[2] - margin[4]
    theme + ggplot2::theme(plot.subtitle = subtitle, plot.title.position = "plot")
  }
  if (inherits(plot, "patchwork")) {
    plot$patches$annotation$theme <- fit_subtitle(plot$patches$annotation$theme)
  } else {
    plot$theme <- fit_subtitle(plot$theme)
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    path, plot,
    width = width, height = height, dpi = 300, bg = "white",
    device = ragg::agg_png
  )
  path
}

# State borders of Germany (Natural Earth 1:10 m, downloaded into
# cache/naturalearth once), in `crs`. Without rnaturalearth: the country
# outline from rnaturalearthdata (1:50 m).
read_germany <- function(crs = 25832) {
  if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    destdir <- "cache/naturalearth"
    dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
    args <- list(
      scale = 10, type = "admin_1_states_provinces", category = "cultural",
      destdir = destdir
    )
    # ne_download() returns only the file name when it downloads, so the
    # layer is always read with ne_load() afterwards
    states <- try(do.call(rnaturalearth::ne_load, c(args, returnclass = "sf")), silent = TRUE)
    if (inherits(states, "try-error")) {
      do.call(rnaturalearth::ne_download, c(args, load = FALSE))
      states <- do.call(rnaturalearth::ne_load, c(args, returnclass = "sf"))
    }
    states <- states[states$adm0_a3 == "DEU", ]
  } else {
    countries <- rnaturalearthdata::countries50
    states <- sf::st_as_sf(countries[countries$adm0_a3 == "DEU", ])
  }
  sf::st_transform(sf::st_geometry(states), crs)
}
