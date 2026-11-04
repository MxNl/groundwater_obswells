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

# State borders of Germany (Natural Earth 1:10 m), in `crs`. The zip is
# downloaded once into cache/naturalearth/ with R's own download (proxy and
# certificate settings of R; on Windows also the system's Internet settings)
# and read locally: rnaturalearth reads it through GDAL directly from the
# web, which fails behind many company proxies. If the download fails, the
# country outline of the rnaturalearthdata package (1:50 m, no state
# borders, no download) is used. To provide the file by hand, put
# ne_10m_admin_1_states_provinces.zip, or the unpacked shapefile as
# rnaturalearth stores it (e.g. copied from another project's cache), into
# cache/naturalearth/.
read_germany <- function(crs = 25832) {
  destdir <- "cache/naturalearth"
  layer <- "ne_10m_admin_1_states_provinces"
  shp <- file.path(destdir, paste0(layer, ".shp"))
  zip <- file.path(destdir, paste0(layer, ".zip"))
  url <- paste0("https://naciscdn.org/naturalearth/10m/cultural/", layer, ".zip")

  if (!file.exists(shp) && !file.exists(zip)) {
    dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
    download_zip(url, zip)
  }
  source <- if (file.exists(shp)) {
    shp
  } else if (file.exists(zip)) {
    paste0("/vsizip/", normalizePath(zip, winslash = "/"), "/", layer, ".shp")
  }
  if (!is.null(source)) {
    states <- sf::st_read(source, quiet = TRUE)
    states <- states[states$adm0_a3 == "DEU", ]
  } else {
    if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
      stop(
        "Could not download ", url, " and rnaturalearthdata is not installed. ",
        "Install rnaturalearthdata, or download the zip by hand into ", destdir, ".",
        call. = FALSE
      )
    }
    warning(
      "Could not download the Natural Earth state borders, the maps show the ",
      "country outline only. Download ", url, " by hand into ", destdir,
      " to get the state borders.",
      call. = FALSE
    )
    countries <- rnaturalearthdata::countries50
    states <- sf::st_as_sf(countries[countries$adm0_a3 == "DEU", ])
  }
  sf::st_transform(sf::st_geometry(states), crs)
}

# Downloads `url` to `path`; tries the default method and on Windows also
# "wininet" (system proxy settings). Leaves no partial file behind.
download_zip <- function(url, path) {
  methods <- c("auto", if (.Platform$OS.type == "windows") "wininet")
  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  for (method in methods) {
    ok <- tryCatch(
      utils::download.file(url, path, mode = "wb", method = method, quiet = TRUE) == 0,
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (ok && file.exists(path) && file.size(path) > 0) {
      return(invisible(TRUE))
    }
    unlink(path)
  }
  invisible(FALSE)
}
