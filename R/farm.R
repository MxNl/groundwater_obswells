# FARM master data, filled from gw_base ----------------------------------------------
#
# The wells analysed are those of the FARM master data (farm_config$file).
# Where FARM has no ground elevation, screen top or bottom or coordinates,
# the value of the same well in gw_base (tbl_meta_well_import) is used. The
# wells are linked by state and ID (state_id_key(): state prefix such as
# "HE_", signs and leading zeros removed), as in the source check. The
# groundwater levels come from gw_base (tbl_gwl_raw) under the gw_base well
# ID, so FARM wells not found in gw_base have no levels.

# fields filled from gw_base (and the coordinates)
farm_filled_fields <- c("elev_gs", "screen_top", "screen_bot")

# The FARM table: the columns of farm_config$columns (names as in the file;
# case, umlauts, spaces and signs do not matter), numbers parsed. State from
# the state column (codes or names) or the ID prefix.
read_farm_meta <- function(path, config) {
  d <- read_table_file(path)
  map <- unlist(config$columns)
  found <- match(norm_name(map), norm_name(names(d)))
  if (is.na(found[names(map) == "id"])) {
    stop("FARM: ID column ", map[["id"]], " not found in ", path, ". Columns: ",
         paste(names(d), collapse = ", "), call. = FALSE)
  }
  if (any(is.na(found))) {
    warning("FARM: columns not found in ", basename(path), ": ",
            paste(map[is.na(found)], collapse = ", "), call. = FALSE)
  }
  column <- function(field) {
    i <- found[names(map) == field]
    if (length(i) == 0 || is.na(i)) rep(NA_character_, nrow(d)) else as.character(d[[i]])
  }
  state_raw <- column("state")
  unknown <- unique(state_raw[!is.na(state_raw) & nzchar(trimws(state_raw)) & is.na(norm_state(state_raw))])
  if (length(unknown) > 0) {
    warning("FARM: state values not recognised: ", paste(utils::head(unknown, 10), collapse = ", "),
            call. = FALSE)
  }
  id <- trimws(column("id"))
  tibble::tibble(
    farm_row = seq_len(nrow(d)),
    farm_id = ifelse(nzchar(id), id, NA_character_),
    state = dplyr::coalesce(norm_state(state_raw), state_from_id(id)),
    elev_gs = parse_num(column("elev_gs")),
    elev_mp = parse_num(column("elev_mp")),
    screen_top = parse_num(column("screen_top")),
    screen_bot = parse_num(column("screen_bot")),
    x = parse_num(column("x")),
    y = parse_num(column("y")),
    epsg = gsub("[^0-9]", "", column("epsg"))
  )
}

# The analysis table: one row per FARM record, the values of FARM where
# given, else those of the linked gw_base well. Columns:
#   well_id   gw_base well ID (for the levels); for FARM wells not found in
#             gw_base "<state>_<FARM ID>"
#   farm_id, state, in_gw_base
#   elev_gs, elev_mp (FARM only), screen_top, screen_bot, x, y (EPSG 25832)
#   <field>_from, coords_from: "FARM", "gw_base", "missing in both" or
#             "missing, not in gw_base"
#   coords_epsg_from: how the FARM coordinates were transformed
# `gwbase`: fetch_well_meta() (coordinates in EPSG `crs`; NULL: guessed).
fill_farm_meta <- function(farm, gwbase, crs = NULL) {
  gw <- gwbase |>
    dplyr::mutate(gw_state = state_from_id(well_id), key = state_id_key(gw_state, well_id)) |>
    dplyr::filter(!is.na(key)) |>
    dplyr::distinct(key, .keep_all = TRUE)
  gw_xy <- coords_to_25832(gw$x, gw$y, crs %||% NA)
  gw$x <- gw_xy$x
  gw$y <- gw_xy$y

  farm$key <- state_id_key(farm$state, farm$farm_id)
  # without a state: by the ID alone, where it is unique in gw_base
  id_only <- norm_id(gw$well_id)
  unique_ids <- id_only[!id_only %in% id_only[duplicated(id_only)]]
  no_state <- is.na(farm$state) & norm_id(farm$farm_id) %in% unique_ids
  farm$key[no_state] <- gw$key[match(norm_id(farm$farm_id[no_state]), id_only)]

  j <- dplyr::left_join(
    farm,
    dplyr::select(gw, key, gw_well_id = well_id, gw_elev_gs = elev_gs, gw_screen_top = screen_top,
                  gw_screen_bot = screen_bot, gw_x = x, gw_y = y),
    by = "key"
  )
  linked <- !is.na(j$gw_well_id)
  source_of <- function(farm_value, gw_value) {
    dplyr::case_when(
      !is.na(farm_value) ~ "FARM",
      !is.na(gw_value) ~ "gw_base",
      linked ~ "missing in both",
      .default = "missing, not in gw_base"
    )
  }
  farm_xy <- coords_to_25832(j$x, j$y, j$epsg)
  out <- tibble::tibble(
    well_id = dplyr::coalesce(
      j$gw_well_id, paste0(dplyr::coalesce(j$state, "unknown"), "_", j$farm_id)
    ),
    farm_id = j$farm_id,
    state = j$state,
    in_gw_base = linked,
    elev_gs = dplyr::coalesce(j$elev_gs, j$gw_elev_gs),
    elev_mp = j$elev_mp,
    screen_top = dplyr::coalesce(j$screen_top, j$gw_screen_top),
    screen_bot = dplyr::coalesce(j$screen_bot, j$gw_screen_bot),
    x = dplyr::coalesce(farm_xy$x, j$gw_x),
    y = dplyr::coalesce(farm_xy$y, j$gw_y),
    elev_gs_from = source_of(j$elev_gs, j$gw_elev_gs),
    screen_top_from = source_of(j$screen_top, j$gw_screen_top),
    screen_bot_from = source_of(j$screen_bot, j$gw_screen_bot),
    coords_from = source_of(farm_xy$x, j$gw_x),
    coords_epsg_from = farm_xy$from
  )
  dplyr::filter(out, !is.na(farm_id))
}

# The ground elevations of the analysis wells found in gw_base, for the
# level queries on the server (the check of the depth below ground): one
# row per gw_base well ID.
well_elevations <- function(meta) {
  meta |>
    dplyr::filter(in_gw_base) |>
    dplyr::distinct(well_id, .keep_all = TRUE) |>
    dplyr::select(well_id, elev_gs) |>
    dplyr::arrange(well_id)
}

# Outputs -------------------------------------------------------------------------------

farm_fill_colours <- c(
  "FARM" = "#2E86AB", "gw_base" = "#F0A202",
  "missing in both" = "#E4572E", "missing, not in gw_base" = "grey70"
)
farm_field_labels <- c(
  elev_gs = "Ground elevation", screen_top = "Screen top", screen_bot = "Screen bottom",
  coords = "Coordinates"
)

# Per field and state (and Germany): the FARM wells by where the value comes
# from.
summarise_farm_filling <- function(meta) {
  long <- tidyr::pivot_longer(
    dplyr::mutate(meta, state = dplyr::coalesce(state, "unknown")),
    c(elev_gs_from, screen_top_from, screen_bot_from, coords_from),
    names_to = "field", values_to = "from"
  ) |>
    dplyr::mutate(field = sub("_from$", "", field))
  dplyr::bind_rows(long, dplyr::mutate(long, state = "Germany")) |>
    dplyr::count(state, field, from) |>
    dplyr::mutate(share = n / sum(n), .by = c(state, field))
}

# Figure of the filling and the table behind it (output/farm_filling.csv)
# and the filled analysis table (output/well_meta_filled.csv). Returns the
# paths.
plot_farm_filling <- function(meta, plot_config, font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  counts <- summarise_farm_filling(meta)
  dir.create("output", showWarnings = FALSE)
  tables <- c(
    write_csv_file(counts, file.path("output", "farm_filling.csv")),
    write_csv_file(meta, file.path("output", "well_meta_filled.csv"))
  )
  states <- setdiff(sort(unique(counts$state)), "Germany")
  d <- counts |>
    dplyr::mutate(
      from = factor(from, levels = names(farm_fill_colours)),
      field = factor(farm_field_labels[field], levels = farm_field_labels),
      state = factor(state, levels = rev(c("Germany", states)))
    )
  filled <- d |>
    dplyr::filter(from == "gw_base") |>
    dplyr::select(state, field, n_filled = n)
  totals <- d |>
    dplyr::summarise(n = sum(n), .by = c(state, field)) |>
    dplyr::left_join(filled, by = c("state", "field")) |>
    dplyr::mutate(label = paste0("+", label_n(dplyr::coalesce(n_filled, 0L))))
  p <- ggplot2::ggplot(d, ggplot2::aes(share, state, fill = from)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE), show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.03, y = state, label = label),
      data = totals, inherit.aes = FALSE, hjust = 0, family = font, size = 2.6, colour = "#B87A00"
    ) +
    ggplot2::scale_fill_manual(values = farm_fill_colours, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(), breaks = c(0, 0.5, 1),
      limits = c(0, 1.3), expand = ggplot2::expansion(0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::facet_wrap(ggplot2::vars(field), nrow = 1) +
    ggplot2::labs(x = "Share of the FARM wells (right: values filled from gw_base)", y = NULL) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.spacing.x = grid::unit(1.2, "lines")
    )
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "How many values could be filled from gw_base?",
      subtitle = paste0(
        "Per state: the FARM wells with the value ", colour_word("from FARM", "#2E86AB"), ", ",
        colour_word("filled from gw_base", "#F0A202"), " (same well, linked by state and ID), ",
        colour_word("missing in both", "#E4572E"), " or ",
        colour_word("missing, the well not found in gw_base", "grey55"), ". The analysis uses ",
        "the filled values; the levels come from gw_base, so wells not found there have none."
      ),
      caption = "Data: FARM master data, gw_base (tbl_meta_well_import).",
      theme = theme_maps(font)
    )
  png <- save_figure(
    figure, file.path(plot_config$dir, "farm_filling.png"),
    width = figure_width, height = 3 + 0.25 * (length(states) + 1)
  )
  c(tables, png)
}
