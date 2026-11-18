# FARM master data, filled from gw_base ----------------------------------------------
#
# The wells analysed are those of the FARM master data (farm_config$file).
# Where FARM has no ground elevation, screen top or bottom or coordinates,
# the value of the same well in gw_base (tbl_meta_well_import) is used;
# where the ground elevation is still missing, it is estimated from the
# measuring point height minus the median stick-up of the state. The wells
# are linked to the other sources by state and ID (state_id_key()), with the
# ID columns of the mapping table (config/id_mapping.csv, read_id_mapping()).
# The groundwater levels: see R/levels.R.

# The ID mapping table: per source (gw_base, state_levels) and state (or "*"
# for all states without a row of their own) the FARM column with the ID
# (farm_column), the column of the other source (other_column), and whether
# the wells can be linked at all (link: "id" or "none"), with a note.
read_id_mapping <- function(path) {
  m <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"), strip.white = TRUE)
  need <- c("source", "state", "farm_column", "other_column", "link")
  missing <- setdiff(need, names(m))
  if (length(missing) > 0) {
    stop("ID mapping ", path, ": columns missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  m <- tibble::as_tibble(m)
  m$link <- tolower(m$link)
  if (!all(m$link %in% c("id", "none"))) {
    stop("ID mapping ", path, ": link must be \"id\" or \"none\".", call. = FALSE)
  }
  for (s in unique(m$source)) {
    if (sum(m$source == s & m$state == "*") != 1) {
      stop("ID mapping ", path, ": source ", s, " needs exactly one row with state \"*\".", call. = FALSE)
    }
  }
  if (is.null(m$note)) m$note <- NA_character_
  m
}

# The mapping row of `source` for each state in `states` (the state's own
# row, else the row "*").
id_mapping_for <- function(mapping, source, states) {
  m <- mapping[mapping$source == source, ]
  if (nrow(m) == 0) stop("ID mapping: no rows for source ", source, call. = FALSE)
  row <- match(states, m$state)
  row[is.na(row)] <- match("*", m$state)
  m[row, ]
}

# column of the FARM table holding the IDs of a FARM column
farm_id_field <- function(column) paste0("id_", norm_name(column))

# The FARM table: the columns of farm_config$columns and every FARM ID column
# named in the mapping (names as in the file; case, umlauts, spaces and signs
# do not matter), numbers parsed. State from the state column (codes or
# names) or the ID prefix.
read_farm_meta <- function(path, config, mapping) {
  d <- read_table_file(path)
  id_columns <- unique(stats::na.omit(c(config$columns$id, mapping$farm_column)))
  map <- c(unlist(config$columns), stats::setNames(id_columns, farm_id_field(id_columns)))
  map <- map[!duplicated(names(map))]
  found <- match(norm_name(map), norm_name(names(d)))
  if (is.na(found[names(map) == "id"])) {
    stop("FARM: ID column ", map[["id"]], " not found in ", path, ". Columns: ",
         paste(names(d), collapse = ", "), call. = FALSE)
  }
  if (any(is.na(found))) {
    warning("FARM: columns not found in ", basename(path), ": ",
            paste(unique(map[is.na(found)]), collapse = ", "), call. = FALSE)
  }
  column <- function(field) {
    i <- found[names(map) == field]
    if (length(i) == 0 || is.na(i)) return(rep(NA_character_, nrow(d)))
    x <- trimws(as.character(d[[i]]))
    ifelse(nzchar(x), x, NA_character_)
  }
  state_raw <- column("state")
  unknown <- unique(state_raw[!is.na(state_raw) & is.na(norm_state(state_raw))])
  if (length(unknown) > 0) {
    warning("FARM: state values not recognised: ", paste(utils::head(unknown, 10), collapse = ", "),
            call. = FALSE)
  }
  id <- column("id")
  out <- tibble::tibble(
    farm_row = seq_len(nrow(d)),
    farm_id = id,
    state = dplyr::coalesce(norm_state(state_raw), state_from_id(id)),
    elev_gs = parse_num(column("elev_gs")),
    elev_mp = parse_num(column("elev_mp")),
    screen_top = parse_num(column("screen_top")),
    screen_bot = parse_num(column("screen_bot")),
    x = parse_num(column("x")),
    y = parse_num(column("y")),
    epsg = gsub("[^0-9]", "", column("epsg"))
  )
  for (f in farm_id_field(id_columns)) out[[f]] <- column(f)
  out
}

# The key of each FARM record for `source` ("state ID" with the FARM column
# of the mapping; NA where the mapping says "none" or the ID is empty), and
# the mapping rows used.
farm_link_keys <- function(farm, mapping, source) {
  m <- id_mapping_for(mapping, source, farm$state)
  ids <- rep(NA_character_, nrow(farm))
  for (col in unique(stats::na.omit(m$farm_column))) {
    rows <- which(m$farm_column %in% col & m$link == "id")
    f <- farm_id_field(col)
    if (length(rows) > 0 && f %in% names(farm)) ids[rows] <- farm[[f]][rows]
  }
  list(key = state_id_key(farm$state, ids), ids = ids, mapping = m)
}

# The analysis table: one row per FARM record. Values from FARM where given,
# else from the linked gw_base well; a ground elevation still missing is
# estimated from the measuring point (estimate_elev_from_mp()). Columns:
#   well_id   gw_base well ID; for FARM wells not linked to gw_base
#             "<state>_<FARM ID>"
#   farm_id, state, in_gw_base, gwbase_link (how the link was made, or why
#             not), state_levels_key (key for the state level series)
#   elev_gs, elev_mp (FARM only), screen_top, screen_bot, x, y (EPSG 25832)
#   <field>_from, coords_from: "FARM", "gw_base", "estimated from measuring
#             point" (elev_gs only), "missing in both", "missing, not in
#             gw_base"
#   coords_epsg_from: how the FARM coordinates were transformed
# `gwbase`: fetch_well_meta() (coordinates in EPSG `crs`; NULL: guessed),
# with the gw_base ID columns of the mapping.
fill_farm_meta <- function(farm, gwbase, mapping, crs = NULL, stickup_min_pairs = 10) {
  gw_xy <- coords_to_25832(gwbase$x, gwbase$y, crs %||% NA)
  gwbase$x <- gw_xy$x
  gwbase$y <- gw_xy$y
  gwbase$gw_state <- state_from_id(gwbase$well_id)

  links <- farm_link_keys(farm, mapping, "gw_base")
  farm$key <- links$key
  # without a state: by the ID alone, where it is unique in the gw_base column
  m <- links$mapping
  gw_row <- rep(NA_integer_, nrow(farm))
  for (col in unique(stats::na.omit(m$other_column[m$link == "id"]))) {
    if (!col %in% names(gwbase)) {
      stop("ID mapping: gw_base column ", col, " is not in the metadata query.", call. = FALSE)
    }
    gw_key <- state_id_key(gwbase$gw_state, gwbase[[col]])
    rows <- which(m$other_column %in% col & m$link == "id")
    gw_row[rows] <- match(farm$key[rows], gw_key)
    id_only <- norm_id(gwbase[[col]])
    unique_ids <- id_only[!is.na(id_only) & !id_only %in% id_only[duplicated(id_only)]]
    no_state <- rows[is.na(farm$state[rows])]
    farm_ids <- norm_id(links$ids[no_state])
    hit <- farm_ids %in% unique_ids
    gw_row[no_state[hit]] <- match(farm_ids[hit], id_only)
  }
  gw <- gwbase[gw_row, ]
  linked <- !is.na(gw_row)
  source_of <- function(farm_value, gw_value) {
    dplyr::case_when(
      !is.na(farm_value) ~ "FARM",
      !is.na(gw_value) ~ "gw_base",
      linked ~ "missing in both",
      .default = "missing, not in gw_base"
    )
  }
  farm_xy <- coords_to_25832(farm$x, farm$y, farm$epsg)
  out <- tibble::tibble(
    well_id = dplyr::coalesce(gw$well_id, paste0(dplyr::coalesce(farm$state, "unknown"), "_", farm$farm_id)),
    farm_id = farm$farm_id,
    state = farm$state,
    in_gw_base = linked,
    gwbase_link = dplyr::case_when(
      linked ~ paste0("ID (", m$farm_column, " = ", m$other_column, ")"),
      m$link == "none" ~ paste0("none (mapping: ", dplyr::coalesce(m$note, "not linkable"), ")"),
      .default = "ID not found in gw_base"
    ),
    state_levels_key = farm_link_keys(farm, mapping, "state_levels")$key,
    elev_gs = dplyr::coalesce(farm$elev_gs, gw$elev_gs),
    elev_mp = farm$elev_mp,
    screen_top = dplyr::coalesce(farm$screen_top, gw$screen_top),
    screen_bot = dplyr::coalesce(farm$screen_bot, gw$screen_bot),
    x = dplyr::coalesce(farm_xy$x, gw$x),
    y = dplyr::coalesce(farm_xy$y, gw$y),
    elev_gs_from = source_of(farm$elev_gs, gw$elev_gs),
    screen_top_from = source_of(farm$screen_top, gw$screen_top),
    screen_bot_from = source_of(farm$screen_bot, gw$screen_bot),
    coords_from = source_of(farm_xy$x, gw$x),
    coords_epsg_from = farm_xy$from
  )
  out <- dplyr::filter(out, !is.na(farm_id))
  estimate_elev_from_mp(out, stickup_min_pairs)
}

# Stick-up (measuring point height minus ground elevation) per state, from
# the FARM wells with both values in FARM: median and number of wells; the
# row "Germany" for all.
farm_stickup <- function(meta) {
  pairs <- dplyr::filter(meta, elev_gs_from == "FARM", !is.na(elev_mp)) |>
    dplyr::mutate(stickup = elev_mp - elev_gs)
  dplyr::bind_rows(
    dplyr::summarise(pairs, stickup_m = stats::median(stickup), n_wells = dplyr::n(), .by = state),
    tibble::tibble(state = "Germany", stickup_m = stats::median(pairs$stickup), n_wells = nrow(pairs))
  )
}

# Where the ground elevation is still missing and FARM has the measuring
# point height: elevation = measuring point - median stick-up of the state
# (of Germany where the state has fewer than `min_pairs` wells with both
# values). elev_gs_from "estimated from measuring point", elev_gs_stickup the
# stick-up used.
estimate_elev_from_mp <- function(meta, min_pairs = 10) {
  stick <- farm_stickup(meta)
  germany <- stick$stickup_m[stick$state == "Germany"]
  by_state <- dplyr::filter(stick, state != "Germany", n_wells >= min_pairs)
  used <- dplyr::coalesce(by_state$stickup_m[match(meta$state, by_state$state)], germany)
  est <- is.na(meta$elev_gs) & !is.na(meta$elev_mp) & !is.na(used)
  meta$elev_gs_stickup <- ifelse(est, used, NA_real_)
  meta$elev_gs[est] <- meta$elev_mp[est] - used[est]
  meta$elev_gs_from[est] <- "estimated from measuring point"
  meta
}

# Outputs -------------------------------------------------------------------------------

# Where a value comes from (fields) and which series gives the levels
# (levels): colours of the filling figure. The neighbour class is named
# after the radius (assign_level_sources()).
farm_fill_colours <- function(sources = character(0)) {
  neighbour <- unique(grep("within", sources, value = TRUE))
  c(
    "FARM" = "#2E86AB", "gw_base" = "#F0A202", "estimated from measuring point" = "#6A4C93",
    "missing in both" = "#E4572E", "missing, not in gw_base" = "#A6A6A6",
    "state data" = "#1B998B", stats::setNames(rep("#B5446E", length(neighbour)), neighbour),
    "none" = "#D4D4D4"
  )
}
farm_field_labels <- c(
  elev_gs = "Ground elevation", screen_top = "Screen top", screen_bot = "Screen bottom",
  coords = "Coordinates", levels = "Groundwater levels"
)

# Per field and state (and Germany): the FARM wells by where the value comes
# from; for the levels, by the source of the level series.
summarise_farm_filling <- function(meta) {
  meta <- dplyr::mutate(meta, state = dplyr::coalesce(state, "unknown"), levels_from = level_source)
  long <- tidyr::pivot_longer(
    meta,
    c(elev_gs_from, screen_top_from, screen_bot_from, coords_from, levels_from),
    names_to = "field", values_to = "from"
  ) |>
    dplyr::mutate(field = sub("_from$", "", field))
  dplyr::bind_rows(long, dplyr::mutate(long, state = "Germany")) |>
    dplyr::count(state, field, from) |>
    dplyr::mutate(share = n / sum(n), .by = c(state, field))
}

# Figure of the filling and the table behind it (output/farm_filling.csv),
# the stick-up per state (output/farm_stickup.csv) and the filled analysis
# table (output/well_meta_filled.csv). Returns the paths.
plot_farm_filling <- function(meta, plot_config, font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  counts <- summarise_farm_filling(meta)
  dir.create("output", showWarnings = FALSE)
  tables <- c(
    write_csv_file(counts, file.path("output", "farm_filling.csv")),
    write_csv_file(farm_stickup(meta), file.path("output", "farm_stickup.csv")),
    write_csv_file(meta, file.path("output", "well_meta_filled.csv"))
  )
  colours <- farm_fill_colours(counts$from)
  states <- setdiff(sort(unique(counts$state)), "Germany")
  d <- counts |>
    dplyr::mutate(
      from = factor(from, levels = names(colours)),
      field = factor(farm_field_labels[field], levels = farm_field_labels),
      state = factor(state, levels = rev(c("Germany", states)))
    )
  # right of the bars: values added (filled or estimated), for the levels the
  # wells with a series
  added <- d |>
    dplyr::filter(
      (field != farm_field_labels[["levels"]] & from %in% c("gw_base", "estimated from measuring point")) |
        (field == farm_field_labels[["levels"]] & from != "none")
    ) |>
    dplyr::summarise(n_added = sum(n), .by = c(state, field))
  totals <- d |>
    dplyr::distinct(state, field) |>
    dplyr::left_join(added, by = c("state", "field")) |>
    dplyr::mutate(
      n_added = dplyr::coalesce(n_added, 0L),
      label = ifelse(field == farm_field_labels[["levels"]], label_n(n_added), paste0("+", label_n(n_added)))
    )
  p <- ggplot2::ggplot(d, ggplot2::aes(share, state, fill = from)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE), show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.03, y = state, label = label),
      data = totals, inherit.aes = FALSE, hjust = 0, family = font, size = 2.5, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(values = colours, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(), breaks = c(0, 0.5, 1),
      limits = c(0, 1.38), expand = ggplot2::expansion(0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::facet_wrap(ggplot2::vars(field), nrow = 1) +
    ggplot2::labs(
      x = "Share of the FARM wells (right: values added; for the levels, wells with a series)",
      y = NULL
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.spacing.x = grid::unit(1, "lines")
    )
  neighbour <- grep("within", names(colours), value = TRUE)[1]
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Where the values and the levels of the FARM wells come from",
      subtitle = paste0(
        "Per state, the FARM wells with the value ", colour_word("from FARM", "#2E86AB"), ", ",
        colour_word("filled from gw_base", "#F0A202"), " (same well, linked by state and ID), ",
        colour_word("estimated from the measuring point", "#6A4C93"), " (ground elevation only), ",
        colour_word("missing in both", "#E4572E"), " or ",
        colour_word("missing, the well not found in gw_base", "#8C8C8C"),
        ". Groundwater levels: the series of the ", colour_word("state data", "#1B998B"), ", of the ",
        colour_word("linked gw_base well", "#F0A202"), ", of a ",
        colour_word(if (is.na(neighbour)) "gw_base well nearby" else neighbour, "#B5446E"),
        ", or ", colour_word("none", "#A6A6A6"), ", in this order of priority."
      ),
      caption = "Data: FARM master data, gw_base (tbl_meta_well_import, tbl_gwl_raw), state deliveries and MENTOR.",
      theme = theme_maps(font)
    )
  png <- save_figure(
    figure, file.path(plot_config$dir, "farm_filling.png"),
    width = figure_width, height = 3.3 + 0.25 * (length(states) + 1)
  )
  c(tables, png)
}
