# Check of the well metadata against other sources (temporary) ----------------------
#
# Compares the metadata of gw_base (tbl_meta_well_import) with three other
# sources and these among each other: FARM (one csv), "meer" (one file per
# state, with a DEM elevation) and the original data of the states (one file
# per state). Kept apart from the rest of the pipeline (targets check_*,
# outputs in plots/source_check/ and output/source_check/), so it can be
# removed again.
#
# Steps: read every file robustly (separator, decimal comma, encoding,
# everything as text first); take the ID and the compared fields from the
# columns named in check_settings$mapping (column_mapping.csv shows what was
# found); link the wells of all sources by state and normalised ID (a state
# prefix such as "HE_" removed); check the links by the coordinates; compare
# every pair of sources per field.

# Compared fields
check_fields <- c("elev_gs", "elev_mp", "screen_top", "screen_bot")
# Further columns read where present (normalised names, see norm_name()):
# the state, the coordinates for the check of the links, the DEM of meer
check_extra_columns <- list(
  farm = c(state = "bundesland", x = "x", y = "y", epsg = "epsg"),
  meer = c(
    state = "land", x = "rechtswert_epsg25832", y = "hochwert_epsg25832", dem = "hoehe_dem_m"
  ),
  laender = c(
    state = "bundesland", x = "r_wert_etrs89", y = "h_wert_etrs89",
    epsg = "epsg_code_fuer_koordinatenreferenzsystem"
  )
)
check_numeric_fields <- c(check_fields, "x", "y", "epsg", "dem")
# sources in the order of the comparisons (the first of a pair is the reference)
check_source_labels <- c(
  gw_base = "gw_base", farm = "FARM", meer = "meer", laender = "states", dem = "DEM (meer)"
)

state_codes <- c(
  baden_wuerttemberg = "BW", bayern = "BY", berlin = "BE", brandenburg = "BB",
  bremen = "HB", hamburg = "HH", hessen = "HE", mecklenburg_vorpommern = "MV",
  niedersachsen = "NI", nordrhein_westfalen = "NW", rheinland_pfalz = "RP",
  saarland = "SL", sachsen = "SN", sachsen_anhalt = "ST", schleswig_holstein = "SH",
  thueringen = "TH"
)

# UTF-8 text read as Latin-1 ("messpunkth\u00c3\u00b6he"): the umlauts repaired
repair_umlauts <- function(x) {
  # general repair: the bytes read back as UTF-8, where that gives valid text
  broken <- !is.na(x) & grepl("\u00c3", x, fixed = TRUE)
  if (any(broken)) {
    fixed <- iconv(x[broken], from = "UTF-8", to = "latin1")
    Encoding(fixed) <- "UTF-8"
    ok <- !is.na(fixed) & validUTF8(fixed)
    x[broken][ok] <- fixed[ok]
  }
  # remaining cases (Windows-1252 and Latin-1 forms of capitals and sharp s)
  # (as two vectors: names with these characters fail in some locales)
  from <- c(
    "\u00c3\u0084", "\u00c3\u0096", "\u00c3\u009c", "\u00c3\u009f", "\u00c3\u00a4",
    "\u00c3\u00b6", "\u00c3\u00bc", "\u00c3\u201e", "\u00c3\u2013", "\u00c3\u0153",
    "\u00c3\u0178"
  )
  to <- c(
    "\u00c4", "\u00d6", "\u00dc", "\u00df", "\u00e4", "\u00f6", "\u00fc", "\u00c4",
    "\u00d6", "\u00dc", "\u00df"
  )
  for (k in seq_along(from)) x <- gsub(from[k], to[k], x, fixed = TRUE)
  x
}

norm_name <- function(x) {
  x <- tolower(repair_umlauts(x))
  x <- gsub("\u00e4", "ae", x, fixed = TRUE)
  x <- gsub("\u00f6", "oe", x, fixed = TRUE)
  x <- gsub("\u00fc", "ue", x, fixed = TRUE)
  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# Two-letter state codes from codes or names
norm_state <- function(x) {
  x <- trimws(as.character(x))
  code <- toupper(x)
  named <- state_codes[norm_name(x)]
  as.character(ifelse(grepl("^[A-Za-z]{2}$", x), code, unname(named)))
}

# IDs comparable across sources: upper case, a leading state code with a
# separator ("HE_", "HE-", "HE ") removed, only letters and digits, leading
# zeros removed: "HE_00001000", "he-1000" and "1000" all give "1000". The
# state is part of the key (state_id_key()), so equal numbers in different
# states stay apart. Numbers read as text keep all digits ("25470023", not
# "2.547e+07").
norm_id <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  x <- toupper(trimws(as.character(x)))
  x <- sub(paste0("^(", paste(state_codes, collapse = "|"), ")[_ -]+"), "", x)
  x <- gsub("[^A-Z0-9]", "", x)
  x <- sub("^0+(?=.)", "", x, perl = TRUE)
  ifelse(is.na(x) | x == "" | x == "NA", NA_character_, x)
}

# Numbers written with a decimal comma or point, spaces as thousands marks
parse_num <- function(x) {
  x <- gsub("[[:space:]]", "", as.character(x))
  comma_only <- grepl(",", x, fixed = TRUE) & !grepl(".", x, fixed = TRUE)
  x[comma_only] <- sub(",", ".", x[comma_only], fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# One table file as text: csv/txt with detected separator and encoding, or
# Excel. Column names as in the file (umlauts repaired).
read_check_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    d <- readxl::read_excel(path, col_types = "text")
    names(d) <- repair_umlauts(names(d))
    return(tibble::as_tibble(d))
  }
  # the whole file as UTF-8 (Windows-1252 if it is not valid UTF-8), read
  # from a temporary copy with the strings marked as UTF-8: independent of
  # the locale of the R session
  bytes <- readBin(path, "raw", file.size(path))
  if (length(bytes) >= 3 && all(bytes[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  text <- rawToChar(bytes)
  if (!validUTF8(text)) text <- iconv(text, from = "CP1252", to = "UTF-8", sub = "?")
  Encoding(text) <- "UTF-8"
  header <- sub("[\r\n].*$", "", text)
  counts <- c(";" = lengths(gregexpr(";", header)), "\t" = lengths(gregexpr("\t", header)),
              "," = lengths(gregexpr(",", header)))
  sep <- names(which.max(counts))
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, "wb")
  writeBin(charToRaw(enc2utf8(text)), con)
  close(con)
  d <- utils::read.table(
    tmp, sep = sep, header = TRUE, quote = "\"", comment.char = "",
    colClasses = "character", check.names = FALSE, na.strings = c("", "NA"),
    encoding = "UTF-8", strip.white = TRUE, fill = TRUE
  )
  names(d) <- repair_umlauts(names(d))
  tibble::as_tibble(d)
}

# The input files of the check (for a target with format = "file", so a
# changed file reruns the check): FARM, meer and the original state files.
check_input_files <- function(settings) {
  base <- settings$dir
  files <- c(
    file.path(base, settings$farm_file),
    list.files(file.path(base, settings$meer_dir), pattern = settings$meer_pattern, full.names = TRUE),
    list.files(file.path(base, settings$laender_dir), pattern = settings$laender_pattern, full.names = TRUE)
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) stop("No input files for the source check found in ", base, call. = FALSE)
  files
}

source_type <- function(path, settings) {
  if (grepl(settings$meer_dir, path, fixed = TRUE)) "meer"
  else if (grepl(settings$laender_dir, path, fixed = TRUE)) "laender"
  else "farm"
}

# State from a file name such as FARM_HE_hoehe_abweichung or RP_Stammdaten
state_from_file <- function(path) {
  m <- regmatches(basename(path), regexpr("(^|_)(BW|BY|BE|BB|HB|HH|HE|MV|NI|NW|RP|SL|SN|ST|SH|TH)(_|\\.)", basename(path)))
  if (length(m) == 0) NA_character_ else gsub("[^A-Z]", "", m)
}


# The column names of a source type from the mapping, with the entries for a
# single state ("laender_ST") over those of the type. Empty entries dropped.
check_mapping <- function(settings, type, state = NA) {
  map <- unlist(settings$mapping[[type]])
  extra <- if (!is.na(state)) unlist(settings$mapping[[paste0(type, "_", state)]])
  map[names(extra)] <- extra
  map[!is.na(map) & nzchar(map)]
}

# State codes from IDs with a state prefix ("HE_1000"), else NA
state_from_id <- function(x) {
  code <- toupper(sub("^([A-Za-z]{2})[_ -].*$", "\\1", trimws(as.character(x))))
  as.character(ifelse(code %in% state_codes, code, NA_character_))
}

# The key that links the wells of all sources: state and normalised ID
state_id_key <- function(state, id) {
  id <- norm_id(id)
  ifelse(is.na(id), NA_character_, paste(state, id))
}

# All files read; the ID, the fields of the mapping and the extra columns
# taken by their normalised names. Returns the data (one row per row of a
# file) and the mapping report (which column became which field).
read_check_sources <- function(files, settings) {
  parts <- lapply(files, function(path) {
    type <- source_type(path, settings)
    file_state <- state_from_file(path)
    d <- read_check_file(path)
    mapped <- check_mapping(settings, type, file_state)
    map <- c(mapped, check_extra_columns[[type]][setdiff(names(check_extra_columns[[type]]), names(mapped))])
    found <- match(norm_name(map), norm_name(names(d)))
    report <- tibble::tibble(
      source = type, file = basename(path), field = names(map), expected = unname(map),
      from = ifelse(names(map) %in% names(mapped), "mapping", "extra"),
      column = ifelse(is.na(found), NA_character_, names(d)[found]),
      found = !is.na(found)
    )
    out <- tibble::tibble(source = type, file = basename(path), row = seq_len(nrow(d)))
    for (i in seq_along(map)) {
      out[[names(map)[i]]] <- if (is.na(found[i])) NA_character_ else d[[found[i]]]
    }
    if (!"id" %in% names(out)) out$id <- NA_character_
    if (!"state" %in% names(out)) out$state <- NA_character_
    out$state <- dplyr::coalesce(
      norm_state(out$state), file_state, state_from_id(out$id)
    )
    if (type == "meer") out$epsg <- "25832"
    list(data = out, report = report)
  })
  report <- dplyr::bind_rows(lapply(parts, `[[`, "report"))
  missing <- dplyr::filter(report, from == "mapping", !found)
  if (nrow(missing) > 0) {
    warning(
      "Source check: columns of the mapping not found: ",
      paste0(missing$file, ": ", missing$expected, collapse = "; "),
      call. = FALSE
    )
  }
  data <- dplyr::bind_rows(lapply(parts, `[[`, "data"))
  if ("epsg" %in% names(data)) data$epsg <- gsub("[^0-9]", "", data$epsg)
  for (f in intersect(check_numeric_fields, names(data))) data[[f]] <- parse_num(data[[f]])
  list(data = data, report = report)
}

# gw_base: all columns of the metadata table that hold IDs, numbers or text,
# so every column can be tried for the IDs. Numbers as float, text as
# nvarchar(400), long and binary columns left out.
# `checksums` is not used: it reruns the query when the metadata change.
fetch_gwbase_check <- function(spec, columns, checksums = NULL) {
  cols <- dplyr::filter(columns, table_schema == spec$meta_schema, table_name == spec$meta_table)
  type <- sub("\\(.*$", "", cols$data_type)
  select <- dplyr::case_when(
    type %in% c("tinyint", "smallint", "int", "bigint", "bit") ~ sprintf("CAST(%s AS bigint) AS %s", sql_ident(cols$column_name), sql_ident(cols$column_name)),
    type %in% c("decimal", "numeric", "float", "real", "money", "smallmoney") ~ sprintf("CAST(%s AS float) AS %s", sql_ident(cols$column_name), sql_ident(cols$column_name)),
    type %in% c("char", "varchar", "nchar", "nvarchar") ~ sprintf("CAST(LEFT(%s, 400) AS nvarchar(400)) AS %s", sql_ident(cols$column_name), sql_ident(cols$column_name)),
    type %in% c("date", "datetime", "datetime2", "smalldatetime") ~ sprintf("CONVERT(varchar(30), %s, 126) AS %s", sql_ident(cols$column_name), sql_ident(cols$column_name)),
    .default = NA_character_
  )
  select <- select[!is.na(select)]
  db_query(sprintf("SELECT %s FROM %s.%s", paste(select, collapse = ", "),
                   sql_ident(spec$meta_schema), sql_ident(spec$meta_table)))
}

# gw_base in the form of the sources: ID, state (from the ID prefix), key,
# the mapped fields and the coordinates (EPSG 25832).
gwbase_table <- function(gwbase, settings) {
  map <- check_mapping(settings, "gw_base")
  if (!map[["id"]] %in% names(gwbase)) {
    stop("Source check: the gw_base ID column ", map[["id"]], " is not in the metadata.", call. = FALSE)
  }
  id <- as.character(gwbase[[map[["id"]]]])
  gw <- tibble::tibble(source = "gw_base", well_id = id, state = state_from_id(id))
  gw$key <- state_id_key(gw$state, id)
  for (f in intersect(names(map), check_fields)) {
    gw[[f]] <- if (map[[f]] %in% names(gwbase)) parse_num(gwbase[[map[[f]]]]) else NA_real_
  }
  coords <- settings$gwbase_coords
  gw$gw_x <- if (!is.null(coords$x)) parse_num(gwbase[[coords$x]]) else NA_real_
  gw$gw_y <- if (!is.null(coords$y)) parse_num(gwbase[[coords$y]]) else NA_real_
  gw
}

# Diagnostic: for the ID column of every file and every gw_base column with
# text or whole numbers, the share of the file's wells found in gw_base, by
# state and normalised ID (and by ID alone, to see state code problems). The
# state of gw_base is taken from the prefix of its mapped ID column.
check_id_matching <- function(sources, gwbase, settings) {
  gw <- gwbase_table(gwbase, settings)
  mapped_id <- check_mapping(settings, "gw_base")[["id"]]
  candidates <- names(gwbase)[vapply(gwbase, function(v) {
    if (is.character(v)) return(TRUE)
    is.numeric(v) && all(is.na(v) | v == round(v))
  }, logical(1))]
  keys <- lapply(candidates, function(col) {
    id <- norm_id(gwbase[[col]])
    list(with_state = unique(paste(gw$state, id)[!is.na(id)]), id = unique(id[!is.na(id)]))
  })
  names(keys) <- candidates

  d <- sources$data
  out <- list()
  for (file in unique(d$file)) {
    rows <- d[d$file == file, ]
    id <- norm_id(rows$id)
    ok <- !is.na(id)
    if (!any(ok)) next
    for (col in candidates) {
      out[[length(out) + 1]] <- tibble::tibble(
        source = rows$source[1], file = file, source_column = "id",
        gwbase_column = col, mapped = col == mapped_id,
        n_source = sum(ok),
        share_with_state = mean(paste(rows$state, id)[ok] %in% keys[[col]]$with_state),
        share_id_only = mean(id[ok] %in% keys[[col]]$id)
      )
    }
  }
  dplyr::bind_rows(out) |>
    dplyr::arrange(source, file, dplyr::desc(share_with_state), dplyr::desc(share_id_only))
}

# The gw_base column used per file: from settings$id_pairs (by file name or
# source type) if given; else the mapped ID column, unless it links less
# than half of the file's wells and another column at least 20 percentage
# points more (settings$id_auto; then chosen_by says so).
choose_id_pairs <- function(matching, settings) {
  overrides <- settings$id_pairs %||% list()
  out <- lapply(split(matching, matching$file), function(m) {
    m <- dplyr::arrange(m, dplyr::desc(share_with_state), dplyr::desc(share_id_only))
    key <- intersect(c(m$file[1], m$source[1]), names(overrides))
    if (length(key) > 0) {
      pick <- dplyr::filter(m, gwbase_column == overrides[[key[1]]])
      if (nrow(pick) == 0) {
        stop("Source check: gw_base column ", overrides[[key[1]]], " (id_pairs) not found.", call. = FALSE)
      }
      return(dplyr::mutate(pick, chosen_by = "setting id_pairs"))
    }
    by_mapping <- m[m$mapped, ]
    best <- m[1, ]
    if (isTRUE(settings$id_auto) && nrow(by_mapping) == 1 && by_mapping$share_with_state < 0.5 &&
          best$share_with_state >= by_mapping$share_with_state + 0.2) {
      note <- sprintf("best match (mapped column: %.0f%%)", 100 * by_mapping$share_with_state)
      return(dplyr::mutate(best, chosen_by = note))
    }
    dplyr::mutate(if (nrow(by_mapping) == 1) by_mapping else best, chosen_by = "mapping")
  })
  dplyr::arrange(dplyr::bind_rows(out), source, file)
}

# The rows of the sources with the key that links them to gw_base and to
# each other, and the distance of their coordinates to those of gw_base.
# Returns the rows and the gw_base table.
check_join <- function(sources, gwbase, pairs, settings) {
  gw <- gwbase_table(gwbase, settings)
  gw_unique <- dplyr::distinct(gw, key, .keep_all = TRUE)
  # by ID alone where a source row has no state, if the ID is unique in gw_base
  id_only <- norm_id(gw$well_id)
  id_only_unique <- id_only[!id_only %in% id_only[duplicated(id_only)]]
  out <- lapply(seq_len(nrow(pairs)), function(i) {
    p <- pairs[i, ]
    rows <- sources$data[sources$data$file == p$file, ]
    own <- state_id_key(rows$state, rows$id)
    if (p$gwbase_column == check_mapping(settings, "gw_base")[["id"]]) {
      rows$key <- own
      no_state <- is.na(rows$state) & norm_id(rows$id) %in% id_only_unique
      rows$key[no_state] <- gw$key[match(norm_id(rows$id[no_state]), id_only)]
    } else {
      # another gw_base column: translated to the key of the mapped ID
      other <- paste(gw$state, norm_id(gwbase[[p$gwbase_column]]))
      rows$key <- dplyr::coalesce(gw$key[match(own, other)], own)
    }
    rows$gwbase_duplicates <- as.integer(table(gw$key)[rows$key])
    rows
  })
  j <- dplyr::bind_rows(out)
  j <- dplyr::left_join(j, dplyr::select(gw_unique, key, well_id, gw_x, gw_y), by = "key")
  j$matched <- !is.na(j$well_id)
  # source coordinates in EPSG 25832 (guessed where the EPSG is missing)
  epsg <- ifelse(is.na(j$epsg), guess_crs(j$x, j$y), j$epsg)
  j$x25832 <- NA_real_
  j$y25832 <- NA_real_
  for (code in unique(stats::na.omit(epsg))) {
    k <- which(epsg == code & !is.na(j$x) & !is.na(j$y))
    if (length(k) == 0) next
    x <- j$x[k]
    x[x >= 32e6 & x < 34e6] <- x[x >= 32e6 & x < 34e6] %% 1e6 # UTM zone prefix
    xy <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(
      data.frame(x = x, y = j$y[k]), coords = c("x", "y"), crs = as.integer(code)
    ), 25832))
    j$x25832[k] <- xy[, 1]
    j$y25832[k] <- xy[, 2]
  }
  j$distance_m <- sqrt((j$x25832 - j$gw_x)^2 + (j$y25832 - j$gw_y)^2)
  list(rows = j, gwbase = gw)
}

# All values in one long table: per well (key), source and field, for the
# fields a source has (mapped column found; NA where the cell is empty).
# The DEM of meer counts as a source of its own for the ground elevation. A
# well listed twice in one source: the first value.
check_values <- function(joined, report) {
  gw <- joined$gwbase |>
    dplyr::filter(!is.na(key)) |>
    dplyr::distinct(key, .keep_all = TRUE)
  gw_fields <- intersect(check_fields, names(gw))
  gw_fields <- gw_fields[vapply(gw_fields, function(f) any(!is.na(gw[[f]])), logical(1))]
  values <- list(tidyr::pivot_longer(
    dplyr::select(gw, key, dplyr::all_of(gw_fields)),
    -key, names_to = "field", values_to = "value"
  ) |> dplyr::mutate(source = "gw_base"))
  rows <- dplyr::filter(joined$rows, !is.na(key))
  has <- dplyr::filter(report, found)
  for (f in c(check_fields, "dem")) {
    if (!f %in% names(rows)) next
    r <- rows[rows$file %in% has$file[has$field == f], ]
    if (nrow(r) == 0) next
    values[[length(values) + 1]] <- tibble::tibble(
      key = r$key, field = if (f == "dem") "elev_gs" else f, value = r[[f]],
      source = if (f == "dem") "dem" else r$source
    )
  }
  dplyr::bind_rows(values) |>
    dplyr::summarise(
      value = dplyr::first(value, na_rm = TRUE), n_rows = dplyr::n(),
      .by = c(key, source, field)
    ) |>
    dplyr::left_join(dplyr::select(gw, key, well_id), by = "key") |>
    dplyr::mutate(state = sub(" .*$", "", key), state = ifelse(state == "NA", NA, state))
}

# Every pair of sources per field, for the wells in both: the values and the
# difference (second minus first; the first is the earlier one in
# check_source_labels, so gw_base is always the reference).
check_differences <- function(values) {
  sources <- intersect(names(check_source_labels), unique(values$source))
  pairs <- utils::combn(sources, 2, simplify = FALSE)
  out <- lapply(pairs, function(p) {
    a <- dplyr::filter(values, source == p[1])
    b <- dplyr::filter(values, source == p[2])
    dplyr::inner_join(
      dplyr::select(a, key, well_id, state, field, value_first = value),
      dplyr::select(b, key, field, value_second = value),
      by = c("key", "field")
    ) |>
      dplyr::mutate(first = p[1], second = p[2], .after = field)
  })
  dplyr::bind_rows(out) |>
    dplyr::filter(!is.na(value_first) | !is.na(value_second)) |>
    dplyr::mutate(
      pair = paste(check_source_labels[first], "vs.", check_source_labels[second]),
      difference = value_second - value_first,
      class = dplyr::case_when(
        is.na(value_first) ~ "only in the second",
        is.na(value_second) ~ "only in the first",
        abs(difference) <= 0.01 ~ "same (\u2264 1 cm)",
        abs(difference) <= 0.1 ~ "\u2264 10 cm",
        abs(difference) <= 1 ~ "\u2264 1 m",
        abs(difference) <= 5 ~ "\u2264 5 m",
        .default = "> 5 m"
      )
    )
}

# Outputs -------------------------------------------------------------------------------

check_class_colours <- c(
  "same (\u2264 1 cm)" = "#1B998B", "\u2264 10 cm" = "#8CC7BD", "\u2264 1 m" = "#F0A202",
  "\u2264 5 m" = "#E4572E", "> 5 m" = "#83053C",
  "only in the first" = "grey80", "only in the second" = "grey55"
)
check_field_labels <- c(
  elev_gs = "Ground elevation", elev_mp = "Measuring point",
  screen_top = "Screen top", screen_bot = "Screen bottom"
)
check_caption <- "Data: gw_base (tbl_meta_well_import), FARM, meer, original state data."

# pair labels in the order of the sources
check_pair_levels <- function(pairs) {
  s <- names(check_source_labels)
  all <- utils::combn(s, 2, simplify = FALSE)
  labels <- vapply(all, function(p) paste(check_source_labels[p[1]], "vs.", check_source_labels[p[2]]), "")
  labels[labels %in% pairs]
}

# Tables as csv in output/source_check/: the column mapping, the ID matching,
# the ID pairs used with link and distance figures, all values side by side,
# the pairwise differences and the large ones. Returns the paths.
write_check_tables <- function(sources, matching, pairs, joined, values, differences, settings) {
  dir <- file.path("output", "source_check")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  links <- joined$rows |>
    dplyr::summarise(
      rows = dplyr::n(), without_id = sum(is.na(key)), linked_to_gw_base = sum(matched),
      gwbase_duplicates = sum(gwbase_duplicates > 1, na.rm = TRUE),
      median_distance_m = stats::median(distance_m, na.rm = TRUE),
      over_100_m = sum(distance_m > 100, na.rm = TRUE),
      .by = c(source, file)
    ) |>
    dplyr::left_join(dplyr::select(pairs, -source, -source_column), by = "file")
  wide <- values |>
    dplyr::select(key, well_id, state, field, source, value) |>
    dplyr::mutate(source = factor(source, levels = names(check_source_labels))) |>
    dplyr::arrange(source) |>
    tidyr::pivot_wider(names_from = source, values_from = value) |>
    dplyr::arrange(field, key)
  large <- differences |>
    dplyr::filter(!is.na(difference), abs(difference) > settings$report_threshold_m) |>
    dplyr::arrange(dplyr::desc(abs(difference)))
  files <- list(
    column_mapping = sources$report, id_matching = matching, id_pairs = links,
    values = wide, differences = differences, large_differences = large
  )
  vapply(names(files), function(name) {
    path <- file.path(dir, paste0(name, ".csv"))
    utils::write.csv(files[[name]], path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    path
  }, character(1))
}

# reorder within facets: labels made unique per facet, the suffix removed in
# the axis labels
tidytext_reorder <- function(x, by, within) {
  stats::reorder(paste(x, within, sep = "___"), by)
}

plot_check_matching <- function(matching, pairs, joined, plot_config, font) {
  used <- paste(pairs$file, pairs$gwbase_column)
  top <- matching |>
    dplyr::mutate(used = paste(file, gwbase_column) %in% used) |>
    dplyr::filter(used | mapped | rank(-(share_with_state + share_id_only / 1000), ties.method = "first") <= 3, .by = file) |>
    dplyr::mutate(
      label = paste0(gwbase_column, ifelse(mapped, " (mapping)", "")),
      source = paste0(check_source_labels[source], ": ", file)
    ) |>
    dplyr::mutate(label = tidytext_reorder(label, share_with_state, source))
  bars <- ggplot2::ggplot(top, ggplot2::aes(y = label)) +
    ggplot2::geom_col(ggplot2::aes(x = share_id_only), fill = NA, colour = "grey55", width = 0.7, linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(x = share_with_state, fill = used), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(x = share_with_state, label = scales::label_percent(accuracy = 1)(share_with_state)),
      hjust = -0.15, family = font, size = 2.6, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#2E86AB", `FALSE` = "grey70"), guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1.12), expand = ggplot2::expansion(0)) +
    ggplot2::facet_wrap(ggplot2::vars(source), ncol = 2, scales = "free_y") +
    ggplot2::scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    ggplot2::labs(
      title = "gw_base column compared with the ID of the file",
      x = "Wells of the file found in gw_base (bar: same state and ID; outline: ID only)", y = NULL
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0)
    )

  dist <- joined$rows |>
    dplyr::filter(matched, !is.na(distance_m)) |>
    dplyr::mutate(source = paste0(check_source_labels[source], ": ", file))
  if (nrow(dist) == 0) dist <- tibble::tibble(distance_m = NA_real_, source = "no coordinates")
  distances <- ggplot2::ggplot(dist, ggplot2::aes(pmax(distance_m, 0.1), source)) +
    ggplot2::geom_vline(xintercept = 100, colour = "#E4572E", linewidth = 0.4, linetype = "22") +
    ggplot2::geom_jitter(height = 0.25, width = 0, size = 0.6, alpha = 0.4, colour = "grey30", na.rm = TRUE) +
    ggplot2::scale_x_log10(labels = function(x) paste(label_n(x), "m"), breaks = c(0.1, 1, 10, 100, 1000, 10000, 1e5)) +
    ggplot2::labs(
      title = "Distance between the coordinates of linked wells",
      x = "Distance to gw_base (log scale; below 0.1 m shown at 0.1 m)", y = NULL
    ) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0))

  n_files <- length(unique(top$source))
  figure <- patchwork::wrap_plots(bars, distances, ncol = 1, heights = c(ceiling(n_files / 2) * 0.9, 0.25 * n_files + 0.6)) +
    patchwork::plot_annotation(
      title = "Do the IDs link the right wells?",
      subtitle = paste0(
        "Top: share of each file's wells found in gw_base by state and ID (a state ",
        "prefix such as HE_, spaces and leading zeros removed), for the gw_base ",
        "column of the mapping and the best other columns; ", colour_word("blue", "#2E86AB"),
        ": the column used. Bottom: distance between the coordinates of the linked ",
        "wells; beyond 100 m points to wrong links or wrong coordinates."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  save_figure(
    figure, file.path(plot_config$dir, "source_check", "id_matching.png"),
    width = figure_width, height = 3 + ceiling(n_files / 2) * 1.6 + 0.3 * n_files
  )
}

plot_check_agreement <- function(differences, plot_config, font) {
  d <- differences |>
    dplyr::count(pair, field, state, class) |>
    dplyr::mutate(share = n / sum(n), .by = c(pair, field, state)) |>
    dplyr::mutate(
      class = factor(class, levels = names(check_class_colours)),
      pair = factor(pair, levels = check_pair_levels(unique(pair))),
      field = factor(check_field_labels[field], levels = check_field_labels),
      state = factor(state, levels = rev(sort(unique(state))))
    )
  if (nrow(d) == 0) {
    d <- tibble::tibble(share = 0, state = "none", class = factor(names(check_class_colours)[1]),
                        pair = "no wells in two sources", field = "")
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(share, state, fill = class)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = check_class_colours, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), breaks = c(0, 0.5, 1)) +
    ggplot2::facet_grid(rows = ggplot2::vars(pair), cols = ggplot2::vars(field), scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Share of the wells in both sources", y = NULL) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9)),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
      legend.position = "bottom", panel.spacing = grid::unit(0.6, "lines")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Where do the sources agree?",
      subtitle = paste0(
        "Per pair of sources, field and state: share of the wells in both whose ",
        "values are equal (within 1 cm), differ by up to 10 cm, 1 m, 5 m or more, ",
        "or are given in only one of them. DEM (meer) is compared as a ground elevation."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  height <- 3 + 0.22 * nrow(dplyr::distinct(d, pair, state)) + 0.15 * length(unique(d$pair))
  save_figure(figure, file.path(plot_config$dir, "source_check", "agreement.png"), width = figure_width, height = height)
}

# symmetric log x scale; the argument is "transform" from ggplot2 3.5, "trans" before
pseudo_log_x <- function(...) {
  tr <- scales::pseudo_log_trans(sigma = 0.1, base = 10)
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    ggplot2::scale_x_continuous(transform = tr, ...)
  } else {
    ggplot2::scale_x_continuous(trans = tr, ...)
  }
}

plot_check_differences <- function(differences, settings, plot_config, font) {
  d <- differences |>
    dplyr::filter(field %in% c("elev_gs", "elev_mp"), !is.na(difference)) |>
    dplyr::mutate(
      pair = factor(pair, levels = check_pair_levels(unique(pair))),
      panel = paste0(pair, ": ", check_field_labels[field]),
      panel = factor(panel, levels = unique(panel[order(field, pair)])),
      large = abs(difference) > settings$report_threshold_m
    )
  if (nrow(d) == 0) d <- tibble::tibble(panel = "no values", state = NA, difference = 0, large = FALSE)
  n_panels <- length(unique(d$panel))
  p <- ggplot2::ggplot(d, ggplot2::aes(difference, state)) +
    ggplot2::annotate("rect", xmin = -settings$report_threshold_m, xmax = settings$report_threshold_m,
                      ymin = -Inf, ymax = Inf, fill = "#1B998B", alpha = 0.08) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_jitter(ggplot2::aes(colour = large), height = 0.25, width = 0, size = 0.7, alpha = 0.5) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#E4572E", `FALSE` = "grey45"), guide = "none") +
    pseudo_log_x(
      breaks = c(-100, -10, -1, 0, 1, 10, 100),
      labels = function(x) paste(x, "m")
    ) +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 3, scales = "free_y") +
    ggplot2::labs(x = "Second minus first source (symmetric log scale)", y = NULL) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(0.85)))
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "How large are the differences in elevation?",
      subtitle = paste0(
        "Each point a well in both sources of a pair: the elevation in the second ",
        "minus the first, by state. ",
        colour_word(paste0("Red: more than ", settings$report_threshold_m, " m"), "#E4572E"),
        " (listed in output/source_check/large_differences.csv); shaded: within that."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  save_figure(
    figure, file.path(plot_config$dir, "source_check", "differences.png"),
    width = figure_width, height = 3 + 1.9 * ceiling(n_panels / 3)
  )
}

# All outputs of the check; returns the paths.
write_source_check <- function(sources, matching, pairs, joined, values, differences, settings,
                               plot_config, font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  c(
    write_check_tables(sources, matching, pairs, joined, values, differences, settings),
    plot_check_matching(matching, pairs, joined, plot_config, font),
    plot_check_agreement(differences, plot_config, font),
    plot_check_differences(differences, settings, plot_config, font)
  )
}
