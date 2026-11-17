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
# prefix such as "HE_" removed); show per pair of sources and state how many
# wells are in both; for the wells of the meer files, put the values of all
# sources side by side and compare them with gw_base.

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
  # both signs: the last one is the decimal mark ("1.234,5" or "1,234.5")
  both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
  german <- both & regexpr(",[^,]*$", x) > regexpr("\\.[^.]*$", x)
  x[german] <- gsub(".", "", x[german], fixed = TRUE)
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
  # source coordinates in EPSG 25832. EPSG codes that PROJ does not know
  # (0, typos) count as missing; missing ones are guessed from the
  # coordinates (epsg_from says which)
  given <- suppressWarnings(as.integer(j$epsg))
  codes <- unique(stats::na.omit(given))
  known <- codes[vapply(codes, function(code) {
    crs <- tryCatch(suppressWarnings(sf::st_crs(code)), error = function(e) sf::NA_crs_)
    !is.na(crs)
  }, logical(1))]
  guessed <- guess_crs(j$x, j$y)
  epsg <- ifelse(given %in% known, given, guessed)
  j$epsg_from <- dplyr::case_when(
    given %in% known ~ "file",
    !is.na(given) & !is.na(guessed) ~ "guessed (unknown code in file)",
    !is.na(guessed) ~ "guessed",
    !is.na(j$x) & !is.na(j$y) ~ "unknown, not transformed",
    .default = NA_character_
  )
  j$x25832 <- NA_real_
  j$y25832 <- NA_real_
  for (code in unique(stats::na.omit(epsg))) {
    k <- which(epsg == code & !is.na(j$x) & !is.na(j$y))
    if (length(k) == 0) next
    x <- j$x[k]
    x[x >= 32e6 & x < 34e6] <- x[x >= 32e6 & x < 34e6] %% 1e6 # UTM zone prefix
    xy <- tryCatch(
      sf::st_coordinates(sf::st_transform(sf::st_as_sf(
        data.frame(x = x, y = j$y[k]), coords = c("x", "y"), crs = as.integer(code)
      ), 25832)),
      error = function(e) {
        warning("Source check: coordinates with EPSG ", code, " not transformed: ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(xy)) {
      j$epsg_from[k] <- "unknown, not transformed"
      next
    }
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

# Do the IDs match? Per pair of sources and state: the wells (keys) in both,
# only in the first and only in the second source. The states are those of
# the other source in pairs with gw_base (a state missing in gw_base shows),
# else those of the source with fewer states, so a source covering three
# states is not compared with all states of another one.
check_id_overlap <- function(joined) {
  keys <- dplyr::bind_rows(
    dplyr::transmute(joined$gwbase, source = "gw_base", key),
    dplyr::transmute(joined$rows, source, key)
  ) |>
    dplyr::filter(!is.na(key)) |>
    dplyr::distinct() |>
    dplyr::mutate(state = sub(" .*$", "", key), state = ifelse(state == "NA", "unknown", state))
  sources <- intersect(c("gw_base", "farm", "meer", "laender"), unique(keys$source))
  if (length(sources) < 2) return(tibble::tibble())
  out <- lapply(utils::combn(sources, 2, simplify = FALSE), function(p) {
    a <- keys[keys$source == p[1], ]
    b <- keys[keys$source == p[2], ]
    states <- if (p[1] == "gw_base" || length(unique(b$state)) <= length(unique(a$state))) {
      unique(b$state)
    } else {
      unique(a$state)
    }
    dplyr::bind_rows(lapply(sort(states), function(s) {
      ka <- a$key[a$state == s]
      kb <- b$key[b$state == s]
      tibble::tibble(
        first = p[1], second = p[2], state = s,
        both = length(intersect(ka, kb)),
        only_first = length(setdiff(ka, kb)),
        only_second = length(setdiff(kb, ka))
      )
    }))
  })
  dplyr::bind_rows(out) |>
    dplyr::mutate(
      share_of_first_found = both / (both + only_first),
      share_of_second_found = both / (both + only_second)
    )
}

# Source names in the columns of the meer table
check_column_names <- c(gw_base = "gw_base", farm = "farm", meer = "meer", laender = "states", dem = "dem")

# The wells of the meer files with the IDs, values and coordinate distances
# of all other sources side by side: one row per meer well. Columns:
# <field>_<source> (e.g. elev_gs_farm), <field>_<source>_minus_gw_base, and
# distance_<source>_m (distance of the source's coordinates to gw_base).
check_meer_table <- function(joined, values) {
  rows <- dplyr::filter(joined$rows, !is.na(key)) |> dplyr::distinct(key, source, .keep_all = TRUE)
  meer <- rows |>
    dplyr::filter(source == "meer") |>
    dplyr::transmute(state, key, meer_file = file, meer_id = id, x_25832 = x25832, y_25832 = y25832,
                     distance_meer_m = distance_m)
  if (nrow(meer) == 0) return(meer)
  gw <- dplyr::distinct(joined$gwbase, key, .keep_all = TRUE) |> dplyr::select(key, well_id)
  others <- lapply(c("farm", "laender"), function(s) {
    name <- check_column_names[[s]]
    d <- dplyr::filter(rows, source == s) |> dplyr::select(key, id, distance_m)
    names(d) <- c("key", paste0(name, "_id"), paste0("distance_", name, "_m"))
    d
  })
  wide <- values |>
    dplyr::filter(key %in% meer$key) |>
    dplyr::mutate(column = paste(field, check_column_names[source], sep = "_")) |>
    dplyr::select(key, column, value) |>
    tidyr::pivot_wider(names_from = column, values_from = value)
  out <- meer |>
    dplyr::left_join(gw, by = "key") |>
    dplyr::left_join(others[[1]], by = "key") |>
    dplyr::left_join(others[[2]], by = "key") |>
    dplyr::mutate(
      in_gw_base = !is.na(well_id), in_farm = !is.na(farm_id), in_states = !is.na(states_id)
    ) |>
    dplyr::left_join(wide, by = "key")
  for (f in check_fields) {
    ref <- paste0(f, "_gw_base")
    if (!ref %in% names(out)) next
    for (s in c("meer", "farm", "states", "dem")) {
      col <- paste0(f, "_", s)
      if (col %in% names(out)) out[[paste0(col, "_minus_gw_base")]] <- out[[col]] - out[[ref]]
    }
  }
  # per field: the values of all sources next to each other, then the
  # differences; the distances at the end
  order <- unlist(lapply(check_fields, function(f) {
    c(paste0(f, "_", c("gw_base", "meer", "farm", "states", "dem")),
      paste0(f, "_", c("meer", "farm", "states", "dem"), "_minus_gw_base"))
  }))
  first <- setdiff(names(out), c(order, grep("^distance_", names(out), value = TRUE)))
  out[, c(first, intersect(order, names(out)), grep("^distance_", names(out), value = TRUE))] |>
    dplyr::arrange(state, key)
}

check_class_colours <- c(
  "up to 1 cm" = "#1B998B", "up to 10 cm" = "#8CC7BD", "up to 1 m" = "#F0A202",
  "up to 5 m" = "#E4572E", "over 5 m" = "#83053C",
  "no value" = "grey82", "well not in the source" = "grey62", "well not in gw_base" = "grey35"
)
check_field_labels <- c(
  elev_gs = "Ground elevation", elev_mp = "Measuring point",
  screen_top = "Screen top", screen_bot = "Screen bottom"
)
check_comparison_labels <- c(meer = "meer", farm = "FARM", states = "states", dem = "DEM (meer)")
check_caption <- "Data: gw_base (tbl_meta_well_import), FARM, meer, original state data."

# The meer table long: per well, field and source the difference to gw_base
# and its class
check_meer_long <- function(meer) {
  out <- list()
  for (f in check_fields) {
    ref <- paste0(f, "_gw_base")
    if (!ref %in% names(meer)) next
    for (s in names(check_comparison_labels)) {
      col <- paste0(f, "_", s)
      if (!col %in% names(meer)) next
      in_source <- switch(s, farm = meer$in_farm, states = meer$in_states, rep(TRUE, nrow(meer)))
      out[[length(out) + 1]] <- tibble::tibble(
        state = meer$state, key = meer$key, meer_id = meer$meer_id, well_id = meer$well_id,
        x = meer$x_25832, y = meer$y_25832, field = f, source = s,
        gw_base = meer[[ref]], value = meer[[col]], difference = meer[[col]] - meer[[ref]],
        class = dplyr::case_when(
          !meer$in_gw_base ~ "well not in gw_base",
          !in_source ~ "well not in the source",
          is.na(meer[[col]] - meer[[ref]]) ~ "no value",
          abs(meer[[col]] - meer[[ref]]) <= 0.01 ~ "up to 1 cm",
          abs(meer[[col]] - meer[[ref]]) <= 0.1 ~ "up to 10 cm",
          abs(meer[[col]] - meer[[ref]]) <= 1 ~ "up to 1 m",
          abs(meer[[col]] - meer[[ref]]) <= 5 ~ "up to 5 m",
          .default = "over 5 m"
        )
      )
    }
  }
  dplyr::bind_rows(out) |>
    dplyr::mutate(
      class = factor(class, levels = names(check_class_colours)),
      comparison = factor(check_comparison_labels[source], levels = rev(check_comparison_labels)),
      field_label = factor(check_field_labels[field], levels = check_field_labels)
    )
}

# Tables as csv in output/source_check/: the column mapping, the ID
# matching, the ID pairs used, the ID overlap per pair of sources and state,
# all values side by side, the meer table and its large differences.
# Returns the paths.
write_check_tables <- function(sources, matching, pairs, joined, values, overlap, meer, settings) {
  dir <- file.path("output", "source_check")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  links <- joined$rows |>
    dplyr::summarise(
      rows = dplyr::n(), without_id = sum(is.na(key)), linked_to_gw_base = sum(matched),
      gwbase_duplicates = sum(gwbase_duplicates > 1, na.rm = TRUE),
      median_distance_m = stats::median(distance_m, na.rm = TRUE),
      over_100_m = sum(distance_m > 100, na.rm = TRUE),
      epsg_from_file = sum(epsg_from == "file", na.rm = TRUE),
      epsg_guessed = sum(grepl("^guessed", epsg_from)),
      epsg_unknown_codes = paste(sort(unique(epsg[grepl("unknown code", epsg_from)])), collapse = " "),
      not_transformed = sum(epsg_from == "unknown, not transformed", na.rm = TRUE),
      .by = c(source, file)
    ) |>
    dplyr::left_join(dplyr::select(pairs, -source, -source_column), by = "file")
  wide <- values |>
    dplyr::select(key, well_id, state, field, source, value) |>
    dplyr::mutate(source = factor(source, levels = names(check_source_labels))) |>
    dplyr::arrange(source) |>
    tidyr::pivot_wider(names_from = source, values_from = value) |>
    dplyr::arrange(field, key)
  large <- check_meer_long(meer) |>
    dplyr::filter(!is.na(difference), abs(difference) > settings$report_threshold_m) |>
    dplyr::select(state, meer_id, well_id, field, source, gw_base, value, difference) |>
    dplyr::arrange(dplyr::desc(abs(difference)))
  files <- list(
    column_mapping = sources$report, id_matching = matching, id_pairs = links,
    id_overlap = overlap, values = wide, meer_wells = meer, meer_large_differences = large
  )
  vapply(names(files), function(name) {
    path <- file.path(dir, paste0(name, ".csv"))
    utils::write.csv(files[[name]], path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    path
  }, character(1))
}

# Bar per pair of sources and state: the wells only in the left source, in
# both and only in the right source
plot_check_overlap <- function(overlap, plot_config, font) {
  colours <- c("only in the left source" = "#F0A202", "in both" = "#2E86AB", "only in the right source" = "#E4572E")
  pair_levels <- unique(paste0(check_source_labels[overlap$first], "  |  ", check_source_labels[overlap$second]))
  d <- overlap |>
    dplyr::mutate(pair = factor(paste0(check_source_labels[first], "  |  ", check_source_labels[second]), levels = pair_levels)) |>
    tidyr::pivot_longer(c(only_first, both, only_second), names_to = "part", values_to = "n") |>
    dplyr::mutate(
      part = factor(names(colours)[match(part, c("only_first", "both", "only_second"))], levels = names(colours)),
      share = n / sum(n), total = sum(n),
      .by = c(pair, state)
    ) |>
    dplyr::mutate(state = factor(state, levels = rev(sort(unique(state)))))
  p <- ggplot2::ggplot(d, ggplot2::aes(share, state, fill = part)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(share >= 0.1, label_n(n), "")),
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      family = font, size = 2.5, colour = "white"
    ) +
    ggplot2::scale_fill_manual(values = colours, name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), breaks = c(0, 0.5, 1), expand = ggplot2::expansion(0)) +
    ggplot2::facet_wrap(ggplot2::vars(pair), ncol = 2, scales = "free_y") +
    ggplot2::labs(x = "Share of the wells in either source (numbers: wells)", y = NULL) +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top", panel.spacing.x = grid::unit(1.5, "lines")
    )
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Do the IDs match?",
      subtitle = paste0(
        "Per pair of sources and state: wells ", colour_word("in both", "#2E86AB"),
        ", only in the ", colour_word("left", "#F0A202"), " or only in the ",
        colour_word("right", "#E4572E"), " source, linked by state and ID (state ",
        "prefix such as HE_, punctuation and leading zeros removed). A bar mostly ",
        "orange and red means the IDs do not match."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  rows <- d |> dplyr::distinct(pair, state) |> dplyr::count(pair)
  n_rows <- sum(stats::aggregate(rows$n, list(ceiling(seq_along(rows$n) / 2)), max)$x)
  save_figure(
    figure, file.path(plot_config$dir, "source_check", "id_overlap.png"),
    width = figure_width, height = 3 + 0.26 * n_rows + 0.45 * ceiling(nrow(rows) / 2)
  )
}

# The meer wells: per field, source and state the share of wells by size of
# the difference to gw_base
plot_check_meer <- function(meer, plot_config, font) {
  d <- check_meer_long(meer)
  if (nrow(d) == 0) return(character(0))
  counts <- dplyr::count(d, state, field_label, comparison, class) |>
    dplyr::mutate(share = n / sum(n), .by = c(state, field_label, comparison))
  totals <- dplyr::count(d, state, field_label, comparison)
  p <- ggplot2::ggplot(counts, ggplot2::aes(share, comparison, fill = class)) +
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.03, y = comparison, label = label_n(n)),
      data = totals, inherit.aes = FALSE, hjust = 0, family = font, size = 2.4, colour = "grey40"
    ) +
    ggplot2::scale_fill_manual(values = check_class_colours, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(), breaks = c(0, 0.5, 1),
      limits = c(0, 1.22), expand = ggplot2::expansion(0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(field_label), cols = ggplot2::vars(state), scales = "free_y", space = "free_y"
    ) +
    ggplot2::labs(x = "Share of the meer wells (numbers: wells)", y = "Compared with gw_base") +
    theme_charts(font, base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
      legend.position = "bottom", panel.spacing.x = grid::unit(1.2, "lines")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "How far do the meer wells differ from gw_base?",
      subtitle = paste0(
        "For every well in the meer files: the value in meer, FARM, the state data ",
        "and the DEM of meer minus the value in gw_base, by size of the difference. ",
        "Grey: no comparison possible (no value, or the well is not found in the ",
        "source or in gw_base). Values per well: target check_meer or ",
        "output/source_check/meer_wells.csv."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  n_bars <- nrow(dplyr::distinct(d, field_label, comparison))
  save_figure(
    figure, file.path(plot_config$dir, "source_check", "meer_differences.png"),
    width = figure_width, height = 3.4 + 0.32 * n_bars + 0.3 * length(unique(d$field_label))
  )
}

# Where: map of the meer wells, coloured by the difference of the ground
# elevation to gw_base, one map per source
plot_check_meer_map <- function(meer, germany, plot_config, font) {
  d <- check_meer_long(meer) |> dplyr::filter(field == "elev_gs", !is.na(x), !is.na(y))
  if (nrow(d) == 0) return(character(0))
  d$comparison <- factor(d$comparison, levels = check_comparison_labels)
  d <- dplyr::arrange(d, class != "up to 1 cm", as.integer(class))
  pad <- 20000
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = germany, fill = "grey95", colour = "grey70", linewidth = 0.2) +
    ggplot2::geom_point(ggplot2::aes(x, y, colour = class), data = d, size = 0.9, stroke = 0) +
    ggplot2::scale_colour_manual(values = check_class_colours, name = NULL, drop = FALSE) +
    ggplot2::coord_sf(
      datum = NA, crs = sf::st_crs(germany),
      xlim = range(d$x) + c(-pad, pad), ylim = range(d$y) + c(-pad, pad)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(comparison), nrow = 1) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3), nrow = 2)) +
    theme_maps(font, base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
  figure <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Where do the ground elevations of the meer wells differ?",
      subtitle = paste0(
        "Each point a well in the meer files, coloured by its ground elevation in the ",
        "source (map title) minus gw_base. Large differences on top."
      ),
      caption = check_caption,
      theme = theme_maps(font)
    )
  save_figure(figure, file.path(plot_config$dir, "source_check", "meer_map.png"), width = figure_width, height = 6.5)
}

# All outputs of the check; returns the paths.
write_source_check <- function(sources, matching, pairs, joined, values, overlap, meer, settings,
                               germany, plot_config, font = setup_plot_font(plot_config$font)) {
  setup_plot_font(plot_config$font)
  c(
    write_check_tables(sources, matching, pairs, joined, values, overlap, meer, settings),
    plot_check_overlap(overlap, plot_config, font),
    plot_check_meer(meer, plot_config, font),
    plot_check_meer_map(meer, germany, plot_config, font)
  )
}
