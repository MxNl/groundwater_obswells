# Reading tables and linking wells across sources -----------------------------------
#
# Used by the analysis (FARM master data filled from gw_base, R/farm.R) and
# by the source check (R/source_check.R): reading csv/txt/Excel files
# robustly, column names and state names normalised, IDs made comparable
# (norm_id()) and the key "state ID" that links the wells of two sources
# (state_id_key()), coordinates to EPSG 25832.

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

# Two-letter state codes from codes or names; a name is also found within
# a longer one ("Freie und Hansestadt Hamburg", "Land Berlin"), longer names
# first (Sachsen-Anhalt before Sachsen). NA where nothing is recognised.
norm_state <- function(x) {
  x <- trimws(as.character(x))
  out <- toupper(x)
  out[!out %in% state_codes] <- NA_character_
  n <- norm_name(x)
  aliases <- c(state_codes, nrw = "NW")
  for (name in names(aliases)[order(-nchar(names(aliases)))]) {
    hit <- is.na(out) & !is.na(n) & grepl(paste0("(^|_)", name, "($|_)"), n)
    out[hit] <- aliases[[name]]
  }
  as.character(out)
}

# IDs comparable across sources: upper case, a leading state code with a
# separator ("HE_", "HE-", "HE ") removed, then everything except letters
# and digits (spaces, "-", "/", "."), then leading zeros: "HE_00001000",
# "00001000" and "1000" all give "1000", "0100/569-4" gives "1005694". The
# state is part of the key (state_id_key()), so equal IDs in different
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
read_table_file <- function(path) {
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

# State codes from IDs with a state prefix ("HE_1000"), else NA
state_from_id <- function(x) {
  code <- toupper(sub("^([A-Za-z]{2})[_ -].*$", "\\1", trimws(as.character(x))))
  as.character(ifelse(code %in% state_codes, code, NA_character_))
}

# The key that links the wells of all sources: state and normalised ID
state_id_key <- function(state, id) {
  id <- norm_id(id)
  as.character(ifelse(is.na(id), NA_character_, paste(state, id)))
}

# Coordinates in EPSG 25832, from x/y in the EPSG code `epsg` per row. EPSG
# codes that PROJ does not know (0, typos) count as missing; missing ones
# are guessed from the coordinates (guess_crs()). Returns x, y (NA where not
# possible) and `from`: "file", "guessed (unknown code in file)", "guessed",
# "unknown, not transformed" or NA (no coordinates).
coords_to_25832 <- function(x, y, epsg = NA) {
  n <- length(x)
  given <- suppressWarnings(as.integer(rep_len(epsg, n)))
  codes <- unique(stats::na.omit(given))
  known <- codes[vapply(codes, function(code) {
    crs <- tryCatch(suppressWarnings(sf::st_crs(code)), error = function(e) sf::NA_crs_)
    !is.na(crs)
  }, logical(1))]
  guessed <- guess_crs(x, y)
  code_used <- ifelse(given %in% known, given, guessed)
  from <- dplyr::case_when(
    is.na(x) | is.na(y) ~ NA_character_,
    given %in% known ~ "file",
    !is.na(given) & !is.na(guessed) ~ "guessed (unknown code in file)",
    !is.na(guessed) ~ "guessed",
    .default = "unknown, not transformed"
  )
  out_x <- rep(NA_real_, n)
  out_y <- rep(NA_real_, n)
  for (code in unique(stats::na.omit(code_used))) {
    k <- which(code_used == code & !is.na(x) & !is.na(y))
    if (length(k) == 0) next
    xk <- x[k]
    xk[xk >= 32e6 & xk < 34e6] <- xk[xk >= 32e6 & xk < 34e6] %% 1e6 # UTM zone prefix
    xy <- tryCatch(
      sf::st_coordinates(sf::st_transform(sf::st_as_sf(
        data.frame(x = xk, y = y[k]), coords = c("x", "y"), crs = as.integer(code)
      ), 25832)),
      error = function(e) {
        warning("Coordinates with EPSG ", code, " not transformed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(xy)) {
      from[k] <- "unknown, not transformed"
      next
    }
    out_x[k] <- xy[, 1]
    out_y[k] <- xy[, 2]
  }
  list(x = out_x, y = out_y, from = from)
}

# A table as csv (UTF-8, comma separated, empty cells for NA); returns the
# path (for a target with format = "file").
write_csv_file <- function(data, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}
