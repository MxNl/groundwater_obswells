# State level series: state deliveries and MENTOR, read in the pipeline -------------
#
# Reads the raw groundwater level sources of the states (state deliveries
# and MENTOR) and builds one long table: one row per well and measurement
# date with the level (gwl_m_nhn, m a.s.l.), the ground elevation (gok_m_nhn)
# and the depth (depth_m), as the script gw_timeseries_all_states.R did. The
# logic follows the per-state scripts GW_Staende_Laenderdaten_<state>.R:
#   * no clipping of negative depths (flag above_ground);
#   * all wells are kept (the pipeline links them to FARM, R/levels.R);
#   * the ground elevation comes from several master data tables in a fixed
#     priority order (gok_source);
#   * depth_ref: "GOK" = depth below ground, "Messpunkt" = depth below the
#     measuring point as delivered (RP, BW Abstich);
#   * aggregation: "single" or "monthly_mean" (NI);
#   * wells delivered by the state and by MENTOR: the state data only.
# States without a reader yet: SL, ST, SN, TH.
#
# Paths: state_levels_config in _targets.R. A state whose files cannot be
# read is left out with a warning; the others are still used.

# IDs as character, without scientific notation (as.character(100000) is
# "1e+05")
sl_id <- function(x) {
  out <- if (is.numeric(x)) formatC(x, format = "f", digits = 0) else as.character(x)
  out[is.na(x)] <- NA
  trimws(out)
}

# numbers with a decimal comma ("12,34") or already numeric
sl_num <- function(x) suppressWarnings(as.numeric(sub(",", ".", as.character(x), fixed = TRUE)))

# German numbers with thousands separator ("1.234,56")
sl_num_de <- function(x) {
  suppressWarnings(readr::parse_number(
    as.character(x), locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  ))
}

sl_date <- function(x, format = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (!is.null(format)) return(as.Date(as.character(x), format = format))
  as.Date(x)
}

# A column by its name, with a clear error if it is missing (the names with
# umlauts are written as \u escapes)
sl_col <- function(d, name) {
  if (!name %in% names(d)) {
    stop("Column '", name, "' not found. Columns: ", paste(names(d), collapse = ", "), call. = FALSE)
  }
  d[[name]]
}

# the one object of an .RData file
sl_load_one <- function(file) {
  if (!file.exists(file)) stop("file not found: ", file, call. = FALSE)
  env <- new.env()
  get(load(file, envir = env)[1], envir = env)
}

# Ground elevation from several master data tables, in order of priority:
# `tabs` is a named list of tibbles with messstellen_id and gok; the name is
# stored in gok_source; the first table with a value wins.
sl_gok_by_priority <- function(tabs) {
  parts <- lapply(names(tabs), function(name) {
    tibble::tibble(
      gok_source = name,
      messstellen_id = sl_id(tabs[[name]]$messstellen_id),
      gok_m_nhn = sl_num(tabs[[name]]$gok)
    )
  })
  dplyr::bind_rows(parts) |>
    dplyr::filter(!is.na(messstellen_id), !is.na(gok_m_nhn)) |>
    dplyr::mutate(prio = match(gok_source, names(tabs))) |>
    dplyr::arrange(messstellen_id, prio) |>
    dplyr::distinct(messstellen_id, .keep_all = TRUE) |>
    dplyr::select(messstellen_id, gok_m_nhn, gok_source)
}

# MENTOR rows of one or more state codes (bl, messstellen_id, datum,
# nn_messwert)
sl_mentor <- function(mentor, state) {
  m <- mentor[mentor$bl %in% state, ]
  tibble::tibble(
    messstellen_id = sl_id(m$messstellen_id),
    datum = sl_date(m$datum),
    gwl_m_nhn = sl_num(m$nn_messwert),
    source = "mentor"
  )
}

# ---- readers per state -------------------------------------------------------------
# Each takes the config (paths) and the MENTOR table and returns the rows of
# its state.

sl_read_mv <- function(cfg, mentor) {
  # INVID (state ID, used in FARM) <-> INVKENN (= MKZ, used by MENTOR)
  link_raw <- readxl::read_excel(file.path(cfg$dir_orig, "MV/Lieferung2/test/20210510_MV_Verkn\u00fcpfungsfelder.xlsx"))
  link <- tibble::tibble(INVID = sl_id(sl_col(link_raw, "[INVID]")), MKZ = sl_id(sl_col(link_raw, "[INVKENN]"))) |>
    dplyr::distinct()
  # MENTOR master data are keyed by MKZ -> translated to INVID
  stamm_mentor <- readxl::read_excel(file.path(cfg$dir_gwst, "info_mentor/Stammdaten_mv.xlsx"))
  stamm_mentor$MKZ <- sl_id(stamm_mentor$MKZ)
  stamm_mentor <- dplyr::inner_join(stamm_mentor, link, by = "MKZ")
  stamm_db <- utils::read.csv(file.path(cfg$dir_dbstamm, "MV_Stammdaten_neu.csv"))
  gok <- sl_gok_by_priority(list(
    mentor_stamm_gelaende = tibble::tibble(messstellen_id = stamm_mentor$INVID,
                                           gok = sl_col(stamm_mentor, "GEL\u00c4NDEH\u00d6HE")),
    # fallback: height of the measuring point (top of casing, usually above ground)
    laender_messpunkt = tibble::tibble(messstellen_id = sl_col(stamm_db, "Messtellen.ID"),
                                       gok = sl_col(stamm_db, "Messpunkth\u00f6he..m.NN."))
  ))
  w <- readxl::read_excel(file.path(cfg$dir_orig, "MV/Lieferung2/Excel_export/GEODIN_MES_WSTTAB01.xlsx"))
  laender <- tibble::tibble(
    messstellen_id = sl_id(w$INVID), datum = sl_date(w$SMPDATE), gwl_m_nhn = sl_num(w$WASSPNN),
    source = "laender"
  ) |>
    dplyr::left_join(link, by = c("messstellen_id" = "INVID")) |>
    dplyr::rename(id_alt = MKZ)
  # MENTOR MV uses MKZ as ID -> translated to INVID where possible
  mentor_mv <- sl_mentor(mentor, "MV") |>
    dplyr::rename(MKZ = messstellen_id) |>
    dplyr::left_join(dplyr::distinct(link, MKZ, .keep_all = TRUE), by = "MKZ") |>
    dplyr::mutate(messstellen_id = dplyr::coalesce(INVID, MKZ), id_alt = MKZ) |>
    dplyr::select(-INVID, -MKZ)
  dplyr::bind_rows(laender, mentor_mv) |> dplyr::left_join(gok, by = "messstellen_id")
}

sl_read_sh <- function(cfg, mentor) {
  s_l <- readxl::read_excel(file.path(cfg$dir_orig, "SH/SH_Stammdaten.xlsx"))
  stamm_l <- tibble::tibble(messstellen_id = sl_col(s_l, "Messtellen-ID"), gok = sl_col(s_l, "GOK [m NN]"))
  s_m <- readxl::read_excel(file.path(cfg$dir_gwst, "info_mentor/Stammdaten_SH.xlsx"))
  stamm_m <- tibble::tibble(messstellen_id = sl_col(s_m, "Nummer"), gok = sl_col(s_m, "Gel\u00e4ndeh\u00f6he"))
  # columns by position: 2 = Messstellen_ID, 3 = date, 6 = level above sea level
  w <- readr::read_delim(file.path(cfg$dir_orig, "SH/GW/Grundwasserstaende_Messstellen_FARM_SH.txt"),
                         delim = ";", col_types = readr::cols(.default = readr::col_character()))
  laender <- tibble::tibble(
    messstellen_id = sl_id(w[[2]]), datum = sl_date(w[[3]], "%d.%m.%Y"), gwl_m_nhn = sl_num_de(w[[6]]),
    source = "laender"
  ) |>
    dplyr::left_join(sl_gok_by_priority(list(laender_stamm = stamm_l, mentor_stamm = stamm_m)), by = "messstellen_id")
  mentor_sh <- sl_mentor(mentor, "SH") |>
    dplyr::left_join(sl_gok_by_priority(list(mentor_stamm = stamm_m, laender_stamm = stamm_l)), by = "messstellen_id")
  dplyr::bind_rows(laender, mentor_sh)
}

# BB: files without header (X1 = date, X2 = level); the ID is in the file name
sl_read_bb_folder <- function(path, file_suffix) {
  files <- list.files(path, pattern = "csv", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) return(NULL)
  d <- readr::read_delim(files, delim = ";", col_names = FALSE, id = "file",
                         col_types = readr::cols(.default = readr::col_character()))
  id <- regmatches(d$file, regexec(paste0("exp\\s*(.*?)\\s*_", file_suffix), d$file))
  id <- vapply(id, function(m) if (length(m) >= 2) m[2] else NA_character_, character(1))
  tibble::tibble(
    messstellen_id = sl_id(suppressWarnings(as.numeric(id))),
    datum = sl_date(d$X1, "%d.%m.%Y"),
    gwl_m_nhn = sl_num(d$X2)
  ) |>
    dplyr::filter(!is.na(datum), !is.na(gwl_m_nhn))
}

sl_read_bb <- function(cfg, mentor) {
  laender <- dplyr::bind_rows(
    sl_read_bb_folder(file.path(cfg$dir_orig, "BB/LfU_BB_Datenanfrage_05_02_21/LfU BB_Ganglinien"), "260321"),
    sl_read_bb_folder(file.path(cfg$dir_orig, "BB/Nachtrag1/STYX_ZR_ALLE/STYX_ZR_ALLE"), "241121"),
    sl_read_bb_folder(file.path(cfg$dir_orig, "BB/Nachtrag1/STYX_ZR_bis2021/STYX_ZR_bis2021"), "011221")
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(source = "laender")
  styx <- readxl::read_excel(file.path(cfg$dir_orig, "BB/Nachtrag1/STYX_ZR_Stammdaten.xlsx"))
  ment <- readxl::read_excel(file.path(cfg$dir_gwst, "info_mentor/Stammdaten_BB.xlsx"))
  bbst <- readxl::read_excel(file.path(cfg$dir_orig, "BB/BB_Stammdaten.xlsx"))
  gwb <- tibble::as_tibble(foreign::read.dbf(file.path(cfg$dir_gwst, "gw_basis_mn/gw_basis_mn_20220101/gw_basis_mn.dbf")))
  # same priority as GOK_final in the original script
  gok <- sl_gok_by_priority(list(
    styx_stamm = tibble::tibble(messstellen_id = sl_col(styx, "MKZ"), gok = sl_col(styx, "Gel\u00e4ndeh\u00f6he")),
    mentor_stamm = tibble::tibble(messstellen_id = sl_col(ment, "MKZ"), gok = sl_col(ment, "GOK")),
    bb_stamm = tibble::tibble(messstellen_id = sl_col(bbst, "Messtellen-ID"), gok = sl_col(bbst, "GOK [m NN]")),
    gw_basis_mn = tibble::tibble(messstellen_id = sl_id(sl_col(gwb, "MKZ")), gok = sl_col(gwb, "GLH"))
  ))
  dplyr::bind_rows(laender, sl_mentor(mentor, "BB")) |> dplyr::left_join(gok, by = "messstellen_id")
}

sl_read_bw <- function(cfg, mentor) {
  fix_id <- function(x) gsub("/", "-", sl_id(x), fixed = TRUE)
  st <- readxl::read_excel(file.path(cfg$dir_orig, "BW/Stammdaten_FARM_LUBW_211209.xlsx"), sheet = 2)
  gok <- sl_gok_by_priority(list(lubw_stamm = tibble::tibble(
    messstellen_id = fix_id(sl_col(st, "GW-Nummer")), gok = sl_col(st, "Gel\u00e4ndeh\u00f6he [m +NN]")
  )))
  # columns by position: 1 = GW-Nummer, 3 = date, 7 = unit, 8 = value
  w <- readxl::read_excel(file.path(cfg$dir_orig, "BW/Grundwasserstand-Daten_FARM_LUBW_211221.xlsx"))
  laender <- tibble::tibble(
    messstellen_id = fix_id(w[[1]]), datum = sl_date(w[[3]]), gwl_m_nhn = sl_num(w[[8]]),
    unit = as.character(w[[7]]), source = "laender"
  )
  # depth measured at sampling (Abstich, cm) from the water quality file,
  # below the measuring point
  q <- readxl::read_excel(file.path(cfg$dir_orig, "BW/G\u00fcte-Daten_FARM_LUBW_211215.xlsx"))
  abstich <- tibble::tibble(
    messstellen_id = fix_id(sl_col(q, "GW-Nummer")), datum = sl_date(sl_col(q, "Zeitpunkt")),
    depth_m = sl_num(sl_col(q, "Abstich")) / 100, depth_ref = "Messpunkt", source = "laender_abstich"
  ) |>
    dplyr::filter(!is.na(depth_m))
  mentor_bw <- sl_mentor(mentor, "BW") |> dplyr::mutate(messstellen_id = fix_id(messstellen_id))
  dplyr::bind_rows(laender, abstich, mentor_bw) |> dplyr::left_join(gok, by = "messstellen_id")
}

sl_read_by <- function(cfg, mentor) {
  f_bde <- file.path(cfg$dir_orig, "BY/Gru_BGR_Prof_Broda/BIS_Daten_BGR_Prof_Broda/bde_export_bg_15062021_142744.xlsx")
  # the sheets have 2 description rows below the header
  s1 <- readxl::read_excel(f_bde, sheet = 1, col_types = "text")[-(1:2), ]
  stamm <- tibble::tibble(
    OBJEKTID = s1$OBJEKTID,
    Objektkennzahl = dplyr::coalesce(s1$INFOWAS_ID, s1$INFOWAS_ID2),  # ID used in FARM
    INFOWAS_LGD_MSNR = s1$INFOWAS_LGD_MSNR,                           # ID used in MENTOR
    G_HOEHE = s1$G_HOEHE
  ) |>
    dplyr::distinct()
  # MENTOR LGD number -> Objektkennzahl (keeps the LGD number if no match)
  lgd2obj <- stamm |>
    dplyr::filter(!is.na(INFOWAS_LGD_MSNR), !is.na(Objektkennzahl)) |>
    dplyr::distinct(INFOWAS_LGD_MSNR, .keep_all = TRUE)
  to_farm_id <- function(lgd) dplyr::coalesce(lgd2obj$Objektkennzahl[match(lgd, lgd2obj$INFOWAS_LGD_MSNR)], lgd)
  sm <- readxl::read_excel(file.path(cfg$dir_gwst, "info_mentor/Stammdaten_BY.xlsx"))
  stamm_mentor <- tibble::tibble(messstellen_id = to_farm_id(sl_id(sl_col(sm, "LGD-Mst-Nr."))),
                                 gok = sl_col(sm, "Gel\u00e4ndeh\u00f6he (NN+m), aktuell"))
  stamm_bde <- tibble::tibble(messstellen_id = stamm$Objektkennzahl, gok = stamm$G_HOEHE)
  gok_l <- sl_gok_by_priority(list(bde_stamm = stamm_bde, mentor_stamm = stamm_mentor))
  gok_m <- sl_gok_by_priority(list(mentor_stamm = stamm_mentor, bde_stamm = stamm_bde))
  s8 <- readxl::read_excel(f_bde, sheet = 8, col_types = "text")[-(1:2), ]
  s8 <- dplyr::left_join(
    s8, dplyr::distinct(stamm, OBJEKTID, .keep_all = TRUE) |> dplyr::select(OBJEKTID, Objektkennzahl, INFOWAS_LGD_MSNR),
    by = "OBJEKTID"
  )
  laender <- tibble::tibble(
    messstellen_id = s8$Objektkennzahl, id_alt = s8$INFOWAS_LGD_MSNR,
    datum = sl_date(s8$DATUM, "%Y%m%d"), gwl_m_nhn = sl_num(s8$GW_STAND_NN), source = "laender"
  ) |>
    dplyr::filter(!is.na(gwl_m_nhn)) |>
    dplyr::left_join(gok_l, by = "messstellen_id")
  mentor_by <- sl_mentor(mentor, "BY") |>
    dplyr::mutate(id_alt = messstellen_id, messstellen_id = to_farm_id(messstellen_id))
  # later MENTOR delivery (Nachlieferung): LGD number, date, level
  nl <- tibble::as_tibble(sl_load_one(file.path(cfg$dir_gwst, "neuer_Abgleich_Mentor/nachlieferung_BY_mentor_stamm.RData")))
  nachlieferung <- tibble::tibble(
    id_alt = sl_id(nl$messstellen_id), messstellen_id = to_farm_id(sl_id(nl$messstellen_id)),
    datum = sl_date(nl$date), gwl_m_nhn = sl_num(nl$gwl), source = "mentor_nachlieferung"
  )
  dplyr::bind_rows(
    laender,
    dplyr::bind_rows(mentor_by, nachlieferung) |> dplyr::left_join(gok_m, by = "messstellen_id")
  )
}

sl_read_he <- function(cfg, mentor) {
  f_he <- file.path(cfg$dir_orig, "HE/Berthold_W4_2021-04-16_083539/Berthold_W4/BGR_Anfrage_202103_gesamt.xlsx")
  stamm_l <- readxl::read_excel(f_he, sheet = 1)
  stamm_l$messstellen_id <- sl_id(stamm_l$ID)
  sm <- readxl::read_excel(file.path(cfg$dir_gwst, "info_mentor/Stammdaten_HE.xlsx"))
  stamm_m <- tibble::tibble(messstellen_id = sl_id(sm$ID), gok = sm$GELAENDEHOEHE)
  w <- readxl::read_excel(f_he, sheet = 3)
  laender <- tibble::tibble(
    messstellen_id = sl_id(w$MESSSTELLE_ID), datum = sl_date(w$MESSDATUM), gwl_m_nhn = sl_num(w$MESSWERT),
    source = "laender"
  ) |>
    dplyr::left_join(sl_gok_by_priority(list(
      laender_gelaende = tibble::tibble(messstellen_id = stamm_l$messstellen_id, gok = stamm_l$GELAENDEHOEHE),
      laender_messpunkt = tibble::tibble(messstellen_id = stamm_l$messstellen_id, gok = stamm_l$MESSPUNKTHOEHE),
      mentor_stamm = stamm_m
    )), by = "messstellen_id")
  mentor_he <- sl_mentor(mentor, "HE") |>
    dplyr::left_join(sl_gok_by_priority(list(
      mentor_stamm = stamm_m,
      laender_gelaende = tibble::tibble(messstellen_id = stamm_l$messstellen_id, gok = stamm_l$GELAENDEHOEHE)
    )), by = "messstellen_id")
  dplyr::bind_rows(laender, mentor_he)
}

sl_read_rp <- function(cfg, mentor) {
  # the state data only contain the depth below the measuring point
  w <- readxl::read_excel(file.path(cfg$dir_orig, "RP/RP_GW_Messst_PSM_nrM_Werte.xlsx"), sheet = 2)
  w <- w[w$STOFF_BEZ %in% "Wasserstand unter Messpunkt", ]
  laender <- tibble::tibble(
    messstellen_id = sl_id(w$MESSST_NR), datum = sl_date(w$ANA_DATUM), depth_m = sl_num(w$WERT),
    depth_ref = "Messpunkt", source = "laender"
  )
  # no RP master data with ground elevation yet: MENTOR RP rows get a level
  # but no depth
  dplyr::bind_rows(laender, sl_mentor(mentor, "RP"))
}

# "Jan 2010", "M\u00e4r 2010", "Mrz 2010", "Okt 2010" ... -> first day of that month
sl_month_to_date <- function(x) {
  m <- tolower(substr(trimws(x), 1, 3))
  month <- dplyr::case_when(
    startsWith(m, "ja") ~ 1L,
    startsWith(m, "f") ~ 2L,
    grepl("^ma[iy]", m) ~ 5L,
    startsWith(m, "m") ~ 3L, # Maerz / Mrz / Mar (also with a broken umlaut)
    startsWith(m, "ap") ~ 4L,
    startsWith(m, "jun") ~ 6L,
    startsWith(m, "jul") ~ 7L,
    startsWith(m, "au") ~ 8L,
    startsWith(m, "s") ~ 9L,
    startsWith(m, "o") ~ 10L,
    startsWith(m, "n") ~ 11L,
    startsWith(m, "d") ~ 12L
  )
  year <- suppressWarnings(as.integer(ifelse(grepl("\\d{4}", x), sub("^.*?(\\d{4}).*$", "\\1", x), NA)))
  as.Date(ISOdate(year, month, 1))
}

sl_read_ni <- function(cfg, mentor) {
  f_ni <- file.path(
    cfg$dir_orig, "NI/WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx",
    "WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx.csv"
  )
  # monthly values 2010-2020: mean, minimum and maximum level per month
  w <- readr::read_csv2(f_ni, col_types = readr::cols(`ID Messstelle` = readr::col_character(),
                                                      Monat = readr::col_character()))
  laender <- tibble::tibble(
    messstellen_id = sl_id(w$`ID Messstelle`), datum = sl_month_to_date(w$Monat),
    gwl_m_nhn = sl_num(w$`mittlerer GW-Stand`), gwl_min_period = sl_num(w$`min. GW-Stand`),
    gwl_max_period = sl_num(w$`max. GW-Stand`), depth_m = sl_num(w$`GW-Flurabstand`),
    gok_m_nhn = sl_num(w$GOK_NN), gok_source = "ni_monatswerte", n_period = as.integer(w$Anzahl),
    aggregation = "monthly_mean", source = "laender"
  )
  # MENTOR NI: ground elevation from the same file
  gok <- sl_gok_by_priority(list(ni_monatswerte = dplyr::distinct(laender, messstellen_id, gok = gok_m_nhn)))
  dplyr::bind_rows(laender, sl_mentor(mentor, "NI") |> dplyr::left_join(gok, by = "messstellen_id"))
}

sl_read_nw <- function(cfg, mentor) {
  # LANUV open data (all NRW wells): level, depth, ground elevation; decimal
  # point; IDs with leading zeros, so read as text
  raw <- readr::read_delim(file.path(cfg$dir_orig, "NRW/wasserstand/opendata.gw_wasserstand.csv"),
                           delim = ";", col_types = readr::cols(.default = readr::col_character()))
  laender <- tibble::tibble(
    messstellen_id = raw$messstelle_id, datum = sl_date(raw$datum_messung, "%Y%m%d"),
    gwl_m_nhn = sl_num(raw$wasserstd_m), depth_m = sl_num(raw$flurabstd_m),
    gok_m_nhn = sl_num(raw$gok_m), gok_source = "nw_opendata",
    note = dplyr::na_if(raw$hinweis, "-"), source = "laender"
  )
  # MENTOR NW: ground elevation from the open data (last known value per well)
  gok <- laender |>
    dplyr::filter(!is.na(gok_m_nhn)) |>
    dplyr::arrange(messstellen_id, dplyr::desc(datum)) |>
    dplyr::transmute(messstellen_id, gok = gok_m_nhn)
  gok <- sl_gok_by_priority(list(nw_opendata = gok))
  dplyr::bind_rows(laender, sl_mentor(mentor, c("NW", "NRW")) |> dplyr::left_join(gok, by = "messstellen_id"))
}

state_level_readers <- list(
  MV = sl_read_mv, SH = sl_read_sh, BB = sl_read_bb, BW = sl_read_bw, BY = sl_read_by,
  HE = sl_read_he, RP = sl_read_rp, NI = sl_read_ni, NW = sl_read_nw
)

# ---- input files and the combined table -----------------------------------------

# All input files of the readers (the BB folders as their files), with size
# and time of the last change: a target checked on every run, so changed or
# added deliveries rebuild the table.
state_level_input_info <- function(cfg) {
  o <- cfg$dir_orig
  g <- cfg$dir_gwst
  files <- c(
    file.path(g, "mentor_grundwasserstand_neu.Rdata"),
    file.path(o, "MV/Lieferung2/test/20210510_MV_Verkn\u00fcpfungsfelder.xlsx"),
    file.path(g, "info_mentor/Stammdaten_mv.xlsx"),
    file.path(cfg$dir_dbstamm, "MV_Stammdaten_neu.csv"),
    file.path(o, "MV/Lieferung2/Excel_export/GEODIN_MES_WSTTAB01.xlsx"),
    file.path(o, "SH/SH_Stammdaten.xlsx"), file.path(g, "info_mentor/Stammdaten_SH.xlsx"),
    file.path(o, "SH/GW/Grundwasserstaende_Messstellen_FARM_SH.txt"),
    list.files(file.path(o, c("BB/LfU_BB_Datenanfrage_05_02_21/LfU BB_Ganglinien",
                              "BB/Nachtrag1/STYX_ZR_ALLE/STYX_ZR_ALLE",
                              "BB/Nachtrag1/STYX_ZR_bis2021/STYX_ZR_bis2021")),
               pattern = "csv", full.names = TRUE, recursive = TRUE),
    file.path(o, "BB/Nachtrag1/STYX_ZR_Stammdaten.xlsx"), file.path(g, "info_mentor/Stammdaten_BB.xlsx"),
    file.path(o, "BB/BB_Stammdaten.xlsx"), file.path(g, "gw_basis_mn/gw_basis_mn_20220101/gw_basis_mn.dbf"),
    file.path(o, "BW/Stammdaten_FARM_LUBW_211209.xlsx"), file.path(o, "BW/Grundwasserstand-Daten_FARM_LUBW_211221.xlsx"),
    file.path(o, "BW/G\u00fcte-Daten_FARM_LUBW_211215.xlsx"),
    file.path(o, "BY/Gru_BGR_Prof_Broda/BIS_Daten_BGR_Prof_Broda/bde_export_bg_15062021_142744.xlsx"),
    file.path(g, "info_mentor/Stammdaten_BY.xlsx"), file.path(g, "neuer_Abgleich_Mentor/nachlieferung_BY_mentor_stamm.RData"),
    file.path(o, "HE/Berthold_W4_2021-04-16_083539/Berthold_W4/BGR_Anfrage_202103_gesamt.xlsx"),
    file.path(g, "info_mentor/Stammdaten_HE.xlsx"),
    file.path(o, "RP/RP_GW_Messst_PSM_nrM_Werte.xlsx"),
    file.path(o, "NI/WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx",
              "WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx.csv"),
    file.path(o, "NRW/wasserstand/opendata.gw_wasserstand.csv")
  )
  info <- file.info(files)
  tibble::tibble(file = files, exists = file.exists(files), size = info$size, modified = as.character(info$mtime))
}

# The combined table of all states in cfg$states (one row per well and
# measurement date), wells delivered by the state and by MENTOR with the
# state data only. `inputs` (state_level_input_info()) only reruns the step
# when the files change. A state whose files cannot be read is left out with
# a warning. Returns list(levels, log): log has one row per state (and
# MENTOR) with the rows and wells read, or the error.
build_state_levels <- function(cfg, inputs = NULL) {
  template <- tibble::tibble(
    state = character(), source = character(), messstellen_id = character(), id_alt = character(),
    datum = as.Date(character()), gwl_m_nhn = numeric(), gok_m_nhn = numeric(), gok_source = character(),
    depth_m = numeric(), depth_ref = character(), aggregation = character(), n_period = integer(),
    gwl_min_period = numeric(), gwl_max_period = numeric(), unit = character(), note = character()
  )
  log_row <- function(part, status, rows = 0L, wells = 0L, error = NA_character_) {
    tibble::tibble(part = part, status = status, rows = rows, wells = wells, error = error)
  }
  mentor_file <- file.path(cfg$dir_gwst, "mentor_grundwasserstand_neu.Rdata")
  mentor_empty <- tibble::tibble(bl = character(), messstellen_id = character(),
                                 datum = as.Date(character()), nn_messwert = numeric())
  mentor <- tryCatch({
    m <- tibble::as_tibble(sl_load_one(mentor_file))
    need <- c("bl", "messstellen_id", "datum", "nn_messwert")
    if (!all(need %in% names(m))) {
      stop("columns ", paste(setdiff(need, names(m)), collapse = ", "), " missing; it has: ",
           paste(names(m), collapse = ", "))
    }
    m
  }, error = function(e) structure(mentor_empty, error = conditionMessage(e)))
  log <- if (is.null(attr(mentor, "error"))) {
    log_row("MENTOR", "read", nrow(mentor), length(unique(mentor$messstellen_id)))
  } else {
    warning("State levels: MENTOR file not used (", attr(mentor, "error"), "); only state deliveries.",
            call. = FALSE)
    log_row("MENTOR", "failed", error = attr(mentor, "error"))
  }
  states <- intersect(cfg$states %||% names(state_level_readers), names(state_level_readers))
  parts <- list()
  for (st in states) {
    message("State levels: reading ", st, " ...")
    part <- tryCatch(
      dplyr::mutate(state_level_readers[[st]](cfg, mentor), state = st),
      error = function(e) structure(list(), error = conditionMessage(e))
    )
    if (!is.null(attr(part, "error"))) {
      warning("State levels: ", st, " not read: ", attr(part, "error"), call. = FALSE)
      log <- dplyr::bind_rows(log, log_row(st, "failed", error = attr(part, "error")))
    } else {
      parts[[st]] <- part
      log <- dplyr::bind_rows(log, log_row(st, "read", nrow(part), length(unique(part$messstellen_id))))
    }
  }
  raw <- dplyr::bind_rows(c(list(template), unname(parts)))
  raw <- raw[names(template)]
  all <- raw |>
    dplyr::mutate(
      aggregation = dplyr::coalesce(aggregation, "single"),
      depth_ref = dplyr::coalesce(depth_ref, "GOK"),
      # depth = GOK - level; where only a depth below GROUND was delivered,
      # the level from it (not for depths below the measuring point: the
      # pipeline uses the FARM measuring point there, link_state_levels())
      depth_m = dplyr::coalesce(depth_m, gok_m_nhn - gwl_m_nhn),
      gwl_m_nhn = dplyr::coalesce(gwl_m_nhn, dplyr::if_else(depth_ref == "GOK", gok_m_nhn - depth_m, NA_real_)),
      above_ground = depth_m < 0
    ) |>
    dplyr::filter(!is.na(messstellen_id), !is.na(datum)) |>
    dplyr::distinct()
  # wells delivered by the state and by MENTOR: the state data only
  levels <- all |>
    dplyr::mutate(from_mentor = startsWith(source, "mentor")) |>
    dplyr::filter(!(from_mentor & any(!from_mentor)), .by = c(state, messstellen_id)) |>
    dplyr::select(-from_mentor) |>
    dplyr::arrange(state, messstellen_id, source, datum)
  list(levels = levels, log = log)
}

# Wells of the state series, as in the script's overview: per well the
# sources, period, number of values, ground elevation and its source, mean
# level and depth (output/state_levels_wells.csv).
summarise_state_level_wells <- function(levels) {
  levels |>
    dplyr::summarise(
      sources = paste(sort(unique(source)), collapse = ", "),
      aggregation = paste(sort(unique(aggregation)), collapse = ", "),
      depth_ref = paste(sort(unique(depth_ref)), collapse = ", "),
      gok_m_nhn = dplyr::first(gok_m_nhn), gok_source = dplyr::first(gok_source),
      start = min(datum), end = max(datum), n_points = dplyr::n(),
      gwl_mean = mean(gwl_m_nhn, na.rm = TRUE), depth_mean = mean(depth_m, na.rm = TRUE),
      n_above_ground = sum(above_ground, na.rm = TRUE),
      .by = c(state, messstellen_id)
    ) |>
    dplyr::mutate(dplyr::across(c(gwl_mean, depth_mean), \(x) ifelse(is.nan(x), NA_real_, x)))
}
