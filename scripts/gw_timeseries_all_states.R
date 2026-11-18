## Used by the pipeline: the .RData written at the end (object gwl_preferred)
## is the first-priority source of groundwater levels (R/levels.R); its path
## is level_config$state_levels_file in _targets.R. Run this script again
## when deliveries change; the pipeline notices the new file. Not run by the
## pipeline itself (it reads the deliveries from the project drives).
## =============================================================================
## Groundwater level time series - all federal states
##
## Reads the raw groundwater level sources (Laender deliveries + MENTOR) of all
## states and builds ONE long table:
##   one row per well x measurement date with
##     gwl_m_nhn  groundwater level            [m NHN]
##     gok_m_nhn  ground elevation (GOK)       [m NHN]
##     depth_m    depth to groundwater (Flab)  [m below ground], = GOK - GWL
##
## Logic follows the per-state scripts GW_Staende_Laenderdaten_<BL>.R.
## Differences to those scripts (on purpose):
##   * no clipping of negative depths to 0 -> raw value kept, see flag
##     `above_ground` (clip later if wanted: pmax(depth_m, 0))
##   * all wells are kept, not only FARM wells -> see column `in_farm`
##   * GOK is taken from several master-data tables in a fixed priority order;
##     `gok_source` tells which table was used for each well
##
##   * column `depth_ref`: "GOK" = depth below ground, "Messpunkt" = depth below
##     measuring point as delivered (RP, BW Abstich) -> slightly too deep
##   * column `aggregation`: "single" = single measurement, "monthly_mean" = NI
##     (monthly means; the monthly min/max are kept in gwl_min_period/gwl_max_period
##     and used for the extremes in the per-well overview)
##
## States still missing (scripts not yet available): SL, ST, SN, TH
## =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)   # year(); part of tidyverse >= 2.0 but loaded explicitly

# ---- paths -------------------------------------------------------------------
dir_orig      <- "J:/PROJEKTE/FARM/Daten/Originaldaten_Laender"
dir_gwst      <- "J:/PROJEKTE/FARM/Daten/Datenanalyse/Grundwasserstaende"
dir_dbstamm   <- "J:/PROJEKTE/FARM/Daten/Datenbank_Laender/Stammdaten"
file_farm_mst <- "F:/PROJEKTE/FARM/Daten/Datenanalyse/Stoffauswahl/messstellen_bl.RData"
file_mentor   <- file.path(dir_gwst, "mentor_grundwasserstand_neu.Rdata")
dir_out       <- file.path(dir_gwst, "final_output")


# ---- helpers -----------------------------------------------------------------

# load an .RData file containing one object and return that object
load_one <- function(file) get(load(file))

# IDs as character, without scientific notation (as.character(100000) = "1e+05")
as_id <- function(x) {
  out <- if (is.numeric(x)) formatC(x, format = "f", digits = 0) else as.character(x)
  out[is.na(x)] <- NA
  str_trim(out)
}

# numbers with decimal comma ("12,34") or already numeric
num <- function(x) as.numeric(str_replace(as.character(x), ",", "."))

# German numbers with thousands separator ("1.234,56")
num_de <- function(x) {
  parse_number(as.character(x), locale = locale(decimal_mark = ",", grouping_mark = "."))
}

to_date <- function(x, format = NULL) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (!is.null(format))      return(as.Date(as.character(x), format = format))
  as.Date(x)
}

# Ground elevation from several master-data tables, in order of priority.
# Every argument is a tibble with columns messstellen_id + gok; the argument
# name is stored in `gok_source`. The first table with a non-NA value wins.
gok_by_priority <- function(...) {
  tabs <- list(...)
  tabs %>%
    map(~ transmute(.x, messstellen_id = as_id(messstellen_id), gok_m_nhn = num(gok))) %>%
    bind_rows(.id = "gok_source") %>%
    filter(!is.na(messstellen_id), !is.na(gok_m_nhn)) %>%
    mutate(prio = match(gok_source, names(tabs))) %>%
    arrange(messstellen_id, prio) %>%
    distinct(messstellen_id, .keep_all = TRUE) %>%
    select(messstellen_id, gok_m_nhn, gok_source)
}

# MENTOR data (all states in one file): bl, messstellen_id, datum, nn_messwert
mentor_all <- load_one(file_mentor) %>% as_tibble()

read_mentor <- function(state) {      # state can be several codes, e.g. c("NW", "NRW")
  mentor_all %>%
    filter(bl %in% .env$state) %>%
    transmute(messstellen_id = as_id(messstellen_id),
              datum          = to_date(datum),
              gwl_m_nhn      = num(nn_messwert),
              source         = "mentor")
}


# ---- MV ----------------------------------------------------------------------
read_mv <- function() {
  # INVID (Laender ID, used in FARM) <-> INVKENN (= MKZ, used by MENTOR)
  link <- read_excel(file.path(dir_orig, "MV/Lieferung2/test/20210510_MV_Verknüpfungsfelder.xlsx")) %>%
    transmute(INVID = as_id(`[INVID]`), MKZ = as_id(`[INVKENN]`)) %>%
    distinct()

  # MENTOR master data is keyed by MKZ -> translate to INVID
  # (the original script joined without `by=`; MKZ is assumed to be the common column)
  stamm_mentor <- read_excel(file.path(dir_gwst, "info_mentor/Stammdaten_mv.xlsx")) %>%
    mutate(MKZ = as_id(MKZ)) %>%
    inner_join(link, by = "MKZ")

  stamm_db <- read.csv(file.path(dir_dbstamm, "MV_Stammdaten_neu.csv")) %>% as_tibble()

  gok <- gok_by_priority(
    mentor_stamm_gelaende = stamm_mentor %>% transmute(messstellen_id = INVID, gok = GELÄNDEHÖHE),
    # fallback: height of the measuring point (top of casing, usually above ground)
    laender_messpunkt     = stamm_db %>% transmute(messstellen_id = Messtellen.ID, gok = Messpunkthöhe..m.NN.)
  )

  laender <- read_excel(file.path(dir_orig, "MV/Lieferung2/Excel_export/GEODIN_MES_WSTTAB01.xlsx")) %>%
    transmute(messstellen_id = as_id(INVID),
              datum          = to_date(SMPDATE),
              gwl_m_nhn      = num(WASSPNN),
              source         = "laender") %>%
    left_join(link, by = c("messstellen_id" = "INVID")) %>%
    rename(id_alt = MKZ)

  # MENTOR MV uses MKZ as ID -> translate to INVID where possible
  mentor <- read_mentor("MV") %>%
    rename(MKZ = messstellen_id) %>%
    left_join(distinct(link, MKZ, .keep_all = TRUE), by = "MKZ") %>%
    mutate(messstellen_id = coalesce(INVID, MKZ), id_alt = MKZ) %>%
    select(-INVID, -MKZ)

  bind_rows(laender, mentor) %>% left_join(gok, by = "messstellen_id")
}


# ---- SH ----------------------------------------------------------------------
read_sh <- function() {
  stamm_l <- read_excel(file.path(dir_orig, "SH/SH_Stammdaten.xlsx")) %>%
    transmute(messstellen_id = `Messtellen-ID`, gok = `GOK [m NN]`)
  stamm_m <- read_excel(file.path(dir_gwst, "info_mentor/Stammdaten_SH.xlsx")) %>%
    transmute(messstellen_id = Nummer, gok = Geländehöhe)

  # columns by position as in the original script:
  # 2 = Messstellen_ID, 3 = Datum, 6 = Wasserstand ueber NHN
  laender <- read_delim(file.path(dir_orig, "SH/GW/Grundwasserstaende_Messstellen_FARM_SH.txt"),
                        delim = ";", col_types = cols(.default = col_character())) %>%
    select(messstellen_id = 2, datum = 3, gwl = 6) %>%
    transmute(messstellen_id = as_id(messstellen_id),
              datum          = to_date(datum, "%d.%m.%Y"),
              gwl_m_nhn      = num_de(gwl),
              source         = "laender") %>%
    left_join(gok_by_priority(laender_stamm = stamm_l, mentor_stamm = stamm_m), by = "messstellen_id")

  mentor <- read_mentor("SH") %>%
    left_join(gok_by_priority(mentor_stamm = stamm_m, laender_stamm = stamm_l), by = "messstellen_id")

  bind_rows(laender, mentor)
}


# ---- BB ----------------------------------------------------------------------
read_bb_folder <- function(path, file_suffix) {
  files <- list.files(path, pattern = "csv", full.names = TRUE, recursive = TRUE)
  # files have no header: X1 = date, X2 = groundwater level; ID is in the file name
  read_delim(files, delim = ";", col_names = FALSE, id = "file",
             col_types = cols(.default = col_character())) %>%
    transmute(messstellen_id = as_id(as.numeric(str_match(file, paste0("exp\\s*(.*?)\\s*_", file_suffix))[, 2])),
              datum          = to_date(X1, "%d.%m.%Y"),
              gwl_m_nhn      = num(X2)) %>%
    filter(!is.na(datum), !is.na(gwl_m_nhn))
}

read_bb <- function() {
  laender <- bind_rows(
    read_bb_folder(file.path(dir_orig, "BB/LfU_BB_Datenanfrage_05_02_21/LfU BB_Ganglinien"), "260321"),
    read_bb_folder(file.path(dir_orig, "BB/Nachtrag1/STYX_ZR_ALLE/STYX_ZR_ALLE"),            "241121"),
    read_bb_folder(file.path(dir_orig, "BB/Nachtrag1/STYX_ZR_bis2021/STYX_ZR_bis2021"),      "011221")
  ) %>%
    distinct() %>%
    mutate(source = "laender")

  # same priority as GOK_final in the original script
  gok <- gok_by_priority(
    styx_stamm   = read_excel(file.path(dir_orig, "BB/Nachtrag1/STYX_ZR_Stammdaten.xlsx")) %>%
                     transmute(messstellen_id = MKZ, gok = Geländehöhe),
    mentor_stamm = read_excel(file.path(dir_gwst, "info_mentor/Stammdaten_BB.xlsx")) %>%
                     transmute(messstellen_id = MKZ, gok = GOK),
    bb_stamm     = read_excel(file.path(dir_orig, "BB/BB_Stammdaten.xlsx")) %>%
                     transmute(messstellen_id = `Messtellen-ID`, gok = `GOK [m NN]`),
    gw_basis_mn  = foreign::read.dbf(file.path(dir_gwst, "gw_basis_mn/gw_basis_mn_20220101/gw_basis_mn.dbf")) %>%
                     as_tibble() %>% transmute(messstellen_id = as_id(MKZ), gok = GLH)
  )

  bind_rows(laender, read_mentor("BB")) %>% left_join(gok, by = "messstellen_id")
}


# ---- BW ----------------------------------------------------------------------
read_bw <- function() {
  fix_id <- function(x) str_replace_all(as_id(x), "/", "-")

  gok <- read_excel(file.path(dir_orig, "BW/Stammdaten_FARM_LUBW_211209.xlsx"), sheet = 2) %>%
    transmute(messstellen_id = fix_id(`GW-Nummer`), gok = `Geländehöhe [m +NN]`) %>%
    gok_by_priority(lubw_stamm = .)

  # columns by position as in the original script: 1 = GW-Nummer, 3 = date, 7 = unit, 8 = value
  laender <- read_excel(file.path(dir_orig, "BW/Grundwasserstand-Daten_FARM_LUBW_211221.xlsx")) %>%
    select(messstellen_id = 1, datum = 3, unit = 7, gwl = 8) %>%
    transmute(messstellen_id = fix_id(messstellen_id),
              datum          = to_date(datum),
              gwl_m_nhn      = num(gwl),
              unit           = as.character(unit),
              source         = "laender")

  # depth measured at sampling (Abstich, in cm) from the water quality file.
  # The original script used it directly as depth below ground.
  abstich <- read_excel(file.path(dir_orig, "BW/Güte-Daten_FARM_LUBW_211215.xlsx")) %>%
    transmute(messstellen_id = fix_id(`GW-Nummer`),
              datum          = to_date(Zeitpunkt),
              depth_m        = num(Abstich) / 100,
              depth_ref      = "Messpunkt",
              source         = "laender_abstich") %>%
    filter(!is.na(depth_m))

  mentor <- read_mentor("BW") %>% mutate(messstellen_id = fix_id(messstellen_id))

  bind_rows(laender, abstich, mentor) %>% left_join(gok, by = "messstellen_id")
}


# ---- BY ----------------------------------------------------------------------
read_by <- function() {
  f_bde <- file.path(dir_orig, "BY/Gru_BGR_Prof_Broda/BIS_Daten_BGR_Prof_Broda/bde_export_bg_15062021_142744.xlsx")

  # sheets have 2 description rows below the header
  stamm <- read_excel(f_bde, sheet = 1, col_types = "text") %>%
    slice(-(1:2)) %>%
    transmute(OBJEKTID,
              Objektkennzahl   = coalesce(INFOWAS_ID, INFOWAS_ID2),  # ID used in FARM
              INFOWAS_LGD_MSNR,                                        # ID used in MENTOR
              G_HOEHE) %>%
    distinct()

  # MENTOR LGD number -> Objektkennzahl (keeps LGD number if no match)
  lgd2obj <- stamm %>%
    filter(!is.na(INFOWAS_LGD_MSNR), !is.na(Objektkennzahl)) %>%
    distinct(INFOWAS_LGD_MSNR, .keep_all = TRUE)
  to_farm_id <- function(lgd) coalesce(lgd2obj$Objektkennzahl[match(lgd, lgd2obj$INFOWAS_LGD_MSNR)], lgd)

  stamm_mentor <- read_excel(file.path(dir_gwst, "info_mentor/Stammdaten_BY.xlsx")) %>%
    transmute(messstellen_id = to_farm_id(as_id(`LGD-Mst-Nr.`)), gok = `Geländehöhe (NN+m), aktuell`)
  stamm_bde <- stamm %>% transmute(messstellen_id = Objektkennzahl, gok = G_HOEHE)

  gok_l <- gok_by_priority(bde_stamm = stamm_bde, mentor_stamm = stamm_mentor)
  gok_m <- gok_by_priority(mentor_stamm = stamm_mentor, bde_stamm = stamm_bde)

  laender <- read_excel(f_bde, sheet = 8, col_types = "text") %>%
    slice(-(1:2)) %>%
    left_join(distinct(stamm, OBJEKTID, .keep_all = TRUE) %>% select(OBJEKTID, Objektkennzahl, INFOWAS_LGD_MSNR),
              by = "OBJEKTID") %>%
    transmute(messstellen_id = Objektkennzahl,
              id_alt         = INFOWAS_LGD_MSNR,
              datum          = to_date(DATUM, "%Y%m%d"),
              gwl_m_nhn      = num(GW_STAND_NN),
              source         = "laender") %>%
    filter(!is.na(gwl_m_nhn)) %>%
    left_join(gok_l, by = "messstellen_id")

  mentor <- read_mentor("BY") %>%
    mutate(id_alt = messstellen_id, messstellen_id = to_farm_id(messstellen_id))

  # later MENTOR delivery (Nachlieferung): messstellen_id = LGD number, date, gwl
  nachlieferung <- load_one(file.path(dir_gwst, "neuer_Abgleich_Mentor/nachlieferung_BY_mentor_stamm.RData")) %>%
    as_tibble() %>%
    transmute(id_alt         = as_id(messstellen_id),
              messstellen_id = to_farm_id(id_alt),
              datum          = to_date(date),
              gwl_m_nhn      = num(gwl),
              source         = "mentor_nachlieferung")

  bind_rows(laender,
            bind_rows(mentor, nachlieferung) %>% left_join(gok_m, by = "messstellen_id"))
}


# ---- HE ----------------------------------------------------------------------
read_he <- function() {
  f_he <- file.path(dir_orig, "HE/Berthold_W4_2021-04-16_083539/Berthold_W4/BGR_Anfrage_202103_gesamt.xlsx")

  stamm_l <- read_excel(f_he, sheet = 1) %>% mutate(messstellen_id = as_id(ID))
  stamm_m <- read_excel(file.path(dir_gwst, "info_mentor/Stammdaten_HE.xlsx")) %>%
    transmute(messstellen_id = as_id(ID), gok = GELAENDEHOEHE)

  laender <- read_excel(f_he, sheet = 3) %>%
    transmute(messstellen_id = as_id(MESSSTELLE_ID),
              datum          = to_date(MESSDATUM),
              gwl_m_nhn      = num(MESSWERT),
              source         = "laender") %>%
    left_join(gok_by_priority(
      laender_gelaende  = stamm_l %>% transmute(messstellen_id, gok = GELAENDEHOEHE),
      laender_messpunkt = stamm_l %>% transmute(messstellen_id, gok = MESSPUNKTHOEHE),
      mentor_stamm      = stamm_m), by = "messstellen_id")

  mentor <- read_mentor("HE") %>%
    left_join(gok_by_priority(
      mentor_stamm      = stamm_m,
      laender_gelaende  = stamm_l %>% transmute(messstellen_id, gok = GELAENDEHOEHE)), by = "messstellen_id")

  bind_rows(laender, mentor)
}


# ---- RP ----------------------------------------------------------------------
read_rp <- function() {
  # State data only contain "Wasserstand unter Messpunkt" = depth below the
  # measuring point (no level in m NHN, no ground elevation in the original script)
  laender <- read_excel(file.path(dir_orig, "RP/RP_GW_Messst_PSM_nrM_Werte.xlsx"), sheet = 2) %>%
    filter(STOFF_BEZ == "Wasserstand unter Messpunkt") %>%
    transmute(messstellen_id = as_id(MESSST_NR),
              datum          = to_date(ANA_DATUM),
              depth_m        = num(WERT),
              depth_ref      = "Messpunkt",
              source         = "laender")

  # TODO: no RP master data with ground elevation used so far -> MENTOR RP rows
  # get gwl_m_nhn but no depth. If info_mentor/Stammdaten_RP.xlsx (or similar)
  # exists, add it here with gok_by_priority() and left_join it like in the other states.
  mentor <- read_mentor("RP")

  bind_rows(laender, mentor)
}


# ---- NI ----------------------------------------------------------------------
# "Jan 2010" / "Mär 2010" / "Mrz 2010" / "Okt 2010" ... -> first day of that month
monat_to_date <- function(x) {
  m <- str_to_lower(str_sub(str_trim(x), 1, 3))
  month <- case_when(
    str_starts(m, "ja")             ~ 1L,
    str_starts(m, "f")              ~ 2L,
    str_detect(m, "^ma[iy]")        ~ 5L,
    str_starts(m, "m")              ~ 3L,   # Mär / Mrz / Mar (also with broken umlaut)
    str_starts(m, "ap")             ~ 4L,
    str_starts(m, "jun")            ~ 6L,
    str_starts(m, "jul")            ~ 7L,
    str_starts(m, "au")             ~ 8L,
    str_starts(m, "s")              ~ 9L,
    str_starts(m, "o")              ~ 10L,
    str_starts(m, "n")              ~ 11L,
    str_starts(m, "d")              ~ 12L
  )
  make_date(as.integer(str_extract(x, "\\d{4}")), month, 1L)
}

read_ni <- function() {
  f_ni <- file.path(dir_orig, "NI/WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx",
                    "WSP_Monatsmittelwerte_2010_2020_Gruppe_aus_Taberst_NI_FARM-Stammdaten_xlsx.csv")

  # MONTHLY values 2010-2020: mean / min / max groundwater level per month
  laender <- read_csv2(f_ni, col_types = cols(`ID Messstelle` = col_character(), Monat = col_character())) %>%
    transmute(messstellen_id = as_id(`ID Messstelle`),
              datum          = monat_to_date(Monat),
              gwl_m_nhn      = num(`mittlerer GW-Stand`),
              gwl_min_period = num(`min. GW-Stand`),
              gwl_max_period = num(`max. GW-Stand`),
              depth_m        = num(`GW-Flurabstand`),   # delivered depth (= GOK - mean level)
              gok_m_nhn      = num(GOK_NN),
              gok_source     = "ni_monatswerte",
              n_period       = as.integer(Anzahl),
              aggregation    = "monthly_mean",
              source         = "laender")

  # MENTOR NI: GOK from the same file (assumes MENTOR uses the same well IDs)
  gok <- laender %>% distinct(messstellen_id, gok = gok_m_nhn) %>% gok_by_priority(ni_monatswerte = .)
  mentor <- read_mentor("NI") %>% left_join(gok, by = "messstellen_id")

  bind_rows(laender, mentor)
}


# ---- NW ----------------------------------------------------------------------
read_nw <- function() {
  # LANUV open data (all NRW wells): level, depth, ground and measuring point height
  # are all delivered; decimal point; IDs with leading zeros -> read as text
  raw <- read_delim(file.path(dir_orig, "NRW/wasserstand/opendata.gw_wasserstand.csv"),
                    delim = ";", col_types = cols(.default = col_character()))

  laender <- raw %>%
    transmute(messstellen_id = messstelle_id,
              datum          = to_date(datum_messung, "%Y%m%d"),
              gwl_m_nhn      = num(wasserstd_m),
              depth_m        = num(flurabstd_m),     # delivered depth below ground
              gok_m_nhn      = num(gok_m),
              gok_source     = "nw_opendata",
              note           = na_if(hinweis, "-"),
              source         = "laender")

  # MENTOR NW: GOK from the open data (last known value per well)
  gok <- laender %>%
    filter(!is.na(gok_m_nhn)) %>%
    arrange(messstellen_id, desc(datum)) %>%
    transmute(messstellen_id, gok = gok_m_nhn) %>%
    gok_by_priority(nw_opendata = .)
  mentor <- read_mentor(c("NW", "NRW")) %>% left_join(gok, by = "messstellen_id")

  bind_rows(laender, mentor)
}


# ---- still missing -------------------------------------------------------------
# TODO: add read_sl(), read_st(), read_sn(), read_th()
# following the same pattern and register them in `readers` below.


# ==============================================================================
# Combine
# ==============================================================================
readers <- list(
  MV = read_mv,
  SH = read_sh,
  BB = read_bb,
  BW = read_bw,
  BY = read_by,
  HE = read_he,
  RP = read_rp,
  NI = read_ni,
  NW = read_nw
)

# column template, so every state ends up with the same columns
template <- tibble(state = character(), source = character(),
                   messstellen_id = character(), id_alt = character(),
                   datum = as.Date(character()),
                   gwl_m_nhn = numeric(), gok_m_nhn = numeric(), gok_source = character(),
                   depth_m = numeric(), depth_ref = character(),
                   aggregation = character(), n_period = integer(),
                   gwl_min_period = numeric(), gwl_max_period = numeric(),
                   unit = character(), note = character())

gwl_raw <- imap(readers, function(read_fun, st) {
  message("Reading ", st, " ...")
  tryCatch(read_fun() %>% mutate(state = st),
           error = function(e) { warning("State ", st, " failed: ", conditionMessage(e)); NULL })
}) %>%
  bind_rows(template, .) %>%
  select(all_of(names(template)))

# FARM wells (IDs harmonised like in the combine script)
farm_mst <- load_one(file_farm_mst) %>%
  as_tibble() %>%
  transmute(state          = recode(bundesland, NRW = "NW"),
            messstellen_id = str_replace_all(as_id(messstellen_id), "/", "-"),
            in_farm        = TRUE) %>%
  distinct()

gwl_all <- gwl_raw %>%
  mutate(
    aggregation  = replace_na(aggregation, "single"),
    depth_ref    = replace_na(depth_ref, "GOK"),
    # depth = GOK - GWL; where only a depth below GROUND was delivered, derive GWL
    # (not for depths below the measuring point - its height is not known)
    depth_m      = coalesce(depth_m, gok_m_nhn - gwl_m_nhn),
    gwl_m_nhn    = coalesce(gwl_m_nhn, if_else(depth_ref == "GOK", gok_m_nhn - depth_m, NA_real_)),
    # extremes within the period (only for aggregated data, e.g. NI monthly values)
    depth_min_period = gok_m_nhn - gwl_max_period,
    depth_max_period = gok_m_nhn - gwl_min_period,
    above_ground = depth_m < 0          # artesian / GOK wrong
  ) %>%
  filter(!is.na(messstellen_id), !is.na(datum)) %>%
  distinct() %>%
  left_join(farm_mst, by = c("state", "messstellen_id")) %>%
  mutate(in_farm = replace_na(in_farm, FALSE)) %>%
  arrange(state, messstellen_id, source, datum)

# Wells delivered by the state AND by MENTOR: keep the state data only
# (same rule as in the combine script)
gwl_preferred <- gwl_all %>%
  group_by(state, messstellen_id) %>%
  filter(!(str_starts(source, "mentor") & any(!str_starts(source, "mentor")))) %>%
  ungroup()


# ---- per-well overview ---------------------------------------------------------
# min / max are extremes over the WHOLE record of each well (not annual values).
# For monthly data (NI) the monthly min/max are used, not the monthly means.
# Note: smallest depth = highest groundwater level.
gwl_wells <- gwl_preferred %>%
  group_by(state, messstellen_id, in_farm) %>%
  summarise(
    sources      = str_c(sort(unique(source)), collapse = ", "),
    aggregation  = str_c(sort(unique(aggregation)), collapse = ", "),
    depth_ref    = str_c(sort(unique(depth_ref)), collapse = ", "),
    gok_m_nhn    = first(gok_m_nhn),
    gok_source   = first(gok_source),
    start        = min(datum),
    end          = max(datum),
    n_points     = n(),                                  # rows (months for NI)
    n_measurements = sum(coalesce(n_period, 1L)),         # single readings behind them
    n_years      = n_distinct(year(datum)),
    gwl_mean     = mean(gwl_m_nhn, na.rm = TRUE),
    gwl_min      = min(coalesce(gwl_min_period, gwl_m_nhn), na.rm = TRUE),
    gwl_max      = max(coalesce(gwl_max_period, gwl_m_nhn), na.rm = TRUE),
    depth_mean   = mean(depth_m,   na.rm = TRUE),
    depth_min    = min(coalesce(depth_min_period, depth_m), na.rm = TRUE),
    depth_max    = max(coalesce(depth_max_period, depth_m), na.rm = TRUE),
    depth_sd     = sd(depth_m,     na.rm = TRUE),
    n_above_ground = sum(above_ground, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # min()/max() return +-Inf when a well has only NA values
  mutate(across(c(gwl_min, gwl_max, depth_min, depth_max), ~ if_else(is.infinite(.x), NA_real_, .x)))

# quick check: wells without ground elevation (no depth possible)
gwl_wells %>% filter(is.na(gok_m_nhn)) %>% count(state, name = "wells_without_gok")
gwl_wells %>% count(state, in_farm)


# ---- save ----------------------------------------------------------------------
dir.create(dir_out, showWarnings = FALSE)
save(gwl_all, gwl_preferred, gwl_wells, file = file.path(dir_out, "gwl_timeseries_all_states.RData"))
write_csv2(gwl_preferred, file.path(dir_out, "gwl_timeseries_all_states.csv"))
write_csv2(gwl_wells,     file.path(dir_out, "gwl_wells_all_states.csv"))
