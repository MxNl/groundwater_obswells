# 22. State series without MENTOR levels, only for FARM wells

- Status: accepted (amends ADR 21)
- Date: 2026-11-18
- Code: `R/state_levels.R` (`build_state_levels()`, `sl_read_text()`,
  `sl_read_bb_file()`, `sl_read_excel()`); settings
  `state_levels_config$use_mentor` (FALSE), target `state_levels_keep`

## Context

The first run on the real deliveries showed: the MENTOR level file was not
at the expected place, and all MENTOR data are also in gw_base, which is the
second level source anyway (ADR 18). NW delivers all wells of the state
(about 20 million rows). Two readers failed on the format of single files
(SH text file not UTF-8; BB files with other separators), and the MV and BY
Excel exports have about as many rows as an Excel sheet can hold.

## Decision

- MENTOR level data are not read (`use_mentor = FALSE`); the BY
  "Nachlieferung" (MENTOR) neither. The MENTOR master data tables (ground
  elevations, ID translations) are still read.
- Only rows of wells that can be linked to a FARM well are kept (by
  `messstellen_id` or `id_alt`); the keys come from the FARM table and the
  mapping, so changes in gw_base do not reread the deliveries.
- Text files are read as UTF-8, and as Windows-1252 if that is not valid;
  the BB files one by one with their separator detected.
- An Excel sheet at the row limit is noted in the log as probably cut off.

## Consequences

- The state series and the targets store stay small; the log shows rows read
  and kept per state.
- Cut-off Excel exports (MV, BY) have to be replaced by complete exports at
  the source; until then the series of those states may end early.
