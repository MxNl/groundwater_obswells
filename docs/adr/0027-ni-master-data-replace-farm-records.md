# 27. NI: the FARM records replaced by the NI master data

- Status: accepted (amends ADR 1, 17)
- Date: 2026-11-24
- Code: `R/farm.R` (`replace_farm_states()`); `_targets.R`
  (`farm_config$state_files`, targets `farm_state_files`, `farm_meta`);
  `config/id_mapping.csv` (NI row removed)

## Context

The FARM table has only hashed IDs and no coordinates for NI, so the NI
wells could be linked neither to gw_base nor to the NI level delivery, and
could not be mapped (ADR 17: "none"). An NI master data file with the real
IDs (`MS_NR`), coordinates, ground elevation and screens has been found.
Matching the hashed records to it through the other metadata columns would
be tedious and uncertain.

## Decision

- The NI records of the FARM table are dropped and replaced by the records
  of `NI_Stammdaten.xlsx`, one analysis well per `MS_NR`.
- Columns: `MS_NR` (ID), `MS_GOKNN` (ground elevation), `MS_MBPNN`
  (measuring point, for the stick-up estimate), `MS_FOK`/`MS_FUK` (screen
  top/bottom), `MS_UTMRECHTS`/`MS_UTMHOCH` (coordinates; EPSG 258xx from
  `MS_UTMZONE`).
- Screen values: read as depths below ground or as elevations (m NN,
  converted with the ground elevation), detected from the order of top and
  bottom (`screen_reference = "auto"`) and reported; can be fixed in the
  setting.
- The ID goes into every FARM ID column, so the default mapping row ("*")
  links NI to gw_base and to the NI level series (IDs like `1110050`).
- Duplicate IDs in the file: the first record is used, with a warning.
- The mechanism is general (`farm_config$state_files`, per state file and
  columns), for further states with better separate master data.

## Consequences

- NI wells can get levels (state series 2010-2020, gw_base) and appear on
  the maps. The NI series end in 2020, so many NI wells may still drop out
  at "no data in the last 10 years" unless gw_base has newer levels.
- The NI well set is now that of the NI file. If the file holds more wells
  than FARM's NI selection, the analysis grows; the replacement message
  gives both counts. Decommissioned wells (`MS_STILLDATUM`) are not removed.
- The hashed NI IDs are no longer used anywhere.
