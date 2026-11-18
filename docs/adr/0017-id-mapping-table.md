# 17. ID columns per source and state from a mapping table

- Status: accepted (extends ADR 2)
- Date: 2026-11-18
- Code: `config/id_mapping.csv`, `R/farm.R` (`read_id_mapping()`,
  `id_mapping_for()`, `farm_link_keys()`, `fill_farm_meta()`),
  `R/levels.R` (`prepare_state_levels()`), `R/wells.R` (`meta_query_spec()`)

## Context

The linking by state and ID (ADR 2) used one FARM ID column for all states.
The checks of unmatched IDs showed that the states differ: for MV, the FARM
ID that matches gw_base is in `messstellen_id2`; NI has only hashed IDs in
FARM and no coordinates, so it cannot be linked to gw_base at all. The state
level series (ADR 18, 19) are a second source with its own IDs.

## Decision

- A tracked table `config/id_mapping.csv` gives per source (`gw_base`,
  `state_levels`) and state the FARM column with the ID (`farm_column`), the
  column of the other source (`other_column`) and whether the wells can be
  linked (`link`: `id` or `none`), with a note. A row with state `*` holds
  for all states without a row of their own.
- Current rows: gw_base `*` messstellen_id = well_id; gw_base MV
  messstellen_id2 = well_id; gw_base NI none; state_levels `*`
  messstellen_id = messstellen_id.
- The normalisation of ADR 2 applies to every column. The gw_base columns
  named in the table are read with the metadata.
- `well_meta$gwbase_link` records how a well was linked, or why not.

## Consequences

- MV wells are linked to gw_base; NI wells are not, and get levels only from
  the state series.
- New findings about IDs are entered in the table, not in the code.

## Alternatives considered

- Linking by coordinates: not possible for NI (no coordinates); used only for
  levels, as a third option (ADR 18).
