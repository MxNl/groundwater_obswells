# 2. Linking wells across sources by state and normalised ID

- Status: accepted; the ID columns per source and state: ADR 17
- Date: 2026-11-17
- Code: `R/link.R` (`norm_id()`, `norm_state()`, `state_from_id()`,
  `state_id_key()`), `R/farm.R` (`fill_farm_meta()`)

## Context

The same well is written differently in the sources: gw_base prefixes the
state (`HE_00001000`), FARM and the state files do not (`1000`, `00001000`),
some IDs contain signs (FARM writes BW IDs as `1/370-5`, gw_base as numbers
with leading zeros). Equal numbers occur in different states.

## Decision

- A well is identified by the key "state + normalised ID".
- The state comes from the file name, a state column (codes or names, also
  within longer names such as "Freie und Hansestadt Hamburg") or a prefix of
  the ID.
- Normalised ID: upper case; a leading state code with a separator (`HE_`,
  `HE-`, `HE `) removed; then all characters other than letters and digits;
  then leading zeros.
- A record without a state is linked by its ID alone where that ID is unique
  in gw_base.

## Consequences

- Removing signs and leading zeros links the BW wells (`1/370-5` becomes
  `13705`, as does `BW_00013705`); keeping them (tried in between) lost
  these links.
- Two different IDs can become equal after the normalisation (e.g. `13/70-5`
  and `1/370-5`); this has not been seen in the data.
- Letters outside A-Z are dropped (`Jäg03` becomes `JG03`); umlauts should
  be written out first. Open.
- Where the sources use different ID systems, the key cannot link them
  (`output/source_check/id_examples.csv`): NI (FARM IDs look pseudonymised),
  MV, BY, ST. Open: another gw_base ID column per state, or linking by
  coordinates.

## Alternatives considered

- Linking by coordinates (nearest well within a few metres): independent of
  the IDs, but needs a tolerance and fails where coordinates are wrong;
  planned as a fallback.
- Exact IDs without normalisation: links almost nothing between FARM and
  gw_base.
