# 16. A temporary check of the metadata against other sources

- Status: accepted (temporary)
- Date: 2026-11-16
- Code: `R/source_check.R`, targets `check_*`; settings `check_settings`
  (column mapping per source, `meer_reference` "states",
  `report_threshold_m` 1)

## Context

Ground elevations and screen depths differ between gw_base, FARM, the "meer"
files (with a DEM elevation) and the original data of the states. Before
relying on one source, the agreement had to be known.

## Decision

- A separate block of targets reads all sources with an explicit column
  mapping, links the wells by state and ID (ADR 2) and compares them:
  ID overlap per pair of sources and state, the meer wells with the values
  of all sources side by side (`check_meer`, `check_meer_elevation`), and
  their differences.
- For the meer wells the state data are the reference, as gw_base does not
  contain all wells.
- The block is independent of the analysis; the shared functions are in
  `R/link.R`, so it can be removed.

## Consequences

- The check led to the FARM base filled from gw_base (ADR 1) and showed the
  ID systems that cannot be linked (ADR 2).
- The meer DEM differs from all other sources by tens of metres; its meaning
  is unclear and it is not used.
