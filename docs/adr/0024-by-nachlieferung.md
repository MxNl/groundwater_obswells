# 24. The BY "Nachlieferung" is read despite not using MENTOR

- Status: accepted (amends ADR 22)
- Date: 2026-11-18
- Code: `R/state_levels.R` (`sl_read_by()`); settings
  `state_levels_config$use_by_nachlieferung` (TRUE)

## Context

Without MENTOR (ADR 22) the BY state series shrank from about one million to
1,373 rows (116 FARM wells): almost all BY levels came from the later MENTOR
delivery (Nachlieferung). gw_base files the BY wells under other IDs than
FARM, so they cannot take their levels from gw_base either.

## Decision

- The BY Nachlieferung is read (`use_by_nachlieferung = TRUE`), keyed by the
  LGD number and translated to the FARM ID (Objektkennzahl) with the BDE
  table, as in the original script; the other MENTOR level data stay off.

## Consequences

- BY FARM wells get their levels from the Nachlieferung.

## Alternatives considered

- Linking BY to gw_base through the BDE table (Objektkennzahl to LGD
  number), if the gw_base BY IDs are the LGD numbers; not checked yet.
