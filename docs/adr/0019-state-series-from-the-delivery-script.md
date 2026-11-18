# 19. The state level series come from a separate script

- Status: accepted
- Date: 2026-11-18
- Code: `scripts/gw_timeseries_all_states.R`, `R/levels.R`
  (`state_levels_file_info()`, `read_state_levels()`); settings
  `level_config$state_levels_file`

## Context

The state deliveries (and MENTOR) come as many files per state, each with its
own format, ID system and ground elevation sources, on the project drives. A
script collected them into one long table (`gwl_preferred`: one row per well
and date, level, ground elevation, depth and its reference; state data
preferred over MENTOR per well).

## Decision

- The script stays separate, under `scripts/`; it is run by hand when
  deliveries change and writes its .RData output.
- The pipeline reads the object `gwl_preferred` from that file. Its time and
  size are checked on every run, so a new file reruns the steps after it.
  Without the file only gw_base levels are used, with a warning.
- The pipeline uses the state ID, date, level and, where no level was
  delivered, the depth with its reference; it does not use the script's
  ground elevations or summaries.

## Consequences

- The pipeline does not depend on the drive layout of the deliveries, and
  the script can be maintained on its own (SL, ST, SN and TH are not in it
  yet).
- The script and the pipeline have to be run one after the other.

## Alternatives considered

- Reading the deliveries in the pipeline: each file tracked and rebuilt when
  it changes, but tied to the paths of the project drives and much slower to
  set up.
