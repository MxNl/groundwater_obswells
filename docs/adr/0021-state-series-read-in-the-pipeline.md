# 21. The state level series are read in the pipeline

- Status: accepted (supersedes ADR 19)
- Date: 2026-11-18
- Code: `R/state_levels.R` (`build_state_levels()`, `state_level_readers`,
  `state_level_input_info()`, `summarise_state_level_wells()`),
  `R/levels.R` (`prepare_state_levels()`); settings `state_levels_config`
  (`dir_orig`, `dir_gwst`, `dir_dbstamm`, `states`)

## Context

ADR 19 kept the script that collects the state deliveries outside the
pipeline and read its output file. The script had not been run, the file did
not exist, and the pipeline fell back to gw_base levels. The series should
be built where they are used, without a manual step.

## Decision

- The logic of the script (one reader per state, ground elevation from
  master data tables in a fixed priority, state data before MENTOR per well)
  becomes functions of the pipeline; the script is removed.
- The delivery folders and the states read are settings
  (`state_levels_config`). Size and time of every input file are checked on
  every run (`state_levels_inputs`), so changed or added deliveries rebuild
  the series, and only then.
- A state whose files cannot be read is left out with a warning; the other
  states are used. Without the MENTOR file, only state deliveries are used.
- The script's per-well overview is written as
  `output/state_levels_wells.csv`; its file outputs are not.

## Consequences

- One `tar_make()` builds everything; the pipeline needs read access to the
  project drives and the packages readxl, readr and foreign.
- The first run reads all deliveries (slow); later runs only check the files.
- SL, ST, SN and TH still have no reader.

## Alternatives considered

- Each delivery file as a file target: tracks the files too, but the readers
  share files across states and folders (BB), which a list of file
  information covers more simply.
