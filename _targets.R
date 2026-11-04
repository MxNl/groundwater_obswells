# Pipeline: run with targets::tar_make(), inspect with targets::tar_read(<name>).

library(targets)

tar_option_set(
  packages = c("DBI", "RPostgres", "dplyr", "tibble")
)

tar_source("R")

list(
  # The database changes outside the pipeline, so its catalogue is read on
  # every run. It is cheap (system catalogue only) and the targets below only
  # rebuild when it actually changed.
  tar_target(
    db_catalog,
    fetch_catalog(),
    cue = tar_cue(mode = "always")
  ),
  tar_target(db_tables, db_catalog$tables),
  tar_target(db_columns, db_catalog$columns),
  tar_target(db_table_overview, summarise_tables(db_tables, db_columns))
)
