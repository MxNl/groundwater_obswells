# 14. When the database is read again

- Status: accepted
- Date: 2026-11-16
- Code: `_targets.R` (`db_catalog`, `meta_checksum`, `reference_month`,
  `query_spec`, `threshold_query_settings`), `R/wells.R`
  (`server_query_spec()`, `meta_query_spec()`, `meta_checksum_sql()`)

## Context

The database changes outside the pipeline, and targets does not see it. The
level queries take minutes, so they should rerun only when their result can
change.

## Decision

- On every run: the catalogue (row counts of the tables), a checksum of the
  gw_base metadata columns used, and the reference month (ADR 4).
- The queries depend only on what they use: the level queries on the level
  table's row count, their settings, the reference month and the analysis
  wells with their ground elevations; the metadata query on its checksum;
  the threshold queries on `threshold_query_settings`.

## Consequences

- Changed settings used only locally (spikes, seasons, scenarios, plots)
  rerun only the local steps.
- Corrected levels with the same number of rows are read at the next new
  month at the latest; `tar_invalidate(starts_with("gwl_"))` reads them at
  once.
