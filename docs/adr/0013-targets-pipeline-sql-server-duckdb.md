# 13. A targets pipeline on SQL Server via ODBC, DuckDB locally

- Status: accepted
- Date: 2026-11-04
- Code: `_targets.R`, `R/db.R`, `R/catalog.R`; credentials in `.Renviron`
  (git-ignored), password in the system keyring (`set_db_password()`)

## Context

The data are in a Microsoft SQL Server database (first assumed to be
PostgreSQL). The analysis has many steps whose inputs change at different
times.

## Decision

- targets package: steps are functions in `R/`, `_targets.R` defines the
  dependencies, `tar_make()` rebuilds what is outdated.
- Connection through ODBC (ODBC Driver 18) with a connection pool;
  connections are checked out explicitly and always returned.
- Heavy aggregation in SQL on the server (ADR 3); window statistics in an
  in-memory DuckDB (`gwl_window_stats_sql()`), results as tibbles.
- Connection details never in the repository: `.Renviron` and the keyring.

## Consequences

- Results are reproducible and up to date; `tar_visnetwork()` documents the
  method.
- The pipeline reruns database queries only when needed (ADR 14).

## Alternatives considered

- PostgreSQL with RPostgres: the database turned out to be SQL Server.
- Reading the tables into DuckDB first: one copy of the data to keep up to
  date; aggregating on the server reads only what is needed.
