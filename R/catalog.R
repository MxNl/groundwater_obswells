# Database catalogue -------------------------------------------------------------
#
# Lists the tables, views and columns the user can read. Everything comes from
# the PostgreSQL system catalogue, so no table is scanned: row counts are the
# planner's estimates (pg_class.reltuples), NA for tables never analysed.

sql_tables <- "
SELECT n.nspname::text AS table_schema,
       c.relname::text AS table_name,
       CASE c.relkind
         WHEN 'r' THEN 'table'
         WHEN 'p' THEN 'partitioned table'
         WHEN 'v' THEN 'view'
         WHEN 'm' THEN 'materialized view'
         WHEN 'f' THEN 'foreign table'
       END AS table_type,
       c.reltuples::bigint AS n_rows_estimate,
       pg_total_relation_size(c.oid) AS size_bytes,
       obj_description(c.oid, 'pg_class') AS table_comment
FROM pg_class c
JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f')
  AND NOT c.relispartition
  AND n.nspname NOT IN ('pg_catalog', 'information_schema')
  AND n.nspname NOT LIKE 'pg\\_toast%'
  AND has_schema_privilege(n.oid, 'USAGE')
  AND has_table_privilege(c.oid, 'SELECT')
ORDER BY 1, 2
"

sql_columns <- "
SELECT n.nspname::text AS table_schema,
       c.relname::text AS table_name,
       a.attnum::integer AS column_position,
       a.attname::text AS column_name,
       format_type(a.atttypid, a.atttypmod) AS data_type,
       a.attnotnull AS not_null,
       col_description(c.oid, a.attnum) AS column_comment
FROM pg_attribute a
JOIN pg_class c ON c.oid = a.attrelid
JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f')
  AND NOT c.relispartition
  AND a.attnum > 0
  AND NOT a.attisdropped
  AND n.nspname NOT IN ('pg_catalog', 'information_schema')
  AND n.nspname NOT LIKE 'pg\\_toast%'
  AND has_schema_privilege(n.oid, 'USAGE')
  AND has_table_privilege(c.oid, 'SELECT')
ORDER BY 1, 2, 3
"

# Reads tables and columns over one connection. Returns a list of two tibbles.
fetch_catalog <- function(schemas = pg_schemas()) {
  con <- pg_connect()
  on.exit(pg_disconnect(con), add = TRUE)

  tables <- pg_query(con, sql_tables)
  columns <- pg_query(con, sql_columns)

  if (!is.null(schemas)) {
    tables <- dplyr::filter(tables, table_schema %in% schemas)
    columns <- dplyr::filter(columns, table_schema %in% schemas)
  }
  # reltuples is -1 for tables never vacuumed or analysed, and views have
  # no rows of their own.
  tables <- dplyr::mutate(
    tables,
    n_rows_estimate = dplyr::if_else(
      n_rows_estimate < 0 | table_type == "view", NA, n_rows_estimate
    )
  )

  list(tables = tables, columns = columns)
}

# One row per table: its size, row estimate and column names.
summarise_tables <- function(tables, columns) {
  cols <- columns |>
    dplyr::summarise(
      n_columns = dplyr::n(),
      columns = paste(column_name, collapse = ", "),
      .by = c(table_schema, table_name)
    )
  tables |>
    dplyr::left_join(cols, by = c("table_schema", "table_name")) |>
    dplyr::mutate(size_mb = round(size_bytes / 1024^2, 1)) |>
    dplyr::select(
      table_schema, table_name, table_type, n_rows_estimate,
      size_mb, n_columns, columns, table_comment
    ) |>
    dplyr::arrange(table_schema, table_name)
}
