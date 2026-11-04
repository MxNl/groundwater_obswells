# Database catalogue -------------------------------------------------------------
#
# Lists the tables, views and columns the user can see. Everything comes from
# SQL Server's catalogue views (sys.*), so no table is scanned: row counts come
# from sys.partitions and sizes from sys.allocation_units. Comments are the
# "MS_Description" extended properties.

sql_tables <- "
SELECT s.name AS table_schema,
       o.name AS table_name,
       CASE o.type WHEN 'U' THEN 'table' WHEN 'V' THEN 'view' END AS table_type,
       CAST(r.n_rows AS float) AS n_rows,
       CAST(z.size_bytes AS float) AS size_bytes,
       CAST(ep.value AS nvarchar(4000)) AS table_comment
FROM sys.objects AS o
JOIN sys.schemas AS s ON s.schema_id = o.schema_id
-- rows of the heap (index 0) or clustered index (1), summed over partitions
LEFT JOIN (
  SELECT object_id, SUM(rows) AS n_rows
  FROM sys.partitions
  WHERE index_id IN (0, 1)
  GROUP BY object_id
) AS r ON r.object_id = o.object_id
-- reserved pages of all indexes, 8 KB each
LEFT JOIN (
  SELECT p.object_id, SUM(CAST(a.total_pages AS bigint)) * 8192 AS size_bytes
  FROM sys.partitions AS p
  JOIN sys.allocation_units AS a ON a.container_id = p.partition_id
  GROUP BY p.object_id
) AS z ON z.object_id = o.object_id
LEFT JOIN sys.extended_properties AS ep
  ON ep.class = 1 AND ep.major_id = o.object_id AND ep.minor_id = 0
 AND ep.name = 'MS_Description'
WHERE o.type IN ('U', 'V')
  AND o.is_ms_shipped = 0
ORDER BY 1, 2
"

sql_columns <- "
SELECT s.name AS table_schema,
       o.name AS table_name,
       c.column_id AS column_position,
       c.name AS column_name,
       t.name +
         CASE
           WHEN t.name IN ('varchar', 'char', 'varbinary', 'binary')
             THEN '(' + CASE WHEN c.max_length = -1 THEN 'max'
                        ELSE CAST(c.max_length AS varchar(10)) END + ')'
           WHEN t.name IN ('nvarchar', 'nchar')
             THEN '(' + CASE WHEN c.max_length = -1 THEN 'max'
                        ELSE CAST(c.max_length / 2 AS varchar(10)) END + ')'
           WHEN t.name IN ('decimal', 'numeric')
             THEN '(' + CAST(c.precision AS varchar(10)) + ',' +
                  CAST(c.scale AS varchar(10)) + ')'
           ELSE ''
         END AS data_type,
       CAST(CASE WHEN c.is_nullable = 0 THEN 1 ELSE 0 END AS bit) AS not_null,
       CAST(ep.value AS nvarchar(4000)) AS column_comment
FROM sys.columns AS c
JOIN sys.objects AS o ON o.object_id = c.object_id
JOIN sys.schemas AS s ON s.schema_id = o.schema_id
JOIN sys.types AS t ON t.user_type_id = c.user_type_id
LEFT JOIN sys.extended_properties AS ep
  ON ep.class = 1 AND ep.major_id = c.object_id AND ep.minor_id = c.column_id
 AND ep.name = 'MS_Description'
WHERE o.type IN ('U', 'V')
  AND o.is_ms_shipped = 0
ORDER BY 1, 2, 3
"

# Reads tables and columns. Returns a list of two tibbles.
fetch_catalog <- function(schemas = db_schemas()) {
  tables <- db_query(sql_tables)
  columns <- db_query(sql_columns)

  if (!is.null(schemas)) {
    tables <- dplyr::filter(tables, table_schema %in% schemas)
    columns <- dplyr::filter(columns, table_schema %in% schemas)
  }

  list(tables = tables, columns = columns)
}

# One row per table: its size, row count and column names.
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
      table_schema, table_name, table_type, n_rows,
      size_mb, n_columns, columns, table_comment
    ) |>
    dplyr::arrange(table_schema, table_name)
}
