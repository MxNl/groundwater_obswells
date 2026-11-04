# Database connection ----------------------------------------------------------
#
# DuckDB is the query engine: its postgres extension attaches the PostgreSQL
# database read-only, so queries can be pushed down to PostgreSQL and large
# tables read in parallel with the binary COPY protocol. Downstream steps can
# then work with dplyr/duckplyr verbs on DuckDB instead of R data frames.
#
# Connection details come from the libpq environment variables
# (PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD) or a PGSERVICE entry,
# normally set in the git-ignored .Renviron. See .Renviron.example.

check_pg_env <- function() {
  if (nzchar(Sys.getenv("PGSERVICE"))) {
    return(invisible(TRUE))
  }
  required <- c("PGHOST", "PGDATABASE", "PGUSER")
  missing <- required[!nzchar(Sys.getenv(required))]
  if (length(missing) > 0) {
    stop(
      "Database connection not configured, missing: ",
      paste(missing, collapse = ", "),
      ". Copy .Renviron.example to .Renviron, fill it in and restart R.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Opens an in-memory DuckDB connection with the PostgreSQL database attached
# read-only as `pg`. Close it with pg_disconnect().
pg_connect <- function(alias = "pg") {
  check_pg_env()
  con <- DBI::dbConnect(duckdb::duckdb())
  ok <- FALSE
  on.exit(if (!ok) pg_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSTALL postgres")
  DBI::dbExecute(con, "LOAD postgres")
  # An empty connection string makes libpq take everything from the
  # environment, so no credentials pass through R.
  DBI::dbExecute(
    con,
    sprintf("ATTACH '' AS %s (TYPE postgres, READ_ONLY)", alias)
  )
  ok <- TRUE
  con
}

pg_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

# Runs `sql` as-is in PostgreSQL (postgres_query()) and returns the result
# as a tibble. Use this for catalogue queries and anything PostgreSQL
# should compute itself.
pg_query <- function(con, sql, alias = "pg") {
  sql <- gsub("'", "''", sql, fixed = TRUE)
  res <- DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM postgres_query('%s', '%s')", alias, sql)
  )
  tibble::as_tibble(res)
}

# Schemas to include, from MW_SCHEMAS (comma-separated); NULL means all.
pg_schemas <- function() {
  x <- Sys.getenv("MW_SCHEMAS")
  if (!nzchar(x)) {
    return(NULL)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}
