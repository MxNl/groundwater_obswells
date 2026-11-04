# Database connection ----------------------------------------------------------
#
# DuckDB is the query engine: its postgres extension attaches the PostgreSQL
# database read-only, so queries can be pushed down to PostgreSQL and large
# tables read in parallel with the binary COPY protocol. Downstream steps can
# then work with dplyr/duckplyr verbs on DuckDB instead of R data frames.
#
# Connection details come from the libpq environment variables
# (PGHOST, PGPORT, PGDATABASE, PGUSER) or a PGSERVICE entry, normally set in
# the git-ignored .Renviron. See .Renviron.example.
#
# The password is looked up in this order:
#   1. PGPASSWORD, if set
#   2. the system keyring (keyring package), service "groundwater_obswells"
#      and the PGUSER user name; store it once with set_pg_password()
#   3. libpq's password file (~/.pgpass, on Windows
#      %APPDATA%\postgresql\pgpass.conf)

keyring_service <- "groundwater_obswells"

# Stores the database password in the system keyring (prompts for it).
set_pg_password <- function(user = Sys.getenv("PGUSER")) {
  if (!nzchar(user)) {
    stop("Set PGUSER in .Renviron first and restart R.", call. = FALSE)
  }
  keyring::key_set(keyring_service, username = user)
}

# Puts the keyring password into PGPASSWORD for this R process, unless a
# password is set already. libpq reads it whenever DuckDB opens a connection
# (also the extra ones for parallel scans), so it stays set until
# pg_disconnect(). Returns the previous PGPASSWORD value, or NA if unset.
use_keyring_password <- function() {
  old <- Sys.getenv("PGPASSWORD", unset = NA)
  user <- Sys.getenv("PGUSER")
  if (!is.na(old) && nzchar(old)) {
    return(old)
  }
  if (!nzchar(user) || !requireNamespace("keyring", quietly = TRUE)) {
    return(old)
  }
  password <- tryCatch(
    keyring::key_get(keyring_service, username = user),
    error = function(e) NULL
  )
  if (!is.null(password)) {
    Sys.setenv(PGPASSWORD = password)
  }
  old
}

restore_pgpassword <- function(old) {
  if (is.na(old)) {
    Sys.unsetenv("PGPASSWORD")
  } else {
    Sys.setenv(PGPASSWORD = old)
  }
}

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
  old_password <- use_keyring_password()
  con <- DBI::dbConnect(duckdb::duckdb())
  attr(con, "old_pgpassword") <- old_password
  ok <- FALSE
  on.exit(if (!ok) pg_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSTALL postgres")
  DBI::dbExecute(con, "LOAD postgres")
  # An empty connection string makes libpq take everything from the
  # environment, so no credentials end up in SQL or error messages.
  DBI::dbExecute(
    con,
    sprintf("ATTACH '' AS %s (TYPE postgres, READ_ONLY)", alias)
  )
  ok <- TRUE
  con
}

pg_disconnect <- function(con) {
  old_password <- attr(con, "old_pgpassword")
  DBI::dbDisconnect(con, shutdown = TRUE)
  if (!is.null(old_password)) {
    restore_pgpassword(old_password)
  }
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
