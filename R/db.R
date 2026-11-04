# Database connection ----------------------------------------------------------
#
# Connects to PostgreSQL with RPostgres. Connection details come from the libpq
# environment variables (PGHOST, PGPORT, PGDATABASE, PGUSER) or a PGSERVICE
# entry, normally set in the git-ignored .Renviron. See .Renviron.example.
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

# The keyring password for PGUSER, or NULL if PGPASSWORD is set, keyring is
# not installed or no password is stored.
keyring_password <- function() {
  user <- Sys.getenv("PGUSER")
  if (nzchar(Sys.getenv("PGPASSWORD")) || !nzchar(user) ||
      !requireNamespace("keyring", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(
    keyring::key_get(keyring_service, username = user),
    error = function(e) NULL
  )
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

# Opens a connection to the PostgreSQL database. Close it with
# pg_disconnect(). Everything not given here (host, port, database, user,
# SSL mode) libpq takes from the environment.
pg_connect <- function() {
  check_pg_env()
  args <- list(
    RPostgres::Postgres(),
    # Return 64-bit integers as doubles, which dplyr handles without bit64.
    bigint = "numeric"
  )
  password <- keyring_password()
  if (!is.null(password)) {
    args$password <- password
  }
  con <- do.call(DBI::dbConnect, args)
  # The analysis only reads: make the session read-only as a safeguard.
  DBI::dbExecute(con, "SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY")
  con
}

pg_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}

# Runs `sql` in PostgreSQL and returns the result as a tibble.
pg_query <- function(con, sql) {
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Schemas to include, from MW_SCHEMAS (comma-separated); NULL means all.
pg_schemas <- function() {
  x <- Sys.getenv("MW_SCHEMAS")
  if (!nzchar(x)) {
    return(NULL)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}
