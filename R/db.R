# Database connection ----------------------------------------------------------
#
# Connects to Microsoft SQL Server through ODBC (odbc package). The connection
# details come from environment variables, normally set in the git-ignored
# .Renviron (see README):
#
#   DB_SERVER    host name or IP address (required)
#   DB_PORT      port (optional, SQL Server default 1433)
#   DB_NAME      database name (required)
#   DB_USER      user name (required)
#   DB_DRIVER    ODBC driver (optional, default "ODBC Driver 18 for SQL Server")
#   DB_TRUST_SERVER_CERTIFICATE
#                "yes" to accept a server certificate that is not signed by a
#                trusted authority (optional, default "no")
#
# The password is looked up in this order:
#   1. DB_PASSWORD, if set
#   2. the system keyring (keyring package), service "groundwater_obswells"
#      and the DB_USER user name; store it once with set_db_password()

keyring_service <- "groundwater_obswells"

# Stores the database password in the system keyring (prompts for it).
set_db_password <- function(user = Sys.getenv("DB_USER")) {
  if (!nzchar(user)) {
    stop("Set DB_USER in .Renviron first and restart R.", call. = FALSE)
  }
  keyring::key_set(keyring_service, username = user)
}

db_password <- function() {
  password <- Sys.getenv("DB_PASSWORD")
  if (nzchar(password)) {
    return(password)
  }
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "No database password: install keyring and run set_db_password(), ",
      "or set DB_PASSWORD in .Renviron.",
      call. = FALSE
    )
  }
  tryCatch(
    keyring::key_get(keyring_service, username = Sys.getenv("DB_USER")),
    error = function(e) {
      stop(
        "No database password in the keyring. Run set_db_password() once.",
        call. = FALSE
      )
    }
  )
}

check_db_env <- function() {
  required <- c("DB_SERVER", "DB_NAME", "DB_USER")
  missing <- required[!nzchar(Sys.getenv(required))]
  if (length(missing) > 0) {
    stop(
      "Database connection not configured, missing: ",
      paste(missing, collapse = ", "),
      ". Add them to .Renviron (see README) and restart R.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Wraps an ODBC connection string value in braces, so characters such as
# ; = { } in a password cannot break the string.
odbc_value <- function(x) {
  paste0("{", gsub("}", "}}", x, fixed = TRUE), "}")
}

# Opens a connection to the SQL Server database. Close it with
# db_disconnect().
db_connect <- function() {
  check_db_env()
  server <- Sys.getenv("DB_SERVER")
  port <- Sys.getenv("DB_PORT")
  if (nzchar(port)) {
    server <- paste0(server, ",", port) # SQL Server syntax: host,port
  }
  connection_string <- paste0(
    "Driver=", odbc_value(Sys.getenv("DB_DRIVER", "ODBC Driver 18 for SQL Server")), ";",
    "Server=", odbc_value(server), ";",
    "Database=", odbc_value(Sys.getenv("DB_NAME")), ";",
    "UID=", odbc_value(Sys.getenv("DB_USER")), ";",
    "PWD=", odbc_value(db_password()), ";",
    # Driver 18 encrypts by default; with TrustServerCertificate=yes it
    # accepts a self-signed server certificate.
    "Encrypt=yes;",
    "TrustServerCertificate=", Sys.getenv("DB_TRUST_SERVER_CERTIFICATE", "no"), ";",
    # Read-only intent: the analysis never writes to the database.
    "ApplicationIntent=ReadOnly;"
  )
  DBI::dbConnect(
    odbc::odbc(),
    .connection_string = connection_string,
    # Return 64-bit integers as doubles, which dplyr handles without bit64.
    bigint = "numeric"
  )
}

db_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}

# Runs `sql` in the database and returns the result as a tibble.
db_query <- function(con, sql) {
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

# Schemas to include, from DB_SCHEMAS (comma-separated); NULL means all.
db_schemas <- function() {
  x <- Sys.getenv("DB_SCHEMAS")
  if (!nzchar(x)) {
    return(NULL)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}
