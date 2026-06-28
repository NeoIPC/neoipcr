#' Configure neoipcr's log verbosity
#'
#' Sets the log threshold for neoipcr's `"neoipcr"` logger namespace from a
#' verbosity level, controlling how much of the DHIS2 query trace and import
#' progress neoipcr emits. neoipcr logs through the \pkg{logger} package; this
#' is a thin convenience over [logger::log_threshold()] using neoipcr's
#' verbosity vocabulary.
#'
#' @param verbosity One of `"quiet"`, `"normal"`, `"verbose"`, `"debug"`
#'   (mapped to logger thresholds `WARN`, `INFO`, `DEBUG`, `TRACE`). When
#'   `NULL` (the default) the level is read from the `NEOIPC_LOG_LEVEL`
#'   environment variable, falling back to `"normal"`. neoipcr's per-request
#'   query trace is emitted at `DEBUG`, so `"verbose"` or `"debug"` reveals it.
#'
#' @details
#' neoipcr is silent by default: it logs only at `DEBUG`/`TRACE`, and the
#' global logger threshold defaults to `INFO`. The effective level is resolved
#' in this order of precedence: an explicit `neoipcr_log_config()` call, then
#' the `NEOIPC_LOG_LEVEL` environment variable (also read in `.onLoad`, so a
#' bare [import_dhis2()] honours it), then the inherited global threshold.
#'
#' @returns The applied logger threshold, invisibly.
#' @examples
#' neoipcr_log_config("debug")  # reveal the DHIS2 query trace
#' @export
neoipcr_log_config <- function(verbosity = NULL)
{
  if (is.null(verbosity))
    verbosity <- Sys.getenv("NEOIPC_LOG_LEVEL", unset = "normal")
  threshold <- neoipcr_log_threshold(verbosity)
  logger::log_threshold(threshold, namespace = "neoipcr")
  invisible(threshold)
}

# Map a neoipcr verbosity level to a logger threshold. Unknown values fall
# back to INFO — the silent-by-default level for neoipcr's DEBUG/TRACE logging.
neoipcr_log_threshold <- function(verbosity)
{
  switch(
    tolower(verbosity),
    quiet   = logger::WARN,
    normal  = logger::INFO,
    verbose = logger::DEBUG,
    debug   = logger::TRACE,
    logger::INFO)
}

# Log a single DHIS2 request as URL + HTTP status + row count ONLY.
#
# Data-protection boundary (GDPR): DHIS2 responses carry surveillance data, so
# this helper must never log a response body. It records only the request URL
# (no credentials — auth travels in a redacted header or a cookie, never the
# URL), the HTTP status, and an optional row count of the parsed result.
#
# `x` is either an httr2 response or — at the parallel-perform sites that use
# `on_error = "continue"` — an httr2 error object carrying the failed `$resp`.
# Both are accepted so a failed sibling request is still traced.
log_dhis2_request <- function(x, endpoint, n_rows = NULL)
{
  resp <- if (rlang::is_error(x)) x$resp else x
  url <- tryCatch(httr2::resp_url(resp), error = function(e) NA_character_)
  status <- tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_)
  rows <- if (is.null(n_rows)) "" else paste0(" rows=", n_rows)
  logger::log_debug(
    "DHIS2 {endpoint}: status={status} {url}{rows}",
    endpoint = endpoint, status = status, url = url, rows = rows,
    namespace = "neoipcr")
}

# Package load hook: configure neoipcr's logger namespace. Per logger's package
# guidance the package sets only its formatter (so its glue-style messages
# interpolate regardless of the application's global formatter) and leaves the
# appender/layout to the application. It sets its own namespace threshold only
# when the pipeline's NEOIPC_LOG_LEVEL is present, so that env var governs
# neoipcr automatically (even a bare import_dhis2()); when it is absent the
# namespace inherits the global threshold (INFO) — silent by default.
.onLoad <- function(libname, pkgname)
{
  logger::log_formatter(logger::formatter_glue, namespace = "neoipcr")
  env_level <- Sys.getenv("NEOIPC_LOG_LEVEL", unset = "")
  if (nzchar(env_level))
    logger::log_threshold(
      neoipcr_log_threshold(env_level), namespace = "neoipcr")
}
