#' Configures the connection to a NeoIPC DHIS2 server
#'
#' @param token The personal access token to use for authentication
#' @param username The username to use for authentication
#' @param session_id The session id to use for authentication
#' @param scheme The url scheme to use. The default is "https"
#' @param hostname The hostname of the DHIS2 server to connect to (e.g.
#'  "dhis2.example.org"). When omitted (`NULL`), it is taken from the
#'  `NEOIPC_DHIS2_HOST` environment variable — which is what lets
#'  [import_dhis2()] run with no arguments. `neoipcr` does not default to any
#'  deployment's host: with neither the argument nor the environment variable
#'  set, this errors.
#' @param port The TCP port to connect to. The default NULL does not set a port
#'  explicitly.
#' @param path The URL path to connect to. The default is "/api"
#'
#' @export
dhis2_connection_options <- function(
    token, username, session_id, scheme = "https",
    hostname = NULL, port = NULL, path = "/api")
{
  if(is.null(hostname) || !nzchar(hostname)) {
    env_host <- Sys.getenv("NEOIPC_DHIS2_HOST", unset = "")
    hostname <- if(nzchar(env_host)) env_host else NULL
  }
  if(is.null(hostname))
    rlang::abort(c(
      "No DHIS2 hostname provided.",
      "i" = "Pass a `hostname` (e.g. dhis2_connection_options(hostname = \"dhis2.example.org\")), or set the NEOIPC_DHIS2_HOST environment variable.",
      "i" = "neoipcr does not default to any deployment's host; the caller supplies it."),
      class = "neoipcr_missing_hostname")

  if(is.character(port)) port <- as.integer(port)

  ret <- list(
    base_url = httr2::url_build(
      structure(list(scheme = scheme, hostname = hostname, port = port, path = path), class = "httr2_url")))

  ret <- switch(
    rlang::check_exclusive(token, username, session_id, .require = FALSE),
    token = c(ret, list(token = read_token(token))),
    username = c(ret, list(username = username, password = get_password(ret$base_url))),
    session_id = c(ret, list(session_id = session_id)),
    c(ret, get_auth_data(ret$base_url))
  )

  structure(ret, class = "neoipcr_dhis2_conopt")
}

#' @export
print.neoipcr_dhis2_conopt <- function(x, ...)
{
  parts <- paste0("Base URL: ", x$base_url)
  if(!is.null(x$token)) {
    parts <- c(parts, "Authentication: Token")
  } else if(!is.null(x$session_id)) {
    parts <- c(parts, "Authentication: Cookie")
  } else if(!is.null(x$username)) {
    parts <- c(
      parts,
      "Authentication: Basic",
      paste0("Username: ", x$username))
  }

  writeLines(parts)
  invisible(x)
}

read_token <- function(token)
{
  if(stringr::str_starts(token, "d2pat_") &&  nchar(token) == 48)
    return(token)

  fileInfo <- file.info(token, extra_cols = FALSE)
  if(!rlang::is_na(fileInfo$isdir) && !fileInfo$isdir)
  {
    fileContent <- readChar(token, fileInfo$size) |>
      stringr::str_replace("\\r?\\n?$", "")
    if(stringr::str_starts(fileContent, "d2pat_") && nchar(fileContent) == 48)
      return(fileContent)
  }
  rlang::abort(gettext("Invalid DHIS2 personal access token."))
}

get_password <- function(url)
{
  pw <- Sys.getenv("NEOIPC_DHIS2_PASSWORD", unset = NA)
  if(!is.na(pw)) return(pw)

  if(!interactive()) rlang::abort(c(
    gettext("No password found"),
    "i" = gettext("NEOIPC_DHIS2_USER is set but NEOIPC_DHIS2_PASSWORD is not."),
    "i" = gettext("Set the NEOIPC_DHIS2_PASSWORD environment variable, or use a personal access token (NEOIPC_DHIS2_TOKEN) instead."),
    "i" = gettext("Interactive password prompting is only available in interactive R sessions.")))

  pw <- askpass::askpass(
    prompt = gettextf("Please enter your password for %s: ", url))

  if(is.null(pw)) rlang::abort(c(
    gettext("No password provided"),
    "i" = gettext("Please provide username and password, a personal access token or a session id to authenticate to DHIS2.")))

  pw
}

get_auth_data <- function(url)
{
  env_session_id <- Sys.getenv("NEOIPC_DHIS2_SESSION_ID", unset = NA)
  if(!is.na(env_session_id) && nzchar(env_session_id))
    return(list(session_id = env_session_id))

  env_token <- Sys.getenv("NEOIPC_DHIS2_TOKEN", unset = NA)
  if(!is.na(env_token) && nzchar(env_token))
    return(list(token = read_token(env_token)))

  env_user <- Sys.getenv("NEOIPC_DHIS2_USER", unset = NA)
  if(!is.na(env_user) && nzchar(env_user))
    return(list(username = env_user, password = get_password(url)))

  if(!interactive()) rlang::abort(c(
    gettext("No authentication credentials found"),
    "i" = gettext("Set the NEOIPC_DHIS2_TOKEN, NEOIPC_DHIS2_SESSION_ID, or NEOIPC_DHIS2_USER environment variable."),
    "i" = gettext("Interactive username/password prompting is only available in interactive R sessions.")))

  user <- readline(
    prompt = gettextf(
      "Please enter your username for %s: ", url))

  if(!nzchar(user)) rlang::abort(c(
    gettext("No username provided"),
    "i" = gettext("Please provide username and password, a personal access token or a session id to authenticate to DHIS2")))

  list(username = user, password = get_password(url))
}
