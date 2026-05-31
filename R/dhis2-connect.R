#' Configures the connection to a NeoIPC DHIS2 server
#'
#' @param token The personal access token to use for authentication
#' @param username The username to use for authentication
#' @param session_id The session id to use for authentication
#' @param scheme The url scheme to use. The default is "https"
#' @param hostname The hostname to connect to. The default is
#'  "neoipc.charite.de".
#' @param port The TCP port to connect to. The default NULL does not set a port
#'  explicitly.
#' @param path The URL path to connect to. The default is "/api"
#'
#' @export
dhis2_connection_options <- function(
    token, username, session_id, scheme = "https",
    hostname = "neoipc.charite.de", port = NULL, path = "/api")
{
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

  pw <- askpass::askpass(
    prompt = gettextf("Please enter your password for %s: ", url))

  if(is.null(pw)) rlang::abort(
    message = gettext("No password provided"),
    body = gettext("Please provide username and password, a personal access token or a session id to authenticate to DHIS2"))

  pw
}

get_auth_data <- function(url)
{
  env_session_id <- Sys.getenv("NEOIPC_DHIS2_SESSION_ID", unset = NA)
  if(!is.na(env_session_id)) return(list(session_id = env_session_id))

  env_token <- Sys.getenv("NEOIPC_DHIS2_TOKEN", unset = NA)
  if(!is.na(env_token)) return(list(token = read_token(env_token)))

  user <- Sys.getenv("NEOIPC_DHIS2_USER", unset = NA)
  if(is.na(user)) user <- askpass::askpass(
    prompt = gettextf(
      "Please enter your username for %s: ", url))

  if(is.null(user)) rlang::abort(
    message = gettext("No username provided"),
    body = gettext("Please provide username and password, a personal access token or a session id to authenticate to DHIS2"))

  list(username = user, password = get_password(url))
}
