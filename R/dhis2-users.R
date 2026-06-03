get_user_info <- function(req)
{
  # Two-level tryCatch: inner level translates specific HTTP errors into

  # user-friendly messages; outer level catches everything else (DNS failure,
  # timeout, etc.) and wraps with a generic connection message.  The outer

  # handler passes through errors already translated by the inner level.
  resp <- tryCatch(
    tryCatch(
      req |>
        httr2::req_url_path_append("me") |>
        httr2::req_url_query(
          fields = "id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[name,authorities],userGroups[name]") |>
        httr2::req_perform(),
      httr2_http_401 = function(cnd) {
        rlang::abort(c(
          sprintf("DHIS2 authentication failed (HTTP 401) at %s.", req$url),
          i = "Check that your token or username/password is correct.",
          i = "Token auth: set the NEOIPC_DHIS2_TOKEN environment variable.",
          i = "Basic auth: set NEOIPC_DHIS2_USER and NEOIPC_DHIS2_PASSWORD environment variables."
        ), class = "neoipcr_dhis2_error", call = NULL)
      },
      httr2_http_403 = function(cnd) {
        rlang::abort(c(
          sprintf("DHIS2 access denied (HTTP 403) at %s.", req$url),
          i = "Your credentials were accepted but you lack permission to access /api/me.",
          i = "Contact a DHIS2 administrator to check your user role."
        ), class = "neoipcr_dhis2_error", call = NULL)
      }
    ),
    error = function(cnd) {
      if (inherits(cnd, "neoipcr_dhis2_error")) stop(cnd)
      rlang::abort(c(
        sprintf("Failed to connect to DHIS2 at %s.", req$url),
        i = "Check your network connection and DHIS2 server URL.",
        i = conditionMessage(cnd)
      ), class = "neoipcr_dhis2_error", call = NULL)
    }
  )

  raw_info <- tryCatch(
    resp |>
      httr2::resp_check_status() |>
      httr2::resp_body_json(simplifyVector = TRUE),
    error = function(cnd) {
      ct <- httr2::resp_content_type(resp)
      sc <- httr2::resp_status(resp)
      url <- resp$url
      if (grepl("text/html", ct, fixed = TRUE)) {
        rlang::abort(c(
          sprintf("DHIS2 returned an HTML page instead of JSON (HTTP %d, URL: %s).", sc, url),
          i = "This usually means the server redirected to a login page.",
          i = "Your credentials may be missing, expired, or incorrect.",
          i = "Token auth: set the NEOIPC_DHIS2_TOKEN environment variable.",
          i = "Basic auth: set NEOIPC_DHIS2_USER and NEOIPC_DHIS2_PASSWORD environment variables."
        ), call = NULL)
      }
      rlang::abort(c(
        sprintf("Unexpected DHIS2 response content type: %s", ct),
        i = conditionMessage(cnd)
      ), parent = cnd)
    }
  )

  structure(list(
    id = raw_info$id,
    username = raw_info$username,
    firstName = raw_info$firstName,
    surname = raw_info$surname,
    email = raw_info$email,
    lastLogin = readr::parse_datetime(raw_info$userCredentials$lastLogin),
    created = readr::parse_datetime(raw_info$created),
    organisationUnits = raw_info$organisationUnits$id,
    dataViewOrganisationUnits = raw_info$dataViewOrganisationUnits$id,
    teiSearchOrganisationUnits = raw_info$teiSearchOrganisationUnits$id,
    groups = raw_info$userGroups$name |>
      sort(),
    roles = raw_info$userRoles$name |>
      sort(),
    authorities = raw_info$userRoles$authorities |>
      unlist() |>
      unique() |>
      sort()
  ), class = c("neoipc_dhis2_usrinfo", "list"))
}

read_user_info_table <- function(user_info, include_user)
{
  if(include_user == "no")
    return(NULL)

  if(include_user == "yes")
    return(
      user_info |>
        list() |>
        tibble::tibble() |>
        tidyr::unnest_wider(1) |>
        dplyr::select(!c(
          "organisationUnits",
          "dataViewOrganisationUnits",
          "teiSearchOrganisationUnits",
          "groups",
          "roles",
          "authorities")) |>
        add_key_column("user_key"))

  user_info <- tibble::tibble(
    user_key = 1L,
    user = user_info$id,
    username = user_info$username)
}

read_metadata_users <- function(metadata, include_user)
{
  if(include_user == "no")
    return(invisible(NULL))

  users <- metadata |>
    purrr::pluck("users")

  if(rlang::is_null(users))
    return(invisible(NULL))

  users <- users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  if(include_user == "yes")
    users <- users |>
    dplyr::select(
      !c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits",
        "userRoles")) |>
    dplyr::mutate(
      created = readr::parse_datetime(.data$created),
      lastLogin = readr::parse_datetime(.data$lastLogin)) |>
    dplyr::relocate("user" = "id","username","firstName","surname","email",
                    "lastLogin","created")
  else
    users <- users |>
    dplyr::rename("user" = "id")

  users |>
    add_key_column("user_key")
}
