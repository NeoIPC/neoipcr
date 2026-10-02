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
        # `lastLogin` lives under the `userCredentials` back-compat shim on the
        # /me response for 2.40 and 2.41. Newer lines (2.42+) drop
        # `userCredentials` from /me and expose `lastLogin` nowhere, so it reads
        # as NA there (below). Requesting a field the server does not have is
        # silently omitted by the field filter, not rejected.
        httr2::req_url_query(
          fields = "id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[name,authorities],userGroups[name]") |>
        httr2::req_perform(),
      httr2_http_401 = function(cnd) {
        rlang::abort(c(
          gettextf("DHIS2 authentication failed (HTTP 401) at %s.", req$url),
          i = gettext("Check that your token or username/password is correct."),
          i = gettextf("Token auth: set the %s environment variable.", "NEOIPC_DHIS2_TOKEN"),
          i = gettextf(
            "Basic auth: set %s and %s environment variables.",
            "NEOIPC_DHIS2_USER", "NEOIPC_DHIS2_PASSWORD")
        ), class = "neoipcr_dhis2_error", call = NULL)
      },
      httr2_http_403 = function(cnd) {
        rlang::abort(c(
          gettextf("DHIS2 access denied (HTTP 403) at %s.", req$url),
          i = gettextf(
            "Your credentials were accepted but you lack permission to access %s.",
            "/api/me"),
          i = gettext("Contact a DHIS2 administrator to check your user role.")
        ), class = "neoipcr_dhis2_error", call = NULL)
      }
    ),
    error = function(cnd) {
      if (inherits(cnd, "neoipcr_dhis2_error")) rlang::cnd_signal(cnd)
      rlang::abort(c(
        gettextf("Failed to connect to DHIS2 at %s.", req$url),
        i = gettext("Check your network connection and DHIS2 server URL."),
        i = conditionMessage(cnd)
      ), class = "neoipcr_dhis2_error", call = NULL)
    }
  )

  log_dhis2_request(resp, "me")

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
          gettextf("DHIS2 returned an HTML page instead of JSON (HTTP %d, URL: %s).", sc, url),
          i = gettext("This usually means the server redirected to a login page."),
          i = gettext("Your credentials may be missing, expired, or incorrect."),
          i = gettextf("Token auth: set the %s environment variable.", "NEOIPC_DHIS2_TOKEN"),
          i = gettextf(
            "Basic auth: set %s and %s environment variables.",
            "NEOIPC_DHIS2_USER", "NEOIPC_DHIS2_PASSWORD")
        ), call = NULL)
      }
      rlang::abort(c(
        gettextf("Unexpected DHIS2 response content type: %s", ct),
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
    # Nested under the `userCredentials` shim (2.40/2.41), or NA when absent
    # (2.42+ drop it from /me, and a user may never have logged in).
    # `parse_datetime()` errors on NULL, so the NA guard prevents a crash.
    lastLogin = readr::parse_datetime(
      raw_info$userCredentials$lastLogin %||% NA_character_),
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

# Read the currently-authenticated user into a `users` tibble.
#
# Used as a fallback by `assemble_metadata()` when the caller lacks
# the `F_USER_VIEW` / `F_METADATA_EXPORT` / `ALL` authorities required to
# fetch the full user list through the metadata endpoint.
#
# Returns a named list with two components:
#   * `public`       — schema-conformant tibble matching
#                      `compile_schema(users_cols, dataset_options)`.
#                      Always returned (never NULL); under `include_user
#                      = "no"` the entity gate short-circuits to 0×0.
#   * `internal_map` — orchestrator-internal tibble with columns
#                      `user_key`, `username`, `user` — consumed by fact
#                      readers for `createdBy` / `updatedBy` / `storedBy`
#                      / `completedBy` username→user_key (and DHIS2
#                      id→user_key) substitution. NULL under
#                      `include_user = "no"`.
read_user_info_table <- function(user_info, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    public       = compile_schema(users_cols, opts),
    internal_map = NULL
  )

  if (opts$include_user == "no")
    return(empty_result)

  raw <- user_info |>
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
    dplyr::rename("user" = "id") |>
    add_key_column("user_key")

  internal_map <- raw |>
    dplyr::select(tidyselect::all_of(c("user_key", "username", "user")))

  public <- raw |>
    finalize_to_schema(users_cols, opts)
  assert_schema(public, users_cols, opts)

  list(public = public, internal_map = internal_map)
}

# Read the metadata `users` collection into a `users` tibble.
#
# Same contract as `read_user_info_table()`: returns `list(public,
# internal_map)`. Called when the caller's authorities include
# `F_USER_VIEW` / `F_METADATA_EXPORT` / `ALL` and the metadata endpoint
# returns a full `users` payload. When `include_user = "no"` or the
# payload is absent, falls back to the empty shape so the orchestrator
# can distinguish "users not fetched yet" from "users fetched but
# empty" via the returned list's nullability of `internal_map`.
read_metadata_users <- function(metadata, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    public       = compile_schema(users_cols, opts),
    internal_map = NULL
  )

  if (opts$include_user == "no")
    return(empty_result)

  users <- metadata |>
    purrr::pluck("users")

  if (rlang::is_null(users))
    return(empty_result)

  raw <- users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::rename("user" = "id") |>
    add_key_column("user_key")

  if (opts$include_user == "full")
    raw <- raw |>
      dplyr::select(!tidyselect::any_of(c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits",
        "userRoles"))) |>
      dplyr::mutate(
        created   = readr::parse_datetime(.data$created),
        lastLogin = readr::parse_datetime(.data$lastLogin))

  internal_map <- raw |>
    dplyr::select(tidyselect::any_of(c("user_key", "username", "user")))

  public <- raw |>
    finalize_to_schema(users_cols, opts)
  assert_schema(public, users_cols, opts)

  list(public = public, internal_map = internal_map)
}

# Replace the user references in `fields` with their `user_key` from
# `metadata$.users_internal_map`, matched on `by`: the username, or the
# DHIS2 user id (`uid`, the map's `user` column). A reference is either the
# plain value (`storedBy`, `completedBy`) or a User object carrying it
# (`createdBy`, `updatedBy`); a reference without a match becomes NA.
#
# A field is resolved only where the input has it. DHIS2 omits a field that
# is null on every object in the response, and `createdBy` and `updatedBy`
# are null on every record created before the instance was upgraded to 2.36
# (events) or 2.37 (enrolments, tracked entities): those releases added the
# columns behind them without filling them in, and `updatedBy` is set only
# once a record is next edited.
resolve_user_fields <- function(x, metadata, fields, by = c("username", "uid"))
{
  by <- match.arg(by)
  users <- metadata$.users_internal_map
  known <- if (by == "uid") users$user else users$username

  for (field in intersect(fields, names(x))) {
    references <- x[[field]]
    if (is.list(references))
      references <- purrr::map_chr(
        references, \(user) if (is.list(user)) user[[by]] %||% NA_character_ else NA_character_)
    x[[field]] <- users$user_key[match(references, known, incomparables = NA)]
  }

  x
}
