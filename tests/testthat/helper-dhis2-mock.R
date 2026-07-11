# Offline DHIS2 HTTP interception helpers.
#
# neoipcr's import pipeline performs HTTP through httr2 — a sequential
# `req_perform()` for /me and `req_perform_parallel()` for the metadata and
# tracker stages. httr2's `local_mocked_responses()` intercepts both, so these
# helpers let tests drive the full pipeline against synthetic fixtures with no
# network, honouring the package's no-real-HTTP test rule.

# Build a synthetic JSON httr2 response.
#
# `url` MUST be the request URL: `read_metadata_reponse()` dispatches the
# metadata-vs-organisationUnits response by URL-path suffix, and the
# request-shape assertions read the URL back off the response.
mock_json_response <- function(url, body, status = 200L) {
  if (!is.character(body))
    body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  httr2::response(
    status_code = status,
    url = url,
    method = "GET",
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(paste(body, collapse = "\n")))
}

# Read a fixture file's raw JSON text (served as-is, never parsed here).
read_fixture_text <- function(name) {
  path <- testthat::test_path("fixtures", name)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# Assemble a synthetic /api/metadata response body from the shared metadata
# fixtures (the same merge read_test_metadata() feeds to read_metadata()), so
# the version matrix reuses one metadata graph and varies only the reported
# `system.version`.
build_metadata_response <- function(version = "2.40.3.2") {
  read_fx <- function(name)
    jsonlite::fromJSON(
      testthat::test_path("fixtures", name), simplifyVector = FALSE)

  md <- utils::modifyList(read_fx("system.json"), read_fx("program.json"))
  md <- utils::modifyList(md, read_fx("org-units.json"))
  am <- read_fx("antimicrobials.json")
  md$options        <- c(md$options, am$options)
  md$optionGroupSets <- c(md$optionGroupSets, am$optionGroupSets)
  md$system$version <- version

  jsonlite::toJSON(md, auto_unbox = TRUE, null = "null")
}

# URL-dispatching mock for the whole import_dhis2() pipeline.
#
# `fixtures` maps endpoint keys — me, metadata, organisationUnits,
# trackedEntities, enrollments, events — to raw JSON text. Returns:
#   * `mock` — pass to httr2::local_mocked_responses()
#   * `urls` — zero-arg accessor returning every request URL seen, in order
#     (used to assert per-version request shapes)
# A request whose URL matches no fixture ABORTS: a NULL return from the mock
# would silently fall through to a real network call, the one failure mode the
# no-real-HTTP test rule must forbid.
new_dhis2_mock <- function(fixtures, status = list()) {
  seen <- character()

  endpoint_of <- function(path) {
    if (endsWith(path, "/me")) "me"
    else if (endsWith(path, "/metadata")) "metadata"
    else if (endsWith(path, "/organisationUnits")) "organisationUnits"
    else if (grepl("/tracker/trackedEntities", path, fixed = TRUE))
      "trackedEntities"
    else if (grepl("/tracker/enrollments", path, fixed = TRUE)) "enrollments"
    else if (grepl("/tracker/events", path, fixed = TRUE)) "events"
    else NA_character_
  }

  mock <- function(req) {
    seen[[length(seen) + 1L]] <<- req$url
    key <- endpoint_of(httr2::url_parse(req$url)$path)
    if (is.na(key) || is.null(fixtures[[key]]))
      rlang::abort(paste0("unmocked DHIS2 request: ", req$url))
    # A function-valued fixture is called with the request, so a single
    # endpoint can return per-request bodies — e.g. the /tracker/events
    # per-org-unit fan-out returns each department's own events.
    body <- fixtures[[key]]
    if (is.function(body)) body <- body(req)
    mock_json_response(req$url, body, status[[key]] %||% 200L)
  }

  list(mock = mock, urls = function() seen)
}
