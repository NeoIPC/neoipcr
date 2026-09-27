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
# `system.version`. `org_unit_attributes` merges the custom-attribute
# definitions in on request; the baseline graph carries none.
# `surveillance_end` adds the Surveillance-End stage with its patient-days
# field and two antibiotic-substance slots, which the baseline program lacks.
# `patient_eligibility` adds the birth-weight, gestational-age and
# total-gestation-days attributes, which the baseline program lacks too, so
# its patients arrive with neither value.
build_metadata_response <- function(version = "2.40.3.2",
                                    org_unit_attributes = FALSE,
                                    surveillance_end = FALSE,
                                    patient_eligibility = FALSE) {
  read_fx <- function(name)
    jsonlite::fromJSON(
      testthat::test_path("fixtures", name), simplifyVector = FALSE)

  md <- utils::modifyList(read_fx("system.json"), read_fx("program.json"))
  md <- utils::modifyList(md, read_fx("org-units.json"))
  if (org_unit_attributes)
    md <- utils::modifyList(md, read_fx("org-unit-attributes.json"))
  if (surveillance_end)
    md$programs[[1]]$programStages <- c(
      md$programs[[1]]$programStages,
      list(read_fx("program-stage-surveillance-end.json")))
  if (patient_eligibility)
    md$programs[[1]]$programTrackedEntityAttributes <- c(
      md$programs[[1]]$programTrackedEntityAttributes,
      read_fx("patient-eligibility-attributes.json"))
  am <- read_fx("antimicrobials.json")
  md$options        <- c(md$options, am$options)
  md$optionGroupSets <- c(md$optionGroupSets, am$optionGroupSets)
  md$system$version <- version

  jsonlite::toJSON(md, auto_unbox = TRUE, null = "null")
}

# URL-dispatching mock for the whole import_dhis2() pipeline.
#
# `fixtures` maps endpoint keys — me, metadata, organisationUnits, testUnits
# (the `IsTestunit` follow-up, told apart from the org-unit request by its
# filter on one attribute's value), trackedEntities, enrollments, events — to
# raw JSON text. Returns:
#   * `mock` — pass to httr2::local_mocked_responses()
#   * `urls` — zero-arg accessor returning every request URL seen, in order
#     (used to assert per-version request shapes)
# A request whose URL matches no fixture ABORTS: a NULL return from the mock
# would silently fall through to a real network call, the one failure mode the
# no-real-HTTP test rule must forbid.
# With `honour_fields`, each tracker response keeps only the fields its
# request's `fields` selector names, as a server's does; without it the mock
# serves every fixture whole, whatever the request asks for.
new_dhis2_mock <- function(fixtures, status = list(), honour_fields = FALSE) {
  seen <- character()

  endpoint_of <- function(url) {
    path <- url$path
    if (endsWith(path, "/me")) "me"
    else if (endsWith(path, "/metadata")) "metadata"
    else if (endsWith(path, "/organisationUnits")) {
      filters <- unlist(url$query[names(url$query) == "filter"])
      if (any(grepl("^[^.]+:eq:true$", filters))) "testUnits"
      else "organisationUnits"
    }
    else if (grepl("/tracker/trackedEntities", path, fixed = TRUE))
      "trackedEntities"
    else if (grepl("/tracker/enrollments", path, fixed = TRUE)) "enrollments"
    else if (grepl("/tracker/events", path, fixed = TRUE)) "events"
    else NA_character_
  }

  mock <- function(req) {
    seen[[length(seen) + 1L]] <<- req$url
    key <- endpoint_of(httr2::url_parse(req$url))
    if (is.na(key) || is.null(fixtures[[key]]))
      rlang::abort(paste0("unmocked DHIS2 request: ", req$url))
    # A function-valued fixture is called with the request, so a single
    # endpoint can return per-request bodies — e.g. the /tracker/events
    # per-org-unit fan-out returns each department's own events.
    body <- fixtures[[key]]
    if (is.function(body)) body <- body(req)
    if (honour_fields && key %in% c("trackedEntities", "enrollments", "events"))
      body <- select_requested_fields(
        body, httr2::url_parse(req$url)$query$fields)
    mock_json_response(req$url, body, status[[key]] %||% 200L)
  }

  list(mock = mock, urls = function() seen)
}

# A tracker response body narrowed to a request's `fields` selector: every
# instance of each collection in it keeps only the selected fields.
select_requested_fields <- function(body, fields) {
  if (is.character(body))
    body <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  selector <- parse_fields_selector(fields)
  body <- lapply(body, \(collection)
    if (is.list(collection)) select_fields(collection, selector)
    else collection)
  jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
}

# Parse a DHIS2 `fields` selector such as `a,b[c,d[e]]` into a named list with
# one entry per selected field: TRUE for a field taken whole, the parsed inner
# selector for one narrowed by brackets.
parse_fields_selector <- function(selector) {
  chars <- strsplit(selector, "", fixed = TRUE)[[1]]
  pos <- 0L
  # Consumes characters up to the `]` closing this level, or to the end;
  # `pos` is shared, so a nested call resumes its caller after the bracket.
  read_level <- function() {
    selected <- list()
    name <- ""
    while (pos < length(chars)) {
      pos <<- pos + 1L
      ch <- chars[[pos]]
      if (ch == "[") {
        selected[[name]] <- read_level()
        name <- ""
      } else if (ch %in% c(",", "]")) {
        if (nzchar(name)) selected[[name]] <- TRUE
        name <- ""
        if (ch == "]") return(selected)
      } else {
        name <- paste0(name, ch)
      }
    }
    if (nzchar(name)) selected[[name]] <- TRUE
    selected
  }
  read_level()
}

# Apply a parsed selector to a parsed JSON value: an object keeps the selected
# fields, each narrowed further where the selector says so; an array applies
# the selector to every element.
select_fields <- function(x, selector) {
  if (!is.list(x))
    return(x)
  if (is.null(names(x)))
    return(lapply(x, select_fields, selector))
  x <- x[intersect(names(x), names(selector))]
  for (field in names(x))
    if (is.list(selector[[field]]))
      x[[field]] <- select_fields(x[[field]], selector[[field]])
  x
}
