# Tests for R/import-dhis2.R — the import_dhis2() pipeline and its helpers.
#
# The end-to-end pipeline tests intercept all HTTP with
# httr2::local_mocked_responses via the dispatcher in helper-dhis2-mock.R
# (no real network calls, per the package's no-real-HTTP test rule).

# ---------------------------------------------------------------------------
# dhis2_ou_dialect() — version -> tracker org-unit request dialect.
# Pure function of a numeric_version; no HTTP.
# ---------------------------------------------------------------------------

test_that("dhis2_ou_dialect selects 2.40 param names + %3B-encoded semicolons", {
  for (v in c("2.40.3.2", "2.40.12.0", "2.40.99")) {
    d <- neoipcr:::dhis2_ou_dialect(as.numeric_version(v))
    expect_equal(d$mode_key, "ouMode", info = v)
    expect_equal(d$ou_key, "orgUnit", info = v)
    joined <- d$multi_uid(c("AAA", "BBB", "CCC"))
    # I()-wrapped so httr2 does not re-encode the pre-encoded %3B.
    expect_s3_class(joined, "AsIs")
    expect_equal(as.character(joined), "AAA%3BBBB%3BCCC", info = v)
  }
})

test_that("dhis2_ou_dialect selects >= 2.41 param names + plain comma", {
  for (v in c("2.41.0", "2.41.9.0", "2.42.5.1", "2.43.0.1")) {
    d <- neoipcr:::dhis2_ou_dialect(as.numeric_version(v))
    expect_equal(d$mode_key, "orgUnitMode", info = v)
    expect_equal(d$ou_key, "orgUnits", info = v)
    joined <- d$multi_uid(c("AAA", "BBB", "CCC"))
    expect_false(inherits(joined, "AsIs"), info = v)
    expect_equal(joined, "AAA,BBB,CCC", info = v)
  }
})

test_that("dhis2_ou_dialect switches dialect exactly at 2.41.0", {
  expect_equal(
    neoipcr:::dhis2_ou_dialect(as.numeric_version("2.40.99"))$mode_key,
    "ouMode")
  expect_equal(
    neoipcr:::dhis2_ou_dialect(as.numeric_version("2.41.0"))$mode_key,
    "orgUnitMode")
})

# ---------------------------------------------------------------------------
# import_dhis2() end-to-end — full pipeline against mocked responses.
# ---------------------------------------------------------------------------

# Dataset options for the offline pipeline tests: fetch the patient/enrollment/
# event tibbles, and bypass eligibility + validation filtering so the read
# path is exercised on its own (validation is covered by test-validation*).
import_test_opts <- function(...)
  dhis2_dataset_options(
    include_patient             = "full",
    patient_columns             = "id",
    include_enrollment          = "full",
    include_event               = "full",
    include_department          = "pseudo",
    include_ineligible_patients = TRUE,
    include_invalid_patients    = TRUE,
    ...)

# Fixture set for the default no-filter (ACCESSIBLE) path at a given version.
import_test_fixtures <- function(version = "2.40.3.2", me = "me-nested.json")
  list(
    me                = read_fixture_text(me),
    metadata          = build_metadata_response(version),
    organisationUnits = read_fixture_text("orgunits-departments.json"),
    trackedEntities   = read_fixture_text("tracker-trackedEntities.json"),
    enrollments       = read_fixture_text("tracker-enrollments.json"),
    events            = read_fixture_text("tracker-events.json"))

test_that("import_dhis2 reads a full dataset from mocked 2.40 responses", {
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)

  conn <- dhis2_connection_options(
    session_id = "test", hostname = "dhis2.example.org")

  ds <- import_dhis2(conn, import_test_opts())

  expect_s3_class(ds, "neoipcr_ds")
  expect_equal(nrow(ds$patients), 2L)
  expect_equal(nrow(ds$enrollments), 2L)
  expect_equal(nrow(ds$events), 2L)
  expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
  # Both events are Admission-stage; their per-event admission data reads out.
  expect_equal(nrow(ds$admissionData), 2L)
  expect_setequal(ds$admissionData$dol, c(3L, 5L))
  # No real HTTP: the mock served every request off a fixture.
  expect_true(all(grepl("dhis2.example.org", m$urls(), fixed = TRUE)))
})

test_that("import_dhis2 surfaces an HTTP error from a failed tracker request", {
  m <- new_dhis2_mock(
    import_test_fixtures(), status = list(trackedEntities = 409L))
  httr2::local_mocked_responses(m$mock)

  conn <- dhis2_connection_options(
    session_id = "test", hostname = "dhis2.example.org")

  expect_error(import_dhis2(conn, import_test_opts()), "trackedEntities")
})

test_that("import_dhis2 makes no real HTTP call for an unmocked endpoint", {
  # Drops the events fixture: reaching /tracker/events must abort in the mock
  # rather than fall through to a real network request.
  fx <- import_test_fixtures()
  fx$events <- NULL
  m <- new_dhis2_mock(fx)
  httr2::local_mocked_responses(m$mock)

  conn <- dhis2_connection_options(
    session_id = "test", hostname = "dhis2.example.org")

  expect_error(import_dhis2(conn, import_test_opts()), "unmocked DHIS2 request")
})

# ---------------------------------------------------------------------------
# Compatibility matrix — the declared DHIS2 versions (our pinned version plus
# the tip of each released line at or above it). See neoipcr_supported_versions().
# The read path must produce an identical dataset across every version; the
# tracker request shape must follow the version's org-unit dialect.
# ---------------------------------------------------------------------------

# The /me lastLogin shape follows the DHIS2 line: 2.40 and 2.41 nest it under
# the `userCredentials` shim; 2.42+ drop it from /me entirely.
me_fixture_for <- function(version) {
  if (as.numeric_version(version) >= "2.42") "me-no-lastlogin.json"
  else "me-nested.json"
}

test_conn <- function()
  dhis2_connection_options(session_id = "test", hostname = "dhis2.example.org")

for (v in neoipcr_supported_versions()$dhis2) {
  local({
    version <- v
    test_that(sprintf("import_dhis2 reads an identical dataset at DHIS2 %s", version), {
      m <- new_dhis2_mock(
        import_test_fixtures(version, me_fixture_for(version)))
      httr2::local_mocked_responses(m$mock)

      ds <- import_dhis2(test_conn(), import_test_opts())

      expect_equal(nrow(ds$patients), 2L)
      expect_equal(nrow(ds$enrollments), 2L)
      expect_equal(nrow(ds$events), 2L)
      expect_equal(nrow(ds$admissionData), 2L)
      expect_setequal(
        as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
    })
  })
}

# Fixtures for the two-department `department_filter` path — the only path that
# populates multi-UID org-unit values, so the only one that reveals the
# separator difference. Events fan out per department, so serve each
# department's events by the request's (decoded) org-unit id.
dept_filter_fixtures <- function(version) {
  fx <- import_test_fixtures(version, me_fixture_for(version))
  fx$organisationUnits <- read_fixture_text("orgunits-departments-2.json")
  # Events fan out one org unit per request, using the SINGULAR `orgUnit`
  # parameter on every line — serve each department's events by that id.
  fx$events <- function(req) {
    ou <- httr2::url_parse(req$url)$query$orgUnit
    if (identical(ou, "OU_DEPT_1")) read_fixture_text("tracker-events.json")
    else '{"events":[]}'
  }
  fx
}

# Extract a single query parameter's RAW (still percent-encoded) value from a
# URL. Exact-name match via the trailing "=" so "orgUnit=" does not also catch
# "orgUnitMode="/"orgUnits=". Used to assert the on-the-wire id separator.
raw_query_param <- function(url, name) {
  qs <- sub("^[^?]*\\?", "", url)
  parts <- strsplit(qs, "&", fixed = TRUE)[[1]]
  hit <- parts[startsWith(parts, paste0(name, "="))]
  sub(paste0("^", name, "="), "", hit)
}

# Run the department-filtered import and return the trackedEntities and (first)
# events request URLs with their parsed (percent-decoded) queries.
tracker_requests_for <- function(version) {
  m <- new_dhis2_mock(dept_filter_fixtures(version))
  httr2::local_mocked_responses(m$mock, env = rlang::caller_env())
  import_dhis2(
    test_conn(),
    import_test_opts(department_filter = c("DEPT_01", "DEPT_02")))
  te_url <- Find(
    function(u) grepl("/tracker/trackedEntities", u, fixed = TRUE), m$urls())
  ev_url <- Find(
    function(u) grepl("/tracker/events", u, fixed = TRUE), m$urls())
  list(
    te_url = te_url, te = httr2::url_parse(te_url)$query,
    ev_url = ev_url, ev = httr2::url_parse(ev_url)$query)
}

test_that("2.40 tracker requests use ouMode/orgUnit with %3B-joined ids", {
  r <- tracker_requests_for("2.40.3.2")

  expect_true("ouMode" %in% names(r$te))
  expect_false("orgUnitMode" %in% names(r$te))
  expect_true("orgUnit" %in% names(r$te))
  # add_key_column randomizes id order, so assert the id set + separator, not a
  # fixed string. url_parse decodes %3B back to ';'.
  expect_setequal(
    strsplit(r$te$orgUnit, ";", fixed = TRUE)[[1]],
    c("OU_DEPT_1", "OU_DEPT_2"))
  # The org-unit id value itself is joined with pre-encoded %3B — never a
  # literal or encoded comma (the fields= param has its own commas, so assert
  # on the orgUnit value, not the whole URL).
  raw <- raw_query_param(r$te_url, "orgUnit")
  expect_true(grepl("%3B", raw, fixed = TRUE))
  expect_false(grepl(",", raw, fixed = TRUE))
  expect_false(grepl("%2C", raw, fixed = TRUE))

  # /tracker/events uses the SINGULAR orgUnit (never orgUnits) with the
  # version's mode key.
  expect_true("ouMode" %in% names(r$ev))
  expect_true("orgUnit" %in% names(r$ev))
  expect_false("orgUnits" %in% names(r$ev))
})

test_that("2.41+ tracker requests use orgUnitMode/orgUnits with comma-joined ids", {
  for (v in c("2.41.9.0", "2.42.5.1", "2.43.0.1")) {
    r <- tracker_requests_for(v)

    expect_true("orgUnitMode" %in% names(r$te), info = v)
    expect_false("ouMode" %in% names(r$te), info = v)
    expect_true("orgUnits" %in% names(r$te), info = v)
    expect_setequal(
      strsplit(r$te$orgUnits, ",", fixed = TRUE)[[1]],
      c("OU_DEPT_1", "OU_DEPT_2"))
    # The decoded value above already proves comma-joining; the raw URL must carry
    # NO semicolon encoding (the 2.40 dialect). Accept the comma in whatever form
    # httr2 emits it -- a literal "," or the percent-encoded "%2C" -- so the test is
    # not coupled to httr2's encoding choice (both are valid for a query value).
    raw <- raw_query_param(r$te_url, "orgUnits")
    expect_true(grepl("%2C", raw, fixed = TRUE) || grepl(",", raw, fixed = TRUE), info = v)
    expect_false(grepl("%3B", raw, fixed = TRUE), info = v)
    expect_false(grepl(";", raw, fixed = TRUE), info = v)

    # /tracker/events keeps the SINGULAR orgUnit even on 2.41+ (it never gained
    # orgUnits); sending orgUnits there would 400 on a real server.
    expect_true("orgUnitMode" %in% names(r$ev), info = v)
    expect_true("orgUnit" %in% names(r$ev), info = v)
    expect_false("orgUnits" %in% names(r$ev), info = v)
  }
})

test_that("import_dhis2 warns when the DHIS2 server line is unsupported", {
  # A line above the declared range still reads (>= 2.41 dialect), but neoipcr
  # flags that it is unverified rather than failing.
  m <- new_dhis2_mock(import_test_fixtures("2.44.0.0", "me-no-lastlogin.json"))
  httr2::local_mocked_responses(m$mock)

  expect_warning(
    import_dhis2(test_conn(), import_test_opts()),
    class = "neoipcr_unsupported_dhis2_version")
})

test_that("patients/events/enrollments requests always fetch orgUnit (isTest mark + test-unit filter)", {
  # read_patients()/read_events()/read_enrollments() need orgUnit in BOTH
  # test-data states — to mark isTest when include_test_data = TRUE, and to
  # filter out test units when FALSE — so each request must carry it regardless
  # of the hierarchy includes. Asserted directly on the request builders (the
  # offline mock serves the full fixture regardless of the requested fields, so
  # it cannot see this).
  base <- httr2::request("https://dhis2.example.org/api/tracker")
  for (test_data in c(TRUE, FALSE)) {
    opts <- dhis2_dataset_options(
      include_patient    = "full",
      include_event      = "full",
      include_enrollment = "full",
      include_test_data  = test_data)
    info <- paste("include_test_data =", test_data)
    te_fields <- httr2::url_parse(
      neoipcr:::get_trackedEntities_request(base, opts, "PROG", "TET")$url
    )$query$fields
    ev_fields <- httr2::url_parse(
      neoipcr:::get_events_request(base, opts, "PROG")$url)$query$fields
    enr_fields <- httr2::url_parse(
      neoipcr:::get_enrollments_request(base, opts, "PROG")$url)$query$fields
    expect_match(te_fields, "orgUnit", info = info)
    expect_match(ev_fields, "orgUnit", info = info)
    expect_match(enr_fields, "orgUnit", info = info)
  }
})

test_that("import_dhis2 runs parameterless from NEOIPC_DHIS2_HOST + env auth", {
  # With the host in NEOIPC_DHIS2_HOST and credentials in the env, import_dhis2()
  # needs no arguments — the default connection_options resolves both.
  withr::local_envvar(
    NEOIPC_DHIS2_HOST       = "dhis2.example.org",
    NEOIPC_DHIS2_SESSION_ID = "test-session")
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)

  ds <- import_dhis2(dataset_options = import_test_opts())

  expect_equal(nrow(ds$patients), 2L)
  expect_true(any(grepl("dhis2.example.org", m$urls(), fixed = TRUE)))
})
