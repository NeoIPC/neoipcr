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
import_test_opts <- function(...) {
  defaults <- list(
    include_patient             = "full",
    patient_columns             = "id",
    include_enrollment          = "full",
    include_event               = "full",
    include_department          = "pseudo",
    include_ineligible_patients = TRUE,
    include_invalid_patients    = TRUE)
  # Overrides replace a default rather than duplicating its argument.
  do.call(dhis2_dataset_options, utils::modifyList(defaults, list(...)))
}

# Fixture set for the default no-filter (ACCESSIBLE) path at a given version.
# `org_unit_attributes` merges the custom-attribute definitions into the
# metadata response and mocks the IsTestunit follow-up they trigger; without
# them no follow-up is mocked, so an unwanted one aborts in the mock.
import_test_fixtures <- function(version = "2.40.12.0", me = "me-nested.json",
                                 org_unit_attributes = FALSE) {
  fx <- list(
    me                = read_fixture_text(me),
    metadata          = build_metadata_response(version, org_unit_attributes),
    organisationUnits = read_fixture_text("orgunits-departments.json"),
    trackedEntities   = read_fixture_text("tracker-trackedEntities.json"),
    enrollments       = read_fixture_text("tracker-enrollments.json"),
    events            = read_fixture_text("tracker-events.json"))
  if (org_unit_attributes)
    fx$testUnits <- '{"organisationUnits":[]}'
  fx
}

# The org-unit requests an import issued, parsed: the department request and,
# when the instance defines `IsTestunit`, the test-unit follow-up.
orgunit_requests <- function(urls) {
  parsed <- Filter(function(u) grepl("/organisationUnits", u, fixed = TRUE), urls) |>
    lapply(httr2::url_parse)
  is_flag <- vapply(parsed, function(u)
    any(grepl("^[^.]+:eq:true$", unlist(u$query[names(u$query) == "filter"]))),
    logical(1))
  list(departments = parsed[!is_flag], test_units = parsed[is_flag])
}

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
# Compatibility matrix — every DHIS2 version the offline read path is driven
# against. This set is deliberately WIDER than neoipcr_supported_versions():
# the declared range follows live verification, while these fixture runs keep
# the version-dependent request-shape and /me-shape logic covered on lines that
# are NOT supported. A green run here is not evidence that a real server of that
# version works, which is precisely why the two lists are kept separate.
# The read path must produce an identical dataset across every version; the
# tracker request shape must follow the version's org-unit dialect.
# ---------------------------------------------------------------------------

# Live-verified: a full import has been run against a real server on these.
# 2.40.3.2 is deliberately absent — it is a known-broken patch release. Because
# the runtime gate compares major.minor lines, excluding it here does not make a
# 2.40.3.2 server warn; it keeps the declaration and this matrix honest.
matrix_supported_versions <- c("2.40.12.0", "2.41.9.0")

# Not live-verified, and driven here for request-shape coverage only: a live
# import against 2.42 is known to fail, and 2.43 has never been run against a
# real server. Both must therefore raise the unsupported-version warning.
matrix_unsupported_versions <- c("2.42.5.1", "2.43.0.1")

# The declared range is a claim about live verification, so pin it here: a
# silent widening (adding a line nobody has run a real server on) goes red.
test_that("neoipcr_supported_versions declares only live-verified DHIS2 lines", {
  expect_equal(neoipcr_supported_versions()$dhis2, matrix_supported_versions)
})

# Muffle only the unsupported-version warning, so a test that deliberately
# drives an unsupported line still surfaces every other warning.
without_unsupported_warning <- function(expr)
  withCallingHandlers(
    expr,
    neoipcr_unsupported_dhis2_version = function(w) rlang::cnd_muffle(w))

# The /me lastLogin shape follows the DHIS2 line: 2.40 and 2.41 nest it under
# the `userCredentials` shim; 2.42+ drop it from /me entirely.
me_fixture_for <- function(version) {
  if (as.numeric_version(version) >= "2.42") "me-no-lastlogin.json"
  else "me-nested.json"
}

test_conn <- function()
  dhis2_connection_options(session_id = "test", hostname = "dhis2.example.org")

for (v in c(matrix_supported_versions, matrix_unsupported_versions)) {
  local({
    version <- v
    supported <- version %in% matrix_supported_versions
    test_that(sprintf("import_dhis2 reads an identical dataset at DHIS2 %s", version), {
      m <- new_dhis2_mock(
        import_test_fixtures(version, me_fixture_for(version)))
      httr2::local_mocked_responses(m$mock)

      # The gate is asserted in BOTH directions. Only checking that an
      # unsupported line warns leaves a regression that warns unconditionally
      # undetected — a stray warning does not fail a testthat test, so the
      # supported branch has to assert absence explicitly.
      if (supported)
        expect_no_warning(
          ds <- import_dhis2(test_conn(), import_test_opts()),
          class = "neoipcr_unsupported_dhis2_version")
      else
        expect_warning(
          ds <- import_dhis2(test_conn(), import_test_opts()),
          class = "neoipcr_unsupported_dhis2_version")

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
  r <- tracker_requests_for("2.40.12.0")

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
    # 2.42/2.43 are outside the declared range and warn; that is asserted in the
    # matrix above, so muffle just that warning here and keep this test on the
    # request shape. Any other warning still surfaces.
    r <- without_unsupported_warning(tracker_requests_for(v))

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

# ---------------------------------------------------------------------------
# Org-unit attribute values end to end — the opt-in import and the always-on
# IsTestunit test-unit source. The attribute fixture carries three departments
# under one hospital: DEPT_01 with typed values (one of them unparseable, one
# on an attribute without a definition, and IsTestunit "false"), DEPT_02 with
# none, DEPT_03 flagged IsTestunit "true"; the hospital carries a text value
# and IsTestunit "true", which must never reach isTest or the values; the
# matching tracker fixtures put PAT_3 in DEPT_03.
# ---------------------------------------------------------------------------

attribute_fixtures <- function(version = "2.40.12.0") {
  fx <- import_test_fixtures(
    version, me_fixture_for(version), org_unit_attributes = TRUE)
  fx$organisationUnits <- read_fixture_text("orgunits-departments-attributes.json")
  fx$trackedEntities   <- read_fixture_text("tracker-trackedEntities-attributes.json")
  fx$enrollments       <- read_fixture_text("tracker-enrollments-attributes.json")
  fx$events            <- read_fixture_text("tracker-events-attributes.json")
  # What the IsTestunit follow-up request returns for this org-unit fixture.
  fx$testUnits         <- '{"organisationUnits":[{"id":"OU_DEPT_3"}]}'
  fx
}

typed_value_columns <- c(
  "value_text", "value_logical", "value_integer", "value_number",
  "value_date", "value_datetime")

test_that("import_dhis2 imports typed org-unit attribute values for the opted-in entities", {
  m <- new_dhis2_mock(attribute_fixtures())
  httr2::local_mocked_responses(m$mock)

  # DEPT_01's unparseable NUMBER value is reported to the caller who opted
  # into the values — and only then (the tests below assert the silence).
  expect_warning(
    ds <- import_dhis2(test_conn(), import_test_opts(
      include_department        = "full",
      include_hospital          = "full",
      include_custom_attributes = c("departments", "hospitals"))),
    class = "neoipcr_attribute_value_parse_failure")

  defs <- ds$metadata$orgUnitAttributes
  expect_named(defs, c("code", "name", "valueType"))
  expect_true(all(c("IsTestunit", "TEST_ATTR_TEXT", "TEST_ATTR_DATE",
                    "TEST_ATTR_INT", "TEST_ATTR_NUMBER") %in% defs$code))

  dept_values <- ds$metadata$departmentAttributeValues
  expect_named(dept_values, c("department_key", "attribute_code", typed_value_columns))
  dept_1 <- ds$metadata$departments$department_key[
    ds$metadata$departments$code == "DEPT_01"]
  rows <- dept_values[dept_values$department_key == dept_1, ]
  expect_setequal(
    rows$attribute_code,
    c("TEST_ATTR_TEXT", "TEST_ATTR_DATE", "TEST_ATTR_INT", "TEST_ATTR_NUMBER"))
  expect_equal(
    rows$value_date[rows$attribute_code == "TEST_ATTR_DATE"],
    as.Date("2024-08-03"))
  expect_true(is.na(rows$value_text[rows$attribute_code == "TEST_ATTR_DATE"]))
  expect_identical(rows$value_integer[rows$attribute_code == "TEST_ATTR_INT"], 12L)
  expect_equal(rows$value_text[rows$attribute_code == "TEST_ATTR_TEXT"], "A text value")
  expect_true(is.na(rows$value_number[rows$attribute_code == "TEST_ATTR_NUMBER"]))
  # The undefined attribute's value is dropped, IsTestunit never surfaces as
  # a value, and DEPT_02 (no values) / DEPT_03 (test unit, excluded)
  # contribute nothing.
  expect_equal(nrow(dept_values), 4L)
  expect_false("IsTestunit" %in% dept_values$attribute_code)

  hosp_values <- ds$metadata$hospitalAttributeValues
  expect_named(hosp_values, c("hospital_key", "attribute_code", typed_value_columns))
  # The hospital's own IsTestunit flag is dropped like a department's.
  expect_equal(nrow(hosp_values), 1L)
  expect_equal(hosp_values$attribute_code, "TEST_ATTR_TEXT")
  expect_equal(hosp_values$value_text, "Hospital text")
  expect_equal(hosp_values$hospital_key, ds$metadata$hospitals$hospital_key)

  expect_null(ds$metadata$.orgUnitAttributes_internal_map)
  expect_false("attributeValues" %in% names(ds$metadata$departments))
  expect_false("attributeValues" %in% names(ds$metadata$hospitals))

  requests <- orgunit_requests(m$urls())
  expect_length(requests$departments, 1L)
  expect_match(
    requests$departments[[1]]$query$fields,
    "attributeValues[attribute[id],value]", fixed = TRUE)
  # The IsTestunit follow-up: ids only, filtered by the attribute's value,
  # with the uid resolved from the definitions by code.
  expect_length(requests$test_units, 1L)
  flag_query <- requests$test_units[[1]]$query
  expect_equal(flag_query$fields, "id")
  expect_equal(flag_query$withinUserHierarchy, "true")
  expect_setequal(
    unlist(flag_query[names(flag_query) == "filter"]),
    c("organisationUnitGroups.code:eq:NEO_DEPARTMENT", "ATTR_FLAG_01:eq:true"))
  md_url <- Find(function(u) grepl("/metadata", u, fixed = TRUE), m$urls())
  expect_equal(
    httr2::url_parse(md_url)$query[["attributes:filter"]],
    "organisationUnitAttribute:eq:true")
})

# The same import on every supported line, in outline: the request shapes
# above are version-independent, but a line-specific regression anywhere in
# the pipeline would otherwise pass unseen.
for (v in matrix_supported_versions) {
  local({
    version <- v
    test_that(sprintf("import_dhis2 imports typed attribute values and the IsTestunit flag at DHIS2 %s", version), {
      m <- new_dhis2_mock(attribute_fixtures(version))
      httr2::local_mocked_responses(m$mock)

      expect_warning(
        ds <- import_dhis2(test_conn(), import_test_opts(
          include_department        = "full",
          include_hospital          = "full",
          include_custom_attributes = c("departments", "hospitals"))),
        class = "neoipcr_attribute_value_parse_failure")

      expect_equal(as.character(ds$metadata$system$version), version)
      expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
      expect_false("DEPT_03" %in% ds$metadata$departments$code)

      dept_values <- ds$metadata$departmentAttributeValues
      expect_setequal(
        dept_values$attribute_code,
        c("TEST_ATTR_TEXT", "TEST_ATTR_DATE", "TEST_ATTR_INT", "TEST_ATTR_NUMBER"))
      expect_equal(
        dept_values$value_date[dept_values$attribute_code == "TEST_ATTR_DATE"],
        as.Date("2024-08-03"))
      expect_equal(ds$metadata$hospitalAttributeValues$attribute_code, "TEST_ATTR_TEXT")

      requests <- orgunit_requests(m$urls())
      expect_length(requests$test_units, 1L)
      flag_query <- requests$test_units[[1]]$query
      expect_true("ATTR_FLAG_01:eq:true" %in% unlist(flag_query[names(flag_query) == "filter"]))
    })
  })
}

test_that("import_dhis2 excludes a department flagged IsTestunit like a TEST_UNITS member", {
  # Under the pseudonymized department default no attribute value is
  # requested at all: the flag arrives from the follow-up request, and the
  # unparseable value on DEPT_01 never reaches the client.
  m <- new_dhis2_mock(attribute_fixtures())
  httr2::local_mocked_responses(m$mock)

  expect_no_warning(
    ds <- import_dhis2(test_conn(), import_test_opts()),
    class = "neoipcr_attribute_value_parse_failure")

  expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
  # DEPT_01 survives; DEPT_03 is a test unit and DEPT_02 has no patients.
  expect_equal(nrow(ds$metadata$departments), 1L)

  requests <- orgunit_requests(m$urls())
  expect_equal(requests$departments[[1]]$query$fields, "id")
  expect_length(requests$test_units, 1L)
})

test_that("import_dhis2 names the IsTestunit lookup when its follow-up request fails", {
  m <- new_dhis2_mock(attribute_fixtures(), status = list(testUnits = 500L))
  httr2::local_mocked_responses(m$mock)

  expect_error(
    import_dhis2(test_conn(), import_test_opts()),
    "IsTestunit.*HTTP 500")

  # A connection failure has no response to report a status from.
  fx <- attribute_fixtures()
  fx$testUnits <- function(req)
    rlang::abort("no route to host", class = c("httr2_failure", "httr2_error"))
  m <- new_dhis2_mock(fx)
  httr2::local_mocked_responses(m$mock)

  expect_error(
    import_dhis2(test_conn(), import_test_opts()),
    "IsTestunit.*could not be performed: no route to host")
})

test_that("import_dhis2 issues no test-unit follow-up when the instance defines no IsTestunit attribute", {
  # The baseline metadata fixture carries no attribute definitions and the
  # fixture set mocks no follow-up, so an unwanted one aborts in the mock.
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)

  ds <- import_dhis2(test_conn(), import_test_opts())

  expect_length(orgunit_requests(m$urls())$test_units, 0L)
  expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
})

test_that("import_dhis2 marks a department flagged IsTestunit as isTest under include_test_data", {
  m <- new_dhis2_mock(attribute_fixtures())
  httr2::local_mocked_responses(m$mock)

  expect_no_warning(
    ds <- import_dhis2(test_conn(), import_test_opts(
      include_department = "full", include_test_data = TRUE)),
    class = "neoipcr_attribute_value_parse_failure")

  expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2", "PAT_3"))
  depts <- ds$metadata$departments
  expect_true(depts$isTest[depts$code == "DEPT_03"])
  # DEPT_01 carries IsTestunit "false", and its hospital's "true" is not
  # consulted.
  expect_false(depts$isTest[depts$code == "DEPT_01"])
  # Without the opt-in the values tables and the definitions stay 0x0.
  expect_equal(ncol(ds$metadata$departmentAttributeValues), 0L)
  expect_equal(ncol(ds$metadata$hospitalAttributeValues), 0L)
  expect_equal(ncol(ds$metadata$orgUnitAttributes), 0L)
})

test_that("import_dhis2 yields empty, schema-shaped attribute tables when the response carries no attribute values", {
  # orgunits-departments.json carries `id` only — no `attributeValues` key.
  m <- new_dhis2_mock(import_test_fixtures(org_unit_attributes = TRUE))
  httr2::local_mocked_responses(m$mock)

  ds <- import_dhis2(test_conn(), import_test_opts(
    include_department = "full", include_custom_attributes = "departments"))

  expect_named(
    ds$metadata$departmentAttributeValues,
    c("department_key", "attribute_code", typed_value_columns))
  expect_equal(nrow(ds$metadata$departmentAttributeValues), 0L)
  expect_equal(ncol(ds$metadata$hospitalAttributeValues), 0L)
  # The five coded definitions; the code-less one is listed nowhere.
  expect_equal(nrow(ds$metadata$orgUnitAttributes), 5L)
})

test_that("import_dhis2 reads the org-unit metadata alone when every fact entity is switched off", {
  # A site list: the departments with their attribute values, but no
  # patients, enrollments or events — under the default eligibility filter,
  # which has no admission data to act on, and the default validation pass,
  # which has no patients to validate.
  m <- new_dhis2_mock(attribute_fixtures())
  httr2::local_mocked_responses(m$mock)

  expect_warning(
    ds <- import_dhis2(test_conn(), import_test_opts(
      include_patient             = "no",
      include_enrollment          = "no",
      include_event               = "no",
      include_department          = "full",
      include_custom_attributes   = "departments",
      include_ineligible_patients = FALSE,
      include_invalid_patients    = FALSE)),
    class = "neoipcr_attribute_value_parse_failure")

  expect_equal(ncol(ds$patients), 0L)
  expect_equal(ncol(ds$enrollments), 0L)
  expect_equal(ncol(ds$admissionData), 0L)
  # No fact tibble anchors the post-filter, so every non-test department
  # stays listed, whether or not it has patients.
  expect_setequal(ds$metadata$departments$code, c("DEPT_01", "DEPT_02"))
  expect_equal(nrow(ds$metadata$departmentAttributeValues), 4L)
})

test_that("import_dhis2 completes a metadata-only import under the public defaults", {
  # `dhis2_dataset_options()` itself, not the test helper's overrides: the
  # default eligibility filter and validation pass must both stand aside.
  # Departments default to "no" as well, so the pseudonymized tier is asked
  # for to have something to observe.
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)

  ds <- import_dhis2(test_conn(), dhis2_dataset_options(
    include_patient    = "no",
    include_enrollment = "no",
    include_event      = "no",
    include_department = "pseudo"))

  expect_equal(ncol(ds$patients), 0L)
  expect_gt(nrow(ds$metadata$departments), 0L)
})

test_that("import_dhis2 refuses to validate patients without the full enrollments and events to check them against", {
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)

  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_enrollment       = "no",
      include_event            = "no",
      include_invalid_patients = FALSE)),
    class = "neoipcr_validation_needs_facts")
  # The pseudonymized tiers do not carry what the rules read either.
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_enrollment       = "pseudo",
      include_invalid_patients = FALSE)),
    class = "neoipcr_validation_needs_facts")
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_event            = "pseudo",
      include_invalid_patients = FALSE)),
    class = "neoipcr_validation_needs_facts")
  # The precondition reads the options alone, so it fails before any request.
  expect_length(m$urls(), 0L)

  # With the full tiers the pass runs.
  ds <- import_dhis2(test_conn(), import_test_opts(
    include_invalid_patients = FALSE))
  expect_true(is.data.frame(ds$validationResults))

  # An exception list must be a data frame of exception records; anything
  # else is refused before the first request, since the list is mapped onto
  # the imported records by those columns.
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_invalid_patients = c("PAT_1", "PAT_2"))),
    class = "neoipcr_invalid_exception_list")
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_invalid_patients = tibble::tibble(NEOIPC_PATIENT_ID = "PAT_1"))),
    class = "neoipcr_invalid_exception_list")
  # `NULL` is neither switch nor list; the message names what was supplied.
  expect_error(
    import_dhis2(test_conn(), dhis2_dataset_options(
      include_patient          = "full",
      include_enrollment       = "full",
      include_event            = "full",
      include_invalid_patients = NULL)),
    "Got `NULL`", class = "neoipcr_invalid_exception_list")
  # The dates join onto `Date` columns, so text dates are refused up front.
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_invalid_patients = tibble::tibble(
        RULE_ID           = 1L,
        NEOIPC_PATIENT_ID = "PAT_1",
        ENROLMENT_DATE    = "2024-01-01",
        EVENT_TYPE        = "adm",
        EVENT_DATE        = "2024-01-01"))),
    class = "neoipcr_invalid_exception_list")
  expect_length(m$urls(), 0L)

  # A well-formed list needs the full patient tier, which keeps `patient_id`
  # for the matching whatever `patient_columns` says; a pseudonymized tier
  # is refused before the first request.
  exceptions <- tibble::tibble(
    RULE_ID           = integer(),
    NEOIPC_PATIENT_ID = character(),
    ENROLMENT_DATE    = as.Date(character()),
    EVENT_TYPE        = character(),
    EVENT_DATE        = as.Date(character()))
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_patient          = "pseudo",
      include_invalid_patients = exceptions)),
    class = "neoipcr_validation_needs_facts")
  expect_length(m$urls(), 0L)
  ds <- import_dhis2(test_conn(), import_test_opts(
    include_patient          = "full",
    patient_columns          = character(),
    include_invalid_patients = exceptions))
  expect_true(is.data.frame(ds$validationResults))
  expect_true("patient_id" %in% names(ds$patients))

  # The list's columns must be of the types the records join on, and an
  # event type outside the stage vocabulary would silently match nothing.
  # A fresh mock, so the request count below covers these cases alone.
  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)
  for (bad in list(
    list(NEOIPC_PATIENT_ID = 1L),
    list(RULE_ID = "3"),
    list(EVENT_TYPE = "admission"),
    list(ENROLMENT_DATE = as.POSIXct("2024-01-01", tz = "UTC")))) {
    malformed <- tibble::tibble(
      RULE_ID           = 3L,
      NEOIPC_PATIENT_ID = "PAT_1",
      ENROLMENT_DATE    = as.Date("2024-01-01"),
      EVENT_TYPE        = NA_character_,
      EVENT_DATE        = as.Date(NA))
    malformed[[names(bad)]] <- bad[[1]]
    expect_error(
      import_dhis2(test_conn(), import_test_opts(
        include_invalid_patients = malformed)),
      class = "neoipcr_invalid_exception_list")
  }
  # The records are matched within their department, which a department
  # tier of "no" cannot provide.
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_department       = "no",
      include_invalid_patients = exceptions)),
    class = "neoipcr_validation_needs_facts")
  expect_length(m$urls(), 0L)

  # Without patients the pass never reads the list, so a metadata-only
  # import accepts one under the public constructor.
  ds <- import_dhis2(test_conn(), dhis2_dataset_options(
    include_patient          = "no",
    include_enrollment       = "no",
    include_event            = "no",
    include_department       = "pseudo",
    include_invalid_patients = exceptions))
  expect_equal(ncol(ds$patients), 0L)

  # With more than one department the records join on the department code
  # as well; a list without it is refused once the count is known, before
  # the mapping.
  m <- new_dhis2_mock(attribute_fixtures())
  httr2::local_mocked_responses(m$mock)
  expect_error(
    import_dhis2(test_conn(), import_test_opts(
      include_patient          = "full",
      include_department       = "full",
      include_test_data        = TRUE,
      include_invalid_patients = exceptions)),
    class = "neoipcr_invalid_exception_list")
})

test_that("import_dhis2 keeps the records an exception list names", {
  # On the mock, rules 3 and 25 flag both patients at the enrollment level
  # (their admission events are dated a day after the enrollment). A list
  # naming those records — `NA` event type and date for enrollment-level
  # findings — keeps both patients with nothing left flagged.
  flagged <- tibble::tibble(
    RULE_ID           = c(3L, 3L, 25L, 25L),
    NEOIPC_PATIENT_ID = c("PAT_1", "PAT_2", "PAT_1", "PAT_2"),
    ENROLMENT_DATE    = as.Date(c("2024-01-01", "2024-01-05", "2024-01-01", "2024-01-05")),
    EVENT_TYPE        = NA_character_,
    EVENT_DATE        = as.Date(NA))

  m <- new_dhis2_mock(import_test_fixtures())
  httr2::local_mocked_responses(m$mock)
  removed <- import_dhis2(test_conn(), import_test_opts(
    include_department       = "full",
    include_invalid_patients = FALSE))
  expect_equal(nrow(removed$patients), 0L)
  expect_setequal(removed$validationResults$rule_id, c(3L, 25L))

  kept <- import_dhis2(test_conn(), import_test_opts(
    include_department       = "full",
    include_invalid_patients = flagged))
  expect_setequal(as.character(kept$patients$patient_id), c("PAT_1", "PAT_2"))
  expect_equal(nrow(kept$validationResults), 0L)

  # With a second department in the import the records join on the
  # department code as well.
  fx <- import_test_fixtures()
  fx$organisationUnits <- read_fixture_text("orgunits-departments-2.json")
  m <- new_dhis2_mock(fx)
  httr2::local_mocked_responses(m$mock)
  kept <- import_dhis2(test_conn(), import_test_opts(
    include_department       = "full",
    include_invalid_patients = flagged |> dplyr::mutate(DEPARTMENT_CODE = "DEPT_01")))
  expect_setequal(as.character(kept$patients$patient_id), c("PAT_1", "PAT_2"))
  expect_equal(nrow(kept$validationResults), 0L)

  # Opting out of validation is the way to a patient-only import.
  ds <- import_dhis2(test_conn(), import_test_opts(
    include_enrollment = "no",
    include_event      = "no"))
  expect_equal(ncol(ds$enrollments), 0L)
  expect_setequal(as.character(ds$patients$patient_id), c("PAT_1", "PAT_2"))
})
