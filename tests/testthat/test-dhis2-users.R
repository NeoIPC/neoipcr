# Tests for R/dhis2-users.R: get_user_info() — the /me reader — and the
# user-reference resolution the tracker readers share.
# All HTTP is intercepted with httr2::local_mocked_responses (no real calls).

me_request <- function()
  httr2::request("https://dhis2.example.org/api")

mock_me <- function(fixture)
  httr2::local_mocked_responses(
    list(mock_json_response(
      "https://dhis2.example.org/api/me",
      read_fixture_text(fixture))),
    env = rlang::caller_env())

expected_last_login <- readr::parse_datetime("2024-06-01T12:00:00.000+0000")

test_that("get_user_info reads lastLogin nested under userCredentials (2.40/2.41)", {
  mock_me("me-nested.json")

  info <- neoipcr:::get_user_info(me_request())

  expect_s3_class(info, "neoipc_dhis2_usrinfo")
  expect_equal(info$lastLogin, expected_last_login)
  expect_equal(info$username, "neoipc_user")
  expect_equal(info$organisationUnits, "OU_DEPT_1")
  expect_true("F_TRACKED_ENTITY_INSTANCE_SEARCH" %in% info$authorities)
})

test_that("get_user_info yields NA lastLogin (no crash) when /me carries none", {
  # 2.42+ drop lastLogin from /me entirely, and a user may never have logged
  # in — either way the read must be NA, not a crash (parse_datetime errors on
  # NULL).
  mock_me("me-no-lastlogin.json")

  info <- neoipcr:::get_user_info(me_request())

  expect_true(is.na(info$lastLogin))
  expect_equal(info$username, "neoipc_user")
})

test_that("get_user_info passes a rejected login through as the authentication error", {
  # The outer handler wraps any other failure as a connection error of the
  # same class, quoting the original message in a bullet; the translated
  # authentication error must pass it with its own headline.
  httr2::local_mocked_responses(list(mock_json_response(
    "https://dhis2.example.org/api/me", "{}", status = 401L)))

  cnd <- expect_error(
    neoipcr:::get_user_info(me_request()), class = "neoipcr_dhis2_error")
  expect_match(cnd$message, "^DHIS2 authentication failed")
})

test_that("get_user_info names the URL it requested when access is denied", {
  # A server whose API is not at /api, so the URL is the connection's.
  httr2::local_mocked_responses(list(mock_json_response(
    "https://dhis2.example.org/dhis/api/me", "{}", status = 403L)))
  cnd <- expect_error(
    neoipcr:::get_user_info(httr2::request("https://dhis2.example.org/dhis/api")),
    class = "neoipcr_dhis2_error")
  expect_match(conditionMessage(cnd),
               "you lack permission to access https://dhis2.example.org/dhis/api/me.",
               fixed = TRUE)
})

test_that("get_user_info reports an HTTP error status as one, not as a failure to connect", {
  # The server answered, so the connection did not fail.
  for (status in c(404L, 500L)) {
    httr2::local_mocked_responses(list(mock_json_response(
      "https://dhis2.example.org/api/me", "{}", status = status)))
    cnd <- expect_error(
      neoipcr:::get_user_info(me_request()), class = "neoipcr_dhis2_error")
    expect_match(conditionMessage(cnd),
                 sprintf("DHIS2 answered with an error (HTTP %d) at https://dhis2.example.org/api.", status),
                 fixed = TRUE, info = status)
    expect_no_match(conditionMessage(cnd), "Failed to connect", fixed = TRUE)
  }
})

test_that("get_user_info reports a request that got no answer as a failure to connect", {
  # httr2 raises an `httr2_failure`, with curl's error as its parent, when a
  # request gets no answer; the mock raises the same condition in curl's place.
  httr2::local_mocked_responses(function(req)
    rlang::abort(
      "Failed to perform HTTP request.",
      class = c("httr2_failure", "httr2_error"),
      parent = simpleError("Could not resolve host: dhis2.example.org")))
  cnd <- expect_error(
    neoipcr:::get_user_info(me_request()), class = "neoipcr_dhis2_error")
  expect_match(conditionMessage(cnd),
               "Failed to connect to DHIS2 at https://dhis2.example.org/api.", fixed = TRUE)
  expect_match(conditionMessage(cnd), "Could not resolve host", fixed = TRUE)
})

# ---- resolve_user_fields() ---------------------------------------------------

users_metadata <- list(.users_internal_map = tibble::tibble(
  user_key = c(1L, 2L, 3L),
  user     = c("UID_admin", "UID_other", "UID_nameless"),
  username = c("admin", "other", NA)))

test_that("resolve_user_fields resolves plain usernames and User objects by name", {
  records <- tibble::tibble(
    storedBy  = c("admin", "unknown", NA),
    createdBy = list(
      list(username = "other"),
      NULL,
      list(uid = "UID_admin", username = "admin")))

  resolved <- neoipcr:::resolve_user_fields(
    records, users_metadata, c("storedBy", "createdBy", "updatedBy"))

  # A field the input lacks stays absent. An unknown or missing reference
  # resolves to NA; a missing one does not match the user the map holds
  # without a username.
  expect_named(resolved, c("storedBy", "createdBy"))
  expect_identical(resolved$storedBy, c(1L, NA, NA))
  expect_identical(resolved$createdBy, c(2L, NA, 1L))
})

test_that("resolve_user_fields matches a User object on its uid when asked to", {
  notes <- tibble::tibble(createdBy = list(
    list(uid = "UID_other", username = "admin"),
    list(uid = "UID_nameless")))

  resolved <- neoipcr:::resolve_user_fields(
    notes, users_metadata, "createdBy", by = "uid")

  expect_identical(resolved$createdBy, c(2L, 3L))
})

test_that("resolve_user_fields reads a field with no value on any record, and no records", {
  # A hoist from records none of which carries the field yields NA, not a
  # list.
  expect_identical(
    neoipcr:::resolve_user_fields(
      tibble::tibble(createdBy = c(NA, NA)), users_metadata, "createdBy")$createdBy,
    c(NA_integer_, NA_integer_))
  expect_identical(
    neoipcr:::resolve_user_fields(
      tibble::tibble(createdBy = list()), users_metadata, "createdBy")$createdBy,
    integer())
})
