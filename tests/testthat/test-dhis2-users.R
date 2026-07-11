# Tests for R/dhis2-users.R::get_user_info() — the /me reader.
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
