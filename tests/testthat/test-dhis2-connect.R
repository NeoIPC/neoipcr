# Tests for R/dhis2-connect.R — connection options, authentication, token
# handling, and password resolution.

# A syntactically valid test token for neoipcr's read_token(), which checks
# only the `d2pat_` prefix and the 48-char total. Shaped to resemble a real
# DHIS2 PAT (a mixed Base64-alphabet body rather than all digits; DHIS2's real
# generator appends a 10-digit CRC32 to a 32-char random part). It is not a
# checksum-valid token and never reaches a server — the tests are offline, and
# DHIS2 does not verify the checksum on the auth path regardless.
test_token <- "d2pat_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789012345"
test_host  <- "dhis2.example.org"

# --- dhis2_connection_options() ---

test_that("dhis2_connection_options errors without a host or NEOIPC_DHIS2_HOST", {
  # neoipcr is a public library: it does not default to any deployment's host.
  withr::local_envvar(NEOIPC_DHIS2_HOST = NA)
  expect_error(
    dhis2_connection_options(test_token),
    class = "neoipcr_missing_hostname")
  expect_error(
    dhis2_connection_options(test_token, hostname = ""),
    class = "neoipcr_missing_hostname")
  # An explicit NA is treated as unset (nzchar(NA) is TRUE, so it would otherwise slip past the guard and
  # reach httr2::url_build() as an unbranded error).
  expect_error(
    dhis2_connection_options(test_token, hostname = NA_character_),
    class = "neoipcr_missing_hostname")
})

test_that("dhis2_connection_options builds a base URL from an explicit host", {
  withr::local_envvar(NEOIPC_DHIS2_HOST = NA)
  result <- dhis2_connection_options(test_token, hostname = test_host)
  expect_equal(result$base_url, "https://dhis2.example.org/api")
})

test_that("dhis2_connection_options resolves the host from NEOIPC_DHIS2_HOST", {
  withr::local_envvar(NEOIPC_DHIS2_HOST = "env.example.org")
  result <- dhis2_connection_options(test_token)
  expect_equal(result$base_url, "https://env.example.org/api")
})

test_that("an explicit hostname overrides NEOIPC_DHIS2_HOST", {
  withr::local_envvar(NEOIPC_DHIS2_HOST = "env.example.org")
  result <- dhis2_connection_options(test_token, hostname = test_host)
  expect_equal(result$base_url, "https://dhis2.example.org/api")
})

test_that("dhis2_connection_options builds custom base URL", {
  result <- dhis2_connection_options(
    token    = test_token,
    scheme   = "http",
    hostname = "testhost",
    port     = 8080,
    path     = "/api/41")
  expect_equal(result$base_url, "http://testhost:8080/api/41")
})

test_that("dhis2_connection_options coerces string port", {
  result <- dhis2_connection_options(
    token    = test_token,
    hostname = "testhost",
    port     = "9090")
  expect_true(grepl(":9090", result$base_url, fixed = TRUE))
})

test_that("dhis2_connection_options rejects token and username together", {
  expect_error(
    dhis2_connection_options(
      token = test_token, username = "admin", hostname = test_host),
    "Exactly one of")
})

test_that("dhis2_connection_options returns neoipcr_dhis2_conopt class", {
  result <- dhis2_connection_options(test_token, hostname = test_host)
  expect_s3_class(result, "neoipcr_dhis2_conopt")
})

# --- print.neoipcr_dhis2_conopt() ---

test_that("print method shows token authentication", {
  result <- dhis2_connection_options(test_token, hostname = test_host)
  output <- capture.output(print(result))
  expect_true(any(grepl("Token", output)))
})

test_that("print method shows base URL", {
  result <- dhis2_connection_options(test_token, hostname = test_host)
  output <- capture.output(print(result))
  expect_true(any(grepl(test_host, output)))
})

# --- read_token() ---

test_that("read_token accepts a valid inline token", {
  expect_equal(neoipcr:::read_token(test_token), test_token)
})

test_that("read_token rejects a token with wrong prefix", {
  expect_error(
    neoipcr:::read_token(
      "xxxxx_789012345678901234567890123456789012345678"),
    "Invalid")
})

test_that("read_token rejects a token that is too short", {
  expect_error(
    neoipcr:::read_token("d2pat_abc"),
    "Invalid")
})

test_that("read_token rejects a token with correct prefix but wrong length", {
  # 49 chars total (one too many)
  expect_error(
    neoipcr:::read_token(
      "d2pat_7890123456789012345678901234567890123456789"),
    "Invalid")
})

test_that("read_token rejects a non-existent file path", {
  expect_error(
    neoipcr:::read_token(
      file.path(tempdir(), "nonexistent_token_file.txt")),
    "Invalid")
})

test_that("read_token reads a valid token from a file", {
  tmp <- withr::local_tempfile(lines = test_token)
  expect_equal(neoipcr:::read_token(tmp), test_token)
})

test_that("read_token rejects a file containing an invalid token", {
  tmp <- withr::local_tempfile(lines = "not-a-valid-token")
  expect_error(neoipcr:::read_token(tmp), "Invalid")
})

# --- get_auth_data() ---

test_that("get_auth_data returns session_id from env var (highest priority)", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = "test-session-abc",
      NEOIPC_DHIS2_TOKEN      = test_token,
      NEOIPC_DHIS2_USER       = "admin",
      NEOIPC_DHIS2_PASSWORD   = "secret"),
    {
      result <- neoipcr:::get_auth_data("https://example.com")
      expect_equal(result, list(session_id = "test-session-abc"))
    })
})

test_that("get_auth_data returns token when session_id absent", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = NA_character_,
      NEOIPC_DHIS2_TOKEN      = test_token,
      NEOIPC_DHIS2_USER       = NA_character_),
    {
      result <- neoipcr:::get_auth_data("https://example.com")
      expect_equal(result, list(token = test_token))
    })
})

test_that("get_auth_data returns username and password from env vars", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = NA_character_,
      NEOIPC_DHIS2_TOKEN      = NA_character_,
      NEOIPC_DHIS2_USER       = "admin",
      NEOIPC_DHIS2_PASSWORD   = "secret"),
    {
      result <- neoipcr:::get_auth_data("https://example.com")
      expect_equal(result, list(username = "admin", password = "secret"))
    })
})

test_that("get_auth_data aborts when user set but password missing (non-interactive)", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = NA_character_,
      NEOIPC_DHIS2_TOKEN      = NA_character_,
      NEOIPC_DHIS2_USER       = "admin",
      NEOIPC_DHIS2_PASSWORD   = NA_character_),
    expect_error(
      neoipcr:::get_auth_data("https://example.com"),
      "No password found"))
})

test_that("get_auth_data aborts with no credentials (non-interactive)", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = NA_character_,
      NEOIPC_DHIS2_TOKEN      = NA_character_,
      NEOIPC_DHIS2_USER       = NA_character_,
      NEOIPC_DHIS2_PASSWORD   = NA_character_),
    expect_error(
      neoipcr:::get_auth_data("https://example.com"),
      "No authentication credentials found"))
})

# --- get_password() ---

test_that("get_password returns password from env var", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_PASSWORD = "secret123"),
    expect_equal(
      neoipcr:::get_password("https://example.com"),
      "secret123"))
})

test_that("get_password aborts without env var (non-interactive)", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_PASSWORD = NA_character_),
    expect_error(
      neoipcr:::get_password("https://example.com"),
      "No password found"))
})
