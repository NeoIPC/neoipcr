# Tests for R/dhis2-connect.R — connection options, authentication, token
# handling, and password resolution.

# A syntactically valid test token for neoipcr's read_token(), which checks a
# token's shape: version 1 is `d2pat_`, 32 letters or digits, and 10 digits;
# version 2 is `d2p_`, 44 letters or digits, and 6 more. Shaped to resemble
# real DHIS2 personal access tokens (a mixed Base64-alphabet body rather
# than all digits; DHIS2's real generators append a CRC32 checksum to the
# random part). Neither is a checksum-valid token and neither ever reaches
# a server — the tests are offline, and DHIS2 checks the checksum only
# when the token is used.
test_token <- "d2pat_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789012345"
# The version-2 fixture of the shape DHIS2 2.41 and later issue.
test_token_v2 <- "d2p_aB3dEfGhaB3dEfGhaB3dEfGhaB3dEfGhaB3dEfGhaB3dKx7Qm2"
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
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  expect_equal(neoipcr:::read_token(test_token), test_token)
})

test_that("read_token accepts a valid inline version-2 token", {
  expect_equal(neoipcr:::read_token(test_token_v2), test_token_v2)
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
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  # 49 chars total (one too many)
  expect_error(
    neoipcr:::read_token(
      "d2pat_7890123456789012345678901234567890123456789"),
    "Invalid")
})

test_that("read_token rejects a version-1 prefix carrying a version-2 body", {
  expect_error(
    neoipcr:::read_token(
      "d2pat_aB3dEfGhaB3dEfGhaB3dEfGhaB3dEfGhaB3dEfGhaB3dKx7Qm2"),
    "Invalid")
})

test_that("read_token rejects a version-2 prefix carrying a version-1 body", {
  expect_error(
    neoipcr:::read_token(
      "d2p_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789012345"),
    "Invalid")
})

test_that("read_token rejects a version-1 token with a character outside its set", {
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  # A `-` among the 32 random characters, and a letter among the 10 checksum digits.
  expect_error(
    neoipcr:::read_token(
      "d2pat_aBcDeFgHiJkLmNo-qRsTuVwXyZ0123456789012345"),
    "Invalid")
  expect_error(
    neoipcr:::read_token(
      "d2pat_aBcDeFgHiJkLmNoPqRsTuVwXyZ012345678901234a"),
    "Invalid")
})

test_that("read_token rejects a version-2 token with a character outside its set", {
  # A `-` among the 44 random characters.
  expect_error(
    neoipcr:::read_token(
      "d2p_aB3dEfGhaB3dEfGhaB3dE-GhaB3dEfGhaB3dEfGhaB3dKx7Qm2"),
    "Invalid")
})

test_that("read_token rejects each version one character short and one long", {
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  expect_error(
    neoipcr:::read_token(substring(test_token, 1, 47)),
    "Invalid")
  expect_error(
    neoipcr:::read_token(paste0(test_token, "7")),
    "Invalid")
  expect_error(
    neoipcr:::read_token(substring(test_token_v2, 1, 53)),
    "Invalid")
  expect_error(
    neoipcr:::read_token(paste0(test_token_v2, "7")),
    "Invalid")
})

test_that("read_token rejects an inline token with a trailing space or line break", {
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  expect_error(neoipcr:::read_token(paste0(test_token, " ")), "Invalid")
  expect_error(neoipcr:::read_token(paste0(test_token, "\n")), "Invalid")
  expect_error(neoipcr:::read_token(paste0(test_token_v2, " ")), "Invalid")
  expect_error(neoipcr:::read_token(paste0(test_token_v2, "\n")), "Invalid")
  # Whitespace standing inside a full-length string, where a check of prefix
  # and length alone would let the token through.
  expect_error(
    neoipcr:::read_token(paste0(substring(test_token, 1, 47), " ")),
    "Invalid")
  expect_error(
    neoipcr:::read_token(paste0(substring(test_token, 1, 47), "\n")),
    "Invalid")
})

test_that("read_token rejects a non-existent file path", {
  expect_error(
    neoipcr:::read_token(
      file.path(tempdir(), "nonexistent_token_file.txt")),
    "Invalid")
})

test_that("read_token reads a valid token from a file", {
  # NEOIPC-COMPAT(dhis2-version-1-token): see read_token() in R/dhis2-connect.R
  tmp <- withr::local_tempfile(lines = test_token)
  expect_equal(neoipcr:::read_token(tmp), test_token)
})

test_that("read_token reads a valid version-2 token from a file", {
  tmp <- withr::local_tempfile(lines = test_token_v2)
  expect_equal(neoipcr:::read_token(tmp), test_token_v2)
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
      neoipcr:::get_password("https://example.com", username_from_env = TRUE),
      "secret123"))
})

test_that("get_password aborts without env var (non-interactive)", {
  withr::with_envvar(
    c(NEOIPC_DHIS2_PASSWORD = NA_character_),
    expect_error(
      neoipcr:::get_password("https://example.com", username_from_env = TRUE),
      "No password found"))
})

test_that("the missing-password error names the source the username came from", {
  # A username passed as an argument says nothing about NEOIPC_DHIS2_USER,
  # which may be unset.
  withr::with_envvar(
    c(NEOIPC_DHIS2_USER = NA_character_, NEOIPC_DHIS2_PASSWORD = NA_character_),
    {
      error <- expect_error(
        dhis2_connection_options(username = "admin", hostname = test_host),
        "No password found")
      expect_match(conditionMessage(error),
                   "A username was given, but NEOIPC_DHIS2_PASSWORD is not set.", fixed = TRUE)
      expect_no_match(conditionMessage(error), "NEOIPC_DHIS2_USER", fixed = TRUE)
    })
  withr::with_envvar(
    c(NEOIPC_DHIS2_SESSION_ID = NA_character_,
      NEOIPC_DHIS2_TOKEN      = NA_character_,
      NEOIPC_DHIS2_USER       = "admin",
      NEOIPC_DHIS2_PASSWORD   = NA_character_),
    {
      error <- expect_error(
        dhis2_connection_options(hostname = test_host),
        "No password found")
      expect_match(conditionMessage(error),
                   "NEOIPC_DHIS2_USER is set but NEOIPC_DHIS2_PASSWORD is not.", fixed = TRUE)
    })
})
