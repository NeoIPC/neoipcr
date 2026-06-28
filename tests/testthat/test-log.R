# Tests for the neoipcr logging layer (R/log.R). No network: a fake httr2
# response is constructed offline. The headline assertion is the data-
# protection guard — log_dhis2_request must record URL + status + row count
# but never any part of the response body.

# Run `expr` with the neoipcr-namespace log threshold set to `threshold`,
# returning the log lines it emits. neoipcr's appender is the inherited
# console appender (stderr), so the output is captured from the message
# stream; only the threshold is mutated, and it is restored afterwards.
with_captured_neoipcr_logs <- function(expr, threshold = logger::TRACE) {
  prev_threshold <- logger::log_threshold(namespace = "neoipcr")
  on.exit(
    logger::log_threshold(prev_threshold, namespace = "neoipcr"),
    add = TRUE)
  logger::log_threshold(threshold, namespace = "neoipcr")
  utils::capture.output(force(expr), type = "message")
}

# A minimal offline httr2 response carrying a recognizable body token that must
# never appear in any log line.
fake_response <- function(status = 200L, url = "https://example.org/api/me") {
  httr2::response(
    status_code = status,
    url = url,
    body = charToRaw('{"SENSITIVE_BODY_TOKEN":"surveillance-data"}'))
}

test_that("log_dhis2_request records URL + status + row count, never the body", {
  lines <- with_captured_neoipcr_logs(
    neoipcr:::log_dhis2_request(fake_response(), "me", n_rows = 42L))

  expect_length(lines, 1L)
  expect_match(lines, "https://example.org/api/me", fixed = TRUE)
  expect_match(lines, "status=200")
  expect_match(lines, "rows=42")
  # Data-protection guard: the response body must never reach the log.
  expect_no_match(lines, "SENSITIVE_BODY_TOKEN", fixed = TRUE)
  expect_no_match(lines, "surveillance-data", fixed = TRUE)
})

test_that("log_dhis2_request omits the row count when not supplied", {
  lines <- with_captured_neoipcr_logs(
    neoipcr:::log_dhis2_request(fake_response(), "me"))

  expect_match(lines, "status=200")
  expect_no_match(lines, "rows=")
})

test_that("log_dhis2_request accepts an httr2 error object (failed request)", {
  err <- tryCatch(
    httr2::resp_check_status(fake_response(status = 404L)),
    error = function(e) e)
  expect_true(rlang::is_error(err))

  lines <- with_captured_neoipcr_logs(
    neoipcr:::log_dhis2_request(err, "events"))

  expect_match(lines, "status=404")
  expect_match(lines, "events", fixed = TRUE)
  expect_no_match(lines, "SENSITIVE_BODY_TOKEN", fixed = TRUE)
})

test_that("the DHIS2 query trace is gated by the log threshold", {
  resp <- fake_response()
  # At INFO (normal) the DEBUG query trace is suppressed ...
  expect_length(
    with_captured_neoipcr_logs(
      neoipcr:::log_dhis2_request(resp, "me"), threshold = logger::INFO),
    0L)
  # ... and at DEBUG (verbose) it appears.
  expect_length(
    with_captured_neoipcr_logs(
      neoipcr:::log_dhis2_request(resp, "me"), threshold = logger::DEBUG),
    1L)
})

test_that("neoipcr_log_config maps verbosity and reads NEOIPC_LOG_LEVEL", {
  prev <- logger::log_threshold(namespace = "neoipcr")
  withr::defer(logger::log_threshold(prev, namespace = "neoipcr"))

  expect_equal(neoipcr_log_config("quiet"),   logger::WARN)
  expect_equal(neoipcr_log_config("normal"),  logger::INFO)
  expect_equal(neoipcr_log_config("verbose"), logger::DEBUG)
  expect_equal(neoipcr_log_config("debug"),   logger::TRACE)
  # Unknown levels fall back to the silent-by-default INFO.
  expect_equal(neoipcr_log_config("bogus"),   logger::INFO)

  withr::local_envvar(NEOIPC_LOG_LEVEL = "debug")
  expect_equal(neoipcr_log_config(), logger::TRACE)
})
