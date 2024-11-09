test_that("dhis2_connection_options defaults", {
  expect_equal(dhis2_connection_options("test_token")$base_url, "https://localhost/api")
})

test_that("dhis2_connection_options defaults", {
  expect_equal(dhis2_connection_options(token = "test_token", scheme = "http", hostname = "testhost", port = 8080, path = "/api/41")$base_url, "http://testhost:8080/api/41")
})

test_that("dhis2_connection_options fails if token and username are set", {
  expect_error(dhis2_connection_options(token = "test_token", username = "admin"), "Exactly one of `token` or `username` must be supplied.")
})

test_that("read_metadata_text reads required data", {
  metadata <- read_metadata('{"system":{"date":"2024-11-08T14:06:41.216+0000","id":"72c2bd70-573a-4d69-8bc3-f7bb431bdc23","rev":"3fcd748","version":"2.40.3.2"},"programs":[{"id": "D8mSSpOpsKj"}]}')

  expect_equal(metadata$system$date, readr::parse_datetime("2024-11-08T14:06:41.216+0000"))
  expect_equal(metadata$system$id, uuid::as.UUID("72c2bd70-573a-4d69-8bc3-f7bb431bdc23"))
  expect_equal(metadata$system$rev, "3fcd748")
  expect_equal(metadata$system$version, as.numeric_version("2.40.3.2"))
  expect_equal(metadata$programId, "D8mSSpOpsKj")
})
