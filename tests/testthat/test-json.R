test_that("write_json emits plain JSON for a metadata-shaped list", {
  metadata <- list(
    calculated = as.POSIXct("2026-05-10 12:00:00", tz = "UTC"),
    surveillance_end_from = as.Date("2024-01-01"),
    surveillance_end_to = as.Date("2025-12-31"),
    birth_weight_from = NULL,
    countries = c("DE", "AT"),
    include_test_data = FALSE
  )

  json <- write_json(metadata)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$surveillance_end_from, "2024-01-01")
  expect_equal(parsed$surveillance_end_to, "2025-12-31")
  expect_equal(parsed$countries, c("DE", "AT"))
  expect_false(parsed$include_test_data)
  expect_match(parsed$calculated, "^2026-05-10T12:00:00")
})

test_that("write_json writes to a file when given a path", {
  path <- withr::local_tempfile(fileext = ".json")
  write_json(list(a = 1L, b = "two"), file = path)

  expect_true(file.exists(path))
  parsed <- jsonlite::fromJSON(path)
  expect_equal(parsed$a, 1L)
  expect_equal(parsed$b, "two")
})

test_that("write_json preserves character vectors as JSON arrays", {
  json <- write_json(list(countries = c("DE", "AT", "CH")))
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$countries, c("DE", "AT", "CH"))
})

test_that("write_json emits NA as null", {
  json <- write_json(list(value = NA_integer_))
  parsed <- jsonlite::fromJSON(json)
  expect_null(parsed$value)
})
