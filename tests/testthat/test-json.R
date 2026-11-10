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

test_that("write_json writes LF line endings and no BOM on every platform", {
  # R translates LF to CRLF in text mode on Windows, so passing a path straight to writeLines()
  # emits CRLF there. The bytes are read by other languages (the .NET reporting service, JS
  # tooling), so they must not depend on the platform that produced them. Asserted on the raw
  # bytes rather than via readLines(), which normalizes line endings and would pass either way.
  path <- withr::local_tempfile(fileext = ".json")
  write_json(list(a = 1L, b = "two"), file = path, pretty = TRUE)

  bytes <- readBin(path, "raw", file.size(path))

  expect_false(as.raw(0x0D) %in% bytes)
  expect_true(as.raw(0x0A) %in% bytes)
  expect_false(identical(bytes[seq_len(3)], as.raw(c(0xEF, 0xBB, 0xBF))))
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

test_that("a serialized reference table by infection type gives a share a type lacks as null", {
  # A sepsis and a pneumonia tested positive name an infectious agent; a
  # second pneumonia is not tested.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c("bsi", "hap", "hap"),
      microbiological_test_result = c(NA, "1", "-1")),
    findings = tibble::tibble(
      form = 1:2, secondary_bsi = FALSE, pathogen_key = 1L))

  json <- write_json(calculate_reference_data(ds, use_cache = FALSE)$
                       infectious_agent_detection_rate_per_inf_type_table)
  records <- jsonlite::parse_json(json)

  expect_equal(vapply(records, \(r) r$inf, character(1)),
               c("all", "bsi", "hap", "ssi"))
  hap <- records[[3]]
  expect_equal(c(hap$n, hap$tested, hap$total), c(1L, 1L, 2L))
  expect_equal(c(hap$pooled, hap$tested_pooled), c(100, 50))
  bsi <- records[[2]]
  expect_equal(c(bsi$n, bsi$tested, bsi$total), c(1L, 1L, 1L))
  expect_true(all(c("tested_pooled", "tested_q1") %in% names(bsi)))
  expect_null(bsi$tested_pooled)
  expect_null(bsi$tested_q1)
})

test_that("serialized reference metadata names neither the exception records nor the departments", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$department_filter <- "DEPT_ONLY"
  ds$metadata$dataset_options$include_invalid_patients <- tibble::tibble(
    RULE_ID           = 3L,
    NEOIPC_PATIENT_ID = "PAT_LISTED",
    ENROLMENT_DATE    = as.Date("2024-02-29"),
    EVENT_TYPE        = NA_character_,
    EVENT_DATE        = as.Date(NA))

  json <- write_json(calculate_reference_data(ds, use_cache = FALSE)$metadata)

  expect_false(grepl("PAT_LISTED", json, fixed = TRUE))
  expect_false(grepl("2024-02-29", json, fixed = TRUE))
  expect_false(grepl("DEPT_ONLY", json, fixed = TRUE))
  expect_true(grepl("exception_list_applied", json, fixed = TRUE))
})
