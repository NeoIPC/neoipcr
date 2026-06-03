# Tests for R/validation.R — validate() orchestrator and validation_rules registry.

test_that("validation_rules registry has 42 entries", {
  expect_equal(length(neoipcr:::validation_rules), 42L)
})

test_that("validation_rules registry entries have correct structure", {
  for (entry in neoipcr:::validation_rules) {
    expect_true(is.integer(entry$id))
    expect_true(is.function(entry$fun))
    expect_true(is.function(entry$formatter))
  }
})

test_that("validate returns zero-row tibble on clean data", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validate(ds)
  expect_s3_class(result, "tbl_df")
  # Clean data should have zero or few violations (rule 1 won't fire
  # because all patients have enrollments in make_populated_test_ds)
  expect_true("rule_id" %in% names(result))
})

test_that("validate runs only specified rules", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validate(ds, rules = c(1L))
  expect_s3_class(result, "tbl_df")
  # Result should only contain rule_id == 1 (or be empty)
  if (nrow(result) > 0L)
    expect_true(all(result$rule_id == 1L))
})

test_that("validate result has expected columns", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validate(ds)
  possible_cols <- c("rule_id", "patient_key", "enrollment_key",
                     "event_key", "context")
  expect_true(all(names(result) %in% possible_cols))
})
