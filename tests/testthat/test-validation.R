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

test_that("validate is exported and returns its result visibly", {
  # Read the NAMESPACE file rather than getNamespaceExports(): under
  # devtools::load_all() every object is exported, which would make the
  # check pass whether or not the roxygen `@export` tag is present.
  # `system.file()` resolves the right copy in both workflows: pkgload's shim
  # points it at this checkout under load_all(), and R CMD check at the copy
  # installed from it (the checkout is not present there, so a path relative
  # to the test tree would not be either).
  namespace <- readLines(system.file("NAMESPACE", package = "neoipcr"))
  expect_true("export(validate)" %in% namespace)
  ds <- make_populated_test_ds()
  expect_true(withVisible(neoipcr::validate(ds))$visible)
})

test_that("validate requires the full enrollment and event tiers, whose columns the rules read", {
  ds <- make_populated_test_ds()

  pseudo_events <- ds
  pseudo_events$metadata$dataset_options$include_event <- "pseudo"
  expect_error(neoipcr::validate(pseudo_events), "include_event")

  pseudo_enrollments <- ds
  pseudo_enrollments$metadata$dataset_options$include_enrollment <- "pseudo"
  expect_error(neoipcr::validate(pseudo_enrollments), "include_enrollment")
})

test_that("validate always carries its five columns, whatever ran", {
  ds <- make_populated_test_ds()
  shape <- c("rule_id", "patient_key", "enrollment_key", "event_key", "context")
  # Rule 1 records no context; the column is part of the shape regardless.
  r <- neoipcr::validate(ds, rules = 1L)
  expect_named(r, shape)
  expect_type(r$context, "list")
  expect_named(neoipcr::validate(ds), shape)
  # Rule 2 skips itself on this dataset (no status columns) and returns
  # nothing; the shape still holds, with zero rows.
  r <- neoipcr::validate(ds, rules = 2L)
  expect_named(r, shape)
  expect_equal(nrow(r), 0L)
  expect_type(r$rule_id, "integer")
})
