# Tests for R/dhis2-options.R — dhis2_dataset_options() constructor.

test_that("dhis2_dataset_options() serialises cleanly via jsonlite", {
  # The "list" entry in the object's class vector is what lets jsonlite (and the
  # package's write_json) serialise the dsopt as its underlying list without a
  # bespoke asJSON method. Before it was added this call errored with
  # "No method asJSON S3 class: neoipcr_dhis2_dsopt".
  expect_no_error(jsonlite::toJSON(dhis2_dataset_options()))
})

test_that("dhis2_dataset_options() keeps its S3 class first in the class vector", {
  # neoipcr_dhis2_dsopt must precede "list" so S3 dispatch keeps finding the
  # dsopt methods first; a well-meaning reorder would silently change dispatch.
  expect_identical(
    class(dhis2_dataset_options()),
    c("neoipcr_dhis2_dsopt", "list"))
})

test_that("include_custom_attributes defaults to empty and accepts the org-unit entities", {
  expect_identical(dhis2_dataset_options()$include_custom_attributes, character())
  expect_identical(
    dhis2_dataset_options(include_custom_attributes = "departments")$include_custom_attributes,
    "departments")
  expect_setequal(
    dhis2_dataset_options(
      include_custom_attributes = c("hospitals", "departments"))$include_custom_attributes,
    c("departments", "hospitals"))
})

test_that("reconcile defaults to TRUE and takes a single logical only", {
  expect_true(dhis2_dataset_options()$reconcile)
  expect_false(dhis2_dataset_options(reconcile = FALSE)$reconcile)
  expect_error(dhis2_dataset_options(reconcile = "yes"), "reconcile")
  expect_error(dhis2_dataset_options(reconcile = NA), "reconcile")
  expect_error(dhis2_dataset_options(reconcile = c(TRUE, FALSE)), "reconcile")
})

test_that("reconcile leaves the package in a calculated dataset's options as it is", {
  # A boolean names no record, so it needs no marker on its way out.
  for (value in c(TRUE, FALSE)) {
    opts <- dhis2_dataset_options(reconcile = value)
    for (keep in c(TRUE, FALSE)) {
      emitted <- neoipcr:::serializable_dataset_options(
        opts, keep_department_filter = keep)
      expect_identical(emitted$reconcile, value)
      expect_no_error(neoipcr:::assert_serializable_dataset_options(
        emitted, allow_department_filter = keep))
    }
    expect_match(
      as.character(jsonlite::toJSON(opts)),
      sprintf('"reconcile":[%s]', tolower(value)), fixed = TRUE)
  }
})

test_that("include_custom_attributes rejects an entity it does not know", {
  expect_error(dhis2_dataset_options(include_custom_attributes = "countries"))
  expect_error(dhis2_dataset_options(include_custom_attributes = "users"))
})
