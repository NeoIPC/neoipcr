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

test_that("include_custom_attributes rejects an entity it does not know", {
  expect_error(dhis2_dataset_options(include_custom_attributes = "countries"))
  expect_error(dhis2_dataset_options(include_custom_attributes = "users"))
})
