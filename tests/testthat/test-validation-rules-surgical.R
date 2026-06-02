# Tests for R/validation-rules-surgical.R — rules 19, 22-24.

# --- Rule 19: SSI outside follow-up period (STUB) ---

test_that("rule 19 returns no rows (stub, not yet migrated)", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_19(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 19 detects SSI outside follow-up period", {
  skip("Rule 19 not yet migrated from Validation-Report")
  # When migrated: build ds with surgery at day 1 and SSI at day 35
  # (non-implant, >30 days = outside follow-up window)
})

test_that("rule 19 honours exceptions", {
  skip("Rule 19 not yet migrated from Validation-Report")
})

# --- Rules 22-24: invalid ICHE procedure codes ---
# These rules require ../ICHE-Health-Intervention-Codes.csv.
# Without it, they warn and return empty.

test_that("rule 22 returns empty when no surgery data", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_22(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 22 warns when ICHE file is missing", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "pro"),
    surgeryData = make_test_surgery_data(1L))
  expect_warning(
    result <- neoipcr:::validation_rule_22(ds, NULL),
    "ICHE")
  expect_equal(nrow(result), 0L)
})

test_that("rule 23 returns empty when no surgery data", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_23(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 24 returns empty when side_procedure_code_2 column absent", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_24(ds, NULL)
  # Returns NULL when side_procedure_code_2 is not in surgeryData
  expect_true(is.null(result) || nrow(result) == 0L)
})
