# Tests for R/validation-rules-surveillance-end.R — rules 18, 21.

# One enrolment from 2024-01-01 with an admission event (key 1) and an end
# event on 2024-01-11 (key 2), whose surveillance-end form takes `...`.
surveillance_end_ds <- function(..., substance_days = NULL)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-11"))),
    surveillanceEndData = make_test_surveillance_end_data(event_keys = 2L, ...),
    substanceDays = if (is.null(substance_days))
      make_test_substance_days(integer(0))
    else
      make_test_substance_days(rep(2L, length(substance_days)), days = substance_days))

# --- Rule 18: patient days validation ---

test_that("rule 18 detects patient_days mismatch", {
  # Formula: patient_days_calculated = 1 + (end_date - enrollment_date)
  # enrollment Jan 1 → end Jan 11 → 1 + 10 = 11
  result <- neoipcr:::validation_rule_18(surveillance_end_ds(patient_days = 999L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 18L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("patient_days", "patient_days_calculated"))
  expect_equal(result$context[[1]]$patient_days_calculated, 11L)
})

test_that("rule 18 returns no rows when patient_days is correct", {
  expect_equal(nrow(neoipcr:::validation_rule_18(surveillance_end_ds(patient_days = 11L), NULL)), 0L)
})

test_that("rule 18 detects missing (NA) patient_days", {
  # patient_days is compulsory + auto-calculated in DHIS2; a missing value
  # bypassed that calculation and must be flagged. Without the is.na() guard
  # the `NA != calculated` comparison is NA and filter() would drop it.
  result <- neoipcr:::validation_rule_18(surveillance_end_ds(patient_days = NA_integer_), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 18L)
  expect_true(1L %in% result$patient_key)
})

test_that("rule 18 honours exceptions", {
  result <- neoipcr:::validation_rule_18(
    surveillance_end_ds(patient_days = 999L), make_test_exceptions(18L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 18 skips without a warning when patient_days is absent", {
  ds <- surveillance_end_ds(patient_days = 999L)
  ds$surveillanceEndData$patient_days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_18(ds, NULL))
  expect_null(result)
})

# --- Rule 21: substance days sum to less than the antibiotic days ---

test_that("rule 21 detects substance days short of the antibiotic days", {
  result <- neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L)), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 21L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("ab_substance_days", "ab_days"))
  expect_equal(result$context[[1]]$ab_substance_days, 4L)
  expect_equal(result$context[[1]]$ab_days, 5L)
})

test_that("rule 21 counts antibiotic days with no substance recorded as a shortfall", {
  result <- neoipcr:::validation_rule_21(surveillance_end_ds(ab_days = 5L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$ab_substance_days, 0L)
})

test_that("rule 21 returns no rows when the substance days reach or exceed the antibiotic days", {
  # Combination therapy: several substances on one day exceed the count.
  expect_equal(nrow(neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(3L, 2L)), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(5L, 5L)), NULL)), 0L)
})

test_that("rule 21 does not check a record without antibiotic days", {
  expect_equal(nrow(neoipcr:::validation_rule_21(surveillance_end_ds(ab_days = 0L), NULL)), 0L)
})

test_that("rule 21 honours exceptions", {
  result <- neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L)),
    make_test_exceptions(21L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 21 skips without a warning when the substance days or the antibiotic days are absent", {
  ds <- surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L))
  ds$substanceDays$days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_21(ds, NULL))
  expect_null(result)
  ds <- surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L))
  ds$surveillanceEndData$ab_days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_21(ds, NULL))
  expect_null(result)
})
