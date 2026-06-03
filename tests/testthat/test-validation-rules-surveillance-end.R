# Tests for R/validation-rules-surveillance-end.R — rules 18, 21.

# --- Rule 18: patient days validation ---

test_that("rule 18 detects patient_days mismatch", {
  # Build ds where patient_days != date difference
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-11"))),
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = 2L,
      patient_days = 999L))  # Wrong: should be ~10
  result <- neoipcr:::validation_rule_18(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 18L)
})

test_that("rule 18 returns no rows when patient_days is correct", {
  # Formula: patient_days_calculated = 1 + (end_date - enrollment_date)
  # enrollment Jan 1 → end Jan 11 → 1 + 10 = 11
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-11"))),
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = 2L,
      patient_days = 11L))
  result <- neoipcr:::validation_rule_18(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 18 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-11"))),
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = 2L,
      patient_days = 999L))
  exc <- tibble::tibble(rule_id = 18L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_18(ds, exc)
  expect_equal(nrow(result), 0L)
})

# --- Rule 21: substance days < total AB days (STUB) ---

test_that("rule 21 returns no rows (stub, not yet migrated)", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_21(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 21 detects substance days < total AB days", {
  skip("Rule 21 not yet migrated from Validation-Report")
  # When migrated: build ds where sum of substance_days < ab_days
})

test_that("rule 21 honours exceptions", {
  skip("Rule 21 not yet migrated from Validation-Report")
})
