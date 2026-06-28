# Tests for R/validation-rules-enrollment.R — rules 1, 2, 17, 25, 26.

# --- Rule 1: patients without enrollment ---

test_that("rule 1 detects patient without enrollment", {
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(1, patient_keys = 1L))
  # Patient 2 has no enrollment
  result <- neoipcr:::validation_rule_1(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 1L)
  expect_equal(result$patient_key, 2L)
})

test_that("rule 1 returns no rows when all patients have enrollments", {
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 2L)))
  result <- neoipcr:::validation_rule_1(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 1 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(1, patient_keys = 1L))
  exceptions <- tibble::tibble(rule_id = 1L, patient_key = 2L)
  result <- neoipcr:::validation_rule_1(ds, exceptions)
  expect_equal(nrow(result), 0L)
})

# --- Rule 2: enrollment active but surveillance end completed ---

test_that("rule 2 detects active enrollment with completed end event", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("ACTIVE", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end",
      status = factor("COMPLETED",
        levels = c("ACTIVE", "COMPLETED", "VISITED",
                   "SCHEDULE", "OVERDUE", "SKIPPED"))))
  result <- neoipcr:::validation_rule_2(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 2L)
  expect_equal(result$enrollment_key, 1L)
})

test_that("rule 2 returns no rows on consistent data", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end",
      status = factor("COMPLETED",
        levels = c("ACTIVE", "COMPLETED", "VISITED",
                   "SCHEDULE", "OVERDUE", "SKIPPED"))))
  result <- neoipcr:::validation_rule_2(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 2 skips without a warning when status columns are absent", {
  ds <- make_populated_test_ds()
  # Default enrollments have status; remove it so the rule cannot run. A rule
  # that cannot run logs a debug diagnostic and returns — it must not warn.
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_2(ds, NULL))
  expect_null(result)
})

test_that("rule 2 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("ACTIVE", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end",
      status = factor("COMPLETED",
        levels = c("ACTIVE", "COMPLETED", "VISITED",
                   "SCHEDULE", "OVERDUE", "SKIPPED"))))
  # Rule 2 anti-joins on (rule_id, enrollment_key, event_key), so the
  # exception must carry the event_key for the violating enrollment.
  exceptions <- tibble::tibble(rule_id = 2L, enrollment_key = 1L, event_key = 1L)
  result <- neoipcr:::validation_rule_2(ds, exceptions)
  expect_equal(nrow(result), 0L)
})

# --- Rule 17: overlapping enrollments ---

test_that("rule 17 detects overlapping enrollments for same patient", {
  # Rule 17 builds a surveillance interval from each enrollment's enrolledAt
  # to its corresponding "end" event's occurredAt, then detects overlap. So
  # the test ds needs one "end" event per enrollment, not "adm".
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-05"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = rep("end", 2),
      occurredAt = as.Date(c("2024-01-10", "2024-01-15"))))
  result <- neoipcr:::validation_rule_17(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 17L)
})

test_that("rule 17 returns no rows for non-overlapping enrollments", {
  # Two enrollments for same patient, non-overlapping intervals
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-02-01"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = rep("end", 2),
      occurredAt = as.Date(c("2024-01-10", "2024-02-10"))))
  result <- neoipcr:::validation_rule_17(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 17 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-05"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = rep("end", 2),
      occurredAt = as.Date(c("2024-01-10", "2024-01-15"))))
  # Both enrollments are flagged (overlap is bidirectional)
  exceptions <- tibble::tibble(
    rule_id        = c(17L, 17L),
    enrollment_key = c(1L, 2L))
  result <- neoipcr:::validation_rule_17(ds, exceptions)
  expect_equal(nrow(result), 0L)
})

# --- Rule 25: completed enrollment without surveillance end event ---

test_that("rule 25 detects completed enrollment without end event", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm"))  # admission only, no end event
  result <- neoipcr:::validation_rule_25(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 25L)
  expect_equal(result$enrollment_key, 1L)
})

test_that("rule 25 returns no rows when end event exists", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end"))
  result <- neoipcr:::validation_rule_25(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 25 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm"))
  exceptions <- tibble::tibble(rule_id = 25L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_25(ds, exceptions)
  expect_equal(nrow(result), 0L)
})

# --- Rule 26: completed enrollment without admission event ---

test_that("rule 26 detects completed enrollment without admission event", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end"))  # end only, no admission
  result <- neoipcr:::validation_rule_26(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 26L)
  expect_equal(result$enrollment_key, 1L)
})

test_that("rule 26 returns no rows when admission event exists", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm"))
  result <- neoipcr:::validation_rule_26(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 26 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor("COMPLETED", levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end"))
  exceptions <- tibble::tibble(rule_id = 26L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_26(ds, exceptions)
  expect_equal(nrow(result), 0L)
})
