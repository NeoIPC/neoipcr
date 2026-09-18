# Tests for R/validation-rules-enrollment.R — rules 1, 2, 17, 25, 26.

enrollment_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "CANCELLED"))

event_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))

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
  expect_equal(result$enrollment_key, NA_integer_)
  expect_equal(result$event_key, NA_integer_)
  # A patient-level finding has no values to report.
  expect_null(result$context[[1]])
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
  result <- neoipcr:::validation_rule_1(
    ds, make_test_exceptions(1L, patient_key = 2L))
  expect_equal(nrow(result), 0L)
})

# --- Rule 2: enrollment active but surveillance end completed ---

rule_2_ds <- function(enrollment = "ACTIVE", end = "COMPLETED")
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = enrollment_status(enrollment)),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end",
      status = event_status(end)))

test_that("rule 2 detects active enrollment with completed end event", {
  result <- neoipcr:::validation_rule_2(rule_2_ds(), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 2L)
  expect_equal(result$enrollment_key, 1L)
  # The finding names the end event that closed the record.
  expect_equal(result$event_key, 1L)
  expect_null(result$context[[1]])
})

test_that("rule 2 returns no rows on consistent data", {
  expect_equal(nrow(neoipcr:::validation_rule_2(rule_2_ds("COMPLETED", "COMPLETED"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_2(rule_2_ds("ACTIVE", "ACTIVE"), NULL)), 0L)
})

test_that("rule 2 treats records without a status column as completed", {
  # The enrolment column is absent when only completed enrolments were
  # imported, so none of them is active and the rule finds nothing — without
  # a warning.
  ds <- rule_2_ds()
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_2(ds, NULL))
  expect_equal(nrow(result), 0L)
  # The event column is absent when only completed events were imported, so
  # the end event of an active enrolment counts as completed and is found.
  ds <- rule_2_ds("ACTIVE", "ACTIVE")
  ds$events$status <- NULL
  expect_equal(nrow(neoipcr:::validation_rule_2(ds, NULL)), 1L)
})

test_that("rule 2 honours exceptions", {
  result <- neoipcr:::validation_rule_2(
    rule_2_ds(), make_test_exceptions(2L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

# --- Rule 17: overlapping enrollments ---

rule_17_ds <- function(enrolled_at, ended_at)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(enrolled_at)),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = rep("end", 2),
      occurredAt = as.Date(ended_at)))

test_that("rule 17 detects overlapping enrollments for the same patient", {
  # Each enrolment's interval runs from its enrolment date to its end event.
  ds <- rule_17_ds(c("2024-01-01", "2024-01-05"), c("2024-01-10", "2024-01-15"))
  result <- neoipcr:::validation_rule_17(ds, NULL)
  # The overlap is found from both sides, one finding per enrolment.
  expect_equal(nrow(result), 2L)
  expect_equal(unique(result$rule_id), 17L)
  expect_setequal(result$enrollment_key, c(1L, 2L))
  expect_true(all(is.na(result$event_key)))
  expect_named(
    result$context[[1]],
    c("enrolledAt_this", "endOccurredAt_this", "enrolledAt_other", "endOccurredAt_other"))
  this <- result$context[[which(result$enrollment_key == 1L)]]
  expect_equal(this$enrolledAt_this, as.Date("2024-01-01"))
  expect_equal(this$endOccurredAt_other, as.Date("2024-01-15"))
})

test_that("rule 17 returns no rows for non-overlapping enrollments", {
  ds <- rule_17_ds(c("2024-01-01", "2024-02-01"), c("2024-01-10", "2024-02-10"))
  expect_equal(nrow(neoipcr:::validation_rule_17(ds, NULL)), 0L)
})

test_that("rule 17 counts the surveillance-end day as part of the period", {
  # A period ending on the day the next one begins overlaps it; one ending
  # the day before does not.
  expect_equal(nrow(neoipcr:::validation_rule_17(
    rule_17_ds(c("2024-01-01", "2024-01-10"), c("2024-01-10", "2024-01-20")), NULL)), 2L)
  expect_equal(nrow(neoipcr:::validation_rule_17(
    rule_17_ds(c("2024-01-01", "2024-01-11"), c("2024-01-10", "2024-01-20")), NULL)), 0L)
})

test_that("rule 17 records one finding per overlapping pair", {
  # Three enrolments of one patient that all overlap: each is found twice,
  # once with each partner's dates, so a reader sees which pair overlaps.
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(3,
      patient_keys = c(1L, 1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-05", "2024-01-08"))),
    events = make_test_events(3,
      enrollment_keys = c(1L, 2L, 3L),
      patient_keys    = c(1L, 1L, 1L),
      event_type_keys = rep("end", 3),
      occurredAt = as.Date(c("2024-01-10", "2024-01-15", "2024-01-20"))))
  result <- neoipcr:::validation_rule_17(ds, NULL)
  expect_equal(nrow(result), 6L)
  expect_equal(as.vector(table(result$enrollment_key)), c(2L, 2L, 2L))
  partners <- vapply(
    result$context[result$enrollment_key == 1L],
    \(ctx) format(ctx$enrolledAt_other), character(1))
  expect_setequal(partners, c("2024-01-05", "2024-01-08"))
})

test_that("rule 17 compares the enrolments of one patient only", {
  # Two patients whose surveillance periods overlap are not each other's
  # finding.
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 2L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-05"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 2L),
      event_type_keys = rep("end", 2),
      occurredAt = as.Date(c("2024-01-10", "2024-01-15"))))
  expect_equal(nrow(neoipcr:::validation_rule_17(ds, NULL)), 0L)
})

# Two enrolments of one patient, the first closed by an end event on
# `first_ends`, the second without an end event.
rule_17_open_ds <- function(enrolled_at, first_ends = NULL)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(enrolled_at)),
    events = if (is.null(first_ends))
      make_test_events(1, enrollment_keys = 1L, patient_keys = 1L, event_type_keys = "adm")
    else
      make_test_events(1, enrollment_keys = 1L, patient_keys = 1L, event_type_keys = "end",
                       occurredAt = as.Date(first_ends)))

test_that("rule 17 finds an open enrolment by its enrolment date", {
  # An enrolment without an end event is under surveillance on its
  # enrolment date: found when that day lies inside another enrolment's
  # period, not when it lies after it.
  result <- neoipcr:::validation_rule_17(
    rule_17_open_ds(c("2024-01-01", "2024-01-05"), first_ends = "2024-01-10"), NULL)
  expect_equal(nrow(result), 2L)
  this <- result$context[[which(result$enrollment_key == 2L)]]
  expect_true(is.na(this$endOccurredAt_this))
  expect_equal(this$endOccurredAt_other, as.Date("2024-01-10"))
  expect_equal(nrow(neoipcr:::validation_rule_17(
    rule_17_open_ds(c("2024-01-01", "2024-01-11"), first_ends = "2024-01-10"), NULL)), 0L)
  # Two open enrolments are found when they share the day, and only then.
  expect_equal(nrow(neoipcr:::validation_rule_17(
    rule_17_open_ds(c("2024-01-01", "2024-01-05")), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_17(
    rule_17_open_ds(c("2024-01-01", "2024-01-01")), NULL)), 2L)
})

test_that("rule 17 honours exceptions", {
  ds <- rule_17_ds(c("2024-01-01", "2024-01-05"), c("2024-01-10", "2024-01-15"))
  # Both enrollments are flagged (overlap is bidirectional)
  result <- neoipcr:::validation_rule_17(
    ds, make_test_exceptions(c(17L, 17L), enrollment_key = c(1L, 2L)))
  expect_equal(nrow(result), 0L)
  # Exempting one leaves the other's finding standing.
  result <- neoipcr:::validation_rule_17(
    ds, make_test_exceptions(17L, enrollment_key = 1L))
  expect_equal(result$enrollment_key, 2L)
})

# --- Rules 25 and 26: completed enrollment without end / admission event ---

rule_25_26_ds <- function(event_type, enrollment = "COMPLETED")
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = enrollment_status(enrollment)),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = event_type))

test_that("rule 25 detects completed enrollment without end event", {
  result <- neoipcr:::validation_rule_25(rule_25_26_ds("adm"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 25L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, NA_integer_)
  expect_null(result$context[[1]])
})

test_that("rule 25 returns no rows when end event exists or the enrolment is open", {
  expect_equal(nrow(neoipcr:::validation_rule_25(rule_25_26_ds("end"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_25(rule_25_26_ds("adm", "ACTIVE"), NULL)), 0L)
})

test_that("rule 25 treats enrolments without a status column as completed", {
  # Only completed enrolments were imported, so every one without an end
  # event is a finding.
  ds <- rule_25_26_ds("adm", "ACTIVE")
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_25(ds, NULL))
  expect_equal(nrow(result), 1L)
})

test_that("rule 25 honours exceptions", {
  result <- neoipcr:::validation_rule_25(
    rule_25_26_ds("adm"), make_test_exceptions(25L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 26 detects completed enrollment without admission event", {
  result <- neoipcr:::validation_rule_26(rule_25_26_ds("end"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 26L)
  expect_equal(result$enrollment_key, 1L)
  expect_null(result$context[[1]])
})

test_that("rule 26 returns no rows when admission event exists or the enrolment is open", {
  expect_equal(nrow(neoipcr:::validation_rule_26(rule_25_26_ds("adm"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_26(rule_25_26_ds("end", "ACTIVE"), NULL)), 0L)
})

test_that("rule 26 treats enrolments without a status column as completed", {
  ds <- rule_25_26_ds("end", "ACTIVE")
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_26(ds, NULL))
  expect_equal(nrow(result), 1L)
})

test_that("rule 26 honours exceptions", {
  result <- neoipcr:::validation_rule_26(
    rule_25_26_ds("end"), make_test_exceptions(26L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})
