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
  expect_declared_context(result)
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
  ds <- rule_2_ds()
  result <- neoipcr:::validation_rule_2(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
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
  expect_declared_context(result)
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

test_that("rule 17 records one finding for each enrolment of an overlapping pair", {
  # Three enrolments of one patient that all overlap: each is found twice,
  # once with each partner's dates, so every overlap is seen from either side.
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

test_that("rule 17 treats an end event without a date like a missing one", {
  # An undated end form leaves the period's end unknown, so the enrolment
  # counts as under surveillance on its enrolment date only.
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-05"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("end", "end"),
      occurredAt = as.Date(c("2024-01-10", NA))))
  result <- neoipcr:::validation_rule_17(ds, NULL)
  expect_equal(nrow(result), 2L)
  this <- result$context[[which(result$enrollment_key == 2L)]]
  expect_true(is.na(this$endOccurredAt_this))
  ds$enrollments$enrolledAt[2] <- as.Date("2024-01-11")
  expect_equal(nrow(neoipcr:::validation_rule_17(ds, NULL)), 0L)
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
  expect_declared_context(result)
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
  expect_declared_context(result)
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

# --- Rules 43 and 44: enrolment still open long after its enrolment date ---

open_enrolment_as_of <- as.Date("2025-01-01")

# One patient enrolled `days_before` the reference date, with a completed
# admission event and a surveillance-end event of the given status, or none.
open_enrolment_ds <- function(days_before, enrollment = "ACTIVE", end = NULL) {
  events <- if (is.null(end))
    make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      status = event_status("COMPLETED"))
  else
    make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "end",
      status = event_status(end))
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = open_enrolment_as_of - days_before,
      status = enrollment_status(enrollment)),
    events = events)
}

event_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))

test_that("rule 43 detects an active enrolment without an end event long after its enrolment date", {
  result <- neoipcr:::validation_rule_43(open_enrolment_ds(121), NULL, open_enrolment_as_of)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 43L)
  expect_equal(result$enrollment_key, 1L)
  expect_true(is.na(result$event_key))
  expect_equal(result$context[[1]]$enrolledAt, open_enrolment_as_of - 121)
  expect_equal(result$context[[1]]$days_open, 121L)
})

test_that("rule 43 questions an enrolment only past the threshold", {
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(120), NULL, open_enrolment_as_of)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(121), NULL, open_enrolment_as_of)), 1L)
  # The same enrolment measured against an earlier reading is not yet due.
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(121), NULL, open_enrolment_as_of - 10)), 0L)
})

test_that("rule 43 returns no rows for a completed enrolment or one with an end event", {
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(400, "COMPLETED"), NULL, open_enrolment_as_of)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(400, "CANCELLED"), NULL, open_enrolment_as_of)), 0L)
  # An end event of any status is rule 2's or 44's concern, not this one's.
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(400, end = "ACTIVE"), NULL, open_enrolment_as_of)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_43(open_enrolment_ds(400, end = "COMPLETED"), NULL, open_enrolment_as_of)), 0L)
})

test_that("rule 43 treats enrolments without a status column as completed", {
  ds <- open_enrolment_ds(400)
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_43(ds, NULL, open_enrolment_as_of))
  expect_equal(nrow(result), 0L)
})

test_that("rule 43 honours exceptions", {
  result <- neoipcr:::validation_rule_43(
    open_enrolment_ds(400), make_test_exceptions(43L, enrollment_key = 1L), open_enrolment_as_of)
  expect_equal(nrow(result), 0L)
})

test_that("rule 44 detects an active enrolment with an open end event long after its enrolment date", {
  ds <- open_enrolment_ds(200, end = "ACTIVE")
  result <- neoipcr:::validation_rule_44(ds, NULL, open_enrolment_as_of)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 44L)
  expect_equal(result$enrollment_key, 1L)
  # The finding names the end event that is still open.
  expect_equal(result$event_key, 1L)
  expect_equal(as.character(result$context[[1]]$status), "ACTIVE")
  expect_equal(result$context[[1]]$days_open, 200L)
})

test_that("rule 44 questions an enrolment only past the threshold", {
  expect_equal(nrow(neoipcr:::validation_rule_44(open_enrolment_ds(120, end = "ACTIVE"), NULL, open_enrolment_as_of)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_44(open_enrolment_ds(121, end = "ACTIVE"), NULL, open_enrolment_as_of)), 1L)
})

test_that("rule 44 returns no rows when the end event is completed, the enrolment is not active, or there is no end event", {
  # A completed end event on an active enrolment is rule 2's finding.
  expect_equal(nrow(neoipcr:::validation_rule_44(open_enrolment_ds(400, end = "COMPLETED"), NULL, open_enrolment_as_of)), 0L)
  # An open end event on a completed enrolment is rule 6's.
  expect_equal(nrow(neoipcr:::validation_rule_44(open_enrolment_ds(400, "COMPLETED", end = "ACTIVE"), NULL, open_enrolment_as_of)), 0L)
  # No end event at all is rule 43's.
  expect_equal(nrow(neoipcr:::validation_rule_44(open_enrolment_ds(400), NULL, open_enrolment_as_of)), 0L)
})

test_that("rule 44 treats records without a status column as completed", {
  ds <- open_enrolment_ds(400, end = "ACTIVE")
  ds$enrollments$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_44(ds, NULL, open_enrolment_as_of))
  expect_equal(nrow(result), 0L)
  ds <- open_enrolment_ds(400, end = "ACTIVE")
  ds$events$status <- NULL
  expect_equal(nrow(neoipcr:::validation_rule_44(ds, NULL, open_enrolment_as_of)), 0L)
})

test_that("rule 44 honours exceptions", {
  result <- neoipcr:::validation_rule_44(
    open_enrolment_ds(400, end = "ACTIVE"), make_test_exceptions(44L, enrollment_key = 1L), open_enrolment_as_of)
  expect_equal(nrow(result), 0L)
})
