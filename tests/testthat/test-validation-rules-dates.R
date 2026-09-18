# Tests for R/validation-rules-dates.R — rules 3, 4, 12-16.

# --- Rule 3: admission event date differs from enrollment date ---

rule_3_ds <- function(admission_date)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      occurredAt = as.Date(admission_date)))

test_that("rule 3 detects admission date != enrollment date", {
  result <- neoipcr:::validation_rule_3(rule_3_ds("2024-01-02"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 3L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 1L)
  expect_s3_class(result$context[[1]], "tbl_df")
  expect_named(result$context[[1]], c("enrolledAt", "occurredAt"))
  expect_equal(result$context[[1]]$occurredAt, as.Date("2024-01-02"))
})

test_that("rule 3 returns no rows when dates match", {
  expect_equal(nrow(neoipcr:::validation_rule_3(rule_3_ds("2024-01-01"), NULL)), 0L)
})

test_that("rule 3 honours exceptions", {
  result <- neoipcr:::validation_rule_3(
    rule_3_ds("2024-01-02"), make_test_exceptions(3L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

# --- Rule 4: surveillance end date before admission date ---

rule_4_ds <- function(admission_date, end_date)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c(admission_date, end_date))))

test_that("rule 4 detects end date before admission date", {
  result <- neoipcr:::validation_rule_4(rule_4_ds("2024-01-10", "2024-01-05"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 4L)
  # The finding names the end event.
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("admOccurredAt", "endOccurredAt"))
})

test_that("rule 4 returns no rows when end is after admission", {
  expect_equal(nrow(neoipcr:::validation_rule_4(rule_4_ds("2024-01-01", "2024-01-15"), NULL)), 0L)
})

test_that("rule 4 honours exceptions", {
  result <- neoipcr:::validation_rule_4(
    rule_4_ds("2024-01-10", "2024-01-05"), make_test_exceptions(4L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

# --- Rules 12-16: infection/surgery event date outside the enrolment window ---
# Rule 12=bsi, 13=nec, 14=hap, 15=pro, 16=ssi. The window runs from the later
# of the enrolment date and the admission event to the surveillance-end event.

window_ds <- function(event_type, event_date, admission_date = "2024-01-01")
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(3,
      enrollment_keys = c(1L, 1L, 1L),
      patient_keys    = c(1L, 1L, 1L),
      event_type_keys = c("adm", "end", event_type),
      occurredAt = as.Date(c(admission_date, "2024-01-15", event_date))))

date_rules <- list(
  list(rule = 12L, type = "bsi", fun = neoipcr:::validation_rule_12),
  list(rule = 13L, type = "nec", fun = neoipcr:::validation_rule_13),
  list(rule = 14L, type = "hap", fun = neoipcr:::validation_rule_14),
  list(rule = 15L, type = "pro", fun = neoipcr:::validation_rule_15),
  list(rule = 16L, type = "ssi", fun = neoipcr:::validation_rule_16)
)

for (entry in date_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun

    test_that(paste0("rule ", r, " detects ", t, " event after the surveillance end"), {
      result <- f(window_ds(t, "2024-02-01"), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 3L)
      expect_named(
        result$context[[1]],
        c("enrolledAt", "admOccurredAt", "endOccurredAt", paste0(t, "OccurredAt")))
      expect_equal(result$context[[1]][[paste0(t, "OccurredAt")]], as.Date("2024-02-01"))
    })

    test_that(paste0("rule ", r, " detects ", t, " event before the admission event"), {
      # The enrolment starts on the first, the admission form is dated the
      # third: an event on the second is inside the enrolment but before the
      # admission, and is a finding.
      result <- f(window_ds(t, "2024-01-02", admission_date = "2024-01-03"), NULL)
      expect_equal(nrow(result), 1L)
    })

    test_that(paste0("rule ", r, " returns no rows when ", t, " event is within the window"), {
      expect_equal(nrow(f(window_ds(t, "2024-01-10"), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " counts the admission and surveillance-end days as inside the window"), {
      expect_equal(nrow(f(window_ds(t, "2024-01-01"), NULL)), 0L)
      expect_equal(nrow(f(window_ds(t, "2024-01-15"), NULL)), 0L)
      expect_equal(nrow(f(window_ds(t, "2024-01-16"), NULL)), 1L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(window_ds(t, "2024-02-01"), make_test_exceptions(r, event_key = 3L))
      expect_equal(nrow(result), 0L)
    })
  })
}
