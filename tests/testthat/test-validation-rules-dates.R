# Tests for R/validation-rules-dates.R — rules 3, 4, 12-16.

# --- Rule 3: admission event date differs from enrollment date ---

test_that("rule 3 detects admission date != enrollment date", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      occurredAt = as.Date("2024-01-02")))  # different from enrollment
  result <- neoipcr:::validation_rule_3(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 3L)
})

test_that("rule 3 returns no rows when dates match", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      occurredAt = as.Date("2024-01-01")))
  result <- neoipcr:::validation_rule_3(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 3 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      occurredAt = as.Date("2024-01-02")))
  exc <- tibble::tibble(rule_id = 3L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_3(ds, exc)
  expect_equal(nrow(result), 0L)
})

# --- Rule 4: surveillance end date before admission date ---

test_that("rule 4 detects end date before admission date", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-10", "2024-01-05"))))  # end before adm
  result <- neoipcr:::validation_rule_4(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 4L)
})

test_that("rule 4 returns no rows when end is after admission", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-15"))))
  result <- neoipcr:::validation_rule_4(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 4 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-10", "2024-01-05"))))
  exc <- tibble::tibble(rule_id = 4L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_4(ds, exc)
  expect_equal(nrow(result), 0L)
})

# --- Rules 12-16: infection/surgery event date outside surveillance period ---
# Rule 12=bsi, 13=nec, 14=hap, 15=pro, 16=ssi
# NOTE: Rule 12 has a bug — sets rule_id = 13L instead of 12L.
#       Tests assert against actual (buggy) behavior.

# Helper: ds with admission + end + one event of given type outside the period
outside_period_ds <- function(event_type, event_date = "2024-02-01") {
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(3,
      enrollment_keys = c(1L, 1L, 1L),
      patient_keys    = c(1L, 1L, 1L),
      event_type_keys = c("adm", "end", event_type),
      occurredAt = as.Date(c("2024-01-01", "2024-01-15", event_date))))
}

within_period_ds <- function(event_type) {
  outside_period_ds(event_type, "2024-01-10")  # within Jan 1 - Jan 15
}

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

    test_that(paste0("rule ", r, " detects ", t, " event outside surveillance period"), {
      ds <- outside_period_ds(t)
      result <- f(ds, NULL)
      expect_true(nrow(result) > 0L)
      expect_equal(unique(result$rule_id), r)
    })

    test_that(paste0("rule ", r, " returns no rows when ", t, " event is within period"), {
      ds <- within_period_ds(t)
      result <- f(ds, NULL)
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      ds <- outside_period_ds(t)
      exc <- tibble::tibble(rule_id = r, event_key = 3L)
      result <- f(ds, exc)
      expect_equal(nrow(result), 0L)
    })
  })
}
