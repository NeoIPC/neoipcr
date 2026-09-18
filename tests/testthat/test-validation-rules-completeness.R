# Tests for R/validation-rules-completeness.R — rules 5-11.
# All rules check for forms that are not completed on records that are.

enrollment_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "CANCELLED"))

event_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))

# One enrolment with one event of `event_type`; with `end_status` set, a
# surveillance-end event of that status as well (event key 2).
completeness_ds <- function(event_type, event_status_value = "ACTIVE",
                            enrollment_status_value = "COMPLETED",
                            end_status = NULL) {
  types    <- c(event_type, if (!is.null(end_status)) "end")
  statuses <- c(event_status_value, end_status)
  n <- length(types)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = enrollment_status(enrollment_status_value)),
    events = make_test_events(n,
      enrollment_keys = rep(1L, n),
      patient_keys    = rep(1L, n),
      event_type_keys = types,
      status = event_status(statuses)))
}

# --- Rule 5: incomplete admission event ---

test_that("rule 5 detects an admission event that is not completed", {
  # The enrolment's own status does not matter for the admission form.
  result <- neoipcr:::validation_rule_5(completeness_ds("adm", "ACTIVE", "ACTIVE"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 5L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "status")
  expect_equal(as.character(result$context[[1]]$status), "ACTIVE")
})

test_that("rule 5 returns no rows on completed admission", {
  expect_equal(nrow(neoipcr:::validation_rule_5(completeness_ds("adm", "COMPLETED"), NULL)), 0L)
})

test_that("rule 5 honours exceptions", {
  result <- neoipcr:::validation_rule_5(
    completeness_ds("adm", "ACTIVE"), make_test_exceptions(5L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 5 treats events without a status column as completed", {
  # The column is absent when only completed events were imported, so no
  # form is open and the rule finds nothing — without a warning.
  ds <- completeness_ds("adm", "ACTIVE")
  ds$events$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_5(ds, NULL))
  expect_equal(nrow(result), 0L)
})

# --- Rule 6: completed enrollment with incomplete surveillance-end event ---

test_that("rule 6 detects an open surveillance-end form on a completed enrolment", {
  result <- neoipcr:::validation_rule_6(completeness_ds("end", "ACTIVE", "COMPLETED"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 6L)
  expect_named(result$context[[1]], "status")
})

test_that("rule 6 returns no rows when the form is completed or the enrolment is open", {
  expect_equal(nrow(neoipcr:::validation_rule_6(completeness_ds("end", "COMPLETED", "COMPLETED"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_6(completeness_ds("end", "ACTIVE", "ACTIVE"), NULL)), 0L)
})

test_that("rule 6 honours exceptions", {
  result <- neoipcr:::validation_rule_6(
    completeness_ds("end", "ACTIVE", "COMPLETED"), make_test_exceptions(6L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 6 treats records without a status column as completed", {
  ds <- completeness_ds("end", "ACTIVE", "COMPLETED")
  ds$events$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_6(ds, NULL))
  expect_equal(nrow(result), 0L)
  # Without the enrolment status every enrolment counts as completed, so an
  # open surveillance-end form is a finding whatever the enrolment says.
  ds <- completeness_ds("end", "ACTIVE", "ACTIVE")
  ds$enrollments$status <- NULL
  expect_equal(nrow(neoipcr:::validation_rule_6(ds, NULL)), 1L)
})

# --- Rules 7-11: infection or surgery form not completed on a completed record ---
# Rule 7 = bsi, 8 = nec, 9 = hap, 10 = pro, 11 = ssi. Either the enrolment or
# its surveillance-end form being completed closes the record.

form_rules <- list(
  list(rule = 7L,  type = "bsi", fun = neoipcr:::validation_rule_7),
  list(rule = 8L,  type = "nec", fun = neoipcr:::validation_rule_8),
  list(rule = 9L,  type = "hap", fun = neoipcr:::validation_rule_9),
  list(rule = 10L, type = "pro", fun = neoipcr:::validation_rule_10),
  list(rule = 11L, type = "ssi", fun = neoipcr:::validation_rule_11)
)

for (entry in form_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun
    status_col <- paste0(t, "_status")

    test_that(paste0("rule ", r, " detects an open ", t, " form on a completed enrolment"), {
      result <- f(completeness_ds(t, "ACTIVE", "COMPLETED"), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 1L)
      expect_named(result$context[[1]], c("enrollment_status", "end_status", status_col))
      expect_equal(as.character(result$context[[1]][[status_col]]), "ACTIVE")
      # No surveillance-end event: its status is absent, not a value.
      expect_true(is.na(result$context[[1]]$end_status))
    })

    test_that(paste0("rule ", r, " detects an open ", t, " form on an active enrolment whose surveillance end is completed"), {
      result <- f(completeness_ds(t, "ACTIVE", "ACTIVE", end_status = "COMPLETED"), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(as.character(result$context[[1]]$enrollment_status), "ACTIVE")
      expect_equal(as.character(result$context[[1]]$end_status), "COMPLETED")
    })

    test_that(paste0("rule ", r, " returns no rows while the record is still open"), {
      expect_equal(nrow(f(completeness_ds(t, "ACTIVE", "ACTIVE"), NULL)), 0L)
      expect_equal(nrow(f(completeness_ds(t, "ACTIVE", "ACTIVE", end_status = "ACTIVE"), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " returns no rows when the ", t, " form is completed"), {
      expect_equal(nrow(f(completeness_ds(t, "COMPLETED", "COMPLETED"), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " finds each open ", t, " form of an enrolment on its own"), {
      # Two forms of one type on a completed enrolment, one of them
      # completed: one finding, naming the open form's event.
      ds <- make_test_ds(
        patients    = make_test_patients(1),
        enrollments = make_test_enrollments(1,
          patient_keys = 1L,
          status = enrollment_status("COMPLETED")),
        events = make_test_events(2,
          enrollment_keys = c(1L, 1L),
          patient_keys    = c(1L, 1L),
          event_type_keys = c(t, t),
          status = event_status(c("COMPLETED", "ACTIVE"))))
      result <- f(ds, NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$event_key, 2L)
    })

    test_that(paste0("rule ", r, " honours exceptions on the event"), {
      result <- f(completeness_ds(t, "ACTIVE", "COMPLETED"), make_test_exceptions(r, event_key = 1L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " treats records without a status column as completed"), {
      ds <- completeness_ds(t, "ACTIVE", "COMPLETED")
      ds$events$status <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_equal(nrow(result), 0L)
      # Without the enrolment status every enrolment counts as completed.
      ds <- completeness_ds(t, "ACTIVE", "ACTIVE")
      ds$enrollments$status <- NULL
      result <- f(ds, NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(as.character(result$context[[1]]$enrollment_status), "COMPLETED")
    })
  })
}
