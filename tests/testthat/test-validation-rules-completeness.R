# Tests for R/validation-rules-completeness.R — rules 5-11.
# All rules check for incomplete events within enrollments.

# Helper: build a ds with one enrollment and one event of given type and status
completeness_ds <- function(event_type, event_status = "ACTIVE",
                            enrollment_status = "COMPLETED") {
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      status = factor(enrollment_status,
        levels = c("ACTIVE", "COMPLETED", "CANCELLED"))),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = event_type,
      status = factor(event_status,
        levels = c("ACTIVE", "COMPLETED", "VISITED",
                   "SCHEDULE", "OVERDUE", "SKIPPED"))))
}

# --- Rule 5: incomplete admission event ---

test_that("rule 5 detects incomplete admission event", {
  ds <- completeness_ds("adm", "ACTIVE")
  result <- neoipcr:::validation_rule_5(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 5L)
})

test_that("rule 5 returns no rows on completed admission", {
  ds <- completeness_ds("adm", "COMPLETED")
  result <- neoipcr:::validation_rule_5(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 5 honours exceptions", {
  ds <- completeness_ds("adm", "ACTIVE")
  exc <- tibble::tibble(rule_id = 5L, enrollment_key = 1L)
  result <- neoipcr:::validation_rule_5(ds, exc)
  expect_equal(nrow(result), 0L)
})

test_that("rule 5 skips without a warning when events lack status", {
  # A rule that cannot run (the dataset was imported without the event status)
  # logs a warn-level diagnostic via logger and returns; it must not raise an
  # R warning(). Guards the warning-free-by-default contract.
  ds <- completeness_ds("adm", "ACTIVE")
  ds$events$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_5(ds, NULL))
  expect_null(result)
})

# --- Rules 6-11: completed enrollment with incomplete event ---
# Rule 6 = end, 7 = bsi, 8 = nec, 9 = hap, 10 = pro, 11 = ssi

rule_event_type_map <- list(
  list(rule = 6L,  type = "end", fun = neoipcr:::validation_rule_6),
  list(rule = 7L,  type = "bsi", fun = neoipcr:::validation_rule_7),
  list(rule = 8L,  type = "nec", fun = neoipcr:::validation_rule_8),
  list(rule = 9L,  type = "hap", fun = neoipcr:::validation_rule_9),
  list(rule = 10L, type = "pro", fun = neoipcr:::validation_rule_10),
  list(rule = 11L, type = "ssi", fun = neoipcr:::validation_rule_11)
)

for (entry in rule_event_type_map) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun

    test_that(paste0("rule ", r, " detects incomplete ", t, " event in completed enrollment"), {
      ds <- completeness_ds(t, "ACTIVE", "COMPLETED")
      result <- f(ds, NULL)
      expect_true(nrow(result) > 0L)
      expect_equal(unique(result$rule_id), r)
    })

    test_that(paste0("rule ", r, " returns no rows when ", t, " event is completed"), {
      ds <- completeness_ds(t, "COMPLETED", "COMPLETED")
      result <- f(ds, NULL)
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      ds <- completeness_ds(t, "ACTIVE", "COMPLETED")
      exc <- tibble::tibble(rule_id = r, enrollment_key = 1L)
      result <- f(ds, exc)
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when status is absent"), {
      ds <- completeness_ds(t, "ACTIVE", "COMPLETED")
      ds$events$status <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}
