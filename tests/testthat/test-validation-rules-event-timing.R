# Tests for R/validation-rules-event-timing.R — rules 27-42, four families
# that each run once per infection or surgery event type.

event_type_slots <- list(
  bsi = list(slot = "sepsisData",    make = make_test_sepsis_data),
  hap = list(slot = "pneumoniaData", make = make_test_pneumonia_data),
  nec = list(slot = "necData",       make = make_test_nec_data),
  pro = list(slot = "surgeryData",   make = make_test_surgery_data),
  ssi = list(slot = "ssiData",       make = make_test_ssi_data))

admission_type <- function(x)
  factor(x, levels = c("1", "2", "3"))

# One enrolment from 2024-01-01 with the admission event (key 1, day of life
# 1 unless `admission` says otherwise) and one event of `type` `offset` days
# later (key 2), whose form data takes `form`.
timing_ds <- function(type, form = list(), admission = list(), offset = 5L) {
  slot <- event_type_slots[[type]]
  make <- slot$make
  args <- list(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", type),
      occurredAt = as.Date("2024-01-01") + c(0L, offset)),
    admissionData = do.call(
      make_test_admission_data,
      utils::modifyList(list(event_keys = 1L, dol = 1L), admission)))
  args[[slot$slot]] <- do.call(make, c(list(event_keys = 2L), form))
  do.call(make_test_ds, args)
}

# --- Day-of-life mismatch: rules 27 (bsi), 31 (hap), 35 (nec), 39 (pro), 41 (ssi) ---
# dol_calc = admission day of life + days from the enrolment to the event = 1 + 5.

dol_rules <- list(
  list(rule = 27L, type = "bsi", fun = neoipcr:::validation_rule_27),
  list(rule = 31L, type = "hap", fun = neoipcr:::validation_rule_31),
  list(rule = 35L, type = "nec", fun = neoipcr:::validation_rule_35),
  list(rule = 39L, type = "pro", fun = neoipcr:::validation_rule_39),
  list(rule = 41L, type = "ssi", fun = neoipcr:::validation_rule_41)
)

for (entry in dol_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun

    test_that(paste0("rule ", r, " detects a ", t, " day of life that does not match the calculated value"), {
      result <- f(timing_ds(t, form = list(dol = 7L)), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 2L)
      expect_named(result$context[[1]], c("dol", "dol_calc"))
      expect_equal(result$context[[1]]$dol, 7L)
      expect_equal(result$context[[1]]$dol_calc, 6L)
    })

    test_that(paste0("rule ", r, " returns no rows when the ", t, " day of life matches"), {
      expect_equal(nrow(f(timing_ds(t, form = list(dol = 6L)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " advances the admission form's day of life, not a constant"), {
      # Admitted on the third day of life, the event five days later falls
      # on the eighth; a rule that read the admission form as day one would
      # expect the sixth and get both cases wrong.
      admitted_on_3 <- list(dol = 3L)
      expect_equal(nrow(f(timing_ds(t, form = list(dol = 8L), admission = admitted_on_3), NULL)), 0L)
      result <- f(timing_ds(t, form = list(dol = 6L), admission = admitted_on_3), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dol_calc, 8L)
    })

    test_that(paste0("rule ", r, " does not flag a ", t, " form without a day of life"), {
      # A missing value is not a mismatch; the form's compulsory field is
      # DHIS2's to require.
      expect_equal(nrow(f(timing_ds(t, form = list(dol = NA_integer_)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(timing_ds(t, form = list(dol = 7L)), make_test_exceptions(r, event_key = 2L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when a day of life is absent"), {
      ds <- timing_ds(t, form = list(dol = 7L))
      ds[[event_type_slots[[t]]$slot]]$dol <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
      # The admission form's day of life is read as well.
      ds <- timing_ds(t, form = list(dol = 7L))
      ds$admissionData$dol <- NULL
      expect_null(f(ds, NULL))
    })
  })
}

# --- Day-of-occurrence mismatch: rules 28 (bsi), 32 (hap), 36 (nec), 40 (pro), 42 (ssi) ---
# los_calc = days from the enrolment to the event = 5.

los_rules <- list(
  list(rule = 28L, type = "bsi", fun = neoipcr:::validation_rule_28),
  list(rule = 32L, type = "hap", fun = neoipcr:::validation_rule_32),
  list(rule = 36L, type = "nec", fun = neoipcr:::validation_rule_36),
  list(rule = 40L, type = "pro", fun = neoipcr:::validation_rule_40),
  list(rule = 42L, type = "ssi", fun = neoipcr:::validation_rule_42)
)

for (entry in los_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun

    test_that(paste0("rule ", r, " detects a ", t, " day of occurrence that does not match the calculated value"), {
      result <- f(timing_ds(t, form = list(los = 9L)), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 2L)
      expect_named(result$context[[1]], c("los", "los_calc"))
      expect_equal(result$context[[1]]$los, 9L)
      expect_equal(result$context[[1]]$los_calc, 5L)
    })

    test_that(paste0("rule ", r, " returns no rows when the ", t, " day of occurrence matches"), {
      expect_equal(nrow(f(timing_ds(t, form = list(los = 5L)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " does not flag a ", t, " form without a day of occurrence"), {
      expect_equal(nrow(f(timing_ds(t, form = list(los = NA_integer_)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(timing_ds(t, form = list(los = 9L)), make_test_exceptions(r, event_key = 2L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when the day of occurrence is absent"), {
      ds <- timing_ds(t, form = list(los = 9L))
      ds[[event_type_slots[[t]]$slot]]$los <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}

# --- Early onset: rules 29 (bsi), 33 (hap), 37 (nec) ---

early_dol_rules <- list(
  list(rule = 29L, type = "bsi", fun = neoipcr:::validation_rule_29),
  list(rule = 33L, type = "hap", fun = neoipcr:::validation_rule_33),
  list(rule = 37L, type = "nec", fun = neoipcr:::validation_rule_37)
)

for (entry in early_dol_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun

    test_that(paste0("rule ", r, " detects a ", t, " event within the first three days of life"), {
      result <- f(timing_ds(t, form = list(dol = 2L)), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 2L)
      expect_named(result$context[[1]], "dol")
      expect_equal(result$context[[1]]$dol, 2L)
    })

    test_that(paste0("rule ", r, " returns no rows from the fourth day of life"), {
      expect_equal(nrow(f(timing_ds(t, form = list(dol = 4L)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " does not flag a ", t, " form without a day of life"), {
      expect_equal(nrow(f(timing_ds(t, form = list(dol = NA_integer_)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(timing_ds(t, form = list(dol = 2L)), make_test_exceptions(r, event_key = 2L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when the day of life is absent"), {
      ds <- timing_ds(t, form = list(dol = 2L))
      ds[[event_type_slots[[t]]$slot]]$dol <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}

# --- Early after (re-)admission: rules 30 (bsi), 34 (hap), 38 (nec) ---
# The day of hospitalization is the stored day of occurrence plus one.

early_dos_rules <- list(
  list(rule = 30L, type = "bsi", fun = neoipcr:::validation_rule_30),
  list(rule = 34L, type = "hap", fun = neoipcr:::validation_rule_34),
  list(rule = 38L, type = "nec", fun = neoipcr:::validation_rule_38)
)

for (entry in early_dos_rules) {
  local({
    r <- entry$rule
    t <- entry$type
    f <- entry$fun
    readmitted <- list(type = admission_type("3"))

    test_that(paste0("rule ", r, " detects a ", t, " event on the second day after a (re-)admission"), {
      result <- f(timing_ds(t, form = list(los = 1L), admission = readmitted), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 2L)
      expect_named(result$context[[1]], "dos")
      expect_equal(result$context[[1]]$dos, 2L)
    })

    test_that(paste0("rule ", r, " returns no rows from the third day of hospitalization"), {
      expect_equal(nrow(f(timing_ds(t, form = list(los = 2L), admission = readmitted), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " returns no rows for an admission that is not a (re-)admission"), {
      expect_equal(nrow(f(timing_ds(t, form = list(los = 1L)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " does not flag a ", t, " form without a day of occurrence"), {
      expect_equal(nrow(f(
        timing_ds(t, form = list(los = NA_integer_), admission = readmitted), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(
        timing_ds(t, form = list(los = 1L), admission = readmitted),
        make_test_exceptions(r, event_key = 2L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when the admission type is absent"), {
      ds <- timing_ds(t, form = list(los = 1L), admission = readmitted)
      ds$admissionData$type <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}
