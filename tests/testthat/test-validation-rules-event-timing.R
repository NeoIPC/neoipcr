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
      expect_declared_context(result)
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
      # A missing value is not a mismatch. The field is compulsory on the
      # form, so DHIS2 refuses its absence at entry; this rule compares
      # values that were entered.
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
      expect_declared_context(result)
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
# The day of life is derived from the dates and the admission form, not read
# off the infection form: the admission's day of life — day 1 for admission
# types 1 and 2, the recorded day for type 3 — advanced by the days from the
# enrolment date to the event date. Admission types 1 and 2 hold day 1 as the
# client assigns and stores it, so a stored value on such a form is one the
# team never chose; a type-3 admission without the recorded day, whose form
# rule 46 flags, gives no day to judge on.

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
    slot <- event_type_slots[[t]]$slot

    test_that(paste0("rule ", r, " flags a ", t, " on a derived day of life below 4, not the stored one"), {
      # A readmission recorded as type 2, the infection on its second day:
      # derived day of life 2, while the form stores a stale 9.
      result <- f(
        timing_ds(t, form = list(dol = 9L),
                  admission = list(type = admission_type("2")), offset = 1L),
        NULL)
      expect_equal(nrow(result), 1L)
      expect_declared_context(result)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 2L)
      expect_named(result$context[[1]], "dol")
      expect_equal(result$context[[1]]$dol, 2L)
    })

    test_that(paste0("rule ", r, " leaves a ", t, " whose derived day of life reaches 4 a stored one below it"), {
      # The same rule must not flag when the dates say the fourth day of
      # life or later although the form stores a stale 2: the stored day is
      # stale, not the onset early.
      expect_equal(nrow(f(timing_ds(t, form = list(dol = 2L)), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " flags a ", t, " on the derived third day of life"), {
      # The window's lower side is what decides an infection three days
      # into life: type 1 gives the base day 1, the event two days after
      # enrolment lands on day 3, which lies inside the window.
      result <- f(timing_ds(t, form = list(dol = 2L), offset = 2L), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dol, 3L)
    })

    test_that(paste0("rule ", r, " flags a ", t, " on a derived day of life below 4 of a type-3 admission"), {
      # Admitted on the second day of life, the infection on the day of
      # admission falls on day 2; nothing is stored on the infection form.
      result <- f(
        timing_ds(t, form = list(dol = NA_integer_),
                  admission = list(type = admission_type("3"), dol = 2L),
                  offset = 0L),
        NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dol, 2L)
    })

    test_that(paste0("rule ", r, " returns no rows from the derived fourth day of life"), {
      # Type 3 admitted on the second day of life, the infection four days
      # later: derived day 6, and a derived day exactly 4.
      expect_equal(nrow(f(
        timing_ds(t, form = list(dol = NA_integer_),
                  admission = list(type = admission_type("3"), dol = 2L),
                  offset = 2L), NULL)), 0L)
      expect_equal(nrow(f(timing_ds(t, offset = 3L), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " takes day 1 from a type-1 or type-2 form whatever day of life it stores"), {
      # The client assigns and stores day 1 on these forms, so a value
      # beside a type 1 or 2 was never the team's and takes no part in the
      # derivation: a stale 5 must not move the infection to day 6, and a
      # missing one must not leave the day undeterminable.
      for (ty in c("1", "2")) {
        result <- f(
          timing_ds(t, form = list(dol = 9L),
                    admission = list(type = admission_type(ty), dol = 5L),
                    offset = 1L),
          NULL)
        expect_equal(nrow(result), 1L, info = paste("admission type", ty))
        expect_equal(result$event_key, 2L)
        expect_equal(result$context[[1]]$dol, 2L)
        result <- f(
          timing_ds(t, form = list(dol = 9L),
                    admission = list(type = admission_type(ty), dol = NA_integer_),
                    offset = 1L),
          NULL)
        expect_equal(nrow(result), 1L,
                     info = paste("admission type", ty, "without a stored day"))
        expect_equal(result$context[[1]]$dol, 2L)
      }
    })

    test_that(paste0("rule ", r, " leaves a type-3 admission without a recorded day of life unjudged"), {
      # The day cannot be derived, so the event is left unjudged whatever it
      # would fall on — day 1 included, which is where a wrong base would
      # place it. Rule 46 flags that form, and the pass removes its patient.
      expect_equal(nrow(f(
        timing_ds(t, form = list(dol = 2L),
                  admission = list(type = admission_type("3"), dol = NA_integer_)),
        NULL)), 0L)
      expect_equal(nrow(f(
        timing_ds(t, form = list(dol = 2L),
                  admission = list(type = admission_type("3"), dol = NA_integer_),
                  offset = 0L), NULL)), 0L)
      # An enrolment whose admission form is absent in the data derives no
      # day either, so its event is left unjudged on the same footing.
      ds <- timing_ds(t, form = list(dol = 2L), offset = 0L)
      ds$admissionData <- ds$admissionData[0L, ]
      expect_equal(nrow(f(ds, NULL)), 0L)
    })

    test_that(paste0("rule ", r, " treats an admission without a type as day 1 of life"), {
      result <- f(
        timing_ds(t, form = list(dol = 9L),
                  admission = list(type = factor(NA_character_, levels = c("1", "2", "3"))),
                  offset = 1L),
        NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dol, 2L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      ds <- timing_ds(t, form = list(dol = 9L),
                      admission = list(type = admission_type("2")), offset = 1L)
      expect_equal(nrow(f(ds, NULL)), 1L)
      expect_equal(nrow(f(ds, make_test_exceptions(r, event_key = 2L))), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when the admission form lacks its type or day of life"), {
      ds <- timing_ds(t, form = list(dol = 9L), offset = 1L)
      ds$admissionData$type <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
      ds <- timing_ds(t, form = list(dol = 9L), offset = 1L)
      ds$admissionData$dol <- NULL
      expect_null(f(ds, NULL))
    })

    test_that(paste0("rule ", r, " runs without a day of life on the ", t, " form"), {
      # The rule derives the day and never reads the form's stored one, so a
      # dataset lacking that column still gives it everything it needs.
      ds <- timing_ds(t, form = list(dol = 9L),
                     admission = list(type = admission_type("2")), offset = 1L)
      ds[[slot]]$dol <- NULL
      expect_equal(nrow(f(ds, NULL)), 1L)
    })
  })
}

# --- Early after admission: rules 30 (bsi), 34 (hap), 38 (nec) ---
# The day of the stay is derived from the dates — the days from the enrolment
# date to the event date, plus one, the day of admission being day 1 — and
# the rule applies to every admission type: an infection before its third day
# is more likely acquired before this stay whatever the stay's beginning.

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
    types <- list("1", "2", "3", NA_character_)

    test_that(paste0("rule ", r, " flags a ", t, " on day 1 or 2 of the stay whatever the admission type"), {
      for (ty in types) {
        admission <- list(type = admission_type(ty), dol = 2L)
        result <- f(
          timing_ds(t, form = list(los = NA_integer_), admission = admission, offset = 0L),
          NULL)
        expect_equal(nrow(result), 1L, info = paste("admission type", ty, "day 1"))
        expect_declared_context(result)
        expect_equal(result$rule_id, r)
        expect_equal(result$event_key, 2L)
        expect_named(result$context[[1]], "dos")
        expect_equal(result$context[[1]]$dos, 1L)
        result <- f(
          timing_ds(t, form = list(los = NA_integer_), admission = admission, offset = 1L),
          NULL)
        expect_equal(nrow(result), 1L, info = paste("admission type", ty, "day 2"))
        expect_equal(result$context[[1]]$dos, 2L)
      }
    })

    test_that(paste0("rule ", r, " returns no rows from the derived third day of the stay"), {
      expect_equal(nrow(f(
        timing_ds(t, form = list(los = NA_integer_),
                  admission = list(type = admission_type("2"), dol = 2L),
                  offset = 2L), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " flags a ", t, " whose derived day of the stay is below 3 a stored one above it"), {
      # The form stores a stale day of occurrence of 5; the dates say the
      # infection falls on day 2 of the stay.
      result <- f(
        timing_ds(t, form = list(los = 5L),
                  admission = list(type = admission_type("3"), dol = 2L),
                  offset = 1L),
        NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dos, 2L)
    })

    test_that(paste0("rule ", r, " judges an event whose enrolment carries no admission form"), {
      # The day of the stay needs only the dates, so the rule judges the
      # event even when the admission form is absent: unlike the day of
      # life, it has no base to read there.
      ds <- timing_ds(t, form = list(los = NA_integer_), offset = 1L)
      ds$admissionData <- ds$admissionData[0L, ]
      result <- f(ds, NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$context[[1]]$dos, 2L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      ds <- timing_ds(t, form = list(los = NA_integer_),
                      admission = list(type = admission_type("2"), dol = 2L),
                      offset = 1L)
      expect_equal(nrow(f(ds, NULL)), 1L)
      expect_equal(nrow(f(ds, make_test_exceptions(r, event_key = 2L))), 0L)
    })

    test_that(paste0("rule ", r, " runs whatever the admission data lack"), {
      # The dates carry the whole derivation, so no missing admission column
      # can leave the rule with nothing to run on.
      ds <- timing_ds(t, form = list(los = NA_integer_), offset = 1L)
      ds$admissionData$type <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_equal(nrow(result), 1L)
      ds <- timing_ds(t, form = list(los = NA_integer_), offset = 1L)
      ds$admissionData$dol <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_equal(nrow(result), 1L)
    })
  })
}
