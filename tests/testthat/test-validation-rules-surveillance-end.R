# Tests for R/validation-rules-surveillance-end.R — rules 18, 21, 51–54.

# One enrolment from 2024-01-01 with an admission event (key 1) and an end
# event on 2024-01-11 (key 2), whose surveillance-end form takes `...`.
surveillance_end_ds <- function(..., substance_days = NULL)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(2,
      enrollment_keys = c(1L, 1L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("adm", "end"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-11"))),
    surveillanceEndData = make_test_surveillance_end_data(event_keys = 2L, ...),
    substanceDays = if (is.null(substance_days))
      make_test_substance_days(integer(0))
    else
      make_test_substance_days(rep(2L, length(substance_days)), days = substance_days))

# --- Rule 18: patient days validation ---

test_that("rule 18 detects patient_days mismatch", {
  # Formula: patient_days_calculated = 1 + (end_date - enrollment_date)
  # enrollment Jan 1 → end Jan 11 → 1 + 10 = 11
  ds <- surveillance_end_ds(patient_days = 999L)
  result <- neoipcr:::validation_rule_18(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 18L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("patient_days", "patient_days_calculated"))
  expect_equal(result$context[[1]]$patient_days_calculated, 11L)
})

test_that("rule 18 returns no rows when patient_days is correct", {
  expect_equal(nrow(neoipcr:::validation_rule_18(surveillance_end_ds(patient_days = 11L), NULL)), 0L)
})

test_that("rule 18 detects missing (NA) patient_days", {
  # patient_days is compulsory + auto-calculated in DHIS2; a missing value
  # bypassed that calculation and must be flagged. Without the is.na() guard
  # the `NA != calculated` comparison is NA and filter() would drop it.
  result <- neoipcr:::validation_rule_18(surveillance_end_ds(patient_days = NA_integer_), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 18L)
  expect_true(1L %in% result$patient_key)
})

test_that("rule 18 honours exceptions", {
  result <- neoipcr:::validation_rule_18(
    surveillance_end_ds(patient_days = 999L), make_test_exceptions(18L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 18 skips without a warning when patient_days is absent", {
  ds <- surveillance_end_ds(patient_days = 999L)
  ds$surveillanceEndData$patient_days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_18(ds, NULL))
  expect_null(result)
})

# --- Rule 21: substance days sum to less than the antibiotic days ---

test_that("rule 21 detects substance days short of the antibiotic days", {
  ds <- surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L))
  result <- neoipcr:::validation_rule_21(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 21L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("ab_substance_days", "ab_days"))
  expect_equal(result$context[[1]]$ab_substance_days, 4L)
  expect_equal(result$context[[1]]$ab_days, 5L)
})

test_that("rule 21 counts antibiotic days with no substance recorded as a shortfall", {
  result <- neoipcr:::validation_rule_21(surveillance_end_ds(ab_days = 5L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$ab_substance_days, 0L)
})

test_that("rule 21 returns no rows when the substance days reach or exceed the antibiotic days", {
  # Combination therapy: several substances on one day exceed the count.
  expect_equal(nrow(neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(3L, 2L)), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(5L, 5L)), NULL)), 0L)
})

test_that("rule 21 does not check a record without antibiotic days", {
  expect_equal(nrow(neoipcr:::validation_rule_21(surveillance_end_ds(ab_days = 0L), NULL)), 0L)
})

test_that("rule 21 honours exceptions", {
  result <- neoipcr:::validation_rule_21(
    surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L)),
    make_test_exceptions(21L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 21 skips without a warning when the substance days or the antibiotic days are absent", {
  ds <- surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L))
  ds$substanceDays$days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_21(ds, NULL))
  expect_null(result)
  ds <- surveillance_end_ds(ab_days = 5L, substance_days = c(2L, 2L))
  ds$surveillanceEndData$ab_days <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_21(ds, NULL))
  expect_null(result)
})

# --- Rule 51: a cumulative count above the patient days ---

# The fixture's counts (3 CVC, 2 PVC, 1 INV, 1 NIV, 2 ventilation, 5
# antibiotic, 8 human milk, 4 kangaroo care, 6 probiotic days) all fit in the
# 11 patient days the enrolment's dates imply.
counts_ds <- function(...)
  surveillance_end_ds(patient_days = 11L, ...)

test_that("rule 51 detects a count above the patient days", {
  ds <- counts_ds(cvc_days = 12L)
  result <- neoipcr:::validation_rule_51(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 51L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("count", "days", "patient_days"))
  expect_equal(result$context[[1]]$count, "cvc_days")
  expect_equal(result$context[[1]]$days, 12L)
  expect_equal(result$context[[1]]$patient_days, 11L)
})

test_that("rule 51 bounds every cumulative count", {
  for (count in c("cvc_days", "pvc_days", "inv_days", "niv_days", "vs_days", "ab_days",
                  "human_milk_days", "kangaroo_care_days", "probiotic_days")) {
    result <- neoipcr:::validation_rule_51(
      do.call(counts_ds, rlang::set_names(list(20L), count)), NULL)
    expect_equal(nrow(result), 1L, info = count)
    expect_equal(result$context[[1]]$count, count)
  }
})

test_that("rule 51 records one finding per count that exceeds the patient days", {
  result <- neoipcr:::validation_rule_51(counts_ds(cvc_days = 12L, ab_days = 20L), NULL)
  expect_equal(nrow(result), 2L)
  expect_equal(result$enrollment_key, c(1L, 1L))
  expect_true(all(vapply(result$context, nrow, integer(1)) == 1L))
  expect_setequal(vapply(result$context, \(ctx) ctx$count, character(1)), c("cvc_days", "ab_days"))
})

test_that("rule 51 bounds the invasive and non-invasive ventilation days together", {
  # Six days of each fit the patient days, twelve of ventilation do not.
  result <- neoipcr:::validation_rule_51(
    counts_ds(inv_days = 6L, niv_days = 6L, vs_days = 12L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$count, "vs_days")
  expect_equal(result$context[[1]]$days, 12L)
})

test_that("rule 51 accepts a count equal to the patient days and does not judge a missing one", {
  expect_equal(nrow(neoipcr:::validation_rule_51(counts_ds(cvc_days = 11L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_51(counts_ds(cvc_days = NA_integer_), NULL)), 0L)
  # Without patient days there is nothing to compare with: rule 18's finding.
  expect_equal(nrow(neoipcr:::validation_rule_51(
    surveillance_end_ds(patient_days = NA_integer_, cvc_days = 12L), NULL)), 0L)
})

test_that("rule 51 honours exceptions", {
  result <- neoipcr:::validation_rule_51(
    counts_ds(cvc_days = 12L, ab_days = 20L), make_test_exceptions(51L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 51 skips without a warning when the patient days or a count is absent", {
  for (col in c("patient_days", "vs_days", "probiotic_days")) {
    ds <- counts_ds(cvc_days = 12L)
    ds$surveillanceEndData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_51(ds, NULL))
    expect_null(result)
  }
})

# --- Rules 52 to 54: the antibiotic substance slots ---

# The `surveillance_end_ds()` enrolment with one substance slot per element
# of `codes` and `days`, in slot order, and a surveillance-end form taking
# `...` with 11 patient days.
substance_slots_ds <- function(codes, days, ...)
  make_test_ds(
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
      event_keys = 2L, patient_days = 11L, ...),
    substanceDays = make_test_substance_days(
      rep(2L, length(codes)),
      index = seq_along(codes), substance_code = codes, days = as.integer(days)))

slot_contexts <- function(result)
  dplyr::bind_rows(result$context)

test_that("rule 52 detects days recorded without a substance", {
  ds <- substance_slots_ds(c("J01CA04", NA), c(3L, 2L))
  result <- neoipcr:::validation_rule_52(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 52L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("index", "substance_code", "days"))
  expect_equal(result$context[[1]]$index, 2L)
  expect_true(is.na(result$context[[1]]$substance_code))
  expect_equal(result$context[[1]]$days, 2L)
})

test_that("rule 52 detects a substance recorded without its days", {
  result <- neoipcr:::validation_rule_52(
    substance_slots_ds(c("J01CA04", "J01DD04"), c(3L, NA)), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$index, 2L)
  expect_equal(result$context[[1]]$substance_code, "J01DD04")
  expect_true(is.na(result$context[[1]]$days))
  # Zero days are no days.
  result <- neoipcr:::validation_rule_52(
    substance_slots_ds(c("J01CA04", "J01DD04"), c(3L, 0L)), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$days, 0L)
})

test_that("rule 52 records one finding per slot", {
  result <- neoipcr:::validation_rule_52(
    substance_slots_ds(c("J01CA04", NA, "J01DD04"), c(3L, 2L, NA)), NULL)
  expect_equal(nrow(result), 2L)
  expect_true(all(vapply(result$context, nrow, integer(1)) == 1L))
  expect_equal(slot_contexts(result)$index, c(2L, 3L))
})

test_that("rule 52 returns no rows for complete slots or no slots", {
  expect_equal(nrow(neoipcr:::validation_rule_52(
    substance_slots_ds(c("J01CA04", "J01DD04"), c(3L, 2L)), NULL)), 0L)
  # One day is the smallest count the form admits, and a complete slot.
  expect_equal(nrow(neoipcr:::validation_rule_52(
    substance_slots_ds("J01CA04", 1L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_52(
    substance_slots_ds(character(), integer()), NULL)), 0L)
  # A slot empty on both sides holds nothing to be incomplete.
  expect_equal(nrow(neoipcr:::validation_rule_52(
    substance_slots_ds(c(NA, NA), c(NA, 0L)), NULL)), 0L)
})

test_that("rule 52 honours exceptions", {
  result <- neoipcr:::validation_rule_52(
    substance_slots_ds(c("J01CA04", NA), c(3L, 2L)),
    make_test_exceptions(52L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 53 detects a substance's days above the antibiotic days", {
  ds <- substance_slots_ds(c("J01CA04", "J01DD04"), c(2L, 5L), ab_days = 4L)
  result <- neoipcr:::validation_rule_53(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 53L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("index", "substance_code", "days", "ab_days", "patient_days"))
  expect_equal(result$context[[1]]$index, 2L)
  expect_equal(result$context[[1]]$substance_code, "J01DD04")
  expect_equal(result$context[[1]]$days, 5L)
  expect_equal(result$context[[1]]$ab_days, 4L)
  expect_equal(result$context[[1]]$patient_days, 11L)
})

test_that("rule 53 detects a substance's days above the patient days and a substance without antibiotic days", {
  result <- neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 12L, ab_days = 12L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$days, 12L)
  # A substance on a form with no antibiotic days exceeds them with its
  # first day.
  result <- neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 1L, ab_days = 0L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$ab_days, 0L)
})

test_that("rule 53 records one finding per slot", {
  result <- neoipcr:::validation_rule_53(
    substance_slots_ds(c("J01CA04", "J01DD04", "J01XA01"), c(5L, 5L, 1L), ab_days = 4L), NULL)
  expect_equal(nrow(result), 2L)
  expect_true(all(vapply(result$context, nrow, integer(1)) == 1L))
  expect_equal(slot_contexts(result)$index, c(1L, 2L))
})

test_that("rule 53 accepts days equal to either bound and leaves a slot without days to rule 52", {
  expect_equal(nrow(neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 4L, ab_days = 4L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 11L, ab_days = 11L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", NA, ab_days = 4L), NULL)), 0L)
  # Without antibiotic days the patient days still bound the slot.
  expect_equal(nrow(neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 4L, ab_days = NA_integer_), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 12L, ab_days = NA_integer_), NULL)), 1L)
})

test_that("rule 53 honours exceptions", {
  result <- neoipcr:::validation_rule_53(
    substance_slots_ds("J01CA04", 5L, ab_days = 4L),
    make_test_exceptions(53L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 54 detects a substance recorded in two slots", {
  ds <- substance_slots_ds(c("J01CA04", "J01DD04", "J01CA04"), c(1L, 1L, 1L))
  result <- neoipcr:::validation_rule_54(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 54L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("substance_code", "index", "index_other"))
  expect_equal(result$context[[1]]$substance_code, "J01CA04")
  expect_equal(result$context[[1]]$index, 1L)
  expect_equal(result$context[[1]]$index_other, 3L)
})

test_that("rule 54 records one finding per pair of slots", {
  result <- neoipcr:::validation_rule_54(
    substance_slots_ds(c("J01CA04", "J01CA04", "J01CA04"), c(1L, 1L, 1L)), NULL)
  expect_equal(nrow(result), 3L)
  expect_true(all(vapply(result$context, nrow, integer(1)) == 1L))
  pairs <- slot_contexts(result)
  expect_equal(pairs$index,       c(1L, 1L, 2L))
  expect_equal(pairs$index_other, c(2L, 3L, 3L))
})

test_that("rule 54 returns no rows for distinct substances or for slots without a substance", {
  expect_equal(nrow(neoipcr:::validation_rule_54(
    substance_slots_ds(c("J01CA04", "J01DD04"), c(1L, 1L)), NULL)), 0L)
  # Two slots holding days alone are rule 52's, not a substance twice.
  expect_equal(nrow(neoipcr:::validation_rule_54(
    substance_slots_ds(c(NA, NA), c(1L, 1L)), NULL)), 0L)
})

test_that("rule 54 pairs the slots of one form only", {
  # The same substance on the surveillance-end forms of two enrolments, in
  # different slots, is once per form.
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 1L)),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      event_type_keys = c("end", "end")),
    substanceDays = make_test_substance_days(
      c(1L, 2L), index = c(1L, 2L), substance_code = c("J01CA04", "J01CA04"), days = c(2L, 2L)))
  expect_equal(nrow(neoipcr:::validation_rule_54(ds, NULL)), 0L)
})

test_that("rule 54 honours exceptions", {
  result <- neoipcr:::validation_rule_54(
    substance_slots_ds(c("J01CA04", "J01CA04"), c(1L, 1L)),
    make_test_exceptions(54L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rules 52 to 54 skip without a warning when a slot column or a bound is absent", {
  for (col in c("index", "substance_code", "days")) {
    for (rule in list(neoipcr:::validation_rule_52, neoipcr:::validation_rule_53, neoipcr:::validation_rule_54)) {
      ds <- substance_slots_ds(c("J01CA04", NA), c(3L, 2L))
      ds$substanceDays[[col]] <- NULL
      expect_no_warning(result <- rule(ds, NULL))
      expect_null(result)
    }
  }
  for (col in c("ab_days", "patient_days")) {
    ds <- substance_slots_ds("J01CA04", 5L, ab_days = 4L)
    ds$surveillanceEndData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_53(ds, NULL))
    expect_null(result)
  }
})
