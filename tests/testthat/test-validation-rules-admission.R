# Tests for R/validation-rules-admission.R — rules 45, 46, 47.

admission_type <- function(x)
  factor(x, levels = c("1", "2", "3"))

# One patient with one enrolment from 2024-01-01 and its admission event
# (key 1), whose admission form takes `...`.
admission_ds <- function(...)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm"),
    admissionData = make_test_admission_data(event_keys = 1L, ...))

# --- Rule 45: admission beyond the last eligible day of life ---

test_that("rule 45 detects an admission beyond day of life 120", {
  ds <- admission_ds(type = admission_type("3"), dol = 121L)
  result <- neoipcr:::validation_rule_45(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 45L)
  expect_equal(result$enrollment_key, 1L)
  # The finding is shown on the admission form.
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "dol")
  expect_equal(result$context[[1]]$dol, 121L)
})

test_that("rule 45 admits day of life 120 and refuses 121", {
  expect_equal(nrow(neoipcr:::validation_rule_45(
    admission_ds(type = admission_type("3"), dol = 120L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_45(
    admission_ds(type = admission_type("3"), dol = 121L), NULL)), 1L)
})

test_that("rule 45 leaves a missing day of life to the rules about the form", {
  expect_equal(nrow(neoipcr:::validation_rule_45(
    admission_ds(type = admission_type("3"), dol = NA_integer_), NULL)), 0L)
})

test_that("rule 45 leaves the client-assigned day of life of the other admission types to the network", {
  # For an infant born in the hospital or admitted on the day of birth, and
  # for a form without a type, the client sets the day of life to 1 on every
  # save, so a higher value stored there is not the team's to correct.
  for (type in c("1", "2", NA))
    expect_equal(nrow(neoipcr:::validation_rule_45(
      admission_ds(type = admission_type(type), dol = 150L), NULL)), 0L, info = type)
})

test_that("rule 45 honours exceptions", {
  result <- neoipcr:::validation_rule_45(
    admission_ds(type = admission_type("3"), dol = 150L),
    make_test_exceptions(45L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 45 skips without a warning when the type or the day of life is absent", {
  for (col in c("type", "dol")) {
    ds <- admission_ds(type = admission_type("3"), dol = 150L)
    ds$admissionData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_45(ds, NULL))
    expect_null(result)
  }
})

# --- Rule 46: a later transfer or readmission without a plausible day of life ---

test_that("rule 46 detects a type-3 admission on day of life 1", {
  ds <- admission_ds(type = admission_type("3"), dol = 1L)
  result <- neoipcr:::validation_rule_46(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 46L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "dol")
  expect_equal(result$context[[1]]$dol, 1L)
})

test_that("rule 46 detects a type-3 admission without a day of life", {
  result <- neoipcr:::validation_rule_46(
    admission_ds(type = admission_type("3"), dol = NA_integer_), NULL)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$context[[1]]$dol))
})

test_that("rule 46 returns no rows for a type-3 admission from day 2 or for the other types", {
  expect_equal(nrow(neoipcr:::validation_rule_46(
    admission_ds(type = admission_type("3"), dol = 2L), NULL)), 0L)
  # Types 1 and 2 have day 1 assigned; a missing or other value there is not
  # this rule's finding.
  for (type in c("1", "2")) {
    expect_equal(nrow(neoipcr:::validation_rule_46(
      admission_ds(type = admission_type(type), dol = 1L), NULL)), 0L, info = type)
    expect_equal(nrow(neoipcr:::validation_rule_46(
      admission_ds(type = admission_type(type), dol = NA_integer_), NULL)), 0L, info = type)
  }
  # Without a type there is nothing to hold the day of life to.
  expect_equal(nrow(neoipcr:::validation_rule_46(
    admission_ds(type = admission_type(NA), dol = 1L), NULL)), 0L)
})

test_that("rule 46 honours exceptions", {
  result <- neoipcr:::validation_rule_46(
    admission_ds(type = admission_type("3"), dol = 1L),
    make_test_exceptions(46L, enrollment_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 46 skips without a warning when the type or the day of life is absent", {
  for (col in c("type", "dol")) {
    ds <- admission_ds(type = admission_type("3"), dol = 1L)
    ds$admissionData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_46(ds, NULL))
    expect_null(result)
  }
})

# --- Rule 47: a later enrolment typed as the infant's first admission ---

# Enrolments on `enrolled_at`, the i-th belonging to `patient_keys[i]`, each
# with an admission event of the same key whose form records `types[i]`.
readmission_ds <- function(enrolled_at, types,
                           patient_keys = rep(1L, length(enrolled_at))) {
  n <- length(enrolled_at)
  make_test_ds(
    patients    = make_test_patients(max(patient_keys)),
    enrollments = make_test_enrollments(n,
      patient_keys = patient_keys,
      enrolledAt = as.Date(enrolled_at)),
    events = make_test_events(n,
      enrollment_keys = seq_len(n),
      patient_keys    = patient_keys,
      event_type_keys = rep("adm", n)),
    admissionData = make_test_admission_data(seq_len(n), type = admission_type(types)))
}

test_that("rule 47 detects a later enrolment typed as an admission from the delivery room", {
  ds <- readmission_ds(c("2024-01-01", "2024-03-01"), c("3", "1"))
  result <- neoipcr:::validation_rule_47(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_declared_form(result, ds)
  expect_equal(result$rule_id, 47L)
  expect_equal(result$enrollment_key, 2L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], c("type", "enrolledAt", "enrolledAt_previous"))
  expect_equal(as.character(result$context[[1]]$type), "1")
  expect_equal(result$context[[1]]$enrolledAt, as.Date("2024-03-01"))
  expect_equal(result$context[[1]]$enrolledAt_previous, as.Date("2024-01-01"))
})

test_that("rule 47 detects a later enrolment typed as an admission on the day of birth", {
  result <- neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-03-01"), c("1", "2")), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$context[[1]]$type), "2")
})

test_that("rule 47 names the latest earlier enrolment", {
  result <- neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-02-01", "2024-03-01"), c("1", "3", "1")), NULL)
  expect_equal(result$enrollment_key, 3L)
  expect_equal(result$context[[1]]$enrolledAt_previous, as.Date("2024-02-01"))
})

test_that("rule 47 finds the earlier enrolment whether or not it has an admission form", {
  # The earlier stay is the enrolment itself, not its admission form: a
  # readmission is one even when the earlier record is incomplete.
  ds <- readmission_ds(c("2024-01-01", "2024-03-01"), c("3", "1"))
  ds$events        <- ds$events[ds$events$enrollment_key != 1L, ]
  ds$admissionData <- ds$admissionData[ds$admissionData$event_key != 1L, ]
  result <- neoipcr:::validation_rule_47(ds, NULL)
  expect_equal(result$enrollment_key, 2L)
  expect_equal(result$context[[1]]$enrolledAt_previous, as.Date("2024-01-01"))
})

test_that("rule 47 records one finding with a one-row context when two earlier enrolments share a day", {
  # Two enrolments on one day are rule 17's overlap; for this rule they are
  # one earlier date, and the finding must not carry both rows.
  result <- neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-01-01", "2024-03-01"), c("3", "3", "1")), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(nrow(result$context[[1]]), 1L)
  expect_equal(result$context[[1]]$enrolledAt_previous, as.Date("2024-01-01"))
})

test_that("rule 47 returns no rows for a first enrolment, a type-3 readmission or another patient's earlier enrolment", {
  # The patient's first enrolment has nothing before it, whatever its type.
  expect_equal(nrow(neoipcr:::validation_rule_47(
    readmission_ds("2024-01-01", "1"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-03-01"), c("1", "3")), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-03-01"), c("1", "1"), patient_keys = c(1L, 2L)), NULL)), 0L)
  # Two enrolments on one day are rule 17's overlap, not a readmission.
  expect_equal(nrow(neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-01-01"), c("1", "1")), NULL)), 0L)
  # An enrolment without a type says nothing.
  expect_equal(nrow(neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-03-01"), c("1", NA)), NULL)), 0L)
})

test_that("rule 47 honours exceptions", {
  result <- neoipcr:::validation_rule_47(
    readmission_ds(c("2024-01-01", "2024-03-01"), c("3", "1")),
    make_test_exceptions(47L, enrollment_key = 2L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 47 skips without a warning when the type is absent", {
  ds <- readmission_ds(c("2024-01-01", "2024-03-01"), c("3", "1"))
  ds$admissionData$type <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_47(ds, NULL))
  expect_null(result)
})
