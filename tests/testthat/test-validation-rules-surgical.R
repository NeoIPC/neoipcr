# Tests for R/validation-rules-surgical.R — rules 19, 22-24.

# --- Rule 19: SSI outside the follow-up window of every surgery ---

ssi_type <- function(x)
  factor(x, levels = c("1", "2", "3"))

# One patient, one enrolment; a surgery on 2024-01-01 (event key 1) and an
# SSI `offset` days later (event key 2). `surgery = FALSE` leaves the patient
# without any procedure.
rule_19_ds <- function(offset, infection_type = "1", implant = FALSE, surgery = TRUE) {
  types <- c(if (surgery) "pro", "ssi")
  dates <- as.Date("2024-01-01") + c(if (surgery) 0L, offset)
  n <- length(types)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(n,
      enrollment_keys = rep(1L, n),
      patient_keys    = rep(1L, n),
      event_type_keys = types,
      occurredAt = dates),
    surgeryData = if (surgery)
      make_test_surgery_data(1L, implant = implant)
    else
      make_test_surgery_data(integer(0)),
    ssiData = make_test_ssi_data(n, infection_type = ssi_type(infection_type)))
}

test_that("rule 19 detects a superficial SSI after the 30-day window", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(35L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 19L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], "infection_type")
  expect_equal(as.character(result$context[[1]]$infection_type), "1")
})

test_that("rule 19 counts the procedure date as day 1 of the window", {
  # The protocol's 30 days run from the procedure date, so the window covers
  # the offsets 0 to 29: an infection on the procedure day is inside it and
  # one 30 days later is the first outside.
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(0L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(20L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(29L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(30L), NULL)), 1L)
})

test_that("rule 19 detects an SSI dated before its only surgery", {
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(-1L), NULL)), 1L)
})

test_that("rule 19 extends the window to 90 days for a deep infection after an implant", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = TRUE), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(89L, infection_type = "2", implant = TRUE), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(90L, infection_type = "2", implant = TRUE), NULL)), 1L)
  # Without an implant a deep infection keeps the 30-day window.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = FALSE), NULL)), 1L)
})

test_that("rule 19 reads an implant flag that was not recorded as no implant", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(20L, infection_type = "2", implant = NA), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = NA), NULL)), 1L)
})

test_that("rule 19 detects an SSI of a patient without any surgery", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(10L, surgery = FALSE), NULL)
  expect_equal(nrow(result), 1L)
})

# One patient, one enrolment; a surgery on each of `surgery_dates` (event
# keys 1..k) and an SSI on `ssi_date` (event key k + 1).
rule_19_multi_ds <- function(surgery_dates, ssi_date) {
  k <- length(surgery_dates)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(k + 1L,
      enrollment_keys = rep(1L, k + 1L),
      patient_keys    = rep(1L, k + 1L),
      event_type_keys = c(rep("pro", k), "ssi"),
      occurredAt = as.Date(c(surgery_dates, ssi_date))),
    surgeryData = make_test_surgery_data(seq_len(k)),
    ssiData     = make_test_ssi_data(k + 1L))
}

test_that("rule 19 accepts an SSI covered by any one of several surgeries", {
  # The first surgery's window has closed, the second's has not.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_multi_ds(c("2024-01-01", "2024-02-15"), "2024-03-01"), NULL)), 0L)
  # Both windows have closed: one finding for the infection, not one per
  # surgery.
  result <- neoipcr:::validation_rule_19(
    rule_19_multi_ds(c("2024-01-01", "2024-01-10"), "2024-03-01"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$event_key, 3L)
})

test_that("rule 19 matches surgeries to the patient they belong to", {
  # The second patient's SSI follows the first patient's surgery in time,
  # but not in person.
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 2L)),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 2L),
      event_type_keys = c("pro", "ssi"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-10"))),
    surgeryData = make_test_surgery_data(1L),
    ssiData     = make_test_ssi_data(2L))
  result <- neoipcr:::validation_rule_19(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$patient_key, 2L)
})

test_that("rule 19 accepts a surgery from another enrolment of the same patient", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-20"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("pro", "ssi"),
      occurredAt = as.Date(c("2024-01-05", "2024-01-25"))),
    surgeryData = make_test_surgery_data(1L),
    ssiData     = make_test_ssi_data(2L))
  expect_equal(nrow(neoipcr:::validation_rule_19(ds, NULL)), 0L)
})

test_that("rule 19 leaves an SSI without a date alone", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(NA_integer_), NULL)), 0L)
})

test_that("rule 19 honours exceptions", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(35L), make_test_exceptions(19L, event_key = 2L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 19 skips without a warning when the implant flag is absent", {
  ds <- rule_19_ds(35L)
  ds$surgeryData$implant <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_19(ds, NULL))
  expect_null(result)
})

test_that("rule 19 skips without a warning when the infection type is absent", {
  ds <- rule_19_ds(35L)
  ds$ssiData$infection_type <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_19(ds, NULL))
  expect_null(result)
})

# --- Rules 22-24: procedure codes that are not valid ICHI codes ---
# Rule 22 = main code, 23 = first side code, 24 = second side code.

surgery_ds <- function(...)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "pro"),
    surgeryData = make_test_surgery_data(1L, ...))

ichi_rules <- list(
  list(rule = 22L, col = "main_procedure_code",   fun = neoipcr:::validation_rule_22),
  list(rule = 23L, col = "side_procedure_code_1", fun = neoipcr:::validation_rule_23),
  list(rule = 24L, col = "side_procedure_code_2", fun = neoipcr:::validation_rule_24)
)

for (entry in ichi_rules) {
  local({
    r   <- entry$rule
    col <- entry$col
    f   <- entry$fun
    with_code <- function(code) do.call(surgery_ds, stats::setNames(list(code), col))

    test_that(paste0("rule ", r, " detects an invalid code in ", col), {
      result <- f(with_code("NOT A CODE"), NULL)
      expect_equal(nrow(result), 1L)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 1L)
      expect_named(result$context[[1]], c("procedure_description", "procedure_code"))
      expect_equal(result$context[[1]]$procedure_code, "NOT A CODE")
    })

    test_that(paste0("rule ", r, " returns no rows for a valid or absent code in ", col), {
      expect_equal(nrow(f(with_code("PZX.AA.JA"), NULL)), 0L)
      expect_equal(nrow(f(with_code(NA_character_), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(with_code("NOT A CODE"), make_test_exceptions(r, event_key = 1L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when ", col, " is absent"), {
      ds <- with_code("NOT A CODE")
      ds$surgeryData[[col]] <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
      # The procedure description travels in the context, so it is read too.
      ds <- with_code("NOT A CODE")
      ds$surgeryData$procedure_description <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}
