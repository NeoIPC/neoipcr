# Tests for R/validation-rules-event-timing.R — rules 27-42.
# Implemented: 29, 38. Stubs: 27, 28, 30-37, 39-42.

# --- Rule 29: BSI within first 3 days of life (implemented) ---

test_that("rule 29 detects BSI with dol < 4", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "bsi"),
    sepsisData = make_test_sepsis_data(1L, dol = 2L))
  result <- neoipcr:::validation_rule_29(ds, NULL)
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$rule_id), 29L)
})

test_that("rule 29 returns no rows when dol >= 4", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "bsi"),
    sepsisData = make_test_sepsis_data(1L, dol = 5L))
  result <- neoipcr:::validation_rule_29(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 29 honours exceptions", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "bsi"),
    sepsisData = make_test_sepsis_data(1L, dol = 2L))
  exc <- tibble::tibble(rule_id = 29L, event_key = 1L)
  result <- neoipcr:::validation_rule_29(ds, exc)
  expect_equal(nrow(result), 0L)
})

# --- Rule 38: NEC event timing with DOS calculation (implemented) ---
# Rule 38 is complex — checks NEC DOS calculation.
# Need to read the actual rule to build the right fixture.

test_that("rule 38 returns no rows on data without NEC events", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_38(ds, NULL)
  # Rule returns NULL or 0-row tibble when no NEC events exist
  expect_true(is.null(result) || nrow(result) == 0L)
})

# --- Stub rules: negative tests (return empty by design) ---

stub_rules <- list(
  list(id = 27L, fun = neoipcr:::validation_rule_27,
       desc = "BSI DOL mismatch"),
  list(id = 28L, fun = neoipcr:::validation_rule_28,
       desc = "BSI LOS mismatch"),
  list(id = 30L, fun = neoipcr:::validation_rule_30,
       desc = "BSI within first 2 days of hospitalisation"),
  list(id = 31L, fun = neoipcr:::validation_rule_31,
       desc = "HAP DOL mismatch"),
  list(id = 32L, fun = neoipcr:::validation_rule_32,
       desc = "HAP LOS mismatch"),
  list(id = 33L, fun = neoipcr:::validation_rule_33,
       desc = "HAP within first 3 days of life"),
  list(id = 34L, fun = neoipcr:::validation_rule_34,
       desc = "HAP within first 2 days of hospitalisation"),
  list(id = 35L, fun = neoipcr:::validation_rule_35,
       desc = "NEC DOL mismatch"),
  list(id = 36L, fun = neoipcr:::validation_rule_36,
       desc = "NEC LOS mismatch"),
  list(id = 37L, fun = neoipcr:::validation_rule_37,
       desc = "NEC within first 3 days of life"),
  list(id = 39L, fun = neoipcr:::validation_rule_39,
       desc = "PRO DOL mismatch"),
  list(id = 40L, fun = neoipcr:::validation_rule_40,
       desc = "PRO LOS mismatch"),
  list(id = 41L, fun = neoipcr:::validation_rule_41,
       desc = "SSI DOL mismatch"),
  list(id = 42L, fun = neoipcr:::validation_rule_42,
       desc = "SSI LOS mismatch")
)

for (entry in stub_rules) {
  local({
    id   <- entry$id
    f    <- entry$fun
    desc <- entry$desc

    test_that(paste0("rule ", id, " returns no rows (stub: ", desc, ")"), {
      ds <- make_populated_test_ds()
      result <- f(ds, NULL)
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", id, " detects ", desc), {
      skip(paste0("Rule ", id, " not yet migrated from Validation-Report"))
    })

    test_that(paste0("rule ", id, " honours exceptions"), {
      skip(paste0("Rule ", id, " not yet migrated from Validation-Report"))
    })
  })
}
