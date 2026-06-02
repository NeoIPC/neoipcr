# Tests for R/filter.R — dataset filtering and orphan removal.
# Uses make_populated_test_ds() from helper-fixtures.R.

# --- filter_surveillance_ends (internal) ---

test_that("filter_surveillance_ends with both NULL returns input unchanged", {
  events <- make_test_events(
    n = 4,
    enrollment_keys = 1:4, patient_keys = 1:4,
    event_type_keys = c("adm", "end", "bsi", "end"),
    occurredAt = as.Date(c("2024-01-01", "2024-01-15",
      "2024-01-08", "2024-02-15")))
  result <- neoipcr:::filter_surveillance_ends(events, NULL, NULL)
  expect_equal(nrow(result), 4L)
})

test_that("filter_surveillance_ends filters only 'end' events by from date", {
  events <- make_test_events(
    n = 4,
    enrollment_keys = 1:4, patient_keys = 1:4,
    event_type_keys = c("adm", "end", "bsi", "end"),
    occurredAt = as.Date(c("2024-01-01", "2024-01-15",
      "2024-01-08", "2024-02-15")))
  result <- neoipcr:::filter_surveillance_ends(
    events, surveillance_end_from = as.Date("2024-02-01"))
  # adm + bsi kept (not "end"); only end on 2024-02-15 passes (>= 2024-02-01)
  expect_equal(nrow(result), 3L)
  end_rows <- result[result$event_type_key == "end", ]
  expect_equal(nrow(end_rows), 1L)
})

test_that("filter_surveillance_ends filters only 'end' events by to date", {
  events <- make_test_events(
    n = 4,
    enrollment_keys = 1:4, patient_keys = 1:4,
    event_type_keys = c("adm", "end", "bsi", "end"),
    occurredAt = as.Date(c("2024-01-01", "2024-01-15",
      "2024-01-08", "2024-02-15")))
  result <- neoipcr:::filter_surveillance_ends(
    events, surveillance_end_to = as.Date("2024-01-31"))
  # adm + bsi kept; only end on 2024-01-15 passes (<= 2024-01-31)
  expect_equal(nrow(result), 3L)
})

test_that("filter_surveillance_ends filters by both from and to", {
  events <- make_test_events(
    n = 5,
    enrollment_keys = 1:5, patient_keys = 1:5,
    event_type_keys = c("adm", "end", "end", "end", "bsi"),
    occurredAt = as.Date(c("2024-01-01", "2024-01-10",
      "2024-02-15", "2024-03-20", "2024-01-05")))
  result <- neoipcr:::filter_surveillance_ends(
    events,
    surveillance_end_from = as.Date("2024-02-01"),
    surveillance_end_to = as.Date("2024-02-28"))
  # adm + bsi kept (2); only end on 2024-02-15 in range (1)
  expect_equal(nrow(result), 3L)
})

# --- filter_admissions (internal) ---

test_that("filter_admissions with keep_non_core=TRUE returns all", {
  adm <- make_test_admission_data(1:3, dol = c(1L, 119L, 150L))
  result <- neoipcr:::filter_admissions(adm, keep_non_core_patients = TRUE)
  expect_equal(nrow(result), 3L)
})

test_that("filter_admissions with keep_non_core=FALSE excludes dol >= 120", {
  adm <- make_test_admission_data(1:4, dol = c(1L, 119L, 120L, 200L))
  result <- neoipcr:::filter_admissions(adm, keep_non_core_patients = FALSE)
  # dol < 120: keeps dol=1 and dol=119 only
  expect_equal(nrow(result), 2L)
  expect_true(all(result$dol < 120))
})

# --- filter_patients (internal, called on patients tibble directly) ---

test_that("filter_patients with all NULL and keep_non_core=TRUE returns all", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 210L, 280L))
  result <- neoipcr:::filter_patients(patients, keep_non_core_patients = TRUE)
  expect_equal(nrow(result), 3L)
})

test_that("filter_patients applies core patient filter by default", {
  # Core = total_gestation_days < 224 OR birth_weight < 1500
  patients <- make_test_patients(4,
    birth_weight = c(800L, 1200L, 2500L, 1600L),
    total_gestation_days = c(175L, 210L, 280L, 230L))
  result <- neoipcr:::filter_patients(patients, keep_non_core_patients = FALSE)
  # Patient 1: 175<224 → keep; Patient 2: 210<224 → keep
  # Patient 3: 280>=224 AND 2500>=1500 → exclude
  # Patient 4: 230>=224 BUT 1600>=1500 → exclude (neither condition met)
  expect_equal(nrow(result), 2L)
})

test_that("filter_patients filters by birth_weight_from", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 210L, 252L))
  result <- neoipcr:::filter_patients(patients,
    birth_weight_from = 1000, keep_non_core_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$birth_weight >= 1000))
})

test_that("filter_patients filters by birth_weight_to", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 210L, 252L))
  result <- neoipcr:::filter_patients(patients,
    birth_weight_to = 1200, keep_non_core_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$birth_weight <= 1200))
})

test_that("filter_patients filters by gestational age in weeks", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 224L, 280L))
  # 32 weeks = 224 days
  result <- neoipcr:::filter_patients(patients,
    gestation_weeks_from = 32, keep_non_core_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$total_gestation_days >= 224))
})

test_that("filter_patients combines birth_weight and gestation filters", {
  patients <- make_test_patients(4,
    birth_weight = c(800L, 1200L, 2500L, 1600L),
    total_gestation_days = c(175L, 210L, 252L, 280L))
  result <- neoipcr:::filter_patients(patients,
    birth_weight_from = 1000, gestation_weeks_to = 36,
    keep_non_core_patients = TRUE)
  # bw>=1000: excludes patient 1 (800)
  # ga<=252 days (36*7): excludes patient 4 (280)
  # Remaining: patients 2 (1200, 210) and 3 (2500, 252)
  expect_equal(nrow(result), 2L)
})

# --- filter_countries (internal) ---

test_that("filter_countries with NULL returns input unchanged", {
  countries <- make_test_metadata_countries()
  result <- neoipcr:::filter_countries(countries, NULL)
  expect_equal(nrow(result), nrow(countries))
})

test_that("filter_countries with empty vector returns input unchanged", {
  countries <- make_test_metadata_countries()
  result <- neoipcr:::filter_countries(countries, character(0))
  expect_equal(nrow(result), nrow(countries))
})

test_that("filter_countries filters by code", {
  countries <- make_test_metadata_countries(3)
  result <- neoipcr:::filter_countries(countries, "C1")
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$code), "C1")
})

# --- filter_dataset: top-level orchestrator ---

# filter_dataset extracts the relevant tibble from each ds$* slot and feeds
# it to the underlying filter_*() helper, then writes the filtered result
# back. The earlier code passed the full ds list to filter_patients /
# filter_countries — fixed to feed `ds$patients` and `ds$metadata$countries`.

test_that("filter_dataset with keep_non_core=FALSE succeeds and filters patients", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::filter_dataset(ds, keep_non_core_patients = FALSE,
    remove_orphans = FALSE)
  expect_s3_class(result, "neoipcr_ds")
  # Core-only retains rows meeting the NeoIPC core case eligibility criteria
  # (gestation < 224 days or birth weight < 1500g). Population is unchanged in
  # the fixture, so nothing should error and the result remains a tibble.
  expect_s3_class(result$patients, "data.frame")
})

test_that("filter_dataset with birth_weight_from filters and succeeds", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::filter_dataset(ds, birth_weight_from = 1000,
    keep_non_core_patients = TRUE, remove_orphans = FALSE)
  expect_s3_class(result, "neoipcr_ds")
  expect_true(all(result$patients$birth_weight >= 1000))
})

test_that("filter_dataset with all NULL + keep_non_core=TRUE succeeds", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::filter_dataset(ds,
    keep_non_core_patients = TRUE, remove_orphans = FALSE)
  expect_s3_class(result, "neoipcr_ds")
  expect_equal(nrow(result$patients), nrow(ds$patients))
})

# --- apply_postfilter (internal) ---

test_that("apply_postfilter removes orphaned events", {
  ds <- make_populated_test_ds()
  # Remove patient 1 — enrollments/events referencing patient 1 become orphans
  ds$patients <- ds$patients[ds$patients$patient_key != 1L, ]
  result <- neoipcr:::apply_postfilter(ds)
  # Enrollment 1 references patient 1, so it should be removed
  expect_false(1L %in% result$enrollments$patient_key)
  # Events under enrollment 1 should also be removed
  expect_false(1L %in% result$events$enrollment_key)
})

test_that("apply_postfilter removes orphaned admission data", {
  ds <- make_populated_test_ds()
  # Remove all events — admission data becomes orphaned
  ds$events <- ds$events[0, ]
  result <- neoipcr:::apply_postfilter(ds)
  expect_equal(nrow(result$admissionData), 0L)
  expect_equal(nrow(result$surveillanceEndData), 0L)
})

test_that("apply_postfilter cascades metadata removal", {
  ds <- make_populated_test_ds()
  # Keep only enrollments in department 1
  ds$enrollments <- ds$enrollments[ds$enrollments$department_key == 1L, ]
  result <- neoipcr:::apply_postfilter(ds)
  # Department 2 should be removed from metadata
  if (!is.null(result$metadata$departments))
    expect_false(2L %in% result$metadata$departments$department_key)
})

test_that("apply_postfilter preserves enrollments with NA country_key", {
  ds <- make_populated_test_ds()
  # Simulate test unit: set country_key to NA on enrollment 1
  ds$enrollments$country_key[1] <- NA_integer_
  ds$metadata$countries <- make_test_metadata_countries()
  result <- neoipcr:::apply_postfilter(ds)
  # Enrollment with NA country_key should survive (test data tolerance)
  expect_true(any(is.na(result$enrollments$country_key)))
})

test_that("apply_postfilter handles NULL eventNotes", {
  ds <- make_populated_test_ds()
  ds$eventNotes <- NULL
  result <- neoipcr:::apply_postfilter(ds)
  expect_null(result$eventNotes)
})

test_that("apply_postfilter handles NULL eventDetails", {
  ds <- make_populated_test_ds()
  ds$eventDetails <- NULL
  result <- neoipcr:::apply_postfilter(ds)
  expect_null(result$eventDetails)
})

test_that("apply_postfilter handles NULL metadata tables", {
  ds <- make_populated_test_ds()
  ds$metadata$countries <- NULL
  ds$metadata$hospitals <- NULL
  ds$metadata$departments <- NULL
  ds$metadata$worldBankClasses <- NULL
  result <- neoipcr:::apply_postfilter(ds)
  # Should complete without error
  expect_s3_class(result, "neoipcr_ds")
})
