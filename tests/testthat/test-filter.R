# Tests for R/filter.R — dataset filtering and orphan removal.
# Uses make_populated_test_ds() from helper-fixtures.R.

# --- filter_enrollments_by_surveillance_end (internal) ---

# Five enrolments: 1 and 5 have no surveillance-end event, 2, 3 and 4 have
# one dated in January, February and March.
period_enrollments <- function() make_test_enrollments(5, patient_keys = 1:5)
period_events <- function() make_test_events(
  n = 6,
  enrollment_keys = c(1L, 2L, 3L, 4L, 5L, 2L), patient_keys = c(1L, 2L, 3L, 4L, 5L, 2L),
  event_type_keys = c("adm", "end", "end", "end", "bsi", "adm"),
  occurredAt = as.Date(c("2024-01-01", "2024-01-15", "2024-02-15", "2024-03-20",
                         "2024-01-05", "2024-01-02")))

test_that("filter_enrollments_by_surveillance_end with both NULL returns the enrolments unchanged", {
  result <- neoipcr:::filter_enrollments_by_surveillance_end(
    period_enrollments(), period_events(), NULL, NULL)
  expect_equal(result$enrollment_key, 1:5)
})

test_that("filter_enrollments_by_surveillance_end keeps the enrolments ended on or after the from date", {
  result <- neoipcr:::filter_enrollments_by_surveillance_end(
    period_enrollments(), period_events(), surveillance_end_from = as.Date("2024-02-15"))
  # The bound is inclusive; an enrolment without an end event has no date
  # to fall in the window and is left out.
  expect_equal(result$enrollment_key, c(3L, 4L))
})

test_that("filter_enrollments_by_surveillance_end keeps the enrolments ended on or before the to date", {
  result <- neoipcr:::filter_enrollments_by_surveillance_end(
    period_enrollments(), period_events(), surveillance_end_to = as.Date("2024-02-15"))
  expect_equal(result$enrollment_key, c(2L, 3L))
})

test_that("filter_enrollments_by_surveillance_end keeps the enrolments ended within both bounds", {
  result <- neoipcr:::filter_enrollments_by_surveillance_end(
    period_enrollments(), period_events(),
    surveillance_end_from = as.Date("2024-02-01"),
    surveillance_end_to = as.Date("2024-02-28"))
  expect_equal(result$enrollment_key, 3L)
  # A window no end falls in leaves nothing, the enrolments without an end
  # event included.
  result <- neoipcr:::filter_enrollments_by_surveillance_end(
    period_enrollments(), period_events(),
    surveillance_end_from = as.Date("2025-01-01"),
    surveillance_end_to = as.Date("2025-12-31"))
  expect_equal(nrow(result), 0L)
})

# --- narrow_to_surveillance_period (internal) ---

test_that("narrow_to_surveillance_period leaves out the enrolments, events and patients outside the period", {
  result <- neoipcr:::narrow_to_surveillance_period(
    make_test_patients(5), period_enrollments(), period_events(),
    surveillance_end_from = as.Date("2024-02-01"))
  expect_equal(result$enrollments$enrollment_key, c(3L, 4L))
  # The events of the enrolments left out go with them.
  expect_equal(sort(result$events$enrollment_key), c(3L, 4L))
  # A patient whose every enrolment is left out goes too.
  expect_equal(result$patients$patient_key, c(3L, 4L))
})

test_that("narrow_to_surveillance_period keeps a patient that arrived without an enrolment", {
  # Patient 6 has no enrolment: the period cannot say anything about it,
  # and rule 1 is the one to report it.
  result <- neoipcr:::narrow_to_surveillance_period(
    make_test_patients(6), period_enrollments(), period_events(),
    surveillance_end_to = as.Date("2024-01-31"))
  expect_equal(result$patients$patient_key, c(2L, 6L))
})

test_that("narrow_to_surveillance_period reads the enrolled patients off the events when the enrolments carry no patient link", {
  enrollments <- period_enrollments() |> dplyr::select(!"patient_key")
  result <- neoipcr:::narrow_to_surveillance_period(
    make_test_patients(5), enrollments, period_events(),
    surveillance_end_from = as.Date("2024-02-01"))
  expect_equal(result$patients$patient_key, c(3L, 4L))
})

test_that("narrow_to_surveillance_period leaves a tier without the enrolment link on the events unchanged", {
  patients <- make_test_patients(2)
  events <- period_events() |> dplyr::select(!"enrollment_key")
  result <- neoipcr:::narrow_to_surveillance_period(
    patients, tibble::tibble(), events, surveillance_end_from = as.Date("2024-02-01"))
  expect_identical(result$patients, patients)
  expect_identical(result$events, events)
  expect_equal(ncol(result$enrollments), 0L)
  # No bound leaves everything as it is.
  result <- neoipcr:::narrow_to_surveillance_period(patients, period_enrollments(), period_events())
  expect_identical(result$enrollments, period_enrollments())
  expect_identical(result$patients, patients)
})

# --- filter_admissions (internal) ---

test_that("filter_admissions with include_ineligible=TRUE returns all", {
  adm <- make_test_admission_data(1:3, dol = c(1L, 119L, 150L))
  result <- neoipcr:::filter_admissions(adm, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 3L)
})

test_that("filter_admissions with include_ineligible=FALSE excludes dol >= 120", {
  adm <- make_test_admission_data(1:4, dol = c(1L, 119L, 120L, 200L))
  result <- neoipcr:::filter_admissions(adm, include_ineligible_patients = FALSE)
  # dol < 120: keeps dol=1 and dol=119 only
  expect_equal(nrow(result), 2L)
  expect_true(all(result$dol < 120))
})

test_that("filter_admissions passes a 0x0 admission tibble through under the default filter", {
  # An `include_event = "no"` import carries no admission data at all; the
  # eligibility filter must not look for `dol` there.
  result <- neoipcr:::filter_admissions(tibble::tibble(), include_ineligible_patients = FALSE)
  expect_equal(ncol(result), 0L)
  expect_equal(nrow(result), 0L)
})

test_that("filter_admissions still requires `dol` on a populated admission tibble", {
  expect_error(neoipcr:::filter_admissions(
    tibble::tibble(event_key = 1L), include_ineligible_patients = FALSE))
})

# --- filter_patients (internal, called on patients tibble directly) ---

test_that("filter_patients with all NULL and include_ineligible=TRUE returns all", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 210L, 280L))
  result <- neoipcr:::filter_patients(patients, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 3L)
})

test_that("filter_patients applies core patient filter by default", {
  # Core = total_gestation_days < 224 OR birth_weight < 1500
  patients <- make_test_patients(4,
    birth_weight = c(800L, 1200L, 2500L, 1600L),
    total_gestation_days = c(175L, 210L, 280L, 230L))
  result <- neoipcr:::filter_patients(patients, include_ineligible_patients = FALSE)
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
    birth_weight_from = 1000, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$birth_weight >= 1000))
})

test_that("filter_patients filters by birth_weight_to", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 210L, 252L))
  result <- neoipcr:::filter_patients(patients,
    birth_weight_to = 1200, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$birth_weight <= 1200))
})

test_that("filter_patients filters by gestational age in weeks", {
  patients <- make_test_patients(3,
    birth_weight = c(800L, 1200L, 2500L),
    total_gestation_days = c(175L, 224L, 280L))
  # 32 weeks = 224 days
  result <- neoipcr:::filter_patients(patients,
    gestational_age_from = 32, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$total_gestation_days >= 224))
})

test_that("filter_patients gestational_age_to keeps the whole completed week", {
  # `gestational_age_to = 31` means 31 completed weeks: 31+0 (217 days) and
  # 31+6 (223 days) stay in, 32+0 (224 days) drops out.
  patients <- make_test_patients(3,
    birth_weight = c(800L, 800L, 800L),
    total_gestation_days = c(217L, 223L, 224L))
  result <- neoipcr:::filter_patients(patients,
    gestational_age_to = 31, include_ineligible_patients = TRUE)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$total_gestation_days < 224L))
})

test_that("filter_patients combines birth_weight and gestation filters", {
  patients <- make_test_patients(4,
    birth_weight = c(800L, 1200L, 2500L, 1600L),
    total_gestation_days = c(175L, 210L, 252L, 280L))
  result <- neoipcr:::filter_patients(patients,
    birth_weight_from = 1000, gestational_age_to = 36,
    include_ineligible_patients = TRUE)
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

# --- filter_dataset (new signature: takes dhis2_dataset_options) ---
#
# Pre phase-c-audit the function took individual args
# (`birth_weight_from`, `include_ineligible_patients`, …) AND passed the
# full `neoipcr_ds` to `filter_patients`/`filter_countries` (which
# expect component tibbles). Both are fixed in C3: the signature takes
# a `dhis2_dataset_options` object, and the helpers get `x$patients` /
# `x$metadata$countries` respectively. Tests below cover the happy
# paths that the old signature either crashed on or failed to exercise.

test_that("filter_dataset(opts) with default opts does not crash", {
  # The old signature crashed here (passing full `x` to
  # `filter_patients` → `dplyr::filter` on a list). Default opts has
  # `include_ineligible_patients = FALSE`, which under the old code
  # was the bug-trigger path.
  ds <- make_populated_test_ds()
  opts <- dhis2_dataset_options()
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = FALSE)
  expect_s3_class(result, "neoipcr_ds")
})

test_that("filter_dataset(opts) with birth_weight_from narrows patients", {
  ds <- make_populated_test_ds()
  # Ensure the fixture has a range we can narrow; make_test_patients
  # default birth_weight = 1500 for every row, so BW >= 1600 is empty.
  opts <- dhis2_dataset_options(
    birth_weight_from            = 1600,
    include_ineligible_patients  = TRUE)  # don't re-apply core filter
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = FALSE)
  expect_equal(nrow(result$patients), 0L)
})

test_that("filter_dataset(opts) with include_ineligible_patients = TRUE keeps all", {
  ds <- make_populated_test_ds()
  opts <- dhis2_dataset_options(include_ineligible_patients = TRUE)
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = FALSE)
  expect_equal(nrow(result$patients), nrow(ds$patients))
})

test_that("filter_dataset(opts) with country_filter narrows countries", {
  ds <- make_populated_test_ds()
  # Capture pre-filter country codes to pick one that exists.
  pre <- as.character(ds$metadata$countries$code)
  opts <- dhis2_dataset_options(
    country_filter              = pre[1],
    include_ineligible_patients = TRUE)
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = FALSE)
  expect_true(all(as.character(result$metadata$countries$code) == pre[1]))
})

test_that("filter_dataset(opts) with a period leaves out the stays outside it and the patients they leave unenrolled", {
  # The populated fixture ends its three enrolments on 2024-01-15, -16 and
  # -17, one per patient.
  ds <- make_populated_test_ds()
  opts <- dhis2_dataset_options(
    surveillance_end_from       = as.Date("2024-01-16"),
    include_ineligible_patients = TRUE)
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = FALSE)
  expect_equal(result$enrollments$enrollment_key, c(2L, 3L))
  expect_setequal(result$events$enrollment_key, c(2L, 3L))
  expect_equal(result$patients$patient_key, c(2L, 3L))
  # Patient 1 had a stay and keeps none: it is not an unenrolled patient for
  # the cascade to keep, whatever the dataset was requested with.
  ds$metadata$dataset_options$include_unenrolled_patients <- TRUE
  result <- neoipcr:::filter_dataset(ds, opts, remove_orphans = TRUE)
  expect_false(1L %in% result$patients$patient_key)
  expect_equal(nrow(neoipcr::validate(result, rules = 1L)), 0L)
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

test_that("apply_postfilter removes the pathogen names of removed findings", {
  # The free-text names hang off the findings by `agent_finding_key`; a
  # finding removed with its event takes its name with it, so nothing of a
  # removed patient's pathogens survives.
  ds <- make_populated_test_ds(
    infectiousAgentFindings = make_test_iaf(c(1L, 2L)),
    unknownPathogenNames    = make_test_unknown_pathogen_names(c(1L, 2L)))
  ds$patients <- ds$patients[ds$patients$patient_key != 1L, ]
  result <- neoipcr:::apply_postfilter(ds)
  expect_equal(result$infectiousAgentFindings$agent_finding_key, 2L)
  expect_equal(result$unknownPathogenNames$agent_finding_key, 2L)
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

test_that("apply_postfilter handles 0x0 eventNotes (was NULL pre-schema)", {
  # Under the schema contract eventNotes is always a tibble (never
  # NULL) — the entity gate produces 0x0 when the user opts out.
  # apply_postfilter() uses a column-presence guard for the
  # event_key semi-join.
  ds <- make_populated_test_ds()
  ds$eventNotes <- tibble::tibble()
  result <- neoipcr:::apply_postfilter(ds)
  expect_s3_class(result$eventNotes, "tbl_df")
  expect_equal(ncol(result$eventNotes), 0L)
})

# `eventDetails` was merged into `events` in phase-b-event-details; the
# slot no longer exists. The former "handles NULL eventDetails" test is
# obsolete under the schema contract and has been removed.

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


# --- apply_postfilter: fixed-point + dynamic-anchor cascade (C4) ---
#
# The C4 rewrite replaced the legacy one-pass enrollments-anchored
# cascade with a fixed-point loop of (a) link-FK orphan cleanup + (b)
# hierarchy-metadata upward cascade using a per-key dynamic anchor
# (union of every data-carrying tibble that carries the key, excluding
# the target). These tests verify the new behaviours that the legacy
# cascade couldn't handle.

test_that("apply_postfilter: multi-level cascade converges (hospital -> department -> country -> WB class)", {
  ds <- make_populated_test_ds()

  # Keep only enrollments in department 1 (department 2 should
  # cascade all the way up through hospitals, countries, WB classes if
  # nothing else keeps them alive).
  ds$enrollments <- ds$enrollments[ds$enrollments$department_key == 1L, ]

  result <- neoipcr:::apply_postfilter(ds)

  # Direct cascade: department 2 removed.
  expect_false(2L %in% result$metadata$departments$department_key)

  # Cascade further: hospital_key / country_key / world_bank_class_key
  # values uniquely belonging to department 2 should also be gone from
  # their respective metadata tibbles (since the only anchor carrying
  # them was enrollments → departments → … → that hierarchy row).
  surviving_hospital_keys <-
    result$metadata$departments$hospital_key
  surviving_country_keys <-
    result$metadata$departments$country_key
  surviving_wb_keys <-
    result$metadata$departments$world_bank_class_key

  # Every remaining hierarchy row must appear in `surviving_*_keys`
  # (union of what the departments anchor carries).
  expect_true(all(
    result$metadata$hospitals$hospital_key %in% surviving_hospital_keys))
  expect_true(all(
    result$metadata$countries$country_key %in% surviving_country_keys))
  expect_true(all(
    result$metadata$worldBankClasses$world_bank_class_key %in%
      surviving_wb_keys))
})

test_that("apply_postfilter: cascade runs to fixed point (multi-iteration)", {
  # Construct a ds where the cascade needs >1 iteration: removing
  # enrollment triggers patient removal (iteration 1); patient removal
  # in turn removes its only surviving enrollment (iteration 2); etc.
  # We verify convergence by asserting the function returns (no hang).

  ds <- make_populated_test_ds()

  # Remove all events — this drops every per-event-type data tibble,
  # but enrollments stay via the surveillance_end admission check
  # (which runs once in the prefilter, not the cascade). The cascade
  # should still terminate.
  ds$events <- ds$events[0, ]

  # This will hang (infinite loop) if the fixed-point check is broken.
  # `expect_no_error` + the implicit timeout is enough — testthat
  # aborts the whole run if a single test hangs.
  expect_no_error(result <- neoipcr:::apply_postfilter(ds))
  # Per-event-type data all drops to 0 rows.
  expect_equal(nrow(result$admissionData), 0L)
  expect_equal(nrow(result$surveillanceEndData), 0L)
})

test_that("apply_postfilter: link-FK cascade tolerates include_event = 'no' (no event_key)", {
  # Under `include_event = "no"`, every per-event-type data tibble is
  # 0x0 (no `event_key` column). Every `event_key`-based semi-join in
  # the cascade must be skipped via the column-presence guard.
  ds <- make_populated_test_ds()
  ds$events <- tibble::tibble()  # simulate 0x0 under include_event = "no"
  ds$admissionData <- tibble::tibble()
  ds$surveillanceEndData <- tibble::tibble()

  expect_no_error(result <- neoipcr:::apply_postfilter(ds))
  expect_s3_class(result, "neoipcr_ds")
})

test_that("apply_postfilter: link-FK cascade tolerates include_patient = 'no' (no patient_key)", {
  ds <- make_populated_test_ds()
  # Drop `patient_key` from every fact tibble that carries it —
  # simulates include_patient = "no" at the schema level.
  drop_patient_key <- function(tib) {
    if ("patient_key" %in% names(tib))
      tib |> dplyr::select(!"patient_key")
    else tib
  }
  ds$patients    <- tibble::tibble()  # 0x0 under gate
  ds$enrollments <- drop_patient_key(ds$enrollments)
  ds$events      <- drop_patient_key(ds$events)

  expect_no_error(result <- neoipcr:::apply_postfilter(ds))
  expect_s3_class(result, "neoipcr_ds")
})

# --- apply_postfilter: droplevels on data-sourced factors (C5) ---
#
# Protocol-fixed factors keep their full declared level list regardless
# of which values survive filtering (schema-contract invariant).
# Data-sourced factors (`levels_source = "data"` in the schema) have
# `droplevels()` applied after the cascade so the level list matches
# the surviving rows — preventing the "this country / event-type
# existed in the pre-filter data" leak.

test_that("apply_postfilter: droplevels on data-sourced factor levels (countries)", {
  ds <- make_populated_test_ds()
  # Record pre-filter country-code levels to verify the leak scenario:
  # the fixture must carry >1 country-code level.
  pre_levels <- levels(ds$metadata$countries$code)
  skip_if(length(pre_levels) < 2L,
          "fixture has only one country-code level — cannot exercise the leak")

  # Narrow to the first country only, via metadata direct filtering
  # (simulates what filter_dataset does with country_filter).
  kept_code <- as.character(ds$metadata$countries$code[1])
  ds$metadata$countries <- ds$metadata$countries[
    as.character(ds$metadata$countries$code) == kept_code, ]

  result <- neoipcr:::apply_postfilter(ds)

  # Only the surviving code should remain in `levels()`. Without the
  # droplevels step, the other pre-filter codes would still appear.
  expect_equal(
    as.character(levels(result$metadata$countries$code)),
    kept_code)
})

test_that("apply_postfilter: droplevels leaves protocol-fixed factor levels untouched (status / event_type_key)", {
  ds <- make_populated_test_ds()
  # Narrow events to only "adm" type.
  ds$events <- ds$events[ds$events$event_type_key == "adm", ]

  result <- neoipcr:::apply_postfilter(ds)

  # `event_type_key` on events is declared `levels_source = "fixed"`
  # (7 protocol event types). Even though only "adm" survives filtering,
  # the level list must retain all 7 — dropping would break the schema
  # contract.
  if (is.factor(result$events$event_type_key))
    expect_setequal(
      levels(result$events$event_type_key),
      c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))
})

test_that("apply_postfilter: dynamic anchor handles pseudo-event (hierarchy keys on per-event-type data)", {
  # Under `include_event = "pseudo"` events is PK-only (no hierarchy
  # keys). If per-event-type data carries hierarchy keys via
  # inheritance, the dynamic anchor should pick those up for the
  # metadata cascade.
  ds <- make_populated_test_ds()

  # Strip hierarchy keys from events (simulate pseudo mode).
  ds$events <- ds$events |>
    dplyr::select(!tidyselect::any_of(c(
      "department_key", "hospital_key", "country_key",
      "world_bank_class_key")))

  # Strip hierarchy keys from enrollments too (simulate that the
  # anchor has shifted deeper than the legacy enrollments-only code
  # could handle). Admissions don't carry hierarchy keys in the fixture,
  # but departments still does via the metadata pre-join — so the
  # cascade uses `metadata$departments` as the anchor for the other
  # three keys.
  ds$enrollments <- ds$enrollments |>
    dplyr::select(!tidyselect::any_of(c(
      "hospital_key", "country_key", "world_bank_class_key")))

  expect_no_error(result <- neoipcr:::apply_postfilter(ds))
  # The cascade should have left countries / hospitals / WB classes
  # non-empty (they're anchored off departments, which still carries
  # the keys).
  expect_gt(nrow(result$metadata$countries), 0L)
  expect_gt(nrow(result$metadata$hospitals), 0L)
  expect_gt(nrow(result$metadata$worldBankClasses), 0L)
})

# --- apply_postfilter: org-unit attribute-value leaves ---
#
# The values tables hang off departments / hospitals: they are pruned to
# the surviving parents once the hierarchy has settled, and never keep a
# parent alive on their own.

test_that("apply_postfilter prunes the attribute values of a pruned department and hospital", {
  ds <- make_populated_test_ds(orgunit_attributes = TRUE)
  ds$enrollments <- ds$enrollments[ds$enrollments$department_key == 1L, ]

  result <- neoipcr:::apply_postfilter(ds)

  expect_false(2L %in% result$metadata$departments$department_key)
  expect_false(2L %in% result$metadata$departmentAttributeValues$department_key)
  expect_true(1L %in% result$metadata$departmentAttributeValues$department_key)
  expect_true(all(
    result$metadata$hospitalAttributeValues$hospital_key %in%
      result$metadata$hospitals$hospital_key))
})

test_that("apply_postfilter: a value row alone does not keep its org unit alive", {
  ds <- make_populated_test_ds(orgunit_attributes = TRUE)
  ds$enrollments <- ds$enrollments[ds$enrollments$department_key == 1L, ]
  ds$metadata$departmentAttributeValues <-
    make_test_metadata_department_attribute_values(
      department_keys = c(1L, 2L, 99L))

  result <- neoipcr:::apply_postfilter(ds)

  expect_false(99L %in% result$metadata$departments$department_key)
  expect_setequal(result$metadata$departmentAttributeValues$department_key, 1L)
})

test_that("apply_postfilter tolerates absent or 0x0 attribute-value tables", {
  ds <- make_populated_test_ds()
  ds$metadata$departmentAttributeValues <- NULL
  expect_no_error(neoipcr:::apply_postfilter(ds))
  ds$metadata$departmentAttributeValues <- tibble::tibble()
  expect_no_error(neoipcr:::apply_postfilter(ds))
})

test_that("apply_postfilter keeps a patient without enrolments only when the dataset asked for them", {
  ds <- make_populated_test_ds()
  # Take patient 1's enrollments away; the patient row itself stays.
  ds$enrollments <- ds$enrollments[ds$enrollments$patient_key != 1L, ]

  pruned <- neoipcr:::apply_postfilter(ds)
  expect_false(1L %in% pruned$patients$patient_key)

  ds$metadata$dataset_options$include_unenrolled_patients <- TRUE
  kept <- neoipcr:::apply_postfilter(ds)
  expect_true(1L %in% kept$patients$patient_key)
  expect_false(1L %in% kept$enrollments$patient_key)
  expect_false(1L %in% kept$events$patient_key)
  # Nothing but the patient row refers to department 1 any more, and the
  # patient alone keeps it in the hierarchy metadata.
  expect_true(1L %in% kept$metadata$departments$department_key)
})

test_that("apply_postfilter still prunes a patient whose enrolments the cascade removed when unenrolled patients are kept", {
  ds <- make_populated_test_ds()
  ds$metadata$dataset_options$include_unenrolled_patients <- TRUE
  # Enrolment 1 loses its admission event and data, so the prefilter drops
  # it; the patient arrived enrolled and must go with it rather than surface
  # as unenrolled.
  admission <- ds$events$event_key[
    ds$events$enrollment_key == 1L & ds$events$event_type_key == "adm"]
  ds$events        <- ds$events[!(ds$events$event_key %in% admission), ]
  ds$admissionData <- ds$admissionData[
    !(ds$admissionData$event_key %in% admission), ]

  result <- neoipcr:::apply_postfilter(ds)
  expect_false(1L %in% result$enrollments$enrollment_key)
  expect_false(1L %in% result$patients$patient_key)
})

test_that("apply_postfilter keeps an enrolment without an admission form only when invalid patients are kept", {
  without_admission <- function() {
    ds <- make_populated_test_ds()
    admission <- ds$events$event_key[
      ds$events$enrollment_key == 1L & ds$events$event_type_key == "adm"]
    ds$events        <- ds$events[!(ds$events$event_key %in% admission), ]
    ds$admissionData <- ds$admissionData[
      !(ds$admissionData$event_key %in% admission), ]
    ds
  }

  # The invariant holds for an import that runs the validation pass, whether
  # without exceptions or with a list of them.
  ds <- without_admission()
  ds$metadata$dataset_options$include_invalid_patients <- FALSE
  expect_false(1L %in% neoipcr:::apply_postfilter(ds)$enrollments$enrollment_key)
  ds$metadata$dataset_options$include_invalid_patients <- make_test_exceptions(26L)
  expect_false(1L %in% neoipcr:::apply_postfilter(ds)$enrollments$enrollment_key)

  # Skipping the pass asks for the records it would remove, and this is one.
  ds$metadata$dataset_options$include_invalid_patients <- TRUE
  kept <- neoipcr:::apply_postfilter(ds)
  expect_true(1L %in% kept$enrollments$enrollment_key)
  expect_true(1L %in% kept$patients$patient_key)
})
