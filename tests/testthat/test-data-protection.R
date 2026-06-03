# Tests for R/data-protection.R — assert_data_protection()
# The authoritative data-protection guardian (renamed from
# apply_data_removal in Phase C1).
#
# Semantics changed with the rename: the old scrubber removed columns
# that the user had opted out of; the new asserter verifies that the
# readers already removed them under the schema contract. Tests cover
# both the happy path (assertion passes when the ds is schema-compliant)
# and the failure path (assertion aborts when a reader regression leaks
# a forbidden column).
#
# Phase-b-event-details removed the `eventDetails` sidecar (merged into
# `events`); the last remaining scrub shim is gone, so the guardian is
# now purely assertions.

# Build a fully-populated dataset once for reuse across tests.
base_ds <- make_populated_test_ds()

# Helper: run assert_data_protection with specific options.
# Defaults keep everything; overrides via ... replace specific flags.
guard_with <- function(ds = base_ds, ...) {
  defaults <- list(
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    patient_columns          = "id",
    include_dhis2_ids        = c("patients", "enrollments", "departments",
                                 "events", "notes", "event_types", "users"))
  args <- utils::modifyList(defaults, list(...))
  opts <- do.call(dhis2_dataset_options, args)
  neoipcr:::assert_data_protection(ds, opts)
}

# Helper: strip a hierarchy key from the fact and metadata targets
# (simulating what a schema-compliant reader would produce under
# `include_X = "no"`).
strip_key <- function(ds, col, fact_targets = c("patients", "enrollments", "events"),
                     metadata_targets = character()) {
  for (t in fact_targets)
    ds[[t]] <- ds[[t]] |> dplyr::select(!tidyselect::any_of(col))
  for (t in metadata_targets) {
    if (!is.null(ds$metadata[[t]]))
      ds$metadata[[t]] <- ds$metadata[[t]] |>
        dplyr::select(!tidyselect::any_of(col))
  }
  ds
}

# --- Happy path: full opts + full fixture -----------------------------

test_that("assert_data_protection passes under full opts + full fixture", {
  expect_no_error(guard_with())
  # And actually returns the dataset unchanged.
  result <- guard_with()
  expect_true("department_key"       %in% names(result$patients))
  expect_true("hospital_key"         %in% names(result$metadata$departments))
  expect_true("country_key"          %in% names(result$metadata$hospitals))
  expect_true("world_bank_class_key" %in% names(result$metadata$countries))
})

# --- Happy path: narrow opts with fact tables honoring the gate -------

test_that("assert_data_protection passes under include_department = 'no' when fact tables honor the gate", {
  ds <- strip_key(base_ds, "department_key")
  expect_no_error(guard_with(ds = ds, include_department = "no"))
})

test_that("assert_data_protection passes under include_hospital = 'no' when fact + metadata honor the gate", {
  ds <- strip_key(base_ds, "hospital_key",
                  metadata_targets = c("departments"))
  expect_no_error(guard_with(ds = ds, include_hospital = "no"))
})

test_that("assert_data_protection passes under include_country = 'no' when fact + metadata honor the gate", {
  ds <- strip_key(base_ds, "country_key",
                  metadata_targets = c("hospitals", "departments"))
  expect_no_error(guard_with(ds = ds, include_country = "no"))
})

test_that("assert_data_protection passes under include_world_bank_class = 'no' when fact + metadata honor the gate", {
  ds <- strip_key(base_ds, "world_bank_class_key",
                  metadata_targets = c("countries", "hospitals", "departments"))
  expect_no_error(guard_with(ds = ds, include_world_bank_class = "no"))
})

# --- Failure path: schema regression leaks a forbidden key ------------
#
# These tests simulate a reader regression by keeping the populated
# fixture (which carries every hierarchy key) and asking the guardian
# to verify the ds against narrower opts. The guardian must abort with
# an actionable message that names the leaked column and the leaking
# tibble.

test_that("assert_data_protection aborts when department_key leaks under include_department = 'no'", {
  expect_error(
    guard_with(include_department = "no"),
    "department_key")
  expect_error(
    guard_with(include_department = "no"),
    "include_department")
  expect_error(
    guard_with(include_department = "no"),
    "x\\$patients")
})

test_that("assert_data_protection aborts when hospital_key leaks under include_hospital = 'no'", {
  expect_error(
    guard_with(include_hospital = "no"),
    "hospital_key")
  expect_error(
    guard_with(include_hospital = "no"),
    "x\\$metadata\\$departments")
})

test_that("assert_data_protection aborts when country_key leaks under include_country = 'no'", {
  expect_error(
    guard_with(include_country = "no"),
    "country_key")
  expect_error(
    guard_with(include_country = "no"),
    "x\\$metadata\\$hospitals")
})

test_that("assert_data_protection aborts when world_bank_class_key leaks under include_world_bank_class = 'no'", {
  expect_error(
    guard_with(include_world_bank_class = "no"),
    "world_bank_class_key")
  expect_error(
    guard_with(include_world_bank_class = "no"),
    "x\\$metadata\\$countries")
})

# --- pseudo / full modes must not trip the "absent" assertion ---------

test_that("assert_data_protection passes under include_department = 'pseudo' with key present", {
  expect_no_error(guard_with(include_department = "pseudo"))
})

test_that("assert_data_protection passes under include_country = 'pseudo' with key present", {
  expect_no_error(guard_with(include_country = "pseudo"))
})

test_that("assert_data_protection passes under include_world_bank_class = 'pseudo' with key present", {
  expect_no_error(guard_with(include_world_bank_class = "pseudo"))
})

# --- Cascading failures: multiple leaks in one message -----------------

test_that("assert_data_protection aborts naming multiple leak sites under include_country = 'no'", {
  # `country_key` leaks on patients, enrollments, events, hospitals, departments.
  # All five leak sites should appear in the single abort message.
  err <- tryCatch(
    guard_with(include_country = "no"),
    error = identity)
  expect_s3_class(err, "rlang_error")
  msg <- conditionMessage(err)
  expect_match(msg, "x\\$patients")
  expect_match(msg, "x\\$enrollments")
  expect_match(msg, "x\\$events")
  expect_match(msg, "x\\$metadata\\$hospitals")
  expect_match(msg, "x\\$metadata\\$departments")
})

# `eventDetails` scrub-shim tests removed in phase-b-event-details —
# the sidecar tibble was merged into `events`, and the `event` id on
# events is now schema-gated by `events_cols`. Coverage of the
# `events$event` gate lives in `test-schema-events.R`.

# --- Full-restriction smoke -------------------------------------------

test_that("assert_data_protection passes under all-'no' opts when fact + metadata tables are honored", {
  ds <- base_ds |>
    strip_key("department_key") |>
    strip_key("hospital_key",   metadata_targets = c("departments")) |>
    strip_key("country_key",    metadata_targets = c("hospitals", "departments")) |>
    strip_key("world_bank_class_key",
              metadata_targets = c("countries", "hospitals", "departments"))

  expect_no_error(
    guard_with(
      ds                        = ds,
      patient_columns           = character(),
      include_dhis2_ids         = character(),
      include_department        = "no",
      include_hospital          = "no",
      include_country           = "no",
      include_world_bank_class  = "no"))
})
