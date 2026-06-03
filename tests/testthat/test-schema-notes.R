# Tests for R/schema-notes.R — event_notes_cols + enrollment_notes_cols.

# ---- event_notes_cols three-mode shape -----------------------------------

test_that("event_notes_cols: 0x0 when include_event = 'no'", {
  opts <- dhis2_dataset_options(include_event = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("event_notes_cols: 0x0 when 'events' not in include_notes", {
  # Entity gate is compound — both include_event != "no" AND
  # "events" %in% include_notes must pass.
  opts <- dhis2_dataset_options(
    include_event = "full",
    include_notes = character())
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  expect_equal(ncol(schema), 0L)
})

test_that("event_notes_cols: pseudo-event keeps only event_key + inherited keys (payload absent)", {
  opts <- dhis2_dataset_options(
    include_event      = "pseudo",
    include_notes      = "events",
    include_enrollment = "full",
    include_patient    = "full",
    include_test_data  = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  # Under pseudo event, events_cols has only event_key → children
  # materialize enrollment_key + patient_key directly.
  expect_true("event_key"      %in% names(schema))
  expect_true("enrollment_key" %in% names(schema))
  expect_true("patient_key"    %in% names(schema))
  expect_true("isTest"         %in% names(schema))
  # Payload atoms require include_event == "full".
  for (col in c("note", "value", "storedBy", "storedAt", "createdBy"))
    expect_false(col %in% names(schema), info = col)
})

test_that("event_notes_cols: full-event minimal shape", {
  # Under full event + "events" %in% include_notes, with no user /
  # timestamp opt-in, the tibble has event_key + value (and `note`
  # only if id-opt-in is set).
  opts <- dhis2_dataset_options(
    include_event = "full",
    include_notes = "events")
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  expect_true("event_key" %in% names(schema))
  expect_true("value"     %in% names(schema))
  expect_false("note"      %in% names(schema))  # needs include_dhis2_ids
  expect_false("storedBy"  %in% names(schema))  # needs include_user
  expect_false("createdBy" %in% names(schema))  # needs include_user
  expect_false("storedAt"  %in% names(schema))  # needs include_timestamps
})

test_that("event_notes_cols: note gated on include_dhis2_ids == 'notes'", {
  opts_off <- dhis2_dataset_options(
    include_event     = "full",
    include_notes     = "events",
    include_dhis2_ids = character())
  opts_on <- dhis2_dataset_options(
    include_event     = "full",
    include_notes     = "events",
    include_dhis2_ids = "notes")
  expect_false("note" %in% names(neoipcr:::compile_schema(
    neoipcr:::event_notes_cols, opts_off)))
  expect_true("note" %in% names(neoipcr:::compile_schema(
    neoipcr:::event_notes_cols, opts_on)))
})

test_that("event_notes_cols: storedBy + createdBy gated on include_user", {
  opts_user <- dhis2_dataset_options(
    include_event = "full",
    include_notes = "events",
    include_user  = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts_user)
  expect_true("storedBy"  %in% names(schema))
  expect_true("createdBy" %in% names(schema))
  expect_type(schema$storedBy, "integer")   # user_key substituted
  expect_type(schema$createdBy, "integer")
})

test_that("event_notes_cols: storedAt gated on include_timestamps", {
  opts <- dhis2_dataset_options(
    include_event      = "full",
    include_notes      = "events",
    include_timestamps = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  expect_true("storedAt" %in% names(schema))
  expect_s3_class(schema$storedAt, "POSIXct")
})

test_that("event_notes_cols: hierarchy keys absent via inheritance under full events", {
  opts <- dhis2_dataset_options(
    include_event            = "full",
    include_notes            = "events",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  # Events carries these directly → children inherit (absent directly).
  for (col in c("enrollment_key", "patient_key", "department_key",
                "hospital_key", "country_key", "world_bank_class_key",
                "isTest"))
    expect_false(col %in% names(schema), info = col)
})

# ---- enrollment_notes_cols three-mode shape ------------------------------

test_that("enrollment_notes_cols: 0x0 when include_enrollment = 'no' or 'enrollments' not in include_notes", {
  for (opts in list(
    dhis2_dataset_options(include_enrollment = "no",
                          include_notes      = "enrollments"),
    dhis2_dataset_options(include_enrollment = "full",
                          include_notes      = character())
  )) {
    schema <- neoipcr:::compile_schema(
      neoipcr:::enrollment_notes_cols, opts)
    expect_equal(ncol(schema), 0L)
  }
})

test_that("enrollment_notes_cols: full mode with ids + user + timestamps includes full payload", {
  opts <- dhis2_dataset_options(
    include_enrollment = "full",
    include_notes      = "enrollments",
    include_dhis2_ids  = "notes",
    include_user       = "full",
    include_timestamps = TRUE)
  schema <- neoipcr:::compile_schema(
    neoipcr:::enrollment_notes_cols, opts)
  for (col in c("enrollment_key", "note", "value",
                "storedBy", "storedAt", "createdBy"))
    expect_true(col %in% names(schema), info = col)
})

test_that("enrollment_notes_cols: hierarchy absent when enrollments carries keys directly", {
  opts <- dhis2_dataset_options(
    include_enrollment       = "full",
    include_notes            = "enrollments",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  schema <- neoipcr:::compile_schema(
    neoipcr:::enrollment_notes_cols, opts)
  # Enrollments uses direct materialization — the fat-lookup deviation.
  # Children inherit (absent directly).
  for (col in c("patient_key", "department_key", "hospital_key",
                "country_key", "world_bank_class_key", "isTest"))
    expect_false(col %in% names(schema), info = col)
})

# ---- Fixture round-trips -------------------------------------------------

test_that("make_test_event_notes output matches event_notes_cols schema", {
  opts <- dhis2_dataset_options(
    include_event     = "full",
    include_notes     = "events",
    include_dhis2_ids = "notes")
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  fixture <- make_test_event_notes(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_enrollment_notes output matches enrollment_notes_cols schema", {
  opts <- dhis2_dataset_options(
    include_enrollment = "full",
    include_notes      = "enrollments",
    include_dhis2_ids  = "notes")
  schema <- neoipcr:::compile_schema(
    neoipcr:::enrollment_notes_cols, opts)
  fixture <- make_test_enrollment_notes(enrollment_keys = 1:2)
  expect_schema_matches(fixture, schema)
})
