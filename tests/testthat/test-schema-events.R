# Tests for R/schema-events.R — events_cols.

# ---- events_cols three-mode shape ----------------------------------------

test_that("events_cols: 'no' mode returns 0x0 via the entity gate", {
  opts <- dhis2_dataset_options(include_event = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_s3_class(schema, "tbl_df")
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("events_cols: 'pseudo' mode is strictly event_key only", {
  opts <- dhis2_dataset_options(
    include_event            = "pseudo",
    # Every orthogonal gate is open, yet pseudo stays 1-col because
    # every non-PK atom compounds with `include_event == "full"`.
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = c("events", "enrollments", "patients"),
    include_incomplete       = "events",
    include_user             = "full",
    include_timestamps       = TRUE,
    include_test_data        = TRUE,
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_identical(names(schema), "event_key")
})

test_that("events_cols: 'full' minimal — event_key + occurredAt + event_type_key + enrollment_key + patient_key + followup", {
  opts <- dhis2_dataset_options(
    include_event      = "full",
    include_enrollment = "full",
    include_patient    = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  # `followup` is always declared under full mode (no sub-gate).
  # Mirrors enrollments' `followUp`. The other entity-level companions
  # (user / timestamp / deleted) need include_user / include_timestamps
  # / include_deleted to be set and are covered in the dedicated tests
  # further down.
  expect_identical(
    names(schema),
    c("event_key", "occurredAt", "event_type_key",
      "enrollment_key", "patient_key", "followup"))
})

test_that("events_cols: event id gated on include_dhis2_ids", {
  opts_no <- dhis2_dataset_options(include_event     = "full",
                                   include_dhis2_ids = character())
  opts_yes <- dhis2_dataset_options(include_event     = "full",
                                    include_dhis2_ids = "events")
  expect_false("event" %in% names(
    neoipcr:::compile_schema(neoipcr:::events_cols, opts_no)))
  expect_true("event" %in% names(
    neoipcr:::compile_schema(neoipcr:::events_cols, opts_yes)))
})

test_that("events_cols: enrollment_key link FK gated by both sides", {
  # events = "full" but include_enrollment = "no" → no link FK.
  opts_no_enr <- dhis2_dataset_options(include_event      = "full",
                                       include_enrollment = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_no_enr)
  expect_false("enrollment_key" %in% names(schema))

  # Both sides open → link present.
  opts_both <- dhis2_dataset_options(include_event      = "full",
                                     include_enrollment = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_both)
  expect_true("enrollment_key" %in% names(schema))
})

test_that("events_cols: patient_key secondary link FK gated by both sides", {
  # events = "full" but include_patient = "no" → no patient_key link.
  opts_no_pat <- dhis2_dataset_options(include_event   = "full",
                                       include_patient = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_no_pat)
  expect_false("patient_key" %in% names(schema))

  # Both sides open → link present.
  opts_both <- dhis2_dataset_options(include_event   = "full",
                                     include_patient = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_both)
  expect_true("patient_key" %in% names(schema))
})

test_that("events_cols: status gated on include_incomplete", {
  opts_off <- dhis2_dataset_options(include_event      = "full",
                                    include_incomplete = character())
  opts_on  <- dhis2_dataset_options(include_event      = "full",
                                    include_incomplete = "events")
  schema_off <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_off)
  schema_on  <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_on)
  expect_false("status" %in% names(schema_off))
  expect_true("status" %in% names(schema_on))
  expect_identical(
    levels(schema_on$status),
    c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))
})

test_that("events_cols: event_type_key is a fixed-levels factor", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_true(is.factor(schema$event_type_key))
  expect_identical(
    levels(schema$event_type_key),
    c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))
})

# ---- Hierarchy-key direct materialization (fat-lookup deviation) ----------
#
# Events carries hierarchy keys directly under the option's own gate
# (not via inheritance). Same rationale as enrollments and departments:
# downstream consumers (calc-rates, reports) read hierarchy keys off
# events directly (e.g. `names(x$events)` intersection with
# `group_cols`). Strict inheritance would silently break these
# consumers under the full-chain case.

test_that("direct materialization: hierarchy keys under every upstream option's own gate", {
  opts_full <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_full)
  expect_true("department_key"       %in% names(schema))
  expect_true("hospital_key"         %in% names(schema))
  expect_true("country_key"          %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("direct materialization: hierarchy keys present under include_enrollment = 'no'", {
  opts_no_enr <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "no",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_no_enr)
  expect_true("department_key"       %in% names(schema))
  expect_true("hospital_key"         %in% names(schema))
  expect_true("country_key"          %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("direct materialization: hierarchy absent when its own option is 'no'", {
  opts <- dhis2_dataset_options(
    include_event      = "full",
    include_enrollment = "full",
    include_patient    = "full",
    include_hospital   = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_false("hospital_key" %in% names(schema))
})

# ---- isTest (direct materialization) ------------------------------------
#
# Events carries `isTest` under `include_test_data = TRUE` via direct
# materialization from the departments fat-lookup — same pattern as
# enrollments. The legacy reader actively fetched `isTest` into the
# pipeline; dropping it in the final `select()` was an accidental
# omission. Schematizing with the atom declared fixes that bug.

test_that("isTest on events gated on include_test_data", {
  opts_on <- dhis2_dataset_options(
    include_event     = "full",
    include_test_data = TRUE)
  expect_true("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::events_cols, opts_on)))

  opts_off <- dhis2_dataset_options(
    include_event     = "full",
    include_test_data = FALSE)
  expect_false("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::events_cols, opts_off)))
})

test_that("isTest absent under pseudo-event even with include_test_data = TRUE", {
  opts <- dhis2_dataset_options(
    include_event     = "pseudo",
    include_test_data = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_false("isTest" %in% names(schema))
})

# ---- Entity-level companion atoms (phase-b-event-details) ---------------

test_that("events_cols: user fields gated by include_user != 'no' × full", {
  opts_off <- dhis2_dataset_options(
    include_event = "full", include_user = "no")
  opts_on  <- dhis2_dataset_options(
    include_event = "full", include_user = "full")
  opts_pseudo <- dhis2_dataset_options(
    include_event = "pseudo", include_user = "full")

  user_cols <- c("storedBy", "createdBy", "updatedBy", "completedBy")
  s_off    <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_off)
  s_on     <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_on)
  s_pseudo <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_pseudo)

  for (col in user_cols) {
    expect_false(col %in% names(s_off),    info = col)
    expect_true (col %in% names(s_on),     info = col)
    expect_false(col %in% names(s_pseudo), info = col)
  }
})

test_that("events_cols: timestamps gated by include_timestamps × full", {
  opts_off <- dhis2_dataset_options(
    include_event = "full", include_timestamps = FALSE)
  opts_on  <- dhis2_dataset_options(
    include_event = "full", include_timestamps = TRUE)
  opts_pseudo <- dhis2_dataset_options(
    include_event = "pseudo", include_timestamps = TRUE)

  ts_cols <- c("scheduledAt", "completedAt", "createdAt", "createdAtClient",
               "updatedAt", "updatedAtClient")
  s_off    <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_off)
  s_on     <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_on)
  s_pseudo <- neoipcr:::compile_schema(neoipcr:::events_cols, opts_pseudo)

  for (col in ts_cols) {
    expect_false(col %in% names(s_off),    info = col)
    expect_true (col %in% names(s_on),     info = col)
    expect_false(col %in% names(s_pseudo), info = col)
  }
})

test_that("events_cols: followup always present under full, absent under pseudo", {
  s_full   <- neoipcr:::compile_schema(
    neoipcr:::events_cols, dhis2_dataset_options(include_event = "full"))
  s_pseudo <- neoipcr:::compile_schema(
    neoipcr:::events_cols, dhis2_dataset_options(include_event = "pseudo"))
  expect_true ("followup" %in% names(s_full))
  expect_false("followup" %in% names(s_pseudo))
})

test_that("events_cols: deleted gated by include_deleted × full", {
  s_off <- neoipcr:::compile_schema(
    neoipcr:::events_cols,
    dhis2_dataset_options(include_event = "full", include_deleted = FALSE))
  s_on <- neoipcr:::compile_schema(
    neoipcr:::events_cols,
    dhis2_dataset_options(include_event = "full", include_deleted = TRUE))
  s_pseudo <- neoipcr:::compile_schema(
    neoipcr:::events_cols,
    dhis2_dataset_options(include_event = "pseudo", include_deleted = TRUE))
  expect_false("deleted" %in% names(s_off))
  expect_true ("deleted" %in% names(s_on))
  expect_false("deleted" %in% names(s_pseudo))
})

# ---- Fixture round-trip ---------------------------------------------------

test_that("make_test_events output matches events_cols schema (full mode)", {
  opts <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = "events",
    include_incomplete       = "events",
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  fixture <- make_test_events(
    n = 3,
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = "events",
    include_incomplete       = "events",
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  expect_schema_matches(fixture, schema)
})

test_that("make_test_events output matches events_cols schema (pseudo mode)", {
  opts <- dhis2_dataset_options(include_event = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  fixture <- make_test_events(n = 2, include_event = "pseudo")
  expect_schema_matches(fixture, schema)
  expect_identical(names(fixture), "event_key")
})
