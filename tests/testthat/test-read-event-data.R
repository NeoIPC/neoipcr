# Integration tests for the reader pipeline — sparse-data resilience.
#
# Tests here exercise the readers end-to-end with raw DHIS2-shaped
# fixtures that have missing/sparse data elements. They complement
# the schema-level tests (test-schema-event-data.R) which verify the
# schema declarations themselves.
#
# Coverage targets (per the task file):
#   Gaps 1-5:  Pivot-volatility per event type — fully-missing DEs,
#              partial coverage, rename path, companion columns.
#   Gap 6:     DHIS2 API omits null-valued fields.
#   Gap 7:     All-NA columns arrive as logical (type drift).
#   Gap 8:     Substance-days pivot-volatility.
#   Gap 9:     Hierarchy-key absence on fact tibbles.
#   Gap 10:    trackedEntity absence on events.

# ---- Default opts for tests --------------------------------------------------

.test_opts <- function(include_user = "no", include_timestamps = FALSE) {
  neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_user             = include_user,
    include_timestamps       = include_timestamps,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
}

# ---- Gaps 1-5: Per-event-type pivot-volatility --------------------------------

# Helper: run read_event_data on sparse raw events and verify the output
# matches the schema exactly. Sends one event with only `present_fields`
# populated — all other DEs are absent from the dataValues. The pivot
# must produce the full schema with missing DEs as NA.
.test_event_type_sparse <- function(event_type_key, present_fields,
                                     present_values, label) {
  opts     <- .test_opts()
  cols     <- neoipcr:::event_data_cols_for(event_type_key)
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata(event_type_key)

  events_raw <- build_raw_events(
    event_keys     = 1L,
    event_type_key = event_type_key,
    rows           = list(present_values))
  processed <- build_processed_events(1L, event_type_key)

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, event_type_key)

  # Column names and order match the schema.
  expect_identical(names(result), names(expected), label = label)

  # All declared columns have the right type.
  for (col in names(expected)) {
    expect_identical(
      class(result[[col]]), class(expected[[col]]),
      label = paste(label, "-- column", col, "class"))
  }

  # Fixed-factor levels match.
  for (col in names(expected)) {
    if (is.factor(expected[[col]])) {
      expect_identical(
        levels(result[[col]]), levels(expected[[col]]),
        label = paste(label, "-- column", col, "levels"))
    }
  }

  # Fully-missing DE columns should be all NA.
  all_names <- names(expected)
  non_de    <- c("event_key", "enrollment_key", "patient_key",
                 "department_key", "hospital_key", "country_key",
                 "world_bank_class_key", "isTest", "vs_days")
  companion <- grepl(
    "_(storedBy|createdBy|updatedBy|createdAt|updatedAt)$", all_names)
  de_codes  <- setdiff(all_names[!companion], non_de)
  missing_fields <- setdiff(de_codes, present_fields)
  for (field in missing_fields) {
    expect_true(
      is.na(result[[field]][[1L]]),
      label = paste(label, "-- missing DE", field, "is NA"))
  }

  # Present fields should carry their values.
  for (field in present_fields) {
    expect_false(
      is.na(result[[field]][[1L]]),
      label = paste(label, "-- present DE", field, "is not NA"))
  }

  expect_equal(nrow(result), 1L, label = paste(label, "-- row count"))

  result
}


test_that("admission: sparse data produces full schema", {
  .test_event_type_sparse(
    "adm",
    present_fields = "type",
    present_values = list(type = "1"),
    label = "admission sparse")
})

test_that("surveillance-end: sparse data produces full schema", {
  result <- .test_event_type_sparse(
    "end",
    present_fields = c("reason", "patient_days"),
    present_values = list(reason = "1", patient_days = "10"),
    label = "surveillance-end sparse")
  # vs_days must exist and equal inv_days + niv_days (both NA -> vs_days NA).
  expect_true("vs_days" %in% names(result))
  expect_true(is.na(result$vs_days[[1L]]))
})

test_that("sepsis/BSI: sparse data produces full schema", {
  .test_event_type_sparse(
    "bsi",
    present_fields = c("dev_ass", "los"),
    present_values = list(dev_ass = "1", los = "5"),
    label = "BSI sparse")
})

test_that("NEC: sparse data produces full schema + rename path", {
  .test_event_type_sparse(
    "nec",
    present_fields = c("sec_bsi", "los"),
    present_values = list(sec_bsi = "0", los = "5"),
    label = "NEC sparse + rename")
})

test_that("pneumonia/HAP: sparse data + both renames", {
  .test_event_type_sparse(
    "hap",
    present_fields = c("dev_ass", "sec_bsi", "los"),
    present_values = list(dev_ass = "1", sec_bsi = "0", los = "5"),
    label = "HAP sparse + renames")
})

test_that("surgery: sparse data produces full schema", {
  .test_event_type_sparse(
    "pro",
    present_fields = c("los", "main_procedure_code"),
    present_values = list(los = "3", main_procedure_code = "PZX.AA.JA"),
    label = "surgery sparse")
})

test_that("SSI: sparse data produces full schema", {
  .test_event_type_sparse(
    "ssi",
    present_fields = c("infection_type", "los"),
    present_values = list(infection_type = "1", los = "10"),
    label = "SSI sparse")
})

test_that("zero-row events produce the schema shape", {
  # When no events match this event type, the reader takes the
  # nrow == 0 early-return branch and emits compile_schema.
  opts     <- .test_opts()
  cols     <- neoipcr:::event_data_cols_for("end")
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata("end")

  # An admission event — filtered out for "end" type.
  events_raw <- build_raw_events(
    event_keys     = 1L,
    event_type_key = "end",
    rows           = list(list(reason = "1")))
  # processed_events has the event as "adm", not "end" — so the join
  # in read_event_data filters it out.
  processed <- build_processed_events(1L, "adm")

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, "end")

  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 0L)
  for (col in names(expected)) {
    expect_identical(class(result[[col]]), class(expected[[col]]))
  }
})


# ---- Gap 6: DHIS2 API omits null-valued fields --------------------------------
#
# When a declared column is absent from the raw response because every
# value was null, finalize_to_schema materializes it as all-NA of the
# right type.

test_that("gap 6: absent declared columns materialized as NA", {
  opts     <- .test_opts()
  cols     <- neoipcr:::event_data_cols_for("end")
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata("end")

  events_raw <- build_raw_events(
    event_keys     = 1L,
    event_type_key = "end",
    rows           = list(list(reason = "1", patient_days = "10")))
  processed <- build_processed_events(1L, "end")

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, "end")

  expect_identical(names(result), names(expected))

  # Absent DEs are NA with correct type.
  expect_true(is.na(result$cvc_days[[1L]]))
  expect_true(is.integer(result$cvc_days))
  expect_true(is.na(result$inv_days[[1L]]))
  expect_true(is.integer(result$inv_days))
  expect_true(is.na(result$ab_days[[1L]]))
  expect_true(is.integer(result$ab_days))

  # Present DEs carry their values.
  expect_equal(result$patient_days[[1L]], 10L)
  expect_equal(as.character(result$reason[[1L]]), "1")
})


# ---- Gap 7: All-NA columns type drift ----------------------------------------
#
# When a pivot produces an all-NA column, R defaults to logical.
# finalize_to_schema must coerce to the declared type.

test_that("gap 7: all-NA columns coerced to declared type", {
  opts     <- .test_opts()
  cols     <- neoipcr:::event_data_cols_for("adm")
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata("adm")

  # Three events, all have `type` but none have `dol`.
  events_raw <- build_raw_events(
    event_keys     = 1:3,
    event_type_key = "adm",
    rows           = list(
      list(type = "1"),
      list(type = "2"),
      list(type = "3")))
  processed <- build_processed_events(1:3, "adm")

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, "adm")

  expect_identical(names(result), names(expected))
  # `dol` must be integer (not logical), all NA.
  expect_true(is.integer(result$dol))
  expect_true(all(is.na(result$dol)))
  # `type` has values.
  expect_equal(nrow(result), 3L)
  expect_false(any(is.na(result$type)))
})


# ---- Gap 8: Substance-days pivot-volatility -----------------------------------

test_that("gap 8: substance-days with no _DAYS codes still has `days` column", {
  opts     <- .test_opts()
  expected <- neoipcr:::compile_schema(neoipcr:::substanceDays_cols, opts)
  metadata <- build_reader_metadata("end")

  events_raw <- build_raw_substance_events(
    event_keys     = 1L,
    substance_rows = list(list(
      "1" = list(substance_code = "J01CA04"),
      "2" = list(substance_code = "J01CR02"))))
  processed <- build_processed_events(1L, "end")

  result <- neoipcr:::read_substance_days(
    events_raw, processed, metadata, opts)

  expect_identical(names(result), names(expected))
  expect_true("days" %in% names(result))
  expect_true(is.integer(result$days))
  expect_equal(nrow(result), 2L)
  expect_true(all(is.na(result$days)))
})

test_that("gap 8: substance-days with both codes and days", {
  opts     <- .test_opts()
  expected <- neoipcr:::compile_schema(neoipcr:::substanceDays_cols, opts)
  metadata <- build_reader_metadata("end")

  events_raw <- build_raw_substance_events(
    event_keys     = 1L,
    substance_rows = list(list(
      "1" = list(substance_code = "J01CA04", days = "3"))))
  processed <- build_processed_events(1L, "end")

  result <- neoipcr:::read_substance_days(
    events_raw, processed, metadata, opts)

  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 1L)
  expect_equal(result$substance_code[[1L]], "J01CA04")
  expect_equal(result$days[[1L]], 3L)
  expect_equal(result$index[[1L]], 1L)
})

test_that("substance-days: a two-digit slot index parses to the full number, not the last digit", {
  # Regression for the index regex AB_SUBST_\d(\d) -> AB_SUBST_(\d\d): slot 15 must read as 15, not 5.
  opts     <- .test_opts()
  metadata <- build_reader_metadata("end", substance_count = 15L)

  events_raw <- build_raw_substance_events(
    event_keys     = 1L,
    substance_rows = list(list(
      "9"  = list(substance_code = "J01CA04", days = "3"),
      "15" = list(substance_code = "J01CR02", days = "5"))))
  processed <- build_processed_events(1L, "end")

  result <- neoipcr:::read_substance_days(events_raw, processed, metadata, opts)

  expect_setequal(result$index, c(9L, 15L))
  expect_equal(result$substance_code[result$index == 15L], "J01CR02")
  expect_equal(result$days[result$index == 15L], 5L)
})

test_that("gap 8: substance-days zero matching events produce schema shape", {
  opts     <- .test_opts()
  expected <- neoipcr:::compile_schema(neoipcr:::substanceDays_cols, opts)
  metadata <- build_reader_metadata("end")

  # events_raw has a different event ID than processed_events,
  # so the inner_join produces 0 rows.
  events_raw <- build_raw_substance_events(
    event_keys     = 99L,
    substance_rows = list(list("1" = list(substance_code = "J01CA04"))))
  processed <- build_processed_events(1L, "end")

  result <- neoipcr:::read_substance_days(
    events_raw, processed, metadata, opts)

  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 0L)
  for (col in names(expected)) {
    expect_identical(
      class(result[[col]]), class(expected[[col]]),
      label = paste("substance-days empty -- column", col))
  }
})


# ---- Gap 9: Hierarchy-key absence on fact tibbles -----------------------------
#
# Under include_department = "full", patients do NOT carry
# hospital_key / country_key / world_bank_class_key directly (they
# reach them via departments via the inheritance rule).
# Enrollments and events use direct materialization, so they ALWAYS
# carry hierarchy keys under full mode.

test_that("gap 9: patients do not carry hospital_key under full departments", {
  opts <- neoipcr::dhis2_dataset_options(
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_true("department_key" %in% names(schema))
  expect_false("hospital_key" %in% names(schema))
  expect_false("country_key" %in% names(schema))
  expect_false("world_bank_class_key" %in% names(schema))
})

test_that("gap 9: patients under pseudo departments inherit selectively", {
  # Under pseudo departments, departments carries department_key +
  # hospital_key (shared col_hospital_key). Pre-joined country_key
  # and world_bank_class_key require include_department == "full", so
  # they're absent on departments under pseudo. Patients inherits
  # only what departments doesn't already carry.
  opts <- neoipcr::dhis2_dataset_options(
    include_patient          = "full",
    include_department       = "pseudo",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_true("department_key" %in% names(schema))
  # hospital_key is on departments (pseudo still carries it via shared
  # col_hospital_key), so patients does NOT inherit it.
  expect_false("hospital_key" %in% names(schema))
  # country_key and world_bank_class_key are NOT on departments under
  # pseudo (pre-join requires full), so patients DOES inherit them.
  expect_true("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("gap 9: enrollments carry hierarchy keys (direct materialization)", {
  opts <- neoipcr::dhis2_dataset_options(
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  # Enrollments uses enrollment_hierarchy_col (direct materialization),
  # so it always carries hierarchy keys under full enrollment.
  expect_true("department_key" %in% names(schema))
  expect_true("hospital_key" %in% names(schema))
  expect_true("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("gap 9: events always carry hierarchy keys (direct materialization)", {
  opts <- neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_true("department_key" %in% names(schema))
  expect_true("hospital_key" %in% names(schema))
  expect_true("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("gap 9: per-event-type data inherits from events", {
  # Under full events: per-event-type data does NOT carry hierarchy
  # keys directly (events has them).
  opts_full <- neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema_full <- neoipcr:::compile_schema(
    neoipcr:::admissionData_cols, opts_full)
  expect_true("event_key" %in% names(schema_full))
  expect_false("department_key" %in% names(schema_full))
  expect_false("hospital_key" %in% names(schema_full))

  # Under pseudo events: per-event-type data materializes hierarchy
  # keys directly (events only has event_key).
  opts_pseudo <- neoipcr::dhis2_dataset_options(
    include_event            = "pseudo",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema_pseudo <- neoipcr:::compile_schema(
    neoipcr:::admissionData_cols, opts_pseudo)
  expect_true("event_key" %in% names(schema_pseudo))
  expect_true("department_key" %in% names(schema_pseudo))
  expect_true("hospital_key" %in% names(schema_pseudo))
  expect_true("country_key" %in% names(schema_pseudo))
  expect_true("world_bank_class_key" %in% names(schema_pseudo))
})


# ---- Gap 10: trackedEntity absence on events ----------------------------------
#
# read_events derives patient_key from the enrollment chain
# (enrollment_key -> enrollments -> patient_key), NOT from trackedEntity.

test_that("gap 10: read_events works without trackedEntity on raw events", {
  opts <- neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = c("enrollments", "patients", "events"),
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)

  # Raw events WITHOUT trackedEntity.
  raw_events <- tibble::tibble(
    event        = c("EVT_1", "EVT_2"),
    programStage = c("PS_ADM", "PS_ADM"),
    enrollment   = c("ENR_1", "ENR_1"),
    occurredAt   = c("2024-01-15T00:00:00.000", "2024-01-16T00:00:00.000"),
    orgUnit      = c("OU_DEPT_1", "OU_DEPT_1"),
    followup     = c(FALSE, FALSE),
    status       = c("COMPLETED", "COMPLETED"),
    createdBy    = list(list(username = "admin"), list(username = "admin")),
    updatedBy    = list(list(username = "admin"), list(username = "admin")),
    dataValues   = list(list(), list()),
    notes        = list(list(), list())
  )

  # Enrollments internal map (the enrollment chain with patient_key).
  enrollments_internal_map <- tibble::tibble(
    enrollment_key = 1L,
    enrollment     = "ENR_1",
    patient_key    = 1L
  )

  enrollments <- tibble::tibble(enrollment_key = 1L)

  departments <- tibble::tibble(
    department_key       = 1L,
    orgUnit              = "OU_DEPT_1",
    hospital_key         = 1L,
    country_key          = 1L,
    world_bank_class_key = 1L,
    isTest               = FALSE
  )
  event_types_map <- tibble::tibble(
    programStage   = "PS_ADM",
    event_type_key = factor("adm",
      levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))
  )
  users_map <- tibble::tibble(
    user_key = 1L,
    user     = "UID_admin",
    username = "admin"
  )

  departments_internal_map <- tibble::tibble(
    department_key       = 1L,
    orgUnit              = "OU_DEPT_1",
    hospital_key         = 1L,
    country_key          = 1L,
    world_bank_class_key = 1L,
    isTest               = FALSE
  )

  metadata <- list(
    departments                = departments,
    .departments_internal_map  = departments_internal_map,
    .enrollments_internal_map  = enrollments_internal_map,
    .eventTypes_internal_map   = event_types_map,
    .users_internal_map        = users_map
  )

  events_result <- neoipcr:::read_events(
    raw_events, enrollments, metadata, opts)

  result <- events_result$public
  expected <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 2L)
  # patient_key derived from enrollment chain.
  expect_true(all(result$patient_key == 1L))
  expect_true(all(result$enrollment_key == 1L))

  # Internal map carries event_key + event for downstream readers.
  imap <- events_result$internal_map
  expect_true(all(c("event_key", "event") %in% names(imap)))
  expect_equal(nrow(imap), 2L)
})


# ---- Additional: companion columns under include_user/include_timestamps ------

test_that("companion columns appear under include_user = 'full'", {
  opts <- neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_user             = "full",
    include_timestamps       = FALSE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")

  cols     <- neoipcr:::event_data_cols_for("adm")
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata("adm")

  events_raw <- tibble::tibble(
    event = "EVT_1",
    dataValues = list(list(
      list(
        dataElement = .de_uid("NEOIPC_ADMISSION_TYPE"),
        value       = "1",
        storedBy    = "admin",
        createdBy   = list(username = "admin"),
        updatedBy   = list(username = "admin")),
      list(
        dataElement = .de_uid("NEOIPC_ADMISSION_DOL"),
        value       = "5",
        storedBy    = "admin",
        createdBy   = list(username = "admin"),
        updatedBy   = list(username = "admin"))
    ))
  )
  processed <- build_processed_events(1L, "adm")

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, "adm")

  expect_identical(names(result), names(expected))
  expect_true("type_storedBy" %in% names(result))
  expect_true("type_createdBy" %in% names(result))
  expect_true("type_updatedBy" %in% names(result))
  expect_true("dol_storedBy" %in% names(result))
  expect_true(is.integer(result$type_storedBy))
  expect_equal(result$type_storedBy[[1L]], 1L)
})

test_that("companion columns appear under include_timestamps = TRUE", {
  opts <- neoipcr::dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_user             = "no",
    include_timestamps       = TRUE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")

  cols     <- neoipcr:::event_data_cols_for("adm")
  expected <- neoipcr:::compile_schema(cols, opts)
  metadata <- build_reader_metadata("adm")

  events_raw <- tibble::tibble(
    event = "EVT_1",
    dataValues = list(list(
      list(
        dataElement = .de_uid("NEOIPC_ADMISSION_TYPE"),
        value       = "1",
        createdAt   = "2024-01-15T10:30:00.000",
        updatedAt   = "2024-01-15T11:00:00.000"),
      list(
        dataElement = .de_uid("NEOIPC_ADMISSION_DOL"),
        value       = "5",
        createdAt   = "2024-01-15T10:30:00.000",
        updatedAt   = "2024-01-15T11:00:00.000")
    ))
  )
  processed <- build_processed_events(1L, "adm")

  result <- neoipcr:::read_event_data(
    events_raw, processed, metadata, opts, "adm")

  expect_identical(names(result), names(expected))
  expect_true("type_createdAt" %in% names(result))
  expect_true("type_updatedAt" %in% names(result))
  expect_true("dol_createdAt" %in% names(result))
  expect_s3_class(result$type_createdAt, "POSIXct")
})


# ---- Additional: infectious agent findings sparse data ------------------------

test_that("findings: sparse pathogens still produce full schema", {
  opts     <- .test_opts()
  expected <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  metadata <- build_reader_metadata("bsi")

  # One BSI event with only a single pathogen key — no resistance
  # markers, no source, no multiple, no name.
  events_raw <- build_raw_pathogen_events(
    event_keys      = 1L,
    event_type_keys = "bsi",
    pathogen_rows   = list(list("1" = list(pathogen = "42"))))
  processed <- build_processed_events(1L, "bsi")

  pair   <- neoipcr:::read_infectious_agent_findings(
    events_raw, processed, metadata, opts)
  result <- pair$infectiousAgentFindings

  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 1L)
  # Resistance markers should be NA (not absent).
  expect_true(is.na(result[["3gcr"]][[1L]]))
  expect_true(is.factor(result[["3gcr"]]))
  # Source should be NA.
  expect_true(is.na(result$source[[1L]]))
  expect_true(is.factor(result$source))
  # Multiple should be NA.
  expect_true(is.na(result$multiple[[1L]]))
  # Sibling unknownPathogenNames is empty here (no free-text name).
  expect_equal(nrow(pair$unknownPathogenNames), 0L)
})

test_that("findings: no matching events produce empty schema", {
  opts     <- .test_opts()
  expected <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  upn_expected <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  metadata <- build_reader_metadata("bsi")

  # Event IDs don't match processed_events, so inner_join produces
  # 0 rows and the reader takes the early-return path.
  events_raw <- build_raw_pathogen_events(
    event_keys      = 99L,
    event_type_keys = "bsi",
    pathogen_rows   = list(list("1" = list(pathogen = "42"))))
  processed <- build_processed_events(1L, "bsi")

  pair   <- neoipcr:::read_infectious_agent_findings(
    events_raw, processed, metadata, opts)
  result <- pair$infectiousAgentFindings

  expect_identical(names(result), names(expected))
  expect_equal(nrow(result), 0L)
  expect_identical(
    names(pair$unknownPathogenNames), names(upn_expected))
  expect_equal(nrow(pair$unknownPathogenNames), 0L)
})

test_that("findings: free-text pathogen names land in unknownPathogenNames", {
  # Regression test for `tasks/fix-unknown-pathogen-names-split.md`:
  # the pre-fix split ran on the already-finalized findings (where
  # `name` had been stripped by `finalize_to_schema`'s scratch
  # handling), producing an always-empty unknownPathogenNames and an
  # `'NA'` in Validation Rule 20. The split now runs on the
  # pre-finalize intermediate while `name` is still attached.
  opts     <- .test_opts()
  metadata <- build_reader_metadata("bsi")

  events_raw <- build_raw_pathogen_events(
    event_keys      = c(1L, 2L),
    event_type_keys = c("bsi", "bsi"),
    pathogen_rows   = list(
      list("1" = list(pathogen = "0", name = "Bizarre bacterium A")),
      list("1" = list(pathogen = "42"),
           "2" = list(pathogen = "0", name = "Bizarre bacterium B"))))
  processed <- build_processed_events(c(1L, 2L), "bsi")

  pair <- neoipcr:::read_infectious_agent_findings(
    events_raw, processed, metadata, opts)

  # Findings carries all three rows; `name` is stripped (not declared).
  expect_equal(nrow(pair$infectiousAgentFindings), 3L)
  expect_false("name" %in% names(pair$infectiousAgentFindings))

  # unknownPathogenNames carries the two free-text rows, linked by
  # agent_finding_key.
  upn <- pair$unknownPathogenNames
  expect_identical(names(upn), c("agent_finding_key", "name"))
  expect_equal(nrow(upn), 2L)
  expect_setequal(
    upn$name, c("Bizarre bacterium A", "Bizarre bacterium B"))
  # Each name's key must resolve to a `pathogen_key == 0` row on findings.
  iaf <- pair$infectiousAgentFindings
  for (k in upn$agent_finding_key)
    expect_equal(
      iaf$pathogen_key[iaf$agent_finding_key == k], 0L)
})
