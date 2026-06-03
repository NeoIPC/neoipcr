# Tests for R/schema-enrollments.R — enrollments_cols.

# ---- enrollments_cols three-mode shape ------------------------------------

test_that("enrollments_cols: 'no' mode returns 0x0 via the entity gate", {
  opts <- dhis2_dataset_options(include_enrollment = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  expect_s3_class(schema, "tbl_df")
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("enrollments_cols: 'pseudo' mode is strictly enrollment_key only", {
  opts <- dhis2_dataset_options(
    include_enrollment       = "pseudo",
    # Every orthogonal gate is open, yet pseudo stays 1-col because
    # every non-PK atom compounds with `include_enrollment == "full"`.
    include_patient          = "full",
    include_dhis2_ids        = c("enrollments", "patients"),
    include_incomplete       = "enrollments",
    include_user             = "full",
    include_timestamps       = TRUE,
    include_test_data        = TRUE,
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  expect_identical(names(schema), "enrollment_key")
})

test_that("enrollments_cols: 'full' minimal — enrollment_key + patient_key + enrolledAt + followUp", {
  opts <- dhis2_dataset_options(
    include_enrollment = "full",
    include_patient    = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  expect_identical(
    names(schema),
    c("enrollment_key", "patient_key", "enrolledAt", "followUp"))
})

test_that("enrollments_cols: enrollment id gated on include_dhis2_ids", {
  opts_no <- dhis2_dataset_options(include_enrollment = "full",
                                   include_dhis2_ids  = character())
  opts_yes <- dhis2_dataset_options(include_enrollment = "full",
                                    include_dhis2_ids  = "enrollments")
  expect_false("enrollment" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_no)))
  expect_true("enrollment" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_yes)))
})

test_that("enrollments_cols: patient_key link FK gated by both sides", {
  # enrollments = "full" but include_patient = "no" → no link FK.
  opts_no_pat <- dhis2_dataset_options(include_enrollment = "full",
                                       include_patient    = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_no_pat)
  expect_false("patient_key" %in% names(schema))

  # Both sides open → link present.
  opts_both <- dhis2_dataset_options(include_enrollment = "full",
                                     include_patient    = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_both)
  expect_true("patient_key" %in% names(schema))
})

test_that("enrollments_cols: status gated on include_incomplete", {
  opts_off <- dhis2_dataset_options(include_enrollment = "full",
                                    include_incomplete = character())
  opts_on  <- dhis2_dataset_options(include_enrollment = "full",
                                    include_incomplete = "enrollments")
  schema_off <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_off)
  schema_on  <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_on)
  expect_false("status" %in% names(schema_off))
  expect_true("status" %in% names(schema_on))
  expect_identical(
    levels(schema_on$status),
    c("ACTIVE", "COMPLETED", "CANCELLED"))
})

test_that("enrollments_cols: entity-level user fields gated on include_user", {
  opts_no   <- dhis2_dataset_options(include_enrollment = "full",
                                     include_user       = "no")
  opts_full <- dhis2_dataset_options(include_enrollment = "full",
                                     include_user       = "full")
  user_cols <- c("createdBy", "updatedBy", "completedBy", "storedBy")
  for (col in user_cols) {
    expect_false(col %in% names(
      neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_no)))
    expect_true(col %in% names(
      neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_full)))
  }
})

test_that("enrollments_cols: entity-level timestamps gated on include_timestamps", {
  opts_off <- dhis2_dataset_options(include_enrollment = "full",
                                    include_timestamps = FALSE)
  opts_on  <- dhis2_dataset_options(include_enrollment = "full",
                                    include_timestamps = TRUE)
  ts_cols <- c("occurredAt", "createdAt", "createdAtClient",
               "updatedAt", "updatedAtClient", "completedAt")
  for (col in ts_cols) {
    expect_false(col %in% names(
      neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_off)))
    expect_true(col %in% names(
      neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_on)))
  }
})

test_that("enrollments_cols: deleted gated on include_deleted", {
  opts_off <- dhis2_dataset_options(include_enrollment = "full",
                                    include_deleted    = FALSE)
  opts_on  <- dhis2_dataset_options(include_enrollment = "full",
                                    include_deleted    = TRUE)
  expect_false("deleted" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_off)))
  expect_true("deleted" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_on)))
})

# ---- Hierarchy-key direct materialization (fat-lookup deviation) ----------
#
# Enrollments carries hierarchy keys directly under the option's own
# gate (not via the inheritance helper). This deviates from the strict
# inheritance rule for the same reason departments does under
# `include_department = "full"`: downstream consumers (calc-api's
# `get_countries_with_wb_class()` joins enrollments with
# metadata$countries on country_key) read the hierarchy keys directly
# off the fact table. Strict inheritance would silently break these
# consumers under the full-chain case.

test_that("direct materialization: hierarchy keys under every upstream option's own gate", {
  opts_full <- dhis2_dataset_options(
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_full)
  expect_true("department_key"       %in% names(schema))
  expect_true("hospital_key"         %in% names(schema))
  expect_true("country_key"          %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("direct materialization: hierarchy keys present under include_patient = 'no'", {
  opts_no_pat <- dhis2_dataset_options(
    include_enrollment       = "full",
    include_patient          = "no",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_no_pat)
  expect_true("department_key"       %in% names(schema))
  expect_true("hospital_key"         %in% names(schema))
  expect_true("country_key"          %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("direct materialization: hierarchy absent when its own option is 'no'", {
  opts <- dhis2_dataset_options(
    include_enrollment = "full",
    include_patient    = "full",
    include_hospital   = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  expect_false("hospital_key" %in% names(schema))
})

test_that("isTest on enrollments gated on include_test_data", {
  opts_on <- dhis2_dataset_options(
    include_enrollment = "full",
    include_patient    = "full",
    include_test_data  = TRUE)
  expect_true("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_on)))

  opts_off <- dhis2_dataset_options(
    include_enrollment = "full",
    include_patient    = "full",
    include_test_data  = FALSE)
  expect_false("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts_off)))
})

# ---- Fixture round-trip ---------------------------------------------------

test_that("make_test_enrollments output matches enrollments_cols schema (full mode)", {
  opts <- dhis2_dataset_options(
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = "enrollments",
    include_incomplete       = "enrollments",
    include_user             = "full",
    include_timestamps       = TRUE,
    include_test_data        = TRUE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  fixture <- make_test_enrollments(
    n = 2,
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = "enrollments",
    include_incomplete       = "enrollments",
    include_user             = "full",
    include_timestamps       = TRUE,
    include_test_data        = TRUE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  expect_schema_matches(fixture, schema)
})

test_that("make_test_enrollments output matches enrollments_cols schema (pseudo mode)", {
  opts <- dhis2_dataset_options(include_enrollment = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  fixture <- make_test_enrollments(n = 2, include_enrollment = "pseudo")
  expect_schema_matches(fixture, schema)
  expect_identical(names(fixture), "enrollment_key")
})
