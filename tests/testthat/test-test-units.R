# Tests for test organisation unit tolerance.
#
# Test org units live outside the real hierarchy:
#   Real: Root -> Country -> Hospital -> Department
#   Test: Root -> TEST_UNITS -> Department (no hospital level)
#
# Test units have NA country_key, hospital_key, and world_bank_class_key.
# Code must tolerate these NAs via left_join (not inner_join).
# See repos/neoipcr/CLAUDE.md "DHIS2 Test Units" section.

# Build a neoipcr_ds where all data has NA hierarchy keys (simulating test
# units). The department_key itself is valid — only the upstream hierarchy
# keys are missing.

make_test_unit_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments     <- make_test_metadata_departments(1)
  md$hospitals        <- make_test_metadata_hospitals(1)
  md$countries        <- make_test_metadata_countries(1)
  md$worldBankClasses <- make_test_metadata_wb_classes(1)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full")

  patients <- make_test_patients(2,
    department_key       = c(1L, 1L),
    hospital_key         = c(NA_integer_, NA_integer_),
    country_key          = c(NA_integer_, NA_integer_),
    world_bank_class_key = c(NA_integer_, NA_integer_),
    birth_weight         = c(1000L, 1500L),
    total_gestation_days = c(196L, 224L))

  enrollments <- make_test_enrollments(2,
    patient_keys         = 1:2,
    department_key       = c(1L, 1L),
    hospital_key         = c(NA_integer_, NA_integer_),
    country_key          = c(NA_integer_, NA_integer_),
    world_bank_class_key = c(NA_integer_, NA_integer_),
    enrolledAt           = as.Date(c("2024-01-01", "2024-01-05")))

  events <- make_test_events(
    n               = 6,
    enrollment_keys = c(1, 1, 1, 2, 2, 2),
    patient_keys    = c(1, 1, 1, 2, 2, 2),
    event_type_keys = c("adm", "end", "bsi", "adm", "end", "bsi"),
    occurredAt      = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-01-08",
      "2024-01-05", "2024-01-20", "2024-01-12")),
    department_key       = rep(1L, 6),
    hospital_key         = rep(NA_integer_, 6),
    country_key          = rep(NA_integer_, 6),
    world_bank_class_key = rep(NA_integer_, 6))

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    admissionData       = make_test_admission_data(c(1L, 4L)),
    surveillanceEndData = make_test_surveillance_end_data(c(2L, 5L),
      patient_days = c(15L, 16L)),
    sepsisData          = make_test_sepsis_data(c(3L, 6L)),
    infectiousAgentFindings = make_test_iaf(c(3L, 6L)),
    substanceDays       = make_test_substance_days(c(2L, 5L)))
}

# --- Calc pipeline tolerates NA hierarchy keys ---

test_that("calculate_department_data succeeds with NA hierarchy keys", {
  ds <- make_test_unit_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)
  expect_s3_class(result, "neoipcr_rep_ds")
})

test_that("calculate_department_data preserves all patients with NA keys", {
  ds <- make_test_unit_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)
  expect_equal(result$n_patients$total, 2L)
  expect_equal(result$n_enrollments$total, 2L)
})

test_that("calculate_department_data produces valid tables with NA keys", {
  ds <- make_test_unit_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)
  # Usage density rate table should have data (non-empty patient days)
  udr <- result$usage_density_rate_table
  expect_s3_class(udr, "tbl_df")
  expect_true(nrow(udr) > 0L)
  # Incidence density rate table should have BSI counts
  idr <- result$incidence_density_rate_table
  expect_s3_class(idr, "tbl_df")
  bsi_row <- idr[idr$inf == "bsi", ]
  expect_equal(bsi_row$n, 2L)
})

# --- apply_postfilter preserves test units ---

test_that("apply_postfilter preserves enrollments with NA country_key", {
  ds <- make_test_unit_ds()
  result <- neoipcr:::apply_postfilter(ds)
  # All enrollments have NA country_key — they must survive
  expect_equal(nrow(result$enrollments), 2L)
  expect_true(all(is.na(result$enrollments$country_key)))
})

test_that("apply_postfilter preserves patients with NA hierarchy keys", {
  ds <- make_test_unit_ds()
  result <- neoipcr:::apply_postfilter(ds)
  expect_equal(nrow(result$patients), 2L)
})

# --- Individual table builders tolerate NA hierarchy keys ---

test_that("get_usage_density_rate_table works with NA hierarchy keys", {
  ds <- make_test_unit_ds()
  result <- get_usage_density_rate_table(ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 12L)
})

test_that("get_incidence_density_rate_table works with NA hierarchy keys", {
  ds <- make_test_unit_ds()
  result <- get_incidence_density_rate_table(ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_s3_class(result, "tbl_df")
  bsi_row <- result[result$inf == "bsi", ]
  expect_equal(bsi_row$n, 2L)
})

test_that("get_surgery_rate_table works with NA hierarchy keys (no surgeries)", {
  ds <- make_test_unit_ds()
  result <- get_surgery_rate_table(ds, use_cache = FALSE)
  expect_s3_class(result, "tbl_df")
  # No surgery events → single "overall" row with n=0
  expect_equal(result$n[result$pro_cat == "overall"], 0L)
})
