# Tests for R/data-removal.R — apply_data_removal()
# The authoritative data-protection guardian.

# Build a fully-populated dataset once for reuse across tests.
base_ds <- make_populated_test_ds()

# Helper: run apply_data_removal with specific options.
# Defaults keep everything; overrides via ... replace specific flags.
remove_with <- function(ds = base_ds, ...) {
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
  neoipcr:::apply_data_removal(ds, opts)
}

# --- patient_columns — controls patient_id exposure ---

test_that("apply_data_removal keeps patient_id when 'id' is in patient_columns", {
  result <- remove_with(patient_columns = "id")
  expect_true("patient_id" %in% names(result$patients))
})

test_that("apply_data_removal removes patient_id when 'id' not in patient_columns", {
  result <- remove_with(patient_columns = character())
  expect_false("patient_id" %in% names(result$patients))
})

# --- include_dhis2_ids ---

test_that("apply_data_removal removes trackedEntity when patients not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("enrollments", "departments",
    "events", "notes", "event_types", "users"))
  expect_false("trackedEntity" %in% names(result$patients))
})

test_that("apply_data_removal removes enrollment ID when enrollments not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "departments",
    "events", "notes", "event_types", "users"))
  expect_false("enrollment" %in% names(result$enrollments))
})

test_that("apply_data_removal removes orgUnit from departments when departments not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "events", "notes", "event_types", "users"))
  expect_false("orgUnit" %in% names(result$metadata$departments))
})

test_that("apply_data_removal removes event ID when events not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "notes", "event_types", "users"))
  expect_false("event" %in% names(result$events))
  expect_false("event" %in% names(result$eventDetails))
})

test_that("apply_data_removal removes note ID when notes not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "event_types", "users"))
  expect_false("note" %in% names(result$eventNotes))
})

test_that("apply_data_removal removes programStage when event_types not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "notes", "users"))
  expect_false("programStage" %in% names(result$metadata$eventTypes))
})

test_that("apply_data_removal removes user ID when users not in include_dhis2_ids", {
  result <- remove_with(include_dhis2_ids = c("patients", "enrollments",
    "departments", "events", "notes", "event_types"))
  expect_false("user" %in% names(result$metadata$users))
})

test_that("apply_data_removal keeps all IDs when all types in include_dhis2_ids", {
  result <- remove_with()
  expect_true("trackedEntity" %in% names(result$patients))
  expect_true("enrollment" %in% names(result$enrollments))
  expect_true("event" %in% names(result$events))
  expect_true("programStage" %in% names(result$metadata$eventTypes))
})

# --- include_department ---

test_that("include_department = fullkeeps departments and department_key", {
  result <- remove_with(include_department = "full")
  expect_false(is.null(result$metadata$departments))
  expect_true("department_key" %in% names(result$patients))
  expect_true("department_key" %in% names(result$enrollments))
  expect_true("department_key" %in% names(result$events))
})

test_that("include_department = no removes departments table and department_key columns", {
  result <- remove_with(include_department = "no")
  expect_null(result$metadata$departments)
  expect_false("department_key" %in% names(result$patients))
  expect_false("department_key" %in% names(result$enrollments))
  expect_false("department_key" %in% names(result$events))
})

test_that("include_department = pseudo with dhis2 IDs keeps only department_key", {
  # `orgUnit` is the raw DHIS2 organisationUnit id; the privacy boundary for
  # the "pseudo" branch strips it from every table at the end of the
  # function, so the metadata$departments tibble has just `department_key`.
  result <- remove_with(include_department = "pseudo")
  expect_equal(
    names(result$metadata$departments),
    "department_key")
  # Foreign keys preserved in data tables
  expect_true("department_key" %in% names(result$patients))
  # Raw orgUnit DHIS2 id stripped from all data tables for pseudonymisation
  expect_false("orgUnit" %in% names(result$patients))
  expect_false("orgUnit" %in% names(result$enrollments))
  expect_false("orgUnit" %in% names(result$events))
})

test_that("include_department = pseudo without dhis2 IDs removes departments table", {
  result <- remove_with(
    include_department = "pseudo",
    include_dhis2_ids = c("patients", "enrollments",
      "events", "notes", "event_types", "users"))
  expect_null(result$metadata$departments)
})

# --- include_hospital ---

test_that("include_hospital = no removes hospitals table and hospital_key columns", {
  result <- remove_with(include_hospital = "no")
  expect_null(result$metadata$hospitals)
  expect_false("hospital_key" %in% names(result$patients))
  expect_false("hospital_key" %in% names(result$enrollments))
  expect_false("hospital_key" %in% names(result$events))
  # Also removed from departments metadata
  expect_false("hospital_key" %in% names(result$metadata$departments))
})

test_that("include_hospital = pseudo removes hospitals table but keeps hospital_key", {
  result <- remove_with(include_hospital = "pseudo")
  expect_null(result$metadata$hospitals)
  expect_true("hospital_key" %in% names(result$patients))
})

# --- include_country ---

test_that("include_country = no removes countries table and country_key from all tables", {
  result <- remove_with(include_country = "no")
  expect_null(result$metadata$countries)
  expect_false("country_key" %in% names(result$patients))
  expect_false("country_key" %in% names(result$enrollments))
  expect_false("country_key" %in% names(result$events))
  # Cascades to hospitals and departments metadata
  expect_false("country_key" %in% names(result$metadata$hospitals))
  expect_false("country_key" %in% names(result$metadata$departments))
})

test_that("include_country = pseudo removes countries table but keeps country_key", {
  result <- remove_with(include_country = "pseudo")
  expect_null(result$metadata$countries)
  expect_true("country_key" %in% names(result$patients))
})

# --- include_world_bank_class ---

test_that("include_world_bank_class = no removes WB table and world_bank_class_key everywhere", {
  result <- remove_with(include_world_bank_class = "no")
  expect_null(result$metadata$worldBankClasses)
  expect_false("world_bank_class_key" %in% names(result$patients))
  expect_false("world_bank_class_key" %in% names(result$enrollments))
  expect_false("world_bank_class_key" %in% names(result$events))
  # Cascades through metadata
  expect_false("world_bank_class_key" %in% names(result$metadata$countries))
  expect_false("world_bank_class_key" %in% names(result$metadata$hospitals))
  expect_false("world_bank_class_key" %in% names(result$metadata$departments))
})

test_that("include_world_bank_class = pseudo removes WB table but keeps key", {
  result <- remove_with(include_world_bank_class = "pseudo")
  expect_null(result$metadata$worldBankClasses)
  expect_true("world_bank_class_key" %in% names(result$patients))
})

test_that("include_world_bank_class = fullkeeps everything", {
  result <- remove_with(include_world_bank_class = "full")
  expect_false(is.null(result$metadata$worldBankClasses))
  expect_true("world_bank_class_key" %in% names(result$patients))
})

# --- Cascading removal ---

test_that("removing country with hospital = no does not error on missing hospitals", {
  result <- remove_with(
    include_hospital = "no",
    include_country  = "no")
  expect_null(result$metadata$hospitals)
  expect_null(result$metadata$countries)
})

test_that("removing world_bank_class with country = no does not error on missing countries", {
  result <- remove_with(
    include_country          = "no",
    include_world_bank_class = "no")
  expect_null(result$metadata$countries)
  expect_null(result$metadata$worldBankClasses)
})

# --- Full removal (most restrictive) ---

test_that("all include flags at most restrictive removes all optional data", {
  result <- remove_with(
    patient_columns          = character(),
    include_dhis2_ids        = character(),
    include_department       = "no",
    include_hospital         = "no",
    include_country          = "no",
    include_world_bank_class = "no")
  # Core keys survive
  expect_true("patient_key" %in% names(result$patients))
  expect_true("enrollment_key" %in% names(result$enrollments))
  expect_true("event_key" %in% names(result$events))
  # Optional data removed
  expect_false("patient_id" %in% names(result$patients))
  expect_false("trackedEntity" %in% names(result$patients))
  expect_null(result$metadata$departments)
  expect_null(result$metadata$hospitals)
  expect_null(result$metadata$countries)
  expect_null(result$metadata$worldBankClasses)
})

# --- Null-safe: eventNotes can be NULL ---

test_that("apply_data_removal handles NULL eventNotes gracefully", {
  ds <- base_ds
  ds$eventNotes <- NULL
  result <- remove_with(ds, include_dhis2_ids = character())
  expect_null(result$eventNotes)
})
