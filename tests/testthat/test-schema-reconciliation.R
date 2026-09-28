# Tests for R/schema-reconciliation.R — the reconciliation slot's schema and
# the summary the import derives from its reconciliation log.

reconciliation_summary_cols <- c(
  "reconciliation_id", "record_kind", "n_repaired", "n_reported")

test_that("the reconciliation slot exists exactly when the import reconciles", {
  for (opts in iter_dataset_options(c("reconcile", "include_patient", "include_event"))) {
    summary <- neoipcr:::get_reconciliationSummary_schema(opts)
    info <- sprintf("reconcile = %s, include_patient = %s, include_event = %s",
                    opts$reconcile, opts$include_patient, opts$include_event)
    if (opts$reconcile && opts$include_patient != "no") {
      expect_named(summary, reconciliation_summary_cols)
      expect_type(summary$reconciliation_id, "integer")
      expect_identical(
        levels(summary$record_kind), c("patients", "enrollments", "events"))
      expect_type(summary$n_repaired, "integer")
      expect_type(summary$n_reported, "integer")
    } else {
      expect_equal(ncol(summary), 0L, info = info)
    }
  }
})

test_that("the reconciliation slot's gate is not the validation pass's", {
  # An import that keeps the invalid patients runs no pass and reconciles all
  # the same.
  opts <- dhis2_dataset_options(
    include_patient = "full", include_invalid_patients = TRUE)
  expect_named(neoipcr:::get_reconciliationSummary_schema(opts),
               reconciliation_summary_cols)
  expect_equal(
    ncol(neoipcr:::compile_schema(neoipcr:::validationSummary_cols, opts)), 0L)
})

test_that("an options object without the reconciliation switch reads as one that reconciled nothing", {
  # Such an object predates the option, and so does the import that wrote it.
  opts <- dhis2_dataset_options(include_patient = "full")
  opts$reconcile <- NULL
  expect_false(neoipcr:::.reconciliation_runs(opts))
  expect_equal(ncol(neoipcr:::get_reconciliationSummary_schema(opts)), 0L)
})

test_that("a test dataset carries the reconciliation slot as an import that repaired nothing leaves it", {
  ds <- make_test_ds()
  expect_named(ds$reconciliationSummary, reconciliation_summary_cols)
  expect_equal(ds$reconciliationSummary$reconciliation_id, reconciliation_ids())
  expect_equal(ds$reconciliationSummary$n_repaired, rep(0L, 6L))
  expect_equal(ds$reconciliationSummary$n_reported, rep(0L, 6L))
  expect_named(make_populated_test_ds()$reconciliationSummary,
               reconciliation_summary_cols)

  metadata <- read_test_metadata()
  metadata$dataset_options <- dhis2_dataset_options(
    include_department = "full", include_patient = "full",
    include_enrollment = "full", include_event = "full",
    reconcile = FALSE)
  ds <- make_test_ds(metadata = metadata)
  expect_equal(ncol(ds$reconciliationSummary), 0L)
})

# A dataset holding the patients, enrolments and events `keys` names, under
# the options `...` set on top of the full tiers.
summary_ds <- function(patient_keys = 1:3, enrollment_keys = 1:3,
                       event_keys = 1:3, ...) {
  opts <- utils::modifyList(
    list(include_patient = "full", include_enrollment = "full",
         include_event = "full"),
    list(...))
  list(
    patients    = tibble::tibble(patient_key = patient_keys),
    enrollments = tibble::tibble(enrollment_key = enrollment_keys),
    events      = tibble::tibble(event_key = event_keys),
    metadata    = list(dataset_options = do.call(dhis2_dataset_options, opts)))
}

log_rows <- function(reconciliation_id, action, patient_key,
                     enrollment_key = NA_integer_, event_key = NA_integer_)
  tibble::tibble(
    reconciliation_id = as.integer(reconciliation_id),
    action            = action,
    patient_key       = as.integer(patient_key),
    enrollment_key    = as.integer(enrollment_key),
    event_key         = as.integer(event_key))

test_that("the reconciliation summary counts the distinct records of the dataset per reconciliation and action", {
  log <- dplyr::bind_rows(
    log_rows(1L, "repair", 1L, 1L, 11L),
    log_rows(1L, "repair", 2L, 2L, 12L),
    # A record a reconciliation lists twice is one record.
    log_rows(3L, "repair", c(1L, 1L)),
    # Records the dataset no longer holds are not counted.
    log_rows(4L, "repair", c(3L, 9L)),
    log_rows(2L, "repair", 1L, 1L, c(1L, 2L, 99L)),
    log_rows(6L, "repair", 1L, 1L, 1L),
    log_rows(6L, "report", 2L, 2L, c(2L, 3L)),
    log_rows(1L, "repair", 9L, 9L, 19L))

  s <- neoipcr:::.reconciliation_summary(log, summary_ds())

  expect_named(s, reconciliation_summary_cols)
  expect_equal(s$reconciliation_id, 1:6)
  expect_equal(as.character(s$record_kind),
               c("enrollments", "events", "patients", "patients", "events", "events"))
  expect_equal(s$n_repaired, c(2L, 2L, 1L, 1L, 0L, 1L))
  expect_equal(s$n_reported, c(0L, 0L, 0L, 0L, 0L, 2L))
})

test_that("the reconciliation summary is missing a count where the import could not read the records a reconciliation acts on", {
  log <- log_rows(3L, "repair", 1L)
  # The forms are read through their events and the events through their
  # enrolments, so without either tier reconciliations 1, 2, 5 and 6 read
  # nothing: a count of 0 would claim they found nothing to change.
  s <- neoipcr:::.reconciliation_summary(log, summary_ds(include_enrollment = "no"))
  expect_equal(s$n_repaired, c(NA, NA, 1L, 0L, NA, NA))
  expect_equal(s$n_reported, c(NA, NA, 0L, 0L, NA, NA))
  # The dataset holds the enrolments here, but not the admission forms
  # reconciliation 1 acts on.
  s <- neoipcr:::.reconciliation_summary(log, summary_ds(include_event = "no"))
  expect_equal(s$n_repaired, c(NA, NA, 1L, 0L, NA, NA))
  expect_equal(s$n_reported, c(NA, NA, 0L, 0L, NA, NA))
  # The pseudonymized tiers hold every key, and are counted.
  s <- neoipcr:::.reconciliation_summary(
    log, summary_ds(include_enrollment = "pseudo", include_event = "pseudo",
                    include_patient = "pseudo"))
  expect_equal(s$n_repaired, c(0L, 0L, 1L, 0L, 0L, 0L))
})

test_that("the reconciliation summary is 0×0 where the import reconciled nothing", {
  expect_equal(
    dim(neoipcr:::.reconciliation_summary(
      neoipcr:::.reconciliation_log(), summary_ds(reconcile = FALSE))),
    c(0L, 0L))
  expect_equal(
    dim(neoipcr:::.reconciliation_summary(
      neoipcr:::.reconciliation_log(), summary_ds(include_patient = "no"))),
    c(0L, 0L))
})
