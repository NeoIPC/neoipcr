# Tests for R/schema-validation.R — the validation slots' schemas and the
# summary the import derives from a validation pass.

finding_cols <- c("rule_id", "patient_key", "enrollment_key", "event_key", "context")
summary_cols <- c("rule_id", "record_kind", "n_removed", "n_exempted")

test_that("the validation slots exist exactly when the validation pass runs", {
  for (patient in c("no", "pseudo", "full")) {
    for (invalid in list(FALSE, TRUE, tibble::tibble(RULE_ID = 3L))) {
      opts <- dhis2_dataset_options(
        include_patient = patient, include_invalid_patients = invalid)
      results <- compile_schema(validationResults_cols, opts)
      summary <- compile_schema(validationSummary_cols, opts)
      if (patient == "no" || isTRUE(invalid)) {
        expect_equal(ncol(results), 0L)
        expect_equal(ncol(summary), 0L)
      } else {
        expect_named(results, finding_cols)
        expect_named(summary, summary_cols)
        expect_type(results$context, "list")
        expect_identical(
          levels(summary$record_kind), c("patients", "enrollments", "events"))
        expect_type(summary$n_removed, "integer")
      }
    }
  }
})

test_that("a test dataset carries both slots as an import that found nothing leaves them", {
  ds <- make_test_ds()
  expect_named(ds$validationResults, finding_cols)
  expect_equal(nrow(ds$validationResults), 0L)
  expect_named(ds$validationSummary, summary_cols)
  expect_true(all(is.na(ds$validationSummary$rule_id)))
  expect_equal(ds$validationSummary$n_removed, c(0L, 0L, 0L))

  metadata <- read_test_metadata()
  metadata$dataset_options <- dhis2_dataset_options(
    include_department = "full", include_patient = "full",
    include_enrollment = "full", include_event = "full",
    include_invalid_patients = TRUE)
  ds <- make_test_ds(metadata = metadata)
  expect_equal(ncol(ds$validationResults), 0L)
  expect_equal(ncol(ds$validationSummary), 0L)
})

test_that("validate returns the finding shape whatever the dataset's options say about invalid patients", {
  ds <- make_populated_test_ds()
  ds$metadata$dataset_options$include_invalid_patients <- TRUE
  expect_named(neoipcr::validate(ds), finding_cols)
  expect_equal(ncol(compile_schema(validationResults_cols, ds$metadata$dataset_options)), 0L)
})

test_that("the validation summary counts distinct records per rule and the records the findings concern per kind", {
  finding <- function(rule_id, patient_key, enrollment_key = NA_integer_,
                      event_key = NA_integer_)
    tibble::tibble(
      rule_id = rule_id, patient_key = patient_key,
      enrollment_key = enrollment_key, event_key = event_key,
      context = list(NULL))
  # Rules 3 and 25 both flag enrolments 1 and 2, rule 3 naming the admission
  # form it compared as it does; rule 1 flags patient 9; rule 20 flags event
  # 7 of enrolment 2 twice, once per unknown pathogen name; and rule 3 also
  # flagged enrolment 3 of patient 2, which the exception list exempted.
  removed <- dplyr::bind_rows(
    finding(3L, 1L, 1L, 11L), finding(3L, 2L, 2L, 12L),
    finding(25L, 1L, 1L), finding(25L, 2L, 2L),
    finding(20L, 2L, 2L, 7L), finding(20L, 2L, 2L, 7L),
    finding(1L, 9L))
  exempted <- finding(3L, 2L, 3L, 13L)

  s <- neoipcr:::.validation_summary(removed, exempted)

  expect_named(s, summary_cols)
  expect_s3_class(s$record_kind, "factor")
  per_rule <- s[!is.na(s$rule_id), ]
  expect_equal(per_rule$rule_id, c(1L, 3L, 25L, 20L))
  # Rule 3 is recorded on the enrolment although its findings name an
  # event: the kind is the registry's level, not the deepest key.
  expect_equal(as.character(per_rule$record_kind),
               c("patients", "enrollments", "enrollments", "events"))
  # Rule 20's two findings on one event are one event.
  expect_equal(per_rule$n_removed, c(1L, 2L, 2L, 1L))
  expect_equal(per_rule$n_exempted, c(0L, 1L, 0L, 0L))
  totals <- s[is.na(s$rule_id), ]
  expect_equal(as.character(totals$record_kind),
               c("patients", "enrollments", "events"))
  # Every finding concerns its patient, so patients 1, 2 and 9 are removed;
  # enrolments 1 and 2 are concerned however many rules flagged each; the
  # admission forms rule 3 named are not events concerned, event 7 is.
  expect_equal(totals$n_removed, c(3L, 2L, 1L))
  expect_equal(totals$n_exempted, c(1L, 1L, 0L))
})

test_that("the validation summary of a pass with no findings has its totals rows at zero", {
  empty <- compile_schema(validation_finding_atoms, dhis2_dataset_options())
  s <- neoipcr:::.validation_summary(empty, empty)
  expect_equal(nrow(s), 3L)
  expect_true(all(is.na(s$rule_id)))
  expect_equal(s$n_removed, c(0L, 0L, 0L))
})
