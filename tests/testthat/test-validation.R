# Tests for R/validation.R — validate() orchestrator and validation_rules registry.

test_that("validation_rules registry has 42 entries with an id and a function each", {
  expect_equal(length(neoipcr:::validation_rules), 42L)
  for (entry in neoipcr:::validation_rules) {
    expect_named(entry, c("id", "fun"))
    expect_true(is.integer(entry$id))
    expect_true(is.function(entry$fun))
  }
})

test_that("validation_rule_ids is exported and lists the registry in order", {
  namespace <- readLines(system.file("NAMESPACE", package = "neoipcr"))
  expect_true("export(validation_rule_ids)" %in% namespace)
  expect_identical(neoipcr::validation_rule_ids(), 1:42)
})

# The populated fixture with its surveillance-end forms made consistent: the
# patient days as the enrolment dates and end events imply them, and no
# antibiotic days for the substance days to fall short of.
clean_ds <- function() {
  ds <- make_populated_test_ds()
  ds$surveillanceEndData$patient_days <- 15L
  ds$surveillanceEndData$ab_days      <- 0L
  ds
}

# A dataset two rules flag: the admission event is dated a day after the
# enrolment (rule 3, at the enrolment level), and the completed enrolment has
# no surveillance-end event (rule 25).
rule_3_flagged_ds <- function()
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1,
      patient_keys = 1L,
      enrolledAt = as.Date("2024-01-01")),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "adm",
      occurredAt = as.Date("2024-01-02")))

test_that("validate returns a zero-row tibble on consistent data", {
  result <- neoipcr::validate(clean_ds())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("rule_id", "patient_key", "enrollment_key", "event_key", "context"))
})

test_that("validate runs only the rules named", {
  ds <- rule_3_flagged_ds()
  expect_setequal(neoipcr::validate(ds)$rule_id, c(3L, 25L))
  result <- neoipcr::validate(ds, rules = 3L)
  expect_equal(result$rule_id, 3L)
})

test_that("validate refuses a rule id it does not know", {
  ds <- make_populated_test_ds()
  expect_error(
    neoipcr::validate(ds, rules = 99L),
    class = "neoipcr_unknown_validation_rule")
  expect_error(
    neoipcr::validate(ds, rules = c(1L, 43L)),
    regexp = "43")
  expect_error(
    neoipcr::validate(ds, rules = 1.5),
    class = "neoipcr_unknown_validation_rule")
  expect_error(
    neoipcr::validate(ds, rules = "1"),
    class = "neoipcr_unknown_validation_rule")
  # A double that is a whole number names a rule.
  expect_s3_class(neoipcr::validate(ds, rules = 1), "tbl_df")
})

test_that("validate is exported and returns its result visibly", {
  # Read the NAMESPACE file rather than getNamespaceExports(): under
  # devtools::load_all() every object is exported, which would make the
  # check pass whether or not the roxygen `@export` tag is present.
  # `system.file()` resolves the right copy in both workflows: pkgload's shim
  # points it at this checkout under load_all(), and R CMD check at the copy
  # installed from it (the checkout is not present there, so a path relative
  # to the test tree would not be either).
  namespace <- readLines(system.file("NAMESPACE", package = "neoipcr"))
  expect_true("export(validate)" %in% namespace)
  ds <- make_populated_test_ds()
  expect_true(withVisible(neoipcr::validate(ds))$visible)
})

test_that("validate requires the full enrollment and event tiers, whose columns the rules read", {
  ds <- make_populated_test_ds()

  pseudo_events <- ds
  pseudo_events$metadata$dataset_options$include_event <- "pseudo"
  expect_error(neoipcr::validate(pseudo_events), "include_event")

  pseudo_enrollments <- ds
  pseudo_enrollments$metadata$dataset_options$include_enrollment <- "pseudo"
  expect_error(neoipcr::validate(pseudo_enrollments), "include_enrollment")
})

test_that("validate always carries its five columns, whatever ran", {
  ds <- make_populated_test_ds()
  shape <- c("rule_id", "patient_key", "enrollment_key", "event_key", "context")
  # Rule 1 records no context; the column is part of the shape regardless.
  r <- neoipcr::validate(ds, rules = 1L)
  expect_named(r, shape)
  expect_type(r$context, "list")
  expect_named(neoipcr::validate(ds), shape)
  # Every enrolment in this dataset is completed, so rule 2 finds no active
  # one; the shape still holds, with zero rows.
  r <- neoipcr::validate(ds, rules = 2L)
  expect_named(r, shape)
  expect_equal(nrow(r), 0L)
  expect_type(r$rule_id, "integer")
  # Rule 3 nests its context; the keys are integer, and the class is a plain
  # tibble.
  r <- neoipcr::validate(ds, rules = 3L)
  expect_named(r, shape)
  expect_false(dplyr::is_grouped_df(r))
  expect_type(r$enrollment_key, "integer")
  expect_identical(class(r), c("tbl_df", "tbl", "data.frame"))
  # A rule that skips itself for want of a column contributes nothing; the
  # shape still holds.
  skipping <- ds
  skipping$surveillanceEndData$patient_days <- NULL
  r <- neoipcr::validate(skipping, rules = 18L)
  expect_named(r, shape)
  expect_equal(nrow(r), 0L)
})

test_that("validate carries a rule's values as a one-row tibble in context", {
  r <- neoipcr::validate(rule_3_flagged_ds(), rules = 3L)
  expect_equal(nrow(r), 1L)
  expect_s3_class(r$context[[1]], "tbl_df")
  expect_equal(nrow(r$context[[1]]), 1L)
  expect_named(r$context[[1]], c("enrolledAt", "occurredAt"))
})

test_that("validate exempts records named in key form", {
  ds <- rule_3_flagged_ds()
  expect_equal(nrow(neoipcr::validate(ds, rules = 3L)), 1L)
  # The key an exception is matched on is the rule's level; a record on
  # another level, or for another rule, exempts nothing.
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3L, enrollment_key = 1L))), 0L)
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 4L, enrollment_key = 1L))), 1L)
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3L, event_key = 1L))), 1L)
})

test_that("validate resolves an exception list written in the user's form", {
  ds <- rule_3_flagged_ds()
  ds$metadata$departments <- make_test_metadata_departments(n = 1)
  written <- tibble::tibble(
    RULE_ID           = 3L,
    NEOIPC_PATIENT_ID = "PAT_1",
    ENROLMENT_DATE    = as.Date("2024-01-01"),
    EVENT_TYPE        = NA_character_,
    EVENT_DATE        = as.Date(NA))
  expect_equal(nrow(neoipcr::validate(ds, rules = 3L, exceptions = written)), 0L)
  # A record naming a patient the dataset does not hold exempts nothing.
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L,
    exceptions = written |> dplyr::mutate(NEOIPC_PATIENT_ID = "PAT_9"))), 1L)
  # A record that names the enrolment's admission event names the enrolment
  # too, and exempts it once it has resolved as a whole; one naming an event
  # the dataset does not hold resolves to nothing, whatever else it names.
  on_event <- written |>
    dplyr::mutate(EVENT_TYPE = "adm", EVENT_DATE = as.Date("2024-01-02"))
  expect_equal(nrow(neoipcr::validate(ds, rules = 3L, exceptions = on_event)), 0L)
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L,
    exceptions = on_event |> dplyr::mutate(EVENT_DATE = as.Date("2024-01-07")))), 1L)
})

test_that("validate refuses exceptions that are neither form", {
  ds <- rule_3_flagged_ds()
  expect_error(
    neoipcr::validate(ds, exceptions = tibble::tibble(patient_key = 1L)),
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, exceptions = "PAT_1"),
    class = "neoipcr_invalid_exception_list")
})

test_that("validate checks a key-form list as it checks the written form", {
  ds <- rule_3_flagged_ds()
  # A rule id no rule carries, or a key that is not an integer, is refused
  # rather than carried along as an exception that exempts nothing.
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 99L, enrollment_key = 1L)),
    regexp = "99",
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = "1")),
    regexp = "enrollment_key",
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = NA_integer_, enrollment_key = 1L)),
    class = "neoipcr_invalid_exception_list")
  # Whole-number doubles are integers in disguise and are accepted.
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3, enrollment_key = 1))), 0L)
})
