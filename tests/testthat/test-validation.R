# Tests for R/validation.R — validate() orchestrator and validation_rules registry.

test_that("validation_rules registry has 41 entries with an id, a level and a function each", {
  expect_equal(length(neoipcr:::validation_rules), 41L)
  for (entry in neoipcr:::validation_rules) {
    expect_true(all(c("id", "level", "fun") %in% names(entry)))
    expect_true(is.integer(entry$id))
    expect_true(entry$level %in% c("patient", "enrollment", "event"))
    # An event-level rule names the event types it concerns; no other does.
    expect_equal("event_types" %in% names(entry), entry$level == "event")
    if (entry$level == "event")
      expect_true(all(entry$event_types %in% neoipcr:::.exception_event_types))
    expect_true(is.function(entry$fun))
  }
  levels <- neoipcr:::.rule_levels()
  expect_equal(unname(levels["1"]), "patient")
  expect_equal(unname(levels["3"]), "enrollment")
  expect_equal(unname(levels["12"]), "event")
  expect_equal(neoipcr:::.rule_event_types(20L), c("bsi", "nec", "hap", "ssi"))
})

test_that("validation_rule_ids is exported and lists the registry in order", {
  namespace <- readLines(system.file("NAMESPACE", package = "neoipcr"))
  expect_true("export(validation_rule_ids)" %in% namespace)
  # Rule 16 is gone, so the ids keep their numbering with a gap at 16.
  expect_identical(neoipcr::validation_rule_ids(), c(1:15, 17:42))
  expect_true("export(validation_rule_context_fields)" %in% namespace)
  fields <- neoipcr::validation_rule_context_fields()
  expect_identical(names(fields), as.character(neoipcr::validation_rule_ids()))
  expect_identical(fields[["1"]], character())
  expect_setequal(fields[["3"]], c("enrolledAt", "occurredAt"))
  expect_setequal(fields[["20"]], c("index", "secondary_bsi", "name"))
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

# A dataset that triggers two rules: its admission event is dated a day after
# the enrolment (rule 3, at the enrolment level), and its completed enrolment
# has no surveillance-end event (rule 25).
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
  # An infinite id is refused outright, not cast to `NA` with a warning
  # first: the first condition signalled is the classed error.
  cnd <- rlang::catch_cnd(neoipcr::validate(ds, rules = Inf))
  expect_s3_class(cnd, "neoipcr_unknown_validation_rule")
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
  # shape still holds, and the result names the rule it could not run.
  skipping <- ds
  skipping$surveillanceEndData$patient_days <- NULL
  r <- neoipcr::validate(skipping, rules = 18L)
  expect_named(r, shape)
  expect_equal(nrow(r), 0L)
  expect_identical(attr(r, "rules_skipped"), 18L)
  r <- neoipcr::validate(skipping, rules = c(3L, 18L))
  expect_identical(attr(r, "rules_skipped"), 18L)
  # A run in which every selected rule ran says so with an empty vector.
  expect_identical(attr(neoipcr::validate(ds, rules = 18L), "rules_skipped"),
                   integer(0))
})

test_that("a finding whose fields differ from the registry's declaration is refused", {
  # The declaration is the contract consumers write against, so a rule that
  # drifts from it must fail rather than reach a rendered document. The
  # check validate() runs with the registry's declaration is exercised on
  # its own, with a declaration that drops a field of rule 3.
  findings <- neoipcr::validate(rule_3_flagged_ds(), rules = 3L)
  declared <- neoipcr::validation_rule_context_fields()
  expect_invisible(neoipcr:::.assert_declared_context(findings, declared))
  declared[["3"]] <- "enrolledAt"
  expect_error(
    neoipcr:::.assert_declared_context(findings, declared),
    "Rule\\(s\\): 3")
  # A rule that declares no fields takes a finding without context.
  none <- findings
  none$rule_id <- 1L
  none$context <- list(NULL)
  expect_invisible(neoipcr:::.assert_declared_context(none, declared))
})

test_that("a pass that could not run a rule is refused where it must read as complete", {
  # The import stores a pass without its `rules_skipped` bookkeeping, so it
  # refuses one that skipped a rule rather than storing it as complete; the
  # check is exercised on the result validate() returns, with and without
  # a skipped rule recorded on it.
  findings <- neoipcr::validate(rule_3_flagged_ds(), rules = 3L)
  expect_identical(attr(findings, "rules_skipped"), integer())
  expect_invisible(neoipcr:::.assert_no_rule_skipped(findings))
  attr(findings, "rules_skipped") <- c(18L, 19L)
  expect_error(
    neoipcr:::.assert_no_rule_skipped(findings),
    class = "neoipcr_validation_rule_skipped")
  expect_error(
    neoipcr:::.assert_no_rule_skipped(findings),
    "18, 19")
})

test_that("validate carries a rule's values as a one-row tibble in context", {
  r <- neoipcr::validate(rule_3_flagged_ds(), rules = 3L)
  expect_equal(nrow(r), 1L)
  expect_s3_class(r$context[[1]], "tbl_df")
  expect_equal(nrow(r$context[[1]]), 1L)
  expect_named(r$context[[1]], c("enrolledAt", "occurredAt"))
  expect_setequal(
    names(r$context[[1]]), neoipcr::validation_rule_context_fields()[["3"]])
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
  # Rule 3 is recorded on the enrolment, so a record for it that names an
  # event is written at the wrong level and refused rather than resolved.
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = written |>
        dplyr::mutate(EVENT_TYPE = "adm", EVENT_DATE = as.Date("2024-01-02"))),
    regexp = "level",
    class = "neoipcr_invalid_exception_list")
})

test_that("validate refuses exceptions that are neither form", {
  ds <- rule_3_flagged_ds()
  expect_error(
    neoipcr::validate(ds, exceptions = tibble::tibble(patient_key = 1L)),
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, exceptions = "PAT_1"),
    class = "neoipcr_invalid_exception_list")
  # A key-form record without any record key could name nothing and is
  # refused rather than carried along; an empty key-form table is fine.
  expect_error(
    neoipcr::validate(ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3L)),
    regexp = "patient_key",
    class = "neoipcr_invalid_exception_list")
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = integer()))), 1L)
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
  # A fraction would be truncated onto another record's key, an infinity
  # onto `NA`; both are refused rather than cast.
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = 1.5)),
    regexp = "enrollment_key",
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = Inf)),
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = Inf, enrollment_key = 1L)),
    class = "neoipcr_invalid_exception_list")
  # A whole number beyond R's integer range, or `NaN`, would become `NA` in
  # the cast.
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = 2147483648)),
    regexp = "enrollment_key",
    class = "neoipcr_invalid_exception_list")
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = NaN)),
    regexp = "enrollment_key",
    class = "neoipcr_invalid_exception_list")
  # An `NA` of a type that cannot be bound onto the integer keys is refused
  # under the same class; a bare `NA` is accepted.
  expect_error(
    neoipcr::validate(ds, rules = 3L,
      exceptions = tibble::tibble(rule_id = 3L, enrollment_key = 1L, event_key = NA_character_)),
    regexp = "event_key",
    class = "neoipcr_invalid_exception_list")
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3L, enrollment_key = 1L, event_key = NA))), 0L)
  # Whole-number doubles are integers in disguise and are accepted.
  expect_equal(nrow(neoipcr::validate(
    ds, rules = 3L, exceptions = tibble::tibble(rule_id = 3, enrollment_key = 1))), 0L)
})
