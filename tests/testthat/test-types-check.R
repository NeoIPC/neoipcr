# Tests for R/types-check.R — the import-option and stored-dataset
# preconditions of the exported functions and how their refusals read.

test_that("assert_options_for shows a string option value in quotes", {
  ds <- make_test_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  error <- expect_error(neoipcr::validate(ds))
  expect_match(conditionMessage(error),
               "`include_event` is \"pseudo\"; need one of \"full\".", fixed = TRUE)
})

test_that("assert_options_for gives the reason of an unmet option, and only of one unmet", {
  ds <- make_test_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  cnd <- expect_error(
    neoipcr:::assert_options_for(
      ds,
      required = list(include_event = "full", include_patient = "full"),
      reasons  = list(include_event   = "The events are needed whole.",
                      include_patient = "The patients are needed whole."),
      fn_name  = "f"),
    class = "neoipcr_unmet_import_options")
  expect_match(conditionMessage(cnd), "The events are needed whole.", fixed = TRUE)
  expect_no_match(conditionMessage(cnd), "The patients are needed whole.", fixed = TRUE)
})

test_that("assert_hierarchy_pair accepts a dataset with the full pair", {
  ds <- make_calc_test_ds()
  # make_calc_test_ds's options set include_department and include_country
  # to "full"; hospital and world_bank_class are "no", so their legs are
  # not required.
  expect_no_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"))
})

test_that("assert_hierarchy_pair names only the legs of imported levels", {
  # The stale fixture drops all four legs of both imported levels. The
  # refusal must name exactly those four, not the unimported levels' legs.
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_hospital      <- "no"
  ds$metadata$dataset_options$include_world_bank_class <- "no"
  ds <- make_stale_import_ds(ds)
  cnd <- expect_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"),
    class = "neoipcr_missing_hierarchy_pair")
  msg <- conditionMessage(cnd)
  expect_match(msg, "`event_department_key`",       fixed = TRUE)
  expect_match(msg, "`enrollment_department_key`",   fixed = TRUE)
  expect_match(msg, "`event_country_key`",           fixed = TRUE)
  expect_match(msg, "`enrollment_country_key`",      fixed = TRUE)
  expect_no_match(msg, "hospital_key",                fixed = TRUE)
  expect_no_match(msg, "world_bank_class_key",        fixed = TRUE)
})

test_that("assert_hierarchy_pair refuses a single missing leg of an imported level", {
  # Either leg is required on its own: the `event_` side names the
  # recording unit and the `enrollment_` side the stay, and every
  # grouping of the calculations needs the second.
  ds <- make_calc_test_ds()
  ds$events$enrollment_country_key <- NULL
  cnd <- expect_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "Its events table lacks `enrollment_country_key`.",
               fixed = TRUE)
})

test_that("assert_options_for shows a logical option value as code, not as a string", {
  # A flag requirement is `TRUE`, the form its comment documents; quoted, it
  # would read as the string "TRUE".
  ds <- make_test_ds()
  ds$metadata$dataset_options$include_test_data <- FALSE
  error <- expect_error(neoipcr:::assert_options_for(
    ds, required = list(include_test_data = TRUE), fn_name = "f"))
  expect_match(conditionMessage(error),
               "`include_test_data` is `FALSE`; need one of `TRUE`.", fixed = TRUE)
})

test_that("assert_hierarchy_pair takes a current dataset whose events sit at the pseudo tier", {
  # `event_hierarchy_col()` (R/schema-events.R) materializes the pair
  # only under `include_event = "full"`, so the legs being absent at
  # that tier is the shape of a current import, not of an older one.
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  ds$events <- ds$events["event_key"]
  expect_no_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"))
})

test_that("assert_hierarchy_pair refuses a dataset whose import options it cannot read", {
  # An options block that cannot be read is refused, not taken for
  # consent: its absence cannot show that the legs were left out.
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options <- NULL
  cnd <- expect_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "its import tiers cannot be read.", fixed = TRUE)
})

test_that("assert_hierarchy_pair refuses a level option outside the tiers", {
  # NA is not a tier; reading it as "not asked for" would let a
  # half-written options block pass over the legs a full import
  # materializes.
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_department <- NA
  cnd <- expect_error(
    neoipcr:::assert_hierarchy_pair(ds, fn_name = "f"),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "need one of \"no\", \"pseudo\", \"full\".", fixed = TRUE)
})
