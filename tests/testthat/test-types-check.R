# Tests for R/types-check.R — the import-option precondition of the exported
# functions and how its refusal shows the option values.

test_that("assert_options_for shows a string option value in quotes", {
  ds <- make_test_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  error <- expect_error(neoipcr::validate(ds))
  expect_match(conditionMessage(error),
               "`include_event` is \"pseudo\"; need one of \"full\".", fixed = TRUE)
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
