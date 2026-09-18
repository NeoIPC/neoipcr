# Tests for R/validation-rules-pathogens.R — rule 20.

# One sepsis event (key 1) with one infectious-agent finding, whose columns
# take `...`, and optionally the free-text name entered for it.
pathogen_ds <- function(..., unknown_name = NULL)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "bsi"),
    infectiousAgentFindings = make_test_iaf(event_keys = 1L, ...),
    unknownPathogenNames = if (is.null(unknown_name))
      make_test_unknown_pathogen_names(integer(0))
    else
      make_test_unknown_pathogen_names(1L, name = unknown_name))

test_that("rule 20 detects a finding recorded as the unknown pathogen, with its entered name", {
  result <- neoipcr:::validation_rule_20(
    pathogen_ds(pathogen_key = 0L, index = 2L, secondary_bsi = TRUE, unknown_name = "Some bacterium"),
    NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$rule_id, 20L)
  expect_equal(result$event_key, 1L)
  expect_equal(result$enrollment_key, 1L)
  expect_named(result$context[[1]], c("index", "secondary_bsi", "name"))
  expect_equal(result$context[[1]]$index, 2L)
  expect_true(result$context[[1]]$secondary_bsi)
  expect_equal(result$context[[1]]$name, "Some bacterium")
})

test_that("rule 20 reports a missing name as NA", {
  result <- neoipcr:::validation_rule_20(pathogen_ds(pathogen_key = 0L), NULL)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$context[[1]]$name))
})

test_that("rule 20 returns no rows for a catalogued pathogen", {
  expect_equal(nrow(neoipcr:::validation_rule_20(pathogen_ds(pathogen_key = 5L), NULL)), 0L)
})

test_that("rule 20 honours exceptions", {
  result <- neoipcr:::validation_rule_20(
    pathogen_ds(pathogen_key = 0L), make_test_exceptions(20L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

# NEOIPC-PERMANENT(dataset-format): this test guards a path that must never be
# removed. A dataset serialized before the free-text pathogen names had a slot
# of their own carries no `unknownPathogenNames`, and a file on disk outlives
# the code that wrote it — so rule 20 skips such a dataset for good instead of
# failing it. Delete this test only if that branch is deliberately being
# dropped, which it should not be.
test_that("rule 20 skips without a warning when the unknown pathogen names are absent", {
  ds <- pathogen_ds(pathogen_key = 0L)
  ds$unknownPathogenNames <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_20(ds, NULL))
  expect_null(result)
})
