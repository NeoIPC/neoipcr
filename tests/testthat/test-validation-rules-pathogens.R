# Tests for R/validation-rules-pathogens.R — rule 20.

# --- Rule 20: unknown pathogen (STUB) ---

test_that("rule 20 returns no rows (stub, not yet migrated)", {
  ds <- make_populated_test_ds()
  result <- neoipcr:::validation_rule_20(ds, NULL)
  expect_equal(nrow(result), 0L)
})

test_that("rule 20 detects unknown pathogen (code = 0)", {
  skip("Rule 20 not yet migrated from Validation-Report")
  # When migrated: build ds with infectiousAgentFindings containing
  # pathogen_key pointing to a pathogen with code 0
})

test_that("rule 20 honours exceptions", {
  skip("Rule 20 not yet migrated from Validation-Report")
})
