test_that("get_procedure_category_pretty labels the current category codes", {
  expect_equal(
    neoipcr:::get_procedure_category_pretty(
      c("abdominal_surgery", "not_surgery", "to_be_categorized")),
    c("Abdominal surgery", "Not a surgical procedure", "Not yet categorized"))
})

test_that("get_procedure_category emits the current spelling of the default code", {
  # `.default` is what an uncategorized ICHI code falls through to, and it is
  # the value that reaches `surgery_rate_table` and therefore a serialized
  # dataset — so the spelling is a storage contract, not a display choice.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("ZZZ.ZZ.ZZ")),
    "to_be_categorized")
})

# NEOIPC-PERMANENT(dataset-format): this test guards a path that must never be
# removed. A dataset serialized before the category code took its -ize spelling
# carries `to_be_categorised`, and a file on disk outlives the code that wrote
# it — so the older code stays renderable for good. Delete this test only if
# the mapping it guards is deliberately being dropped, which it should not be.
test_that("get_procedure_category_pretty still labels a pre-rename category code", {
  expect_equal(
    neoipcr:::get_procedure_category_pretty("to_be_categorised"),
    "Not yet categorized")
})

test_that("get_procedure_category_pretty renders both spellings identically", {
  # The two codes mean the same thing, so a dataset written before the rename
  # and one written after must produce the same label for the same rows.
  expect_equal(
    neoipcr:::get_procedure_category_pretty("to_be_categorised"),
    neoipcr:::get_procedure_category_pretty("to_be_categorized"))
})

test_that("get_procedure_category_pretty passes an unknown code through unchanged", {
  expect_equal(neoipcr:::get_procedure_category_pretty("not_a_category"),
               "not_a_category")
})
