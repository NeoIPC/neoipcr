# Test helpers for the schema engine.
#
# `expect_schema_matches(x, expected)` is the workhorse assertion used by
# every Phase B entity test: the actual reader output must match the
# expected 0-row compiled schema on column names, order, base class, and
# fixed-levels factor level lists.
#
# `iter_dataset_options(fields = …)` generates the cross-product of valid
# values for the named option fields, keeping other fields at their
# defaults. Used to parameterize tests across meaningful option
# combinations without manually spelling out each one.

# Asserts that `x` (actual tibble) has the same column names in the same
# order as `expected` (a 0-row schema tibble), and that each column's
# base class (and, for factors, level list) matches. Produces exactly
# one testthat expectation: passes with no message when everything
# matches; fails with a consolidated diff when anything differs.
expect_schema_matches <- function(x, expected)
{
  act <- testthat::quasi_label(rlang::enquo(x), arg = "x")
  issues <- character()

  if (!identical(names(act$val), names(expected)))
    issues <- c(issues, sprintf(
      "column names / order differ — expected: [%s]; actual: [%s]",
      paste(names(expected), collapse = ", "),
      paste(names(act$val),  collapse = ", ")))

  for (col in intersect(names(expected), names(act$val))) {
    if (!identical(class(act$val[[col]]), class(expected[[col]])))
      issues <- c(issues, sprintf(
        "column `%s` class differs — expected: %s; actual: %s",
        col,
        paste(class(expected[[col]]), collapse = "/"),
        paste(class(act$val[[col]]),  collapse = "/")))
    # Factor-level comparison is skipped when `expected` has empty
    # levels — this is how the schema engine represents a
    # `levels_source = "data"` column (levels populated at read time by
    # the reader, not declared in the schema). For fixed-level factors,
    # the schema's compiled factor carries the declared levels and the
    # comparison fires.
    if (is.factor(expected[[col]]) && is.factor(act$val[[col]]) &&
        length(levels(expected[[col]])) > 0L &&
        !identical(levels(act$val[[col]]), levels(expected[[col]])))
      issues <- c(issues, sprintf(
        "column `%s` factor levels differ — expected: [%s]; actual: [%s]",
        col,
        paste(levels(expected[[col]]), collapse = ", "),
        paste(levels(act$val[[col]]),  collapse = ", ")))
  }

  testthat::expect_true(
    length(issues) == 0L,
    info = if (length(issues) > 0L) paste(issues, collapse = "\n") else NULL)

  invisible(act$val)
}

# Catalogue of valid values per option field — extended as new fields
# become test-relevant.
.schema_test_field_values <- list(
  include_world_bank_class = c("no", "pseudo", "full"),
  include_country          = c("no", "pseudo", "full"),
  include_hospital         = c("no", "pseudo", "full"),
  include_department       = c("no", "pseudo", "full"),
  include_user             = c("no", "pseudo", "full"),
  include_patient          = c("no", "pseudo", "full"),
  include_enrollment       = c("no", "pseudo", "full"),
  include_event            = c("no", "pseudo", "full"),
  include_timestamps       = c(FALSE, TRUE),
  include_test_data        = c(FALSE, TRUE)
)

# Build the cross-product of values for the named option fields and
# return a list of `dhis2_dataset_options` objects, one per combination.
# Fields not listed stay at their constructor defaults.
iter_dataset_options <- function(fields = NULL)
{
  if (is.null(fields) || length(fields) == 0L)
    return(list(dhis2_dataset_options()))

  unknown <- setdiff(fields, names(.schema_test_field_values))
  if (length(unknown) > 0L)
    rlang::abort(c(
      "Unknown option field(s) in iter_dataset_options():",
      "x" = paste(unknown, collapse = ", "),
      "i" = paste(
        "extend `.schema_test_field_values` in helper-schema.R if needed.")
    ))

  chosen <- .schema_test_field_values[fields]
  grid   <- do.call(expand.grid, c(chosen, stringsAsFactors = FALSE))

  purrr::map(seq_len(nrow(grid)), \(i)
    do.call(dhis2_dataset_options, as.list(grid[i, , drop = FALSE]))
  )
}
