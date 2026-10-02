is_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds")
is_scalar_neoipcr_ds <- function(x) inherits(x, "neoipcr_ds") && rlang::is_scalar_list(x)
is_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt")
is_scalar_neoipcr_dhis2_conopt <- function(x) inherits(x, "neoipcr_dhis2_conopt") && rlang::is_scalar_list(x)
is_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt")
is_scalar_neoipcr_dhis2_dsopt <- function(x) inherits(x, "neoipcr_dhis2_dsopt") && rlang::is_scalar_list(x)
is_neoipcr_rep_ds <- function(x) inherits(x, "neoipcr_rep_ds")
is_scalar_neoipcr_rep_ds <- function(x) inherits(x, "neoipcr_rep_ds") && rlang::is_scalar_list(x)
is_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds")
is_scalar_neoipcr_ref_ds <- function(x) inherits(x, "neoipcr_ref_ds") && rlang::is_scalar_list(x)
is_neoipcr_bnch_ds <- function(x) inherits(x, "neoipcr_bnch_ds")
is_scalar_neoipcr_bnch_ds <- function(x) inherits(x, "neoipcr_bnch_ds") && rlang::is_scalar_list(x)

check_neoipcr_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_ds(x)) return(invisible(NULL))
    if (is_neoipcr_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_ds object")
}

check_neoipcr_dhis2_conopt <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_dhis2_conopt(x)) return(invisible(NULL))
    if (is_neoipcr_dhis2_conopt(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_dhis2_conopt object")
}

check_neoipcr_dhis2_dsopt <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_dhis2_dsopt(x)) return(invisible(NULL))
    if (is_neoipcr_dhis2_dsopt(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_dhis2_dsopt object")
}

check_neoipcr_rep_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_rep_ds(x)) return(invisible(NULL))
    if (is_neoipcr_rep_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_rep_ds object")
}

check_neoipcr_ds_or_rep_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && (is_scalar_neoipcr_ds(x) || is_scalar_neoipcr_rep_ds(x))) return(invisible(NULL))
    if (is_neoipcr_ds(x) || is_neoipcr_rep_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_ds or a neoipcr_rep_ds object")
}

check_neoipcr_ref_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_ref_ds(x)) return(invisible(NULL))
    if (is_neoipcr_ref_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_ref_ds object")
}

check_neoipcr_ds_or_ref_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && (is_scalar_neoipcr_ds(x) || is_scalar_neoipcr_ref_ds(x))) return(invisible(NULL))
    if (is_neoipcr_ds(x) || is_neoipcr_ref_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_ds or a neoipcr_ref_ds object")
}

check_neoipcr_bnch_ds <- function(x, require_scalar = TRUE, allow_null = FALSE) {
  if (!missing(x)) {
    if (require_scalar && is_scalar_neoipcr_bnch_ds(x)) return(invisible(NULL))
    if (is_neoipcr_bnch_ds(x)) return(invisible(NULL))
    if (allow_null && is_null(x)) return(invisible(NULL))
  }
  stop_input_type(x, "a neoipcr_bnch_ds object")
}

# Assert that the `dhis2_dataset_options` attached to a `neoipcr_ds`
# satisfies an exported function's implicit requirements.
#
# The three-valued gates (`include_patient` / `include_enrollment` /
# `include_event`, plus the pre-existing `include_country` /
# `include_hospital` / `include_department` / `include_world_bank_class`
# / `include_user`) let consumers opt out at any level of the hierarchy
# or link chain. Calc / table / benchmark functions typically need
# several of these non-`"no"` to build their denominators and joins.
# Before this helper a mis-configured import surfaced as a deep
# pipeline crash; this one aborts early with an actionable message
# that names every unmet requirement and tells the caller which option
# to change.
#
# `required` is a named list: names are option names on
# `dhis2_dataset_options()`; values are the accepted option values
# (the check is `actual %in% value`). Use `c("pseudo", "full")` for a
# non-`"no"` requirement on a three-valued gate, `TRUE` for a boolean
# opt-in flag, character vectors for `include_dhis2_ids` memberships.
#
# Example usage in an exported function:
#
#   calculate_department_data <- function(x, use_cache = TRUE) {
#     check_neoipcr_ds(x)
#     assert_options_for(x, required = list(
#       include_department = c("pseudo", "full"),
#       include_patient    = c("pseudo", "full"),
#       include_enrollment = c("pseudo", "full"),
#       include_event      = c("pseudo", "full")
#     ), fn_name = "calculate_department_data")
#     ...
#   }
assert_options_for <- function(x, required, fn_name) {
  opts <- x$metadata$dataset_options
  if (is.null(opts))
    rlang::abort(c(
      gettextf("%s() requires a %s with import options attached.",
               fn_name, "neoipcr_ds"),
      "i" = gettextf("The dataset must have been imported via %s.",
                     "`import_dhis2()`"),
      "x" = gettextf("%s is NULL.", "`x$metadata$dataset_options`")))

  violations <- character()
  for (opt_name in names(required)) {
    accepted <- required[[opt_name]]
    actual   <- opts[[opt_name]]
    ok <- !is.null(actual) && all(actual %in% accepted)
    if (!ok) {
      shown_actual <- if (is.null(actual)) "NULL" else
        paste(.shown_option_values(actual), collapse = ", ")
      shown_accepted <- paste(.shown_option_values(accepted), collapse = " / ")
      violations <- c(
        violations,
        gettextf("`%s` is %s; need one of %s.",
                 opt_name, shown_actual, shown_accepted))
    }
  }

  if (length(violations) == 0L) return(invisible(NULL))

  rlang::abort(c(
    gettextf("%s() requires specific import options.", fn_name),
    rlang::set_names(violations, rep("x", length(violations))),
    "i" = gettextf("Re-import via %s with the required options set.",
                   "`import_dhis2(dhis2_dataset_options(...))`")))
}

# Option values as `assert_options_for()` shows them: a string in double
# quotes, any other value as code, so that the flag `TRUE` does not read as
# the string "TRUE".
.shown_option_values <- function(values)
  if (is.character(values)) paste0('"', values, '"') else paste0("`", values, "`")

# Assert that a dataset carries its validation summary. A calculated dataset
# documents `validationSummary` as the summary of the dataset it was built
# from; a dataset saved before the slot existed has none, and the
# calculation refuses it rather than emitting `NULL` where a summary is
# promised. `fn_name` names the caller in the message, as above.
assert_validation_summary <- function(x, fn_name) {
  if (is.null(x$validationSummary))
    rlang::abort(c(
      gettextf("%s() needs a dataset that carries its validation summary.", fn_name),
      "x" = gettextf("%s is NULL.", "`x$validationSummary`"),
      "i" = gettextf("Import the dataset again with this version of neoipcr; see %s.",
                     "`?import_dhis2`")))
  invisible(x)
}

# Assert that a dataset's reconciliation summary, where it has one, is the
# slot the import writes: its schema's columns, or 0×0 where the import
# reconciled nothing. `fn_name` names the caller in the message, as above.
#
# NEOIPC-PERMANENT(dataset-format): never refuse a dataset without the slot.
# A raw dataset is kept as a backup, and one written before the slot existed
# carries none; a file on disk outlives the code that wrote it, so no
# condition retires this, and the calculation carries `NULL` for it.
assert_reconciliation_summary <- function(x, fn_name) {
  summary <- x$reconciliationSummary
  if (is.null(summary))
    return(invisible(x))
  declared <- purrr::map_chr(reconciliationSummary_cols, "name")
  if (!is.data.frame(summary) ||
      !(ncol(summary) == 0L || identical(names(summary), declared)))
    rlang::abort(c(
      gettextf("%s() needs a dataset whose reconciliation summary is the one an import writes.", fn_name),
      "x" = if (is.data.frame(summary))
        sprintf(ngettext(ncol(summary), "%s has the column %s.", "%s has the columns %s."),
                "`x$reconciliationSummary`",
                paste0("`", names(summary), "`", collapse = ", "))
      else gettextf("%s is not a data frame.", "`x$reconciliationSummary`"),
      "i" = gettextf(
        "An import writes %s, or no column where it reconciled nothing; see %s.",
        paste0("`", declared, "`", collapse = ", "), "`?import_dhis2`")),
      class = "neoipcr_malformed_reconciliation_summary")
  invisible(x)
}
