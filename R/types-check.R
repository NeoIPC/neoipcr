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
      sprintf("%s() requires a neoipcr_ds with import options attached.",
              fn_name),
      "i" = "The dataset must have been imported via `import_dhis2()`.",
      "x" = "`x$metadata$dataset_options` is NULL."))

  violations <- character()
  for (opt_name in names(required)) {
    accepted <- required[[opt_name]]
    actual   <- opts[[opt_name]]
    ok <- !is.null(actual) && all(actual %in% accepted)
    if (!ok) {
      shown_actual <- if (is.null(actual)) "NULL" else
        paste0('"', paste(actual, collapse = '", "'), '"')
      shown_accepted <- paste(paste0('"', accepted, '"'), collapse = " / ")
      violations <- c(
        violations,
        sprintf("`%s` is %s; need one of %s.",
                opt_name, shown_actual, shown_accepted))
    }
  }

  if (length(violations) == 0L) return(invisible(NULL))

  rlang::abort(c(
    sprintf("%s() requires specific import options.", fn_name),
    rlang::set_names(violations, rep("x", length(violations))),
    "i" = "Re-import via `import_dhis2(dhis2_dataset_options(...))` with the required options set."))
}
