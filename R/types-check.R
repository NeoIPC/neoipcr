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
