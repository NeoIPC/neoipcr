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
