#' Filter a NeoIPC dataset
#'
#' Returns a `neoipcr_ds` narrowed by the supplied filter parameters.
#' Filtering implementation is added incrementally in later releases;
#' until then the dataset is returned unchanged after the input type is
#' validated.
#'
#' @param ds A `neoipcr_ds` to filter.
#' @param world_bank_classes,countries,hospitals,departments Optional
#'   org-unit-level scope filters.
#' @param birth_weight_from,birth_weight_to,gestational_age_from,gestational_age_to
#'   Optional patient-level numeric range filters.
#' @param admission_date_from,admission_date_to,surveillance_end_date_from,surveillance_end_date_to
#'   Optional date range filters.
#' @param remove_patients_with_incomplete_data,remove_patients_with_invalid_data
#'   Whether to drop patients failing the completeness / validation checks.
#' @param validation_exceptions Optional list of validation rule ids to
#'   ignore when applying `remove_patients_with_invalid_data`.
#' @param remove_ineligible_patients Drop patients that don't meet the
#'   protocol's inclusion criteria.
#' @param remove_test_unit_data Drop data originating from test units.
#' @return A `neoipcr_ds`.
#' @export
filter_neoipc_ds <- function(
    ds,
    world_bank_classes = NULL,
    countries = NULL,
    hospitals = NULL,
    departments = NULL,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestational_age_from = NULL,
    gestational_age_to = NULL,
    admission_date_from = NULL,
    admission_date_to = NULL,
    surveillance_end_date_from = NULL,
    surveillance_end_date_to = NULL,
    remove_patients_with_incomplete_data = TRUE,
    remove_patients_with_invalid_data = TRUE,
    validation_exceptions = NULL,
    remove_ineligible_patients = TRUE,
    remove_test_unit_data = TRUE)
{
  check_neoipcr_ds(ds)
  ds
}
