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

}
