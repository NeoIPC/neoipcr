# The rules of this file read the patient record itself.

# Find patients recorded as part of a multiple birth with fewer than two
# infants at birth. The number counts every infant of the pregnancy, the
# patient included, so at a multiple birth it is two or more; the field
# admits any positive number and nothing refuses a one at entry. Without the
# flag the form does not ask for the number, and a value stored there is the
# client's to clear rather than the team's to correct; a flag without a
# number is a completeness matter this rule leaves alone.
validation_rule_56 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("multiple_birth", "siblings") %in% names(x$patients)))
    return(.rule_skipped(
      56L, "the patients' multiple-birth flag and number of infants at birth"))

  x$patients |>
    dplyr::select("patient_key", "multiple_birth", "siblings") |>
    dplyr::filter(dplyr::coalesce(.data$multiple_birth, FALSE) &
                  .data$siblings < 2L) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 56L),
      dplyr::join_by("patient_key")) |>
    tidyr::nest(context = "siblings") |>
    dplyr::mutate(
      rule_id        = 56L,
      patient_key    = .data$patient_key,
      enrollment_key = NA_integer_,
      event_key      = NA_integer_,
      context        = .data$context,
      .keep = "none")
}

# Find patients whose record holds neither the birth weight nor the
# gestational age (the total gestation days, the value the eligibility filter
# and the calculations read). Eligibility rests on one of the two, so without
# both it cannot be established. The registration form refuses such a record
# in every department, one of the two being compulsory wherever the other is
# empty, so a record without either reaches the dataset around the form:
# imported through the API, or registered before that requirement existed.
# The eligibility filter keeps these patients for this rule rather
# than remove them unreported. Not an eligibility rule: it finds a record
# that is incomplete whichever patients were requested.
validation_rule_57 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("birth_weight", "total_gestation_days") %in% names(x$patients)))
    return(.rule_skipped(
      57L, "the patients' birth weight and total gestation days"))

  x$patients |>
    dplyr::filter(is.na(.data$birth_weight) &
                  is.na(.data$total_gestation_days)) |>
    dplyr::select("patient_key") |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 57L),
      dplyr::join_by("patient_key")) |>
    dplyr::mutate(
      rule_id        = 57L,
      patient_key    = .data$patient_key,
      enrollment_key = NA_integer_,
      event_key      = NA_integer_,
      context        = list(NULL),
      .keep = "none")
}
