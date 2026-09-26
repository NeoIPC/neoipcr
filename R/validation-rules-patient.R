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
