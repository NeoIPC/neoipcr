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

# Whether a text value is recorded: an empty text records nothing.
.text_recorded <- function(x)
  !is.na(x) & nzchar(x)

# Find patients whose record holds neither the birth weight nor the
# gestational age in either of its forms: the total gestation days, the
# value the eligibility filter and the calculations read, and the text in
# weeks and days the registration form takes. Eligibility rests on one of the
# two criteria, so without both it cannot be established. The registration
# form refuses such a record in every department, one of the two being
# compulsory wherever the other is empty, so a record without either reaches
# the dataset around the form: imported through the API, or registered
# before that requirement existed. A text, even one in the wrong format, is a
# recorded gestational age: the wrong format is rule 58's finding. The
# eligibility filter keeps these patients for this rule rather than remove
# them unreported. Not an eligibility rule: it finds a record that is
# incomplete whichever patients were requested.
validation_rule_57 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("birth_weight", "total_gestation_days", "gest_age") %in%
           names(x$patients)))
    return(.rule_skipped(
      57L, "the patients' birth weight, total gestation days, and gestational-age text"))

  x$patients |>
    dplyr::filter(is.na(.data$birth_weight) &
                  is.na(.data$total_gestation_days) &
                  !.text_recorded(.data$gest_age)) |>
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

# The format the registration form holds the gestational-age text to: two
# digits for the completed weeks, 20 to 49, a plus sign, and one digit for
# the days, 0 to 6 (`NEOIPC_PATIENT_GA_FORMAT_VR`). The pattern is the
# client's own definition of the format, and no parser exists for it, so it
# is matched as a regular expression. `\A` and `\z` anchor it to the whole
# text, as the client's `d2:validatePattern()` requires the match to be the
# whole value; the `$` of stringr's regular-expression engine, the ICU
# library (International Components for Unicode), would also match before a
# trailing line break, which the client refuses.
.gestational_age_text_pattern <- "\\A[2-4][0-9][+][0-6]\\z"

# Find patients whose gestational-age text is recorded in a format other than
# the one the registration form requires. The form refuses such a text, so
# one reaches the dataset around the form: imported through the API, or
# registered before the check existed. An empty text is no text. Not an
# eligibility rule.
validation_rule_58 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"gest_age" %in% names(x$patients))
    return(.rule_skipped(58L, "the patients' gestational-age text"))

  x$patients |>
    dplyr::select("patient_key", "gest_age") |>
    dplyr::filter(
      .text_recorded(.data$gest_age) &
      !stringr::str_detect(.data$gest_age, .gestational_age_text_pattern)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 58L),
      dplyr::join_by("patient_key")) |>
    tidyr::nest(context = "gest_age") |>
    dplyr::mutate(
      rule_id        = 58L,
      patient_key    = .data$patient_key,
      enrollment_key = NA_integer_,
      event_key      = NA_integer_,
      context        = .data$context,
      .keep = "none")
}
