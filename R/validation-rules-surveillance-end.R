# Find surveillance end events where the stored number of patient days does not
# match the value calculated from the enrolment date and the event date.
validation_rule_18 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(18L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::inner_join(
            x$surveillanceEndData |>
              dplyr::select("event_key", "patient_days"),
            dplyr::join_by("event_key")) |>
          dplyr::select("enrollment_key","occurredAt","patient_days"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::mutate(
        patient_days_calculated = 1L + as.integer(.data$occurredAt - .data$enrolledAt)
      ) |>
      dplyr::filter(.data$patient_days_calculated != .data$patient_days) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","enrolledAt","occurredAt",
        "patient_days","patient_days_calculated")) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt","patient_days","patient_days_calculated"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt = .data$occurredAt,
          patient_days = .data$patient_days,
          patient_days_calculated = .data$patient_days_calculated)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find surveillance end events where the sum of all individual antibiotic
# substance days is less than the number of antibiotic days.
validation_rule_21 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- dplyr::bind_cols(
    rule_id = c(21L),
    x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1))

  return(r)
}
