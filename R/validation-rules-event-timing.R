# Find sepsis events where the stored day of life does not match the value
# calculated from the enrolment date and the event date.
validation_rule_27 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 27L, .before = 1)

  return(r)
}

# Find sepsis events where the stored length of stay does not match the value
# calculated from the enrolment date and the event date.
validation_rule_28 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 28L, .before = 1)

  return(r)
}

# Find sepsis events where the event date is within the first 3 days of life
validation_rule_29 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::select("event_key","enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$sepsisData |>
          dplyr::select("event_key","dol"),
        dplyr::join_by("event_key")) |>
      dplyr::filter(.data$dol < 4) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","dol") |>
    dplyr::mutate(rule_id = 29L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("dol"))) |>
    dplyr::summarise(
      context = list(
        list(dol = .data$dol)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find sepsis events where the event date is within the first two days of
# hospitalisation of a referred or (re-)admitted patient
validation_rule_30 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 30L, .before = 1)

  return(r)
}

# Find pneumonia events where the stored day of life does not match the value
# calculated from the enrolment date and the event date.
validation_rule_31 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 31L, .before = 1)

  return(r)
}

# Find pneumonia events where the stored length of stay does not match the value
# calculated from the enrolment date and the event date.
validation_rule_32 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 32L, .before = 1)

  return(r)
}

# Find pneumonia events where the event date is within the first 3 days of life
validation_rule_33 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 33L, .before = 1)

  return(r)
}

# Find pneumonia events where the event date is within the first two days of
# hospitalisation of a referred or (re-)admitted patient
validation_rule_34 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 34L, .before = 1)

  return(r)
}

# Find NEC events where the stored day of life does not match the value
# calculated from the enrolment date and the event date.
validation_rule_35 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 35L, .before = 1)

  return(r)
}

# Find NEC events where the stored length of stay does not match the value
# calculated from the enrolment date and the event date.
validation_rule_36 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 36L, .before = 1)

  return(r)
}

# Find NEC events where the event date is within the first 3 days of life
validation_rule_37 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 37L, .before = 1)

  return(r)
}

# Find NEC events where the event date is within the first two days of
# hospitalisation of a referred or (re-)admitted patient
validation_rule_38 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if (nrow(x$necData) < 1) {
    return(NULL)
  }

  r <- x$enrollments |>
    dplyr::select(
      tidyselect::any_of(c("hospital_key","department_key","patient_key")),
      "enrollment_key","enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("event_key","enrollment_key","occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::semi_join(
      x$admissionData |>
        dplyr::filter(.data$type == 3) |>
        dplyr::select("event_key"),
      dplyr::join_by("event_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "nec") |>
        dplyr::select("event_key","enrollment_key","occurredAt"),
      dplyr::join_by("enrollment_key"),
      suffix = c(".adm",".nec"))

  if ("los" %in% names(x$necData))
    r <- r |>
      dplyr::inner_join(
        x$necData |>
          dplyr::select("event_key","los"),
        dplyr::join_by("event_key.nec" == "event_key")) |>
      dplyr::mutate(dos = .data$los + 1L, .keep = "unused")
  else
    r <- r |>
      dplyr::mutate(
        dos = as.integer(.data$occurredAt.nec - .data$occurredAt.adm) + 1L,
        .keep = "unused")

  r <- r |>
      dplyr::filter(.data$dos < 3) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key"="event_key.nec","dos") |>
    dplyr::mutate(rule_id = 38L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("dos"))) |>
    dplyr::summarise(
      context = list(
        list(dos = .data$dos)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find surgical procedure events where the stored day of life does not match the
# value calculated from the enrolment date and the event date.
validation_rule_39 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 39L, .before = 1)

  return(r)
}

# Find surgical procedure events where the stored length of stay does not match
# the value calculated from the enrolment date and the event date.
validation_rule_40 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 40L, .before = 1)

  return(r)
}

# Find SSI events where the stored day of life does not match the value
# calculated from the enrolment date and the event date.
validation_rule_41 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 41L, .before = 1)

  return(r)
}

# Find SSI events where the stored length of stay does not match the value
# calculated from the enrolment date and the event date.
validation_rule_42 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 42L, .before = 1)

  return(r)
}
