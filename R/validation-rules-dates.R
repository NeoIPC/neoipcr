# Find admission events where the event date differs from the enrolment date.
validation_rule_3 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrolledAt != .data$occurredAt) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","enrolledAt","occurredAt") |>
    dplyr::mutate(rule_id = 3L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt = .data$occurredAt)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find enrolments where the admission date and the surveillance end date are the
# same.
validation_rule_4 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","admOccurredAt"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","endOccurredAt"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$endOccurredAt < .data$admOccurredAt) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","admOccurredAt","endOccurredAt") |>
    dplyr::mutate(rule_id = 4L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("admOccurredAt","endOccurredAt"))) |>
    dplyr::summarise(
      context = list(
        list(
          admOccurredAt = .data$admOccurredAt,
          endOccurredAt = .data$endOccurredAt)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find sepsis events whose event date is not between the enrollment date and the
# event date of the surveillance end event.
validation_rule_12 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key"),
        suffix = c(".adm",".end")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "bsi") |>
          dplyr::select("event_key","enrollment_key","occurredAt.bsi"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$occurredAt.bsi < .data$enrolledAt |
                      .data$occurredAt.bsi < .data$occurredAt.adm |
                      .data$occurredAt.bsi > .data$occurredAt.end) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.bsi") |>
    dplyr::mutate(rule_id = 12L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.bsi"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt.adm = .data$occurredAt.adm,
          occurredAt.end = .data$occurredAt.end,
          occurredAt.bsi = .data$occurredAt.bsi)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find NEC events whose event date is not between the enrollment date and the
# event date of the surveillance end event.
validation_rule_13 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key"),
        suffix = c(".adm",".end")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "nec") |>
          dplyr::select("event_key","enrollment_key","occurredAt.nec"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$occurredAt.nec < .data$enrolledAt |
                      .data$occurredAt.nec < .data$occurredAt.adm |
                      .data$occurredAt.nec > .data$occurredAt.end) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.nec") |>
    dplyr::mutate(rule_id = 13L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.nec"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt.adm = .data$occurredAt.adm,
          occurredAt.end = .data$occurredAt.end,
          occurredAt.nec = .data$occurredAt.nec)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find pneumonia events whose event date is not between the enrollment date and
# the event date of the surveillance end event.
validation_rule_14 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key"),
        suffix = c(".adm",".end")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "hap") |>
          dplyr::select("event_key","enrollment_key","occurredAt.hap"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$occurredAt.hap < .data$enrolledAt |
                      .data$occurredAt.hap < .data$occurredAt.adm |
                      .data$occurredAt.hap > .data$occurredAt.end) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.hap") |>
    dplyr::mutate(rule_id = 14L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.hap"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt.adm = .data$occurredAt.adm,
          occurredAt.end = .data$occurredAt.end,
          occurredAt.hap = .data$occurredAt.hap)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find surgical procedure events whose event date is not between the enrollment
# date and the event date of the surveillance end event.
validation_rule_15 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key"),
        suffix = c(".adm",".end")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "pro") |>
          dplyr::select("event_key","enrollment_key","occurredAt.pro"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$occurredAt.pro < .data$enrolledAt |
                      .data$occurredAt.pro < .data$occurredAt.adm |
                      .data$occurredAt.pro > .data$occurredAt.end) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.pro") |>
    dplyr::mutate(rule_id = 15L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.pro"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt.adm = .data$occurredAt.adm,
          occurredAt.end = .data$occurredAt.end,
          occurredAt.pro = .data$occurredAt.pro)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find SSI events whose event date is not between the enrollment date and the
# event date of the surveillance end event.
validation_rule_16 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key"),
        suffix = c(".adm",".end")) |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "ssi") |>
          dplyr::select("event_key","enrollment_key","occurredAt.ssi"="occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$occurredAt.ssi < .data$enrolledAt |
                      .data$occurredAt.ssi < .data$occurredAt.adm |
                      .data$occurredAt.ssi > .data$occurredAt.end) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.ssi") |>
    dplyr::mutate(rule_id = 16L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("enrolledAt","occurredAt.adm","occurredAt.end","occurredAt.ssi"))) |>
    dplyr::summarise(
      context = list(
        list(
          enrolledAt = .data$enrolledAt,
          occurredAt.adm = .data$occurredAt.adm,
          occurredAt.end = .data$occurredAt.end,
          occurredAt.ssi = .data$occurredAt.ssi)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}
