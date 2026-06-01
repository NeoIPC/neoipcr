# Find patient records without enrollment.
validation_rule_1 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$patients |>
    dplyr::select(
      tidyselect::any_of(c("hospital_key", "department_key")),
      "patient_key") |>
    dplyr::anti_join(
      x$enrollments,
      dplyr::join_by("patient_key")) |>
    dplyr::select(
      tidyselect::any_of(
        c("hospital_key",
          "department_key",
          "patient_key"))) |>
    dplyr::mutate(rule_id = 1L, .before = 1)

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","patient_key"))

  return(r)
}

# Find enrollments where the surveillance end event is completed but the
# admission event is still open.
validation_rule_2 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 2),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
    dplyr::select(
      tidyselect::any_of(c("hospital_key", "department_key")),
      "patient_key",
      "enrollment_key",
      "enrollment_status" = "status") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select(
          "enrollment_key",
          "event_key",
          "event_status" = "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$enrollment_status == "ACTIVE" & .data$event_status == "COMPLETED") |>
    dplyr::select(
      tidyselect::any_of(
        c("hospital_key",
          "department_key",
          "patient_key",
          "enrollment_key",
          "event_key"))) |>
    dplyr::mutate(rule_id = 2L, .before = 1)

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

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
          occurredAt = .data$occurredAt)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find enrolments where the surveillance end date is before the admission date.
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
          endOccurredAt = .data$endOccurredAt)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find incomplete admission events
validation_rule_5 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 5L),
      gettext("The dataset must contain the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 5L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where the surveillance end event is incomplete.
validation_rule_6 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 6L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 6L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where a sepsis event is incomplete.
validation_rule_7 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 7L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "bsi") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 7L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where a NEC event is incomplete.
validation_rule_8 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 8L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "nec") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 8L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where a pneumonia event is incomplete.
validation_rule_9 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 9L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "hap") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 9L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where a surgical procedure event is incomplete.
validation_rule_10 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 10L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "pro") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 10L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrolments where a SSI event is incomplete.
validation_rule_11 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$enrollments) || !"status" %in% names(x$events))
  {
    rlang::warn(paste(
      gettextf("Validation rule %i failed to execute.", 11L),
      gettext("The dataset must contain the enrolment status and the event status to execute this rule.")))
    return()
  }

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrollment_status" = "status") |>
      dplyr::inner_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "ssi") |>
          dplyr::select("enrollment_key","status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "COMPLETED" &
                      .data$status != "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","status") |>
    dplyr::mutate(rule_id = 11L, .before = 1) |>
    dplyr::group_by(dplyr::across(!"status")) |>
    dplyr::summarise(
      context = list(
        list(
          status = .data$status)))

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
          occurredAt.bsi = .data$occurredAt.bsi)))

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
          occurredAt.nec = .data$occurredAt.nec)))

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
          occurredAt.hap = .data$occurredAt.hap)))

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
          occurredAt.pro = .data$occurredAt.pro)))

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
          occurredAt.ssi = .data$occurredAt.ssi)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find overlapping enrolments (an enrolment's enrolment date or surveillance end
# date is in the interval between the enrolment data and the surveillance end
# date of another enrolment).
validation_rule_17 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  intervals <- x$enrollments |>
    dplyr::select(
      tidyselect::any_of(
        c("hospital_key","department_key")),
      "patient_key","enrollment_key","enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key","event_key","occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(
      surveillanceInterval = lubridate::interval(
        .data$enrolledAt,
        .data$occurredAt),
      .keep = "unused")

  r <- intervals |>
      dplyr::inner_join(
        intervals,
        dplyr::join_by("patient_key"),
        relationship = "many-to-many") |>
      dplyr::filter(
        .data$enrollment_key.x != .data$enrollment_key.y &
          (lubridate::int_overlaps(
            .data$surveillanceInterval.x,
            .data$surveillanceInterval.y) |
             lubridate::int_start(
               .data$surveillanceInterval.x) ==
             lubridate::int_start(
               .data$surveillanceInterval.y))) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key.x",
            "department_key.x"
            )),"patient_key","enrollment_key.x","enrollment_key.y","surveillanceInterval.x","surveillanceInterval.y") |>
    dplyr::mutate(rule_id = 17L, .before = 1) |>
    dplyr::rename_with(
      ~ stringr::str_extract(.x,"^[^\\.]*"),
      !tidyselect::any_of(
        c("surveillanceInterval.x","surveillanceInterval.y","enrollment_key.y"))) |>
    dplyr::group_by(dplyr::across(!c("surveillanceInterval.x","surveillanceInterval.y","enrollment_key.y"))) |>
    dplyr::summarise(
      context = list(
        list(
          surveillanceInterval.x = .data$surveillanceInterval.x,
          surveillanceInterval.y = .data$surveillanceInterval.y,
          enrollment_key.y = .data$enrollment_key.y)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find surveillance end events where the stored number of patient days does not
# match the value calculated from the enrolment date and the event date.
validation_rule_18 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
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
        "patient_days","patient_days_calculated") |>
    dplyr::mutate(rule_id = 18L, .before = 1) |>
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

# Find surgical site infections where the event date is not within the follow-up
# period of a previous surgical procedure event.
validation_rule_19 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 19L, .before = 1)

  return(r)
}

# Find infection events where the unknown pathogen is recorded.
validation_rule_20 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 20L, .before = 1)

  return(r)
}

# Find surveillance end events where the sum of all individual antibiotic
# substance days is less than the number of antibiotic days.
validation_rule_21 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1) |>
    dplyr::mutate(rule_id = 21L, .before = 1)

  return(r)
}

# Find surgical procedure events with a main procedure code that is not a valid
# ICHE procedure code.
validation_rule_22 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if (nrow(x$surgeryData) < 1)
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 22L, .before = 1))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 22L, .before = 1))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::select("event_key","enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$surgeryData |>
          dplyr::select("event_key","procedure_description",
                        "main_procedure_code"),
        dplyr::join_by("event_key")) |>
      dplyr::filter(!(.data$main_procedure_code %in% valid_iche_codes)) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","procedure_description","main_procedure_code") |>
    dplyr::mutate(rule_id = 22L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("procedure_description","main_procedure_code"))) |>
    dplyr::summarise(
      context = list(
        list(
          procedure_description = .data$procedure_description,
          procedure_code = .data$main_procedure_code)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find surgical procedure events with a first side procedure code that is not a
# valid ICHE procedure code.
validation_rule_23 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if (nrow(x$surgeryData) < 1)
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 23L, .before = 1))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 23L, .before = 1))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::select("event_key","enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$surgeryData |>
          dplyr::filter(!is.na(.data$side_procedure_code_1)) |>
          dplyr::select("event_key","procedure_description",
                        "side_procedure_code_1"),
        dplyr::join_by("event_key")) |>
      dplyr::filter(!(.data$side_procedure_code_1 %in% valid_iche_codes)) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","procedure_description","side_procedure_code_1") |>
    dplyr::mutate(rule_id = 23L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("procedure_description","side_procedure_code_1"))) |>
    dplyr::summarise(
      context = list(
        list(
          procedure_description = .data$procedure_description,
          procedure_code = .data$side_procedure_code_1)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find surgical procedure events with a second side procedure code that is not a
# valid ICHE procedure code.
validation_rule_24 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if (!('side_procedure_code_2' %in% names(x$surgeryData))) {
    return(NULL)
  }

  if (nrow(x$surgeryData) < 1)
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 24L, .before = 1))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1) |>
        dplyr::mutate(rule_id = 24L, .before = 1))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- x$enrollments |>
      dplyr::select(
        tidyselect::any_of(c("hospital_key","department_key","patient_key")),
        "enrollment_key","enrolledAt") |>
      dplyr::inner_join(
        x$events |>
          dplyr::select("event_key","enrollment_key","occurredAt"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::inner_join(
        x$surgeryData |>
          dplyr::filter(!is.na(.data$side_procedure_code_2)) |>
          dplyr::select("event_key","procedure_description",
                        "side_procedure_code_2"),
        dplyr::join_by("event_key")) |>
      dplyr::filter(!(.data$side_procedure_code_2 %in% valid_iche_codes)) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key","event_key","procedure_description","side_procedure_code_2") |>
    dplyr::mutate(rule_id = 24L, .before = 1) |>
    dplyr::group_by(dplyr::across(!c("procedure_description","side_procedure_code_2"))) |>
    dplyr::summarise(
      context = list(
        list(
          procedure_description = .data$procedure_description,
          procedure_code = .data$side_procedure_code_2)))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","event_key"))

  return(r)
}

# Find completed enrollments without surveillance end event.
validation_rule_25 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::filter(
        dplyr::if_all(
          tidyselect::any_of("status"),
          ~ .x == "COMPLETED")) |>
      dplyr::anti_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "end"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key") |>
    dplyr::mutate(rule_id = 25L, .before = 1)

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find completed enrollments without admission event.
validation_rule_26 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- x$enrollments |>
      dplyr::filter(
        dplyr::if_all(
          tidyselect::any_of("status"),
          ~ .x == "COMPLETED")) |>
      dplyr::anti_join(
        x$events |>
          dplyr::filter(.data$event_type_key == "adm"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key")),"enrollment_key") |>
    dplyr::mutate(rule_id = 26L, .before = 1)

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

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
        list(dol = .data$dol)))

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
        list(dos = .data$dos)))

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

validation_rules <- list(
  list(
    id = 1L,
    fun = validation_rule_1,
    formatter = function(x) {
      gettext("The patient record does not have an enrolment.")
    }
  ),
  list(
    id = 2L,
    fun = validation_rule_2,
    formatter = function(x) {
      gettext(
        "The patient record has a completed surveillance end form but the enrolment is still active.")
    }
  ),
  list(
    id = 3L,
    fun = validation_rule_3,
    formatter = function(x) {
      gettextf(
        "The admission date in the admission form (%s) differs from the admission date in the enrolment (%s).",
        format(x$occurredAt, format = "%x"),
        format(x$enrolledAt, format = "%x"))
    }
  ),
  list(
    id = 4L,
    fun = validation_rule_4,
    formatter = function(x) {
      gettextf(
        "The date of the end of the surveillance (%s) is earlier than the date of admission on the admission form (%s).",
        format(x$endOccurredAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"))
    }
  ),
  list(
    id = 5L,
    fun = validation_rule_5,
    formatter = function(x) {
      gettextf(
        "The patient record's admission form is not completed (status is '%s').",
        as.character(x$status))
    }
  ),
  list(
    id = 6L,
    fun = validation_rule_6,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment but the surveillance end form is not completed (status is '%s').",
        as.character(x$status))
    }
  ),
  list(
    id = 7L,
    fun = validation_rule_7,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a sepsis form is not completed (enrolment status is '%s', surveillance end form status is '%s', sepsis form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$bsi_status)
      )
    }
  ),
  list(
    id = 8L,
    fun = validation_rule_8,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a necrotizing enterocolitis form is not completed (enrolment status is '%s', surveillance end form status is '%s', necrotizing enterocolitis form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$nec_status)
      )
    }
  ),
  list(
    id = 9L,
    fun = validation_rule_9,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a pneumonia form is not completed (enrolment status is '%s', surveillance end form status is '%s', pneumonia form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$hap_status)
      )
    }
  ),
  list(
    id = 10L,
    fun = validation_rule_10,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a surgical procedure form is not completed (enrolment status is '%s', surveillance end form status is '%s', surgical procedure form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$pro_status)
      )
    }
  ),
  list(
    id = 11L,
    fun = validation_rule_11,
    formatter = function(x) {
      gettextf(
        "The patient record has a completed enrolment or surveillance end form but a surgical site infection form is not completed (enrolment status is '%s', surveillance end form status is '%s', surgical site infection form status is '%s').",
        as.character(x$enrollment_status),
        as.character(x$end_status),
        as.character(x$ssi_status)
      )
    }
  ),
  list(
    id = 12L,
    fun = validation_rule_12,
    formatter = function(x) {
      gettextf(
        "The patient record contains a sepsis form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', sepsis date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$bsiOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 13L,
    fun = validation_rule_13,
    formatter = function(x) {
      gettextf(
        "The patient record contains a necrotizing enterocolitis form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', necrotizing enterocolitis date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$necOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 14L,
    fun = validation_rule_14,
    formatter = function(x) {
      gettextf(
        "The patient record contains a pneumonia form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', pneumonia date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$hapOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 15L,
    fun = validation_rule_15,
    formatter = function(x) {
      gettextf(
        "The patient record contains a surgical procedure form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', surgical procedure date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$surOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 16L,
    fun = validation_rule_16,
    formatter = function(x) {
      gettextf(
        "The patient record contains a surgical site infection form with an infection date that is not within the time frame of a documented enrolment (admission date in the enrolment '%s', admission date in the admission form is '%s', surveillance end date is '%s', surgical site infection date is '%s').",
        format(x$enrolledAt, format = "%x"),
        format(x$admOccurredAt, format = "%x"),
        format(x$endOccurredAt, format = "%x"),
        format(x$ssiOccurredAt, format = "%x")
      )
    }
  ),
  list(
    id = 17L,
    fun = validation_rule_17,
    formatter = function(x) {
      gettextf(
        "The patient record contains an enrolment with a time interval that overlaps with that of another enrolment (this enrolment has an interval from %s to %s and the other enrolment has an interval from %s to %s).",
        format(x$admOccurredAt_1, format = "%x"),
        format(x$endOccurredAt_1, format = "%x"),
        format(x$admOccurredAt_2, format = "%x"),
        format(x$endOccurredAt_2, format = "%x")
      )
    }
  ),
  list(
    id = 18L,
    fun = validation_rule_18,
    formatter = function(x) {
      gettextf(
        "The number of patient days (%s) does not match the calculated value (%s).",
        as.character(x$patient_days),
        as.character(x$patient_days_calculated)
      )
    }
  ),
  list(
    id = 19L,
    fun = validation_rule_19,
    formatter = function(x) {
      inf_type_string <- switch(
        x$ssi_infection_type,
        gettext("superficial incisional SSI"),
        gettext("deep incisional SSI"),
        gettext("organ/space SSI"))
      gettextf(
        "The surgical site infection (%s) did not occur during the follow-up period of a recorded surgical procedure.",
        inf_type_string
      )
    }
  ),
  list(
    id = 20L,
    fun = validation_rule_20,
    formatter = function(x) {
      sec_bsi_part <- ""
      if(x$is_secondary_bsi)
        sec_bsi_part <- gettext(" causing secondary sepsis", trim = FALSE)

      gettextf(
        "The pathogen manually entered as pathogen %i%s ('%s') cannot be assigned.",
        x$pathogen_index,
        as.character(sec_bsi_part),
        as.character(x$pathogen_name)
      )
    }
  ),
  list(
    id = 21L,
    fun = validation_rule_21,
    formatter = function(x) {
      gettextf(
        "The sum of all antibiotic substance days (%i) is less than the total number of antibiotic days (%i).",
        x$ab_substance_days,
        x$ab_days
      )
    }
  ),
  list(
    id = 22L,
    fun = validation_rule_22,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the main procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 23L,
    fun = validation_rule_23,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the first side procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 24L,
    fun = validation_rule_24,
    formatter = function(x) {
      gettextf(
        "The surgical procedure ('%s') has an invalid ICHE code ('%s') as the second side procedure code.",
        as.character(x$procedure_description),
        as.character(x$procedure_code)
      )
    }
  ),
  list(
    id = 25L,
    fun = validation_rule_25,
    formatter = function(x) {
      gettext(
        "The patient record has a completed enrolment but no surveillance end form."
      )
    }
  ),
  list(
    id = 26L,
    fun = validation_rule_26,
    formatter = function(x) {
      gettext(
        "The patient record has a completed enrolment but no admission form."
      )
    }
  ),
  list(
    id = 27L,
    fun = validation_rule_27,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the sepsis form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 28L,
    fun = validation_rule_28,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the sepsis form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 29L,
    fun = validation_rule_29,
    formatter = function(x) {
      gettextf(
        "The sepsis occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 30L,
    fun = validation_rule_30,
    formatter = function(x) {
      gettextf(
        "The sepsis occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 31L,
    fun = validation_rule_31,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the pneumonia form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 32L,
    fun = validation_rule_32,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the pneumonia form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 33L,
    fun = validation_rule_33,
    formatter = function(x) {
      gettextf(
        "The pneumonia occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 34L,
    fun = validation_rule_34,
    formatter = function(x) {
      gettextf(
        "The pneumonia occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 35L,
    fun = validation_rule_35,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the necrotising enterocolitis form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 36L,
    fun = validation_rule_36,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the necrotising enterocolitis form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 37L,
    fun = validation_rule_37,
    formatter = function(x) {
      gettextf(
        "The necrotising enterocolitis occurred within the first 3 days of life (day of life is %i).",
        x$dol
      )
    }
  ),
  list(
    id = 38L,
    fun = validation_rule_38,
    formatter = function(x) {
      gettextf(
        "The necrotising enterocolitis occurred within the first two days of hospitalisation of a referred or (re-)admitted patient (day of hospitalisation is %i).",
        x$dos
      )
    }
  ),
  list(
    id = 39L,
    fun = validation_rule_39,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the surgical procedure form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 40L,
    fun = validation_rule_40,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the surgical procedure form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  ),
  list(
    id = 41L,
    fun = validation_rule_41,
    formatter = function(x) {
      gettextf(
        "The day of life stored in the surgical site infection form (%i) does not match the calculated value (%i).",
        x$dol,
        x$dol_calc
      )
    }
  ),
  list(
    id = 42L,
    fun = validation_rule_42,
    formatter = function(x) {
      gettextf(
        "The day of occurrence after admission stored in the surgical site infection form (%i) does not match the calculated value (%i).",
        x$los,
        x$los_calc
      )
    }
  )
)

validate <- function(x, rules = NULL, exceptions = NULL)
{
  check_neoipcr_ds(x)

  r <- validation_rules |>
    lapply(\(r)if(is.null(rules)||r$id%in%rules)r$fun(x,exceptions)) |>
    dplyr::bind_rows() |>
    dplyr::select(
      tidyselect::any_of(
        c("rule_id","patient_key","enrollment_key","event_key","context")))

  invisible(r)
}
