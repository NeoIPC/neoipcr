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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

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
          status = .data$status)),
      .groups = "drop")

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}
