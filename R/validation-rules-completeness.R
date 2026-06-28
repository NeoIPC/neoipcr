# Find incomplete admission events
validation_rule_5 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if(!"status" %in% names(x$events))
  {
    logger::log_debug(
      "Validation rule 5 skipped: dataset lacks the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(5L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 6 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(6L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 7 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(7L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 8 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(8L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 9 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(9L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 10 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(10L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
    logger::log_debug(
      "Validation rule 11 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(11L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","status")) |>
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
