# Find patient records without enrollment.
validation_rule_1 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(1L),
    .with_hierarchy_context(x$patients, x$metadata$departments) |>
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
            "patient_key"))))

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
    logger::log_debug(
      "Validation rule 2 skipped: dataset lacks the enrolment status and the event status.",
      namespace = "neoipcr")
    return()
  }

  r <- dplyr::bind_cols(
    rule_id = c(2L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "event_status" = "status"),
        dplyr::join_by("enrollment_key")) |>
      dplyr::filter(.data$enrollment_status == "ACTIVE" & .data$event_status == "COMPLETED") |>
      dplyr::select(
        tidyselect::any_of(
          c("hospital_key",
            "department_key",
            "patient_key",
            "enrollment_key"))))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}

# Find overlapping enrolments (an enrolment's enrolment date or surveillance end
# date is in the interval between the enrolment data and the surveillance end
# date of another enrolment).
validation_rule_17 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  intervals <- .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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

  r <- dplyr::bind_cols(
    rule_id = c(17L),
    intervals |>
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
            )),"patient_key","enrollment_key.x","enrollment_key.y","surveillanceInterval.x","surveillanceInterval.y")) |>
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

# Find completed enrollments without surveillance end event.
validation_rule_25 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  r <- dplyr::bind_cols(
    rule_id = c(25L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key"))

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

  r <- dplyr::bind_cols(
    rule_id = c(26L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key"))

  if(!is.null(exceptions))
    r <- r |>
    dplyr::anti_join(
      exceptions,
      dplyr::join_by("rule_id","enrollment_key"))

  return(r)
}
