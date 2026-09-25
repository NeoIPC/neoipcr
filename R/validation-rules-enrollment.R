# Find patient records without an enrolment.
validation_rule_1 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  x$patients |>
    dplyr::select("patient_key") |>
    dplyr::anti_join(
      x$enrollments,
      dplyr::join_by("patient_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 1L),
      dplyr::join_by("patient_key")) |>
    dplyr::mutate(
      rule_id        = 1L,
      patient_key    = .data$patient_key,
      enrollment_key = NA_integer_,
      event_key      = NA_integer_,
      context        = list(NULL),
      .keep = "none")
}

# Find active enrolments whose surveillance-end event is completed.
validation_rule_2 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "ACTIVE") |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      .with_status(x$events, .event_status_levels) |>
        dplyr::filter(.data$event_type_key == "end" &
                      .data$status == "COMPLETED") |>
        dplyr::select("enrollment_key", "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 2L),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(
      rule_id        = 2L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = list(NULL),
      .keep = "none")
}

# Find enrolments of one patient whose surveillance periods overlap. A
# period runs from the enrolment date to the surveillance-end event, both
# days included. An enrolment without a surveillance-end event, or whose
# end event carries no date, is known to be under surveillance on its
# enrolment date only, so that day is its period: it is found when the day
# falls inside another enrolment's period, and two such enrolments are found
# when they share the day.
validation_rule_17 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  intervals <- x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::left_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "endOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(
      surveillanceInterval = lubridate::interval(
        .data$enrolledAt,
        dplyr::coalesce(.data$endOccurredAt, .data$enrolledAt)))

  intervals |>
    dplyr::inner_join(
      intervals,
      dplyr::join_by("patient_key"),
      relationship = "many-to-many",
      suffix = c("_this", "_other")) |>
    dplyr::filter(
      .data$enrollment_key_this != .data$enrollment_key_other &
        lubridate::int_overlaps(
          .data$surveillanceInterval_this,
          .data$surveillanceInterval_other)) |>
    dplyr::select(!c("surveillanceInterval_this", "surveillanceInterval_other")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 17L),
      dplyr::join_by("enrollment_key_this" == "enrollment_key")) |>
    tidyr::nest(context = c(
      "enrolledAt_this", "endOccurredAt_this",
      "enrolledAt_other", "endOccurredAt_other")) |>
    dplyr::mutate(
      rule_id        = 17L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key_this,
      event_key      = NA_integer_,
      context        = .data$context,
      .keep = "none")
}

# Find completed enrolments without a surveillance-end event.
validation_rule_25 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "COMPLETED") |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::anti_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 25L),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(
      rule_id        = 25L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = NA_integer_,
      context        = list(NULL),
      .keep = "none")
}

# Find completed enrolments without an admission event.
validation_rule_26 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "COMPLETED") |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::anti_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 26L),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(
      rule_id        = 26L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = NA_integer_,
      context        = list(NULL),
      .keep = "none")
}

# Find enrolments still active more than `.open_enrolment_max_days` after
# their enrolment date, measured against `as_of`, that have no
# surveillance-end event. Skips itself on a dataset that carries the
# enrolments' status but not the events', where an end form that is not
# completed is absent like a missing one.
validation_rule_43 <- function(x, exceptions, as_of)
{
  check_neoipcr_ds(x)

  # An import that requested active enrolments but only completed events
  # left out an end form that is not completed, so on such a dataset the
  # form is absent whether it is missing or merely open — this rule's
  # finding or rule 44's, which the dataset cannot tell apart. The status
  # columns say what was requested: the enrolments carrying one and the
  # events none is that shape.
  if ("status" %in% names(x$enrollments) && !("status" %in% names(x$events)))
    return(.rule_skipped(
      43L, "the events' status column, so an end form that is not completed is absent from it like a missing one"))
  # The import's surveillance-end date filter drops the end forms dated
  # outside its window and keeps their enrolments, so on such a dataset too
  # an end form is absent whether it is missing or merely out of range.
  opts <- x$metadata$dataset_options
  if ("status" %in% names(x$enrollments) &&
      (!is.null(opts$surveillance_end_from) || !is.null(opts$surveillance_end_to)))
    return(.rule_skipped(
      43L, "the end forms dated outside its surveillance-end window, so an end form there is absent like a missing one"))

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "ACTIVE") |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::anti_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(days_open = .days_open(.data$enrolledAt, as_of)) |>
    dplyr::filter(.data$days_open > .open_enrolment_max_days) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 43L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("enrolledAt", "days_open")) |>
    dplyr::mutate(
      rule_id        = 43L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = NA_integer_,
      context        = .data$context,
      .keep = "none")
}

# Find enrolments still active more than `.open_enrolment_max_days` after
# their enrolment date, measured against `as_of`, whose surveillance-end
# event exists but is not completed. The finding carries that event and its
# status.
validation_rule_44 <- function(x, exceptions, as_of)
{
  check_neoipcr_ds(x)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "ACTIVE") |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      .with_status(x$events, .event_status_levels) |>
        dplyr::filter(.data$event_type_key == "end" &
                      .data$status != "COMPLETED") |>
        dplyr::select("enrollment_key", "event_key", "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::mutate(days_open = .days_open(.data$enrolledAt, as_of)) |>
    dplyr::filter(.data$days_open > .open_enrolment_max_days) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 44L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("enrolledAt", "days_open", "status")) |>
    dplyr::mutate(
      rule_id        = 44L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
