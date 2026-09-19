# Find enrolments whose admission event is dated differently from the
# enrolment itself.
validation_rule_3 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "event_key", "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$enrolledAt != .data$occurredAt) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 3L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("enrolledAt", "occurredAt")) |>
    dplyr::mutate(
      rule_id        = 3L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find enrolments whose surveillance-end event is dated before the admission
# event.
validation_rule_4 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "admOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "event_key", "endOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$endOccurredAt < .data$admOccurredAt) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 4L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("admOccurredAt", "endOccurredAt")) |>
    dplyr::mutate(
      rule_id        = 4L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 12–15 share one shape: an infection or surgery event dated outside
# the window of its enrolment, which runs from the later of the enrolment
# date and the admission event to the surveillance-end event. The event's own
# date travels under a type-specific name so the consumer's sentence can name
# the form. A surgical site infection is not held to that window: it is
# attributed to its procedure's follow-up period (rule 19), which may run past
# the discharge and into a readmission.
.rule_event_outside_enrolment <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  id <- rule_id
  date_col <- paste0(event_type, "OccurredAt")

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "admOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "endOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == event_type) |>
        dplyr::select("enrollment_key", "event_key", "eventOccurredAt" = "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$eventOccurredAt < .data$enrolledAt |
                  .data$eventOccurredAt < .data$admOccurredAt |
                  .data$eventOccurredAt > .data$endOccurredAt) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    dplyr::rename_with(\(nm) date_col, .cols = "eventOccurredAt") |>
    tidyr::nest(context = c(
      "enrolledAt", "admOccurredAt", "endOccurredAt",
      tidyselect::all_of(date_col))) |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find sepsis events dated outside the enrolment window.
validation_rule_12 <- function(x, exceptions)
  .rule_event_outside_enrolment(x, exceptions, 12L, "bsi")

# Find necrotizing enterocolitis events dated outside the enrolment window.
validation_rule_13 <- function(x, exceptions)
  .rule_event_outside_enrolment(x, exceptions, 13L, "nec")

# Find pneumonia events dated outside the enrolment window.
validation_rule_14 <- function(x, exceptions)
  .rule_event_outside_enrolment(x, exceptions, 14L, "hap")

# Find surgical procedure events dated outside the enrolment window.
validation_rule_15 <- function(x, exceptions)
  .rule_event_outside_enrolment(x, exceptions, 15L, "pro")
