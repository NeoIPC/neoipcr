# Find enrolments whose admission event is not completed.
validation_rule_5 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      .with_status(x$events, .event_status_levels) |>
        dplyr::filter(.data$event_type_key == "adm" &
                      .data$status != "COMPLETED") |>
        dplyr::select("enrollment_key", "event_key", "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 5L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = "status") |>
    dplyr::mutate(
      rule_id        = 5L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find completed enrolments whose surveillance-end event is not completed.
validation_rule_6 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::filter(.data$status == "COMPLETED") |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      .with_status(x$events, .event_status_levels) |>
        dplyr::filter(.data$event_type_key == "end" &
                      .data$status != "COMPLETED") |>
        dplyr::select("enrollment_key", "event_key", "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 6L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = "status") |>
    dplyr::mutate(
      rule_id        = 6L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 7–11 share one shape: an infection or surgery form that is not
# completed while the enrolment, or its surveillance-end form, already is.
# Either completion closes the record, so either one makes an open form a
# finding. The form's own status travels under a type-specific name so the
# consumer's sentence can name the form.
.rule_form_not_completed <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  id <- rule_id
  status_col <- paste0(event_type, "_status")
  events <- .with_status(x$events, .event_status_levels)

  .with_status(x$enrollments, .enrollment_status_levels) |>
    dplyr::select("patient_key", "enrollment_key", "enrollment_status" = "status") |>
    dplyr::left_join(
      events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "end_status" = "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      events |>
        dplyr::filter(.data$event_type_key == event_type &
                      .data$status != "COMPLETED") |>
        dplyr::select("enrollment_key", "event_key", "form_status" = "status"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$enrollment_status == "COMPLETED" |
                  .data$end_status == "COMPLETED") |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    dplyr::rename_with(\(nm) status_col, .cols = "form_status") |>
    tidyr::nest(context = c(
      "enrollment_status", "end_status", tidyselect::all_of(status_col))) |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find sepsis forms not completed on a completed enrolment or surveillance end.
validation_rule_7 <- function(x, exceptions)
  .rule_form_not_completed(x, exceptions, 7L, "bsi")

# Find necrotizing enterocolitis forms not completed on a completed enrolment
# or surveillance end.
validation_rule_8 <- function(x, exceptions)
  .rule_form_not_completed(x, exceptions, 8L, "nec")

# Find pneumonia forms not completed on a completed enrolment or surveillance
# end.
validation_rule_9 <- function(x, exceptions)
  .rule_form_not_completed(x, exceptions, 9L, "hap")

# Find surgical procedure forms not completed on a completed enrolment or
# surveillance end.
validation_rule_10 <- function(x, exceptions)
  .rule_form_not_completed(x, exceptions, 10L, "pro")

# Find surgical site infection forms not completed on a completed enrolment
# or surveillance end.
validation_rule_11 <- function(x, exceptions)
  .rule_form_not_completed(x, exceptions, 11L, "ssi")
