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
      dplyr::join_by("rule_id","enrollment_key","event_key"))

  return(r)
}

validation_rules <- list(
  list(
    id = 1,
    fun = validation_rule_1,
    formatter = function(x) gettext("The patient record does not have an enrolment.")
  ),
  list(
    id = 2,
    fun = validation_rule_2,
    formatter = function(x) gettext("The patient record has a completed surveillance end form but the enrolment is still active.")
  )
)

validate <- function(x, rules = NULL, exceptions = NULL)
{
  check_neoipcr_ds(x)

  r <- validation_rules |>
    lapply(\(r)if(is.null(rules)||r$id%in%rules)r$fun(x,exceptions)) |>
    dplyr::bind_rows()

  invisible(r)
}
