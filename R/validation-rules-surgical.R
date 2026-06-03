# Find surgical site infections where the event date is not within the follow-up
# period of a previous surgical procedure event.
validation_rule_19 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  # TODO: Implement
  r <- dplyr::bind_cols(
    rule_id = c(19L),
    x$enrollments |>
      dplyr::select("enrollment_key") |>
      dplyr::filter(.data$enrollment_key == -1))

  return(r)
}

# Find surgical procedure events with a main procedure code that is not a valid
# ICHE procedure code.
validation_rule_22 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  if (nrow(x$surgeryData) < 1)
    return(dplyr::bind_cols(
      rule_id = c(22L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(dplyr::bind_cols(
      rule_id = c(22L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- dplyr::bind_cols(
    rule_id = c(22L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","event_key","procedure_description","main_procedure_code")) |>
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
    return(dplyr::bind_cols(
      rule_id = c(23L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(dplyr::bind_cols(
      rule_id = c(23L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- dplyr::bind_cols(
    rule_id = c(23L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","event_key","procedure_description","side_procedure_code_1")) |>
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
    return(dplyr::bind_cols(
      rule_id = c(24L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  if (!file.exists("../ICHE-Health-Intervention-Codes.csv")) {
    warn("Skipping validation of ICHE codes due to missing ICHE Health Intervention Code information.")
    return(dplyr::bind_cols(
      rule_id = c(24L),
      x$enrollments |>
        dplyr::select("enrollment_key") |>
        dplyr::filter(.data$enrollment_key == -1)))
  }

  valid_iche_codes <- readr::read_csv("../ICHE-Health-Intervention-Codes.csv", col_names = FALSE, col_types = "c") |>
    dplyr::pull(1)

  r <- dplyr::bind_cols(
    rule_id = c(24L),
    .with_hierarchy_context(x$enrollments, x$metadata$departments) |>
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
            "patient_key")),"enrollment_key","event_key","procedure_description","side_procedure_code_2")) |>
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
