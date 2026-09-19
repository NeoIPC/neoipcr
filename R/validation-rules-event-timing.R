# The four rule families of this file each run once per infection or surgery
# event type. Every one reads the type's form data from the dataset slot
# `.event_data_slot` names for it.

# Rules 27, 31, 35, 39 and 41: the day of life stored on an event form does
# not match the day of life on the admission form advanced by the days from
# the enrolment date to the event date.
.rule_dol_mismatch <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  form <- x[[.event_data_slot[[event_type]]]]
  if (!"dol" %in% names(form) || !"dol" %in% names(x$admissionData))
    return(.rule_skipped(
      rule_id, "the day of life on the admission form and on the event form"))
  id <- rule_id

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "adm_event_key" = "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$admissionData |>
        dplyr::select("event_key", "admission_dol" = "dol"),
      dplyr::join_by("adm_event_key" == "event_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == event_type) |>
        dplyr::select("enrollment_key", "event_key", "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      form |>
        dplyr::select("event_key", "dol"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(
      dol_calc = .data$admission_dol + as.integer(.data$occurredAt - .data$enrolledAt)) |>
    dplyr::filter(.data$dol != .data$dol_calc) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("dol", "dol_calc")) |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 28, 32, 36, 40 and 42: the day of occurrence after admission stored
# on an event form does not match the days from the enrolment date to the
# event date.
.rule_los_mismatch <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  form <- x[[.event_data_slot[[event_type]]]]
  if (!"los" %in% names(form))
    return(.rule_skipped(rule_id, "the day of occurrence on the event form"))
  id <- rule_id

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == event_type) |>
        dplyr::select("enrollment_key", "event_key", "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      form |>
        dplyr::select("event_key", "los"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(
      los_calc = as.integer(.data$occurredAt - .data$enrolledAt)) |>
    dplyr::filter(.data$los != .data$los_calc) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("los", "los_calc")) |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 29, 33 and 37: an infection recorded within the first three days of
# life, where an early-onset infection is not attributed to the hospital.
.rule_early_dol <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  form <- x[[.event_data_slot[[event_type]]]]
  if (!"dol" %in% names(form))
    return(.rule_skipped(rule_id, "the day of life on the event form"))
  id <- rule_id

  x$events |>
    dplyr::filter(.data$event_type_key == event_type) |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(
      form |>
        dplyr::select("event_key", "dol"),
      dplyr::join_by("event_key")) |>
    dplyr::filter(.data$dol < 4L) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = "dol") |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 30, 34 and 38: an infection recorded on the first or second day of
# hospitalization of a referred or (re-)admitted patient (admission type 3),
# where it is more likely to have been acquired before this stay. The day of
# hospitalization counts from one, the stored day of occurrence from zero.
.rule_early_dos <- function(x, exceptions, rule_id, event_type)
{
  check_neoipcr_ds(x)
  form <- x[[.event_data_slot[[event_type]]]]
  if (!"los" %in% names(form) || !"type" %in% names(x$admissionData))
    return(.rule_skipped(
      rule_id, "the admission type and the day of occurrence on the event form"))
  id <- rule_id

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "adm_event_key" = "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$admissionData |>
        dplyr::filter(as.character(.data$type) == "3") |>
        dplyr::select("event_key"),
      dplyr::join_by("adm_event_key" == "event_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == event_type) |>
        dplyr::select("enrollment_key", "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      form |>
        dplyr::select("event_key", "los"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(dos = .data$los + 1L) |>
    dplyr::filter(.data$dos < 3L) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = "dos") |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Sepsis: day-of-life mismatch, day-of-occurrence mismatch, early onset,
# early after (re-)admission.
validation_rule_27 <- function(x, exceptions)
  .rule_dol_mismatch(x, exceptions, 27L, "bsi")
validation_rule_28 <- function(x, exceptions)
  .rule_los_mismatch(x, exceptions, 28L, "bsi")
validation_rule_29 <- function(x, exceptions)
  .rule_early_dol(x, exceptions, 29L, "bsi")
validation_rule_30 <- function(x, exceptions)
  .rule_early_dos(x, exceptions, 30L, "bsi")

# Pneumonia: the same four checks.
validation_rule_31 <- function(x, exceptions)
  .rule_dol_mismatch(x, exceptions, 31L, "hap")
validation_rule_32 <- function(x, exceptions)
  .rule_los_mismatch(x, exceptions, 32L, "hap")
validation_rule_33 <- function(x, exceptions)
  .rule_early_dol(x, exceptions, 33L, "hap")
validation_rule_34 <- function(x, exceptions)
  .rule_early_dos(x, exceptions, 34L, "hap")

# Necrotizing enterocolitis: the same four checks.
validation_rule_35 <- function(x, exceptions)
  .rule_dol_mismatch(x, exceptions, 35L, "nec")
validation_rule_36 <- function(x, exceptions)
  .rule_los_mismatch(x, exceptions, 36L, "nec")
validation_rule_37 <- function(x, exceptions)
  .rule_early_dol(x, exceptions, 37L, "nec")
validation_rule_38 <- function(x, exceptions)
  .rule_early_dos(x, exceptions, 38L, "nec")

# Surgical procedure: the two mismatch checks only — a procedure has no onset
# to attribute.
validation_rule_39 <- function(x, exceptions)
  .rule_dol_mismatch(x, exceptions, 39L, "pro")
validation_rule_40 <- function(x, exceptions)
  .rule_los_mismatch(x, exceptions, 40L, "pro")

# Surgical site infection: the two mismatch checks only — its onset is judged
# against the procedure's follow-up window by rule 19.
validation_rule_41 <- function(x, exceptions)
  .rule_dol_mismatch(x, exceptions, 41L, "ssi")
validation_rule_42 <- function(x, exceptions)
  .rule_los_mismatch(x, exceptions, 42L, "ssi")
