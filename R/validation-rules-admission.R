# The rules of this file read the admission form's type and day of life
# against the eligibility window and against the patient's other enrolments.
# The admission type's values are the codes of the option set
# NEOIPC_ADMISSION_TYPES: 1 an infant admitted from the delivery room after
# a birth in the hospital, 2 one transferred or readmitted on the day of
# birth, 3 one transferred or readmitted the day after birth or later.

# The last day of life on which an infant is eligible for the surveillance:
# the protocol admits an infant within 120 days of birth, the day of birth
# being day 1, so day 120 is the last eligible day and day 121 the first
# ineligible one. The capture-time configuration warns from day 150 only.
.admission_max_dol <- 120L

# Each enrolment's admission form: the enrolment's keys and date, the
# admission event's key, and the columns `cols` of the form.
.admission_forms <- function(x, cols)
  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "adm") |>
        dplyr::select("enrollment_key", "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$admissionData |>
        dplyr::select("event_key", tidyselect::all_of(cols)),
      dplyr::join_by("event_key"))

# Find admission forms of an infant transferred or readmitted the day after
# birth or later (type 3) whose day of life at admission lies beyond the
# eligibility window. The other two types have day 1 assigned by the client
# on every save, so a higher value stored there is not the team's to see or
# to correct and is left to the network.
validation_rule_45 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("type", "dol") %in% names(x$admissionData)))
    return(.rule_skipped(45L, "the admission form's type and day of life"))

  .admission_forms(x, c("type", "dol")) |>
    dplyr::filter(.data$type == "3" & .data$dol > .admission_max_dol) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 45L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = "dol") |>
    dplyr::mutate(
      rule_id        = 45L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find admission forms recording an infant transferred or readmitted the day
# after birth or later (type 3) whose day of life at admission is missing or
# below 2. Such an infant is at least one day old when admitted, and this is
# the one admission type whose day of life the team enters itself, the
# client assigning day 1 to the other two. A missing value also leaves rules
# 27, 31, 35, 39 and 41 without the base they compute an event's expected
# day of life from, so every event of the enrolment escapes them.
validation_rule_46 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("type", "dol") %in% names(x$admissionData)))
    return(.rule_skipped(46L, "the admission form's type and day of life"))

  .admission_forms(x, c("type", "dol")) |>
    dplyr::filter(.data$type == "3" & (is.na(.data$dol) | .data$dol < 2L)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 46L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = "dol") |>
    dplyr::mutate(
      rule_id        = 46L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find enrolments whose admission form records an infant admitted from the
# delivery room or on the day of birth (types 1 and 2) while the patient has
# an earlier enrolment: a stay that follows another is a transfer or
# readmission after the day of birth (type 3) by definition. The finding
# names the date of the latest earlier enrolment; an enrolment dated the
# same day as another is rule 17's concern, not this one's.
validation_rule_47 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"type" %in% names(x$admissionData))
    return(.rule_skipped(47L, "the admission form's type"))

  .admission_forms(x, "type") |>
    dplyr::filter(.data$type %in% c("1", "2")) |>
    dplyr::inner_join(
      x$enrollments |>
        dplyr::select("patient_key", "enrolledAt_previous" = "enrolledAt"),
      dplyr::join_by("patient_key", "enrolledAt" > "enrolledAt_previous")) |>
    # The latest earlier enrolment names the finding; two earlier enrolments
    # on one day share the date, so the tie is broken arbitrarily.
    dplyr::slice_max(
      .data$enrolledAt_previous, by = "enrollment_key", with_ties = FALSE) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 47L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("type", "enrolledAt", "enrolledAt_previous")) |>
    dplyr::mutate(
      rule_id        = 47L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
