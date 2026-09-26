# Find surveillance-end events whose stored number of patient days is
# missing, or does not match the value calculated from the enrolment date and
# the event date. patient_days is compulsory in DHIS2 and calculated from those
# dates, so a missing value is a data-quality failure and is flagged like a
# mismatch. The `is.na()` guard is required because `NA != x` is NA, which
# `filter()` drops — so without it a missing value would slip past validation.
validation_rule_18 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"patient_days" %in% names(x$surveillanceEndData))
    return(.rule_skipped(18L, "the surveillance-end form's patient days"))

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key", "enrolledAt") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "event_key", "occurredAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$surveillanceEndData |>
        dplyr::select("event_key", "patient_days"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(
      patient_days_calculated = 1L + as.integer(.data$occurredAt - .data$enrolledAt)) |>
    dplyr::filter(is.na(.data$patient_days) |
                  .data$patient_days != .data$patient_days_calculated) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 18L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("patient_days", "patient_days_calculated")) |>
    dplyr::mutate(
      rule_id        = 18L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find surveillance-end events whose antibiotic substance days sum to less
# than the total number of antibiotic days. The sum may exceed the total —
# combination therapy counts one antibiotic day and several substance days —
# so only the shortfall is a finding, and an event without antibiotic days is
# not checked at all.
validation_rule_21 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"ab_days" %in% names(x$surveillanceEndData) ||
      !"days" %in% names(x$substanceDays))
    return(.rule_skipped(21L, "the antibiotic days and the substance days"))

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$surveillanceEndData |>
        dplyr::filter(.data$ab_days > 0L) |>
        dplyr::select("event_key", "ab_days"),
      dplyr::join_by("event_key")) |>
    dplyr::left_join(
      x$substanceDays |>
        dplyr::group_by(.data$event_key) |>
        dplyr::summarise(
          ab_substance_days = sum(.data$days, na.rm = TRUE),
          .groups = "drop"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(
      ab_substance_days = tidyr::replace_na(.data$ab_substance_days, 0L)) |>
    dplyr::filter(.data$ab_substance_days < .data$ab_days) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 21L),
      dplyr::join_by("enrollment_key")) |>
    tidyr::nest(context = c("ab_substance_days", "ab_days")) |>
    dplyr::mutate(
      rule_id        = 21L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# The cumulative counts of the surveillance-end form that count patient days
# and so cannot exceed them: the device days, the ventilation days as a whole
# (`vs_days`, the reader's sum of the invasive and non-invasive days — a
# ventilation day is either an invasive or a non-invasive one, so the sum is
# bounded like each part), the antibiotic days and the protective-factor
# days.
.patient_day_counts <- c(
  "cvc_days", "pvc_days", "inv_days", "niv_days", "vs_days", "ab_days",
  "human_milk_days", "kangaroo_care_days", "probiotic_days")

# Find surveillance-end forms on which a cumulative count exceeds the
# patient days, one finding per count, which the finding names by its
# column. A form without patient days compares nothing and is rule 18's.
validation_rule_51 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("patient_days", .patient_day_counts) %in%
           names(x$surveillanceEndData)))
    return(.rule_skipped(
      51L, "the surveillance-end form's patient days and cumulative counts"))

  x$enrollments |>
    dplyr::select("patient_key", "enrollment_key") |>
    dplyr::inner_join(
      x$events |>
        dplyr::filter(.data$event_type_key == "end") |>
        dplyr::select("enrollment_key", "event_key"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      x$surveillanceEndData |>
        dplyr::select("event_key", "patient_days",
                      tidyselect::all_of(.patient_day_counts)),
      dplyr::join_by("event_key")) |>
    tidyr::pivot_longer(
      tidyselect::all_of(.patient_day_counts),
      names_to = "count", values_to = "days") |>
    dplyr::filter(.data$days > .data$patient_days) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 51L),
      dplyr::join_by("enrollment_key")) |>
    # One finding per count: on the keys alone, a form's counts would nest
    # into one finding with a context row per count.
    dplyr::mutate(finding = dplyr::row_number()) |>
    tidyr::nest(context = c("count", "days", "patient_days")) |>
    dplyr::mutate(
      rule_id        = 51L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# The antibiotic substance slots of every surveillance-end form with the
# enrolment's keys: a slot is a substance and its days, and a row exists for
# a slot as soon as either was entered.
.substance_slots <- function(x)
  x$events |>
    dplyr::filter(.data$event_type_key == "end") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(
      x$substanceDays |>
        dplyr::select("event_key", "index", "substance_code", "days"),
      dplyr::join_by("event_key"))

.substance_slot_columns <- c("index", "substance_code", "days")

# Find substance slots that hold a substance without its days, or days
# without a substance. Both are entered together, and the form makes the
# days mandatory once a substance is chosen. A count of zero is no days;
# the field admits positive values only, so only a write outside the form
# records one.
validation_rule_52 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(.substance_slot_columns %in% names(x$substanceDays)))
    return(.rule_skipped(52L, "the substance slots' substance and days"))

  .substance_slots(x) |>
    dplyr::filter(is.na(.data$substance_code) !=
                  dplyr::coalesce(.data$days < 1L, TRUE)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 52L),
      dplyr::join_by("enrollment_key")) |>
    # One finding per slot: on the keys alone, a form's slots would nest
    # into one finding with a context row per slot.
    dplyr::mutate(finding = dplyr::row_number()) |>
    tidyr::nest(context = c("index", "substance_code", "days")) |>
    dplyr::mutate(
      rule_id        = 52L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find substance slots whose days exceed the form's antibiotic days or its
# patient days. A substance day is an antibiotic day and an antibiotic day a
# patient day, so a slot's days are bounded by both; a substance on a form
# with no antibiotic days at all exceeds them with its first day.
validation_rule_53 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(.substance_slot_columns %in% names(x$substanceDays)) ||
      !all(c("ab_days", "patient_days") %in% names(x$surveillanceEndData)))
    return(.rule_skipped(
      53L, "the substance slots' days and the surveillance-end form's antibiotic and patient days"))

  .substance_slots(x) |>
    dplyr::inner_join(
      x$surveillanceEndData |>
        dplyr::select("event_key", "ab_days", "patient_days"),
      dplyr::join_by("event_key")) |>
    dplyr::filter(
      dplyr::coalesce(.data$days > .data$ab_days, FALSE) |
      dplyr::coalesce(.data$days > .data$patient_days, FALSE)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 53L),
      dplyr::join_by("enrollment_key")) |>
    # One finding per slot, as in rule 52.
    dplyr::mutate(finding = dplyr::row_number()) |>
    tidyr::nest(context = c("index", "substance_code", "days", "ab_days", "patient_days")) |>
    dplyr::mutate(
      rule_id        = 53L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find substances recorded in two slots of one surveillance-end form, one
# finding per pair, naming the lower slot first. The form means one slot
# per substance: a substance in two has its days either split or entered
# twice, and the record cannot say which.
validation_rule_54 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(.substance_slot_columns %in% names(x$substanceDays)))
    return(.rule_skipped(54L, "the substance slots' substance and days"))

  slots <- .substance_slots(x) |>
    dplyr::filter(!is.na(.data$substance_code)) |>
    dplyr::select(!"days")

  slots |>
    dplyr::inner_join(
      slots |>
        dplyr::select("event_key", "substance_code", "index_other" = "index"),
      dplyr::join_by("event_key", "substance_code", "index" < "index_other")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 54L),
      dplyr::join_by("enrollment_key")) |>
    # One finding per pair, as in rule 52.
    dplyr::mutate(finding = dplyr::row_number()) |>
    tidyr::nest(context = c("substance_code", "index", "index_other")) |>
    dplyr::mutate(
      rule_id        = 54L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
