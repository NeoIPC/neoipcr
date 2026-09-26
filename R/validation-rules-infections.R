# The rules of this file relate an infection event to the patient's other
# infection events, to the enrolment's surveillance-end form, and to its own
# infectious-agent findings.

# The fewest days between two infections of one type: the protocol registers
# the same type of infection again after 14 days at the earliest, so a
# repeat within that interval is one infection recorded twice, or a new
# organism at a site already infected, rather than a new infection.
.infection_min_interval_days <- 14L

# Find infection events recorded fewer than `.infection_min_interval_days`
# after the patient's previous event of the same type, across the patient's
# enrolments. The later event of each such pair is the finding, with both
# dates and the days between; an event without a date cannot be placed and
# is left out of the sequence. Two events of one type on one day are
# ordered by key, which the import assigns arbitrarily, so which of the two
# carries the finding is arbitrary too; either is the repeat of the other.
validation_rule_49 <- function(x, exceptions)
{
  check_neoipcr_ds(x)

  x$events |>
    dplyr::filter(.data$event_type_key %in% .infection_event_types &
                  !is.na(.data$occurredAt)) |>
    dplyr::select("patient_key", "enrollment_key", "event_key",
                  "event_type_key", "occurredAt") |>
    dplyr::arrange(.data$patient_key, .data$event_type_key,
                   .data$occurredAt, .data$event_key) |>
    dplyr::mutate(
      occurredAt_previous = dplyr::lag(.data$occurredAt),
      .by = c("patient_key", "event_type_key")) |>
    dplyr::mutate(
      days_between = as.integer(.data$occurredAt - .data$occurredAt_previous)) |>
    dplyr::filter(.data$days_between < .infection_min_interval_days) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 49L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("occurredAt", "occurredAt_previous", "days_between")) |>
    dplyr::mutate(
      rule_id        = 49L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# The device each device-association value names on each infection form,
# as the stem of the surveillance-end column counting its days. The values
# are the codes of the option sets NEOIPC_BSI_DEVICE_ASS (1 a central, 2 a
# peripheral venous catheter) and NEOIPC_HAP_DEVICE_ASS (1 non-invasive, 2
# invasive ventilation); 0 is no association on either.
.device_associations <- tibble::tibble(
  event_type_key = c("bsi", "bsi", "hap", "hap"),
  dev_ass        = c("1",   "2",   "1",   "2"),
  device         = c("cvc", "pvc", "niv", "inv"))

# Find device-associated sepsis and pneumonia events on an enrolment whose
# completed surveillance-end form counts no day of that device. The
# association says the device was in place on the days before the infection,
# which the cumulative count cannot confirm, but a count of zero, or none,
# contradicts it. An enrolment without a completed surveillance-end form has
# its counts still to come and is not judged.
validation_rule_50 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"dev_ass" %in% names(x$sepsisData) ||
      !"dev_ass" %in% names(x$pneumoniaData) ||
      !all(c("cvc_days", "pvc_days", "niv_days", "inv_days") %in%
           names(x$surveillanceEndData)))
    return(.rule_skipped(
      50L, "the device association on the sepsis and pneumonia forms and the device days on the surveillance-end form"))

  associations <- dplyr::bind_rows(
    x$sepsisData |>
      dplyr::select("event_key", "dev_ass") |>
      dplyr::mutate(event_type_key = "bsi"),
    x$pneumoniaData |>
      dplyr::select("event_key", "dev_ass") |>
      dplyr::mutate(event_type_key = "hap")) |>
    dplyr::mutate(dev_ass = as.character(.data$dev_ass)) |>
    dplyr::inner_join(
      .device_associations,
      dplyr::join_by("event_type_key", "dev_ass")) |>
    dplyr::select("event_key", "device")

  device_days <- .with_status(x$events, .event_status_levels) |>
    dplyr::filter(.data$event_type_key == "end" &
                  .data$status == "COMPLETED") |>
    dplyr::select("enrollment_key", "event_key") |>
    dplyr::inner_join(
      x$surveillanceEndData |>
        dplyr::select("event_key", "cvc_days", "pvc_days", "niv_days", "inv_days"),
      dplyr::join_by("event_key")) |>
    dplyr::select(!"event_key") |>
    tidyr::pivot_longer(
      !"enrollment_key",
      names_to = "device", names_pattern = "^(.*)_days$",
      values_to = "device_days")

  x$events |>
    dplyr::filter(.data$event_type_key %in% c("bsi", "hap")) |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(associations, dplyr::join_by("event_key")) |>
    dplyr::inner_join(device_days, dplyr::join_by("enrollment_key", "device")) |>
    dplyr::filter(is.na(.data$device_days) | .data$device_days == 0L) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 50L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("device", "device_days")) |>
    dplyr::mutate(
      rule_id        = 50L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find pneumonia, NEC and SSI events whose secondary-BSI item disagrees with
# the secondary-BSI organisms recorded on them: the item Yes (code 1 of the
# option set NEOIPC_YES_NO_NO_FOLLOWUP) with no organism on any of the three
# forms, and organisms under an item that is not Yes — No, No follow-up or
# unanswered — on a pneumonia or NEC form, where the organism fields the
# client hides still show while they hold a value. The same organisms on an
# SSI form sit in a section the client hides whatever it holds, so the team
# cannot see them and they are not this rule's finding.
validation_rule_55 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"sec_bsi" %in% names(x$necData) ||
      !"sec_bsi" %in% names(x$pneumoniaData) ||
      !"sec_bsi" %in% names(x$ssiData) ||
      !all(c("secondary_bsi", "pathogen_key") %in% names(x$infectiousAgentFindings)))
    return(.rule_skipped(
      55L, "the secondary-BSI item on the NEC, pneumonia and SSI forms and the findings' secondary-BSI flag and pathogen"))

  items <- dplyr::bind_rows(
    x$necData       |> dplyr::select("event_key", "sec_bsi"),
    x$pneumoniaData |> dplyr::select("event_key", "sec_bsi"),
    x$ssiData       |> dplyr::select("event_key", "sec_bsi"))

  # A finding row without an organism — a resistance or name companion
  # stored on its own — records no organism.
  organisms <- x$infectiousAgentFindings |>
    dplyr::filter(dplyr::coalesce(.data$secondary_bsi, FALSE) &
                  !is.na(.data$pathogen_key)) |>
    dplyr::count(.data$event_key, name = "organisms")

  x$events |>
    dplyr::filter(.data$event_type_key %in% c("nec", "hap", "ssi")) |>
    dplyr::select("patient_key", "enrollment_key", "event_key", "event_type_key") |>
    # An event whose only stored values are organisms has no form row: its
    # item is unanswered, which is an answer other than Yes.
    dplyr::left_join(items, dplyr::join_by("event_key")) |>
    dplyr::left_join(organisms, dplyr::join_by("event_key")) |>
    dplyr::mutate(
      organisms = tidyr::replace_na(.data$organisms, 0L),
      yes       = dplyr::coalesce(.data$sec_bsi == "1", FALSE)) |>
    dplyr::filter(
      (.data$yes & .data$organisms == 0L) |
      (!.data$yes & .data$organisms > 0L & .data$event_type_key != "ssi")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 55L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("sec_bsi", "organisms")) |>
    dplyr::mutate(
      rule_id        = 55L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
