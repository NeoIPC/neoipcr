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
# is left out of the sequence.
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

# Find pneumonia and SSI events none of whose secondary-BSI organisms was
# identified at the primary infection site. The protocol assigns a
# bloodstream infection to a pneumonia or an SSI only when an organism from
# the blood matches one from the primary site, so a form with secondary
# organisms and no match — none of its primary organisms among them, or no
# primary organism at all — records an infection the definition does not
# allow. Organisms are compared as catalogue concepts, a synonym resolving
# to the concept it names, and the finding carries the concepts' names; an
# entry at genus level does not match one at species level. A finding
# recorded as not listed (concept 0), without a concept, or with one the
# catalogue does not carry has no identity to compare, so a form carrying
# one on either side is left alone; the last of those is the catalogue
# lagging the option set, which is the network's to mend. Only what the form
# shows is compared. An SSI is judged only while its secondary-BSI item is
# Yes: under any other answer its secondary organisms sit in a section the
# client hides whatever it holds, so the team cannot see them, whereas a
# pneumonia's organism fields show while they hold a value. The primary-site
# organisms count only while their section shows — on a pneumonia while the
# microbiological test result is Yes, on an SSI while an organism was
# identified at one of its depths — since otherwise the client hides that
# section with whatever it holds, blanking the first slot only; a form whose
# primary section is hidden is judged as recording no primary organism. NEC
# forms record no primary organism and are not judged.
validation_rule_56 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(c("pathogen_key", "secondary_bsi") %in%
           names(x$infectiousAgentFindings)) ||
      !"microbiological_test_result" %in% names(x$pneumoniaData) ||
      !all(c("sec_bsi", "organisms_superf", "organisms_deep", "organisms_organ") %in%
           names(x$ssiData)))
    return(.rule_skipped(
      56L, "the findings' pathogen and secondary-BSI flag, the pneumonia form's test result and the SSI form's secondary-BSI item and organism flags"))

  concepts <- get_pathogen_taxonomy() |>
    dplyr::select("input_id", "output_id", "output_name")

  # The catalogue names of the findings `selected` picks out, joined for the
  # finding's context; `NA` where none is selected.
  names_of <- function(selected, concept_names)
    dplyr::na_if(paste(sort(unique(concept_names[selected])), collapse = ", "), "")

  yes <- function(item) dplyr::coalesce(item == "1", FALSE)

  pneumonia_forms <- x$pneumoniaData |>
    dplyr::select("event_key", "microbiological_test_result") |>
    dplyr::mutate(
      primary_shown = yes(.data$microbiological_test_result),
      .keep = "unused")
  ssi_forms <- x$ssiData |>
    dplyr::filter(yes(.data$sec_bsi)) |>
    dplyr::select("event_key", "organisms_superf", "organisms_deep", "organisms_organ") |>
    dplyr::mutate(
      primary_shown = yes(.data$organisms_superf) | yes(.data$organisms_deep) |
                      yes(.data$organisms_organ),
      .keep = "unused")

  # A pneumonia without a form row has its test result unanswered, so its
  # primary section is hidden; an SSI without one has its item unanswered,
  # so it is not judged.
  judged <- x$events |>
    dplyr::select("patient_key", "enrollment_key", "event_key", "event_type_key")
  judged <- dplyr::bind_rows(
    judged |>
      dplyr::filter(.data$event_type_key == "hap") |>
      dplyr::left_join(pneumonia_forms, dplyr::join_by("event_key")),
    judged |>
      dplyr::filter(.data$event_type_key == "ssi") |>
      dplyr::inner_join(ssi_forms, dplyr::join_by("event_key"))) |>
    dplyr::mutate(primary_shown = dplyr::coalesce(.data$primary_shown, FALSE)) |>
    dplyr::select(!"event_type_key")

  judged |>
    dplyr::inner_join(
      x$infectiousAgentFindings |>
        dplyr::select("event_key", "pathogen_key", "secondary_bsi"),
      dplyr::join_by("event_key")) |>
    dplyr::mutate(secondary_bsi = dplyr::coalesce(.data$secondary_bsi, FALSE)) |>
    dplyr::filter(.data$secondary_bsi | .data$primary_shown) |>
    dplyr::left_join(concepts, dplyr::join_by("pathogen_key" == "input_id")) |>
    dplyr::summarise(
      comparable = all(!is.na(.data$output_id) & .data$output_id != 0L),
      matched    = any(.data$output_id[.data$secondary_bsi] %in%
                       .data$output_id[!.data$secondary_bsi]),
      secondary  = names_of(.data$secondary_bsi, .data$output_name),
      primary    = names_of(!.data$secondary_bsi, .data$output_name),
      .by = c("patient_key", "enrollment_key", "event_key")) |>
    dplyr::filter(.data$comparable & !is.na(.data$secondary) & !.data$matched) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 56L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("secondary", "primary")) |>
    dplyr::mutate(
      rule_id        = 56L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
