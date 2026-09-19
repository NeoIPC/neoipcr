# Find surgical site infections that fall outside the follow-up window of
# every surgical procedure recorded for the patient. The protocol counts the
# procedure date as day 1 of the window, which lasts 30 days, or 90 days for
# a deep or organ/space infection after a procedure with an implant, so the
# window covers the offsets 0 to 29 (or 89) from the procedure. Procedures
# from the patient's other enrolments count, since an infection can follow a
# procedure of an earlier stay.
validation_rule_19 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!"infection_type" %in% names(x$ssiData) ||
      !"implant" %in% names(x$surgeryData))
    return(.rule_skipped(
      19L, "the infection type on the SSI form and the implant flag on the surgery form"))

  # An SSI without a date has no offset to judge: it is not outside any
  # window, so it is left to the rules about the form itself.
  ssi_events <- x$events |>
    dplyr::filter(.data$event_type_key == "ssi", !is.na(.data$occurredAt)) |>
    dplyr::select("patient_key", "enrollment_key", "event_key",
                  "ssiOccurredAt" = "occurredAt") |>
    dplyr::inner_join(
      x$ssiData |>
        dplyr::select("event_key", "infection_type"),
      dplyr::join_by("event_key"))

  surgery_events <- x$events |>
    dplyr::filter(.data$event_type_key == "pro") |>
    dplyr::select("patient_key", "event_key", "surgeryOccurredAt" = "occurredAt") |>
    dplyr::inner_join(
      x$surgeryData |>
        dplyr::select("event_key", "implant"),
      dplyr::join_by("event_key")) |>
    # An implant that was not recorded is no implant: the flag decides only
    # whether the longer window applies, and left `NA` it would void both.
    dplyr::mutate(implant = tidyr::replace_na(.data$implant, FALSE)) |>
    dplyr::select(!"event_key")

  ssi_events |>
    dplyr::left_join(
      surgery_events,
      dplyr::join_by("patient_key"),
      relationship = "many-to-many") |>
    dplyr::mutate(
      ssi_offset = as.integer(.data$ssiOccurredAt - .data$surgeryOccurredAt),
      covered    = tidyr::replace_na(
        .data$ssi_offset >= 0L & (
          (.data$infection_type == "1" & .data$ssi_offset < 30L) |
          (.data$infection_type != "1" &  .data$implant & .data$ssi_offset < 90L) |
          (.data$infection_type != "1" & !.data$implant & .data$ssi_offset < 30L)),
        FALSE)) |>
    dplyr::group_by(
      .data$patient_key, .data$enrollment_key, .data$event_key,
      .data$infection_type) |>
    dplyr::summarise(any_covering = any(.data$covered), .groups = "drop") |>
    dplyr::filter(!.data$any_covering) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 19L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = "infection_type") |>
    dplyr::mutate(
      rule_id        = 19L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Rules 22–24 share one shape: a procedure code on the surgery form that is
# not a syntactically valid ICHI code. Only the grammar is checked, since the
# classification itself is not bundled; the code travels under one name
# whichever of the three slots it came from.
.rule_invalid_ichi_code <- function(x, exceptions, rule_id, code_col)
{
  check_neoipcr_ds(x)
  if (!all(c("procedure_description", code_col) %in% names(x$surgeryData)))
    return(.rule_skipped(
      rule_id, sprintf("the surgery form's procedure description and %s", code_col)))
  id <- rule_id

  x$events |>
    dplyr::filter(.data$event_type_key == "pro") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(
      x$surgeryData |>
        dplyr::select("event_key", "procedure_description",
                      "procedure_code" = tidyselect::all_of(code_col)),
      dplyr::join_by("event_key")) |>
    dplyr::filter(!is.na(.data$procedure_code) &
                  !is_valid_ichi_code(.data$procedure_code)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, id),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("procedure_description", "procedure_code")) |>
    dplyr::mutate(
      rule_id        = id,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# Find surgical procedures whose main procedure code is not a valid ICHI code.
validation_rule_22 <- function(x, exceptions)
  .rule_invalid_ichi_code(x, exceptions, 22L, "main_procedure_code")

# Find surgical procedures whose first side procedure code is not a valid
# ICHI code.
validation_rule_23 <- function(x, exceptions)
  .rule_invalid_ichi_code(x, exceptions, 23L, "side_procedure_code_1")

# Find surgical procedures whose second side procedure code is not a valid
# ICHI code.
validation_rule_24 <- function(x, exceptions)
  .rule_invalid_ichi_code(x, exceptions, 24L, "side_procedure_code_2")
