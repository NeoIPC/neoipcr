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
    # A procedure covers an infection from its own day for 30 days whatever
    # the infection's type, and for 90 days when an implant was inserted and
    # the infection is not superficial. An infection whose type was not
    # recorded takes the longer window where the implant allows it: the
    # missing type is a completeness problem, not a reason to place the
    # infection outside a window it may well be in. A procedure without a
    # date covers nothing, since no window can be placed on it.
    dplyr::mutate(
      ssi_offset = as.integer(.data$ssiOccurredAt - .data$surgeryOccurredAt),
      covered    = tidyr::replace_na(
        .data$ssi_offset >= 0L & (
          .data$ssi_offset < 30L |
          (.data$implant & .data$ssi_offset < 90L &
             (is.na(.data$infection_type) | .data$infection_type != "1"))),
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

# --- Rules 66–69: the placement of a procedure among the groups -------------
# These four ask the partners about a procedure the group rules cannot settle
# from its codes alone, or whose recorded codes say something the form's
# other answers contradict. Each reads the whole procedure — the main code
# and both side codes — beside what it needs of the rest of the form.

.rule_procedure_code_cols <-
  c("main_procedure_code", "side_procedure_code_1", "side_procedure_code_2")

# The procedure events with their codes and any further form columns a rule
# reads, or a skip when the dataset lacks the main code or one of those
# further columns. An absent side-code column stands in as a missing value
# in every row, as in the surgical counts: a dataset from a form version
# without them is counted and asked about alike.
.rule_procedures <- function(x, rule_id, extra_cols = character()) {
  need <- c("main_procedure_code", extra_cols)
  if (!all(need %in% names(x$surgeryData)))
    return(.rule_skipped(
      rule_id,
      sprintf("the surgery form's %s", paste(need, collapse = ", "))))
  surgeryData <- .surgery_code_columns(x$surgeryData)
  x$events |>
    dplyr::filter(.data$event_type_key == "pro") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(
      surgeryData |>
        dplyr::select(
          tidyselect::all_of(c("event_key", .rule_procedure_code_cols,
                               extra_cols))),
      dplyr::join_by("event_key"))
}

# The codes of one procedure, main first, with the absent ones left out.
.rule_procedure_codes_of <- function(df, i) {
  codes <- vapply(.rule_procedure_code_cols,
                  \(col) df[[col]][[i]], character(1))
  codes[!is.na(codes)]
}

# Find class I procedures whose own procedure group opens a tract. A class I
# wound of such a procedure is clean-contaminated by the protocol's own
# reading, so the wound class and the codes do not agree: either the wound
# class is too low or the codes are not the operation that was done.
validation_rule_66 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  df <- .rule_procedures(x, 66L, "wound_class")
  if (is.null(df)) return(df)
  df$found <- vapply(seq_len(nrow(df)), function(i) {
    if (!identical(as.character(df$wound_class[[i]]), "1"))
      return(NA_character_)
    .rule_procedure_opening_code(df, i)
  }, character(1))
  df |>
    dplyr::filter(!is.na(.data$found)) |>
    dplyr::anti_join(.rule_exceptions(exceptions, 66L), dplyr::join_by("event_key")) |>
    dplyr::rename(procedure_code = "found") |>
    tidyr::nest(context = c("wound_class", "procedure_code")) |>
    dplyr::mutate(
      rule_id = 66L,
      patient_key = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key = .data$event_key,
      context = .data$context,
      .keep = "none")
}

# The first stem of the procedure that opens a tract of the procedure's own
# group, or `NA`. Only a recorded stem has a tract; the group is the one the
# procedure's codes together take.
.rule_procedure_opening_code <- function(df, i) {
  # The group the procedure's codes together take, held pending codes
  # included: a procedure the classifier places nowhere has no own group
  # whose tract could open, and rule 68 already asks about it.
  g <- as.character(get_procedure_group(df$main_procedure_code[[i]],
                                        df$side_procedure_code_1[[i]],
                                        df$side_procedure_code_2[[i]]))
  if (is.na(g) || !(g %in% .procedure_group_levels)) return(NA_character_)
  codes <- .rule_procedure_codes_of(df, i)
  stems <- unlist(lapply(codes, .procedure_stems_of_one), use.names = FALSE)
  if (length(stems) == 0L) return(NA_character_)
  idx <- match(stems, .procedure_code_table$procedure_code)
  recorded <- !is.na(idx)
  code_group <- .procedure_group_for_stem(stems)
  tract <- rep(NA_character_, length(stems))
  tract[recorded] <- .procedure_code_table$tract[idx[recorded]]
  hits <- which(code_group == g & !is.na(tract) &
                  tract %in% .procedure_opening_tracts)
  if (length(hits) == 0L) NA_character_ else stems[[hits[[1]]]]
}

# Find procedures an approach code alone places. The codes name the road
# taken — a laparoscopy, a thoracotomy, a craniotomy — and not the operation
# done through it: the procedure rests on an incision with nothing behind
# it, which asks whether the operation itself was left out of the codes.
validation_rule_67 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  df <- .rule_procedures(x, 67L)
  if (is.null(df)) return(df)
  df$found <- vapply(seq_len(nrow(df)),
                     \(i) .rule_procedure_approach_only_code(df, i),
                     character(1))
  df |>
    dplyr::filter(!is.na(.data$found)) |>
    dplyr::anti_join(.rule_exceptions(exceptions, 67L), dplyr::join_by("event_key")) |>
    dplyr::rename(procedure_code = "found") |>
    tidyr::nest(context = "procedure_code") |>
    dplyr::mutate(
      rule_id = 67L,
      patient_key = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key = .data$event_key,
      context = .data$context,
      .keep = "none")
}

# The first approach stem when approach codes and codes that are no surgical
# procedure are all the procedure holds, else `NA`.
.rule_procedure_approach_only_code <- function(df, i) {
  # A held pending code means the procedure is no approach code's alone:
  # the classifier places it in no group, and rule 68 asks about the code.
  if (.procedure_holds_procedure(df$main_procedure_code[[i]],
                                 df$side_procedure_code_1[[i]],
                                 df$side_procedure_code_2[[i]]))
    return(NA_character_)
  codes <- .rule_procedure_codes_of(df, i)
  stems <- unlist(lapply(codes, .procedure_stems_of_one), use.names = FALSE)
  if (length(stems) == 0L) return(NA_character_)
  idx <- match(stems, .procedure_code_table$procedure_code)
  recorded <- !is.na(idx)
  groups <- .procedure_group_for_stem(stems)
  placed <- groups %in% .procedure_group_levels
  is_approach <- logical(length(stems))
  is_approach[recorded] <- .procedure_code_table$approach[idx[recorded]]
  if (any(placed & !is_approach)) return(NA_character_)
  approach_hits <- which(placed & is_approach)
  if (length(approach_hits) == 0L) NA_character_ else stems[[approach_hits[[1]]]]
}

# Find a code no group takes, and a code whose group the partners queried.
# A valid code no rule places, or one whose query is still pending, says the
# procedure cannot be grouped from its codes; the peritoneal dialysis access
# counts as abdominal unless the procedure holds a shunt code beside it, and
# the umbilical vein repair was recorded only with an implant and is probably
# a patch closure: each queried code is asked about whenever the procedure
# itself does not settle it.
validation_rule_68 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  df <- .rule_procedures(x, 68L)
  if (is.null(df)) return(df)
  df$found <- vapply(seq_len(nrow(df)),
                     \(i) .rule_procedure_ungrouped_code(df, i),
                     character(1))
  df |>
    dplyr::filter(!is.na(.data$found)) |>
    dplyr::anti_join(.rule_exceptions(exceptions, 68L), dplyr::join_by("event_key")) |>
    dplyr::rename(procedure_code = "found") |>
    tidyr::nest(context = "procedure_code") |>
    dplyr::mutate(
      rule_id = 68L,
      patient_key = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key = .data$event_key,
      context = .data$context,
      .keep = "none")
}

# The first code no group takes — a pending stem or a stem the placement
# table and the axes alike leave out — else the first queried code the
# procedure does not settle, else `NA`. A value no ICHI grammar reads is
# rules 22 to 24's to report, an error on the same form: this warning asks
# of valid codes only.
.rule_procedure_ungrouped_code <- function(df, i) {
  codes <- .rule_procedure_codes_of(df, i)
  if (length(codes) == 0L) return(NA_character_)
  stems <- unlist(lapply(codes, .procedure_stems_of_one), use.names = FALSE)
  if (length(stems) == 0L) return(NA_character_)
  ungrouped <- which(.procedure_group_for_stem(stems) == "no_group")
  if (length(ungrouped) > 0L) return(stems[[ungrouped[[1]]]])
  idx <- match(stems, .procedure_code_table$procedure_code)
  conditional_at <- which(!is.na(idx) & .procedure_code_table$conditional[idx])
  shunt <- any(stems %in% .procedure_shunt_codes)
  for (at in conditional_at) {
    # A shunt code settles the peritoneal access: beside it the code names
    # the shunt's peritoneal end. The umbilical vein repair's query is about
    # what was done, which no other code of the procedure answers.
    if (identical(stems[[at]], "KMA.FA.AA") && shunt) next
    return(stems[[at]])
  }
  NA_character_
}

# Find a procedure carrying one of the percutaneous abdominal drainages.
# Both are no surgical procedure; the warning asks what the form describes
# beside them, since an open drainage coded percutaneous belongs among the
# abdominal procedures and a percutaneous one does not belong among
# procedures at all.
validation_rule_69 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  df <- .rule_procedures(x, 69L, "procedure_description")
  if (is.null(df)) return(df)
  asked <- c("KMA.JB.AE", "PAK.JB.AE")
  df$found <- vapply(seq_len(nrow(df)), function(i) {
    stems <- unlist(lapply(.rule_procedure_codes_of(df, i),
                           .procedure_stems_of_one), use.names = FALSE)
    hits <- which(stems %in% asked)
    if (length(hits) == 0L) NA_character_ else stems[[hits[[1]]]]
  }, character(1))
  df |>
    dplyr::filter(!is.na(.data$found)) |>
    dplyr::anti_join(.rule_exceptions(exceptions, 69L), dplyr::join_by("event_key")) |>
    dplyr::rename(procedure_code = "found") |>
    tidyr::nest(context = c("procedure_description", "procedure_code")) |>
    dplyr::mutate(
      rule_id = 69L,
      patient_key = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key = .data$event_key,
      context = .data$context,
      .keep = "none")
}
