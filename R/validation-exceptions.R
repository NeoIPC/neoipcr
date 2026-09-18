#' Read a validation exception list
#'
#' Reads the records a data manager has reviewed and wants kept despite the
#' validation rule that flags them, from the CSV file the Validation Report
#' and the reporting tools exchange. Its columns are `RULE_ID`,
#' `DEPARTMENT_CODE`, `NEOIPC_PATIENT_ID`, `ENROLMENT_DATE`, `EVENT_TYPE` and
#' `EVENT_DATE`. A record is written at the level of the rule it names, as
#' the "Context fields" section of [validate()] lists it: for rule 1 the
#' patient alone, with `ENROLMENT_DATE`, `EVENT_TYPE` and `EVENT_DATE` empty;
#' for an enrolment-level rule the patient and `ENROLMENT_DATE`, with the
#' event columns empty; for an event-level rule the event's type (one the
#' rule concerns) and date as well. `DEPARTMENT_CODE` may be absent, or left
#' empty throughout, when the list covers a single department. Dates are read
#' in ISO 8601 form.
#'
#' @param path Path to the CSV file.
#'
#' @returns A tibble with one row per exception record: `RULE_ID` (integer),
#'  `NEOIPC_PATIENT_ID` and `EVENT_TYPE` (character), `ENROLMENT_DATE` and
#'  `EVENT_DATE` (`Date`), and `DEPARTMENT_CODE` (character) when the file
#'  carries it with a value — the shape [dhis2_dataset_options()] accepts as
#'  `include_invalid_patients` and [validate()] as `exceptions`. A path that
#'  is not a file, a file that lacks a record column, holds a row with the
#'  wrong number of fields or a value that does not parse, names a rule
#'  outside [validation_rule_ids()], or holds a record written at another
#'  level than its rule's, is an error of class
#'  `neoipcr_invalid_exception_list`.
#' @family validation
#' @export
read_validation_exceptions <- function(path)
{
  check_string(path)
  if (!file.exists(path) || dir.exists(path))
    rlang::abort(c(
      "The validation exception file does not exist.",
      x = sprintf("`path` is \"%s\".", path)),
      class = "neoipcr_invalid_exception_list")
  header <- sprintf(
    "The validation exception file \"%s\" does not hold exception records.", path)

  # Every column arrives as text and is parsed below, where a value that
  # does not parse can be named; readr's own guessing would coerce a whole
  # column silently on the first bad row. Its parsing problems are read off
  # `problems()` rather than left as the warning it also raises.
  ex <- suppressWarnings(readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE))
  ragged <- readr::problems(ex)
  if (nrow(ragged) > 0L)
    rlang::abort(c(
      header,
      x = sprintf("Line %d has %s where the header has %s.",
                  ragged$row[1], ragged$actual[1], ragged$expected[1])),
      class = "neoipcr_invalid_exception_list")
  missing_cols <- setdiff(.exception_list_cols, names(ex))
  if (length(missing_cols) > 0L)
    rlang::abort(c(
      header,
      x = paste0("Missing column(s): ", paste(missing_cols, collapse = ", "), "."),
      i = paste0("An exception record carries ", paste(.exception_list_cols, collapse = ", "),
                 " (and DEPARTMENT_CODE when more than one department is imported).")),
      class = "neoipcr_invalid_exception_list")

  parse <- function(values, parser, column, what) {
    parsed <- suppressWarnings(parser(values))
    bad <- readr::problems(parsed)
    if (nrow(bad) > 0L) {
      shown <- unique(bad$actual)
      rlang::abort(c(
        header,
        x = sprintf("`%s` holds %d value(s) that cannot be read as %s: %s.",
                    column, nrow(bad), what,
                    paste(shown[seq_len(min(3L, length(shown)))], collapse = ", "))),
        class = "neoipcr_invalid_exception_list")
    }
    parsed
  }
  # Assigned outside `dplyr::mutate()`, which rethrows an error raised in
  # one of its expressions under its own class and would hide the documented
  # class from a caller's handler.
  ex$RULE_ID        <- parse(ex$RULE_ID, readr::parse_integer, "RULE_ID", "a whole number")
  ex$ENROLMENT_DATE <- parse(ex$ENROLMENT_DATE, readr::parse_date, "ENROLMENT_DATE", "a date")
  ex$EVENT_DATE     <- parse(ex$EVENT_DATE, readr::parse_date, "EVENT_DATE", "a date")
  check_exception_list(ex, header)
}

#' Resolve an exception list onto the records of a dataset
#'
#' Maps each exception record, written in terms of patient id, department
#' code and dates, onto the integer keys of the dataset it applies to. A
#' record resolves as a whole: the patient, and the enrolment and event when
#' the record names them, must all be in the dataset, or every key is `NA`
#' and the record exempts nothing — so a mistyped event date cannot exempt
#' the enrolment it belongs to. A record whose values fit several dataset
#' records (two enrolments of one patient on one day, two procedures of one
#' enrolment on one day) resolves to each of them, since nothing a list can
#' say tells them apart. [validate()] resolves a list itself; call this to
#' see which records a list actually reaches.
#'
#' @param x A `neoipcr_ds` imported with the full patient, enrollment and
#'  event tiers and with the patient id (`"id"` in `patient_columns`; an
#'  import given the list as `include_invalid_patients` keeps it by itself),
#'  since the records are matched by patient id and joined through the
#'  enrollments' patient link and the events' type; and with a department
#'  tier: `"pseudo"` suffices for a single department, while more than one
#'  department needs `include_department = "full"`, since the records are
#'  then matched by department code as well. A list carrying
#'  `DEPARTMENT_CODE` is matched by it whenever the dataset holds the codes.
#' @param exceptions The list as [read_validation_exceptions()] returns it.
#'
#' @returns A tibble with `rule_id`, `patient_key`, `enrollment_key` and
#'  `event_key` (`department_key` too when the records were matched by
#'  department code): one row per record, or one per dataset record it fits;
#'  `NA` throughout where a record did not resolve, and on the keys below
#'  its rule's level.
#' @family validation
#' @export
resolve_validation_exceptions <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  exceptions <- check_exception_list(
    exceptions, "`exceptions` must be a data frame of exception records.")
  assert_options_for(x, required = list(
    include_enrollment = "full",
    include_event      = "full"
  ), fn_name = "resolve_validation_exceptions")
  if (!"patient_id" %in% names(x$patients))
    rlang::abort(c(
      "An exception list is matched by patient id, which this dataset does not carry.",
      i = "Import with `include_patient = \"full\"` and `\"id\"` in `patient_columns`, or pass the list to the import as `include_invalid_patients`."),
      class = "neoipcr_validation_needs_facts")

  # The import resolves before it strips its internal maps, which carry the
  # department codes whatever `include_department` says; a returned dataset
  # carries its departments on `metadata$departments` — none under the
  # `"no"` tier, and with their codes under the full tier only.
  departments <- x$metadata$.departments_internal_map
  if (is.null(departments))
    departments <- x$metadata$departments
  if (is.null(departments) || ncol(departments) == 0L)
    rlang::abort(c(
      "An exception list needs a department tier: its records are matched within their department.",
      x = "This dataset carries no departments.",
      i = "Import with `include_department = \"pseudo\"` (one department) or `\"full\"`."),
      class = "neoipcr_validation_needs_facts")
  if (nrow(departments) > 1L) {
    if (!"DEPARTMENT_CODE" %in% names(exceptions))
      rlang::abort(c(
        "The exception list needs `DEPARTMENT_CODE` when more than one department is imported.",
        x = sprintf("%d departments were imported.", nrow(departments)),
        i = "Add the column, or narrow the import to one department with `department_filter`."),
        class = "neoipcr_invalid_exception_list")
    if (!"code" %in% names(departments))
      rlang::abort(c(
        "An exception list for more than one department is matched by department code, which this dataset does not carry.",
        i = "Import with `include_department = \"full\"`."),
        class = "neoipcr_validation_needs_facts")
  }
  transform_user_exceptions(exceptions, x, departments)
}

# The columns an exception record carries in the form a user writes it (see
# `read_validation_exceptions()`); `DEPARTMENT_CODE` joins in addition when
# more than one department is imported.
.exception_list_cols <- c(
  "RULE_ID", "NEOIPC_PATIENT_ID", "ENROLMENT_DATE", "EVENT_TYPE", "EVENT_DATE")

# The event types an exception record may name, as
# `resolve_validation_exceptions()` levels them.
.exception_event_types <- c("adm", "pro", "bsi", "nec", "ssi", "hap", "end")

# Whether `include_invalid_patients` carries an exception list rather than a
# switch. The full patient tier keeps `patient_id` whenever it does, whatever
# `patient_columns` says, so the list can be matched; `import_dhis2()`
# requires that tier for a list.
has_exception_list <- function(dataset_options)
  is.data.frame(dataset_options$include_invalid_patients)

# Assert that `ex` is an exception list of the shape the records join on:
# the record columns, `Date` dates (a `POSIXct` is refused rather than
# cast, since one carrying a time of day would silently match nothing
# against the records' dates), a rule id that names a registered rule, a patient id and a
# department code (where the column is present) on every record — a blank
# one would match nothing and exempt nothing in silence — and an event type
# from the stage vocabulary (case does not matter; `NA` names an
# enrolment-level record together with an `NA` event date, and a record that
# names an event names its enrolment date too). `header` opens the message,
# since the same shape is refused from an option and from a file.
check_exception_list <- function(ex, header)
{
  missing_cols <- if (is.data.frame(ex))
    setdiff(.exception_list_cols, names(ex))
  else
    character()
  if (!is.data.frame(ex) || length(missing_cols) > 0L)
    rlang::abort(c(
      header,
      x = if (is.data.frame(ex))
            paste0("Missing column(s): ", paste(missing_cols, collapse = ", "), ".")
          else
            paste0("Got ", obj_type_friendly(ex), "."),
      i = paste0("An exception record carries ", paste(.exception_list_cols, collapse = ", "),
                 " (and DEPARTMENT_CODE when more than one department is imported).")),
      class = "neoipcr_invalid_exception_list")

  event_types <- tolower(as.character(ex$EVENT_TYPE))
  wrong <- c(
    if (!inherits(ex$ENROLMENT_DATE, "Date")) "`ENROLMENT_DATE` is not a `Date`",
    if (!inherits(ex$EVENT_DATE, "Date")) "`EVENT_DATE` is not a `Date`",
    .rule_id_problem(ex$RULE_ID, "RULE_ID"),
    if (!is.character(ex$NEOIPC_PATIENT_ID)) "`NEOIPC_PATIENT_ID` is not character"
    else if (any(.blank(ex$NEOIPC_PATIENT_ID))) "`NEOIPC_PATIENT_ID` is empty on some record",
    if ("DEPARTMENT_CODE" %in% names(ex) && !is.character(ex$DEPARTMENT_CODE))
      "`DEPARTMENT_CODE` is not character"
    else if ("DEPARTMENT_CODE" %in% names(ex) && any(.blank(ex$DEPARTMENT_CODE)) &&
             !all(.blank(ex$DEPARTMENT_CODE)))
      "`DEPARTMENT_CODE` is empty on some record (fill it on every record, or leave it empty throughout for a single-department list)",
    if (!all(is.na(event_types) | event_types %in% .exception_event_types))
      paste0("`EVENT_TYPE` outside ", paste(.exception_event_types, collapse = "/"), " or `NA`"),
    if (inherits(ex$EVENT_DATE, "Date") && any(is.na(event_types) != is.na(ex$EVENT_DATE)))
      "`EVENT_TYPE` and `EVENT_DATE` not both set or both `NA` (an enrollment-level record has neither)")
  if (length(wrong) > 0L)
    rlang::abort(c(
      "An exception list's columns must be of the types the records join on.",
      rlang::set_names(wrong, rep("x", length(wrong)))),
      class = "neoipcr_invalid_exception_list")

  misplaced <- .record_level_problems(ex, event_types)
  if (length(misplaced) > 0L)
    rlang::abort(c(
      "An exception record is written at the level of the rule it names.",
      rlang::set_names(misplaced, rep("x", length(misplaced))),
      i = "The \"Context fields\" section of `?validate` lists each rule's level and event types."),
      class = "neoipcr_invalid_exception_list")

  # A `DEPARTMENT_CODE` column left empty throughout is the single-department
  # list written in the six-column shape the tools exchange; it says nothing
  # and is treated as absent. A list without records keeps the column: the
  # empty template exempts nothing and is valid for any number of
  # departments.
  if (nrow(ex) > 0L && "DEPARTMENT_CODE" %in% names(ex) &&
      all(.blank(ex$DEPARTMENT_CODE)))
    ex <- ex |> dplyr::select(!"DEPARTMENT_CODE")

  ex
}

# A character value that would match no record: `NA`, or nothing but
# whitespace. readr reads an empty CSV field as `NA`; a data frame built in
# code can carry the empty string instead.
.blank <- function(x)
  is.na(x) | !nzchar(trimws(x))

# Whether a key or id column holds nothing but whole numbers within R's
# integer range and `NA` — what `as.integer()` carries over unchanged; a
# fraction would be truncated onto another record's key, an infinity, a
# `NaN` or a value beyond the range onto `NA`. A column of bare `NA`s is
# logical and passes; an `NA` of another type does not, since it could not
# be bound onto the integer keys.
.whole_or_na <- function(x)
  (is.numeric(x) &&
     all((is.na(x) & !is.nan(x)) |
         (is.finite(x) & x == round(x) & abs(x) <= .Machine$integer.max))) ||
    (is.logical(x) && all(is.na(x)))

# Why `ids` cannot name rules, or `NULL` when every one does: a rule id must
# be a finite whole number that `validation_rule_ids()` lists, whichever
# form the list arrives in. `column` names the column in the message.
.rule_id_problem <- function(ids, column)
{
  if (!is.numeric(ids))
    sprintf("`%s` is not numeric", column)
  else if (anyNA(ids) || !.whole_or_na(ids))
    sprintf("`%s` is empty or not a whole number on some record", column)
  else if (!all(ids %in% validation_rule_ids()))
    sprintf("`%s` names rules that do not exist: %s", column,
            paste(sort(unique(setdiff(ids, validation_rule_ids()))), collapse = ", "))
}

# Which records of `ex` — whose rule ids and column types have passed — are
# not written at the level of the rule they name, as `validation_rules`
# declares it, each as a sentence naming the rule ids concerned. A record
# at another level would resolve keys its rule never joins on and exempt
# nothing, or name an event of a type the rule does not look at.
.record_level_problems <- function(ex, event_types)
{
  ids <- ex$RULE_ID
  level <- unname(.rule_levels()[as.character(ids)])
  has_enrolment <- !is.na(ex$ENROLMENT_DATE)
  has_event     <- !is.na(event_types)
  type_fits <- vapply(
    seq_along(ids),
    \(i) !has_event[i] || event_types[i] %in% .rule_event_types(ids[i]),
    logical(1))
  rules_where <- function(cond)
    paste(sort(unique(ids[cond])), collapse = ", ")
  c(
    if (any(level == "patient" & has_enrolment))
      sprintf("rule %s concerns the patient alone: its records leave `ENROLMENT_DATE` empty",
              rules_where(level == "patient" & has_enrolment)),
    if (any(level != "patient" & !has_enrolment))
      sprintf("rule(s) %s are recorded on the enrolment or an event: their records name `ENROLMENT_DATE`",
              rules_where(level != "patient" & !has_enrolment)),
    if (any(level != "event" & has_event))
      sprintf("rule(s) %s are not recorded on an event: their records leave `EVENT_TYPE` and `EVENT_DATE` empty",
              rules_where(level != "event" & has_event)),
    if (any(level == "event" & !has_event))
      sprintf("rule(s) %s are recorded on an event: their records name `EVENT_TYPE` and `EVENT_DATE`",
              rules_where(level == "event" & !has_event)),
    if (!all(type_fits))
      sprintf("rule(s) %s do not concern the `EVENT_TYPE` their records name",
              rules_where(!type_fits)))
}

# Map the records onto the dataset's keys. Every join matches on the values
# a record gives, an `NA` matching nothing, and a record then resolves as a
# whole or not at all. Only the record columns take part, so a stray column
# that happens to share a key's name cannot derail a join.
transform_user_exceptions <- function(ex, ds, departments)
{
  ex <- ex |>
    dplyr::select(tidyselect::any_of(c(.exception_list_cols, "DEPARTMENT_CODE"))) |>
    dplyr::mutate(
      event_type_key = factor(
        tolower(.data$EVENT_TYPE),
        levels = .exception_event_types),
      .keep = "unused")

  # With the codes at hand a record is matched within its department, so a
  # list written for another department exempts nothing here even where the
  # patient ids coincide; without them (one department under the pseudo
  # tier) the column has nothing to match against and is ignored.
  by_department <- "DEPARTMENT_CODE" %in% names(ex) && "code" %in% names(departments)
  if (by_department)
    ex <- ex |>
      dplyr::left_join(
        departments |>
          dplyr::select("department_key", "code"),
        dplyr::join_by("DEPARTMENT_CODE" == "code"),
        na_matches = "never") |>
      dplyr::left_join(
        ds$patients |>
          dplyr::select("department_key", "patient_key", "patient_id"),
        dplyr::join_by("department_key", "NEOIPC_PATIENT_ID" == "patient_id"),
        na_matches = "never")
  else
    ex <- ex |>
      dplyr::left_join(
        ds$patients |>
          dplyr::select("patient_key", "patient_id"),
        dplyr::join_by("NEOIPC_PATIENT_ID" == "patient_id"),
        na_matches = "never")

  ex |>
    dplyr::left_join(
      ds$enrollments |>
        dplyr::select("patient_key", "enrollment_key", "enrolledAt"),
      dplyr::join_by("patient_key", "ENROLMENT_DATE" == "enrolledAt"),
      na_matches = "never") |>
    dplyr::left_join(
      ds$events |>
        dplyr::select("event_key", "enrollment_key", "event_type_key", "occurredAt"),
      dplyr::join_by("enrollment_key", "event_type_key", "EVENT_DATE" == "occurredAt"),
      na_matches = "never") |>
    dplyr::mutate(
      resolved = !is.na(.data$patient_key) &
        (is.na(.data$ENROLMENT_DATE) | !is.na(.data$enrollment_key)) &
        (is.na(.data$event_type_key) | !is.na(.data$event_key))) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("department_key", "patient_key", "enrollment_key", "event_key")),
        \(key) dplyr::if_else(.data$resolved, key, NA_integer_)),
      rule_id = as.integer(.data$RULE_ID)) |>
    dplyr::select(
      "rule_id", tidyselect::any_of("department_key"),
      "patient_key", "enrollment_key", "event_key")
}
