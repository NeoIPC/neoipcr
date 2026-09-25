# The registry of validation rules, in id order. Each entry names the rule,
# the level its finding is recorded on — the key a finding is identified by
# and an exception record is matched on — the event types a finding may
# name (the types an event-level rule concerns; for an enrolment-level rule
# the form its finding is shown on, which a record may name or leave
# empty), the fields the rule records in a finding's `context`, the
# function that implements it, and `dated` on a rule that measures a
# record's age against the date the data was read, which `validate()` then
# passes as a third argument. A finding is data — keys and the values a
# rule compared — never a sentence: the prose belongs to whichever document
# renders the finding, where it can be localized, and the declared fields
# are the contract its sentences are written against; `validate()` refuses
# a finding whose fields differ from the declaration.
.frame_fields <- function(type)
  c("enrolledAt", "admOccurredAt", "endOccurredAt", paste0(type, "OccurredAt"))

.completion_fields <- function(type)
  c("enrollment_status", "end_status", paste0(type, "_status"))

validation_rules <- list(
  list(id = 1L,  level = "patient",    context = character(),
       fun = validation_rule_1),
  list(id = 2L,  level = "enrollment", event_types = "end",
       context = character(), fun = validation_rule_2),
  list(id = 3L,  level = "enrollment", event_types = "adm",
       context = c("enrolledAt", "occurredAt"), fun = validation_rule_3),
  list(id = 4L,  level = "enrollment", event_types = "end",
       context = c("admOccurredAt", "endOccurredAt"), fun = validation_rule_4),
  list(id = 5L,  level = "enrollment", event_types = "adm", context = "status",
       fun = validation_rule_5),
  list(id = 6L,  level = "enrollment", event_types = "end", context = "status",
       fun = validation_rule_6),
  list(id = 7L,  level = "event", event_types = "bsi",
       context = .completion_fields("bsi"), fun = validation_rule_7),
  list(id = 8L,  level = "event", event_types = "nec",
       context = .completion_fields("nec"), fun = validation_rule_8),
  list(id = 9L,  level = "event", event_types = "hap",
       context = .completion_fields("hap"), fun = validation_rule_9),
  list(id = 10L, level = "event", event_types = "pro",
       context = .completion_fields("pro"), fun = validation_rule_10),
  list(id = 11L, level = "event", event_types = "ssi",
       context = .completion_fields("ssi"), fun = validation_rule_11),
  list(id = 12L, level = "event", event_types = "bsi",
       context = .frame_fields("bsi"), fun = validation_rule_12),
  list(id = 13L, level = "event", event_types = "nec",
       context = .frame_fields("nec"), fun = validation_rule_13),
  list(id = 14L, level = "event", event_types = "hap",
       context = .frame_fields("hap"), fun = validation_rule_14),
  list(id = 15L, level = "event", event_types = "pro",
       context = .frame_fields("pro"), fun = validation_rule_15),
  list(id = 17L, level = "enrollment",
       context = c("enrolledAt_this", "endOccurredAt_this",
                   "enrolledAt_other", "endOccurredAt_other"),
       fun = validation_rule_17),
  list(id = 18L, level = "enrollment", event_types = "end",
       context = c("patient_days", "patient_days_calculated"),
       fun = validation_rule_18),
  list(id = 19L, level = "event", event_types = "ssi",
       context = "infection_type", fun = validation_rule_19),
  list(id = 20L, level = "event", event_types = c("bsi", "nec", "hap", "ssi"),
       context = c("index", "secondary_bsi", "name"), fun = validation_rule_20),
  list(id = 21L, level = "enrollment", event_types = "end",
       context = c("ab_substance_days", "ab_days"), fun = validation_rule_21),
  list(id = 22L, level = "event", event_types = "pro",
       context = c("procedure_description", "procedure_code"),
       fun = validation_rule_22),
  list(id = 23L, level = "event", event_types = "pro",
       context = c("procedure_description", "procedure_code"),
       fun = validation_rule_23),
  list(id = 24L, level = "event", event_types = "pro",
       context = c("procedure_description", "procedure_code"),
       fun = validation_rule_24),
  list(id = 25L, level = "enrollment", context = character(),
       fun = validation_rule_25),
  list(id = 26L, level = "enrollment", context = character(),
       fun = validation_rule_26),
  list(id = 27L, level = "event", event_types = "bsi",
       context = c("dol", "dol_calc"), fun = validation_rule_27),
  list(id = 28L, level = "event", event_types = "bsi",
       context = c("los", "los_calc"), fun = validation_rule_28),
  list(id = 29L, level = "event", event_types = "bsi",
       context = "dol", fun = validation_rule_29),
  list(id = 30L, level = "event", event_types = "bsi",
       context = "dos", fun = validation_rule_30),
  list(id = 31L, level = "event", event_types = "hap",
       context = c("dol", "dol_calc"), fun = validation_rule_31),
  list(id = 32L, level = "event", event_types = "hap",
       context = c("los", "los_calc"), fun = validation_rule_32),
  list(id = 33L, level = "event", event_types = "hap",
       context = "dol", fun = validation_rule_33),
  list(id = 34L, level = "event", event_types = "hap",
       context = "dos", fun = validation_rule_34),
  list(id = 35L, level = "event", event_types = "nec",
       context = c("dol", "dol_calc"), fun = validation_rule_35),
  list(id = 36L, level = "event", event_types = "nec",
       context = c("los", "los_calc"), fun = validation_rule_36),
  list(id = 37L, level = "event", event_types = "nec",
       context = "dol", fun = validation_rule_37),
  list(id = 38L, level = "event", event_types = "nec",
       context = "dos", fun = validation_rule_38),
  list(id = 39L, level = "event", event_types = "pro",
       context = c("dol", "dol_calc"), fun = validation_rule_39),
  list(id = 40L, level = "event", event_types = "pro",
       context = c("los", "los_calc"), fun = validation_rule_40),
  list(id = 41L, level = "event", event_types = "ssi",
       context = c("dol", "dol_calc"), fun = validation_rule_41),
  list(id = 42L, level = "event", event_types = "ssi",
       context = c("los", "los_calc"), fun = validation_rule_42),
  list(id = 43L, level = "enrollment", dated = TRUE,
       context = c("enrolledAt", "days_open"), fun = validation_rule_43),
  list(id = 44L, level = "enrollment", event_types = "end", dated = TRUE,
       context = c("enrolledAt", "days_open", "status"),
       fun = validation_rule_44))

# How long an enrolment may stay active after its enrolment date before
# rules 43 and 44 question it. A neonatal stay past four months is
# exceptional, so an enrolment still open then is most often a record nobody
# closed once the infant left — though it may still be admitted, which is
# why a finding asks for a look and an exception record keeps a genuine stay.
.open_enrolment_max_days <- 120L

# The whole days from an enrolment date to the reference date.
.days_open <- function(enrolled_at, as_of)
  as.integer(as_of - enrolled_at)

# The level of each rule, named by rule id, and the event types a rule's
# finding may name; `check_exception_list()` holds a record's shape to its
# rule's level through these.
.rule_levels <- function()
  rlang::set_names(
    vapply(validation_rules, \(r) r$level, character(1)),
    validation_rule_ids())

.rule_event_types <- function(rule_id)
  validation_rules[[match(rule_id, validation_rule_ids())]]$event_types

# `as_of` is a single `Date` or nothing; a caller is told so whatever rules
# it selected, rather than only when one of them reads the date.
.check_as_of <- function(as_of)
{
  if (!is.null(as_of) &&
      (!inherits(as_of, "Date") || length(as_of) != 1L || is.na(as_of)))
    rlang::abort("`as_of` must be a single `Date`.")
}

# The date the dated rules measure a record's age against: `as_of` when the
# caller gives one, else the calendar day of the DHIS2 server's own clock
# when the data was read, which the import records on
# `metadata$system$server_date` — the calendar the enrolment dates are on,
# so that the age is whole days on one calendar. A dataset carrying neither
# cannot run those rules.
.reference_date <- function(x, as_of = NULL)
{
  if (!is.null(as_of))
    return(as_of)
  stamp <- x$metadata$system$server_date
  if (is.null(stamp) || length(stamp) != 1L || is.na(stamp))
    rlang::abort(c(
      "The age of an open enrolment is measured against the date the data was read, which this dataset does not carry.",
      i = "An import records it on `metadata$system$server_date`; pass `as_of` to `validate()` otherwise."),
      class = "neoipcr_validation_needs_facts")
  # NEOIPC-PERMANENT(dataset-format): never replace this conversion by a
  # class check. A dataset restored from a serialization carries the day as
  # text, and such a file outlives every version of this package; refusing
  # the text would refuse the open-enrolment rules on every one of them.
  as.Date(stamp)
}

# The dataset slot that carries each infection or surgery event type's form
# data, for the rule families that run once per type.
.event_data_slot <- c(
  bsi = "sepsisData",
  hap = "pneumoniaData",
  nec = "necData",
  pro = "surgeryData",
  ssi = "ssiData")

# The status vocabularies the enrolment and event schemas declare
# (`schema-enrollments.R`, `schema-events.R`).
.enrollment_status_levels <- c("ACTIVE", "COMPLETED", "CANCELLED")
.event_status_levels <- c(
  "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED")

# Enrolments or events with their `status`. The column is imported only when
# `include_incomplete` names the entity; otherwise the request itself was
# filtered to completed records, so every row is completed by construction
# and the column is added saying so, rather than a rule reading its absence
# as "unknown".
.with_status <- function(records, levels)
{
  if ("status" %in% names(records))
    return(records)
  records |>
    dplyr::mutate(status = factor("COMPLETED", levels = levels))
}

# A rule that cannot run on this dataset — a narrower option tier left out a
# column it reads — says so on the log and contributes nothing, so the pass
# completes with the rules the dataset can support.
.rule_skipped <- function(rule_id, what)
{
  logger::log_warn(
    sprintf("Validation rule %d skipped: dataset lacks %s.", rule_id, what),
    namespace = "neoipcr")
  NULL
}

# Refuse a validation result whose `rules_skipped` attribute names a rule,
# for a caller that must not read a pass with a rule left out as complete —
# the import, whose full tiers give every rule its columns, so a skip there
# is a dataset that is not what the pass needs.
.assert_no_rule_skipped <- function(findings)
{
  skipped <- attr(findings, "rules_skipped")
  if (length(skipped) > 0L)
    rlang::abort(c(
      "The validation pass could not run every rule on this dataset.",
      x = sprintf("Rule(s) %s found no column to read; the log names it.",
                  paste(skipped, collapse = ", ")),
      i = "The pass needs the full enrollment and event tiers with every column they declare, and the events that are not completed whenever the enrolments that are not completed are requested."),
      class = "neoipcr_validation_rule_skipped")
  invisible(findings)
}

.exception_keys <- function()
  tibble::tibble(
    rule_id        = integer(),
    patient_key    = integer(),
    enrollment_key = integer(),
    event_key      = integer())

# The exception records addressed to one rule, in key form with every key
# column present. A rule anti-joins its findings on its natural key — the
# key of its level — so a record that is `NA` there exempts nothing. A
# record in the user's form is written at its rule's level, which
# `check_exception_list()` enforces, so once it has resolved it carries that
# key — with the event's key too when it named the form an enrolment-level
# finding is shown on, which the anti-join does not read; a key form built
# by hand may name another level and then exempts nothing.
.rule_exceptions <- function(exceptions, rule_id)
{
  if (is.null(exceptions))
    return(.exception_keys())
  id <- rule_id
  dplyr::bind_rows(.exception_keys(), exceptions) |>
    dplyr::filter(.data$rule_id == id)
}

# Whatever form the caller passed exceptions in, the rules read key form. A
# list in key form is checked the way the written form is: its rule ids must
# name rules, and its keys must be integers — a key form built by hand with
# a mistyped id would otherwise exempt nothing in silence.
.exceptions_in_key_form <- function(x, exceptions)
{
  if (is.null(exceptions))
    return(.exception_keys())
  if (is.data.frame(exceptions) && "RULE_ID" %in% names(exceptions))
    return(resolve_validation_exceptions(x, exceptions))
  if (!is.data.frame(exceptions) || !"rule_id" %in% names(exceptions))
    rlang::abort(c(
      "`exceptions` must be a data frame of exception records.",
      i = "Pass the list `read_validation_exceptions()` returns, or the key form `resolve_validation_exceptions()` returns."),
      class = "neoipcr_invalid_exception_list")

  key_cols <- c("department_key", "patient_key", "enrollment_key", "event_key")
  record_keys <- c("patient_key", "enrollment_key", "event_key")
  present <- intersect(key_cols, names(exceptions))
  not_integer <- present[!vapply(
    present,
    \(key) .whole_or_na(exceptions[[key]]),
    logical(1))]
  wrong <- c(
    .rule_id_problem(exceptions$rule_id, "rule_id"),
    # A record with no key at all could name nothing; an empty table is
    # the resolver's own shape for a list without records.
    if (nrow(exceptions) > 0L && !any(record_keys %in% names(exceptions)))
      sprintf("a record names its record through at least one of %s",
              paste0("`", record_keys, "`", collapse = ", ")),
    if (length(not_integer) > 0L)
      sprintf("%s must hold integer keys or `NA`",
              paste0("`", not_integer, "`", collapse = ", ")))
  if (length(wrong) > 0L)
    rlang::abort(c(
      "`exceptions` in key form must name existing rules through integer keys.",
      rlang::set_names(wrong, rep("x", length(wrong)))),
      class = "neoipcr_invalid_exception_list")

  dplyr::bind_rows(.exception_keys(), exceptions) |>
    dplyr::mutate(dplyr::across(c("rule_id", tidyselect::all_of(present)), as.integer))
}

#' Ids of the validation rules
#'
#' The integer ids of every rule [validate()] runs, in ascending order. A
#' consumer that lets its user choose rules, or that keeps a catalogue of
#' rule descriptions, checks itself against this list.
#'
#' @returns An integer vector.
#' @family validation
#' @export
validation_rule_ids <- function()
  vapply(validation_rules, \(r) r$id, integer(1))

#' Context fields of the validation rules
#'
#' The names of the fields each rule records in a finding's `context`, as the
#' "Context fields" section of [validate()] lists them. They are the contract
#' a consumer's sentences are written against, so a consumer that keeps a
#' template per rule checks its placeholders against this rather than against
#' a copy of the table.
#'
#' @returns A list named by rule id, each element a character vector of field
#'  names, empty for a rule that records none.
#' @family validation
#' @export
validation_rule_context_fields <- function()
  rlang::set_names(
    lapply(validation_rules, \(r) r$context),
    validation_rule_ids())

# The summary of a validation pass: one row per rule that flagged or
# exempted a record, with the rule's record kind and the distinct records
# it removed and the exception list exempted from it — a record a rule
# flags twice, as rule 20 does an event with two unknown pathogen names, is
# one record — and one row per record kind (`rule_id` `NA`) with the
# distinct records of that kind the findings concern: every finding
# concerns its patient, a finding of an enrolment- or event-level rule also
# concerns its enrolment, and a finding of an event-level rule also concerns
# its event. The `patients` row is thus the
# number of patients the pass removes, whatever level flagged them; the
# orphan removal that follows the pass is not the pass's doing and may drop
# more. A rule's record kind is the level the registry declares for it, so an
# enrolment-level rule that names the form it compared still counts
# enrolments, and its event is not among the events concerned. An exception
# keeps a record from the rule it names, not from the others, so a record
# exempted from one rule and flagged under another counts in both columns.
# Every record kind has its totals row, at zero when nothing of that kind
# was concerned.
.validation_summary <- function(removed, exempted)
{
  kinds   <- c(patient = "patients", enrollment = "enrollments",
               event = "events")
  kind_of <- rlang::set_names(
    unname(kinds[.rule_levels()]), as.character(validation_rule_ids()))

  counts <- function(findings, name) {
    f <- findings |>
      dplyr::mutate(
        record_kind = unname(kind_of[as.character(.data$rule_id)]))
    f$record_key <- dplyr::case_when(
      f$record_kind == "patients"    ~ f$patient_key,
      f$record_kind == "enrollments" ~ f$enrollment_key,
      .default = f$event_key)
    dplyr::bind_rows(
      f |>
        dplyr::group_by(.data$rule_id, .data$record_kind) |>
        dplyr::summarise(
          !!name := dplyr::n_distinct(.data$record_key, na.rm = TRUE),
          .groups = "drop"),
      tibble::tibble(
        rule_id     = NA_integer_,
        record_kind = unname(kinds),
        !!name := c(
          dplyr::n_distinct(f$patient_key, na.rm = TRUE),
          dplyr::n_distinct(
            f$enrollment_key[f$record_kind != "patients"], na.rm = TRUE),
          dplyr::n_distinct(
            f$event_key[f$record_kind == "events"], na.rm = TRUE))))
  }

  dplyr::full_join(
    counts(removed,  "n_removed"),
    counts(exempted, "n_exempted"),
    dplyr::join_by("rule_id", "record_kind")) |>
    dplyr::mutate(
      record_kind = factor(.data$record_kind, levels = unname(kinds)),
      n_removed   = tidyr::replace_na(.data$n_removed, 0L),
      n_exempted  = tidyr::replace_na(.data$n_exempted, 0L)) |>
    dplyr::arrange(.data$record_kind, .data$rule_id) |>
    dplyr::select("rule_id", "record_kind", "n_removed", "n_exempted")
}

#' Validate a NeoIPC dataset against the protocol's validation rules
#'
#' Runs every registered validation rule, or the subset named in `rules`, over
#' the dataset and returns the records each rule flags. [import_dhis2()] runs
#' it by default and removes the flagged patients from the dataset; call it
#' directly on a dataset imported with `include_invalid_patients = TRUE` to see
#' which records would be removed and why.
#'
#' A finding is data, never prose: the rule id, the keys that identify the
#' record, and the values the rule compared. The sentence a reader sees is
#' the consumer's, composed from the context fields listed below, so that it
#' is written and translated where the document is rendered.
#'
#' @param x A `neoipcr_ds` object imported with `include_patient` set to
#'  `"pseudo"` or `"full"` and `include_enrollment` and `include_event` set to
#'  `"full"`: the rules read the enrollments' patient link and the events'
#'  type, which the pseudonymized tiers do not carry. An exception list in
#'  the form a user writes needs the patient id and a department tier on
#'  top, as [resolve_validation_exceptions()] describes.
#' @param rules Integer vector of rule ids to run; `NULL` (the default) runs all
#'  of them. An id outside [validation_rule_ids()] is an error.
#' @param exceptions The records to exempt from the rule that flags them:
#'  either the list a user writes, as [read_validation_exceptions()] returns
#'  it, or its resolved key form as [resolve_validation_exceptions()] returns
#'  it (`rule_id`, `patient_key`, `enrollment_key`, `event_key`). `NULL`
#'  exempts nothing.
#' @param as_of The date the data was read, a `Date`, against which rules 43
#'  and 44 measure how long an enrolment has been open; `NULL` (the default)
#'  takes the calendar day of the DHIS2 server's own clock when the import
#'  read the data, recorded on `metadata$system$server_date` — the calendar
#'  the enrolment dates are on. A dataset that records no such day, validated
#'  without `as_of`, cannot run those two rules: that is an error of class
#'  `neoipcr_validation_needs_facts` when either of them is selected, the
#'  default selection included, while a selection that leaves both out runs
#'  without a date. A value that is not a single `Date` is an error whatever
#'  rules are selected.
#'
#' @returns A tibble with one row per finding — a flagged record, or for
#'  rule 17 one of the two enrolments of an overlapping pair: `rule_id`;
#'  `patient_key`, `enrollment_key` and
#'  `event_key`, each naming the record the finding refers to at that level
#'  and `NA` where there is none (an enrolment-level rule that compared a
#'  form names that form's event, so a consumer can show the finding under
#'  it; the level a rule is recorded and exempted on is the one the table
#'  below names); and `context`, a list column holding a one-row tibble of
#'  the values the finding refers to (`NULL` where the rule records none).
#'  Zero rows when nothing is flagged. The result's `rules_skipped`
#'  attribute names the selected rules that could not run because the
#'  dataset lacks a column they read (an integer vector, empty when every
#'  rule ran); such a rule logs a warning and flags nothing, so a caller
#'  stating which rules a result rests on reads that attribute rather than
#'  the selection.
#'
#' @section Context fields:
#' Each rule records the fields below in `context`, identifies its finding
#' by the key named as its level, and is exempted by an exception record
#' written at that level: the patient alone for rule 1, the patient and the
#' enrolment date for an enrolment-level rule, and the event's type and date
#' as well for an event-level rule, the type being one the rule concerns
#' (rules 7, 12 and 27–30 sepsis, 8, 13 and 35–38 necrotizing enterocolitis,
#' 9, 14 and 31–34 pneumonia, 10, 15, 22–24, 39 and 40 surgical procedures,
#' 11, 19, 41 and 42 surgical site infections, 20 any infection). An
#' enrolment-level rule that compares a form carries that form's event on
#' its finding, so a document shows the finding on the form; a record for
#' such a rule may name that form's type and date as well, or leave them
#' empty, and is refused naming any other type (rules 3 and 5 the admission
#' form, 2, 4, 6, 18, 21 and 44 the surveillance-end form; 17, 25, 26 and 43
#' carry no event).
#' Dates are `Date`, statuses factors, counts integers. A dataset imported
#' without incomplete enrolments or events (`include_incomplete`) carries no
#' `status` column for them; the rules then treat every such record as
#' completed, which is what the import's request filter made it.
#'
#' Rules 43 and 44 question an enrolment still active more than 120 days
#' after its enrolment date, measured against `as_of`: 43 one without a
#' surveillance-end form, 44 one whose surveillance-end form is not
#' completed. A neonatal stay that long is exceptional, so such a record is
#' most often one nobody closed once the infant left; but the infant may
#' still be admitted, in which case the finding is to be ignored, and an
#' exception record keeps the enrolment out of the findings while the stay
#' lasts. `days_open` is the whole days from the enrolment date to `as_of`.
#' A dataset imported with the enrolments that are not completed but only
#' the completed events holds no end form that is not completed, and one
#' imported with a surveillance-end date filter holds none dated outside
#' its window, so on either a missing form and one the dataset does not hold
#' look alike; rule 43 is skipped on such a dataset (named in
#' `rules_skipped`), and an import of that shape with the validation pass on
#' refuses.
#'
#' | Rules | Level | Context fields |
#' |---|---|---|
#' | 1 | `patient_key` | none |
#' | 2 | `enrollment_key` | none |
#' | 3 | `enrollment_key` | `enrolledAt`, `occurredAt` |
#' | 4 | `enrollment_key` | `admOccurredAt`, `endOccurredAt` |
#' | 5, 6 | `enrollment_key` | `status` |
#' | 7, 8, 9, 10, 11 | `event_key` | `enrollment_status`, `end_status`, and the form's own status as `bsi_status`, `nec_status`, `hap_status`, `pro_status` or `ssi_status` |
#' | 12, 13, 14, 15 | `event_key` | `enrolledAt`, `admOccurredAt`, `endOccurredAt`, and the event's date as `bsiOccurredAt`, `necOccurredAt`, `hapOccurredAt` or `proOccurredAt` |
#' | 17 | `enrollment_key` | `enrolledAt_this`, `endOccurredAt_this`, `enrolledAt_other`, `endOccurredAt_other` — one finding for each enrolment of an overlapping pair, naming the other's dates, so each overlap appears once from either side and an enrolment that overlaps two others appears twice |
#' | 18 | `enrollment_key` | `patient_days`, `patient_days_calculated` |
#' | 19 | `event_key` | `infection_type` |
#' | 20 | `event_key` | `index`, `secondary_bsi`, `name` |
#' | 21 | `enrollment_key` | `ab_substance_days`, `ab_days` |
#' | 22, 23, 24 | `event_key` | `procedure_description`, `procedure_code` |
#' | 25, 26 | `enrollment_key` | none |
#' | 27, 31, 35, 39, 41 | `event_key` | `dol`, `dol_calc` |
#' | 28, 32, 36, 40, 42 | `event_key` | `los`, `los_calc` |
#' | 29, 33, 37 | `event_key` | `dol` |
#' | 30, 34, 38 | `event_key` | `dos` |
#' | 43 | `enrollment_key` | `enrolledAt`, `days_open` |
#' | 44 | `enrollment_key` | `enrolledAt`, `days_open`, `status` |
#'
#' @family validation
#' @export
validate <- function(x, rules = NULL, exceptions = NULL, as_of = NULL)
{
  check_neoipcr_ds(x)
  # The rules read the enrollments' `patient_key` and the events'
  # `event_type_key`, which only the "full" tiers carry; a narrower tier
  # would fail inside a rule with a column-absent error.
  assert_options_for(x, required = list(
    include_patient    = c("pseudo", "full"),
    include_enrollment = "full",
    include_event      = "full"
  ), fn_name = "validate")

  ids <- validation_rule_ids()
  if (!is.null(rules)) {
    if (!is.numeric(rules) || anyNA(rules) || !.whole_or_na(rules))
      rlang::abort(
        "`rules` must be a vector of whole numbers naming validation rules.",
        class = "neoipcr_unknown_validation_rule")
    rules <- as.integer(rules)
    unknown <- setdiff(rules, ids)
    if (length(unknown) > 0L)
      rlang::abort(c(
        "`rules` names validation rules that do not exist.",
        x = sprintf("Unknown rule id(s): %s.", paste(unknown, collapse = ", ")),
        i = sprintf("The rules are numbered %d to %d; see `validation_rule_ids()`.",
                    min(ids), max(ids))),
        class = "neoipcr_unknown_validation_rule")
  }
  exceptions <- .exceptions_in_key_form(x, exceptions)

  # A rule returns `NULL` when it cannot run for want of a column the dataset
  # does not hold; it has logged that, but a caller reporting which rules a
  # result rests on needs the ids, so they ride along as an attribute.
  selected <- Filter(\(r) is.null(rules) || r$id %in% rules, validation_rules)
  # The reference date is resolved only when a dated rule is selected, so a
  # dataset without one still runs a selection that leaves those rules out.
  .check_as_of(as_of)
  dated <- vapply(selected, \(r) isTRUE(r$dated), logical(1))
  if (any(dated))
    as_of <- .reference_date(x, as_of)
  results  <- lapply(selected, \(r)
    if (isTRUE(r$dated)) r$fun(x, exceptions, as_of) else r$fun(x, exceptions))
  skipped  <- vapply(selected, \(r) r$id, integer(1))[
    vapply(results, is.null, logical(1))]

  flagged <- results |>
    dplyr::bind_rows() |>
    dplyr::ungroup()

  # The shape is the same whatever ran. `bind_rows()` takes its class,
  # grouping and column types from the first rule's result, so the result is
  # bound onto a plain template instead: a rule that skips itself, records no
  # context or returns a grouped tibble, or a selection that flags nothing,
  # still yields exactly these five columns with integer keys.
  template <- tibble::tibble(
    rule_id        = integer(),
    patient_key    = integer(),
    enrollment_key = integer(),
    event_key      = integer(),
    context        = list())
  findings <- dplyr::bind_rows(template, flagged) |>
    dplyr::mutate(dplyr::across(
      c("rule_id", "patient_key", "enrollment_key", "event_key"),
      as.integer)) |>
    finalize_to_schema(
      validation_finding_atoms, x$metadata$dataset_options)

  # A finding carries the fields its rule's registry entry declares — the
  # contract a consumer's sentences are written against — so a drift
  # between a rule and its declaration surfaces here, not in a document.
  .assert_declared_context(findings, validation_rule_context_fields())

  attr(findings, "rules_skipped") <- unname(skipped)
  findings
}

# Refuse findings whose context fields are not the ones `declared` names for
# their rule, `declared` being a list by rule id as
# `validation_rule_context_fields()` returns it.
.assert_declared_context <- function(findings, declared)
{
  fields_of <- function(context)
    if (is.null(names(context))) character() else names(context)
  undeclared <- purrr::map2_lgl(
    findings$rule_id, findings$context,
    \(id, context) !setequal(fields_of(context), declared[[as.character(id)]]))
  if (any(undeclared))
    rlang::abort(c(
      "A validation rule recorded context fields its registry entry does not declare.",
      x = sprintf("Rule(s): %s.",
                  paste(sort(unique(findings$rule_id[undeclared])), collapse = ", "))))
  invisible(findings)
}
