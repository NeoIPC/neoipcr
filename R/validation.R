# The registry of validation rules, in id order. Each entry names the rule,
# the level its finding is recorded on — the key a finding is identified by
# and an exception record is matched on — the event types an event-level
# rule concerns, and the function that implements it. A finding is data —
# keys and the values a rule compared — never a sentence: the prose belongs
# to whichever document renders the finding, where it can be localized.
validation_rules <- list(
  list(id = 1L,  level = "patient",    fun = validation_rule_1),
  list(id = 2L,  level = "enrollment", fun = validation_rule_2),
  list(id = 3L,  level = "enrollment", fun = validation_rule_3),
  list(id = 4L,  level = "enrollment", fun = validation_rule_4),
  list(id = 5L,  level = "enrollment", fun = validation_rule_5),
  list(id = 6L,  level = "enrollment", fun = validation_rule_6),
  list(id = 7L,  level = "event", event_types = "bsi", fun = validation_rule_7),
  list(id = 8L,  level = "event", event_types = "nec", fun = validation_rule_8),
  list(id = 9L,  level = "event", event_types = "hap", fun = validation_rule_9),
  list(id = 10L, level = "event", event_types = "pro", fun = validation_rule_10),
  list(id = 11L, level = "event", event_types = "ssi", fun = validation_rule_11),
  list(id = 12L, level = "event", event_types = "bsi", fun = validation_rule_12),
  list(id = 13L, level = "event", event_types = "nec", fun = validation_rule_13),
  list(id = 14L, level = "event", event_types = "hap", fun = validation_rule_14),
  list(id = 15L, level = "event", event_types = "pro", fun = validation_rule_15),
  list(id = 16L, level = "event", event_types = "ssi", fun = validation_rule_16),
  list(id = 17L, level = "enrollment", fun = validation_rule_17),
  list(id = 18L, level = "enrollment", fun = validation_rule_18),
  list(id = 19L, level = "event", event_types = "ssi", fun = validation_rule_19),
  list(id = 20L, level = "event", event_types = c("bsi", "nec", "hap", "ssi"),
       fun = validation_rule_20),
  list(id = 21L, level = "enrollment", fun = validation_rule_21),
  list(id = 22L, level = "event", event_types = "pro", fun = validation_rule_22),
  list(id = 23L, level = "event", event_types = "pro", fun = validation_rule_23),
  list(id = 24L, level = "event", event_types = "pro", fun = validation_rule_24),
  list(id = 25L, level = "enrollment", fun = validation_rule_25),
  list(id = 26L, level = "enrollment", fun = validation_rule_26),
  list(id = 27L, level = "event", event_types = "bsi", fun = validation_rule_27),
  list(id = 28L, level = "event", event_types = "bsi", fun = validation_rule_28),
  list(id = 29L, level = "event", event_types = "bsi", fun = validation_rule_29),
  list(id = 30L, level = "event", event_types = "bsi", fun = validation_rule_30),
  list(id = 31L, level = "event", event_types = "hap", fun = validation_rule_31),
  list(id = 32L, level = "event", event_types = "hap", fun = validation_rule_32),
  list(id = 33L, level = "event", event_types = "hap", fun = validation_rule_33),
  list(id = 34L, level = "event", event_types = "hap", fun = validation_rule_34),
  list(id = 35L, level = "event", event_types = "nec", fun = validation_rule_35),
  list(id = 36L, level = "event", event_types = "nec", fun = validation_rule_36),
  list(id = 37L, level = "event", event_types = "nec", fun = validation_rule_37),
  list(id = 38L, level = "event", event_types = "nec", fun = validation_rule_38),
  list(id = 39L, level = "event", event_types = "pro", fun = validation_rule_39),
  list(id = 40L, level = "event", event_types = "pro", fun = validation_rule_40),
  list(id = 41L, level = "event", event_types = "ssi", fun = validation_rule_41),
  list(id = 42L, level = "event", event_types = "ssi", fun = validation_rule_42))

# The level of each rule, named by rule id, and the event types an
# event-level rule concerns; `check_exception_list()` holds a record's shape
# to its rule's level through these.
.rule_levels <- function()
  rlang::set_names(
    vapply(validation_rules, \(r) r$level, character(1)),
    validation_rule_ids())

.rule_event_types <- function(rule_id)
  validation_rules[[match(rule_id, validation_rule_ids())]]$event_types

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
# key; a key form built by hand may name another level and then exempts
# nothing.
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
  present <- intersect(key_cols, names(exceptions))
  not_integer <- present[!vapply(
    present,
    \(key) .whole_or_na(exceptions[[key]]),
    logical(1))]
  wrong <- c(
    .rule_id_problem(exceptions$rule_id, "rule_id"),
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
#'
#' @returns A tibble with one row per finding — a flagged record, or for
#'  rule 17 a pair of them: `rule_id`; `patient_key`, `enrollment_key` and
#'  `event_key`, each naming the record the finding refers to at that level
#'  and `NA` where there is none (an enrolment-level rule that compared a
#'  form names that form's event, so a consumer can show the finding under
#'  it; the level a rule is recorded and exempted on is the one the table
#'  below names); and `context`, a list column holding a one-row tibble of
#'  the values the finding refers to (`NULL` where the rule records none).
#'  Zero rows when nothing is flagged.
#'
#' @section Context fields:
#' Each rule records the fields below in `context`, identifies its finding
#' by the key named as its level, and is exempted by an exception record
#' written at that level: the patient alone for rule 1, the patient and the
#' enrolment date for an enrolment-level rule, and the event's type and date
#' as well for an event-level rule, the type being one the rule concerns
#' (rules 7, 12 and 27–30 sepsis, 8, 13 and 35–38 necrotizing enterocolitis,
#' 9, 14 and 31–34 pneumonia, 10, 15, 22–24, 39 and 40 surgical procedures,
#' 11, 16, 19, 41 and 42 surgical site infections, 20 any infection).
#' Dates are `Date`, statuses factors, counts integers. A dataset imported
#' without incomplete enrolments or events (`include_incomplete`) carries no
#' `status` column for them; the rules then treat every such record as
#' completed, which is what the import's request filter made it.
#'
#' | Rules | Level | Context fields |
#' |---|---|---|
#' | 1 | `patient_key` | none |
#' | 2 | `enrollment_key` | none |
#' | 3 | `enrollment_key` | `enrolledAt`, `occurredAt` |
#' | 4 | `enrollment_key` | `admOccurredAt`, `endOccurredAt` |
#' | 5, 6 | `enrollment_key` | `status` |
#' | 7, 8, 9, 10, 11 | `event_key` | `enrollment_status`, `end_status`, and the form's own status as `bsi_status`, `nec_status`, `hap_status`, `pro_status` or `ssi_status` |
#' | 12, 13, 14, 15, 16 | `event_key` | `enrolledAt`, `admOccurredAt`, `endOccurredAt`, and the event's date as `bsiOccurredAt`, `necOccurredAt`, `hapOccurredAt`, `proOccurredAt` or `ssiOccurredAt` |
#' | 17 | `enrollment_key` | `enrolledAt_this`, `endOccurredAt_this`, `enrolledAt_other`, `endOccurredAt_other` — one finding per overlapping pair, so an enrolment that overlaps two others appears twice, once with each partner's dates |
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
#'
#' @family validation
#' @export
validate <- function(x, rules = NULL, exceptions = NULL)
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

  flagged <- validation_rules |>
    lapply(\(r) if (is.null(rules) || r$id %in% rules) r$fun(x, exceptions)) |>
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
  dplyr::bind_rows(template, flagged) |>
    dplyr::mutate(dplyr::across(
      c("rule_id", "patient_key", "enrollment_key", "event_key"),
      as.integer)) |>
    dplyr::select(
      "rule_id", "patient_key", "enrollment_key", "event_key", "context")
}
