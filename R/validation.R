# The registry of validation rules, in id order. Each entry names the rule
# and the function that implements it. A finding is data — keys and the
# values a rule compared — never a sentence: the prose belongs to whichever
# document renders the finding, where it can be localized.
validation_rules <- list(
  list(id = 1L,  fun = validation_rule_1),
  list(id = 2L,  fun = validation_rule_2),
  list(id = 3L,  fun = validation_rule_3),
  list(id = 4L,  fun = validation_rule_4),
  list(id = 5L,  fun = validation_rule_5),
  list(id = 6L,  fun = validation_rule_6),
  list(id = 7L,  fun = validation_rule_7),
  list(id = 8L,  fun = validation_rule_8),
  list(id = 9L,  fun = validation_rule_9),
  list(id = 10L, fun = validation_rule_10),
  list(id = 11L, fun = validation_rule_11),
  list(id = 12L, fun = validation_rule_12),
  list(id = 13L, fun = validation_rule_13),
  list(id = 14L, fun = validation_rule_14),
  list(id = 15L, fun = validation_rule_15),
  list(id = 16L, fun = validation_rule_16),
  list(id = 17L, fun = validation_rule_17),
  list(id = 18L, fun = validation_rule_18),
  list(id = 19L, fun = validation_rule_19),
  list(id = 20L, fun = validation_rule_20),
  list(id = 21L, fun = validation_rule_21),
  list(id = 22L, fun = validation_rule_22),
  list(id = 23L, fun = validation_rule_23),
  list(id = 24L, fun = validation_rule_24),
  list(id = 25L, fun = validation_rule_25),
  list(id = 26L, fun = validation_rule_26),
  list(id = 27L, fun = validation_rule_27),
  list(id = 28L, fun = validation_rule_28),
  list(id = 29L, fun = validation_rule_29),
  list(id = 30L, fun = validation_rule_30),
  list(id = 31L, fun = validation_rule_31),
  list(id = 32L, fun = validation_rule_32),
  list(id = 33L, fun = validation_rule_33),
  list(id = 34L, fun = validation_rule_34),
  list(id = 35L, fun = validation_rule_35),
  list(id = 36L, fun = validation_rule_36),
  list(id = 37L, fun = validation_rule_37),
  list(id = 38L, fun = validation_rule_38),
  list(id = 39L, fun = validation_rule_39),
  list(id = 40L, fun = validation_rule_40),
  list(id = 41L, fun = validation_rule_41),
  list(id = 42L, fun = validation_rule_42))

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
# column present. A rule anti-joins its findings on its natural key, so a
# record whose key at that level is `NA` exempts nothing: one that did not
# resolve, or one written on a level above the rule's (an enrolment for an
# event-level rule). A record written below the rule's level — an event
# named for an enrolment-level rule — carries the enrolment's key as well,
# and exempts the enrolment once it has resolved as a whole (see
# `resolve_validation_exceptions()`).
.rule_exceptions <- function(exceptions, rule_id)
{
  if (is.null(exceptions))
    return(.exception_keys())
  id <- rule_id
  dplyr::bind_rows(.exception_keys(), exceptions) |>
    dplyr::filter(.data$rule_id == id)
}

# Whatever form the caller passed exceptions in, the rules read key form.
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
  dplyr::bind_rows(.exception_keys(), exceptions)
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
#'  the form a user writes needs `include_patient = "full"` and a department
#'  tier on top, as [resolve_validation_exceptions()] describes.
#' @param rules Integer vector of rule ids to run; `NULL` (the default) runs all
#'  of them. An id outside [validation_rule_ids()] is an error.
#' @param exceptions The records to exempt from the rule that flags them:
#'  either the list a user writes, as [read_validation_exceptions()] returns
#'  it, or its resolved key form as [resolve_validation_exceptions()] returns
#'  it (`rule_id`, `patient_key`, `enrollment_key`, `event_key`). `NULL`
#'  exempts nothing.
#'
#' @returns A tibble with one row per flagged record: `rule_id`, the keys that
#'  identify the record (`patient_key`, `enrollment_key`, `event_key`; `NA`
#'  where a rule does not operate at that level) and `context`, a list column
#'  holding a one-row tibble of the values the finding refers to (`NULL`
#'  where the rule records none). Zero rows when nothing is flagged.
#'
#' @section Context fields:
#' Each rule records the fields below in `context`, and is exempted by an
#' exception record matched on the key named as its level. Dates are `Date`,
#' statuses factors, counts integers. A dataset imported without incomplete
#' enrolments or events (`include_incomplete`) carries no `status` column for
#' them; the rules then treat every such record as completed, which is what
#' the import's request filter made it.
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
#' | 17 | `enrollment_key` | `enrolledAt_this`, `endOccurredAt_this`, `enrolledAt_other`, `endOccurredAt_other` |
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
    if (!is.numeric(rules) || anyNA(rules) || any(rules != round(rules)))
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
