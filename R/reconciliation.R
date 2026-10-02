# The reconciliations of the NeoIPC coordinating centre: stored values that
# Tracker Capture derives itself and the partner team never chooses, and
# values it keeps in a section it hides. The client assigns a day of life
# whenever it processes a form that can still be edited, and saves it, so a
# different stored value survives only on a completed form or a form of a
# completed enrolment, which it shows read-only as stored until the form is
# reopened, or where the value was stored around the client
# (reconciliations 1 and 2). It computes the total gestation days into a
# field the partner cannot edit and the dashboard does not show (3 and 4),
# and a section it hides stays hidden with what it holds (5 and 6).
# `import_dhis2()` repairs them under `reconcile = TRUE` before its filters
# and its validation pass, so that both judge a record by the values the
# client derives; `reconciliation_details()` lists what the import would
# repair on a dataset imported with the values as stored. The numbering is
# the one the `reconcile` documentation of `dhis2_dataset_options()` uses.
#
# Every reconciliation reads its records through a frame of keys and dates
# rather than through the public tibbles, whose pseudonymized tiers leave out
# the links and dates it needs: the import builds the frame from its internal
# maps, `reconciliation_details()` from a dataset's full tiers. A
# reconciliation returns its changes: one row per record it acts on, with
# `reconciliation_id`, `action` (`"repair"` or `"report"`), the record's keys
# at every level it has and the fields the registry declares as its context.

# The tiers a reconciliation of the enrolments' forms reads besides the
# patients': the forms are read through their events, and the events
# through their enrolments.
.reconciliation_form_tiers <- c("include_enrollment", "include_event")

# The registry of reconciliations, in id order, an id space of their own.
# Each entry names the level its records are counted and identified on, the
# tiers besides the patients' it reads its records through, and the fields
# `reconciliation_details()` records in a row's `context`.
reconciliations <- list(
  list(id = 1L, level = "enrollment", tiers = .reconciliation_form_tiers,
       context = c("type", "dol", "dol_reconciled")),
  list(id = 2L, level = "event", tiers = .reconciliation_form_tiers,
       context = c("dol", "dol_reconciled")),
  list(id = 3L, level = "patient", tiers = character(),
       context = c("gest_age", "total_gestation_days",
                   "total_gestation_days_reconciled")),
  list(id = 4L, level = "patient", tiers = character(),
       context = c("gest_age", "total_gestation_days",
                   "total_gestation_days_reconciled")),
  list(id = 5L, level = "event", tiers = .reconciliation_form_tiers,
       context = c("sec_bsi", "slots", "agents")),
  list(id = 6L, level = "event", tiers = .reconciliation_form_tiers,
       context = c("slots", "agents", "findings")))

.reconciliation_keys <- c("patient_key", "enrollment_key", "event_key")

# Whether none of the tiers `tiers` names is "no" under `opts`.
.tiers_held <- function(opts, tiers)
  all(vapply(tiers, \(tier) opts[[tier]] != "no", logical(1)))

# Whether an import under `opts` reconciled the enrolments' forms.
.form_reconciliations_run <- function(opts)
  .reconciliation_runs(opts) && .tiers_held(opts, .reconciliation_form_tiers)

# The import's reconciliation log: one row per record a reconciliation acts
# on, with the reconciliation, the action and the record's keys, from
# `changes`, a list of changes as the reconciliations return them.
.reconciliation_log <- function(changes = list())
  dplyr::bind_rows(
    tibble::tibble(
      reconciliation_id = integer(),
      action            = character(),
      patient_key       = integer(),
      enrollment_key    = integer(),
      event_key         = integer()),
    # Unnamed: `bind_rows()` would take a named list for the columns of one
    # data frame.
    purrr::map(unname(changes), \(change) dplyr::select(
      change, "reconciliation_id", "action",
      tidyselect::all_of(.reconciliation_keys))))

# The audit fields a value carries beside it. They describe the value as
# stored, so they do not describe a repaired one.
.audit_companions <- function(col)
  paste0(col, c("_storedBy", "_createdBy", "_updatedBy", "_createdAt",
                "_updatedAt"))

# `records` with `col` set to `value` on the rows `repair` marks, and the
# audit fields of `col` missing there. `value` is a single value or one per
# row.
.repair_value <- function(records, col, repair, value)
  records |>
    dplyr::mutate(
      !!col := dplyr::if_else(repair, value, .data[[col]]),
      dplyr::across(
        tidyselect::any_of(.audit_companions(col)),
        \(companion) replace(companion, repair, NA)))

# ---- Gestational age (reconciliations 3 and 4) ----------------------------

# The total gestation days the registration form computes from a
# gestational-age text: the completed weeks times seven plus the days, the
# client reading the weeks as the text's first two characters and the days as
# its last (`NEOIPC_PATIENT_SET_GESTATION_DAYS_AND_WEEKS`), for a text in the
# required format (`.gestational_age_text_pattern`), and `NA` for any other
# text, from which the client computes nothing.
.gestation_days_from_text <- function(text)
{
  text  <- as.character(text)
  valid <- .text_recorded(text) &
    dplyr::coalesce(
      stringr::str_detect(text, .gestational_age_text_pattern), FALSE)
  days <- rep(NA_integer_, length(text))
  days[valid] <-
    as.integer(stringr::str_sub(text[valid], 1L, 2L)) * 7L +
    as.integer(stringr::str_sub(text[valid], 4L, 4L))
  days
}

# Reconciliations 3 and 4 on `patients`, which carries `patient_key`,
# `gest_age` and `total_gestation_days`. The client computes the total from
# the text whenever the text is in the required format and shows it
# read-only, so a stored total that differs from such a text, or a missing
# one beside it, is one the partner neither sees nor can correct:
# reconciliation 3 computes it from the text. For any other text the client
# computes 0, and a text in the format yields 140 to 349 days, so a total
# outside that range beside no such text, 0 among them, is no gestational age
# at all: reconciliation 4 removes it. A total within the range beside a text
# in another format is left as stored; validation rule 58 reports the text. A
# total within the range beside no text at all, or an empty one, is left as
# stored too and reported by no rule, since rule 57 counts the total alone as
# a recorded gestational age.
.reconcile_gestational_age <- function(patients)
{
  stored     <- as.integer(patients$total_gestation_days)
  from_text  <- .gestation_days_from_text(patients$gest_age)
  stale      <- !is.na(from_text) & dplyr::coalesce(stored != from_text, TRUE)
  impossible <- is.na(from_text) &
    dplyr::coalesce(stored < 140L | stored > 349L, FALSE)

  changes <- tibble::tibble(
    reconciliation_id = dplyr::if_else(stale, 3L, 4L),
    action            = "repair",
    patient_key       = patients$patient_key,
    enrollment_key    = NA_integer_,
    event_key         = NA_integer_,
    gest_age          = as.character(patients$gest_age),
    total_gestation_days = stored,
    total_gestation_days_reconciled = from_text)[stale | impossible, ]

  list(
    records = .repair_value(
      patients, "total_gestation_days", stale | impossible, from_text),
    changes = list(
      `3` = changes[changes$reconciliation_id == 3L, ],
      `4` = changes[changes$reconciliation_id == 4L, ]))
}

# ---- Day of life (reconciliations 1 and 2) --------------------------------

# Reconciliation 1 on `admission` (`event_key`, `type`, `dol`), placed in its
# enrolment by `frame` (`event_key`, `enrollment_key`, `patient_key`), with
# `enrollments` (`enrollment_key`, `patient_key`, `enrolledAt`) every
# enrolment read of the patients `admission` belongs to, the ones a
# reporting period leaves out included. The client assigns day of life 1 to
# an admission form of type 1 or 2 (`NEOIPC_ADM_TYPE_1`) whenever it
# processes the form while it can still be edited, and saves it, so the team
# never chooses the value; a stored value other than 1 survives only on a
# completed form or a form of a completed enrolment, which the client shows
# read-only as stored until the form is reopened, or where it was stored
# around the client. A type-1 or type-2 admission dated after another
# enrolment of the patient is left as stored, whether or not the dataset
# holds that enrolment: its type is what is wrong, which validation rule 47
# reports where the dataset holds both and the partner can correct. An
# admission without a type, to which the client assigns day 1 as well, is
# left as stored too; under the default options one stored above day 120 is
# then dropped by the admission filter (`filter_admissions()`) with no
# finding, since no rule checks a missing admission type.
.reconcile_admission_dol <- function(admission, frame, enrollments)
{
  changes <- admission |>
    dplyr::select("event_key", "type", "dol") |>
    dplyr::inner_join(
      frame |>
        dplyr::select("event_key", "enrollment_key", "patient_key"),
      dplyr::join_by("event_key")) |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select("enrollment_key", "enrolledAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::filter(.data$type %in% c("1", "2") &
                  dplyr::coalesce(.data$dol != 1L, TRUE)) |>
    dplyr::anti_join(
      enrollments |>
        dplyr::select("patient_key", "enrolledAt_previous" = "enrolledAt"),
      dplyr::join_by("patient_key", "enrolledAt" > "enrolledAt_previous")) |>
    dplyr::mutate(
      reconciliation_id = 1L,
      action            = "repair",
      patient_key       = .data$patient_key,
      enrollment_key    = .data$enrollment_key,
      event_key         = .data$event_key,
      type              = .data$type,
      dol               = .data$dol,
      dol_reconciled    = 1L,
      .keep = "none")

  list(
    records = .repair_value(
      admission, "dol", admission$event_key %in% changes$event_key, 1L),
    changes = changes)
}

# Reconciliation 2 on the infection and procedure forms `forms`, a list of
# tibbles carrying `event_key` and `dol`, on the enrolments reconciliation 1
# repaired (`repaired`, its changes), with `frame` (`event_key`,
# `enrollment_key`, `patient_key`, `occurredAt`) and `enrollments`
# (`enrollment_key`, `enrolledAt`). The client derives a form's day of life
# from the admission's (`NEOIPC_<STAGE>_SET_DOL`: the admission value plus
# the days from the enrolment date to the event date, an empty admission
# value counting as 0) whenever it processes the form while it can still be
# edited, and saves it, so the team never chooses the value, and a form
# derived from the stored admission value carries that value's error. Such
# a form, and one without a day of life, is derived again from day 1; a
# form that matches neither stays as stored for validation rules 27, 31, 35,
# 39 and 41, since the admission value does not explain it. The day of
# occurrence after admission does not depend on the admission value and is
# left as it is.
.reconcile_event_dol <- function(forms, repaired, frame, enrollments)
{
  events <- repaired |>
    dplyr::select("enrollment_key", "admission_dol" = "dol") |>
    dplyr::inner_join(
      enrollments |>
        dplyr::select("enrollment_key", "enrolledAt"),
      dplyr::join_by("enrollment_key")) |>
    dplyr::inner_join(
      frame |>
        dplyr::select("event_key", "enrollment_key", "patient_key", "occurredAt"),
      dplyr::join_by("enrollment_key"))

  changes <- purrr::map(forms, \(form)
    form |>
      dplyr::select("event_key", "dol") |>
      dplyr::inner_join(events, dplyr::join_by("event_key")) |>
      dplyr::mutate(
        days           = as.integer(.data$occurredAt - .data$enrolledAt),
        derived        = dplyr::coalesce(.data$admission_dol, 0L) + .data$days,
        dol_reconciled = 1L + .data$days) |>
      dplyr::filter(dplyr::coalesce(.data$dol == .data$derived, is.na(.data$dol)) &
                    !is.na(.data$dol_reconciled)) |>
      dplyr::mutate(
        reconciliation_id = 2L,
        action            = "repair",
        patient_key       = .data$patient_key,
        enrollment_key    = .data$enrollment_key,
        event_key         = .data$event_key,
        dol               = .data$dol,
        dol_reconciled    = .data$dol_reconciled,
        .keep = "none"))

  records <- purrr::map2(forms, changes, \(form, change) {
    reconciled <- change$dol_reconciled[match(form$event_key, change$event_key)]
    .repair_value(form, "dol", !is.na(reconciled), reconciled)
  })

  list(records = records, changes = dplyr::bind_rows(changes))
}

# ---- Hidden infectious-agent sections (reconciliations 5 and 6) ----------

# The slots of a hidden section by event: `slots` the findings rows it
# holds, a row holding only a resistance or name companion among them, and
# `agents` the rows that name a concept.
.section_counts <- function(rows)
  rows |>
    dplyr::summarise(
      slots  = dplyr::n(),
      agents = sum(!is.na(.data$pathogen_key)),
      .by = "event_key")

# Reconciliation 5 on `findings` (`agent_finding_key`, `event_key`,
# `secondary_bsi`, `index`, `pathogen_key`) and the surgical site infection
# forms `ssi` (`event_key`, `sec_bsi`), with `frame` (`event_key`,
# `event_type_key`, `enrollment_key`, `patient_key`). The client hides the
# form's secondary-BSI section, and every field in it, unless its
# secondary-BSI item is Yes, and blanks those fields whenever it processes
# the form while it can still be edited (`NEOIPC_SSI_NO_SEC_BSI`). Infectious
# agents stored in the section under an item that is not Yes therefore
# survive only on a completed form or a form of a completed enrolment, where
# the section stays hidden with them, or where they were stored around the
# client. The section is removed whole, as the reopened form would blank it,
# each event counted once; an event without a form row has an unanswered
# item, which is not Yes. Returns the changes and the findings to remove.
.reconcile_ssi_secondary <- function(findings, ssi, frame)
{
  hidden <- frame |>
    dplyr::filter(.data$event_type_key == "ssi") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::left_join(
      ssi |>
        dplyr::select("event_key", "sec_bsi"),
      dplyr::join_by("event_key")) |>
    dplyr::filter(!dplyr::coalesce(.data$sec_bsi == "1", FALSE))
  rows <- findings |>
    dplyr::filter(dplyr::coalesce(.data$secondary_bsi, FALSE)) |>
    dplyr::semi_join(hidden, dplyr::join_by("event_key"))

  changes <- hidden |>
    dplyr::inner_join(.section_counts(rows), dplyr::join_by("event_key")) |>
    dplyr::mutate(
      reconciliation_id = 5L,
      action            = "repair",
      patient_key       = .data$patient_key,
      enrollment_key    = .data$enrollment_key,
      event_key         = .data$event_key,
      sec_bsi           = .data$sec_bsi,
      slots             = .data$slots,
      agents            = .data$agents,
      .keep = "none")

  list(changes = changes, removed = rows$agent_finding_key)
}

# Reconciliation 6 on `findings` and the sepsis forms `sepsis`, which carry
# `event_key` and every item of `.bsi_clinical_sepsis_items`, with `frame` as
# for reconciliation 5. The client hides the form's infectious-agent section
# once the culture is recorded as negative (`no_pos_culture`), and keeps what
# the section holds. Its three primary slots are removed whole, each event
# counted once, unless the form names an infectious agent and would not meet
# the clinical-sepsis definition without it: the client hides the clinical
# signs and laboratory findings behind a recognized pathogen, so the partner
# may never have been shown the items the definition counts, and validation
# rule 59 would flag the form for them. Such a form is reported and kept as
# stored. The configuration also hides the culture-negative item once slot 1
# names an infectious agent
# (`NEOIPC_BSI_NO_POS_CULTURE_HIDE_IF_AGENT_RECORDED`), so the client, once it
# processes the reopened form, blanks the flag when slot 1 holds an agent and
# keeps the agents. The reconciliation follows the completed form instead,
# which is what the team sees: the flag and the antibiotic therapy stay, and
# a repaired form loses its infectious agents. Returns the changes and the
# findings to remove.
.reconcile_culture_negative <- function(findings, sepsis, frame)
{
  forms <- frame |>
    dplyr::filter(.data$event_type_key == "bsi") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::inner_join(
      sepsis |>
        dplyr::select("event_key", tidyselect::all_of(.bsi_clinical_sepsis_items)),
      dplyr::join_by("event_key")) |>
    dplyr::filter(dplyr::coalesce(.data$no_pos_culture, FALSE))
  rows <- .primary_slot_rows(findings) |>
    dplyr::semi_join(forms, dplyr::join_by("event_key"))

  changes <- forms |>
    dplyr::inner_join(.section_counts(rows), dplyr::join_by("event_key")) |>
    .bsi_clinical_sepsis() |>
    dplyr::mutate(
      reconciliation_id = 6L,
      action            = dplyr::if_else(
        .data$agents > 0L & !.data$clinical_sepsis, "report", "repair"),
      patient_key       = .data$patient_key,
      enrollment_key    = .data$enrollment_key,
      event_key         = .data$event_key,
      slots             = .data$slots,
      agents            = .data$agents,
      findings          = .data$findings,
      .keep = "none")
  repaired <- changes$event_key[changes$action == "repair"]

  list(
    changes = changes,
    removed = rows$agent_finding_key[rows$event_key %in% repaired])
}

# ---- Reconciling a dataset ------------------------------------------------

# The reconciliations of the enrolments' forms, in id order, on `data`, a
# list of the dataset slots `admissionData`, the five slots
# `.event_data_slot` names, `infectiousAgentFindings` and
# `unknownPathogenNames`. `frame` places each event (`event_key`,
# `event_type_key`, `enrollment_key`, `patient_key`, `occurredAt`),
# `enrollments` lists the enrolments the dataset holds (`enrollment_key`,
# `patient_key`, `enrolledAt`), and `findings` gives every finding its slot
# (`agent_finding_key`, `event_key`, `secondary_bsi`, `index`,
# `pathogen_key`), whatever tier `data` holds the findings in. The names of
# the removed findings go with them here, so that the validation pass sees
# the two tables agree. Returns `data` reconciled and the changes of each
# reconciliation, named by its id.
.reconcile_event_data <- function(data, frame, enrollments, findings)
{
  admission <- .reconcile_admission_dol(data$admissionData, frame, enrollments)
  data$admissionData <- admission$records

  forms <- .reconcile_event_dol(
    data[unname(.event_data_slot)], admission$changes, frame, enrollments)
  data[names(forms$records)] <- forms$records

  ssi    <- .reconcile_ssi_secondary(findings, data$ssiData, frame)
  sepsis <- .reconcile_culture_negative(findings, data$sepsisData, frame)
  removed <- c(ssi$removed, sepsis$removed)
  data$infectiousAgentFindings <- data$infectiousAgentFindings |>
    dplyr::filter(!(.data$agent_finding_key %in% removed))
  data$unknownPathogenNames <- data$unknownPathogenNames |>
    dplyr::filter(!(.data$agent_finding_key %in% removed))

  list(
    data    = data,
    changes = list(
      `1` = admission$changes,
      `2` = forms$changes,
      `5` = ssi$changes,
      `6` = sepsis$changes))
}

# The summary of the reconciliations: one row per registered reconciliation,
# in id order, with the record kind its level counts, and the distinct
# records of that kind in `x` that the log `log` lists as repaired and as
# reported. A record the import reconciled and then left out — outside the
# reporting period, or removed by a filter or by the validation pass — is not
# counted, so the summary describes the dataset it comes with. The counts are
# `NA` where the import could not read the records a reconciliation acts on,
# a tier its registry entry names being "no": a count of 0 says the
# reconciliation read its records and found none to change.
.reconciliation_summary <- function(log, x)
{
  opts  <- x$metadata$dataset_options
  kinds <- c(patient = "patients", enrollment = "enrollments", event = "events")
  keys  <- c(patient = "patient_key", enrollment = "enrollment_key",
             event = "event_key")

  rows <- purrr::map(reconciliations, \(entry) {
    key <- keys[[entry$level]]
    held <- x[[kinds[[entry$level]]]][[key]]
    count <- function(action) {
      if (!.reconciliation_runs(opts) || !.tiers_held(opts, entry$tiers))
        return(NA_integer_)
      acted <- log[[key]][log$reconciliation_id == entry$id &
                          log$action == action]
      length(intersect(acted, held))
    }
    tibble::tibble(
      reconciliation_id = entry$id,
      record_kind       = kinds[[entry$level]],
      n_repaired        = count("repair"),
      n_reported        = count("report"))
  })

  summary <- dplyr::bind_rows(rows) |>
    finalize_to_schema(reconciliationSummary_cols, opts)
  assert_schema(summary, reconciliationSummary_cols, opts)
  summary
}

#' Ids of the reconciliations
#'
#' The integer ids of every reconciliation [import_dhis2()] applies under
#' `reconcile = TRUE`, in ascending order; the documentation of `reconcile`
#' on [dhis2_dataset_options()] describes each under its id. A consumer that
#' keeps a label per reconciliation checks itself against this list.
#'
#' @returns An integer vector.
#' @family reconciliation
#' @export
reconciliation_ids <- function()
  vapply(reconciliations, \(r) r$id, integer(1))

#' List the records the reconciliations would repair
#'
#' Lists every record [import_dhis2()] would repair or report under
#' `reconcile = TRUE` (see [dhis2_dataset_options()]), on a dataset imported
#' with its values as stored, together with the stored value and what the
#' reconciliation would put in its place. It is the NeoIPC coordinating
#' centre's view of what the summary `reconciliationSummary` counts, record
#' by record.
#'
#' @param x A `neoipcr_ds` imported with every record and every value as
#'  stored: `reconcile = FALSE`; `include_invalid_patients = TRUE` and
#'  `include_ineligible_patients = TRUE`, since the validation pass and the
#'  eligibility filters would otherwise have removed records a
#'  reconciliation acts on; no range filter (`birth_weight_from`,
#'  `birth_weight_to`, `gestational_age_from`, `gestational_age_to`), since
#'  on stored values it judges other totals than the reconciled import's;
#'  no reporting period (`surveillance_end_from`, `surveillance_end_to`),
#'  since reconciliation 1 looks for a patient's earlier enrolments among all
#'  it read, the ones outside the period included; `include_patient`,
#'  `include_enrollment` and `include_event` set to `"full"`, since the
#'  pseudonymized tiers do not carry the keys and dates the reconciliations
#'  read; and the gestational age among `patient_columns` (an empty
#'  selection selects it). A dataset whose options predate `reconcile` holds
#'  its values as stored. Any other dataset is an error of class
#'  `neoipcr_reconciliation_needs_stored_values` naming what is missing.
#'
#' @returns A tibble with one row per record a reconciliation acts on,
#'  ordered by reconciliation and keys: `reconciliation_id`; `action`, a
#'  factor, `repair` for a record the import repairs and `report` for one it
#'  reports and keeps as stored; `patient_key`, `enrollment_key` and
#'  `event_key`, naming the record at its level and the ones above it, `NA`
#'  below it; and `context`, a list column holding a one-row tibble of the
#'  fields below. Zero rows when nothing would be reconciled.
#'
#' | Reconciliation | Level | Context fields |
#' |---|---|---|
#' | 1 | `enrollment_key` (`event_key` names the admission form) | `type`, `dol` and `dol_reconciled`, the admission type and the stored and reconciled day of life |
#' | 2 | `event_key` | `dol`, `dol_reconciled` |
#' | 3, 4 | `patient_key` | `gest_age`, `total_gestation_days` and `total_gestation_days_reconciled`, the text and the stored and reconciled total, missing for 4 |
#' | 5 | `event_key` | `sec_bsi`, and `slots` and `agents`, the secondary-BSI slots holding a value and the ones naming an infectious agent |
#' | 6 | `event_key` | `slots` and `agents` for the three primary slots, and `findings`, the features of generalized infection the clinical-sepsis definition counts, as validation rule 59 counts them |
#'
#' @family reconciliation
#' @export
reconciliation_details <- function(x)
{
  check_neoipcr_ds(x)
  opts <- x$metadata$dataset_options
  tiers <- c("include_patient", "include_enrollment", "include_event")
  range_filters <- c("birth_weight_from", "birth_weight_to",
                     "gestational_age_from", "gestational_age_to")
  period <- c("surveillance_end_from", "surveillance_end_to")
  given <- function(options)
    options[vapply(options, \(option) !is.null(opts[[option]]), logical(1))]
  unmet <- c(
    if (is.null(opts))
      gettext("The dataset carries no import options."),
    if (isTRUE(opts$reconcile))
      gettextf("%s is %s: the import repaired the values.",
               "`reconcile`", "`TRUE`"),
    if (!is.null(opts) && !isTRUE(opts$include_invalid_patients))
      gettextf("%s is not %s: the validation pass removed patients.",
               "`include_invalid_patients`", "`TRUE`"),
    if (!is.null(opts) && !isTRUE(opts$include_ineligible_patients))
      gettextf("%s is not %s: the eligibility filters removed patients and admissions.",
               "`include_ineligible_patients`", "`TRUE`"),
    if (!is.null(opts))
      purrr::map_chr(
        given(range_filters),
        \(option) gettextf("`%s` is set: the range filter removed patients by their stored values.", option)),
    if (!is.null(opts))
      purrr::map_chr(
        given(period),
        \(option) gettextf("`%s` is set: the reporting period removed enrolments.", option)),
    if (!is.null(opts))
      purrr::map_chr(
        tiers[vapply(tiers, \(tier) !identical(opts[[tier]], "full"), logical(1))],
        \(tier) gettextf("`%s` is \"%s\", not %s.", tier, opts[[tier]], "\"full\"")),
    if (!is.null(opts) &&
        !("gestational_age" %in% .selected_patient_columns(opts)))
      gettextf("%s leaves out %s.", "`patient_columns`", "\"gestational_age\""))
  if (length(unmet) > 0L)
    rlang::abort(c(
      gettext("Listing the reconciliations needs a dataset imported with every record and every value as stored."),
      rlang::set_names(unmet, rep("x", length(unmet))),
      i = gettextf(
        "Import with %s, %s, %s, no range filter and no reporting period, the full patient, enrolment, and event tiers, and %s among %s.",
        "`reconcile = FALSE`", "`include_invalid_patients = TRUE`",
        "`include_ineligible_patients = TRUE`", "\"gestational_age\"",
        "`patient_columns`")),
      class = "neoipcr_reconciliation_needs_stored_values")

  forms <- .reconcile_event_data(
    list(
      admissionData           = x$admissionData,
      sepsisData              = x$sepsisData,
      necData                 = x$necData,
      pneumoniaData           = x$pneumoniaData,
      surgeryData             = x$surgeryData,
      ssiData                 = x$ssiData,
      infectiousAgentFindings = x$infectiousAgentFindings,
      unknownPathogenNames    = x$unknownPathogenNames),
    frame = x$events |>
      dplyr::select("event_key", "event_type_key", "enrollment_key",
                    "patient_key", "occurredAt"),
    enrollments = x$enrollments |>
      dplyr::select("enrollment_key", "patient_key", "enrolledAt"),
    findings = x$infectiousAgentFindings |>
      dplyr::select("agent_finding_key", "event_key", "secondary_bsi",
                    "index", "pathogen_key"))
  changes <- c(.reconcile_gestational_age(x$patients)$changes, forms$changes)

  # A plain tibble, so that no context carries the class of the dataset slot
  # its values came from.
  purrr::map(reconciliations, \(entry)
    tibble::as_tibble(changes[[as.character(entry$id)]]) |>
      tidyr::nest(context = tidyselect::all_of(entry$context)) |>
      dplyr::select(
        "reconciliation_id", "action",
        tidyselect::all_of(.reconciliation_keys), "context")) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      action = factor(.data$action, levels = c("repair", "report"))) |>
    dplyr::arrange(
      .data$reconciliation_id, .data$patient_key, .data$enrollment_key,
      .data$event_key)
}
