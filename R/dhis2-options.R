#' Configure the DHIS2 dataset
#'
#' @param surveillance_end_from The earliest surveillance end date of patient
#'  records to include into the dataset.
#' @param surveillance_end_to The latest surveillance end date of patient
#'  records to include into the dataset.
#' @param birth_weight_from The lowest birth weight (in grams) of patient
#'  records to include into the dataset.
#' @param birth_weight_to The highest birth weight (in grams) of patient
#'  records to include into the dataset.
#' @param gestational_age_from The lowest gestational age (in completed weeks)
#'  of patient records to include into the dataset.
#' @param gestational_age_to The highest gestational age (in completed weeks) of
#'  patient records to include into the dataset. The bound covers the whole
#'  completed week: `31` keeps 31+0 through 31+6.
#' @param country_filter ISO 3166 country codes	of the countries the enrolling
#'  departments are located in to include into the dataset.
#' @param department_filter NeoIPC department codes of the departments to
#'  include into the dataset.
#' @param include_world_bank_class Include the World Bank class into the
#'  dataset. Possible values are "no", "pseudo" and "full"
#' @param include_country Include the country into the dataset. Possible values
#'  are "no", "pseudo" and "full"
#' @param include_hospital Include the hospital into the dataset. Possible
#'  values are "no", "pseudo" and "full"
#' @param include_department Include the department into the dataset. Possible
#'  values are "no", "pseudo" and "full"
#' @param include_user Include the user metadata into the dataset. Possible
#'  values are "no", "pseudo" and "full"
#' @param include_patient Include the patient tibble into the dataset and
#'  expose the `patient_key` link column on downstream tibbles. Possible values
#'  are "no", "pseudo" and "full". Under "no" the patient tibble is an empty
#'  (0-col, 0-row) tibble and `patient_key` is absent from every downstream
#'  tibble. Under "pseudo" the patient tibble carries only `patient_key` and
#'  `patient_key` is present on downstream tibbles. Under "full" the patient
#'  tibble also carries the columns selected by `patient_columns`.
#' @param patient_columns Character vector selecting which patient-specific
#'  columns beyond `patient_key` are included when `include_patient = "full"`.
#'  Choices: "id", "birth_weight", "sex", "delivery_mode", "siblings",
#'  "gestational_age", "inactive", "potentialDuplicate". Empty (the default) means
#'  all columns allowed by the gate. Ignored when `include_patient` is
#'  "no" or "pseudo".
#' @param include_enrollment Include the enrollment tibble into the dataset
#'  and expose the `enrollment_key` link column on downstream tibbles. Same
#'  three-mode semantics as `include_patient`.
#' @param include_event Include the event tibble into the dataset and expose
#'  the `event_key` link column on every downstream per-event table
#'  (event details, per-event-type data, findings, substance days, event
#'  notes, unknown pathogen names). Same three-mode semantics as
#'  `include_patient`.
#' @param include_dhis2_ids Include the DHIS2 ids into the dataset.
#' @param include_custom_attributes Include the values of the DHIS2 custom
#'  attributes set on organisation units into the dataset, as
#'  `metadata$departmentAttributeValues` and `metadata$hospitalAttributeValues`,
#'  with the attribute definitions in `metadata$orgUnitAttributes`. Possible
#'  values are "departments" and "hospitals"; an entity whose `include_*`
#'  option is "no" contributes nothing. Each value arrives typed by its
#'  attribute's DHIS2 value type. Custom attributes can hold personal data (a
#'  site's contact person), so requesting them under "pseudo" re-identifies the
#'  entity — the caller's explicit choice, as with `include_dhis2_ids`. The
#'  `IsTestunit` attribute is evaluated for test-unit detection regardless of
#'  this option and never appears in the values tables.
#' @param include_timestamps Include the createdAt and modifiedAt timestamps
#'  into the dataset.
#' @param include_ineligible_patients Include data from patients that don't meet
#'  the NeoIPC core case eligibility criteria into the dataset.
#' @param include_unenrolled_patients Include the NeoIPC patient records that
#'  are not enrolled in the surveillance program as well: they are requested
#'  by tracked-entity type rather than by program, and the removal of orphan
#'  records that follows an import leaves in place the patients that arrive
#'  without an enrollment (one that arrives with enrollments and loses them
#'  to a filter is pruned like any other). They reach the returned dataset
#'  only where the validation pass leaves them: rule 1 flags a patient with
#'  no enrollment, so under `include_invalid_patients = FALSE` the pass
#'  removes it first, and it stays with `include_invalid_patients = TRUE`
#'  or an exception naming it under rule 1 — the pairing the Validation
#'  Report uses. Without this option an import with enrollments removes
#'  every patient with no enrollment, the validation pass having flagged it
#'  first where it runs.
#' @param include_test_data Include data from test departments into the dataset.
#' @param include_invalid_patients Include data from patient records that
#'  could have validation errors: `FALSE` (the default) removes them, `TRUE`
#'  skips the validation pass altogether, and a data frame of exception
#'  records — as [read_validation_exceptions()] returns it — keeps the named
#'  records despite the rule that flags them. `TRUE` also keeps the
#'  enrolments without an admission form, which the removal of orphan
#'  records after the import otherwise drops, so a [validate()] on the
#'  returned dataset can report them under rule 26. An
#'  exception record carries `RULE_ID` (numeric), `NEOIPC_PATIENT_ID`
#'  (character), `ENROLMENT_DATE` and `EVENT_DATE` (`Date`), `EVENT_TYPE`
#'  (one of `adm`, `pro`, `bsi`, `nec`, `ssi`, `hap`, `end`, in any case),
#'  and `DEPARTMENT_CODE` when more than one department is imported; a
#'  record a rule flags at the enrollment level carries `NA` for both
#'  `EVENT_TYPE` and `EVENT_DATE`, or, where the rule shows its finding on
#'  a form, that form's type and date. The records are matched by patient id
#'  within their department, so a list needs `include_patient = "full"`
#'  (which then keeps `patient_id` whatever `patient_columns` says) and
#'  `include_department` not `"no"`. The import resolves the list under
#'  either remaining department tier, since it holds the department codes
#'  while it runs; the returned dataset carries them under the full tier
#'  only, so resolving the same list on it with
#'  [resolve_validation_exceptions()] needs `include_department = "full"`
#'  when more than one department was imported. An exception keeps a record
#'  from the validation pass, not from the shape of the dataset: a patient
#'  without any enrolment (rule 1) stays in the returned dataset only when
#'  `include_unenrolled_patients` asks for such patients; otherwise the
#'  removal of every patient with no enrollment that follows an import with
#'  enrollments takes it, and its exception only stops the pass from
#'  reporting it. Validation is patient-anchored: with
#'  `include_patient = "no"` there is nothing to validate, the pass is
#'  skipped and a list is not read. When it does run — patients present and
#'  this option not `TRUE` — it needs `include_enrollment` and
#'  `include_event` set to `"full"`, and an import asking for less, or
#'  handing over a malformed exception list, aborts before its first
#'  request; `TRUE` imposes no such requirement.
#' @param include_incomplete Include incomplete records into the dataset.
#'  Possible values are "enrollments" and "events"
#' @param include_notes Include notes into the dataset. Possible values are
#'  "enrollments" and "events"
#' @param include_deleted Include deleted records into the dataset.
#' @param trial_keys Only include date for the trials listed in this variable.
#' @param translate Translate DHIS2 metadata
#' @param locale The locale to translate DHIS2 metadata to
#'
#' @export
dhis2_dataset_options <- function(
    surveillance_end_from = NULL,
    surveillance_end_to = NULL,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestational_age_from = NULL,
    gestational_age_to = NULL,
    country_filter = NULL,
    department_filter = NULL,
    include_world_bank_class = c("no","pseudo","full"),
    include_country = c("no","pseudo","full"),
    include_hospital = c("no","pseudo","full"),
    include_department = c("no","pseudo","full"),
    include_user = c("no","pseudo","full"),
    include_patient = c("no","pseudo","full"),
    patient_columns = character(),
    include_enrollment = c("no","pseudo","full"),
    include_event = c("no","pseudo","full"),
    include_dhis2_ids = character(),
    include_custom_attributes = character(),
    include_timestamps = FALSE,
    include_test_data = FALSE,
    include_ineligible_patients = FALSE,
    include_unenrolled_patients = FALSE,
    include_invalid_patients = FALSE,
    include_incomplete = character(),
    include_notes = character(),
    include_deleted = FALSE,
    trial_keys = NULL,
    translate = TRUE,
    locale = NULL)
{
  if(is.character(birth_weight_from)) birth_weight_from <- as.integer(birth_weight_from)
  if(is.character(birth_weight_to)) birth_weight_to <- as.integer(birth_weight_to)
  if(is.character(gestational_age_from)) gestational_age_from <- as.integer(gestational_age_from)
  if(is.character(gestational_age_to)) gestational_age_to <- as.integer(gestational_age_to)

  check_number_whole(birth_weight_from, allow_null = TRUE)
  check_number_whole(birth_weight_to, allow_null = TRUE)
  check_number_whole(gestational_age_from, allow_null = TRUE)
  check_number_whole(gestational_age_to, allow_null = TRUE)
  check_character(country_filter, allow_null = TRUE)
  check_character(department_filter, allow_null = TRUE)
  check_character(patient_columns)
  check_character(include_custom_attributes)
  check_bool(include_timestamps)
  check_bool(include_test_data)
  check_bool(include_ineligible_patients)
  check_bool(include_unenrolled_patients)
  #check_bool(include_invalid_patients) # ToDo: validate
  check_bool(include_deleted)
  check_bool(translate)

  if(!is.null(surveillance_end_from))
    surveillance_end_from <- as.Date(surveillance_end_from)

  if(!is.null(surveillance_end_to))
    surveillance_end_to <- as.Date(surveillance_end_to)

  structure(list(
    surveillance_end_from = surveillance_end_from,
    surveillance_end_to = surveillance_end_to,
    birth_weight_from = birth_weight_from,
    birth_weight_to = birth_weight_to,
    gestational_age_from = gestational_age_from,
    gestational_age_to = gestational_age_to,
    country_filter = country_filter,
    department_filter = department_filter,
    include_world_bank_class = rlang::arg_match(include_world_bank_class),
    include_country = rlang::arg_match(include_country),
    include_hospital = rlang::arg_match(include_hospital),
    include_department = rlang::arg_match(include_department),
    include_user = rlang::arg_match(include_user),
    include_patient = rlang::arg_match(include_patient),
    patient_columns = rlang::arg_match(
      patient_columns,
      c("id","birth_weight","sex","delivery_mode","siblings","gestational_age",
        "inactive","potentialDuplicate"),
      multiple = TRUE),
    include_enrollment = rlang::arg_match(include_enrollment),
    include_event = rlang::arg_match(include_event),
    include_dhis2_ids = rlang::arg_match(
      include_dhis2_ids,
      c("countries","hospitals","departments","patients","enrollments",
        "events","notes","event_types","users"),
      multiple = TRUE),
    include_custom_attributes = rlang::arg_match(
      include_custom_attributes,
      c("departments", "hospitals"),
      multiple = TRUE),
    include_timestamps = include_timestamps,
    include_test_data = include_test_data,
    include_ineligible_patients = include_ineligible_patients,
    include_unenrolled_patients = include_unenrolled_patients,
    include_invalid_patients = include_invalid_patients,
    include_incomplete = rlang::arg_match(
      include_incomplete,
      c("enrollments","events"),
      multiple = TRUE),
    include_notes = rlang::arg_match(
      include_notes,
      c("enrollments","events"),
      multiple = TRUE),
    include_deleted = include_deleted,
    trial_keys = trial_keys,
    translate = translate,
    locale = locale
    # Inherit "list" so jsonlite (and other serialisers) handle it as its
    # underlying list — it is a structure(list(...)) — without needing a bespoke
    # asJSON method; every type check uses inherits(), so the extra class is
    # transparent to them.
  ), class = c("neoipcr_dhis2_dsopt", "list"))
}

# The copy of a dataset's options that a calculated dataset carries out of
# the package. An exception list is a data frame of patient ids and enrolment
# dates, and a department filter names the departments behind reference
# values, so each is replaced by a marker saying it was applied: the
# calculated dataset records that a list was used without carrying it, and
# reference data records that it was filtered without saying to what. A
# department dataset keeps its filter, which is its own department. The
# import applies a filter only when it names a department, so an empty one
# leaves as `NULL` rather than as the marker; the element stays in place,
# which `$<-` with `NULL` would not do.
serializable_dataset_options <- function(opts, keep_department_filter)
{
  if (is.data.frame(opts$include_invalid_patients))
    opts$include_invalid_patients <- "exception_list_applied"
  if (!keep_department_filter && !is.null(opts$department_filter))
    opts["department_filter"] <- list(
      if (length(opts$department_filter) > 0L) "applied")
  opts
}
