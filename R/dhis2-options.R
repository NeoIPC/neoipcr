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
#'  patient records to include into the dataset
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
#' @param include_timestamps Include the createdAt and modifiedAt timestamps
#'  into the dataset.
#' @param include_ineligible_patients Include data from patients that don't meet
#'  the NeoIPC core case eligibility criteria into the dataset.
#' @param include_unenrolled_patients Include data from unenrolled NeoIPC
#'  patient records into the dataset.
#' @param include_test_data Include data from test departments into the dataset.
#' @param include_invalid_patients Include data from patient records that
#'  could have validation errors
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
