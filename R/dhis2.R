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
#' @param include_world_bank_class Include the World Bank class into the
#'  dataset. Possible values are "no", "pseudonymised" and "yes"
#' @param include_country Include the country into the dataset. Possible values
#'  are "no", "pseudonymised" and "yes"
#' @param include_hospital Include the hospital into the dataset. Possible
#'  values are "no", "pseudonymised" and "yes"
#' @param include_department Include the World Bank class into the dataset.
#'  Possible values are "no", "pseudonymised" and "yes"
#' @param include_user Include the World Bank class into the dataset. Possible
#'  values are "no", "pseudonymised" and "yes"
#' @param include_patient_id Include the NeoIPC Patient ID into the dataset.
#' @param include_dhis2_id Include the DHIS2 ids into the dataset.
#' @param include_timestamps Include the createdAt and modifiedAt timestamps
#'  into the dataset.
#' @param include_ineligible_patients Include data from patients that don't meet
#'  the NeoIPC core case eligibility criteria into the dataset.
#' @param include_unenrolled_patients Include data from unenrolled NeoIPC
#'  patient records into the dataset.
#' @param include_test_data Include data from test units into the dataset.
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
    include_world_bank_class = c("no","pseudonymised","yes"),
    include_country = c("no","pseudonymised","yes"),
    include_hospital = c("no","pseudonymised","yes"),
    include_department = c("no","pseudonymised","yes"),
    include_user = c("no","pseudonymised","yes"),
    include_patient_id = FALSE,
    include_dhis2_id = FALSE,
    include_timestamps = FALSE,
    include_test_data = FALSE,
    include_ineligible_patients = FALSE,
    include_unenrolled_patients = FALSE,
    include_incomplete = rlang::chr(),
    include_notes = rlang::chr(),
    include_deleted = FALSE,
    trial_keys = NULL,
    translate = TRUE,
    locale = NULL)
{
  check_number_whole(birth_weight_from, allow_null = TRUE)
  check_number_whole(birth_weight_to, allow_null = TRUE)
  check_number_whole(gestational_age_from, allow_null = TRUE)
  check_number_whole(gestational_age_to, allow_null = TRUE)
  check_character(country_filter, allow_null = TRUE)
  check_bool(include_patient_id)
  check_bool(include_dhis2_id)
  check_bool(include_timestamps)
  check_bool(include_test_data)
  check_bool(include_ineligible_patients)
  check_bool(include_unenrolled_patients)
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
    include_world_bank_class = rlang::arg_match(include_world_bank_class),
    include_country = rlang::arg_match(include_country),
    include_hospital = rlang::arg_match(include_hospital),
    include_department = rlang::arg_match(include_department),
    include_user = rlang::arg_match(include_user),
    include_patient_id = include_patient_id,
    include_dhis2_id = include_dhis2_id,
    include_timestamps = include_timestamps,
    include_test_data = include_test_data,
    include_ineligible_patients = include_ineligible_patients,
    include_unenrolled_patients = include_unenrolled_patients,
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
  ), class = "neoipcr_dhis2_dsopt")
}

#' Import data from a NeoIPC DHIS2 server
#'
#' @param connection_options The options to use for connecting to the DHIS2
#'  server.
#' @param dataset_options The options to use for the dataset configuration
#'
#' @returns A NeoIPC dataset.
#' @export
import_dhis2 <- function(
    connection_options = dhis2_connection_options(),
    dataset_options = dhis2_dataset_options())
{
  check_neoipcr_dhis2_conopt(connection_options)
  check_neoipcr_dhis2_dsopt(dataset_options)

  d2req_base <- dhis2_request(connection_options)

  user_info <- d2req_base |>
    get_user_info()

  metadata <- get_metadata(d2req_base, user_info, dataset_options)

  tracker_req <- d2req_base |>
    httr2::req_url_path_append("tracker") |>
    httr2::req_url_query(
      ouMode ="ACCESSIBLE",
      skipPaging = "true",
      includeDeleted = tolower(dataset_options$include_deleted))
  reqs <- list(
    get_trackedEntities_request(
      tracker_req,
      dataset_options,
      metadata$programId,
      metadata$trackedEntityTypeId),
    get_enrollments_request(tracker_req, dataset_options, metadata$programId),
    get_events_request(tracker_req, dataset_options, metadata$programId))

  data <-  reqs |>
    httr2::req_perform_parallel() |>
    httr2::resps_data(\(resp){
      list(httr2::resp_body_json(resp) |>
             tibble::tibble() |>
             tidyr::unnest_longer(1) |>
             tidyr::unnest_wider(1))})

  trackedEntities_raw <- data[[1]]
  enrollments_raw <- data[[2]]
  events_raw <- data[[3]]

  patients <- read_patients(trackedEntities_raw, metadata, dataset_options)
  enrollments <- read_enrollments(enrollments_raw, patients, metadata, dataset_options)
  events <- read_events(events_raw, enrollments, patients, metadata, dataset_options)
  admissionData <- read_event_data(events_raw, events, metadata, dataset_options, "adm")

  events <- events |>
    filter_surveillance_ends(
      dataset_options$surveillance_end_from,
      dataset_options$surveillance_end_to)

  admissionData <- admissionData |>
    filter_admissions(dataset_options$include_ineligible_patients)

  patients <- patients |>
    filter_patients(
      dataset_options$birth_weight_from,
      dataset_options$birth_weight_to,
      dataset_options$gestational_age_from,
      dataset_options$gestational_age_to,
      dataset_options$include_ineligible_patients)

  metadata$countries <- metadata$countries |>
    filter_countries(dataset_options$country_filter)

  # read_enrollment_details
  # read_enrollment_notes
  eventDetails <- read_event_details(events_raw, events, metadata, dataset_options)
  eventNotes <- read_event_notes(events_raw, events, metadata, dataset_options)
  surveillanceEndData <- read_event_data(events_raw, events, metadata, dataset_options, "end")
  sepsisData <- read_event_data(events_raw, events, metadata, dataset_options, "bsi")
  necData <- read_event_data(events_raw, events, metadata, dataset_options, "nec")
  pneumoniaData <- read_event_data(events_raw, events, metadata, dataset_options, "hap")
  surgeryData <- read_event_data(events_raw, events, metadata, dataset_options, "pro")
  ssiData <- read_event_data(events_raw, events, metadata, dataset_options, "ssi")

  infectiousAgentFindings <- read_infectious_agent_findings(events_raw, events, metadata, dataset_options)
  # read_infectious_agent_findings_details
  substanceDays <- read_substance_days(events_raw, events, metadata, dataset_options)
  # read_substance_days_details

  class(patients) <- c("neoipcr_pat", class(patients))
  class(enrollments) <- c("neoipcr_enr", class(enrollments))
  class(events) <- c("neoipcr_evt", class(events))
  class(eventDetails) <- c("neoipcr_evd", class(eventDetails))
  if(!is.null(eventNotes))
    class(eventNotes) <- c("neoipcr_evn", class(eventNotes))
  class(admissionData) <- c("neoipcr_adm", class(admissionData))
  class(surveillanceEndData) <- c("neoipcr_end", class(surveillanceEndData))
  class(surgeryData) <- c("neoipcr_pro", class(surgeryData))
  class(sepsisData) <- c("neoipcr_bsi", class(sepsisData))
  class(necData) <- c("neoipcr_nec", class(necData))
  class(ssiData) <- c("neoipcr_ssi", class(ssiData))
  class(pneumoniaData) <- c("neoipcr_hap", class(pneumoniaData))
  class(substanceDays) <- c("neoipcr_sbd", class(substanceDays))
  class(infectiousAgentFindings) <- c("neoipcr_iaf", class(infectiousAgentFindings))
  class(metadata) <- c("neoipcr_metadata", class(metadata))

  structure(
    list(
      patients = patients,
      enrollments = enrollments,
      events = events,
      admissionData = admissionData,
      surveillanceEndData = surveillanceEndData,
      sepsisData = sepsisData,
      necData = necData,
      pneumoniaData = pneumoniaData,
      surgeryData = surgeryData,
      ssiData = ssiData,
      substanceDays = substanceDays,
      infectiousAgentFindings = infectiousAgentFindings,
      metadata = metadata,
      `.cache` = new.env(parent = emptyenv())),
    class = c("neoipcr_ds", "list")) |>
    apply_postfilter() |>
    apply_data_removal(dataset_options)
}

dhis2_request <- function(connection_options)
{
  req <- httr2::request(connection_options$base_url)
  if(exists('token', where = connection_options))
    req |>
    httr2::req_headers(
      Authorization = sprintf("ApiToken %s", connection_options$token),
      .redact = "Authorization")
  else if(exists('session_id', where = connection_options))
    req |>
    httr2::req_cookies_set(JSESSIONID = connection_options$session_id)
  else
    req |>
    httr2::req_auth_basic(
      username = connection_options$username,
      password = connection_options$password)
}

get_user_info <- function(req)
{
  raw_info <- req |>
    httr2::req_url_path_append("me") |>
    httr2::req_url_query(
      fields = "id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[name,authorities],userGroups[name]") |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  structure(list(
    id = raw_info$id,
    username = raw_info$username,
    firstName = raw_info$firstName,
    surname = raw_info$surname,
    email = raw_info$email,
    lastLogin = readr::parse_datetime(raw_info$userCredentials$lastLogin),
    created = readr::parse_datetime(raw_info$created),
    organisationUnits = raw_info$organisationUnits$id,
    dataViewOrganisationUnits = raw_info$dataViewOrganisationUnits$id,
    teiSearchOrganisationUnits = raw_info$teiSearchOrganisationUnits$id,
    groups = raw_info$userGroups$name |>
      sort(),
    roles = raw_info$userRoles$name |>
      sort(),
    authorities = raw_info$userRoles$authorities |>
      unlist() |>
      unique() |>
      sort()
  ), class = c("neoipc_dhis2_usrinfo", "list"))
}

add_key_column <- function(table, key_name = "key", as_factor = FALSE)
{
  tmp <- table |>
    dplyr::mutate(random = ids::random_id(nrow(table))) |>
    dplyr::arrange(.data$random) |>
    dplyr::select(!"random")

  if (as_factor) tmp <- tmp |>
      dplyr::mutate(!!key_name := as.factor(dplyr::row_number()))
  else tmp <- tmp |>
      dplyr::mutate(!!key_name := dplyr::row_number())

  tmp |>
    dplyr::relocate(dplyr::all_of(key_name))
}

convert_value <- function(values, valueTypes, levelsLists)
{
  ret <- NULL
  for (i in seq_along(values)) {
    value <- values[i]
    valueType <- valueTypes[i]
    levels <- unlist(levelsLists[i])
    if(!is.null(levels))
      value <- factor(value, levels = levels)
    else if (stringr::str_starts(valueType, "INTEGER"))
      value <- as.integer(value)
    else if (valueType == "BOOLEAN" || valueType == "TRUE_ONLY")
      value <- as.logical(value)

    ret <- c(ret, list(value))
  }
  ret
}
