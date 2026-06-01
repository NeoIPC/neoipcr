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
#'  dataset. Possible values are "no", "pseudonymised" and "yes"
#' @param include_country Include the country into the dataset. Possible values
#'  are "no", "pseudonymised" and "yes"
#' @param include_hospital Include the hospital into the dataset. Possible
#'  values are "no", "pseudonymised" and "yes"
#' @param include_department Include the department into the dataset.
#'  Possible values are "no", "pseudonymised" and "yes"
#' @param include_user Include the user metadata into the dataset. Possible
#'  values are "no", "pseudonymised" and "yes"
#' @param include_patient_id Include the NeoIPC Patient ID into the dataset.
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
    include_world_bank_class = c("no","pseudonymised","yes"),
    include_country = c("no","pseudonymised","yes"),
    include_hospital = c("no","pseudonymised","yes"),
    include_department = c("no","pseudonymised","yes"),
    include_user = c("no","pseudonymised","yes"),
    include_patient_id = FALSE,
    include_dhis2_ids = rlang::chr(),
    include_timestamps = FALSE,
    include_test_data = FALSE,
    include_ineligible_patients = FALSE,
    include_unenrolled_patients = FALSE,
    include_invalid_patients = FALSE,
    include_incomplete = rlang::chr(),
    include_notes = rlang::chr(),
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
  check_bool(include_patient_id)
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
    include_patient_id = include_patient_id,
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
      skipPaging = "true",
      includeDeleted = tolower(dataset_options$include_deleted))

  # Push org unit filters to the API to reduce network traffic and memory use.
  # metadata$departments and metadata$countries are already filtered during
  # metadata processing (read_metadata_reponses), so we can use them directly.
  #
  # The /tracker/events endpoint only accepts a single orgUnit UID (through
  # DHIS2 2.42), so event requests are made per org unit while trackedEntities
  # and enrollments use multi-UID parameters. All requests run in parallel.
  #
  # DHIS2 2.41 renames ouMode -> orgUnitMode and orgUnit (singular, semicolon-
  # separated) -> orgUnits (plural, comma-separated). The old parameters are
  # scheduled for removal in 2.42.
  # See: https://docs.dhis2.org/en/implement/software-release-information/
  #   dhis2-core-releases/dhis-core-version-241/upgrade-notes.html
  #   #semicolon-as-separator-for-identifiers-uid
  #
  # On 2.40, semicolons in multi-UID orgUnit values must be URL-encoded (%3B)
  # — literal semicolons are parsed as parameter delimiters by the servlet
  # container. httr2 does not encode semicolons (valid per RFC 3986), so we
  # pre-encode them and wrap in I() to prevent double-encoding.
  v41 <- metadata$system$version >= "2.41"
  mode_key <- if (v41) "orgUnitMode" else "ouMode"
  ou_key   <- if (v41) "orgUnits" else "orgUnit"

  ou_query <- function(mode, ou_value)
    stats::setNames(list(mode, ou_value), c(mode_key, ou_key))

  multi_uid <- function(ids)
    if (v41) paste0(ids, collapse = ",")
    else I(paste0(ids, collapse = "%3B"))

  if (length(dataset_options$department_filter) > 0) {
    dept_ids <- metadata$departments |> dplyr::pull(.data$orgUnit)

    te_enrl_req <- tracker_req |>
      httr2::req_url_query(!!!ou_query("SELECTED", multi_uid(dept_ids)))

    event_reqs <- lapply(dept_ids, \(id) tracker_req |>
      httr2::req_url_query(!!!ou_query("SELECTED", id)))

  } else if (length(dataset_options$country_filter) > 0) {
    country_ids <- metadata$countries |> dplyr::pull(.data$country)

    te_enrl_req <- tracker_req |>
      httr2::req_url_query(!!!ou_query("DESCENDANTS", multi_uid(country_ids)))

    event_reqs <- lapply(country_ids, \(id) tracker_req |>
      httr2::req_url_query(!!!ou_query("DESCENDANTS", id)))

  } else {
    te_enrl_req <- tracker_req |>
      httr2::req_url_query(!!!stats::setNames(list("ACCESSIBLE"), mode_key))
    event_reqs <- list(te_enrl_req)
  }

  reqs <- c(
    list(
      get_trackedEntities_request(
        te_enrl_req,
        dataset_options,
        metadata$programId,
        metadata$trackedEntityTypeId),
      get_enrollments_request(
        te_enrl_req, dataset_options, metadata$programId)),
    lapply(event_reqs, \(req)
      get_events_request(req, dataset_options, metadata$programId)))

  resps <- reqs |>
    httr2::req_perform_parallel(progress = FALSE, on_error = "continue")

  # Check for HTTP errors and surface the response body.
  # With on_error="continue", failed requests are stored as error objects
  # (class httr2_error), not response objects.
  endpoints <- c("trackedEntities", "enrollments",
    rep("events", length(event_reqs)))
  for (i in seq_along(resps)) {
    if (rlang::is_error(resps[[i]])) {
      err <- resps[[i]]
      resp <- err$resp
      body <- tryCatch(
        httr2::resp_body_string(resp),
        error = \(e) conditionMessage(err))
      status <- tryCatch(
        httr2::resp_status(resp),
        error = \(e) "unknown")
      rlang::abort(paste0(
        "DHIS2 tracker/", endpoints[i], " returned HTTP ", status, ".\n",
        "Response body:\n", body))
    }
  }

  parse_resp <- \(resp) {
    tbl <- httr2::resp_body_json(resp) |>
      tibble::tibble() |>
      tidyr::unnest_longer(1)
    if (nrow(tbl) == 0 || ncol(tbl) == 0)
      return(tibble::tibble())
    tidyr::unnest_wider(tbl, 1)
  }

  trackedEntities_raw <- parse_resp(resps[[1]])
  if (nrow(trackedEntities_raw) == 0) {
    filter_summary <- if (!is.null(dataset_options$department_filter))
      paste0("department_filter: ",
             paste(dataset_options$department_filter, collapse = ", "))
    else if (!is.null(dataset_options$country_filter))
      paste0("country_filter: ",
             paste(dataset_options$country_filter, collapse = ", "))
    else
      "active organisation units: <accessible>"
    rlang::abort(c(
      "No tracked entities returned by DHIS2.",
      "i" = "The selected organisation unit(s) may have no enrolled patients.",
      "i" = filter_summary))
  }
  enrollments_raw <- parse_resp(resps[[2]])
  events_raw <- resps[seq(3, length(resps))] |>
    purrr::map(parse_resp) |>
    purrr::list_rbind()

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

  metadata$dataset_options <- dataset_options

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

  r <- structure(
    list(
      patients = patients,
      enrollments = enrollments,
      events = events,
      eventDetails = eventDetails,
      eventNotes = eventNotes,
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
    class = c("neoipcr_ds", "list"))

  if(!rlang::is_bool(dataset_options$include_invalid_patients) ||
     dataset_options$include_invalid_patients == FALSE)
  {
    if(!rlang::is_bool(dataset_options$include_invalid_patients))
      exceptions <- dataset_options$include_invalid_patients |>
        transform_user_exceptions(r)
    else exceptions <- NULL

    v <- r |> validate(exceptions = exceptions)
    r$validationResults <- v
    r$patients <- r$patients |>
      dplyr::anti_join(v, dplyr::join_by("patient_key"))
  }

  r <- r |>
    apply_postfilter()

  r |>
    apply_data_removal(dataset_options)
}

is_single_department <- function(ds) ds$metadata$departments |>
  dplyr::pull("code") |>
  length() == 1

transform_user_exceptions <- function(ex, ds)
{
  ex <- ex |>
    dplyr::mutate(
      event_type_key = factor(
        tolower(.data$EVENT_TYPE),
        levels = c("adm","pro","bsi","nec","ssi","hap","end")),
      .keep = "unused")

  if(is_single_department(ds))
    ex <- ex |>
      dplyr::left_join(
        ds$patients |>
          dplyr::select("patient_key","patient_id"),
        dplyr::join_by("NEOIPC_PATIENT_ID"=="patient_id")) |>
      dplyr::left_join(
        ds$enrollments |>
          dplyr::select("patient_key","enrollment_key","enrolledAt"),
        dplyr::join_by("patient_key","ENROLMENT_DATE"=="enrolledAt"))
  else
    ex <- ex |>
      dplyr::inner_join(
        ds$metadata$departments |>
          dplyr::select("department_key","code"),
        dplyr::join_by("DEPARTMENT_CODE" == "code")) |>
      dplyr::left_join(
        ds$patients |>
          dplyr::select("department_key","patient_key","patient_id"),
        dplyr::join_by("department_key","NEOIPC_PATIENT_ID"=="patient_id")) |>
      dplyr::left_join(
        ds$enrollments |>
          dplyr::select("department_key","patient_key","enrollment_key","enrolledAt"),
        dplyr::join_by("department_key","patient_key","ENROLMENT_DATE"=="enrolledAt"))

  ex |>
    dplyr::left_join(
      ds$events |>
        dplyr::select("event_key","enrollment_key","event_type_key","occurredAt"),
      dplyr::join_by("enrollment_key","event_type_key","EVENT_DATE"=="occurredAt")) |>
    dplyr::select("rule_id"="RULE_ID",tidyselect::any_of("department_key"),"patient_key","enrollment_key","event_key")
}

dhis2_request <- function(connection_options)
{
  req <- httr2::request(connection_options$base_url)
  if(!is.null(connection_options$token))
    req |>
    httr2::req_headers(
      Authorization = sprintf("ApiToken %s", connection_options$token),
      .redact = "Authorization")
  else if(!is.null(connection_options$session_id))
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
  # Two-level tryCatch: inner level translates specific HTTP errors into

  # user-friendly messages; outer level catches everything else (DNS failure,
  # timeout, etc.) and wraps with a generic connection message.  The outer

  # handler passes through errors already translated by the inner level.
  resp <- tryCatch(
    tryCatch(
      req |>
        httr2::req_url_path_append("me") |>
        httr2::req_url_query(
          fields = "id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[name,authorities],userGroups[name]") |>
        httr2::req_perform(),
      httr2_http_401 = function(cnd) {
        rlang::abort(c(
          sprintf("DHIS2 authentication failed (HTTP 401) at %s.", req$url),
          i = "Check that your token or username/password is correct.",
          i = "Token auth: set the NEOIPC_DHIS2_TOKEN environment variable.",
          i = "Basic auth: set NEOIPC_DHIS2_USER and NEOIPC_DHIS2_PASSWORD environment variables."
        ), class = "neoipcr_dhis2_error", call = NULL)
      },
      httr2_http_403 = function(cnd) {
        rlang::abort(c(
          sprintf("DHIS2 access denied (HTTP 403) at %s.", req$url),
          i = "Your credentials were accepted but you lack permission to access /api/me.",
          i = "Contact a DHIS2 administrator to check your user role."
        ), class = "neoipcr_dhis2_error", call = NULL)
      }
    ),
    error = function(cnd) {
      if (inherits(cnd, "neoipcr_dhis2_error")) stop(cnd)
      rlang::abort(c(
        sprintf("Failed to connect to DHIS2 at %s.", req$url),
        i = "Check your network connection and DHIS2 server URL.",
        i = conditionMessage(cnd)
      ), class = "neoipcr_dhis2_error", call = NULL)
    }
  )

  raw_info <- tryCatch(
    resp |>
      httr2::resp_check_status() |>
      httr2::resp_body_json(simplifyVector = TRUE),
    error = function(cnd) {
      ct <- httr2::resp_content_type(resp)
      sc <- httr2::resp_status(resp)
      url <- resp$url
      if (grepl("text/html", ct, fixed = TRUE)) {
        rlang::abort(c(
          sprintf("DHIS2 returned an HTML page instead of JSON (HTTP %d, URL: %s).", sc, url),
          i = "This usually means the server redirected to a login page.",
          i = "Your credentials may be missing, expired, or incorrect.",
          i = "Token auth: set the NEOIPC_DHIS2_TOKEN environment variable.",
          i = "Basic auth: set NEOIPC_DHIS2_USER and NEOIPC_DHIS2_PASSWORD environment variables."
        ), call = NULL)
      }
      rlang::abort(c(
        sprintf("Unexpected DHIS2 response content type: %s", ct),
        i = conditionMessage(cnd)
      ), parent = cnd)
    }
  )

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

add_key_column <- function(table, key_name = "key")
{
  table |>
    dplyr::mutate(random = ids::random_id(nrow(table))) |>
    dplyr::arrange(.data$random) |>
    dplyr::select(!"random") |>
    dplyr::mutate(!!key_name := dplyr::row_number()) |>
    dplyr::relocate(dplyr::all_of(key_name))
}

convert_value <- function(values, valueTypes, levelsLists)
{
  convertedValues <- vector(mode = "list", length = length(values))
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

    convertedValues[[i]] <- value
  }
  return(convertedValues)
}
