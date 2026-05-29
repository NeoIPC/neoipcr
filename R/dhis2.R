#' Import a NeoIPC dataset from a DHIS2 instance
#'
#' Fetches metadata, tracked entities, enrollments and events from a
#' DHIS2 NeoIPC instance and returns them as a `neoipcr_ds` (a list of
#' tibbles representing patients, enrollments, events, etc.).
#'
#' @param connection_options A `neoipcr_dhis2_conopt` object describing
#'   how to connect to the DHIS2 instance. Use [dhis2_connection_options()].
#' @param translate Whether to translate metadata into the configured locale.
#' @param locale Optional locale identifier passed to DHIS2.
#' @param include_deleted Whether to include deleted records.
#' @return A `neoipcr_ds` list of tibbles.
#' @export
import_dhis2 <- function(connection_options = dhis2_connection_options(), translate = TRUE, locale = NULL, include_deleted = FALSE)
{
  check_neoipcr_dhis2_conopt(connection_options)

  d2req_base <- dhis2_request(connection_options)

  user_info <- d2req_base |>
    get_user_info()

  metadata <- get_metadata(d2req_base, user_info, translate, locale)

  reqs <- list()

  tracker_req <- d2req_base |>
    httr2::req_url_path_append("tracker") |>
    httr2::req_url_query(
      ouMode ="ACCESSIBLE",
      skipPaging = "true",
      includeDeleted = tolower(include_deleted))

  reqs <- append(
    reqs,
    list(
      tracker_req |>
        httr2::req_url_path_append("trackedEntities") |>
        httr2::req_url_query(
          trackedEntityType = metadata$trackedEntityTypeId,
          fields = "trackedEntity,createdAt,createdAtClient,updatedAt,updatedAtClient,orgUnit,inactive,deleted,createdBy[username],updatedBy[username],potentialDuplicate,attributes[code,value]")))

  reqs <- append(
    reqs,
    list(
      tracker_req |>
        httr2::req_url_path_append("enrollments") |>
        httr2::req_url_query(
          fields = "enrollment,createdAt,createdAtClient,updatedAt,updatedAtClient,trackedEntity,status,orgUnit,enrolledAt,occurredAt,followUp,deleted,completedAt,completedBy,storedBy,createdBy[username],updatedBy[username],notes")))

  reqs <- append(
    reqs,
    list(
      tracker_req |>
        httr2::req_url_path_append("events") |>
        httr2::req_url_query(fields = "event,status,programStage,enrollment,trackedEntity,orgUnit,scheduledAt,occurredAt,completedAt,followup,deleted,createdAt,createdAtClient,updatedAt,updatedAtClient,storedBy,createdBy[username],updatedBy[username],notes,dataValues[dataElement,value,createdAt,updatedAt,createdBy[username]]")))

  data <-  reqs |>
    httr2::req_perform_parallel() |>
    httr2::resps_data(function(resp) list(httr2::resp_body_json(resp) |> tibble::tibble() |> tidyr::unnest_longer(1) |> tidyr::unnest_wider(1)))

  trackedEntities <- data[[1]]
  enrollments <- data[[2]]
  events <- data[[3]]

  patients <- read_patients(trackedEntities, metadata)
  enrollments <- read_enrollments(enrollments, events, metadata, patients)
  ab_treatments <- read_ab_treatments(events, metadata, enrollments)
  surgeries <- read_eventData(
    events, metadata, "Surgical Procedure", keyColumn = "surgery_key") |>
    recode_enrollments(enrollments)
  sepses <- read_eventData(
    events, metadata, "Primary Sepsis/BSI", keyColumn = "sepsis_key",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  necs <- read_eventData(
    events, metadata, "Necrotizing enterocolitis", keyColumn = "nec_key",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_NEC_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  ssis <- read_eventData(
    events, metadata, "Surgical Site Infection", keyColumn = "ssi_key",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_SSI_PATHOGEN", TRUE) &
      stringr::str_starts(x, "NEOIPC_SSI_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  pneumonias <- read_eventData(
    events, metadata, "Pneumonia", keyColumn = "pneumonia_key",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_HAP_PATHOGEN", TRUE) &
      stringr::str_starts(x, "NEOIPC_HAP_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  causative_pathogens <- read_causative_pathogens(events, metadata) |>
    recode_events(list(sepses, necs, ssis, pneumonias))

  sepses <- sepses |>
    infer_sepsis_types(causative_pathogens)

  class(patients) <- c("neoipcr_pat", class(patients))
  class(enrollments) <- c("neoipcr_enr", class(enrollments))
  class(ab_treatments) <- c("neoipcr_trt", class(ab_treatments))
  class(surgeries) <- c("neoipcr_srg", class(surgeries))
  class(sepses) <- c("neoipcr_sep", class(sepses))
  class(necs) <- c("neoipcr_nec", class(necs))
  class(ssis) <- c("neoipcr_ssi", class(ssis))
  class(pneumonias) <- c("neoipcr_pne", class(pneumonias))
  class(causative_pathogens) <- c("neoipcr_cspg", class(causative_pathogens))
  class(metadata) <- c("neoipcr_metadata", class(metadata))

  structure(
    list(
      patients = patients,
      enrollments = enrollments,
      sepses = sepses,
      necs = necs,
      pneumonias = pneumonias,
      surgeries = surgeries,
      ssis = ssis,
      ab_treatments = ab_treatments,
      causative_pathogens = causative_pathogens,
      metadata = metadata),
    class = c("neoipcr_ds", "list"))
}

#' Build connection options for a DHIS2 instance
#'
#' Constructs a `neoipcr_dhis2_conopt` object describing how to
#' authenticate against a DHIS2 NeoIPC instance and reach its API. Exactly
#' one of `token`, `username` or `session_id` must be supplied; missing
#' credentials are filled from the `NEOIPC_DHIS2_*` environment variables
#' or via interactive prompts.
#'
#' @param token A DHIS2 personal access token (string of the form
#'   `d2pat_` followed by 42 characters, or a path to a file containing one).
#' @param username DHIS2 username (paired with a password supplied via env
#'   variable or prompt).
#' @param session_id An existing DHIS2 session cookie value.
#' @param scheme,hostname,port,path URL components for the DHIS2 API.
#' @return A `neoipcr_dhis2_conopt` object.
#' @export
dhis2_connection_options <- function(
    token, username, session_id, scheme = "https",
    hostname = "neoipc.charite.de", port = NULL, path = "/api")
{
  ret <- list(
    base_url = httr2::url_build(
      structure(list(scheme = scheme, hostname = hostname, port = port, path = path), class = "httr2_url")))

  ret <- switch(
    rlang::check_exclusive(token, username, session_id, .require = FALSE),
    token = c(ret, list(token = read_token(token))),
    username = c(ret, list(username = username, password = get_password(ret$base_url))),
    session_id = c(ret, list(session_id = session_id)),
    c(ret, get_auth_data(ret$base_url))
  )

  structure(ret, class = "neoipcr_dhis2_conopt")
}

get_auth_data <- function(url)
{
  env_session_id <- Sys.getenv("NEOIPC_DHIS2_SESSION_ID", unset = NA)
  if(!is.na(env_session_id)) return(list(session_id = env_session_id))

  env_token <- Sys.getenv("NEOIPC_DHIS2_TOKEN", unset = NA)
  if(!is.na(env_token)) return(list(token = read_token(env_token)))

  user <- Sys.getenv("NEOIPC_DHIS2_USER", unset = NA)
  if(is.na(user)) user <- askpass::askpass(
    prompt = sprintf(
      "Please enter your username for %s: ", url))

  if(is.null(user)) rlang::abort(
    message = "No username provided",
    body = "Please provide username and password, a personal access token or a session id to authenticate to DHIS2")

  list(username = user, password = get_password(url))
}

get_password <- function(url)
{
  pw <- Sys.getenv("NEOIPC_DHIS2_PASSWORD", unset = NA)
  if(!is.na(pw)) return(pw)

  pw <- askpass::askpass(
    prompt = sprintf("Please enter your password for %s: ", url))

  if(is.null(pw)) rlang::abort(
    message = "No password provided",
    body = "Please provide username and password, a personal access token or a session id to authenticate to DHIS2")

  pw
}

read_token <- function(token)
{
  if(stringr::str_starts(token, "d2pat_") &&  nchar(token) == 48)
    return(token)

  fileInfo <- file.info(token, extra_cols = FALSE)
  if(!rlang::is_na(fileInfo$isdir) && !fileInfo$isdir)
  {
    fileContent <- readChar(token, fileInfo$size)
    if(stringr::str_starts(fileContent, "d2pat_") && nchar(fileContent) == 48)
      return(fileContent)
  }
  rlang::abort("Invalid DHIS2 personal access token.")
}

#' @export
print.neoipcr_dhis2_conopt <- function(x, ...)
{
  parts <- paste0("Base URL: ", x$base_url)
  if(!is.null(x$token)) {
    parts <- c(parts, "Authentication: Token")
  } else if(!is.null(x$session_id)) {
    parts <- c(parts, "Authentication: Cookie")
  } else if(!is.null(x$username)) {
    parts <- c(
      parts,
      "Authentication: Basic",
      paste0("Username: ", x$username))
  }

  writeLines(parts)
  invisible(x)
}

read_eventData <- function(
    events,
    metadata,
    programStageName,
    prefix = NULL,
    dataElementFilter = NULL,
    keyColumn = NULL,
    keepEventType = TRUE)
{
  e <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name == programStageName) |>
        dplyr::select("programStage","event_type_key"),
      dplyr::join_by("programStage")) |>
    dplyr::select(!"programStage") |>
    dplyr::mutate(
      notes = process_notes(.data$notes)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "createdAt",
      "updatedAt")), readr::parse_datetime)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "occurredAt",
      "scheduledAt",
      "completedAt")), ~ readr::parse_date(stringr::str_sub(.x, end = 10)))) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("followup", "deleted")),
      as.logical)) |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
    dplyr::mutate(
      status = factor(.data$status, levels = c(
        "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))) |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$departments |>
        dplyr::select("organisationUnit", "department_key"),
      dplyr::join_by("orgUnit" == "organisationUnit"))

  if(!is.null(keyColumn))
    e <- e |>
    add_key_column(keyColumn)

  if(!keepEventType)
    e <- e |>
    dplyr::select(!"event_type_key")

  if(!is.null(prefix))
    e <- e |> dplyr::rename_with(
      ~ paste0(prefix, .x, recycle0 = TRUE),
      !c("enrollment","trackedEntity", "dataValues"))

  e <- e |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues", names_sep = "_") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("id", "code"),
      dplyr::join_by("dataValues_dataElement" == "id")) |>
    dplyr::select(!c("dataValues_dataElement", "dataValues_createdAt", "dataValues_updatedAt", "dataValues_createdBy"))

  if(!is.null(dataElementFilter))
    e <- e |>
    dplyr::filter(dataElementFilter(.data$code))

  e |>
    tidyr::pivot_wider(names_from = "code", values_from = "dataValues_value", names_sort = TRUE) |>
    convert_dataElementColumns(metadata$dataElements, metadata$options)
}

recode_enrollments <- function(events, enrollments)
  events |>
  dplyr::inner_join(
    enrollments |>
      dplyr::select("enrollment_key", "enrollment"),
    dplyr::join_by("enrollment")) |>
  dplyr::select(!"enrollment")

recode_events <- function(events, eventList)
{
  map <- dplyr::bind_rows(lapply(eventList, \(x) {
    x |>
      dplyr::select(dplyr::matches("^((sepsis|nec|ssi|pneumonia|surgery)_key)|(event)$")) |>
      dplyr::rename(infection_key = dplyr::matches("^(sepsis|nec|ssi|pneumonia|surgery)_key$"))
  }))
  events |>
    dplyr::left_join(map, dplyr::join_by("event")) |>
    dplyr::relocate("infection_key") |>
    dplyr::select(!"event")
}

get_pathogen_list <- function()
{
  pc <- pathogenConcepts |>
    dplyr::rename("name" = "concept") |>
    dplyr::mutate(synonym_for = rlang::na_int)

  not_listed <- pc |>
    dplyr::slice_head()

  rest <- pc |>
      dplyr::filter(.data$id != 0) |>
      dplyr::bind_rows(
        pathogenSynonyms |>
          dplyr::inner_join(
            pathogenConcepts |>
              dplyr::select(!c("concept","concept_source","concept_id")),
            dplyr::join_by("synonym_for" == "id")) |>
          dplyr::relocate("concept_type", .before = "concept_source") |>
          dplyr::relocate("synonym_for", .after = "show_coli_r") |>
          dplyr::rename("name" = "synonym")) |>
      dplyr::arrange(.data$name)

  dplyr::bind_rows(not_listed, rest)
}

read_causative_pathogens <- function(events, metadata)
{
  e <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name %in% c(
          "Primary Sepsis/BSI",
          "Necrotizing enterocolitis",
          "Surgical Site Infection",
          "Pneumonia")) |>
        dplyr::select("programStage", "name", "event_type_key") |>
        dplyr::rename(programStageName = "name"),
      dplyr::join_by("programStage")) |>
    dplyr::select(!"programStage") |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues", names_sep = "_") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("id", "code"),
      dplyr::join_by("dataValues_dataElement" == "id")) |>
    dplyr::select(!c("dataValues_dataElement", "dataValues_createdAt", "dataValues_updatedAt", "dataValues_createdBy")) |>
    dplyr::filter(stringr::str_detect(.data$code, "PATHOGEN_\\d")) |>
    dplyr::mutate(
      type = factor(stringr::str_replace(.data$code, "^.+(PATHOGEN)_\\d+(.*)$", "\\1\\2")),
      index = as.integer(stringr::str_replace(.data$code, "^.+PATHOGEN_(\\d+).*$", "\\1")),
      secondary_bsi = stringr::str_detect(.data$code, "_SEC_BSI_"),
      .keep = "unused"
    ) |>
    dplyr::select("event", "event_type_key", "type", "index", "secondary_bsi", "dataValues_value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "dataValues_value", names_sort = TRUE) |>
    dplyr::mutate(dplyr::across(c("PATHOGEN_3GCR","PATHOGEN_CAR","PATHOGEN_COR","PATHOGEN_MRSA","PATHOGEN_VRE"), ~ as.logical(dplyr::na_if(as.integer(.x), -1)))) |>
    dplyr::mutate(PATHOGEN = as.integer(.data$PATHOGEN))
}

infer_sepsis_types <- function(sepses, causative_pathogens)
{
  sepses |>
    dplyr::left_join(
      causative_pathogens |>
        dplyr::inner_join(
          get_pathogen_list() |>
            dplyr::select("id", "is_cc"),
          dplyr::join_by("PATHOGEN" == "id")) |>
        dplyr::select("infection_key","event_type_key","is_cc"),
      dplyr::join_by("sepsis_key" == "infection_key", "event_type_key")) |>
    # if a sepsis contains both, a cc and a non-cc pathogen it is a non-cc sepsis
    dplyr::group_by(dplyr::across(!"is_cc")) |>
    dplyr::summarise("is_cc" = as.logical(min(.data$is_cc)), .groups = "drop") |>
    dplyr::mutate(
      bsiType = factor(
        dplyr::case_when(
          is.na(.data$is_cc) ~ "Clin",
          .data$is_cc ~ "CoNS",
          !.data$is_cc ~ "BSI"),
        levels = c("BSI","CoNS","Clin")),
      .before = "NEOIPC_BSI_AB_TREATMENT") |>
    dplyr::select(!"is_cc")
}

convert_dataElementColumns <- function(t, dataElements, options)
{
  t |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(dataElements |> dplyr::pull("code")),
        ~ convert_dataElementColumn(.x, dplyr::cur_column(), dataElements, options)
        )
      )
}

convert_dataElementColumn <- function(col, col_name, dataElements, options)
{
  col_type <- dataElements |>
    dplyr::filter(.data$code == col_name) |>
    dplyr::select("valueType", "optionSet") |>
    unlist()

  if(!rlang::is_na(col_type[["optionSet"]]))
  {
    o <- options |>
      dplyr::filter(.data$optionSet_code == col_type[["optionSet"]])

    if(nrow(o) > 0)
      return(factor(col, levels = (o |> dplyr::pull("code"))))
  }

  if(stringr::str_starts(col_type[["valueType"]], "INTEGER"))
    return(as.integer(col))

  if(col_type[["valueType"]] %in% c("BOOLEAN", "TRUE_ONLY"))
    return(as.logical(col))

  col
}

read_enrollments <- function(enrollments, events, metadata, patients)
{
  admissions <- read_eventData(
    events,
    metadata,
    "Admission",
    "admission_",
    keepEventType = FALSE)

  surveillanceEnds <- read_eventData(
    events,
    metadata,
    "Surveillance-End",
    "surveillanceEnd_",
    \(x) stringr::str_starts(x, "NEOIPC_SURVEILLANCE_END_AB_SUBST", TRUE),
    keepEventType = FALSE)

  enrollments |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "createdAt",
      "createdAtClient",
      "updatedAt",
      "updatedAtClient",
      "completedAt")), readr::parse_datetime)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "enrolledAt",
      "occurredAt")), ~ readr::parse_date(stringr::str_sub(.x, end = 10)))) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(c("followUp", "deleted")),
      as.logical)) |>
    hoist_createdByAndupdatedBy() |>
    dplyr::mutate(
      status = factor(.data$status, levels = c(
        "ACTIVE", "COMPLETED", "CANCELLED"))) |>
    dplyr::mutate(
      notes = process_notes(.data$notes)) |>
    dplyr::rename_with(~ paste0("enrollment_", .x, recycle0 = TRUE), !c("enrollment","trackedEntity")) |>
    dplyr::left_join(admissions, dplyr::join_by("enrollment", "trackedEntity")) |>
    dplyr::left_join(surveillanceEnds, dplyr::join_by("enrollment", "trackedEntity")) |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("enrollment_completedBy" == "username")) |>
    dplyr::mutate(enrollment_completedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("enrollment_storedBy" == "username")) |>
    dplyr::mutate(enrollment_storedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("enrollment_createdBy" == "username")) |>
    dplyr::mutate(enrollment_createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("enrollment_updatedBy" == "username")) |>
    dplyr::mutate(enrollment_updatedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$departments |>
        dplyr::select("organisationUnit", "department_key"),
      dplyr::join_by("enrollment_orgUnit" == "organisationUnit")) |>
    dplyr::mutate(enrollment_department_key = .data$department_key, .keep = "unused") |>
    dplyr::inner_join(
      patients |>
        dplyr::select("trackedEntity", "patient_key"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::select(!"trackedEntity") |>
    add_key_column("enrollment_key")
}

read_ab_treatments <- function(events, metadata, enrollments) {
  events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name == "Surveillance-End") |>
        dplyr::select("programStage","event_type_key"),
      dplyr::join_by("programStage")) |>
    dplyr::select("enrollment","dataValues") |>
    recode_enrollments(enrollments) |>
    dplyr::relocate("enrollment_key") |>
    tidyr::unnest_longer(2) |>
    tidyr::unnest_wider(2) |>
    dplyr::select("enrollment_key","dataElement","value") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::filter(stringr::str_starts( .data$code, "NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d\\d")) |>
        dplyr::select("id", "code"),
      dplyr::join_by("dataElement" == "id")) |>
    dplyr::select(!"dataElement") |>
    dplyr::mutate(
      index = as.integer(
        stringr::str_extract(.data$code,"^NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d(\\d)(_DAYS)?$", 1)),
      name = dplyr::if_else(stringr::str_ends(.data$code, "_DAYS"), "days", "substance_code"),
      .keep = "unused") |>
    tidyr::pivot_wider() |>
    dplyr::mutate(days = as.integer(.data$days), .after = "substance_code") |>
    dplyr::arrange("enrollment", "index")
}


process_notes <- function(notes)
{
  sapply(
    notes,
    \(x){
      if(length(x) == 0 || rlang::is_na(x))
        NA
      else
        paste0(
          purrr::map_chr(
            x,
            \(y) {
              sprintf(
                '%s %s (%s): "%s"',
                y[["createdBy"]][["firstName"]],
                y[["createdBy"]][["surname"]],
                format(readr::parse_datetime(y[["storedAt"]]), "%x %X"),
                y[["value"]])
              }), collapse = "; ")})
}

hoist_createdByAndupdatedBy <- function(table)
{
  table |>
    tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
    tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE)
}

read_patients <- function(trackedEntities, metadata)
{
  patients <- trackedEntities |>
    tidyr::unnest_longer("attributes") |>
    tidyr::unnest_wider("attributes", names_sep = "_") |>
    tidyr::pivot_wider(
      names_from = "attributes_code",
      values_from = "attributes_value") |>
    hoist_createdByAndupdatedBy() |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "createdAt",
      "createdAtClient",
      "updatedAt",
      "updatedAtClient")), readr::parse_datetime)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "inactive",
      "potentialDuplicate",
      "NEOIPC_TEA_MULTIPLE_BIRTH")), as.logical)) |>
    dplyr::mutate(
      NEOIPC_TEA_DELIVERY_MODE = factor(
        .data$NEOIPC_TEA_DELIVERY_MODE)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "NeoIPC_TEA_TOTAL_GESTATION_DAYS",
      "NEOIPC_TEA_SIBLINGS",
      "NEOIPC_TEA_BIRTH_WEIGHT")), as.integer))|>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "user_key"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
    dplyr::left_join(
      metadata$departments |>
        dplyr::select("organisationUnit", "department_key"),
      dplyr::join_by("orgUnit" == "organisationUnit")) |>
    dplyr::select(!"orgUnit") |>
    add_key_column("patient_key")

  if("NEOIPC_TEA_SIBLINGS" %in% names(patients))
    patients <- patients |>
    dplyr::mutate(
      NEOIPC_TEA_SIBLINGS = tidyr::replace_na(.data$NEOIPC_TEA_SIBLINGS, 1))

  if("NEOIPC_TEA_MULTIPLE_BIRTH" %in% names(patients))
    patients <- patients |>
    dplyr::mutate(
      NEOIPC_TEA_MULTIPLE_BIRTH = tidyr::replace_na(
        .data$NEOIPC_TEA_MULTIPLE_BIRTH, FALSE))

  patients
}

get_testUnitIds <- function(metadata)
{
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroups")

  if(is.null(organisationUnitGroups))
    NULL
  else
    organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == "TEST_UNITS") |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::select("id") |>
    unlist(use.names = FALSE)
}

add_key_column <- function(table, key_name = "key", as_factor = FALSE)
{
  if(rlang::is_null(table)) return(NULL)

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

get_users_orgUnits <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(is.null(users))
    NULL
  else
    users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(
      c(
        "id",
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits")) |>
    tidyr::pivot_longer(
      cols = c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits"),
      names_to = "type",
      values_to = "organisationUnit_id")
}

get_users_roles <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(is.null(users))
    NULL
  else
    users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(c("id","userRoles")) |>
    tidyr::unnest_longer(2) |>
    tidyr::unnest_wider(2, names_sep = "_")
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

dhis2_request <- function(connection_options = dhis2_connection_options())
{
  req <- httr2::request(connection_options$base_url)
  if(exists('token', where = connection_options))
    req |>
      httr2::req_headers(Authorization = sprintf("ApiToken %s", connection_options$token), .redact = "Authorization")
  else if(exists('session_id', where = connection_options))
    req |>
      httr2::req_cookies_set(JSESSIONID = connection_options$session_id)
  else
    req |>
      httr2::req_auth_basic(username = connection_options$username, password = connection_options$password)
}
