#' @importFrom rlang .data
#' @export
import_dhis2 <- function(connection_options = dhis2_connection_options(), translate = TRUE, locale = NULL, include_deleted = FALSE)
{
  d2req_base <- dhis2_request(connection_options)

  metadata <- get_metadata(d2req_base, translate, locale)

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
  surgeries <- read_eventData(events, metadata, "Surgical Procedure") |>
    recode_enrollments(enrollments)
  sepses <- read_eventData(
    events, metadata, "Primary Sepsis/BSI",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  necs <- read_eventData(
    events, metadata, "Necrotizing enterocolitis",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_NEC_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  ssis <- read_eventData(
    events, metadata, "Surgical Site Infection",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_SSI_PATHOGEN", TRUE) &
      stringr::str_starts(x, "NEOIPC_SSI_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  pneumonias <- read_eventData(
    events, metadata, "Pneumonia",
    dataElementFilter = \(x) stringr::str_starts(x, "NEOIPC_HAP_PATHOGEN", TRUE) &
      stringr::str_starts(x, "NEOIPC_HAP_SEC_BSI_PATHOGEN", TRUE)) |>
    recode_enrollments(enrollments)
  causative_pathogens <- read_causative_pathogens(events, metadata) |>
    recode_events(list(sepses, necs, ssis, pneumonias))

  sepses <- sepses |>
    infer_sepsis_types(causative_pathogens)

  class(patients) <- c("neoipc_pat", class(patients))
  class(enrollments) <- c("neoipc_enr", class(enrollments))
  class(ab_treatments) <- c("neoipc_trt", class(ab_treatments))
  class(surgeries) <- c("neoipc_srg", class(surgeries))
  class(sepses) <- c("neoipc_sep", class(sepses))
  class(necs) <- c("neoipc_nec", class(necs))
  class(ssis) <- c("neoipc_ssi", class(ssis))
  class(pneumonias) <- c("neoipc_pne", class(pneumonias))
  class(causative_pathogens) <- c("neoipc_cspath", class(causative_pathogens))

  structure(
    list(
      events = events,
      patients = patients,
      enrollments = enrollments,
      ab_treatments = ab_treatments,
      surgeries = surgeries,
      sepses = sepses,
      necs = necs,
      ssis = ssis,
      pneumonias = pneumonias,
      causative_pathogens = causative_pathogens,
      metadata = metadata),
    class = c("neoipc_ds", "list"))
}


#' @export
dhis2_connection_options <- function(
    token, username, session_id, password = NULL, scheme = "https",
    hostname = "neoipc.charite.de", port = NULL, path = "/api")
{
  ret <- list(
    base_url = httr2::url_build(
      structure(list(scheme = scheme, hostname = hostname, port = port, path = path), class = "httr2_url")))

  switch(
    rlang::check_exclusive(token, username, session_id, .require = FALSE),
    token = c(ret, list(token = read_token(token))),
    username = c(ret, list(username = username, password = password)),
    session_id = c(ret, list(session_id = session_id)),
    c(ret, list(
      username = askpass::askpass(
        prompt = sprintf(
          "Please enter your username for %s: ", ret$base_url)),
      password = password))
  )
}

read_eventData <- function(
    events,
    metadata,
    programStageName,
    prefix = NULL,
    dataElementFilter = NULL,
    addKeyColumn = TRUE,
    keepEventType = TRUE)
{
  e <- events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name == programStageName) |>
        dplyr::select("id","key"),
      dplyr::join_by("programStage" == "id")) |>
    dplyr::mutate(eventType = .data$key, .keep = "unused") |>
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
        dplyr::select("username", "key"),
      dplyr::join_by("storedBy" == "username")) |>
    dplyr::mutate(storedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata[["departments"]] |>
        dplyr::select("id", "key"),
      dplyr::join_by("orgUnit" == "id")) |>
    dplyr::mutate(orgUnit = .data$key, .keep = "unused") |>
    dplyr::rename(department = "orgUnit")

  if(addKeyColumn)
    e <- e |>
    add_key_column()

  if(!keepEventType)
    e <- e |>
    dplyr::select(!"eventType")

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
        dplyr::select("key", "enrollment") |>
        dplyr::rename(enrollment_key = "key"),
      dplyr::join_by("enrollment")) |>
    dplyr::mutate(enrollment = .data$enrollment_key, .keep = "unused")

recode_events <- function(events, eventList)
{
  map <- dplyr::bind_rows(lapply(eventList, \(x) x |> dplyr::select("key", "event")))
  events |>
    dplyr::left_join(map, dplyr::join_by("event")) |>
    dplyr::relocate("key") |>
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
        dplyr::select("id", "name", "key") |>
        dplyr::rename(
          programStageid = "id",
          programStageName = "name",
          programStageKey = "key"),
      dplyr::join_by("programStage" == "programStageid")) |>
    dplyr::mutate(programStage = .data$programStageKey, .keep = "unused") |>
    dplyr::rename(eventType = "programStage") |>
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
    dplyr::select("event", "eventType", "type", "index", "secondary_bsi", "dataValues_value",) |>
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
        dplyr::select("key","eventType","is_cc"),
      dplyr::join_by("key", "eventType")) |>
    # if a sepsis contains both, a cc and a non-cc pathogen it is a non-cc sepsis
    dplyr::group_by(across(!.data$is_cc)) |>
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

  if(!rlang::is_na(col_type[["optionSet"]])){
    o <- options |>
      dplyr::filter(.data$optionSet_code == col_type[["optionSet"]])

  if(nrow(o) > 0)
    return(factor(col, levels = (o |> dplyr::pull("code"))))
  }

  if(stringr::str_starts(col_type[["valueType"]], "INTEGER"))
    as.integer(col)
  else if(col_type[["valueType"]] %in% c("BOOLEAN", "TRUE_ONLY"))
    as.logical(col)
  else
    col

}

read_enrollments <- function(enrollments, events, metadata, patients)
{
  admissions <- read_eventData(
    events,
    metadata,
    "Admission",
    "admission_",
    addKeyColumn = FALSE,
    keepEventType = FALSE)

  surveillanceEnds <- read_eventData(
    events,
    metadata,
    "Surveillance-End",
    "surveillanceEnd_",
    \(x) stringr::str_starts(x, "NEOIPC_SURVEILLANCE_END_AB_SUBST", TRUE),
    addKeyColumn = FALSE,
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
        dplyr::select("username", "key"),
      dplyr::join_by("enrollment_completedBy" == "username")) |>
    dplyr::mutate(enrollment_completedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("enrollment_storedBy" == "username")) |>
    dplyr::mutate(enrollment_storedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("enrollment_createdBy" == "username")) |>
    dplyr::mutate(enrollment_createdBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("enrollment_updatedBy" == "username")) |>
    dplyr::mutate(enrollment_updatedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata[["departments"]] |>
        dplyr::select("id", "key"),
      dplyr::join_by("enrollment_orgUnit" == "id")) |>
    dplyr::mutate(enrollment_orgUnit = .data$key, .keep = "unused") |>
    dplyr::rename(enrollment_department = "enrollment_orgUnit") |>
    dplyr::inner_join(
      patients |>
        dplyr::select("trackedEntity", "key"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(patient = .data$key, .keep = "unused") |>
    dplyr::relocate("patient", .after = "enrollment") |>
    dplyr::select(!"trackedEntity") |>
    add_key_column()

}

read_ab_treatments <- function(events, metadata, enrollments) {
  events |>
    dplyr::inner_join(
      metadata$eventTypes |>
        dplyr::filter(.data$name == "Surveillance-End")
        |> dplyr::select("id","key"),
      dplyr::join_by("programStage" == "id")) |>
    dplyr::select("enrollment","status","dataValues") |>
    recode_enrollments(enrollments) |>
    tidyr::unnest_longer(3) |>
    tidyr::unnest_wider(3) |>
    dplyr::select("enrollment","status","dataElement","value") |>
    dplyr::rename(surveillanceEnd_status = .data$status) |>
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
      metadata[["users"]] |>
        dplyr::select("username", "key"),
      dplyr::join_by("createdBy" == "username")) |>
    dplyr::mutate(createdBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata[["users"]] |>
        dplyr::select("username", "key"),
      dplyr::join_by("updatedBy" == "username")) |>
    dplyr::mutate(updatedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata[["departments"]] |>
        dplyr::select("id", "key"),
      dplyr::join_by("orgUnit" == "id")) |>
    dplyr::mutate(department = .data$key, .keep = "unused") |>
    dplyr::select(!"orgUnit") |>
    add_key_column()

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

add_key_column <- function(table)
{
  table |>
    dplyr::mutate(random = ids::random_id(nrow(table))) |>
    dplyr::arrange(.data$random) |>
    dplyr::mutate(key = dplyr::row_number()) |>
    dplyr::select(!"random") |>
    dplyr::relocate("key")
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

read_token <- function(token)
{
  if(stringr::str_starts(token, "d2pat_"))
    return(token)

  fileInfo <- file.info(token, extra_cols = FALSE)
  if(!rlang::is_na(fileInfo$isdir) && !fileInfo$isdir)
  {
    fileContent <- readChar(token, fileInfo$size)
    if(stringr::str_starts(fileContent, "d2pat_"))
      return(fileContent)
  }
  rlang::abort("Invalid DHIS2 personal access token.")
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
