#' @importFrom rlang .data
import_dhis2 <- function(connection_options = dhis2_connection_options(), include_deleted = FALSE)
{
  d2req_base <- dhis2_request(connection_options)

  raw_metadata <- d2req_base |>
    httr2::req_url_path_append("metadata") |>
    httr2::req_url_query(
      paging = "false",
      translate = "false",
      `programs:fields` = "id,programTrackedEntityAttributes[trackedEntityAttribute[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[id]]],programStages[id,name,displayName,displayFormName,displayDescription,programStageDataElements[dataElement[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[id]]]]",
      `programs:filter` = "code:eq:NEOIPC_CORE",
      `organisationUnitGroups:fields` = "code,organisationUnits[id,code,displayName,displayShortName,displayDescription]",
      `organisationUnitGroups:filter` = "code:in:[COUNTRY,TEST_UNITS]",
      `organisationUnits:fields` = "id,displayName,displayShortName,displayDescription,openingDate,comment,geometry,parent[id,code,displayName,displayShortName,displayDescription,comment,geometry,parent[code]]",
      `organisationUnits:filter` = "organisationUnitGroups.code:eq:NEO_DEPARTMENT",
      `organisationUnitGroupSets:fields` = "organisationUnitGroups[displayName,displayShortName,displayDescription,organisationUnits[id]]",
      `organisationUnitGroupSets:filter` = "code:eq:NEOIPC_TRIALS",
      `optionGroupSets:fields` = "optionGroups[code,displayName,displayShortName,displayDescription,options[code,displayName,displayFormName,displayDescription]]",
      `optionGroupSets:filter` = "code:eq:ANTIMICROBIALS",
      `users:fields` = "id,username,firstName,surname,email,created,lastLogin,organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]",
      `users:filter` = "disabled:eq:false") |>
    httr2::req_perform() |>
    httr2::resp_body_string("UTF-8")

  metadata <- raw_metadata |>
    read_metadata()

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
          program = metadata$programId,
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

  enrollments <- data[[2]]
  events <- data[[3]]

  c(data, list(
    patients = read_patients(data[[1]], metadata),
    enrollments = read_enrollments(enrollments, events, metadata),
    surgeries = read_surgeries(events),
    infections = read_infections(events),
    metadata = metadata,
    raw_metadata = raw_metadata))
}

read_eventData <- function(
    events,
    metadata,
    programStageName,
    prefix = NULL,
    dataElementFilter = NULL)
{
  eventId <- metadata$programStages |>
    dplyr::filter(.data$name == programStageName) |>
    dplyr::pull("id")

  e <- events |>
    dplyr::filter(.data$programStage == eventId) |>
    dplyr::select(!c("programStage")) |>
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
        "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED")))


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
    tidyr::pivot_wider(names_from = .data$code, values_from = .data$dataValues_value) |>
    convert_dataElementColumns(metadata$dataElements)
}

convert_dataElementColumns <- function(t, dataElements)
{
  t |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(dataElements |> dplyr::pull("code")),
        ~ convert_dataElementColumn(.x, dplyr::cur_column(), dataElements)
        )
      )
}

convert_dataElementColumn <- function(col, col_name, dataElements)
{
  col_type <- dataElements |>
    get_valueType(col_name)

  if(stringr::str_starts(col_type, "INTEGER"))
    as.integer(col)
  else if(col_type %in% c("BOOLEAN", "TRUE_ONLY"))
    as.logical(col)
  else
    col
}

get_valueType <- function(dataElements, dataElementCode)
{
  dataElements |>
    dplyr::filter(.data$code == dataElementCode) |>
    dplyr::pull("valueType")
}

read_infections <- function(events)
{
}

read_enrollments <- function(enrollments, events, metadata)
{
  admissions <- read_eventData(events, metadata, "Admission", "admission_")

  surveillanceEnds <- read_eventData(
    events,
    metadata,
    "Surveillance-End",
    "surveillanceEnd_",
    \(x) stringr::str_starts(x, "NEOIPC_SURVEILLANCE_END_AB_SUBST", TRUE))

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
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("admission_storedBy" == "username")) |>
    dplyr::mutate(admission_storedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("admission_createdBy" == "username")) |>
    dplyr::mutate(admission_createdBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("admission_updatedBy" == "username")) |>
    dplyr::mutate(admission_updatedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("surveillanceEnd_storedBy" == "username")) |>
    dplyr::mutate(surveillanceEnd_storedBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("surveillanceEnd_createdBy" == "username")) |>
    dplyr::mutate(surveillanceEnd_createdBy = .data$key, .keep = "unused") |>
    dplyr::left_join(
      metadata$users |>
        dplyr::select("username", "key"),
      dplyr::join_by("surveillanceEnd_updatedBy" == "username")) |>
    dplyr::mutate(surveillanceEnd_updatedBy = .data$key, .keep = "unused")
}

process_notes <- function(notes)
{
  sapply(
    notes,
    \(x){
      if(length(x) == 0)
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

read_surgeries <- function(events)
{
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
      names_from = .data$attributes_code,
      values_from = .data$attributes_value) |>
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
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "NEOIPC_TEA_DELIVERY_MODE",
      "NeoIPC_TEA_TOTAL_GESTATION_DAYS",
      "NEOIPC_TEA_SIBLINGS",
      "NEOIPC_TEA_BIRTH_WEIGHT")), as.integer))|>
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

read_metadata <- function(metadata_text)
{
  metadata <- jsonlite::fromJSON(metadata_text, simplifyVector = FALSE)
  ret <- list(
    system = get_system(metadata))

  programId <- get_program_id(metadata)
  if(!is.null(programId))
    ret <- c(ret, list(programId = programId) )

  programStages <- get_programStages(metadata)
  if(!is.null(programStages))
    ret <- c(ret, list(programStages = programStages) )

  dataElements <- get_dataElements(metadata)
  if(!is.null(dataElements))
    ret <- c(ret, list(dataElements = dataElements) )

  trackedEntityAttributes <- get_trackedEntityAttributes(metadata)
  if(!is.null(trackedEntityAttributes))
    ret <- c(ret, list(trackedEntityAttributes = trackedEntityAttributes) )

  countries <- get_countries(metadata)
  if(!is.null(countries))
    ret <- c(ret, list(countries = countries) )

  hospitals <- get_hospitals(metadata)
  if(!is.null(hospitals))
    ret <- c(ret, list(hospitals = hospitals) )

  departments <- get_departments(metadata, hospitals)
  if(!is.null(departments))
    ret <- c(ret, list(departments = departments) )

  users <- get_users(metadata)
  if(!is.null(users))
    ret <- c(ret, list(users = users) )

  ret
}

get_system <- function(metadata)
{
  system <- purrr::pluck(metadata, "system")
  if(is.null(system))
    rlang::abort("Invalid DHIS2 metadata.", "neoipcr_metadata_system_missing")

  list(id = uuid::as.UUID(system$id),
       version = as.numeric_version(system$version),
       rev = system$rev,
       date = readr::parse_datetime(system$date))
}

get_program_id <- function(metadata)
{
  metadata |>
    purrr::pluck("programs", 1, "id")
}

get_programStages <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(is.null(programStages))
    NULL
  else
    programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(!"programStageDataElements")
}

get_dataElements <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(is.null(programStages))
    NULL
  else
    programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select("programStageDataElements") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider("optionSet", names_sep = "_")
}

get_trackedEntityAttributes <- function(metadata)
{
  programTrackedEntityAttributes <- metadata |>
    purrr::pluck("programs", 1, "programTrackedEntityAttributes")

  if(is.null(programTrackedEntityAttributes))
    NULL
  else
    programTrackedEntityAttributes |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider("optionSet", names_sep = "_")
}

get_countries <- function(metadata)
{
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroups")

  if(is.null(organisationUnitGroups))
    NULL
  else
    organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == "COUNTRY") |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1)
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

get_hospitals <- function(metadata)
{
  organisationUnits <- metadata |>
    purrr::pluck("organisationUnits")

  if(is.null(organisationUnits))
    NULL
  else
  {
    hospitals <- organisationUnits |>
      tibble::tibble() |>
      tidyr::unnest_longer(1) |>
      dplyr::filter(.data$organisationUnits_id == "parent")

    if(nrow(hospitals) < 1)
      NULL
    else
      hospitals |>
      dplyr::select(1) |>
      tidyr::unnest_wider(1) |>
      tidyr::unnest_wider(c("parent", "geometry"), names_sep = "_") |>
      tidyr::unnest_wider("geometry_coordinates", names_sep = "_") |>
      dplyr::rename(
        country_code = "parent_code",
        longitude = "geometry_coordinates_1",
        latitude = "geometry_coordinates_2") |>
      dplyr::select(!"geometry_type") |>
      dplyr::filter(.data$country_code != "NEOIPC") |>
      dplyr::distinct() |>
      add_key_column()
  }

}

get_departments <- function(metadata, hospitals)
{
  organisationUnits <- metadata |>
    purrr::pluck("organisationUnits")

  if(is.null(organisationUnits))
    NULL
  else
  {
    t <- organisationUnits |>
      tibble::tibble() |>
      tidyr::unnest_wider(1) |>
      dplyr::mutate(
        openingDate =  readr::parse_date(
          stringr::str_sub(.data$openingDate, end = 10)))


    if("geometry" %in% names(t))
      t <- t |>
      tidyr::unnest_wider(c("parent", "geometry"), names_sep = "_") |>
      tidyr::unnest_wider("geometry_coordinates", names_sep = "_") |>
      dplyr::select(
        !c(
          tidyselect::starts_with("parent_") & !tidyselect::ends_with("_id"),
          "geometry_type")) |>
      dplyr::rename(
        longitude = "geometry_coordinates_1",
        latitude = "geometry_coordinates_2")
    else
      t <- t |>
      tidyr::unnest_wider("parent", names_sep = "_") |>
      dplyr::select(
        !c(
          tidyselect::starts_with("parent_") & !tidyselect::ends_with("_id")))

    t |>
      dplyr::left_join(hospitals |> dplyr::select("id", "key"),
                       dplyr::join_by("parent_id" == "id")) |>
      dplyr::mutate(hospital = .data$key, .keep = "unused") |>
      dplyr::select(!"parent_id") |>
      add_key_column()
  }
}

get_users <- function(metadata)
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
      !c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits",
        "userRoles")) |>
    dplyr::mutate(
      created = readr::parse_datetime(.data$created),
      lastLogin = readr::parse_datetime(.data$lastLogin)) |>
    add_key_column()
}

add_key_column <- function(table)
{
  table |>
    dplyr::mutate(random = ids::random_id(nrow(table))) |>
    dplyr::arrange(.data$random) |>
    dplyr::mutate(key = dplyr::row_number()) |>
    dplyr::select(!"random")
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

dhis2_connection_options <- function(
    token, username, password = NULL, scheme = "https", hostname = "localhost",
    port = NULL, path = "/api")
{
  ret <- list(
    base_url = httr2::url_build(
      list(scheme = scheme, hostname = hostname, port = port, path = path)))

  switch(
    rlang::check_exclusive(token, username, .require = FALSE),
    token = c(ret, list(token = token)),
    username = c(ret, list(username = username, password = password)),
    c(ret, list(
      username = askpass::askpass(
        prompt = sprintf(
          "Please enter your username for %s: ", ret$base_url)),
      password = password))
  )
}

dhis2_request <- function(connection_options = dhis2_connection_options())
{
  req <- httr2::request(connection_options$base_url)
  if(exists('token', where = connection_options))
  {
    req |>
      httr2::req_headers(Authorization = sprintf("ApiToken %s", connection_options$token), .redact = "Authorization")
  }
  else
  {
    req |>
      httr2::req_auth_basic(username = connection_options$username, password = connection_options$password)
  }
}
