#' @importFrom rlang .data
import_dhis2 <- function(connection_options = dhis2_connection_options(), include_deleted = FALSE)
{
  d2req_base <- dhis2_request(connection_options)

  metadata <- d2req_base |>
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
    httr2::resp_body_string("UTF-8") |>
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
          fields = "trackedEntity,createdAt,createdAtClient,updatedAt,updatedAtClient,orgUnit,inactive,potentialDuplicate,attributes[code,value]")))

  reqs <- append(
    reqs,
    list(
      tracker_req |>
        httr2::req_url_path_append("enrollments") |>
        httr2::req_url_query(
          fields = "enrollment,createdAt,createdAtClient,updatedAt,updatedAtClient,trackedEntity,status,orgUnit,enrolledAt,occurredAt,followUp,completedAt,notes")))

  reqs <- append(
    reqs,
    list(
      tracker_req |>
        httr2::req_url_path_append("events") |>
        httr2::req_url_query(fields = "event,status,programStage,enrollment,trackedEntity,orgUnit,occurredAt,scheduledAt,followup,createdAt,updatedAt,completedAt,notes,dataValues[dataElement,value]")))

  data <-  reqs |>
    httr2::req_perform_parallel() |>
    httr2::resps_data(function(resp) list(httr2::resp_body_json(resp) |> tibble::tibble() |> tidyr::unnest_longer(1) |> tidyr::unnest_wider(1)))

  c(data, list(metadata = metadata))
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

  departments <- get_departments(metadata)
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
      dplyr::distinct()
  }

}

get_departments <- function(metadata)
{
  organisationUnits <- metadata |>
    purrr::pluck("organisationUnits")

  if(is.null(organisationUnits))
    NULL
  else
  {
    t <- organisationUnits |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

    if("geometry" %in% names(t))
      t |>
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
      t |>
      tidyr::unnest_wider("parent", names_sep = "_") |>
      dplyr::select(
        !c(
          tidyselect::starts_with("parent_") & !tidyselect::ends_with("_id")))
  }
}

get_users <- function(metadata)
{

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
