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
    metadata = metadata))
}


#' @export
dhis2_connection_options <- function(
    token, username, password = NULL, scheme = "https",
    hostname = "neoipc.charite.de", port = NULL, path = "/api")
{
  ret <- list(
    base_url = httr2::url_build(
      list(scheme = scheme, hostname = hostname, port = port, path = path)))

  switch(
    rlang::check_exclusive(token, username, .require = FALSE),
    token = c(ret, list(token = read_token(token))),
    username = c(ret, list(username = username, password = password)),
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
    tidyr::pivot_wider(names_from = "code", values_from = "dataValues_value") |>
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
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(
      "NEOIPC_TEA_DELIVERY_MODE",
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
