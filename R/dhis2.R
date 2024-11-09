import_dhis2 <- function(connection_options = dhis2_connection_options(), include_deleted = FALSE)
{
  d2req_base <- dhis2_request(connection_options)
  metadata_text <- d2req_base |>
    httr2::req_url_path_append("metadata") |>
    httr2::req_url_query(
      paging = "false",
      translate = "false",
      `programs:fields` = "id",
      `programs:filter` = "code:eq:NEOIPC_CORE") |>
    httr2::req_perform() |>
    httr2::resp_body_string("UTF-8")

  metadata <- read_metadata(metadata_text)

  reqs <- list()
  tracker_req <- d2req_base |>
    httr2::req_url_path_append("tracker") |>
    httr2::req_url_query(
      ouMode ="ACCESSIBLE",
      skipPaging = "true",
      includeDeleted = tolower(include_deleted))

  reqs <- append(reqs, base::list(tracker_req |>
                                    httr2::req_url_path_append("trackedEntities") |>
                                    httr2::req_url_query(program = metadata$programId, fields = "trackedEntity,createdAt,createdAtClient,updatedAt,updatedAtClient,orgUnit,inactive,potentialDuplicate,attributes[code,value]")))
  reqs <- append(reqs, base::list(tracker_req |>
                                    httr2::req_url_path_append("enrollments") |>
                                    httr2::req_url_query(fields = "enrollment,createdAt,createdAtClient,updatedAt,updatedAtClient,trackedEntity,status,orgUnit,enrolledAt,occurredAt,followUp,completedAt,notes")))
  reqs <- append(reqs, base::list(tracker_req |>
                                    httr2::req_url_path_append("events") |>
                                    httr2::req_url_query(fields = "event,status,programStage,enrollment,trackedEntity,orgUnit,occurredAt,scheduledAt,followup,createdAt,updatedAt,completedAt,notes,dataValues[dataElement,value]")))

  data <-  reqs |>
    httr2::req_perform_parallel() |>
    httr2::resps_data(function(resp) list(httr2::resp_body_json(resp) |> tibble::tibble() |> tidyr::unnest_longer(1) |> tidyr::unnest_wider(1)))

  data
}

read_metadata <- function(text)
{
  metadata <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  ret <- list(
    system = get_system(metadata),
    programId = get_program_id(metadata))

  ret
}

get_system <- function(metadata)
{
  system <- purrr::pluck(metadata, "system")
  list(id = uuid::as.UUID(system$id),
       version = as.numeric_version(system$version),
       rev = system$rev,
       date = readr::parse_datetime(system$date))
}

get_program_id <- function(metadata)
{
  metadata |>
    purrr::pluck("programs", 1, "id") |>
    unlist(recursive = FALSE)
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
