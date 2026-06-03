# Creates the organisationUnits query, which we use, so that we can apply the
# withinUserHierarchy filter
get_organisationUnit_request <- function(req_base, user_info, dataset_options)
{
  fields <- "id"

  if(dataset_options$include_department == "full")
    fields <- paste0(fields, ",code,displayName,displayShortName,displayDescription,openingDate,comment,geometry")
  # We need the department code for filtering or to transform the supplied exceptions
  else if(length(dataset_options$include_invalid_patients) > 1 || length(dataset_options$department_filter) > 0)
    fields <- paste0(fields, ",code")

  if(length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    country_fields <- ",parent[id]]"
  else
    country_fields <- "]"

  if(dataset_options$include_hospital == "full")
    fields <- paste0(fields, paste0(",parent[id,code,displayName,displayShortName,displayDescription,comment,geometry", country_fields))
  else if (dataset_options$include_hospital == "pseudo" ||
           length(dataset_options$country_filter) > 0 ||
           dataset_options$include_country != "no" ||
           dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields, paste0(",parent[id", country_fields))

  req_base |>
    httr2::req_url_path_append("organisationUnits") |>
    httr2::req_url_query(
      withinUserHierarchy = "true",
      fields = fields,
      filter = "organisationUnitGroups.code:eq:NEO_DEPARTMENT")
}

read_organisationUnits <- function(organisationUnits, dataset_options)
{
  department_base <- tibble::tibble(units = organisationUnits$organisationUnits) |>
    tidyr::unnest_wider(1)

  ret <- list()

  # The parent of the department is the hospital
  if("parent" %in% names(department_base)) {
    hospital_base <- tibble::tibble(hospital = department_base$parent) |>
      tidyr::unnest_wider(1)

    hospitals_result <- read_organisationUnits_hospitals(
      hospital_base, dataset_options)
    ret$hospitals               <- hospitals_result$processed
    ret$.hospitals_internal_map <- hospitals_result$internal_map
  }

  departments_result <- read_organisationUnits_departments(
    department_base,
    ret,
    dataset_options)
  ret$departments               <- departments_result$processed
  ret$.departments_internal_map <- departments_result$internal_map

  ret
}

# Read hospital rows from the parent-of-department block of the
# /organisationUnits response.
#
# Returns a named list with two components:
#   * `processed`    — transformed tibble carrying every column the
#                      orchestrator needs to finish building the public
#                      hospitals tibble: `hospital_key`, `orgUnit`, any
#                      display / geometry fields under "full", and the
#                      raw `country` DHIS2 id (used by the orchestrator's
#                      country_key join). `metadata$hospitals` starts as
#                      this tibble and is narrowed to
#                      `compile_schema(hospitals_cols, opts)` in
#                      `read_metadata_reponses()` once the country_key
#                      join has added its column.
#   * `internal_map` — lookup subset with `hospital_key`, `orgUnit`, and
#                      `country` (when available). Used by
#                      `read_organisationUnits_departments()` for the
#                      dept→hospital join, and by
#                      `read_metadata_reponses()` for the country_key
#                      lookup and the WB-class inheritance path under
#                      `include_country = "no"`. Threaded through
#                      `metadata$.hospitals_internal_map` and stripped at
#                      `import_dhis2()` exit.
read_organisationUnits_hospitals <- function(x, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    processed    = tibble::tibble(),
    internal_map = NULL
  )

  if (is.null(x) || nrow(x) < 1L)
    return(empty_result)

  # Hoist geometry when present; otherwise pad with NA under "full" so
  # the schema's longitude/latitude columns are populated either way.
  if ("geometry" %in% names(x)) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (opts$include_hospital == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  # Hoist the parent reference — for hospitals, the parent is the
  # country. Present in the raw response only when country / WB-class
  # info is requested (see `get_organisationUnit_request`).
  if ("parent" %in% names(x))
    x <- x |> tidyr::hoist("parent", country = "id")

  processed <- x |>
    dplyr::distinct() |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("hospital_key")

  internal_map <- processed |>
    dplyr::select(tidyselect::any_of(c("hospital_key", "orgUnit", "country")))

  list(processed = processed, internal_map = internal_map)
}

read_organisationUnits_departments <- function(x, y, dataset_options) {

  # Dept → hospital join uses the orchestrator-internal hospitals map
  # (not `y$hospitals` directly), because `metadata$hospitals` is later
  # narrowed to the public schema which may strip `orgUnit` when
  # `"hospitals" %not in% include_dhis2_ids`. The map always carries
  # `hospital_key` + `orgUnit` for this join.
  if(!is.null(y$.hospitals_internal_map) &&
     "orgUnit" %in% names(y$.hospitals_internal_map)){
    x <- x |>
      tidyr::hoist("parent", orgUnit = "id") |>
      dplyr::left_join(
        y$.hospitals_internal_map |>
          dplyr::select("orgUnit", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!c("orgUnit","parent"))
  }

  cols <- names(x)
  if("openingDate" %in% cols)
    x <- x |>
      dplyr::mutate(
        openingDate =  readr::parse_date(
          stringr::str_sub(.data$openingDate, end = 10)))

  # Hoist geometry when present; otherwise pad NA under "full" so the
  # schema's longitude/latitude columns are populated either way.
  if("geometry" %in% cols) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (dataset_options$include_department == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  processed <- x |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("department_key")

  internal_map <- processed |>
    dplyr::select("department_key", "orgUnit")

  list(processed = processed, internal_map = internal_map)
}
