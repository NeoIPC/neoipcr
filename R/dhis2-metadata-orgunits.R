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
    ret$hospitals <- read_organisationUnits_hospitals(
      hospital_base)
  }

  ret$departments <- read_organisationUnits_departments(
    department_base,
    ret,
    "hospitals" %in% dataset_options$include_dhis2_ids)

  # Apply the include_dhis2_ids redaction to ret$hospitals here, in the
  # caller — read_organisationUnits_departments() can't mutate ret$hospitals
  # in place since its `y` parameter is a local copy.
  if (!is.null(ret$hospitals) &&
      !("hospitals" %in% dataset_options$include_dhis2_ids))
    ret$hospitals <- ret$hospitals |>
      dplyr::select(!tidyselect::any_of("orgUnit"))

  ret
}

read_organisationUnits_hospitals <- function(x) {
  if("geometry" %in% names(x))
    x <- x |> tidyr::hoist(
      "geometry",
      longitude = list("coordinates", 1),
      latitude = list("coordinates", 2)) |>
      dplyr::select(!"geometry")

  # The parent of the hospital is the country
  if("parent" %in% names(x))
    x <- x |> tidyr::hoist(
      "parent",
      country = "id")
  x |>
    dplyr::distinct() |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("hospital_key")
}

read_organisationUnits_departments <- function(x, y, include_hospital_ids) {

  if("hospitals" %in% names(y)){
    x <- x |>
      tidyr::hoist("parent", hospital_orgUnit = "id") |>
      dplyr::left_join(
        y$hospitals |>
          dplyr::select("orgUnit", "hospital_key"),
        dplyr::join_by("hospital_orgUnit" == "orgUnit")) |>
      dplyr::select(!"parent")
    if (!include_hospital_ids)
      x <- x |>
        dplyr::select(!tidyselect::any_of("hospital_orgUnit"))
  }

  cols <- names(x)
  if("openingDate" %in% cols)
    x <- x |>
      dplyr::mutate(
        openingDate =  readr::parse_date(
          stringr::str_sub(.data$openingDate, end = 10)))

  if("geometry" %in% cols)
    x <- x |>
    tidyr::hoist(
      "geometry",
      longitude = list("coordinates", 1),
      latitude = list("coordinates", 2)) |>
    dplyr::select(!"geometry")

  x |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("department_key")
}
