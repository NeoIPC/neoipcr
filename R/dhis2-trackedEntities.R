get_trackedEntities_request <- function(
    req_base, dataset_options, programId, trackedEntityTypeId)
{
  fields <- "trackedEntity,inactive,potentialDuplicate"
  attributeFields <- "attribute,value"

  if(dataset_options$include_unenrolled_patients)
    req_base <- req_base |>
    httr2::req_url_query(trackedEntityType = trackedEntityTypeId)
  else
  {
    req_base <- req_base |>
    httr2::req_url_query(program = programId)

    if(!("enrollments" %in% dataset_options$include_incomplete))
      req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")
  }

  if(dataset_options$include_timestamps)
  {
    fields <- paste0(fields,",createdAt,createdAtClient,updatedAt,updatedAtClient")
    attributeFields <- paste0(attributeFields,",createdAt,updatedAt")
  }

  if(!dataset_options$include_test_data ||
     !is.null(dataset_options$country_filter) ||
     !is.null(dataset_options$trial_keys) ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
  {
    fields <- paste0(fields,",createdBy[username],updatedBy[username]")
    attributeFields <- paste0(attributeFields,",storedBy")
  }

  fields <- paste0(fields,",attributes[", attributeFields, "]")

  req_base |>
    httr2::req_url_path_append("trackedEntities") |>
    httr2::req_url_query(fields = fields)
}

read_patients <- function(trackedEntities, metadata, dataset_options)
{
  patients <- trackedEntities |>
    tidyr::unnest_longer("attributes") |>
    tidyr::unnest_wider("attributes", names_sep = "_") |>
    dplyr::inner_join(
      metadata$trackedEntityAttributes |>
        dplyr::select("attribute","code","valueType","optionSet"),
      dplyr::join_by("attributes_attribute" == "attribute")) |>
    dplyr::left_join(
      metadata$options |>
        dplyr::arrange(.data$optionSet_code, .data$sortOrder) |>
        dplyr::group_by(.data$optionSet_code) |>
        dplyr::summarise(levels = list(.data$code)),
      dplyr::join_by("optionSet" == "optionSet_code")) |>
    dplyr::select(!c("attributes_attribute", "optionSet"))

  if(!dataset_options$include_patient_id)
    patients <- patients |>
      dplyr::filter(.data$code != "NEOIPC_PATIENT_ID")

  if(!dataset_options$include_timestamps)
    patients <- patients |>
      dplyr::mutate(dplyr::across(tidyselect::contains("At", ignore.case = FALSE), readr::parse_datetime))

  if(dataset_options$include_user != "no")
    patients <- patients |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("attributes_storedBy" == "username")) |>
      dplyr::mutate(attributes_storedBy = .data$user_key, .keep = "unused")

  if(!dataset_options$include_test_data ||
     !is.null(dataset_options$country_filter) ||
     !is.null(dataset_options$trial_keys))
    patients <- patients |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  patients <- patients |>
    dplyr::mutate(
      attributes_value = convert_value(
        .data$attributes_value, .data$valueType, .data$levels),
      code = stringr::str_extract(
        tolower(.data$code), "^neoipc_(tea_)?(.+)$", group = 2),
      .keep = "unused"
    ) |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = tidyselect::starts_with("attributes_"),
      names_glue = "{code}_{.value}",
      names_vary = "slowest") |>
    tidyr::unnest_longer(dplyr::ends_with("value"), keep_empty = TRUE) |>
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_attributes"),
      tidyselect::contains("_attributes_")) |>
    dplyr::relocate (
      tidyselect::any_of(
        c("patient_id_value","sex_value","siblings_value","gest_age_value",
          "birth_weight_value")),
      tidyselect::ends_with("_value"), .after = "trackedEntity") |>
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_value$"),
      tidyselect::ends_with("_value")) |>
    add_key_column("patient_key")

  if(dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
  {
    if(dataset_options$include_world_bank_class != "no")
      patients <- patients |>
        dplyr::inner_join(
          metadata$departments |>
            dplyr::select("orgUnit", "department_key", "hospital_key"),
          dplyr::join_by("orgUnit")) |>
        dplyr::inner_join(
          metadata$hospitals |>
            dplyr::select("hospital_key","country_key"),
          dplyr::join_by("hospital_key")) |>
        dplyr::inner_join(
          metadata$countries |>
            dplyr::select("country_key", "world_bank_class_key"),
          dplyr::join_by("country_key"))
    else if(dataset_options$include_country != "no")
      patients <- patients |>
        dplyr::inner_join(
          metadata$departments |>
            dplyr::select("orgUnit", "department_key", "hospital_key"),
          dplyr::join_by("orgUnit")) |>
        dplyr::inner_join(
          metadata$hospitals |>
            dplyr::select("hospital_key","country_key"),
          dplyr::join_by("hospital_key"))
    else if(dataset_options$include_hospital != "no")
      patients <- patients |>
        dplyr::inner_join(
          metadata$departments |>
            dplyr::select("orgUnit", "department_key", "hospital_key"),
          dplyr::join_by("orgUnit"))
    else if(dataset_options$include_department != "no")
      patients <- patients |>
        dplyr::inner_join(
          metadata$departments |>
            dplyr::select("orgUnit", "department_key"),
          dplyr::join_by("orgUnit"))

    exclusions <- NULL

    if(dataset_options$include_world_bank_class == "no")
      exclusions <- c(exclusions, "world_bank_class_key")
    if(dataset_options$include_country == "no")
      exclusions <- c(exclusions, "country_key")
    if(dataset_options$include_hospital == "no")
      exclusions <- c(exclusions, "hospital_key")
    if(dataset_options$include_department == "no")
      exclusions <- c(exclusions, "department_key")

    patients <- patients |>
      dplyr::select(!tidyselect::any_of(exclusions))
  }
  return(patients)
}
