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
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
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

  if(!dataset_options$include_patient_id && length(dataset_options$include_invalid_patients) <= 1)
    patients <- patients |>
      dplyr::filter(.data$code != "NEOIPC_PATIENT_ID")

  if(dataset_options$include_timestamps)
    patients <- patients |>
      dplyr::mutate(dplyr::across(tidyselect::contains("At", ignore.case = FALSE), readr::parse_datetime))
  else
    patients <- patients |>
      dplyr::select(!tidyselect::contains("At", ignore.case = FALSE))

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
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

  if(dataset_options$include_user != "no" &&
     "attributes_storedBy" %in% names(patients))
    patients <- patients |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("attributes_storedBy" == "username")) |>
      dplyr::mutate(attributes_storedBy = .data$user_key, .keep = "unused")

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
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
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
    # Build the list of columns to keep from the pre-joined departments table
    cols <- "orgUnit"
    if(dataset_options$include_department != "no" ||
       length(dataset_options$include_invalid_patients) > 1)
      cols <- c(cols, "department_key")
    if(dataset_options$include_hospital != "no")
      cols <- c(cols, "hospital_key")
    if(dataset_options$include_country != "no")
      cols <- c(cols, "country_key")
    if(dataset_options$include_world_bank_class != "no")
      cols <- c(cols, "world_bank_class_key")

    patients <- patients |>
      dplyr::left_join(
        metadata$departments |>
          dplyr::select(tidyselect::any_of(cols)),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!"orgUnit")
  }

  # Apply eligibility and range filters early so downstream joins operate on
  # fewer rows
  patients <- patients |>
    filter_patients(
      dataset_options$birth_weight_from,
      dataset_options$birth_weight_to,
      dataset_options$gestational_age_from,
      dataset_options$gestational_age_to,
      dataset_options$include_ineligible_patients)

  return(patients)
}
