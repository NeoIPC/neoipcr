get_enrollments_request <- function(req_base, dataset_options, programId)
{
  fields <- "enrollment,trackedEntity,enrolledAt,followUp"

  if("enrollments" %in% dataset_options$include_notes)
    fields <- paste0(fields, ",notes")

  if("enrollments" %in% dataset_options$include_incomplete)
    fields <- paste0(fields,",status")
  else
    req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")

  if(dataset_options$include_timestamps)
    fields <- paste0(
      fields,
      ",occurredAt,createdAt,createdAtClient,updatedAt,updatedAtClient,completedAt")

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$department_filter) > 0 ||
     !is.null(dataset_options$trial_keys) ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
    fields <- paste0(
      fields,",completedBy,storedBy,createdBy[username],updatedBy[username]")

  req_base |>
    httr2::req_url_path_append("enrollments") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_enrollments <- function(enrollments, patients, metadata, dataset_options)
{
  enrollments <- enrollments |>
    dplyr::inner_join(
      patients |>
        dplyr::select("patient_key", "trackedEntity"),
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(
      enrolledAt = readr::parse_date(
        stringr::str_sub(.data$enrolledAt, end = 10))) |>
    dplyr::select(!"trackedEntity")

  if("enrollments" %in% dataset_options$include_incomplete)
    enrollments <- enrollments |>
      dplyr::mutate(
        status = factor(.data$status, levels = c(
          "ACTIVE", "COMPLETED", "CANCELLED")))

  if(dataset_options$include_timestamps)
    enrollments <- enrollments |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(
            c("occurredAt","createdAt","createdAtClient","updatedAt",
              "updatedAtClient","completedAt")), readr::parse_datetime))

  if(dataset_options$include_test_data ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
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
    if(dataset_options$include_test_data)
      cols <- c(cols, "isTest")

    enrollments <- enrollments |>
      dplyr::left_join(
        metadata$departments |>
          dplyr::select(tidyselect::any_of(cols)),
        dplyr::join_by("orgUnit"))
  }

  if(dataset_options$include_user != "no") {
    enrollments <- enrollments |>
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

    if("completedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$users |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("completedBy" == "username")) |>
        dplyr::mutate(completedBy = .data$user_key, .keep = "unused")

    if("storedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$users |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     !is.null(dataset_options$trial_keys))
    enrollments <- enrollments |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  enrollments |>
    dplyr::select(!"orgUnit") |>
    add_key_column("enrollment_key")
}
