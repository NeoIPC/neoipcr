get_enrollments_request <- function(req_base, dataset_options, programId)
{
  fields <- "enrollment,trackedEntity,enrolledAt,followUp,notes"

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
    dplyr::select(!c("trackedEntity","notes"))

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

  if(dataset_options$include_world_bank_class != "no")
    enrollments <- enrollments |>
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
    enrollments <- enrollments |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select("orgUnit", "department_key", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      dplyr::inner_join(
        metadata$hospitals |>
          dplyr::select("hospital_key","country_key"),
        dplyr::join_by("hospital_key"))
  else if(dataset_options$include_test_data ||
          dataset_options$include_hospital != "no" ||
          dataset_options$include_department != "no")
  {
    fields <- "orgUnit"

    if(dataset_options$include_hospital != "no")
      fields <- c(fields, "department_key", "hospital_key")
    else if(dataset_options$include_department != "no")
      fields <- c(fields, "department_key")

    if(dataset_options$include_test_data)
      fields <- c(fields, "isTest")

    enrollments <- enrollments |>
      dplyr::inner_join(
        metadata$departments |>
          dplyr::select(tidyselect::all_of(fields)),
        dplyr::join_by("orgUnit"))
  }

  if(dataset_options$include_user != "no")
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
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("completedBy" == "username")) |>
      dplyr::mutate(completedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$users |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("storedBy" == "username")) |>
      dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

  if(!dataset_options$include_test_data ||
     !is.null(dataset_options$country_filter) ||
     !is.null(dataset_options$trial_keys))
    enrollments <- enrollments |>
      dplyr::semi_join(metadata$departments, dplyr::join_by("orgUnit"))

  enrollments |>
    add_key_column("enrollment_key")
}
