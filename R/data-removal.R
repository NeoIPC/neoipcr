apply_data_removal <- function(x, dataset_options)
{
  if(!("id" %in% dataset_options$patient_columns))
  {
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("patient_id"))
  }

  if(!("patients" %in% dataset_options$include_dhis2_ids))
  {
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("trackedEntity"))
  }

  if(!("enrollments" %in% dataset_options$include_dhis2_ids))
  {
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("enrollment"))
  }

  if(!("events" %in% dataset_options$include_dhis2_ids))
  {
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("event"))
    if(!is.null(x$eventDetails))
      x$eventDetails <- x$eventDetails |>
        dplyr::select(!tidyselect::any_of("event"))
  }

  if(!("notes" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$eventNotes))
      x$eventNotes <- x$eventNotes |>
        dplyr::select(!tidyselect::any_of("note"))
  }

  if(!("event_types" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$metadata$eventTypes))
      x$metadata$eventTypes <- x$metadata$eventTypes |>
        dplyr::select(!tidyselect::any_of("programStage"))
  }

  if(!("users" %in% dataset_options$include_dhis2_ids))
  {
    if(!is.null(x$metadata$users))
      x$metadata$users <- x$metadata$users |>
        dplyr::select(!tidyselect::any_of("user"))
  }

  if(!("departments" %in% dataset_options$include_dhis2_ids))
  {
    x$metadata$departments <- x$metadata$departments |>
      dplyr::select(!tidyselect::any_of("orgUnit"))
  }

  if(dataset_options$include_department == "no")
  {
    x$metadata$departments <- NULL

    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("department_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("department_key"))
  }
  else if(dataset_options$include_department == "pseudo")
  {
    if("departments" %in% dataset_options$include_dhis2_ids)
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(tidyselect::all_of("department_key"))
    else
      x$metadata$departments <- NULL
  }

  # `orgUnit` is the raw DHIS2 organisationUnit id for the department
  # the row belongs to. The readers keep it on patient/enrollment rows
  # so the apply_postfilter cascade can filter on it, but it is a
  # DHIS2-id leak that defeats both the "no" and "pseudonymised"
  # branches of include_department — drop it once we're past filtering.
  if(dataset_options$include_department != "yes")
  {
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("orgUnit"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("orgUnit"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("orgUnit"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("orgUnit"))
  }

  if(dataset_options$include_hospital == "no")
  {
    x$metadata$hospitals <- NULL
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("hospital_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("hospital_key"))
  }
  if(dataset_options$include_hospital == "pseudo")
    x$metadata$hospitals <- NULL

  if(dataset_options$include_country == "no")
  {
    x$metadata$countries <- NULL
    if(!is.null(x$metadata$hospitals))
      x$metadata$hospitals <- x$metadata$hospitals |>
        dplyr::select(!tidyselect::any_of("country_key"))
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("country_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("country_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("country_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("country_key"))
  }
  if(dataset_options$include_country == "pseudo")
    x$metadata$countries <- NULL

  if(dataset_options$include_world_bank_class == "no")
  {
    x$metadata$worldBankClasses <- NULL
    if(!is.null(x$metadata$countries))
      x$metadata$countries <- x$metadata$countries |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    if(!is.null(x$metadata$hospitals))
      x$metadata$hospitals <- x$metadata$hospitals |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    if(!is.null(x$metadata$departments))
      x$metadata$departments <- x$metadata$departments |>
        dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$enrollments <- x$enrollments |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
    x$events <- x$events |>
      dplyr::select(!tidyselect::any_of("world_bank_class_key"))
  }
  else if(dataset_options$include_world_bank_class == "pseudo")
    x$metadata$worldBankClasses <- NULL

  if(dataset_options$include_user == "pseudonymised")
    x$metadata$users <- NULL

  if(!("id" %in% dataset_options$patient_columns))
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("patient_id"))

  return(x)
}
