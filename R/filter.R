filter_dataset <- function(
    x,
    surveillance_end_from = NULL,
    surveillance_end_to = NULL,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestational_age_from = NULL,
    gestational_age_to = NULL,
    countries = NULL,
    keep_non_core_patients = FALSE,
    remove_orphans = TRUE)
{
  x$events <- x$events |>
    filter_surveillance_ends(
      surveillance_end_from,
      surveillance_end_to)

  x$admissionData <- x$admissionData |>
    filter_admissions(keep_non_core_patients)

  x$patients <- x$patients |>
    filter_patients(
      birth_weight_from,
      birth_weight_to,
      gestational_age_from,
      gestational_age_to,
      keep_non_core_patients)

  x$metadata$countries <- x$metadata$countries |>
    filter_countries(countries)

  if(remove_orphans)
    x <- x |>
    apply_postfilter()

  return(x)
}

filter_surveillance_ends <- function(
    events,
    surveillance_end_from = NULL,
    surveillance_end_to = NULL)
{
  if(is.null(surveillance_end_from) && is.null(surveillance_end_to))
    return(events)

  if(is.null(surveillance_end_from))
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt <= surveillance_end_to))
  else if(is.null(surveillance_end_to))
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt >= surveillance_end_from))
  else
    dplyr::bind_rows(
      events |>
        dplyr::filter(.data$event_type_key != "end"),
      events |>
        dplyr::filter(
          .data$event_type_key == "end" &
            .data$occurredAt >= surveillance_end_from &
            .data$occurredAt <= surveillance_end_to))
}

filter_admissions <- function(
    admission_data,
    keep_non_core_patients = FALSE)
{
  if(keep_non_core_patients)
    return(admission_data)

  admission_data |>
    dplyr::filter(.data$dol < 120)
}

filter_patients <- function(
    patients,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestation_weeks_from = NULL,
    gestation_weeks_to = NULL,
    keep_non_core_patients = FALSE)
{
  if(!is.null(birth_weight_from))
    patients <- patients |>
      dplyr::filter(.data$birth_weight >= birth_weight_from)
  if(!is.null(birth_weight_to))
    patients <- patients |>
      dplyr::filter(.data$birth_weight <= birth_weight_to)
  if(!is.null(gestation_weeks_from))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days >= (gestation_weeks_from * 7))
  if(!is.null(gestation_weeks_to))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days <= (gestation_weeks_to * 7))
  if(!keep_non_core_patients)
    patients <- patients |>
      dplyr::filter(
        .data$total_gestation_days < 224 | .data$birth_weight < 1500)
  return(patients)
}

filter_countries <- function(
    countries,
    included_countries)
{
  if(is.null(included_countries) || length(included_countries) < 1)
    return(countries)

  countries |>
    dplyr::filter(.data$code %in% included_countries)
}

apply_postfilter <- function(x)
{
  worldBankClasses <- x$metadata$worldBankClasses
  countries <- x$metadata$countries
  hospitals <- x$metadata$hospitals
  departments <- x$metadata$departments
  patients <- x$patients
  enrollments <- x$enrollments
  events <- x$events
  eventDetails <- x$eventDetails
  eventNotes <- x$eventNotes
  admissionData <- x$admissionData
  surveillanceEndData <- x$surveillanceEndData
  sepsisData <- x$sepsisData
  necData <- x$necData
  pneumoniaData <- x$pneumoniaData
  surgeryData <- x$surgeryData
  ssiData <- x$ssiData
  infectiousAgentFindings <- x$infectiousAgentFindings
  substanceDays <- x$substanceDays

  surveillance_end_events <- x$events |>
    dplyr::filter(.data$event_type_key != "end") |>
    dplyr::select("enrollment_key", "event_key")

  #############################
  ## First filter enrollments #
  #############################
  # Filtering by patients, admissionData and surveillance_end_events will always work
  enrollments <- enrollments |>
    dplyr::semi_join(patients, dplyr::join_by("patient_key")) |>
    dplyr::semi_join(
      surveillance_end_events |>
        dplyr::semi_join(
          admissionData, dplyr::join_by("event_key")),
      dplyr::join_by("enrollment_key"))

  # Filtering by country will only work if we have country information
  # Keep enrollments with NA country_key (test data without a country)
  if(!is.null(countries) && "country_key" %in% names(enrollments))
    enrollments <- enrollments |>
    dplyr::filter(
      is.na(.data$country_key) |
      .data$country_key %in% countries$country_key)

  # Filtering by unit will only work if we have unit information
  if(!is.null(departments) && "department_key" %in% names(enrollments))
    enrollments <- enrollments |>
    dplyr::semi_join(departments, dplyr::join_by("department_key"))

  ########################################################
  ## Second filter all the other elements by enrollments #
  ########################################################
  if(!is.null(departments) && "department_key" %in% names(enrollments))
    departments <- departments |>
    dplyr::semi_join(enrollments, dplyr::join_by("department_key"))
  if(!is.null(hospitals) && "hospital_key" %in% names(enrollments))
    hospitals <- hospitals |>
    dplyr::semi_join(enrollments, dplyr::join_by("hospital_key"))
  if(!is.null(countries) && "country_key" %in% names(enrollments))
    countries <- countries |>
    dplyr::semi_join(enrollments, dplyr::join_by("country_key"))
  if(!is.null(worldBankClasses) && "world_bank_class_key" %in% names(enrollments))
    worldBankClasses <- worldBankClasses |>
    dplyr::semi_join(enrollments, dplyr::join_by("world_bank_class_key"))

  patients <- patients |>
    dplyr::semi_join(enrollments, dplyr::join_by("patient_key"))

  events <- events |>
    dplyr::semi_join(enrollments, dplyr::join_by("enrollment_key"))

  if(!is.null(eventDetails))
    eventDetails <- eventDetails |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  if(!is.null(eventNotes))
    eventNotes <- eventNotes |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  admissionData <- admissionData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  surveillanceEndData <- surveillanceEndData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  sepsisData <- sepsisData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  necData <- necData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  pneumoniaData <- pneumoniaData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  surgeryData <- surgeryData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  ssiData <- ssiData |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  infectiousAgentFindings <- infectiousAgentFindings |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  substanceDays <- substanceDays |>
    dplyr::semi_join(events, dplyr::join_by("event_key"))

  x$metadata$worldBankClasses <- worldBankClasses
  x$metadata$countries <- countries
  x$metadata$hospitals <- hospitals
  x$metadata$departments <- departments
  x$patients <- patients
  x$enrollments <- enrollments
  x$events <- events
  x$eventDetails <- eventDetails
  x$eventNotes <- eventNotes
  x$admissionData <- admissionData
  x$surveillanceEndData <- surveillanceEndData
  x$sepsisData <- sepsisData
  x$necData <- necData
  x$pneumoniaData <- pneumoniaData
  x$surgeryData <- surgeryData
  x$ssiData <- ssiData
  x$infectiousAgentFindings <- infectiousAgentFindings
  x$substanceDays <- substanceDays

  return(x)
}

apply_data_removal <- function(x, dataset_options)
{
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
  else if(dataset_options$include_department == "pseudonymised")
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
  if(dataset_options$include_hospital == "pseudonymised")
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
  if(dataset_options$include_country == "pseudonymised")
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
  else if(dataset_options$include_world_bank_class == "pseudonymised")
    x$metadata$worldBankClasses <- NULL

  if(dataset_options$include_user == "pseudonymised")
    x$metadata$users <- NULL

  if(!dataset_options$include_patient_id)
    x$patients <- x$patients |>
      dplyr::select(!tidyselect::any_of("patient_id"))

  return(x)
}
