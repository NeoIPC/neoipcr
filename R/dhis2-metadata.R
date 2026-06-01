get_metadata <- function(d2_req_base, user_info, dataset_options)
{
  md_req_base <- d2_req_base |>
    httr2::req_url_query(
      paging = "false",
      translate = tolower(dataset_options$translate))

  if(dataset_options$translate &&
     rlang::is_character(dataset_options$locale, n = 1))
    md_req_base <- md_req_base |>
      httr2::req_url_query(
        locale = dataset_options$locale)

  requests <- list(
    get_metadata_request(md_req_base, user_info, dataset_options),
    get_organisationUnit_request(md_req_base, user_info, dataset_options)) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    read_metadata_reponses(user_info, dataset_options)
}

# Creates the overall query to get most of the NeoIPC-related metadata that
# every NeoIPC user should be allowed to see
get_metadata_request <- function(req_base, user_info, dataset_options)
{
  req <- req_base |>
    httr2::req_url_path_append("metadata")

  if(length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_world_bank_class != "no")
  {
    if(length(dataset_options$trial_keys) == 0)
    {
      if(dataset_options$include_world_bank_class == "yes")
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]")
      else # pseudonymise
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,organisationUnits[id]]")

      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroupSets:filter` = "code:eq:WORLD_BANK_CLASSES")
    }
    else
    {
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]")

      if(dataset_options$include_world_bank_class == "no")
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:filter` = "code:eq:NEOIPC_TRIALS")
      else
        req <- req |>
          httr2::req_url_query(
            `organisationUnitGroupSets:filter` = "code:in:[NEOIPC_TRIALS,WORLD_BANK_CLASSES]")
    }
  }

  if(length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
  {
    if(dataset_options$include_country == "yes")
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,code,displayName,displayShortName,displayDescription]")
    else if(length(dataset_options$country_filter) > 0)
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,code]")
    else # pseudonymised, or include_world_bank_class != "no"
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id]")

    req <- req |>
      httr2::req_url_query(
        `organisationUnitGroups:filter` = "code:in:[COUNTRY,TEST_UNITS]")
  }
  else
    req <- req |>
      httr2::req_url_query(
        `organisationUnitGroups:fields` = "code,organisationUnits[id]",
        `organisationUnitGroups:filter` = "code:eq:TEST_UNITS")

  req <- req |>
    httr2::req_url_query(
      `programs:fields` = "id,programTrackedEntityAttributes[trackedEntityAttribute[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]],programStages[id,name,displayName,displayFormName,displayDescription,programStageDataElements[dataElement[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]]]",
      `programs:filter` = "code:eq:NEOIPC_CORE",
      `trackedEntityTypes:fields` = "id",
      `trackedEntityTypes:filter` = "name:eq:NeoIPC Patient",
      `optionGroupSets:fields` = "code,optionGroups[code,displayName,displayShortName,displayDescription,options[code]]",
      `optionGroupSets:filter` = "code:in:[ATC5,WHO_AWARE]",
      `options:fields` = "code,displayName,displayFormName,displayDescription,sortOrder,optionSet[code]",
      `options:filter` = "optionSet.code:in:[NEOIPC_ASA_SCORE,NEOIPC_ADMISSION_TYPES,NEOIPC_ANTIMICROBIAL_SUBSTANCES,NEOIPC_BSI_DEVICE_ASS,NEOIPC_BSI_PATHOGEN_RECOVERED_FROM,NEOIPC_DELIVERY_MODES,NEOIPC_HAP_DEVICE_ASS,NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES,NEOIPC_SSI_TYPE,NEOIPC_SEX_VALUES,NEOIPC_SURVEILLANCE_END_REASON,NEOIPC_WOUND_CLASSES,NEOIPC_YES_NO_NO_FOLLOWUP,NEOIPC_YES_NO_NOT_TESTED]"
    )

  # We only read the complete user information via the metadata endpoint if we
  # have the required authorities to do so
  if (dataset_options$include_user != "no" && length(intersect(c("ALL","F_METADATA_EXPORT","F_USER_VIEW"), user_info$authorities)) > 0) {
    if(dataset_options$include_user == "yes")
      req <- req |>
        httr2::req_url_query(
          `users:fields` = "id,username,firstName,surname,email,created,lastLogin,organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]")
    else # pseudonymise
      req <- req |>
        httr2::req_url_query(
          `users:fields` = "id,username")
  }
  req
}

# Creates the organisationUnits query, which we use, so that we can apply the
# withinUserHierarchy filter
get_organisationUnit_request <- function(req_base, user_info, dataset_options)
{
  fields <- "id"

  if(dataset_options$include_department == "yes")
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

  if(dataset_options$include_hospital == "yes")
    fields <- paste0(fields, paste0(",parent[id,code,displayName,displayShortName,displayDescription,comment,geometry", country_fields))
  else if (dataset_options$include_hospital == "pseudonymised" ||
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

read_metadata_reponses <- function(resps, user_info, dataset_options)
{
  metadata <- resps |>
    lapply(read_metadata_reponse, dataset_options) |>
    unlist(recursive = FALSE)

  if (!("users" %in% names(metadata)))
    metadata$users <- read_user_info_table(
      user_info,
      dataset_options$include_user)

  if(dataset_options$include_test_data)
    metadata$departments <- metadata$departments |>
      dplyr::mutate(isTest = .data$orgUnit %in% metadata$testUnitIds)
  else
    metadata$departments <- metadata$departments |>
      dplyr::filter(!(.data$orgUnit %in% metadata$testUnitIds))

  # Filter departments by department_filter
  if (length(dataset_options$department_filter) > 0)
    metadata$departments <- metadata$departments |>
      dplyr::filter(.data$code %in% dataset_options$department_filter)

  # Filter countries by country_filter and remove departments not in those countries
  if (length(dataset_options$country_filter) > 0 && !is.null(metadata$countries))
  {
    metadata$countries <- metadata$countries |>
      dplyr::filter(.data$code %in% dataset_options$country_filter)

    if (!is.null(metadata$hospitals) &&
        "hospital_key" %in% names(metadata$departments))
    {
      filtered_hospital_keys <- metadata$hospitals |>
        dplyr::semi_join(metadata$countries, dplyr::join_by("country")) |>
        dplyr::pull("hospital_key")

      if (dataset_options$include_test_data &&
          "isTest" %in% names(metadata$departments))
        metadata$departments <- metadata$departments |>
          dplyr::filter(
            .data$isTest | .data$hospital_key %in% filtered_hospital_keys)
      else
        metadata$departments <- metadata$departments |>
          dplyr::filter(.data$hospital_key %in% filtered_hospital_keys)
    }
  }

  # Filter hospitals to only those with remaining departments
  if(dataset_options$include_hospital != "no" ||
     length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    metadata$hospitals <- metadata$hospitals |>
      dplyr::semi_join(
        metadata$departments |>
          dplyr::select("hospital_key"),
        dplyr::join_by("hospital_key"))

  # Join country_key into hospitals
  if(dataset_options$include_country != "no" ||
     length(dataset_options$country_filter) > 0 ||
     dataset_options$include_world_bank_class != "no")
  {
    metadata$hospitals <- metadata$hospitals |>
      dplyr::left_join(
        metadata$countries |>
          dplyr::select("country","country_key"),
        dplyr::join_by("country")) |>
      dplyr::select(!"country")
  }

  # Pre-join hierarchy into departments so that read_patients/enrollments/events
  # can use a single flat left_join instead of cascading joins
  if ("hospital_key" %in% names(metadata$departments) &&
      !is.null(metadata$hospitals) &&
      "country_key" %in% names(metadata$hospitals))
  {
    metadata$departments <- metadata$departments |>
      dplyr::left_join(
        metadata$hospitals |>
          dplyr::select("hospital_key", "country_key"),
        dplyr::join_by("hospital_key"))

    if (!is.null(metadata$countries) &&
        "world_bank_class_key" %in% names(metadata$countries))
      metadata$departments <- metadata$departments |>
        dplyr::left_join(
          metadata$countries |>
            dplyr::select("country_key", "world_bank_class_key"),
          dplyr::join_by("country_key"))
  }

  metadata$testUnitIds <- NULL

  metadata
}

read_metadata_reponse <- function(resp, dataset_options)
{
  path <- httr2::resp_url_path(resp)
  json <- httr2::resp_body_json(resp)

  if(stringr::str_ends(path, "/metadata"))
    return(json |> read_metadata(dataset_options))
  else if(stringr::str_ends(path, "/organisationUnits"))
    return(json |> read_organisationUnits(dataset_options))

  rlang::abort("Unexpected DHIS2 metadata response.")
}

read_metadata <- function(metadata, dataset_options)
{
  system <- read_metadata_system(metadata)
  programId <- read_metadata_program_id(metadata)
  trackedEntityTypeId <- metadata$trackedEntityTypes |>
    unlist(use.names = FALSE)
  eventTypes <- read_metadata_programStages(metadata)
  dataElements <- read_metadata_dataElements(metadata)
  options <- read_metadata_options(metadata)
  admissionTypes <- read_metadata_admissionTypes(options)
  asaScores <- read_metadata_asaScores(options)
  sepsisDeviceAssociation <- read_metadata_sepsisDeviceAssociation(options)
  sepsisPathogenSources  <- read_metadata_sepsis_pathogen_sources(options)
  deliveryModes <- read_metadata_deliveryModes(options)
  pneumoniaDeviceAssociation <- read_metadata_pneumoniaDeviceAssociation(
    options)
  pneumoniaPathogenSources <- read_metadata_pneumonia_pathogen_sources(options)
  sexes <- read_metadata_sexes(options)
  ssiTypes <- read_metadata_ssiTypes(options)
  surveillanceEndReasons <- read_metadata_surveillanceEndReasons(options)
  woundClasses <- read_metadata_woundClasses(options)
  testResults <- read_metadata_testResults(options)
  surveillanceResults <- read_metadata_surveillanceResults(options)
  trackedEntityAttributes <- read_metadata_trackedEntityAttributes(metadata)
  antimicrobialSubstances <- read_metadata_AntimicrobialSubstances(metadata)
  awareCategories <- read_metadata_AWaReCategories(metadata)
  atc5Categories <- read_metadata_atc5Categories(metadata)
  testUnitIds <- read_metadata_test_unit_ids(
    metadata, dataset_options$include_test_data)

  users <- read_metadata_users(
    metadata,
    dataset_options$include_user)

  trials <- read_metadata_trials(
    metadata,
    dataset_options$trial_keys)

  world_bank_classes <- read_metadata_wb_classes(
    metadata,
    dataset_options$include_world_bank_class)

  countries <- read_metadata_countries(
    metadata,
    dataset_options$include_country,
    length(dataset_options$country_filter) > 0,
    world_bank_classes)

  ret <- list(
    system = system,
    programId = programId,
    trackedEntityTypeId = trackedEntityTypeId,
    eventTypes = eventTypes,
    options = options,
    dataElements = dataElements,
    trackedEntityAttributes = trackedEntityAttributes,
    antimicrobialSubstances = antimicrobialSubstances,
    awareCategories = awareCategories,
    atc5Categories = atc5Categories,
    testUnitIds = testUnitIds,
    admissionTypes = admissionTypes,
    asaScores = asaScores,
    sepsisDeviceAssociation = sepsisDeviceAssociation,
    sepsisPathogenSources = sepsisPathogenSources,
    deliveryModes = deliveryModes,
    pneumoniaDeviceAssociation = pneumoniaDeviceAssociation,
    pneumoniaPathogenSources = pneumoniaPathogenSources,
    sexes = sexes,
    ssiTypes = ssiTypes,
    surveillanceEndReasons = surveillanceEndReasons,
    woundClasses = woundClasses,
    testResults = testResults,
    surveillanceResults = surveillanceResults
  )

  if(!is.null(users))
    ret <- c(ret, list(users = users))
  if(!is.null(trials))
    ret <- c(ret, list(trials = trials))
  if(!is.null(world_bank_classes))
    ret <- c(ret, list(worldBankClasses = world_bank_classes))
  if(!is.null(countries))
    ret <- c(ret, list(countries = countries))

  ret
}

read_user_info_table <- function(user_info, include_user)
{
  if(include_user == "no")
    return(NULL)

  if(include_user == "yes")
    return(
      user_info |>
        list() |>
        tibble::tibble() |>
        tidyr::unnest_wider(1) |>
        dplyr::select(!c(
          "organisationUnits",
          "dataViewOrganisationUnits",
          "teiSearchOrganisationUnits",
          "groups",
          "roles",
          "authorities")) |>
        add_key_column("user_key"))

  user_info <- tibble::tibble(
    user_key = 1L,
    user = user_info$id,
    username = user_info$username)
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

read_metadata_system <- function(metadata)
{
  system <- purrr::pluck(metadata, "system")
  if(rlang::is_null(system))
    rlang::abort("Invalid DHIS2 metadata. The system element is missing.",
                 "neoipcr_metadata_system_missing")

  list(id = uuid::as.UUID(system$id),
       version = as.numeric_version(system$version),
       rev = system$rev,
       date = readr::parse_datetime(system$date))
}

read_metadata_program_id <- function(metadata)
{
  program_id <- metadata |>
    purrr::pluck("programs", 1, "id")

  if(rlang::is_null(program_id))
    rlang::abort("Invalid DHIS2 metadata. The program element is missing.",
                 "neoipcr_metadata_program_missing")

  program_id
}

read_metadata_programStages <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(rlang::is_null(programStages))
    rlang::abort("Invalid DHIS2 metadata. The programStages list is missing.",
                 "neoipcr_metadata_programStages_missing")

  programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(!tidyselect::any_of("programStageDataElements")) |>
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = c("Admission","Surgical Procedure","Primary Sepsis/BSI",
                   "Necrotizing enterocolitis","Surgical Site Infection",
                   "Pneumonia","Surveillance-End")),
      event_type_key = factor(
        dplyr::case_match(
          .data$name,
          "Admission" ~ "adm",
          "Surgical Procedure" ~ "pro",
          "Primary Sepsis/BSI" ~ "bsi",
          "Necrotizing enterocolitis" ~ "nec",
          "Surgical Site Infection" ~ "ssi",
          "Pneumonia" ~ "hap",
          "Surveillance-End" ~ "end"
        ),
        levels = c("adm","pro","bsi","nec","ssi","hap","end"))
    ) |>
    dplyr::arrange(.data$name) |>
    dplyr::mutate(
      displayName = factor(
        .data$displayName,
        levels = unique(.data$displayName)),
      displayFormName = factor(
        .data$displayFormName,
        levels = unique(.data$displayFormName))
    ) |>
    dplyr::relocate("event_type_key", "programStage" = "id")
}

read_metadata_dataElements <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(rlang::is_null(programStages))
    rlang::abort(
      "Invalid DHIS2 metadata. The programStages element is missing.",
      "neoipcr_metadata_programStages_missing")

  programStageTable <- programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  if(!("programStageDataElements" %in% names(programStageTable)))
    rlang::abort(
      "Invalid DHIS2 metadata. The programStageDataElements list is missing.",
      "neoipcr_metadata_programStageDataElements_missing")

  programStageTable |>
    dplyr::select("programStageDataElements") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::hoist("optionSet", optionSet = "code", .remove = FALSE) |>
    dplyr::relocate(dataElement = "id")
}

read_metadata_options <- function(metadata)
{
  options <- metadata |>
    purrr::pluck("options")

  if(rlang::is_null(options))
    rlang::abort("Invalid DHIS2 metadata. The options list is missing.",
                 "neoipcr_metadata_options_missing")

  options |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider("optionSet", names_sep = "_") |>
    dplyr::arrange(.data$optionSet_code, .data$sortOrder)
}

read_metadata_admissionTypes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_ADMISSION_TYPES") |>
  convert_metadata_options()

read_metadata_asaScores <- function(options)
  options |>
  filter_metadata_options("NEOIPC_ASA_SCORE") |>
  convert_metadata_options()

read_metadata_sepsisDeviceAssociation <- function(options)
  options |>
  filter_metadata_options("NEOIPC_BSI_DEVICE_ASS") |>
  convert_metadata_options()

read_metadata_sepsis_pathogen_sources <- function(options)
  options |>
  filter_metadata_options("NEOIPC_BSI_PATHOGEN_RECOVERED_FROM") |>
  convert_metadata_options()

read_metadata_deliveryModes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_DELIVERY_MODES") |>
  convert_metadata_options()

read_metadata_pneumoniaDeviceAssociation <- function(options)
  options |>
  filter_metadata_options("NEOIPC_HAP_DEVICE_ASS") |>
  convert_metadata_options()

read_metadata_pneumonia_pathogen_sources <- function(options)
  options |>
  filter_metadata_options("NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES") |>
  convert_metadata_options()

read_metadata_sexes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SEX_VALUES") |>
  convert_metadata_options()

read_metadata_ssiTypes <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SSI_TYPE") |>
  convert_metadata_options()

read_metadata_surveillanceEndReasons <- function(options)
  options |>
  filter_metadata_options("NEOIPC_SURVEILLANCE_END_REASON") |>
  convert_metadata_options()

read_metadata_woundClasses <- function(options)
  options |>
  filter_metadata_options("NEOIPC_WOUND_CLASSES") |>
  convert_metadata_options()

read_metadata_testResults <- function(options)
  options |>
  filter_metadata_options("NEOIPC_YES_NO_NOT_TESTED") |>
  convert_metadata_options()

read_metadata_surveillanceResults <- function(options)
  options |>
  filter_metadata_options("NEOIPC_YES_NO_NO_FOLLOWUP") |>
  convert_metadata_options()

read_metadata_trackedEntityAttributes <- function(metadata)
{
  programTrackedEntityAttributes <- metadata |>
    purrr::pluck("programs", 1, "programTrackedEntityAttributes")

  if(rlang::is_null(programTrackedEntityAttributes))
    rlang::abort(
      "Invalid DHIS2 metadata. The programTrackedEntityAttributes list is missing.",
      "neoipcr_metadata_programTrackedEntityAttributes_missing")

  programTrackedEntityAttributes |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::rename(attribute = "id") |>
    tidyr::hoist("optionSet", optionSet = "code", .remove = FALSE)
}

read_metadata_AntimicrobialSubstances <- function(metadata)
{
  optionGroupSets <- metadata |>
    purrr::pluck("optionGroupSets")

  if(rlang::is_null(optionGroupSets))
    rlang::abort("Invalid DHIS2 metadata. The optionGroupSets list is missing.",
                 "neoipcr_metadata_optionGroupSets_missing")

  optionGroupSets <- optionGroupSets |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::rename(system = "code") |>
    tidyr::unnest_longer(2) |>
    tidyr::unnest_wider(2) |>
    dplyr::rename(group = "code") |>
    dplyr::select(c("system", "group", "options")) |>
    tidyr::unnest_longer("options") |>
    tidyr::hoist("options", code = "code")

  read_metadata_options(metadata) |>
    filter_metadata_options("NEOIPC_ANTIMICROBIAL_SUBSTANCES") |>
    dplyr::left_join(optionGroupSets, dplyr::join_by("code")) |>
    tidyr::pivot_wider(names_from = "system", values_from = "group") |>
    dplyr::mutate(
      WHO_AWARE = factor(.data$WHO_AWARE,
                         levels = c(
                           "WHO_AWARE_ACCESS","WHO_AWARE_WATCH",
                           "WHO_AWARE_RESERVE"))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(rlang::is_character), factor))
}

read_metadata_AWaReCategories <- function(metadata)
  aware <- read_metadata_optionGroupSets(
    metadata, "WHO_AWARE",
    c("WHO_AWARE_ACCESS","WHO_AWARE_WATCH","WHO_AWARE_RESERVE"))

read_metadata_atc5Categories <- function(metadata)
  atc5 <- read_metadata_optionGroupSets(metadata, "ATC5") |>
  dplyr::arrange(.data$code) |>
  dplyr::mutate(
    code = factor(.data$code),
    displayShortName = ordered(
      .data$displayShortName, levels = unique(.data$displayShortName)),
    displayName = ordered(
      .data$displayName, levels = unique(.data$displayName)))

read_metadata_test_unit_ids <- function(metadata, include_test_data)
{
  organisationUnitGroups <- read_metadata_organisationUnitGroups(
    metadata, "TEST_UNITS")

  if(rlang::is_null(organisationUnitGroups) || nrow(organisationUnitGroups) < 1)
    return(NULL)

  organisationUnitGroups |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::pull("id")
}

read_metadata_users <- function(metadata, include_user)
{
  if(include_user == "no")
    return(invisible(NULL))

  users <- metadata |>
    purrr::pluck("users")

  if(rlang::is_null(users))
    return(invisible(NULL))

  users <- users |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  if(include_user == "yes")
    users <- users |>
    dplyr::select(
      !c(
        "organisationUnits",
        "dataViewOrganisationUnits",
        "teiSearchOrganisationUnits",
        "userRoles")) |>
    dplyr::mutate(
      created = readr::parse_datetime(.data$created),
      lastLogin = readr::parse_datetime(.data$lastLogin)) |>
    dplyr::relocate("user" = "id","username","firstName","surname","email",
                    "lastLogin","created")
  else
    users <- users |>
    dplyr::rename("user" = "id")

  users |>
    add_key_column("user_key")
}

read_metadata_trials <- function(metadata, trial_keys)
{
  if(length(trial_keys) < 1)
    return(NULL)

  groupSets <- purrr::pluck(metadata, "organisationUnitGroupSets")
  if(rlang::is_null(groupSets))
    return(NULL)

  trialsIdx <- NULL
  for (i in seq_along(groupSets)) {
    if (identical(purrr::pluck(groupSets, i, "code"), "NEOIPC_TRIALS")) {
      trialsIdx <- i
      break
    }
  }
  if(is.null(trialsIdx))
    return(NULL)

  organisationUnitGroups <- purrr::pluck(groupSets, trialsIdx, "organisationUnitGroups")
  if(rlang::is_null(organisationUnitGroups))
    return(NULL)

  organisationUnitGroups <- organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(
      stringr::str_detect(
        .data$code,
        stringr::regex(paste0(trial_keys, collapse = "|"),
                       ignore_case = TRUE)))
}

read_metadata_wb_classes <- function(metadata, include_world_bank_class)
{
  if(include_world_bank_class == "no")
    return(NULL)

  groupSets <- purrr::pluck(metadata, "organisationUnitGroupSets")
  if(rlang::is_null(groupSets))
    return(NULL)

  wbIdx <- NULL
  for (i in seq_along(groupSets)) {
    if (identical(purrr::pluck(groupSets, i, "code"), "WORLD_BANK_CLASSES")) {
      wbIdx <- i
      break
    }
  }
  if(is.null(wbIdx))
    return(NULL)

  organisationUnitGroups <- purrr::pluck(groupSets, wbIdx, "organisationUnitGroups")
  if(rlang::is_null(organisationUnitGroups))
    return(NULL)

  pseudonymise <- include_world_bank_class != "yes"

  organisationUnitGroups <- organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  if(pseudonymise)
    organisationUnitGroups <- organisationUnitGroups |>
    dplyr::mutate(
      fiscal_year = as.integer(
        stringr::str_extract(
          .data$code, "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$", group = 2)),
      .keep = "unused",
      .before = 1) |>
    dplyr::arrange(dplyr::desc(.data$fiscal_year))
  else
    organisationUnitGroups <- organisationUnitGroups |>
    dplyr::mutate(
      class = factor(
        stringr::str_extract(
          .data$code,
          "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$",
          group = 1),
        levels = c('L','LM','UM','H')),
      fiscal_year = as.integer(
        stringr::str_extract(
          .data$code,
          "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$",
          group = 2)),
      .keep = "unused",
      .before = 1) |>
    dplyr::arrange(dplyr::desc(.data$fiscal_year), .data$class)

  if (nrow(organisationUnitGroups) == 0L) {
    filtered <- organisationUnitGroups
  } else {
    current_year <- as.POSIXlt(Sys.Date())$year + 1900
    candidates <- organisationUnitGroups$fiscal_year[organisationUnitGroups$fiscal_year <= current_year]
    target_year <- if (length(candidates) > 0L) max(candidates) else max(organisationUnitGroups$fiscal_year, na.rm = TRUE)
    filtered <- organisationUnitGroups |>
      dplyr::filter(.data$fiscal_year == target_year)
  }

  filtered <- filtered |>
    add_key_column('world_bank_class_key')

  if(pseudonymise)
    return(filtered |> dplyr::select(!"fiscal_year"))

  filtered
}

read_metadata_countries <- function(
    metadata, include_country, has_country_filter, world_bank_classes)
{
  if(!has_country_filter && include_country == "no" && is.null(world_bank_classes))
    return(NULL)

  organisationUnitGroups <- read_metadata_organisationUnitGroups(
    metadata, "COUNTRY")

  if(rlang::is_null(organisationUnitGroups) || nrow(organisationUnitGroups) < 1)
    return(NULL)

  organisationUnitGroups <- organisationUnitGroups |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(dplyr::across(!"id", ordered)) |>
    dplyr::relocate("id", .before = 1) |>
    dplyr::rename(country = .data$id) |>
    add_key_column('country_key')

  if(!is.null(world_bank_classes))
    organisationUnitGroups <- organisationUnitGroups |>
    dplyr::left_join(
      world_bank_classes |>
        dplyr::select("world_bank_class_key", "organisationUnits") |>
        tidyr::unnest_longer("organisationUnits") |>
        tidyr::hoist("organisationUnits", country = list(1L)),
      dplyr::join_by("country"))

  organisationUnitGroups
}

read_metadata_optionGroupSets <- function(
    metadata, filter, code_levels = NULL, ordered = FALSE)
{
  optionGroupSets <- metadata |>
    purrr::pluck("optionGroupSets")

  if(rlang::is_null(optionGroupSets))
    rlang::abort("Invalid DHIS2 metadata. The optionGroupSets list is missing.",
                 "neoipcr_metadata_optionGroupSets_missing")

  optionGroupSets <- optionGroupSets |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == filter)

  if(nrow(optionGroupSets) < 1)
    rlang::abort(
      sprintf(
        "Invalid DHIS2 metadata. The optionGroupSets list does not contain elements with code %s.",
        filter), "neoipcr_metadata_optionGroupSets_code_missing")

  optionGroupSets <- optionGroupSets |>
    dplyr::select(2) |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::select(!"options")

  if(!rlang::is_null(code_levels))
    optionGroupSets <- optionGroupSets |>
    dplyr::mutate(
      code = factor(.data$code, levels = code_levels, ordered = ordered)) |>
    dplyr::arrange(.data$code) |>
    dplyr::mutate(
      displayName = factor(
        .data$displayName,
        levels = unique(.data$displayName),
        ordered = ordered),
      displayShortName = factor(
        .data$displayShortName,
        levels = unique(.data$displayShortName),
        ordered = ordered))

  optionGroupSets
}

filter_metadata_options <- function(options, filter)options |>
  dplyr::filter(.data$optionSet_code == filter) |>
  dplyr::select(!"optionSet_code")

convert_metadata_options <- function(options, ordered = FALSE)
  options |>
  dplyr::mutate(
    code = factor(.data$code, levels = unique(.data$code), ordered = ordered),
    displayName = factor(.data$displayName, levels = unique(.data$displayName), ordered = ordered),
    displayFormName = factor(.data$displayFormName, levels = unique(.data$displayFormName), ordered = ordered))

read_metadata_organisationUnitGroups <- function(metadata, code_filter)
{
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroups")

  if(rlang::is_null(organisationUnitGroups) || length(organisationUnitGroups) < 1 || length(organisationUnitGroups[[1]]) < 1)
    return(NULL)

  organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == code_filter)
}
