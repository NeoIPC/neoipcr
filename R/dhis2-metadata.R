get_metadata <- function(d2_req_base, translate, locale)
{
  reqs <- list()

  md_req_base <- d2_req_base |>
    httr2::req_url_query(
      paging = "false",
      translate = tolower(translate))

  if(translate && rlang::is_character(locale, n = 1))
    md_req_base <- md_req_base |>
    httr2::req_url_query(
      locale = locale)

  md_req <- md_req_base |>
    httr2::req_url_path_append("metadata")

  list(
    # This is the overall query to get most of the NeoIPC-related metadata that
    # every NeoIPC user should be allowed to see
    md_req |>
      httr2::req_url_query(
        `programs:fields` = "id,programTrackedEntityAttributes[trackedEntityAttribute[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[id]]],programStages[id,name,displayName,displayFormName,displayDescription,programStageDataElements[dataElement[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]]]",
        `programs:filter` = "code:eq:NEOIPC_CORE",
        `organisationUnitGroups:fields` = "code,organisationUnits[id,code,displayName,displayShortName,displayDescription]",
        `organisationUnitGroups:filter` = "code:in:[COUNTRY,TEST_UNITS]",
        `organisationUnitGroupSets:fields` = "organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]",
        `organisationUnitGroupSets:filter` = "code:eq:NEOIPC_TRIALS",
        `optionGroupSets:fields` = "code,optionGroups[code,displayName,displayShortName,displayDescription,options[code]]",
        `optionGroupSets:filter` = "code:in:[ATC5,WHO_AWARE]",
        `options:fields` = "code,displayName,displayFormName,displayDescription,sortOrder,optionSet[code]",
        `options:filter` = "optionSet.code:in:[NEOIPC_ASA_SCORE,NEOIPC_ADMISSION_TYPES,NEOIPC_ANTIMICROBIAL_SUBSTANCES,NEOIPC_BSI_DEVICE_ASS,NEOIPC_BSI_PATHOGEN_RECOVERED_FROM,NEOIPC_DELIVERY_MODES,NEOIPC_HAP_DEVICE_ASS,NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES,NEOIPC_SSI_TYPE,NEOIPC_SEX_VALUES,NEOIPC_SURVEILLANCE_END_REASON,NEOIPC_WOUND_CLASSES,NEOIPC_YES_NO_NO_FOLLOWUP,NEOIPC_YES_NO_NOT_TESTED]"),

    # We try to read the complete user information via the metadata endpoint
    # so that we can audit who added/changed what.
    # This may fail due to insufficient rights
    md_req |>
      httr2::req_url_query(
        `users:fields` = "id,username,firstName,surname,email,created,lastLogin,organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]"),

    # As a fallback we read the user information of the user running the query
    # via the me endpoint so that they can at least see their own
    # additions/changes
    d2_req_base |>
      httr2::req_url_path_append("me") |>
      httr2::req_url_query(
        fields="id,username,firstName,surname,email,created,userCredentials[lastLogin],organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]"),

    # We query the organisationUnits endpoint so that we can apply the
    # withinUserHierarchy filter
    md_req_base |>
      httr2::req_url_path_append("organisationUnits") |>
      httr2::req_url_query(
        withinUserHierarchy = "true",
        fields = "id,displayName,displayShortName,displayDescription,openingDate,comment,geometry,parent[id,code,displayName,displayShortName,displayDescription,comment,geometry,parent[code]]",
        filter = "organisationUnitGroups.code:eq:NEO_DEPARTMENT")
  ) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    read_metadata_reponses()
}

read_metadata_reponses <- function(resps)
{
  metadata <- resps |>
    filter_metadata_reponses() |>
    lapply(read_metadata_reponse) |>
    unlist(recursive = FALSE)

  # Make the country_code column a factor but make sure it contains all
  # potential values
  metadata[["hospitals"]] <- metadata[["hospitals"]] |>
    dplyr::left_join(
      metadata[["countries"]] |>
        dplyr::select("code") |>
        dplyr::rename(zzz = .data$code),
      dplyr::join_by("country_code" == "zzz"),
      keep = TRUE) |>
    dplyr::mutate(country_code = .data$zzz, .keep = "unused")

  metadata$departments <- metadata$departments |>
    dplyr::mutate(isTestUnit = .data$id %in% metadata$testUnitIds) |>
    dplyr::left_join(metadata$trials |>
                       dplyr::select("code", "organisationUnits") |>
                       tidyr::unnest_longer(2) |>
                       tidyr::unnest_wider(2), dplyr::join_by("id")) |>
    dplyr::mutate(isTrial = !is.na(.data$code)) |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = "isTrial",
      values_fill = FALSE) |>
    dplyr::select(!tidyselect::any_of("NA"))

  metadata$testUnitIds <- NULL
  metadata$trials$organisationUnits <- NULL

  metadata
}

filter_metadata_reponses <- function(resps)
{
  successes <- resps |>
    httr2::resps_successes() |>
    resps_not_login()

  if(length(successes) == 3)
    return(successes)
  if(length(successes) == 4)
    return(successes[resps_not_me(resps)])

  rlang::abort(sprintf("Expected 3 or 4 successful queries but got %i.", length(successes)))
}

read_metadata_reponse <- function(resp)
{
  path <- httr2::resp_url_path(resp)
  json <- httr2::resp_body_json(resp)

  if(stringr::str_ends(path, "/metadata"))
  {
    if(!rlang::is_null(httr2::resp_url_query(resp, "users:fields")))
      return(json |> read_metadata_users())
    if(!rlang::is_null(httr2::resp_url_query(resp, "programs:fields")))
      return(json |> read_metadata())
  }
  else if(stringr::str_ends(path, "/me"))
    return(json |> read_me())
  else if(stringr::str_ends(path, "/organisationUnits"))
    return(json |> read_organisationUnits())

  rlang::abort("Unexpected DHIS2 metadata response.")
}

read_metadata_users <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(rlang::is_null(users))
    NULL
  else
    list(
      users = users |>
        tibble::tibble() |>
        tidyr::unnest_wider(1) |>
        dplyr::select(
          !c(
            "organisationUnits",
            "dataViewOrganisationUnits",
            "teiSearchOrganisationUnits",
            "userRoles")) |>
        dplyr::mutate(
          created = readr::parse_datetime(.data$created),
          lastLogin = readr::parse_datetime(.data$lastLogin)) |>
        add_key_column()
    )
}

read_metadata <- function(metadata)
{
  system <- read_metadata_system(metadata)
  programId <- read_metadata_program_id(metadata)
  eventTypes <- read_metadata_programStages(metadata)
  dataElements <- read_metadata_dataElements(metadata)
  trackedEntityAttributes <- read_metadata_trackedEntityAttributes(metadata)
  antimicrobialSubstances <- read_metadata_AntimicrobialSubstances(metadata)
  awareCategories <- read_metadata_AWaReCategories(metadata)
  atc5Categories <- read_metadata_atc5Categories(metadata)
  testUnitIds <- read_metadata_test_unit_ids(metadata)
  trials <- read_metadata_trials(metadata)

  options <- read_metadata_options(metadata)
  admissionTypes <- read_metadata_admissionTypes(options)
  asaScores <- read_metadata_asaScores(options)
  sepsisDeviceAssociation <- read_metadata_sepsisDeviceAssociation(options)
  sepsisPathogenSources  <- read_metadata_sepsis_pathogen_sources(options)
  deliveryModes <- read_metadata_deliveryModes(options)
  pneumoniaDeviceAssociation <- read_metadata_pneumoniaDeviceAssociation(options)
  pneumoniaPathogenSources <- read_metadata_pneumonia_pathogen_sources(options)
  sexes <- read_metadata_sexes(options)
  ssiTypes <- read_metadata_ssiTypes(options)
  surveillanceEndReasons <- read_metadata_surveillanceEndReasons(options)
  woundClasses <- read_metadata_woundClasses(options)
  testResults <- read_metadata_testResults(options)
  surveillanceResults <- read_metadata_surveillanceResults(options)

  countries <- read_metadata_countries(metadata)

  ret <- list(
    system = system,
    programId = programId,
    eventTypes = eventTypes,
    options = options,
    dataElements = dataElements,
    trackedEntityAttributes = trackedEntityAttributes,
    antimicrobialSubstances = antimicrobialSubstances,
    awareCategories = awareCategories,
    atc5Categories = atc5Categories,
    testUnitIds = testUnitIds,
    trials = trials,
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

  if(!rlang::is_null(countries))
    ret <- c(ret, list(countries = countries))

  ret
}

read_me <- function(me_data)
{
  list(
    users = me_data |>
      list() |>
      tibble::tibble() |>
      tidyr::unnest_wider(1) |>
      dplyr::select(
        !c(
          "organisationUnits",
          "dataViewOrganisationUnits",
          "teiSearchOrganisationUnits",
          "userRoles")) |>
      tidyr::hoist("userCredentials", lastLogin = "lastLogin", .remove = TRUE) |>
      dplyr::mutate(
        created = readr::parse_datetime(.data$created),
        lastLogin = readr::parse_datetime(.data$lastLogin)) |>
      add_key_column()
  )
}

read_organisationUnits <- function(organisationUnits)
{
  ret = list()
  hospitals <- read_organisationUnits_hospitals(organisationUnits)
  if(!rlang::is_null(hospitals))
    ret <- c(ret, list(hospitals = hospitals) )

  departments <- read_organisationUnits_departments(organisationUnits, hospitals)
  if(!rlang::is_null(departments))
    ret <- c(ret, list(departments = departments) )

  ret
}

read_metadata_system <- function(metadata)
{
  system <- purrr::pluck(metadata, "system")
  if(rlang::is_null(system))
    rlang::abort("Invalid DHIS2 metadata. The system element is missing.", "neoipcr_metadata_system_missing")

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
    rlang::abort("Invalid DHIS2 metadata. The program element is missing.", "neoipcr_metadata_program_missing")

  program_id
}

read_metadata_programStages <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(rlang::is_null(programStages))
    rlang::abort("Invalid DHIS2 metadata. The programStages list is missing.", "neoipcr_metadata_programStages_missing")

  programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(!tidyselect::any_of("programStageDataElements")) |>
    dplyr::mutate(
      name = factor(
        .data$name,
        levels = c(
          "Admission",
          "Surgical Procedure",
          "Primary Sepsis/BSI",
          "Necrotizing enterocolitis",
          "Surgical Site Infection",
          "Pneumonia",
          "Surveillance-End"))
    ) |>
    dplyr::arrange(.data$name) |>
    dplyr::mutate(
      displayName = factor(.data$displayName, levels = unique(.data$displayName)),
      displayFormName = factor(.data$displayFormName, levels = unique(.data$displayFormName))
    ) |>
    add_key_column()
}

read_metadata_dataElements <- function(metadata)
{
  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(rlang::is_null(programStages))
    rlang::abort("Invalid DHIS2 metadata. The programStages element is missing.", "neoipcr_metadata_programStages_missing")

  programStageTable <- programStages |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  if(!("programStageDataElements" %in% names(programStageTable)))
    rlang::abort("Invalid DHIS2 metadata. The programStageDataElements list is missing.", "neoipcr_metadata_programStageDataElements_missing")

  programStageTable |>
    dplyr::select("programStageDataElements") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::hoist("optionSet", optionSet = "code", .remove = FALSE)
}

read_metadata_trackedEntityAttributes <- function(metadata)
{
  programTrackedEntityAttributes <- metadata |>
    purrr::pluck("programs", 1, "programTrackedEntityAttributes")

  if(rlang::is_null(programTrackedEntityAttributes))
    rlang::abort("Invalid DHIS2 metadata. The programTrackedEntityAttributes list is missing.", "neoipcr_metadata_programTrackedEntityAttributes_missing")

  programTrackedEntityAttributes |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::hoist("optionSet", optionSet = "id", .remove = FALSE)
}

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

read_metadata_countries <- function(metadata)
{
  organisationUnitGroups <- read_metadata_organisationUnitGroups(metadata, "COUNTRY")

  if(rlang::is_null(organisationUnitGroups) || nrow(organisationUnitGroups) < 1)
    return(NULL)

  organisationUnitGroups |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(dplyr::across(!"id", ordered))
}

read_metadata_test_unit_ids <- function(metadata)
{
  organisationUnitGroups <- read_metadata_organisationUnitGroups(metadata, "TEST_UNITS")

  if(rlang::is_null(organisationUnitGroups) || nrow(organisationUnitGroups) < 1)
    return(NULL)

  organisationUnitGroups |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::pull("id")
}

read_metadata_trials <- function(metadata)
{
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroupSets", 1, "organisationUnitGroups")

  if(rlang::is_null(organisationUnitGroups))
    return(NULL)

  organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)
}

read_metadata_options <- function(metadata)
{
  options <- metadata |>
    purrr::pluck("options")

  if(rlang::is_null(options))
    rlang::abort("Invalid DHIS2 metadata. The options list is missing.", "neoipcr_metadata_options_missing")

  options |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    tidyr::unnest_wider("optionSet", names_sep = "_") |>
    dplyr::arrange(.data$optionSet_code, .data$sortOrder)
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

read_metadata_AntimicrobialSubstances <- function(metadata)
{
  optionGroupSets <- metadata |>
    purrr::pluck("optionGroupSets")

  if(rlang::is_null(optionGroupSets))
    rlang::abort("Invalid DHIS2 metadata. The optionGroupSets list is missing.", "neoipcr_metadata_optionGroupSets_missing")

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
        levels = c("WHO_AWARE_ACCESS","WHO_AWARE_WATCH","WHO_AWARE_RESERVE"))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(rlang::is_character), factor))
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


read_metadata_optionGroupSets <- function(metadata, filter, code_levels = NULL, ordered = FALSE)
{
  optionGroupSets <- metadata |>
    purrr::pluck("optionGroupSets")

  if(rlang::is_null(optionGroupSets))
    rlang::abort("Invalid DHIS2 metadata. The optionGroupSets list is missing.", "neoipcr_metadata_optionGroupSets_missing")

  optionGroupSets <- optionGroupSets |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(.data$code == filter)

  if(nrow(optionGroupSets) < 1)
    rlang::abort(sprintf("Invalid DHIS2 metadata. The optionGroupSets list does not contain elements with code %s.", filter), "neoipcr_metadata_optionGroupSets_code_missing")

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
      displayFormName = factor(
        .data$displayShortName,
        levels = unique(.data$displayShortName),
        ordered = ordered))

  optionGroupSets
}

read_metadata_atc5Categories <- function(metadata)
  atc5 <- read_metadata_optionGroupSets(metadata, "ATC5") |>
    dplyr::arrange(.data$code) |>
    dplyr::mutate(
      code = factor(.data$code),
      displayShortName = ordered(.data$displayShortName, levels = unique(.data$displayShortName)),
      displayName = ordered(.data$displayName, levels = unique(.data$displayName)))

read_metadata_AWaReCategories <- function(metadata)
  aware <- read_metadata_optionGroupSets(metadata, "WHO_AWARE",
                                         c("WHO_AWARE_ACCESS","WHO_AWARE_WATCH","WHO_AWARE_RESERVE"))

read_organisationUnits_hospitals <- function(organisationUnits)
{
  hospitals <- organisationUnits |>
    tibble::tibble() |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_longer(1) |>
    dplyr::filter(.data$organisationUnits_id == "parent")

  if(nrow(hospitals) < 1)
    NULL
  else
    hospitals <- hospitals |>
    dplyr::select(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::hoist("parent", country_code = "code")

  if("geometry" %in% names(hospitals))
    hospitals <- hospitals |>
      tidyr::hoist("geometry",
                   longitude = list("coordinates", 1),
                   latitude = list("coordinates", 2))

  hospitals |>
    dplyr::filter(.data$country_code != "NEOIPC") |>
    dplyr::select(!tidyselect::any_of("geometry")) |>
    dplyr::distinct() |>
    add_key_column() |>
    dplyr::arrange(.data$code)
}

read_organisationUnits_departments <- function(organisationUnits, hospitals)
{
  departments <- organisationUnits |>
    tibble::tibble() |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    tidyr::hoist("parent", parent_id = "id", parent_code = "code") |>
    dplyr::mutate(
      openingDate =  readr::parse_date(
        stringr::str_sub(.data$openingDate, end = 10)))


  if("geometry" %in% names(departments))
    departments <- departments |>
      tidyr::hoist("geometry",
                   longitude = list("coordinates", 1),
                   latitude = list("coordinates", 2))

  departments |>
    dplyr::left_join(hospitals |> dplyr::select("id", "key"),
                     dplyr::join_by("parent_id" == "id")) |>
    dplyr::mutate(hospital = .data$key, .keep = "unused") |>
    add_key_column() |>
    dplyr::arrange(.data$parent_code, .data$openingDate) |>
    dplyr::select(!tidyselect::starts_with("parent"))
}

resps_not_login <- function(resps)
  resps[vapply(resps, resp_not_login, logical(1))]

resp_not_login <- function(resp)
  httr2::resp_url_path(resp) |>
  stringr::str_ends("/security/login.action", negate = TRUE)

resps_not_me <- function(resps)
  vapply(resps, resp_not_me, logical(1))

resp_not_me <- function(resp)
  httr2::resp_url_path(resp) |>
  stringr::str_ends("/me", negate = TRUE)
