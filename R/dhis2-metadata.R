get_metadata <- function(d2_req_base, user_info, translate, locale)
{
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

  requests <- list(
    # This is the overall query to get most of the NeoIPC-related metadata that
    # every NeoIPC user should be allowed to see
    md_req |>
      httr2::req_url_query(
        `programs:fields` = "id,programTrackedEntityAttributes[trackedEntityAttribute[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[id]]],programStages[id,name,displayName,displayFormName,displayDescription,programStageDataElements[dataElement[id,valueType,code,displayName,displayShortName,displayFormName,displayDescription,optionSet[code]]]]",
        `programs:filter` = "code:eq:NEOIPC_CORE",
        `trackedEntityTypes:fields` = "id",
        `trackedEntityTypes:filter` = "name:eq:NeoIPC Patient",
        `organisationUnitGroups:fields` = "code,organisationUnits[id,code,displayName,displayShortName,displayDescription]",
        `organisationUnitGroups:filter` = "code:in:[COUNTRY,TEST_UNITS]",
        `organisationUnitGroupSets:fields` = "code,organisationUnitGroups[code,displayName,displayShortName,displayDescription,organisationUnits[id]]",
        `organisationUnitGroupSets:filter` = "code:in:[NEOIPC_TRIALS,WORLD_BANK_CLASSES]",
        `optionGroupSets:fields` = "code,optionGroups[code,displayName,displayShortName,displayDescription,options[code]]",
        `optionGroupSets:filter` = "code:in:[ATC5,WHO_AWARE]",
        `options:fields` = "code,displayName,displayFormName,displayDescription,sortOrder,optionSet[code]",
        `options:filter` = "optionSet.code:in:[NEOIPC_ASA_SCORE,NEOIPC_ADMISSION_TYPES,NEOIPC_ANTIMICROBIAL_SUBSTANCES,NEOIPC_BSI_DEVICE_ASS,NEOIPC_BSI_PATHOGEN_RECOVERED_FROM,NEOIPC_DELIVERY_MODES,NEOIPC_HAP_DEVICE_ASS,NEOIPC_HAP_RESPIRATORY_TRACT_SAMPLE_SOURCES,NEOIPC_SSI_TYPE,NEOIPC_SEX_VALUES,NEOIPC_SURVEILLANCE_END_REASON,NEOIPC_WOUND_CLASSES,NEOIPC_YES_NO_NO_FOLLOWUP,NEOIPC_YES_NO_NOT_TESTED]"),

    # We query the organisationUnits endpoint so that we can apply the
    # withinUserHierarchy filter
    md_req_base |>
      httr2::req_url_path_append("organisationUnits") |>
      httr2::req_url_query(
        withinUserHierarchy = "true",
        fields = "id,code,displayName,displayShortName,displayDescription,openingDate,comment,geometry,parent[id,code,displayName,displayShortName,displayDescription,comment,geometry,parent[code]]",
        filter = "organisationUnitGroups.code:eq:NEO_DEPARTMENT")
  )

  # We only read the complete user information via the metadata endpoint if we
  # have the required authorities to do so
  if (length(intersect(c("ALL","F_METADATA_EXPORT","F_USER_VIEW"), user_info$authorities)) > 0) {
    requests[[1]] <- requests[[1]] |>
      httr2::req_url_query(
        `users:fields` = "id,username,firstName,surname,email,created,lastLogin,organisationUnits[id],dataViewOrganisationUnits[id],teiSearchOrganisationUnits[id],userRoles[id]")
  }

  requests |>
    httr2::req_perform_parallel(on_error = "continue") |>
    read_metadata_reponses(user_info)
}

read_metadata_reponses <- function(resps, user_info)
{
  metadata <- resps |>
    lapply(read_metadata_reponse) |>
    unlist(recursive = FALSE)

  if (!("users" %in% names(metadata)))
    metadata$users <- read_user_info_table(user_info)

  metadata$hospitals <- metadata$hospitals |>
    dplyr::left_join(
      metadata$countries |>
        dplyr::select("code","country_key") |>
        dplyr::rename(country_code = .data$code),
      dplyr::join_by("country_code")) |>
    dplyr::select(!"country_code")

  metadata$departments <- metadata$departments |>
    dplyr::mutate(isTestUnit = .data$organisationUnit %in% metadata$testUnitIds) |>
    dplyr::left_join(
      metadata$trials |>
        dplyr::select("organisationUnits", "code") |>
        dplyr::rename(
          organisationUnit = .data$organisationUnits,
          name = .data$code) |>
        tidyr::unnest_longer(1) |>
        tidyr::unnest_wider(1),
      dplyr::join_by("organisationUnit" == "id")) |>
    dplyr::mutate(value = !is.na(.data$name)) |>
    tidyr::pivot_wider(values_fill = FALSE) |>
    dplyr::select(!tidyselect::any_of("NA"))

  metadata$testUnitIds <- NULL
  metadata$trials$organisationUnits <- NULL

  metadata
}

read_metadata_reponse <- function(resp)
{
  path <- httr2::resp_url_path(resp)
  json <- httr2::resp_body_json(resp)

  if(stringr::str_ends(path, "/metadata"))
      return(json |> read_metadata())
  else if(stringr::str_ends(path, "/organisationUnits"))
    return(json |> read_organisationUnits())

  rlang::abort("Unexpected DHIS2 metadata response.")
}

read_metadata <- function(metadata)
{
  system <- read_metadata_system(metadata)
  programId <- read_metadata_program_id(metadata)
  trackedEntityTypeId <- metadata$trackedEntityTypes |>
    unlist(use.names = FALSE)
  eventTypes <- read_metadata_programStages(metadata)
  dataElements <- read_metadata_dataElements(metadata)
  trackedEntityAttributes <- read_metadata_trackedEntityAttributes(metadata)
  users <- read_metadata_users(metadata)
  antimicrobialSubstances <- read_metadata_AntimicrobialSubstances(metadata)
  awareCategories <- read_metadata_AWaReCategories(metadata)
  atc5Categories <- read_metadata_atc5Categories(metadata)
  testUnitIds <- read_metadata_test_unit_ids(metadata)
  trials <- read_metadata_trials(metadata)
  world_bank_classes <- read_metadata_wb_classes(metadata) |>
    add_key_column('world_bank_class_key', as_factor = TRUE)

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
  countries <- read_metadata_countries(metadata) |>
    add_key_column('country_key', as_factor = TRUE)
  if(!rlang::is_null(countries) && !rlang::is_null(world_bank_classes))
    countries <- countries |>
      dplyr::left_join(
        world_bank_classes |>
          dplyr::select("world_bank_class_key", "organisationUnits") |>
          tidyr::unnest_longer("organisationUnits") |>
          tidyr::hoist("organisationUnits", organisationUnit = list(1L)),
        dplyr::join_by("organisationUnit"))

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
  if(!rlang::is_null(users))
    ret <- c(ret, list(users = users))

  ret
}

read_user_info_table <- function(user_info)
{
  user_info |>
      list() |>
      tibble::tibble() |>
      tidyr::unnest_wider(1) |>
      dplyr::select(
        !c(
          "organisationUnits",
          "dataViewOrganisationUnits",
          "teiSearchOrganisationUnits",
          "groups",
          "roles",
          "authorities")) |>
      add_key_column()
}

read_metadata_users <- function(metadata)
{
  users <- metadata |>
    purrr::pluck("users")

  if(rlang::is_null(users))
    return(invisible(NULL))

  users |>
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
    dplyr::relocate("user" = "id", "username", "firstName", "surname", "email", "lastLogin", "created") |>
    add_key_column("user_key")
}

read_organisationUnits <- function(organisationUnits)
{
  ret <- list()
  hospitals <- read_organisationUnits_hospitals(organisationUnits)
  if(!rlang::is_null(hospitals))
    ret <- c(ret, list(hospitals = hospitals) )

  departments <- read_organisationUnits_departments(organisationUnits, hospitals)
  if(!rlang::is_null(departments))
    ret <- c(ret, list(departments = departments) )

  ret
}

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
    dplyr::relocate("organisationUnit" = "id") |>
    add_key_column("hospital_key", as_factor = TRUE) |>
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
    dplyr::left_join(hospitals |> dplyr::select("organisationUnit", "hospital_key"),
                     dplyr::join_by("parent_id" == "organisationUnit")) |>
    dplyr::relocate("organisationUnit" = "id") |>
    add_key_column("department_key", as_factor = TRUE) |>
    dplyr::arrange(.data$parent_code, .data$openingDate) |>
    dplyr::select(!tidyselect::starts_with("parent")) |>
    dplyr::select(!tidyselect::any_of(c("hospital","geometry")))
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
    dplyr::relocate("programStage" = "id") |>
    add_key_column("event_type_key")
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
    dplyr::mutate(dplyr::across(!"id", ordered)) |>
    dplyr::relocate("id", .before = 1) |>
    dplyr::rename(organisationUnit = .data$id)
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
  groupSets <- purrr::pluck(metadata, "organisationUnitGroupSets")
  if(rlang::is_null(groupSets) || length(groupSets) == 0L)
    return(NULL)

  i <- NA_integer_
  for (k in seq_along(groupSets)) {
    if (isTRUE('NEOIPC_TRIALS' == purrr::pluck(groupSets, k, "code"))) {
      i <- k
      break
    }
  }
  if(is.na(i)) return(NULL)

  organisationUnitGroups <- purrr::pluck(groupSets, i, "organisationUnitGroups")

  if(rlang::is_null(organisationUnitGroups))
    return(NULL)

  organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)
}

read_metadata_wb_classes <- function(metadata)
{
  groupSets <- purrr::pluck(metadata, "organisationUnitGroupSets")
  if(rlang::is_null(groupSets) || length(groupSets) == 0L)
    return(NULL)

  i <- NA_integer_
  for (k in seq_along(groupSets)) {
    if (isTRUE('WORLD_BANK_CLASSES' == purrr::pluck(groupSets, k, "code"))) {
      i <- k
      break
    }
  }
  if(is.na(i)) return(NULL)

  organisationUnitGroups <- purrr::pluck(groupSets, i, "organisationUnitGroups")

  if(rlang::is_null(organisationUnitGroups))
    return(NULL)

  organisationUnitGroups <- organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(
      class = factor(stringr::str_extract(.data$code, "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$", group = 1), levels = c('L','LM','UM','H')),
      fiscal_year = as.integer(stringr::str_extract(.data$code, "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$", group = 2)),
      .keep = "unused",
      .before = 1) |>
    dplyr::arrange(dplyr::desc(.data$fiscal_year), .data$class)

  if (nrow(organisationUnitGroups) == 0L)
    return(organisationUnitGroups)

  current_year <- as.POSIXlt(Sys.Date())$year + 1900
  candidates <- organisationUnitGroups$fiscal_year[organisationUnitGroups$fiscal_year <= current_year]
  target_year <- if (length(candidates) > 0L) max(candidates) else max(organisationUnitGroups$fiscal_year, na.rm = TRUE)

  organisationUnitGroups |>
    dplyr::filter(.data$fiscal_year == target_year)
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
