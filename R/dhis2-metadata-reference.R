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
    dplyr::rename(country = "id") |>
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
