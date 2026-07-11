read_metadata_system <- function(metadata)
{
  system <- purrr::pluck(metadata, "system")
  if(rlang::is_null(system))
    rlang::abort("Invalid DHIS2 metadata. The system element is missing.",
                 "neoipcr_metadata_system_missing")

  version <- as.numeric_version(system$version)
  warn_if_unsupported_dhis2(version)

  list(id = uuid::as.UUID(system$id),
       version = version,
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

# Read the program-stages metadata into an `eventTypes` tibble.
#
# Returns a named list with two components:
#   * `public`       — schema-conformant tibble matching
#                      `compile_schema(eventTypes_cols, dataset_options)`.
#                      Always returned (never NULL); rows = all program
#                      stages present in the DHIS2 payload, mapped to
#                      the protocol's `event_type_key` factor.
#   * `internal_map` — orchestrator-internal tibble with two columns,
#                      `event_type_key + programStage`. Consumed by
#                      `read_events()` for the raw `programStage` →
#                      `event_type_key` substitution, regardless of
#                      whether `"event_types" %in% include_dhis2_ids`
#                      (that option controls only public exposure of
#                      `programStage`, not internal FK resolution).
read_metadata_programStages <- function(metadata, dataset_options)
{
  opts <- dataset_options

  programStages <- metadata |>
    purrr::pluck("programs", 1, "programStages")

  if(rlang::is_null(programStages))
    rlang::abort("Invalid DHIS2 metadata. The programStages list is missing.",
                 "neoipcr_metadata_programStages_missing")

  raw <- programStages |>
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
        dplyr::recode_values(
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
    dplyr::rename("programStage" = "id")

  internal_map <- raw |>
    dplyr::select(tidyselect::all_of(c("event_type_key", "programStage")))

  # `raw` already has exactly the columns declared in `eventTypes_cols`
  # (after the `programStageDataElements` select-out above). No scratch
  # needed — finalize is a pure selection / relocation under the
  # `include_dhis2_ids == "event_types"` gate.
  public <- raw |>
    finalize_to_schema(eventTypes_cols, opts)
  assert_schema(public, eventTypes_cols, opts)

  list(public = public, internal_map = internal_map)
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
  if(is.null(trial_keys))
    return(NULL)

  for (i in 1:2) {
    if ('NEOIPC_TRIALS' ==
        (purrr::pluck(metadata,"organisationUnitGroupSets", i, "code"))) break
  }
  organisationUnitGroups <- metadata |>
    purrr::pluck("organisationUnitGroupSets", i, "organisationUnitGroups")

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

#' @include schema-orgunits.R
NULL

# Read the World Bank income classes from the DHIS2 metadata.
#
# Returns a named list with two components:
#   * `public`      — schema-conformant tibble matching
#                     `compile_schema(worldBankClasses_cols, dataset_options)`.
#                     Always returned (never NULL); shape follows the
#                     three-mode contract on `include_world_bank_class`.
#   * `country_map` — internal lookup tibble with columns
#                     `world_bank_class_key` + `organisationUnits` (the
#                     nested list of country group members). Consumed by
#                     `read_metadata_countries()` to enrich each country
#                     with its WB-class membership. NULL when there is no
#                     WB-class metadata to map against (e.g.
#                     `include_world_bank_class == "no"`, or the WB-classes
#                     group set is absent from the response).
read_metadata_wb_classes <- function(metadata, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    public      = compile_schema(worldBankClasses_cols, opts),
    country_map = NULL
  )

  if (opts$include_world_bank_class == "no")
    return(empty_result)

  group_sets <- purrr::pluck(metadata, "organisationUnitGroupSets")
  if (rlang::is_null(group_sets) || length(group_sets) < 1L)
    return(empty_result)

  wb_set <- purrr::detect(
    group_sets,
    \(gs) identical(purrr::pluck(gs, "code"), "WORLD_BANK_CLASSES"))
  if (rlang::is_null(wb_set))
    return(empty_result)

  organisationUnitGroups <- purrr::pluck(wb_set, "organisationUnitGroups")
  if (rlang::is_null(organisationUnitGroups) ||
      length(organisationUnitGroups) < 1L)
    return(empty_result)

  organisationUnitGroups <- organisationUnitGroups |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(
      class = factor(
        stringr::str_extract(
          .data$code,
          "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$",
          group = 1),
        levels = c("L", "LM", "UM", "H")),
      fiscal_year = as.integer(
        stringr::str_extract(
          .data$code,
          "^WORLD_BANK_CLASS_(H|L|LM|UM)_FY_(\\d{4})$",
          group = 2)),
      .keep  = "unused",
      .before = 1) |>
    dplyr::arrange(dplyr::desc(.data$fiscal_year), .data$class)

  # Narrow to the most recent fiscal year that has WB-class rows. Current
  # year downward, stop at first non-empty year. If no row remains (e.g.
  # a metadata snapshot predating any fiscal year we know about), fall
  # back to the empty shape.
  current_year <- as.POSIXlt(Sys.Date())$year + 1900L
  filtered <- NULL
  for (year in current_year:2025L) {
    candidate <- organisationUnitGroups |>
      dplyr::filter(.data$fiscal_year == year)

    if (nrow(candidate) > 0L) {
      filtered <- candidate
      break
    }
  }
  if (is.null(filtered))
    return(empty_result)

  filtered <- filtered |>
    add_key_column("world_bank_class_key")

  country_map <- filtered |>
    dplyr::select(tidyselect::all_of(
      c("world_bank_class_key", "organisationUnits")))

  # `id`, `name`, `displayName`, `organisationUnits` are raw DHIS2 fields
  # unnest_wider'd from the WB-class group response. The orchestrator
  # consumes `organisationUnits` (via `country_map`) for the WB-class
  # inheritance path on hospitals under `include_country = "no"`; the
  # rest are unused public-side. Listed as `scratch` so the new loud
  # finalize doesn't flag them as unintended mismatches.
  public <- filtered |>
    finalize_to_schema(
      worldBankClasses_cols, opts,
      scratch = c("id", "name", "displayName", "displayShortName",
                   "displayDescription", "organisationUnits"))
  assert_schema(public, worldBankClasses_cols, opts)

  list(public = public, country_map = country_map)
}

# Read the country metadata from the DHIS2 metadata response.
#
# Returns a named list with two components:
#   * `public`       — schema-conformant tibble matching
#                      `compile_schema(countries_cols, dataset_options)`.
#                      Always returned (never NULL); shape follows the
#                      three-mode contract on `include_country` (0×0 / 1-col
#                      / full) plus the direct WB-class link-FK when
#                      `include_world_bank_class != "no"`.
#   * `internal_map` — orchestrator-internal lookup tibble with columns
#                      `country` (raw DHIS2 id), `code`, and
#                      `country_key`. Consumed by the orchestrator's
#                      post-read joins (country_filter narrowing,
#                      hospitals country_key lookup). NULL when there is
#                      no country metadata to map against (e.g. no
#                      COUNTRY organisationUnitGroup, or the user opted
#                      out of everything country-related).
read_metadata_countries <- function(metadata, dataset_options, wb_country_map)
{
  opts <- dataset_options
  empty_result <- list(
    public       = compile_schema(countries_cols, opts),
    internal_map = NULL
  )

  has_country_filter <- length(opts$country_filter) > 0L

  # Early-exit when the user wants nothing country-related and no WB
  # join is needed. Retains legacy behaviour of returning the empty
  # shape without attempting to read the COUNTRY group.
  if (!has_country_filter &&
      opts$include_country == "no" &&
      opts$include_world_bank_class == "no")
    return(empty_result)

  organisationUnitGroups <- read_metadata_organisationUnitGroups(
    metadata, "COUNTRY")

  if (rlang::is_null(organisationUnitGroups) ||
      nrow(organisationUnitGroups) < 1L)
    return(empty_result)

  countries <- organisationUnitGroups |>
    dplyr::select("organisationUnits") |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(dplyr::across(!c("id", "name"), ordered)) |>
    dplyr::relocate("id", .before = 1) |>
    dplyr::rename(country = "id") |>
    add_key_column("country_key")

  if (opts$include_world_bank_class != "no" && !is.null(wb_country_map))
    countries <- countries |>
    dplyr::left_join(
      wb_country_map |>
        tidyr::unnest_longer("organisationUnits") |>
        tidyr::hoist("organisationUnits", country = list(1L)),
      dplyr::join_by("country"))

  # The schema declares `world_bank_class_key` under any non-"no" WB
  # option — this is a static schema-level fact, independent of whether
  # actual WB-class metadata was present in the response. If the WB
  # metadata was empty (wb_country_map NULL or no matching rows),
  # materialize the column as NA so finalize_to_schema can select it.
  if (opts$include_world_bank_class != "no" &&
      !("world_bank_class_key" %in% names(countries)))
    countries$world_bank_class_key <- NA_integer_

  # Orchestrator-internal lookup — carries the raw DHIS2 `country` id
  # and the `code` needed for `country_filter` narrowing plus
  # `country_key` for joining back to the public tibble. Kept outside
  # the public schema because DHIS2 id exposure is gated separately
  # (`include_dhis2_ids == "countries"`), and `code` is internal to
  # filtering, not to the public output.
  internal_map <- countries |>
    dplyr::select(tidyselect::any_of(c("country", "code", "country_key")))

  # `country` is the raw DHIS2 org-unit id; kept only on `internal_map`
  # for orchestrator-side joins (country_filter narrowing, hospitals
  # country_key lookup). The schema deliberately doesn't expose it in
  # the public tibble (that's gated by `include_dhis2_ids == "countries"`
  # at a future Phase B sub-task), so it's `scratch` for the finalize.
  public <- countries |>
    finalize_to_schema(countries_cols, opts, scratch = "country")
  assert_schema(public, countries_cols, opts)

  list(public = public, internal_map = internal_map)
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
      displayFormName = factor(
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
