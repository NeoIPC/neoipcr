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

  resps <- list(
    get_metadata_request(md_req_base, user_info, dataset_options),
    get_organisationUnit_request(md_req_base, user_info, dataset_options)) |>
    httr2::req_perform_parallel(on_error = "continue")

  endpoints <- c("metadata", "organisationUnits")
  purrr::iwalk(resps, \(resp, i) log_dhis2_request(resp, endpoints[[i]]))

  read_metadata_reponses(resps, user_info, dataset_options)
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
      if(dataset_options$include_world_bank_class == "full")
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
    if(dataset_options$include_country == "full")
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,name,code,displayName,displayShortName,displayDescription]")
    else if(length(dataset_options$country_filter) > 0)
      req <- req |>
        httr2::req_url_query(
          `organisationUnitGroups:fields` = "code,organisationUnits[id,code]")
    else # include_country == "pseudo", or include_world_bank_class != "no"
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
    if(dataset_options$include_user == "full")
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

read_metadata_reponses <- function(resps, user_info, dataset_options)
{
  metadata <- resps |>
    lapply(read_metadata_reponse, dataset_options) |>
    unlist(recursive = FALSE)

  # `metadata$.countries_internal_map` is the orchestrator-internal
  # countries lookup — it carries the raw DHIS2 `country` id + `code` +
  # `country_key` used by every post-read country/hospital/department
  # join that can't reach into `metadata$countries` (which is the
  # schema-conformant public tibble without the raw id or `code`). The
  # field is kept on `metadata` through the rest of `import_dhis2()` so
  # that request-building code (e.g. country_filter → event request
  # URLs) can consume it, and stripped at the `import_dhis2()` exit
  # just before the final dataset is assembled.

  # Users are read via one of two paths depending on caller authorities:
  # the full metadata endpoint (earlier in `read_metadata_reponse()`) or
  # the fallback `/me` single-row path below. Either way, the reader
  # returns `list(public, internal_map)` — the public tibble matches
  # `users_cols`, the internal map carries `user_key + username + user`
  # for fact-reader FK substitution. `.users_internal_map` is threaded
  # through the orchestrator and stripped at `import_dhis2()` exit.
  if (!is.list(metadata$.users_result)) {
    metadata$.users_result <- read_user_info_table(
      user_info, dataset_options)
  }

  metadata$users                <- metadata$.users_result$public
  metadata$.users_internal_map  <- metadata$.users_result$internal_map
  metadata$.users_result        <- NULL

  assert_schema(metadata$users, users_cols, dataset_options)

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
  if (length(dataset_options$country_filter) > 0 &&
      !is.null(metadata$.countries_internal_map))
  {
    surviving_keys <- metadata$.countries_internal_map |>
      dplyr::filter(.data$code %in% dataset_options$country_filter) |>
      dplyr::pull("country_key")

    # Narrow the public tibble only when it actually carries the key
    # (i.e. include_country != "no"). The map is always narrowed so
    # downstream joins see the filtered country set.
    if ("country_key" %in% names(metadata$countries))
      metadata$countries <- metadata$countries |>
        dplyr::filter(.data$country_key %in% surviving_keys)

    metadata$.countries_internal_map <- metadata$.countries_internal_map |>
      dplyr::filter(.data$country_key %in% surviving_keys)

    if (!is.null(metadata$hospitals) &&
        "hospital_key" %in% names(metadata$departments))
    {
      filtered_hospital_keys <- metadata$hospitals |>
        dplyr::semi_join(
          metadata$.countries_internal_map, dplyr::join_by("country")) |>
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

  # Join country_key into hospitals via the raw `country` id held by the
  # orchestrator-internal map. The public countries tibble no longer
  # carries `country` under the schema contract, so the join has to
  # consume the map. `metadata$hospitals` is still the reader's
  # `processed` tibble at this point (narrowed to the public schema
  # below, after all joins run).
  if ((dataset_options$include_country != "no" ||
       length(dataset_options$country_filter) > 0 ||
       dataset_options$include_world_bank_class != "no") &&
      !is.null(metadata$.countries_internal_map) &&
      "country" %in% names(metadata$hospitals))
  {
    metadata$hospitals <- metadata$hospitals |>
      dplyr::left_join(
        metadata$.countries_internal_map |>
          dplyr::select("country", "country_key"),
        dplyr::join_by("country")) |>
      dplyr::select(!"country")
  }

  # Hospitals WB-class inheritance: under `include_country = "no"` +
  # `include_world_bank_class != "no"`, countries is 0×0 so it can't
  # relay `world_bank_class_key`. The inheritance rule makes hospitals
  # carry the key directly; populate it from the raw WB-class →
  # country-id membership map held in `.wb_country_map`, joined through
  # the hospitals internal map's `country` column.
  if (dataset_options$include_world_bank_class != "no" &&
      dataset_options$include_country == "no" &&
      !is.null(metadata$.wb_country_map) &&
      !is.null(metadata$.hospitals_internal_map) &&
      "country" %in% names(metadata$.hospitals_internal_map))
  {
    wb_country_lookup <- metadata$.wb_country_map |>
      tidyr::unnest_longer("organisationUnits") |>
      tidyr::hoist("organisationUnits", country = list(1L)) |>
      dplyr::select("country", "world_bank_class_key")

    wb_hospital_lookup <- metadata$.hospitals_internal_map |>
      dplyr::left_join(wb_country_lookup, dplyr::join_by("country")) |>
      dplyr::select("hospital_key", "world_bank_class_key")

    metadata$hospitals <- metadata$hospitals |>
      dplyr::left_join(wb_hospital_lookup, dplyr::join_by("hospital_key"))
  }

  # Narrow `metadata$hospitals` from the reader's `processed` tibble to
  # the public three-mode shape declared by `hospitals_cols`. The
  # containing-entity gate on `hospitals_cols` short-circuits to 0×0
  # under `include_hospital = "no"`, dropping every internal-only
  # column in one step. Tail `assert_schema()` confirms the result.
  #
  # `country` (raw DHIS2 id from the hoisted parent reference) is
  # reader-internal scratch: consumed by the orchestrator above for the
  # country_key and WB-class inheritance joins, and explicitly not part
  # of the public schema. Declared as `scratch` so the new loud
  # finalize recognises it as intentional, not an unintended mismatch.
  metadata$hospitals <- metadata$hospitals |>
    finalize_to_schema(hospitals_cols, dataset_options, scratch = "country")
  assert_schema(metadata$hospitals, hospitals_cols, dataset_options)

  # Pre-join hierarchy into departments so that the internal map carries
  # the full chain (department → hospital → country → WB class).
  # `read_organisationUnits_departments` already joined hospital_key
  # from .hospitals_internal_map (even under include_hospital = "no"),
  # so hospital_key is available here pre-finalize. The relay walks
  # hospital → country → WB class via the internal hospital map (which
  # carries the raw `country` id) and the countries tibble.
  if ("hospital_key" %in% names(metadata$departments) &&
      !is.null(metadata$.hospitals_internal_map) &&
      "country" %in% names(metadata$.hospitals_internal_map))
  {
    # Build hospital_key → country_key relay via the raw country id.
    # metadata$countries is already finalized (scratch "country"
    # stripped), so the raw DHIS2 id lives on .countries_internal_map.
    if (!is.null(metadata$.countries_internal_map) &&
        "country_key" %in% names(metadata$.countries_internal_map)) {
      metadata$departments <- metadata$departments |>
        dplyr::left_join(
          metadata$.hospitals_internal_map |>
            dplyr::select("hospital_key", "country") |>
            dplyr::inner_join(
              metadata$.countries_internal_map |>
                dplyr::select("country_key", "country"),
              dplyr::join_by("country")) |>
            dplyr::select("hospital_key", "country_key"),
          dplyr::join_by("hospital_key"))
    }

    if ("world_bank_class_key" %in% names(metadata$countries))
      metadata$departments <- metadata$departments |>
        dplyr::left_join(
          metadata$countries |>
            dplyr::select("country_key", "world_bank_class_key"),
          dplyr::join_by("country_key"))
  }

  # Snapshot the full hierarchy lookup before finalize_to_schema strips
  # columns gated by the public schema. After the pre-join above,
  # departments carries the complete hierarchy chain regardless of
  # include_department mode. Unlike the other internal maps (which are
  # simple UID-to-key bridges), this one is the single hierarchy
  # lookup that every fact-entity reader joins on orgUnit to populate
  # all hierarchy keys consistently.
  metadata$.departments_internal_map <- metadata$departments |>
    dplyr::select("department_key", "orgUnit",
                  tidyselect::any_of(c(
                    "code", "hospital_key", "country_key",
                    "world_bank_class_key", "isTest")))

  metadata$departments <- metadata$departments |>
    finalize_to_schema(departments_cols, dataset_options)
  assert_schema(metadata$departments, departments_cols, dataset_options)

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
  # `read_metadata_programStages()` now returns `list(public,
  # internal_map)`. `internal_map` is consumed by `read_events()` for
  # the raw programStage → event_type_key substitution regardless of
  # whether `"event_types"` is in include_dhis2_ids; `public` follows
  # the schema contract (always present; `programStage` only exposed
  # when id-opt-in fires).
  eventTypes_result <- read_metadata_programStages(metadata, dataset_options)
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

  # `read_metadata_users()` now returns `list(public, internal_map)`.
  # Under `include_user = "no"` or when the response lacks a `users`
  # payload, `internal_map` is NULL and the caller falls back to
  # `read_user_info_table()` in `read_metadata_reponses()`.
  users_result <- read_metadata_users(metadata, dataset_options)

  trials <- read_metadata_trials(
    metadata,
    dataset_options$trial_keys)

  wb_result <- read_metadata_wb_classes(metadata, dataset_options)
  world_bank_classes <- wb_result$public
  wb_country_map     <- wb_result$country_map

  countries_result <- read_metadata_countries(
    metadata, dataset_options, wb_country_map)
  countries              <- countries_result$public
  countries_internal_map <- countries_result$internal_map

  ret <- list(
    system = system,
    programId = programId,
    trackedEntityTypeId = trackedEntityTypeId,
    # Public `eventTypes` tibble — three-column subset of `eventTypes_cols`,
    # `programStage` gated on `"event_types" %in% include_dhis2_ids`.
    eventTypes = eventTypes_result$public,
    # Orchestrator-internal two-column map consumed by `read_events()`
    # for the raw programStage → event_type_key substitution during
    # fact-table processing. Stripped at `import_dhis2()` exit.
    .eventTypes_internal_map = eventTypes_result$internal_map,
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

  # Thread users through only when the metadata endpoint actually
  # supplied a payload (`internal_map != NULL`). Otherwise leave the
  # orchestrator's fallback path (`read_user_info_table`) to fill in
  # from the `/me` response. `.users_result` carries the whole
  # `list(public, internal_map)` up to `read_metadata_reponses()`
  # where it is unpacked into `metadata$users` and
  # `metadata$.users_internal_map`.
  if (!is.null(users_result$internal_map))
    ret <- c(ret, list(.users_result = users_result))
  if(!is.null(trials))
    ret <- c(ret, list(trials = trials))
  # `world_bank_classes` is always a tibble (never NULL) — the three-mode
  # shape is the signal: 0×0 under "no", 1-col under "pseudo", full schema
  # under "full". See `R/schema-orgunits.R::worldBankClasses_cols`.
  ret <- c(ret, list(worldBankClasses = world_bank_classes))
  # `countries` follows the same three-mode contract via
  # `R/schema-orgunits.R::countries_cols`. The orchestrator-internal
  # `countries_internal_map` (with raw DHIS2 `country` id + `code` used
  # by post-read joins and `country_filter`) is threaded through via
  # `.countries_internal_map` and stripped at the top of the multi-response
  # orchestrator's post-processing.
  ret <- c(ret, list(countries = countries))
  if (!is.null(countries_internal_map))
    ret <- c(ret, list(.countries_internal_map = countries_internal_map))
  # `.wb_country_map` is the raw WB-class → country-id membership
  # lookup. Threaded through so `read_metadata_reponses()` can populate
  # `world_bank_class_key` on hospitals under the inheritance case
  # (`include_country = "no"` + `include_world_bank_class != "no"`)
  # where the countries tibble is empty and can't serve as a join
  # relay. Stripped at `import_dhis2()` exit.
  if (!is.null(wb_country_map))
    ret <- c(ret, list(.wb_country_map = wb_country_map))

  ret
}
