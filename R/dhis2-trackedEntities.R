get_trackedEntities_request <- function(
    req_base, dataset_options, programId, trackedEntityTypeId)
{
  fields <- "trackedEntity,inactive,potentialDuplicate"
  attributeFields <- "attribute,value"

  if(dataset_options$include_unenrolled_patients)
    req_base <- req_base |>
    httr2::req_url_query(trackedEntityType = trackedEntityTypeId)
  else
  {
    req_base <- req_base |>
    httr2::req_url_query(program = programId)

    if(!("enrollments" %in% dataset_options$include_incomplete))
      req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")
  }

  if(dataset_options$include_timestamps)
  {
    fields <- paste0(fields,",createdAt,createdAtClient,updatedAt,updatedAtClient")
    attributeFields <- paste0(attributeFields,",createdAt,updatedAt")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0 ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
  {
    # Entity-level `storedBy` added in phase-b-event-details (closing
    # the latent-drop symmetric with enrollments + events). Previously
    # only per-attribute `storedBy` was requested; the entity itself
    # also carries one and it's now in `patients_cols`.
    fields <- paste0(
      fields,
      ",storedBy,createdBy[username],updatedBy[username]")
    attributeFields <- paste0(attributeFields,",storedBy")
  }

  fields <- paste0(fields, ",attributes[", attributeFields, "]")

  req_base |>
    httr2::req_url_path_append("trackedEntities") |>
    httr2::req_url_query(fields = fields)
}

read_patients <- function(trackedEntities, metadata, dataset_options)
{
  opts <- dataset_options

  # Entity gate short-circuit: under `include_patient = "no"` the
  # public patients tibble is 0×0. `assert_schema` + `finalize_to_schema`
  # short-circuit too, but returning early here avoids running the
  # whole unnest / join / pivot pipeline on a dataset the caller has
  # explicitly opted out of.
  if (opts$include_patient == "no")
    return(list(
      public       = compile_schema(patients_cols, opts),
      internal_map = tibble::tibble(
        patient_key    = integer(),
        trackedEntity  = character())))

  # Empty-input guard: DHIS2 returned no tracked entities. Produce a
  # valid empty dataset rather than crashing on missing columns.
  if (nrow(trackedEntities) == 0L)
    return(list(
      public       = compile_schema(patients_cols, opts),
      internal_map = tibble::tibble(
        patient_key    = integer(),
        trackedEntity  = character())))

  patients <- trackedEntities |>
    tidyr::unnest_longer("attributes") |>
    tidyr::unnest_wider("attributes", names_sep = "_") |>
    dplyr::inner_join(
      metadata$trackedEntityAttributes |>
        dplyr::select("attribute","code","valueType","optionSet"),
      dplyr::join_by("attributes_attribute" == "attribute")) |>
    dplyr::left_join(
      metadata$options |>
        dplyr::arrange(.data$optionSet_code, .data$sortOrder) |>
        dplyr::group_by(.data$optionSet_code) |>
        dplyr::summarise(levels = list(.data$code)),
      dplyr::join_by("optionSet" == "optionSet_code")) |>
    dplyr::select(!c("attributes_attribute", "optionSet"))

  # Filter attributes to the user-selected subset. Every attribute in
  # `patient_columns` is a code stem (e.g. "sex", "birth_weight"); the
  # DHIS2 TEA codes are `NEOIPC_[TEA_]<UPPER>`. The "id" → "patient_id"
  # mapping is the legacy naming preserved in the schema; everything
  # else is a 1:1 stem match against the lowercased/stripped code.
  allowed_codes <- opts$patient_columns
  if ("id" %in% allowed_codes)
    allowed_codes <- c(allowed_codes, "patient_id")
  # `gest_age` pulls its paired `total_gestation_days` — per the schema
  # note, both TEAs carry the same datum in two shapes (text vs
  # integer) and stay in sync via DHIS2 program rules.
  # `gestational_age` is the user-facing patient_columns key; the
  # DHIS2-derived column codes are `gest_age` (text "25+4") and
  # `total_gestation_days` (integer total days). Both must be in
  # allowed_codes so the pre-pivot factor pinning picks them up.
  if ("gestational_age" %in% allowed_codes)
    allowed_codes <- c(allowed_codes, "gest_age", "total_gestation_days")
  # `include_invalid_patients` can be a character vector of patient IDs
  # (exceptions to the invalid filter in `import_dhis2()`). When it is,
  # `patient_id` must remain accessible regardless of `patient_columns`
  # so the downstream filter can match IDs. Preserves pre-schema
  # behaviour of the patient reader.
  if (length(opts$include_invalid_patients) > 1)
    allowed_codes <- c(allowed_codes, "patient_id")
  # Match against the normalized code (lowercase, NEOIPC_[TEA_]
  # prefix stripped) — same extraction that will run below.
  normalized_code <- stringr::str_extract(
    tolower(patients$code), "^neoipc_(tea_)?(.+)$", group = 2)
  patients <- patients |>
    dplyr::filter(normalized_code %in% allowed_codes)

  if(dataset_options$include_timestamps)
    patients <- patients |>
      dplyr::mutate(dplyr::across(tidyselect::contains("At", ignore.case = FALSE), readr::parse_datetime))
  else
    patients <- patients |>
      dplyr::select(!tidyselect::contains("At", ignore.case = FALSE))

  if(dataset_options$include_user != "no")
    patients <- patients |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("storedBy" == "username")) |>
      dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

  if(dataset_options$include_user != "no" &&
     "attributes_storedBy" %in% names(patients))
    patients <- patients |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("attributes_storedBy" == "username")) |>
      dplyr::mutate(attributes_storedBy = .data$user_key, .keep = "unused")
  # `metadata$users` → `metadata$.users_internal_map` on every lookup above.
  # `metadata$users` carries the public three-mode shape (0×0 / 1-col
  # `user_key` / full) declared by `users_cols`; pseudo mode intentionally
  # drops `username`, so FK substitution here must read the internal map
  # carrying `user_key + username + user` regardless of the public mode.
  # See `R/schema-orgunits.R::users_cols` and
  # `R/dhis2-users.R::read_metadata_users()`.

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
    patients <- patients |>
      dplyr::semi_join(
        metadata$.departments_internal_map, dplyr::join_by("orgUnit"))

  # Pre-pivot factor-level pinning on `code`: guarantees one column per
  # expected TEA stem regardless of whether the input data carried
  # rows for every attribute. Paired with `pivot_wider(..., names_expand =
  # TRUE)` below, this closes the pivot-volatility hazard where a
  # filter-induced absence of one attribute's rows would silently drop
  # its columns from the pivot output.
  expected_codes <- c(
    "patient_id", "sex", "birth_weight", "gest_age",
    "total_gestation_days", "delivery_mode", "siblings")
  expected_codes <- intersect(expected_codes, allowed_codes)

  patients <- patients |>
    dplyr::mutate(
      attributes_value = convert_value(
        .data$attributes_value, .data$valueType, .data$levels),
      code = factor(
        stringr::str_extract(
          tolower(.data$code), "^neoipc_(tea_)?(.+)$", group = 2),
        levels = expected_codes),
      .keep = "unused"
    ) |>
    tidyr::pivot_wider(
      names_from = "code",
      values_from = tidyselect::starts_with("attributes_"),
      names_glue = "{code}_{.value}",
      names_vary = "slowest",
      names_expand = TRUE) |>
    tidyr::unnest_longer(dplyr::ends_with("value"), keep_empty = TRUE) |>
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_attributes"),
      tidyselect::contains("_attributes_")) |>
    dplyr::rename_with(
      ~ stringr::str_remove(.x, "_value$"),
      tidyselect::ends_with("_value")) |>
    add_key_column("patient_key")

  if(dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
    # Select only the hierarchy columns the schema declares under
    # the current opts, plus orgUnit for the join key.
    hierarchy_cols <- intersect(
      c("department_key", "hospital_key", "country_key",
        "world_bank_class_key", "isTest"),
      names(compile_schema(patients_cols, opts)))
    patients <- patients |>
      dplyr::left_join(
        metadata$.departments_internal_map |>
          dplyr::select(tidyselect::all_of(c("orgUnit", hierarchy_cols))),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!"orgUnit")
  }

  # Apply eligibility and range filters early so downstream joins operate on
  # fewer rows
  patients <- patients |>
    filter_patients(
      dataset_options$birth_weight_from,
      dataset_options$birth_weight_to,
      dataset_options$gestational_age_from,
      dataset_options$gestational_age_to,
      dataset_options$include_ineligible_patients)

  # Narrow to the public schema + loud-assert. `finalize_to_schema`
  # drops columns that the reader intentionally carries as scratch
  # (e.g. the raw `orgUnit` id consumed by the departments-join
  # block above is already `select(!"orgUnit")`-dropped, so no
  # scratch declaration is needed here).
  # Internal map: carries `patient_key + trackedEntity` for
  # downstream readers (read_enrollments) that need to substitute
  # the raw DHIS2 TE uid with the integer key. Built before finalize
  # so `trackedEntity` is always available regardless of schema gates.
  internal_map <- patients |>
    dplyr::select("patient_key", "trackedEntity")

  patients <- patients |>
    finalize_to_schema(patients_cols, opts)
  assert_schema(patients, patients_cols, opts)

  list(public = patients, internal_map = internal_map)
}
