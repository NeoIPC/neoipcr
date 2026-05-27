get_events_request <- function(req_base, dataset_options, programId)
{
  fields <- "event,programStage,enrollment,trackedEntity,occurredAt,followup"
  dataValueFields <- "dataElement,value"

  if("events" %in% dataset_options$include_incomplete)
    fields <- paste0(fields,",status")
  else
    req_base <- req_base |>
    httr2::req_url_query(status = "COMPLETED")

  if(!("enrollments" %in% dataset_options$include_incomplete))
    req_base <- req_base |>
    httr2::req_url_query(programStatus = "COMPLETED")

  if(dataset_options$include_timestamps)
  {
    fields <- paste0(
      fields,
      ",scheduledAt,completedAt,createdAt,createdAtClient,updatedAt,updatedAtClient")

    dataValueFields <- paste0(dataValueFields,",createdAt,updatedAt")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     !is.null(dataset_options$trial_keys) ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields,",orgUnit")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if("events" %in% dataset_options$include_notes)
    fields <- paste0(fields,",notes")

  if(dataset_options$include_user != "no")
  {
    # `completedBy` is a plain String in DHIS2 (Event.java) — no
    # `[username]` subselect needed. Added in phase-b-event-details
    # alongside the entity-level user-field merger from the old
    # `eventDetails` sidecar.
    fields <- paste0(
      fields,
      ",storedBy,createdBy[username],updatedBy[username],completedBy")
    # Per-DE companions extended in phase-b-event-details to cover all
    # five DataValue.java audit fields (was only `createdBy[username]`).
    dataValueFields <- paste0(
      dataValueFields,
      ",storedBy,createdBy[username],updatedBy[username]")
  }

  fields <- paste0(fields,",dataValues[", dataValueFields, "]")

  req_base |>
    httr2::req_url_path_append("events") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_events <- function(events, enrollments, metadata, dataset_options)
{
  opts <- dataset_options

  .empty_result <- function()
    list(
      public       = compile_schema(events_cols, opts),
      internal_map = tibble::tibble(
        event_key      = integer(),
        event          = character(),
        event_type_key = factor(
          character(),
          levels = c("adm","pro","bsi","nec","ssi","hap","end"))))

  if (opts$include_event == "no")
    return(.empty_result())

  if (nrow(events) == 0L)
    return(.empty_result())

  if (opts$include_enrollment == "no" || opts$include_patient == "no")
    return(.empty_result())

  # Raw `programStage` → `event_type_key` + raw `enrollment` →
  # `enrollment_key` + `patient_key` substitution via internal maps.
  # `.enrollments_internal_map` carries enrollment_key + enrollment +
  # patient_key (patient_key derived from the enrollment→patient chain
  # inside read_enrollments, not from trackedEntity on the raw events).
  events <- events |>
    dplyr::inner_join(
      metadata$.eventTypes_internal_map,
      dplyr::join_by("programStage")) |>
    dplyr::inner_join(
      metadata$.enrollments_internal_map,
      dplyr::join_by("enrollment")) |>
    dplyr::mutate(
      occurredAt = readr::parse_date(
        stringr::str_sub(.data$occurredAt, end = 10)))

  # Hierarchy-key fat-lookup: pull each key that events_cols declares
  # under the current opts directly off `metadata$departments`. Under
  # the three-mode schema contract, departments carries exactly the
  # hierarchy keys whose `include_*` option is non-"no" (when
  # `include_department == "full"`), so the column list is a pure
  # function of opts — no legacy branching needed.
  hierarchy_keys <- c("department_key", "hospital_key",
                      "country_key", "world_bank_class_key")
  expected <- names(compile_schema(events_cols, opts))
  needed   <- intersect(hierarchy_keys, expected)

  if (length(needed) > 0L || opts$include_test_data) {
    hierarchy_cols <- intersect(
      c("department_key", "hospital_key", "country_key",
        "world_bank_class_key", "isTest"),
      names(compile_schema(events_cols, opts)))
    events <- events |>
      dplyr::left_join(
        metadata$.departments_internal_map |>
          dplyr::select(tidyselect::all_of(c("orgUnit", hierarchy_cols))),
        dplyr::join_by("orgUnit"))
  }

  if ("events" %in% opts$include_incomplete)
    events <- events |>
      dplyr::mutate(
        status = factor(
          .data$status,
          levels = c(
            "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))
      )

  if (!opts$include_test_data ||
      length(opts$country_filter) > 0 ||
      !is.null(opts$trial_keys))
    events <- events |>
      dplyr::semi_join(
        metadata$.departments_internal_map, dplyr::join_by("orgUnit"))

  # Entity-level user-field substitution — folded in from the former
  # `read_event_details()` ahead of the schema finalize. Each raw field
  # arrives with a different JSON shape:
  #   - `storedBy` / `completedBy`: plain String — username directly.
  #   - `createdBy` / `updatedBy`:   User object from `createdBy[username]`
  #                                  subselect; the first element of the
  #                                  hoisted list is the username.
  # All four get substituted to integer `user_key` via the orchestrator-
  # internal `.users_internal_map`'s `username` column (different from
  # notes' `createdBy` join, which uses the UID-bearing `user` column —
  # see the reader comment in `read_event_notes()` and `docs/dhis2-user-
  # timestamp-semantics.md` for the DHIS2-source rationale).
  if (opts$include_user != "no") {
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused") |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user_key", "username"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

    if ("storedBy" %in% names(events))
      events <- events |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

    if ("completedBy" %in% names(events))
      events <- events |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("completedBy" == "username")) |>
        dplyr::mutate(completedBy = .data$user_key, .keep = "unused")
  }

  # Entity-level timestamp parsing — six ISO-8601 Instants from the API
  # (scheduledAt / completedAt / createdAt / createdAtClient / updatedAt
  # / updatedAtClient) parsed to POSIXct. Gated by `include_timestamps`,
  # which is also what gates their presence in the request (see
  # `get_events_request()` line 16-23). The legacy `read_event_details()`
  # nested this parse inside the `include_user != "no"` branch — under
  # `include_timestamps = TRUE` + `include_user = "no"` the columns
  # survived unparsed as strings (latent bug); the merged reader parses
  # them unconditionally under the schema's gate.
  if (isTRUE(opts$include_timestamps))
    events <- events |>
      dplyr::mutate(dplyr::across(dplyr::ends_with("At"), readr::parse_datetime))

  # `orgUnit`, `programStage`, `trackedEntity`, `enrollment` are
  # reader-internal scratch — fetched for the joins / filter above and
  # either substituted (programStage → event_type_key, trackedEntity
  # → patient_key, enrollment → enrollment_key) or used only for the
  # departments lookup (orgUnit). `isTest` is declared on events_cols
  # and survives the finalize. `event` stays if the id opt-in is set;
  # the schema gates it. `dataValues` / `notes` are raw DHIS2 payloads
  # consumed downstream by `read_event_data()` / `read_event_notes()`
  # (via the separate `processed_events` argument) — they live on the
  # raw response but not on the public events tibble.
  events <- events |>
    add_key_column("event_key")

  internal_map <- events |>
    dplyr::select("event_key", "event", "event_type_key")

  events <- events |>
    finalize_to_schema(
      events_cols, opts,
      scratch = c(
        "orgUnit", "programStage", "trackedEntity", "enrollment",
        "dataValues", "notes"))

  assert_schema(events, events_cols, opts)

  list(public = events, internal_map = internal_map)
}

read_event_notes <- function(events, processed_events, metadata, dataset_options)
{
  opts <- dataset_options

  # Entity gate short-circuit. Under the schema contract the reader
  # ALWAYS returns a schema-shaped tibble (never NULL) — callers
  # guard on column presence, not null-ness.
  if (!entity_exists(event_notes_cols, opts))
    return(compile_schema(event_notes_cols, opts))

  if (nrow(events) == 0L)
    return(compile_schema(event_notes_cols, opts))

  events <- events |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event_key", "event"),
      dplyr::join_by("event")) |>
    dplyr::select("event_key", "notes") |>
    dplyr::filter(!is.na(.data$notes) & lengths(.data$notes) > 0)

  if (nrow(events) == 0) {
    public <- compile_schema(event_notes_cols, opts)
    assert_schema(public, event_notes_cols, opts)
    return(public)
  }

  events <- events |>
    tidyr::unnest_longer("notes") |>
    tidyr::hoist("notes",
      note = "note",
      value = "value",
      storedBy = "storedBy",
      storedAt = "storedAt",
      createdBy = "createdBy",
      .remove = TRUE)

  if (nrow(events) == 0) {
    public <- compile_schema(event_notes_cols, opts)
    assert_schema(public, event_notes_cols, opts)
    return(public)
  }

  if (opts$include_user != "no") {
    # Note: `createdBy` here is a DHIS2 user UID (Note.java emits the
    # User object as `{id: <UID>, ...}`), so we join on the `user`
    # column of the internal map — NOT `username` (the latter is how
    # event / event-data createdBy joins work because those API
    # endpoints request `createdBy[username]`). See phase-b-notes
    # Divergences for the DHIS2-source-confirmed rationale.
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user", "user_key"),
        dplyr::join_by("createdBy" == "user")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused")

    if ("storedBy" %in% names(events))
      events <- events |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("username", "user_key"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  if (opts$include_timestamps && "storedAt" %in% names(events))
    events <- events |>
      dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt))

  public <- events |>
    finalize_to_schema(event_notes_cols, opts)
  assert_schema(public, event_notes_cols, opts)

  public
}

read_event_data <- function(events, processed_events, metadata, dataset_options, event_type_key)
{
  opts <- dataset_options
  cols <- event_data_cols_for(event_type_key)

  # Entity gate short-circuit: under `include_event = "no"` every
  # per-event-type tibble is 0×0.
  if (!entity_exists(cols, opts))
    return(compile_schema(cols, opts))

  # Empty-input guard: when DHIS2 returns no events, parse_resp
  # produces a 0-col tibble — the select/unnest chain below would
  # crash on missing columns.
  if (nrow(events) == 0L)
    return(compile_schema(cols, opts))

  # Derive the set of declared DE codes for the pivot's `code`-factor
  # pinning. "Declared DE codes" = every schema atom minus the link /
  # hierarchy / isTest / vs_days columns minus the per-DE companion
  # columns (which the pivot generates from value + storedBy +
  # createdBy + updatedBy + createdAt + updatedAt). Under the schema
  # contract this set is a pure function of opts.
  all_names  <- names(compile_schema(cols, opts))
  non_de     <- c(
    "event_key", "enrollment_key", "patient_key",
    "department_key", "hospital_key", "country_key",
    "world_bank_class_key", "isTest", "vs_days")
  companion  <- grepl(
    "_(storedBy|createdBy|updatedBy|createdAt|updatedAt)$", all_names)
  de_codes   <- setdiff(all_names[!companion], non_de)

  events <- events |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key == !!event_type_key) |>
        dplyr::select("event", "event_key"),
      dplyr::join_by("event")) |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues")

  # Per-DE user-key substitution — extended in phase-b-event-details
  # to cover all three user companions (`storedBy`, `createdBy`,
  # `updatedBy`). `storedBy` arrives as a plain String per DataValue
  # (no hoist); `createdBy` / `updatedBy` arrive as `{username: ...}`
  # User-subselect objects (hoist the first element = username).
  if (opts$include_user != "no" && "createdBy" %in% names(events))
    events <- events |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("username", "user_key"),
        dplyr::join_by("createdBy" == "username")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused")

  if (opts$include_user != "no" && "updatedBy" %in% names(events))
    events <- events |>
      tidyr::hoist("updatedBy", updatedBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("username", "user_key"),
        dplyr::join_by("updatedBy" == "username")) |>
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

  if (opts$include_user != "no" && "storedBy" %in% names(events))
    events <- events |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("username", "user_key"),
        dplyr::join_by("storedBy" == "username")) |>
      dplyr::mutate(storedBy = .data$user_key, .keep = "unused")

  if (opts$include_timestamps &&
      "createdAt" %in% names(events) &&
      "updatedAt" %in% names(events))
    events <- events |>
      dplyr::mutate(
        createdAt = readr::parse_datetime(.data$createdAt),
        updatedAt = readr::parse_datetime(.data$updatedAt),
        .keep = "unused")

  if (nrow(events) > 0) {
    events <- events |>
      dplyr::inner_join(
        metadata$dataElements |>
          dplyr::select("dataElement","code","valueType","optionSet"),
        dplyr::join_by("dataElement")) |>
      dplyr::left_join(
        metadata$options |>
          dplyr::arrange(.data$optionSet_code, .data$sortOrder) |>
          dplyr::group_by(.data$optionSet_code) |>
          dplyr::summarise(levels = list(.data$code)),
        dplyr::join_by("optionSet" == "optionSet_code")) |>
      dplyr::mutate(
        value = convert_value(.data$value, .data$valueType, .data$levels),
        code = stringr::str_extract(
          tolower(.data$code),
          "^neoipc_(admission|surveillance_end|bsi|nec|hap|ssi|surgery)_(.+)$",
          group = 2),
        .keep = "unused") |>
      dplyr::select(!c("event","dataElement","optionSet"))

    # Apply pre-pivot renames for NEC / HAP so the schema's post-
    # rename DE codes match the data. `device_association → dev_ass`
    # on HAP; `secondary_bsi → sec_bsi` on NEC and HAP.
    if (event_type_key == "nec")
      events <- events |>
        dplyr::mutate(code = dplyr::if_else(
          .data$code == "secondary_bsi", "sec_bsi", .data$code))
    else if (event_type_key == "hap")
      events <- events |>
        dplyr::mutate(code = dplyr::recode_values(
          .data$code,
          "device_association" ~ "dev_ass",
          "secondary_bsi"      ~ "sec_bsi",
          default              = .data$code))

    # Filter to the declared DE codes. Pathogen + AB_SUBST codes fall
    # out here (they route to infectiousAgentFindings and substanceDays
    # in their own sub-tasks). LOS falls out for admission (dropped
    # per the protocol).
    events <- events |>
      dplyr::filter(.data$code %in% de_codes)

    # Pin the `code` factor to the declared DE codes so that
    # `pivot_wider(..., names_expand = TRUE)` guarantees one
    # column-group per declared code regardless of whether any
    # surviving event carried that DE. Closes the pivot-volatility
    # hazard (the `vs_days` crash was the canonical instance).
    events <- events |>
      dplyr::mutate(code = factor(.data$code, levels = de_codes))

    events <- events |>
      tidyr::pivot_wider(
        names_from  = "code",
        values_from = !c("code", "event_key"),
        names_glue  = "{code}_{.value}",
        names_vary  = "slowest",
        names_expand = TRUE) |>
      tidyr::unnest_longer(dplyr::ends_with("value"), keep_empty = TRUE) |>
      dplyr::relocate(dplyr::ends_with("value"), .after = "event_key") |>
      dplyr::rename_with(
        ~ stringr::str_extract(.x, "^(.+)_value$", 1),
        tidyselect::ends_with("_value"))
  } else {
    # No surviving events: synthesize the expected shape directly from
    # the schema. Without this branch, the pivot path above can't run
    # (no rows → no columns to expand), and finalize_to_schema would
    # fail looking for declared columns that don't exist.
    events <- compile_schema(cols, opts)
  }

  # Derived column: vs_days = inv_days + niv_days on surveillance-end.
  # Under the schema contract both operands are guaranteed present
  # (pre-pivot factor pinning + names_expand). The legacy reader's
  # crash on missing columns is fixed by construction.
  if (event_type_key == "end" && "vs_days" %in% all_names)
    events <- events |>
      dplyr::mutate(vs_days = .data$inv_days + .data$niv_days)

  # Tail loud-finalize + assertion. `finalize_to_schema` drops any
  # column not declared on the schema; `assert_schema` verifies the
  # final shape.
  events <- finalize_to_schema(events, cols, opts)
  assert_schema(events, cols, opts)

  events
}

# Reader for `infectiousAgentFindings` *and* its sibling
# `unknownPathogenNames`. Returns a list with both tibbles.
#
# The split is load-bearing here, not at the call site: `name` is in
# `findings_cols`'s `scratch` set (allowed into `finalize_to_schema()`
# but stripped from the public output, because the design keeps the
# main findings tibble lean — see the schema comment on
# `unknownPathogenNames_cols`). Splitting *after* finalize sees no
# `name` column and produces an always-empty `unknownPathogenNames`,
# which silently broke Validation-Report Rule 20. The split therefore
# runs on the pre-finalize intermediate while `name` is still
# attached.
read_infectious_agent_findings <- function(events_raw, processed_events, metadata, dataset_options)
{
  opts <- dataset_options

  # Both children share the `include_event != "no"` gate. When closed,
  # emit each child's schema-shaped 0×0 directly.
  if (!entity_exists(findings_cols, opts))
    return(empty_findings_pair(opts))

  if (nrow(events_raw) == 0L)
    return(empty_findings_pair(opts))

  # Pre-pivot type levels: every DE-suffix this reader turns into a
  # column. `names_expand = TRUE` + this factor guarantees every
  # column exists regardless of data content — fixes failure pattern
  # #6 (missing `source` / `multiple` / resistance-marker / `name`
  # columns when no surviving pathogen carried the corresponding DE).
  type_levels <- c(
    "pathogen",           # base pathogen_key id
    "pathogen_source",    # BSI / HAP source code
    "pathogen_multiple",  # BSI multiple flag
    "pathogen_name",      # free-text pathogen name
    "pathogen_3gcr",
    "pathogen_car",
    "pathogen_cor",
    "pathogen_mrsa",
    "pathogen_vre")

  pathogen_data <- events_raw |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::filter(.data$event_type_key %in% c("bsi","nec","ssi","hap")) |>
        dplyr::select("event", "event_key", "event_type_key"),
      dplyr::join_by("event"))

  if (nrow(pathogen_data) == 0)
    return(empty_findings_pair(opts))

  pathogen_data <- pathogen_data |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("dataElement", "code"),
      dplyr::join_by("dataElement")) |>
    dplyr::filter(stringr::str_detect(.data$code, "PATHOGEN_\\d"))

  if (nrow(pathogen_data) == 0)
    return(empty_findings_pair(opts))

  intermediate <- pathogen_data |>
    dplyr::mutate(
      type = factor(
        tolower(stringr::str_replace(
          .data$code, "^.+(PATHOGEN)_\\d+(.*)$", "\\1\\2")),
        levels = type_levels),
      index = as.integer(stringr::str_replace(
        .data$code, "^.+PATHOGEN_(\\d+).*$", "\\1")),
      secondary_bsi = stringr::str_detect(.data$code, "_SEC_BSI_"),
      .keep = "unused") |>
    dplyr::select("event_key", "event_type_key", "type", "index",
                  "secondary_bsi", "value") |>
    tidyr::pivot_wider(
      names_from  = "type",
      values_from = "value",
      names_expand = TRUE) |>
    dplyr::rename_with(
      ~ stringr::str_extract(.x, "^pathogen_(.+)$", 1),
      .cols = tidyselect::starts_with("pathogen_")) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(c("3gcr", "car", "cor", "mrsa", "vre")),
        ~ factor(
          dplyr::recode_values(as.integer(.x),
                               0 ~ "no", 1 ~ "yes", -1 ~ "not_tested"),
          levels = c("no", "yes", "not_tested"))),
      multiple     = as.logical(.data$multiple),
      pathogen_key = as.integer(.data$pathogen),
      source = factor(
        dplyr::case_when(
          as.character(.data$event_type_key) == "bsi" &
            as.integer(.data$source) == 1L ~ "B",
          as.character(.data$event_type_key) == "bsi" &
            as.integer(.data$source) == 2L ~ "C",
          as.character(.data$event_type_key) == "bsi" &
            as.integer(.data$source) == 3L ~ "B+C",
          as.character(.data$event_type_key) == "hap" &
            as.integer(.data$source) == 1L ~ "U",
          as.character(.data$event_type_key) == "hap" &
            as.integer(.data$source) == 2L ~ "L",
          as.character(.data$event_type_key) == "hap" &
            as.integer(.data$source) == 3L ~ "U+L"),
        levels = c("B", "C", "B+C", "U", "L", "U+L")),
      .keep = "unused") |>
    add_key_column("agent_finding_key")

  unknownPathogenNames <- split_unknown_pathogen_names(intermediate, opts)

  findings <- intermediate |>
    finalize_to_schema(
      findings_cols, opts,
      scratch = c("event_type_key", "name", "pathogen"))
  assert_schema(findings, findings_cols, opts)

  list(
    infectiousAgentFindings = findings,
    unknownPathogenNames    = unknownPathogenNames)
}

# Pre-finalize-intermediate → `unknownPathogenNames`. Reads the still-
# attached `name` column off the intermediate produced by
# `read_infectious_agent_findings()`; only that caller has the
# intermediate, so this is an internal helper, not an exported reader.
split_unknown_pathogen_names <- function(intermediate, opts)
{
  if (!entity_exists(unknownPathogenNames_cols, opts))
    return(compile_schema(unknownPathogenNames_cols, opts))

  # Under pseudo events, payload atoms (including `name`) are gated off
  # upstream — the intermediate has no `name` column to split. Emit the
  # schema's 0-row shape directly.
  if (!("name" %in% names(intermediate))) {
    public <- compile_schema(unknownPathogenNames_cols, opts)
    assert_schema(public, unknownPathogenNames_cols, opts)
    return(public)
  }

  public <- intermediate |>
    dplyr::filter(!is.na(.data$name) & nzchar(.data$name)) |>
    dplyr::select("agent_finding_key", "name") |>
    finalize_to_schema(unknownPathogenNames_cols, opts)
  assert_schema(public, unknownPathogenNames_cols, opts)

  public
}

empty_findings_pair <- function(opts)
{
  list(
    infectiousAgentFindings = compile_schema(findings_cols, opts),
    unknownPathogenNames    = compile_schema(unknownPathogenNames_cols, opts))
}

read_substance_days <- function(events_raw, processed_events, metadata, dataset_options)
{
  opts <- dataset_options

  if (!entity_exists(substanceDays_cols, opts))
    return(compile_schema(substanceDays_cols, opts))

  if (nrow(events_raw) == 0L)
    return(compile_schema(substanceDays_cols, opts))

  pathogen_data <- events_raw |>
    dplyr::select("event", "dataValues") |>
    dplyr::inner_join(
      processed_events |>
        dplyr::select("event", "event_key"),
      dplyr::join_by("event"))

  if (nrow(pathogen_data) == 0) {
    public <- compile_schema(substanceDays_cols, opts)
    assert_schema(public, substanceDays_cols, opts)
    return(public)
  }

  public <- pathogen_data |>
    tidyr::unnest_longer("dataValues") |>
    tidyr::unnest_wider("dataValues") |>
    dplyr::inner_join(
      metadata$dataElements |>
        dplyr::select("dataElement", "code") |>
        dplyr::filter(stringr::str_starts(
          .data$code, "NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d\\d")),
      dplyr::join_by("dataElement")) |>
    dplyr::select(!"dataElement") |>
    dplyr::mutate(
      index = as.integer(stringr::str_extract(
        .data$code,
        "^NEOIPC_SURVEILLANCE_END_AB_SUBST_\\d(\\d)(_DAYS)?$", 1)),
      name = factor(
        dplyr::if_else(
          stringr::str_ends(.data$code, "_DAYS"),
          "days", "substance_code"),
        levels = c("substance_code", "days")),
      .keep = "unused") |>
    tidyr::pivot_wider(names_expand = TRUE) |>
    dplyr::mutate(days = as.integer(.data$days)) |>
    finalize_to_schema(substanceDays_cols, opts, scratch = "event")
  assert_schema(public, substanceDays_cols, opts)

  public
}
