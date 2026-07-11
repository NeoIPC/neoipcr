get_enrollments_request <- function(req_base, dataset_options, programId)
{
  # `orgUnit` is a base field: read_enrollments() always needs it — to mark
  # isTest (include_test_data = TRUE) or to filter out test units (FALSE), which
  # are exhaustive, plus hierarchy keys — and drops it after the joins.
  fields <- "enrollment,trackedEntity,enrolledAt,followUp,orgUnit"

  if("enrollments" %in% dataset_options$include_notes)
    fields <- paste0(fields, ",notes")

  if("enrollments" %in% dataset_options$include_incomplete)
    fields <- paste0(fields,",status")
  else
    req_base <- req_base |>
      httr2::req_url_query(programStatus = "COMPLETED")

  if(dataset_options$include_timestamps)
    fields <- paste0(
      fields,
      ",occurredAt,createdAt,createdAtClient,updatedAt,updatedAtClient,completedAt")

  if(dataset_options$include_deleted)
    fields <- paste0(fields,",deleted")

  if(dataset_options$include_user != "no")
    fields <- paste0(
      fields,",completedBy,storedBy,createdBy[username],updatedBy[username]")

  req_base |>
    httr2::req_url_path_append("enrollments") |>
    httr2::req_url_query(program = programId) |>
    httr2::req_url_query(fields = fields)
}

read_enrollments <- function(enrollments, patients, metadata, dataset_options)
{
  opts <- dataset_options

  # Entity gate short-circuit: under `include_enrollment = "no"` the
  # public enrollments tibble is 0×0, and downstream readers that
  # consume `enrollments` must tolerate the empty shape. Matches the
  # pattern in `read_patients()`.
  .empty_result <- function()
    list(
      public       = compile_schema(enrollments_cols, opts),
      internal_map = tibble::tibble(
        enrollment_key = integer(),
        enrollment     = character(),
        patient_key    = integer()))

  if (opts$include_enrollment == "no")
    return(.empty_result())

  if (nrow(enrollments) == 0L)
    return(.empty_result())

  # Substitute raw DHIS2 `trackedEntity` UID with `patient_key` via
  # the orchestrator-internal patients map. The public patients tibble
  # may not carry `trackedEntity` (it's gated on include_dhis2_ids),
  # so the join goes through `.patients_internal_map` which always
  # carries `patient_key + trackedEntity` regardless of schema gates.
  enrollments <- enrollments |>
    dplyr::inner_join(
      metadata$.patients_internal_map,
      dplyr::join_by("trackedEntity")) |>
    dplyr::mutate(
      enrolledAt = readr::parse_date(
        stringr::str_sub(.data$enrolledAt, end = 10))) |>
    dplyr::select(!"trackedEntity")

  if("enrollments" %in% dataset_options$include_incomplete)
    enrollments <- enrollments |>
      dplyr::mutate(
        status = factor(.data$status, levels = c(
          "ACTIVE", "COMPLETED", "CANCELLED")))

  if(dataset_options$include_timestamps)
    enrollments <- enrollments |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(
            c("occurredAt","createdAt","createdAtClient","updatedAt",
              "updatedAtClient","completedAt")), readr::parse_datetime))

  if(dataset_options$include_test_data ||
     dataset_options$include_department != "no" ||
     dataset_options$include_hospital != "no" ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no" ||
     length(dataset_options$include_invalid_patients) > 1)
  {
    hierarchy_cols <- intersect(
      c("department_key", "hospital_key", "country_key",
        "world_bank_class_key", "isTest"),
      names(compile_schema(enrollments_cols, opts)))
    enrollments <- enrollments |>
      dplyr::left_join(
        metadata$.departments_internal_map |>
          dplyr::select(tidyselect::all_of(c("orgUnit", hierarchy_cols))),
        dplyr::join_by("orgUnit"))
  }

  if(dataset_options$include_user != "no") {
    enrollments <- enrollments |>
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
      dplyr::mutate(updatedBy = .data$user_key, .keep = "unused")

    if("completedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("completedBy" == "username")) |>
        dplyr::mutate(completedBy = .data$user_key, .keep = "unused")

    if("storedBy" %in% names(enrollments))
      enrollments <- enrollments |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("user_key", "username"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  if(!dataset_options$include_test_data ||
     length(dataset_options$country_filter) > 0 ||
     length(dataset_options$trial_keys) > 0)
    enrollments <- enrollments |>
      dplyr::semi_join(
        metadata$.departments_internal_map, dplyr::join_by("orgUnit"))

  enrollments <- enrollments |>
    dplyr::select(!tidyselect::any_of("orgUnit")) |>
    add_key_column("enrollment_key")

  internal_map <- enrollments |>
    dplyr::select("enrollment_key", "enrollment",
                  tidyselect::any_of("patient_key"))

  # Narrow to the public schema + loud-assert.
  enrollments <- enrollments |>
    finalize_to_schema(enrollments_cols, opts,
                       scratch = c("trackedEntity", "notes"))
  assert_schema(enrollments, enrollments_cols, opts)

  list(public = enrollments, internal_map = internal_map)
}

# Enrollment-notes reader, mirroring `read_event_notes()`. The raw
# enrollments response carries a `notes` nested field per enrollment;
# the enrollment reader drops it via finalize_to_schema's scratch
# list so this reader needs the raw response as its first argument.
#
# Same user-lookup asymmetry as event notes (see
# `dhis2-events.R::read_event_notes` for the rationale): `createdBy`
# is a DHIS2 UID, so we join on the internal map's `user` column;
# `storedBy` is a username, so we join on `username`.
read_enrollment_notes <- function(enrollments_raw, processed_enrollments,
                                  metadata, dataset_options)
{
  opts <- dataset_options

  if (!entity_exists(enrollment_notes_cols, opts))
    return(compile_schema(enrollment_notes_cols, opts))

  if (nrow(enrollments_raw) == 0L)
    return(compile_schema(enrollment_notes_cols, opts))

  notes <- enrollments_raw |>
    dplyr::inner_join(
      processed_enrollments |>
        dplyr::select("enrollment_key", "enrollment"),
      dplyr::join_by("enrollment")) |>
    dplyr::select("enrollment_key", "notes") |>
    dplyr::filter(!is.na(.data$notes) & lengths(.data$notes) > 0)

  if (nrow(notes) == 0) {
    public <- compile_schema(enrollment_notes_cols, opts)
    assert_schema(public, enrollment_notes_cols, opts)
    return(public)
  }

  notes <- notes |>
    tidyr::unnest_longer("notes") |>
    tidyr::hoist("notes",
      note = "note",
      value = "value",
      storedBy = "storedBy",
      storedAt = "storedAt",
      createdBy = "createdBy",
      .remove = TRUE)

  if (nrow(notes) == 0) {
    public <- compile_schema(enrollment_notes_cols, opts)
    assert_schema(public, enrollment_notes_cols, opts)
    return(public)
  }

  if (opts$include_user != "no") {
    notes <- notes |>
      tidyr::hoist("createdBy", createdBy = 1, .remove = FALSE) |>
      dplyr::left_join(
        metadata$.users_internal_map |>
          dplyr::select("user", "user_key"),
        dplyr::join_by("createdBy" == "user")) |>
      dplyr::mutate(createdBy = .data$user_key, .keep = "unused")

    if ("storedBy" %in% names(notes))
      notes <- notes |>
        dplyr::left_join(
          metadata$.users_internal_map |>
            dplyr::select("username", "user_key"),
          dplyr::join_by("storedBy" == "username")) |>
        dplyr::mutate(storedBy = .data$user_key, .keep = "unused")
  }

  if (opts$include_timestamps && "storedAt" %in% names(notes))
    notes <- notes |>
      dplyr::mutate(storedAt = readr::parse_datetime(.data$storedAt))

  public <- notes |>
    finalize_to_schema(enrollment_notes_cols, opts)
  assert_schema(public, enrollment_notes_cols, opts)

  public
}
