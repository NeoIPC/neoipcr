#' Apply dataset-level filters anchored on a `dhis2_dataset_options`
#' object.
#'
#' Narrows `x` by the date / range / country filters declared on
#' `dataset_options` and then optionally runs `apply_postfilter()` to
#' cascade orphan removal through the hierarchy. The caller typically
#' uses the same `dataset_options` object that drove `import_dhis2()`;
#' the filters here re-apply the configuration post-hoc (e.g. on a
#' narrower birth-weight range than the original import request).
#'
#' Before phase-c-audit the function took individual filter parameters
#' (`birth_weight_from`, `gestational_age_from`, `countries`, `units`,
#' `keep_non_core_patients`). Pre-alpha: the old signature was removed
#' outright, no deprecation shim. See
#' `tasks/neoipcr-schema-arc/phase-c-audit.md` C3.
#'
#' @param x A `neoipcr_ds` object.
#' @param dataset_options A `dhis2_dataset_options` object. Consulted
#'   for `surveillance_end_from/to`, `birth_weight_from/to`,
#'   `gestational_age_from/to`, `country_filter`, and
#'   `include_ineligible_patients`.
#' @param remove_orphans If `TRUE` (default), also runs
#'   `apply_postfilter()` so that hierarchy rows with no surviving
#'   descendants are pruned.
#' @noRd
filter_dataset <- function(x, dataset_options, remove_orphans = TRUE)
{
  opts <- dataset_options

  x$events <- x$events |>
    filter_surveillance_ends(
      opts$surveillance_end_from,
      opts$surveillance_end_to)

  x$admissionData <- x$admissionData |>
    filter_admissions(opts$include_ineligible_patients)

  x$patients <- x$patients |>
    filter_patients(
      opts$birth_weight_from,
      opts$birth_weight_to,
      opts$gestational_age_from,
      opts$gestational_age_to,
      opts$include_ineligible_patients)

  x$metadata$countries <- x$metadata$countries |>
    filter_countries(opts$country_filter)

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
    include_ineligible_patients = FALSE)
{
  if(include_ineligible_patients)
    return(admission_data)

  admission_data |>
    dplyr::filter(.data$dol < 120)
}

filter_patients <- function(
    patients,
    birth_weight_from = NULL,
    birth_weight_to = NULL,
    gestational_age_from = NULL,
    gestational_age_to = NULL,
    include_ineligible_patients = FALSE)
{
  if(!is.null(birth_weight_from))
    patients <- patients |>
      dplyr::filter(.data$birth_weight >= birth_weight_from)
  if(!is.null(birth_weight_to))
    patients <- patients |>
      dplyr::filter(.data$birth_weight <= birth_weight_to)
  if(!is.null(gestational_age_from))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days >= (gestational_age_from * 7))
  if(!is.null(gestational_age_to))
    patients <- patients |>
      dplyr::filter(.data$total_gestation_days <= (gestational_age_to * 7))
  if(!include_ineligible_patients)
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

#' Orphan-cleanup post-filter.
#'
#' Runs after `filter_dataset()` (and after `import_dhis2()`'s
#' narrowing passes) to prune every tibble in `x` so the final dataset
#' is internally consistent: no fact row whose link-FK points at a
#' removed parent, no metadata row whose hierarchy-key has no surviving
#' descendant.
#'
#' Structure (post phase-c-audit):
#'
#' 1. **One-time prefilter** — neoipcr-specific enrollment invariants
#'    (an enrollment only counts if it has a surveillance-end event
#'    whose admission data is present; enrollments outside the filtered
#'    country / department lists drop). Runs once before the cascade.
#' 2. **Fixed-point cascade** — link-FK orphan cleanup (`patient_key`
#'    / `enrollment_key` / `event_key`) + hierarchy-metadata upward
#'    cascade with **per-hierarchy-key dynamic anchor selection**
#'    (union of every data-carrying tibble that carries the key).
#'    Iterates until no tibble loses a row in a full pass.
#'
#' Every join site is **column-presence-guarded**: under link-privacy
#' gates (`include_patient` / `include_enrollment` / `include_event`
#' = `"no"`) the relevant key column is absent from one or more
#' tibbles, and the corresponding semi-join is skipped. Under the
#' inheritance rule hierarchy keys materialize at whichever tibble the
#' rule dictates; the dynamic anchor picks that up without per-entity
#' branching.
#'
#' `country_key` retains **NA-tolerance** because test units have no
#' country and must survive the cascade.
#'
#' @param x A `neoipcr_ds` object.
#' @return `x` with every tibble semi-joined / filtered per the
#'   cascade.
#' @noRd
apply_postfilter <- function(x)
{
  # Step 1: one-time dataset-driven enrollment prefilter.
  x <- .postfilter_prefilter_enrollments(x)

  # Step 2: fixed-point cascade.
  repeat {
    before <- .postfilter_row_counts(x)
    x      <- .postfilter_pass(x)
    after  <- .postfilter_row_counts(x)
    if (identical(before, after)) break
  }

  # Step 3: drop unused levels on data-sourced factor columns. Protocol-
  # fixed factors (`levels_source = "fixed"`) keep their full level list
  # per the schema contract; data-sourced factors
  # (`levels_source = "data"`) get their levels regenerated so the level
  # list matches the surviving rows. Prevents the "this country existed
  # in the source data before filtering" leak via unused factor levels.
  x <- .postfilter_droplevels_data_factors(x)

  x
}


# ---- Step 1: one-time prefilter ------------------------------------------
#
# Three pieces, each guarded on column presence so link-privacy gates
# (`"no"`) don't crash:
#   a. Enrollments keep only the ones whose surveillance-end event is
#      present AND whose admission data row is present.
#   b. Enrollments keep only NA-country or country-in-filtered-metadata.
#   c. Enrollments keep only department-in-filtered-metadata.
#
# These propagate dataset-options-driven metadata narrowings (country
# filter, department filter, surveillance-end-with-admission invariant)
# down into enrollments. The cascade in Step 2 then carries the
# narrowing through the rest of the fact and metadata tibbles.
.postfilter_prefilter_enrollments <- function(x)
{
  if (!("enrollment_key" %in% names(x$enrollments)))
    return(x)

  # (a) surveillance-end-with-admission invariant.
  if ("event_key" %in% names(x$events) &&
      "enrollment_key" %in% names(x$events) &&
      "event_key" %in% names(x$admissionData)) {
    surveillance_end_events <- x$events |>
      dplyr::filter(.data$event_type_key != "end") |>
      dplyr::select("enrollment_key", "event_key")
    se_with_admission <- surveillance_end_events |>
      dplyr::semi_join(x$admissionData, dplyr::join_by("event_key"))
    x$enrollments <- x$enrollments |>
      dplyr::semi_join(se_with_admission, dplyr::join_by("enrollment_key"))
  }

  # (b) NA-tolerant country-filter propagation.
  if ("country_key" %in% names(x$metadata$countries) &&
      "country_key" %in% names(x$enrollments)) {
    country_keys <- x$metadata$countries$country_key
    x$enrollments <- x$enrollments |>
      dplyr::filter(
        is.na(.data$country_key) |
          .data$country_key %in% country_keys)
  }

  # (c) department-filter propagation.
  if ("department_key" %in% names(x$metadata$departments) &&
      "department_key" %in% names(x$enrollments))
    x$enrollments <- x$enrollments |>
      dplyr::semi_join(
        x$metadata$departments, dplyr::join_by("department_key"))

  x
}


# ---- Step 2: fixed-point cascade pass ------------------------------------
#
# Executes one full cascade pass:
#   a. Link-FK orphan cleanup across fact tibbles.
#   b. Hierarchy-metadata upward cascade with dynamic anchor selection.
#
# `apply_postfilter()` iterates this pass to a fixed point (no tibble
# loses rows in a full pass).
.postfilter_pass <- function(x)
{
  x <- .postfilter_link_fk_cascade(x)
  x <- .postfilter_hierarchy_cascade(x)
  x
}


# Link-FK cascade — fact-to-fact orphan cleanup.
#
# Every join guarded on column presence on both sides so link-privacy
# gates (`include_patient` / `include_enrollment` / `include_event`
# = `"no"`) turn off the corresponding branch.
.postfilter_link_fk_cascade <- function(x)
{
  # events ← enrollments (link FK).
  if ("enrollment_key" %in% names(x$events) &&
      "enrollment_key" %in% names(x$enrollments))
    x$events <- x$events |>
      dplyr::semi_join(x$enrollments, dplyr::join_by("enrollment_key"))

  # events ← patients (secondary link FK — under include_patient != "no"
  # + include_enrollment == "no" this is the only events→patients
  # linkage).
  if ("patient_key" %in% names(x$events) &&
      "patient_key" %in% names(x$patients))
    x$events <- x$events |>
      dplyr::semi_join(x$patients, dplyr::join_by("patient_key"))

  # per-event-type data ← events.
  event_child_tbls <- c(
    "admissionData", "surveillanceEndData", "sepsisData", "necData",
    "pneumoniaData", "surgeryData", "ssiData",
    "infectiousAgentFindings", "substanceDays", "eventNotes")
  for (t in event_child_tbls) {
    if ("event_key" %in% names(x[[t]]) &&
        "event_key" %in% names(x$events))
      x[[t]] <- x[[t]] |>
        dplyr::semi_join(x$events, dplyr::join_by("event_key"))
  }

  # enrollment_notes ← enrollments.
  if ("enrollment_key" %in% names(x$enrollment_notes) &&
      "enrollment_key" %in% names(x$enrollments))
    x$enrollment_notes <- x$enrollment_notes |>
      dplyr::semi_join(x$enrollments, dplyr::join_by("enrollment_key"))

  # Upward prune: enrollments with no surviving patient.
  if ("patient_key" %in% names(x$enrollments) &&
      "patient_key" %in% names(x$patients))
    x$enrollments <- x$enrollments |>
      dplyr::semi_join(x$patients, dplyr::join_by("patient_key"))

  # Upward prune: patients with no surviving enrollment.
  if ("patient_key" %in% names(x$patients) &&
      "patient_key" %in% names(x$enrollments))
    x$patients <- x$patients |>
      dplyr::semi_join(x$enrollments, dplyr::join_by("patient_key"))

  x
}


# Hierarchy-metadata upward cascade with dynamic anchor selection.
#
# For each of the four hierarchy keys, compute the "surviving key"
# anchor as the union of that key's values across every data-carrying
# tibble (fact or metadata) that carries the column — excluding the
# metadata tibble being filtered, which is the target of the cascade.
#
# The union-based anchor handles every inheritance configuration:
#
#   - Full-department-chain case: `metadata$departments` carries every
#     hierarchy key via pre-join; fact tibbles don't materialize
#     upstream keys directly. Anchor for `hospital_key` /
#     `country_key` / `world_bank_class_key` comes from
#     `metadata$departments`.
#
#   - Pseudo-event case: `events` is 1-column PK-only; per-event-type
#     data tibbles materialize hierarchy keys via inheritance. Anchor
#     for those keys comes from the per-event-type data tibbles.
#
#   - Full-fact-chain case: hierarchy keys materialize on every fact
#     level. Anchor is the union across every non-empty fact tibble
#     (the cascade converges after link-FK cleanup stabilizes).
#
# NA-tolerance is applied to `country_key` only (test units have no
# country).
.postfilter_hierarchy_cascade <- function(x)
{
  hierarchy_map <- list(
    department_key       = "departments",
    hospital_key         = "hospitals",
    country_key          = "countries",
    world_bank_class_key = "worldBankClasses")

  for (hk in names(hierarchy_map)) {
    md_tbl <- hierarchy_map[[hk]]
    md     <- x$metadata[[md_tbl]]
    if (is.null(md) || !(hk %in% names(md))) next

    surviving <- .surviving_hierarchy_keys(x, hk, exclude_md_tbl = md_tbl)
    # If no other tibble carries the key, there's nothing to propagate
    # from — leave metadata alone.
    if (is.null(surviving)) next

    x$metadata[[md_tbl]] <- md |>
      dplyr::filter(.data[[hk]] %in% surviving)
  }

  x
}


# Collect the union of a hierarchy key's values across every data-
# carrying tibble in `x` that carries the column, excluding the
# metadata tibble being filtered (which is the target).
#
# Returns `NULL` when no tibble carries the key (caller should skip
# filtering in that case — there's no source of truth).
.surviving_hierarchy_keys <- function(x, hk, exclude_md_tbl)
{
  fact_tbls <- c(
    "patients", "enrollments", "events",
    "admissionData", "surveillanceEndData", "sepsisData", "necData",
    "pneumoniaData", "surgeryData", "ssiData",
    "infectiousAgentFindings", "substanceDays",
    "eventNotes", "enrollment_notes")
  md_tbls <- setdiff(
    c("worldBankClasses", "countries", "hospitals", "departments"),
    exclude_md_tbl)

  vals <- list()
  for (t in fact_tbls) {
    tib <- x[[t]]
    if (!is.null(tib) && hk %in% names(tib))
      vals[[length(vals) + 1L]] <- tib[[hk]]
  }
  for (t in md_tbls) {
    tib <- x$metadata[[t]]
    if (!is.null(tib) && hk %in% names(tib))
      vals[[length(vals) + 1L]] <- tib[[hk]]
  }

  if (length(vals) == 0L) return(NULL)
  unique(unlist(vals))
}


# Snapshot row counts of every tibble in `x` for fixed-point detection.
.postfilter_row_counts <- function(x)
{
  fact_tbls <- c(
    "patients", "enrollments", "events",
    "admissionData", "surveillanceEndData", "sepsisData", "necData",
    "pneumoniaData", "surgeryData", "ssiData",
    "infectiousAgentFindings", "substanceDays",
    "eventNotes", "enrollment_notes")
  md_tbls <- c("worldBankClasses", "countries", "hospitals", "departments")

  c(
    vapply(fact_tbls, function(t) {
      tib <- x[[t]]; if (is.null(tib)) 0L else nrow(tib)
    }, integer(1)),
    vapply(md_tbls, function(t) {
      tib <- x$metadata[[t]]; if (is.null(tib)) 0L else nrow(tib)
    }, integer(1)))
}


# Drop unused levels on every data-sourced factor column in `x`.
#
# Per the schema contract, factor columns are either "fixed" (levels
# come from the protocol / DHIS2 source and are asserted by
# `assert_schema`) or "data" (levels come from DHIS2 data and may
# change run-to-run). Fixed-levels factors retain their full declared
# level list regardless of which values survive filtering. Data-sourced
# factors have `droplevels()` applied after the cascade so the level
# list matches the surviving rows — preventing the "this value existed
# in the pre-filter data" leak.
#
# Only tibbles that carry data-sourced factor columns today appear
# below (`countries`, `eventTypes`, `patients`). The helper is
# idempotent and column-presence-guarded, so a future schema that
# adds / removes data-sourced factors just needs its cols list wired
# in here.
.postfilter_droplevels_data_factors <- function(x)
{
  targets <- list(
    list(path = c("metadata", "countries"),  cols = countries_cols),
    list(path = c("metadata", "eventTypes"), cols = eventTypes_cols),
    list(path = "patients",                  cols = patients_cols))

  for (t in targets) {
    tib <- .get_path(x, t$path)
    if (is.null(tib) || nrow(tib) == 0L) next
    tib <- .droplevels_data_factors_on(tib, t$cols)
    x   <- .set_path(x, t$path, tib)
  }

  x
}


# Call `droplevels()` on every column of `tib` whose matching schema
# atom is a factor with `levels_source = "data"`.
.droplevels_data_factors_on <- function(tib, cols_list)
{
  for (col in cols_list) {
    if (!identical(col$levels_source, "data")) next
    if (!(col$name %in% names(tib)))           next
    if (!is.factor(tib[[col$name]]))           next
    tib[[col$name]] <- droplevels(tib[[col$name]])
  }
  tib
}


# Minimal path accessors so `metadata$countries` and `patients` can be
# addressed uniformly in `.postfilter_droplevels_data_factors()`.
.get_path <- function(x, path)
{
  for (p in path) x <- x[[p]]
  x
}

.set_path <- function(x, path, value)
{
  if (length(path) == 1L) {
    x[[path]] <- value
    return(x)
  }
  x[[path[[1]]]] <- .set_path(x[[path[[1]]]], path[-1L], value)
  x
}
