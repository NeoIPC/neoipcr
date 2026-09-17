#' Assert data-protection invariants on a neoipcr dataset.
#'
#' @description
#' Runs last in the `import_dhis2()` pipeline. Under the schema contract
#' the readers own every tibble's shape, so this function's job is to
#' **assert invariants** -- not to scrub columns.
#'
#' Scope is currently narrow: three invariant families are asserted here
#' as a final net.
#'
#' 1. **Hierarchy keys** under `include_<level> == "no"` -- the key
#'    (`department_key`, `hospital_key`, `country_key`,
#'    `world_bank_class_key`) must be absent from every fact tibble and
#'    from adjacent metadata tibbles that pre-join it.
#' 2. **Companion columns** on metadata tibbles -- `createdBy`,
#'    `updatedBy`, `createdAt`, `updatedAt` are reserved for
#'    partner-site-entered entities and must never appear on metadata
#'    tibbles curated by NeoIPC.
#' 3. **Org-unit attribute values** -- `departmentAttributeValues` and
#'    `hospitalAttributeValues` must be empty unless the caller opted
#'    into that entity's attributes (`include_custom_attributes`) and the
#'    entity is present, and must never carry an `IsTestunit` row (that
#'    flag is folded into `isTest`).
#'
#' Everything else (per-entity id / link / patient narrowing, factor
#' levels, metadata-tibble nulling) is owned by the schemas + readers
#' + `finalize_to_schema()`. A regression in any of those surfaces in
#' the per-schema tests, not here. Tightening this guardian to assert
#' the broader id / link / patient invariants as a final net (and to
#' make the metadata data-protection contract explicit around the
#' `users` PII surface) is tracked as follow-up work.
#'
#' The name change from `apply_data_removal()` (which scrubbed)
#' reflects the semantic shift: this is a guardian, not a remover.
#' Adding a new scrub here should not be necessary -- push the
#' narrowing into the relevant `R/schema-*.R` instead.
#'
#' @param x A `neoipcr_ds` object.
#' @param dataset_options A `dhis2_dataset_options` object.
#' @return `x`, unchanged.
#' @noRd
assert_data_protection <- function(x, dataset_options)
{
  # Fact-table hierarchy keys -- reader-owned via `R/schema-<entity>.R`.
  # When the user opted out of a hierarchy level, the key must be absent
  # from every fact tibble and from adjacent metadata tibbles that carry
  # it as a pre-join. A reader regression that leaks the key surfaces
  # here.
  .assert_hierarchy_key_absent(x, dataset_options,
    opts_key         = "include_department",
    col_name         = "department_key",
    fact_targets     = c("patients", "enrollments", "events"),
    metadata_targets = character())

  .assert_hierarchy_key_absent(x, dataset_options,
    opts_key         = "include_hospital",
    col_name         = "hospital_key",
    fact_targets     = c("patients", "enrollments", "events"),
    metadata_targets = c("departments"))

  .assert_hierarchy_key_absent(x, dataset_options,
    opts_key         = "include_country",
    col_name         = "country_key",
    fact_targets     = c("patients", "enrollments", "events"),
    metadata_targets = c("hospitals", "departments"))

  .assert_hierarchy_key_absent(x, dataset_options,
    opts_key         = "include_world_bank_class",
    col_name         = "world_bank_class_key",
    fact_targets     = c("patients", "enrollments", "events"),
    metadata_targets = c("countries", "hospitals", "departments"))

  # (Former `eventDetails` scrub removed in phase-b-event-details --
  # the sidecar tibble was merged into `events`, and the `event` id on
  # events itself is now schema-gated via `events_cols`.)

  # Metadata tibbles are curated by the NeoIPC team, not by partner-site
  # data entry, so they must never carry per-row author/timestamp
  # companion columns. An accidental reader regression that leaks these
  # surfaces loudly here.
  .assert_metadata_companion_cols_absent(x)

  # Org-unit attribute values are an explicit opt-in per entity and can
  # carry personal data (a site's contact person), so a populated table
  # without the opt-in -- or for an entity the caller excluded -- is a
  # leak. `IsTestunit` is folded into `isTest` by the orchestrator and
  # must never surface as a value row.
  .assert_attribute_values_absent(x, dataset_options,
    entity   = "departments",
    opts_key = "include_department",
    tbl      = "departmentAttributeValues")
  .assert_attribute_values_absent(x, dataset_options,
    entity   = "hospitals",
    opts_key = "include_hospital",
    tbl      = "hospitalAttributeValues")
  .assert_no_test_unit_attribute_rows(x)

  x
}


#' Assert that an org-unit attribute-values table is 0×0 unless the caller
#' opted into that entity's attributes and the entity is present.
#'
#' The contract is the shape, not the row count: a closed gate yields a 0×0
#' tibble (`finalize_to_schema()`), so a table that carries the schema's
#' columns without the opt-in was emitted by a reader that ignored the gate,
#' whether or not any row happens to be in it. Passing an empty
#' schema-shaped table would let exactly that reader go unnoticed until an
#' instance with values hit it.
#'
#' @noRd
.assert_attribute_values_absent <- function(x, opts, entity, opts_key, tbl) {
  t <- x$metadata[[tbl]]
  if (is.null(t) || ncol(t) == 0L)
    return(invisible(NULL))
  if (entity %in% opts$include_custom_attributes && opts[[opts_key]] != "no")
    return(invisible(NULL))

  opted <- if (length(opts$include_custom_attributes) == 0L) "character()"
           else paste0('"', paste(opts$include_custom_attributes, collapse = '", "'), '"')
  rlang::abort(c(
    sprintf("Data-protection violation: `x$metadata$%s` carries columns without the `%s` opt-in (a closed gate yields a 0x0 tibble).",
            tbl, entity),
    "x" = sprintf("`include_custom_attributes` = %s; `%s` = \"%s\".",
                  opted, opts_key, opts[[opts_key]]),
    "i" = paste0("Custom attribute values can carry personal data. Fix the ",
                 "reader that emits them -- this guardian asserts, it does ",
                 "not scrub.")
  ))
}


#' Assert that no `IsTestunit` row survives in an attribute-values table.
#'
#' @noRd
.assert_no_test_unit_attribute_rows <- function(x) {
  for (tbl in c("departmentAttributeValues", "hospitalAttributeValues")) {
    t <- x$metadata[[tbl]]
    if (is.null(t) || !("attribute_code" %in% names(t)))
      next
    if (any(t$attribute_code == "IsTestunit", na.rm = TRUE))
      rlang::abort(c(
        sprintf("`x$metadata$%s` carries `IsTestunit` rows.", tbl),
        "i" = paste0("The test-unit flag is represented by `isTest`; the ",
                     "orchestrator folds these rows in and drops them.")
      ))
  }
}


#' Assert that a hierarchy key is absent from every relevant tibble
#' under `opts[[opts_key]] == "no"`.
#'
#' Belt-and-suspenders guardian that fires when a reader regression
#' leaks the column the user opted out of. Reports all leak sites in
#' one message so the caller sees the full picture.
#'
#' @noRd
.assert_hierarchy_key_absent <- function(x, opts, opts_key, col_name,
                                         fact_targets,
                                         metadata_targets = character()) {
  if (opts[[opts_key]] != "no")
    return(invisible(NULL))

  leaks <- character()
  for (t in fact_targets) {
    if (!is.null(x[[t]]) && col_name %in% names(x[[t]]))
      leaks <- c(leaks, paste0("x$", t))
  }
  for (t in metadata_targets) {
    m <- x$metadata[[t]]
    if (!is.null(m) && col_name %in% names(m))
      leaks <- c(leaks, paste0("x$metadata$", t))
  }

  if (length(leaks) == 0L)
    return(invisible(NULL))

  rlang::abort(c(
    sprintf("Data-protection violation: `%s` leaked under `%s = \"no\"`.",
            col_name, opts_key),
    "x" = paste(leaks, collapse = ", "),
    "i" = paste0("Fix the reader that emits `", col_name,
                 "` on the tibble(s) above -- this guardian asserts, it ",
                 "does not scrub.")
  ))
}


#' Assert that metadata tibbles do not carry companion columns.
#'
#' `createdBy` / `updatedBy` / `createdAt` / `updatedAt` are reserved
#' for partner-site-entered entities (patients, enrollments, events,
#' per-event-type data, findings, substance days, notes). Metadata
#' tibbles are curated by the NeoIPC team, so these columns have no
#' semantic meaning there.
#'
#' @noRd
.assert_metadata_companion_cols_absent <- function(x) {
  companion_cols  <- c("createdBy", "updatedBy", "createdAt", "updatedAt")
  metadata_tables <- c("worldBankClasses", "countries", "hospitals",
                       "departments", "users", "eventTypes",
                       "orgUnitAttributes", "departmentAttributeValues",
                       "hospitalAttributeValues")
  for (tbl in metadata_tables) {
    t <- x$metadata[[tbl]]
    if (!is.null(t)) {
      leaked <- intersect(companion_cols, names(t))
      if (length(leaked) > 0L)
        rlang::abort(c(
          sprintf(paste0("Metadata tibble `%s` carries companion ",
                         "column(s) that are reserved for partner-site-",
                         "entered entities:"), tbl),
          "x" = paste(leaked, collapse = ", "),
          "i" = paste0("Metadata entities are curated by NeoIPC, not ",
                       "partner sites. Drop these columns from the reader.")
        ))
    }
  }
}
