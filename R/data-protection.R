#' Assert data-protection invariants on a neoipcr dataset.
#'
#' @description
#' This is the authoritative data-protection guardian. It runs last in
#' the `import_dhis2()` pipeline. Under the schema contract the readers
#' own every tibble's shape, so this function's job is to **assert
#' invariants** -- not to scrub columns. A schema regression that leaks
#' a column reserved for another option value surfaces here with an
#' actionable `rlang::abort()`.
#'
#' Under the schema contract every branch is an assertion -- no scrubs.
#' The name change from `apply_data_removal()` (which scrubbed) reflects
#' this semantic shift: this is a guardian, not a remover. Adding a new
#' scrub here should not be necessary -- push the narrowing into the
#' relevant `R/schema-*.R` instead.
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

  x
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
                       "departments", "users", "eventTypes")
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
