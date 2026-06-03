# Tests for R/data-protection.R — comprehensive option-matrix coverage.
# Phase C2: iterates every relevant `include_*` option against every
# tibble in a neoipcr_ds to verify the guardian's invariants hold under
# every option combination the schema contract supports.
#
# The per-entity schema-shape tests in `test-schema-*.R` verify that
# `compile_schema()` produces the right column set for each entity
# under each option value. This file verifies the *dataset-level*
# constraint: when a hierarchy key or link FK is absent from a fact
# tibble (because the relevant `include_*` gate is "no"), the
# guardian's assertion must pass (belt-and-suspenders agreement with
# the reader's schema contract).

# ---- Helper: build a schema-compliant ds for a given set of opts ----

# Build a minimal neoipcr_ds where every tibble's shape matches the
# schema for the given opts. Uses the compile_schema 0-row shape for
# each tibble rather than populating rows — the guardian's assertions
# are column-presence checks, not row-count checks.
.schema_compliant_ds <- function(opts) {
  md <- read_test_metadata(dataset_options = opts)
  md$departments     <- neoipcr:::compile_schema(neoipcr:::departments_cols, opts)
  md$hospitals       <- neoipcr:::compile_schema(neoipcr:::hospitals_cols, opts)
  md$countries       <- neoipcr:::compile_schema(neoipcr:::countries_cols, opts)
  md$worldBankClasses <- neoipcr:::compile_schema(neoipcr:::worldBankClasses_cols, opts)
  md$eventTypes      <- neoipcr:::compile_schema(neoipcr:::eventTypes_cols, opts)
  md$users           <- neoipcr:::compile_schema(neoipcr:::users_cols, opts)
  md$dataset_options <- opts

  make_test_ds(
    metadata    = md,
    patients    = neoipcr:::compile_schema(neoipcr:::patients_cols, opts),
    enrollments = neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts),
    events      = neoipcr:::compile_schema(neoipcr:::events_cols, opts))
}


# ---- Hierarchy-key assertions across the full option matrix ----------
#
# For each of the four hierarchy keys, iterate every combination of
# that key's include_* gate value (no / pseudo / full). Under "no" the
# key must be absent from every fact tibble that the guardian checks;
# under "pseudo" / "full" the guardian passes.

hierarchy_keys <- list(
  list(key = "department_key",       opt = "include_department"),
  list(key = "hospital_key",         opt = "include_hospital"),
  list(key = "country_key",          opt = "include_country"),
  list(key = "world_bank_class_key", opt = "include_world_bank_class"))

for (hk in hierarchy_keys) {
  test_that(
    sprintf("assert_data_protection passes for %s under every gate value", hk$key), {
    for (val in c("no", "pseudo", "full")) {
      args <- stats::setNames(list(val), hk$opt)
      opts <- do.call(dhis2_dataset_options, args)
      ds   <- .schema_compliant_ds(opts)
      expect_error(
        neoipcr:::assert_data_protection(ds, opts), NA,
        info = sprintf("%s = %s", hk$opt, val))
    }
  })
}


# ---- Link-privacy gates (include_patient / enrollment / event) -------
#
# Under "no", the corresponding tibble is 0x0 and the link FK is absent
# from downstream tibbles. The guardian must pass.

link_gates <- c("include_patient", "include_enrollment", "include_event")

for (gate in link_gates) {
  test_that(
    sprintf("assert_data_protection passes for %s under every gate value", gate), {
    for (val in c("no", "pseudo", "full")) {
      args <- stats::setNames(list(val), gate)
      opts <- do.call(dhis2_dataset_options, args)
      ds   <- .schema_compliant_ds(opts)
      expect_error(
        neoipcr:::assert_data_protection(ds, opts), NA,
        info = sprintf("%s = %s", gate, val))
    }
  })
}


# ---- include_user gate -----------------------------------------------
#
# Under include_user = "no", entity-level user columns (createdBy,
# updatedBy, storedBy, completedBy) are absent from fact tibbles.
# Under "pseudo" / "full" they are present. The guardian's metadata-
# companion-column assertion also applies here: metadata tibbles must
# never carry them regardless of include_user.

test_that("assert_data_protection passes under include_user = 'no' / 'pseudo' / 'full'", {
  for (val in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(include_user = val)
    ds   <- .schema_compliant_ds(opts)
    expect_error(
      neoipcr:::assert_data_protection(ds, opts), NA,
      info = sprintf("include_user = %s", val))
  }
})


# ---- include_timestamps gate -----------------------------------------

test_that("assert_data_protection passes under include_timestamps = FALSE / TRUE", {
  for (val in c(FALSE, TRUE)) {
    opts <- dhis2_dataset_options(include_timestamps = val)
    ds   <- .schema_compliant_ds(opts)
    expect_error(
      neoipcr:::assert_data_protection(ds, opts), NA,
      info = sprintf("include_timestamps = %s", val))
  }
})


# ---- include_deleted gate --------------------------------------------

test_that("assert_data_protection passes under include_deleted = FALSE / TRUE", {
  for (val in c(FALSE, TRUE)) {
    opts <- dhis2_dataset_options(include_deleted = val)
    ds   <- .schema_compliant_ds(opts)
    expect_error(
      neoipcr:::assert_data_protection(ds, opts), NA,
      info = sprintf("include_deleted = %s", val))
  }
})


# ---- Most-restrictive option combination -----------------------------
#
# Everything at its most restrictive: every hierarchy gate = "no",
# every link gate = "no", include_user = "no", timestamps = FALSE,
# deleted = FALSE, dhis2_ids = empty. The ds is nearly all 0x0 tibbles.
# The guardian must pass without error.

test_that("assert_data_protection passes under maximally restrictive opts", {
  opts <- dhis2_dataset_options(
    include_department       = "no",
    include_hospital         = "no",
    include_country          = "no",
    include_world_bank_class = "no",
    include_patient          = "no",
    include_enrollment       = "no",
    include_event            = "no",
    include_user             = "no",
    include_timestamps       = FALSE,
    include_deleted          = FALSE,
    include_dhis2_ids        = character())
  ds <- .schema_compliant_ds(opts)
  expect_error(neoipcr:::assert_data_protection(ds, opts), NA)
  # Every fact tibble should be 0x0 under this config.
  expect_equal(ncol(ds$patients), 0L)
  expect_equal(ncol(ds$enrollments), 0L)
  expect_equal(ncol(ds$events), 0L)
})
