# Tests for R/schema-patients.R — patients_cols and patient_attribute_cols.

# ---- patient_attribute_cols wrapper ---------------------------------------

test_that("patient_attribute_cols emits 4 atoms for trackable attrs (base + _storedBy + _createdAt + _updatedAt)", {
  cols <- neoipcr:::patient_attribute_cols("sex", factor(),
    factor_levels = character(), levels_source = "data")
  expect_length(cols, 4L)
  expect_identical(
    purrr::map_chr(cols, "name"),
    c("sex", "sex_storedBy", "sex_createdAt", "sex_updatedAt"))
})

test_that("patient_attribute_cols(trackable = FALSE) emits base atom only", {
  cols <- neoipcr:::patient_attribute_cols(
    "inactive", logical(), trackable = FALSE)
  expect_length(cols, 1L)
  expect_identical(purrr::map_chr(cols, "name"), "inactive")
})

test_that("patient_attribute_cols gates every atom by include_patient == 'full'", {
  cols <- neoipcr:::patient_attribute_cols("sex", factor(),
    factor_levels = character(), levels_source = "data")
  # Open every orthogonal companion-column gate so the for-loop
  # iterates under a full-permissive opts. The companions have
  # *additional* predicates (include_user for _storedBy,
  # include_timestamps for the two timestamps); those are tested
  # separately below.
  opts_no     <- dhis2_dataset_options(include_patient    = "no",
                                       patient_columns    = "sex",
                                       include_user       = "full",
                                       include_timestamps = TRUE)
  opts_pseudo <- dhis2_dataset_options(include_patient    = "pseudo",
                                       patient_columns    = "sex",
                                       include_user       = "full",
                                       include_timestamps = TRUE)
  opts_full   <- dhis2_dataset_options(include_patient    = "full",
                                       patient_columns    = "sex",
                                       include_user       = "full",
                                       include_timestamps = TRUE)
  for (c in cols) {
    expect_false(c$include_when(opts_no))
    expect_false(c$include_when(opts_pseudo))
    expect_true(c$include_when(opts_full))
  }
})

test_that("patient_attribute_cols gates each atom by patient_columns membership", {
  cols <- neoipcr:::patient_attribute_cols("sex", factor(),
    factor_levels = character(), levels_source = "data")
  opts_with    <- dhis2_dataset_options(include_patient    = "full",
                                        patient_columns    = "sex",
                                        include_user       = "full",
                                        include_timestamps = TRUE)
  opts_without <- dhis2_dataset_options(include_patient    = "full",
                                        patient_columns    = "birth_weight",
                                        include_user       = "full",
                                        include_timestamps = TRUE)
  for (c in cols) {
    expect_true(c$include_when(opts_with))
    expect_false(c$include_when(opts_without))
  }
})

test_that("patient_id maps to 'id' patient_columns key (legacy naming)", {
  cols <- neoipcr:::patient_attribute_cols(
    "patient_id", character(), patient_columns_key = "id")
  opts_with    <- dhis2_dataset_options(include_patient = "full",
                                        patient_columns = "id")
  opts_without <- dhis2_dataset_options(include_patient = "full",
                                        patient_columns = "birth_weight")
  expect_true(cols[[1]]$include_when(opts_with))
  expect_false(cols[[1]]$include_when(opts_without))
})

test_that("patient_id survives via also_when when include_invalid_patients is a list", {
  # `transform_user_exceptions()` needs `patients$patient_id` to match
  # caller-supplied IDs in `include_invalid_patients`. Schema must
  # preserve the column under that opts combination regardless of
  # `patient_columns` membership.
  opts_id_via_list <- dhis2_dataset_options(
    include_patient          = "full",
    patient_columns          = character(),
    include_invalid_patients = c("PAT_1", "PAT_2"))
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts_id_via_list)
  expect_true("patient_id" %in% names(schema))

  # Boolean TRUE / FALSE does not trigger the escape hatch — only a
  # multi-element character vector does. Under boolean TRUE the
  # validator doesn't need patient IDs for matching.
  opts_id_true_bool <- dhis2_dataset_options(
    include_patient          = "full",
    patient_columns          = character(),
    include_invalid_patients = TRUE)
  schema_bool <- neoipcr:::compile_schema(
    neoipcr:::patients_cols, opts_id_true_bool)
  expect_false("patient_id" %in% names(schema_bool))
})

test_that("total_gestation_days pairs with gest_age (same patient_columns key)", {
  cols <- neoipcr:::patient_attribute_cols(
    "total_gestation_days", integer(), patient_columns_key = "gestational_age")
  opts_ga   <- dhis2_dataset_options(include_patient = "full",
                                     patient_columns = "gestational_age")
  opts_none <- dhis2_dataset_options(include_patient = "full",
                                     patient_columns = "sex")
  expect_true(cols[[1]]$include_when(opts_ga))
  expect_false(cols[[1]]$include_when(opts_none))
})

test_that("_storedBy companion additionally requires include_user != 'no'", {
  cols <- neoipcr:::patient_attribute_cols("sex", factor(),
    factor_levels = character(), levels_source = "data")
  storedBy <- cols[[2]]
  expect_identical(storedBy$name, "sex_storedBy")

  base_open <- list(include_patient = "full", patient_columns = "sex")
  expect_false(storedBy$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_user = "no")))))
  expect_true(storedBy$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_user = "pseudo")))))
  expect_true(storedBy$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_user = "full")))))
})

test_that("_createdAt / _updatedAt companions additionally require include_timestamps", {
  cols <- neoipcr:::patient_attribute_cols("sex", factor(),
    factor_levels = character(), levels_source = "data")
  createdAt <- cols[[3]]
  updatedAt <- cols[[4]]

  base_open <- list(include_patient = "full", patient_columns = "sex")
  expect_false(createdAt$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_timestamps = FALSE)))))
  expect_true(createdAt$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_timestamps = TRUE)))))
  expect_false(updatedAt$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_timestamps = FALSE)))))
  expect_true(updatedAt$include_when(
    do.call(dhis2_dataset_options, c(base_open, list(include_timestamps = TRUE)))))
})

# ---- patients_cols three-mode shape ---------------------------------------

test_that("patients_cols: 'no' mode returns 0x0 via the entity gate", {
  opts <- dhis2_dataset_options(include_patient = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_s3_class(schema, "tbl_df")
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("patients_cols: 'pseudo' mode is strictly patient_key only", {
  opts <- dhis2_dataset_options(
    include_patient = "pseudo",
    # Even with every orthogonal opt-in set, pseudo stays 1-col —
    # compound predicates on every non-PK atom AND against
    # include_patient == "full".
    patient_columns          = c("id", "sex", "birth_weight"),
    include_dhis2_ids        = "patients",
    include_user             = "full",
    include_timestamps       = TRUE,
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_identical(names(schema), "patient_key")
})

test_that("patients_cols: 'full' + empty patient_columns = patient_key + department_key", {
  # No attribute in `patient_columns` → no attribute or companion
  # columns appear. No user/timestamp opts → no entity-level companion
  # columns. Under include_department = "full" (inheritance), departments
  # carries the hierarchy keys → patients doesn't materialize them.
  opts <- dhis2_dataset_options(
    include_patient    = "full",
    patient_columns    = character(),
    include_department = "full",
    include_hospital   = "full",
    include_country    = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_identical(names(schema), c("patient_key", "department_key"))
})

test_that("patients_cols: trackedEntity gated on include_dhis2_ids", {
  opts_no <- dhis2_dataset_options(
    include_patient   = "full",
    include_dhis2_ids = character())
  opts_yes <- dhis2_dataset_options(
    include_patient   = "full",
    include_dhis2_ids = "patients")
  expect_false("trackedEntity" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_no)))
  expect_true("trackedEntity" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_yes)))
})

test_that("patients_cols: entity-level storedBy/createdBy/updatedBy gated on include_user", {
  base <- list(include_patient = "full")
  no_user <- do.call(dhis2_dataset_options, c(base, list(include_user = "no")))
  with_user <- do.call(dhis2_dataset_options,
                       c(base, list(include_user = "full")))
  # `storedBy` added in phase-b-event-details (latent-drop symmetric
  # with enrollments + events — TrackedEntity.java carries it at
  # entity level; previously only per-TEA storedBy was captured).
  user_cols <- c("storedBy", "createdBy", "updatedBy")
  for (col in user_cols) {
    expect_false(col %in% names(
      neoipcr:::compile_schema(neoipcr:::patients_cols, no_user)), info = col)
    expect_true(col %in% names(
      neoipcr:::compile_schema(neoipcr:::patients_cols, with_user)), info = col)
  }
})

test_that("patients_cols: entity-level timestamps gated on include_timestamps", {
  base <- list(include_patient = "full")
  no_ts <- do.call(dhis2_dataset_options,
                   c(base, list(include_timestamps = FALSE)))
  with_ts <- do.call(dhis2_dataset_options,
                     c(base, list(include_timestamps = TRUE)))
  ts_cols <- c("createdAt", "createdAtClient", "updatedAt", "updatedAtClient")
  for (col in ts_cols) {
    expect_false(col %in% names(
      neoipcr:::compile_schema(neoipcr:::patients_cols, no_ts)))
    expect_true(col %in% names(
      neoipcr:::compile_schema(neoipcr:::patients_cols, with_ts)))
  }
})

test_that("patients_cols: deleted gated on include_deleted", {
  opts_off <- dhis2_dataset_options(
    include_patient = "full", include_deleted = FALSE)
  opts_on  <- dhis2_dataset_options(
    include_patient = "full", include_deleted = TRUE)
  expect_false("deleted" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_off)))
  expect_true("deleted" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_on)))
})

# ---- Hierarchy-key inheritance --------------------------------------------
#
# The inheritance rule: patients carries a hierarchy key directly only
# when the parent (`departments_cols`) doesn't carry it. Under the
# fat-lookup departments design, `include_department = "full"` puts the
# full hierarchy on departments → patients reaches them via one-hop.
# Under narrower department modes, patients materializes the keys
# directly.

test_that("inheritance: hospital_key on patients iff departments doesn't have it", {
  # include_department = "full" + include_hospital = "full" → departments
  # has hospital_key → patients does NOT.
  opts_full_full <- dhis2_dataset_options(
    include_patient    = "full",
    include_department = "full",
    include_hospital   = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts_full_full)
  expect_false("hospital_key" %in% names(schema))

  # include_department = "no" + include_hospital = "full" → departments
  # is 0×0 → patients materializes hospital_key directly.
  opts_no_full <- dhis2_dataset_options(
    include_patient    = "full",
    include_department = "no",
    include_hospital   = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts_no_full)
  expect_true("hospital_key" %in% names(schema))
})

test_that("inheritance: country_key / world_bank_class_key follow the same rule", {
  # Under full-department, departments carries country_key +
  # world_bank_class_key (via the fat-lookup direct-materialization
  # predicate) → patients doesn't.
  opts_full <- dhis2_dataset_options(
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts_full)
  expect_false("country_key" %in% names(schema))
  expect_false("world_bank_class_key" %in% names(schema))

  # Under pseudo-department, departments has only department_key +
  # hospital_key → patients materializes country_key and
  # world_bank_class_key directly.
  opts_pseudo_dept <- dhis2_dataset_options(
    include_patient          = "full",
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts_pseudo_dept)
  expect_true("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

# ---- isTest ----------------------------------------------------------------

test_that("patients_cols: isTest gated on include_test_data (full mode only)", {
  # Present only under include_patient = "full" + include_test_data = TRUE,
  # populated by the reader from the departments fat-lookup — same pattern
  # as enrollments/events.
  opts_on <- dhis2_dataset_options(
    include_patient   = "full",
    include_test_data = TRUE)
  expect_true("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_on)))

  opts_off <- dhis2_dataset_options(
    include_patient   = "full",
    include_test_data = FALSE)
  expect_false("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_off)))

  # Pseudo mode stays strictly patient_key even with test data on.
  opts_pseudo <- dhis2_dataset_options(
    include_patient   = "pseudo",
    include_test_data = TRUE)
  expect_false("isTest" %in% names(
    neoipcr:::compile_schema(neoipcr:::patients_cols, opts_pseudo)))
})

# ---- Per-TEA companion columns (DHIS2 Attribute.java semantics) ------------

test_that("patients_cols emits no _createdBy / _updatedBy on TEA attributes", {
  # Per docs/dhis2-user-timestamp-semantics.md: Attribute.java has no
  # per-attribute createdBy/updatedBy User objects. `tea_attribute_cols`
  # encodes this by emitting only `_storedBy`, `_createdAt`,
  # `_updatedAt`. Regression guard.
  declared <- purrr::map_chr(neoipcr:::patients_cols, "name")
  per_tea_attrs <- c(
    "patient_id", "sex", "birth_weight", "gest_age",
    "total_gestation_days", "delivery_mode", "siblings")
  for (attr in per_tea_attrs) {
    expect_false(paste0(attr, "_createdBy") %in% declared,
      info = sprintf("%s_createdBy must not be declared", attr))
    expect_false(paste0(attr, "_updatedBy") %in% declared,
      info = sprintf("%s_updatedBy must not be declared", attr))
    expect_true(paste0(attr, "_storedBy") %in% declared,
      info = sprintf("%s_storedBy must be declared", attr))
    expect_true(paste0(attr, "_createdAt") %in% declared,
      info = sprintf("%s_createdAt must be declared", attr))
    expect_true(paste0(attr, "_updatedAt") %in% declared,
      info = sprintf("%s_updatedAt must be declared", attr))
  }
})

test_that("patients_cols: inactive / potentialDuplicate have no companion columns", {
  declared <- purrr::map_chr(neoipcr:::patients_cols, "name")
  non_tea  <- c("inactive", "potentialDuplicate")
  for (attr in non_tea) {
    expect_true(attr %in% declared)
    expect_false(paste0(attr, "_storedBy")  %in% declared)
    expect_false(paste0(attr, "_createdAt") %in% declared)
    expect_false(paste0(attr, "_updatedAt") %in% declared)
  }
})

# ---- Fixture round-trip ---------------------------------------------------

test_that("make_test_patients output matches patients_cols schema (full mode, all attrs)", {
  opts <- dhis2_dataset_options(
    include_patient    = "full",
    patient_columns    = c("id", "sex", "birth_weight", "gestational_age",
                           "delivery_mode", "siblings"),
    include_dhis2_ids  = "patients",
    include_user       = "full",
    include_timestamps = TRUE,
    include_department = "pseudo",
    include_hospital   = "pseudo",
    include_country    = "pseudo",
    include_world_bank_class = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  fixture <- make_test_patients(
    n = 2,
    include_patient    = "full",
    patient_columns    = c("id", "sex", "birth_weight", "gestational_age",
                           "delivery_mode", "siblings"),
    include_dhis2_ids  = "patients",
    include_user       = "full",
    include_timestamps = TRUE,
    include_department = "pseudo",
    include_hospital   = "pseudo",
    include_country    = "pseudo",
    include_world_bank_class = "pseudo")
  expect_schema_matches(fixture, schema)
})

test_that("make_test_patients output matches patients_cols schema (pseudo mode)", {
  opts <- dhis2_dataset_options(include_patient = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  fixture <- make_test_patients(n = 2, include_patient = "pseudo")
  expect_schema_matches(fixture, schema)
  expect_identical(names(fixture), "patient_key")
})

test_that("make_test_patients carries isTest under include_test_data (round-trip)", {
  # Pin every hierarchy mode to make_test_patients' defaults ("pseudo") so
  # the compiled schema and the fixture agree on the inherited keys, and the
  # test isolates the isTest addition.
  opts <- dhis2_dataset_options(
    include_patient          = "full",
    patient_columns          = "id",
    include_dhis2_ids        = "patients",
    include_test_data        = TRUE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  expect_true("isTest" %in% names(schema))
  fixture <- make_test_patients(
    n                        = 2,
    include_patient          = "full",
    patient_columns          = "id",
    include_dhis2_ids        = "patients",
    include_test_data        = TRUE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
  expect_schema_matches(fixture, schema)
})
