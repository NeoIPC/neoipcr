#' @include schema-cols-shared.R schema-orgunits.R
NULL

# Schema declarations for patients (tracked entities).
#
# First fact-layer entity. Follows the three-mode `include_patient`
# contract:
#   "no"     — 0×0 (via the entity gate).
#   "pseudo" — strictly `patient_key` only.
#   "full"   — `patient_key`, `trackedEntity` (id-opt-in), every
#              `patient_columns`-selected attribute + its per-TEA
#              companion columns (`_storedBy`, `_createdAt`,
#              `_updatedAt` — no `_createdBy` / `_updatedBy` per
#              DHIS2's `Attribute.java`, see
#              `docs/dhis2-user-timestamp-semantics.md`), entity-level
#              flags (`inactive`, `potentialDuplicate` — selected via
#              `patient_columns`), entity-level `createdBy` /
#              `updatedBy` (gated by `include_user`), entity-level
#              `createdAt` / `createdAtClient` / `updatedAt` /
#              `updatedAtClient` (gated by `include_timestamps`),
#              `deleted` (gated by `include_deleted`), `department_key`
#              link FK (gated by `include_department != "no"`),
#              the hierarchy keys (`hospital_key`, `country_key`,
#              `world_bank_class_key`) via `col_inherited_from()` —
#              materialized only when `departments_cols` doesn't
#              carry them under the current opts — and the `isTest`
#              flag (gated by `include_test_data`).
#
# Every non-PK atom predicate ANDs against `include_patient == "full"`
# so pseudo mode narrows strictly to `patient_key`. The entity gate
# closes "no" mode.

# Patient-attribute wrapper.
#
# Wraps a base schema_col with its per-TEA companion columns
# (`_storedBy`, `_createdAt`, `_updatedAt`). Every patient attribute is
# gated by both `include_patient == "full"` AND the attribute's own
# membership in `patient_columns`; companions additionally require
# `include_user != "no"` (for `_storedBy`) or `include_timestamps`
# (for the two timestamp companions) per the shared
# `tea_attribute_cols()` helper.
#
# `patient_columns_key` defaults to `name` — same key used in
# `patient_columns`. The one exception is `patient_id`, which maps to
# the `"id"` key in `patient_columns` (legacy naming). Pass
# `patient_columns_key = "id"` there.
#
# `trackable = FALSE` for entity-level flags (`inactive`,
# `potentialDuplicate`) — these are not TEAs, have no companion
# columns, and are gated only by the base predicate.
patient_attribute_cols <- function(name, type,
                                   patient_columns_key = name,
                                   factor_levels       = NULL,
                                   levels_source       = c("fixed", "data"),
                                   trackable           = TRUE,
                                   also_when           = \(opts) FALSE)
{
  levels_source <- match.arg(levels_source)
  # `also_when` is an escape hatch for attributes that must be present
  # for reasons other than `patient_columns` membership — today used
  # only by `patient_id`, which `transform_user_exceptions()` needs
  # when `include_invalid_patients` is a character vector of patient
  # IDs. Defaults to always-FALSE so unused attributes retain the
  # pure `patient_columns`-gating behaviour.
  base_when <- \(opts)
    opts$include_patient == "full" &&
    (patient_columns_key %in% opts$patient_columns ||
     isTRUE(also_when(opts)))
  base_col <- schema_col(
    name, type, base_when,
    factor_levels = factor_levels,
    levels_source = levels_source)
  if (trackable) tea_attribute_cols(base_col, base_when)
  else list(base_col)
}

# Inherited hierarchy key on patients — carried directly only when the
# parent (`departments_cols`) doesn't carry it under the current opts.
# Matches `col_inherited_from()` in schema-cols-shared.R but adds the
# `include_patient == "full"` prefix so pseudo mode stays strictly
# `patient_key`-only regardless of upstream hierarchy state.
patient_inherited_from <- function(col_name, opts_key, type = integer())
{
  schema_col(
    col_name, type,
    include_when = \(opts)
      opts$include_patient == "full" &&
      opts[[opts_key]] != "no" &&
      !(col_name %in% names(compile_schema(departments_cols, opts)))
  )
}

patients_cols <- with_entity_gate(
  c(
    list(col_patient_key),

    # Raw DHIS2 tracked-entity id — orthogonal id-opt-in axis, same
    # pattern as hospitals' / departments' `orgUnit` and users' `user`.
    list(schema_col(
      "trackedEntity", character(),
      include_when = \(opts) opts$include_patient == "full" &&
                             "patients" %in% opts$include_dhis2_ids
    )),

    # Per-TEA attributes — each wrapped with its `_storedBy` /
    # `_createdAt` / `_updatedAt` companions via `tea_attribute_cols`.
    # Types match what `convert_value()` produces for each TEA's
    # `valueType`: option-set attrs (sex, delivery_mode) become
    # factors, INTEGER_* → integer, BOOLEAN / TRUE_ONLY → logical,
    # rest → character. Factor levels come from data (option-set
    # codes in the DHIS2 metadata), so `levels_source = "data"`.
    # `patient_id` must also survive when the caller passes a character
    # vector via `include_invalid_patients` — `transform_user_exceptions()`
    # in `import_dhis2.R` needs `patients$patient_id` to match the
    # caller-supplied patient IDs. Same `also_when` escape hatch
    # propagates to the per-TEA companion columns.
    patient_attribute_cols(
      "patient_id", character(), patient_columns_key = "id",
      also_when = \(opts) length(opts$include_invalid_patients) > 1),
    patient_attribute_cols(
      "sex", factor(), factor_levels = character(),
      levels_source = "data"),
    patient_attribute_cols("birth_weight", integer()),
    patient_attribute_cols(
      "gest_age", character(), patient_columns_key = "gestational_age"),
    # `total_gestation_days` pairs with `gest_age` — both TEAs carry
    # the same datum in two shapes (text "25+4" vs integer total days),
    # kept in sync by DHIS2 program rules. Selection follows
    # `gestational_age` (the user-facing key) rather than being its own
    # `patient_columns` entry.
    patient_attribute_cols(
      "total_gestation_days", integer(), patient_columns_key = "gestational_age"),
    patient_attribute_cols(
      "delivery_mode", factor(), factor_levels = character(),
      levels_source = "data"),
    patient_attribute_cols("siblings", integer()),

    # Entity-level flags — not TEAs, no companion columns.
    patient_attribute_cols("inactive", logical(), trackable = FALSE),
    patient_attribute_cols(
      "potentialDuplicate", logical(), trackable = FALSE),

    # Entity-level user fields. `createdBy` / `updatedBy` are server-
    # authenticated User objects (fetched as `createdBy[username]`);
    # `storedBy` is a client-asserted String. All three substituted to
    # integer `user_key` via `metadata$.users_internal_map` in the
    # reader.
    list(schema_col(
      "storedBy", integer(),
      include_when = \(opts) opts$include_patient == "full" &&
                             opts$include_user != "no"
    )),
    list(schema_col(
      "createdBy", integer(),
      include_when = \(opts) opts$include_patient == "full" &&
                             opts$include_user != "no"
    )),
    list(schema_col(
      "updatedBy", integer(),
      include_when = \(opts) opts$include_patient == "full" &&
                             opts$include_user != "no"
    )),

    # Entity-level timestamps.
    list(schema_col(
      "createdAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_timestamps)
    )),
    list(schema_col(
      "createdAtClient", as.POSIXct(character()),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_timestamps)
    )),
    list(schema_col(
      "updatedAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_timestamps)
    )),
    list(schema_col(
      "updatedAtClient", as.POSIXct(character()),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_timestamps)
    )),

    # Deletion marker — only requested when the caller opts in.
    list(schema_col(
      "deleted", logical(),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_deleted)
    )),

    # Link FK to departments — compound-gated so pseudo mode has no
    # link column (strict 1-col shape). Under "full" it follows the
    # departments gate.
    list(schema_col(
      "department_key", integer(),
      include_when = \(opts) opts$include_patient == "full" &&
                             opts$include_department != "no"
    )),

    # Inherited hierarchy keys — materialized on patients only when
    # `departments_cols` doesn't carry the key under the current opts.
    # Under the fat-lookup design (`include_department == "full"`),
    # departments carries them all and patients reaches them via
    # one-hop `department_key → departments`. Under "pseudo" / "no"
    # departments modes, patients materializes them directly.
    list(patient_inherited_from("hospital_key",          "include_hospital")),
    list(patient_inherited_from("country_key",           "include_country")),
    list(patient_inherited_from("world_bank_class_key",
                                "include_world_bank_class")),

    # `isTest` — test-unit marker, populated by the reader from the
    # departments fat-lookup under `include_test_data = TRUE`, same as on
    # enrollments and events. Patients previously omitted it — an
    # asymmetry with the other fact tibbles rather than a deliberate
    # design — so downstream code had to detour through
    # `metadata$departments` to test a patient's unit. Declared directly
    # here to close that gap.
    list(schema_col(
      "isTest", logical(),
      include_when = \(opts) opts$include_patient == "full" &&
                             isTRUE(opts$include_test_data)
    ))
  ),
  gate = \(opts) opts$include_patient != "no"
)

get_patients_schema <- function(opts) compile_schema(patients_cols, opts)
