#' @include schema-cols-shared.R schema-patients.R
NULL

# Schema declarations for enrollments.
#
# Second fact-layer entity. Follows the three-mode `include_enrollment`
# contract:
#   "no"     — 0×0 (via the entity gate).
#   "pseudo" — strictly `enrollment_key` only.
#   "full"   — `enrollment_key`, `enrollment` (id-opt-in via
#              `"enrollments" %in% include_dhis2_ids`), `patient_key`
#              (link FK to patients, gated on `include_patient`),
#              `enrolledAt`, `followUp`, `status` (gated on
#              `"enrollments" %in% include_incomplete`), entity-level
#              user fields (`createdBy`, `updatedBy`, `completedBy`,
#              `storedBy`), entity-level timestamps (`occurredAt`,
#              `createdAt`, `createdAtClient`, `updatedAt`,
#              `updatedAtClient`, `completedAt`), `deleted`, and the
#              hierarchy keys (`department_key`, `hospital_key`,
#              `country_key`, `world_bank_class_key`, `isTest`) via
#              direct materialization — see
#              `enrollment_hierarchy_col()` below for the fat-lookup
#              rationale.
#
# Unlike patients, enrollments have no per-attribute data elements
# with independent user/timestamp attribution — every user and
# timestamp field is entity-level. So there is no
# `enrollment_attribute_cols()` wrapper; atoms are declared directly.
# DHIS2's Enrollment.java carries both `storedBy` (client-asserted
# string) and `createdBy` / `updatedBy` (server-authenticated User
# objects) at the entity level — see
# `docs/dhis2-user-timestamp-semantics.md` for the source-backed
# table. All user fields are gated by `include_user != "no"`; all
# timestamps are gated by `isTRUE(include_timestamps)`.
#
# Every non-PK atom predicate ANDs against `include_enrollment ==
# "full"` so pseudo mode narrows strictly to `enrollment_key`. The
# entity gate closes "no" mode.

# Direct-materialization helper for enrollment hierarchy keys.
#
# Mirrors the "fat departments" deviation from phase-b-departments
# (direct materialization under the option's own gate, not strict
# inheritance). Rationale: `calc-api.R` and adjacent downstream
# consumers do one-hop joins from enrollments to hierarchy metadata
# tibbles via `enrollments$<hierarchy>_key` (e.g.
# `get_countries_with_wb_class()` at calc-api.R:894 joins
# `enrollments` with `metadata$countries` on `country_key`). Strict
# inheritance via `patients_cols` would remove these keys under the
# full-chain case, silently breaking every such consumer.
#
# The key still goes away under `include_<X> = "no"` (removed from
# every tibble) and `include_enrollment != "full"` (pseudo
# enrollments strictly 1-col).
enrollment_hierarchy_col <- function(col_name, opts_key, type = integer())
{
  schema_col(
    col_name, type,
    include_when = \(opts)
      opts$include_enrollment == "full" &&
      opts[[opts_key]] != "no"
  )
}

enrollments_cols <- with_entity_gate(
  list(
    col_enrollment_key,

    # Raw DHIS2 enrollment id — id-opt-in axis.
    schema_col(
      "enrollment", character(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             "enrollments" %in% opts$include_dhis2_ids
    ),

    # Link FK to patients. Gated by both sides: enrollments exists
    # AND patients exists. Under pseudo-enrollment, the link is
    # dropped (strict 1-col progression).
    schema_col(
      "patient_key", integer(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             opts$include_patient != "no"
    ),

    # Entity-level payload.
    schema_col(
      "enrolledAt", as.Date(character()),
      include_when = \(opts) opts$include_enrollment == "full"
    ),
    schema_col(
      "followUp", logical(),
      include_when = \(opts) opts$include_enrollment == "full"
    ),
    # `status` is protocol-fixed with three levels. Only present when
    # the caller opted in to incomplete enrollments; otherwise every
    # row is COMPLETED by construction (API filter), and the column
    # is omitted.
    schema_col(
      "status", factor(),
      factor_levels = c("ACTIVE", "COMPLETED", "CANCELLED"),
      include_when  = \(opts) opts$include_enrollment == "full" &&
                              "enrollments" %in% opts$include_incomplete
    ),

    # Entity-level user fields. All four are gated by
    # `include_user != "no"`. `completedBy` is only populated by DHIS2
    # for COMPLETED enrollments; the schema carries it unconditionally
    # under "full" + user access — rows that aren't completed just
    # have NA user_key there.
    schema_col(
      "createdBy", integer(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             opts$include_user != "no"
    ),
    schema_col(
      "updatedBy", integer(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             opts$include_user != "no"
    ),
    schema_col(
      "completedBy", integer(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             opts$include_user != "no"
    ),
    schema_col(
      "storedBy", integer(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             opts$include_user != "no"
    ),

    # Entity-level timestamps.
    schema_col(
      "occurredAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),
    schema_col(
      "createdAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),
    schema_col(
      "createdAtClient", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),
    schema_col(
      "updatedAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),
    schema_col(
      "updatedAtClient", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),
    schema_col(
      "completedAt", as.POSIXct(character()),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_timestamps)
    ),

    # Deletion marker.
    schema_col(
      "deleted", logical(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_deleted)
    ),

    # Hierarchy keys via direct materialization (see
    # `enrollment_hierarchy_col` above for the fat-lookup rationale).
    enrollment_hierarchy_col("department_key",       "include_department"),
    enrollment_hierarchy_col("hospital_key",         "include_hospital"),
    enrollment_hierarchy_col("country_key",          "include_country"),
    enrollment_hierarchy_col("world_bank_class_key",
                             "include_world_bank_class"),

    # `isTest` — populated by the legacy reader from the departments
    # lookup when `include_test_data = TRUE`. Direct materialization
    # on the same fat-lookup rationale.
    schema_col(
      "isTest", logical(),
      include_when = \(opts) opts$include_enrollment == "full" &&
                             isTRUE(opts$include_test_data)
    )
  ),
  gate = \(opts) opts$include_enrollment != "no"
)

get_enrollments_schema <- function(opts) compile_schema(enrollments_cols, opts)
