#' @include schema-cols-shared.R
NULL

# Schema declarations for org-unit-derived entities: World Bank income
# classes, countries, hospitals, departments. Loaded after
# `schema-cols-shared.R` so the cross-entity atoms (`col_wb_class_key`,
# `col_country_key`, `col_hospital_key`, `col_department_key`, `col_isTest`)
# and the inheritance helper (`col_inherited_from`) are in scope.
#
# Topological order within this file: WB classes → countries → hospitals →
# departments. A child entity's column list may reference a parent's
# compiled schema via `col_inherited_from(..., parent_cols)`, so parents
# must be declared before their children. Populated incrementally across
# the Phase B sub-tasks; each sub-task extends this file with the next
# entity rather than creating a parallel per-entity file.
#
# Internal — no `@export`.

# ---- World Bank income classes --------------------------------------------
#
# Top of the org-unit hierarchy; no parent, so no inheritance rule applies.
# Each row represents a `{class, fiscal_year}` bucket of countries per the
# World Bank income classification (L = low, LM = lower-middle,
# UM = upper-middle, H = high). The reader narrows to the most recent
# fiscal year that has data in the DHIS2 metadata.
#
# Three-mode shape progression (strict `0 → 1 → N`):
#   "no"     — 0-column, 0-row tibble. Nothing to leak, nothing to join.
#   "pseudo" — single `world_bank_class_key` column; rows = distinct keys
#              surviving upstream filtering. Consumers that want a
#              human-readable label must either render from the key alone
#              or call `pseudonymise_labels(ds, "worldBankClasses")` (to
#              land in a later Phase).
#   "full"   — `world_bank_class_key`, `class`, `fiscal_year`. The `class`
#              factor uses protocol-declared levels (fixed); `fiscal_year`
#              is an integer year.

worldBankClasses_cols <- with_entity_gate(
  list(
    col_wb_class_key,
    schema_col(
      "class", factor(),
      include_when  = \(opts) opts$include_world_bank_class == "full",
      factor_levels = c("L", "LM", "UM", "H")
    ),
    schema_col(
      "fiscal_year", integer(),
      include_when = \(opts) opts$include_world_bank_class == "full"
    )
  ),
  gate = \(opts) opts$include_world_bank_class != "no"
)

get_worldBankClasses_schema <- function(opts)
  compile_schema(worldBankClasses_cols, opts)

# ---- Countries ------------------------------------------------------------
#
# Second tier of the org-unit hierarchy. Each country belongs to exactly
# one World Bank income class (via the WB-class membership lookup), so
# `world_bank_class_key` is the direct parent-link FK — not an inherited
# key. The plan's `col_inherited_from()` helper is for hierarchy keys
# *further up* the chain that the immediate parent might or might not
# carry (e.g. patients inheriting `country_key` from departments). For a
# direct parent-child link the FK is *always* present when both sides
# exist; per plan.md's "gated on both sides of the link" rule for link
# FKs. The shared `col_wb_class_key` atom encodes the "WB side exists"
# half (its predicate is `include_world_bank_class != "no"`); the
# "countries side exists" half is supplied by the containing-entity gate
# declared below via `with_entity_gate()`.
#
# Display columns are ordered factors with data-derived levels, matching
# the current reader's `dplyr::across(!"id", ordered)` conversion. Under
# the three-mode contract the reader's `finalize_to_schema()` narrows
# any extra columns (e.g. the intermediate `country` DHIS2 id used for
# orchestrator-level joins) that aren't in the public schema.
#
# Three-mode shape:
#   "no"     — 0×0 tibble (via the entity gate's short-circuit).
#   "pseudo" — `country_key` only, plus `world_bank_class_key` when
#              `include_world_bank_class != "no"` (direct link-FK).
#   "full"   — adds `name`, `code`, `displayName`, `displayShortName`,
#              `displayDescription`.

countries_cols <- with_entity_gate(
  list(
    col_country_key,
    schema_col(
      "name", character(),
      include_when  = \(opts) opts$include_country == "full"
    ),
    schema_col(
      "code", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayName", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayShortName", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    schema_col(
      "displayDescription", ordered(),
      include_when  = \(opts) opts$include_country == "full",
      levels_source = "data"
    ),
    col_wb_class_key
  ),
  gate = \(opts) opts$include_country != "no"
)

get_countries_schema <- function(opts)
  compile_schema(countries_cols, opts)

# ---- Hospitals ------------------------------------------------------------
#
# Third tier of the org-unit hierarchy. Each hospital belongs to exactly
# one country (via the DHIS2 parent-org-unit link). Inherits
# `world_bank_class_key` from countries per the hierarchy-key
# inheritance rule: hospitals carries it directly only when the
# immediate parent (countries) doesn't. Under `include_country != "no"`,
# countries carries `world_bank_class_key` (either directly or via its
# own inheritance), so hospitals does not duplicate it — downstream
# analyses reach WB class via `hospital.country_key → country.world_bank_class_key`.
# Under `include_country = "no"`, countries is 0×0, and hospitals
# materializes `world_bank_class_key` directly to preserve
# classifiability; the orchestrator populates it using the raw
# WB-class→country membership map.
#
# Display / geometry columns are character and numeric (not factors),
# matching the current reader's passthrough (it does not ordered-cast
# display strings for hospitals).
#
# Three-mode shape:
#   "no"     — 0×0 tibble (via the entity gate).
#   "pseudo" — `hospital_key`, `orgUnit` (iff `"hospitals" %in% include_dhis2_ids`),
#              `country_key` (iff include_country != "no"), and `world_bank_class_key`
#              (by inheritance when countries doesn't carry it).
#   "full"   — adds `code`, `displayName`, `displayShortName`,
#              `displayDescription`, `comment`, `longitude`, `latitude`.

hospitals_cols <- with_entity_gate(
  list(
    col_hospital_key,
    schema_col(
      "orgUnit", character(),
      include_when = \(opts) "hospitals" %in% opts$include_dhis2_ids
    ),
    schema_col(
      "code", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayName", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayShortName", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "displayDescription", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "comment", character(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "longitude", double(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    schema_col(
      "latitude", double(),
      include_when = \(opts) opts$include_hospital == "full"
    ),
    col_country_key,
    col_inherited_from(
      "world_bank_class_key",
      "include_world_bank_class",
      countries_cols
    )
  ),
  gate = \(opts) opts$include_hospital != "no"
)

get_hospitals_schema <- function(opts)
  compile_schema(hospitals_cols, opts)

# ---- Departments ----------------------------------------------------------
#
# Bottom of the org-unit hierarchy (patients are the next layer below,
# but they belong to the fact-table side of the schema). Departments
# sits at the "fat lookup" tier: under `include_department = "full"`,
# all hierarchy keys are pre-joined in (`hospital_key`, `country_key`,
# `world_bank_class_key`) so downstream fact readers (patients,
# enrollments, events) can do a single one-hop join to reach any
# hierarchy level. This pragmatic design deviates from the strict
# `col_inherited_from()` rule used on hospitals: inheritance would say
# "departments doesn't carry `country_key` under full-chain-intact
# because hospitals reaches it via its own country_key". But existing
# downstream consumers (e.g. `R/dhis2-trackedEntities.R:151-153`) read
# `metadata$departments$country_key` directly, so the pre-join is the
# load-bearing behaviour the schema must describe, not optimise away.
# Under pseudo / no modes, the fat-lookup role collapses to just the
# link-FK and PK.
#
# `isTest` is populated by the orchestrator (computed from
# `orgUnit %in% testUnitIds`), gated by `include_test_data`. Source #1
# of the three-source isTest merge (group membership); sources #2
# (subtree) and #3 (IsTestunit attribute) land later via
# `tasks/orgunit-attributes-import.md`.
#
# Three-mode shape:
#   "no"     — 0×0 tibble (via the entity gate).
#   "pseudo" — `department_key`, `hospital_key` (link FK when hospital
#              != "no"), `isTest` (when include_test_data = TRUE). Plus
#              `orgUnit` if "departments" is in include_dhis2_ids.
#   "full"   — adds display / geometry / openingDate columns AND the
#              pre-joined hierarchy keys (`country_key`,
#              `world_bank_class_key`) for downstream one-hop access.

departments_cols <- with_entity_gate(
  list(
    col_department_key,
    schema_col(
      "orgUnit", character(),
      include_when = \(opts) "departments" %in% opts$include_dhis2_ids
    ),
    schema_col(
      "code", character(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "displayName", character(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "displayShortName", character(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "displayDescription", character(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "comment", character(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "openingDate", as.Date(character()),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "longitude", double(),
      include_when = \(opts) opts$include_department == "full"
    ),
    schema_col(
      "latitude", double(),
      include_when = \(opts) opts$include_department == "full"
    ),
    col_hospital_key,
    # Pre-joined hierarchy keys under full mode (see module note above).
    schema_col(
      "country_key", integer(),
      include_when = \(opts)
        opts$include_country != "no" && opts$include_department == "full"
    ),
    schema_col(
      "world_bank_class_key", integer(),
      include_when = \(opts)
        opts$include_world_bank_class != "no" &&
        opts$include_department == "full"
    ),
    col_isTest
  ),
  gate = \(opts) opts$include_department != "no"
)

get_departments_schema <- function(opts)
  compile_schema(departments_cols, opts)

# ---- Users ----------------------------------------------------------------
#
# Users sit outside the org-unit hierarchy but are metadata curated by the
# NeoIPC team, not by partner-site data entry. No per-attribute companion
# columns (settled decision in plan.md — "Explicitly out of scope: metadata
# tibbles"). The public three-mode shape follows the strict `0 → 1 → N`
# progression:
#
#   "no"     — 0×0 (via the entity gate).
#   "pseudo" — `user_key`, plus `user` when `"users" %in%
#              include_dhis2_ids` (same orthogonal id-opt-in axis as
#              hospitals' / departments' `orgUnit`). Content columns
#              (`username`, `firstName`, `email`, …) stay absent.
#              Downstream fact readers substitute `createdBy` /
#              `updatedBy` / etc. via the orchestrator-internal
#              `.users_internal_map` rather than through
#              `metadata$users`, so the pseudo shape can omit
#              `username` without breaking the FK-resolution path.
#   "full"   — `user_key`, `user` (when id-opt-in), `username`,
#              `firstName`, `surname`, `email`, `lastLogin`, `created`.
#
# Fallback path (`read_user_info_table`, fires when the caller lacks
# `F_USER_VIEW` / `F_METADATA_EXPORT` / `ALL`): produces the same public
# shape, populated with only the calling user's row. The reader must emit
# a schema-conformant tibble in both paths — tail `assert_schema()`
# enforces it at the orchestrator boundary.

users_cols <- with_entity_gate(
  list(
    col_user_key,
    schema_col(
      "user", character(),
      # `user` is the raw DHIS2 user id — an opaque UID, not a
      # human-identifying string. Gated only on
      # `"users" %in% include_dhis2_ids`, same as `orgUnit` on hospitals
      # and departments. The DHIS2-id opt-in is an orthogonal axis to
      # the `include_user` no/pseudo/full mode axis: the mode axis
      # controls content (names, email, lastLogin, ...), while
      # include_dhis2_ids controls exposure of the raw DHIS2 id. Under
      # pseudo + `"users"` in include_dhis2_ids the tibble carries
      # `user_key + user` — the DHIS2 id by itself identifies nothing
      # outside DHIS2 (in contrast to `username`, which IS a public
      # identifier and stays gated by `include_user == "full"`).
      include_when = \(opts) "users" %in% opts$include_dhis2_ids
    ),
    schema_col(
      "username", character(),
      include_when = \(opts) opts$include_user == "full"
    ),
    schema_col(
      "firstName", character(),
      include_when = \(opts) opts$include_user == "full"
    ),
    schema_col(
      "surname", character(),
      include_when = \(opts) opts$include_user == "full"
    ),
    schema_col(
      "email", character(),
      include_when = \(opts) opts$include_user == "full"
    ),
    schema_col(
      "lastLogin", as.POSIXct(character()),
      include_when = \(opts) opts$include_user == "full"
    ),
    schema_col(
      "created", as.POSIXct(character()),
      include_when = \(opts) opts$include_user == "full"
    )
  ),
  gate = \(opts) opts$include_user != "no"
)

get_users_schema <- function(opts)
  compile_schema(users_cols, opts)

# ---- Event types ----------------------------------------------------------
#
# Event types is the protocol-fixed list of 7 program stages
# (`adm` / `pro` / `bsi` / `nec` / `ssi` / `hap` / `end`). Not
# privacy-sensitive — the set is public domain knowledge — so there is no
# dedicated `include_event_type` gate and no entity-gate short-circuit:
# the tibble is always present. `include_dhis2_ids == "event_types"`
# controls only whether the DHIS2 `programStage` UID is exposed, same
# two-axis pattern as hospitals' / departments' `orgUnit` and users'
# `user`.
#
# Fact readers (`read_events()` in particular) need `event_type_key +
# programStage` regardless of the id-opt-in, because the reader
# substitutes the raw `programStage` into `event_type_key` during
# import. The two-column FK-resolution lookup travels on the
# orchestrator-internal `.eventTypes_internal_map` — same pattern as
# `.users_internal_map` — so absence of `programStage` from the public
# schema doesn't break the internal substitution.
#
# Shape:
#   default                              — `event_type_key`, `name`,
#                                          `displayName`, `displayFormName`,
#                                          `displayDescription`.
#   `"event_types"` in include_dhis2_ids — same, plus `programStage`
#                                          (between `event_type_key` and
#                                          `name` per the reader's
#                                          pre-schema relocate).

eventTypes_cols <- list(
  schema_col(
    "event_type_key", factor(),
    factor_levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"),
    levels_source = "fixed"
  ),
  schema_col(
    "programStage", character(),
    include_when = \(opts) "event_types" %in% opts$include_dhis2_ids
  ),
  schema_col(
    "name", factor(),
    factor_levels = c(
      "Admission", "Surgical Procedure", "Primary Sepsis/BSI",
      "Necrotizing enterocolitis", "Surgical Site Infection",
      "Pneumonia", "Surveillance-End"),
    levels_source = "fixed"
  ),
  schema_col(
    "displayName", factor(),
    factor_levels = character(),
    levels_source = "data"
  ),
  schema_col(
    "displayFormName", factor(),
    factor_levels = character(),
    levels_source = "data"
  ),
  schema_col(
    "displayDescription", character()
  )
)

get_eventTypes_schema <- function(opts)
  compile_schema(eventTypes_cols, opts)
