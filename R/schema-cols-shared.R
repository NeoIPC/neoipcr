#' @include schema-tools.R
NULL

# Shared schema_col declarations that appear in more than one entity's
# schema. Per-entity files (R/schema-orgunits.R, R/schema-patients.R,
# …) reference these atoms directly. When the "presence rule" for a
# shared column changes (new option value, new privacy semantic), it
# changes once here.
#
# Topological load note: child entity files consume these atoms via
# `col_inherited_from(..., parent_cols)`, which closes over the parent
# column list at the time it is defined. Parent schema files therefore
# load before child schema files at package load time. This file
# (`schema-cols-shared.R`) must load before every `schema-<domain>.R`.
#
# Internal — no `@export`.

# ---- Link keys ------------------------------------------------------------
#
# Every entity on the "partner-site data" side of the pipeline carries a
# primary key column, and entities further down carry the parent's primary
# key as a foreign key. These shared declarations are the "this entity
# exists / this link exists" gates.

col_patient_key    <- schema_col(
  "patient_key",    integer(),
  \(opts) opts$include_patient    != "no"
)

col_enrollment_key <- schema_col(
  "enrollment_key", integer(),
  \(opts) opts$include_enrollment != "no"
)

col_event_key      <- schema_col(
  "event_key",      integer(),
  \(opts) opts$include_event      != "no"
)

# ---- Hierarchy keys -------------------------------------------------------
#
# Present on an entity when the corresponding metadata tibble exists
# (`include_X != "no"`). The inheritance rule (`col_inherited_from`) is
# what decides whether a downstream fact tibble materializes the key
# directly or reaches it via a one-hop join through its parent.

col_department_key <- schema_col(
  "department_key",       integer(),
  \(opts) opts$include_department != "no"
)

col_hospital_key   <- schema_col(
  "hospital_key",         integer(),
  \(opts) opts$include_hospital   != "no"
)

col_country_key    <- schema_col(
  "country_key",          integer(),
  \(opts) opts$include_country    != "no"
)

col_wb_class_key   <- schema_col(
  "world_bank_class_key", integer(),
  \(opts) opts$include_world_bank_class != "no"
)

# ---- User key -------------------------------------------------------------
#
# Users sit alongside the hierarchy-metadata entities. `user_key` is the
# public pseudonymous key; the username→user_key lookup needed by fact
# readers (for `createdBy` / `updatedBy` / `storedBy` / `completedBy`
# substitution) is carried by the orchestrator-internal `.users_internal_map`
# so the public `metadata$users` can honour the strict `0 → 1 → N`
# progression without exposing `username` in pseudo mode.

col_user_key       <- schema_col(
  "user_key",             integer(),
  \(opts) opts$include_user != "no"
)

# ---- isTest flag ----------------------------------------------------------
#
# Present when the caller asked for test departments alongside real ones
# (`include_test_data = TRUE`). The `isTest` column distinguishes the two
# on the downstream fact tables that carry it. Propagation to fact tibbles
# follows the hierarchy-inheritance rule.

col_isTest <- schema_col(
  "isTest", logical(),
  \(opts) isTRUE(opts$include_test_data)
)

# ---- Inheritance helper ---------------------------------------------------
#
# Emit a schema_col that is present on the child entity only when its
# immediate parent's compiled schema does NOT already carry the column
# under the same `opts`. This is how the plan's "redundant foreign keys"
# principle is realized without exploding column counts: each tibble
# materializes a hierarchy key only where the one-hop chain to it breaks.
#
# col_name     — the column the child may or may not carry (e.g.
#                "hospital_key").
# opts_key     — the option field controlling whether the column can
#                exist at all (e.g. "include_hospital").
# parent_cols  — the parent entity's column list (e.g. `departments_cols`).
#                Must be defined before this helper is called.
# type         — the column's R type; defaults to integer() (keys).
col_inherited_from <- function(col_name, opts_key, parent_cols,
                               type = integer())
{
  schema_col(
    col_name,
    type,
    include_when = \(opts)
      opts[[opts_key]] != "no" &&
      !(col_name %in% names(compile_schema(parent_cols, opts)))
  )
}

# ---- Per-attribute companion columns --------------------------------------
#
# DHIS2 tracker entities carry two different user-attribution claims
# (client-asserted `storedBy` vs server-authenticated `createdBy`) and a
# pair of server-assigned timestamps (`createdAt` / `updatedAt`). Each
# user-entered attribute on an event, enrollment, or event-data value
# picks up companion columns that mirror these claims, gated jointly by
# the attribute's own inclusion predicate and by `include_user` /
# `include_timestamps`.
#
# See [docs/dhis2-user-timestamp-semantics.md] for the source-backed
# explanation of why `_storedBy` and `_createdBy` are distinct signals
# (and why per-TEA attributes need a different variant below).
#
# Usage: wrap a base attribute column together with its inclusion
# predicate; the helper returns a flat list of all the columns to splice
# into the entity's `*_cols` list.

attribute_cols <- function(base_col, base_when)
{
  name <- base_col$name
  list(
    base_col,
    schema_col(
      paste0(name, "_storedBy"),  integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_createdBy"), integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_updatedBy"), integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_createdAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    ),
    schema_col(
      paste0(name, "_updatedAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    )
  )
}

# Per-TEA-attribute variant — used on patients (tracked-entity attributes).
# DHIS2's `Attribute.java` carries `createdAt` / `updatedAt` timestamps but
# no per-attribute `createdBy` / `updatedBy` User objects, so `_storedBy`
# is the only user-attribution companion at this level.
tea_attribute_cols <- function(base_col, base_when)
{
  name <- base_col$name
  list(
    base_col,
    schema_col(
      paste0(name, "_storedBy"),  integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_createdAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    ),
    schema_col(
      paste0(name, "_updatedAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    )
  )
}

# Per-event-data-element companion columns. Used on the seven
# per-event-type data tibbles (`admissionData`, `surveillanceEndData`,
# `sepsisData`, `necData`, `pneumoniaData`, `surgeryData`, `ssiData`).
#
# DHIS2's tracker `DataValue.java` carries all five audit fields on
# every data value: `createdAt`, `updatedAt`, `storedBy` (String),
# `createdBy` (User, fetched as `createdBy[username]`), `updatedBy`
# (User, fetched as `updatedBy[username]`). Before phase-b-event-details
# neoipcr only fetched three of them (`createdBy`, `createdAt`,
# `updatedAt`); `storedBy` and `updatedBy` were latent drops (analogous
# to the `isTest` / `createdAtClient` / `completedBy` fixes in other
# entities). This wrapper now declares five companions per DE —
# `_storedBy`, `_createdBy`, `_updatedBy` gated by `include_user`;
# `_createdAt`, `_updatedAt` gated by `include_timestamps`. The matching
# request + reader extensions land in `dhis2-events.R`.
event_data_attribute_cols <- function(base_col, base_when)
{
  name <- base_col$name
  list(
    base_col,
    schema_col(
      paste0(name, "_storedBy"), integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_createdBy"), integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_updatedBy"), integer(),
      \(opts) base_when(opts) && opts$include_user != "no"
    ),
    schema_col(
      paste0(name, "_createdAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    ),
    schema_col(
      paste0(name, "_updatedAt"), as.POSIXct(character()),
      \(opts) base_when(opts) && isTRUE(opts$include_timestamps)
    )
  )
}
