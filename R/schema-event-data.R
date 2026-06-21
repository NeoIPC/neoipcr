#' @include schema-cols-shared.R schema-events.R
NULL

# Schema declarations for the seven per-event-type data tibbles:
#   admissionData        (adm — tibble class `neoipcr_adm`)
#   surveillanceEndData  (end — tibble class `neoipcr_end`)
#   sepsisData           (bsi — tibble class `neoipcr_bsi`)
#   necData              (nec — tibble class `neoipcr_nec`)
#   pneumoniaData        (hap — tibble class `neoipcr_hap`)
#   surgeryData          (pro — tibble class `neoipcr_pro`)
#   ssiData              (ssi — tibble class `neoipcr_ssi`)
#
# Each is one pivot_wider away from `read_event_data(events_raw,
# processed_events, metadata, dataset_options, <event_type_key>)`. The
# schema contract drives the pivot's `names_from` factor via
# `schema_codes(<type>_cols, opts)` + `pivot_wider(..., names_expand =
# TRUE)`, so the full set of payload columns is present regardless of
# which data-element codes any surviving event happens to carry. This
# closes the pivot-volatility hazard that produced the `vs_days` crash
# on surveillance-end.
#
# Pathogen DEs (`NEOIPC_<stage>_PATHOGEN_<n>` + `_3GCR` / `_CAR` /
# `_COR` / `_MRSA` / `_VRE` / `_SOURCE` / `_NAME` / `_MULTIPLE`, and
# the secondary-BSI variants on NEC / HAP / SSI) are not part of any
# per-event-type tibble — they route to `infectiousAgentFindings`
# (schematized in phase-b-findings). The per-event-type readers
# currently filter them out before pivot. The schemas here declare
# only the post-filter, pre-pivot codes.
#
# `AB_SUBST_<n>` and `AB_SUBST_<n>_DAYS` codes on surveillance-end
# route to `substanceDays` (also in phase-b-findings). Same filter
# pattern; `surveillanceEndData_cols` doesn't declare them.
#
# Per-DE companion columns follow `event_data_attribute_cols()` from
# `schema-cols-shared.R` — three companions per data element
# (`_createdBy`, `_createdAt`, `_updatedAt`). DHIS2's
# `EventDataValue.java` carries more (`storedBy`, server timestamps
# plus user info blobs), but the current reader's API request only
# fetches `createdBy[username]`, `createdAt`, `updatedAt` — the
# schema mirrors the current reader output.
#
# Entity gate: every per-event-type tibble is gated on
# `include_event != "no"`. Under pseudo events (events has only
# `event_key`), per-event-type data tibbles still carry their payload
# plus `event_key` — matches the plan's "pseudo event enables group-
# by-external-classification without individual-event traceability"
# shape.
#
# Hierarchy and link keys inherit strictly from `events_cols` via
# `col_inherited_from()`: events already materializes every hierarchy
# key, `patient_key`, `enrollment_key`, and `isTest` directly under
# the fat-lookup design, so the child tibble carries them only when
# the parent schema doesn't. In practice that means the per-event-
# type tibbles do NOT materialize hierarchy / link keys directly
# under normal options — they reach them via one-hop `event_key →
# events`. Under pseudo events (events has only event_key), the
# inheritance rule kicks in and the child tibble materializes the
# hierarchy keys directly.

# ---- Internal helpers -----------------------------------------------------

# Factor levels for DHIS2 option sets used by per-event-type DEs.
# Codes come from `repos/neoipc-dhis2/dhis_metadata/metadata.json` —
# these are the option codes in `sortOrder`.
.event_data_levels <- list(
  ADMISSION_TYPE           = c("1", "2", "3"),
  ASA_SCORE                = c("1", "2", "3", "4", "5"),
  BSI_DEV_ASS              = c("0", "1", "2"),
  HAP_DEV_ASS              = c("0", "1", "2"),
  SSI_INFECTION_TYPE       = c("1", "2", "3"),
  SURV_END_REASON          = c("1", "2"),
  WOUND_CLASS              = c("1", "2", "3", "4"),
  YES_NO_NO_FOLLOWUP       = c("1", "0", "-1"),   # KfIEzWRibj7
  YES_NO_NOT_TESTED        = c("1", "0", "-1")    # TnE2yuSrqEP
)

# Build the `<type>_cols` entity-gate predicate with the compound
# `include_event` predicate applied once per entity. Each payload atom
# ANDs on `include_event == "full"` under the fat-design semantic — but
# because the entity gate covers the "no" case and pseudo needs the
# event_key + payload, the payload predicate collapses to TRUE under
# the entity gate. The wrapper therefore applies the payload predicate
# verbatim to each DE, gated only by `base_when(opts)`.

# Wrapper for a per-event-type data element. Base inclusion is always
# TRUE under the entity gate (protocol-declared columns always appear
# on the tibble when it exists); companion columns follow the shared
# `event_data_attribute_cols` helper.
event_data_col <- function(name, type,
                           factor_levels = NULL,
                           levels_source = c("fixed", "data"))
{
  levels_source <- match.arg(levels_source)
  base_when <- \(opts) TRUE
  base_col  <- schema_col(
    name, type, base_when,
    factor_levels = factor_levels,
    levels_source = levels_source)
  event_data_attribute_cols(base_col, base_when)
}

# Common link / hierarchy prefix for every per-event-type tibble.
# Link FKs and hierarchy keys inherit strictly from events_cols — they
# materialize here only when events's compiled schema doesn't already
# carry them under the current opts.
.event_data_link_cols <- function() {
  list(
    col_event_key,
    col_inherited_from("enrollment_key",      "include_enrollment",
                       events_cols),
    col_inherited_from("patient_key",         "include_patient",
                       events_cols),
    col_inherited_from("department_key",       "include_department",
                       events_cols),
    col_inherited_from("hospital_key",         "include_hospital",
                       events_cols),
    col_inherited_from("country_key",          "include_country",
                       events_cols),
    col_inherited_from("world_bank_class_key", "include_world_bank_class",
                       events_cols),
    schema_col(
      "isTest", logical(),
      include_when = \(opts)
        isTRUE(opts$include_test_data) &&
        !("isTest" %in% names(compile_schema(events_cols, opts))))
  )
}

# ---- admissionData (adm) --------------------------------------------------

admissionData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col(
      "type", factor(),
      factor_levels = .event_data_levels$ADMISSION_TYPE,
      levels_source = "fixed"),
    event_data_col("dol", integer())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- surveillanceEndData (end) --------------------------------------------
#
# `vs_days` is NOT a DHIS2 data element — it's computed post-pivot as
# `inv_days + niv_days`. Declared on the schema so downstream
# consumers (calc-rates) see a stable column; the reader materializes
# it via a `mutate()` after `finalize_to_schema` has guaranteed the
# two operand columns exist. No companion columns on vs_days (it has
# no DHIS2-side user / timestamp attribution).

surveillanceEndData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col(
      "reason", factor(),
      factor_levels = .event_data_levels$SURV_END_REASON,
      levels_source = "fixed"),
    event_data_col("patient_days",       integer()),
    event_data_col("cvc_days",           integer()),
    event_data_col("pvc_days",           integer()),
    list(schema_col(
      "vs_days", integer(),
      include_when = \(opts) TRUE,
      levels_source = "fixed")),
    event_data_col("inv_days",           integer()),
    event_data_col("niv_days",           integer()),
    event_data_col("ab_days",            integer()),
    event_data_col("human_milk_days",    integer()),
    event_data_col("kangaroo_care_days", integer()),
    event_data_col("probiotic_days",     integer())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- sepsisData (bsi) -----------------------------------------------------
#
# BSI entity-level DEs minus pathogens (which route to
# `infectiousAgentFindings`). `ab_treatment` and `no_pos_culture` are
# TRUE_ONLY in DHIS2 — convert_value() converts them to logical.
# Boolean symptom flags are BOOLEAN → logical.

sepsisData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col(
      "dev_ass", factor(),
      factor_levels = .event_data_levels$BSI_DEV_ASS,
      levels_source = "fixed"),
    event_data_col("los",                 integer()),
    event_data_col("dol",                 integer()),
    event_data_col("acidosis",            logical()),
    event_data_col("ab_treatment",        logical()),
    event_data_col("apnoea",              logical()),
    event_data_col("bradycardia",         logical()),
    event_data_col("crp",                 logical()),
    event_data_col("feeding_intolerance", logical()),
    event_data_col("hyperglycaemia",      logical()),
    event_data_col("it_ratio",            logical()),
    event_data_col("interleukin",         logical()),
    event_data_col("irritability",        logical()),
    event_data_col("no_pos_culture",      logical()),
    event_data_col("perfusion",           logical()),
    event_data_col("platelet_count",      logical()),
    event_data_col("procalcitonin",       logical()),
    event_data_col("temperature",         logical()),
    event_data_col("wbc",                 logical())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- necData (nec) --------------------------------------------------------
#
# NEC entity-level DEs minus pathogens + secondary-BSI pathogens. The
# DHIS2 code `secondary_bsi` is renamed to `sec_bsi` in the reader —
# the schema declares the post-rename name.

necData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col("los", integer()),
    event_data_col("dol", integer()),
    event_data_col(
      "sec_bsi", factor(),
      factor_levels = .event_data_levels$YES_NO_NO_FOLLOWUP,
      levels_source = "fixed"),
    event_data_col("abdominal_skin_tone",         logical()),
    event_data_col("abdominal_distension",        logical()),
    event_data_col("bilious_aspirate",            logical()),
    event_data_col("bloody_stools",               logical()),
    event_data_col("bowel_necrosis",              logical()),
    event_data_col("fixed_loop",                  logical()),
    event_data_col("gastric_residuals",           logical()),
    event_data_col("pneumatosis_intestinalis_img", logical()),
    event_data_col("pneumatosis_intestinalis_surg", logical()),
    event_data_col("pneumoperitoneum",            logical()),
    event_data_col("portal_venous_gas",           logical()),
    event_data_col("vomiting",                    logical())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- pneumoniaData (hap) --------------------------------------------------
#
# HAP entity-level DEs minus pathogens + secondary-BSI pathogens.
# Reader renames: `device_association → dev_ass`, `secondary_bsi →
# sec_bsi`. Schema declares post-rename names.

pneumoniaData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col(
      "dev_ass", factor(),
      factor_levels = .event_data_levels$HAP_DEV_ASS,
      levels_source = "fixed"),
    event_data_col("los", integer()),
    event_data_col("dol", integer()),
    event_data_col(
      "sec_bsi", factor(),
      factor_levels = .event_data_levels$YES_NO_NO_FOLLOWUP,
      levels_source = "fixed"),
    event_data_col(
      "microbiological_test_result", factor(),
      factor_levels = .event_data_levels$YES_NO_NOT_TESTED,
      levels_source = "fixed"),
    event_data_col("bradycardia",                     logical()),
    event_data_col("fever",                           logical()),
    event_data_col("imaging_findings",                logical()),
    event_data_col("increased_respiratory_secretion", logical()),
    event_data_col("laboratory_findings",             logical()),
    event_data_col("purulent_tracheal_aspirate",      logical()),
    event_data_col("respiratory_distress",            logical()),
    event_data_col("respiratory_support",             logical()),
    event_data_col("tachypnoea",                      logical())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- surgeryData (pro) ----------------------------------------------------
#
# Surgery has no pathogen filter — every DE is entity-level. Text
# fields (procedure_description, main/side procedure codes,
# infection_signs) are character. `asa_score` is INTEGER_POSITIVE
# with an option set → factor. `wound_class` same pattern.

surgeryData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col("los",                   integer()),
    event_data_col("dol",                   integer()),
    event_data_col("procedure_description", character()),
    event_data_col("main_procedure_code",   character()),
    event_data_col("side_procedure_code_1", character()),
    event_data_col("side_procedure_code_2", character()),
    event_data_col(
      "asa_score", factor(),
      factor_levels = .event_data_levels$ASA_SCORE,
      levels_source = "fixed"),
    event_data_col(
      "wound_class", factor(),
      factor_levels = .event_data_levels$WOUND_CLASS,
      levels_source = "fixed"),
    event_data_col("duration",            integer()),
    event_data_col("emergency_procedure", logical()),
    event_data_col("endoscopic_procedure", logical()),
    event_data_col("implant",             logical()),
    event_data_col("infection_signs",     character()),
    event_data_col("primary_closure",     logical()),
    event_data_col("revision_procedure",  logical())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- ssiData (ssi) --------------------------------------------------------
#
# SSI entity-level DEs minus pathogens + secondary-BSI pathogens. No
# reader renames (DHIS2 codes already use `sec_bsi` rather than
# `secondary_bsi` on SSI). Organism-finding factors use the
# yes/no/not_tested option set.

ssiData_cols <- with_entity_gate(
  c(
    .event_data_link_cols(),
    event_data_col("los", integer()),
    event_data_col("dol", integer()),
    event_data_col(
      "infection_type", factor(),
      factor_levels = .event_data_levels$SSI_INFECTION_TYPE,
      levels_source = "fixed"),
    event_data_col(
      "sec_bsi", factor(),
      factor_levels = .event_data_levels$YES_NO_NO_FOLLOWUP,
      levels_source = "fixed"),
    event_data_col(
      "organisms_superf", factor(),
      factor_levels = .event_data_levels$YES_NO_NOT_TESTED,
      levels_source = "fixed"),
    event_data_col(
      "organisms_deep", factor(),
      factor_levels = .event_data_levels$YES_NO_NOT_TESTED,
      levels_source = "fixed"),
    event_data_col(
      "organisms_organ", factor(),
      factor_levels = .event_data_levels$YES_NO_NOT_TESTED,
      levels_source = "fixed"),
    event_data_col("abscess_deep",             logical()),
    event_data_col("abscess_organ",            logical()),
    event_data_col("fever",                    logical()),
    event_data_col("inc_dehisces_deep",        logical()),
    event_data_col("inc_opened_superf",        logical()),
    event_data_col("infection_present",        logical()),
    event_data_col("localized_erythema",       logical()),
    event_data_col("localized_heat",           logical()),
    event_data_col("localized_pain_deep",      logical()),
    event_data_col("localized_pain_superf",    logical()),
    event_data_col("localized_swelling",       logical()),
    event_data_col("physician_diag_superf",    logical()),
    event_data_col("purulent_drainage_deep",   logical()),
    event_data_col("purulent_drainage_drain",  logical()),
    event_data_col("purulent_drainage_superf", logical())
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- Dispatch helper ------------------------------------------------------
#
# Map `event_type_key` → `<type>_cols`. Used by the reader to pick the
# right schema for each per-event-type pivot.
event_data_cols_for <- function(event_type_key)
{
  switch(
    as.character(event_type_key),
    "adm" = admissionData_cols,
    "end" = surveillanceEndData_cols,
    "bsi" = sepsisData_cols,
    "nec" = necData_cols,
    "hap" = pneumoniaData_cols,
    "pro" = surgeryData_cols,
    "ssi" = ssiData_cols,
    rlang::abort(sprintf(
      "Unknown event_type_key: %s", event_type_key))
  )
}

# ---- infectiousAgentFindings ---------------------------------------------
#
# Per-pathogen findings extracted from bsi / nec / hap / ssi events.
# Failure pattern #6 (empty-data-resilience): the legacy reader's
# `source` column was absent when no surviving pathogen had a
# `_SOURCE` data element; same hazard on resistance markers + `name`
# + `multiple`. The schema contract fixes this by construction —
# every column is declared, and the reader's pre-pivot factor pinning
# + `names_expand = TRUE` guarantees the shape regardless of which
# DE suffixes are present on any row.
#
# `source` is a merged factor covering both the BSI option set
# (`B3oP3uOI5Ef`, codes 1/2/3 → B/C/B+C) and the HAP option set
# (`Y64Emj9405U`, codes 1/2/3 → U/L/U+L). SSI pathogens carry no
# `_SOURCE` DE at all; NEC has no primary pathogens; those rows
# therefore carry `NA` in `source`. The factor levels are fixed by
# the protocol.
#
# Resistance markers (`3gcr`, `car`, `cor`, `mrsa`, `vre`) are
# fixed-level factors `c("no", "yes", "not_tested")` remapped from
# the DHIS2 option set `TnE2yuSrqEP` (codes 1/0/-1).
#
# `multiple` is BSI-only (TRUE_ONLY in DHIS2) — rows for other event
# types carry NA.
#
# `name` is sparse free-text; the orchestrator splits it off into
# `unknownPathogenNames` at [R/import-dhis2.R:174-181] unconditionally
# under the schema contract (the legacy `if ("name" %in% names(...))`
# guard becomes obsolete — the column is always declared).
#
# Link / hierarchy keys inherit strictly from `events_cols` (lean
# children under fat-events, directly materialized under pseudo
# events — same pattern as per-event-type data).

findings_cols <- with_entity_gate(
  list(
    # PK — assigned by `add_key_column("agent_finding_key")` in the
    # reader.
    schema_col(
      "agent_finding_key", integer(),
      include_when = \(opts) opts$include_event != "no"),

    # Link FK to events.
    col_event_key,

    # Inherited link FKs + hierarchy + isTest from events_cols (same
    # pattern as per-event-type data: under fat-events these are
    # absent; under pseudo events they materialize directly).
    col_inherited_from("enrollment_key",       "include_enrollment",
                       events_cols),
    col_inherited_from("patient_key",          "include_patient",
                       events_cols),
    col_inherited_from("department_key",       "include_department",
                       events_cols),
    col_inherited_from("hospital_key",         "include_hospital",
                       events_cols),
    col_inherited_from("country_key",          "include_country",
                       events_cols),
    col_inherited_from("world_bank_class_key", "include_world_bank_class",
                       events_cols),
    schema_col(
      "isTest", logical(),
      include_when = \(opts)
        isTRUE(opts$include_test_data) &&
        !("isTest" %in% names(compile_schema(events_cols, opts)))),

    # Finding-specific payload.
    schema_col(
      "secondary_bsi", logical(),
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "pathogen_key", integer(),
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "index", integer(),
      include_when = \(opts) opts$include_event == "full"),

    # Source — merged BSI + HAP levels. Always declared when the
    # tibble is in full mode; NA on SSI / NEC rows.
    schema_col(
      "source", factor(),
      factor_levels = c("B", "C", "B+C", "U", "L", "U+L"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full"),

    # Multiple — BSI only (TRUE_ONLY); NA elsewhere.
    schema_col(
      "multiple", logical(),
      include_when = \(opts) opts$include_event == "full"),

    # Resistance markers — fixed 3-level factors remapped from the
    # DHIS2 1/0/-1 codes.
    schema_col(
      "3gcr", factor(),
      factor_levels = c("no", "yes", "not_tested"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "car", factor(),
      factor_levels = c("no", "yes", "not_tested"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "cor", factor(),
      factor_levels = c("no", "yes", "not_tested"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "mrsa", factor(),
      factor_levels = c("no", "yes", "not_tested"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "vre", factor(),
      factor_levels = c("no", "yes", "not_tested"),
      levels_source = "fixed",
      include_when = \(opts) opts$include_event == "full")
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- substanceDays -------------------------------------------------------
#
# AB substance-days pivot from the surveillance-end DEs. Row per
# (event_key × index), with index ∈ 1..99 extracted from
# `NEOIPC_SURVEILLANCE_END_AB_SUBST_<NN>` and its paired `_DAYS`
# companion.
#
# No separate PK — `(event_key, index)` is unique per row. Composite
# key is also how downstream consumers join to events.
#
# Gate: `include_event != "no"`. Under pseudo events the tibble still
# carries event_key + its payload (same semantic as per-event-type
# data).

substanceDays_cols <- with_entity_gate(
  list(
    col_event_key,

    # Inherited link FKs + hierarchy + isTest from events_cols.
    col_inherited_from("enrollment_key",       "include_enrollment",
                       events_cols),
    col_inherited_from("patient_key",          "include_patient",
                       events_cols),
    col_inherited_from("department_key",       "include_department",
                       events_cols),
    col_inherited_from("hospital_key",         "include_hospital",
                       events_cols),
    col_inherited_from("country_key",          "include_country",
                       events_cols),
    col_inherited_from("world_bank_class_key", "include_world_bank_class",
                       events_cols),
    schema_col(
      "isTest", logical(),
      include_when = \(opts)
        isTRUE(opts$include_test_data) &&
        !("isTest" %in% names(compile_schema(events_cols, opts)))),

    schema_col(
      "index", integer(),
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "substance_code", character(),
      include_when = \(opts) opts$include_event == "full"),
    schema_col(
      "days", integer(),
      include_when = \(opts) opts$include_event == "full")
  ),
  gate = \(opts) opts$include_event != "no"
)

# ---- unknownPathogenNames ------------------------------------------------
#
# Split off from `infectiousAgentFindings` at
# [R/import-dhis2.R:174-181] so the main findings tibble doesn't carry
# a mostly-NA free-text column. Rows where a user manually typed a
# pathogen name (typically when `pathogen_key == 0` / "unknown") end
# up here.
#
# Under the schema contract the split runs unconditionally —
# `name` is always a declared column on findings (possibly all-NA),
# so the `"name" %in% names(findings)` guard in the orchestrator
# becomes unnecessary.
#
# Link is via `agent_finding_key` (findings PK), not `event_key`.
# Gate is `include_event != "no"` so the tibble exists exactly when
# findings does.

unknownPathogenNames_cols <- with_entity_gate(
  list(
    schema_col(
      "agent_finding_key", integer(),
      include_when = \(opts) opts$include_event != "no"),
    schema_col(
      "name", character(),
      include_when = \(opts) opts$include_event == "full")
  ),
  gate = \(opts) opts$include_event != "no"
)
