#' @include schema-cols-shared.R schema-events.R schema-enrollments.R
NULL

# Schema declarations for the two notes tibbles:
#   eventNotes       — rows of per-event free-text notes (class
#                      `neoipcr_evn`)
#   enrollment_notes — rows of per-enrollment free-text notes (new
#                      slot, class `neoipcr_eln` — slug aligned with
#                      the `_evn` precedent; finalized by
#                      tasks/neoipcr-class-slug-rename.md)
#
# Both follow the per-event-type-data inheritance pattern: link PK +
# strict inheritance of hierarchy keys / secondary link FKs from the
# parent (events_cols for event notes; enrollments_cols for
# enrollment notes). Under fat parents the child is lean; under
# pseudo parents the inheritance rule materializes the inherited
# keys directly.
#
# Entity gate: compound of `include_<parent> != "no"` AND
# `"<parent>" %in% include_notes`. The `include_notes` option is
# multi-choice (`c("enrollments","events")`), so a caller can opt in
# to one kind of note independently of the other.
#
# Payload fields (same shape for both tibbles):
#   note       — DHIS2 note UID, id-opt-in via
#                `"notes" %in% include_dhis2_ids`.
#   value      — the note body text. Always present under full mode.
#   storedBy   — `user_key` substituted from the raw `storedBy`
#                username; gated on `include_user != "no"`.
#   storedAt   — POSIXct; gated on `include_timestamps`.
#   createdBy  — `user_key` substituted from the raw `createdBy`
#                user UID (not username — see Divergences in the
#                phase-b-notes sub-task); gated on
#                `include_user != "no"`.
#
# No per-attribute companion wrappers — every user / timestamp field
# on a note is entity-level (one `createdBy` per note, one `storedAt`
# per note), same as on enrollments. Atoms declared directly.

# Shared payload factory for notes tibbles — both notes schemas
# declare the identical set of non-link payload atoms with identical
# gating. The `parent_gate` predicate is `\(opts) opts$include_event
# == "full"` or the enrollment equivalent; it also covers the
# containing entity-gate assertion (payload only appears under full
# parent mode).
.notes_payload_cols <- function(parent_full)
{
  list(
    schema_col(
      "note", character(),
      include_when = \(opts) parent_full(opts) &&
                             "notes" %in% opts$include_dhis2_ids),
    schema_col(
      "value", character(),
      include_when = \(opts) parent_full(opts)),
    schema_col(
      "storedBy", integer(),
      include_when = \(opts) parent_full(opts) &&
                             opts$include_user != "no"),
    schema_col(
      "storedAt", as.POSIXct(character()),
      include_when = \(opts) parent_full(opts) &&
                             isTRUE(opts$include_timestamps)),
    schema_col(
      "createdBy", integer(),
      include_when = \(opts) parent_full(opts) &&
                             opts$include_user != "no")
  )
}

# ---- eventNotes ----------------------------------------------------------

event_notes_cols <- with_entity_gate(
  c(
    list(col_event_key),
    list(col_inherited_from("enrollment_key",      "include_enrollment",
                            events_cols)),
    list(col_inherited_from("patient_key",         "include_patient",
                            events_cols)),
    list(col_inherited_from("department_key",      "include_department",
                            events_cols)),
    list(col_inherited_from("hospital_key",        "include_hospital",
                            events_cols)),
    list(col_inherited_from("country_key",         "include_country",
                            events_cols)),
    list(col_inherited_from("world_bank_class_key", "include_world_bank_class",
                            events_cols)),
    list(schema_col(
      "isTest", logical(),
      include_when = \(opts)
        isTRUE(opts$include_test_data) &&
        !("isTest" %in% names(compile_schema(events_cols, opts))))),
    .notes_payload_cols(\(opts) opts$include_event == "full")
  ),
  gate = \(opts) opts$include_event != "no" &&
                 "events" %in% opts$include_notes
)

# ---- enrollment_notes ----------------------------------------------------

enrollment_notes_cols <- with_entity_gate(
  c(
    list(col_enrollment_key),
    list(col_inherited_from("patient_key",         "include_patient",
                            enrollments_cols)),
    list(col_inherited_from("department_key",      "include_department",
                            enrollments_cols)),
    list(col_inherited_from("hospital_key",        "include_hospital",
                            enrollments_cols)),
    list(col_inherited_from("country_key",         "include_country",
                            enrollments_cols)),
    list(col_inherited_from("world_bank_class_key", "include_world_bank_class",
                            enrollments_cols)),
    list(schema_col(
      "isTest", logical(),
      include_when = \(opts)
        isTRUE(opts$include_test_data) &&
        !("isTest" %in% names(compile_schema(enrollments_cols, opts))))),
    .notes_payload_cols(\(opts) opts$include_enrollment == "full")
  ),
  gate = \(opts) opts$include_enrollment != "no" &&
                 "enrollments" %in% opts$include_notes
)
