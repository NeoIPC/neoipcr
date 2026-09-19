#' @include schema-cols-shared.R
NULL

# Schema declarations for the two validation slots of a dataset:
#   validationResults — one row per finding of the import's validation pass
#   validationSummary — one row per rule that flagged or exempted a record,
#                       and one per record kind, counting the distinct
#                       records the pass removed and the ones the exception
#                       list exempted
#
# Both slots exist on every dataset. Their entity gate is the validation pass
# itself: the pass runs when patients are imported and
# `include_invalid_patients` is not `TRUE` (`FALSE` removes the flagged
# patients, an exception list keeps the named ones), so under `TRUE`, or with
# no patients, both slots are 0×0 — there was no pass to report on. A consumer
# that wants the findings of such a dataset calls `validate()` on it.
#
# The finding atoms are shared with `validate()`, which normalises its result
# to them without the gate: a caller validating a dataset imported with
# `include_invalid_patients = TRUE` gets the findings, not an empty tibble.

.validation_pass_runs <- function(opts)
  opts$include_patient != "no" && !isTRUE(opts$include_invalid_patients)

validation_finding_atoms <- list(
  schema_col("rule_id",        integer()),
  schema_col("patient_key",    integer()),
  schema_col("enrollment_key", integer()),
  schema_col("event_key",      integer()),
  schema_col("context",        list())
)

validationResults_cols <- with_entity_gate(
  validation_finding_atoms,
  gate = .validation_pass_runs
)

# `record_kind` is the level the registry declares for the rule, not the
# deepest key a finding carries: an enrolment-level rule that names the form
# it compared still counts enrolments.
validationSummary_cols <- with_entity_gate(
  list(
    schema_col("rule_id", integer()),
    schema_col(
      "record_kind", factor(),
      factor_levels = c("patients", "enrollments", "events")),
    schema_col("n_removed",  integer()),
    schema_col("n_exempted", integer())
  ),
  gate = .validation_pass_runs
)

get_validationResults_schema <- function(opts)
  compile_schema(validationResults_cols, opts)

get_validationSummary_schema <- function(opts)
  compile_schema(validationSummary_cols, opts)
