#' @include schema-cols-shared.R
NULL

# Schema declaration for the reconciliation slot of a dataset:
#   reconciliationSummary — one row per registered reconciliation, counting
#                           the records of the returned dataset whose stored
#                           values the import repaired, and the ones it
#                           reported without repairing them
#
# The slot exists on every dataset. Its entity gate is the reconciliation
# itself, which runs when `reconcile` is set and patients are imported, so
# without either the slot is 0×0. It is not the validation pass's gate: an
# import with `include_invalid_patients = TRUE` runs no pass but reconciles
# all the same, since a consumer that validates it validates reconciled
# data. An options object without `reconcile` predates the option, and the
# import that wrote it reconciled nothing.

.reconciliation_runs <- function(opts)
  isTRUE(opts$reconcile) && opts$include_patient != "no"

reconciliationSummary_cols <- with_entity_gate(
  list(
    schema_col("reconciliation_id", integer()),
    schema_col(
      "record_kind", factor(),
      factor_levels = c("patients", "enrollments", "events")),
    schema_col("n_repaired", integer()),
    schema_col("n_reported", integer())
  ),
  gate = .reconciliation_runs
)

get_reconciliationSummary_schema <- function(opts)
  compile_schema(reconciliationSummary_cols, opts)
