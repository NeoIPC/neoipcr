# Find infectious-agent findings recorded as the unknown pathogen (concept
# key 0), together with the free-text name the user entered for it, so the
# reader can assign the finding to a catalogued agent.
validation_rule_20 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  # NEOIPC-PERMANENT(dataset-format): never remove the `is.null()` branch. A
  # dataset serialized before the free-text pathogen names had a slot of
  # their own has no `unknownPathogenNames`, and a file on disk outlives the
  # code that wrote it; the rule skips on such a dataset rather than failing
  # it. Without the branch the `left_join()` below aborts on a `NULL`.
  if (!all(c("pathogen_key", "index", "secondary_bsi") %in%
           names(x$infectiousAgentFindings)) ||
      is.null(x$unknownPathogenNames))
    return(.rule_skipped(
      20L, "the infectious-agent findings' pathogen and the unknown pathogen names"))

  x$infectiousAgentFindings |>
    dplyr::filter(.data$pathogen_key == 0L) |>
    dplyr::select("agent_finding_key", "event_key", "index", "secondary_bsi") |>
    dplyr::left_join(
      x$unknownPathogenNames,
      dplyr::join_by("agent_finding_key")) |>
    dplyr::inner_join(
      x$events |>
        dplyr::select("event_key", "patient_key", "enrollment_key"),
      dplyr::join_by("event_key")) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 20L),
      dplyr::join_by("event_key")) |>
    tidyr::nest(context = c("index", "secondary_bsi", "name")) |>
    dplyr::mutate(
      rule_id        = 20L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
