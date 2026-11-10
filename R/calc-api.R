#' Calculate a NeoIPC reference data set
#'
#' The `metadata$dataset_options` the result carries are a serializable copy
#' of the dataset's: an exception list is replaced by the marker
#' `"exception_list_applied"` and the department filter by `"applied"` — an
#' empty filter, which the import does not apply, leaves as `NULL` — so
#' reference data names neither the records a list exempted nor the
#' departments it was built from. `validationSummary` is the dataset's
#' validation summary (see [import_dhis2()]); a dataset without it is
#' refused. `reconciliationSummary` is the dataset's reconciliation summary
#' (see [import_dhis2()]), `NULL` for a dataset written before the summary
#' existed; a summary of another shape is refused.
#'
#' The dataset must have been imported with `include_department`,
#' `include_patient`, and `include_enrollment` not `"no"` and with
#' `include_event = "full"` (see [dhis2_dataset_options()]): the
#' infectious-agent figures read the findings' `pathogen_key`, which the
#' `"pseudo"` tier leaves out. Another dataset is refused with an error of
#' class `neoipcr_unmet_import_options`.
#'
#' Where the findings hold codes the NeoIPC infectious-agent list lacks, a
#' warning of class `neoipcr_unknown_infectious_agent_code` is raised once,
#' with the codes in its field `codes`: the infectious-agent tables count such
#' a code in their Totals only, and no resistance category applies to it.
#'
#' @section Versions of the rate definitions:
#' `metadata$rate_definitions` stamps each table the result carries, one row
#' per slot (`table`), with `version`, the version of the definitions of the
#' table's rates, which a change to that table's definitions raises and no
#' other; and the digests of what the table reads from the bundled
#' infectious-agent list, each `NA` where it does not read it:
#' `common_commensal_digest`, the common commensals, on which the incidence
#' densities' rate definitions split the kinds of sepsis;
#' `concept_mapping_digest`, the concept each code names, by which the tables
#' whose rows are infectious agents and the completeness of antimicrobial
#' susceptibility testing count an infectious agent once per infection;
#' `groups_tree_digest` or `ontology_tree_digest`, the tree of the rows over
#' the reporting groups or over the full ontology, which a table whose rows
#' are infectious agents reads in its version, with the `layout` of its rows;
#' and `applicability_digest`, to which infectious agents each resistance
#' category applies, which the tables of the resistance categories read.
#' [get_benchmark_data()] joins a table of two datasets only where its stamps
#' match, so that a release of the list that changes only what a table does
#' not read leaves it comparable. `metadata$infectious_agent_tables` names
#' the release of the list (`version`, `commit`) and the `layout` of the
#' tables whose rows are infectious agents, and `metadata$neoipcr_version`
#' the version of neoipcr that calculated the result.
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#'
#' @returns A NeoIPC reference data set
#' @export
calculate_reference_data <- function(x, use_cache = TRUE) {
  check_neoipcr_ds(x)
  # Three-valued gates all need to be non-"no" for the reference
  # pipeline: department grouping, per-patient / per-enrollment / per-
  # event denominators, and hierarchy metadata for the country-level
  # joins all feed into the computed reference data. The events need the
  # full tier, whose findings carry the infectious agents.
  assert_options_for(x, required = list(
    include_department = c("pseudo", "full"),
    include_patient    = c("pseudo", "full"),
    include_enrollment = c("pseudo", "full"),
    include_event      = "full"
  ), reasons = calculation_option_reasons(),
  fn_name = "calculate_reference_data")
  assert_validation_summary(x, fn_name = "calculate_reference_data")
  assert_reconciliation_summary(x, fn_name = "calculate_reference_data")
  .warn_unknown_infectious_agent_codes(x)

  # `metadata$countries` is always a tibble under the three-mode schema
  # contract; gate on the key column instead of null-ness. Under "no" the
  # tibble is 0×0 so `country_key` is absent.
  if(!("country_key" %in% names(x$metadata$countries)))
    logger::log_warn(
      "Data is missing country metadata; the resulting dataset cannot be used to create reference reports.",
      namespace = "neoipcr")

  pd <- get_risk_time(x, use_cache = use_cache)$patient_days
  pd_dept <- get_risk_time(
    x,
    group_cols = "department_key",
    use_cache = use_cache)$patient_days
  pd_q <- pd_dept |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  rp <- x |>
    get_risk_population(use_cache = use_cache)
  rp_dept <- x |>
    get_risk_population(group_cols = "department_key", use_cache = use_cache)
  pat_q <- rp_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  enr_q <- rp_dept$n_enrollments |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  sr_dept <- x |>
    get_surgery_risk(
      group_cols = "department_key",
      use_cache = use_cache) |>
    dplyr::full_join(
      x$enrollments |>
        dplyr::select("department_key") |>
        dplyr::distinct(),
      dplyr::join_by("department_key")) |>
    dplyr::mutate(
      n_patients = tidyr::replace_na(.data$n_patients, 0L),
      n_procedures = tidyr::replace_na(.data$n_procedures, 0L))
  sur_pat_q <- sr_dept$n_patients |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()
  sur_proc_q <- sr_dept$n_procedures |>
    stats::quantile(probs = quartile_probs) |>
    as.integer()

  ds_opts <- serializable_dataset_options(
    x$metadata$dataset_options, keep_department_filter = FALSE)
  assert_serializable_dataset_options(ds_opts, allow_department_filter = FALSE)

  n_infections <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(event_type_key = "overall"),
      x |>
        get_infection_counts() |>
        dplyr::bind_cols(
          x |>
            get_infection_counts(group_cols = "department_key") |>
            dplyr::summarise(
              pooled_mean = as.integer(round(mean(.data$n))),
              q = list(
                stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
            tidyr::unnest_wider(q, names_sep = "", transform = as.integer) |>
            ensure_quartile_cols())),
    x |>
      get_infection_counts(group_cols = c("event_type_key")) |>
      dplyr::inner_join(
        x |>
          get_infection_counts(
            group_cols = c("department_key", "event_type_key")) |>
          dplyr::group_by(.data$event_type_key) |>
          dplyr::summarise(
            pooled_mean = as.integer(round(mean(.data$n))),
            q = list(
              stats::quantile(.data$n, quartile_probs, names = FALSE))) |>
          tidyr::unnest_wider(q, names_sep = "", transform = as.integer) |>
          ensure_quartile_cols(),
        dplyr::join_by("event_type_key"))) |>
    dplyr::rename(inf_type = "event_type_key", total = "n")

  structure(
    list(
      metadata = list(
        calculated = lubridate::now("UTC"),
        neoipcr_version = .neoipcr_version(),
        rate_definitions = .rate_definitions(),
        infectious_agent_tables = .infectious_agent_tables(),
        dataset_options = ds_opts,
        data_up_to = x$metadata$system$date,
        effective_analysis_period = get_effective_analysis_period(x),
        countries = get_countries_with_wb_class(x)
      ),
      birth_weight_figure = x|> get_birthweight_figure_data(),
      gestational_age_figure = x|> get_gestational_age_figure_data(),
      n_departments = x$enrollments$department_key |> unique() |> length(),
      n_patients = tibble::tibble(
        total = rp$n_patients,
        pooled_mean = rp_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pat_q[1],
        q2 = pat_q[2],
        q3 = pat_q[3]),
      n_enrollments = tibble::tibble(
        total = rp$n_enrollments,
        pooled_mean = rp_dept$n_enrollments |>
          mean() |>
          round() |>
          as.integer(),
        q1 = enr_q[1],
        q2 = enr_q[2],
        q3 = enr_q[3]),
      n_patient_days = tibble::tibble(
        total = pd,
        pooled_mean = pd_dept |>
          mean() |>
          round() |>
          as.integer(),
        q1 = pd_q[1],
        q2 = pd_q[2],
        q3 = pd_q[3]),
      n_surgical_departments = sr$n_departments,
      n_surgical_patients = tibble::tibble(
        total = sr$n_patients,
        pooled_mean = sr_dept$n_patients |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_pat_q[1],
        q2 = sur_pat_q[2],
        q3 = sur_pat_q[3]),
      n_surgical_procedures = tibble::tibble(
        total = sr$n_procedures,
        pooled_mean = sr_dept$n_procedures |>
          mean() |>
          round() |>
          as.integer(),
        q1 = sur_proc_q[1],
        q2 = sur_proc_q[2],
        q3 = sur_proc_q[3]),
      n_infections = n_infections,
      validationSummary = x$validationSummary,
      # NEOIPC-PERMANENT(dataset-format): `NULL` for a raw dataset written
      # before the slot existed; see `assert_reconciliation_summary()`.
      reconciliationSummary = x$reconciliationSummary,
      usage_density_rate_table =
        get_usage_density_rate_table(x, use_cache),
      antibiotic_utilization_table =
        get_antibiotic_utilization_table(x, use_cache),
      surgery_rate_table =
        get_ref_surgery_rate_table(x, use_cache),
      incidence_density_rate_table =
        get_incidence_density_rate_table(x, use_cache),
      dev_ass_incidence_density_rate_table =
        get_dev_ass_incidence_density_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_agent_table =
        get_infectious_agent_detection_rate_per_agent_table(x, use_cache),
      infectious_agent_detection_rate_per_agent_ontology_table =
        get_infectious_agent_detection_rate_per_agent_table(
          x, use_cache, rows = "ontology"),
      abr_infection_rate_table =
        get_abr_infection_rate_table(x, use_cache),
      abr_infection_rate_ontology_table =
        get_abr_infection_rate_table(x, use_cache, rows = "ontology"),
      infectious_agent_resistance_rate_table =
        get_infectious_agent_resistance_rate_table(x, use_cache),
      infectious_agent_resistance_rate_ontology_table =
        get_infectious_agent_resistance_rate_table(
          x, use_cache, rows = "ontology"),
      secondary_bsi_rate_table =
        get_secondary_bsi_rate_table(x, use_cache),
      infectious_agent_detection_rate_per_inf_type_table =
        get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache),
      resistance_test_rate_table =
        get_resistance_test_rate_table(x, use_cache)
    ),
    class = c("neoipcr_ref_ds", "neoipcr_rep_ds", "list"))
}

#' Calculate a NeoIPC department report data set
#'
#' The `metadata$dataset_options` the result carries are a serializable copy
#' of the dataset's: an exception list is replaced by the marker
#' `"exception_list_applied"`, while the department filter — the
#' department's own — is kept. `validationSummary` is the dataset's
#' validation summary (see [import_dhis2()]); a dataset without it is
#' refused. `reconciliationSummary` is the dataset's reconciliation summary
#' (see [import_dhis2()]), `NULL` for a dataset written before the summary
#' existed; a summary of another shape is refused.
#'
#' The dataset must have been imported with `include_department`,
#' `include_patient`, and `include_enrollment` not `"no"` and with
#' `include_event = "full"` (see [dhis2_dataset_options()]): the
#' infectious-agent figures read the findings' `pathogen_key`, which the
#' `"pseudo"` tier leaves out. Another dataset is refused with an error of
#' class `neoipcr_unmet_import_options`.
#'
#' Where the findings hold codes the NeoIPC infectious-agent list lacks, a
#' warning of class `neoipcr_unknown_infectious_agent_code` is raised once,
#' as [calculate_reference_data()] raises it. The result carries the versions
#' of its tables' rate definitions as [calculate_reference_data()] describes
#' them.
#'
#' @param x The neoipcr_ds object containing the data
#' @param use_cache Use the cache
#'
#' @returns A NeoIPC department report data set
#' @export
calculate_department_data <- function(x, use_cache = TRUE) {
  check_neoipcr_ds(x)
  # Three-valued gates all need to be non-"no" for the department
  # pipeline: department grouping, per-patient / per-enrollment / per-
  # event denominators. The events need the full tier, whose findings carry
  # the infectious agents.
  assert_options_for(x, required = list(
    include_department = c("pseudo", "full"),
    include_patient    = c("pseudo", "full"),
    include_enrollment = c("pseudo", "full"),
    include_event      = "full"
  ), reasons = calculation_option_reasons(),
  fn_name = "calculate_department_data")
  assert_validation_summary(x, fn_name = "calculate_department_data")
  assert_reconciliation_summary(x, fn_name = "calculate_department_data")
  .warn_unknown_infectious_agent_codes(x)

  rt <- x |>
    get_risk_time(use_cache = use_cache)
  rp <- x |>
    get_risk_population(use_cache = use_cache)
  sr <- x |>
    get_surgery_risk(use_cache = use_cache)

  usage_density_rate_table <- rt |>
    dplyr::select(!"patient_days") |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("factor","name"),
      names_pattern = "^(.+)_([^_]+)$") |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      factor = factor(.data$factor,
        levels = c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                   "kangaroo_care","ab","a","w","r"))
    ) |>
    (\(d) dplyr::bind_cols(d, poisson_ci_cols(d$days, rt$patient_days, multiplier = 100)))() |>
    (\(r) {
      expected_levels <- c("cvc","pvc","vs","inv","niv","human_milk","probiotic",
                           "kangaroo_care","ab","a","w","r")
      missing <- setdiff(expected_levels, as.character(r$factor))
      if (length(missing) > 0)
        r <- r |>
          dplyr::bind_rows(
            tibble::tibble(
              factor = factor(missing, levels = levels(r$factor)),
              days = 0L,
              rate = NA_real_,
              ci_lower = NA_real_,
              ci_upper = NA_real_))
      r
    })() |>
    dplyr::arrange(.data$factor)

  # Extract infection counts for metadata
  n_infections <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(inf_type = "overall"),
      x |>
        get_infection_counts(use_cache = use_cache) |>
        dplyr::rename(total = "n")),
    x |>
      get_infection_counts(group_cols = c("event_type_key"), use_cache = use_cache) |>
      dplyr::rename(inf_type = "event_type_key", total = "n"))

  ds_opts <- serializable_dataset_options(
    x$metadata$dataset_options, keep_department_filter = TRUE)
  assert_serializable_dataset_options(ds_opts, allow_department_filter = TRUE)

  structure(
    list(
      metadata = list(
        calculated = lubridate::now("UTC"),
        neoipcr_version = .neoipcr_version(),
        rate_definitions = .rate_definitions(),
        infectious_agent_tables = .infectious_agent_tables(),
        dataset_options = ds_opts,
        data_up_to = x$metadata$system$date,
        effective_analysis_period = get_effective_analysis_period(x),
        hospitals = x$metadata$hospitals,
        departments = x$metadata$departments,
        countries = get_countries_with_wb_class(x)
      ),
      birth_weight_figure = get_birthweight_figure_data(x),
      gestational_age_figure = get_gestational_age_figure_data(x),
      n_departments = if (!is.null(x$enrollments$department_key)) {
        x$enrollments$department_key |> unique() |> length()
      } else {
        1L
      },
      n_patients = list(total = rp$n_patients),
      n_enrollments = list(total = rp$n_enrollments),
      n_patient_days = list(total = rt$patient_days),
      n_infections = n_infections,
      n_surgical_departments = sr$n_departments,
      n_surgical_procedures = list(total = sr$n_procedures),
      n_surgical_patients = list(total = sr$n_patients),
      validationSummary = x$validationSummary,
      # NEOIPC-PERMANENT(dataset-format): `NULL` for a raw dataset written
      # before the slot existed; see `assert_reconciliation_summary()`.
      reconciliationSummary = x$reconciliationSummary,
      usage_density_rate_table = usage_density_rate_table,
      antibiotic_utilization_table =
        get_antibiotic_utilization_table(x, use_cache, include_quartiles = FALSE),
      surgery_rate_table =
        get_surgery_rate_table(
          x,
          use_cache),
      incidence_density_rate_table =
        get_incidence_density_rate_table(
          x,
          use_cache,
          include_quartiles = FALSE),
      dev_ass_incidence_density_rate_table =
        get_dev_ass_incidence_density_rate_table(x, use_cache, include_quartiles = FALSE),
      infectious_agent_detection_rate_per_agent_table =
        get_infectious_agent_detection_rate_per_agent_table(x, use_cache, include_quartiles = FALSE),
      infectious_agent_detection_rate_per_agent_ontology_table =
        get_infectious_agent_detection_rate_per_agent_table(
          x, use_cache, include_quartiles = FALSE, rows = "ontology"),
      abr_infection_rate_table =
        get_abr_infection_rate_table(x, use_cache, include_quartiles = FALSE),
      abr_infection_rate_ontology_table =
        get_abr_infection_rate_table(
          x, use_cache, include_quartiles = FALSE, rows = "ontology"),
      infectious_agent_resistance_rate_table =
        get_infectious_agent_resistance_rate_table(
          x, use_cache, include_quartiles = FALSE),
      infectious_agent_resistance_rate_ontology_table =
        get_infectious_agent_resistance_rate_table(
          x, use_cache, include_quartiles = FALSE, rows = "ontology"),
      secondary_bsi_rate_table =
        get_secondary_bsi_rate_table(x, use_cache, include_quartiles = FALSE),
      infectious_agent_detection_rate_per_inf_type_table =
        get_infectious_agent_detection_rate_per_inf_type_table(x, use_cache, include_quartiles = FALSE),
      resistance_test_rate_table =
        get_resistance_test_rate_table(x, use_cache, include_quartiles = FALSE)
    ),
    class = c("neoipcr_rep_ds", "list"))
}

#' Creates a NeoIPC benchmark data set from department report datasets and a
#'  reference data set
#'
#' @param ... A set of name-value pairs. Each of them should be either a
#'  neoipcr_rep_ds or a neoipcr_ref_ds (typically it's exactly one
#'  neoipcr_rep_ds and one neoipcr_ref_ds to benchmark department data against
#'  reference data but it can also be multiple neoipcr_rep_ds or multiple
#'  neoipcr_ref_ds to benchmark them against each other). Each name is appended
#'  to that dataset's column names in the resulting tables, so
#'  `get_benchmark_data(own = x, ref = y)` yields `n_own` and `n_ref`. At
#'  least one dataset is required; a call without any aborts with class
#'  `neoipcr_no_benchmark_datasets`.
#' @details
#' A table is joined only across datasets whose stamps of it match: the
#' version of its rate definitions and the digests of what it reads from the
#' infectious-agent list (see [calculate_reference_data()]). A later dataset
#' whose stamps of a table differ from those of the first dataset that holds
#' the table is not joined into it: the table keeps the rows of the datasets
#' joined, the table and the dataset are recorded in `not_comparable`, and
#' one warning of class `neoipcr_tables_not_comparable` is raised, whose
#' field `not_comparable` holds the same record; a figure takes none of that
#' dataset's rows, and any other table leaves that dataset's columns empty. A
#' stamp that a dataset lacks, such as one a later version of neoipcr adds,
#' counts as missing there, so that only the tables whose stamp in the other
#' dataset has a value in it are not joined. A dataset calculated before the
#' datasets carried these stamps counts, for every table, as calculated under
#' other definitions.
#'
#' The tables whose rows are infectious agents are joined on `node_id`, within
#' the resistance category `abr` where they have one, and take the order of
#' the reference: the first reference dataset (a `neoipcr_ref_ds`) joined
#' into the table, or else the first dataset that holds the table. So
#' departments benchmarked against each other take the order of the first
#' one that holds the table, and a department passed before a reference
#' whose table is not joined keeps its own. Their rows are those of every
#' dataset joined, in the reference's order: in each block, its Total, then
#' the summary rows of the reporting groups, then the tree, each row after
#' its parent. A summary row is one whose `node_id`, `<attribute>|<value>`,
#' names an attribute of the bundled classification, such as
#' `gram_reaction`, whatever its value. The summary rows are laid out
#' attribute by attribute in the reference's order of the attributes and of
#' their values, which the benchmark takes from the reference's summary rows
#' of every block of the table, whether or not the bundled classification
#' holds their values:
#'
#' 1. The attributes are in the reference's order; an attribute of which the
#'    reference holds no row in the table comes directly after the last of
#'    the reference's attributes that the bundled classification orders
#'    before it, or first, several such attributes in the bundled
#'    classification's order.
#' 2. Within an attribute, the values the reference holds in the table are
#'    in its order, also in a block where it lacks some of their rows.
#' 3. A value the reference holds nowhere in the table comes directly after
#'    the reference's value of its attribute that the reference places last
#'    among those the bundled classification orders before its own, or,
#'    where there is none, directly before the reference's first value of
#'    its attribute; a value the bundled classification lacks comes last in
#'    its attribute's run.
#' 4. Rows that share a place are in the bundled classification's order,
#'    those whose value it lacks in their order in the joined table, so that
#'    each attribute's run stays contiguous.
#'
#' So the attributes, and the values that the reference holds in the table or
#' the bundled classification holds, follow one order in every block, while
#' the values that neither holds follow, in each block, their order in the
#' joined table, which can differ between blocks. Each block of the reference
#' holds its summary rows in the order of the release of the
#' infectious-agent list it was calculated from, and the reference's order
#' keeps the order of every block. Where the blocks leave it open, as for two
#' attributes, or two values, that the blocks order neither directly nor through
#' others, the bundled classification decides it as far as the blocks allow:
#' place by place, of the attributes or values the blocks let come next, the
#' first in its order, a value it lacks after those it holds, and two values it
#' lacks in the order in which the reference's table first shows them. The
#' joined table holds the rows of the datasets in the order in which they are
#' passed, the first dataset's first, each later one adding the rows the
#' earlier ones lack. A summary row of an attribute the bundled
#' classification lacks, which only a dataset calculated by another version
#' of neoipcr can hold, is placed as a top-level row of the tree. Siblings
#' keep the order the reference gives them, and a row the reference lacks
#' follows its siblings, in its order in the joined table, but precedes a
#' residual "n.o.s." (not otherwise specified) row, which stays last among
#' its node's children. A dataset has a row wherever it records an
#' infectious agent at or beneath the row's node, and in a resistance
#' category's block one to which the list applies the category or with a
#' result for it, so a dataset without a row has none of its events, and the
#' row gives it 0 events, with their interval, over its own denominator of
#' the row: its infections with an infectious agent in the tree (in total and
#' of each type) for the infectious agents detected; its patient-days for the
#' infections with resistant infectious agents; and none for the resistance
#' rates, which have no rate there. A table calculated before its rows had
#' `node_id`, whose dataset is joined with no other, keeps its rows as they
#' are; such a dataset holds its resistance rates per infectious agent as
#' `organism_resistance_rate_table`, which counts as its
#' `infectious_agent_resistance_rate_table`.
#'
#' @returns A neoipcr_bnch_ds. Each dataset's `metadata`,
#'  `validationSummary`, and `reconciliationSummary` are carried under the
#'  dataset's name — a dataset serialized before a summary existed, or
#'  calculated from a raw dataset written before it existed, contributes no
#'  entry to it — and the counts and tables are merged with the names as
#'  column suffixes. `not_comparable` holds one row per table and dataset not
#'  joined: `table`, the table's slot, and `dataset`, the dataset's name.
#' @export
get_benchmark_data <- function(...) {
  x <- list(...)
  if (length(x) == 0L)
    rlang::abort(
      gettext("No dataset to benchmark: pass at least one calculated dataset."),
      class = "neoipcr_no_benchmark_datasets")
  ds_names = rlang::names2(x)
  comparable <- .benchmark_comparability(x)
  output <- list(
    dataset_names = ds_names,
    metadata = list(),
    validationSummary = list(),
    reconciliationSummary = list(),
    not_comparable = .benchmark_not_comparable(comparable, ds_names))
  suffixes = ds_names |>
    sapply(\(x)ifelse(x=="",x,paste0("_",x)), USE.NAMES = FALSE)

  # The columns that each table holds of the datasets not joined into it,
  # which stay empty whatever a later dataset's join fills in.
  left_empty <- list()
  leave_empty <- function(slot, tbl, by = character()) {
    output[[slot]] <<- .benchmark_empty_columns(output[[slot]], tbl, by)
    left_empty[[slot]] <<- c(left_empty[[slot]], setdiff(names(tbl), by))
  }
  # For each table whose rows are infectious agents, the suffixes of the
  # datasets joined into it, and the rows of the dataset whose order it takes.
  agent_suffixes <- list()
  agent_order <- list()
  # Each dataset's own patients and patient-days, which a table joined
  # repairs its rows at 0 events over, whether or not the counts are joined.
  denominators <- list()

  for (i in seq_along(x)) {
    ds <- x[[i]]
    suffix <- suffixes[i]
    ds_name <- ds_names[i]
    elements <- names(ds)
    # Whether the dataset joins each table: `TRUE`, `FALSE` where its stamps
    # of the table differ from those of the first dataset that holds it, and
    # `NA` where it does not hold the table.
    joins <- vapply(comparable, \(j) j[[i]], logical(1))

    # Extract metadata if present
    if ("metadata" %in% elements) {
      output$metadata[[ds_name]] <- ds$metadata
    }
    # The validation and reconciliation summaries are a dataset's own
    # account, like its metadata, so they ride under the dataset's name
    # rather than merged by suffix. A calculated dataset serialized before a
    # slot existed, or calculated from a raw dataset written before it
    # existed, carries `NULL` there, and assigning `NULL` adds no entry, so
    # such a dataset contributes none, as the return value's documentation
    # says, and its consumer renders without the summary.
    output$validationSummary[[ds_name]] <- ds[["validationSummary"]]
    output$reconciliationSummary[[ds_name]] <- ds[["reconciliationSummary"]]

    # The counts: one row, a column per count and dataset.
    for (slot in c("n_departments", "n_surgical_departments", "n_patients",
                   "n_enrollments", "n_patient_days", "n_surgical_patients",
                   "n_surgical_procedures")) {
      if (is.na(joins[[slot]]))
        next
      tbl <- ds[[slot]]
      if (!is.data.frame(tbl))
        tbl <- tibble::as_tibble(if (is.list(tbl)) tbl else list(n = tbl))
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix))

      if (joins[[slot]])
        output[[slot]] <- dplyr::bind_cols(output[[slot]], tbl)
      else
        leave_empty(slot, tbl)
      if (slot %in% c("n_patients", "n_patient_days"))
        denominators[[slot]] <- dplyr::bind_cols(denominators[[slot]], tbl)
    }
    # The figures hold a row per dataset and point, so a dataset whose figure
    # is not joined adds none of its rows.
    for (slot in c("birth_weight_figure", "gestational_age_figure")) {
      if (!isTRUE(joins[[slot]]))
        next
      for (tbl_name in names(ds[[slot]])) {
        tbl <- tibble::tibble(dataset = ds_name) |>
          dplyr::bind_cols(ds[[slot]][[tbl_name]])

        if (is.null(output[[slot]])) {
          output[[slot]] <- list()
        }
        output[[slot]][[tbl_name]] <- dplyr::bind_rows(
          output[[slot]][[tbl_name]], tbl)
      }
    }
    if (!is.na(joins[["usage_density_rate_table"]])) {
      tbl <- ds$usage_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"factor")

      if (is.null(output$usage_density_rate_table)) {
        output$usage_density_rate_table <- tbl
      } else if (!joins[["usage_density_rate_table"]]) {
        leave_empty("usage_density_rate_table", tbl, "factor")
      } else {
        output$usage_density_rate_table <- output$usage_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("factor")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("days_"),
              ~tidyr::replace_na(.x, 0L)),
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0L)),
            dplyr::across(
              c(dplyr::starts_with("rate_"), dplyr::starts_with("pooled_"),
                dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )

        output$usage_density_rate_table <- fix_zero_event_ci(
          output$usage_density_rate_table, suffixes,
          denominator_tbl = denominators$n_patient_days,
          multiplier = 100)
      }
    }
    if (!is.na(joins[["antibiotic_utilization_table"]])) {
      structural <- c("row_id", "atc5_group", "row_type", "display_name", "aware")
      tbl <- ds$antibiotic_utilization_table |>
        dplyr::rename_with(~ paste0(.x, suffix),
                           !tidyselect::any_of(structural))

      if (is.null(output$antibiotic_utilization_table)) {
        output$antibiotic_utilization_table <- tbl
      } else if (!joins[["antibiotic_utilization_table"]]) {
        leave_empty("antibiotic_utilization_table", tbl, structural)
      } else {
        output$antibiotic_utilization_table <-
          output$antibiotic_utilization_table |>
          dplyr::full_join(tbl, dplyr::join_by("row_id"),
                           suffix = c("", ".y")) |>
          dplyr::mutate(
            atc5_group = dplyr::coalesce(
              .data$atc5_group, .data$atc5_group.y),
            row_type = dplyr::coalesce(
              .data$row_type, .data$row_type.y),
            display_name = dplyr::coalesce(
              .data$display_name, .data$display_name.y),
            aware = dplyr::coalesce(
              .data$aware, .data$aware.y)) |>
          dplyr::select(!tidyselect::ends_with(".y")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~ tidyr::replace_na(.x, 0L)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"),
                dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"),
                dplyr::starts_with("ci_upper_")),
              ~ tidyr::replace_na(.x, NA_real_)))

        output$antibiotic_utilization_table <- fix_zero_event_ci(
          output$antibiotic_utilization_table, suffixes,
          denominator_tbl = denominators$n_patient_days,
          multiplier = 100)

        # Re-sort after merge: ATC5 group, then header before substances
        output$antibiotic_utilization_table <-
          output$antibiotic_utilization_table |>
          dplyr::arrange(.data$atc5_group, .data$row_type, .data$row_id)
      }
    }
    if (!is.na(joins[["surgery_rate_table"]])) {
      tbl <- ds$surgery_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"pro_cat")

      if (is.null(output$surgery_rate_table)) {
        output$surgery_rate_table <- tbl
      } else if (!joins[["surgery_rate_table"]]) {
        leave_empty("surgery_rate_table", tbl, "pro_cat")
      } else {
        output$surgery_rate_table <- output$surgery_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("pro_cat")) |>
          dplyr::mutate(
            dplyr::across(
              !tidyselect::matches("^q|^ci_", ignore.case = F),
              ~ tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_)))

        output$surgery_rate_table <- fix_zero_event_ci(
          output$surgery_rate_table, suffixes,
          denominator_tbl = denominators$n_patients,
          multiplier = 100)
      }
    }
    if (!is.na(joins[["incidence_density_rate_table"]])) {
      tbl <- ds$incidence_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"inf")

      if (is.null(output$incidence_density_rate_table)) {
        output$incidence_density_rate_table <- tbl
      } else if (!joins[["incidence_density_rate_table"]]) {
        leave_empty("incidence_density_rate_table", tbl, "inf")
      } else {
        output$incidence_density_rate_table <-
          output$incidence_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("inf")) |>
          dplyr::mutate(
            dplyr::across(dplyr::starts_with("n_"), ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )

        output$incidence_density_rate_table <- fix_zero_event_ci(
          output$incidence_density_rate_table, suffixes,
          denominator_tbl = denominators$n_patient_days,
          multiplier = 1000)
      }
    }
    if (!is.na(joins[["dev_ass_incidence_density_rate_table"]])) {
      tbl <- ds$dev_ass_incidence_density_rate_table
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"dev")

      if (is.null(output$dev_ass_incidence_density_rate_table)) {
        output$dev_ass_incidence_density_rate_table <- tbl
      } else if (!joins[["dev_ass_incidence_density_rate_table"]]) {
        leave_empty("dev_ass_incidence_density_rate_table", tbl, "dev")
      } else {
        output$dev_ass_incidence_density_rate_table <-
          output$dev_ass_incidence_density_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("dev")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"), dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )
      }
    }
    if (!is.na(joins[["n_infections"]])) {
      tbl <- ds$n_infections
      tbl <- tbl |>
        dplyr::rename_with(~ paste0(.x, suffix), !"inf_type")

      if (is.null(output$n_infections)) {
        output$n_infections <- tbl
      } else if (!joins[["n_infections"]]) {
        leave_empty("n_infections", tbl, "inf_type")
      } else {
        output$n_infections <- output$n_infections |>
          dplyr::full_join(tbl, dplyr::join_by("inf_type"))
      }
    }
    # Both versions of each table whose rows are infectious agents join on
    # `node_id`, the resistance tables' within the resistance category; the
    # columns that place and label a row are the same in every dataset joined,
    # whose stamps match. The rows' order and the measures of a dataset
    # without a row are completed once every dataset is joined.
    for (tbl_name in .infectious_agent_table_slots()) {
      if (is.na(joins[[tbl_name]]))
        next
      tbl <- .benchmark_table(ds, tbl_name)
      key_cols <- intersect(c("abr", "node_id"), names(tbl))
      row_cols <- intersect(
        c("abr", "level", "rank", "node_id", "parent_id", "group"),
        names(tbl))
      tbl <- tbl |>
        dplyr::rename_with(
          ~ paste0(.x, suffix), !tidyselect::any_of(row_cols))

      if (!joins[[tbl_name]]) {
        leave_empty(tbl_name, tbl, row_cols)
        next
      }
      if (is.null(output[[tbl_name]])) {
        output[[tbl_name]] <- tbl
      } else {
        output[[tbl_name]] <- output[[tbl_name]] |>
          dplyr::full_join(tbl, by = key_cols, suffix = c("", ".joined"))
        for (col in setdiff(row_cols, key_cols)) {
          joined_col <- paste0(col, ".joined")
          output[[tbl_name]][[col]] <- dplyr::coalesce(
            output[[tbl_name]][[col]], output[[tbl_name]][[joined_col]])
          output[[tbl_name]][[joined_col]] <- NULL
        }
      }
      agent_suffixes[[tbl_name]] <- c(agent_suffixes[[tbl_name]], suffix)
      if (is.null(agent_order[[tbl_name]]) ||
          (!agent_order[[tbl_name]]$reference && is_neoipcr_ref_ds(ds)))
        agent_order[[tbl_name]] <- list(
          rows = tbl[key_cols], reference = is_neoipcr_ref_ds(ds))
    }
    # The next three tables join only where they have rows (see
    # `.benchmark_carries()`).
    if (!is.na(joins[["secondary_bsi_rate_table"]])) {
      # Key column is event_type_key
      tbl <- ds$secondary_bsi_rate_table |>
        dplyr::rename_with(~ paste0(.x, suffix), !"event_type_key")

      if (is.null(output$secondary_bsi_rate_table)) {
        output$secondary_bsi_rate_table <- tbl
      } else if (!joins[["secondary_bsi_rate_table"]]) {
        leave_empty("secondary_bsi_rate_table", tbl, "event_type_key")
      } else {
        output$secondary_bsi_rate_table <- output$secondary_bsi_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("event_type_key")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"),
                dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )
      }
    }
    if (!is.na(joins[["infectious_agent_detection_rate_per_inf_type_table"]])) {
      tbl <- ds$infectious_agent_detection_rate_per_inf_type_table |>
        dplyr::rename_with(~ paste0(.x, suffix), !"inf")

      if (is.null(
        output$infectious_agent_detection_rate_per_inf_type_table)) {
        output$infectious_agent_detection_rate_per_inf_type_table <- tbl
      } else if (
        !joins[["infectious_agent_detection_rate_per_inf_type_table"]]) {
        leave_empty("infectious_agent_detection_rate_per_inf_type_table",
                    tbl, "inf")
      } else {
        output$infectious_agent_detection_rate_per_inf_type_table <-
          output$infectious_agent_detection_rate_per_inf_type_table |>
          dplyr::full_join(tbl, dplyr::join_by("inf")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"),
                dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )
      }
    }
    if (!is.na(joins[["resistance_test_rate_table"]])) {
      tbl <- ds$resistance_test_rate_table
      key_cols <- intersect(names(tbl), c("abr", "cond"))
      tbl <- tbl |>
        dplyr::rename_with(
          ~ paste0(.x, suffix), !tidyselect::any_of(key_cols))

      if (is.null(output$resistance_test_rate_table)) {
        output$resistance_test_rate_table <- tbl
      } else if (!joins[["resistance_test_rate_table"]]) {
        leave_empty("resistance_test_rate_table", tbl, key_cols)
      } else {
        output$resistance_test_rate_table <-
          output$resistance_test_rate_table |>
          dplyr::full_join(tbl, dplyr::join_by("abr", "cond")) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("n_"),
              ~tidyr::replace_na(.x, 0)),
            dplyr::across(
              c(dplyr::starts_with("pooled_"), dplyr::starts_with("q"),
                dplyr::starts_with("ci_lower_"),
                dplyr::starts_with("ci_upper_")),
              ~tidyr::replace_na(.x, NA_real_))
          )
      }
    }
  }

  # NEOIPC-PERMANENT(dataset-format): see .rate_definition_stamps(). A
  # dataset calculated before these tables had `node_id` carries them with
  # `taxon` and `.ontology_path` instead, its resistance rates per infectious
  # agent in their earlier slot (`.benchmark_table()`); it joins with no other
  # dataset, so where it holds such a table first, or alone, the table keeps
  # its rows as they are, which no key orders or completes.
  for (tbl_name in names(agent_suffixes))
    if ("node_id" %in% names(output[[tbl_name]]))
      output[[tbl_name]] <- .complete_infectious_agent_table(
        output[[tbl_name]], tbl_name, agent_suffixes[[tbl_name]],
        agent_order[[tbl_name]]$rows)
  # A later join may have filled in a column left empty, such as a count
  # set to 0 where a row lacked it.
  for (tbl_name in names(left_empty))
    for (col in left_empty[[tbl_name]])
      output[[tbl_name]][[col]][] <- NA
  .warn_tables_not_comparable(output$not_comparable, x)

  structure(output, class = c("neoipcr_bnch_ds", "list"))
}

#' Prettify the names of a neoipcr object
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns the same object as x but with pretty and potentially translated
#'  names
#' @export
pretty_names <- function(x, ...) {
  UseMethod("pretty_names")
}

#' @export
pretty_names.default <- function(x, ...) x

#' @export
pretty_names.neoipcr_tbl_sr_ref <- function(x, ...) {
  col_names <- stats::setNames(
    gettext("Procedure category","N","Pooled","Q1","Q2","Q3"),
    c("pro_cat","n","pooled","q1","q2","q3"))

  pairs <- x |>
    dplyr::select("pro_cat") |>
    dplyr::mutate(
      pretty_name = get_procedure_category_pretty(.data$pro_cat))

  row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

  attr(x, "names.pretty") <- col_names
  attr(x, "row.names.pretty") <- row_names

  x |>
    dplyr::inner_join(pairs, dplyr::join_by("pro_cat")) |>
    dplyr::mutate(pro_cat = .data$pretty_name, .keep = "unused") |>
    dplyr::rename_with(
      ~ dplyr::recode_values(
        .x,
        "pro_cat"~col_names[["pro_cat"]],
        "n"~col_names[["n"]],
        "pooled"~col_names[["pooled"]],
        "q1"~col_names[["q1"]],
        "q2"~col_names[["q2"]],
        "q3"~col_names[["q3"]],
        default = .x))
}

quartile_probs <- c(0.25,0.5,0.75)

ensure_quartile_cols <- function(df) {
  for (col in c("q1", "q2", "q3")) {
    if (!col %in% names(df))
      df[[col]] <- NA_real_
  }
  df
}

# Recompute Poisson CIs for zero-event rows created by full_join or
# upstream missing-level backfill.  A rate of 0 with a known denominator
# has a valid CI (lower = 0, upper > 0). A denominator left empty, where the
# dataset's counts were not joined, gives none.
fix_zero_event_ci <- function(tbl, suffixes, denominator_tbl,
                              denom_prefix = "total",
                              n_prefix = "n",
                              multiplier = 100) {
  for (sfx in suffixes[suffixes != ""]) {
    n_col  <- paste0(n_prefix, sfx)
    ci_lo  <- paste0("ci_lower", sfx)
    ci_hi  <- paste0("ci_upper", sfx)
    d_col  <- paste0(denom_prefix, sfx)
    if (all(c(n_col, ci_lo) %in% names(tbl)) &&
        d_col %in% names(denominator_tbl)) {
      zero <- !is.na(tbl[[n_col]]) & tbl[[n_col]] == 0 & is.na(tbl[[ci_lo]])
      if (any(zero) && isTRUE(denominator_tbl[[d_col]] > 0)) {
        ci <- neoipc_poisson_ci(0, denominator_tbl[[d_col]],
                                 multiplier = multiplier)
        tbl[[ci_lo]][zero] <- ci$lower
        tbl[[ci_hi]][zero] <- ci$upper
      }
    }
  }
  tbl
}

get_countries_with_wb_class <- function(x) {
  # `metadata$countries` is always a tibble under the three-mode schema
  # contract. Gate on `displayName` presence — it only appears under
  # `include_country == "full"`. The helper's name-based output only
  # makes sense under "full" where labels exist.
  if ("displayName" %in% names(x$metadata$countries)) {
    countries_data <- x$enrollments |>
      dplyr::inner_join(
        x$metadata$countries |>
          dplyr::select("country_key", "displayName"),
        dplyr::join_by("country_key")) |>
        dplyr::select(name = "displayName", tidyselect::any_of("world_bank_class_key")) |>
        dplyr::distinct()

    # Join with worldBankClasses to get stable `class` label. Gate on
    # column presence rather than null-ness: under the three-mode schema
    # contract `worldBankClasses` is always a tibble, but `class` only
    # appears under `include_world_bank_class == "full"`.
    if ("class" %in% names(x$metadata$worldBankClasses) &&
        "world_bank_class_key" %in% names(countries_data)) {
      countries_data |>
        dplyr::left_join(
          x$metadata$worldBankClasses |>
            dplyr::select("world_bank_class_key", wb_class = "class"),
          dplyr::join_by("world_bank_class_key")) |>
        dplyr::select("name", "wb_class") |>
        dplyr::distinct() |>
        dplyr::arrange(.data$name)
    } else {
      countries_data |>
      dplyr::select("name") |>
      dplyr::arrange(.data$name)
    }
  } else {
  NULL
  }
}

# Calculates effective analysis period from actual surveillance end dates
get_effective_analysis_period <- function(x) {
  surveillance_end_dates <- x$events |>
    dplyr::filter(.data$event_type_key == "end") |>
    dplyr::pull(.data$occurredAt)

  if (length(surveillance_end_dates) > 0) {
    return(
      list(
        from = min(surveillance_end_dates, na.rm = TRUE),
        to = max(surveillance_end_dates, na.rm = TRUE)))
  }

  return(NULL)
}

# ---- Versions of the rate definitions ------------------------------------

# The version of the definitions of the rates of each table a calculated
# dataset carries, by the table's slot. A change to what a table counts, over
# what, on what scale, with what interval and sparse-data flag, or which
# departments enter its quartiles raises its version and no other, so that
# get_benchmark_data() joins the table only with one computed under the same
# definitions. The two versions of a table whose rows are infectious agents
# share its definitions.
rate_definition_versions <- c(
  n_departments = 1L,
  n_surgical_departments = 1L,
  n_patients = 1L,
  n_enrollments = 1L,
  n_patient_days = 1L,
  n_surgical_patients = 1L,
  n_surgical_procedures = 1L,
  birth_weight_figure = 1L,
  gestational_age_figure = 1L,
  usage_density_rate_table = 1L,
  antibiotic_utilization_table = 1L,
  surgery_rate_table = 1L,
  incidence_density_rate_table = 1L,
  dev_ass_incidence_density_rate_table = 1L,
  n_infections = 1L,
  infectious_agent_detection_rate_per_agent_table = 1L,
  infectious_agent_detection_rate_per_agent_ontology_table = 1L,
  abr_infection_rate_table = 1L,
  abr_infection_rate_ontology_table = 1L,
  infectious_agent_resistance_rate_table = 1L,
  infectious_agent_resistance_rate_ontology_table = 1L,
  secondary_bsi_rate_table = 1L,
  infectious_agent_detection_rate_per_inf_type_table = 1L,
  resistance_test_rate_table = 1L)

# The columns of `metadata$rate_definitions`: the table's slot, the version of
# its rate definitions, and the stamps of what it reads from the bundled
# infectious-agent list, each digest named after the digest of the
# provenance (`internal_infectious_agent_ontology`) it carries.
rate_definition_columns <- c(
  "table", "version", "common_commensal_digest", "concept_mapping_digest",
  "groups_tree_digest", "ontology_tree_digest", "layout",
  "applicability_digest")

# The slots of the tables whose rows are infectious agents, both versions of
# each.
.infectious_agent_table_slots <- function()
  unname(c(infectious_agent_detection_table_names,
           abr_infection_rate_table_names,
           infectious_agent_resistance_rate_table_names))

# What each table reads from the bundled infectious-agent list, by the stamps
# that cover it, for the tables that read it:
#   1. the common-commensal flags, on which the rate definitions of the
#      incidence densities split the kinds of sepsis
#      (`common_commensal_digest`);
#   2. the concept each Id names, by which the tables whose rows are
#      infectious agents and the completeness of antimicrobial susceptibility
#      testing count an infectious agent once per infection
#      (`concept_mapping_digest`);
#   3. the tree of the version a table whose rows are infectious agents is
#      laid out over (`groups_tree_digest` or `ontology_tree_digest`), and the
#      `layout` of its rows;
#   4. to which infectious agents each resistance category applies
#      (`applicability_digest`), which the tables of the resistance categories
#      read.
# A table carries the digests of what it reads and no other, so that a
# release of the list that changes only what it does not read leaves it
# comparable.
.infectious_agent_list_reads <- function() {
  tree <- function(rows)
    c("concept_mapping_digest", paste0(rows, "_tree_digest"), "layout")
  applicability <- "applicability_digest"
  reads <- list(incidence_density_rate_table = "common_commensal_digest")
  for (rows in names(infectious_agent_detection_table_names)) {
    reads[[infectious_agent_detection_table_names[[rows]]]] <- tree(rows)
    for (slot in c(abr_infection_rate_table_names[[rows]],
                   infectious_agent_resistance_rate_table_names[[rows]]))
      reads[[slot]] <- c(tree(rows), applicability)
  }
  reads$resistance_test_rate_table <- c("concept_mapping_digest",
                                        applicability)
  reads
}

# The stamps of the tables of a calculated dataset, its
# `metadata$rate_definitions`: one row per table, with the columns
# `rate_definition_columns` name, each digest taken from `provenance`, the
# provenance of the bundled infectious-agent list, and each stamp of the list
# `NA` for a table that does not read what it covers.
.rate_definitions <- function(provenance = internal_infectious_agent_ontology) {
  tables <- names(rate_definition_versions)
  reads <- .infectious_agent_list_reads()
  read <- function(stamp)
    tables %in% names(reads)[vapply(reads, \(r) stamp %in% r, logical(1))]
  digest <- function(stamp)
    dplyr::if_else(read(stamp), provenance[[stamp]], NA_character_)
  tibble::tibble(
    table = tables,
    version = unname(rate_definition_versions),
    common_commensal_digest = digest("common_commensal_digest"),
    concept_mapping_digest = digest("concept_mapping_digest"),
    groups_tree_digest = digest("groups_tree_digest"),
    ontology_tree_digest = digest("ontology_tree_digest"),
    layout = dplyr::if_else(
      read("layout"), infectious_agent_table_layout, NA_integer_),
    applicability_digest = digest("applicability_digest"))
}

# The release of the bundled infectious-agent list a calculated dataset was
# built from, and the layout of its tables whose rows are infectious agents,
# its `metadata$infectious_agent_tables`. The digests of what each table reads
# from the list are its stamps in `metadata$rate_definitions`.
.infectious_agent_tables <- function() {
  provenance <- internal_infectious_agent_ontology
  list(
    version = provenance$version,
    commit = provenance$commit,
    layout = infectious_agent_table_layout)
}

# The version of neoipcr that calculates a dataset.
.neoipcr_version <- function()
  as.character(getNamespaceVersion("neoipcr"))

# Warn once where the findings of `x` hold codes the bundled
# infectious-agent list lacks, which the infectious-agent tables count in
# their Totals only.
.warn_unknown_infectious_agent_codes <- function(x) {
  codes <- as.integer(x$infectiousAgentFindings$pathogen_key)
  codes <- sort(unique(codes[
    !is.na(codes) &
      !codes %in% internal_infectious_agent_classification$input_id]))
  if (length(codes) == 0L)
    return(invisible(x))
  rlang::warn(
    c(sprintf(ngettext(length(codes),
                       "The findings hold %d code that is not in the NeoIPC infectious-agent list.",
                       "The findings hold %d codes that are not in the NeoIPC infectious-agent list."),
              length(codes)),
      "x" = gettextf("Not in the list: %s.", paste(codes, collapse = ", ")),
      "i" = gettext("The infectious-agent tables count such a code in their Totals only, and no resistance category applies to it.")),
    class = "neoipcr_unknown_infectious_agent_code",
    codes = codes)
  invisible(x)
}

# The stamps a calculated dataset carries of its tables, or `NULL` where it
# carries none: it has no `metadata$rate_definitions` data frame, or one
# without the columns `table` and `version`.
#
# NEOIPC-PERMANENT(dataset-format): never treat a dataset without the stamps
# as comparable, nor refuse it, nor count a dataset as without them for
# lacking a stamp column that a later release added. A calculated dataset
# serialized before the stamps existed has no `metadata$rate_definitions`,
# one serialized before a release added a stamp column lacks that column, and
# a file on disk outlives the code that wrote it, so no condition retires
# this. `NULL` makes every table of a dataset without stamps count as computed
# under other definitions, which get_benchmark_data() does not join, where
# without it the benchmark would abort on the missing stamps or join rates of
# two definitions. A stamp column a dataset lacks reads as `NA`
# (`.rate_definition_stamp()`), so that only the tables whose stamp in the
# other dataset has a value there are not joined, where counting the dataset
# as without stamps would leave every table of it unjoined, those whose
# definitions the release left as they were among them.
.rate_definition_stamps <- function(ds) {
  stamps <- ds$metadata$rate_definitions
  if (!is.data.frame(stamps) ||
      !all(c("table", "version") %in% names(stamps)))
    return(NULL)
  stamps
}

# The stamps of the table `slot` of the calculated dataset `ds`, as text in
# the order of `rate_definition_columns`, a column the dataset lacks as `NA`
# (NEOIPC-PERMANENT(dataset-format): see .rate_definition_stamps()), or `NULL`
# where it carries none of that table.
.rate_definition_stamp <- function(ds, slot) {
  stamps <- .rate_definition_stamps(ds)
  if (is.null(stamps))
    return(NULL)
  row <- stamps[stamps$table %in% slot, , drop = FALSE]
  if (nrow(row) != 1L)
    return(NULL)
  vapply(rate_definition_columns, function(column)
    if (column %in% names(row)) as.character(row[[column]]) else NA_character_,
    character(1))
}

# The table `slot` of the calculated dataset `ds` that get_benchmark_data()
# joins, `NULL` where it holds none.
#
# NEOIPC-PERMANENT(dataset-format): see .rate_definition_stamps(). A dataset
# calculated before the stamps holds its resistance rates per infectious
# agent in the slot `organism_resistance_rate_table`, which counts as its
# `infectious_agent_resistance_rate_table`; without it the benchmark would
# hold no resistance rates of such a dataset placed first or alone, and,
# placed later, would neither join nor list them as not comparable, so that a
# department's resistance rates would show without the reference's columns
# and without a warning.
.benchmark_table <- function(ds, slot) {
  if (slot == "infectious_agent_resistance_rate_table" &&
      is.null(ds[[slot]]) && is.null(.rate_definition_stamps(ds)))
    return(ds[["organism_resistance_rate_table"]])
  ds[[slot]]
}

# Whether the dataset `ds` holds the table `slot` that get_benchmark_data()
# joins (`.benchmark_table()`): a table without rows of the secondary-BSI
# rates, the testing by infection type, or the completeness of antimicrobial
# susceptibility testing, and a table without columns whose rows are
# infectious agents, count as none.
.benchmark_carries <- function(ds, slot) {
  value <- .benchmark_table(ds, slot)
  if (is.null(value))
    return(FALSE)
  if (slot %in% c("secondary_bsi_rate_table",
                  "infectious_agent_detection_rate_per_inf_type_table",
                  "resistance_test_rate_table"))
    return(nrow(value) > 0L)
  if (slot %in% .infectious_agent_table_slots())
    return(ncol(value) > 0L)
  TRUE
}

# For each table get_benchmark_data() joins, by its slot, whether each dataset
# of `x` joins it: `NA` where the dataset does not hold the table, `TRUE` for
# the first dataset that holds it and for each later one whose stamps of the
# table match that dataset's, and `FALSE` for the others.
.benchmark_comparability <- function(x) {
  purrr::map(
    rlang::set_names(names(rate_definition_versions)),
    function(slot) {
      holds <- vapply(x, .benchmark_carries, logical(1), slot = slot,
                      USE.NAMES = FALSE)
      joins <- rep(NA, length(x))
      if (!any(holds))
        return(joins)
      first <- which(holds)[1]
      stamp <- .rate_definition_stamp(x[[first]], slot)
      for (i in which(holds))
        joins[i] <- i == first ||
          (!is.null(stamp) &&
             identical(stamp, .rate_definition_stamp(x[[i]], slot)))
      joins
    })
}

# The tables get_benchmark_data() does not join, by `comparable`
# (`.benchmark_comparability()`): one row per table and dataset not joined,
# with the table's slot and the dataset's name, in the order of the tables
# and then of the datasets.
.benchmark_not_comparable <- function(comparable, ds_names)
  purrr::list_rbind(c(
    list(tibble::tibble(table = character(), dataset = character())),
    unname(purrr::imap(comparable, \(joins, table)
      tibble::tibble(table = table,
                     dataset = ds_names[joins %in% FALSE])))))

# `prev`, the benchmark's table so far, with the columns of `tbl`, a later
# dataset's table, beyond `by` added and left empty.
.benchmark_empty_columns <- function(prev, tbl, by = character()) {
  added <- tbl[rep(NA_integer_, nrow(prev)), setdiff(names(tbl), by),
               drop = FALSE]
  dplyr::bind_cols(prev, added)
}

# Warn once about the tables of `not_comparable`
# (`.benchmark_not_comparable()`) of the datasets `x`.
.warn_tables_not_comparable <- function(not_comparable, x) {
  if (nrow(not_comparable) == 0L)
    return(invisible())
  ds_names <- rlang::names2(x)
  label <- function(name)
    if (name == "") gettext("the unnamed dataset") else paste0("`", name, "`")
  datasets <- unique(not_comparable$dataset)
  left_empty <- vapply(datasets, function(name)
    gettextf("Left empty for %s: %s.", label(name),
             paste0("`", not_comparable$table[not_comparable$dataset == name],
                    "`", collapse = ", ")),
    character(1), USE.NAMES = FALSE)
  unstamped <- ds_names[vapply(
    x, \(ds) is.null(.rate_definition_stamps(ds)), logical(1),
    USE.NAMES = FALSE)]
  recalculate <- vapply(unique(unstamped), function(name)
    gettextf("%s was calculated before calculated datasets carried the versions of their rate definitions; calculate it again from its raw dataset to compare it.",
             label(name)),
    character(1), USE.NAMES = FALSE)
  n <- length(unique(not_comparable$table))
  rlang::warn(
    c(sprintf(ngettext(n,
                       "%d table was calculated under other rate definitions, or from another release of the NeoIPC infectious-agent list, than in the first dataset that holds it, and is not joined.",
                       "%d tables were calculated under other rate definitions, or from another release of the NeoIPC infectious-agent list, than in the first dataset that holds them, and are not joined."),
              n),
      rlang::set_names(left_empty, rep("x", length(left_empty))),
      rlang::set_names(recalculate, rep("i", length(recalculate))),
      "i" = gettext("Each such table keeps the rows of the datasets joined into it; a figure takes no rows from a dataset for which it is left empty, and any other such table leaves that dataset's columns empty.")),
    class = "neoipcr_tables_not_comparable",
    not_comparable = not_comparable)
}

# ---- The benchmark's tables whose rows are infectious agents -------------

# The benchmark's table `tbl` whose rows are infectious agents, held in the
# slot `slot` and joined from the datasets whose columns carry `suffixes`,
# completed: its rows in the order `.benchmark_row_order()` gives them by
# `reference`, and the measures of each dataset on a row it lacks set to 0
# events over its own denominator of the row, with the interval of 0 events.
# A dataset lacks a row only where it records no infectious agent at or
# beneath the row's node (for a residual row, at its parent's node), and in a
# resistance category's block none to which the list applies the category or
# that has a result for it; whether it has the row depends on nothing else
# (`.infectious_agent_memberships()`, `.infectious_agent_block_nodes()`). So
# the dataset has none of the row's events, over the denominator its own rows
# share:
#   1. the infectious agents detected: its infections with an infectious agent
#      in the tree, in total and of each type, the denominator of every row
#      but the Total, which every row's quartile departments share too;
#   2. the infections with resistant infectious agents: its patient-days, the
#      denominator of every row, with the quartile departments of the
#      category's Total;
#   3. the resistance rates: none, since no infection has a result for the
#      category in an infectious agent of the row's node, nor one to which
#      the category applies, so that the row has neither a rate nor a Tested
#      share, and no department enters its quartiles.
# The quartiles of a row a dataset lacks stay empty, its pooled rate being 0
# or missing. The Totals are in every dataset.
.complete_infectious_agent_table <- function(tbl, slot, suffixes, reference) {
  tbl <- tbl[.benchmark_row_order(tbl, reference), ]
  for (suffix in suffixes) {
    col <- function(name) paste0(name, suffix)
    held <- !is.na(tbl[[col("n")]])
    lacking <- !held & !is.na(tbl$node_id)
    if (!any(lacking))
      next
    # The value the dataset's rows `rows` share, `default` where it has none.
    shared <- function(name, rows = held & !is.na(tbl$node_id),
                       default = 0L) {
      values <- tbl[[col(name)]][rows]
      if (length(values) > 0L) values[[1]] else default
    }
    quartile_departments <- col("quartile_departments")
    if (slot %in% infectious_agent_detection_table_names) {
      tbl <- .zero_event_measures(tbl, lacking, col, shared("total"),
                                  100, "wilson")
      for (type in infectious_agent_infection_types)
        tbl <- .zero_event_measures(
          tbl, lacking, \(name) col(paste0(name, "_", type)),
          shared(paste0("total_", type)), 100, "wilson")
      if (quartile_departments %in% names(tbl))
        tbl[[quartile_departments]][lacking] <- shared("quartile_departments")
    } else if (slot %in% abr_infection_rate_table_names) {
      tbl <- .zero_event_measures(tbl, lacking, col,
                                  shared("total", rows = held),
                                  1000, "poisson")
      if (quartile_departments %in% names(tbl)) {
        category <- as.character(tbl$abr)
        for (block in unique(category[lacking]))
          tbl[[quartile_departments]][lacking & category == block] <-
            shared("quartile_departments",
                   rows = held & is.na(tbl$node_id) & category == block)
      }
    } else {
      tbl <- .zero_event_measures(tbl, lacking, col, 0L, 100, "wilson")
      tbl <- .zero_event_measures(
        tbl, lacking, \(name) col(paste0("tested_", name)), 0L, 100,
        "wilson")
      if (quartile_departments %in% names(tbl))
        tbl[[quartile_departments]][lacking] <- 0L
    }
  }
  tbl
}

# `tbl` with the measures named by `name` (a function of the measure: `n`,
# `total`, `pooled`, `ci_lower`, or `ci_upper`) set, on the rows `rows`, to 0
# events over `denominator` on the scale `scale`, with the `interval` of 0
# events, and no rate without a denominator.
.zero_event_measures <- function(tbl, rows, name, denominator, scale,
                                 interval = c("wilson", "poisson")) {
  interval <- rlang::arg_match(interval)
  if (!name("n") %in% names(tbl))
    return(tbl)
  events <- rep(0L, sum(rows))
  exposure <- rep(denominator, sum(rows))
  ci <- if (interval == "wilson")
    wilson_ci_cols(events, exposure, scale = scale)
  else
    poisson_ci_cols(events, exposure, multiplier = scale)
  tbl[[name("n")]][rows] <- 0L
  tbl[[name("total")]][rows] <- denominator
  tbl[[name("pooled")]][rows] <- if (isTRUE(denominator > 0)) 0 else NA_real_
  tbl[[name("ci_lower")]][rows] <- ci$ci_lower
  tbl[[name("ci_upper")]][rows] <- ci$ci_upper
  tbl
}

# The order of the rows of the benchmark's table `tbl` whose rows are
# infectious agents, by `reference`, the rows (`node_id`, within `abr` where
# the table has resistance categories) of the dataset whose order it takes:
# the categories in their order, and within each, its Total, then the summary
# block of the reporting groups, then the tree, each row after its parent.
#   1. A summary row is one whose `node_id` names, before its `|`, an
#      attribute of the bundled classification, whatever its value
#      (`.is_summary_row()`). Every block's summary rows are laid out
#      attribute by attribute in the order of `reference`, read from its
#      summary rows of every block (`.benchmark_reference_summary()`,
#      `.benchmark_summary_order()`): the attributes, and the values that
#      `reference` or the bundled classification holds, in one order for
#      every block, and the values that neither holds in their order in
#      `tbl`, which can differ between blocks.
#   2. Siblings keep the order of `reference`, any other row `reference`
#      lacks following its siblings, in its order in `tbl`, which is the
#      order in which the datasets were joined.
#   3. A residual row stays last among its node's children.
# Each dataset lays out its summary rows in its own bundle's order of the
# attributes and their values. No digest covers that order, so a reference
# calculated under another release of the list can hold them in another
# order than this bundle's and still be joined. A dataset calculated under a
# release that adds or renames a value can hold a value this bundle lacks;
# the digest of the tree over the reporting groups covers the values' keys,
# so such a dataset is joined only with datasets calculated under that
# release, or benchmarked alone. So `reference` orders the attributes and
# values it holds in any block of the table, each block showing its
# release's order of those the block holds, and this bundle's order places
# the others and decides place by place what the blocks leave open
# (`.benchmark_reference_summary()`). A summary row of an attribute this
# bundle lacks, which only a dataset calculated by a version of neoipcr whose
# bundle has that attribute can hold, is not recognized as one and is placed
# as a top-level row of the tree.
.benchmark_row_order <- function(tbl, reference) {
  blocks <- "abr" %in% names(tbl)
  key <- function(d)
    if (blocks) paste(d$abr, d$node_id, sep = "\u001f") else d$node_id
  block <- if (blocks)
    match(as.character(tbl$abr), infectious_agent_resistance_categories)
  else
    rep(1L, nrow(tbl))
  summary <- .is_summary_row(tbl$node_id)
  position <- match(key(tbl), key(reference))
  # A summary row's position is its rank in its block's summary rows, which
  # their section keeps apart from the other rows' positions.
  reference_order <- .benchmark_reference_summary(reference)
  for (b in unique(block[summary])) {
    rows <- which(summary & block == b)
    position[rows] <- .benchmark_summary_order(tbl$node_id[rows],
                                               reference_order)
  }
  unplaced <- is.na(position)
  position[unplaced] <- nrow(reference) + seq_len(sum(unplaced))
  section <- dplyr::case_when(
    is.na(tbl$node_id) ~ 1L,
    summary ~ 2L,
    .default = 3L)
  residual <- endsWith(dplyr::coalesce(tbl$node_id, ""),
                       infectious_agent_residual_suffix)

  sorted <- order(block, section, residual, position)
  self <- paste(block, tbl$node_id, sep = "\u001f")[sorted]
  parent <- paste(block, tbl$parent_id, sep = "\u001f")[sorted]
  top <- is.na(tbl$parent_id[sorted]) | !parent %in% self
  children <- split(
    which(!top), factor(parent[!top], levels = unique(parent[!top])))
  visited <- integer()
  visit <- function(i) {
    visited <<- c(visited, i)
    for (child in children[[self[i]]])
      visit(child)
  }
  for (i in which(top))
    visit(i)
  sorted[visited]
}

# Whether each `node_id` is that of a summary row: one that names, before
# its `|`, an attribute of the bundled classification, whatever its value.
# The attributes are neoipcr's column names, `gram_reaction` and
# `oxygen_requirement`, which no group's key, lower-case words joined by
# hyphens, can be.
.is_summary_row <- function(node_id)
  .summary_row_attribute(node_id) %in%
    internal_infectious_agent_attribute_values$attribute

# The attribute a summary row's `node_id`, `<attribute>|<value>`, names.
.summary_row_attribute <- function(node_id)
  sub("\\|.*$", "", node_id)

# The order of the attributes and their values that `reference`, the rows
# (`node_id`, within `abr` where the table has resistance categories) of the
# dataset whose order the benchmark's table takes, gives its summary rows:
# their `node_id`s, each once, attribute by attribute. Each block of
# `reference` holds its summary rows in the order of the release of the list
# it was calculated from, so this order keeps the order of every block: an
# attribute, or a value of one, comes after those a block places before it.
# Where the blocks leave the order open, as for two attributes, or two values,
# that the blocks order neither directly nor through others, the bundled
# classification decides it place by place as far as the blocks allow, a
# value it lacks after those it holds, and such values in their order in
# `reference` (`.merge_chains()`).
.benchmark_reference_summary <- function(reference) {
  values <- internal_infectious_agent_attribute_values
  attributes <- unique(values$attribute)
  bundled <- paste0(values$attribute, "|", values$key)
  summary <- .is_summary_row(reference$node_id)
  block <- if ("abr" %in% names(reference))
    as.character(reference$abr)
  else
    rep("", nrow(reference))
  chains <- unname(split(
    reference$node_id[summary],
    factor(block[summary], levels = unique(block[summary]))))
  ordered <- .merge_chains(
    lapply(chains, \(chain) unique(.summary_row_attribute(chain))),
    \(a) match(a, attributes))
  as.character(unlist(lapply(ordered, \(a)
    .merge_chains(
      lapply(chains, \(chain) chain[.summary_row_attribute(chain) == a]),
      \(node_id) match(node_id, bundled)))))
}

# The elements of the character vectors `chains`, each once, in an order that
# keeps the order of every chain: each in turn, of the elements no chain
# places after one not yet placed, the first by `fallback`, a function giving
# the elements their ranks in another order, `NA` last, ties by their first
# appearance in `chains`. Chains that contradict one another, which no single
# order lays out, leave no element free at some turn; the first unplaced one
# by `fallback` then comes next, so that every element is placed.
.merge_chains <- function(chains, fallback) {
  elements <- as.character(unique(unlist(chains, use.names = FALSE)))
  priority <- order(order(fallback(elements), seq_along(elements)))
  before <- lapply(elements, \(element) unlist(lapply(chains, \(chain)
    chain[seq_len(max(0L, match(element, chain, nomatch = 0L) - 1L))])))
  placed <- logical(length(elements))
  merged <- integer()
  while (!all(placed)) {
    free <- !placed &
      vapply(before, \(b) all(b %in% elements[placed]), logical(1))
    candidates <- which(if (any(free)) free else !placed)
    next_element <- candidates[which.min(priority[candidates])]
    placed[next_element] <- TRUE
    merged <- c(merged, next_element)
  }
  elements[merged]
}

# The ranks of the summary rows of one block of the benchmark's table, given
# their `node_id`s, in their order in the table, and `reference_order`, the
# order of the attributes and their values that the reference gives
# (`.benchmark_reference_summary()`): the rows are laid out attribute by
# attribute, the attributes, and the values that the reference or the
# classification holds, in one order for every block, and the values that
# neither holds in their order in the table, which can differ between
# blocks.
#   1. The attributes are in the reference's order; an attribute of which the
#      reference holds no row in any block comes directly after the
#      reference's attribute that the reference places last among those the
#      classification orders before it, or first.
#   2. Within an attribute, the values the reference holds in any block are
#      in its order, whether or not the classification holds them, and so in
#      a block where the reference lacks some of their rows.
#   3. A value the reference holds in no block comes directly after the
#      reference's value of its attribute that the reference places last
#      among those the classification orders before its own, or, where there
#      is none, directly before the reference's first value of its
#      attribute; a value the classification lacks comes last in its
#      attribute's run.
#   4. Rows that share a place are in the classification's order, those
#      whose value it lacks in their order in the table, which is the order
#      in which the datasets were joined, and so are attributes that share
#      one, so that each attribute's run stays contiguous, the rows of the
#      earlier run's attribute first where two runs meet between two of the
#      reference's values.
.benchmark_summary_order <- function(node_id, reference_order) {
  values <- internal_infectious_agent_attribute_values
  attributes <- unique(values$attribute)
  bundled <- paste0(values$attribute, "|", values$key)
  attribute <- .summary_row_attribute(node_id)
  value <- match(node_id, bundled)
  reference_attribute <- .summary_row_attribute(reference_order)
  theirs <- unique(reference_attribute)

  # Each attribute's place: the reference's attributes at 1, 2, and so on, in
  # its order, and an attribute the reference holds no row of at the place of
  # the attribute it follows, or 0, after that one.
  attribute_place <- match(attribute, theirs)
  attribute_tie <- integer(length(node_id))
  for (lacking in setdiff(attribute, theirs)) {
    rows <- attribute == lacking
    attribute_rank <- match(lacking, attributes)
    attribute_place[rows] <-
      max(c(0L, which(match(theirs, attributes) < attribute_rank)))
    attribute_tie[rows] <- attribute_rank
  }

  # Each row's place within its attribute's run: the reference's values at 1,
  # 2, and so on, in its order, a value the reference holds in no block at
  # the place of the value it follows, or 0, after that one, and a value the
  # classification lacks after the reference's last.
  row_place <- integer(length(node_id))
  row_tie <- integer(length(node_id))
  for (a in unique(attribute)) {
    rows <- which(attribute == a)
    held <- reference_order[reference_attribute == a]
    row_place[rows] <- match(node_id[rows], held)
    for (i in rows[is.na(row_place[rows])]) {
      if (is.na(value[i])) {
        row_place[i] <- length(held) + 1L
        row_tie[i] <- i
      } else {
        row_place[i] <- max(c(0L, which(match(held, bundled) < value[i])))
        row_tie[i] <- value[i]
      }
    }
  }
  order(order(attribute_place, attribute_tie, row_place, row_tie))
}
