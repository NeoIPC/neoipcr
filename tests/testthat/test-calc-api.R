# Tests for R/calc-api.R — calculate_department_data() integration test.
# Uses make_calc_test_ds() from helper-fixtures.R.

test_that("calculate_department_data produces neoipcr_rep_ds", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  expect_s3_class(result, "neoipcr_rep_ds")

  # All expected slots present
  expected_slots <- c(
    "metadata", "birth_weight_figure", "gestational_age_figure",
    "n_departments", "n_patients", "n_enrollments", "n_patient_days",
    "n_infections", "n_surgical_departments",
    "n_surgical_procedures", "n_surgical_patients",
    "usage_density_rate_table", "antibiotic_utilization_table",
    "surgery_rate_table", "incidence_density_rate_table",
    "dev_ass_incidence_density_rate_table",
    "infectious_agent_detection_rate_per_agent_table",
    "infectious_agent_detection_rate_per_agent_ontology_table",
    "abr_infection_rate_table", "abr_infection_rate_ontology_table",
    "infectious_agent_resistance_rate_table",
    "infectious_agent_resistance_rate_ontology_table",
    "secondary_bsi_rate_table",
    "infectious_agent_detection_rate_per_inf_type_table",
    "resistance_test_rate_table")
  expect_true(all(expected_slots %in% names(result)))
})

test_that("both calculation functions carry both versions of the table of the infectious agents detected", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  for (calculated in list(own, ref)) {
    groups <- calculated$infectious_agent_detection_rate_per_agent_table
    ontology <- calculated$infectious_agent_detection_rate_per_agent_ontology_table
    # The codes 1 and 503 are Azorhizobium caulinodans, a non-fermenting
    # Gram-negative bacillus, and the genus Enterococcus.
    expect_true(all(c("bacteria", "non-fermenting-gram-negative-bacilli",
                      "gram-positive-cocci") %in% groups$node_id))
    expect_true(all(c("LPSN|domain/bacteria", "LPSN|genus/enterococcus") %in%
                      ontology$node_id))
  }
  expect_true("q1" %in% names(
    ref$infectious_agent_detection_rate_per_agent_ontology_table))
  expect_false("q1" %in% names(
    own$infectious_agent_detection_rate_per_agent_ontology_table))

  # The benchmark joins each version on its own rows.
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(
    benchmark$infectious_agent_detection_rate_per_agent_ontology_table$node_id,
    own$infectious_agent_detection_rate_per_agent_ontology_table$node_id)
  expect_equal(
    benchmark$infectious_agent_detection_rate_per_agent_table$n_ref,
    ref$infectious_agent_detection_rate_per_agent_table$n)
})

test_that("both calculation functions carry both versions of the resistance tables, and the benchmark joins each on its rows", {
  # The fixture's findings hold a code the list lacks.
  ds <- make_resistance_calc_test_ds()
  expect_warning(own <- calculate_department_data(ds, use_cache = FALSE),
                 class = "neoipcr_unknown_infectious_agent_code")
  expect_warning(ref <- calculate_reference_data(ds, use_cache = FALSE),
                 class = "neoipcr_unknown_infectious_agent_code")
  slots <- c("abr_infection_rate_table", "abr_infection_rate_ontology_table",
             "infectious_agent_resistance_rate_table",
             "infectious_agent_resistance_rate_ontology_table")
  for (slot in slots) {
    expect_true(all(c("abr", "node_id", "group", "n", "pooled") %in%
                      names(own[[slot]])))
    expect_false("q1" %in% names(own[[slot]]))
    expect_true("q1" %in% names(ref[[slot]]))
  }
  expect_true("LPSN|genus/enterococcus~n.o.s." %in%
                own$abr_infection_rate_ontology_table$node_id)
  expect_false("organism_resistance_rate_table" %in% names(own))

  benchmark <- get_benchmark_data(own = own, ref = ref)
  for (slot in slots) {
    joined <- benchmark[[slot]]
    expect_equal(paste(joined$abr, joined$node_id),
                 paste(own[[slot]]$abr, own[[slot]]$node_id))
    expect_equal(joined$n_ref, ref[[slot]]$n)
    expect_equal(joined$pooled_own, own[[slot]]$pooled)
  }
})

test_that("calculate_department_data computes correct summary counts", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  expect_equal(result$n_patients$total, 3L)
  expect_equal(result$n_enrollments$total, 3L)
  expect_equal(result$n_departments, 2L)
  # Patient days = sum of surveillance end patient_days: 15 + 16 + 16 = 47
  expect_equal(result$n_patient_days$total, 47L)
})

test_that("calculate_department_data usage_density_rate_table has expected structure", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  udr <- result$usage_density_rate_table
  expect_s3_class(udr, "tbl_df")
  expect_true("factor" %in% names(udr))
  expect_true("days" %in% names(udr))
  expect_true("rate" %in% names(udr))
  # Should have rows for: cvc, pvc, vs, inv, niv, human_milk,
  # probiotic, kangaroo_care, ab, a, w, r
  expect_equal(nrow(udr), 12L)
})

test_that("calculate_department_data incidence_density_rate_table has structure", {
  ds <- make_calc_test_ds()
  result <- calculate_department_data(ds, use_cache = FALSE)

  idr <- result$incidence_density_rate_table
  expect_s3_class(idr, "tbl_df")
  expect_true(nrow(idr) > 0L)
  # Table has rate and count columns
  expect_true("rate" %in% names(idr) || "n" %in% names(idr))
})


# --- Precondition checks (C6) --------------------------------------------
#
# The three-valued link-privacy gates default to "no". If a caller
# doesn't set them when importing, calculate_department_data() and
# calculate_reference_data() must abort early with an actionable
# message rather than crashing deep in the pipeline.

test_that("calculate_department_data aborts when include_patient = 'no'", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_patient <- "no"
  expect_error(
    calculate_department_data(ds),
    "include_patient")
})

test_that("calculate_department_data aborts when include_enrollment = 'no'", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_enrollment <- "no"
  expect_error(
    calculate_department_data(ds),
    "include_enrollment")
})

test_that("calculate_department_data aborts when include_event = 'no'", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_event <- "no"
  expect_error(
    calculate_department_data(ds),
    "include_event")
})

test_that("calculate_department_data aborts when include_department = 'no'", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_department <- "no"
  expect_error(
    calculate_department_data(ds),
    "include_department")
})

test_that("the calculation functions refuse pseudonymized events, naming the option and the reason", {
  # The findings carry `pathogen_key` and `secondary_bsi` only under the full
  # event tier; the fixture's tibbles are full, so only the precondition can
  # refuse the dataset.
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$include_event <- "pseudo"
  for (calculate in list(calculate_department_data, calculate_reference_data)) {
    cnd <- expect_error(calculate(ds), class = "neoipcr_unmet_import_options")
    expect_match(conditionMessage(cnd),
                 "`include_event` is \"pseudo\"; need one of \"full\".",
                 fixed = TRUE)
    expect_match(conditionMessage(cnd), "`pathogen_key`", fixed = TRUE)
    expect_match(conditionMessage(cnd),
                 "a rare infectious agent can single out a patient",
                 fixed = TRUE)
  }
})

test_that("calculate_department_data aborts when dataset_options is NULL", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options <- NULL
  expect_error(
    calculate_department_data(ds),
    "import options")
})

test_that("the calculation functions abort on a dataset without a validation summary", {
  # A dataset saved before the slot existed: the calculation refuses it
  # rather than emitting `NULL` where a summary is promised.
  ds <- make_calc_test_ds()
  ds$validationSummary <- NULL
  expect_error(calculate_department_data(ds), "validationSummary")
  expect_error(calculate_reference_data(ds), "validationSummary")
})

test_that("the calculation functions abort on a dataset stored before the hierarchy pair", {
  # A dataset saved by an older neoipcr cannot be recalculated at all:
  # its events hold the pre-pair keys, so the refusal names the
  # re-import that replaces it.
  ds <- make_stale_import_ds()
  cnd <- expect_error(calculate_department_data(ds),
                     class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "calculate_department_data() needs a dataset imported by",
               fixed = TRUE)
  cnd <- expect_error(calculate_reference_data(ds),
                     class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "calculate_reference_data() needs a dataset imported by",
               fixed = TRUE)
})


# --- Validation summary and serializable options --------------------------

test_that("the calculated datasets carry the validation summary and options fit to leave the package", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$department_filter <- "DEPT_01"
  ds$metadata$dataset_options$trial_filter <- "TRIAL_A"
  ds$metadata$dataset_options$include_invalid_patients <- tibble::tibble(
    RULE_ID           = 3L,
    NEOIPC_PATIENT_ID = "PAT_X",
    ENROLMENT_DATE    = as.Date("2024-01-01"),
    EVENT_TYPE        = NA_character_,
    EVENT_DATE        = as.Date(NA))
  # The fixture's summary is the shape of a pass that found nothing, which
  # a list in the options leaves in place: the pass ran.
  expect_equal(nrow(ds$validationSummary), 3L)

  dept <- calculate_department_data(ds, use_cache = FALSE)
  expect_identical(dept$validationSummary, ds$validationSummary)
  expect_identical(
    dept$metadata$dataset_options$include_invalid_patients,
    "exception_list_applied")
  # A department dataset keeps its own filters.
  expect_identical(dept$metadata$dataset_options$department_filter, "DEPT_01")
  expect_identical(dept$metadata$dataset_options$trial_filter, "TRIAL_A")

  ref <- calculate_reference_data(ds, use_cache = FALSE)
  expect_identical(ref$validationSummary, ds$validationSummary)
  expect_identical(
    ref$metadata$dataset_options$include_invalid_patients,
    "exception_list_applied")
  expect_identical(ref$metadata$dataset_options$department_filter, "applied")
  # Every user can read a trial's member departments, so reference data does
  # not name the trial either.
  expect_identical(ref$metadata$dataset_options$trial_filter, "applied")
  expect_false(any(vapply(ref$metadata$dataset_options, is.data.frame, logical(1))))
})

test_that("the calculation functions refuse to emit a data frame the markers do not cover", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$extra <- tibble::tibble(id = "PAT_X")
  expect_error(calculate_department_data(ds, use_cache = FALSE), "data frame")
  expect_error(calculate_reference_data(ds, use_cache = FALSE), "data frame")
})

test_that("an empty department filter leaves reference data as unfiltered, not as applied", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options$department_filter <- character()
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  expect_true("department_filter" %in% names(ref$metadata$dataset_options))
  expect_null(ref$metadata$dataset_options$department_filter)
})

test_that("get_benchmark_data refuses a call without any dataset", {
  expect_error(get_benchmark_data(), class = "neoipcr_no_benchmark_datasets")
})

test_that("the benchmark of a department against the reference carries both shares of the table by infection type", {
  # Each of departments 1-5 has a sepsis naming an infectious agent and 20
  # pneumonias, `tested[d]` of department d tested and `positive[d]` of these
  # positive with an infectious agent in an own slot; department 1 is
  # benchmarked against all 5.
  tested   <- c(5L, 10L, 15L, 20L, 20L)
  positive <- c(2L, 0L, 9L, 5L, 20L)
  infections <- function(departments) {
    forms <- dplyr::bind_rows(lapply(departments, \(d) tibble::tibble(
      department_key              = d,
      event_type_key              = c(rep("hap", 20), "bsi"),
      microbiological_test_result = c(
        rep(c("1", "0", "-1"),
            c(positive[d], tested[d] - positive[d], 20L - tested[d])),
        NA))))
    make_infection_calc_test_ds(
      forms,
      findings = tibble::tibble(
        form          = which(forms$microbiological_test_result %in% "1" |
                                forms$event_type_key == "bsi"),
        secondary_bsi = FALSE,
        pathogen_key  = 1L))
  }
  ref <- calculate_reference_data(infections(1:5), use_cache = FALSE)
  own <- calculate_department_data(infections(1L), use_cache = FALSE)

  tbl <- get_benchmark_data(own = own, ref = ref)$
    infectious_agent_detection_rate_per_inf_type_table
  expect_equal(as.character(tbl$inf), c("bsi", "hap", "ssi"))
  hap <- tbl[tbl$inf == "hap", ]
  expect_equal(c(hap$n_own, hap$tested_own, hap$total_own), c(2L, 5L, 20L))
  expect_equal(c(hap$pooled_own, hap$tested_pooled_own), c(40, 25))
  expect_equal(c(hap$n_ref, hap$tested_ref, hap$total_ref), c(36L, 70L, 100L))
  expect_equal(c(hap$pooled_ref, hap$tested_pooled_ref), c(3600 / 70, 70))
  expect_equal(c(hap$q1_ref, hap$q2_ref, hap$q3_ref), c(25, 40, 60))
  expect_equal(c(hap$tested_q1_ref, hap$tested_q2_ref, hap$tested_q3_ref),
               c(50, 75, 100))
  expect_false(anyNA(unlist(hap[paste0(
    "tested_q", rep(1:3, each = 2), c("_ci_lower_ref", "_ci_upper_ref"))])))
  expect_equal(
    c(hap$quartile_departments_ref, hap$tested_quartile_departments_ref),
    c(5L, 5L))
  # The department's own table has no quartiles.
  expect_false(any(grepl("(q[123]|quartile_departments).*_own$", names(tbl))))
  bsi <- tbl[tbl$inf == "bsi", ]
  expect_equal(c(bsi$n_own, bsi$tested_own, bsi$total_own), c(1L, 1L, 1L))
  expect_equal(c(bsi$n_ref, bsi$tested_ref, bsi$total_ref), c(5L, 5L, 5L))
  expect_true(all(is.na(c(bsi$tested_pooled_own, bsi$tested_pooled_ref,
                          bsi$tested_q1_ref,
                          bsi$tested_quartile_departments_ref))))
  # 1 episode per department is below the minimum of 5.
  expect_equal(bsi$quartile_departments_ref, 0L)

  # The reference's table without quartiles leaves out both shares'.
  expect_false(any(grepl("q[123]|quartile_departments", names(
    get_infectious_agent_detection_rate_per_inf_type_table(
      ref, include_quartiles = FALSE)))))
})

test_that("get_benchmark_data carries each dataset's validation summary under its name", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_named(benchmark$validationSummary, c("own", "ref"))
  expect_identical(benchmark$validationSummary$own, own$validationSummary)
  expect_identical(benchmark$validationSummary$ref, ref$validationSummary)
})

test_that("the calculated datasets carry the reconciliation summary, and get_benchmark_data carries it under each dataset's name", {
  ds <- make_calc_test_ds()
  expect_named(ds$reconciliationSummary,
               c("reconciliation_id", "record_kind", "n_repaired", "n_reported"))

  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  expect_identical(own$reconciliationSummary, ds$reconciliationSummary)
  expect_identical(ref$reconciliationSummary, ds$reconciliationSummary)
  expect_true(own$metadata$dataset_options$reconcile)
  expect_true(ref$metadata$dataset_options$reconcile)

  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_named(benchmark$reconciliationSummary, c("own", "ref"))
  expect_identical(benchmark$reconciliationSummary$own, own$reconciliationSummary)
  expect_identical(benchmark$reconciliationSummary$ref, ref$reconciliationSummary)
})

test_that("the calculation functions take a raw dataset written before the reconciliation summary existed", {
  # NEOIPC-PERMANENT(dataset-format): see assert_reconciliation_summary(). A
  # raw dataset kept as a backup outlives the code that wrote it, so the
  # calculations carry `NULL` for its summary rather than refuse it, and the
  # benchmark holds no entry for it.
  ds <- make_calc_test_ds()
  ds$reconciliationSummary <- NULL

  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  expect_null(own$reconciliationSummary)
  expect_null(ref$reconciliationSummary)

  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_length(benchmark$reconciliationSummary, 0L)
  expect_true("n_patients" %in% names(benchmark))
})

test_that("get_benchmark_data takes a calculated dataset serialized before the reconciliation summary existed", {
  # NEOIPC-PERMANENT(dataset-format): a calculated dataset serialized before
  # the slot existed outlives the code that wrote it; it contributes no entry
  # and is otherwise combined as before.
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  ref$reconciliationSummary <- NULL
  expect_false("reconciliationSummary" %in% names(ref))

  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_named(benchmark$reconciliationSummary, "own")
})

test_that("the calculation functions refuse a reconciliation summary of another shape", {
  ds <- make_calc_test_ds()
  ds$reconciliationSummary <- tibble::tibble(reconciliation_id = 1L)
  expect_error(calculate_department_data(ds, use_cache = FALSE),
               "has the column `reconciliation_id`\\.",
               class = "neoipcr_malformed_reconciliation_summary")
  expect_error(calculate_reference_data(ds, use_cache = FALSE),
               class = "neoipcr_malformed_reconciliation_summary")
  ds$reconciliationSummary <- tibble::tibble(reconciliation_id = 1L, n = 1L)
  expect_error(calculate_department_data(ds, use_cache = FALSE),
               "has the columns `reconciliation_id`, `n`\\.",
               class = "neoipcr_malformed_reconciliation_summary")
  ds$reconciliationSummary <- list(n_repaired = 1L)
  expect_error(calculate_department_data(ds, use_cache = FALSE),
               "not a data frame", class = "neoipcr_malformed_reconciliation_summary")

  # The 0×0 slot of an import that reconciled nothing is the import's own.
  ds$reconciliationSummary <- tibble::tibble()
  own <- calculate_department_data(ds, use_cache = FALSE)
  expect_equal(dim(own$reconciliationSummary), c(0L, 0L))
})

test_that("get_benchmark_data takes a calculated dataset without a validation summary", {
  # NEOIPC-PERMANENT(dataset-format): a calculated dataset serialized before
  # the slot existed outlives the code that wrote it; it contributes no entry
  # and is otherwise combined as before.
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  ref$validationSummary <- NULL
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_named(benchmark$validationSummary, "own")
  expect_identical(benchmark$validationSummary$own, own$validationSummary)
  expect_true("n_patients" %in% names(benchmark))
})


# --- Versions of the rate definitions and the benchmark's guard ------------

# The tables of a calculated dataset: every slot but its metadata and the
# import's summaries.
calculated_tables <- function(calculated)
  setdiff(names(calculated),
          c("metadata", "validationSummary", "reconciliationSummary"))

# The tables that read the tree of the infectious-agent list over the
# reporting groups, those that read it over the full ontology, those that
# count infectious agents by the concept each Id names, and those that read to
# which infectious agents each resistance category applies.
groups_tables <- function()
  c("infectious_agent_detection_rate_per_agent_table",
    "abr_infection_rate_table", "infectious_agent_resistance_rate_table")
ontology_tables <- function()
  c("infectious_agent_detection_rate_per_agent_ontology_table",
    "abr_infection_rate_ontology_table",
    "infectious_agent_resistance_rate_ontology_table")
tree_tables <- function() c(groups_tables(), ontology_tables())
concept_tables <- function() c(tree_tables(), "resistance_test_rate_table")
applicability_tables <- function()
  c("abr_infection_rate_table", "abr_infection_rate_ontology_table",
    "infectious_agent_resistance_rate_table",
    "infectious_agent_resistance_rate_ontology_table",
    "resistance_test_rate_table")

# `expr`, a call of get_benchmark_data(), with the warnings of class
# `neoipcr_tables_not_comparable` it raises: `value` and `warnings`.
benchmark_warnings <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(
    expr,
    neoipcr_tables_not_comparable = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  list(value = value, warnings = warnings)
}

# The bullet of that warning which says what each table not joined holds of
# the datasets its "Left empty for" lines name.
not_joined_bullet <- paste(
  "Each such table keeps the rows of the datasets joined into it; a figure",
  "takes no rows from a dataset for which it is left empty, and any other",
  "such table leaves that dataset's columns empty.")

# Expect `benchmark` to hold the rows of the table `table` of `first`, the
# first dataset, named "own", and the columns of the dataset `name` of that
# table to be empty.
expect_left_empty <- function(benchmark, first, table, name) {
  joined <- benchmark[[table]]
  own <- first[[table]]
  if (table %in% c("birth_weight_figure", "gestational_age_figure")) {
    for (part in names(own)) {
      expect_false(name %in% joined[[part]]$dataset)
      expect_equal(sum(joined[[part]]$dataset == "own"), nrow(own[[part]]))
    }
    return(invisible())
  }
  if (!is.data.frame(own))
    own <- tibble::as_tibble(if (is.list(own)) own else list(n = own))
  theirs <- grep(paste0("_", name, "$"), names(joined), value = TRUE)
  expect_gt(length(theirs), 0L)
  for (col in theirs)
    expect_true(all(is.na(joined[[col]])), label = paste(table, col))
  expect_equal(nrow(joined), nrow(own))
  for (col in names(own)) {
    mine <- paste0(col, "_own")
    if (mine %in% names(joined))
      expect_equal(joined[[mine]], own[[col]], ignore_attr = TRUE,
                   label = paste(table, mine))
  }
}

test_that("both calculation functions stamp every table with the version of its rate definitions and the digests of what it reads from the infectious-agent list", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  provenance <- neoipcr:::internal_infectious_agent_ontology
  layout <- neoipcr:::infectious_agent_table_layout
  for (calculated in list(own, ref)) {
    md <- calculated$metadata
    expect_identical(md$neoipcr_version,
                     as.character(utils::packageVersion("neoipcr")))
    expect_identical(md$infectious_agent_tables, list(
      version = provenance$version,
      commit = provenance$commit,
      layout = layout))

    stamps <- md$rate_definitions
    expect_named(stamps, c("table", "version", "common_commensal_digest",
                           "concept_mapping_digest", "groups_tree_digest",
                           "ontology_tree_digest", "layout",
                           "applicability_digest"))
    # Every table, once, and nothing else.
    expect_setequal(stamps$table, calculated_tables(calculated))
    expect_equal(anyDuplicated(stamps$table), 0L)
    expect_type(stamps$version, "integer")
    expect_true(all(stamps$version >= 1L))
    # The rate definitions of the incidence densities split the sepsis kinds
    # on the common-commensal flags; the tables that count infectious agents
    # by their concept read the concept each Id names; the tables whose rows
    # are infectious agents read the tree of their version and are laid out
    # by the layout; and the resistance tables read to which infectious agents
    # each category applies.
    expect_identical(
      stamps$common_commensal_digest,
      ifelse(stamps$table == "incidence_density_rate_table",
             provenance$common_commensal_digest, NA_character_))
    expect_identical(
      stamps$concept_mapping_digest,
      ifelse(stamps$table %in% concept_tables(),
             provenance$concept_mapping_digest, NA_character_))
    expect_identical(
      stamps$groups_tree_digest,
      ifelse(stamps$table %in% groups_tables(),
             provenance$groups_tree_digest, NA_character_))
    expect_identical(
      stamps$ontology_tree_digest,
      ifelse(stamps$table %in% ontology_tables(),
             provenance$ontology_tree_digest, NA_character_))
    expect_identical(
      stamps$layout,
      ifelse(stamps$table %in% tree_tables(), layout, NA_integer_))
    expect_identical(
      stamps$applicability_digest,
      ifelse(stamps$table %in% applicability_tables(),
             provenance$applicability_digest, NA_character_))
  }
  expect_identical(own$metadata$rate_definitions,
                   ref$metadata$rate_definitions)
})

test_that("both calculation functions warn once where the findings hold codes the infectious-agent list lacks", {
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = c("bsi", "bsi", "hap")),
    findings = tibble::tibble(
      form = c(1L, 1L, 2L, 3L),
      pathogen_key = c(99999999L, 1807L, 99999998L, 99999999L)))
  for (calculate in list(calculate_department_data, calculate_reference_data)) {
    warnings <- list()
    withCallingHandlers(
      calculate(ds, use_cache = FALSE),
      neoipcr_unknown_infectious_agent_code = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      })
    expect_length(warnings, 1L)
    expect_identical(warnings[[1]]$codes, c(99999998L, 99999999L))
    expect_match(conditionMessage(warnings[[1]]), "99999998, 99999999",
                 fixed = TRUE)
  }

  # Codes the list holds, "Not listed" and "Unidentifiable" among them, raise
  # none.
  known <- make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi"),
    findings = tibble::tibble(form = 1L, pathogen_key = c(0L, 3561L, 1807L)))
  expect_no_warning(calculate_department_data(known, use_cache = FALSE),
                    class = "neoipcr_unknown_infectious_agent_code")
  expect_no_warning(calculate_reference_data(known, use_cache = FALSE),
                    class = "neoipcr_unknown_infectious_agent_code")
})

test_that("get_benchmark_data joins a table only where the versions of its rate definitions match, and otherwise keeps the rows of the datasets joined, the others adding no rows to a figure and their columns of any other table left empty, as its warning says", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  matching <- benchmark_warnings(get_benchmark_data(own = own, ref = ref))
  expect_length(matching$warnings, 0L)
  expect_identical(
    matching$value$not_comparable,
    tibble::tibble(table = character(), dataset = character()))
  # The tables whose rows are not infectious agents join the two datasets'
  # rows on their keys, one row each.
  for (table in c("usage_density_rate_table", "antibiotic_utilization_table",
                  "surgery_rate_table", "incidence_density_rate_table",
                  "dev_ass_incidence_density_rate_table", "n_infections",
                  "secondary_bsi_rate_table",
                  "infectious_agent_detection_rate_per_inf_type_table",
                  "resistance_test_rate_table")) {
    expect_equal(nrow(matching$value[[table]]), nrow(ref[[table]]),
                 label = table)
    if ("n" %in% names(ref[[table]]))
      expect_equal(sort(matching$value[[table]]$n_ref), sort(ref[[table]]$n),
                   label = table)
  }

  for (table in calculated_tables(own)) {
    changed <- ref
    stamps <- changed$metadata$rate_definitions
    stamps$version[stamps$table == table] <-
      stamps$version[stamps$table == table] + 1L
    changed$metadata$rate_definitions <- stamps

    r <- benchmark_warnings(get_benchmark_data(own = own, ref = changed))
    expected <- tibble::tibble(table = table, dataset = "ref")
    expect_length(r$warnings, 1L)
    expect_identical(r$value$not_comparable, expected)
    expect_identical(r$warnings[[1]]$not_comparable, expected)
    expect_match(conditionMessage(r$warnings[[1]]), table, fixed = TRUE)
    expect_match(conditionMessage(r$warnings[[1]]), not_joined_bullet,
                 fixed = TRUE, label = table)
    expect_left_empty(r$value, own, table, "ref")
    # A change to one table's definitions leaves every other table joined.
    for (other in setdiff(calculated_tables(own), table))
      expect_identical(r$value[[other]], matching$value[[other]],
                       label = paste(table, other))

    # A dataset joined after the one left out fills none of its columns, and
    # the table is what the two others give, as the warning says: it keeps
    # the rows of every dataset joined, not those of the first alone.
    r <- benchmark_warnings(
      get_benchmark_data(own = own, old = changed, ref = ref))
    expect_identical(r$value$not_comparable,
                     tibble::tibble(table = table, dataset = "old"))
    expect_length(r$warnings, 1L)
    expect_match(conditionMessage(r$warnings[[1]]), not_joined_bullet,
                 fixed = TRUE, label = table)
    joined <- r$value[[table]]
    if (table %in% c("birth_weight_figure", "gestational_age_figure")) {
      for (part in names(joined))
        expect_false("old" %in% joined[[part]]$dataset)
      expect_identical(joined, matching$value[[table]], label = table)
    } else {
      old_cols <- grep("_old$", names(joined), value = TRUE)
      expect_gt(length(old_cols), 0L)
      for (col in old_cols)
        expect_true(all(is.na(joined[[col]])), label = paste(table, col))
      expect_identical(joined[setdiff(names(joined), old_cols)],
                       matching$value[[table]], label = table)
    }
  }
})

test_that("a release of the infectious-agent list makes a calculated dataset incomparable only for the tables that read what it changed", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  matching <- get_benchmark_data(own = own, ref = ref)
  readers <- list(
    common_commensal_digest = "incidence_density_rate_table",
    concept_mapping_digest = concept_tables(),
    groups_tree_digest = groups_tables(),
    ontology_tree_digest = ontology_tables(),
    layout = tree_tables(),
    applicability_digest = applicability_tables())
  for (stamp in names(readers)) {
    changed <- ref
    stamps <- changed$metadata$rate_definitions
    read <- !is.na(stamps[[stamp]])
    stamps[[stamp]][read] <- if (stamp == "layout")
      stamps[[stamp]][read] + 1L
    else
      strrep("0", 64L)
    changed$metadata$rate_definitions <- stamps

    r <- benchmark_warnings(get_benchmark_data(own = own, ref = changed))
    expect_length(r$warnings, 1L)
    expect_setequal(r$value$not_comparable$table, readers[[stamp]])
    expect_true(all(r$value$not_comparable$dataset == "ref"))
    for (table in readers[[stamp]])
      expect_left_empty(r$value, own, table, "ref")
    for (table in setdiff(calculated_tables(own), readers[[stamp]]))
      expect_identical(r$value[[table]], matching[[table]],
                       label = paste(stamp, table))
  }

  # A release that changes nothing a table reads leaves every table
  # comparable.
  changed <- ref
  changed$metadata$infectious_agent_tables$version <- "0.0.0-other"
  changed$metadata$infectious_agent_tables$commit <- strrep("0", 40L)
  r <- benchmark_warnings(get_benchmark_data(own = own, ref = changed))
  expect_length(r$warnings, 0L)
  expect_equal(nrow(r$value$not_comparable), 0L)
})

# Every bundled table, as the derivation returns them.
bundled_tables <- function() list(
  classification = neoipcr:::internal_infectious_agent_classification,
  reporting_groups = neoipcr:::internal_infectious_agent_reporting_groups,
  attribute_values = neoipcr:::internal_infectious_agent_attribute_values,
  nodes = neoipcr:::internal_infectious_agent_nodes,
  closure = neoipcr:::internal_infectious_agent_closure)

# The provenance of a release of the list whose bundled tables are those of
# the bundle with `change` applied, with the digests it changes: each digest
# whose text changes is replaced by another value, which stands for the
# SHA-256 the build would record.
release_with <- function(change) {
  before <- neoipcr:::infectious_agent_digest_texts(bundled_tables())
  after <- neoipcr:::infectious_agent_digest_texts(change(bundled_tables()))
  provenance <- neoipcr:::internal_infectious_agent_ontology
  for (name in names(after)[after != before])
    provenance[[paste0(name, "_digest")]] <- rlang::hash(after[[name]])
  list(provenance = provenance, changed = names(after)[after != before])
}

test_that("a release that changes only the concept an Id names, only the full ontology, only the reporting groups, or only the applicability makes exactly the tables that read it incomparable", {
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  matching <- get_benchmark_data(own = own, ref = ref)
  id_of <- function(x, name) x$input_id[x$input_name == name]

  # 1. A subspecies turned into a synonym of its species, on the path it had.
  concept <- function(tables) {
    x <- tables$classification
    species <- id_of(x, "Bifidobacterium longum")
    infantis <- x$input_id == id_of(x, "Bifidobacterium longum subsp. infantis")
    x$output_id[infantis] <- species
    x$output_name[infantis] <- "Bifidobacterium longum"
    tables$classification <- x
    tables
  }
  # 2. A record number corrected in the full ontology, a node without an Id.
  ontology <- function(tables) {
    old <- "LPSN|family/staphylococcaceae"
    new <- "LPSN|family/staphylococcaceae-1"
    nodes <- tables$nodes
    ontology_rows <- nodes$version == "ontology"
    nodes$node_id[ontology_rows & nodes$node_id == old] <- new
    nodes$parent_id[ontology_rows & nodes$parent_id %in% old] <- new
    closure <- tables$closure
    closure$node_id[closure$version == "ontology" & closure$node_id == old] <-
      new
    tables$nodes <- nodes
    tables$closure <- closure
    tables
  }
  # 3. A reporting group renamed.
  groups <- function(tables) {
    renamed <- "Gram-positive cocci of all kinds"
    tables$reporting_groups$name[
      tables$reporting_groups$key == "gram-positive-cocci"] <- renamed
    tables$nodes$name[tables$nodes$version == "groups" &
                        tables$nodes$node_id == "gram-positive-cocci"] <-
      renamed
    tables
  }
  # 4. Vancomycin resistance applied to a species of Candida.
  applicability <- function(tables) {
    x <- tables$classification
    x$applies_vre[x$input_id == id_of(x, "Candida albicans")] <- TRUE
    tables$classification <- x
    tables
  }

  for (case in list(
    list(change = concept, digest = "concept_mapping",
         tables = concept_tables()),
    list(change = ontology, digest = "ontology_tree",
         tables = ontology_tables()),
    list(change = groups, digest = "groups_tree", tables = groups_tables()),
    list(change = applicability, digest = "applicability",
         tables = applicability_tables()))) {
    release <- release_with(case$change)
    expect_identical(release$changed, c("classification", case$digest))
    changed <- ref
    changed$metadata$rate_definitions <-
      neoipcr:::.rate_definitions(release$provenance)

    r <- benchmark_warnings(get_benchmark_data(own = own, ref = changed))
    expect_length(r$warnings, 1L)
    expect_setequal(r$value$not_comparable$table, case$tables)
    expect_equal(nrow(r$value$not_comparable), length(case$tables))
    expect_true(all(r$value$not_comparable$dataset == "ref"))
    for (table in case$tables)
      expect_left_empty(r$value, own, table, "ref")
    for (table in setdiff(calculated_tables(own), case$tables))
      expect_identical(r$value[[table]], matching[[table]],
                       label = paste(case$digest, table))
  }
})

test_that("get_benchmark_data joins a dataset calculated before the datasets carried the versions of their rate definitions with no other dataset", {
  # NEOIPC-PERMANENT(dataset-format): see .rate_definition_stamps(). A
  # calculated dataset serialized before the stamps existed outlives the code
  # that wrote it; it counts, for every table, as calculated under other
  # definitions.
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  unstamped <- function(calculated) {
    calculated$metadata$rate_definitions <- NULL
    calculated$metadata$infectious_agent_tables <- NULL
    calculated$metadata$neoipcr_version <- NULL
    calculated
  }
  tables <- calculated_tables(own)

  r <- benchmark_warnings(get_benchmark_data(own = own, ref = unstamped(ref)))
  expect_length(r$warnings, 1L)
  expect_setequal(r$value$not_comparable$table, tables)
  expect_equal(nrow(r$value$not_comparable), length(tables))
  expect_true(all(r$value$not_comparable$dataset == "ref"))
  expect_match(conditionMessage(r$warnings[[1]]), "calculate it again",
               fixed = TRUE)
  for (table in tables)
    expect_left_empty(r$value, own, table, "ref")

  # Placed first, its rows are kept and the versioned dataset's columns are
  # empty. The warning, which also names it to be calculated again, leaves
  # empty the tables of the versioned dataset alone, and says what they hold
  # of that dataset only.
  r <- benchmark_warnings(get_benchmark_data(ref = unstamped(ref), own = own))
  expect_setequal(r$value$not_comparable$table, tables)
  expect_true(all(r$value$not_comparable$dataset == "own"))
  expect_length(r$warnings, 1L)
  warned <- conditionMessage(r$warnings[[1]])
  expect_match(warned, "Left empty for `own`:", fixed = TRUE)
  expect_no_match(warned, "Left empty for `ref`", fixed = TRUE)
  expect_match(warned, not_joined_bullet, fixed = TRUE)

  # Two such datasets are not compared with each other either.
  r <- benchmark_warnings(
    get_benchmark_data(own = unstamped(own), ref = unstamped(ref)))
  expect_setequal(r$value$not_comparable$table, tables)
  expect_true(all(r$value$not_comparable$dataset == "ref"))

  # Such a dataset carries its infectious-agent tables in their earlier
  # shape: the table of the infectious agents detected and the density of
  # the infections with resistant infectious agents with `taxon` and
  # `.ontology_path` in place of `rank`, `node_id`, and `parent_id`, `abr` as
  # text, and the resistance rates per infectious agent as
  # `organism_resistance_rate_table`, without the ontology versions.
  earlier_slots <- c("infectious_agent_detection_rate_per_agent_table",
                     "abr_infection_rate_table")
  earlier <- function(calculated) {
    calculated <- unstamped(calculated)
    reshape <- function(tbl) {
      tbl$taxon <- dplyr::coalesce(tbl$rank, "none")
      tbl$.ontology_path <- dplyr::coalesce(tbl$node_id, "")
      if ("abr" %in% names(tbl))
        tbl$abr <- as.character(tbl$abr)
      tbl[setdiff(names(tbl), c("rank", "node_id", "parent_id"))]
    }
    calculated$organism_resistance_rate_table <-
      reshape(calculated$infectious_agent_resistance_rate_table)
    for (slot in earlier_slots)
      calculated[[slot]] <- reshape(calculated[[slot]])
    calculated[setdiff(tree_tables(), earlier_slots)] <- NULL
    calculated
  }
  old <- earlier(ref)
  # The earlier dataset's columns of a table, `suffix` appended to every
  # column but those that place and label a row.
  suffixed <- function(tbl, suffix) {
    row_cols <- intersect(c("abr", "level", "group"), names(tbl))
    names(tbl) <- ifelse(names(tbl) %in% row_cols, names(tbl),
                         paste0(names(tbl), suffix))
    tbl
  }
  # The earlier dataset's slot of each table whose rows are infectious agents,
  # by the benchmark's slot that holds it: its resistance rates per
  # infectious agent count as `infectious_agent_resistance_rate_table`.
  earlier_tables <- c(
    rlang::set_names(earlier_slots),
    infectious_agent_resistance_rate_table = "organism_resistance_rate_table")
  # Every table the earlier dataset holds: all but the ontology versions.
  held <- setdiff(tables, ontology_tables())

  # Placed second, it is left out as before, its resistance rates per
  # infectious agent too, and the benchmark holds no slot of their earlier
  # name.
  r <- benchmark_warnings(get_benchmark_data(own = own, ref = old))
  expect_length(r$warnings, 1L)
  expect_setequal(r$value$not_comparable$table, held)
  expect_equal(nrow(r$value$not_comparable), length(held))
  expect_true(all(r$value$not_comparable$dataset == "ref"))
  expect_match(conditionMessage(r$warnings[[1]]),
               "`infectious_agent_resistance_rate_table`", fixed = TRUE)
  for (table in names(earlier_tables))
    expect_left_empty(r$value, own, table, "ref")
  expect_false("organism_resistance_rate_table" %in% names(r$value))

  # Placed first, its rows are kept as they are, in their order, and the
  # versioned dataset's columns are empty.
  r <- benchmark_warnings(get_benchmark_data(ref = old, own = own))
  expect_length(r$warnings, 1L)
  expect_setequal(r$value$not_comparable$table, held)
  expect_equal(nrow(r$value$not_comparable), length(held))
  expect_true(all(r$value$not_comparable$dataset == "own"))
  for (table in names(earlier_tables)) {
    joined <- r$value[[table]]
    kept <- suffixed(old[[earlier_tables[[table]]]], "_ref")
    expect_identical(as.list(joined[names(kept)]), as.list(kept),
                     label = table)
    theirs <- grep("_own$", names(joined), value = TRUE)
    expect_gt(length(theirs), 0L)
    for (col in theirs)
      expect_true(all(is.na(joined[[col]])), label = paste(table, col))
  }
  expect_false("organism_resistance_rate_table" %in% names(r$value))

  # Alone, its tables are what it holds.
  r <- benchmark_warnings(get_benchmark_data(ref = old))
  expect_length(r$warnings, 0L)
  for (table in names(earlier_tables))
    expect_identical(as.list(r$value[[table]]),
                     as.list(suffixed(old[[earlier_tables[[table]]]], "_ref")),
                     label = table)
  expect_false("organism_resistance_rate_table" %in% names(r$value))

  # The earlier slot is read only from a dataset without stamps: a stamped
  # dataset's table under that name is not the benchmark's.
  renamed <- own
  renamed$organism_resistance_rate_table <-
    renamed$infectious_agent_resistance_rate_table
  renamed$infectious_agent_resistance_rate_table <- NULL
  r <- benchmark_warnings(get_benchmark_data(own = renamed, ref = ref))
  expect_length(r$warnings, 0L)
  expect_false(any(grepl(
    "_own$", names(r$value$infectious_agent_resistance_rate_table))))
})

test_that("get_benchmark_data reads a stamp a dataset lacks as missing, so that only the tables whose stamp carries it are not joined", {
  # NEOIPC-PERMANENT(dataset-format): see .rate_definition_stamps(). A
  # calculated dataset outlives the code that wrote it, and a later release
  # may add a stamp column, which a dataset calculated before it lacks: such
  # a dataset counts as calculated under other definitions only for the
  # tables whose stamp in the other dataset has a value in that column.
  ds <- make_calc_test_ds()
  own <- calculate_department_data(ds, use_cache = FALSE)
  ref <- calculate_reference_data(ds, use_cache = FALSE)
  matching <- get_benchmark_data(own = own, ref = ref)
  lacking <- ref
  lacking$metadata$rate_definitions$concept_mapping_digest <- NULL
  # The warning's claim that a dataset predates the stamps.
  predates <- "was calculated before calculated datasets carried"

  r <- benchmark_warnings(get_benchmark_data(own = own, ref = lacking))
  expect_length(r$warnings, 1L)
  expect_setequal(r$value$not_comparable$table, concept_tables())
  expect_equal(nrow(r$value$not_comparable), length(concept_tables()))
  expect_true(all(r$value$not_comparable$dataset == "ref"))
  expect_no_match(conditionMessage(r$warnings[[1]]), predates, fixed = TRUE)
  for (table in concept_tables())
    expect_left_empty(r$value, own, table, "ref")
  for (table in setdiff(calculated_tables(own), concept_tables()))
    expect_identical(r$value[[table]], matching[[table]], label = table)

  # Placed first, it is the other dataset whose tables are not joined.
  r <- benchmark_warnings(get_benchmark_data(ref = lacking, own = own))
  expect_length(r$warnings, 1L)
  expect_setequal(r$value$not_comparable$table, concept_tables())
  expect_true(all(r$value$not_comparable$dataset == "own"))
  expect_no_match(conditionMessage(r$warnings[[1]]), predates, fixed = TRUE)

  # Stamps without the table or the version they stamp are none: every table
  # of the dataset counts as calculated under other definitions.
  for (column in c("table", "version")) {
    unstamped <- ref
    unstamped$metadata$rate_definitions[[column]] <- NULL
    r <- benchmark_warnings(get_benchmark_data(own = own, ref = unstamped))
    expect_setequal(r$value$not_comparable$table, calculated_tables(own))
    expect_true(all(r$value$not_comparable$dataset == "ref"))
    expect_match(conditionMessage(r$warnings[[1]]), predates, fixed = TRUE,
                 label = column)
  }
})

test_that("get_benchmark_data gives a department the reference's rows of each infectious-agent table, in the reference's order, at 0 over its own denominator", {
  # The reference: Escherichia coli (1807) with third-generation cephalosporin
  # resistance (3GCR) and Staphylococcus aureus (934) as methicillin-resistant
  # S. aureus (MRSA) in department 1, Klebsiella pneumoniae (390) 3GCR
  # resistant and E. coli not in department 2, each a sepsis of its own. The
  # department: E. coli 3GCR resistant and Candida albicans (2273), which the
  # reference lacks, each a sepsis of its own, over 50 patient-days.
  ref_ds <- with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi",
                           department_key = c(1L, 1L, 2L, 2L)),
    findings = tibble::tibble(
      form = 1:4,
      pathogen_key = c(1807L, 934L, 390L, 1807L),
      `3gcr` = c("yes", NA, "yes", "no"),
      car = NA_character_,
      cor = NA_character_,
      mrsa = c(NA, "yes", NA, NA),
      vre = NA_character_),
    patient_days = c(100L, 100L)))
  own_ds <- with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi",
                           department_key = c(1L, 1L)),
    findings = tibble::tibble(
      form = 1:2,
      pathogen_key = c(1807L, 2273L),
      `3gcr` = c("yes", NA),
      car = NA_character_,
      cor = NA_character_,
      mrsa = NA_character_,
      vre = NA_character_),
    patient_days = 50L))
  own <- calculate_department_data(own_ds, use_cache = FALSE)
  ref <- calculate_reference_data(ref_ds, use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  key <- function(t)
    if ("abr" %in% names(t)) paste(t$abr, t$node_id) else paste(t$node_id)
  parent_key <- function(t)
    if ("abr" %in% names(t)) paste(t$abr, t$parent_id) else paste(t$parent_id)
  for (slot in tree_tables()) {
    joined <- benchmark[[slot]]
    rows <- key(joined)
    # Every row of either dataset, once, the reference's in its order.
    expect_setequal(rows, union(key(own[[slot]]), key(ref[[slot]])))
    expect_equal(anyDuplicated(rows), 0L)
    expect_identical(rows[rows %in% key(ref[[slot]])], key(ref[[slot]]),
                     label = slot)
    # Each row follows its parent.
    child <- !is.na(joined$parent_id)
    expect_true(all(match(parent_key(joined)[child], rows) < which(child)),
                label = slot)
  }

  # The infectious agents detected over the reporting groups: the
  # department's Candida albicans adds a group the reference lacks, after the
  # reference's rows.
  detected <- benchmark$infectious_agent_detection_rate_per_agent_table
  expect_identical(
    detected$node_id,
    c(NA, "gram_reaction|positive", "gram_reaction|negative",
      "oxygen_requirement|facultative-anaerobic", "bacteria",
      "enterobacterales", "gram-positive-cocci", "fungi", "yeasts"))
  # The department has 2 infections with an infectious agent in the tree,
  # both sepsis, and none with S. aureus.
  cocci <- detected[detected$node_id %in% "gram-positive-cocci", ]
  expect_identical(cocci$n_own, 0L)
  expect_identical(cocci$total_own, 2L)
  expect_identical(cocci$pooled_own, 0)
  expect_identical(cocci$ci_lower_own, 0)
  expect_equal(cocci$ci_upper_own, neoipcr::neoipc_wilson_ci(0L, 2L)$upper * 100)
  expect_identical(cocci$n_bsi_own, 0L)
  expect_identical(cocci$total_bsi_own, 2L)
  expect_identical(cocci$pooled_bsi_own, 0)
  expect_identical(cocci$total_hap_own, 0L)
  expect_identical(cocci$pooled_hap_own, NA_real_)
  expect_identical(cocci$n_total_only_own, NA_integer_)
  # The reference has 4 such infections and none with Candida.
  yeasts <- detected[detected$node_id %in% "yeasts", ]
  bacteria <- detected[detected$node_id %in% "bacteria", ]
  expect_identical(yeasts$n_ref, 0L)
  expect_identical(yeasts$total_ref, 4L)
  expect_identical(yeasts$pooled_ref, 0)
  expect_equal(yeasts$ci_upper_ref, neoipcr::neoipc_wilson_ci(0L, 4L)$upper * 100)
  expect_identical(yeasts$quartile_departments_ref,
                   bacteria$quartile_departments_ref)
  expect_identical(yeasts$q1_ref, NA_real_)

  # The density of infections with resistant infectious agents: the
  # department's MRSA block has its Total alone.
  density <- benchmark$abr_infection_rate_table
  mrsa <- density[density$abr == "mrsa", ]
  expect_identical(mrsa$node_id, ref$abr_infection_rate_table$node_id[
    ref$abr_infection_rate_table$abr == "mrsa"])
  expect_true(all(mrsa$n_own == 0L))
  expect_true(all(mrsa$total_own == 50L))
  expect_true(all(mrsa$pooled_own == 0))
  expect_equal(mrsa$ci_upper_own,
               rep(neoipcr::neoipc_poisson_ci(0L, 50L, multiplier = 1000)$upper,
                   nrow(mrsa)))

  # The resistance rates: the department has no infection in the MRSA rows'
  # denominators, so they have no rate.
  rates <- benchmark$infectious_agent_resistance_rate_table
  mrsa <- rates[rates$abr == "mrsa", ]
  expect_gt(nrow(mrsa), 0L)
  expect_true(all(mrsa$n_own == 0L))
  expect_true(all(mrsa$total_own == 0L))
  expect_true(all(is.na(mrsa$pooled_own)))
  expect_true(all(is.na(mrsa$ci_upper_own)))
  expect_true(all(mrsa$tested_n_own == 0L))
  expect_true(all(mrsa$tested_total_own == 0L))
  expect_true(all(is.na(mrsa$tested_pooled_own)))
})

test_that("get_benchmark_data gives a dataset all its patient-days on a density row it lacks, whatever departments its Totals leave out", {
  # The reference: Escherichia coli (1807) with third-generation cephalosporin
  # resistance (3GCR) and Staphylococcus aureus (934) as MRSA in department 1,
  # and Klebsiella pneumoniae (390) without a 3GCR result in department 2,
  # which takes department 2 out of the 3GCR Total, each a sepsis of its own
  # over 100 patient-days per department; and a sepsis without an infectious
  # agent in department 3, over 40 patient-days, too few to enter quartiles.
  # The department: E. coli and the genus Kluyvera (9), neither with a 3GCR
  # result, which takes it out of the 3GCR Total, over 50 patient-days. A
  # dataset lacks a row only where none of its departments detected an
  # infectious agent at or beneath it, so all of them enter the row at 0.
  ref_ds <- with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi",
                           department_key = c(1L, 1L, 2L, 3L)),
    findings = tibble::tibble(
      form = 1:3,
      pathogen_key = c(1807L, 934L, 390L),
      `3gcr` = c("yes", NA, NA),
      car = NA_character_,
      cor = NA_character_,
      mrsa = c(NA, "yes", NA),
      vre = NA_character_),
    patient_days = c(100L, 100L, 40L)))
  own_ds <- with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi",
                           department_key = c(1L, 1L)),
    findings = tibble::tibble(
      form = 1:2,
      pathogen_key = c(1807L, 9L),
      `3gcr` = NA_character_,
      car = NA_character_,
      cor = NA_character_,
      mrsa = NA_character_,
      vre = NA_character_),
    patient_days = 50L))
  own <- calculate_department_data(own_ds, use_cache = FALSE)
  ref <- calculate_reference_data(ref_ds, use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  density <- benchmark$abr_infection_rate_ontology_table
  gcr <- density[density$abr == "3gcr", ]

  # The department's 3GCR Total leaves it out, while the rows of
  # K. pneumoniae, which it lacks, hold it at 0 over its 50 patient-days, as
  # does every row of the MRSA block, all of which it lacks.
  expect_equal(gcr$total_own[is.na(gcr$node_id)], 0L)
  klebsiella <- gcr[gcr$node_id %in% c("LPSN|genus/klebsiella",
                                       "LPSN|species/klebsiella-pneumoniae"), ]
  expect_equal(nrow(klebsiella), 2L)
  expect_true(all(klebsiella$n_own == 0L))
  expect_true(all(klebsiella$total_own == 50L))
  mrsa <- density[density$abr == "mrsa" & !is.na(density$node_id), ]
  expect_gt(nrow(mrsa), 0L)
  expect_true(all(mrsa$total_own == 50L))

  # The reference's 3GCR Total leaves department 2 out, while the row of
  # Kluyvera, which the reference lacks, holds all 3 departments at 0 over
  # their 240 patient-days, the 2 with at least 50 of them in its quartiles.
  expect_equal(gcr$total_ref[is.na(gcr$node_id)], 140L)
  expect_equal(gcr$quartile_departments_ref[is.na(gcr$node_id)], 1L)
  expect_equal(ref$n_patient_days$quartile_departments, 2L)
  kluyvera <- gcr[gcr$node_id %in% "LPSN|genus/kluyvera", ]
  expect_equal(nrow(kluyvera), 1L)
  expect_equal(kluyvera$n_ref, 0L)
  expect_equal(kluyvera$total_ref, 240L)
  expect_equal(kluyvera$quartile_departments_ref, 2L)
  expect_identical(kluyvera$pooled_ref, 0)
  expect_equal(kluyvera$ci_upper_ref,
               neoipcr::neoipc_poisson_ci(0L, 240L, multiplier = 1000)$upper)
  expect_identical(kluyvera$q1_ref, NA_real_)
})

test_that("get_benchmark_data gives a department its own counts on a resistance block's rows, a value recorded for a category that cannot apply included", {
  # The department's only 3GCR result: Staphylococcus aureus (934) recorded
  # resistant, a category the list does not apply to it. The reference holds
  # the department and another one, whose Escherichia coli (1807) is not 3GCR
  # resistant.
  aureus <- tibble::tibble(form = 1L, pathogen_key = 934L, `3gcr` = "yes")
  coli <- tibble::tibble(form = 2L, pathogen_key = 1807L, `3gcr` = "no")
  own <- calculate_department_data(make_sepsis_findings_test_ds(aureus),
                                   use_cache = FALSE)
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(dplyr::bind_rows(aureus, coli),
                                 department_key = 1:2),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # The rows S. aureus lies beneath: the Bacteria, and over the reporting
  # groups the facultative anaerobes, the other department's E. coli among
  # them too.
  beneath <- c("bacteria", "oxygen_requirement|facultative-anaerobic",
               "LPSN|domain/bacteria")
  for (slot in c(neoipcr:::abr_infection_rate_table_names,
                 neoipcr:::infectious_agent_resistance_rate_table_names)) {
    joined <- benchmark[[slot]]
    gcr <- joined[joined$abr == "3gcr" & !is.na(joined$node_id), ]
    at <- gcr$node_id %in% beneath
    expect_gt(sum(at), 0L)
    # The department's infection, the one the reference counts there.
    expect_identical(gcr$n_own[at], rep(1L, sum(at)), label = slot)
    expect_identical(gcr$n_ref[at], rep(1L, sum(at)), label = slot)
    expect_identical(gcr$n_own[!at], rep(0L, sum(!at)), label = slot)
    if (slot %in% neoipcr:::infectious_agent_resistance_rate_table_names) {
      expect_identical(gcr$total_own[at], rep(1L, sum(at)), label = slot)
      expect_identical(gcr$total_ref[at], rep(2L, sum(at)), label = slot)
    }
  }
})

test_that("get_benchmark_data gives a department its residual row where another department of the reference records an infectious agent beneath the node", {
  # The department records the genus Staphylococcus (481) alone; the
  # reference holds it and another department with Staphylococcus aureus
  # (934).
  genus <- tibble::tibble(form = 1L, pathogen_key = 481L)
  aureus <- tibble::tibble(form = 2L, pathogen_key = 934L)
  own <- calculate_department_data(make_sepsis_findings_test_ds(genus),
                                   use_cache = FALSE)
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(dplyr::bind_rows(genus, aureus),
                                 department_key = 1:2),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)

  detected <- benchmark$infectious_agent_detection_rate_per_agent_ontology_table
  genus_row <- detected[detected$node_id %in% "LPSN|genus/staphylococcus", ]
  residual <- detected[
    detected$node_id %in% "LPSN|genus/staphylococcus~n.o.s.", ]
  expect_identical(genus_row$n_own, 1L)
  expect_identical(residual$n_own, genus_row$n_own)
  expect_identical(residual$total_own, genus_row$total_own)
  expect_identical(residual$n_ref, 1L)
})

test_that("get_benchmark_data keeps a residual row last among its node's children, after a child the reference lacks", {
  # The reference: the genus Enterococcus (503), not vancomycin-resistant
  # Enterococcus (VRE), in department 1, and Enterococcus faecium (1865), VRE,
  # in department 2, so that the genus has a residual row. The department:
  # Enterococcus faecalis (2345), VRE, which the reference lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(503L, 1865L),
                     vre = c("no", "yes")),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 2345L, vre = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  genus <- "LPSN|genus/enterococcus"
  faecium <- "LPSN|species/enterococcus-faecium"
  faecalis <- "LPSN|species/enterococcus-faecalis"
  residual <- paste0(genus, "~n.o.s.")
  children <- function(tbl) tbl$node_id[tbl$parent_id %in% genus]

  # The infectious agents detected over the full ontology, and the VRE block
  # of both resistance tables over it: the reference's E. faecium, then the
  # department's E. faecalis, then the reference's residual row.
  vre <- function(tbl) tbl[tbl$abr == "vre", ]
  for (case in list(
    list(slot = "infectious_agent_detection_rate_per_agent_ontology_table",
         block = identity),
    list(slot = "abr_infection_rate_ontology_table", block = vre),
    list(slot = "infectious_agent_resistance_rate_ontology_table",
         block = vre))) {
    expect_identical(children(case$block(ref[[case$slot]])),
                     c(faecium, residual), label = case$slot)
    expect_identical(children(case$block(own[[case$slot]])), faecalis,
                     label = case$slot)
    expect_identical(children(case$block(benchmark[[case$slot]])),
                     c(faecium, faecalis, residual), label = case$slot)
  }
})

test_that("get_benchmark_data keeps the summary block above the tree, a summary row the reference lacks in the classification's order", {
  # The reference: Escherichia coli (1807) in two departments, recorded
  # carbapenem resistant in one. The department: Staphylococcus aureus (934)
  # and Acinetobacter baumannii (1660), carbapenem resistant, which add a
  # Gram-positive and an aerobic summary row the reference lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = 1807L, car = c("yes", "no")),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(934L, 1660L),
                     car = c(NA, "yes"))),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)

  # The reference's rows in its order, each row the department adds after
  # its reference siblings.
  expect_identical(
    benchmark$infectious_agent_detection_rate_per_agent_table$node_id,
    c(NA, "gram_reaction|positive", "gram_reaction|negative",
      "oxygen_requirement|aerobic", "oxygen_requirement|facultative-anaerobic",
      "bacteria", "enterobacterales", "gram-negative-bacilli",
      "non-fermenting-gram-negative-bacilli", "gram-positive-cocci"))
  car <- c("gram_reaction|negative", "oxygen_requirement|aerobic",
           "oxygen_requirement|facultative-anaerobic", "bacteria",
           "enterobacterales", "gram-negative-bacilli",
           "non-fermenting-gram-negative-bacilli")
  density <- benchmark$abr_infection_rate_table
  expect_identical(density$node_id[density$abr == "car"], c(NA, car))
  rates <- benchmark$infectious_agent_resistance_rate_table
  expect_identical(rates$node_id[rates$abr == "car"], car)
})

test_that("get_benchmark_data keeps the reference's order of the summary rows attribute by attribute, a summary row it lacks within its attribute's run by the classification's order", {
  # The reference: Escherichia coli (1807), carbapenem not resistant, and
  # Bacteroides fragilis (1296), each in a department of its own. The
  # department: Staphylococcus aureus (934), Campylobacter jejuni (47), and
  # Acinetobacter baumannii (1660), carbapenem resistant, which add the
  # summary rows Gram-positive, oxygen requirement not determined, and
  # aerobic, which the reference lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 1296L),
                     car = c("no", NA)),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:3, pathogen_key = c(934L, 47L, 1660L),
                     car = c(NA, NA, "yes"))),
    use_cache = FALSE)
  values <- neoipcr:::internal_infectious_agent_attribute_values
  summary_ids <- paste0(values$attribute, "|", values$key)
  groups_slots <- c(
    neoipcr:::infectious_agent_detection_table_names[["groups"]],
    neoipcr:::abr_infection_rate_table_names[["groups"]],
    neoipcr:::infectious_agent_resistance_rate_table_names[["groups"]])
  block_of <- function(tbl)
    if ("abr" %in% names(tbl)) as.character(tbl$abr) else rep("", nrow(tbl))

  # No digest covers the classification's order of the attribute values, so
  # a release of the list that orders them otherwise gives the same stamps,
  # and a reference calculated under it lays out its summary rows in that
  # order. Such a reference: the calculated one with the summary rows of each
  # block reversed.
  for (slot in groups_slots) {
    tbl <- ref[[slot]]
    blocks <- block_of(tbl)
    rows <- seq_len(nrow(tbl))
    for (b in unique(blocks)) {
      summary <- which(tbl$node_id %in% summary_ids & blocks == b)
      rows[summary] <- rev(summary)
    }
    ref[[slot]] <- tbl[rows, ]
  }
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # Each block holds the reference's summary rows in the reference's order.
  for (slot in groups_slots) {
    joined <- benchmark[[slot]]
    for (b in unique(block_of(ref[[slot]]))) {
      theirs <- ref[[slot]]$node_id[
        block_of(ref[[slot]]) == b & ref[[slot]]$node_id %in% summary_ids]
      ours <- joined$node_id[block_of(joined) == b]
      expect_identical(ours[ours %in% theirs], theirs,
                       label = paste(slot, b))
    }
  }

  # The infectious agents detected: the Total, then the summary block, its
  # attributes in the reference's order, the oxygen requirement before the
  # Gram reaction. The run of the oxygen requirement
  # holds the reference's rows obligately and facultatively anaerobic, in its
  # order; the aerobic row opens it, the classification ordering no value the
  # reference holds of the attribute before the aerobic one; and the row of
  # the oxygen requirement not determined follows the facultatively anaerobic
  # row, which the reference places last of the values the classification
  # orders before it. The run of the Gram reaction follows, the Gram-positive
  # row before the reference's Gram-negative row. The tree follows.
  detected <- benchmark$infectious_agent_detection_rate_per_agent_table
  opening <- c(NA, "oxygen_requirement|aerobic",
               "oxygen_requirement|obligate-anaerobic",
               "oxygen_requirement|facultative-anaerobic",
               "oxygen_requirement|not-determined",
               "gram_reaction|positive", "gram_reaction|negative")
  expect_identical(detected$node_id[seq_along(opening)], opening)
  expect_false(any(detected$node_id[-seq_along(opening)] %in% summary_ids))
  # The carbapenem and the colistin blocks, in the order the reference's
  # blocks of the table show: the reference's E. coli rows, reversed, the
  # oxygen requirement first, the aerobic row of the department's
  # A. baumannii, which the reference holds in no block, opening its run,
  # before the facultatively anaerobic row.
  summary_rows <- c("oxygen_requirement|aerobic",
                    "oxygen_requirement|facultative-anaerobic",
                    "gram_reaction|negative")
  for (category in c("car", "cor")) {
    density <- benchmark$abr_infection_rate_table
    density <- density$node_id[density$abr == category]
    expect_identical(density[seq_len(4L)], c(NA, summary_rows),
                     label = category)
    expect_false(any(density[-seq_len(4L)] %in% summary_ids))
    rates <- benchmark$infectious_agent_resistance_rate_table
    rates <- rates$node_id[rates$abr == category]
    expect_identical(rates[seq_len(3L)], summary_rows, label = category)
    expect_false(any(rates[-seq_len(3L)] %in% summary_ids))
  }
})

# The `node_id`s of the summary rows, in the bundled classification's order of
# the attributes and their values.
bundled_summary_ids <- function() {
  values <- neoipcr:::internal_infectious_agent_attribute_values
  paste0(values$attribute, "|", values$key)
}

# The calculated dataset `ds` as a release of the infectious-agent list that
# orders the attribute values as `node_ids` does would lay it out: the summary
# rows of each block of its tables over the reporting groups in that order,
# on the rows they hold. No digest covers that order, so such a dataset
# carries the stamps `ds` carries.
with_summary_order <- function(ds, node_ids) {
  for (slot in groups_tables()) {
    tbl <- ds[[slot]]
    blocks <- if ("abr" %in% names(tbl)) as.character(tbl$abr)
              else rep("", nrow(tbl))
    rows <- seq_len(nrow(tbl))
    for (b in unique(blocks)) {
      summary <- which(tbl$node_id %in% node_ids & blocks == b)
      rows[summary] <- summary[order(match(tbl$node_id[summary], node_ids))]
    }
    ds[[slot]] <- tbl[rows, ]
  }
  ds
}

# The `node_id`s of the summary rows of the block of the resistance category
# `category` of `tbl`, in their order: those among `ids`, by default the
# bundled classification's.
block_summary_ids <- function(tbl, category, ids = bundled_summary_ids())
  tbl$node_id[tbl$abr %in% category & tbl$node_id %in% ids]

# The calculated dataset `ds` as a release of the infectious-agent list that
# keys the summary rows `names(keys)` as `keys` would lay it out, in each of
# its tables over the reporting groups. The digest of the tree over the
# reporting groups covers the values' keys, so datasets calculated under such
# a release are joined only with one another: every dataset of a benchmark
# is re-keyed alike, and the stamps they all keep stand in for that
# release's.
with_summary_keys <- function(ds, keys) {
  for (slot in groups_tables()) {
    rekeyed <- ds[[slot]]$node_id %in% names(keys)
    ds[[slot]]$node_id[rekeyed] <- unname(keys[ds[[slot]]$node_id[rekeyed]])
  }
  ds
}

test_that("get_benchmark_data gives a table whose rows are infectious agents the order of the first reference dataset joined into it, or else of the first dataset that holds it", {
  # A department of Escherichia coli (1807) and Staphylococcus aureus (934),
  # and a reference of the same infectious agents in two departments, each
  # also as a release of the list that orders the Gram-negative value before
  # the Gram-positive one would lay it out, with the same stamps.
  swapped <- c("gram_reaction|positive", "gram_reaction|negative")
  release <- bundled_summary_ids()
  release[match(swapped, release)] <- rev(swapped)
  agents <- tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L))
  own <- calculate_department_data(make_sepsis_findings_test_ds(agents),
                                   use_cache = FALSE)
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(agents, department_key = 1:2),
    use_cache = FALSE)
  own_swapped <- with_summary_order(own, release)
  ref_swapped <- with_summary_order(ref, release)
  slot <- "infectious_agent_detection_rate_per_agent_table"
  summary_rows <- function(ds)
    ds[[slot]]$node_id[ds[[slot]]$node_id %in% bundled_summary_ids()]
  positive_first <- c("gram_reaction|positive", "gram_reaction|negative",
                      "oxygen_requirement|facultative-anaerobic")
  negative_first <- positive_first[c(2L, 1L, 3L)]
  expect_identical(summary_rows(own), positive_first)
  expect_identical(summary_rows(ref), positive_first)
  expect_identical(summary_rows(own_swapped), negative_first)
  expect_identical(summary_rows(ref_swapped), negative_first)

  # 1. A reference whose table is not joined gives it no order, so the
  #    department passed before it keeps its own.
  changed <- ref_swapped
  stamps <- changed$metadata$rate_definitions
  stamps$version[stamps$table == slot] <-
    stamps$version[stamps$table == slot] + 1L
  changed$metadata$rate_definitions <- stamps
  r <- benchmark_warnings(get_benchmark_data(own = own, ref = changed))
  expect_identical(r$value$not_comparable,
                   tibble::tibble(table = slot, dataset = "ref"))
  expect_identical(summary_rows(r$value), positive_first)

  # 2. Of two references joined after the department, the first gives the
  #    order, whichever it is.
  for (case in list(
    list(label = "ref, ref_swapped", first = ref, second = ref_swapped,
         expected = positive_first),
    list(label = "ref_swapped, ref", first = ref_swapped, second = ref,
         expected = negative_first))) {
    benchmark <- get_benchmark_data(own = own, ref1 = case$first,
                                    ref2 = case$second)
    expect_equal(nrow(benchmark$not_comparable), 0L, label = case$label)
    expect_identical(summary_rows(benchmark), case$expected,
                     label = case$label)
  }

  # 3. Of two departments benchmarked against each other, the first gives
  #    the order, whichever it is.
  for (case in list(
    list(label = "own, own_swapped", first = own, second = own_swapped,
         expected = positive_first),
    list(label = "own_swapped, own", first = own_swapped, second = own,
         expected = negative_first))) {
    benchmark <- get_benchmark_data(d1 = case$first, d2 = case$second)
    expect_equal(nrow(benchmark$not_comparable), 0L, label = case$label)
    expect_identical(summary_rows(benchmark), case$expected,
                     label = case$label)
  }
})

test_that("get_benchmark_data keeps an attribute's summary rows together where the reference orders its values otherwise", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own, calculated under a release of the list
  # that orders the Gram-negative value before the Gram-positive one. The
  # department: Acinetobacter baumannii (1660), which adds the aerobic row
  # the reference lacks.
  swapped <- c("gram_reaction|positive", "gram_reaction|negative")
  release <- bundled_summary_ids()
  release[match(swapped, release)] <- rev(swapped)
  ref <- with_summary_order(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
        department_key = 1:2),
      use_cache = FALSE),
    release)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 1660L)),
    use_cache = FALSE)
  expect_identical(
    ref$infectious_agent_detection_rate_per_agent_table$node_id[seq_len(4L)],
    c(NA, "gram_reaction|negative", "gram_reaction|positive",
      "oxygen_requirement|facultative-anaerobic"))
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # The run of the Gram reaction, in the reference's order, then that of the
  # oxygen requirement, which the aerobic row opens, before the reference's
  # facultatively anaerobic row: the classification orders no value the
  # reference holds of the attribute before the aerobic one.
  detected <- benchmark$infectious_agent_detection_rate_per_agent_table
  opening <- c(NA, "gram_reaction|negative", "gram_reaction|positive",
               "oxygen_requirement|aerobic",
               "oxygen_requirement|facultative-anaerobic")
  expect_identical(detected$node_id[seq_along(opening)], opening)
  expect_false(
    any(detected$node_id[-seq_along(opening)] %in% bundled_summary_ids()))
})

test_that("get_benchmark_data places a summary row the reference lacks in its attribute's run where the reference orders the attributes otherwise", {
  # The reference: Acinetobacter baumannii (1660), calculated under a release
  # of the list that orders the oxygen requirement before the Gram reaction.
  # The department: Staphylococcus aureus (934), which adds the facultatively
  # anaerobic and the Gram-positive rows the reference lacks, and the MRSA
  # block, which the reference lacks whole.
  ids <- bundled_summary_ids()
  release <- c(ids[startsWith(ids, "oxygen_requirement|")],
               ids[startsWith(ids, "gram_reaction|")])
  ref <- with_summary_order(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1L, pathogen_key = 1660L)),
      use_cache = FALSE),
    release)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 934L)),
    use_cache = FALSE)
  expect_identical(
    ref$infectious_agent_detection_rate_per_agent_table$node_id[seq_len(3L)],
    c(NA, "oxygen_requirement|aerobic", "gram_reaction|negative"))
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # The run of the oxygen requirement, the facultatively anaerobic row after
  # the reference's aerobic row, which the classification orders before it,
  # then that of the Gram reaction, the Gram-positive row before the
  # reference's Gram-negative row, the classification ordering no value the
  # reference holds of the attribute before it.
  detected <- benchmark$infectious_agent_detection_rate_per_agent_table
  opening <- c(NA, "oxygen_requirement|aerobic",
               "oxygen_requirement|facultative-anaerobic",
               "gram_reaction|positive", "gram_reaction|negative")
  expect_identical(detected$node_id[seq_along(opening)], opening)
  expect_false(any(detected$node_id[-seq_along(opening)] %in% ids))
  # The MRSA block, of which the reference holds no row: its attributes in
  # the order the reference's carbapenem and colistin blocks show, the oxygen
  # requirement first.
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(
      ref[[slot]]$node_id[ref[[slot]]$abr %in% c("car", "cor") &
                            ref[[slot]]$node_id %in% ids],
      rep(c("oxygen_requirement|aerobic", "gram_reaction|negative"), 2L),
      label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "mrsa"),
                     c("oxygen_requirement|facultative-anaerobic",
                       "gram_reaction|positive"),
                     label = slot)
  }
})

test_that("get_benchmark_data lays out a block the reference holds partly in the order of the attributes its other blocks show", {
  # The reference: Staphylococcus aureus (934), recorded 3GCR resistant, a
  # category the list does not apply to it, in two departments, calculated
  # under a release of the list that orders the oxygen requirement before the
  # Gram reaction. Its 3GCR block holds the facultatively anaerobic row
  # alone, no 3GCR block having a Gram-positive row, and its MRSA block both
  # attributes, in that release's order. The department:
  # Escherichia coli (1807), 3GCR resistant, which adds the Gram-negative row
  # to the 3GCR block.
  ids <- bundled_summary_ids()
  release <- c(ids[startsWith(ids, "oxygen_requirement|")],
               ids[startsWith(ids, "gram_reaction|")])
  ref <- with_summary_order(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = 934L, `3gcr` = "yes"),
        department_key = 1:2),
      use_cache = FALSE),
    release)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 1807L, `3gcr` = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the 3GCR block in
  # the order of the attributes the reference's MRSA block shows, the oxygen
  # requirement first, the MRSA block as the reference holds it.
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(block_summary_ids(ref[[slot]], "3gcr"),
                     "oxygen_requirement|facultative-anaerobic", label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "mrsa"),
                     c("oxygen_requirement|facultative-anaerobic",
                       "gram_reaction|positive"), label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "3gcr"),
                     c("oxygen_requirement|facultative-anaerobic",
                       "gram_reaction|negative"), label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "mrsa"),
                     block_summary_ids(ref[[slot]], "mrsa"), label = slot)
  }
})

test_that("get_benchmark_data lays out a block the reference holds partly in the order of the values its other blocks show", {
  # The reference: Escherichia coli (1807), not 3GCR resistant, and
  # Acinetobacter baumannii (1660), not carbapenem resistant, each in a
  # department of its own, calculated under a release of the list that
  # orders the facultatively anaerobic value before the aerobic one. Its
  # carbapenem block holds both values, in that release's order, and its
  # 3GCR block, whose category the list applies to E. coli and not to
  # A. baumannii, the facultatively anaerobic one alone. The department:
  # A. baumannii recorded 3GCR resistant, a category the list does not apply
  # to it, which adds the aerobic row to the 3GCR block.
  swapped <- c("oxygen_requirement|aerobic",
               "oxygen_requirement|facultative-anaerobic")
  release <- bundled_summary_ids()
  release[match(swapped, release)] <- rev(swapped)
  ref <- with_summary_order(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = c(1807L, 1660L),
                       `3gcr` = c("no", NA), car = c(NA, "no")),
        department_key = 1:2),
      use_cache = FALSE),
    release)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 1660L, `3gcr` = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the 3GCR block in
  # the order of the values the reference's carbapenem block shows, the
  # aerobic row after the facultatively anaerobic one, although the bundled
  # classification orders it first.
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(block_summary_ids(ref[[slot]], "car"),
                     c("gram_reaction|negative", rev(swapped)), label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "3gcr"),
                     c("gram_reaction|negative",
                       "oxygen_requirement|facultative-anaerobic"),
                     label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "3gcr"),
                     c("gram_reaction|negative", rev(swapped)), label = slot)
  }
})

test_that("get_benchmark_data keeps the order of every block of the reference where a later block holds a value before those an earlier block holds", {
  # The reference: Escherichia coli (1807) and Pseudomonas pertucinogena
  # (1691), not 3GCR resistant, and Acinetobacter baumannii (1660), not
  # carbapenem resistant, each in a department of its own. Its 3GCR block,
  # whose category the list applies to E. coli and P. pertucinogena and not
  # to A. baumannii, holds the facultatively anaerobic row and the row of the
  # oxygen requirement not determined; its carbapenem block holds the aerobic
  # row before both. The department: A. baumannii recorded 3GCR resistant, a
  # category the list does not apply to it, which adds the aerobic row to the
  # 3GCR block.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:3, pathogen_key = c(1807L, 1691L, 1660L),
                     `3gcr` = c("no", "no", NA), car = c(NA, NA, "no")),
      department_key = 1:3),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 1660L, `3gcr` = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the carbapenem block
  # as the reference holds it, and the 3GCR block with the aerobic row before
  # the facultatively anaerobic one, as the carbapenem block orders them.
  oxygen <- c("oxygen_requirement|aerobic",
              "oxygen_requirement|facultative-anaerobic",
              "oxygen_requirement|not-determined")
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(block_summary_ids(ref[[slot]], "3gcr"),
                     c("gram_reaction|negative", oxygen[-1L]), label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "car"),
                     c("gram_reaction|negative", oxygen), label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "car"),
                     c("gram_reaction|negative", oxygen), label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "3gcr"),
                     c("gram_reaction|negative", oxygen), label = slot)
  }
})

test_that("get_benchmark_data places first, of two attributes or two values the reference's blocks let come next, the one the classification orders first", {
  # 1. The reference: Streptococcus pneumoniae (126), recorded 3GCR
  #    resistant, and Kytococcus sedentarius (28), recorded VRE, categories
  #    the list does not apply to them, each in a department of its own. Its
  #    3GCR block holds the facultatively anaerobic row, its VRE block the
  #    Gram-positive row, and no block both attributes. The department:
  #    Escherichia coli (1807), 3GCR resistant, which adds the Gram-negative
  #    row to the 3GCR block, before the reference's facultatively anaerobic
  #    row, the classification ordering the Gram reaction first.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(126L, 28L),
                     `3gcr` = c("yes", NA), vre = c(NA, "yes")),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 1807L, `3gcr` = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(block_summary_ids(ref[[slot]], "3gcr"),
                     "oxygen_requirement|facultative-anaerobic", label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "vre"),
                     "gram_reaction|positive", label = slot)
    expect_identical(
      ref[[slot]]$node_id[ref[[slot]]$node_id %in% bundled_summary_ids()],
      c("oxygen_requirement|facultative-anaerobic", "gram_reaction|positive"),
      label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "3gcr"),
                     c("gram_reaction|negative",
                       "oxygen_requirement|facultative-anaerobic"),
                     label = slot)
  }

  # 2. The reference: Escherichia coli (1807) and Staphylococcus aureus
  #    (934), each in a department of its own. Its blocks of 3GCR,
  #    carbapenems, and colistin hold the Gram-negative row, its MRSA block
  #    the Gram-positive row, and no block both. The department:
  #    Streptococcus parvulus (26), Gram-positive, carbapenem resistant, which
  #    adds the Gram-positive row and the row of the oxygen requirement not
  #    determined to the carbapenem block.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 26L, car = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the carbapenem block
  # with the Gram-positive row before the Gram-negative one, as the
  # classification orders them, then the run of the oxygen requirement.
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    for (category in c("3gcr", "car", "cor"))
      expect_identical(block_summary_ids(ref[[slot]], category),
                       c("gram_reaction|negative",
                         "oxygen_requirement|facultative-anaerobic"),
                       label = paste(slot, category))
    expect_identical(block_summary_ids(ref[[slot]], "mrsa"),
                     c("gram_reaction|positive",
                       "oxygen_requirement|facultative-anaerobic"),
                     label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "car"),
                     c("gram_reaction|positive", "gram_reaction|negative",
                       "oxygen_requirement|facultative-anaerobic",
                       "oxygen_requirement|not-determined"),
                     label = slot)
  }
})

test_that("get_benchmark_data keeps the order that the reference's blocks give two values through others, although no block holds both", {
  # The department: Campylobacter sputorum subsp. bubulus (22),
  # Gram-negative, its oxygen requirement not determined, to which the list
  # applies no category, and Acinetobacter baumannii (1660), aerobic, both
  # recorded carbapenem resistant, which add the row of the oxygen
  # requirement not determined to the carbapenem block. Each reference holds
  # each of its infectious agents in a department of its own and is
  # calculated under a release of the list that orders the oxygen
  # requirement not determined before the facultatively anaerobic one, and
  # that before the aerobic one, the bundled classification ordering them
  # the other way round. Three cases:
  #   1. The reference: Escherichia coli (1807); C. sputorum subsp. bubulus,
  #      recorded not 3GCR resistant; and A. baumannii. Its 3GCR block holds
  #      the row not determined before the facultatively anaerobic one, and
  #      its blocks of carbapenems and colistin the facultatively anaerobic
  #      row before the aerobic one: no block holds the aerobic row and the
  #      row not determined together, but the blocks order them through the
  #      facultatively anaerobic row.
  #   2. The same reference without E. coli: no block holds the
  #      facultatively anaerobic row, so the blocks order the aerobic row and
  #      the row not determined neither directly nor through others.
  #   3. The merge of the blocks' orders, `.merge_chains()`, given the orders
  #      of two blocks that link two values through a third, and of three
  #      blocks that link them through two others, the classification
  #      ranking the last value of each link first.
  negative <- "gram_reaction|negative"
  aerobic <- "oxygen_requirement|aerobic"
  facultative <- "oxygen_requirement|facultative-anaerobic"
  undetermined <- "oxygen_requirement|not-determined"
  release <- bundled_summary_ids()
  release[match(c(aerobic, facultative, undetermined), release)] <-
    c(undetermined, facultative, aerobic)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(22L, 1660L), car = "yes")),
    use_cache = FALSE)
  reference <- function(pathogen_key, gcr)
    with_summary_order(
      calculate_reference_data(
        make_sepsis_findings_test_ds(
          tibble::tibble(form = seq_along(pathogen_key),
                         pathogen_key = pathogen_key, `3gcr` = gcr),
          department_key = seq_along(pathogen_key)),
        use_cache = FALSE),
      release)
  cases <- list(
    linked = list(
      ref = reference(c(1807L, 22L, 1660L), c(NA, "no", NA)),
      gcr = c(negative, undetermined, facultative),
      car = c(negative, facultative, aerobic),
      expected = c(negative, undetermined, facultative, aerobic)),
    unlinked = list(
      ref = reference(c(22L, 1660L), c("no", NA)),
      gcr = c(negative, undetermined),
      car = c(negative, aerobic),
      expected = c(negative, aerobic, undetermined)))

  # Cases 1 and 2: in both resistance tables over the reporting groups, the
  # carbapenem block with the row not determined before the aerobic one, as
  # the reference's blocks order them through the facultatively anaerobic
  # row, and, without that row, after the aerobic one, as the classification
  # orders them.
  for (name in names(cases)) {
    case <- cases[[name]]
    benchmark <- get_benchmark_data(own = own, ref = case$ref)
    expect_equal(nrow(benchmark$not_comparable), 0L, label = name)
    for (slot in c("abr_infection_rate_table",
                   "infectious_agent_resistance_rate_table")) {
      label <- paste(name, slot)
      expect_identical(block_summary_ids(case$ref[[slot]], "3gcr"),
                       case$gcr, label = label)
      for (category in c("car", "cor"))
        expect_identical(block_summary_ids(case$ref[[slot]], category),
                         case$car, label = paste(label, category))
      expect_identical(block_summary_ids(benchmark[[slot]], "car"),
                       case$expected, label = label)
    }
  }

  # Case 3: the order the blocks give, through a third value and through two
  # others, not the classification's.
  merge <- neoipcr:::.merge_chains
  expect_identical(merge(list(c("a", "c"), c("c", "b")),
                         \(x) match(x, c("b", "a", "c"))),
                   c("a", "c", "b"))
  expect_identical(merge(list(c("a", "c"), c("c", "d"), c("d", "b")),
                         \(x) match(x, c("b", "a", "c", "d"))),
                   c("a", "c", "d", "b"))
})

test_that("get_benchmark_data places a value that the reference's blocks let come next before one that a block makes wait, although the classification orders the waiting one first", {
  # The reference, each infectious agent in a department of its own and
  # calculated under a release of the list that orders the oxygen
  # requirement not determined before the facultatively anaerobic one, and
  # that before the aerobic one, the bundled classification ordering them
  # the other way round: Campylobacter sputorum subsp. bubulus (22),
  # Gram-negative, its oxygen requirement not determined, recorded not
  # resistant to third-generation cephalosporins or carbapenems;
  # Acinetobacter baumannii (1660), aerobic; and Staphylococcus aureus (934),
  # facultatively anaerobic. Its blocks of carbapenems and colistin hold the
  # aerobic row, the carbapenem block after the row not determined, which the
  # 3GCR block holds too, and its MRSA block the facultatively anaerobic row:
  # no block holds the facultatively anaerobic row with another value of the
  # oxygen requirement, nor links it to one. The department: Escherichia coli
  # (1807), facultatively anaerobic, and A. baumannii, both recorded 3GCR
  # resistant, which add the facultatively anaerobic and the aerobic row to
  # the 3GCR block and the facultatively anaerobic row to the carbapenem
  # block.
  negative <- "gram_reaction|negative"
  positive <- "gram_reaction|positive"
  aerobic <- "oxygen_requirement|aerobic"
  facultative <- "oxygen_requirement|facultative-anaerobic"
  undetermined <- "oxygen_requirement|not-determined"
  release <- bundled_summary_ids()
  release[match(c(aerobic, facultative, undetermined), release)] <-
    c(undetermined, facultative, aerobic)
  ref <- with_summary_order(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:3, pathogen_key = c(22L, 1660L, 934L),
                       `3gcr` = c("no", NA, NA), car = c("no", NA, NA)),
        department_key = 1:3),
      use_cache = FALSE),
    release)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 1660L),
                     `3gcr` = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the blocks of 3GCR
  # and carbapenems with the facultatively anaerobic row first of the oxygen
  # requirement, since the blocks let it come first, then the row not
  # determined, and last the aerobic row, which the carbapenem block makes
  # wait for that row, although the classification orders it first.
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(block_summary_ids(ref[[slot]], "3gcr"),
                     c(negative, undetermined), label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "car"),
                     c(negative, undetermined, aerobic), label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "cor"),
                     c(negative, aerobic), label = slot)
    expect_identical(block_summary_ids(ref[[slot]], "mrsa"),
                     c(positive, facultative), label = slot)
    for (category in c("3gcr", "car"))
      expect_identical(block_summary_ids(benchmark[[slot]], category),
                       c(negative, facultative, undetermined, aerobic),
                       label = paste(slot, category))
  }

  # The merge of the blocks' orders, `.merge_chains()`: of the elements the
  # chains let come next, the first by the classification, so that one a
  # chain makes wait follows a free one the classification ranks after it.
  expect_identical(neoipcr:::.merge_chains(list(c("c", "a"), "b"),
                                           \(x) match(x, c("a", "b", "c"))),
                   c("b", "c", "a"))
})

test_that("get_benchmark_data places, of the values the reference's blocks let come next, one the bundle lacks after those it holds", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own. The department: Streptococcus
  # parvulus (26), Gram-positive, carbapenem resistant. Both calculated under
  # a release of the list that keys the Gram-negative value `gram-negative`,
  # which the bundle lacks. The reference's blocks of 3GCR, carbapenems, and
  # colistin hold the Gram-negative row, its MRSA block the Gram-positive
  # row, and no block both; the department adds the Gram-positive row and
  # the row of the oxygen requirement not determined to the carbapenem block.
  keys <- c("gram_reaction|negative" = "gram_reaction|gram-negative")
  ref <- with_summary_keys(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
        department_key = 1:2),
      use_cache = FALSE),
    keys)
  own <- with_summary_keys(
    calculate_department_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1L, pathogen_key = 26L, car = "yes")),
      use_cache = FALSE),
    keys)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the carbapenem block
  # with the Gram-positive row before the Gram-negative one, whose value the
  # classification lacks, then the run of the oxygen requirement.
  ids <- c(bundled_summary_ids(), unname(keys))
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    for (category in c("3gcr", "car", "cor"))
      expect_identical(block_summary_ids(ref[[slot]], category, ids),
                       c("gram_reaction|gram-negative",
                         "oxygen_requirement|facultative-anaerobic"),
                       label = paste(slot, category))
    expect_identical(block_summary_ids(ref[[slot]], "mrsa", ids),
                     c("gram_reaction|positive",
                       "oxygen_requirement|facultative-anaerobic"),
                     label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "car", ids),
                     c("gram_reaction|positive", "gram_reaction|gram-negative",
                       "oxygen_requirement|facultative-anaerobic",
                       "oxygen_requirement|not-determined"),
                     label = slot)
  }
})

test_that("get_benchmark_data places first, of two values the bundle lacks that the reference's blocks let come next, the one the reference's table shows first", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own. The department: Streptococcus
  # parvulus (26), Gram-positive, carbapenem resistant. Both calculated under
  # a release of the list that keys the Gram-negative value `g-negative` and
  # the Gram-positive one `g-positive`, neither of which the bundle holds.
  # The reference's table shows the Gram-negative row first, in its blocks
  # of 3GCR, carbapenems, and colistin, and the Gram-positive row only in its
  # MRSA block, after them; the department adds the Gram-positive row and the
  # row of the oxygen requirement not determined to the carbapenem block.
  keys <- c("gram_reaction|negative" = "gram_reaction|g-negative",
            "gram_reaction|positive" = "gram_reaction|g-positive")
  ref <- with_summary_keys(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
        department_key = 1:2),
      use_cache = FALSE),
    keys)
  own <- with_summary_keys(
    calculate_department_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1L, pathogen_key = 26L, car = "yes")),
      use_cache = FALSE),
    keys)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # In both resistance tables over the reporting groups, the carbapenem block
  # with the Gram-negative row before the Gram-positive one, in the order in
  # which the reference's table first shows them, then the run of the oxygen
  # requirement.
  ids <- c(bundled_summary_ids(), unname(keys))
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_identical(ref[[slot]]$node_id[ref[[slot]]$node_id %in% keys],
                     c(rep("gram_reaction|g-negative", 3L),
                       "gram_reaction|g-positive"),
                     label = slot)
    expect_identical(block_summary_ids(own[[slot]], "car", ids),
                     c("gram_reaction|g-positive",
                       "oxygen_requirement|not-determined"),
                     label = slot)
    expect_identical(block_summary_ids(benchmark[[slot]], "car", ids),
                     c("gram_reaction|g-negative", "gram_reaction|g-positive",
                       "oxygen_requirement|facultative-anaerobic",
                       "oxygen_requirement|not-determined"),
                     label = slot)
  }
})

test_that("get_benchmark_data places an attribute of which the reference holds no row in a table by the classification's order", {
  # Two cases, each a reference in two departments whose resistance tables
  # over the reporting groups hold summary rows of one attribute only, and a
  # department that adds a row of the other attribute to the reference's
  # block:
  #   1. The reference: Streptococcus pneumoniae (126), recorded 3GCR
  #      resistant, a category the list does not apply to it, nor any other:
  #      its one block, 3GCR, holds the facultatively anaerobic row, no 3GCR
  #      block having a Gram-positive row. The department:
  #      Escherichia coli (1807), 3GCR resistant, which adds the
  #      Gram-negative row, before the reference's attribute, the
  #      classification ordering the Gram reaction first.
  #   2. The reference: Kytococcus sedentarius (28), recorded VRE, a category
  #      the list does not apply to it, nor any other: its one block, VRE,
  #      holds the Gram-positive row, no VRE block having an aerobic row. The
  #      department: Enterococcus faecalis (2345), VRE, which adds the
  #      facultatively anaerobic row, after the reference's attribute.
  cases <- list(
    list(reference = 126L, department = 1807L, category = "3gcr",
         held = "oxygen_requirement|facultative-anaerobic",
         expected = c("gram_reaction|negative",
                      "oxygen_requirement|facultative-anaerobic")),
    list(reference = 28L, department = 2345L, category = "vre",
         held = "gram_reaction|positive",
         expected = c("gram_reaction|positive",
                      "oxygen_requirement|facultative-anaerobic")))
  for (case in cases) {
    findings <- function(form, pathogen_key) {
      findings <- tibble::tibble(form = form, pathogen_key = pathogen_key)
      findings[[case$category]] <- "yes"
      findings
    }
    ref <- calculate_reference_data(
      make_sepsis_findings_test_ds(findings(1:2, case$reference),
                                   department_key = 1:2),
      use_cache = FALSE)
    own <- calculate_department_data(
      make_sepsis_findings_test_ds(findings(1L, case$department)),
      use_cache = FALSE)
    benchmark <- get_benchmark_data(own = own, ref = ref)
    expect_equal(nrow(benchmark$not_comparable), 0L)

    for (slot in c("abr_infection_rate_table",
                   "infectious_agent_resistance_rate_table")) {
      label <- paste(slot, case$category)
      expect_identical(
        ref[[slot]]$node_id[ref[[slot]]$node_id %in% bundled_summary_ids()],
        case$held, label = label)
      expect_identical(block_summary_ids(benchmark[[slot]], case$category),
                       case$expected, label = label)
    }
  }
})

test_that("get_benchmark_data places two attributes of which the dataset whose order a table takes holds no row in the classification's order", {
  # The department: Escherichia coli (1807), 3GCR resistant, and
  # Staphylococcus aureus (934), MRSA, whose summary rows of the Gram
  # reaction and of the oxygen requirement share a place in each table over
  # the reporting groups, the dataset whose order the table takes holding no
  # summary row there. Two cases, both of Candida albicans (2273), to which
  # the list applies no category:
  #   1. a reference in two departments, whose order the table takes
  #      wherever it is passed;
  #   2. a department passed first, whose order the table takes for want of
  #      a reference.
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L),
                     `3gcr` = c("yes", NA), mrsa = c(NA, "yes"))),
    use_cache = FALSE)
  candida <- tibble::tibble(form = 1:2, pathogen_key = 2273L)
  cases <- list(
    reference = list(
      first = calculate_reference_data(
        make_sepsis_findings_test_ds(candida, department_key = 1:2),
        use_cache = FALSE),
      benchmark = \(first) get_benchmark_data(own = own, ref = first)),
    department = list(
      first = calculate_department_data(
        make_sepsis_findings_test_ds(candida), use_cache = FALSE),
      benchmark = \(first) get_benchmark_data(candida = first, own = own)))

  # In every table over the reporting groups, the run of the Gram reaction
  # before that of the oxygen requirement, as the classification orders
  # them: in the table of the infectious agents detected, and in the blocks
  # of both resistance tables, those of the categories the list applies to
  # E. coli and the MRSA block of S. aureus.
  gram_negative <- c("gram_reaction|negative",
                     "oxygen_requirement|facultative-anaerobic")
  gram_positive <- c("gram_reaction|positive",
                     "oxygen_requirement|facultative-anaerobic")
  for (name in names(cases)) {
    first <- cases[[name]]$first
    for (slot in groups_tables())
      expect_false(any(first[[slot]]$node_id %in% bundled_summary_ids()),
                   label = paste(name, slot))
    benchmark <- cases[[name]]$benchmark(first)
    expect_equal(nrow(benchmark$not_comparable), 0L, label = name)

    detected <- benchmark$infectious_agent_detection_rate_per_agent_table
    expect_identical(
      detected$node_id[detected$node_id %in% bundled_summary_ids()],
      c("gram_reaction|positive", "gram_reaction|negative",
        "oxygen_requirement|facultative-anaerobic"),
      label = name)
    for (slot in c("abr_infection_rate_table",
                   "infectious_agent_resistance_rate_table")) {
      for (category in c("3gcr", "car", "cor"))
        expect_identical(block_summary_ids(benchmark[[slot]], category),
                         gram_negative,
                         label = paste(name, slot, category))
      expect_identical(block_summary_ids(benchmark[[slot]], "mrsa"),
                       gram_positive, label = paste(name, slot))
    }
  }
})

test_that("get_benchmark_data places the summary rows the reference lacks that share a place in the classification's order, whatever order the datasets are joined in", {
  # The reference: Escherichia coli (1807) in two departments. Then three
  # departments: Acinetobacter baumannii (1660), which adds the aerobic row;
  # Clostridium tetani (42), which adds the Gram-variable row and the row of
  # the oxygen requirement not determined; and Bacteroides fragilis (1296),
  # which adds the obligately anaerobic row.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = 1807L),
      department_key = 1:2),
    use_cache = FALSE)
  department <- function(pathogen_key)
    calculate_department_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1L, pathogen_key = pathogen_key)),
      use_cache = FALSE)
  departments <- list(baumannii = department(1660L), tetani = department(42L),
                      fragilis = department(1296L))

  # The Gram-variable row closes the run of the Gram reaction, after the
  # reference's Gram-negative row, and the aerobic row opens that of the
  # oxygen requirement, before the reference's facultatively anaerobic row;
  # the obligately anaerobic row precedes the row of the oxygen requirement
  # not determined, both after the reference's facultatively anaerobic row,
  # whether the departments are joined with B. fragilis after C. tetani or
  # before it.
  opening <- c(NA, "gram_reaction|negative", "gram_reaction|variable",
               "oxygen_requirement|aerobic",
               "oxygen_requirement|facultative-anaerobic",
               "oxygen_requirement|obligate-anaerobic",
               "oxygen_requirement|not-determined")
  for (joined in list(c("baumannii", "tetani", "fragilis"),
                      c("baumannii", "fragilis", "tetani"))) {
    benchmark <- do.call(get_benchmark_data,
                         c(list(ref = ref), departments[joined]))
    expect_equal(nrow(benchmark$not_comparable), 0L)
    detected <- benchmark$infectious_agent_detection_rate_per_agent_table
    label <- paste(joined, collapse = ", ")
    expect_identical(detected$node_id[seq_along(opening)], opening,
                     label = label)
    expect_false(
      any(detected$node_id[-seq_along(opening)] %in% bundled_summary_ids()),
      label = label)
  }
})

test_that("get_benchmark_data keeps the order of a reference benchmarked alone whose summary row has a value the bundle lacks", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own, as a later release of the list that
  # keys the Gram-negative value `gram-negative` would lay it out.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  for (slot in groups_tables()) {
    renamed <- ref[[slot]]$node_id %in% "gram_reaction|negative"
    expect_true(any(renamed), label = slot)
    ref[[slot]]$node_id[renamed] <- "gram_reaction|gram-negative"
  }
  benchmark <- get_benchmark_data(ref = ref)

  key <- function(t)
    if ("abr" %in% names(t)) paste(t$abr, t$node_id) else paste(t$node_id)
  for (slot in groups_tables())
    expect_identical(key(benchmark[[slot]]), key(ref[[slot]]), label = slot)
})

test_that("get_benchmark_data places a summary row whose value the bundle lacks, which only a department holds, last in its attribute's run", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own. The department: Clostridium tetani (42)
  # and Ureaplasma (158), which add the rows Gram-variable, not classified by
  # the Gram stain, and of the oxygen requirement not determined; both
  # calculated under a release of the list that keys the Gram-variable value
  # `indeterminate`, which the bundle lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(42L, 158L))),
    use_cache = FALSE)
  slot <- "infectious_agent_detection_rate_per_agent_table"
  renamed <- own[[slot]]$node_id %in% "gram_reaction|variable"
  expect_equal(sum(renamed), 1L)
  own[[slot]]$node_id[renamed] <- "gram_reaction|indeterminate"
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # The run of the Gram reaction: the reference's rows, the row not
  # classified by the Gram stain after them, the classification ordering both
  # values before its own, and the row whose value the bundle lacks last;
  # then that of the oxygen requirement.
  detected <- benchmark[[slot]]
  opening <- c(NA, "gram_reaction|positive", "gram_reaction|negative",
               "gram_reaction|none", "gram_reaction|indeterminate",
               "oxygen_requirement|facultative-anaerobic",
               "oxygen_requirement|not-determined")
  expect_identical(detected$node_id[seq_along(opening)], opening)
  expect_false(any(detected$node_id[-seq_along(opening)] %in%
                     c(bundled_summary_ids(), "gram_reaction|indeterminate")))
})

test_that("get_benchmark_data places a summary row the reference lacks by none of the reference's rows whose value the bundle lacks", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own, as a later release of the list that
  # keys the Gram-negative value `gram-negative` would lay it out. The
  # department: Clostridium tetani (42) and Ureaplasma (158), which add the
  # Gram-variable row, the row not classified by the Gram stain, and the row
  # of the oxygen requirement not determined.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  for (slot in groups_tables()) {
    renamed <- ref[[slot]]$node_id %in% "gram_reaction|negative"
    expect_true(any(renamed), label = slot)
    ref[[slot]]$node_id[renamed] <- "gram_reaction|gram-negative"
  }
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(42L, 158L))),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)

  # The run of the Gram reaction: the reference's Gram-positive row, the only
  # one of its rows whose value the classification orders before the
  # department's values, which follow it in the classification's order; then
  # the reference's row whose value the classification lacks, after which
  # the classification places none of them; then the run of the oxygen
  # requirement.
  detected <- benchmark$infectious_agent_detection_rate_per_agent_table
  opening <- c(NA, "gram_reaction|positive", "gram_reaction|variable",
               "gram_reaction|none", "gram_reaction|gram-negative",
               "oxygen_requirement|facultative-anaerobic",
               "oxygen_requirement|not-determined")
  expect_identical(detected$node_id[seq_along(opening)], opening)
  expect_false(any(detected$node_id[-seq_along(opening)] %in%
                     c(bundled_summary_ids(), "gram_reaction|gram-negative")))
})

test_that("get_benchmark_data keeps the summary rows the reference lacks whose values the bundle lacks, and the siblings it lacks, in the order the datasets are passed", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own. Two departments, calculated under a
  # release of the list that keys the Gram-variable value `indeterminate`
  # and the value not classified by the Gram stain `not-classified`, neither
  # of which the bundle holds: Clostridium tetani (42), which adds the
  # Gram-variable row, the row of the oxygen requirement not determined, and
  # the group of the anaerobes beneath the Bacteria; and Ureaplasma (158),
  # which adds the row not classified by the Gram stain and the group of the
  # other bacteria beneath the Bacteria.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  slot <- "infectious_agent_detection_rate_per_agent_table"
  department <- function(pathogen_key, bundled, released) {
    ds <- calculate_department_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1L, pathogen_key = pathogen_key)),
      use_cache = FALSE)
    renamed <- ds[[slot]]$node_id %in% bundled
    expect_equal(sum(renamed), 1L)
    ds[[slot]]$node_id[renamed] <- released
    ds
  }
  departments <- list(
    tetani = department(42L, "gram_reaction|variable",
                        "gram_reaction|indeterminate"),
    ureaplasma = department(158L, "gram_reaction|none",
                            "gram_reaction|not-classified"))
  released <- c(tetani = "gram_reaction|indeterminate",
                ureaplasma = "gram_reaction|not-classified")
  groups <- c(tetani = "anaerobes", ureaplasma = "other-bacteria")

  # Whichever order the departments are passed in, the run of the Gram
  # reaction holds the reference's rows, then the departments' rows whose
  # values the bundle lacks, which share the last place of the run, in that
  # order; and beneath the Bacteria, the reference's groups precede the
  # departments' groups, in that order too.
  for (passed in list(c("tetani", "ureaplasma"), c("ureaplasma", "tetani"))) {
    benchmark <- do.call(get_benchmark_data,
                         c(list(ref = ref), departments[passed]))
    expect_equal(nrow(benchmark$not_comparable), 0L)
    detected <- benchmark[[slot]]
    label <- paste(passed, collapse = ", ")
    opening <- c(NA, "gram_reaction|positive", "gram_reaction|negative",
                 unname(released[passed]),
                 "oxygen_requirement|facultative-anaerobic",
                 "oxygen_requirement|not-determined")
    expect_identical(detected$node_id[seq_along(opening)], opening,
                     label = label)
    expect_identical(detected$node_id[detected$parent_id %in% "bacteria"],
                     c("enterobacterales", "gram-positive-cocci",
                       unname(groups[passed])),
                     label = label)
  }
})

test_that("get_benchmark_data lays out the summary rows whose values neither the reference nor the bundle holds in each block in their order in the joined table, which can differ between blocks", {
  # The reference: Escherichia coli (1807) in two departments, which holds
  # the facultatively anaerobic row alone of the oxygen requirement. Two
  # departments, both calculated under a release of the list that keys the
  # obligately anaerobic value `anaerobic-x` and the value of the oxygen
  # requirement not determined `undetermined-x`, neither of which the bundle
  # holds:
  #   1. `d1`: Bacteroides vulgatus (197), obligately anaerobic, to which
  #      the list applies carbapenems alone, and Pasteurella canis (491),
  #      oxygen requirement not determined, colistin alone;
  #   2. `d2`: Streptococcus parvulus (26), Gram-positive, oxygen requirement
  #      not determined, carbapenems alone, and Prevotella tannerae (526),
  #      obligately anaerobic, colistin alone.
  # Department 1 adds one value to the carbapenem block and the other to the
  # colistin block, and department 2 adds them the other way round.
  anaerobic <- "oxygen_requirement|anaerobic-x"
  undetermined <- "oxygen_requirement|undetermined-x"
  keys <- c("oxygen_requirement|obligate-anaerobic" = anaerobic,
            "oxygen_requirement|not-determined" = undetermined)
  ref <- with_summary_keys(
    calculate_reference_data(
      make_sepsis_findings_test_ds(
        tibble::tibble(form = 1:2, pathogen_key = 1807L),
        department_key = 1:2),
      use_cache = FALSE),
    keys)
  department <- function(pathogen_key)
    with_summary_keys(
      calculate_department_data(
        make_sepsis_findings_test_ds(
          tibble::tibble(form = 1:2, pathogen_key = pathogen_key)),
        use_cache = FALSE),
      keys)
  departments <- list(d1 = department(c(197L, 491L)),
                      d2 = department(c(26L, 526L)))
  ids <- c(bundled_summary_ids(), unname(keys))
  for (slot in c("abr_infection_rate_table",
                 "infectious_agent_resistance_rate_table")) {
    expect_false(any(ref[[slot]]$node_id %in% keys), label = slot)
    expect_identical(
      block_summary_ids(departments$d1[[slot]], c("car", "cor"), ids),
      c("gram_reaction|negative", anaerobic,
        "gram_reaction|negative", undetermined),
      label = slot)
    expect_identical(
      block_summary_ids(departments$d2[[slot]], c("car", "cor"), ids),
      c("gram_reaction|positive", undetermined,
        "gram_reaction|negative", anaerobic),
      label = slot)
  }

  # In both resistance tables over the reporting groups, the run of the
  # oxygen requirement of each block ends with the two values the
  # departments add, in the order in which the department passed first adds
  # them to that block, so that the carbapenem and the colistin blocks order
  # them otherwise, whichever department is passed first.
  opening <- c("gram_reaction|negative",
               "oxygen_requirement|facultative-anaerobic")
  cases <- list(
    list(passed = c("d1", "d2"),
         car = c(anaerobic, undetermined), cor = c(undetermined, anaerobic)),
    list(passed = c("d2", "d1"),
         car = c(undetermined, anaerobic), cor = c(anaerobic, undetermined)))
  for (case in cases) {
    benchmark <- do.call(get_benchmark_data,
                         c(list(ref = ref), departments[case$passed]))
    expect_equal(nrow(benchmark$not_comparable), 0L)
    for (slot in c("abr_infection_rate_table",
                   "infectious_agent_resistance_rate_table")) {
      label <- paste(slot, paste(case$passed, collapse = ", "))
      expect_identical(block_summary_ids(benchmark[[slot]], "car", ids),
                       c("gram_reaction|positive", opening, case$car),
                       label = label)
      expect_identical(block_summary_ids(benchmark[[slot]], "cor", ids),
                       c(opening, case$cor), label = label)
    }
  }
})

test_that("get_benchmark_data places a summary row of an attribute the bundle lacks as a top-level row of the tree, where the reference places it", {
  # The reference: Escherichia coli (1807) and Staphylococcus aureus (934),
  # each in a department of its own, as a version of neoipcr whose bundle
  # has an attribute of spore formation would lay it out, its Gram-negative
  # row in each table over the reporting groups re-keyed as that attribute's
  # row `spore_formation|none`. Only such a version calculates a dataset
  # holding the attribute, whose digest of the tree over the reporting groups
  # then differs from this version's, so it is benchmarked alone.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L)),
      department_key = 1:2),
    use_cache = FALSE)
  for (slot in groups_tables()) {
    rekeyed <- ref[[slot]]$node_id %in% "gram_reaction|negative"
    expect_true(any(rekeyed), label = slot)
    ref[[slot]]$node_id[rekeyed] <- "spore_formation|none"
  }
  expect_identical(
    ref$infectious_agent_detection_rate_per_agent_table$node_id[seq_len(4L)],
    c(NA, "gram_reaction|positive", "spore_formation|none",
      "oxygen_requirement|facultative-anaerobic"))
  benchmark <- get_benchmark_data(ref = ref)

  # The summary block of the Gram reaction and the oxygen requirement; then
  # the tree, whose first top-level row is the row of spore formation, which
  # the reference places before the Bacteria.
  expect_identical(
    benchmark$infectious_agent_detection_rate_per_agent_table$node_id,
    c(NA, "gram_reaction|positive",
      "oxygen_requirement|facultative-anaerobic", "spore_formation|none",
      "bacteria", "enterobacterales", "gram-positive-cocci"))
  # The 3GCR block of E. coli in both resistance tables, the density of
  # infections with resistant infectious agents opening it with the block's
  # Total.
  gcr <- c("oxygen_requirement|facultative-anaerobic", "spore_formation|none",
           "bacteria", "enterobacterales")
  for (case in list(list(slot = "abr_infection_rate_table",
                         expected = c(NA, gcr)),
                    list(slot = "infectious_agent_resistance_rate_table",
                         expected = gcr))) {
    joined <- benchmark[[case$slot]]
    expect_identical(joined$node_id[joined$abr == "3gcr"], case$expected,
                     label = case$slot)
  }
})

test_that("get_benchmark_data gives a department without a detection any category applies to the reference's rows of the resistance rates, at 0 over 0", {
  # The reference: Escherichia coli (1807), 3GCR resistant, and
  # Staphylococcus aureus (934), MRSA. The department: Candida albicans
  # (2273), to which the list applies no category, without a result.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(1807L, 934L),
                     `3gcr` = c("yes", NA), mrsa = c(NA, "yes")),
      department_key = 1:2),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 2273L)),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)

  for (slot in neoipcr:::infectious_agent_resistance_rate_table_names) {
    # The department's table has no rows, but its columns.
    expect_equal(nrow(own[[slot]]), 0L)
    expect_gt(ncol(own[[slot]]), 0L)
    joined <- benchmark[[slot]]
    expect_identical(paste(joined$abr, joined$node_id),
                     paste(ref[[slot]]$abr, ref[[slot]]$node_id), label = slot)
    expect_gt(nrow(joined), 0L)
    zero <- rep(0L, nrow(joined))
    none <- rep(NA_real_, nrow(joined))
    expect_identical(joined$n_own, zero, label = slot)
    expect_identical(joined$total_own, zero, label = slot)
    expect_identical(joined$pooled_own, none, label = slot)
    expect_identical(joined$tested_n_own, zero, label = slot)
    expect_identical(joined$tested_total_own, zero, label = slot)
    expect_identical(joined$tested_pooled_own, none, label = slot)
  }
})

test_that("get_benchmark_data gives a row the reference lacks the number of departments that enter the reference's quartiles of the rows it shares, and none for the resistance rates", {
  # The reference: 5 departments of 100 patient-days, each with 5 sepsis with
  # Escherichia coli (1807). The department: Enterococcus faecalis (2345),
  # VRE, which the reference lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:25, pathogen_key = 1807L),
      department_key = rep(1:5, each = 5)),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 2345L, vre = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)

  # The infectious agents detected: every row shares the reference's
  # denominator of its infections with an infectious agent in the tree, which
  # its 5 departments enter.
  for (slot in neoipcr:::infectious_agent_detection_table_names) {
    source <- ref[[slot]]
    shared <- unique(source$quartile_departments[!is.na(source$node_id)])
    expect_identical(shared, 5L, label = slot)
    joined <- benchmark[[slot]]
    lacking <- !joined$node_id %in% source$node_id
    expect_gt(sum(lacking), 0L)
    expect_identical(joined$quartile_departments_ref[lacking],
                     rep(shared, sum(lacking)), label = slot)
  }
  # The density of infections with resistant infectious agents: the rows of
  # the VRE block below its Total are the department's alone, and take the
  # departments that enter the reference's VRE Total.
  for (slot in neoipcr:::abr_infection_rate_table_names) {
    source <- ref[[slot]]
    total <- source$quartile_departments[
      source$abr == "vre" & is.na(source$node_id)]
    expect_identical(total, 5L, label = slot)
    joined <- benchmark[[slot]]
    vre <- joined[joined$abr == "vre" & !is.na(joined$node_id), ]
    expect_gt(nrow(vre), 0L)
    expect_false(any(vre$node_id %in%
                       source$node_id[source$abr == "vre"]))
    expect_identical(vre$quartile_departments_ref, rep(total, nrow(vre)),
                     label = slot)
  }
  # The resistance rates: the reference has no VRE row, and none of its
  # departments has a tested infection on the department's, so none enters
  # their quartiles.
  for (slot in neoipcr:::infectious_agent_resistance_rate_table_names) {
    source <- ref[[slot]]
    expect_false(any(source$abr == "vre"), label = slot)
    joined <- benchmark[[slot]]
    vre <- joined[joined$abr == "vre", ]
    expect_gt(nrow(vre), 0L)
    expect_identical(vre$quartile_departments_ref, rep(0L, nrow(vre)),
                     label = slot)
  }
})

test_that("get_benchmark_data gives a row of the resistance rates the reference lacks no department in its quartiles where the reference's departments enter those of the rows they hold", {
  # The reference: 5 departments of 100 patient-days, each with 5 sepsis with
  # Escherichia coli (1807), each with a 3GCR result, 1 of them resistant. The
  # department: Enterococcus faecalis (2345), VRE, which the reference lacks.
  ref <- calculate_reference_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:25, pathogen_key = 1807L,
                     `3gcr` = rep(c("yes", "no", "no", "no", "no"), 5)),
      department_key = rep(1:5, each = 5)),
    use_cache = FALSE)
  own <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1L, pathogen_key = 2345L, vre = "yes")),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = own, ref = ref)

  for (slot in neoipcr:::infectious_agent_resistance_rate_table_names) {
    # Each of the reference's 5 departments enters the quartiles of its 3GCR
    # rows, with 5 infections with a result.
    source <- ref[[slot]]
    gcr <- source$quartile_departments[source$abr == "3gcr"]
    expect_gt(length(gcr), 0L)
    expect_identical(gcr, rep(5L, length(gcr)), label = slot)
    # The VRE rows are the department's alone: no infection of the reference
    # has a result for VRE in an infectious agent of their nodes, so none of
    # its departments enters their quartiles.
    expect_false(any(source$abr == "vre"), label = slot)
    joined <- benchmark[[slot]]
    vre <- joined[joined$abr == "vre", ]
    expect_gt(nrow(vre), 0L)
    expect_identical(vre$quartile_departments_ref, rep(0L, nrow(vre)),
                     label = slot)
  }

  # A department with rows the reference lacks inside the 3GCR block, whose
  # quartiles the reference's departments enter: Klebsiella pneumoniae (390),
  # 3GCR resistant, beside E. faecalis, VRE. Over the full ontology its genus
  # and species are rows the reference lacks; over the reporting groups it
  # shares E. coli's rows.
  klebsiella <- calculate_department_data(
    make_sepsis_findings_test_ds(
      tibble::tibble(form = 1:2, pathogen_key = c(2345L, 390L),
                     vre = c("yes", NA), `3gcr` = c(NA, "yes"))),
    use_cache = FALSE)
  benchmark <- get_benchmark_data(own = klebsiella, ref = ref)
  expect_equal(nrow(benchmark$not_comparable), 0L)
  # In the 3GCR block, the reference's 5 departments enter the quartiles of
  # the rows it holds and none those of the rows it lacks; no department
  # enters those of the VRE rows.
  ontology <- neoipcr:::infectious_agent_resistance_rate_table_names[[
    "ontology"]]
  for (slot in neoipcr:::infectious_agent_resistance_rate_table_names) {
    source <- ref[[slot]]
    joined <- benchmark[[slot]]
    gcr <- joined[joined$abr == "3gcr", ]
    lacking <- !gcr$node_id %in% source$node_id[source$abr == "3gcr"]
    expect_identical(sort(gcr$node_id[lacking]),
                     if (slot == ontology)
                       c("LPSN|genus/klebsiella",
                         "LPSN|species/klebsiella-pneumoniae")
                     else character(),
                     label = slot)
    expect_identical(gcr$quartile_departments_ref,
                     dplyr::if_else(lacking, 0L, 5L), label = slot)
    vre <- joined[joined$abr == "vre", ]
    expect_gt(nrow(vre), 0L)
    expect_identical(vre$quartile_departments_ref, rep(0L, nrow(vre)),
                     label = slot)
  }
})
