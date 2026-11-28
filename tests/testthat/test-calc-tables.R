# Tests for R/calc-tables.R — per-table unit tests.
# Uses make_calc_test_ds() from helper-fixtures.R.

calc_ds <- make_calc_test_ds()

# --- Figure data builders ---

test_that("get_birthweight_figure_data returns expected structure", {
  result <- neoipcr:::get_birthweight_figure_data(calc_ds)
  expect_type(result, "list")
  expect_true(all(c("density", "frequency", "location_parameters", "scale")
                  %in% names(result)))
  expect_s3_class(result$density, "tbl_df")
  expect_s3_class(result$frequency, "tbl_df")
})

test_that("get_gestational_age_figure_data returns expected structure", {
  result <- neoipcr:::get_gestational_age_figure_data(calc_ds)
  expect_type(result, "list")
  expect_true(all(c("density", "frequency", "location_parameters", "scale")
                  %in% names(result)))
})

# --- Rate tables: each must return a non-empty tibble ---

table_fns <- list(
  list(name = "get_usage_density_rate_table",
       fn = get_usage_density_rate_table, has_q = TRUE),
  list(name = "get_antibiotic_utilization_table",
       fn = get_antibiotic_utilization_table, has_q = TRUE),
  list(name = "get_surgery_rate_table",
       fn = get_surgery_rate_table, has_q = FALSE),
  list(name = "get_incidence_density_rate_table",
       fn = get_incidence_density_rate_table, has_q = TRUE),
  list(name = "get_dev_ass_incidence_density_rate_table",
       fn = get_dev_ass_incidence_density_rate_table, has_q = TRUE),
  list(name = "get_infectious_agent_detection_rate_per_inf_type_table",
       fn = get_infectious_agent_detection_rate_per_inf_type_table, has_q = TRUE),
  list(name = "get_infectious_agent_detection_rate_per_agent_table",
       fn = get_infectious_agent_detection_rate_per_agent_table, has_q = TRUE),
  list(name = "get_infectious_agent_detection_rate_per_agent_table over the full ontology",
       fn = \(x, ...) get_infectious_agent_detection_rate_per_agent_table(
         x, ..., rows = "ontology"),
       has_q = TRUE),
  list(name = "get_abr_infection_rate_table",
       fn = get_abr_infection_rate_table, has_q = TRUE),
  list(name = "get_abr_infection_rate_table over the full ontology",
       fn = \(x, ...) get_abr_infection_rate_table(x, ..., rows = "ontology"),
       has_q = TRUE),
  list(name = "get_infectious_agent_resistance_rate_table",
       fn = get_infectious_agent_resistance_rate_table, has_q = TRUE),
  list(name = "get_infectious_agent_resistance_rate_table over the full ontology",
       fn = \(x, ...) get_infectious_agent_resistance_rate_table(
         x, ..., rows = "ontology"),
       has_q = TRUE),
  list(name = "get_resistance_test_rate_table",
       fn = get_resistance_test_rate_table, has_q = TRUE),
  list(name = "get_secondary_bsi_rate_table",
       fn = get_secondary_bsi_rate_table, has_q = TRUE)
)

for (entry in table_fns) {
  local({
    nm <- entry$name
    fn <- entry$fn
    hq <- entry$has_q

    test_that(paste0(nm, " returns a tibble"), {
      if (hq)
        result <- fn(calc_ds, use_cache = FALSE, include_quartiles = FALSE)
      else
        result <- fn(calc_ds, use_cache = FALSE)
      expect_s3_class(result, "tbl_df")
      expect_true(ncol(result) > 0L)
    })
  })
}

test_that("the infectious-agent table builders refuse pseudonymized events, and the others take them", {
  # The findings carry `pathogen_key` and `secondary_bsi` only under the full
  # event tier; the fixture's tibbles are full, so only the precondition can
  # refuse the dataset.
  ds <- calc_ds
  ds$metadata$dataset_options$include_event <- "pseudo"
  needs_full_events <- c(
    "get_infectious_agent_detection_rate_per_inf_type_table",
    "get_infectious_agent_detection_rate_per_agent_table",
    "get_infectious_agent_detection_rate_per_agent_table over the full ontology",
    "get_abr_infection_rate_table",
    "get_abr_infection_rate_table over the full ontology",
    "get_infectious_agent_resistance_rate_table",
    "get_infectious_agent_resistance_rate_table over the full ontology",
    "get_resistance_test_rate_table",
    "get_secondary_bsi_rate_table")
  for (entry in table_fns) {
    build <- if (entry$has_q)
      \() entry$fn(ds, use_cache = FALSE, include_quartiles = FALSE)
    else
      \() entry$fn(ds, use_cache = FALSE)
    if (entry$name %in% needs_full_events) {
      cnd <- expect_error(build(), class = "neoipcr_unmet_import_options")
      expect_match(conditionMessage(cnd), "`pathogen_key`", fixed = TRUE)
    } else {
      expect_no_error(build(), class = "neoipcr_unmet_import_options")
    }
  }
})

test_that("a table builder refuses a dataset stored before the hierarchy pair", {
  # The shared chokepoint of `check_ds_and_try_get_table()` and the two
  # builders that compute outside it each carry the precondition and
  # name the table; every cache lookup sits ahead of it, so a table
  # already computed for the dataset, even one stored before the pair,
  # is still served from the cache.
  cnd <- expect_error(
    get_usage_density_rate_table(make_stale_import_ds(),
                                 use_cache = FALSE,
                                 include_quartiles = FALSE),
    class = "neoipcr_missing_hierarchy_pair")
  # The shared branch names the table by its slot, as the refusal of
  # the options already does.
  expect_match(conditionMessage(cnd),
               "usage_density_rate_table() needs a dataset imported by",
               fixed = TRUE)
  cnd <- expect_error(
    get_surgery_rate_table(make_stale_import_ds(), use_cache = FALSE),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "get_surgery_rate_table() needs a dataset imported by",
               fixed = TRUE)
  cnd2 <- expect_error(
    get_ref_surgery_rate_table(make_stale_import_ds(), use_cache = FALSE),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd2),
               "get_ref_surgery_rate_table() needs a dataset imported by",
               fixed = TRUE)
})

test_that("a table builder still serves a cached table from a dataset stored before the hierarchy pair", {
  # The refusal guards recomputation, not the stored result: the stale
  # copy shares its source's cache, so the table computed once for the
  # dataset is served from it untouched.
  ds <- make_calc_test_ds()
  computed <- get_usage_density_rate_table(ds, use_cache = TRUE,
                                           include_quartiles = FALSE)
  expect_identical(
    get_usage_density_rate_table(make_stale_import_ds(ds), use_cache = TRUE,
                                 include_quartiles = FALSE),
    computed)
})

# --- Numerical spot-check: usage density CVC ---

test_that("usage_density_rate_table CVC days match fixture", {
  result <- get_usage_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  cvc_row <- result[result$factor == "cvc", ]
  # 3 enrollments × 3 cvc_days each = 9
  expect_equal(cvc_row$n, 9L)
})

test_that("usage_density_rate_table has 12 factor rows", {
  result <- get_usage_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_equal(nrow(result), 12L)
})

# --- Numerical spot-checks per table ---

# Fixture: 3 enrollments, patient_days = 15+16+16 = 47
# Surveillance end data: cvc=3*3=9, pvc=3*2=6, ab=3*5=15
# Events: 2 BSI (events 3,9), 1 surgery (event 6)
# SepsisData: dev_ass = factor("1","0") → 1 CVC-associated BSI
# IAF: 2 entries, all resistance = "no"
# SubstanceDays: 3 entries × 3 days = 9 days of J01CA04

test_that("usage_density_rate_table PVC days match fixture", {
  result <- get_usage_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  pvc_row <- result[result$factor == "pvc", ]
  # 3 enrollments × 2 pvc_days each = 6

  expect_equal(pvc_row$n, 6L)
  # rate = 6 / 47 * 100
  expect_equal(pvc_row$pooled, 6 / 47 * 100, tolerance = 0.01)
})

test_that("antibiotic_utilization_table substance days match fixture", {
  result <- get_antibiotic_utilization_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 2 rows: 1 atc5-level + 1 substance-level for J01CA04
  expect_equal(nrow(result), 2L)
  j01_row <- result[result$row_type == "substance", ]
  expect_equal(j01_row$row_id, "J01CA04")
  # 3 enrollments × 3 days = 9 total substance-days
  expect_equal(j01_row$n, 9L)
  # rate = 9 / 47 * 100
  expect_equal(j01_row$pooled, 9 / 47 * 100, tolerance = 0.01)
})

test_that("surgery_rate_table counts match fixture", {
  result <- get_surgery_rate_table(calc_ds, use_cache = FALSE)
  # 2 rows: overall + inguinal_umbilical_hernia_repair (fixture default code)
  expect_equal(nrow(result), 2L)
  overall <- result[result$pro_cat == "overall", ]
  expect_equal(overall$n, 1L)
  # rate = 1 / 3 patients * 100
  expect_equal(overall$pooled, 1 / 3 * 100, tolerance = 0.01)
})

test_that("a procedure held by a pending code stays out of the surgical counts", {
  # The classifier places a procedure holding a pending code that names no
  # operation nowhere, and the counts hold it out too: the table counts the
  # procedures the classifier places.
  held <- calc_ds
  held$surgeryData$side_procedure_code_1 <- "NMR.MK.AB"
  expect_equal(get_procedures(held, use_cache = FALSE)$n, 0L)
  expect_equal(get_surgery_risk(held, use_cache = FALSE)$n_procedures, 0L)
  # With its codes alone the procedure counts, so it is the held code that
  # leaves it out.
  expect_equal(get_procedures(calc_ds, use_cache = FALSE)$n, 1L)
  expect_equal(get_surgery_risk(calc_ds, use_cache = FALSE)$n_procedures, 1L)
})

test_that("a procedure no group places stays out of the surgical counts", {
  # A procedure only of codes no group takes asks what was done (rule 68);
  # until its codes are placed, neither counting path counts it as a
  # surgical procedure.
  ungrouped <- calc_ds
  ungrouped$surgeryData$main_procedure_code <- "PZX.AA.JA"
  expect_equal(get_procedures(ungrouped, use_cache = FALSE)$n, 0L)
  expect_equal(get_surgery_risk(ungrouped, use_cache = FALSE)$n_procedures,
               0L)
  # The fixture's own code places, so it is the ungrouped code that leaves
  # the procedure out.
  expect_equal(get_procedures(calc_ds, use_cache = FALSE)$n, 1L)
  expect_equal(get_surgery_risk(calc_ds, use_cache = FALSE)$n_procedures, 1L)
})

test_that("a procedure its side code places counts in both paths", {
  # Both counts judge the procedure over all three of its codes, as the
  # classifier does: a main code no group takes beside a placing side
  # code is a placed procedure and counts in each path alike.
  sided <- calc_ds
  sided$surgeryData$main_procedure_code <- "ZZZ.ZZ.ZZ"
  sided$surgeryData$side_procedure_code_1 <- "PAM.MK.AA"
  expect_equal(get_procedures(sided, use_cache = FALSE)$n, 1L)
  expect_equal(get_surgery_risk(sided, use_cache = FALSE)$n_procedures, 1L)
  # And the group it counts in is the classifier's over all its codes, not
  # the main code's own: the procedure joins the hernia repair's row.
  by_cat <- get_procedures(sided, group_cols = "pro_cat", use_cache = FALSE)
  expect_identical(as.character(by_cat$pro_cat),
                   "inguinal_umbilical_hernia_repair")
  table <- get_surgery_rate_table(sided, use_cache = FALSE)
  expect_true("inguinal_umbilical_hernia_repair" %in%
                as.character(table$pro_cat))
  expect_false("no_group" %in% as.character(table$pro_cat))
})

test_that("surgical cache entries carry the definitions version", {
  ds <- make_calc_test_ds()
  invisible(get_procedures(ds, use_cache = TRUE))
  invisible(get_procedure_categories(ds, use_cache = TRUE))
  invisible(get_surgery_risk(ds, use_cache = TRUE))
  invisible(get_surgery_rate_table(ds, use_cache = TRUE))
  v <- paste0("[.]v",
              as.character(neoipcr:::.surgical_definitions_version()), "$")
  # The tag carries each of the four surgical definitions, so one of them
  # rising alone retires the entries too.
  expect_equal(
    length(strsplit(as.character(
      neoipcr:::.surgical_definitions_version()), ".", fixed = TRUE)[[1]]),
    4L)
  keys <- ls(ds$.cache)
  surgical <- keys[grepl(
    "^(procedures|procedure_categories|surgery_risk|surgery_rate_table)",
    keys)]
  expect_true(length(surgical) >= 4L)
  expect_true(all(grepl(v, surgical)))
  # An entry restored with its dataset under the old untagged key answers
  # nothing after the bump: the counts are recomputed, not mislabelled.
  stale <- tibble::tibble(n = 999L)
  ds$.cache$procedures <- stale
  expect_false(identical(get_procedures(ds, use_cache = TRUE), stale))
})

test_that("the surgical counts take a surgery form without the side codes", {
  # A dataset taken from a form version that carries no side codes counts
  # by the codes it does carry, standing in missing values row by row.
  ds <- calc_ds
  ds$surgeryData$side_procedure_code_1 <- NULL
  ds$surgeryData$side_procedure_code_2 <- NULL
  expect_equal(get_procedures(ds, use_cache = FALSE)$n, 1L)
  expect_equal(get_surgery_risk(ds, use_cache = FALSE)$n_procedures, 1L)
})

test_that("incidence_density_rate_table BSI count matches fixture", {
  result <- get_incidence_density_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 4 rows: si, bsi, hap, nec
  expect_equal(nrow(result), 4L)
  bsi_row <- result[result$inf == "bsi", ]
  expect_equal(bsi_row$n, 2L)
  # rate = 2 / 47 * 1000
  expect_equal(bsi_row$pooled, 2 / 47 * 1000, tolerance = 0.01)
  # si = bsi + hap = 2 + 0 = 2
  si_row <- result[result$inf == "si", ]
  expect_equal(si_row$n, 2L)
})

test_that("dev_ass_incidence_density_rate_table CVC event matches fixture", {
  result <- get_dev_ass_incidence_density_rate_table(calc_ds,
    use_cache = FALSE, include_quartiles = FALSE)
  # 5 devices: cvc, pvc, vs, inv, niv
  expect_equal(nrow(result), 5L)
  cvc_row <- result[result$dev == "cvc", ]
  # 1 CVC-associated BSI (dev_ass = "1" on event 3)
  expect_equal(cvc_row$n, 1L)
  # rate = 1 / 9 cvc_days * 1000
  expect_equal(cvc_row$pooled, 1 / 9 * 1000, tolerance = 0.01)
})

test_that("infectious_agent_detection_per_inf_type all BSI have pathogens", {
  result <- get_infectious_agent_detection_rate_per_inf_type_table(
    calc_ds, use_cache = FALSE, include_quartiles = FALSE)
  # 3 rows: bsi, hap, ssi; none for all types together.
  expect_equal(as.character(result$inf), c("bsi", "hap", "ssi"))
  # Both episodes name an infectious agent, over every episode.
  bsi_row <- result[result$inf == "bsi", ]
  expect_equal(bsi_row$n, 2L)
  expect_equal(bsi_row$tested, 2L)
  expect_equal(bsi_row$pooled, 100)
})

test_that("infectious_agent_detection_per_agent has Total row", {
  result <- get_infectious_agent_detection_rate_per_agent_table(
    calc_ds, use_cache = FALSE, include_quartiles = FALSE)
  expect_equal(result$group[1], "Total")
  expect_equal(result$n[1], 2L)
  expect_equal(result$total[1], 2L)
  expect_equal(result$pooled[1], 100)
})

# calc_ds's two sepsis name Azorhizobium caulinodans (1), a Gram-negative
# aerobic non-fermenting bacillus to which the list applies no resistance
# category, and the genus Enterococcus (503), Gram-positive and
# facultatively anaerobic, to which it applies vancomycin resistance alone;
# each slot records every category as not resistant. A value recorded for a
# category that cannot apply still opens the rows above its infectious agent
# that lie on the paths of those the list applies the category to.

# The rows of each category's block over the reporting groups, by its Total.
calc_ds_block_rows <- tibble::tribble(
  ~abr,   ~group,                                         ~level,
  "3gcr", "Total",                                            0L,
  "3gcr", "Gram-negative",                                    1L,
  "3gcr", "Aerobic",                                          1L,
  "3gcr", "Facultatively anaerobic",                          1L,
  "3gcr", "Bacteria",                                         1L,
  "3gcr", "Gram-negative bacilli, not Enterobacterales",      2L,
  "3gcr", "Non-fermenting Gram-negative bacilli",             3L,
  "car",  "Total",                                            0L,
  "car",  "Gram-positive",                                    1L,
  "car",  "Gram-negative",                                    1L,
  "car",  "Aerobic",                                          1L,
  "car",  "Facultatively anaerobic",                          1L,
  "car",  "Bacteria",                                         1L,
  "car",  "Gram-negative bacilli, not Enterobacterales",      2L,
  "car",  "Non-fermenting Gram-negative bacilli",             3L,
  "cor",  "Total",                                            0L,
  "cor",  "Gram-negative",                                    1L,
  "cor",  "Aerobic",                                          1L,
  "cor",  "Facultatively anaerobic",                          1L,
  "cor",  "Bacteria",                                         1L,
  "cor",  "Gram-negative bacilli, not Enterobacterales",      2L,
  "cor",  "Non-fermenting Gram-negative bacilli",             3L,
  "mrsa", "Total",                                            0L,
  "mrsa", "Gram-positive",                                    1L,
  "mrsa", "Facultatively anaerobic",                          1L,
  "mrsa", "Bacteria",                                         1L,
  "mrsa", "Gram-positive cocci",                              2L,
  "vre",  "Total",                                            0L,
  "vre",  "Gram-positive",                                    1L,
  "vre",  "Facultatively anaerobic",                          1L,
  "vre",  "Bacteria",                                         1L,
  "vre",  "Gram-positive cocci",                              2L)

test_that("abr_infection_rate_table has a Total per category, and rows only on the paths of the infectious agents it applies to, all zero", {
  result <- get_abr_infection_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # The genus Enterococcus gives the carbapenem block its Gram-positive row,
  # since the list applies carbapenem resistance to some Gram-positive
  # bacteria, but no row of the Gram-positive cocci, to none of which it
  # applies it.
  expect_equal(paste(result$abr, result$group),
               paste(calc_ds_block_rows$abr, calc_ds_block_rows$group))
  expect_equal(result$level, calc_ds_block_rows$level)
  expect_true(all(result$n == 0L))
  # Over all 47 patient-days.
  expect_true(all(result$total == 47L))
  expect_true(all(result$pooled == 0))

  result <- get_abr_infection_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE, rows = "ontology")
  # The genus Enterococcus, which no other infectious agent of the dataset
  # lies beneath, has a residual row in the block of vancomycin-resistant
  # Enterococcus (VRE).
  expect_equal(
    paste(result$abr, result$group),
    c("3gcr Total", "3gcr Bacteria", "3gcr Pseudomonadati",
      "3gcr Pseudomonadota",
      "car Total", "car Bacteria", "car Bacillati", "car Bacillota",
      "car Pseudomonadati", "car Pseudomonadota",
      "cor Total", "cor Bacteria", "cor Pseudomonadati", "cor Pseudomonadota",
      "mrsa Total", "mrsa Bacteria", "mrsa Bacillati", "mrsa Bacillota",
      "mrsa Bacilli",
      "vre Total", "vre Bacteria", "vre Bacillati", "vre Bacillota",
      "vre Bacilli", "vre Lactobacillales", "vre Enterococcaceae",
      "vre Enterococcus", "vre Enterococcus n.o.s."))
  expect_true(all(result$n == 0L))
})

test_that("the resistance rates per infectious agent have rows only on the paths of the infectious agents a category applies to, all zero", {
  result <- get_infectious_agent_resistance_rate_table(calc_ds,
    use_cache = FALSE, include_quartiles = FALSE)
  expect_s3_class(result, "neoipcr_tbl_iarr")
  rows <- calc_ds_block_rows[calc_ds_block_rows$group != "Total", ]
  expect_equal(paste(result$abr, result$group), paste(rows$abr, rows$group))
  expect_true(all(result$n == 0L))
  # The Bacteria rows count both infections' results, recorded for a
  # category the list does not apply to their infectious agent but for
  # vancomycin resistance and the genus Enterococcus, while the Tested share
  # takes the infectious agents the category applies to alone.
  expect_equal(result$total, ifelse(rows$group == "Bacteria", 2L, 1L))
  expect_true(all(result$pooled == 0))
  vre <- rows$abr == "vre"
  expect_equal(result$tested_n, ifelse(vre, 1L, 0L))
  expect_equal(result$tested_total, ifelse(vre, 1L, 0L))
  expect_equal(result$tested_pooled, ifelse(vre, 100, NA_real_))
})

test_that("resistance_test_rate_table has 7 rows, each over the detections the list applies its category to", {
  result <- get_resistance_test_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_equal(
    names(result),
    c("abr", "cond", "n", "total", "pooled", "ci_lower", "ci_upper"))
  expect_equal(
    paste(result$abr, result$cond),
    c("3gcr routine", "car routine", "car if_3gcr", "cor routine",
      "cor if_3gcr&car", "mrsa routine", "vre routine"))
  # Only vancomycin resistance applies, to the genus Enterococcus, whose
  # result is recorded; the values recorded for Azorhizobium count nowhere.
  expect_equal(result$n, c(0L, 0L, 0L, 0L, 0L, 0L, 1L))
  expect_equal(result$total, c(0L, 0L, 0L, 0L, 0L, 0L, 1L))
  expect_equal(result$pooled, c(NA, NA, NA, NA, NA, NA, 100))
})

test_that("secondary_bsi_rate_table has 3 types, all zero", {
  result <- get_secondary_bsi_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # nec, hap, ssi — no secondary BSI data in fixture
  expect_equal(nrow(result), 3L)
  expect_true(all(result$n == 0L))
  expect_true(all(is.nan(result$pooled)))
})

# --- Reference surgery rate table ---

test_that("get_ref_surgery_rate_table returns tibble with quartile columns", {
  result <- get_ref_surgery_rate_table(calc_ds, use_cache = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("pro_cat", "n", "pooled", "q1", "q2", "q3") %in%
    names(result)))
  # Same base data as surgery_rate_table
  expect_equal(nrow(result), 2L)
  overall <- result[result$pro_cat == "overall", ]
  expect_equal(overall$n, 1L)
  # Only 2 departments (< 5) → quartiles dropped to NA
  expect_true(is.na(overall$q1))
  expect_true(is.na(overall$q2))
  expect_true(is.na(overall$q3))
})

# --- Figure data numerical checks ---

test_that("get_birthweight_figure_data location parameters match fixture", {
  result <- neoipcr:::get_birthweight_figure_data(calc_ds)
  # Patients: birth_weight = c(800, 1200, 2500)
  lp <- result$location_parameters
  expect_equal(lp$mean, as.integer(mean(c(800L, 1200L, 2500L))))
  q <- quantile(c(800L, 1200L, 2500L), names = FALSE)
  expect_equal(lp$q1, as.integer(q[2]))
  expect_equal(lp$q2, as.integer(q[3]))
  expect_equal(lp$q3, as.integer(q[4]))
})

test_that("get_gestational_age_figure_data location parameters match fixture", {
  result <- neoipcr:::get_gestational_age_figure_data(calc_ds)
  # Patients: total_gestation_days = c(175, 210, 252)
  lp <- result$location_parameters
  expect_equal(lp$mean, as.integer(mean(c(175L, 210L, 252L))))
  q <- quantile(c(175L, 210L, 252L), names = FALSE)
  expect_equal(lp$q1, as.integer(q[2]))
  expect_equal(lp$q2, as.integer(q[3]))
  expect_equal(lp$q3, as.integer(q[4]))
})

test_that("figure builders drop NA optional attributes", {
  # birth_weight and gestational age are optional TEAs (mandatory = false);
  # a patient can be missing either one. The distribution figures cover only
  # recorded values, so an NA must be excluded, not crash the quantile.
  ds <- make_calc_test_ds()
  ds$patients$birth_weight[1]         <- NA_integer_
  ds$patients$total_gestation_days[2] <- NA_integer_

  present_bw <- ds$patients$birth_weight[!is.na(ds$patients$birth_weight)]
  bw <- neoipcr:::get_birthweight_figure_data(ds)$location_parameters
  expect_equal(bw$mean, as.integer(mean(present_bw)))
  expect_equal(bw$q2, as.integer(quantile(present_bw, names = FALSE)[3]))

  present_ga <- ds$patients$total_gestation_days[
    !is.na(ds$patients$total_gestation_days)]
  ga <- neoipcr:::get_gestational_age_figure_data(ds)$location_parameters
  expect_equal(ga$mean, as.integer(mean(present_ga)))
  expect_equal(ga$q2, as.integer(quantile(present_ga, names = FALSE)[3]))
})

# --- Empty-data resilience ---

empty_ds <- make_empty_calc_test_ds()

# Without quartiles

for (entry in table_fns) {
  local({
    nm <- entry$name
    fn <- entry$fn
    hq <- entry$has_q

    test_that(paste0(nm, " survives empty data (no quartiles)"), {
      if (hq)
        result <- fn(empty_ds, use_cache = FALSE, include_quartiles = FALSE)
      else
        result <- fn(empty_ds, use_cache = FALSE)
      expect_s3_class(result, "tbl_df")
      expect_true("ci_lower" %in% names(result))
      expect_true("ci_upper" %in% names(result))
    })
  })
}

# With quartiles

for (entry in table_fns) {
  local({
    nm <- entry$name
    fn <- entry$fn
    hq <- entry$has_q

    if (!hq) return()

    test_that(paste0(nm, " survives empty data (with quartiles)"), {
      result <- fn(empty_ds, use_cache = FALSE, include_quartiles = TRUE)
      expect_s3_class(result, "tbl_df")
      expect_true("q1" %in% names(result))
    })
  })
}

# Ref surgery (always with quartiles)

test_that("get_ref_surgery_rate_table survives empty data", {
  result <- get_ref_surgery_rate_table(empty_ds, use_cache = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_true("q1" %in% names(result))
})

# Figure data

test_that("get_birthweight_figure_data survives empty data", {
  result <- neoipcr:::get_birthweight_figure_data(empty_ds)
  expect_type(result, "list")
  expect_named(result, c("density", "frequency", "location_parameters", "scale"))
})

test_that("get_gestational_age_figure_data survives empty data", {
  result <- neoipcr:::get_gestational_age_figure_data(empty_ds)
  expect_type(result, "list")
  expect_named(result, c("density", "frequency", "location_parameters", "scale"))
})

# Orchestrators

test_that("calculate_department_data survives empty data", {
  result <- calculate_department_data(empty_ds, use_cache = FALSE)
  expect_s3_class(result, "neoipcr_rep_ds")
})

test_that("calculate_reference_data survives empty data", {
  result <- calculate_reference_data(empty_ds, use_cache = FALSE)
  expect_s3_class(result, "neoipcr_ref_ds")
})

test_that("a table builder refuses a calculated dataset that lacks its table", {
  # NEOIPC-PERMANENT(dataset-format): see check_ds_and_try_get_table(). A
  # calculated dataset written before a table existed has no slot for it, and
  # a calculated dataset holds nothing the table could be computed from.
  ds <- make_calc_test_ds()
  calculated <- calculate_department_data(ds, use_cache = FALSE)
  calculated$infectious_agent_detection_rate_per_agent_ontology_table <- NULL
  cnd <- expect_error(
    get_infectious_agent_detection_rate_per_agent_table(
      calculated, include_quartiles = FALSE, rows = "ontology"),
    class = "neoipcr_table_not_in_dataset")
  expect_match(conditionMessage(cnd),
               "infectious_agent_detection_rate_per_agent_ontology_table",
               fixed = TRUE)

  reference <- calculate_reference_data(ds, use_cache = FALSE)
  reference$abr_infection_rate_ontology_table <- NULL
  for (include_quartiles in c(TRUE, FALSE))
    expect_error(
      get_abr_infection_rate_table(
        reference, include_quartiles = include_quartiles, rows = "ontology"),
      class = "neoipcr_table_not_in_dataset")
})

# --- get_cumulative_incidence_table ---
#
# calc_ds: department 1 admits patients 1 (2024-01-01, BSI on 2024-01-08) and
# 2 (2024-01-05, no infection); department 2 admits patient 3 (2024-01-10,
# BSI on 2024-01-18).

january_window <- function(department_key)
  tibble::tibble(
    department_key = department_key,
    window = "jan",
    start  = as.Date("2024-01-01"),
    end    = as.Date("2024-01-31"))

test_that("get_cumulative_incidence_table counts admissions and infections inside each window", {
  result <- get_cumulative_incidence_table(calc_ds, january_window(c(1L, 2L)))

  expect_s3_class(result, "neoipcr_tbl_cuminc")
  expect_named(result, c(
    "department_key", "window", "start", "end",
    "n_patients", "n_enrollments", "n_infected_patients", "n_infected_enrollments",
    "n", "n_infected", "proportion", "ci_lower", "ci_upper"))

  d1 <- result[result$department_key == 1L, ]
  expect_equal(d1$n_patients, 2L)
  expect_equal(d1$n_enrollments, 2L)
  expect_equal(d1$n_infected_patients, 1L)
  expect_equal(d1$n, 2L)
  expect_equal(d1$n_infected, 1L)
  expect_equal(d1$proportion, 50)
  ci <- neoipc_wilson_ci(1, 2)
  expect_equal(d1$ci_lower, ci$lower * 100)
  expect_equal(d1$ci_upper, ci$upper * 100)

  d2 <- result[result$department_key == 2L, ]
  expect_equal(d2$n, 1L)
  expect_equal(d2$n_infected, 1L)
  expect_equal(d2$proportion, 100)
})

test_that("get_cumulative_incidence_table refuses a dataset stored before the hierarchy pair", {
  # This entry checks the options itself rather than through the shared
  # raw-dataset branch, so it needs the pair precondition in its own
  # assertion list.
  cnd <- expect_error(
    get_cumulative_incidence_table(make_stale_import_ds(),
                                   january_window(c(1L, 2L))),
    class = "neoipcr_missing_hierarchy_pair")
  expect_match(conditionMessage(cnd),
               "get_cumulative_incidence_table() needs a dataset imported by",
               fixed = TRUE)
})

test_that("get_cumulative_incidence_table keeps a window with no admissions, with zero counts and NA interval", {
  windows <- tibble::tibble(
    department_key = 1L, window = "feb",
    start = as.Date("2024-02-01"), end = as.Date("2024-02-29"))
  result <- get_cumulative_incidence_table(calc_ds, windows)

  expect_equal(nrow(result), 1L)
  expect_equal(result$n, 0L)
  expect_equal(result$n_infected, 0L)
  expect_true(is.na(result$proportion))
  expect_true(is.na(result$ci_lower))
  expect_true(is.na(result$ci_upper))
})

test_that("get_cumulative_incidence_table ignores an infection that falls outside the window", {
  # Both department-1 admissions fall in the window, but the BSI on
  # 2024-01-08 is after its end.
  windows <- tibble::tibble(
    department_key = 1L, window = "early",
    start = as.Date("2024-01-01"), end = as.Date("2024-01-05"))
  result <- get_cumulative_incidence_table(calc_ds, windows)

  expect_equal(result$n_patients, 2L)
  expect_equal(result$n_infected_patients, 0L)
  expect_equal(result$proportion, 0)
})

test_that("get_cumulative_incidence_table counts a patient admitted twice in one window once", {
  ds <- make_calc_test_ds()
  ds$enrollments <- make_test_enrollments(2,
    patient_keys   = c(1L, 1L),
    department_key = c(1L, 1L),
    enrolledAt     = as.Date(c("2024-01-01", "2024-01-20")))
  ds$events <- make_test_events(
    n               = 4,
    enrollment_keys = c(1L, 1L, 2L, 2L),
    patient_keys    = c(1L, 1L, 1L, 1L),
    event_type_keys = c("adm", "bsi", "adm", "bsi"),
    occurredAt      = as.Date(c("2024-01-01", "2024-01-08",
                                "2024-01-20", "2024-01-25")),
    department_key  = c(1L, 1L, 1L, 1L))

  by_patient <- get_cumulative_incidence_table(ds, january_window(1L))
  expect_equal(by_patient$n_patients, 1L)
  expect_equal(by_patient$n_enrollments, 2L)
  expect_equal(by_patient$n_infected_patients, 1L)
  expect_equal(by_patient$n_infected_enrollments, 2L)
  expect_equal(by_patient$n, 1L)
  expect_equal(by_patient$proportion, 100)

  by_enrollment <- get_cumulative_incidence_table(
    ds, january_window(1L), unit = "enrollments")
  expect_equal(by_enrollment$n, 2L)
  expect_equal(by_enrollment$n_infected, 2L)
})

test_that("get_cumulative_incidence_table counts infections only on the window department's own admissions", {
  # Department 2's window must not pick up the department-1 BSI.
  result <- get_cumulative_incidence_table(calc_ds, january_window(2L))
  expect_equal(result$n_infected, 1L)
  expect_equal(result$n, 1L)
})

test_that("get_cumulative_incidence_table ties an infection to the admission it belongs to, not to the patient", {
  # Patient 1 is admitted to department 1 inside the window and later to
  # department 2; the only BSI sits on the department-2 admission. A join
  # through the patient would count it for department 1.
  ds <- make_calc_test_ds()
  ds$enrollments <- make_test_enrollments(2,
    patient_keys   = c(1L, 1L),
    department_key = c(1L, 2L),
    enrolledAt     = as.Date(c("2024-01-01", "2024-01-10")))
  ds$events <- make_test_events(
    n               = 3,
    enrollment_keys = c(1L, 2L, 2L),
    patient_keys    = c(1L, 1L, 1L),
    event_type_keys = c("adm", "adm", "bsi"),
    occurredAt      = as.Date(c("2024-01-01", "2024-01-10", "2024-01-15")),
    department_key  = c(1L, 2L, 2L))

  d1 <- get_cumulative_incidence_table(ds, january_window(1L))
  expect_equal(d1$n_patients, 1L)
  expect_equal(d1$n_infected_patients, 0L)

  d2 <- get_cumulative_incidence_table(ds, january_window(2L))
  expect_equal(d2$n_patients, 1L)
  expect_equal(d2$n_infected_patients, 1L)
})

test_that("get_cumulative_incidence_table ignores an infection dated before its admission", {
  # Patient 2 (department 1, admitted 2024-01-05, no infection) gets a BSI
  # dated 2024-01-03: inside the window, but before the admission — a
  # validation error that only `include_invalid_patients = TRUE` lets in.
  ds <- make_calc_test_ds()
  early <- ds$events[ds$events$event_type_key == "bsi" & ds$events$enrollment_key == 1L, ][1, ]
  early$event_key <- 99L
  early$enrollment_key <- 2L
  early$patient_key <- 2L
  early$occurredAt <- as.Date("2024-01-03")
  ds$events <- dplyr::bind_rows(ds$events, early)

  result <- get_cumulative_incidence_table(ds, january_window(1L))
  expect_equal(result$n_patients, 2L)
  expect_equal(result$n_infected_patients, 1L)
})

test_that("get_cumulative_incidence_table counts an admission in every window it falls into", {
  windows <- tibble::tibble(
    department_key = c(1L, 1L),
    window = c("jan", "early-jan"),
    start  = as.Date(c("2024-01-01", "2024-01-01")),
    end    = as.Date(c("2024-01-31", "2024-01-15")))
  result <- get_cumulative_incidence_table(calc_ds, windows)

  # Both windows hold both admissions and the BSI of 2024-01-08.
  expect_equal(result$window, c("jan", "early-jan"))
  expect_equal(result$n_patients, c(2L, 2L))
  expect_equal(result$n_infected_patients, c(1L, 1L))
})

test_that("get_cumulative_incidence_table honours event_types and conf.level", {
  none <- get_cumulative_incidence_table(
    calc_ds, january_window(1L), event_types = "nec")
  expect_equal(none$n_infected, 0L)

  ci95 <- get_cumulative_incidence_table(calc_ds, january_window(1L))
  ci99 <- get_cumulative_incidence_table(
    calc_ds, january_window(1L), conf.level = 0.99)
  expect_true(ci99$ci_lower < ci95$ci_lower)
  expect_true(ci99$ci_upper > ci95$ci_upper)
})

test_that("get_cumulative_incidence_table numbers the windows on the ungrouped rows, so a grouped input does not pool departments", {
  # The idiomatic construction of the input leaves it grouped; a numbering
  # that restarted per group would give both departments the pooled counts.
  grouped <- january_window(c(1L, 2L)) |>
    dplyr::group_by(department_key)
  result <- get_cumulative_incidence_table(calc_ds, grouped)

  expect_false(dplyr::is_grouped_df(result))
  expect_equal(result$department_key, c(1L, 2L))
  expect_equal(result$n_patients, c(2L, 1L))
  expect_equal(result$n_infected_patients, c(1L, 1L))
})

test_that("get_cumulative_incidence_table returns a tibble for a plain data frame and drops the caller's other columns", {
  plain <- as.data.frame(january_window(1L))
  plain$n_patients <- 99L
  plain$note <- "kept nowhere"
  result <- get_cumulative_incidence_table(calc_ds, plain)

  expect_s3_class(result, "tbl_df")
  expect_false(dplyr::is_grouped_df(result))
  expect_equal(result$n_patients, 2L)
  expect_false("note" %in% names(result))
})

test_that("get_cumulative_incidence_table accepts a pseudonymized department tier", {
  ds <- make_calc_test_ds()
  ds$metadata$dataset_options <- dhis2_dataset_options(
    include_department = "pseudo",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")
  result <- get_cumulative_incidence_table(ds, january_window(1L))
  expect_equal(result$n_patients, 2L)
  expect_equal(result$n_infected_patients, 1L)
})

test_that("get_cumulative_incidence_table rejects malformed windows, unknown event types and narrowed datasets", {
  windows <- january_window(1L)

  no_end <- windows[, c("department_key", "window", "start")]
  expect_error(get_cumulative_incidence_table(calc_ds, no_end), "end")
  cnd <- expect_error(get_cumulative_incidence_table(calc_ds, no_end))
  expect_match(conditionMessage(cnd), "`windows` lacks a required column:", fixed = TRUE)
  no_dates <- windows[, c("department_key", "window")]
  cnd <- expect_error(get_cumulative_incidence_table(calc_ds, no_dates))
  expect_match(conditionMessage(cnd), "`windows` lacks required columns:", fixed = TRUE)

  no_department <- windows
  no_department$department_key <- NA_integer_
  expect_error(get_cumulative_incidence_table(calc_ds, no_department), "department_key")

  text_dates <- windows
  text_dates$start <- as.character(text_dates$start)
  expect_error(get_cumulative_incidence_table(calc_ds, text_dates), "Date")

  reversed <- windows
  reversed$end <- reversed$start - 1L
  expect_error(get_cumulative_incidence_table(calc_ds, reversed), "start <= end")

  expect_error(get_cumulative_incidence_table(calc_ds, windows, event_types = "adm"))

  narrowed <- calc_ds
  narrowed$metadata$dataset_options <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "pseudo",
    include_event      = "full")
  expect_error(
    get_cumulative_incidence_table(narrowed, windows), "include_enrollment")
})

# --- Infectious-agent tables on infection forms -----------------------------
#
# make_infection_calc_test_ds() builds the infection forms; the form items
# are the stored option codes: 1 Yes, 0 No, -1 Not tested or No follow-up.

infectious_agent_tables <- function(ds) list(
  per_agent = get_infectious_agent_detection_rate_per_agent_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
  per_inf_type = get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
  abr_infection = get_abr_infection_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
  abr_infection_ontology = get_abr_infection_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE, rows = "ontology"),
  resistance = get_infectious_agent_resistance_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
  resistance_ontology = get_infectious_agent_resistance_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE, rows = "ontology"),
  resistance_test = get_resistance_test_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
  secondary_bsi = get_secondary_bsi_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE))

resistance_levels <- c("no", "yes", "not_tested")

test_that("a slot without an infectious agent leaves every infectious-agent figure unchanged", {
  # The sepsis of form 1 names Staphylococcus aureus (934) as methicillin
  # resistant; form 7 is a NEC answered Yes whose secondary-BSI slots name no
  # infectious agent.
  forms <- tibble::tibble(
    event_type_key              = c("bsi", "bsi", "hap", "hap", "nec", "ssi",
                                    "nec"),
    sec_bsi                     = c(NA, NA, "1", "0", "1", "0", "1"),
    microbiological_test_result = c(NA, NA, "1", "1", NA, NA, NA))
  agents <- tibble::tibble(
    form          = c(1L, 3L, 3L, 5L, 6L),
    secondary_bsi = c(FALSE, FALSE, TRUE, TRUE, FALSE),
    pathogen_key  = c(934L, 503L, 503L, 1L, 1L),
    source        = c("B", "L", NA, NA, NA),
    multiple      = FALSE,
    mrsa          = factor(c("yes", "no", "no", "no", "no"),
                           levels = resistance_levels))
  # The slots of a second sepsis, a pneumonia answered No, the SSI, and the
  # NEC of form 7, each holding a source, the "recovered multiple times" item,
  # or a resistance category, and no infectious agent.
  companions <- tibble::tibble(
    form          = c(2L, 4L, 4L, 6L, 7L),
    secondary_bsi = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    pathogen_key  = NA_integer_,
    source        = c("B", "U", NA, NA, NA),
    multiple      = c(TRUE, NA, NA, NA, NA),
    mrsa          = factor(c("yes", "yes", "not_tested", "yes", "yes"),
                           levels = resistance_levels))

  base    <- make_infection_calc_test_ds(forms, agents)
  variant <- make_infection_calc_test_ds(forms, dplyr::bind_rows(agents, companions))

  expect_equal(infectious_agent_tables(variant), infectious_agent_tables(base))
})

test_that("the share of pneumonias tested counts a test recorded positive or negative, over every pneumonia", {
  # Pneumonias 1-3 are not tested: their test is recorded "Not tested"
  # (pneumonia 1, which holds an infectious agent in the slots the form does
  # not ask for), left blank (pneumonia 2), or recorded "Not tested" on a form
  # followed up for a secondary BSI and answered No (pneumonia 3). Pneumonia 4
  # is tested negative, pneumonia 5 positive with an infectious agent.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = "hap",
      microbiological_test_result = c("-1", NA, "-1", "0", "1"),
      sec_bsi                     = c("-1", "-1", "0", "-1", "-1")),
    findings = tibble::tibble(
      form = c(1L, 5L), secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  hap <- r[r$inf == "hap", ]

  expect_equal(hap$total, 5L)
  expect_equal(hap$tested, 2L)
  expect_equal(hap$tested_pooled, 40)
  ci <- neoipc_wilson_ci(2L, 5L)
  expect_equal(c(hap$tested_ci_lower, hap$tested_ci_upper),
               c(ci$lower, ci$upper) * 100)
  expect_equal(hap$n, 1L)
  expect_equal(hap$pooled, 50)
  ci <- neoipc_wilson_ci(1L, 2L)
  expect_equal(c(hap$ci_lower, hap$ci_upper), c(ci$lower, ci$upper) * 100)
  # No row gives all types together.
  expect_false("all" %in% r$inf)
})

test_that("the share of SSIs tested counts the culture at the depth the infection type records", {
  # SSIs 1-4 are not tested at their depth: the culture there is recorded
  # "Not tested" (SSI 1, followed up for a secondary BSI and answered No),
  # left blank (SSI 2), tested only at another depth (SSI 3, whose positive
  # deep culture shows the slots that name an infectious agent), or no depth
  # is recorded (SSI 4, cultured positive at every depth). SSI 5 is tested
  # negative at its depth, SSI 6 positive with an infectious agent.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key   = "ssi",
      infection_type   = c("1", "2", "3", NA, "2", "3"),
      organisms_superf = c("-1", NA, NA, "1", NA, NA),
      organisms_deep   = c(NA, NA, "1", "1", "0", NA),
      organisms_organ  = c(NA, NA, "-1", "1", NA, "1"),
      sec_bsi          = c("0", "-1", "-1", "-1", "-1", "-1")),
    findings = tibble::tibble(
      form = c(3L, 6L), secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  ssi <- r[r$inf == "ssi", ]

  expect_equal(ssi$total, 6L)
  expect_equal(ssi$tested, 2L)
  expect_equal(ssi$tested_pooled, 100 / 3)
  expect_equal(ssi$n, 1L)
  expect_equal(ssi$pooled, 50)
})

test_that("the table has a row each for sepsis/BSI, pneumonia, and SSI, none for NEC or all types, and counts no slot its form does not ask for", {
  # NEC 1 is followed up and answered Yes with an infectious agent in its
  # secondary-BSI slots, NEC 2 answered No with one left there, NEC 3 not
  # followed up. Pneumonia 4, not tested, and sepsis 5, recorded
  # culture-negative, hold one in the slots their forms do not ask for.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c("nec", "nec", "nec", "hap", "bsi"),
      sec_bsi                     = c("1", "0", "-1", "-1", NA),
      microbiological_test_result = c(NA, NA, NA, "-1", NA),
      no_pos_culture              = c(NA, NA, NA, NA, TRUE)),
    findings = tibble::tibble(
      form          = c(1L, 2L, 4L, 5L),
      secondary_bsi = c(TRUE, TRUE, FALSE, FALSE),
      pathogen_key  = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_equal(as.character(r$inf), c("bsi", "hap", "ssi"))
  expect_equal(levels(r$inf), c("bsi", "hap", "ssi"))
  # Sepsis 5 is in its denominator, not positive; pneumonia 4 is in the
  # tested share's denominator, not tested.
  bsi <- r[r$inf == "bsi", ]
  expect_equal(c(bsi$n, bsi$tested, bsi$total), c(0L, 1L, 1L))
  expect_equal(bsi$pooled, 0)
  hap <- r[r$inf == "hap", ]
  expect_equal(c(hap$n, hap$tested, hap$total), c(0L, 0L, 1L))
  expect_true(is.na(hap$pooled))
  expect_equal(hap$tested_pooled, 0)
  # A type without infections has its counts at 0 and no share.
  ssi <- r[r$inf == "ssi", ]
  expect_equal(c(ssi$n, ssi$tested, ssi$total), c(0L, 0L, 0L))
  expect_true(all(is.na(c(ssi$pooled, ssi$tested_pooled))))
})

test_that("primary sepsis/BSI has a positive share over every episode and no tested share", {
  # Episode 1 is recorded culture-negative with an infectious agent left in
  # its slots, which the form does not ask for. Episodes 2 and 3 leave the
  # culture item unticked, which DHIS2 stores as no value, since the item
  # takes only "true": episode 2 names no infectious agent, episode 3 names
  # one.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "bsi",
      no_pos_culture = c(TRUE, NA, NA)),
    findings = tibble::tibble(
      form = c(1L, 3L), secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  bsi <- r[r$inf == "bsi", ]

  expect_equal(bsi$n, 1L)
  expect_equal(bsi$tested, 3L)
  expect_equal(bsi$pooled, 100 / 3)
  expect_equal(bsi$total, 3L)
  expect_true(all(is.na(
    c(bsi$tested_pooled, bsi$tested_ci_lower, bsi$tested_ci_upper))))
})

test_that("an infection without a form row stays in its type's denominators", {
  # The reader keeps no form row for an event whose stored values are only
  # infectious-agent slots. Pneumonia 1 is tested positive with an
  # infectious agent and keeps its row; pneumonia 2, SSI 3, and sepsis 4 name
  # one in an own slot and lose their rows, and sepsis 5 names none.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c("hap", "hap", "ssi", "bsi", "bsi"),
      microbiological_test_result = c("1", "1", NA, NA, NA),
      infection_type              = c(NA, NA, "1", NA, NA),
      organisms_superf            = c(NA, NA, "1", NA, NA),
      no_pos_culture              = NA),
    findings = tibble::tibble(
      form = 1:4, secondary_bsi = FALSE, pathogen_key = 1L))
  # The events of forms 2-4, whose findings are rows 2-4.
  without_row <- ds$infectiousAgentFindings$event_key[2:4]
  ds$pneumoniaData <- ds$pneumoniaData[
    !ds$pneumoniaData$event_key %in% without_row, ]
  ds$ssiData <- ds$ssiData[!ds$ssiData$event_key %in% without_row, ]
  ds$sepsisData <- ds$sepsisData[!ds$sepsisData$event_key %in% without_row, ]

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  counts <- function(type)
    unlist(r[r$inf == type, c("n", "tested", "total")], use.names = FALSE)

  # Pneumonia 2 and SSI 3 count as not tested, sepsis 4 as positive.
  expect_equal(counts("hap"), c(1L, 1L, 2L))
  expect_equal(counts("ssi"), c(0L, 0L, 1L))
  expect_equal(counts("bsi"), c(1L, 2L, 2L))
})

test_that("the positive share counts a test recorded positive with an own slot naming an infectious agent", {
  # Every pneumonia and SSI is tested. Pneumonia 1 and SSI 6 are tested
  # positive with an infectious agent in an own slot. Pneumonias 2-4 and
  # SSIs 7-8 are tested positive without one: no slot (2, 7), an own slot
  # holding only companion values (3), or an infectious agent only in a
  # secondary-BSI slot, the follow-up answered Yes (4, 8). Pneumonia 5 is
  # tested negative with an infectious agent in the slots the form does not
  # ask for, and SSI 9 is tested negative at its depth with one in the slots
  # that a positive culture at another depth shows.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c(rep("hap", 5), rep("ssi", 4)),
      microbiological_test_result = c("1", "1", "1", "1", "0", rep(NA, 4)),
      sec_bsi                     = c("-1", "-1", "-1", "1", "-1",
                                      "-1", "-1", "1", "-1"),
      infection_type              = c(rep(NA, 5), rep("1", 4)),
      organisms_superf            = c(rep(NA, 5), "1", "1", "1", "0"),
      organisms_deep              = c(rep(NA, 8), "1"),
      organisms_organ             = NA),
    findings = tibble::tibble(
      form          = c(1L, 3L, 4L, 5L, 6L, 8L, 9L),
      secondary_bsi = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
      pathogen_key  = c(1L, NA, 1L, 1L, 1L, 1L, 1L)))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  hap <- r[r$inf == "hap", ]
  ssi <- r[r$inf == "ssi", ]

  expect_equal(c(hap$n, hap$tested, hap$total), c(1L, 5L, 5L))
  expect_equal(hap$pooled, 20)
  expect_equal(hap$tested_pooled, 100)
  expect_equal(c(ssi$n, ssi$tested, ssi$total), c(1L, 4L, 4L))
  expect_equal(ssi$pooled, 25)
  expect_equal(ssi$tested_pooled, 100)
  # No row gives all types together.
  expect_false("all" %in% r$inf)
})

test_that("the follow-up for a secondary BSI moves neither share", {
  # Two departments of 10 pneumonias each, 5 of them tested and 2 of these
  # positive with an infectious agent in an own slot. Department 1 follows
  # every pneumonia up for a secondary BSI, pneumonia 10, not tested, answered
  # Yes with an infectious agent in its secondary-BSI slots; department 2
  # follows none up.
  pneumonias <- function(sec_bsi)
    tibble::tibble(
      event_type_key              = "hap",
      microbiological_test_result = rep(c("1", "0", "-1"), c(2, 3, 5)),
      sec_bsi                     = sec_bsi)
  followed_up <- make_infection_calc_test_ds(
    forms    = pneumonias(c(rep("0", 9), "1")),
    findings = tibble::tibble(
      form          = c(1L, 2L, 10L),
      secondary_bsi = c(FALSE, FALSE, TRUE),
      pathogen_key  = 1L))
  not_followed_up <- make_infection_calc_test_ds(
    forms    = pneumonias("-1"),
    findings = tibble::tibble(
      form = 1:2, secondary_bsi = FALSE, pathogen_key = 1L))
  shares <- function(ds) {
    r <- get_infectious_agent_detection_rate_per_inf_type_table(
      ds, use_cache = FALSE, include_quartiles = FALSE)
    r[r$inf == "hap", c("n", "tested", "pooled", "ci_lower", "ci_upper",
                        "total", "tested_pooled", "tested_ci_lower",
                        "tested_ci_upper")]
  }

  expect_equal(shares(followed_up), shares(not_followed_up))
  expect_equal(shares(not_followed_up)$tested_pooled, 50)
  expect_equal(shares(not_followed_up)$pooled, 40)
})

test_that("the positive share counts \"Not listed\", \"Unidentifiable\", and a code the list lacks", {
  # Sepsis 1-3, pneumonias 4-6 and SSIs 7-9 tested positive, and NECs 10-12
  # answered Yes name "Not listed" (0), "Unidentifiable" (3561), and a code
  # the list lacks, in their own and the NECs' secondary-BSI slots.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = rep(c("bsi", "hap", "ssi", "nec"),
                                        each = 3),
      microbiological_test_result = rep(c(NA, "1", NA, NA), each = 3),
      infection_type              = rep(c(NA, NA, "1", NA), each = 3),
      organisms_superf            = rep(c(NA, NA, "1", NA), each = 3),
      sec_bsi                     = rep(c(NA, "-1", "-1", "1"), each = 3)),
    findings = tibble::tibble(
      form          = 1:12,
      secondary_bsi = rep(c(FALSE, TRUE), c(9, 3)),
      pathogen_key  = rep(c(0L, 3561L, 99999999L), 4)))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)

  for (type in c("bsi", "hap", "ssi")) {
    expect_equal(r$n[r$inf == type], 3L)
    expect_equal(r$pooled[r$inf == type], 100)
  }
  # The NECs' secondary-BSI slots count in no row, and no row gives all types
  # together.
  expect_equal(as.character(r$inf), c("bsi", "hap", "ssi"))
})

tested_quartile_cols <- c(
  "tested_q1", "tested_q2", "tested_q3",
  "tested_q1_ci_lower", "tested_q1_ci_upper",
  "tested_q2_ci_lower", "tested_q2_ci_upper",
  "tested_q3_ci_lower", "tested_q3_ci_upper",
  "tested_quartile_departments")

test_that("the reference per-type table gives each share the departments' quartiles over its own denominator", {
  # Each department has `size` pneumonias: `tested[d]` of department d are
  # tested, `positive[d]` of these positive with an infectious agent in an own
  # slot, and every pneumonia is followed up for a secondary BSI and answered
  # No. The departments take the keys 1, 2, 4, 5, and so on: department 3 has
  # no infections and enters no quartiles.
  departments <- function(tested, positive, size = 20L) {
    size <- rep_len(size, length(tested))
    forms <- dplyr::bind_rows(lapply(seq_along(tested), \(d) tibble::tibble(
      department_key              = d + (d >= 3L),
      microbiological_test_result = rep(
        c("1", "0", "-1"),
        c(positive[d], tested[d] - positive[d], size[d] - tested[d])),
      sec_bsi                     = "0")))
    make_infection_calc_test_ds(
      forms    = dplyr::mutate(forms, event_type_key = "hap"),
      findings = tibble::tibble(
        form          = which(forms$microbiological_test_result == "1"),
        secondary_bsi = FALSE,
        pathogen_key  = 1L))
  }
  quartiles <- function(row, prefix = "")
    unlist(row[paste0(prefix, c("q1", "q2", "q3"))], use.names = FALSE)
  intervals <- function(row, prefix = "")
    unlist(row[paste0(prefix, c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower",
                                "q2_ci_upper", "q3_ci_lower", "q3_ci_upper"))],
           use.names = FALSE)
  binomial_intervals <- function(events, denominators)
    unlist(bootstrap_quantile_ci(events, denominators, type = "binomial",
                                 multiplier = 100), use.names = FALSE)

  # Departments 1-5 have 20 pneumonias each and test 25, 50, 75, 100, and
  # 100% of them, and find an infectious agent in 40, 0, 60, 25, and 100% of
  # the tested, department 2 entering the positive share's quartiles at 0.
  # Department 6 has 4 pneumonias, all tested positive, below the minimum of
  # 5 of either share's denominator: it enters neither share's quartiles,
  # while its pneumonias count in the pooled shares, 74 of 104 tested and 40
  # of these positive. Each pooled share passes the display test, against a
  # median of 20 pneumonias and of 15 tested ones.
  tested   <- c(5L, 10L, 15L, 20L, 20L, 4L)
  positive <- c(2L, 0L, 9L, 5L, 20L, 4L)
  ds <- departments(tested, positive, size = c(rep(20L, 5), 4L))
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = TRUE)
  expect_equal(
    names(r),
    c("inf", "n", "tested", "pooled", "ci_lower", "ci_upper",
      "q1", "q2", "q3", "q1_ci_lower", "q1_ci_upper", "q2_ci_lower",
      "q2_ci_upper", "q3_ci_lower", "q3_ci_upper", "quartile_departments",
      "total", "tested_pooled", "tested_ci_lower", "tested_ci_upper",
      tested_quartile_cols))
  hap <- r[r$inf == "hap", ]
  expect_equal(c(hap$n, hap$tested, hap$total), c(40L, 74L, 104L))
  expect_equal(hap$tested_pooled, 7400 / 104)
  expect_equal(hap$pooled, 4000 / 74)
  expect_equal(quartiles(hap, "tested_"), c(50, 75, 100))
  expect_equal(intervals(hap, "tested_"),
               binomial_intervals(tested[1:5], rep(20L, 5)))
  expect_equal(hap$tested_quartile_departments, 5L)
  expect_equal(quartiles(hap), c(25, 40, 60))
  expect_equal(intervals(hap), binomial_intervals(positive[1:5], tested[1:5]))
  expect_equal(hap$quartile_departments, 5L)
  # Sepsis/BSI has no tested share, and no row gives all types together.
  expect_equal(as.character(r$inf), c("bsi", "hap", "ssi"))
  expect_true(all(is.na(unlist(r[r$inf == "bsi", tested_quartile_cols]))))
  # A table without quartiles leaves out both shares' quartiles and the
  # departments that enter them.
  without <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = TRUE, include_quartiles = FALSE)
  expect_false(any(c("q1", "quartile_departments", tested_quartile_cols) %in%
                     names(without)))

  # Department 5 tests none of its pneumonias: it enters the tested share's
  # quartiles at 0, and the positive share's 4 departments are too few,
  # their number given all the same.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(c(5L, 10L, 15L, 20L, 0L), c(2L, 0L, 9L, 5L, 0L)),
      use_cache = FALSE))
  expect_equal(quartiles(hap, "tested_"), c(25, 50, 75))
  expect_equal(hap$tested_quartile_departments, 5L)
  expect_true(all(is.na(c(quartiles(hap), intervals(hap)))))
  expect_equal(hap$quartile_departments, 4L)

  # Every department tests all its 8 pneumonias and finds an infectious
  # agent in 1: at the pooled share of 12.5%, 8 tested pneumonias give 1
  # positive one, and the departments' median of 8 tested pneumonias does not
  # exceed them, though all 5 enter.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(rep(8L, 5), rep(1L, 5), size = 8L), use_cache = FALSE))
  expect_equal(quartiles(hap, "tested_"), c(100, 100, 100))
  expect_true(all(is.na(c(quartiles(hap), intervals(hap)))))
  expect_equal(hap$quartile_departments, 5L)

  # 4 departments are too few for either share's quartiles.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(tested[1:4], positive[1:4]), use_cache = FALSE))
  expect_true(all(is.na(c(quartiles(hap), intervals(hap)))))
  expect_true(all(is.na(unlist(hap[tested_quartile_cols[1:9]]))))
  expect_equal(c(hap$quartile_departments, hap$tested_quartile_departments),
               c(4L, 4L))

  # Sepsis/BSI's positive share takes every episode. Each department has 5
  # episodes, of which 1, 2, 2, 3, and 5 name an infectious agent, the pooled
  # 52% passing the display test against a median of 5 episodes.
  episodes <- c(1L, 2L, 2L, 3L, 5L)
  bsi <- (\(r) r[r$inf == "bsi", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      make_infection_calc_test_ds(
        forms    = tibble::tibble(
          event_type_key = "bsi",
          department_key = rep(1:5, each = 5),
          no_pos_culture = NA),
        findings = tibble::tibble(
          form          = which(sequence(rep(5L, 5)) <=
                                  rep(episodes, each = 5)),
          secondary_bsi = FALSE,
          pathogen_key  = 1L)),
      use_cache = FALSE))
  expect_equal(quartiles(bsi), c(40, 40, 60))
  expect_equal(intervals(bsi), binomial_intervals(episodes, rep(5L, 5)))
  expect_equal(bsi$quartile_departments, 5L)
  expect_equal(bsi$total, 25L)
  expect_true(all(is.na(unlist(bsi[c("tested_pooled", "tested_ci_lower",
                                     "tested_ci_upper",
                                     tested_quartile_cols)]))))
})

# --- Infectious agents detected: golden tests --------------------------------
#
# make_infectious_agent_calc_test_ds() describes the 17 infections. Forms 8,
# 16, and 17 name no infectious agent in a slot the form asks for, so 14 are
# confirmed: sepsis 1-4, 10, 11, 13, and 14, pneumonias 5, 6, and 12, NEC 7,
# and SSIs 9 and 15. Forms 13 and 15 are confirmed only by "Not listed" and a
# code the list lacks, so 12 have an infectious agent in the tree: 7 sepsis,
# 3 pneumonias, 1 NEC, and 1 SSI.

golden_ds <- make_infectious_agent_calc_test_ds()

# The ontology version's rows of the golden fixture: the infections with an
# infectious agent at or beneath each node.
golden_ontology_rows <- tibble::tribble(
  ~group,                             ~level, ~n,
  "Total",                                0L, 18L,
  "Bacteria",                             1L,  7L,
  "Bacillati",                            2L,  4L,
  "Bacillota",                            3L,  4L,
  "Bacilli",                              4L,  4L,
  "Caryophanales",                        5L,  3L,
  "Staphylococcaceae",                    6L,  3L,
  "Staphylococcus",                       7L,  3L,
  "Coagulase-positive staphylococci",     8L,  2L,
  "Staphylococcus aureus complex",        9L,  2L,
  "Staphylococcus aureus",               10L,  2L,
  "Coagulase-negative staphylococci",     8L,  1L,
  "Staphylococcus epidermidis",           9L,  1L,
  "Staphylococcus n.o.s.",                8L,  1L,
  "Lactobacillales",                      5L,  1L,
  "Enterococcaceae",                      6L,  1L,
  "Enterococcus",                         7L,  1L,
  "Enterococcus faecalis",                8L,  1L,
  "Pseudomonadati",                       2L,  3L,
  "Pseudomonadota",                       3L,  3L,
  "Gammaproteobacteria",                  4L,  3L,
  "Enterobacterales",                     5L,  2L,
  "Enterobacteriaceae",                   6L,  2L,
  "Escherichia",                          7L,  1L,
  "Escherichia coli",                     8L,  1L,
  "Klebsiella",                           7L,  1L,
  "Klebsiella pneumoniae",                8L,  1L,
  "Pseudomonadales",                      5L,  1L,
  "Pseudomonadaceae",                     6L,  1L,
  "Pseudomonas",                          7L,  1L,
  "Pseudomonas aeruginosa",               8L,  1L,
  "Fungi",                                1L,  3L,
  "Dikarya",                              2L,  3L,
  "Ascomycota",                           3L,  3L,
  "Saccharomycotina",                     4L,  2L,
  "Pichiomycetes",                        5L,  2L,
  "Serinales",                            6L,  2L,
  "Debaryomycetaceae",                    7L,  2L,
  "Candida",                              8L,  2L,
  "Candida albicans",                     9L,  1L,
  "Candida n.o.s.",                       9L,  1L,
  "Pezizomycotina",                       4L,  1L,
  "Eurotiomycetes",                       5L,  1L,
  "Eurotiomycetidae",                     6L,  1L,
  "Eurotiales",                           7L,  1L,
  "Aspergillaceae",                       8L,  1L,
  "Aspergillus",                          9L,  1L,
  "Aspergillus fumigatus",               10L,  1L,
  "Viruses",                              1L,  3L,
  "Duplodnaviria",                        2L,  1L,
  "Heunggongvirae",                       3L,  1L,
  "Peploviricota",                        4L,  1L,
  "Herviviricetes",                       5L,  1L,
  "Herpesvirales",                        6L,  1L,
  "Orthoherpesviridae",                   7L,  1L,
  "Alphaherpesvirinae",                   8L,  1L,
  "Simplexvirus",                         9L,  1L,
  "Human herpes simplex virus",          10L,  1L,
  "Simplexvirus humanalpha1",            11L,  1L,
  "Riboviria",                            2L,  1L,
  "Orthornavirae",                        3L,  1L,
  "Pisuviricota",                         4L,  1L,
  "Pisoniviricetes",                      5L,  1L,
  "Picornavirales",                       6L,  1L,
  "Picornaviridae",                       7L,  1L,
  "Ensavirinae",                          8L,  1L,
  "Enterovirus",                          9L,  1L,
  "Enterovirus betacoxsackie",           10L,  1L,
  "Varidnaviria",                         2L,  1L,
  "Bamfordvirae",                         3L,  1L,
  "Preplasmiviricota",                    4L,  1L,
  "Polisuviricotina",                     5L,  1L,
  "Pharingeaviricetes",                   6L,  1L,
  "Rowavirales",                          7L,  1L,
  "Adenoviridae",                         8L,  1L,
  "Mastadenovirus",                       9L,  1L,
  "Human adenovirus",                    10L,  1L,
  "Mastadenovirus adami",                11L,  1L,
  "Protozoa",                             1L,  1L,
  "Myxomycota",                           2L,  1L,
  "Prototheca",                           3L,  1L,
  "Prototheca zopfii",                    4L,  1L)

test_that("the Total counts every infectious agent once per infection, those outside the tree included", {
  for (rows in c("groups", "ontology")) {
    r <- get_infectious_agent_detection_rate_per_agent_table(
      golden_ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows)
    total <- r[1, ]
    expect_equal(total$level, 0L)
    expect_equal(total$group, "Total")
    expect_true(is.na(total$node_id))
    # 18 infectious agents in 14 confirmed infections: forms 1, 6, 9, and 14
    # hold 2 each, form 5 one in two slots, and form 10 two synonyms of one
    # concept.
    expect_equal(total$n, 18L)
    expect_equal(total$total, 14L)
    expect_equal(total$pooled, 18 / 14 * 100)
    ci <- neoipc_poisson_ci(18, 14, multiplier = 100)
    expect_equal(c(total$ci_lower, total$ci_upper), c(ci$lower, ci$upper))
    # "Not listed", "Unidentifiable", and the code the list lacks, which no
    # row counts.
    expect_equal(total$n_total_only, 3L)
    # Per type, infectious agents per 100 confirmed infections of the type.
    expect_equal(
      unlist(total[c("n_bsi", "n_hap", "n_nec", "n_ssi")], use.names = FALSE),
      c(10L, 4L, 1L, 3L))
    expect_equal(
      unlist(total[c("total_bsi", "total_hap", "total_nec", "total_ssi")],
             use.names = FALSE),
      c(8L, 3L, 1L, 2L))
    expect_equal(total$pooled_hap, 4 / 3 * 100)
    ci <- neoipc_poisson_ci(4, 3, multiplier = 100)
    expect_equal(c(total$ci_lower_hap, total$ci_upper_hap),
                 c(ci$lower, ci$upper))
    # Every other row's share is over the 12 infections with an infectious
    # agent in the tree.
    expect_true(all(r$total[-1] == 12L))
    expect_true(all(r$total_bsi[-1] == 7L))
    expect_true(all(is.na(r$n_total_only[-1])))
  }
})

test_that("over the reporting groups, the summary block and the group tree count infections, a parent's included", {
  r <- get_infectious_agent_detection_rate_per_agent_table(
    golden_ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_s3_class(r, "neoipcr_tbl_iadrpa")
  expect_equal(
    names(r)[1:10],
    c("level", "rank", "node_id", "parent_id", "group", "n", "total",
      "pooled", "ci_lower", "ci_upper"))
  expect_equal(
    r$node_id,
    c(NA,
      "gram_reaction|positive", "gram_reaction|negative",
      "oxygen_requirement|aerobic",
      "oxygen_requirement|facultative-anaerobic",
      "bacteria", "gram-positive-cocci", "enterobacterales",
      "gram-negative-bacilli", "non-fermenting-gram-negative-bacilli",
      "fungi", "yeasts", "moulds",
      "viruses", "parasites"))
  expect_equal(
    r$group,
    c("Total", "Gram-positive", "Gram-negative", "Aerobic",
      "Facultatively anaerobic", "Bacteria", "Gram-positive cocci",
      "Enterobacterales", "Gram-negative bacilli, not Enterobacterales",
      "Non-fermenting Gram-negative bacilli", "Fungi", "Yeasts", "Moulds",
      "Viruses", "Parasites"))
  expect_equal(r$level, c(0L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 1L, 2L, 2L,
                          1L, 1L))
  expect_equal(
    r$parent_id,
    c(NA, NA, NA, NA, NA, NA, "bacteria", "bacteria", "bacteria",
      "gram-negative-bacilli", NA, "fungi", "fungi", NA, NA))
  expect_true(all(is.na(r$rank)))
  # Bacteria: forms 1, 2, 3, 5, 6, 7, and 14; form 1's two staphylococci
  # count once in their group; form 9's mould and protozoon in two top-level
  # groups. The summary block counts the bacteria by Gram reaction and oxygen
  # requirement, in the classification's order of their values.
  expect_equal(r$n, c(18L, 4L, 3L, 1L, 6L, 7L, 4L, 2L, 1L, 1L, 3L, 2L, 1L,
                      3L, 1L))
  expect_equal(r$pooled[-1], r$n[-1] / 12 * 100)
  ci <- neoipc_wilson_ci(7, 12)
  expect_equal(c(r$ci_lower[6], r$ci_upper[6]), c(ci$lower, ci$upper) * 100)
  # The type columns of the bacteria: sepsis 1, 2, 3, and 14, pneumonias 5
  # and 6, NEC 7, and no SSI.
  bacteria <- r[r$node_id %in% "bacteria", ]
  expect_equal(
    unlist(bacteria[c("n_bsi", "n_hap", "n_nec", "n_ssi")], use.names = FALSE),
    c(4L, 2L, 1L, 0L))
  expect_equal(
    unlist(bacteria[c("total_bsi", "total_hap", "total_nec", "total_ssi")],
           use.names = FALSE),
    c(7L, 3L, 1L, 1L))
  expect_equal(bacteria$pooled_bsi, 4 / 7 * 100)
  ci <- neoipc_wilson_ci(0, 1)
  expect_equal(c(bacteria$pooled_ssi, bacteria$ci_lower_ssi,
                 bacteria$ci_upper_ssi),
               c(0, ci$lower * 100, ci$upper * 100))
  # The pooled column alone has quartiles.
  r <- get_infectious_agent_detection_rate_per_agent_table(
    golden_ds, use_cache = FALSE)
  expect_s3_class(r, "neoipcr_tbl_iadrpa_ref")
  expect_true(all(c("q1", "q2", "q3", "q1_ci_lower", "q3_ci_upper",
                    "quartile_departments") %in% names(r)))
  expect_false(any(grepl("^q[123].*_(bsi|hap|nec|ssi)$", names(r))))
})

test_that("over the full ontology, every level of a path has a row, with a residual row for the findings recorded above their rows", {
  r <- get_infectious_agent_detection_rate_per_agent_table(
    golden_ds, use_cache = FALSE, include_quartiles = FALSE,
    rows = "ontology")

  expect_s3_class(r, "neoipcr_tbl_iadrpa")
  # Pre-order, siblings by rate, descending, ties by name, and a residual row
  # last among its siblings.
  expect_equal(r$group, golden_ontology_rows$group)
  expect_equal(r$level, golden_ontology_rows$level)
  expect_equal(r$n, golden_ontology_rows$n)
  expect_true(all(r$total[-1] == 12L))
  # Each row's parent is the nearest row above it one level up.
  for (i in seq_len(nrow(r))[-1]) {
    above <- which(r$level[seq_len(i - 1L)] == r$level[i] - 1L)
    expected_parent <- if (r$level[i] == 1L) NA_character_
                       else r$node_id[max(above)]
    expect_identical(r$parent_id[i], expected_parent)
  }

  staphylococcus <- which(r$node_id %in% "LPSN|genus/staphylococcus")
  expect_equal(r$rank[staphylococcus], "Genus")
  # Form 2's genus is recorded above the species rows beneath it.
  residual <- r[r$parent_id %in% "LPSN|genus/staphylococcus", ]
  expect_equal(residual$node_id[3], "LPSN|genus/staphylococcus~n.o.s.")
  expect_equal(residual$group[3], "Staphylococcus n.o.s.")
  expect_true(is.na(residual$rank[3]))
  # A parent's share is not the sum of its children's: form 1 holds two
  # staphylococci. Nor do the top-level rows add up to the 12 infections with
  # an infectious agent in the tree: form 9 holds a fungus and a protozoon,
  # each in a top-level row of its own, Eukaryota having none.
  expect_equal(r$n[staphylococcus], 3L)
  expect_equal(sum(residual$n), 4L)
  top <- r[r$level == 1L, ]
  expect_equal(top$group, c("Bacteria", "Fungi", "Viruses", "Protozoa"))
  expect_true(all(is.na(top$parent_id)))
  expect_equal(sum(top$n), 14L)
  expect_false("NeoIPC|99" %in% c(r$node_id, r$parent_id))
  # Synonyms count as their concept: form 10's two codes give one infection
  # with one species, Simplexvirus humanalpha1, keyed by its IctvId.
  expect_equal(r$n[r$node_id %in% "ICTV|19710176"], 1L)
  # The type columns of the genus: sepsis 1, 2, and 14.
  expect_equal(r$n_bsi[staphylococcus], 3L)
  expect_equal(r$pooled_bsi[staphylococcus], 3 / 7 * 100)
  expect_equal(r$n_hap[staphylococcus], 0L)
  # No summary block over the full ontology.
  expect_false(any(grepl("^(gram_reaction|oxygen_requirement)\\|",
                         r$node_id)))
})

test_that("an infection whose slot the form does not ask for, or which holds a companion value alone, counts in no infectious-agent table", {
  # Forms 8, 16, and 17 removed leave every figure as it is.
  ds <- golden_ds
  ds$infectiousAgentFindings <- dplyr::filter(
    ds$infectiousAgentFindings,
    !.data$event_key %in% ds$events$event_key[
      ds$events$event_type_key %in% c("bsi", "nec", "hap", "ssi")][
        c(8L, 16L, 17L)])
  for (rows in c("groups", "ontology"))
    expect_equal(
      get_infectious_agent_detection_rate_per_agent_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows),
      get_infectious_agent_detection_rate_per_agent_table(
        golden_ds, use_cache = FALSE, include_quartiles = FALSE,
        rows = rows))
})

# The tables of `ds` that count its infectious agents, without quartiles: in
# each version, the infectious agents detected, the density of infections
# with resistant infectious agents, and the resistance rates per infectious
# agent; and the completeness of antimicrobial susceptibility testing.
counted_infectious_agent_tables <- function(ds) {
  version <- function(rows)
    list(
      per_agent = get_infectious_agent_detection_rate_per_agent_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows),
      abr = get_abr_infection_rate_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows),
      resistance = get_infectious_agent_resistance_rate_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows))
  list(
    groups = version("groups"),
    ontology = version("ontology"),
    resistance_test = get_resistance_test_rate_table(
      ds, use_cache = FALSE, include_quartiles = FALSE))
}

# The nodes of the version `rows` on the path of the infectious agent `id`
# that are on no path of the infectious agents `others`.
nodes_of_only <- function(rows, id, others) {
  closure <- neoipcr:::internal_infectious_agent_closure
  closure <- closure[closure$version == rows, ]
  setdiff(closure$node_id[closure$input_id == id],
          closure$node_id[closure$input_id %in% others])
}

test_that("an SSI's own slots count where a culture at any depth is positive, whether or not it was followed up for a secondary BSI", {
  # A superficial SSI not tested at its own depth, its deep culture positive,
  # names Staphylococcus aureus (934), recorded as methicillin-resistant
  # S. aureus (MRSA), in its own slot; its follow-up for a secondary BSI is
  # answered No follow-up, No, or Yes.
  tables <- function(sec_bsi)
    counted_infectious_agent_tables(with_antibiotic_entries(
      make_infection_calc_test_ds(
        forms = tibble::tibble(
          event_type_key   = "ssi",
          infection_type   = "1",
          organisms_superf = "-1",
          organisms_deep   = "1",
          sec_bsi          = sec_bsi),
        findings = tibble::tibble(
          form          = 1L,
          secondary_bsi = FALSE,
          pathogen_key  = 934L,
          mrsa          = "yes"),
        patient_days = 100L)))
  not_followed_up <- tables("-1")

  # The slot counts: S. aureus is the Total's infectious agent, and the
  # infection one with MRSA, tested for it.
  for (rows in c("groups", "ontology")) {
    t <- not_followed_up[[rows]]
    expect_equal(t$per_agent$n[t$per_agent$group == "Total"], 1L, info = rows)
    expect_equal(t$abr$n[t$abr$abr == "mrsa" & t$abr$group == "Total"], 1L,
                 info = rows)
    mrsa <- t$resistance[t$resistance$abr == "mrsa", ]
    expect_gt(nrow(mrsa), 0L)
    expect_true(all(mrsa$n == 1L & mrsa$total == 1L), info = rows)
  }
  testing <- not_followed_up$resistance_test
  expect_equal(unlist(testing[testing$abr == "mrsa", c("n", "total")],
                      use.names = FALSE),
               c(1L, 1L))

  expect_equal(tables("0"), not_followed_up)
  expect_equal(tables("1"), not_followed_up)
})

test_that("a secondary-BSI slot counts only where the follow-up is answered Yes, and an own slot only where the form records a positive culture or test", {
  # A pneumonia tested positive and followed up for a secondary BSI, answered
  # No, names Staphylococcus aureus (934) in its own slot and Escherichia coli
  # (1807), recorded with third-generation cephalosporin resistance (3GCR), in
  # a secondary-BSI slot.
  pneumonia <- counted_infectious_agent_tables(with_antibiotic_entries(
    make_infection_calc_test_ds(
      forms = tibble::tibble(
        event_type_key              = "hap",
        microbiological_test_result = "1",
        sec_bsi                     = "0"),
      findings = tibble::tibble(
        form          = 1L,
        secondary_bsi = c(FALSE, TRUE),
        pathogen_key  = c(934L, 1807L),
        `3gcr`        = c(NA, "yes")),
      patient_days = 100L)))
  nodes <- neoipcr:::internal_infectious_agent_nodes
  for (rows in c("groups", "ontology")) {
    e_coli <- nodes_of_only(rows, 1807L, 934L)
    # The nodes of E. coli alone hold Enterobacterales, and over the full
    # ontology E. coli itself.
    names_e_coli <- nodes$name[nodes$version == rows &
                                 nodes$node_id %in% e_coli]
    expect_true("Enterobacterales" %in% names_e_coli, info = rows)
    if (rows == "ontology")
      expect_true("Escherichia coli" %in% names_e_coli)
    t <- pneumonia[[rows]]
    expect_equal(t$per_agent$n[t$per_agent$group == "Total"], 1L, info = rows)
    expect_false(any(t$per_agent$node_id %in% e_coli), info = rows)
    gcr <- t$abr[t$abr$abr == "3gcr", ]
    expect_equal(gcr$n[gcr$group == "Total"], 0L, info = rows)
    expect_false(any(gcr$node_id %in% e_coli), info = rows)
    expect_false(any(t$resistance$node_id[t$resistance$abr == "3gcr"] %in%
                       e_coli),
                 info = rows)
  }
  testing <- pneumonia$resistance_test
  expect_equal(
    unlist(testing[testing$abr == "3gcr" & testing$cond == "routine",
                   c("n", "total")],
           use.names = FALSE),
    c(0L, 0L))

  # The mirror case: an SSI followed up and answered Yes, with no positive
  # culture at any depth, names S. aureus, recorded MRSA, in its own slot and
  # E. coli, recorded 3GCR resistant, in a secondary-BSI slot. Only E. coli
  # counts.
  ssi <- counted_infectious_agent_tables(with_antibiotic_entries(
    make_infection_calc_test_ds(
      forms = tibble::tibble(
        event_type_key   = "ssi",
        infection_type   = "1",
        organisms_superf = "0",
        organisms_deep   = "-1",
        organisms_organ  = "-1",
        sec_bsi          = "1"),
      findings = tibble::tibble(
        form          = 1L,
        secondary_bsi = c(FALSE, TRUE),
        pathogen_key  = c(934L, 1807L),
        `3gcr`        = c(NA, "yes"),
        mrsa          = c("yes", NA)),
      patient_days = 100L)))
  for (rows in c("groups", "ontology")) {
    s_aureus <- nodes_of_only(rows, 934L, 1807L)
    e_coli <- nodes_of_only(rows, 1807L, 934L)
    t <- ssi[[rows]]
    expect_equal(t$per_agent$n[t$per_agent$group == "Total"], 1L, info = rows)
    expect_false(any(t$per_agent$node_id %in% s_aureus), info = rows)
    expect_true(all(e_coli %in% t$per_agent$node_id), info = rows)
    expect_equal(t$abr$n[t$abr$abr == "3gcr" & t$abr$group == "Total"], 1L,
                 info = rows)
    expect_equal(t$abr$n[t$abr$abr == "mrsa" & t$abr$group == "Total"], 0L,
                 info = rows)
    expect_false(any(t$resistance$abr == "mrsa"), info = rows)
  }
  testing <- ssi$resistance_test
  expect_equal(
    unlist(testing[testing$cond == "routine" &
                     testing$abr %in% c("3gcr", "mrsa"), c("n", "total")],
           use.names = FALSE),
    c(1L, 0L, 1L, 0L))
})

test_that("the per-type table counts the golden infections positive within each type's denominators", {
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    golden_ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_s3_class(r, "neoipcr_tbl_iadrpit")
  expect_equal(names(r), c("inf", "n", "tested", "pooled", "ci_lower",
                           "ci_upper", "total", "tested_pooled",
                           "tested_ci_lower", "tested_ci_upper"))
  expect_equal(as.character(r$inf), c("bsi", "hap", "ssi"))
  # Sepsis 16 and 17 are in the sepsis denominators but not positive; NECs 7
  # and 8 have no row.
  expect_equal(r$n, c(8L, 3L, 2L))
  expect_equal(r$tested, c(10L, 3L, 2L))
  expect_equal(r$total, c(10L, 3L, 2L))
  expect_equal(r$pooled, c(80, 100, 100))
  expect_equal(r$tested_pooled, c(NA, 100, 100))
  ci <- neoipc_wilson_ci(8, 10)
  expect_equal(c(r$ci_lower[1], r$ci_upper[1]), c(ci$lower, ci$upper) * 100)
})

test_that("the per-agent quartiles take the departments with 5 infections in a row's denominator, at 0 without the row's", {
  # Departments 1-5 have 5 confirmed sepsis each, k of department k's naming
  # Staphylococcus aureus (934) and the others Escherichia coli (1807);
  # department 1's first also names Candida albicans (2273). Department 6 has
  # 4, all naming S. aureus, below the minimum; department 7 has 5 sepsis
  # recorded culture-negative, whose slots name S. aureus where the form does
  # not ask for them, so that none is confirmed.
  departments <- c(rep(1:5, each = 5), rep(6L, 4), rep(7L, 5))
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "bsi",
      department_key = departments,
      no_pos_culture = departments == 7L),
    findings = tibble::tibble(
      form = c(seq_len(29), 1L, 30:34),
      pathogen_key = c(
        unlist(lapply(1:5, \(k) c(rep(934L, k), rep(1807L, 5L - k)))),
        rep(934L, 4), 2273L, rep(934L, 5))))

  r <- get_infectious_agent_detection_rate_per_agent_table(ds, use_cache = FALSE)
  row <- function(id) r[r$node_id %in% id, ]

  gpc <- row("gram-positive-cocci")
  expect_equal(gpc$pooled, 19 / 29 * 100)
  expect_equal(gpc$quartile_departments, 5L)
  expect_equal(c(gpc$q1, gpc$q2, gpc$q3), c(40, 60, 80))
  expect_equal(
    gpc[c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
          "q3_ci_lower", "q3_ci_upper")],
    bootstrap_quantile_ci(1:5, rep(5L, 5L), type = "binomial",
                          multiplier = 100),
    ignore_attr = TRUE)
  # Department 5 enters the Enterobacterales at 0.
  entero <- row("enterobacterales")
  expect_equal(entero$quartile_departments, 5L)
  expect_equal(c(entero$q1, entero$q2, entero$q3), c(20, 40, 60))
  # The yeasts' pooled share gives 1 infection in 29, above the median of 5
  # infections, so the row shows no quartiles.
  yeasts <- row("yeasts")
  expect_equal(yeasts$quartile_departments, 5L)
  expect_true(all(is.na(c(yeasts$q1, yeasts$q2, yeasts$q3,
                          yeasts$q1_ci_lower, yeasts$q3_ci_upper))))
  # The Total's quartiles are of infectious agents per 100 confirmed
  # infections, with a Poisson bootstrap.
  total <- r[1, ]
  expect_equal(total$quartile_departments, 5L)
  expect_equal(c(total$q1, total$q2, total$q3), c(100, 100, 100))
  expect_equal(
    total$q3_ci_upper,
    bootstrap_quantile_ci(c(6L, 5L, 5L, 5L, 5L), rep(5L, 5L),
                          type = "poisson", multiplier = 100)$q3_ci_upper)
  # Over the full ontology, the same rule.
  r <- get_infectious_agent_detection_rate_per_agent_table(
    ds, use_cache = FALSE, rows = "ontology")
  aureus <- r[r$node_id %in% "LPSN|species/staphylococcus-aureus", ]
  expect_equal(c(aureus$q1, aureus$q2, aureus$q3), c(40, 60, 80))
  expect_equal(aureus$quartile_departments, 5L)
})

test_that("each version of the per-agent table is a table of its own, in the cache and in a calculated dataset", {
  ds <- make_infectious_agent_calc_test_ds()
  groups <- get_infectious_agent_detection_rate_per_agent_table(
    ds, include_quartiles = FALSE)
  ontology <- get_infectious_agent_detection_rate_per_agent_table(
    ds, include_quartiles = FALSE, rows = "ontology")
  expect_false(identical(groups$node_id, ontology$node_id))
  expect_identical(
    get_infectious_agent_detection_rate_per_agent_table(
      ds, include_quartiles = FALSE),
    groups)
  expect_error(
    get_infectious_agent_detection_rate_per_agent_table(ds, rows = "species"),
    class = "rlang_error")

  calculated <- structure(
    list(infectious_agent_detection_rate_per_agent_table = groups,
         infectious_agent_detection_rate_per_agent_ontology_table = ontology),
    class = c("neoipcr_rep_ds", "list"))
  expect_identical(
    get_infectious_agent_detection_rate_per_agent_table(
      calculated, include_quartiles = FALSE, rows = "ontology"),
    ontology)
  expect_identical(
    get_infectious_agent_detection_rate_per_agent_table(
      calculated, include_quartiles = FALSE),
    groups)
})

# --- Resistance tables: golden tests -----------------------------------------
#
# make_resistance_calc_test_ds() describes the 18 infections. Forms 13, 14,
# and 16 name no infectious agent in a slot the form asks for, so they count
# nowhere. Of the others, the list applies 3GCR to Escherichia coli and
# Klebsiella pneumoniae, carbapenem and colistin resistance also to
# Acinetobacter baumannii, MRSA to Staphylococcus aureus and S. argenteus, VRE
# to the enterococci, and every category to "Not listed".

resistance_ds <- make_resistance_calc_test_ds()

test_that("the density of infections with resistant infectious agents counts each infection once per row over all patient-days, its Total of any infectious agent", {
  r <- get_abr_infection_rate_table(resistance_ds, use_cache = FALSE,
                                    include_quartiles = FALSE)

  expect_s3_class(r, "neoipcr_tbl_abr_ir")
  expect_equal(
    names(r),
    c("abr", "level", "rank", "node_id", "parent_id", "group", "n", "total",
      "pooled", "ci_lower", "ci_upper", "n_total_only"))
  expected <- tibble::tribble(
    ~abr,   ~group,                                         ~level, ~n,
    "3gcr", "Total",                                            0L, 7L,
    "3gcr", "Gram-negative",                                    1L, 4L,
    "3gcr", "Aerobic",                                          1L, 1L,
    "3gcr", "Facultatively anaerobic",                          1L, 4L,
    "3gcr", "Bacteria",                                         1L, 5L,
    "3gcr", "Enterobacterales",                                 2L, 3L,
    "3gcr", "Gram-negative bacilli, not Enterobacterales",      2L, 1L,
    "3gcr", "Non-fermenting Gram-negative bacilli",             3L, 1L,
    "car",  "Total",                                            0L, 3L,
    "car",  "Gram-negative",                                    1L, 2L,
    "car",  "Aerobic",                                          1L, 1L,
    "car",  "Facultatively anaerobic",                          1L, 1L,
    "car",  "Bacteria",                                         1L, 2L,
    "car",  "Enterobacterales",                                 2L, 1L,
    "car",  "Gram-negative bacilli, not Enterobacterales",      2L, 1L,
    "car",  "Non-fermenting Gram-negative bacilli",             3L, 1L,
    "cor",  "Total",                                            0L, 2L,
    "cor",  "Gram-negative",                                    1L, 1L,
    "cor",  "Aerobic",                                          1L, 0L,
    "cor",  "Facultatively anaerobic",                          1L, 1L,
    "cor",  "Bacteria",                                         1L, 1L,
    "cor",  "Enterobacterales",                                 2L, 1L,
    "cor",  "Gram-negative bacilli, not Enterobacterales",      2L, 0L,
    "cor",  "Non-fermenting Gram-negative bacilli",             3L, 0L,
    "mrsa", "Total",                                            0L, 2L,
    "mrsa", "Gram-positive",                                    1L, 1L,
    "mrsa", "Facultatively anaerobic",                          1L, 1L,
    "mrsa", "Bacteria",                                         1L, 1L,
    "mrsa", "Gram-positive cocci",                              2L, 1L,
    "vre",  "Total",                                            0L, 1L,
    "vre",  "Gram-positive",                                    1L, 1L,
    "vre",  "Facultatively anaerobic",                          1L, 1L,
    "vre",  "Bacteria",                                         1L, 1L,
    "vre",  "Gram-positive cocci",                              2L, 1L)
  # Pairs of category and row: a category's rows lie on the paths of the
  # infectious agents the list applies it to, so Staphylococcus aureus and
  # Acinetobacter baumannii, recorded 3GCR resistant, a category it applies
  # to neither, give the 3GCR block no row of their own but the rows above
  # them on those paths, and count there, as in the 3GCR Total. A. baumannii
  # alone gives the block the aerobic bacteria and the non-fermenting
  # Gram-negative bacilli with their parent group, since no infectious agent
  # of the dataset to which the list applies 3GCR lies beneath them. "Not
  # listed", "Unidentifiable", and the code the list lacks count in the
  # Totals alone.
  expect_equal(as.character(r$abr), expected$abr)
  expect_equal(r$group, expected$group)
  expect_equal(r$level, expected$level)
  expect_equal(r$n, expected$n)
  expect_true(all(r$total == 200L))
  expect_equal(r$pooled, r$n / 200 * 1000)
  ci <- neoipc_poisson_ci(7, 200, multiplier = 1000)
  expect_equal(c(r$ci_lower[1], r$ci_upper[1]), c(ci$lower, ci$upper))
  ci <- neoipc_poisson_ci(0, 200, multiplier = 1000)
  aerobic <- r[r$abr == "cor" & r$group == "Aerobic", ]
  expect_equal(c(aerobic$ci_lower, aerobic$ci_upper), c(ci$lower, ci$upper))
  totals <- r[r$group == "Total", ]
  expect_true(all(is.na(c(totals$node_id, totals$parent_id, totals$rank))))
  # The infections each Total counts and no row of its block does: forms 10
  # and 12 in the 3GCR block, form 10 in the carbapenem and colistin blocks,
  # and form 11 in the MRSA block.
  expect_equal(totals$n_total_only, c(2L, 1L, 1L, 1L, 0L))
  expect_true(all(is.na(r$n_total_only[r$group != "Total"])))
  expect_equal(
    r$node_id[r$abr == "car"],
    c(NA, "gram_reaction|negative", "oxygen_requirement|aerobic",
      "oxygen_requirement|facultative-anaerobic", "bacteria",
      "enterobacterales", "gram-negative-bacilli",
      "non-fermenting-gram-negative-bacilli"))

  r <- get_abr_infection_rate_table(resistance_ds, use_cache = FALSE,
                                    include_quartiles = FALSE,
                                    rows = "ontology")
  gcr <- r[r$abr == "3gcr", ]
  # A. baumannii's path leaves those of the infectious agents the list
  # applies 3GCR to beneath the Pseudomonadales, its last row there.
  expect_equal(
    gcr$group,
    c("Total", "Bacteria", "Pseudomonadati", "Pseudomonadota",
      "Gammaproteobacteria", "Enterobacterales", "Enterobacteriaceae",
      "Escherichia", "Escherichia coli", "Klebsiella",
      "Klebsiella pneumoniae", "Pseudomonadales"))
  expect_equal(gcr$level, c(0L, 1:8, 7L, 8L, 5L))
  expect_equal(gcr$n, c(7L, 5L, 4L, 4L, 4L, 3L, 3L, 2L, 2L, 1L, 1L, 1L))
  expect_equal(gcr$rank[9], "Species")
  # The genus Enterococcus, recorded above its species, has a residual row in
  # the VRE block, last among its siblings.
  vre <- r[r$abr == "vre", ]
  expect_equal(
    vre$group,
    c("Total", "Bacteria", "Bacillati", "Bacillota", "Bacilli",
      "Lactobacillales", "Enterococcaceae", "Enterococcus",
      "Enterococcus faecalis", "Enterococcus faecium", "Enterococcus n.o.s."))
  expect_equal(vre$node_id[11], "LPSN|genus/enterococcus~n.o.s.")
  expect_equal(vre$n, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L))
  expect_false(any(grepl("^(gram_reaction|oxygen_requirement)\\|",
                         r$node_id)))
})

test_that("a resistance block's Total gives the infections it counts that no row of the block counts, never more than the Total", {
  # By form, each a sepsis:
  #   1. "Not listed" (0) and a code the list lacks (99999999), both 3GCR
  #      resistant: the 3GCR Total counts the infection once, and no row
  #      counts it. "Not listed", to which the list applies MRSA, has a
  #      result for it, so that the department enters the MRSA Total;
  #   2. Escherichia coli (1807) and "Not listed", both 3GCR resistant: the
  #      Total and the rows on E. coli's path count it;
  #   3. Candida albicans (2273), recorded MRSA resistant, a category the
  #      list applies to no fungus: the MRSA Total counts it, and no row of
  #      the MRSA block lies above a fungus.
  ds <- make_sepsis_findings_test_ds(tibble::tribble(
    ~form, ~pathogen_key, ~`3gcr`, ~mrsa,
       1L,            0L,   "yes",  "no",
       1L,     99999999L,   "yes",    NA,
       2L,         1807L,   "yes",    NA,
       2L,            0L,   "yes",    NA,
       3L,         2273L,      NA, "yes"))

  for (rows in c("groups", "ontology")) {
    r <- get_abr_infection_rate_table(ds, use_cache = FALSE,
                                      include_quartiles = FALSE, rows = rows)
    totals <- r[r$group == "Total", ]
    expect_equal(as.character(totals$abr),
                 c("3gcr", "car", "cor", "mrsa", "vre"), info = rows)
    expect_equal(totals$n, c(2L, 0L, 0L, 1L, 0L), info = rows)
    expect_equal(totals$n_total_only, c(1L, 0L, 0L, 1L, 0L), info = rows)
    expect_false(any(r$abr == "mrsa" & r$group != "Total"), info = rows)
    expect_true(all(is.na(r$n_total_only[r$group != "Total"])), info = rows)
  }
})

test_that("a resistance block's Total counts as its own an infection whose department leaves every row that holds it, and nothing of a department that leaves the Total", {
  # Department 1: "Not listed" (0), not 3GCR resistant, which keeps the
  # department in the 3GCR Total; Staphylococcus aureus (934), recorded 3GCR
  # resistant, a category the list does not apply to it, which counts in the
  # rows of the block above it; and Escherichia coli (1807) without a 3GCR
  # result, which takes the department out of every row of the block, each
  # holding E. coli. Department 2: E. coli, not 3GCR resistant. So the Total
  # counts department 1's infection, which no row counts.
  ds <- make_sepsis_findings_test_ds(tibble::tribble(
    ~form, ~pathogen_key, ~`3gcr`,
       1L,            0L,    "no",
       2L,          934L,   "yes",
       3L,         1807L,      NA,
       4L,         1807L,    "no"),
    department_key = c(1L, 1L, 1L, 2L))
  for (rows in c("groups", "ontology")) {
    r <- get_abr_infection_rate_table(ds, use_cache = FALSE, rows = rows)
    gcr <- r[r$abr == "3gcr", ]
    total <- gcr[gcr$group == "Total", ]
    expect_equal(
      c(total$n, total$total, total$quartile_departments, total$n_total_only),
      c(1L, 200L, 2L, 1L), info = rows)
    node_rows <- gcr[gcr$group != "Total", ]
    expect_gt(nrow(node_rows), 0L)
    expect_true(all(node_rows$n == 0L), info = rows)
    expect_true(all(node_rows$total == 100L), info = rows)
    expect_true(all(node_rows$quartile_departments == 1L), info = rows)
  }

  # Department 1: Candida albicans (2273), recorded MRSA resistant, which no
  # row of the MRSA block counts, and S. aureus without an MRSA result, which
  # takes the department out of the MRSA Total. Department 2: S. aureus, not
  # MRSA resistant. The Total leaves department 1's infection out of its count
  # of its own as well.
  ds <- make_sepsis_findings_test_ds(tibble::tribble(
    ~form, ~pathogen_key, ~mrsa,
       1L,         2273L, "yes",
       2L,          934L,    NA,
       3L,          934L,  "no"),
    department_key = c(1L, 1L, 2L))
  for (rows in c("groups", "ontology")) {
    r <- get_abr_infection_rate_table(ds, use_cache = FALSE, rows = rows)
    total <- r[r$abr == "mrsa" & r$group == "Total", ]
    expect_equal(
      c(total$n, total$total, total$quartile_departments, total$n_total_only),
      c(0L, 100L, 1L, 0L), info = rows)
  }
})

test_that("the resistance rates per infectious agent count each infection once per row, with the Tested share over the infectious agents a category applies to", {
  r <- get_infectious_agent_resistance_rate_table(
    resistance_ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_s3_class(r, "neoipcr_tbl_iarr")
  expect_equal(
    names(r),
    c("abr", "level", "rank", "node_id", "parent_id", "group", "n", "total",
      "pooled", "ci_lower", "ci_upper", "tested_n", "tested_total",
      "tested_pooled", "tested_ci_lower", "tested_ci_upper"))
  expected <- tibble::tribble(
    ~abr,   ~group,                                    ~n, ~total, ~tested_n, ~tested_total,
    "3gcr", "Gram-negative",                           4L, 4L,     3L,        4L,
    "3gcr", "Aerobic",                                 1L, 1L,     0L,        0L,
    "3gcr", "Facultatively anaerobic",                 4L, 4L,     3L,        4L,
    "3gcr", "Bacteria",                                5L, 5L,     3L,        4L,
    "3gcr", "Enterobacterales",                        3L, 3L,     3L,        4L,
    "3gcr", "Gram-negative bacilli, not Enterobacterales", 1L, 1L, 0L,       0L,
    "3gcr", "Non-fermenting Gram-negative bacilli",    1L, 1L,     0L,        0L,
    "car",  "Gram-negative",                           2L, 3L,     3L,        5L,
    "car",  "Aerobic",                                 1L, 1L,     1L,        1L,
    "car",  "Facultatively anaerobic",                 1L, 2L,     2L,        4L,
    "car",  "Bacteria",                                2L, 3L,     3L,        5L,
    "car",  "Gram-negative bacilli, not Enterobacterales", 1L, 1L, 1L,       1L,
    "car",  "Non-fermenting Gram-negative bacilli",    1L, 1L,     1L,        1L,
    "car",  "Enterobacterales",                        1L, 2L,     2L,        4L,
    "cor",  "Gram-negative",                           1L, 2L,     2L,        5L,
    "cor",  "Aerobic",                                 0L, 1L,     1L,        1L,
    "cor",  "Facultatively anaerobic",                 1L, 1L,     1L,        4L,
    "cor",  "Bacteria",                                1L, 2L,     2L,        5L,
    "cor",  "Enterobacterales",                        1L, 1L,     1L,        4L,
    "cor",  "Gram-negative bacilli, not Enterobacterales", 0L, 1L, 1L,       1L,
    "cor",  "Non-fermenting Gram-negative bacilli",    0L, 1L,     1L,        1L,
    "mrsa", "Gram-positive",                           1L, 2L,     2L,        2L,
    "mrsa", "Facultatively anaerobic",                 1L, 2L,     2L,        2L,
    "mrsa", "Bacteria",                                1L, 2L,     2L,        2L,
    "mrsa", "Gram-positive cocci",                     1L, 2L,     2L,        2L,
    "vre",  "Gram-positive",                           1L, 2L,     2L,        4L,
    "vre",  "Facultatively anaerobic",                 1L, 2L,     2L,        4L,
    "vre",  "Bacteria",                                1L, 2L,     2L,        4L,
    "vre",  "Gram-positive cocci",                     1L, 2L,     2L,        4L)
  # The rate counts the 3GCR results of Staphylococcus aureus and
  # Acinetobacter baumannii, a category the list does not apply to them, in
  # the rows above them, while the Tested share leaves them out, so that the
  # rows A. baumannii alone gives the 3GCR block have a rate and no Tested
  # share, and Streptococcus agalactiae, to which the list does not apply
  # VRE, out of the Gram-positive cocci's. A parent row counts an infection
  # once however many of its infectious agents it holds (form 15).
  # Carbapenem resistance orders the Bacteria's children by its own rate.
  expect_equal(as.character(r$abr), expected$abr)
  expect_equal(r$group, expected$group)
  expect_equal(r$n, expected$n)
  expect_equal(r$total, expected$total)
  expect_equal(r$tested_n, expected$tested_n)
  expect_equal(r$tested_total, expected$tested_total)
  expect_equal(r$pooled, r$n / r$total * 100)
  expect_equal(r$tested_pooled,
               ifelse(r$tested_total > 0L, r$tested_n / r$tested_total * 100,
                      NA_real_))
  ci <- neoipc_wilson_ci(2, 3)
  car <- r[r$abr == "car" & r$group == "Bacteria", ]
  expect_equal(c(car$ci_lower, car$ci_upper), c(ci$lower, ci$upper) * 100)
  ci <- neoipc_wilson_ci(3, 5)
  expect_equal(c(car$tested_ci_lower, car$tested_ci_upper),
               c(ci$lower, ci$upper) * 100)
  expect_false(any(r$group == "Total"))

  r <- get_infectious_agent_resistance_rate_table(
    resistance_ds, use_cache = FALSE, include_quartiles = FALSE,
    rows = "ontology")
  gcr <- r[r$abr == "3gcr", ]
  expect_equal(
    gcr$group,
    c("Bacteria", "Pseudomonadati", "Pseudomonadota", "Gammaproteobacteria",
      "Enterobacterales", "Enterobacteriaceae", "Escherichia",
      "Escherichia coli", "Klebsiella", "Klebsiella pneumoniae",
      "Pseudomonadales"))
  # A. baumannii's 3GCR result: a rate, and no Tested share.
  pseudomonadales <- gcr[gcr$group == "Pseudomonadales", ]
  expect_equal(
    unlist(pseudomonadales[c("n", "total", "tested_n", "tested_total")],
           use.names = FALSE),
    c(1L, 1L, 0L, 0L))
  expect_true(is.na(pseudomonadales$tested_pooled))
  # Klebsiella pneumoniae, which both slots of form 2 name, counts once:
  # resistant in form 2, not resistant in form 15, untested in form 17.
  pneumoniae <- gcr[gcr$group == "Klebsiella pneumoniae", ]
  expect_equal(
    unlist(pneumoniae[c("n", "total", "tested_n", "tested_total")],
           use.names = FALSE),
    c(1L, 2L, 2L, 3L))
  vre <- r[r$abr == "vre", ]
  expect_equal(
    vre$group[8:10],
    c("Enterococcus faecalis", "Enterococcus faecium", "Enterococcus n.o.s."))
  expect_equal(vre$n[8:10], c(1L, 0L, 0L))
  expect_equal(vre$total[8:10], c(1L, 0L, 1L))
  expect_equal(vre$pooled[8:10], c(100, NA, 0))
  expect_equal(vre$tested_total[8:10], c(2L, 1L, 1L))
})

test_that("the completeness of antimicrobial susceptibility testing counts each detection once, a blank as undocumented, and the conditional rows over the detections recorded resistant", {
  r <- get_resistance_test_rate_table(resistance_ds, use_cache = FALSE,
                                      include_quartiles = FALSE)

  expect_s3_class(r, "neoipcr_tbl_rtr")
  expect_equal(
    paste(r$abr, r$cond),
    c("3gcr routine", "car routine", "car if_3gcr", "cor routine",
      "cor if_3gcr&car", "mrsa routine", "vre routine"))
  # 3GCR: forms 1, 2, and 10, and both of form 15, tested, and form 17 not;
  # the 3GCR results of Staphylococcus aureus and Acinetobacter baumannii, a
  # category the list does not apply to them, and the code the list lacks
  # count on neither side. Carbapenem
  # resistance: blank in forms 1, 15 (K. pneumoniae), and 17. Colistin:
  # "Not tested" in form 1. MRSA: "Not listed" blank, and "Unidentifiable",
  # to which the list applies no category, left out. VRE: "Not tested" in
  # form 5, blank in forms 8 and 10.
  expect_equal(r$n, c(5L, 4L, 3L, 3L, 2L, 2L, 2L))
  expect_equal(r$total, c(6L, 7L, 4L, 7L, 2L, 3L, 5L))
  expect_equal(r$pooled, r$n / r$total * 100)
  ci <- neoipc_wilson_ci(4, 7)
  expect_equal(c(r$ci_lower[2], r$ci_upper[2]), c(ci$lower, ci$upper) * 100)
  # The conditional rows hold only the detections recorded resistant to 3GCR,
  # and for colistin also to carbapenems: Acinetobacter baumannii, to which
  # the list does not apply 3GCR, counts in the plain rows alone.
  r <- get_resistance_test_rate_table(resistance_ds, use_cache = FALSE)
  expect_equal(
    names(r),
    c("abr", "cond", "n", "total", "pooled", "ci_lower", "ci_upper", "q1",
      "q2", "q3", "q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
      "q3_ci_lower", "q3_ci_upper", "quartile_departments"))
  expect_s3_class(r, "neoipcr_tbl_rtr_ref")
})

test_that("a department leaves a category's density quartiles and pooled figures where none of its detections the category applies to has a result", {
  ds <- make_resistance_quartile_test_ds()
  r <- get_abr_infection_rate_table(ds, use_cache = FALSE)
  row <- function(abr, group) r[r$abr == abr & r$group == group, ]

  # 3GCR: departments 1-5 and 8 enter, 1 and 8 at 0; 6 has fewer than 50
  # patient-days and stays in the pooled density, while 7, which recorded no
  # 3GCR result for its Escherichia coli, leaves it with its 100 patient-days.
  total <- row("3gcr", "Total")
  expect_equal(total$n, 14L)
  expect_equal(total$total, 640L)
  expect_equal(total$quartile_departments, 6L)
  expect_equal(c(total$q1, total$q2, total$q3), c(2.5, 15, 27.5))
  expect_equal(
    total[c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
            "q3_ci_lower", "q3_ci_upper")],
    bootstrap_quantile_ci(c(0L, 1L, 2L, 3L, 4L, 0L), rep(100L, 6L),
                          type = "poisson", multiplier = 1000),
    ignore_attr = TRUE)
  entero <- row("3gcr", "Enterobacterales")
  expect_equal(entero$quartile_departments, 6L)
  expect_equal(entero$total, 640L)
  expect_equal(c(entero$q1, entero$q2, entero$q3), c(2.5, 15, 27.5))
  # Carbapenem resistance applies to every department's E. coli, whose
  # results are blank; department 8, without such detections, alone enters,
  # its 100 patient-days alone counting in the pooled density.
  expect_equal(row("car", "Total")$quartile_departments, 1L)
  expect_equal(row("car", "Total")$total, 100L)
  # MRSA: department 8's S. aureus has no result, and the departments without
  # such detections enter at 0, so that the pooled rate of 0 shows none.
  mrsa <- row("mrsa", "Total")
  expect_equal(mrsa$quartile_departments, 6L)
  expect_true(all(is.na(c(mrsa$q1, mrsa$q2, mrsa$q3, mrsa$q1_ci_lower))))
})

test_that("a department leaves a density row's quartiles and pooled figures by its testing of the row's own infectious agents, and enters at 0 without them", {
  # 3 departments of 100 patient-days, each infection a sepsis:
  #   1. Escherichia coli (1807), 3GCR resistant, and Klebsiella pneumoniae
  #      (390), 3GCR blank;
  #   2. E. coli, 3GCR not resistant;
  #   3. "Not listed" (0), 3GCR blank, its only detection.
  ds <- make_sepsis_findings_test_ds(
    tibble::tribble(
      ~form, ~pathogen_key, ~`3gcr`,
         1L,         1807L,   "yes",
         2L,          390L,      NA,
         3L,         1807L,    "no",
         4L,            0L,      NA),
    department_key = c(1L, 1L, 2L, 3L))
  r <- get_abr_infection_rate_table(ds, use_cache = FALSE, rows = "ontology")
  row <- function(group) r[r$abr == "3gcr" & r$group == group, ]
  figures <- function(row) c(row$n, row$total, row$quartile_departments)

  # Department 1 tested its E. coli, so it enters E. coli's rows and their
  # parents; department 3 has no E. coli and enters at 0.
  expect_equal(figures(row("Escherichia coli")), c(1L, 300L, 3L))
  expect_equal(figures(row("Enterobacterales")), c(1L, 300L, 3L))
  # Department 1 tested none of its K. pneumoniae, so it leaves the rows of
  # Klebsiella, quartiles and pooled density alike, which departments 2 and 3,
  # without K. pneumoniae, enter at 0.
  ci <- neoipc_poisson_ci(0, 200, multiplier = 1000)
  for (group in c("Klebsiella", "Klebsiella pneumoniae")) {
    k <- row(group)
    expect_equal(figures(k), c(0L, 200L, 2L), info = group)
    expect_equal(k$pooled, 0, info = group)
    expect_equal(c(k$ci_lower, k$ci_upper), c(ci$lower, ci$upper),
                 info = group)
  }
  # The Total tests all of a department's detections to which 3GCR applies:
  # department 3's "Not listed" has no result, so it leaves the Total, and
  # only the Total.
  expect_equal(figures(row("Total")), c(1L, 200L, 2L))

  r <- get_abr_infection_rate_table(ds, use_cache = FALSE, rows = "groups")
  gcr <- r[r$abr == "3gcr", ]
  expect_equal(figures(gcr[gcr$group == "Total", ]), c(1L, 200L, 2L))
  expect_equal(figures(gcr[gcr$group == "Enterobacterales", ]),
               c(1L, 300L, 3L))
})

test_that("the resistance rates per infectious agent and the testing's completeness take the departments with 5 infections or detections in a row's denominator", {
  ds <- make_resistance_quartile_test_ds()

  # Departments 1-5 have 5 tested infections each, 0 to 4 resistant; 6 has 4,
  # and 7 none tested.
  r <- get_infectious_agent_resistance_rate_table(ds, use_cache = FALSE)
  entero <- r[r$abr == "3gcr" & r$group == "Enterobacterales", ]
  expect_equal(entero$pooled, 14 / 29 * 100)
  expect_equal(entero$quartile_departments, 5L)
  expect_equal(c(entero$q1, entero$q2, entero$q3), c(20, 40, 60))
  expect_equal(
    entero[c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
             "q3_ci_lower", "q3_ci_upper")],
    bootstrap_quantile_ci(0:4, rep(5L, 5L), type = "binomial",
                          multiplier = 100),
    ignore_attr = TRUE)
  # The Tested share has no quartiles.
  expect_equal(entero$tested_pooled, 29 / 34 * 100)
  expect_false(any(grepl("^tested_q", names(r))))

  # Department 7 enters the 3GCR testing at 0, with 5 detections and no
  # result; department 6, with 4, does not.
  r <- get_resistance_test_rate_table(ds, use_cache = FALSE)
  gcr <- r[r$abr == "3gcr" & r$cond == "routine", ]
  expect_equal(gcr$pooled, 29 / 34 * 100)
  expect_equal(gcr$quartile_departments, 6L)
  expect_equal(c(gcr$q1, gcr$q2, gcr$q3), c(100, 100, 100))
  expect_equal(
    gcr$q1_ci_lower,
    bootstrap_quantile_ci(c(5L, 5L, 5L, 5L, 5L, 0L), rep(5L, 6L),
                          type = "binomial", multiplier = 100)$q1_ci_lower)
  # No department has 5 detections recorded 3GCR resistant.
  conditional <- r[r$abr == "car" & r$cond == "if_3gcr", ]
  expect_equal(conditional$total, 14L)
  expect_equal(conditional$quartile_departments, 0L)
})

# --- The rows a dataset has ---------------------------------------------------

test_that("a resistance block has the rows on the paths of the infectious agents the list applies its category to that lie at or above a detection it applies to or with a result in it", {
  # The department's only 3GCR result: Staphylococcus aureus (934) recorded
  # resistant, a category the list does not apply to it. Of its path, the
  # 3GCR block has the rows that lie on the paths of the infectious agents
  # the list applies 3GCR to: the Bacteria, and over the reporting groups the
  # facultative anaerobes, but not the Gram-positive bacteria, to none of
  # which the list applies it.
  aureus <- tibble::tibble(form = 1L, pathogen_key = 934L, `3gcr` = "yes")
  coli <- tibble::tibble(form = 2L, pathogen_key = 1807L, `3gcr` = "no")
  alone <- make_sepsis_findings_test_ds(aureus)
  beside <- make_sepsis_findings_test_ds(dplyr::bind_rows(aureus, coli))
  expected <- list(
    groups = c("oxygen_requirement|facultative-anaerobic", "bacteria"),
    ontology = "LPSN|domain/bacteria")
  block <- function(t) t[t$abr == "3gcr" & !is.na(t$node_id), ]

  for (rows in names(expected)) {
    tables <- function(ds) list(
      density = block(get_abr_infection_rate_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows)),
      rates = block(get_infectious_agent_resistance_rate_table(
        ds, use_cache = FALSE, include_quartiles = FALSE, rows = rows)))
    ids <- expected[[rows]]
    ones <- rep(1L, length(ids))

    t <- tables(alone)
    expect_identical(t$density$node_id, ids, label = rows)
    expect_identical(t$density$n, ones, label = rows)
    expect_true(all(t$density$total == 100L), label = rows)
    expect_identical(t$rates$node_id, ids, label = rows)
    expect_identical(t$rates$n, ones, label = rows)
    expect_identical(t$rates$total, ones, label = rows)
    expect_equal(t$rates$pooled, rep(100, length(ids)), label = rows)
    # The Tested share takes the infectious agents the list applies 3GCR to
    # alone, none here.
    expect_identical(t$rates$tested_n, ones * 0L, label = rows)
    expect_identical(t$rates$tested_total, ones * 0L, label = rows)
    expect_true(all(is.na(t$rates$tested_pooled)), label = rows)

    # A susceptible Escherichia coli (1807) beside it adds its own rows and
    # leaves the infections recorded resistant on these as they are.
    t <- tables(beside)
    density <- t$density[match(ids, t$density$node_id), ]
    expect_identical(density$n, ones, label = rows)
    rates <- t$rates[match(ids, t$rates$node_id), ]
    expect_identical(rates$n, ones, label = rows)
    expect_identical(rates$total, ones * 2L, label = rows)
    expect_identical(rates$tested_n, ones, label = rows)
    expect_identical(rates$tested_total, ones, label = rows)
    expect_true(all(nodes_of_only(rows, 1807L, 934L) %in% t$density$node_id),
                label = rows)
  }
})

test_that("a residual row counts the infectious agents recorded at a node with nodes beneath it in the full ontology, whatever else the dataset holds", {
  # Sepsis with the genus Staphylococcus (481), recorded 3GCR resistant, a
  # category the list applies to no staphylococcus; Escherichia coli (1807),
  # not 3GCR, so that the department enters the 3GCR block's rows above it;
  # and the genus Enterococcus (503), not VRE. No infectious agent of the
  # dataset is recorded beneath either genus, and E. coli, a species, has no
  # node beneath it.
  ds <- make_sepsis_findings_test_ds(tibble::tibble(
    form = 1:3,
    pathogen_key = c(481L, 1807L, 503L),
    `3gcr` = c("yes", "no", NA),
    vre = c(NA, NA, "no")))
  residual <- function(t) t[endsWith(dplyr::coalesce(t$node_id, ""), "~n.o.s."), ]

  detected <- get_infectious_agent_detection_rate_per_agent_table(
    ds, use_cache = FALSE, include_quartiles = FALSE, rows = "ontology")
  r <- residual(detected)
  expect_setequal(r$node_id, c("LPSN|genus/staphylococcus~n.o.s.",
                               "LPSN|genus/enterococcus~n.o.s."))
  r <- r[order(r$node_id), ]
  expect_identical(r$parent_id,
                   c("LPSN|genus/enterococcus", "LPSN|genus/staphylococcus"))
  expect_identical(r$group, c("Enterococcus n.o.s.", "Staphylococcus n.o.s."))
  expect_identical(r$n, c(1L, 1L))
  expect_identical(r$total, c(3L, 3L))
  genus <- detected[detected$node_id %in% r$parent_id, ]
  expect_identical(r$level, genus$level[match(r$parent_id, genus$node_id)] + 1L)
  # The reporting groups have no residual rows.
  expect_equal(nrow(residual(get_infectious_agent_detection_rate_per_agent_table(
    ds, use_cache = FALSE, include_quartiles = FALSE))), 0L)

  # In a resistance block, the genus Enterococcus has a residual row, its
  # species being on the paths of the infectious agents the list applies VRE
  # to; the genus Staphylococcus, on none of the paths of those it applies
  # 3GCR to, has neither a row nor a residual row in the 3GCR block, and its
  # 3GCR result counts in the Bacteria row.
  density <- get_abr_infection_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE, rows = "ontology")
  rates <- get_infectious_agent_resistance_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE, rows = "ontology")
  vre <- residual(density[density$abr == "vre", ])
  expect_identical(vre$node_id, "LPSN|genus/enterococcus~n.o.s.")
  expect_identical(vre$n, 0L)
  vre <- residual(rates[rates$abr == "vre", ])
  expect_identical(vre$node_id, "LPSN|genus/enterococcus~n.o.s.")
  expect_identical(c(vre$n, vre$total, vre$tested_n, vre$tested_total),
                   c(0L, 1L, 1L, 1L))
  for (t in list(density, rates)) {
    gcr <- t[t$abr == "3gcr", ]
    expect_false(any(grepl("staphylococc", gcr$node_id)))
    expect_equal(nrow(residual(gcr)), 0L)
    expect_identical(gcr$n[gcr$node_id %in% "LPSN|domain/bacteria"], 1L)
  }
})

test_that("each version of the resistance tables is a table of its own, in the cache and in a calculated dataset", {
  ds <- make_resistance_calc_test_ds()
  for (build in list(get_abr_infection_rate_table,
                     get_infectious_agent_resistance_rate_table)) {
    groups <- build(ds, include_quartiles = FALSE)
    ontology <- build(ds, include_quartiles = FALSE, rows = "ontology")
    expect_false(identical(groups$node_id, ontology$node_id))
    expect_identical(build(ds, include_quartiles = FALSE), groups)
    expect_error(build(ds, rows = "species"), class = "rlang_error")
  }

  # The fixture's findings hold a code the list lacks.
  expect_warning(
    calculated <- calculate_department_data(ds, use_cache = FALSE),
    class = "neoipcr_unknown_infectious_agent_code")
  expect_identical(
    get_abr_infection_rate_table(calculated, include_quartiles = FALSE,
                                 rows = "ontology"),
    calculated$abr_infection_rate_ontology_table)
  expect_identical(
    get_infectious_agent_resistance_rate_table(
      calculated, include_quartiles = FALSE, rows = "ontology"),
    calculated$infectious_agent_resistance_rate_ontology_table)
  expect_identical(
    get_infectious_agent_resistance_rate_table(
      calculated, include_quartiles = FALSE),
    calculated$infectious_agent_resistance_rate_table)
})

test_that("the secondary-BSI table shows each type's share of infections followed up, with its counts", {
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("hap", "hap", "hap", "nec"),
      sec_bsi        = c("1", "0", "-1", "-1")),
    findings = tibble::tibble(
      form = 1L, secondary_bsi = TRUE, pathogen_key = 1L))

  r <- get_secondary_bsi_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)

  hap <- r[r$event_type_key == "hap", ]
  expect_equal(hap$n, 1L)
  expect_equal(hap$pooled, 50)
  expect_equal(hap$followup_n, 2L)
  expect_equal(hap$total_n, 3L)
  expect_equal(hap$followup_pooled, 200 / 3)
  nec <- r[r$event_type_key == "nec", ]
  expect_equal(nec$followup_n, 0L)
  expect_equal(nec$total_n, 1L)
  expect_equal(nec$followup_pooled, 0)
  ssi <- r[r$event_type_key == "ssi", ]
  expect_equal(ssi$total_n, 0L)
})

test_that("the reference secondary-BSI table gives the quartiles of the departments' follow-up shares", {
  # Departments 1-5 each have 5 pneumonias, 5 to 1 of them followed up;
  # department 6 has 4, none followed up, too few to enter the quartiles.
  shares <- tibble::tibble(
    department_key = c(rep(1:5, each = 5), rep(6L, 4)),
    sec_bsi        = c(
      unlist(lapply(5:1, \(k) c(rep("0", k), rep("-1", 5 - k)))),
      rep("-1", 4)))
  ds <- make_infection_calc_test_ds(
    forms = dplyr::mutate(shares, event_type_key = "hap"))

  r <- get_secondary_bsi_rate_table(ds, use_cache = TRUE)
  hap <- r[r$event_type_key == "hap", ]
  expect_equal(hap$followup_pooled, 15 / 29 * 100)
  expect_equal(hap$followup_q1, 40)
  expect_equal(hap$followup_q2, 60)
  expect_equal(hap$followup_q3, 80)
  # A table without quartiles leaves those of the follow-up shares out too.
  expect_false(any(c("followup_q1", "followup_q2", "followup_q3") %in% names(
    get_secondary_bsi_rate_table(ds, use_cache = TRUE, include_quartiles = FALSE))))

  # With 4 departments of at least 5 pneumonias there are no quartiles.
  ds <- make_infection_calc_test_ds(
    forms = dplyr::mutate(
      dplyr::filter(shares, .data$department_key != 5L),
      event_type_key = "hap",
      department_key = dplyr::if_else(.data$department_key == 6L, 5L,
                                      .data$department_key)))
  r <- get_secondary_bsi_rate_table(ds, use_cache = FALSE)
  hap <- r[r$event_type_key == "hap", ]
  expect_true(all(is.na(c(hap$followup_q1, hap$followup_q2, hap$followup_q3))))
})

test_that("the secondary-BSI rate's quartiles take only the departments that followed up", {
  # Departments 1-4 each followed up 2 NECs, both with a secondary BSI;
  # department 5 followed up none of its 2, so 4 departments remain, too few
  # for quartiles, although the gate would pass for 5.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "nec",
      department_key = rep(1:5, each = 2),
      sec_bsi        = c(rep("1", 8), "-1", "-1")),
    findings = tibble::tibble(
      form = 1:8, secondary_bsi = TRUE, pathogen_key = 1L))

  r <- get_secondary_bsi_rate_table(ds, use_cache = FALSE)
  nec <- r[r$event_type_key == "nec", ]

  expect_equal(nec$n, 8L)
  expect_equal(nec$pooled, 100)
  expect_true(all(is.na(c(nec$q1, nec$q2, nec$q3))))
})
