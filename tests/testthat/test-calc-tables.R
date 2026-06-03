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
  list(name = "get_antibiotic_utilisation_table",
       fn = get_antibiotic_utilisation_table, has_q = TRUE),
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
  list(name = "get_abr_infection_rate_table",
       fn = get_abr_infection_rate_table, has_q = TRUE),
  list(name = "get_organism_resistance_rate_table",
       fn = get_organism_resistance_rate_table, has_q = TRUE),
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

test_that("antibiotic_utilisation_table substance days match fixture", {
  result <- get_antibiotic_utilisation_table(calc_ds, use_cache = FALSE,
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
  # 2 rows: overall + to_be_categorised (PZX.AA.JA not in ICHI list)
  expect_equal(nrow(result), 2L)
  overall <- result[result$pro_cat == "overall", ]
  expect_equal(overall$n, 1L)
  # rate = 1 / 3 patients * 100
  expect_equal(overall$pooled, 1 / 3 * 100, tolerance = 0.01)
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
  # 5 levels: all, bsi, hap, nec, ssi
  expect_equal(nrow(result), 5L)
  all_row <- result[result$inf == "all", ]
  # 2 infections, both with pathogens = 100%
  expect_equal(all_row$n, 2L)
  expect_equal(all_row$pooled, 100)
})

test_that("infectious_agent_detection_per_agent has Total row", {
  result <- get_infectious_agent_detection_rate_per_agent_table(
    calc_ds, use_cache = FALSE, include_quartiles = FALSE)
  # Only 1 row (Total) — pathogens map to unknown taxonomy
  expect_equal(nrow(result), 1L)
  expect_equal(result$group, "Total")
  expect_equal(result$n, 2L)
  expect_equal(result$pooled, 100)
})

test_that("abr_infection_rate_table has 5 ABR types, all zero", {
  result <- get_abr_infection_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 5 rows: 3gcr, car, cor, mrsa, vre (all resistance = "no")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$n == 0L))
  expect_true(all(result$pooled == 0))
})

test_that("organism_resistance_rate_table has 10 rows, all zero", {
  result <- get_organism_resistance_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 2 rows per ABR type (genus total + nos): 5 × 2 = 10
  expect_equal(nrow(result), 10L)
  expect_true(all(result$n == 0L))
})

test_that("resistance_test_rate_table has 7 rows with expected rates", {
  result <- get_resistance_test_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 5 routine + 2 conditional = 7
  expect_equal(nrow(result), 7L)
  routine <- result[result$cond == "routine", ]
  # All 2 IAF entries have resistance != "not_tested" → tested = 2, rate = 100
  expect_true(all(routine$n == 2L))
  expect_true(all(routine$pooled == 100))
  # Conditional rows: n = 0 (no 3gcr = "yes" findings)
  conditional <- result[result$cond != "routine", ]
  expect_true(all(conditional$n == 0L))
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

# --- get_infectious_agent_detection_rates: cross_join branch ---
#
# The cross_join branch fires when group_cols is non-NULL but every column in
# group_cols is itself a column of infectiousAgentFindings or the pathogen
# taxonomy (so inf_groups is empty after the setdiff). In that case there is
# no shared key between the pathogen-grouped `r` and infection-count
# `inf_counts`, and cross_join is the right operation.

test_that("get_infectious_agent_detection_rates cross_joins when group_cols are all in findings/taxonomy", {
  # `pathogen_key` is a column of infectiousAgentFindings; `domain` is a
  # column of the pathogen taxonomy. Both → inf_groups is empty.
  r <- neoipcr:::get_infectious_agent_detection_rates(
    calc_ds, group_cols = c("pathogen_key", "domain"))
  expect_s3_class(r, "tbl_df")
  expect_true(nrow(r) > 0L)
  expect_true(all(c("pathogen_key", "domain", "n",
                    "inf_with_pathogen", "total_inf",
                    "n_per_iwp", "n_per_t", "iwp_per_t") %in% names(r)))
  # Every row has the same (inf_with_pathogen, total_inf) pair because the
  # cross_join replicates a single-row inf_counts across pathogen groups.
  expect_equal(length(unique(r$total_inf)), 1L)
})
