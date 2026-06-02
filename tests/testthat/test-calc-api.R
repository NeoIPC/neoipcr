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
    "usage_density_rate_table", "antibiotic_utilisation_table",
    "surgery_rate_table", "incidence_density_rate_table",
    "dev_ass_incidence_density_rate_table",
    "infectious_agent_detection_rate_per_agent_table",
    "abr_infection_rate_table", "organism_resistance_rate_table",
    "secondary_bsi_rate_table",
    "infectious_agent_detection_rate_per_inf_type_table",
    "resistance_test_rate_table")
  expect_true(all(expected_slots %in% names(result)))
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
