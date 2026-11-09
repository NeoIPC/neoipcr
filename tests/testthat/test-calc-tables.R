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

test_that("the infectious-agent table builders refuse pseudonymized events, and the others take them", {
  # The findings carry `pathogen_key` and `secondary_bsi` only under the full
  # event tier; the fixture's tibbles are full, so only the precondition can
  # refuse the dataset.
  ds <- calc_ds
  ds$metadata$dataset_options$include_event <- "pseudo"
  needs_full_events <- c(
    "get_infectious_agent_detection_rate_per_inf_type_table",
    "get_infectious_agent_detection_rate_per_agent_table",
    "get_abr_infection_rate_table",
    "get_organism_resistance_rate_table",
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
  # 2 rows: overall + to_be_categorized (PZX.AA.JA not in ICHI list)
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
  expect_equal(result$group[1], "Total")
  expect_equal(result$n[1], 2L)
  expect_equal(result$pooled[1], 100)
})

test_that("abr_infection_rate_table has 5 ABR types, all zero", {
  result <- get_abr_infection_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  # 5 rows: 3gcr, car, cor, mrsa, vre (all resistance = "no")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$n == 0L))
  expect_true(all(result$pooled == 0))
})

test_that("organism_resistance_rate_table rows are all zero", {
  result <- get_organism_resistance_rate_table(calc_ds, use_cache = FALSE,
    include_quartiles = FALSE)
  expect_true(nrow(result) > 0L)
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
  organism_resistance = get_organism_resistance_rate_table(
    ds, use_cache = FALSE, include_quartiles = FALSE),
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

  # The organism-resistance table joins the classification on `pathogen_key`,
  # which drops a finding without one by itself, so its comparison guards a
  # change of that join rather than this rule.
  expect_equal(infectious_agent_tables(variant), infectious_agent_tables(base))
})

test_that("pneumonias neither tested nor followed up for a secondary BSI leave the per-type denominator", {
  # Forms 1-3 can have no infectious agent recorded: not tested, or with the
  # required test left blank, and not followed up; form 1 holds one in the
  # slots the form does not ask for. Forms 4 and 5 are followed up or tested
  # negative, form 6 tested positive with an infectious agent.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = "hap",
      microbiological_test_result = c("-1", "-1", NA, "-1", "0", "1"),
      sec_bsi                     = c("-1", NA, "-1", "0", "-1", "-1")),
    findings = tibble::tibble(
      form = c(1L, 6L), secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  hap <- r[r$inf == "hap", ]

  expect_equal(hap$n, 1L)
  expect_equal(hap$pooled, 100 / 3)
  # The row of all types keeps every infection and every slot.
  expect_equal(r$n[r$inf == "all"], 2L)
  expect_equal(r$pooled[r$inf == "all"], 100 / 3)
})

test_that("SSIs neither tested at their depth nor followed up for a secondary BSI leave the per-type denominator", {
  # Forms 1-4 can have no infectious agent recorded: their culture at the
  # depth the infection type records is not tested (form 1), left blank
  # (form 2), tested only at another depth (form 3), or no depth is recorded
  # (form 4), and none is followed up. Form 5 is followed up, form 6 tested
  # negative at its depth, form 7 positive with an infectious agent.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key   = "ssi",
      infection_type   = c("1", "2", "3", NA, "1", "2", "3"),
      organisms_superf = c("-1", NA, NA, "1", "-1", NA, NA),
      organisms_deep   = c(NA, NA, "1", "1", NA, "0", NA),
      organisms_organ  = c(NA, NA, "-1", "1", NA, NA, "1"),
      sec_bsi          = c("-1", "-1", "-1", "-1", "0", "-1", "-1")),
    findings = tibble::tibble(
      form = 7L, secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  ssi <- r[r$inf == "ssi", ]

  expect_equal(ssi$n, 1L)
  expect_equal(ssi$pooled, 100 / 3)
})

test_that("NECs not followed up for a secondary BSI leave the per-type denominator", {
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "nec",
      sec_bsi        = c("-1", NA, "0", "1")),
    findings = tibble::tibble(
      form = 4L, secondary_bsi = TRUE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  nec <- r[r$inf == "nec", ]

  expect_equal(nec$n, 1L)
  expect_equal(nec$pooled, 50)
})

test_that("every primary sepsis/BSI stays in the per-type denominator, culture-negative or not", {
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "bsi",
      no_pos_culture = c(TRUE, NA, NA)),
    findings = tibble::tibble(
      form = 3L, secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  bsi <- r[r$inf == "bsi", ]

  expect_equal(bsi$n, 1L)
  expect_equal(bsi$pooled, 100 / 3)
})

test_that("the per-type numerator counts a slot only where the form asks for it", {
  # Every form is in its type's denominator. An infectious agent counts in
  # pneumonia 4's secondary-BSI slot (answered Yes), in SSI 2's slots (a depth
  # tested positive, if not its own), and in sepsis 2's; it stays in a slot
  # the form does not ask for on the others: a pneumonia's own slots unless
  # its test is positive, its secondary-BSI slots unless the follow-up is
  # answered Yes, an SSI's own slots unless a depth is positive, a NEC's
  # secondary-BSI slots answered No, and the slots of a sepsis recorded
  # culture-negative.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c("hap", "hap", "hap", "hap",
                                      "ssi", "ssi", "nec", "bsi", "bsi"),
      microbiological_test_result = c("-1", "0", "1", "-1",
                                      NA, NA, NA, NA, NA),
      sec_bsi                     = c("0", "0", "0", "1",
                                      "0", "-1", "0", NA, NA),
      infection_type              = c(NA, NA, NA, NA, "1", "1", NA, NA, NA),
      organisms_superf            = c(NA, NA, NA, NA, "0", "0", NA, NA, NA),
      organisms_deep              = c(NA, NA, NA, NA, "0", "1", NA, NA, NA),
      organisms_organ             = c(NA, NA, NA, NA, "0", NA, NA, NA, NA),
      no_pos_culture              = c(NA, NA, NA, NA, NA, NA, NA, TRUE, NA)),
    findings = tibble::tibble(
      form          = 1:9,
      secondary_bsi = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE,
                        FALSE, FALSE),
      pathogen_key  = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_equal(r$n[r$inf == "hap"], 1L)
  expect_equal(r$pooled[r$inf == "hap"], 25)
  expect_equal(r$n[r$inf == "ssi"], 1L)
  expect_equal(r$pooled[r$inf == "ssi"], 50)
  expect_equal(r$n[r$inf == "nec"], 0L)
  expect_equal(r$pooled[r$inf == "nec"], 0)
  expect_equal(r$n[r$inf == "bsi"], 1L)
  expect_equal(r$pooled[r$inf == "bsi"], 50)
  # The row of all types counts every infection whose slots name an
  # infectious agent, whatever the form asks for.
  expect_equal(r$n[r$inf == "all"], 9L)
  expect_equal(r$pooled[r$inf == "all"], 100)
  expect_true(all(r$pooled <= 100, na.rm = TRUE))
})

test_that("an SSI not tested at its own depth counts in no numerator, whatever its slots hold", {
  # SSI 1 is superficial, not tested there, and not followed up, so it cannot
  # have an infectious agent recorded; a positive deep culture still lets the
  # form show its slots, which name one. SSI 2 is tested negative at its
  # depth.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key   = "ssi",
      infection_type   = c("1", "1"),
      organisms_superf = c("-1", "0"),
      organisms_deep   = c("1", NA),
      organisms_organ  = c(NA, NA),
      sec_bsi          = c("-1", "-1")),
    findings = tibble::tibble(
      form = 1L, secondary_bsi = FALSE, pathogen_key = 1L))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)
  ssi <- r[r$inf == "ssi", ]

  expect_equal(ssi$n, 0L)
  expect_equal(ssi$pooled, 0)
})

test_that("the per-type numerator counts \"Not listed\", \"Unidentifiable\", and a code the list lacks", {
  # Sepsis 1-3 and NECs 4-6 answered Yes name "Not listed" (0),
  # "Unidentifiable" (3561), and a code the list lacks, in their own and their
  # secondary-BSI slots.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = rep(c("bsi", "nec"), each = 3),
      sec_bsi        = rep(c(NA, "1"), each = 3)),
    findings = tibble::tibble(
      form          = 1:6,
      secondary_bsi = rep(c(FALSE, TRUE), each = 3),
      pathogen_key  = rep(c(0L, 3561L, 99999999L), 2)))

  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE, include_quartiles = FALSE)

  expect_equal(r$n[r$inf == "bsi"], 3L)
  expect_equal(r$pooled[r$inf == "bsi"], 100)
  expect_equal(r$n[r$inf == "nec"], 3L)
  expect_equal(r$pooled[r$inf == "nec"], 100)
})

test_that("the reference per-type table takes the departments' quartiles and its gate over the same denominators", {
  # Each of departments 1-5 has 4 pneumonias tested, 1 to 4 of them naming
  # an infectious agent (department 5 like department 4), and 4 neither
  # tested nor followed up, which cannot have one recorded.
  tested <- tibble::tibble(
    department_key              = rep(1:5, each = 4),
    microbiological_test_result = "1",
    sec_bsi                     = "-1")
  untested <- tibble::tibble(
    department_key              = rep(1:5, each = 4),
    microbiological_test_result = "-1",
    sec_bsi                     = "-1")
  named <- c(1L, 2L, 3L, 4L, 4L)
  agents <- function(departments)
    tibble::tibble(
      form = unlist(lapply(seq_along(departments), \(i)
        (i - 1L) * 4L + seq_len(named[departments[i]]))),
      secondary_bsi = FALSE,
      pathogen_key  = 1L)

  ds <- make_infection_calc_test_ds(
    forms    = dplyr::mutate(dplyr::bind_rows(tested, untested),
                             event_type_key = "hap"),
    findings = agents(1:5))
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE)
  hap <- r[r$inf == "hap", ]
  # The departments' shares are 25, 50, 75, 100, and 100%; the pooled share
  # of 70% passes the gate against a median of 4 pneumonias per department.
  expect_equal(c(hap$q1, hap$q2, hap$q3), c(50, 75, 100))

  # Department 5 with only the pneumonias it cannot have an infectious agent
  # recorded for leaves 4 departments, too few for quartiles.
  ds <- make_infection_calc_test_ds(
    forms    = dplyr::mutate(
      dplyr::bind_rows(dplyr::filter(tested, .data$department_key != 5L),
                       untested),
      event_type_key = "hap"),
    findings = agents(1:4))
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = FALSE)
  hap <- r[r$inf == "hap", ]
  expect_true(all(is.na(c(hap$q1, hap$q2, hap$q3))))
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
