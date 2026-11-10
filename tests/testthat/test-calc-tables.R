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
  # 4 rows: all, bsi, hap, ssi
  expect_equal(as.character(result$inf), c("all", "bsi", "hap", "ssi"))
  all_row <- result[result$inf == "all", ]
  # 2 infections, both with pathogens = 100%
  expect_equal(all_row$n, 2L)
  expect_equal(all_row$pooled, 100)
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
  # The row of all types keeps every infection and every slot.
  expect_equal(r$n[r$inf == "all"], 2L)
  expect_equal(r$tested[r$inf == "all"], 5L)
  expect_equal(r$pooled[r$inf == "all"], 40)
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

test_that("the table has no NEC row, and its row of all types counts every infection whatever the form asks for", {
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

  expect_equal(as.character(r$inf), c("all", "bsi", "hap", "ssi"))
  all_types <- r[r$inf == "all", ]
  expect_equal(all_types$n, 4L)
  expect_equal(all_types$tested, 5L)
  expect_equal(all_types$total, 5L)
  expect_equal(all_types$pooled, 80)
  ci <- neoipc_wilson_ci(4L, 5L)
  expect_equal(c(all_types$ci_lower, all_types$ci_upper),
               c(ci$lower, ci$upper) * 100)
  expect_true(all(is.na(c(all_types$tested_pooled, all_types$tested_ci_lower,
                          all_types$tested_ci_upper))))
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
  # The row of all types counts every infection whose slots name an
  # infectious agent, whatever the form asks for.
  expect_equal(r$n[r$inf == "all"], 6L)
  expect_equal(r$pooled[r$inf == "all"], 600 / 9)
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
  expect_equal(r$n[r$inf == "all"], 12L)
  expect_equal(r$pooled[r$inf == "all"], 100)
})

tested_quartile_cols <- c(
  "tested_q1", "tested_q2", "tested_q3",
  "tested_q1_ci_lower", "tested_q1_ci_upper",
  "tested_q2_ci_lower", "tested_q2_ci_upper",
  "tested_q3_ci_lower", "tested_q3_ci_upper")

test_that("the reference per-type table gives each share the departments' quartiles over its own denominator", {
  # Each department has 8 pneumonias: `tested[d]` of department d are tested,
  # `positive[d]` of these positive with an infectious agent in an own slot,
  # and every pneumonia is followed up for a secondary BSI and answered No.
  # The departments take the keys 1, 2, 4, 5, and so on: department 3 has no
  # infections and enters no quartiles.
  departments <- function(tested, positive) {
    forms <- dplyr::bind_rows(lapply(seq_along(tested), \(d) tibble::tibble(
      department_key              = d + (d >= 3L),
      microbiological_test_result = rep(
        c("1", "0", "-1"),
        c(positive[d], tested[d] - positive[d], 8L - tested[d])),
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
  intervals <- function(row)
    unlist(row[c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
                 "q3_ci_lower", "q3_ci_upper")], use.names = FALSE)
  binomial_intervals <- function(events, denominators)
    unlist(bootstrap_quantile_ci(events, denominators, type = "binomial",
                                 multiplier = 100), use.names = FALSE)

  # Departments 1-5 test 25, 50, 75, 100, and 100% of their pneumonias, the
  # pooled 70% passing the gate against a median of 8 pneumonias, and find
  # an infectious agent in 50, 25, 50, 25, and 100% of the tested, the pooled
  # 15 of 28 passing it against a median of 6 tested pneumonias.
  tested   <- c(2L, 4L, 6L, 8L, 8L)
  positive <- c(1L, 1L, 3L, 2L, 8L)
  ds <- departments(tested, positive)
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    ds, use_cache = TRUE)
  hap <- r[r$inf == "hap", ]
  expect_equal(quartiles(hap, "tested_"), c(50, 75, 100))
  expect_equal(unlist(hap[tested_quartile_cols[4:9]], use.names = FALSE),
               binomial_intervals(tested, rep(8L, 5)))
  expect_equal(quartiles(hap), c(25, 50, 50))
  expect_equal(intervals(hap), binomial_intervals(positive, tested))
  # The row of all types takes each department's share of every infection in
  # which a slot names an infectious agent: 1, 1, 3, 2, and 8 of 8, the
  # pooled 15 of 40 passing the gate against a median of 8 infections.
  all_types <- r[r$inf == "all", ]
  expect_equal(quartiles(all_types), c(12.5, 25, 37.5))
  expect_equal(intervals(all_types), binomial_intervals(positive, rep(8L, 5)))
  # Sepsis/BSI and the row of all types have no tested share.
  expect_true(all(is.na(unlist(
    r[r$inf %in% c("all", "bsi"), tested_quartile_cols]))))
  # A table without quartiles leaves those of the tested shares out too.
  expect_false(any(tested_quartile_cols %in% names(
    get_infectious_agent_detection_rate_per_inf_type_table(
      ds, use_cache = TRUE, include_quartiles = FALSE))))

  # Department 5 tests none of its pneumonias: it enters the tested share's
  # quartiles at 0, and the positive share's 4 departments are too few.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(c(2L, 4L, 6L, 8L, 0L), c(1L, 1L, 3L, 2L, 0L)),
      use_cache = FALSE))
  expect_equal(quartiles(hap, "tested_"), c(25, 50, 75))
  expect_true(all(is.na(quartiles(hap))))

  # Every department tests all its 8 pneumonias and finds an infectious
  # agent in 1: at the pooled share of 12.5%, 8 tested pneumonias give 1
  # positive one, and the departments' median of 8 tested pneumonias does not
  # exceed them.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(rep(8L, 5), rep(1L, 5)), use_cache = FALSE))
  expect_equal(quartiles(hap, "tested_"), c(100, 100, 100))
  expect_true(all(is.na(quartiles(hap))))

  # 4 departments are too few for either share's quartiles.
  hap <- (\(r) r[r$inf == "hap", ])(
    get_infectious_agent_detection_rate_per_inf_type_table(
      departments(tested[1:4], positive[1:4]), use_cache = FALSE))
  expect_true(all(is.na(c(quartiles(hap, "tested_"), quartiles(hap)))))
  expect_true(all(is.na(unlist(hap[tested_quartile_cols]))))

  # Sepsis/BSI's positive share takes every episode. Each department has 4
  # episodes, of which 1, 2, 2, 3, and 4 name an infectious agent, the pooled
  # 60% passing the gate against a median of 4 episodes; with no other
  # infection, the row of all types has the same quartiles.
  episodes <- c(1L, 2L, 2L, 3L, 4L)
  r <- get_infectious_agent_detection_rate_per_inf_type_table(
    make_infection_calc_test_ds(
      forms    = tibble::tibble(
        event_type_key = "bsi",
        department_key = rep(1:5, each = 4),
        no_pos_culture = NA),
      findings = tibble::tibble(
        form          = which(sequence(rep(4L, 5)) <= rep(episodes, each = 4)),
        secondary_bsi = FALSE,
        pathogen_key  = 1L)),
    use_cache = FALSE)
  for (type in c("bsi", "all")) {
    row <- r[r$inf == type, ]
    expect_equal(quartiles(row), c(50, 50, 75), info = type)
    expect_equal(intervals(row), binomial_intervals(episodes, rep(4L, 5)),
                 info = type)
  }
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
