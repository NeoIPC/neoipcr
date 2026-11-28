# Tests for R/calc-denominators.R — per-file unit tests.
# Uses make_infection_calc_test_ds() and make_exposure_patients_test_ds() from
# helper-fixtures.R.

test_that("an enrolments table without any hierarchy key passes the renaming untouched", {
  # A dataset imported with every hierarchy option "no" holds none of the
  # hierarchy keys; `rename_with` gets one name for an empty selection and
  # aborts unless the renaming itself drops to zero names.
  enrollments <- tibble::tibble(
    enrollment_key = 1:2,
    patient_key = 1:2,
    program_start_date = as.Date(c("2024-01-01", "2024-01-05")))
  expect_identical(
    neoipcr:::.enrollment_hierarchy_cols(enrollments), enrollments)
})

test_that("a stay's days stay with its department when the patient record stands in another", {
  # Department 1's patient record moved to department 2's unit, as a
  # patient whose record was reassigned on paper is on a live DHIS2, while
  # the stay, its admission, and its surveillance end stay in department 1.
  # A denominator that let the patient record's own unit win would move
  # department 1's stay to department 2's rows.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("bsi", "bsi"),
      department_key = c(1L, 2L)),
    findings = tibble::tibble(form = 1:2, pathogen_key = c(934L, 1807L)),
    patient_days = c(45L, 70L))
  # The aware-days chain the risk-time denominators join through needs
  # substance days on the surveillance-end forms.
  end_key <- ds$events$event_key[ds$events$event_type_key == "end"]
  ds$substanceDays <- make_test_substance_days(end_key)
  ds$patients$department_key[1L] <- 2L

  population <- neoipcr:::get_risk_population(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(population$enrollment_department_key, c(1L, 2L))
  expect_equal(population$n_enrollments, c(1L, 1L))

  risk_time <- neoipcr:::get_risk_time(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(risk_time$enrollment_department_key, c(1L, 2L))
  # The day values, not just the rows: department 1's stay keeps its own 45
  # days though its patient record stands in department 2, and does not
  # join department 2's 70.
  expect_equal(risk_time$patient_days, c(45L, 70L))
})

exposure_ds <- make_exposure_patients_test_ds()

test_that("get_patient_exposure_days returns one row per patient with the eight factors' days", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 16L)
  expect_setequal(
    names(result),
    c("patient_key", "department_key",
      "cvc_days", "pvc_days", "inv_days", "niv_days", "ab_days",
      "human_milk_days", "probiotic_days", "kangaroo_care_days"))
  p3 <- result[result$patient_key == 3, ]
  expect_equal(p3$cvc_days, 10)
  expect_equal(p3$human_milk_days, 20)
  expect_equal(p3$department_key, 1L)
})

test_that("the stays of one patient sum to that patient, and a stay without an antibiotic entry counts", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  p1 <- result[result$patient_key == 1, ]
  # Patient 1's readmission carries two of its five CVC days and stands
  # without an antibiotic entry; a helper that left such stays out would
  # give three days, and one that kept the stays apart would give two rows.
  expect_equal(nrow(p1), 1L)
  expect_equal(p1$cvc_days, 5)
  expect_equal(p1$niv_days, 1)
})

test_that("patients without an exposure day keep a row at zero", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  zeros <- result[result$patient_key %in% c(11, 14, 16), ]
  expect_equal(nrow(zeros), 3L)
  days <- zeros |> dplyr::select(tidyselect::ends_with("_days"))
  expect_equal(sum(unlist(days)), 0)
})

test_that("the patients of a department gather under its key", {
  result <- neoipcr:::get_patient_exposure_days(exposure_ds, use_cache = FALSE)
  expect_equal(
    result |> dplyr::count(.data$department_key, name = "n") |>
      dplyr::pull(.data$n),
    c(6L, 6L, 4L))
})

test_that("a stay with a blank day count adds none of its days, the known ones still counting", {
  blank_ds <- make_exposure_patients_test_ds()
  # Patient 1's readmission, the surveillance-end form of event 4, holds no
  # CVC day: unknown is not no. Its stay adds none, and the three days of
  # patient 1's first stay are all that stand counted.
  blank_ds$surveillanceEndData$cvc_days[
    blank_ds$surveillanceEndData$event_key == 4L] <- NA_integer_
  result <- neoipcr:::get_patient_exposure_days(blank_ds, use_cache = FALSE)
  expect_equal(result$cvc_days[result$patient_key == 1], 3)
  table_result <- get_patients_exposed_table(blank_ds, use_cache = FALSE)
  cvc <- table_result[table_result$factor == "cvc" &
                        table_result$measure == "patients_exposed", ]
  # Patient 1 is still one of the seven exposed.
  expect_equal(cvc$n, 7L)
  cvc_days <- table_result[table_result$factor == "cvc" &
                             table_result$measure == "mean_days", ]
  expect_equal(cvc_days$n, 29L)
})

test_that("a day column the surveillance-end forms lack fails where the exposure days read it", {
  gap_ds <- make_exposure_patients_test_ds()
  gap_ds$surveillanceEndData <- gap_ds$surveillanceEndData |>
    dplyr::select(-"human_milk_days")
  # The eight day counts are compulsory on the form, so a dataset whose
  # forms lack one is malformed: the error names the column at the read,
  # not at the pivot far downstream.
  expect_error(
    neoipcr:::get_patient_exposure_days(gap_ds, use_cache = FALSE),
    "human_milk_days")
})
