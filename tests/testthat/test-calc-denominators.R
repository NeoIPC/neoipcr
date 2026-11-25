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
    findings = tibble::tibble(form = 1:2, pathogen_key = c(934L, 1807L)))
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
})
