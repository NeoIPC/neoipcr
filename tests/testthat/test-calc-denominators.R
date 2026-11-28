# Tests for R/calc-denominators.R — the denominators of the rates:
# get_risk_time(), get_surgery_risk(), and the substance-day and AWaRe
# totals beneath them.

# --- Risk time over every surveillance end ---

test_that("a stay without an antibiotic entry counts in the pooled risk time", {
  ds <- make_risk_time_test_ds(patient_days = c(10L, 15L))
  rt <- neoipcr:::get_risk_time(ds, use_cache = FALSE)
  # The fixture's surveillance-end forms give every department the same
  # device and other exposure days; both stays count, whichever recorded
  # an antibiotic entry.
  expect_equal(rt$patient_days, 25L)
  expect_equal(rt$cvc_days, 6L)
  expect_equal(rt$ab_rate, 40)
})

test_that("a stay without an antibiotic entry counts in its department's risk time", {
  ds <- make_risk_time_test_ds(patient_days = c(10L, 15L))
  rt <- neoipcr:::get_risk_time(
    ds, group_cols = "department_key", use_cache = FALSE)
  expect_equal(nrow(rt), 2L)
  row1 <- rt[rt$department_key == 1L, ]
  expect_equal(row1$patient_days, 10L)
  expect_equal(row1$inv_days, 1L)
  expect_equal(row1$probiotic_rate, 60)
})

test_that("the substance-day columns of a stay without an entry are 0", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L), entry_departments = 2L, entry_days = 5L)
  rt <- neoipcr:::get_risk_time(
    ds, group_cols = "department_key", use_cache = FALSE)
  rt <- rt[order(rt$department_key), ]
  expect_equal(rt$patient_days, c(10L, 15L))
  expect_equal(rt$a_days, c(0L, 5L))
  expect_equal(rt$a_rate, c(0, 100 / 3))
})

test_that("a procedure on a stay without an antibiotic entry counts in the surgical risk", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    extra_forms = c("pro"))
  ds$surgeryData <- make_test_surgery_data(5L,
    main_procedure_code = "KMA.JB.AE")
  expect_equal(as.character(get_procedure_category("KMA.JB.AE")),
               "abdominal_surgery")
  sr <- neoipcr:::get_surgery_risk(
    ds, group_cols = "department_key", use_cache = FALSE)
  row2 <- sr[sr$department_key == 2L, ]
  expect_equal(row2$n_procedures, 1L)
  expect_equal(row2$n_patients, 1L)
})

# --- Entries without days ---

test_that("an entry without days counts in the risk time with zero substance-days", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = 3L,
      substance_code = "J01CA04",
      days = NA_integer_))
  rt <- neoipcr:::get_risk_time(ds, use_cache = FALSE)
  expect_equal(rt$patient_days, 25L)
  expect_equal(rt$a_days, 0L)
})

test_that("the substance totals give an entry without days its zero known days", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = 3L,
      substance_code = "J01CA04",
      days = NA_integer_))
  sum_sub <- neoipcr:::summarise_substance_days(
    ds, level = "substance", use_cache = FALSE)
  expect_equal(sum_sub$code, "J01CA04")
  expect_equal(sum_sub$days, 0L)
  aw <- neoipcr:::summarise_substance_days(
    ds, level = "aware", use_cache = FALSE)
  expect_equal(sort(as.character(aw$code)), c("a", "nc", "r", "w"))
  row <- aw[aw$code == "a", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$days, 0L)
  expect_equal(aw$days[aw$code == "w"], 0L)
  expect_equal(aw$days[aw$code == "r"], 0L)
  expect_equal(aw$days[aw$code == "nc"], 0L)
})

test_that("all four AWaRe levels appear in the aware summary even when only one is used", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = 3L,
      substance_code = "J01CA04",
      days = 5L))
  aw <- neoipcr:::summarise_substance_days(
    ds, level = "aware", use_cache = FALSE)
  expect_equal(sort(as.character(aw$code)), c("a", "nc", "r", "w"))
  expect_equal(aw$days[aw$code == "a"], 5L)
  expect_equal(aw$days[aw$code == "w"], 0L)
  expect_equal(aw$days[aw$code == "r"], 0L)
  expect_equal(aw$days[aw$code == "nc"], 0L)
})

test_that("risk time has every AWaRe days column when only Access entries exist", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = 3L,
      substance_code = "J01CA04",
      days = 5L))
  rt <- neoipcr:::get_risk_time(ds, use_cache = FALSE)
  expect_true(all(c("a_days", "w_days", "r_days", "nc_days") %in% names(rt)))
  expect_equal(rt$w_days, 0L)
  expect_equal(rt$r_days, 0L)
  expect_equal(rt$nc_days, 0L)
})

test_that("known days still sum where one of several entries lacks its days", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = c(3L, 4L),
      substance_code = c("J01CA04", "J01CA04"),
      days = c(NA_integer_, 2L)))
  sum_sub <- neoipcr:::summarise_substance_days(
    ds, level = "substance", use_cache = FALSE)
  expect_equal(sum_sub$code, "J01CA04")
  expect_equal(sum_sub$days, 2L)
  aw <- neoipcr:::summarise_substance_days(
    ds, level = "aware", use_cache = FALSE)
  row <- aw[aw$code == "a", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$days, 2L)
})

test_that("an entry without days leaves the risk time of every department free of NA", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = 3L,
      substance_code = "J01CA04",
      days = NA_integer_))
  rt <- neoipcr:::get_risk_time(
    ds, group_cols = "department_key", use_cache = FALSE)
  expect_equal(nrow(rt), 2L)
  expect_false(any(is.na(rt$a_days)))
  expect_equal(sum(rt$a_days), 0L)
})

# --- A substance without an AWaRe category ---

test_that("a substance without an AWaRe category counts as not classified", {
  ds <- make_risk_time_test_ds(
    patient_days = c(10L, 15L),
    entry_rows = tibble::tibble(
      event_key = c(3L, 4L),
      substance_code = c("J01MA17", "J01CA04"),
      days = c(2L, 3L)))
  rt <- neoipcr:::get_risk_time(ds, use_cache = FALSE)
  expect_true("nc_days" %in% names(rt))
  expect_equal(rt$nc_days, 2L)
  expect_equal(rt$a_days, 3L)
  sum_sub <- neoipcr:::summarise_substance_days(
    ds, level = "substance", use_cache = FALSE)
  sum_sub <- sum_sub[order(sum_sub$code), ]
  expect_equal(sum_sub$code, c("J01CA04", "J01MA17"))
  expect_equal(sum_sub$days, c(3L, 2L))
  aw <- neoipcr:::summarise_substance_days(
    ds, level = "aware", use_cache = FALSE)
  row <- aw[aw$code == "nc", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$days, 2L)
})
