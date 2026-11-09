# Tests for R/calc-rates.R — the internal rate and count computers, on
# infection forms built by make_infection_calc_test_ds() in helper-fixtures.R.

no_agent <- factor(NA_character_, levels = c("no", "yes", "not_tested"))

# --- Infections with an infectious agent ------------------------------------

test_that("an infection counts as one with an infectious agent only where a finding names one", {
  # Forms 1-4 name an infectious agent: a listed one, "Not listed" (Id 0),
  # "Unidentifiable" (Id 3561), and a code the list lacks. Forms 5 and 6 hold
  # a slot without one: the source, the "recovered multiple times" item, and
  # a resistance category; and nothing at all in the findings, as a slot that
  # holds only a free-text name leaves it. Form 7 holds no slot.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = rep("bsi", 7)),
    findings = tibble::tibble(
      form         = 1:6,
      pathogen_key = c(1L, 0L, 3561L, 99999999L, NA, NA),
      source       = c("B", "B", "B", "B", "B", NA),
      multiple     = c(FALSE, FALSE, FALSE, FALSE, TRUE, NA),
      `3gcr`       = factor(c("no", "no", "no", "no", "yes", NA),
                            levels = c("no", "yes", "not_tested")),
      car = no_agent, cor = no_agent, mrsa = no_agent, vre = no_agent))

  counts <- neoipcr:::get_infection_counts(
    ds, group_cols = c("event_type_key", "with_pathogen"), use_cache = FALSE) |>
    dplyr::filter(.data$event_type_key == "bsi")

  expect_equal(counts$n[counts$with_pathogen], 4L)
  expect_equal(counts$n[!counts$with_pathogen], 3L)
})

# --- Secondary BSI ----------------------------------------------------------

test_that("a secondary-BSI slot without an infectious agent makes no secondary BSI", {
  # Each form is followed up and answered Yes, or for pneumonia No; only the
  # NEC of form 1 names an infectious agent in a secondary-BSI slot. The
  # others hold a resistance category alone there.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("nec", "nec", "hap", "ssi"),
      sec_bsi        = c("1", "1", "0", "1")),
    findings = tibble::tibble(
      form          = 1:4,
      secondary_bsi = TRUE,
      pathogen_key  = c(1L, NA, NA, NA),
      `3gcr`        = factor(c("no", "yes", "yes", "yes"),
                             levels = c("no", "yes", "not_tested"))))

  r <- neoipcr:::get_secondary_bsi_rates(ds, use_cache = FALSE)

  expect_equal(r$n[r$event_type_key == "nec"], 1L)
  expect_equal(r$rate[r$event_type_key == "nec"], 50)
  expect_equal(r$n[r$event_type_key == "hap"], 0L)
  expect_equal(r$n[r$event_type_key == "ssi"], 0L)
})

test_that("a secondary BSI counts only on a follow-up answered Yes, whatever an exception keeps in the slots", {
  # Form 1 is answered Yes; forms 2 and 3 name an infectious agent in the
  # secondary-BSI slots although the follow-up is answered No or not done,
  # which only a validation exception lets through; form 4 is unanswered.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("nec", "nec", "nec", "nec", "hap"),
      sec_bsi        = c("1", "0", "-1", NA, "-1")),
    findings = tibble::tibble(
      form          = c(1L, 2L, 3L, 5L),
      secondary_bsi = TRUE,
      pathogen_key  = c(1L, 1L, 1L, 1L)))

  r <- neoipcr:::get_secondary_bsi_rates(ds, use_cache = FALSE)
  nec <- r[r$event_type_key == "nec", ]

  # The infections followed up are forms 1 and 2, and only form 1 had one.
  expect_equal(nec$followup_n, 2L)
  expect_equal(nec$n, 1L)
  expect_equal(nec$rate, 50)
  expect_true(all(r$n <= r$followup_n))
  expect_equal(r$n[r$event_type_key == "hap"], 0L)
})

test_that("a secondary BSI counts only an infectious agent in a secondary-BSI slot", {
  # The pneumonia and the SSI are answered Yes but name an infectious agent
  # only in their own slots; the NECs answered Yes name "Not listed" (Id 0),
  # "Unidentifiable" (Id 3561), and a code the list lacks in their
  # secondary-BSI slots.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("hap", "ssi", "nec", "nec", "nec"),
      sec_bsi        = "1"),
    findings = tibble::tibble(
      form          = 1:5,
      secondary_bsi = c(FALSE, FALSE, TRUE, TRUE, TRUE),
      pathogen_key  = c(1L, 1L, 0L, 3561L, 99999999L)))

  r <- neoipcr:::get_secondary_bsi_rates(ds, use_cache = FALSE)

  expect_equal(r$n[r$event_type_key == "hap"], 0L)
  expect_equal(r$n[r$event_type_key == "ssi"], 0L)
  expect_equal(r$n[r$event_type_key == "nec"], 3L)
  expect_equal(r$rate[r$event_type_key == "nec"], 100)
})

test_that("each secondary-BSI rate carries the share of the type's infections followed up, pooled and per department", {
  # Department 1: a pneumonia answered Yes with an infectious agent and one
  # not followed up; department 2: a pneumonia answered No; department 3: a
  # pneumonia not followed up and one left unanswered.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "hap",
      department_key = c(1L, 1L, 2L, 3L, 3L),
      sec_bsi        = c("1", "-1", "0", "-1", NA)),
    findings = tibble::tibble(
      form = 1L, secondary_bsi = TRUE, pathogen_key = 1L))

  pooled <- neoipcr:::get_secondary_bsi_rates(ds, use_cache = FALSE)
  hap <- pooled[pooled$event_type_key == "hap", ]
  expect_equal(hap$total_n, 5L)
  expect_equal(hap$followup_n, 2L)
  expect_equal(hap$followup_rate, 40)
  expect_equal(hap$n, 1L)
  expect_equal(hap$rate, 50)
  # A type without infections has nothing to follow up.
  nec <- pooled[pooled$event_type_key == "nec", ]
  expect_equal(nec$total_n, 0L)
  expect_true(is.nan(nec$followup_rate))

  by_dept <- neoipcr:::get_secondary_bsi_rates(
    ds, group_cols = "department_key", use_cache = FALSE) |>
    dplyr::filter(.data$event_type_key == "hap") |>
    dplyr::arrange(.data$department_key)
  expect_equal(by_dept$department_key, c(1, 2, 3))
  expect_equal(by_dept$total_n, c(2L, 1L, 2L))
  expect_equal(by_dept$followup_n, c(1L, 1L, 0L))
  expect_equal(by_dept$followup_rate, c(50, 100, 0))
  expect_equal(by_dept$n, c(1L, 0L, 0L))
  expect_equal(by_dept$rate, c(100, 0, NaN))
})
