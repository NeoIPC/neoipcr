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
    ds, group_cols = "enrollment_department_key", use_cache = FALSE) |>
    dplyr::filter(.data$event_type_key == "hap") |>
    dplyr::arrange(.data$enrollment_department_key)
  expect_equal(by_dept$enrollment_department_key, c(1, 2, 3))
  expect_equal(by_dept$total_n, c(2L, 1L, 2L))
  expect_equal(by_dept$followup_n, c(1L, 1L, 0L))
  expect_equal(by_dept$followup_rate, c(50, 100, 0))
  expect_equal(by_dept$n, c(1L, 0L, 0L))
  expect_equal(by_dept$rate, c(100, 0, NaN))
})

test_that("an infection recorded outside its stay counts for the stay's department", {
  # Both sepsis episodes belong to the stay of department 1; the recording
  # desk moved them to department 2's unit, as an event recorded away from
  # its enrolment's unit is on a live DHIS2.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("bsi", "bsi"),
      department_key = c(1L, 1L)),
    findings = tibble::tibble(form = 1:2, pathogen_key = c(934L, 1807L)))
  # The surveillance-end row's AWaRe days come from the substance-days table
  # joined through its data, so the fixture gives the end event a substance.
  end_key <- ds$events$event_key[ds$events$event_type_key == "end"]
  ds$substanceDays <- make_test_substance_days(end_key)
  inf <- ds$events$event_key[
    ds$events$event_type_key %in% c("bsi", "nec", "hap", "ssi")]
  ds$events$event_department_key[ds$events$event_key %in% inf] <- 2L
  expect_equal(unique(ds$events$event_department_key[
    ds$events$event_key %in% inf]), 2L)

  counts <- neoipcr:::get_infection_counts(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(counts$enrollment_department_key, 1L)
  expect_equal(counts$n, 2L)

  density <- neoipcr:::get_incidence_density_rates(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(unique(density$enrollment_department_key), 1L)
  expect_equal(density$n[density$inf == "bsi"], 2L)

  # The stay's days follow the stay, not the recording desk.
  risk_time <- neoipcr:::get_risk_time(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(risk_time$enrollment_department_key, 1L)
})

test_that("a department with stays but no infection counts at zero by the stay's department", {
  # Both sepsis episodes are department 2's. Department 1 holds a stay,
  # its admission and surveillance-end forms, and no infection — the
  # department a benchmark's pooled mean must still hold, at zero
  # infections.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("bsi", "bsi"),
      department_key = c(2L, 2L)),
    findings = tibble::tibble(form = 1:2, pathogen_key = c(934L, 1807L)))

  counts <- neoipcr:::get_infection_counts(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(counts$enrollment_department_key, c(1L, 2L))
  expect_equal(counts$n, c(0L, 2L))

  # The plain key counts the same stays, under the name the caller gave.
  plain <- neoipcr:::get_infection_counts(
    ds, group_cols = "department_key", use_cache = FALSE)
  expect_equal(plain$department_key, c(1L, 2L))
  expect_equal(plain$n, c(0L, 2L))
})

test_that("a dataset without infections counts zero for every stay's department", {
  # Both stays hold their admission and surveillance-end forms, and the
  # dataset records no infection at all: the frame must still hold both
  # departments at zero, and every department-and-type pair at zero, so a
  # benchmark's means and quartiles do not lose the departments over a
  # dataset no infection entered.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi", department_key = c(1L, 2L)))
  inf <- ds$events$event_key[
    ds$events$event_type_key %in% c("bsi", "nec", "hap", "ssi")]
  ds$events <- ds$events[!ds$events$event_key %in% inf, ]

  counts <- neoipcr:::get_infection_counts(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(counts$enrollment_department_key, c(1L, 2L))
  expect_equal(counts$n, c(0L, 0L))

  by_type <- neoipcr:::get_infection_counts(
    ds, group_cols = c("enrollment_department_key", "event_type_key"),
    use_cache = FALSE)
  expect_equal(nrow(by_type), 8L)
  expect_equal(unique(by_type$enrollment_department_key), c(1L, 2L))
  expect_equal(as.character(unique(by_type$event_type_key)),
               c("bsi", "nec", "hap", "ssi"))
  expect_equal(unique(by_type$n), 0L)

  # The recording side frames on the events it has: the admission and
  # surveillance-end forms' units, each at zero infections.
  recording <- neoipcr:::get_infection_counts(
    ds, group_cols = "event_department_key", use_cache = FALSE)
  expect_equal(recording$event_department_key, c(1L, 2L))
  expect_equal(recording$n, c(0L, 0L))
})

test_that("grouping by the recording side counts and frames on what was recorded", {
  # Both sepsis episodes belong to stays of department 2, which also
  # holds their admissions; department 1 holds one stay, its admission
  # and surveillance-end forms, and no infection of its own.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = c("bsi", "bsi"),
      department_key = c(2L, 2L)),
    findings = tibble::tibble(form = 1:2, pathogen_key = c(934L, 1807L)))

  # Department 1 records its stay's forms but no infection: zero.
  before <- neoipcr:::get_infection_counts(
    ds, group_cols = "event_department_key", use_cache = FALSE)
  expect_equal(before$event_department_key, c(1L, 2L))
  expect_equal(before$n, c(0L, 2L))

  # The recording desk moves one episode to department 1's unit: it now
  # counts there, and no longer at the stay's department on this side.
  inf <- ds$events$event_key[ds$events$event_type_key == "bsi"]
  ds$events$event_department_key[ds$events$event_key == inf[1]] <- 1L
  after <- neoipcr:::get_infection_counts(
    ds, group_cols = "event_department_key", use_cache = FALSE)
  expect_equal(after$event_department_key, c(1L, 2L))
  expect_equal(after$n, c(1L, 1L))

  # The stay side never saw the drift.
  stay <- neoipcr:::get_infection_counts(
    ds, group_cols = "enrollment_department_key", use_cache = FALSE)
  expect_equal(stay$n, c(0L, 2L))
})

# --- Classified findings ----------------------------------------------------

test_that("the classified findings keep every finding that names an infectious agent, an unknown code included", {
  # Slots 1-6 name Staphylococcus aureus (934), a synonym of the herpes simplex
  # virus 1 (2617, whose concept is 3505), "Not listed" (0), "Unidentifiable"
  # (3561), and a code the list lacks; slot 6 holds a resistance category
  # alone.
  findings <- make_test_iaf(
    c(1L, 1L, 2L, 3L, 4L, 5L),
    pathogen_key = c(934L, 2617L, 0L, 3561L, 99999999L, NA))

  r <- neoipcr:::classify_infectious_agent_findings(findings)

  expect_equal(r$pathogen_key, c(934L, 2617L, 0L, 3561L, 99999999L))
  expect_equal(r$concept_key, c(934L, 3505L, 0L, 3561L, 99999999L))
  expect_equal(r$in_tree, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  # The list applies every resistance category to "Not listed" and none to
  # "Unidentifiable"; a code it lacks is an infectious agent to which no
  # category applies and no common commensal.
  expect_equal(r$applies_mrsa, c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(r$applies_3gcr, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(r$common_commensal, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(r$gram_reaction, c("positive", NA, NA, NA, NA))
  # The findings' own columns stay.
  expect_equal(r$agent_finding_key, 1:5)
  expect_true(all(c("secondary_bsi", "3gcr", "mrsa") %in% names(r)))
})

# --- The rows of an infectious agent ----------------------------------------

test_that("the node an infectious agent is recorded at has its residual row only where a node beneath it is among the rows given", {
  # The genus Enterococcus (503) is recorded at its own node, beneath which
  # the bundled full ontology holds its species.
  agents <- neoipcr:::classify_infectious_agent_findings(
    make_test_iaf(1L, pathogen_key = 503L))
  closure <- neoipcr:::internal_infectious_agent_closure
  path <- closure$node_id[closure$version == "ontology" &
                            closure$input_id == 503L]
  residual <- "LPSN|genus/enterococcus~n.o.s."

  every_row <- neoipcr:::.infectious_agent_memberships(agents, "ontology")
  expect_setequal(every_row$node_id, c(path, residual))

  # Among the nodes of the genus's own path, none lies beneath it.
  own_path <- neoipcr:::.infectious_agent_memberships(agents, "ontology",
                                                      nodes = path)
  expect_setequal(own_path$node_id, path)
  expect_false(any(endsWith(own_path$node_id, "~n.o.s.")))
})

# --- Detections -------------------------------------------------------------

test_that("a detection is an infectious agent once per infection, tested where a slot of it has a result and resistant where one records resistance", {
  # Form 1, a pneumonia tested positive and answered Yes, names Klebsiella
  # pneumoniae (390) in its own slot, third-generation cephalosporin resistance
  # (3GCR) "Not tested" and carbapenem resistance not resistant, and in a
  # secondary-BSI slot, 3GCR resistant and carbapenem resistance blank. Form 2,
  # a sepsis, names two synonyms of the herpes simplex virus 1 (2617 and 2616,
  # whose concept is 3505); form 3, recorded culture-negative, Escherichia coli
  # (1807) where the form does not ask for it; form 4 a code the list lacks,
  # 3GCR resistant.
  ds <- make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key              = c("hap", "bsi", "bsi", "bsi"),
      department_key              = c(1L, 2L, 2L, 2L),
      sec_bsi                     = c("1", NA, NA, NA),
      microbiological_test_result = c("1", NA, NA, NA),
      no_pos_culture              = c(NA, FALSE, TRUE, FALSE)),
    findings = tibble::tibble(
      form          = c(1L, 1L, 2L, 2L, 3L, 4L),
      secondary_bsi = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
      pathogen_key  = c(390L, 390L, 2617L, 2616L, 1807L, 99999999L),
      `3gcr`        = c("not_tested", "yes", NA, NA, "yes", "yes"),
      car           = c("no", NA, NA, NA, "yes", NA),
      cor           = NA_character_,
      mrsa          = NA_character_,
      vre           = NA_character_))

  r <- neoipcr:::get_infectious_agent_detections(ds, use_cache = FALSE) |>
    dplyr::arrange(.data$event_key)

  infections <- ds$events$event_key[ds$events$event_type_key != "adm" &
                                      ds$events$event_type_key != "end"]
  expect_equal(r$event_key, infections[c(1L, 2L, 4L)])
  expect_equal(r$department_key, c(1L, 2L, 2L))
  expect_equal(r$concept_key, c(390L, 3505L, 99999999L))
  expect_equal(r$in_tree, c(TRUE, TRUE, FALSE))
  expect_equal(r$applies_3gcr, c(TRUE, FALSE, FALSE))
  expect_equal(r$tested_3gcr, c(TRUE, FALSE, TRUE))
  expect_equal(r$resistant_3gcr, c(TRUE, FALSE, TRUE))
  expect_equal(r$tested_car, c(TRUE, FALSE, FALSE))
  expect_equal(r$resistant_car, c(FALSE, FALSE, FALSE))
  expect_equal(r$tested_cor, c(FALSE, FALSE, FALSE))
  expect_equal(r$gram_reaction, c("negative", NA, NA))
})

# --- Departments' quartiles -------------------------------------------------

test_that("a department enters a row's quartiles with a positive denominator at the minimum, at 0 without the row's events", {
  # Departments 1-5 have 5 infections each, with 0 to 4 of the row's; 6 has 4,
  # below the minimum of a count denominator; 7 has none.
  departments <- tibble::tibble(
    row = "a",
    events = c(0L, 1L, 2L, 3L, 4L, 4L, 0L),
    denominator = c(5L, 5L, 5L, 5L, 5L, 4L, 0L))
  pooled <- tibble::tibble(row = "a", pooled = 14 / 29 * 100)

  r <- neoipcr:::.department_quartiles(
    departments, pooled, by = "row", scale = 100, type = "binomial",
    denominator = "count")

  expect_equal(r$quartile_departments, 5L)
  expect_equal(c(r$q1, r$q2, r$q3), c(20, 40, 60))
  expect_equal(
    r[c("q1_ci_lower", "q1_ci_upper", "q2_ci_lower", "q2_ci_upper",
        "q3_ci_lower", "q3_ci_upper")],
    bootstrap_quantile_ci(c(0L, 1L, 2L, 3L, 4L), rep(5L, 5L),
                          type = "binomial", multiplier = 100))
})

test_that("a day denominator enters the quartiles from 50 days", {
  departments <- tibble::tibble(
    row = "a",
    events = c(1L, 2L, 3L, 4L, 5L, 9L),
    denominator = c(50L, 60L, 70L, 80L, 100L, 49L))
  pooled <- tibble::tibble(row = "a", pooled = 24 / 409 * 1000)

  r <- neoipcr:::.department_quartiles(
    departments, pooled, by = "row", scale = 1000, type = "poisson",
    denominator = "days")

  expect_equal(r$quartile_departments, 5L)
  rates <- c(1 / 50, 2 / 60, 3 / 70, 4 / 80, 5 / 100) * 1000
  expect_equal(c(r$q1, r$q2, r$q3),
               stats::quantile(rates, c(0.25, 0.5, 0.75), names = FALSE))
  expect_equal(
    r$q2_ci_lower,
    bootstrap_quantile_ci(1:5, c(50L, 60L, 70L, 80L, 100L), type = "poisson",
                          multiplier = 1000)$q2_ci_lower)
})

test_that("a row shows its quartiles only where 5 departments enter and their median denominator exceeds the one in which the pooled rate gives 1 event", {
  # Rows a-c have 5 entering departments with a median of 5 infections; row d
  # has 4. Row a's pooled rate gives 1 event in 2.5 infections, row b's in
  # exactly 5, and row c's in none.
  departments <- dplyr::bind_rows(
    tibble::tibble(row = "a", events = c(1L, 2L, 3L, 2L, 2L),
                   denominator = 5L),
    tibble::tibble(row = "b", events = c(1L, 1L, 1L, 1L, 1L),
                   denominator = 5L),
    tibble::tibble(row = "c", events = 0L, denominator = rep(5L, 5L)),
    tibble::tibble(row = "d", events = c(1L, 2L, 3L, 4L),
                   denominator = 5L))
  pooled <- tibble::tibble(row = c("a", "b", "c", "d"),
                           pooled = c(40, 20, 0, 50))

  r <- neoipcr:::.department_quartiles(
    departments, pooled, by = "row", scale = 100, type = "binomial",
    denominator = "count")

  expect_equal(r$row, c("a", "b", "c", "d"))
  expect_equal(r$quartile_departments, c(5L, 5L, 5L, 4L))
  expect_equal(r$q2, c(40, NA, NA, NA))
  expect_equal(is.na(r$q2_ci_lower), c(FALSE, TRUE, TRUE, TRUE))
  # A row no department enters has none.
  r <- neoipcr:::.department_quartiles(
    departments[0, ], pooled[1, ], by = "row", scale = 100,
    type = "binomial", denominator = "count")
  expect_equal(r$quartile_departments, 0L)
  expect_true(is.na(r$q1))
})

test_that("the display test takes the median of the entering departments' denominators, not their smallest or their mean", {
  # Row a's pooled rate of 10% gives 1 event in 10 infections: the median of
  # 12 exceeds it, the smallest denominator, 5, does not. Row b's of 12.5%
  # gives 1 in 8: the median of 6 does not exceed it, the mean, 15.2, does.
  departments <- dplyr::bind_rows(
    tibble::tibble(row = "a", events = c(0L, 1L, 1L, 4L, 4L),
                   denominator = c(5L, 5L, 12L, 40L, 40L)),
    tibble::tibble(row = "b", events = c(1L, 1L, 1L, 4L, 3L),
                   denominator = c(5L, 5L, 6L, 30L, 30L)))
  pooled <- tibble::tibble(row = c("a", "b"), pooled = c(10, 12.5))

  r <- neoipcr:::.department_quartiles(
    departments, pooled, by = "row", scale = 100, type = "binomial",
    denominator = "count")

  expect_equal(r$quartile_departments, c(5L, 5L))
  expect_equal(
    c(r$q1[1], r$q2[1], r$q3[1]),
    stats::quantile(c(0, 20, 1 / 12 * 100, 10, 10), c(0.25, 0.5, 0.75),
                    names = FALSE))
  expect_false(is.na(r$q2_ci_lower[1]))
  expect_true(all(is.na(c(r$q1[2], r$q2[2], r$q3[2], r$q1_ci_lower[2],
                          r$q3_ci_upper[2]))))
})
