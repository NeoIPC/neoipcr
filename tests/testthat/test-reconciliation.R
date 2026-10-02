# Tests for R/reconciliation.R — the coordinating centre's reconciliations,
# their registry and `reconciliation_details()`. The import pipeline tests
# in test-import-dhis2.R drive them end to end.

utc_time <- function(x) as.POSIXct(x, tz = "UTC")

event_type_levels <- c("adm", "pro", "bsi", "nec", "ssi", "hap", "end")

# --- Gestational age (reconciliations 3 and 4) -----------------------------

# Patients keyed 1, 2, … with the gestational-age texts `text` and the stored
# totals `total`, the total carrying its audit fields and the text one of its
# own.
gestation_patients <- function(text, total) {
  n <- length(text)
  tibble::tibble(
    patient_key                    = seq_len(n),
    gest_age                       = text,
    total_gestation_days           = as.integer(total),
    total_gestation_days_storedBy  = rep(7L, n),
    total_gestation_days_createdAt = rep(utc_time("2024-01-01 10:00"), n),
    total_gestation_days_updatedAt = rep(utc_time("2024-01-02 10:00"), n),
    gest_age_storedBy              = rep(7L, n))
}

test_that("reconciliation 3 computes the total gestation days from a text in the required format", {
  p <- gestation_patients(
    c("20+0", "25+4", "25+4", "49+6", "31+6", "32+0"),
    c(NA, 170, 179, 349, 230, 200))

  r <- neoipcr:::.reconcile_gestational_age(p)

  # The client's formula: the completed weeks times seven plus the days.
  expect_identical(
    r$records$total_gestation_days,
    c(20L * 7L, 25L * 7L + 4L, 179L, 349L, 31L * 7L + 6L, 32L * 7L))
  changes <- r$changes$`3`
  expect_equal(changes$patient_key, c(1L, 2L, 5L, 6L))
  expect_equal(changes$reconciliation_id, rep(3L, 4L))
  expect_equal(changes$action, rep("repair", 4L))
  expect_equal(changes$gest_age, c("20+0", "25+4", "31+6", "32+0"))
  expect_equal(changes$total_gestation_days, c(NA, 170L, 230L, 200L))
  expect_equal(changes$total_gestation_days_reconciled, c(140L, 179L, 223L, 224L))
  expect_equal(nrow(r$changes$`4`), 0L)
})

test_that("reconciliation 4 removes a total outside 140 to 349 days beside no text in the required format", {
  # The bounds are the totals a text in the format can yield, so 140 and 349
  # stay. An empty text is no text, and the format is matched against the
  # whole text: a trailing space or line break, or a space inside, fails it.
  p <- gestation_patients(
    c(NA, NA, NA, NA, NA, NA, "", "19+6", "25+7", "25 +4", "25+4\n", "25+4 "),
    c(0, 139, 140, 349, 350, NA, 0, 139, 140, 200, 350, 100))

  r <- neoipcr:::.reconcile_gestational_age(p)

  expect_identical(
    r$records$total_gestation_days,
    c(NA, NA, 140L, 349L, NA, NA, NA, NA, 140L, 200L, NA, NA))
  changes <- r$changes$`4`
  expect_equal(changes$patient_key, c(1L, 2L, 5L, 7L, 8L, 11L, 12L))
  expect_equal(changes$total_gestation_days, c(0L, 139L, 350L, 0L, 139L, 350L, 100L))
  expect_true(all(is.na(changes$total_gestation_days_reconciled)))
  expect_equal(nrow(r$changes$`3`), 0L)
})

test_that("the gestational-age reconciliations clear the audit fields of the totals they repair", {
  p <- gestation_patients(c("25+4", "25+4", NA), c(170, 179, 0))

  r <- neoipcr:::.reconcile_gestational_age(p)$records

  # The repaired totals of the first and third patient no longer have the
  # stored value's author and times; the second patient's stored value, and
  # the text, keep theirs.
  expect_identical(r$total_gestation_days_storedBy, c(NA, 7L, NA))
  expect_identical(
    r$total_gestation_days_createdAt,
    utc_time(c(NA, "2024-01-01 10:00", NA)))
  expect_identical(
    r$total_gestation_days_updatedAt,
    utc_time(c(NA, "2024-01-02 10:00", NA)))
  expect_identical(r$gest_age_storedBy, c(7L, 7L, 7L))
  expect_identical(r$gest_age, p$gest_age)
})

# --- Day of life (reconciliations 1 and 2) ---------------------------------

# Eight admissions, one per enrolment: 1 type 1 on day 150, 2 type 2 without
# a day of life, 3 type 3 on day 150, 4 without a type on day 150, 5 type 1
# already on day 1, 6 type 1 on day 5 as the second enrolment of patient 5,
# 7 and 8 two enrolments of patient 7 dated the same day, the second of type
# 2 on day 3.
admission_case <- function() {
  admission <- tibble::tibble(
    event_key     = 1:8,
    type          = factor(c("1", "2", "3", NA, "1", "1", "1", "2"),
                           levels = c("1", "2", "3")),
    dol           = c(150L, NA, 150L, 150L, 1L, 5L, 1L, 3L),
    los           = rep(0L, 8L),
    dol_storedBy  = rep(7L, 8L),
    dol_createdBy = rep(8L, 8L),
    dol_updatedBy = rep(9L, 8L),
    dol_createdAt = rep(utc_time("2024-01-01 10:00"), 8L),
    dol_updatedAt = rep(utc_time("2024-01-02 10:00"), 8L),
    los_storedBy  = rep(7L, 8L))
  enrollments <- tibble::tibble(
    enrollment_key = 1:8,
    patient_key    = c(1L, 2L, 3L, 4L, 5L, 5L, 7L, 7L),
    enrolledAt     = as.Date(c(
      "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
      "2024-01-01", "2024-02-01", "2024-03-01", "2024-03-01")))
  frame <- enrollments |>
    dplyr::mutate(
      event_key = .data$enrollment_key,
      .before   = 1L)
  list(admission = admission, frame = frame, enrollments = enrollments)
}

test_that("reconciliation 1 gives a type-1 or type-2 admission day of life 1", {
  case <- admission_case()

  r <- neoipcr:::.reconcile_admission_dol(case$admission, case$frame, case$enrollments)

  expect_identical(r$records$dol, c(1L, 1L, 150L, 150L, 1L, 5L, 1L, 1L))
  expect_equal(r$changes$enrollment_key, c(1L, 2L, 8L))
  expect_equal(r$changes$event_key, c(1L, 2L, 8L))
  expect_equal(r$changes$patient_key, c(1L, 2L, 7L))
  expect_equal(r$changes$reconciliation_id, rep(1L, 3L))
  expect_equal(as.character(r$changes$type), c("1", "2", "2"))
  expect_equal(r$changes$dol, c(150L, NA, 3L))
  expect_equal(r$changes$dol_reconciled, c(1L, 1L, 1L))
})

test_that("reconciliation 1 leaves a type-3, untyped or already correct admission, and a readmission, as stored", {
  case <- admission_case()

  r <- neoipcr:::.reconcile_admission_dol(case$admission, case$frame, case$enrollments)

  # Type 3 and no type are not the client's day 1; day 1 needs no repair;
  # patient 5's second enrolment follows an earlier one, which rule 47
  # reports; patient 7's two enrolments share a date, so neither follows the
  # other.
  expect_false(any(c(3L, 4L, 5L, 6L, 7L) %in% r$changes$event_key))
  expect_true(8L %in% r$changes$event_key)
  expect_identical(r$records$los, case$admission$los)
  # The five audit fields of the repaired values go, the others stay, and so
  # do the audit fields of another value on a repaired form.
  repaired <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  for (companion in c("dol_storedBy", "dol_createdBy", "dol_updatedBy"))
    expect_identical(
      r$records[[companion]],
      replace(case$admission[[companion]], repaired, NA), info = companion)
  for (companion in c("dol_createdAt", "dol_updatedAt"))
    expect_identical(is.na(r$records[[companion]]), repaired, info = companion)
  expect_identical(r$records$los_storedBy, case$admission$los_storedBy)

  # The earlier enrolment decides although the frame holds only the
  # readmission's form, as under a reporting period that leaves the first
  # stay out.
  readmission <- neoipcr:::.reconcile_admission_dol(
    case$admission[case$admission$event_key == 6L, ],
    case$frame[case$frame$event_key == 6L, ],
    case$enrollments)
  expect_equal(nrow(readmission$changes), 0L)
  expect_identical(readmission$records$dol, 5L)
})

# Enrolment 1, admitted on day 150 on 2024-01-01, and enrolment 2, admitted
# without a day of life on the same day, both repaired by reconciliation 1;
# enrolment 3 not repaired. Sepsis events 11 (derived from day 150), 12 (a
# day of life of its own), 14 (no date), 21 (derived from the missing value,
# which counts as 0), 22 (already what day 1 gives, but not derived) and 31
# (on the unrepaired enrolment), and a surgical site infection 13 without a
# day of life.
event_dol_case <- function() {
  frame <- tibble::tibble(
    event_key      = c(11L, 12L, 13L, 14L, 21L, 22L, 31L),
    enrollment_key = c(1L, 1L, 1L, 1L, 2L, 2L, 3L),
    patient_key    = c(1L, 1L, 1L, 1L, 2L, 2L, 3L),
    occurredAt     = as.Date(c(
      "2024-01-11", "2024-01-15", "2024-01-05", NA,
      "2024-01-03", "2024-01-03", "2024-01-11")))
  forms <- list(
    sepsisData = tibble::tibble(
      event_key     = c(11L, 12L, 14L, 21L, 22L, 31L),
      dol           = c(160L, 7L, NA, 2L, 3L, 160L),
      los           = c(10L, 14L, NA, 2L, 2L, 10L),
      dol_storedBy  = rep(7L, 6L),
      dol_createdBy = rep(8L, 6L),
      dol_updatedBy = rep(9L, 6L)),
    ssiData = tibble::tibble(
      event_key     = 13L,
      dol           = NA_integer_,
      los           = 4L,
      dol_storedBy  = 7L,
      dol_createdBy = 8L,
      dol_updatedBy = 9L))
  list(
    forms       = forms,
    repaired    = tibble::tibble(enrollment_key = c(1L, 2L), dol = c(150L, NA)),
    frame       = frame,
    enrollments = tibble::tibble(
      enrollment_key = 1:3,
      enrolledAt     = as.Date(rep("2024-01-01", 3L))))
}

test_that("reconciliation 2 derives again from day 1 a day of life the client derived from the stored admission value", {
  case <- event_dol_case()

  r <- neoipcr:::.reconcile_event_dol(
    case$forms, case$repaired, case$frame, case$enrollments)

  # The client's formula with the admission on day 1: one plus the days from
  # the enrolment date to the event date.
  expect_identical(r$records$sepsisData$dol, c(11L, 7L, NA, 3L, 3L, 160L))
  expect_identical(r$records$ssiData$dol, 5L)
  expect_setequal(r$changes$event_key, c(11L, 13L, 21L))
  changes <- r$changes[order(r$changes$event_key), ]
  expect_equal(changes$dol, c(160L, NA, 2L))
  expect_equal(changes$dol_reconciled, c(11L, 5L, 3L))
  expect_equal(changes$enrollment_key, c(1L, 1L, 2L))
  expect_equal(changes$reconciliation_id, rep(2L, 3L))
})

test_that("reconciliation 2 leaves a day of life the stored admission value does not explain", {
  case <- event_dol_case()

  r <- neoipcr:::.reconcile_event_dol(
    case$forms, case$repaired, case$frame, case$enrollments)

  # 12 has a value of its own and stays for rule 27; 22 matches day 1 but not
  # the stored admission value; 14 has no date to derive from; 31's
  # enrolment was not repaired. The day of occurrence does not change.
  expect_false(any(c(12L, 14L, 22L, 31L) %in% r$changes$event_key))
  expect_identical(r$records$sepsisData$los, case$forms$sepsisData$los)
  expect_identical(r$records$ssiData$los, 4L)
  expect_identical(r$records$sepsisData$dol_storedBy, c(NA, 7L, 7L, NA, 7L, 7L))
  expect_identical(r$records$sepsisData$dol_createdBy, c(NA, 8L, 8L, NA, 8L, 8L))
  expect_identical(r$records$sepsisData$dol_updatedBy, c(NA, 9L, 9L, NA, 9L, 9L))
  expect_identical(r$records$ssiData$dol_storedBy, NA_integer_)
  expect_identical(r$records$ssiData$dol_createdBy, NA_integer_)
  expect_identical(r$records$ssiData$dol_updatedBy, NA_integer_)
})

test_that("reconciliation 2 derives again the day of life of the necrotizing enterocolitis, pneumonia and procedure forms", {
  # Enrolment 1, admitted on 2024-01-01 with a type-1 admission form on day
  # 150, and a necrotizing enterocolitis, a pneumonia and a procedure form
  # whose days of life the client derived from that value.
  frame <- tibble::tibble(
    event_key      = 1:4,
    event_type_key = factor(c("adm", "nec", "hap", "pro"), levels = event_type_levels),
    enrollment_key = 1L,
    patient_key    = 1L,
    occurredAt     = as.Date(c("2024-01-01", "2024-01-05", "2024-01-08", "2024-01-11")))
  data <- list(
    admissionData           = make_test_admission_data(1L, dol = 150L),
    sepsisData              = make_test_sepsis_data(integer(0)),
    necData                 = make_test_nec_data(2L, dol = 154L),
    pneumoniaData           = make_test_pneumonia_data(3L, dol = 157L),
    surgeryData             = make_test_surgery_data(4L, dol = 160L),
    ssiData                 = make_test_ssi_data(integer(0)),
    infectiousAgentFindings = make_test_iaf(integer(0)),
    unknownPathogenNames    = make_test_unknown_pathogen_names())

  r <- neoipcr:::.reconcile_event_data(
    data,
    frame       = frame,
    enrollments = tibble::tibble(
      enrollment_key = 1L,
      patient_key    = 1L,
      enrolledAt     = as.Date("2024-01-01")),
    findings    = tibble::tibble(
      agent_finding_key = integer(),
      event_key         = integer(),
      secondary_bsi     = logical(),
      index             = integer(),
      pathogen_key      = integer()))

  expect_identical(r$data$necData$dol, 5L)
  expect_identical(r$data$pneumoniaData$dol, 8L)
  expect_identical(r$data$surgeryData$dol, 11L)
  expect_setequal(r$changes$`2`$event_key, 2:4)
  expect_equal(r$changes$`2`$dol_reconciled[order(r$changes$`2`$event_key)],
               c(5L, 8L, 11L))
})

# --- Hidden infectious-agent sections (reconciliations 5 and 6) -----------

test_that("reconciliation 5 removes the secondary-BSI section of a surgical site infection whose item is not Yes", {
  # SSI events 1 (No), 2 (No follow-up), 3 (unanswered), 4 (Yes), 5 (no form
  # row) and 6 (No, nothing in the section), a NEC event 7 and a pneumonia
  # event 8 with secondary agents under an item that is not Yes.
  frame <- tibble::tibble(
    event_key      = 1:8,
    event_type_key = factor(c(rep("ssi", 6L), "nec", "hap"), levels = event_type_levels),
    enrollment_key = 1L,
    patient_key    = 1L)
  ssi <- tibble::tibble(
    event_key = c(1L, 2L, 3L, 4L, 6L),
    sec_bsi   = factor(c("0", "-1", NA, "1", "0"), levels = c("1", "0", "-1")))
  findings <- tibble::tibble(
    agent_finding_key = 1:9,
    event_key         = c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 7L, 8L),
    secondary_bsi     = c(FALSE, rep(TRUE, 8L)),
    index             = c(1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L),
    pathogen_key      = c(42L, 77L, NA, 77L, 78L, 77L, 77L, 77L, 77L))

  r <- neoipcr:::.reconcile_ssi_secondary(findings, ssi, frame)

  # Every row of the section goes, a name companion stored on its own (2)
  # included; the primary agent of event 1 stays.
  expect_setequal(r$removed, c(2L, 3L, 4L, 5L, 7L))
  expect_equal(r$changes$event_key, c(1L, 2L, 3L, 5L))
  expect_equal(r$changes$slots, c(1L, 1L, 2L, 1L))
  expect_equal(r$changes$agents, c(1L, 0L, 2L, 1L))
  expect_equal(as.character(r$changes$sec_bsi), c("0", "-1", NA, NA))
  expect_equal(r$changes$action, rep("repair", 4L))
  expect_equal(r$changes$reconciliation_id, rep(5L, 4L))
})

# Sepsis forms keyed 1, 2, … with the items `forms` names set, every other
# item FALSE, and the no-positive-culture flag and the antibiotic therapy
# missing unless named: both are TRUE or missing in DHIS2.
sepsis_forms <- function(forms) {
  data <- make_test_sepsis_data(seq_along(forms))
  data$no_pos_culture <- NA
  data$ab_treatment <- NA
  for (i in seq_along(forms))
    for (item in names(forms[[i]]))
      data[[item]][i] <- forms[[i]][[item]]
  data
}

test_that("reconciliation 6 removes a culture-negative sepsis's agents where the form then meets the clinical-sepsis definition", {
  negative <- list(no_pos_culture = TRUE, ab_treatment = TRUE)
  sepsis <- sepsis_forms(list(
    c(negative, temperature = TRUE, apnoea = TRUE),        # 1 two signs, agent
    list(no_pos_culture = TRUE, temperature = TRUE),      # 2 no therapy, two agents
    c(negative, temperature = TRUE),                      # 3 one sign, companion only
    list(ab_treatment = TRUE, temperature = TRUE),        # 4 culture not negative
    c(negative, temperature = TRUE, apnoea = TRUE),       # 5 no infectious agent
    c(negative, temperature = TRUE, wbc = TRUE, crp = TRUE),  # 6 a sign and the laboratory
    c(negative, wbc = TRUE, crp = TRUE, platelet_count = TRUE),  # 7 the laboratory alone
    c(negative, temperature = TRUE, apnoea = TRUE)))      # 8 a secondary row only
  frame <- tibble::tibble(
    event_key      = 1:8,
    event_type_key = factor(rep("bsi", 8L), levels = event_type_levels),
    enrollment_key = 1L,
    patient_key    = 1L)
  findings <- tibble::tibble(
    agent_finding_key = 1:8,
    event_key         = c(1L, 2L, 2L, 3L, 4L, 6L, 7L, 8L),
    secondary_bsi     = c(rep(FALSE, 7L), TRUE),
    index             = c(1L, 2L, 3L, 1L, 1L, 1L, 1L, 1L),
    pathogen_key      = c(42L, 12L, 13L, NA, 42L, 42L, 42L, 42L))

  r <- neoipcr:::.reconcile_culture_negative(findings, sepsis, frame)

  changes <- r$changes[order(r$changes$event_key), ]
  expect_equal(changes$event_key, c(1L, 2L, 3L, 6L, 7L))
  # 1 and 6 meet the definition without their agent, the laboratory findings
  # counting as one feature beside the sign; 3 records no agent, only a name.
  # 2 lacks the therapy and 7 has one feature, so their agents stay.
  expect_equal(changes$action, c("repair", "report", "repair", "repair", "report"))
  expect_equal(changes$slots, c(1L, 2L, 1L, 1L, 1L))
  expect_equal(changes$agents, c(1L, 2L, 0L, 1L, 1L))
  expect_equal(changes$findings, c(2L, 1L, 1L, 2L, 1L))
  expect_equal(changes$reconciliation_id, rep(6L, 5L))
  expect_setequal(r$removed, c(1L, 4L, 6L))
})

# --- Reconciling a dataset -------------------------------------------------

# A dataset imported with every record and value as stored: three patients,
# the first with a stale total, the second with a total of 0 and no text;
# the first enrolment admitted on day 150 with a type-1 form, with a sepsis
# form derived from it and a surgical site infection derived from it whose
# secondary-BSI section, under No, holds an agent named on its own; the
# second a type-3 admission with a culture-negative sepsis that names an
# agent but lacks the therapy; the third a correct type-1 admission.
stored_values_ds <- function(dataset_options = dhis2_dataset_options(
                               include_department          = "full",
                               include_patient             = "full",
                               include_enrollment          = "full",
                               include_event               = "full",
                               include_invalid_patients    = TRUE,
                               include_ineligible_patients = TRUE,
                               reconcile                   = FALSE)) {
  metadata <- read_test_metadata()
  metadata$dataset_options <- dataset_options
  events <- make_test_events(6,
    enrollment_keys = c(1L, 1L, 1L, 2L, 2L, 3L),
    patient_keys    = c(1L, 1L, 1L, 2L, 2L, 3L),
    event_type_keys = c("adm", "bsi", "ssi", "adm", "bsi", "adm"),
    occurredAt      = as.Date(c(
      "2024-01-01", "2024-01-11", "2024-01-06",
      "2024-01-02", "2024-01-05", "2024-01-03")))
  make_test_ds(
    metadata      = metadata,
    patients      = make_test_patients(3,
      gest_age             = c("25+4", NA, "25+7"),
      total_gestation_days = c(170L, 0L, 140L)),
    enrollments   = make_test_enrollments(3),
    events        = events,
    admissionData = make_test_admission_data(c(1L, 4L, 6L),
      type = factor(c("1", "3", "1"), levels = c("1", "2", "3")),
      dol  = c(150L, 5L, 1L)),
    sepsisData    = make_test_sepsis_data(c(2L, 5L),
      dol            = c(160L, 4L),
      no_pos_culture = c(NA, TRUE),
      ab_treatment   = c(NA, NA),
      temperature    = c(FALSE, TRUE)),
    ssiData       = make_test_ssi_data(3L,
      dol     = 155L,
      sec_bsi = factor("0", levels = c("1", "0", "-1"))),
    infectiousAgentFindings = make_test_iaf(c(3L, 3L, 5L, 2L),
      secondary_bsi = c(TRUE, FALSE, FALSE, FALSE),
      pathogen_key  = c(NA, 42L, 42L, 42L),
      index         = c(1L, 1L, 1L, 1L)),
    unknownPathogenNames = make_test_unknown_pathogen_names(c(1L, 2L),
      name = c("Secondary name", "Primary name")))
}

test_that("the reconciliations of the forms remove the names of the findings they remove, and no others", {
  ds <- stored_values_ds()
  findings <- ds$infectiousAgentFindings |>
    dplyr::select("agent_finding_key", "event_key", "secondary_bsi", "index",
                  "pathogen_key")

  r <- neoipcr:::.reconcile_event_data(
    ds[c("admissionData", "sepsisData", "necData", "pneumoniaData",
         "surgeryData", "ssiData", "infectiousAgentFindings",
         "unknownPathogenNames")],
    frame       = ds$events |>
      dplyr::select("event_key", "event_type_key", "enrollment_key",
                    "patient_key", "occurredAt"),
    enrollments = ds$enrollments |>
      dplyr::select("enrollment_key", "patient_key", "enrolledAt"),
    findings    = findings)

  # The SSI's secondary section goes with its name; the reported sepsis keeps
  # its agent.
  expect_setequal(r$data$infectiousAgentFindings$agent_finding_key, c(2L, 3L, 4L))
  expect_identical(r$data$unknownPathogenNames$name, "Primary name")
  expect_named(r$changes, c("1", "2", "5", "6"))
  expect_identical(r$data$admissionData$dol, c(1L, 5L, 1L))
  expect_identical(r$data$sepsisData$dol, c(11L, 4L))
  expect_identical(r$data$ssiData$dol, 6L)
  # The culture result and the antibiotic therapy stay as recorded.
  expect_identical(r$data$sepsisData$no_pos_culture, ds$sepsisData$no_pos_culture)
  expect_identical(r$data$sepsisData$ab_treatment, ds$sepsisData$ab_treatment)
})

test_that("reconciliation_ids lists the reconciliations in ascending order", {
  expect_identical(reconciliation_ids(), 1:6)
})

test_that("reconciliation_details lists every record the reconciliations act on with its stored and reconciled values", {
  ds <- stored_values_ds()

  d <- reconciliation_details(ds)

  expect_named(d, c("reconciliation_id", "action", "patient_key",
                    "enrollment_key", "event_key", "context"))
  expect_identical(levels(d$action), c("repair", "report"))
  expect_equal(d$reconciliation_id, c(1L, 2L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(as.character(d$action),
               c(rep("repair", 6L), "report"))
  expect_equal(d$patient_key, c(1L, 1L, 1L, 1L, 2L, 1L, 2L))
  expect_equal(d$enrollment_key, c(1L, 1L, 1L, NA, NA, 1L, 2L))
  expect_equal(d$event_key, c(1L, 2L, 3L, NA, NA, 3L, 5L))
  context <- rlang::set_names(d$context, paste0(d$reconciliation_id, "_", seq_len(nrow(d))))
  expect_equal(as.list(context$`1_1`),
               list(type = factor("1", levels = c("1", "2", "3")),
                    dol = 150L, dol_reconciled = 1L))
  expect_equal(as.list(context$`2_2`), list(dol = 160L, dol_reconciled = 11L))
  expect_equal(as.list(context$`2_3`), list(dol = 155L, dol_reconciled = 6L))
  expect_equal(as.list(context$`3_4`),
               list(gest_age = "25+4", total_gestation_days = 170L,
                    total_gestation_days_reconciled = 179L))
  expect_equal(as.list(context$`4_5`),
               list(gest_age = NA_character_, total_gestation_days = 0L,
                    total_gestation_days_reconciled = NA_integer_))
  expect_equal(as.list(context$`5_6`),
               list(sec_bsi = factor("0", levels = c("1", "0", "-1")),
                    slots = 1L, agents = 0L))
  expect_equal(as.list(context$`6_7`), list(slots = 1L, agents = 1L, findings = 1L))
  # Each context is a plain tibble, not the class of the slot it came from.
  for (ctx in d$context)
    expect_identical(class(ctx), c("tbl_df", "tbl", "data.frame"))
})

test_that("reconciliation_details names the context fields its documentation declares", {
  ds <- stored_values_ds()
  d <- reconciliation_details(ds)
  declared <- rlang::set_names(
    lapply(neoipcr:::reconciliations, \(r) r$context),
    reconciliation_ids())
  for (i in seq_len(nrow(d)))
    expect_named(d$context[[i]], declared[[as.character(d$reconciliation_id[i])]])
  expect_identical(
    vapply(neoipcr:::reconciliations, \(r) r$level, character(1)),
    c("enrollment", "event", "patient", "patient", "event", "event"))
})

test_that("reconciliation_details returns no row for a dataset nothing in which needs reconciling", {
  ds <- stored_values_ds()
  ds$patients$total_gestation_days <- c(179L, 140L, 140L)
  ds$admissionData$dol <- c(1L, 5L, 1L)
  ds$sepsisData$no_pos_culture <- c(NA, NA)
  ds$ssiData$sec_bsi <- factor("1", levels = c("1", "0", "-1"))

  d <- reconciliation_details(ds)

  expect_equal(nrow(d), 0L)
  expect_named(d, c("reconciliation_id", "action", "patient_key",
                    "enrollment_key", "event_key", "context"))
})

test_that("reconciliation_details takes a dataset whose options predate the reconciliation", {
  ds <- stored_values_ds()
  ds$metadata$dataset_options$reconcile <- NULL
  expect_equal(nrow(reconciliation_details(ds)), 7L)
})

test_that("reconciliation_details refuses a dataset without every record and value as stored, naming what is missing", {
  refused <- function(...) {
    opts <- utils::modifyList(
      unclass(stored_values_ds()$metadata$dataset_options), list(...))
    ds <- stored_values_ds()
    ds$metadata$dataset_options <- structure(opts, class = c("neoipcr_dhis2_dsopt", "list"))
    expect_error(reconciliation_details(ds),
                 class = "neoipcr_reconciliation_needs_stored_values")
  }
  expect_match(conditionMessage(refused(reconcile = TRUE)), "`reconcile` is `TRUE`")
  expect_match(conditionMessage(refused(include_invalid_patients = FALSE)),
               "`include_invalid_patients` is not `TRUE`")
  exceptions <- tibble::tibble(RULE_ID = 3L)
  opts <- unclass(stored_values_ds()$metadata$dataset_options)
  opts["include_invalid_patients"] <- list(exceptions)
  ds <- stored_values_ds()
  ds$metadata$dataset_options <- structure(opts, class = c("neoipcr_dhis2_dsopt", "list"))
  expect_error(reconciliation_details(ds), "include_invalid_patients",
               class = "neoipcr_reconciliation_needs_stored_values")
  expect_match(conditionMessage(refused(include_ineligible_patients = FALSE)),
               "`include_ineligible_patients` is not `TRUE`")
  # A range filter judges the stored totals, and a reporting period hides
  # the earlier stays reconciliation 1 looks for.
  for (option in c("birth_weight_from", "birth_weight_to",
                   "gestational_age_from", "gestational_age_to"))
    expect_match(
      conditionMessage(do.call(refused, rlang::set_names(list(30L), option))),
      sprintf("`%s` is set: the range filter", option), info = option)
  for (option in c("surveillance_end_from", "surveillance_end_to"))
    expect_match(
      conditionMessage(do.call(
        refused, rlang::set_names(list(as.Date("2024-01-01")), option))),
      sprintf("`%s` is set: the reporting period", option), info = option)
  for (tier in c("include_patient", "include_enrollment", "include_event"))
    expect_match(
      conditionMessage(do.call(refused, rlang::set_names(list("pseudo"), tier))),
      sprintf("`%s` is \"pseudo\"", tier), info = tier)
  # A tier the options object lacks is named like any other.
  expect_match(conditionMessage(refused(include_event = NULL)),
               "`include_event` is `NULL`, not \"full\".", fixed = TRUE)
  expect_match(conditionMessage(refused(patient_columns = "id")),
               "leaves out \"gestational_age\"")
  # Every requirement missed is named at once.
  error <- refused(reconcile = TRUE, include_event = "pseudo")
  expect_match(conditionMessage(error), "`reconcile` is `TRUE`")
  expect_match(conditionMessage(error), "`include_event` is \"pseudo\"")
  ds <- stored_values_ds()
  ds$metadata$dataset_options <- NULL
  expect_error(reconciliation_details(ds), "no import options",
               class = "neoipcr_reconciliation_needs_stored_values")
})

# --- The readers' channels on their early returns --------------------------

test_that("the readers return typed, empty reconciliation inputs on their early returns", {
  opts <- dhis2_dataset_options(
    include_patient = "full", include_enrollment = "full",
    include_event = "full")

  patients <- neoipcr:::read_patients(tibble::tibble(), list(), opts)
  expect_identical(patients$reconciliation_log, neoipcr:::.reconciliation_log())
  expect_named(patients$reconciliation_log, c(
    "reconciliation_id", "action", "patient_key", "enrollment_key", "event_key"))
  expect_identical(
    neoipcr:::read_patients(
      tibble::tibble(), list(),
      dhis2_dataset_options(include_patient = "no"))$reconciliation_log,
    neoipcr:::.reconciliation_log())

  enrollments <- neoipcr:::read_enrollments(tibble::tibble(), tibble::tibble(), list(), opts)
  expect_s3_class(enrollments$internal_map$enrolledAt, "Date")

  events <- neoipcr:::read_events(tibble::tibble(), tibble::tibble(), list(), opts)
  map <- events$internal_map
  expect_named(map, c("event_key", "event", "event_type_key", "enrollment_key",
                      "patient_key", "occurredAt"))
  expect_type(map$enrollment_key, "integer")
  expect_s3_class(map$occurredAt, "Date")

  findings <- neoipcr:::read_infectious_agent_findings(
    tibble::tibble(), build_processed_events(1L, "bsi"),
    build_reader_metadata("bsi"), opts)
  expect_identical(
    lapply(findings$internal_map, class),
    list(agent_finding_key = "integer", event_key = "integer",
         secondary_bsi = "logical", index = "integer",
         pathogen_key = "integer"))
})

test_that("the findings reader's map carries every finding's slot and concept under either event tier", {
  events_raw <- build_raw_pathogen_events(
    event_keys      = c(1L, 2L),
    event_type_keys = c("bsi", "ssi"),
    pathogen_rows   = list(
      list("1" = list(pathogen = "42"), "2" = list(mrsa = "1")),
      list("1" = list(pathogen = "77"))))
  processed <- dplyr::bind_rows(
    build_processed_events(1L, "bsi"), build_processed_events(2L, "ssi"))
  metadata <- build_reader_metadata(c("bsi", "ssi"))

  for (tier in c("pseudo", "full")) {
    opts <- dhis2_dataset_options(
      include_patient = "full", include_enrollment = "full",
      include_event = tier)
    pair <- neoipcr:::read_infectious_agent_findings(
      events_raw, processed, metadata, opts)
    map <- pair$internal_map[order(pair$internal_map$event_key, pair$internal_map$index), ]
    expect_equal(map$event_key, c(1L, 1L, 2L), info = tier)
    expect_equal(map$index, c(1L, 2L, 1L), info = tier)
    expect_equal(map$pathogen_key, c(42L, NA, 77L), info = tier)
    expect_equal(map$secondary_bsi, c(FALSE, FALSE, FALSE), info = tier)
    expect_setequal(map$agent_finding_key, pair$infectiousAgentFindings$agent_finding_key)
  }
})
