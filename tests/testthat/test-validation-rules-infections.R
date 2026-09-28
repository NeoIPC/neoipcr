# Tests for R/validation-rules-infections.R — rules 49, 50, 55.

event_status <- function(x)
  factor(x, levels = c("ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED"))

dev_ass <- function(x)
  factor(x, levels = c("0", "1", "2"))

sec_bsi <- function(x)
  factor(x, levels = c("1", "0", "-1"))

# --- Rule 49: the same infection type repeated within 14 days ---

# Events of `types` on `dates` (event key i), the i-th on enrolment
# `enrollment_keys[i]` of patient `patient_keys[i]`; an enrolment belongs to
# the patient of its first event.
infection_sequence_ds <- function(dates,
                                  types           = rep("bsi", length(dates)),
                                  patient_keys    = rep(1L, length(dates)),
                                  enrollment_keys = rep(1L, length(dates))) {
  n <- length(dates)
  enrolments <- seq_len(max(enrollment_keys))
  make_test_ds(
    patients    = make_test_patients(max(patient_keys)),
    enrollments = make_test_enrollments(length(enrolments),
      patient_keys = patient_keys[match(enrolments, enrollment_keys)]),
    events = make_test_events(n,
      enrollment_keys = enrollment_keys,
      patient_keys    = patient_keys,
      event_type_keys = types,
      occurredAt = as.Date(dates)))
}

test_that("rule 49 detects a sepsis recorded within 14 days of the previous one", {
  ds <- infection_sequence_ds(c("2024-01-01", "2024-01-10"))
  result <- neoipcr:::validation_rule_49(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 49L)
  # The later event of the pair is the finding.
  expect_equal(result$event_key, 2L)
  expect_equal(result$enrollment_key, 1L)
  expect_named(result$context[[1]], c("occurredAt", "occurredAt_previous", "days_between"))
  expect_equal(result$context[[1]]$occurredAt, as.Date("2024-01-10"))
  expect_equal(result$context[[1]]$occurredAt_previous, as.Date("2024-01-01"))
  expect_equal(result$context[[1]]$days_between, 9L)
})

test_that("rule 49 flags 13 days between two infections and accepts 14", {
  expect_equal(nrow(neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-14")), NULL)), 1L)
  expect_equal(nrow(neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-15")), NULL)), 0L)
})

test_that("rule 49 compares each infection with the previous one of its type", {
  # Three within a month, each within 14 days of its predecessor: the second
  # and the third are found, each against the one before it.
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10", "2024-01-20")), NULL)
  expect_equal(result$event_key, c(2L, 3L))
  expect_equal(result$context[[2]]$occurredAt_previous, as.Date("2024-01-10"))
  # Two infections on one day: the later key is the repeat, at zero days.
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-05", "2024-01-05")), NULL)
  expect_equal(result$event_key, 2L)
  expect_equal(result$context[[1]]$days_between, 0L)
  # The sequence is by date, not by key.
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-10", "2024-01-01")), NULL)
  expect_equal(result$event_key, 1L)
})

test_that("rule 49 spans the patient's enrolments but not other patients or other types", {
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10"), enrollment_keys = c(1L, 2L)), NULL)
  expect_equal(nrow(result), 1L)
  # The finding is the later event's, on its own enrolment.
  expect_equal(result$event_key, 2L)
  expect_equal(result$enrollment_key, 2L)
  expect_equal(nrow(neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10"),
                          patient_keys = c(1L, 2L), enrollment_keys = c(1L, 2L)), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10"), types = c("bsi", "hap")), NULL)), 0L)
  # Every infection type is judged; a surgical procedure is not an infection.
  for (type in c("nec", "hap", "ssi"))
    expect_equal(nrow(neoipcr:::validation_rule_49(
      infection_sequence_ds(c("2024-01-01", "2024-01-10"), types = c(type, type)), NULL)), 1L,
      info = type)
  expect_equal(nrow(neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10"), types = c("pro", "pro")), NULL)), 0L)
})

test_that("rule 49 leaves an undated infection out of the sequence", {
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", NA, "2024-01-10")), NULL)
  expect_equal(result$event_key, 3L)
  expect_equal(result$context[[1]]$occurredAt_previous, as.Date("2024-01-01"))
})

test_that("rule 49 honours exceptions", {
  result <- neoipcr:::validation_rule_49(
    infection_sequence_ds(c("2024-01-01", "2024-01-10")),
    make_test_exceptions(49L, event_key = 2L))
  expect_equal(nrow(result), 0L)
})

# --- Rule 50: device association without device days ---

# One enrolment with a sepsis event (key 1), a pneumonia event (key 2) and a
# surveillance-end event (key 3) of status `end`; the infection forms record
# the associations `bsi` and `hap`, the surveillance-end form takes `...`.
device_ds <- function(bsi = "0", hap = "0", end = "COMPLETED", ...)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(3,
      event_type_keys = c("bsi", "hap", "end"),
      status = event_status(c("COMPLETED", "COMPLETED", end))),
    sepsisData          = make_test_sepsis_data(1L, dev_ass = dev_ass(bsi)),
    pneumoniaData       = make_test_pneumonia_data(2L, dev_ass = dev_ass(hap)),
    surveillanceEndData = make_test_surveillance_end_data(3L, ...))

test_that("rule 50 detects a CVC-associated sepsis on an enrolment without CVC days", {
  ds <- device_ds(bsi = "1", cvc_days = 0L)
  result <- neoipcr:::validation_rule_50(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 50L)
  expect_equal(result$event_key, 1L)
  expect_equal(result$enrollment_key, 1L)
  expect_named(result$context[[1]], c("device", "device_days"))
  expect_equal(result$context[[1]]$device, "cvc")
  expect_equal(result$context[[1]]$device_days, 0L)
})

test_that("rule 50 reads each association against its own device's days", {
  cases <- list(
    list(bsi = "2", count = "pvc_days", device = "pvc", event = 1L),
    list(hap = "1", count = "niv_days", device = "niv", event = 2L),
    list(hap = "2", count = "inv_days", device = "inv", event = 2L))
  for (case in cases) {
    args <- c(case[intersect(names(case), c("bsi", "hap"))],
              rlang::set_names(list(0L), case$count))
    result <- neoipcr:::validation_rule_50(do.call(device_ds, args), NULL)
    expect_equal(nrow(result), 1L, info = case$device)
    expect_equal(result$event_key, case$event)
    expect_equal(result$context[[1]]$device, case$device)
    # Another device's count at zero says nothing about this one.
    other <- rlang::set_names(list(0L), setdiff(
      c("cvc_days", "pvc_days", "niv_days", "inv_days"), case$count)[1])
    expect_equal(nrow(neoipcr:::validation_rule_50(
      do.call(device_ds, c(case[intersect(names(case), c("bsi", "hap"))], other)), NULL)), 0L,
      info = case$device)
  }
})

test_that("rule 50 records one finding per device-associated infection", {
  result <- neoipcr:::validation_rule_50(
    device_ds(bsi = "1", hap = "2", cvc_days = 0L, inv_days = 0L), NULL)
  expect_equal(result$event_key, c(1L, 2L))
  expect_equal(vapply(result$context, \(ctx) ctx$device, character(1)), c("cvc", "inv"))
})

test_that("rule 50 reads the surveillance-end form of the infection's own enrolment", {
  # The patient's other enrolment has no CVC days; the infection's own has.
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 1L)),
    events = make_test_events(3,
      enrollment_keys = c(1L, 1L, 2L),
      event_type_keys = c("bsi", "end", "end")),
    sepsisData          = make_test_sepsis_data(1L, dev_ass = dev_ass("1")),
    surveillanceEndData = make_test_surveillance_end_data(c(2L, 3L), cvc_days = c(3L, 0L)))
  expect_equal(nrow(neoipcr:::validation_rule_50(ds, NULL)), 0L)
  ds$surveillanceEndData$cvc_days <- c(0L, 3L)
  expect_equal(nrow(neoipcr:::validation_rule_50(ds, NULL)), 1L)
})

test_that("rule 50 counts a missing device count like zero", {
  result <- neoipcr:::validation_rule_50(device_ds(bsi = "1", cvc_days = NA_integer_), NULL)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$context[[1]]$device_days))
})

test_that("rule 50 returns no rows for an association with device days or for no association", {
  expect_equal(nrow(neoipcr:::validation_rule_50(device_ds(bsi = "1", cvc_days = 1L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_50(device_ds(bsi = "0", cvc_days = 0L, pvc_days = 0L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_50(device_ds(hap = "0", niv_days = 0L, inv_days = 0L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_50(device_ds(bsi = NA, cvc_days = 0L), NULL)), 0L)
})

test_that("rule 50 does not judge an enrolment whose surveillance-end form is not completed or missing", {
  expect_equal(nrow(neoipcr:::validation_rule_50(device_ds(bsi = "1", end = "ACTIVE", cvc_days = 0L), NULL)), 0L)
  ds <- device_ds(bsi = "1", cvc_days = 0L)
  ds$events <- ds$events[ds$events$event_type_key != "end", ]
  expect_equal(nrow(neoipcr:::validation_rule_50(ds, NULL)), 0L)
  # Without a status column every event is completed, so the form counts.
  ds <- device_ds(bsi = "1", end = "ACTIVE", cvc_days = 0L)
  ds$events$status <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_50(ds, NULL))
  expect_equal(nrow(result), 1L)
})

test_that("rule 50 honours exceptions", {
  result <- neoipcr:::validation_rule_50(
    device_ds(bsi = "1", cvc_days = 0L), make_test_exceptions(50L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 50 skips without a warning when an association or a device count is absent", {
  ds <- device_ds(bsi = "1", cvc_days = 0L)
  ds$sepsisData$dev_ass <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_50(ds, NULL))
  expect_null(result)
  ds <- device_ds(bsi = "1", cvc_days = 0L)
  ds$pneumoniaData$dev_ass <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_50(ds, NULL))
  expect_null(result)
  for (col in c("cvc_days", "pvc_days", "niv_days", "inv_days")) {
    ds <- device_ds(bsi = "1", cvc_days = 0L)
    ds$surveillanceEndData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_50(ds, NULL))
    expect_null(result)
  }
})

# --- Rule 55: the secondary-BSI item against the secondary-BSI organisms ---

# One enrolment with a NEC event (key 1), a pneumonia event (key 2) and an
# SSI event (key 3), whose secondary-BSI items are `nec`, `hap` and `ssi`,
# with `organisms` secondary-BSI findings and `primary` primary findings on
# the event `on`.
secondary_bsi_ds <- function(nec = "0", hap = "0", ssi = "0",
                             organisms = 0L, primary = 0L, on = 1L) {
  n <- organisms + primary
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(3, event_type_keys = c("nec", "hap", "ssi")),
    necData       = make_test_nec_data(1L, sec_bsi = sec_bsi(nec)),
    pneumoniaData = make_test_pneumonia_data(2L, sec_bsi = sec_bsi(hap)),
    ssiData       = make_test_ssi_data(3L, sec_bsi = sec_bsi(ssi)),
    infectiousAgentFindings = make_test_iaf(
      rep(on, n),
      secondary_bsi = c(rep(TRUE, organisms), rep(FALSE, primary))))
}

test_that("rule 55 detects the item Yes without a secondary-BSI organism on each form", {
  forms <- list(nec = 1L, hap = 2L, ssi = 3L)
  for (form in names(forms)) {
    ds <- do.call(secondary_bsi_ds, rlang::set_names(list("1"), form))
    result <- neoipcr:::validation_rule_55(ds, NULL)
    expect_equal(nrow(result), 1L, info = form)
    expect_declared_context(result)
    expect_equal(result$rule_id, 55L)
    expect_equal(result$event_key, forms[[form]])
    expect_named(result$context[[1]], c("sec_bsi", "organisms"))
    expect_equal(as.character(result$context[[1]]$sec_bsi), "1")
    expect_equal(result$context[[1]]$organisms, 0L)
  }
})

test_that("rule 55 detects secondary-BSI organisms under an item that is not Yes on a NEC or pneumonia form", {
  forms <- list(nec = 1L, hap = 2L)
  for (form in names(forms)) {
    for (item in c("0", "-1", NA)) {
      ds <- do.call(secondary_bsi_ds,
        c(rlang::set_names(list(item), form), list(organisms = 2L, on = forms[[form]])))
      result <- neoipcr:::validation_rule_55(ds, NULL)
      expect_equal(nrow(result), 1L, info = paste(form, item))
      expect_equal(result$event_key, forms[[form]])
      expect_equal(as.character(result$context[[1]]$sec_bsi), item)
      expect_equal(result$context[[1]]$organisms, 2L)
    }
  }
})

test_that("rule 55 leaves infectious agents under a No on an SSI form to the NeoIPC coordinating centre", {
  # They sit in a section the client hides whatever it holds, so the team
  # cannot see them.
  for (item in c("0", "-1", NA))
    expect_equal(nrow(neoipcr:::validation_rule_55(
      secondary_bsi_ds(ssi = item, organisms = 1L, on = 3L), NULL)), 0L, info = item)
})

test_that("rule 55 returns no rows when the item and the organisms agree", {
  for (form in c("nec", "hap", "ssi")) {
    on <- match(form, c("nec", "hap", "ssi"))
    expect_equal(nrow(neoipcr:::validation_rule_55(
      do.call(secondary_bsi_ds, rlang::set_names(list("1", 1L, on), c(form, "organisms", "on"))), NULL)), 0L,
      info = form)
    expect_equal(nrow(neoipcr:::validation_rule_55(
      do.call(secondary_bsi_ds, rlang::set_names(list("0"), form)), NULL)), 0L, info = form)
  }
})

test_that("rule 55 counts the secondary-BSI organisms only", {
  # A primary organism is no secondary one: Yes stays without an organism,
  # and No stays consistent.
  result <- neoipcr:::validation_rule_55(secondary_bsi_ds(hap = "1", primary = 1L, on = 2L), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$organisms, 0L)
  expect_equal(nrow(neoipcr:::validation_rule_55(
    secondary_bsi_ds(hap = "0", primary = 1L, on = 2L), NULL)), 0L)
})

test_that("rule 55 counts a finding without an organism as none", {
  # A resistance or name companion stored on its own leaves a finding row
  # whose pathogen is missing: Yes is still without an organism, and No has
  # none recorded against it.
  ds <- secondary_bsi_ds(hap = "1", organisms = 1L, on = 2L)
  ds$infectiousAgentFindings$pathogen_key <- NA_integer_
  result <- neoipcr:::validation_rule_55(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$organisms, 0L)
  ds <- secondary_bsi_ds(hap = "0", organisms = 1L, on = 2L)
  ds$infectiousAgentFindings$pathogen_key <- NA_integer_
  expect_equal(nrow(neoipcr:::validation_rule_55(ds, NULL)), 0L)
})

test_that("rule 55 reads an event without a form row as an unanswered item", {
  # A pneumonia whose only stored values are organisms has no form row: its
  # organisms are recorded under an item never answered.
  ds <- secondary_bsi_ds(organisms = 2L, on = 2L)
  ds$pneumoniaData <- ds$pneumoniaData[0L, ]
  result <- neoipcr:::validation_rule_55(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$event_key, 2L)
  expect_true(is.na(result$context[[1]]$sec_bsi))
  expect_equal(result$context[[1]]$organisms, 2L)
  # Without organisms such an event has nothing to disagree about.
  ds <- secondary_bsi_ds()
  ds$pneumoniaData <- ds$pneumoniaData[0L, ]
  expect_equal(nrow(neoipcr:::validation_rule_55(ds, NULL)), 0L)
})

test_that("rule 55 honours exceptions", {
  result <- neoipcr:::validation_rule_55(
    secondary_bsi_ds(nec = "1"), make_test_exceptions(55L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 55 skips without a warning when an item or the findings' flag is absent", {
  for (slot in c("necData", "pneumoniaData", "ssiData")) {
    ds <- secondary_bsi_ds(nec = "1")
    ds[[slot]]$sec_bsi <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_55(ds, NULL))
    expect_null(result)
  }
  for (col in c("secondary_bsi", "pathogen_key")) {
    ds <- secondary_bsi_ds(nec = "1")
    ds$infectiousAgentFindings[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_55(ds, NULL))
    expect_null(result)
  }
})
