# Tests for R/validation-rules-definitions.R — rules 59 to 61.

event_status_levels <- c(
  "ACTIVE", "COMPLETED", "VISITED", "SCHEDULE", "OVERDUE", "SKIPPED")

# A dataset of one patient and enrolment with one event of `type` per element
# of `forms`, keyed 1, 2, …, whose form row is `form_data` with each form's
# named values set on its row, and with the findings `findings` (none by
# default). `status` gives the events' statuses, recycled.
definition_ds <- function(type, slot, form_data, forms, findings = NULL,
                          status = "COMPLETED")
{
  n <- length(forms)
  for (i in seq_len(n))
    for (item in names(forms[[i]]))
      form_data[[item]][i] <- forms[[i]][[item]]
  args <- list(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events      = make_test_events(n,
      event_type_keys = rep(type, n),
      status          = factor(rep_len(status, n), levels = event_status_levels)),
    infectiousAgentFindings = if (is.null(findings)) make_test_iaf(integer(0))
                              else findings)
  args[[slot]] <- form_data
  do.call(make_test_ds, args)
}

# Findings rows on the events `event_keys`, primary slot 1 unless `index` or
# `secondary_bsi` say otherwise, each naming the concept `pathogen_key`.
agent_rows <- function(event_keys, pathogen_key = rep(1L, length(event_keys)),
                       index = rep(1L, length(event_keys)),
                       secondary_bsi = rep(FALSE, length(event_keys)))
  make_test_iaf(event_keys,
    pathogen_key  = as.integer(pathogen_key),
    index         = as.integer(index),
    secondary_bsi = secondary_bsi)

# --- Rule 59: clinical sepsis ---

bsi_signs <- c("temperature", "bradycardia", "perfusion", "apnoea",
               "feeding_intolerance", "irritability", "acidosis",
               "hyperglycaemia")
bsi_lab <- c("wbc", "platelet_count", "crp", "procalcitonin", "it_ratio",
             "interleukin")

# Sepsis events with the forms `forms`, every item FALSE unless a form names
# it: a culture-negative form with the antibiotic therapy initiated is
# `sepsis(...)` with the signs and findings named.
sepsis_ds <- function(forms, ...)
  definition_ds("bsi", "sepsisData", make_test_sepsis_data(seq_along(forms)),
                forms, ...)

# A culture-negative form with the antibiotic therapy initiated and the items
# `present` present.
culture_negative <- function(present = character())
  c(list(no_pos_culture = TRUE, ab_treatment = TRUE),
    rlang::set_names(as.list(rep(TRUE, length(present))), present))

test_that("rule 59 detects a culture-negative sepsis with fewer than two features", {
  ds <- sepsis_ds(list(culture_negative("temperature"),
                       culture_negative(c("temperature", "apnoea"))))
  result <- neoipcr:::validation_rule_59(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 59L)
  expect_equal(result$patient_key, 1L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "findings")
  expect_identical(result$context[[1]]$findings, 1L)
})

test_that("rule 59 returns no rows for a clinical sepsis that meets the definition or a form with an agent", {
  expect_equal(nrow(neoipcr:::validation_rule_59(
    sepsis_ds(list(culture_negative(c("temperature", "apnoea")))), NULL)), 0L)
  # A form that records an agent is a laboratory-confirmed infection, which
  # this definition does not describe, whatever else it records.
  expect_equal(nrow(neoipcr:::validation_rule_59(
    sepsis_ds(list(list(), culture_negative()), findings = agent_rows(c(1L, 2L))),
    NULL)), 0L)
})

test_that("rule 59 counts the laboratory findings as one feature together", {
  # Two laboratory findings without a sign are one feature; a sign with them
  # makes two. The platelet count is a laboratory finding.
  counted <- function(present)
    neoipcr:::validation_rule_59(sepsis_ds(list(culture_negative(present))), NULL)
  expect_identical(counted(c("wbc", "crp"))$context[[1]]$findings, 1L)
  expect_identical(counted(bsi_lab)$context[[1]]$findings, 1L)
  expect_identical(counted(c("platelet_count", "wbc"))$context[[1]]$findings, 1L)
  expect_equal(nrow(counted(c("temperature", "wbc", "crp"))), 0L)
  expect_equal(nrow(counted(c("temperature", "platelet_count"))), 0L)
  # Every clinical sign counts as a feature of its own.
  for (sign in bsi_signs) {
    expect_identical(counted(sign)$context[[1]]$findings, 1L, info = sign)
    expect_equal(nrow(counted(c(sign, "interleukin"))), 0L, info = sign)
  }
  expect_equal(nrow(counted(bsi_signs[1:2])), 0L)
})

test_that("rule 59 detects a sepsis without an agent whose antibiotic therapy or negative culture is not recorded", {
  # Without the antibiotic therapy the definition is not met however many
  # features are present.
  no_treatment <- culture_negative(bsi_signs)
  no_treatment$ab_treatment <- FALSE
  result <- neoipcr:::validation_rule_59(sepsis_ds(list(no_treatment)), NULL)
  expect_equal(nrow(result), 1L)
  expect_identical(result$context[[1]]$findings, 8L)
  # A form with neither a negative culture nor an agent describes neither a
  # clinical sepsis nor a laboratory-confirmed one; the client refuses it
  # only by making the first agent mandatory while the culture is not
  # recorded as negative.
  neither <- culture_negative(c("temperature", "apnoea"))
  neither$no_pos_culture <- FALSE
  expect_equal(nrow(neoipcr:::validation_rule_59(sepsis_ds(list(neither)), NULL)), 1L)
})

test_that("rule 59 takes a slot naming a concept for an agent, and a companion alone for none", {
  # Code 0, "Not listed", names a concept.
  expect_equal(nrow(neoipcr:::validation_rule_59(
    sepsis_ds(list(culture_negative()), findings = agent_rows(1L, pathogen_key = 0L)),
    NULL)), 0L)
  # A resistance or name companion stored on its own, a secondary-BSI row,
  # and an agent on another form record no agent on this one.
  for (findings in list(
    agent_rows(1L, pathogen_key = NA_integer_),
    agent_rows(1L, secondary_bsi = TRUE),
    agent_rows(2L))) {
    result <- neoipcr:::validation_rule_59(
      sepsis_ds(list(culture_negative(), culture_negative()), findings = findings),
      NULL)
    expect_true(1L %in% result$event_key)
  }
})

test_that("rule 59 reads a missing item as not present", {
  # Every item missing, the form row included: no negative culture, no
  # therapy, no feature.
  all_missing <- rlang::set_names(
    as.list(rep(NA, length(c(bsi_signs, bsi_lab)) + 2L)),
    c("no_pos_culture", "ab_treatment", bsi_signs, bsi_lab))
  result <- neoipcr:::validation_rule_59(sepsis_ds(list(all_missing)), NULL)
  expect_equal(nrow(result), 1L)
  expect_identical(result$context[[1]]$findings, 0L)
  ds <- sepsis_ds(list(culture_negative()))
  ds$sepsisData <- ds$sepsisData[0L, ]
  result <- neoipcr:::validation_rule_59(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_identical(result$context[[1]]$findings, 0L)
  # One item present among missing ones is counted, not lost to the missing.
  for (item in c(bsi_signs, bsi_lab)) {
    form <- all_missing
    form$no_pos_culture <- TRUE
    form$ab_treatment <- TRUE
    form[[item]] <- TRUE
    result <- neoipcr:::validation_rule_59(sepsis_ds(list(form)), NULL)
    expect_identical(result$context[[1]]$findings, 1L, info = item)
  }
  # A missing negative culture or therapy is none.
  for (item in c("no_pos_culture", "ab_treatment")) {
    form <- culture_negative(c("temperature", "apnoea"))
    form[[item]] <- NA
    expect_equal(nrow(neoipcr:::validation_rule_59(sepsis_ds(list(form)), NULL)), 1L,
                 info = item)
  }
})

test_that("rule 59 judges only completed forms", {
  ds <- sepsis_ds(list(culture_negative(), culture_negative()),
                  status = c("ACTIVE", "COMPLETED"))
  expect_equal(neoipcr:::validation_rule_59(ds, NULL)$event_key, 2L)
  # Without a status column every event is completed.
  ds$events$status <- NULL
  expect_equal(neoipcr:::validation_rule_59(ds, NULL)$event_key, c(1L, 2L))
})

test_that("rule 59 honours exceptions", {
  result <- neoipcr:::validation_rule_59(
    sepsis_ds(list(culture_negative(), culture_negative())),
    make_test_exceptions(59L, event_key = 1L))
  expect_equal(result$event_key, 2L)
})

test_that("rule 59 skips without a warning when an item it reads is absent", {
  for (col in c("no_pos_culture", "ab_treatment", bsi_signs, bsi_lab)) {
    ds <- sepsis_ds(list(culture_negative()))
    ds$sepsisData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_59(ds, NULL))
    expect_null(result, info = col)
  }
  for (col in c("secondary_bsi", "index", "pathogen_key")) {
    ds <- sepsis_ds(list(culture_negative()))
    ds$infectiousAgentFindings[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_59(ds, NULL))
    expect_null(result, info = col)
  }
})

test_that("the clinical-sepsis count adds its two columns and changes none", {
  # The reconciliation of a culture-negative sepsis reads the same count.
  forms <- tibble::tibble(
    event_key      = 1:3,
    no_pos_culture = c(TRUE, TRUE, NA),
    ab_treatment   = c(TRUE, NA, TRUE))
  for (item in c(bsi_signs, bsi_lab))
    forms[[item]] <- NA
  forms$temperature <- c(TRUE, TRUE, TRUE)
  forms$wbc         <- c(TRUE, TRUE, TRUE)
  forms$crp         <- c(TRUE, FALSE, NA)
  result <- neoipcr:::.bsi_clinical_sepsis(forms)
  expect_identical(result[names(forms)], forms)
  expect_identical(result$findings, c(2L, 2L, 2L))
  expect_identical(result$clinical_sepsis, c(TRUE, FALSE, FALSE))
})

# --- Rule 60: necrotizing enterocolitis ---

nec_imaging  <- c("fixed_loop", "pneumatosis_intestinalis_img",
                  "pneumoperitoneum", "portal_venous_gas")
nec_clinical <- c("abdominal_distension", "abdominal_skin_tone",
                  "bloody_stools", "bilious_aspirate", "gastric_residuals",
                  "vomiting")
nec_surgical <- c("bowel_necrosis", "pneumatosis_intestinalis_surg")

# NEC events with the forms `forms`, each a character vector of the findings
# present, every other finding FALSE.
nec_ds <- function(forms, ...)
  definition_ds("nec", "necData", make_test_nec_data(seq_along(forms)),
                lapply(forms, \(present)
                  rlang::set_names(as.list(rep(TRUE, length(present))), present)),
                ...)

test_that("rule 60 detects a NEC form with imaging findings but no clinical or surgical one", {
  result <- neoipcr:::validation_rule_60(
    nec_ds(list("fixed_loop", c("fixed_loop", "vomiting"))), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 60L)
  expect_equal(result$patient_key, 1L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]],
               c("imaging_count", "clinical_count", "surgical_count"))
  expect_identical(result$context[[1]]$imaging_count, 1L)
  expect_identical(result$context[[1]]$clinical_count, 0L)
  expect_identical(result$context[[1]]$surgical_count, 0L)
})

test_that("rule 60 returns no rows for a form that meets either definition", {
  forms <- list(
    c("pneumoperitoneum", "bloody_stools"),
    "bowel_necrosis",
    "pneumatosis_intestinalis_surg",
    c(nec_imaging, nec_clinical))
  expect_equal(nrow(neoipcr:::validation_rule_60(nec_ds(forms), NULL)), 0L)
})

test_that("rule 60 counts each finding in its own group", {
  counts <- function(present) {
    result <- neoipcr:::validation_rule_60(nec_ds(list(present)), NULL)
    if (nrow(result) == 0L) return(NULL)
    unlist(result$context[[1]])
  }
  # Clinical findings alone, however many, are not enough.
  expect_equal(counts(nec_clinical),
               c(imaging_count = 0L, clinical_count = 6L, surgical_count = 0L))
  expect_equal(counts(nec_imaging),
               c(imaging_count = 4L, clinical_count = 0L, surgical_count = 0L))
  for (item in nec_imaging)
    expect_equal(counts(item)[["imaging_count"]], 1L, info = item)
  for (item in nec_clinical)
    expect_equal(counts(item)[["clinical_count"]], 1L, info = item)
  for (item in nec_surgical)
    expect_null(counts(item), info = item)
})

test_that("rule 60 reads a missing finding as not present", {
  all_missing <- rlang::set_names(
    as.list(rep(NA, length(c(nec_imaging, nec_clinical, nec_surgical)))),
    c(nec_imaging, nec_clinical, nec_surgical))
  ds <- definition_ds("nec", "necData", make_test_nec_data(1L), list(all_missing))
  result <- neoipcr:::validation_rule_60(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(unlist(result$context[[1]]),
               c(imaging_count = 0L, clinical_count = 0L, surgical_count = 0L))
  # A finding present among missing ones is counted in its group, and a
  # surgical one meets the definition.
  for (item in c(nec_imaging, nec_clinical, nec_surgical)) {
    form <- all_missing
    form[[item]] <- TRUE
    result <- neoipcr:::validation_rule_60(
      definition_ds("nec", "necData", make_test_nec_data(1L), list(form)), NULL)
    if (item %in% nec_surgical)
      expect_equal(nrow(result), 0L, info = item)
    else
      expect_equal(sum(unlist(result$context[[1]])), 1L, info = item)
  }
  # An event of which only infectious agents are stored has no form row.
  ds <- nec_ds(list(character()))
  ds$necData <- ds$necData[0L, ]
  expect_equal(neoipcr:::validation_rule_60(ds, NULL)$event_key, 1L)
})

test_that("rule 60 judges only completed forms", {
  ds <- nec_ds(list("vomiting", "vomiting"), status = c("ACTIVE", "COMPLETED"))
  expect_equal(neoipcr:::validation_rule_60(ds, NULL)$event_key, 2L)
})

test_that("rule 60 honours exceptions", {
  result <- neoipcr:::validation_rule_60(
    nec_ds(list("vomiting", "vomiting")),
    make_test_exceptions(60L, event_key = 1L))
  expect_equal(result$event_key, 2L)
})

test_that("rule 60 skips without a warning when a finding it counts is absent", {
  for (col in c(nec_imaging, nec_clinical, nec_surgical)) {
    ds <- nec_ds(list("vomiting"))
    ds$necData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_60(ds, NULL))
    expect_null(result, info = col)
  }
})

# --- Rule 61: surgical site infection ---

ssi_items <- c(
  "purulent_drainage_superf", "physician_diag_superf", "inc_opened_superf",
  "localized_pain_superf", "localized_swelling", "localized_erythema",
  "localized_heat", "purulent_drainage_deep", "abscess_deep",
  "inc_dehisces_deep", "fever", "localized_pain_deep",
  "purulent_drainage_drain", "abscess_organ")
ssi_organisms <- c("organisms_superf", "organisms_deep", "organisms_organ")

# SSI events with the forms `forms`, each a named list of the values it
# records; every finding not named is FALSE and every organisms item "0"
# (none identified).
ssi_ds <- function(forms, ...)
{
  form_data <- make_test_ssi_data(seq_along(forms))
  for (col in ssi_organisms)
    form_data[[col]] <- factor(rep("0", length(forms)), levels = c("1", "0", "-1"))
  definition_ds("ssi", "ssiData", form_data, forms, ...)
}

# Whether rule 61 flags the one SSI form `form`.
ssi_flagged <- function(form)
  nrow(neoipcr:::validation_rule_61(ssi_ds(list(form)), NULL)) == 1L

test_that("rule 61 detects an SSI form that does not meet the definition of its depth", {
  result <- neoipcr:::validation_rule_61(
    ssi_ds(list(list(infection_type = "2"),
                list(infection_type = "2", abscess_deep = TRUE))), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 61L)
  expect_equal(result$patient_key, 1L)
  expect_equal(result$enrollment_key, 1L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "infection_type")
  expect_equal(as.character(result$context[[1]]$infection_type), "2")
})

test_that("rule 61 holds a superficial incisional SSI to its four criteria", {
  expect_true(ssi_flagged(list(infection_type = "1")))
  expect_false(ssi_flagged(list(infection_type = "1", purulent_drainage_superf = TRUE)))
  expect_false(ssi_flagged(list(infection_type = "1", organisms_superf = "1")))
  expect_false(ssi_flagged(list(infection_type = "1", physician_diag_superf = TRUE)))
  # The incision opened and not tested, with a local sign.
  for (sign in c("localized_pain_superf", "localized_swelling",
                 "localized_erythema", "localized_heat")) {
    opened <- list(infection_type = "1", inc_opened_superf = TRUE,
                   organisms_superf = "-1")
    opened[[sign]] <- TRUE
    expect_false(ssi_flagged(opened), info = sign)
    # Tested, with or without agents found, the opening is not the
    # criterion; nor is it without a sign.
    opened$organisms_superf <- "0"
    expect_true(ssi_flagged(opened), info = sign)
    opened$organisms_superf <- NA
    expect_true(ssi_flagged(opened), info = sign)
  }
  expect_true(ssi_flagged(list(infection_type = "1", inc_opened_superf = TRUE,
                               organisms_superf = "-1")))
})

test_that("rule 61 holds a deep incisional SSI to its three criteria", {
  expect_true(ssi_flagged(list(infection_type = "2")))
  expect_false(ssi_flagged(list(infection_type = "2", purulent_drainage_deep = TRUE)))
  expect_false(ssi_flagged(list(infection_type = "2", abscess_deep = TRUE)))
  # The incision dehiscing, with agents identified or not tested, and fever
  # or pain.
  for (organisms in c("1", "-1"))
    for (sign in c("fever", "localized_pain_deep")) {
      dehiscing <- list(infection_type = "2", inc_dehisces_deep = TRUE,
                        organisms_deep = organisms)
      expect_true(ssi_flagged(dehiscing), info = paste(organisms, sign))
      dehiscing[[sign]] <- TRUE
      expect_false(ssi_flagged(dehiscing), info = paste(organisms, sign))
    }
  # None identified, or the organisms item missing, which reads as none.
  for (organisms in list("0", NA))
    expect_true(ssi_flagged(list(infection_type = "2", inc_dehisces_deep = TRUE,
                                 organisms_deep = organisms, fever = TRUE)))
})

test_that("rule 61 holds an organ/space SSI to its three criteria", {
  for (organisms in list("0", "-1", NA))
    expect_true(ssi_flagged(list(infection_type = "3", organisms_organ = organisms)))
  expect_false(ssi_flagged(list(infection_type = "3", organisms_organ = "1")))
  expect_false(ssi_flagged(list(infection_type = "3", purulent_drainage_drain = TRUE)))
  expect_false(ssi_flagged(list(infection_type = "3", abscess_organ = TRUE)))
})

test_that("rule 61 reads only the recorded depth's criteria", {
  expect_true(ssi_flagged(list(infection_type = "1", abscess_deep = TRUE,
                               organisms_organ = "1")))
  expect_true(ssi_flagged(list(infection_type = "2", organisms_superf = "1",
                               abscess_organ = TRUE)))
  expect_true(ssi_flagged(list(infection_type = "3",
                               purulent_drainage_superf = TRUE,
                               purulent_drainage_deep = TRUE)))
})

test_that("rule 61 leaves an SSI form without a depth alone", {
  expect_false(ssi_flagged(list(infection_type = NA)))
})

test_that("rule 61 reads a missing item as not present", {
  all_missing <- rlang::set_names(
    as.list(rep(NA, length(c(ssi_items, ssi_organisms)))),
    c(ssi_items, ssi_organisms))
  for (type in c("1", "2", "3")) {
    form <- all_missing
    form$infection_type <- type
    expect_true(ssi_flagged(form), info = type)
  }
  # One criterion met among missing items meets the definition.
  met <- list(
    list(infection_type = "1", physician_diag_superf = TRUE),
    list(infection_type = "2", abscess_deep = TRUE),
    list(infection_type = "3", organisms_organ = "1"))
  for (criterion in met)
    expect_false(ssi_flagged(utils::modifyList(all_missing, criterion)),
                 info = criterion$infection_type)
  # An event of which only infectious agents are stored records no depth.
  ds <- ssi_ds(list(list(infection_type = "1")))
  ds$ssiData <- ds$ssiData[0L, ]
  expect_equal(nrow(neoipcr:::validation_rule_61(ds, NULL)), 0L)
})

test_that("rule 61 reads a missing item of a compound criterion as not present", {
  # A compound criterion with its gate met and one of its items missing, the
  # others No, is not met, so the form is flagged; the same item Yes meets
  # it. Read as missing rather than as No, the item would leave the
  # criterion undecided, and the form would silently pass.
  missing_and_present <- function(form, item) {
    form[[item]] <- NA
    expect_true(ssi_flagged(form), info = item)
    form[[item]] <- TRUE
    expect_false(ssi_flagged(form), info = item)
  }

  # The superficial incision opened and not tested, with a local sign.
  opened <- list(infection_type = "1", inc_opened_superf = TRUE,
                 organisms_superf = "-1")
  for (sign in c("localized_pain_superf", "localized_swelling",
                 "localized_erythema", "localized_heat"))
    missing_and_present(opened, sign)
  missing_and_present(
    list(infection_type = "1", organisms_superf = "-1",
         localized_pain_superf = TRUE),
    "inc_opened_superf")

  # The deep incision dehiscing, with agents identified or not tested, and
  # fever or pain.
  for (organisms in c("1", "-1")) {
    dehiscing <- list(infection_type = "2", inc_dehisces_deep = TRUE,
                      organisms_deep = organisms)
    for (sign in c("fever", "localized_pain_deep"))
      missing_and_present(dehiscing, sign)
    missing_and_present(
      list(infection_type = "2", organisms_deep = organisms, fever = TRUE),
      "inc_dehisces_deep")
  }
})

test_that("rule 61 judges only completed forms", {
  ds <- ssi_ds(list(list(infection_type = "1"), list(infection_type = "1")),
               status = c("ACTIVE", "COMPLETED"))
  expect_equal(neoipcr:::validation_rule_61(ds, NULL)$event_key, 2L)
})

test_that("rule 61 honours exceptions", {
  result <- neoipcr:::validation_rule_61(
    ssi_ds(list(list(infection_type = "1"), list(infection_type = "3"))),
    make_test_exceptions(61L, event_key = 1L))
  expect_equal(result$event_key, 2L)
})

test_that("rule 61 skips without a warning when an item it reads is absent", {
  for (col in c("infection_type", ssi_items, ssi_organisms)) {
    ds <- ssi_ds(list(list(infection_type = "1")))
    ds$ssiData[[col]] <- NULL
    expect_no_warning(result <- neoipcr:::validation_rule_61(ds, NULL))
    expect_null(result, info = col)
  }
})

test_that("rules 59 to 61 run through validate with the context they declare", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events      = make_test_events(3, event_type_keys = c("bsi", "nec", "ssi")),
    sepsisData  = make_test_sepsis_data(1L),
    necData     = make_test_nec_data(2L),
    ssiData     = make_test_ssi_data(3L,
      organisms_superf = factor("0", levels = c("1", "0", "-1"))))
  result <- neoipcr::validate(ds, rules = 59:61)
  expect_equal(result$rule_id, 59:61)
  expect_equal(result$event_key, 1:3)
  expect_identical(attr(result, "rules_skipped"), integer())
})
