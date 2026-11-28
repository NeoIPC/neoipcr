# Tests for R/validation-rules-surgical.R — rules 19, 22-24, 66-69.

# --- Rule 19: SSI outside the follow-up window of every surgery ---

ssi_type <- function(x)
  factor(x, levels = c("1", "2", "3"))

# One patient, one enrolment; a surgery on 2024-01-01 (event key 1) and an
# SSI `offset` days later (event key 2). `surgery = FALSE` leaves the patient
# without any procedure; `surgery_dated = FALSE` leaves the procedure without
# a date.
rule_19_ds <- function(offset, infection_type = "1", implant = FALSE,
                       surgery = TRUE, surgery_dated = TRUE) {
  types <- c(if (surgery) "pro", "ssi")
  dates <- as.Date("2024-01-01") + c(if (surgery) 0L, offset)
  if (surgery && !surgery_dated) dates[1] <- NA
  n <- length(types)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(n,
      enrollment_keys = rep(1L, n),
      patient_keys    = rep(1L, n),
      event_type_keys = types,
      occurredAt = dates),
    surgeryData = if (surgery)
      make_test_surgery_data(1L, implant = implant)
    else
      make_test_surgery_data(integer(0)),
    ssiData = make_test_ssi_data(n, infection_type = ssi_type(infection_type)))
}

test_that("rule 19 detects a superficial SSI after the 30-day window", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(35L), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 19L)
  expect_equal(result$event_key, 2L)
  expect_named(result$context[[1]], "infection_type")
  expect_equal(as.character(result$context[[1]]$infection_type), "1")
})

test_that("rule 19 counts the procedure date as day 1 of the window", {
  # The protocol's 30 days run from the procedure date, so the window covers
  # the offsets 0 to 29: an infection on the procedure day is inside it and
  # one 30 days later is the first outside.
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(0L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(20L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(29L), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(30L), NULL)), 1L)
})

test_that("rule 19 detects an SSI dated before its only surgery", {
  expect_equal(nrow(neoipcr:::validation_rule_19(rule_19_ds(-1L), NULL)), 1L)
})

test_that("rule 19 extends the window to 90 days for a deep infection after an implant", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = TRUE), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(89L, infection_type = "2", implant = TRUE), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(90L, infection_type = "2", implant = TRUE), NULL)), 1L)
  # Without an implant a deep infection keeps the 30-day window.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = FALSE), NULL)), 1L)
})

test_that("rule 19 reads an implant flag that was not recorded as no implant", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(20L, infection_type = "2", implant = NA), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(60L, infection_type = "2", implant = NA), NULL)), 1L)
})

test_that("rule 19 detects an SSI of a patient without any surgery", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(10L, surgery = FALSE), NULL)
  expect_equal(nrow(result), 1L)
})

# One patient, one enrolment; a surgery on each of `surgery_dates` (event
# keys 1..k) and an SSI on `ssi_date` (event key k + 1).
rule_19_multi_ds <- function(surgery_dates, ssi_date) {
  k <- length(surgery_dates)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(k + 1L,
      enrollment_keys = rep(1L, k + 1L),
      patient_keys    = rep(1L, k + 1L),
      event_type_keys = c(rep("pro", k), "ssi"),
      occurredAt = as.Date(c(surgery_dates, ssi_date))),
    surgeryData = make_test_surgery_data(seq_len(k)),
    ssiData     = make_test_ssi_data(k + 1L))
}

test_that("rule 19 accepts an SSI covered by any one of several surgeries", {
  # The first surgery's window has closed, the second's has not.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_multi_ds(c("2024-01-01", "2024-02-15"), "2024-03-01"), NULL)), 0L)
  # Both windows have closed: one finding for the infection, not one per
  # surgery.
  result <- neoipcr:::validation_rule_19(
    rule_19_multi_ds(c("2024-01-01", "2024-01-10"), "2024-03-01"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$event_key, 3L)
})

test_that("rule 19 matches surgeries to the patient they belong to", {
  # The second patient's SSI follows the first patient's surgery in time,
  # but not in person.
  ds <- make_test_ds(
    patients    = make_test_patients(2),
    enrollments = make_test_enrollments(2, patient_keys = c(1L, 2L)),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 2L),
      event_type_keys = c("pro", "ssi"),
      occurredAt = as.Date(c("2024-01-01", "2024-01-10"))),
    surgeryData = make_test_surgery_data(1L),
    ssiData     = make_test_ssi_data(2L))
  result <- neoipcr:::validation_rule_19(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$patient_key, 2L)
})

test_that("rule 19 accepts a surgery from another enrolment of the same patient", {
  ds <- make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(2,
      patient_keys = c(1L, 1L),
      enrolledAt = as.Date(c("2024-01-01", "2024-01-20"))),
    events = make_test_events(2,
      enrollment_keys = c(1L, 2L),
      patient_keys    = c(1L, 1L),
      event_type_keys = c("pro", "ssi"),
      occurredAt = as.Date(c("2024-01-05", "2024-01-25"))),
    surgeryData = make_test_surgery_data(1L),
    ssiData     = make_test_ssi_data(2L))
  expect_equal(nrow(neoipcr:::validation_rule_19(ds, NULL)), 0L)
})

test_that("rule 19 leaves an SSI without a date alone", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(NA_integer_), NULL)), 0L)
})

test_that("rule 19 places an SSI of unknown type by the windows its implant flag allows", {
  # Inside 30 days every window covers it, whatever the type would be.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(10L, infection_type = NA), NULL)), 0L)
  # Beyond 30 days only the implant window can, and only with an implant.
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(40L, infection_type = NA, implant = FALSE), NULL)), 1L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(40L, infection_type = NA, implant = TRUE), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(100L, infection_type = NA, implant = TRUE), NULL)), 1L)
})

test_that("rule 19 treats a surgery without a date as covering nothing", {
  expect_equal(nrow(neoipcr:::validation_rule_19(
    rule_19_ds(10L, surgery_dated = FALSE), NULL)), 1L)
})

test_that("rule 19 honours exceptions", {
  result <- neoipcr:::validation_rule_19(rule_19_ds(35L), make_test_exceptions(19L, event_key = 2L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 19 skips without a warning when the implant flag is absent", {
  ds <- rule_19_ds(35L)
  ds$surgeryData$implant <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_19(ds, NULL))
  expect_null(result)
})

test_that("rule 19 skips without a warning when the infection type is absent", {
  ds <- rule_19_ds(35L)
  ds$ssiData$infection_type <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_19(ds, NULL))
  expect_null(result)
})

# --- Rules 22-24: procedure codes that are not valid ICHI codes ---
# Rule 22 = main code, 23 = first side code, 24 = second side code.

surgery_ds <- function(...)
  make_test_ds(
    patients    = make_test_patients(1),
    enrollments = make_test_enrollments(1, patient_keys = 1L),
    events = make_test_events(1,
      enrollment_keys = 1L,
      patient_keys    = 1L,
      event_type_keys = "pro"),
    surgeryData = make_test_surgery_data(1L, ...))

ichi_rules <- list(
  list(rule = 22L, col = "main_procedure_code",   fun = neoipcr:::validation_rule_22),
  list(rule = 23L, col = "side_procedure_code_1", fun = neoipcr:::validation_rule_23),
  list(rule = 24L, col = "side_procedure_code_2", fun = neoipcr:::validation_rule_24)
)

for (entry in ichi_rules) {
  local({
    r   <- entry$rule
    col <- entry$col
    f   <- entry$fun
    with_code <- function(code) do.call(surgery_ds, stats::setNames(list(code), col))

    test_that(paste0("rule ", r, " detects an invalid code in ", col), {
      result <- f(with_code("NOT A CODE"), NULL)
      expect_equal(nrow(result), 1L)
      expect_declared_context(result)
      expect_equal(result$rule_id, r)
      expect_equal(result$event_key, 1L)
      expect_named(result$context[[1]], c("procedure_description", "procedure_code"))
      expect_equal(result$context[[1]]$procedure_code, "NOT A CODE")
    })

    test_that(paste0("rule ", r, " returns no rows for a valid or absent code in ", col), {
      expect_equal(nrow(f(with_code("PZX.AA.JA"), NULL)), 0L)
      expect_equal(nrow(f(with_code(NA_character_), NULL)), 0L)
    })

    test_that(paste0("rule ", r, " honours exceptions"), {
      result <- f(with_code("NOT A CODE"), make_test_exceptions(r, event_key = 1L))
      expect_equal(nrow(result), 0L)
    })

    test_that(paste0("rule ", r, " skips without a warning when ", col, " is absent"), {
      ds <- with_code("NOT A CODE")
      ds$surgeryData[[col]] <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
      # The procedure description travels in the context, so it is read too.
      ds <- with_code("NOT A CODE")
      ds$surgeryData$procedure_description <- NULL
      expect_no_warning(result <- f(ds, NULL))
      expect_null(result)
    })
  })
}

# --- Rules 66–69: the placement of a procedure among the groups ---
# Rule 66 = class I with a tract-opening code of its own group, 67 = an
# approach code is all that places the procedure, 68 = a code no group takes
# or the conditional peritoneum code, 69 = a percutaneous abdominal drain
# that asks whether the operation was in fact open.

# One procedure with these codes and this wound class, on one patient and
# enrolment.
procedure_ds <- function(main = NA_character_, side1 = NA_character_,
                         side2 = NA_character_, wound_class = "1") {
  surgery_ds(
    main_procedure_code   = main,
    side_procedure_code_1 = side1,
    side_procedure_code_2 = side2,
    wound_class = factor(wound_class, levels = c("1", "2", "3", "4")))
}

test_that("rule 66 detects a class I procedure whose own group opens a tract", {
  # The alimentary tract through an abdominal code, and the urinary tract
  # through a urogenital one: a class I dressing of the closure does not
  # make them clean.
  result <- neoipcr:::validation_rule_66(procedure_ds("KBF.LI.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 66L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], c("wound_class", "procedure_code"))
  expect_equal(as.character(result$context[[1]]$wound_class), "1")
  expect_equal(result$context[[1]]$procedure_code, "KBF.LI.AA")
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("NAM.MK.AA"), NULL)), 1L)
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("JBA.MK.AA"), NULL)), 1L)
  # A recorded tract-opening stem inside an expression is found as the bare
  # code is, and reported by its stem.
  result <- neoipcr:::validation_rule_66(procedure_ds("KBF.LI.AA&XK9K"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "KBF.LI.AA")
})

test_that("rule 66 leaves a class I procedure alone when nothing of its group opens a tract", {
  # A hernia repair opens no tract.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("PAM.MK.AA"), NULL)), 0L)
  # A wound class higher than I is already placed with the classed wounds.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("KBF.LI.AA", wound_class = "2"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("KBF.LI.AA", wound_class = "3"), NULL)), 0L)
  # A code whose tract is unclear raises nothing.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("KBF.FA.AA"), NULL)), 0L)
  # A tract opened by a code of another group is no code of this procedure's
  # own group: the procedure joins the thoracic group, whose own code opens
  # no tract.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("MCX.MK.AA", "KBF.LI.AA"), NULL)), 0L)
  # A procedure no group takes is left to rule 68.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("ZZZ.ZZ.ZZ"), NULL)), 0L)
  # A held pending code holds the procedure out of every group, so it has
  # no own group whose tract could open: rule 68 asks about it.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("NMR.MK.AB", "KBF.LI.AA"), NULL)), 0L)
  # A wound class that was not recorded decides nothing.
  expect_equal(nrow(neoipcr:::validation_rule_66(
    procedure_ds("KBF.LI.AA", wound_class = NA_character_), NULL)), 0L)
})

test_that("rule 66 honours exceptions", {
  result <- neoipcr:::validation_rule_66(
    procedure_ds("KBF.LI.AA"), make_test_exceptions(66L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 66 skips without a warning when the wound class or a code column is absent", {
  ds <- procedure_ds("KBF.LI.AA")
  ds$surgeryData$wound_class <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_66(ds, NULL))
  expect_null(result)
  ds <- procedure_ds("KBF.LI.AA")
  ds$surgeryData$main_procedure_code <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_66(ds, NULL))
  expect_null(result)
})

test_that("rule 67 detects a procedure an approach code alone places", {
  # Only the approach was coded: the operation done through it is missing.
  result <- neoipcr:::validation_rule_67(procedure_ds("PAK.FA.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 67L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "procedure_code")
  expect_equal(result$context[[1]]$procedure_code, "PAK.FA.AA")
  # Codes that are no surgical procedure drop out first, so this procedure
  # holds nothing but its approach code.
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("PAK.JB.AE", "PAK.AE.AA"), NULL)), 1L)
  # The approach stem inside an expression is found as the bare code is.
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("PAK.FA.AA&XK9K"), NULL)), 1L)
  # A pending code that places nothing holds nothing back: the operation
  # behind the approach is still uncoded.
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("PAK.FA.AA", "LZZ.DK.AH"), NULL)), 1L)
})

test_that("rule 67 leaves alone a procedure its operation places", {
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("PAK.FA.AA", "KBF.LI.AA"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("KBF.LI.AA"), NULL)), 0L)
  # A procedure of no surgical procedure is placed by nothing, and one no
  # group takes is rule 68's.
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("BCC.GA.AA"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("ZZZ.ZZ.ZZ"), NULL)), 0L)
  # A held pending code holds the procedure out of every group, so it is
  # no approach code's alone: the classifier places it in no group and
  # rule 68 asks what was done.
  expect_equal(nrow(neoipcr:::validation_rule_67(
    procedure_ds("PAK.FA.AA", "NMR.MK.AB"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("PAK.FA.AA", "NMR.MK.AB"), NULL)), 1L)
})

test_that("rule 67 honours exceptions", {
  result <- neoipcr:::validation_rule_67(
    procedure_ds("PAK.FA.AA"), make_test_exceptions(67L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 67 skips without a warning when no code column is present", {
  ds <- procedure_ds("PAK.FA.AA")
  ds$surgeryData$main_procedure_code <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_67(ds, NULL))
  expect_null(result)
})

test_that("rules 66 to 69 take a surgery form without the side codes", {
  # An absent side-code column stands in as a missing value in every row,
  # as in the surgical counts: a dataset from a form version without them
  # is counted and asked about alike.
  ds <- procedure_ds("PAK.FA.AA")
  ds$surgeryData$side_procedure_code_1 <- NULL
  ds$surgeryData$side_procedure_code_2 <- NULL
  expect_equal(nrow(neoipcr:::validation_rule_67(ds, NULL)), 1L)
  ds <- procedure_ds("NMR.MK.AB", "KBF.LI.AA")
  ds$surgeryData$side_procedure_code_2 <- NULL
  result <- neoipcr:::validation_rule_68(ds, NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "NMR.MK.AB")
})

test_that("rule 68 detects a code no group takes", {
  # A code pending an answer, a valid code no rule places, and a held-pending
  # code: each needs the partners to say what was done.
  result <- neoipcr:::validation_rule_68(procedure_ds("KZZ.AD.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 68L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], "procedure_code")
  expect_equal(result$context[[1]]$procedure_code, "KZZ.AD.AA")
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("PZX.AA.JA"), NULL)), 1L)
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("NMR.MK.AB"), NULL)), 1L)
  # The pending code is asked about even where the procedure's other code
  # places the procedure.
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AA", "LZZ.DK.AH"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "LZZ.DK.AH")
  # The grammar composes expressions of stems, and it is the stem that
  # places: a stem no rule takes is asked about beside the stems that do
  # place.
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AA&XK9K/ZZZ.ZZ.ZZ"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "ZZZ.ZZ.ZZ")
  # A value no grammar reads is left to rules 22 to 24, which report it as
  # an error on the same form: this warning asks of valid codes only, and
  # beside a valid ungrouped code it asks of that code.
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AAA"), NULL)), 0L)
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AAA", "ZZZ.ZZ.ZZ"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "ZZZ.ZZ.ZZ")
})

test_that("rule 68 detects the peritoneum code whose group turns on a shunt", {
  # `KMA.FA.AA` counts as abdominal unless the procedure holds a shunt code,
  # in which case it follows the shunt: the coding is asked about unless a
  # shunt code settles it.
  result <- neoipcr:::validation_rule_68(procedure_ds("KMA.FA.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "KMA.FA.AA")
  # Beside a shunt code the placement is settled and nothing is asked.
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KMA.FA.AA", "AAE.LI.AA"), NULL)), 0L)
  # Only a shunt settles it: a code of another group that wins the
  # procedure says nothing about what was done at the peritoneum.
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KMA.FA.AA", "HIK.MK.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "KMA.FA.AA")
  # The umbilical vein repair was recorded only with an implant and is
  # probably a patch closure: the same warning asks about it, whatever
  # else the procedure holds and whether or not it places the procedure.
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KMA.MK.AA"), NULL)), 1L)
  result <- neoipcr:::validation_rule_68(procedure_ds("KMA.MK.AA"), NULL)
  expect_equal(result$context[[1]]$procedure_code, "KMA.MK.AA")
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KMA.MK.AA", "HIK.MK.AA"), NULL)), 1L)
  # Beside a shunt the access is settled yet the repair keeps its query.
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KMA.FA.AA", "AAE.LI.AA", "KMA.MK.AA"), NULL)
  expect_equal(nrow(result), 1L)
  expect_equal(result$context[[1]]$procedure_code, "KMA.MK.AA")
})

test_that("rule 68 leaves a procedure its codes place alone", {
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AA"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("KBF.LI.AA", "PAM.MK.AA", "BCC.GA.AA"), NULL)), 0L)
  # A code that is no surgical procedure is not a code no group takes.
  expect_equal(nrow(neoipcr:::validation_rule_68(
    procedure_ds("BCC.GA.AA"), NULL)), 0L)
})

test_that("rule 68 honours exceptions", {
  result <- neoipcr:::validation_rule_68(
    procedure_ds("KZZ.AD.AA"), make_test_exceptions(68L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 68 skips without a warning when no code column is present", {
  ds <- procedure_ds("KZZ.AD.AA")
  ds$surgeryData$main_procedure_code <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_68(ds, NULL))
  expect_null(result)
})

test_that("rule 69 detects a percutaneous abdominal drain among the codes", {
  # The percutaneous drainages of the abdomen are no surgical procedure: a
  # procedure carrying one is asked about, since an open drainage coded as
  # percutaneous would belong among the abdominal procedures.
  result <- neoipcr:::validation_rule_69(procedure_ds("KMA.JB.AE"), NULL)
  expect_equal(nrow(result), 1L)
  expect_declared_context(result)
  expect_equal(result$rule_id, 69L)
  expect_equal(result$event_key, 1L)
  expect_named(result$context[[1]], c("procedure_description", "procedure_code"))
  expect_equal(result$context[[1]]$procedure_code, "KMA.JB.AE")
  expect_equal(result$context[[1]]$procedure_description, "Test procedure")
  expect_equal(nrow(neoipcr:::validation_rule_69(
    procedure_ds("KBF.LI.AA", "PAK.JB.AE"), NULL)), 1L)
  # The drain stem inside an expression is found as the bare code is.
  expect_equal(nrow(neoipcr:::validation_rule_69(
    procedure_ds("KMA.JB.AE&XK9K"), NULL)), 1L)
})

test_that("rule 69 leaves the other drains and the procedures without them alone", {
  # The pleural drainage counts as surgery wherever it is coded.
  expect_equal(nrow(neoipcr:::validation_rule_69(
    procedure_ds("JCB.JB.AE"), NULL)), 0L)
  expect_equal(nrow(neoipcr:::validation_rule_69(
    procedure_ds("KBF.LI.AA"), NULL)), 0L)
})

test_that("rule 69 honours exceptions", {
  result <- neoipcr:::validation_rule_69(
    procedure_ds("KMA.JB.AE"), make_test_exceptions(69L, event_key = 1L))
  expect_equal(nrow(result), 0L)
})

test_that("rule 69 skips without a warning when the description or a code column is absent", {
  ds <- procedure_ds("KMA.JB.AE")
  ds$surgeryData$procedure_description <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_69(ds, NULL))
  expect_null(result)
  ds <- procedure_ds("KMA.JB.AE")
  ds$surgeryData$main_procedure_code <- NULL
  expect_no_warning(result <- neoipcr:::validation_rule_69(ds, NULL))
  expect_null(result)
})
