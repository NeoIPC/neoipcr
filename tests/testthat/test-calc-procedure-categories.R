# Tests for R/calc-procedure-categories.R — the eight procedure groups, the
# placement of the recorded codes, the combination of a procedure's codes,
# the risk strata, the labels, and the exported classifier.

# The recorded codes with their groups: every code placed exactly once. These
# are the test cases the group definitions must reproduce, as API-DESIGN.md
# §13.9.2 defines them.
placed_neurosurgery <- c(
  "AAA.JB.AA", "AAE.DN.AA", "AAE.GA.AE", "AAE.JB.AA", "AAE.JD.AA", "AAE.KA.AA",
  "AAE.LH.AA", "AAE.LI.AA", "AAE.LI.AB", "AAE.SN.AA", "AAG.DN.AA", "ABA.FC.AA",
  "ABA.MK.AA", "ABA.ML.AA", "ABG.LI.AA", "MAA.FA.AA")
placed_cardiac <- c("HBB.JD.AA", "HIJ.LA.AA", "HIJ.LA.AB", "HIK.MK.AA")
placed_thoracic <- c(
  "HIK.LJ.AA", "JCA.JD.AA", "JCB.JB.AE", "JCH.AE.AA", "JCH.FA.AA",
  "MCX.MK.AA", "MCX.MK.AB",
  # tract respiratory
  "JBA.MK.AA", "JBF.JJ.AA", "JBF.LL.AA",
  # tract gastrointestinal: the oesophagus is placed in the chest
  "KBA.LB.AA", "KBA.MK.AA")
placed_abdominal <- c(
  "DJA.MK.AA", "IED.LA.AA", "KBZ.LD.AA", "KMA.DN.AA", "KMA.FA.AA", "KMA.FC.AA",
  "KMA.JA.AA", "KMA.JB.AA", "KMA.JJ.AA", "KMA.KA.AA", "KMA.MK.AA", "PAK.AE.AA",
  "PAK.AE.AB", "PAK.FA.AA", "PAK.FC.AA", "PAK.FC.AB", "PAK.JB.AA", "PAL.FA.AA",
  # tract gastrointestinal
  "KBF.AE.AB", "KBF.LI.AA", "KBF.LI.AB", "KBF.MK.AA", "KBI.LB.AA", "KBI.MK.AA",
  "KBI.ML.AA", "KBK.JI.AA", "KBK.JJ.AA", "KBK.JJ.AB", "KBK.LB.AA", "KBK.LI.AA",
  "KBK.LI.AB", "KBK.MK.AA", "KBK.MK.AB", "KBO.JK.AA", "KBO.JK.AB", "KBP.FA.AB",
  "KBP.JJ.AA", "KBP.LI.AA", "KBP.MK.AA", "KBW.JK.AA", "KBZ.JJ.AA", "KBZ.JJ.AB",
  "KBZ.LB.AA", "KBZ.MK.AA", "KZZ.MK.AA", "PTA.LA.AC", "PTB.LA.AC",
  # tract unclear, which raises nothing for the stratum
  "KBF.FA.AA", "KBZ.LC.AA")
placed_abdominal_wall <- c(
  "PAK.LI.AA", "PAL.MK.AA", "PAL.MK.AB", "PAL.ML.AA", "PAO.JI.AA")
placed_hernia <- c("PAM.MK.AA", "PAM.MK.AB", "PAO.MK.AA", "PAO.MK.AB")
placed_head_neck <- c(
  # tract respiratory
  "JAM.ML.AD", "JAN.JI.AD", "JAN.MK.AD", "JBA.LI.AA",
  # tract unclear
  "KAA.AD.AA",
  # tract gastrointestinal: a frenotomy is an incision of mucosa
  "KAB.FB.AC")
placed_skin_urogenital <- c(
  "LAB.LL.AA", "LAG.JG.AA", "LCA.JG.AA", "NGL.LC.AA", "PAW.JB.AA",
  # tract urinary
  "NAM.MK.AA")
outside_surgery <- c(
  # imaging
  "AAA.BA.BH", "ABA.BA.BH", "MBO.BA.BC", "PAB.BA.BH", "PAE.BA.BH", "PAK.BA.BH",
  "PZA.BA.BH",
  # endoscopy without incision
  "JAN.AE.AC", "JBA.AE.AB", "JBB.AE.AD", "KBA.LG.AD", "KBI.AE.AD",
  # treatment of retinopathy of prematurity
  "BCC.GA.AA", "BCD.DB.AE",
  # vascular access
  "HIB.DL.AF", "IBD.DL.AF", "IDB.DL.AF", "IZD.DL.AF",
  # transcatheter heart intervention
  "HDG.LG.AF",
  # needle punctures and percutaneous drains; only the pleura's drainage
  # counts as a surgical procedure, and that one sits among the thoracic codes
  "JBF.JB.AE", "JBF.LL.AE", "KMA.JB.AE", "NAA.LI.AE", "PAK.JB.AE",
  # device, stoma, and wound care; education
  "JBA.KA.AC", "KBF.DL.AC", "KBF.KA.AC", "KBK.DL.AC", "KBK.LD.AH", "KBK.LG.AC",
  "LAB.JG.AH", "PTA.PM.ZZ", "PTB.SN.AC", "PTC.PM.ZZ")
# Codes that name no operation that can be placed until the partners say what
# was done. None of them places a procedure.
pending_codes <- c(
  "BAA.BA.AD", "LZZ.DK.AH", "KZZ.AD.AA", "NMR.MK.AB", "NZZ.ZZ.ZZ", "KPB.LI.AA")

recorded_groups <- list(
  neurosurgery = placed_neurosurgery,
  cardiac_great_vessel_surgery = placed_cardiac,
  non_cardiac_thoracic_surgery = placed_thoracic,
  abdominal_surgery = placed_abdominal,
  abdominal_wall_surgery = placed_abdominal_wall,
  inguinal_umbilical_hernia_repair = placed_hernia,
  head_neck_upper_airway_surgery = placed_head_neck,
  skin_soft_tissue_urogenital_surgery = placed_skin_urogenital)
recorded_placements <- data.frame(
  code  = unlist(recorded_groups, use.names = FALSE),
  group = rep(names(recorded_groups), lengths(recorded_groups)),
  stringsAsFactors = FALSE)

recorded_all <- c(recorded_placements$code, outside_surgery, pending_codes)

# --- The placement of every recorded code -----------------------------------

test_that("the recorded codes are the 142 placed exactly once", {
  # The count of the production codes to 2026-11-08: every one of them falls
  # in a group, among the codes that are no surgical procedure, or among the
  # codes pending a query — exactly once.
  expect_equal(length(recorded_all), 142L)
  expect_equal(sum(duplicated(recorded_all)), 0L)
})

test_that("every recorded group code is placed in its group", {
  result <- neoipcr:::get_procedure_category(recorded_placements$code)
  expect_identical(as.character(result), recorded_placements$group)
})

test_that("every code that is no surgical procedure is placed outside surgery", {
  result <- neoipcr:::get_procedure_category(outside_surgery)
  expect_identical(
    as.character(result), rep("not_surgery", length(outside_surgery)))
})

test_that("no pending code places anything", {
  result <- neoipcr:::get_procedure_category(pending_codes)
  expect_identical(as.character(result), rep("no_group", length(pending_codes)))
})

test_that("the axes agree with the table wherever they speak about a recorded code", {
  # The axes only ever serve a code the placement table does not hold,
  # and wherever they speak about a recorded code they confirm the
  # table's decision: the codes outside surgery stand outside it, and a
  # pending code waits on its answer, so the axes place it nowhere.
  table <- neoipcr:::.procedure_code_table
  group <- table$group[match(recorded_all, table$procedure_code)]
  axis <- neoipcr:::.procedure_group_by_axis(recorded_all)
  expect_true(all(is.na(axis) | axis == group))
  expect_true(all(is.na(neoipcr:::.procedure_group_by_axis(pending_codes))))
})

test_that("a code the table holds is placed by the table, whatever its family", {
  # `PAK.LI.AA` is a laparostomy closure, an abdominal wall procedure, while
  # its target's family falls in abdominal surgery: the recorded placement
  # of a code outranks the axis of its family.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("PAK.LI.AA")),
    "abdominal_wall_surgery")
})

test_that("the axes place a first-time code of a placed family by the family", {
  # A valid code entered for the first time falls in a group by the same
  # rules as the recorded ones, or is a code no group takes.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("AAE.QQ.AB")), "neurosurgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("KBF.QQ.AB")), "abdominal_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("PAM.QQ.AA")),
    "inguinal_umbilical_hernia_repair")
  # A percutaneous pleural drain counts as surgery wherever it is coded; a
  # percutaneous drainage or puncture of no other site does. The pleura's
  # own target admits the percutaneous means for its drainage only; the
  # open and endoscopic means stand for every action of it.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("JCB.QQ.AA")),
    "non_cardiac_thoracic_surgery")
  expect_true(is.na(neoipcr:::.procedure_group_by_axis("JCB.QQ.AE")))
  expect_true(is.na(neoipcr:::.procedure_group_by_axis("JBF.QQ.AE")))
  expect_true(is.na(neoipcr:::.procedure_group_by_axis("KMA.QQ.AE")))
  # A transcatheter duct closure is a transcatheter heart intervention,
  # which never counts: the cardiac axis admits the open means only, and
  # the transluminal means stands outside surgery for every target.
  expect_equal(
    neoipcr:::.procedure_group_by_axis("HIJ.LA.AF"), "not_surgery")
  expect_equal(
    as.character(neoipcr::get_procedure_group("HIJ.LA.AF")), "not_surgery")
  # The body wall, the head and neck, and the skin and urogenital tract
  # place first-time codes by their own axes too.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("PAL.QQ.AA")),
    "abdominal_wall_surgery")
  # The release of a laparostomy stands with the wall from the first time,
  # as the recorded `PAK.LI.AA` does.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("PAK.LI.AB")),
    "abdominal_wall_surgery")
  # And an incision of the abdominal wall rests with the abdomen from the
  # first time, as the recorded `PAL.FA.AA` does.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("PAL.FA.AB")),
    "abdominal_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("JAN.QQ.AD")),
    "head_neck_upper_airway_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("LAB.QQ.AA")),
    "skin_soft_tissue_urogenital_surgery")
  # And so do the codes outside surgery: imaging, the transluminal and
  # the external means, education, and every percutaneous drainage of a
  # site but the pleura's are no surgical procedure from the first time.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("JCA.BA.BH")), "not_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("KMA.DL.AF")), "not_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("KBF.QQ.AH")), "not_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("KAB.PM.ZZ")), "not_surgery")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("ABG.JB.AE")), "not_surgery")
  # A percutaneous puncture of the skull or canal is no incision and no
  # drainage: the neurosurgery axis admits the percutaneous means for the
  # external drainage alone, and such a code asks of its own.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("AAE.JA.AE")), "no_group")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("MAA.QQ.AE")), "no_group")
  expect_equal(
    as.character(neoipcr:::get_procedure_category("AAE.GA.AB")), "neurosurgery")
})

test_that("a valid code no rule places, and a malformed value, are a code no group takes", {
  expect_equal(as.character(neoipcr:::get_procedure_category("ZZZ.ZZ.ZZ")), "no_group")
  expect_equal(as.character(neoipcr:::get_procedure_category("PZX.AA.JA")), "no_group")
  expect_equal(as.character(neoipcr:::get_procedure_category("NOT A CODE")), "no_group")
})

test_that("get_procedure_category emits the current spelling of the residual code", {
  # A code no group takes falls through to `no_group`, and it is a value
  # that can reach `surgery_rate_table` and therefore a serialized dataset —
  # so the spelling is a storage contract, not a display choice.
  expect_equal(
    as.character(neoipcr:::get_procedure_category("ZZZ.ZZ.ZZ")), "no_group")
})

test_that("get_procedure_category keeps NA for a code that was not recorded", {
  expect_equal(
    as.character(neoipcr:::get_procedure_category(NA_character_)), NA_character_)
})

test_that("get_procedure_category turns every code that places nothing out with not_surgery_na", {
  # The flag serves the surgical counts: only a code a group takes may hold
  # a procedure in them, so a code that is no surgical procedure and a code
  # no group takes alike read as missing there.
  result <- neoipcr:::get_procedure_category(
    c("BCC.GA.AA", "ZZZ.ZZ.ZZ", "KBF.LI.AA"), not_surgery_na = TRUE)
  expect_identical(
    as.character(result), c(NA_character_, NA_character_, "abdominal_surgery"))
})

test_that("the placement table holds no code twice and no unknown group", {
  table <- neoipcr:::.procedure_code_table
  expect_equal(sum(duplicated(table$procedure_code)), 0L)
  expect_true(all(table$group %in% c(
    names(recorded_groups), "not_surgery", NA_character_)))
  expect_true(all(table$tract %in% c(
    "none", "gastrointestinal", "respiratory", "urinary", "genital", "unclear",
    NA_character_)))
})

# --- The combination of a procedure's codes ---------------------------------

group_of <- function(main, side1 = NA_character_, side2 = NA_character_) {
  as.character(neoipcr::get_procedure_group(main, side1, side2))
}

test_that("a procedure joins the first group, in order, that any of its codes places", {
  # Group order: neurosurgery, cardiac and great-vessel, non-cardiac
  # thoracic, abdominal, abdominal wall, hernia, head and neck, skin and
  # urogenital.
  expect_equal(group_of("KBF.LI.AA", "PAM.MK.AA"), "abdominal_surgery")
  # The abdominal wall's group comes before the hernia's.
  expect_equal(
    group_of("PAM.MK.AA", "PAK.LI.AA"), "abdominal_wall_surgery")
  expect_equal(group_of("PAK.LI.AA", "NAM.MK.AA"), "abdominal_wall_surgery")
  expect_equal(
    group_of("KAB.FB.AC", "PAW.JB.AA"), "head_neck_upper_airway_surgery")
  # The oesophagus is placed in the chest, so an atresia repair beside a
  # gastric code joins the thoracic group.
  expect_equal(group_of("KBF.LI.AA", "KBA.MK.AA"), "non_cardiac_thoracic_surgery")
  expect_equal(group_of("HIK.MK.AA", "AAE.JB.AA"), "neurosurgery")
})

test_that("a procedure counts once whatever of its codes was entered as the main one", {
  expect_equal(group_of("PAM.MK.AA", "KBF.LI.AA"), group_of("KBF.LI.AA", "PAM.MK.AA"))
  expect_equal(group_of("PAM.MK.AA", NA, "KBF.LI.AA"),
               group_of("PAM.MK.AA", "KBF.LI.AA"))
})

test_that("an approach code gives way to the operation done through it", {
  expect_equal(group_of("PAK.FA.AA", "KBF.LI.AA"), "abdominal_surgery")
  expect_equal(group_of("MAA.FA.AA", "AAE.JB.AA"), "neurosurgery")
  expect_equal(group_of("PAK.AE.AA", "HIK.MK.AA"), "cardiac_great_vessel_surgery")
})

test_that("a procedure whose only placed codes are approach codes counts in the approach's group", {
  expect_equal(group_of("PAK.FA.AA"), "abdominal_surgery")
  expect_equal(group_of("JCH.AE.AA", "PAK.AE.AB"), "non_cardiac_thoracic_surgery")
  # A code that is no surgical procedure drops out first, so this procedure
  # holds nothing but its approach code.
  expect_equal(group_of("PAK.JB.AE", "PAK.AE.AA"), "abdominal_surgery")
})

test_that("codes that are no surgical procedure drop out", {
  expect_equal(group_of("JBB.AE.AD", "KBF.LI.AA"), "abdominal_surgery")
  expect_equal(group_of("JBB.AE.AD"), "not_surgery")
  expect_equal(group_of("KMA.JB.AE"), "not_surgery")
  expect_equal(group_of("PAK.JB.AE"), "not_surgery")
})

test_that("a percutaneous pleural drain counts as surgery, in the thoracic group", {
  expect_equal(group_of("JCB.JB.AE"), "non_cardiac_thoracic_surgery")
})

test_that("a peritoneum operation is abdominal unless the procedure holds a shunt code", {
  expect_equal(group_of("KMA.FA.AA"), "abdominal_surgery")
  # The revision of a shunt's peritoneal end beside its shunt code follows
  # the shunt: the group order already places the procedure in
  # neurosurgery.
  expect_equal(group_of("KMA.FA.AA", "AAE.LI.AA"), "neurosurgery")
})

test_that("a pending code places nothing, and a procedure rests on its other codes", {
  expect_equal(group_of("KPB.LI.AA", "PAK.FA.AA"), "abdominal_surgery")
  expect_equal(group_of("KZZ.AD.AA", "KBF.LI.AA"), "abdominal_surgery")
  # With nothing else to rest on, the procedure is one its codes no group
  # takes.
  expect_equal(group_of("LZZ.DK.AH"), "no_group")
  expect_equal(group_of("LZZ.DK.AH", "JBB.AE.AD"), "no_group")
})

test_that("a held-pending code holds its whole procedure until the answer", {
  # `NMR.MK.AB` can follow no birth, and `BAA.BA.AD` decodes to no
  # intervention: the procedures that hold them rest nowhere until the
  # partners say what was done, even where their other codes would place
  # them.
  expect_equal(group_of("NMR.MK.AB", "ABA.ML.AA", "NAM.MK.AA"), "no_group")
  expect_equal(group_of("BAA.BA.AD", "KBF.LI.AA"), "no_group")
})

test_that("a procedure whose codes no group takes is one no group takes", {
  expect_equal(group_of("ZZZ.ZZ.ZZ"), "no_group")
  expect_equal(group_of("NOT A CODE", "ZZZ.ZZ.ZZ"), "no_group")
  # A valid code no group takes does not hold a procedure out that its
  # other code places.
  expect_equal(group_of("PZX.AA.JA", "PAK.FA.AA"), "abdominal_surgery")
})

test_that("an expression places by the intervention stems it is built of", {
  # The grammar reads a stem with extension clauses, a stem with an
  # other-code suffix, and stems of interventions done together joined by
  # "/"; the stems place, each as the bare code would.
  expect_equal(group_of("AAE.LI.AA&XK9K"), "neurosurgery")
  expect_equal(group_of("PAK.LI.AA&XK9K"), "abdominal_wall_surgery")
  expect_equal(group_of("MAA.FA.AA/KBF.LI.AA"), "abdominal_surgery")
  expect_equal(group_of("KBF.LI.AA/AAE.LI.AA"), "neurosurgery")
  expect_equal(group_of("JBB.AE.AD/KBF.LI.AA"), "abdominal_surgery")
  expect_equal(group_of("KBF.LI.AA#ICF d6402"), "abdominal_surgery")
  # A held-pending stem inside an expression holds the procedure, and a
  # value no grammar reads is a code no group takes.
  expect_equal(group_of("KBF.LI.AA/NMR.MK.AB"), "no_group")
  expect_equal(group_of("KBF.LI.AAA"), "no_group")
  # A value no grammar reads is a code no group takes, so the procedure is
  # no surgical procedure only once the value is corrected: it does not
  # fall back beside a code that is no surgical procedure, and a code that
  # places still places.
  expect_equal(group_of("NOT A CODE", "BCC.GA.AA"), "no_group")
  expect_equal(group_of("KBF.LI.AAA", "PAM.MK.AA"),
               "inguinal_umbilical_hernia_repair")
})

test_that("a procedure without any code is no procedure", {
  expect_equal(group_of(NA_character_), NA_character_)
  expect_equal(group_of(NA_character_, NA_character_, NA_character_), NA_character_)
})

test_that("get_procedure_group recycles its arguments and returns a factor of the groups", {
  result <- neoipcr::get_procedure_group(
    c("PAM.MK.AA", "BCC.GA.AA", "ZZZ.ZZ.ZZ", NA_character_),
    c(NA_character_, "KBF.LI.AA", NA_character_, NA_character_))
  expect_s3_class(result, "factor")
  expect_equal(
    levels(result),
    c("neurosurgery", "cardiac_great_vessel_surgery", "non_cardiac_thoracic_surgery",
      "abdominal_surgery", "abdominal_wall_surgery",
      "inguinal_umbilical_hernia_repair", "head_neck_upper_airway_surgery",
      "skin_soft_tissue_urogenital_surgery", "not_surgery", "no_group"))
  expect_identical(as.character(result),
                   c("inguinal_umbilical_hernia_repair", "abdominal_surgery",
                     "no_group", NA_character_))
})

test_that("get_procedure_group takes its arguments as vctrs recycles them", {
  # An empty argument decides an empty result of the same factor levels,
  # length 1 covers a whole vector, any other mismatch is refused, and a
  # factor stands for its labels.
  empty <- neoipcr::get_procedure_group(character(0))
  expect_length(empty, 0L)
  expect_equal(levels(empty),
               c(neoipcr:::.procedure_group_levels, "not_surgery",
                 "no_group"))
  expect_identical(
    as.character(neoipcr::get_procedure_group(
      c("KBF.LI.AA", "PAM.MK.AA"), NA_character_)),
    c("abdominal_surgery", "inguinal_umbilical_hernia_repair"))
  expect_error(
    neoipcr::get_procedure_group(
      c("KBF.LI.AA", "PAM.MK.AA"),
      c("KBF.LI.AA", "PAM.MK.AA", "PZX.AA.JA")),
    "share one length")
  # An empty argument is compatible only with empty or length-1 ones, as
  # vctrs recycles: empty beside a whole vector is refused, not silenced.
  expect_error(
    neoipcr::get_procedure_group(
      character(0), c("KBF.LI.AA", "PAM.MK.AA", "PZX.AA.JA")),
    "share one length")
  expect_identical(
    as.character(neoipcr::get_procedure_group(factor("KBF.LI.AA"))),
    "abdominal_surgery")
})

test_that("get_procedure_group is exported", {
  namespace <- readLines(system.file("NAMESPACE", package = "neoipcr"))
  expect_true("export(get_procedure_group)" %in% namespace)
})

# --- The risk strata within a group ------------------------------------------

stratum_of <- function(main, side1 = NA_character_, side2 = NA_character_,
                       wound_class, implant = NA, primary_closure = NA) {
  as.character(neoipcr:::.procedure_risk_stratum(
    main, side1, side2,
    wound_class = factor(wound_class, levels = c("1", "2", "3", "4")),
    implant = implant, primary_closure = primary_closure))
}

test_that("the strata place a procedure by its wound class", {
  expect_equal(stratum_of("KBF.LI.AA", wound_class = "3"), "contaminated_or_dirty")
  expect_equal(stratum_of("KBF.LI.AA", wound_class = "4"), "contaminated_or_dirty")
  expect_equal(stratum_of("PAM.MK.AA", wound_class = "2"), "clean_contaminated")
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean")
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = TRUE, primary_closure = TRUE),
    "clean_with_implant")
})

test_that("a class I procedure with a code of its group opening a tract is clean-contaminated", {
  # The alimentary tract gastrointestinal, and the respiratory, genital, and
  # urinary tracts by their own names.
  expect_equal(
    stratum_of("KBF.LI.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean_contaminated")
  expect_equal(
    stratum_of("JBF.JJ.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean_contaminated")
  expect_equal(
    stratum_of("NAM.MK.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean_contaminated")
  expect_equal(
    stratum_of("KAB.FB.AC", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean_contaminated")
  # The tract outweighs the implant flag in the stratum; the flag keeps its
  # own work in the follow-up window.
  expect_equal(
    stratum_of("KBF.LI.AA", wound_class = "1", implant = TRUE, primary_closure = TRUE),
    "clean_contaminated")
})

test_that("only a code of the procedure's own group opens its tract", {
  # The procedure joins the thoracic group; the abdominal code's alimentary
  # tract is no code of that group, so the class I thoracic code with no
  # tract of its own group stays clean.
  expect_equal(
    stratum_of("MCX.MK.AA", "KBF.LI.AA",
               wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean")
})

test_that("a code whose tract is unclear raises nothing", {
  expect_equal(
    stratum_of("KBF.FA.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean")
  expect_equal(
    stratum_of("KBZ.LC.AA", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean")
})

test_that("the strata place nothing the forms leave unplaced", {
  # A class I procedure not closed primarily, or a wound class that was not
  # recorded, or a procedure no group takes: the four strata do not place it.
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = FALSE, primary_closure = FALSE),
    NA_character_)
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = FALSE, primary_closure = NA),
    NA_character_)
  # Both clean strata are closed primarily, and a clean one needs the
  # implant recorded as absent: an implant beside a wound not closed
  # primarily, and an implant not recorded at all, stratify nothing.
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = TRUE, primary_closure = FALSE),
    NA_character_)
  expect_equal(
    stratum_of("PAM.MK.AA", wound_class = "1", implant = NA, primary_closure = TRUE),
    NA_character_)
  expect_equal(stratum_of("PAM.MK.AA", wound_class = NA_character_), NA_character_)
  expect_equal(
    stratum_of("ZZZ.ZZ.ZZ", wound_class = "1", implant = FALSE, primary_closure = TRUE),
    NA_character_)
  expect_equal(
    stratum_of(NA_character_, wound_class = "1", implant = FALSE, primary_closure = TRUE),
    NA_character_)
})

test_that("the strata take the procedure's codes together", {
  # The tract of the operation done through the approach opens the tract of
  # the procedure's group.
  expect_equal(
    stratum_of("PAK.FA.AA", "KBF.LI.AA",
               wound_class = "1", implant = FALSE, primary_closure = TRUE),
    "clean_contaminated")
})

# --- The labels --------------------------------------------------------------

test_that("get_procedure_category_pretty labels the group codes", {
  expect_equal(
    neoipcr:::get_procedure_category_pretty(names(recorded_groups)),
    c("Neurosurgery", "Cardiac and great-vessel surgery",
      "Non-cardiac thoracic surgery", "Abdominal surgery",
      "Abdominal wall surgery", "Inguinal and umbilical hernia repair",
      "Head, neck, and upper airway surgery",
      "Skin, soft tissue, perineal, and urogenital surgery"))
  expect_equal(
    neoipcr:::get_procedure_category_pretty(c("overall", "not_surgery", "no_group")),
    c("Overall", "Not a surgical procedure", "Placed by no procedure group"))
})

# NEOIPC-PERMANENT(dataset-format): these tests guard paths that must never
# be removed. A dataset serialized while the old categories stood carries
# their codes, and a file on disk outlives the code that wrote it — so the
# retired codes stay renderable for good. Delete these tests only if the
# mapping they guard is deliberately being dropped, which it should not be.
test_that("get_procedure_category_pretty still labels every retired category code", {
  expect_equal(
    neoipcr:::get_procedure_category_pretty("abdominal_surgery"),
    "Abdominal surgery")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("neurosurgery"), "Neurosurgery")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("cardiac_and_large_vessel_surgery"),
    "Cardiac- / large vessel surgery")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("lung_pleural_space_thoracic_surgery"),
    "Lung- / pleural space- / thoracic surgery")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("oesophageal_surgery"),
    "Oesophageal surgery")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("inguinal_hernia_surgery"),
    "Inguinal hernia surgery")
  expect_equal(neoipcr:::get_procedure_category_pretty("other"), "Other")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("not_surgery"),
    "Not a surgical procedure")
})

test_that("get_procedure_category_pretty still labels both spellings of the old residual", {
  expect_equal(
    neoipcr:::get_procedure_category_pretty("to_be_categorized"),
    "Not yet categorized")
  expect_equal(
    neoipcr:::get_procedure_category_pretty("to_be_categorised"),
    "Not yet categorized")
})

test_that("get_procedure_category_pretty passes an unknown code through unchanged", {
  expect_equal(neoipcr:::get_procedure_category_pretty("not_a_category"),
               "not_a_category")
})
