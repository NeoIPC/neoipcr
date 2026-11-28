# The eight procedure groups, the placement of the recorded ICHI codes among
# them, and the classifier the reports and the validation rules share.
#
# A surgical procedure is grouped by the site its codes place it at and the
# tract it opens, as API-DESIGN.md §13.9.2 defines it; the recorded codes
# with their groups, tracts, approach codes and pending codes are the test
# cases of these rules. A code the table holds is placed as the table says,
# and it stands wherever an axis below would place the code otherwise: the
# axes serve only a code the table does not hold, and never contradict the
# table. An ICHI code expression is read as
# the intervention stems it is built of, and only the stems place; a value
# no grammar reads places nothing. A code no group takes stays out of
# every group: the warnings ask the partners what was done, and only the
# answer places it.

#' Group surgical procedures by the site their ICHI codes place them at
#'
#' @description
#' `get_procedure_group()` classifies a surgical procedure, given by its main
#' procedure code and its two side procedure codes, into one of the eight
#' procedure groups: the site the procedure is placed at, as API-DESIGN.md
#' §13.9.2 defines it and the placement table of this file records it. Each
#' slot may hold a full ICHI code
#' expression, and every intervention stem of every slot counts among the
#' procedure's codes; a malformed value is a code no group takes. Every code
#' of a procedure counts:
#' the procedure joins the first group, in the fixed order of the group
#' levels, that any of its codes places, so the same procedure is grouped the
#' same way whichever of its codes was entered as the main one. An approach
#' code places a procedure only when it holds nothing but approach codes, and
#' a code that is no surgical procedure drops out. A code that places
#' nothing — a valid code no rule places, a malformed value, or a code whose
#' query is still pending — counts for nothing in the placement: the
#' procedure takes its group from the codes that do place, and is no
#' surgical procedure or placed by no group according as the codes left over
#' are all codes that are no surgical procedure or place nowhere. Two of the
#' pending codes hold their whole procedure: they name no operation which
#' the procedure's other codes can be trusted to be, so a procedure holding
#' one of them is placed by no group whatever else it holds, until the query
#' is answered. The warnings ask what was done.
#'
#' @param main Character vector of main ICHI procedure codes.
#' @param side1 Character vector of first side ICHI procedure codes. `NA`
#'   stands for a slot that holds no code, and one `NA` covers a whole
#'   vector of procedures without a first side code.
#' @param side2 Character vector of second side ICHI procedure codes, with
#'   the same meaning. The arguments recycle as `vctrs` does: length 1
#'   covers a whole vector; an empty argument makes the result empty unless
#'   another holds more than one code; a length that is none of these is
#'   refused. A factor stands for its labels.
#' @returns A factor with the eight procedure groups as levels in their
#'   fixed order, plus `not_surgery` for a procedure all of whose codes are
#'   no surgical procedure, `no_group` for one its codes place nowhere, and
#'   `NA` for a procedure without any code.
#' @export
get_procedure_group <- function(main, side1 = NA_character_,
                                side2 = NA_character_) {
  # A factor stands for its labels; the lengths recycle as vctrs does:
  # length 1 covers a whole vector, an empty argument is compatible only
  # with empty and length-1 arguments and decides an empty result, and any
  # other mismatch is a mistake in the call, not something to silently
  # repeat.
  main <- as.character(main)
  side1 <- as.character(side1)
  side2 <- as.character(side2)
  lens <- c(length(main), length(side1), length(side2))
  n <- max(lens)
  ok <- if (n <= 1L) c(0L, 1L) else c(1L, n)
  if (!all(lens %in% ok))
    rlang::abort(c(
      gettext("Arguments to `get_procedure_group()` must share one length or have length 1."),
      i = gettextf("Lengths given: %d, %d and %d.",
                   lens[[1]], lens[[2]], lens[[3]])))
  if (any(lens == 0L))
    return(factor(character(0),
                  levels = c(.procedure_group_levels, "not_surgery",
                             "no_group")))
  main <- rep_len(main, n)
  side1 <- rep_len(side1, n)
  side2 <- rep_len(side2, n)
  held <- rep_len(.procedure_holds_procedure(main, side1, side2), n)
  group <- .procedure_group_row(main, side1, side2)
  group[held] <- "no_group"
  group[is.na(main) & is.na(side1) & is.na(side2)] <- NA_character_
  factor(group,
         levels = c(.procedure_group_levels, "not_surgery", "no_group"))
}

# The groups in their order: a procedure whose several codes place it in
# more than one group joins the first of them in this order.
.procedure_group_levels <- c(
  "neurosurgery",
  "cardiac_great_vessel_surgery",
  "non_cardiac_thoracic_surgery",
  "abdominal_surgery",
  "abdominal_wall_surgery",
  "inguinal_umbilical_hernia_repair",
  "head_neck_upper_airway_surgery",
  "skin_soft_tissue_urogenital_surgery")

# The tracts whose opening raises a class I wound's risk. `none` covers a
# procedure that opens no tract; `unclear` covers a code whose description
# does not settle whether it does, and raises nothing.
.procedure_opening_tracts <- c("gastrointestinal", "respiratory", "urinary",
                               "genital")

# The shunt codes: beside one of them, `KMA.FA.AA` is the shunt's
# peritoneal end and its placement asks nothing; without one it is an
# operation on the peritoneum and the warning asks what was done.
.procedure_shunt_codes <- c("AAE.JB.AA", "AAE.KA.AA", "AAE.LH.AA",
                            "AAE.LI.AA", "AAE.LI.AB", "AAE.SN.AA")

# The placement of every ICHI code the production data carries: its group,
# the tract it opens, whether it is an approach code, and — among the pending
# codes — whether it holds its whole procedure. A pending code's group is
# `NA`: it places nothing until the answer to its query; the classifier gives
# it the residual code no group takes.
.procedure_code_table <- local({
  chunks <- list(
    list("neurosurgery", "none", c(
      "AAA.JB.AA",
      "AAE.DN.AA",
      "AAE.GA.AE",
      "AAE.JB.AA",
      "AAE.JD.AA",
      "AAE.KA.AA",
      "AAE.LH.AA",
      "AAE.LI.AA",
      "AAE.LI.AB",
      "AAE.SN.AA",
      "AAG.DN.AA",
      "ABA.FC.AA",
      "ABA.MK.AA",
      "ABA.ML.AA",
      "ABG.LI.AA",
      "MAA.FA.AA"
    )),
    list("cardiac_great_vessel_surgery", "none", c(
      "HBB.JD.AA",
      "HIJ.LA.AA", # the arterial duct closes with the heart, whoever performs it
      "HIJ.LA.AB",
      "HIK.MK.AA"
    )),
    list("non_cardiac_thoracic_surgery", "none", c(
      "HIK.LJ.AA",
      "JCA.JD.AA",
      "JCB.JB.AE", # the pleura's drainage is the one percutaneous means that counts
      "JCH.AE.AA",
      "JCH.FA.AA",
      "MCX.MK.AA", # the diaphragm sits with the chest, by either approach
      "MCX.MK.AB"
    )),
    list("non_cardiac_thoracic_surgery", "respiratory", c(
      "JBA.MK.AA",
      "JBF.JJ.AA",
      "JBF.LL.AA"
    )),
    list("non_cardiac_thoracic_surgery", "gastrointestinal", c(
      # The oesophagus is placed in the chest.
      "KBA.LB.AA",
      "KBA.MK.AA"
    )),
    list("abdominal_surgery", "none", c(
      "DJA.MK.AA",
      "IED.LA.AA",
      "KBZ.LD.AA",
      "KMA.DN.AA",
      "KMA.FA.AA",
      "KMA.FC.AA",
      "KMA.JA.AA",
      "KMA.JB.AA",
      "KMA.JJ.AA",
      "KMA.KA.AA",
      "KMA.MK.AA",
      "PAK.AE.AA",
      "PAK.AE.AB",
      "PAK.FA.AA",
      "PAK.FC.AA",
      "PAK.FC.AB",
      "PAK.JB.AA",
      "PAL.FA.AA"
    )),
    list("abdominal_surgery", "gastrointestinal", c(
      "KBF.AE.AB",
      "KBF.LI.AA",
      "KBF.LI.AB",
      "KBF.MK.AA",
      "KBI.LB.AA",
      "KBI.MK.AA",
      "KBI.ML.AA",
      "KBK.JI.AA",
      "KBK.JJ.AA",
      "KBK.JJ.AB",
      "KBK.LB.AA",
      "KBK.LI.AA",
      "KBK.LI.AB",
      "KBK.MK.AA",
      "KBK.MK.AB",
      "KBO.JK.AA",
      "KBO.JK.AB",
      "KBP.FA.AB",
      "KBP.JJ.AA",
      "KBP.LI.AA",
      "KBP.MK.AA",
      "KBW.JK.AA",
      "KBZ.JJ.AA",
      "KBZ.JJ.AB",
      "KBZ.LB.AA",
      "KBZ.MK.AA",
      "KZZ.MK.AA",
      "PTA.LA.AC",
      "PTB.LA.AC"
    )),
    list("abdominal_surgery", "unclear", c(
      "KBF.FA.AA",
      "KBZ.LC.AA"
    )),
    list("abdominal_wall_surgery", "none", c(
      "PAK.LI.AA", # re-entry of an open abdomen, placed with the wall
      "PAL.MK.AA",
      "PAL.MK.AB",
      "PAL.ML.AA",
      "PAO.JI.AA"
    )),
    list("inguinal_umbilical_hernia_repair", "none", c(
      "PAM.MK.AA",
      "PAM.MK.AB",
      "PAO.MK.AA",
      "PAO.MK.AB"
    )),
    list("head_neck_upper_airway_surgery", "respiratory", c(
      "JAM.ML.AD",
      "JAN.JI.AD",
      "JAN.MK.AD",
      "JBA.LI.AA"
    )),
    list("head_neck_upper_airway_surgery", "unclear", c(
      "KAA.AD.AA"
    )),
    list("head_neck_upper_airway_surgery", "gastrointestinal", c(
      # A frenotomy is an incision of the mucosa.
      "KAB.FB.AC"
    )),
    list("skin_soft_tissue_urogenital_surgery", "none", c(
      "LAB.LL.AA",
      "LAG.JG.AA",
      "LCA.JG.AA",
      "NGL.LC.AA",
      "PAW.JB.AA"
    )),
    list("skin_soft_tissue_urogenital_surgery", "urinary", c(
      "NAM.MK.AA"
    )))
  outside <- c(
    # Imaging, whatever the anatomical target.
    "AAA.BA.BH", "ABA.BA.BH", "MBO.BA.BC", "PAB.BA.BH", "PAE.BA.BH",
    "PAK.BA.BH", "PZA.BA.BH",
    # Endoscopy without an incision of a tract.
    "JAN.AE.AC", "JBA.AE.AB", "JBB.AE.AD", "KBA.LG.AD", "KBI.AE.AD",
    # Treatment of retinopathy of prematurity.
    "BCC.GA.AA", "BCD.DB.AE",
    # Vascular access.
    "HIB.DL.AF", "IBD.DL.AF", "IDB.DL.AF", "IZD.DL.AF",
    # Transcatheter heart intervention.
    "HDG.LG.AF",
    # Needle punctures and percutaneous drains. Only the pleura's drainage
    # counts as a surgical procedure, and that one sits among the thoracic
    # codes above; the warning asks about the abdomen's two when the form
    # describes an open procedure.
    "JBF.JB.AE", "JBF.LL.AE", "KMA.JB.AE", "NAA.LI.AE", "PAK.JB.AE",
    # Device, stoma and wound care, and education.
    "JBA.KA.AC", "KBF.DL.AC", "KBF.KA.AC", "KBK.DL.AC", "KBK.LD.AH",
    "KBK.LG.AC", "LAB.JG.AH", "PTA.PM.ZZ", "PTB.SN.AC", "PTC.PM.ZZ")
  # Codes that name no operation that can be placed: what was done waits on
  # the answer to a query. The two held codes hold their whole procedure out
  # of every group until then, since no other code of such a procedure can
  # be trusted to be the operation; the other four place nothing and leave
  # the procedure to its codes that do place.
  pending <- c("BAA.BA.AD", "LZZ.DK.AH", "KZZ.AD.AA", "NMR.MK.AB",
               "NZZ.ZZ.ZZ", "KPB.LI.AA")
  pending_held <- c("BAA.BA.AD", "NMR.MK.AB")
  # Approach codes name the road taken and not the operation done, so they
  # place a procedure only when it holds nothing else that places.
  approach <- c("MAA.FA.AA", "JCH.AE.AA", "JCH.FA.AA", "PAK.AE.AA",
                "PAK.AE.AB", "PAK.FA.AA")

  group <- data.frame(
    procedure_code = unlist(lapply(chunks, \(ch) ch[[3]]), use.names = FALSE),
    group = unlist(lapply(chunks, \(ch)
      rep(ch[[1]], length(ch[[3]]))), use.names = FALSE),
    tract = unlist(lapply(chunks, \(ch)
      rep(ch[[2]], length(ch[[3]]))), use.names = FALSE),
    stringsAsFactors = FALSE)
  outside <- data.frame(
    procedure_code = outside, group = "not_surgery",
    tract = NA_character_, stringsAsFactors = FALSE)
  pending <- data.frame(
    procedure_code = pending, group = NA_character_,
    tract = NA_character_, stringsAsFactors = FALSE)
  table <- rbind(group, outside, pending)
  table$approach <- table$procedure_code %in% approach
  table$holds_procedure <- table$procedure_code %in% pending_held
  # Codes placed by a group yet whose placement the partners queried: the
  # peritoneal dialysis access counts as abdominal unless the procedure
  # holds a shunt code beside it, and the umbilical vein repair was recorded
  # only with an implant and is probably a patch closure. The warning asks
  # about the access whenever the procedure holds no shunt code, and about
  # the repair whatever else the procedure holds.
  table$conditional <- table$procedure_code %in% c("KMA.FA.AA", "KMA.MK.AA")
  table
})

# The axes of target, action and means that place a code the table does not
# hold. A recorded code is never asked: the table's placement of it stands.
# A valid code no rule places, and a value that is no ICHI code at all, fall
# to `NA`: the code no group takes.
.procedure_group_by_axis <- function(x) {
  # A pending code waits on the partners' answer, which may be an
  # operation or no operation: the axes assert no placement for it.
  pending <- .procedure_code_table$procedure_code[
    is.na(.procedure_code_table$group)]
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)

  dplyr::case_when(
    x %in% pending ~ NA_character_,
    # Neurosurgery: interventions on the brain, its ventricles and
    # coverings, the spinal cord and canal, and the skull, by an incision
    # or an excision; the percutaneous means is admitted only as the
    # external drainage the recorded codes show it for, a needle puncture
    # of the skull or canal being no incision.
    target %in% c(
      "AAA", # brain, several sites at once
      "AAE", # brain's ventricles
      "AAG", # intracranial space
      "ABA", # spinal cord
      "ABG", # spinal canal
      "MAA"  # skull
      ) &
      (means %in% c("AA", "AB") |
        (action == "GA" & means == "AE")) ~ "neurosurgery",

    # Cardiac and great-vessel surgery. Vascular catheters and ports, and
    # transcatheter heart interventions, never count, so the axis admits the
    # open and endoscopic means only — never the percutaneous one.
    target == "HIJ" & action == "LA" & means %in% c("AA", "AB") ~
      "cardiac_great_vessel_surgery",
    target == "HIK" & action == "MK" & means == "AA" ~
      "cardiac_great_vessel_surgery",

    # Non-cardiac thoracic surgery. The percutaneous drainage of the pleural
    # cavity counts as surgery wherever it is coded, so the percutaneous
    # means is admitted for a thoracic target only as the pleura's drainage;
    # every thoracic target admits the open and endoscopic means.
    target %in% c(
      "MCX", # diaphragm
      "JBF", # lung
      "JCA", # visceral pleura
      "JCH"  # thoracic cavity
      ) &
      means %in% c("AA", "AB") ~ "non_cardiac_thoracic_surgery",
    target == "JCB" &
      (means %in% c("AA", "AB") |
        (action == "JB" & means == "AE")) ~
      "non_cardiac_thoracic_surgery",

    # The oesophagus is placed in the chest.
    target == "KBA" & means %in% c("AA", "AB") ~
      "non_cardiac_thoracic_surgery",

    # Abdominal surgery, the abdominal wall's and the hernia's repairs
    # aside; the shunt's distal ends follow their target's means.
    target %in% c(
      "KBF", # stomach
      "KBI", # duodenum
      "KBK", # small intestine, site left open
      "KBO", # appendix
      "KBP", # colon
      "KBZ", # large intestine, site left open
      "KMA", # peritoneum
      "PAK"  # abdomen, site left open
      ) &
      means %in% c("AA", "AB") &
      !(target == "PAK" & action == "LI") ~ "abdominal_surgery",
    target %in% c("PTA", "PTB") & action == "LA" & means == "AC" ~
      "abdominal_surgery", # the two recorded stoma closures
    target == "PAL" & action == "FA" & means %in% c("AA", "AB") ~
      "abdominal_surgery", # the wall's incision rests with the abdomen's

    # The abdominal wall: the wall's repairs and the release of a
    # laparostomy; the wall's incision rests with the abdomen's.
    target == "PAL" & action != "FA" & means %in% c("AA", "AB") ~
      "abdominal_wall_surgery",
    target == "PAO" & action != "MK" & means %in% c("AA", "AB") ~
      "abdominal_wall_surgery",
    target == "PAK" & action == "LI" & means %in% c("AA", "AB") ~
      "abdominal_wall_surgery",

    # Inguinal and umbilical hernia repair.
    target == "PAM" & means %in% c("AA", "AB") ~
      "inguinal_umbilical_hernia_repair",
    target == "PAO" & action == "MK" & means %in% c("AA", "AB") ~
      "inguinal_umbilical_hernia_repair",

    # Head, neck and upper airway: the pharynx, the larynx, the lip and
    # the tongue, the trachea's tracheostomy among them; the looking
    # about through a natural orifice is no incision. The trachea's other
    # operations rest with the chest's group.
    target %in% c(
      "JAM", # pharynx
      "JAN", # larynx
      "KAA", # lip
      "KAB"  # tongue
      ) &
      action != "AE" & means %in% c("AA", "AB", "AC", "AD") ~
      "head_neck_upper_airway_surgery",
    target == "JBA" & action == "LI" & means %in% c("AA", "AB") ~
      "head_neck_upper_airway_surgery",

    # Skin, soft tissue, perineal and urogenital surgery.
    target %in% c(
      "LAB", # skin and soft tissue of the trunk
      "LAG", # skin and soft tissue of head and neck
      "LCA", # breast
      "NAM", # urethra
      "NGL", # testis
      "PAW"  # perineum
      ) &
      means %in% c("AA", "AB") ~ "skin_soft_tissue_urogenital_surgery",

    # The codes that are no surgical procedure: imaging, the transluminal
    # and the external means, education, and every percutaneous drainage
    # of a site but the pleura's, which the chest's clauses place first.
    action == "BA" & means %in% c("BH", "BC") ~ "not_surgery",
    means == "AF" ~ "not_surgery",
    means == "AH" ~ "not_surgery",
    action == "PM" ~ "not_surgery",
    action == "JB" & means == "AE" ~ "not_surgery",

    .default = NA_character_)
}

# The intervention stems of one ICHI code expression. The grammar composes
# an expression of a stem with extension clauses and other-code suffixes,
# and joins the stems of interventions done together with "/"; only stems
# classify. An expression the grammar rejects yields no stem: a malformed
# value places nothing.
.procedure_stems_of_one <- function(code) {
  if (is.na(code) || !is_valid_ichi_code(code)) return(character(0))
  parts <- strsplit(code, "/", fixed = TRUE)[[1]]
  sub("[&#].*$", "", parts)
}

# The group each stem places by itself: the table for a stem it holds, the
# axes for one it does not, and the code no group takes for one neither
# places. `not_surgery` codes and pending alike pass through their lookup.
.procedure_group_for_stem <- function(stems) {
  idx <- match(stems, .procedure_code_table$procedure_code)
  in_table <- !is.na(idx)
  table_group <- .procedure_code_table$group[idx]
  axis <- .procedure_group_by_axis(stems)
  group <- dplyr::case_when(
    is.na(stems) ~ NA_character_,
    in_table & !is.na(table_group) ~ table_group,
    in_table ~ "no_group",
    !is.na(axis) ~ axis,
    .default = "no_group")
  group
}

# The group the given stems of one procedure place: the earliest group of a
# stem that is no approach code, an approach's group when nothing else
# places, and no surgical procedure when every stem is. `ungrammatical`
# marks that a filled code slot no grammar reads was left out of the stems;
# it is a code no group takes, so it stands against the last fallback.
.procedure_group_of_stems <- function(stems, ungrammatical = FALSE) {
  idx <- match(stems, .procedure_code_table$procedure_code)
  recorded <- !is.na(idx)
  groups <- .procedure_group_for_stem(stems)
  placed <- groups %in% .procedure_group_levels
  is_approach <- logical(length(stems))
  is_approach[recorded] <- .procedure_code_table$approach[idx[recorded]]
  non_approach <- placed & !is_approach
  if (any(non_approach)) {
    return(.procedure_first_group(groups[non_approach]))
  }
  if (any(placed)) return(.procedure_first_group(groups[placed]))
  # A procedure is no surgical procedure only when every one of its codes
  # says so: a code no group takes among them asks what was done, and the
  # answer could still place the procedure. A value no grammar reads asks
  # too, until it is corrected.
  if (all(groups == "not_surgery") && !ungrammatical) return("not_surgery")
  "no_group"
}

get_procedure_category <- function(x, not_surgery_na = FALSE) {
  group <- vapply(as.character(x), function(one) {
    if (is.na(one)) return(NA_character_)
    stems <- .procedure_stems_of_one(one)
    # A value no grammar reads — a malformed code — places nothing.
    if (length(stems) == 0L) return("no_group")
    .procedure_group_of_stems(stems)
  }, character(1), USE.NAMES = FALSE)
  if (not_surgery_na) {
    # The surgical counts hold only a code a group takes: a code that is no
    # surgical procedure and a code no group takes alike read as missing.
    group[group %in% c("not_surgery", "no_group")] <- NA_character_
  }
  levels <- c(.procedure_group_levels,
              if (!not_surgery_na) c("not_surgery", "no_group"))
  factor(group, levels = levels)
}

# Whether any of a procedure's codes holds the whole procedure out of every
# group: the held pending codes, which name no operation the other codes of
# their procedure can be trusted beside.
.procedure_holds_procedure <- function(main, side1, side2) {
  holds <- function(codes) {
    stems <- unlist(lapply(codes, .procedure_stems_of_one), use.names = FALSE)
    idx <- match(stems, .procedure_code_table$procedure_code)
    recorded <- which(!is.na(idx))
    length(recorded) > 0L &&
      any(.procedure_code_table$holds_procedure[idx[recorded]])
  }
  vapply(seq_along(main),
         \(i) holds(c(main[[i]], side1[[i]], side2[[i]])), logical(1))
}

# The group one procedure joins: the earliest group a code of its own
# operation places, an approach's group only when nothing else places, and
# the code that is no surgical procedure's own code when all its codes are.
# Each code slot holds a full ICHI code expression; its stems count among
# the procedure's codes, and a slot that is no valid expression places
# nothing yet marks the procedure as recorded.
.procedure_group_row <- function(main, side1, side2) {
  row <- function(m, s1, s2) {
    fields <- c(m, s1, s2)
    if (all(is.na(fields))) return(NA_character_)
    stems_per_field <- lapply(fields, .procedure_stems_of_one)
    stems <- unlist(stems_per_field, use.names = FALSE)
    # A slot was filled and no grammar reads it: a code no group takes,
    # left out of the stems yet standing against the surgical fallback.
    ungrammatical <- any(!is.na(fields) &
                           vapply(stems_per_field, length, integer(1)) == 0L)
    if (length(stems) == 0L) return("no_group")
    .procedure_group_of_stems(stems, ungrammatical = ungrammatical)
  }
  mapply(row, main, side1, side2, USE.NAMES = FALSE)
}

# The earliest of the given groups in the order of `.procedure_group_levels`.
.procedure_first_group <- function(groups) {
  groups[[which.min(match(groups, .procedure_group_levels))]]
}

# The risk stratum of a procedure within its group: its wound class, and —
# within class I — whether a code of the procedure's own group opens a tract
# and whether an implant was inserted or the wound closed primarily.
.procedure_risk_stratum <- function(main, side1, side2, wound_class,
                                    implant, primary_closure) {
  n <- max(vapply(list(main, side1, side2, wound_class, implant,
                       primary_closure), length, integer(1)))
  main <- rep_len(main, n)
  side1 <- rep_len(side1, n)
  side2 <- rep_len(side2, n)
  wound <- as.character(rep_len(wound_class, n))
  implant <- rep_len(implant, n)
  closure <- rep_len(primary_closure, n)
  group <- as.character(get_procedure_group(main, side1, side2))

  opens_tract <- vapply(seq_len(n), function(i) {
    g <- group[[i]]
    if (is.na(g) || !(g %in% .procedure_group_levels)) return(FALSE)
    stems <- unlist(lapply(c(main[[i]], side1[[i]], side2[[i]]),
                           .procedure_stems_of_one), use.names = FALSE)
    if (length(stems) == 0L) return(FALSE)
    idx <- match(stems, .procedure_code_table$procedure_code)
    recorded <- !is.na(idx)
    # Only a code of the procedure's own group opens its tract: an
    # unrecorded code's group by the axes counts too, and its tract, which
    # nothing records, raises nothing.
    code_group <- .procedure_group_for_stem(stems)
    own <- code_group == g
    tract <- rep(NA_character_, length(stems))
    tract[recorded] <- .procedure_code_table$tract[idx[recorded]]
    any(own & !is.na(tract) & tract %in% .procedure_opening_tracts)
  }, logical(1))

  # Both clean strata are closed primarily and open no tract: an implant
  # makes the one, an explicitly absent implant the other, and a class I
  # wound that fails either test counts in its group and in no stratum.
  stratum <- dplyr::case_when(
    is.na(group) | !(group %in% .procedure_group_levels) ~ NA_character_,
    is.na(wound) ~ NA_character_,
    wound %in% c("3", "4") ~ "contaminated_or_dirty",
    wound == "2" ~ "clean_contaminated",
    wound == "1" & opens_tract ~ "clean_contaminated",
    wound == "1" & !is.na(closure) & closure &
      !is.na(implant) & implant ~ "clean_with_implant",
    wound == "1" & !is.na(closure) & closure &
      !is.na(implant) & !implant ~ "clean",
    .default = NA_character_)
  factor(stratum,
         levels = c("clean", "clean_with_implant", "clean_contaminated",
                    "contaminated_or_dirty"))
}

get_procedure_category_pretty <- function(x) {
  dplyr::recode_values(
    as.character(x),
    "overall" ~ gettext("Overall"),
    "neurosurgery" ~ gettext("Neurosurgery"),
    "cardiac_great_vessel_surgery" ~
      gettext("Cardiac and great-vessel surgery"),
    "non_cardiac_thoracic_surgery" ~ gettext("Non-cardiac thoracic surgery"),
    "abdominal_surgery" ~ gettext("Abdominal surgery"),
    "abdominal_wall_surgery" ~ gettext("Abdominal wall surgery"),
    "inguinal_umbilical_hernia_repair" ~
      gettext("Inguinal and umbilical hernia repair"),
    "head_neck_upper_airway_surgery" ~
      gettext("Head, neck, and upper airway surgery"),
    "skin_soft_tissue_urogenital_surgery" ~
      gettext("Skin, soft tissue, perineal, and urogenital surgery"),
    "not_surgery" ~ gettext("Not a surgical procedure"),
    "no_group" ~ gettext("Placed by no procedure group"),
    # NEOIPC-PERMANENT(dataset-format): never remove the following lines.
    # This function renders whatever category vector it is handed, and such
    # a vector reaches it inside a dataset serialized before the groups
    # stood; a file on disk outlives the code that wrote it, so no condition
    # retires these. Without them those rows render as their bare code
    # instead of a label.
    "cardiac_and_large_vessel_surgery" ~ gettext("Cardiac- / large vessel surgery"),
    "lung_pleural_space_thoracic_surgery" ~ gettext("Lung- / pleural space- / thoracic surgery"),
    "oesophageal_surgery" ~ gettext("Oesophageal surgery"),
    "inguinal_hernia_surgery" ~ gettext("Inguinal hernia surgery"),
    "other" ~ gettext("Other"),
    "to_be_categorized" ~ gettext("Not yet categorized"),
    "to_be_categorised" ~ gettext("Not yet categorized"),
    default = x
  )
}

get_procedure_categories <- function(
    x, pretty = FALSE, include_iche = FALSE, use_cache = TRUE) {
  cache_key <- paste0("procedure_categories.v",
                      .surgical_definitions_version())

  # ToDo: Clarify licensing and inclusion criteria for ICHE information with WHO
  # and add ICHE table
  # if(include_iche)
  #   cache_key <- paste0(cache_key, ".iche")

  if(pretty)
  {
    l <- Sys.getenv("LANGUAGE")
    if(l == "")
      l <- Sys.getlocale("LC_MESSAGES")
    if(l == "")
      pretty <- FALSE # just in case
    else
      cache_key <- paste0(cache_key, ".", l)
  }

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- tibble::tibble(
    procedure_code = c(
      x$surgeryData$main_procedure_code,
      x$surgeryData$side_procedure_code_1,
      x$surgeryData$side_procedure_code_2) |>
      unique() |>
      sort() |>
      as.character()) |>
    dplyr::mutate(pro_cat = get_procedure_category(.data$procedure_code))

  if(pretty)
  {

    with_pretty <- r |>
      dplyr::select("pro_cat") |>
      dplyr::mutate(
        pretty_name = get_procedure_category_pretty(.data$pro_cat))

    pairs <- with_pretty |> dplyr::distinct()

    col_names <- stats::setNames(
      gettext("Procedure code","Procedure category"),
      c("procedure_code","pro_cat"))
    row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

    attr(r, "names.pretty") <- col_names
    attr(r, "row.names.pretty") <- row_names

    r <- r |>
    dplyr::mutate(
      pro_cat = with_pretty$pretty_name) |>
    dplyr::rename(
      !!col_names[["procedure_code"]] := .data$procedure_code,
      !!col_names[["pro_cat"]] := .data$pro_cat)
  }

  # if(include_iche)
  #   r <- r |>
  #   dplyr::inner_join(
  #     ichi_health_interventions,join_by(main_procedure_code == code))

  r |>
    cache(x, cache_key)
}
