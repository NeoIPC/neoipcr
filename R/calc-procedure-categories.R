get_procedure_categories <- function(
    x, pretty = FALSE, include_iche = FALSE, use_cache = TRUE) {
  cache_key <- "procedure_categories"

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

get_procedure_category <- function(x, not_surgery_na = FALSE) {
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)
  if (not_surgery_na) {
    not_surgery <- NA_character_
  } else {
    not_surgery <- "not_surgery"
  }

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    # Neurosurgery
    ############################################################################
    target %in% c(
      "AAE",# Interventions on ventricles of brain
      "AAG",# Interventions on intracranial space
      "ABA",# Interventions on spinal cord
      "ABG",# Interventions on spinal canal
      "MAA" # Interventions on skull
      ) &
      means %in% c("AA","AB","AE") ~ "neurosurgery",

    # Cardiac/large vessel surgery
    ############################################################################
    target == "HIJ" & action == "LA" ~ "cardiac_and_large_vessel_surgery",

    target == "HIK" & means == "AA" ~ "cardiac_and_large_vessel_surgery",

    # Lung/pleural space/thoracic surgery
    ############################################################################
    target %in% c(
      "MCX",# Interventions on diaphragm
      "JBF",# Interventions on lung parenchyma
      "JCA",# Interventions on pleura
      "JCB",# Interventions on pleura
      "JCH" # Interventions on thoracic cavity
    ) &
      means %in% c("AA","AB") ~ "lung_pleural_space_thoracic_surgery",

    # Oesophageal surgery
    ############################################################################
    target == "KBA" & means %in% c("AA","AB") ~ "oesophageal_surgery",

    # Abdominal surgery
    ############################################################################
    target %in% c(
      "KBF",# Interventions on stomach
      "KBI",# Interventions on duodenum
      "KBK",# Interventions on small intestine, not elsewhere classified
      "KBO",# Interventions on appendix
      "KBP",# Interventions on colon
      "KBZ",# Interventions on large intestine, not elsewhere classified
      "KMA",# Interventions on peritoneum
      "PAK",# Interventions on abdomen, not otherwise specified
      "PAL",# Interventions on abdominal wall, not otherwise specified
      "PAO" # Interventions on abdominal wall, umbilical
      ) &
      means %in% c("AA","AB") ~ "abdominal_surgery",

    target %in% c(
      "PTA",
      "PTB"
      ) &
      action == "LA" &
      means == "AC" ~ "abdominal_surgery",

    x == "KMA.JB.AE" ~ "abdominal_surgery",# Percutaneous drainage of peritoneal cavity
    x == "KZZ.MK.AA" ~ "abdominal_surgery",# Repair of intestine, not elsewhere classified
    x == "PAK.JB.AE" ~ "abdominal_surgery",# Percutaneous abdominal drainage

    # Inguinal hernia surgery
    ############################################################################
    x %in% c(
      "PAM.MK.AA",# Repair of inguinal hernia
      "PAM.MK.AB" # Laparoscopic repair of inguinal hernia
    ) ~ "inguinal_hernia_surgery",

    # Other
    ############################################################################
    x %in% c(
      "BCC.GA.AA",# Destruction of retina
      "BCD.DB.AE",# Injection into vitreous body
      "HDG.LG.AF",# Percutaneous transluminal balloon dilatation of pulmonary valve
      "HIB.DL.AF",# Percutaneous transluminal insertion of device into superior vena cava
      "IBD.DL.AF",# Percutaneous transluminal insertion of device into vein of head and neck
      "IZD.DL.AF",# Insertion of a device into a vein, not elsewhere classified
      "JAN.AE.AC",# Laryngoscopy
      "JAN.MK.AD",# Endoscopic repair of larynx
      "JAM.ML.AD",# Endoscopic reconstruction of nasopharynx
      "JBA.AE.AB",# Tracheoscopy through artificial stoma
      "JBA.KA.AC",# Replacement of tracheal device
      "JBA.LI.AA",# Tracheostomy
      "JBA.MK.AA",# Repair of trachea
      "KAA.AD.AA",# Biopsy of lip
      "KAB.FB.AC",# Lingual fraenotomy
      "LAB.JG.AH",# Debridement of skin and subcutaneous cell tissue of trunk, without incision
      "LAB.LL.AA",# Reduction of skin and subcutaneous cell tissue of trunk
      "LCA.JG.AA",# Debridement of breast with incision
      "NAM.MK.AA",# Repair of urethra
      "NGL.LC.AA",# Orchiopexy
      "NMR.MK.AB",# Endoscopic repair of fetal or embryonic structure
      "NZZ.ZZ.ZZ",# Interventions on the genitourinary system, unspecified
      "PAW.JB.AA" # Drainage of perineum
      ) ~ "other",

    # Not considered as surgery (remove)
    ############################################################################
    x %in% c(
      "ABA.BA.BH",# Magnetic resonance imaging of spinal cord
      "JBB.AE.AD",# Bronchoscopy
      "KBA.LG.AD",# Endoscopic dilatation of oesophagus
      "KBF.DL.AC",# Insertion of device into stomach
      "KBF.KA.AC",# Replacement of gastric device
      "KBK.LD.AH",# Manual reduction of ileostomy prolapse
      "LZZ.DK.AH",# Application of dressing to skin or subcutaneous cell tissue, not elsewhere classified
      "MBO.BA.BC",# Computerised tomography of lumbosacral spine, not elsewhere classified
      "PAB.BA.BH",# Magnetic resonance imaging of head or neck
      "PAE.BA.BH",# Magnetic resonance imaging of thorax
      "PAK.BA.BH",# Magnetic resonance imaging of abdomen
      "PTB.SN.AC",# Management of enterostomy
      "PTA.PM.ZZ",# Gastrostomy education
      "PTC.PM.ZZ",# Tracheostomy education
      "PZA.BA.BH" # Magnetic resonance imaging of whole body
      ) ~ not_surgery,

    # To be categorised (default)
    ############################################################################
    .default = "to_be_categorised"
  ) |>
    factor(
      levels = c(
        "abdominal_surgery",
        "neurosurgery",
        "inguinal_hernia_surgery",
        "cardiac_and_large_vessel_surgery",
        "lung_pleural_space_thoracic_surgery",
        "oesophageal_surgery",
        "other",
        not_surgery,
        "to_be_categorised"))
}

get_procedure_category_pretty <- function(x) {
  dplyr::recode_values(
    as.character(x),
    "overall" ~ gettext("Overall"),
    "abdominal_surgery" ~ gettext("Abdominal surgery"),
    "neurosurgery" ~ gettext("Neurosurgery"),
    "inguinal_hernia_surgery" ~ gettext("Inguinal hernia surgery"),
    "cardiac_and_large_vessel_surgery" ~ gettext("Cardiac- / large vessel surgery"),
    "lung_pleural_space_thoracic_surgery" ~ gettext("Lung- / pleural space- / thoracic surgery"),
    "oesophageal_surgery" ~ gettext("Oesophageal surgery"),
    "other" ~ gettext("Other"),
    "not_surgery" ~ gettext("Not a surgical procedure"),
    "to_be_categorised" ~ gettext("Not yet categorised"),
    default = x
  )
}
