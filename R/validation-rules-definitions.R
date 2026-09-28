# The rules of this file hold a completed infection form to the case
# definition Tracker Capture checks when the form is completed, and without
# which it refuses to complete it: the clinical-sepsis definition on the
# sepsis form, the necrotizing enterocolitis definition, and the surgical
# site infection definition of the depth the form records. A form that does
# not meet its definition reached the dataset around that check: imported
# through the API, completed before the check existed, or changed after
# completion. The rules copy the client's reading of the form, so a missing
# value counts as the client counts it, and they judge only completed forms,
# since the client checks a form only on completion.

# Tracker Capture's reading of an empty value (`processSingleValue()` in the
# legacy rule engine): an empty BOOLEAN or TRUE_ONLY value is the empty
# string, which counts as false, and an empty integer, an option-set code
# among them, is 0.
.as_client_flag <- function(x)
  dplyr::coalesce(x, FALSE)

.as_client_code <- function(x)
  dplyr::coalesce(as.character(x), "0")

# The number of `items` present on each row, `items` being a data frame of
# logical columns, a missing value counting as not present.
.items_present <- function(items)
  as.integer(rowSums(dplyr::mutate(
    items, dplyr::across(tidyselect::everything(), .as_client_flag))))

# The completed events of `event_type` with the columns `items` of their
# form, one row per event. An event without a form row, one whose only stored
# values are infectious agents, reads every item as missing.
.completed_forms <- function(x, event_type, form, items)
  .with_status(x$events, .event_status_levels) |>
    dplyr::filter(.data$event_type_key == event_type &
                  .data$status == "COMPLETED") |>
    dplyr::select("patient_key", "enrollment_key", "event_key") |>
    dplyr::left_join(
      form |>
        dplyr::select("event_key", tidyselect::all_of(items)),
      dplyr::join_by("event_key"))

# The three infectious-agent slots of a form the client's rules read.
.agent_slots <- c(1L, 2L, 3L)

# The findings rows of the primary infectious-agent slots, those outside a
# form's secondary-BSI section. A row may hold only a resistance or name
# companion, its `pathogen_key` missing; whether a slot records an agent is
# the caller's to read from `pathogen_key`.
.primary_slot_rows <- function(findings)
  findings |>
    dplyr::filter(!dplyr::coalesce(.data$secondary_bsi, FALSE) &
                  .data$index %in% .agent_slots)

# ---- Clinical sepsis ------------------------------------------------------

# The sepsis form's clinical signs, each of which the clinical-sepsis
# definition counts as a feature, and its laboratory findings, which it
# counts as one feature together however many are present
# (`NEOIPC_BSI_SET_FIRST_FINDING_COUNTS`,
# `NEOIPC_BSI_SET_COMMON_LAB_FINDINGS_COUNT`).
.bsi_clinical_signs <- c(
  "temperature", "bradycardia", "perfusion", "apnoea",
  "feeding_intolerance", "irritability", "acidosis", "hyperglycaemia")
.bsi_laboratory_findings <- c(
  "wbc", "platelet_count", "crp", "procalcitonin", "it_ratio", "interleukin")

# The sepsis form's items the clinical-sepsis definition reads.
.bsi_clinical_sepsis_items <- c(
  "no_pos_culture", "ab_treatment",
  .bsi_clinical_signs, .bsi_laboratory_findings)

# The clinical-sepsis definition on sepsis forms, as Tracker Capture counts
# it when a form is completed (`NEOIPC_BSI_CLIN_SEPSIS_VR`). Shared by rule 59
# and the reconciliation of a culture-negative sepsis that records infectious
# agents.
#
# Contract: `forms` is a data frame with one row per sepsis form and every
# column of `.bsi_clinical_sepsis_items`, as `sepsisData` carries them. A
# missing value counts as not present, as the client counts an empty value,
# so the missing values a left join gives an event without a form row count
# as nothing present. The result is `forms` with two columns added and none
# changed:
# - `findings`, an integer: the clinical signs present, each counted once,
#   plus one when any laboratory finding is present;
# - `clinical_sepsis`, a logical never missing: whether the form's items meet
#   the definition, that is no positive culture, intravenous antibiotic
#   therapy for five days or more initiated, and at least two findings.
# Whether the form records an infectious agent is not read here; the
# definition applies only to a form that records none, which the caller
# establishes.
.bsi_clinical_sepsis <- function(forms)
  forms |>
    dplyr::mutate(
      findings        =
        .items_present(dplyr::pick(tidyselect::all_of(.bsi_clinical_signs))) +
        as.integer(.items_present(
          dplyr::pick(tidyselect::all_of(.bsi_laboratory_findings))) > 0L),
      clinical_sepsis = .as_client_flag(.data$no_pos_culture) &
                        .as_client_flag(.data$ab_treatment) &
                        .data$findings >= 2L)

# Find completed sepsis forms that record no infectious agent in a primary
# slot and do not meet the clinical-sepsis definition. Without an agent the
# form describes a clinical sepsis, which needs the culture recorded as
# negative, the antibiotic therapy recorded as initiated, and at least two
# findings. The client refuses completion only where the culture is recorded
# as negative; a form with neither a negative culture nor an agent it refuses
# through the compulsory first agent, so this rule flags that form as well.
# An agent is a slot that names a concept, "Not listed" (code 0) included; a
# resistance or name companion stored on its own records none.
validation_rule_59 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(.bsi_clinical_sepsis_items %in% names(x$sepsisData)) ||
      !all(c("secondary_bsi", "index", "pathogen_key") %in%
           names(x$infectiousAgentFindings)))
    return(.rule_skipped(
      59L, "the sepsis form's culture result, antibiotic therapy, clinical signs and laboratory findings, and the findings' slot, secondary-BSI flag and infectious agent"))

  with_agent <- .primary_slot_rows(x$infectiousAgentFindings) |>
    dplyr::filter(!is.na(.data$pathogen_key)) |>
    dplyr::select("event_key") |>
    dplyr::distinct()

  .completed_forms(x, "bsi", x$sepsisData, .bsi_clinical_sepsis_items) |>
    dplyr::anti_join(with_agent, dplyr::join_by("event_key")) |>
    .bsi_clinical_sepsis() |>
    dplyr::filter(!.data$clinical_sepsis) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 59L),
      dplyr::join_by("event_key")) |>
    dplyr::select("patient_key", "enrollment_key", "event_key", "findings") |>
    tidyr::nest(context = "findings") |>
    dplyr::mutate(
      rule_id        = 59L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# ---- Necrotizing enterocolitis --------------------------------------------

# The NEC form's findings by the group the definition counts them in
# (`NEOIPC_NEC_SET_FINDING_COUNTS`).
.nec_imaging_findings <- c(
  "fixed_loop", "pneumatosis_intestinalis_img", "pneumoperitoneum",
  "portal_venous_gas")
.nec_clinical_findings <- c(
  "abdominal_distension", "abdominal_skin_tone", "bloody_stools",
  "bilious_aspirate", "gastric_residuals", "vomiting")
.nec_surgical_findings <- c(
  "bowel_necrosis", "pneumatosis_intestinalis_surg")

# Find completed necrotizing enterocolitis forms that meet neither the
# definition by imaging and clinical findings, at least one of each, nor the
# definition by surgical findings, at least one (`NEOIPC_NEC_VR`).
validation_rule_60 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  items <- c(.nec_imaging_findings, .nec_clinical_findings,
             .nec_surgical_findings)
  if (!all(items %in% names(x$necData)))
    return(.rule_skipped(
      60L, "the NEC form's imaging, clinical and surgical findings"))

  .completed_forms(x, "nec", x$necData, items) |>
    dplyr::mutate(
      imaging_count  = .items_present(dplyr::pick(tidyselect::all_of(.nec_imaging_findings))),
      clinical_count = .items_present(dplyr::pick(tidyselect::all_of(.nec_clinical_findings))),
      surgical_count = .items_present(dplyr::pick(tidyselect::all_of(.nec_surgical_findings)))) |>
    dplyr::filter(
      !(.data$imaging_count >= 1L & .data$clinical_count >= 1L) &
      .data$surgical_count < 1L) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 60L),
      dplyr::join_by("event_key")) |>
    dplyr::select("patient_key", "enrollment_key", "event_key",
                  "imaging_count", "clinical_count", "surgical_count") |>
    tidyr::nest(context = c("imaging_count", "clinical_count", "surgical_count")) |>
    dplyr::mutate(
      rule_id        = 60L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}

# ---- Surgical site infection ----------------------------------------------

# The SSI form's items the three depths' definitions read. The
# `organisms_*` items are codes of the yes/no/not-tested option set:
# 1 infectious agents identified, 0 none identified, -1 not tested.
.ssi_definition_items <- c(
  "infection_type",
  "purulent_drainage_superf", "organisms_superf", "physician_diag_superf",
  "inc_opened_superf", "localized_pain_superf", "localized_swelling",
  "localized_erythema", "localized_heat",
  "purulent_drainage_deep", "abscess_deep", "inc_dehisces_deep",
  "organisms_deep", "fever", "localized_pain_deep",
  "purulent_drainage_drain", "organisms_organ", "abscess_organ")

# Find completed surgical site infection forms that do not meet the
# definition of the depth they record (`NEOIPC_SSI_SUPERFICIAL_INCISIONAL_VR`,
# `NEOIPC_SSI_DEEP_INCISIONAL_VR`, `NEOIPC_SSI_ORGAN_SPACE_VR`). A superficial
# incisional infection needs purulent drainage from the incision, agents
# identified from it, a physician's diagnosis, or the incision opened, not
# tested, and pain, swelling, erythema or heat; a deep incisional infection
# needs purulent drainage from the deep incision, an abscess, or the incision
# dehiscing, with agents identified or not tested, and fever or pain; an
# organ/space infection needs purulent drainage from a drain, agents
# identified from the organ or space, or an abscess. A missing `organisms_*`
# item reads as none identified, as the client reads an empty code. A form that
# records no depth is judged against none, as the client judges it; the depth
# is a compulsory value of the form, which this rule does not check.
validation_rule_61 <- function(x, exceptions)
{
  check_neoipcr_ds(x)
  if (!all(.ssi_definition_items %in% names(x$ssiData)))
    return(.rule_skipped(
      61L, "the SSI form's infection type and the findings of its three depths"))

  .completed_forms(x, "ssi", x$ssiData, .ssi_definition_items) |>
    dplyr::mutate(
      organisms_superf = .as_client_code(.data$organisms_superf),
      organisms_deep   = .as_client_code(.data$organisms_deep),
      organisms_organ  = .as_client_code(.data$organisms_organ),
      superficial_met  =
        .as_client_flag(.data$purulent_drainage_superf) |
        .data$organisms_superf == "1" |
        .as_client_flag(.data$physician_diag_superf) |
        (.as_client_flag(.data$inc_opened_superf) &
           .data$organisms_superf == "-1" &
           (.as_client_flag(.data$localized_pain_superf) |
              .as_client_flag(.data$localized_swelling) |
              .as_client_flag(.data$localized_erythema) |
              .as_client_flag(.data$localized_heat))),
      deep_met         =
        .as_client_flag(.data$purulent_drainage_deep) |
        .as_client_flag(.data$abscess_deep) |
        (.as_client_flag(.data$inc_dehisces_deep) &
           .data$organisms_deep != "0" &
           (.as_client_flag(.data$fever) |
              .as_client_flag(.data$localized_pain_deep))),
      organ_met        =
        .as_client_flag(.data$purulent_drainage_drain) |
        .data$organisms_organ == "1" |
        .as_client_flag(.data$abscess_organ)) |>
    dplyr::filter(dplyr::case_when(
      .data$infection_type == "1" ~ !.data$superficial_met,
      .data$infection_type == "2" ~ !.data$deep_met,
      .data$infection_type == "3" ~ !.data$organ_met,
      .default = FALSE)) |>
    dplyr::anti_join(
      .rule_exceptions(exceptions, 61L),
      dplyr::join_by("event_key")) |>
    dplyr::select("patient_key", "enrollment_key", "event_key",
                  "infection_type") |>
    tidyr::nest(context = "infection_type") |>
    dplyr::mutate(
      rule_id        = 61L,
      patient_key    = .data$patient_key,
      enrollment_key = .data$enrollment_key,
      event_key      = .data$event_key,
      context        = .data$context,
      .keep = "none")
}
