# Fixture builders for reader-level integration tests.
#
# These helpers construct raw DHIS2-shaped data that the readers
# (read_event_data, read_substance_days, read_infectious_agent_findings,
# read_events, read_patients, read_enrollments) can process. They
# complement helper-fixtures.R (which builds post-reader output) by
# testing the reader pipeline itself — particularly the pivot-volatility
# hazard, absent-column materialization, and type-drift scenarios that
# only surface when the raw input is sparse.
#
# Auto-loaded by testthat 3.x before test files run.

# ---- DHIS2 DE code mapping --------------------------------------------------

# Stage prefix for each event type's DHIS2 data element codes.
.stage_prefix <- c(
  adm = "ADMISSION",
  end = "SURVEILLANCE_END",
  bsi = "BSI",

  nec = "NEC",
  hap = "HAP",
  pro = "SURGERY",
  ssi = "SSI"
)

# Pre-rename field names: the reader renames these after extracting the
# DE suffix. Schema uses the post-rename name; DHIS2 uses the pre-rename.
.pre_rename <- list(
  nec = c(sec_bsi = "SECONDARY_BSI"),
  hap = c(dev_ass = "DEVICE_ASSOCIATION", sec_bsi = "SECONDARY_BSI")
)

# Derived columns that are NOT DHIS2 data elements (computed post-pivot).
.derived_cols <- c("vs_days")

# ValueType for each schema field name. Fields with option sets are TEXT.
.value_types <- list(
  # Admission
  type = "TEXT",      # option set
  dol  = "INTEGER_POSITIVE",
  # Surveillance-end
  reason             = "TEXT",  # option set
  patient_days       = "INTEGER_ZERO_OR_POSITIVE",
  cvc_days           = "INTEGER_ZERO_OR_POSITIVE",
  pvc_days           = "INTEGER_ZERO_OR_POSITIVE",
  inv_days           = "INTEGER_ZERO_OR_POSITIVE",
  niv_days           = "INTEGER_ZERO_OR_POSITIVE",
  ab_days            = "INTEGER_ZERO_OR_POSITIVE",
  human_milk_days    = "INTEGER_ZERO_OR_POSITIVE",
  kangaroo_care_days = "INTEGER_ZERO_OR_POSITIVE",
  probiotic_days     = "INTEGER_ZERO_OR_POSITIVE",
  # BSI
  dev_ass            = "TEXT",  # option set (BSI / HAP both)
  los                = "INTEGER_POSITIVE",
  acidosis           = "BOOLEAN",
  ab_treatment       = "TRUE_ONLY",
  apnoea             = "BOOLEAN",
  bradycardia        = "BOOLEAN",
  crp                = "BOOLEAN",
  feeding_intolerance = "BOOLEAN",
  hyperglycaemia     = "BOOLEAN",
  it_ratio           = "BOOLEAN",
  interleukin        = "BOOLEAN",
  irritability       = "BOOLEAN",
  no_pos_culture     = "TRUE_ONLY",
  perfusion          = "BOOLEAN",
  platelet_count     = "BOOLEAN",
  procalcitonin      = "BOOLEAN",
  temperature        = "BOOLEAN",
  wbc                = "BOOLEAN",
  # NEC
  sec_bsi            = "TEXT",  # option set (NEC / HAP / SSI)
  abdominal_skin_tone         = "BOOLEAN",
  abdominal_distension        = "BOOLEAN",
  bilious_aspirate            = "BOOLEAN",
  bloody_stools               = "BOOLEAN",
  bowel_necrosis              = "BOOLEAN",
  fixed_loop                  = "BOOLEAN",
  gastric_residuals           = "BOOLEAN",
  pneumatosis_intestinalis_img = "BOOLEAN",
  pneumatosis_intestinalis_surg = "BOOLEAN",
  pneumoperitoneum            = "BOOLEAN",
  portal_venous_gas           = "BOOLEAN",
  vomiting                    = "BOOLEAN",
  # HAP
  microbiological_test_result = "TEXT",  # option set
  fever                       = "BOOLEAN",
  imaging_findings            = "BOOLEAN",
  increased_respiratory_secretion = "BOOLEAN",
  laboratory_findings         = "BOOLEAN",
  purulent_tracheal_aspirate  = "BOOLEAN",
  respiratory_distress        = "BOOLEAN",
  respiratory_support         = "BOOLEAN",
  tachypnoea                  = "BOOLEAN",
  # Surgery
  procedure_description = "TEXT",
  main_procedure_code   = "TEXT",
  side_procedure_code_1 = "TEXT",
  side_procedure_code_2 = "TEXT",
  asa_score             = "TEXT",     # option set
  wound_class           = "TEXT",     # option set
  duration              = "INTEGER_POSITIVE",
  emergency_procedure   = "BOOLEAN",
  endoscopic_procedure  = "BOOLEAN",
  implant               = "BOOLEAN",
  infection_signs       = "TEXT",
  primary_closure       = "BOOLEAN",
  revision_procedure    = "BOOLEAN",
  # SSI
  infection_type       = "TEXT",     # option set
  organisms_superf     = "TEXT",     # option set
  organisms_deep       = "TEXT",     # option set
  organisms_organ      = "TEXT"      # option set
  # (SSI boolean flags reuse names already declared above: fever,
  # abscess_deep, abscess_organ, etc.)
)

# SSI-specific boolean DEs not already in the global list.
.value_types$abscess_deep             <- "BOOLEAN"
.value_types$abscess_organ            <- "BOOLEAN"
.value_types$inc_dehisces_deep        <- "BOOLEAN"
.value_types$inc_opened_superf        <- "BOOLEAN"
.value_types$infection_present        <- "BOOLEAN"
.value_types$localized_erythema       <- "BOOLEAN"
.value_types$localized_heat           <- "BOOLEAN"
.value_types$localized_pain_deep      <- "BOOLEAN"
.value_types$localized_pain_superf    <- "BOOLEAN"
.value_types$localized_swelling       <- "BOOLEAN"
.value_types$physician_diag_superf    <- "BOOLEAN"
.value_types$purulent_drainage_deep   <- "BOOLEAN"
.value_types$purulent_drainage_drain  <- "BOOLEAN"
.value_types$purulent_drainage_superf <- "BOOLEAN"

# Option set mapping: schema field name -> optionSet code (matching
# .event_data_levels in R/schema-event-data.R).
.option_set_map <- list(
  type                        = "ADMISSION_TYPE",
  reason                      = "SURV_END_REASON",
  dev_ass                     = "BSI_DEV_ASS",  # shared BSI/HAP
  asa_score                   = "ASA_SCORE",
  wound_class                 = "WOUND_CLASS",
  infection_type              = "SSI_INFECTION_TYPE",
  sec_bsi                     = "YES_NO_NO_FOLLOWUP",
  microbiological_test_result = "YES_NO_NOT_TESTED",
  organisms_superf            = "YES_NO_NOT_TESTED",
  organisms_deep              = "YES_NO_NOT_TESTED",
  organisms_organ             = "YES_NO_NOT_TESTED"
)

# ---- Helper functions --------------------------------------------------------

# Convert a schema field name to a DHIS2 code for a given event type.
# Returns the full DHIS2 code like "NEOIPC_ADMISSION_TYPE".
.to_dhis2_code <- function(event_type_key, field_name) {
  prefix <- .stage_prefix[[event_type_key]]
  # Reverse any reader renames.
  renames <- .pre_rename[[event_type_key]]
  dhis2_suffix <- field_name
  if (!is.null(renames) && field_name %in% names(renames))
    dhis2_suffix <- renames[[field_name]]
  paste0("NEOIPC_", prefix, "_", toupper(dhis2_suffix))
}

# Generate a stable fake UID for a data element given its DHIS2 code.
.de_uid <- function(dhis2_code) paste0("DE_", dhis2_code)


# ---- Build raw events --------------------------------------------------------

#' Build a raw events tibble mimicking the DHIS2 API response.
#'
#' Each event gets a nested `dataValues` list-column with entries for the
#' data elements specified in `rows`. Events not supplying a given DE
#' will not have that DE in their `dataValues` — this is exactly the
#' sparsity pattern that causes pivot-volatility.
#'
#' @param event_keys Integer vector — one per event.
#' @param event_type_key Character scalar — e.g. "adm", "end", "bsi".
#' @param rows A list of named lists. Each inner list maps schema field
#'   names to raw string values. E.g. `list(list(type = "1", dol = "5"))`.
#'   Length must match `event_keys`. Use `list()` (empty) for an event
#'   with no dataValues.
#' @return A tibble with columns `event`, `dataValues`.
build_raw_events <- function(event_keys, event_type_key, rows) {
  stopifnot(
    length(event_keys) == length(rows),
    event_type_key %in% names(.stage_prefix))

  data_values <- purrr::map(rows, function(row) {
    purrr::map(names(row), function(field) {
      dhis2_code <- .to_dhis2_code(event_type_key, field)
      list(
        dataElement = .de_uid(dhis2_code),
        value       = as.character(row[[field]])
      )
    })
  })

  tibble::tibble(
    event      = paste0("EVT_", event_keys),
    dataValues = data_values
  )
}


#' Build processed events — the output of read_events() minus raw columns.
#'
#' @param event_keys Integer vector.
#' @param event_type_key Character scalar — recycled to match length.
#' @return A tibble with `event`, `event_key`, `event_type_key`.
build_processed_events <- function(event_keys, event_type_key) {
  tibble::tibble(
    event          = paste0("EVT_", event_keys),
    event_key      = event_keys,
    event_type_key = factor(
      rep(event_type_key, length(event_keys)),
      levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))
  )
}


#' Build minimal metadata for read_event_data / read_substance_days /
#' read_infectious_agent_findings.
#'
#' @param event_type_key Character scalar (or vector for multi-type).
#' @param substance_count Integer scalar — number of antibiotic-substance slots to
#'   materialise (DEs `NEOIPC_SURVEILLANCE_END_AB_SUBST_01..NN` + `_DAYS`). Defaults to 5L.
#' @return A list with `$dataElements`, `$options`, `$.users_internal_map`.
build_reader_metadata <- function(event_type_key = c("adm", "end", "bsi",
                                                      "nec", "hap", "pro",
                                                      "ssi"),
                                  substance_count = 5L) {
  opts <- neoipcr::dhis2_dataset_options(
    include_event     = "full",
    include_user      = "no",
    include_timestamps = FALSE)

  # Build dataElements for all requested event types.
  de_rows <- list()
  for (etk in event_type_key) {
    cols <- neoipcr:::event_data_cols_for(etk)
    all_names <- names(neoipcr:::compile_schema(cols, opts))
    non_de    <- c("event_key", "enrollment_key", "patient_key",
                   "department_key", "hospital_key", "country_key",
                   "world_bank_class_key", "isTest", "vs_days")
    companion <- grepl(
      "_(storedBy|createdBy|updatedBy|createdAt|updatedAt)$", all_names)
    de_codes  <- setdiff(all_names[!companion], non_de)

    for (field in de_codes) {
      dhis2_code <- .to_dhis2_code(etk, field)
      vt <- .value_types[[field]]
      if (is.null(vt)) vt <- "TEXT"
      os <- .option_set_map[[field]]
      if (is.null(os)) os <- NA_character_
      de_rows[[length(de_rows) + 1L]] <- list(
        dataElement = .de_uid(dhis2_code),
        code        = dhis2_code,
        valueType   = vt,
        optionSet   = os
      )
    }
  }

  # Add substance-days DEs (always associated with surveillance-end).
  if ("end" %in% event_type_key) {
    for (i in seq_len(substance_count)) {
      subst_code <- sprintf("NEOIPC_SURVEILLANCE_END_AB_SUBST_%02d", i)
      days_code  <- paste0(subst_code, "_DAYS")
      de_rows[[length(de_rows) + 1L]] <- list(
        dataElement = .de_uid(subst_code),
        code        = subst_code,
        valueType   = "TEXT",
        optionSet   = NA_character_)
      de_rows[[length(de_rows) + 1L]] <- list(
        dataElement = .de_uid(days_code),
        code        = days_code,
        valueType   = "INTEGER_ZERO_OR_POSITIVE",
        optionSet   = NA_character_)
    }
  }

  # Add pathogen DEs (for bsi/nec/hap/ssi).
  pathogen_types <- intersect(event_type_key, c("bsi", "nec", "hap", "ssi"))
  for (etk in pathogen_types) {
    prefix <- .stage_prefix[[etk]]
    for (idx in 1:3) {
      base <- sprintf("NEOIPC_%s_PATHOGEN_%d", prefix, idx)
      suffixes <- c("", "_3GCR", "_CAR", "_COR", "_MRSA", "_VRE", "_NAME")
      if (etk == "bsi")
        suffixes <- c(suffixes, "_SOURCE", "_MULTIPLE")
      else if (etk == "hap")
        suffixes <- c(suffixes, "_SOURCE")
      # Secondary BSI pathogens for NEC/HAP/SSI.
      if (etk %in% c("nec", "hap", "ssi")) {
        sec_base <- sprintf("NEOIPC_%s_SEC_BSI_PATHOGEN_%d", prefix, idx)
        for (sfx in c("", "_3GCR", "_CAR", "_COR", "_MRSA", "_VRE", "_NAME")) {
          de_rows[[length(de_rows) + 1L]] <- list(
            dataElement = .de_uid(paste0(sec_base, sfx)),
            code        = paste0(sec_base, sfx),
            valueType   = "TEXT",
            optionSet   = NA_character_)
        }
      }
      for (sfx in suffixes) {
        de_rows[[length(de_rows) + 1L]] <- list(
          dataElement = .de_uid(paste0(base, sfx)),
          code        = paste0(base, sfx),
          valueType   = "TEXT",
          optionSet   = NA_character_)
      }
    }
  }

  data_elements <- tibble::tibble(
    dataElement = purrr::map_chr(de_rows, "dataElement"),
    code        = purrr::map_chr(de_rows, "code"),
    valueType   = purrr::map_chr(de_rows, "valueType"),
    optionSet   = purrr::map_chr(de_rows, "optionSet")
  )

  # Build options table from .event_data_levels.
  levels_data <- neoipcr:::.event_data_levels
  options_rows <- list()
  for (os_name in names(levels_data)) {
    lvls <- levels_data[[os_name]]
    for (i in seq_along(lvls)) {
      options_rows[[length(options_rows) + 1L]] <- list(
        optionSet_code = os_name,
        sortOrder      = i,
        code           = lvls[[i]]
      )
    }
  }
  options_tbl <- tibble::tibble(
    optionSet_code = purrr::map_chr(options_rows, "optionSet_code"),
    sortOrder      = purrr::map_int(options_rows, "sortOrder"),
    code           = purrr::map_chr(options_rows, "code")
  )

  # Users internal map — minimal, just enough for user-key substitution.
  users_map <- tibble::tibble(
    user_key = 1L,
    user     = "UID_admin",
    username = "admin"
  )

  list(
    dataElements         = data_elements,
    options              = options_tbl,
    .users_internal_map  = users_map
  )
}


# ---- Substance-days raw events -----------------------------------------------

#' Build raw events containing substance-day dataValues.
#'
#' @param event_keys Integer vector — one per surveillance-end event.
#' @param substance_rows A list of lists. Each inner list maps a 1-based slot
#'   index (1-99, zero-padded to two digits in the emitted DE code) to a list
#'   with `substance_code` and optionally `days`.
#'   E.g. `list(list("1" = list(substance_code = "J01CA04", days = "3")))`.
#'   Use `list()` for an event with no substance DEs.
build_raw_substance_events <- function(event_keys, substance_rows) {
  stopifnot(length(event_keys) == length(substance_rows))

  data_values <- purrr::map(substance_rows, function(row) {
    dvs <- list()
    for (idx_str in names(row)) {
      entry <- row[[idx_str]]
      subst_code <- sprintf("NEOIPC_SURVEILLANCE_END_AB_SUBST_%02d", as.integer(idx_str))
      if (!is.null(entry$substance_code)) {
        dvs[[length(dvs) + 1L]] <- list(
          dataElement = .de_uid(subst_code),
          value       = entry$substance_code)
      }
      if (!is.null(entry$days)) {
        days_code <- paste0(subst_code, "_DAYS")
        dvs[[length(dvs) + 1L]] <- list(
          dataElement = .de_uid(days_code),
          value       = as.character(entry$days))
      }
    }
    dvs
  })

  tibble::tibble(
    event      = paste0("EVT_", event_keys),
    dataValues = data_values
  )
}


# ---- Pathogen findings raw events --------------------------------------------

#' Build raw events containing pathogen dataValues.
#'
#' @param event_keys Integer vector.
#' @param event_type_keys Character vector (same length as event_keys).
#' @param pathogen_rows A list of lists. Each inner list maps pathogen
#'   index to a list of suffix→value. E.g.
#'   `list(list("1" = list(pathogen = "42", "3gcr" = "0")))`.
build_raw_pathogen_events <- function(event_keys, event_type_keys,
                                      pathogen_rows) {
  stopifnot(
    length(event_keys) == length(pathogen_rows),
    length(event_keys) == length(event_type_keys))

  data_values <- purrr::map2(pathogen_rows, event_type_keys, function(row, etk) {
    prefix <- .stage_prefix[[etk]]
    dvs <- list()
    for (idx_str in names(row)) {
      entry <- row[[idx_str]]
      for (sfx in names(entry)) {
        if (sfx == "pathogen") {
          de_code <- sprintf("NEOIPC_%s_PATHOGEN_%s", prefix, idx_str)
        } else {
          de_code <- sprintf("NEOIPC_%s_PATHOGEN_%s_%s",
                             prefix, idx_str, toupper(sfx))
        }
        dvs[[length(dvs) + 1L]] <- list(
          dataElement = .de_uid(de_code),
          value       = as.character(entry[[sfx]]))
      }
    }
    dvs
  })

  tibble::tibble(
    event      = paste0("EVT_", event_keys),
    dataValues = data_values
  )
}
