# Test fixtures and builders for neoipcr tests.
# Auto-loaded by testthat 3.x before test files run.

# Valid values for the `exclude` parameter of read_test_metadata()
.valid_exclusions <- c(
  "system", "program", "program_id", "program_stages",
  "stage_data_elements", "tracked_entity_attributes", "tracked_entity_type",
  "countries", "test_units", "antimicrobials"
)

# Valid values for the `include` parameter of read_test_metadata(): fixtures
# merged only on request, so the baseline metadata graph stays what the
# existing tests assume.
.valid_inclusions <- c("org_unit_attributes", "duplicate_tracked_entity_type")

#' Read static JSON fixtures and return processed metadata.
#'
#' Loads fixture files from tests/testthat/fixtures/, merges them into a single
#' metadata list, and passes the result through read_metadata().
#'
#' @param exclude Character vector of components to omit. See .valid_exclusions
#'   for allowed values.
#' @param dataset_options A neoipcr_dhis2_dsopt object (default: all defaults).
#' @param include Character vector of optional components to merge in. See
#'   .valid_inclusions for allowed values.
#' @return A neoipcr_metadata object.
read_test_metadata <- function(
    exclude = character(),
    dataset_options = dhis2_dataset_options(),
    include = character())
{
  bad <- setdiff(exclude, .valid_exclusions)
  if (length(bad) > 0L)
    rlang::abort(c(
      paste0("Unknown exclusion(s): ", paste(bad, collapse = ", ")),
      "i" = paste0("Valid values: ", paste(.valid_exclusions, collapse = ", "))))
  bad <- setdiff(include, .valid_inclusions)
  if (length(bad) > 0L)
    rlang::abort(c(
      paste0("Unknown inclusion(s): ", paste(bad, collapse = ", ")),
      "i" = paste0("Valid values: ", paste(.valid_inclusions, collapse = ", "))))

  fixture_path <- testthat::test_path("fixtures")
  read_fixture <- function(name) {
    jsonlite::fromJSON(
      file.path(fixture_path, name),
      simplifyVector = FALSE)
  }

  metadata <- list()

  # --- system ---
  if (!("system" %in% exclude))
    metadata <- utils::modifyList(metadata, read_fixture("system.json"))

  # --- program ---
  if (!("program" %in% exclude)) {
    prog <- read_fixture("program.json")

    if ("program_id" %in% exclude)
      prog$programs[[1L]]$id <- NULL

    if ("program_stages" %in% exclude) {
      prog$programs[[1L]]$programStages <- NULL
    } else if ("stage_data_elements" %in% exclude) {
      prog$programs[[1L]]$programStages <- lapply(
        prog$programs[[1L]]$programStages,
        function(s) { s$programStageDataElements <- NULL; s })
    }

    if ("tracked_entity_attributes" %in% exclude)
      prog$programs[[1L]]$programTrackedEntityAttributes <- NULL

    if ("tracked_entity_type" %in% exclude)
      prog$trackedEntityTypes <- NULL

    if ("duplicate_tracked_entity_type" %in% include)
      prog$trackedEntityTypes <- c(
        prog$trackedEntityTypes, list(list(id = "TET_OTHER")))

    metadata <- utils::modifyList(metadata, prog)
  }

  # --- org units ---
  if (!("countries" %in% exclude && "test_units" %in% exclude)) {
    ou <- read_fixture("org-units.json")

    if ("countries" %in% exclude)
      ou$organisationUnitGroups <- Filter(
        function(g) g$code != "COUNTRY",
        ou$organisationUnitGroups)

    if ("test_units" %in% exclude)
      ou$organisationUnitGroups <- Filter(
        function(g) g$code != "TEST_UNITS",
        ou$organisationUnitGroups)

    metadata <- utils::modifyList(metadata, ou)
  }

  # --- org-unit custom-attribute definitions (opt-in) ---
  if ("org_unit_attributes" %in% include)
    metadata <- utils::modifyList(
      metadata, read_fixture("org-unit-attributes.json"))

  # --- antimicrobials (options + optionGroupSets) ---
  if (!("antimicrobials" %in% exclude)) {
    am <- read_fixture("antimicrobials.json")
    metadata$options <- c(metadata$options, am$options)
    metadata$optionGroupSets <- c(metadata$optionGroupSets, am$optionGroupSets)
  }

  read_metadata(metadata, dataset_options)
}


# ---------------------------------------------------------------------------
# Per-table builders
# ---------------------------------------------------------------------------
# Each produces a tibble with correct column names, types, and S3 class.
# `n` controls row count; keys are 1:n.  `...` overrides any column.

make_test_patients <- function(
    n                     = 3,
    include_patient       = "full",
    patient_columns       = c("id", "sex", "birth_weight", "gestational_age"),
    include_dhis2_ids     = "patients",
    include_user          = "no",
    include_timestamps    = FALSE,
    include_test_data     = FALSE,
    include_deleted       = FALSE,
    include_department    = "pseudo",
    include_hospital      = "pseudo",
    include_country       = "pseudo",
    include_world_bank_class = "pseudo",
    ...) {
  opts <- dhis2_dataset_options(
    include_patient          = include_patient,
    patient_columns          = patient_columns,
    include_dhis2_ids        = include_dhis2_ids,
    include_user             = include_user,
    include_timestamps       = include_timestamps,
    include_test_data        = include_test_data,
    include_deleted          = include_deleted,
    include_department       = include_department,
    include_hospital         = include_hospital,
    include_country          = include_country,
    include_world_bank_class = include_world_bank_class)
  schema <- neoipcr:::compile_schema(neoipcr:::patients_cols, opts)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_pat", class(schema))))

  # Deterministic per-patient values for any column that might be in
  # the schema. `compile_schema` subset + factor coercion below align
  # whatever set is actually declared with the declared column types.
  keys <- seq_len(n)
  full <- list(
    patient_key          = keys,
    trackedEntity        = paste0("TE_", keys),
    patient_id           = paste0("PAT_", keys),
    sex                  = factor(rep("M", n), levels = c("F", "M", "U")),
    birth_weight         = rep(1500L, n),
    gest_age             = rep("30+0", n),
    total_gestation_days = rep(210L, n),
    delivery_mode        = factor(rep("1", n), levels = c("1", "2", "3")),
    multiple_birth       = rep(FALSE, n),
    siblings             = rep(1L, n),
    inactive             = rep(FALSE, n),
    potentialDuplicate   = rep(FALSE, n),
    storedBy             = rep(1L, n),
    createdBy            = rep(1L, n),
    updatedBy            = rep(1L, n),
    createdAt            = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    createdAtClient      = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    updatedAt            = as.POSIXct("2024-01-02", tz = "UTC") + keys,
    updatedAtClient      = as.POSIXct("2024-01-02", tz = "UTC") + keys,
    deleted              = rep(FALSE, n),
    department_key       = keys,
    hospital_key         = keys,
    country_key          = keys,
    world_bank_class_key = keys,
    isTest               = rep(FALSE, n))

  # Per-TEA companion columns — exist only when the corresponding base
  # attribute AND the relevant include_user/include_timestamps gates
  # are open. Populate them for every attribute we know about; the
  # schema's include_when predicates drop whatever isn't asked for.
  trackable_attrs <- c(
    "patient_id", "sex", "birth_weight", "gest_age",
    "total_gestation_days", "delivery_mode", "multiple_birth", "siblings")
  for (attr in trackable_attrs) {
    full[[paste0(attr, "_storedBy")]]  <- rep(1L, n)
    full[[paste0(attr, "_createdAt")]] <- as.POSIXct("2024-01-01", tz = "UTC") + keys
    full[[paste0(attr, "_updatedAt")]] <- as.POSIXct("2024-01-02", tz = "UTC") + keys
  }

  # Apply caller overrides, then subset to the schema's declared
  # columns in declared order. `...` lets tests pin specific values.
  full <- utils::modifyList(full, list(...))
  d <- tibble::as_tibble(full[names(schema)])
  # Re-apply factor levels declared by the schema so the tibble exactly
  # matches `assert_schema`'s expectations.
  for (col in names(schema)) {
    if (is.factor(schema[[col]]) && !is.factor(d[[col]]))
      d[[col]] <- factor(d[[col]], levels = levels(schema[[col]]))
  }
  structure(d, class = c("neoipcr_pat", class(d)))
}

make_test_enrollments <- function(
    n = 3,
    patient_keys         = seq_len(n),
    include_enrollment   = "full",
    include_patient      = "full",
    include_dhis2_ids    = "enrollments",
    include_incomplete   = "enrollments",
    include_user         = "no",
    include_timestamps   = FALSE,
    include_test_data    = FALSE,
    include_deleted      = FALSE,
    include_department   = "pseudo",
    include_hospital     = "pseudo",
    include_country      = "pseudo",
    include_world_bank_class = "pseudo",
    ...) {
  opts <- dhis2_dataset_options(
    include_enrollment       = include_enrollment,
    include_patient          = include_patient,
    include_dhis2_ids        = include_dhis2_ids,
    include_incomplete       = include_incomplete,
    include_user             = include_user,
    include_timestamps       = include_timestamps,
    include_test_data        = include_test_data,
    include_deleted          = include_deleted,
    include_department       = include_department,
    include_hospital         = include_hospital,
    include_country          = include_country,
    include_world_bank_class = include_world_bank_class)
  schema <- neoipcr:::compile_schema(neoipcr:::enrollments_cols, opts)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_enr", class(schema))))

  keys <- seq_len(n)
  full <- list(
    enrollment_key       = keys,
    enrollment           = paste0("ENR_", keys),
    patient_key          = patient_keys[keys],
    enrolledAt           = as.Date("2024-01-01") + keys - 1L,
    followUp             = rep(FALSE, n),
    status               = factor(rep("COMPLETED", n),
                                  levels = c("ACTIVE","COMPLETED","CANCELLED")),
    createdBy            = rep(1L, n),
    updatedBy            = rep(1L, n),
    completedBy          = rep(1L, n),
    storedBy             = rep(1L, n),
    occurredAt           = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    createdAt            = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    createdAtClient      = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    updatedAt            = as.POSIXct("2024-01-02", tz = "UTC") + keys,
    updatedAtClient      = as.POSIXct("2024-01-02", tz = "UTC") + keys,
    completedAt          = as.POSIXct("2024-01-03", tz = "UTC") + keys,
    deleted              = rep(FALSE, n),
    department_key       = keys,
    hospital_key         = keys,
    country_key          = keys,
    world_bank_class_key = keys,
    isTest               = rep(FALSE, n))
  full <- utils::modifyList(full, list(...))
  d <- tibble::as_tibble(full[names(schema)])
  for (col in names(schema)) {
    if (is.factor(schema[[col]]) && !is.factor(d[[col]]))
      d[[col]] <- factor(d[[col]], levels = levels(schema[[col]]))
  }
  structure(d, class = c("neoipcr_enr", class(d)))
}

make_test_events <- function(
    n = 5,
    enrollment_keys = rep(1L, n),
    patient_keys    = rep(1L, n),
    event_type_keys = rep("adm", n),
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_dhis2_ids        = "events",
    include_incomplete       = "events",
    include_test_data        = FALSE,
    include_user             = "no",
    include_timestamps       = FALSE,
    include_deleted          = FALSE,
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo",
    ...) {
  opts <- dhis2_dataset_options(
    include_event            = include_event,
    include_enrollment       = include_enrollment,
    include_patient          = include_patient,
    include_dhis2_ids        = include_dhis2_ids,
    include_incomplete       = include_incomplete,
    include_test_data        = include_test_data,
    include_user             = include_user,
    include_timestamps       = include_timestamps,
    include_deleted          = include_deleted,
    include_department       = include_department,
    include_hospital         = include_hospital,
    include_country          = include_country,
    include_world_bank_class = include_world_bank_class)
  schema <- neoipcr:::compile_schema(neoipcr:::events_cols, opts)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_evt", class(schema))))

  keys <- seq_len(n)
  base_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  full <- list(
    event_key            = keys,
    event                = paste0("EVT_", keys),
    occurredAt           = as.Date("2024-01-01") + keys - 1L,
    status               = factor(rep("COMPLETED", n),
                                  levels = c("ACTIVE", "COMPLETED", "VISITED",
                                             "SCHEDULE", "OVERDUE", "SKIPPED")),
    event_type_key       = factor(event_type_keys[keys],
                                  levels = c("adm", "pro", "bsi", "nec",
                                             "ssi", "hap", "end")),
    enrollment_key       = enrollment_keys[keys],
    patient_key          = patient_keys[keys],
    isTest               = rep(FALSE, n),
    # Entity-level user fields (phase-b-event-details).
    storedBy             = rep(1L, n),
    createdBy            = rep(1L, n),
    updatedBy            = rep(1L, n),
    completedBy          = rep(1L, n),
    # Entity-level timestamps.
    scheduledAt          = rep(base_time, n),
    completedAt          = rep(base_time, n),
    createdAt            = rep(base_time, n),
    createdAtClient      = rep(base_time, n),
    updatedAt            = rep(base_time, n),
    updatedAtClient      = rep(base_time, n),
    # Lifecycle flags.
    followup             = rep(FALSE, n),
    deleted              = rep(FALSE, n))
  # The hierarchy pair, both legs at the first unit: an event recorded
  # in its stay's unit. A fixture wants drift, it passes leg names.
  for (leg in neoipcr:::.hierarchy_leg_names())
    full[[leg]] <- rep(1L, n)
  given <- names(list(...))
  full <- utils::modifyList(full, list(...))
  # The hierarchy columns exist twice, as `event_*` and `enrollment_*`: the
  # recording event's unit and the stay's. A plain name given as an override
  # sets both legs to one value, a fixture without drift; a test that wants
  # drift passes the leg names.
  for (plain in neoipcr:::.hierarchy_key_names) {
    if (plain %in% given) {
      for (leg in c("event_", "enrollment_")) {
        leg_col <- paste0(leg, plain)
        if (!(leg_col %in% given)) full[[leg_col]] <- full[[plain]]
      }
      full[[plain]] <- NULL
    }
  }
  d <- tibble::as_tibble(full[names(schema)])
  for (col in names(schema)) {
    if (is.factor(schema[[col]]) && !is.factor(d[[col]]))
      d[[col]] <- factor(d[[col]], levels = levels(schema[[col]]))
  }
  structure(d, class = c("neoipcr_evt", class(d)))
}

# The hierarchy link columns exist twice, as `event_*` and `enrollment_*`.
# An event-child fixture holds the same value on both legs, which is an
# event recorded in its stay's unit; a schema that declares a leg gets it.
.fill_hierarchy_legs <- function(full, schema, n) {
  for (plain in neoipcr:::.hierarchy_key_names)
    for (leg in c("event_", "enrollment_")) {
      leg_col <- paste0(leg, plain)
      if (leg_col %in% names(schema) && is.null(full[[leg_col]]))
        full[[leg_col]] <- rep(1L, n)
    }
  full
}

# Internal helper: build a per-event-type data fixture from a schema.
# `default_full` supplies concrete values for every base payload column
# under the given event-type's default-full opts. Callers pass
# overrides via `...` and opts-narrowing args.
.make_event_data_fixture <- function(cols, schema, event_keys, default_full,
                                     class_name) {
  n <- length(event_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c(class_name, class(schema))))

  full <- c(list(event_key = event_keys), default_full(n))
  full <- .fill_hierarchy_legs(full, schema, n)

  d <- tibble::as_tibble(full[names(schema)])
  for (col in names(schema)) {
    if (is.factor(schema[[col]]) && !is.factor(d[[col]]))
      d[[col]] <- factor(d[[col]], levels = levels(schema[[col]]))
  }
  structure(d, class = c(class_name, class(d)))
}

.default_event_data_opts <- function(include_event = "full") {
  dhis2_dataset_options(
    include_event            = include_event,
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "pseudo",
    include_hospital         = "pseudo",
    include_country          = "pseudo",
    include_world_bank_class = "pseudo")
}

make_test_admission_data <- function(event_keys = 1L,
                                     include_event = "full",
                                     ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  default <- function(n) list(
    type = factor(rep("1", n), levels = c("1", "2", "3")),
    dol  = rep(1L, n))
  d <- .make_event_data_fixture(
    neoipcr:::admissionData_cols, schema, event_keys, default, "neoipcr_adm")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_surveillance_end_data <- function(event_keys = 1L,
                                             include_event = "full",
                                             ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(
    neoipcr:::surveillanceEndData_cols, opts)
  default <- function(n) list(
    reason             = factor(rep("1", n), levels = c("1", "2")),
    patient_days       = rep(10L, n),
    cvc_days           = rep(3L, n),
    pvc_days           = rep(2L, n),
    vs_days            = rep(2L, n),
    inv_days           = rep(1L, n),
    niv_days           = rep(1L, n),
    ab_days            = rep(5L, n),
    human_milk_days    = rep(8L, n),
    kangaroo_care_days = rep(4L, n),
    probiotic_days     = rep(6L, n))
  d <- .make_event_data_fixture(
    neoipcr:::surveillanceEndData_cols, schema, event_keys, default,
    "neoipcr_end")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_sepsis_data <- function(event_keys = 1L,
                                   include_event = "full",
                                   ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::sepsisData_cols, opts)
  default <- function(n) c(
    list(
      dev_ass = factor(rep("1", n), levels = c("0", "1", "2")),
      los     = rep(5L, n),
      dol     = rep(6L, n)),
    stats::setNames(
      lapply(
        c("acidosis", "ab_treatment", "apnoea", "bradycardia", "crp",
          "feeding_intolerance", "hyperglycaemia", "it_ratio",
          "interleukin", "irritability", "no_pos_culture", "perfusion",
          "platelet_count", "procalcitonin", "temperature", "wbc"),
        \(nm) rep(FALSE, n)),
      c("acidosis", "ab_treatment", "apnoea", "bradycardia", "crp",
        "feeding_intolerance", "hyperglycaemia", "it_ratio",
        "interleukin", "irritability", "no_pos_culture", "perfusion",
        "platelet_count", "procalcitonin", "temperature", "wbc")))
  d <- .make_event_data_fixture(
    neoipcr:::sepsisData_cols, schema, event_keys, default, "neoipcr_bsi")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_nec_data <- function(event_keys = 1L,
                               include_event = "full",
                               ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::necData_cols, opts)
  bool_cols <- c("abdominal_skin_tone", "abdominal_distension",
                 "bilious_aspirate", "bloody_stools", "bowel_necrosis",
                 "fixed_loop", "gastric_residuals",
                 "pneumatosis_intestinalis_img",
                 "pneumatosis_intestinalis_surg", "pneumoperitoneum",
                 "portal_venous_gas", "vomiting")
  default <- function(n) c(
    list(
      los     = rep(5L, n),
      dol     = rep(6L, n),
      sec_bsi = factor(rep("0", n), levels = c("1", "0", "-1"))),
    stats::setNames(
      lapply(bool_cols, \(nm) rep(FALSE, n)),
      bool_cols))
  d <- .make_event_data_fixture(
    neoipcr:::necData_cols, schema, event_keys, default, "neoipcr_nec")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_pneumonia_data <- function(event_keys = 1L,
                                     include_event = "full",
                                     ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::pneumoniaData_cols, opts)
  bool_cols <- c("bradycardia", "fever", "imaging_findings",
                 "increased_respiratory_secretion",
                 "laboratory_findings", "purulent_tracheal_aspirate",
                 "respiratory_distress", "respiratory_support",
                 "tachypnoea")
  default <- function(n) c(
    list(
      dev_ass                     = factor(rep("1", n),
                                           levels = c("0", "1", "2")),
      los                         = rep(5L, n),
      dol                         = rep(6L, n),
      sec_bsi                     = factor(rep("0", n),
                                           levels = c("1", "0", "-1")),
      microbiological_test_result = factor(rep("1", n),
                                           levels = c("1", "0", "-1"))),
    stats::setNames(
      lapply(bool_cols, \(nm) rep(FALSE, n)),
      bool_cols))
  d <- .make_event_data_fixture(
    neoipcr:::pneumoniaData_cols, schema, event_keys, default,
    "neoipcr_hap")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_surgery_data <- function(event_keys = 1L,
                                   include_event = "full",
                                   ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::surgeryData_cols, opts)
  bool_cols <- c("emergency_procedure", "endoscopic_procedure",
                 "implant", "primary_closure", "revision_procedure")
  default <- function(n) c(
    list(
      los                   = rep(3L, n),
      dol                   = rep(4L, n),
      procedure_description = rep("Test procedure", n),
      main_procedure_code   = rep("PZX.AA.JA", n),
      side_procedure_code_1 = rep(NA_character_, n),
      side_procedure_code_2 = rep(NA_character_, n),
      asa_score             = factor(rep("1", n),
                                     levels = c("1", "2", "3", "4", "5")),
      wound_class           = factor(rep("1", n),
                                     levels = c("1", "2", "3", "4")),
      duration              = rep(60L, n),
      infection_signs       = rep(NA_character_, n)),
    stats::setNames(
      lapply(bool_cols, \(nm) rep(FALSE, n)),
      bool_cols))
  d <- .make_event_data_fixture(
    neoipcr:::surgeryData_cols, schema, event_keys, default, "neoipcr_pro")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_ssi_data <- function(event_keys = 1L,
                               include_event = "full",
                               ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::ssiData_cols, opts)
  bool_cols <- c("abscess_deep", "abscess_organ", "fever",
                 "inc_dehisces_deep", "inc_opened_superf",
                 "infection_present", "localized_erythema",
                 "localized_heat", "localized_pain_deep",
                 "localized_pain_superf", "localized_swelling",
                 "physician_diag_superf", "purulent_drainage_deep",
                 "purulent_drainage_drain", "purulent_drainage_superf")
  default <- function(n) c(
    list(
      los              = rep(10L, n),
      dol              = rep(11L, n),
      infection_type   = factor(rep("1", n), levels = c("1", "2", "3")),
      sec_bsi          = factor(rep("0", n), levels = c("1", "0", "-1")),
      organisms_superf = factor(rep("1", n), levels = c("1", "0", "-1")),
      organisms_deep   = factor(rep("1", n), levels = c("1", "0", "-1")),
      organisms_organ  = factor(rep("1", n), levels = c("1", "0", "-1"))),
    stats::setNames(
      lapply(bool_cols, \(nm) rep(FALSE, n)),
      bool_cols))
  d <- .make_event_data_fixture(
    neoipcr:::ssiData_cols, schema, event_keys, default, "neoipcr_ssi")
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  d
}

make_test_substance_days <- function(event_keys = 1L,
                                      include_event = "full",
                                      ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(
    neoipcr:::substanceDays_cols, opts)
  n <- length(event_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_sbd", class(schema))))

  full <- list(
    event_key      = event_keys,
    index          = seq_len(n),
    substance_code = rep("J01CA04", n),
    days           = rep(3L, n))
  full <- .fill_hierarchy_legs(full, schema, n)
  d <- tibble::as_tibble(full[names(schema)])
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  structure(d, class = c("neoipcr_sbd", class(d)))
}

make_test_iaf <- function(event_keys = 1L,
                          include_event = "full",
                          ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  n <- length(event_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_iaf", class(schema))))

  full <- list(
    agent_finding_key = seq_len(n),
    event_key         = event_keys,
    secondary_bsi     = rep(FALSE, n),
    pathogen_key      = seq_len(n),
    index             = rep(1L, n),
    source            = factor(
      rep("B", n), levels = c("B", "C", "B+C", "U", "L", "U+L")),
    multiple          = rep(FALSE, n),
    `3gcr` = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    car    = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    cor    = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    mrsa   = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    vre    = factor(rep("no", n), levels = c("no", "yes", "not_tested")))
  full <- .fill_hierarchy_legs(full, schema, n)
  d <- tibble::as_tibble(full[names(schema)])
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  structure(d, class = c("neoipcr_iaf", class(d)))
}

make_test_unknown_pathogen_names <- function(agent_finding_keys = integer(0),
                                             include_event = "full",
                                             ...) {
  opts <- .default_event_data_opts(include_event)
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  n <- length(agent_finding_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_upn", class(schema))))

  full <- list(
    agent_finding_key = agent_finding_keys,
    name              = rep("Unknown pathogen", n))
  full <- .fill_hierarchy_legs(full, schema, n)
  d <- tibble::as_tibble(full[names(schema)])
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  structure(d, class = c("neoipcr_upn", class(d)))
}

make_test_event_notes <- function(event_keys = 1L,
                                   include_event      = "full",
                                   include_notes      = "events",
                                   include_dhis2_ids  = "notes",
                                   ...) {
  opts <- dhis2_dataset_options(
    include_event     = include_event,
    include_notes     = include_notes,
    include_dhis2_ids = include_dhis2_ids)
  schema <- neoipcr:::compile_schema(neoipcr:::event_notes_cols, opts)
  n <- length(event_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_evn", class(schema))))

  full <- list(
    event_key = event_keys,
    note      = paste0("NOTE_", event_keys),
    value     = rep("test note", n))
  full <- .fill_hierarchy_legs(full, schema, n)
  d <- tibble::as_tibble(full[names(schema)])
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  structure(d, class = c("neoipcr_evn", class(d)))
}

make_test_enrollment_notes <- function(enrollment_keys = integer(0),
                                       include_enrollment = "full",
                                       include_notes      = "enrollments",
                                       include_dhis2_ids  = "notes",
                                       ...) {
  opts <- dhis2_dataset_options(
    include_enrollment = include_enrollment,
    include_notes      = include_notes,
    include_dhis2_ids  = include_dhis2_ids)
  schema <- neoipcr:::compile_schema(
    neoipcr:::enrollment_notes_cols, opts)
  n <- length(enrollment_keys)
  if (ncol(schema) == 0L || n == 0L)
    return(structure(schema, class = c("neoipcr_eln", class(schema))))

  full <- list(
    enrollment_key = enrollment_keys,
    note           = paste0("ENR_NOTE_", enrollment_keys),
    value          = rep("test enrollment note", n))
  d <- tibble::as_tibble(full[names(schema)])
  if (length(list(...)) > 0L) {
    overrides <- list(...)
    for (nm in names(overrides))
      d[[nm]] <- overrides[[nm]]
  }
  structure(d, class = c("neoipcr_eln", class(d)))
}

# ---------------------------------------------------------------------------
# Metadata builders for data-removal tests
# ---------------------------------------------------------------------------

# Shape matches `departments_cols` in R/schema-orgunits.R. Defaults are
# "full" across the board plus "departments" in include_dhis2_ids so
# existing callers (make_populated_test_ds / make_calc_test_ds /
# test-test-units.R) keep producing fully-populated tibbles. Pass
# narrower modes to test other shapes.
make_test_metadata_departments <- function(
    n = 2,
    include_department       = "full",
    include_dhis2_ids        = "departments",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = FALSE)
{
  d_mode  <- rlang::arg_match(
    include_department, c("no", "pseudo", "full"))
  h_mode  <- rlang::arg_match(
    include_hospital, c("no", "pseudo", "full"))
  c_mode  <- rlang::arg_match(
    include_country, c("no", "pseudo", "full"))
  wb_mode <- rlang::arg_match(
    include_world_bank_class, c("no", "pseudo", "full"))

  if (d_mode == "no")
    return(tibble::tibble())

  out <- tibble::tibble(department_key = seq_len(n))

  if ("departments" %in% include_dhis2_ids)
    out$orgUnit <- paste0("OU_DEPT_", seq_len(n))

  if (d_mode == "full") {
    out$code               <- paste0("DEPT_", seq_len(n))
    out$displayName        <- paste0("Department ", seq_len(n))
    out$displayShortName   <- paste0("Dept ", seq_len(n))
    out$displayDescription <- paste0("Department description ", seq_len(n))
    out$comment            <- paste0("Department comment ", seq_len(n))
    out$openingDate        <- as.Date("2020-01-01") + seq_len(n) - 1L
    out$longitude          <- 7.5 + seq_len(n)
    out$latitude           <- 47.5 + seq_len(n)
  }

  if (h_mode != "no")
    out$hospital_key <- seq_len(n)

  # Pre-joined hierarchy keys: present only under d_mode = "full".
  if (d_mode == "full" && c_mode != "no")
    out$country_key <- seq_len(n)
  if (d_mode == "full" && wb_mode != "no")
    out$world_bank_class_key <- seq_len(n)

  if (isTRUE(include_test_data))
    out$isTest <- rep(FALSE, n)

  out
}

# Shape matches `hospitals_cols` in R/schema-orgunits.R. `world_bank_class_key`
# under the inheritance rule appears only when countries doesn't carry
# it in its own compiled schema — i.e. under `include_country = "no"` +
# `include_world_bank_class != "no"`. Default mode is "full"/"full"/"full"
# so existing callers keep producing fully-populated tibbles; pass
# narrower modes to test other shapes.
make_test_metadata_hospitals <- function(
    n = 2,
    include_hospital         = "full",
    include_dhis2_ids        = "hospitals",
    include_country          = "full",
    include_world_bank_class = "full")
{
  h_mode  <- rlang::arg_match(
    include_hospital, c("no", "pseudo", "full"))
  c_mode  <- rlang::arg_match(
    include_country, c("no", "pseudo", "full"))
  wb_mode <- rlang::arg_match(
    include_world_bank_class, c("no", "pseudo", "full"))

  if (h_mode == "no")
    return(tibble::tibble())

  out <- tibble::tibble(hospital_key = seq_len(n))

  if ("hospitals" %in% include_dhis2_ids)
    out$orgUnit <- paste0("OU_HOSP_", seq_len(n))

  if (h_mode == "full") {
    out$code               <- paste0("HOSP_", seq_len(n))
    out$displayName        <- paste0("Hospital ", seq_len(n))
    out$displayShortName   <- paste0("Hosp ", seq_len(n))
    out$displayDescription <- paste0("Hospital description ", seq_len(n))
    out$comment            <- paste0("Hospital comment ", seq_len(n))
    out$longitude          <- 7.5 + seq_len(n)
    out$latitude           <- 47.5 + seq_len(n)
  }

  if (c_mode != "no")
    out$country_key <- seq_len(n)

  # Inherited from countries: hospitals carries `world_bank_class_key`
  # only when countries' compiled schema doesn't (which happens when
  # `include_country = "no"`).
  if (wb_mode != "no" && c_mode == "no")
    out$world_bank_class_key <- seq_len(n)

  out
}

# Shape matches `countries_cols` in R/schema-orgunits.R. Default mode is
# "full" with both `include_country` and `include_world_bank_class` at
# "full" so downstream test code that needs every column keeps working.
# Pass specific modes to test narrower shapes.
make_test_metadata_countries <- function(
    n = 2,
    include_country = "full",
    include_world_bank_class = "full")
{
  country_mode <- rlang::arg_match(
    include_country, c("no", "pseudo", "full"))
  wb_mode <- rlang::arg_match(
    include_world_bank_class, c("no", "pseudo", "full"))

  if (country_mode == "no")
    return(tibble::tibble())

  out <- tibble::tibble(country_key = seq_len(n))

  if (country_mode == "full") {
    out$name               <- paste0("Country ", seq_len(n))
    out$code               <- ordered(paste0("C", seq_len(n)))
    out$displayName        <- ordered(paste0("Country ", seq_len(n)))
    out$displayShortName   <- ordered(paste0("Ctry ", seq_len(n)))
    out$displayDescription <- ordered(paste0("Description ", seq_len(n)))
  }

  if (wb_mode != "no")
    out$world_bank_class_key <- seq_len(n)

  out
}

# Shape matches `worldBankClasses_cols` in R/schema-orgunits.R under the
# default `dhis2_dataset_options()` which sets `include_world_bank_class`
# to "no". Pass `include_world_bank_class = "full"` to receive the full
# three-column shape; "pseudo" returns only the `world_bank_class_key`
# column; "no" returns a 0×0 tibble.
make_test_metadata_wb_classes <- function(
    n = 2,
    include_world_bank_class = "full")
{
  mode <- rlang::arg_match(
    include_world_bank_class, c("no", "pseudo", "full"))

  if (mode == "no")
    return(tibble::tibble())

  out <- tibble::tibble(world_bank_class_key = seq_len(n))

  if (mode == "pseudo")
    return(out)

  out$class       <- factor(
    rep(c("L", "LM", "UM", "H"), length.out = n),
    levels = c("L", "LM", "UM", "H"))
  out$fiscal_year <- rep(2025L, n)
  out
}

make_test_metadata_event_types <- function(
    n = 7,
    include_dhis2_ids = "event_types") {
  all_keys  <- c("adm", "pro", "bsi", "nec", "ssi", "hap", "end")
  all_names <- c(
    "Admission", "Surgical Procedure", "Primary Sepsis/BSI",
    "Necrotizing enterocolitis", "Surgical Site Infection",
    "Pneumonia", "Surveillance-End")
  if (n < 0L || n > length(all_keys))
    rlang::abort(sprintf(
      "n must be between 0 and %d (the 7 protocol-fixed event types).",
      length(all_keys)))

  schema <- neoipcr:::compile_schema(
    neoipcr:::eventTypes_cols,
    dhis2_dataset_options(include_dhis2_ids = include_dhis2_ids))

  keys   <- all_keys[seq_len(n)]
  labels <- all_names[seq_len(n)]
  full <- tibble::tibble(
    event_type_key     = factor(keys, levels = all_keys),
    programStage       = paste0("PS_", keys),
    name               = factor(labels, levels = all_names),
    displayName        = factor(labels, levels = labels),
    displayFormName    = factor(labels, levels = labels),
    displayDescription = paste0(labels, " description"))

  full |>
    dplyr::select(tidyselect::all_of(names(schema)))
}

make_test_metadata_users <- function(
    n = 2,
    include_user      = "full",
    include_dhis2_ids = "users") {
  schema <- neoipcr:::compile_schema(
    neoipcr:::users_cols,
    dhis2_dataset_options(
      include_user      = include_user,
      include_dhis2_ids = include_dhis2_ids))
  if (ncol(schema) == 0L) return(schema)

  # Deterministic values across modes — tests that need to cross-check
  # user_key / username / user can rely on the same formula.
  keys <- seq_len(n)
  full <- tibble::tibble(
    user_key  = keys,
    user      = paste0("USER_", keys),
    username  = paste0("user", keys),
    firstName = paste0("First", keys),
    surname   = paste0("Surname", keys),
    email     = paste0("user", keys, "@example.org"),
    lastLogin = as.POSIXct("2024-01-01", tz = "UTC") + keys,
    created   = as.POSIXct("2023-01-01", tz = "UTC") + keys)

  full |>
    dplyr::select(tidyselect::all_of(names(schema)))
}

# Shape matches `orgUnitAttributes_cols` in R/schema-orgunits.R: 0×0 while
# no opted-in entity is present, else the three definition columns. The
# value types cycle through the families so a fixture exercises every
# typed column.
make_test_metadata_orgUnitAttributes <- function(
    n = 3,
    include_department        = "full",
    include_hospital          = "full",
    include_custom_attributes = c("departments", "hospitals"))
{
  schema <- neoipcr:::compile_schema(
    neoipcr:::orgUnitAttributes_cols,
    dhis2_dataset_options(
      include_department        = include_department,
      include_hospital          = include_hospital,
      include_custom_attributes = include_custom_attributes))
  if (ncol(schema) == 0L) return(schema)

  keys <- seq_len(n)
  tibble::tibble(
    code      = paste0("ATTR_", keys),
    name      = paste0("Attribute ", keys),
    valueType = rep(
      c("TEXT", "DATE", "INTEGER", "TRUE_ONLY", "NUMBER", "DATETIME"),
      length.out = n))
}

# One text-valued row of `ATTR_1` per key, with every other typed column NA
# — the shape a resolved TEXT attribute takes.
.make_test_attribute_values <- function(key_col, keys, schema)
{
  if (ncol(schema) == 0L) return(schema)

  n   <- length(keys)
  out <- tibble::tibble(
    attribute_code = rep("ATTR_1", n),
    value_text     = paste0("Value ", keys),
    value_logical  = rep(NA, n),
    value_integer  = rep(NA_integer_, n),
    value_number   = rep(NA_real_, n),
    value_date     = rep(as.Date(NA), n),
    value_datetime = rep(as.POSIXct(NA_real_, tz = "UTC"), n))
  out[[key_col]] <- as.integer(keys)
  out |>
    dplyr::relocate(tidyselect::all_of(key_col))
}

# Shape matches `departmentAttributeValues_cols` in R/schema-orgunits.R.
make_test_metadata_department_attribute_values <- function(
    department_keys           = 1:2,
    include_department        = "full",
    include_custom_attributes = "departments")
{
  schema <- neoipcr:::compile_schema(
    neoipcr:::departmentAttributeValues_cols,
    dhis2_dataset_options(
      include_department        = include_department,
      include_custom_attributes = include_custom_attributes))
  .make_test_attribute_values("department_key", department_keys, schema)
}

# Shape matches `hospitalAttributeValues_cols` in R/schema-orgunits.R.
make_test_metadata_hospital_attribute_values <- function(
    hospital_keys             = 1:2,
    include_hospital          = "full",
    include_custom_attributes = "hospitals")
{
  schema <- neoipcr:::compile_schema(
    neoipcr:::hospitalAttributeValues_cols,
    dhis2_dataset_options(
      include_hospital          = include_hospital,
      include_custom_attributes = include_custom_attributes))
  .make_test_attribute_values("hospital_key", hospital_keys, schema)
}

# ---------------------------------------------------------------------------
# Convenience: build a populated neoipcr_ds for testing
# ---------------------------------------------------------------------------

# `orgunit_attributes = TRUE` populates the org-unit attribute tables
# (definitions plus one text value per department and hospital); the
# default leaves them 0×0, the shape an import without the
# `include_custom_attributes` opt-in produces, so the narrowed-gate tests
# see exactly that.
make_populated_test_ds <- function(
    n_patients    = 3,
    n_enrollments = 3,
    n_adm_events  = 3,
    n_end_events  = 3,
    metadata      = read_test_metadata(),
    orgunit_attributes = FALSE,
    ...) {
  patients    <- make_test_patients(n_patients)
  enrollments <- make_test_enrollments(n_enrollments,
    patient_keys = rep(seq_len(n_patients), length.out = n_enrollments))

  # Admission events
  adm_events <- make_test_events(
    n               = n_adm_events,
    enrollment_keys = seq_len(n_adm_events),
    patient_keys    = enrollments$patient_key[seq_len(n_adm_events)],
    event_type_keys = rep("adm", n_adm_events))

  # Surveillance end events
  end_events <- make_test_events(
    n               = n_end_events,
    enrollment_keys = seq_len(n_end_events),
    patient_keys    = enrollments$patient_key[seq_len(n_end_events)],
    event_type_keys = rep("end", n_end_events))
  end_events$event_key    <- n_adm_events + seq_len(n_end_events)
  end_events$occurredAt   <- as.Date("2024-01-15") + seq_len(n_end_events) - 1L

  all_events <- dplyr::bind_rows(adm_events, end_events)

  adm_data <- make_test_admission_data(adm_events$event_key)
  end_data <- make_test_surveillance_end_data(end_events$event_key)

  # Add metadata tables for data-removal testing
  md <- metadata
  md$departments     <- make_test_metadata_departments()
  md$hospitals       <- make_test_metadata_hospitals()
  md$countries       <- make_test_metadata_countries()
  md$worldBankClasses <- make_test_metadata_wb_classes()
  md$eventTypes      <- make_test_metadata_event_types()
  md$users           <- make_test_metadata_users()
  if (isTRUE(orgunit_attributes)) {
    md$orgUnitAttributes         <- make_test_metadata_orgUnitAttributes()
    md$departmentAttributeValues <- make_test_metadata_department_attribute_values()
    md$hospitalAttributeValues   <- make_test_metadata_hospital_attribute_values()
  } else {
    md$orgUnitAttributes         <- tibble::tibble()
    md$departmentAttributeValues <- tibble::tibble()
    md$hospitalAttributeValues   <- tibble::tibble()
  }

  make_test_ds(
    metadata        = md,
    patients        = patients,
    enrollments     = enrollments,
    events          = all_events,
    admissionData   = adm_data,
    surveillanceEndData = end_data,
    eventNotes      = make_test_event_notes(all_events$event_key[1:2]),
    ...)
}

# ---------------------------------------------------------------------------
# Calc pipeline fixture: realistic neoipcr_ds for calculate_department_data()
# ---------------------------------------------------------------------------

make_calc_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments()
  md$hospitals         <- make_test_metadata_hospitals()
  md$countries         <- make_test_metadata_countries()
  md$worldBankClasses  <- make_test_metadata_wb_classes()
  md$eventTypes        <- make_test_metadata_event_types()
  # calculate_*_data preconditions (Phase C6) require the link-privacy
  # gates non-"no". Defaults are "no" — set them explicitly here so
  # make_calc_test_ds() produces a dataset that the calc pipeline
  # accepts.
  md$dataset_options   <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  patients <- make_test_patients(3,
    department_key = c(1L, 1L, 2L),
    birth_weight   = c(800L, 1200L, 2500L),
    gest_age       = c("25+0", "30+0", "36+0"),
    total_gestation_days = c(175L, 210L, 252L))

  enrollments <- make_test_enrollments(3,
    patient_keys   = 1:3,
    department_key = c(1L, 1L, 2L),
    enrolledAt     = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10")))

  events <- make_test_events(
    n               = 9,
    enrollment_keys = c(1,1,1, 2,2,2, 3,3,3),
    patient_keys    = c(1,1,1, 2,2,2, 3,3,3),
    event_type_keys = c("adm","end","bsi", "adm","end","pro", "adm","end","bsi"),
    occurredAt      = as.Date(c(
      "2024-01-01","2024-01-15","2024-01-08",
      "2024-01-05","2024-01-20","2024-01-12",
      "2024-01-10","2024-01-25","2024-01-18")),
    department_key  = c(1,1,1, 1,1,1, 2,2,2))

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    admissionData       = make_test_admission_data(c(1L, 4L, 7L)),
    surveillanceEndData = make_test_surveillance_end_data(c(2L, 5L, 8L),
      patient_days = c(15L, 16L, 16L)),
    sepsisData          = make_test_sepsis_data(c(3L, 9L),
      dol = c(8L, 9L), los = c(7L, 8L),
      dev_ass = factor(c("1", "0"))),
    surgeryData         = make_test_surgery_data(6L),
    infectiousAgentFindings = make_test_iaf(c(3L, 9L),
      pathogen_key = c(1L, 503L)),
    substanceDays       = make_test_substance_days(c(2L, 5L, 8L)))
}

# A calculation fixture of one stay with the hierarchy pair at its widest
# spread: the patient record stands in department 1, the stay, its admission,
# and its surveillance end in department 2, and the recording desk of its one
# sepsis in department 3. The stay spans 100 patient-days, enough to enter a
# reference quartile.
make_split_department_calc_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments(3)
  md$hospitals        <- make_test_metadata_hospitals(3)
  md$countries        <- make_test_metadata_countries(3)
  md$worldBankClasses <- make_test_metadata_wb_classes(3)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  events <- make_test_events(
    n                         = 3L,
    enrollment_keys           = c(1, 1, 1),
    patient_keys              = c(1, 1, 1),
    event_type_keys           = c("adm", "end", "bsi"),
    occurredAt                = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-01-08")),
    event_department_key      = c(2L, 2L, 3L),
    enrollment_department_key = rep(2L, 3))

  make_test_ds(
    metadata                = md,
    patients                = make_test_patients(
      1L, department_key = 1L, birth_weight = 1200L, gest_age = "30+0",
      total_gestation_days = 210L),
    enrollments             = make_test_enrollments(
      1L, patient_keys = 1L, department_key = 2L,
      enrolledAt = as.Date("2024-01-01")),
    events                  = events,
    admissionData           = make_test_admission_data(1L),
    surveillanceEndData     = make_test_surveillance_end_data(
      2L, patient_days = 100L),
    sepsisData              = make_test_sepsis_data(3L, dol = 8L, los = 7L),
    infectiousAgentFindings = make_test_iaf(3L),
    substanceDays           = make_test_substance_days(2L))
}

# A dataset as an older neoipcr would have stored it: its events table
# holds the plain hierarchy keys of the pre-pair import — the values of
# the event side stand for them, which no refusal depends on — and
# lacks the `event_`/`enrollment_` pair its calculations group by.
make_stale_import_ds <- function(ds = make_calc_test_ds()) {
  for (key in c("department_key", "hospital_key", "country_key",
                "world_bank_class_key")) {
    ds$events[[key]] <- ds$events[[paste0("event_", key)]]
    ds$events[[paste0("event_", key)]] <- NULL
    ds$events[[paste0("enrollment_", key)]] <- NULL
  }
  ds
}

# Calc pipeline fixture of infection forms. `forms` holds one row per
# infection form: its `event_type_key` ("bsi", "nec", "hap", "ssi"), its
# `department_key` (1 where the column is left out), and any form item to set
# (`sec_bsi`, `microbiological_test_result`, `infection_type`,
# `organisms_superf`, `organisms_deep`, `organisms_organ`, `no_pos_culture`),
# as the stored option code or flag; an item left out keeps the form
# builder's default, and `NA` leaves it unanswered. `findings` holds one row
# per infectious-agent slot: `form`, the row of `forms` whose form holds it,
# and any findings column to set (`pathogen_key`, `secondary_bsi`, `source`,
# `multiple`, a resistance category); a column left out keeps
# `make_test_iaf()`'s default. Departments 1 to the highest key exist, each
# with one patient and one enrolment, its admission and surveillance-end
# forms, and the department's infections; `patient_days` gives each
# department's stay its patient-days, one value per department, the
# surveillance-end form's default where left out.
make_infection_calc_test_ds <- function(forms, findings = NULL,
                                        patient_days = NULL) {
  if (!"department_key" %in% names(forms))
    forms$department_key <- 1L
  forms$department_key <- as.integer(forms$department_key)
  n_dep    <- max(forms$department_key)
  dep_keys <- seq_len(n_dep)
  n_inf    <- nrow(forms)
  inf_keys <- 2L * n_dep + seq_len(n_inf)

  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments(n_dep)
  md$hospitals        <- make_test_metadata_hospitals(n_dep)
  md$countries        <- make_test_metadata_countries(n_dep)
  md$worldBankClasses <- make_test_metadata_wb_classes(n_dep)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  events <- make_test_events(
    n               = 2L * n_dep + n_inf,
    enrollment_keys = c(dep_keys, dep_keys, forms$department_key),
    patient_keys    = c(dep_keys, dep_keys, forms$department_key),
    event_type_keys = c(rep("adm", n_dep), rep("end", n_dep),
                        forms$event_type_key),
    department_key  = c(dep_keys, dep_keys, forms$department_key))

  # A stored value replaces the builder's default in the builder's type, so
  # that a code stays a factor level as the reader leaves it.
  set_columns <- function(d, values) {
    for (col in names(values))
      d[[col]] <- if (is.factor(d[[col]]))
        factor(values[[col]], levels = levels(d[[col]]))
      else values[[col]]
    d
  }
  form_data <- function(type, builder) {
    rows <- forms$event_type_key == type
    items <- setdiff(names(forms), c("event_type_key", "department_key"))
    d <- builder(inf_keys[rows])
    set_columns(d, as.list(forms[rows, intersect(items, names(d))]))
  }

  iaf <- if (is.null(findings) || nrow(findings) == 0L)
    make_test_iaf(integer(0))
  else
    set_columns(
      make_test_iaf(inf_keys[findings$form]),
      as.list(findings[setdiff(names(findings), "form")]))

  surveillance_end <- if (is.null(patient_days))
    make_test_surveillance_end_data(n_dep + dep_keys)
  else
    make_test_surveillance_end_data(
      n_dep + dep_keys, patient_days = as.integer(patient_days))

  make_test_ds(
    metadata                = md,
    patients                = make_test_patients(n_dep, department_key = dep_keys),
    enrollments             = make_test_enrollments(
      n_dep, patient_keys = dep_keys, department_key = dep_keys),
    events                  = events,
    admissionData           = make_test_admission_data(dep_keys),
    surveillanceEndData     = surveillance_end,
    sepsisData              = form_data("bsi", make_test_sepsis_data),
    necData                 = form_data("nec", make_test_nec_data),
    pneumoniaData           = form_data("hap", make_test_pneumonia_data),
    ssiData                 = form_data("ssi", make_test_ssi_data),
    infectiousAgentFindings = iaf)
}

# Golden fixture of the infectious-agent tables: 17 infections of department 1,
# their infectious agents spread over the reporting groups, two eukaryotic
# kingdoms (Fungi and Protozoa), and three virus families (Orthoherpesviridae,
# Picornaviridae, Adenoviridae). By form, with the codes its slots hold:
#   1. sepsis: Staphylococcus aureus (934) and S. epidermidis (810), one genus;
#   2. sepsis: the genus Staphylococcus (481), recorded above its species;
#   3. sepsis: Escherichia coli (1807);
#   4. sepsis: Candida albicans (2273);
#   5. pneumonia tested positive and answered Yes: Klebsiella pneumoniae (390)
#      in its own and in a secondary-BSI slot;
#   6. pneumonia tested positive: Pseudomonas aeruginosa (1958) and the genus
#      Candida (3422), recorded above its species;
#   7. NEC answered Yes: Enterococcus faecalis (2345) in a secondary-BSI slot;
#   8. NEC answered No: E. coli (1807) in a secondary-BSI slot, which the form
#      does not ask for;
#   9. superficial SSI tested positive there: Aspergillus fumigatus (1717) and
#      Prototheca zopfii (2411);
#  10. sepsis: two synonyms of the herpes simplex virus 1 (2617 and 2616);
#  11. sepsis: Enterovirus betacoxsackie (3496);
#  12. pneumonia tested positive: Mastadenovirus adami (3598);
#  13. sepsis: "Not listed" (0) alone;
#  14. sepsis: "Unidentifiable" (3561) and S. aureus (934);
#  15. deep SSI tested positive there: a code the list lacks (99999999) alone;
#  16. sepsis: a slot holding a source and a resistance category alone;
#  17. sepsis recorded culture-negative: S. aureus (934) in a slot the form does
#      not ask for.
make_infectious_agent_calc_test_ds <- function() {
  forms <- tibble::tibble(
    event_type_key = c("bsi", "bsi", "bsi", "bsi", "hap", "hap", "nec",
                       "nec", "ssi", "bsi", "bsi", "hap", "bsi", "bsi",
                       "ssi", "bsi", "bsi"),
    sec_bsi = c(NA, NA, NA, NA, "1", "-1", "1", "0", "-1", NA, NA, "-1", NA,
                NA, "-1", NA, NA),
    microbiological_test_result = c(NA, NA, NA, NA, "1", "1", NA, NA, NA, NA,
                                    NA, "1", NA, NA, NA, NA, NA),
    infection_type = c(rep(NA, 8), "1", rep(NA, 5), "2", NA, NA),
    organisms_superf = c(rep(NA, 8), "1", rep(NA, 5), "-1", NA, NA),
    organisms_deep = c(rep(NA, 14), "1", NA, NA),
    organisms_organ = NA_character_,
    no_pos_culture = c(FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, NA, FALSE,
                       FALSE, NA, FALSE, FALSE, NA, FALSE, TRUE))
  findings <- tibble::tibble(
    form = c(1L, 1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L, 7L, 8L, 9L, 9L, 10L, 10L,
             11L, 12L, 13L, 14L, 14L, 15L, 16L, 17L),
    secondary_bsi = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                      FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    pathogen_key = c(934L, 810L, 481L, 1807L, 2273L, 390L, 390L, 1958L, 3422L,
                     2345L, 1807L, 1717L, 2411L, 2617L, 2616L, 3496L, 3598L,
                     0L, 3561L, 934L, 99999999L, NA, 934L),
    `3gcr` = factor(c(rep("no", 21), "yes", "no"),
                    levels = c("no", "yes", "not_tested")))
  make_infection_calc_test_ds(forms, findings)
}

# Golden fixture of the resistance tables: 18 infections of department 1,
# whose stay has 200 patient-days. The resistance categories are
# third-generation cephalosporin resistance (3GCR), carbapenem and colistin
# resistance, methicillin-resistant Staphylococcus aureus (MRSA), and
# vancomycin-resistant Enterococcus (VRE); every category a slot does not
# name below is blank. By form, with the codes its slots hold:
#   1. sepsis: Escherichia coli (1807), 3GCR resistant, colistin not tested;
#   2. pneumonia tested positive and answered Yes: Klebsiella pneumoniae (390)
#      in its own slot, 3GCR not resistant, and in a secondary-BSI slot, 3GCR
#      resistant and carbapenem not resistant;
#   3. sepsis: Staphylococcus aureus (934), MRSA, and 3GCR resistant, a
#      category the list does not apply to it;
#   4. sepsis: Staphylococcus argenteus (2778), not MRSA;
#   5. sepsis: Enterococcus faecium (1865), vancomycin not tested;
#   6. sepsis: Enterococcus faecalis (2345), VRE;
#   7. sepsis: Streptococcus agalactiae (2600), no resistance category;
#   8. sepsis: E. faecalis (2345), vancomycin blank;
#   9. sepsis: Acinetobacter baumannii (1660), 3GCR resistant, a category the
#      list does not apply to it, carbapenem resistant, and colistin not
#      resistant;
#  10. sepsis: "Not listed" (0), 3GCR, carbapenem, and colistin resistant;
#  11. sepsis: "Unidentifiable" (3561), MRSA;
#  12. deep SSI tested positive there: a code the list lacks (99999999), 3GCR
#      resistant;
#  13. sepsis recorded culture-negative: E. coli (1807), 3GCR, carbapenem, and
#      colistin resistant, in a slot the form does not ask for;
#  14. sepsis: a slot holding a carbapenem resistance alone;
#  15. sepsis: E. coli (1807), 3GCR, carbapenem, and colistin resistant, and
#      K. pneumoniae (390), 3GCR not resistant;
#  16. NEC answered No: E. coli (1807), 3GCR resistant, in a secondary-BSI
#      slot, which the form does not ask for;
#  17. sepsis: K. pneumoniae (390), 3GCR not tested;
#  18. sepsis: the genus Enterococcus (503), recorded above its species, not
#      VRE.
make_resistance_calc_test_ds <- function() {
  forms <- tibble::tibble(
    event_type_key = c("bsi", "hap", "bsi", "bsi", "bsi", "bsi", "bsi", "bsi",
                       "bsi", "bsi", "bsi", "ssi", "bsi", "bsi", "bsi", "nec",
                       "bsi", "bsi"),
    sec_bsi = c(NA, "1", rep(NA, 9), "-1", NA, NA, NA, "0", NA, NA),
    microbiological_test_result = c(NA, "1", rep(NA, 16)),
    infection_type = c(rep(NA, 11), "2", rep(NA, 6)),
    organisms_superf = c(rep(NA, 11), "-1", rep(NA, 6)),
    organisms_deep = c(rep(NA, 11), "1", rep(NA, 6)),
    organisms_organ = NA_character_,
    no_pos_culture = c(FALSE, NA, rep(FALSE, 9), NA, TRUE, FALSE, FALSE, NA,
                       FALSE, FALSE))
  findings <- tibble::tribble(
    ~form, ~secondary_bsi, ~pathogen_key, ~`3gcr`,      ~car,  ~cor,         ~mrsa, ~vre,
       1L, FALSE,                  1807L, "yes",        NA,    "not_tested", NA,    NA,
       2L, FALSE,                   390L, "no",         NA,    NA,           NA,    NA,
       2L, TRUE,                    390L, "yes",        "no",  NA,           NA,    NA,
       3L, FALSE,                   934L, "yes",        NA,    NA,           "yes", NA,
       4L, FALSE,                  2778L, NA,           NA,    NA,           "no",  NA,
       5L, FALSE,                  1865L, NA,           NA,    NA,           NA,    "not_tested",
       6L, FALSE,                  2345L, NA,           NA,    NA,           NA,    "yes",
       7L, FALSE,                  2600L, NA,           NA,    NA,           NA,    NA,
       8L, FALSE,                  2345L, NA,           NA,    NA,           NA,    NA,
       9L, FALSE,                  1660L, "yes",        "yes", "no",         NA,    NA,
      10L, FALSE,                     0L, "yes",        "yes", "yes",        NA,    NA,
      11L, FALSE,                  3561L, NA,           NA,    NA,           "yes", NA,
      12L, FALSE,              99999999L, "yes",        NA,    NA,           NA,    NA,
      13L, FALSE,                  1807L, "yes",        "yes", "yes",        NA,    NA,
      14L, FALSE,            NA_integer_, NA,           "yes", NA,           NA,    NA,
      15L, FALSE,                  1807L, "yes",        "yes", "yes",        NA,    NA,
      15L, FALSE,                   390L, "no",         NA,    NA,           NA,    NA,
      16L, TRUE,                   1807L, "yes",        NA,    NA,           NA,    NA,
      17L, FALSE,                   390L, "not_tested", NA,    NA,           NA,    NA,
      18L, FALSE,                   503L, NA,           NA,    NA,           NA,    "no")
  with_antibiotic_entries(
    make_infection_calc_test_ds(forms, findings, patient_days = 200L))
}

# A sepsis per form of `findings` (columns `form`, numbered from 1,
# `pathogen_key`, and any of the resistance categories), of the department
# `department_key` gives it, by form, with a stay of `patient_days` per
# department, departments 1 to the highest key, and an antibiotic entry on
# it. A resistance category `findings` does not name is blank, where
# make_test_iaf() records every category as not resistant.
make_sepsis_findings_test_ds <- function(findings, department_key = 1L,
                                         patient_days = 100L) {
  for (category in c("3gcr", "car", "cor", "mrsa", "vre"))
    if (!category %in% names(findings))
      findings[[category]] <- NA_character_
  department_key <- rep_len(as.integer(department_key), max(findings$form))
  with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(event_type_key = "bsi",
                           department_key = department_key),
    findings = findings,
    patient_days = rep_len(as.integer(patient_days), max(department_key))))
}

# `ds` with an antibiotic entry on each surveillance-end form, since
# `get_risk_time()` leaves a stay without one out of the patient-days.
with_antibiotic_entries <- function(ds) {
  ds$substanceDays <- make_test_substance_days(
    ds$events$event_key[ds$events$event_type_key == "end"])
  ds
}

# Departments' quartiles of the resistance tables: 8 departments, each
# infection a sepsis.
#   * Departments 1-5: 100 patient-days, 5 infections with Escherichia coli
#     (1807), k - 1 of department k's 3GCR resistant and the others not;
#   * department 6: 40 patient-days, 4 infections with E. coli, 3GCR
#     resistant;
#   * department 7: 100 patient-days, 5 infections with E. coli, 3GCR blank;
#   * department 8: 100 patient-days, 1 infection with Staphylococcus aureus
#     (934), MRSA blank.
# Every other resistance category is blank.
make_resistance_quartile_test_ds <- function() {
  departments <- c(rep(1:5, each = 5), rep(6L, 4), rep(7L, 5), 8L)
  resistant <- unlist(lapply(1:5, \(k) c(rep("yes", k - 1L), rep("no", 6L - k))))
  with_antibiotic_entries(make_infection_calc_test_ds(
    forms = tibble::tibble(
      event_type_key = "bsi",
      department_key = departments),
    findings = tibble::tibble(
      form = seq_along(departments),
      pathogen_key = c(rep(1807L, 34), 934L),
      `3gcr` = c(resistant, rep("yes", 4), rep(NA, 6)),
      car = NA_character_,
      cor = NA_character_,
      mrsa = NA_character_,
      vre = NA_character_),
    patient_days = c(rep(100L, 5), 40L, 100L, 100L)))
}

# Exposure-days fixture: sixteen patients in three departments of six, six
# and four patients, on seventeen completed stays — patient 1 of department 1
# is readmitted, and its second stay carries no antibiotic entry. The days of
# the eight exposure factors, summed over the stays:
#   * CVC: seven patients, 31 days (patient 1's two stays carry 3 and 2 of
#     them, so the readmission's days must sum in although its stay stands
#     without an antibiotic entry);
#   * PVC: two patients, 5 days; INV: one patient, 4 days;
#     NIV: two patients, 7 days; any antibiotic: three patients, 12 days;
#   * human milk: eight patients, 55 days — five of them, 40 days, in
#     department 1, the one department of at least five exposed patients for
#     one factor;
#   * probiotics: no patient, no day; kangaroo care: two patients, 17 days.
# Patients 11, 14 and 16 complete a stay with no exposure day.
make_exposure_patients_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments(3)
  md$hospitals        <- make_test_metadata_hospitals(3)
  md$countries        <- make_test_metadata_countries(3)
  md$worldBankClasses <- make_test_metadata_wb_classes(3)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  stay_patient    <- c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  stay_department <- c(rep(1L, 7), rep(2L, 6), rep(3L, 4))

  patients <- make_test_patients(16,
    department_key = c(rep(1L, 6), rep(2L, 6), rep(3L, 4)))

  enrollments <- make_test_enrollments(
    n              = 17,
    patient_keys   = stay_patient,
    department_key = stay_department)

  events <- make_test_events(
    n               = 34,
    enrollment_keys = rep(seq_len(17), each = 2),
    patient_keys    = rep(stay_patient, each = 2),
    event_type_keys = rep(c("adm", "end"), 17),
    department_key  = rep(stay_department, each = 2))

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = seq.int(2, 34, by = 2),
      patient_days =
        c(8L, 5L, 12L, 22L, 14L, 9L, 6L, 11L, 4L, 8L, 5L, 6L, 7L, 4L, 5L, 5L, 4L),
      cvc_days =
        c(3L, 2L, 4L, 10L, 7L, 0L, 0L, 3L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
      pvc_days = c(0L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L),
      vs_days = rep(0L, 17),
      inv_days = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
      niv_days = c(0L, 1L, 6L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
      ab_days = c(0L, 0L, 4L, 0L, 0L, 0L, 0L, 5L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 0L),
      human_milk_days =
        c(0L, 0L, 4L, 20L, 6L, 8L, 2L, 10L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 2L, 0L),
      kangaroo_care_days =
        c(0L, 0L, 0L, 0L, 12L, 0L, 0L, 0L, 0L, 5L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
      probiotic_days = rep(0L, 17)),
    # Stay 2, patient 1's readmission, holds no antibiotic entry: its event
    # key is the one left out of the substance days.
    substanceDays = make_test_substance_days(
      event_keys = seq.int(2, 34, by = 2)[-2]))
}

# Exposure-days fixture for the departments' quartiles: thirty patients on
# thirty completed stays, six in each of five departments, so five departments
# reach the quartiles' denominator of five patients. CVC: one, two, three,
# four and five exposed patients with six, eight, twelve, twenty and ten days
# — fifteen patients, fifty-six days. Human milk: five, six, five, six and
# five exposed patients with one, two, three, four and five days each —
# twenty-seven patients, eighty-one days, mean days one to five by
# department.
make_exposure_quartiles_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments(5)
  md$hospitals        <- make_test_metadata_hospitals(5)
  md$countries        <- make_test_metadata_countries(5)
  md$worldBankClasses <- make_test_metadata_wb_classes(5)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  patients <- make_test_patients(30,
    department_key = rep(seq_len(5), each = 6))

  enrollments <- make_test_enrollments(
    n              = 30,
    patient_keys   = seq_len(30),
    department_key = rep(seq_len(5), each = 6))

  events <- make_test_events(
    n               = 60,
    enrollment_keys = rep(seq_len(30), each = 2),
    patient_keys    = rep(seq_len(30), each = 2),
    event_type_keys = rep(c("adm", "end"), 30),
    department_key  = rep(rep(seq_len(5), each = 6), each = 2))

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = seq.int(2, 60, by = 2),
      patient_days = rep(10L, 30),
      cvc_days = c(6L, rep(0L, 5), 4L, 4L, rep(0L, 4),
                   4L, 4L, 4L, rep(0L, 3),
                   5L, 5L, 5L, 5L, rep(0L, 2),
                   2L, 2L, 2L, 2L, 2L, 0L),
      pvc_days = rep(0L, 30),
      vs_days = rep(0L, 30),
      inv_days = rep(0L, 30),
      niv_days = rep(0L, 30),
      ab_days = rep(0L, 30),
      human_milk_days = c(rep(1L, 5), 0L,
                          rep(2L, 6),
                          rep(3L, 5), 0L,
                          rep(4L, 6),
                          rep(5L, 5), 0L),
      kangaroo_care_days = rep(0L, 30),
      probiotic_days = rep(0L, 30)))
}

# Exposure-days fixture with the patients table pseudonymized: two patients
# on three completed stays — patient 1 with one stay in each of two
# departments, patient 1 readmission and patient 2 a single stay in the
# first. The patients table of the pseudo tier carries no department, so the
# only department left is the enrolment's, and patient 1 stands in two rows
# of the per-department figures: the pooled figures must count that patient
# once. CVC: patient 1 exposed 5 days over its two stays, patient 2 4.
make_exposure_pseudo_patient_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments(2)
  md$hospitals        <- make_test_metadata_hospitals(2)
  md$countries        <- make_test_metadata_countries(2)
  md$worldBankClasses <- make_test_metadata_wb_classes(2)
  md$eventTypes       <- make_test_metadata_event_types()
  md$dataset_options  <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "pseudo",
    include_enrollment = "full",
    include_event      = "full")

  patients <- make_test_patients(2,
    include_patient = "pseudo",
    include_department = "full")

  enrollments <- make_test_enrollments(
    n                = 3,
    patient_keys     = c(1L, 1L, 2L),
    department_key   = c(1L, 2L, 1L),
    include_patient  = "pseudo")

  events <- make_test_events(
    n               = 6,
    enrollment_keys = rep(seq_len(3), each = 2),
    patient_keys    = rep(c(1L, 1L, 2L), each = 2),
    event_type_keys = rep(c("adm", "end"), 3),
    department_key  = c(1L, 1L, 2L, 2L, 1L, 1L),
    include_patient  = "pseudo")

  make_test_ds(
    metadata            = md,
    patients            = patients,
    enrollments         = enrollments,
    events              = events,
    surveillanceEndData = make_test_surveillance_end_data(
      event_keys = c(2L, 4L, 6L),
      patient_days = c(5L, 6L, 7L),
      cvc_days = c(3L, 2L, 4L),
      pvc_days = rep(0L, 3),
      vs_days = rep(0L, 3),
      inv_days = rep(0L, 3),
      niv_days = rep(0L, 3),
      ab_days = rep(0L, 3),
      human_milk_days = rep(0L, 3),
      kangaroo_care_days = rep(0L, 3),
      probiotic_days = rep(0L, 3)))
}


#' Construct a minimal structurally valid neoipcr_ds object.
#'
#' Produces a list with the same structure and S3 classes as import_dhis2().
#' All data tibbles default to empty. Override individual slots via named
#' arguments.
#'
#' @param metadata A neoipcr_metadata object (default: read_test_metadata()).
#' @param patients Override the patients tibble.
#' @param enrollments Override the enrollments tibble.
#' @param events Override the events tibble.
#' @param ... Additional named slots merged into the list.
#' @return A list of class c("neoipcr_ds", "list").
make_test_ds <- function(
    metadata    = read_test_metadata(),
    patients    = tibble::tibble(),
    enrollments = tibble::tibble(),
    events      = tibble::tibble(),
    ...)
{
  # Ensure dataset_options is present so assert_options_for() doesn't
  # abort with "dataset_options is NULL". import_dhis2() stores this;
  # test fixtures must mirror it. Default: full gates on everything
  # so calc-pipeline / validate / table builders can run without
  # per-test opt-in.
  if (is.null(metadata$dataset_options))
    metadata$dataset_options <- dhis2_dataset_options(
      include_department = "full",
      include_patient    = "full",
      include_enrollment = "full",
      include_event      = "full")

  # The validation slots as the import leaves them after a pass that found
  # nothing — the results empty, the summary its three totals rows at zero —
  # where the options run the pass, else the 0×0 the schema gives; the
  # import emits no other shape.
  validation_results <- neoipcr:::compile_schema(
    neoipcr:::validationResults_cols, metadata$dataset_options)
  validation_summary <- if (ncol(validation_results) == 0L)
    neoipcr:::compile_schema(neoipcr:::validationSummary_cols, metadata$dataset_options)
  else neoipcr:::.validation_summary(validation_results, validation_results)
  # The reconciliation slot as the import leaves it where no stored value
  # needed repair: a row per reconciliation, at zero where the options let it
  # read its records and `NA` where they do not, or 0×0 where the options
  # reconcile nothing. With nothing reconciled the counts do not depend on
  # which records the dataset holds, so the options alone decide them.
  reconciliation_summary <- neoipcr:::.reconciliation_summary(
    neoipcr:::.reconciliation_log(), list(metadata = metadata))

  # Empty tibbles with correct column names so rules can select columns
  # even when no data rows exist.
  base <- list(
    patients                = structure(patients, class = c("neoipcr_pat", class(patients))),
    enrollments             = structure(enrollments, class = c("neoipcr_enr", class(enrollments))),
    events                  = structure(events, class = c("neoipcr_evt", class(events))),
    eventNotes              = make_test_event_notes(integer(0)),
    enrollment_notes        = make_test_enrollment_notes(integer(0)),
    admissionData           = make_test_admission_data(integer(0)),
    surveillanceEndData     = make_test_surveillance_end_data(integer(0)),
    sepsisData              = make_test_sepsis_data(integer(0)),
    necData                 = make_test_nec_data(integer(0)),
    pneumoniaData           = make_test_pneumonia_data(integer(0)),
    surgeryData             = make_test_surgery_data(integer(0)),
    ssiData                 = make_test_ssi_data(integer(0)),
    substanceDays           = make_test_substance_days(integer(0)),
    infectiousAgentFindings = make_test_iaf(integer(0)),
    unknownPathogenNames    = make_test_unknown_pathogen_names(integer(0)),
    validationResults       = validation_results,
    validationSummary       = validation_summary,
    reconciliationSummary   = reconciliation_summary,
    metadata                = structure(metadata, class = c("neoipcr_metadata", class(metadata))),
    .cache                  = new.env(parent = emptyenv())
  )

  overrides <- list(...)
  for (nm in names(overrides))
    base[[nm]] <- overrides[[nm]]

  structure(base, class = c("neoipcr_ds", "list"))
}

# Key-form exception records for one rule, as `validate()` hands them to the
# rules: every key column present, `NA` where the record has none.
make_test_exceptions <- function(rule_id,
                                 patient_key    = NA_integer_,
                                 enrollment_key = NA_integer_,
                                 event_key      = NA_integer_)
  tibble::tibble(
    rule_id        = as.integer(rule_id),
    patient_key    = as.integer(patient_key),
    enrollment_key = as.integer(enrollment_key),
    event_key      = as.integer(event_key))


# Empty calc pipeline fixture: neoipcr_ds with 0 patients/enrollments/events
# but full metadata — for testing table builders on empty data.
# ---------------------------------------------------------------------------

make_empty_calc_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full",
      include_country    = "full"))
  md$departments      <- make_test_metadata_departments()
  md$hospitals         <- make_test_metadata_hospitals()
  md$countries         <- make_test_metadata_countries()
  md$worldBankClasses  <- make_test_metadata_wb_classes()
  md$eventTypes        <- make_test_metadata_event_types()
  md$dataset_options   <- dhis2_dataset_options(
    include_department = "full",
    include_country    = "full",
    include_patient    = "full",
    include_enrollment = "full",
    include_event      = "full")

  make_test_ds(
    metadata    = md,
    patients    = make_test_patients(0),
    enrollments = make_test_enrollments(0),
    events      = make_test_events(0))
}

# Hold a rule's findings to the context fields the registry declares for the
# rule — the contract a consumer's templates are written against, which
# `validate()` enforces on a pass. The rule tests call their rule directly,
# so each detect case holds its findings to the declaration through this.
expect_declared_context <- function(result)
  testthat::expect_no_error(neoipcr:::.assert_declared_context(
    result, neoipcr::validation_rule_context_fields()))

# The form the registry says an enrolment-level rule shows its finding on
# is the type of the event every finding of the rule carries; an exception
# record naming that form is accepted on the strength of the declaration,
# so the declaration is checked against the rule's own output.
expect_declared_form <- function(result, ds) {
  declared <- neoipcr:::.rule_event_types(result$rule_id[1])
  carried  <- ds$events$event_type_key[match(result$event_key, ds$events$event_key)]
  testthat::expect_equal(unique(as.character(carried)), declared)
}
