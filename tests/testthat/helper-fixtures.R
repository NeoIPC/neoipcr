# Test fixtures and builders for neoipcr tests.
# Auto-loaded by testthat 3.x before test files run.

# Valid values for the `exclude` parameter of read_test_metadata()
.valid_exclusions <- c(
  "system", "program", "program_id", "program_stages",
  "stage_data_elements", "tracked_entity_attributes",
  "countries", "test_units", "antimicrobials"
)

#' Read static JSON fixtures and return processed metadata.
#'
#' Loads fixture files from tests/testthat/fixtures/, merges them into a single
#' metadata list, and passes the result through read_metadata().
#'
#' @param exclude Character vector of components to omit. See .valid_exclusions
#'   for allowed values.
#' @param dataset_options A neoipcr_dhis2_dsopt object (default: all defaults).
#' @return A neoipcr_metadata object.
read_test_metadata <- function(
    exclude = character(),
    dataset_options = dhis2_dataset_options())
{
  bad <- setdiff(exclude, .valid_exclusions)
  if (length(bad) > 0L)
    stop("Unknown exclusion(s): ", paste(bad, collapse = ", "),
         "\nValid values: ", paste(.valid_exclusions, collapse = ", "))

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
    siblings             = rep(0L, n),
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
    "total_gestation_days", "delivery_mode", "siblings")
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
    department_key       = rep(1L, n),
    hospital_key         = rep(1L, n),
    country_key          = rep(1L, n),
    world_bank_class_key = rep(1L, n),
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
  full <- utils::modifyList(full, list(...))
  d <- tibble::as_tibble(full[names(schema)])
  for (col in names(schema)) {
    if (is.factor(schema[[col]]) && !is.factor(d[[col]]))
      d[[col]] <- factor(d[[col]], levels = levels(schema[[col]]))
  }
  structure(d, class = c("neoipcr_evt", class(d)))
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

# ---------------------------------------------------------------------------
# Convenience: build a populated neoipcr_ds for testing
# ---------------------------------------------------------------------------

make_populated_test_ds <- function(
    n_patients    = 3,
    n_enrollments = 3,
    n_adm_events  = 3,
    n_end_events  = 3,
    metadata      = read_test_metadata(),
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
    metadata                = structure(metadata, class = c("neoipcr_metadata", class(metadata))),
    .cache                  = new.env(parent = emptyenv())
  )

  overrides <- list(...)
  for (nm in names(overrides))
    base[[nm]] <- overrides[[nm]]

  structure(base, class = c("neoipcr_ds", "list"))
}


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
