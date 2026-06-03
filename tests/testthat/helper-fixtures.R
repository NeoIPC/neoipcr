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

make_test_patients <- function(n = 3, ...) {
  d <- list(
    patient_key        = seq_len(n),
    trackedEntity      = paste0("TE_", seq_len(n)),
    patient_id         = paste0("PAT_", seq_len(n)),
    sex                = factor(rep("M", n)),
    birth_weight       = rep(1500L, n),
    gest_age           = rep("30+0", n),
    total_gestation_days = rep(210L, n),
    department_key     = seq_len(n),
    hospital_key       = seq_len(n),
    country_key        = seq_len(n),
    world_bank_class_key = seq_len(n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_pat", class(d)))
}

make_test_enrollments <- function(
    n = 3,
    patient_keys = seq_len(n),
    ...) {
  d <- list(
    enrollment_key     = seq_len(n),
    enrollment         = paste0("ENR_", seq_len(n)),
    patient_key        = patient_keys[seq_len(n)],
    enrolledAt         = as.Date("2024-01-01") + seq_len(n) - 1L,
    followUp           = rep(FALSE, n),
    status             = factor(rep("COMPLETED", n),
                                levels = c("ACTIVE", "COMPLETED", "CANCELLED")),
    department_key     = seq_len(n),
    hospital_key       = seq_len(n),
    country_key        = seq_len(n),
    world_bank_class_key = seq_len(n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_enr", class(d)))
}

make_test_events <- function(
    n = 5,
    enrollment_keys = rep(1L, n),
    patient_keys    = rep(1L, n),
    event_type_keys = rep("adm", n),
    ...) {
  d <- list(
    event_key          = seq_len(n),
    event              = paste0("EVT_", seq_len(n)),
    occurredAt         = as.Date("2024-01-01") + seq_len(n) - 1L,
    status             = factor(rep("COMPLETED", n),
      levels = c("ACTIVE", "COMPLETED", "VISITED",
                 "SCHEDULE", "OVERDUE", "SKIPPED")),
    event_type_key     = event_type_keys[seq_len(n)],
    enrollment_key     = enrollment_keys[seq_len(n)],
    patient_key        = patient_keys[seq_len(n)],
    department_key     = rep(1L, n),
    hospital_key       = rep(1L, n),
    country_key        = rep(1L, n),
    world_bank_class_key = rep(1L, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_evt", class(d)))
}

make_test_admission_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    type      = factor(rep("1", n)),
    dol       = rep(1L, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_adm", class(d)))
}

make_test_surveillance_end_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key          = event_keys,
    reason             = factor(rep("1", n)),
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
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_end", class(d)))
}

make_test_sepsis_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    dev_ass   = factor(rep("1", n)),
    los       = rep(5L, n),
    dol       = rep(6L, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_bsi", class(d)))
}

make_test_nec_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    los       = rep(5L, n),
    dol       = rep(6L, n),
    sec_bsi   = rep(FALSE, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_nec", class(d)))
}

make_test_pneumonia_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    dev_ass   = factor(rep("1", n)),
    los       = rep(5L, n),
    dol       = rep(6L, n),
    sec_bsi   = rep(FALSE, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_hap", class(d)))
}

make_test_surgery_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key             = event_keys,
    los                   = rep(3L, n),
    dol                   = rep(4L, n),
    procedure_description = rep("Test procedure", n),
    main_procedure_code   = rep("PZX.AA.JA", n),
    side_procedure_code_1 = rep(NA_character_, n),
    side_procedure_code_2 = rep(NA_character_, n),
    asa_score             = rep(1L, n),
    wound_class           = factor(rep("1", n)),
    duration              = rep(60L, n),
    emergency_procedure   = rep(FALSE, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_pro", class(d)))
}

make_test_ssi_data <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key      = event_keys,
    los            = rep(10L, n),
    dol            = rep(11L, n),
    infection_type = factor(rep("1", n)),
    sec_bsi        = rep(FALSE, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_ssi", class(d)))
}

make_test_substance_days <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key      = event_keys,
    index          = seq_len(n),
    substance_code = rep("J01CA04", n),
    days           = rep(3L, n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_sbd", class(d)))
}

make_test_iaf <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key     = event_keys,
    secondary_bsi = rep(FALSE, n),
    pathogen_key  = seq_len(n),
    index         = rep(1L, n),
    source        = factor(rep("B", n)),
    multiple      = rep(FALSE, n),
    `3gcr`        = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    car           = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    cor           = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    mrsa          = factor(rep("no", n), levels = c("no", "yes", "not_tested")),
    vre           = factor(rep("no", n), levels = c("no", "yes", "not_tested")))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_iaf", class(d)))
}

make_test_event_details <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    event     = paste0("EVT_", event_keys))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_evd", class(d)))
}

make_test_event_notes <- function(event_keys = 1L, ...) {
  n <- length(event_keys)
  d <- list(
    event_key = event_keys,
    note      = paste0("NOTE_", event_keys),
    value     = rep("test note", n))
  d <- utils::modifyList(d, list(...))
  d <- tibble::as_tibble(d)
  structure(d, class = c("neoipcr_evn", class(d)))
}

# ---------------------------------------------------------------------------
# Metadata builders for data-removal tests
# ---------------------------------------------------------------------------

make_test_metadata_departments <- function(n = 2) {
  tibble::tibble(
    department_key       = seq_len(n),
    orgUnit              = paste0("OU_DEPT_", seq_len(n)),
    code                 = paste0("DEPT_", seq_len(n)),
    displayName          = paste0("Department ", seq_len(n)),
    hospital_key         = seq_len(n),
    country_key          = seq_len(n),
    world_bank_class_key = seq_len(n))
}

make_test_metadata_hospitals <- function(n = 2) {
  tibble::tibble(
    hospital_key         = seq_len(n),
    code                 = paste0("HOSP_", seq_len(n)),
    displayName          = paste0("Hospital ", seq_len(n)),
    country_key          = seq_len(n),
    world_bank_class_key = seq_len(n))
}

make_test_metadata_countries <- function(n = 2) {
  tibble::tibble(
    country_key          = seq_len(n),
    country              = paste0("CTRY_", seq_len(n)),
    code                 = ordered(paste0("C", seq_len(n))),
    displayName          = ordered(paste0("Country ", seq_len(n))),
    world_bank_class_key = seq_len(n))
}

make_test_metadata_wb_classes <- function(n = 2) {
  tibble::tibble(
    world_bank_class_key = seq_len(n),
    class                = paste0("WB_", seq_len(n)),
    displayName          = paste0("WB Class ", seq_len(n)))
}

make_test_metadata_event_types <- function() {
  tibble::tibble(
    event_type_key = c("adm", "end", "bsi", "nec", "hap", "ssi", "pro"),
    programStage   = paste0("PS_", c("adm", "end", "bsi", "nec", "hap", "ssi", "pro")),
    name           = c("Admission", "Surveillance End", "BSI", "NEC", "HAP", "SSI", "Surgery"))
}

make_test_metadata_users <- function(n = 2) {
  tibble::tibble(
    user_key = seq_len(n),
    username = paste0("user", seq_len(n)),
    user     = paste0("USER_", seq_len(n)))
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
    eventDetails    = make_test_event_details(all_events$event_key),
    eventNotes      = make_test_event_notes(all_events$event_key[1:2]),
    ...)
}

# ---------------------------------------------------------------------------
# Calc pipeline fixture: realistic neoipcr_ds for calculate_department_data()
# ---------------------------------------------------------------------------

make_calc_test_ds <- function() {
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "yes",
      include_country    = "yes"))
  md$departments      <- make_test_metadata_departments()
  md$hospitals         <- make_test_metadata_hospitals()
  md$countries         <- make_test_metadata_countries()
  md$worldBankClasses  <- make_test_metadata_wb_classes()
  md$eventTypes        <- make_test_metadata_event_types()
  md$dataset_options   <- dhis2_dataset_options(
    include_department = "yes",
    include_country    = "yes")

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
    infectiousAgentFindings = make_test_iaf(c(3L, 9L)),
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
  # Empty tibbles with correct column names so rules can select columns
  # even when no data rows exist.
  base <- list(
    patients                = structure(patients, class = c("neoipcr_pat", class(patients))),
    enrollments             = structure(enrollments, class = c("neoipcr_enr", class(enrollments))),
    events                  = structure(events, class = c("neoipcr_evt", class(events))),
    eventDetails            = make_test_event_details(integer(0)),
    eventNotes              = NULL,
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
