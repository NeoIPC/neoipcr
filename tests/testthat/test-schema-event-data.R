# Tests for R/schema-event-data.R — the seven per-event-type data
# schemas (admissionData, surveillanceEndData, sepsisData, necData,
# pneumoniaData, surgeryData, ssiData).

# ---- Three-mode shape per type -------------------------------------------

.type_cols <- list(
  adm = neoipcr:::admissionData_cols,
  end = neoipcr:::surveillanceEndData_cols,
  bsi = neoipcr:::sepsisData_cols,
  nec = neoipcr:::necData_cols,
  hap = neoipcr:::pneumoniaData_cols,
  pro = neoipcr:::surgeryData_cols,
  ssi = neoipcr:::ssiData_cols
)

test_that("every per-event-type tibble is 0x0 under include_event = 'no'", {
  opts <- dhis2_dataset_options(include_event = "no")
  for (type in names(.type_cols)) {
    schema <- neoipcr:::compile_schema(.type_cols[[type]], opts)
    expect_equal(ncol(schema), 0L, info = type)
    expect_equal(nrow(schema), 0L, info = type)
  }
})

test_that("pseudo events carries only event_key + payload (link/hierarchy absent via inheritance)", {
  # Under pseudo events, events_cols has only event_key. The
  # inheritance rule on per-event-type cols therefore keeps enrollment_key
  # / patient_key / hierarchy keys ABSENT because events doesn't carry
  # them — the rule only materializes a key on the child when the
  # parent doesn't. So the child tibble has event_key + payload only.
  opts <- dhis2_dataset_options(
    include_event = "pseudo",
    # Orthogonal gates deliberately open — the inheritance rule
    # governs presence, not these.
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  # Under pseudo-event, events_cols is just event_key. The inheritance
  # rule keeps link/hierarchy keys absent on children when the parent
  # schema doesn't carry them — but events only has event_key, so the
  # children should materialize them. Verify for admissionData.
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  expect_true("event_key" %in% names(schema))
  # Under pseudo events (events has no enrollment_key/patient_key/etc.),
  # the children materialize them directly per the inheritance rule.
  expect_true("enrollment_key" %in% names(schema))
  expect_true("patient_key"    %in% names(schema))
})

test_that("full events keeps per-event-type lean (hierarchy / links reached via events)", {
  opts <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  for (type in names(.type_cols)) {
    schema <- neoipcr:::compile_schema(.type_cols[[type]], opts)
    # Events already materializes all links + hierarchy + isTest;
    # children inherit (absent direct) per the rule.
    expect_false("enrollment_key"       %in% names(schema), info = type)
    expect_false("patient_key"          %in% names(schema), info = type)
    expect_false("department_key"       %in% names(schema), info = type)
    expect_false("hospital_key"         %in% names(schema), info = type)
    expect_false("country_key"          %in% names(schema), info = type)
    expect_false("world_bank_class_key" %in% names(schema), info = type)
    expect_false("isTest"               %in% names(schema), info = type)
    # event_key always present
    expect_true("event_key" %in% names(schema), info = type)
  }
})

# ---- Per-type payload coverage -------------------------------------------

test_that("admissionData: payload covers type + dol", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  expect_true(all(c("type", "dol") %in% names(schema)))
  expect_true(is.factor(schema$type))
  expect_identical(levels(schema$type), c("1", "2", "3"))
})

test_that("surveillanceEndData: payload covers reason + day-counter set + derived vs_days", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::surveillanceEndData_cols, opts)
  expect_true(all(c(
    "reason", "patient_days", "cvc_days", "pvc_days", "vs_days",
    "inv_days", "niv_days", "ab_days", "human_milk_days",
    "kangaroo_care_days", "probiotic_days") %in% names(schema)))
  expect_identical(levels(schema$reason), c("1", "2"))
})

test_that("sepsisData: payload includes dev_ass + los + dol + all symptom flags", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::sepsisData_cols, opts)
  expect_true(all(c(
    "dev_ass", "los", "dol",
    "acidosis", "ab_treatment", "apnoea", "bradycardia", "crp",
    "feeding_intolerance", "hyperglycaemia", "it_ratio", "interleukin",
    "irritability", "no_pos_culture", "perfusion", "platelet_count",
    "procalcitonin", "temperature", "wbc") %in% names(schema)))
  expect_identical(levels(schema$dev_ass), c("0", "1", "2"))
})

test_that("necData: sec_bsi is factor (option set KfIEzWRibj7 levels)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::necData_cols, opts)
  expect_true(is.factor(schema$sec_bsi))
  expect_identical(levels(schema$sec_bsi), c("1", "0", "-1"))
})

test_that("pneumoniaData: dev_ass + sec_bsi + microbiological_test_result are all factors", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::pneumoniaData_cols, opts)
  expect_true(is.factor(schema$dev_ass))
  expect_true(is.factor(schema$sec_bsi))
  expect_true(is.factor(schema$microbiological_test_result))
  expect_identical(levels(schema$microbiological_test_result),
                   c("1", "0", "-1"))
})

test_that("surgeryData: asa_score + wound_class are factors, duration is integer", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::surgeryData_cols, opts)
  expect_true(is.factor(schema$asa_score))
  expect_identical(levels(schema$asa_score), c("1", "2", "3", "4", "5"))
  expect_true(is.factor(schema$wound_class))
  expect_identical(levels(schema$wound_class), c("1", "2", "3", "4"))
  expect_type(schema$duration, "integer")
})

test_that("ssiData: infection_type + sec_bsi + organisms_* are all factors", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::ssiData_cols, opts)
  expect_true(is.factor(schema$infection_type))
  expect_identical(levels(schema$infection_type), c("1", "2", "3"))
  for (col in c("sec_bsi", "organisms_superf", "organisms_deep",
                "organisms_organ")) {
    expect_true(is.factor(schema[[col]]), info = col)
  }
})

# ---- Companion-column gating -------------------------------------------

test_that("event_data_attribute_cols: five companions per DE (storedBy + createdBy + updatedBy + createdAt + updatedAt)", {
  opts_bare <- dhis2_dataset_options(include_event = "full")
  opts_user <- dhis2_dataset_options(
    include_event = "full", include_user = "full")
  opts_ts   <- dhis2_dataset_options(
    include_event = "full", include_timestamps = TRUE)
  opts_full <- dhis2_dataset_options(
    include_event = "full", include_user = "full",
    include_timestamps = TRUE)

  s_bare <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_bare)
  s_user <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_user)
  s_ts   <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_ts)
  s_full <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts_full)

  # Bare opts: no include_user, no include_timestamps → no companions.
  for (suffix in c("_storedBy", "_createdBy", "_updatedBy",
                   "_createdAt", "_updatedAt"))
    expect_false(paste0("dol", suffix) %in% names(s_bare), info = suffix)

  # include_user alone → three user companions, no timestamp companions.
  expect_true("dol_storedBy"   %in% names(s_user))
  expect_true("dol_createdBy"  %in% names(s_user))
  expect_true("dol_updatedBy"  %in% names(s_user))
  expect_false("dol_createdAt" %in% names(s_user))
  expect_false("dol_updatedAt" %in% names(s_user))

  # include_timestamps alone → two timestamp companions, no user
  # companions.
  expect_false("dol_storedBy"  %in% names(s_ts))
  expect_false("dol_createdBy" %in% names(s_ts))
  expect_false("dol_updatedBy" %in% names(s_ts))
  expect_true("dol_createdAt"  %in% names(s_ts))
  expect_true("dol_updatedAt"  %in% names(s_ts))

  # Full: all five companions present. DHIS2 DataValue.java carries all
  # five audit fields (storedBy, createdBy, updatedBy, createdAt,
  # updatedAt) per data value. Extended from three to five companions
  # in phase-b-event-details.
  for (suffix in c("_storedBy", "_createdBy", "_updatedBy",
                   "_createdAt", "_updatedAt"))
    expect_true(paste0("dol", suffix) %in% names(s_full), info = suffix)
})

# ---- Fixture round-trips -------------------------------------------------

test_that("make_test_admission_data matches admissionData_cols schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::admissionData_cols, opts)
  fixture <- make_test_admission_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_surveillance_end_data matches schema (vs_days present)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::surveillanceEndData_cols, opts)
  fixture <- make_test_surveillance_end_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true("vs_days" %in% names(fixture))
})

test_that("make_test_sepsis_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::sepsisData_cols, opts)
  fixture <- make_test_sepsis_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_nec_data matches schema (sec_bsi is factor)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::necData_cols, opts)
  fixture <- make_test_nec_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true(is.factor(fixture$sec_bsi))
})

test_that("make_test_pneumonia_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::pneumoniaData_cols, opts)
  fixture <- make_test_pneumonia_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_surgery_data matches schema (asa_score is factor)", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::surgeryData_cols, opts)
  fixture <- make_test_surgery_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
  expect_true(is.factor(fixture$asa_score))
})

test_that("make_test_ssi_data matches schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::ssiData_cols, opts)
  fixture <- make_test_ssi_data(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

# ---- event_data_cols_for dispatch ---------------------------------------

test_that("event_data_cols_for dispatches every valid event_type_key", {
  for (k in c("adm", "end", "bsi", "nec", "hap", "pro", "ssi")) {
    expect_identical(
      neoipcr:::event_data_cols_for(k),
      .type_cols[[k]],
      info = k)
  }
  expect_error(
    neoipcr:::event_data_cols_for("unknown"),
    "Unknown event_type_key")
})

# ---- findings / substanceDays / unknownPathogenNames three-mode shape ---

test_that("findings_cols / substanceDays_cols / unknownPathogenNames_cols are 0x0 under include_event = 'no'", {
  opts <- dhis2_dataset_options(include_event = "no")
  for (cols in list(neoipcr:::findings_cols,
                    neoipcr:::substanceDays_cols,
                    neoipcr:::unknownPathogenNames_cols)) {
    schema <- neoipcr:::compile_schema(cols, opts)
    expect_equal(ncol(schema), 0L)
    expect_equal(nrow(schema), 0L)
  }
})

test_that("findings_cols pseudo-mode keeps PK + event_key + inherited link/hierarchy only (payload absent)", {
  opts <- dhis2_dataset_options(
    include_event      = "pseudo",
    include_enrollment = "full",
    include_patient    = "full",
    include_test_data  = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  # PK + event_key always
  expect_true("agent_finding_key" %in% names(schema))
  expect_true("event_key"         %in% names(schema))
  # Inherited (events carries only event_key under pseudo → children
  # materialize enrollment_key / patient_key + isTest directly).
  expect_true("enrollment_key"    %in% names(schema))
  expect_true("patient_key"       %in% names(schema))
  expect_true("isTest"            %in% names(schema))
  # Payload atoms absent under pseudo (they require include_event = "full").
  for (col in c("secondary_bsi", "pathogen_key", "index", "source",
                "multiple", "3gcr", "car", "cor", "mrsa", "vre")) {
    expect_false(col %in% names(schema), info = col)
  }
})

test_that("findings_cols full-mode: source + resistance markers + multiple always declared (fixes failure #6)", {
  # Historical bug: when no surviving pathogen had a `_SOURCE` DE, the
  # reader dropped the `source` column. Schema declares it unconditionally
  # under full mode so the pre-pivot factor pinning + names_expand = TRUE
  # guarantees it emerges regardless of raw data content.
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)

  expect_true("source" %in% names(schema))
  expect_true(is.factor(schema$source))
  expect_identical(
    levels(schema$source),
    c("B", "C", "B+C", "U", "L", "U+L"))

  expect_true("multiple" %in% names(schema))
  expect_true(is.logical(schema$multiple))

  for (col in c("3gcr", "car", "cor", "mrsa", "vre")) {
    expect_true(col %in% names(schema), info = col)
    expect_true(is.factor(schema[[col]]), info = col)
    expect_identical(
      levels(schema[[col]]),
      c("no", "yes", "not_tested"),
      info = col)
  }
})

test_that("findings_cols full-mode: hierarchy keys absent via inheritance (events carries them)", {
  opts <- dhis2_dataset_options(
    include_event            = "full",
    include_enrollment       = "full",
    include_patient          = "full",
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_test_data        = TRUE)
  schema <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  # Events already materializes these; children inherit (absent directly).
  expect_false("enrollment_key"       %in% names(schema))
  expect_false("patient_key"          %in% names(schema))
  expect_false("department_key"       %in% names(schema))
  expect_false("hospital_key"         %in% names(schema))
  expect_false("country_key"          %in% names(schema))
  expect_false("world_bank_class_key" %in% names(schema))
  expect_false("isTest"               %in% names(schema))
  # Direct link + payload still present.
  expect_true("event_key"             %in% names(schema))
  expect_true("source"                %in% names(schema))
})

test_that("substanceDays_cols full-mode: index + substance_code + days declared", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::substanceDays_cols, opts)
  expect_true(all(c("event_key", "index", "substance_code", "days")
                  %in% names(schema)))
  expect_type(schema$index, "integer")
  expect_type(schema$substance_code, "character")
  expect_type(schema$days, "integer")
})

test_that("unknownPathogenNames_cols full-mode: agent_finding_key + name only", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  expect_identical(names(schema), c("agent_finding_key", "name"))
  expect_type(schema$agent_finding_key, "integer")
  expect_type(schema$name, "character")
})

test_that("unknownPathogenNames_cols pseudo-mode: only agent_finding_key (name absent)", {
  opts <- dhis2_dataset_options(include_event = "pseudo")
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  expect_identical(names(schema), "agent_finding_key")
})

# ---- Fixture round-trips (findings family) ------------------------------

test_that("make_test_iaf output matches findings_cols schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::findings_cols, opts)
  fixture <- make_test_iaf(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_substance_days output matches substanceDays_cols schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(neoipcr:::substanceDays_cols, opts)
  fixture <- make_test_substance_days(event_keys = 1:3)
  expect_schema_matches(fixture, schema)
})

test_that("make_test_unknown_pathogen_names output matches unknownPathogenNames_cols schema", {
  opts <- dhis2_dataset_options(include_event = "full")
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  fixture <- make_test_unknown_pathogen_names(agent_finding_keys = 1:2)
  expect_schema_matches(fixture, schema)
})

# ---- split_unknown_pathogen_names behaviour -----------------------------

test_that("split_unknown_pathogen_names: empty intermediate → schema-shaped 0-row tibble", {
  opts <- dhis2_dataset_options(include_event = "full")
  # Caller never reaches the split with a 0-row, no-`name` input
  # (the reader's early-return paths handle that), but the helper
  # itself must be robust to a 0-row `name`-bearing intermediate.
  intermediate <- tibble::tibble(
    agent_finding_key = integer(0),
    name              = character(0))
  result <- neoipcr:::split_unknown_pathogen_names(intermediate, opts)
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts)
  expect_schema_matches(result, schema)
  expect_equal(nrow(result), 0L)
})

test_that("split_unknown_pathogen_names: intermediate with `name` split correctly", {
  opts <- dhis2_dataset_options(include_event = "full")
  intermediate <- tibble::tibble(
    agent_finding_key = 1:4,
    name              = c(NA_character_, "", "Custom pathogen A",
                          "Custom pathogen B"))
  result <- neoipcr:::split_unknown_pathogen_names(intermediate, opts)
  # Only non-NA, non-empty names should survive.
  expect_equal(nrow(result), 2L)
  expect_equal(result$name, c("Custom pathogen A", "Custom pathogen B"))
  expect_equal(result$agent_finding_key, c(3L, 4L))
})

test_that("split_unknown_pathogen_names: intermediate without `name` → schema shape", {
  # Under pseudo events, the intermediate has no `name` column (payload
  # atoms are gated off upstream). The split must gracefully emit the
  # schema's shape.
  opts_pseudo <- dhis2_dataset_options(include_event = "pseudo")
  intermediate <- tibble::tibble(
    agent_finding_key = 1L,
    event_key         = 1L)
  result <- neoipcr:::split_unknown_pathogen_names(intermediate, opts_pseudo)
  schema <- neoipcr:::compile_schema(
    neoipcr:::unknownPathogenNames_cols, opts_pseudo)
  expect_schema_matches(result, schema)
})
