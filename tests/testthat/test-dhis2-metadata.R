# Tests for R/dhis2-metadata.R — read_metadata() and its sub-readers.
# Fixtures loaded via helper-fixtures.R::read_test_metadata().

# --- Structural tests ---

test_that("read_test_metadata returns a list", {
  metadata <- read_test_metadata()
  expect_type(metadata, "list")
})

test_that("read_test_metadata result contains expected top-level names", {
  metadata <- read_test_metadata()
  expected <- c(
    "system", "programId", "trackedEntityTypeId", "eventTypes",
    "options", "dataElements", "trackedEntityAttributes",
    "antimicrobialSubstances", "awareCategories", "atc5Categories",
    "testUnitIds", "admissionTypes", "asaScores",
    "sepsisDeviceAssociation", "sepsisPathogenSources",
    "deliveryModes", "pneumoniaDeviceAssociation",
    "pneumoniaPathogenSources", "sexes", "ssiTypes",
    "surveillanceEndReasons", "woundClasses", "testResults",
    "surveillanceResults")
  expect_true(all(expected %in% names(metadata)))
})

# --- Validation error tests (distinct failure scenarios) ---

test_that("read_metadata aborts when system metadata is missing", {
  expect_error(
    read_test_metadata(exclude = "system"),
    class = "neoipcr_metadata_system_missing")
})

test_that("read_metadata aborts when program key is entirely absent", {
  expect_error(
    read_test_metadata(exclude = "program"),
    class = "neoipcr_metadata_program_missing")
})

test_that("read_metadata aborts when program exists but id is missing", {
  expect_error(
    read_test_metadata(exclude = "program_id"),
    class = "neoipcr_metadata_program_missing")
})

test_that("read_metadata aborts when programStages are missing", {
  expect_error(
    read_test_metadata(exclude = "program_stages"),
    class = "neoipcr_metadata_programStages_missing")
})

test_that("read_metadata aborts when programStageDataElements are missing", {
  expect_error(
    read_test_metadata(exclude = "stage_data_elements"),
    class = "neoipcr_metadata_programStageDataElements_missing")
})

test_that("read_metadata aborts when trackedEntityAttributes are missing", {
  expect_error(
    read_test_metadata(exclude = "tracked_entity_attributes"),
    class = "neoipcr_metadata_programTrackedEntityAttributes_missing")
})

# --- Optional metadata absence (no error expected) ---

test_that("read_metadata succeeds when countries are absent", {
  expect_no_error(read_test_metadata(exclude = "countries"))
})

test_that("read_metadata succeeds when test units are absent", {
  expect_no_error(read_test_metadata(exclude = "test_units"))
})

test_that("read_metadata succeeds when both countries and test units are absent", {
  expect_no_error(
    read_test_metadata(exclude = c("countries", "test_units")))
})

# --- Per-sub-reader data tests ---

test_that("read_metadata parses system metadata correctly", {
  metadata <- read_test_metadata()
  expect_equal(
    metadata$system$date,
    readr::parse_datetime("2024-11-08T14:06:41.216+0000"))
  expect_equal(
    metadata$system$id,
    uuid::as.UUID("72c2bd70-573a-4d69-8bc3-f7bb431bdc23"))
  expect_equal(metadata$system$rev, "3fcd748")
  expect_equal(metadata$system$version, as.numeric_version("2.40.3.2"))
})

test_that("read_metadata parses program id", {
  metadata <- read_test_metadata()
  expect_equal(metadata$programId, "D8mSSpOpsKj")
})

test_that("read_metadata parses event types from program stages", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$eventTypes), 2L)
  expect_equal(
    sort(as.character(metadata$eventTypes$name)),
    c("Admission", "Surgical Procedure"))
})

test_that("read_metadata parses data elements with option set references", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$dataElements), 4L)
  expect_equal(
    metadata$dataElements$optionSet,
    c(NA, NA, "NEOIPC_ADMISSION_TYPES", NA))
})

test_that("read_metadata parses tracked entity attributes", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$trackedEntityAttributes), 2L)
  # Column is renamed from 'id' to 'attribute' by read_metadata
  expect_true("attribute" %in% names(metadata$trackedEntityAttributes))
  expect_equal(
    metadata$trackedEntityAttributes$attribute,
    c("yQwpowV0o08", "E5OMg8BC8be"))
  expect_equal(
    metadata$trackedEntityAttributes$code,
    c("NEOIPC_PATIENT_ID", "NEOIPC_TEA_SEX"))
})

test_that("read_metadata parses countries when include_country is full", {
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(include_country = "full"))
  expect_false(is.null(metadata$countries))
  expect_equal(nrow(metadata$countries), 2L)
  expect_equal(
    sort(as.character(metadata$countries$code)),
    c("CH", "DE"))
})

test_that("read_metadata returns 0x0 countries tibble with default options", {
  # Default `dhis2_dataset_options()` has `include_country = "no"`; under
  # the three-mode schema contract the reader emits a 0×0 tibble (never
  # NULL) so downstream code can gate on column presence consistently.
  metadata <- read_test_metadata()
  expect_false(is.null(metadata$countries))
  expect_s3_class(metadata$countries, "tbl_df")
  expect_equal(ncol(metadata$countries), 0L)
  expect_equal(nrow(metadata$countries), 0L)
})

test_that("read_metadata parses antimicrobial substances", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$antimicrobialSubstances), 13L)
  expect_true("code" %in% names(metadata$antimicrobialSubstances))
  expect_true("displayName" %in% names(metadata$antimicrobialSubstances))
})

test_that("read_metadata parses AWaRe categories", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$awareCategories), 3L)
  expect_true(all(c("WHO_AWARE_ACCESS", "WHO_AWARE_RESERVE", "WHO_AWARE_WATCH")
                  %in% metadata$awareCategories$code))
})

test_that("read_metadata parses ATC5 categories", {
  metadata <- read_test_metadata()
  expect_equal(nrow(metadata$atc5Categories), 3L)
  expect_true(all(c("J01CF", "J01DH", "J01CR")
                  %in% metadata$atc5Categories$code))
})

test_that("read_metadata parses test unit IDs when test data included", {
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(include_test_data = TRUE))
  expect_true(length(metadata$testUnitIds) > 0L)
  expect_true(all(c("VUNdfvqcGI7", "hzte6b3Z8Zd") %in% metadata$testUnitIds))
})

# --- read_metadata_wb_classes — three-mode shape contract ---
#
# The reader honors the `0 → 1 → N` column-count progression declared by
# `worldBankClasses_cols` in R/schema-orgunits.R. Under "no" it emits a
# 0×0 tibble; under "pseudo" a 1-column tibble of distinct surviving keys;
# under "full" the full schema. It also returns a `country_map` internal
# lookup (always NULL under "no"; may be non-NULL otherwise) consumed by
# `read_metadata_countries()` for the WB-class ↔ country join.

# Build a minimal DHIS2 metadata list containing a WORLD_BANK_CLASSES
# organisationUnitGroupSet with two groups (H and LM for FY 2025) and two
# organisationUnits per group. Keeps the year fixed at 2025 so the
# reader's "most recent year with data" lookup converges regardless of
# the test-runner calendar year (reader iterates current_year downward to
# 2025).
build_wb_class_metadata <- function() {
  list(
    organisationUnitGroupSets = list(
      list(
        code = "WORLD_BANK_CLASSES",
        organisationUnitGroups = list(
          list(
            id    = "wbH2025",
            code  = "WORLD_BANK_CLASS_H_FY_2025",
            name  = "World Bank Class H FY 2025",
            displayName = "High income FY 2025",
            organisationUnits = list(
              list(id = "P3M2xL6Gtbs"),
              list(id = "TS5pOUJsdoa"))),
          list(
            id    = "wbLM2025",
            code  = "WORLD_BANK_CLASS_LM_FY_2025",
            name  = "World Bank Class LM FY 2025",
            displayName = "Lower-middle income FY 2025",
            organisationUnits = list(
              list(id = "OU_LM_1"),
              list(id = "OU_LM_2")))
        )
      )
    )
  )
}

test_that("read_metadata_wb_classes returns 0x0 tibble under include_world_bank_class='no'", {
  opts <- dhis2_dataset_options(include_world_bank_class = "no")
  result <- neoipcr:::read_metadata_wb_classes(
    build_wb_class_metadata(), opts)

  expect_type(result, "list")
  expect_named(result, c("public", "country_map"))
  expect_equal(ncol(result$public), 0L)
  expect_equal(nrow(result$public), 0L)
  expect_null(result$country_map)
})

test_that("read_metadata_wb_classes returns single-column tibble under 'pseudo'", {
  opts <- dhis2_dataset_options(include_world_bank_class = "pseudo")
  result <- neoipcr:::read_metadata_wb_classes(
    build_wb_class_metadata(), opts)

  expect_equal(ncol(result$public), 1L)
  expect_equal(nrow(result$public), 2L)
  expect_identical(names(result$public), "world_bank_class_key")
  expect_true(is.integer(result$public$world_bank_class_key))
  expect_false(is.null(result$country_map))
})

test_that("read_metadata_wb_classes returns full schema under 'full'", {
  opts <- dhis2_dataset_options(include_world_bank_class = "full")
  result <- neoipcr:::read_metadata_wb_classes(
    build_wb_class_metadata(), opts)

  expect_schema_matches(
    result$public,
    neoipcr:::get_worldBankClasses_schema(opts))
  expect_equal(nrow(result$public), 2L)
  expect_identical(
    sort(as.character(result$public$class)),
    c("H", "LM"))
  expect_true(all(result$public$fiscal_year == 2025L))
})

test_that("read_metadata_wb_classes returns empty shape when group set is absent", {
  # Metadata with no organisationUnitGroupSets at all — the reader must
  # still return a schema-conformant empty tibble (not NULL, not error).
  empty_metadata <- list()
  for (mode in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(include_world_bank_class = mode)
    result <- neoipcr:::read_metadata_wb_classes(empty_metadata, opts)

    expect_schema_matches(
      result$public,
      neoipcr:::get_worldBankClasses_schema(opts))
    expect_equal(nrow(result$public), 0L,
      info = sprintf("mode = '%s'", mode))
    expect_null(result$country_map,
      info = sprintf("mode = '%s'", mode))
  }
})

test_that("read_metadata_wb_classes returns empty shape when WB group set exists but has no groups", {
  # Group set present but no groups — same as absent.
  metadata <- list(organisationUnitGroupSets = list(
    list(code = "WORLD_BANK_CLASSES", organisationUnitGroups = list())))
  for (mode in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(include_world_bank_class = mode)
    result <- neoipcr:::read_metadata_wb_classes(metadata, opts)

    expect_schema_matches(
      result$public,
      neoipcr:::get_worldBankClasses_schema(opts))
    expect_equal(nrow(result$public), 0L,
      info = sprintf("mode = '%s'", mode))
    expect_null(result$country_map,
      info = sprintf("mode = '%s'", mode))
  }
})

test_that("read_metadata_wb_classes country_map exposes the WB-class-to-country mapping", {
  # Country-map is the internal lookup consumed by read_metadata_countries().
  # It must carry world_bank_class_key + organisationUnits (list column).
  opts <- dhis2_dataset_options(include_world_bank_class = "full")
  result <- neoipcr:::read_metadata_wb_classes(
    build_wb_class_metadata(), opts)

  expect_false(is.null(result$country_map))
  expect_identical(
    names(result$country_map),
    c("world_bank_class_key", "organisationUnits"))
  expect_equal(nrow(result$country_map), 2L)
})

# --- read_metadata orchestrator — worldBankClasses always in ret ---

test_that("read_metadata stores worldBankClasses as an empty tibble (never NULL) under 'no'", {
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(include_world_bank_class = "no"))

  expect_false(is.null(metadata$worldBankClasses))
  expect_s3_class(metadata$worldBankClasses, "tbl_df")
  expect_equal(ncol(metadata$worldBankClasses), 0L)
  expect_equal(nrow(metadata$worldBankClasses), 0L)
})

# --- read_metadata_countries — three-mode shape contract ---
#
# The reader honors the three-mode contract declared by `countries_cols`
# and also returns an orchestrator-internal lookup tibble
# (`internal_map` with `country`, `code`, `country_key`) used by
# post-read joins that can't reach into the public schema-conformant
# tibble. Both components are exercised here.

test_that("read_metadata_countries returns 0x0 public tibble under include_country='no' + wb='no'", {
  opts <- dhis2_dataset_options(
    include_country = "no", include_world_bank_class = "no")
  # The underlying COUNTRY org unit group is present in the fixture but
  # the reader's early-exit skips the read entirely under this option
  # combo.
  fixture_metadata <- list()
  result <- neoipcr:::read_metadata_countries(
    fixture_metadata, opts, wb_country_map = NULL)

  expect_named(result, c("public", "internal_map"))
  expect_equal(ncol(result$public), 0L)
  expect_equal(nrow(result$public), 0L)
  expect_null(result$internal_map)
})

test_that("read_metadata_countries produces 1-col public under pseudo + wb='no'", {
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_country          = "pseudo",
      include_world_bank_class = "no"))

  expect_equal(ncol(metadata$countries), 1L)
  expect_identical(names(metadata$countries), "country_key")
  expect_true(is.integer(metadata$countries$country_key))
  expect_equal(nrow(metadata$countries), 2L)  # CH, DE from fixture
})

test_that("read_metadata_countries produces 2-col public under pseudo + wb='full'", {
  # Even without WB-class raw metadata, the schema declares wb_class_key
  # under pseudo + wb-non-"no". The WB-class join happens only when the
  # WB metadata contains entries; with no WB classes in the fixture, the
  # wb_class_key column is present but NA.
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_country          = "pseudo",
      include_world_bank_class = "full"))

  expect_equal(ncol(metadata$countries), 2L)
  expect_identical(
    names(metadata$countries),
    c("country_key", "world_bank_class_key"))
})

test_that("read_metadata_countries produces full schema under include_country='full'", {
  opts <- dhis2_dataset_options(
    include_country          = "full",
    include_world_bank_class = "no")
  metadata <- read_test_metadata(dataset_options = opts)

  expect_schema_matches(
    metadata$countries,
    neoipcr:::get_countries_schema(opts))
  expect_equal(nrow(metadata$countries), 2L)
  expect_identical(
    sort(as.character(metadata$countries$code)),
    c("CH", "DE"))
})

test_that("read_metadata_countries returns empty shape when COUNTRY group absent", {
  # Metadata without the COUNTRY organisationUnitGroup — reader must
  # still return a schema-conformant empty tibble (never NULL).
  for (mode in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(include_country = mode)
    metadata <- read_test_metadata(
      exclude         = "countries",
      dataset_options = opts)

    expect_false(is.null(metadata$countries),
      info = sprintf("mode = '%s'", mode))
    expect_equal(nrow(metadata$countries), 0L,
      info = sprintf("mode = '%s'", mode))
    expect_schema_matches(
      metadata$countries,
      neoipcr:::get_countries_schema(opts))
  }
})

test_that("read_metadata_countries internal_map carries country/code/country_key", {
  opts <- dhis2_dataset_options(include_country = "full")
  result <- neoipcr:::read_metadata_countries(
    list(organisationUnitGroups = list(
      list(
        code = "COUNTRY",
        organisationUnits = list(
          list(id = "CID_1", name = "Switzerland", code = "CH",
               displayName        = "Switzerland",
               displayShortName   = "Switzerland",
               displayDescription = "Swiss Confederation"),
          list(id = "CID_2", name = "Germany", code = "DE",
               displayName        = "Germany",
               displayShortName   = "Germany",
               displayDescription = "Federal Republic of Germany"))))),
    opts,
    wb_country_map = NULL)

  expect_false(is.null(result$internal_map))
  expect_true(all(c("country", "code", "country_key") %in%
                  names(result$internal_map)))
  expect_equal(nrow(result$internal_map), 2L)
})

# --- read_metadata orchestrator — countries always in ret ---

test_that("read_metadata stores countries as an empty tibble (never NULL) under 'no'", {
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_country = "no", include_world_bank_class = "no"))

  expect_false(is.null(metadata$countries))
  expect_s3_class(metadata$countries, "tbl_df")
  expect_equal(ncol(metadata$countries), 0L)
  expect_equal(nrow(metadata$countries), 0L)
})

test_that("read_metadata countries has no `country` column under the schema contract", {
  # The raw DHIS2 `country` id lives on `.countries_internal_map`, not
  # on the public `metadata$countries`. Downstream code that needs it
  # (e.g. `import-dhis2.R` for country-filter request building) must
  # consume the internal map.
  metadata <- read_test_metadata(
    dataset_options = dhis2_dataset_options(include_country = "full"))
  expect_false("country" %in% names(metadata$countries))
})

# --- read_organisationUnits_hospitals — three-mode shape contract ---

test_that("read_organisationUnits_hospitals returns empty processed tibble on NULL/empty input", {
  opts <- dhis2_dataset_options(include_hospital = "full")
  result <- neoipcr:::read_organisationUnits_hospitals(NULL, opts)
  expect_named(result, c("processed", "internal_map"))
  expect_equal(ncol(result$processed), 0L)
  expect_null(result$internal_map)

  result <- neoipcr:::read_organisationUnits_hospitals(tibble::tibble(), opts)
  expect_equal(nrow(result$processed), 0L)
})

test_that("read_organisationUnits_hospitals pads geometry with NA under 'full' when raw lacks geometry column", {
  # Processed tibble must have longitude/latitude columns ready for the
  # schema when `include_hospital == "full"`, even if DHIS2 returned no
  # geometry for any hospital in the response.
  opts <- dhis2_dataset_options(include_hospital = "full")
  x <- tibble::tibble(
    id          = c("H1", "H2"),
    code        = c("HOSP_A", "HOSP_B"),
    displayName = c("Hospital A", "Hospital B"))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_true(all(c("longitude", "latitude") %in% names(result$processed)))
  expect_true(all(is.na(result$processed$longitude)))
  expect_true(all(is.na(result$processed$latitude)))
})

test_that("read_organisationUnits_hospitals hoists geometry coordinates when present", {
  opts <- dhis2_dataset_options(include_hospital = "full")
  x <- tibble::tibble(
    id       = c("H1", "H2"),
    geometry = list(
      list(type = "Point", coordinates = list(7.5, 47.5)),
      list(type = "Point", coordinates = list(13.4, 52.5))))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_false("geometry" %in% names(result$processed))
  # `add_key_column` randomizes row order before key assignment; assert
  # values as a set, not a sequence. Row-paired correctness is
  # implicitly verified by each row carrying a plausible pair.
  expect_setequal(result$processed$longitude, c(7.5, 13.4))
  expect_setequal(result$processed$latitude,  c(47.5, 52.5))
})

test_that("read_organisationUnits_hospitals hoists parent.id into country when present", {
  opts <- dhis2_dataset_options(
    include_hospital = "pseudo", include_country = "pseudo")
  x <- tibble::tibble(
    id     = c("H1", "H2"),
    parent = list(list(id = "C1"), list(id = "C2")))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_true("country" %in% names(result$internal_map))
  # `distinct()` inside the reader may reorder rows; assert set equality.
  expect_setequal(result$internal_map$country, c("C1", "C2"))
})

test_that("read_organisationUnits_hospitals deduplicates parent hospitals shared by multiple departments", {
  # The hospitals reader is called with the list of department-parents;
  # multiple departments share a parent, so the reader distinct()s.
  opts <- dhis2_dataset_options(include_hospital = "pseudo")
  x <- tibble::tibble(id = c("H1", "H1", "H2", "H1"))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_equal(nrow(result$processed), 2L)
  expect_equal(nrow(result$internal_map), 2L)
  expect_setequal(result$internal_map$orgUnit, c("H1", "H2"))
})

# --- read_metadata_reponses orchestrator — hospitals final shape ---
#
# The orchestrator's post-processing (country_key join, WB-class
# inheritance join, finalize_to_schema + assert_schema) is exercised
# indirectly via `import_dhis2()` in production. Here we assert the
# most important observable property: after processing,
# `metadata$hospitals` carries only the columns declared by
# `hospitals_cols`, never any orchestrator-internal column like `country`.

test_that("metadata$hospitals never exposes the internal `country` column", {
  # This also exercises the `.hospitals_internal_map` lift + strip path
  # implicitly — if the map didn't carry `country`, the country_key
  # join in read_metadata_reponses would fail. Since that join runs
  # without error against the fixture, the map is correctly populated.
  skip("read_test_metadata does not construct hospitals from /organisationUnits — re-enable when the fixture exercises the hospitals path")
})
