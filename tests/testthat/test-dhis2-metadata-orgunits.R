# Tests for R/dhis2-metadata-orgunits.R — the /organisationUnits request
# builder and the org-unit readers, with the custom-attribute values. The
# offline mock in helper-dhis2-mock.R serves fixtures regardless of the
# requested `fields`, so request-shape assertions are made directly on the
# request builders. The hospital / department readers' three-mode shapes
# are covered in test-dhis2-metadata.R; this file adds the attribute path.

ou_request_fields <- function(opts)
  httr2::url_parse(
    neoipcr:::get_organisationUnit_request(
      httr2::request("https://dhis2.example.org/api"), NULL, opts)$url
  )$query$fields

attribute_fragment <- "attributeValues[attribute[id],value]"

count_fragment <- function(fields)
  lengths(regmatches(
    fields, gregexpr(attribute_fragment, fields, fixed = TRUE)))

opts_label <- function(opts)
  sprintf(
    "include_department='%s', include_hospital='%s', include_country='%s', include_custom_attributes=[%s]",
    opts$include_department, opts$include_hospital, opts$include_country,
    paste(opts$include_custom_attributes, collapse = ","))

# A raw DHIS2 attribute value as `resp_body_json()` yields it.
raw_values <- function(id, value)
  list(list(attribute = list(id = id), value = value))

# --- get_organisationUnit_request ---

test_that("the department block requests attribute values only when departments are opted in and present", {
  for (opts in iter_dataset_options(c(
    "include_department", "include_custom_attributes"))) {
    fields <- ou_request_fields(opts)
    dept_opted <-
      "departments" %in% opts$include_custom_attributes &&
      opts$include_department != "no"
    expect_equal(
      startsWith(fields, paste0("id,", attribute_fragment)), dept_opted,
      info = opts_label(opts))
  }
})

test_that("the hospital block requests attribute values only when opted in and hospitals are present", {
  for (opts in iter_dataset_options(c(
    "include_department", "include_hospital", "include_country",
    "include_custom_attributes"))) {
    fields <- ou_request_fields(opts)
    dept_opted <-
      "departments" %in% opts$include_custom_attributes &&
      opts$include_department != "no"
    hospital_opted <-
      "hospitals" %in% opts$include_custom_attributes &&
      opts$include_hospital != "no"

    expect_equal(
      count_fragment(fields), sum(dept_opted, hospital_opted),
      info = opts_label(opts))
    if (hospital_opted)
      expect_match(
        fields, "parent\\[.*attributeValues\\[attribute\\[id\\],value\\]",
        info = opts_label(opts))

    chars <- strsplit(fields, "", fixed = TRUE)[[1]]
    expect_equal(sum(chars == "["), sum(chars == "]"), info = opts_label(opts))
  }
})

test_that("get_test_unit_attribute_request asks for ids only, within the user's hierarchy, filtered by the attribute's value", {
  req <- neoipcr:::get_test_unit_attribute_request(
    httr2::request("https://dhis2.example.org/api"), "ATTR_FLAG_01")
  url <- httr2::url_parse(req$url)

  expect_true(endsWith(url$path, "/organisationUnits"))
  expect_equal(url$query$fields, "id")
  expect_equal(url$query$withinUserHierarchy, "true")
  expect_setequal(
    unlist(url$query[names(url$query) == "filter"]),
    c("organisationUnitGroups.code:eq:NEO_DEPARTMENT", "ATTR_FLAG_01:eq:true"))
})

test_that("read_test_unit_attribute_ids yields the ids, or nothing", {
  expect_equal(
    neoipcr:::read_test_unit_attribute_ids(
      list(organisationUnits = list(list(id = "OU_1"), list(id = "OU_2")))),
    c("OU_1", "OU_2"))
  expect_equal(
    neoipcr:::read_test_unit_attribute_ids(list(organisationUnits = list())),
    character())
  expect_equal(neoipcr:::read_test_unit_attribute_ids(list()), character())
})

test_that("get_metadata_request always requests the org-unit attribute definitions", {
  for (opts in iter_dataset_options(c(
    "include_department", "include_custom_attributes"))) {
    query <- httr2::url_parse(
      neoipcr:::get_metadata_request(
        httr2::request("https://dhis2.example.org/api"),
        list(authorities = character()), opts)$url)$query
    expect_equal(
      query[["attributes:fields"]], "id,code,name,valueType",
      info = opts_label(opts))
    expect_equal(
      query[["attributes:filter"]], "organisationUnitAttribute:eq:true",
      info = opts_label(opts))
  }
})

# --- read_organisationUnit_attribute_values ---

test_that("read_organisationUnit_attribute_values unnests the list column into the raw long form", {
  processed <- tibble::tibble(
    department_key  = c(1L, 2L, 3L),
    attributeValues = list(
      c(raw_values("ATTR_A", "x"), raw_values("ATTR_B", "2024-01-31")),
      list(),
      raw_values("ATTR_A", "y")))
  result <- neoipcr:::read_organisationUnit_attribute_values(
    processed, "department_key")

  expect_named(result, c("department_key", "attribute", "value"))
  expect_equal(nrow(result), 3L)
  expect_true(is.character(result$value))
  expect_true(is.character(result$attribute))
  expect_setequal(
    result$attribute[result$department_key == 1L], c("ATTR_A", "ATTR_B"))
  expect_equal(result$value[result$department_key == 3L], "y")
  expect_false(2L %in% result$department_key)
})

test_that("read_organisationUnit_attribute_values yields the empty shape when the column is absent or every list is empty", {
  absent <- neoipcr:::read_organisationUnit_attribute_values(
    tibble::tibble(hospital_key = 1:2), "hospital_key")
  expect_named(absent, c("hospital_key", "attribute", "value"))
  expect_equal(nrow(absent), 0L)
  expect_true(is.integer(absent$hospital_key))

  empty <- neoipcr:::read_organisationUnit_attribute_values(
    tibble::tibble(hospital_key = 1:2, attributeValues = list(list(), list())),
    "hospital_key")
  expect_named(empty, c("hospital_key", "attribute", "value"))
  expect_equal(nrow(empty), 0L)

  # What the reader actually receives from a response in which every org unit
  # serializes an empty array: widening the parsed units turns the column into
  # logical NA, one per org unit, not into a list of empty lists.
  widened <- tibble::tibble(units = list(
    list(id = "OU_1", attributeValues = list()),
    list(id = "OU_2", attributeValues = list()))) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(hospital_key = 1:2)
  expect_type(widened$attributeValues, "logical")
  all_empty <- neoipcr:::read_organisationUnit_attribute_values(
    widened, "hospital_key")
  expect_named(all_empty, c("hospital_key", "attribute", "value"))
  expect_equal(nrow(all_empty), 0L)
})

# --- readers: attribute values are split off, the list column stripped ---

test_that("read_organisationUnits_hospitals dedupes repeated parents that carry attribute values and strips the list column", {
  opts <- dhis2_dataset_options(
    include_hospital = "full", include_custom_attributes = "hospitals")
  parent_values <- raw_values("ATTR_A", "Hospital text")
  x <- tibble::tibble(
    id              = c("H1", "H1", "H2"),
    code            = c("HOSP_1", "HOSP_1", "HOSP_2"),
    attributeValues = list(parent_values, parent_values, list()))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_named(result, c("processed", "internal_map", "attribute_values"))
  expect_equal(nrow(result$processed), 2L)
  expect_false("attributeValues" %in% names(result$processed))
  expect_false("attributeValues" %in% names(result$internal_map))
  expect_equal(nrow(result$attribute_values), 1L)
  h1_key <- result$processed$hospital_key[result$processed$orgUnit == "H1"]
  expect_equal(result$attribute_values$hospital_key, h1_key)
  expect_equal(result$attribute_values$attribute, "ATTR_A")
  expect_equal(result$attribute_values$value, "Hospital text")
})

test_that("read_organisationUnits_hospitals dedupes on the id, whatever order a copy lists its attribute values in", {
  opts <- dhis2_dataset_options(
    include_hospital = "full", include_custom_attributes = "hospitals")
  values <- c(raw_values("ATTR_A", "Hospital text"), raw_values("ATTR_B", "2024-01-31"))
  x <- tibble::tibble(
    id              = c("H1", "H1"),
    code            = c("HOSP_1", "HOSP_1"),
    attributeValues = list(values, rev(values)))
  result <- neoipcr:::read_organisationUnits_hospitals(x, opts)

  expect_equal(nrow(result$processed), 1L)
  expect_equal(nrow(result$attribute_values), 2L)
  expect_setequal(result$attribute_values$attribute, c("ATTR_A", "ATTR_B"))
})

test_that("read_organisationUnits_departments returns the attribute values and a processed tibble that still finalizes without scratch", {
  opts <- dhis2_dataset_options(
    include_department = "full", include_custom_attributes = "departments")
  x <- tibble::tibble(
    id              = c("D1", "D2"),
    code            = c("DEPT_1", "DEPT_2"),
    attributeValues = list(raw_values("ATTR_A", "text"), list()))
  result <- neoipcr:::read_organisationUnits_departments(x, list(), opts)

  expect_named(result, c("processed", "internal_map", "attribute_values"))
  expect_false("attributeValues" %in% names(result$processed))
  expect_no_error(neoipcr:::finalize_to_schema(
    result$processed, neoipcr:::departments_cols, opts))
  d1_key <- result$processed$department_key[result$processed$orgUnit == "D1"]
  expect_equal(result$attribute_values$department_key, d1_key)
  expect_equal(result$attribute_values$value, "text")
})

# --- resolve_organisationUnit_attribute_values ---

definitions_map <- tibble::tibble(
  attribute = c("ATTR_A", "ATTR_D", "ATTR_F"),
  code      = c("TEST_TEXT", "TEST_DATE", "IsTestunit"),
  valueType = c("TEXT", "DATE", "TRUE_ONLY"))

typed_columns <- c(
  "value_text", "value_logical", "value_integer", "value_number",
  "value_date", "value_datetime")

test_that("resolve_organisationUnit_attribute_values maps UIDs to codes, types the values and drops what it cannot resolve", {
  values <- tibble::tibble(
    department_key = c(1L, 1L, 2L, 3L),
    attribute      = c("ATTR_A", "ATTR_D", "ATTR_UNKNOWN", "ATTR_F"),
    value          = c("some text", "2024-08-03T00:00:00.000", "dropped", "true"))
  # Department 3 did not survive the metadata narrowing.
  parents <- tibble::tibble(department_key = c(1L, 2L))

  result <- neoipcr:::resolve_organisationUnit_attribute_values(
    values, definitions_map, "department_key", parents,
    "departmentAttributeValues")

  expect_named(result, c("department_key", "attribute_code", typed_columns))
  expect_equal(nrow(result), 2L)
  expect_setequal(result$attribute_code, c("TEST_TEXT", "TEST_DATE"))
  expect_equal(
    result$value_text[result$attribute_code == "TEST_TEXT"], "some text")
  expect_equal(
    result$value_date[result$attribute_code == "TEST_DATE"],
    as.Date("2024-08-03"))
  expect_true(is.na(result$value_text[result$attribute_code == "TEST_DATE"]))
  expect_false("ATTR_UNKNOWN" %in% result$attribute_code)
})

test_that("resolve_organisationUnit_attribute_values drops rows outside `parents` before spreading, so a pruned org unit's value never warns", {
  values <- tibble::tibble(
    department_key = c(1L, 2L),
    attribute      = c("ATTR_D", "ATTR_D"),
    value          = c("2024-08-03", "not a date"))
  parents <- tibble::tibble(department_key = 1L)

  expect_no_warning(
    result <- neoipcr:::resolve_organisationUnit_attribute_values(
      values, definitions_map, "department_key", parents,
      "departmentAttributeValues"),
    class = "neoipcr_attribute_value_parse_failure")
  expect_equal(result$department_key, 1L)
  expect_equal(result$value_date, as.Date("2024-08-03"))
})

test_that("resolve_organisationUnit_attribute_values keeps every org unit when no parents are given and tolerates NULL inputs", {
  values <- tibble::tibble(
    hospital_key = c(1L, 2L),
    attribute    = c("ATTR_F", "ATTR_F"),
    value        = c("true", "true"))
  unpruned <- neoipcr:::resolve_organisationUnit_attribute_values(
    values, definitions_map, "hospital_key", NULL, "hospitalAttributeValues")
  expect_equal(nrow(unpruned), 2L)
  expect_true(all(unpruned$value_logical))

  empty <- neoipcr:::resolve_organisationUnit_attribute_values(
    NULL, NULL, "hospital_key", NULL, "hospitalAttributeValues")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("hospital_key", "attribute_code", typed_columns))
})

# --- value_type_family / spread_typed_values ---

test_that("value_type_family follows DHIS2's ValueType families and degrades unknown types to text", {
  family <- neoipcr:::value_type_family
  expect_equal(
    family(c("INTEGER", "INTEGER_POSITIVE", "INTEGER_NEGATIVE",
             "INTEGER_ZERO_OR_POSITIVE")),
    rep("integer", 4))
  expect_equal(family(c("NUMBER", "UNIT_INTERVAL", "PERCENTAGE")), rep("number", 3))
  expect_equal(family(c("BOOLEAN", "TRUE_ONLY")), rep("logical", 2))
  expect_equal(family(c("DATE", "AGE")), rep("date", 2))
  expect_equal(family("DATETIME"), "datetime")
  expect_equal(
    family(c("TEXT", "LONG_TEXT", "LETTER", "TIME", "USERNAME", "EMAIL",
             "PHONE_NUMBER", "URL", "MULTI_TEXT", "FILE_RESOURCE", "IMAGE",
             "COORDINATE", "GEOJSON", "ORGANISATION_UNIT", "REFERENCE",
             "TRACKER_ASSOCIATE")),
    rep("text", 16))
  expect_equal(family(c("SOMETHING_NEW", NA)), c("text", "text"))
  expect_equal(family(character()), character())
})

test_that("spread_typed_values fills exactly the column of each value's family", {
  tbl <- tibble::tibble(
    attribute_code = c("T", "L", "L2", "I", "N", "D", "D2", "DT"),
    valueType = c("TEXT", "TRUE_ONLY", "BOOLEAN", "INTEGER", "PERCENTAGE",
                  "DATE", "AGE", "DATETIME"),
    value = c("hello", "true", "false", "12", "12.5",
              "2024-08-03T00:00:00.000", "2020-02-29",
              "2024-08-03T10:30:00.000"))
  result <- neoipcr:::spread_typed_values(tbl, code_col = "attribute_code")

  expect_named(result, c("attribute_code", typed_columns))
  expect_equal(
    rowSums(!is.na(result[, typed_columns])), rep(1, 8), ignore_attr = TRUE)
  expect_equal(result$value_text[1], "hello")
  expect_equal(result$value_logical[2:3], c(TRUE, FALSE))
  expect_identical(result$value_integer[4], 12L)
  expect_equal(result$value_number[5], 12.5)
  expect_equal(result$value_date[6:7], as.Date(c("2024-08-03", "2020-02-29")))
  expect_equal(
    result$value_datetime[8], as.POSIXct("2024-08-03 10:30:00", tz = "UTC"))
  expect_equal(attr(result$value_datetime, "tzone"), "UTC")
})

test_that("spread_typed_values sets an unparseable value to NA and warns once, by attribute code and count", {
  tbl <- tibble::tibble(
    attribute_code = c("D", "D", "I", "T", "L", "N"),
    valueType      = c("DATE", "DATE", "INTEGER", "TEXT", "TRUE_ONLY", "DATE"),
    value          = c("not a date", "2024-01-01", "twelve", "fine", "yes",
                       NA_character_))

  expect_warning(
    result <- neoipcr:::spread_typed_values(tbl, code_col = "attribute_code"),
    class = "neoipcr_attribute_value_parse_failure")
  expect_true(is.na(result$value_date[1]))
  expect_equal(result$value_date[2], as.Date("2024-01-01"))
  expect_true(is.na(result$value_integer[3]))
  expect_equal(result$value_text[4], "fine")
  # A boolean that is neither "true" nor "false" fails like any other family.
  expect_true(is.na(result$value_logical[5]))
  # An absent value is NA without being a failure.
  expect_true(is.na(result$value_date[6]))

  msg <- conditionMessage(tryCatch(
    neoipcr:::spread_typed_values(tbl, code_col = "attribute_code"),
    warning = identity))
  expect_match(msg, "D (1)", fixed = TRUE)
  expect_match(msg, "I (1)", fixed = TRUE)
  expect_match(msg, "L (1)", fixed = TRUE)
  expect_false(grepl("N (", msg, fixed = TRUE))
  # Never the value itself — it may be a person's name.
  expect_false(grepl("not a date", msg, fixed = TRUE))
})

test_that("spread_typed_values keeps the six typed columns on a 0-row input", {
  result <- neoipcr:::spread_typed_values(tibble::tibble(
    attribute_code = character(), valueType = character(), value = character()))
  expect_equal(nrow(result), 0L)
  expect_true(is.character(result$value_text))
  expect_true(is.logical(result$value_logical))
  expect_true(is.integer(result$value_integer))
  expect_true(is.double(result$value_number))
  expect_s3_class(result$value_date, "Date")
  expect_s3_class(result$value_datetime, "POSIXct")
})

# --- read_metadata_orgUnitAttributes ---

test_that("read_metadata_orgUnitAttributes reads the definitions when an entity is opted in", {
  md <- read_test_metadata(
    include = "org_unit_attributes",
    dataset_options = dhis2_dataset_options(
      include_department = "full", include_custom_attributes = "departments"))

  expect_named(md$orgUnitAttributes, c("code", "name", "valueType"))
  # The fixture's sixth definition has no code, so its values could never be
  # addressed: neither the public list nor the code map carries it.
  expect_equal(nrow(md$orgUnitAttributes), 5L)
  expect_true("IsTestunit" %in% md$orgUnitAttributes$code)
  expect_false(any(is.na(md$orgUnitAttributes$code)))
  expect_named(
    md$.orgUnitAttributes_internal_map, c("attribute", "code", "valueType"))
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 5L)
  expect_false(any(is.na(md$.orgUnitAttributes_internal_map$code)))
})

test_that("read_metadata_orgUnitAttributes keeps the code map while the public tibble is gated off", {
  md <- read_test_metadata(include = "org_unit_attributes")
  expect_equal(ncol(md$orgUnitAttributes), 0L)
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 5L)
})

test_that("read_metadata_orgUnitAttributes yields empty shapes when the payload carries no definitions", {
  # The baseline metadata fixture carries no definitions.
  md <- read_test_metadata(
    dataset_options = dhis2_dataset_options(
      include_department = "full", include_custom_attributes = "departments"))
  expect_named(md$orgUnitAttributes, c("code", "name", "valueType"))
  expect_equal(nrow(md$orgUnitAttributes), 0L)
  expect_equal(nrow(md$.orgUnitAttributes_internal_map), 0L)
})
