# Creates the organisationUnits query, which we use, so that we can apply the
# withinUserHierarchy filter
get_organisationUnit_request <- function(req_base, user_info, dataset_options)
{
  # Attribute values travel only for an opted-in entity that is present. The
  # `IsTestunit` flag does not need them: it arrives as org-unit ids from its
  # own narrowed request (see `get_test_unit_attribute_request()`).
  attribute_fields <- ",attributeValues[attribute[id],value]"
  fields <- "id"
  if ("departments" %in% dataset_options$include_custom_attributes &&
      dataset_options$include_department != "no")
    fields <- paste0(fields, attribute_fields)

  if(dataset_options$include_department == "full")
    fields <- paste0(fields, ",code,displayName,displayShortName,displayDescription,openingDate,comment,geometry")
  # We need the department code for filtering or to transform the supplied exceptions
  else if(has_exception_list(dataset_options) || length(dataset_options$department_filter) > 0)
    fields <- paste0(fields, ",code")

  # Hospital attribute values are fetched only on request. The hospital block
  # exists whenever hospitals are pseudonymized or the country / World Bank
  # hierarchy is wanted, so the opt-in is checked on its own.
  hospital_attribute_fields <-
    if ("hospitals" %in% dataset_options$include_custom_attributes &&
        dataset_options$include_hospital != "no")
      attribute_fields
    else
      ""

  if(length(dataset_options$country_filter) > 0 ||
     dataset_options$include_country != "no" ||
     dataset_options$include_world_bank_class != "no")
    country_fields <- ",parent[id]]"
  else
    country_fields <- "]"

  if(dataset_options$include_hospital == "full")
    fields <- paste0(fields, paste0(",parent[id,code,displayName,displayShortName,displayDescription,comment,geometry", hospital_attribute_fields, country_fields))
  else if (dataset_options$include_hospital == "pseudo" ||
           length(dataset_options$country_filter) > 0 ||
           dataset_options$include_country != "no" ||
           dataset_options$include_world_bank_class != "no")
    fields <- paste0(fields, paste0(",parent[id", hospital_attribute_fields, country_fields))

  req_base |>
    httr2::req_url_path_append("organisationUnits") |>
    httr2::req_url_query(
      withinUserHierarchy = "true",
      fields = fields,
      filter = "organisationUnitGroups.code:eq:NEO_DEPARTMENT")
}

# The departments flagged by the `IsTestunit` custom attribute, ids only: the
# same scope as `get_organisationUnit_request()` plus DHIS2's filter on one
# attribute's value, `<attribute uid>:eq:true` (see
# `get_test_unit_attribute_ids()`).
get_test_unit_attribute_request <- function(req_base, attribute_uid)
{
  req_base |>
    httr2::req_url_path_append("organisationUnits") |>
    httr2::req_url_query(
      withinUserHierarchy = "true",
      fields = "id",
      filter = c(
        "organisationUnitGroups.code:eq:NEO_DEPARTMENT",
        paste0(attribute_uid, ":eq:true")),
      .multi = "explode")
}

# The org-unit ids of a `get_test_unit_attribute_request()` response body.
read_test_unit_attribute_ids <- function(body)
{
  units <- body$organisationUnits
  if (length(units) == 0L)
    return(character())
  vapply(units, \(unit) as.character(unit$id), character(1))
}

read_organisationUnits <- function(organisationUnits, dataset_options)
{
  department_base <- tibble::tibble(units = organisationUnits$organisationUnits) |>
    tidyr::unnest_wider(1)

  ret <- list()

  # The parent of the department is the hospital
  if("parent" %in% names(department_base)) {
    hospital_base <- tibble::tibble(hospital = department_base$parent) |>
      tidyr::unnest_wider(1)

    hospitals_result <- read_organisationUnits_hospitals(
      hospital_base, dataset_options)
    ret$hospitals               <- hospitals_result$processed
    ret$.hospitals_internal_map <- hospitals_result$internal_map
    ret$hospitalAttributeValues <- hospitals_result$attribute_values
  }

  departments_result <- read_organisationUnits_departments(
    department_base,
    ret,
    dataset_options)
  ret$departments               <- departments_result$processed
  ret$.departments_internal_map <- departments_result$internal_map
  ret$departmentAttributeValues <- departments_result$attribute_values

  ret
}

# Read hospital rows from the parent-of-department block of the
# /organisationUnits response.
#
# Returns a named list with two components:
#   * `processed`    — transformed tibble carrying every column the
#                      orchestrator needs to finish building the public
#                      hospitals tibble: `hospital_key`, `orgUnit`, any
#                      display / geometry fields under "full", and the
#                      raw `country` DHIS2 id (used by the orchestrator's
#                      country_key join). `metadata$hospitals` starts as
#                      this tibble and is narrowed to
#                      `compile_schema(hospitals_cols, opts)` in
#                      `assemble_metadata()` once the country_key
#                      join has added its column.
#   * `internal_map` — lookup subset with `hospital_key`, `orgUnit`, and
#                      `country` (when available). Used by
#                      `read_organisationUnits_departments()` for the
#                      dept→hospital join, and by
#                      `assemble_metadata()` for the country_key
#                      lookup and the WB-class inheritance path under
#                      `include_country = "no"`. Threaded through
#                      `metadata$.hospitals_internal_map` and stripped at
#                      `import_dhis2()` exit.
#   * `attribute_values` — the raw custom-attribute values keyed by
#                      `hospital_key` (see
#                      `read_organisationUnit_attribute_values()`), resolved
#                      and typed by the orchestrator.
read_organisationUnits_hospitals <- function(x, dataset_options)
{
  opts <- dataset_options
  empty_result <- list(
    processed        = tibble::tibble(),
    internal_map     = NULL,
    attribute_values = empty_attribute_values("hospital_key")
  )

  if (is.null(x) || nrow(x) < 1L)
    return(empty_result)

  # Hoist geometry when present; otherwise pad with NA under "full" so
  # the schema's longitude/latitude columns are populated either way.
  if ("geometry" %in% names(x)) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (opts$include_hospital == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  # Hoist the parent reference — for hospitals, the parent is the
  # country. Present in the raw response only when country / WB-class
  # info is requested (see `get_organisationUnit_request`).
  if ("parent" %in% names(x))
    x <- x |> tidyr::hoist("parent", country = "id")

  # The parent block repeats once per department. Every copy of a hospital is
  # the same entity, so the first row per id is kept; deduplicating on the
  # whole row would make the result depend on the order in which DHIS2 lists
  # a copy's attribute values, which nothing in the API promises.
  processed <- x[!duplicated(x$id), ] |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("hospital_key")

  attribute_values <- read_organisationUnit_attribute_values(
    processed, "hospital_key")
  processed <- processed |>
    dplyr::select(!tidyselect::any_of("attributeValues"))

  internal_map <- processed |>
    dplyr::select(tidyselect::any_of(c("hospital_key", "orgUnit", "country")))

  list(
    processed        = processed,
    internal_map     = internal_map,
    attribute_values = attribute_values)
}

read_organisationUnits_departments <- function(x, y, dataset_options) {

  # Dept → hospital join uses the orchestrator-internal hospitals map
  # (not `y$hospitals` directly), because `metadata$hospitals` is later
  # narrowed to the public schema which may strip `orgUnit` when
  # `"hospitals" %not in% include_dhis2_ids`. The map always carries
  # `hospital_key` + `orgUnit` for this join.
  if(!is.null(y$.hospitals_internal_map) &&
     "orgUnit" %in% names(y$.hospitals_internal_map)){
    x <- x |>
      tidyr::hoist("parent", orgUnit = "id") |>
      dplyr::left_join(
        y$.hospitals_internal_map |>
          dplyr::select("orgUnit", "hospital_key"),
        dplyr::join_by("orgUnit")) |>
      dplyr::select(!c("orgUnit","parent"))
  }

  cols <- names(x)
  if("openingDate" %in% cols)
    x <- x |>
      dplyr::mutate(
        openingDate =  readr::parse_date(
          stringr::str_sub(.data$openingDate, end = 10)))

  # Hoist geometry when present; otherwise pad NA under "full" so the
  # schema's longitude/latitude columns are populated either way.
  if("geometry" %in% cols) {
    x <- x |>
      tidyr::hoist(
        "geometry",
        longitude = list("coordinates", 1),
        latitude  = list("coordinates", 2)) |>
      dplyr::select(!"geometry")
  } else if (dataset_options$include_department == "full") {
    x <- x |> dplyr::mutate(
      longitude = NA_real_,
      latitude  = NA_real_)
  }

  processed <- x |>
    dplyr::relocate("orgUnit" = "id") |>
    add_key_column("department_key")

  attribute_values <- read_organisationUnit_attribute_values(
    processed, "department_key")
  processed <- processed |>
    dplyr::select(!tidyselect::any_of("attributeValues"))

  internal_map <- processed |>
    dplyr::select("department_key", "orgUnit")

  list(
    processed        = processed,
    internal_map     = internal_map,
    attribute_values = attribute_values)
}

# The empty shape of a raw attribute-values tibble: one row per (org unit,
# attribute) with the attribute's DHIS2 UID and the value as DHIS2 serializes
# it, a string.
empty_attribute_values <- function(key_col)
  tibble::tibble(
    !!key_col := integer(),
    attribute = character(),
    value = character())

# Split the `attributeValues` list column of a keyed org-unit tibble into the
# raw long form: `<key_col>`, `attribute` (the attribute UID) and `value`.
# An org unit without values serializes an empty array, which
# `unnest_longer()` drops; a response that omits the column altogether (a
# fixture, or a request that did not ask for it) yields the empty shape, and
# so does one in which every org unit's array is empty: `unnest_wider()`
# then delivers the column as logical `NA` rather than as a list, which
# `unnest_longer()` would keep as one row per org unit with nothing to widen.
read_organisationUnit_attribute_values <- function(processed, key_col)
{
  if (!("attributeValues" %in% names(processed)) ||
      !is.list(processed$attributeValues))
    return(empty_attribute_values(key_col))

  values <- processed |>
    dplyr::select(tidyselect::all_of(c(key_col, "attributeValues"))) |>
    tidyr::unnest_longer("attributeValues")

  if (nrow(values) == 0L)
    return(empty_attribute_values(key_col))

  values <- values |>
    tidyr::unnest_wider("attributeValues") |>
    tidyr::unnest_wider("attribute", names_sep = "_")

  if (!("value" %in% names(values)))
    values$value <- NA_character_

  values |>
    dplyr::mutate(
      attribute = as.character(.data$attribute_id),
      value = as.character(.data$value)) |>
    dplyr::select(tidyselect::all_of(c(key_col, "attribute", "value")))
}

# Resolve raw attribute values to their public, typed shape: the attribute
# UID becomes `attribute_code` through the definitions map, rows whose
# attribute is not in that map are dropped, rows whose org unit is not among
# `parents` are dropped, and the string value is spread into the typed
# `value_*` columns by the attribute's value type. Rows not in `parents`
# are dropped before the spread, so only surviving org units can raise a
# parse-failure warning.
#
# A value without a definition is not an error: DHIS2 serializes a value even
# when the caller cannot read the attribute's definition (the contact-person
# attributes are shared privately), and such a value has no code to be
# addressed by. Only counts are logged — the value may be a person's name.
resolve_organisationUnit_attribute_values <- function(
    values, definitions_map, key_col, parents, entity_name)
{
  if (is.null(values))
    values <- empty_attribute_values(key_col)
  if (is.null(definitions_map))
    definitions_map <- tibble::tibble(
      attribute = character(), code = character(), valueType = character())

  unmatched <- values |>
    dplyr::anti_join(definitions_map, dplyr::join_by("attribute"))
  if (nrow(unmatched) > 0L)
    logger::log_debug(
      "{entity_name}: dropped {nrow(unmatched)} attribute value(s) on {dplyr::n_distinct(unmatched$attribute)} attribute(s) without a readable definition",
      namespace = "neoipcr")

  resolved <- values |>
    dplyr::inner_join(definitions_map, dplyr::join_by("attribute")) |>
    dplyr::rename(attribute_code = "code")

  if (!is.null(parents) && key_col %in% names(parents))
    resolved <- resolved |>
      dplyr::semi_join(parents, by = key_col)

  resolved |>
    spread_typed_values(
      value_col = "value", type_col = "valueType", code_col = "attribute_code") |>
    dplyr::select(
      tidyselect::all_of(c(key_col, "attribute_code")),
      tidyselect::starts_with("value_"))
}

# Map a DHIS2 value type to the typed column that stores it, following the
# families DHIS2 itself declares in `ValueType.java` (2.40 and 2.41):
#   INTEGER_TYPES  INTEGER, INTEGER_POSITIVE, INTEGER_NEGATIVE,
#                  INTEGER_ZERO_OR_POSITIVE                    → "integer"
#   DECIMAL_TYPES  NUMBER, UNIT_INTERVAL, PERCENTAGE            → "number"
#   BOOLEAN_TYPES  BOOLEAN, TRUE_ONLY                           → "logical"
#   DATE_TYPES     DATE, AGE (a date of birth)                  → "date"
#                  DATETIME                                     → "datetime"
# Every other type — the text types (TEXT, LONG_TEXT, LETTER, TIME, USERNAME,
# EMAIL, PHONE_NUMBER, URL), MULTI_TEXT, and the file, geo, reference and
# tracker kinds — keeps the string, and so does any value type this package
# does not know: a value type added upstream degrades to text, not to an
# error.
value_type_family <- function(value_type)
{
  dplyr::case_when(
    value_type %in% c("INTEGER", "INTEGER_POSITIVE", "INTEGER_NEGATIVE",
                      "INTEGER_ZERO_OR_POSITIVE") ~ "integer",
    value_type %in% c("NUMBER", "UNIT_INTERVAL", "PERCENTAGE") ~ "number",
    value_type %in% c("BOOLEAN", "TRUE_ONLY") ~ "logical",
    value_type %in% c("DATE", "AGE") ~ "date",
    value_type == "DATETIME" ~ "datetime",
    .default = "text")
}

# Spread string values into one typed column per value-type family —
# `value_text`, `value_logical`, `value_integer`, `value_number`, `value_date`
# and `value_datetime` — replacing `value_col` and `type_col`. Each row fills
# at most the column of its family and is NA elsewhere.
#
# A value that does not parse under its family becomes NA in every typed
# column and is reported once, by count per `code_col` (or in total when the
# tibble has no such column) — never by value, which may be a person's name.
# `parse_date()` reads only the date part, since DHIS2 stores a DATE either
# bare or with a midnight time suffix; `parse_datetime()` reads ISO 8601 as
# UTC.
spread_typed_values <- function(
    tbl, value_col = "value", type_col = "valueType", code_col = NULL)
{
  value  <- as.character(tbl[[value_col]])
  family <- value_type_family(as.character(tbl[[type_col]]))
  n      <- length(value)

  is_text <- family == "text"
  is_lgl  <- family == "logical"
  is_int  <- family == "integer"
  is_num  <- family == "number"
  is_date <- family == "date"
  is_dt   <- family == "datetime"

  value_text <- rep(NA_character_, n)
  value_text[is_text] <- value[is_text]

  value_logical <- rep(NA, n)
  lgl_raw <- tolower(value[is_lgl])
  value_logical[is_lgl] <- ifelse(
    lgl_raw == "true", TRUE, ifelse(lgl_raw == "false", FALSE, NA))

  value_integer <- rep(NA_integer_, n)
  value_integer[is_int] <- suppressWarnings(readr::parse_integer(value[is_int]))

  value_number <- rep(NA_real_, n)
  value_number[is_num] <- suppressWarnings(readr::parse_double(value[is_num]))

  value_date <- rep(as.Date(NA), n)
  value_date[is_date] <- suppressWarnings(
    readr::parse_date(stringr::str_sub(value[is_date], end = 10)))

  value_datetime <- rep(as.POSIXct(NA_real_, tz = "UTC"), n)
  value_datetime[is_dt] <- suppressWarnings(readr::parse_datetime(value[is_dt]))

  failed <- !is.na(value) & (
    (is_lgl  & is.na(value_logical)) |
    (is_int  & is.na(value_integer)) |
    (is_num  & is.na(value_number)) |
    (is_date & is.na(value_date)) |
    (is_dt   & is.na(value_datetime)))

  if (any(failed)) {
    if (!is.null(code_col) && code_col %in% names(tbl)) {
      counts <- table(tbl[[code_col]][failed])
      detail <- sprintf("%s (%d)", names(counts), as.integer(counts))
    } else
      detail <- sprintf("%d value(s)", sum(failed))
    rlang::warn(c(
      "Custom attribute value(s) that do not parse under their attribute's value type were set to NA:",
      rlang::set_names(detail, rep("x", length(detail)))),
      class = "neoipcr_attribute_value_parse_failure")
  }

  tbl |>
    dplyr::select(!tidyselect::all_of(c(value_col, type_col))) |>
    dplyr::bind_cols(
      tibble::tibble(
        value_text     = value_text,
        value_logical  = value_logical,
        value_integer  = value_integer,
        value_number   = value_number,
        value_date     = value_date,
        value_datetime = value_datetime))
}
