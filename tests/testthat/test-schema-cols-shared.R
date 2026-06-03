# Tests for R/schema-cols-shared.R — cross-entity column declarations,
# the inheritance helper, and the two attribute-column helpers.

# Shared opts builder used across tests.
opts_with <- function(...) dhis2_dataset_options(...)

# ---- Link keys -----------------------------------------------------------

test_that("col_patient_key is included iff include_patient != 'no'", {
  for (mode in c("no", "pseudo", "full"))
    expect_identical(
      neoipcr:::col_patient_key$include_when(opts_with(include_patient = mode)),
      mode != "no",
      info = paste("mode:", mode))
})

test_that("col_enrollment_key is included iff include_enrollment != 'no'", {
  for (mode in c("no", "pseudo", "full"))
    expect_identical(
      neoipcr:::col_enrollment_key$include_when(
        opts_with(include_enrollment = mode)),
      mode != "no")
})

test_that("col_event_key is included iff include_event != 'no'", {
  for (mode in c("no", "pseudo", "full"))
    expect_identical(
      neoipcr:::col_event_key$include_when(opts_with(include_event = mode)),
      mode != "no")
})

# ---- Hierarchy keys ------------------------------------------------------

test_that("hierarchy-key cols are included iff their gate != 'no'", {
  checks <- list(
    list(col = neoipcr:::col_department_key,
         opts_field = "include_department"),
    list(col = neoipcr:::col_hospital_key,
         opts_field = "include_hospital"),
    list(col = neoipcr:::col_country_key,
         opts_field = "include_country"),
    list(col = neoipcr:::col_wb_class_key,
         opts_field = "include_world_bank_class")
  )
  for (c in checks) for (mode in c("no", "pseudo", "full")) {
    opts <- do.call(dhis2_dataset_options,
                    stats::setNames(list(mode), c$opts_field))
    expect_identical(
      c$col$include_when(opts), mode != "no",
      info = paste(c$opts_field, "=", mode))
  }
})

# ---- isTest -------------------------------------------------------------

test_that("col_isTest is included iff include_test_data is TRUE", {
  expect_false(neoipcr:::col_isTest$include_when(
    opts_with(include_test_data = FALSE)))
  expect_true(neoipcr:::col_isTest$include_when(
    opts_with(include_test_data = TRUE)))
})

# ---- col_inherited_from -------------------------------------------------
#
# Child tibble carries the inherited column only when both:
#   - the option gate is not "no", AND
#   - the parent's compiled schema doesn't already carry it.

test_that("col_inherited_from is FALSE when the option gate is 'no'", {
  parent_cols <- list(neoipcr:::col_hospital_key)
  child <- neoipcr:::col_inherited_from(
    "hospital_key", "include_hospital", parent_cols)
  expect_false(child$include_when(opts_with(include_hospital = "no")))
})

test_that("col_inherited_from is FALSE when the parent already carries the col", {
  parent_cols <- list(neoipcr:::col_hospital_key)     # parent carries it
  child <- neoipcr:::col_inherited_from(
    "hospital_key", "include_hospital", parent_cols)
  expect_false(child$include_when(opts_with(include_hospital = "full")))
})

test_that("col_inherited_from is TRUE when the parent does NOT carry the col", {
  # parent carries unrelated column — the inherited key is absent upstream.
  parent_cols <- list(neoipcr:::col_department_key)
  child <- neoipcr:::col_inherited_from(
    "hospital_key", "include_hospital", parent_cols)
  expect_true(child$include_when(
    opts_with(include_hospital = "full", include_department = "full")))
})

test_that("col_inherited_from adapts when parent's own include_when flips", {
  # Parent carries hospital_key only when include_hospital != "no", which
  # is the same gate as the child. Under "full" both parent and child see
  # the option as active — but parent carries the col, so child does not.
  # The inherited helper's recursion via `compile_schema(parent_cols, opts)`
  # should see this correctly.
  parent_cols <- list(neoipcr:::col_hospital_key)
  child <- neoipcr:::col_inherited_from(
    "hospital_key", "include_hospital", parent_cols)
  # include_hospital = "no": both gate says FALSE → child FALSE.
  expect_false(child$include_when(opts_with(include_hospital = "no")))
  # include_hospital = "pseudo": gate TRUE; parent carries → child FALSE.
  expect_false(child$include_when(opts_with(include_hospital = "pseudo")))
  # include_hospital = "full": gate TRUE; parent carries → child FALSE.
  expect_false(child$include_when(opts_with(include_hospital = "full")))
})

test_that("col_inherited_from accepts a non-default type", {
  parent_cols <- list()
  child <- neoipcr:::col_inherited_from(
    "flag", "include_test_data", parent_cols, type = logical())
  expect_identical(child$type, logical())
})

# ---- attribute_cols -----------------------------------------------------

test_that("attribute_cols emits base + 5 companions with correct names/types", {
  base  <- neoipcr:::schema_col("birth_weight", integer())
  base_when <- \(opts) opts$include_patient == "full"
  cols <- neoipcr:::attribute_cols(base, base_when)
  expect_length(cols, 6L)
  expect_identical(
    purrr::map_chr(cols, "name"),
    c("birth_weight",
      "birth_weight_storedBy",
      "birth_weight_createdBy",
      "birth_weight_updatedBy",
      "birth_weight_createdAt",
      "birth_weight_updatedAt"))
  # user keys are integers; timestamps POSIXct; base retains its own type
  expect_identical(cols[[1]]$type, integer())
  expect_identical(cols[[2]]$type, integer())
  expect_identical(cols[[3]]$type, integer())
  expect_identical(cols[[4]]$type, integer())
  expect_s3_class(cols[[5]]$type, "POSIXct")
  expect_s3_class(cols[[6]]$type, "POSIXct")
})

test_that("attribute_cols companions are gated by base_when × include_user / timestamps", {
  base <- neoipcr:::schema_col("sex", factor(), factor_levels = c("f", "m", "u"))
  base_when <- \(opts) opts$include_patient == "full"
  cols <- neoipcr:::attribute_cols(base, base_when)

  # base_when FALSE → every companion FALSE regardless of user/timestamps.
  no_patient <- opts_with(
    include_patient = "no",
    include_user = "full",
    include_timestamps = TRUE)
  for (i in 2:6)
    expect_false(cols[[i]]$include_when(no_patient))

  # base_when TRUE, include_user = "no" → storedBy / createdBy / updatedBy OFF.
  full_no_user <- opts_with(
    include_patient = "full",
    include_user = "no",
    include_timestamps = TRUE)
  for (i in 2:4)
    expect_false(cols[[i]]$include_when(full_no_user))
  # timestamp companions ON
  for (i in 5:6)
    expect_true(cols[[i]]$include_when(full_no_user))

  # base_when TRUE, include_timestamps FALSE → _createdAt / _updatedAt OFF.
  full_no_ts <- opts_with(
    include_patient = "full",
    include_user = "full",
    include_timestamps = FALSE)
  for (i in 5:6)
    expect_false(cols[[i]]$include_when(full_no_ts))
  # user companions ON
  for (i in 2:4)
    expect_true(cols[[i]]$include_when(full_no_ts))
})

# ---- tea_attribute_cols -------------------------------------------------

test_that("tea_attribute_cols emits base + 3 companions (no createdBy/updatedBy)", {
  base <- neoipcr:::schema_col("birth_weight", integer())
  base_when <- \(opts) opts$include_patient == "full"
  cols <- neoipcr:::tea_attribute_cols(base, base_when)
  expect_length(cols, 4L)
  expect_identical(
    purrr::map_chr(cols, "name"),
    c("birth_weight",
      "birth_weight_storedBy",
      "birth_weight_createdAt",
      "birth_weight_updatedAt"))
  # Note absence of _createdBy / _updatedBy — per the DHIS2 Attribute.java
  # finding (docs/dhis2-user-timestamp-semantics.md).
})

test_that("tea_attribute_cols companions respect include_user / include_timestamps", {
  base <- neoipcr:::schema_col("sex", factor(), factor_levels = c("f", "m", "u"))
  base_when <- \(opts) opts$include_patient == "full"
  cols <- neoipcr:::tea_attribute_cols(base, base_when)

  full_full <- opts_with(
    include_patient = "full",
    include_user = "full",
    include_timestamps = TRUE)
  full_no_user <- opts_with(
    include_patient = "full",
    include_user = "no",
    include_timestamps = TRUE)
  full_no_ts <- opts_with(
    include_patient = "full",
    include_user = "full",
    include_timestamps = FALSE)

  # _storedBy: on under user != no, off otherwise
  expect_true (cols[[2]]$include_when(full_full))
  expect_false(cols[[2]]$include_when(full_no_user))
  expect_true (cols[[2]]$include_when(full_no_ts))
  # _createdAt / _updatedAt: on under timestamps
  expect_true (cols[[3]]$include_when(full_full))
  expect_true (cols[[3]]$include_when(full_no_user))
  expect_false(cols[[3]]$include_when(full_no_ts))
})

# ---- helper-schema -------------------------------------------------------
#
# Not strictly a schema-cols test, but exercised here alongside the
# helpers they support so we don't need a separate test file.

test_that("expect_schema_matches passes on identical tibbles", {
  expected <- tibble::tibble(
    id  = integer(),
    sex = factor(character(), levels = c("f", "m", "u")))
  actual <- expected
  expect_invisible(expect_schema_matches(actual, expected))
})

test_that("expect_schema_matches fails on divergent names / classes / levels", {
  expected <- tibble::tibble(
    id  = integer(),
    sex = factor(character(), levels = c("f", "m", "u")))
  expect_failure(expect_schema_matches(
    tibble::tibble(id = integer()), expected))
  expect_failure(expect_schema_matches(
    tibble::tibble(id = character(),
                   sex = factor(character(), levels = c("f", "m", "u"))),
    expected))
  expect_failure(expect_schema_matches(
    tibble::tibble(id = integer(),
                   sex = factor(character(), levels = c("f", "m"))),
    expected))
})

test_that("iter_dataset_options with no fields returns a single default opts", {
  out <- iter_dataset_options()
  expect_length(out, 1L)
  expect_s3_class(out[[1]], "neoipcr_dhis2_dsopt")
})

test_that("iter_dataset_options builds the cross-product of the named fields", {
  out <- iter_dataset_options(c("include_country", "include_hospital"))
  expect_length(out, 9L)   # 3 × 3
  # Each combination should appear exactly once.
  pairs <- purrr::map_chr(out, \(o) paste(
    o$include_country, o$include_hospital, sep = "/"))
  expect_identical(sort(pairs), sort(c(
    "no/no", "no/pseudo", "no/full",
    "pseudo/no", "pseudo/pseudo", "pseudo/full",
    "full/no", "full/pseudo", "full/full")))
})

test_that("iter_dataset_options errors on an unknown field", {
  expect_error(
    iter_dataset_options(c("include_country", "not_a_field")),
    "Unknown option field")
})
