# Tests for R/schema-tools.R — the schema engine.

# ---- schema_col ----------------------------------------------------------

test_that("schema_col builds a neoipcr_schema_col object with declared fields", {
  col <- neoipcr:::schema_col("foo", integer())
  expect_s3_class(col, "neoipcr_schema_col")
  expect_identical(col$name, "foo")
  expect_identical(col$type, integer())
  expect_true(is.function(col$include_when))
  expect_null(col$factor_levels)
  expect_identical(col$levels_source, "fixed")
})

test_that("schema_col accepts a factor column with explicit levels", {
  col <- neoipcr:::schema_col(
    "sex", factor(),
    factor_levels = c("f", "m", "u"))
  expect_identical(col$factor_levels, c("f", "m", "u"))
  expect_identical(col$levels_source, "fixed")
})

test_that("schema_col accepts levels_source = 'data' for data-derived factors", {
  col <- neoipcr:::schema_col(
    "displayName", factor(),
    factor_levels = character(),
    levels_source = "data")
  expect_identical(col$levels_source, "data")
})

test_that("schema_col rejects invalid inputs with actionable messages", {
  expect_error(
    neoipcr:::schema_col(c("a", "b"), integer()),
    "single character string")
  expect_error(
    neoipcr:::schema_col("foo", 1:3),
    "zero-length vector")
  expect_error(
    neoipcr:::schema_col("foo", integer(), include_when = "not a function"),
    "must be a function")
  expect_error(
    neoipcr:::schema_col("foo", integer(), factor_levels = 1:3),
    "NULL or a character vector")
  expect_error(
    neoipcr:::schema_col("foo", integer(), levels_source = "bogus"),
    "must be one of")
})

# ---- compile_schema -----------------------------------------------------

# A small set of opts objects we can reuse. `opts_all_full` turns every
# 3-valued gate to "full" so every include_when returning != "no"
# evaluates TRUE.
opts_all_full <- function(...) dhis2_dataset_options(
  include_world_bank_class = "full",
  include_country          = "full",
  include_hospital         = "full",
  include_department       = "full",
  include_user             = "full",
  include_patient          = "full",
  include_enrollment       = "full",
  include_event            = "full",
  ...
)

test_that("compile_schema returns an empty tibble when no column is included", {
  cols <- list(
    neoipcr:::schema_col("a", integer(), \(opts) FALSE),
    neoipcr:::schema_col("b", character(), \(opts) FALSE)
  )
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_identical(ncol(out), 0L)
})

test_that("compile_schema produces a 0-row tibble with declared columns and types", {
  cols <- list(
    neoipcr:::schema_col("id",   integer()),
    neoipcr:::schema_col("name", character())
  )
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_identical(names(out), c("id", "name"))
  expect_identical(nrow(out), 0L)
  expect_type(out$id,   "integer")
  expect_type(out$name, "character")
})

test_that("compile_schema preserves declaration order", {
  cols <- list(
    neoipcr:::schema_col("z", integer()),
    neoipcr:::schema_col("a", character()),
    neoipcr:::schema_col("m", logical())
  )
  expect_identical(
    names(neoipcr:::compile_schema(cols, dhis2_dataset_options())),
    c("z", "a", "m"))
})

test_that("compile_schema filters by include_when(opts)", {
  cols <- list(
    neoipcr:::schema_col("always",    integer()),
    neoipcr:::schema_col("full_only", integer(),
                         \(opts) opts$include_country == "full")
  )
  out_no <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "no"))
  out_full <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "full"))
  expect_identical(names(out_no),   "always")
  expect_identical(names(out_full), c("always", "full_only"))
})

test_that("compile_schema builds factor columns with declared levels", {
  cols <- list(neoipcr:::schema_col(
    "sex", factor(), factor_levels = c("f", "m", "u")))
  out <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_true(is.factor(out$sex))
  expect_identical(levels(out$sex), c("f", "m", "u"))
})

# ---- schema_codes --------------------------------------------------------

test_that("schema_codes returns compiled column names", {
  cols <- list(
    neoipcr:::schema_col("a", integer()),
    neoipcr:::schema_col("b", integer(), \(opts) FALSE),
    neoipcr:::schema_col("c", integer())
  )
  expect_identical(
    neoipcr:::schema_codes(cols, dhis2_dataset_options()),
    c("a", "c"))
})

# ---- assert_schema -------------------------------------------------------

make_cols <- function() list(
  neoipcr:::schema_col("id",   integer()),
  neoipcr:::schema_col("sex",  factor(), factor_levels = c("f", "m", "u")),
  neoipcr:::schema_col("name", character())
)

test_that("assert_schema passes when x exactly matches the compiled schema", {
  cols <- make_cols()
  expected <- neoipcr:::compile_schema(cols, dhis2_dataset_options())
  expect_invisible(
    neoipcr:::assert_schema(expected, cols, dhis2_dataset_options()))
})

test_that("assert_schema errors on column-name mismatch", {
  cols <- make_cols()
  wrong <- tibble::tibble(id = integer(), name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "column names / order differ")
})

test_that("assert_schema errors on column-order mismatch", {
  cols <- make_cols()
  reordered <- tibble::tibble(
    sex  = factor(character(), levels = c("f", "m", "u")),
    id   = integer(),
    name = character())
  expect_error(
    neoipcr:::assert_schema(reordered, cols, dhis2_dataset_options()),
    "column names / order differ")
})

test_that("assert_schema errors on class mismatch for a non-factor column", {
  cols <- make_cols()
  wrong <- tibble::tibble(
    id   = character(),
    sex  = factor(character(), levels = c("f", "m", "u")),
    name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "class differs")
})

test_that("assert_schema errors on level mismatch for a fixed-levels factor", {
  cols <- make_cols()
  wrong <- tibble::tibble(
    id   = integer(),
    sex  = factor(character(), levels = c("f", "m")),      # missing "u"
    name = character())
  expect_error(
    neoipcr:::assert_schema(wrong, cols, dhis2_dataset_options()),
    "factor column .* levels differ")
})

test_that("assert_schema does not check levels of data-derived factor columns", {
  cols <- list(neoipcr:::schema_col(
    "displayName", factor(),
    factor_levels = character(),
    levels_source = "data"))
  # different levels from what's declared — still passes because "data"-sourced.
  x <- tibble::tibble(
    displayName = factor(character(),
                         levels = c("Country A", "Country B")))
  expect_invisible(
    neoipcr:::assert_schema(x, cols, dhis2_dataset_options()))
})

# ---- finalize_to_schema --------------------------------------------------

test_that("finalize_to_schema selects declared columns in declaration order", {
  cols <- make_cols()
  # Input columns are declared in `cols` but in the wrong order.
  x <- tibble::tibble(
    name  = letters[1:3],
    id    = 1:3,
    sex   = c("f", "m", "u"))
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_identical(names(out), c("id", "sex", "name"))
})

test_that("finalize_to_schema drops columns listed in `scratch`", {
  cols <- list(neoipcr:::schema_col("keep", integer()))
  x <- tibble::tibble(keep = 1:3, drop = letters[1:3])
  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(), scratch = "drop")
  expect_identical(names(out), "keep")
})

test_that("finalize_to_schema errors on undeclared-and-not-scratch columns", {
  cols <- list(neoipcr:::schema_col("keep", integer()))
  x <- tibble::tibble(keep = 1:3, stray = letters[1:3])
  expect_error(
    neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options()),
    "not declared in schema and not in `scratch`")
  expect_error(
    neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options()),
    "stray")
})

test_that("finalize_to_schema silently drops declared-but-gated-out columns", {
  # `b` is declared but gated out; it is silently dropped (intentional
  # option-gating, not a mismatch). Matches the invariant that `cols`
  # lists *all* known columns, and `include_when` narrows them per opts.
  cols <- list(
    neoipcr:::schema_col("a", integer()),
    neoipcr:::schema_col("b", integer(), \(opts) opts$include_country == "full")
  )
  x <- tibble::tibble(a = 1:3, b = 1:3)
  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(include_country = "no"))
  expect_identical(names(out), "a")
})

test_that("finalize_to_schema accepts mix of scratch and declared-gated-out", {
  cols <- list(
    neoipcr:::schema_col("a", integer()),
    neoipcr:::schema_col("b", integer(), \(opts) opts$include_country == "full")
  )
  x <- tibble::tibble(a = 1:3, b = 1:3, raw_id = letters[1:3])
  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(include_country = "no"),
    scratch = "raw_id")
  expect_identical(names(out), "a")
})

test_that("finalize_to_schema materializes absent declared columns as NA", {
  # DHIS2's API omits fields that have null/empty values for all rows.
  # finalize_to_schema fills them with NA of the right type so the
  # schema shape is guaranteed regardless of data content.
  cols <- make_cols()
  x <- tibble::tibble(id = 1:3, name = letters[1:3])   # missing `sex`
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_true("sex" %in% names(out))
  expect_true(all(is.na(out$sex)))
  expect_identical(names(out), c("id", "sex", "name"))
})

test_that("finalize_to_schema applies declared factor levels", {
  cols <- make_cols()
  x <- tibble::tibble(
    id   = 1:3,
    sex  = c("f", "m", "u"),   # character, not yet factor
    name = letters[1:3])
  out <- neoipcr:::finalize_to_schema(x, cols, dhis2_dataset_options())
  expect_true(is.factor(out$sex))
  expect_identical(levels(out$sex), c("f", "m", "u"))
})

test_that("finalize_to_schema respects include_when() filtering", {
  cols <- list(
    neoipcr:::schema_col("always",    integer()),
    neoipcr:::schema_col("full_only", integer(),
                         \(opts) opts$include_country == "full")
  )
  # `full_only` is declared but gated out → silently dropped.
  # `extra` is not declared → must be listed in scratch.
  x <- tibble::tibble(always = 1:3, full_only = 1:3, extra = letters[1:3])
  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(include_country = "no"),
    scratch = "extra")
  expect_identical(names(out), "always")
})

# ---- with_entity_gate / entity_gate / entity_exists ----------------------
#
# The containing-entity gate closes a latent gap: individual atoms only
# know their own option, not the containing entity's option. Without the
# gate, using a shared atom whose predicate is "the linked entity
# exists" on a child entity would produce a stray non-empty tibble
# under `include_<child_entity> = "no"` — a violation of the `0 → 1 → N`
# strict progression. See R/schema-tools.R::with_entity_gate for the
# design rationale, and tasks/neoipcr-schema-arc/schema-entity-gate.md
# for the tracker.

test_that("with_entity_gate attaches the gate attribute; entity_gate reads it", {
  cols <- list(neoipcr:::schema_col("x", integer()))
  gate <- \(opts) opts$include_country != "no"
  gated <- neoipcr:::with_entity_gate(cols, gate = gate)

  expect_identical(neoipcr:::entity_gate(gated), gate)
})

test_that("with_entity_gate rejects non-function gate", {
  expect_error(
    neoipcr:::with_entity_gate(list(), gate = "not a function"),
    "must be a function")
})

test_that("entity_gate returns NULL for cols without a gate", {
  expect_null(neoipcr:::entity_gate(list()))
})

test_that("entity_exists returns TRUE when no gate is set", {
  opts <- dhis2_dataset_options()
  expect_true(neoipcr:::entity_exists(list(), opts))
})

test_that("entity_exists honors the gate predicate", {
  cols <- neoipcr:::with_entity_gate(
    list(), gate = \(o) o$include_country != "no")
  expect_false(neoipcr:::entity_exists(
    cols, dhis2_dataset_options(include_country = "no")))
  expect_true(neoipcr:::entity_exists(
    cols, dhis2_dataset_options(include_country = "pseudo")))
  expect_true(neoipcr:::entity_exists(
    cols, dhis2_dataset_options(include_country = "full")))
})

test_that("compile_schema returns 0x0 when entity_gate rejects opts, ignoring atoms", {
  # `always_true_atom` would fire under any opts; with the gate
  # rejecting opts, the entity is treated as empty anyway.
  always_true_atom <- neoipcr:::schema_col("stray", integer())
  cols <- neoipcr:::with_entity_gate(
    list(always_true_atom),
    gate = \(o) o$include_country != "no")

  schema <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "no"))

  expect_s3_class(schema, "tbl_df")
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("compile_schema proceeds when entity_gate accepts opts", {
  always_true_atom <- neoipcr:::schema_col("x", integer())
  cols <- neoipcr:::with_entity_gate(
    list(always_true_atom),
    gate = \(o) o$include_country != "no")

  schema <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "pseudo"))

  expect_identical(names(schema), "x")
})

test_that("compile_schema with no gate behaves exactly as before (backward compat)", {
  cols <- list(neoipcr:::schema_col("x", integer()))

  # Default gate is absent → entity_exists always TRUE → no short-circuit.
  schema <- neoipcr:::compile_schema(
    cols, dhis2_dataset_options(include_country = "no"))
  expect_identical(names(schema), "x")
})

test_that("assert_schema short-circuits when entity_gate rejects opts", {
  cols <- neoipcr:::with_entity_gate(
    list(neoipcr:::schema_col("x", integer())),
    gate = \(o) o$include_country != "no")
  opts <- dhis2_dataset_options(include_country = "no")

  # A 0×0 tibble matches the expected 0×0 shape → passes silently.
  expect_silent(neoipcr:::assert_schema(tibble::tibble(), cols, opts))

  # A tibble with stray columns fails the column-name-order check.
  expect_error(
    neoipcr:::assert_schema(tibble::tibble(x = integer()), cols, opts),
    "column names / order differ")
})

test_that("finalize_to_schema returns 0x0 when entity_gate rejects opts", {
  cols <- neoipcr:::with_entity_gate(
    list(neoipcr:::schema_col("x", integer())),
    gate = \(o) o$include_country != "no")
  x <- tibble::tibble(x = 1:3, y = letters[1:3])

  out <- neoipcr:::finalize_to_schema(
    x, cols, dhis2_dataset_options(include_country = "no"))

  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 0L)
  expect_equal(nrow(out), 0L)
})

# ---- require_cols --------------------------------------------------------
#
# Consumer-side assertion helper. Replaces `dplyr::select(any_of(cols))`
# at schema-to-consumer boundaries where the caller has already decided
# the columns should be present (per the current schema + opts). Silent
# `any_of` tolerance at these boundaries is the "Layer 2" half of the
# silent-failure sandwich (see tasks/neoipcr-schema-arc/schema-finalize-loud.md).

test_that("require_cols returns invisibly when every column is present", {
  x <- tibble::tibble(a = 1:3, b = letters[1:3], c = 1:3)
  expect_invisible(neoipcr:::require_cols(x, c("a", "b"), "test"))
  expect_identical(neoipcr:::require_cols(x, c("a", "b"), "test"), x)
})

test_that("require_cols errors when a required column is missing", {
  x <- tibble::tibble(a = 1:3)
  expect_error(
    neoipcr:::require_cols(x, c("a", "b"), "test"),
    "Required columns missing from `test`")
  expect_error(
    neoipcr:::require_cols(x, c("a", "b"), "test"),
    "b")
})

test_that("require_cols lists every missing column in the error", {
  x <- tibble::tibble(a = 1:3)
  err <- tryCatch(
    neoipcr:::require_cols(x, c("a", "b", "c"), "departments"),
    error = identity)
  expect_match(conditionMessage(err), "b")
  expect_match(conditionMessage(err), "c")
  expect_match(conditionMessage(err), "departments")
})

test_that("require_cols passes when cols is empty", {
  x <- tibble::tibble(a = 1:3)
  expect_invisible(neoipcr:::require_cols(x, character(), "test"))
})

test_that("schema_codes honors entity_gate via compile_schema", {
  cols <- neoipcr:::with_entity_gate(
    list(neoipcr:::schema_col("x", integer())),
    gate = \(o) o$include_country != "no")

  expect_identical(
    neoipcr:::schema_codes(cols, dhis2_dataset_options(include_country = "no")),
    character(0))
  expect_identical(
    neoipcr:::schema_codes(cols, dhis2_dataset_options(include_country = "pseudo")),
    "x")
})
