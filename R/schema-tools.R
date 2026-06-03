# Schema engine for neoipcr tibbles.
#
# Each column of each schematized tibble is declared as a `schema_col()`
# atom. Entity schemas are lists of atoms. `compile_schema(cols, opts)`
# assembles a 0-row tibble representing the final shape under the given
# `dhis2_dataset_options`. Readers pin factor levels pre-pivot via
# `schema_codes()`, normalize output via `finalize_to_schema()`, and
# assert the tail via `assert_schema()`.
#
# Internal — no `@export`. Used by `R/schema-cols-shared.R` and the
# per-domain `R/schema-*.R` files landed in Phase B.

# Construct a schema_col atom.
#
# name           — column name (character scalar)
# type           — zero-length vector of the column's R type, e.g.
#                  `integer()`, `character()`, `as.POSIXct(character())`.
#                  For factor columns pass `factor()` and supply
#                  `factor_levels`.
# include_when   — `function(opts) logical(1)`. TRUE means the column
#                  appears in the compiled schema under `opts`.
# factor_levels  — NULL, or character vector of levels for factor columns.
# levels_source  — "fixed" (protocol-declared — asserted by assert_schema)
#                  or "data" (data-derived — level list determined by
#                  surviving rows; apply `droplevels()` after filtering;
#                  not asserted in schema).
schema_col <- function(name, type,
                       include_when = function(opts) TRUE,
                       factor_levels = NULL,
                       levels_source = c("fixed", "data"))
{
  if (!is.character(name) || length(name) != 1L)
    rlang::abort("`name` must be a single character string.")
  if (length(type) != 0L)
    rlang::abort("`type` must be a zero-length vector (e.g. `integer()`).")
  if (!is.function(include_when))
    rlang::abort("`include_when` must be a function of `opts`.")
  if (!is.null(factor_levels) && !is.character(factor_levels))
    rlang::abort("`factor_levels` must be NULL or a character vector.")
  levels_source <- rlang::arg_match(levels_source)

  structure(
    list(
      name          = name,
      type          = type,
      include_when  = include_when,
      factor_levels = factor_levels,
      levels_source = levels_source
    ),
    class = "neoipcr_schema_col"
  )
}

# Wrap a list of schema_col atoms with a containing-entity gate. The gate
# is a `function(opts) logical(1)` predicate evaluated before any atom's
# `include_when`; when it returns FALSE, the whole entity is treated as
# empty and `compile_schema` / `schema_codes` / `assert_schema` /
# `finalize_to_schema` return (or operate on) a 0×0 tibble regardless of
# individual atom predicates.
#
# This closes a latent gap in the pure per-atom design: shared atoms such
# as `col_wb_class_key` only know about their own option
# (`include_world_bank_class != "no"`), not about the containing entity's
# option. Without the gate, using `col_wb_class_key` on countries under
# `include_country = "no" + include_world_bank_class = "full"` would
# produce a stray 1-col tibble instead of the required 0×0. The gate is
# the "entity exists" half of the compound predicate, applied once per
# entity, centralized and DRY.
#
# `get_<entity>_schema()` wrappers don't need to change — they still
# call `compile_schema(cols, opts)`. The gate is picked up via the
# attribute.
with_entity_gate <- function(cols, gate)
{
  if (!is.function(gate))
    rlang::abort("`gate` must be a function of `opts`.")
  attr(cols, "entity_gate") <- gate
  cols
}

# Look up the containing-entity gate for a cols list. Returns `NULL` when
# no gate is set — callers should treat that as "always-open".
entity_gate <- function(cols)
  attr(cols, "entity_gate", exact = TRUE)

# TRUE iff the containing entity's gate passes under `opts`; always TRUE
# when no gate has been declared.
entity_exists <- function(cols, opts)
{
  g <- entity_gate(cols)
  if (is.null(g)) TRUE else isTRUE(g(opts))
}

# Compile a list of schema_col atoms into a 0-row tibble under `opts`.
# Short-circuits to 0×0 when the containing-entity gate (if any) rejects
# `opts`. Otherwise, columns appear in declaration order, filtered by
# each atom's `include_when(opts)`. Factor columns are built with their
# declared levels.
compile_schema <- function(cols, opts)
{
  if (!entity_exists(cols, opts))
    return(tibble::tibble())

  included <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))

  if (length(included) == 0L)
    return(tibble::tibble())

  fields <- purrr::map(included, \(c) {
    if (!is.null(c$factor_levels))
      factor(character(), levels = c$factor_levels)
    else
      c$type
  })
  names(fields) <- purrr::map_chr(included, \(c) c$name)

  tibble::tibble(!!!fields)
}

# Column names from the compiled schema — used to pin factor levels on a
# `pivot_wider(names_from = …)` column via `factor(x, levels = schema_codes(...))`
# before `pivot_wider(..., names_expand = TRUE)`.
#
# For now returns every compiled column name. The events/per-event-type
# phases may refine this to only pivot-produced codes (vs. link keys and
# inherited hierarchy keys) when the need is concrete.
schema_codes <- function(cols, opts)
{
  names(compile_schema(cols, opts))
}

# Loud tail check: assert that `x` matches `compile_schema(cols, opts)`
# exactly — same column names in the same order, same base class
# per column, same factor levels for fixed-levels factors.
# Data-derived-levels factors are not asserted on levels (use
# `droplevels()` to regenerate them post-filter).
#
# Honours the containing-entity gate via `compile_schema()`: when the
# gate rejects `opts`, `x` must be a 0-col tibble to match the empty
# expected shape. The per-atom class / levels loop is driven by
# `exp_names` which is empty under that condition, so the check is a
# single name-order comparison.
assert_schema <- function(x, cols, opts)
{
  expected  <- compile_schema(cols, opts)
  exp_names <- names(expected)
  act_names <- names(x)

  if (!identical(act_names, exp_names))
    rlang::abort(c(
      "Schema mismatch: column names / order differ.",
      "i" = paste("expected:", paste(exp_names, collapse = ", ")),
      "x" = paste("actual:  ", paste(act_names, collapse = ", "))
    ))

  # When the entity gate rejected `opts`, `exp_names` is empty and we
  # skip the per-atom iteration — the name-order equality above is the
  # whole contract for "entity doesn't exist".
  if (!entity_exists(cols, opts))
    return(invisible(x))

  included <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))
  col_map  <- stats::setNames(included, purrr::map_chr(included, "name"))

  for (nm in exp_names) {
    if (!identical(class(x[[nm]]), class(expected[[nm]])))
      rlang::abort(c(
        sprintf("Schema mismatch on column `%s`: class differs.", nm),
        "i" = paste("expected:", paste(class(expected[[nm]]), collapse = "/")),
        "x" = paste("actual:  ", paste(class(x[[nm]]), collapse = "/"))
      ))

    if (is.factor(expected[[nm]]) &&
        col_map[[nm]]$levels_source == "fixed" &&
        !identical(levels(x[[nm]]), levels(expected[[nm]])))
      rlang::abort(c(
        sprintf("Schema mismatch on factor column `%s`: levels differ.", nm),
        "i" = paste("expected:", paste(levels(expected[[nm]]), collapse = ", ")),
        "x" = paste("actual:  ", paste(levels(x[[nm]]), collapse = ", "))
      ))
  }

  invisible(x)
}

# Normalize `x` to `compile_schema(cols, opts)`: select declared columns
# in declaration order, drop extras, and apply declared factor levels.
# Errors via `dplyr::select(all_of(...))` if any declared column is
# missing from `x` — the pre-pivot `names_expand = TRUE` contract
# makes missing columns a real bug, not silent drift.
#
# `scratch` is the caller's allowlist of reader-internal columns that
# the schema deliberately does not declare — e.g. the raw DHIS2
# `country` id on the hospitals reader, which the orchestrator uses
# for cross-response joins and then drops. Columns present in `x` that
# are neither declared in `cols` (under any `opts`) nor listed in
# `scratch` trip a loud `rlang::abort()` naming the offenders. This
# closes the "Layer 1" half of the silent-failure sandwich identified
# in `tasks/neoipcr-schema-arc/schema-finalize-loud.md`: the
# orchestrator pre-joins a column expecting it to survive, the schema
# doesn't declare it, and the silent drop feeds a downstream consumer
# that also silently tolerates absence → wrong data, no error.
#
# Honours the containing-entity gate: when the gate rejects `opts`,
# return a 0×0 tibble (no atoms iterated, no columns selected, scratch
# check skipped because the entity itself is absent).
finalize_to_schema <- function(x, cols, opts, scratch = character())
{
  if (!entity_exists(cols, opts))
    return(tibble::tibble())

  included     <- purrr::keep(cols, \(c) isTRUE(c$include_when(opts)))
  exp_names    <- purrr::map_chr(included, "name")
  declared     <- purrr::map_chr(cols, "name")
  undeclared   <- setdiff(names(x), c(declared, scratch))

  if (length(undeclared) > 0L)
    rlang::abort(c(
      "Input has column(s) not declared in schema and not in `scratch`:",
      "x" = paste(undeclared, collapse = ", "),
      "i" = "Declare them in the schema or list them in `scratch = ...`."
    ))

  # Materialize absent-but-declared columns with NA of the right type.
  # DHIS2's API omits fields that have null/empty values for all rows in
  # a response; after unnest_wider() the column simply doesn't exist.
  # The schema still declares it, so the select below would crash. This
  # mirrors the pre-pivot `names_expand = TRUE` pattern but for non-
  # pivoted columns.
  missing <- setdiff(exp_names, names(x))
  if (length(missing) > 0L) {
    expected <- compile_schema(cols, opts)
    for (m in missing)
      x[[m]] <- rep(expected[[m]][NA_integer_], nrow(x))
  }

  x <- x |>
    dplyr::select(tidyselect::all_of(exp_names))

  # Coerce each column to the schema's declared type. Handles two
  # cases that the pivot / unnest path can produce:
  #   1. All-NA columns arrive as logical (R's default NA type) but
  #      the schema declares integer / character / POSIXct / etc.
  #   2. Factor columns need their declared levels applied.
  expected <- if (exists("expected", inherits = FALSE)) expected
              else compile_schema(cols, opts)
  for (c in included) {
    nm  <- c$name
    act <- x[[nm]]
    exp <- expected[[nm]]
    # Factor levels (existing behaviour).
    if (!is.null(c$factor_levels)) {
      x[[nm]] <- factor(act, levels = c$factor_levels)
    # Base-type coercion: when actual class differs from expected and
    # all values are NA, coerce to the expected type.
    } else if (!identical(class(act), class(exp)) && all(is.na(act))) {
      x[[nm]] <- rep(exp[NA_integer_], length(act))
    }
  }

  x
}

# Assert that `x` has every column in `cols`. A loud replacement for
# `dplyr::select(tidyselect::any_of(cols))` at schema-to-consumer
# boundaries: where the caller has already *decided* the columns must
# be present (based on the current options and the schema contract),
# `any_of`'s silent tolerance is a hazard. `require_cols` makes the
# mismatch an error instead — typically paired with a subsequent
# `dplyr::select(tidyselect::all_of(cols))` which double-checks on
# forward-compat.
#
# `any_of` remains correct for truly option-dependent absence (e.g. a
# column gated on one option the consumer does *not* want to force
# present). Use `require_cols` specifically at boundaries where the
# schema contract has already committed the column to exist.
require_cols <- function(x, cols, entity_name)
{
  missing <- setdiff(cols, names(x))

  if (length(missing) > 0L)
    rlang::abort(c(
      sprintf("Required columns missing from `%s`:", entity_name),
      "x" = paste(missing, collapse = ", "),
      "i" = sprintf(
        paste0("Check that `%s_cols` schema declares these columns ",
               "under the current options."),
        entity_name)))

  invisible(x)
}
