---
applyTo: "**"
---

# `neoipcr` — code review instructions

Read these before generating review comments on this repository. The points below cover (a) review-process discipline that has been a recurring problem in past reviews, and (b) domain / API facts that have caused repeated false-positive findings.

## Review-process discipline

- **One comment per finding.** Do NOT post multiple comments for the same finding — neither at the same `(file, line)` nor at different occurrences of the same pattern. If the same construct appears at multiple lines, raise it ONCE and list the additional lines in that single comment.
- **Continue the conversation on existing threads.** If a finding has already been raised on this PR in an earlier review, do NOT create a new comment for it. Reply on the existing thread instead — even if the line number has shifted or the surrounding diff has changed. Tie new observations to the prior thread by referencing the thread or the prior comment id.
- **Respect resolved threads.** If a previously-raised finding was marked resolved (either because it was fixed in a commit, accepted as a false positive with reasoning, or explicitly deferred to a later PR), do NOT raise the same finding again in subsequent reviews of the same PR. The maintainer's resolution is authoritative.
- **Trust maintainer rebuttals.** When a maintainer replies to a finding with a reasoned rebuttal (e.g. "the third arg is `negate = TRUE`, the `&` is correct", or "this is the actual DHIS2 code, not a typo"), accept the rebuttal and do not re-raise the same finding in any later review of the same PR.
- **Before raising a finding, check the file's full context.** Many false positives in past reviews came from looking at a single line in isolation when a few lines above or below would have shown the relevant `negate = TRUE` argument, the `purrr::pluck` default, the `is.null()` guard, etc.

## Domain context — DHIS2 attribute / data-element codes

DHIS2 attribute, data-element, and option codes used by this package are defined on the **NeoIPC DHIS2 server** and have whatever casing and prefixing the server's metadata declares. Local R code MUST match the server's casing exactly, otherwise joins on `code` silently lose rows.

In particular:

- `NeoIPC_TEA_TOTAL_GESTATION_DAYS` — mixed-case `NeoIPC_` prefix — is the actual upstream attribute code (verified in the NeoIPC instance's `metadata.json`). It is NOT a typo against the all-uppercase `NEOIPC_…` convention used by most other codes. Do NOT flag mixed-case prefixes as typos.
- Several other older attributes use mixed-case prefixes for historical reasons. The all-uppercase convention only applies to attributes / data elements introduced from a certain point onward.

## Library / API conventions

### `stringr`

- `stringr::str_starts(string, pattern, negate)` — the third positional argument is `negate`. When `negate = TRUE`, the result is "string does NOT start with pattern". Filters that combine two such calls with `&` (e.g. `str_starts(x, "A", TRUE) & str_starts(x, "B", TRUE)`) are **exclusion filters** ("x starts with neither A nor B"), NOT impossible intersections. Do NOT flag these constructs as "always FALSE / will filter out everything"; verify whether `negate = TRUE` is present before raising.
- `stringr::str_extract(string, pattern, group)` — the `group` argument was added in stringr 1.5.0. This package declares `Imports: stringr (>= 1.5.1)` in `DESCRIPTION`, so `str_extract(..., group = N)` is supported. Do NOT flag it as "unsupported argument" or recommend switching to `str_match()` for the named-group case.

### `httr2`

- `httr2::resps_data(resps, resp_data)` is implemented as `vctrs::list_unchop(lapply(resps, resp_data))`. The callback is expected to return a value that survives `list_unchop`. When the callback returns `list(tibble)`, `list_unchop` collapses one level of nesting so the result is a flat list of tibbles indexable as `data[[1]]`, `data[[2]]`, etc. — which is exactly what call sites in `import_dhis2()` expect. The `list(...)` wrap in such callbacks is intentional; do NOT recommend removing it.

### Base R

- `tolower(x)` accepts non-character vectors and coerces them via `as.character` first. `tolower(TRUE)` returns `"true"`, `tolower(FALSE)` returns `"false"`. These are exactly the strings DHIS2 expects for boolean query parameters. Do NOT flag `tolower(<logical>)` as erroring or producing wrong output.
- R 4.4+ accepts trailing commas in function calls. This package declares `Depends: R (>= 4.4)`, so a trailing comma like `f(x, y,)` is not a syntax error in any supported R version. Do NOT flag trailing commas as syntax errors.

### `dplyr` / tidyselect

- `dplyr::arrange()` takes column references, not strings. Quoted strings like `arrange("col")` sort by the constant string, not by the column. Use `.data$col` or `all_of(c(...))` to reference columns dynamically. (This one IS worth flagging when you see it.)
- `dplyr::relocate(string_var)` where `string_var` holds a column name will look for a column literally named `"string_var"`, not the column named by its value. Use `dplyr::all_of(string_var)` or `dplyr::any_of(string_var)`. (Also worth flagging.)
