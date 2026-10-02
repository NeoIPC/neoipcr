---
applyTo: "**"
---

# `neoipcr` — Code Review Instructions

Read these before generating review comments on this repository. They cover (1) review-process discipline and (2) domain and API facts without which a correct construct looks wrong.

## Review-Process Discipline

- **One comment per finding.** Do NOT post multiple comments for the same finding — neither at the same `(file, line)` nor at different occurrences of the same pattern. If the same construct appears at multiple lines, raise it ONCE and list the additional lines in that single comment.
- **Continue the conversation on existing threads.** If a finding has already been raised on this pull request in an earlier review, do NOT create a new comment for it. Reply on the existing thread instead — even if the line number has shifted or the surrounding diff has changed. Tie new observations to the prior thread by referencing the thread or the prior comment id.
- **Respect resolved threads.** If a previously raised finding was marked resolved (because it was fixed in a commit, accepted as a false positive with reasoning, or explicitly deferred to a later pull request), do NOT raise the same finding again in subsequent reviews of the same pull request. The maintainer's resolution is authoritative.
- **Trust maintainer rebuttals.** When a maintainer replies to a finding with a reasoned rebuttal (e.g. "the third arg is `negate = TRUE`, the `&` is correct", or "this is the actual DHIS2 code, not a typo"), accept the rebuttal and do not re-raise the same finding in any later review of the same pull request.
- **Before raising a finding, check the file's full context**: a line read in isolation looks wrong where a few lines above or below show the relevant `negate = TRUE` argument, the `purrr::pluck` default, or the `is.null()` guard.

## Domain Context — DHIS2 Attribute and Data-Element Codes

DHIS2 attribute, data-element, and option codes used by this package are defined on the **NeoIPC DHIS2 server** and have whatever casing and prefixing the server's metadata declares. Local R code MUST match the server's casing exactly, otherwise joins on `code` silently lose rows.

- `NeoIPC_TEA_TOTAL_GESTATION_DAYS` — mixed-case `NeoIPC_` prefix — is the actual upstream attribute code, and the one code with a mixed-case prefix: collected data and downstream consumers depend on it, so it keeps its spelling. Do NOT flag it as a typo.
- Every other NeoIPC code uses the all-uppercase `NEOIPC_` prefix. A mixed-case prefix anywhere else is worth flagging, since a join on it would match nothing.

## Library and API Conventions

### `stringr`

- `stringr::str_starts(string, pattern, negate)` — the third positional argument is `negate`. When `negate = TRUE`, the result is "string does NOT start with pattern". Filters that combine two such calls with `&` (e.g. `str_starts(x, "A", TRUE) & str_starts(x, "B", TRUE)`) are **exclusion filters** ("x starts with neither A nor B"), NOT impossible intersections. Do NOT flag these constructs as "always FALSE / will filter out everything"; verify whether `negate = TRUE` is present before raising.
- `stringr::str_extract(string, pattern, group)` — the `group` argument was added in stringr 1.5.0. This package declares `Imports: stringr (>= 1.5.1)` in `DESCRIPTION`, so `str_extract(..., group = N)` is supported. Do NOT flag it as "unsupported argument" or recommend switching to `str_match()` for the named-group case.

### `httr2`

- `httr2::resps_data(resps, resp_data)` is implemented as `vctrs::list_unchop(lapply(resps, resp_data))`. The callback is expected to return a value that survives `list_unchop`. When the callback returns `list(tibble)`, `list_unchop` collapses one level of nesting so the result is a flat list of tibbles indexable as `data[[1]]`, `data[[2]]`, etc. — which is exactly what call sites in `import_dhis2()` expect. The `list(...)` wrap in such callbacks is intentional; do NOT recommend removing it.

### Base R

- `tolower(x)` accepts non-character vectors and coerces them via `as.character` first. `tolower(TRUE)` returns `"true"`, `tolower(FALSE)` returns `"false"`. These are exactly the strings DHIS2 expects for boolean query parameters. Do NOT flag `tolower(<logical>)` as erroring or producing wrong output.
- A trailing comma in a call (`f(x, y,)`) parses in every R version, but it passes an empty argument, which most base functions and ordinary closures reject at run time (`argument 3 is empty`, `unused argument`). Functions whose dots go through `rlang::list2()`, such as the `dplyr` verbs and `tibble::tibble()`, accept it. Do NOT flag a trailing comma in such a call as a syntax error; flag one anywhere else.

### `dplyr` / tidyselect

- `dplyr::arrange()` takes column references, not strings. Quoted strings like `arrange("col")` sort by the constant string, not by the column. Use `.data$col` or `all_of(c(...))` to reference columns dynamically. (This one IS worth flagging when you see it.)
- `dplyr::relocate(string_var)`, where `string_var` holds a column name, is ambiguous: tidyselect picks a column literally named `string_var` if one exists, and otherwise falls back to the variable's value with a deprecation warning (deprecated since tidyselect 1.1.0). Use `dplyr::all_of(string_var)` or `dplyr::any_of(string_var)`. (Also worth flagging.)
