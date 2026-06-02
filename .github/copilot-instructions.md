# GitHub Copilot — neoipcr Instructions

This file documents the neoipcr R package. If this repository is checked out as a submodule of the `neoipc-workspace`, the workspace-level `.github/copilot-instructions.md` adds additional workspace-specific guardrails (file boundary, cross-repo change order) on top of the guardrails below.

---

## Guardrails

These guardrails are **universal** — mirrored in every NeoIPC repository's instruction files. If you add or change a guardrail here, add `<!-- SYNC: propagate to all repos -->` next to it so the change gets propagated when the workspace is next used.

- **Never** put personal names or other identifying information in source code (comments, strings, commit messages, etc.), except in copyright statements and file-header attribution lines (e.g. `Author:`, `@author`, `Copyright (c)` fields).
- **Never** read, write, or access files under `secrets/`, `data/local/`, or `.env`. This includes listing, globbing, searching, or interacting with these paths in any way — not just reading file contents. If the user provides a path under these directories, use it as-is without exploring the directory.
- **Never** push directly to `main` or `master` on this repository.
- **Never** make HTTP calls to the DHIS2 API or attempt to read JSON files returned from the DHIS2 API. These files contain sensitive surveillance data and are not needed for code-level tasks.
- **Never** put absolute local paths into files that get checked in. Use relative paths or generic placeholders. Local checkout paths are developer-specific and meaningless to others.
- Treat infection definitions in the Surveillance-Toolkit repository as normative. When a conflict exists between code and definitions, **fix the code**, not the definitions.
- **Never** introduce non-permissive dependencies (fonts, libraries, templates). All fonts must be SIL OFL or equivalent.
- **Always** keep `CLAUDE.md` and `.github/copilot-instructions.md` in sync within this repository. When you modify one, apply the same change to the other.
- **Always** push back when evidence contradicts the user's suggestion or implied assumption. Do not defer to the user's position when authoritative sources (AMA Manual of Style, protocol definitions, language specifications, etc.) say otherwise. Present the evidence clearly and let the user decide.
- **Always** consider both personal data protection (GDPR) and organizational/reputational concerns when making decisions about data shared between partners, published in reports, or exposed through APIs. Small cell counts in shared reports can expose which departments had specific rare pathogens or resistance patterns.
- **Always** namespace-qualify calls to functions from non-`base` packages with `pkg::fn(...)`, even when `pkg` is a recommended package auto-attached at R startup (`stats`, `utils`, `methods`, `grDevices`, `graphics`, `datasets`). The alternative is an explicit `#' @importFrom pkg fn` in roxygen plus a corresponding entry in `DESCRIPTION` `Imports`. Auto-attachment populates the interactive search path, but `R CMD check` codetools resolves package code against `base` + declared imports only — unqualified non-`base` calls produce *"no visible global function definition"* NOTEs. Documentation links (`[pkg::fn()]` in roxygen) and in-message references inside backticks (e.g. `` `stats::rbinom()` `` in an error message) stay as-is — they're documentation, not calls. Authoritative source: *Writing R Extensions* §1.1.3 / §1.6. <!-- SYNC: propagate to all repos -->

### R/ file structure

The `R/` directory follows a deliberate structure established by the neoipcr file restructure. Maintaining it requires discipline — every new function and file must land in the right place, or the structure decays silently.

#### File naming

- **`import-*.R`** — user-facing import orchestrators. One per data source (currently only `import-dhis2.R`; a future Excel source would be `import-excel.R`).
- **`dhis2-*.R`** — DHIS2-specific internals (connection, metadata, readers). These have no non-DHIS2 equivalent.
- **`calc-api.R`** — exported pipeline entry points. **`calc-tables.R`** — exported table/figure builders. **`calc-rates.R`** — internal rate/count computers. **`calc-denominators.R`** — internal risk-time/population denominators. Each is a layer in the epidemiological analysis pipeline; new functions go in the layer they belong to.
- **`validation-rules-*.R`** — one file per validation domain. The `validation_rules` registry list and `validate()` orchestrator stay in `validation.R`.
- **`data-removal.R`** — single-purpose file for the data-protection guardian. Do not add unrelated functions here.

#### Function placement

- **Exported functions first**, internal helpers below. Within a group of peers, follow the domain's logical progression (e.g. epidemiological: usage → incidence → detection → resistance).
- A new **table builder** goes in `calc-tables.R`. A new **rate computer** goes in `calc-rates.R`. A new **denominator** goes in `calc-denominators.R`. Do not add internal rate helpers to `calc-tables.R` or vice versa — the layers exist for a reason.
- A new **validation rule** goes in the `validation-rules-*.R` file matching its domain. If no existing domain fits, create a new `validation-rules-<domain>.R` file rather than forcing a rule into the wrong group.
- A new **metadata reader** goes in `dhis2-metadata-options.R` (option-set readers), `dhis2-metadata-reference.R` (reference data), or `dhis2-metadata-orgunits.R` (org unit structure) — whichever matches. Keep the orchestration file (`dhis2-metadata.R`) thin.
- **If in doubt** where a new function belongs, ask the user rather than guessing. A function in the wrong file is worse than a brief conversation.

#### Maintenance

- When adding or renaming an `R/*.R` file, update the **Key R Files** table below (and mirror to `CLAUDE.md`).
- When touching `R/` in any neoipcr task, scan for functions that have drifted into the wrong file (e.g. a helper added to a table-builder file during a rushed fix). Flag them to the user rather than silently moving them — the user may have context about why.

---

## Package Overview

`neoipcr` is an R package that facilitates working with NeoIPC surveillance data stored in DHIS2. It handles data import, calculation of epidemiological indicators, and data protection.

### Key R Files

| File | Purpose |
|------|---------|
| **Import pipeline** | |
| `R/import-dhis2.R` | `import_dhis2()` orchestrator + cross-cutting utilities (`add_key_column`, `convert_value`) |
| `R/dhis2-connect.R` | Connection options, authentication (token/basic/session/interactive) |
| `R/dhis2-options.R` | `dhis2_dataset_options()` constructor |
| `R/dhis2-users.R` | `get_user_info()` API call, `read_user_info_table()`, `read_metadata_users()` |
| `R/dhis2-metadata.R` | Metadata orchestration (`get_metadata`, request builders, response readers) |
| `R/dhis2-metadata-orgunits.R` | Org unit request builder + `read_organisationUnits*` response readers |
| `R/dhis2-metadata-options.R` | Option-set readers (12 `read_metadata_<optionset>` functions) + filter/convert helpers |
| `R/dhis2-metadata-reference.R` | Program structure + reference data (substances, AWaRe, ATC5, TEAs, trials, WB classes, countries) |
| `R/dhis2-trackedEntities.R` | Patient (tracked entity) import |
| `R/dhis2-enrollments.R` | Enrollment import |
| `R/dhis2-events.R` | Event import and processing |
| **Calculations** | |
| `R/calc-api.R` | Pipeline entry points (`calculate_reference_data`, `calculate_department_data`, `get_benchmark_data`, `pretty_names`) |
| `R/calc-tables.R` | 15 public table/figure builders (epidemiological progression: usage → incidence → detection → resistance) |
| `R/calc-rates.R` | 11 internal rate/count computers |
| `R/calc-denominators.R` | Risk-time, population, substance-day, AWaRe denominators |
| `R/calc-procedure-categories.R` | ICHI procedure category mapping |
| `R/scales.R` | Birth-weight / gestational-age binning helpers (`ga7`, `bw50`, `bw125`, `bw250`, `bw500`) |
| `R/cache.R` | Cache primitives (`cache`, `get_cached`, `new_cache`, `clean_cache`) + `add_class` |
| `R/ci.R` | Confidence interval functions (`neoipc_poisson_ci()`, `neoipc_wilson_ci()`) |
| **Data protection** | |
| `R/data-removal.R` | `apply_data_removal()` — the authoritative data-protection guardian |
| `R/filter.R` | `filter_*` family + `apply_postfilter` |
| **Validation** | |
| `R/validation.R` | `validation_rules` registry list + `validate()` orchestrator |
| `R/validation-rules-enrollment.R` | Rules 1, 2, 17, 25, 26 — enrollment lifecycle |
| `R/validation-rules-dates.R` | Rules 3, 4, 12–16 — date consistency |
| `R/validation-rules-completeness.R` | Rules 5–11 — form completion |
| `R/validation-rules-surgical.R` | Rules 19, 22–24 — surgical procedure validation |
| `R/validation-rules-surveillance-end.R` | Rules 18, 21 — surveillance-end consistency |
| `R/validation-rules-pathogens.R` | Rule 20 — pathogen resolution |
| `R/validation-rules-event-timing.R` | Rules 27–42 — DOL/LOS verification + early-onset flags |
| **Other** | |
| `R/pathogens.R` | Pathogen taxonomy and resistance markers |
| `R/types-check.R` | `is_*` predicates + `check_*` assertions for neoipcr S3 classes |

---

## Integrated Data Protection

A primary design goal is **integrated data protection**: the resulting dataset contains *only* the data the user explicitly requested via `dhis2_dataset_options()`, and supports safe pseudonymisation. Data scientists from partner hospitals are among the intended users -- safe defaults must prevent accidental data exposure while still allowing full access to their own site's data.

### Progressive narrowing

Every stage of the `import_dhis2()` pipeline should shed data it no longer needs:

1. **API request** -- fetch only the org units the user asked for (`ouMode` + `orgUnit`)
2. **Metadata processing** -- filter countries/departments/hospitals early; only build hierarchy columns downstream steps will use
3. **`read_patients/enrollments/events`** -- only join and keep columns needed for remaining processing and the final output
4. **`apply_data_removal()`** -- final redundant safety net; ideally a near-no-op because earlier stages already removed everything unnecessary

### Redundant foreign keys

Hierarchy keys (`department_key`, `hospital_key`, `country_key`, `world_bank_class_key`) appear *directly* in patients, enrollments, and events -- not only via the relational chain. This allows breaking the chain without losing the ability to classify records by higher-level categories (e.g., events can carry `world_bank_class_key` without exposing the country or patient).

### `apply_data_removal()` as authoritative guardian

`apply_data_removal()` is intended to be the **authoritative guardian** that ensures no unauthorized data leaks into the final dataset. It runs last and strips columns/tables based on `include_*` options. Earlier pipeline stages should already have removed most data, making `apply_data_removal()` a redundant safety net in the best case.

> **Note:** `apply_data_removal()` has not yet been thoroughly vetted. A future task should audit it against every `include_*` option.

---

## Authentication

neoipcr is the **single auth authority**. All other components (PS scripts, Docker containers, R data scripts) feed credentials into neoipcr via environment variables or function parameters.

### `dhis2_connection_options()` auth resolution

When no explicit `token`, `username`, or `session_id` parameter is provided, `get_auth_data()` falls back to environment variables:

1. `NEOIPC_DHIS2_SESSION_ID` -> session_id (Docker only)
2. `NEOIPC_DHIS2_TOKEN` -> token (validated via `read_token()`)
3. `NEOIPC_DHIS2_USER` + `NEOIPC_DHIS2_PASSWORD` -> username/password
4. `interactive()` -> prompt for username/password via `readline()` + `askpass::askpass()`
5. `!interactive()` -> `rlang::abort()` with actionable error message

### `get_password()` interactive guard

If `NEOIPC_DHIS2_USER` is set but `NEOIPC_DHIS2_PASSWORD` is not, `get_password()` checks `interactive()` before calling `askpass::askpass()`. In non-interactive sessions (e.g., Quarto renders), it aborts with a clear error instead of hanging.

### Token format

DHIS2 personal access tokens match `d2pat_` prefix + 42 characters (48 total). `read_token()` accepts either a raw token string or a file path containing the token.

---

## String Coercion

`dhis2_connection_options()` coerces `port` via `as.integer()` so that string values passed from Quarto params work without manual conversion. `dhis2_dataset_options()` similarly coerces numeric parameters (`birth_weight_from/to`, `gestational_age_from/to`).

---

## DHIS2 Test Units

Test organisation units live outside the real country hierarchy:

- Real hierarchy: **Root (NEOIPC) -> Country -> Hospital -> Department**
- Test hierarchy: **Root (NEOIPC) -> TEST_UNITS -> Department** (no hospital level)

Because test units have **no country, no hospital, and no World Bank income class**, code must tolerate `NA` in these keys when `include_test_data = TRUE`. Use `left_join` (not `inner_join`) when joining with countries or World Bank classes so that test data is preserved with `NA` keys.

---

## Gestational Age

Two tracked entity attributes store gestational age -- both **must** be set consistently when importing data:

| TEA code | Format | Example | Purpose |
|---|---|---|---|
| `NEOIPC_TEA_GEST_AGE` | `weeks+days` (text) | `25+4` | Human-friendly display in DHIS2 UI |
| `NeoIPC_TEA_TOTAL_GESTATION_DAYS` | integer (total days) | `179` | Used by neoipcr and DHIS2 program rules for all calculations |

**Note the inconsistent casing** of `NeoIPC_TEA_TOTAL_GESTATION_DAYS` -- this cannot be changed due to downstream dependencies.

Conversion: `total_days = weeks * 7 + days` (e.g. `25+4` -> `25*7 + 4 = 179`).

---

## Testing

### Test layout

Test files mirror source files: `R/foo.R` -> `tests/testthat/test-foo.R`.

| File | Scope |
|------|-------|
| `tests/testthat/test-ci.R` | `neoipc_poisson_ci()`, `neoipc_wilson_ci()`, bootstrap CI, vectorized wrappers |
| `tests/testthat/test-dhis2-metadata.R` | `read_metadata()` validation and data-reading tests |
| `tests/testthat/test-dhis2-connect.R` | `dhis2_connection_options()`, `get_auth_data()`, `read_token()`, `get_password()` |
| `tests/testthat/test-data-removal.R` | `apply_data_removal()` — every `include_*` option, cascading removal |
| `tests/testthat/test-validation.R` | `validate()` orchestrator, rule registry |
| `tests/testthat/test-validation-rules-*.R` | Per-domain rule tests (42 rules; 34 skip-wrapped stubs for unmigrated rules) |
| `tests/testthat/test-calc-api.R` | `calculate_department_data()` integration: structure, counts, table presence |
| `tests/testthat/test-calc-tables.R` | All 16 `get_*_table()` + figure data builders, with numerical spot-checks |
| `tests/testthat/test-pathogens.R` | `get_pathogen_taxonomy()`, `get_pathogen_list()`, synonym resolution |
| `tests/testthat/test-filter.R` | `filter_*` family, `apply_postfilter()` cascade, `filter_dataset()` bug coverage |
| `tests/testthat/test-test-units.R` | Test org unit tolerance — NA hierarchy keys through calc pipeline |
| `tests/testthat/helper-fixtures.R` | `read_test_metadata()`, `make_test_ds()`, `make_populated_test_ds()`, `make_calc_test_ds()`, per-table builders |

Planned (not yet created):

| File | Scope |
|------|-------|
| `test-import-dhis2.R` | `import_dhis2()` pipeline |
| `test-dhis2-options.R` | `dhis2_dataset_options()` constructor |
| `test-dhis2-users.R` | `get_user_info()`, `read_user_info_table()`, `read_metadata_users()` |
| `test-dhis2-enrollments.R` | Enrollment import |
| `test-dhis2-events.R` | Event import and processing |
| `test-dhis2-trackedEntities.R` | Patient (tracked entity) import |
| `test-dhis2-metadata-orgunits.R` | Org unit reading |
| `test-dhis2-metadata-options.R` | Option-set readers |
| `test-dhis2-metadata-reference.R` | Program structure + reference data readers |
| `test-calc-rates.R` | Rate/count computers |
| `test-calc-denominators.R` | Denominators |
| `test-calc-procedure-categories.R` | Procedure category mapping |
| `test-scales.R` | Binning helpers |
| `test-cache.R` | Cache primitives |

### Fixture files

Static JSON fixtures live under `tests/testthat/fixtures/`. They represent minimal valid DHIS2 `/api/metadata` responses shaped as `read_metadata()` expects them.

| File | Keys it provides |
|------|-----------------|
| `system.json` | `system` |
| `program.json` | `programs`, `trackedEntityTypes` |
| `org-units.json` | `organisationUnitGroups` |
| `antimicrobials.json` | `options` (antimicrobials), `optionGroupSets` |

### Running tests locally

```r
# From an R session in the package root:
devtools::test()                                            # all tests
devtools::test_file("tests/testthat/test-dhis2-connect.R")  # one file

# Coverage report:
Rscript scripts/coverage.R       # opens browser
Rscript scripts/coverage.R quiet  # no browser
```

### Rules

- Tests must **never** make real HTTP calls to DHIS2. All DHIS2 data comes from the static fixture files.
- Never read from `data/local/` or `secrets/` in tests.
- Use `withr::with_envvar()` to set environment variables in tests. Never use `Sys.setenv()` directly -- it leaks state between tests.
- Use `withr::local_tempfile()` for temporary files. Never use `tempfile()` directly -- `local_tempfile()` ensures cleanup.
- Internal (non-exported) functions are accessed via `neoipcr:::fn_name()` in tests.
- **Test failure scenarios, not implementation details.** When a function can fail for multiple reasons (e.g. missing key vs. malformed value), test each failure path independently. Do not assume two failure modes produce the same result just because they happen to share an internal code path today.
