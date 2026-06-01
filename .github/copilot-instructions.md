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

---

## Package Overview

`neoipcr` is an R package that facilitates working with NeoIPC surveillance data stored in DHIS2. It handles data import, calculation of epidemiological indicators, and data protection.

### Key R Files

> **This table describes the current layout.** A pending task — `tasks/neoipcr-file-restructure.md` in the `neoipc-workspace` — will split `R/calc.R`, `R/validation.R`, `R/dhis2.R`, and `R/dhis2-metadata.R` into smaller domain-coherent files, extract `apply_data_removal()` from `R/filter.R` into its own `R/data-removal.R`, and merge `R/obj-type.R` into `R/types-check.R`. If you are reading this in a workspace checkout, re-survey `R/` before relying on the file paths below — whichever neoipcr-touching task lands first publishes a fait accompli, and the table here may be ahead of or behind the actual state.

| File | Purpose |
|------|---------|
| `R/dhis2-connect.R` | Connection options, authentication (token/basic/session/interactive) |
| `R/dhis2.R` | `import_dhis2()` pipeline, `dhis2_dataset_options()` |
| `R/dhis2-metadata.R` | Org unit hierarchy, departments, hospitals, countries |
| `R/dhis2-trackedEntities.R` | Patient (tracked entity) import |
| `R/dhis2-enrollments.R` | Enrollment import |
| `R/dhis2-events.R` | Event import and processing |
| `R/calc.R` | Epidemiological calculations (`calculate_department_data()`, `get_benchmark_data()`) |
| `R/ci.R` | Confidence interval functions (`neoipc_poisson_ci()`, `neoipc_wilson_ci()`) |
| `R/filter.R` | Data filtering and subsetting (also currently hosts `apply_data_removal()` — the data-protection guardian — until the restructure extracts it) |
| `R/pathogens.R` | Pathogen taxonomy and resistance markers |
| `R/validation.R` | Data validation rules |

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
