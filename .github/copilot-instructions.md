# GitHub Copilot — neoipcr Instructions

This file documents the neoipcr R package. If this repository is checked out as a submodule of the `neoipc-workspace`, the workspace-level `.github/copilot-instructions.md` adds additional workspace-specific guardrails (file boundary, cross-repo change order) on top of the guardrails below.

---

## Guardrails

The rules below are the NeoIPC **universal** guardrails, localized to this repository's stack — language-specific examples are adapted to the languages actually used here (R), and code-authoring rules with no referent in this repository are omitted. Rules tagged *(repo-specific)* or *(\<lang\>-specific)* apply only to the repositories that carry them. To add or change a universal guardrail, edit it here and add `<!-- SYNC: propagate to all repos -->` so it is propagated — and re-localized — across every repo when the workspace is next used.

- **Never** put personal names or other identifying information in source code (comments, strings, commit messages, etc.), except in copyright statements and file-header attribution lines (e.g. `Author:`, `@author`, `Copyright (c)` fields).
- **Never** read, write, or access files under `secrets/`, `data/`, or `.env`. This includes listing, globbing, searching, or interacting with these paths in any way — not just reading file contents. If the user provides a path under these directories, use it as-is without exploring the directory.
- **Never** push directly to `main` or `master` on this repository.
- **Never** make HTTP calls to the DHIS2 API or attempt to read JSON files returned from the DHIS2 API. These files contain sensitive surveillance data and are not needed for code-level tasks.
- **Never** put absolute local paths into files that get checked in. Use relative paths or generic placeholders. Local checkout paths are developer-specific and meaningless to others.
- Treat infection definitions in the Surveillance-Toolkit repository as normative. When a conflict exists between code and definitions, **fix the code**, not the definitions.
- **Never** introduce non-permissive dependencies (fonts, libraries, templates). All fonts must be SIL OFL or equivalent.
- **Always** keep `CLAUDE.md` and `.github/copilot-instructions.md` in sync within this repository. When you modify one, apply the same change to the other.
- **Always** push back when evidence contradicts the user's suggestion or implied assumption. Do not defer to the user's position when authoritative sources (AMA Manual of Style, protocol definitions, language specifications, etc.) say otherwise. Present the evidence clearly and let the user decide.
- **Always** consider both personal data protection (GDPR) and organizational/reputational concerns when making decisions about data shared between partners, published in reports, or exposed through APIs. Small cell counts in shared reports can expose which departments had specific rare pathogens or resistance patterns.
- **Never** use deprecated or outdated APIs. Before introducing a function from a third-party package or a base library, verify it is current. When a replacement exists, use the replacement. When unsure, check the package's `NEWS.md` / release notes rather than assuming.
- **Never** use the `.data$` pronoun in tidyselect contexts (`select()`, `rename()`, `relocate()`, `across(.cols=)`, `pivot_wider(names_from=)` / `pivot_longer(cols=)`, `unnest_wider(col=)`, gt column-selection arguments). Use string column names (`"col"`), bare names, or tidyselect helpers (`all_of()`, `any_of()`, `starts_with()`, `where()`, etc.) instead. `.data$col` is correct **only** in data-masking contexts (`mutate()`, `filter()`, `summarise()`, `arrange()`, `if_else()`, `case_when()`, `aes()`).
- **Always** read the upstream source directly when you need a definitive answer about a third-party system's behaviour (DHIS2 in particular, but also R / tidyverse packages, Quarto, Pandoc, .NET runtime, etc.). Docs, release notes, and changelogs are known to be unreliable for some of these projects — the source is the ultimate authority, the written reference is a convenience shortcut. When working via the neoipc-workspace, see its `CLAUDE.md` → Reference checkouts for the `refs/` submodules that support this workflow.
- **Always** verify upstream claims now, not later. When a plan or recommendation depends on a fact about a third-party system's behaviour, read the source as part of the planning step; do not write "verify at implementation time" or "TBD against upstream" and move on. Deferred verification compounds — each unresolved fact is an attack surface for later wrong implementation. Pairs with the "read the upstream source directly" guardrail above.
- **Always** verify factual claims in design notes and task files against the actual source before propagating them. Treat "X does Y" descriptions in repo documentation as a hypothesis to verify by reading the function or module, not as ground truth — these documents can carry stale or wrong claims that survive long enough to look authoritative. When the claim turns out to be wrong, fix the documentation in the same commit.
- **Always** re-read iteratively-edited documents end-to-end before marking them done. After several rounds of edits to a long document (plan files, design docs, multi-section task files), proactively read the whole thing to catch sentences that contradict later edits, file/path references that no longer match the current model, summaries that drifted from the detail, deferred-section markers that disappeared, naming-scheme drift between sections. Don't make the user point each one out individually.
- **Never** dismiss identified inconsistencies as "cosmetic" when a rename window is already open (pre-alpha, release prep, planned breaking change in the same area). The cost-benefit changes the moment a rename for anything else in the same family is proposed; the right default in that window is to fix the inconsistency in the same pass.
- **Never** write filler comments — comments that describe absent behavior ("not currently used"), restate the obvious, or hedge ("maybe this is needed?") add no information. The default is no comment; reserve comments for hidden constraints, subtle invariants, surprising behavior, or workarounds for specific bugs. If a property's existence is unclear without a comment, the property is misnamed, misplaced, or shouldn't exist — fix that instead.
- **Always** write doc comments on exported functions (Roxygen `#'` blocks) and targeted explanatory comments at non-obvious design points as part of the same change that introduces the code — don't defer to a "doc-comments sweep" follow-up. Pairs with the "no filler comments" guardrail above: comments must add information, AND the ones that are warranted must land in-band, not later.
- **Never** predict the future in code comments. Speculative commented-out code, `# TODO: when X happens, do Y` notes, or any forward-looking text describing not-yet-decided changes belongs in the project's task tracking, not in checked-in source. Source comments describe *what is*, not *what might be*.
- **Never** leave placeholder stubs (a function whose body is just `stop("not implemented")` or similar) in source as scaffolding for future work. A function that exists only to error because the real implementation "comes later" is dead code. Delete it; the planned work belongs in the project's task tracking, not in checked-in source.
- **Never** add a `Co-Authored-By` trailer to git commit messages. The user does not want AI co-author attribution.
- **Never** put long-lived guidance in per-machine local memory (e.g. Claude Code's `~/.claude/.../memory/`) — it does not follow the user across machines. Coding rules, communication preferences, domain conventions, and recurring corrections belong in this `CLAUDE.md` (and its `.github/copilot-instructions.md` sibling) so they travel with the repo. Reserve local memory for genuinely ephemeral session context.
- **Never** modify the user's global git config (`git config --global ...` / `~/.gitconfig`) as a workaround for a transient problem. For network disconnects, slow clones, or intermittent failures, **retry** — the failure is usually elsewhere and a config tweak persists across every repo on the machine. For genuine repo-specific tuning, use `git config --local ...` or a one-shot `-c key=value` flag on the command. Examples to avoid: `core.compression=0` (kills compression for all future git operations), `http.postBuffer` bumps (only relevant for HTTP-1.1 push edge cases). If a genuine global change is needed, surface it to the user first with the specific reason and the persistent cost.
- **Never** force-push to a branch that has an open pull request under review. Rewriting already-pushed history mid-review is hostile to reviewers — it discards their in-progress review, breaks the anchoring of existing review comments to lines and commits, and hides what actually changed since they last looked. Push follow-up commits instead; because merges are squash-merged, the intermediate commits collapse into one on merge, so a clean final history costs nothing. Force-pushing is acceptable only on a private WIP branch that has not been shared for review.
- **Always** namespace-qualify calls to functions from non-`base` packages with `pkg::fn(...)`, even when `pkg` is a recommended package auto-attached at R startup (`stats`, `utils`, `methods`, `grDevices`, `graphics`, `datasets`). The alternative is an explicit `#' @importFrom pkg fn` in roxygen plus a corresponding entry in `DESCRIPTION` `Imports`. Auto-attachment populates the interactive search path, but `R CMD check` codetools resolves package code against `base` + declared imports only — unqualified non-`base` calls produce *"no visible global function definition"* NOTEs. Documentation links (`[pkg::fn()]` in roxygen) and in-message references inside backticks (e.g. `` `stats::rbinom()` `` in an error message) stay as-is — they're documentation, not calls. Authoritative source: *Writing R Extensions* §1.1.3 / §1.6.

### Joining tibbles

- **Never** join tibbles on DHIS2 UIDs (`trackedEntity`, `enrollment`, `event`, `orgUnit`, `country`, `programStage`, `dataElement`, etc.) when both sides already carry synthesized integer keys (`patient_key`, `enrollment_key`, `event_key`, `department_key`, `hospital_key`, `country_key`, etc.). Always join on the integer keys. The only exception is during import when at least one side of the join doesn't have an integer key yet (raw API response → keyed tibble bridge via an internal map like `.patients_internal_map`). DHIS2 UIDs are opaque artefacts for linking back to DHIS2; integer keys are the relational backbone.

### R/ file structure

The `R/` directory follows a deliberate structure established by the neoipcr file restructure. Maintaining it requires discipline — every new function and file must land in the right place, or the structure decays silently.

#### File naming

- **`import-*.R`** — user-facing import orchestrators. One per data source (currently only `import-dhis2.R`; a future Excel source would be `import-excel.R`).
- **`dhis2-*.R`** — DHIS2-specific internals (connection, metadata, readers). These have no non-DHIS2 equivalent.
- **`calc-api.R`** — exported pipeline entry points. **`calc-tables.R`** — exported table/figure builders. **`calc-rates.R`** — internal rate/count computers. **`calc-denominators.R`** — internal risk-time/population denominators. Each is a layer in the epidemiological analysis pipeline; new functions go in the layer they belong to.
- **`validation-rules-*.R`** — one file per validation domain. The `validation_rules` registry list and `validate()` orchestrator stay in `validation.R`.
- **`data-protection.R`** — single-purpose file for the data-protection guardian (`assert_data_protection()`). Do not add unrelated functions here.

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
| **Schema engine** | |
| `R/schema-tools.R` | `schema_col()`, `compile_schema()`, `schema_codes()`, `assert_schema()`, `finalize_to_schema()` — column-as-declaration engine. `with_entity_gate(cols, gate)` + `entity_gate()` + `entity_exists()` attach a containing-entity-gate predicate so `compile_schema`/`assert_schema`/`finalize_to_schema` short-circuit to 0×0 when the gate rejects `opts`. Internal, no `@export`. |
| `R/schema-cols-shared.R` | Cross-entity column declarations: `col_patient_key`, `col_enrollment_key`, `col_event_key`, `col_department_key`, `col_hospital_key`, `col_country_key`, `col_wb_class_key`, `col_isTest`, plus `col_inherited_from()` (hierarchy-inheritance helper) and `attribute_cols()` / `tea_attribute_cols()` (companion-column helpers for partner-site-entered attributes). Loads before every `schema-<domain>.R` via `@include`. |
| `R/schema-orgunits.R` | Column declarations + `get_<entity>_schema(opts)` wrappers for the org-unit-derived metadata entities: WB classes, countries, hospitals, departments, users, event types. Loads after `schema-cols-shared.R`. Internal. |
| `R/schema-patients.R` | `patient_attribute_cols()` wrapper + `patients_cols` + `get_patients_schema()`. First fact-layer schema. Loads after `schema-orgunits.R`. Internal. |
| `R/schema-enrollments.R` | `enrollment_inherited_from()` helper + `enrollments_cols` + `get_enrollments_schema()`. Second fact-layer schema; atoms declared directly (no per-attribute wrapper — every user/timestamp field on enrollments is entity-level). Loads after `schema-patients.R`. Internal. |
| `R/schema-events.R` | `event_hierarchy_col()` helper + `events_cols` + `get_events_schema()`. Third fact-layer schema; introduces the `include_event` gate; carries PK + id-opt-in + occurredAt + status + event_type_key + link FKs + hierarchy keys via direct materialization + entity-level user fields (`createdBy` / `updatedBy` / `storedBy` / `completedBy`) + six entity-level timestamps + `followup` + `deleted`. The former `eventDetails` sidecar tibble was merged in here in phase-b-event-details. Loads after `schema-enrollments.R`. Internal. |
| `R/schema-event-data.R` | Per-event-type data schemas for all seven event types (`admissionData_cols`, `surveillanceEndData_cols`, `sepsisData_cols`, `necData_cols`, `pneumoniaData_cols`, `surgeryData_cols`, `ssiData_cols`) + `event_data_col()` wrapper (payload + three DE-level companion columns via `event_data_attribute_cols()`) + `event_data_cols_for(event_type_key)` dispatcher. Pre-pivot factor pinning via `schema_codes()` + `pivot_wider(names_expand = TRUE)` closes the pivot-volatility hazard in `read_event_data()`. `vs_days` on surveillance-end is declared on the schema and computed post-pivot from the guaranteed `inv_days + niv_days`. Also hosts the three findings-family schemas — `findings_cols` (`infectiousAgentFindings` — `source` + resistance markers + `multiple` all unconditionally declared under full mode, fixing failure pattern #6), `substanceDays_cols`, and `unknownPathogenNames_cols`. `read_infectious_agent_findings()` produces both findings and the `unknownPathogenNames` split as a list, splitting on the pre-finalize intermediate (where `name` is still attached) via the internal `split_unknown_pathogen_names()` helper. Pre-pivot factor pinning on the pathogen-subfield `type` + `names_expand = TRUE` closes the pivot-volatility hazard; `read_substance_days()` follows the same pattern. Loads after `schema-events.R`. Internal. |
| `R/schema-notes.R` | `event_notes_cols` and `enrollment_notes_cols` with a shared `.notes_payload_cols(parent_full)` factory (same payload shape: `note`, `value`, `storedBy`, `storedAt`, `createdBy`). Entity gate compounds `include_<parent> != "no"` with `"<parent>" %in% include_notes`. Hierarchy-key inheritance via `col_inherited_from(..., <parent>_cols)` — lean children under fat parents, direct materialization under pseudo parents. Loads after `schema-enrollments.R` + `schema-events.R`. Internal. |
| **Data protection** | |
| `R/data-protection.R` | `assert_data_protection()` — the authoritative data-protection guardian. Asserts invariants under the schema contract (reader-owned tibble shapes); no scrubs remain after phase-b-event-details. |
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
| `R/log.R` | Logging infrastructure (via the `logger` package, namespace `"neoipcr"`): `log_dhis2_request()` — the data-protection-safe DHIS2 query trace logging **URL + HTTP status + row count only**, never a response body; exported `neoipcr_log_config()` (verbosity → namespace threshold); `.onLoad` sets the namespace formatter and reads `NEOIPC_LOG_LEVEL`. |

---

## Integrated Data Protection

A primary design goal is **integrated data protection**: the resulting dataset contains *only* the data the user explicitly requested via `dhis2_dataset_options()`, and supports safe pseudonymisation. Data scientists from partner hospitals are among the intended users -- safe defaults must prevent accidental data exposure while still allowing full access to their own site's data.

### Progressive narrowing

Every stage of the `import_dhis2()` pipeline should shed data it no longer needs:

1. **API request** -- fetch only the org units the user asked for (`ouMode` + `orgUnit`)
2. **Metadata processing** -- filter countries/departments/hospitals early; only build hierarchy columns downstream steps will use
3. **`read_patients/enrollments/events`** -- only join and keep columns needed for remaining processing and the final output
4. **`assert_data_protection()`** -- final guardian that asserts every reader has honored the user's `include_*` options; aborts loudly on a reader regression. Named scrubber in earlier revisions (`apply_data_removal()`), but under the schema contract the readers own every tibble's shape, so this function now asserts rather than scrubs.

### Redundant foreign keys

Hierarchy keys (`department_key`, `hospital_key`, `country_key`, `world_bank_class_key`) appear *directly* in patients, enrollments, and events -- not only via the relational chain. This allows breaking the chain without losing the ability to classify records by higher-level categories (e.g., events can carry `world_bank_class_key` without exposing the country or patient).

### `assert_data_protection()` as authoritative guardian

`assert_data_protection()` is the **authoritative guardian** that verifies no unauthorized data leaks into the final dataset. It runs last in the `import_dhis2()` pipeline. Under the schema contract every reader owns its tibble's shape, so this function's job is to **assert invariants** — not to scrub columns. A schema regression that leaks a column reserved for another option value surfaces here with an actionable `rlang::abort()` naming the leaking tibble and the column.

Post phase-b-event-details every branch is an assertion — no scrubs remain. The former `eventDetails` sidecar tibble was merged into `events` and the `event` id gate moved into `events_cols`.

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
| `tests/testthat/test-data-protection.R` | `assert_data_protection()` — happy-path (schema-compliant ds passes) and failure-path (reader regression leaks a forbidden column → abort) coverage; pure-assertion guardian post phase-b-event-details |
| `tests/testthat/test-data-protection-complete.R` | Option-matrix coverage: iterates every `include_*` gate (hierarchy, link-privacy, user, timestamps, deleted) under every value against a schema-compliant ds, verifying the guardian passes. Maximally-restrictive smoke test. |
| `tests/testthat/test-validation.R` | `validate()` orchestrator, rule registry |
| `tests/testthat/test-validation-rules-*.R` | Per-domain rule tests (42 rules; 34 skip-wrapped stubs for unmigrated rules) |
| `tests/testthat/test-calc-api.R` | `calculate_department_data()` integration: structure, counts, table presence |
| `tests/testthat/test-calc-tables.R` | All 16 `get_*_table()` + figure data builders, with numerical spot-checks |
| `tests/testthat/test-pathogens.R` | `get_pathogen_taxonomy()`, `get_pathogen_list()`, synonym resolution |
| `tests/testthat/test-log.R` | `log_dhis2_request()` records URL+status+count and **never** the response body (data-protection guard), httr2-error handling, threshold gating; `neoipcr_log_config()` verbosity mapping + `NEOIPC_LOG_LEVEL` default |
| `tests/testthat/test-filter.R` | `filter_*` family, `apply_postfilter()` cascade, `filter_dataset()` bug coverage |
| `tests/testthat/test-test-units.R` | Test org unit tolerance — NA hierarchy keys through calc pipeline |
| `tests/testthat/test-schema-tools.R` | `schema_col()`, `compile_schema()`, `schema_codes()`, `assert_schema()`, `finalize_to_schema()` |
| `tests/testthat/test-schema-cols-shared.R` | Shared column declarations, `col_inherited_from()`, `attribute_cols()` / `tea_attribute_cols()`, plus `expect_schema_matches()` / `iter_dataset_options()` helpers |
| `tests/testthat/test-schema-orgunits.R` | Per-domain schema assembly for org-unit-derived metadata entities: WB classes / countries / hospitals / departments / users / event types. |
| `tests/testthat/test-schema-patients.R` | `patient_attribute_cols()` wrapper behaviour + `patients_cols` three-mode shape, hierarchy-key inheritance, companion-column semantics. |
| `tests/testthat/test-schema-enrollments.R` | `enrollments_cols` three-mode shape, entity-level user/timestamp gating, hierarchy-key inheritance anchored on patients. |
| `tests/testthat/test-schema-events.R` | `events_cols` three-mode shape, compound link-FK gating on enrollment/patient, hierarchy-key direct materialization, status/event_type_key factor levels, `isTest` direct materialization. |
| `tests/testthat/test-schema-event-data.R` | Per-event-type data schemas (all 7) + the three findings-family schemas (`findings_cols`, `substanceDays_cols`, `unknownPathogenNames_cols`): three-mode shape, inheritance-driven link/hierarchy key absence, per-type payload coverage + fixed factor levels, companion-column gating (3 per DE), `source` / resistance / `multiple` unconditional declaration under full mode, `split_unknown_pathogen_names()` behaviour (the internal split helper invoked by `read_infectious_agent_findings()`), fixture round-trip, dispatcher. |
| `tests/testthat/test-schema-notes.R` | `event_notes_cols` + `enrollment_notes_cols`: compound entity-gate (include_event/include_enrollment AND include_notes), pseudo-parent inheritance-driven direct materialization, payload gating on include_dhis2_ids / include_user / include_timestamps, hierarchy-key inheritance absence under full parent, fixture round-trip. |
| `tests/testthat/test-read-event-data.R` | Reader-level integration tests: sparse-data resilience for `read_event_data` (all 7 types), `read_substance_days`, `read_infectious_agent_findings`, `read_events`; pivot-volatility, absent-column materialization, type-drift, hierarchy-key inheritance, enrollment-chain patient_key derivation, companion columns. |
| `tests/testthat/helper-fixtures.R` | `read_test_metadata()`, `make_test_ds()`, `make_populated_test_ds()`, `make_calc_test_ds()`, per-table builders |
| `tests/testthat/helper-schema.R` | `expect_schema_matches(x, expected)`, `iter_dataset_options(fields)` |
| `tests/testthat/helper-event-data.R` | Raw DHIS2-shaped fixture builders for reader integration tests: `build_raw_events()`, `build_processed_events()`, `build_reader_metadata()`, `build_raw_substance_events()`, `build_raw_pathogen_events()` |

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
- Never read from `data/` or `secrets/` in tests.
- Use `withr::with_envvar()` to set environment variables in tests. Never use `Sys.setenv()` directly -- it leaks state between tests.
- Use `withr::local_tempfile()` for temporary files. Never use `tempfile()` directly -- `local_tempfile()` ensures cleanup.
- Internal (non-exported) functions are accessed via `neoipcr:::fn_name()` in tests.
- **Test failure scenarios, not implementation details.** When a function can fail for multiple reasons (e.g. missing key vs. malformed value), test each failure path independently. Do not assume two failure modes produce the same result just because they happen to share an internal code path today.
