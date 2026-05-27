# neoipcr — API design note (Phase 1 of public-interface refinement)

**Purpose.** This note is the single deliverable of Phase 1 of [tasks/neoipcr-public-interface-refinement.md](../../tasks/neoipcr-public-interface-refinement.md) (see also the CRAN-release coordination plan at [projects/neoipcr-cran-release-plan.md](../../projects/neoipcr-cran-release-plan.md)). It pins the public-surface decisions needed to unblock Phase 2 (helper promotion), Phase 3 (renames), and the sibling [lifecycle-badges task](../../tasks/neoipcr-lifecycle-badges.md). Nothing in this note changes code; the commitments here are executed by later phases.

**Audiences addressed.** Data scientists at partner departments, external researchers building their own reports/tools, clinicians running ad-hoc queries, and the internal NeoIPC pipeline (Partner-/Reference-/Validation-Report, Patient-Data-Report, the .NET reporting service, the DHIS2 app shell). All four are first-class.

**Pre-alpha release status.** The package is pre-alpha (version `0.1.0` in [DESCRIPTION](DESCRIPTION)). Renames and promotions are direct — no `lifecycle::deprecate_warn()` aliases. Internal Surveillance-Toolkit callers move in lockstep with API changes.

---

## §1. Status

**Phase 1 exit-criterion checklist.** All items ticked — every decision below surfaces in §10 for PI resolution; content is ready for Phase 2 to consume once D-A…D-H are resolved.

- [x] All 24 `export()` lines in [NAMESPACE](NAMESPACE) have a row in §3 with eight columns populated.
- [x] All 3 `S3method()` entries have a row in §3 with dispatch class recorded.
- [x] Each of the four custom classes (`neoipcr_ds`, `neoipcr_rep_ds`, `neoipcr_ref_ds`, `neoipcr_tbl_sr_ref`) has a §4 subsection with constructor site, columns, invariants, and pointer to the relevant `R/schema-*.R`.
- [x] The `_iaf` / `_sbd` / `_udr` subclasses each have a §4 row (§4.5) under the post-task-1.2 names with an explicit "depends on [task 1.2](../../tasks/neoipcr-class-slug-rename.md)" callout. Three additional subclasses surfaced and are flagged in §3.2 for task 1.2 expansion.
- [x] Every helper from the Phase 2 sketch of the task file appears in §5 (46 rows) with target `R/*.R` file + signature. Rejected promotions listed in §5.2 with rationale.
- [x] Every `gettext` / `gettextf` call site in `R/` appears in §6's classified call-site table. F-class + B-class (13 sites) individually enumerated in §6.1.1; M-class (~75 sites) aggregated by file in §6.1.2 with explicit counts and a single shared migration action.
- [x] §6 states the architectural split (messages vs. data) as a decision, not a proposal — §6.5, governed by D-H (§10.8) for PI confirmation.
- [x] §6 cites 5 established R-ecosystem precedents — §6.4 (countrycode, Writing R Extensions, scales, rlang/cli, potools).
- [x] The unified locale-resolution chain is stated in §6.7 and referenced from relevant decisions in §10.
- [x] §6 carries a per-call-site migration plan for Phase 2 — §6.6.
- [x] Every package-data candidate has a §7 row (7 rows); infectious-agent files are explicitly held for [csv-to-yaml-migration](../../tasks/csv-to-yaml-migration.md) in §7.4.
- [x] §8 states the GT-styling boundary decision (S3 `as_gt.*` methods); describes the DESCRIPTION delta (add `gt` to `Suggests`).
- [x] §9 covers every rename required by Phase 3 of the task file — two function renames (D-C, D-D) plus coordination with task 1.2 for class renames.
- [x] §10 lists D-A through D-I, each with a recommendation and "PI resolution: _pending_" line.
- [x] §9.5 catalogues the cross-surface naming divergence for all 11 result tables + 2 figures across **nine surfaces** (function / class / slot / rate-name / Quarto param / PS switch / `.qmd` filename / heading key / display string); proposes a master-name + derivation-rules scheme; addresses the section-vs-table heading question per row; aligns code identifiers with PDF display strings via the po4a translation cycle; coordinates with task 1.2.
- [x] §11 points at three in-tree roxygen exemplars and the test-fixture helpers.
- [x] §12 cross-refs five sibling tasks.

**Decisions pending PI sign-off.** See §10 for full context on each.

| ID | Decision | Recommendation | PI resolution |
|----|----------|----------------|---------------|
| D-A | Audience tier per exported symbol | Defaults from usage patterns (see §3) | pending |
| D-B | GT-styling entry point shape | `as_gt.neoipcr_tbl_sr_ref()` (S3) | pending |
| D-C | `get_benchmark_data()` rename | **Withdraw** — function combines, doesn't compute (see §10.3) | pending |
| D-D | CI-function prefix unification | Rename all three to `neoipcr_*_ci()` (matches package name) | pending |
| D-E | Long table-function-name shortening | Keep as-is; rely on `@family` grouping | pending |
| D-F | Accessor-vs-raw-data per package-data candidate | Accessor per taxonomy (follows `get_pathogen_taxonomy()`) | pending |
| D-G | Message-catalog scope | Single `R-neoipcr.pot` | pending |
| D-H | Localization architecture split | See §10 / §6 for the full proposal | pending |
| D-I | Cross-surface naming alignment (9 surfaces, code + report display) | Master-name + heading-treatment table in §9.5.4 / §10.9; supersedes parts of task 1.2; alignment extends to PDF display strings via po4a cycle | pending |

---

## §2. Scope and non-goals

**In scope (this note).** Inventory and design paperwork only. Produces commitments for later phases of [task 1.1](../../tasks/neoipcr-public-interface-refinement.md) to execute.

**Out of scope (this note).** Every code change. Specifically: no new `R/*.R` files, no `NAMESPACE` edits, no roxygen regeneration, no `DESCRIPTION` edits, no rename applied anywhere, no Surveillance-Toolkit `_setup.qmd` / `Generate-*.R` edits, no `data-raw/sysdata.R` extensions, no `po/` catalog regeneration, no `man/*.Rd` edits, no vignettes / README.Rmd / `_pkgdown.yml`, no convenience-layer (`neoipcr_quickstart()`). Those all happen in Phases 2–6 of the task file — governed by the commitments below.

**Out of scope (permanently or by handoff to sibling tasks).** Listed in §12.

---

## §3. Public surface inventory

All 24 `export()` entries + 3 `S3method()` entries from [NAMESPACE](NAMESPACE).

Where a row's return-class slug is scheduled for rename by [task 1.2 (neoipcr-class-slug-rename.md)](../../tasks/neoipcr-class-slug-rename.md), the current slug is given first and the candidate post-rename name in a parenthetical. Slug-rename scheme in task 1.2 is labelled "suggestions, not commitments" — this note does not pin the scheme, it cross-references it.

| # | Symbol | File | Signature sketch | Returns class | Audience tier | Lifecycle | Rename proposal | Notes |
|---|--------|------|------------------|---------------|---------------|-----------|-----------------|-------|
| 1 | `import_dhis2` | [R/import-dhis2.R](R/import-dhis2.R) | `(connection_options, dataset_options)` | `neoipcr_ds` | external-stable | stable | — | Core entry point; invokes the 5-step auth chain. |
| 2 | `dhis2_connection_options` | [R/dhis2-connect.R](R/dhis2-connect.R) | `(token, username, session_id, scheme, hostname, port, path)` | `neoipcr_dhis2_conopt` (→ `neoipcr_dhis2_connection_options`) | external-stable | stable | — | Auth entry; documents auth fallback chain. |
| 3 | `dhis2_dataset_options` | [R/dhis2-options.R](R/dhis2-options.R) | `(..., translate = TRUE, locale = NULL)` | `neoipcr_dhis2_dsopt` (→ `neoipcr_dhis2_dataset_options`) | external-stable | stable | — | Data-protection gates + locale entry point. |
| 4 | `calculate_reference_data` | [R/calc-api.R](R/calc-api.R) | `(x, use_cache = TRUE, redact = TRUE)` | `neoipcr_ref_ds` (→ `neoipcr_reference_ds`) | internal-stable | stable | — | Reference-Report entry. |
| 5 | `calculate_department_data` | [R/calc-api.R](R/calc-api.R) | `(x, use_cache = TRUE)` | `neoipcr_rep_ds` (→ `neoipcr_report_ds`) | internal-stable | stable | — | Partner-Report entry. |
| 6 | `get_benchmark_data` | [R/calc-api.R](R/calc-api.R) | `(...)` | `neoipcr_bnch_ds` *(not in task 1.2 rename scope — see §3.2)* | internal-stable | stable | — (D-C: rename withdrawn — function combines pre-computed datasets, doesn't calculate) | Side-by-side stitcher with one CI fix-up; not a calculator. Roxygen at calc-api.R:311 says "Creates ... from ...". |
| 7 | `pretty_names` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` (S3 generic) | varies | internal-stable | experimental | — | S3 generic. |
| 8 | `pretty_names.default` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` | identity on `x` | internal-stable | experimental | — | Default method. |
| 9 | `pretty_names.neoipcr_tbl_sr_ref` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` | `neoipcr_tbl_sr_ref` (→ `neoipcr_surgery_rate_table_ref` — candidate) | internal-stable | experimental | — | Translates column/row names; contains the multi-arg `gettext` bug (A4a B-1). |
| 10 | `get_usage_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_udr` / `_udr_ref` (→ `neoipcr_usage_density_rate_table`) | internal-stable | stable | — | Part of rate-table family (@family grouping — §11). |
| 11 | `get_antibiotic_utilisation_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `tibble` with class markers | internal-stable | stable | — | Return class slug deferred; no dedicated slug in task 1.2. |
| 12 | `get_surgery_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache)` | `neoipcr_tbl_sr` (→ `neoipcr_surgery_rate_table`) | internal-stable | stable | — | — |
| 13 | `get_ref_surgery_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(ref, use_cache)` | `neoipcr_tbl_sr_ref` (→ `neoipcr_surgery_rate_table_ref` — candidate) | internal-stable | stable | — | Reference variant; 13 columns, see §4.4. |
| 14 | `get_incidence_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_idr` (→ `neoipcr_incidence_density_rate_table`) | internal-stable | stable | — | — |
| 15 | `get_dev_ass_incidence_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_daidr` (→ `neoipcr_device_associated_incidence_density_rate_table`) | internal-stable | stable | — | Long name — D-E recommends keeping. |
| 16 | `get_infectious_agent_detection_rate_per_inf_type_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_iadrpit` (→ `neoipcr_agent_detection_rate_per_infection_type_table`) | internal-stable | stable | — | — |
| 17 | `get_infectious_agent_detection_rate_per_agent_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_iadrpa` / `_ref` (→ `neoipcr_agent_detection_rate_per_agent_table`) | internal-stable | stable | — | — |
| 18 | `get_abr_infection_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_abr_ir` / `_ref` (→ `neoipcr_abr_infection_rate_table` — keeps `abr`) | internal-stable | stable | — | — |
| 19 | `get_organism_resistance_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_org_rr` / `_ref` (→ `neoipcr_organism_resistance_rate_table`) | internal-stable | stable | — | — |
| 20 | `get_resistance_test_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_rtr` / `_ref` *(not in task 1.2 rename scope — see §12)* | internal-stable | stable | — | Slug `_rtr` not listed in task 1.2 rename table. |
| 21 | `get_secondary_bsi_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_sec_bsi` / `_ref` *(not in task 1.2 rename scope — see §12)* | internal-stable | stable | — | Slug `_sec_bsi` not listed in task 1.2 rename table. |
| 22 | `neoipc_poisson_ci` | [R/ci.R](R/ci.R) | `(events, exposure, multiplier = 1000, conf.level = 0.95)` | `list(rate, lower, upper)` | external-stable | stable | `neoipc_poisson_ci` → `neoipcr_poisson_ci` (D-D) | Existing prefix `neoipc_` doesn't match package name `neoipcr`. Well-documented; has `@examples`. |
| 23 | `neoipc_wilson_ci` | [R/ci.R](R/ci.R) | `(x, n, conf.level = 0.95)` | `list(proportion, lower, upper)` | external-stable | stable | `neoipc_wilson_ci` → `neoipcr_wilson_ci` (D-D) | Existing prefix `neoipc_` doesn't match package name `neoipcr`. Well-documented; has `@examples`. |
| 24 | `bootstrap_quantile_ci` | [R/ci.R](R/ci.R) | `(events, exposure, type, multiplier, B, conf.level, seed)` | `tibble` (6 cols) | external-stable | experimental | `bootstrap_quantile_ci` → `neoipcr_bootstrap_quantile_ci` (D-D) | Not integrated into rate-table pipeline. |
| 25 | `get_pathogen_taxonomy` | [R/pathogens.R](R/pathogens.R) | `(ids = NULL)` | `tibble` (taxonomic hierarchy) | external-stable | stable | — | Roxygen model for accessors over package data (§11). |
| 26 | `is_valid_ichi_code` | [R/ichi.R](R/ichi.R) | `(code)` | `logical` | external-stable | experimental | — | Syntax validator only. Bundling ICHI code list blocked by WHO licensing (see §7). |
| 27 | `print.neoipcr_dhis2_conopt` | [R/dhis2-connect.R](R/dhis2-connect.R) | `(x, ...)` S3 method | `invisible(x)` | external-stable | stable | — | Dispatch class: `neoipcr_dhis2_conopt` → `neoipcr_dhis2_connection_options`. |

### §3.1. Naming patterns and inconsistencies (A1 findings, feed into §9)

- **CI function prefix inconsistency + package-name mismatch** → D-D. Two functions carry a `neoipc_` prefix that doesn't even match the package name (`neoipcr`); the third has no prefix at all. Recommendation: rename all three to `neoipcr_*_ci()`.
- **`get_*` vs `calculate_*` semantic confusion** → D-C. `get_benchmark_data()` computes; its siblings are `calculate_*`.
- **Result-table class hierarchy is ad-hoc**: no consistent inheritance contract between `_tbl_*` and `_tbl_*_ref` variants. `neoipcr_ref_ds` extends `neoipcr_rep_ds` (consistent); the table classes do not. Not this note's scope to fix — flag for consideration when rate-table expansion tasks land.
- **(Resolved by D-D)** Earlier draft of this section dismissed the `neoipcr` package vs. `neoipc_` function prefix mismatch as "cosmetic, not worth fixing." On reflection (and after PI pushback) the rename is the right move — see D-D and §9.1. Keeping this bullet visible because the original dismissal was wrong and worth flagging for posterity.

### §3.2. Classes not covered by task 1.2 (gap report)

Audit A1 discovered three return-class slugs that are **not listed in [task 1.2's rename table](../../tasks/neoipcr-class-slug-rename.md#result-tables)**:

| Current slug | Where returned | Meaning | Suggested addition to task 1.2 |
|---|---|---|---|
| `neoipcr_bnch_ds` | `get_benchmark_data()` | Merged benchmark dataset | `neoipcr_benchmark_ds` |
| `neoipcr_tbl_rtr` / `neoipcr_tbl_rtr_ref` | `get_resistance_test_rate_table()` | Conditional resistance-testing rates | per §9.5 master |
| `neoipcr_tbl_sec_bsi` / `neoipcr_tbl_sec_bsi_ref` | `get_secondary_bsi_rate_table()` | Secondary-BSI rates by infection type | per §9.5 master |

**Recommendation**: add these rows to task 1.2 before it executes, so the rename is comprehensive. The two `neoipcr_tbl_*` slugs should adopt the §9.5 master-name scheme (D-I) rather than be renamed independently — see §9.5.6.

---

## §4. Custom-class schemas

The four main classes span two shapes: dataset lists (`neoipcr_ds`, `neoipcr_rep_ds`, `neoipcr_ref_ds`) and individual result tables (`neoipcr_tbl_sr_ref`). Schema authority lives in `R/schema-*.R`; this section gives constructor sites, member tibbles, invariants, predicates — and references those schema files rather than restating column-by-column.

### §4.1. `neoipcr_ds`

**Constructor site**: [R/import-dhis2.R:230](R/import-dhis2.R#L230) — `structure(..., class = c("neoipcr_ds", "list"))`.

**Shape**: list of tibbles (member slots) + `metadata` + `.cache`.

**Member tibbles** (slot name → tibble class slug → schema file):

| Slot | Class slug (current) | Post-1.2 candidate | Schema authority |
|---|---|---|---|
| `patients` | `neoipcr_pat` | `neoipcr_patient` | [R/schema-patients.R](R/schema-patients.R) → `patients_cols` |
| `enrollments` | `neoipcr_enr` | `neoipcr_enrollment` | [R/schema-enrollments.R](R/schema-enrollments.R) → `enrollments_cols` |
| `enrollment_notes` | — | (pseudo-parent inherits) | [R/schema-notes.R](R/schema-notes.R) → `enrollment_notes_cols` |
| `events` | `neoipcr_evt` | `neoipcr_event` | [R/schema-events.R](R/schema-events.R) → `events_cols` *(absorbed former `eventDetails` in phase-b-event-details)* |
| `eventNotes` | `neoipcr_evn` | `neoipcr_event_note` | [R/schema-notes.R](R/schema-notes.R) → `event_notes_cols` |
| `admissionData` | `neoipcr_adm` | `neoipcr_admission_data` | [R/schema-event-data.R](R/schema-event-data.R) → `admissionData_cols` |
| `surveillanceEndData` | `neoipcr_end` | `neoipcr_surveillance_end_data` | [R/schema-event-data.R](R/schema-event-data.R) → `surveillanceEndData_cols` |
| `sepsisData` | `neoipcr_bsi` | `neoipcr_sepsis_data` | [R/schema-event-data.R](R/schema-event-data.R) → `sepsisData_cols` |
| `necData` | `neoipcr_nec` | `neoipcr_nec_data` | [R/schema-event-data.R](R/schema-event-data.R) → `necData_cols` |
| `pneumoniaData` | `neoipcr_hap` | `neoipcr_pneumonia_data` | [R/schema-event-data.R](R/schema-event-data.R) → `pneumoniaData_cols` |
| `surgeryData` | `neoipcr_pro` | `neoipcr_surgery_data` | [R/schema-event-data.R](R/schema-event-data.R) → `surgeryData_cols` |
| `ssiData` | `neoipcr_ssi` | `neoipcr_ssi_data` | [R/schema-event-data.R](R/schema-event-data.R) → `ssiData_cols` |
| `substanceDays` | `neoipcr_sbd` | `neoipcr_substance_day` | [R/schema-event-data.R](R/schema-event-data.R) → `substanceDays_cols` |
| `infectiousAgentFindings` | `neoipcr_iaf` | `neoipcr_agent_finding` | [R/schema-event-data.R](R/schema-event-data.R) → `findings_cols` |
| `unknownPathogenNames` | *(currently unclassed — intentional, awaiting task 1.2)* | `neoipcr_unknown_pathogen_name` | [R/schema-event-data.R](R/schema-event-data.R) → `unknownPathogenNames_cols` |
| `metadata` | `neoipcr_metadata` | `neoipcr_metadata` *(keep)* | [R/schema-orgunits.R](R/schema-orgunits.R) + [R/dhis2-metadata-reference.R](R/dhis2-metadata-reference.R) |
| `.cache` | (environment) | — | [R/cache.R](R/cache.R) |

**Invariants**:

- **Data-protection contract** enforced by `assert_data_protection()` in [R/data-protection.R](R/data-protection.R) — aborts on reader regression. See CLAUDE.md § "Integrated data protection".
- **Three-mode schema contract** per fact tibble: `"no"` → 0×0 (entity gate rejects); `"pseudo"` → minimal key-only; `"full"` → full column set per `include_*` flags. Contract is authoritative in `R/schema-tools.R`.
- **Foreign-key invariants**: `patients ← enrollments ← events` chain on integer keys (`patient_key`, `enrollment_key`, `event_key`) — never on DHIS2 UIDs (see CLAUDE.md § "Joining tibbles").
- **Redundant hierarchy keys**: `department_key`, `hospital_key`, `country_key`, `world_bank_class_key` appear directly on `patients`, `enrollments`, and `events` — chain-breakable per CLAUDE.md § "Redundant foreign keys".
- **Conditional companion gates**: user fields gate on `include_user`; timestamps gate on `include_timestamps`; deletion flag gates on `include_deleted`; DHIS2 IDs gate on `include_dhis2_ids`.

**Predicates**: `is_neoipcr_ds()`, `check_neoipcr_ds()`, plus the compound guards `check_neoipcr_ds_or_rep_ds()`, `check_neoipcr_ds_or_ref_ds()` — all in [R/types-check.R](R/types-check.R).

---

### §4.2. `neoipcr_rep_ds` → post-1.2 `neoipcr_report_ds`

**Constructor site**: [R/calc-api.R:308](R/calc-api.R#L308) — `structure(..., class = c("neoipcr_rep_ds", "list"))`.

**Shape**: list of tibbles — computed view of an imported `neoipcr_ds`, aggregated at department level.

**Member tibbles**:

- `metadata` — list: `calculated` (timestamp), `dataset_options`, `data_up_to`, `effective_analysis_period`, `hospitals`, `departments`, `countries`.
- `birth_weight_figure`, `gestational_age_figure` — histogram tibbles for figures.
- `n_departments`, `n_patients`, `n_enrollments`, `n_patient_days` — summary counts.
- `n_surgical_departments`, `n_surgical_patients`, `n_surgical_procedures` — surgery-specific counts.
- `n_infections` — tibble (`inf_type`, `total`).
- Sixteen result tables: `usage_density_rate_table`, `antibiotic_utilisation_table`, `surgery_rate_table`, `incidence_density_rate_table`, `dev_ass_incidence_density_rate_table`, `infectious_agent_detection_rate_per_agent_table`, `infectious_agent_detection_rate_per_inf_type_table`, `abr_infection_rate_table`, `organism_resistance_rate_table`, `secondary_bsi_rate_table`, `resistance_test_rate_table` — each a tibble with factors, counts, pooled rate, and 95 % Poisson CI. **No quartile columns** — that's what distinguishes `neoipcr_rep_ds` from `neoipcr_ref_ds`.

**Invariants**:

- Computed via `calculate_department_data()`. Requires `include_patient`, `include_enrollment`, `include_event`, `include_department` each at `"pseudo"` or `"full"`.
- Summary counts inherit the user's `dataset_options` (display-purpose metadata).
- Result tables carry pooled-rate + CI only — no department-level distribution statistics.

**Predicates**: `is_neoipcr_rep_ds()`, `check_neoipcr_rep_ds()`, `check_neoipcr_ds_or_rep_ds()` — in [R/types-check.R](R/types-check.R).

---

### §4.3. `neoipcr_ref_ds` → post-1.2 `neoipcr_reference_ds`

**Constructor site**: [R/calc-api.R:186](R/calc-api.R#L186) — `structure(..., class = c("neoipcr_ref_ds", "neoipcr_rep_ds", "list"))`. **Inherits from `neoipcr_rep_ds`.**

**Shape**: same member structure as `neoipcr_rep_ds` with additional quartile statistics.

**Delta from §4.2**:

- Every result table gains `q1`, `q2`, `q3` (25th / 50th / 75th percentiles of department-level rates).
- Tables for which bootstrap CIs are computed gain `q1_ci_lower` / `q1_ci_upper` / `q2_ci_lower` / `q2_ci_upper` / `q3_ci_lower` / `q3_ci_upper`.

**Invariants** (in addition to §4.2):

- Computed via `calculate_reference_data()`. Requires `country_key` to be present in metadata (warns otherwise).
- Quartiles are set to `NA` when `n_departments < 5`, or when `round(100 / pooled) >= median_patient_days` (small-cell-exposure protection).
- Inheritance contract: any generic that dispatches on `neoipcr_rep_ds` also applies to `neoipcr_ref_ds`. Only quartile-specific code paths need `inherits(x, "neoipcr_ref_ds")` checks.

**Predicates**: `is_neoipcr_ref_ds()`, `check_neoipcr_ref_ds()`, `check_neoipcr_ds_or_ref_ds()` — in [R/types-check.R](R/types-check.R).

---

### §4.4. `neoipcr_tbl_sr_ref` → post-1.2 `neoipcr_surgery_rate_table_ref` (candidate — see §3.1)

**Constructor sites**: [R/calc-tables.R:506](R/calc-tables.R#L506) and [R/calc-tables.R:587](R/calc-tables.R#L587) — two `add_class()` calls in `get_ref_surgery_rate_table()`.

**Shape**: tibble (a single result table, not a dataset list).

**Columns**:

| Column | Type | Semantics |
|---|---|---|
| `pro_cat` | factor | Procedure category code — `"overall"` or one of the ICHI-derived categories (see §7 row 1). |
| `n` | integer | Count of procedures in category. |
| `pooled` | double | Overall pooled rate (per-100 units). |
| `ci_lower`, `ci_upper` | double | 95 % Poisson CI on pooled rate. |
| `q1`, `q2`, `q3` | double | 25th / 50th / 75th percentile of department-level rates (NA when gated out). |
| `q1_ci_lower`, `q1_ci_upper`, `q2_ci_lower`, `q2_ci_upper`, `q3_ci_lower`, `q3_ci_upper` | double | Bootstrap 95 % CI on each quartile (NA when gated out). |

**Invariants**:

- Fixed category vocabulary: rows correspond to `c("cvc", "pvc", "vs", "inv", "niv", "ab", "a", "w", "r", "human_milk", "probiotic", "kangaroo_care")` plus `"overall"`. Missing categories are added as rows with `n = 0`, `pooled = NA`.
- Quartiles dropped to `NA` when `n_departments < 5` OR when `round(100 / pooled) >= median_patient_days` (protective gate).
- **No schema file** — column contract is fixed by `get_ref_surgery_rate_table()` directly (anomaly relative to `neoipcr_ds` members).

**Predicates**: no dedicated `is_*` or `check_*`; callers use `inherits(x, "neoipcr_tbl_sr_ref")` directly. **This is a gap**: `pretty_names.neoipcr_tbl_sr_ref` dispatches on the class but the package does not assert it — callers constructing the class by hand bypass the schema.

**Recommendation**: add `is_neoipcr_tbl_sr_ref()` / `check_neoipcr_tbl_sr_ref()` to [R/types-check.R](R/types-check.R) when Phase 2 touches the GT-styling layer. Out of scope for this note; flagged here for Phase 2.

---

### §4.5. Sub-class reference (task-1.2 coordination)

The four cryptic-slug sub-classes below are all pending rename under [task 1.2](../../tasks/neoipcr-class-slug-rename.md). Entries here preserve construction sites for traceability; post-rename names are the **candidates** listed in task 1.2 (scheme is not yet pinned).

| Current slug | Post-1.2 candidate | Parent shape | Construction site | Represents |
|---|---|---|---|---|
| `neoipcr_iaf` | `neoipcr_agent_finding` | tibble slot on `neoipcr_ds` | [R/import-dhis2.R:207](R/import-dhis2.R#L207) | Per-event pathogen identifications + resistance markers. |
| `neoipcr_sbd` | `neoipcr_substance_day` | tibble slot on `neoipcr_ds` | [R/import-dhis2.R:206](R/import-dhis2.R#L206) | Antibiotic-substance-day exposures per event/enrollment. |
| `neoipcr_tbl_udr` | `neoipcr_usage_density_rate_table` | result tibble | [R/calc-tables.R:199](R/calc-tables.R#L199) | Usage-density rate table (pooled rates per therapeutic category). |
| `neoipcr_tbl_udr_ref` | `neoipcr_usage_density_rate_table_ref` | result tibble | [R/calc-tables.R:273](R/calc-tables.R#L273) | Reference variant with quartile statistics. |

All four **depend on [task 1.2](../../tasks/neoipcr-class-slug-rename.md)**. See §3.2 for additional classes (`neoipcr_bnch_ds`, `neoipcr_tbl_rtr(_ref)`, `neoipcr_tbl_sec_bsi(_ref)`) that surfaced during this audit and should be added to task 1.2's rename table.

---

## §5. Report-helper promotion list

42 helpers identified. All Phase-2-sketch candidates in the task file confirmed; additions and refinements below. Total ~1200 LOC to promote; 8 new `R/*.R` files + extensions to existing `R/scales.R` and `R/dhis2-metadata.R`.

### §5.1. Promotion table

| # | Helper | Current location(s) | LOC | Target `R/*.R` | Proposed signature | Exp? | Deps | Blocking |
|---|--------|---------------------|-----|----------------|---------------------|------|------|----------|
| 1–4 | `log_info`, `log_verbose`, `log_debug`, `log_warn` | [Partner-Report/Generate-PartnerData.R:19–21](../Surveillance-Toolkit/reports/Partner-Report/Generate-PartnerData.R#L19), [Reference-Report/Generate-ReferenceData.R:19–21](../Surveillance-Toolkit/reports/Reference-Report/Generate-ReferenceData.R#L19) | ~3 each | `R/logging.R` | `log_<level>(msg, ...)` → `invisible(NULL)` | Y | base | Global verbosity state must become an env var or option; do not keep a package-global. |
| 5 | `get_validation_exceptions` | [common/helpers.R:151–159](../Surveillance-Toolkit/reports/common/helpers.R#L151) | 9 | `R/format.R` (or `R/validation.R`) | `get_validation_exceptions(path) → tibble \| FALSE` | Y | readr (already) | — |
| 6 | `parse_locales` | [common/helpers.R:4–53](../Surveillance-Toolkit/reports/common/helpers.R#L4) | 50 | `R/strings.R` | `parse_locales(x) → list(language, territory, codeset, modifier)` | Y | base | Foundation of cascade. |
| 7 | `get_string_resources` | [common/helpers.R:55–110](../Surveillance-Toolkit/reports/common/helpers.R#L55) | 56 | `R/strings.R` | `get_string_resources(localeObj) → list` | Y | yaml | Central to all report localization; feeds the data-label layer per D-H. |
| 8 | `get_localised_path` | [common/helpers.R:112–126](../Surveillance-Toolkit/reports/common/helpers.R#L112) | 15 | `R/strings.R` | `get_localised_path(file_name, language, territory) → character` | Y | base | — |
| 9 | `include_localised` | [common/helpers.R:128–139](../Surveillance-Toolkit/reports/common/helpers.R#L128) | 12 | `R/strings.R` | `include_localised(file_name)` | Y | readr, knitr | Knitr-specific but worth exporting for extension. |
| 10 | `get_localised_world_bank_class_names` | [common/helpers.R:141–149](../Surveillance-Toolkit/reports/common/helpers.R#L141) | 9 | `R/format.R` | `get_localised_world_bank_class_names(x) → character` | Y | purrr | — |
| 11 | `format_integer` | [common/helpers.R:206–211](../Surveillance-Toolkit/reports/common/helpers.R#L206) | 6 | `R/format.R` | `format_integer(x, big_mark)` | Y | base | `sR` → explicit arg (see §5.3 Open design issues). |
| 12 | `format_countries` | [common/helpers.R:217–253](../Surveillance-Toolkit/reports/common/helpers.R#L217) | 37 | `R/format.R` | `format_countries(countries, sR)` | Y | dplyr, rlang | Depends on sR accessor pattern (§5.3). |
| 13 | `format_range_filter` | [common/helpers.R:261–271](../Surveillance-Toolkit/reports/common/helpers.R#L261) | 11 | `R/format.R` | `format_range_filter(from, to, unit, all_label)` | Y | base | — |
| 14 | `format_dataset_resources` | [common/helpers.R:278–372](../Surveillance-Toolkit/reports/common/helpers.R#L278) | 95 | `R/format.R` | `format_dataset_resources(metadata, counts, sR) → list` | Y | lubridate, dplyr | Largest formatter; structured locale-aware output. |
| 15 | `parse_args` | [common/parse-args.R:20–77](../Surveillance-Toolkit/reports/common/parse-args.R#L20) | 58 | `R/cli.R` | `parse_args(args, long_map, short_map) → list` | Y | base | Python-free — do NOT introduce `argparse`. |
| 16–20 | `as_null`, `as_bool`, `as_date_or_null`, `as_number_or_null`, `as_vector_or_null` | [common/parse-args.R:87–147](../Surveillance-Toolkit/reports/common/parse-args.R#L87) | 5–10 each | `R/cli.R` | coercers | Y | base, lubridate | — |
| 21 | `get_connection_options` | [common/helpers.R:161–169](../Surveillance-Toolkit/reports/common/helpers.R#L161) | 9 | `R/dhis2-options.R` | `get_connection_options(...)` thin wrapper with report defaults | N | neoipcr itself | Extension point for external builders with their own defaults. |
| 22 | `get_dataset_options` | [common/helpers.R:171–204](../Surveillance-Toolkit/reports/common/helpers.R#L171) | 34 | `R/dhis2-options.R` | `get_dataset_options(...)` with report defaults | N | neoipcr itself | Pairs with #21. |
| 23 | `no_data_table` | [common/helpers.R:374–387](../Surveillance-Toolkit/reports/common/helpers.R#L374) | 14 | `R/gt.R` | `no_data_table(...)` | Y | base | Pairs with GT layer. |
| 24 | `compute_col_widths` | [Reference-Report/_setup.qmd:152–181](../Surveillance-Toolkit/reports/Reference-Report/_setup.qmd#L152), [Partner-Report/_setup.qmd:1535–1577](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1535) — **duplicated** | ~30 each | `R/gt.R` | `compute_col_widths(tbl_data, stub_min, has_reference, base_font_size, bare_w, n_w) → list` | Y | base | Unify the two variants. Partner version slightly larger. |
| 25–28 | `labels_bw`, `breaks_bw`, `labels_wk`, `breaks_wk` | [Reference-Report/_setup.qmd:19–46](../Surveillance-Toolkit/reports/Reference-Report/_setup.qmd#L19) | 6–9 each | `R/scales.R` (extend existing) | scale helpers for ggplot axes | Y | base | Coordinate with `feature-bw-ga-stratification.md` task. |
| 29 | `detect_mixed_wb_classes` | [Reference-Report/_setup.qmd:52–87](../Surveillance-Toolkit/reports/Reference-Report/_setup.qmd#L52) | 36 | `R/dhis2-metadata.R` | companion to #30 | Y | dplyr | Unify with #30 (see A3 Open design issues §5.3). |
| 30 | `detect_wb_class_mismatch` | [Partner-Report/_setup.qmd:1353–1412](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1353) | 60 | `R/dhis2-metadata.R` | unified helper | Y | dplyr | Unify with #29. |
| 31 | `extract_counts` | [Partner-Report/_setup.qmd:1707](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1707) | — | `R/calc-api.R` (alongside `get_benchmark_data`) | per task file sketch | Y | dplyr | Stays in `calc-api.R`; no new `R/benchmark.R` needed. |
| 32 | `coarse_state` | [Partner-Report/_setup.qmd:49–57](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L49) | 9 | `R/metric-state.R` | `coarse_state(state) → character` | Y | dplyr | Foundation of outlier pipeline. |
| 33 | `classify_metrics` | [Partner-Report/_setup.qmd:844–941](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L844) | 98 | `R/metric-state.R` | `classify_metrics(tbl, id_col, combine_cols) → ordered factor` | Y | dplyr, rlang, stats | Core outlier-detection engine. |
| 34 | `has_outliers` | [Partner-Report/_setup.qmd:951–954](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L951) | 4 | `R/metric-state.R` | `has_outliers(state_map) → logical` | Y | base | — |
| 35–36 | `get_outliers_below`, `get_outliers_above` | [Partner-Report/_setup.qmd:957–963](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L957) | 3 each | `R/metric-state.R` | `get_outliers_<dir>(state_map) → character` | Y | base | — |
| 37 | `get_metrics_in_state` | [Partner-Report/_setup.qmd:946–948](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L946) | 3 | `R/metric-state.R` | `get_metrics_in_state(state_map, states) → character` | Y | base | — |
| 38 | `classify_concordance` | [Partner-Report/_setup.qmd:66–86](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L66) | 21 | `R/interpretation.R` | paired-metric classification | Y | dplyr | — |
| 39 | `compose_group_text` | [Partner-Report/_setup.qmd:969–1040](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L969) | 72 | `R/interpretation.R` | engine | Y | dplyr, purrr, rlang | — |
| 40 | `group_outliers` | [Partner-Report/_setup.qmd:1045–1078](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1045) | 34 | `R/interpretation.R` | orchestrator | Y | dplyr | — |
| 41 | `format_outlier_text` | [Partner-Report/_setup.qmd:1193–1275](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1193) | 83 | `R/interpretation.R` | rendering | Y | knitr | — |
| 42 | `render_outlier_section` | [Partner-Report/_setup.qmd:1180–1188](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1180) | 9 | `R/interpretation.R` | — | Y | base | — |
| 43 | `localize_metric_name` | [Partner-Report/_setup.qmd:1084–1177](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L1084) | 94 | `R/interpretation.R` | lookup cascade for composite IDs | Y | rlang, tools | **Depends on D-H** — localization architecture governs the cascade shape. |
| 44 | `select_clause` | [Partner-Report/_setup.qmd:90–117](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L90) | 28 | `R/interpretation.R` | internal | N | glue | Internal to engine. |
| 45 | `select_action` | [Partner-Report/_setup.qmd:120–132](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L120) | 13 | `R/interpretation.R` | internal | N | base | — |
| 46 | `compose_interpretation` | [Partner-Report/_setup.qmd:136–177](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L136) | 42 | `R/interpretation.R` | internal composer | N | glue | — |

### §5.2. Helpers rejected for promotion (stay in reports)

- **Per-table `interpret_*()` narrative callbacks** (~25 functions in Partner-Report, e.g. `interpret_antibiotic_rates`, `interpret_agent_staphylococci`, `interpret_ventilatory_support`) — these encode report-specific narrative copy and metric-pairing configuration, not reusable logic. The engine that runs them (#38–#46) promotes; the copy does not.
- **Per-table `metric_classify_args`** (Partner-Report/_setup.qmd:1685–1691) — table-specific configuration consumed by `classify_metrics()`; stays in the report as caller data.
- **`interpret_distribution()`** (Partner-Report/_setup.qmd:1278–1346) — figure-specific; report copy.
- **CI configuration variables** (`ci_pattern_inline`, `ci_pattern_wrap`, `include_*_ci`, `wrap_ci_default`) — per-report display policy; parameterised into the promoted GT layer via §8.
- **Section visibility flags** (`show_section_surgical` etc.) — Quarto YAML param handling.
- **`.show()` / `.end()`** — trivial Quarto formatting.

### §5.3. Open design issues (flagged for Phase 2)

- **`sR` accessor pattern.** Many formatters (`format_integer`, `format_countries`, `labels_bw`, `labels_wk`, `format_dataset_resources`) depend on a global `sR` (string-resources) object in each report. Promoting them requires either (i) pass `sR` as an explicit argument everywhere, or (ii) a module-internal accessor that resolves from the locale chain. Recommend (i) for explicitness and testability. Phase 2 decision.
- **Logging verbosity control.** Four `log_*` helpers read a global `verbosity` variable. Promote with an option-based control: `log_info()` reads `getOption("neoipcr.verbosity", default = "info")`. Phase 2 decision.
- **`detect_mixed_wb_classes` vs. `detect_wb_class_mismatch` unification.** Different arg shapes, overlapping semantics. Recommend a single `detect_wb_class_mismatch(own_countries = NULL, ref_countries, include_wb_class)` that reduces to `detect_mixed_wb_classes` when `own_countries = NULL`. Phase 2 decision.
- **YAML cascade ownership.** Should neoipcr own the *entire* YAML resource cascade (move `content/` folders from Surveillance-Toolkit), or only the code that traverses it? Recommendation: code moves in Phase 2, the YAML files themselves stay in the reports (because they encode report-specific narrative that belongs to the report, not the package). But the *contract* of the cascade (path shape, precedence order) is owned by neoipcr.
- **Interpretation engine — which helpers are exported?** `classify_concordance`, `compose_group_text`, `group_outliers`, `format_outlier_text`, `render_outlier_section`, `localize_metric_name` — all exported so report authors can call them. `select_clause`, `select_action`, `compose_interpretation` — stay internal.

### §5.4. Missing-piece extractions (promote AND extract)

- **Event-type vocabulary** inlined at [Validation-Report/_setup.qmd:131](../Surveillance-Toolkit/reports/Validation-Report/_setup.qmd#L131) as `factor(..., levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))`. Extract to a utility function `event_types()` in `R/dhis2-events.R` (per CLAUDE.md's R/ file doctrine — DHIS2-specific utility) AND promote the factor labels to package data (§7 row 5, `event_type_map`).
- **Metric-state levels** at [Partner-Report/_setup.qmd:837–840](../Surveillance-Toolkit/reports/Partner-Report/_setup.qmd#L837) — a constant vector `metric_state_levels`. Promote as a package-level constant accessible via `get_metric_state_levels()` in `R/metric-state.R`.

### §5.5. Report-to-helper crossref (promotion impact)

| Helper group | Partner | Reference | Validation | Certificate | Patient-Data |
|---|:---:|:---:|:---:|:---:|:---:|
| `parse_locales`, `get_string_resources`, `get_localised_path` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `include_localised`, `get_localised_world_bank_class_names` | ✓ | ✓ | — | — | — |
| `format_integer`, `format_range_filter`, `format_dataset_resources` | ✓ | ✓ | — | — | — |
| `format_countries` | ✓ | — | — | — | — |
| `get_validation_exceptions` | ✓ | ✓ | ✓ | — | — |
| `log_*` (4 helpers) | ✓ | ✓ | — | — | — |
| `parse_args` + coercers | ✓ | ✓ | — | — | — |
| `get_connection_options` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `get_dataset_options` | ✓ | ✓ | — | — | — |
| `no_data_table`, `compute_col_widths` | ✓ | ✓ | — | — | — |
| `labels_bw`, `breaks_bw`, `labels_wk`, `breaks_wk` | — | ✓ | — | — | — |
| `detect_mixed_wb_classes` | — | ✓ | — | — | — |
| `detect_wb_class_mismatch` | ✓ | — | — | — | — |
| metric-state + interpretation engine (#32–#46) | ✓ | — | — | — | — |

The localization cascade (`parse_locales`, `get_string_resources`, `get_localised_path`) is consumed by all five reports; the metric-state / interpretation engine is Partner-Report-only for now but is the obvious reuse path for future reports.

### §5.6. DESCRIPTION deltas implied by promotion

Add to `Imports`: `yaml`, `purrr` (not currently there; used by several helpers).

Add to `Suggests`: `gt` (for the GT styling layer — §8), `knitr` (if not already transitive), `cli` (if the tidyverse message-style upgrade in D-H goes ahead).

Already present: `dplyr`, `lubridate`, `rlang`, `readr`, `stringr`, `tidyr`, `tidyselect`.

---

## §6. Localization architecture + resolution chain

> **Section status.** A4a (enumeration + classification) and the `display*` inventory are complete — §6.1 through §6.3 below. A4b (ecosystem precedents) and A4c/d (architecture proposal + migration plan) are drafted in §6.4 onward.

### §6.1. Current-state summary

**Scope of the problem is narrower than feared.** Audit A4a enumerated ~87 `gettext()` / `gettextf()` call sites across `repos/neoipcr/R/`:

| Classification | Count | Locations |
|---|---|---|
| M — Message (error/warning/diagnostic shown to a human operator) | ~75 | [R/dhis2-connect.R](R/dhis2-connect.R) (14), [R/validation.R](R/validation.R) (~45), [R/validation-rules-completeness.R](R/validation-rules-completeness.R) (14), [R/validation-rules-enrollment.R](R/validation-rules-enrollment.R) (2) |
| C — Column header | 0 | — |
| F — Factor-level label | 10 | All in [R/calc-procedure-categories.R:215–224](R/calc-procedure-categories.R#L215) (`get_procedure_category_pretty()`) |
| D — DHIS2-display passthrough (redundant wrap) | 0 | — |
| B — Bug (multi-arg `gettext` with silently dropped args) | 2 | [R/calc-api.R:840](R/calc-api.R#L840) and [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) |
| **Total** | **~87** | — |

Two corrections to the A4 problem-statement in the plan file:

1. **No `C` (column header) cases exist today.** `pretty_names()` uses English literals + a `gettext`-based dispatch for one class-specific path ([R/calc-api.R:840](R/calc-api.R#L840)) — and that path is one of the two B-class bugs, not a genuine column-header translation.
2. **No `D` (DHIS2-display passthrough) cases exist today.** The feared pattern — wrapping DHIS2 `displayName` output in `gettext` — does not appear. DHIS2 localization is already consumed correctly (see §6.3).

The real issue is confined to **two files and one function family**: the 10 factor-level labels in `get_procedure_category_pretty()` and the two multi-arg `gettext` bugs.

#### §6.1.1. All F-class and B-class call sites (individually enumerated)

The 12 non-M sites each need distinct treatment in Phase 2, so they're individually enumerated here. M-class sites follow in §6.1.2 aggregated by file.

| File:line | Call shape | Surrounding function | Class | Migration target |
|-----------|------------|----------------------|-------|------------------|
| [R/calc-api.R:840](R/calc-api.R#L840) | `gettext("Procedure category","N","Pooled","Q1","Q2","Q3")` | `pretty_names.neoipcr_tbl_sr_ref` | B | Fix: rewrite as explicit per-column lookup against `procedure_category_labels` tibble (§7.1 row 2). Silently-dropped args are column headers — `"N"`, `"Pooled"`, `"Q1"`…`"Q3"` — that either stay English or move to YAML cascade. |
| [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) | `gettext("Procedure code","Procedure category")` | `get_procedure_categories` | B | Fix: two separate `gettext` calls OR move to YAML cascade (decide alongside §5 row 7 promotion). |
| [R/calc-procedure-categories.R:215](R/calc-procedure-categories.R#L215) | `gettext("Overall")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (§7.1 row 2, code `"overall"`). |
| [R/calc-procedure-categories.R:216](R/calc-procedure-categories.R#L216) | `gettext("Abdominal surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (code `"ab"`). |
| [R/calc-procedure-categories.R:217](R/calc-procedure-categories.R#L217) | `gettext("Neurosurgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:218](R/calc-procedure-categories.R#L218) | `gettext("Inguinal hernia surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:219](R/calc-procedure-categories.R#L219) | `gettext("Cardiac- / large vessel surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:220](R/calc-procedure-categories.R#L220) | `gettext("Lung- / pleural space- / thoracic surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:221](R/calc-procedure-categories.R#L221) | `gettext("Oesophageal surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:222](R/calc-procedure-categories.R#L222) | `gettext("Other")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:223](R/calc-procedure-categories.R#L223) | `gettext("Not a surgical procedure")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:224](R/calc-procedure-categories.R#L224) | `gettext("Not yet categorised")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (code `"to_be_categorised"`). |

The 10 F-class calls at lines 215–224 also require removing the top-of-function `Sys.getlocale("LC_MESSAGES")` at [R/calc-procedure-categories.R:14](R/calc-procedure-categories.R#L14) — the accessor pattern honours the resolution chain via the explicit `locale` argument instead.

#### §6.1.2. M-class call sites (aggregated by file — ~75 total)

All ~75 M-class sites have the same migration action: **stay on `gettext` / `gettextf`**. Aggregated by file:

| File | M-class count | Surrounding functions | Notes |
|------|---------------|----------------------|-------|
| [R/dhis2-connect.R:67–118](R/dhis2-connect.R#L67) | 14 | `read_token`, `get_password`, `get_auth_data` | Token validation + 5-step auth chain error messages. |
| [R/validation.R:6–460](R/validation.R#L6) | ~45 | `validation_rules` formatter closures (rules 1–42) | One per rule body plus a few fragments (e.g., SSI severity at rule 19). |
| [R/validation-rules-completeness.R:9–285](R/validation-rules-completeness.R#L9) | 14 | `validation_rule_5` through `validation_rule_11` | Paired `gettextf("Validation rule %i failed to execute.", N)` + `gettext("The dataset must contain ...")` in each rule's short-circuit handler. |
| [R/validation-rules-enrollment.R:39–40](R/validation-rules-enrollment.R#L39) | 2 | `validation_rule_2` | Same short-circuit pattern. |

No individual-line enumeration needed — every site stays on gettext, catalog is regenerated post-migration (see §6.6).

### §6.2. Locale parameter flow (current state)

- **Entry point**: `dhis2_dataset_options()` accepts `locale = NULL` (signature in [R/dhis2-options.R](R/dhis2-options.R)).
- **Only correct consumer**: [R/dhis2-metadata.R:8–12](R/dhis2-metadata.R#L8) — passes `locale` to the DHIS2 API when `dataset_options$translate && rlang::is_character(dataset_options$locale, n = 1)`.
- **Broken consumers**: every R-level `gettext` / `gettextf` call. They consult `Sys.getlocale("LC_MESSAGES")` and `Sys.getenv("LANGUAGE")` — not `opts$locale`. Concretely: [R/calc-procedure-categories.R:14](R/calc-procedure-categories.R#L14) uses `Sys.getlocale("LC_MESSAGES")` at the top of `get_procedure_category_pretty()`.
- **Result**: a caller who sets `dhis2_dataset_options(locale = "de")` gets German-translated DHIS2 metadata but English-language validation errors and English procedure-category labels.

### §6.3. `display*` field inventory (current)

| Output surface | Tibble on `neoipcr_ds$metadata` | `display*` variant(s) used | Source |
|---|---|---|---|
| World Bank classes | `world_bank_class` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnitGroupSets → organisationUnitGroups` |
| Countries | `countries` | `displayName` (factor-pinned) | `organisationUnits` |
| Hospitals | `hospitals` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnits` (parent level) |
| Departments | `departments` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnits` |
| Antimicrobials | `antimicrobials` | `displayName`, `displayFormName` | `options` |
| Data elements / TEAs | *(not surfaced to user-facing tibbles)* | all four variants, internally | `trackedEntityAttributes`, `programStageDataElements` |

All of these `display*` values are **preserved only when the relevant `include_*` gate is `"full"`**. Under `pseudo` / `no`, they are not written by the reader.

### §6.4. Ecosystem precedents

| # | Package / source | Mechanism demonstrated | Concrete example | Applicability to neoipcr |
|---|------------------|------------------------|------------------|--------------------------|
| 1 | **[`countrycode`](https://github.com/vincentarelbundock/countrycode)** | Locale-as-column-selector on taxonomy tibbles. Data is English-canonical; labels live in separate `country.name.en`, `country.name.de`, `country.name.fr` columns of the `codelist` tibble. Accessor takes a `destination` parameter and reads the appropriate column. | `codelist` (~600 rows) with ~30 locale-keyed columns, e.g. `country.name.en` / `country.name.de` / `country.name.fr`. No gettext wraps around row values. | Directly applicable to the 10 F-class calls. `get_procedure_category_pretty()` migrates to a `procedure_category_labels` tibble in `sysdata.Rda` with `label_en` / `label_de` columns, served by `get_procedure_category_labels(locale = NULL)`. neoipcr's own `get_pathogen_taxonomy()` already uses this idiom — we extend it. |
| 2 | **"Writing R Extensions", § Translations** (R-core / CRAN manual) | Establishes that `gettext` / `gettextf` are for messages shown to users (errors, warnings, diagnostics). Data-layer labels (column names, factor levels, taxonomy terms) are not in scope for PO catalogs — they're part of the package's data contract. | The manual enumerates the set of `gettext`-wrapped user-facing messages and explicitly does not place factor labels, taxonomy tables, or column headers in that set. | Validates the split. The ~75 M-class calls (validation errors, auth diagnostics) stay on `gettext/gettextf`; the 10 F-class calls migrate to locale-keyed columns per #1 above. |
| 3 | **[`scales`](https://scales.r-lib.org)** (tidyverse) | Separates formatter construction from locale consumption. `scales::label_date()`, `scales::label_number()`, `scales::label_currency()` take format strings and locale hints as **explicit parameters**; they do not consult `Sys.getlocale()` silently at definition time. | `scales::label_date(format = "%b %d", locale = "de")` — caller is responsible for passing the locale. | Informs the unified resolution chain (§6.7). Every neoipcr entry point that produces localized output accepts an explicit `locale` argument that wins over `opts$locale`, which wins over `Sys.getlocale("LC_MESSAGES")`, which wins over `"en"`. |
| 4 | **[`rlang`](https://rlang.r-lib.org)** + **[`cli`](https://cli.r-lib.org)** (tidyverse) | Class-tagged condition system: `rlang::abort()` / `cli::cli_abort()` / `cli::cli_warn()` attach a class to the condition; error handlers and tests dispatch on the class, not on the human-readable message. Translation of the message is **orthogonal** — it happens via gettext if you want it, but the class is what programs check. | `rlang::abort(class = "neoipcr_missing_token", message = gettext("…"))`. Tests assert on `class`, not on the translated string. | **Does not force a decision for Phase 1.** neoipcr can stay on `stop(gettext(...))` or migrate to `rlang::abort()` independently of the M/F split. Migration recommended but out of scope here — filed as a Phase 2/3 follow-up task if adopted. |
| 5 | **[`potools`](https://cran.r-project.org/package=potools)** | Package-scoped message catalogs. `potools::create_catalog()` extracts every `gettext(...)` call from `R/` and writes a single `.pot`; translators add `.po` files. `potools` does not split catalogs by message class — the split happens **at the source level** before extraction. | `potools::create_catalog()` → `po/R-<pkg>.pot`. Data-layer labels (taxonomy tibbles in `sysdata.Rda`) are not scanned because they're not `gettext` calls. | Confirms D-G (single `R-neoipcr.pot`). Once the 10 F-class calls migrate out of `gettext`, the catalog shrinks to the ~75 M-class entries — no clutter from data labels. |

**Mainstream consensus.** Five sources converge on: (i) `gettext` is for user-facing messages only; (ii) data labels live in locale-keyed columns or equivalent structured data (not in PO catalogs); (iii) locale selection is explicit (parameter or resolver), never implicit from `Sys.getlocale()` alone; (iv) a single `.pot` per package is standard. neoipcr is already close on (i), (iii-partial), and (iv); the M/F split is what brings it into (ii).

**Non-consensus.** Whether to stay on bare `gettext/gettextf` or upgrade to `rlang::abort()` / `cli::cli_abort()` class-tagged conditions (#4). Both paths are ecosystem-mainstream. Recommend **stay on `gettext` for Phase 2**; file a separate task for the `rlang`/`cli` upgrade if the PI wants it.

### §6.5. Proposed architecture (D-H concrete form)

**Principle.** Every user-facing string in `neoipcr` belongs to exactly one of two tracks:

- **Messages track.** Error, warning, and diagnostic strings shown to human operators at runtime. Owned by gettext + `R-neoipcr.po` / `R-neoipcr.pot`. Wrapped by `gettext()` / `gettextf()` at the call site. Translators work off the PO catalog.
- **Data track.** Column headers, factor labels, taxonomy terms, and any string that is part of the *value* returned by a neoipcr function. Owned by locale-keyed columns in `sysdata.Rda` tibbles or in ingested YAML resource cascades. Accessed via accessor functions that honour the locale-resolution chain.

Nothing is in both tracks.

**Mechanism per current category (from §6.1):**

| Class | Count | Mechanism under new architecture |
|---|---|---|
| **M** — Message | ~75 | Stay on `gettext` / `gettextf`. No call-site change in Phase 2 (other than the `Sys.getlocale` cleanup in §6.7's locale chain). `R-neoipcr.pot` regenerated after the F-class calls exit. |
| **C** — Column header | 0 | None exist today. If new entry points add column headers, they use the YAML resource cascade (via `get_string_resources()` promoted in §5 row 7), not `gettext`. |
| **F** — Factor-level label | 10 | Migrate to package data. All 10 are procedure-category labels in [R/calc-procedure-categories.R:215–224](R/calc-procedure-categories.R#L215). Target: the `procedure_category_labels` tibble proposed in §7 row 2, served by a new `get_procedure_category_labels(locale = NULL)`. Migration is the only non-trivial code change in Phase 2. |
| **D** — DHIS2-display passthrough | 0 | None exist today. Entry points already consume DHIS2's `display*` fields directly (see §6.3). Document this pattern in every public entry point's roxygen. |
| **B** — Bug | 2 | [R/calc-api.R:840](R/calc-api.R#L840) — the `pretty_names.neoipcr_tbl_sr_ref` six-argument `gettext` — rewritten as an explicit locale-aware mapping against the `procedure_category_labels` tibble. [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) — the two-argument `gettext("Procedure code", "Procedure category")` — rewritten as two separate `gettext` calls for the two column headers, *or* moved to YAML cascade if those headers are report-visible (decide in Phase 2 when the string-resources promotion lands). |

**What this looks like to a caller.**

```r
# Before (current):
#   Sys.setlocale("LC_MESSAGES", "de_DE.UTF-8")
#   get_procedure_category_pretty(codes)  # uses Sys.getlocale

# After:
opts <- dhis2_dataset_options(locale = "de_DE")
labels <- get_procedure_category_labels(locale = opts$locale)  # explicit arg wins
# or: labels <- get_procedure_category_labels()  # resolution chain kicks in
```

**What this looks like to a translator.** `R-neoipcr.pot` contains ~75 entries after migration (down from ~87, after removing the 10 F-class calls and the 2 B-class bug rewrites), all of them operator-facing messages. Procedure-category labels are translated in the YAML / CSV under Surveillance-Toolkit's metadata/ (out of scope for this note; §7 row 2 coordinates with A5b).

### §6.6. Migration plan for Phase 2

| Call site | Classification | Action in Phase 2 |
|---|---|---|
| `R/calc-api.R:840` | B → C (after fix) | Rewrite as an explicit tibble lookup against the new `procedure_category_labels`; remove the six-argument `gettext`. |
| `R/calc-procedure-categories.R:45` | B | Fix to two separate `gettext` calls *or* move to YAML cascade (decide alongside string-resources promotion, §5 row 7). |
| `R/calc-procedure-categories.R:215–224` (10 calls) | F → data-layer | Replace with reads from `procedure_category_labels` tibble. `get_procedure_category_pretty()` becomes `get_procedure_category_labels(locale = NULL)`; locale-resolution chain applied. Remove the top-of-function `Sys.getlocale("LC_MESSAGES")` call at line 14. |
| `R/dhis2-connect.R` (14 calls) | M | No action required. Stay as-is. Optionally document the behaviour of `gettext` falling back to English when the catalog lookup fails — useful for vignette writers. |
| `R/validation.R` (~55 calls) | M | No action. Stay on `gettext/gettextf`. |
| `R/validation-rules-completeness.R` (14 calls) | M | No action. |
| `R/validation-rules-enrollment.R` (2 calls) | M | No action. |

**Phase 2 sequencing.** The migration has exactly one non-trivial step: create `procedure_category_labels`, populate it, rewrite `get_procedure_category_pretty()` to read from it, delete the 11 `gettext` lines. Everything else is either "do nothing" (M-class) or a two-line bugfix (B-class). Estimate: half a day of work plus PO catalog regeneration.

**PO catalog regeneration.** After the F-class migration, regenerate `po/R-neoipcr.pot` via `tools::xgettext2pot()` (or `potools::create_catalog()`) and merge the changes into `po/R-de.po` / `po/R-en.po`. Compiled catalogs at `inst/po/{de,en}/LC_MESSAGES/R-neoipcr.mo` regenerate via `tools::msgfmt()`. Out of scope for this note; part of Phase 2.

### §6.7. Unified locale resolution chain

Applies to **both** the messages track (gettext domain resolution) and the data track (column selection in `sysdata.Rda` label tibbles and path selection in the YAML resource cascade):

```
explicit function argument → dhis2_dataset_options()$locale → Sys.getlocale("LC_MESSAGES") → "en" (package default)
```

**Precedence rule.** First non-`NULL`, non-empty value wins. Functions accept `locale = NULL` to trigger the chain; `locale = "de_DE"` bypasses it.

**Which functions consult the chain.** Every user-facing entry point that produces localized output:

- `pretty_names()` and its methods — column headers and factor labels
- `get_procedure_category_labels()` (new in Phase 2) — factor labels from package data
- `get_aware_categories()` (new in Phase 2) — factor labels from package data
- `get_event_type_map()` (new in Phase 2) — factor labels from package data
- The `as_gt.*` methods (new in Phase 2, §8) — table captions and CI merge patterns
- Every promoted YAML-cascade reader (`get_string_resources()` etc. from §5) — report-side captions

**Which functions do NOT consult the chain.** Internal plumbing that doesn't produce user-visible output (`read_metadata_*()` readers, validation-rule executors, the schema engine in `R/schema-*.R`). Messages from `gettext/gettextf` resolve via R's own `bindtextdomain()` machinery — that's a separate mechanism that respects `Sys.getenv("LANGUAGE")` and `Sys.getlocale("LC_MESSAGES")` at call time.

**`dhis2_dataset_options(locale = ...)` already exists.** [R/dhis2-options.R](R/dhis2-options.R) accepts the parameter; the only broken piece is that it doesn't flow to the R-side message layer (§6.2). Phase 2 fixes the flow without changing the signature.

### §6.7. Unified locale resolution chain

**Proposal**: `explicit_arg → dataset_options$locale → Sys.getlocale("LC_MESSAGES") → package default ("en")`.

Applies to both:
- Message catalogs (gettext `domain` resolution).
- Data-layer taxonomies (which `label_<locale>` column of the locale-keyed tibbles in §7 to read).

A4c will finalize and §6.5 will restate.

---

## §7. Package-data promotion list

**Anchor pattern** (do not redesign). [data-raw/sysdata.R](data-raw/sysdata.R) already ingests three pathogen files from Surveillance-Toolkit via `raw.githubusercontent.com/.../refs/heads/main/metadata/common/infectious-agents/` and packs them via `usethis::use_data(..., internal = TRUE, compress = "xz")`. Every new sysdata candidate below follows the same URL shape, the same `internal = TRUE` default, and the `get_pathogen_taxonomy()` roxygen model for its accessor function.

### §7.1. Candidate table

| # | Candidate | Current inline location | Shape today | Target (package data) | Int/Ext | Accessor | Blocking |
|---|-----------|-------------------------|-------------|-----------------------|---------|----------|----------|
| 1 | Procedure-category map (ICHI → category code) | [R/calc-procedure-categories.R:69–210](R/calc-procedure-categories.R#L69) — `case_when` inside `get_procedure_category()` | Inline `case_when` (9 categories + `to_be_categorised` + `not_surgery`) | Tibble `procedure_category_map` in `sysdata.Rda` with columns `ichi_code`, `category_code` | internal | `get_procedure_category()` (existing) | Needs a CSV source under [Surveillance-Toolkit/metadata/common/](../Surveillance-Toolkit/metadata/common/) (A5b). |
| 2 | Procedure-category pretty-name labels | [R/calc-procedure-categories.R:215–224](R/calc-procedure-categories.R#L215) — 10 `gettext()` calls (A4a F-1…F-10) | 10 factor-level labels routed through gettext | Tibble `procedure_category_labels` in `sysdata.Rda` with locale-keyed columns (`category_code`, `label_en`, `label_de`, …) — the `countrycode` pattern | internal | new: `get_procedure_category_labels(locale = NULL)` | **Depends on D-H.** The 10 factor labels are the primary motivation for D-H. |
| 3 | AWaRe categories (code → label) | [R/calc-denominators.R:216–224, 294–300](R/calc-denominators.R#L216) — inline factor construction over `{"a", "w", "r"}` | Factor with levels `c("a", "w", "r")`; labels hardcoded English (Access / Watch / Reserve) | Tibble `aware_categories` in `sysdata.Rda`: `code`, `label_en`, `label_de` | internal | new: `get_aware_categories(locale = NULL)` | **Depends on D-H.** Codes come from DHIS2 option groups (read at import time); labels need the locale-column pattern. |
| 4 | BW/GA breakpoints | [R/scales.R:1–75](R/scales.R) — hardcoded arithmetic in `ga7`, `bw50`, `bw125`, `bw250`, `bw500` | Hardcoded `floor(...)`-arithmetic in five functions | Named-list `scales_parameters` in `sysdata.Rda` (one sublist per binning) | internal | new: `get_scales_parameters()` returning the list; five binning fns consume it | **None.** Pure numeric constants. Promotable immediately (Phase 2 or earlier). |
| 5 | Event-type vocabulary | [R/dhis2-metadata-reference.R:41–98](R/dhis2-metadata-reference.R#L41) — hardcoded `recode_values` mapping DHIS2 program-stage name → event-type key (`adm`, `end`, `bsi`, `nec`, `hap`, `pro`, `ssi`) | 7-entry `case_match` in reader function + inline factor levels | Tibble `event_type_map` in `sysdata.Rda`: `programStage`, `name`, `event_type_key`, `label_en`, `label_de` | internal | new: `get_event_type_map(locale = NULL)` | **Depends on D-H** *and* on a Surveillance-Toolkit source file (A5b). Also blocks (and is blocked by) the event-type promotion in A3 from [Validation-Report/_setup.qmd:17](../Surveillance-Toolkit/reports/Validation-Report/_setup.qmd#L17). |
| 6 | Resistance markers | [R/pathogens.R](R/pathogens.R) + columns on `internal_pathogen_concepts` | Already package-data | *(no change)* — already ingested via `data-raw/sysdata.R` | internal | `get_pathogen_taxonomy()` (existing) | **Complete.** |
| 7 | ICHI code list / validity check | [R/ichi.R:1–69](R/ichi.R) — regex grammar assembled at package load | Compiled regex pattern | **Reject for promotion** — keep as procedural validator. | — | `is_valid_ichi_code()` (existing) | Full ICHI ontology bundling blocked by WHO licensing (see [tasks/ichi-classification-bundling.md](../../tasks/ichi-classification-bundling.md)). Current syntax-only check is intentional. |

### §7.2. Rejected from package-data promotion

- **DHIS2-sourced option sets** (wound classes, ASA scores, admission types, ATC5 categories, etc.). These are *instance-specific* — sourced live from the running DHIS2 server via `read_metadata_*()` functions in [R/dhis2-metadata-options.R](R/dhis2-metadata-options.R) and [R/dhis2-metadata-reference.R](R/dhis2-metadata-reference.R). Bundling them as package data would fossilize one deployment's state — wrong by construction.
- **Country codes / World Bank class map**. Sourced from DHIS2 org-unit hierarchy; deployment-specific.
- **Full ICHI code list** (row 7 above). Licensing.

### §7.3. Surveillance-Toolkit source-of-truth paths (A5b)

Surveillance-Toolkit's `metadata/common/` layout today:

```
metadata/common/
├── antibiotics/
│   ├── ListElements.csv / .de.csv / .es.csv          # locale-keyed labels
│   ├── NeoIPC-Antibiotics.csv / .de.csv / .es.csv
│   └── WHO-AWaRe-Classification-2021.csv
├── infectious-agents/
│   ├── NeoIPC-Infectious-Agents.yaml                 # currently ingested
│   ├── NeoIPC-Pathogen-Concepts.csv (+ locale variants)
│   ├── NeoIPC-Pathogen-Synonyms.csv (+ locale variants)
│   └── ListElements.csv (+ locale variants)
├── organisation_units/
│   └── organisationUnits.csv / .de.csv / .en.csv / .es.csv
├── optionSets.csv
├── options.csv / options.de.csv
└── README.md
```

**Locale-keying convention.** Surveillance-Toolkit splits locale labels into separate files — `X.csv` holds the English canonical, `X.de.csv` / `X.es.csv` hold translations. This differs from the `countrycode` pattern (locale columns inside a single tibble). The `data-raw/sysdata.R` ingestion can follow either pattern — **recommend** ingesting all locale variants and pivoting to a single tibble with `label_en` / `label_de` / `label_es` columns so downstream accessor code is uniform.

**Per-candidate source paths** (extends §7.1):

| # | Candidate | Source path(s) under `metadata/common/` | Status |
|---|-----------|-----------------------------------------|--------|
| 1 | Procedure-category map (ICHI → category) | *(file does not yet exist)* — needs new `metadata/common/procedure-categories/NeoIPC-Procedure-Categories.csv` with columns `ichi_code`, `category_code` | **New file required.** Create in Surveillance-Toolkit before Phase 2. Coordinates with [csv-to-yaml-migration.md](../../tasks/csv-to-yaml-migration.md) if that task restructures this domain. |
| 2 | Procedure-category labels | *(file does not yet exist)* — needs `metadata/common/procedure-categories/ListElements.csv` + `ListElements.de.csv` + `ListElements.es.csv` following the locale-file convention | **New files required** + depends on D-H. |
| 3 | AWaRe categories (code → label) | Taxonomy: `antibiotics/WHO-AWaRe-Classification-2021.csv` (exists). Labels: `antibiotics/ListElements.csv` (+ `.de.csv`, `.es.csv`) — verify the `a` / `w` / `r` codes have label entries there | **Verify first.** If labels exist, ingest; if not, extend `ListElements.csv` with three rows. Depends on D-H. |
| 4 | BW/GA breakpoints | *(no source file needed)* — these are numeric constants, not metadata | **None.** Numbers ship inline in `sysdata.Rda` per row 4 of §7.1. Optionally document them in the Surveillance-Toolkit protocol text, but no machine-readable file is warranted. |
| 5 | Event-type vocabulary | *(file does not yet exist)* — needs new `metadata/common/event-types/NeoIPC-Event-Types.csv` + `ListElements.csv` (+ `.de.csv`, `.es.csv`) with rows for `adm` / `end` / `bsi` / `nec` / `hap` / `pro` / `ssi` | **New files required** + depends on D-H. Also feeds §5.4 (event-type extraction from Validation-Report). |
| 6 | Resistance markers | `infectious-agents/NeoIPC-Pathogen-Concepts.csv` (existing — resistance-marker columns already present). No change. | **Complete.** |
| 7 | ICHI code list | Not promoted — WHO licensing. | — |

**Ingestion recipe per new file** (Phase 2 will execute):

```r
# In data-raw/sysdata.R, following the existing pathogen pattern:
procedure_category_map <- readr::read_csv(
  "https://raw.githubusercontent.com/NeoIPC/Surveillance-Toolkit/refs/heads/main/metadata/common/procedure-categories/NeoIPC-Procedure-Categories.csv",
  col_types = "cc")

procedure_category_labels_en <- readr::read_csv(".../procedure-categories/ListElements.csv", col_types = "cc")
procedure_category_labels_de <- readr::read_csv(".../procedure-categories/ListElements.de.csv", col_types = "cc")
procedure_category_labels_es <- readr::read_csv(".../procedure-categories/ListElements.es.csv", col_types = "cc")

procedure_category_labels <- procedure_category_labels_en |>
  dplyr::rename(label_en = label) |>
  dplyr::left_join(dplyr::rename(procedure_category_labels_de, label_de = label), by = "category_code") |>
  dplyr::left_join(dplyr::rename(procedure_category_labels_es, label_es = label), by = "category_code")

usethis::use_data(
  # existing:
  internal_pathogen_concepts, internal_pathogen_synonyms, internal_pathogen_list,
  # new:
  procedure_category_map, procedure_category_labels,
  scales_parameters, aware_categories, event_type_map,
  internal = TRUE, overwrite = TRUE, compress = "xz")
```

(Phase 2 fills in the URLs and the column specs once the Surveillance-Toolkit files exist.)

### §7.4. Coordination

- Rows 2, 3, 5 block on **[D-H](#10-open-decisions-for-the-pi)** — all need locale-keyed label columns per the A4 architecture decision.
- Rows 1, 2, 5 require **new files** under `Surveillance-Toolkit/metadata/common/` — Phase 2 of task 1.1 must file a companion task (or extend Surveillance-Toolkit directly) to create them.
- Row 3 requires a **one-file verification** under `antibiotics/ListElements.csv`.
- Row 4 has **no blockers** — promotable first as the ingestion-pattern pilot.
- **Infectious-agent files stay held for [csv-to-yaml-migration.md](../../tasks/csv-to-yaml-migration.md)** — no rows above touch them. `internal_pathogen_concepts` / `internal_pathogen_synonyms` / `internal_pathogen_list` in [data-raw/sysdata.R](data-raw/sysdata.R) stay exactly as they are until that task reshapes the upstream YAML.

---

## §8. GT-styling boundary

### §8.1. Decision

**Entry-point shape: S3 method `as_gt.<neoipcr_class>()`.** The package already uses S3 dispatch for `pretty_names.neoipcr_tbl_sr_ref` in [R/calc-api.R](R/calc-api.R); extending the same pattern for `as_gt` gives a uniform way for callers to convert any `neoipcr_tbl_*` value into a `gt` table. Method signature:

```r
as_gt <- function(x, ...) UseMethod("as_gt")
as_gt.neoipcr_tbl_sr_ref <- function(x, ci_display = c("wrap", "inline"), ...)
as_gt.neoipcr_tbl_udr     <- function(x, ci_display = c("wrap", "inline"), ...)
# …one per rate-table class
```

One S3 method per rate-table class (about 10 methods once the slug-rename lands). Each method does the `fmt_integer` / `sub_missing` / optional `cols_merge` dance internally; callers write one line per table.

**Rejected alternative.** A single named function `format_neoipcr_table_gt(x, ci_display, ...)` that dispatches manually on `inherits(x, ...)` — rejected because it re-implements S3 dispatch, forces a growing `if/else` chain as rate tables proliferate, and loses the "`methods("as_gt")` lists every supported input" discoverability.

### §8.2. CI display-mode parameter

Replaces the report-side globals `ci_pattern_inline` / `ci_pattern_wrap` / `wrap_ci_default` with a single `ci_display` argument on each `as_gt.*` method:

- `ci_display = "wrap"` (default) — lower/upper CI columns kept separate; no `cols_merge`. Matches the current Reference-Report appearance.
- `ci_display = "inline"` — lower/upper columns merged into a single `(lower, upper)` cell. Matches the current Partner-Report appearance for pooled CIs.

The report passes `ci_display` per table from its Quarto params. The package owns the formatting logic; the reports own the display policy.

### §8.3. DESCRIPTION delta

Currently `gt` is in neither `Imports` nor `Suggests` (confirmed against [DESCRIPTION](DESCRIPTION)). Add:

```
Suggests:
    gt (>= X.Y.Z)    # minimum version pinned from current Surveillance-Toolkit usage
```

(Phase 2 fills in the version floor by auditing `renv.lock` in `repos/Surveillance-Toolkit/`.)

**Not `Imports`.** Reports that want `gt` output call `neoipcr::as_gt()` directly; callers who want the raw tibble (JSON exporters, the .NET reporting service) never touch `gt`. Keeping it in `Suggests` preserves the no-GT install path.

**Graceful fallback.** `as_gt.*` methods check `rlang::check_installed("gt")` on entry (or the `requireNamespace` equivalent), aborting with a clear message if `gt` is not installed. Pattern used elsewhere in tidyverse packages (e.g., `broom.mixed`'s `glance.*` methods guard on `lme4`).

### §8.4. Per-report wiring sketch

**Before** (Reference-Report, one table — abbreviated):

```r
tbl_data |> ... |> compute_col_widths(...) -> widths
tbl_data |> gt() |>
  fmt_integer(...) |>
  sub_missing(...) |>
  cols_merge(columns = c(ci_lower, ci_upper), pattern = ci_pattern_wrap) |>
  ... |>
  cols_width(widths$stub, widths$n, ...) |>
  tab_style(...)
```

~40–60 LOC per table, repeated per table per report (~15 tables × 2 reports).

**After**:

```r
tbl_data |> neoipcr::as_gt(ci_display = "wrap")
```

Everything above moves into the S3 method. Report files shed ~500–800 LOC across Partner-Report and Reference-Report.

### §8.5. Dependency posture of rate-table functions

Rate-table functions (`get_*_table()`) continue to return tibbles with the right class attached; they do NOT return `gt` objects. This preserves:

- JSON export pipeline for the .NET reporting service.
- The testable-without-`gt` property of the package.
- Composability — users can call `as_gt()` on an already-cached table result.

§8 is the only section that proposes adding `gt` to any dependency list.

---

## §9. Naming proposal

Flat rename table. Renames are applied in Phase 3 of task 1.1 (not in this session); no `lifecycle::deprecate_warn()` aliases (pre-alpha).

### §9.1. Function renames

| Current name | Proposed name | Reason |
|---|---|---|
| `neoipc_poisson_ci()` | `neoipcr_poisson_ci()` | Existing prefix doesn't match the package name (`neoipcr`). Fix in lockstep with the other two CI renames. (D-D) |
| `neoipc_wilson_ci()` | `neoipcr_wilson_ci()` | Same — existing prefix doesn't match the package name. (D-D) |
| `bootstrap_quantile_ci()` | `neoipcr_bootstrap_quantile_ci()` | Unifies with the other two CI functions under the package-matching `neoipcr_` prefix. (D-D) |

**One rename proposal withdrawn.** D-C originally proposed `get_benchmark_data()` → `calculate_benchmark_data()` based on the task file's claim that the function "actually computes." Reading the source ([R/calc-api.R:323–](R/calc-api.R#L323)) shows it combines pre-computed datasets with one small CI fix-up — not a calculation primitive. See §10.3. The task file claim is corrected in the same commit as this note.

No other renames proposed for the 24 current exports. Long table-function names stay (D-E — use `@family` grouping instead of shortening).

### §9.2. Class renames

All class renames are owned by [task 1.2 (neoipcr-class-slug-rename.md)](../../tasks/neoipcr-class-slug-rename.md); §9 does not duplicate them. §3.2 lists three classes found by this audit that should be added to task 1.2's rename table:

- `neoipcr_bnch_ds` → `neoipcr_benchmark_ds`
- `neoipcr_tbl_rtr` / `_ref` → `neoipcr_resistance_test_rate_table` / `_ref`
- `neoipcr_tbl_sec_bsi` / `_ref` → `neoipcr_secondary_bsi_rate_table` / `_ref`

### §9.3. Naming rules for promoted helpers

Rules followed in §5 when assigning names to helpers moving from Surveillance-Toolkit reports into `neoipcr`:

1. **Snake case, lowercase.** Matches tidyverse convention and the existing package.
2. **No abbreviations of domain terms** in exported function names. `log_info` ok (standard); `bw` / `ga` ok (well-known medical abbreviations); avoid ad-hoc package-internal abbreviations.
3. **Verb-noun order for actions, noun-verb for state queries.** `format_integer()`, `classify_metrics()`, `compose_interpretation()` (actions); `has_outliers()`, `get_outliers_below()` (state queries). The existing `get_*_table()` family is grandfathered.
4. **`get_*` for retrieval from data or state; `calculate_*` for computation; `format_*` for pure presentation transforms.** This is the rule that forces D-C (`get_benchmark_data` → `calculate_benchmark_data`).
5. **`neoipcr_` prefix for domain-specific statistics functions only** (post-D-D: `neoipcr_poisson_ci`, `neoipcr_wilson_ci`, `neoipcr_bootstrap_quantile_ci`). Formatting helpers (`format_integer`), CLI helpers (`parse_args`), and logging helpers (`log_info`) do NOT carry the prefix — they're general-purpose and callers already say `neoipcr::format_integer()`.
6. **Class predicates follow `is_<class>()` / `check_<class>()` convention** (already established in [R/types-check.R](R/types-check.R)). When Phase 2 creates new classes, they get matching predicates.

### §9.4. Names explicitly NOT proposed for change

- All S3 methods on existing classes. Renames happen in task 1.2 when the classes themselves rename.
- Long table-function names (`get_dev_ass_incidence_density_rate_table` etc.) under their **current** spelling are flagged for change in §9.5 below — but as part of the cross-surface alignment, not as standalone shortening. D-E (don't shorten in isolation) still stands.

*(Earlier draft listed the `neoipc_*` → `neoipcr_*` CI rename as "not proposed — cosmetic." That dismissal was wrong; the rename is now in §9.1 / D-D.)*

### §9.5. Cross-surface naming alignment for result tables

PI flagged that result-table names diverge across six surfaces — function name, S3 class slug, dataset slot, conceptual rate name, Quarto YAML param, PowerShell wrapper switch — and the divergence makes the API hard to memorize. This section catalogues the divergence and proposes derivation rules from a single per-table master name.

#### §9.5.1. Surfaces in scope

| # | Surface | Convention today | Owner |
|---|---------|------------------|-------|
| F | Function name in neoipcr | `get_<...>_table()` (snake_case) | neoipcr |
| C | S3 class slug | `neoipcr_tbl_<airport-code>` (snake_case + abbreviation) — pending [task 1.2](../../tasks/neoipcr-class-slug-rename.md) | neoipcr (slug rename owned by 1.2) |
| S | `neoipcr_rep_ds` / `neoipcr_ref_ds` slot | `<...>_table` (snake_case) | neoipcr |
| R | Conceptual rate name (the metric the table is about) | varies | shared concept |
| Q | Quarto YAML param in report `*.qmd` | `include<...>Table` (camelCase) | Surveillance-Toolkit reports |
| P | `EnableElements` / `DisableElements` switch token in `New-*Report.ps1` | `<...>Rates` (PascalCase) | Surveillance-Toolkit scripts |
| Z | `.qmd` filename under `reports/<Report>/{tables,figures}/` | `_tbl-<...>.qmd` / `_fig-<...>.qmd` (kebab-case) | Surveillance-Toolkit reports |
| H | Heading key in `sR$headings$<key>` (English source in `_sR.yaml` / `common.yaml`) | snake_case | Surveillance-Toolkit reports |
| D | Display heading string (per-locale, in `_sR.yaml` / `common.yaml` / glossary.yaml) | English source per AMA Manual; translations via po4a/Weblate | Surveillance-Toolkit reports + Weblate translators |

The casing-per-layer convention is documented in [Surveillance-Toolkit CLAUDE.md](../Surveillance-Toolkit/CLAUDE.md): "PS `PascalCase` → QMD `camelCase` → R `snake_case`, mapped once at each boundary." §9.5 extends this to: filesystem paths use kebab-case (Z), heading keys use snake_case (H), display strings follow the AMA Manual (D, with translations via po4a/Weblate).

One surface remains out of §9.5's scope:
- **Per-factor rate column names within a table** (e.g. `cvc_rate`, `pvc_rate` inside `usage_density_rate_table`). These name the *factors* the table groups by, not the *table*. They follow their own per-domain conventions (device codes, infection-type codes, etc.) and are out of scope for the per-table master-name decision.

Nested snippet filenames (`_tbl-intro-<...>.Rmd`, `_methods-<...>.Rmd` per [reports/<Report>/content/](../Surveillance-Toolkit/reports/Partner-Report/content/)) are derived mechanically from the .qmd filename's kebab-case stem and follow surface Z's convention; they are not separately enumerated.

#### §9.5.2. Current state — full inventory

Confirmed end-to-end via grep across `repos/neoipcr/R/*.R`, `repos/Surveillance-Toolkit/reports/Partner-Report/Partner-Report.qmd` + `tables/_tbl-*.qmd` + `figures/_fig-*.qmd`, `Reference-Report/Reference-Report.qmd`, `Partner-Report/content/_sR.yaml`, `reports/common.yaml`, and `repos/Surveillance-Toolkit/scripts/New-PartnerReports.ps1`.

**Code-identifier surfaces (F / C / S / Q / P / Z / H):**

| # | F (function) | S (slot) | Q (Quarto param) | P (PS switch) | Z (.qmd file) | H (heading key) |
|---|---|---|---|---|---|---|
| 1 | `get_usage_density_rate_table` | `usage_density_rate_table` | `includeRiskDensityRateTable` | `RiskDensityRates` | `_tbl-risk-density-rates.qmd` | `presence_of_risk_and_protective_factors` |
| 2 | `get_antibiotic_utilisation_table` | `antibiotic_utilisation_table` | `includeAntibioticUtilisationTable` | `AntibioticUtilisationRates` | `_tbl-antibiotic-utilisation.qmd` | `antibiotic_utilisation` |
| 3 | `get_surgery_rate_table` | `surgery_rate_table` | `includeSurgicalProcedureRateTable` | `SurgicalProcedureRates` | `_tbl-surgical-procedure-rates.qmd` | `surgical_procedures_by_category` |
| 4 | `get_incidence_density_rate_table` | `incidence_density_rate_table` | `includeIncidenceDensityTable` | `IncidenceDensityRates` | `_tbl-incidence-density-rates.qmd` | `severe_infections_and_nec` |
| 5 | `get_dev_ass_incidence_density_rate_table` | `dev_ass_incidence_density_rate_table` | `includeDeviceAssociatedIncidenceDensityTable` | `DeviceAssociatedRates` | `_tbl-device-associated-incidence-density-rates.qmd` | `device_associated_infections` |
| 6 | `get_infectious_agent_detection_rate_per_inf_type_table` | `infectious_agent_detection_rate_per_inf_type_table` | `includeAgentPerInfectionRateTable` | `AgentPerInfectionRates` | `_tbl-agent-per-infection-rate.qmd` | `infectious_agents_in_nosocomial_infections` |
| 7 | `get_infectious_agent_detection_rate_per_agent_table` | `infectious_agent_detection_rate_per_agent_table` | `includeInfectiousAgentDetectionRateTable` | `InfectiousAgentDetectionRates` | `_tbl-infectious-agent-detection-rate.qmd` | `detection_of_infectious_agents` |
| 8 | `get_abr_infection_rate_table` | `abr_infection_rate_table` | `includeResistantPathogenInfectionRateTable` | `AntibioticResistanceRates` | `_tbl-abr-infection-rate.qmd` | `resistant_pathogens` |
| 9 | `get_organism_resistance_rate_table` | `organism_resistance_rate_table` | `includeOrganismResistanceRateTable` | `OrganismResistanceRates` | `_tbl-organism-resistance-rate.qmd` | `organism_resistance` |
| 10 | `get_resistance_test_rate_table` | `resistance_test_rate_table` | `includeAntibioticResistanceTestRateTable` | `ResistanceTestRates` | `_tbl-resistance-test-rate.qmd` | `antimicrobial_susceptibility_testing` |
| 11 | `get_secondary_bsi_rate_table` | `secondary_bsi_rate_table` | `includeSecondaryBsiRateTable` | `SecondaryBloodstreamInfectionRates` | `_tbl-secondary-bsi-rates.qmd` | `secondary_bsi` |
| F1 | (figure slot) | `birth_weight_figure` | `includeBirthWeightFigure` | `BirthWeightDistribution` | `_fig-bw.qmd` | `birth_weight_distribution` |
| F2 | (figure slot) | `gestational_age_figure` | `includeGestationalAgeFigure` | `GestationalAgeDistribution` | `_fig-ga.qmd` | `gestational_age_distribution` |

C (class slug) column omitted from this table for width — see §3 / §4.5; all `neoipcr_tbl_*` slugs follow the airport-code abbreviation pattern pending [task 1.2](../../tasks/neoipcr-class-slug-rename.md). The `_ref` variants append `_ref` on F/C/S only (Q/P/Z/H/D unaffected).

**Display heading strings (D — English source per AMA Manual; translated via po4a/Weblate):**

| # | Display heading (English) | Cascade level |
|---|---|---|
| 1 | "Presence of Risk and Protective Factors in Your Department" (Partner-Report) / "Presence of Risk and Protective Factors" (Reference-Report) | `Partner-Report/content/_sR.yaml` and `Reference-Report/content/_sR.yaml` (per-report variants) |
| 2 | "Antibiotic Utilisation" (presumed — verify in `common.yaml`) | likely `common.yaml` |
| 3 | "Surgical Procedures by Category" | likely `common.yaml` |
| 4 | "Severe Infections and NEC" | likely `common.yaml` |
| 5 | "Device-Associated Infections" | likely `common.yaml` |
| 6 | "Infectious Agents in Nosocomial Infections" | likely `common.yaml` |
| 7 | "Detection of Infectious Agents" | confirmed in `common.yaml:40` |
| 8 | "Resistant Pathogens" | confirmed in `common.yaml:50` |
| 9 | "Organism Resistance" | likely `common.yaml` |
| 10 | "Antimicrobial Susceptibility Testing" | likely `common.yaml` |
| 11 | "Secondary BSI" (or "Secondary Bloodstream Infection") | likely `common.yaml` |
| F1 | "Birth Weight Distribution" | confirmed in `Partner-Report/content/_sR.yaml:80` |
| F2 | "Gestational Age Distribution" | confirmed in `Partner-Report/content/_sR.yaml:81` |

Phase 1 has not exhaustively confirmed every D-row — this is a representative sample. Phase 2 verification reads each `sR$headings$<key>` lookup and confirms the resolved string under English locale. The shape of the alignment proposal in §9.5.4 does not depend on the specific D string; it depends on the relationship between H (key) and the table identifier.

#### §9.5.2.1. Notable cross-surface findings

- **Row 1**: 5-way divergence — `usage` (F/S) vs `Risk` (Q/P/Z) vs `presence_of_risk_and_protective_factors` (H — section concept, not the table itself).
- **Row 3**: Z and H surfaces use **different words** (`surgical-procedure-rates` vs `surgical_procedures_by_category`). Section heading is broader than the table.
- **Row 4**: H uses `severe_infections_and_nec` — semantically a *parent grouping*, not specifically "incidence density rate." Indicates a section that *contains* the table rather than being named after it.
- **Row 6**: Z says `agent-per-infection-rate`, H says `infectious_agents_in_nosocomial_infections` — H is a higher-level section.
- **Row 8**: All five vocabulary tracks diverge: `abr` (F/S/C), `Resistant Pathogen` (Q), `Antibiotic Resistance` (P), `abr` (Z), `resistant_pathogens` (H/D).
- **Row 10**: H uses `antimicrobial_susceptibility_testing` — different concept entirely from F/S/C/P/Q/Z which all say `resistance_test`. "Antimicrobial susceptibility testing" (AST) is the AMA-canonical term; "resistance test" is internal jargon. **Strong indicator that the AMA term should be the master name** (i.e., `antimicrobial_susceptibility_test_rate`).
- **Rows F1/F2**: `_fig-bw.qmd` and `_fig-ga.qmd` use the **most cryptic abbreviations of any surface** (two letters only). Inconsistent even with the report's own naming elsewhere.
- **Row 3b** (`get_ref_surgery_rate_table`): Asymmetric — it's the only table with two distinct functions for own-vs-ref. Phase 2 design fix (out of §9.5 scope).

#### §9.5.2.2. Section-vs-table relationship

Some H entries are 1:1 with their table (rows 2, 9, 11, F1, F2 — heading key matches the noun). Others are *parent sections* that contain the table (rows 1, 4, 5, 6, 7, 8, 10). For the second case, two subsequent decisions:

- **(a) Align the section heading to the table** (rename H + D to match the master). Strongest alignment; may require multiple tables that share a section to migrate together.
- **(b) Keep the section name; rename only the table-internal identifiers** (Z stays kebab of master; H stays as section concept). Preserves editorial structure; accepts that H is a section concept, not a table concept.
- **(c) Introduce a new H per table** under the existing section (sections become parents containing keyed sub-headings). Adds structure; may grow the YAML cascade.

Per §9.5.4, defaults vary per row: where the table is the only thing in its section, prefer (a); where the section legitimately groups several tables, prefer (c).

#### §9.5.3. Proposed derivation rules from a master name

**Master name shape**: `<concept>_rate` (snake_case, no package-internal abbreviations). The master is the AMA-canonical term for the metric — see §9.5.4. For figures: `<concept>_figure`.

| Surface | Derivation rule | Example for master `usage_density_rate` |
|---------|-----------------|------------------------------------------|
| F | `get_<master>_table()` | `get_usage_density_rate_table()` |
| C | `neoipcr_<master>_table` (post-task-1.2) | `neoipcr_usage_density_rate_table` |
| S | `<master>_table` | `usage_density_rate_table` |
| R | `<master>` (master itself, used in roxygen prose and column-comments) | `usage_density_rate` |
| Q | `include` + UpperCamelCase(`<master>_table`) | `includeUsageDensityRateTable` |
| P | UpperCamelCase(`<master>s`) — pluralized | `UsageDensityRates` |
| Z | `_tbl-` + kebab-case(`<master>s`) + `.qmd` (or `_fig-` for figures) | `_tbl-usage-density-rates.qmd` |
| H | snake_case(`<master>s`) — when the heading is the table itself | `usage_density_rates` |
| D | Title-case AMA-canonical phrasing (English source); per-locale via po4a/Weblate | "Usage Density Rates" |

For `_ref` variants: append `_ref` on F/C/S; Q/P/Z/H/D unaffected (the param toggles whether the table appears, regardless of own vs reference).

For nested snippet filenames under `content/` (e.g. `_tbl-intro-<...>.Rmd`, `_methods-<...>.Rmd`): kebab-case derived from the master, prefixed by snippet role.

**Universally-medical abbreviations stay** (BSI, NEC, SSI, CVC, PVC, ICU, AST etc.) — these are clinician vocabulary, not package-internal slugs. The boundary case is "AbR" (rows 8 / earlier draft): not a clinician-standard abbreviation in widespread use; the AMA-canonical term is "antibiotic resistance" or "antimicrobial resistance" (AMR). Treat `AbR` as package-internal; spell it out in the master.

Package-internal abbreviations to eliminate: `udr`, `idr`, `daidr`, `iadrpit`, `iadrpa`, `org_rr`, `rtr`, `sec`, `tbl`, `bw` (figure file), `ga` (figure file), `dev_ass` (slot prefix).

**Section-vs-table headings (H + D special case).** When a heading is a *section* that contains multiple tables (per §9.5.2.2): the H and D values are the section's name, not the table's. The mechanical derivation above gives the *table*'s identifiers; the *section*'s heading + display can stay as a separate identifier. Per row, §9.5.4 picks (a) align section to table, (b) keep section, or (c) add a per-table sub-heading.

#### §9.5.4. Master-name proposals (D-I — needs PI confirmation)

Master-name choice is a *domain* call grounded in the AMA-canonical term for each metric, not a code-style call. The PI is the epidemiologist; recommendations below are starting points for §10.9 (D-I) review. Each row also addresses the H/D section-vs-table question (per §9.5.2.2 options (a)/(b)/(c)).

| # | Working concept | Proposed master | Section heading (H/D) treatment | Rationale |
|---|-----------------|-----------------|----------------------------------|-----------|
| 1 | usage / risk density rate | `usage_density_rate` | (b) keep section `presence_of_risk_and_protective_factors` as a parent; new H per table = `usage_density_rates` (option c if multiple density-rate tables grouped here later) | Numerator is days of use (device-days, antibiotic-days); "usage" describes the math. Section "Presence of risk and protective factors" is a broader epi grouping — keep it. **PI: is "usage density" the right epi term, or should we follow the report's "risk density" naming throughout?** |
| 2 | antibiotic utilisation rate | `antibiotic_utilisation_rate` | (a) align — H/D = "Antibiotic Utilisation Rate" | Add `_rate` suffix everywhere for consistency. **Confirm British `utilisation` vs American `utilization`** — relevant for international audience; current code is British. |
| 3 | surgery rate | `surgery_rate` | (b) keep section `surgical_procedures_by_category` as the broader section title; new sub-H = `surgery_rates` if needed | "Surgery" is shorter and clinically standard. The H "Surgical Procedures by Category" is descriptive of how the table is structured, not the metric — keep as section copy. |
| 3b | (asymmetric ref function) | resolve in Phase 2 | — | The two-function asymmetry (`get_surgery_rate_table` + `get_ref_surgery_rate_table` instead of one S3 dispatch) is a Phase 2 design fix, not a §9.5 naming fix. |
| 4 | incidence density rate | `incidence_density_rate` | (b) keep section `severe_infections_and_nec`; new sub-H per table | Already aligned on F/S/Q/P. Section H is a broader epi grouping ("severe infections and NEC") that legitimately groups tables — keep. |
| 5 | device-associated incidence density rate | `device_associated_incidence_density_rate` | (a) align — H/D = "Device-Associated Incidence Density Rates" | Drop `dev_ass` slug; spell out. Long but unambiguous; rely on `@family` grouping (D-E). |
| 6 | agent detection rate per infection type | `agent_detection_rate_per_infection_type` | (b) keep section `infectious_agents_in_nosocomial_infections`; sub-H = `agent_detection_per_infection_type` | "Infectious agent" simplified to "agent" — H/D-side keeps "Infectious Agents" as section copy because it reads more naturally to clinicians; identifier uses "agent" for brevity. **Verify epi-correctness**. |
| 7 | agent detection rate per agent | `agent_detection_rate_per_agent` | (a) align — sub-H = `agent_detection_per_agent` under same parent as row 6 | Distinguishes from row 6 by `_per_agent`. |
| 8 | antibiotic-resistance infection rate | `antibiotic_resistance_infection_rate` (alt: `amr_infection_rate` if AMR is the preferred professional term) | (a) align — H/D = "Antibiotic Resistance Infection Rates" | Spell out `abr` — package-internal abbreviation. **PI: AbR vs AMR vs `antibiotic_resistance` vs `antimicrobial_resistance` — which term does the surveillance literature use?** |
| 9 | organism resistance rate | `organism_resistance_rate` | (a) align — H/D = "Organism Resistance Rates" | Already aligned across F/S/Q/P. |
| 10 | antimicrobial susceptibility test rate | `antimicrobial_susceptibility_test_rate` (per H surface — the AMA-canonical term) | (a) align — H/D = "Antimicrobial Susceptibility Testing" | **The H surface is more correct than F/S/C/P**: "antimicrobial susceptibility testing" (AST) is the AMA term; "resistance test" is internal jargon. Recommend adopting the H wording as the master and renaming F/S/C/P to match. **Big Phase 3 change but it aligns with the literature**. |
| 11 | secondary BSI rate | `secondary_bsi_rate` | (a) align — H/D = "Secondary BSI Rates" (or "Secondary Bloodstream Infection Rates" if AMA prefers full form in headings) | Keep `bsi` (clinician-standard). PI may prefer the full form in the display heading per AMA — that's a D-only choice; identifiers stay `bsi`. |
| F1 | birth weight figure | `birth_weight_figure` | (a) align — H/D = "Birth Weight Distribution" (already correct) | Z renames `_fig-bw.qmd` → `_fig-birth-weight.qmd`. P migrates `BirthWeightDistribution` → `BirthWeightFigure` (or keep "Distribution" if PI prefers — but then F1's "figure" suffix in F/S becomes the outlier and we should pick one). |
| F2 | gestational age figure | `gestational_age_figure` | (a) align — H/D = "Gestational Age Distribution" (already correct) | Z renames `_fig-ga.qmd` → `_fig-gestational-age.qmd`. Same Distribution-vs-Figure note as F1. |

#### §9.5.5. Cross-domain alignment principle

**Earlier draft of this section argued for separating identifiers from display strings.** That was wrong. The PI's call: alignment should extend across all surfaces — including display headings — so the coding domain and the epidemiology domain use the same vocabulary. A reader of the PDF should be able to find the code that generates a section heading without consulting a translation map.

**Principle**: the master name per row in §9.5.4 is the **AMA-canonical epidemiological term** for the metric, and **all nine surfaces (F / C / S / R / Q / P / Z / H / D)** derive from it via the §9.5.3 rules:

- Code identifiers (F / C / S / R / Q / P / Z / H) follow the casing-per-layer convention with the master name as the stem.
- Display strings (D, English source) phrase the master in title-case AMA-canonical English.
- Per-locale translations (D, non-English) flow through po4a/Weblate as usual; aligning the English source means translators see one canonical concept, not three competing ones.

**One legitimate case for divergence remains** (per §9.5.2.2 / §9.5.4): when a heading **section** legitimately groups multiple tables, the section heading (H/D) reflects the *grouping concept*, not any one table. Per row, §9.5.4 picks (a) align section heading to the table identifier, (b) keep section heading and add per-table sub-headings, or (c) some hybrid. This is editorial: only the PI knows which sections are intentional groupings vs. cosmetic.

**What this changes vs. the earlier draft**: PDF headings become the master name's title-case form, not whatever editorial copy the report previously used. "Risk Density Rates" (current PDF heading for row 1) becomes "Usage Density Rates" if the master is `usage_density_rate` — *unless* the PI keeps the broader section heading "Presence of Risk and Protective Factors" as a parent (§9.5.4 row 1 is option b — keep the section, add a sub-heading per table).

#### §9.5.6. Coordination with task 1.2

The class-slug column (C) above pre-empts [task 1.2 (neoipcr-class-slug-rename.md)](../../tasks/neoipcr-class-slug-rename.md). After D-I lands, task 1.2's rename table for result-table classes should be **replaced** by the §9.5 master names rather than rederived independently. Concretely, task 1.2's result-table candidates (`neoipcr_tbl_udr` → `neoipcr_usage_density_rate_table`, etc. — see [task 1.2 §3](../../tasks/neoipcr-class-slug-rename.md)) should be updated to follow §9.5 row #1's master `usage_density_rate` (under whatever PI ratifies in D-I), and the same for every other row. This avoids two independent rename schemes drifting apart.

The dataset-slot classes (`neoipcr_pat`, `neoipcr_enr`, `neoipcr_evt`, etc. in task 1.2 §1) are not affected by §9.5 — they're not result tables.

#### §9.5.7. Cross-repo execution

Phase 2 of task 1.1 promotes helpers but does not rename existing exports. **Phase 3** of task 1.1 applies all §9.5 renames, in lockstep across nine surfaces:

1. **neoipcr R/** (F / C / S / R) — function names, S3 classes, slot names, internal rate-name references in roxygen and column-comment text.
2. **Surveillance-Toolkit reports — Quarto YAML param defaults (Q)** — every report's `_quarto*.yml` and `_main.qmd` `params:` block; every `params$includeXxx` reference in `_setup.qmd` and `_content.qmd` files.
3. **Surveillance-Toolkit reports — `.qmd` filenames (Z)** — `git mv` each `_tbl-*.qmd` and `_fig-*.qmd` under `reports/<Report>/{tables,figures}/`. Update the `{{< include ... >}}` calls in `_content.qmd` (and any other includer). Rename nested snippet files (`_tbl-intro-*.Rmd`, `_methods-*.Rmd`) and their `include_localised(...)` callers.
4. **Surveillance-Toolkit reports — heading keys (H)** — rename keys in `reports/common.yaml` and per-report `content/_sR.yaml`. Update `sR$headings$<key>` references in every `.qmd` file.
5. **Surveillance-Toolkit reports — display strings (D)** — change the English source values in `common.yaml` / `content/_sR.yaml` per AMA Manual. **Run [`scripts/Invoke-Localization.ps1 -Update`](../Surveillance-Toolkit/scripts/Invoke-Localization.ps1)** to regenerate `.pot` files; the existing translations get marked fuzzy in `.po` files; translators (or the PI) re-confirm via Weblate. Per Surveillance-Toolkit's [po4a guardrails](../Surveillance-Toolkit/CLAUDE.md), do not manually edit generated `common.<lang>.yaml` / `content.<lang>/_sR.yaml` files.
6. **Surveillance-Toolkit scripts** (P) — `EnableElements`/`DisableElements` mapping tables in every `New-*.ps1` wrapper (`New-PartnerReports.ps1`, `New-ReferenceReport.ps1`, etc.). Per Surveillance-Toolkit's PowerShell-alignment guardrail: the mapping tables must stay in sync across all wrapper scripts.
7. **.NET reporting service** ([repos/NeoIPC-Reporting/](../NeoIPC-Reporting/)) — verify whether it consumes the PS switch tokens or the Quarto params or both; update accordingly.
8. **Documentation** — README.md, vignettes (Phase 5), CLAUDE.md "Key R Files" table, NEWS.md entry per-rename.

**Cross-repo change order** per workspace [CLAUDE.md](../../CLAUDE.md): protocol → DHIS2 config → neoipcr → reports → web. Phase 3's §9.5 renames all sit in the neoipcr → reports portion. The lockstep nature means the workspace-level commit references all touched repos: e.g. "Apply §9.5 cross-surface rename for usage-density-rate across neoipcr R/, Surveillance-Toolkit reports + scripts, NeoIPC-Reporting." Workflow per row of §9.5.4:

1. Update neoipcr R/ + tests → submodule commit.
2. Update Surveillance-Toolkit reports + scripts → submodule commit.
3. Run `scripts/Invoke-Localization.ps1 -Update` → regenerate `.pot` and merge into `.po`.
4. PI / translators re-confirm fuzzy strings in Weblate (or accept the English source as a no-op for English).
5. Update workspace submodule pointers → workspace-level commit referencing the row.

For 13 rows in §9.5.4, that's 13 lockstep change cycles. Phase 3 may batch related rows (e.g. 1+4+5 device-and-incidence cluster) into one PR per cluster. Estimate: 5–10 PRs spread over 1–2 weeks, plus the Weblate translation cycle.

---

## §10. Open decisions for the PI

Each subsection states the question, the recommendation with rationale, and a "PI resolution" line for you to fill in. Resolution can be: `accept`, `accept with amendment: …`, `reject — <alternative>`.

### §10.1. D-A. Audience tier per exported symbol

**Question.** For each of the 27 exported symbols in §3, which audience tier applies? External-stable (documented for data scientists / researchers / clinicians as part of the stable public API), internal-stable (stable for the NeoIPC internal pipeline but not primarily targeted at external users), or experimental (API may change, warn external users)?

**Recommendation.** Accept the tier column proposed in §3 as the default assignment. Key assignments:

- **external-stable (7):** `import_dhis2`, `dhis2_connection_options`, `dhis2_dataset_options`, `neoipc_poisson_ci`, `neoipc_wilson_ci`, `get_pathogen_taxonomy`, `print.neoipcr_dhis2_conopt`.
- **experimental (3):** `bootstrap_quantile_ci` (not yet integrated into the rate-table pipeline), `is_valid_ichi_code` (syntax-only validator; the full-code bundling task is in flight), `pretty_names` + its two methods (the S3 generic is unstable; the B-class bug at [calc-api.R:840](R/calc-api.R#L840) confirms it).
- **internal-stable (17):** all 11 rate-table functions, both `calculate_*_data` pipeline entries, `get_benchmark_data` (pre-rename), plus the S3 methods.

**Rationale.** External-stable tier covers functions that appear in all five reports AND are documented in the auth chain / data-protection section of CLAUDE.md (the auth and dataset-options trio), plus standalone statistical utilities (`neoipc_*_ci`), plus the widely-used taxonomy accessor. Everything internal-stable is load-bearing for the NeoIPC pipeline but not primarily targeted at external users — Phase 5 vignettes can expose them progressively. Everything experimental has a specific reason flagged in §3.

Task 1.4 (lifecycle badges) consumes this column verbatim.

**PI resolution:** _pending_

### §10.2. D-B. GT-styling entry point shape

**Question.** For the new GT-styling layer (§8), which shape: (i) S3 method `as_gt.<neoipcr_class>()`, or (ii) named function `format_neoipcr_table_gt(x, ...)` with manual `inherits()` dispatch?

**Recommendation.** (i) S3 method. Three reasons: the package already uses S3 dispatch for `pretty_names.neoipcr_tbl_sr_ref`; `methods("as_gt")` discovers every supported class automatically; adding a new rate-table class (when the resistance-rates expansion task lands) becomes "write an `as_gt.new_class()` method" rather than "edit the big if/else in `format_neoipcr_table_gt()`."

**Rationale.** See §8.1 for the rejected alternative. The S3 path also allows downstream consumers to dispatch `as_gt()` themselves for custom classes without neoipcr having to know about them.

**PI resolution:** _pending_

### §10.3. D-C. `get_benchmark_data()` rename

**Question.** Rename `get_benchmark_data()` to `calculate_benchmark_data()` as the task file asserts?

**Recommendation.** **Reject the proposed rename. Keep `get_benchmark_data()` as-is.**

**Rationale (corrected after reading the source).** The task file claims `get_benchmark_data()` "actually *computes*" — that claim is wrong. The function body in [R/calc-api.R:323–](R/calc-api.R#L323) takes pre-computed `neoipcr_rep_ds` / `neoipcr_ref_ds` inputs, prefixes their column names with the dataset name, and stitches them side-by-side via `bind_cols` / `full_join`. The roxygen on line 311 literally describes it as "*Creates* a NeoIPC benchmark data set *from* department report datasets and a reference data set." The only computation in the body is `fix_zero_event_ci()` (a small CI patch where merged-in zero-event departments produced garbage CIs); that's a fix-up, not a calculation primitive.

So the symmetry argument with `calculate_department_data()` / `calculate_reference_data()` doesn't hold — those two compute rates and CIs from raw `neoipcr_ds`; this one combines already-computed datasets for side-by-side display.

**Alternative renames considered.** If we want a name that more accurately describes "combine for side-by-side display," `combine_benchmark_data()` or `merge_benchmark_data()` would fit. None of these is clearly better than the current `get_benchmark_data()` — `get_*` is a familiar fall-back when neither "compute" nor "retrieve from storage" cleanly fits, and a cosmetic rename is not worth touching every Surveillance-Toolkit caller.

**Companion fix.** The task file at line 31 ([tasks/neoipcr-public-interface-refinement.md](../../tasks/neoipcr-public-interface-refinement.md)) needs its "actually *computes*" claim corrected — already done in the same commit as this note.

**PI resolution:** _pending_

### §10.4. D-D. CI-function prefix

**Question.** Three CI functions today: `neoipc_poisson_ci()`, `neoipc_wilson_ci()`, `bootstrap_quantile_ci()`. Two carry a `neoipc_` prefix that doesn't match the package name; one carries no prefix at all. Unify how?

**Recommendation.** **Rename all three to use the package-matching `neoipcr_` prefix:**

- `neoipc_poisson_ci()` → `neoipcr_poisson_ci()`
- `neoipc_wilson_ci()` → `neoipcr_wilson_ci()`
- `bootstrap_quantile_ci()` → `neoipcr_bootstrap_quantile_ci()`

**Rationale.** The existing `neoipc_` prefix (without the `r`) is a quiet bug, not a convention — the package is `neoipcr`. R-package convention is that an explicit function prefix, when used at all, matches the package name (`stringr::str_*`, `glue::glue_*`, etc.). Tab-completion discoverability is preserved (typing `neoipcr_` surfaces all three); the namespace-collision-guard argument also survives — `neoipcr_poisson_ci` is just as collision-resistant as `neoipc_poisson_ci` and is additionally findable via `?neoipcr` package-help cross-references.

We're already touching one of the three for the prefix-unification rename, and pre-alpha means no soft-deprecation cost — three renames are nearly as cheap as one. CRAN reviewers tend to notice prefix/package mismatches; resolving it now is cheaper than after first release.

**Considered and rejected:** dropping the prefix entirely (`poisson_ci()`, `wilson_ci()`, `bootstrap_quantile_ci()`). Callers would write `neoipcr::poisson_ci()` which is unambiguous, but loses the collision-resistance and bare-name discoverability the prefix provides. The recommended option is preferable.

**PI resolution:** _pending_

### §10.5. D-E. Long table-function-name shortening

**Question.** Shorten `get_dev_ass_incidence_density_rate_table()` and similar long names?

**Recommendation.** Keep as-is. Discoverability via `@family` grouping in roxygen (Phase 4 of task 1.1) is the mainstream way to organize a family of long-named functions. Shortening to e.g. `get_daid_table()` trades clarity for five fewer keystrokes; users who type a lot are using IDE completion anyway.

**Rationale.** Any rename here would need a matching rename in the return-class hierarchy (task 1.2), compounding churn.

**PI resolution:** _pending_

### §10.6. D-F. Accessor-vs-raw-data for each package-data candidate

**Question.** Per package-data candidate (§7), do we ship an accessor function (the `get_pathogen_taxonomy()` model) or expose the data frame directly via `data()`?

**Recommendation.** Accessor per taxonomy, `internal = TRUE` on every `usethis::use_data()` call. The accessor owns the locale-resolution chain (so callers can pass `locale = NULL` and have the right column selected automatically); direct `data()` exposure makes callers reimplement that logic every time.

**Rationale.** `get_pathogen_taxonomy()` already demonstrates the pattern; extending it is uniform. The one exception is the pathogen taxonomy itself, where `get_pathogen_taxonomy()` is already exported — keep it exported (D-A has it as external-stable).

**PI resolution:** _pending_

### §10.7. D-G. Message-catalog scope

**Question.** One `R-neoipcr.pot` / `R-neoipcr.po` catalog for all M-class messages, or split catalogs by subject (e.g., auth vs. validation)?

**Recommendation.** Single catalog. Matches `potools` convention (§6.4 row 5), matches R's own convention, matches the current state. Splitting catalogs creates a coordination burden for translators with no operational benefit.

**Rationale.** After the F-class migration (§6.5), the catalog shrinks to ~75 entries — well within a single-file size. No need to complicate.

**PI resolution:** _pending_

### §10.8. D-H. Localization architecture split

**Question.** Confirm the messages-vs-data split architecture described in §6.5?

**Recommendation.** Accept. Five independent ecosystem precedents (§6.4) converge on the same split; neoipcr is already close to it; the A4a enumeration showed the scope of the migration is narrow (one function family, 10 call sites, plus 2 bugs). The gettext-vs-`rlang::abort()` question — whether to additionally migrate messages from `stop(gettext(...))` to class-tagged conditions — is flagged as a Phase 3 / follow-up question.

**What this commits:**

- `gettext` / `gettextf` stays on messages only.
- Factor labels (the 10 procedure-category labels) migrate to `procedure_category_labels` tibble in `sysdata.Rda` (§7 row 2).
- DHIS2 `display*` fields flow through unchanged; no `gettext` wraps added on top (§6.3 confirms none exist today).
- Every neoipcr entry point that produces localized output accepts an explicit `locale` parameter; the resolution chain is `explicit_arg → opts$locale → Sys.getlocale("LC_MESSAGES") → "en"`.
- Two `gettext` bugs ([calc-api.R:840](R/calc-api.R#L840), [calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45)) fixed in Phase 2.

**Rationale.** See §6.4 precedents + §6.5 proposal + §6.6 migration plan. This is the most architecturally consequential decision in this note because §5, §7, §9, and Phase 2/4 all key off it.

**PI resolution:** _pending_

### §10.9. D-I. Cross-surface naming alignment for result tables (across 9 surfaces, code + report display)

**Question.** Adopt the master-name approach in §9.5 (single canonical AMA-grounded name per result table; mechanical derivation rules for nine surfaces: function / class / slot / rate / Quarto param / PS switch / `.qmd` filename / heading key / display string)? And — separately — confirm or amend each of the 13 master-name proposals in §9.5.4 plus their per-row section-vs-table heading treatment (option a/b/c per §9.5.2.2)?

**Recommendation.** Accept the framework (master-name + derivation rules) — including the §9.5.5 cross-domain alignment principle that drops the earlier "separation of identifier from display" carve-out. Each of the 13 master-name proposals deserves an individual yes/no/amend; recommendations from §9.5.4 reproduced here for one-pass review:

| # | Today | Proposed master | Heading treatment |
|---|-------|-----------------|-------------------|
| 1 | usage / risk density | `usage_density_rate` | (b) keep section heading "Presence of Risk and Protective Factors"; sub-heading per table |
| 2 | antibiotic utilisation | `antibiotic_utilisation_rate` (also: British vs American?) | (a) align to master |
| 3 | surgery / surgical procedure rate | `surgery_rate` | (b) keep section "Surgical Procedures by Category" |
| 3b | (asymmetric ref function) | Phase 2 design fix, not §9.5 | — |
| 4 | incidence density rate | `incidence_density_rate` | (b) keep section "Severe Infections and NEC" |
| 5 | dev-ass incidence density | `device_associated_incidence_density_rate` | (a) align |
| 6 | iadrpit | `agent_detection_rate_per_infection_type` | (b) keep section "Infectious Agents in Nosocomial Infections"; sub-heading per table |
| 7 | iadrpa | `agent_detection_rate_per_agent` | (a) align under same parent as #6 |
| 8 | abr / resistant pathogen / antibiotic resistance | `antibiotic_resistance_infection_rate` (alt: `amr_infection_rate`) | (a) align |
| 9 | organism resistance rate | `organism_resistance_rate` | (a) align |
| 10 | resistance test rate | **`antimicrobial_susceptibility_test_rate`** (adopt the H-surface AMA term, rename F/S/C/P/Q/Z to match) | (a) align |
| 11 | secondary BSI rate | `secondary_bsi_rate` | (a) align — D string can spell out "Bloodstream Infection" if AMA prefers |
| F1 | birth weight figure | `birth_weight_figure` | (a) align — Z renames `_fig-bw.qmd` → `_fig-birth-weight.qmd` |
| F2 | gestational age figure | `gestational_age_figure` | (a) align — Z renames `_fig-ga.qmd` → `_fig-gestational-age.qmd` |

**PI calls flagged in §9.5.4 that need explicit answers:**

- Row 1: is "usage density" the correct epidemiological term, or should the master adopt the report's existing "risk density" wording across all surfaces?
- Row 2: British `utilisation` vs American `utilization`?
- Row 6: keep "infectious agent" in display copy but use shortened "agent" in identifiers, or align both?
- Row 8: `antibiotic_resistance` vs `amr` (antimicrobial resistance) — which is preferred in the surveillance literature you cite?
- Row 10: confirm adopting AMA's "antimicrobial susceptibility test" over the package's current "resistance test"? This is the largest individual rename.
- F1/F2: P-surface currently says "Distribution"; F/S/Q/Z currently say "Figure". Pick one.

**Rationale.** Reduces a nine-way memorization tax to one canonical name per table. Per §9.5.5, the alignment extends to display headings (PDF, HTML) so report readers and code readers share vocabulary. Coordinated with task 1.2 (§9.5.6).

**Scope of Phase 3 execution if D-I is accepted.** Per §9.5.7: lockstep renames across neoipcr R/ + Surveillance-Toolkit reports (Quarto params, `.qmd` filenames, heading keys, `_sR.yaml` display strings) + scripts (PowerShell wrapper mapping tables) + .NET reporting service. **Plus** a po4a/Weblate translation cycle for the display strings: existing translations get marked fuzzy in `.po` files and are reconfirmed via Weblate. Estimate: 5–10 PRs spread over 1–2 weeks of execution time, plus the translation cycle (variable depending on translator availability).

**PI resolution:** _pending — needs per-row confirmation of the 13 master-name proposals **and** per-row choice of heading-treatment option (a/b/c)_

---

## §11. Roxygen conventions to preserve

In-tree exemplars — do not invent new conventions:

- [get_pathogen_taxonomy()](R/pathogens.R) — model for exported accessors over package data.
- [calculate_department_data()](R/calc-api.R) — model for pipeline entry points with full roxygen.
- [dhis2_connection_options()](R/dhis2-connect.R) — model for option-constructor functions with parameter semantics documented in detail.

**Example runtime for Phase 4 examples.** Test fixtures already exist in [tests/testthat/helper-fixtures.R](tests/testthat/helper-fixtures.R): `read_test_metadata()`, `make_test_ds()`, `make_populated_test_ds()`, `make_calc_test_ds()`, plus per-table builders. Phase 4 `@examples` blocks should hook into these rather than inventing new fixtures.

---

## §12. Cross-refs to sibling tasks

- [tasks/neoipcr-class-slug-rename.md](../../tasks/neoipcr-class-slug-rename.md) — owns the `_iaf` / `_sbd` / `_udr` rename. §3/§4/§9 assume post-rename names.
- [tasks/csv-to-yaml-migration.md](../../tasks/csv-to-yaml-migration.md) — owns the infectious-agent metadata reshape. §7 rows touching infectious agents wait on it.
- [tasks/neoipcr-lifecycle-badges.md](../../tasks/neoipcr-lifecycle-badges.md) — consumes the audience-tier column of §3 and applies `lifecycle::badge()` markup. Cannot start until this note lands.
- [tasks/completed/neoipcr-empty-data-resilience.md](../../tasks/completed/neoipcr-empty-data-resilience.md) — already completed; remaining crash paths are scoped out.
- [tasks/completed/neoipcr-test-coverage.md](../../tasks/completed/neoipcr-test-coverage.md) — already completed; the fixture helpers §11 points at are its output.
