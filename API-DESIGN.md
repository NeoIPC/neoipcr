# neoipcr — API design note (Phase 1 of public-interface refinement)

**Purpose.** This note is the single deliverable of Phase 1 of the public-interface refinement task. It pins the public-surface decisions needed to unblock Phase 2 (helper promotion), Phase 3 (renames), and the sibling lifecycle-badges task. Nothing in this note changes code; the commitments here are executed by later phases.

**On the task references below.** Numbered tasks and phases (`task 1.1`, `task 1.2`, the CRAN-release coordination plan, the CSV-to-YAML migration) live in NeoIPC's internal planning repository, which is not part of this one. They are named rather than linked: a reader of this repository alone cannot open them, and a path that resolves only inside one particular multi-repository checkout is a dead link dressed as a reference. Everything this note *commits to* is stated here in full; the task names locate the surrounding work for anyone who does have that repository.

**On the Surveillance-Toolkit citations below.** Every citation that names a line range links to a *pinned* commit rather than to `main`. The reports repository moves independently of this one, so a line anchor on a branch keeps resolving while pointing at whatever code has since taken that line — a reference that is wrong without ever saying so. The pin makes each citation permanently show the code its surrounding claim was written about; refresh it by re-deriving the ranges against a newer commit, never by editing the revision in place. Citations with no line anchor — a directory, a whole document, a script to run — point at `main` instead, because there is no line for a moving branch to invalidate and current content is what a reader of those wants.

**Audiences addressed.** Data scientists at partner departments, external researchers building their own reports/tools, clinicians running ad-hoc queries, and the internal NeoIPC pipeline (Partner-/Reference-/Validation-Report, Patient-Data-Report, the .NET reporting service, the DHIS2 app shell). All four are first-class.

**Pre-alpha release status.** The package is pre-alpha (version `0.0.0.9000` in [DESCRIPTION](DESCRIPTION) — the `.9000` development suffix; `0.1.0` stays reserved for the first CRAN release). Renames and promotions are direct — no `lifecycle::deprecate_warn()` aliases. Internal Surveillance-Toolkit callers move in lockstep with API changes.

---

## §1. Status

**Phase 1 exit-criterion checklist.** All items ticked — every decision below surfaces in §10 for PI resolution; content is ready for Phase 2 to consume once D-A…D-H are resolved.

- [x] Every `export()` line in [NAMESPACE](NAMESPACE) has a row in §3 with eight columns populated.
- [x] All 3 `S3method()` entries have a row in §3 with dispatch class recorded.
- [x] Each of the four custom classes (`neoipcr_ds`, `neoipcr_rep_ds`, `neoipcr_ref_ds`, `neoipcr_tbl_sr_ref`) has a §4 subsection with constructor site, columns, invariants, and pointer to the relevant `R/schema-*.R`.
- [x] The `_iaf` / `_sbd` / `_udr` subclasses each have a §4 row (§4.5) under the post-task-1.2 names with an explicit "depends on task 1.2" callout. Three additional subclasses surfaced and are flagged in §3.2 for task 1.2 expansion.
- [x] Every helper from the Phase 2 sketch of the task file appears in §5 (46 rows) with target `R/*.R` file + signature. Rejected promotions listed in §5.2 with rationale.
- [x] Every `gettext` / `gettextf` call site in `R/` is classified in §6.1. F-class + C-class (13 sites) individually enumerated in §6.1.1; the M-class sites, every condition message, the two credential prompts, and the print method's text, share a single migration action in §6.1.2, and §6.8 describes their catalogue.
- [x] §6 states the architectural split (messages vs. data) as a decision, not a proposal — §6.5, governed by D-H (§10.8) for PI confirmation.
- [x] §6 cites 5 established R-ecosystem precedents — §6.4 (countrycode, Writing R Extensions, scales, rlang/cli, potools).
- [x] The unified locale-resolution chain is stated in §6.7 and referenced from relevant decisions in §10.
- [x] §6 carries a per-call-site migration plan for Phase 2 — §6.6.
- [x] Every package-data candidate has a §7 row (7 rows); infectious-agent files are explicitly held for the CSV-to-YAML migration in §7.4.
- [x] §8 states the GT-styling boundary decision (S3 `as_gt.*` methods); describes the DESCRIPTION delta (add `gt` to `Suggests`).
- [x] §9 covers every rename required by Phase 3 of the task file — two function renames (D-C, D-D) plus coordination with task 1.2 for class renames.
- [x] §10 lists D-A through D-I, each with a recommendation and "PI resolution: _pending_" line.
- [x] §9.5 catalogues the cross-surface naming divergence for all 11 result tables + 2 figures across **nine surfaces** (function / class / slot / rate-name / Quarto param / PS switch / `.qmd` filename / heading key / display string); proposes a master-name + derivation-rules scheme; addresses the section-vs-table heading question per row; aligns code identifiers with PDF display strings via the po4a translation cycle; coordinates with task 1.2.
- [x] §11 points at three in-tree roxygen exemplars and the test-fixture helpers.
- [x] §12 cross-refs five sibling tasks.

**Decisions pending PI sign-off.** See §10 for full context on each.

| ID | Decision | Recommendation | PI resolution |
|----|----------|----------------|---------------|
| D-A | Audience tier per public-surface symbol | Defaults from usage patterns (see §3) | pending |
| D-B | GT-styling entry point shape | `as_gt.neoipcr_tbl_sr_ref()` (S3) | pending |
| D-C | `get_benchmark_data()` rename | **Withdraw** — function combines, doesn't compute (see §10.3) | pending |
| D-D | CI-function prefix unification | Rename all three to `neoipcr_*_ci()` (matches package name) | pending |
| D-E | Long table-function-name shortening | Keep as-is; rely on `@family` grouping | pending |
| D-F | Accessor-vs-raw-data per package-data candidate | Accessor per taxonomy (follows `get_pathogen_taxonomy()`) | pending |
| D-G | Message-catalogue scope | Single `R-neoipcr.pot` | pending |
| D-H | Localization architecture split | See §10 / §6 for the full proposal | pending |
| D-I | Cross-surface naming alignment (9 surfaces, code + report display) | Master-name + heading-treatment table in §9.5.4 / §10.9; supersedes parts of task 1.2; alignment extends to PDF display strings via po4a cycle | pending |

---

## §2. Scope and non-goals

**In scope (this note).** Inventory and design paperwork only. Produces commitments for later phases of task 1.1 to execute.

**Out of scope (this note).** Every code change. Specifically: no new `R/*.R` files, no `NAMESPACE` edits, no roxygen regeneration, no `DESCRIPTION` edits, no rename applied anywhere, no Surveillance-Toolkit `_setup.qmd` / `Generate-*.R` edits, no `data-raw/sysdata.R` extensions, no `po/` catalogue regeneration, no `man/*.Rd` edits, no vignettes / README.Rmd / `_pkgdown.yml`, no convenience-layer (`neoipcr_quickstart()`). Those all happen in Phases 2–6 of the task file — governed by the commitments below.

**Out of scope (permanently or by handoff to sibling tasks).** Listed in §12.

---

## §3. Public surface inventory

Every `export()` and `S3method()` entry in [NAMESPACE](NAMESPACE).

Where a row's return-class slug is scheduled for rename by task 1.2 (the class-slug rename), the current slug is given first and the candidate post-rename name in a parenthetical. Slug-rename scheme in task 1.2 is labelled "suggestions, not commitments" — this note does not pin the scheme, it cross-references it.

| # | Symbol | File | Signature sketch | Returns class | Audience tier | Lifecycle | Rename proposal | Notes |
|---|--------|------|------------------|---------------|---------------|-----------|-----------------|-------|
| 1 | `import_dhis2` | [R/import-dhis2.R](R/import-dhis2.R) | `(connection_options, dataset_options)` | `neoipcr_ds` | external-stable | stable | — | Core entry point; invokes the 5-step auth chain. The dataset carries the validation pass's findings (`validationResults`, the five columns `validate()` returns, without its `rules_run`, `rules_skipped` and `findings_exempted` attributes) and their counts (`validationSummary`: per rule the distinct records removed, exempted, and warned of, at the rule's record kind, and a totals row per record kind; the pass removes the patients an error flags and keeps those only a warning flags); both are 0×0 when the pass does not run, under `include_invalid_patients = TRUE` or without patients. It also carries the counts of the NeoIPC coordinating centre's reconciliation, which repairs before the filters and the pass the stored values Tracker Capture derives or hides and the partner never chooses (`reconciliationSummary`: per reconciliation the distinct records of the returned dataset repaired and reported, at the reconciliation's record kind, zero rows included, `NA` where the import could not read the records a reconciliation acts on, and no totals rows); it is 0×0 under `reconcile = FALSE` or without patients. |
| 2 | `dhis2_connection_options` | [R/dhis2-connect.R](R/dhis2-connect.R) | `(token, username, session_id, scheme, hostname, port, path)` | `neoipcr_dhis2_conopt` (→ `neoipcr_dhis2_connection_options`) | external-stable | stable | — | Auth entry; documents auth fallback chain. |
| 3 | `dhis2_dataset_options` | [R/dhis2-options.R](R/dhis2-options.R) | `(..., reconcile = TRUE, translate = TRUE, locale = NULL)` | `neoipcr_dhis2_dsopt` (→ `neoipcr_dhis2_dataset_options`) | external-stable | stable | — | Data-protection gates + locale entry point. `reconcile` switches the coordinating centre's reconciliation, on by default; `FALSE` keeps every value as stored, which a copy of the stored record under Article 15 of the GDPR and `reconciliation_details()` need. |
| 4 | `calculate_reference_data` | [R/calc-api.R](R/calc-api.R) | `(x, use_cache = TRUE)` | `neoipcr_ref_ds` (→ `neoipcr_reference_ds`) | internal-stable | stable | — | Reference-Report entry. Carries `validationSummary` and a serializable copy of the options: an exception list replaced by `"exception_list_applied"`, a department filter by `"applied"` (an empty one leaves as `NULL`), and the emitted copy asserted to hold no data frame and to name no department. Refuses a dataset without the summary. Carries `reconciliationSummary` as well, `NULL` for a raw dataset written before that slot existed; a slot of another shape is refused (`neoipcr_malformed_reconciliation_summary`). |
| 5 | `calculate_department_data` | [R/calc-api.R](R/calc-api.R) | `(x, use_cache = TRUE)` | `neoipcr_rep_ds` (→ `neoipcr_report_ds`) | internal-stable | stable | — | Partner-Report entry. Carries `validationSummary`; an exception list is replaced by `"exception_list_applied"`, the department's own filter kept, and the emitted copy asserted to hold no data frame. Refuses a dataset without the summary. Carries `reconciliationSummary` as row 4 does. |
| 6 | `get_benchmark_data` | [R/calc-api.R](R/calc-api.R) | `(...)` | `neoipcr_bnch_ds` *(not in task 1.2 rename scope — see §3.2)* | internal-stable | stable | — (D-C: rename withdrawn — function combines pre-computed datasets, doesn't calculate) | Side-by-side stitcher with one CI fix-up; not a calculator. Roxygen at [calc-api.R:313](R/calc-api.R#L313) says "Creates ... from ...". Each dataset's `metadata`, `validationSummary` and `reconciliationSummary` ride under the dataset's name; a dataset serialized before a summary existed, or calculated from a raw dataset written before it existed, contributes no entry to it. |
| 7 | `pretty_names` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` (S3 generic) | varies | internal-stable | experimental | — | S3 generic. |
| 8 | `pretty_names.default` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` | identity on `x` | internal-stable | experimental | — | Default method. |
| 9 | `pretty_names.neoipcr_tbl_sr_ref` | [R/calc-api.R](R/calc-api.R) | `(x, ...)` | `neoipcr_tbl_sr_ref` (→ `neoipcr_surgery_rate_table_ref` — candidate) | internal-stable | experimental | — | Translates column/row names; its six column headers go through one multi-argument `gettext` (§6.1.1). |
| 10 | `get_usage_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_udr` / `_udr_ref` (→ `neoipcr_usage_density_rate_table`) | internal-stable | stable | — | Part of rate-table family (@family grouping — §11). |
| 11 | `get_antibiotic_utilization_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `tibble` with class markers | internal-stable | stable | — | Return class slug deferred; no dedicated slug in task 1.2. |
| 12 | `get_surgery_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache)` | `neoipcr_tbl_sr` (→ `neoipcr_surgery_rate_table`) | internal-stable | stable | — | — |
| 13 | `get_ref_surgery_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(ref, use_cache)` | `neoipcr_tbl_sr_ref` (→ `neoipcr_surgery_rate_table_ref` — candidate) | internal-stable | stable | — | Reference variant; 13 columns, see §4.4. |
| 14 | `get_incidence_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_idr` (→ `neoipcr_incidence_density_rate_table`) | internal-stable | stable | — | — |
| 15 | `get_dev_ass_incidence_density_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_daidr` (→ `neoipcr_device_associated_incidence_density_rate_table`) | internal-stable | stable | — | Long name — D-E recommends keeping. |
| 16 | `get_infectious_agent_detection_rate_per_inf_type_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_iadrpit` (→ `neoipcr_infectious_agent_detection_rate_per_infection_type_table`) | internal-stable | stable | — | — |
| 17 | `get_infectious_agent_detection_rate_per_agent_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_iadrpa` / `_ref` (→ `neoipcr_infectious_agent_detection_rate_per_agent_table`) | internal-stable | stable | — | — |
| 18 | `get_abr_infection_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_abr_ir` / `_ref` (→ `neoipcr_abr_infection_rate_table` — keeps `abr`) | internal-stable | stable | — | — |
| 19 | `get_organism_resistance_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_org_rr` / `_ref` (→ `neoipcr_infectious_agent_resistance_rate_table`) | internal-stable | stable | `get_organism_resistance_rate_table` → `get_infectious_agent_resistance_rate_table` (§13.8) | — |
| 20 | `get_resistance_test_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_rtr` / `_ref` *(not in task 1.2 rename scope — see §12)* | internal-stable | stable | — | Slug `_rtr` not listed in task 1.2 rename table. |
| 21 | `get_secondary_bsi_rate_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, use_cache, include_quartiles)` | `neoipcr_tbl_sec_bsi` / `_ref` *(not in task 1.2 rename scope — see §12)* | internal-stable | stable | — | Slug `_sec_bsi` not listed in task 1.2 rename table. |
| 22 | `neoipc_poisson_ci` | [R/ci.R](R/ci.R) | `(events, exposure, multiplier = 1000, conf.level = 0.95)` | `list(rate, lower, upper)` | external-stable | stable | `neoipc_poisson_ci` → `neoipcr_poisson_ci` (D-D) | Existing prefix `neoipc_` doesn't match package name `neoipcr`. Well-documented; has `@examples`. |
| 23 | `neoipc_wilson_ci` | [R/ci.R](R/ci.R) | `(x, n, conf.level = 0.95)` | `list(proportion, lower, upper)` | external-stable | stable | `neoipc_wilson_ci` → `neoipcr_wilson_ci` (D-D) | Existing prefix `neoipc_` doesn't match package name `neoipcr`. Well-documented; has `@examples`. |
| 24 | `bootstrap_quantile_ci` | [R/ci.R](R/ci.R) | `(events, exposure, type, multiplier, B, conf.level, seed)` | `tibble` (6 cols) | external-stable | experimental | `bootstrap_quantile_ci` → `neoipcr_bootstrap_quantile_ci` (D-D) | Not integrated into rate-table pipeline. |
| 25 | `get_pathogen_taxonomy` | [R/pathogens.R](R/pathogens.R) | `(ids = NULL)` | `tibble` (taxonomic hierarchy) | external-stable | stable | — | Roxygen model for accessors over package data (§11). |
| 26 | `is_valid_ichi_code` | [R/ichi.R](R/ichi.R) | `(code)` | `logical` | external-stable | experimental | — | Syntax validator only. Bundling ICHI code list blocked by WHO licensing (see §7). |
| 27 | `print.neoipcr_dhis2_conopt` | [R/dhis2-connect.R](R/dhis2-connect.R) | `(x, ...)` S3 method | `invisible(x)` | external-stable | stable | — | Dispatch class: `neoipcr_dhis2_conopt` → `neoipcr_dhis2_connection_options`. |
| 28 | `neoipcr_log_config` | [R/log.R](R/log.R) | `(verbosity = NULL)` | the applied `logger` threshold, invisibly | external-stable | stable | — | Sets this package's `"neoipcr"` logger-namespace threshold from `quiet` / `normal` / `verbose` / `debug`; with no argument it re-reads `NEOIPC_LOG_LEVEL`. Carries the `neoipcr_` prefix under the §9.3 rule-5 exception. |
| 29 | `neoipcr_supported_versions` | [R/import-dhis2.R](R/import-dhis2.R) | `()` | `list(dhis2, metadata_package)` | external-stable | stable | — | Introspection: the DHIS2 releases verified against a live server and the supported metadata-package range; `import_dhis2()` warns on a server outside them. Carries the `neoipcr_` prefix under the §9.3 rule-5 exception. |
| 30 | `write_json` | [R/json.R](R/json.R) | `(x, file = NULL, pretty = FALSE)` | JSON string (invisible when `file` is given) | internal-stable | experimental | — | Plain-JSON serializer for consumers in other languages (the .NET reporting service); a deliberately narrow first cut with no redaction of its own. |
| 31 | `get_cumulative_incidence_table` | [R/calc-tables.R](R/calc-tables.R) | `(x, ...)` | tibble | external-stable | experimental | — | The share of patients (or admissions) admitted to a department within a calendar window who acquired an infection within that window, with a Wilson interval. |
| 32 | `validate` | [R/validation.R](R/validation.R) | `(x, rules = NULL, exceptions = NULL, as_of = NULL)` | tibble (`rule_id`, `patient_key`, `enrollment_key`, `event_key`, `context`) | external-stable | experimental | — | The validation rules over a dataset; a finding is data (keys and a one-row `context` tibble), never prose. Aborts on a rule id it does not know. `as_of` is the date the two open-enrolment rules measure against, defaulting to the import's server date. Its attributes name the rules that ran (`rules_run`) and those that could not (`rules_skipped`), and hold the findings the exceptions exempted (`findings_exempted`); they describe the whole run, also on a subset of the rows. |
| 33 | `validation_rule_ids` | [R/validation.R](R/validation.R) | `()` | integer vector | external-stable | experimental | — | The registry's rule ids, for consumers that let a user choose rules or keep a catalogue of rule descriptions. |
| 34 | `read_validation_exceptions` | [R/validation-exceptions.R](R/validation-exceptions.R) | `(path)` | tibble (`RULE_ID`, `NEOIPC_PATIENT_ID`, `ENROLMENT_DATE`, `EVENT_TYPE`, `EVENT_DATE`, optional `DEPARTMENT_CODE`) | external-stable | experimental | — | The exception-list CSV reader with the shape checks `import_dhis2()` applies; every defect is `neoipcr_invalid_exception_list`. |
| 35 | `resolve_validation_exceptions` | [R/validation-exceptions.R](R/validation-exceptions.R) | `(x, exceptions)` | tibble (`rule_id` + keys) | external-stable | experimental | — | Maps a written list onto a dataset's keys, as a whole per record; `validate()` calls it itself. |
| 36 | `validation_rule_context_fields` | [R/validation.R](R/validation.R) | `()` | named list of character vectors | external-stable | experimental | — | The context field names each rule records, per rule id, as the registry declares them; the contract a consumer's sentence templates are checked against. `validate()` refuses a finding whose fields differ from the declaration. |
| 37 | `reconciliation_ids` | [R/reconciliation.R](R/reconciliation.R) | `()` | integer vector | external-stable | experimental | — | The reconciliations' ids, an id space of their own, for consumers that keep a label per reconciliation. `@family reconciliation`. |
| 38 | `reconciliation_details` | [R/reconciliation.R](R/reconciliation.R) | `(x)` | tibble (`reconciliation_id`, `action`, `patient_key`, `enrollment_key`, `event_key`, `context`) | external-stable | experimental | — | The coordinating centre's record-by-record view of what `reconciliationSummary` counts: the stored and the reconciled value of every record a reconciliation repairs or reports. Needs a dataset imported with every record and value as stored (`reconcile = FALSE`, `include_invalid_patients = TRUE`, `include_ineligible_patients = TRUE`, no range filter and no reporting period, full patient, enrolment and event tiers, the gestational age selected); any other is refused (`neoipcr_reconciliation_needs_stored_values`), naming what is missing. `@family reconciliation`. |
| 39 | `validation_summary` | [R/validation.R](R/validation.R) | `(findings)` | tibble (`rule_id`, `record_kind`, `n_flagged`, `n_exempted`) | external-stable | experimental | — | Counts a `validate()` result's flagged and exempted records per rule and record kind, as `import_dhis2()` stores them in `validationSummary`; reads the result's `findings_exempted` attribute and refuses a result without the attributes `validate()` sets (`neoipcr_invalid_validation_result`). |
| 40 | `validation_exception_usage` | [R/validation-exceptions.R](R/validation-exceptions.R) | `(x, exceptions, findings)` | tibble (the list's records for the dataset's departments, with `matched` and `n_exempted`) | external-stable | experimental | — | Whether each record of an exception list matches the dataset, and how many records it exempted in a `validate()` result, `NA` for a rule the result did not run; a record for a department the dataset does not hold is left out, so a list with department codes needs a dataset that carries them (`neoipcr_validation_needs_facts`). For the upkeep of the list. |
| 41 | `validation_rule_severities` | [R/validation.R](R/validation.R) | `()` | named character vector (`"error"` or `"warning"`, per rule id) | external-stable | experimental | — | Each rule's severity as the registry declares it, for consumers that present a warning differently from an error. The import's pass removes the patients an error flags and keeps those only a warning flags; `validationSummary$n_warned` counts every record a warning flags, whether or not an error removed its patient. |

### §3.1. Naming patterns and inconsistencies (A1 findings, feed into §9)

- **CI function prefix inconsistency + package-name mismatch** → D-D. Two functions carry a `neoipc_` prefix that doesn't even match the package name (`neoipcr`); the third has no prefix at all. Recommendation: rename all three to `neoipcr_*_ci()`.
- **`get_*` vs `calculate_*` semantic confusion** → D-C. `get_benchmark_data()` computes; its siblings are `calculate_*`.
- **Result-table class hierarchy is ad-hoc**: no consistent inheritance contract between `_tbl_*` and `_tbl_*_ref` variants. `neoipcr_ref_ds` extends `neoipcr_rep_ds` (consistent); the table classes do not. Not this note's scope to fix — flag for consideration when rate-table expansion tasks land.
- **(Resolved by D-D)** Earlier draft of this section dismissed the `neoipcr` package vs. `neoipc_` function prefix mismatch as "cosmetic, not worth fixing." On reflection (and after PI pushback) the rename is the right move — see D-D and §9.1. Keeping this bullet visible because the original dismissal was wrong and worth flagging for posterity.

### §3.2. Classes not covered by task 1.2 (gap report)

Audit A1 discovered three return-class slugs that are **not listed in task 1.2's rename table**:

| Current slug | Where returned | Meaning | Suggested addition to task 1.2 |
|---|---|---|---|
| `neoipcr_bnch_ds` | `get_benchmark_data()` | Merged benchmark dataset | `neoipcr_benchmark_ds` |
| `neoipcr_tbl_rtr` / `neoipcr_tbl_rtr_ref` | `get_resistance_test_rate_table()` | Completeness of antimicrobial susceptibility testing | per §9.5 master |
| `neoipcr_tbl_sec_bsi` / `neoipcr_tbl_sec_bsi_ref` | `get_secondary_bsi_rate_table()` | Secondary-BSI rates by infection type | per §9.5 master |

**Recommendation**: add these rows to task 1.2 before it executes, so the rename is comprehensive. The two `neoipcr_tbl_*` slugs should adopt the §9.5 master-name scheme (D-I) rather than be renamed independently — see §9.5.6.

---

## §4. Custom-class schemas

The four main classes span two shapes: dataset lists (`neoipcr_ds`, `neoipcr_rep_ds`, `neoipcr_ref_ds`) and individual result tables (`neoipcr_tbl_sr_ref`). Schema authority lives in `R/schema-*.R`; this section gives constructor sites, member tibbles, invariants, predicates — and references those schema files rather than restating column-by-column.

### §4.1. `neoipcr_ds`

**Constructor site**: [R/import-dhis2.R:254](R/import-dhis2.R#L254) — `structure(..., class = c("neoipcr_ds", "list"))`.

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
| `validationResults` | *(unclassed tibble — the five columns `validate()` returns)* | — | [R/schema-validation.R](R/schema-validation.R) → `validationResults_cols` |
| `validationSummary` | *(unclassed tibble — `rule_id`, `record_kind`, `n_removed`, `n_exempted`, `n_warned`)* | — | [R/schema-validation.R](R/schema-validation.R) → `validationSummary_cols` |
| `reconciliationSummary` | *(unclassed tibble — `reconciliation_id`, `record_kind`, `n_repaired`, `n_reported`)* | — | [R/schema-reconciliation.R](R/schema-reconciliation.R) → `reconciliationSummary_cols` |
| `metadata` | `neoipcr_metadata` | `neoipcr_metadata` *(keep)* | [R/schema-orgunits.R](R/schema-orgunits.R) + [R/dhis2-metadata-reference.R](R/dhis2-metadata-reference.R) |
| `.cache` | (environment) | — | [R/cache.R](R/cache.R) |

**Invariants**:

- **Data-protection contract** enforced by `assert_data_protection()` in [R/data-protection.R](R/data-protection.R) — aborts on reader regression. See [.claude/rules/package-internals.md](.claude/rules/package-internals.md) § "Integrated Data Protection".
- **Three-mode schema contract** per fact tibble: `"no"` → 0×0 (entity gate rejects); `"pseudo"` → minimal key-only; `"full"` → full column set per `include_*` flags. Contract is authoritative in `R/schema-tools.R`.
- **Foreign-key invariants**: `patients ← enrollments ← events` chain on integer keys (`patient_key`, `enrollment_key`, `event_key`) — never on DHIS2 UIDs (see [.claude/rules/r-code.md](.claude/rules/r-code.md) § "Joining Tibbles").
- **Redundant hierarchy keys**: `department_key`, `hospital_key`, `country_key`, `world_bank_class_key` appear directly on `patients`, `enrollments`, and `events` — chain-breakable per [.claude/rules/package-internals.md](.claude/rules/package-internals.md) § "Redundant Foreign Keys".
- **Conditional companion gates**: user fields gate on `include_user`; timestamps gate on `include_timestamps`; deletion flag gates on `include_deleted`; DHIS2 IDs gate on `include_dhis2_ids`.
- **Validation slots**: `validationResults` and `validationSummary` are populated exactly when the import's validation pass ran — patients imported and `include_invalid_patients` not `TRUE` — and 0×0 otherwise; an import whose pass could not run a rule aborts (`neoipcr_validation_rule_skipped`) rather than storing a pass that reads as complete. Shape documented on `?import_dhis2`.
- **Reconciliation slot**: `reconciliationSummary` is populated exactly when the import reconciled — `reconcile = TRUE` and patients imported — and 0×0 otherwise. Its gate is deliberately not the validation pass's: an import with `include_invalid_patients = TRUE` runs no pass but reconciles, so a consumer that validates such a dataset validates reconciled data. An options object without `reconcile` predates the option and reads as not reconciled. The reconciliation reads the import's internal maps (links, dates, every finding's slot and concept), so it acts on the same records under every tier; the per-record log it keeps while the import runs is an internal metadata element, stripped with the internal maps before `assert_data_protection()`. The summary counts the records the returned dataset holds, after the period narrowing, the filters, the pass and `apply_postfilter()`, and passes `finalize_to_schema()` and `assert_schema()`. Shape documented on `?import_dhis2`; each reconciliation on `reconcile` in `?dhis2_dataset_options`, numbered as `reconciliation_ids()` lists it.

**Predicates**: `is_neoipcr_ds()`, `check_neoipcr_ds()`, plus the compound guards `check_neoipcr_ds_or_rep_ds()`, `check_neoipcr_ds_or_ref_ds()` — all in [R/types-check.R](R/types-check.R).

---

### §4.2. `neoipcr_rep_ds` → post-1.2 `neoipcr_report_ds`

**Constructor site**: [R/calc-api.R:310](R/calc-api.R#L310) — `structure(..., class = c("neoipcr_rep_ds", "list"))`.

**Shape**: list of tibbles — computed view of an imported `neoipcr_ds`, aggregated at department level.

**Member tibbles**:

- `metadata` — list: `calculated` (timestamp), `dataset_options` (the serializable copy: an exception list replaced by `"exception_list_applied"`, the department's own filter kept), `data_up_to`, `effective_analysis_period`, `hospitals`, `departments`, `countries`.
- `validationSummary` — the imported dataset's summary (§4.1), carried verbatim; the calculation refuses a dataset without it.
- `reconciliationSummary` — the imported dataset's reconciliation summary (§4.1), carried verbatim; `NULL` for a raw dataset written before the slot existed, which the calculation accepts because a raw dataset kept as a backup outlives the code that wrote it (`NEOIPC-PERMANENT(dataset-format)` in `assert_reconciliation_summary()`). A slot that is neither the schema's columns nor 0×0 is refused with class `neoipcr_malformed_reconciliation_summary`.
- `birth_weight_figure`, `gestational_age_figure` — histogram tibbles for figures.
- `n_departments`, `n_patients`, `n_enrollments`, `n_patient_days` — summary counts.
- `n_surgical_departments`, `n_surgical_patients`, `n_surgical_procedures` — surgery-specific counts.
- `n_infections` — tibble (`inf_type`, `total`).
- Eleven result tables: `usage_density_rate_table`, `antibiotic_utilization_table`, `surgery_rate_table`, `incidence_density_rate_table`, `dev_ass_incidence_density_rate_table`, `infectious_agent_detection_rate_per_agent_table`, `infectious_agent_detection_rate_per_inf_type_table`, `abr_infection_rate_table`, `organism_resistance_rate_table`, `secondary_bsi_rate_table`, `resistance_test_rate_table` — each a tibble with factors, counts, pooled rate, and its 95 % interval: Wilson for the shares (`infectious_agent_detection_rate_per_inf_type_table`, `abr_infection_rate_table`, `organism_resistance_rate_table`, `resistance_test_rate_table`, `secondary_bsi_rate_table`), Poisson for the others. **No quartile columns** — that's what distinguishes `neoipcr_rep_ds` from `neoipcr_ref_ds`.

**Invariants**:

- Computed via `calculate_department_data()`. Requires `include_patient`, `include_enrollment`, and `include_department` each at `"pseudo"` or `"full"`, and `include_event` at `"full"`: the infectious-agent figures read the findings' `pathogen_key` and `secondary_bsi`, which the `"pseudo"` tier leaves out because a rare infectious agent can single out a patient. Another dataset is refused with an error of class `neoipcr_unmet_import_options`.
- `secondary_bsi_rate_table` also carries, beside each rate, the infections followed up for a secondary BSI (`followup_n`), all infections of the type (`total_n`), and the share followed up (`followup_pooled`).
- `infectious_agent_detection_rate_per_inf_type_table` has the rows `all`, `bsi`, `hap`, and `ssi` (`inf`), NEC having none. Each gives two shares: the share positive among the tested (`n` over `tested`, `pooled`, `ci_lower`, `ci_upper`) and the share tested (`tested` over `total`, every infection of the type; `tested_pooled`, `tested_ci_lower`, `tested_ci_upper`), the latter `NA` for `bsi`, whose form does not record whether a blood culture was taken, and for `all`, the share of every infection in which a slot names an infectious agent.
- Summary counts inherit the user's `dataset_options` (display-purpose metadata).
- Result tables carry pooled-rate + CI only — no department-level distribution statistics.

**Predicates**: `is_neoipcr_rep_ds()`, `check_neoipcr_rep_ds()`, `check_neoipcr_ds_or_rep_ds()` — in [R/types-check.R](R/types-check.R).

---

### §4.3. `neoipcr_ref_ds` → post-1.2 `neoipcr_reference_ds`

**Constructor site**: [R/calc-api.R:188](R/calc-api.R#L188) — `structure(..., class = c("neoipcr_ref_ds", "neoipcr_rep_ds", "list"))`. **Inherits from `neoipcr_rep_ds`.**

**Shape**: same member structure as `neoipcr_rep_ds` with additional quartile statistics.

**Delta from §4.2**:

- Every result table gains `q1`, `q2`, `q3` (25th / 50th / 75th percentiles of department-level rates).
- Tables for which bootstrap CIs are computed gain `q1_ci_lower` / `q1_ci_upper` / `q2_ci_lower` / `q2_ci_upper` / `q3_ci_lower` / `q3_ci_upper`.
- `secondary_bsi_rate_table` gains `followup_q1` / `followup_q2` / `followup_q3`, the quartiles of the departments' shares of infections followed up, over the departments with at least 5 infections of the type, `NA` with fewer than 5 such departments.
- `infectious_agent_detection_rate_per_inf_type_table` also gains the quartiles of the departments' shares tested, `tested_q1` / `tested_q2` / `tested_q3`, with their bootstrap intervals (`tested_q1_ci_lower` and so on). Each share's quartiles take the departments whose denominator of the share is above 0, and are `NA` with fewer than 5 such departments or where their median denominator does not exceed `round(100 / pooled)`.
- `metadata$dataset_options$department_filter` is the marker `"applied"` (or `NULL` for an empty filter), never the department codes; the emitted copy is asserted to name no department. `validationSummary` and `reconciliationSummary` are carried as in §4.2.

**Invariants** (in addition to §4.2):

- Computed via `calculate_reference_data()`. Requires `country_key` to be present in metadata (warns otherwise).
- Quartiles are set to `NA` when `n_departments < 5`, or when `round(100 / pooled) >= median_patient_days` (small-cell-exposure protection).
- Inheritance contract: any generic that dispatches on `neoipcr_rep_ds` also applies to `neoipcr_ref_ds`. Only quartile-specific code paths need `inherits(x, "neoipcr_ref_ds")` checks.

**Predicates**: `is_neoipcr_ref_ds()`, `check_neoipcr_ref_ds()`, `check_neoipcr_ds_or_ref_ds()` — in [R/types-check.R](R/types-check.R).

---

### §4.4. `neoipcr_tbl_sr_ref` → post-1.2 `neoipcr_surgery_rate_table_ref` (candidate — see §3.1)

**Constructor sites**: [R/calc-tables.R:521](R/calc-tables.R#L521) and [R/calc-tables.R:604](R/calc-tables.R#L604) — two `add_class()` calls in `get_ref_surgery_rate_table()`.

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

The four cryptic-slug sub-classes below are all pending rename under task 1.2. Entries here preserve construction sites for traceability; post-rename names are the **candidates** listed in task 1.2 (scheme is not yet pinned).

| Current slug | Post-1.2 candidate | Parent shape | Construction site | Represents |
|---|---|---|---|---|
| `neoipcr_iaf` | `neoipcr_agent_finding` | tibble slot on `neoipcr_ds` | [R/import-dhis2.R:231](R/import-dhis2.R#L231) | Per-event pathogen identifications + resistance markers. |
| `neoipcr_sbd` | `neoipcr_substance_day` | tibble slot on `neoipcr_ds` | [R/import-dhis2.R:230](R/import-dhis2.R#L230) | Antibiotic-substance-day exposures per event/enrollment. |
| `neoipcr_tbl_udr` | `neoipcr_usage_density_rate_table` | result tibble | [R/calc-tables.R:211](R/calc-tables.R#L211) | Usage-density rate table (pooled rates per therapeutic category). |
| `neoipcr_tbl_udr_ref` | `neoipcr_usage_density_rate_table_ref` | result tibble | [R/calc-tables.R:287](R/calc-tables.R#L287) | Reference variant with quartile statistics. |

All four **depend on task 1.2**. See §3.2 for additional classes (`neoipcr_bnch_ds`, `neoipcr_tbl_rtr(_ref)`, `neoipcr_tbl_sec_bsi(_ref)`) that surfaced during this audit and should be added to task 1.2's rename table.

---

## §5. Report-helper promotion list

42 helpers identified. All Phase-2-sketch candidates in the task file confirmed; additions and refinements below. Total ~1200 LOC to promote; 8 new `R/*.R` files + extensions to existing `R/scales.R` and `R/dhis2-metadata.R`.

### §5.1. Promotion table

| # | Helper | Current location(s) | LOC | Target `R/*.R` | Proposed signature | Exp? | Deps | Blocking |
|---|--------|---------------------|-----|----------------|---------------------|------|------|----------|
| 1–4 | `logInfo`, `logVerbose`, `logDebug`, `logWarn` | [common/logging.R:147–161](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/logging.R#L147-L161) | 3 each | — | `log<Level>(..., namespace)` → `logger::log_*()` | — | logger | **Superseded.** They left the two Generate scripts for the shared `common/logging.R` and are now thin wrappers over the `logger` package, which already supplies the level, namespace and threshold semantics a promotion would have had to build; `logError` joined the set with that move. neoipcr ships `R/log.R` with the exported `neoipcr_log_config()`, and both sides map `quiet`/`normal`/`verbose`/`debug` onto logger's `WARN`/`INFO`/`DEBUG`/`TRACE` from `NEOIPC_LOG_LEVEL`, so the verbosity-state caveat this row raised is discharged at both ends. |
| 5 | `get_validation_exceptions` | [common/helpers.R:294–303](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L294-L303) | 10 | `R/validation-exceptions.R` — landed as `read_validation_exceptions()`; the report helper still reads the CSV itself, and its move to delegating (keeping only the absent-file → `FALSE` semantic) is the Surveillance-Toolkit side of this row | `read_validation_exceptions(path) → tibble` | Y | readr (already) | — |
| 6 | `parse_locales` | [common/helpers.R:27–76](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L27-L76) | 50 | `R/strings.R` | `parse_locales(x) → list(language, territory, codeset, modifier)` | Y | base | Foundation of cascade. |
| 7 | `get_string_resources` | [common/helpers.R:105–180](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L105-L180) | 76 | `R/strings.R` | `get_string_resources(localeObj) → list` | Y | yaml | Central to all report localization; feeds the data-label layer per D-H. |
| 8 | `get_localised_path` | [common/helpers.R:255–269](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L255-L269) | 15 | `R/strings.R` | `get_localised_path(file_name, language, territory) → character` | Y | base | — |
| 9 | `include_localised` | [common/helpers.R:271–282](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L271-L282) | 12 | `R/strings.R` | `include_localised(file_name)` | Y | readr, knitr | Knitr-specific but worth exporting for extension. |
| 10 | `get_localised_world_bank_class_names` | [common/helpers.R:284–292](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L284-L292) | 9 | `R/format.R` | `get_localised_world_bank_class_names(x) → character` | Y | purrr | — |
| 11 | `format_integer` | [common/helpers.R:371–372](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L371-L372) | 2 | `R/format.R` | `format_integer(x, big_mark)` | Y | base | `sR` → explicit arg (see §5.3 Open design issues). |
| 12 | `format_countries` | [common/helpers.R:378–422](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L378-L422) | 45 | `R/format.R` | `format_countries(countries, sR)` | Y | dplyr, rlang | Depends on sR accessor pattern (§5.3). |
| 13 | `format_range_filter` | [common/helpers.R:430–440](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L430-L440) | 11 | `R/format.R` | `format_range_filter(from, to, unit, all_label)` | Y | base | — |
| 14 | `format_dataset_resources` | [common/helpers.R:447–541](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L447-L541) | 95 | `R/format.R` | `format_dataset_resources(metadata, counts, sR) → list` | Y | lubridate, dplyr | Largest formatter; structured locale-aware output. |
| 15 | `parse_args` | [common/parse-args.R:20–77](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/parse-args.R#L20-L77) | 58 | `R/cli.R` | `parse_args(args, long_map, short_map) → list` | Y | base | Python-free — do NOT introduce `argparse`. |
| 16–20 | `as_null`, `as_bool`, `as_date_or_null`, `as_number_or_null`, `as_vector_or_null` | [common/parse-args.R:87–147](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/parse-args.R#L87-L147) | 5–10 each | `R/cli.R` | coercers | Y | base, lubridate | — |
| 21 | `get_connection_options` | [common/helpers.R:313–327](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L313-L327) | 15 | `R/dhis2-options.R` | `get_connection_options(...)` thin wrapper with report defaults | N | neoipcr itself | Extension point for external builders with their own defaults. |
| 22 | `get_dataset_options` | [common/helpers.R:329–365](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L329-L365) | 37 | `R/dhis2-options.R` | `get_dataset_options(...)` with report defaults | N | neoipcr itself | Pairs with #21. |
| 23 | `no_data_table` | [common/helpers.R:543–556](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common/helpers.R#L543-L556) | 14 | `R/gt.R` | `no_data_table(...)` | Y | base | Pairs with GT layer. |
| 24 | `compute_col_widths` | [Reference-Report/_setup.qmd:154–183](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Reference-Report/_setup.qmd#L154-L183), [Partner-Report/_setup.qmd:1581–1623](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1581-L1623) — **duplicated** | 30 / 43 | `R/gt.R` | `compute_col_widths(tbl_data, stub_min, has_reference, base_font_size, bare_w, n_w) → list` | Y | base | Unify the two variants. Partner version slightly larger. |
| 25–28 | `labels_bw`, `breaks_bw`, `labels_wk`, `breaks_wk` | [Reference-Report/_setup.qmd:21–48](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Reference-Report/_setup.qmd#L21-L48) | 6–9 each | `R/scales.R` (extend existing) | scale helpers for ggplot axes | Y | base | Coordinate with `feature-bw-ga-stratification.md` task. |
| 29 | `detect_mixed_wb_classes` | [Reference-Report/_setup.qmd:54–89](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Reference-Report/_setup.qmd#L54-L89) | 36 | `R/dhis2-metadata.R` | companion to #30 | Y | dplyr | Unify with #30 (see A3 Open design issues §5.3). |
| 30 | `detect_wb_class_mismatch` | [Partner-Report/_setup.qmd:1395–1454](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1395-L1454) | 60 | `R/dhis2-metadata.R` | unified helper | Y | dplyr | Unify with #29. |
| 31 | `extract_counts` | [Partner-Report/_setup.qmd:1802–1829](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1802-L1829) | 28 | `R/calc-api.R` (alongside `get_benchmark_data`) | per task file sketch | Y | dplyr | Stays in `calc-api.R`; no new `R/benchmark.R` needed. |
| 32 | `coarse_state` | [Partner-Report/_setup.qmd:50–58](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L50-L58) | 9 | `R/metric-state.R` | `coarse_state(state) → character` | Y | dplyr | Foundation of outlier pipeline. |
| 33 | `classify_metrics` | [Partner-Report/_setup.qmd:870–967](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L870-L967) | 98 | `R/metric-state.R` | `classify_metrics(tbl, id_col, combine_cols) → ordered factor` | Y | dplyr, rlang, stats | Core outlier-detection engine. |
| 34 | `has_outliers` | [Partner-Report/_setup.qmd:977–980](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L977-L980) | 4 | `R/metric-state.R` | `has_outliers(state_map) → logical` | Y | base | — |
| 35–36 | `get_outliers_below`, `get_outliers_above` | [Partner-Report/_setup.qmd:983–989](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L983-L989) | 3 each | `R/metric-state.R` | `get_outliers_<dir>(state_map) → character` | Y | base | — |
| 37 | `get_metrics_in_state` | [Partner-Report/_setup.qmd:972–974](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L972-L974) | 3 | `R/metric-state.R` | `get_metrics_in_state(state_map, states) → character` | Y | base | — |
| 38 | `classify_concordance` | [Partner-Report/_setup.qmd:67–87](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L67-L87) | 21 | `R/interpretation.R` | paired-metric classification | Y | dplyr | — |
| 39 | `compose_group_text` | [Partner-Report/_setup.qmd:995–1066](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L995-L1066) | 72 | `R/interpretation.R` | engine | Y | dplyr, purrr, rlang | — |
| 40 | `group_outliers` | [Partner-Report/_setup.qmd:1071–1104](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1071-L1104) | 34 | `R/interpretation.R` | orchestrator | Y | dplyr | — |
| 41 | `format_outlier_text` | [Partner-Report/_setup.qmd:1235–1317](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1235-L1317) | 83 | `R/interpretation.R` | rendering | Y | knitr | — |
| 42 | `render_outlier_section` | [Partner-Report/_setup.qmd:1222–1230](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1222-L1230) | 9 | `R/interpretation.R` | — | Y | base | — |
| 43 | `localize_metric_name` | [Partner-Report/_setup.qmd:1110–1219](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1110-L1219) | 110 | `R/interpretation.R` | lookup cascade for composite IDs | Y | rlang, tools | **Depends on D-H** — localization architecture governs the cascade shape. |
| 44 | `select_clause` | [Partner-Report/_setup.qmd:91–118](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L91-L118) | 28 | `R/interpretation.R` | internal | N | glue | Internal to engine. |
| 45 | `select_action` | [Partner-Report/_setup.qmd:121–133](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L121-L133) | 13 | `R/interpretation.R` | internal | N | base | — |
| 46 | `compose_interpretation` | [Partner-Report/_setup.qmd:137–175](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L137-L175) | 39 | `R/interpretation.R` | internal composer | N | glue | — |

### §5.2. Helpers rejected for promotion (stay in reports)

- **Per-table `interpret_*()` narrative callbacks** (~25 functions in Partner-Report, e.g. `interpret_antibiotic_rates`, `interpret_agent_staphylococci`, `interpret_ventilatory_support`) — these encode report-specific narrative copy and metric-pairing configuration, not reusable logic. The engine that runs them (#38–#46) promotes; the copy does not.
- **Per-table `metric_classify_args`** ([Partner-Report/_setup.qmd:1777–1783](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1777-L1783)) — table-specific configuration consumed by `classify_metrics()`; stays in the report as caller data.
- **`interpret_distribution()`** ([Partner-Report/_setup.qmd:1320–1388](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L1320-L1388)) — figure-specific; report copy.
- **CI configuration variables** (`ci_pattern_inline`, `ci_pattern_wrap`, `include_*_ci`, `wrap_ci_default`) — per-report display policy; parameterised into the promoted GT layer via §8.
- **Section visibility flags** (`show_section_surgical` etc.) — Quarto YAML param handling.
- **`.show()` / `.end()`** — trivial Quarto formatting.

### §5.3. Open design issues (flagged for Phase 2)

- **`sR` accessor pattern.** Many formatters (`format_integer`, `format_countries`, `labels_bw`, `labels_wk`, `format_dataset_resources`) depend on a global `sR` (string-resources) object in each report. Promoting them requires either (i) pass `sR` as an explicit argument everywhere, or (ii) a module-internal accessor that resolves from the locale chain. Recommend (i) for explicitness and testability. Phase 2 decision.
- **`detect_mixed_wb_classes` vs. `detect_wb_class_mismatch` unification.** Different arg shapes, overlapping semantics. Recommend a single `detect_wb_class_mismatch(own_countries = NULL, ref_countries, include_wb_class)` that reduces to `detect_mixed_wb_classes` when `own_countries = NULL`. Phase 2 decision.
- **YAML cascade ownership.** Should neoipcr own the *entire* YAML resource cascade (move `content/` folders from Surveillance-Toolkit), or only the code that traverses it? Recommendation: code moves in Phase 2, the YAML files themselves stay in the reports (because they encode report-specific narrative that belongs to the report, not the package). But the *contract* of the cascade (path shape, precedence order) is owned by neoipcr.
- **Interpretation engine — which helpers are exported?** `classify_concordance`, `compose_group_text`, `group_outliers`, `format_outlier_text`, `render_outlier_section`, `localize_metric_name` — all exported so report authors can call them. `select_clause`, `select_action`, `compose_interpretation` — stay internal.

### §5.4. Missing-piece extractions (promote AND extract)

- **Event-type vocabulary** inlined at [Validation-Report/_setup.qmd:135–137](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Validation-Report/_setup.qmd#L135-L137) as `factor(..., levels = c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))`. Extract to a utility function `event_types()` in `R/dhis2-events.R` (per CLAUDE.md's R/ file doctrine — DHIS2-specific utility) AND promote the factor labels to package data (§7 row 5, `event_type_map`).
- **Metric-state levels** at [Partner-Report/_setup.qmd:863–866](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Partner-Report/_setup.qmd#L863-L866) — a constant vector `metric_state_levels`. Promote as a package-level constant accessible via `get_metric_state_levels()` in `R/metric-state.R`.

### §5.5. Report-to-helper crossref (promotion impact)

| Helper group | Partner | Reference | Validation | Certificate | Patient-Data |
|---|:---:|:---:|:---:|:---:|:---:|
| `parse_locales`, `get_string_resources`, `get_localised_path` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `include_localised`, `get_localised_world_bank_class_names` | ✓ | ✓ | — | — | — |
| `format_integer`, `format_range_filter`, `format_dataset_resources` | ✓ | ✓ | — | — | — |
| `format_countries` | ✓ | — | — | — | — |
| `get_validation_exceptions` | ✓ | ✓ | ✓ | — | — |
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

## §6. Localization Architecture + Resolution Chain

> **Section status.** §6.1 through §6.3 describe the current state: the classified `gettext()` call sites, the locale flow, and the `display*` inventory. §6.4 through §6.7 propose the messages-versus-data architecture and its migration, governed by D-H (§10.8). §6.8 describes the message catalogue as it is: what is translated, how a message is written, and how the catalogue is built and checked.

### §6.1. Current-State Summary

**Scope of the problem is narrower than feared.** Every `gettext()`, `gettextf()`, and `ngettext()` call in this package's `R/` belongs to one of three groups:

1. the **condition messages**, every user-facing string of an `rlang::abort()` or `rlang::warn()` call and the text of `print.neoipcr_dhis2_conopt()`, spread across many files of `R/` and governed by §6.8;
2. the **two credential prompts** of [R/dhis2-connect.R](R/dhis2-connect.R), the password prompt of `get_password()` and the username prompt of `get_auth_data()`, which a person reads in an interactive session and which §6.8 governs as well;
3. **13 call sites that translate labels rather than messages**, classified and enumerated below.

| Classification | Count | Locations |
|---|---|---|
| M — Message (a condition, a prompt, or console text a person reads) | every condition message and both prompts | §6.1.2, §6.8 |
| C — Column header | 2 | [R/calc-api.R:893](R/calc-api.R#L893) and [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) |
| F — Factor-level label | 11 | All in [R/calc-procedure-categories.R:215–231](R/calc-procedure-categories.R#L215-L231) (`get_procedure_category_pretty()`; the eleventh, at :231, is the `NEOIPC-PERMANENT(dataset-format)` recode of the pre-`-ize` category spelling) |
| D — DHIS2-display passthrough (redundant wrap) | 0 | — |
| B — Bug (multi-arg `gettext` with silently dropped args) | 0 | — |

Two corrections to the A4 problem-statement in the plan file:

1. **No `B` (bug) cases exist.** `gettext()` translates each of its arguments and returns one element per string (`?gettext`), and both catalogue extractors collect every argument (§6.8.3), so the two multi-argument calls drop nothing: they translate the column headers that `pretty_names.neoipcr_tbl_sr_ref()` and `get_procedure_categories()` attach, which makes them the two `C` cases.
2. **No `D` (DHIS2-display passthrough) cases exist today.** The feared pattern — wrapping DHIS2 `displayName` output in `gettext` — does not appear. DHIS2 localization is already consumed correctly (see §6.3).

The real issue is confined to **two files and one function family**: the 11 factor-level label calls in `get_procedure_category_pretty()` and the two column-header calls.

#### §6.1.1. All F-Class and C-Class Call Sites (Individually Enumerated)

The 13 non-M sites each need distinct treatment in Phase 2, so they're individually enumerated here. M-class sites follow in §6.1.2.

| File:line | Call shape | Surrounding function | Class | Migration target |
|-----------|------------|----------------------|-------|------------------|
| [R/calc-api.R:893](R/calc-api.R#L893) | `gettext("Procedure category","N","Pooled","Q1","Q2","Q3")` | `pretty_names.neoipcr_tbl_sr_ref` | C | Rewrite as explicit per-column lookup against `procedure_category_labels` tibble (§7.1 row 2). The other headers — `"N"`, `"Pooled"`, `"Q1"`…`"Q3"` — either stay English or move to YAML cascade. |
| [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) | `gettext("Procedure code","Procedure category")` | `get_procedure_categories` | C | Keep on `gettext` OR move to YAML cascade (decide alongside §5 row 7 promotion). |
| [R/calc-procedure-categories.R:215](R/calc-procedure-categories.R#L215) | `gettext("Overall")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (§7.1 row 2, code `"overall"`). |
| [R/calc-procedure-categories.R:216](R/calc-procedure-categories.R#L216) | `gettext("Abdominal surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (code `"abdominal_surgery"`). |
| [R/calc-procedure-categories.R:217](R/calc-procedure-categories.R#L217) | `gettext("Neurosurgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:218](R/calc-procedure-categories.R#L218) | `gettext("Inguinal hernia surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:219](R/calc-procedure-categories.R#L219) | `gettext("Cardiac- / large vessel surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:220](R/calc-procedure-categories.R#L220) | `gettext("Lung- / pleural space- / thoracic surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:221](R/calc-procedure-categories.R#L221) | `gettext("Oesophageal surgery")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:222](R/calc-procedure-categories.R#L222) | `gettext("Other")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:223](R/calc-procedure-categories.R#L223) | `gettext("Not a surgical procedure")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>`. |
| [R/calc-procedure-categories.R:224](R/calc-procedure-categories.R#L224) | `gettext("Not yet categorized")` | `get_procedure_category_pretty` | F | Replace with read from `procedure_category_labels$label_<locale>` (code `"to_be_categorized"`). |
| [R/calc-procedure-categories.R:231](R/calc-procedure-categories.R#L231) | `gettext("Not yet categorized")` | `get_procedure_category_pretty` | F | Same read, for code `"to_be_categorised"` — the `NEOIPC-PERMANENT(dataset-format)` recode that renders datasets serialized under the pre-`-ize` spelling. The lookup must keep accepting this code; only the `gettext` wrapper goes. |

The 11 F-class calls at lines 215–231 also require removing the `Sys.getlocale("LC_MESSAGES")` fallback at [R/calc-procedure-categories.R:14](R/calc-procedure-categories.R#L14), in the `pretty` branch of `get_procedure_categories()` — the accessor pattern honours the resolution chain via the explicit `locale` argument instead.

#### §6.1.2. M-Class Call Sites

Every M-class site is a condition message, a credential prompt, or the print method's text, and all of them have the same migration action: **stay on `gettext()`, `gettextf()`, or `ngettext()`**. They are spread across many files of `R/`, so they are not enumerated here: §6.8.1 says which strings are translated and which stay English, and §6.8.2 how a message is written.

Validation findings and reconciliation counts carry no message. `validate()` returns data — the rule id, the keys of the record a finding refers to and a one-row `context` tibble of the values the rule compared — and the sentence a reader sees is composed and translated by the document that renders it (the Validation Report's string resources). The exception list is public API alongside: `read_validation_exceptions()` reads the user's CSV, `resolve_validation_exceptions()` maps it onto a dataset's keys, `validation_rule_ids()` lists the registry for consumers that keep a catalogue of rule descriptions, and `validation_rule_context_fields()` names the fields each rule records, so a consumer checks its templates' placeholders against the package rather than against a copied table. `validation_summary()` counts a result's flagged and exempted records, and `validation_exception_usage()` says how each list record fared. Likewise `reconciliationSummary` holds reconciliation ids and counts, the reports keep a label per reconciliation, and `reconciliation_ids()` is the list they check their labels against. The catalogue's entries from `R/validation.R` and `R/reconciliation.R` are the conditions their functions raise, such as a rule id `validate()` does not know.

### §6.2. Locale Parameter Flow (Current State)

- **Entry point**: `dhis2_dataset_options()` accepts `locale = NULL` (signature in [R/dhis2-options.R](R/dhis2-options.R)).
- **Only correct consumer**: [R/dhis2-metadata.R:8–12](R/dhis2-metadata.R#L8) — passes `locale` to the DHIS2 API when `dataset_options$translate && rlang::is_character(dataset_options$locale, n = 1)`.
- **Broken consumers**: every R-level `gettext` / `gettextf` call. They consult `Sys.getlocale("LC_MESSAGES")` and `Sys.getenv("LANGUAGE")` — not `opts$locale`. Concretely: [R/calc-procedure-categories.R:14](R/calc-procedure-categories.R#L14) falls back to `Sys.getlocale("LC_MESSAGES")` in the `pretty` branch of `get_procedure_categories()`, before it calls `get_procedure_category_pretty()`.
- **Result**: a caller who sets `dhis2_dataset_options(locale = "de")` gets German-translated DHIS2 metadata, but condition messages and procedure-category labels in the R session's language, German only where the session's `LANGUAGE` or `LC_MESSAGES` selects it.

### §6.3. `display*` Field Inventory (Current)

| Output surface | Tibble on `neoipcr_ds$metadata` | `display*` variant(s) used | Source |
|---|---|---|---|
| World Bank classes | `world_bank_class` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnitGroupSets → organisationUnitGroups` |
| Countries | `countries` | `displayName` (factor-pinned) | `organisationUnits` |
| Hospitals | `hospitals` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnits` (parent level) |
| Departments | `departments` | `displayName`, `displayShortName`, `displayDescription` | `organisationUnits` |
| Antimicrobials | `antimicrobials` | `displayName`, `displayFormName` | `options` |
| Data elements / TEAs | *(not surfaced to user-facing tibbles)* | all four variants, internally | `trackedEntityAttributes`, `programStageDataElements` |

All of these `display*` values are **preserved only when the relevant `include_*` gate is `"full"`**. Under `pseudo` / `no`, they are not written by the reader.

### §6.4. Ecosystem Precedents

| # | Package / source | Mechanism demonstrated | Concrete example | Applicability to neoipcr |
|---|------------------|------------------------|------------------|--------------------------|
| 1 | **[`countrycode`](https://github.com/vincentarelbundock/countrycode)** | Locale-as-column-selector on taxonomy tibbles. Data is English-canonical; labels live in separate `country.name.en`, `country.name.de`, `country.name.fr` columns of the `codelist` tibble. Accessor takes a `destination` parameter and reads the appropriate column. | `codelist` (~600 rows) with ~30 locale-keyed columns, e.g. `country.name.en` / `country.name.de` / `country.name.fr`. No gettext wraps around row values. | Directly applicable to the 11 F-class calls. `get_procedure_category_pretty()` migrates to a `procedure_category_labels` tibble in `sysdata.Rda` with `label_en` / `label_de` columns, served by `get_procedure_category_labels(locale = NULL)`. neoipcr's own `get_pathogen_taxonomy()` already uses this idiom — we extend it. |
| 2 | **"Writing R Extensions", § Translations** (R-core / CRAN manual) | Establishes that `gettext` / `gettextf` are for messages shown to users (errors, warnings, diagnostics). Data-layer labels (column names, factor levels, taxonomy terms) are not in scope for PO catalogues — they're part of the package's data contract. | The manual enumerates the set of `gettext`-wrapped user-facing messages and explicitly does not place factor labels, taxonomy tables, or column headers in that set. | Validates the split. The condition messages stay on `gettext()` / `gettextf()` / `ngettext()` (§6.8); the 11 F-class calls migrate to locale-keyed columns per #1 above. |
| 3 | **[`scales`](https://scales.r-lib.org)** (tidyverse) | Separates formatter construction from locale consumption. `scales::label_date()`, `scales::label_number()`, `scales::label_currency()` take format strings and locale hints as **explicit parameters**; they do not consult `Sys.getlocale()` silently at definition time. | `scales::label_date(format = "%b %d", locale = "de")` — caller is responsible for passing the locale. | Informs the unified resolution chain (§6.7). Every neoipcr entry point that produces localized output accepts an explicit `locale` argument that wins over `opts$locale`, which wins over `Sys.getlocale("LC_MESSAGES")`, which wins over `"en"`. |
| 4 | **[`rlang`](https://rlang.r-lib.org)** + **[`cli`](https://cli.r-lib.org)** (tidyverse) | Class-tagged condition system: `rlang::abort()` / `cli::cli_abort()` / `cli::cli_warn()` attach a class to the condition; error handlers and tests dispatch on the class, not on the human-readable message. Translation of the message is **orthogonal** — it happens via gettext if you want it, but the class is what programs check. | `rlang::abort(class = "neoipcr_missing_token", message = gettext("…"))`. Tests assert on `class`, not on the translated string. | **Adopted.** neoipcr raises every condition with `rlang::abort()` or `rlang::warn()` and wraps each user-facing string in `gettext()`, `gettextf()`, or `ngettext()` (§6.8.2). Tests run in the `C` language, so a test that matches a class is unaffected by translation, and one that matches English text still passes (§6.8.4). |
| 5 | **[`potools`](https://cran.r-project.org/package=potools)** | Package-scoped message catalogues. `potools::create_catalog()` extracts every `gettext(...)` call from `R/` and writes a single `.pot`; translators add `.po` files. `potools` does not split catalogues by message class — the split happens **at the source level** before extraction. | `potools::create_catalog()` → `po/R-<pkg>.pot`. Data-layer labels (taxonomy tibbles in `sysdata.Rda`) are not scanned because they're not `gettext` calls. | Confirms D-G (single `R-neoipcr.pot`), which neoipcr builds with its own `tools/update_po.R` (§6.8.3). Once the 11 F-class calls migrate out of `gettext`, the catalogue holds the condition messages, the credential prompts, and the print method's text — no clutter from data labels, unless the `:45` call keeps its two column headers on `gettext` (§6.5 row C), which would add exactly those two. |

**Mainstream consensus.** Five sources converge on: (i) `gettext` is for user-facing messages only; (ii) data labels live in locale-keyed columns or equivalent structured data (not in PO catalogues); (iii) locale selection is explicit (parameter or resolver), never implicit from `Sys.getlocale()` alone; (iv) a single `.pot` per package is standard. neoipcr is already close on (i), (iii-partial), and (iv); the M/F split is what brings it into (ii).

**Class and translation together.** A class-tagged condition and a translated message are not alternatives (#4): neoipcr raises every condition with `rlang::abort()` or `rlang::warn()`, carrying a `class` where a caller needs one, and wraps each user-facing string for the catalogue (§6.8). `cli` is not among its dependencies.

### §6.5. Proposed Architecture (D-H Concrete Form)

**Principle.** Every user-facing string in `neoipcr` belongs to exactly one of two tracks:

- **Messages track.** The user-facing strings of errors and warnings, the credential prompts, and the console text of the print method, shown to a person at run time (§6.8.1; the `logger` messages stay English). Owned by gettext: `po/R-neoipcr.pot` and its `po/R-<lang>.po` files. Wrapped by `gettext()`, `gettextf()`, or `ngettext()` at the call site (§6.8.2). Translators work from the catalogue.
- **Data track.** Column headers, factor labels, taxonomy terms, and any string that is part of the *value* returned by a neoipcr function. Owned by locale-keyed columns in `sysdata.Rda` tibbles or in ingested YAML resource cascades. Accessed via accessor functions that honour the locale-resolution chain.

Nothing is in both tracks.

**Mechanism per current category (from §6.1):**

| Class | Count | Mechanism under new architecture |
|---|---|---|
| **M** — Message | every condition message | Stay on `gettext()` / `gettextf()` / `ngettext()`. No call-site change in Phase 2 (other than the `Sys.getlocale` cleanup in §6.7's locale chain). The catalogue is regenerated with `tools/update_po.R` (§6.8.3) after the F-class calls exit. |
| **C** — Column header | 2 | [R/calc-api.R:893](R/calc-api.R#L893) — the `pretty_names.neoipcr_tbl_sr_ref` six-argument `gettext` — rewritten as an explicit locale-aware mapping against the `procedure_category_labels` tibble. [R/calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45) — the two-argument `gettext("Procedure code", "Procedure category")` — kept on `gettext`, *or* moved to YAML cascade if those headers are report-visible (decide in Phase 2 when the string-resources promotion lands). New column headers use the YAML resource cascade (via `get_string_resources()` promoted in §5 row 7), not `gettext`. |
| **F** — Factor-level label | 11 | Migrate to package data. All 11 are procedure-category labels in [R/calc-procedure-categories.R:215–231](R/calc-procedure-categories.R#L215-L231). Target: the `procedure_category_labels` tibble proposed in §7 row 2, served by a new `get_procedure_category_labels(locale = NULL)`. Migration is the only non-trivial code change in Phase 2. |
| **D** — DHIS2-display passthrough | 0 | None exist today. Entry points already consume DHIS2's `display*` fields directly (see §6.3). Document this pattern in every public entry point's roxygen. |

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

**What this looks like to a translator.** After the migration, `R-neoipcr.pot` holds the condition messages, the credential prompts, and the print method's text (§6.8), all of them messages a person reads, plus the two column headers of `calc-procedure-categories.R:45` if that call stays on `gettext` rather than moving to the YAML cascade. The F- and C-class calls account for 17 of today's msgids, fewer than their 19 strings, because duplicates collapse to one entry: the recode at `calc-procedure-categories.R:231` repeats the label at `:224`, and `"Procedure category"` heads both `calc-api.R:893` and `calc-procedure-categories.R:45`. Procedure-category labels are translated in the YAML / CSV under Surveillance-Toolkit's metadata/ (out of scope for this note; §7 row 2 coordinates with A5b).

### §6.6. Migration Plan for Phase 2

| Call site | Classification | Action in Phase 2 |
|---|---|---|
| [R/calc-api.R:893](R/calc-api.R#L893) | C | Rewrite as an explicit tibble lookup against the new `procedure_category_labels`; remove the six-argument `gettext`. |
| `R/calc-procedure-categories.R:45` | C | Keep on `gettext` *or* move to YAML cascade (decide alongside string-resources promotion, §5 row 7). |
| [R/calc-procedure-categories.R:215–231](R/calc-procedure-categories.R#L215-L231) (11 calls) | F → data-layer | Replace with reads from `procedure_category_labels` tibble. `get_procedure_category_pretty()` becomes `get_procedure_category_labels(locale = NULL)`; locale-resolution chain applied; the `"to_be_categorised"` recode at :231 stays as a code the lookup accepts. Remove the `Sys.getlocale("LC_MESSAGES")` fallback at line 14 of `get_procedure_categories()`. |
| Condition messages and the print method (§6.1.2) | M | No action required. Stay as-is. Optionally document that `gettext` returns the English source string where the catalogue has no translation (`?gettext`) — useful for vignette writers. |

**Phase 2 sequencing.** The migration has exactly one non-trivial step: create `procedure_category_labels`, populate it, rewrite `get_procedure_category_pretty()` to read from it, delete the 11 `gettext` lines. Everything else is either "do nothing" (M-class) or a short rewrite of the two column-header calls (C-class). Estimate: half a day of work plus catalogue regeneration.

**Catalogue regeneration.** After the F-class migration, run `Rscript tools/update_po.R` (§6.8.3), which regenerates `po/R-neoipcr.pot`, merges it into `po/R-de.po` and `po/R-en.po`, and recompiles `inst/po/{de,en}/LC_MESSAGES/R-neoipcr.mo`; the five files are committed with the code. Part of Phase 2.

### §6.7. Unified Locale Resolution Chain

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

### §6.8. Message Catalogue

The catalogue is `po/R-neoipcr.pot`, merged into `po/R-de.po` and `po/R-en.po` and compiled into `inst/po/<lang>/LC_MESSAGES/R-neoipcr.mo` for each of the two languages. It holds 187 msgids, 7 of them plural entries: 17 are the F- and C-class strings of §6.1.1, whose fate §6.6 decides, 2 are the credential prompts, and 168 are condition messages and the print method's text. `msgfmt --statistics -o /dev/null po/R-neoipcr.pot` counts them, as untranslated messages.

#### §6.8.1. What Is Translated, and What Stays English

1. **Every user-facing string of a condition is translated**, headline and bullets alike, in every `rlang::abort()` and `rlang::warn()` call of `R/`. Every condition is raised with one of the two, never with `stop()`, `warning()`, or `stopifnot()`, which raise unclassed conditions: `abort()` names the calling function, renders a message vector as a headline with `i` / `x` bullets, and takes a `class` that a caller or a test matches without parsing text.
2. **The text of `print.neoipcr_dhis2_conopt()` and the two credential prompts are translated**, since a user reads them in the console. The values they show (the base URL, the name of the authentication method, the username) reach them as values and stay as they are.
3. **A developer-only assertion stays English**: a broken package invariant that no user can act on. Its literal stays unwrapped, so neither extractor sees it, and the call passes `.internal = TRUE` and nothing else to mark it. rlang then appends a footer, itself English, saying that the error is internal to neoipcr and asking the user to report it at the package's `BugReports` address with a reprex and the full backtrace; the argument states the same fact at the call site, where a comment could drift from the code. What decides is what a message asks of its reader: one that asks for a change to the package is developer-only even where a caller can reach it. `read_metadata_reponse()` fails on a response from a path it did not request, which nothing but a redirecting proxy produces; `require_cols()` and `assert_serializable_dataset_options()`, which checks the options a calculated dataset carries out of the package, can be reached from exported functions with an old or tampered dataset, but their bullets tell a developer what to fix. All three are developer-only.
4. **The `logger` messages stay English and untranslated.** Their readers are the operators and supporters who run and debug the reports and the reporting service, not the people who enter data; English keeps them searchable across deployments and comparable in a support request; and the package's logger formats its messages with `logger::formatter_glue` (`R/log.R`), which would interpolate any braces a translation introduced.
5. **The vendored rlang standalone files** (`R/import-standalone-obj-type.R`, `R/import-standalone-types-check.R`) keep their condition calls as rlang ships them.

#### §6.8.2. Writing a Translatable Message

The headline and every bullet go through `gettext()`, `gettextf()`, or `ngettext()`, and `class` and `call` are untouched, as in this sketch of an invented condition:

```r
rlang::abort(c(
  gettextf("Unknown trial code %s.", code),
  "i" = gettextf("Trial codes are the codes of the option set %s.", trials_option_set),
  "x" = sprintf(ngettext(n, "%d department references it.", "%d departments reference it."), n)),
  class = "neoipcr_unknown_trial")
```

1. **A literal at the call site.** Each string is the direct literal argument of a bare `gettext()`, of `gettextf()` as its format string, or of `ngettext()` as its singular and plural messages. A string assembled at run time, held in a variable, or nested in `paste0()` is never extracted, and one piped into the call (`"…" |> gettext()`) reaches only `tools::xgettext()` (§6.8.3, case 6). The translated result may be assigned to a variable before it reaches `abort()`.
2. **One sentence, one literal.** A translator cannot reorder fragments pasted around a value, while a `%s` can move. `gettextf()` goes where `sprintf()` would, and a count goes through `sprintf(ngettext(n, singular, plural), n)`, since a count changes the grammar. A sentence whose number follows a count it does not show (`"Missing columns: %s."` before a list) chooses with `if (n == 1) gettextf(singular) else gettextf(plural)` instead: `ngettext()` hands form 0 to 21, 31, and so on in languages such as Ukrainian, where only a shown number makes that form right, so the GNU gettext manual rules it out when neither message contains the number.
3. **No `domain` argument.** The domain is inferred (§6.8.4), and `domain = NA` makes the two extractors disagree (§6.8.3, case 2).
4. **Identifiers stay outside the literal** and reach it as values: codes, column names, URLs, and code identifiers go in through a placeholder (`trials_option_set` above), so a translator never sees one. A translated value is assigned to a variable before it goes into `gettextf()`, never nested in the call (§6.8.3, case 4).
5. **Bullet names stay outside `gettext()`**: `"i" = gettext("…")`.
6. **A developer-only assertion** keeps its literal unwrapped and passes `.internal = TRUE` (§6.8.1, item 3).

#### §6.8.3. The Catalogue Tool

`Rscript tools/update_po.R`, run from the package root, builds the catalogue in place of `tools::update_pkg_po()`, whose template writer opens a text-mode connection and so writes CRLF line endings on Windows. It needs GNU gettext's `msgmerge` and `msgfmt` on the `PATH`; Rtools provides both on Windows.

1. **What it collects.** From the parse data of each `R/` file, the direct literal arguments of every `gettext()` call except `domain`, and the format string of every `gettextf()` call, namespace-qualified calls included, each trimmed of leading and trailing blanks, tabs, and newlines, in the order of first appearance, file by file. It takes the plural entries from `tools::xngettext()`, which reads a bare `ngettext()` whose singular and plural messages are both literals, and writes them after the singular entries, without `#:` references.
2. **The comparison with `tools::xgettext()`.** It warns when its singular msgids differ, in content or order, from those of `tools::xgettext(asCall = FALSE)`, which reads `stop()`, `warning()`, `message()`, `packageStartupMessage()`, `gettext()`, and `gettextf()` by their bare names only, trims the same whitespace, and takes only `gettextf()`'s format string. The two still differ, and the warning fires, in six cases:
   1. a namespace-qualified call: `update_po.R` reads `base::gettext()`, `tools::xgettext()` does not (`gettext()` is a base function, so nothing asks for the prefix);
   2. `domain = NA` on `gettext()` or `gettextf()`, which suppresses extraction in `tools::xgettext()` only;
   3. a literal nested inside a call within the message: `gettext(paste0("a", x))` yields `"a"` in `tools::xgettext()` and nothing in `update_po.R`;
   4. a `gettext()` or `gettextf()` call inside a value argument of `gettextf()`, directly or within another call, which `update_po.R` extracts and `tools::xgettext()`, reading only the format string, does not;
   5. a literal passed straight to `stop()`, `warning()`, `message()`, or `packageStartupMessage()`, which only `tools::xgettext()` extracts;
   6. a literal piped into the call (`"x" |> gettext()`), which the parse data keeps outside the call, so that only `tools::xgettext()`, reading the parsed expression, sees it.

   A message written by the rules of §6.8.2 meets none of them, and a run on the package prints no warning.
3. **What it writes.** The template goes through a binary connection, so it has LF line endings on every platform. Its header carries `Project-Id-Version` from `DESCRIPTION`'s package name and version, `Report-Msgid-Bugs-To` from `BugReports`, and, only when plural entries exist, the placeholder `Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;`. For each `po/R-<lang>.po` (`po/R-en@quot.po` excepted) the tool then runs `msgmerge --update`; then `tools::checkPoFile()`, which compares each translation's format conversions with its msgid's and keeps a file it faults from being compiled; then `msgfmt -c --statistics` into `inst/po/<lang>/LC_MESSAGES/R-neoipcr.mo`. The compiled catalogues are tracked, since an installed package reads its translations from what `inst/po` installs: the template, the merged `.po` files, and the `.mo` files are committed with the code, and the working-tree column of `git ls-files --eol po/` shows the line endings.
4. **The `Plural-Forms` headers are maintained by hand.** `msgmerge` copies `POT-Creation-Date` and `Report-Msgid-Bugs-To` from the template into each `.po` file but never adds a `Plural-Forms` field that exists only there, so each language file carries its own: `po/R-de.po` and `po/R-en.po` both declare `nplurals=2; plural=(n != 1);`. A new language file needs the header for its language before `msgfmt` accepts its first translated plural entry.
5. **The template mode and the check.** `Rscript tools/update_po.R --template <file>` writes the template to `<file>` and touches nothing under `po/` or `inst/po`. The `po-template` job of the `R-CMD-check` workflow runs it into a temporary file and fails when the regenerated template's messages differ from the committed template's. GNU gettext's `msgcomm` reduces both files to their messages first: it drops the header, whose `POT-Creation-Date` changes on every run, and the `#:` references, which change whenever a line moves, and sorts what remains, keeping each msgid with its `msgid_plural`, which `msgcmp` does not compare. The job therefore fails on a message added, removed, or reworded, or turned singular or plural, and not on a moved line. It also fails where the two extractors differ (item 2), since the template mode raises an error where a catalogue run only warns.

#### §6.8.4. At Run Time

1. **The domain is inferred.** When `gettext()` or `ngettext()` is called from a function in the namespace of package *pkg*, the domain is `R-pkg` (`?gettext`), here `R-neoipcr`, and `gettextf()` called from such a function translates through the same domain.
2. **Lookups are trimmed.** `gettext()` and `gettextf()` look a message up without its leading and trailing whitespace (`trim = TRUE`), which is why the tool trims every singular msgid: a msgid kept with a trailing space would never be found.
3. **Tests cannot show a translation.** testthat's third edition, which the package declares, runs each `test_that()` block with the language set to `C`, so a test sees the English source text: a test that matches a message's English text keeps passing after the message is wrapped, and the `class` is what a test matches. A translation shows from an installed package or a `devtools::load_all()` session, since `pkgload::load_all()` binds the domain to a copy of the source tree's `inst/po`, in a session whose language selects it, such as one started with `LANGUAGE=de` or switched with `Sys.setLanguage("de")`.

---

## §7. Package-data promotion list

**Anchor pattern** (do not redesign). [data-raw/sysdata.R](data-raw/sysdata.R) already ingests three pathogen files from Surveillance-Toolkit via `raw.githubusercontent.com/.../refs/heads/main/metadata/common/infectious-agents/` and packs them via `usethis::use_data(..., internal = TRUE, compress = "xz")`. Every new sysdata candidate below follows the same URL shape, the same `internal = TRUE` default, and the `get_pathogen_taxonomy()` roxygen model for its accessor function.

### §7.1. Candidate table

| # | Candidate | Current inline location | Shape today | Target (package data) | Int/Ext | Accessor | Blocking |
|---|-----------|-------------------------|-------------|-----------------------|---------|----------|----------|
| 1 | Procedure-category map (ICHI → category code) | [R/calc-procedure-categories.R:69–210](R/calc-procedure-categories.R#L69) — `case_when` inside `get_procedure_category()` | Inline `case_when` over 9 factor levels (7 surgical categories, `not_surgery`, `to_be_categorized`) | Tibble `procedure_category_map` in `sysdata.Rda` with columns `ichi_code`, `category_code` | internal | `get_procedure_category()` (existing) | **Superseded by §13.9.2**, whose 8 procedure groups replace the 7 categories; the placement of the codes, which replaces this map, comes with the change that implements them. |
| 2 | Procedure-category pretty-name labels | [R/calc-procedure-categories.R:215–231](R/calc-procedure-categories.R#L215-L231) — 11 `gettext()` calls (A4a F-1…F-11) | 11 factor-level label calls routed through gettext | Tibble `procedure_category_labels` in `sysdata.Rda` with locale-keyed columns (`category_code`, `label_en`, `label_de`, …) — the `countrycode` pattern | internal | new: `get_procedure_category_labels(locale = NULL)` | **Depends on D-H.** The 11 factor-label calls are the primary motivation for D-H. |
| 3 | AWaRe categories (code → label) | [R/calc-denominators.R:218–224, 294–300](R/calc-denominators.R#L218-L224) — inline factor construction over `{"a", "w", "r"}` | Factor with levels `c("a", "w", "r")`; labels hardcoded English (Access / Watch / Reserve) | Tibble `aware_categories` in `sysdata.Rda`: `code`, `label_en`, `label_de` | internal | new: `get_aware_categories(locale = NULL)` | **Depends on D-H.** Codes come from DHIS2 option groups (read at import time); labels need the locale-column pattern. |
| 4 | BW/GA breakpoints | [R/scales.R:1–75](R/scales.R) — hardcoded arithmetic in `ga7`, `bw50`, `bw125`, `bw250`, `bw500` | Hardcoded `floor(...)`-arithmetic in five functions | Named-list `scales_parameters` in `sysdata.Rda` (one sublist per binning) | internal | new: `get_scales_parameters()` returning the list; five binning fns consume it | **None.** Pure numeric constants. Promotable immediately (Phase 2 or earlier). |
| 5 | Event-type vocabulary | [R/dhis2-metadata-reference.R:115–140](R/dhis2-metadata-reference.R#L115-L140) — two named lookup vectors resolved by `event_type_key_of()` into the event-type key (`adm`, `end`, `bsi`, `nec`, `hap`, `pro`, `ssi`) | 7-entry vector keyed by program-stage **code** (`NEOIPC_STG_*`), with a name-keyed vector behind it as a marked compatibility path for instances whose stages carry no code, plus inline factor levels | Tibble `event_type_map` in `sysdata.Rda`: `programStage`, `name`, `event_type_key`, `label_en`, `label_de` | internal | new: `get_event_type_map(locale = NULL)` | **Depends on D-H** *and* on a Surveillance-Toolkit source file (A5b). Also blocks (and is blocked by) the event-type promotion in A3 from [Validation-Report/_setup.qmd:135–137](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/Validation-Report/_setup.qmd#L135-L137). |
| 6 | Resistance markers | [R/pathogens.R](R/pathogens.R) + columns on `internal_pathogen_concepts` | Already package-data | *(no change)* — already ingested via `data-raw/sysdata.R` | internal | `get_pathogen_taxonomy()` (existing) | **Complete.** |
| 7 | ICHI code list / validity check | [R/ichi.R:1–69](R/ichi.R) — regex grammar assembled at package load | Compiled regex pattern | **Reject for promotion** — keep as procedural validator. | — | `is_valid_ichi_code()` (existing) | Full ICHI ontology bundling blocked by WHO licensing (see the ICHI classification-bundling task). Current syntax-only check is intentional. |

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
| 1 | Procedure-category map (ICHI → category) | *(file does not yet exist)* — needs new `metadata/common/procedure-categories/NeoIPC-Procedure-Categories.csv` with columns `ichi_code`, `category_code` | **Superseded by §13.9.2**, whose 8 procedure groups replace the 7 categories; the placement of the codes, which replaces this map, comes with the change that implements them. |
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
- **Infectious-agent files stay held for the CSV-to-YAML migration** — no rows above touch them. `internal_pathogen_concepts` / `internal_pathogen_synonyms` / `internal_pathogen_list` in [data-raw/sysdata.R](data-raw/sysdata.R) stay exactly as they are until that task reshapes the upstream YAML.

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

(Phase 2 fills in the version floor by auditing the Surveillance-Toolkit's `renv.lock`.)

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

**One rename proposal withdrawn.** D-C originally proposed `get_benchmark_data()` → `calculate_benchmark_data()` based on the task file's claim that the function "actually computes." Reading the source ([R/calc-api.R:326–823](R/calc-api.R#L326-L823)) shows it combines pre-computed datasets with one small CI fix-up — not a calculation primitive. See §10.3. The task file claim is corrected in the same commit as this note.

§13.8 adds one: `get_organism_resistance_rate_table()` becomes `get_infectious_agent_resistance_rate_table()`, and its slot `organism_resistance_rate_table` becomes `infectious_agent_resistance_rate_table`. No other renames proposed for the current exports (§3). Long table-function names stay (D-E — use `@family` grouping instead of shortening).

### §9.2. Class renames

All class renames are owned by task 1.2, the class-slug rename; §9 does not duplicate them. §3.2 lists three classes found by this audit that should be added to task 1.2's rename table:

- `neoipcr_bnch_ds` → `neoipcr_benchmark_ds`
- `neoipcr_tbl_rtr` / `_ref` → `neoipcr_resistance_test_rate_table` / `_ref`
- `neoipcr_tbl_sec_bsi` / `_ref` → `neoipcr_secondary_bsi_rate_table` / `_ref`

### §9.3. Naming rules for promoted helpers

Rules followed in §5 when assigning names to helpers moving from Surveillance-Toolkit reports into `neoipcr`:

1. **Snake case, lowercase.** Matches tidyverse convention and the existing package.
2. **No abbreviations of domain terms** in exported function names. `ci` ok (standard, as in `neoipc_poisson_ci`); `bw` / `ga` ok (well-known medical abbreviations); avoid ad-hoc package-internal abbreviations.
3. **Verb-noun order for actions, noun-verb for state queries.** `format_integer()`, `classify_metrics()`, `compose_interpretation()` (actions); `has_outliers()`, `get_outliers_below()` (state queries). The existing `get_*_table()` family is grandfathered.
4. **`get_*` for retrieval from data or state; `calculate_*` for computation; `format_*` for pure presentation transforms.** This is the rule that forces D-C (`get_benchmark_data` → `calculate_benchmark_data`).
5. **`neoipcr_` prefix for domain-specific statistics functions only** (post-D-D: `neoipcr_poisson_ci`, `neoipcr_wilson_ci`, `neoipcr_bootstrap_quantile_ci`). Formatting helpers (`format_integer`) and CLI helpers (`parse_args`) do NOT carry the prefix — they're general-purpose and callers already say `neoipcr::format_integer()`. Package configuration and introspection are the standing exception, and deliberately so: `neoipcr_log_config()` configures *this package's* logger namespace rather than logging anything, and `neoipcr_supported_versions()` reports what *this package* supports, so in both the prefix names what the function acts on or speaks for. The report layer's own level helpers are not a promotion candidate — see §5.1.
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
| C | S3 class slug | `neoipcr_tbl_<airport-code>` (snake_case + abbreviation) — pending task 1.2 | neoipcr (slug rename owned by 1.2) |
| S | `neoipcr_rep_ds` / `neoipcr_ref_ds` slot | `<...>_table` (snake_case) | neoipcr |
| R | Conceptual rate name (the metric the table is about) | varies | shared concept |
| Q | Quarto YAML param in report `*.qmd` | `include<...>Table` (camelCase) | Surveillance-Toolkit reports |
| P | `EnableElements` / `DisableElements` switch token in the report-wrapper scripts | `<...>Rates` (PascalCase) | Surveillance-Toolkit scripts |
| Z | `.qmd` filename under `reports/<Report>/{tables,figures}/` | `_tbl-<...>.qmd` / `_fig-<...>.qmd` (kebab-case) | Surveillance-Toolkit reports |
| H | Heading key in `sR$headings$<key>` (English source in `_sR.yaml` / `common.yaml`) | snake_case | Surveillance-Toolkit reports |
| D | Display heading string (per-locale, in `_sR.yaml` / `common.yaml` / glossary.yaml) | English source per AMA Manual; translations via po4a/Weblate | Surveillance-Toolkit reports + Weblate translators |

The casing-per-layer convention is documented in [the Surveillance-Toolkit's report-wrapper rules](https://github.com/NeoIPC/Surveillance-Toolkit/blob/main/.claude/rules/wrappers.md): "PowerShell `PascalCase` → Quarto document `camelCase` → R `snake_case`, mapped once at each boundary." §9.5 extends this to: filesystem paths use kebab-case (Z), heading keys use snake_case (H), display strings follow the AMA Manual (D, with translations via po4a/Weblate).

One surface remains out of §9.5's scope:
- **Per-factor rate column names within a table** (e.g. `cvc_rate`, `pvc_rate` inside `usage_density_rate_table`). These name the *factors* the table groups by, not the *table*. They follow their own per-domain conventions (device codes, infection-type codes, etc.) and are out of scope for the per-table master-name decision.

Nested snippet filenames (`_tbl-intro-<...>.Rmd`, `_methods-<...>.Rmd` per [reports/<Report>/content/](https://github.com/NeoIPC/Surveillance-Toolkit/tree/main/reports/Partner-Report/content)) are derived mechanically from the .qmd filename's kebab-case stem and follow surface Z's convention; they are not separately enumerated.

#### §9.5.2. Current state — full inventory

Confirmed end-to-end via grep across this package's `R/*.R` and, in the Surveillance-Toolkit, `reports/Partner-Report/Partner-Report.qmd` + `tables/_tbl-*.qmd` + `figures/_fig-*.qmd`, `Reference-Report/Reference-Report.qmd`, `Partner-Report/content/_sR.yaml`, `reports/common.yaml`, and the Partner Report wrapper script.

**Code-identifier surfaces (F / C / S / Q / P / Z / H):**

| # | F (function) | S (slot) | Q (Quarto param) | P (PS switch) | Z (.qmd file) | H (heading key) |
|---|---|---|---|---|---|---|
| 1 | `get_usage_density_rate_table` | `usage_density_rate_table` | `includeRiskDensityRateTable` | `RiskDensityRates` | `_tbl-risk-density-rates.qmd` | `presence_of_risk_and_protective_factors` |
| 2 | `get_antibiotic_utilization_table` | `antibiotic_utilization_table` | `includeAntibioticUtilisationTable` | `AntibioticUtilisationRates` | `_tbl-antibiotic-utilization.qmd` | `antibiotic_utilization` |
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

C (class slug) column omitted from this table for width — see §3 / §4.5; all `neoipcr_tbl_*` slugs follow the airport-code abbreviation pattern pending task 1.2. The `_ref` variants append `_ref` on F/C/S only (Q/P/Z/H/D unaffected).

**Display heading strings (D — English source per AMA Manual; translated via po4a/Weblate):**

| # | Display heading (English) | Cascade level |
|---|---|---|
| 1 | "Presence of Risk and Protective Factors in Your Department" (Partner-Report) / "Presence of Risk and Protective Factors" (Reference-Report) | `Partner-Report/content/_sR.yaml` and `Reference-Report/content/_sR.yaml` (per-report variants) |
| 2 | "Antibiotic Utilization" | [common.yaml:47](https://github.com/NeoIPC/Surveillance-Toolkit/blob/5e74901ae1919816f349608f38bb904db84c347b/reports/common.yaml#L47) |
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
| 2 | antibiotic utilization rate | `antibiotic_utilization_rate` | (a) align — H/D = "Antibiotic Utilization Rate" | Add `_rate` suffix everywhere for consistency. The spelling is settled: the neoipcr function and slot, the `.qmd` filename, the heading key and the display string are `utilization`; what remains is renaming the Quarto param `includeAntibioticUtilisationTable` and the PowerShell switch `AntibioticUtilisationRates` to match. |
| 3 | surgery rate | `surgery_rate` | (b) keep section `surgical_procedures_by_category` as the broader section title; new sub-H = `surgery_rates` if needed | "Surgery" is shorter and clinically standard. The H "Surgical Procedures by Category" is descriptive of how the table is structured, not the metric — keep as section copy. |
| 3b | (asymmetric ref function) | resolve in Phase 2 | — | The two-function asymmetry (`get_surgery_rate_table` + `get_ref_surgery_rate_table` instead of one S3 dispatch) is a Phase 2 design fix, not a §9.5 naming fix. |
| 4 | incidence density rate | `incidence_density_rate` | (b) keep section `severe_infections_and_nec`; new sub-H per table | Already aligned on F/S/Q/P. Section H is a broader epi grouping ("severe infections and NEC") that legitimately groups tables — keep. |
| 5 | device-associated incidence density rate | `device_associated_incidence_density_rate` | (a) align — H/D = "Device-Associated Incidence Density Rates" | Drop `dev_ass` slug; spell out. Long but unambiguous; rely on `@family` grouping (D-E). |
| 6 | infectious-agent detection rate per infection type | `infectious_agent_detection_rate_per_infection_type` | (b) keep section `infectious_agents_in_nosocomial_infections`; sub-H = `infectious_agent_detection_per_infection_type` | Identifiers keep "infectious agent" in full, as the current functions do and row 9 does, long names staying (D-E); the H/D side keeps "Infectious Agents" as section copy because it reads more naturally to clinicians. |
| 7 | infectious-agent detection rate per agent | `infectious_agent_detection_rate_per_agent` | (a) align — sub-H = `infectious_agent_detection_per_agent` under same parent as row 6 | Distinguishes from row 6 by `_per_agent`. |
| 8 | antibiotic-resistance infection rate | `antibiotic_resistance_infection_rate` (alt: `amr_infection_rate` if AMR is the preferred professional term) | (a) align — H/D = "Antibiotic Resistance Infection Rates" | Spell out `abr` — package-internal abbreviation. **PI: AbR vs AMR vs `antibiotic_resistance` vs `antimicrobial_resistance` — which term does the surveillance literature use?** |
| 9 | organism resistance rate | `infectious_agent_resistance_rate` | (a) align — H/D = "Infectious-Agent Resistance Rates" | §9.1 renames F and S to it (§13.8); C, Q, P, Z, and H follow, since what causes an infection may be no organism. |
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

The class-slug column (C) above pre-empts task 1.2, the class-slug rename. After D-I lands, task 1.2's rename table for result-table classes should be **replaced** by the §9.5 master names rather than rederived independently. Concretely, task 1.2's result-table candidates (`neoipcr_tbl_udr` → `neoipcr_usage_density_rate_table`, etc. — see task 1.2 §3) should be updated to follow §9.5 row #1's master `usage_density_rate` (under whatever PI ratifies in D-I), and the same for every other row. This avoids two independent rename schemes drifting apart.

The dataset-slot classes (`neoipcr_pat`, `neoipcr_enr`, `neoipcr_evt`, etc. in task 1.2 §1) are not affected by §9.5 — they're not result tables.

#### §9.5.7. Cross-repo execution

Phase 2 of task 1.1 promotes helpers but does not rename existing exports. **Phase 3** of task 1.1 applies all §9.5 renames, in lockstep across nine surfaces:

1. **neoipcr R/** (F / C / S / R) — function names, S3 classes, slot names, internal rate-name references in roxygen and column-comment text.
2. **Surveillance-Toolkit reports — Quarto YAML param defaults (Q)** — every report's `_quarto*.yml` and `_main.qmd` `params:` block; every `params$includeXxx` reference in `_setup.qmd` and `_content.qmd` files.
3. **Surveillance-Toolkit reports — `.qmd` filenames (Z)** — `git mv` each `_tbl-*.qmd` and `_fig-*.qmd` under `reports/<Report>/{tables,figures}/`. Update the `{{< include ... >}}` calls in `_content.qmd` (and any other includer). Rename nested snippet files (`_tbl-intro-*.Rmd`, `_methods-*.Rmd`) and their `include_localised(...)` callers.
4. **Surveillance-Toolkit reports — heading keys (H)** — rename keys in `reports/common.yaml` and per-report `content/_sR.yaml`. Update `sR$headings$<key>` references in every `.qmd` file.
5. **Surveillance-Toolkit reports — display strings (D)** — change the English source values in `common.yaml` / `content/_sR.yaml` per AMA Manual. **Run [`scripts/Invoke-Localization.ps1 -Update`](https://github.com/NeoIPC/Surveillance-Toolkit/blob/main/scripts/Invoke-Localization.ps1)** to regenerate `.pot` files; the existing translations get marked fuzzy in `.po` files; translators (or the PI) re-confirm via Weblate. Per Surveillance-Toolkit's [po4a guardrails](https://github.com/NeoIPC/Surveillance-Toolkit/blob/main/.claude/rules/localization.md), do not manually edit generated `common.<lang>.yaml` / `content.<lang>/_sR.yaml` files.
6. **Surveillance-Toolkit scripts** (P) — `EnableElements`/`DisableElements` mapping tables in every report-wrapper script. Per Surveillance-Toolkit's PowerShell-alignment guardrail: the mapping tables must stay in sync across all wrapper scripts.
7. **.NET reporting service** (the `NeoIPC-Reporting` repository) — verify whether it consumes the PS switch tokens or the Quarto params or both; update accordingly.
8. **Documentation** — README.md, vignettes (Phase 5), the Key R Files index in `CLAUDE.md` and its detail in `.claude/rules/r-code.md`, NEWS.md entry per-rename.

**Cross-repo change order**, as NeoIPC's workspace-level agent instructions define it: protocol → DHIS2 config → neoipcr → reports → web. Phase 3's §9.5 renames all sit in the neoipcr → reports portion. The lockstep nature means the workspace-level commit references all touched repos: e.g. "Apply §9.5 cross-surface rename for usage-density-rate across neoipcr R/, Surveillance-Toolkit reports + scripts, NeoIPC-Reporting." Workflow per row of §9.5.4:

1. Update neoipcr R/ + tests → submodule commit.
2. Update Surveillance-Toolkit reports + scripts → submodule commit.
3. Run `scripts/Invoke-Localization.ps1 -Update` → regenerate `.pot` and merge into `.po`.
4. PI / translators re-confirm fuzzy strings in Weblate (or accept the English source as a no-op for English).
5. Update workspace submodule pointers → workspace-level commit referencing the row.

For 13 rows in §9.5.4, that's 13 lockstep change cycles. Phase 3 may batch related rows (e.g. 1+4+5 device-and-incidence cluster) into one PR per cluster. Estimate: 5–10 PRs spread over 1–2 weeks, plus the Weblate translation cycle.

---

## §10. Open decisions for the PI

Each subsection states the question, the recommendation with rationale, and a "PI resolution" line for you to fill in. Resolution can be: `accept`, `accept with amendment: …`, `reject — <alternative>`.

### §10.1. D-A. Audience tier per public-surface symbol

**Question.** For each of the 41 public-surface symbols in §3 — 38 `export()` entries and 3 `S3method()` entries — which audience tier applies? External-stable (documented for data scientists / researchers / clinicians as part of the stable public API) or internal-stable (stable for the NeoIPC internal pipeline but not primarily targeted at external users)? The lifecycle — stable or experimental (API may change, warn external users) — is §3's separate column, on top of the tier.

**Recommendation.** Accept the tier column proposed in §3 as the default assignment. Key assignments, counted over that column:

- **external-stable (22):** `import_dhis2`, `dhis2_connection_options`, `dhis2_dataset_options`, `neoipc_poisson_ci`, `neoipc_wilson_ci`, `bootstrap_quantile_ci`, `get_pathogen_taxonomy`, `is_valid_ichi_code`, `print.neoipcr_dhis2_conopt`, `neoipcr_log_config`, `neoipcr_supported_versions`, `get_cumulative_incidence_table`, the validation surface — `validate`, `validation_rule_ids`, `validation_rule_context_fields`, `validation_rule_severities`, `validation_summary`, `read_validation_exceptions`, `resolve_validation_exceptions`, `validation_exception_usage` — and the reconciliation surface, `reconciliation_ids` and `reconciliation_details`.
- **internal-stable (19):** the 12 `get_*_table` builders, both `calculate_*_data` pipeline entries, `get_benchmark_data` (pre-rename), `pretty_names` with its two methods, `write_json`.
- **Lifecycle `experimental` (17), across both tiers:** `bootstrap_quantile_ci` (not yet integrated into the rate-table pipeline), `is_valid_ichi_code` (syntax-only validator; the full-code bundling task is in flight), `get_cumulative_incidence_table`, the eight validation-surface symbols and the two reconciliation-surface symbols (their shapes settle with the reports that consume them), all external-stable; `pretty_names` + its two methods (the S3 generic is unstable, and §6.6 rewrites its class-specific method at [calc-api.R:893](R/calc-api.R#L893)) and `write_json` (a deliberately narrow first cut — plain lists, scalars, character vectors and dates — without a reading companion), all internal-stable.

**Rationale.** External-stable tier covers functions that appear in all five reports AND are documented in the auth chain / data-protection section of `.claude/rules/package-internals.md` (the auth and dataset-options trio), plus standalone statistical utilities (`neoipc_*_ci`, `bootstrap_quantile_ci`), plus the widely-used taxonomy accessor and the ICHI validator, plus the two package-configuration and introspection entry points any caller may need before importing. Everything internal-stable is load-bearing for the NeoIPC pipeline but not primarily targeted at external users — Phase 5 vignettes can expose them progressively. Everything experimental has a specific reason flagged in §3.

Task 1.4 (lifecycle badges) consumes §3's Lifecycle column verbatim — `lifecycle::badge()` accepts only lifecycle stages, not a tier; the tier column decides which symbols the Phase 5 vignettes present to external users first.

**PI resolution:** _pending_

### §10.2. D-B. GT-styling entry point shape

**Question.** For the new GT-styling layer (§8), which shape: (i) S3 method `as_gt.<neoipcr_class>()`, or (ii) named function `format_neoipcr_table_gt(x, ...)` with manual `inherits()` dispatch?

**Recommendation.** (i) S3 method. Three reasons: the package already uses S3 dispatch for `pretty_names.neoipcr_tbl_sr_ref`; `methods("as_gt")` discovers every supported class automatically; adding a new rate-table class (when the resistance-rates expansion task lands) becomes "write an `as_gt.new_class()` method" rather than "edit the big if/else in `format_neoipcr_table_gt()`."

**Rationale.** See §8.1 for the rejected alternative. The S3 path also allows downstream consumers to dispatch `as_gt()` themselves for custom classes without neoipcr having to know about them.

**PI resolution:** _pending_

### §10.3. D-C. `get_benchmark_data()` rename

**Question.** Rename `get_benchmark_data()` to `calculate_benchmark_data()` as the task file asserts?

**Recommendation.** **Reject the proposed rename. Keep `get_benchmark_data()` as-is.**

**Rationale (corrected after reading the source).** The task file claims `get_benchmark_data()` "actually *computes*" — that claim is wrong. The function body in [R/calc-api.R:326–823](R/calc-api.R#L326-L823) takes pre-computed `neoipcr_rep_ds` / `neoipcr_ref_ds` inputs, prefixes their column names with the dataset name, and stitches them side-by-side via `bind_cols` / `full_join`. The roxygen at [calc-api.R:313](R/calc-api.R#L313) literally describes it as "*Creates* a NeoIPC benchmark data set *from* department report datasets and a reference data set." The only computation in the body is `fix_zero_event_ci()` (a small CI patch where merged-in zero-event departments produced garbage CIs); that's a fix-up, not a calculation primitive.

So the symmetry argument with `calculate_department_data()` / `calculate_reference_data()` doesn't hold — those two compute rates and CIs from raw `neoipcr_ds`; this one combines already-computed datasets for side-by-side display.

**Alternative renames considered.** If we want a name that more accurately describes "combine for side-by-side display," `combine_benchmark_data()` or `merge_benchmark_data()` would fit. None of these is clearly better than the current `get_benchmark_data()` — `get_*` is a familiar fall-back when neither "compute" nor "retrieve from storage" cleanly fits, and a cosmetic rename is not worth touching every Surveillance-Toolkit caller.

**Companion fix.** The task file at line 31 (the public-interface refinement task) needs its "actually *computes*" claim corrected — already done in the same commit as this note.

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

### §10.7. D-G. Message-catalogue scope

**Question.** One `R-neoipcr.pot` / `R-neoipcr.po` catalogue for all M-class messages, or split catalogues by subject (e.g., auth vs. validation)?

**Recommendation.** Single catalogue. Matches `potools` convention (§6.4 row 5), matches R's own convention, matches the current state. Splitting catalogues creates a coordination burden for translators with no operational benefit.

**Rationale.** After the F-class migration (§6.5), the catalogue holds the condition messages, the credential prompts, and the print method's text (§6.8) — plus the two column headers if the `:45` call keeps them on `gettext` — well within a single-file size. No need to complicate.

**PI resolution:** _pending_

### §10.8. D-H. Localization architecture split

**Question.** Confirm the messages-vs-data split architecture described in §6.5?

**Recommendation.** Accept. Five independent ecosystem precedents (§6.4) converge on the same split; neoipcr is already close to it; the A4a enumeration showed the scope of the migration is narrow (one function family, 11 call sites, plus 2 column-header calls). Class-tagged conditions are already in place: every condition is raised with `rlang::abort()` or `rlang::warn()`, its user-facing strings wrapped for the catalogue (§6.8).

**What this commits:**

- `gettext` / `gettextf` stays on messages only.
- Factor labels (the 11 procedure-category label calls, one of them the compatibility recode) migrate to `procedure_category_labels` tibble in `sysdata.Rda` (§7 row 2).
- DHIS2 `display*` fields flow through unchanged; no `gettext` wraps added on top (§6.3 confirms none exist today).
- Every neoipcr entry point that produces localized output accepts an explicit `locale` parameter; the resolution chain is `explicit_arg → opts$locale → Sys.getlocale("LC_MESSAGES") → "en"`.
- The two column-header `gettext` calls ([calc-api.R:893](R/calc-api.R#L893), [calc-procedure-categories.R:45](R/calc-procedure-categories.R#L45)) rewritten in Phase 2 (§6.6).

**Rationale.** See §6.4 precedents + §6.5 proposal + §6.6 migration plan. This is the most architecturally consequential decision in this note because §5, §7, §9, and Phase 2/4 all key off it.

**PI resolution:** _pending_

### §10.9. D-I. Cross-surface naming alignment for result tables (across 9 surfaces, code + report display)

**Question.** Adopt the master-name approach in §9.5 (single canonical AMA-grounded name per result table; mechanical derivation rules for nine surfaces: function / class / slot / rate / Quarto param / PS switch / `.qmd` filename / heading key / display string)? And — separately — confirm or amend each of the 13 master-name proposals in §9.5.4 plus their per-row section-vs-table heading treatment (option a/b/c per §9.5.2.2)?

**Recommendation.** Accept the framework (master-name + derivation rules) — including the §9.5.5 cross-domain alignment principle that drops the earlier "separation of identifier from display" carve-out. Each of the 13 master-name proposals deserves an individual yes/no/amend; recommendations from §9.5.4 reproduced here for one-pass review:

| # | Today | Proposed master | Heading treatment |
|---|-------|-----------------|-------------------|
| 1 | usage / risk density | `usage_density_rate` | (b) keep section heading "Presence of Risk and Protective Factors"; sub-heading per table |
| 2 | antibiotic utilization | `antibiotic_utilization_rate` | (a) align to master |
| 3 | surgery / surgical procedure rate | `surgery_rate` | (b) keep section "Surgical Procedures by Category" |
| 3b | (asymmetric ref function) | Phase 2 design fix, not §9.5 | — |
| 4 | incidence density rate | `incidence_density_rate` | (b) keep section "Severe Infections and NEC" |
| 5 | dev-ass incidence density | `device_associated_incidence_density_rate` | (a) align |
| 6 | iadrpit | `infectious_agent_detection_rate_per_infection_type` | (b) keep section "Infectious Agents in Nosocomial Infections"; sub-heading per table |
| 7 | iadrpa | `infectious_agent_detection_rate_per_agent` | (a) align under same parent as #6 |
| 8 | abr / resistant pathogen / antibiotic resistance | `antibiotic_resistance_infection_rate` (alt: `amr_infection_rate`) | (a) align |
| 9 | organism resistance rate | `infectious_agent_resistance_rate` | (a) align |
| 10 | resistance test rate | **`antimicrobial_susceptibility_test_rate`** (adopt the H-surface AMA term, rename F/S/C/P/Q/Z to match) | (a) align |
| 11 | secondary BSI rate | `secondary_bsi_rate` | (a) align — D string can spell out "Bloodstream Infection" if AMA prefers |
| F1 | birth weight figure | `birth_weight_figure` | (a) align — Z renames `_fig-bw.qmd` → `_fig-birth-weight.qmd` |
| F2 | gestational age figure | `gestational_age_figure` | (a) align — Z renames `_fig-ga.qmd` → `_fig-gestational-age.qmd` |

**PI calls flagged in §9.5.4 that need explicit answers:**

- Row 1: is "usage density" the correct epidemiological term, or should the master adopt the report's existing "risk density" wording across all surfaces?
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

- Task 1.2, the class-slug rename — owns the `_iaf` / `_sbd` / `_udr` rename. §3/§4/§9 assume post-rename names.
- The CSV-to-YAML migration — owns the infectious-agent metadata reshape. §7 rows touching infectious agents wait on it.
- The lifecycle-badges task — consumes the Lifecycle column of §3 and applies `lifecycle::badge()` markup. Cannot start until this note lands.
- The empty-data-resilience work — already completed; remaining crash paths are scoped out.
- The test-coverage work — already completed; the fixture helpers §11 points at are its output.

---

## §13. Rate Definitions of the Report Tables

This section defines the rate of every table and figure that the Partner and Reference Reports print from a calculated
dataset: what each row counts, over what, on what scale, with what interval and sparse-data flag, and which departments
enter its quartiles. Different rates answer different questions, so their numerators and denominators differ between
tables by design; each denominator holds only the population at risk of what its numerator counts. In the measure
tables below, *Counted* is the numerator, *Over* the denominator, *Interval* the method of the row's pooled and
department intervals (§13.1.4), *Flag* what the sparse-data flag tests (§13.1.2), and *Quartiles* the departments that
enter a row's quartiles. Where an earlier section describes a table's rows, columns, intervals, or quartiles, a
dataset's slots, or the procedure categories otherwise, it describes the package as it is, and this section the
definitions the package moves to, which govern.

### §13.1. Rules Every Table Follows

#### §13.1.1. Pooled Rate and Quartiles

A row's pooled rate divides the events of all departments by their combined denominator, so that each department weighs
by its size, and the pooled rate can therefore fall outside the interquartile range; it takes every department, whatever
the minimum below, except where a row of the density of infections with resistant infectious agents leaves out a
department that tested none of its detections (§13.8.5).

Q₁, Q₂, and Q₃ are the 25th, 50th, and 75th percentiles, as `stats::quantile()` computes them by default (type 7,
interpolating between adjacent ranks), of the rates of the departments that enter a row: those whose own denominator of
that row is positive and reaches the minimum, at least 50 days of a day denominator (patient-days, device-days,
substance-days) or at least 5 of a count denominator (patients, infections, procedures, detections), each at a rate of 0
where it had none of the row's events. The same departments make up the quartiles, their intervals (§13.1.4), and the
display test, which is taken row by row: the quartiles are shown where at least 5 departments enter and their median
denominator (`stats::median()`) exceeds the denominator in which the pooled rate gives 1 event, the rate's scale divided
by the pooled rate, so that a row whose pooled rate is 0 shows none. A row that fails the test shows its pooled rate and
its interval only. The quartiles are those of the departments' observed rates, chance included: the fewer events a
department can expect, the further its rate can lie from its underlying rate, so that in rows of rare events Q₁ is
often 0 and the quartiles spread wider than the departments' underlying rates, and a rate above Q₃ or below Q₁ is a
reason to look closer, not by itself evidence of an excess or a deficit, which the methods texts say. The Partner
Report's interpretation texts call a department's rate above Q₃ (below Q₁) only where its 95% interval lies wholly above
(below) the pooled rate, and say of the others that chance can explain the difference.

#### §13.1.2. Sparse-Data Flag

A count, a density, or a ratio is flagged where its effective count, the count divided by its design effect (§13.1.4),
is below 16: below 16 independent events a count's relative standard error exceeds 25% (1/√16), the level from which
the Washington State Department of Health marks an estimate as unstable (*Agency Standards for Reporting Data With
Small Numbers*, 2018), and an effective count carries the clustering into that error. A share of patient-days, the days
with a device, a ventilatory support, an antibiotic, or a protective factor per 100 patient-days or a single
substance's days of therapy per 1000 patient-days (§13.7), is flagged the same way, on its effective count of days, so
that a rate resting on few days is flagged, a 0 included, and a substance's row and its ATC group's alike. Any other
share is flagged where its effective denominator, the denominator divided by the share's design effect, is below 16,
the denominator itself not printed, as the US National Center for Health Statistics tests a proportion's effective
sample size (Parker JD, Talih M, Malec DJ, et al. National Center for Health Statistics data presentation standards for
proportions. *Vital Health Stat 2*. 2017;(175)), but at 16 rather than its 30 and without its limits on the interval's
width: where that standard withholds a proportion, these reports flag it, and its interval shows its width. A figure
without a design effect of its own tests the counts its interval takes (§13.1.4); a pooled figure to which fewer than 2
departments contribute, a pooled 0 or share of 100%, and a share whose days exceed its denominator, which have no
interval (§13.1.4), are always flagged; and the share followed up for a secondary BSI (§13.5), which has no interval,
tests its denominator. The mean days per exposed patient is the one ratio flagged on its effective number of exposed
patients instead: a department's count of exposed patients, who are its clusters and so independent, and for a pooled
figure that count divided by a design effect for the clustering of the patients within departments: the bias-reduced
cluster-robust (CR2) variance of the pooled mean, with the departments as clusters (§13.1.4, item 2), over its variance
with the patients as clusters, at least 1 and widened for the Bell-McCaffrey degrees of freedom of the CR2 variance, as
item 2's design effect is, and 1 where both variances are 0, as when every exposed patient has the same days. The
threshold is configurable, 16 being its default. A pooled figure, or a department's, whose own design effect rests on
fewer than 8 degrees of freedom carries a flag of its own (§13.1.4), and so does a department's figure to which fewer
than 9 of its patients contribute a positive denominator of the row, whatever design effect it takes (§13.1.4, item 4),
since 8 or fewer leave fewer than 8 degrees of freedom: a 0, a share of 100%, and a figure to which a single patient
contributes, which have no design effect of their own, are thus flagged where few patients contribute, as a pooled 0 or
100% and a pooled figure from 1 department always are, while such a figure from 9 or more patients carries the
sparse-data flag alone. A share whose denominator counts the department's patients, each once, takes a design effect of
1 (§13.1.4, item 4) and keeps the flag on its effective denominator alone, which already falls below 16 with fewer than
16 patients. Every share carries its denominator, and every figure the effective number its flag tests, the degrees of
freedom of a design effect that the steps of §13.1.4 estimate from its own clusters, and, for a department's figure, the
number of its patients that contribute a positive denominator of the row, in the table neoipcr returns, so that a report
can flag it.

#### §13.1.3. Zeros

Where a department has a row's denominator but none of its events, its rate is 0, given, where the row has an
interval, with the upper limit of the row's own 95% interval (§13.1.4), Clopper-Pearson's for a share and the gamma
interval for a rate over days or a ratio, on the counts divided by the design effect that §13.1.4 gives it (item 4), so
that every value of a row comes from a single method; a pooled 0 has no interval (§13.1.4, item 2). Where the
department lacks the denominator, the row has no rate, and the Partner Report prints "No data." The reports'
interpretation texts keep their note that a 0 may mean incomplete data capture.

#### §13.1.4. Intervals

Every interval is at 95%, and none may understate the uncertainty, to which the clustering of events and days within
patients and within departments adds. Each row keeps a single method for every value it prints, its pooled figure, a
department's figure, and a zero's upper limit (§13.1.3), while its quartiles take their own:

1. **Quartiles:** a quartile's interval runs between the rates of 2 of the departments that enter the row (§13.1.1), an
   order-statistic interval with the innermost ranks from the binomial distribution at which each tail holds at most
   2.5%; since each department's rate counts once in it, it carries the clustering within patients and within
   departments, and it bounds the quartile of the departments' observed rates, which their counting noise spreads wider
   than their underlying rates. A side is open, a lower limit printed as the measure's minimum (below) and an upper
   limit as open, where its rank does not exist (fewer than 13 departments for Q₁'s lower and Q₃'s upper limit, fewer
   than 6 for each limit of Q₂) or is the lowest rank for a lower limit or the highest for an upper one, by rank
   whatever ties, so that no limit rests on the lowest or highest rank alone: Q₁'s lower and Q₃'s upper limit thus close
   from 20 departments, and each limit of Q₂ from 9. Q₁'s interval is [0, 0] once as many departments recorded none of
   the row's events as its upper rank, which need not be a majority, so a Q₁ of 0 with that interval carries a note that
   counts them: "None recorded in {zeros} of {departments} departments." Every report keeps the note, which gives the
   number of the departments entering the quartiles that recorded the row's events where the quartile limits at 0 only
   bound it, since it names none and tells the reader that [0, 0] need not mean a majority; with few departments, as in
   a per-class report, the interval [0, 0] itself says nearly as much.
2. **Pooled figures:** a reference dataset's pooled rate, ratio, or share takes the row's interval on effective counts,
   its events and denominator divided by a design effect: the bias-reduced cluster-robust (CR2) variance of the pooled
   figure, with the departments as clusters, over its Poisson or binomial variance, at least 1 and widened for the
   Bell-McCaffrey degrees of freedom of the CR2 variance. The exact interval is the gamma interval for a rate over days
   and for a ratio, which at a design effect of 1 is the exact Poisson interval on the counts, and Clopper-Pearson's for
   every share, a share of days included (item 3). A pooled interval is given from 2 contributing departments, those
   with a positive denominator of the row but those that §13.8.5 leaves out of a row of its density, and one that rests
   on fewer than 8 degrees of freedom is flagged. A pooled figure from fewer than 2 contributing departments, a pooled
   0, and a pooled share of 100% leave no variance to estimate a design effect from, and so have no interval: such a
   figure is flagged (§13.1.2), and a note says that its uncertainty cannot be estimated from the data.
3. **Shares of days:** a share of patient-days (§13.6.1, §13.7) and the Access share of substance-days (§13.6.1) take
   Clopper-Pearson's interval on an effective number of days, the days divided by the design effect, so that their
   interval never passes 100%. Kish's effective number of patients, (Σmᵢ)²/Σmᵢ², mᵢ being patient i's patient-days, or
   for the Access share its substance-days, is what a department's figure that counts days takes without a reference
   where nothing else gives it a design effect (item 4). A share whose days exceed its denominator, which validation
   flags as an error (§13.1.5) but which an exception or an import without the validation pass lets through, has no
   interval and is flagged (§13.1.2).
4. **A department's own figure** takes its row's method on effective counts, with a design effect of at least 1 that the
   steps below compute with the department's patients that have a positive denominator of the row as the clusters,
   widened as item 2's is, a figure whose design effect rests on fewer than 8 degrees of freedom being flagged. The
   degrees of freedom equal the number of contributing patients less 1 only where all have the same denominator, and
   fall below it as the denominators grow unequal and as the patients' figures vary, so that a department of 20 to 50
   patients can rest on fewer than 8. A figure of 0, a share of 100%, and a figure to which a single patient
   contributes, which leave nothing to estimate a design effect from, and a figure whose own design effect rests on
   fewer than 8 degrees of freedom, which leaves too little, take instead the network's design effect for the row and
   the department dataset's birth-weight class (§13.11.2), which the same steps compute over the patients of all
   departments in that class that have a positive denominator of the row, at least 1, and which the reference stores;
   where it stores none for that class, as where the class's own figure is 0 or 100%, they take the reference's design
   effect for the row over its whole range, and where it stores none for the row either, as where the network's figure
   is 0 or 100% or the reference lacks the row (§13.8.2), what they take without a reference, so that such a 0 of a row
   that does not count days counts the department's events as independent, which the methods texts say. Without a
   reference, such a figure takes the following: one that counts days, a share of days or a ratio of days over
   patient-days (the Access, Watch, Reserve, and Not classified rows of §13.6.1 and an ATC group's use rate, §13.7), the
   design effect that makes its effective denominator Kish's effective number of the department's patients (item 3); the
   mean days per exposed patient and the operations per patient operated on, an effective count equal to their number of
   patients, 1 for a single patient, since a design effect of 1 would count the days or operations as independent
   events; and any other, a design effect of 1. A figure of 2 or more patients that has a design effect of its own,
   being neither 0 nor a share of 100%, takes the larger of that fallback and its own, so that without a reference no
   interval falls below the figure's own estimate, as a design effect of 1 would for events clustered within patients. A
   figure whose own design effect rests on 8 or more degrees of freedom keeps it, with a reference or without. A share
   whose denominator counts the department's patients, each once (the patients exposed of §13.6.2, the patients
   receiving a substance or group of §13.7, and the patients operated on of §13.9.3), takes a design effect of 1
   whatever its figure, with a reference or without, since its patients are its clusters, each a 0 or a 1, and leave no
   clustering to correct for, so that it carries no degrees of freedom.
5. **Scope:** the intervals describe neonatal departments like the participating ones.

No lower limit is printed below its measure's minimum, an open one of a quartile included: 0, but 1 for the operations
per patient operated on (§13.9.3) and the mean days per exposed patient (§13.6.2), and 100 per 100 infections for the
Total of the infectious agents detected (§13.8.3), measures in which every unit of the denominator holds at least 1 of
the numerator. Since the true value never lies below the minimum, the cut keeps every interval's coverage.

The pooled interval of item 2 takes 7 steps, over the G departments that contribute to it, those with a positive
denominator of the row but those that §13.8.5 leaves out, department i having yᵢ events over its denominator wᵢ (days or
counted entities), with S = Σwᵢ, Y = Σyᵢ, the pooled figure R = Y/S, the residuals uᵢ = yᵢ − Rwᵢ, and b = R for a rate
or ratio (Poisson) or R(1 − R) for a share (binomial); a department's design effect and the network's (item 4) take the
same steps with patients as the clusters, those with a positive denominator of the row, i then being a patient:

1. The variance between departments is a moment estimator of the form of DerSimonian and Laird's:
   τ² = max{0, [Σwᵢ(yᵢ/wᵢ − R)² − (G − 1)b]/(S − Σwᵢ²/S)}.
2. Department i's own figure yᵢ/wᵢ has the working variance φᵢ = b/wᵢ + τ², Poisson or binomial noise plus the variance
   between departments.
3. The ratio estimator's leverage is H = 1wᵀ/S, 1 being a vector of ones and w that of the wᵢ, so that every row of H is
   wᵀ/S; with Φ = diag(φᵢ) and I the identity matrix, M = (I − H)Φ(I − H)ᵀ.
4. The CR2 variance of Bell and McCaffrey, in the working-model form that Pustejovsky and Tipton give for weighted
   estimators, is v = Σuᵢ²(φᵢ/Mᵢᵢ)/S².
5. Its Bell-McCaffrey degrees of freedom are ν = (tr P)²/tr(P²), tr being the trace, with
   P = diag(√φᵢ)(I − H)ᵀ diag[wᵢ²φᵢ/(MᵢᵢS²)](I − H) diag(√φᵢ).
6. The design effect is d = max{1, vS²/Y}(t/1.96)² for a rate or ratio and d = max{1, vS/[R(1 − R)]}(t/1.96)² for a
   share, t being the 97.5th percentile of the t distribution on ν degrees of freedom, so that the effective counts
   shrink for the degrees of freedom as Korn and Graubard's effective sample size does, with the normal quantile 1.96 in
   place of their t quantile on n − 1 degrees of freedom, and for rates and ratios as for shares.
7. The interval is the row's interval on the effective counts Y/d and S/d: for a rate or ratio, the gamma interval of
   Fay and Feuer, [F⁻¹(0.025; Y/d), F⁻¹(0.975; Y/d + 1)]/(S/d), F⁻¹(p; a) being the p quantile of the gamma distribution
   with shape a and scale 1 (`stats::qgamma()`); for a share, the Clopper-Pearson interval on Y/d of S/d
   (`stats::qbeta()`), as Korn and Graubard propose for survey proportions.

The degrees of freedom are ν: the reference stores those of each pooled interval, and a department's dataset those of
each figure's own design effect, which decide whether the figure takes the network's (item 4); the flag below 8 tests
both (§13.1.2), comparing ν with 8 after rounding it to 6 decimal places, since equal denominators give exactly G − 1
only in exact arithmetic. The steps follow these sources:

- DerSimonian R, Laird N. Meta-analysis in clinical trials. *Control Clin Trials*. 1986;7(3):177-188.
  doi:10.1016/0197-2456(86)90046-2
- Bell RM, McCaffrey DF. Bias reduction in standard errors for linear regression with multi-stage samples. *Surv
  Methodol*. 2002;28(2):169-181.
- Pustejovsky JE, Tipton E. Small-sample methods for cluster-robust variance estimation and hypothesis testing in fixed
  effects models. *J Bus Econ Stat*. 2018;36(4):672-683. doi:10.1080/07350015.2016.1247004
- Fay MP, Feuer EJ. Confidence intervals for directly standardized rates: a method based on the gamma distribution.
  *Stat Med*. 1997;16(7):791-801. doi:10.1002/(SICI)1097-0258(19970415)16:7\<791::AID-SIM500\>3.0.CO;2-#
- Korn EL, Graubard BI. Confidence intervals for proportions with small expected number of positive counts estimated
  from survey data. *Surv Methodol*. 1998;24(2):193-201.

The Interval columns below name each row's method, the gamma interval or Clopper-Pearson's, as items 2 to 4 apply it,
the row's quartiles taking the interval of item 1. Beside a table's figures, a reference dataset stores what these
intervals need: for each quartile's interval, which of its sides are open; for each row, the network's design effect
over the reference's whole range and for each birth-weight class (item 4), and the degrees of freedom of the pooled
interval; and, for each row with quartiles, the number of departments that enter them (§13.1.6) and how many of them
recorded none of the row's events.

#### §13.1.5. Which Records Count, and Where

- A stay counts, with all its days, in the period in which its surveillance ended, once the stay has been completed;
  an infection form still open when the data are exported is left out, so a recent period lacks the stays not yet
  entered, and a department that began in the period holds only those of its stays that have ended since, its shorter
  ones first.
- An infection and its days count for the department of the stay (its DHIS2 enrollment) to which the infection belongs,
  an SSI attributed to a surgical procedure counting with it, in the period and for the department of the procedure's
  stay (§13.10.1), and one without an eligible procedure (§13.10.1) counting with its own stay. A validation rule flags
  an event or a patient record whose department differs from its stay's, as a warning that removes nothing.
- A stay without an antibiotic entry counts with all its days and exposures, its antibiotic days being 0; an antibiotic
  entry without days adds none, every sum counting the days that are known, and no analysis is refused.
- A patient record that a validation rule flags as an error leaves every table with all its stays, unless an exception
  names the record or the dataset is imported with `include_invalid_patients = TRUE`, which runs no validation pass; but
  an infection that fails the onset or repeat rules (§13.3), or an SSI that no procedure form holds in its window
  (§13.10.1), leaves by itself, with its infectious agents and secondary BSI, every table and every count of the
  dataset, `n_infections` and `n_severe_infection_patients` included, and its patient record stays, since removing the
  record would select infants on the outcome, and the NeoIPC Core Protocol (version 1.2, pp. 15 and 30) and the National
  Healthcare Safety Network (NHSN; Patient Safety Component Manual, ch. 2, p. 2-2) leave out such an infection, not the
  infant.
- A patient record without a recorded birth weight leaves every table and figure, the overall report and a department's
  own dataset included, and the overall report states their number, so that no report gives their figures back and no
  per-class report withholds a row for them (§13.11.2); the Validation Reports query a missing birth weight.
- A patient is a patient record of 1 department, so an infant whom 2 departments register counts twice in a reference
  dataset.

#### §13.1.6. Contributors

Beneath each table, the reports state how many departments and patients contribute to its pooled rates and how many
departments enter its quartiles, with the inclusion rule: "Pooled: {departments} departments, {patients} patients.
Quartiles: {quartile_departments} departments, those with {inclusion rule}." The statement counts the departments
rather than naming them, since in a small network a named list beside a rare infectious agent or resistance can show
which department had it. A reference dataset names no department and carries, for every table, the number of
departments and of patients that contribute to its pooled rates and, for every row with quartiles, the number of
departments that enter them; where a table's rows differ in the departments that enter, the statement gives the number
per row, and where a row's pooled figure leaves out departments with a positive denominator of the row, as those of the
density of infections with resistant infectious agents do (§13.8.5), the dataset carries, and the statement gives, the
contributing departments and patients per row as well.

#### §13.1.7. Version of the Rate Definitions

Every dataset that `calculate_reference_data()` and `calculate_department_data()` return carries, for every table, the
version of the definitions of that table's rates under which it was computed, and `get_benchmark_data()` joins a table
only where its versions match. Where they differ from those of the first dataset that holds the table, it keeps the
rows of that dataset and of each later dataset whose versions match them, leaves the others' columns of it empty (a
figure, whose rows each name their dataset, takes none of the others' rows), records that the table is not comparable,
and raises a classed warning, never an error; the Partner Report then shows the department's rows of that table without
reference columns, under a callout saying that the department's table and the reference's were calculated under
different definitions of these rates. A change to the definition of a table's rates changes that table's version and no
other; the placement of the procedure codes (§13.9.2) belongs to the definitions of both the surgery table and the SSI
rate, so a change to it changes both versions. A dataset written before it carried these versions counts, for every
table, as calculated under different definitions, so that `get_benchmark_data()` joins none of its tables with a
versioned dataset's.

A table that reads the infectious-agent list compares, beside its version, a digest of each part of the list it reads:
the incidence densities (§13.3), which infectious agents are common commensals, on which their sepsis kinds split; the
tables whose rows are infectious agents (§13.8.2), the tree of their version (over the reporting groups, what places an
infectious agent among the group and summary rows and labels them; over the full ontology, its nodes and paths) and the
concept each code names, by which they count an infectious agent once per infection; the resistance tables, also the
infectious agents to which each resistance category applies; and the completeness of antimicrobial susceptibility
testing (§13.8.7), the concept each code names and the infectious agents to which each category applies. Both flags
are digested as an infectious agent takes them (below), not as the list writes them. The tables whose rows are
infectious agents also compare the version of the layout of their rows (their order, levels, residual and summary rows,
keys, and labels). A release of the list that changes what a table's rows count, which rows it has, how it nests and
labels them, or, over the full ontology, the concept type it gives them (`rank`) makes stored datasets incomparable for
that table until they are recomputed, and one that changes none of these leaves it comparable. An infectious agent takes
its reporting group and attribute values from its nearest listed ancestor or itself, and whether it is a common
commensal and whether a resistance category applies to it each from the nearest flag on its path to the root, its own
included, so a release that adds an `Id` in the ontology alone, moves one, or turns one into a synonym can change the
versions over the reporting groups as well; one that changes the ontology's nodes or paths without changing any
infectious agent's reporting group, attribute values, or concept, or, for the 2 resistance tables, the infectious agents
to which a category applies, leaves them comparable, and one that changes only the reporting classification leaves the
versions over the full ontology comparable. The one part of the list a table reads that no digest compares is the order
of the attributes and their values, which lays out the summary rows over the reporting groups: it comes from the release
a dataset was calculated from, and `get_benchmark_data()` keeps the reference's order as far as its rows of the table
show it, but places a summary row of an attribute the running neoipcr's classification lacks as a top-level row of the
tree, ordered in each block as the tree's rows are (§13.8.2). A stored dataset lacking a digest that a later release of
neoipcr adds counts as calculated under different definitions only for the tables that compare it.

The exposure rates (§13.6.1) and the antibiotic use (§13.7) compare, beside their version, a digest of the category of
the World Health Organization's Access, Watch, Reserve (AWaRe) classification that the DHIS2 metadata a dataset was
imported from gives each substance of its option set `NEOIPC_ANTIMICROBIAL_SUBSTANCES`, a substance without a category
counting as one of its own, Not classified (§13.6.1), so that datasets whose metadata carry different editions of the
classification, or give a substance different categories, are compared for neither table. The antibiotic use also
compares a digest of the group of the 4th level of the Anatomical Therapeutic Chemical (ATC) classification that the
same metadata gives each substance (its option group set `ATC5`), since its group rows sum their substances, so that
datasets whose metadata place a substance in different groups, a substance in no group counting as one of its own
(§13.7), are not compared for it. Metadata that place a substance in 2 groups of `WHO_AWARE` or of `ATC5` are refused:
`calculate_reference_data()` and `calculate_department_data()` stop with a classed error naming the substance and its
groups, since each classification gives a substance at most 1 group.

### §13.2. Overview of the Dataset

`calculate_reference_data()` and `calculate_department_data()` give `n_departments`, a single count, and `n_patients`,
`n_enrollments`, and `n_patient_days`, each with its `total` and, in a reference dataset over the whole range, the
departments' `pooled_mean`, `q1`, `q2`, and `q3`, the type 7 quartiles of §13.1.1 over every department, truncated to
whole numbers. Both also give `n_patients_without_birth_weight`, the patient records of the period that leave the
dataset for lacking a recorded birth weight (§13.1.5), counted from the import before they leave. A reference dataset
with a birth-weight or gestational-age range stores no quartiles of these counts, and a per-class report prints none,
since with few departments they give the departments' sizes back; the overall Reference Report prints them only where at
least 5 departments enter, as §13.1.1's display test shows every row's quartiles; from 5 departments, a quartile can
equal 1 department's count, as a rate's quartile can equal 1 department's rate, without naming the department. The
reports' introductions print the counts and the mean surveillance time per admission, which replaces today's average
surveillance period per patient record; the overall Reference Report also prints the departments' quartiles of patient
records and of patient-days, which no report prints today, and the number of patient records left out for lacking a
recorded birth weight. None of them has an interval or flag, and no report prints `pooled_mean`.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Departments | departments with a stay whose surveillance ended in the period | — | count | — | — | — |
| Patient records | patients with such a stay | — | count | — | — | — |
| Patient admissions | such stays | — | count | — | — | — |
| Patient-days | every calendar day of those stays, the days of admission and discharge included | — | days | — | — | — |
| Mean surveillance time per admission | patient-days | admissions | days | — | — | — |
| Patient records per department, Q₁, Q₂, and Q₃ (overall Reference Report) | each department's patient records | — | count | — | — | every department, from 5 departments |
| Patient-days per department, Q₁, Q₂, and Q₃ (overall Reference Report) | each department's patient-days | — | days | — | — | every department, from 5 departments |

The mean surveillance time per admission is the mean length of stay over which exposure accrues; the quartiles per
department show how far the largest departments carry the pooled rates.

### §13.3. Incidence Density Rates of Nosocomial Infections

`get_incidence_density_rate_table()` builds the table, 1 row per measure in `inf`, with `n`, `pooled`, `ci_lower`,
`ci_upper`, and in a reference dataset `q1`, `q2`, and `q3` with their limits (§13.1.4), `q1_ci_lower` to `q3_ci_upper`.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Severe infection | episodes of primary sepsis/BSI and of pneumonia | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Primary sepsis/BSI | episodes of primary sepsis/BSI | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Laboratory-confirmed BSI with a recognized pathogen | primary BSI with at least 1 recognized pathogen, with or without a common commensal | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Laboratory-confirmed BSI with a common commensal | primary BSI with a common commensal and no recognized pathogen | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Clinical sepsis | primary sepsis recorded as "No positive blood culture" (`NEOIPC_BSI_NO_POS_CULTURE`) | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Pneumonia | episodes of pneumonia | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| NEC | episodes of NEC | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Surgical NEC | NEC with a surgical or pathological finding or a pneumoperitoneum | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |
| Medical NEC | the other NEC | patient-days | per 1000 | gamma | effective count < 16 | ≥ 50 patient-days |

Patient-days include days of life 1 to 3 and days 1 and 2 of a stay, on which a sepsis/BSI, a pneumonia, or an NEC
counts only where an exception keeps it, and the 13 days after an infection, in which the NeoIPC Core Protocol registers
no new infection of the same type; the protocol's formula counts every patient-day, which the reports' methods texts say
(here and below, what the methods texts say is a statement for the reworked reports' methods texts, which today's do not
hold). Since departments differ in how many of these days their stays hold, a rate over every patient-day can order 2
departments otherwise than one over the days at risk alone, and since the 13 days dilute a rate the more, the higher it
is, they draw a department's ratio to the reference towards 1, which the methods texts say. A patient with a sepsis/BSI
and a pneumonia counts once for each in severe infection, and a further episode of the same type counts where it begins
14 days or more after the previous one and after a period without symptoms of infection. An infection fails the onset
rules, a validation error that leaves it out of every table and count while its patient record stays (§13.1.5), where a
sepsis/BSI, a pneumonia, or an NEC begins before day of life 4 or before day 3 of its stay, or where an infection of any
type, an SSI included, begins fewer than 14 days after the previous one of its type. An SSI's onset is otherwise judged
by its procedure's follow-up window alone (§13.10.1), although the protocol states both cut-offs for every infection it
records, SSIs included, in the general text of its section on infection data collection: an SSI within its procedure's
window is hospital-acquired by its definition, and the cut-off of day 3 would drop the SSIs on days 1 and 2 of a
readmission that §13.10.1 counts, which the methods texts say. The protocol gives the cut-off of day 3 of the stay for
transfers and readmissions; §13 applies it to a sepsis/BSI, a pneumonia, or an NEC in every stay, as NHSN (Patient
Safety Component Manual, ch. 2) and ITS-KISS, the intensive-care component of the German hospital infection surveillance
system (KISS; Nationales Referenzzentrum für Surveillance von nosokomialen Infektionen. *Protokoll: Surveillance
nosokomialer Infektionen auf Intensivstationen*. Updated January 2026), judge every admission by its third day, so that
the rule, judged from the dates, does not depend on the recorded admission type, which can be wrong; this changes only
an infection that an exception keeps before day of life 4, since a stay that begins on the day of birth has its days 1
and 2 on days of life 1 and 2. An infection that the protocol accepts before day of life 4, as clearly
hospital-acquired, is kept by an exception that names its record for each rule it fails. Primary sepsis/BSI leaves out
secondary BSI, which its own table shows.

The 3 kinds of primary sepsis/BSI add up to it, and the 2 kinds of NEC to NEC. A BSI with both a recognized pathogen and
a common commensal counts with the recognized pathogen; the common commensals are those the released NeoIPC
infectious-agent list marks as such, an infectious agent taking the flag from the nearest one on its path to the root,
its own included (§13.1.7). "Not listed" (`Id` 0) and "Unidentifiable" (`Id` 3561) count as recognized pathogens, as the
list has them, and so does a positive culture whose infectious agent the list cannot classify, its slots holding only
codes the list lacks or naming none, whose number the row of laboratory-confirmed BSI with a recognized pathogen gives
in `n_unclassified` and its footnote states. Clinical sepsis includes sepsis without a blood culture, which the form
cannot tell from a negative one. Surgical NEC is an NEC whose form records `NEOIPC_NEC_BOWEL_NECROSIS`,
`NEOIPC_NEC_PNEUMATOSIS_INTESTINALIS_SURG`, or `NEOIPC_NEC_PNEUMOPERITONEUM`, the pneumoperitoneum standing in for the
intestinal perforation that the form does not record. The row is thus wider than the protocol's definition of the same
name, whose findings are bowel necrosis and pneumatosis intestinalis at surgery or on pathology, a pneumoperitoneum on
imaging being a sign of its symptom-based definition, and "Medical NEC" is no term of the protocol; the reports' methods
texts say both.

The reports' section on nosocomial infections opens with counts only, since an average per patient has no time base: the
severe infections (`n` of the severe-infection row), the patients with at least 1 (`n_severe_infection_patients`, which
both `calculate_reference_data()` and `calculate_department_data()` give with its `total`, counting each patient record
once whatever its episodes and stays), and all patients (`n_patients`). Strata by birth-weight class or gestational-age
range are reports of their own (§13.11.2), not rows of this table; a per-class report that withholds the
severe-infection row leaves the severe infections and the patients with at least 1 out of its opening.

### §13.4. Incidence Density Rates of Device-Associated Infections

`get_dev_ass_incidence_density_rate_table()` builds the table, 1 row per device in `dev` (`cvc`, a central vascular
catheter (CVC); `pvc`, a peripheral vascular catheter (PVC); `vs`, any ventilatory support; `inv`, invasive mechanical
ventilation (INV); and `niv`, non-invasive ventilatory support (NIV), by continuous positive airway pressure or
high-flow nasal cannula), with `n`, `pooled`, `ci_lower`, `ci_upper`, and the quartile columns.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| CVC-associated sepsis/BSI | primary sepsis/BSI associated with a CVC | CVC-days | per 1000 | gamma | effective count < 16 | ≥ 50 CVC-days |
| PVC-associated sepsis/BSI | primary sepsis/BSI associated with a PVC | PVC-days | per 1000 | gamma | effective count < 16 | ≥ 50 PVC-days |
| Ventilatory support–associated pneumonia | pneumonia associated with INV or NIV | INV-days plus NIV-days | per 1000 | gamma | effective count < 16 | ≥ 50 INV-days plus NIV-days |
| INV-associated pneumonia | pneumonia associated with INV | INV-days | per 1000 | gamma | effective count < 16 | ≥ 50 INV-days |
| NIV-associated pneumonia | pneumonia associated with NIV | NIV-days | per 1000 | gamma | effective count < 16 | ≥ 50 NIV-days |

An infection is associated with the device its form records, by a single rule for every device, ventilation included, as
the protocol's data dictionary and the forms state it: the device is in place on at least its third day, the day of
placement being day 1, on the day of infection or the day before, a CVC taking precedence over a PVC and INV over NIV;
the association is temporal, not proven causal. The protocol's note on device-associated pneumonia, which asks for at
least 4 calendar days of ventilation, departs from this rule, and the note and the reports' methods text, which follows
the note today, are to follow the rule; since neoipcr counts the association that the form records, a department that
followed the note recorded none where ventilation lasted exactly 3 calendar days, which the methods texts say, citing
version 1.2's rule for every device (p. 20), until the protocol's next release corrects the note. A device day has the
catheter in place, or the support given, for at least 12 hours that calendar day, as version 1.2 of the NeoIPC Core
Protocol (pp. 35 and 41) and the forms in use define it; an edition whose period spans a change of that definition in
the forms names both definitions and the date of the change. A device's first 2 days, which cannot carry an associated
infection, stay in its denominator, since the day counts carry no dates by which to remove them. The sepsis/BSI rows
include clinical sepsis, so the CVC row is not the central line–associated BSI rate of the National Healthcare Safety
Network (NHSN). The protocol's definition of a PVC day makes no exception for a CVC, so a day with both catheters counts
for both; a department that counts such days otherwise makes its PVC rates differ by that convention.

### §13.5. Secondary Bloodstream Infections

`get_secondary_bsi_rate_table()` builds the table, 1 row per infection type in `event_type_key` (`nec`, `hap`, and
`ssi`), with `n`, `pooled`, `ci_lower`, `ci_upper`, and the quartile columns, and beside them each type's followed-up
infections, its infections, and its share followed up.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Secondary BSI rate | infections of the type followed up and answered Yes, with a secondary-BSI slot naming an infectious agent | infections of the type followed up for a secondary BSI, answered Yes or No | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 followed-up infections of the type |
| Followed up | infections of the type followed up for a secondary BSI | all infections of the type | % | none | denominator < 16 | Reference Report: ≥ 5 infections of the type |

A slot names an infectious agent where it holds one, "Not listed", "Unidentifiable", and a code the list lacks
included, but not where it holds only a companion value, such as a resistance result. Since the numerator takes only
infections followed up and answered Yes, it is part of the denominator whatever a validation exception lets through.
The Partner Report gives the department's share followed up beside the pooled one, the Reference Report the pooled
share with the departments' quartiles of their shares; a department that follows up only some infections of a type
enters the pooled rate and the quartiles as any other. A secondary BSI is attributed to its infection by an infectious
agent in the blood that matches one from the primary site; the NEC form records none from the primary site, so for NEC
the match cannot be checked. No density of bloodstream infections of all origins is given, since it would undercount
wherever follow-up is partial.

### §13.6. Exposure to Risk and Protective Factors

Exposure counts whole days, device days as the device-associated rates do. An antibiotic day lies between the first
and the last dose of a therapy inclusive, a skipped dose's day included, and counts once whatever the number of
substances; a substance-day, or day of therapy, counts each substance on each day from its first to its last dose. A
human milk day has enteral feeding with human milk only, donor milk and fortified breast milk included; a probiotic day
an oral probiotic containing *Lactobacillus* or *Bifidobacterium* species; a kangaroo care day at least 2 hours of
skin-to-skin contact.

#### §13.6.1. Exposure Rates

`get_usage_density_rate_table()` builds a reference dataset's table and `calculate_department_data()` a department's,
1 row per factor in `factor` (`cvc`, `pvc`, `vs`, `inv`, `niv`, `ab`, `a`, `w`, `r`, `human_milk`, `probiotic`, and
`kangaroo_care`), with `n`, `pooled`, `ci_lower`, `ci_upper`, and the quartile columns (a department's table: `days`,
`rate`, `ci_lower`, and `ci_upper`), each row's denominator (patient-days, or all substance-days for the Access share),
and the number of departments that enter the row's quartiles (§13.1.6), and a row each for the substance-days not
classified and the Access share, in the reference and the department tables alike.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| CVC; PVC | days with the catheter | patient-days | per 100 | Clopper-Pearson | effective count of days < 16 | ≥ 50 patient-days |
| Any ventilatory support; INV; NIV | days with the support, any support being INV-days plus NIV-days | patient-days | per 100 | Clopper-Pearson | effective count of days < 16 | ≥ 50 patient-days |
| Any antibiotic | antibiotic days | patient-days | per 100 | Clopper-Pearson | effective count of days < 16 | ≥ 50 patient-days |
| Access; Watch; Reserve; Not classified | substance-days of the category | patient-days | per 100 | gamma | effective count < 16 | ≥ 50 patient-days |
| Access share | Access substance-days | all substance-days, Not classified included | % | Clopper-Pearson | effective denominator < 16 | ≥ 50 substance-days |
| Human milk; Probiotics; Kangaroo care | days of the factor | patient-days | per 100 | Clopper-Pearson | effective count of days < 16 | ≥ 50 patient-days |

The rows over patient-days are shares of patient-days (§13.1.4), except the Access, Watch, Reserve, and Not classified
rows, which sum their substances' days of therapy and so are ratios: a patient given 2 antibiotics on 1 day contributes
2, so these rows can add up to more than the row of any antibiotic. They give the days of therapy by the category of the
World Health Organization's Access, Watch, Reserve (AWaRe) classification that the DHIS2 metadata gives each substance:
its option group set `WHO_AWARE` holds the substances of each category in an option group (`WHO_AWARE_ACCESS`,
`WHO_AWARE_WATCH`, or `WHO_AWARE_RESERVE`), so that the categories are those of the edition of the classification that
the metadata carries, and `get_benchmark_data()` compares this table and the antibiotic use (§13.7) only across datasets
whose metadata give every substance the same category (§13.1.7). A substance without an AWaRe category counts in Not
classified, and in the Access share's denominator but not its numerator. No target stands beside the Access share: the
World Health Organization's target for 2023, at least 60% of each country's total antibiotic consumption (target 4b of
its Thirteenth General Programme of Work), and that of the political declaration on antimicrobial resistance that the
United Nations General Assembly adopted on 7 October 2024, at least 70% of human antibiotic use globally by 2030
(resolution 79/2, paragraph 64), concern antibiotic use in human health as a whole, not the care of high-risk neonates,
for which no realistic benchmark is known, so the departments' quartiles are the reference. A department whose policy
excludes a factor, such as probiotics or donor milk, has a true 0, which enters the pooled rate and the quartiles.

#### §13.6.2. Patients Exposed and Their Days of Exposure

A table of its own gives, for CVC, PVC, INV, NIV, any antibiotic, human milk, probiotics, and kangaroo care, the breadth
and the duration of exposure, which the days per 100 patient-days cannot tell apart. The mean days per exposed patient
ties the duration to the exposure rate; since durations are skewed, a few long exposures raise it above the typical
patient's days, which the methods texts say.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Patients exposed | patients with at least 1 day of the factor | all patients | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 patients |
| Mean days per exposed patient | the days of the factor over all the exposed patients' stays in the period | patients with at least 1 day of it | days per patient, at least 1 | gamma | effective number of exposed patients < 16 (§13.1.2) | ≥ 5 exposed patients |

### §13.7. Antibiotic Use by Anatomical Therapeutic Chemical Group and Substance

`get_antibiotic_utilization_table()` builds the table, 1 row per group of the 4th level of the Anatomical Therapeutic
Chemical (ATC) classification, a 5-character code such as J01CA, in which the DHIS2 metadata's option group set `ATC5`
places each substance (§13.1.7), and 1 per substance used beneath it (`row_id`, `atc5_group`, `row_type`,
`display_name`, and `aware`, the code naming the 4th level `atc5`), with `n`, `pooled`, `ci_lower`, `ci_upper`, and the
quartile columns, and beside them, for each row, the patients receiving it, the patients given any antibiotic, and the
share of the patients given any antibiotic who receive it. A substance that `ATC5` places in no group stands beneath a
group row "No ATC group", last in the table, with `atc5_group` missing, as a substance without an AWaRe category counts
in Not classified, so that the group rows cover every substance-day. No totals of the 1st or 2nd ATC level are given,
although the NeoIPC Core Protocol names these levels too: since nearly every substance of the NeoIPC antibiotic list
that has an ATC code lies in J01, antibacterials for systemic use, they would nearly repeat the sum of the AWaRe rows of
the exposure rates (§13.6.1), which give all substance-days.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Use rate | days of therapy of the substance, or of the group's substances summed | patient-days | per 1000 | Clopper-Pearson for a substance; gamma for a group | effective count of days < 16 | ≥ 50 patient-days |
| Patients receiving it | patients given the substance, or a substance of the group | patients given any antibiotic | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 patients given an antibiotic |

The use rate is per 1000 patient-days, as the NeoIPC Core Protocol's text defines it, in its published version 1.2 of
2023-08-01 (p. 44) and its current draft alike, and as version 1.2's formula computes it; the current draft's formula
reads ×100, against the sentence above it, and is to follow it, as is the reports' methods text, which gives
substance-days per 100 patient-days today. `get_antibiotic_utilization_table()` gives the rate per 100 today, and the
AWaRe rows of the exposure rates stay per 100. A substance's days of therapy in a stay are bounded by its patient-days,
which validation checks as an error (§13.1.5), so its rate is a share of patient-days (§13.1.4), while a group's counts
2 of its substances given on 1 day twice and so is a ratio, and the substances' days of therapy can add up to more than
the antibiotic days. The share of patients receiving a substance or group shows the choice of antibiotic. It is over the
patients given any antibiotic, as the protocol's text defines it (version 1.2, p. 44, whose formula also divides
patients by patients); the current draft's formula repeats that of the use rate, days of therapy over patient-days, and
is to follow the text. A patient receives a substance where an entry records it, whether or not the entry records its
days. The patients given any antibiotic are those with at least 1 entry that records a substance in a stay counted in
the period, so that every patient receiving a substance is among them; their count can differ from the patients exposed
to any antibiotic (§13.6.2), who go by the form's antibiotic days, where an entry stands on a form without antibiotic
days or antibiotic days lack an entry. `aware` holds a substance's AWaRe category as the metadata gives it
(`WHO_AWARE_ACCESS`, `WHO_AWARE_WATCH`, or `WHO_AWARE_RESERVE`; §13.6.1), or Not classified for a substance without one,
and is `NA` on a group's row; the reports print it as Access, Watch, Reserve, or Not classified. Every substance and
group with an entry keeps its row, at 0 known days where all its entries lack days. Antifungal use is not recorded.

### §13.8. Infectious-Agent Tables

Five tables report what the microbiology of the nosocomial infections found. Three have infectious agents as their
rows; 1 engine builds them and the completeness of antimicrobial susceptibility testing, which reads the same
classification. All 5 count findings by the same rules.

| Table | Built by | Reports |
| --- | --- | --- |
| Infectious agents detected in nosocomial infections | `get_infectious_agent_detection_rate_per_agent_table()` | on by default |
| Microbiological testing of nosocomial infections by infection type | `get_infectious_agent_detection_rate_per_inf_type_table()` | on by default |
| Nosocomial infections with resistant infectious agents | `get_abr_infection_rate_table()` | off by default; on in the "full" preset |
| Resistance rates per infectious agent | `get_infectious_agent_resistance_rate_table()` | off by default; on in the "full" preset |
| Completeness of antimicrobial susceptibility testing | `get_resistance_test_rate_table()` | off by default; on in the "full" preset |

`get_infectious_agent_resistance_rate_table()` replaces `get_organism_resistance_rate_table()`, and the slot
`infectious_agent_resistance_rate_table` replaces `organism_resistance_rate_table` (§9.1).

#### §13.8.1. Findings

The tables read the infectious-agent slots of the infection forms, which carry their infectious agent only in a
dataset imported with `include_event = "full"`; `calculate_reference_data()` and `calculate_department_data()` refuse
any other, since the pseudonymized tier leaves the infectious agent out, a rare one being able to single out a patient.
Both warn once where the findings hold codes the list lacks.

1. **A finding names an infectious agent** where its slot holds an infectious-agent code. A slot holding only a
   companion value, such as a resistance category, the source, or a free-text name, names none and makes no infection
   one with an infectious agent.
2. **A slot counts only where the form asks for it:** a primary sepsis/BSI form's slots unless it is recorded as
   "No positive blood culture" (`NEOIPC_BSI_NO_POS_CULTURE`); a pneumonia's own slots where its microbiological test
   is recorded positive; an SSI's own slots where a culture at any depth is recorded positive, whatever the depth its
   infection type records and whatever its follow-up for a secondary BSI, since the form shows them then (the program
   rule `NEOIPC_SSI_SET_AGENTS_IDENTIFIED_FROM_ANY_SITE` sets whether any depth's culture is positive, and the rule
   `NEOIPC_SSI_NO_MIBI_RESULT_AVAILABLE` hides both the slots' section and each slot's fields where none is); and the
   secondary-BSI slots of an NEC, a pneumonia, or an SSI where its follow-up for a secondary BSI is answered Yes, the
   answer that records a secondary BSI. Every infection a numerator counts is thus in its denominator. The program
   rules that decide this hide an SSI's own and secondary-BSI slots, a pneumonia's 1st own slot (its infectious-agent
   field), and a pneumonia's or NEC's secondary-BSI slots field by field, so Tracker Capture blanks such a field's
   value, with a notice, on a form the user can edit, but keeps one on a form it does not let the user edit, such as a
   completed one; they hide a primary sepsis/BSI's slots and a pneumonia's 2nd and 3rd own slots by their section,
   which keeps their values, though a further rule hides a pneumonia's 2nd and 3rd slots field by field too once its
   1st slot is blank. Such a slot does not count either. The item "No positive blood culture" is itself hidden field by
   field once slot 1 names an infectious agent, so that Tracker Capture would blank it on such a form reopened for
   editing; §13 follows the form as stored, as neoipcr's reconciliation does, so where a stored form records both, the
   item stands and the slots do not count.
3. **An infection is microbiologically confirmed** where at least 1 slot that counts names an infectious agent, for the
   infection itself or for a secondary BSI following it. "Not listed" (`Id` 0), "Unidentifiable" (`Id` 3561), and a
   code the list lacks confirm an infection as any other code does.
4. **An infectious agent counts once per infection,** even where both a primary and a secondary-BSI slot record it. It
   is identified by its concept, a synonym resolving to the concept it names, so that 2 slots of 1 infection that
   record 1 code, "Not listed" included, count once.

#### §13.8.2. Rows Over the Reporting Groups and Over the Full Ontology

Each table whose rows are infectious agents comes in 2 versions, both built from what neoipcr bundles from the
released infectious-agent list and its reporting classification:

1. **Over the reporting groups:** the groups of the list's reporting classification, nested as it nests them. Every
   infectious agent of the list but `Id`s 0 and 3561 belongs to exactly 1 leaf group, so this version has no residual
   rows. Above its tree, a summary block gives 1 row per value of the bacteria's Gram reaction and of their oxygen
   requirement, counted as the group rows are; the 2 attributes overlap the groups rather than nest in them.
2. **Over the full ontology:** every level of each infectious agent's path, from its top-level node, Bacteria, Fungi,
   Protozoa, or Viruses, through every concept beneath it, those without an `Id` and the ontology's own groups (such as
   the coagulase-negative staphylococci) included, to the node at which it is recorded; a concept beneath a species (a
   subspecies, a serotype, or a group such as the polioviruses) counts in its species, and a synonym as its concept.
   The list's Eukaryota, which holds the fungi and the protozoa and at which no infectious agent is recorded, has no
   row. Where infectious agents are recorded at a node that has nodes beneath it in the full ontology (a species has
   none, the concepts beneath it counting in it), a residual "n.o.s." (not otherwise specified) row, last among that
   node's children, counts them, whatever rows a dataset holds beneath the node, so that a department's rows match the
   reference's; in a resistance category's block, there is one only where the node has a node of the block beneath
   it. No node is collapsed into its only child, since a department's dataset and the reference would then collapse
   differently and their rows would not match.

The infectious agents detected open with a "Total" row, and the density of infections with resistant infectious agents
opens each category's block with one; the resistance rates per infectious agent have none. In both versions:

- **A row other than a Total counts infections, a parent's included:** an infection counts once in the row of each node
  at or above an infectious agent found in it. Children therefore need not add up to their parent, and the share rows
  can add up to more than 100%, since an infection can hold infectious agents of several nodes.
- **Rows:** a dataset has a row for each node at or above an infectious agent found in it, and the residual and
  summary rows its infectious agents give it (items 1 and 2). In a resistance category's block, the possible rows are
  the nodes on the paths of the infectious agents to which the list applies the category (over the reporting groups,
  with the summary rows of their attribute values; over the full ontology, with the residual rows item 2 gives them),
  and a dataset has each that lies at or above an infectious agent found in it to which the list applies the category
  or that has a result for it. Whether a dataset has a row thus depends only on its infectious agents at or beneath it.
  `get_benchmark_data()` gives every dataset joined into a table, the reference included, each row of the table that it
  lacks, at 0 events over its own denominator of the row, a rate of 0 where it has that denominator and none where it
  lacks it (§13.1.3), with its quartiles empty.
- **Order:** each row's children follow it, siblings by rate, descending, then by label, those without a rate after
  those with one, and a residual row last; over the reporting groups, the summary block stands above the tree, in the
  classification's order of the attributes and their values. `level` gives the depth. `get_benchmark_data()` orders the
  rows as the reference orders them, a row the reference lacks after its siblings, several such siblings in their order
  in the joined table, and a residual row last among its node's children; the joined table follows the order in which
  the datasets are passed, the first dataset's rows first. The reference is the first reference dataset joined into the
  table, or else the first dataset that holds the table, so where the reference dataset's table is not joined (§13.1.7),
  the Partner Report, which passes the department first, shows the table in the department's own order. It lays out the
  summary block attribute by attribute and recognizes a summary row by its attribute, so that a value a later release
  renames stays in its attribute's run; a summary row of an attribute the running neoipcr's classification lacks, which
  only a dataset calculated by another version of neoipcr can hold, is placed as a top-level row of the tree. It takes
  the reference's order of the attributes and of their values from all of the reference's summary rows in the table,
  across the resistance categories' blocks where the table has them: each block holds them in the order of the release
  the reference was calculated from, and the reference's order keeps the order of every block. Where the blocks leave
  that order open, as for 2 attributes, or 2 values, that the blocks order neither directly nor through others, the
  running neoipcr's classification decides it place by place: of the attributes or values that the blocks let come
  next, the first in that classification's order, a value it lacks after those it holds, and several values it lacks in
  the order in which the reference's rows first show them. Since the reference's order is taken table by table, where
  that classification orders the reference's attributes, or the values of 1 of them, otherwise than the release the
  reference was calculated from, or lacks 1 or more of the reference's values of 1 of them, the table of the infectious
  agents detected, whose single block holds them all, shows the release's order, while the 2 resistance tables can show
  in another order 2 of those attributes, or 2 values of such an attribute, wherever the reference's blocks leave the
  order of the 2 open, even 2 that the classification orders as the release does. That classification also places an
  attribute or a value that the reference holds nowhere in the table: such a value follows the value of its attribute
  that the reference places last among those preceding it in that classification's order, or else precedes the
  reference's first value of its attribute, and one that classification lacks too comes last in its attribute's run;
  such an attribute follows the attribute that the reference places last among those preceding it in that
  classification's order, or else precedes the reference's first attribute. Summary rows or attributes that would
  share a place follow that classification's order, whatever the order in which the datasets are passed, and such
  rows whose value it lacks follow their order in the joined table.
- **Outside the tree:** "Not listed", "Unidentifiable", and codes the list lacks count in a table's Total only, and in
  no row of a table without one. Each Total gives in `n_total_only` the number its footnote states, the part of the
  Total that no row of its table or block counts, in the Total's own unit: in §13.8.3, the infectious agents outside the
  tree that the Total counts, each once per infection, as 1 number for all types; in each block of §13.8.5, the
  infections that the Total counts and no row of the block counts, such as one whose resistant infectious agents in the
  category all lie outside the tree, or one whose only value for the category stands on an infectious agent beneath no
  row of the block, such as a fungus (§13.8.7). Every other row leaves `n_total_only` missing.
- **Nothing is hard-coded:** a group appears wherever the classification places it, and a resistance category applies
  wherever the list applies it.

As the reports show them, the 2 resistance tables have the resistance categories as their top rows, each a heading above
its block's rows of either version; in the tables neoipcr returns, every row carries its category in `abr`, and the
blocks follow one another in the order in which §13.8.5 lists the categories. Every calculated dataset carries both
versions of each table. Each function returns 1 version, by an argument whose default is the reporting groups, which the
reports show unless a parameter asks for the full ontology. The result columns are `level`, `rank` (the node's concept
type), `node_id`, `parent_id`, `group` (the row's label), the measures, each share with its numerator and denominator,
the number of departments that enter the row's quartiles (§13.1.6), and `n_total_only`, which only a Total fills, so
that it is missing on every row of the resistance rates per infectious agent, which have none; in the 2 resistance
tables, `abr` precedes them. Over the full ontology, `node_id` is the node's source and its record there
(`<ConceptSource>|<ConceptId>`), a node whose source is the International Committee on Taxonomy of Viruses (ICTV) taking
its ICTV identifier (`IctvId`) in place of its record, so that a row keeps its key across the releases of the ICTV's
Master Species List that the list adopts; over the reporting groups, it is the group's key, and a summary row's is
neoipcr's name of its attribute (`gram_reaction` or `oxygen_requirement`, which no group's key can equal), `|`, and the
value's key as the classification spells it (such as `gram_reaction|positive`); a residual row's is its node's followed
by `~n.o.s.`. A Total row has `level` 0 and no `node_id`, `parent_id`, or `rank`, and a summary row `level` 1 and no
`parent_id` or `rank`; `rank` is missing as well on every group and residual row and where a node has no concept type,
as Viruses has none. The reports find the rows their interpretation texts quote, the group rows in the groups version
and the ontology rows in the ontology version, by `node_id`, and in the 2 resistance tables, where a `node_id` recurs in
each block that has its row, by `abr` and `node_id` together. In the density of infections with resistant infectious
agents, each row also carries the departments and patients that contribute to its pooled figure (§13.8.5).

Every calculated dataset also carries the version and commit of the infectious-agent list it was built from; beside
each table's version, the digests of the parts of the list the table reads (§13.1.7): of the tree of each version (the
tree digests), of the concept each code names (the concept digest), of which infectious agents are common commensals
(the common-commensal digest), and of the infectious agents to which each resistance category applies (the applicability
digest), both as an infectious agent takes its flags (§13.1.7); and the version of the layout of these tables' rows. The
tree digest over the reporting groups covers that version's nodes and paths, each infectious agent's reporting group and
2 attributes, and the keys and names of the attribute values that infectious agents take, but neither a group's category
of the European Centre for Disease Prevention and Control (ECDC) nor an attribute's name, which no table reads, nor the
order of the attributes and their values, which lays out the summary block and which no digest compares (§13.1.7); the
one over the full ontology covers that version's nodes and paths. `get_benchmark_data()` joins one of these tables of 2
datasets only where the version of its rate definitions, the tree digest of its version, the concept digest, and the
layout match, and for a resistance table also the applicability digest (§13.1.7).

The rules of §13.1 apply; a department enters a row's quartiles with at least 5 of the row's infections or detections,
or 50 patient-days for the density over patient-days.

#### §13.8.3. Infectious Agents Detected in Nosocomial Infections

Built by `get_infectious_agent_detection_rate_per_agent_table()`. Beside the column for all infection types together,
each row has a column for each type: primary sepsis/BSI, pneumonia, NEC, and SSI.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Total | infectious agents recorded, each once per infection, primary and secondary-BSI slots together, `Id`s 0 and 3561 and codes the list lacks included | microbiologically confirmed infections, those confirmed by `Id`s 0 and 3561 or codes the list lacks included | per 100 infections, at least 100 | gamma | effective count < 16 | ≥ 5 confirmed infections |
| Row other than the Total | confirmed infections with at least 1 infectious agent of the row's node | confirmed infections with at least 1 infectious agent in the tree, which leaves out an infection confirmed only by `Id`s 0 or 3561 or codes the list lacks | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 such infections, at 0 without the row's infectious agents |
| Type columns | as the row, among the type's infections; in the Total row, infectious agents per 100 confirmed infections of the type | as the row, among the type's infections | % (Total: per 100 infections, at least 100) | Clopper-Pearson (Total: gamma) | effective denominator < 16 (Total: effective count < 16) | none; the column for all types keeps them |

Each row counts infections, as NEO-KISS, the neonatal component of KISS (§13.3), counts the infections per group of
infectious agents, so that a row gives the share of the infections in which an infectious agent of its node was found.
An infection confirmed only by infectious agents outside the tree stays out of the rows' denominators, since it can hold
no infectious agent of any row, and counts in the Total, which counts every recorded infectious agent and exceeds 100
where infections are polymicrobial. Where the SSI table withholds its overall cell, the column for all types, its Total
included, is withheld with the SSI column, since it less the other types' columns would give the SSIs back (§13.10.4).

#### §13.8.4. Microbiological Testing of Nosocomial Infections by Infection Type

Built by `get_infectious_agent_detection_rate_per_inf_type_table()`, with a row for primary sepsis/BSI, pneumonia, and
SSI, and none for all types together, whose share would depend on each department's mix of infection types. Each row
gives 2 shares that answer 2 questions: how often the infection's site was tested microbiologically, which the treating
team decides and the laboratory available to it allows; and how often a test recorded an infectious agent, the yield
of the testing. Neither tells a true from a false result; a department sets its shares beside the reference's to judge
whether a deviation is explained, and by what. The follow-up for a secondary BSI, a matter of surveillance capacity,
enters neither share, so that a lack of it cannot move them: §13.5 gives it beside the secondary-BSI rate, which
counts the infections with an infectious agent found in the blood.

Both reports open their infectious-agent section with figures of §13.8.3's Total and of this table, in place of today's
detection rate over all infection types, which this section leaves out: the infectious agents and the confirmed
infections that the Total counts; for primary sepsis/BSI, `n` of `total`; and for pneumonia and SSI, `total`, `tested`,
and `n`, with both shares, the reference's SSI figures and the Total's left out where the SSI table withholds its
overall cell, the Partner Report keeping the department's own with a note (§13.10.4). The Partner Report gives the
department's figures first, then the reference's. The opening no longer reads the slot `n_infections`, nor does the
surgery paragraph (§13.9.4), so no report reads it; it stays as the dataset's count of infections of each type, NEC and
all types together included, its SSIs and all types together withheld where the SSI table withholds its overall cell.

Its result columns are `inf` (the type); `n` (the positive infections), `tested` (the positive share's denominator),
`pooled` with `ci_lower` and `ci_upper`, and `q1`, `q2`, and `q3` with their intervals; `total` (every infection of the
type, the tested share's denominator), `tested_pooled` with `tested_ci_lower` and `tested_ci_upper`, and `tested_q1`,
`tested_q2`, and `tested_q3` with their intervals; and, for each share, the number of departments that enter its
quartiles (§13.1.6). The reports flag each share on its effective denominator (§13.1.2) without printing the
denominator.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Tested: pneumonia | pneumonias whose microbiological test is recorded positive or negative | every pneumonia | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 pneumonias, at 0 without a tested one |
| Tested: SSI | SSIs whose culture at the depth their infection type records is recorded positive or negative | every SSI | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 SSIs, at 0 without a tested one |
| Positive: primary sepsis/BSI | episodes with at least 1 slot that counts naming an infectious agent | every episode, clinical sepsis included, with or without a blood culture | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 episodes, at 0 without a positive one |
| Positive: pneumonia | pneumonias tested positive with at least 1 own slot naming an infectious agent | the tested pneumonias | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 tested pneumonias, at 0 without a positive one |
| Positive: SSI | SSIs tested positive at that depth with at least 1 own slot naming an infectious agent | the SSIs tested at that depth | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 tested SSIs, at 0 without a positive one |

A blank test or culture counts as not tested; the pneumonia stage makes its test compulsory and the program rules make
the culture at the SSI's depth mandatory, so a blank arises only on a form not completed or one stored without the
client. Primary sepsis/BSI has no tested share, since the form's single item "No positive blood culture" does not tell
a culture never taken from a negative one: the tested share's columns but `total` are `NA` in its row, its number of
departments that enter the quartiles included, and `tested`, its positive share's denominator, counts every episode,
as `total` does. A secondary-BSI slot counts in neither share, and NEC, which has no test of its own and whose
infectious agents are recorded only for a secondary BSI, has no row: §13.5 gives both.

#### §13.8.5. Nosocomial Infections With Resistant Infectious Agents

Built by `get_abr_infection_rate_table()`. Each resistance category, third-generation cephalosporin resistance (3GCR),
carbapenem resistance, colistin resistance, methicillin-resistant *Staphylococcus aureus* (MRSA), and
vancomycin-resistant *Enterococcus* (VRE), heads a block: its Total, then its rows of either version, as §13.8.2 derives
them from the infectious agents to which the list applies the category. MRSA thus covers the *Staphylococcus aureus*
complex, *Staphylococcus argenteus* included, where NEO-KISS, ECDC, and its European Antimicrobial Resistance
Surveillance Network (EARS-Net) count it within *S. aureus* alone, and the reports' methods texts say so; VRE covers
*Enterococcus* and its species. The categories overlap: an infectious agent can meet more than 1.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Density | infections with at least 1 infectious agent of the row's node recorded resistant in the category, each infection once per row; the Total, of any infectious agent | patient-days, as the incidence densities count them | per 1000 patient-days | gamma | effective count < 16 | ≥ 50 patient-days, at 0 without such infections; a department that had detections of the row's infectious agents to which the category applies and recorded a result for none of them enters neither the row's quartiles nor its pooled figure, while one without such detections enters |

Every infant is at risk of an infection with a resistant infectious agent, so the density takes all patient-days, as
NEO-KISS publishes it for multidrug-resistant infectious agents and ECDC does. A department that tested none of its
detections of a row's infectious agents to which the category applies could find no resistance in them, and its 0
would dilute the row's quartiles and pooled density, so it leaves both, its pooled interval, design effect, and count
included, the contributors being counted per row (§13.1.6); a department without such detections enters at 0. The
test is the row's own, its Tested share of §13.8.6, so that an untested "Not listed" detection, which counts in a
block's Total only, takes a department out of no node row, and an untested species cannot leave a department at an
untested 0 in its row; a block's Total tests all of the department's detections to which the category applies.

#### §13.8.6. Resistance Rates per Infectious Agent

Built by `get_infectious_agent_resistance_rate_table()`. Each resistance category heads a block of its rows of either
version, as §13.8.2 derives them. An infection counts once per row: tested where an infectious agent of the row's node
has a result for the category in any of its slots, resistant where one is recorded resistant.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Rate | infections in which an infectious agent of the row's node is recorded resistant in the category | infections in which an infectious agent of the row's node has a result for the category | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 such tested infections, at 0 without a resistant one |
| Tested | infections in which an infectious agent of the row's node to which the list applies the category has a result for it | every infection with an infectious agent of the row's node to which the list applies the category, a blank counting as undocumented | % | Clopper-Pearson | effective denominator < 16 | none |

The Tested share takes only the infectious agents of the row's node to which the list applies the category, as the
completeness of antimicrobial susceptibility testing does, so that an infectious agent the category cannot apply to,
such as a streptococcus beneath the order of *Enterococcus*, does not lower a parent row's share. Counting each
infection's infectious agent once keeps one that a primary and a secondary-BSI slot both record from counting twice. The
rate counts every infection, not only an infant's first isolate as EARS-Net does, so infants with recurrent infections
weigh more, and the methods texts say so. The Tested share beside each row shows how far the rate rests on the testing.
Where the SSI rate withholds its overall cell, the Tested share, with its counts and interval, is withheld in every
block of both versions, since its denominator, over every infection type, less §13.8.3's counts of the other types gives
the SSIs back for a node whose infectious agents all take the category, such as *Staphylococcus aureus* in the MRSA
block (§13.10.4).

#### §13.8.7. Completeness of Antimicrobial Susceptibility Testing

Built by `get_resistance_test_rate_table()`, with a row for each category (column `abr`) and the conditional rows
"carbapenem resistance in case of 3GCR" and "colistin resistance in case of 3GCR and carbapenem resistance" (column
`cond`). Its result columns are `abr`, `cond`, `n` (the detections with a result), `total` (the applicable detections,
on whose effective number (§13.1.2) the reports flag the share without printing it), `pooled` with `ci_lower` and
`ci_upper`, `q1`, `q2`, and `q3` with their intervals, and the number of departments that enter the row's quartiles
(§13.1.6). A detection is an infectious agent in an infection, counted once per infection and tested if either of its
slots has a result.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Each category | detections with a result for the category | every detection of an infectious agent to which the list applies the category, a blank counting as undocumented; `Id` 0, to which the list applies every category, enters every category's denominator, while `Id` 3561 and a code the list lacks enter none | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 applicable detections, at 0 without a tested one |
| Conditional rows | detections with a result for the category | the applicable detections recorded resistant to third-generation cephalosporins, and for colistin also to carbapenems | % | Clopper-Pearson | effective denominator < 16 | ≥ 5 such detections, at 0 without a tested one |

A blank is undocumented, so it stays in the denominator and lowers the share; this holds also where an earlier form
version did not ask for a category that the list applies, so detections entered before the forms followed the list
lower it too. A value recorded for a category that cannot apply to its infectious agent never counts in this table,
since its detection is in no denominator of that category. The engine applies no other filter of applicability, so
such a value counts, wherever its infection is counted, in the density of infections with resistant infectious
agents, its Total included, and in the rate, but not the Tested share, of the resistance rates per infectious agent
(§13.8.6), and gives both tables' blocks the rows at or above its infectious agent that a block can have (§13.8.2); a
logged reconciliation of the data removes it, not the engine. The conditional rows hold only detections recorded
resistant, so that their labels are true; an infectious agent to which 3GCR does not apply, such as *Acinetobacter*,
counts in the plain rows only. The same share per department decides whether a department enters the Total of a
category's block of the density of infections with resistant infectious agents, and its other rows test their own
(§13.8.5). Where the SSI rate withholds its overall cell, every figure of this table is withheld, since its applicable
detections, over every infection type, less §13.8.3's counts of the other types nearly give the SSIs back, `Id` 0
entering every category's denominator but no row of §13.8.3 (§13.10.4).

### §13.9. Surgical Procedures by Procedure Group

The table shows, overall and per procedure group, how many patients are operated on and how often, 2 measures that
procedures per patient would mix. `get_surgery_rate_table()` builds it for a dataset, and `get_ref_surgery_rate_table()`
adds the departments' quartiles and their intervals (§13.1.4) for the reference; `calculate_department_data()` and
`calculate_reference_data()` carry it as `surgery_rate_table`, 1 row per group keyed by `pro_cat`, `"overall"` first.

#### §13.9.1. What Counts as a Surgical Procedure

A surgical procedure makes at least 1 incision through skin or mucosa (laparoscopic or thoracoscopic ports and burr
holes included), re-enters an operative wound, or closes a stoma or an open body-wall defect, sutured or sutureless,
wherever it takes place, the cot side included. A frenotomy counts, and so does a drain placed through an incision, in
any cavity. The World Health Organization's International Classification of Health Interventions (ICHI) files the tube
thoracostomy under percutaneous pleural drainage (`JCB.JB.AE`), so every procedure so coded counts, needle drains among
them. Vascular catheters and ports never count, however placed, as NHSN's operative-procedure mappings hold no central
venous access, so an infection of a port pocket or tunnel falls outside SSI surveillance; nor do other needle
punctures, endoscopy and dilatation without an incision, laser or cryotherapy, imaging, dressings, the care of devices,
or education. The definition applies to every record, those collected before it included, and the surgery table, the
Reference Report's surgery paragraph, the dataset's surgical counts, and the SSI rate share it.

#### §13.9.2. The Procedure Groups

Each surgical procedure falls in 1 of 8 groups by body site and surgical team, which the table lists in this fixed
order, cavities before the body wall and superficial sites:

1. neurosurgery;
2. cardiac and great-vessel surgery, including arterial duct closure, whoever performs it;
3. non-cardiac thoracic surgery, including diaphragmatic hernia repair by either approach and chest drains;
4. abdominal surgery;
5. abdominal wall surgery: gastroschisis, silos, and the repair of the wall;
6. inguinal and umbilical hernia repair;
7. head, neck, and upper airway surgery;
8. skin, soft tissue, perineal, and urogenital surgery.

The groups are defined by ICHI's axes rather than by lists of codes, so that a valid code entered for the first time is
placed by the same rules as the recorded ones, in a group or among the codes that are no surgical procedure, or else is
a code that no group takes (below). No version of ICHI is pinned: the groups take every code recorded under any of its
states, each mapped to its current entity where one exists. Their placement of the codes, each with its group, the tract
it opens, and whether it is an approach code, together with the codes that are no surgical procedure, is part of the
definitions this section versions (§13.1.7), the recorded codes being its test cases. This document does not hold the
placement: the change that implements the groups adds it to neoipcr, beside `get_procedure_category()`, as a table of
every recorded code with its group, the tract it opens, and whether it is an approach code. It places the oesophagus in
the chest: an oesophageal atresia repair is non-cardiac thoracic surgery (group 3). The approach codes recorded so far
are `MAA.FA.AA` in neurosurgery (group 1), `JCH.AE.AA` and `JCH.FA.AA` in non-cardiac thoracic surgery (group 3), and
`PAK.AE.AA`, `PAK.AE.AB`, and `PAK.FA.AA` in abdominal surgery (group 4). Of the codes of percutaneous drainage, only
the pleura's counts (§13.9.1), and that of any other site, the abdomen's (`PAK.JB.AE`) and the peritoneum's
(`KMA.JB.AE`) included, is a needle puncture and no surgical procedure, a drain placed through an incision being coded
as open drainage, such as the peritoneum's (`KMA.JB.AA`).

A procedure counts once, by all its codes, whichever of them was entered as the main code:

1. codes that are no surgical procedure drop out, and a procedure left with none is no surgical procedure;
2. an approach code (laparotomy, laparoscopy, thoracotomy, thoracoscopy, craniotomy) gives way to any operation done
   through it whose code a group takes, as ICHI's coding notes say to omit the approach beside the operation;
3. the procedure joins the first group, in the order above, that one of its remaining codes places it in.

A procedure whose only codes that a group takes are approach codes counts in the approach's group, its definitive
operation unknown. `KMA.FA.AA`, an operation on the peritoneum that is no approach code and also covers the revision of
a ventricular shunt's peritoneal end, is abdominal surgery unless the procedure holds a shunt code. A procedure none of
whose codes a group takes, such as one holding only a code that is no valid ICHI code, leaves the surgery table, the
dataset's surgical counts, and the SSI rate's denominator until its codes are corrected, and §13.10.1 states what it
does to the attribution of an SSI. Validation rules report, as warnings that remove no record, a procedure that holds
only an approach code, a code that no group takes or whose group depends on what was done, such as `KMA.FA.AA`, a
percutaneous drainage of the peritoneum (`KMA.JB.AE`), since a primary peritoneal drain placed through an incision may
have been coded so, and a procedure recorded as wound class I although a code of its group opens a tract. A code that is
no valid ICHI code fails validation as an error, which removes its patient record (§13.1.5), so that for such a code the
rule on a procedure whose codes no group takes decides only where an exception or `include_invalid_patients = TRUE`
keeps the record.

#### §13.9.3. Measures

The rows are "Overall" and the 8 groups, each listed even where it is small or empty, its small cells flagged rather
than pooled: a group in which a dataset has no surgical procedure shows 0 patients operated on, and no operations per
patient operated on, since it has no patient operated on.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Patients operated on | patients with at least 1 surgical procedure of the group ("Overall": of any group) | all patients | % | Clopper-Pearson | effective denominator < 16 | departments that operated in the period, with ≥ 5 patients, at 0 where they operated on none in the group |
| Operations per patient operated on | surgical procedures of the group | patients with at least 1 surgical procedure of the group | per patient, at least 1 | gamma | effective count < 16 | ≥ 5 patients operated on in the group |

The groups' procedures add up to the overall row's, since each surgical procedure falls in 1 group; their patients
operated on need not, since a patient can be operated on in several groups. The pooled rates take every department,
those that do not operate included. Only departments with at least 1 surgical procedure in the period enter the
quartiles, their intervals, and the display test, since a department that transfers its patients elsewhere for surgery
is not at risk of operations performed in it; the methods texts state how many departments operate.

#### §13.9.4. The Surgery Paragraph and the Dataset's Surgical Counts

The Reference Report's surgery paragraph quotes the table's measures and nothing else: the departments that operated in
the period among the dataset's departments, and their share; and, from the overall row, the patients operated on at
least once among all patients, their share, and the operations per patient operated on. It applies the one definition
of a surgical procedure above and joins no antibiotic data, so a patient operated on counts whether or not the stay
records an antibiotic. The dataset's slots `n_surgical_departments`, `n_surgical_patients`, and
`n_surgical_procedures`, in both `calculate_reference_data()` and `calculate_department_data()`, take their totals from
the table: the departments with at least 1 surgical procedure, and the overall row's patients operated on and surgical
procedures. In `calculate_reference_data()`, their per-department mean and quartiles take every department, one that
does not operate at 0, each counting its patients operated on and surgical procedures under the same definition, with
no antibiotic join. The surgery paragraph replaces today's, which says that SSI rates are not calculated; §13.10 gives
them.

### §13.10. Surgical Site Infections per 100 Surgical Procedures

The rate counts the SSIs that follow a surgical procedure per 100 surgical procedures, overall and for groups of similar
procedures, crude and adjusted for nothing, as the NeoIPC Core Protocol defines it but for the points that §13.10.1 and
§13.3 state with their reasons, which the reports' methods texts say: the rate leaves out an SSI present at surgery and
a procedure on the date of death, which the protocol's formula keeps; it follows a procedure's window into a
readmission, where the protocol ends the follow-up at transfer and discharge; a revision on any date but that of death
ends every earlier window, where the protocol ends only that of a procedure in the same area; a procedure whose codes no
group takes (§13.9.2), a revision included, leaves the denominator and opens no window, since its codes cannot say
whether it was a surgical procedure or in which group, and, on any date but that of death, it takes out of the rate an
SSI that its window would hold where it falls on or after the date of the most recent surgical procedure whose window
holds that SSI, where the protocol counts the SSI with a recorded procedure whose follow-up holds it; and an SSI's onset
is judged by its window alone, where the protocol states the cut-offs of day of life 4 and day 3 of the stay for every
infection it records, SSIs included. The rate is not comparable with NHSN's crude SSI rate: NHSN counts only the
operative procedures of its code mapping done in an operating room (Patient Safety Component Manual, ch. 9, January
2026, p. 9-4), its crude rate only those closed primarily (p. 9-28), and it counts SSIs found after discharge, where
this rate counts every surgical procedure of §13.9.1, cot-side ones and those not closed primarily included, and only
the SSIs seen in hospital (§13.10.1), which the methods texts say. No report gives it today; both reports print it, on
by default, its overall row and depths alone unless a parameter adds the groups and strata (§13.10.2).
`get_ssi_rate_table()` builds it for a dataset, and `get_ref_ssi_rate_table()` adds the reference's quartiles and
suppression; `calculate_reference_data()` carries it as `ssi_rate_table`, its cells suppressed within the dataset as the
minimum cell (§13.10.4) requires, and `calculate_department_data()` carries the department's own counts and rates,
unsuppressed, which the Partner Report compares with the reference's. The per-class reports print no SSI table
(§13.11.2).

#### §13.10.1. Numerator, Denominator, and Attribution

- **Counted:** the SSIs attributed to a surgical procedure of the row, each counting. An SSI already present at the time
  of surgery (`NEOIPC_SSI_INFECTION_PRESENT`) leaves the rate together with its procedure, since the procedure did not
  cause it, and any other SSI attributed to that procedure leaves with it; where 2 or more candidate procedures of one
  date take the SSI (below), all of them leave, with every SSI attributed to any of them. A column beside the rate gives
  the number of SSIs present at surgery. The protocol's formula keeps such an SSI and its procedure; §13 leaves both
  out, as the standardized infection ratios of NHSN's 2015 and 2022 baselines do (NHSN Patient Safety Component Manual,
  ch. 9, January 2026, p. 9-29), although NHSN's crude SSI rate per 100 procedures keeps them (p. 9-28).
- **Over:** the surgical procedures of the row, each counted once, by all its codes, in the group the rule of §13.9.2
  gives it. A procedure on the date of the surveillance-end form that records the patient's death
  (`NEOIPC_SURVEILLANCE_END_REASON`, Death) leaves: NHSN leaves a procedure in which the patient dies in the operating
  room out of its denominator (ch. 9, p. 9-27), the forms record dates without times, and the same date also catches
  surgery on a patient already deceased, which the protocol leaves out. The protocol's formula keeps a procedure
  followed by death. Revisions and procedures not closed primarily count.
- **Attribution:** the SSI form names no procedure, so an SSI is attributed by date to the most recent recorded surgical
  procedure of its patient record, in its own stay or an earlier one, whose follow-up window holds the SSI's date, the
  procedure's date being day 1. The window is 30 days, or 90 days for a deep incisional or organ/space SSI after a
  surgical procedure that left an implant. A procedure recorded as a revision (`NEOIPC_SURGERY_REVISION_PROCEDURE`)
  ends, on its date, the window of every earlier surgical procedure of the patient record and opens its own: the form
  names neither the procedure revised nor its area, which the protocol's "revision in the same area" would need. A
  revision whose codes no group takes (§13.9.2), on any date but that of death, ends the earlier windows as well and
  opens none. A record whose codes are no surgical procedure (§13.9.1), such as a catheter's insertion or a needle
  puncture, neither ends nor opens a window. A procedure on the date of a surveillance-end form that records the
  patient's death, whatever its codes and a revision included, is absent for attribution: it ends no window, voids no
  attribution, and takes no SSI but one recorded as present at its own surgery, which leaves with it (Counted); any
  other SSI of that date goes to the most recent earlier surgical procedure whose window holds it, as NHSN, which
  completes no denominator form for a patient who dies in the operating room, cannot attribute an SSI to such a
  procedure (ch. 9, pp. 9-20 and 9-27). An SSI that another surgical procedure's window also holds goes to the most
  recent all the same, and the validation reports it as a warning, since the form cannot say which procedure opened the
  infected site, as the definition of an organ/space SSI requires; a group can thus take an SSI of another group's
  earlier procedure, a frenotomy or a chest drain included, while the overall rate is unaffected, which the methods
  texts say. Where 2 or more surgical procedures of the most recent date hold the SSI in their windows, these candidates
  take it once: in their group's row where they share a group, and otherwise in the overall row alone, and in a
  stratum's row only where they share the stratum, otherwise in their group's total alone; the validation reports this
  too. An SSI's eligible procedure is the surgical procedure, or the candidates of 1 date, to which these rules
  attribute it and which the next sentence does not void. An SSI within the window that a procedure whose codes no group
  takes, on any date but that of death, would open has no eligible procedure where that procedure falls on the date of
  the most recent surgical procedure whose window holds the SSI, or later, even though that surgical procedure's window
  holds it too. Such an SSI, and any other that no eligible procedure's window holds, leaves the rate, as the protocol
  leaves out an SSI after surgery elsewhere, although here a surgical procedure may be recorded (§13.10). The validation
  reports as an error an SSI that no procedure form of its patient record holds in the window that the form's date and
  implant flag would give it were it a surgical procedure, whatever its codes and whatever revision follows it, and the
  error leaves that SSI out of every table and count, its patient record staying (§13.1.5). It reports as a warning,
  which removes nothing and takes the SSI out of the SSI rate alone, any other SSI without an eligible procedure, such
  as one held only in the windows of procedures that no group takes, that are no surgical procedure, or that fall on the
  date of death, or in windows that a revision ended; such an SSI counts in the other tables that count SSIs (§13.5,
  §13.8) and in `n_infections`, for its own stay (§13.1.5).
- **Observation:** death ends the follow-up, while a readmission of the patient record continues it within the window,
  since an SSI belongs to its procedure's follow-up, which can run past a discharge into a readmission; the protocol
  ends the follow-up at transfer and discharge as well, which the reports' methods texts say. An SSI that a later stay
  records counts with the procedure it is attributed to, in the period and for the department of the stay that holds the
  procedure (§13.1.5), so that a period's numerator and denominator hold the same procedures. Attribution, and the
  validation of an SSI against the procedure forms of its patient record, read every stay of the patient record, those
  of other periods and those not yet completed included, and the period narrows the dataset only after them, a finding
  of that validation removing no stay of any period, while the rest of the validation pass judges the period's stays
  after the narrowing, so that an error on a form of another period removes none of them; an SSI not yet recorded when
  its procedure's period is calculated is missing from that period, which the methods texts say. The rate thus counts
  the SSIs seen in hospital, in the procedure's stay or a later one, and how much of the window a department observes
  depends on how long its patients stay after surgery and whether they return, which the methods texts say.

#### §13.10.2. Rows

1. **Overall.**
2. **By depth, overall only:** superficial incisional, deep incisional, and organ/space SSI, by the SSI form's infection
   type (`NEOIPC_SSI_INFECTION_TYPE`), each over the overall row's procedures, adding up to the overall rate. The groups
   show their total only. By default the reports print rows 1 and 2 alone; rows 3 and 4 are built, tested, and stored
   and print on a parameter, as the tables of §13.8.5 to §13.8.7 do, since while the network's SSIs are few they would
   flag every row and leave most groups and strata pooled or withheld.
3. **Per procedure group:** its total, the groups below the minimum being pooled into 1 row that names them (§13.10.4).
   The groups' SSIs need not add up to the overall row's, since an SSI that candidates of 2 groups share counts in the
   overall row alone (§13.10.1), even where both groups are in the pool; the difference is printed nowhere on its own.
4. **Per infection-risk stratum within a group,** shown only where its cell reaches the minimum, by the surgery form's
   wound class, primary closure, and implant (`NEOIPC_SURGERY_WOUND_CLASS`, `NEOIPC_SURGERY_PRIMARY_CLOSURE`,
   `NEOIPC_SURGERY_IMPLANT`):
   1. clean: wound class I, closed primarily, no code of the group opening a tract, and no implant;
   2. clean with an implant: as stratum 1, with an implant recorded;
   3. clean-contaminated: class II, or class I where a code of the group opens the alimentary, respiratory, genital,
      or urinary tract;
   4. contaminated or dirty: class III or IV.

   A class I procedure not closed primarily, and without a code of its group opening a tract, counts in its group's
   total and in no stratum, a footnote giving their number where they reach the minimum and no stratum of the group is
   withheld (§13.10.4), so the strata need not add up to the group. The surgery form requires all 3 items, so the strata
   and these procedures together make up the group.

#### §13.10.3. Measure

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| SSI rate | SSIs attributed to a surgical procedure that the row's denominator holds | surgical procedures of the row, those on the date of a surveillance end that records death and those to which an SSI present at surgery is attributed left out | per 100 procedures | gamma | effective count < 16 | the overall rate only, ≥ 5 procedures; none for the depths, groups, or strata |

The interval is the gamma interval of a ratio (§13.1.4) because the rate counts SSIs, not procedures with an SSI: every
SSI attributed to a procedure of the denominator counts, as the protocol's "Total SSI Cases" do, and a procedure can be
followed by more than 1, which a binomial interval over the procedures, such as Clopper-Pearson's, cannot hold. The rate
is a ratio of SSIs to procedures, so its flag tests the effective count of SSIs. The table prints the SSIs, the
procedures, the rate with its interval, and the SSIs present at surgery; it prints the procedures, although the tables
otherwise leave denominators unprinted, since every openly published cell that holds procedures holds at least 16 of
them.

#### §13.10.4. Minimum Cell

An openly published cell that holds procedures holds at least 16 of them from at least 3 departments, none contributing
more than half of them, and the departments beyond the 2 largest together contributing at least 10% of the largest one's
procedures, the p% rule of statistical disclosure control at p = 10 (Hundepool A, Domingo-Ferrer J, Franconi L, et al.
*Statistical Disclosure Control*. Wiley; 2012. doi:10.1002/9781118348239), so that the cell's SSIs come from enough
departments that neither of the 2 largest can attribute the others' SSIs to 1 department. The SSI rate alone holds its
cells to a minimum, where the other rates flag a small figure (§13.1.2), since its cells are small by design: it splits
few procedures by group, stratum, and depth, so many of its cells come from 1 or 2 departments, while every other rate
is a row over the whole network to which most departments contribute, where a row from 1 or 2 departments is rare and
its flag shows it. Where the largest department holds 10 or fewer procedures, any further department meets the 10%, so
the p% test withholds no cell that the other conditions pass below 22 procedures (11, 10, and 1 from 3 departments). The
SSIs themselves, which the rule does not test, one department can bound for another only to within those of the
remaining departments, which may be none: a cell of 3 departments leaves each the others' sum, whatever the rule. This
is accepted, and the methods texts say so, since a test of the SSIs too would withhold most cells, whose few SSIs come
from 1 or 2 departments. A cell below the minimum is suppressed, its counts with its rate, and further cells are
suppressed wherever a total would give a suppressed one back, until the suppressed cells meet the minimum together.
Where the overall cell is suppressed, the report and the reference dataset also withhold every figure that counts the
SSIs apart or that, over all infection types, gives them back less the other types: the SSI part and the Total's figures
of the infectious-agent section's opening (§13.8.4), the SSI rows and columns of §13.5, §13.8.3, and §13.8.4, §13.8.3's
column for all types with its Total, the Tested share of §13.8.6, with its counts and interval, in every block of both
versions, every figure of §13.8.7, and `n_infections` for SSIs and for all types together. The Tested share's
denominator and §13.8.7's applicable detections count every type, so less §13.8.3's counts of the other types give the
SSIs back, exactly for a node whose infectious agents all take the category, as for *Staphylococcus aureus* in the MRSA
block, and nearly in §13.8.7. The other types' columns still print, and so do §13.8.5 and the rates of §13.8.6, since no
printed figure gives their SSI part back; the Partner Report shows the department's own figures where the reference
withholds them, without reference figures, with a note. The procedures stay printed in the surgery table (§13.9), so the
rule protects the SSIs, not the procedures. A withheld figure is stored as withheld, which `get_benchmark_data()` and
the Partner Report keep apart from one the reference lacks. A cell is empty only where it has no surgical procedure
before those on the date of death and those with an SSI present at surgery leave: an empty cell needs no protection,
counts as no suppressed cell, makes no other cell suppressed, and joins no pool, an empty group showing 0 procedures and
no rate, while a cell whose procedures all left the denominator counts as a cell below the minimum, its SSIs present at
surgery withheld with it.

The groups below the minimum are pooled into 1 row: while the pool misses the minimum, a further group with procedures
joins it, the one with the fewest procedures among those with which it would meet it, or, where none would, the one with
the fewest, ties going by the order of §13.9.2, until the pool meets the minimum or holds every group with procedures; a
single group below the minimum takes a partner the same way. A group in the pool shows no strata and no remainder
footnote, since its strata are rows beneath its own row, which the pool replaces. Where the pool holds every group with
procedures, it prints no row of its own, since it would repeat the overall row's procedures and, beside it, give back
the SSIs that candidates of 2 groups share (§13.10.2); the overall row prints, with its depths, where it meets the
minimum, and an empty overall row or group shows 0 procedures and no rate.

A stratum appears only where it reaches the minimum. A group's procedures that fall in no stratum (§13.10.2) form a cell
too, whose footnote gives their number only where they reach the minimum and no stratum of the group is withheld; where
a stratum is withheld, the footnote is withheld with it, and the remainder counts among the withheld cells. Where the
remainder, with any strata withheld beside it, misses the minimum, further strata with procedures are withheld, the one
with the fewest procedures first among those with which the withheld cells would meet the minimum together, or, where
none would, the one with the fewest, ties going by stratum number, until they meet it or every stratum with procedures
is withheld. Each department sees its own counts, unsuppressed, in its Partner Report.

### §13.11. Case-Mix Figures, Birth-Weight Classes, and Gestational-Age Ranges

#### §13.11.1. Distributions of Birth Weight and Gestational Age

The internal `get_birthweight_figure_data()` and `get_gestational_age_figure_data()` fill the slots
`birth_weight_figure` and `gestational_age_figure` of both datasets: the frequency per bin, a density curve, the
quartiles and mean, and the axis range. Each figure shows a distribution, not a rate.

| Measure | Counted | Over | Scale | Interval | Flag | Quartiles |
| --- | --- | --- | --- | --- | --- | --- |
| Birth weight | patient records per 50 g bin | patient records with a recorded birth weight | count per bin, with a density curve from 10 patient records | none | none | none; the figure marks the patients' Q₁, median, Q₃, and mean from 10 patient records |
| Gestational age | patient records per completed week | patient records with a recorded gestational age | count per bin, with a density curve from 10 patient records | none | none | none; the figure marks the patients' Q₁, median, Q₃, and mean from 10 patient records |

Each patient record with a stay whose surveillance ended in the period counts once, whatever its number of stays; in
the reference, a patient registered by 2 departments counts twice.

A 50 g bin is centred on a multiple of 50 g, so that the 1500 g bar holds 1475 to 1524 g: recorded birth weights heap at
the class boundaries that reimbursement draws, an infant being recorded on the side of a boundary that pays more, and
bins centred on the round values even that heaping out. Lines at 500, 1000, and 1500 g mark the classes' boundaries, a
bar on a line holding infants of both classes, so that the first and last bars of a birth-weight class's report hold
only its class's infants, which the legend says. A 1-week bin holds a completed week, the 26-week bar holding 26+0 to
26+6, as the protocol counts gestational age and its eligibility (below 32 weeks, up to 31+6), so that the bars of a
gestational-age range's report hold only its range's infants. The marks and the density curve print only where at least
10 patient records contribute, so that none gives an infant's own birth weight or gestational age back, a curve over few
infants peaking at each one's value; the binned bars always print.

#### §13.11.2. Birth-Weight Classes and Gestational-Age Ranges

The dataset options `birth_weight_from` and `birth_weight_to` of `dhis2_dataset_options()` bound the birth weight
inclusively, in grams, and `gestational_age_from` and `gestational_age_to` the gestational age inclusively, in completed
weeks, so that `gestational_age_to = 31` keeps 31+6; a patient without a recorded birth weight falls outside every
birth-weight range, and one without a recorded gestational age outside every gestational-age range. A range narrows the
whole dataset, so every table and figure of a report built from it counts only the patients within it; no report
stratifies within itself, and none adjusts for birth weight, days of life, or lengths of stay or calculates the NeoIPC
Core Protocol's standardized infection rate (version 1.2, section 6.2.4), which the methods texts say. Beside the
overall Reference Report stand those of 2 of the protocol's 4 birth-weight classes and of 2 ranges of gestational age,
each with a reference dataset of its own:

| Report | Patients | Range |
| --- | --- | --- |
| Overall | every recorded birth weight | none |
| "500–999 g" | birth weight 500 to 999 g | `birth_weight_from = 500`, `birth_weight_to = 999` |
| "1000–1499 g" | birth weight 1000 to 1499 g | `birth_weight_from = 1000`, `birth_weight_to = 1499` |
| "24–27 weeks" | gestational age 24+0 to 27+6 | `gestational_age_from = 24`, `gestational_age_to = 27` |
| "28–31 weeks" | gestational age 28+0 to 31+6 | `gestational_age_from = 28`, `gestational_age_to = 31` |

The protocol's classes below 500 g and of 1500 g or more hold too few patients for a reference of their own, so no
report is published for them, which the methods texts say, and a Partner Report over either compares with the overall
reference, its header noting that the birth-weight filter differs from the reference's. A patient without a recorded
birth weight leaves every report, the overall report included, which states their number (§13.1.5), so that no report
gives their figures back and no report withholds a row for them. `calculate_reference_data()` takes any bounds, but only
the per-class reports in the table above withhold rows as below; no reference dataset or Reference Report over other
bounds is published, and the reporting service shows such a dataset to its administrators alone, who may explore the
reference over any bounds.

A report of a class or a range, a per-class report below, is the same Reference Report, rendered with its range and a
preset of its table parameters. It prints the overview, without the departments' quartiles of patient records and
patient-days (§13.2), the case-mix figures (§13.11.1), and the tables of §13.3, §13.4, and §13.6, and leaves out those
of §13.5, §13.7, §13.8 with its opening, §13.9, and §13.10, as NEO-KISS's reference data give the infectious agents and
resistance over all classes; its header names its class or range. It compares a department with patients like its own at
the price of statistical power: with fewer departments and patients, its intervals widen, more quartiles fail the
display test, and more rows carry the flag or are withheld, which the methods texts say. It prints a pooled figure only
where at least 3 departments contribute to the figure's denominator, none contributing more than half of it and the
departments beyond the 2 largest together at least 10% of the largest one's part (§13.10.4), since a pooled figure from
1 or 2 departments shows their own rates, and so that neither of the 2 largest can attribute the others' events to 1
department. Where the departments with days of a device or factor, or those with patients exposed to it, fail this rule,
the report withholds every figure of it, its associated infection rate (§13.4), its exposure rate (§13.6.1), and its
share of patients exposed and mean days (§13.6.2), since these give one another back, and the smallest further row that
would give it back, such as INV or any ventilatory support beside NIV. A withheld row withholds every figure it prints,
its counts, rate, interval, and quartiles with the note on their zeros, and its numbers in the contributors' statement
(§13.1.6), and a text that quotes it follows it, as the opening of §13.3 does.

The birth-weight classes divide the overall report's patients into the 2 published classes and the rest, below 500 g or
of 1500 g or more, and the gestational-age ranges into the 2 published ranges and the rest, below 24 weeks, of 32 weeks
or more, or without a recorded gestational age. Within each division, the overall report's row, less the printed parts',
gives back the sum of the parts that do not print it, the rest always among them, so these must together meet the rule
that withholds a row, and a per-class report that withholds a row is never the only part not printing it where another
part has a denominator of the row. Where they do not meet the rule together, or a per-class report would be the only
part, a further per-class report of the division with a denominator of the row withholds it, the one with the smallest
denominator among those with which they would meet it, or, where none would, the one with the smallest denominator, ties
going by the order in the table above, until they meet it or every per-class report of the division with a denominator
withholds it; for a device or factor withheld as a whole, the further reports are chosen once, by its days, and each
withholds every figure of it and the same further rows, so that no report prints part of it. A row whose denominator is
0 in a part counts as no withheld row and makes no other part withhold it, since it shows no department's figures, so a
part that alone has a denominator of a row withholds it alone. Each division is decided on its own, since the reports of
the one are no sums or differences of the other's. The overall report holds its own rows to no such rule: it prints a
row that every per-class report of a division withholds, flagged where small, keeping the rule of flagging small cells
rather than suppressing them that its rates follow, the SSI rate's minimum cell (§13.10.4) aside, so the withholding
protects only the split among the parts that have a denominator of the row, a part without one showing that it holds
none of it, which the methods texts say.

`calculate_reference_data()` over the whole range decides from its import, which holds each department's part of each
row's denominator in every class and range, which rows each per-class report withholds, and stores the list, by table,
row, and report and naming no department, in the overall reference dataset, from which each per-class run takes it. Each
per-class run stores the rows its report withholds as withheld, and only the tables its report prints, with its own
counts of §13.2, its `n_severe_infection_patients`, stored as withheld where its report withholds the severe-infection
row, and its case-mix figures (§13.11.1), so that a report rendered from its dataset shows what its PDF shows, and its
dataset is uploaded beside the overall one, so that a department whose patients differ systematically from the network's
can compare with a reference over a range like its own. `get_benchmark_data()` keeps a reference row stored as withheld
apart from a row the reference lacks: the Partner Report prints the department's figures beside a note that the
reference withholds the row, and a table that the reference lacks without reference columns, with a note saying why.
