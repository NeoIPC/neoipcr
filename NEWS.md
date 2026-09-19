<!--
Version headings follow R's convention — `# neoipcr <version>`, one H1 per released
version — rather than the `## [x.y.z]` shape the other NeoIPC products use, because
this is a `NEWS.md` that R tooling reads.

The release workflow extracts the section whose heading matches the version being
released — the tag's, which its verify job has already checked against DESCRIPTION —
and publishes it as the GitHub Release body; the pull-request check extracts the
section for DESCRIPTION's version. So a release cannot be cut for a version this
file does not describe. When bumping DESCRIPTION, rename `(development version)` to
the new version in the same commit and open a fresh `# neoipcr (development version)`
section above it for the next changes.
-->

# neoipcr (development version)

# neoipcr 0.0.0.9002

* A dataset carries the validation pass's findings in `validationResults` and their counts in
  `validationSummary`: per rule, the distinct records the import removed and the ones the exception
  list exempted from the rule, at the rule's record kind, and a totals row per record kind counting
  the records the findings concern — the `patients` row is the number of patients the pass removed.
  Both slots exist on every dataset and are empty when the pass does not run.
  `calculate_department_data()` and `calculate_reference_data()` carry the summary and refuse a
  dataset without it, and `get_benchmark_data()` carries each dataset's summary under its name beside
  its metadata, so a report can state what its data rests on.
* A calculated dataset's options are fit to leave the package: an exception list is replaced by the
  marker `"exception_list_applied"`, and reference data replaces its department filter by `"applied"`,
  since the list carries patient ids and enrolment dates and the filter names the departments behind
  the reference values. Both calculation functions refuse to emit a data frame in the options, and
  `calculate_reference_data()` a department filter. The `redact` argument of
  `calculate_reference_data()` is gone with the replacement it switched.
* The rule registry declares the context fields each rule records, `validation_rule_context_fields()`
  exports them, and `validate()` refuses a finding whose fields differ from the declaration, so a
  consumer's templates are checked against the package rather than against a copied table.
* Rule 16 is removed. It reported a surgical site infection form dated outside the time frame of its
  enrolment, but a surgical site infection is attributed to its procedure's follow-up period, which may
  run past the discharge and into a readmission — an infection date on or before the admission of the
  enrolment it is recorded in, or after that enrolment's surveillance end, is legitimate as long as a
  recorded procedure covers it, which rule 19 checks across a patient's enrolments. The rule ids now
  run from 1 to 42 with a gap at 16; an exception list naming rule 16 is refused as naming an unknown
  rule.
* `validate()` runs every one of the 41 validation rules, ported from the Validation Report's
  implementation, which had been the only complete one. Rules 19, 20, 21, 27, 28, 30–37 and
  39–42 were placeholders that flagged nothing; rules 22–24 read a code list from a file the
  package cannot rely on and now check the ICHI code grammar with `is_valid_ichi_code()`; rule 17
  now enters an enrolment without a surveillance-end event into the overlap comparison as under
  surveillance on its enrolment date, so it is found when that day falls inside another enrolment's
  period or coincides with another open enrolment's start; rule 19 counts the procedure date as day
  1 of the follow-up window, as the protocol does, so an infection on the procedure day is inside
  the window and one 30 (or 90) days later is the first outside it, where the report's rule had
  started the window the day after the procedure, it reads an implant flag that was not
  recorded as no implant, and it places an infection whose type was not recorded by the windows
  its implant flag allows rather than outside every window; and rules 7–11 flag an open infection or
  surgery form whenever the enrolment *or* its surveillance-end form is completed, keyed on the
  form's event. Every rule is exempted through the key its finding is recorded on.
* `validate()` returns no prose. A finding's `context` is a one-row tibble of the values the rule
  compared, named as the "Context fields" section of `?validate` lists them; the sentence a reader
  sees belongs to the document that renders the finding, where it is written and translated. The
  rule descriptions the package carried as unused message-catalogue entries are gone with the
  formatters that held them.
* `validate()` accepts the exception list in the form a user writes it as well as in key form, and
  aborts on a rule id it does not know instead of running nothing. New exports:
  `validation_rule_ids()`; `read_validation_exceptions()`, the CSV reader with the shape checks
  `import_dhis2()` applies to `include_invalid_patients`; and `resolve_validation_exceptions()`,
  the mapping of a list onto a dataset's keys, which also works on a returned dataset. A record is
  written at the level of the rule it names (the patient alone, the enrolment, or an event of a
  type the rule concerns) and is refused otherwise; it resolves as a whole — one whose enrolment
  or event is not in the dataset exempts nothing — and is matched within its department whenever
  the dataset carries the department codes. A `DEPARTMENT_CODE` column left empty throughout, the
  single-department list in the six-column shape, counts as absent.
* A removed patient's free-text pathogen names no longer survive in `unknownPathogenNames`: the
  post-import cascade prunes them with the findings they belong to, whether the patient was removed
  by the validation pass or by a filter.
* `validate()` names the selected rules it could not run, for want of a column the dataset lacks, in
  the result's `rules_skipped` attribute, so a consumer stating which rules a result rests on can
  tell a rule that found nothing from one that never ran.
* `import_dhis2()` refuses an instance that does not carry exactly one NeoIPC Patient tracked-entity
  type, naming whether it is missing or duplicated. An import of the unenrolled patients requests by
  that type; without it the request carried neither program nor type, which DHIS2 refuses with a
  message that names neither the option nor the missing type.
* `include_invalid_patients = TRUE` now keeps the enrolments without an admission form in the returned
  dataset. The orphan removal that follows an import dropped them as a dataset invariant before a
  `validate()` on the returned dataset could report them under rule 26, so a consumer that skips the
  pass in order to list the records it would remove never saw those; the invariant still holds for
  every import that runs the pass.
* `include_unenrolled_patients = TRUE` now keeps the patients with no enrolment in the returned
  dataset when enrollments are imported as well: the orphan removal that follows an import leaves them
  in place, where it used to prune them with the enrollments they never had, so a `validate()` on such
  a dataset can flag them under rule 1. Only the patients that arrive without an enrolment are kept —
  one that loses its enrolments to a filter is pruned as before — and they reach the dataset only
  where the validation pass leaves them, with `include_invalid_patients = TRUE` or an exception naming
  them under rule 1. Without the option the removal is unchanged.
* An instance on which no organisation unit carries a custom-attribute value imports as no values. It
  used to fail the import: widening a response in which every org unit serializes an empty array
  delivers the column as logical `NA` rather than as a list, and the reader took that for one value
  per org unit with nothing to read.
* `import_dhis2()` reads the custom attributes an instance sets on its organisation units. The new
  `include_custom_attributes` option of `dhis2_dataset_options()` names the entities whose values to
  import (`"departments"`, `"hospitals"`); their values land typed by the attribute's DHIS2 value type
  in `metadata$departmentAttributeValues` / `metadata$hospitalAttributeValues`, with the definitions in
  `metadata$orgUnitAttributes`. Departments flagged by the `IsTestunit` attribute are fetched on every
  import through a narrowed request and now count as test units alongside `TEST_UNITS` group
  membership.
* New export `get_cumulative_incidence_table()`: the share of patients (or admissions) admitted to a
  department within a calendar window who acquired an infection within that same window, with a
  Wilson interval; the default outcome is the severe-infection composite (primary sepsis/BSI plus
  pneumonia).
* `validate()` is exported, so the records the import would remove can be listed with the rule that
  flags them.
* `gestational_age_to` now covers the whole completed week it names: `31` keeps 31+0 through 31+6,
  where it used to stop at 31+0. Reference data serialized with an upper bound before this change
  describes a cohort six days narrower than an import with the same nominal bound yields now, and a
  consumer that matches reference data to a report by that nominal bound compares the two as equal;
  such reference data needs regenerating before it is compared.
* The validation pass is skipped whenever no patients are imported (`include_patient = "no"`),
  since it is patient-anchored and has nothing to remove; a metadata-only import, which used to
  trip the pass's preconditions and the eligibility filter's look for admission data, now completes.
  An import that asks for validated patients without the full enrollments and events to check them
  against aborts before the first request, naming both ways out (import both with `"full"`, or
  `include_invalid_patients = TRUE`); `validate()` requires the same. An exception list passed as
  `include_invalid_patients` is checked for its record columns before the first request as well.

# neoipcr 0.0.0.9001

* `import_dhis2()` reads DHIS2 2.40 and 2.41 through one org-unit request dialect per version line,
  and reads `/me` `lastLogin` whether it is nested under `userCredentials` or absent. An offline
  compatibility matrix drives the whole import pipeline against synthetic fixtures for every version
  line, with a mock dispatcher that aborts on any unmocked request.
* New export `neoipcr_supported_versions()` names the DHIS2 releases verified against a live server
  (2.40.12.0 and 2.41.9.0) and the supported metadata-package range; `import_dhis2()` warns when the
  server's major.minor line is not among them.
* `dhis2_connection_options()` no longer defaults to any deployment's host: it requires an explicit
  `hostname` or the `NEOIPC_DHIS2_HOST` environment variable and aborts with an actionable message
  otherwise.
* Program stages resolve by their authored `NEOIPC_STG_*` code rather than by display name; an
  instance whose stages carry no code falls back to the name, as a marked compatibility path.
* `get_antibiotic_utilisation_table()` is renamed `get_antibiotic_utilization_table()`, and the
  `antibiotic_utilization_table` slot of the report and reference datasets with it. The procedure
  category code `to_be_categorised` is now `to_be_categorized`; datasets serialized under the old
  code still render its label.
* The reference-report distribution figures drop missing birth weights and gestational ages instead
  of aborting `calculate_reference_data()`, and validation rule 18 flags a missing `patient_days`.
* The `patients` schema carries the `isTest` column, as `events` and `enrollments` already do.
* `write_json()` and the `.pot` writer emit LF on every platform. The package declares its text-file
  contract — LF, UTF-8, no BOM — in `.gitattributes` and `.editorconfig` and checks it in CI.
* The GitHub Release body for a tag is this file's section for that version, and a pull-request
  check requires a non-empty section for DESCRIPTION's version.
* The copyright holder is The NeoIPC Project Consortium across DESCRIPTION, the licence files and
  the message catalogues.

# neoipcr 0.0.0.9000

First dev-snapshot tag.

* The package is pre-alpha. The first CRAN release is planned as `0.1.0`; until then it carries the
  `.9000` development suffix, and `0.1.0` stays reserved for that release rather than being spent on
  a snapshot.
* Immutable dev-snapshot tags (`v0.0.0.9000`, `v0.0.0.9001`, …) mark the commits the NeoIPC reporting
  container pins, so a built image records exactly which neoipcr snapshot it shipped.
