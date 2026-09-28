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

# neoipcr 0.0.0.9007

* `import_dhis2()` reconciles, before its filters and its validation pass, the stored values the NeoIPC
  coordinating centre is responsible for: values Tracker Capture derives itself and the partner team
  never chooses, and values it keeps in a section it hides. The client assigns a day of life whenever it
  saves a form that can still be edited, so a different stored value survives only on a completed form,
  which it shows read-only as stored until the form is reopened, or where the value was stored around
  the client; it computes the total gestation days into a field the partner cannot edit. Reconciliation
  1 gives day of life 1 to the admission form of an infant admitted from the delivery room or on the day
  of birth (admission types 1 and 2), as the client does, unless the import read an earlier enrolment of
  the patient, even one a reporting period leaves out, which rule 47 reports where the dataset holds
  both; reconciliation 2 derives again from day 1 the day of life of an infection or procedure form on an
  enrolment whose admission reconciliation 1 repaired, where the client derived it from the stored
  admission value or it is missing; reconciliation 3 computes the total gestation days from a
  gestational-age text in the format the registration form requires where the stored total differs from
  it or is missing;
  reconciliation 4 removes a total outside 140 to 349 days beside no text in that format;
  reconciliation 5 removes the secondary-BSI infectious agents of a surgical site infection form whose
  secondary-BSI item is not Yes; reconciliation 6 removes the infectious agents of a sepsis form recorded
  as culture-negative, unless the form names an infectious agent and would not then meet the
  clinical-sepsis definition, in which case it is reported and kept as stored. The reconciliation reads
  the import's own links and dates, so it reconciles the same records under the pseudonymized tiers as
  under the full ones. It also runs under `include_invalid_patients = TRUE`, which leaves out the pass,
  and under `include_ineligible_patients = TRUE`, which leaves out the eligibility filters. A repaired
  value's audit fields (`_storedBy`, `_createdBy`, `_updatedBy`, `_createdAt`, `_updatedAt`) are
  missing, since they describe the value as stored.
* `dhis2_dataset_options()` gains `reconcile`, `TRUE` by default, so every import reconciles unless it
  asks otherwise. `reconcile = FALSE` keeps every value as stored, which a copy of the stored record under
  Article 15 of the GDPR requires: a consumer that produces one must now pass it. An earlier neoipcr
  rejects the argument.
* A dataset carries the reconciliation's counts in `reconciliationSummary`: one row per reconciliation,
  in the order of `reconciliation_ids()`, zero counts included, with the kind of record it acts on
  (`record_kind`: `patients`, `enrollments` or `events`) and the distinct records of the returned dataset
  it repaired (`n_repaired`) and reported without repairing them (`n_reported`). A record reconciled and
  then left out, outside the reporting period or removed by a filter or by the pass, is not counted, so
  the summary describes the dataset it comes with. A count is `NA` where the import could not read the
  records the reconciliation acts on, so that 0 always means it read them and found none to change:
  reconciliations 1, 2, 5 and 6 act on the forms and count `NA` under `include_enrollment` or
  `include_event` `"no"`. The slot is 0×0 under `reconcile = FALSE` or without patients.
  `calculate_department_data()` and `calculate_reference_data()` carry it, and `get_benchmark_data()`
  carries it under each dataset's name. A raw dataset written before the slot existed is still
  accepted, its calculated dataset carrying `NULL` in the slot; a slot of another shape is refused with
  class `neoipcr_malformed_reconciliation_summary`.
* `reconciliation_details()` lists, record by record, what the reconciliations repair or report, with
  the stored value and the one that takes its place, on a dataset imported with every record and every
  value as stored: `reconcile = FALSE`, `include_invalid_patients = TRUE`,
  `include_ineligible_patients = TRUE`, no range filter and no reporting period, the full patient,
  enrolment and event tiers, and the gestational age among `patient_columns`. Any other dataset is
  refused with class `neoipcr_reconciliation_needs_stored_values`, naming what is missing.
  `reconciliation_ids()` lists the reconciliations, for a consumer that keeps a label for each.
* The eligibility and range filters and the validation pass judge the reconciled values, so the figures
  of the Partner and Reference reports can change; the admission filter now runs after the forms are
  read and reconciled. Reconciliation 1 keeps a type-1 or type-2 first admission stored with a day of
  life above 120, which the admission filter used to drop. Reconciliation 3 can move a patient across the
  eligibility bound of 224 days in either direction. Reconciliations 3 and 4 change which patients pass
  `gestational_age_from` and `gestational_age_to`, a total set to missing passing neither.
  Reconciliation 4 turns a stored total of 0 into a missing one, so that a patient with a birth weight
  of 1500 g or more is no longer eligible, and one without a birth weight is reported under rule 57, or
  under rule 58 where it records a text in the wrong format. After reconciliation 5 a surgical site
  infection whose secondary-BSI item is not Yes no longer counts as one with a secondary BSI.
  Reconciliation 6 counts a repaired culture-negative sepsis as an infection without an infectious
  agent, and its infectious agents no longer enter the figures built from them.
* Four rules extend the validation pass. On the patient record, rule 58 flags a gestational-age text in
  a format other than the one the registration form requires, two digits for the completed weeks, 20 to
  49, a plus sign and one digit for the days, as in `25+4`; its finding records the text as stored, in
  `validationResults` as well, whatever `patient_columns` selects. On the completed infection forms, three
  rules check the case definition Tracker Capture checks when a form is completed: rule 59 a sepsis form
  that records no infectious agent and does not meet the clinical-sepsis definition (no positive culture,
  intravenous antibiotic therapy for five days or more initiated, and at least two features of
  generalized infection, the laboratory findings counting as one together), a form with neither a
  negative culture nor an infectious agent included; rule 60 a necrotizing enterocolitis form without
  either at least one imaging and one clinical finding or at least one surgical finding; rule 61 a
  surgical site infection form that does not meet the definition of the depth it records. The three read
  an empty item as the client reads it and judge only completed forms. None of the four is an
  eligibility rule, so the import's pass removes the patient of a record any of them flags, whatever
  `include_ineligible_patients` holds. The definitions of a laboratory-confirmed sepsis with a
  common commensal and of pneumonia are not checked: they rest on the client's classification of
  infectious agents, which differs from the package's. The context fields of each rule are listed on
  `validate()`.
* Rule 57 flags a patient only where the gestational-age text is missing as well, an empty text counting
  as missing: a text, even one in the wrong format, is a recorded gestational age, and rule 58 reports
  the wrong format. The pass reads the text whatever `patient_columns` selects, and a later `validate()`
  on a dataset without it skips rule 57. Under `reconcile = FALSE`, a patient whose only gestational age
  is a text in the required format is kept by the eligibility filter, which reads the total, and is
  reported by no rule; under the default, reconciliation 3 computes the total from that text first.
* The documentation of `validate()` names the NeoIPC coordinating centre where it said "the network".

# neoipcr 0.0.0.9006

* `import_dhis2()` with `include_timestamps = TRUE` no longer fails on the events. It parses the
  timestamps the events schema declares as date-times — `scheduledAt`, `completedAt`, `createdAt`,
  `createdAtClient`, `updatedAt` and `updatedAtClient` — where it used to parse every column whose name
  ends in "At", `occurredAt` among them, which is a date by then.
* `substanceDays` carries the audit fields of both of a slot's data values when they are requested:
  under `include_user = "pseudo"` or `"full"` the user keys in `substance_code_storedBy`,
  `substance_code_createdBy`, `substance_code_updatedBy` and their `days_` counterparts, under
  `include_timestamps = TRUE` the date-times in `substance_code_createdAt`, `substance_code_updatedAt`
  and theirs — the same companions the per-event-type data tibbles carry for each field. An import
  that requested either used to fail on these fields. A slot stays one row when its substance and its
  days were entered by different users or at different times.
* `import_dhis2()` with `include_user = "pseudo"` or `"full"` no longer fails on records without a
  creator. DHIS2 leaves out a user field that is empty on every record it returns, and `createdBy` and
  `updatedBy` are empty on every record created before the instance was upgraded to DHIS2 2.36 (events)
  or 2.37 (enrolments, tracked entities), which added them without filling them in for existing records.
  An import whose patients, enrolments or events were all created before that upgrade, or that carried
  no `storedBy` on its patients, or no creator on its event or enrolment notes, used to stop; each such
  field now reads as NA.
* `import_dhis2()` with `include_notes = "enrollments"` no longer fails. It matched the notes to their
  enrolment on the DHIS2 enrolment id, which the enrolments carry only when they are read in full and
  `include_dhis2_ids` asks for it, so every import without those ids, and every import of pseudonymized
  enrolments, stopped. The notes now follow the enrolments whatever `include_dhis2_ids` holds, and leave
  the dataset with them. Under pseudonymized enrolments their patient and department keys are empty, as
  on the other tibbles below a pseudonymized parent.
* The eligibility filter no longer takes a missing value for ineligibility, so what it used to drop
  silently now reaches the validation pass and is counted in `validationSummary`. An admission form
  without a day of life stays: on an admission of type 3 rule 46 reports it and the pass removes the
  patient, where the filter used to remove the form before the pass could see it and the enrolment
  then left with the orphan removal, unreported; on types 1 and 2, admitted on the day of birth, the
  admission stays in the dataset. Only a recorded day of life above 120 leaves. The patient filter
  used to drop a patient with neither birth weight nor gestational age the same way; it now keeps that
  patient for the pass. A patient whose one recorded criterion fails while the other is missing is
  still left out as ineligible, as registration treats it, now by an explicit condition rather than
  by the comparison's missing result.
* Rule 57 flags a patient with neither a birth weight nor a gestational age, whose eligibility
  cannot be established. It is not an eligibility rule, so the pass applies it whether or not
  `include_ineligible_patients` is set. The pass reads the birth weight and the total gestation days
  whatever `patient_columns` selects, as it does the multiple-birth flag and the number of infants
  for rule 56, and drops them again unless selected.
* `import_dhis2()` keeps a patient that records none of the selected patient attributes, with those
  columns missing. Each patient's row used to be built from its attribute values, so such a patient
  left the dataset silently, its enrolments and events with it, and an import in which the patients
  recorded attributes, but none of the selected ones, failed. Under `include_patient = "pseudo"` the validation pass
  selects only the attributes it reads — the multiple-birth flag, the number of infants, the birth
  weight and the gestational age — so an import without any of them failed, and a patient without
  any of them left before rule 57 could report it.
* `get_benchmark_data()` without any dataset aborts with class `neoipcr_no_benchmark_datasets`,
  where it failed with "subscript out of bounds".
* `include_trials` shows the NeoIPC trials the imported departments take part in, the organisation
  unit groups of the group set `NEOIPC_TRIALS`: `metadata$trials` lists them — by `trial_key`, and
  under `"full"` with their code and display names — and `metadata$departmentTrials` links each
  department to its trials by `department_key` and `trial_key` when the departments are included.
  With them, the trials of departments that leave the dataset are dropped; without them, the trials
  of every department the import read are listed. It defaults to `"no"`, since a trial with few
  departments narrows down which department a pseudonymized key stands for.
* `trial_keys` is now `trial_filter`, beside `department_filter` and `country_filter`, and selects
  again: the import keeps the departments that take part in at least one of the named trials, with
  their patients, enrolments and events, and sends its tracker requests to those departments alone.
  It had stopped narrowing anything. The codes are matched exactly rather than as case-insensitive
  patterns, and a code the instance has no trial for aborts with class `neoipcr_unknown_trial`, where
  an instance without the trials group set failed with "argument is of length zero". The trials
  themselves reach `metadata` only with `include_trials`, where `trial_keys` used to add them. In
  reference data the filter is carried as the marker `"applied"`, as `department_filter` is, since
  every user can read which departments a trial has.

# neoipcr 0.0.0.9005

* Twelve rules extend the validation pass to protocol constraints the partner team can see and
  correct in Tracker Capture, each recorded on the form that shows it. On the admission form:
  rule 45 an infant transferred or readmitted after the day of birth admitted beyond day of life 120,
  the last eligible day; rule 46 such an infant whose day of life at admission is missing or below 2;
  rule 47 a later enrolment of the patient typed as an admission from the delivery room or on the day
  of birth. On the enrolment: rule 48 one dated on or after the patient's recorded death. On the
  infection forms: rule 49 the same infection type recorded again within 14 days, across the
  patient's enrolments; rule 50 a device-associated sepsis or pneumonia on an enrolment whose
  completed surveillance-end form counts no day of that device; rule 55 a secondary-BSI item that
  disagrees with the secondary-BSI organisms (Yes without one on any form, organisms under another
  answer on a NEC or pneumonia form). On the surveillance-end form: rule 51 a cumulative count above
  the patient days, one finding per count, the invasive and non-invasive ventilation days bounded
  together as well as apart, since a day counts as one or the other; rules 52 to 54 the antibiotic
  substance slots — a substance without its days or days without a substance, a substance's days
  above the antibiotic or patient days, the same substance in two slots — each finding naming the
  substance as the form shows it. On the patient record: rule 56 fewer than two infants at a recorded
  multiple birth. The context fields of each are listed on `validate()`. An SSI dated after the
  patient's recorded death is caught by no rule; whether a secondary-BSI organism was identified at
  the primary site is not checked until the package reads its pathogen identities from the canonical
  catalogue.
* Rule 45 is an eligibility rule, and the import's pass leaves it out when `include_ineligible_patients`
  is set, since it would remove exactly the late admissions that option keeps; it acts where
  `validate()` runs on such a dataset. The import's eligibility filter now admits day of life 120, the
  last eligible day under the protocol's examples table, where it admitted day 119 at most.
* An empty `patient_columns`, the default, selects every patient column, as documented; it selected
  none. The patient tibble gains the multiple-birth flag (`multiple_birth`, the `patient_columns`
  key of the same name), and the validation pass reads it and the number of infants whatever the
  selection, then narrows the patients back to the selection, so the dataset holds only what was
  requested; a later `validate()` on a dataset without them skips rule 56. The eligibility and range
  filters likewise compare the birth weight and the total gestation days whatever the selection,
  where a selection without them failed the import.

# neoipcr 0.0.0.9004

* The surveillance-end date filter (`surveillance_end_from`, `surveillance_end_to`) selects the
  enrolments whose surveillance ended in the window and leaves the others out with their forms, before
  the validation pass, as records outside the period rather than invalid ones. It used to drop the end
  forms dated outside the window and keep their enrolments, so the pass then met every out-of-period
  enrolment without its end form: it removed those patients under rule 25, counted them as invalid in
  `validationSummary`, and removed a patient's stays inside the period as well whenever another stay of
  the patient ended outside it. With either bound set, an enrolment without a surveillance-end form —
  an active one among them — is left out, since a stay that has not ended has not ended in any window;
  a consumer that wants the open stays sets no period, and rule 43 no longer stands aside under the
  filter. An overlap between a stay in the period and one that ended
  outside it (rule 17) is a finding the pass no longer sees under a period, the other stay being gone
  before it runs.

# neoipcr 0.0.0.9003

* Two rules question an enrolment left open long after its admission: rule 43 an active enrolment
  without a surveillance-end form, rule 44 one whose surveillance-end form is not completed, both once
  the enrolment date lies more than 120 days before the date the data was read — the calendar day of
  the DHIS2 server's own clock, which the import now records on `metadata$system$server_date` beside
  the instant on `metadata$system$date`, or the new `as_of` argument of `validate()`. A stay that
  long is exceptional, so such a record is most often one nobody closed once the infant left; the infant
  may still be admitted, in which case the finding is to be ignored, and an exception record keeps the
  enrolment out of the findings while the stay lasts. A completed-only import carries no active
  enrolments, so the import's own pass is unchanged; an import that requests active enrolments with the
  pass on now removes a patient whose enrolment these rules question, a genuine long stay included,
  unless an exception record names it. An import that requests the enrolments that are not completed
  but only the completed events holds no end form that is not completed, and one that filters the
  surveillance-end dates holds none outside its window, so on either a missing form and one the dataset
  does not hold look alike: rule 43 is skipped on such a dataset, and an import of that shape with the
  pass on refuses. A dataset without a server date refuses a `validate()` whose selection holds either rule,
  the default selection included; a selection without them still runs. The rules now number 43.
* An exception record for an enrolment-level rule that compares a form — rules 3 and 5 the admission
  form, 2, 4, 6, 18, 21 and 44 the surveillance-end form — may name that form's type and date, as a reader
  of the Validation Report writes it from the form the finding is shown on, or leave them empty; both
  exempt the enrolment. A record naming any other form is still refused, as is one naming an event for
  a rule that carries none. `resolve_validation_exceptions()` carries the form's event key on such a
  record.

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
  the reference values. Both calculation functions assert that the copy they emit holds no data frame;
  `calculate_reference_data()` also asserts that its copy names no department. The `redact` argument
  of `calculate_reference_data()` is gone with the replacement it switched.
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
