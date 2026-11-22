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

* Events and the form-data tables carry their hierarchy keys as a symmetric pair over the organisation-unit chain:
  the side the record's own organisation unit shows, `event_department_key` with its `event_hospital_key`,
  `event_country_key`, and `event_world_bank_class_key`, beside the stay's side, `enrollment_department_key` and its
  counterparts, replacing the plain `department_key` and its chain those tables held. The import resolves the pair
  before the schema projects the tiers, and the tiers place it where a chain breaks: on `events` at the full tier,
  and directly on the form-data tables at the pseudonymized, where the inherited links and hierarchy keys they
  declare are now materialized instead of left `NA`. Patient records keep their own keys as they are: a patient
  holds no single stay. This is a breaking change for dataset readers: a reader of a stored dataset that joined
  the events or a form-data table on the plain `department_key` finds that column gone, and the pre-alpha package
  offers no migration.
* A department- or trial-scoped import fetches its events through the stays it kept: it reads the scoped enrolments
  first and then asks the events endpoint for their events by enrolment UID, which DHIS2 searches over the importing
  user's accessible units. A form of the department's stay recorded under another organisation unit therefore reaches
  the department's own dataset and counts for the stay's department, where the per-unit event requests had dropped
  it silently; an event recorded beyond the importing user's reach stays out. Country- and access-scoped imports keep
  their per-unit event requests.
* Every rate table's department rows group by `enrollment_department_key`, and `get_infection_counts()` answers
  every grouping column from the side its name names: an `event_` column counts and frames on the events, a plain
  or `enrollment_` column counts each event's own stay side and frames on the enrolments, so a department with stays
  but no infection shows a zero. A dataset stored by an older neoipcr has no pair to calculate over where its import
  tiers materialize the pair: the calculations refuse it and name the re-import that replaces it rather than
  recalculating on the old keys, so it must be imported again when a benchmark spanning the change needs its tables
  recalculated; tables already computed in a stored dataset keep being served.
* `read_token()`, and with it the `token` argument of `dhis2_connection_options()` and the
  `NEOIPC_DHIS2_TOKEN` environment variable, now accepts a DHIS2 personal access token of either
  version by its shape: version 1 (`d2pat_`, 48 characters), which only DHIS2 2.40 and earlier
  issued, and version 2 (`d2p_`, 54 characters), the only version DHIS2 2.41 and later issue. Every
  other string is still refused with the same error, and the shape check also refuses strings the
  prefix-and-length check it replaces used to accept, such as a 48-character `d2pat_` string with a
  `-`, `_`, or a letter among its last 10 characters. The token's checksum stays checked by DHIS2,
  which does so when the token is used, and not by neoipcr.
* The documentation now says what the `storedBy` columns hold: from DHIS2 2.41.9, and on the later
  lines from 2.42.6 and 2.43.1, a record written through the tracker API holds the authenticated
  user's name there, while the legacy APIs that Tracker Capture writes through still store the value
  the client sent, so attribution rests on the `createdBy` and `updatedBy` columns that `include_user`
  adds, of which a patient's attribute values have none.
* neoipcr classifies infectious-agent findings by tables built from a release of the NeoIPC
  infectious-agent list and its reporting classification in the Surveillance-Toolkit, the release
  that `neoipcr_supported_versions()` gives as the new element `infectious_agents`. The tables they
  replace came from a concept table the toolkit does not hold: they gave some of the list's codes to
  other taxa and lacked others, "Unidentifiable" (`Id` 3561) among them, so that a finding with a
  code they lacked dropped out of every infectious-agent table, its Total included. The package
  bundles only the factual extract of the list that the list's licence leaves outside its terms, no
  longer a copy of the whole list, and its README attributes the extract's sources. It no longer
  suggests yaml.
* `get_infectious_agents()` is the package's export over the list: each code with its name and the
  concept it names (`id`, `name`, `concept_id`, `concept_name`), a synonym naming the concept it
  stands for. A code the list lacks returns no row, under a warning of class
  `neoipcr_unknown_infectious_agent_code`, and `ids` that are no codes are refused with an error of
  class `neoipcr_invalid_infectious_agent_ids`. `get_pathogen_taxonomy()` is removed; the lineage and
  the flags it returned stay internal.
* An infection counts as one with an infectious agent only where one of its findings names one, "Not
  listed" (`Id` 0), "Unidentifiable" (`Id` 3561), and a code the infectious-agent list lacks included.
  A slot that holds only a companion value (the source, the "recovered multiple times" item, a
  resistance category, or a free-text name) yields a finding without `pathogen_key`, and such a
  finding no longer makes its infection one with an infectious agent, in any infectious-agent table
  or in the secondary-BSI rate.
* The infectious-agent tables count a slot only where the form asks for it, so that every infection
  a numerator counts is in its denominator: a sepsis form's slots unless it is recorded
  culture-negative; a pneumonia's own slots where its test is positive; an SSI's own slots where its
  culture at any depth is positive, whatever depth its infection type records and whether or not it
  was followed up for a secondary BSI; and the secondary-BSI slots of a NEC, pneumonia, or SSI where
  its follow-up is answered Yes. An infectious agent left in another slot counts in none of them.
* The secondary-BSI rate counts a NEC, pneumonia, or SSI as one with a secondary BSI only where its
  follow-up is answered Yes and one of its secondary-BSI findings names an infectious agent. An
  infectious agent left in the secondary-BSI slots under another answer, which only a validation
  exception or `include_invalid_patients = TRUE` lets through, no longer counts, so the rate is a
  share of the infections followed up by construction.
* `get_infectious_agent_detection_rate_per_inf_type_table()` has a row each for primary sepsis/BSI,
  pneumonia, and SSI, each with two shares: the share tested (`tested_pooled`, with
  `tested_ci_lower` and `tested_ci_upper`), the pneumonias whose microbiological test, and the SSIs
  whose culture at the depth their infection type records, is recorded positive or negative, over
  every infection of the type (`total`); and the share positive (`pooled`), the tested infections
  whose test is positive (for an SSI, at its own depth) with an infectious agent in an own slot
  (`n`), over the tested ones (`tested`). A test recorded "Not tested" or left blank counts as not
  tested. Primary sepsis/BSI has the positive share alone, over every episode, since its form cannot
  tell a blood culture never taken from a negative one; an episode recorded culture-negative counts
  as not positive whatever its slots hold. Secondary-BSI slots count in neither share, and the
  follow-up for a secondary BSI moves neither; the secondary-BSI table gives both. NEC has no row: it
  has no test of its own, and its infectious agents are recorded only for a secondary BSI. Nor does
  any row give all types together, whose share would depend on each department's mix of infection
  types. In the reference table, a department enters a share's quartiles with at least 5 infections
  in its denominator of the share, at 0 where none of them is in its numerator; a share shows them
  where at least 5 departments enter and their median denominator exceeds the number of infections
  in which the pooled share gives 1 infection, and gives the number of departments that enter them,
  shown or not, as `quartile_departments`, and for the share tested as
  `tested_quartile_departments`, beside `tested_q1` to `tested_q3` and their bootstrap intervals.
* `get_infectious_agent_detection_rate_per_agent_table()` takes its rows from the released NeoIPC
  infectious-agent list and its reporting classification, by the new argument `rows`: `"groups"`,
  the default, over the reporting groups, below a summary block of the bacteria by Gram reaction and
  oxygen requirement, or `"ontology"`, over every level of each infectious agent's path through the
  list from its top-level row, Bacteria, Fungi, Protozoa, or Viruses, to the node at which it is
  recorded, with a residual "n.o.s." (not otherwise specified) row where infectious agents are
  recorded at a node that has nodes beneath it in the full ontology (a species has none, the
  concepts beneath it counting in it), whatever other infectious agents the dataset holds, so that a
  department's rows match those a reference holding it gives it.
  A row's `node_id` stays the same across releases of the list that keep its node: over the full
  ontology it is the node's source and its record there, which for a node whose source is the
  International Committee on Taxonomy of Viruses (ICTV) is the identifier that links its taxon
  across the releases of the ICTV Master Species List rather than its node in one release. Each row
  other than the Total is the share of the confirmed infections with an infectious agent in the
  tree in which one at or beneath the row's node was found, with a Wilson interval, so a parent's
  share need not be the sum of its children's; an infectious agent counts once per infection, a
  synonym as its concept, and only in a slot the form asks for. The Total gives the infectious
  agents per 100 confirmed infections, "Not listed", "Unidentifiable", and the codes the list lacks
  included, with an exact Poisson interval and their number as `n_total_only`. The columns are
  `level`, `rank`, `node_id`, `parent_id`, `group`, `n`, `total`, `pooled`, `ci_lower`, and
  `ci_upper`, a column of each measure per infection type (`n_bsi`, `total_bsi`, `pooled_bsi`, and
  so on), and in a reference dataset the quartiles with `quartile_departments`, by the rule of the
  per-type table; `taxon` and `.ontology_path` are gone. Both calculation functions carry both
  versions, the ontology version as `infectious_agent_detection_rate_per_agent_ontology_table`.
* The three resistance tables count detections: an infectious agent in an infection, once per
  infection and only in a slot the form asks for, with a result for a resistance category where
  either slot naming it is recorded resistant or not, and resistant where either records it
  resistant; "Not tested" and a blank are no result. Which infectious agents a category applies to
  comes from the released list, so methicillin-resistant *Staphylococcus aureus* (MRSA) covers the
  *S. aureus* complex, *Staphylococcus argenteus* included, and vancomycin-resistant *Enterococcus*
  (VRE) the genus and its species. `get_abr_infection_rate_table()` and
  `get_infectious_agent_resistance_rate_table()` take the argument `rows` of the per-agent table and
  give each resistance category (`abr`) a block of the rows of that version on the paths of the
  infectious agents to which the list applies it, those that lie at or above an infectious agent
  found to which it applies the category or that has a result for it, with the per-agent table's
  `level`, `rank`, `node_id`, `parent_id`, and `group`, a residual row only beneath a node that has
  a node on those paths beneath it; each infection counts once per row. A value recorded for a
  category that cannot apply to its infectious agent counts in the block's rows at or above it as
  any other, and whether a dataset has a row depends only on the infectious agents at or beneath it.
  Both calculation functions carry both versions, as `abr_infection_rate_table` and
  `abr_infection_rate_ontology_table`, and as `infectious_agent_resistance_rate_table` and
  `infectious_agent_resistance_rate_ontology_table`.
* `get_abr_infection_rate_table()` gives densities per 1000 patient-days, every infant being at risk,
  in place of shares of the infections with an infectious agent: per category a Total, the
  infections with any infectious agent recorded resistant, "Not listed", "Unidentifiable", and the
  codes the list lacks included, `n_total_only` giving those of them that no row of the block
  counts, then its rows, with exact Poisson intervals and the patient-days as `total`. A department
  enters a row's quartiles with at least 50 patient-days, at 0 without the row's infections, unless
  it detected infectious agents of the row's node to which the list applies the category and none
  of them has a result for it, in which case it leaves the row's pooled density, interval, and
  count as well; a Total tests all of the department's detections to which the list applies the
  category. `get_benchmark_data()` gives a dataset a row it lacks at 0 over all its patient-days,
  all its departments entering the row, and a reference dataset's `n_patient_days` gives those with
  at least 50 patient-days, which enter the row's quartiles, as `quartile_departments`.
* `get_organism_resistance_rate_table()` is renamed `get_infectious_agent_resistance_rate_table()`,
  its class `neoipcr_tbl_iarr`, and its slot `infectious_agent_resistance_rate_table`. A row's rate is
  the share of the infections in which an infectious agent of the row's node has a result for the
  category in which one is recorded resistant (`n`, `total`, `pooled`), and beside it the Tested
  share (`tested_n`, `tested_total`, `tested_pooled`, `tested_ci_lower`, `tested_ci_upper`) over the
  infections with an infectious agent of the node to which the list applies the category. A
  department enters a row's quartiles with at least 5 infections with a result.
* `get_resistance_test_rate_table()` gives, for each category, the share of the detections to which
  the list applies it that have a result, a blank counting as undocumented in the denominator, where
  it counted every slot holding a value of the category and left the blanks out. "Not listed" counts
  in every category, "Unidentifiable" and a code the list lacks in none, and a value recorded for a
  category that the list does not apply to its infectious agent in none. The conditional rows hold
  only the detections recorded resistant to third-generation cephalosporins (3GCR), and for colistin
  also to carbapenems, where they counted a blank as resistant. The table gives each row's
  denominator as `total` and the number of departments that enter its quartiles, those with at least
  5 detections in its denominator.
* `get_secondary_bsi_rate_table()` shows beside each rate the share of the type's infections
  followed up for a secondary BSI (`followup_pooled`), with its counts (`followup_n`, the infections
  followed up, answered Yes or No; `total_n`, all infections of the type), and the reference table
  the quartiles of the departments' shares (`followup_q1`, `followup_q2`, `followup_q3`) over the
  departments with at least 5 infections of the type, `NA` with fewer than 5 such departments.
* `calculate_department_data()` and `calculate_reference_data()` require `include_event = "full"`
  and refuse a dataset imported with `"pseudo"`: its findings carry no `pathogen_key` or
  `secondary_bsi`, since a rare infectious agent can single out a patient in a pseudonymized
  dataset, and the infectious-agent figures need them. The refusal of these and every other import
  option a function requires has the class `neoipcr_unmet_import_options`.
* `calculate_department_data()` and `calculate_reference_data()` stamp each table of the dataset
  they return with the version of the definitions of its rates, in `metadata$rate_definitions`
  (`table`, `version`), beside the digests of what the table reads from the bundled infectious-agent
  list, and no other: the common-commensal flags for the incidence densities
  (`common_commensal_digest`); the concept each code names, by which an infectious agent counts once
  per infection, for the tables whose rows are infectious agents and the completeness of
  antimicrobial susceptibility testing (`concept_mapping_digest`); the tree of the version a table
  whose rows are infectious agents is laid out over (`groups_tree_digest` or `ontology_tree_digest`)
  and the `layout` of its rows; and to which infectious agents each resistance category applies, for
  the tables of the resistance categories (`applicability_digest`). A release of the list that
  changes only the full ontology thus leaves the tables over the reporting groups comparable, and
  one that changes only the concept a code names makes the tables that count by it incomparable.
  `metadata$infectious_agent_tables` names the list's release (`version`, `commit`) and the layout,
  and `metadata$neoipcr_version` the package version. Both warn once, with the class
  `neoipcr_unknown_infectious_agent_code` and the codes in its field `codes`, where the findings hold
  codes the list lacks.
* `get_benchmark_data()` joins a table only across datasets whose stamps of it match. A dataset
  whose stamps of a table differ from those of the first dataset that holds it is not joined into
  the table: the table keeps the rows of the datasets joined, the table and dataset are listed in
  the new element `not_comparable`, and a warning of class `neoipcr_tables_not_comparable` is
  raised; a figure takes none of that dataset's rows, and any other table leaves that dataset's
  columns empty. A stamp that a dataset lacks, such as one a later version adds, counts as missing
  there, so that only the tables whose stamp in the other dataset has a value in it are not joined.
  A dataset calculated before the stamps existed counts as calculated under other definitions for
  every table. The tables whose rows are infectious agents are joined on `node_id`, within `abr`
  where they have one, and take the order of the reference: the first reference dataset joined
  into the table, or else the first dataset that holds it. So departments benchmarked against each
  other take the order of the first one that holds the table, and a department passed before a
  reference whose table is not joined keeps its own. The benchmark holds the rows of every dataset
  in the reference's order, the summary block above the tree, and any other row the reference
  lacks after its siblings, in its order in the joined table, but before a residual "n.o.s." row,
  which stays last among its node's children; a dataset without a row has it at 0 over its own
  denominator of the row. The joined table holds the rows of the datasets in the order in which
  they are passed, the first dataset's first, each later one adding the rows the earlier ones
  lack. A summary row is one whose `node_id` names an attribute of the bundled classification
  before its `|`, whatever its value, and the summary rows are laid out attribute by attribute in
  the reference's order of the attributes and their values, which the benchmark takes from the
  reference's summary rows of every block of the table, each block holding them in the order of
  the release of the list the reference was calculated from, whether or not the bundled
  classification holds their values. Where the blocks leave that order open, as for two attributes,
  or two values, that the blocks order neither directly nor through others, the bundled
  classification decides it as far as the blocks allow: place by place, of the attributes or values
  the blocks let come next, the first in its order, a value it lacks after those it holds, and two
  values it lacks in the order in which the reference's table first shows them. An attribute of
  which the reference holds no row in the table comes directly after the last of the reference's
  attributes that the bundled classification orders before it, or first, several such attributes in
  the bundled classification's order; a value the reference holds nowhere in the table directly
  after the reference's value of its attribute that the reference places last among those the
  bundled classification orders before it, or else directly before the reference's first value of
  its attribute, and last in its attribute's run where the bundled classification lacks it; and
  rows that share a place in the bundled classification's order, those whose value it lacks in
  their order in the joined table. So the attributes, and the values that the reference holds in
  the table or the bundled classification holds, follow one order in every block, while the values
  that neither holds follow, in each block, their order in the joined table, which can differ
  between blocks. A summary row of an attribute the bundled classification lacks, which only a
  dataset calculated by another version of neoipcr can hold, is placed as a top-level row of the
  tree. Where a dataset calculated before the stamps holds a table whose rows are infectious
  agents first or alone, the table keeps its rows as they are, that dataset's resistance rates per
  infectious agent, held as `organism_resistance_rate_table`, counting as its
  `infectious_agent_resistance_rate_table`.
* A table builder given a calculated dataset that lacks its table, one calculated before the table
  existed, refuses it with an error of class `neoipcr_table_not_in_dataset`.
* Every stored reference dataset must be calculated again with this version, and so must every
  stored department dataset that is to be benchmarked: its infectious-agent tables count otherwise
  than the rules above, its tables lack the ontology versions and the renamed slot, and, without the
  stamps, `get_benchmark_data()` joins none of its tables with those of a dataset calculated now.
* Rules 29, 33, and 37 and rules 30, 34, and 38 now derive the day they judge instead of reading
  the day of life or day of occurrence stored on the infection form, which can be stale or
  missing. The day of life is the admission's day of life, day 1 for admission types 1 and 2 and
  for a form without a type and the recorded day for type 3, advanced by the days from the
  enrolment date to the event date; the day of the stay counts the day of admission as day 1 and
  is derived from the dates for every admission, so a wrong recorded admission type can no longer
  hide an early onset.
  The context fields `dol` and `dos` hold the derived days. An infection dated before the stay's
  first day is left to rules 12 to 14. Rules 29, 33, and 37 leave unjudged the infections of an
  enrolment without its admission event, which rule 26 reports for a completed enrolment, and of a
  type-3 admission without its day of life, which rule 46 reports. An exception that keeps an
  early-onset infection of an inborn infant on day of life 1 or 2 now needs a record for a rule 30,
  34, or 38 as well as for its rule 29, 33, or 37, since both families now fire.
* The package now holds the internal interval engine the reference report's figures rest on: the
  interval of the departments' quartiles between two order statistics with binomial ranks, a side
  open where its rank would rest on the lowest or highest department alone; the exact gamma and
  Clopper-Pearson intervals on the effective counts and denominators a design effect leaves; the
  bias-reduced cluster-robust (CR2) design effect with its Bell-McCaffrey degrees of freedom, for
  the clustering of episodes within patients and within departments, on counts, on a share, and on
  a mean; Kish's effective number of a department's patients; the choice a department figure makes
  between its own design effect, the reference's, and the fallback for a department without one;
  and the sparse-data, low-degrees-of-freedom, and few-patients flags beside the measure minima
  that cut a lower limit. None of it is exported, and the quartile figures take the bootstrap
  interval.

# neoipcr 0.0.0.9008

* Validation rules have a severity, which the new `validation_rule_severities()` lists. An error flags
  a record the analyses cannot use as recorded; a warning flags one they can, although it may hide a
  mistake the record cannot prove. The validation pass of `import_dhis2()` runs both. It removes the
  patients an error flags and keeps those only a warning flags. It reports a warning's findings in
  `validationResults` and counts the records a warning flags in the new `validationSummary` column
  `n_warned`, whether or not an error removed their patient. Rules 43 and 44 (an enrolment open for
  more than 120 days) and rule 54 (a substance recorded in two slots of a surveillance-end form) are
  warnings, so the pass no longer removes a patient they flag.
* The new rule 62, an error, flags a substance whose slots on a surveillance-end form add up to more
  days than the form's antibiotic days, one finding per substance and form. It records the substance
  (`substance_code`, `substance`), the sum (`substance_days`), and the antibiotic days (`ab_days`).
  Separate treatment courses cannot add up to more than the antibiotic days, so such slots count days
  twice or wrongly. Only slots that hold days count, and the rule is skipped on a dataset without the
  antibiotic days.
* Rule 54's findings carry what tells a substance recorded once per treatment course from a course
  entered twice: each slot's days as stored (`days`, `days_other`), their sum over all the substance's
  slots on the form (`substance_days`), and the form's antibiotic days (`ab_days`). A value the form
  does not hold is recorded as `NA`, and the pair is reported all the same, also where the
  surveillance-end event has no form data or the dataset lacks the antibiotic days. A pair whose two
  slots hold days that together with the substance's other slots exceed the antibiotic days is rule
  62's, not rule 54's, also in a `validate()` call that does not select rule 62. An exception record
  for rule 54 therefore no longer exempts such a pair; only a record for rule 62 does. The records for
  rule 54 in a stored list need review: `validation_exception_usage()` shows those that now exempt
  nothing, and each that still holds is to be written for rule 62.
* `validate()` returns, beside `rules_skipped`, the rules it ran as the attribute `rules_run` and the
  findings its exceptions exempted as `findings_exempted`. The new `validation_summary()` counts a
  result's flagged and exempted records per rule and record kind, as `import_dhis2()` stores them in
  `validationSummary`, which it now derives from `findings_exempted` rather than from a second pass of
  its own.
* The new `validation_exception_usage()` says, for each record of an exception list that concerns the
  dataset's departments, whether it matches a record of the dataset and how many records it exempted,
  so that a list's records that match nothing or exempt nothing can be found. A list with department
  codes needs a dataset that carries them (`include_department = "full"`).
* `read_validation_exceptions()` refuses a file that is not UTF-8 text, such as a UTF-16 export or a
  file with a byte another encoding wrote, as `neoipcr_invalid_exception_list`, like its other defects.
  Such a file used to fail with an error of another class.

# neoipcr 0.0.0.9007

* `import_dhis2()` reconciles, before its filters and its validation pass, the stored values the NeoIPC
  coordinating centre is responsible for: values Tracker Capture derives itself and the partner team
  never chooses, and values it keeps in a section it hides. The client assigns a day of life whenever it
  processes a form while it can still be edited, and saves it, so a different stored value survives only
  on a completed form, which it shows read-only as stored until the form is reopened, or where the value
  was stored around the client; it computes the total gestation days into a field the partner cannot
  edit. Reconciliation 1 gives day of life 1 to the admission form of an infant admitted from the
  delivery room or on the day of birth (admission types 1 and 2), as the client does, unless the import
  read an earlier enrolment of the patient, even one a reporting period leaves out, which rule 47
  reports where the dataset holds both; reconciliation 2 derives again from day 1 the day of life of an
  infection or procedure form on an enrolment whose admission reconciliation 1 repaired, where the
  client derived it from the stored admission value or it is missing; reconciliation 3 computes the
  total gestation days from a gestational-age text in the format the registration form requires where
  the stored total differs from it or is missing; reconciliation 4 removes a total outside 140 to 349
  days beside no text in that format; reconciliation 5 removes the secondary-BSI infectious agents of a
  surgical site infection form whose secondary-BSI item is not Yes; reconciliation 6 removes the
  infectious agents of a sepsis form recorded as culture-negative, unless the form names an infectious
  agent and would not then meet the clinical-sepsis definition, in which case it is reported and kept
  as stored. The reconciliation reads the import's own links and dates, so it reconciles the same
  records under the pseudonymized tiers as under the full ones. It also runs under
  `include_invalid_patients = TRUE`, which leaves out the pass, and under
  `include_ineligible_patients = TRUE`, which leaves out the eligibility filters. A repaired value's
  audit fields (`_storedBy`, `_createdBy`, `_updatedBy`, `_createdAt`, `_updatedAt`) are missing, since
  they describe the value as stored.
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
  49, a plus sign and one digit for the days, as in `25+4`; its finding records the text as stored,
  which the import's `validationResults` keeps only where the returned patients carry the gestational
  age and records as `NA` otherwise. On the completed infection forms, three rules check the case
  definition Tracker Capture checks when a form is completed: rule 59 a sepsis form that records no
  infectious agent and does not meet the clinical-sepsis definition (no positive culture, intravenous
  antibiotic therapy for five days or more initiated, and at least two features of generalized
  infection, the laboratory findings counting as one together), a form with neither a negative culture
  nor an infectious agent included; rule 60 a necrotizing enterocolitis form without either at least
  one imaging and one clinical finding or at least one surgical finding; rule 61 a surgical site
  infection form that does not meet the definition of the depth it records. The three read an empty
  item as the client reads it and judge only completed forms. None of the four is an eligibility rule,
  so the import's pass removes the patient of a record any of them flags, whatever
  `include_ineligible_patients` holds. The definitions of a laboratory-confirmed sepsis with a common
  commensal and of pneumonia are not checked: they rest on the client's classification of infectious
  agents, which differs from the package's. The context fields of each rule are listed on `validate()`.
* Rule 57 flags a patient only where the gestational-age text is missing as well, an empty text counting
  as missing: a text, even one in the wrong format, is a recorded gestational age, and rule 58 reports
  the wrong format. The pass reads the text whatever `patient_columns` selects, and a later `validate()`
  on a dataset without it skips rule 57.
* The eligibility filter and the range filters `gestational_age_from` and `gestational_age_to` judge,
  where the stored total gestation days are missing, the total the registration form computes from a
  gestational-age text in the required format, so that under `reconcile = FALSE` a patient whose only
  gestational age is such a text is judged by it; its stored values stay as they are. The import reads
  the text for the filters whatever `patient_columns` selects and drops it again unless selected.
* Rule 56's `siblings`, the number of infants the finding records, is `NA` in the import's
  `validationResults` where the returned patients do not carry it, under `include_patient = "pseudo"`
  or with `"siblings"` left out of `patient_columns`, as rule 58's text is: the pass reads both
  attributes whatever `patient_columns` selects, and the dataset now holds only the patient attributes
  that were asked for.
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
