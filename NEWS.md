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
