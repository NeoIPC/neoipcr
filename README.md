
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neoipcr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NeoIPC/neoipcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NeoIPC/neoipcr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/NeoIPC/neoipcr/graph/badge.svg)](https://app.codecov.io/gh/NeoIPC/neoipcr)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)

<!-- badges: end -->

neoipcr reads NeoIPC surveillance data out of a DHIS2 server and turns
it into the epidemiological indicators the NeoIPC network reports on —
device-associated infection rates, antimicrobial usage density,
resistance rates and the denominators behind them.

[NeoIPC](https://neoipc.org) works to reduce the transmission of
resistant bacteria in neonatal intensive care across Europe and
globally. Its [surveillance system](https://neoipc.org/surveillance/)
collects healthcare-associated infection and antimicrobial-use data from
neonatal departments, and this package is how that data is read and
analysed — both by the project’s own reports and by data scientists at
participating hospitals working with their own site’s data.

## Installation

neoipcr is not on CRAN yet. Install the development version from
[GitHub](https://github.com/NeoIPC/neoipcr):

``` r
# install.packages("pak")
pak::pak("NeoIPC/neoipcr")
```

## Importing Data From DHIS2

To import data from a NeoIPC DHIS2 server use `import_dhis2()`. neoipcr
is a public library for *any* DHIS2 instance running the NeoIPC metadata
package, so it does not default to a specific host — pass your
instance’s `hostname` to `dhis2_connection_options()` (with
`scheme`/`port`/`path` if they differ from the `https` / 443 / `/api`
defaults). Credentials can be passed directly (a personal access token,
username/password, or session id), or left to be resolved from
environment variables (`NEOIPC_DHIS2_TOKEN`, `NEOIPC_DHIS2_USER` +
`NEOIPC_DHIS2_PASSWORD`, or `NEOIPC_DHIS2_SESSION_ID`), falling back to
an interactive prompt; see `?dhis2_connection_options`.

``` r
library(neoipcr)

connection <- dhis2_connection_options(hostname = "dhis2.example.org")
data <- import_dhis2(connection)
```

Alternatively, set the host in the `NEOIPC_DHIS2_HOST` environment
variable (alongside the credential variables above) and call
`import_dhis2()` with no arguments:

``` r
# NEOIPC_DHIS2_HOST and, e.g., NEOIPC_DHIS2_TOKEN set in the environment
data <- import_dhis2()
```

The custom attributes an instance sets on its organisation units (a
trial’s per-site dates, for example) are imported on request for an
entity that is itself imported:
`dhis2_dataset_options(include_department = "full", include_custom_attributes = "departments")`
adds their typed values as `metadata$departmentAttributeValues`, with
the attribute definitions in `metadata$orgUnitAttributes`.

The trials the imported departments take part in are imported on request
too:
`dhis2_dataset_options(include_department = "full", include_trials = "full")`
lists them as `metadata$trials` and links each department to its trials
in `metadata$departmentTrials`. `trial_filter` narrows the import to the
departments of the trials it names.

## Calculating Indicators

`import_dhis2()` returns a keyed, relational dataset. From there,
`calculate_department_data()` produces one department’s indicators and
`calculate_reference_data()` the network-wide reference figures that
department is compared against; `get_benchmark_data()` combines them,
appending the name you give each dataset to its column names — so `own`
and `ref` below yield `n_own` and `n_ref`. The individual
`get_*_table()` builders return the tables the reports render.

``` r
own_data <- calculate_department_data(data)
reference_data <- calculate_reference_data(data)
benchmark <- get_benchmark_data(own = own_data, ref = reference_data)
```

## Data Protection

Restricting what a dataset contains is a first-class feature rather than
something callers are expected to do afterwards.
`dhis2_dataset_options()` declares which identifiers, timestamps,
hierarchy levels and record types you want; the import pipeline sheds
everything else as early as it can, and a guardian at the end of the
pipeline asserts that every reader honoured what you asked for. Ask for
less and the data is never fetched, rather than fetched and then
dropped.

## Supported Versions

neoipcr supports the DHIS2 **2.40** and **2.41** lines. Call
`neoipcr_supported_versions()` for the exact releases — those both
verified against a live server and still current targets. Treat that
function as the source of truth rather than any version list quoted
elsewhere, including here:

``` r
neoipcr_supported_versions()
```

Two limits on that are worth knowing before you rely on it:

- **2.42 and later are not supported.** A live import against 2.42 is
  known to fail, and 2.43 has never been run against a real server.
  Reading such a server is not blocked, but it raises a warning and the
  result should be treated as unverified.
- **The warning is raised per line, not per release,** so a patch
  release of a supported line does not raise it even when that exact
  release is known to be broken. `2.40.3.2` is one such release: it is
  deliberately excluded from what `neoipcr_supported_versions()`
  reports, yet a server running it is on the supported 2.40 line and
  reads without complaint.

The vocabulary neoipcr binds to — the `NEOIPC_CORE` program, the
stage/option-set codes, and the org-unit-group codes (`COUNTRY`,
`HOSPITAL`, `NEO_DEPARTMENT`) — is defined by the NeoIPC metadata
package, not by any particular deployment.

## Part of the NeoIPC surveillance system

| Repository | Role |
|----|----|
| [Surveillance-Toolkit](https://github.com/NeoIPC/Surveillance-Toolkit) | The protocol, the case definitions, the DHIS2 metadata and the report sources |
| **neoipcr** | *(this repository)* Reads NeoIPC data out of DHIS2 and computes the surveillance indicators |
| [NeoIPC-Reporting](https://github.com/NeoIPC/NeoIPC-Reporting) | Service that renders the toolkit’s reports on demand and serves them over HTTP |
| [neoipc-app](https://github.com/NeoIPC/neoipc-app) | DHIS2 application through which people request reports and administer reference data |

The vocabulary this package binds to — programs, stages, option sets,
organisation-unit groups — is defined by the Surveillance-Toolkit’s
metadata package. Where this package and those definitions disagree, the
definitions win.

## Contributing

Issues and pull requests are welcome at
<https://github.com/NeoIPC/neoipcr/issues>. The package is pre-alpha and
its public interface is still moving, so it is worth raising an issue
before a larger change.

Two conventions matter more here than in most packages. Tests never make
real HTTP calls — every DHIS2 response comes from a hand-authored
synthetic fixture under `tests/testthat/fixtures/`. And DHIS2 objects
are resolved by their mnemonic `code`, never by a hard-coded UID,
because UIDs belong to whichever instance generated them.

Translations of the NeoIPC protocol, report text and surveillance
vocabulary are managed on
[Weblate](https://hosted.weblate.org/projects/neoipc/); contributions in
any language are welcome and need no git knowledge.

## Licensing

MIT — see [LICENSE.md](LICENSE.md).

The package bundles the infectious-agent tables by which it classifies
findings, built from a release of the NeoIPC infectious-agent list and
its reporting classification in the Surveillance-Toolkit’s
[`metadata/common/infectious-agents/`](https://github.com/NeoIPC/Surveillance-Toolkit/tree/main/metadata/common/infectious-agents).
That directory is licensed [CC BY-NC-ND
4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/), the strictest
of its sources’ licences. The tables are a factual extract of it: for
each infectious agent its code, its names and synonyms, its rank and
lineage, the identifier of its record in its source, and the
common-commensal status, resistance categories, reporting group (with
the groups above it), Gram reaction, and oxygen requirement that NeoIPC
assigns it. The list’s
[licence](https://github.com/NeoIPC/Surveillance-Toolkit/blob/main/metadata/common/infectious-agents/LICENSE.md)
leaves such an extract outside those terms, licensed with the work that
carries it, and requires an extract that NeoIPC publishes to attribute
the sources that the list’s
[README](https://github.com/NeoIPC/Surveillance-Toolkit/blob/main/metadata/common/infectious-agents/README.md)
lists:

- **NHSN Organism List**:
  <https://www.cdc.gov/nhsn/xls/master-organism-com-commensals-lists.xlsx>,
  the 2024 edition, whose classification the common-commensal status
  follows, except where NHSN is silent or the list records that NeoIPC
  departs from it. Source: [Centers for Disease Control and Prevention
  National Healthcare Safety Network
  (NHSN)](https://www.cdc.gov/nhsn/index.html). Available on the NHSN
  website for no charge. Reference to specific commercial products,
  manufacturers, companies, or trademarks does not constitute its
  endorsement or recommendation by the U.S. Government, Department of
  Health and Human Services, or Centers for Disease Control and
  Prevention. Not subject to copyright but some
  [requirements](https://www.cdc.gov/other/agencymaterials.html) must be
  followed.
- **List of Prokaryotic names with Standing in Nomenclature (LPSN)**:
  <https://lpsn.dsmz.de/>. Parte, A.C., Sardà Carbasse, J.,
  Meier-Kolthoff, J.P., Reimer, L.C. and Göker, M. (2020). List of
  Prokaryotic names with Standing in Nomenclature (LPSN) moves to the
  DSMZ. International Journal of Systematic and Evolutionary
  Microbiology, 70, 5607-5612; DOI:
  [10.1099/ijsem.0.004332](https://doi.org/10.1099/ijsem.0.004332).
- **List of Recommended Names for Bacteria of Medical Importance**:
  Göker, M., Christensen, H., Fingerle, V., Kostovski, M., Margos, G.,
  Moore, E.R.B., Oren, A., Patrick, S., Reischl, U. and Vázquez-Boland,
  J.A. (2025). List of Recommended Names for bacteria of medical
  importance: report of the Ad Hoc Committee on Mitigating Changes in
  Prokaryotic Nomenclature. International Journal of Systematic and
  Evolutionary Microbiology, 75(10); DOI:
  [10.1099/ijsem.0.006943](https://doi.org/10.1099/ijsem.0.006943).
- **MycoBank**: <https://www.mycobank.org/>, and its web services,
  `https://webservices.bio-aware.com/westerdijk/mycobank`. Vincent
  Robert, Duong Vu, Ammar Ben Hadj Amor, Nathalie van de Wiele, Carlo
  Brouwer, Bernard Jabas, Szaniszlo Szoke, Ahmed Dridi, Maher Triki,
  Samy ben Daoud, Oussema Chouchen, Lea Vaas, Arthur de Cock, Joost A.
  Stalpers, Dora Stalpers, Gerard J.M. Verkley, Marizeth Groenewald,
  Felipe Borges dos Santos, Gerrit Stegehuis, Wei Li, Linhuan Wu, Run
  Zhang, Juncai Ma, Miaomiao Zhou, Sergio Pérez Gorjón, Lily
  Eurwilaichitr, Supawadee Ingsriswang, Karen Hansen, Conrad Schoch,
  Barbara Robbertse, Laszlo Irinyi, Wieland Meyer, Gianluigi Cardinali,
  David L. Hawksworth, John W. Taylor, and Pedro W. Crous. 2013.
  MycoBank gearing up for new horizons. IMA Fungus · volume 4 · no 2:
  371–379; DOI:
  [10.5598/imafungus.2013.04.02.16](https://doi.org/10.5598/imafungus.2013.04.02.16).
- **International Committee on Taxonomy of Viruses (ICTV)**:
  <https://ictv.global/taxonomy>. Lefkowitz E, Hendrickson R,
  International Committee on Taxonomy of Viruses. [ICTV Master Species
  List (MSL), MSL41.v1, as corrected on 14 October
  2026](https://doi.org/10.5281/zenodo.23376199). Zenodo; 2026. doi:
  10.5281/zenodo.23376199. International Committee on Taxonomy of
  Viruses. [Virus Metadata Resource (VMR), MSL41.v1, as exported on 14
  October
  2026](https://github.com/ICTV-Virus-Knowledgebase/ICTVdatabase).
  ICTVdatabase; 2026. Black EJ, Powell CS, Dempsey DM, Hendrickson RC,
  Mims LR, Lefkowitz EJ. [Virus taxonomy: the database of the
  International Committee on Taxonomy of
  Viruses](https://doi.org/10.1093/nar/gkaf1159). Nucleic Acids Res.
  2026;54(D1):D776-D789. doi: 10.1093/nar/gkaf1159.
- **ECDC HAI-Net ICU protocol**: European Centre for Disease Prevention
  and Control. [Protocol for the surveillance of healthcare-associated
  infections and prevention indicators in European intensive care
  units](https://doi.org/10.2900/7455254): HAI-Net ICU protocol, version
  2.3. Stockholm: ECDC; 2025. doi: 10.2900/7455254.
- **BacDive**: <https://bacdive.dsmz.de/>, read through its API
  (<https://api.bacdive.dsmz.de/>). Schober I, Koblitz J, Sardà Carbasse
  J, et al. [BacDive in 2025: the core database for prokaryotic strain
  data](https://doi.org/10.1093/nar/gkae959). Nucleic Acids Res.
  2025;53(D1):D748-D756. doi: 10.1093/nar/gkae959. BacDive’s content is
  licensed [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

As the list’s licence says, the CDC’s requirements for the use of agency
materials still apply wherever content derived from the NHSN Organism
List, such as that common-commensal status, is reproduced.

## Funding

The NeoIPC project has received funding from the European Union’s
Horizon 2020 research and innovation programme under grant agreement No
965328.
