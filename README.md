
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

## Funding

The NeoIPC project has received funding from the European Union’s
Horizon 2020 research and innovation programme under grant agreement No
965328.
