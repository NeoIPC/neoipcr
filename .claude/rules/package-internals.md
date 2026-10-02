---
paths: "R/**,tests/**"
---

## Package Overview

`neoipcr` is an R package that facilitates working with NeoIPC surveillance data stored in DHIS2. It handles data import, calculation of epidemiological indicators, and data protection.

---

## Integrated Data Protection

A primary design goal is **integrated data protection**: the resulting dataset contains *only* the data the user explicitly requested via `dhis2_dataset_options()`, and supports safe pseudonymization. Data scientists from partner hospitals are among the intended users -- safe defaults must prevent accidental data exposure while still allowing full access to their own site's data.

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

The **host** resolves separately from auth: an explicit `hostname` argument, else the `NEOIPC_DHIS2_HOST` environment variable, else `rlang::abort()`. `neoipcr` is a public library with **no baked-in deployment host** — the Charité default lives in the report tooling (`reports/common/helpers.R::get_connection_options()`), not here. With `NEOIPC_DHIS2_HOST` and credentials both in the environment, `import_dhis2()` runs argument-free.

### `get_password()` interactive guard

If `NEOIPC_DHIS2_USER` is set but `NEOIPC_DHIS2_PASSWORD` is not, `get_password()` checks `interactive()` before calling `askpass::askpass()`. In non-interactive sessions (e.g., Quarto renders), it aborts with a clear error instead of hanging.

### Token format

DHIS2 personal access tokens match `d2pat_` prefix + 42 characters (48 total). `read_token()` accepts either a raw token string or a file path containing the token, and validates **only** the prefix and the 48-char length. DHIS2 itself generates the body as a 32-char Base64 random part + a 10-digit CRC32 checksum (`ApiTokenServiceImpl.initToken`); the first body char can be a digit (it is **not** the letter-first UID form), and neither the leading char nor the checksum is verified on the 2.40 auth path (`ApiTokenAuthManager` hashes + looks up the key). So a fixture token only needs the prefix + length — no valid checksum.

---

## String Coercion

`dhis2_connection_options()` coerces `port` via `as.integer()` so that string values passed from Quarto params work without manual conversion. `dhis2_dataset_options()` similarly coerces numeric parameters (`birth_weight_from/to`, `gestational_age_from/to`).

---

## DHIS2 Test Units

Test organisation units live outside the real country hierarchy:

- Real hierarchy: **Root (NEOIPC) -> Country -> Hospital -> Department**
- Test hierarchy: **Root (NEOIPC) -> TEST_UNITS -> Department** (no hospital level)

Because test units have **no country, no hospital, and no World Bank income class**, code must tolerate `NA` in these keys when `include_test_data = TRUE`. Use `left_join` (not `inner_join`) when joining with countries or World Bank classes so that test data is preserved with `NA` keys.

A department counts as a test unit when it is a member of the `TEST_UNITS` org-unit group **or** carries the `IsTestunit` custom attribute (`TRUE_ONLY`) itself. The flagged departments are fetched on every import, whatever `include_custom_attributes` says, through a narrowed follow-up request that returns ids only (see Org-unit Attributes below). A test flag on an ancestor — a hospital's group membership or attribute — is not consulted.

---

## Org-unit Attributes

DHIS2 custom attributes on organisation units are imported on request through `dhis2_dataset_options(include_custom_attributes = ...)`, an opt-in axis like `include_dhis2_ids` that names the entities whose values to import (`"departments"`, `"hospitals"`). Three metadata tibbles carry them:

| Tibble | Columns | Present when |
|---|---|---|
| `metadata$orgUnitAttributes` | `code`, `name`, `valueType` (the DHIS2 enum name, as character); coded definitions only, since a value is addressed by code and a code-less definition's values never arrive | any opted-in entity is present (`include_<entity>` not `"no"`) |
| `metadata$departmentAttributeValues` | `department_key`, `attribute_code`, six typed `value_*` columns | `"departments"` opted in and `include_department != "no"` |
| `metadata$hospitalAttributeValues` | `hospital_key`, `attribute_code`, six typed `value_*` columns | `"hospitals"` opted in and `include_hospital != "no"` |

A closed gate yields a 0×0 tibble; there is no "pseudo" tier, because the opt-in is the whole contract. The values can carry personal data (a site's contact person), so opting in under a pseudonymized entity re-identifies it — the caller's explicit choice, as with `include_dhis2_ids`. Without the opt-in no attribute value is requested at all — the `IsTestunit` flag arrives as org-unit ids from its own request — so a malformed value can only ever be reported to a caller who receives the values.

**Typed values.** Each value row fills at most one typed column — the one of its attribute's value-type family, through the families DHIS2 declares in `ValueType.java` (`value_type_family()` in `R/dhis2-metadata-orgunits.R`); a missing or unparseable value leaves every typed column `NA`:

| DHIS2 value types | Column |
|---|---|
| `INTEGER`, `INTEGER_POSITIVE`, `INTEGER_NEGATIVE`, `INTEGER_ZERO_OR_POSITIVE` | `value_integer` |
| `NUMBER`, `UNIT_INTERVAL`, `PERCENTAGE` | `value_number` |
| `BOOLEAN`, `TRUE_ONLY` | `value_logical` |
| `DATE`, `AGE` | `value_date` |
| `DATETIME` | `value_datetime` (UTC) |
| every other type, and any value type the package does not know | `value_text` |

The raw string is not kept. A value that does not parse under its family becomes `NA` in every typed column and is reported in a single `neoipcr_attribute_value_parse_failure` warning per resolved table, listing each failing attribute code with its count — never the value.

**Resolution by code.** The definitions (`/api/metadata` `attributes`, filtered to `organisationUnitAttribute:eq:true`) are requested on every import, and a value's attribute UID is resolved to `attribute_code` through them; no UID is public on any of the three tibbles. A value whose definition the caller cannot read (the contact-person attributes are shared privately) is dropped and counted in a debug log line. Values are requested only for the opted-in entities, and resolved for the org units that survived the metadata narrowing, so a pruned department's value never raises a warning.

**`IsTestunit`.** Evaluated on every import regardless of the opt-in through a follow-up request that returns ids only: DHIS2 filters metadata objects by one attribute's value with `filter=<attribute uid>:eq:<value>` — the query parser turns a path that is no schema property but a valid uid into an attribute restriction (`DefaultJpaQueryParser.getRestriction()` → `asAttribute()`), and since `withinUserHierarchy` preselects the org units and sets them on the query, `DefaultQueryService.queryObjects()` hands the whole query to the in-memory engine (`InMemoryQueryEngine.getValue()` → `getAttributeValue(uid)`, every restriction AND-ed; the query planner is not reached on this path); identical on 2.40 and 2.41 — and the uid is resolved by code from the definitions of the same import. The ids are folded into `isTest` (see DHIS2 Test Units above) alongside the `TEST_UNITS` membership; a caller who cannot read the definition gets no flag from this source. Under the opt-in the attribute's rows are dropped from the values tables. `assert_data_protection()` aborts if an `IsTestunit` row survives, or if a values table carries columns without the opt-in — the contract is the shape: a closed gate yields a 0×0 tibble, so a schema-shaped table there, rows or not, means a reader ignored the gate.

**Post-filter.** The values tables are leaves: `apply_postfilter()` prunes them to the surviving departments / hospitals after the hierarchy cascade, and they never anchor that cascade.

---

## Gestational Age

Two tracked entity attributes store gestational age -- both **must** be set consistently when importing data:

| TEA code | Format | Example | Purpose |
|---|---|---|---|
| `NEOIPC_TEA_GEST_AGE` | `weeks+days` (text) | `25+4` | Human-friendly display in DHIS2 UI |
| `NeoIPC_TEA_TOTAL_GESTATION_DAYS` | integer (total days) | `179` | Used by neoipcr and DHIS2 program rules for all calculations |

**Note the inconsistent casing** of `NeoIPC_TEA_TOTAL_GESTATION_DAYS` -- this cannot be changed due to downstream dependencies.

Conversion: `total_days = weeks * 7 + days` (e.g. `25+4` -> `25*7 + 4 = 179`).
