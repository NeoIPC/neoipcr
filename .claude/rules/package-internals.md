---
paths: "R/**,tests/**"
---

## Integrated Data Protection

A primary design goal is **integrated data protection**: the resulting dataset contains *only* the data the user explicitly requested via `dhis2_dataset_options()`, and supports safe pseudonymization. Data scientists from partner hospitals are among the intended users, so safe defaults prevent accidental data exposure while still allowing full access to their own site's data.

### Progressive Narrowing

Every stage of the `import_dhis2()` pipeline sheds data it no longer needs:

1. **API request**: fetch only the organisation units the user asked for (`ouMode` and `orgUnit`).
2. **Metadata processing**: filter countries, departments, and hospitals early; build only the hierarchy columns downstream steps will use.
3. **`read_patients/enrollments/events`**: join and keep only the columns the remaining processing and the final output need.
4. **`assert_data_protection()`**: the final guardian, which asserts that every reader has honoured the user's `include_*` options and aborts on a reader regression; under the schema contract the readers own every tibble's shape, so it asserts and scrubs nothing.

### Redundant Foreign Keys

Hierarchy keys (`department_key`, `hospital_key`, `country_key`, `world_bank_class_key`) appear *directly* in patients, enrollments, and events, not only via the relational chain. This allows breaking the chain without losing the ability to classify records by higher-level categories (events can carry `world_bank_class_key` without exposing the country or patient).

### `assert_data_protection()` as the Authoritative Guardian

`assert_data_protection()` is the **authoritative guardian** that verifies no unauthorized data leaks into the final dataset. It runs last in the `import_dhis2()` pipeline. Under the schema contract every reader owns its tibble's shape, so this function's job is to **assert invariants**, not to scrub columns: every branch is an assertion, and the `event` id gate lives in `events_cols`. A schema regression that leaks a column reserved for another option value surfaces here with an actionable `rlang::abort()` naming the leaking tibble and the column.

---

## Authentication

neoipcr is the **single authentication authority**. All other components (PowerShell scripts, Docker containers, R data scripts) feed credentials into neoipcr via environment variables or function parameters.

### Credential Resolution in `dhis2_connection_options()`

When no explicit `token`, `username`, or `session_id` parameter is provided, `get_auth_data()` falls back to environment variables:

1. `NEOIPC_DHIS2_SESSION_ID` -> session_id (Docker only)
2. `NEOIPC_DHIS2_TOKEN` -> token (validated via `read_token()`)
3. `NEOIPC_DHIS2_USER` + `NEOIPC_DHIS2_PASSWORD` -> username/password
4. `interactive()` -> prompt for username/password via `readline()` + `askpass::askpass()`
5. `!interactive()` -> `rlang::abort()` with actionable error message

The **host** resolves separately from the credentials: an explicit `hostname` argument, else the `NEOIPC_DHIS2_HOST` environment variable, else `rlang::abort()`. `neoipcr` is a public library with **no baked-in deployment host** — the Charité default lives in the Surveillance-Toolkit's report tooling (`get_connection_options()` in its `reports/common/helpers.R`), not here. With `NEOIPC_DHIS2_HOST` and credentials both in the environment, `import_dhis2()` runs argument-free.

### Interactive Guard in `get_password()`

When a username is known (the `username` argument, `NEOIPC_DHIS2_USER`, or the prompt) but `NEOIPC_DHIS2_PASSWORD` is not set, `get_password()` checks `interactive()` before calling `askpass::askpass()`. In non-interactive sessions, such as Quarto renders, it aborts with a clear error instead of hanging; its caller says through `username_from_env` whether the username came from `NEOIPC_DHIS2_USER`, and the message names that variable only then.

### Token Format

DHIS2 personal access tokens match the `d2pat_` prefix and 42 characters (48 in total). `read_token()` accepts either a raw token string or a file path containing the token, and validates **only** the prefix and the 48-character length. DHIS2 itself generates the body as a 32-character Base64 random part and a 10-digit CRC32 checksum (`ApiTokenServiceImpl.initToken`); the first body character can be a digit (it is **not** the letter-first UID form), and neither the leading character nor the checksum is verified on the 2.40 authentication path (`ApiTokenAuthManager` hashes and looks up the key). So a fixture token needs only the prefix and the length, no valid checksum.

---

## String Coercion

`dhis2_connection_options()` coerces `port` via `as.integer()` so that string values passed from Quarto params work without manual conversion. `dhis2_dataset_options()` similarly coerces numeric parameters (`birth_weight_from/to`, `gestational_age_from/to`).

---

## DHIS2 Test Units

Test organisation units live outside the real country hierarchy:

- Real hierarchy: **Root (NEOIPC) -> Country -> Hospital -> Department**
- Test hierarchy: **Root (NEOIPC) -> TEST_UNITS -> Department** (no hospital level)

Because test units have **no country, no hospital, and no World Bank income class**, code must tolerate `NA` in these keys when `include_test_data = TRUE`. Use `left_join` (not `inner_join`) when joining with countries or World Bank income classes so that test data is preserved with `NA` keys.

A department counts as a test unit when it is a member of the `TEST_UNITS` organisation-unit group **or** carries the `IsTestunit` custom attribute (`TRUE_ONLY`) itself. The flagged departments are fetched on every import, whatever `include_custom_attributes` says, through a narrowed follow-up request that returns ids only (see Organisation-Unit Attributes below). A test flag on an ancestor — a hospital's group membership or attribute — is not consulted.

---

## Organisation-Unit Attributes

DHIS2 custom attributes on organisation units are imported on request through `dhis2_dataset_options(include_custom_attributes = ...)`, an opt-in axis like `include_dhis2_ids` that names the entities whose values to import (`"departments"`, `"hospitals"`). Three metadata tibbles carry them:

| Tibble | Columns | Present when |
|---|---|---|
| `metadata$orgUnitAttributes` | `code`, `name`, `valueType` (the DHIS2 enum name, as character); coded definitions only, since a value is addressed by code and a code-less definition's values never arrive | any opted-in entity is present (`include_<entity>` not `"no"`) |
| `metadata$departmentAttributeValues` | `department_key`, `attribute_code`, six typed `value_*` columns | `"departments"` opted in and `include_department != "no"` |
| `metadata$hospitalAttributeValues` | `hospital_key`, `attribute_code`, six typed `value_*` columns | `"hospitals"` opted in and `include_hospital != "no"` |

A closed gate yields a 0×0 tibble; there is no "pseudo" tier, because the opt-in is the whole contract. The values can carry personal data (a site's contact person), so opting in under a pseudonymized entity re-identifies it — the caller's explicit choice, as with `include_dhis2_ids`. Without the opt-in no attribute value is requested at all — the `IsTestunit` flag arrives as organisation-unit ids from its own request — so a malformed value can only ever be reported to a caller who receives the values.

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

**Resolution by code.** The definitions (`/api/metadata` `attributes`, filtered to `organisationUnitAttribute:eq:true`) are requested on every import, and a value's attribute UID is resolved to `attribute_code` through them; no UID is public on any of the three tibbles. A value whose definition the caller cannot read (the contact-person attributes are shared privately) is dropped and counted in a debug log line. Values are requested only for the opted-in entities, and resolved for the organisation units that survived the metadata narrowing, so a pruned department's value never raises a warning.

**`IsTestunit`.** Evaluated on every import regardless of the opt-in through a follow-up request that returns ids only: DHIS2 filters metadata objects by one attribute's value with `filter=<attribute uid>:eq:<value>` — the query parser turns a path that is no schema property but a valid uid into an attribute restriction (`DefaultJpaQueryParser.getRestriction()` → `asAttribute()`), and since `withinUserHierarchy` preselects the organisation units and sets them on the query, `DefaultQueryService.queryObjects()` hands the whole query to the in-memory engine (`InMemoryQueryEngine.getValue()` → `getAttributeValue(uid)`, every restriction AND-ed; the query planner is not reached on this path); identical on 2.40 and 2.41 — and the uid is resolved by code from the definitions of the same import. The ids are folded into `isTest` (see DHIS2 Test Units above) alongside the `TEST_UNITS` membership; a caller who cannot read the definition gets no flag from this source. Under the opt-in the attribute's rows are dropped from the values tables. `assert_data_protection()` aborts if an `IsTestunit` row survives, or if a values table carries columns without the opt-in — the contract is the shape: a closed gate yields a 0×0 tibble, so a schema-shaped table there, rows or not, means a reader ignored the gate.

**Post-filter.** The values tables are leaves: `apply_postfilter()` prunes them to the surviving departments / hospitals after the hierarchy cascade, and they never anchor that cascade.

---

## Gestational Age

Two tracked-entity attributes store gestational age, and both **must** be set consistently when importing data:

| Attribute code | Format | Example | Purpose |
|---|---|---|---|
| `NEOIPC_TEA_GEST_AGE` | `weeks+days` (text) | `25+4` | Display in the DHIS2 user interface |
| `NeoIPC_TEA_TOTAL_GESTATION_DAYS` | integer (total days) | `179` | Used by neoipcr and DHIS2 program rules for all calculations |

**Note the inconsistent casing** of `NeoIPC_TEA_TOTAL_GESTATION_DAYS`: downstream dependencies keep it from being changed.

Conversion: `total_days = weeks * 7 + days` (`25+4` -> `25*7 + 4 = 179`).
