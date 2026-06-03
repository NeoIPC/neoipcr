# Tests for R/schema-orgunits.R — entity column declarations for the
# org-unit hierarchy (World Bank classes today; countries / hospitals /
# departments in subsequent Phase B sub-tasks).
#
# These tests are schema-engine level: they exercise the column lists
# and `compile_schema()` output directly, without constructing DHIS2
# metadata input. Reader-level tests that exercise the three-mode
# contract on populated metadata live in `test-dhis2-metadata.R`.

# --- worldBankClasses_cols — per-column include_when predicates ---

test_that("col_wb_class_key appears iff include_world_bank_class != 'no'", {
  key_col <- neoipcr:::col_wb_class_key

  expect_false(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

test_that("`class` column appears only under include_world_bank_class = 'full'", {
  class_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "class")
  expect_false(is.null(class_col))

  expect_false(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_false(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(class_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

test_that("`class` column declares fixed factor levels c('L','LM','UM','H')", {
  class_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "class")
  expect_identical(class_col$factor_levels, c("L", "LM", "UM", "H"))
  expect_identical(class_col$levels_source, "fixed")
})

test_that("`fiscal_year` column appears only under 'full'", {
  fy_col <- purrr::detect(
    neoipcr:::worldBankClasses_cols, \(c) c$name == "fiscal_year")
  expect_false(is.null(fy_col))

  expect_false(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "no")))
  expect_false(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "pseudo")))
  expect_true(fy_col$include_when(dhis2_dataset_options(
    include_world_bank_class = "full")))
})

# --- get_worldBankClasses_schema — three-mode shape contract ---

test_that("get_worldBankClasses_schema is 0x0 under 'no'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "no"))

  expect_schema_matches(schema, tibble::tibble())
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("get_worldBankClasses_schema is single-column under 'pseudo'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "pseudo"))

  expect_equal(ncol(schema), 1L)
  expect_equal(nrow(schema), 0L)
  expect_identical(names(schema), "world_bank_class_key")
  expect_true(is.integer(schema$world_bank_class_key))
})

test_that("get_worldBankClasses_schema is full-schema under 'full'", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "full"))

  expect_equal(ncol(schema), 3L)
  expect_equal(nrow(schema), 0L)
  expect_identical(
    names(schema),
    c("world_bank_class_key", "class", "fiscal_year"))
  expect_true(is.integer(schema$world_bank_class_key))
  expect_true(is.factor(schema$class))
  expect_identical(levels(schema$class), c("L", "LM", "UM", "H"))
  expect_true(is.integer(schema$fiscal_year))
})

test_that("get_worldBankClasses_schema strict 0 -> 1 -> N column-count progression", {
  opts_no     <- dhis2_dataset_options(include_world_bank_class = "no")
  opts_pseudo <- dhis2_dataset_options(include_world_bank_class = "pseudo")
  opts_full   <- dhis2_dataset_options(include_world_bank_class = "full")

  expect_equal(ncol(neoipcr:::get_worldBankClasses_schema(opts_no)),     0L)
  expect_equal(ncol(neoipcr:::get_worldBankClasses_schema(opts_pseudo)), 1L)
  expect_true(ncol(neoipcr:::get_worldBankClasses_schema(opts_full))    > 1L)
})

test_that("get_worldBankClasses_schema does not depend on unrelated options", {
  # Iterate every other relevant option field; schema shape must follow
  # only `include_world_bank_class`.
  for (opts in iter_dataset_options(
    fields = c("include_country", "include_hospital"))) {
    for (wb_mode in c("no", "pseudo", "full")) {
      opts$include_world_bank_class <- wb_mode
      schema <- neoipcr:::get_worldBankClasses_schema(opts)

      expected_ncol <- switch(wb_mode, "no" = 0L, "pseudo" = 1L, "full" = 3L)
      expect_equal(ncol(schema), expected_ncol,
        info = sprintf(
          "wb_mode='%s', include_country='%s', include_hospital='%s'",
          wb_mode, opts$include_country, opts$include_hospital))
    }
  }
})

# --- assert_schema sanity: builder output matches declared schema ---

test_that("make_test_metadata_wb_classes('full') matches full schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "full"))
  fixture <- make_test_metadata_wb_classes(
    n = 2, include_world_bank_class = "full")

  expect_schema_matches(fixture, schema)
  expect_equal(nrow(fixture), 2L)
})

test_that("make_test_metadata_wb_classes('pseudo') matches pseudo schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "pseudo"))
  fixture <- make_test_metadata_wb_classes(
    n = 3, include_world_bank_class = "pseudo")

  expect_schema_matches(fixture, schema)
  expect_equal(nrow(fixture), 3L)
  expect_identical(names(fixture), "world_bank_class_key")
})

test_that("make_test_metadata_wb_classes('no') matches empty schema", {
  schema <- neoipcr:::get_worldBankClasses_schema(
    dhis2_dataset_options(include_world_bank_class = "no"))
  fixture <- make_test_metadata_wb_classes(include_world_bank_class = "no")

  expect_schema_matches(fixture, schema)
  expect_equal(ncol(fixture), 0L)
  expect_equal(nrow(fixture), 0L)
})

# ---- countries_cols — per-column include_when predicates ---

test_that("col_country_key appears iff include_country != 'no'", {
  key_col <- neoipcr:::col_country_key

  expect_false(key_col$include_when(dhis2_dataset_options(
    include_country = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_country = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_country = "full")))
})

test_that("country display columns appear only under include_country = 'full'", {
  # All four display columns are requested from DHIS2 under `include_country
  # == "full"` via `organisationUnitGroups:fields` in R/dhis2-metadata.R.
  for (name in c("code", "displayName", "displayShortName",
                 "displayDescription")) {
    col <- purrr::detect(
      neoipcr:::countries_cols, \(c) c$name == name)
    expect_false(is.null(col))
    expect_false(col$include_when(dhis2_dataset_options(
      include_country = "no")),
      info = name)
    expect_false(col$include_when(dhis2_dataset_options(
      include_country = "pseudo")),
      info = name)
    expect_true(col$include_when(dhis2_dataset_options(
      include_country = "full")),
      info = name)
    expect_identical(col$levels_source, "data", info = name)
  }
})

test_that("countries schema has world_bank_class_key iff BOTH country and WB are non-'no'", {
  # Tests the direct parent-link FK contract at the SCHEMA level, not
  # at the atom level. The `world_bank_class_key` column on countries
  # is the shared `col_wb_class_key` atom (predicate:
  # `include_world_bank_class != "no"`) gated by the countries
  # containing-entity gate (`include_country != "no"`). Neither alone
  # encodes the compound rule; the composition does — which is the
  # whole point of `with_entity_gate`.
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
        include_country = cmode, include_world_bank_class = wbmode))
      expected <- cmode != "no" && wbmode != "no"
      expect_equal(
        "world_bank_class_key" %in% names(schema),
        expected,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_countries_schema honors 0 -> 1 -> N progression across full cross-product", {
  # Regression for the latent containing-entity-gate gap: under every
  # combination of `include_country × include_world_bank_class`, the
  # schema shape must obey the strict progression. In particular,
  # under `include_country = "no"` + any WB mode, the schema must be
  # 0×0 — no stray `world_bank_class_key` leaking through because the
  # shared atom's predicate would otherwise fire.
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_country = cmode, include_world_bank_class = wbmode)
      schema <- neoipcr:::get_countries_schema(opts)

      expected_ncol <- if (cmode == "no") {
        0L
      } else if (cmode == "pseudo") {
        if (wbmode == "no") 1L else 2L  # country_key (+ wb FK if WB exists)
      } else {  # "full"
        if (wbmode == "no") 6L else 7L  # + name + 4 display cols (+ wb FK)
      }

      expect_equal(ncol(schema), expected_ncol,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_equal(nrow(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

# --- get_countries_schema — three-mode shape contract ---

test_that("get_countries_schema is 0x0 under include_country = 'no'", {
  # Strict 0×0 regardless of include_world_bank_class (inheritance
  # through an absent entity is still absent).
  for (wb_mode in c("no", "pseudo", "full")) {
    opts <- dhis2_dataset_options(
      include_country = "no", include_world_bank_class = wb_mode)
    schema <- neoipcr:::get_countries_schema(opts)
    expect_equal(ncol(schema), 0L,
      info = sprintf("wb_mode='%s'", wb_mode))
    expect_equal(nrow(schema), 0L,
      info = sprintf("wb_mode='%s'", wb_mode))
  }
})

test_that("get_countries_schema is 1 column under include_country='pseudo', wb='no'", {
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "pseudo", include_world_bank_class = "no"))

  expect_equal(ncol(schema), 1L)
  expect_identical(names(schema), "country_key")
  expect_true(is.integer(schema$country_key))
})

test_that("get_countries_schema is 2 columns under pseudo + wb non-'no'", {
  # Under pseudo mode, the public schema keeps `country_key` + the
  # direct WB-class link FK — that's how pseudo countries still group
  # into WB classes.
  for (wb_mode in c("pseudo", "full")) {
    schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
      include_country = "pseudo", include_world_bank_class = wb_mode))
    expect_equal(ncol(schema), 2L,
      info = sprintf("wb_mode='%s'", wb_mode))
    expect_identical(
      names(schema),
      c("country_key", "world_bank_class_key"),
      info = sprintf("wb_mode='%s'", wb_mode))
  }
})

test_that("get_countries_schema is full schema under include_country='full'", {
  # Full schema with all display columns + direct WB-class link when WB
  # is non-"no".
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "full"))

  expect_equal(ncol(schema), 7L)
  expect_identical(
    names(schema),
    c("country_key", "name", "code", "displayName", "displayShortName",
      "displayDescription", "world_bank_class_key"))
  expect_true(is.integer(schema$country_key))
  expect_true(is.character(schema$name))
  expect_s3_class(schema$code, "ordered")
  expect_s3_class(schema$displayName, "ordered")
  expect_s3_class(schema$displayShortName, "ordered")
  expect_s3_class(schema$displayDescription, "ordered")
  expect_true(is.integer(schema$world_bank_class_key))
})

test_that("get_countries_schema full - wb_no = 6 columns (no WB FK)", {
  schema <- neoipcr:::get_countries_schema(dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "no"))

  expect_equal(ncol(schema), 6L)
  expect_identical(
    names(schema),
    c("country_key", "name", "code", "displayName", "displayShortName",
      "displayDescription"))
})

test_that("get_countries_schema strict 0 -> 1 -> N column-count progression under wb='no'", {
  opts_no     <- dhis2_dataset_options(
    include_country = "no", include_world_bank_class = "no")
  opts_pseudo <- dhis2_dataset_options(
    include_country = "pseudo", include_world_bank_class = "no")
  opts_full   <- dhis2_dataset_options(
    include_country = "full", include_world_bank_class = "no")

  expect_equal(ncol(neoipcr:::get_countries_schema(opts_no)),     0L)
  expect_equal(ncol(neoipcr:::get_countries_schema(opts_pseudo)), 1L)
  expect_true(ncol(neoipcr:::get_countries_schema(opts_full))   > 1L)
})

test_that("make_test_metadata_countries matches schema across all (country, wb) modes", {
  for (c_mode in c("no", "pseudo", "full")) {
    for (wb_mode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_country = c_mode, include_world_bank_class = wb_mode)
      schema  <- neoipcr:::get_countries_schema(opts)
      fixture <- make_test_metadata_countries(
        n = 2,
        include_country = c_mode,
        include_world_bank_class = wb_mode)

      expect_schema_matches(fixture, schema)
    }
  }
})

# ---- hospitals_cols — per-column include_when predicates ---

test_that("col_hospital_key appears iff include_hospital != 'no'", {
  key_col <- neoipcr:::col_hospital_key
  expect_false(key_col$include_when(dhis2_dataset_options(
    include_hospital = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_hospital = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_hospital = "full")))
})

test_that("hospitals orgUnit appears iff 'hospitals' in include_dhis2_ids", {
  col <- purrr::detect(
    neoipcr:::hospitals_cols, \(c) c$name == "orgUnit")
  expect_false(is.null(col))
  expect_false(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = character())))
  expect_true(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = "hospitals")))
})

test_that("hospitals display columns appear only under include_hospital = 'full'", {
  for (name in c("code", "displayName", "displayShortName",
                 "displayDescription", "comment",
                 "longitude", "latitude")) {
    col <- purrr::detect(
      neoipcr:::hospitals_cols, \(c) c$name == name)
    expect_false(is.null(col), info = name)
    expect_false(col$include_when(dhis2_dataset_options(
      include_hospital = "pseudo")),
      info = name)
    expect_true(col$include_when(dhis2_dataset_options(
      include_hospital = "full")),
      info = name)
  }
})

test_that("hospitals country_key (direct link-FK) appears iff BOTH include_hospital and include_country are non-'no'", {
  # `col_country_key`'s single-option predicate is `include_country != "no"`;
  # the hospitals entity-gate supplies the `include_hospital != "no"` half.
  # Result: the column only appears at the schema level when both halves pass.
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
        include_hospital = hmode, include_country = cmode))
      expected <- hmode != "no" && cmode != "no"
      expect_equal(
        "country_key" %in% names(schema),
        expected,
        info = sprintf("h=%s, c=%s", hmode, cmode))
    }
  }
})

test_that("hospitals world_bank_class_key follows the inheritance rule", {
  # The key appears on hospitals only when:
  #   - include_hospital != "no" (entity exists), AND
  #   - include_world_bank_class != "no" (key is meaningful), AND
  #   - countries' compiled schema doesn't already carry it
  #     (countries has it when include_country != "no", so inheritance
  #     only fires under include_country == "no").
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      for (wbmode in c("no", "pseudo", "full")) {
        schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
          include_hospital         = hmode,
          include_country          = cmode,
          include_world_bank_class = wbmode))
        expected <-
          hmode != "no" &&
          wbmode != "no" &&
          cmode == "no"
        expect_equal(
          "world_bank_class_key" %in% names(schema),
          expected,
          info = sprintf("h=%s, c=%s, wb=%s", hmode, cmode, wbmode))
      }
    }
  }
})

# --- get_hospitals_schema — three-mode shape contract ---

test_that("get_hospitals_schema is 0x0 under include_hospital = 'no' regardless of country/WB mode", {
  for (cmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      opts <- dhis2_dataset_options(
        include_hospital         = "no",
        include_country          = cmode,
        include_world_bank_class = wbmode)
      schema <- neoipcr:::get_hospitals_schema(opts)
      expect_equal(ncol(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_equal(nrow(schema), 0L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_hospitals_schema is 1 column under pseudo + country='no' + wb='no'", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "pseudo",
    include_country          = "no",
    include_world_bank_class = "no",
    include_dhis2_ids        = character()))
  expect_equal(ncol(schema), 1L)
  expect_identical(names(schema), "hospital_key")
})

test_that("get_hospitals_schema under pseudo + country='no' + wb!='no' has 2 cols (inherited WB)", {
  for (wbmode in c("pseudo", "full")) {
    schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
      include_hospital         = "pseudo",
      include_country          = "no",
      include_world_bank_class = wbmode,
      include_dhis2_ids        = character()))
    expect_equal(ncol(schema), 2L, info = sprintf("wb=%s", wbmode))
    expect_identical(
      names(schema),
      c("hospital_key", "world_bank_class_key"),
      info = sprintf("wb=%s", wbmode))
  }
})

test_that("get_hospitals_schema under pseudo + country!='no' has 2 cols (country_key, no inherited WB)", {
  # Inheritance rule: countries carries wb_class_key here, so hospitals
  # does NOT — reach it via one-hop join through country_key.
  for (cmode in c("pseudo", "full")) {
    for (wbmode in c("pseudo", "full")) {
      schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
        include_hospital         = "pseudo",
        include_country          = cmode,
        include_world_bank_class = wbmode,
        include_dhis2_ids        = character()))
      expect_equal(ncol(schema), 2L,
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
      expect_identical(
        names(schema),
        c("hospital_key", "country_key"),
        info = sprintf("c=%s, wb=%s", cmode, wbmode))
    }
  }
})

test_that("get_hospitals_schema under full + full country + full WB has the expected 10 columns", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_dhis2_ids        = "hospitals"))
  expect_identical(
    names(schema),
    c("hospital_key", "orgUnit", "code", "displayName", "displayShortName",
      "displayDescription", "comment", "longitude", "latitude",
      "country_key"))
  # No `world_bank_class_key` — inheritance says countries carries it.
  expect_false("world_bank_class_key" %in% names(schema))
})

test_that("get_hospitals_schema under full + country='no' + full WB materializes inherited WB key", {
  schema <- neoipcr:::get_hospitals_schema(dhis2_dataset_options(
    include_hospital         = "full",
    include_country          = "no",
    include_world_bank_class = "full",
    include_dhis2_ids        = "hospitals"))
  # `country_key` absent (countries gate = "no"); `world_bank_class_key`
  # inherited directly because countries can't relay.
  expect_false("country_key" %in% names(schema))
  expect_true("world_bank_class_key" %in% names(schema))
})

test_that("make_test_metadata_hospitals matches schema across key (h, c, wb) combinations", {
  # Full 27-combo cross-product of (hospital × country × WB modes) on
  # both include_dhis2_ids configurations (hospitals present or not).
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      for (wbmode in c("no", "pseudo", "full")) {
        for (ids in list(character(), "hospitals")) {
          opts <- dhis2_dataset_options(
            include_hospital         = hmode,
            include_country          = cmode,
            include_world_bank_class = wbmode,
            include_dhis2_ids        = ids)
          schema  <- neoipcr:::get_hospitals_schema(opts)
          fixture <- make_test_metadata_hospitals(
            n = 2,
            include_hospital         = hmode,
            include_country          = cmode,
            include_world_bank_class = wbmode,
            include_dhis2_ids        = ids)

          expect_schema_matches(fixture, schema)
        }
      }
    }
  }
})

# ---- departments_cols — per-column include_when predicates ---

test_that("col_department_key appears iff include_department != 'no'", {
  key_col <- neoipcr:::col_department_key
  expect_false(key_col$include_when(dhis2_dataset_options(
    include_department = "no")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_department = "pseudo")))
  expect_true(key_col$include_when(dhis2_dataset_options(
    include_department = "full")))
})

test_that("departments orgUnit appears iff 'departments' in include_dhis2_ids", {
  col <- purrr::detect(
    neoipcr:::departments_cols, \(c) c$name == "orgUnit")
  expect_false(is.null(col))
  expect_false(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = character())))
  expect_true(col$include_when(dhis2_dataset_options(
    include_dhis2_ids = "departments")))
})

test_that("departments display / geometry / openingDate columns appear only under include_department = 'full'", {
  for (name in c("code", "displayName", "displayShortName",
                 "displayDescription", "comment", "openingDate",
                 "longitude", "latitude")) {
    col <- purrr::detect(
      neoipcr:::departments_cols, \(c) c$name == name)
    expect_false(is.null(col), info = name)
    expect_false(col$include_when(dhis2_dataset_options(
      include_department = "pseudo")),
      info = name)
    expect_true(col$include_when(dhis2_dataset_options(
      include_department = "full")),
      info = name)
  }
})

test_that("departments pre-joined country_key appears iff dept='full' AND country!='no'", {
  # Under the pragmatic direct-materialization design (not the strict
  # inheritance rule), departments carries country_key as a pre-joined
  # fat-lookup column — but only under "full" dept mode where the full
  # schema is in play. Pseudo dept trims to the PK + link-FK.
  for (dmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
        include_department = dmode, include_country = cmode))
      expected <- dmode == "full" && cmode != "no"
      expect_equal(
        "country_key" %in% names(schema),
        expected,
        info = sprintf("d=%s, c=%s", dmode, cmode))
    }
  }
})

test_that("departments pre-joined world_bank_class_key appears iff dept='full' AND wb!='no'", {
  for (dmode in c("no", "pseudo", "full")) {
    for (wbmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
        include_department = dmode, include_world_bank_class = wbmode))
      expected <- dmode == "full" && wbmode != "no"
      expect_equal(
        "world_bank_class_key" %in% names(schema),
        expected,
        info = sprintf("d=%s, wb=%s", dmode, wbmode))
    }
  }
})

test_that("departments hospital_key (direct link-FK) appears iff dept!='no' AND hospital!='no'", {
  for (dmode in c("no", "pseudo", "full")) {
    for (hmode in c("no", "pseudo", "full")) {
      schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
        include_department = dmode, include_hospital = hmode))
      expected <- dmode != "no" && hmode != "no"
      expect_equal(
        "hospital_key" %in% names(schema),
        expected,
        info = sprintf("d=%s, h=%s", dmode, hmode))
    }
  }
})

test_that("departments isTest appears iff include_test_data = TRUE AND dept != 'no'", {
  for (dmode in c("no", "pseudo", "full")) {
    for (td in c(FALSE, TRUE)) {
      schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
        include_department = dmode, include_test_data = td))
      expected <- dmode != "no" && isTRUE(td)
      expect_equal(
        "isTest" %in% names(schema),
        expected,
        info = sprintf("d=%s, td=%s", dmode, td))
    }
  }
})

# --- get_departments_schema — three-mode shape contract ---

test_that("get_departments_schema is 0x0 under include_department = 'no' regardless of other modes", {
  for (hmode in c("no", "pseudo", "full")) {
    for (cmode in c("no", "pseudo", "full")) {
      for (td in c(FALSE, TRUE)) {
        opts <- dhis2_dataset_options(
          include_department = "no",
          include_hospital   = hmode,
          include_country    = cmode,
          include_test_data  = td)
        schema <- neoipcr:::get_departments_schema(opts)
        expect_equal(ncol(schema), 0L,
          info = sprintf("h=%s, c=%s, td=%s", hmode, cmode, td))
      }
    }
  }
})

test_that("get_departments_schema pseudo has just {PK, hospital_key link, isTest?}", {
  # Pseudo departments = PK + direct link-FK (hospital_key) + isTest if
  # include_test_data. No display cols, no pre-joined hierarchy keys,
  # no orgUnit (unless dhis2_ids includes it).
  schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
    include_department = "pseudo",
    include_hospital   = "full",
    include_dhis2_ids  = character()))
  expect_equal(ncol(schema), 2L)
  expect_identical(
    names(schema),
    c("department_key", "hospital_key"))
})

test_that("get_departments_schema full + full hierarchy has all hierarchy keys + display cols", {
  schema <- neoipcr:::get_departments_schema(dhis2_dataset_options(
    include_department       = "full",
    include_hospital         = "full",
    include_country          = "full",
    include_world_bank_class = "full",
    include_dhis2_ids        = "departments",
    include_test_data        = TRUE))
  expect_identical(
    names(schema),
    c("department_key", "orgUnit", "code", "displayName", "displayShortName",
      "displayDescription", "comment", "openingDate", "longitude", "latitude",
      "hospital_key", "country_key", "world_bank_class_key", "isTest"))
})

test_that("make_test_metadata_departments matches schema across key mode combinations", {
  # Sample the space: full cross-product is 3×3×3×3×2×2 = 324 combos;
  # exercise a reasonable subset to keep test runtime bounded. The
  # 27-combo per-column tests above already pin the per-column rules.
  for (dmode in c("no", "pseudo", "full")) {
    for (hmode in c("no", "pseudo", "full")) {
      for (cmode in c("no", "full")) {
        for (wbmode in c("no", "full")) {
          for (td in c(FALSE, TRUE)) {
            for (ids in list(character(), "departments")) {
              opts <- dhis2_dataset_options(
                include_department       = dmode,
                include_hospital         = hmode,
                include_country          = cmode,
                include_world_bank_class = wbmode,
                include_dhis2_ids        = ids,
                include_test_data        = td)
              schema  <- neoipcr:::get_departments_schema(opts)
              fixture <- make_test_metadata_departments(
                n = 2,
                include_department       = dmode,
                include_dhis2_ids        = ids,
                include_hospital         = hmode,
                include_country          = cmode,
                include_world_bank_class = wbmode,
                include_test_data        = td)

              expect_schema_matches(fixture, schema)
            }
          }
        }
      }
    }
  }
})

# ---- Regression: loud finalize_to_schema at orchestrator boundary ---------
#
# The hazard class motivating tasks/neoipcr-schema-arc/schema-finalize-loud.md:
# the orchestrator pre-joins a column onto a hierarchy entity expecting it to
# survive, the schema doesn't declare it, and the old silent drop in
# `finalize_to_schema` composed with downstream `any_of` silent tolerance to
# produce wrong data with no error. These tests pin the new loud behaviour at
# the schema boundary so that any future regression in the orchestrator → reader
# contract surfaces as a clear error, not silent data loss.

test_that("finalize_to_schema errors loudly on undeclared column joined into hospitals", {
  # Simulate an orchestrator pre-join that puts a column onto the hospitals
  # processed tibble that the schema doesn't declare and that isn't listed
  # as scratch. Without the loud check, this would have been silently
  # dropped — turning a schema ↔ orchestrator mismatch into downstream
  # wrong data (per the schema-finalize-loud.md analysis).
  opts <- dhis2_dataset_options(include_hospital = "full")
  rogue <- tibble::tibble(
    hospital_key = integer(),
    code         = character(),
    displayName  = character(),
    displayShortName   = character(),
    displayDescription = character(),
    comment      = character(),
    longitude    = double(),
    latitude     = double(),
    country_key  = integer(),
    # orchestrator mistakenly pre-joined a `rogue_hierarchy_key` that
    # isn't in `hospitals_cols` and isn't in the caller's scratch list.
    rogue_hierarchy_key = integer())

  expect_error(
    neoipcr:::finalize_to_schema(rogue, neoipcr:::hospitals_cols, opts),
    "rogue_hierarchy_key")
  expect_error(
    neoipcr:::finalize_to_schema(rogue, neoipcr:::hospitals_cols, opts),
    "not declared in schema and not in `scratch`")
})

test_that("finalize_to_schema tolerates the `country` scratch column on hospitals", {
  # The production hospitals orchestrator call passes scratch = "country"
  # for the raw DHIS2 parent id it uses internally. Verify that the
  # scratch declaration accepts it without error, matching the
  # production code path.
  opts <- dhis2_dataset_options(include_hospital = "full")
  x <- tibble::tibble(
    hospital_key = integer(),
    code         = character(),
    displayName  = character(),
    displayShortName   = character(),
    displayDescription = character(),
    comment      = character(),
    longitude    = double(),
    latitude     = double(),
    country_key  = integer(),
    # scratch — raw DHIS2 parent id used only inside the orchestrator.
    country      = character())

  out <- neoipcr:::finalize_to_schema(
    x, neoipcr:::hospitals_cols, opts, scratch = "country")
  expect_false("country" %in% names(out))
  expect_identical(names(out), names(
    neoipcr:::compile_schema(neoipcr:::hospitals_cols, opts)))
})

test_that("finalize_to_schema silently drops schema-declared-but-gated-out cols on departments", {
  # Under `include_department = "pseudo"`, departments narrows to
  # `{department_key, hospital_key}` (+ isTest / orgUnit when their
  # gates fire). Display columns (`code`, `displayName`, ...) remain in
  # `departments_cols` but are gated out by `include_when`. A reader
  # that still produces them in the narrower mode has the narrowing
  # delegated to finalize's existing silent-drop-for-gated-out logic —
  # *not* flagged as a mismatch. This is the intentional counterpart to
  # the loud error on undeclared columns.
  opts <- dhis2_dataset_options(include_department = "pseudo",
                                include_hospital   = "pseudo")
  x <- tibble::tibble(
    department_key = integer(),
    hospital_key   = integer(),
    code           = character(),      # declared but gated OFF in pseudo
    displayName    = character())      # declared but gated OFF in pseudo

  out <- neoipcr:::finalize_to_schema(
    x, neoipcr:::departments_cols, opts)
  expect_identical(names(out), c("department_key", "hospital_key"))
})

# ---- Users ----------------------------------------------------------------
#
# Users is a metadata tibble curated by the NeoIPC team, not by partner-site
# data entry. The three-mode contract applies identically to the hierarchy
# metadata entities: 0×0 under `"no"`, single `user_key` column under
# `"pseudo"`, full schema under `"full"`. The username → user_key FK
# lookup used by fact readers goes through `.users_internal_map`, not
# through `metadata$users` — so pseudo mode can stay strictly 1-col
# without breaking the FK resolution path.

test_that("users_cols: `no` mode returns 0x0 via the entity gate", {
  opts <- dhis2_dataset_options(include_user = "no")
  schema <- neoipcr:::compile_schema(neoipcr:::users_cols, opts)
  expect_s3_class(schema, "tbl_df")
  expect_equal(ncol(schema), 0L)
  expect_equal(nrow(schema), 0L)
})

test_that("users_cols: `pseudo` mode is strictly user_key only", {
  opts <- dhis2_dataset_options(include_user = "pseudo")
  schema <- neoipcr:::compile_schema(neoipcr:::users_cols, opts)
  expect_identical(names(schema), "user_key")
  expect_type(schema$user_key, "integer")
})

test_that("users_cols: `pseudo` mode adds `user` when include_dhis2_ids opts in", {
  # The `user` column is gated only on `"users" %in% include_dhis2_ids`
  # (the raw DHIS2-id opt-in axis), same pattern as hospitals' /
  # departments' `orgUnit`. Under pseudo + id-opt-in the tibble is
  # `user_key + user` — the DHIS2 id by itself is opaque and
  # identifies nothing outside DHIS2; content-gated columns
  # (`username`, `firstName`, `email`, …) stay absent.
  opts <- dhis2_dataset_options(
    include_user      = "pseudo",
    include_dhis2_ids = "users")
  schema <- neoipcr:::compile_schema(neoipcr:::users_cols, opts)
  expect_identical(names(schema), c("user_key", "user"))
})

test_that("users_cols: `full` mode exposes all content columns; `user` gated on include_dhis2_ids", {
  # Without "users" in include_dhis2_ids, the raw DHIS2 id (`user`) is
  # absent from the public schema even under "full" — the id is an
  # orthogonal opt-in to the content columns (username / firstName /
  # …). Same two-axis pattern as hospitals' / departments' `orgUnit`.
  opts_no_ids <- dhis2_dataset_options(
    include_user = "full", include_dhis2_ids = character())
  schema_no_ids <- neoipcr:::compile_schema(neoipcr:::users_cols, opts_no_ids)
  expect_identical(
    names(schema_no_ids),
    c("user_key", "username", "firstName", "surname", "email",
      "lastLogin", "created"))

  opts_with_ids <- dhis2_dataset_options(
    include_user = "full", include_dhis2_ids = "users")
  schema_with_ids <- neoipcr:::compile_schema(
    neoipcr:::users_cols, opts_with_ids)
  expect_identical(
    names(schema_with_ids),
    c("user_key", "user", "username", "firstName", "surname", "email",
      "lastLogin", "created"))
})

test_that("users_cols: no companion columns (createdBy/updatedBy/createdAt/updatedAt)", {
  # Metadata tibbles are curated by the NeoIPC team, not partner-site
  # users. Companion columns are reserved for partner-site-entered
  # entities. users_cols must never declare any of these atoms.
  declared <- purrr::map_chr(neoipcr:::users_cols, "name")
  forbidden <- c("createdBy", "updatedBy", "createdAt", "updatedAt")
  expect_setequal(intersect(declared, forbidden), character())
})

test_that("users fixture honors the three-mode contract across include_user x include_dhis2_ids", {
  for (umode in c("no", "pseudo", "full")) {
    for (ids in list(character(), "users")) {
      opts <- dhis2_dataset_options(
        include_user      = umode,
        include_dhis2_ids = ids)
      schema <- neoipcr:::compile_schema(neoipcr:::users_cols, opts)
      fixture <- make_test_metadata_users(
        n = 2,
        include_user      = umode,
        include_dhis2_ids = ids)
      expect_schema_matches(fixture, schema)
    }
  }
})

# ---- Metadata companion-column assertion (assert_data_protection) ---------
#
# Covered here rather than in test-data-protection.R because the
# invariant is tightly coupled to the metadata schema contract exercised
# by the schema-orgunits tests above.

test_that("assert_data_protection errors loudly when metadata carries companion columns", {
  # A metadata-reader regression that accidentally emits `createdBy` /
  # `updatedBy` / `createdAt` / `updatedAt` on a curated metadata
  # tibble must be loud — these columns are reserved for
  # partner-site-entered entities and have no meaning on NeoIPC-
  # curated metadata.
  ds <- make_test_ds(
    metadata = list(
      users       = tibble::tibble(user_key = 1L, createdBy = "rogue"),
      departments = tibble::tibble(department_key = integer())))

  expect_error(
    neoipcr:::assert_data_protection(ds, dhis2_dataset_options()),
    "companion column")
  expect_error(
    neoipcr:::assert_data_protection(ds, dhis2_dataset_options()),
    "createdBy")
  expect_error(
    neoipcr:::assert_data_protection(ds, dhis2_dataset_options()),
    "`users`")
})

test_that("assert_data_protection succeeds when metadata tibbles carry no companion columns", {
  ds <- make_test_ds(
    metadata = list(
      users       = tibble::tibble(user_key = 1L),
      departments = tibble::tibble(department_key = integer())))

  # Should not throw the companion-column error.
  expect_no_error(
    neoipcr:::assert_data_protection(ds, dhis2_dataset_options()))
})

# ---- Event types ----------------------------------------------------------
#
# The 7 program stages are protocol-fixed; the tibble is always present
# (no entity gate, no `include_event_type` option). The only gate is
# `include_dhis2_ids == "event_types"` which controls exposure of the
# DHIS2 `programStage` UID — same two-axis pattern as users' `user` and
# hospitals' / departments' `orgUnit`.

test_that("eventTypes_cols default shape exposes event_type_key + display cols", {
  opts_no_ids <- dhis2_dataset_options(include_dhis2_ids = character())
  schema <- neoipcr:::compile_schema(neoipcr:::eventTypes_cols, opts_no_ids)
  expect_identical(
    names(schema),
    c("event_type_key", "name", "displayName",
      "displayFormName", "displayDescription"))
  expect_true(is.factor(schema$event_type_key))
  expect_identical(
    levels(schema$event_type_key),
    c("adm", "pro", "bsi", "nec", "ssi", "hap", "end"))
})

test_that("eventTypes_cols exposes programStage when event_types is in include_dhis2_ids", {
  opts_with_ids <- dhis2_dataset_options(include_dhis2_ids = "event_types")
  schema <- neoipcr:::compile_schema(neoipcr:::eventTypes_cols, opts_with_ids)
  expect_identical(
    names(schema),
    c("event_type_key", "programStage", "name", "displayName",
      "displayFormName", "displayDescription"))
  expect_type(schema$programStage, "character")
})

test_that("eventTypes_cols has no entity gate — tibble is always present", {
  # The protocol-fixed event-type set has no "opt out" mode. Unlike
  # every other entity in `schema-orgunits.R`, `eventTypes_cols` is
  # not wrapped with `with_entity_gate()` — `entity_gate()` must
  # return NULL so `compile_schema` / `finalize_to_schema` proceed
  # under any opts.
  expect_null(neoipcr:::entity_gate(neoipcr:::eventTypes_cols))
})

test_that("assert_schema enforces factor levels on event_type_key", {
  # Fixed-levels factor must not have levels dropped: a regression in
  # the reader that slices the factor to a subset (e.g. missing `end`
  # from the DHIS2 response) must fail loudly.
  wrong_levels <- tibble::tibble(
    event_type_key     = factor("adm", levels = c("adm", "bsi")),
    name               = factor("Admission", levels = c(
      "Admission", "Surgical Procedure", "Primary Sepsis/BSI",
      "Necrotizing enterocolitis", "Surgical Site Infection",
      "Pneumonia", "Surveillance-End")),
    displayName        = factor("Aufnahme"),
    displayFormName    = factor("Aufnahme"),
    displayDescription = "desc")
  opts <- dhis2_dataset_options()
  expect_error(
    neoipcr:::assert_schema(wrong_levels, neoipcr:::eventTypes_cols, opts),
    "levels differ")
})

test_that("make_test_metadata_event_types honors n and include_dhis2_ids", {
  full <- make_test_metadata_event_types(
    n = 7, include_dhis2_ids = "event_types")
  expect_identical(
    names(full),
    c("event_type_key", "programStage", "name", "displayName",
      "displayFormName", "displayDescription"))
  expect_equal(nrow(full), 7L)

  no_ids <- make_test_metadata_event_types(
    n = 3, include_dhis2_ids = character())
  expect_identical(
    names(no_ids),
    c("event_type_key", "name", "displayName",
      "displayFormName", "displayDescription"))
  expect_equal(nrow(no_ids), 3L)

  zero <- make_test_metadata_event_types(
    n = 0, include_dhis2_ids = "event_types")
  expect_equal(nrow(zero), 0L)

  # Protocol has 7 event types; asking for 8 is a test-author error.
  expect_error(make_test_metadata_event_types(n = 8), "between 0 and 7")
})

test_that("make_test_metadata_event_types output matches eventTypes_cols schema", {
  for (ids in list(character(), "event_types")) {
    opts <- dhis2_dataset_options(include_dhis2_ids = ids)
    schema <- neoipcr:::compile_schema(neoipcr:::eventTypes_cols, opts)
    fixture <- make_test_metadata_event_types(
      n = 3, include_dhis2_ids = ids)
    expect_schema_matches(fixture, schema)
  }
})
