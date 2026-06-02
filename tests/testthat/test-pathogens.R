# Tests for R/pathogens.R — pathogen taxonomy and synonym resolution.

# --- get_pathogen_taxonomy: output structure ---

test_that("get_pathogen_taxonomy returns expected columns", {
  result <- get_pathogen_taxonomy(ids = 934L)
  expected_cols <- c(
    "input_id", "input_name", "output_id", "output_name",
    "concept_type", "is_cc", "coagulase",
    "species", "genus", "family", "order", "class",
    "subdivision", "phylum", "subkingdom", "kingdom", "domain")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_pathogen_taxonomy returns tibble", {
  result <- get_pathogen_taxonomy(ids = 934L)
  expect_s3_class(result, "tbl_df")
})

# --- get_pathogen_taxonomy: known pathogen lookup ---

test_that("get_pathogen_taxonomy resolves S. aureus correctly", {
  # Staphylococcus aureus (id=934) is a well-known species
  result <- get_pathogen_taxonomy(ids = 934L)
  expect_equal(nrow(result), 1L)
  expect_equal(result$input_id, 934L)
  expect_equal(result$output_id, 934L)
  expect_equal(result$output_name, "Staphylococcus aureus")
  expect_equal(as.character(result$genus), "Staphylococcus")
  expect_equal(as.character(result$domain), "Bacteria")
  expect_equal(as.character(result$coagulase), "p")
  expect_equal(as.character(result$concept_type), "species")
})

# --- get_pathogen_taxonomy: synonym resolution ---

test_that("get_pathogen_taxonomy resolves synonyms to canonical concept", {
  # id=1230 "Abiotrophia adiacens" is a synonym for id=201
  # "Granulicatella adiacens"
  result <- get_pathogen_taxonomy(ids = 1230L)
  expect_equal(nrow(result), 1L)
  expect_equal(result$input_id, 1230L)
  expect_equal(result$input_name, "Abiotrophia adiacens")
  expect_equal(result$output_id, 201L)
  expect_equal(result$output_name, "Granulicatella adiacens")
})

test_that("get_pathogen_taxonomy returns concept and its synonyms together", {
  # Query both the concept (201) and its synonym (1230)
  result <- get_pathogen_taxonomy(ids = c(201L, 1230L))
  expect_equal(nrow(result), 2L)
  # Both resolve to the same output
  expect_true(all(result$output_id == 201L))
  expect_true(all(result$output_name == "Granulicatella adiacens"))
  # But input_id differs
  expect_equal(sort(result$input_id), c(201L, 1230L))
})

# --- get_pathogen_taxonomy: edge cases ---

test_that("get_pathogen_taxonomy returns empty tibble for non-existent ID", {
  result <- get_pathogen_taxonomy(ids = 999999L)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("get_pathogen_taxonomy with ids=NULL returns all resolvable pathogens", {
  result <- get_pathogen_taxonomy(ids = NULL)
  # All concepts are present; synonyms whose synonym_for has no matching
  # concept are dropped by the inner_join (4 orphan synonyms as of 2026-04)
  n_concepts <- nrow(neoipcr:::internal_pathogen_concepts)
  expect_true(nrow(result) >= n_concepts)
  expect_true(nrow(result) <= n_concepts + nrow(neoipcr:::internal_pathogen_synonyms))
})

test_that("get_pathogen_taxonomy id=0 returns 'Not listed'", {
  result <- get_pathogen_taxonomy(ids = 0L)
  expect_equal(nrow(result), 1L)
  expect_equal(result$output_name, "Not listed")
})

test_that("get_pathogen_taxonomy taxonomic columns are factors", {
  result <- get_pathogen_taxonomy(ids = 934L)
  factor_cols <- c("concept_type", "coagulase", "species", "genus", "family",
    "order", "class", "subdivision", "phylum", "subkingdom", "kingdom",
    "domain")
  for (col in factor_cols)
    expect_true(is.factor(result[[col]]),
      info = paste0(col, " should be a factor"))
})

# --- get_pathogen_list: internal helper ---

test_that("get_pathogen_list returns expected structure", {
  result <- neoipcr:::get_pathogen_list()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "name", "concept_type", "synonym_for") %in%
    names(result)))
})

test_that("get_pathogen_list includes concepts and synonyms", {
  result <- neoipcr:::get_pathogen_list()
  # Concepts have synonym_for = NA
  concepts <- result[is.na(result$synonym_for), ]
  synonyms <- result[!is.na(result$synonym_for), ]
  expect_true(nrow(concepts) > 0L)
  expect_true(nrow(synonyms) > 0L)
})

test_that("get_pathogen_list first row is 'Not listed' (id=0)", {
  result <- neoipcr:::get_pathogen_list()
  expect_equal(result$id[1], 0L)
  expect_equal(result$name[1], "Not listed")
})

test_that("get_pathogen_list non-zero entries are sorted alphabetically", {
  result <- neoipcr:::get_pathogen_list()
  # Skip the first row (id=0 "Not listed"), rest should be sorted by name
  rest <- result[-1, ]
  expect_equal(rest$name, sort(rest$name))
})
