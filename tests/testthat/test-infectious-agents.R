# Tests for R/infectious-agents.R — get_infectious_agents() — and for the
# bundled infectious-agent tables it reads: their facts, their provenance, and
# their parity with the Surveillance-Toolkit's export of the code sets of the
# list and its reporting classification at the bundled release
# (fixtures/infectious-agent-code-sets.json, written by the toolkit's
# Export-NeoIPCInfectiousAgentCodeSet).

bundled <- function() neoipcr:::internal_infectious_agent_classification

# Every bundled table, as the derivation returns them.
bundled_tables <- function() list(
  classification = neoipcr:::internal_infectious_agent_classification,
  reporting_groups = neoipcr:::internal_infectious_agent_reporting_groups,
  attribute_values = neoipcr:::internal_infectious_agent_attribute_values,
  nodes = neoipcr:::internal_infectious_agent_nodes,
  closure = neoipcr:::internal_infectious_agent_closure)

# The names of the nodes on an Id's path in a version of the tables' rows,
# its top-level node first.
path_names <- function(id, version = "ontology") {
  closure <- neoipcr:::internal_infectious_agent_closure
  nodes <- neoipcr:::internal_infectious_agent_nodes
  path <- closure[closure$version == version & closure$input_id == id, ]
  path <- path[order(path$depth), ]
  nodes <- nodes[nodes$version == version, ]
  nodes$name[match(path$node_id, nodes$node_id)]
}

# The keys of the reporting groups on an Id's path.
group_path <- function(id) {
  closure <- neoipcr:::internal_infectious_agent_closure
  path <- closure[closure$version == "groups" & closure$input_id == id, ]
  path$node_id[order(path$depth)]
}

id_of <- function(name) bundled()$input_id[bundled()$input_name == name]

code_sets <- function()
  jsonlite::fromJSON(
    testthat::test_path("fixtures", "infectious-agent-code-sets.json"),
    simplifyVector = TRUE)

# --- get_infectious_agents() ---------------------------------------------------

test_that("get_infectious_agents() returns every code of the list with its name and its concept, in ascending order", {
  agents <- get_infectious_agents()

  expect_s3_class(agents, "tbl_df")
  expect_identical(names(agents), c("id", "name", "concept_id", "concept_name"))
  expect_type(agents$id, "integer")
  expect_type(agents$name, "character")
  expect_type(agents$concept_id, "integer")
  expect_type(agents$concept_name, "character")
  expect_identical(agents$id, sort(bundled()$input_id))
  expect_false(anyNA(agents$name))
  expect_false(anyNA(agents$concept_id))
})

test_that("a concept names itself, and a synonym its concept", {
  agents <- get_infectious_agents(c(934L, 1230L))

  expect_identical(agents$id, c(934L, 1230L))
  expect_identical(agents$name,
                   c("Staphylococcus aureus", "Abiotrophia adiacens"))
  expect_identical(agents$concept_id, c(934L, 201L))
  expect_identical(agents$concept_name,
                   c("Staphylococcus aureus", "Granulicatella adiacens"))
})

test_that("the codes are returned in the order given, without the missing values and each once", {
  agents <- get_infectious_agents(c(1230, NA, 934, 1230, NA))

  expect_identical(agents$id, c(1230L, 934L))
})

test_that("no codes, or missing ones alone, return no row and no warning", {
  for (ids in list(integer(), NA, NA_integer_, c(NA_real_, NA_real_))) {
    expect_no_warning(agents <- get_infectious_agents(ids))
    expect_identical(nrow(agents), 0L)
    expect_identical(names(agents),
                     c("id", "name", "concept_id", "concept_name"))
  }
})

test_that("a code the list lacks returns no row, under a warning that names it", {
  cnd <- NULL
  agents <- withCallingHandlers(
    get_infectious_agents(c(934L, 999999L, 888888L)),
    neoipcr_unknown_infectious_agent_code = function(w) {
      cnd <<- w
      rlang::cnd_muffle(w)
    })

  expect_identical(agents$id, 934L)
  expect_s3_class(cnd, "neoipcr_unknown_infectious_agent_code")
  expect_s3_class(cnd, "warning")
  expect_identical(cnd$codes, c(999999L, 888888L))
  expect_match(conditionMessage(cnd), "999999, 888888")
  expect_warning(get_infectious_agents(999999L),
                 class = "neoipcr_unknown_infectious_agent_code")
})

test_that("ids that are no codes are refused", {
  for (ids in list("934", 934.5, -1L, Inf, list(934L), TRUE, factor(934L),
                   3e9, Sys.Date()))
    expect_error(get_infectious_agents(ids),
                 class = "neoipcr_invalid_infectious_agent_ids")
})

test_that("the accessor hands on no lineage and no flag", {
  expect_identical(names(get_infectious_agents(934L)),
                   c("id", "name", "concept_id", "concept_name"))
})

# --- The bundled facts -----------------------------------------------------------

test_that("the bundled tables keep their contract", {
  expect_no_error(neoipcr:::assert_infectious_agent_contract(bundled_tables()))
})

test_that("the not-listed option takes every resistance category and no group, and the unidentifiable one no flag", {
  x <- bundled()
  not_listed <- x[x$input_id == 0L, ]
  unidentifiable <- x[x$input_id == 3561L, ]

  expect_identical(not_listed$input_name, "Not listed")
  expect_true(all(unlist(not_listed[c("applies_3gcr", "applies_car",
                                      "applies_cor", "applies_mrsa",
                                      "applies_vre")])))
  expect_identical(unidentifiable$input_name, "Unidentifiable")
  expect_false(any(unlist(unidentifiable[c("common_commensal", "applies_3gcr",
                                           "applies_car", "applies_cor",
                                           "applies_mrsa", "applies_vre")])))
  expect_identical(c(not_listed$reporting_group,
                     unidentifiable$reporting_group),
                   c(NA_character_, NA_character_))
})

test_that("Staphylococcus aureus is a coagulase-positive staphylococcus among the Gram-positive cocci, where methicillin resistance is recorded", {
  aureus <- bundled()[bundled()$input_id == 934L, ]

  expect_identical(aureus$hierarchy, "Bacteria")
  expect_identical(aureus$genus, "Staphylococcus")
  expect_identical(aureus$species, "Staphylococcus aureus")
  expect_identical(aureus$coagulase, "p")
  expect_true(aureus$applies_mrsa)
  expect_false(aureus$applies_vre)
  expect_false(aureus$common_commensal)
  expect_identical(aureus$reporting_group, "gram-positive-cocci")
  expect_identical(aureus$gram_reaction, "positive")
  expect_identical(aureus$oxygen_requirement, "facultative-anaerobic")
})

test_that("methicillin resistance is recorded for Staphylococcus argenteus, beside Staphylococcus aureus", {
  x <- bundled()

  expect_true(x$applies_mrsa[x$input_name == "Staphylococcus argenteus"])
  expect_true(all(c(934L, 2778L) %in% x$input_id[x$applies_mrsa]))
})

test_that("the coagulase-negative staphylococci are common commensals", {
  group <- bundled()[bundled()$input_id == 2776L, ]

  expect_identical(group$input_name, "Coagulase-negative staphylococci")
  expect_identical(group$coagulase, "n")
  expect_true(group$common_commensal)
})

test_that("no infectious agent outside the bacteria has a Gram reaction or an oxygen requirement", {
  x <- bundled()
  outside <- x[x$hierarchy != "Bacteria", ]

  expect_true(all(is.na(outside$gram_reaction)))
  expect_true(all(is.na(outside$oxygen_requirement)))
  expect_false(anyNA(x$gram_reaction[x$hierarchy == "Bacteria"]))
})

test_that("the group tree's leaves hold every reporting group the classification gives", {
  groups <- neoipcr:::internal_infectious_agent_reporting_groups
  leaves <- setdiff(groups$key, groups$parent_key)

  expect_true(all(stats::na.omit(bundled()$reporting_group) %in% leaves))
  expect_identical(groups$parent_key[groups$key == "gram-positive-cocci"],
                   "bacteria")
  expect_identical(groups$ecdc_category[groups$key == "viruses"], "Viruses")
})

test_that("no bundled column is a factor", {
  for (x in bundled_tables())
    expect_false(any(vapply(x, is.factor, logical(1))))
})

# --- The paths of the tables' rows -------------------------------------------

test_that("Staphylococcus aureus and Staphylococcus argenteus count in the Staphylococcus aureus complex, a group without an Id within the coagulase-positive staphylococci", {
  complex <- c("Staphylococcus", "Coagulase-positive staphylococci",
               "Staphylococcus aureus complex")

  expect_identical(path_names(934L)[[1]], "Bacteria")
  expect_identical(utils::tail(path_names(934L), 4L),
                   c(complex, "Staphylococcus aureus"))
  expect_identical(utils::tail(path_names(2778L), 4L),
                   c(complex, "Staphylococcus argenteus"))
  expect_false("Staphylococcus aureus complex" %in% bundled()$input_name)
  expect_identical(group_path(934L), c("bacteria", "gram-positive-cocci"))
})

test_that("the viridans streptococci hold groups within groups, each on the paths of the nodes it holds", {
  expect_identical(utils::tail(path_names(3638L), 4L),
                   c("Streptococcus", "Viridans streptococci",
                     "Streptococcus anginosus group", "Group F streptococci"))
  expect_identical(utils::tail(path_names(2803L), 3L),
                   c("Streptococcus", "Viridans streptococci",
                     "Streptococcus mutans group"))
})

test_that("a subspecies, a serotype, and a group beneath a species count in the species", {
  infantis <- id_of("Bifidobacterium longum subsp. infantis")
  longum <- id_of("Bifidobacterium longum")
  expect_identical(path_names(infantis), path_names(longum))
  expect_identical(utils::tail(path_names(infantis), 1L),
                   "Bifidobacterium longum")

  polio <- path_names(id_of("Enterovirus coxsackiepol"))
  expect_identical(utils::tail(polio, 1L), "Enterovirus coxsackiepol")
  for (name in c("Poliovirus", "Poliovirus 1"))
    expect_identical(path_names(id_of(name)), polio)
})

test_that("a finding recorded above the order has a path of its own, from the kingdom, which every level holds", {
  expect_identical(path_names(3427L), c("Fungi", "Dikarya", "Ascomycota"))
})

test_that("the full ontology's top-level nodes are Bacteria, Fungi, Protozoa, and Viruses, and Eukaryota is no node", {
  nodes <- neoipcr:::internal_infectious_agent_nodes
  nodes <- nodes[nodes$version == "ontology", ]
  top <- nodes[nodes$depth == 0L, ]

  expect_setequal(top$name, c("Bacteria", "Fungi", "Protozoa", "Viruses"))
  expect_true(all(is.na(top$parent_id)))
  expect_false("Eukaryota" %in% nodes$name)
  # The Ids beneath it keep the list's top-level node as their hierarchy.
  albicans <- id_of("Candida albicans")
  expect_identical(path_names(albicans)[[1]], "Fungi")
  expect_identical(bundled()$hierarchy[bundled()$input_id == albicans],
                   "Eukaryota")
})

test_that("an Id's path over the reporting groups runs through each group that holds its reporting group", {
  expect_identical(group_path(id_of("Pseudomonas aeruginosa")),
                   c("bacteria", "gram-negative-bacilli",
                     "non-fermenting-gram-negative-bacilli"))
  expect_identical(group_path(id_of("Candida albicans")), c("fungi", "yeasts"))
})

test_that("the Ids 0 and 3561 are on no path, and every other Id has one in each version", {
  closure <- neoipcr:::internal_infectious_agent_closure
  inside <- setdiff(bundled()$input_id, c(0L, 3561L))

  expect_false(any(c(0L, 3561L) %in% closure$input_id))
  for (version in c("groups", "ontology"))
    expect_identical(sort(unique(closure$input_id[closure$version == version])),
                     inside)
})

test_that("a node of the full ontology is keyed by its ConceptSource and ConceptId, an ICTV node by its IctvId, a node of the groups version by its group's key", {
  nodes <- neoipcr:::internal_infectious_agent_nodes
  node_id <- function(name)
    nodes$node_id[nodes$version == "ontology" & nodes$name == name]

  expect_identical(node_id("Staphylococcus aureus"),
                   "LPSN|species/staphylococcus-aureus")
  expect_identical(node_id("Viridans streptococci"), "NeoIPC|68")
  # Its ConceptId in the list is 202501428, the node of the Master Species
  # List the list follows.
  expect_identical(node_id("Simplexvirus humanalpha1"), "ICTV|19710176")
  ictv <- nodes$node_id[nodes$version == "ontology" &
                          startsWith(nodes$node_id, "ICTV|")]
  expect_gt(length(ictv), 0L)
  expect_true(all(grepl("^ICTV\\|[0-9]+$", ictv)))
  expect_true(all(nodes$node_id[nodes$version == "groups"] %in%
                    neoipcr:::internal_infectious_agent_reporting_groups$key))
})

test_that("the attribute values name each value the classification gives a bacterium", {
  values <- neoipcr:::internal_infectious_agent_attribute_values

  expect_identical(
    values$name[values$attribute == "gram_reaction" &
                  values$key == "positive"],
    "Gram-positive")
  for (attribute in c("gram_reaction", "oxygen_requirement"))
    expect_true(all(stats::na.omit(bundled()[[attribute]]) %in%
                      values$key[values$attribute == attribute]))
})

# --- The provenance ------------------------------------------------------------

test_that("the provenance names the repository, the commit, and the release, and the SHA-256s of the export's files", {
  provenance <- neoipcr:::internal_infectious_agent_ontology
  sets <- code_sets()

  expect_identical(
    names(provenance),
    c("repository", "tag", "version", "commit", "list_sha256",
      "reporting_classification_sha256", "classification_digest",
      "groups_tree_digest", "ontology_tree_digest", "concept_mapping_digest",
      "common_commensal_digest", "applicability_digest"))
  expect_identical(provenance$repository, "NeoIPC/Surveillance-Toolkit")
  expect_match(provenance$commit, "^[0-9a-f]{40}$")
  expect_type(provenance$tag, "character")
  expect_type(provenance$version, "character")
  expect_identical(is.na(provenance$tag), is.na(provenance$version))
  if (!is.na(provenance$tag))
    expect_identical(provenance$tag,
                     paste0("infectious-agents-v", provenance$version))
  expect_identical(provenance$list_sha256, sets$Sha256)
  expect_identical(provenance$reporting_classification_sha256,
                   sets$ClassificationSha256)
})

test_that("the provenance's digests are those of the bundled tables", {
  skip_if(getRversion() < "4.5.0", "tools::sha256sum() needs R 4.5 or later")
  provenance <- neoipcr:::internal_infectious_agent_ontology
  texts <- neoipcr:::infectious_agent_digest_texts(bundled_tables())
  digest <- function(name) tools::sha256sum(bytes = charToRaw(texts[[name]]))

  expect_identical(names(texts),
                   c("classification", "groups_tree", "ontology_tree",
                     "concept_mapping", "common_commensal", "applicability"))
  for (name in names(texts))
    expect_identical(provenance[[paste0(name, "_digest")]], digest(name),
                     label = name)
})

test_that("neoipcr_supported_versions() gives the release of the list the bundled tables come from", {
  expect_identical(neoipcr_supported_versions()$infectious_agents,
                   neoipcr:::internal_infectious_agent_ontology$version)
})

# --- Parity with the toolkit's export ---------------------------------------------

test_that("the concepts and the synonyms are the export's", {
  x <- bundled()
  sets <- code_sets()

  expect_identical(x$input_id[x$input_id == x$output_id],
                   as.integer(sets$Concepts))
  expect_identical(x$input_id[x$input_id != x$output_id],
                   as.integer(sets$Synonyms))
})

test_that("the common commensals and the viruses are the export's", {
  x <- bundled()
  sets <- code_sets()

  expect_identical(x$input_id[x$common_commensal],
                   as.integer(sets$CommonCommensals))
  expect_identical(x$input_id[x$hierarchy == "Viruses"],
                   as.integer(sets$Viruses))
})

test_that("each resistance category applies to the export's set", {
  x <- bundled()
  categories <- code_sets()$ResistanceCategories
  columns <- c(`3GCR` = "applies_3gcr", CARBR = "applies_car",
               COLR = "applies_cor", MRSA = "applies_mrsa",
               VRE = "applies_vre")

  expect_identical(sort(names(categories)), sort(names(columns)))
  for (category in names(columns))
    expect_identical(x$input_id[x[[columns[[category]]]]],
                     as.integer(categories[[category]]),
                     label = category)
})

test_that("every Id takes the reporting group, the Gram reaction, and the oxygen requirement the export gives it", {
  x <- bundled()
  rows <- code_sets()$Classification

  expect_identical(x$input_id, as.integer(rows$Id))
  expect_identical(x$reporting_group, rows$ReportingGroup)
  expect_identical(x$gram_reaction, rows$GramReaction)
  expect_identical(x$oxygen_requirement, rows$OxygenRequirement)
})
