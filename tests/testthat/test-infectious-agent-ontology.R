# Tests for R/infectious-agent-ontology.R — the derivation of the bundled
# infectious-agent tables from the infectious-agent list and its reporting
# classification, and the checks data-raw/sysdata.R runs on them. The trees
# are inline R lists in the shape yaml12::parse_yaml(simplify = FALSE) gives:
# a mapping is a named list, a sequence an unnamed one. The walker tests port
# the Surveillance-Toolkit's tests of its walkers, with each tree's nodes
# under one of the list's top-level nodes, which the derivation requires.

derive <- function(tree, classification = list())
  neoipcr:::derive_infectious_agent_tables(tree, classification)$classification

# The rows of `x` with the given input_ids, in that order.
rows_of <- function(x, ids) x[match(ids, x$input_id), ]

# The closure's rows of an Id in a version of the tables' rows, its top-level
# node first.
path_rows <- function(tables, id, version = "ontology") {
  x <- tables$closure[tables$closure$version == version &
                        tables$closure$input_id == id, ]
  x[order(x$depth), ]
}

# The node_ids on that path.
path_of <- function(tables, id, version = "ontology")
  path_rows(tables, id, version)$node_id

# The names of the nodes on that path.
path_names <- function(tables, id, version = "ontology") {
  nodes <- tables$nodes[tables$nodes$version == version, ]
  nodes$name[match(path_of(tables, id, version), nodes$node_id)]
}

# The node of `x` named `name`, at any depth, replaced by what `change` makes
# of it; NULL removes it from its list.
edit_node <- function(x, name, change) {
  if (is.list(x) && !is.null(names(x)) && identical(x[["Name"]], name))
    return(change(x))
  if (is.list(x))
    for (i in rev(seq_along(x))) {
      item <- edit_node(x[[i]], name, change)
      if (is.null(item)) x[i] <- NULL else x[[i]] <- item
    }
  x
}

# The group of a classification with the given Key, at any depth, replaced as
# by edit_node().
edit_group <- function(x, key, change) {
  if (is.list(x) && !is.null(names(x)) && identical(x[["Key"]], key))
    return(change(x))
  if (is.list(x))
    for (i in rev(seq_along(x))) {
      item <- edit_group(x[[i]], key, change)
      if (is.null(item)) x[i] <- NULL else x[[i]] <- item
    }
  x
}

# The attribute of a classification with the given Key, its Assignments
# replaced by what `change` makes of them.
edit_assignments <- function(x, key, change) {
  for (i in seq_along(x$Attributes))
    if (identical(x$Attributes[[i]]$Key, key))
      x$Attributes[[i]]$Assignments <- change(x$Attributes[[i]]$Assignments)
  x
}

# A member, or with a value an assignment, naming a node of the list.
entry <- function(name, source, concept_id, value = NULL, reason = NULL) {
  e <- list(Name = name, ConceptSource = source, ConceptId = concept_id,
            Evidence = list("doi:10.2900/7455254"))
  if (!is.null(value)) e$Value <- value
  if (!is.null(reason)) e$Reason <- reason
  e
}

all_resistance <- list(MRSA = TRUE, VRE = TRUE, `3GCR` = TRUE,
                       Carbapenems = TRUE, Colistin = TRUE)

# --- A complete scaffold ------------------------------------------------------

# A list in the released list's shape, which the derivation and the contract
# check both pass: the two options that name no infectious agent, a virus
# genus with a synonym, its former name, which shares its IctvId, the
# staphylococci in their three coagulase groups, an order of Gram-negative
# bacteria, and a fungal genus beneath Eukaryota.
scaffold_tree <- function() {
  epidermidis <- list(
    Name = "Staphylococcus epidermidis", ConceptType = "Species",
    ConceptSource = "LPSN", ConceptId = "species/staphylococcus-epidermidis",
    Id = 12L)
  aureus <- list(
    Name = "Staphylococcus aureus", ConceptType = "Species",
    ConceptSource = "LPSN", ConceptId = "species/staphylococcus-aureus",
    Id = 14L, MRSA = TRUE,
    Synonyms = list(list(
      Name = "Micrococcus aureus", ConceptSource = "LPSN",
      ConceptId = "species/micrococcus-aureus", Id = 15L)),
    Children = list(list(
      Name = "Staphylococcus aureus subsp. aureus", ConceptType = "Subspecies",
      ConceptSource = "LPSN",
      ConceptId = "subspecies/staphylococcus-aureus-aureus", Id = 16L)))
  chromogenes <- list(
    Name = "Staphylococcus chromogenes", ConceptType = "Species",
    ConceptSource = "LPSN", ConceptId = "species/staphylococcus-chromogenes",
    Id = 17L)
  staphylococcus <- list(
    Name = "Staphylococcus", ConceptType = "Genus", ConceptSource = "LPSN",
    ConceptId = "genus/staphylococcus", Id = 10L,
    Children = list(
      list(Name = "Coagulase-negative staphylococci", ConceptType = "Group",
           ConceptSource = "NeoIPC", ConceptId = 63L, Id = 11L,
           CommonCommensal = TRUE, Children = list(epidermidis)),
      list(Name = "Coagulase-positive staphylococci", ConceptType = "Group",
           ConceptSource = "NeoIPC", ConceptId = 64L, Id = 13L,
           Children = list(aureus)),
      list(Name = "Coagulase-variable staphylococci", ConceptType = "Group",
           ConceptSource = "NeoIPC", ConceptId = 102L,
           Children = list(chromogenes))))
  bacillati <- list(
    Name = "Bacillati", ConceptType = "Kingdom", ConceptSource = "LPSN",
    ConceptId = "kingdom/bacillati",
    Children = list(list(
      Name = "Caryophanales", ConceptType = "Order", ConceptSource = "LPSN",
      ConceptId = "order/caryophanales",
      Children = list(list(
        Name = "Staphylococcaceae", ConceptType = "Family",
        ConceptSource = "LPSN", ConceptId = "family/staphylococcaceae",
        Children = list(staphylococcus))))))
  pseudomonadati <- list(
    Name = "Pseudomonadati", ConceptType = "Kingdom", ConceptSource = "LPSN",
    ConceptId = "kingdom/pseudomonadati",
    Children = list(list(
      Name = "Enterobacterales", ConceptType = "Order", ConceptSource = "LPSN",
      ConceptId = "order/enterobacterales", `3GCR` = TRUE,
      Carbapenems = TRUE, Colistin = TRUE,
      Children = list(list(
        Name = "Enterobacteriaceae", ConceptType = "Family",
        ConceptSource = "LPSN", ConceptId = "family/enterobacteriaceae",
        Children = list(list(
          Name = "Klebsiella", ConceptType = "Genus", ConceptSource = "LPSN",
          ConceptId = "genus/klebsiella", Id = 30L,
          Children = list(list(
            Name = "Klebsiella pneumoniae", ConceptType = "Species",
            ConceptSource = "LPSN",
            ConceptId = "species/klebsiella-pneumoniae", Id = 31L)))))))))
  list(Hierarchies = list(
    c(list(Name = "Not listed", ConceptType = "Unknown",
           ConceptSource = "NeoIPC", ConceptId = 1L, Id = 0L),
      all_resistance),
    list(Name = "Unidentifiable", ConceptType = "Unknown",
         ConceptSource = "NeoIPC", ConceptId = 101L, Id = 3561L),
    list(Name = "Viruses", ConceptSource = "NeoIPC", ConceptId = 100L,
         Children = list(list(
           Name = "Orthoflavivirus", ConceptType = "Genus",
           ConceptSource = "ICTV", ConceptId = 202401234L, IctvId = 19900123L,
           Id = 20L,
           Synonyms = list(list(
             Name = "Flavivirus", ConceptSource = "ICTV",
             ConceptId = 201901234L, IctvId = 19900123L, Id = 21L))))),
    list(Name = "Bacteria", ConceptType = "Domain", ConceptSource = "LPSN",
         ConceptId = "domain/bacteria",
         Children = list(bacillati, pseudomonadati)),
    list(Name = "Eukaryota", ConceptType = "Domain", ConceptSource = "NeoIPC",
         ConceptId = 99L,
         Children = list(list(
           Name = "Fungi", ConceptType = "Kingdom", ConceptSource = "MycoBank",
           ConceptId = 455206L,
           Children = list(list(
             Name = "Candida", ConceptType = "Genus",
             ConceptSource = "MycoBank", ConceptId = 56219L, Id = 40L)))))))
}

# A classification of the scaffold that the contract check passes.
scaffold_classification <- function() list(
  Groups = list(
    list(Key = "bacteria", Name = "Bacteria",
         Children = list(
           list(Key = "gram-positive-cocci", Name = "Gram-positive cocci",
                EcdcCategory = "Gram-positive cocci",
                Members = list(entry("Staphylococcus", "LPSN",
                                     "genus/staphylococcus"))),
           list(Key = "enterobacterales", Name = "Enterobacterales",
                EcdcCategory = "Enterobacterales",
                Members = list(entry("Enterobacterales", "LPSN",
                                     "order/enterobacterales"))))),
    list(Key = "fungi", Name = "Fungi", EcdcCategory = "Fungi",
         Members = list(entry("Fungi", "MycoBank", 455206L))),
    list(Key = "viruses", Name = "Viruses", EcdcCategory = "Viruses",
         Members = list(entry("Viruses", "NeoIPC", 100L)))),
  Attributes = list(
    list(Key = "gram-reaction", Name = "Gram reaction",
         Values = list(list(Key = "positive", Name = "Gram-positive"),
                       list(Key = "negative", Name = "Gram-negative")),
         Assignments = list(
           entry("Bacillati", "LPSN", "kingdom/bacillati", "positive"),
           entry("Pseudomonadati", "LPSN", "kingdom/pseudomonadati",
                 "negative"))),
    list(Key = "oxygen-requirement", Name = "Oxygen requirement",
         Values = list(list(Key = "facultative-anaerobic",
                            Name = "Facultatively anaerobic")),
         Assignments = list(
           entry("Bacteria", "LPSN", "domain/bacteria",
                 "facultative-anaerobic")))))

derive_scaffold <- function(tree = scaffold_tree(),
                            classification = scaffold_classification())
  neoipcr:::derive_infectious_agent_tables(tree, classification)

assert_contract <- function(tables)
  neoipcr:::assert_infectious_agent_contract(tables)

column_classes <- function(x)
  vapply(x, function(column) class(column)[[1]], character(1))

test_that("the scaffold derives one row per Id-bearing node, typed as the contract requires, which it passes", {
  tables <- derive_scaffold()
  x <- tables$classification

  expect_identical(
    names(tables), c("classification", "reporting_groups", "attribute_values",
                     "nodes", "closure"))
  expect_identical(
    x$input_id, c(0L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 20L, 21L, 30L,
                  31L, 40L, 3561L))
  expect_identical(
    names(x), c("input_id", "input_name", "output_id", "output_name",
                "hierarchy", "kingdom", "order", "family", "genus", "species",
                "coagulase", "common_commensal", "applies_3gcr", "applies_car",
                "applies_cor", "applies_mrsa", "applies_vre",
                "reporting_group", "gram_reaction", "oxygen_requirement"))
  expect_identical(
    column_classes(tables$nodes),
    c(version = "character", node_id = "character", parent_id = "character",
      name = "character", rank = "character", depth = "integer"))
  expect_identical(
    column_classes(tables$closure),
    c(version = "character", input_id = "integer", node_id = "character",
      depth = "integer"))
  expect_identical(
    column_classes(tables$attribute_values),
    c(attribute = "character", attribute_name = "character",
      key = "character", name = "character"))
  for (table in tables)
    expect_false(any(vapply(table, is.factor, logical(1))))
  expect_no_error(assert_contract(tables))
})

test_that("a synonym names its concept, and takes its concept's lineage", {
  x <- rows_of(derive_scaffold()$classification, c(14L, 15L, 21L))

  expect_identical(x$output_id, c(14L, 14L, 20L))
  expect_identical(x$output_name,
                   c("Staphylococcus aureus", "Staphylococcus aureus",
                     "Orthoflavivirus"))
  expect_identical(x$input_name[2], "Micrococcus aureus")
  expect_identical(x$genus, c("Staphylococcus", "Staphylococcus",
                              "Orthoflavivirus"))
  expect_identical(x$species, c("Staphylococcus aureus",
                                "Staphylococcus aureus", NA))
})

test_that("each rank column names the nearest node of its rank at or above the concept, through groups", {
  tables <- derive_scaffold()
  x <- rows_of(tables$classification, c(16L, 13L, 31L, 40L, 0L))

  # The subspecies reports its species, and the groups between the genus and
  # its species leave the lineage as it is.
  expect_identical(x$kingdom, c("Bacillati", "Bacillati", "Pseudomonadati",
                                "Fungi", NA))
  expect_identical(x$order, c("Caryophanales", "Caryophanales",
                              "Enterobacterales", NA, NA))
  expect_identical(x$family, c("Staphylococcaceae", "Staphylococcaceae",
                               "Enterobacteriaceae", NA, NA))
  expect_identical(x$genus, c("Staphylococcus", "Staphylococcus",
                              "Klebsiella", "Candida", NA))
  expect_identical(x$species, c("Staphylococcus aureus", NA,
                                "Klebsiella pneumoniae", NA, NA))
  expect_identical(x$hierarchy, c("Bacteria", "Bacteria", "Bacteria",
                                  "Eukaryota", "Not listed"))
  # The group between the genus and its species is on the paths of the nodes
  # it holds, and on no other.
  expect_identical(utils::tail(path_names(tables, 16L), 2L),
                   c("Coagulase-positive staphylococci",
                     "Staphylococcus aureus"))
  expect_identical(utils::tail(path_names(tables, 13L), 1L),
                   "Coagulase-positive staphylococci")
  expect_false("Coagulase-positive staphylococci" %in%
                 c(path_names(tables, 31L), path_names(tables, 40L),
                   path_names(tables, 0L)))
})

test_that("the coagulase comes from the group of the genus Staphylococcus that holds the node", {
  x <- rows_of(derive_scaffold()$classification,
               c(10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 31L))

  expect_identical(x$coagulase, c(NA, "n", "n", "p", "p", "p", "p", "v", NA))
})

test_that("a node without an Id has no row", {
  x <- derive_scaffold()$classification

  expect_false(any(c("Bacteria", "Viruses", "Fungi", "Enterobacterales",
                     "Coagulase-variable staphylococci") %in% x$input_name))
})

# --- The walker: visit order, ranks, and synonyms -----------------------------

gen_tree <- function() list(Hierarchies = list(
  list(Name = "Not listed", ConceptType = "Unknown", Id = 0L),
  list(Name = "Bacteria", ConceptType = "Domain",
       Children = list(list(
         Name = "Escherichia", ConceptType = "Genus", Id = 10L,
         Children = list(list(
           Name = "Escherichia coli", ConceptType = "Species", Id = 11L,
           Synonyms = list(list(Name = "Bacillus coli", Id = 12L)))))))))

id_bearing <- function(tree) {
  nodes <- neoipcr:::walk_infectious_agent_tree(tree)
  nodes[!is.na(nodes$id), ]
}

test_that("the walk visits a node, then its synonyms and children, depth first, Id-bearing nodes and synonyms included", {
  nodes <- id_bearing(gen_tree())

  expect_identical(nodes$id, c(0L, 10L, 11L, 12L))
  expect_identical(nodes$name, c("Not listed", "Escherichia",
                                 "Escherichia coli", "Bacillus coli"))
})

test_that("the walk captures each node's ConceptType and marks the members of a Synonyms list", {
  nodes <- id_bearing(gen_tree())

  expect_identical(nodes$concept_type, c("Unknown", "Genus", "Species", NA))
  expect_identical(nodes$is_synonym, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("a synonym's children are concepts, beneath the synonym's concept", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(
      Name = "Genus", ConceptType = "Genus", Id = 100L,
      Synonyms = list(list(
        Name = "Old genus", ConceptType = "Genus", Id = 101L,
        Children = list(list(Name = "Child species", ConceptType = "Species",
                             Id = 102L)))))))))

  nodes <- id_bearing(tree)
  expect_identical(nodes$is_synonym[nodes$id == 101L], TRUE)
  expect_identical(nodes$is_synonym[nodes$id == 102L], FALSE)
  x <- rows_of(derive(tree), 102L)
  expect_identical(x$output_id, 102L)
  expect_identical(x$genus, "Genus")
})

test_that("the walk visits a node's synonyms before its children, whatever the order of its keys", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(
      Name = "Genus", Id = 100L,
      Children = list(list(Name = "Child species", Id = 102L)),
      Synonyms = list(list(Name = "Old genus", Id = 101L)))))))

  expect_identical(id_bearing(tree)$id, c(100L, 101L, 102L))
})

test_that("the walk visits the nodes without an Id, which have no row", {
  tree <- gen_tree()

  nodes <- neoipcr:::walk_infectious_agent_tree(tree)
  expect_true("Bacteria" %in% nodes$name[is.na(nodes$id)])
  expect_false("Bacteria" %in% derive(tree)$input_name)
})

# --- The flags ----------------------------------------------------------------

flag_tree <- function() list(Hierarchies = list(
  c(list(Name = "Not listed", Id = 0L), all_resistance),
  list(Name = "Bacteria",
       Children = list(
         list(Name = "Klebsiella", Id = 100L, Carbapenems = TRUE,
              Children = list(list(
                Name = "Klebsiella pneumoniae", Id = 101L,
                Synonyms = list(list(Name = "Old klebsiella", Id = 102L))))),
         list(Name = "Lactobacillus", Id = 200L,
              Children = list(list(Name = "Lactobacillus acidophilus",
                                   Id = 201L)))))))

test_that("a resistance flag flows down the children and the synonyms", {
  x <- rows_of(derive(flag_tree()), c(100L, 101L, 102L))

  expect_identical(x$applies_car, c(TRUE, TRUE, TRUE))
})

test_that("a resistance flag is false where neither the node nor a node above it sets it", {
  x <- derive(flag_tree())

  expect_false(rows_of(x, 201L)$applies_car)
  expect_false(rows_of(x, 101L)$applies_mrsa)
})

test_that("each of the list's resistance flags gives its own column, and every one applies to the not-listed option", {
  x <- rows_of(derive(flag_tree()), 0L)

  expect_identical(
    unlist(x[c("applies_3gcr", "applies_car", "applies_cor", "applies_mrsa",
               "applies_vre")], use.names = FALSE),
    rep(TRUE, 5L))
})

test_that("an explicit false overrides an inherited true, and flows down in turn", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(
      Name = "Genus", Id = 1L, Carbapenems = TRUE,
      Children = list(
        list(Name = "Exception species", Id = 2L, Carbapenems = FALSE,
             Children = list(list(Name = "Sub", Id = 3L))),
        list(Name = "Normal species", Id = 4L)))))))

  expect_identical(rows_of(derive(tree), 1:4)$applies_car,
                   c(TRUE, FALSE, FALSE, TRUE))
})

test_that("the resistance sets hold the Ids whose flag is true, in ascending order", {
  x <- derive(flag_tree())

  expect_identical(x$input_id[x$applies_car], c(0L, 100L, 101L, 102L))
  expect_identical(x$input_id[x$applies_3gcr], 0L)
  expect_identical(x$input_id[x$applies_mrsa], 0L)
  expect_identical(x$input_id[x$applies_cor], 0L)
})

test_that("the rows ascend by Id as numbers, not as text", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(Name = "A", Id = 100L, MRSA = TRUE),
                    list(Name = "B", Id = 9L, MRSA = TRUE),
                    list(Name = "C", Id = 21L, MRSA = TRUE)))))

  x <- derive(tree)
  expect_identical(x$input_id[x$applies_mrsa], c(9L, 21L, 100L))
})

cc_tree <- function() list(Hierarchies = list(
  list(Name = "Not listed", Id = 0L, CommonCommensal = TRUE),
  list(Name = "Bacteria",
       Children = list(
         list(Name = "Staphylococcus", Id = 100L, CommonCommensal = TRUE,
              Children = list(list(
                Name = "Staphylococcus epidermidis", Id = 101L,
                Synonyms = list(list(Name = "Old epidermidis name",
                                     Id = 102L))))),
         list(Name = "Escherichia", Id = 200L,
              Children = list(list(Name = "Escherichia coli",
                                   Id = 201L)))))))

test_that("the common-commensal flag flows down the children and the synonyms, false where no node sets it", {
  x <- derive(cc_tree())

  expect_identical(rows_of(x, c(100L, 101L, 102L))$common_commensal,
                   c(TRUE, TRUE, TRUE))
  expect_identical(rows_of(x, c(200L, 201L))$common_commensal,
                   c(FALSE, FALSE))
  expect_identical(x$input_id[x$common_commensal], c(0L, 100L, 101L, 102L))
})

test_that("an explicit false common-commensal flag overrides an inherited true, and flows down in turn", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(
      Name = "Genus", Id = 1L, CommonCommensal = TRUE,
      Children = list(
        list(Name = "Exception species", Id = 2L, CommonCommensal = FALSE,
             Children = list(list(Name = "Sub", Id = 3L))),
        list(Name = "Normal species", Id = 4L)))))))

  expect_identical(rows_of(derive(tree), 1:4)$common_commensal,
                   c(TRUE, FALSE, FALSE, TRUE))
})

test_that("no node is a common commensal where none is flagged", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(Name = "A", Id = 1L), list(Name = "B", Id = 2L)))))

  expect_false(any(derive(tree)$common_commensal))
})

test_that("a flag written as a text that reads as a Boolean is read as one", {
  tree <- list(Hierarchies = list(list(
    Name = "Bacteria",
    Children = list(list(Name = "A", Id = 1L, MRSA = " True "),
                    list(Name = "B", Id = 2L, MRSA = "FALSE")))))

  expect_identical(derive(tree)$applies_mrsa, c(TRUE, FALSE))
})

# --- The viruses ----------------------------------------------------------------

test_that("the viruses are the Id-bearing nodes beneath the top-level Viruses, synonyms included", {
  tree <- list(Hierarchies = list(
    list(Name = "Viruses",
         Children = list(list(
           Name = "Simplexvirus", Id = 12L,
           Children = list(list(
             Name = "Herpes simplex virus type 1", Id = 10L,
             Synonyms = list(list(Name = "HSV-1 old name", Id = 11L))))))),
    list(Name = "Bacteria",
         Children = list(list(Name = "Escherichia coli", Id = 200L)))))

  x <- derive(tree)
  expect_identical(x$input_id[x$hierarchy == "Viruses"], c(10L, 11L, 12L))
})

test_that("the viruses ascend by Id as numbers", {
  tree <- list(Hierarchies = list(list(
    Name = "Viruses",
    Children = list(list(Name = "A", Id = 100L), list(Name = "B", Id = 9L),
                    list(Name = "C", Id = 21L)))))

  x <- derive(tree)
  expect_identical(x$input_id[x$hierarchy == "Viruses"], c(9L, 21L, 100L))
})

# --- The code sets and the classification of the toolkit's export ---------------

# The toolkit's fixture for its code-set export: the not-listed option with
# every resistance flag; a genus whose flags flow down to its species; a genus
# whose species turns one flag off and adds another; a genus whose flags reach
# its synonym and its species, which turns the common-commensal flag off and
# adds a resistance flag; and a virus with a synonym.
code_set_tree <- function() list(Hierarchies = list(
  c(list(Id = 0L, Name = "Not listed", ConceptSource = "NeoIPC",
         ConceptId = 1L), all_resistance),
  list(Name = "Bacteria", ConceptSource = "LPSN", ConceptId = "domain/bacteria",
       Children = list(
         list(Id = 60L, Name = "Enterococcus", ConceptSource = "LPSN",
              ConceptId = "genus/enterococcus", VRE = TRUE, Carbapenems = TRUE,
              Children = list(list(
                Id = 11L, Name = "Enterococcus faecium", ConceptSource = "LPSN",
                ConceptId = "species/enterococcus-faecium"))),
         list(Id = 50L, Name = "Klebsiella", ConceptSource = "LPSN",
              ConceptId = "genus/klebsiella", `3GCR` = TRUE,
              Carbapenems = TRUE,
              Children = list(list(
                Id = 21L, Name = "Klebsiella pneumoniae", ConceptSource = "LPSN",
                ConceptId = "species/klebsiella-pneumoniae",
                Carbapenems = FALSE, Colistin = TRUE))),
         list(Id = 30L, Name = "Staphylococcus", ConceptSource = "LPSN",
              ConceptId = "genus/staphylococcus", CommonCommensal = TRUE,
              MRSA = TRUE,
              Synonyms = list(list(
                Id = 25L, Name = "Micrococcus", ConceptSource = "LPSN",
                ConceptId = "genus/micrococcus")),
              Children = list(list(
                Id = 12L, Name = "Staphylococcus aureus", ConceptSource = "LPSN",
                ConceptId = "species/staphylococcus-aureus",
                CommonCommensal = FALSE, Colistin = TRUE))))),
  list(Name = "Viruses", ConceptSource = "NeoIPC", ConceptId = 100L,
       Children = list(list(
         Id = 41L, Name = "Influenza virus", ConceptSource = "ICTV",
         ConceptId = 1L,
         Synonyms = list(list(Id = 5L, Name = "Flu virus",
                              ConceptSource = "ICTV", ConceptId = 2L)))))))

# The genera in two groups of bacteria, the viruses in a third by their
# top-level node, the bacteria's Gram reaction from the domain, overridden on
# two genera, and their oxygen requirement from the domain alone.
code_set_classification <- function() list(
  Groups = list(
    list(Key = "bacteria", Name = "Bacteria",
         Children = list(
           list(Key = "gram-positive-cocci", Name = "Gram-positive cocci",
                EcdcCategory = "Gram-positive cocci",
                Members = list(
                  entry("Enterococcus", "LPSN", "genus/enterococcus"),
                  entry("Staphylococcus", "LPSN", "genus/staphylococcus"))),
           list(Key = "enterobacterales", Name = "Enterobacterales",
                EcdcCategory = "Enterobacterales",
                Members = list(entry("Klebsiella", "LPSN",
                                     "genus/klebsiella"))))),
    list(Key = "viruses", Name = "Viruses", EcdcCategory = "Viruses",
         Members = list(entry("Viruses", "NeoIPC", 100L)))),
  Attributes = list(
    list(Key = "gram-reaction", Name = "Gram reaction",
         Values = list(list(Key = "positive", Name = "Positive"),
                       list(Key = "negative", Name = "Negative")),
         Assignments = list(
           entry("Bacteria", "LPSN", "domain/bacteria", "negative"),
           entry("Enterococcus", "LPSN", "genus/enterococcus", "positive",
                 reason = "Its cells stain Gram-positive."),
           entry("Staphylococcus", "LPSN", "genus/staphylococcus", "positive",
                 reason = "Its cells stain Gram-positive."))),
    list(Key = "oxygen-requirement", Name = "Oxygen requirement",
         Values = list(list(Key = "facultative-anaerobic",
                            Name = "Facultative anaerobic"),
                       list(Key = "aerobic", Name = "Aerobic")),
         Assignments = list(
           entry("Bacteria", "LPSN", "domain/bacteria",
                 "facultative-anaerobic")))))

# The rows as Id|reporting group|Gram reaction|oxygen requirement, NA as <NA>.
classification_rows <- function(x)
  paste(x$input_id, x$reporting_group, x$gram_reaction, x$oxygen_requirement,
        sep = "|")

code_set_rows <- c(
  "0|NA|NA|NA",
  "5|viruses|NA|NA",
  "11|gram-positive-cocci|positive|facultative-anaerobic",
  "12|gram-positive-cocci|positive|facultative-anaerobic",
  "21|enterobacterales|negative|facultative-anaerobic",
  "25|gram-positive-cocci|positive|facultative-anaerobic",
  "30|gram-positive-cocci|positive|facultative-anaerobic",
  "41|viruses|NA|NA",
  "50|enterobacterales|negative|facultative-anaerobic",
  "60|gram-positive-cocci|positive|facultative-anaerobic")

test_that("the code sets hold what the toolkit's export gives, the flags flowing down and an explicit false overriding", {
  x <- derive(code_set_tree(), code_set_classification())

  expect_identical(x$input_id[x$input_id == x$output_id],
                   c(0L, 11L, 12L, 21L, 30L, 41L, 50L, 60L))
  expect_identical(x$input_id[x$input_id != x$output_id], c(5L, 25L))
  expect_identical(x$input_id[x$common_commensal], c(25L, 30L))
  expect_identical(x$input_id[x$hierarchy == "Viruses"], c(5L, 41L))
  expect_identical(x$input_id[x$applies_3gcr], c(0L, 21L, 50L))
  expect_identical(x$input_id[x$applies_car], c(0L, 11L, 50L, 60L))
  expect_identical(x$input_id[x$applies_cor], c(0L, 12L, 21L))
  expect_identical(x$input_id[x$applies_mrsa], c(0L, 12L, 25L, 30L))
  expect_identical(x$input_id[x$applies_vre], c(0L, 11L, 60L))
})

test_that("every Id takes the group and the attribute values the classification gives it, a synonym through its concept, NA where it gives none", {
  x <- derive(code_set_tree(), code_set_classification())

  expect_identical(classification_rows(x), code_set_rows)
})

test_that("no node outside the bacteria takes an attribute value, even where an assignment names a node there", {
  classification <- edit_assignments(
    code_set_classification(), "oxygen-requirement",
    function(a) c(a, list(entry("Viruses", "NeoIPC", 100L, "aerobic"))))

  x <- derive(code_set_tree(), classification)
  expect_identical(classification_rows(x), code_set_rows)
})

test_that("a node no member or assignment reaches takes NA, never a default", {
  # A member that names no node, and an assignment moved below the domain,
  # leave nodes unplaced.
  classification <- edit_group(
    code_set_classification(), "enterobacterales", function(g) {
      g$Members[[1]]$ConceptId <- "genus/unlisted"
      g
    })
  classification <- edit_assignments(
    classification, "oxygen-requirement",
    function(a) list(entry("Klebsiella", "LPSN", "genus/klebsiella",
                           "facultative-anaerobic")))

  x <- derive(code_set_tree(), classification)
  expect_identical(classification_rows(x), c(
    "0|NA|NA|NA",
    "5|viruses|NA|NA",
    "11|gram-positive-cocci|positive|NA",
    "12|gram-positive-cocci|positive|NA",
    "21|NA|negative|facultative-anaerobic",
    "25|gram-positive-cocci|positive|NA",
    "30|gram-positive-cocci|positive|NA",
    "41|viruses|NA|NA",
    "50|NA|negative|facultative-anaerobic",
    "60|gram-positive-cocci|positive|NA"))
})

# --- The resolution of the classification's members and assignments -----------

# The toolkit's fixture for the classification's checks: the two ungrouped
# options, Viruses with one genus, two bacterial genera in one phylum, the
# first with a species and its synonym, and a fungal genus.
resolution_tree <- function() list(Hierarchies = list(
  list(Id = 0L, Name = "Not listed", ConceptType = "Unknown",
       ConceptSource = "NeoIPC", ConceptId = 1L),
  list(Id = 3561L, Name = "Unidentifiable", ConceptType = "Unknown",
       ConceptSource = "NeoIPC", ConceptId = 101L),
  list(Name = "Viruses", ConceptSource = "NeoIPC", ConceptId = 100L,
       Children = list(list(Id = 20L, Name = "Orthoflavivirus",
                            ConceptType = "Genus", ConceptSource = "ICTV",
                            ConceptId = 202401234L))),
  list(Name = "Bacteria", ConceptType = "Domain", ConceptSource = "LPSN",
       ConceptId = "domain/bacteria",
       Children = list(list(
         Name = "Bacillota", ConceptType = "Phylum", ConceptSource = "LPSN",
         ConceptId = "phylum/bacillota",
         Children = list(
           list(Id = 10L, Name = "Staphylococcus", ConceptType = "Genus",
                ConceptSource = "LPSN", ConceptId = "genus/staphylococcus",
                Children = list(list(
                  Id = 12L, Name = "Staphylococcus aureus",
                  ConceptType = "Species", ConceptSource = "LPSN",
                  ConceptId = "species/staphylococcus-aureus",
                  Synonyms = list(list(
                    Id = 13L, Name = "Micrococcus aureus",
                    ConceptSource = "LPSN",
                    ConceptId = "species/micrococcus-aureus"))))),
           list(Id = 30L, Name = "Clostridium", ConceptType = "Genus",
                ConceptSource = "LPSN", ConceptId = "genus/clostridium"))))),
  list(Name = "Eukaryota", ConceptType = "Domain", ConceptSource = "NeoIPC",
       ConceptId = 99L,
       Children = list(list(
         Name = "Fungi", ConceptType = "Kingdom", ConceptSource = "MycoBank",
         ConceptId = 455206L,
         Children = list(list(Id = 40L, Name = "Candida",
                              ConceptType = "Genus", ConceptSource = "MycoBank",
                              ConceptId = 56219L)))))))

resolution_classification <- function() list(
  Groups = list(
    list(Key = "bacteria", Name = "Bacteria",
         Children = list(
           list(Key = "gram-positive-cocci", Name = "Gram-positive cocci",
                EcdcCategory = "Gram-positive cocci",
                Members = list(entry("Staphylococcus", "LPSN",
                                     "genus/staphylococcus"))),
           list(Key = "anaerobes", Name = "Anaerobes",
                EcdcCategory = "Anaerobes",
                Members = list(entry("Clostridium", "LPSN",
                                     "genus/clostridium"))))),
    list(Key = "fungi", Name = "Fungi", EcdcCategory = "Fungi",
         Members = list(entry("Fungi", "MycoBank", 455206L))),
    list(Key = "viruses", Name = "Viruses", EcdcCategory = "Viruses",
         Members = list(entry("Viruses", "NeoIPC", 100L)))),
  Attributes = list(
    list(Key = "gram-reaction", Name = "Gram reaction",
         Assignments = list(entry("Bacillota", "LPSN", "phylum/bacillota",
                                  "positive"))),
    list(Key = "oxygen-requirement", Name = "Oxygen requirement",
         Assignments = list(
           entry("Staphylococcus", "LPSN", "genus/staphylococcus",
                 "facultative-anaerobic"),
           entry("Clostridium", "LPSN", "genus/clostridium",
                 "obligate-anaerobic")))))

add_member <- function(classification, key, member)
  edit_group(classification, key, function(g) {
    g$Members <- c(g$Members, list(member))
    g
  })

groups_of <- function(x, ids) rows_of(x, ids)$reporting_group

test_that("a concept beneath a synonym is placed by the synonym's concept", {
  tree <- edit_node(resolution_tree(), "Micrococcus aureus", function(n) {
    n$Children <- list(list(
      Id = 14L, Name = "Staphylococcus aureus subsp. aureus",
      ConceptType = "Subspecies", ConceptSource = "LPSN",
      ConceptId = "subspecies/staphylococcus-aureus-aureus"))
    n
  })

  expect_identical(groups_of(derive(tree, resolution_classification()), 14L),
                   "gram-positive-cocci")
})

test_that("a synonym takes the group of its concept, which a member beneath the genus places", {
  classification <- add_member(
    resolution_classification(), "anaerobes",
    entry("Staphylococcus aureus", "LPSN", "species/staphylococcus-aureus",
          reason = "r"))

  x <- derive(resolution_tree(), classification)
  expect_identical(groups_of(x, c(13L, 12L, 10L)),
                   c("anaerobes", "anaerobes", "gram-positive-cocci"))
})

test_that("a member that names a synonym places nothing, since a synonym takes its concept's group", {
  classification <- add_member(
    resolution_classification(), "anaerobes",
    entry("Micrococcus aureus", "LPSN", "species/micrococcus-aureus",
          reason = "r"))

  x <- derive(resolution_tree(), classification)
  expect_identical(groups_of(x, c(13L, 12L)),
                   c("gram-positive-cocci", "gram-positive-cocci"))
})

test_that("a member whose ConceptSource and ConceptId name two nodes places neither", {
  tree <- edit_node(resolution_tree(), "Fungi", function(n) {
    n$Children <- c(n$Children, list(list(
      Id = 41L, Name = "Clostridium fungi", ConceptType = "Genus",
      ConceptSource = "LPSN", ConceptId = "genus/clostridium")))
    n
  })

  # Both nodes resolve through the first node with that key, as the toolkit's
  # index resolves them.
  expect_identical(groups_of(derive(tree, resolution_classification()),
                             c(30L, 41L)),
                   c(NA_character_, NA_character_))
})

test_that("a node listed in two groups takes the group listed first", {
  classification <- add_member(
    resolution_classification(), "anaerobes",
    entry("Staphylococcus", "LPSN", "genus/staphylococcus"))

  expect_identical(groups_of(derive(resolution_tree(), classification), 10L),
                   "gram-positive-cocci")
})

test_that("a member whose Name is not its node's places the node it names by its ConceptSource and ConceptId", {
  classification <- edit_group(resolution_classification(), "anaerobes",
                               function(g) {
                                 g$Members[[1]]$Name <- "Clostridia"
                                 g
                               })

  expect_identical(groups_of(derive(resolution_tree(), classification), 30L),
                   "anaerobes")
})

test_that("a node no member reaches takes no group, and the Ids 0 and 3561 take none", {
  classification <- edit_group(resolution_classification(), "anaerobes",
                               function(g) {
                                 g$Members[[1]]$ConceptId <- "genus/nothing"
                                 g
                               })

  expect_identical(groups_of(derive(resolution_tree(), classification),
                             c(30L, 0L, 3561L)),
                   rep(NA_character_, 3L))
})

test_that("an assignment to a node outside the bacteria gives it nothing", {
  classification <- edit_assignments(
    resolution_classification(), "gram-reaction",
    function(a) c(a, list(entry("Fungi", "MycoBank", 455206L, "positive"))))

  x <- derive(resolution_tree(), classification)
  expect_identical(rows_of(x, c(40L, 20L))$gram_reaction,
                   c(NA_character_, NA_character_))
  expect_identical(rows_of(x, c(10L, 12L, 13L, 30L))$gram_reaction,
                   rep("positive", 4L))
})

test_that("an attribute's assignments resolve as the members do: the nearest at or above the node, the one listed first where a node is assigned twice", {
  classification <- edit_assignments(
    resolution_classification(), "gram-reaction",
    function(a) c(a, list(entry("Bacillota", "LPSN", "phylum/bacillota",
                                "negative"))))

  x <- derive(resolution_tree(), classification)
  expect_identical(rows_of(x, c(10L, 12L, 13L, 30L))$gram_reaction,
                   rep("positive", 4L))
  expect_identical(rows_of(x, c(10L, 12L, 13L, 30L))$oxygen_requirement,
                   c("facultative-anaerobic", "facultative-anaerobic",
                     "facultative-anaerobic", "obligate-anaerobic"))
})

# --- The group tree ---------------------------------------------------------

test_that("the group tree holds every group in pre-order, with its parent and its ECDC category", {
  groups <- derive_scaffold()$reporting_groups

  expect_identical(groups$key, c("bacteria", "gram-positive-cocci",
                                 "enterobacterales", "fungi", "viruses"))
  expect_identical(groups$name, c("Bacteria", "Gram-positive cocci",
                                  "Enterobacterales", "Fungi", "Viruses"))
  expect_identical(groups$parent_key, c(NA, "bacteria", "bacteria", NA, NA))
  expect_identical(groups$ecdc_category,
                   c(NA, "Gram-positive cocci", "Enterobacterales", "Fungi",
                     "Viruses"))
})

test_that("the attribute values carry each value's key and name, under their attribute's, in the classification's order", {
  x <- derive_scaffold()$attribute_values

  expect_identical(x$attribute, c("gram_reaction", "gram_reaction",
                                  "oxygen_requirement"))
  expect_identical(x$attribute_name, c("Gram reaction", "Gram reaction",
                                       "Oxygen requirement"))
  expect_identical(x$key, c("positive", "negative", "facultative-anaerobic"))
  expect_identical(x$name, c("Gram-positive", "Gram-negative",
                             "Facultatively anaerobic"))
})

# --- The nodes and the paths of the tables' rows ------------------------------

staphylococcus_path <- c("Bacteria", "Bacillati", "Caryophanales",
                         "Staphylococcaceae", "Staphylococcus")

# The scaffold with the streptococci: a genus whose group of viridans
# streptococci holds a group with an Id, which holds another, and a group
# without one, which holds a species.
streptococci_tree <- function() edit_node(scaffold_tree(), "Bacillati",
                                          function(n) {
  group <- function(name, concept_id, id = NULL, children = NULL) {
    g <- list(Name = name, ConceptType = "Group", ConceptSource = "NeoIPC",
              ConceptId = concept_id)
    if (!is.null(id)) g$Id <- id
    if (!is.null(children)) g$Children <- children
    g
  }
  viridans <- group(
    "Viridans streptococci", 70L, 51L, list(
      group("Streptococcus anginosus group", 71L, 52L, list(
        group("Group F streptococci", 72L, 53L))),
      group("Streptococcus mitis group", 73L, children = list(list(
        Name = "Streptococcus mitis", ConceptType = "Species",
        ConceptSource = "LPSN", ConceptId = "species/streptococcus-mitis",
        Id = 54L)))))
  n$Children <- c(n$Children, list(list(
    Name = "Lactobacillales", ConceptType = "Order", ConceptSource = "LPSN",
    ConceptId = "order/lactobacillales",
    Children = list(list(
      Name = "Streptococcaceae", ConceptType = "Family",
      ConceptSource = "LPSN", ConceptId = "family/streptococcaceae",
      Children = list(list(
        Name = "Streptococcus", ConceptType = "Genus", ConceptSource = "LPSN",
        ConceptId = "genus/streptococcus", Id = 50L,
        Children = list(viridans))))))))
  n
})

# The scaffold's classification with the streptococci among the Gram-positive
# cocci.
streptococci_classification <- function()
  add_member(scaffold_classification(), "gram-positive-cocci",
             entry("Streptococcus", "LPSN", "genus/streptococcus"))

# The scaffold's classification with the Enterobacterales nested in a group
# of the Gram-negative bacilli beneath the bacteria.
nested_classification <- function()
  edit_group(scaffold_classification(), "bacteria", function(g) {
    g$Children[[2]] <- list(
      Key = "gram-negative-bacilli", Name = "Gram-negative bacilli",
      EcdcCategory = "Gram-negative bacilli",
      Children = list(g$Children[[2]]))
    g
  })

test_that("an Id's path over the full ontology runs from its top-level node through every concept above it, those without an Id and the groups included", {
  tables <- derive_scaffold()

  expect_identical(path_names(tables, 17L),
                   c(staphylococcus_path, "Coagulase-variable staphylococci",
                     "Staphylococcus chromogenes"))
  expect_identical(path_rows(tables, 17L)$depth, 0:6)
  expect_identical(path_names(tables, 10L), staphylococcus_path)
  expect_identical(path_names(tables, 11L),
                   c(staphylococcus_path, "Coagulase-negative staphylococci"))
  expect_identical(path_names(tables, 31L),
                   c("Bacteria", "Pseudomonadati", "Enterobacterales",
                     "Enterobacteriaceae", "Klebsiella",
                     "Klebsiella pneumoniae"))
  expect_identical(path_names(tables, 40L), c("Fungi", "Candida"))
  expect_identical(path_rows(tables, 40L)$depth, 0:1)
  expect_identical(path_names(tables, 20L), c("Viruses", "Orthoflavivirus"))
})

# The scaffold with a protozoon beside the fungi beneath Eukaryota, in a group
# of its own.
protozoa_tree <- function() edit_node(scaffold_tree(), "Eukaryota",
                                      function(n) {
  n$Children <- c(n$Children, list(list(
    Name = "Protozoa", ConceptType = "Kingdom", ConceptSource = "MycoBank",
    ConceptId = 92339L,
    Children = list(list(
      Name = "Toxoplasma", ConceptType = "Genus", ConceptSource = "MycoBank",
      ConceptId = 300002L,
      Children = list(list(
        Name = "Toxoplasma gondii", ConceptType = "Species",
        ConceptSource = "MycoBank", ConceptId = 300003L, Id = 45L)))))))
  n
})
protozoa_classification <- function() {
  classification <- scaffold_classification()
  classification$Groups <- c(classification$Groups, list(list(
    Key = "parasites", Name = "Parasites", EcdcCategory = "Fungi",
    Members = list(entry("Protozoa", "MycoBank", 92339L)))))
  classification
}

test_that("the full ontology's paths start beneath Eukaryota, which has no node, so that Fungi and Protozoa are top-level nodes", {
  expect_identical(neoipcr:::infectious_agent_skipped_hierarchies, "Eukaryota")
  expect_true(all(neoipcr:::infectious_agent_skipped_hierarchies %in%
                    neoipcr:::infectious_agent_hierarchies))

  tables <- derive_scaffold(protozoa_tree(), protozoa_classification())
  nodes <- tables$nodes[tables$nodes$version == "ontology", ]
  top <- nodes[nodes$depth == 0L, ]
  expect_identical(top$name, c("Viruses", "Bacteria", "Fungi", "Protozoa"))
  expect_true(all(is.na(top$parent_id)))
  expect_false("Eukaryota" %in% nodes$name)
  expect_false("NeoIPC|99" %in% c(nodes$node_id, nodes$parent_id,
                                  tables$closure$node_id))
  expect_identical(path_names(tables, 45L),
                   c("Protozoa", "Toxoplasma", "Toxoplasma gondii"))
  expect_identical(path_rows(tables, 45L)$depth, 0:2)
  # The Ids beneath it keep the list's top-level node as their hierarchy.
  expect_identical(rows_of(tables$classification, c(40L, 45L))$hierarchy,
                   c("Eukaryota", "Eukaryota"))
  expect_no_error(assert_contract(tables))
})

test_that("a subspecies counts in its species, and a synonym as its concept, neither a node of its own", {
  tables <- derive_scaffold()
  aureus <- c(staphylococcus_path, "Coagulase-positive staphylococci",
              "Staphylococcus aureus")

  expect_identical(path_names(tables, 14L), aureus)
  expect_identical(path_names(tables, 15L), aureus)
  expect_identical(path_names(tables, 16L), aureus)
  expect_identical(path_names(tables, 21L), c("Viruses", "Orthoflavivirus"))
  expect_false(any(c("Staphylococcus aureus subsp. aureus",
                     "Micrococcus aureus", "Flavivirus") %in%
                     tables$nodes$name))
})

test_that("a serotype, and a group beneath a species, count in the species, while a variety without a species is a node of its own", {
  tree <- edit_node(scaffold_tree(), "Viruses", function(n) {
    n$Children <- c(n$Children, list(list(
      Name = "Enterovirus", ConceptType = "Genus", ConceptSource = "ICTV",
      ConceptId = 202400001L, IctvId = 19710001L, Id = 60L,
      Children = list(list(
        Name = "Enterovirus coxsackiepol", ConceptType = "Species",
        ConceptSource = "ICTV", ConceptId = 202400002L, IctvId = 19710002L,
        Id = 61L,
        Children = list(list(
          Name = "Poliovirus", ConceptType = "Group", ConceptSource = "NeoIPC",
          ConceptId = 80L, Id = 62L,
          Children = list(list(
            Name = "Poliovirus 1", ConceptType = "Serotype",
            ConceptSource = "NeoIPC", ConceptId = 81L, Id = 63L)))))))))
    n
  })
  tree <- edit_node(tree, "Candida", function(n) {
    n$Children <- list(list(
      Name = "Candida pintolopesii var. pintolopesii",
      ConceptType = "Variety", ConceptSource = "MycoBank",
      ConceptId = 300001L, Id = 41L))
    n
  })

  tables <- derive_scaffold(tree)
  polio <- c("Viruses", "Enterovirus", "Enterovirus coxsackiepol")
  for (id in 61:63)
    expect_identical(path_names(tables, id), polio)
  expect_identical(path_names(tables, 41L),
                   c("Fungi", "Candida",
                     "Candida pintolopesii var. pintolopesii"))
})

test_that("groups within groups are each on the paths of the nodes they hold, a group without an Id included", {
  tables <- derive_scaffold(streptococci_tree(), streptococci_classification())
  streptococcus <- c("Bacteria", "Bacillati", "Lactobacillales",
                     "Streptococcaceae", "Streptococcus",
                     "Viridans streptococci")

  expect_identical(path_names(tables, 53L),
                   c(streptococcus, "Streptococcus anginosus group",
                     "Group F streptococci"))
  expect_identical(path_names(tables, 52L),
                   c(streptococcus, "Streptococcus anginosus group"))
  expect_identical(path_names(tables, 54L),
                   c(streptococcus, "Streptococcus mitis group",
                     "Streptococcus mitis"))
  expect_identical(path_rows(tables, 54L)$depth, 0:7)
  expect_no_error(assert_contract(tables))
})

test_that("each node of the full ontology on a path is a node of the table once, keyed by its ConceptSource and ConceptId, an ICTV node by its IctvId, in the order of the walk", {
  nodes <- derive_scaffold()$nodes
  nodes <- nodes[nodes$version == "ontology", ]

  expect_identical(
    nodes$name,
    c("Viruses", "Orthoflavivirus", staphylococcus_path,
      "Coagulase-negative staphylococci", "Staphylococcus epidermidis",
      "Coagulase-positive staphylococci", "Staphylococcus aureus",
      "Coagulase-variable staphylococci", "Staphylococcus chromogenes",
      "Pseudomonadati", "Enterobacterales", "Enterobacteriaceae",
      "Klebsiella", "Klebsiella pneumoniae", "Fungi", "Candida"))
  cops <- nodes[nodes$name == "Coagulase-positive staphylococci", ]
  expect_identical(
    unlist(cops[c("node_id", "parent_id", "rank")], use.names = FALSE),
    c("NeoIPC|64", "LPSN|genus/staphylococcus", "Group"))
  expect_identical(cops$depth, 5L)
  viruses <- nodes[nodes$name == "Viruses", ]
  expect_identical(
    unlist(viruses[c("node_id", "parent_id", "rank")], use.names = FALSE),
    c("NeoIPC|100", NA, NA))
  expect_identical(viruses$depth, 0L)
  genus <- nodes[nodes$name == "Orthoflavivirus", ]
  expect_identical(
    unlist(genus[c("node_id", "parent_id", "rank")], use.names = FALSE),
    c("ICTV|19900123", "NeoIPC|100", "Genus"))
  fungi <- nodes[nodes$name == "Fungi", ]
  expect_identical(
    unlist(fungi[c("node_id", "parent_id", "rank")], use.names = FALSE),
    c("MycoBank|455206", NA, "Kingdom"))
  expect_identical(fungi$depth, 0L)
})

# The scaffold's virus genus, edited by `change`.
with_virus_genus <- function(change, tree = scaffold_tree())
  edit_node(tree, "Orthoflavivirus", change)

test_that("an ICTV node keeps its node_id where a release gives it another ConceptId, a new name included", {
  node_ids <- function(tree) {
    nodes <- derive_scaffold(tree)$nodes
    nodes$node_id[nodes$version == "ontology" & nodes$rank %in% "Genus" &
                    startsWith(nodes$name, "Orthoflavivirus")]
  }

  expect_identical(node_ids(scaffold_tree()), "ICTV|19900123")
  expect_identical(node_ids(with_virus_genus(function(n) {
    n$ConceptId <- 202501234L
    n
  })), "ICTV|19900123")
  expect_identical(node_ids(with_virus_genus(function(n) {
    n$ConceptId <- 202501234L
    n$Name <- "Orthoflavivirus novus"
    n
  })), "ICTV|19900123")
})

test_that("a node on no Id's path, beneath a species or with no Id beneath it, is no node of the table", {
  tree <- edit_node(scaffold_tree(), "Coagulase-positive staphylococci",
                    function(n) {
                      n$Children <- c(n$Children, list(list(
                        Name = "Staphylococcus schweitzeri",
                        ConceptType = "Species", ConceptSource = "LPSN",
                        ConceptId = "species/staphylococcus-schweitzeri")))
                      n
                    })

  nodes <- derive_scaffold(tree)$nodes
  expect_false(any(c("Staphylococcus schweitzeri",
                     "Staphylococcus aureus subsp. aureus", "Not listed",
                     "Unidentifiable") %in% nodes$name))
})

test_that("the Ids 0 and 3561 are outside the tree, on no path of either version, and every other Id is on one of each", {
  tables <- derive_scaffold()
  closure <- tables$closure
  inside <- setdiff(tables$classification$input_id, c(0L, 3561L))

  expect_false(any(c(0L, 3561L) %in% closure$input_id))
  for (version in c("groups", "ontology"))
    expect_identical(sort(unique(closure$input_id[closure$version == version])),
                     inside)
})

test_that("an Id's path over the reporting groups runs from the top of the group tree to its reporting group", {
  tables <- derive_scaffold(classification = nested_classification())

  expect_identical(path_of(tables, 31L, "groups"),
                   c("bacteria", "gram-negative-bacilli", "enterobacterales"))
  expect_identical(path_rows(tables, 31L, "groups")$depth, 0:2)
  expect_identical(path_of(tables, 15L, "groups"),
                   c("bacteria", "gram-positive-cocci"))
  expect_identical(path_of(tables, 40L, "groups"), "fungi")
  expect_identical(path_of(tables, 21L, "groups"), "viruses")
  expect_no_error(assert_contract(tables))
})

test_that("each reporting group on a path is a node of the groups version, keyed by its key, without a rank", {
  nodes <- derive_scaffold(classification = nested_classification())$nodes
  nodes <- nodes[nodes$version == "groups", ]

  expect_identical(nodes$node_id,
                   c("bacteria", "gram-positive-cocci", "gram-negative-bacilli",
                     "enterobacterales", "fungi", "viruses"))
  expect_identical(nodes$parent_id,
                   c(NA, "bacteria", "bacteria", "gram-negative-bacilli", NA,
                     NA))
  expect_identical(nodes$name,
                   c("Bacteria", "Gram-positive cocci", "Gram-negative bacilli",
                     "Enterobacterales", "Fungi", "Viruses"))
  expect_identical(nodes$rank, rep(NA_character_, 6L))
  expect_identical(nodes$depth, c(0L, 1L, 1L, 2L, 0L, 0L))
})

# --- The derivation's refusals, each on the scaffold less one element ---------

expect_ontology_error <- function(expr, class) {
  cnd <- expect_error(expr, class = class)
  expect_s3_class(cnd, "neoipcr_infectious_agent_ontology_error")
}

test_that("an Id that is no code, a whole number from 0, is refused", {
  for (id in list("", "abc", 1.5, -1L, TRUE, NULL, list(5L))) {
    tree <- edit_node(scaffold_tree(), "Klebsiella", function(n) {
      n["Id"] <- list(id)
      n
    })
    expect_ontology_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_invalid_id")
  }
})

test_that("an Id written as a text of digits, or as a whole number of another type, is read as the code", {
  tree <- edit_node(scaffold_tree(), "Klebsiella", function(n) {
    n$Id <- " 30 "
    n
  })
  tree <- edit_node(tree, "Candida", function(n) {
    n$Id <- 40
    n
  })

  expect_identical(rows_of(derive_scaffold(tree)$classification,
                           c(30L, 40L))$input_name,
                   c("Klebsiella", "Candida"))
})

test_that("an Id given to two nodes is refused", {
  tree <- edit_node(scaffold_tree(), "Candida", function(n) {
    n$Id <- 30L
    n
  })

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_repeated_id")
})

test_that("a blank name is refused, on a node without an Id too", {
  for (name in c("Klebsiella", "Enterobacteriaceae")) {
    tree <- edit_node(scaffold_tree(), name, function(n) {
      n$Name <- " "
      n
    })
    expect_ontology_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_blank_name")
  }
})

test_that("a flag that is neither true nor false is refused", {
  for (flag in c("CommonCommensal", "3GCR", "Carbapenems", "Colistin", "MRSA",
                 "VRE")) {
    tree <- edit_node(scaffold_tree(), "Klebsiella", function(n) {
      n[[flag]] <- "maybe"
      n
    })
    expect_ontology_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_invalid_flag")
  }
  for (value in list(NULL, NA, NA_character_, 1L, list(TRUE))) {
    tree <- edit_node(scaffold_tree(), "Klebsiella", function(n) {
      n["MRSA"] <- list(value)
      n
    })
    expect_ontology_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_invalid_flag")
  }
})

test_that("a ConceptType that is none of the list's ranks is refused", {
  tree <- edit_node(scaffold_tree(), "Klebsiella", function(n) {
    n$ConceptType <- "genus"
    n
  })

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_unknown_concept_type")
})

test_that("a synonym whose concept has no Id is refused", {
  tree <- edit_node(scaffold_tree(), "Orthoflavivirus", function(n) {
    n$Id <- NULL
    n
  })

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_orphan_synonym")
})

test_that("a group of the genus Staphylococcus that maps to no coagulase is refused", {
  tree <- edit_node(scaffold_tree(), "Coagulase-variable staphylococci",
                    function(n) {
                      n$Name <- "Coagulase-unknown staphylococci"
                      n
                    })

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_unmapped_coagulase_group")
})

test_that("a name two concepts of one rank share is refused", {
  tree <- edit_node(scaffold_tree(), "Candida", function(n) {
    n$Name <- "Klebsiella"
    n
  })

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_ambiguous_name")
})

test_that("a name two concepts of different ranks share is no ambiguity", {
  tree <- edit_node(scaffold_tree(), "Enterobacteriaceae", function(n) {
    n$Name <- "Klebsiella"
    n
  })

  expect_no_error(derive_scaffold(tree))
})

test_that("a name holding one of the reserved characters is refused, a group's and an attribute value's included", {
  for (character in c("|", "~", "_")) {
    tree <- edit_node(scaffold_tree(), "Klebsiella pneumoniae", function(n) {
      n$Name <- paste0("Klebsiella", character, "pneumoniae")
      n
    })
    expect_ontology_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_reserved_character")
    classification <- edit_group(scaffold_classification(), "fungi",
                                 function(g) {
                                   g$Name <- paste0("Fungi", character)
                                   g
                                 })
    expect_ontology_error(derive_scaffold(classification = classification),
                          "neoipcr_infectious_agent_reserved_character")
    classification <- scaffold_classification()
    classification$Attributes[[2]]$Values[[1]]$Name <-
      paste0("Facultatively", character, "anaerobic")
    expect_ontology_error(derive_scaffold(classification = classification),
                          "neoipcr_infectious_agent_reserved_character")
  }
})

test_that("a top-level node other than the list's five is refused", {
  tree <- scaffold_tree()
  tree$Hierarchies <- c(tree$Hierarchies, list(list(
    Name = "Archaea", ConceptType = "Domain", ConceptSource = "LPSN",
    ConceptId = "domain/archaea")))

  expect_ontology_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_unexpected_hierarchy")
})

# --- The contract, each element on the scaffold less that element -------------

expect_contract_error <- function(tables, class) {
  cnd <- expect_error(assert_contract(tables), class = class)
  expect_s3_class(cnd, "neoipcr_infectious_agent_contract_violation")
  expect_s3_class(cnd, "neoipcr_infectious_agent_ontology_error")
}

test_that("a table without a documented column, or with one of another type, breaks the contract", {
  tables <- derive_scaffold()
  tables$classification$species <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$classification$coagulase <- factor(tables$classification$coagulase)
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$reporting_groups$parent_key <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$attribute_values$name <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$nodes$depth <- as.double(tables$nodes$depth)
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$closure$node_id <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$closure <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")
})

test_that("a list without one of its five top-level nodes breaks the contract", {
  for (top in c("Viruses", "Eukaryota")) {
    tree <- edit_node(scaffold_tree(), top, function(n) NULL)
    expect_contract_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_contract_hierarchy")
  }
})

test_that("an Id at a node the full ontology's paths start beneath breaks the contract", {
  tree <- edit_node(scaffold_tree(), "Eukaryota", function(n) {
    n$Id <- 90L
    n
  })
  classification <- add_member(scaffold_classification(), "fungi",
                               entry("Eukaryota", "NeoIPC", 99L))

  expect_contract_error(derive_scaffold(tree, classification),
                        "neoipcr_infectious_agent_contract_skipped_node")
})

test_that("a not-listed option missing, without a resistance category, or in a group breaks the contract", {
  tree <- edit_node(scaffold_tree(), "Not listed", function(n) NULL)
  expect_contract_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_contract_not_listed")

  tree <- edit_node(scaffold_tree(), "Not listed", function(n) {
    n$Colistin <- FALSE
    n
  })
  expect_contract_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_contract_not_listed")

  classification <- add_member(scaffold_classification(), "fungi",
                               entry("Not listed", "NeoIPC", 1L))
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_not_listed")
})

test_that("an unidentifiable option missing, with a flag, or in a group breaks the contract", {
  tree <- edit_node(scaffold_tree(), "Unidentifiable", function(n) NULL)
  expect_contract_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_contract_unidentifiable")

  for (flag in c("CommonCommensal", "MRSA")) {
    tree <- edit_node(scaffold_tree(), "Unidentifiable", function(n) {
      n[[flag]] <- TRUE
      n
    })
    expect_contract_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_contract_unidentifiable")
  }

  classification <- add_member(scaffold_classification(), "fungi",
                               entry("Unidentifiable", "NeoIPC", 101L))
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_unidentifiable")
})

test_that("an Id without a reporting group, or with one that is no leaf of the group tree, breaks the contract", {
  classification <- edit_group(scaffold_classification(), "fungi",
                               function(g) {
                                 g$Members <- list()
                                 g
                               })
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_reporting_group")

  tables <- derive_scaffold()
  x <- tables$classification
  x$reporting_group[x$input_id == 40L] <- "bacteria"
  tables$classification <- x
  expect_contract_error(tables,
                        "neoipcr_infectious_agent_contract_reporting_group")
})

test_that("a bacterium without a Gram reaction or an oxygen requirement breaks the contract, as does a value outside the bacteria", {
  for (attribute in c("gram-reaction", "oxygen-requirement")) {
    classification <- edit_assignments(scaffold_classification(), attribute,
                                       function(a) a[-1])
    expect_contract_error(derive_scaffold(classification = classification),
                          "neoipcr_infectious_agent_contract_attribute")
  }

  tables <- derive_scaffold()
  x <- tables$classification
  x$gram_reaction[x$input_id == 40L] <- "positive"
  tables$classification <- x
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_attribute")
})

test_that("a group tree with a key twice, a group without a name, or a parent that is no group breaks the contract", {
  classification <- edit_group(scaffold_classification(), "enterobacterales",
                               function(g) {
                                 g$Key <- "gram-positive-cocci"
                                 g
                               })
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_group_tree")

  classification <- edit_group(scaffold_classification(), "viruses",
                               function(g) {
                                 g$Name <- NULL
                                 g
                               })
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_group_tree")

  tables <- derive_scaffold()
  tables$reporting_groups$parent_key[2] <- "bacterium"
  expect_contract_error(tables,
                        "neoipcr_infectious_agent_contract_group_tree")
})

test_that("staphylococci without one of the three coagulases break the contract", {
  tree <- edit_node(scaffold_tree(), "Coagulase-variable staphylococci",
                    function(n) NULL)

  expect_contract_error(derive_scaffold(tree),
                        "neoipcr_infectious_agent_contract_coagulase")
})

test_that("an attribute value that is none of its attribute's values, or a value listed twice or without a name, breaks the contract", {
  classification <- edit_assignments(
    scaffold_classification(), "gram-reaction", function(a) {
      a[[2]]$Value <- "negativ"
      a
    })
  expect_contract_error(derive_scaffold(classification = classification),
                        "neoipcr_infectious_agent_contract_attribute_value")

  tables <- derive_scaffold()
  tables$attribute_values <- rbind(tables$attribute_values,
                                   tables$attribute_values[1, ])
  expect_contract_error(tables,
                        "neoipcr_infectious_agent_contract_attribute_value")

  tables <- derive_scaffold()
  tables$attribute_values$name[3] <- NA
  expect_contract_error(tables,
                        "neoipcr_infectious_agent_contract_attribute_value")
})

# The scaffold's tables with the nodes replaced by what `change` makes of them.
with_nodes <- function(change) {
  tables <- derive_scaffold()
  tables$nodes <- change(tables$nodes)
  tables
}

# The scaffold's tables with the closure replaced by what `change` makes of it.
with_closure <- function(change) {
  tables <- derive_scaffold()
  tables$closure <- change(tables$closure)
  tables
}

test_that("a node of the full ontology without a ConceptSource or a ConceptId, or a node_id two nodes of a version share, breaks the contract", {
  for (field in c("ConceptSource", "ConceptId")) {
    tree <- edit_node(scaffold_tree(), "Caryophanales", function(n) {
      n[[field]] <- NULL
      n
    })
    expect_contract_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_contract_node_id")
  }

  expect_contract_error(
    with_nodes(function(n) {
      n$node_id[n$version == "ontology" & n$name == "Candida"] <-
        "LPSN|genus/klebsiella"
      n
    }),
    "neoipcr_infectious_agent_contract_node_id")
})

test_that("an ICTV concept without an IctvId, or two ICTV concepts with one IctvId, break the contract, while a synonym shares its concept's", {
  for (id in list(NULL, " ")) {
    tree <- with_virus_genus(function(n) {
      n["IctvId"] <- list(id)
      n
    })
    expect_contract_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_contract_node_id")
  }

  twins <- edit_node(scaffold_tree(), "Viruses", function(n) {
    n$Children <- c(n$Children, list(list(
      Name = "Orthohepevirus", ConceptType = "Genus", ConceptSource = "ICTV",
      ConceptId = 202405678L, IctvId = 19900123L, Id = 22L)))
    n
  })
  expect_contract_error(derive_scaffold(twins),
                        "neoipcr_infectious_agent_contract_node_id")

  # The scaffold's synonym Flavivirus has its concept's IctvId.
  expect_no_error(assert_contract(derive_scaffold()))
})

test_that("a node of no version, without a name, whose parent is no node of its version, or whose depth is not its parent's plus one breaks the contract", {
  for (change in list(
    function(n) {
      n$version[n$name == "Candida"] <- "taxa"
      n
    },
    function(n) {
      n$name[n$name == "Candida"] <- " "
      n
    },
    function(n) {
      n$parent_id[n$name == "Candida"] <- "MycoBank|1"
      n
    },
    function(n) {
      n$depth[n$name == "Candida"] <- 2L
      n
    },
    function(n) {
      n$depth[n$name == "Fungi"] <- 1L
      n
    }))
    expect_contract_error(with_nodes(change),
                          "neoipcr_infectious_agent_contract_node_tree")
})

test_that("a node on no path, or a node of the groups version that is not its reporting group, breaks the contract", {
  expect_contract_error(
    with_nodes(function(n) rbind(n, tibble::tibble(
      version = "ontology", node_id = "MycoBank|1",
      parent_id = "MycoBank|455206", name = "Aspergillus", rank = "Genus",
      depth = 1L))),
    "neoipcr_infectious_agent_contract_node_tree")

  for (column in c("name", "parent_id"))
    expect_contract_error(
      with_nodes(function(n) {
        n[[column]][n$version == "groups" & n$node_id == "enterobacterales"] <-
          "gram-positive-cocci"
        n
      }),
      "neoipcr_infectious_agent_contract_node_tree")
})

test_that("an Id other than 0 and 3561 without a path in a version, one of them with a path, or a path of an Id the classification lacks breaks the contract", {
  for (version in c("groups", "ontology"))
    expect_contract_error(
      with_closure(function(x)
        x[!(x$version == version & x$input_id == 31L), ]),
      "neoipcr_infectious_agent_contract_closure")

  for (id in c(0L, 3561L, 999L))
    expect_contract_error(
      with_closure(function(x) {
        path <- x[x$version == "ontology" & x$input_id == 40L, ]
        path$input_id <- id
        rbind(x, path)
      }),
      "neoipcr_infectious_agent_contract_closure")
})

test_that("a path that is no chain from a top-level node to the node its Id is recorded at breaks the contract", {
  at <- function(x, id, depth, version = "ontology")
    which(x$version == version & x$input_id == id & x$depth == depth)
  for (change in list(
    function(x) x[-at(x, 31L, 2L), ],
    function(x) x[-at(x, 31L, 5L), ],
    function(x) x[-at(x, 31L, 0L), ],
    function(x) rbind(x, x[at(x, 31L, 5L), ]),
    function(x) {
      x$depth[at(x, 40L, 1L)] <- 2L
      x
    },
    function(x) {
      x$node_id[at(x, 40L, 1L)] <- "MycoBank|1"
      x
    },
    function(x) {
      x$node_id[at(x, 17L, 5L)] <- "NeoIPC|64"
      x
    },
    function(x) x[-at(x, 31L, 1L, "groups"), ]))
    expect_contract_error(with_closure(change),
                          "neoipcr_infectious_agent_contract_path")

  # A path over the reporting groups that ends at another group than the
  # Id's, and a path over the full ontology from another top-level node than
  # the Id's, or, for an Id beneath Eukaryota, from a top-level node of the
  # list rather than from beneath one.
  tables <- derive_scaffold()
  x <- tables$classification
  x$reporting_group[x$input_id == 40L] <- "viruses"
  tables$classification <- x
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_path")

  tables <- derive_scaffold()
  x <- tables$classification
  x$hierarchy[x$input_id == 21L] <- "Eukaryota"
  tables$classification <- x
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_path")

  tables <- derive_scaffold(protozoa_tree(), protozoa_classification())
  x <- tables$classification
  x$hierarchy[x$input_id == 40L] <- "Viruses"
  tables$classification <- x
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_path")
})

# --- The comparison with the bundled classification ---------------------------

test_that("the comparison says there is nothing to compare with where no classification is bundled", {
  current <- derive_scaffold()$classification

  expect_message(
    r <- neoipcr:::compare_infectious_agent_classifications(NULL, current),
    "nothing")
  expect_null(r)
})

test_that("the comparison shows the counts per hierarchy and the sizes of the sets beside the bundled ones", {
  previous <- derive_scaffold()$classification
  tree <- edit_node(scaffold_tree(), "Candida", function(n) {
    n$Children <- list(list(
      Name = "Candida albicans", ConceptType = "Species",
      ConceptSource = "MycoBank", ConceptId = 256187L, Id = 41L))
    n
  })
  current <- derive_scaffold(tree)$classification

  messages <- character()
  r <- withCallingHandlers(
    neoipcr:::compare_infectious_agent_classifications(previous, current),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  text <- paste(messages, collapse = "\n")
  expect_match(text, "Eukaryota")
  expect_match(text, "concepts")
  eukaryota <- r$hierarchies[r$hierarchies$hierarchy == "Eukaryota", ]
  expect_identical(c(eukaryota$previous, eukaryota$current), c(1L, 2L))
  concepts <- r$sets[r$sets$set == "concepts", ]
  expect_identical(concepts$current - concepts$previous, 1L)
  expect_identical(r$added, 41L)
  expect_identical(r$removed, integer())
})

test_that("the comparison refuses an Id removed without acknowledgement, and accepts one acknowledged", {
  previous <- derive_scaffold()$classification
  tree <- edit_node(scaffold_tree(), "Klebsiella pneumoniae", function(n) NULL)
  current <- derive_scaffold(tree)$classification

  expect_ontology_error(
    suppressMessages(neoipcr:::compare_infectious_agent_classifications(
      previous, current)),
    "neoipcr_infectious_agent_removed_id")
  expect_no_error(suppressMessages(
    neoipcr:::compare_infectious_agent_classifications(
      previous, current, acknowledged_removed_ids = 31L)))
})

# --- The digests' texts -------------------------------------------------------

digest_texts <- function(tables)
  neoipcr:::infectious_agent_digest_texts(tables)

# The digests whose text the tables derived from `tree` and `classification`
# change against those of `base`.
changed_digests <- function(base, tree = scaffold_tree(),
                            classification = scaffold_classification()) {
  before <- digest_texts(base)
  after <- digest_texts(derive_scaffold(tree, classification))
  names(before)[before != after]
}

flag <- function(node, name, value = TRUE, tree = scaffold_tree())
  edit_node(tree, node, function(n) {
    n[[name]] <- value
    n
  })

renamed <- function(node, name, tree = scaffold_tree())
  edit_node(tree, node, function(n) {
    n$Name <- name
    n
  })

with_candida_species <- function(...)
  edit_node(scaffold_tree(), "Candida", function(n) {
    n$Children <- list(c(
      list(Name = "Candida albicans", ConceptType = "Species",
           ConceptSource = "MycoBank", ConceptId = 256187L, Id = 41L),
      list(...)))
    n
  })

test_that("the provenance's digests are the classification, each version's tree, the concept each Id names, the common commensals, and the resistance categories' applicability", {
  expect_identical(names(digest_texts(derive_scaffold())),
                   c("classification", "groups_tree", "ontology_tree",
                     "concept_mapping", "common_commensal", "applicability"))
})

test_that("the groups tree digest covers what places an Id among the rows over the reporting groups and their summary block, the classification its columns", {
  expect_identical(
    neoipcr:::infectious_agent_groups_tree_digest_columns,
    c("input_id", "reporting_group", "gram_reaction", "oxygen_requirement"))
})

test_that("a common-commensal flag changes the classification and common-commensal digests alone", {
  base <- derive_scaffold()

  expect_identical(changed_digests(base, flag("Candida", "CommonCommensal")),
                   c("classification", "common_commensal"))
  expect_identical(
    changed_digests(base, flag("Coagulase-negative staphylococci",
                               "CommonCommensal", FALSE)),
    c("classification", "common_commensal"))
})

test_that("a resistance category applied to another Id changes the classification and applicability digests alone", {
  base <- derive_scaffold()

  expect_identical(changed_digests(base, flag("Candida", "MRSA")),
                   c("classification", "applicability"))
  expect_identical(changed_digests(base, flag("Klebsiella", "Colistin",
                                              FALSE)),
                   c("classification", "applicability"))
})

test_that("an Id added changes the flag digests only where a flag applies to it", {
  base <- derive_scaffold()
  added <- c("classification", "groups_tree", "ontology_tree",
             "concept_mapping")

  expect_identical(changed_digests(base, with_candida_species()), added)
  expect_identical(changed_digests(base, with_candida_species(VRE = TRUE)),
                   c(added, "applicability"))
  expect_identical(
    changed_digests(base, with_candida_species(CommonCommensal = TRUE)),
    c(added, "common_commensal"))
})

test_that("a subspecies that becomes a synonym of its species changes the concept mapping digest alone, beside the classification digest", {
  base <- derive_scaffold()
  tree <- edit_node(scaffold_tree(), "Staphylococcus aureus", function(n) {
    n$Synonyms <- c(n$Synonyms, n$Children)
    n$Children <- NULL
    n
  })

  # The subspecies counted in its species before as after, on the same path,
  # but now as the species' concept, so that an infection recording both
  # counts one infectious agent where it counted two.
  derived <- derive_scaffold(tree)
  expect_identical(rows_of(base$classification, 16L)$output_id, 16L)
  expect_identical(rows_of(derived$classification, 16L)$output_id, 14L)
  expect_identical(path_of(derived, 16L), path_of(base, 16L))
  expect_identical(changed_digests(base, tree),
                   c("classification", "concept_mapping"))
})

test_that("a node renamed or moved, or a corrected record number, changes the ontology tree digest, a group of the classification or an attribute value renamed, or an Id placed in another group, the groups tree digest, and a synonym renamed the classification digest alone", {
  base <- derive_scaffold(streptococci_tree(), streptococci_classification())
  changed <- function(tree = streptococci_tree(),
                      classification = streptococci_classification())
    changed_digests(base, tree, classification)
  ontology_changed <- c("classification", "ontology_tree")
  groups_changed <- c("classification", "groups_tree")

  # A group named in no rank column, and one without an Id, are covered by
  # the ontology tree digest through the nodes.
  expect_identical(
    changed(renamed("Streptococcus anginosus group",
                    "Streptococcus milleri group", streptococci_tree())),
    ontology_changed)
  expect_identical(
    changed(renamed("Streptococcus mitis group", "Mitis group",
                    streptococci_tree())),
    ontology_changed)
  moved <- edit_node(streptococci_tree(), "Viridans streptococci",
                     function(n) {
                       f <- n$Children[[1]]$Children[[1]]
                       n$Children[[1]]$Children <- NULL
                       n$Children <- c(n$Children, list(f))
                       n
                     })
  expect_identical(changed(moved), ontology_changed)
  corrected <- edit_node(streptococci_tree(), "Staphylococcaceae",
                         function(n) {
                           n$ConceptId <- "family/staphylococcaceae-1"
                           n
                         })
  expect_identical(changed(corrected), ontology_changed)

  expect_identical(
    changed(classification = edit_group(
      streptococci_classification(), "fungi", function(g) {
        g$Name <- "Fungi of all kinds"
        g
      })),
    groups_changed)
  expect_identical(
    changed(classification = edit_assignments(
      streptococci_classification(), "gram-reaction", function(a) a)),
    character())
  renamed_value <- streptococci_classification()
  renamed_value$Attributes[[1]]$Values[[1]]$Name <- "Gram-positive bacteria"
  expect_identical(changed(classification = renamed_value), groups_changed)
  expect_identical(
    changed(classification = add_member(
      streptococci_classification(), "enterobacterales",
      entry("Streptococcus", "LPSN", "genus/streptococcus"))),
    character())
  regrouped <- edit_group(streptococci_classification(), "gram-positive-cocci",
                          function(g) {
                            g$Members <- g$Members[1]
                            g
                          })
  regrouped <- add_member(regrouped, "enterobacterales",
                          entry("Streptococcus", "LPSN", "genus/streptococcus"))
  expect_identical(changed(classification = regrouped), groups_changed)

  expect_identical(
    changed(renamed("Micrococcus aureus", "Micrococcus aureus Rosenbach",
                    streptococci_tree())),
    "classification")
})

test_that("a group's ECDC category corrected, a group on no Id's path added, an attribute renamed, or a value no Id takes added or renamed, none of which a table reads, changes the classification digest alone", {
  base <- derive_scaffold()

  corrected <- edit_group(scaffold_classification(), "enterobacterales",
                          function(g) {
                            g$EcdcCategory <- "Gram-negative bacilli"
                            g
                          })
  expect_identical(changed_digests(base, classification = corrected),
                   "classification")

  # A group that no member places holds no Id, so that it is a row of no
  # table.
  added <- edit_group(scaffold_classification(), "bacteria", function(g) {
    g$Children <- c(g$Children, list(list(
      Key = "anaerobes", Name = "Anaerobes", EcdcCategory = "Anaerobes")))
    g
  })
  expect_identical(changed_digests(base, classification = added),
                   "classification")

  renamed_attribute <- scaffold_classification()
  renamed_attribute$Attributes[[1]]$Name <- "Gram stain"
  expect_identical(changed_digests(base, classification = renamed_attribute),
                   "classification")

  # A value that no assignment gives an Id is the summary row of no table,
  # whatever its key, even one another attribute takes, and its name, in
  # either attribute.
  with_value <- function(attribute, key, name) {
    x <- scaffold_classification()
    x$Attributes[[attribute]]$Values <- c(
      x$Attributes[[attribute]]$Values, list(list(Key = key, Name = name)))
    x
  }
  expect_identical(
    changed_digests(base, classification = with_value(1L, "variable",
                                                      "Gram-variable")),
    "classification")
  expect_identical(
    changed_digests(base, classification = with_value(2L, "aerobic",
                                                      "Aerobic")),
    "classification")
  # The Gram reaction's Ids take the key "positive", which the oxygen
  # requirement's Ids do not.
  expect_identical(
    changed_digests(base, classification = with_value(2L, "positive",
                                                      "Positive")),
    "classification")
  untaken <- derive_scaffold(
    classification = with_value(1L, "variable", "Gram-variable"))
  expect_identical(
    changed_digests(untaken, classification = with_value(
      1L, "variable", "Gram-indeterminate")),
    "classification")
  expect_identical(
    changed_digests(untaken, classification = with_value(
      1L, "indeterminate", "Gram-variable")),
    "classification")
})

test_that("a release that gives an ICTV node another ConceptId leaves every digest as it is, and one that renames it too only the digests that cover its name", {
  base <- derive_scaffold()

  expect_identical(
    changed_digests(base, with_virus_genus(function(n) {
      n$ConceptId <- 202501234L
      n
    })),
    character())
  expect_identical(
    changed_digests(base, with_virus_genus(function(n) {
      n$ConceptId <- 202501234L
      n$Name <- "Orthoflavivirus novus"
      n
    })),
    c("classification", "ontology_tree"))
})

test_that("the digests' texts do not depend on the order of the rows", {
  tables <- derive_scaffold(streptococci_tree(),
                            streptococci_classification())
  shuffled <- lapply(tables, function(x) x[rev(seq_len(nrow(x))), ])

  expect_identical(digest_texts(shuffled), digest_texts(tables))
})

test_that("the digests' texts tell a missing value from the text NA, and keep a value's tabs and line breaks apart", {
  tables <- derive_scaffold()
  named <- tables
  named$classification$species[named$classification$input_id == 40L] <- "NA"
  expect_false(identical(digest_texts(named), digest_texts(tables)))

  a <- tables
  b <- tables
  a$classification$input_name[1] <- "a\tb"
  b$classification$input_name[1] <- "a\\tb"
  expect_false(identical(digest_texts(a), digest_texts(b)))
})
