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
# genus with a synonym, the staphylococci in their three coagulase groups, an
# order of Gram-negative bacteria, and a fungal genus.
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
           ConceptSource = "ICTV", ConceptId = 202401234L, Id = 20L,
           Synonyms = list(list(
             Name = "Flavivirus", ConceptSource = "ICTV",
             ConceptId = 201901234L, Id = 21L))))),
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
  neoipcr:::assert_infectious_agent_contract(
    tables$classification, tables$reporting_groups)

test_that("the scaffold derives one row per Id-bearing node, typed as the contract requires, which it passes", {
  tables <- derive_scaffold()
  x <- tables$classification

  expect_identical(
    x$input_id, c(0L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 20L, 21L, 30L,
                  31L, 40L, 3561L))
  expect_identical(
    names(x), c("input_id", "input_name", "output_id", "output_name",
                "hierarchy", "kingdom", "order", "family", "genus", "species",
                "agent_group", "coagulase", "common_commensal",
                "applies_3gcr", "applies_car", "applies_cor", "applies_mrsa",
                "applies_vre", "reporting_group", "gram_reaction",
                "oxygen_requirement"))
  expect_false(any(vapply(x, is.factor, logical(1))))
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
  x <- rows_of(derive_scaffold()$classification, c(16L, 13L, 31L, 40L, 0L))

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
  expect_identical(x$agent_group,
                   c("Coagulase-positive staphylococci",
                     "Coagulase-positive staphylococci", NA, NA, NA))
  expect_identical(x$hierarchy, c("Bacteria", "Bacteria", "Bacteria",
                                  "Eukaryota", "Not listed"))
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

test_that("a name holding one of the reserved characters is refused, a group's included", {
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
  tables$classification$agent_group <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$classification$coagulase <- factor(tables$classification$coagulase)
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")

  tables <- derive_scaffold()
  tables$reporting_groups$parent_key <- NULL
  expect_contract_error(tables, "neoipcr_infectious_agent_contract_column")
})

test_that("a list without one of its five top-level nodes breaks the contract", {
  for (top in c("Viruses", "Eukaryota")) {
    tree <- edit_node(scaffold_tree(), top, function(n) NULL)
    expect_contract_error(derive_scaffold(tree),
                          "neoipcr_infectious_agent_contract_hierarchy")
  }
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

# --- The digests' text ----------------------------------------------------------

digest_text <- function(tables, columns = names(tables$classification))
  neoipcr:::infectious_agent_digest_text(tables$classification,
                                         tables$reporting_groups, columns)

tables_digest_text <- function(tables)
  digest_text(tables, neoipcr:::infectious_agent_tables_digest_columns)

test_that("the tables digest covers what places an Id in a table's rows, and no flag", {
  expect_identical(
    neoipcr:::infectious_agent_tables_digest_columns,
    c("input_id", "hierarchy", "kingdom", "order", "family", "genus",
      "species", "agent_group", "coagulase", "reporting_group",
      "gram_reaction", "oxygen_requirement"))

  tables <- derive_scaffold()
  flagged <- derive_scaffold(edit_node(scaffold_tree(), "Candida",
                                       function(n) {
                                         n$CommonCommensal <- TRUE
                                         n
                                       }))
  expect_identical(tables_digest_text(flagged), tables_digest_text(tables))
  expect_false(identical(digest_text(flagged), digest_text(tables)))

  regrouped <- derive_scaffold(classification = edit_group(
    scaffold_classification(), "fungi", function(g) {
      g$Name <- "Fungi of all kinds"
      g
    }))
  expect_false(identical(tables_digest_text(regrouped),
                         tables_digest_text(tables)))
})

test_that("the digests' text does not depend on the order of the rows", {
  tables <- derive_scaffold()
  shuffled <- tables
  shuffled$classification <- tables$classification[
    rev(seq_len(nrow(tables$classification))), ]
  shuffled$reporting_groups <- tables$reporting_groups[
    rev(seq_len(nrow(tables$reporting_groups))), ]

  expect_identical(digest_text(shuffled), digest_text(tables))
})

test_that("the digests' text tells a missing value from the text NA, and keeps a value's tabs and line breaks apart", {
  tables <- derive_scaffold()
  named <- tables
  named$classification$agent_group[named$classification$input_id == 40L] <- "NA"
  expect_false(identical(digest_text(named), digest_text(tables)))

  a <- tables
  b <- tables
  a$classification$input_name[1] <- "a\tb"
  b$classification$input_name[1] <- "a\\tb"
  expect_false(identical(digest_text(a), digest_text(b)))
})
