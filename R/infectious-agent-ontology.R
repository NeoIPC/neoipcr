# The derivation of the bundled infectious-agent tables from the NeoIPC
# infectious-agent list and its reporting classification, and the checks that
# hold the tables to what the package relies on. Both inputs are YAML as
# yaml12::parse_yaml(simplify = FALSE) returns it: a mapping is a named list, a
# sequence an unnamed one, and a scalar a vector of length one. The walk and the
# resolution of the classification mirror the Surveillance-Toolkit's, whose
# export of the two files' code sets the tests hold the tables to.
#
# Nothing here runs when the package is used, which reads the tables from
# R/sysdata.rda. data-raw/sysdata.R loads this file alone, through
# sys.source(), so it calls other packages through `pkg::` and nothing else of
# this package.

# The list's top-level nodes, each the `hierarchy` of the rows beneath it.
infectious_agent_hierarchies <- c(
  "Not listed", "Unidentifiable", "Viruses", "Bacteria", "Eukaryota")

# The top-level nodes of the list beneath which the paths over the full
# ontology start: such a node is no node of that version, and each concept
# directly beneath it is a top-level node there, at depth 0. The full
# ontology's top-level rows are thus Bacteria, Fungi, Protozoa, and Viruses,
# Eukaryota, which holds the fungi and the protozoa, having none. An Id at such
# a node would have no row, which the contract refuses.
infectious_agent_skipped_hierarchies <- "Eukaryota"

# The ConceptSource whose nodes are keyed over the full ontology by their
# IctvId rather than their ConceptId: an ICTV node's ConceptId is its node in
# the Master Species List of the International Committee on Taxonomy of
# Viruses that the list follows, which changes with each release the list
# adopts, unchanged taxa included, while its IctvId, unique among the list's
# concepts, stays a taxon's across releases.
infectious_agent_ictv_source <- "ICTV"

# The ranks a node's ConceptType may name.
infectious_agent_concept_types <- c(
  "Realm", "Domain", "Kingdom", "Subkingdom", "Phylum", "Subphylum", "Class",
  "Subclass", "Order", "Suborder", "Family", "Subfamily", "Genus", "Subgenus",
  "Group", "Species", "Subspecies", "Variety", "Serotype", "Unknown")

# The rank columns, by the ConceptType of the node each names.
infectious_agent_rank_columns <- c(
  kingdom = "Kingdom", order = "Order", family = "Family", genus = "Genus",
  species = "Species")

# The flag columns, by the key of the list's flag each takes.
infectious_agent_flag_columns <- c(
  common_commensal = "CommonCommensal", applies_3gcr = "3GCR",
  applies_car = "Carbapenems", applies_cor = "Colistin", applies_mrsa = "MRSA",
  applies_vre = "VRE")

# The coagulase of the staphylococci, by the group of the genus Staphylococcus
# that holds them.
infectious_agent_coagulase_groups <- c(
  "Coagulase-negative staphylococci" = "n",
  "Coagulase-positive staphylococci" = "p",
  "Coagulase-variable staphylococci" = "v")

# The attribute columns, by the Key of the classification's attribute each
# takes.
infectious_agent_attribute_columns <- c(
  gram_reaction = "gram-reaction", oxygen_requirement = "oxygen-requirement")

# The options for an infectious agent the list lacks and for one the laboratory
# could not identify, which say nothing of what the agent was.
infectious_agent_not_listed_id <- 0L
infectious_agent_unidentifiable_id <- 3561L

# The versions of the rows of the infectious-agent tables: over the reporting
# groups, and over the full ontology.
infectious_agent_row_versions <- c("groups", "ontology")

# The bundled tables' columns and their classes.
infectious_agent_classification_columns <- c(
  input_id = "integer", input_name = "character", output_id = "integer",
  output_name = "character", hierarchy = "character", kingdom = "character",
  order = "character", family = "character", genus = "character",
  species = "character", coagulase = "character",
  common_commensal = "logical", applies_3gcr = "logical",
  applies_car = "logical", applies_cor = "logical", applies_mrsa = "logical",
  applies_vre = "logical", reporting_group = "character",
  gram_reaction = "character", oxygen_requirement = "character")
infectious_agent_reporting_group_columns <- c(
  key = "character", name = "character", parent_key = "character",
  ecdc_category = "character")
infectious_agent_attribute_value_columns <- c(
  attribute = "character", attribute_name = "character", key = "character",
  name = "character")
infectious_agent_node_columns <- c(
  version = "character", node_id = "character", parent_id = "character",
  name = "character", rank = "character", depth = "integer")
infectious_agent_closure_columns <- c(
  version = "character", input_id = "integer", node_id = "character",
  depth = "integer")

# The classification's columns the groups tree digest covers, beside the
# `attribute`, `key`, and `name` of each attribute value some Id takes and
# the nodes and closure over the reporting groups: what places an Id among
# the rows of that version and its summary block and labels them. The nodes
# carry the key, name, parent, and depth of every group a row can be, which
# the contract holds to the group tree, so the group tree stays out, its ECDC
# categories and its groups on no Id's path with it, and so do each
# attribute's name and the values no Id takes, which are the summary row of
# no table: no table reads them. The rank columns, the hierarchy, and the
# coagulase, which no table reads, stay out too, and so do the flags and the
# concept each Id names, each with a digest of its own, so that a change to
# one alone leaves the tables that do not read it comparable.
infectious_agent_groups_tree_digest_columns <- c(
  "input_id", "reporting_group", names(infectious_agent_attribute_columns))

#' Derive the bundled infectious-agent tables
#'
#' The rows of the infectious-agent tables come in two versions, `"groups"`,
#' over the reporting groups, and `"ontology"`, over the full ontology. Both
#' are given by a node table and a closure, so that the tables find an Id's
#' rows with one join: an Id's path in a version runs from a top-level node of
#' that version's tree, at depth 0, to the node the Id is recorded at, its
#' deepest node there, each node on it once.
#'
#' * Over the reporting groups, an Id is recorded at its reporting group, and
#'   its path holds that group and each group above it.
#' * Over the full ontology, an Id is recorded at its concept, a synonym
#'   standing for the concept it names, or, where the concept lies beneath a
#'   species, as a subspecies, a serotype, or a group of a species does, at the
#'   nearest species above it. Its path holds every concept from the list's
#'   top-level node down to that node, those without an Id and the groups among
#'   them, so that no node is collapsed into its only child; beneath a
#'   top-level node of `infectious_agent_skipped_hierarchies`, it starts at the
#'   concept directly beneath that node.
#'
#' The Ids 0 and 3561, which say nothing of what the infectious agent was, are
#' on no path. A node is keyed by `node_id`: over the full ontology its
#' `ConceptSource` and `ConceptId`, separated by `|`, an ICTV node's `IctvId`
#' standing in for its `ConceptId` (see `infectious_agent_ictv_source`), and
#' over the reporting groups its group's key, each of which stays what it is
#' for a node that a release leaves unchanged, so that the rows of datasets
#' computed apart match.
#'
#' @param tree The parsed infectious-agent list.
#' @param classification The parsed reporting classification.
#' @returns A list of five tibbles:
#'   * `classification`, one row per Id-bearing node of the list, in ascending
#'     order of `input_id`;
#'   * `reporting_groups`, the tree of reporting groups, in pre-order;
#'   * `attribute_values`, the values of the attributes the classification
#'     table carries: `attribute`, the table's column, `attribute_name`, `key`,
#'     the value the column holds, and `name`, in the classification's order;
#'   * `nodes`, one row per node on a path: `version`, `node_id`, `parent_id`,
#'     `NA` for a top-level node, `name`, `rank`, the node's `ConceptType`, `NA`
#'     for a reporting group and a node without one, and `depth`, 0 for a
#'     top-level node; the groups first, then the ontology, each in pre-order;
#'   * `closure`, one row per Id and node on its path in each version:
#'     `version`, `input_id`, `node_id`, and `depth`, ascending by `version`,
#'     `input_id`, and `depth`.
#' @noRd
derive_infectious_agent_tables <- function(tree, classification)
{
  nodes <- walk_infectious_agent_tree(tree)
  check_infectious_agent_nodes(nodes)
  lineage <- infectious_agent_lineage(nodes)
  reporting_groups <- infectious_agent_reporting_group_tree(classification)
  groups <- resolve_infectious_agent_placements(
    nodes, infectious_agent_member_placements(classification),
    bacteria_only = FALSE)
  attributes <- lapply(
    infectious_agent_attribute_placements(classification),
    function(placements)
      resolve_infectious_agent_placements(nodes, placements,
                                          bacteria_only = TRUE))

  rows <- which(!is.na(nodes$id))
  rows <- rows[order(nodes$id[rows])]
  concept <- nodes$concept[rows]
  columns <- list(
    input_id = nodes$id[rows],
    input_name = nodes$name[rows],
    output_id = nodes$id[concept],
    output_name = nodes$name[concept],
    hierarchy = nodes$top[rows])
  for (column in names(infectious_agent_rank_columns))
    columns[[column]] <- lineage[[column]][concept]
  columns$coagulase <- lineage$coagulase[concept]
  for (column in names(infectious_agent_flag_columns))
    columns[[column]] <- nodes[[column]][rows]
  columns$reporting_group <- groups[rows]
  for (column in names(infectious_agent_attribute_columns))
    columns[[column]] <- attributes[[column]][rows]

  inside <- !columns$input_id %in% c(infectious_agent_not_listed_id,
                                     infectious_agent_unidentifiable_id)
  over_groups <- infectious_agent_group_paths(
    reporting_groups, columns$input_id[inside],
    columns$reporting_group[inside])
  over_ontology <- infectious_agent_ontology_paths(nodes, rows[inside])

  list(classification = tibble::as_tibble(columns),
       reporting_groups = reporting_groups,
       attribute_values = infectious_agent_attribute_values(classification),
       nodes = rbind(over_groups$nodes, over_ontology$nodes),
       closure = rbind(over_groups$closure, over_ontology$closure))
}

#' Walk the infectious-agent list
#'
#' Visits a node, then its `Hierarchies`, `Synonyms`, and `Children`, depth
#' first, as the toolkit's walkers do. A synonym names the concept whose
#' `Synonyms` hold it, and a synonym's children are concepts beneath that
#' concept. A flag takes the nearest explicit value on the walk's path to the
#' node, an explicit false overriding, and is false where none is set.
#'
#' @param tree The parsed infectious-agent list.
#' @returns A tibble with one row per node, in the order visited: `id`, `NA`
#'   for a node without one; `name`; `concept_type`; `is_synonym`; `concept`,
#'   the row of the concept the node names, its own for a concept; `parent`,
#'   the row of the concept above a concept, `NA` for a top-level node and a
#'   synonym; `top`, the name of its top-level node; `key`, its
#'   `ConceptSource` and `ConceptId` as the classification names it;
#'   `node_key`, its `node_id` over the full ontology
#'   (`infectious_agent_node_key()`); and the flag columns.
#' @noRd
walk_infectious_agent_tree <- function(tree)
{
  nodes <- list()
  visit <- function(value, synonym, concept, parent, top, inherited)
  {
    if (is_yaml_sequence(value)) {
      for (item in value)
        visit(item, synonym, concept, parent, top, inherited)
      return(invisible())
    }
    if (!is_yaml_mapping(value))
      return(invisible())

    index <- length(nodes) + 1L
    name <- yaml_text(value[["Name"]])
    if (!nzchar(trimws(name)))
      rlang::abort(
        c("A node of the infectious-agent list has a blank Name.",
          "i" = sprintf("It is node %d in the order of the walk, beneath '%s'.",
                        index, if (is.na(top)) "the list" else top)),
        class = c("neoipcr_infectious_agent_blank_name",
                  "neoipcr_infectious_agent_ontology_error"),
        .internal = TRUE)
    check_infectious_agent_name(name, "node of the infectious-agent list")
    if (is.na(top)) {
      if (!name %in% infectious_agent_hierarchies)
        rlang::abort(
          c(sprintf("The infectious-agent list has the top-level node '%s'.",
                    name),
            "i" = sprintf("Its top-level nodes are %s.",
                          paste0("'", infectious_agent_hierarchies, "'",
                                 collapse = ", "))),
          class = c("neoipcr_infectious_agent_unexpected_hierarchy",
                    "neoipcr_infectious_agent_ontology_error"),
          .internal = TRUE)
      top <- name
    }

    id <- NA_integer_
    if ("Id" %in% names(value)) {
      id <- infectious_agent_id(value[["Id"]])
      if (is.na(id))
        rlang::abort(
          sprintf("The infectious-agent node '%s' has the Id '%s', which is no code, a whole number from 0 to %d.",
                  name, yaml_text(value[["Id"]]), .Machine$integer.max),
          class = c("neoipcr_infectious_agent_invalid_id",
                    "neoipcr_infectious_agent_ontology_error"),
          .internal = TRUE)
    }

    concept_type <- NA_character_
    if ("ConceptType" %in% names(value)) {
      concept_type <- yaml_text(value[["ConceptType"]])
      if (!concept_type %in% infectious_agent_concept_types)
        rlang::abort(
          sprintf("The infectious-agent node '%s' has the ConceptType '%s', which is none of the list's ranks.",
                  name, concept_type),
          class = c("neoipcr_infectious_agent_unknown_concept_type",
                    "neoipcr_infectious_agent_ontology_error"),
          .internal = TRUE)
    }

    effective <- inherited
    for (column in names(infectious_agent_flag_columns)) {
      flag <- infectious_agent_flag_columns[[column]]
      if (!flag %in% names(value))
        next
      effective[[column]] <- infectious_agent_flag(value[[flag]])
      if (is.na(effective[[column]]))
        rlang::abort(
          sprintf("The flag %s of the infectious-agent node '%s' is '%s', neither true nor false.",
                  flag, name, yaml_text(value[[flag]])),
          class = c("neoipcr_infectious_agent_invalid_flag",
                    "neoipcr_infectious_agent_ontology_error"),
          .internal = TRUE)
    }

    own <- if (synonym) concept else index
    nodes[[index]] <<- c(
      list(id = id, name = name, concept_type = concept_type,
           is_synonym = synonym, concept = own,
           parent = if (synonym) NA_integer_ else parent, top = top,
           key = infectious_agent_concept_key(value),
           node_key = infectious_agent_node_key(value)),
      effective)
    if ("Hierarchies" %in% names(value))
      visit(value[["Hierarchies"]], FALSE, NA_integer_, own, top, effective)
    if ("Synonyms" %in% names(value))
      visit(value[["Synonyms"]], TRUE, own, NA_integer_, top, effective)
    if ("Children" %in% names(value))
      visit(value[["Children"]], FALSE, NA_integer_, own, top, effective)
    invisible()
  }

  none <- as.list(rep(FALSE, length(infectious_agent_flag_columns)))
  names(none) <- names(infectious_agent_flag_columns)
  if (is_yaml_mapping(tree))
    visit(tree[["Hierarchies"]], FALSE, NA_integer_, NA_integer_,
          NA_character_, none)

  column <- function(name, type) vapply(nodes, function(n) n[[name]], type)
  out <- list(
    id = column("id", integer(1)),
    name = enc2utf8(column("name", character(1))),
    concept_type = column("concept_type", character(1)),
    is_synonym = column("is_synonym", logical(1)),
    concept = column("concept", integer(1)),
    parent = column("parent", integer(1)),
    top = enc2utf8(column("top", character(1))),
    key = enc2utf8(column("key", character(1))),
    node_key = enc2utf8(column("node_key", character(1))))
  for (flag in names(infectious_agent_flag_columns))
    out[[flag]] <- column(flag, logical(1))
  tibble::as_tibble(out)
}

# Refuses a walk whose Ids repeat, whose synonyms name concepts without an Id,
# or whose concepts of one rank share a name, which would make the rank
# columns ambiguous.
check_infectious_agent_nodes <- function(nodes)
{
  bearing <- !is.na(nodes$id)
  repeated <- unique(nodes$id[bearing][duplicated(nodes$id[bearing])])
  if (length(repeated) > 0L) {
    named <- nodes$name[bearing & nodes$id %in% repeated[[1]]]
    rlang::abort(
      c(sprintf("The Id %d is that of more than one node of the infectious-agent list: %s.",
                repeated[[1]], paste0("'", named, "'", collapse = ", ")),
        "i" = sprintf("Repeated Ids: %s.", paste(repeated, collapse = ", "))),
      class = c("neoipcr_infectious_agent_repeated_id",
                "neoipcr_infectious_agent_ontology_error"),
      .internal = TRUE)
  }

  orphans <- which(bearing & nodes$is_synonym &
                     is.na(nodes$id[nodes$concept]))
  if (length(orphans) > 0L) {
    first <- orphans[[1]]
    rlang::abort(
      sprintf("The synonym '%s' (Id %d) belongs to '%s', which has no Id for the synonym to stand for.",
              nodes$name[first], nodes$id[first],
              nodes$name[nodes$concept[first]]),
      class = c("neoipcr_infectious_agent_orphan_synonym",
                "neoipcr_infectious_agent_ontology_error"),
      .internal = TRUE)
  }

  ranked <- which(!nodes$is_synonym & !is.na(nodes$concept_type))
  shared <- duplicated(nodes[ranked, c("concept_type", "name")])
  if (any(shared)) {
    first <- ranked[shared][[1]]
    rlang::abort(
      sprintf("The name '%s' is that of more than one concept of the rank %s in the infectious-agent list.",
              nodes$name[first], nodes$concept_type[first]),
      class = c("neoipcr_infectious_agent_ambiguous_name",
                "neoipcr_infectious_agent_ontology_error"),
      .internal = TRUE)
  }
  invisible(nodes)
}

# Refuses a name that holds a character neoipcr reserves.
check_infectious_agent_name <- function(name, what)
{
  if (grepl("[|~_]", name))
    rlang::abort(
      sprintf("The name '%s' of a %s holds one of the reserved characters |, ~, and _.",
              name, what),
      class = c("neoipcr_infectious_agent_reserved_character",
                "neoipcr_infectious_agent_ontology_error"),
      .internal = TRUE)
  invisible(name)
}

# The rank columns and the coagulase of every concept of the walk, each the
# nearest at or above it, a synonym taking its concept's. Groups stand between
# the ranks without breaking the lineage, and a node beneath a species, such
# as a subspecies, reports that species.
infectious_agent_lineage <- function(nodes)
{
  ranks <- matrix(
    NA_character_, nrow(nodes), length(infectious_agent_rank_columns),
    dimnames = list(NULL, names(infectious_agent_rank_columns)))
  coagulase <- rep(NA_character_, nrow(nodes))
  # A concept's parent is visited before it, so one pass in the order of the
  # walk sees each parent's lineage complete.
  for (i in which(!nodes$is_synonym)) {
    parent <- nodes$parent[i]
    if (!is.na(parent)) {
      ranks[i, ] <- ranks[parent, ]
      coagulase[i] <- coagulase[parent]
    }
    rank <- match(nodes$concept_type[i], infectious_agent_rank_columns)
    if (!is.na(rank))
      ranks[i, rank] <- nodes$name[i]
    if (identical(nodes$concept_type[i], "Group") && !is.na(parent) &&
        identical(nodes$concept_type[parent], "Genus") &&
        identical(nodes$name[parent], "Staphylococcus")) {
      coagulase[i] <- unname(infectious_agent_coagulase_groups[nodes$name[i]])
      if (is.na(coagulase[i]))
        rlang::abort(
          c(sprintf("The group '%s' of the genus Staphylococcus maps to no coagulase.",
                    nodes$name[i]),
            "i" = sprintf("The groups that map to one are %s.",
                          paste0("'", names(infectious_agent_coagulase_groups),
                                 "'", collapse = ", "))),
          class = c("neoipcr_infectious_agent_unmapped_coagulase_group",
                    "neoipcr_infectious_agent_ontology_error"),
          .internal = TRUE)
    }
  }
  synonyms <- which(nodes$is_synonym)
  ranks[synonyms, ] <- ranks[nodes$concept[synonyms], ]
  coagulase[synonyms] <- coagulase[nodes$concept[synonyms]]

  lineage <- lapply(names(infectious_agent_rank_columns),
                    function(column) unname(ranks[, column]))
  names(lineage) <- names(infectious_agent_rank_columns)
  lineage$coagulase <- coagulase
  lineage
}

#' Resolve members or assignments to the nodes of the walk
#'
#' Resolves as the toolkit's `Resolve-NeoIPCReportingGroupPlacement()` does: a
#' placement names a concept by its `ConceptSource` and `ConceptId`; one that
#' names no concept, or more than one, places nothing, and of two that name one
#' concept the first counts. A node takes the nearest placement on the path from
#' its concept, itself included, to its top-level node, through concepts of
#' every rank, those without an Id and groups among them; the path from a
#' concept whose key another concept shares runs from the first of them.
#'
#' @param nodes The walk, as `walk_infectious_agent_tree()` returns it.
#' @param placements A tibble of `key` and `value`, in the classification's
#'   order.
#' @param bacteria_only Whether the placements apply to the bacteria alone, so
#'   that every other node takes none.
#' @returns The value each node of the walk takes, `NA` for none.
#' @noRd
resolve_infectious_agent_placements <- function(nodes, placements,
                                                bacteria_only)
{
  concepts <- which(!nodes$is_synonym)
  keys <- nodes$key[concepts]
  first <- concepts[!duplicated(keys)]
  parent_key <- stats::setNames(nodes$key[nodes$parent[first]],
                                nodes$key[first])
  bacterial <- stats::setNames(nodes$top[first] == "Bacteria",
                               nodes$key[first])
  shared <- unique(keys[duplicated(keys)])

  placed <- placements$key %in% names(parent_key) &
    !placements$key %in% shared
  if (bacteria_only)
    placed <- placed & bacterial[placements$key] %in% TRUE
  placed[placed] <- !duplicated(placements$key[placed])
  effective <- stats::setNames(placements$value[placed],
                               placements$key[placed])

  resolved <- vapply(names(parent_key), function(key) {
    while (!is.na(key)) {
      if (key %in% names(effective))
        return(effective[[key]])
      key <- parent_key[[key]]
    }
    NA_character_
  }, character(1))

  concept_key <- nodes$key[nodes$concept]
  value <- unname(resolved[concept_key])
  if (bacteria_only)
    value[!bacterial[concept_key]] <- NA_character_
  value
}

# The members of the classification's leaf groups, depth first, each placing
# the node it names in its group.
infectious_agent_member_placements <- function(classification)
{
  keys <- character()
  values <- character()
  visit <- function(groups)
  {
    for (group in yaml_items(groups)) {
      if (!is_yaml_mapping(group))
        next
      if ("Children" %in% names(group)) {
        visit(group[["Children"]])
        next
      }
      if (!"Members" %in% names(group))
        next
      leaf <- yaml_text(group[["Key"]])
      for (member in Filter(is_yaml_mapping, yaml_items(group[["Members"]]))) {
        keys <<- c(keys, infectious_agent_concept_key(member))
        values <<- c(values,
                     if (nzchar(leaf)) leaf else yaml_text(member[["Value"]]))
      }
    }
  }
  if (is_yaml_mapping(classification))
    visit(classification[["Groups"]])
  tibble::tibble(key = keys, value = values)
}

# The assignments of the attributes the classification table carries, each
# placing the node it names at its value; of two attributes with one Key, the
# first counts.
infectious_agent_attribute_placements <- function(classification)
{
  out <- lapply(infectious_agent_attribute_columns, function(attribute)
    tibble::tibble(key = character(), value = character()))
  taken <- character()
  attributes <- if (is_yaml_mapping(classification))
    yaml_items(classification[["Attributes"]]) else list()
  for (attribute in Filter(is_yaml_mapping, attributes)) {
    column <- names(infectious_agent_attribute_columns)[
      match(yaml_text(attribute[["Key"]]), infectious_agent_attribute_columns)]
    if (is.na(column) || column %in% taken)
      next
    taken <- c(taken, column)
    entries <- Filter(is_yaml_mapping, yaml_items(attribute[["Assignments"]]))
    out[[column]] <- tibble::tibble(
      key = vapply(entries, infectious_agent_concept_key, character(1)),
      value = vapply(entries, function(e) yaml_text(e[["Value"]]),
                     character(1)))
  }
  out
}

# The classification's groups in pre-order, each with its key, its name, its
# parent's key, and its own ECDC category.
infectious_agent_reporting_group_tree <- function(classification)
{
  rows <- list()
  visit <- function(groups, parent)
  {
    for (group in Filter(is_yaml_mapping, yaml_items(groups))) {
      key <- blank_as_na(yaml_text(group[["Key"]]))
      name <- blank_as_na(yaml_text(group[["Name"]]))
      if (!is.na(name))
        check_infectious_agent_name(name, "reporting group")
      category <- NA_character_
      if ("EcdcCategory" %in% names(group))
        category <- blank_as_na(yaml_text(group[["EcdcCategory"]]))
      rows[[length(rows) + 1L]] <<- list(
        key = key, name = name, parent_key = parent, ecdc_category = category)
      if ("Children" %in% names(group))
        visit(group[["Children"]], key)
    }
  }
  if (is_yaml_mapping(classification))
    visit(classification[["Groups"]], NA_character_)
  column <- function(name)
    enc2utf8(vapply(rows, function(r) r[[name]], character(1)))
  tibble::tibble(
    key = column("key"),
    name = column("name"),
    parent_key = column("parent_key"),
    ecdc_category = column("ecdc_category"))
}

# The values of the attributes the classification table carries, each with
# its attribute's name, in the classification's order; of two attributes with
# one Key, the first counts, as for their assignments.
infectious_agent_attribute_values <- function(classification)
{
  rows <- list()
  taken <- character()
  attributes <- if (is_yaml_mapping(classification))
    yaml_items(classification[["Attributes"]]) else list()
  for (attribute in Filter(is_yaml_mapping, attributes)) {
    column <- names(infectious_agent_attribute_columns)[
      match(yaml_text(attribute[["Key"]]), infectious_agent_attribute_columns)]
    if (is.na(column) || column %in% taken)
      next
    taken <- c(taken, column)
    attribute_name <- blank_as_na(yaml_text(attribute[["Name"]]))
    for (value in Filter(is_yaml_mapping, yaml_items(attribute[["Values"]]))) {
      name <- blank_as_na(yaml_text(value[["Name"]]))
      if (!is.na(name))
        check_infectious_agent_name(name, "value of an attribute")
      rows[[length(rows) + 1L]] <- list(
        attribute = column, attribute_name = attribute_name,
        key = blank_as_na(yaml_text(value[["Key"]])), name = name)
    }
  }
  column <- function(name)
    enc2utf8(vapply(rows, function(r) r[[name]], character(1)))
  tibble::tibble(
    attribute = column("attribute"),
    attribute_name = column("attribute_name"),
    key = column("key"),
    name = column("name"))
}

# The attribute values some Id of the classification table takes, each as
# its attribute (the table's column) and its key, separated by a tab.
infectious_agent_taken_attribute_values <- function(classification)
  unlist(lapply(names(infectious_agent_attribute_columns), function(column) {
    x <- classification[[column]]
    paste(column, unique(x[!is.na(x)]), sep = "\t", recycle0 = TRUE)
  }))

#' The paths of the Ids over the reporting groups
#'
#' @param reporting_groups The group tree, in pre-order.
#' @param ids The Ids whose paths to give.
#' @param groups The reporting group of each of `ids`; an Id without one has
#'   no path.
#' @returns A list of `nodes` and `closure`, in the shape
#'   `derive_infectious_agent_tables()` documents, of the version `"groups"`.
#' @noRd
infectious_agent_group_paths <- function(reporting_groups, ids, groups)
{
  # A group's parent precedes it in pre-order, and so does the first group
  # that shares the parent's key, so each step up the tree moves to an
  # earlier group and every walk ends.
  parent <- match(reporting_groups$parent_key, reporting_groups$key,
                  incomparables = NA)
  depth <- integer(nrow(reporting_groups))
  for (i in seq_len(nrow(reporting_groups)))
    if (!is.na(parent[i]))
      depth[i] <- depth[parent[i]] + 1L
  paths <- lapply(match(groups, reporting_groups$key, incomparables = NA),
                  function(group) infectious_agent_path(group, parent))
  infectious_agent_paths(
    "groups", ids, paths, node_id = reporting_groups$key,
    parent_id = reporting_groups$parent_key, name = reporting_groups$name,
    rank = rep(NA_character_, nrow(reporting_groups)), depth = depth)
}

#' The paths of the Ids over the full ontology
#'
#' An Id is recorded at its concept or, where the concept lies beneath a
#' species, at the nearest species above it, and its path runs from the list's
#' top-level node to there through every concept between, or, beneath a
#' top-level node of `infectious_agent_skipped_hierarchies`, from the concept
#' directly beneath it. An Id recorded at such a node has no path.
#'
#' @param nodes The walk, as `walk_infectious_agent_tree()` returns it.
#' @param rows The rows of the walk whose paths to give, each an Id-bearing
#'   node, a concept or a synonym.
#' @returns A list of `nodes` and `closure`, in the shape
#'   `derive_infectious_agent_tables()` documents, of the version `"ontology"`.
#' @noRd
infectious_agent_ontology_paths <- function(nodes, rows)
{
  skipped <- !nodes$is_synonym & is.na(nodes$parent) &
    nodes$name %in% infectious_agent_skipped_hierarchies
  parent <- nodes$parent
  parent[parent %in% which(skipped)] <- NA_integer_
  depth <- rep(NA_integer_, nrow(nodes))
  species <- rep(NA_integer_, nrow(nodes))
  # A concept's parent is visited before it, so one pass in the order of the
  # walk sees each parent's depth and species.
  for (i in which(!nodes$is_synonym & !skipped)) {
    if (is.na(parent[i])) {
      depth[i] <- 0L
    } else {
      depth[i] <- depth[parent[i]] + 1L
      species[i] <- species[parent[i]]
    }
    if (identical(nodes$concept_type[i], "Species"))
      species[i] <- i
  }
  recorded <- ifelse(is.na(species), seq_len(nrow(nodes)), species)
  recorded[skipped] <- NA_integer_
  paths <- lapply(recorded[nodes$concept[rows]],
                  function(node) infectious_agent_path(node, parent))
  infectious_agent_paths(
    "ontology", nodes$id[rows], paths, node_id = nodes$node_key,
    parent_id = nodes$node_key[parent], name = nodes$name,
    rank = nodes$concept_type, depth = depth)
}

# The rows from a tree's top-level row down to `row`, following `parent`;
# none where `row` is NA.
infectious_agent_path <- function(row, parent)
{
  path <- integer()
  while (!is.na(row)) {
    path <- c(row, path)
    row <- parent[row]
  }
  path
}

# The nodes on the paths, in the order of the tree's rows, and the closure,
# one row per Id and node on its path, from the paths as rows of the tree and
# the tree's columns.
infectious_agent_paths <- function(version, ids, paths, node_id, parent_id,
                                   name, rank, depth)
{
  steps <- as.integer(unlist(paths, use.names = FALSE))
  on_path <- sort(unique(steps))
  list(
    nodes = tibble::tibble(
      version = rep(version, length(on_path)),
      node_id = enc2utf8(node_id[on_path]),
      parent_id = enc2utf8(parent_id[on_path]),
      name = enc2utf8(name[on_path]),
      rank = rank[on_path],
      depth = depth[on_path]),
    closure = tibble::tibble(
      version = rep(version, length(steps)),
      input_id = rep(ids, lengths(paths)),
      node_id = enc2utf8(node_id[steps]),
      depth = depth[steps]))
}

#' Assert the bundled infectious-agent tables' contract
#'
#' The tables carry their documented columns, of their classes; the option for
#' an infectious agent the list lacks (Id 0) is a top-level node of its own, to
#' which every resistance category applies, in no reporting group; the option
#' for one the laboratory could not identify (Id 3561) is one too, without any
#' flag, in no reporting group; every other top-level node holds Id-bearing
#' nodes; no Id is at a top-level node of
#' `infectious_agent_skipped_hierarchies`, where it would have no row over the
#' full ontology; every other Id is in a leaf of the group tree; every
#' bacterium has a Gram reaction and an oxygen requirement, and no other node
#' either; the group tree's keys are unique, each group has a name, and each
#' parent is a group; the staphylococci take each of the three coagulases;
#' each attribute value the classification gives is one of its attribute's
#' values, each listed once with a name; each node has a version, a `node_id`
#' that no other node of its version has, over the full ontology both a
#' `ConceptSource` and a `ConceptId`, an ICTV node an `IctvId` in place of the
#' `ConceptId`, a name, and a parent of its version one level above it, and is
#' on a path, a node of the groups version being its reporting group; every Id
#' but 0 and 3561 has a path in each version, and no other Id one; and each
#' path is a chain from a top-level node to the node its Id is recorded at:
#' over the reporting groups its reporting group, and over the full ontology,
#' from its hierarchy, or from beneath it where the paths start beneath it,
#' its species or, without one, its concept.
#'
#' @param tables The derived tables, as `derive_infectious_agent_tables()`
#'   returns them.
#' @returns `TRUE`, invisibly; each element of the contract the tables break is
#'   an error of a class of its own.
#' @noRd
assert_infectious_agent_contract <- function(tables)
{
  call <- rlang::current_env()
  violation <- function(class, message)
    rlang::abort(
      message,
      class = c(class, "neoipcr_infectious_agent_contract_violation",
                "neoipcr_infectious_agent_ontology_error"),
      call = call,
      .internal = TRUE)
  among <- function(x, n = 10L)
    sprintf("%d, among them %s", length(x),
            paste(utils::head(x, n), collapse = ", "))

  for (table in list(
    list(name = "classification", x = tables$classification,
         columns = infectious_agent_classification_columns),
    list(name = "group tree", x = tables$reporting_groups,
         columns = infectious_agent_reporting_group_columns),
    list(name = "attribute values", x = tables$attribute_values,
         columns = infectious_agent_attribute_value_columns),
    list(name = "node table", x = tables$nodes,
         columns = infectious_agent_node_columns),
    list(name = "closure", x = tables$closure,
         columns = infectious_agent_closure_columns))) {
    classes <- if (is.data.frame(table$x))
      vapply(table$x, function(column) class(column)[[1]], character(1))
    else character()
    if (!identical(classes, table$columns))
      violation("neoipcr_infectious_agent_contract_column", c(
        sprintf("The %s does not carry the columns of the contract.",
                table$name),
        "i" = sprintf("Expected: %s.", paste0(names(table$columns), " <",
                                               table$columns, ">",
                                               collapse = ", ")),
        "x" = sprintf("Found: %s.", paste0(names(classes), " <", classes, ">",
                                            collapse = ", "))))
  }
  classification <- tables$classification
  reporting_groups <- tables$reporting_groups

  resistance <- grep("^applies_", names(infectious_agent_flag_columns),
                     value = TRUE)
  option <- function(id, hierarchy, class, flags) {
    row <- classification[classification$input_id %in% id, ]
    if (nrow(row) != 1L || !identical(row$hierarchy, hierarchy))
      violation(class, sprintf(
        "The classification has no top-level node '%s' with the Id %d.",
        hierarchy, id))
    if (!identical(unlist(row[names(flags)], use.names = FALSE),
                   unname(flags)))
      violation(class, c(
        sprintf("The flags of the Id %d are not those of the contract.", id),
        "i" = sprintf("Expected: %s.", paste(names(flags), flags, sep = " = ",
                                             collapse = ", "))))
    if (!is.na(row$reporting_group))
      violation(class, sprintf(
        "The Id %d is in the reporting group '%s', while it says nothing of what the infectious agent was.",
        id, row$reporting_group))
  }
  option(infectious_agent_not_listed_id, "Not listed",
         "neoipcr_infectious_agent_contract_not_listed",
         stats::setNames(rep(TRUE, length(resistance)), resistance))
  option(infectious_agent_unidentifiable_id, "Unidentifiable",
         "neoipcr_infectious_agent_contract_unidentifiable",
         stats::setNames(rep(FALSE, length(infectious_agent_flag_columns)),
                         names(infectious_agent_flag_columns)))

  absent <- setdiff(infectious_agent_hierarchies, classification$hierarchy)
  if (length(absent) > 0L)
    violation("neoipcr_infectious_agent_contract_hierarchy", sprintf(
      "No Id-bearing node of the classification is beneath the top-level node %s.",
      paste0("'", absent, "'", collapse = ", ")))

  # An Id at such a node is one whose concept bears the node's name and that
  # has no path over the full ontology; one beneath it has a path.
  over_ontology <- tables$closure$input_id[
    tables$closure$version == "ontology"]
  at_skipped <- classification$input_id[
    classification$hierarchy %in% infectious_agent_skipped_hierarchies &
      classification$output_name == classification$hierarchy &
      !classification$input_id %in% over_ontology]
  if (length(at_skipped) > 0L)
    violation("neoipcr_infectious_agent_contract_skipped_node", c(
      sprintf("An Id is at the top-level node %s, beneath which the paths over the full ontology start, so that it has no row there.",
              paste0("'", infectious_agent_skipped_hierarchies, "'",
                     collapse = ", ")),
      "x" = sprintf("Ids that break it: %s.", among(at_skipped))))

  leaves <- setdiff(reporting_groups$key, c(reporting_groups$parent_key, NA))
  grouped <- !classification$input_id %in% c(
    infectious_agent_not_listed_id, infectious_agent_unidentifiable_id)
  ungrouped <- classification$input_id[
    grouped & !classification$reporting_group %in% leaves]
  if (length(ungrouped) > 0L)
    violation("neoipcr_infectious_agent_contract_reporting_group", c(
      "Ids other than 0 and 3561 are in no leaf of the group tree.",
      "x" = sprintf("Ungrouped: %d, among them %s.", length(ungrouped),
                    paste(utils::head(ungrouped, 10L), collapse = ", "))))

  bacterium <- classification$hierarchy == "Bacteria"
  for (column in names(infectious_agent_attribute_columns)) {
    wrong <- classification$input_id[
      is.na(classification[[column]]) == bacterium]
    if (length(wrong) > 0L)
      violation("neoipcr_infectious_agent_contract_attribute", c(
        sprintf("Every bacterium has a %s, and no other node has one.", column),
        "x" = sprintf("Ids that break it: %d, among them %s.", length(wrong),
                      paste(utils::head(wrong, 10L), collapse = ", "))))
  }

  key <- reporting_groups$key
  if (anyNA(key) || anyDuplicated(key) || anyNA(reporting_groups$name) ||
      !all(reporting_groups$parent_key %in% c(key, NA)))
    violation("neoipcr_infectious_agent_contract_group_tree",
              "The group tree has a key missing or twice, a group without a name, or a parent that is no group.")

  coagulase <- classification$coagulase
  if (length(setdiff(infectious_agent_coagulase_groups, coagulase)) > 0L ||
      !all(coagulase %in% c(infectious_agent_coagulase_groups, NA)))
    violation("neoipcr_infectious_agent_contract_coagulase", sprintf(
      "The staphylococci do not take each of the coagulases %s, and only those.",
      paste(infectious_agent_coagulase_groups, collapse = ", ")))

  values <- tables$attribute_values
  value_keys <- paste(values$attribute, values$key, sep = "\t")
  given <- infectious_agent_taken_attribute_values(classification)
  unknown <- setdiff(given, value_keys)
  if (length(unknown) > 0L || anyNA(values$key) || anyNA(values$name) ||
      anyDuplicated(value_keys) > 0L ||
      !all(values$attribute %in% names(infectious_agent_attribute_columns)))
    violation("neoipcr_infectious_agent_contract_attribute_value", c(
      "An attribute value of the classification is none of its attribute's values, or a value is listed twice or without a key or a name.",
      "x" = sprintf("Values that are none of their attribute's: %s.",
                    if (length(unknown) > 0L)
                      among(sub("\t", " = ", unknown, fixed = TRUE))
                    else "none")))

  nodes <- tables$nodes
  closure <- tables$closure
  ontology <- nodes$version == "ontology"
  parts <- strsplit(nodes$node_id, "|", fixed = TRUE)
  keyed <- !is.na(nodes$node_id) & nzchar(trimws(nodes$node_id)) &
    (!ontology | vapply(parts, function(p)
      length(p) == 2L && all(nzchar(trimws(p))), logical(1)))
  node_keys <- paste(nodes$version, nodes$node_id, sep = "\t")
  repeated <- duplicated(node_keys)
  if (!all(keyed) || any(repeated))
    violation("neoipcr_infectious_agent_contract_node_id", c(
      "A node has no node_id, a node of the full ontology lacks its ConceptSource or its ConceptId, an ICTV node its IctvId, or two nodes of a version share a node_id.",
      "x" = sprintf("Nodes that break it: %s.",
                    among(unique(nodes$name[!keyed | repeated])))))

  same <- function(a, b)
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  parent <- match(paste(nodes$version, nodes$parent_id, sep = "\t"),
                  node_keys)
  parent[is.na(nodes$parent_id)] <- NA_integer_
  placed <- ifelse(is.na(nodes$parent_id), nodes$depth == 0L,
                   !is.na(parent) & nodes$depth == nodes$depth[parent] + 1L)
  group_nodes <- which(nodes$version == "groups")
  group <- match(nodes$node_id[group_nodes], reporting_groups$key)
  grouped <- rep(TRUE, nrow(nodes))
  grouped[group_nodes] <- !is.na(group) &
    same(nodes$name[group_nodes], reporting_groups$name[group]) &
    same(nodes$parent_id[group_nodes], reporting_groups$parent_key[group]) &
    is.na(nodes$rank[group_nodes])
  sound <- nodes$version %in% infectious_agent_row_versions &
    !is.na(nodes$name) & nzchar(trimws(nodes$name)) & placed %in% TRUE &
    grouped
  if (!all(sound))
    violation("neoipcr_infectious_agent_contract_node_tree", c(
      "A node has no version or no name, a parent that is no node of its version or a depth that is not its parent's plus one, or, over the reporting groups, is not its reporting group.",
      "x" = sprintf("Nodes that break it: %s.",
                    among(nodes$name[!sound]))))

  inside <- setdiff(classification$input_id,
                    c(infectious_agent_not_listed_id,
                      infectious_agent_unidentifiable_id))
  for (version in infectious_agent_row_versions) {
    with_path <- unique(closure$input_id[closure$version == version])
    missing <- setdiff(inside, with_path)
    extra <- setdiff(with_path, inside)
    tree <- if (version == "groups") "reporting groups" else "full ontology"
    if (length(missing) > 0L || length(extra) > 0L)
      violation("neoipcr_infectious_agent_contract_closure", c(
        sprintf("Over the %s, an Id other than 0 and 3561 has no path, or an Id that is 0, 3561, or no Id of the classification has one.",
                tree),
        "x" = sprintf("Without a path: %s.",
                      if (length(missing) > 0L) among(missing) else "none"),
        "x" = sprintf("With a path that should have none: %s.",
                      if (length(extra) > 0L) among(extra) else "none")))
  }
  if (!all(closure$version %in% infectious_agent_row_versions))
    violation("neoipcr_infectious_agent_contract_closure",
              "A path of the closure is of no version of the tables' rows.")

  ordered <- closure[order(closure$version, closure$input_id, closure$depth,
                           method = "radix"), ]
  path <- paste(ordered$version, ordered$input_id, sep = "\t")
  position <- stats::ave(seq_along(path), path, FUN = seq_along) - 1L
  node <- match(paste(ordered$version, ordered$node_id, sep = "\t"),
                node_keys)
  previous <- utils::head(c(NA_character_, ordered$node_id), nrow(ordered))
  previous[position == 0L] <- NA_character_
  linked <- !is.na(node) & same(ordered$depth, position) &
    same(nodes$depth[node], ordered$depth) &
    same(nodes$parent_id[node], previous)
  id <- match(ordered$input_id, classification$input_id)
  first <- position == 0L & ordered$version == "ontology"
  # Beneath a skipped top-level node, a path starts at a node beneath it,
  # which is none of the list's top-level nodes.
  top_name <- nodes$name[node[first]]
  hierarchy <- classification$hierarchy[id[first]]
  linked[first] <- linked[first] &
    ifelse(hierarchy %in% infectious_agent_skipped_hierarchies,
           !is.na(top_name) & !top_name %in% infectious_agent_hierarchies,
           same(top_name, hierarchy))
  last <- !duplicated(path, fromLast = TRUE)
  recorded <- ifelse(ordered$version == "groups",
                     classification$reporting_group[id],
                     ifelse(is.na(classification$species[id]),
                            classification$output_name[id],
                            classification$species[id]))
  reached <- ifelse(ordered$version == "groups", ordered$node_id,
                    nodes$name[node])
  linked[last] <- linked[last] & same(reached[last], recorded[last])
  broken <- unique(path[!linked %in% TRUE])
  if (length(broken) > 0L)
    violation("neoipcr_infectious_agent_contract_path", c(
      "A path is no chain from a top-level node, one node per level, each beneath the one before, to the node its Id is recorded at: over the reporting groups its reporting group, and over the full ontology, from its hierarchy, or from beneath it where the paths start beneath it, its species or, without one, its concept.",
      "x" = sprintf("Paths that break it, by version and Id: %s.",
                    among(sub("\t", " ", broken, fixed = TRUE)))))

  # Checked after the paths, so that a path cut short is reported as such
  # rather than as the nodes it no longer reaches.
  stranded <- !node_keys %in% paste(closure$version, closure$node_id,
                                    sep = "\t")
  if (any(stranded))
    violation("neoipcr_infectious_agent_contract_node_tree", c(
      "A node lies on no path.",
      "x" = sprintf("Nodes on no path: %s.", among(nodes$name[stranded]))))

  invisible(TRUE)
}

#' Compare a derived classification with the bundled one
#'
#' Shows the rows per hierarchy and the sizes of the code sets of both, and the
#' Ids added and removed, and refuses an Id removed that is not acknowledged.
#'
#' @param previous The bundled classification, `NULL` where there is none.
#' @param current The derived classification.
#' @param acknowledged_removed_ids The Ids the list is known to have removed.
#' @returns `NULL` where there is no bundled classification; otherwise, a list
#'   of `hierarchies` and `sets`, each a tibble of the counts before and after,
#'   and the Ids `added` and `removed`, invisibly.
#' @noRd
compare_infectious_agent_classifications <- function(
    previous, current, acknowledged_removed_ids = integer())
{
  if (is.null(previous)) {
    rlang::inform(
      "No infectious-agent classification is bundled yet, so there is nothing to compare the derived one with.")
    return(invisible(NULL))
  }

  count <- function(x, values) vapply(values, function(v) sum(x == v),
                                      integer(1))
  hierarchy <- sort(union(previous$hierarchy, current$hierarchy),
                    method = "radix")
  hierarchies <- tibble::tibble(
    hierarchy = hierarchy,
    previous = unname(count(previous$hierarchy, hierarchy)),
    current = unname(count(current$hierarchy, hierarchy)))
  set_sizes <- function(x) c(
    concepts = sum(x$input_id == x$output_id),
    synonyms = sum(x$input_id != x$output_id),
    vapply(names(infectious_agent_flag_columns),
           function(flag) sum(x[[flag]]), integer(1)))
  sets <- tibble::tibble(
    set = names(set_sizes(current)),
    previous = unname(set_sizes(previous)),
    current = unname(set_sizes(current)))
  added <- setdiff(current$input_id, previous$input_id)
  removed <- setdiff(previous$input_id, current$input_id)
  renamed <- intersect(previous$input_id, current$input_id)
  renamed <- renamed[previous$output_name[match(renamed, previous$input_id)] !=
                       current$output_name[match(renamed, current$input_id)]]

  # Named " ", so that rlang indents each line rather than bulleting it.
  indented <- function(x) {
    lines <- utils::capture.output(print(as.data.frame(x), row.names = FALSE))
    stats::setNames(lines, rep(" ", length(lines)))
  }
  rlang::inform(c(
    "The derived infectious-agent classification beside the bundled one:",
    indented(hierarchies), indented(sets),
    "i" = sprintf("Ids added: %d; removed: %d; with another concept name: %d.",
                  length(added), length(removed), length(renamed))))

  unacknowledged <- setdiff(removed, acknowledged_removed_ids)
  if (length(unacknowledged) > 0L)
    rlang::abort(
      c("The derived classification lacks Ids the bundled one has, which no one has acknowledged as removed.",
        "x" = sprintf("Unacknowledged: %d, among them %s.",
                      length(unacknowledged),
                      paste(utils::head(sort(unacknowledged), 10L),
                            collapse = ", ")),
        "i" = "Acknowledge each in data-raw/sysdata.R once it is known to be removed from the list."),
      class = c("neoipcr_infectious_agent_removed_id",
                "neoipcr_infectious_agent_ontology_error"),
      .internal = TRUE)

  invisible(list(hierarchies = hierarchies, sets = sets, added = added,
                 removed = removed))
}

#' The texts the provenance's digests are taken over
#'
#' Each digest covers one part of the bundled tables that the tables of a
#' calculated dataset read, so that a comparison of the tables of datasets
#' computed apart keys on the digests of what each reads, and each changes
#' when its part changes, and only then:
#'
#' * `classification`: every bundled table, whole;
#' * `groups_tree`: what places an Id among the rows over the reporting
#'   groups and their summary block and labels them: the classification's
#'   columns `infectious_agent_groups_tree_digest_columns` names, the
#'   `attribute`, `key`, and `name` of each attribute value some Id takes,
#'   and the nodes and the closure of the groups version, whose nodes carry
#'   the key, name, parent, and depth of every group a row can be;
#' * `ontology_tree`: what places an Id among the rows over the full ontology
#'   and labels them: the nodes and the closure of the ontology version;
#' * `concept_mapping`: the concept each Id names (`input_id`, `output_id`),
#'   by which the tables count an infectious agent once per infection;
#' * `common_commensal`: the Ids flagged as common commensals;
#' * `applicability`: the Ids to which each resistance category applies.
#'
#' The two digests of flags cover the Ids whose flag is set, not every Id, since
#' a code the list lacks counts as neither a common commensal nor an infectious
#' agent to which a resistance category applies: an Id added or removed
#' without the flag leaves what those flags decide unchanged. No digest but
#' `classification` covers the classification's rank columns, `hierarchy`,
#' `coagulase`, `input_name`, or `output_name`, the group tree, with its ECDC
#' categories and its groups on no Id's path, an attribute's name, or an
#' attribute value no Id takes, which no table reads: a table labels its rows
#' with the names of the nodes and of the attribute values its Ids take.
#'
#' @param tables The derived tables, as `derive_infectious_agent_tables()`
#'   returns them.
#' @returns A named character vector of the six texts, each UTF-8, as
#'   `infectious_agent_digest_text()` writes them.
#' @noRd
infectious_agent_digest_texts <- function(tables)
{
  classification <- tables$classification
  of_version <- function(table, version)
    tables[[table]][tables[[table]]$version == version, ]
  flagged <- function(column)
    classification$input_id[classification[[column]] %in% TRUE]
  resistance <- setdiff(names(infectious_agent_flag_columns),
                        "common_commensal")
  applicable <- lapply(resistance, flagged)
  values <- tables$attribute_values
  taken <- paste(values$attribute, values$key, sep = "\t") %in%
    infectious_agent_taken_attribute_values(classification)

  c(classification = infectious_agent_digest_text(
      tables[c("classification", "reporting_groups", "attribute_values",
               "nodes", "closure")]),
    groups_tree = infectious_agent_digest_text(list(
      classification =
        classification[infectious_agent_groups_tree_digest_columns],
      attribute_values = values[taken, c("attribute", "key", "name")],
      nodes = of_version("nodes", "groups"),
      closure = of_version("closure", "groups"))),
    ontology_tree = infectious_agent_digest_text(list(
      nodes = of_version("nodes", "ontology"),
      closure = of_version("closure", "ontology"))),
    concept_mapping = infectious_agent_digest_text(list(
      concept_mapping = classification[c("input_id", "output_id")])),
    common_commensal = infectious_agent_digest_text(list(
      common_commensals = tibble::tibble(
        input_id = flagged("common_commensal")))),
    applicability = infectious_agent_digest_text(list(
      applicability = tibble::tibble(
        category = rep(resistance, lengths(applicable)),
        input_id = as.integer(unlist(applicable, use.names = FALSE))))))
}

#' The text a digest is taken over
#'
#' Each table under a header line, its column names, and one line per row,
#' fields separated by tabs; a missing value is written `\N`, and a backslash,
#' a tab, a carriage return, and a line feed within a value are escaped. Each
#' table's rows ascend by its columns in turn, in the C locale's order, so that
#' the text depends on the tables' content alone.
#'
#' @param tables A named list of data frames.
#' @returns A character scalar, UTF-8.
#' @noRd
infectious_agent_digest_text <- function(tables)
{
  field <- function(x) {
    x <- as.character(x)
    escaped <- gsub("\\", "\\\\", x, fixed = TRUE)
    escaped <- gsub("\t", "\\t", escaped, fixed = TRUE)
    escaped <- gsub("\r", "\\r", escaped, fixed = TRUE)
    escaped <- gsub("\n", "\\n", escaped, fixed = TRUE)
    escaped[is.na(x)] <- "\\N"
    enc2utf8(escaped)
  }
  lines <- function(title, x) {
    if (nrow(x) > 0L)
      x <- x[do.call(order, c(unname(as.list(x)), method = "radix")), ]
    c(paste0("# ", title),
      paste(names(x), collapse = "\t"),
      if (nrow(x) > 0L) do.call(paste, c(lapply(x, field), sep = "\t")))
  }
  paste(unlist(Map(lines, names(tables), tables), use.names = FALSE),
        collapse = "\n")
}

is_yaml_mapping <- function(x) is.list(x) && !is.null(names(x))

is_yaml_sequence <- function(x) is.list(x) && is.null(names(x))

# A sequence's items, or a mapping as the one item, as PowerShell's @() reads
# a value; nothing for a scalar.
yaml_items <- function(x)
{
  if (is_yaml_mapping(x))
    return(list(x))
  if (is_yaml_sequence(x))
    return(x)
  list()
}

# A scalar as the toolkit reads one through PowerShell's [string]: the empty
# text for nothing or a collection, True or False for a Boolean, and a whole
# number without an exponent.
yaml_text <- function(x)
{
  if (is.null(x) || is.list(x) || length(x) != 1L || is.na(x))
    return("")
  if (is.logical(x))
    return(if (x) "True" else "False")
  if (is.double(x) && is.finite(x) && x == trunc(x))
    return(format(x, scientific = FALSE, trim = TRUE))
  as.character(x)
}

blank_as_na <- function(x) if (nzchar(trimws(x))) x else NA_character_

infectious_agent_concept_key <- function(node)
  paste0(yaml_text(node[["ConceptSource"]]), "|",
         yaml_text(node[["ConceptId"]]))

# A node's node_id over the full ontology: its ConceptSource and ConceptId,
# but an ICTV node's IctvId in place of its ConceptId (see
# infectious_agent_ictv_source), so that an ICTV node without an IctvId has
# the node_id "ICTV|", which the contract refuses.
infectious_agent_node_key <- function(node)
{
  source <- yaml_text(node[["ConceptSource"]])
  if (!identical(source, infectious_agent_ictv_source))
    return(infectious_agent_concept_key(node))
  paste0(source, "|", yaml_text(node[["IctvId"]]))
}

# An Id as the toolkit's walkers read one, through int.TryParse() of its text,
# but from 0 only, since it is the code of an option of non-negative integers:
# an integer, a whole number of another type, or a text of digits, from 0 to
# the largest 32-bit integer; NA for any other value.
infectious_agent_id <- function(x)
{
  if (is.list(x) || length(x) != 1L)
    return(NA_integer_)
  if (is.character(x)) {
    if (!grepl("^[[:space:]]*[+]?[0-9]+[[:space:]]*$", x))
      return(NA_integer_)
    x <- as.numeric(x)
  }
  if (!is.numeric(x) || !is.finite(x) || x != trunc(x) || x < 0 ||
      x > .Machine$integer.max)
    return(NA_integer_)
  as.integer(x)
}

# A flag as the toolkit's walkers read one: a Boolean, or a text that
# bool.TryParse() reads, true or false in any letter case between blanks; NA
# for any other value.
infectious_agent_flag <- function(x)
{
  if (is.list(x) || length(x) != 1L)
    return(NA)
  if (is.logical(x))
    return(x)
  if (is.character(x) && !is.na(x)) {
    text <- tolower(trimws(x))
    if (text == "true")
      return(TRUE)
    if (text == "false")
      return(FALSE)
  }
  NA
}
