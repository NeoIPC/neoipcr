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

# The ranks a node's ConceptType may name.
infectious_agent_concept_types <- c(
  "Realm", "Domain", "Kingdom", "Subkingdom", "Phylum", "Subphylum", "Class",
  "Subclass", "Order", "Suborder", "Family", "Subfamily", "Genus", "Subgenus",
  "Group", "Species", "Subspecies", "Variety", "Serotype", "Unknown")

# The rank columns, by the ConceptType of the node each names.
infectious_agent_rank_columns <- c(
  kingdom = "Kingdom", order = "Order", family = "Family", genus = "Genus",
  species = "Species", agent_group = "Group")

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

# The bundled tables' columns and their classes.
infectious_agent_classification_columns <- c(
  input_id = "integer", input_name = "character", output_id = "integer",
  output_name = "character", hierarchy = "character", kingdom = "character",
  order = "character", family = "character", genus = "character",
  species = "character", agent_group = "character", coagulase = "character",
  common_commensal = "logical", applies_3gcr = "logical",
  applies_car = "logical", applies_cor = "logical", applies_mrsa = "logical",
  applies_vre = "logical", reporting_group = "character",
  gram_reaction = "character", oxygen_requirement = "character")
infectious_agent_reporting_group_columns <- c(
  key = "character", name = "character", parent_key = "character",
  ecdc_category = "character")

# The columns the tables digest covers: what places an Id among a reporting
# table's rows. The flags stay out, so that a change of flags alone leaves the
# tables of stored reference datasets comparable.
infectious_agent_tables_digest_columns <- c(
  "input_id", "hierarchy", names(infectious_agent_rank_columns), "coagulase",
  "reporting_group", names(infectious_agent_attribute_columns))

#' Derive the bundled infectious-agent tables
#'
#' @param tree The parsed infectious-agent list.
#' @param classification The parsed reporting classification.
#' @returns A list of two tibbles: `classification`, one row per Id-bearing
#'   node of the list in ascending order of `input_id`, and `reporting_groups`,
#'   the tree of reporting groups in pre-order.
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

  list(classification = tibble::as_tibble(columns),
       reporting_groups = reporting_groups)
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
#'   `ConceptSource` and `ConceptId` as the classification names it; and the
#'   flag columns.
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
           key = infectious_agent_concept_key(value)),
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
    key = enc2utf8(column("key", character(1))))
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

#' Assert the bundled infectious-agent tables' contract
#'
#' The tables carry their documented columns, of their classes; the option for
#' an infectious agent the list lacks (Id 0) is a top-level node of its own, to
#' which every resistance category applies, in no reporting group; the option
#' for one the laboratory could not identify (Id 3561) is one too, without any
#' flag, in no reporting group; every other top-level node holds Id-bearing
#' nodes; every other Id is in a leaf of the group tree; every bacterium has a
#' Gram reaction and an oxygen requirement, and no other node either; the group
#' tree's keys are unique, each group has a name, and each parent is a group;
#' and the staphylococci take each of the three coagulases.
#'
#' @param classification The derived classification table.
#' @param reporting_groups The derived group tree.
#' @returns `TRUE`, invisibly; each element of the contract the tables break is
#'   an error of a class of its own.
#' @noRd
assert_infectious_agent_contract <- function(classification, reporting_groups)
{
  call <- rlang::current_env()
  violation <- function(class, message)
    rlang::abort(
      message,
      class = c(class, "neoipcr_infectious_agent_contract_violation",
                "neoipcr_infectious_agent_ontology_error"),
      call = call,
      .internal = TRUE)

  for (table in list(
    list(name = "classification", x = classification,
         columns = infectious_agent_classification_columns),
    list(name = "group tree", x = reporting_groups,
         columns = infectious_agent_reporting_group_columns))) {
    classes <- vapply(table$x, function(column) class(column)[[1]],
                      character(1))
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

#' The text the provenance's digests are taken over
#'
#' Each table under a header line, its column names, and one line per row,
#' fields separated by tabs; a missing value is written `\N`, and a backslash,
#' a tab, a carriage return, and a line feed within a value are escaped. The
#' classification's rows ascend by `input_id` and the group tree's by `key` in
#' the C locale's order, so that the text depends on the tables' content alone.
#'
#' @param classification The classification table.
#' @param reporting_groups The group tree.
#' @param columns The classification's columns the text covers; the group tree's
#'   are all covered.
#' @returns A character scalar, UTF-8.
#' @noRd
infectious_agent_digest_text <- function(classification, reporting_groups,
                                         columns)
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
  lines <- function(title, x)
    c(paste0("# ", title),
      paste(names(x), collapse = "\t"),
      if (nrow(x) > 0L) do.call(paste, c(lapply(x, field), sep = "\t")))

  classification <- classification[order(classification$input_id), columns]
  reporting_groups <- reporting_groups[
    order(reporting_groups$key, method = "radix"), ]
  paste(c(lines("classification", classification),
          lines("reporting_groups", reporting_groups)),
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
