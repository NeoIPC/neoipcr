# Builds R/sysdata.rda, the package's bundled infectious-agent tables, from a
# pinned release of the NeoIPC infectious-agent list and its reporting
# classification in the Surveillance-Toolkit repository.
#
#   Rscript data-raw/sysdata.R
#     fetches both files and the list's VERSION at the pinned commit, and both
#     files at the pinned tag; stops unless each file has its pinned SHA-256,
#     the tag's bytes are the commit's, and VERSION names the tag's version;
#     derives the tables, checks them, shows how they differ from the bundled
#     ones, and writes R/sysdata.rda.
#   Rscript data-raw/sysdata.R --check
#     derives the tables in the same way and fails where they differ from those
#     R/sysdata.rda holds, writing nothing.
#   --untagged
#     builds from a pin that names no tag yet, alone or with --check: fetches
#     both files at the commit alone and warns that the two checks against the
#     tag were skipped, and the bundle records neither a tag nor a version. A
#     pin without a tag needs it, and a pin with one refuses it.
#
# Run it from the package root. It needs R 4.5 or later, for tools::sha256sum(),
# and the packages that DESCRIPTION's Config/Needs/data-raw names beside its
# imports. The derivation is loaded alone, through sys.source(), never through
# pkgload::load_all(), which would also load an untracked local data/ directory.

options(warn = 1)

if (getRversion() < "4.5.0")
  rlang::abort(sprintf(
    "data-raw/sysdata.R needs R 4.5 or later, for tools::sha256sum(); this is R %s.",
    getRversion()))
for (package in c("httr2", "rlang", "tibble", "yaml12"))
  if (!requireNamespace(package, quietly = TRUE))
    rlang::abort(sprintf(
      "data-raw/sysdata.R needs the package %s, which is not installed.",
      package))
if (!file.exists("DESCRIPTION") ||
    !identical(unname(read.dcf("DESCRIPTION", fields = "Package")[1, 1]),
               "neoipcr"))
  rlang::abort("Run data-raw/sysdata.R from the root of the neoipcr package.")

# The pin: the release of the list and its classification the tables come
# from, and the SHA-256 of each file there.
repository <- "NeoIPC/Surveillance-Toolkit"
tag <- "infectious-agents-v0.2.0-alpha"
commit <- "0e8833ff707c6b3fbbb02d6cd3773bdc94567d1d"
directory <- "metadata/common/infectious-agents"
files <- c(
  list = "NeoIPC-Infectious-Agents.yaml",
  reporting_classification = "NeoIPC-Infectious-Agent-Reporting-Groups.yaml")
sha256 <- c(
  list = "963218192deda2709dd9a60e7c9d2f3194ab105fbd9fbcf076cff958a3901c92",
  reporting_classification = "e3fa55934a23cd1724c88aaca9d2572a8ae695f7522a7e4641c77ae196c4e95e")

# The Ids the list has removed since the bundled release, each acknowledged
# once the removal is known to be intended.
acknowledged_removed_ids <- integer()

arguments <- commandArgs(trailingOnly = TRUE)
unknown <- setdiff(arguments, c("--check", "--untagged"))
if (length(unknown) > 0L)
  rlang::abort(c(
    sprintf("data-raw/sysdata.R takes no argument %s.",
            paste0("'", unknown, "'", collapse = ", ")),
    "i" = "It takes --check and --untagged."))
check <- "--check" %in% arguments
untagged <- "--untagged" %in% arguments
if (is.na(tag) && !untagged)
  rlang::abort(c(
    "The pin names no tag of the release, so the files cannot be checked against one.",
    "i" = "Pass --untagged to build from the commit alone, or pin the release's tag."))
if (!is.na(tag) && untagged)
  rlang::abort(sprintf(
    "The pin names the tag %s, so --untagged, which skips the checks against it, is refused.",
    tag))

fetch <- function(ref, file)
{
  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s", repository,
                 ref, directory, file)
  tryCatch(
    httr2::request(url) |>
      httr2::req_retry(max_tries = 3L, retry_on_failure = TRUE) |>
      httr2::req_perform() |>
      httr2::resp_body_raw(),
    error = function(e)
      rlang::abort(sprintf("Cannot fetch %s.", url), parent = e))
}

at_commit <- lapply(files, function(file) fetch(commit, file))
for (name in names(files)) {
  found <- tools::sha256sum(bytes = at_commit[[name]])
  if (!identical(found, sha256[[name]]))
    rlang::abort(c(
      sprintf("%s at the commit %s is not the pinned file.", files[[name]],
              commit),
      "i" = sprintf("Pinned SHA-256: %s.", sha256[[name]]),
      "x" = sprintf("SHA-256 fetched: %s.", found)))
}

list_version <- NA_character_
if (is.na(tag)) {
  rlang::warn(
    c("The pin names no tag, so two checks were skipped:",
      "*" = "that the tag's bytes of both files are the commit's;",
      "*" = "that the list's VERSION at the commit names the tag's version.",
      "i" = "The bundle records neither a tag nor a version."),
    class = "neoipcr_sysdata_untagged")
} else {
  ref <- paste0("refs/tags/", tag)
  for (name in names(files))
    if (!identical(fetch(ref, files[[name]]), at_commit[[name]]))
      rlang::abort(sprintf("%s at the tag %s is not the file at the commit %s.",
                           files[[name]], tag, commit))
  list_version <- sub("^infectious-agents-v", "", tag)
  listed <- trimws(rawToChar(fetch(commit, "VERSION")))
  if (!identical(listed, list_version))
    rlang::abort(sprintf(
      "The list's VERSION at the commit %s reads '%s', not the version %s of the tag %s.",
      commit, listed, list_version, tag))
}

read_yaml_bytes <- function(bytes, file)
{
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text))
    rlang::abort(sprintf("%s is not UTF-8 text.", file))
  yaml12::parse_yaml(text, simplify = FALSE)
}
tree <- read_yaml_bytes(at_commit$list, files[["list"]])
classification <- read_yaml_bytes(at_commit$reporting_classification,
                                  files[["reporting_classification"]])

ontology <- new.env()
sys.source(file.path("R", "infectious-agent-ontology.R"), envir = ontology,
           keep.source = FALSE)
tables <- ontology$derive_infectious_agent_tables(tree, classification)
ontology$assert_infectious_agent_contract(tables$classification,
                                          tables$reporting_groups)

bundle_path <- file.path("R", "sysdata.rda")
bundled <- new.env()
if (file.exists(bundle_path))
  load(bundle_path, envir = bundled)
ontology$compare_infectious_agent_classifications(
  get0("internal_infectious_agent_classification", envir = bundled,
       inherits = FALSE),
  tables$classification, acknowledged_removed_ids)

digest <- function(columns)
  tools::sha256sum(bytes = charToRaw(ontology$infectious_agent_digest_text(
    tables$classification, tables$reporting_groups, columns)))
bundle <- list(
  internal_infectious_agent_classification = tables$classification,
  internal_infectious_agent_reporting_groups = tables$reporting_groups,
  internal_infectious_agent_ontology = list(
    repository = repository,
    tag = tag,
    version = list_version,
    commit = commit,
    list_sha256 = sha256[["list"]],
    reporting_classification_sha256 = sha256[["reporting_classification"]],
    classification_digest = digest(names(tables$classification)),
    tables_digest = digest(ontology$infectious_agent_tables_digest_columns)))

if (check) {
  differences <- c(
    sprintf("%s is in R/sysdata.rda but is not built here.",
            setdiff(ls(bundled), names(bundle))),
    unlist(lapply(names(bundle), function(name) {
      held <- get0(name, envir = bundled, inherits = FALSE)
      if (is.null(held))
        return(sprintf("%s is missing from R/sysdata.rda.", name))
      if (identical(held, bundle[[name]]))
        return(NULL)
      detail <- all.equal(held, bundle[[name]])
      if (isTRUE(detail))
        detail <- "in a way all.equal() does not show"
      sprintf("%s differs: %s", name,
              paste(utils::head(detail, 5L), collapse = "; "))
    })))
  if (length(differences) > 0L)
    rlang::abort(c(
      "R/sysdata.rda is not what the pinned files give.",
      stats::setNames(differences, rep("x", length(differences))),
      "i" = sprintf("Rebuild it with Rscript data-raw/sysdata.R%s.",
                    if (untagged) " --untagged" else "")))
  rlang::inform("R/sysdata.rda is what the pinned files give.")
} else {
  save(list = names(bundle), envir = list2env(bundle), file = bundle_path,
       compress = "xz", version = 3)
  rlang::inform(sprintf("Wrote %s.", bundle_path))
}
