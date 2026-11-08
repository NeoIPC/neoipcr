#' The codes of the NeoIPC infectious-agent list
#'
#' The codes by which neoipcr classifies infectious-agent findings, those of
#' the NeoIPC infectious-agent list, each with its name and the concept it
#' names. A code names a concept of the list, or a synonym of one, which stands
#' for its concept; a finding's `pathogen_key` holds such a code. Code 0, *Not
#' listed*, records an infectious agent the list lacks, and code 3561,
#' *Unidentifiable*, one the laboratory could not identify.
#'
#' The codes come from the release of the list that
#' [neoipcr_supported_versions()] gives as `infectious_agents`, and their names
#' are its English ones. Another release may give a code another concept, as
#' where a renamed infectious agent keeps its code under its old name, as a
#' synonym of the concept under its new name, so findings are matched by code,
#' not by name.
#'
#' @param ids The codes to look up, as whole numbers from 0; `NULL`, the
#'   default, for every code of the list. Missing values are ignored, and a code
#'   given more than once is returned once.
#'
#' @returns A tibble with one row per code: `id` (integer), the code; `name`
#'   (character), its name; and `concept_id` (integer) and `concept_name`
#'   (character), the code and the name of the concept it names, its own for a
#'   concept. The rows follow the order of `ids`, or ascend by code where `ids`
#'   is `NULL`. A code the list lacks has no row and raises a warning of class
#'   `neoipcr_unknown_infectious_agent_code`, whose field `codes` holds every
#'   such code; `ids` that are no codes are an error of class
#'   `neoipcr_invalid_infectious_agent_ids`.
#' @export
get_infectious_agents <- function(ids = NULL)
{
  agents <- internal_infectious_agent_classification
  if (!is.null(ids)) {
    codes <- infectious_agent_codes(ids)
    rows <- match(codes, agents$input_id)
    unknown <- codes[is.na(rows)]
    if (length(unknown) > 0L)
      rlang::warn(
        c(sprintf(ngettext(length(unknown),
                           "%d code is not in the NeoIPC infectious-agent list, so no row is returned for it.",
                           "%d codes are not in the NeoIPC infectious-agent list, so no row is returned for them."),
                  length(unknown)),
          "x" = gettextf("Not in the list: %s.",
                         paste(unknown, collapse = ", "))),
        class = "neoipcr_unknown_infectious_agent_code",
        codes = unknown)
    agents <- agents[rows[!is.na(rows)], ]
  }
  tibble::tibble(
    id = agents$input_id,
    name = agents$input_name,
    concept_id = agents$output_id,
    concept_name = agents$output_name)
}

# The distinct codes in `ids`, in their order, without the missing values;
# anything but whole numbers from 0 to the largest integer is refused.
infectious_agent_codes <- function(ids, arg = rlang::caller_arg(ids),
                                   call = rlang::caller_env())
{
  missing_only <- is.logical(ids) && is.null(dim(ids)) && all(is.na(ids))
  if (!missing_only && !rlang::is_bare_numeric(ids))
    rlang::abort(
      c(gettextf("%s must hold infectious-agent codes, whole numbers from 0.",
                 paste0("`", arg, "`")),
        "x" = gettextf("It is %s.", obj_type_friendly(ids))),
      class = "neoipcr_invalid_infectious_agent_ids", call = call)
  given <- as.vector(ids[!is.na(ids)], mode = "double")
  invalid <- !is.finite(given) | given != trunc(given) | given < 0 |
    given > .Machine$integer.max
  if (any(invalid))
    rlang::abort(
      c(gettextf("%s must hold infectious-agent codes, whole numbers from 0.",
                 paste0("`", arg, "`")),
        "x" = gettextf("Not a code: %s.",
                       paste(unique(given[invalid]), collapse = ", "))),
      class = "neoipcr_invalid_infectious_agent_ids", call = call)
  unique(as.integer(given))
}
