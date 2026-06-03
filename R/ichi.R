# Regex for a full ICHI code expression, assembled once at package load from
# the grammar in the ICHI Reference Guide (WHO 2023):
#
#   expr         := intervention ("/" intervention)*
#   intervention := stem ("&" ext_group)* ("#" other_code_suffix)?
#   stem         := [A-Z][A-Z0-9]{2} "." [A-Z]{2} "." [A-Z]{2}
#   ext_group    := extension | "(" extension ("&" extension)+ ")"
#   extension    := "X" [A-Z0-9]+ ("." [A-Z0-9]+)*
#   other_code_suffix := [^/]+
#
# Reference: https://icd.who.int/dev11/Downloads/Download?fileName=ichi/ICHI_Reference_Guide.pdf
.ichi_code_pattern <- local({
  stem         <- "[A-Z][A-Z0-9]{2}\\.[A-Z]{2}\\.[A-Z]{2}"
  ext          <- "X[A-Z0-9]+(?:\\.[A-Z0-9]+)*"
  ext_group    <- sprintf("(?:%s|\\(%s(?:&%s)+\\))", ext, ext, ext)
  ext_clause   <- sprintf("(?:&%s)*", ext_group)
  other_suffix <- "(?:#[^/]+)?"
  intervention <- sprintf("%s%s%s", stem, ext_clause, other_suffix)
  sprintf("^%s(?:/%s)*$", intervention, intervention)
})

#' Validate the syntax of an ICHI code
#'
#' Checks whether a character vector of codes conforms to the syntactic
#' grammar defined in the ICHI Reference Guide (WHO 2023). The check is
#' purely structural: it does not verify that a code exists in the
#' classification, only that it is well-formed. A complete classification
#' membership check requires the full ICHI ontology, which cannot currently
#' be bundled with neoipcr (see `tasks/ichi-classification-bundling.md` in
#' the workspace repo).
#'
#' Accepts:
#' \itemize{
#'   \item Bare stem codes: `TTT.AA.MM` (3+2+2 alphanumeric characters
#'     separated by dots, Target's first character always a letter).
#'   \item Stem codes with extensions: `TTT.AA.MM&EXT` or
#'     `TTT.AA.MM&EXT1&EXT2`. Extensions start with `X` and are
#'     alphanumeric with optional dot-separated suffixes
#'     (e.g. `XK9K`, `XP305.01`).
#'   \item Grouped extensions: `TTT.AA.MM&(EXT1&EXT2)&EXT3`.
#'   \item Interventions performed together: `STEM1/STEM2` (forward slash
#'     between interventions).
#'   \item Other-code-list suffixes: `TTT.AA.MM#ICF d6402`,
#'     `TTT.AA.MM#ISIC Group 421`, `TTT.AA.MM#CPC Sub-class 34661`.
#' }
#'
#' A small number of domain-specific ICHI Target codes carry a digit in
#' positions 2 or 3 (e.g. `AS1.AC.ZZ`); the validator accepts alphanumeric
#' at those positions to cover them. Action and Means axes are uppercase
#' letters only.
#'
#' Reference: ICHI Reference Guide, WHO 2023.
#' \url{https://icd.who.int/dev11/Downloads/Download?fileName=ichi/ICHI_Reference_Guide.pdf}
#'
#' @param code Character vector of codes to validate. `NA_character_`
#'   preserves as `NA` in the output.
#' @returns Logical vector of the same length as `code`. `TRUE` for valid
#'   codes, `FALSE` for malformed codes, `NA` for `NA` inputs.
#' @examples
#' is_valid_ichi_code(c("KCF.JK.AA", "AAA.FA.AE", "MMD.ML.AA&XK9K&XG3J07"))
#' is_valid_ichi_code(c("KBA.JJ.AA/KBF.LI.AA", "HIA.LI.AA&(XK8G&XA2N78)&XA4YJ3"))
#' is_valid_ichi_code(c("kcf.jk.aa", "KCF.JK", "KCF.JK.AAA", "not-an-ichi-code"))
#' is_valid_ichi_code(c("KCF.JK.AA", NA_character_, ""))
#' @export
is_valid_ichi_code <- function(code) {
  result <- grepl(.ichi_code_pattern, code, perl = TRUE)
  result[is.na(code)] <- NA
  result
}
