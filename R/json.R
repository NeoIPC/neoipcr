#' Write a neoipcr object as plain JSON
#'
#' Writes plain, non-R-specific JSON suitable for consumers in other
#' languages (e.g. the NeoIPC.Reporting .NET service, JS tooling). Unlike
#' [jsonlite::serializeJSON()] — which wraps every value in an R type
#' descriptor and only round-trips through [jsonlite::unserializeJSON()] —
#' the output is a normal JSON document.
#'
#' This is a deliberately narrow first cut. It handles plain lists,
#' scalars, character vectors, and Date/POSIXct values well, which
#' covers the `metadata` block produced by [calculate_reference_data()]
#' (the consumer driving this addition).
#'
#' Round-tripping the full output of [calculate_reference_data()] (with
#' tibbles, factors, and class metadata) is tracked separately. A
#' `read_json()` companion will land alongside that work.
#'
#' @param x The object to serialise.
#' @param file Output file path. If `NULL`, returns the JSON string.
#' @param pretty Whether to pretty-print the output.
#'
#' @return The JSON string. Returned invisibly when `file` is non-NULL.
#' @export
write_json <- function(x, file = NULL, pretty = FALSE) {
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    Date = "ISO8601",
    POSIXt = "ISO8601",
    pretty = pretty
  )
  if (is.null(file)) {
    return(json)
  }
  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(json, file, useBytes = TRUE)
  invisible(json)
}
