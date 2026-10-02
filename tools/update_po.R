# Builds the package's R message catalogue, in place of tools::update_pkg_po().
#
#   Rscript tools/update_po.R
#     extracts po/R-<package>.pot from R/, merges it into each po/R-<lang>.po
#     with msgmerge, and compiles each into
#     inst/po/<lang>/LC_MESSAGES/R-<package>.mo with msgfmt; both are GNU
#     gettext tools and must be on the PATH.
#   Rscript tools/update_po.R --template <file>
#     extracts the template into <file> and touches nothing under po/ or
#     inst/po, so the R-CMD-check workflow can compare it with the committed
#     template.

update_po <- function(dir = ".", verbose = FALSE, template = NULL) {
  # Each failure is reported as it happens, between the tools' own output,
  # rather than collected at the end.
  old_options <- options(warn = 1)
  on.exit(options(old_options), add = TRUE)

  extract_gettext2pot <- function(pot_file, package, copyright, encoding,
                                  version, bugs, dir = ".", verbose = FALSE)
  {
    extract_gettext <- function(dir = ".", verbose = FALSE)
    {
      # The argument expressions of the call whose parse id is `call_id`, each
      # with the name it is passed under, "" for a positional one.
      call_arguments <- function(x, call_id)
      {
        children <- x |>
          dplyr::filter(.data$parent == call_id) |>
          dplyr::arrange(.data$line1, .data$col1)
        args <- tibble::tibble(name = character(), id = integer())
        in_args <- FALSE
        name <- ""
        for (i in seq_len(nrow(children))) {
          token <- children$token[i]
          if (token == "'('")
            in_args <- TRUE
          else if (token == "SYMBOL_SUB")
            name <- children$text[i]
          else if (token == "','")
            name <- ""
          else if (in_args && token == "expr")
            args <- tibble::add_row(args, name = name, id = children$id[i])
        }
        args
      }

      # gettext() looks a message up without its leading and trailing blanks,
      # tabs, and newlines, and tools::xgettext() strips them the same way.
      trim_msgid <- function(s) sub("[ \t\n]*$", "", sub("^[ \t\n]*", "", s))

      # Collects the direct literal arguments of every gettext() and gettextf()
      # call, namespace-qualified ones included: all of gettext()'s but
      # `domain`, and only gettextf()'s format string, whose other arguments
      # are values, not messages. The comparison with tools::xgettext() below
      # warns wherever the two extractors still differ.
      find_gettext_strings <- function(f)
      {
        e <- parse(file = f, keep.source = TRUE)
        x <- utils::getParseData(e)
        calls <- x |>
          dplyr::filter(
            token == "SYMBOL_FUNCTION_CALL" &
              text %in% c("gettext","gettextf")) |>
          dplyr::select(fun = "text", p1 = "parent") |>
          dplyr::inner_join(x, dplyr::join_by("p1" == "id")) |>
          dplyr::select("fun", call_id = "parent")
        message_args <- tibble::tibble(id = integer())
        for (i in seq_len(nrow(calls))) {
          args <- call_arguments(x, calls$call_id[i])
          if (calls$fun[i] == "gettextf") {
            fmt <- dplyr::filter(args, .data$name == "fmt")
            if (nrow(fmt) == 0L)
              fmt <- dplyr::slice_head(dplyr::filter(args, .data$name == ""), n = 1L)
            args <- fmt
          } else
            args <- dplyr::filter(args, .data$name != "domain")
          message_args <- dplyr::bind_rows(message_args, dplyr::select(args, "id"))
        }
        message_args |>
          dplyr::inner_join(x, dplyr::join_by("id" == "parent")) |>
          dplyr::filter(token == "STR_CONST") |>
          dplyr::mutate(
            reference = paste0("#: ", sub(paste0("^", stringr::str_escape(tools::file_path_as_absolute(".")), "/?"), "", tools::file_path_as_absolute(f)), ":", line1),
            msgid = trim_msgid(as.character(sapply(.data$text, \(x) eval(parse(text=x))))),
            .keep = "none")
      }

      r_dir <- tools::file_path_as_absolute(dir) |> file.path("R")
      r_exts <- tools:::.make_file_exts("code")
      r_files <- tools::list_files_with_exts(r_dir, r_exts)
      for (os in c("unix", "windows")) {
        os_subdir <- file.path(r_dir, os)
        if (dir.exists(os_subdir))
          r_files <- c(r_files, tools::list_files_with_exts(os_subdir, r_exts))
      }
      out <- tibble::tibble()
      for (f in r_files) {
        if (verbose)
          message("Parsing ", sQuote(f), domain = NA)
        out <- out |>
          dplyr::bind_rows(find_gettext_strings(f))
      }
      out |>
        dplyr::filter(msgid != "") |>
        dplyr::mutate(msgid = ordered(.data$msgid, levels = unique(.data$msgid))) |>
        dplyr::group_by(msgid) |>
        dplyr::summarise(references = list(reference)) |>
        dplyr::arrange(msgid) |>
        dplyr::mutate(msgid = as.character(.data$msgid))
    }

    # Get the msgids via tools::xgettext and compare to ensure compatibility
    msgids <- unique(unlist(tools::xgettext(dir, asCall = FALSE)))
    msgids <- msgids[nzchar(msgids)]
    msgids_info <- extract_gettext(dir, verbose = verbose)
    msgids2 <- msgids_info |> dplyr::pull(msgid)
    if(length(msgids) != length(msgids2) || !all(msgids == msgids2))
      rlang::warn("The generated msgids differ from the ones generated by tools::xgettext")
    if(nrow(msgids_info) > 0)
      msgids_info <- msgids_info |> dplyr::mutate(msgid = shQuote(encodeString(.data$msgid), type = "cmd"))
    msgids_plural <- tools::xngettext(dir)
    msgids_plural_uniqe <- unique(unlist(msgids_plural))
    # Binary, not "wt". A text-mode connection translates LF to CRLF on Windows, and every
    # writeLines() below goes through this one connection — so "wt" would emit a CRLF .pot on
    # Windows and an LF one elsewhere. A gettext catalogue is rewritten in turn by msgmerge,
    # po4a and Weblate, all of which write LF; a partial rewrite by a tool that disagrees is
    # what produces a mixed file no parser accepts. writeLines' default sep = "\n" is written
    # literally in binary mode, which is what makes the output platform-independent.
    pot_con <- file(pot_file, "wb")
    on.exit(close(pot_con))
    now <- Sys.time()
    writeLines(
      con = pot_con,
      c(
        sprintf('# %s Translation Template File.', package),
        sprintf('# Copyright (C) %s %s', format(now, "%Y"), copyright),
        sprintf('# This file is distributed under the same license as the %s package.', package),
        '#',
        'msgid ""',
        'msgstr ""',
        sprintf('"Project-Id-Version: %s %s\\n"', package, version),
        sprintf('"Report-Msgid-Bugs-To: %s\\n"', bugs),
        paste0('"POT-Creation-Date: ', format(now, "%Y-%m-%d %H:%M%z"),'\\n"'),
        '"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"',
        '"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"',
        '"Language-Team: LANGUAGE <LL@li.org>\\n"',
        '"Language: \\n"',
        '"MIME-Version: 1.0\\n"',
        sprintf('"Content-Type: text/plain; charset=%s\\n"', encoding),
        '"Content-Transfer-Encoding: 8bit\\n"',
        if (length(msgids_plural_uniqe) > 0) '"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"'
      )
    )
    for (i in seq_len(nrow(msgids_info))) {
      msgid <- msgids_info[i,1]
      references <- unlist(msgids_info[i,2])
      writeLines(con = pot_con, c("", references, paste("msgid", msgid), 'msgstr ""'))
    }
    for (msgid_plural in msgids_plural)
      for (p in msgid_plural)
        if (p[1L] %in% msgids_plural_uniqe)
        {
          writeLines(
            con = pot_con,
            c(
              "",
              paste(
                "msgid       ",
                shQuote(encodeString(p[1L]), type = "cmd")
              ),
              paste(
                "msgid_plural",
                shQuote(encodeString(p[2L]), type = "cmd")
              ),
              'msgstr[0]    ""',
              'msgstr[1]    ""'
            )
          )
          msgids_plural_uniqe <- msgids_plural_uniqe[msgids_plural_uniqe != p[1L]]
        }
  }

  # Resolved before setwd(dir), since a relative path names a file relative to
  # the caller's working directory.
  if (!is.null(template))
    template <- file.path(normalizePath(dirname(template), winslash = "/", mustWork = TRUE),
                          basename(template))
  wd_bkp <- getwd()
  on.exit(setwd(wd_bkp), add = TRUE)
  setwd(dir)
  collation_bkp <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", collation_bkp), add = TRUE)
  Sys.setlocale("LC_COLLATE", "C")
  if (is.null(template))
    dir.create("po", FALSE)
  po_files <- list.files(path = "po", pattern = "^R-.+\\.pot?$",
                         full.names = TRUE)
  description_info <- read.dcf(
    "DESCRIPTION",
    fields = c("Package", "Version", "BugReports", "Authors@R", "Encoding"))
  package_name <- description_info[1L]
  version <- description_info[2L]
  bugs <- description_info[3L]
  copyright <- eval(parse(text=description_info[4L])) |>
    format(
      include = c("given","family","role"),
      braces = list(
        given=c('list(value="',''),
        role=c('", roles=c("','"))')),
      collapse = list(
        role='","')) |>
    sapply(\(x){
      item <- eval(parse(text=x));
      if("cph" %in% item$roles)
        item$value
      },
      USE.NAMES = FALSE) |>
    unlist() |>
    trimws()
  encoding <- description_info[5L]

  if (!is.null(template)) {
    if (verbose)
      message("Creating pot: ", sQuote(template), domain = NA)
    extract_gettext2pot(pot_file = template, package = package_name, copyright,
                        encoding, version, bugs, dir = ".", verbose = verbose)
    return(invisible())
  }

  po_inst_dir <- file.path("inst", "po")
  tmp_file <- tempfile(fileext = "pot")
  on.exit(file.remove(tmp_file), add = TRUE, after = FALSE)
  if (verbose)
    message("Creating pot: .. ", domain = NA)

  extract_gettext2pot(pot_file = tmp_file, package = package_name, copyright, encoding,
                      version, bugs, dir = ".", verbose = verbose)

  pot_file <- file.path("po", paste0("R-", package_name, ".pot"))
  if (verbose)
    message("Copying to potfile ", sQuote(pot_file), domain = NA)
  file.copy(tmp_file, pot_file, overwrite = TRUE)
  po_files <- po_files[!(po_files %in% c("po/R-en@quot.po", paste0("po/R-", package_name, ".pot")))]
  for (po_file in po_files) {
    language <- sub("^R-(.*)\\.po$", "\\1", basename(po_file))
    command <- "msgmerge"
    args <- c("--update", shQuote(po_file), shQuote(pot_file))
    has_warning <- FALSE
    warning_handler <- function(w) {
      environment(warning_handler)$has_warning <- TRUE
      tryInvokeRestart("muffleWarning")
    }

    if (verbose)
      message("Running command ", paste0(c(command, args), collapse = " "), appendLF = FALSE, domain = NA)
    else
      message(paste(command, language), appendLF = FALSE, domain = NA)

    result <- withCallingHandlers(system2(command, args, stderr = TRUE), warning = warning_handler)

    if (has_warning)
    {
      message(":\n  ", result, domain = NA)
      rlang::warn(paste0("Running ", command, " on ", sQuote(po_file), " failed"))
      next
    }
    else message(" ", result, domain = NA)

    po_check <- tools::checkPoFile(po_file, TRUE)
    if (nrow(po_check)) {
      print(po_check)
      message("not installing", domain = NA)
      next
    }
    lang_inst_dir <- file.path(po_inst_dir, language, "LC_MESSAGES")
    dir.create(lang_inst_dir, FALSE, TRUE)
    mo_file <- file.path(lang_inst_dir, sprintf("R-%s.mo", package_name))

    has_warning <- FALSE
    command <- "msgfmt"
    args <- c("-c", "--statistics", "-o", shQuote(mo_file), shQuote(po_file))

    if (verbose)
      message("Running command ", paste0(c(command, args), collapse = " "), appendLF = FALSE, domain = NA)
    else
      message(paste(command, language), appendLF = FALSE, domain = NA)

    result <- withCallingHandlers(system2(command, args, stderr = TRUE), warning = warning_handler)

    if (has_warning)
    {
      message("\n  ", result, domain = NA)
      rlang::warn(paste0("Running ", command, " on ", sQuote(po_file), " failed"))
      next
    }
    else message(" ... done.\n  ", result, domain = NA)
  }

  invisible()
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  update_po()
} else if (length(args) == 2L && args[1L] == "--template") {
  update_po(template = args[2L])
} else
  rlang::abort(c(
    "Unrecognized arguments.",
    i = "Usage: Rscript tools/update_po.R [--template <file>]"))
