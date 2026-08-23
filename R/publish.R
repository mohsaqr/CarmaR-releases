# publish.R — one call to make a Quarto project's R chunks runnable.
#
# The manual recipe (copy _extensions/carmar into the project, add the filter
# to _quarto.yml, render) is three steps of things authors mistype — and the
# extension's JS and CSS must travel together, which a hand copy gets wrong
# exactly once. The package already reaches every author's machine, so the
# package carries the extension and this verb does the copying and the YAML.

#' Make a Quarto project's R chunks runnable with CarmaR
#'
#' Copies the CarmaR Quarto extension into `project/_extensions/carmar` and
#' enables the `carmar` filter in the project's `_quarto.yml`. After the next
#' `quarto render`, every visible R chunk on the published pages gains Run and
#' Edit controls against each reader's own local R (see
#' `docs/publishing.md` in the CarmaR repository for the trust model).
#'
#' Idempotent: running it again refreshes the extension files (JS and CSS
#' together — the pair must never drift apart) and leaves an already-enabled
#' `_quarto.yml` untouched. A `_quarto.yml` it cannot edit safely (an inline
#' `filters: [...]` list, or several top-level `filters:` keys) is left
#' exactly as it was, and the lines to add are printed instead.
#'
#' @param project Path to the Quarto project (the directory holding
#'   `_quarto.yml`, or where one should be created). Default: the working
#'   directory.
#' @param extension_source Directory holding the extension files. The default
#'   is the copy shipped inside the installed carmar package; a development
#'   checkout can pass its own `_extensions/carmar` instead.
#' @return Invisibly, a list with `extension` (the destination directory),
#'   `config` (the `_quarto.yml` path), and `enabled` (`TRUE` when the filter
#'   is active in that file, `FALSE` when the file was left for the author).
#' @examples
#' \dontrun{
#' carmar::use_publishing("~/my-book")
#' # then: quarto render
#' }
#' @export
use_publishing <- function(project = ".",
                           extension_source = system.file("quarto", "_extensions", "carmar",
                                                          package = "carmar")) {
  stopifnot(
    "`project` must be an existing directory" =
      is.character(project) && length(project) == 1L && dir.exists(project)
  )
  if (!nzchar(extension_source) || !dir.exists(extension_source)) {
    stop(errorCondition(paste(
      "This carmar installation does not carry the Quarto extension.",
      "Upgrade the package (carmar::upgrade()), or copy _extensions/carmar",
      "from the CarmaR repository into the project by hand."),
      class = "carmar_no_extension", call = NULL))
  }
  files <- list.files(extension_source)
  stopifnot(
    "the extension source must carry the filter and both runtime assets" =
      all(c("carmar.lua", "carmar-publish.js", "carmar-publish.css") %in% files)
  )

  dest <- file.path(project, "_extensions", "carmar")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(file.path(extension_source, files), file.path(dest, files),
                      overwrite = TRUE)
  if (!all(copied)) {
    stop(errorCondition(paste("could not write the extension into", dest),
                        class = "carmar_copy_failed", call = NULL))
  }
  message("Extension installed: ", dest, " (", length(files), " files)")

  yml <- file.path(project, "_quarto.yml")
  enabled <- enable_carmar_filter(yml)
  if (enabled) {
    message("Filter enabled in ", yml, " - render the project (quarto render) and publish as usual.")
  }
  invisible(list(extension = dest, config = yml, enabled = enabled))
}

#' Put `- carmar` into a project's top-level `filters:` block.
#'
#' Text surgery, not a YAML round-trip: re-serialising an author's
#' `_quarto.yml` would reorder keys and strip comments, which is a worse
#' outcome than declining. The cases handled are exactly the ones that can be
#' edited without risk; anything else is refused with the lines to add.
#'
#' @param yml Path to `_quarto.yml`; created (filters only) when absent.
#' @return `TRUE` when the file now enables the filter, `FALSE` when it was
#'   left untouched for the author.
#' @keywords internal
enable_carmar_filter <- function(yml) {
  if (!file.exists(yml)) {
    writeLines(c(
      "# Created by carmar::use_publishing() - the carmar filter makes each",
      "# rendered page's R chunks runnable against the reader's own local R.",
      "filters:",
      "  - carmar"), yml)
    return(TRUE)
  }

  lines <- readLines(yml, warn = FALSE)
  if (any(grepl("^\\s*-\\s*carmar\\s*$", lines))) {
    message("Filter already enabled in ", yml, " - nothing to change.")
    return(TRUE)
  }

  refuse <- function(reason) {
    message("Left ", yml, " untouched (", reason, "). Add by hand:\n",
            "  filters:\n    - carmar")
    FALSE
  }

  heads <- grep("^filters:", lines)
  if (length(heads) > 1L) return(refuse("several top-level filters keys"))
  if (length(heads) == 1L && !grepl("^filters:\\s*$", lines[heads])) {
    return(refuse("an inline filters list"))     # filters: [a, b]
  }

  if (!length(heads)) {
    writeLines(c(lines, "", "filters:", "  - carmar"), yml)
    return(TRUE)
  }

  # The block's items: the run of `  - item` lines right after `filters:`.
  # carmar goes LAST, matching docs/publishing.md - an author's existing
  # filters keep their order and carmar appends.
  after <- seq_along(lines) > heads
  items <- after & grepl("^\\s+-\\s", lines)
  # cumsum trick: the first non-item line after the head ends the block.
  block_end <- heads
  run <- which(after)[cumsum(!items[after]) == 0L]
  if (length(run)) block_end <- max(run)
  indent <- if (block_end > heads) {
    sub("^(\\s+)-.*$", "\\1", lines[block_end])
  } else "  "
  writeLines(append(lines, paste0(indent, "- carmar"), block_end), yml)
  TRUE
}
