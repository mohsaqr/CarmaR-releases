# publish.R - one call to make a Quarto project's R chunks runnable, and one
# call to check that the rendered site actually carries what it needs.
#
# The manual recipe (copy the extension into the project, add the filter to
# _quarto.yml, render) is three steps of things authors mistype - and the
# extension's JS and CSS must travel together, which a hand copy gets wrong
# exactly once. The package already reaches every author's machine, so the
# package carries the extension and this verb does the copying and the YAML.

#' Make a Quarto project's R chunks runnable with CarmaR
#'
#' Copies the CarmaR 'Quarto' extension into `project/_extensions/carmar`,
#' enables the `carmar` filter in the project's `_quarto.yml`, and writes the
#' filter's options block (`carmar: port:` and `label:`) when the file has
#' none, so the two things an author may want to change are visible. After
#' the next `quarto render`, every visible R chunk on the published pages
#' gains a Run control against each reader's own local R (the reader starts
#' it with [listen()]).
#'
#' @section How a published page reaches R:
#' The filter rewrites nothing: the page still renders identically for a
#' reader without R. It adds `<meta name="carmar-port">` and
#' `<meta name="carmar-label">` to the page head and a script and stylesheet
#' pair under `site_libs/`. On the reader's machine the script finds the
#' page's R chunks and dials a CarmaR kernel on the page's port over a
#' loopback WebSocket. The kernel does not trust a site because it asked: the
#' reader approves the exact origin once per kernel, either in advance
#' (`listen("https://book.example")`) or on a local consent page the first
#' Run opens. Code that should be shown but never offered to run goes in a
#' fenced div with the class `carmar-no-run`. A site with its own Content
#' Security Policy
#' must allow `connect-src ws://127.0.0.1:4747` (or the port chosen). This is
#' execution, not a sandbox: what a reader runs has the same access as their
#' own R console, so keep the code visible.
#'
#' Idempotent: running it again refreshes the extension files (JS and CSS
#' together - the pair must never drift apart) and leaves an already-enabled
#' `_quarto.yml` untouched. A `_quarto.yml` it cannot edit safely (an inline
#' `filters: [...]` list, or several top-level `filters:` keys) is left
#' exactly as it was, and the lines to add are printed instead.
#'
#' @param project Path to the 'Quarto' project (the directory holding
#'   `_quarto.yml`, or where one should be created). Default: the working
#'   directory.
#' @param extension_source Directory holding the extension files. The default
#'   is the copy shipped inside the installed carmar package; a development
#'   checkout can pass its own `_extensions/carmar` instead.
#' @return Invisibly, a one-row `data.frame` of class `carmar_publishing`:
#'   `project`, `extension` (the destination directory), `config` (the
#'   `_quarto.yml` path), `enabled` (logical - is the filter active in that
#'   file), `options` (logical - does the file carry a `carmar:` options
#'   block). Its print method says what happened and what to do next.
#' @examples
#' project <- tempfile("book")
#' dir.create(project)
#' writeLines(c("project:", "  type: book"), file.path(project, "_quarto.yml"))
#' carmar::use_publishing(project)
#' cat(readLines(file.path(project, "_quarto.yml")), sep = "\n")
#' unlink(project, recursive = TRUE)
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
      "Reinstall the package, or pass `extension_source`."),
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
  options <- enabled && write_carmar_options(yml)
  if (enabled) {
    message("Filter enabled in ", yml, " - render the project (quarto render) and publish as usual.")
  }
  out <- data.frame(project = project, extension = dest, config = yml,
                    enabled = enabled, options = options, stringsAsFactors = FALSE)
  class(out) <- c("carmar_publishing", "data.frame")
  invisible(out)
}

#' @rdname use_publishing
#' @param x A `carmar_publishing`, as returned by [use_publishing()].
#' @param ... Ignored.
#' @return `print()` returns `x` invisibly.
#' @export
print.carmar_publishing <- function(x, ...) {
  cat(if (isTRUE(x$enabled[[1L]])) {
    paste0("CarmaR publishing is enabled in ", x$config[[1L]],
           " - run `quarto render`, publish, then carmar::check_publishing().")
  } else {
    paste0("The extension is in ", x$extension[[1L]], " but ", x$config[[1L]],
           " was left for you to edit - add `- carmar` under `filters:`.")
  }, "\n", sep = "")
  invisible(x)
}

#' Check a rendered site for the CarmaR publishing pieces
#'
#' Reads every rendered `*.html` page under `site` and reports, per page,
#' whether the `carmar` filter ran (the page carries the port and label meta
#' tags) and whether the runtime assets it references - the
#' `carmar-published-*/carmar-publish.js` script and its `.css` - exist
#' beside the page. A page with `port` `NA` was rendered without the filter;
#' a page with `assets` `FALSE` will render but its Run controls will never
#' appear, which is what happens when a site is published without its
#' `site_libs` directory.
#'
#' @param site Path to the rendered output directory (default `"_site"`;
#'   a 'Quarto' book renders to `_book`).
#' @return A `data.frame`, one row per HTML page, paths ascending: `file`
#'   (relative to `site`), `port` (integer, `NA` without the filter), `label`
#'   (the Run button's text, `NA` without the filter), `assets` (logical -
#'   both runtime files referenced by the page exist). Zero rows when the
#'   directory holds no HTML. An error of class `carmar_no_site` when `site`
#'   does not exist.
#' @examples
#' site <- tempfile("site")
#' dir.create(file.path(site, "site_libs", "carmar-published-0.1.0"), recursive = TRUE)
#' file.create(file.path(site, "site_libs", "carmar-published-0.1.0",
#'                       c("carmar-publish.js", "carmar-publish.css")))
#' writeLines(c('<meta name="carmar-port" content="4747">',
#'              '<meta name="carmar-label" content="Run on my computer">',
#'              '<script src="site_libs/carmar-published-0.1.0/carmar-publish.js"></script>',
#'              '<link href="site_libs/carmar-published-0.1.0/carmar-publish.css" rel="stylesheet">'),
#'            file.path(site, "index.html"))
#' writeLines("<p>rendered without the filter</p>", file.path(site, "about.html"))
#' carmar::check_publishing(site)
#' unlink(site, recursive = TRUE)
#' @export
check_publishing <- function(site = "_site") {
  stopifnot("`site` must be a single path" =
              is.character(site) && length(site) == 1L && !is.na(site))
  if (!dir.exists(site)) {
    stop(errorCondition(paste0(
      "No rendered site at ", site, ". Render the project first (quarto render), ",
      "or pass the output directory (a book renders to _book)."),
      class = "carmar_no_site", call = NULL))
  }
  pages <- sort(list.files(site, pattern = "\\.html?$", recursive = TRUE))
  rows <- lapply(pages, function(rel) page_publishing(site, rel))
  out <- do.call(rbind, c(rows, list(
    data.frame(file = character(0), port = integer(0), label = character(0),
               assets = logical(0), stringsAsFactors = FALSE))))
  rownames(out) <- NULL
  out
}

#' One page's row for check_publishing().
#' @noRd
page_publishing <- function(site, rel) {
  html <- paste(readLines(file.path(site, rel), warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  port <- meta_content(html, "carmar-port")
  label <- meta_content(html, "carmar-label")
  port <- if (is.na(port)) NA_integer_ else suppressWarnings(as.integer(port))
  js <- asset_ref(html, "src", "carmar-publish.js")
  css <- asset_ref(html, "href", "carmar-publish.css")
  here <- dirname(file.path(site, rel))
  assets <- !is.na(js) && !is.na(css) &&
    file.exists(file.path(here, js)) && file.exists(file.path(here, css))
  data.frame(file = rel, port = port, label = label, assets = assets,
             stringsAsFactors = FALSE)
}

#' The content of `<meta name="...">`, or NA.
#' @noRd
meta_content <- function(html, name) {
  pat <- paste0("<meta\\s+name=\"", name, "\"\\s+content=\"([^\"]*)\"")
  hit <- regmatches(html, regexec(pat, html))[[1L]]
  if (length(hit) < 2L) NA_character_ else html_unescape(hit[[2L]])
}

#' The `src`/`href` of the tag referencing `file` inside a `carmar-published-*`
#' directory, or NA.
#' @noRd
asset_ref <- function(html, attr, file) {
  pat <- paste0(attr, "=\"([^\"]*carmar-published-[^\"/]*/", file, ")\"")
  hit <- regmatches(html, regexec(pat, html))[[1L]]
  if (length(hit) < 2L) NA_character_ else hit[[2L]]
}

#' Undo the five escapes the filter applies to the label.
#' @noRd
html_unescape <- function(x) {
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

#' Put `- carmar` into a project's top-level `filters:` block.
#'
#' Text surgery, not a YAML round-trip: re-serialising an author's
#' `_quarto.yml` would reorder keys and strip comments, which is a worse
#' outcome than declining. The cases handled are exactly the ones that can be
#' edited without risk; anything else is refused with the lines to add.
#' Returns `TRUE` when the file now enables the filter, `FALSE` when it was
#' left untouched for the author.
#' @noRd
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
            "  filters:\n    - carmar\n  carmar:\n    port: 4747\n",
            "    label: Run on my computer")
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
  # carmar goes LAST - an author's existing filters keep their order and
  # carmar appends.
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

#' Append the filter's options block when the file has no top-level `carmar:`
#' key. A new top-level key at the end of the file is always safe YAML; an
#' existing block, however it is written, is the author's and is not touched.
#' Returns `TRUE` when the file now carries a `carmar:` key.
#' @noRd
write_carmar_options <- function(yml) {
  lines <- readLines(yml, warn = FALSE)
  if (any(grepl("^carmar:", lines))) return(TRUE)
  writeLines(c(lines, "",
               "# CarmaR: the loopback port the reader's kernel listens on, and the",
               "# text of the Run button. Readers start the kernel with carmar::listen().",
               "carmar:",
               "  port: 4747",
               "  label: Run on my computer"), yml)
  TRUE
}

#' Make already rendered HTML pages runnable
#'
#' A page that was rendered long ago - an R Markdown report, a bookdown
#' `_book/`, a pkgdown article, any HTML that shows R code in `<pre class="r">`
#' or `<code class="sourceCode r">` blocks - becomes runnable without being
#' rendered again: one `<script>` tag before `</head>` loads the CarmaR
#' runtime, which gives every R block a Run button and a strip at the top of
#' the page that connects to the reader's own local R (see `listen()`).
#'
#' The tag is inserted into each file in place, once: a file that already
#' loads `carmar-publish.js` is left as it is. The runtime links its own
#' stylesheet, so the tag is all a page needs.
#'
#' @param path an `.html` file, or a directory whose `.html` files (searched
#'   recursively) are all stamped - a bookdown `_book/`, say.
#' @param assets where the page loads the runtime from: a URL prefix ending in
#'   `/` (default: CarmaR's published copy), or `"local"` to copy the two
#'   runtime files from this package into a `carmar/` folder beside the pages
#'   and reference them relatively - for a site that must not load anything
#'   from elsewhere.
#' @param port the loopback port the page dials (the fixed 4747 unless the
#'   reader is told otherwise); written as `<meta name="carmar-port">`.
#' @param label the strip's button; written as `<meta name="carmar-label">`.
#' @return Invisibly, a `data.frame` of class `carmar_runnable`, one row per
#'   HTML file, paths ascending: `file` (character), `stamped` (logical:
#'   `TRUE` when the tag was inserted now, `FALSE` when it was already there),
#'   `assets` (character: the `src` the page loads). Raises a classed error
#'   `carmar_no_html` when `path` names no HTML file, and `carmar_no_extension`
#'   when `assets = "local"` and this installation carries no runtime.
#' @examples
#' dir <- file.path(tempdir(), "report")
#' dir.create(dir, showWarnings = FALSE)
#' writeLines(c("<html><head><title>t</title></head>",
#'              "<body><pre class=\"r\"><code>1 + 1</code></pre></body></html>"),
#'            file.path(dir, "index.html"))
#' make_runnable(dir)
#' make_runnable(dir)            # a second call changes nothing
#' @export
make_runnable <- function(path, assets = "https://lacarm.com/carmar/publish/",
                          port = 4747, label = "Run on my computer") {
  stopifnot(
    "`path` must be one file or directory" = is.character(path) && length(path) == 1L && nzchar(path),
    "`assets` must be a URL prefix ending in / or \"local\"" =
      is.character(assets) && length(assets) == 1L && (identical(assets, "local") || grepl("/$", assets)),
    "`port` must be one port number" = is.numeric(port) && length(port) == 1L && port >= 1 && port <= 65535,
    "`label` must be one string" = is.character(label) && length(label) == 1L && nzchar(label)
  )
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.html?$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  } else if (file.exists(path) && grepl("\\.html?$", path, ignore.case = TRUE)) path else character()
  if (!length(files)) {
    stop(errorCondition(paste0("no HTML file at ", path),
                        class = "carmar_no_html", call = NULL))
  }
  files <- sort(normalizePath(files, winslash = "/"))
  root <- if (dir.exists(path)) normalizePath(path, winslash = "/") else dirname(files[[1L]])
  local_dir <- if (identical(assets, "local")) install_runtime_assets(root) else NULL
  rows <- lapply(files, function(f) {
    src <- if (is.null(local_dir)) paste0(assets, "carmar-publish.js")
           else paste0(relative_path(dirname(f), local_dir), "/carmar-publish.js")
    data.frame(file = f, stamped = stamp_runnable(f, src, port, label), assets = src,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("carmar_runnable", "data.frame")
  invisible(out)
}

#' @rdname make_runnable
#' @param x a `carmar_runnable` result.
#' @param ... ignored.
#' @return `print()` returns `x` invisibly.
#' @export
print.carmar_runnable <- function(x, ...) {
  n <- sum(x$stamped)
  cat(sprintf("%d HTML page%s made runnable (%d already %s); the runtime loads from %s\n",
              n, if (n == 1L) "" else "s", nrow(x) - n, if (nrow(x) - n == 1L) "was" else "were",
              x$assets[[1L]]))
  print.data.frame(x, ...)
  invisible(x)
}

#' Insert the CarmaR tags before </head> once. Returns TRUE when it wrote.
#' @noRd
stamp_runnable <- function(file, src, port, label) {
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("carmar-publish\\.js", html, fixed = FALSE)) return(FALSE)
  tags <- paste0(
    '<meta name="carmar-port" content="', as.integer(port), '">',
    '<meta name="carmar-label" content="', html_escape_attr(label), '">',
    '<script defer src="', html_escape_attr(src), '"></script>')
  head_end <- regexpr("</head\\s*>", html, ignore.case = TRUE)
  out <- if (head_end > 0) {
    paste0(substr(html, 1L, head_end - 1L), tags, substr(html, head_end, nchar(html)))
  } else {
    # No <head>: put the tags first; a browser hoists them into the head.
    paste0(tags, "\n", html)
  }
  writeLines(out, file, useBytes = TRUE)
  TRUE
}

#' The two runtime files copied beside the pages, into `<root>/carmar/`.
#' @noRd
install_runtime_assets <- function(root) {
  ext <- system.file("quarto", "_extensions", "carmar", package = "carmar")
  needed <- c("carmar-publish.js", "carmar-publish.css")
  if (!nzchar(ext) || !all(file.exists(file.path(ext, needed)))) {
    stop(errorCondition(
      "This carmar installation carries no runtime to copy; use a URL for `assets`.",
      class = "carmar_no_extension", call = NULL))
  }
  dest <- file.path(root, "carmar")
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  ok <- file.copy(file.path(ext, needed), dest, overwrite = TRUE)
  if (!all(ok)) {
    stop(errorCondition(paste("could not copy the runtime into", dest),
                        class = "carmar_copy_failed", call = NULL))
  }
  normalizePath(dest, winslash = "/")
}

#' `to` relative to `from` (both directories, normalised, forward slashes).
#' @noRd
relative_path <- function(from, to) {
  a <- strsplit(from, "/", fixed = TRUE)[[1L]]
  b <- strsplit(to, "/", fixed = TRUE)[[1L]]
  common <- 0L
  while (common < min(length(a), length(b)) && identical(a[[common + 1L]], b[[common + 1L]])) {
    common <- common + 1L   # a prefix walk is a loop by nature
  }
  up <- rep("..", length(a) - common)
  down <- b[seq_len(length(b) - common) + common]
  rel <- paste(c(up, down), collapse = "/")
  if (nzchar(rel)) rel else "."
}

#' @noRd
html_escape_attr <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
