#
# jobs.R — what a development task IS, decided without starting one.
#
# ── the shape, and why it is this shape ───────────────────────────────────
#
# Stage 5 slice 3 and Stage 6 item 7 ask for the same thing from two
# directions: package tasks that do not consume the interactive worker, and
# detached jobs that outlive the run that started them. Both are one model —
# a THIRD kind of child process, beside the worker (evaluates the user's
# notebook) and the analyzer (evaluates nothing at all).
#
# A job sits between them: it DOES evaluate code, which is why it may not be
# the analyzer, and it evaluates code that is not the notebook's, which is
# why it may not be the worker. `devtools::check()` runs the package's own
# tests in a fresh process; doing that in the session would mean the user's
# workspace is gone and their Stop button is a lie for four minutes.
#
# ── the one security decision in this file ────────────────────────────────
#
# THE WIRE NEVER CARRIES CODE. The browser sends a task NAME out of a fixed
# vocabulary and a directory; this file decides whether that pair is
# runnable, and the supervisor builds the argument vector. Nothing from the
# socket is ever pasted into an R expression or a shell string — task names
# are matched against a list, and the root and filter travel in the child's
# ENVIRONMENT, where they are data to `Sys.getenv()` and can never be parsed.
#
# That is the same rule spike/broker.R follows for systemctl, and it is worth
# stating as a rule rather than a habit: `exec` already lets a page run
# arbitrary R, so this is not the only door to evaluation — but it is a door
# whose vocabulary is small enough to read in one screen, and keeping it that
# way is cheaper than auditing an interpolation.
#
# Pure by construction: nothing here starts a process, writes a file, or
# reads a socket, so spike/test-jobs.R can exercise every branch without one.

#' Is this directory the root of an R package?
#'
#' The test is a DESCRIPTION carrying a `Package:` field, not merely a file
#' called DESCRIPTION — Bioconductor build dirs, some data packages and a
#' stray copied file all put that name somewhere it does not mean this.
#'
#' A DESCRIPTION that cannot be parsed answers FALSE. That is a decision, not
#' a swallowed error: "this is not a package root" is exactly the right
#' answer for a malformed file, and the caller's next step (say so, offer the
#' folder anyway) is the same either way.
#'
#' @param dir Directory to test.
#' @return TRUE when `dir` is an R package root.
is_package_root <- function(dir) {
  desc <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc) || dir.exists(desc)) return(FALSE)
  fields <- tryCatch(read.dcf(desc, fields = "Package"),
                     error = function(e) NULL,
                     warning = function(w) NULL)
  !is.null(fields) && nrow(fields) >= 1L &&
    !is.na(fields[1L, 1L]) && nzchar(trimws(fields[1L, 1L]))
}

#' Every directory from `path` up to the filesystem root.
#'
#' A parent chain cannot be vectorised — each step is computed from the last —
#' so this is a bounded `while`, and the bound is the point: a symlink loop or
#' a pathological path must not spin the supervisor's event loop.
#'
#' @param path Starting path.
#' @param max_up Most ancestors to walk.
#' @return Character vector, nearest first.
path_ancestors <- function(path, max_up = 40L) {
  out <- character(0)
  current <- path
  while (length(out) < max_up) {
    out <- c(out, current)
    parent <- dirname(current)
    if (identical(parent, current)) break     # "/" is its own dirname
    current <- parent
  }
  out
}

#' Find the package root at or above a path.
#'
#' @param path A file or directory inside the package.
#' @return The package root, or "" when there is none.
find_package_root <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) return("")
  start <- if (dir.exists(path)) path else dirname(path)
  start <- tryCatch(normalizePath(start, mustWork = FALSE), error = function(e) start)
  Find(is_package_root, path_ancestors(start)) %||% ""
}

#' The package's name, for a label a person recognises.
#'
#' @param root A package root.
#' @return The `Package:` field, or "" when unreadable.
package_name <- function(root) {
  if (!nzchar(root) || !is_package_root(root)) return("")
  fields <- tryCatch(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package"),
                     error = function(e) NULL, warning = function(w) NULL)
  # unname(): read.dcf() returns a matrix whose dimnames survive the [1,1]
  # extraction, so the "obvious" version returns a string NAMED "Package".
  # It prints identically, compares unequal to the plain string, and is the
  # kind of stowaway attribute that turns into a JSON object on the wire.
  if (is.null(fields)) "" else unname(trimws(fields[1L, 1L]))
}

# ── the vocabulary ──────────────────────────────────────────────────────────
#
# Every task declares WHAT IT ACTS ON, and that field is the reason this list
# is a table rather than a vector of names. Six tasks act on a package root;
# `render` acts on a single document, which has no DESCRIPTION and never
# will. Before `target` existed, `job_spec()` demanded a package root for
# everything — so rendering a standalone .qmd was refused for not being an R
# package, which is a true statement and a useless one.
#
# `pkg` is the package the task needs INSTALLED. It is carried so the child
# can say "devtools is not installed" instead of failing with an
# object-not-found deep inside a call the reader never made. `render` has no
# fixed answer there — it depends on the document — so it carries NA and the
# child decides; see pick_renderer() in job-run.R.
JOB_TASKS <- list(
  load     = list(label = "Load",     running = "Loading",     target = "package",  pkg = "pkgload"),
  document = list(label = "Document", running = "Documenting", target = "package",  pkg = "devtools"),
  test     = list(label = "Test",     running = "Testing",     target = "package",  pkg = "testthat"),
  check    = list(label = "Check",    running = "Checking",    target = "package",  pkg = "devtools"),
  build    = list(label = "Build",    running = "Building",    target = "package",  pkg = "devtools"),
  install  = list(label = "Install",  running = "Installing",  target = "package",  pkg = "devtools"),
  render   = list(label = "Render",   running = "Rendering",   target = "document", pkg = NA_character_)
)

#' Documents this kernel will render. Lower-cased extension, no dot.
#'
#' `.md` is here because both renderers accept it; a plain Markdown file with
#' no chunks still renders, and refusing it would be a distinction the reader
#' does not share.
RENDER_EXTENSIONS <- c("qmd", "rmd", "md")

# The formats offered, and "" FIRST because it is the default and the default
# matters: an empty format passes no `--to` at all, so the document's own YAML
# decides. A document whose header says `format: revealjs` must render as
# revealjs when the reader presses Render without choosing anything —
# defaulting to html would silently overrule the file.
RENDER_FORMATS <- c("", "html", "pdf", "docx")

#' Is this a single, plain, non-empty string? Mirrors serve.R's scalar_chr —
#' R's coercion rules are exactly permissive enough to turn a JSON array into
#' something that runs but is not what anybody meant.
#' @param x Value from the wire.
#' @return TRUE for a length-1 non-NA character vector.
job_scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

# A test filter is a regular expression testthat matches against file names.
# It is data, never code — but it is still bounded and stripped of control
# characters, because it is echoed into a label the UI renders and into the
# audit stream, where a newline would forge a record.
JOB_FILTER_MAX <- 200L

#' The lower-cased extension of a path, with no dot.
#' @param path A file path.
#' @return The extension, or "".
path_extension <- function(path) {
  base <- basename(path)
  if (!grepl(".", base, fixed = TRUE)) return("")
  tolower(sub("^.*\\.", "", base))
}

#' Decide whether one task may run, and exactly what would run.
#'
#' The whole decision in one pure function, returning it whole — the same
#' shape as `carmar_deployment()`, and for the same reason: three callers
#' (the supervisor, the tests, the UI's error message) must not each
#' re-derive it and drift.
#'
#' `target` is the ONE thing the task acts on: a package root for a package
#' task, a document for `render`. Which of those it must be is the task's own
#' declaration, never the caller's guess.
#'
#' @param task Task name from the wire.
#' @param target Directory or file from the wire.
#' @param opts Named list of extras from the wire: `filter` (test), `format`
#'   (render). Unknown members are ignored rather than refused — a page one
#'   version ahead must not be an error.
#' @return list(ok, error, task, target, root, label, package, env) — `env` is
#'   the named character vector of environment variables for the child, and is
#'   the ONLY channel by which wire data reaches it.
job_spec <- function(task, target, opts = list()) {
  bad <- function(msg) list(ok = FALSE, error = msg)
  if (!job_scalar(task) || !nzchar(task)) return(bad("no task was named"))
  if (!task %in% names(JOB_TASKS)) {
    return(bad(sprintf("'%s' is not a task this kernel runs (%s)",
                       task, paste(names(JOB_TASKS), collapse = ", "))))
  }
  spec <- JOB_TASKS[[task]]
  if (!job_scalar(target) || !nzchar(target)) {
    return(bad(if (identical(spec$target, "document")) "no document was named"
               else "no folder was named"))
  }
  filter <- if (job_scalar(opts$filter)) opts$filter else ""
  format <- if (job_scalar(opts$format)) opts$format else ""

  if (identical(spec$target, "package")) {
    if (!dir.exists(target)) return(bad(sprintf("'%s' is not a folder", target)))
    target <- tryCatch(normalizePath(target, mustWork = TRUE), error = function(e) target)
    if (!is_package_root(target)) {
      return(bad(paste0("'", target, "' is not an R package — no DESCRIPTION with a Package field. ",
                        "Open a file inside the package and try again.")))
    }
    root <- target
  } else {
    if (!file.exists(target) || dir.exists(target)) {
      return(bad(sprintf("'%s' is not a file", target)))
    }
    target <- tryCatch(normalizePath(target, mustWork = TRUE), error = function(e) target)
    ext <- path_extension(target)
    if (!ext %in% RENDER_EXTENSIONS) {
      return(bad(sprintf("CarmaR renders %s files; '%s' is a .%s",
                         paste0(".", RENDER_EXTENSIONS, collapse = ", "), basename(target),
                         if (nzchar(ext)) ext else "(no extension)")))
    }
    # The document's own folder, so a job still has one place it belongs —
    # the pane labels by it, and a relative output path resolves against it.
    root <- dirname(target)
  }

  if (nzchar(filter) && !identical(task, "test")) {
    return(bad("a filter only applies to the test task"))
  }
  filter <- gsub("[[:cntrl:]]", "", filter)
  if (nchar(filter) > JOB_FILTER_MAX) return(bad("the test filter is too long"))

  if (nzchar(format) && !identical(task, "render")) {
    return(bad("a format only applies to the render task"))
  }
  if (!format %in% RENDER_FORMATS) {
    return(bad(sprintf("'%s' is not a format this kernel renders (%s)",
                       format, paste(setdiff(RENDER_FORMATS, ""), collapse = ", "))))
  }

  list(
    ok = TRUE, error = "",
    task = task, target = target, root = root,
    label = spec$label, running = spec$running, targets = spec$target,
    package = if (identical(spec$target, "package")) package_name(root) else "",
    needs = spec$pkg,
    env = c(CARMAR_JOB_TASK = task, CARMAR_JOB_TARGET = target,
            CARMAR_JOB_ROOT = root, CARMAR_JOB_FILTER = filter,
            CARMAR_JOB_FORMAT = format)
  )
}

#' A sentence naming what is about to happen, before it happens.
#'
#' The same principle as Stage 6's `describePlan`: nothing runs unannounced,
#' and a label the reader cannot predict from the button they pressed is a
#' click-through waiting to happen.
#'
#' @param spec A successful `job_spec()`.
#' @return One line of text.
job_title <- function(spec) {
  if (identical(spec$targets, "document")) {
    format <- spec$env[["CARMAR_JOB_FORMAT"]]
    return(sprintf("Rendering %s%s", basename(spec$target),
                   if (nzchar(format)) paste0(" to ", format) else ""))
  }
  name <- if (nzchar(spec$package)) spec$package else basename(spec$root)
  filter <- spec$env[["CARMAR_JOB_FILTER"]]
  if (identical(spec$task, "test") && nzchar(filter)) {
    sprintf("%s %s — tests matching '%s'", spec$running, name, filter)
  } else {
    sprintf("%s %s", spec$running, name)
  }
}
