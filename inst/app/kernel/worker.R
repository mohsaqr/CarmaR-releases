#!/usr/bin/env Rscript
#
# Worker — evaluates R code on behalf of the supervisor.
#
# Protocol
#   stdin   NDJSON commands, one per line: {"type":"exec","id":...,"source":...}
#   stdout  the user's own output, untouched, PLUS control frames: a line
#           beginning with the session sentinel followed by JSON.
#   stderr  R's own diagnostics (never framed; the supervisor tags it verbatim).
#
# Frames emitted per cell: `stream` (warning/message), `plot` (one per graphics
# page, base64 PNG), `dataframe` (a structured top-level data.frame), and
# exactly one `done`.
#
# Why no output capture: `sink()` / `capture.output()` buffer, which destroys
# streaming — the caller sees nothing until the cell finishes. Letting R print
# straight to stdout keeps output real-time and costs nothing, as long as
# control frames are distinguishable. The sentinel is a per-session random
# token supplied by the supervisor, so user code cannot forge a frame.
#
# Why a device rather than source-sniffing: the notebook owns this session, so
# it can install a graphics device once and let every page land in it. Guessing
# whether a cell plots by grepping its source for "plot(" misses plots drawn
# inside functions and fires on a variable named `myplot`.

# Finder/launchd can start the app with LC_CTYPE=C even on a UTF-8 Mac. In that
# locale R prints box drawing and other non-ASCII text as escapes, and knit can
# only preserve the already-corrupted output. Select an installed UTF-8 CTYPE
# inside the worker; keep every other locale category (numbers, dates, sorting)
# untouched. The candidates cover current Linux, macOS, and older R builds.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (candidate in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    suppressWarnings(try(Sys.setlocale("LC_CTYPE", candidate), silent = TRUE))
    if (isTRUE(l10n_info()[["UTF-8"]])) break
  }
}

# jsonlite is used via :: only — attaching it would put it on the USER's search
# path, and the worker must leave no trace in the session it hosts.
stopifnot(requireNamespace("jsonlite", quietly = TRUE))

# Everything below lives in a private scope. Two reasons, both load-bearing:
# the Environment pane must show the USER's objects and not the kernel's
# plumbing, and user code must not be able to clobber `emit` or `sentinel` and
# break the protocol from inside the session it is running in.
local({

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L, nzchar(args[1]))
sentinel <- args[1]

PLOT_WIDTH <- 900L
PLOT_HEIGHT <- 620L
PLOT_RES <- 110L
MAX_ROWS <- 500L
# Hard ceilings for the data viewer. Server-side because the client is one
# `limit: 1e9` typo away from asking for everything; the reply reports what was
# clamped, so the UI never has to guess what it actually received.
MAX_VIEW_ROWS <- 500L
MAX_VIEW_COLS <- 100L
MAX_VIEW_BYTES <- 512L * 1024L
MAX_VIEW_CELL_CHARS <- 512L
MAX_VIEW_LABEL_CHARS <- 128L
MAX_STRUCT_CHILDREN <- 200L
MAX_COMPLETIONS <- 200L

# ── the confinement root ────────────────────────────────────────────────────
# CARMAR_ROOT restricts every FILE command — browse, read, write, import, and
# changing the working directory — to one subtree. It exists for managed
# deployments where a notebook should see the project share and nothing else.
#
# It is a GUARDRAIL, not a sandbox, and the difference must be stated plainly
# rather than implied: this session evaluates arbitrary R, so a determined user
# can call file.remove() on anything their account can reach and no in-process
# check can stop them. What the root does buy is real all the same — it stops
# the UI (and a mis-click, a stray path, a shared notebook someone else wrote)
# from wandering out of the project, and it makes "the notebook only touches
# /srv/projects/x" an enforceable default rather than a hope. Containment is
# the operating system's job: run the kernel as a user who cannot read the
# rest, or in a container.
confine_root <- local({
  raw <- Sys.getenv("CARMAR_ROOT", "")
  if (!nzchar(raw)) NULL
  else normalizePath(path.expand(raw), mustWork = FALSE)
})

#' Is `p` inside the confinement root (when there is one)?
#'
#' Compared after normalizePath, so `..` and symlinks are resolved before the
#' comparison rather than after — string-prefix checks on un-normalised paths
#' are how confinement bugs happen. The trailing separator matters too:
#' without it, /srv/project would also admit /srv/project-secrets.
within_root <- function(p) {
  if (is.null(confine_root)) return(TRUE)
  if (!is.character(p) || length(p) != 1L || is.na(p)) return(FALSE)
  full <- normalizePath(path.expand(p), mustWork = FALSE)
  identical(full, confine_root) ||
    startsWith(full, paste0(confine_root, .Platform$file.sep))
}

#' The standard refusal, so every file command says the same thing.
outside_root_msg <- function() {
  sprintf("outside the permitted folder (%s)", confine_root)
}

# R ships the completion engine RStudio and Jupyter both drive; these knobs
# make package names (ipck), function signatures (func → trailing "("),
# argument names (args → trailing "=") and quoted file paths (files) all
# participate. try(): a future R renaming a setting must not kill the boot.
try(utils::rc.settings(ipck = TRUE, func = TRUE, args = TRUE, files = TRUE),
    silent = TRUE)

# ── the import sniffer ──────────────────────────────────────────────────────
# Format and column-type detection lives in its own file so it can be tested
# without starting a worker (test/import-sniff.test.R sources it directly);
# this file runs a dispatch loop the moment it loads, so nothing inside it is
# reachable from a test. `local = TRUE` keeps the functions in the worker's
# private scope, where the rest of the plumbing lives — the Environment pane
# must keep showing the user's objects and not the kernel's.
#
# Resolved from the WORKER'S OWN path, not the working directory: the worker
# is started by three different launchers (kernel.R, the Chrome bridge host,
# and the packaged inst/app/kernel) and none of them guarantee a cwd.
import_sources <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  here <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  } else getwd()
  file.path(here, c("sniff.R", "chooser.R"))
})
# environment(), not parent.frame(): inside a nested local() the parent frame
# is the eval machinery's, not this file's private scope, and the functions
# landed somewhere the handlers could not see them. Caught end-to-end, where
# the kernel answered "this kernel is too old for the import wizard".
# The target environment is captured HERE, at the worker's own scope. Calling
# environment() inside the lambda would name the lambda's frame and the
# functions would load somewhere no handler can see — the same mistake this
# block already made once, with sniff.R, and it surfaced only end-to-end as
# "this kernel is too old for the import wizard".
import_env <- environment()
invisible(lapply(Filter(file.exists, import_sources),
                 function(f) sys.source(f, envir = import_env)))

# `Rscript` starts with repos = "@CRAN@", a placeholder that resolves only by
# ASKING the user to pick a mirror — which a non-interactive session cannot do,
# so install.packages() fails with "trying to use CRAN without setting a
# mirror" before it does anything. Fill it in, but never override a mirror the
# user already set: their .Rprofile loads here (we deliberately do not use
# --vanilla), and it is where r-universe and institutional mirrors live.
local({
  repos <- getOption("repos")
  cran <- if (is.null(repos)) NA_character_ else unname(repos["CRAN"])
  unset <- is.null(repos) || is.na(cran) || !nzchar(cran) || identical(cran, "@CRAN@")
  if (unset) {
    # Keep every other repo the profile declared; only CRAN was missing.
    # modifyList says that in one verb: it replaces CRAN where it exists and
    # appends it where it does not, leaving r-universe and institutional
    # mirrors exactly as the user's .Rprofile left them.
    others <- if (is.null(repos)) list() else as.list(repos)
    options(repos = unlist(utils::modifyList(
      others, list(CRAN = "https://cloud.r-project.org"))))
  }
})

# The reserved words R's engine offers with "(" appended (for/if/while) or
# bare; they are neither functions nor variables and the UI badges them apart.
R_KEYWORDS <- c("if", "else", "repeat", "while", "function", "for", "in",
                "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
                "NA", "NA_integer_", "NA_real_", "NA_character_", "NA_complex_")

#' Find a graphics device type that actually WORKS in this R.
#'
#' `capabilities("cairo")` reports what R was COMPILED with, not what loads.
#' On the CRAN macOS build it says TRUE while `png(type="cairo")` fails to
#' dlopen (it wants libXrender from XQuartz) and silently produces no file —
#' so every plot vanished with no error anywhere. The only honest test is to
#' open a device, draw a page, and see whether bytes appear.
#'
#' @return The first working type: quartz on macOS, else cairo, else Xlib.
detect_plot_type <- function() {
  candidates <- c(if (isTRUE(capabilities("aqua"))) "quartz",
                  if (isTRUE(capabilities("cairo"))) "cairo",
                  "Xlib")
  works <- function(type) {
    f <- tempfile(fileext = ".png")
    ok <- tryCatch({
      suppressWarnings(grDevices::png(f, width = 12, height = 12, type = type))
      graphics::plot.new()
      grDevices::dev.off()
      file.exists(f) && file.info(f)$size > 0L
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!is.null(grDevices::dev.list())) try(grDevices::dev.off(), silent = TRUE)
    unlink(f)
    isTRUE(ok)
  }
  hit <- Position(works, candidates)
  if (is.na(hit)) candidates[1L] else candidates[hit]
}

PLOT_TYPE <- detect_plot_type()

#' Find a VECTOR device that actually works, or NULL.
#'
#' Same discipline as detect_plot_type and for a sharper reason: on the CRAN
#' macOS build `grDevices::svg()` opens without error, draws without error,
#' and writes a ZERO-BYTE file, because it is Cairo and Cairo cannot dlopen
#' without XQuartz. Measured on this machine. So the probe draws a page and
#' insists on bytes that actually begin an SVG document.
#'
#' Unlike detect_plot_type this returns NULL rather than falling through to a
#' first candidate: "no vector device" is a real answer the caller must handle
#' by saying so, not by silently producing something else.
#'
#' @return "svglite", "grDevices", or NULL.
detect_svg_device <- function() {
  works <- function(open) {
    f <- tempfile(fileext = ".svg")
    ok <- tryCatch({
      suppressWarnings(open(f))
      graphics::plot.new()
      graphics::text(0.5, 0.5, "x")
      grDevices::dev.off()
      file.exists(f) && file.info(f)$size > 0L &&
        grepl("<svg", paste(readLines(f, n = 8L, warn = FALSE), collapse = ""), fixed = TRUE)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!is.null(grDevices::dev.list())) try(grDevices::dev.off(), silent = TRUE)
    unlink(f)
    isTRUE(ok)
  }
  # svglite first: pure C++, no Cairo, and the only one that works on a stock
  # macOS R. grDevices::svg() is the fallback for machines that have Cairo.
  if (requireNamespace("svglite", quietly = TRUE) &&
      works(function(f) svglite::svglite(f, width = 2, height = 2))) return("svglite")
  if (works(function(f) grDevices::svg(f, width = 2, height = 2))) return("grDevices")
  NULL
}

SVG_DEVICE <- detect_svg_device()

#' Is this chunk asking for vector output, and can we give it?
plot_is_svg <- function(dims) isTRUE(dims$format == "svg") && !is.null(SVG_DEVICE)

#' Write one control frame to stdout and flush immediately.
#'
#' @param obj Named list, serialised to JSON.
#' @return Invisibly NULL.
emit <- function(obj) {
  cat(sentinel, jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", na = "null",
                       digits = NA), "\n", sep = "")
  flush(stdout())
  invisible(NULL)
}

#' A function's formals as one display string, defaults included.
#'
#' `formals()` is NULL for primitives like `sum`; `args()` still knows their
#' signature, so it is the fallback. The empty symbol deparses to "", which is
#' how an argument with no default is told apart from one whose default is "".
#'
#' @param f A function.
#' @return A single string, e.g. "x, y = 2, ...".
formals_string <- function(f) {
  fl <- formals(f)
  if (is.null(fl)) fl <- tryCatch(formals(args(f)), error = function(e) NULL)
  if (is.null(fl) || length(fl) == 0L) return("")
  paste(vapply(names(fl), function(nm) {
    default <- paste(deparse(fl[[nm]]), collapse = " ")
    if (nzchar(default)) paste(nm, "=", default) else nm
  }, character(1)), collapse = ", ")
}

#' Describe every object in the global environment, for the Environment pane.
#'
#' `str()`-style one-liners rather than values: an IDE pane must stay cheap to
#' refresh after every cell, and a 2 GB matrix must not be serialised to say it
#' exists. Sizes come from object.size so the pane can show what is costing.
#'
#' @param id Request id, echoed back.
#' @return Invisibly NULL. Emits one `env` frame.
emit_env <- function(id) {
  names_ <- ls(globalenv(), all.names = FALSE)
  describe <- function(nm) {
    v <- get(nm, envir = globalenv())
    dims <- if (!is.null(dim(v))) paste(dim(v), collapse = " × ") else as.character(length(v))
    summary_line <- if (is.data.frame(v)) {
      sprintf("%d obs. of %d variable%s", nrow(v), ncol(v), if (ncol(v) == 1L) "" else "s")
    } else if (is.function(v)) {
      paste0("function(", paste(names(formals(v)), collapse = ", "), ")")
    } else {
      paste(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE))[1L],
            collapse = "")
    }
    # Three kinds because the pane treats them differently: "data" opens the
    # viewer, "function" opens the inspector at its source, "value" just shows
    # itself. Matrices count as data — cor(), table() and coef(summary()) are
    # the most viewer-worthy things R produces (see matrix_to_df).
    is_data <- is.data.frame(v) || is.matrix(v)
    kind <- if (is.function(v)) "function" else if (is_data) "data" else "value"
    base <- list(name = nm, class = class(v)[1L], dims = dims,
                 bytes = as.numeric(utils::object.size(v)), summary = summary_line,
                 kind = kind)
    if (is_data) c(base, list(nrow = nrow(v), ncol = ncol(v)))
    else if (is.function(v)) c(base, list(args = formals_string(v)))
    else base
  }
  emit(list(type = "env", id = id,
            objects = if (length(names_)) lapply(names_, describe) else list()))
}

#' Inspect one object — the pane an Environment row expands into.
#'
#' str() plus a capped print, never the value itself: an inspector's job is to
#' describe a 2 GB model without shipping it. `max.print` bounds what print()
#' GENERATES — capping capture.output afterwards would still pay to render a
#' 1e8-element vector first.
#'
#' @param id Request id.
#' @param name Name of an object visible from the global environment.
#' @param max_lines Cap applied separately to str, preview and source.
#' @return Invisibly NULL. Emits one `obj` frame.
emit_obj <- function(id, name = NULL, max_lines = 200L) {
  # Wire-supplied fields get guards, not stopifnot: a malformed request must
  # answer with an error field, never take the worker down mid-session.
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    emit(list(type = "obj", id = id, name = name, error = "bad name"))
    return(invisible(NULL))
  }
  # Default inherits = TRUE, so package data (mtcars) inspects like the user's.
  if (!exists(name, envir = globalenv())) {
    emit(list(type = "obj", id = id, name = name, error = "not found"))
    return(invisible(NULL))
  }
  v <- get(name, envir = globalenv())
  capped <- function(lines) {
    if (length(lines) > max_lines) {
      lines <- c(lines[seq_len(max_lines)],
                 sprintf("... (%d more lines)", length(lines) - max_lines))
    }
    paste(lines, collapse = "\n")
  }
  old <- options(max.print = 2000L)
  on.exit(options(old), add = TRUE)
  frame <- list(
    type = "obj", id = id, name = name, class = class(v)[1L],
    str = capped(tryCatch(utils::capture.output(utils::str(v)),
                          error = function(e) conditionMessage(e))),
    preview = capped(tryCatch(utils::capture.output(print(v)),
                              error = function(e) conditionMessage(e)))
  )
  if (is.function(v)) {
    frame$formals <- formals_string(v)
    frame$source <- capped(deparse(v))
  }
  emit(frame)
}

#' The child accessors a node exposes — the tree viewer's notion of "expandable".
#'
#' S4 slots, then list elements (data.frames are lists of columns). Everything
#' else — atomics, functions, calls, and above all ENVIRONMENTS — reports no
#' children: environments can be cyclic (an env holding itself, R6 objects), so
#' walking them turns a lazy tree into an infinite one.
#'
#' @param obj Any object.
#' @return Character vector of accessor tokens: names, or "[[i]]" for unnamed.
child_keys <- function(obj) {
  if (isS4(obj)) return(methods::slotNames(class(obj)))
  if (is.environment(obj) || !is.list(obj)) return(character(0))
  nms <- names(obj)
  if (is.null(nms)) nms <- character(length(obj))
  ifelse(nzchar(nms), nms, sprintf("[[%d]]", seq_along(obj)))
}

#' Fetch one child by the token child_keys() produced for it.
#'
#' @param obj The parent node.
#' @param key A name, "[[i]]" index token, or S4 slot name.
#' @return The child value.
child_get <- function(obj, key) {
  if (isS4(obj)) return(methods::slot(obj, key))
  if (grepl("^\\[\\[\\d+\\]\\]$", key)) obj[[as.integer(gsub("\\D", "", key))]]
  else obj[[key]]
}

#' One line that says what a node IS without shipping what it holds.
#'
#' @param v Any object.
#' @return A single short string.
struct_preview <- function(v) {
  if (is.environment(v)) return("<environment>")
  if (is.function(v)) return(paste0("function(", formals_string(v), ")"))
  if (isS4(v)) return(paste0("S4 object of class ", class(v)[1L]))
  if (is.data.frame(v) || is.matrix(v)) return(paste(dim(v), collapse = " × "))
  if (is.list(v)) return(sprintf("list of %d", length(v)))
  if (is.atomic(v) && length(v) > 0L) {
    # head() before format(): formatting five elements of a 1e8 vector must
    # not pay for the other 99,999,995.
    shown <- format(utils::head(v, 5L), trim = TRUE)
    return(paste0(paste(shown, collapse = ", "), if (length(v) > 5L) ", …" else ""))
  }
  if (is.atomic(v)) return(paste0(class(v)[1L], "(0)"))
  class(v)[1L]
}

#' One level of an object's structure, for a lazily expanded tree viewer.
#'
#' The client sends back the `path` a child arrived with to expand that child;
#' nothing below the requested level is ever serialised, so an lm holding a
#' thousand-row model frame costs 13 child descriptors, not the frame.
#'
#' @param id Request id.
#' @param name Name of an object visible from the global environment.
#' @param path Character vector of accessor tokens; empty/NULL for the root.
#' @return Invisibly NULL. Emits one `struct` frame.
emit_struct <- function(id, name = NULL, path = NULL) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    emit(list(type = "struct", id = id, name = name, error = "bad name"))
    return(invisible(NULL))
  }
  if (!exists(name, envir = globalenv())) {
    emit(list(type = "struct", id = id, name = name, error = "not found"))
    return(invisible(NULL))
  }
  keys <- if (is.null(path)) character(0) else as.character(unlist(path))
  # Each step validates the token against child_keys first: `lst[["absent"]]`
  # returns NULL rather than erroring, and a bad path must be an error frame,
  # not a silent NULL node described with a straight face.
  step <- function(o, k) {
    if (is.environment(o)) stop("environments are not walked")
    if (!(k %in% child_keys(o))) stop("no such element: ", k)
    child_get(o, k)
  }
  node <- tryCatch(Reduce(step, keys, init = get(name, envir = globalenv())),
                   error = function(e) structure(class = "carmar_fail",
                                                 list(msg = conditionMessage(e))))
  if (inherits(node, "carmar_fail")) {
    emit(list(type = "struct", id = id, name = name, path = as.list(keys),
              error = node$msg))
    return(invisible(NULL))
  }
  kids <- child_keys(node)
  describe_child <- function(k) {
    v <- tryCatch(child_get(node, k), error = function(e) NULL)
    list(name = k, path = as.list(c(keys, k)), class = class(v)[1L],
         type = typeof(v), length = length(v),
         size = as.numeric(utils::object.size(v)),
         isLeaf = length(child_keys(v)) == 0L,
         preview = struct_preview(v))
  }
  emit(list(type = "struct", id = id, name = name, path = as.list(keys),
            class = class(node)[1L], type = typeof(node), length = length(node),
            size = as.numeric(utils::object.size(node)),
            preview = struct_preview(node),
            children = lapply(utils::head(kids, MAX_STRUCT_CHILDREN), describe_child),
            truncated = length(kids) > MAX_STRUCT_CHILDREN))
}

#' Describe one column the way a data viewer shows it: type, a sparkline's
#' worth of shape, a stat label, and how much is missing.
#'
#' The bins are computed HERE rather than shipping the column: a 500k-row
#' numeric vector is 4 MB of JSON and 12 counts is 60 bytes, and the viewer
#' only ever draws the 12.
#'
#' @param col A column vector.
#' @param name Column name.
#' @return A named list.
describe_column <- function(col, name) {
  n <- length(col)
  # ONE is.na pass. It was computed twice — once for the count, once to build
  # `ok` — and on a million-row column that second pass is a wasted 4 MB
  # logical and ~0.6 ms, paid for every column of every page.
  na <- is.na(col)
  missing <- sum(na)
  base <- list(name = name, class = class(col)[1L], n = n, missing = missing)
  # A SHORT summary for the hover panel, formatted here for the same reason the
  # card's is (fmt_pct): R decides significant digits, and two formatters would
  # eventually disagree about the same number. Deliberately not the card's list
  # — hovering wants the six figures you would glance at, not fourteen.
  pair <- function(label, value) list(label = label, value = value)
  miss_pair <- if (missing > 0L) list(pair("Missing", fmt_share(missing, n))) else list()

  # Every `bins`/`levels` below is wrapped in I(): auto_unbox turns a length-1
  # vector into a bare scalar, so a constant column or single-level factor
  # shipped `"bins": 10` and broke every client that mapped over it. I() pins
  # the array shape regardless of length; scalars elsewhere stay scalars.
  if (is.numeric(col)) {
    ok <- col[!na]
    if (length(ok) == 0L) return(c(base, list(kind = "numeric", bins = I(integer(0)),
                                              stat = "all missing",
                                              summary = I(list(pair("Missing",
                                                fmt_share(missing, n)))))))
    rng <- range(ok)
    # tabulate(), not table(): table drops empty bins, so a gap in the data
    # silently shortens the sparkline and every bar after it shifts left.
    bins <- if (diff(rng) == 0) rep(length(ok), 1L) else
      tabulate(cut(ok, breaks = 12L, labels = FALSE, include.lowest = TRUE), nbins = 12L)
    med <- stats::median(ok)
    # mean and sd measure at 0.1 and 0.2 ms on a million rows, against the
    # 3.1 ms this function already costs — the cheapest useful thing to add.
    mu <- mean(ok)
    sigma <- stats::sd(ok)
    c(base, list(kind = "numeric", bins = I(as.integer(bins)),
                 min = rng[1L], max = rng[2L], median = med, mean = mu, sd = sigma,
                 summary = I(c(list(pair("Mean", fmt_num(mu)),
                                    pair("Std. dev.", fmt_num(sigma)),
                                    pair("Minimum", fmt_num(rng[1L])),
                                    pair("Median", fmt_num(med)),
                                    pair("Maximum", fmt_num(rng[2L]))), miss_pair)),
                 stat = sprintf("%s – %s", fmt_num(rng[1L]), fmt_num(rng[2L]))))
  } else if (is.logical(col)) {
    yes <- sum(col %in% TRUE)
    no <- sum(col %in% FALSE)
    c(base, list(kind = "logical",
                 bins = I(as.integer(c(yes, no))),
                 levels = I(c("TRUE", "FALSE")),
                 summary = I(c(list(pair("TRUE", fmt_share(yes, yes + no)),
                                    pair("FALSE", fmt_share(no, yes + no))), miss_pair)),
                 stat = sprintf("%d true / %d false", yes, no)))
  } else {
    text <- as.character(col[!na])
    tab <- sort(table(text), decreasing = TRUE)
    top <- utils::head(tab, 12L)
    # The commonest levels answer "what is in here" far better than the level
    # COUNT alone, and they are already computed for the sparkline. Four are
    # listed when four is all there is, so a column with exactly four levels
    # does not show three and silently swallow the last one.
    lead <- utils::head(tab, if (length(tab) <= 4L) length(tab) else 3L)
    tops <- lapply(seq_along(lead), function(i)
      pair(substr(names(lead)[i], 1L, MAX_VIEW_LABEL_CHARS),
           fmt_share(lead[[i]], length(text))))
    c(base, list(kind = "categorical", bins = I(as.integer(top)),
                 levels = I(substr(as.character(names(top)), 1L, MAX_VIEW_LABEL_CHARS)),
                 nlevels = length(tab),
                 summary = I(c(list(pair("Distinct levels", fmt_count(length(tab)))),
                               tops, miss_pair)),
                 stat = sprintf("%d level%s", length(tab), if (length(tab) == 1L) "" else "s")))
  }
}

fmt_num <- function(x) {
  # as.character(NA_real_) is NA_character_, not "NA" — and a genuine NA in a
  # display field ships as JSON null, which the card then prints as the word
  # "null". A constant column's skewness is exactly this case (sd is 0, so the
  # z-scores are 0/0), so it is a value the viewer really does meet.
  if (is.nan(x)) return("NaN")
  if (is.na(x)) return("NA")
  if (!is.finite(x)) return(as.character(x))
  if (abs(x) >= 1e5 || (abs(x) < 1e-3 && x != 0)) format(x, digits = 3, scientific = TRUE)
  else format(round(x, 3), trim = TRUE)
}

fmt_count <- function(x) format(as.numeric(x), big.mark = ",", trim = TRUE, scientific = FALSE)

#' A share as a bare percentage: "85.3%".
#'
#' Every percentage on the statistics card comes from HERE, including the ones
#' beside each level in the bar list. R and JavaScript do not round halves the
#' same way — R's round() goes to even, so 50/4000 is 1.2%, while JavaScript's
#' toFixed(1) gives 1.3% — and a card that computed some of its own percentages
#' printed both, for the same count, two inches apart.
#' Always one decimal: format() drops a trailing zero, so a column of shares
#' read "1.2% / 1.1% / 1% / 1%" and the eye stopped trusting the alignment.
fmt_pct <- function(part, whole) {
  if (!is.finite(whole) || whole <= 0) return("")
  paste0(formatC(100 * part / whole, format = "f", digits = 1), "%")
}

#' A share as count and percentage: "3,412 (85.3%)".
fmt_share <- function(part, whole) {
  if (!is.finite(whole) || whole <= 0) return(fmt_count(part))
  sprintf("%s (%s)", fmt_count(part), fmt_pct(part, whole))
}

MAX_STATS_LEVELS <- 15L
STATS_BINS <- 24L

#' Everything a statistics card shows about ONE column.
#'
#' Deliberately separate from describe_column(): that one runs for every column
#' of every page and must stay cheap, so it ships 12 bins and a label. This one
#' runs when a reader asks about a single column and can afford quantiles, shape
#' moments and a level table.
#'
#' Numbers come back BOTH ways — as strings R formatted (so the card never
#' re-invents significant digits in JavaScript) and, where a drawing needs them,
#' as raw numerics. The two never disagree because they come from one value.
#'
#' @param col A column vector.
#' @param column_name Its name.
#' @return A named list; see the `colstats` frame.
colstats_payload <- function(col, column_name) {
  n <- length(col)
  missing <- sum(is.na(col))
  ok <- col[!is.na(col)]
  pair <- function(label, value) list(label = label, value = value)
  base <- list(column = column_name, class = paste(class(col), collapse = "/"),
               n = n, missing = missing, present = length(ok),
               distinct = length(unique(ok)))

  if (is.numeric(col) || inherits(col, "Date") || inherits(col, "POSIXct")) {
    dated <- inherits(col, "Date") || inherits(col, "POSIXct")
    x <- as.numeric(ok)
    if (!length(x)) {
      return(c(base, list(kind = if (dated) "date" else "numeric",
                          summary = I(list(pair("Present", "0 — every value is missing"))))))
    }
    show <- if (dated) function(v) format(if (inherits(col, "Date"))
      as.Date(v, origin = "1970-01-01") else as.POSIXct(v, origin = "1970-01-01", tz = "UTC"))
      else fmt_num
    q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
    iqr <- q[3L] - q[1L]
    m <- mean(x)
    s <- stats::sd(x)
    # Fences are Tukey's, so "outlier" here means what a boxplot means by it —
    # not a normal-theory z cut, which would be a different claim about data
    # nobody has shown is normal.
    lo_fence <- q[1L] - 1.5 * iqr
    hi_fence <- q[3L] + 1.5 * iqr
    outliers <- sum(x < lo_fence | x > hi_fence)
    # Moments by hand rather than by dependency: both are one line, and
    # e1071/moments are not worth a require() in a kernel that must boot fast.
    z <- if (is.finite(s) && s > 0) (x - m) / s else rep(NA_real_, length(x))
    skew <- if (all(is.finite(z))) mean(z^3) else NA_real_
    kurt <- if (all(is.finite(z))) mean(z^4) - 3 else NA_real_
    rng <- range(x)
    bins <- if (diff(rng) == 0) as.integer(length(x)) else
      tabulate(cut(x, breaks = STATS_BINS, labels = FALSE, include.lowest = TRUE),
               nbins = STATS_BINS)
    summary <- list(
      pair("Mean", show(m)), pair("Std. dev.", if (dated) fmt_num(s) else fmt_num(s)),
      pair("Minimum", show(rng[1L])), pair("1st quartile", show(q[1L])),
      pair("Median", show(q[2L])), pair("3rd quartile", show(q[3L])),
      pair("Maximum", show(rng[2L])), pair("IQR", if (dated) fmt_num(iqr) else fmt_num(iqr)),
      pair("Median abs. dev.", fmt_num(stats::mad(x))),
      pair("Skewness", fmt_num(skew)), pair("Excess kurtosis", fmt_num(kurt)),
      pair("Outliers (1.5 IQR)", fmt_share(outliers, length(x))))
    if (!dated) summary <- c(summary, list(
      pair("Zeros", fmt_share(sum(x == 0), length(x))),
      pair("Negative", fmt_share(sum(x < 0), length(x)))))
    if (dated) summary <- c(summary, list(pair("Span", paste(fmt_num(diff(rng) /
      if (inherits(col, "Date")) 1 else 86400), "days"))))
    return(c(base, list(kind = if (dated) "date" else "numeric",
                        summary = I(summary),
                        bins = I(as.integer(bins)),
                        binMin = rng[1L], binMax = rng[2L],
                        box = list(min = rng[1L], q1 = q[1L], median = q[2L],
                                   q3 = q[3L], max = rng[2L],
                                   lower = max(lo_fence, rng[1L]),
                                   upper = min(hi_fence, rng[2L]),
                                   outliers = outliers))))
  }

  if (is.logical(col)) {
    yes <- sum(col %in% TRUE)
    no <- sum(col %in% FALSE)
    return(c(base, list(kind = "logical",
      summary = I(list(pair("TRUE", fmt_share(yes, yes + no)),
                       pair("FALSE", fmt_share(no, yes + no)),
                       pair("Missing", fmt_share(missing, n)))),
      levels = I(c("TRUE", "FALSE")), counts = I(as.integer(c(yes, no))),
      shares = I(c(fmt_pct(yes, yes + no), fmt_pct(no, yes + no))), other = 0L)))
  }

  text <- as_search_text(ok)
  tab <- sort(table(text), decreasing = TRUE)
  top <- utils::head(tab, MAX_STATS_LEVELS)
  widths <- nchar(text)
  summary <- list(
    pair("Distinct levels", fmt_count(length(tab))),
    pair("Most common", if (length(tab)) sprintf("%s — %s", names(tab)[1L],
                                                 fmt_share(tab[[1L]], length(text))) else "—"),
    pair("Least common", if (length(tab)) sprintf("%s — %s", names(tab)[length(tab)],
                                                  fmt_share(tab[[length(tab)]], length(text))) else "—"),
    pair("Empty strings", fmt_share(sum(!nzchar(text)), length(text))),
    pair("Shortest", if (length(widths)) fmt_count(min(widths)) else "—"),
    pair("Longest", if (length(widths)) fmt_count(max(widths)) else "—"),
    pair("Mean length", if (length(widths)) fmt_num(mean(widths)) else "—"))
  other <- as.integer(length(text) - sum(top))
  c(base, list(kind = "categorical", summary = I(summary),
               levels = I(substr(names(top), 1L, MAX_VIEW_LABEL_CHARS)),
               counts = I(as.integer(top)),
               shares = I(vapply(as.integer(top), fmt_pct, character(1),
                                 whole = length(text))),
               other = other, otherShare = fmt_pct(other, length(text)),
               nlevels = length(tab)))
}

#' The statistics card: one column, profiled over the rows the grid is showing.
#'
#' Resolves the object exactly as emit_view does, and takes the SAME query and
#' filters, so the panel describes the visible data rather than a different
#' population that happens to share a name.
#'
#' @param id Request id.
#' @param name Object name or expression, as for `view`.
#' @param column The column to profile.
#' @param query,filters The viewer's active search and column filters.
emit_colstats <- function(id, name, column, query = NULL, filters = NULL) {
  column <- if (is.character(column) && length(column)) column[[1L]] else ""
  obj <- tryCatch(eval(parse(text = name), globalenv()), error = function(e) NULL)
  if (!is.data.frame(obj)) obj <- tryCatch(as.data.frame(obj, stringsAsFactors = FALSE),
                                           error = function(e) NULL)
  if (is.null(obj) || !nzchar(column)) {
    emit(list(type = "colstats", id = id, name = name, column = column,
              error = "not found"))
    return(invisible(NULL))
  }
  total <- nrow(obj)
  narrowed <- view_filter(obj, query, filters)
  obj <- narrowed$obj
  # The viewer truncates long column names for display; match on the truncated
  # name too, or a card opened from a clipped header can never find its column.
  hit <- match(column, names(obj))
  if (is.na(hit)) hit <- match(column, substr(names(obj), 1L, MAX_VIEW_LABEL_CHARS))
  if (is.na(hit)) {
    emit(list(type = "colstats", id = id, name = name, column = column,
              error = "no such column"))
    return(invisible(NULL))
  }
  tryCatch(
    emit(c(list(type = "colstats", id = id, name = name),
           colstats_payload(obj[[hit]], column),
           list(rows = nrow(obj), totalRows = total,
                filtered = nzchar(narrowed$query) || narrowed$count > 0L))),
    interrupt = function(i) emit(list(type = "colstats", id = id, name = name,
                                      column = column, error = "interrupted"))
  )
}

#' Autocomplete via R's own engine — the one behind TAB in the console,
#' RStudio and Jupyter. Not hand-rolled: the engine already understands `$`
#' and `@` access, argument names inside a call, `::` namespaces, library()
#' package names and quoted file paths, and hand-rolling any one of those
#' badly is worse than none.
#'
#' The five utils functions driving it are internal (:::), so the whole
#' pipeline is wrapped and degrades to an empty item list if a future R
#' renames them — a completion popup that is sometimes empty beats a worker
#' that dies on a keystroke.
#'
#' `start`/`end` are the 0-based character range of `line` the completion
#' REPLACES (`end` exclusive, = cursor): the engine completes "x$al" to the
#' full "x$alpha", so the client splices `line[0:start] + value + line[end:]`
#' and must never re-derive the token itself.
#'
#' @param id Request id.
#' @param line The source line being typed.
#' @param cursor Character offset of the caret in `line` (0-based).
#' @param max_items Cap on items shipped; `truncated` reports the cut.
#' @return Invisibly NULL. Emits one `complete` frame.
emit_complete <- function(id, line = NULL, cursor = NULL, max_items = MAX_COMPLETIONS) {
  if (!is.character(line) || length(line) != 1L || is.na(line)) line <- ""
  cursor <- max(0L, min(as_count(cursor, nchar(line)), nchar(line)))
  empty <- list(type = "complete", id = id, start = cursor, end = cursor,
                token = "", items = list(), truncated = FALSE)
  st <- tryCatch({
    ce <- utils:::.CompletionEnv
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(cursor)
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
    list(token = as.character(ce[["token"]]), start = as.integer(ce[["start"]]),
         comps = as.character(utils:::.retrieveCompletions()),
         quoted = isTRUE(ce[["fileName"]]))
  }, error = function(e) NULL, interrupt = function(i) NULL)
  if (is.null(st) || length(st$start) != 1L || is.na(st$start)) {
    emit(empty)
    return(invisible(NULL))
  }
  # The engine's own fileName flag is not stable across versions (observed
  # FALSE on 4.5.2 while returning paths), so the quote immediately before
  # the token is the authoritative signal for path completion.
  quoted <- st$quoted ||
    (st$start > 0L && substr(line, st$start, st$start) %in% c("\"", "'"))
  in_library <- grepl(
    "(library|require|requireNamespace|loadNamespace)\\s*\\(\\s*[\"']?\\s*$",
    substr(line, 1L, st$start))
  is_name <- function(v) grepl("^[.a-zA-Z][._a-zA-Z0-9]*$", v)
  describe_item <- function(v) {
    if (endsWith(v, "=")) return(list(value = v, kind = "argument", detail = ""))
    if (endsWith(v, "::")) return(list(value = v, kind = "package", detail = ""))
    if (quoted) return(list(value = v, kind = "file", detail = ""))
    if (in_library) return(list(value = v, kind = "package", detail = ""))
    bare <- sub("\\($", "", v)
    if (bare %in% R_KEYWORDS) return(list(value = bare, kind = "keyword", detail = ""))
    if (endsWith(v, "(")) {
      # value ships WITHOUT the "(" — the editor decides about parentheses —
      # and the signature rides as the right-hand hint instead.
      f <- if (is_name(bare)) get0(bare, envir = globalenv(), mode = "function")
           else NULL
      return(list(value = bare, kind = "function",
                  detail = if (is.function(f)) paste0("(", formals_string(f), ")")
                           else ""))
    }
    detail <- ""
    if (is_name(v)) {
      o <- get0(v, envir = globalenv())
      if (!is.null(o)) {
        dims <- if (!is.null(dim(o))) paste(dim(o), collapse = " × ")
                else as.character(length(o))
        detail <- paste0(class(o)[1L], " · ", dims)
      }
    }
    list(value = v, kind = "variable", detail = detail)
  }
  emit(list(type = "complete", id = id, start = st$start, end = cursor,
            token = st$token,
            items = lapply(utils::head(st$comps, max_items), describe_item),
            truncated = length(st$comps) > max_items))
}

#' Coerce a wire-supplied count, falling back rather than erroring: a malformed
#' offset from a client must degrade to the default, not kill the pane.
#'
#' @param x Anything the wire delivered.
#' @param default Used when x is absent or not a number.
#' @return A single integer.
as_count <- function(x, default) {
  n <- suppressWarnings(as.integer(x))
  if (length(n) != 1L || is.na(n)) default else n
}

#' Assemble the body of a view reply: true shape, whole-column descriptions,
#' and one row × column window. Shared by `view` and `import`, so the two
#' frames stay the same shape — and the same CAPS — by construction rather
#' than by discipline.
#'
#' A column as searchable text, whatever it holds.
as_search_text <- function(col) {
  tryCatch(as.character(col), error = function(e) rep("", length(col)))
}

# `grepl(..., fixed = TRUE, ignore.case = TRUE)` is NOT case-insensitive:
# R ignores ignore.case whenever fixed is TRUE, and says so in a warning
# that goes nowhere anyone reads. The viewer's search box and its text
# column filters all promised case-insensitive matching in their own
# documentation and all silently required the exact case — searching
# "Cohesion" found nothing in a column of "cohesion".
#
# So the needle is escaped to a literal pattern instead and `fixed` is
# dropped, which keeps every metacharacter inert (`a.c` matches only "a.c",
# `f[1]` does not blow up) while letting PCRE fold case, including for
# non-ASCII: "É" matches "é".
escape_regex <- function(s) gsub("([][{}()*+?.^$|\\\\])", "\\\\\\1", s)
contains_ci <- function(text, needle) {
  grepl(escape_regex(needle), text, ignore.case = TRUE, perl = TRUE)
}

#' One column filter spec → a logical keep-mask. Never evaluates the spec:
#' comparisons and ranges are parsed, everything else is literal text.
#'
#' @param col A column vector.
#' @param spec The user's filter string.
#' @return A logical vector as long as `col`.
match_filter <- function(col, spec) {
  spec <- trimws(as.character(spec)[1L])
  if (!nzchar(spec)) return(rep(TRUE, length(col)))
  if (identical(toupper(spec), "NA")) return(is.na(col))
  if (is.logical(col)) {
    wanted <- toupper(spec)
    if (wanted %in% c("TRUE", "T")) return(!is.na(col) & col)
    if (wanted %in% c("FALSE", "F")) return(!is.na(col) & !col)
  }
  if (is.numeric(col)) {
    range <- strsplit(spec, "\\.\\.", perl = TRUE)[[1L]]
    if (length(range) == 2L) {
      lo <- suppressWarnings(as.numeric(trimws(range[1L])))
      hi <- suppressWarnings(as.numeric(trimws(range[2L])))
      if (!is.na(lo) && !is.na(hi)) return(!is.na(col) & col >= lo & col <= hi)
    }
    parts <- regmatches(spec, regexec("^\\s*(>=|<=|!=|==|=|>|<)\\s*(-?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)\\s*$", spec))[[1L]]
    if (length(parts) == 3L) {
      target <- as.numeric(parts[3L])
      hit <- switch(parts[2L], ">=" = col >= target, "<=" = col <= target,
                    "!=" = col != target, "==" = col == target, "=" = col == target,
                    ">" = col > target, "<" = col < target)
      return(!is.na(hit) & hit)
    }
    target <- suppressWarnings(as.numeric(spec))
    if (!is.na(target)) return(!is.na(col) & col == target)
  }
  text <- as_search_text(col)
  if (startsWith(spec, "=")) return(!is.na(col) & tolower(text) == tolower(substring(spec, 2L)))
  if (startsWith(spec, "!")) return(is.na(col) | !contains_ci(text, substring(spec, 2L)))
  !is.na(col) & contains_ci(text, spec)
}

#' Narrow a frame by the viewer's search box and its per-column filters.
#'
#' Extracted from view_payload so `colstats` can answer about exactly the rows
#' the grid is displaying. A statistics panel that quietly profiled the whole
#' frame while the grid showed a filtered subset would be worse than no panel:
#' both numbers look authoritative and only one answers the question asked.
#'
#' @param obj A data.frame.
#' @param query Free-text search across every column.
#' @param filters Named list of per-column filter specs.
#' @return list(obj, count = filters applied, query = the trimmed query).
view_filter <- function(obj, query = NULL, filters = NULL) {
  count <- 0L
  if (is.list(filters) && length(filters) && length(names(filters))) {
    for (nm in intersect(names(filters), names(obj))) {
      spec <- filters[[nm]]
      if (length(spec) && nzchar(trimws(as.character(spec)[1L]))) {
        obj <- obj[match_filter(obj[[nm]], spec), , drop = FALSE]
        count <- count + 1L
      }
    }
  }
  query <- if (is.character(query) && length(query)) trimws(query[1L]) else ""
  if (nzchar(query) && nrow(obj)) {
    hits <- lapply(obj, function(col) contains_ci(as_search_text(col), query))
    keep <- Reduce(`|`, hits, init = rep(FALSE, nrow(obj)))
    obj <- obj[keep, , drop = FALSE]
  }
  list(obj = obj, count = count, query = query)
}

#' Sorting happens HERE, not in the client: the client only ever holds one
#' page, so a client-side sort would order 200 rows of a million-row frame and
#' call it sorted. It runs on the full frame BEFORE any windowing, so a column
#' window still sees globally ordered rows.
#'
#' The reply always states both what exists (`nrow`/`ncol`) and what it
#' actually contains (`shown`/`shownCols`, effective `limit`/`colLimit`,
#' clamp flags) — nothing downstream should have to guess.
#'
#' @param obj Any object; coerced to data.frame or described as "not a table".
#' @param shown_name Display name echoed in the frame.
#' @param offset Rows to skip before the page (default 0).
#' @param limit Page size (default 200, hard-capped at MAX_VIEW_ROWS).
#' @param sort Column name to order by; unknown names are ignored.
#' @param desc Descending order when TRUE.
#' @param col_offset Columns to skip before the window (default 0).
#' @param col_limit Column window size (default 30, capped at MAX_VIEW_COLS).
#' @param query Case-insensitive text searched across every column.
#' @param filters Named column filter strings. Numeric comparisons and ranges
#'   are interpreted without eval; all other values use text matching.
#' @return Named list of frame fields (no type/id — the caller owns those).
view_payload <- function(obj, shown_name, offset = NULL, limit = NULL,
                         sort = NULL, desc = FALSE,
                         col_offset = NULL, col_limit = NULL,
                         query = NULL, filters = NULL) {
  if (!is.data.frame(obj)) obj <- tryCatch(as.data.frame(obj, stringsAsFactors = FALSE),
                                           error = function(e) NULL)
  if (is.null(obj)) return(list(name = shown_name, error = "not a table"))
  total_rows <- nrow(obj)
  offset <- max(0L, as_count(offset, 0L))
  limit_req <- max(1L, as_count(limit, 200L))
  limit <- min(limit_req, MAX_VIEW_ROWS)
  col_offset <- max(0L, as_count(col_offset, 0L))
  col_limit_req <- max(1L, as_count(col_limit, 30L))
  col_limit <- min(col_limit_req, MAX_VIEW_COLS)

  # Search and per-column filters. Shared with the statistics card, so a
  # profile of "the rows you are looking at" cannot disagree with the rows the
  # grid is actually showing.
  narrowed <- view_filter(obj, query, filters)
  obj <- narrowed$obj
  filter_count <- narrowed$count
  query <- narrowed$query
  if (is.character(sort) && length(sort) == 1L && sort %in% names(obj)) {
    obj <- obj[order(obj[[sort]], decreasing = isTRUE(desc)), , drop = FALSE]
  }
  cidx <- seq.int(from = col_offset + 1L,
                  length.out = max(0L, min(col_limit, ncol(obj) - col_offset)))
  idx <- seq.int(from = offset + 1L,
                 length.out = max(0L, min(limit, nrow(obj) - offset)))
  page <- obj[idx, cidx, drop = FALSE]
  source_names <- names(obj)[cidx]
  display_names <- make.unique(substr(source_names, 1L, MAX_VIEW_LABEL_CHARS))
  names(page) <- display_names
  # Row names would ride along as a `_row` field in the JSON; the offset
  # already says where the page sits, so they are noise.
  rownames(page) <- NULL
  # A single cell can dwarf the row/column caps (logs, embedded documents,
  # accidental blobs). The viewer is a preview, so clip display values before
  # JSON encoding. This also gives a hard upper bound when the page has only
  # one row and row shedding cannot help.
  clipped_cells <- 0L
  page[] <- lapply(page, function(col) {
    if (is.factor(col)) col <- as.character(col)
    if (is.list(col) && !is.data.frame(col)) {
      col <- vapply(col, function(value) {
        tryCatch(as.character(jsonlite::toJSON(value, auto_unbox = TRUE,
                                                null = "null", na = "null")),
                 error = function(e) paste(capture.output(str(value,
                   max.level = 1L, give.attr = FALSE)), collapse = " "))
      }, character(1))
    }
    if (is.character(col)) {
      too_long <- !is.na(col) & nchar(col) > MAX_VIEW_CELL_CHARS
      clipped_cells <<- clipped_cells + sum(too_long)
      col[too_long] <- paste0(substr(col[too_long], 1L, MAX_VIEW_CELL_CHARS), "…")
    }
    col
  })
  # Payload guard: the caps above bound CELLS, not bytes — 500 rows of 20 KB
  # strings is still a 10 MB frame that would stall the socket. Measure the
  # page as it will actually ship and shed rows until it fits, reporting the
  # shrunken window rather than silently serving it.
  page_json <- jsonlite::toJSON(page, auto_unbox = TRUE, null = "null",
                                na = "null", digits = NA)
  bytes <- nchar(page_json, type = "bytes")
  if (bytes > MAX_VIEW_BYTES && nrow(page) > 1L) {
    keep <- max(1L, as.integer(floor(MAX_VIEW_BYTES / (bytes / nrow(page)))))
    if (keep < nrow(page)) {
      page <- page[seq_len(keep), , drop = FALSE]
      limit <- keep
    }
  }
  # `columns` describes the windowed SET only, but each description covers the
  # WHOLE column — the sparkline must show the full distribution, not a page's.
  list(name = shown_name, nrow = nrow(obj), ncol = ncol(obj), totalRows = total_rows,
       filtered = nzchar(query) || filter_count > 0L, filterCount = filter_count,
       offset = offset, limit = limit,
       colOffset = col_offset, colLimit = col_limit,
       limitClamped = limit < limit_req,
       colLimitClamped = col_limit < col_limit_req,
       clippedCells = clipped_cells,
       columns = Map(function(index, display)
         describe_column(obj[[index]], display), cidx, display_names),
       rows = page, shown = nrow(page), shownCols = ncol(page))
}

#' The data viewer: column descriptions plus one row × column window.
#'
#' @param id Request id.
#' @param name Name of an object in the global environment, or an expression.
#' @param offset,limit,sort,desc,col_offset,col_limit Windowing and ordering —
#'   see view_payload.
#' @param label Display name when it differs from the fetch name (View()).
emit_view <- function(id, name, offset = NULL, limit = NULL, sort = NULL,
                      desc = FALSE, col_offset = NULL, col_limit = NULL,
                      label = NULL, query = NULL, filters = NULL) {
  shown <- if (is.null(label)) name else label
  obj <- tryCatch(eval(parse(text = name), globalenv()), error = function(e) NULL)
  if (is.null(obj)) {
    emit(list(type = "view", id = id, name = shown, error = "not found"))
    return(invisible(NULL))
  }
  # A Stop pressed mid-sort of a big frame must cancel the page, not the worker.
  tryCatch(
    emit(c(list(type = "view", id = id),
           view_payload(obj, shown, offset, limit, sort, desc,
                        col_offset, col_limit, query, filters))),
    interrupt = function(i) emit(list(type = "view", id = id, name = shown,
                                      error = "interrupted"))
  )
}

#' Open the OPERATING SYSTEM's file dialog and report what was chosen.
#'
#' The browser cannot do this — `<input type="file">` opens the real picker but
#' withholds the path, and the kernel needs a path. R is already running on the
#' user's own machine, so R opens the dialog.
#'
#' The worker BLOCKS while the dialog is up, which is correct: the session
#' cannot run a cell whose file has not been chosen yet, and the supervisor
#' stays responsive throughout because it is a different process. A dialog
#' left open all afternoon hits CHOOSER_TIMEOUT rather than wedging R.
#'
#' @param id Request id.
#' @param mode "file", "dir" or "save" — save shows the platform's save sheet
#'   and returns a path that need not exist yet.
#' @param start Directory to open in; defaults to the working directory.
#' @param prompt Dialog title.
#' @param default_name Save mode's suggested file name; basename() is taken,
#'   so a client cannot smuggle directories through it.
#' @param probe_only Report support without opening a dialog. Older clients
#'   discover capabilities this way; ignoring it consumes the user's first
#'   file choice as a probe and makes the second attempt appear to work.
#' @return Invisibly NULL. Emits one `choose` frame carrying exactly one of
#'   `path`, `cancelled`, `unsupported` or `error`.
emit_choose <- function(id, mode = "file", start = NULL, prompt = NULL,
                        probe_only = FALSE, default_name = NULL) {
  # The honest opt-out. A test harness (or any deployment where a dialog
  # would open on a screen nobody is watching) sets CARMAR_NO_NATIVE_DIALOG
  # and this kernel simply reports "no dialog here" — probes included — so
  # every client falls back to its own picker, which is the same contract a
  # machine with no desktop follows.
  if (nzchar(Sys.getenv("CARMAR_NO_NATIVE_DIALOG"))) {
    emit(list(type = "choose", id = id, unsupported = TRUE))
    return(invisible(NULL))
  }
  if (!exists("choose_path", inherits = TRUE)) {
    emit(list(type = "choose", id = id, unsupported = TRUE))
    return(invisible(NULL))
  }
  if (isTRUE(probe_only)) {
    # `modes` is how a client discovers save-sheet support without a second
    # verb: kernels older than the save mode omit the field, and the client
    # reads its absence as file/dir only.
    emit(list(type = "choose", id = id, supported = TRUE,
              modes = I(c("file", "dir", "save"))))
    return(invisible(NULL))
  }
  mode <- if (is.character(mode) && length(mode) == 1L &&
              mode %in% c("dir", "save")) mode else "file"
  begin <- if (is.character(start) && length(start) == 1L && nzchar(start)) {
    path.expand(start)
  } else getwd()
  # A confined deployment must not be handed a dialog rooted outside its
  # subtree — and whatever comes back is checked again below, because the
  # dialog itself cannot be constrained.
  if (!within_root(begin)) begin <- confine_root
  nm <- if (is.character(default_name) && length(default_name) == 1L &&
            nzchar(default_name)) basename(default_name) else NULL
  res <- choose_path(mode, begin,
                     if (is.character(prompt) && nzchar(prompt)) prompt else "Choose a file",
                     default_name = nm)
  if (!is.null(res$path) && !within_root(res$path)) {
    emit(list(type = "choose", id = id, error = outside_root_msg()))
    return(invisible(NULL))
  }
  emit(c(list(type = "choose", id = id), res))
}

#' Describe a file WITHOUT reading it into the session.
#'
#' The import wizard's eyes. Answers with the detected format, the delimited
#' settings, a per-column type guess (including the date format, the timezone
#' and any day/month ambiguity) and a text preview — but assigns nothing and
#' changes nothing. Inspecting a file the user has not yet agreed to import
#' must not put an object in their environment, and re-inspecting it under
#' different settings must not put twenty.
#'
#' Every failure is an `error` FIELD rather than a thrown condition: pointing
#' the wizard at a 2 GB binary is a normal thing to do by accident, and it
#' must cost a message, not the session.
#'
#' @param id Request id.
#' @param path File to inspect; `~` is expanded.
#' @param opts Overrides from the wizard (delim, quote, encoding, header,
#'   skip, sheet, naStrings, tz). Anything absent is detected.
#' @return Invisibly NULL. Emits one `sniff` frame.
emit_sniff <- function(id, path = NULL, opts = NULL) {
  fail <- function(msg) {
    emit(list(type = "sniff", id = id, path = path, error = msg))
    invisible(NULL)
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) return(fail("no path"))
  # A URL is a source too. The confinement root governs the FILESYSTEM, and a
  # remote read touches none of it — but a confined deployment is confined on
  # purpose, so remote sources are refused there rather than quietly allowed.
  remote <- exists("is_url", inherits = TRUE) && is_url(path)
  if (remote) {
    if (!is.null(confine_root)) return(fail("remote sources are disabled in this deployment"))
    p <- path
  } else {
    p <- path.expand(path)
    if (!within_root(p)) return(fail(outside_root_msg()))
    if (!file.exists(p) || dir.exists(p)) return(fail("not a readable file"))
  }
  if (!exists("sniff_file", inherits = TRUE)) {
    return(fail("this kernel is too old for the import wizard - restart CarmaR"))
  }
  res <- tryCatch(
    sniff_file(p, if (is.list(opts)) opts else list()),
    error = function(e) structure(class = "carmar_fail", list(msg = conditionMessage(e))),
    interrupt = function(i) structure(class = "carmar_fail", list(msg = "interrupted"))
  )
  if (inherits(res, "carmar_fail")) return(fail(res$msg))
  emit(c(list(type = "sniff", id = id), res))
}

#' Read a file into the session and answer with a first view of it.
#'
#' rio when installed (one verb, ~30 formats); otherwise the extension picks a
#' base/readxl reader. Every failure — missing file, bad format, absent readxl,
#' a Stop mid-read — comes back as an `error` field, because a bad file must
#' never cost the session.
#'
#' @param id Request id.
#' @param path File to read; `~` is expanded.
#' @param name Name to assign; defaults to the file name, made syntactic.
#' @return Invisibly NULL. Emits one `import` frame.
emit_import <- function(id, path = NULL, name = NULL) {
  fail <- function(msg) {
    emit(list(type = "import", id = id, path = path, error = msg))
    invisible(NULL)
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) return(fail("no path"))
  p <- path.expand(path)
  if (!within_root(p)) return(fail(outside_root_msg()))
  if (!file.exists(p) || dir.exists(p)) return(fail("not a readable file"))
  read_by_ext <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is not installed")
      return(as.data.frame(readxl::read_excel(f)))
    }
    switch(ext,
           csv = utils::read.csv(f, stringsAsFactors = FALSE),
           tsv = utils::read.delim(f, stringsAsFactors = FALSE),
           txt = utils::read.delim(f, stringsAsFactors = FALSE),
           rds = readRDS(f),
           stop("unsupported file type: .", ext))
  }
  obj <- tryCatch(
    if (requireNamespace("rio", quietly = TRUE)) rio::import(p) else read_by_ext(p),
    error = function(e) structure(class = "carmar_fail", list(msg = conditionMessage(e))),
    interrupt = function(i) structure(class = "carmar_fail", list(msg = "interrupted"))
  )
  if (inherits(obj, "carmar_fail")) return(fail(obj$msg))
  nm <- if (is.character(name) && length(name) == 1L && nzchar(name)) name
        else make.names(tools::file_path_sans_ext(basename(p)))
  assign(nm, obj, envir = globalenv())
  # The reply is view-shaped so the client can open the viewer straight from
  # it. An RDS holding a model still assigns; the payload then says "not a
  # table" and the obj inspector is the right next stop.
  emit(c(list(type = "import", id = id, assigned = nm, path = p),
         view_payload(obj, nm)))
}

#' Remove objects — the Environment pane's broom, and single-object delete.
#'
#' @param id Request id.
#' @param names Character vector, or NULL for everything.
emit_rm <- function(id, names = NULL) {
  target <- if (is.null(names) || !length(names)) ls(globalenv(), all.names = TRUE) else names
  removed <- intersect(target, ls(globalenv(), all.names = TRUE))
  if (length(removed)) rm(list = removed, envir = globalenv())
  emit(list(type = "removed", id = id, names = as.list(removed),
            remaining = length(ls(globalenv(), all.names = FALSE))))
}

#' Is this source a complete R expression?
#'
#' The console needs R's own parser to decide, not a brace counter: `f(1,` and
#' `"unterminated` and `x +` are all incomplete for different reasons, and only
#' the parser knows which. Drives the continuation prompt.
#'
#' @param id Request id.
#' @param source Source text.
#' @return Invisibly NULL. Emits one `parse` frame.
emit_parse <- function(id, source) {
  complete <- TRUE
  message_ <- NULL
  tryCatch(
    parse(text = source),
    error = function(e) {
      msg <- conditionMessage(e)
      # R says "unexpected end of input" only when more input would help.
      complete <<- !grepl("unexpected end of input|unexpected INCOMPLETE_STRING", msg)
      message_ <<- msg
    }
  )
  emit(list(type = "parse", id = id, complete = complete, message = message_))
}

#' Reformat R source with a real formatter, if the session has one.
#'
#' "Beautify" through a language model is a guess dressed as a tool: a cautious
#' model hands back what you gave it, which reads as the feature being broken.
#' Formatting is a solved, deterministic problem — styler does it exactly, for
#' free, offline. The source arrives as a JSON string, so nothing is escaped
#' into a literal and no code is constructed from text.
#'
#' `available = FALSE` is not an error: it tells the caller to fall back.
emit_format <- function(id, source) {
  txt <- if (is.character(source)) paste(source, collapse = "\n") else ""
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  engine <- if (requireNamespace("styler", quietly = TRUE)) "styler"
            else if (requireNamespace("formatR", quietly = TRUE)) "formatR"
            else NA_character_
  if (is.na(engine)) {
    emit(list(type = "format", id = id, available = FALSE))
    return(invisible(NULL))
  }
  out <- tryCatch({
    if (identical(engine, "styler")) {
      paste(as.character(styler::style_text(lines)), collapse = "\n")
    } else {
      paste(formatR::tidy_source(text = lines, output = FALSE, comment = TRUE,
                                 arrow = TRUE, width.cutoff = 80L)$text.tidy,
            collapse = "\n")
    }
  }, error = function(e) NULL)
  if (is.null(out)) {
    # Unparseable code is the user's, not the formatter's, problem to report.
    emit(list(type = "format", id = id, available = TRUE, engine = engine,
              error = "this code could not be parsed, so it was left alone"))
  } else {
    emit(list(type = "format", id = id, available = TRUE, engine = engine, text = out))
  }
}

#' Loaded namespaces by default; the larger installed inventory is opt-in.
emit_packages <- function(id, scope = "loaded") {
  scope <- if (is.character(scope) && length(scope) == 1L &&
               identical(scope, "installed")) "installed" else "loaded"
  loaded <- loadedNamespaces()
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  rss_mb <- tryCatch({
    if (!identical(.Platform$OS.type, "unix")) NA_real_ else {
      kb <- suppressWarnings(as.numeric(trimws(system2(
        "ps", c("-o", "rss=", "-p", as.character(Sys.getpid())),
        stdout = TRUE, stderr = FALSE
      ))[1L]))
      if (is.finite(kb)) round(kb / 1024, 1L) else NA_real_
    }
  }, error = function(e) NA_real_)
  record <- function(name, version = NULL, lib = NULL, priority = NULL) {
    if (is.null(lib)) {
      path <- find.package(name, quiet = TRUE)
      lib <- if (nzchar(path)) dirname(path) else ""
    }
    if (is.null(version)) {
      version <- tryCatch(as.character(utils::packageVersion(name)), error = function(e) "")
    }
    if (is.null(priority)) {
      priority <- tryCatch(utils::packageDescription(name, fields = "Priority"),
                           error = function(e) "")
    }
    priority <- if (length(priority) && !is.na(priority[1L])) as.character(priority[1L]) else ""
    lib_norm <- normalizePath(lib, winslash = "/", mustWork = FALSE)
    protected <- identical(priority, "base")
    is_attached <- name %in% attached
    list(name = name, version = as.character(version), lib = lib,
         loaded = is_attached, attached = is_attached,
         namespaceLoaded = name %in% loaded,
         writable = nzchar(lib) && file.access(lib, 2L) == 0L,
         protected = protected, priority = priority)
  }
  if (identical(scope, "installed")) {
    inst <- utils::installed.packages()[, c("Package", "Version", "LibPath", "Priority"), drop = FALSE]
    ord <- order(!inst[, "Package"] %in% attached,
                 !inst[, "Package"] %in% loaded,
                 tolower(inst[, "Package"]))
    inst <- inst[ord, , drop = FALSE]
    packages <- lapply(seq_len(nrow(inst)), function(i) record(
      unname(inst[i, "Package"]), unname(inst[i, "Version"]),
      unname(inst[i, "LibPath"]), unname(inst[i, "Priority"])
    ))
  } else {
    loaded <- loaded[order(!loaded %in% attached, tolower(loaded))]
    packages <- lapply(loaded, record)
  }
  emit(list(type = "packages", id = id, scope = scope,
            memoryMb = rss_mb,
            packages = packages))
}

#' Apply one explicit package operation and report its result to the pane.
emit_package_action <- function(id, action, name, lib = NULL) {
  valid_name <- is.character(name) && length(name) == 1L &&
                grepl("^[A-Za-z][A-Za-z0-9.]*$", name)
  if (!valid_name) {
    emit(list(type = "package_action", id = id, error = "invalid package name"))
    return(invisible(NULL))
  }
  action <- if (is.character(action) && length(action) == 1L) action else ""
  allowed <- c("load", "attach", "detach", "unload", "install", "update", "remove")
  if (!action %in% allowed) {
    emit(list(type = "package_action", id = id, error = "unknown package action"))
    return(invisible(NULL))
  }

  perform <- function() {
    attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
    package_path <- find.package(name, quiet = TRUE)
    installed <- length(package_path) == 1L && nzchar(package_path)
    target_lib <- if (installed) dirname(package_path) else ""
    if (is.character(lib) && length(lib) == 1L && nzchar(lib)) {
      requested <- normalizePath(lib, winslash = "/", mustWork = FALSE)
      known <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
      if (!requested %in% known) stop("the selected package library is not active in this R session")
      target_lib <- lib
    }
    priority <- if (installed) tryCatch(
      utils::packageDescription(name, lib.loc = target_lib, fields = "Priority"),
      error = function(e) "") else ""
    protected <- installed && identical(unname(priority), "base")

    if (action %in% c("detach", "unload", "update", "remove") && protected) {
      stop("R system packages are protected")
    }
    if (identical(action, "load")) {
      loadNamespace(name)
      return(paste(name, "namespace loaded"))
    }
    if (identical(action, "attach")) {
      suppressPackageStartupMessages(library(name, character.only = TRUE))
      return(paste(name, "attached to the search path"))
    }
    if (identical(action, "detach")) {
      if (!name %in% attached) return(paste(name, "is not attached"))
      detach(paste0("package:", name), character.only = TRUE, unload = FALSE)
      return(paste(name, "detached; its namespace remains loaded"))
    }
    if (identical(action, "unload")) {
      was_attached <- name %in% attached
      if (was_attached) detach(paste0("package:", name), character.only = TRUE, unload = FALSE)
      if (!name %in% loadedNamespaces()) return(paste(name, "namespace is not loaded"))
      tryCatch(unloadNamespace(name), error = function(e) {
        if (was_attached) suppressPackageStartupMessages(library(name, character.only = TRUE))
        stop(e)
      })
      return(paste(name, if (was_attached) "detached and unloaded" else "namespace unloaded"))
    }
    if (identical(action, "remove")) {
      if (!installed) return(paste(name, "is not installed"))
      if (file.access(target_lib, 2L) != 0L) stop("the package library is not writable")
      was_attached <- name %in% attached
      was_loaded <- name %in% loadedNamespaces()
      tryCatch({
        if (was_attached) detach(paste0("package:", name), character.only = TRUE, unload = FALSE)
        if (name %in% loadedNamespaces()) unloadNamespace(name)
        utils::remove.packages(name, lib = target_lib)
      }, error = function(e) {
        try(if (was_attached) suppressPackageStartupMessages(library(name, character.only = TRUE))
            else if (was_loaded) loadNamespace(name), silent = TRUE)
        stop(e)
      })
      return(paste(name, "removed from", target_lib))
    }

    repos <- getOption("repos")
    if (!length(repos) || is.na(repos["CRAN"]) || identical(unname(repos["CRAN"]), "@CRAN@")) {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    if (identical(action, "install")) {
      if (installed && protected) stop("R system packages are protected")
      writable <- .libPaths()[file.access(.libPaths(), 2L) == 0L]
      if (!length(writable)) stop("this R session has no writable package library")
      target_lib <- writable[1L]
      suppressWarnings(utils::install.packages(name, lib = target_lib, repos = repos,
                                                dependencies = NA, quiet = TRUE))
      if (!requireNamespace(name, quietly = TRUE)) stop("installation did not produce a loadable package")
      return(paste(name, as.character(utils::packageVersion(name)), "installed"))
    }
    if (!installed) stop("the package is not installed")
    if (file.access(target_lib, 2L) != 0L) stop("the package library is not writable")
    available <- utils::available.packages(repos = repos)
    if (!name %in% rownames(available)) stop("the package is not available from the configured repositories")
    current <- utils::packageVersion(name, lib.loc = target_lib)
    latest <- numeric_version(available[name, "Version"])
    if (current >= latest) return(paste(name, as.character(current), "is current"))
    was_attached <- name %in% attached
    was_loaded <- name %in% loadedNamespaces()
    updated <- tryCatch({
      if (was_attached) detach(paste0("package:", name), character.only = TRUE, unload = FALSE)
      if (name %in% loadedNamespaces()) unloadNamespace(name)
      suppressWarnings(utils::install.packages(name, lib = target_lib, repos = repos,
                                                dependencies = NA, quiet = TRUE))
      installed_version <- utils::packageVersion(name, lib.loc = target_lib)
      if (installed_version < latest) stop("the package update did not complete")
      if (was_attached) suppressPackageStartupMessages(library(name, character.only = TRUE))
      else if (was_loaded) loadNamespace(name)
      installed_version
    }, error = function(e) {
      try(if (was_attached) suppressPackageStartupMessages(library(name, character.only = TRUE))
          else if (was_loaded) loadNamespace(name), silent = TRUE)
      stop(e)
    })
    paste(name, as.character(updated), "updated")
  }

  result <- tryCatch(list(message = perform()), error = function(e) list(error = conditionMessage(e)))
  emit(c(list(type = "package_action", id = id, action = action, name = name), result))
}

#' Package metadata and documentation index for the Help pane.
emit_package_help <- function(id, name) {
  valid_name <- is.character(name) && length(name) == 1L &&
                grepl("^[A-Za-z][A-Za-z0-9.]*$", name)
  if (!valid_name) {
    emit(list(type = "package_help", id = id, error = "invalid package name"))
    return(invisible(NULL))
  }
  result <- tryCatch({
    desc <- utils::packageDescription(name)
    index <- do.call(utils::help, list(package = name))
    table <- tryCatch(index$info[[2L]], error = function(e) NULL)
    topics <- if (is.null(table) || !NROW(table)) list() else {
      columns <- colnames(table)
      item_col <- intersect(c("Item", "Topic"), columns)[1L]
      title_col <- intersect(c("Title", "Description"), columns)[1L]
      if (is.na(item_col)) list() else lapply(seq_len(NROW(table)), function(i) {
        list(topic = unname(table[i, item_col]),
             title = if (is.na(title_col)) "" else unname(table[i, title_col]))
      })
    }
    list(title = unname(desc[["Title"]] %||% name),
         version = unname(desc[["Version"]] %||% ""),
         description = unname(desc[["Description"]] %||% ""),
         license = unname(desc[["License"]] %||% ""),
         topics = topics)
  }, error = function(e) list(error = conditionMessage(e)))
  emit(c(list(type = "package_help", id = id, name = name), result))
}

#' R's own help page, rendered to HTML, for the Help pane.
emit_help <- function(id, topic) {
  html <- tryCatch({
    # `stats::sd` is a qualified NAME, not a help topic — help() finds nothing
    # for it, so F1 on a namespaced call answered "no help found" for a
    # function whose page plainly exists. Split it and name the package.
    args <- if (is.character(topic) && length(topic) == 1L &&
                grepl("^[A-Za-z.][A-Za-z0-9._]*:::?[A-Za-z.][A-Za-z0-9._]*$", topic)) {
      parts <- strsplit(topic, ":::?")[[1]]
      list(parts[2L], package = parts[1L], help_type = "text")
    } else {
      list(topic, help_type = "text", try.all.packages = TRUE)
    }
    # help() substitutes its argument, so `help(topic)` with `topic` holding
    # "lm" looks up a topic literally called "topic". do.call passes the value.
    paths <- do.call(utils::help, args)
    if (length(paths) == 0L) return(NULL)
    # .getHelpFile is internal and not exported in every R (it is not in 4.6.1),
    # so the Rd path is attempted and the rendered TEXT help is the fallback.
    # A Help pane showing R's own text beats a Help pane showing nothing.
    rd <- tryCatch(utils:::.getHelpFile(paths[1L]), error = function(e) NULL)
    if (!is.null(rd)) {
      paste(utils::capture.output(tools::Rd2HTML(rd)), collapse = "\n")
    } else {
      txt <- paste(utils::capture.output(print(paths)), collapse = "\n")
      if (!nzchar(trimws(txt))) NULL
      else paste0("<pre class=\"carmar-help-text\">",
                  gsub("<", "&lt;", gsub("&", "&amp;", txt), fixed = TRUE), "</pre>")
    }
  }, error = function(e) NULL)
  emit(list(type = "help", id = id, topic = topic, html = html))
}

#' What to show when the pointer rests on a name — RStudio's F1, without F1.
#'
#' Three things, in the order a reader wants them: what it IS (a function and
#' its signature, or a value and its shape), what it is FOR (the help page's
#' title), and one paragraph of description. Deliberately not the whole help
#' page: a tooltip that fills the screen is a worse Help pane, and the Help
#' pane already exists.
#'
#' Everything is best-effort and every failure is silent — a hover that throws,
#' or that blocks on a slow help lookup, is worse than a hover that says
#' nothing. `name` may be namespaced (`dplyr::filter`).
#'
#' @param id Request id.
#' @param name Symbol to describe.
#' @return Invisibly NULL. Emits one `hover` frame.
emit_hover <- function(id, name) {
  none <- function() {
    emit(list(type = "hover", id = id, name = name, found = FALSE))
    invisible(NULL)
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) return(none())
  if (!grepl("^[A-Za-z._][A-Za-z0-9._]*(:::?[A-Za-z._][A-Za-z0-9._]*)?$", name)) return(none())

  parts <- strsplit(name, ":::?")[[1]]
  pkg <- if (length(parts) == 2L) parts[1L] else NULL
  sym <- parts[length(parts)]

  # inherits = FALSE, and it matters: get() walks the enclosing environments by
  # default, so `mean` is "found in globalenv" and every base function gets
  # labelled .GlobalEnv. Only what the USER bound counts as theirs.
  obj <- tryCatch({
    if (!is.null(pkg)) getExportedValue(pkg, sym)
    else get(sym, envir = globalenv(), inherits = FALSE)
  }, error = function(e) NULL)
  # Not a user object: look along the search path (base, attached packages).
  where <- NULL
  if (is.null(obj)) {
    obj <- tryCatch(get(sym), error = function(e) NULL)
    if (!is.null(obj)) {
      w <- tryCatch(find(sym)[1L], error = function(e) NA_character_)
      if (!is.na(w)) where <- sub("^package:", "", w)
    }
  } else if (!is.null(pkg)) where <- pkg
  else where <- ".GlobalEnv"

  if (is.null(obj)) {
    # A name that is not bound can still have a help page (an S4 generic, a
    # dataset promise); if it has none either, there is nothing to say.
    doc <- help_summary(sym, pkg)
    if (is.null(doc$title) && is.null(doc$description)) return(none())
    emit(c(list(type = "hover", id = id, name = name, found = TRUE,
                kind = "topic", package = where), doc))
    return(invisible(NULL))
  }

  kind <- if (is.function(obj)) "function" else "value"
  signature <- NULL
  detail <- NULL
  if (is.function(obj)) {
    signature <- tryCatch({
      txt <- paste(utils::capture.output(print(args(obj))), collapse = " ")
      txt <- sub("^function\\s*", paste0(sym, " "), txt)
      txt <- sub("\\s*NULL\\s*$", "", txt)
      trimws(gsub("\\s+", " ", txt))
    }, error = function(e) NULL)
    if (!is.null(signature) && nchar(signature) > 300L) {
      signature <- paste0(substr(signature, 1L, 297L), "...")
    }
  } else {
    detail <- tryCatch({
      cls <- paste(class(obj), collapse = "/")
      dims <- if (!is.null(dim(obj))) paste(dim(obj), collapse = " x ")
              else if (is.atomic(obj) || is.list(obj)) paste0("length ", length(obj))
              else NULL
      paste(c(cls, dims), collapse = " · ")
    }, error = function(e) NULL)
  }

  doc <- help_summary(sym, pkg)
  # Drop the NULLs before they ship. `list(detail = NULL)` survives as a NULL
  # element and jsonlite renders it as `{}` — which arrived in the tooltip as
  # the string "[object Object]". An absent field must be absent.
  frame <- list(type = "hover", id = id, name = name, found = TRUE, kind = kind,
                package = where, signature = signature, detail = detail)
  frame <- c(frame, doc)
  emit(Filter(Negate(is.null), frame))
  invisible(NULL)
}

#' Title and first description paragraph from a help topic, or NULLs.
#'
#' Parsed out of R's own TEXT rendering rather than the Rd tree: the internal
#' that reads an .Rd file (`utils:::.getHelpFile`) is not exported and is not
#' present in every R, so a tooltip built on it would work on one machine and
#' not the next. The text layout — a header line, the title, then a
#' "Description:" block — has been stable for decades.
help_summary <- function(sym, pkg = NULL) {
  empty <- list(title = NULL, description = NULL)
  txt <- tryCatch({
    paths <- if (is.null(pkg)) do.call(utils::help, list(sym, help_type = "text"))
             else do.call(utils::help, list(sym, package = pkg, help_type = "text"))
    if (length(paths) == 0L) return(empty)
    utils::capture.output(tools::Rd2txt(utils:::.getHelpFile(paths[1L]),
                                        options = list(underline_titles = FALSE)))
  }, error = function(e) NULL)
  if (is.null(txt) || !length(txt)) return(empty)

  lines <- trimws(txt)
  # The first non-empty line after the "name package:pkg R Documentation"
  # header is the title.
  start <- which(grepl("R Documentation", txt, fixed = TRUE))
  idx <- if (length(start)) start[1L] + 1L else 1L
  while (idx <= length(lines) && !nzchar(lines[idx])) idx <- idx + 1L
  title <- if (idx <= length(lines)) lines[idx] else NULL

  desc <- NULL
  d <- which(grepl("^Description:", lines))
  if (length(d)) {
    i <- d[1L] + 1L
    while (i <= length(lines) && !nzchar(lines[i])) i <- i + 1L
    para <- character()
    while (i <= length(lines) && nzchar(lines[i])) { para <- c(para, lines[i]); i <- i + 1L }
    if (length(para)) desc <- paste(para, collapse = " ")
  }
  if (!is.null(desc) && nchar(desc) > 400L) desc <- paste0(substr(desc, 1L, 397L), "...")
  list(title = title, description = desc)
}

#' Working directory, and moving it — the Files pane's anchor.
emit_wd <- function(id, path = NULL) {
  if (is.character(path) && length(path) == 1L && nzchar(path)) {
    if (!within_root(path)) {
      emit(list(type = "stream", id = id, kind = "warning", text = outside_root_msg()))
    } else {
      tryCatch(setwd(path.expand(path)), error = function(e)
        emit(list(type = "stream", id = id, kind = "warning", text = conditionMessage(e))))
    }
  }
  emit(list(type = "wd", id = id, path = getwd()))
}

#' List a directory for the Files pane.
#'
#' Directories first because that is how every file browser reads. A bad path
#' answers with an `error` FIELD: the pane holding a stale bookmark must see
#' "not a directory", not take the worker down.
#'
#' @param id Request id.
#' @param path Directory to list; NULL or "" means the working directory. `~`
#'   is expanded.
#' @param all TRUE also lists dotfiles (never `.` / `..`). Off by default —
#'   the same default `list.files()` has — and a client-side toggle.
#' @return Invisibly NULL. Emits one `files` frame.
emit_files <- function(id, path = NULL, all = FALSE) {
  p <- if (is.null(path) || !is.character(path) || !nzchar(path[1L])) getwd()
       else path.expand(path[1L])
  p <- normalizePath(p, mustWork = FALSE)
  if (!within_root(p)) {
    emit(list(type = "files", id = id, path = p, error = outside_root_msg()))
    return(invisible(NULL))
  }
  if (!dir.exists(p)) {
    emit(list(type = "files", id = id, path = p, error = "not a directory"))
    return(invisible(NULL))
  }
  if (file.access(p, mode = 4L) != 0L) {
    emit(list(type = "files", id = id, path = p, error = "not readable"))
    return(invisible(NULL))
  }
  nms <- list.files(p, all.files = isTRUE(all), no.. = TRUE)
  info <- file.info(file.path(p, nms))
  isdir <- info$isdir %in% TRUE          # a broken symlink reports NA
  ord <- order(!isdir, tolower(nms))
  entries <- lapply(ord, function(i) list(
    name = nms[i], size = as.numeric(info$size[i]),
    mtime = as.numeric(info$mtime[i]), isdir = isdir[i]))
  emit(list(type = "files", id = id, path = p, parent = dirname(p),
            entries = entries))
}

#' Read a text file — the script editor's Open.
#'
#' Text only, and capped: the editor is for .R scripts, and handing a 400 MB
#' CSV to a textarea is not an editing session, it is a hung tab. A binary
#' file is refused by the NUL byte rather than rendered as mojibake.
#'
#' @param id Request id.
#' @param path File to read; `~` is expanded.
#' @return Invisibly NULL. Emits one `readfile` frame with `text`, or `error`.
MAX_TEXT_BYTES <- 4e6
emit_readfile <- function(id, path = NULL) {
  fail <- function(msg) {
    emit(list(type = "readfile", id = id, path = path, error = msg))
    invisible(NULL)
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) return(fail("no path"))
  p <- path.expand(path)
  if (!within_root(p)) return(fail(outside_root_msg()))
  if (!file.exists(p) || dir.exists(p)) return(fail("not a readable file"))
  size <- file.info(p)$size
  if (isTRUE(size > MAX_TEXT_BYTES)) {
    return(fail(sprintf("too large to edit (%.1f MB)", size / 1e6)))
  }
  raw_bytes <- tryCatch(readBin(p, "raw", n = size),
                        error = function(e) conditionMessage(e))
  if (is.character(raw_bytes)) return(fail(raw_bytes))
  if (any(raw_bytes == as.raw(0L))) return(fail("not a text file"))
  text <- tryCatch(rawToChar(raw_bytes), error = function(e) NULL)
  if (is.null(text)) return(fail("could not decode as text"))
  Encoding(text) <- "UTF-8"
  emit(list(type = "readfile", id = id, path = normalizePath(p), text = text))
}

#' Write a text file — the script editor's Save.
#'
#' Writes exactly the bytes given, with no trailing-newline politics beyond the
#' one every POSIX text file ends with. The directory must already exist: a Save
#' that silently creates a tree is a Save that puts the file somewhere else than
#' the user believes.
#'
#' @param id Request id.
#' @param path Destination; `~` is expanded.
#' @param text Contents.
#' @return Invisibly NULL. Emits one `writefile` frame with `path`, or `error`.
emit_writefile <- function(id, path = NULL, text = "") {
  fail <- function(msg) {
    emit(list(type = "writefile", id = id, path = path, error = msg))
    invisible(NULL)
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) return(fail("no path"))
  p <- path.expand(path)
  if (!within_root(p)) return(fail(outside_root_msg()))
  if (dir.exists(p)) return(fail("that is a directory"))
  if (!dir.exists(dirname(p))) return(fail(paste0("no such folder: ", dirname(p))))
  if (!is.null(text) && !is.character(text)) return(fail("text must be a string"))
  if (is.character(text) && sum(nchar(text, type = "bytes")) > MAX_TEXT_BYTES) {
    return(fail("too large to save from the editor"))
  }
  body <- if (is.character(text) && length(text)) paste(text, collapse = "\n") else ""
  if (!grepl("\n$", body)) body <- paste0(body, "\n")
  ok <- tryCatch({
    con <- file(p, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(enc2utf8(body)), con)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (is.character(ok)) return(fail(ok))
  emit(list(type = "writefile", id = id, path = normalizePath(p),
            bytes = nchar(body, type = "bytes")))
}

#' Open a fresh PNG device whose pages land in `dir`.
#'
#' `%03d` in the filename makes R write one file per page, so a cell that draws
#' three figures yields three files with no bookkeeping here.
#'
#' @param dir Directory to write pages into.
#' @param seq Index of the top-level expression, to keep filenames unique.
#' @return Invisibly NULL.
open_plot_device <- function(dir, seq, dims = NULL) {
  width  <- if (!is.null(dims$width))  dims$width  else PLOT_WIDTH
  height <- if (!is.null(dims$height)) dims$height else PLOT_HEIGHT
  res    <- if (!is.null(dims$res))    dims$res    else PLOT_RES
  if (plot_is_svg(dims)) {
    # A vector device is sized in INCHES and has no resolution at all — which
    # is the whole point: its cost does not grow with dpi. The client sends
    # pixels because that is what a raster device wants, so convert back.
    file <- file.path(dir, sprintf("e%03d-%%03d.svg", seq))
    if (identical(SVG_DEVICE, "svglite")) {
      svglite::svglite(file, width = width / res, height = height / res)
    } else {
      grDevices::svg(file, width = width / res, height = height / res)
    }
    return(invisible(NULL))
  }
  grDevices::png(
    filename = file.path(dir, sprintf("e%03d-%%03d.png", seq)),
    width = width, height = height, res = res,
    type = PLOT_TYPE
  )
  invisible(NULL)
}

#' The file extension the open device is writing, as a regex.
#'
#' Harvesting used to hard-code `\\.png$`, so an SVG device produced pages
#' that were never collected and the chunk finished with no plots at all.
plot_pattern <- function(dims) if (plot_is_svg(dims)) "\\.svg$" else "\\.png$"

#' Emit one `plot` frame for a finished page file.
#'
#' Report the size the device was ACTUALLY opened at. Reporting the defaults
#' while honouring the request is worse than ignoring the request: the client
#' lays out a 900×620 box around a 1400×500 image.
#' `res` travels with the image because the client cannot infer it: a
#' 1400×900 PNG at res 96 and the same PNG at res 192 are the same pixels
#' describing different physical sizes, and the viewer needs the second
#' number to display it at its true size instead of upscaling it.
#'
#' @param id Cell id.
#' @param f Path to a finalized PNG page.
#' @return Invisibly NULL.
emit_plot_frame <- function(id, f, dims = NULL) {
  # SVG travels base64 too, in the same envelope as PNG. It costs 33% and buys
  # a zero-line client change: lib/output-pane.js already understands
  # `image/svg+xml` (isSvg — vector Save SVG, rasterise-on-demand) and already
  # builds `data:<mime>;base64,<data>`. Sending raw text instead would have
  # meant an encoding field plus edits to output-pane.js AND knit.js, and an
  # older client would have rendered nothing at all.
  svg <- grepl("\\.svg$", f)
  emit(list(type = "plot", id = id, mime = if (svg) "image/svg+xml" else "image/png",
            width  = if (!is.null(dims$width))  dims$width  else PLOT_WIDTH,
            height = if (!is.null(dims$height)) dims$height else PLOT_HEIGHT,
            res    = if (!is.null(dims$res))    dims$res    else PLOT_RES,
            data = jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))))
  invisible(NULL)
}

#' Emit pages the device has finished, leaving the device open.
#'
#' The `%03d` png device finalizes a page's file only when the NEXT page
#' begins (or the device closes), so every file on disk is a complete figure
#' and the page still open on the device has no file yet. That page stays
#' amendable: `abline()` after `seq_heatmap()` draws onto a live plot instead
#' of erroring, and `layout()`/`par()` state survives across statements.
#'
#' @param id Cell id.
#' @param dir Directory the device writes pages into.
#' @param seen Character vector of filenames already emitted.
#' @return The updated `seen` vector.
harvest_finished <- function(id, dir, seen, dims = NULL) {
  files <- list.files(dir, pattern = plot_pattern(dims), full.names = TRUE)
  # Only files with bytes join `seen`: a file still empty here gets another
  # look on the next harvest instead of being remembered as already emitted.
  drawn <- Filter(function(f) file.info(f)$size > 0L, sort(setdiff(files, seen)))
  invisible(lapply(drawn, function(f) emit_plot_frame(id, f, dims)))
  c(seen, drawn)
}

#' Close any open device and emit one `plot` frame per page produced.
#'
#' @param id Cell id.
#' @param dir Directory the device wrote into.
#' @param seen Character vector of filenames already emitted.
#' @return The updated `seen` vector.
harvest_plots <- function(id, dir, seen, dims = NULL) {
  if (!is.null(grDevices::dev.list())) {
    try(grDevices::dev.off(), silent = TRUE)
  }
  files <- list.files(dir, pattern = plot_pattern(dims), full.names = TRUE)
  fresh <- setdiff(files, seen)
  # A device always creates its first file, even with nothing drawn into it.
  drawn <- Filter(function(f) file.info(f)$size > 0L, sort(fresh))
  invisible(lapply(drawn, function(f) emit_plot_frame(id, f, dims)))
  c(seen, fresh)
}

#' Emit a data.frame as structured rows the notebook can put in a real table.
#'
#' Printing a data.frame gives the caller aligned text; a notebook needs
#' columns and types. Capped to a small preview with the true row count
#' reported, so a million-row frame never becomes a million-row JSON payload.
#'
#' @param id Cell id.
#' @param df A data.frame.
#' @return Invisibly NULL.
emit_dataframe <- function(id, df) {
  # A returned value is a NOTEBOOK PREVIEW, not the data viewer. Bounding rows
  # alone is insufficient: one list-column cell may contain a fitted model and
  # one character cell may contain megabytes. Serialising either recursively
  # blocks the R session before the browser receives a single result frame.
  # Keep the preview deliberately small, flatten complex cells to descriptions,
  # clip text, then enforce a final wire-size ceiling. The full object viewer
  # remains paginated independently for assigned objects.
  preview_rows <- 10L
  preview_cols <- 20L
  cell_chars <- 160L
  preview_bytes <- 256L * 1024L
  total_rows <- nrow(df)
  data_cols <- ncol(df)

  # R stores default 1..n row names compactly. Calling rownames(df) expands that
  # ALTREP to millions of strings, and the old identical(... seq_len(n)) check
  # built a second vector just to decide there was nothing to show. type=1 asks
  # R whether names are automatic without materialising them; explicit names
  # are sliced from the raw attribute only AFTER the 10-row preview is taken.
  explicit_rownames <- .row_names_info(df, type = 1L) > 0L
  total_cols <- data_cols + as.integer(explicit_rownames)
  data_col_cap <- max(0L, preview_cols - as.integer(explicit_rownames))
  shown_cols <- if (data_cols && data_col_cap) {
    seq_len(min(data_cols, data_col_cap))
  } else integer(0)
  source_types <- if (length(shown_cols)) {
    unname(vapply(df[shown_cols], function(col) class(col)[1L], character(1)))
  } else character(0)

  cell_preview <- function(value) {
    text <- if (is.null(value)) {
      "NULL"
    } else if (is.data.frame(value) || is.matrix(value)) {
      paste(dim(value), collapse = " x ")
    } else if (is.function(value)) {
      paste0("function(", formals_string(value), ")")
    } else if (is.environment(value)) {
      "<environment>"
    } else if (is.atomic(value)) {
      shown <- as.character(utils::head(value, 3L))
      shown <- substr(shown, 1L, 48L)
      paste0(paste(shown, collapse = ", "), if (length(value) > 3L) ", ..." else "")
    } else {
      paste0("<", class(value)[1L], ">")
    }
    substr(text, 1L, cell_chars)
  }

  safe_column <- function(col, rows) {
    shown <- utils::head(col, rows)
    if (is.factor(shown)) return(as.character(shown))
    if (inherits(shown, "Date") || inherits(shown, "POSIXt")) return(format(shown))
    if (is.atomic(shown) && is.null(dim(shown)) && !is.complex(shown) && !is.raw(shown)) {
      if (is.character(shown)) {
        long <- !is.na(shown) & nchar(shown, type = "chars") > cell_chars
        shown[long] <- paste0(substr(shown[long], 1L, cell_chars - 3L), "...")
      }
      return(shown)
    }
    if (is.list(shown)) return(vapply(shown, cell_preview, character(1)))
    if (!is.null(dim(shown)) && nrow(shown)) {
      return(vapply(seq_len(nrow(shown)), function(i) cell_preview(shown[i, , drop = TRUE]), character(1)))
    }
    rep(paste0("<", class(shown)[1L], ">"), rows)
  }

  head_df <- utils::head(df[shown_cols], preview_rows)
  if (ncol(head_df)) head_df[] <- lapply(head_df, safe_column, rows = nrow(head_df))
  # Informative row names are data (car names in mtcars, coefficient terms in
  # model summaries), but only their preview travels. Never cbind them onto the
  # full frame: that copies every column before head() and makes display time
  # scale with the entire object rather than the preview cells the user will see.
  if (explicit_rownames) {
    raw_names <- attr(df, "row.names", exact = TRUE)
    shown_names <- as.character(utils::head(raw_names, nrow(head_df)))
    head_df <- cbind(data.frame(rowname = shown_names, stringsAsFactors = FALSE),
                     head_df)
    source_types <- c("character", source_types)
  }
  preview_json <- jsonlite::toJSON(head_df, auto_unbox = TRUE, null = "null",
                                   na = "null", digits = NA)
  while (nchar(preview_json, type = "bytes") > preview_bytes && nrow(head_df) > 1L) {
    head_df <- utils::head(head_df, max(1L, as.integer(floor(nrow(head_df) / 2L))))
    preview_json <- jsonlite::toJSON(head_df, auto_unbox = TRUE, null = "null",
                                     na = "null", digits = NA)
  }
  emit(list(
    type = "dataframe", id = id,
    # I(): a one-column frame must ship columns/types as arrays, not scalars.
    columns = I(names(head_df)),
    types = I(source_types),
    nrow = total_rows, ncol = total_cols,
    shown = nrow(head_df), shownCols = ncol(head_df),
    truncated = total_rows > nrow(head_df) || total_cols > ncol(head_df),
    rows = head_df
  ))
  invisible(NULL)
}

#' Files named by an htmltools dependency slot, whatever shape it arrived in.
#'
#' `script`/`stylesheet` can be a character vector, a list of strings, or a
#' list with a `src` field — htmltools allows all three and widgets use all
#' three.
#'
#' @param x The dependency slot.
#' @return Character vector of relative file paths.
dep_files <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) {
    if (!is.null(x$src)) return(as.character(x$src))
    return(unlist(lapply(x, function(e) {
      if (is.list(e)) as.character(e$src) else as.character(e)
    }), use.names = FALSE))
  }
  as.character(x)
}

#' An htmlwidget as ONE self-contained HTML page — no pandoc.
#'
#' `htmlwidgets::saveWidget(selfcontained = TRUE)` needs pandoc, which plain R
#' installs do not have. But a widget is just rendered tags plus dependencies,
#' and every dependency is a file on disk — so read them and inline them.
#'
#' @param w An htmlwidget.
#' @return A single HTML string.
widget_standalone_html <- function(w) {
  rendered <- htmltools::renderTags(htmltools::as.tags(w, standalone = FALSE))
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  slurp <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  inline_one <- function(d) {
    root <- if (!is.null(d$package)) system.file(d$src$file, package = d$package) else d$src$file
    if (is.null(root) || !nzchar(root)) return("")
    css <- vapply(dep_files(d$stylesheet), function(f) {
      p <- file.path(root, f)
      if (file.exists(p)) sprintf("<style>%s</style>", slurp(p)) else ""
    }, character(1))
    js <- vapply(dep_files(d$script), function(f) {
      p <- file.path(root, f)
      # "</script" inside a JS bundle would end the inline tag mid-file; the
      # standard escape is valid everywhere in JS.
      if (file.exists(p)) sprintf("<script>%s</script>",
                                  gsub("</script", "<\\\\/script", slurp(p), fixed = TRUE)) else ""
    }, character(1))
    head_html <- if (!is.null(d$head)) paste(as.character(d$head), collapse = "\n") else ""
    paste(c(css, js, head_html), collapse = "\n")
  }
  paste0(
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
    "<style>html,body{margin:0;padding:0;height:100%;}</style>",
    paste(vapply(deps, inline_one, character(1)), collapse = "\n"),
    rendered$head,
    "</head><body>", rendered$html, "</body></html>"
  )
}

MAX_WIDGET_BYTES <- 15e6

#' Emit an htmlwidget (plotly, leaflet, DT, ...) as a `widget` frame.
#'
#' Falls back to printing a note rather than failing the cell: a widget that
#' cannot inline is an inconvenience, not an error in the user's code.
#'
#' @param id Cell id.
#' @param w The htmlwidget.
#' @return Invisibly NULL.
emit_widget <- function(id, w) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    cat("<htmlwidget: the htmltools package is required to display it>\n")
    return(invisible(NULL))
  }
  html <- tryCatch(widget_standalone_html(w), error = function(e) NULL)
  if (is.null(html)) {
    cat("<htmlwidget: could not render it standalone>\n")
    return(invisible(NULL))
  }
  if (nchar(html, type = "bytes") > MAX_WIDGET_BYTES) {
    cat(sprintf("<htmlwidget: %.1f MB inlined is too large to display here>\n",
                nchar(html, type = "bytes") / 1e6))
    return(invisible(NULL))
  }
  emit(list(type = "widget", id = id, class = class(w)[1L], html = html))
  invisible(NULL)
}

#' Coerce a matrix to a data.frame so it can be shown as a real table.
#'
#' Half of what R hands back is a matrix, not a data.frame — `coef(summary(fit))`,
#' `cor()`, `table()`. Printing those as aligned text when a data.frame would
#' become a sortable, exportable table is an arbitrary distinction to the reader.
#' Row names carry meaning in exactly these cases (term names, factor levels), so
#' they become a leading column rather than being dropped.
#'
#' @param m A matrix.
#' @return A data.frame.
matrix_to_df <- function(m) {
  # as.data.frame.matrix, NOT the generic. On a `table`-class object — which is
  # what summary.data.frame() and table() both return — the generic dispatches
  # to as.data.frame.table and produces a LONG tally (Var1, Var2, Freq),
  # destroying the layout the reader expects. The .matrix method keeps it wide.
  df <- as.data.frame.matrix(m, stringsAsFactors = FALSE, optional = TRUE)
  # optional = TRUE leaves a dimnames-less matrix with NULL column names, and
  # a dataframe frame without keys shipped "columns":null — the client cannot
  # read a row object without keys, so synthesize the V1..Vn R itself shows.
  if (is.null(names(df))) names(df) <- sprintf("V%d", seq_len(ncol(df)))
  # Promoting row names to a column is emit_dataframe's job and only its job —
  # doing it in both places produced two `rowname` columns, because cbind()
  # keeps the row names it just copied.
  #
  # What has to happen HERE is deciding whether this matrix's row names mean
  # anything, because as.data.frame.matrix() destroys the evidence: the blank
  # names that summary.data.frame() produces come out the other side as "X",
  # "X.1", "X.2" — synthesised placeholders indistinguishable from real labels.
  # So the judgement is made against `m` and the answer written onto the frame.
  rn <- rownames(m)
  informative <- !is.null(rn) &&
    !identical(rn, as.character(seq_len(nrow(m)))) &&
    any(nzchar(trimws(rn)))
  rownames(df) <- if (informative) rn else NULL
  df
}

#' Evaluate one cell's source, streaming its output, and report the outcome.
#'
#' Warnings and messages are forwarded as they occur rather than at the end of
#' the cell, so a slow loop that warns halfway through reports halfway through.
#' Interrupts are caught, not fatal: the session survives so the next cell
#' still sees the variables the interrupted one defined.
#'
#' @param id Cell identifier echoed back in the done frame.
#' @param source R source text.
#' One frame's local variables: name, class, and a short shape.
#'
#' Read-only and defensive. `mget` with `ifnotfound` never forces a promise
#' that would error, the value summary is a class-and-dimension line rather
#' than a print (printing a frame's locals could take minutes and megabytes),
#' and everything is capped. A debugger that hangs the session it is
#' debugging has made things worse.
frame_vars <- function(env, max_vars = 40L) {
  if (!is.environment(env)) return(list())
  names_here <- tryCatch(ls(envir = env, all.names = FALSE), error = function(e) character(0))
  if (!length(names_here)) return(list())
  if (length(names_here) > max_vars) names_here <- names_here[seq_len(max_vars)]
  lapply(names_here, function(nm) {
    v <- tryCatch(mget(nm, envir = env, ifnotfound = list(NULL))[[1L]],
                  error = function(e) NULL)
    cls <- tryCatch(paste(class(v), collapse = "/"), error = function(e) "?")
    shape <- tryCatch({
      if (is.null(v)) "NULL"
      else if (is.data.frame(v)) sprintf("%d x %d", nrow(v), ncol(v))
      else if (is.function(v)) "function"
      else if (is.atomic(v) && length(v) == 1L) {
        t <- paste(format(v), collapse = " ")
        if (nchar(t) > 60L) paste0(substr(t, 1L, 57L), "...") else t
      } else sprintf("length %d", length(v))
    }, error = function(e) "?")
    list(name = nm, class = cls, value = shape)
  })
}

#' The call stack at the moment of an error, as frames a UI can click.
#'
#' Stage 5 slice 1. Two details make this useful rather than decorative:
#'
#'   IT RUNS BEFORE THE UNWIND. Called from a `withCallingHandlers` error
#'   handler, so `sys.calls()` still holds the stack. From `tryCatch` it would
#'   be empty, which is why R users are told to call `traceback()` afterwards.
#'
#'   THE HARNESS IS TRIMMED. The bottom frames are this file's own
#'   (`run_cell`, `withCallingHandlers`, the `lapply` over expressions) and
#'   showing them teaches the reader that their error came from CarmaR. Frames
#'   are dropped up to and including the last `eval(e, globalenv())`, which is
#'   the boundary between our code and theirs.
#'
#' @return A list of list(call, file, line) — innermost LAST, the order
#'   `traceback()` prints and the order people read a stack in.
capture_trace <- function(max_frames = 40L) {
  calls <- sys.calls()
  if (!length(calls)) return(list())
  texts <- vapply(calls, function(cl) {
    d <- tryCatch(paste(deparse(cl), collapse = " "), error = function(e) "<call>")
    if (nchar(d) > 300L) paste0(substr(d, 1L, 297L), "...") else d
  }, character(1))
  # Everything up to the user's own evaluation belongs to the harness.
  # startsWith, not grepl(fixed = TRUE): with fixed = TRUE the `^` is a
  # LITERAL caret and the pattern never matches, which silently shipped the
  # whole boot stack to the browser.
  boundary <- which(startsWith(texts, "withVisible(eval(") |
                    startsWith(texts, "eval(e, globalenv())"))
  from <- if (length(boundary)) max(boundary) + 1L else 1L
  # And the handler frames at the very top are ours too.
  to <- length(texts)
  while (to >= from && grepl("^(capture_trace|\\.handleSimpleError|h\\(simpleError|stop\\()", texts[[to]])) {
    to <- to - 1L
  }
  if (to < from) return(list())
  idx <- seq.int(from, to)
  if (length(idx) > max_frames) idx <- utils::tail(idx, max_frames)
  frames <- sys.frames()
  lapply(idx, function(k) {
    ref <- utils::getSrcref(calls[[k]])
    list(call = texts[[k]],
         # The frame's own variables, so "what was `n` at the time?" has an
         # answer without re-running anything. `ls()` and `class()` only — no
         # user code is evaluated here, and the values are capped hard because
         # a frame can hold a 2 GB data frame and this rides the same stdout
         # the cell's output does.
         vars = if (k <= length(frames)) frame_vars(frames[[k]]) else list(),
         file = if (is.null(ref)) NULL else {
           f <- attr(ref, "srcfile")
           if (is.null(f) || is.null(f$filename) || !nzchar(f$filename)) NULL else f$filename
         },
         line = if (is.null(ref)) NULL else as.integer(ref[[1L]]))
  })
}

#' @return Invisibly NULL. Emits exactly one `done` frame.
run_cell <- function(id, source, dims = NULL) {
  stopifnot(is.character(source), length(source) == 1L)
  # `dims` crosses the wire unvalidated (serve.R checks only id and source),
  # and `$` on an atomic vector THROWS — before the eval's own tryCatch, so a
  # malformed dims from an older bundle or a hand-built frame used to escape
  # run_cell entirely and wedge the whole exec route. A dims that is not a
  # list is no dims.
  if (!is.null(dims) && !is.list(dims)) dims <- NULL
  status <- "ok"
  detail <- NULL
  plot_dir <- tempfile("carmar-plots-")
  dir.create(plot_dir)
  seen <- character(0)
  # Asked for vector, cannot give it: SAY SO. Falling back to a raster the
  # reader did not choose, silently, is how "my plots are blurry" becomes an
  # unanswerable bug report — and this R may well be one where svg() opens
  # cleanly and writes nothing (see detect_svg_device).
  if (isTRUE(dims$format == "svg") && is.null(SVG_DEVICE)) {
    emit(list(type = "stream", id = id, kind = "warning",
              text = paste("Vector output is not available in this R —",
                           "install the svglite package (or XQuartz, for Cairo)",
                           "and restart. Drawing at", dims$res %||% PLOT_RES,
                           "dpi instead.\n")))
  }

  # Where the error happened, captured WHILE it happens. By the time
  # tryCatch's handler runs the stack has already unwound, so a traceback
  # taken there is empty — this is the whole reason for the extra calling
  # handler below, and the reason `at_line` is tracked around each top-level
  # expression rather than derived afterwards.
  at_line <- NA_integer_
  trace_frames <- list()

  tryCatch(
    withCallingHandlers(
      {
        # keep.source so every top-level expression knows its own line, which
        # is what "jump from the error to the source" actually needs.
        exprs <- parse(text = source, keep.source = TRUE)
        srcrefs <- attr(exprs, "srcref")
        # ONE device for the whole cell, not one per statement. Base graphics
        # are stateful across statements — `layout()` then two plots, `par()`
        # then a plot, `seq_heatmap()` then `abline()` — and a per-statement
        # device reset broke every one of those idioms ("plot.new has not
        # been called yet"). Finished pages still stream out mid-cell via
        # harvest_finished(); only the page being drawn waits for cell end.
        dev_seq <- 1L
        open_plot_device(plot_dir, dev_seq, dims)
        invisible(lapply(seq_along(exprs), function(.i) {
          e <- exprs[[.i]]
          at_line <<- if (!is.null(srcrefs) && length(srcrefs) >= .i &&
                          !is.null(srcrefs[[.i]])) {
            as.integer(srcrefs[[.i]][[1L]])
          } else {
            NA_integer_
          }
          res <- withVisible(eval(e, globalenv()))
          if (isTRUE(res$visible)) {
            v <- res$value
            if (inherits(v, "htmlwidget")) emit_widget(id, v)
            else if (is.data.frame(v)) emit_dataframe(id, v)
            else if (is.matrix(v) && nrow(v) > 0L && ncol(v) > 0L) emit_dataframe(id, matrix_to_df(v))
            else print(v)
          }
          seen <<- harvest_finished(id, plot_dir, seen, dims)
          # User code may close our device (an explicit dev.off() in the
          # cell). Reopen under a FRESH sequence number: reusing e001-*.png
          # would overwrite files already emitted, and `seen` would silently
          # swallow the replacements.
          if (is.null(grDevices::dev.list())) {
            dev_seq <<- dev_seq + 1L
            open_plot_device(plot_dir, dev_seq, dims)
          }
        }))
      },
      warning = function(w) {
        emit(list(type = "stream", id = id, kind = "warning",
                  text = conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        emit(list(type = "stream", id = id, kind = "message",
                  text = sub("\n$", "", conditionMessage(m))))
        invokeRestart("muffleMessage")
      },
      # The traceback, taken BEFORE the stack unwinds. A calling handler runs
      # inside the erroring frame; tryCatch's handler runs after the unwind,
      # where sys.calls() is empty. This one only RECORDS — it does not handle
      # the error, so the tryCatch below still decides the cell's fate.
      error = function(e) {
        trace_frames <<- capture_trace()
      }
    ),
    error = function(e) {
      status <<- "error"
      detail <<- conditionMessage(e)
      # A parse error has no stack and no expression index; its position comes
      # from the message instead, exactly as the analyzer reads it.
      emit(list(type = "traceback", id = id,
                message = conditionMessage(e),
                # 1-based line WITHIN this chunk's source. NA when the failure
                # happened outside any top-level expression (a parse error).
                line = if (is.na(at_line)) NULL else at_line,
                call = tryCatch(paste(deparse(conditionCall(e)), collapse = " "),
                                error = function(x) NULL),
                frames = trace_frames))
    },
    interrupt = function(i) {
      status <<- "interrupted"
      detail <<- "Execution interrupted"
    }
  )

  # An interrupted or failed cell may still have drawn something, and it always
  # leaves a device open — close it before the next cell inherits it.
  seen <- tryCatch(harvest_plots(id, plot_dir, seen, dims),
                   error = function(e) seen, interrupt = function(i) seen)
  unlink(plot_dir, recursive = TRUE)

  flush(stdout())
  emit(list(type = "done", id = id, status = status, message = detail))
}

#' Read one command line from stdin, tolerating an interrupt that lands while
#' the worker is idle (a Stop pressed with nothing running must not kill R).
#'
#' @param con Open text connection on stdin.
#' @return A single line, character(0) at EOF, or NA_character_ if interrupted.
read_command <- function(con) {
  tryCatch(
    readLines(con, n = 1L, warn = FALSE),
    interrupt = function(i) NA_character_
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# `View(df)` must do what it does in RStudio. Overriding it on the SEARCH PATH
# rather than in globalenv keeps the user's environment clean (the Environment
# pane shows their objects, not ours) while still shadowing utils::View.
carmar_tools <- new.env()
carmar_tools$View <- function(x, title = NULL) {
  # The LABEL is what the user typed; the FETCH NAME is where the viewer reads
  # it from. Conflating them titled `View(mtcars)` as `.carmar_view`, because
  # mtcars lives in the datasets package rather than the global environment.
  label <- if (!is.null(title)) title else deparse(substitute(x))
  assign(".carmar_view", x, envir = globalenv())
  emit_view("view-request", ".carmar_view", label = label)
  invisible(NULL)
}
attach(carmar_tools, name = "carmar:tools", warn.conflicts = FALSE)

con <- file("stdin", open = "rt", blocking = TRUE)
# The ready frame says WHICH R this is. A session that silently uses the wrong
# installation looks identical to one using the right one until `library(tna)`
# fails — so the home and the library count are reported up front.
# The command vocabulary, announced.
#
# An unknown command is SILENTLY IGNORED by the dispatch loop (no else, no
# error), which is the right call for a protocol that must not die on a
# stray frame — but it means a client asking an older kernel for a command
# it has never heard of waits out its own timeout and then guesses why. The
# import wizard's `choose` allowed 320 s for a human at a file dialog, so an
# old kernel turned "Import Data…" into five minutes of nothing.
#
# Advertising the vocabulary lets a client know instantly. Kernels older than
# this simply omit the field, and clients fall back to probing.
emit(list(type = "ready", pid = Sys.getpid(), r = R.version.string,
          # I(): a single library path must still ship as an array.
          home = R.home(), libs = I(.libPaths()),
          commands = I(c("exec", "env", "obj", "struct", "parse", "format",
                         "complete", "packages", "package_action", "package_help",
                         "help", "hover", "wd", "files", "choose", "sniff",
                         "import", "readfile", "writefile", "view", "colstats",
                         "rm"))))
# (No package count here on purpose: installed.packages() reads every
# package's DESCRIPTION — 0.3–1.8 s on a big library — and no client ever
# consumed the number. The packages PANE asks the `packages` op on demand.)

repeat {
  line <- read_command(con)
  if (length(line) == 0L) break                 # EOF: supervisor went away
  if (is.na(line) || !nzchar(trimws(line))) next
  cmd <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
  # A command must be an object with a string `type`. Anything else — a bare
  # number, an array, `{"type":[1,2]}` — used to reach `cmd$type` on an atomic
  # vector and take the whole worker down with it, ending the session.
  if (!is.list(cmd) || !is.character(cmd$type) || length(cmd$type) != 1L) next
  if (identical(cmd$type, "shutdown")) break
  # Every command is handled in the worker because every one of them needs the
  # SESSION — the environment, the search path, the working directory. Even
  # `files` and `import`: import assigns into the environment, and both must
  # see the same wd and `~` the user's own code sees. The cost is that they
  # queue behind a running cell, which is also the correct ordering.
  #
  # A handler that throws must cost its own command and nothing else: the
  # session behind it holds the user's unsaved work, and losing it to a
  # malformed `view` request is not a trade anyone would accept. The failure
  # comes back as an `error` FIELD on a frame carrying the request's id, so
  # the caller sees a reason instead of waiting out a timeout.
  #
  # `interrupt` IS caught below, but only as a last resort: Stop is delivered
  # as an interrupt condition and the handlers that can be interrupted (exec,
  # view) catch it themselves, with meaning. The outer handler exists for the
  # narrow window where one lands outside those — an uncaught interrupt ends
  # the worker script, and the supervisor treats a dead worker as fatal.
  dispatch <- function(cmd) {
  if (identical(cmd$type, "exec"))     run_cell(cmd$id, cmd$source, cmd$dims)
  if (identical(cmd$type, "env"))      emit_env(cmd$id)
  if (identical(cmd$type, "obj"))      emit_obj(cmd$id, cmd$name)
  if (identical(cmd$type, "struct"))   emit_struct(cmd$id, cmd$name, cmd$path)
  if (identical(cmd$type, "parse"))    emit_parse(cmd$id, cmd$source)
  if (identical(cmd$type, "format"))   emit_format(cmd$id, cmd$source)
  if (identical(cmd$type, "complete")) emit_complete(cmd$id, cmd$line, cmd$cursor)
  if (identical(cmd$type, "packages")) emit_packages(cmd$id, cmd$scope)
  if (identical(cmd$type, "package_action")) emit_package_action(cmd$id, cmd$action, cmd$name, cmd$lib)
  if (identical(cmd$type, "package_help")) emit_package_help(cmd$id, cmd$name)
  if (identical(cmd$type, "help"))     emit_help(cmd$id, cmd$topic)
  if (identical(cmd$type, "hover"))    emit_hover(cmd$id, cmd$name)
  if (identical(cmd$type, "wd"))       emit_wd(cmd$id, cmd$path)
  if (identical(cmd$type, "files"))    emit_files(cmd$id, cmd$path, isTRUE(cmd$all))
  if (identical(cmd$type, "choose"))   emit_choose(cmd$id, cmd$mode, cmd$start, cmd$prompt,
                                                    cmd$probeOnly, cmd$default)
  if (identical(cmd$type, "sniff"))    emit_sniff(cmd$id, cmd$path, cmd$opts)
  if (identical(cmd$type, "import"))   emit_import(cmd$id, cmd$path, cmd$name)
  if (identical(cmd$type, "readfile"))  emit_readfile(cmd$id, cmd$path)
  if (identical(cmd$type, "writefile")) emit_writefile(cmd$id, cmd$path, cmd$text)
  # `rows` is the pre-paging spelling of `limit`; old clients keep working.
  if (identical(cmd$type, "view"))     emit_view(cmd$id, cmd$name,
                                                 offset = cmd$offset,
                                                 limit = cmd$limit %||% cmd$rows,
                                                 sort = cmd$sort,
                                                 desc = isTRUE(cmd$desc),
                                                 col_offset = cmd$colOffset,
                                                 col_limit = cmd$colLimit,
                                                 query = cmd$query,
                                                 filters = cmd$filters)
  if (identical(cmd$type, "colstats")) emit_colstats(cmd$id, cmd$name, cmd$column,
                                                     query = cmd$query,
                                                     filters = cmd$filters)
  if (identical(cmd$type, "rm"))       emit_rm(cmd$id, cmd$names)
  invisible(NULL)
  }
  # An exec that dies OUTSIDE run_cell's own handlers must still end in a
  # `done`: the browser settles a cell only on its done frame, and the
  # supervisor retires the route (and frees the worker queue) on the same
  # signal — an `{"type":"exec", error}` frame satisfied neither, so one
  # malformed command wedged every later run behind it, permanently.
  #
  # The interrupt handler is the same guarantee for Stop: run_cell catches
  # interrupts around the eval, but one landing in its epilogue (unlink,
  # flush, the emit itself) used to escape this loop, end the worker script,
  # and take the supervisor's event loop down with it.
  fail_frame <- function(text) {
    id_ok <- is.character(cmd$id) && length(cmd$id) == 1L
    if (identical(cmd$type, "exec")) {
      if (id_ok) emit(list(type = "done", id = cmd$id, status = "error",
                           message = text))
    } else {
      emit(list(type = cmd$type, id = if (id_ok) cmd$id else NULL,
                error = text))
    }
  }
  tryCatch(dispatch(cmd),
           error = function(e) fail_frame(paste("command failed:",
                                                conditionMessage(e))),
           interrupt = function(i) fail_frame("Execution interrupted"))
}

close(con)

})
