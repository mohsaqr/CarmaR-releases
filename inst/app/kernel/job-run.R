#!/usr/bin/env Rscript
#
# job-run.R — one development task, in its own R process.
#
# Spawned by the supervisor through the ordinary `kernel_start()` path, so it
# inherits the things that took a while to get right there: a UTF-8 child
# locale whatever the launcher had, an R environment scrubbed of the
# supervisor's own R_HOME/R_LIBS (which otherwise aim the child at the wrong
# tree), and `kernel_poll`'s bounded drain, so a check that prints a megabyte
# cannot starve the event loop.
#
# ── this process evaluates; that is the point, and the boundary ───────────
#
# analyze.R may never evaluate user source, because a second evaluating
# session would have no Stop button and no session identity. This one may,
# and has neither problem, because it is NOT a session: it is one task, it
# owns no workspace anybody can see, it is killed by PID, and when it dies
# nothing the user typed goes with it. `devtools::check()` is going to
# evaluate the package's code no matter which process asks; the decision here
# is only that the process is not the one holding the notebook's variables.
#
# ── nothing from the wire is parsed ───────────────────────────────────────
#
# The task name arrives in the environment and is matched against a `switch`
# whose branches are literal calls. The root and the filter are passed as
# ARGUMENTS to those calls, never pasted into an expression. There is no
# `parse(text=)`, no `eval(parse(...))`, and none may be added — the same
# rule analyze.R carries, for a different reason: there, so nothing runs;
# here, so only the six things named below can.

args <- commandArgs(trailingOnly = TRUE)
sentinel <- args[1]
if (is.na(sentinel) || !nzchar(sentinel)) {
  stop("job-run.R needs a sentinel argument", call. = FALSE)
}
suppressPackageStartupMessages(library(jsonlite))

#' Write one control frame: the session's random sentinel, then compact JSON.
#' Identical framing to worker.R and analyze.R, so kernel_poll needs no new case.
#'
#' The job's own id is stamped HERE rather than added by the supervisor on the
#' way past, and that is a deliberate consequence of the frame-fidelity rule:
#' a frame the supervisor has to rewrite is a frame it has to re-encode, and
#' jsonlite's defaults round numbers to four digits, render an absent field as
#' a truthy `{}` and collapse a one-element array to a scalar. A frame that
#' already carries its id can be forwarded as BYTES.
#'
#' @param obj A list to serialise.
#' @return Invisibly NULL.
emit <- function(obj) {
  frame <- c(list(type = obj$type, id = job_id), obj[setdiff(names(obj), "type")])
  cat(sentinel, toJSON(frame, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush(stdout())
  invisible(NULL)
}

job_id <- Sys.getenv("CARMAR_JOB_ID", "job")
task   <- Sys.getenv("CARMAR_JOB_TASK", "")
root   <- Sys.getenv("CARMAR_JOB_ROOT", "")
target <- Sys.getenv("CARMAR_JOB_TARGET", "")
filter <- Sys.getenv("CARMAR_JOB_FILTER", "")
format <- Sys.getenv("CARMAR_JOB_FORMAT", "")

# The supervisor validated all of this in jobs.R before spawning. Re-checking
# here is not redundant: this file is also runnable by hand, and a task that
# trusted its environment would be a very quiet way to run the wrong thing.
KNOWN <- c("load", "document", "test", "check", "build", "install", "render")
if (!task %in% KNOWN) {
  emit(list(type = "job", event = "done", ok = FALSE,
            error = sprintf("unknown task '%s'", task)))
  quit(status = 2L, save = "no")
}
if (!nzchar(root) || !dir.exists(root)) {
  emit(list(type = "job", event = "done", ok = FALSE,
            error = sprintf("'%s' is not a folder", root)))
  quit(status = 2L, save = "no")
}
# A render acts on a FILE. Checking only the folder would let a task start
# against a document that is not there and fail somewhere less legible.
if (identical(task, "render") && (!nzchar(target) || !file.exists(target))) {
  emit(list(type = "job", event = "done", ok = FALSE,
            error = sprintf("'%s' is not a file", target)))
  quit(status = 2L, save = "no")
}

#' Refuse early and in words when the task's package is missing.
#' @param pkg Package name the task needs.
#' @return Invisibly TRUE, or quits with a `done` frame.
need <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))
  emit(list(type = "job", event = "done", ok = FALSE,
            error = sprintf("this task needs the '%s' package, which is not installed. install.packages(\"%s\")",
                            pkg, pkg)))
  quit(status = 3L, save = "no")
}

# ── structured test results ────────────────────────────────────────────────
#
# The reason `test` gets its own extraction rather than a log the UI greps:
# Stage 5's exit criterion is that a failure NAVIGATES to exact source, and a
# regex over testthat's printed output cannot do that reliably — the format is
# a reporter's presentation choice and changes between versions.
#
# Measured instead (testthat 3.3.2, against a synthetic package): every
# expectation carries `$srcref`, an integer srcref whose first element is the
# line and whose "srcfile" attribute holds the file name. Failures at lines
# 2/6/10/14 of the fixture reported exactly 2/6/10/14. That is a field, not a
# printed string, so it is what this reads.

#' One expectation's status, from its class.
#' @param e A testthat expectation condition.
#' @return One of "success", "failure", "error", "skip", "warning".
expectation_status <- function(e) {
  cls <- class(e)
  if (inherits(e, "expectation_success")) "success"
  else if (inherits(e, "expectation_failure")) "failure"
  else if (inherits(e, "expectation_error")) "error"
  else if (inherits(e, "expectation_skip")) "skip"
  else if (inherits(e, "expectation_warning")) "warning"
  else sub("^expectation_", "", cls[[1L]])
}

# A failing suite can hold thousands of expectations, and every one of them
# would ride the socket. Bounded — and the bound is REPORTED, because a list
# that silently stops looks exactly like a suite that stopped failing.
MAX_REPORTED <- 200L

#' Flatten testthat's nested results into one tidy row per problem.
#' @param results A `testthat_results` object.
#' @return list(rows, counts, truncated)
collect_results <- function(results) {
  counts <- c(success = 0L, failure = 0L, error = 0L, skip = 0L, warning = 0L)
  rows <- list()
  # Nested lists of conditions: this walks blocks and their expectations. It
  # is a loop rather than an apply because it accumulates into two different
  # places (a tally and a bounded list) and stops adding to one of them.
  for (block in results) {
    for (e in block$results) {
      status <- expectation_status(e)
      if (status %in% names(counts)) counts[[status]] <- counts[[status]] + 1L
      if (identical(status, "success")) next
      if (length(rows) >= MAX_REPORTED) next
      srcref <- e$srcref
      line <- if (is.null(srcref)) NA_integer_ else as.integer(srcref)[1L]
      file <- if (is.null(srcref)) NA_character_ else {
        srcfile <- attr(srcref, "srcfile")
        if (is.null(srcfile)) NA_character_ else (srcfile$filename %||% NA_character_)
      }
      name <- if (is.na(file)) (block$file %||% NA_character_) else basename(file)
      # testthat's srcref carries only a BASENAME, and a basename is not
      # something an editor can open. The full path is resolved here, where
      # the filesystem actually is, and only when the file is really there —
      # a path that does not exist is worse than none, because the UI would
      # offer a link that opens an empty buffer.
      full <- if (is.na(name)) NA_character_ else file.path(root, "tests", "testthat", name)
      if (!is.na(full) && !file.exists(full)) full <- NA_character_
      rows[[length(rows) + 1L]] <- list(
        status = status,
        test = block$test %||% NA_character_,
        file = name,
        path = full,
        line = line,
        message = paste(utils::head(strsplit(conditionMessage(e), "\n", fixed = TRUE)[[1L]], 12L),
                        collapse = "\n")
      )
    }
  }
  problems <- sum(counts[c("failure", "error", "skip", "warning")])
  list(rows = rows, counts = as.list(counts),
       truncated = max(0L, problems - length(rows)))
}

run_tests <- function() {
  need("testthat")
  results <- testthat::test_local(
    root,
    filter = if (nzchar(filter)) filter else NULL,
    reporter = "progress",
    stop_on_failure = FALSE
  )
  collected <- collect_results(results)
  emit(list(type = "job", event = "tests",
            # I() so a single result stays an ARRAY on the wire. jsonlite
            # collapses a length-1 list to a scalar otherwise, and the browser
            # would iterate the characters of a string.
            rows = I(collected$rows), counts = collected$counts,
            truncated = collected$truncated,
            root = root))
  # A suite with failures is a completed job that found problems, not a job
  # that failed to run. The distinction is the whole reason `ok` and the
  # counts are separate fields.
  invisible(collected)
}

# ── rendering a document ───────────────────────────────────────────────────
#
# ── finding quarto is the whole problem, and it is not `Sys.which` ────────
#
# Measured on a normal macOS machine with RStudio installed: `quarto` is NOT
# on PATH, `quarto::quarto_path()` returns NULL, and a perfectly good Quarto
# 1.9.37 sits inside RStudio.app. A renderer that asked PATH would have told
# that user Quarto was not installed while it was on their disk and working.
#
# So this is a ladder, in the same spirit as detect_rscript() in kernel.R:
# an explicit override, then what RStudio itself sets, then the R package's
# own answer, then PATH, then the bundles we know about by name.

#' Locate a quarto binary.
#' @return Path to quarto, or "" when there is none.
find_quarto <- function() {
  explicit <- Sys.getenv("CARMAR_QUARTO", "")
  if (nzchar(explicit) && file.exists(explicit)) return(explicit)
  # RStudio and Positron both export this into the sessions they start.
  from_env <- Sys.getenv("QUARTO_PATH", "")
  if (nzchar(from_env) && file.exists(from_env)) return(from_env)
  if (requireNamespace("quarto", quietly = TRUE)) {
    found <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
    if (!is.null(found) && nzchar(found) && file.exists(found)) return(found)
  }
  on_path <- unname(Sys.which("quarto"))
  if (nzchar(on_path)) return(on_path)
  bundles <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto",
    "/usr/local/bin/quarto", "/opt/homebrew/bin/quarto",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
    "C:/Program Files/Quarto/bin/quarto.exe"
  )
  hit <- Find(file.exists, bundles)
  if (is.null(hit)) "" else hit
}

#' Which renderer for this document?
#'
#' `.qmd` is Quarto's format and rmarkdown cannot read it, so there is no
#' choice to make. `.Rmd` and `.md` go to rmarkdown when it is installed —
#' Quarto renders them too, but with Quarto's semantics, and silently
#' changing how someone's existing .Rmd builds is not an improvement the
#' reader asked for.
#'
#' @param path The document.
#' @return "quarto" or "rmarkdown".
pick_renderer <- function(path) {
  ext <- tolower(sub("^.*\\.", "", basename(path)))
  if (identical(ext, "qmd")) return("quarto")
  if (requireNamespace("rmarkdown", quietly = TRUE)) "rmarkdown" else "quarto"
}

#' Run a command, streaming its output live AND keeping it.
#'
#' `system2(stdout = "")` streams but keeps nothing; `system2(stdout = TRUE)`
#' keeps everything but delivers it only at the end — and a render is exactly
#' the case where a reader wants to watch. Polling processx gives both, which
#' matters because quarto reports where it put the file on a line of ordinary
#' output rather than in an exit code.
#'
#' @param bin Executable.
#' @param args Character vector of arguments — an argument VECTOR, never a
#'   shell string, so a path with a space or a quote in it is one argument.
#' @return list(status, lines)
run_streaming <- function(bin, args) {
  proc <- processx::process$new(bin, args, stdout = "|", stderr = "2>&1")
  kept <- character(0)
  repeat {
    proc$poll_io(200L)
    lines <- tryCatch(proc$read_output_lines(), error = function(e) character())
    if (length(lines)) {
      kept <- c(kept, lines)
      cat(paste0(lines, "\n"), sep = "")
      flush(stdout())
    }
    if (!proc$is_alive()) {
      leftover <- tryCatch(proc$read_output_lines(), error = function(e) character())
      if (length(leftover)) {
        kept <- c(kept, leftover)
        cat(paste0(leftover, "\n"), sep = "")
      }
      break
    }
  }
  list(status = proc$get_exit_status(), lines = kept)
}

#' Render one document, and report where the output actually went.
#'
#' The output path is REPORTED, never guessed. rmarkdown::render returns an
#' absolute path (measured). Quarto prints "Output created: <path>", relative
#' to the document's own directory — also measured, including the case that
#' matters: rendering a full path from an unrelated working directory still
#' puts the output beside the source and still reports it relatively. Either
#' way the file is checked to exist before it is reported, for the same
#' reason a test failure only offers a link to a file that is really there.
render_document <- function() {
  renderer <- pick_renderer(target)
  produced <- NA_character_

  if (identical(renderer, "rmarkdown")) {
    need("rmarkdown")
    out <- rmarkdown::render(
      target,
      output_format = if (nzchar(format)) format else NULL,
      knit_root_dir = dirname(target)
    )
    if (length(out) >= 1L && nzchar(out[[1L]])) produced <- out[[1L]]
  } else {
    bin <- find_quarto()
    if (!nzchar(bin)) {
      stop("Quarto is not installed, or CarmaR could not find it. Install Quarto, ",
           "or set CARMAR_QUARTO to the binary.", call. = FALSE)
    }
    args <- c("render", target)
    if (nzchar(format)) args <- c(args, "--to", format)
    result <- run_streaming(bin, args)
    if (!identical(as.integer(result$status), 0L)) {
      stop(sprintf("quarto render exited %s", result$status), call. = FALSE)
    }
    hits <- grep("^Output created:", trimws(result$lines), value = TRUE)
    if (length(hits)) {
      reported <- trimws(sub("^Output created:", "", trimws(hits[[length(hits)]])))
      # Relative to the SOURCE directory, not this process's cwd.
      candidate <- if (grepl("^(/|[A-Za-z]:)", reported)) reported
                   else file.path(dirname(target), reported)
      produced <- candidate
    }
  }

  if (!is.na(produced) && file.exists(produced)) {
    produced <- normalizePath(produced, mustWork = FALSE)
  } else {
    # It rendered but we could not confirm where. Saying nothing is right;
    # naming a file that may not be there is not.
    produced <- NA_character_
  }
  emit(list(type = "job", event = "output", renderer = renderer,
            path = produced, format = if (nzchar(format)) format else NA_character_))
  invisible(produced)
}

started <- Sys.time()
emit(list(type = "job", event = "started", task = task, root = root, pid = Sys.getpid()))

outcome <- tryCatch({
  switch(task,
    load     = { need("pkgload");  pkgload::load_all(root, quiet = FALSE); NULL },
    document = { need("devtools"); devtools::document(root); NULL },
    test     = run_tests(),
    check    = { need("devtools"); devtools::check(root, quiet = FALSE); NULL },
    build    = { need("devtools"); devtools::build(root); NULL },
    install  = { need("devtools"); devtools::install(root, quick = TRUE, upgrade = "never"); NULL },
    render   = render_document()
  )
  list(ok = TRUE, error = "")
}, error = function(e) {
  # Reported, never swallowed: the message is the job's result, and the
  # non-zero exit below is what the supervisor's `done` frame carries.
  list(ok = FALSE, error = conditionMessage(e))
})

emit(list(type = "job", event = "done", ok = isTRUE(outcome$ok),
          error = outcome$error %||% "",
          elapsed = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)))
quit(status = if (isTRUE(outcome$ok)) 0L else 1L, save = "no")
