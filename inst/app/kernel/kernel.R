#
# Kernel — owns the R worker process. Deliberately knows nothing about HTTP.
#
# The whole point of the two-process split: this code never evaluates user
# source, so it stays responsive while the worker is blocked inside a 40-second
# bootstrap. Interrupting is then a signal to a process, not a message the
# blocked process has to notice.

suppressPackageStartupMessages(library(jsonlite))

#' Which R should host the session?
#'
#' Not "whatever `Rscript` is first on PATH" — that is how this shipped running
#' Homebrew's R 4.6.1 with 176 packages while the user's own library (the CRAN
#' framework build RStudio uses, 1241 packages including tna and TraMineR) sat
#' unused. An IDE that cannot see your packages is not your IDE.
#'
#' Order: an explicit `CARMAR_RSCRIPT`, then the macOS framework R, then PATH.
#'
#' @return Path to an Rscript binary.
detect_rscript <- function() {
  explicit <- Sys.getenv("CARMAR_RSCRIPT", "")
  if (nzchar(explicit) && file.exists(explicit)) return(explicit)
  framework <- "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript"
  if (file.exists(framework)) return(framework)
  unname(Sys.which("Rscript"))
}

#' Start a worker process.
#'
#' @param worker_path Path to worker.R.
#' @param sentinel Random per-session token framing control lines. Generated
#'   if absent; user code cannot guess it, so it cannot forge a frame.
#' @return A kernel handle: list(proc, sentinel, buffer).
#' @param env_extra Named character vector of environment variables to add for
#'   the worker — e.g. `c(CARMAR_ROOT = "/srv/project")`, which confines every
#'   file command to that subtree. Managed deployments set it; tests set it to
#'   prove the confinement holds.
kernel_start <- function(worker_path, sentinel = NULL, rscript = detect_rscript(),
                         env_extra = character()) {
  stopifnot(file.exists(worker_path), nzchar(rscript))
  if (is.null(sentinel)) {
    sentinel <- paste(sample(c(letters, 0:9), 24L, replace = TRUE), collapse = "")
  }
  # The supervisor is itself an R process, so its R_HOME / R_LIBS* point at the
  # R that launched it. Inheriting those into a DIFFERENT R installation aims
  # the child at the wrong tree and it never starts. Strip them and let the
  # chosen binary discover its own.
  parent_env <- Sys.getenv()
  clean_env <- parent_env[!grepl("^R_(HOME|LIBS|LIBS_USER|LIBS_SITE|PROFILE|ENVIRON|DOC_DIR|INCLUDE_DIR|SHARE_DIR)",
                                 names(parent_env))]
  if (length(env_extra)) {
    stopifnot(is.character(env_extra), !is.null(names(env_extra)))
    clean_env <- c(clean_env[setdiff(names(clean_env), names(env_extra))], env_extra)
  }

  proc <- processx::process$new(
    rscript,
    # --vanilla, NOT --no-init-file: the user's .Rprofile/.Renviron are part of
    # their R (library paths, repos, options). Only history and saved workspaces
    # are suppressed, which is what a fresh session wants.
    c("--no-save", "--no-restore", "--no-site-file", worker_path, sentinel),
    stdin = "|", stdout = "|", stderr = "|",
    env = clean_env,
    supervise = TRUE
  )
  list(proc = proc, sentinel = sentinel, rscript = rscript)
}

#' Send a cell to the worker.
#'
#' @param k Kernel handle.
#' @param id Cell id.
#' @param source R source text.
#' @param dims Optional list(width, height, res) in pixels/dpi for this run's
#'   graphics device — the equivalent of a chunk's `fig.width`/`fig.height`.
#'   One hardcoded size is never right for every plot: a seqplot with a
#'   six-column legend needs a different device than a scatter.
#' @return Invisibly TRUE.
kernel_exec <- function(k, id, source, dims = NULL) {
  stopifnot(is.character(id), is.character(source))
  cmd <- list(type = "exec", id = id, source = source)
  if (!is.null(dims)) cmd$dims <- dims
  kernel_write(k, paste0(toJSON(cmd, auto_unbox = TRUE), "\n"))
}

#' Write a whole command to the worker's stdin, however long it is.
#'
#' `processx::write_input()` is a NON-BLOCKING write: it puts what fits in the
#' operating system's pipe buffer and RETURNS THE REST as a raw vector. Ignoring
#' that return value silently truncated every command larger than the buffer —
#' the worker then waited forever for the end of a line that was never sent, and
#' the browser waited for an answer that never came. Anything small worked, so
#' the failure looked like a size-dependent hang rather than a dropped write:
#' `writefile` refused a 50 KB script, and a knitted report carrying one plot
#' never reached R at all.
#'
#' The loop is bounded. A worker busy inside a long-running cell does not drain
#' its stdin, so a command sent at that moment can genuinely have nowhere to go;
#' after `timeout` seconds this says so instead of blocking the supervisor's
#' event loop for the rest of the session.
#'
#' @param k Kernel handle.
#' @param text One complete command line, newline-terminated.
#' @param timeout Seconds to keep trying before giving up.
#' @return Invisibly TRUE.
kernel_write <- function(k, text, timeout = 15) {
  left <- k$proc$write_input(text)
  deadline <- Sys.time() + timeout
  while (length(left) > 0L) {
    if (Sys.time() > deadline) {
      stop(sprintf("R is not reading its input — %d bytes of this command could not be sent.",
                   length(left)), call. = FALSE)
    }
    Sys.sleep(0.01)
    left <- k$proc$write_input(left)
  }
  invisible(TRUE)
}

#' Send any command to the worker.
#'
#' `kernel_exec` is this with type "exec"; the IDE's panes (environment,
#' packages, help, working directory, parse-completeness) all ride the same
#' channel, so there is one queue and one ordering.
#'
#' @param k Kernel handle.
#' @param cmd Named list with at least `type` and `id`.
#' @return Invisibly TRUE.
kernel_send <- function(k, cmd) {
  stopifnot(is.list(cmd), !is.null(cmd$type))
  kernel_write(k, paste0(toJSON(cmd, auto_unbox = TRUE), "\n"))
}

#' Send a raw line to the worker, bypassing JSON encoding.
#'
#' Exists for the security suite: the frames worth testing are exactly the ones
#' `toJSON` cannot produce — a bare number, an array, `{"type":5}`. A worker
#' that dies on those takes the user's session with it, so they have to be
#' sendable.
#'
#' @param k Kernel handle.
#' @param line One line of text; a newline is appended if absent.
#' @return Invisibly TRUE.
kernel_send_raw <- function(k, line) {
  stopifnot(is.character(line), length(line) == 1L)
  kernel_write(k, paste0(sub("\n$", "", line), "\n"))
}

#' Interrupt whatever the worker is doing.
#'
#' SIGINT rather than SIGKILL: the worker catches it, reports `interrupted`,
#' and keeps its global environment, so the session is not lost.
#'
#' @param k Kernel handle.
#' @return Invisibly TRUE.
kernel_interrupt <- function(k) {
  k$proc$interrupt()
  invisible(TRUE)
}

#' Ask the worker to exit, then make sure it did.
#'
#' @param k Kernel handle.
#' @param grace Seconds to wait before killing.
#' @return Invisibly TRUE.
kernel_stop <- function(k, grace = 2) {
  try(k$proc$write_input("{\"type\":\"shutdown\"}\n"), silent = TRUE)
  k$proc$wait(timeout = grace * 1000)
  if (k$proc$is_alive()) k$proc$kill()
  invisible(TRUE)
}

#' Drain whatever the worker has produced since the last call.
#'
#' Blocks at most `timeout_ms`, so a caller can poll this from an event loop
#' without ever stalling. Lines carrying the sentinel are control frames;
#' everything else on stdout is the user's own output.
#'
#' @param k Kernel handle.
#' @param timeout_ms Maximum block, milliseconds.
#' @return List of events, each with `$type` in "stdout"/"stderr"/"stream"/
#'   "done"/"ready".
kernel_poll <- function(k, timeout_ms = 50L) {
  k$proc$poll_io(timeout_ms)
  out <- k$proc$read_output_lines()
  err <- k$proc$read_error_lines()

  # The sentinel is found ANYWHERE in the line, not only at its start.
  #
  # The worker deliberately does not capture output (sink() would kill
  # streaming), so user code controls where the cursor is when the next
  # control frame is written. A cell ending in `cat("done")` — no trailing
  # newline — leaves the cursor mid-line, and the frame lands glued to that
  # text: `donesentinel{"type":"done",...}`. startsWith() then missed it, the
  # frame was reported as ordinary stdout, and the cell never finished: no
  # `done` ever arrived and the run hung until the user pressed Stop.
  #
  # Splitting at the sentinel instead recovers both halves — the text before
  # it is the user's output, the rest is the frame. This does not weaken the
  # forgery guarantee: that rests on the sentinel being 24 unguessable
  # characters, not on its position in the line.
  from_stdout <- unlist(lapply(out, function(line) {
    at <- regexpr(k$sentinel, line, fixed = TRUE)
    if (at < 1L) return(list(list(type = "stdout", text = line)))
    payload <- substring(line, at + nchar(k$sentinel))
    parsed <- tryCatch(fromJSON(payload), error = function(e) NULL)
    if (is.null(parsed)) return(list(list(type = "stdout", text = line)))
    prefix <- substring(line, 1L, at - 1L)
    if (nzchar(prefix)) list(list(type = "stdout", text = prefix), parsed) else list(parsed)
  }), recursive = FALSE)
  from_stderr <- lapply(err, function(line) list(type = "stderr", text = line))

  c(from_stdout, from_stderr)
}
