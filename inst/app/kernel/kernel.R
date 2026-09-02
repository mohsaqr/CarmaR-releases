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
#' `explicit` is an argument rather than only an env read so serve.R can pass
#' the RESOLVED value (a user's rscript_path from settings, or the env var when
#' a deployment set one). Defaulting to the env var keeps every existing call
#' correct without changing it.
detect_rscript <- function(explicit = Sys.getenv("CARMAR_RSCRIPT", "")) {
  if (nzchar(explicit) && file.exists(explicit)) return(explicit)
  # The same ladder tools/app/launch.sh walks, and for the same reason: this
  # process may itself have been started by a Finder launch, whose PATH is
  # /usr/bin:/bin:/usr/sbin:/sbin — so Sys.which() finds a CRAN framework
  # install and misses Homebrew, rig, conda and Posit entirely. PATH is the
  # LAST rung, not the second.
  known <- c(
    "/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript",
    "/opt/homebrew/bin/Rscript",
    "/usr/local/bin/Rscript",
    "/opt/local/bin/Rscript",
    "/usr/bin/Rscript"
  )
  for (cand in known) if (file.exists(cand)) return(cand)
  # `Versions/Current` is a symlink CRAN maintains and other installers do
  # not; newest version first.
  versioned <- sort(Sys.glob(
    "/Library/Frameworks/R.framework/Versions/*/Resources/bin/Rscript"), decreasing = TRUE)
  if (length(versioned)) return(versioned[[1]])
  unname(Sys.which("Rscript"))
}

#' The full R binary beside an Rscript, for the interactive worker.
#'
#' The debugger needs `R --interactive`: only then is the CONSOLE the stdin
#' pipe, which is what lets a native `browser()` prompt read step commands the
#' supervisor sends. `Rscript` cannot do this — under it the console is the
#' script FILE, and a `browser()` reads the script's own remaining lines as
#' debug input (measured: it consumed the lines after the call and never
#' touched stdin).
#'
#' @param rscript Path to the Rscript binary actually chosen.
#' @return Path to the sibling R binary, or "" when it does not exist.
detect_r_binary <- function(rscript) {
  candidate <- file.path(dirname(rscript),
                         if (.Platform$OS.type == "windows") "R.exe" else "R")
  if (nzchar(rscript) && file.exists(candidate)) candidate else ""
}

#' The first expression an interactive worker runs on macOS.
#'
#' Command-line R promotes itself to a foreground LaunchServices application
#' when its Aqua/AppKit support initializes. On macOS 26 that gives every
#' document worker a generic `exec` Dock tile. A packaged CarmaR kernel carries
#' a tiny native library beside kernel.R; calling it before worker.R is
#' sourced registers this process as BackgroundOnly first. Distributions may
#' carry either an architecture-specific marker (R packages) or one universal
#' marker (application bundles and source checkouts). The process itself
#' must make the transition — a UIElement parent does not confer its activation
#' policy on an exec'd child.
#'
#' Source checkouts and non-macOS packages may not carry the compiled library;
#' in those cases this is deliberately a no-op and kernel startup is unchanged.
#'
#' @param worker_path Path to the worker boot script.
#' @param sysname Injectable platform name for source-only tests.
#' @param arch Injectable R process architecture for source-only tests.
#' @return A one-line R expression, or "" when no marker is available.
macos_background_boot <- function(worker_path,
                                  sysname = unname(Sys.info()[["sysname"]]),
                                  arch = R.version$arch) {
  if (!identical(sysname, "Darwin")) return("")
  # OPT-IN, and defaulting off is a reliability decision. TransformProcessType
  # hides the worker's Dock icon, but on a GUI-launched process (macOS 26) it
  # leaves R's console/event handling in a state where a plain readLines(stdin)
  # — every single command the supervisor sends — can block forever in
  # R_checkActivityEx, wedging the whole session at 0% CPU with Stop unable to
  # reach it. Foreground R, which is what RStudio runs, never does this. So a
  # visible Dock icon is the price of a kernel that answers; set
  # CARMAR_MARK_BACKGROUND=1 to restore Dock hiding once a transform that does
  # not wedge the console read is found.
  if (!identical(Sys.getenv("CARMAR_MARK_BACKGROUND", "0"), "1")) return("")
  marker <- Sys.getenv("CARMAR_BACKGROUND_LIBRARY", "")
  if (!nzchar(marker)) {
    suffix <- if (grepl("^(aarch64|arm64)", arch, ignore.case = TRUE)) {
      "arm64"
    } else if (grepl("x86_64|x86-64|amd64", arch, ignore.case = TRUE)) {
      "x86_64"
    } else ""
    beside <- dirname(worker_path)
    candidates <- c(
      if (nzchar(suffix)) file.path(
        beside, paste0("carmar-background-", suffix, ".dylib")),
      file.path(beside, "carmar-background.dylib"))
    found <- candidates[file.exists(candidates)]
    marker <- if (length(found)) found[[1L]] else candidates[[1L]]
  }
  if (!file.exists(marker)) return("")
  marker <- encodeString(normalizePath(marker, mustWork = TRUE), quote = '"')
  # TransformProcessType can report paramErr when the unbundled executable is
  # already background-only; the call still causes the early LaunchServices
  # registration we need. Classification, not that legacy status code, is the
  # contract, so the result is intentionally ignored.
  sprintf('local({dyn.load(%s);.C("carmar_mark_background",result=integer(1));invisible(NULL)})',
          marker)
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
#' A UTF-8 character locale, whatever the launcher handed us.
#'
#' The desktop app is started by launchd, which supplies no locale at all, so
#' R falls back to C — and a C locale is not "English", it is "no character
#' encoding". In that state jsonlite escapes every high byte of a string whose
#' declared encoding is unknown, so `35 – 1158` reaches the browser as
#' `35 <e2><80><93> 1158`, a column called `région` mangles the same way, and R
#' source containing a non-ASCII identifier does not even parse.
#'
#' Both ENDS need this. The supervisor calls it for itself (it parses and
#' re-encodes every frame on the wire); `kernel_start` puts it in each child's
#' environment. Fixing only one end fixes nothing: the mangling simply moves.
#'
#' Surgical: LC_CTYPE alone, so the user's own locale keeps deciding number and
#' date formatting, and only when what we have is not already UTF-8 — a
#' deliberate de_DE.UTF-8 is never overridden.
#'
#' @return The locale name now in force for LC_CTYPE.
utf8_ctype <- function() {
  current <- Sys.getlocale("LC_CTYPE")
  if (grepl("utf-?8", current, ignore.case = TRUE)) return(current)
  for (candidate in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", candidate)))) return(candidate)
  }
  current                                  # nothing available: carry on as-is
}

kernel_start <- function(worker_path, sentinel = NULL, rscript = detect_rscript(),
                         env_extra = character(), interactive = FALSE) {
  stopifnot(file.exists(worker_path), nzchar(rscript))
  if (is.null(sentinel)) {
    sentinel <- paste(sample(c(letters, 0:9), 24L, replace = TRUE), collapse = "")
  }
  # An `--interactive` R is (Unix only) per the R manual, and it is only worth
  # having when the sibling R binary exists. Falling back to batch keeps every
  # session working; only the debugger is absent, and the worker's ready frame
  # says which mode it booted in so nothing has to guess.
  r_binary <- if (interactive && .Platform$OS.type != "windows")
    detect_r_binary(rscript) else ""
  mode <- if (nzchar(r_binary)) "interactive" else "batch"
  # A SECOND token, deliberately not the sentinel. Commands sent to an
  # interactive worker are echoed back on stdout by R's console reader, so
  # they must be recognizable for scrubbing — but if they carried the frame
  # sentinel, their echo would parse as a control frame FROM the worker. Two
  # independent tokens make the two directions structurally non-confusable.
  cmdtag <- paste(sample(c(letters, 0:9), 24L, replace = TRUE), collapse = "")
  # The supervisor is itself an R process, so its R_HOME / R_LIBS* point at the
  # R that launched it. Inheriting those into a DIFFERENT R installation aims
  # the child at the wrong tree and it never starts. Strip them and let the
  # chosen binary discover its own.
  parent_env <- Sys.getenv()
  clean_env <- parent_env[!grepl("^R_(HOME|LIBS|LIBS_USER|LIBS_SITE|PROFILE|ENVIRON|DOC_DIR|INCLUDE_DIR|SHARE_DIR)",
                                 names(parent_env))]
  # ── the child must speak UTF-8, whatever launched the parent ────────────
  # The desktop launcher inherits launchd's C locale, and a child R in a C
  # locale is not merely English — it cannot represent non-ASCII at all:
  #
  #   * a string literal in worker.R with an en dash parses to encoding
  #     "unknown", and jsonlite serialises it as `35 <e2><80><93> 1158`, which
  #     is what the variables panel showed;
  #   * a data frame with a column called `région` mangles the same way;
  #   * a source file containing a non-ASCII IDENTIFIER does not even parse.
  #
  # `encoding = "UTF-8"` on the pipes below fixes only the supervisor's READING
  # of the wire. This fixes the child's PRODUCING of it, which is the half that
  # was missing.
  #
  # Surgical on purpose: only LC_CTYPE (the character encoding), so a user's
  # own locale keeps deciding number and date formatting; only when the
  # inherited locale is not already UTF-8, so a deliberate de_DE.UTF-8 is never
  # overridden; and LC_ALL is DROPPED when it is the non-UTF-8 culprit, because
  # LC_ALL outranks LC_CTYPE and would otherwise win silently.
  utf8_locale <- function(x) grepl("utf-?8", x, ignore.case = TRUE)
  # `Sys.getenv()` is a named CHARACTER VECTOR, and `x[["missing"]]` on one
  # throws "subscript out of bounds" — it does not return NULL the way a list
  # does. LC_ALL is unset on most machines, so reading it directly errored on
  # the common path and took the kernel down at startup.
  from_env <- function(name) if (name %in% names(clean_env)) clean_env[[name]] else ""
  lc_all <- from_env("LC_ALL")
  # Decide from what the CHILD inherits - its ENVIRONMENT - never from
  # Sys.getlocale(), which is the SUPERVISOR's RUNTIME locale. macOS boots a
  # GUI-launched supervisor with a runtime C.UTF-8 that is NEVER in the env, so
  # it never reaches the child; the child sees only these variables, and with
  # LC_ALL/LC_CTYPE/LANG all unset it falls back to bare C. There a single
  # multi-byte character in a command line (an en dash in a comment such as
  # `orders 0-5`, an accent, an emoji) wedges the interactive console reader and
  # freezes the kernel. Reading the runtime locale here let the guard skip in
  # exactly that case, which is why the packaged app hung on the first non-ASCII
  # chunk while a shell or RStudio (both of which export a UTF-8 locale) did not.
  effective <- if (nzchar(lc_all)) lc_all
    else if (nzchar(from_env("LC_CTYPE"))) from_env("LC_CTYPE")
    else from_env("LANG")
  if (!utf8_locale(effective)) {
    # LC_ALL outranks LC_CTYPE, so a non-UTF-8 LC_ALL would win silently.
    if (nzchar(lc_all) && !utf8_locale(lc_all)) {
      clean_env <- clean_env[names(clean_env) != "LC_ALL"]
    }
    clean_env[["LC_CTYPE"]] <- utf8_ctype()
  }

  if (length(env_extra)) {
    stopifnot(is.character(env_extra), !is.null(names(env_extra)))
    clean_env <- c(clean_env[setdiff(names(clean_env), names(env_extra))], env_extra)
  }

  if (identical(mode, "interactive")) {
    # TERM=dumb: readline's terminal probe writes an escape sequence
    # ("\033[?1034h") to stdout before anything else, and a dumb terminal is
    # the documented way to keep it out of the protocol stream.
    clean_env[["TERM"]] <- "dumb"
    # The worker cannot read argv for these — `R --interactive` takes no
    # script argument, the worker is booted by a sys.source line fed through
    # stdin below — so they travel in the environment instead.
    clean_env[["CARMAR_SENTINEL"]] <- sentinel
    clean_env[["CARMAR_CMD_TAG"]] <- cmdtag
    clean_env[["CARMAR_WORKER_MODE"]] <- "interactive"
    clean_env[["CARMAR_WORKER_DIR"]] <- dirname(normalizePath(worker_path, mustWork = FALSE))
  }
  spawn_bin <- if (identical(mode, "interactive")) r_binary else rscript
  spawn_args <- if (identical(mode, "interactive")) {
    # --no-echo suppresses the "> " prompt; the input ECHO it does not suppress
    # is scrubbed in kernel_poll by the cmdtag / pending-echo machinery.
    c("--interactive", "--no-echo", "--no-save", "--no-restore", "--no-site-file")
  } else {
    # --vanilla, NOT --no-init-file: the user's .Rprofile/.Renviron are part of
    # their R (library paths, repos, options). Only history and saved workspaces
    # are suppressed, which is what a fresh session wants.
    c("--no-save", "--no-restore", "--no-site-file", worker_path, sentinel)
  }
  proc <- processx::process$new(
    spawn_bin,
    spawn_args,
    stdin = "|", stdout = "|", stderr = "|",
    env = clean_env,
    supervise = TRUE,
    # The desktop launcher may itself inherit the C locale from launchd.
    # Decode the protocol pipes by their actual wire encoding instead of the
    # supervisor's locale, or non-ASCII output can be escaped before framing.
    encoding = "UTF-8"
  )
  # processx's line reader waits for a complete line before releasing it. A
  # large JSON view/plot frame can therefore sit in its internal buffer long
  # enough to starve the supervisor. Keep our own incremental framing state
  # in an environment so it remains mutable through R's copied list handle.
  io <- new.env(parent = emptyenv())
  io$out <- ""
  io$err <- ""
  k <- list(proc = proc, sentinel = sentinel, rscript = rscript, io = io,
            mode = mode, cmdtag = cmdtag)
  if (identical(mode, "interactive")) {
    # The boot line replaces worker-boot.R: same sys.source, same speed
    # rationale (parse the ~2,000-line file in one pass instead of feeding it
    # through the REPL reader). worker.R itself is sourced, not the shim —
    # the shim's only job was resolving its own directory from --file=,
    # which CARMAR_WORKER_DIR now carries.
    real_worker <- file.path(dirname(worker_path), "worker.R")
    if (!file.exists(real_worker)) real_worker <- worker_path
    worker_boot <- sprintf('sys.source("%s", envir = globalenv(), keep.source = FALSE)',
                           encodeString(normalizePath(real_worker)))
    boot <- paste(Filter(nzchar, c(macos_background_boot(worker_path), worker_boot)),
                  collapse = ";")
    kernel_console(k, boot)
  }
  k
}

#' Send one RAW console line to an interactive worker.
#'
#' This is the debugger's channel: while the worker is paused at a Browse
#' prompt, `n` / `s` / `f` / `c` and debug-console expressions are ordinary
#' console lines, not NDJSON. Measured under processx pipes, R does NOT echo
#' console input back (it does under a shell pipe with a controlling tty —
#' which is only the test bench, never the shipped spawn), so nothing here
#' needs scrubbing; the tagged-command scrub in kernel_poll stays as the
#' belt for the high-volume NDJSON path.
#'
#' @param k Kernel handle (mode "interactive").
#' @param line One line of text, no newline.
#' @return Invisibly TRUE.
kernel_console <- function(k, line) {
  stopifnot(identical(k$mode, "interactive"),
            is.character(line), length(line) == 1L, !grepl("\n", line, fixed = TRUE))
  kernel_write(k, paste0(line, "\n"))
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
  kernel_command(k, toJSON(cmd, auto_unbox = TRUE))
}

# R's interactive console reader wedges the whole process on input lines
# somewhere past 40,000 bytes (measured: 40,000 passed, 45,000 hung, and the
# hang is a hang, not an error). Anything bigger travels through a file
# instead. Batch workers have no such limit and never spill.
MAX_CONSOLE_LINE <- 32000L

#' Deliver one JSON command to the worker, however it must travel.
#'
#' Batch mode: the bare NDJSON line, exactly as always. Interactive mode: the
#' line is prefixed `#<cmdtag> ` — a comment, so the one failure mode where it
#' could reach R's raw top level (a catastrophic dispatch-loop abort) makes it
#' inert instead of evaluated, and so its console echo is self-identifying for
#' the scrubber in kernel_poll. A command too long for the console reader is
#' written to a 0600 temp file and replaced on the wire by a `cmdfile` stub;
#' the worker reads the file, deletes it, and dispatches its content.
#'
#' @param k Kernel handle.
#' @param json One complete JSON command, as text, no newline.
#' @return Invisibly TRUE.
kernel_command <- function(k, json) {
  if (!identical(k$mode, "interactive")) {
    return(kernel_write(k, paste0(json, "\n")))
  }
  if (nchar(json, type = "bytes") > MAX_CONSOLE_LINE) {
    spill <- tempfile("carmar-cmd-", fileext = ".json")
    writeLines(json, spill, useBytes = TRUE)
    Sys.chmod(spill, mode = "0600")
    json <- toJSON(list(type = "cmdfile", path = spill), auto_unbox = TRUE)
  }
  kernel_write(k, paste0("#", k$cmdtag, " ", json, "\n"))
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
  kernel_command(k, toJSON(cmd, auto_unbox = TRUE))
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
  # Interactive mode still tags the line: the point of these tests is what the
  # worker's PARSER does with malformed JSON, and the tag is what routes the
  # line to that parser (and keeps its echo scrubbable) rather than leaving it
  # to be misread as console input.
  kernel_command(k, sub("\n$", "", line))
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
  try(kernel_command(k, "{\"type\":\"shutdown\"}"), silent = TRUE)
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
  # Drain available bytes, not complete processx lines. Control frames can be
  # hundreds of KB; incremental draining prevents the child from blocking on
  # a full pipe while the parent waits for that same frame's newline.
  take_lines <- function(field, chunk, flush = FALSE) {
    incoming <- if (length(chunk)) paste0(chunk, collapse = "") else ""
    data <- paste0(k$io[[field]], incoming)
    nl <- gregexpr("\n", data, fixed = TRUE)[[1L]]
    if (length(nl) == 1L && nl[[1L]] < 0L) {
      if (flush && nzchar(data)) { k$io[[field]] <- ""; return(data) }
      k$io[[field]] <- data
      return(character())
    }
    last <- nl[[length(nl)]]
    # `last = nchar(data)` is NOT redundant. substring()'s default last is
    # 1000000L, so a two-argument substring() SILENTLY TRUNCATES at exactly one
    # million characters — see the note on the payload slice below, which this
    # is the other half of. Here it would drop the tail of any partial line
    # longer than 1 MB still waiting for its newline.
    complete <- if (last > 1L) substring(data, 1L, last - 1L) else ""
    k$io[[field]] <- if (last < nchar(data))
      substring(data, last + 1L, nchar(data)) else ""
    # Appending a marker preserves a final empty line, which base strsplit()
    # would otherwise discard (cat("\n") is real stdout).
    lines <- strsplit(paste0(complete, "\001"), "\n", fixed = TRUE)[[1L]]
    lines[[length(lines)]] <- sub("\001$", "", lines[[length(lines)]])
    sub("\r$", "", lines)
  }
  # DRAIN the pipe, do not sip from it. read_output() returns whatever one
  # read yields — around 64 KB — and this function is called once per event
  # loop tick, so a big frame arrived at one chunk per tick. Measured: a
  # 2.3 MB plot payload needs ~370 reads, which at the loop's cadence took
  # SIXTEEN SECONDS to deliver something R had drawn in 0.27 s. The dpi was
  # never the cost; the number of ticks was, and it scales with payload, so
  # every large plot, every wide data frame and every long print paid it.
  # ... and yet BOUNDED. An unbounded drain has the opposite failure: a worker
  # producing faster than this loop consumes (`repeat cat("x\n")`) keeps the
  # pipe permanently non-empty, `kernel_poll` never returns, httpuv::service()
  # is never reached — and the Stop the user is pressing can never be
  # delivered. Capping the bytes taken per poll keeps both properties: a big
  # frame still crosses in a couple of ticks instead of hundreds, and the
  # event loop always gets its turn, so interrupts stay deliverable.
  drain <- function(read, max_bytes = 4e6) {
    parts <- character(0)
    total <- 0
    repeat {
      piece <- tryCatch(read(), error = function(e) "")
      if (!length(piece) || !nzchar(piece)) break
      parts[[length(parts) + 1L]] <- piece
      total <- total + nchar(piece, type = "bytes")
      if (total >= max_bytes) break
    }
    if (length(parts)) paste0(parts, collapse = "") else ""
  }
  out <- take_lines("out", drain(function() k$proc$read_output()), !k$proc$is_alive())
  err <- take_lines("err", drain(function() k$proc$read_error()), !k$proc$is_alive())

  # ── echo scrubbing (interactive workers only) ─────────────────────────────
  # Under processx pipes R does not echo console input back (measured — the
  # echo seen under a shell pipe comes with a controlling tty, which the
  # shipped spawn never has). This scrub is the belt in case some platform's
  # console does: a line carrying the command tag is our own NDJSON coming
  # back, never user output. Found ANYWHERE in the line for the same reason
  # the sentinel is: a cell ending in cat("x") with no newline leaves the
  # cursor mid-line and an echo would land glued to that text. Anything
  # before the tag is real user output and is kept.
  if (identical(k$mode, "interactive") && length(out)) {
    tag <- paste0("#", k$cmdtag)
    scrubbed <- lapply(out, function(line) {
      at <- regexpr(tag, line, fixed = TRUE)
      if (at < 1L) return(line)
      prefix <- substring(line, 1L, at - 1L)
      if (nzchar(prefix)) prefix else NULL
    })
    out <- as.character(unlist(scrubbed))
  }

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
  # Consecutive plain lines COALESCE into one event. One frame per line made
  # text volume the supervisor's unit of work: a chunk printing 100k lines
  # meant 100k JSON encodes and 100k socket sends in a single-threaded event
  # loop — seconds of stall during which heartbeats and Stop sat unserved.
  # The client accumulates stdout text and joins on "\n", so a multi-line
  # event is byte-identical to the same lines delivered one at a time; only
  # lines carrying a control frame still need individual treatment.
  has_sentinel <- if (length(out)) grepl(k$sentinel, out, fixed = TRUE) else logical(0)
  from_stdout <- if (!length(out)) list() else {
    runs <- rle(has_sentinel)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    unlist(lapply(seq_along(runs$values), function(r) {
      lines <- out[starts[[r]]:ends[[r]]]
      if (!runs$values[[r]]) {
        return(list(list(type = "stdout", text = paste(lines, collapse = "\n"))))
      }
      unlist(lapply(lines, function(line) parse_control_line(k, line)),
             recursive = FALSE)
    }), recursive = FALSE)
  }
  from_stderr <- if (length(err)) {
    list(list(type = "stderr", text = paste(err, collapse = "\n")))
  } else list()

  c(from_stdout, from_stderr)
}

#' One stdout line KNOWN to carry the sentinel: split it into the user text
#' before the frame and the frame itself.
parse_control_line <- function(k, line) {
    at <- regexpr(k$sentinel, line, fixed = TRUE)
    if (at < 1L) return(list(list(type = "stdout", text = line)))
    # `nchar(line)` is NOT redundant: substring()'s default `last` is
    # 1000000L, so the two-argument form silently truncates at exactly one
    # million characters. Every control frame bigger than 1 MB — which means
    # EVERY PLOT above roughly 750 KB of PNG — came out chopped mid-string,
    # failed to parse, and fell through to the branch below that reports the
    # line as ordinary stdout. The plot vanished with no error anywhere: the
    # cell finished "ok", the sentinel and the raw JSON were printed into the
    # output, and the figure simply never appeared. Measured: a 0.25 MB frame
    # arrived, a 0.75 MB frame did not.
    payload <- substring(line, at + nchar(k$sentinel), nchar(line))
    parsed <- tryCatch(fromJSON(payload), error = function(e) NULL)
    if (is.null(parsed)) return(list(list(type = "stdout", text = line)))
    # The EXACT bytes ride along with the parsed frame, because the supervisor
    # relays this to a browser and re-encoding it there is lossy in three ways
    # at once: jsonlite's default `digits = 4` rounded every number the worker
    # sent (1.2e-5 arrived as 0, and exports wrote that zero), the default
    # `null` rendering turned an absent field into `{}` rather than null, and
    # the simplification above collapses `bins: [500]` back to `bins: 500` —
    # undoing the very I() wrapping worker.R applies to prevent it.
    #
    # The parse stays: routing reads $type and $id, and a faithful re-encode
    # from a nested list measures 225 ms/frame against 4 ms, far too slow for
    # a single-threaded event loop. So the supervisor routes on the parse and
    # forwards the ORIGINAL text. See relay_frame() in serve.R.
    attr(parsed, "raw") <- payload
    prefix <- substring(line, 1L, at - 1L)
    if (nzchar(prefix)) list(list(type = "stdout", text = prefix), parsed) else list(parsed)
}
