# chooser.R — the operating system's own file dialog, opened by R.
#
# A browser cannot do this. `<input type="file">` does open the real OS
# picker, but it hands JavaScript a File object with a NAME and no path —
# deliberately, and the File System Access API withholds the path too. The
# kernel needs a path, because the whole promise is that YOUR R reads YOUR
# file from YOUR disk.
#
# So the dialog is opened by the side that already lives on that disk. R shells
# out to the platform's own chooser — AppleScript on macOS, a WinForms dialog
# on Windows, zenity or kdialog on Linux — and gets back a real path. The
# notebook's in-page browser stays as the fallback for a session where none of
# those exist (a container, an SSH box with no display), because "no dialog"
# must never mean "no import".
#
# The command BUILDER is a pure function so it can be tested without a screen;
# only the thin runner touches the OS.

CHOOSER_TIMEOUT <- 300           # seconds; a dialog left open must not wedge R

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

#' Escape a string for embedding in an AppleScript double-quoted literal.
#'
#' The prompt and the starting directory both reach AppleScript as source. A
#' quote in either would end the literal and the remainder would be read as
#' script — the same injection shape as the R string escaping in the code
#' generator, and it gets the same treatment.
as_applescript_string <- function(x) {
  x <- gsub("\\", "\\\\", as.character(x), fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  paste0("\"", gsub("[\r\n]", " ", x), "\"")
}

#' Escape a string for a single-quoted PowerShell literal (doubling quotes).
as_powershell_string <- function(x) {
  paste0("'", gsub("'", "''", gsub("[\r\n]", " ", as.character(x)), fixed = TRUE), "'")
}

#' The command that opens this platform's file dialog.
#'
#' @param mode "file", "dir" or "save". Save is the platform's own save sheet:
#'   it returns a path that need not exist yet, and the OS itself asks about
#'   replacing an existing file — a question the in-app browser never asked.
#' @param start Directory the dialog opens in; NULL for the platform default.
#' @param prompt Dialog title.
#' @param default_name Save mode's pre-filled file name.
#' @param sysname `Sys.info()[["sysname"]]`; a parameter so tests can ask for
#'   a platform they are not running on.
#' @param have A predicate answering "is this binary available"; a parameter
#'   for the same reason.
#' @return list(command, args, script) or NULL when this platform has no
#'   chooser. `script` is the generated dialog source where there is one
#'   (AppleScript, PowerShell) and NULL otherwise — named, so no caller ever
#'   has to reach into `args` by position to find it.
chooser_command <- function(mode = "file", start = NULL, prompt = "Choose",
                            default_name = NULL,
                            sysname = Sys.info()[["sysname"]],
                            have = function(bin) nzchar(Sys.which(bin))) {
  stopifnot(mode %in% c("file", "dir", "save"))
  usable_start <- is.character(start) && length(start) == 1L && nzchar(start) &&
    dir.exists(start)
  usable_name <- is.character(default_name) && length(default_name) == 1L &&
    nzchar(default_name)

  if (identical(sysname, "Darwin")) {
    # `tell application "System Events" ... activate` is what puts the dialog
    # in FRONT. Without it the picker opens behind the browser window and the
    # notebook simply looks frozen — the user is waiting for a dialog they
    # cannot see.
    verb <- switch(mode, dir = "choose folder", save = "choose file name",
                   "choose file")
    loc <- if (usable_start) {
      # Parentheses are syntax, not decoration: `default location POSIX file`
      # is parsed as adjacent parameter/class names by osacompile. Coercing
      # the POSIX path first gives choose file the alias it expects.
      paste0(" default location (POSIX file ",
             as_applescript_string(normalizePath(start)), ")")
    } else ""
    nm <- if (identical(mode, "save") && usable_name) {
      paste0(" default name ", as_applescript_string(default_name))
    } else ""
    script <- paste0(
      'tell application "System Events"\n',
      '  activate\n',
      '  set theItem to ', verb, ' with prompt ', as_applescript_string(prompt), nm, loc, '\n',
      'end tell\n',
      'POSIX path of theItem'
    )
    return(list(command = "osascript", args = c("-e", script), script = script))
  }

  if (identical(sysname, "Windows")) {
    dialog <- if (identical(mode, "dir")) {
      paste0(
        "$d = New-Object System.Windows.Forms.FolderBrowserDialog; ",
        if (usable_start) paste0("$d.SelectedPath = ", as_powershell_string(start), "; ") else "",
        "if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) ",
        "{ [Console]::Out.Write($d.SelectedPath) }")
    } else if (identical(mode, "save")) {
      paste0(
        "$d = New-Object System.Windows.Forms.SaveFileDialog; ",
        if (usable_start) paste0("$d.InitialDirectory = ", as_powershell_string(start), "; ") else "",
        if (usable_name) paste0("$d.FileName = ", as_powershell_string(default_name), "; ") else "",
        "$d.OverwritePrompt = $true; ",
        "if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) ",
        "{ [Console]::Out.Write($d.FileName) }")
    } else {
      paste0(
        "$d = New-Object System.Windows.Forms.OpenFileDialog; ",
        if (usable_start) paste0("$d.InitialDirectory = ", as_powershell_string(start), "; ") else "",
        "$d.Filter = 'Data files|*.csv;*.tsv;*.txt;*.xlsx;*.xls;*.rds;*.sav;*.dta;*.json;*.parquet|All files|*.*'; ",
        "if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) ",
        "{ [Console]::Out.Write($d.FileName) }")
    }
    script <- paste0(
      "Add-Type -AssemblyName System.Windows.Forms; ",
      "$f = New-Object System.Windows.Forms.Form; $f.TopMost = $true; ",
      dialog)
    return(list(command = "powershell",
                args = c("-NoProfile", "-NonInteractive", "-STA", "-Command", script),
                script = script))
  }

  # Linux and the rest: whichever desktop chooser is actually installed.
  if (have("zenity")) {
    start_arg <- if (usable_start) {
      base <- paste0(sub("/+$", "", normalizePath(start)), "/")
      # In save mode the --filename carries the suggested name too.
      if (identical(mode, "save") && usable_name) paste0(base, default_name) else base
    } else NULL
    return(list(command = "zenity", script = NULL, args = c(
      "--file-selection",
      switch(mode, dir = "--directory", save = "--save"),
      paste0("--title=", prompt),
      if (!is.null(start_arg)) paste0("--filename=", start_arg))))
  }
  if (have("kdialog")) {
    start_arg <- if (usable_start) {
      if (identical(mode, "save") && usable_name)
        file.path(normalizePath(start), default_name)
      else normalizePath(start)
    } else "."
    return(list(command = "kdialog", script = NULL, args = c(
      switch(mode, dir = "--getexistingdirectory", save = "--getsavefilename",
             "--getopenfilename"),
      start_arg,
      "--title", prompt)))
  }
  NULL
}

#' Did the chooser come back empty because the user pressed Cancel?
#'
#' Every one of these dialogs reports cancellation as a NON-ZERO EXIT, which
#' is indistinguishable from a crash unless you look. Treating it as an error
#' puts a red message in front of someone who simply changed their mind, so
#' cancellation is its own outcome all the way up to the wizard.
#'
#' The reading is PER BACKEND, because "non-zero and silent" means opposite
#' things. zenity and kdialog report Cancel exactly that way. osascript in a
#' process that CANNOT present UI at all exits 2 instantly and silently (the
#' 2026-08-07 empirics in lib/dialogs.js) — and calling that "cancelled"
#' swallowed the one signal that should have sent the caller to the in-app
#' browser instead. A real osascript cancel always names itself (-128).
chooser_cancelled <- function(status, output, command = NULL) {
  if (identical(as.integer(status), 0L)) return(!nzchar(trimws(paste(output, collapse = ""))))
  txt <- tolower(paste(output, collapse = " "))
  if (grepl("user canceled|user cancelled|-128", txt)) return(TRUE)
  if (!is.null(command) && command %in% c("zenity", "kdialog")) {
    return(!nzchar(trimws(txt)))
  }
  FALSE
}

#' Actually run a chooser command.
#'
#' processx first, because it spawns the binary DIRECTLY. `system2()` pastes
#' its arguments into a shell command line WITHOUT quoting them, which shreds
#' a multi-line AppleScript into separate shell commands — the observed
#' failure was `sh: line 1: activate: command not found`. shQuote() fixes it
#' on Unix, but Windows quoting for `powershell -Command` is a minefield, and
#' not involving a shell at all sidesteps the entire question.
#'
#' @param spec From `chooser_command()`.
#' @return list(status, output)
run_chooser <- function(spec) {
  if (nzchar(system.file(package = "processx"))) {
    res <- processx::run(spec$command, spec$args, error_on_status = FALSE,
                         timeout = CHOOSER_TIMEOUT, stderr_to_stdout = TRUE)
    return(list(status = res$status,
                output = strsplit(res$stdout %||% "", "\n", fixed = TRUE)[[1L]]))
  }
  out <- suppressWarnings(system2(spec$command, shQuote(spec$args),
                                  stdout = TRUE, stderr = TRUE,
                                  timeout = CHOOSER_TIMEOUT))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, output = as.character(out))
}

#' Open the OS file dialog and return what was chosen.
#'
#' @param run The runner; a parameter so the tests can exercise every outcome
#'   without a dialog appearing on somebody's screen.
#' @return list(path=) on success, list(cancelled=TRUE), or list(error=) /
#'   list(unsupported=TRUE) — never a thrown condition, because a missing
#'   zenity must degrade to the in-page browser rather than end the command.
choose_path <- function(mode = "file", start = NULL, prompt = "Choose",
                        default_name = NULL, run = run_chooser) {
  spec <- tryCatch(chooser_command(mode, start, prompt, default_name),
                   error = function(e) NULL)
  if (is.null(spec)) return(list(unsupported = TRUE))
  if (!nzchar(Sys.which(spec$command))) return(list(unsupported = TRUE))
  res <- tryCatch(
    run(spec),
    error = function(e) structure(class = "carmar_fail", list(msg = conditionMessage(e))),
    interrupt = function(i) structure(class = "carmar_fail", list(msg = "interrupted"))
  )
  if (inherits(res, "carmar_fail")) return(list(error = res$msg))
  status <- res$status
  if (is.null(status) || is.na(status)) status <- 1L    # timeout: no status
  out <- res$output
  if (chooser_cancelled(status, out, spec$command)) return(list(cancelled = TRUE))
  if (!identical(as.integer(status), 0L)) {
    return(list(error = paste("the file dialog failed:", paste(out, collapse = " "))))
  }
  # zenity can print a GTK warning to the same stream; the path is the last
  # non-empty line. Opening, it is the only line naming something that
  # exists; saving, the chosen path DOES NOT exist yet, so the test is that
  # its parent folder does.
  lines <- Filter(nzchar, trimws(out))
  hit <- if (identical(mode, "save")) {
    Find(function(p) dir.exists(dirname(p)), rev(lines))
  } else {
    Find(file.exists, rev(lines))
  }
  if (is.null(hit)) return(list(error = "the dialog returned nothing readable"))
  list(path = normalizePath(hit, mustWork = FALSE))
}
