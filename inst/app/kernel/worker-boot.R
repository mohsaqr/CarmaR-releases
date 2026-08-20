#!/usr/bin/env Rscript
#
# Boot shim — the ONLY job is to sys.source() the real worker.
#
# Why it exists: `Rscript worker.R` feeds the file through R's REPL-style
# reader, which handles worker.R's single ~1,800-line local() expression
# pathologically — 2–4 s pass before the first statement runs, and the first
# chunk a user runs waits out that boot. `sys.source()` parses the whole file
# in one pass (0.02 s) and evaluates the identical code. Same semantics,
# thirteen times faster to ready. Measured 2026-08-20; see LEARNINGS.md.
#
# The worker resolves its OWN directory from --file= (for sniff.R/chooser.R);
# this shim lives in the same directory, so that resolution still lands right.
# The sentinel still arrives as the first trailing argument — commandArgs()
# reads the process arguments, which sys.source() does not touch.
local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  here <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  } else getwd()
  sys.source(file.path(here, "worker.R"), envir = globalenv(), keep.source = FALSE)
})
