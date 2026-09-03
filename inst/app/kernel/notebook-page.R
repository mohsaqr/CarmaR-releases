# notebook-page.R — the ONE page a kernel of a given build serves and opens.
#
# The answer is a pin, never a search: a kernel stamped `<build>` belongs to
# exactly `carmar_V<build>.html`, looked for in the two product-owned places a
# built notebook can be (the repo's ../dist, then the folder beside kernel/ in
# a distribution). Nothing else is eligible — not a higher version that
# happens to sit in the same folder, not a `.beta.min` twin, not an unsigned
# per-user download (historical releases accepted CARMAR_DIST and let one
# replace the page executed beside a trusted kernel; a full-product update
# replaces the installation instead, so that override is gone).
#
# This file exists so that serve.R (what to announce and serve) and
# tools/app/launch.sh (what to open when REJOINING a session already running,
# in pure shell against /health's `kernel_build`) agree on the PIN. There used
# to be a "highest version wins" fallback here, sorted by numeric_version; it
# is what let a page from a newer install be served underneath an older live
# supervisor, and it is deliberately not coming back.

#' The built notebook that belongs to one kernel build.
#'
#' @param here  the directory holding serve.R (the repo's spike/, or a
#'   distribution's kernel/ folder).
#' @param build The kernel's stamped release identity (`CARMAR_KERNEL_BUILD`).
#'   Required and non-empty; a source checkout with no stamp at all passes
#'   "unknown", which pins nothing and is answered with "".
#' @return Path to `carmar_V<build>.html`, or "" when that exact file is
#'   present in neither location.
carmar_notebook_page <- function(here, build) {
  stopifnot(
    "`here` must be a single directory path" =
      is.character(here) && length(here) == 1L && !is.na(here) && nzchar(here),
    "`build` must be a single non-empty string (the kernel's stamped build)" =
      is.character(build) && length(build) == 1L && !is.na(build) && nzchar(build)
  )
  wanted <- paste0("carmar_V", build, ".html")
  candidates <- c(file.path(here, "..", "dist", wanted), file.path(here, "..", wanted))
  present <- candidates[file.exists(candidates)]
  if (length(present) == 0L) return("")
  present[[1L]]
}
