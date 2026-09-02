# notebook-page.R — which built notebook is the CURRENT one.
#
# Shared by serve.R (what to announce and serve) and tools/app/launch.sh (what
# to open when REJOINING a session that is already running). It has to be one
# implementation: serve.R computes its announcement once, at startup, so a
# session started before a new build keeps announcing the old file — and a
# double-click that rejoins it opened that stale version. The launcher must be
# able to ask the same question again at open time.
#
# Character sort is wrong for versions twice over (V0.9 > V0.12, V0.13 > V1.0);
# numeric_version knows. That is the whole reason this is R and not a shell glob.

#' The newest built notebook carried by this installed product.
#'
#' @param here  the directory holding serve.R (the repo's spike/, or a
#'   distribution's kernel/ folder).
#' @param build Optional exact release identity. A running kernel passes its
#'   stamped build so an independently downloaded page cannot replace the
#'   matching page underneath a live supervisor.
#' @return Path to an HTML file, or "" when no matching build is present.
carmar_notebook_page <- function(here, build = "") {
  # Only product-owned locations are eligible. Historical package releases
  # accepted CARMAR_DIST and let an unsigned per-user download replace the
  # page executed beside a trusted kernel. Full-product updates now replace
  # the signed/notarized installation, so that mutable override is both
  # unnecessary and a supply-chain bypass. `.beta.min` remains ignored so a
  # debuggable product-owned build is served during development.
  dirs <- c(file.path(here, "..", "dist"), file.path(here, ".."))
  built <- unlist(lapply(dirs, function(d)
    list.files(d, pattern = "^carmar_V.*[^n]\\.html$", full.names = TRUE)))
  if (length(built) == 0L) return("")
  if (nzchar(build) && !identical(build, "unknown")) {
    built <- built[basename(built) == paste0("carmar_V", build, ".html")]
    if (length(built) == 0L) return("")
  }
  v <- tryCatch(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", basename(built))),
                error = function(e) NULL)
  if (!is.null(v)) return(built[order(v, decreasing = TRUE)][1L])
  sort(built, decreasing = TRUE)[1L]                 # unparseable name: old rule
}
