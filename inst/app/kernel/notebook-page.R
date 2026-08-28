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

#' The newest built notebook across every place one can land.
#'
#' @param here  the directory holding serve.R (the repo's spike/, or a
#'   distribution's kernel/ folder).
#' @return Path to an HTML file, or "" when no build is present.
carmar_notebook_page <- function(here) {
  # ALL locations pooled, best VERSION wins — updaters drop new files in a
  # dist/ (the app bundle's, or CARMAR_DIST: the per-user dir carmar::run()
  # announces) while the bundle carries its own. `.beta.min` is ignored so a
  # debuggable build is served during development.
  user_dist <- Sys.getenv("CARMAR_DIST", "")
  dirs <- c(if (nzchar(user_dist)) user_dist,
            file.path(here, "..", "dist"), file.path(here, ".."))
  built <- unlist(lapply(dirs, function(d)
    list.files(d, pattern = "^carmar_V.*[^n]\\.html$", full.names = TRUE)))
  if (length(built) == 0L) return("")
  v <- tryCatch(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", basename(built))),
                error = function(e) NULL)
  if (!is.null(v)) return(built[order(v, decreasing = TRUE)][1L])
  sort(built, decreasing = TRUE)[1L]                 # unparseable name: old rule
}
