# run.R — the whole public surface of the carmar package: start it, stop it.
#
# The package is a DELIVERY VEHICLE, not an application layer: inst/app holds
# the same serve.R/worker.R and the same single-file notebook that every other
# door (CarmaR.app, `npm run kernel`) serves. run() is launch.sh translated
# into portable R — which is what makes it the classroom answer: R is the one
# runtime every student in an R course already has, on macOS and Windows
# alike, and files written by R carry no quarantine bit for Gatekeeper to
# reject.

#' Launch CarmaR
#'
#' Starts the local CarmaR kernel — or reuses one already running on the same
#' port — and opens the notebook in the default browser. The kernel is a
#' separate R process; closing the R session that called `run()` does not
#' close the notebook.
#'
#' @param port Port for the kernel. The default 4747 is deliberate and fixed:
#'   browsers keep a page's saved notebooks per address, so a kernel that
#'   moved ports would greet the student with an empty notebook every time.
#' @param open Open the browser once the kernel answers? Default `TRUE`.
#' @return Invisibly, the notebook URL (with its access token).
#' @examples
#' \dontrun{
#' carmar::run()
#' }
#' @export
run <- function(port = 4747, open = TRUE) {
  stopifnot(is.numeric(port), length(port) == 1L, port > 0, port < 65536)
  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)
  url_file <- file.path(state, paste0("url-", port))

  # Second call means "take me back", never "start a second kernel".
  if (file.exists(url_file)) {
    u <- readLines(url_file, warn = FALSE)[1]
    if (nzchar(u) && kernel_alive(u)) {
      if (open) utils::browseURL(u)
      message("CarmaR is already running: ", u)
      return(invisible(u))
    }
  }

  # Starting a kernel is the moment that matters for freshness (a new class
  # day): check the fast channel quietly, never letting it block or fail the
  # launch. Mid-session, carmar::upgrade() does the same by hand.
  try(upgrade(quiet = TRUE), silent = TRUE)

  serve <- system.file("app", "kernel", "serve.R", package = "carmar")
  if (!nzchar(serve)) stop("carmar is not installed correctly (serve.R is missing) - reinstall the package.")
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  log <- file.path(state, paste0("kernel-", port, ".log"))

  p <- processx::process$new(
    rscript, serve,
    env = c(Sys.getenv(), CARMAR_PORT = as.character(port),
            CARMAR_DIST = file.path(state, "dist")),
    stdout = log, stderr = "2>&1",
    cleanup = FALSE                     # the notebook outlives this session
  )

  # serve.R announces {"url": ...} on stdout. The WHOLE log is scanned, not
  # just line one — a startup warning ahead of the announcement must not make
  # a healthy kernel look dead.
  u <- NULL
  for (i in seq_len(240)) {
    lines <- tryCatch(readLines(log, warn = FALSE), error = function(e) character(0))
    hit <- unlist(regmatches(lines, regexpr('"url":"[^"]+"', lines)))
    if (length(hit)) { u <- sub('"$', "", sub('^"url":"', "", hit[1])); break }
    if (!p$is_alive()) break
    Sys.sleep(0.5)
  }
  if (is.null(u)) {
    tail_lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
    # A port held by some OTHER program is the one startup failure a student
    # can meet — and it deserves plain words, not a socket error.
    if (any(grepl("address already in use|EADDRINUSE|Failed to create server",
                  tail_lines, ignore.case = TRUE))) {
      stop("Port ", port, " is already used by another program on this ",
           "computer. Quit that program, or start CarmaR on a different ",
           "port:  carmar::run(port = ", port + 1, ")")
    }
    stop("CarmaR did not start. The kernel log is at: ", log,
         if (length(tail_lines)) paste0("\nLast lines:\n",
           paste(utils::tail(tail_lines, 5), collapse = "\n")) else "")
  }
  # serve.R falls back to a random free port when the asked-for one is busy —
  # better than dying, but saved notebooks are kept PER PORT (per browser
  # origin), so a silent fallback greets the student with an empty notebook.
  # Say it in plain words instead.
  got <- suppressWarnings(as.integer(sub("^http://127\\.0\\.0\\.1:(\\d+)/.*$", "\\1", u)))
  if (!is.na(got) && got != port) {
    message("NOTE: port ", port, " is busy (another program is using it), so ",
            "CarmaR opened on port ", got, " instead. Your saved notebooks ",
            "live per port - to get your usual notebook back, quit whatever ",
            "uses port ", port, " and run carmar::run() again.")
  }
  writeLines(u, url_file)
  if (open) utils::browseURL(u)
  # ALWAYS printed, clickable in the RStudio console — the browser not
  # opening (locked-down default browser, broken association) must never
  # strand a student without the address.
  message("CarmaR is running. If no browser tab opened, click or copy:\n  ", u)
  invisible(u)
}

#' Stop the CarmaR kernel
#'
#' Asks the kernel on `port` to shut down, through its own token-gated
#' `/shutdown` — the clean path, the same one the notebook's Quit uses.
#'
#' @param port The port `run()` used. Default 4747.
#' @return Invisibly, `TRUE` if a kernel was asked to stop, `FALSE` if none
#'   was running.
#' @export
stop_kernel <- function(port = 4747) {
  state <- tools::R_user_dir("carmar", "data")
  url_file <- file.path(state, paste0("url-", port))
  if (!file.exists(url_file)) { message("No CarmaR kernel recorded on port ", port, "."); return(invisible(FALSE)) }
  u <- readLines(url_file, warn = FALSE)[1]
  if (!nzchar(u) || !kernel_alive(u)) {
    unlink(url_file)
    message("No CarmaR kernel is running on port ", port, ".")
    return(invisible(FALSE))
  }
  ask <- sub("/\\?token=", "/shutdown?token=", u, fixed = FALSE)
  ok <- tryCatch({ slurp(ask); TRUE }, error = function(e) FALSE)
  if (ok) { unlink(url_file); message("CarmaR kernel on port ", port, " is stopping.") }
  invisible(ok)
}

#' Upgrade the CarmaR notebook
#'
#' Downloads the newest notebook from the course's release page into your
#' user data folder, where the kernel finds it on the next reload. This is
#' the FAST channel: the notebook is one small file and a release is live
#' seconds after the instructor publishes it — no package rebuild, no
#' r-universe wait, no reinstall. `run()` also checks quietly on every
#' starts a kernel, so calling this yourself is only needed mid-session.
#'
#' @param quiet Say nothing unless something was updated? Default `FALSE`.
#' @return Invisibly, `TRUE` if a newer notebook was downloaded.
#' @export
upgrade <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  feed <- Sys.getenv("CARMAR_FEED",
    "https://api.github.com/repos/mohsaqr/CarmaR-releases/releases/latest")
  dist <- file.path(tools::R_user_dir("carmar", "data"), "dist")
  dir.create(dist, recursive = TRUE, showWarnings = FALSE)

  rel <- tryCatch(jsonlite::fromJSON(paste(slurp(feed), collapse = "\n"),
                                     simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(rel)) { say("The release page is unreachable - no update check."); return(invisible(FALSE)) }
  assets <- rel$assets %||% list()
  names_ <- vapply(assets, function(a) a$name %||% "", character(1))
  hit <- grepl("^carmar_V[0-9.]+\\.html$", names_)
  if (!any(hit)) { say("No notebook in the latest release."); return(invisible(FALSE)) }
  best <- which(hit)[order(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", names_[hit])),
                           decreasing = TRUE)][1]
  want_name <- names_[[best]]
  want_v <- numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", want_name))

  have <- list.files(c(dist, dirname(system.file("app", "kernel", package = "carmar"))),
                     pattern = "^carmar_V.*\\.html$")
  have_v <- if (length(have)) max(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", have))) else numeric_version("0")
  if (want_v <= have_v) { say("CarmaR is up to date (", as.character(have_v), ")."); return(invisible(FALSE)) }

  target <- file.path(dist, want_name)
  part <- paste0(target, ".part")
  ok <- tryCatch({
    old <- options(timeout = 120); on.exit(options(old), add = TRUE)
    utils::download.file(assets[[best]]$browser_download_url, part,
                         mode = "wb", quiet = TRUE) == 0
  }, error = function(e) FALSE)
  if (ok && file.exists(part) && file.size(part) > 10000) {
    file.rename(part, target)
    say("Updated to ", want_name, " - reload the CarmaR tab (or run carmar::run()).")
    return(invisible(TRUE))
  }
  unlink(part)
  say("The update download failed - CarmaR keeps working on its current version.")
  invisible(FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Is the kernel behind this notebook URL answering? @noRd
kernel_alive <- function(u) {
  health <- sub("/\\?token=.*$", "/health", u)
  tryCatch(length(slurp(health)) > 0, error = function(e) FALSE)
}

#' readLines over http with a short timeout, connection always closed. @noRd
slurp <- function(u) {
  old <- options(timeout = 3)
  on.exit(options(old), add = TRUE)
  con <- url(u)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  readLines(con, warn = FALSE)
}
