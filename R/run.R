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
#' Starts an independent local CarmaR kernel and opens the notebook in the
#' default browser. `run()` never asks permission to do what it was told: you
#' typed the command, so the session starts. The kernel is a
#' separate R process; closing the R session that called `run()` does not
#' close the notebook.
#'
#' @param port Port for the kernel. The default 4747 is deliberate and fixed:
#'   browsers keep a page's saved notebooks per address, so a kernel that
#'   moved ports would greet the student with an empty notebook every time.
#' @param open Open the browser once the kernel answers? Default `TRUE`.
#' @param new Start an independent session? Default `TRUE`. Set `FALSE` to
#'   reopen a live session on `port` when there is one.
#' @param allow_more What to do when `MAX_SESSIONS` are already live. `NULL`
#'   (default) and `TRUE` both start the session; `FALSE` reopens the newest
#'   existing one instead. Never a prompt.
#' @return Invisibly, the versioned notebook file URL with its kernel selected.
#' @examples
#' \dontrun{
#' carmar::run()
#' carmar::run(new = FALSE) # reopen the session on port 4747
#' }
#' @importFrom httpuv startServer
#' @export
#' The point at which `run()` starts SAYING how many sessions are live.
#'
#' Each session is a whole R process plus a browser origin of its own, so the
#' number is about the machine. It is no longer a gate here: a dialog on a
#' typed command is a question whose answer the user already gave, and the one
#' it kept asking was wrong anyway — until 0.60.1 a kernel could not tell a
#' closed laptop from an attached notebook, so sessions nobody had open kept
#' answering, kept counting, and pushed every launch into a prompt about
#' sessions that did not exist. The real cap lives in the supervisor, where
#' the caller is a WEB PAGE rather than a person: an uncapped sibling-start on
#' a browser's say-so is a resource-exhaustion button (see MAX_SESSIONS in
#' spike/serve.R, enforced there and unchanged).
MAX_SESSIONS <- 10L

run <- function(port = 4747, open = TRUE, new = TRUE, allow_more = NULL) {
  stopifnot(is.numeric(port), length(port) == 1L, is.finite(port),
            port == as.integer(port), port > 0, port < 65536)
  stopifnot(is.logical(new), length(new) == 1L, !is.na(new))
  if (!is.null(allow_more)) stopifnot(is.logical(allow_more), length(allow_more) == 1L,
                                      !is.na(allow_more))
  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)

  # The fast channel runs on EVERY call — including the reuse path below.
  # The first version upgraded only on a fresh launch, and the reuse branch
  # returned before ever reaching it: exactly when someone re-runs run() to
  # "get the update", nothing updated. Quiet, and never allowed to block or
  # fail the launch.
  try(upgrade(quiet = TRUE), silent = TRUE)
  # ...and the freshest notebook is copied where EVERY server generation
  # looks, so even a long-lived kernel from an older install serves it on
  # the very next reload.
  try(sync_notebook(), silent = TRUE)

  live <- live_kernel_urls(state)
  at_port <- live[vapply(live, kernel_port, integer(1)) == as.integer(port)]
  if (!new && length(at_port)) {
    u <- unname(at_port[[1]])
    page <- notebook_launch_url(u, state)
    if (open) utils::browseURL(page)
    message("CarmaR is already running behind: ", page)
    return(invisible(page))
  }

  if (new && length(live) >= MAX_SESSIONS) {
    message(length(live), " CarmaR sessions are already running. ",
            "carmar::stop_kernel(\"all\") stops them all; ",
            "carmar::sessions() lists them.")
  }
  if (new && length(live) >= MAX_SESSIONS && !confirm_more_sessions(length(live), allow_more)) {
    u <- unname(live[[1]])
    page <- notebook_launch_url(u, state)
    if (open) utils::browseURL(page)
    message("Kept the existing ", length(live), " CarmaR sessions; reopened: ", page)
    return(invisible(page))
  }

  # Independent sessions get stable adjacent origins. That preserves browser
  # storage per session while making the ordinary ten predictable.
  used <- vapply(live, kernel_port, integer(1))
  chosen <- NA_integer_
  for (candidate in seq.int(as.integer(port), 65535L)) {
    if (!candidate %in% used && port_is_free(candidate)) { chosen <- candidate; break }
  }
  port <- chosen
  if (is.na(port)) stop("No free local port is available for another CarmaR session.")
  url_file <- file.path(state, paste0("url-", port))

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
  page <- NULL
  for (i in seq_len(240)) {
    lines <- tryCatch(readLines(log, warn = FALSE), error = function(e) character(0))
    hit <- grep('"url"', lines, value = TRUE)
    if (length(hit)) {
      rec <- tryCatch(jsonlite::fromJSON(hit[[1]], simplifyVector = TRUE),
                      error = function(e) NULL)
      if (!is.null(rec$url)) { u <- rec$url; page <- rec$file %||% NULL; break }
    }
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
  # serve.R still falls back to a random free port if a race claims the chosen
  # port between our check and startup —
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
  if (!is.na(got) && got != port) url_file <- file.path(state, paste0("url-", got))
  # Discovery state is read concurrently by new R sessions. Publish with a
  # rename so readers see either the previous complete record or the new one,
  # never a half-written URL after a crash.
  part <- paste0(url_file, ".part")
  writeLines(u, part, useBytes = TRUE)
  if (!file.rename(part, url_file)) {
    # Windows cannot atomically rename over an existing file. A stale record
    # on this just-claimed port is harmless; retain a portable fallback.
    copied <- file.copy(part, url_file, overwrite = TRUE)
    unlink(part)
    if (!isTRUE(copied)) stop(
      "CarmaR started, but its session record could not be saved: ", url_file)
  }
  if (is.null(page) || !nzchar(page)) page <- notebook_launch_url(u, state)
  if (open) utils::browseURL(page)
  # ALWAYS printed, clickable in the RStudio console — the browser not
  # opening (locked-down default browser, broken association) must never
  # strand a student without the address.
  message("CarmaR is running behind this notebook file:\n  ", page)
  invisible(page)
}

#' Start CarmaR for a published book
#'
#' Reuses a pairing-capable kernel on `port`. If that port contains an older
#' CarmaR kernel from before published-page pairing was added, it is cleanly
#' restarted from the currently installed package first. This avoids the
#' misleading local `/pair` 404 that an otherwise healthy stale process would
#' return after the package itself had been upgraded.
#'
#' @param port Fixed loopback port used by the published page (default 4747).
#' @param open Open the full local notebook too? Published books normally leave
#'   this `FALSE`.
#' @return Invisibly, the local notebook URL returned by [run()].
#' @export
run_published <- function(port = 4747, open = FALSE) {
  stopifnot(is.numeric(port), length(port) == 1L, is.finite(port),
            port == as.integer(port), port > 0, port < 65536)
  port <- as.integer(port)
  state <- tools::R_user_dir("carmar", "data")
  live <- live_kernel_urls(state)
  at_port <- live[vapply(live, kernel_port, integer(1)) == port]

  if (length(at_port) && !kernel_supports_published_pairing(at_port[[1]])) {
    message("Restarting the older CarmaR kernel on port ", port,
            " so published pages can pair with it.")
    if (!isTRUE(stop_kernel(port))) {
      stop("The older CarmaR kernel on port ", port,
           " could not be stopped. Run carmar::stop_kernel(", port,
           ") and try again.", call. = FALSE)
    }
    for (i in seq_len(50L)) {
      if (port_is_free(port)) break
      Sys.sleep(0.1)
    }
    if (!port_is_free(port)) {
      stop("Port ", port, " did not become available after CarmaR stopped.",
           call. = FALSE)
    }
  }

  run(port = port, open = open, new = FALSE)
}

#' List CarmaR sessions
#'
#' Every CarmaR kernel recorded on this machine — package launches (the
#' `url-<port>` state records) and every other door (the shared
#' `~/.carmar/run/kernel-<port>.json` runtime registry) — one row per port,
#' each health-checked at call time. Read-only: dead records are reported,
#' not removed (`run()` prunes them on its next launch). Kernels also stop
#' themselves after sitting idle with no notebook attached (`CARMAR_LINGER`,
#' default 600 seconds), so a dead row is the ordinary afterlife of a
#' closed tab.
#'
#' @return A `data.frame`, one row per recorded session, ports ascending:
#'   `port` (integer), `alive` (logical), `title` (what the notebook attached
#'   to the session calls itself; `NA` until a page names one), `listen`
#'   (logical — a headless kernel waiting for published pages, started by the
#'   menu's Listen for Web Pages or the keep-ready agent), `page` (the
#'   notebook file URL that reopens the session; `NA` when it is not running),
#'   `source` (`"package"` for `run()` launches, `"runtime"` for other doors).
#' @examples
#' \dontrun{
#' carmar::sessions()
#' carmar::stop_kernel("all")
#' }
#' @export
sessions <- function() {
  state <- tools::R_user_dir("carmar", "data")
  runtime_dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                            file.path(path.expand("~"), ".carmar", "run"))
  state_files <- list.files(state, pattern = "^url-[0-9]+$", full.names = TRUE)
  runtime_files <- list.files(runtime_dir, pattern = "^kernel-[0-9]+\\.json$",
                              full.names = TRUE)
  read_record <- function(f, expected_port) {
    declared <- expected_port
    title <- NA_character_
    listen <- FALSE
    u <- if (grepl("[.]json$", f)) {
      rec <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE),
                      error = function(e) NULL)
      if (is.null(rec) || !is.list(rec)) {
        return(list(url = "", valid = FALSE, title = title, listen = listen))
      }
      if (!is.null(rec$port)) declared <- suppressWarnings(as.integer(rec$port))
      if (length(rec$title) == 1L && is.character(rec$title) && nzchar(rec$title)) {
        title <- rec$title
      }
      listen <- isTRUE(rec$listen)
      rec$url %||% ""
    } else tryCatch(readLines(f, warn = FALSE, n = 1L),
                    error = function(e) "")
    u <- if (length(u) == 1L && is.character(u)) u else ""
    list(url = u, valid = valid_kernel_url(u, expected_port) &&
           length(declared) == 1L && !is.na(declared) && declared == expected_port,
         title = title, listen = listen)
  }
  files <- c(state_files, runtime_files)
  origin <- rep(c("package", "runtime"),
                c(length(state_files), length(runtime_files)))
  ports <- as.integer(sub("^(url-|kernel-)([0-9]+).*$", "\\2", basename(files)))
  records <- Map(read_record, files, ports)
  urls <- vapply(records, `[[`, character(1), "url")
  valid <- vapply(records, `[[`, logical(1), "valid")
  titles <- vapply(records, `[[`, character(1), "title")
  listens <- vapply(records, `[[`, logical(1), "listen")
  alive <- valid & vapply(seq_along(urls), function(i)
    isTRUE(valid[[i]]) && kernel_alive(urls[[i]]), logical(1))
  # Validate all duplicates before choosing. A malformed package record must
  # not hide a live runtime record for the same port.
  priority <- order(ports, -as.integer(alive), -as.integer(valid),
                    match(origin, c("package", "runtime")))
  keep <- priority[!duplicated(ports[priority])]
  keep <- keep[order(ports[keep])]
  ports <- ports[keep]; origin <- origin[keep]; urls <- urls[keep]; alive <- alive[keep]
  titles <- titles[keep]; listens <- listens[keep]
  page <- rep(NA_character_, length(ports))
  page[alive] <- vapply(urls[alive], function(u)
    tryCatch(notebook_launch_url(u, state), error = function(e) NA_character_),
    character(1), USE.NAMES = FALSE)
  data.frame(port = ports, alive = alive, title = titles, listen = listens,
             page = page, source = origin, stringsAsFactors = FALSE)
}

#' Stop the CarmaR kernel
#'
#' Asks the kernel on `port` to shut down through its loopback-only `/shutdown`
#' endpoint — the clean path, the same one the notebook's Quit uses. Pass
#' `"all"` to stop every live session `sessions()` can see.
#'
#' @param port The port `run()` used (default 4747), or `"all"`.
#' @return Invisibly, `TRUE` if a kernel was asked to stop, `FALSE` if none
#'   was running; for `"all"`, one such value per live session (empty when
#'   none were running).
#' @export
stop_kernel <- function(port = 4747) {
  if (identical(port, "all")) {
    live <- sessions()
    live <- live[live$alive, , drop = FALSE]
    if (!nrow(live)) { message("No CarmaR kernels are running."); return(invisible(logical(0))) }
    return(invisible(vapply(live$port, stop_kernel, logical(1))))
  }
  stopifnot(is.numeric(port), length(port) == 1L, is.finite(port),
            port == as.integer(port), port > 0, port < 65536)
  port <- as.integer(port)
  state <- tools::R_user_dir("carmar", "data")
  url_file <- file.path(state, paste0("url-", port))
  known <- sessions()
  row <- known[known$port == port, , drop = FALSE]
  if (!nrow(row)) { message("No CarmaR kernel recorded on port ", port, "."); return(invisible(FALSE)) }
  if (!row$alive[[1]]) {
    unlink(url_file)
    message("No CarmaR kernel is running on port ", port, ".")
    return(invisible(FALSE))
  }
  ask <- sprintf("http://127.0.0.1:%d/shutdown", port)
  ok <- tryCatch({ slurp(ask); TRUE }, error = function(e) FALSE)
  if (ok) { unlink(url_file); message("CarmaR kernel on port ", port, " is stopping.") }
  invisible(ok)
}

#' Put the newest notebook where every kernel looks
#'
#' The contract of `run()` is "serve the latest" — not "check for updates".
#' Downloads land in the per-user dist; a CURRENT kernel pools that folder
#' at request time, but a long-lived kernel from an older install only
#' scans the package's own app/ folder. So on every `run()`, whatever the
#' newest `carmar_V*.html` on this machine is gets copied into app/ too —
#' after that, EVERY kernel generation serves it on its very next reload.
#'
#' @param app The package app directory every serve.R scans.
#' @param dist The per-user dist directory where downloads land.
#' @return Invisibly, `TRUE` if a newer notebook was put in place.
#' @keywords internal
sync_notebook <- function(app = dirname(system.file("app", "kernel", package = "carmar")),
                          dist = file.path(tools::R_user_dir("carmar", "data"), "dist")) {
  v_of <- function(f) numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", basename(f)))
  newest <- function(d) {
    f <- list.files(d, pattern = "^carmar_V[0-9.]+\\.html$", full.names = TRUE)
    if (!length(f)) return(NULL)
    f[order(v_of(f), decreasing = TRUE)][1]
  }
  best <- newest(dist)
  if (is.null(best)) return(invisible(FALSE))
  have <- newest(app)
  if (!is.null(have) && v_of(have) >= v_of(best)) return(invisible(FALSE))
  # A library the user cannot write to just means this kernel serves from
  # the per-user dist instead — file.copy returning FALSE is not an error.
  invisible(isTRUE(suppressWarnings(
    file.copy(best, file.path(app, basename(best)), overwrite = TRUE))))
}

#' Upgrade CarmaR — the notebook and the package
#'
#' Reads the release feed once and updates BOTH halves of an installation:
#' the newest notebook is downloaded into your user data folder, and when the
#' release is newer than the installed carmar package, the release's own
#' package is installed too. `run()` calls this automatically. On macOS, a
#' successful package update also refreshes CarmaR.app and CarmaR Helper.app
#' from that package. Neither check may block or fail a launch: any trouble is
#' a quiet no-op and CarmaR starts on what it has.
#'
#' @param quiet Say nothing unless something was updated? Default `FALSE`.
#' @return Invisibly, `TRUE` if anything was updated.
#' @export
upgrade <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  feed <- Sys.getenv("CARMAR_FEED",
    "https://api.github.com/repos/mohsaqr/CarmaR-releases/releases/latest")
  rel <- tryCatch(jsonlite::fromJSON(paste(slurp(feed), collapse = "\n"),
                                     simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(rel)) { say("The release page is unreachable - no update check."); return(invisible(FALSE)) }
  nb <- upgrade_notebook(rel, say)
  pkg <- upgrade_package(rel, say)
  invisible(isTRUE(nb) || isTRUE(pkg))
}

#' The notebook half of `upgrade()`: newest `carmar_V*.html` into the
#' per-user dist. One release JSON in, `TRUE` if a newer file landed.
#' @keywords internal
upgrade_notebook <- function(rel, say) {
  dist <- file.path(tools::R_user_dir("carmar", "data"), "dist")
  dir.create(dist, recursive = TRUE, showWarnings = FALSE)
  assets <- rel$assets %||% list()
  names_ <- vapply(assets, function(a) a$name %||% "", character(1))
  hit <- grepl("^carmar_V[0-9.]+\\.html$", names_)
  if (!any(hit)) { say("No notebook in the latest release."); return(FALSE) }
  best <- which(hit)[order(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", names_[hit])),
                           decreasing = TRUE)][1]
  want_name <- names_[[best]]
  want_v <- numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", want_name))

  have <- list.files(c(dist, dirname(system.file("app", "kernel", package = "carmar"))),
                     pattern = "^carmar_V.*\\.html$")
  have_v <- if (length(have)) max(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", have))) else numeric_version("0")
  if (want_v <= have_v) { say("CarmaR is up to date (", as.character(have_v), ")."); return(FALSE) }

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
    return(TRUE)
  }
  unlink(part)
  say("The update download failed - CarmaR keeps working on its current version.")
  FALSE
}

#' The package half of `upgrade()`: when the release tag outruns the
#' installed carmar, install the release's own `carmar.tar.gz` — the same
#' payload the feed announced, so the check and the install can never
#' disagree about what "latest" means. The kernel a launch spawns AFTER this
#' reads serve.R from the new files, so the update takes effect immediately;
#' only the R code of the session that ran the update stays old until the
#' next launch. Verified by re-reading the installed version — a failed
#' `R CMD INSTALL` only warns, it does not throw.
#' @keywords internal
upgrade_package <- function(rel, say) {
  want <- tryCatch(numeric_version(sub("^[vV]", "", rel$tag_name %||% "")),
                   error = function(e) NULL)
  have <- tryCatch(utils::packageVersion("carmar"), error = function(e) NULL)
  if (is.null(want) || is.null(have) || want <= have) return(FALSE)
  assets <- rel$assets %||% list()
  names_ <- vapply(assets, function(a) a$name %||% "", character(1))
  hit <- which(names_ == "carmar.tar.gz")
  if (!length(hit)) return(FALSE)

  say("Updating the carmar package ", as.character(have), " -> ",
      as.character(want), " ...")
  part <- tempfile("carmar-", fileext = ".tar.gz")
  on.exit(unlink(part), add = TRUE)
  got <- tryCatch({
    old <- options(timeout = 300); on.exit(options(old), add = TRUE)
    utils::download.file(assets[[hit[1]]]$browser_download_url, part,
                         mode = "wb", quiet = TRUE) == 0
  }, error = function(e) FALSE)
  if (!got || !file.exists(part) || file.size(part) < 1000) {
    say("The package update download failed - CarmaR keeps its current version.")
    return(FALSE)
  }
  ok <- tryCatch({
    suppressWarnings(utils::install.packages(part, repos = NULL,
                                             type = "source", quiet = TRUE))
    isTRUE(utils::packageVersion("carmar") >= want)
  }, error = function(e) FALSE)
  if (ok) {
    say("The carmar package is now ", as.character(want), ".")
    # A macOS package also carries the current CarmaR.app and menu helper.
    # Use a fresh R process so it loads install_app() from the package that was
    # just written, rather than this still-running namespace. This keeps the
    # carmar:// handler and embedded kernel on the same release as the package.
    bundles <- system.file("app", "macos", "carmar-apps.tar.gz",
                           package = "carmar")
    if (identical(unname(Sys.info()["sysname"]), "Darwin") && file.exists(bundles)) {
      package_lib <- dirname(find.package("carmar"))
      lib_literal <- encodeString(package_lib, quote = '"')
      expr <- paste0(".libPaths(c(", lib_literal,
        ",.libPaths())); try(carmar::install_app(quiet=TRUE,helper=TRUE),silent=TRUE)")
      refreshed <- tryCatch(system2(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", shQuote(expr)), stdout = FALSE, stderr = FALSE) == 0L,
        error = function(e) FALSE)
      if (refreshed) say("CarmaR.app and its menu helper are now on the same release.")
      else say("The package updated, but the application bundles could not be refreshed.")
    }
  } else say("The package update could not be installed - CarmaR keeps its current version.")
  ok
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Is the kernel behind this notebook URL answering? @noRd
kernel_health <- function(u) {
  if (!valid_kernel_url(u)) return(FALSE)
  health <- paste0(kernel_base(u), "/health")
  tryCatch({
    jsonlite::fromJSON(paste(slurp(health), collapse = "\n"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)
}

#' Is the kernel behind this notebook URL answering? @noRd
kernel_alive <- function(u) {
  rec <- kernel_health(u)
  is.list(rec) && isTRUE(rec$ok) && isTRUE(rec$worker)
}

#' Can this live kernel host the published-page consent bridge? @noRd
kernel_supports_published_pairing <- function(u) {
  rec <- kernel_health(u)
  is.list(rec) && isTRUE(rec$ok) && isTRUE(rec$worker) &&
    "published-pairing-v3" %in% (rec$capabilities %||% character())
}

#' Is this a loopback-only CarmaR transport URL, optionally on one port? @noRd
valid_kernel_url <- function(u, expected_port = NULL) {
  if (!is.character(u) || length(u) != 1L || is.na(u) || !nzchar(u)) return(FALSE)
  ok <- grepl("^http://(127\\.0\\.0\\.1|localhost|\\[::1\\]):[0-9]{1,5}/?(\\?token=[^#[:space:]]*)?$",
              u, perl = TRUE)
  p <- kernel_port(u)
  ok && !is.na(p) && p > 0L && p < 65536L &&
    (is.null(expected_port) || identical(p, as.integer(expected_port)))
}

#' URL origin for both current clean records and legacy tokenized records. @noRd
kernel_base <- function(u) sub("/+$", "", sub("/\\?token=.*$", "", u))

#' Versioned notebook file with a hidden kernel selector in its fragment. @noRd
notebook_launch_url <- function(kernel_url,
                                state = tools::R_user_dir("carmar", "data")) {
  app <- dirname(system.file("app", "kernel", package = "carmar"))
  dirs <- unique(c(file.path(state, "dist"), if (nzchar(app)) app,
                   file.path(getwd(), "dist")))
  files <- unlist(lapply(dirs, function(d)
    list.files(d, pattern = "^carmar_V[0-9.]+\\.html$", full.names = TRUE)))
  if (!length(files)) stop("The versioned CarmaR notebook file is missing; reinstall CarmaR.")
  versions <- numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", basename(files)))
  file <- normalizePath(files[order(versions, decreasing = TRUE)][1], winslash = "/")
  prefix <- if (startsWith(file, "/")) "file://" else "file:///"
  paste0(prefix, utils::URLencode(file, reserved = FALSE),
         "#kernel=", kernel_port(kernel_url))
}

#' Port number carried by a clean CarmaR URL. @noRd
kernel_port <- function(u) {
  hit <- regmatches(u, regexpr("(?<=:)[0-9]+(?=/|$)", u, perl = TRUE))
  if (!length(hit)) NA_integer_ else suppressWarnings(as.integer(hit))
}

#' Live package-launched kernels, newest records first. @noRd
live_kernel_urls <- function(state = tools::R_user_dir("carmar", "data")) {
  state_files <- list.files(state, pattern = "^url-[0-9]+$", full.names = TRUE)
  runtime_dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                            file.path(path.expand("~"), ".carmar", "run"))
  runtime_files <- list.files(runtime_dir, pattern = "^kernel-[0-9]+\\.json$",
                              full.names = TRUE)
  files <- c(state_files, runtime_files)
  if (!length(files)) return(character())
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  urls <- vapply(files, function(f) {
    expected <- suppressWarnings(as.integer(sub(
      "^(url-|kernel-)([0-9]+).*$", "\\2", basename(f))))
    declared <- expected
    u <- if (grepl("[.]json$", f)) {
      rec <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE),
                      error = function(e) NULL)
      if (is.null(rec) || !is.list(rec)) "" else {
        if (!is.null(rec$port)) declared <- suppressWarnings(as.integer(rec$port))
        rec$url %||% ""
      }
    } else tryCatch(readLines(f, warn = FALSE, n = 1L), error = function(e) "")
    valid <- length(u) == 1L && is.character(u) &&
      valid_kernel_url(u, expected) && length(declared) == 1L &&
      !is.na(declared) && declared == expected
    if (valid && kernel_alive(u)) u
    else {
      # Package state is ours to prune. Runtime discovery files are shared
      # across every launcher; remove only a record whose kernel is dead.
      unlink(f)
      ""
    }
  }, character(1))
  live <- unname(urls[nzchar(urls)])
  live[!duplicated(vapply(live, kernel_base, character(1)))]
}

#' Is a loopback port unused? @noRd
port_is_free <- function(port) {
  con <- suppressWarnings(tryCatch(socketConnection("127.0.0.1", port,
    open = "r+", blocking = TRUE, timeout = 1), error = function(e) NULL))
  if (is.null(con)) return(TRUE)
  close(con)
  FALSE
}

#' Explicitly authorize a sixth-or-later independent session. @noRd
#' Whether a launch past the cap proceeds. It does, unless someone said not to.
#'
#' This used to raise a dialog — `askYesNo`, an osascript panel, `winDialog`,
#' or an outright `stop()` — every single launch once the count was reached.
#' Three things were wrong with it. The count included kernels that were dead
#' to everyone except the health check; the question interrupted a command the
#' user had just typed, which is not consent-seeking but nagging; and refusing
#' silently reopened some OTHER session, so the answer to "start another?" was
#' a notebook the reader did not ask for. Explicit refusal still works, for
#' scripts that want it: `allow_more = FALSE`, or CARMAR_ALLOW_MORE=0.
confirm_more_sessions <- function(count, allow_more = NULL) {
  if (!is.null(allow_more)) return(isTRUE(allow_more))
  env <- tolower(Sys.getenv("CARMAR_ALLOW_MORE", ""))
  if (env %in% c("0", "false", "no")) return(FALSE)
  TRUE
}

#' readLines over http with a short timeout, connection always closed. @noRd
slurp <- function(u) {
  old <- options(timeout = 3)
  on.exit(options(old), add = TRUE)
  con <- url(u)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  readLines(con, warn = FALSE)
}
