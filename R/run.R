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
#' @param file Optional document path delivered to the notebook. File
#'   associations use this with `new = FALSE`, so a double-click reuses the
#'   live session when possible and starts one only when necessary.
#' @return Invisibly, the versioned notebook file URL with its kernel selected.
#' @examples
#' \dontrun{
#' carmar::run()
#' carmar::run(new = FALSE) # reopen the session on port 4747
#' }
#' @importFrom httpuv startServer
#' @export
run <- function(port = 4747, open = TRUE, new = TRUE, file = NULL) {
  stopifnot(is.numeric(port), length(port) == 1L, is.finite(port),
            port == as.integer(port), port > 0, port < 65536)
  stopifnot(is.logical(new), length(new) == 1L, !is.na(new))
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file) ||
        !file.exists(file) || dir.exists(file)) {
      stop("`file` must name one existing document.")
    }
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  }
  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)

  # The signed update CHECK runs on every call, including reuse. It may cache a
  # verified full-product installer and report it in the UI; it never replaces
  # package/app/page files in the background.
  try(upgrade(quiet = TRUE), silent = TRUE)

  live <- live_kernel_urls(state)
  at_port <- live[vapply(live, kernel_port, integer(1)) == as.integer(port)]
  if (!new && length(at_port)) {
    u <- unname(at_port[[1]])
    delivered <- is.null(file) || deliver_open_file(u, file)
    if (delivered) {
      page <- notebook_launch_url(u)
      if (open) open_notebook(page, state)
      message("CarmaR is already running behind: ", page)
      return(invisible(page))
    }
  }

  # Independent sessions get stable adjacent origins. That preserves browser
  # storage per session. There is deliberately no session-count gate: `run()`
  # is an explicit user launch, and it starts the session it was asked for.
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
  update_script <- trimws(Sys.getenv("CARMAR_UPDATE_SCRIPT", ""))
  if (!nzchar(update_script)) update_script <- system.file("app", "update.sh", package = "carmar")

  p <- processx::process$new(
    rscript, serve,
    env = c(Sys.getenv(), CARMAR_PORT = as.character(port),
            CARMAR_OPEN_FILE = if (is.null(file)) "" else file,
            CARMAR_UPDATE_SCRIPT = update_script,
            CARMAR_STATE = state),
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
  if (is.null(page) || !nzchar(page)) page <- notebook_launch_url(u)
  if (open) open_notebook(page, state)
  # ALWAYS printed, clickable in the RStudio console — the browser not
  # opening (locked-down default browser, broken association) must never
  # strand a student without the address.
  message("CarmaR is running behind this notebook file:\n  ", page)
  invisible(page)
}

#' Deliver a double-clicked document to an already-running local supervisor.
#' The worker remains the authority on readable paths, size, and file type; this
#' request only puts the path into the same pending-open slot cold launch uses.
#' @noRd
deliver_open_file <- function(kernel_url, file) {
  target <- paste0(sub("/$", "", kernel_url), "/open?file=",
                   utils::URLencode(file, reserved = TRUE))
  con <- NULL
  tryCatch({
    con <- url(target, open = "rb")
    readBin(con, "raw", n = 1024L)
    TRUE
  }, error = function(e) FALSE, finally = {
    if (!is.null(con)) try(close(con), silent = TRUE)
  })
}

#' Serve CarmaR to other people on the network
#'
#' The classroom deployment: one CarmaR per person, each started under that
#' person's own operating-system account on its own port, with TLS and login
#' handled entirely by a reverse proxy in front. CarmaR itself never sees a
#' password — it trusts an identity header, and only when the request came
#' from the proxy's own address.
#'
#' Binding off loopback INVERTS one of CarmaR's trust rules, and this verb
#' exists so that inversion is stated rather than stumbled into. On
#' `127.0.0.1`, a client that sends no `Origin` header is allowed outright:
#' no Origin means it is not a browser, so it is a process belonging to this
#' same user, who could have run `Rscript` anyway. Reachable from the network,
#' the identical header means an unauthenticated stranger — so here it is
#' refused. The kernel will not start at all without `hosts`, and will not
#' start unauthenticated unless you say `allow_unauthenticated = TRUE` out
#' loud.
#'
#' A worked Caddy configuration is in `docs/server.md` inside the package
#' sources; `readme()` opens it.
#'
#' @param hosts The hostnames readers will type, e.g. `"stats.example.edu"`.
#'   Required: off loopback the `Host` allow-list is the only thing standing
#'   between this kernel and a DNS-rebinding attack, so an empty one is
#'   refused rather than defaulted.
#' @param port Port for this person's kernel (default 4747). One port per
#'   person; the proxy maps each account to its own.
#' @param bind Address to listen on (default `"0.0.0.0"`). Use the proxy-facing
#'   interface address when the machine has more than one.
#' @param user_header The header the proxy sets to the authenticated user's
#'   name (default `"X-Forwarded-User"`; Caddy's `forward_auth` writes
#'   `Remote-User`).
#' @param trusted_proxy Address(es) the proxy connects from (default
#'   `"127.0.0.1"`). The identity header is read from these and ignored from
#'   anywhere else — it is an identity only for as long as nobody but the
#'   proxy can reach the port.
#' @param origins Extra exact browser origins allowed to open a socket, e.g.
#'   `"https://stats.example.edu"`. The kernel's own address is always allowed.
#' @param allow_unauthenticated Serve with no proxy identity at all? Default
#'   `FALSE`. `TRUE` means everyone who can reach this port can run R as the
#'   account this kernel runs under. There is no safe network on which that is
#'   a shortcut.
#' @return Invisibly, a one-row `data.frame` with `port`, `bind`, `url` (the
#'   kernel's own address), `hosts` (comma-separated), `authenticated`
#'   (logical) and `log` (path to the kernel log).
#' @examples
#' \dontrun{
#' carmar::serve_shared(hosts = "stats.example.edu")
#' carmar::serve_shared(hosts = "stats.example.edu", port = 4801,
#'                      user_header = "Remote-User")
#' }
#' @export
serve_shared <- function(hosts, port = 4747, bind = "0.0.0.0",
                         user_header = "X-Forwarded-User",
                         trusted_proxy = "127.0.0.1",
                         origins = character(0),
                         allow_unauthenticated = FALSE) {
  stopifnot(
    "`hosts` must name at least one hostname readers will use" =
      is.character(hosts) && length(hosts) >= 1L && all(nzchar(trimws(hosts))),
    "`port` must be a single port number" =
      is.numeric(port) && length(port) == 1L && is.finite(port) &&
      port == as.integer(port) && port > 0 && port < 65536,
    "`bind` must be a single address" =
      is.character(bind) && length(bind) == 1L && nzchar(trimws(bind)),
    "`user_header` must be a single header name" =
      is.character(user_header) && length(user_header) == 1L,
    "`trusted_proxy` must name at least one address" =
      is.character(trusted_proxy) && length(trusted_proxy) >= 1L,
    "`origins` must be a character vector" = is.character(origins),
    "`allow_unauthenticated` must be TRUE or FALSE" =
      is.logical(allow_unauthenticated) && length(allow_unauthenticated) == 1L &&
      !is.na(allow_unauthenticated))

  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)

  serve <- system.file("app", "kernel", "serve.R", package = "carmar")
  if (!nzchar(serve)) stop("carmar is not installed correctly (serve.R is missing) - reinstall the package.")
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  log <- file.path(state, paste0("kernel-", port, ".log"))

  # An empty CARMAR_TRUST_PROXY is not the same as unset: serve.R compares it
  # to "1", so the unauthenticated case must pass the flag off rather than on.
  cfg <- c(CARMAR_PORT = as.character(as.integer(port)),
           CARMAR_BIND = trimws(bind),
           CARMAR_HOSTS = paste(trimws(hosts), collapse = ","),
           # A shared deployment is updated by its fleet operator, never by a
           # reader's browser or an inherited desktop-app environment.
           CARMAR_UPDATE_SCRIPT = "")
  if (allow_unauthenticated) {
    cfg <- c(cfg, CARMAR_ALLOW_UNAUTHENTICATED = "1")
  } else {
    cfg <- c(cfg, CARMAR_TRUST_PROXY = "1",
             CARMAR_USER_HEADER = trimws(user_header),
             CARMAR_TRUSTED_PROXY = paste(trimws(trusted_proxy), collapse = ","))
  }
  if (length(origins)) cfg <- c(cfg, CARMAR_ORIGINS = paste(trimws(origins), collapse = ","))

  p <- processx::process$new(rscript, serve, env = c(Sys.getenv(), cfg),
                             stdout = log, stderr = "2>&1", cleanup = FALSE)

  u <- NULL
  for (i in seq_len(240)) {
    lines <- tryCatch(readLines(log, warn = FALSE), error = function(e) character(0))
    hit <- grep('"url"', lines, value = TRUE)
    if (length(hit)) {
      rec <- tryCatch(jsonlite::fromJSON(hit[[1]], simplifyVector = TRUE),
                      error = function(e) NULL)
      if (!is.null(rec$url)) { u <- rec$url; break }
    }
    if (!p$is_alive()) break
    Sys.sleep(0.5)
  }
  if (is.null(u)) {
    said <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
    refused <- grep("refuses to start", said, value = TRUE)
    # serve.R's own words, not a rewrite of them: it knows exactly which part
    # of the posture was unsafe and says so in a full sentence.
    if (length(refused)) stop(paste(refused, collapse = "\n"), call. = FALSE)
    stop("CarmaR did not start. The kernel log is at: ", log,
         if (length(said)) paste0("\nLast lines:\n",
           paste(utils::tail(said, 5), collapse = "\n")) else "", call. = FALSE)
  }

  message("CarmaR is serving on ", bind, ":", port,
          if (allow_unauthenticated)
            " with NO authentication - anyone who can reach this port runs R as this account."
          else paste0(", trusting ", user_header, " from ",
                      paste(trusted_proxy, collapse = ", "), "."))
  message("  Point your reverse proxy at ", u, " and see docs/server.md.")

  invisible(data.frame(
    port = as.integer(port), bind = trimws(bind), url = u,
    hosts = paste(trimws(hosts), collapse = ","),
    authenticated = !allow_unauthenticated, log = log,
    stringsAsFactors = FALSE))
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
    tryCatch(notebook_launch_url(u), error = function(e) NA_character_),
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

#' Refuse the removed unsigned notebook synchronisation path
#'
#' Retained as an internal compatibility stub for callers from older package
#' versions. It deliberately never reads or copies the historical per-user
#' `dist` directory: updates are accepted only as signed full products.
#'
#' @param ... Ignored legacy arguments.
#' @return Invisibly, `FALSE`.
#' @keywords internal
sync_notebook <- function(...) invisible(FALSE)

#' Execute the product-owned updater on this platform. @noRd
run_updater <- function(script, args, stdout = FALSE, stderr = FALSE) {
  sysname <- unname(Sys.info()[["sysname"]])
  if (identical(sysname, "Windows")) {
    choices <- Sys.which(c("powershell.exe", "powershell"))
    command <- unname(choices[nzchar(choices)][1L])
    if (!length(command) || is.na(command) || !grepl("[.]ps1$", script, ignore.case = TRUE)) {
      return(list(ok = FALSE, output = character()))
    }
    command_args <- c("-NoLogo", "-NoProfile", "-NonInteractive",
                      "-ExecutionPolicy", "Bypass", "-File", shQuote(script), args)
  } else {
    command <- "sh"
    command_args <- c(shQuote(script), args)
  }
  output <- tryCatch(system2(command, command_args, stdout = stdout, stderr = stderr),
                     error = function(e) structure(character(), status = 1L))
  status <- if (is.numeric(output) && length(output) == 1L) output else attr(output, "status")
  if (is.null(status)) status <- 0L
  list(ok = identical(as.integer(status), 0L), output = as.character(output))
}

#' Check for a signed full-product CarmaR update
#'
#' Invokes the signed updater carried by CarmaR.app (or by this package). The
#' updater verifies a detached release signature, unified component versions,
#' artifact SHA-256 and the platform publisher identity. A check may cache a
#' verified installer, but it never replaces executable files in the
#' background. Unsigned legacy GitHub release metadata is not accepted.
#'
#' @param quiet Say nothing unless something was updated? Default `FALSE`.
#' @return Invisibly, `TRUE` if anything was updated.
#' @export
upgrade <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  explicit <- Sys.getenv("CARMAR_UPDATE_SCRIPT", "")
  candidates <- c(explicit,
    path.expand("~/Applications/CarmaR.app/Contents/Resources/update.sh"),
    "/Applications/CarmaR.app/Contents/Resources/update.sh",
    system.file("app", "update.sh", package = "carmar"))
  script <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (!length(script) || is.na(script)) {
    say("No signed updater is installed; CarmaR left the installed version unchanged.")
    return(invisible(FALSE))
  }
  if (!identical(unname(Sys.info()["sysname"]), "Darwin") && !nzchar(explicit)) {
    say("Use the signed CarmaR installer to update this platform.")
    return(invisible(FALSE))
  }
  checked <- run_updater(script, "check", stdout = FALSE, stderr = FALSE)
  if (!isTRUE(checked$ok)) {
    say("The signed update check could not run; CarmaR was not changed.")
    return(invisible(FALSE))
  }
  line <- run_updater(script, "status", stdout = TRUE, stderr = FALSE)$output
  fields <- if (length(line)) strsplit(line[[1]], "\t", fixed = TRUE)[[1]] else character(0)
  state <- if (length(fields)) fields[[1]] else "unknown"
  available <- if (length(fields) >= 3L) fields[[3]] else ""
  if (state == "available") {
    say("A verified CarmaR ", available, " update is ready in Settings > R session.")
  } else if (!quiet && state %in% c("refused", "error", "unconfigured")) {
    detail <- if (length(fields) >= 6L) fields[[6]] else "The signed update check did not complete."
    say(detail)
  }
  invisible(identical(state, "available"))
}

#' Removed legacy unsigned notebook update door.
#' @keywords internal
upgrade_notebook <- function(rel, say) {
  stop("Legacy unsigned notebook updates were removed; use the signed full-product updater.",
       call. = FALSE)
}

#' Removed legacy unsigned R-package install door.
#' @keywords internal
upgrade_package <- function(rel, say) {
  stop("Legacy unsigned package updates were removed; use the signed full-product updater.",
       call. = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' What the kernel behind this notebook URL says about itself on `/health`,
#' as the parsed list (`ok`, `worker`, `kernel_build`, `capabilities`, ...),
#' or `NULL` when the URL is not a loopback kernel URL or nothing answers
#' with JSON there. Every question about a live kernel — is it alive, which
#' build is it, can it pair — reads this one body. @noRd
kernel_health <- function(u) {
  if (!valid_kernel_url(u)) return(NULL)
  health <- paste0(kernel_base(u), "/health")
  rec <- tryCatch({
    jsonlite::fromJSON(paste(slurp(health), collapse = "\n"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)
  if (is.list(rec)) rec else NULL
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

#' The notebook file a live kernel belongs to, as a `file://` URL with the
#' kernel selector — and the launch capability the kernel announced — hidden
#' in its fragment.
#'
#' A pin, never a search. The kernel names its own build on `/health`
#' (`kernel_build`), and its page is exactly `carmar_V<build>.html`: from this
#' package's `inst/app` first, then a source checkout's `dist/` (the developer
#' fallback, `dist`). A higher version found in either place is somebody
#' else's page and is never substituted. There is deliberately no per-user
#' location: an unsigned download beside a trusted kernel was a supply-chain
#' bypass (the same reasoning as spike/notebook-page.R), and a full-product
#' update replaces the installation instead.
#'
#' @param kernel_url A loopback kernel URL (`http://127.0.0.1:<port>/`).
#' @param dist The developer fallback directory holding `carmar_V*.html`.
#' @return One `file://…carmar_V<build>.html#kernel=<port>[&pair=<cap>]` URL.
#'   Stops, naming the build, when the kernel does not report one or its
#'   page is installed nowhere. @noRd
notebook_launch_url <- function(kernel_url, dist = file.path(getwd(), "dist")) {
  build <- kernel_health(kernel_url)$kernel_build
  if (!is.character(build) || length(build) != 1L || is.na(build) ||
      !grepl("^[A-Za-z0-9._-]+$", build)) {
    stop("The CarmaR kernel behind ", kernel_base(kernel_url),
         " did not report its build; it is not answering, or predates the pinned page.",
         call. = FALSE)
  }
  wanted <- paste0("carmar_V", build, ".html")
  candidates <- c(system.file("app", wanted, package = "carmar"),
                  file.path(dist, wanted))
  present <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(present)) {
    stop("The notebook for CarmaR ", build, " (", wanted, ") is not installed",
         " beside this package or in ", dist, "; reinstall CarmaR ", build, ".",
         call. = FALSE)
  }
  file <- normalizePath(present[[1L]], winslash = "/")
  prefix <- if (startsWith(file, "/")) "file://" else "file:///"
  port <- kernel_port(kernel_url)
  cap <- recorded_pair(port)
  paste0(prefix, utils::URLencode(file, reserved = FALSE),
         "#kernel=", port, if (nzchar(cap)) paste0("&pair=", cap))
}

#' The file-page launch capability a kernel announced, read back from its
#' shared runtime record (`~/.carmar/run/kernel-<port>.json`, written by
#' serve.R). Since 0.60.88 a notebook opened from disk is refused without it
#' (FILE_ORIGIN), so a page URL regenerated here has to carry it or it reopens
#' a notebook the kernel will not talk to. `""` when the record is absent or
#' predates the capability. @noRd
recorded_pair <- function(port) {
  port <- suppressWarnings(as.integer(port))
  if (length(port) != 1L || is.na(port)) return("")
  runtime_dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                            file.path(path.expand("~"), ".carmar", "run"))
  f <- file.path(runtime_dir, sprintf("kernel-%d.json", port))
  if (!file.exists(f)) return("")
  rec <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE),
                  error = function(e) NULL)
  page <- if (is.list(rec) && length(rec$file) == 1L && is.character(rec$file)) rec$file else ""
  hit <- regmatches(page, regexpr("(?<=[#&]pair=)[A-Za-z0-9]+", page, perl = TRUE))
  if (length(hit)) hit else ""
}

#' Open the notebook page in the browser with its fragment intact.
#'
#' `browseURL()` ends in `open` (macOS), `ShellExecute` (Windows) or
#' `xdg-open`, each of which hands a file:// URL to the browser as a PATH:
#' the `#kernel=…&pair=…` fragment never arrives, the page walks the ports,
#' finds the kernel that was just started for it and is refused — "kernel
#' closed" beside a healthy R. So the browser is given a one-line page it
#' can carry, whose only job is to navigate to the real URL. 0600 in the
#' package's state directory; earlier trampolines are swept on every launch,
#' so at most one — the newest session's — is ever on disk. @noRd
open_notebook <- function(page, state = tools::R_user_dir("carmar", "data"),
                          browse = utils::browseURL) {
  if (!grepl("#", page, fixed = TRUE)) return(browse(page))
  unlink(list.files(state, pattern = "^open-[0-9]+\\.html$", full.names = TRUE))
  tramp <- file.path(state, sprintf("open-%d.html", Sys.getpid()))
  esc <- gsub("<", "&lt;", gsub("\"", "&quot;", gsub("&", "&amp;", page,
                                                     fixed = TRUE), fixed = TRUE), fixed = TRUE)
  writeLines(paste0(
    '<!doctype html><meta charset="utf-8"><title>CarmaR</title>',
    '<meta http-equiv="refresh" content="0;url=', esc, '">',
    '<a id="go" href="', esc, '">Opening CarmaR&hellip;</a>',
    '<script>location.replace(document.getElementById("go").href)</script>'),
    tramp)
  Sys.chmod(tramp, "0600")
  browse(tramp)
  invisible(tramp)
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

#' readLines over http with a short timeout, connection always closed. @noRd
slurp <- function(u) {
  old <- options(timeout = 3)
  on.exit(options(old), add = TRUE)
  con <- url(u)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  readLines(con, warn = FALSE)
}
