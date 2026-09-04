# run.R - the public surface of the carmar package: start a kernel, list the
# kernels, stop them.
#
# The package is a DELIVERY VEHICLE, not an application layer: inst/app holds
# the same serve.R/worker.R and the same single-file notebook that every other
# door (the desktop app, `npm run kernel`) serves. run() is the launcher
# translated into portable R - which is what makes it the classroom answer: R
# is the one runtime every student in an R course already has, on macOS,
# Windows and Linux alike.
#
# Nothing here contacts the network. Every HTTP request this file makes goes
# to 127.0.0.1, to a kernel the user started.

#' Launch CarmaR
#'
#' Starts an independent local CarmaR kernel and opens the notebook in the
#' default browser. `run()` never asks permission to do what it was told: you
#' typed the command, so the session starts. The kernel is a separate R
#' process; closing the R session that called `run()` does not close the
#' notebook.
#'
#' @section What is written where:
#' `run()`, [listen()] and [serve_shared()] create the package's state
#' directory, `tools::R_user_dir("carmar", "data")`, and write there the
#' session record `url-<port>`, the kernel log `kernel-<port>.log` and a
#' one-line trampoline page the browser is handed. The kernel itself writes
#' its discovery record `~/.carmar/run/kernel-<port>.json` (mode 0600,
#' removed on a clean stop; `CARMAR_RUNTIME_DIR` relocates it). Nothing else
#' on the system is touched, and no network request leaves the machine.
#'
#' @param port Port for the kernel. The default 4747 is deliberate and fixed:
#'   browsers keep a page's saved notebooks per address, so a kernel that
#'   moved ports would greet the student with an empty notebook every time.
#'   When `port` is busy the next free port is used and said out loud.
#' @param open Open the browser once the kernel answers? Default `TRUE`.
#' @param new Start an independent session? Default `TRUE`. Set `FALSE` to
#'   reopen a live session on `port` when there is one.
#' @param file Optional document path delivered to the notebook. File
#'   associations use this with `new = FALSE`, so a double-click reuses the
#'   live session when possible and starts one only when necessary.
#' @return Invisibly, a single string: the versioned notebook `file://` URL
#'   with its kernel selected in the fragment (`#kernel=<port>`).
#' @examples
#' # Not run: starts an R process that outlives this session and opens a
#' # browser tab.
#' \dontrun{
#' carmar::run()
#' carmar::run(new = FALSE) # reopen the session on port 4747
#' }
#' @export
run <- function(port = 4747, open = TRUE, new = TRUE, file = NULL) {
  check_port(port)
  stopifnot("`new` must be TRUE or FALSE" =
              is.logical(new) && length(new) == 1L && !is.na(new),
            "`open` must be TRUE or FALSE" =
              is.logical(open) && length(open) == 1L && !is.na(open))
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file) ||
        !file.exists(file) || dir.exists(file)) {
      stop(errorCondition("`file` must name one existing document.",
                          class = "carmar_bad_file", call = NULL))
    }
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  }
  state <- state_dir()

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
  candidates <- seq.int(as.integer(port), 65535L)
  candidates <- candidates[!candidates %in% used]
  chosen <- Find(port_is_free, candidates)
  if (is.null(chosen)) {
    stop(errorCondition("No free local port is available for another CarmaR session.",
                        class = "carmar_no_port", call = NULL))
  }
  port <- chosen

  started <- spawn_kernel(port, state, c(
    CARMAR_OPEN_FILE = if (is.null(file)) "" else file))
  if (is.null(started$url)) {
    # A port held by some OTHER program is the one startup failure a student
    # can meet - and it deserves plain words, not a socket error.
    if (any(grepl("address already in use|EADDRINUSE|Failed to create server",
                  started$log_lines, ignore.case = TRUE))) {
      stop(errorCondition(paste0(
        "Port ", port, " is already used by another program on this computer. ",
        "Quit that program, or start CarmaR on a different port: ",
        "carmar::run(port = ", port + 1L, ")"),
        class = "carmar_port_taken", call = NULL))
    }
    stop(errorCondition(did_not_start(started), class = "carmar_no_start",
                        call = NULL))
  }
  u <- started$url
  # serve.R falls back to a random free port if a race claims the chosen port
  # between our check and startup - better than dying, but saved notebooks
  # are kept PER PORT (per browser origin), so a silent fallback greets the
  # student with an empty notebook. Say it in plain words instead.
  got <- kernel_port(u)
  if (!is.na(got) && got != port) {
    message("NOTE: port ", port, " is busy (another program is using it), so ",
            "CarmaR opened on port ", got, " instead. Your saved notebooks ",
            "live per port - to get your usual notebook back, quit whatever ",
            "uses port ", port, " and run carmar::run() again.")
    port <- got
  }
  record_session(state, port, u)
  page <- started$page
  if (is.null(page) || !nzchar(page)) page <- notebook_launch_url(u)
  if (open) open_notebook(page, state)
  # ALWAYS printed, clickable in the RStudio console - the browser not
  # opening (locked-down default browser, broken association) must never
  # strand a student without the address.
  message("CarmaR is running behind this notebook file:\n  ", page)
  invisible(page)
}

#' Keep a kernel listening for published pages
#'
#' Starts a headless CarmaR kernel on a FIXED port for the runnable R chunks
#' of a published web page (a 'Quarto' site rendered with the `carmar` filter,
#' see [use_publishing()]). Nothing opens: the reader keeps reading the page,
#' presses Run, and the page talks to this kernel.
#'
#' The port is a promise, not a preference: a published page carries the port
#' it will look for, so `listen()` never moves to another one. A CarmaR kernel
#' already answering on `port` is reused; a port held by anything else is an
#' error of class `carmar_port_taken`.
#'
#' A page is not trusted because it asked. Pass `site` - the page's exact
#' origin, scheme and host and nothing more - to approve that one site for
#' this kernel's lifetime. When the kernel is started here the approval rides
#' in its environment; when a kernel already answers, the approval is sent to
#' its loopback `/published/authorize` endpoint, which accepts only requests
#' from a local, non-browser process such as this one. Without `site`, the
#' page's first Run opens a local consent page and the reader approves it
#' there, once per kernel.
#'
#' @inheritSection run What is written where
#'
#' @param site The published page's origin, e.g. `"https://book.example"` or
#'   `"http://localhost:8080"`: scheme, host, optional port, no path. `NULL`
#'   (the default) approves nothing in advance.
#' @param port The fixed loopback port the published page expects (default
#'   4747, which is what the filter writes unless the author changed it).
#' @param open Also open the full local notebook against this kernel? Default
#'   `FALSE`; a published page normally needs only the kernel.
#' @return Invisibly, a one-row `data.frame` of class `carmar_listener`:
#'   `port` (integer), `url` (the kernel's loopback address), `site` (the
#'   origin approved, `NA` when none was given), `approved` (logical - was
#'   `site` approved for this kernel), `started` (logical - `TRUE` when this
#'   call started the kernel, `FALSE` when one was already listening). Its
#'   print method says where the kernel listens and for which site.
#' @examples
#' # Not run: starts an R process that outlives this session.
#' \dontrun{
#' carmar::listen("https://book.example")
#' carmar::listen()                       # approve sites from the page itself
#' }
#' # The site must be an exact origin - a path is refused before anything starts:
#' try(carmar::listen("https://book.example/chapter-1.html"))
#' @export
listen <- function(site = NULL, port = 4747, open = FALSE) {
  check_port(port)
  stopifnot("`open` must be TRUE or FALSE" =
              is.logical(open) && length(open) == 1L && !is.na(open))
  port <- as.integer(port)
  if (!is.null(site) && !valid_site(site)) {
    stop(errorCondition(paste0(
      "`site` must be an exact web origin - scheme and host, no path - such as ",
      "\"https://book.example\" or \"http://localhost:8080\"; got ",
      deparse1(site), "."), class = "carmar_bad_site", call = NULL))
  }
  state <- state_dir()
  u <- sprintf("http://127.0.0.1:%d/", port)

  if (kernel_alive(u)) {
    if (!kernel_supports_published_pairing(u)) {
      # An older CarmaR on this port cannot pair a published page and would
      # answer /pair with a 404. Replace it, on the same port, from the
      # installed package.
      message("Restarting the older CarmaR kernel on port ", port,
              " so published pages can pair with it.")
      if (!isTRUE(stop_kernel(port))) {
        stop(errorCondition(paste0(
          "The older CarmaR kernel on port ", port, " could not be stopped. ",
          "Run carmar::stop_kernel(", port, ") and try again."),
          class = "carmar_kernel_stale", call = NULL))
      }
      # A wait is a loop: give the old process up to five seconds to let go.
      for (i in seq_len(50L)) {
        if (port_is_free(port)) break
        Sys.sleep(0.1)
      }
      if (!port_is_free(port)) {
        stop(errorCondition(paste0("Port ", port, " did not become free after ",
                                   "the older CarmaR kernel stopped."),
                            class = "carmar_kernel_stale", call = NULL))
      }
      return(listen(site = site, port = port, open = open))
    }
    approved <- !is.null(site) && authorize_published_origin(u, site)
    if (!is.null(site) && !approved) {
      stop(errorCondition(paste0(
        "The CarmaR kernel on port ", port, " refused to approve ", site,
        ". It answers only a local, non-browser caller; see its log."),
        class = "carmar_not_approved", call = NULL))
    }
    started <- FALSE
  } else {
    if (!port_is_free(port)) {
      stop(errorCondition(paste0(
        "Port ", port, " is held by a program that is not a CarmaR kernel, and ",
        "a published page will look for CarmaR on exactly that port. Quit that ",
        "program, or publish the page with a different `carmar: port:`."),
        class = "carmar_port_taken", call = NULL))
    }
    env <- c(CARMAR_PORT_STRICT = "1", CARMAR_LISTEN = "1",
             CARMAR_PUBLISHED_ORIGIN = site %||% "")
    spawned <- spawn_kernel(port, state, env)
    if (is.null(spawned$url)) {
      if (any(grepl("refuses to start", spawned$log_lines, fixed = TRUE))) {
        stop(errorCondition(paste0(
          "Port ", port, " was taken while CarmaR was starting, and a ",
          "listening kernel never moves ports. ",
          paste(grep("refuses to start", spawned$log_lines, value = TRUE),
                collapse = "\n")),
          class = "carmar_port_taken", call = NULL))
      }
      stop(errorCondition(did_not_start(spawned), class = "carmar_no_start",
                          call = NULL))
    }
    u <- spawned$url
    if (!identical(kernel_port(u), port)) {
      stop(errorCondition(paste0("CarmaR answered on ", u, " instead of port ",
                                 port, "; a listening kernel must not move."),
                          class = "carmar_port_taken", call = NULL))
    }
    record_session(state, port, u)
    approved <- !is.null(site)
    started <- TRUE
  }

  if (open) open_notebook(notebook_launch_url(u), state)
  out <- data.frame(port = port, url = u, site = site %||% NA_character_,
                    approved = approved, started = started,
                    stringsAsFactors = FALSE)
  class(out) <- c("carmar_listener", "data.frame")
  message(listener_sentence(out))
  invisible(out)
}

#' @rdname listen
#' @param x A `carmar_listener`, as returned by [listen()].
#' @param ... Ignored.
#' @return `print()` returns `x` invisibly.
#' @export
print.carmar_listener <- function(x, ...) {
  cat(listener_sentence(x), "\n", sep = "")
  invisible(x)
}

#' The one sentence a listener says about itself.
#' @noRd
listener_sentence <- function(x) {
  site <- x$site[[1L]]
  paste0("CarmaR is listening on port ", x$port[[1L]],
         if (!is.na(site)) paste0(" for ", site) else " for published pages",
         " - open the page and press Run.")
}

#' Start CarmaR for a published page
#'
#' A thin alias of [listen()], kept for the readers whose page told them to
#' type `carmar::run_published()`. Every argument is passed through unchanged.
#'
#' @inheritParams listen
#' @return The `carmar_listener` data frame [listen()] returns, invisibly.
#' @examples
#' # Not run: starts an R process that outlives this session.
#' \dontrun{
#' carmar::run_published("https://book.example")
#' }
#' @export
run_published <- function(site = NULL, port = 4747, open = FALSE) {
  listen(site = site, port = port, open = open)
}

#' Serve CarmaR to other people on the network
#'
#' The classroom deployment: one CarmaR per person, each started under that
#' person's own operating-system account on its own port, with TLS and login
#' handled entirely by a reverse proxy in front. CarmaR itself never sees a
#' password - it trusts an identity header, and only when the request came
#' from the proxy's own address.
#'
#' Binding off loopback INVERTS one of CarmaR's trust rules, and this verb
#' exists so that inversion is stated rather than stumbled into. On
#' `127.0.0.1`, a client that sends no `Origin` header is allowed outright:
#' no Origin means it is not a browser, so it is a process belonging to this
#' same user, who could have run `Rscript` anyway. Reachable from the network,
#' the identical header means an unauthenticated stranger - so here it is
#' refused. The kernel will not start at all without `hosts`, and will not
#' start unauthenticated unless you say `allow_unauthenticated = TRUE` out
#' loud.
#'
#' @section The deployment:
#' The shape is `reader -> TLS reverse proxy -> 127.0.0.1:<port>`, one kernel
#' per person, each running as that person's own account so that file
#' permissions, quotas and `~` keep meaning what they already mean on the
#' machine. The proxy authenticates the reader (OIDC, SAML, LDAP - whatever
#' the institution already has) and maps the authenticated name to that
#' person's port, setting `user_header` on every forwarded request. The kernel
#' reads that header only from `trusted_proxy`, so it is an identity for
#' exactly as long as nobody but the proxy can reach the port. With Caddy, a
#' `forward_auth` block in front of `reverse_proxy localhost:<port>` does
#' both; Caddy's `forward_auth` writes `Remote-User`, hence that value for
#' `user_header`. A misconfigured kernel - off loopback with no `hosts`, or
#' with no authentication in front - does not come up degraded: it refuses to
#' start and says which part of the posture was unsafe.
#'
#' @inheritSection run What is written where
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
#'   anywhere else.
#' @param origins Extra exact browser origins allowed to open a socket, e.g.
#'   `"https://stats.example.edu"`. The kernel's own address is always allowed.
#' @param allow_unauthenticated Serve with no proxy identity at all? Default
#'   `FALSE`. `TRUE` means everyone who can reach this port can run R as the
#'   account this kernel runs under. There is no safe network on which that is
#'   a shortcut.
#' @return Invisibly, a one-row `data.frame` with `port` (integer), `bind`,
#'   `url` (the kernel's own address), `hosts` (comma-separated),
#'   `authenticated` (logical) and `log` (path to the kernel log).
#' @examples
#' # Not run: binds a network port and starts an R process that outlives
#' # this session.
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
  check_port(port)
  stopifnot(
    "`hosts` must name at least one hostname readers will use" =
      is.character(hosts) && length(hosts) >= 1L && all(nzchar(trimws(hosts))),
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
  state <- state_dir()

  # An empty CARMAR_TRUST_PROXY is not the same as unset: serve.R compares it
  # to "1", so the unauthenticated case must pass the flag off rather than on.
  cfg <- c(CARMAR_BIND = trimws(bind),
           CARMAR_HOSTS = paste(trimws(hosts), collapse = ","))
  if (allow_unauthenticated) {
    cfg <- c(cfg, CARMAR_ALLOW_UNAUTHENTICATED = "1")
  } else {
    cfg <- c(cfg, CARMAR_TRUST_PROXY = "1",
             CARMAR_USER_HEADER = trimws(user_header),
             CARMAR_TRUSTED_PROXY = paste(trimws(trusted_proxy), collapse = ","))
  }
  if (length(origins)) cfg <- c(cfg, CARMAR_ORIGINS = paste(trimws(origins), collapse = ","))

  started <- spawn_kernel(as.integer(port), state, cfg)
  if (is.null(started$url)) {
    refused <- grep("refuses to start", started$log_lines, value = TRUE)
    # serve.R's own words, not a rewrite of them: it knows exactly which part
    # of the posture was unsafe and says so in a full sentence.
    if (length(refused)) {
      stop(errorCondition(paste(refused, collapse = "\n"),
                          class = "carmar_refused", call = NULL))
    }
    stop(errorCondition(did_not_start(started), class = "carmar_no_start",
                        call = NULL))
  }

  message("CarmaR is serving on ", bind, ":", port,
          if (allow_unauthenticated)
            " with NO authentication - anyone who can reach this port runs R as this account."
          else paste0(", trusting ", user_header, " from ",
                      paste(trusted_proxy, collapse = ", "), "."))
  message("  Point your reverse proxy at ", started$url, ".")

  invisible(data.frame(
    port = as.integer(port), bind = trimws(bind), url = started$url,
    hosts = paste(trimws(hosts), collapse = ","),
    authenticated = !allow_unauthenticated, log = started$log,
    stringsAsFactors = FALSE))
}

#' List CarmaR sessions
#'
#' Every CarmaR kernel recorded on this machine - package launches (the
#' `url-<port>` state records) and every other door (the shared
#' `~/.carmar/run/kernel-<port>.json` runtime registry) - one row per port,
#' each health-checked at call time over loopback. Read-only: dead records
#' are reported, not removed (`run()` prunes them on its next launch).
#' Kernels also stop themselves after sitting idle with no notebook attached
#' (`CARMAR_LINGER`, default 600 seconds), so a dead row is the ordinary
#' afterlife of a closed tab.
#'
#' @return A `data.frame`, one row per recorded session, ports ascending:
#'   `port` (integer), `alive` (logical), `title` (what the notebook attached
#'   to the session calls itself; `NA` until a page names one), `listen`
#'   (logical - a headless kernel started by [listen()] and not yet claimed
#'   by a notebook), `page` (the notebook file URL that reopens the session;
#'   `NA` when it is not running), `source` (`"package"` for [run()]
#'   launches, `"runtime"` for other doors). Zero rows when nothing is
#'   recorded.
#' @examples
#' carmar::sessions()
#' @export
sessions <- function() {
  state <- tools::R_user_dir("carmar", "data")
  state_files <- list.files(state, pattern = "^url-[0-9]+$", full.names = TRUE)
  runtime_files <- list.files(runtime_dir(), pattern = "^kernel-[0-9]+\\.json$",
                              full.names = TRUE)
  read_record <- function(f, expected_port) {
    declared <- expected_port
    title <- NA_character_
    listen <- FALSE
    u <- if (grepl("[.]json$", f)) {
      rec <- read_json_record(f)
      if (is.null(rec)) {
        return(list(url = "", valid = FALSE, title = title, listen = listen))
      }
      if (!is.null(rec$port)) declared <- suppressWarnings(as.integer(rec$port))
      if (length(rec$title) == 1L && is.character(rec$title) && nzchar(rec$title)) {
        title <- rec$title
      }
      listen <- isTRUE(rec$listen)
      rec$url %||% ""
    } else read_first_line(f)
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
  # A kernel started by run() or listen() is recorded twice - the package's
  # url-<port> file and the kernel's own runtime record - and only the second
  # carries a title and the listen mark. Merge those per port before choosing
  # which record answers, so a listener is a listener whichever record wins.
  first_title <- function(x) { known <- x[!is.na(x)]; if (length(known)) known[[1L]] else NA_character_ }
  by_port <- as.character(ports)
  titles <- as.character(tapply(titles, by_port, first_title)[by_port])
  listens <- as.logical(tapply(listens & valid, by_port, any)[by_port])
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
    tryCatch(notebook_launch_url(u), carmar_no_page = function(e) NA_character_),
    character(1), USE.NAMES = FALSE)
  data.frame(port = ports, alive = alive, title = titles, listen = listens,
             page = page, source = origin, stringsAsFactors = FALSE)
}

#' Stop a CarmaR kernel
#'
#' Asks the kernel on `port` to shut down through its loopback-only `/shutdown`
#' endpoint - the clean path, the same one the notebook's Quit uses. Pass
#' `"all"` to stop every live session [sessions()] can see. A stopping kernel
#' removes its own `~/.carmar/run/kernel-<port>.json`; this function removes
#' the package's `url-<port>` record.
#'
#' @param port The port [run()] or [listen()] used (default 4747), or `"all"`.
#' @return For one port, invisibly `TRUE` if a kernel was asked to stop and
#'   `FALSE` if none was running there. For `"all"`, invisibly a `data.frame`
#'   with one row per live session that was asked: `port` (integer) and
#'   `stopped` (logical); zero rows when nothing was running.
#' @examples
#' # A port with no kernel behind it: nothing to stop, FALSE, no error.
#' carmar::stop_kernel(65000)
#' @export
stop_kernel <- function(port = 4747) {
  if (identical(port, "all")) {
    live <- sessions()
    live <- live[live$alive, , drop = FALSE]
    if (!nrow(live)) {
      message("No CarmaR kernels are running.")
      return(invisible(data.frame(port = integer(0), stopped = logical(0))))
    }
    stopped <- vapply(live$port, stop_kernel, logical(1))
    return(invisible(data.frame(port = live$port, stopped = unname(stopped))))
  }
  check_port(port)
  port <- as.integer(port)
  url_file <- file.path(tools::R_user_dir("carmar", "data"), paste0("url-", port))
  wanted <- port
  row <- subset(sessions(), port == wanted)
  if (!nrow(row)) {
    message("No CarmaR kernel recorded on port ", port, ".")
    return(invisible(FALSE))
  }
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

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- kernel lifecycle -----------------------------------------------------

#' A single port number, or a classed contract error.
#' @noRd
check_port <- function(port) {
  ok <- is.numeric(port) && length(port) == 1L && is.finite(port) &&
    port == as.integer(port) && port > 0 && port < 65536
  if (!ok) {
    stop(errorCondition("`port` must be a single whole number between 1 and 65535.",
                        class = "carmar_bad_port", call = NULL))
  }
  invisible(TRUE)
}

#' An exact web origin: scheme, host, optional port, no path. The same
#' alphabet serve.R's published_origin_valid() accepts.
#' @noRd
valid_site <- function(site) {
  is.character(site) && length(site) == 1L && !is.na(site) &&
    nchar(site, type = "bytes") <= 512L &&
    grepl("^https?://[A-Za-z0-9._~-]+(:[0-9]{1,5})?$", site)
}

#' The package's state directory, created.
#' @noRd
state_dir <- function() {
  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)
  state
}

#' Where the kernels' discovery records live.
#' @noRd
runtime_dir <- function() {
  Sys.getenv("CARMAR_RUNTIME_DIR", file.path(path.expand("~"), ".carmar", "run"))
}

#' The directory holding serve.R.
#'
#' The installed package's copy, always - except for the package's own
#' developers, who set `CARMAR_DEV_KERNEL` to a source checkout's `spike/`
#' directory (or to `1`, meaning `./spike`) to run the kernel from the tree
#' instead of from an installed copy. Internal; not a supported switch.
#' @noRd
kernel_dir <- function() {
  dev <- trimws(Sys.getenv("CARMAR_DEV_KERNEL", ""))
  if (nzchar(dev)) {
    dir <- if (dir.exists(dev)) dev else file.path(getwd(), "spike")
    if (file.exists(file.path(dir, "serve.R"))) return(normalizePath(dir, winslash = "/"))
  }
  system.file("app", "kernel", package = "carmar")
}

#' Start serve.R on `port` and wait for its `{"url": ...}` line.
#'
#' Every door (run, listen, serve_shared) spawns the kernel through this one
#' function so they cannot disagree about how. Returns a list with `url` and
#' `page` (both `NULL` when the kernel never announced itself), `log` (the
#' path of the kernel's log) and `log_lines` (its content, for the caller's
#' diagnosis). Never throws for a kernel that failed to start - the caller
#' knows which words fit.
#' @noRd
spawn_kernel <- function(port, state, env = character()) {
  dir <- kernel_dir()
  serve <- file.path(dir, "serve.R")
  if (!nzchar(dir) || !file.exists(serve)) {
    stop(errorCondition(
      "carmar is not installed correctly (serve.R is missing) - reinstall the package.",
      class = "carmar_no_kernel", call = NULL))
  }
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  log <- file.path(state, paste0("kernel-", port, ".log"))
  p <- processx::process$new(
    rscript, serve,
    env = c(Sys.getenv(), CARMAR_PORT = as.character(port), CARMAR_STATE = state, env),
    stdout = log, stderr = "2>&1",
    cleanup = FALSE                     # the kernel outlives this session
  )
  # serve.R announces {"url": ...} on stdout. The WHOLE log is scanned, not
  # just line one - a startup warning ahead of the announcement must not make
  # a healthy kernel look dead. A wait is a loop: up to two minutes.
  announced <- NULL
  for (i in seq_len(240L)) {
    lines <- tryCatch(readLines(log, warn = FALSE), error = function(e) character(0))
    hit <- grep('"url"', lines, value = TRUE)
    if (length(hit)) {
      rec <- tryCatch(jsonlite::fromJSON(hit[[1]], simplifyVector = TRUE),
                      error = function(e) NULL)
      if (is.list(rec) && !is.null(rec$url)) { announced <- rec; break }
    }
    if (!p$is_alive()) break
    Sys.sleep(0.5)
  }
  lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
  list(url = announced$url, page = announced$file, log = log, log_lines = lines)
}

#' The words for a kernel that never announced itself.
#' @noRd
did_not_start <- function(spawned) {
  paste0("CarmaR did not start. The kernel log is at: ", spawned$log,
         if (length(spawned$log_lines)) paste0(
           "\nLast lines:\n", paste(utils::tail(spawned$log_lines, 5), collapse = "\n")))
}

#' Write the package's `url-<port>` session record atomically.
#'
#' Discovery state is read concurrently by new R sessions. Publish with a
#' rename so readers see either the previous complete record or the new one,
#' never a half-written URL after a crash.
#' @noRd
record_session <- function(state, port, u) {
  url_file <- file.path(state, paste0("url-", port))
  part <- paste0(url_file, ".part")
  writeLines(u, part, useBytes = TRUE)
  if (!file.rename(part, url_file)) {
    # Windows cannot atomically rename over an existing file. A stale record
    # on this just-claimed port is harmless; retain a portable fallback.
    copied <- file.copy(part, url_file, overwrite = TRUE)
    unlink(part)
    if (!isTRUE(copied)) {
      stop(errorCondition(paste0(
        "CarmaR started, but its session record could not be saved: ", url_file),
        class = "carmar_no_record", call = NULL))
    }
  }
  invisible(url_file)
}

#' Approve one published origin on a live kernel.
#'
#' `GET /published/authorize?origin=...` as a native client: no Origin header,
#' which is exactly what the kernel's control gate requires - a browser cannot
#' make this request. `TRUE` on a 200 whose body says `ok`.
#' @noRd
authorize_published_origin <- function(kernel_url, site) {
  target <- paste0(kernel_base(kernel_url), "/published/authorize?origin=",
                   utils::URLencode(site, reserved = TRUE))
  body <- tryCatch(paste(slurp(target), collapse = "\n"), error = function(e) "")
  rec <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                  error = function(e) NULL)
  is.list(rec) && isTRUE(rec$ok) && identical(rec$origin, site)
}

#' Deliver a double-clicked document to an already-running local supervisor.
#' The worker remains the authority on readable paths, size, and file type; this
#' request only puts the path into the same pending-open slot cold launch uses.
#'
#' @noRd
deliver_open_file <- function(kernel_url, file) {
  target <- paste0(kernel_base(kernel_url), "/open?file=",
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

# ---- reading kernels ------------------------------------------------------

#' What the kernel behind this URL says about itself on `/health`, as the
#' parsed list (`ok`, `worker`, `kernel_build`, `capabilities`, ...), or
#' `NULL` when the URL is not a loopback kernel URL or nothing answers with
#' JSON there. Every question about a live kernel - is it alive, which build
#' is it, can it pair - reads this one body.
#' @noRd
kernel_health <- function(u) {
  if (!valid_kernel_url(u)) return(NULL)
  health <- paste0(kernel_base(u), "/health")
  rec <- tryCatch({
    jsonlite::fromJSON(paste(slurp(health), collapse = "\n"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)
  if (is.list(rec)) rec else NULL
}

#' Is the kernel behind this URL answering?
#' @noRd
kernel_alive <- function(u) {
  rec <- kernel_health(u)
  is.list(rec) && isTRUE(rec$ok) && isTRUE(rec$worker)
}

#' Can this live kernel host the published-page consent bridge?
#' @noRd
kernel_supports_published_pairing <- function(u) {
  rec <- kernel_health(u)
  is.list(rec) && isTRUE(rec$ok) && isTRUE(rec$worker) &&
    "published-pairing-v3" %in% (rec$capabilities %||% character())
}

#' Is this a loopback-only CarmaR transport URL, optionally on one port?
#' @noRd
valid_kernel_url <- function(u, expected_port = NULL) {
  if (!is.character(u) || length(u) != 1L || is.na(u) || !nzchar(u)) return(FALSE)
  ok <- grepl("^http://(127\\.0\\.0\\.1|localhost|\\[::1\\]):[0-9]{1,5}/?(\\?token=[^#[:space:]]*)?$",
              u, perl = TRUE)
  p <- kernel_port(u)
  ok && !is.na(p) && p > 0L && p < 65536L &&
    (is.null(expected_port) || identical(p, as.integer(expected_port)))
}

#' URL origin for both current clean records and legacy tokenized records.
#' @noRd
kernel_base <- function(u) sub("/+$", "", sub("/\\?token=.*$", "", u))

#' Port number carried by a clean CarmaR URL.
#' @noRd
kernel_port <- function(u) {
  hit <- regmatches(u, regexpr("(?<=:)[0-9]+(?=/|$)", u, perl = TRUE))
  if (!length(hit)) NA_integer_ else suppressWarnings(as.integer(hit))
}

#' A JSON record file as a list, or NULL when it is not one.
#' @noRd
read_json_record <- function(f) {
  rec <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.list(rec)) rec else NULL
}

#' The first line of a text record, or "" when it cannot be read.
#' @noRd
read_first_line <- function(f) {
  tryCatch(readLines(f, warn = FALSE, n = 1L), error = function(e) "")
}

#' The notebook file a live kernel belongs to, as a `file://` URL with the
#' kernel selector - and the launch capability the kernel announced - in its
#' fragment.
#'
#' A pin, never a search. The kernel names its own build on `/health`
#' (`kernel_build`), and its page is exactly `carmar_V<build>.html`: from this
#' package's `inst/app` first, then a source checkout's `dist/` (the developer
#' fallback, `dist`). A higher version found in either place is somebody
#' else's page and is never substituted. Stops with class `carmar_no_page`,
#' naming the build, when the kernel does not report one or its page is
#' installed nowhere.
#' @noRd
notebook_launch_url <- function(kernel_url, dist = file.path(getwd(), "dist")) {
  build <- kernel_health(kernel_url)$kernel_build
  if (!is.character(build) || length(build) != 1L || is.na(build) ||
      !grepl("^[A-Za-z0-9._-]+$", build)) {
    stop(errorCondition(paste0(
      "The CarmaR kernel behind ", kernel_base(kernel_url),
      " did not report its build; it is not answering, or predates the pinned page."),
      class = "carmar_no_page", call = NULL))
  }
  wanted <- paste0("carmar_V", build, ".html")
  candidates <- c(system.file("app", wanted, package = "carmar"),
                  file.path(dist, wanted))
  present <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(present)) {
    stop(errorCondition(paste0(
      "The notebook for CarmaR ", build, " (", wanted, ") is not installed",
      " beside this package or in ", dist, "; reinstall CarmaR ", build, "."),
      class = "carmar_no_page", call = NULL))
  }
  file <- normalizePath(present[[1L]], winslash = "/")
  prefix <- if (startsWith(file, "/")) "file://" else "file:///"
  port <- kernel_port(kernel_url)
  cap <- recorded_pair(port)
  paste0(prefix, utils::URLencode(file, reserved = FALSE),
         "#kernel=", port, if (nzchar(cap)) paste0("&pair=", cap))
}

#' The file-page launch capability a kernel announced, read back from its
#' discovery record (`~/.carmar/run/kernel-<port>.json`, written by serve.R).
#' A notebook opened from disk is refused without it, so a page URL
#' regenerated here has to carry it or it reopens a notebook the kernel will
#' not talk to. `""` when the record is absent or predates the capability.
#' @noRd
recorded_pair <- function(port) {
  port <- suppressWarnings(as.integer(port))
  if (length(port) != 1L || is.na(port)) return("")
  f <- file.path(runtime_dir(), sprintf("kernel-%d.json", port))
  if (!file.exists(f)) return("")
  rec <- read_json_record(f)
  page <- if (!is.null(rec) && length(rec$file) == 1L && is.character(rec$file)) rec$file else ""
  hit <- regmatches(page, regexpr("(?<=[#&]pair=)[A-Za-z0-9]+", page, perl = TRUE))
  if (length(hit)) hit else ""
}

#' Open the notebook page in the browser with its fragment intact.
#'
#' `browseURL()` ends in `open` (macOS), `ShellExecute` (Windows) or
#' `xdg-open`, each of which hands a file:// URL to the browser as a PATH:
#' the `#kernel=...&pair=...` fragment never arrives, the page walks the
#' ports, finds the kernel that was just started for it and is refused. So
#' the browser is given a one-line page it can carry, whose only job is to
#' navigate to the real URL. Mode 0600 in the package's state directory;
#' earlier trampolines are swept on every launch, so at most one - the newest
#' session's - is ever on disk.
#' @noRd
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

#' Live kernels known through either record source, newest records first.
#' Dead records are pruned: package state is ours; a runtime record is shared
#' across every launcher and is removed only when its kernel is dead.
#' @noRd
live_kernel_urls <- function(state = tools::R_user_dir("carmar", "data")) {
  state_files <- list.files(state, pattern = "^url-[0-9]+$", full.names = TRUE)
  runtime_files <- list.files(runtime_dir(), pattern = "^kernel-[0-9]+\\.json$",
                              full.names = TRUE)
  files <- c(state_files, runtime_files)
  if (!length(files)) return(character())
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  urls <- vapply(files, function(f) {
    expected <- suppressWarnings(as.integer(sub(
      "^(url-|kernel-)([0-9]+).*$", "\\2", basename(f))))
    declared <- expected
    u <- if (grepl("[.]json$", f)) {
      rec <- read_json_record(f)
      if (is.null(rec)) "" else {
        if (!is.null(rec$port)) declared <- suppressWarnings(as.integer(rec$port))
        rec$url %||% ""
      }
    } else read_first_line(f)
    valid <- length(u) == 1L && is.character(u) &&
      valid_kernel_url(u, expected) && length(declared) == 1L &&
      !is.na(declared) && declared == expected
    if (valid && kernel_alive(u)) u else { unlink(f); "" }
  }, character(1))
  live <- unname(urls[nzchar(urls)])
  live[!duplicated(vapply(live, kernel_base, character(1)))]
}

#' Is a loopback port unused?
#' @noRd
port_is_free <- function(port) {
  con <- suppressWarnings(tryCatch(socketConnection("127.0.0.1", port,
    open = "r+", blocking = TRUE, timeout = 1), error = function(e) NULL))
  if (is.null(con)) return(TRUE)
  close(con)
  FALSE
}

#' readLines over loopback http with a short timeout, connection always
#' closed. The only place this package reads a URL.
#'
#' A port nobody listens on makes base::url() emit a warning ("cannot open"
#' / "Couldn't connect") AND then throw. The throw is the answer the callers
#' read (kernel_health() turns it into NULL); the warning is the same fact
#' said twice, and probing an unused port is the ordinary case for
#' sessions() and listen(), so that one diagnosed warning is muffled here.
#' Every other warning passes through.
#' @noRd
slurp <- function(u) {
  old <- options(timeout = 3)
  on.exit(options(old), add = TRUE)
  con <- url(u)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  withCallingHandlers(
    readLines(con, warn = FALSE),
    warning = function(w) {
      if (grepl("cannot open|Couldn't connect|Connection refused", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
}
