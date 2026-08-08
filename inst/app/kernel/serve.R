#!/usr/bin/env Rscript
#
# Supervisor — httpuv on loopback, one worker behind it.
#
# Serving the page from the same origin as the socket is deliberate: it removes
# CORS, removes Chrome's Private Network Access preflight for 127.0.0.1, and
# makes the page a secure context (browsers treat localhost as trustworthy), so
# COOP/COEP can be set later to give a wasm fallback tier real interrupts too.
#
# Run: Rscript spike/serve.R            # prints {"url": "..."} on stdout

suppressPackageStartupMessages({
  library(httpuv)
  library(jsonlite)
})

here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "kernel.R"))

host <- "127.0.0.1"

#' A STABLE port, because the browser partitions storage by ORIGIN.
#'
#' `httpuv::randomPort()` looks like good hygiene and is quietly destructive
#' here. localStorage and sessionStorage are keyed by scheme://host:PORT, so a
#' new port on every start is a new, empty storage on every start: the saved
#' notebooks invisible, the chat history gone, the AI key asked for again. A
#' user restarting the kernel three times has three separate CarmaRs and no way
#' to know why the last one forgot everything.
#'
#' A fixed loopback port is not a weaker position than a random one — the
#' security boundary here is the CSPRNG token and the Host check below, not the
#' port number, which any local process can enumerate in milliseconds either
#' way. If something else already holds it, fall back rather than refuse to
#' start; that session gets its own storage, which is the old behaviour and is
#' the correct trade against not running at all.
port_taken <- function(p) {
  con <- suppressWarnings(tryCatch(
    socketConnection(host, p, open = "r+", blocking = TRUE, timeout = 1),
    error = function(e) NULL))
  if (is.null(con)) FALSE else { close(con); TRUE }
}
port <- local({
  wanted <- suppressWarnings(as.integer(Sys.getenv("CARMAR_PORT", "4747")))
  if (is.na(wanted) || wanted < 1024L || wanted > 65535L) wanted <- 4747L
  if (port_taken(wanted)) httpuv::randomPort() else wanted
})

#' A session token from the operating system's CSPRNG.
#'
#' `sample()` was wrong here and the reason is not pedantry: R's Mersenne
#' Twister is seeded from the clock and the pid, both of which an attacker on
#' the same machine can narrow to a small range, and MT state is recoverable
#' from its own output. This token is the ONLY thing standing between a local
#' process and arbitrary code execution in the user's R session, so it comes
#' from /dev/urandom (or the Windows CSPRNG via openssl), and refuses to start
#' rather than silently fall back to a guessable one.
#'
#' @param n Bytes of entropy; 24 bytes = 192 bits, hex-encoded to 48 chars.
#' @return A hex string.
random_token <- function(n = 24L) {
  bytes <- NULL
  if (file.exists("/dev/urandom")) {
    # raw = TRUE: without it R warns that this is not a regular file, and that
    # warning printed BEFORE the {"url":...} line the app launcher reads.
    con <- file("/dev/urandom", open = "rb", raw = TRUE)
    on.exit(close(con), add = TRUE)
    bytes <- tryCatch(readBin(con, "raw", n = n), error = function(e) NULL)
  }
  if ((is.null(bytes) || length(bytes) != n) &&
      requireNamespace("openssl", quietly = TRUE)) {
    bytes <- tryCatch(openssl::rand_bytes(n), error = function(e) NULL)
  }
  if (is.null(bytes) || length(bytes) != n) {
    stop("CarmaR could not obtain cryptographic randomness for its session ",
         "token (no /dev/urandom and no openssl package). Refusing to start ",
         "with a guessable token.", call. = FALSE)
  }
  paste(sprintf("%02x", as.integer(bytes)), collapse = "")
}

token <- random_token()
origin_ok <- sprintf("http://%s:%d", host, port)
# What a browser may claim to be talking to. Anything else is a DNS-rebinding
# attempt: the attacker's page keeps its own origin while its hostname is
# re-pointed at 127.0.0.1, and the Host header is the one thing that still
# names the attacker. Same class as CVE-2025-66414 in the MCP TypeScript SDK.
hosts_ok <- c(sprintf("127.0.0.1:%d", port), sprintf("localhost:%d", port),
              sprintf("[::1]:%d", port))

# Optional audit log — one JSON object per line, for deployments that must be
# able to answer "what did this session do?". Off unless CARMAR_LOG is set,
# because a notebook that silently records the user's code would be worse than
# no logging at all.
log_path <- Sys.getenv("CARMAR_LOG", "")
audit <- function(event, ...) {
  if (!nzchar(log_path)) return(invisible(NULL))
  rec <- c(list(ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                pid = Sys.getpid(), event = event), list(...))
  try(cat(toJSON(rec, auto_unbox = TRUE), "\n", sep = "", file = log_path,
          append = TRUE), silent = TRUE)
}

k <- kernel_start(file.path(here, "worker.R"))
sockets <- new.env(parent = emptyenv())
sockets$open <- list()
# The worker announces itself once, at server startup — long before any browser
# connects. Without replaying it, every page that opens later sits on
# "connecting…" forever waiting for a frame that was broadcast to nobody.
sockets$hello <- NULL

`%||%` <- function(a, b) if (is.null(a)) b else a

#' The page to serve at `/`: the built CarmaR notebook when there is one,
#' otherwise the bare spike page. Highest version wins, `.beta.min` ignored so
#' a debuggable build is served during development. The repo keeps builds in
#' ../dist; a distribution copy keeps the notebook right next to this folder.
#'
#' @return Path to an HTML file.
notebook_page <- function() {
  # ALL locations pooled, best VERSION wins — updaters drop new files in a
  # dist/ (the app bundle's, or CARMAR_DIST: the per-user dir carmar::run()
  # announces) while the bundle carries its own, and character sort is wrong
  # for versions twice over (V0.9 > V0.12, V0.13 > V1.0). numeric_version
  # knows. Request-time on purpose: a file downloaded while the kernel runs
  # is served on the very next reload.
  user_dist <- Sys.getenv("CARMAR_DIST", "")
  dirs <- c(if (nzchar(user_dist)) user_dist,
            file.path(here, "..", "dist"), file.path(here, ".."))
  built <- unlist(lapply(dirs, function(d)
    list.files(d, pattern = "^carmar_V.*[^n]\\.html$", full.names = TRUE)))
  if (length(built) > 0L) {
    v <- tryCatch(numeric_version(sub("^carmar_V(.*)\\.html$", "\\1", basename(built))),
                  error = function(e) NULL)
    if (!is.null(v)) return(built[order(v, decreasing = TRUE)][1L])
    return(sort(built, decreasing = TRUE)[1L])       # unparseable name: old rule
  }
  file.path(here, "index.html")
}

#' Pull `token` out of a raw query string.
query_token <- function(qs) {
  if (is.null(qs) || !nzchar(qs)) return("")
  hit <- regmatches(qs, regexpr("(?<=[?&])token=[^&]*", qs, perl = TRUE))
  if (length(hit) == 0L) "" else sub("^token=", "", hit)
}

#' Any other query parameter, percent-decoded.
query_param <- function(qs, name) {
  if (is.null(qs) || !nzchar(qs)) return("")
  pat <- paste0("(?<=[?&])", name, "=[^&]*")
  hit <- regmatches(qs, regexpr(pat, qs, perl = TRUE))
  if (length(hit) == 0L) return("")
  utils::URLdecode(sub(paste0("^", name, "="), "", hit))
}

#' Compare two secrets in time that does not depend on where they differ.
#'
#' `identical()` stops at the first differing byte. Over loopback, with R's own
#' overhead on top, extracting a token that way is not a practical attack — but
#' "not practical today" is not a property worth relying on, and constant time
#' costs four lines.
secret_equal <- function(a, b) {
  a <- as.character(a %||% ""); b <- as.character(b %||% "")
  ab <- charToRaw(a); bb <- charToRaw(b)
  if (length(ab) != length(bb)) return(FALSE)
  if (length(ab) == 0L) return(FALSE)          # an empty token is never valid
  sum(as.integer(xor(ab, bb))) == 0L
}

#' Reject with a reason, and record it.
#'
#' Every rejection is logged when logging is on: a burst of "bad host" is a
#' rebinding attempt in progress, and it is the sort of thing a security team
#' needs to be able to see after the fact.
reject <- function(reason, detail = NULL) {
  audit("rejected", reason = reason, detail = detail %||% "")
  list(status = 403L,
       headers = list("Content-Type" = "text/plain",
                      "Cache-Control" = "no-store"),
       body = paste("forbidden:", reason))
}

#' Is this request addressed to the loopback name this server answers to?
#'
#' The check that makes "bound to 127.0.0.1" mean what people think it means.
#' A browser sends `Host: evil.example` when a rebound hostname points at us;
#' loopback clients send 127.0.0.1 or localhost. Requests with no Host at all
#' (HTTP/1.0, some tools) are refused rather than trusted.
host_ok <- function(req) {
  h <- tolower(req$HTTP_HOST %||% "")
  nzchar(h) && h %in% hosts_ok
}

# Commands the supervisor is willing to forward. An allow-list, not a
# deny-list: a worker command added later is unreachable from the browser
# until someone adds it here on purpose.
FORWARDED <- c("env", "obj", "struct", "view", "rm", "packages", "help", "wd",
               "parse", "complete", "files", "import", "readfile", "writefile",
               "hover", "format")

# A single WebSocket frame this large is not a notebook cell; it is either a
# bug or an attempt to exhaust memory in fromJSON(). 8 MB is far above any
# real cell (the biggest legitimate payload is a script save) and far below
# anything that hurts.
MAX_FRAME_BYTES <- 8e6
# Every browser tab holds one socket. Dozens mean something is looping.
MAX_SOCKETS <- 32L

# Headers on every response: nothing about this page belongs in a cache, in a
# frame, or in a MIME-sniffing heuristic.
SAFE_HEADERS <- list(
  "Cache-Control" = "no-store",
  "X-Content-Type-Options" = "nosniff",
  "X-Frame-Options" = "DENY",
  "Referrer-Policy" = "no-referrer",
  # The page carries its own CSP in a <meta> (it has to — it is also opened as
  # a file:// document, where no headers exist). frame-ancestors is the one
  # directive a <meta> cannot deliver, per spec, so it is sent here instead of
  # duplicating the whole policy in two places that would drift apart.
  "Content-Security-Policy" = "frame-ancestors 'none'"
)
resp <- function(status, type, body, extra = list()) {
  list(status = status,
       headers = c(list("Content-Type" = type), SAFE_HEADERS, extra),
       body = body)
}

app <- list(
  onHeaders = function(req) {
    # The Host check applies to the upgrade too — a rebound page trying to
    # open a socket loses here even before the token is considered.
    if (!host_ok(req)) return(reject("bad host", req$HTTP_HOST %||% "(none)"))
    upgrading <- identical(tolower(req$HTTP_UPGRADE %||% ""), "websocket")
    if (!upgrading) return(NULL)
    if (!secret_equal(query_token(req$QUERY_STRING), token)) return(reject("bad token"))
    # WebSockets are exempt from the same-origin policy: without this, any site
    # the user happens to visit could open a socket here and run R.
    origin <- req$HTTP_ORIGIN
    if (!is.null(origin) && !identical(origin, origin_ok)) {
      return(reject("bad origin", origin))
    }
    if (length(sockets$open) >= MAX_SOCKETS) return(reject("too many connections"))
    NULL
  },

  call = function(req) {
    if (!host_ok(req)) return(reject("bad host", req$HTTP_HOST %||% "(none)"))
    if (identical(req$PATH_INFO, "/health")) {
      return(resp(200L, "application/json",
                  toJSON(list(ok = TRUE, worker = k$proc$is_alive()), auto_unbox = TRUE)))
    }
    # Token-gated shutdown: the notebook's Quit, and how an app-bundle launcher
    # ends a background kernel without hunting processes. The flag is honored
    # by the event loop so this response still gets delivered.
    if (identical(req$PATH_INFO, "/shutdown")) {
      if (!secret_equal(query_token(req$QUERY_STRING), token)) return(reject("bad token"))
      audit("shutdown")
      sockets$quit <- TRUE
      return(resp(200L, "application/json",
                  toJSON(list(ok = TRUE, stopping = TRUE), auto_unbox = TRUE)))
    }
    # The double-click door: /open?token=…&file=/abs/path.qmd. This is how
    # CarmaR.app hands a Finder-opened document to the notebook — the server
    # only VALIDATES and redirects; the page itself asks the worker for the
    # text (the readfile op, which enforces its own path rules) and imports.
    # Token-gated like /shutdown: a drive-by page must not be able to make
    # the notebook open files by guessing paths.
    if (identical(req$PATH_INFO, "/open")) {
      if (!secret_equal(query_token(req$QUERY_STRING), token)) return(reject("bad token"))
      f <- query_param(req$QUERY_STRING, "file")
      openable <- nzchar(f) && file.exists(f) && !dir.exists(f) &&
        grepl("\\.(qmd|rmd|md|markdown)$", f, ignore.case = TRUE)
      if (!openable) {
        audit("open-rejected", detail = f)
        return(resp(400L, "text/plain", "not an openable document (.qmd, .Rmd, .md)"))
      }
      audit("open", detail = f)
      loc <- paste0("/?token=", token, "&open=",
                    utils::URLencode(normalizePath(f), reserved = TRUE))
      return(resp(302L, "text/plain", "", extra = list(Location = loc)))
    }
    if (identical(req$PATH_INFO, "/") || identical(req$PATH_INFO, "/spike")) {
      page <- if (identical(req$PATH_INFO, "/spike")) file.path(here, "index.html")
              else notebook_page()
      return(resp(200L, "text/html",
                  paste(readLines(page, warn = FALSE), collapse = "\n")))
    }
    resp(404L, "text/plain", "not found")
  },

  onWSOpen = function(ws) {
    sockets$open <- c(sockets$open, ws)
    audit("socket-open", sockets = length(sockets$open))
    if (!is.null(sockets$hello)) try(ws$send(sockets$hello), silent = TRUE)
    # The whole handler is wrapped: a malformed frame must cost that frame and
    # nothing else. Before this, `{"type":5}` or a bare JSON number reached
    # `cmd$type` on an atomic vector and threw inside httpuv's callback.
    ws$onMessage(function(binary, message) {
      tryCatch(handle_frame(message), error = function(e) {
        audit("frame-error", detail = conditionMessage(e))
      })
      invisible(NULL)
    })
    ws$onClose(function() {
      sockets$open <- Filter(function(s) !identical(s, ws), sockets$open)
      audit("socket-close", sockets = length(sockets$open))
    })
  }
)

#' Is this a single, plain string? Every command field is one, and R's coercion
#' rules are exactly permissive enough to turn a JSON array or object into
#' something that runs but is not what anybody meant.
scalar_chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

#' Act on one WebSocket frame. Validates, then forwards; never evaluates.
handle_frame <- function(message) {
  if (is.raw(message)) return(invisible(NULL))           # binary: not our protocol
  if (nchar(message, type = "bytes") > MAX_FRAME_BYTES) {
    audit("frame-too-large", bytes = nchar(message, type = "bytes"))
    return(invisible(NULL))
  }
  cmd <- tryCatch(fromJSON(message, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(cmd) || !scalar_chr(cmd$type)) return(invisible(NULL))

  if (identical(cmd$type, "exec")) {
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$source)) return(invisible(NULL))
    audit("exec", id = cmd$id, bytes = nchar(cmd$source, type = "bytes"))
    return(invisible(kernel_exec(k, cmd$id, cmd$source, cmd$dims)))
  }
  if (identical(cmd$type, "interrupt")) {
    audit("interrupt")
    return(invisible(kernel_interrupt(k)))
  }
  # Restart: a NEW worker process — fresh globalenv, fresh packages. The stored
  # hello is stale the moment the old worker dies; the new worker's ready frame
  # replaces it via pump() and reaches every open socket.
  if (identical(cmd$type, "restart")) {
    audit("restart")
    try(kernel_stop(k, grace = 1), silent = TRUE)
    k <<- kernel_start(file.path(here, "worker.R"))
    sockets$hello <- NULL
    return(invisible(NULL))
  }
  if (cmd$type %in% FORWARDED) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    audit(cmd$type, id = cmd$id)
    # A command that cannot be handed to the worker is ANSWERED, with the
    # reason. Silence here is indistinguishable from a slow answer, so the page
    # sat on a spinner until its own timeout and then said "R did not answer" —
    # true, and useless.
    failed <- tryCatch({ kernel_send(k, cmd); NULL },
                       error = function(e) conditionMessage(e))
    if (!is.null(failed)) {
      audit("forward-failed", id = cmd$id, detail = failed)
      reply <- toJSON(list(type = cmd$type, id = cmd$id, error = failed), auto_unbox = TRUE)
      lapply(sockets$open, function(ws) try(ws$send(reply), silent = TRUE))
    }
    return(invisible(NULL))
  }
  audit("unknown-command", detail = cmd$type)
  invisible(NULL)
}

#' Forward everything the worker produced to every connected page.
pump <- function() {
  events <- kernel_poll(k, 0L)
  if (length(events) == 0L) return(invisible(NULL))
  payload <- vapply(events, function(e) toJSON(e, auto_unbox = TRUE), character(1))
  ready_at <- which(vapply(events, function(e) identical(e$type, "ready"), logical(1)))
  if (length(ready_at) > 0L) sockets$hello <- payload[[ready_at[[1L]]]]
  lapply(sockets$open, function(ws) {
    lapply(payload, function(p) try(ws$send(p), silent = TRUE))
  })
  invisible(NULL)
}

server <- httpuv::startServer(host, port, app)
on.exit({ httpuv::stopServer(server); kernel_stop(k) }, add = TRUE)

url <- sprintf("%s/?token=%s", origin_ok, token)
audit("started", port = port, r = R.version.string, root = Sys.getenv("CARMAR_ROOT", ""))
cat(toJSON(list(url = url), auto_unbox = TRUE), "\n")
flush(stdout())

# --open: launch the default browser at the tokenized URL — the one-double-
# click path a distribution copy's Start script uses. Off by default so the
# supervisor stays headless under tests and scripts.
if ("--open" %in% commandArgs(trailingOnly = TRUE)) {
  try(utils::browseURL(url), silent = TRUE)
}

# The event loop. `service()` returns at least every 50ms whatever the worker is
# doing, because the worker is a different process — that is the whole design.
repeat {
  httpuv::service(50)
  pump()
  if (isTRUE(sockets$quit)) break
  if (!k$proc$is_alive()) {
    lapply(sockets$open, function(ws)
      try(ws$send(toJSON(list(type = "worker-died"), auto_unbox = TRUE)), silent = TRUE))
    break
  }
}
