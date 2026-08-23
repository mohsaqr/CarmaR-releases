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

# Before anything reads or writes a frame. The supervisor parses every browser
# message and re-encodes every worker reply, so its own character locale
# decides whether `région` survives the round trip — and launchd hands a
# desktop app no locale at all. See utf8_ctype() in kernel.R.
# `invisible`: a bare call at the top level of a script AUTO-PRINTS its value,
# and this stdout is a protocol channel — the launcher reads it to find the
# {"url": ...} line. A stray [1] "UTF-8" ahead of that is noise at best and an
# unparseable first line at worst.
invisible(utf8_ctype())

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
#' browser boundary here is the Host + Origin/Sec-Fetch checks below, not the
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

k <- kernel_start(file.path(here, "worker-boot.R"))
sockets <- new.env(parent = emptyenv())
# Each connection is a RECORD (an environment), not a bare ws: the MCP plane
# below needs to know which socket is a notebook page and which is an agent,
# and httpuv's WebSocket objects are locked R6 instances that refuse new
# fields. rec$ws is the socket; rec$role is "page" until the client declares
# itself with mcp-hello; rec$name is the agent's self-description for the UI.
sockets$open <- list()
# The worker announces itself once, at server startup — long before any browser
# connects. Without replaying it, every page that opens later sits on
# "connecting…" forever waiting for a frame that was broadcast to nobody.
sockets$hello <- NULL
# MCP routing state: which page most recently claimed to be the user's active
# window, and which agent socket is waiting for which request id.
sockets$active_page <- NULL
sockets$mcp_pending <- new.env(parent = emptyenv())
# Subscription chats: id → record of one running `claude -p` process, owned by
# the page socket that asked. See the agent-chat frames in handle_frame.
sockets$chats <- new.env(parent = emptyenv())
# Idle linger: a kernel nobody is connected to stops itself, so closing the
# notebook tab does not strand an R process (and five stranded days do not
# greet day six with the capacity prompt). The clock starts when the LAST
# socket — page or agent — closes, and only while the worker is idle: a run
# someone left cooking finishes first, and the finished session then waits a
# full grace period for its page to come back (a reload or auto-reattach
# cancels the clock). CARMAR_LINGER is the grace in seconds; 0 or negative
# disables. The clock never starts before the first connection ever, so a
# slow browser launch cannot lose the kernel it was opening.
linger_s <- suppressWarnings(as.numeric(Sys.getenv("CARMAR_LINGER", "600")))
if (!length(linger_s) || !is.finite(linger_s)) linger_s <- 600
# A socket that stopped answering is not a connection, and until 0.60.1 the
# supervisor could not tell the difference. `ws$onClose` is the ONLY thing that
# ever removed a page from `sockets$open`, and it fires on an orderly TCP close:
# a laptop that sleeps, a Wi-Fi change, a browser that is force-quit all leave a
# HALF-OPEN socket whose entry stays forever. `linger_check()` then sees a
# connection that no longer exists, never arms its clock, and the kernel becomes
# immortal — which is how a machine ends up with eight live R sessions and one
# open notebook, every one of them counting against the launcher's cap.
#
# httpuv 1.6 exposes no WebSocket ping (send/close/onMessage/onClose is the
# whole surface), so liveness has to be application-level: the client beats
# (`{"type":"hb"}`), the supervisor timestamps every frame, and a socket that
# HAS beaten and then goes quiet past this threshold is closed and dropped.
# Only a socket that has beaten at least once is ever reaped — a notebook built
# before heartbeats exists on people's disks and must keep today's behaviour
# rather than be disconnected while its reader is looking at it.
socket_silence_s <- suppressWarnings(as.numeric(Sys.getenv("CARMAR_SOCKET_SILENCE", "90")))
if (!length(socket_silence_s) || !is.finite(socket_silence_s)) socket_silence_s <- 90
# Runs in flight: exec id → TRUE, cleared when the worker's done frame for
# that id comes back through pump(). This is the busy guard's whole evidence.
sockets$running <- new.env(parent = emptyenv())
# Browser request ids are only unique inside one tab (both tabs begin at c1,
# env-1, ...). The worker sees supervisor-owned ids; this table maps each one
# back to the socket and id that originated it. That makes isolation a server
# guarantee, including for older browser bundles that still accept any frame.
sockets$worker_routes <- new.env(parent = emptyenv())
sockets$route_seq <- 0L
sockets$worker_queue <- list()
# Published Quarto pages never connect to the worker socket directly. A reader
# explicitly pairs one page in a loopback consent window; that LOCAL window
# owns the socket and relays only messages from the exact approved origin.
sockets$pairing_requests <- new.env(parent = emptyenv())
# Consent belongs to this local CarmaR session, not to one popup document.
# Once an origin is approved, a replacement bridge window for that exact
# origin may reconnect without asking again. The environment disappears when
# the kernel stops, so approval is still session-scoped.
sockets$published_approvals <- new.env(parent = emptyenv())
sockets$worker_active <- NULL
sockets$worker_terminal <- NULL
# The analysis plane keeps its own routes and queue: its replies must never be
# mistaken for the evaluating session's, and a stuck analysis must never block
# a run (or the reverse).
sockets$analyzer <- NULL
sockets$analyze_routes <- new.env(parent = emptyenv())
sockets$analyze_queue <- list()
sockets$analyze_active <- NULL
sockets$ever_connected <- FALSE
sockets$idle_since <- NULL
# When this supervisor started. A kernel nobody ever connected to is idle FROM
# BOOT, so the linger clock has something to run from — see linger_check().
sockets$boot_at <- Sys.time()

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

#' Where the carmar-mcp stdio server lives on THIS machine, so the setup
#' dialog can show a command that works verbatim. Repo layout first
#' (tools/mcp beside spike/), then a distribution copy beside serve.R.
#' Empty when neither exists — the dialog then shows a placeholder path.
mcp_server_path <- function() {
  for (p in c(file.path(here, "..", "tools", "mcp", "carmar-mcp.mjs"),
              file.path(here, "mcp", "carmar-mcp.mjs"))) {
    if (file.exists(p)) return(normalizePath(p))
  }
  ""
}

#' Find a binary the way a launcher must: an app-bundle supervisor inherits a
#' minimal PATH, so Sys.which alone misses user-local installs.
find_binary <- function(name, extra = character()) {
  hit <- Sys.which(name)
  if (nzchar(hit)) return(unname(hit))
  for (p in c(extra, file.path(path.expand("~"), ".local", "bin", name),
              file.path("/opt/homebrew/bin", name),
              file.path("/usr/local/bin", name))) {
    if (file.exists(p)) return(p)
  }
  ""
}

#' The Claude Code CLI, if this machine has one. CARMAR_CLAUDE_BIN overrides —
#' that is also how tests substitute a deterministic stub.
claude_bin <- function() {
  override <- Sys.getenv("CARMAR_CLAUDE_BIN", "")
  if (nzchar(override) && file.exists(override)) return(override)
  find_binary("claude")
}

#' The Codex CLI, if this machine has one. CARMAR_CODEX_BIN overrides (tests).
codex_bin <- function() {
  override <- Sys.getenv("CARMAR_CODEX_BIN", "")
  if (nzchar(override) && file.exists(override)) return(override)
  find_binary("codex")
}

#' Any other query parameter, percent-decoded.
query_param <- function(qs, name) {
  if (is.null(qs) || !nzchar(qs)) return("")
  pat <- paste0("(?<=[?&])", name, "=[^&]*")
  hit <- regmatches(qs, regexpr(pat, qs, perl = TRUE))
  if (length(hit) == 0L) return("")
  utils::URLdecode(sub(paste0("^", name, "="), "", hit))
}

#' Is this an exact web origin (scheme + authority, no path or credentials)?
#'
#' It is deliberately a small parser: the value is used only as an opaque key
#' compared with the browser-supplied Origin header.  Keeping paths, fragments
#' and control characters out also makes it safe to show on the consent page.
published_origin_valid <- function(origin) {
  length(origin) == 1L && !is.na(origin) && nchar(origin, type = "bytes") <= 512L &&
    grepl("^https?://[A-Za-z0-9._~-]+(:[0-9]{1,5})?$", origin)
}

# A native CarmaR launcher may start the kernel for one published page.  The
# URL-scheme handoff is the user's consent gesture; carrying only the exact
# origin into this process keeps the permission session-scoped and prevents a
# second site from borrowing it.
initial_published_origin <- Sys.getenv("CARMAR_PUBLISHED_ORIGIN", "")
if (published_origin_valid(initial_published_origin)) {
  sockets$published_approvals[[initial_published_origin]] <- TRUE
  audit("published-origin-authorized", origin = initial_published_origin,
        source = "launcher")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# A secret only the loopback consent document can read.  A foreign page can
# open /pair, but X-Frame-Options keeps it out of an iframe and same-origin
# policy keeps it from learning this value; therefore it cannot manufacture
# the approval navigation without the reader pressing the local button.
pairing_challenge <- function() {
  alphabet <- c(letters, LETTERS, 0:9)
  paste(sample(alphabet, 48L, replace = TRUE), collapse = "")
}

prune_pairing_requests <- function(now = as.numeric(Sys.time())) {
  for (key in ls(sockets$pairing_requests, all.names = TRUE)) {
    rec <- sockets$pairing_requests[[key]]
    if (is.null(rec$created) || now - rec$created > 300) {
      rm(list = key, envir = sockets$pairing_requests)
    }
  }
  invisible(NULL)
}

pairing_page <- function(origin, nonce = "") {
  prune_pairing_requests()
  challenge <- pairing_challenge()
  sockets$pairing_requests[[challenge]] <- list(
    origin = origin, nonce = substr(nonce, 1L, 200L), created = as.numeric(Sys.time()))
  paste0('<!doctype html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; ',
    "style-src 'unsafe-inline'; form-action 'self'; base-uri 'none'\">",
    '<title>Allow this book to use CarmaR?</title><style>',
    'body{font:16px/1.5 system-ui,sans-serif;max-width:42rem;margin:10vh auto;padding:0 1.25rem;color:#172033}',
    '.site{padding:.75rem 1rem;background:#f3f6fb;border-radius:.55rem;overflow-wrap:anywhere}',
    'button{border:0;border-radius:.5rem;background:#2357d9;color:white;padding:.7rem 1rem;font:inherit;font-weight:650;cursor:pointer}',
    'small{color:#59657a}</style></head><body>',
    '<h1>Run this book with your R?</h1><p>The published site</p><p class="site"><strong>',
    html_escape(origin), '</strong></p>',
    '<p>wants to send R chunks to this CarmaR session on your computer. ',
    'The code will run as your user and can access your files and network.</p>',
    '<form method="get" action="/pair/approve"><input type="hidden" name="challenge" value="',
    challenge, '"><button type="submit">Allow for this session</button></form>',
    '<p><small>Nothing runs until you press a chunk\'s Run button. ',
    'Approval disappears when this CarmaR session stops.</small></p></body></html>')
}

pairing_bridge_page <- function(rec) {
  target <- toJSON(rec$origin, auto_unbox = TRUE)
  nonce <- toJSON(rec$nonce, auto_unbox = TRUE)
  socket_url <- toJSON(paste0("ws://127.0.0.1:", port, "/ws"), auto_unbox = TRUE)
  paste0('<!doctype html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; ',
    "script-src 'unsafe-inline'; style-src 'unsafe-inline'; connect-src 'self' ws://127.0.0.1:",
    port, "; base-uri 'none'\">",
    '<title>CarmaR connected</title><style>',
    'body{font:16px/1.5 system-ui,sans-serif;max-width:38rem;margin:10vh auto;padding:0 1.25rem;color:#172033}',
    '.site{overflow-wrap:anywhere;color:#2357d9}</style></head><body>',
    '<h1>CarmaR is connected</h1><p>Keep this window open while running chunks from</p>',
    '<p class="site"><strong>', html_escape(rec$origin), '</strong></p>',
    '<p>You can disconnect the book by closing this window.</p><script>(function(){',
    'const target=', target, ',nonce=', nonce, ',socketUrl=', socket_url, ';let ws=null,wsGeneration=0;',
    'const tell=(type,extra)=>{if(window.opener)window.opener.postMessage(',
    'Object.assign({type,nonce,origin:target,port:', port, '},extra||{}),target)};',
    'addEventListener("message",event=>{const data=event.data||{};',
    'if(event.source!==window.opener||event.origin!==target||data.nonce!==nonce)return;',
    'if(data.type==="carmar:bridge-attach"){const generation=++wsGeneration,old=ws;',
    'if(old){old.onmessage=old.onerror=old.onclose=null;try{old.close()}catch(e){}}',
    'ws=new WebSocket(socketUrl);const current=ws;',
    'current.onmessage=message=>{if(generation===wsGeneration)tell("carmar:bridge-frame",{data:message.data})};',
    'current.onerror=()=>{if(generation===wsGeneration)tell("carmar:bridge-error")};',
    'current.onclose=()=>{if(generation===wsGeneration)tell("carmar:bridge-close")};return}',
    'if(data.type==="carmar:bridge-send"&&ws&&ws.readyState===1)ws.send(data.data);',
    'if(data.type==="carmar:bridge-close"&&ws)ws.close()});',
    'tell("carmar:paired");})();</script></body></html>')
}

pairing_approval_page <- function(challenge) {
  prune_pairing_requests()
  rec <- if (nzchar(challenge) && exists(challenge, sockets$pairing_requests,
                                         inherits = FALSE)) {
    sockets$pairing_requests[[challenge]]
  } else NULL
  if (is.null(rec)) return(NULL)
  rm(list = challenge, envir = sockets$pairing_requests)
  sockets$published_approvals[[rec$origin]] <- TRUE
  audit("published-origin-approved", origin = rec$origin)
  pairing_bridge_page(rec)
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

#' Refuse browser-mediated control requests from any other site.
#'
#' Native same-user clients (the app launcher, R, curl, the MCP bridge) do not
#' send Origin or Sec-Fetch-Site and remain valid. Browsers do: a foreign page
#' cannot stop a kernel or ask it to open a file, while CarmaR keeps the clean,
#' stable localhost URL users expect.
control_rejection <- function(req, allow_file = FALSE) {
  origin <- req$HTTP_ORIGIN %||% ""
  if (nzchar(origin) && !identical(origin, origin_ok) &&
      !(allow_file && identical(origin, "null"))) {
    return(reject("bad origin", origin))
  }
  site <- tolower(req$HTTP_SEC_FETCH_SITE %||% "")
  if (identical(site, "cross-site")) return(reject("cross-site request", site))
  NULL
}

# Commands the supervisor is willing to forward. An allow-list, not a
# deny-list: a worker command added later is unreachable from the browser
# until someone adds it here on purpose.
#
# `colstats` was added deliberately (the data grid's per-column statistics
# card). It reads: it resolves an object the same way `view` already does and
# returns quantiles, level counts and a histogram for ONE named column. It is
# strictly narrower than the `view` beside it — same resolution, less surface,
# no mutation — which is the bar a new entry has to clear, not "it is useful".
#' How many sessions may be open at once.
#'
#' Matches the R package launcher's own cap (tools/r-pkg/R/run.R MAX_SESSIONS)
#' — a session is a whole R process plus a browser origin, so the number is
#' about the machine. Enforced HERE too, because this supervisor can start a
#' sibling on a browser's say-so and an uncapped version of that is a
#' resource-exhaustion button on a web page.
MAX_SESSIONS <- 10L

#' The ports of CarmaR kernels that are actually answering right now.
#'
#' Read from the runtime directory rather than remembered, so sessions started
#' by the R package launcher count too — and health-checked, because a killed
#' kernel leaves its file behind and a dead session must not hold a slot.
live_session_ports <- function() {
  dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                    file.path(path.expand("~"), ".carmar", "run"))
  files <- list.files(dir, pattern = "^kernel-[0-9]+\\.json$", full.names = TRUE)
  ports <- suppressWarnings(as.integer(sub("^kernel-([0-9]+)\\.json$", "\\1", basename(files))))
  ports <- ports[!is.na(ports)]
  Filter(function(p) {
    if (identical(p, port)) return(TRUE)                 # ourselves, definitionally
    ok <- tryCatch({
      con <- url(sprintf("http://%s:%d/health", host, p), open = "rb")
      on.exit(close(con), add = TRUE)
      grepl('"ok":true', paste(readLines(con, warn = FALSE), collapse = ""), fixed = TRUE)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    isTRUE(ok)
  }, ports)
}

#' Start another supervisor on the next free adjacent port.
#'
#' Adjacent and stable on purpose: browsers key storage to the ORIGIN, so a
#' session that moved ports would greet its reader with an empty notebook.
#' Detached — closing this one must not close that one.
#'
#' @return list(port, url), or NULL when nothing could be started.
start_sibling_session <- function() {
  # Do NOT probe the port by binding it here. A serverSocket() we open and
  # close leaves the port in TIME_WAIT, so the child's own port_taken() check
  # says "taken" and it falls back to a RANDOM port — which defeats the whole
  # point, since browsers key storage to the origin and a session that moves
  # ports greets its reader with an empty notebook. Pick the first port no live
  # session holds and let the child do the only binding.
  used <- live_session_ports()
  chosen <- NA_integer_
  for (candidate in seq.int(4747L, 4747L + 200L)) {
    if (!(candidate %in% used)) { chosen <- candidate; break }
  }
  if (is.na(chosen)) return(NULL)
  # `chosen` is a PREFERENCE, not a promise. serve.R takes CARMAR_PORT only if
  # the port is still free when IT looks — between our probe and its start the
  # port can go, and it then falls back to a random one. Waiting for the exact
  # port we picked therefore reported failure for a session that had started
  # perfectly well, and left it orphaned. So: remember what was live before,
  # and accept whichever new port appears.
  before <- live_session_ports()
  started <- tryCatch({
    processx::process$new(file.path(R.home("bin"), "Rscript"),
                          c(file.path(here, "serve.R")),
                          env = c(Sys.getenv(), CARMAR_PORT = as.character(chosen)),
                          stdout = "|", stderr = "|", supervise = FALSE)
  }, error = function(e) NULL)
  if (is.null(started)) return(NULL)
  # Wait for it to ANSWER rather than handing back a URL that 404s.
  deadline <- Sys.time() + 60
  while (Sys.time() < deadline) {
    fresh <- setdiff(live_session_ports(), before)
    if (length(fresh)) {
      got <- fresh[[1L]]
      return(list(port = got, url = sprintf("http://%s:%d/", host, got)))
    }
    if (!started$is_alive()) return(NULL)     # it died; stop waiting on it
    Sys.sleep(0.25)
  }
  NULL
}

FORWARDED <- c("env", "obj", "struct", "view", "colstats", "rm", "packages",
               "package_action", "package_help", "help", "wd",
               "parse", "complete", "files", "import", "readfile", "writefile",
               "hover", "format", "sniff", "choose")

# The ANALYSIS plane is a SECOND allow-list, deliberately kept apart from the
# one above and deliberately tiny. Commands here are routed to spike/analyze.R
# — a process that reads R and never runs it — instead of to the evaluating
# session, which is the whole point: syntax checking must keep working while a
# cell is inside a long calculation (docs/stages/stage-2-intelligence-v1.md).
#
# Two separate lists rather than one flag on a shared list, because the
# question "may the browser ask for this?" and the question "which process
# answers it?" have different answers and different consequences. A command
# added to the wrong one either reaches the user's session when it should not,
# or reaches a process that cannot do it. Adding to EITHER list is a security
# decision (CLAUDE.md, "FORWARDED in serve.R is an allow-list").
ANALYZE_FORWARDED <- c("analyze", "analyze_ping", "analyze_workspace")

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
    # open a socket loses before httpuv creates it.
    if (!host_ok(req)) return(reject("bad host", req$HTTP_HOST %||% "(none)"))
    upgrading <- identical(tolower(req$HTTP_UPGRADE %||% ""), "websocket")
    if (!upgrading) return(NULL)
    # WebSockets are exempt from the same-origin policy: without this, any site
    # the user happens to visit could open a socket here and run R.
    origin <- req$HTTP_ORIGIN
    published_ok <- !is.null(origin) &&
      exists(origin, sockets$published_approvals, inherits = FALSE)
    if (!is.null(origin) && !identical(origin, origin_ok) &&
        !identical(origin, "null") && !published_ok) {
      return(reject("bad origin", origin))
    }
    if (length(sockets$open) >= MAX_SOCKETS) return(reject("too many connections"))
    NULL
  },

  call = function(req) {
    if (!host_ok(req)) return(reject("bad host", req$HTTP_HOST %||% "(none)"))
    if (identical(req$PATH_INFO, "/health")) {
      return(resp(200L, "application/json",
                  toJSON(list(ok = TRUE, worker = k$proc$is_alive(),
                              capabilities = c("published-direct-v1",
                                               "published-pairing-v3")),
                         auto_unbox = TRUE)))
    }
    # The custom carmar:// handler calls this endpoint as a native same-user
    # client. Browser requests carry Origin/Sec-Fetch-Site and are refused by
    # control_rejection(), so visiting a site cannot silently grant itself R
    # execution. One launch authorizes one exact origin for this kernel only.
    if (identical(req$PATH_INFO, "/published/authorize")) {
      blocked <- control_rejection(req)
      if (!is.null(blocked)) return(blocked)
      origin <- query_param(req$QUERY_STRING, "origin")
      if (!published_origin_valid(origin)) {
        audit("published-origin-rejected", detail = origin)
        return(resp(400L, "application/json",
                    toJSON(list(ok = FALSE, error = "invalid origin"),
                           auto_unbox = TRUE)))
      }
      sockets$published_approvals[[origin]] <- TRUE
      audit("published-origin-authorized", origin = origin, source = "native")
      return(resp(200L, "application/json",
                  toJSON(list(ok = TRUE, origin = origin), auto_unbox = TRUE)))
    }
    # A published Quarto page begins here.  Merely opening this route grants
    # nothing: it renders a local, non-frameable consent page containing an
    # unguessable challenge.  Only the button on that page can spend it.
    if (identical(req$PATH_INFO, "/pair")) {
      origin <- query_param(req$QUERY_STRING, "origin")
      nonce <- query_param(req$QUERY_STRING, "nonce")
      if (!published_origin_valid(origin)) {
        audit("pair-rejected", detail = origin)
        return(resp(400L, "text/plain", "A valid http(s) publishing origin is required."))
      }
      if (exists(origin, sockets$published_approvals, inherits = FALSE)) {
        audit("pair-reconnected", origin = origin)
        return(resp(200L, "text/html",
                    pairing_bridge_page(list(origin = origin, nonce = nonce))))
      }
      audit("pair-offered", origin = origin)
      return(resp(200L, "text/html", pairing_page(origin, nonce)))
    }
    if (identical(req$PATH_INFO, "/pair/approve")) {
      page <- pairing_approval_page(query_param(req$QUERY_STRING, "challenge"))
      if (is.null(page)) {
        audit("pair-rejected", detail = "missing or expired challenge")
        return(resp(400L, "text/plain", "This CarmaR approval request has expired."))
      }
      return(resp(200L, "text/html", page))
    }
    # Same-origin shutdown: the notebook's Quit, and how an app-bundle launcher
    # ends a background kernel without hunting processes. The flag is honored
    # by the event loop so this response still gets delivered.
    if (identical(req$PATH_INFO, "/shutdown")) {
      blocked <- control_rejection(req, allow_file = TRUE)
      if (!is.null(blocked)) return(blocked)
      audit("shutdown")
      sockets$quit <- TRUE
      return(resp(200L, "application/json",
                  toJSON(list(ok = TRUE, stopping = TRUE), auto_unbox = TRUE)))
    }
    # Loopback-only: where is the MCP server on this machine, and is the
    # discovery file in place? The setup dialog renders the answer as the
    # exact `claude mcp add` / `codex mcp add` command for THIS install.
    if (identical(req$PATH_INFO, "/mcp-info")) {
      blocked <- control_rejection(req, allow_file = TRUE)
      if (!is.null(blocked)) return(blocked)
      return(resp(200L, "application/json",
                  toJSON(list(server = mcp_server_path(),
                              runtime = nzchar(runtime_file),
                              claude = nzchar(claude_bin()),
                              codex = nzchar(codex_bin())), auto_unbox = TRUE),
                  extra = list("Access-Control-Allow-Origin" = "null")))
    }
    # The double-click door: /open?file=/abs/path.qmd. This is how
    # CarmaR.app hands a Finder-opened document to the notebook — the server
    # only VALIDATES and redirects; the page itself asks the worker for the
    # text (the readfile op, which enforces its own path rules) and imports.
    # Origin/Sec-Fetch gated like /shutdown: a drive-by page must not be able
    # to make the notebook open files by guessing paths.
    if (identical(req$PATH_INFO, "/open")) {
      blocked <- control_rejection(req)
      if (!is.null(blocked)) return(blocked)
      f <- query_param(req$QUERY_STRING, "file")
      openable <- nzchar(f) && file.exists(f) && !dir.exists(f) &&
        grepl("\\.(qmd|rmd|md|markdown)$", f, ignore.case = TRUE)
      if (!openable) {
        audit("open-rejected", detail = f)
        return(resp(400L, "text/plain", "not an openable document (.qmd, .Rmd, .md)"))
      }
      audit("open", detail = f)
      loc <- paste0("/?open=",
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
    rec <- new.env(parent = emptyenv())
    rec$ws <- ws
    rec$role <- "page"
    rec$name <- ""
    # Liveness: `last_seen` is stamped by every frame this socket sends, and
    # `beats` records that it speaks the heartbeat at all. Both are read only
    # by reap_dead_sockets().
    rec$last_seen <- Sys.time()
    rec$beats <- FALSE
    sockets$open <- c(sockets$open, rec)
    sockets$ever_connected <- TRUE
    audit("socket-open", sockets = length(sockets$open))
    if (!is.null(sockets$hello)) try(ws$send(sockets$hello), silent = TRUE)
    # Replay the agent roster too — same lesson as the ready frame: a page
    # opened AFTER an agent connected must still learn the agent is there,
    # or its chip lies by omission.
    agents <- mcp_recs()
    if (length(agents) > 0L) {
      try(ws$send(toJSON(list(
        type = "mcp-clients", count = length(agents),
        names = I(vapply(agents, function(r) r$name %||% "agent", character(1)))
      ), auto_unbox = TRUE)), silent = TRUE)
    }
    # The whole handler is wrapped: a malformed frame must cost that frame and
    # nothing else. Before this, `{"type":5}` or a bare JSON number reached
    # `cmd$type` on an atomic vector and threw inside httpuv's callback.
    ws$onMessage(function(binary, message) {
      tryCatch(handle_frame(message, rec), error = function(e) {
        audit("frame-error", detail = conditionMessage(e))
      })
      invisible(NULL)
    })
    ws$onClose(function() {
      sockets$open <- Filter(function(r) !identical(r, rec), sockets$open)
      if (identical(sockets$active_page, rec)) sockets$active_page <- NULL
      # A page that goes away takes its Claude conversations with it — an
      # orphan `claude -p` would keep billing a turn nobody can see.
      for (id in ls(sockets$chats)) {
        if (identical(sockets$chats[[id]]$rec, rec)) chat_kill(id)
      }
      # An agent that vanishes must not leave its request ids parked forever.
      pending <- ls(sockets$mcp_pending)
      for (id in pending) {
        if (identical(sockets$mcp_pending[[id]], rec)) rm(list = id, envir = sockets$mcp_pending)
      }
      # Do not spend R time on commands whose only recipient has gone away.
      abandoned <- vapply(sockets$worker_queue, function(item) {
        route <- sockets$worker_routes[[item$wire_id]]
        !is.null(route) && identical(route$rec, rec)
      }, logical(1))
      if (any(abandoned)) {
        ids <- vapply(sockets$worker_queue[abandoned], `[[`, character(1), "wire_id")
        sockets$worker_queue <- sockets$worker_queue[!abandoned]
        lapply(ids, drop_worker_route)
      }
      a_abandoned <- vapply(sockets$analyze_queue, function(item) {
        route <- sockets$analyze_routes[[item$wire_id]]
        !is.null(route) && identical(route$rec, rec)
      }, logical(1))
      if (any(a_abandoned)) {
        a_ids <- vapply(sockets$analyze_queue[a_abandoned], `[[`, character(1), "wire_id")
        sockets$analyze_queue <- sockets$analyze_queue[!a_abandoned]
        lapply(a_ids, drop_analyze_route)
      }
      for (wid in ls(sockets$analyze_routes)) {
        if (identical(sockets$analyze_routes[[wid]]$rec, rec)) drop_analyze_route(wid)
      }
      audit("socket-close", sockets = length(sockets$open))
      if (identical(rec$role, "mcp")) notify_mcp_clients()
    })
  }
)

page_recs <- function() Filter(function(r) identical(r$role, "page"), sockets$open)
mcp_recs  <- function() Filter(function(r) identical(r$role, "mcp"), sockets$open)

#' The page an agent's request should land on: the one the user most recently
#' focused, else the most recently opened page. NULL when no notebook window is
#' connected at all — which is an ANSWER (the agent is told), never silence.
target_page <- function() {
  if (!is.null(sockets$active_page) &&
      any(vapply(sockets$open, function(r) identical(r, sockets$active_page), logical(1)))) {
    return(sockets$active_page)
  }
  pages <- page_recs()
  if (length(pages) == 0L) NULL else pages[[length(pages)]]
}

# ── the subscription chat: the pane talks to the user's own Claude Code ─────
#
# One conversation turn = one `claude -p` process, spawned HERE (never in the
# worker: a model turn must not block or be blocked by a running cell). The
# CLI owns its own login — the supervisor passes a prompt on stdin, streams
# the CLI's stream-json lines back to the asking page verbatim, and that is
# the whole exchange. Credentials are never read, stored, or proxied.
#
# The spawned session gets the carmar MCP tools, pinned to THIS kernel via a
# 0600 config file (never argv — argv is visible to every local user in ps),
# and the document tools. The embedded Claude process is a document agent, not
# a chatbot that prints a draft for the user to move by hand. Inserts and
# replacements cross the page's revision/contract gateway and remain
# provisional with Keep/Reject; runs use the ordinary visible chunk path.
AGENT_CHAT_TOOLS <- paste(
  "mcp__carmar__carmar_status", "mcp__carmar__notebook_read",
  "mcp__carmar__chunk_read", "mcp__carmar__chunk_insert",
  "mcp__carmar__chunk_update", "mcp__carmar__chunk_run",
  "mcp__carmar__file_list", "mcp__carmar__file_read",
  sep = ",")
MAX_CHATS <- 3L

chat_send <- function(rec, id, ...) {
  frame <- toJSON(list(type = "agent-chat", id = id, ...), auto_unbox = TRUE)
  try(rec$ws$send(frame), silent = TRUE)
}

#' The two subscription CLIs the pane can ride. `client` travels in the
#' agent-chat frame ("claude" is the default for frames that predate Codex).
#' Codex has no system-prompt flag and no mcp-config FILE: its developer
#' instructions and its MCP server travel as `-c key=value` overrides (the
#' values are TOML — a JSON string literal IS a TOML basic string — and carry
#' no secret: the MCP URL is the kernel origin, nothing more). Every Codex
#' turn gets a private 0700 scratch directory as its working root, removed
#' with the turn, so the CLI's sandbox never sits in the user's project.
chat_client <- function(cmd) if (identical(cmd$client, "codex")) "codex" else "claude"
chat_cli_label <- function(client) if (identical(client, "codex")) "Codex" else "Claude Code"

#' Start one turn. Answers with streamed `line` frames and a final `done`.
chat_start <- function(rec, cmd) {
  id <- cmd$id
  client <- chat_client(cmd)
  label <- chat_cli_label(client)
  bin <- if (identical(client, "codex")) codex_bin() else claude_bin()
  if (!nzchar(bin)) {
    chat_send(rec, id, done = TRUE, code = -1L,
      error = sprintf("%s is not installed on this machine (the `%s` CLI was not found).",
                      label, if (identical(client, "codex")) "codex" else "claude"))
    return(invisible(NULL))
  }
  if (length(ls(sockets$chats)) >= MAX_CHATS) {
    chat_send(rec, id, done = TRUE, code = -1L,
      error = "Too many agent conversations are already running. Stop one first.")
    return(invisible(NULL))
  }

  cfg <- ""        # claude: the 0600 mcp-config file; codex: the 0700 scratch workdir
  server <- mcp_server_path()
  node <- find_binary("node")
  if (identical(client, "codex")) {
    cfg <- tempfile("carmar-codex-")
    dir.create(cfg, mode = "0700")
    toml_str <- function(x) as.character(toJSON(x, auto_unbox = TRUE))
    args <- c("exec", "--json", "--ephemeral", "--skip-git-repo-check",
              "--ignore-user-config", "--approve-for-me", "-C", cfg)
    if (scalar_chr(cmd$system) && nzchar(cmd$system)) {
      args <- c(args, "-c", paste0("developer_instructions=", toml_str(cmd$system)))
    }
    if (scalar_chr(cmd$model) && nzchar(cmd$model) && !identical(cmd$model, "default")) {
      args <- c(args, "-m", cmd$model)
    }
    if (nzchar(server) && nzchar(node)) {
      args <- c(args,
        "-c", paste0("mcp_servers.carmar.command=", toml_str(node)),
        "-c", paste0("mcp_servers.carmar.args=[", toml_str(server), "]"),
        "-c", paste0("mcp_servers.carmar.env={CARMAR_MCP_URL=", toml_str(paste0(origin_ok, "/")), "}"))
    }
    args <- c(args, "-")                     # the prompt arrives on stdin
  } else {
    args <- c("-p", "--output-format", "stream-json", "--verbose",
              "--include-partial-messages")
    if (scalar_chr(cmd$system) && nzchar(cmd$system)) {
      args <- c(args, "--append-system-prompt", cmd$system)
    }
    if (scalar_chr(cmd$model) && nzchar(cmd$model) && !identical(cmd$model, "default")) {
      args <- c(args, "--model", cmd$model)
    }
    if (nzchar(server) && nzchar(node)) {
      cfg <- tempfile("carmar-chat-", fileext = ".json")
      writeLines(toJSON(list(mcpServers = list(carmar = list(
        command = node, args = I(server),
        env = list(CARMAR_MCP_URL = paste0(origin_ok, "/"))
      ))), auto_unbox = TRUE), cfg)
      Sys.chmod(cfg, mode = "0600")
      args <- c(args, "--mcp-config", cfg, "--strict-mcp-config",
                "--allowedTools", AGENT_CHAT_TOOLS)
    }
  }

  proc <- tryCatch(
    processx::process$new(bin, args, stdin = "|", stdout = "|", stderr = "|"),
    error = function(e) e)
  if (inherits(proc, "error")) {
    if (nzchar(cfg)) unlink(cfg, recursive = TRUE)
    chat_send(rec, id, done = TRUE, code = -1L,
      error = paste0("Could not start ", label, ": ", conditionMessage(proc)))
    return(invisible(NULL))
  }

  # The prompt goes on stdin (argv leaks to `ps`, and long contexts overflow
  # it); EOF tells the CLI the prompt is complete. write_input is
  # non-blocking and returns what did not fit — same lesson as kernel_write.
  sent <- tryCatch({
    left <- proc$write_input(paste0(cmd$prompt, "\n"))
    deadline <- Sys.time() + 10
    while (length(left) > 0L && Sys.time() < deadline) { Sys.sleep(0.01); left <- proc$write_input(left) }
    close(proc$get_input_connection())
    length(left) == 0L
  }, error = function(e) FALSE)
  if (!isTRUE(sent)) {
    try(proc$kill(), silent = TRUE)
    if (nzchar(cfg)) unlink(cfg, recursive = TRUE)
    chat_send(rec, id, done = TRUE, code = -1L, error = paste(label, "did not accept the prompt."))
    return(invisible(NULL))
  }

  audit("agent-chat", id = id, client = client)
  sockets$chats[[id]] <- list2env(list(proc = proc, rec = rec, cfg = cfg,
                                       stderr_tail = character()),
                                  parent = emptyenv())
  invisible(NULL)
}

chat_kill <- function(id, notify = FALSE) {
  ch <- sockets$chats[[id]]
  if (is.null(ch)) return(invisible(NULL))
  try(ch$proc$kill(), silent = TRUE)
  if (nzchar(ch$cfg)) unlink(ch$cfg, recursive = TRUE)
  rm(list = id, envir = sockets$chats)
  if (notify) chat_send(ch$rec, id, done = TRUE, code = -2L, stopped = TRUE)
  invisible(NULL)
}

#' Drain every running chat: stream stdout lines to the owner, keep a stderr
#' tail for the post-mortem, and close out finished processes.
pump_chats <- function() {
  for (id in ls(sockets$chats)) {
    ch <- sockets$chats[[id]]
    ch$proc$poll_io(0L)
    lines <- tryCatch(ch$proc$read_output_lines(), error = function(e) character())
    for (line in lines) chat_send(ch$rec, id, line = line)
    err <- tryCatch(ch$proc$read_error_lines(), error = function(e) character())
    if (length(err)) ch$stderr_tail <- utils::tail(c(ch$stderr_tail, err), 20L)
    if (!ch$proc$is_alive()) {
      leftover <- tryCatch(ch$proc$read_output_lines(), error = function(e) character())
      for (line in leftover) chat_send(ch$rec, id, line = line)
      code <- as.integer(tryCatch(ch$proc$get_exit_status(), error = function(e) -1L) %||% -1L)
      if (!identical(code, 0L) && length(ch$stderr_tail)) {
        chat_send(ch$rec, id, done = TRUE, code = code,
                  error = paste(ch$stderr_tail, collapse = "\n"))
      } else {
        chat_send(ch$rec, id, done = TRUE, code = code)
      }
      audit("agent-chat-done", id = id, detail = as.character(code))
      if (nzchar(ch$cfg)) unlink(ch$cfg, recursive = TRUE)
      rm(list = id, envir = sockets$chats)
    }
  }
  invisible(NULL)
}

#' Tell every notebook page who is connected, so the UI can show it.
notify_mcp_clients <- function() {
  agents <- mcp_recs()
  frame <- toJSON(list(
    type = "mcp-clients", count = length(agents),
    names = I(vapply(agents, function(r) r$name %||% "agent", character(1)))
  ), auto_unbox = TRUE)
  lapply(page_recs(), function(r) try(r$ws$send(frame), silent = TRUE))
  invisible(NULL)
}

#' Is this a single, plain string? Every command field is one, and R's coercion
#' rules are exactly permissive enough to turn a JSON array or object into
#' something that runs but is not what anybody meant.
scalar_chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

#' Give a browser command a session-unique worker id and remember its owner.
route_command <- function(cmd, rec, kind) {
  sockets$route_seq <- sockets$route_seq + 1L
  wire_id <- paste0("wire-", sockets$route_seq)
  route <- new.env(parent = emptyenv())
  route$rec <- rec
  route$client_id <- cmd$id
  route$kind <- kind
  route$response_type <- cmd$type
  sockets$worker_routes[[wire_id]] <- route
  cmd$id <- wire_id
  list(cmd = cmd, wire_id = wire_id)
}

drop_worker_route <- function(wire_id) {
  if (exists(wire_id, envir = sockets$worker_routes, inherits = FALSE)) {
    rm(list = wire_id, envir = sockets$worker_routes)
  }
  if (exists(wire_id, envir = sockets$running, inherits = FALSE)) {
    rm(list = wire_id, envir = sockets$running)
  }
  invisible(NULL)
}

#' Serialise one kernel frame for the browser — WITHOUT re-encoding it.
#'
#' The supervisor is a router, and a router must not rewrite the payload. It
#' used to: every frame was parsed by kernel_poll() and re-encoded here with
#' `toJSON(e, auto_unbox = TRUE)`, whose defaults quietly mangled three things
#' the worker had been careful about.
#'
#'   * `digits` defaults to 4, so EVERY number lost precision on the way out.
#'     pi arrived as 3.1416, 1/3 as 0.3333, and 1.2345e-05 as plain 0 — in the
#'     data viewer and in every CSV/Excel/JSON export, which read the same
#'     frames. worker.R emits with `digits = NA` precisely to avoid this; the
#'     relay threw that away one hop later.
#'   * `null` defaults to `{}`, so an absent field became an empty object.
#'     That is where a numeric column's phantom `"levels": {}` came from, and
#'     `{}` is truthy in JavaScript.
#'   * fromJSON's simplification collapses a length-1 array to a scalar, so
#'     `bins: [500]` became `bins: 500` — reintroducing on every frame the
#'     exact bug worker.R's I() wrapping exists to prevent.
#'
#' Re-encoding faithfully is not the fix: a nested-list round-trip is
#' byte-exact but measures 225 ms/frame against 4 ms on a 150 KB view, and
#' this loop is single-threaded. So the original text is forwarded instead and
#' only the routing id is rewritten, which is the one field the supervisor
#' owns. Frames the supervisor built itself (stdout/stderr, error replies)
#' carry no raw text and fall through to an encode that matches worker.R's own
#' options rather than jsonlite's defaults.
#'
#' @param e Parsed frame from kernel_poll(); carries the raw bytes as an
#'   attribute when it came from the kernel rather than from this process.
#' @param wire_id Internal routing id the kernel echoed, or NULL to forward
#'   the frame untouched (session-wide frames belong to no route).
#' @param client_id The browser's own id for the request.
#' @return A single JSON string.
relay_frame <- function(e, wire_id = NULL, client_id = NULL) {
  raw <- attr(e, "raw")
  if (!is.null(raw) && length(raw) == 1L && !is.na(raw)) {
    if (is.null(wire_id)) return(raw)
    # Both ids go through toJSON so a client id needing escaping cannot break
    # the frame. Every emit site in worker.R and analyze.R writes `id` as the
    # second field, so the first occurrence is the id — but a miss falls back
    # to a correct encode rather than shipping the internal id.
    if (scalar_chr(client_id)) {
      needle <- paste0('"id":', toJSON(wire_id, auto_unbox = TRUE))
      if (grepl(needle, raw, fixed = TRUE)) {
        return(sub(needle, paste0('"id":', toJSON(client_id, auto_unbox = TRUE)),
                   raw, fixed = TRUE))
      }
    }
  }
  toJSON(e, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
}

# R is serial, so the supervisor owns one global command queue. Besides
# avoiding blocked stdin writes, this gives otherwise id-less stdout/stderr a
# single unambiguous socket owner.
dispatch_worker_queue <- function() {
  if (!is.null(sockets$worker_active) || !length(sockets$worker_queue)) {
    return(invisible(NULL))
  }
  item <- sockets$worker_queue[[1L]]
  sockets$worker_queue <- sockets$worker_queue[-1L]
  sockets$worker_active <- item$wire_id
  failed <- tryCatch({ kernel_send(k, item$cmd); NULL },
                     error = function(e) conditionMessage(e))
  if (!is.null(failed)) {
    route <- sockets$worker_routes[[item$wire_id]]
    if (!is.null(route)) {
      frame <- if (identical(route$kind, "exec"))
        list(type = "done", id = route$client_id, status = "error", message = failed)
      else list(type = item$cmd$type, id = route$client_id, error = failed)
      try(route$rec$ws$send(toJSON(frame, auto_unbox = TRUE)), silent = TRUE)
    }
    drop_worker_route(item$wire_id)
    sockets$worker_active <- NULL
    return(dispatch_worker_queue())
  }
  invisible(NULL)
}

enqueue_worker_command <- function(cmd, wire_id) {
  sockets$worker_queue <- c(sockets$worker_queue,
                            list(list(cmd = cmd, wire_id = wire_id)))
  dispatch_worker_queue()
}

# ── the analysis plane ─────────────────────────────────────────────────────
# A second child, spawned on FIRST USE rather than at startup: a notebook that
# never asks for a diagnostic should not pay for a second R process, and a
# lazily started one also restarts itself for free after a crash.
ensure_analyzer <- function() {
  if (!is.null(sockets$analyzer) && sockets$analyzer$proc$is_alive()) {
    return(sockets$analyzer)
  }
  if (!is.null(sockets$analyzer)) {
    # It died. Anything still in flight will never be answered by it.
    fail_analyze_routes("the analysis worker stopped")
    sockets$analyze_active <- NULL
    sockets$analyze_queue <- list()
    audit("analyzer-restart")
  }
  started <- tryCatch(kernel_start(file.path(here, "analyze.R")),
                      error = function(e) NULL)
  sockets$analyzer <- started
  if (is.null(started)) audit("analyzer-start-failed")
  started
}

drop_analyze_route <- function(wire_id) {
  if (exists(wire_id, envir = sockets$analyze_routes, inherits = FALSE)) {
    rm(list = wire_id, envir = sockets$analyze_routes)
  }
  invisible(NULL)
}

fail_analyze_routes <- function(reason) {
  for (wire_id in ls(sockets$analyze_routes)) {
    route <- sockets$analyze_routes[[wire_id]]
    try(route$rec$ws$send(toJSON(
      list(type = route$response_type, id = route$client_id, error = reason),
      auto_unbox = TRUE)), silent = TRUE)
    drop_analyze_route(wire_id)
  }
  invisible(NULL)
}

# One in flight at a time, like the worker queue: the analyzer is a single R
# process, so a second large source written while it is mid-parse could block
# the SUPERVISOR inside kernel_write — the one thing this event loop may never
# do. Analysis is milliseconds, so the queue is nearly always empty.
dispatch_analyze_queue <- function() {
  if (!is.null(sockets$analyze_active) || !length(sockets$analyze_queue)) {
    return(invisible(NULL))
  }
  a <- ensure_analyzer()
  item <- sockets$analyze_queue[[1L]]
  if (is.null(a)) {
    sockets$analyze_queue <- sockets$analyze_queue[-1L]
    route <- sockets$analyze_routes[[item$wire_id]]
    if (!is.null(route)) {
      try(route$rec$ws$send(toJSON(
        list(type = route$response_type, id = route$client_id,
             error = "the analysis worker could not start"),
        auto_unbox = TRUE)), silent = TRUE)
    }
    drop_analyze_route(item$wire_id)
    return(dispatch_analyze_queue())
  }
  sockets$analyze_queue <- sockets$analyze_queue[-1L]
  sockets$analyze_active <- item$wire_id
  failed <- tryCatch({
    kernel_write(a, paste0(toJSON(item$cmd, auto_unbox = TRUE), "\n"))
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(failed)) {
    route <- sockets$analyze_routes[[item$wire_id]]
    if (!is.null(route)) {
      try(route$rec$ws$send(toJSON(
        list(type = route$response_type, id = route$client_id, error = failed),
        auto_unbox = TRUE)), silent = TRUE)
    }
    drop_analyze_route(item$wire_id)
    sockets$analyze_active <- NULL
    return(dispatch_analyze_queue())
  }
  invisible(NULL)
}

enqueue_analyze_command <- function(cmd, wire_id) {
  sockets$analyze_queue <- c(sockets$analyze_queue,
                             list(list(cmd = cmd, wire_id = wire_id)))
  dispatch_analyze_queue()
}

#' Route one analysis command, mirroring route_command's ownership rules.
route_analyze <- function(cmd, rec) {
  sockets$route_seq <- sockets$route_seq + 1L
  wire_id <- paste0("awire-", sockets$route_seq)
  route <- new.env(parent = emptyenv())
  route$rec <- rec
  route$client_id <- cmd$id
  # The analyzer answers `analyze_ping` with a `pong` frame; the browser waits
  # on the type it asked for, so the reply type is recorded here, not guessed.
  route$response_type <- if (identical(cmd$type, "analyze_ping")) "pong"
    else if (identical(cmd$type, "analyze_workspace")) "workspace"
    else "analyze"
  sockets$analyze_routes[[wire_id]] <- route
  cmd$id <- wire_id
  # The wire name is the analyzer's vocabulary, not the browser's.
  if (identical(cmd$type, "analyze_ping")) cmd$type <- "ping"
  if (identical(cmd$type, "analyze_workspace")) cmd$type <- "workspace"
  list(cmd = cmd, wire_id = wire_id)
}

#' Drain the analyzer and send each reply to the browser that asked.
pump_analyzer <- function() {
  a <- sockets$analyzer
  if (is.null(a)) return(invisible(NULL))
  if (!a$proc$is_alive()) {
    fail_analyze_routes("the analysis worker stopped")
    sockets$analyze_active <- NULL
    return(invisible(NULL))
  }
  for (e in kernel_poll(a, 0L)) {
    # analyze.R writes nothing to stdout but control frames, so anything
    # without an id is a boot notice, not a user's output.
    if (!scalar_chr(e$id)) next
    wire_id <- e$id
    if (!exists(wire_id, envir = sockets$analyze_routes, inherits = FALSE)) next
    route <- sockets$analyze_routes[[wire_id]]
    e$id <- route$client_id
    # Forwarded verbatim. This reply is full of deliberately-nullable fields —
    # `failed` (absent on success), and `line`/`col` on a diagnostic R could
    # not place — and jsonlite's default renders a NULL field as `{}`, which is
    # truthy in JavaScript: the default turned "nothing went wrong" into
    # "something went wrong" and an unplaceable position into NaN. analyze.R
    # already emits null; relay_frame() forwards its bytes rather than
    # re-deciding, and its fallback encode keeps `null = "null"` for the frames
    # this process builds itself.
    try(route$rec$ws$send(relay_frame(e, wire_id, route$client_id)), silent = TRUE)
    drop_analyze_route(wire_id)
    if (identical(sockets$analyze_active, wire_id)) sockets$analyze_active <- NULL
  }
  dispatch_analyze_queue()
  invisible(NULL)
}

fail_worker_routes <- function(reason) {
  for (wire_id in ls(sockets$worker_routes)) {
    route <- sockets$worker_routes[[wire_id]]
    frame <- if (identical(route$kind, "exec"))
      list(type = "done", id = route$client_id, status = "error", message = reason)
    else list(type = route$response_type, id = route$client_id, error = reason)
    try(route$rec$ws$send(toJSON(frame, auto_unbox = TRUE)), silent = TRUE)
    # A failed route is a finished route. The restart path clears everything
    # itself two lines later, but any future caller that forgot would leave
    # routes answered AND still registered — the next reply with a recycled
    # wire id would then reach the wrong socket.
    drop_worker_route(wire_id)
  }
  invisible(NULL)
}

#' Best-effort refusal for a frame that cannot be forwarded. A silent drop
#' reads as a HANG on the other end — the caller owns a promise with no
#' timeout (a cell) or waits out a long one (a request) — so name the reason
#' when the frame's own head still names its type and id. Reads only a
#' bounded prefix; a frame we refused to parse is not a frame to trust.
refuse_frame <- function(message, rec, reason) {
  head <- substr(message, 1L, 2048L)
  take <- function(pattern) {
    m <- regmatches(head, regexec(pattern, head))[[1L]]
    if (length(m) == 2L && nzchar(m[[2L]])) m[[2L]] else NULL
  }
  type <- take('"type"[[:space:]]*:[[:space:]]*"([A-Za-z_-]{1,32})"')
  id <- take('"id"[[:space:]]*:[[:space:]]*"([^"]{1,128})"')
  if (is.null(type) || is.null(id)) return(invisible(NULL))
  frame <- if (identical(type, "exec")) {
    list(type = "done", id = id, status = "error", message = reason)
  } else {
    list(type = type, id = id, error = reason)
  }
  try(rec$ws$send(toJSON(frame, auto_unbox = TRUE)), silent = TRUE)
  invisible(NULL)
}

#' Act on one WebSocket frame. Validates, then forwards; never evaluates.
handle_frame <- function(message, rec) {
  if (is.raw(message)) return(invisible(NULL))           # binary: not our protocol
  if (nchar(message, type = "bytes") > MAX_FRAME_BYTES) {
    audit("frame-too-large", bytes = nchar(message, type = "bytes"))
    return(refuse_frame(message, rec, sprintf(
      "This request is larger than the kernel accepts (%.1f MB against an 8 MB limit).",
      nchar(message, type = "bytes") / 1e6)))
  }
  cmd <- tryCatch(fromJSON(message, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(cmd) || !scalar_chr(cmd$type)) return(invisible(NULL))

  # Any frame at all proves the socket is alive; the heartbeat is simply the
  # frame an idle notebook can still send. It is answered HERE and never
  # reaches either allow-list: a keep-alive that could be routed to the worker
  # would be a way to keep an evaluating session busy from a page that is
  # doing nothing.
  rec$last_seen <- Sys.time()
  if (identical(cmd$type, "hb")) {
    rec$beats <- TRUE
    try(rec$ws$send(toJSON(list(type = "hb"), auto_unbox = TRUE)), silent = TRUE)
    return(invisible(NULL))
  }

  # ── the MCP plane ─────────────────────────────────────────────────────────
  # A local agent (Claude Code / Codex via tools/mcp/carmar-mcp.mjs) connects
  # through the same loopback/Origin/Host gates as a page, then DECLARES itself.
  # From then on it may only ASK (mcp-request, routed to the active notebook
  # window) and use the read-side FORWARDED ops; pages may only ANSWER.
  # Declared agents are refused exec/interrupt/restart below on purpose: an
  # agent's runs go through a chunk in the notebook, where the user can see
  # them, or they do not happen.
  if (identical(cmd$type, "mcp-hello")) {
    rec$role <- "mcp"
    rec$name <- if (scalar_chr(cmd$client)) substr(cmd$client, 1L, 64L) else "agent"
    audit("mcp-hello", detail = rec$name)
    reply <- toJSON(list(type = "mcp-hello",
                         id = if (scalar_chr(cmd$id)) cmd$id else "hello",
                         ok = TRUE, pages = length(page_recs())), auto_unbox = TRUE)
    try(rec$ws$send(reply), silent = TRUE)
    notify_mcp_clients()
    return(invisible(NULL))
  }
  if (identical(cmd$type, "mcp-active")) {
    if (identical(rec$role, "page")) sockets$active_page <- rec
    return(invisible(NULL))
  }
  if (identical(cmd$type, "mcp-request")) {
    if (!identical(rec$role, "mcp")) {
      audit("mcp-refused", reason = "mcp-request from a non-agent socket")
      return(invisible(NULL))
    }
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$tool)) return(invisible(NULL))
    page <- target_page()
    audit("mcp-request", id = cmd$id, detail = cmd$tool)
    if (is.null(page)) {
      reply <- toJSON(list(type = "mcp-response", id = cmd$id, ok = FALSE,
        error = "No CarmaR notebook window is connected to this kernel. Open the notebook first."),
        auto_unbox = TRUE)
      try(rec$ws$send(reply), silent = TRUE)
      return(invisible(NULL))
    }
    sockets$mcp_pending[[cmd$id]] <- rec
    try(page$ws$send(message), silent = TRUE)          # forwarded verbatim
    return(invisible(NULL))
  }
  if (identical(cmd$type, "mcp-response")) {
    if (identical(rec$role, "mcp")) return(invisible(NULL))   # agents ask, pages answer
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    asker <- sockets$mcp_pending[[cmd$id]]
    if (!is.null(asker)) {
      rm(list = cmd$id, envir = sockets$mcp_pending)
      try(asker$ws$send(message), silent = TRUE)
    }
    return(invisible(NULL))
  }

  # A pane conversation with the user's own Claude Code. Pages only: a
  # declared agent asking the kernel to spawn ANOTHER agent is a loop nobody
  # ordered, and the CLI process acts with the user's full subscription.
  if (identical(cmd$type, "agent-chat")) {
    if (!identical(rec$role, "page")) {
      audit("mcp-refused", reason = "agent-chat from a non-page socket")
      return(invisible(NULL))
    }
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$prompt) || !nzchar(cmd$prompt)) {
      return(invisible(NULL))
    }
    if (!is.null(sockets$chats[[cmd$id]])) return(invisible(NULL))   # id reuse: ignore
    chat_start(rec, cmd)
    return(invisible(NULL))
  }
  if (identical(cmd$type, "agent-chat-stop")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    ch <- sockets$chats[[cmd$id]]
    # Only the socket that started a conversation may stop it.
    if (!is.null(ch) && identical(ch$rec, rec)) {
      audit("agent-chat-stop", id = cmd$id)
      chat_kill(cmd$id, notify = TRUE)
    }
    return(invisible(NULL))
  }

  if (cmd$type %in% c("exec", "interrupt", "restart") && identical(rec$role, "mcp")) {
    audit("mcp-refused", reason = paste("agent asked for", cmd$type))
    if (scalar_chr(cmd$id)) {
      reply <- toJSON(list(type = cmd$type, id = cmd$id,
        error = "Agents run code through notebook chunks (chunk_run), not raw exec."),
        auto_unbox = TRUE)
      try(rec$ws$send(reply), silent = TRUE)
    }
    return(invisible(NULL))
  }

  if (identical(cmd$type, "exec")) {
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$source)) return(invisible(NULL))
    audit("exec", id = cmd$id, bytes = nchar(cmd$source, type = "bytes"))
    routed <- route_command(cmd, rec, "exec")
    assign(routed$wire_id, TRUE, envir = sockets$running)
    enqueue_worker_command(routed$cmd, routed$wire_id)
    return(invisible(NULL))
  }
  if (identical(cmd$type, "interrupt")) {
    audit("interrupt", id = cmd$id %||% "")
    # New clients identify their run. Refuse to interrupt another tab's active
    # command; a queued run can be cancelled without signalling R at all.
    if (scalar_chr(cmd$id)) {
      candidates <- Filter(function(wire_id) {
        route <- sockets$worker_routes[[wire_id]]
        !is.null(route) && identical(route$rec, rec) &&
          identical(route$client_id, cmd$id) && identical(route$kind, "exec")
      }, ls(sockets$worker_routes))
      if (!length(candidates)) return(invisible(NULL))
      wire_id <- candidates[[1L]]
      if (identical(wire_id, sockets$worker_active)) {
        return(invisible(kernel_interrupt(k)))
      }
      sockets$worker_queue <- Filter(function(item) !identical(item$wire_id, wire_id),
                                     sockets$worker_queue)
      try(rec$ws$send(toJSON(list(type = "done", id = cmd$id,
        status = "interrupted", message = "Execution interrupted"),
        auto_unbox = TRUE)), silent = TRUE)
      drop_worker_route(wire_id)
      return(invisible(NULL))
    }
    # Compatibility for an older notebook bundle with id-less Stop.
    return(invisible(kernel_interrupt(k)))
  }
  # Restart: a NEW worker process — fresh globalenv, fresh packages. The stored
  # hello is stale the moment the old worker dies; the new worker's ready frame
  # replaces it via pump() and reaches every open socket.
  if (identical(cmd$type, "restart")) {
    audit("restart")
    fail_worker_routes("R was restarted — this request was abandoned.")
    try(kernel_stop(k, grace = 1), silent = TRUE)
    k <<- kernel_start(file.path(here, "worker-boot.R"))
    sockets$hello <- NULL
    # In-flight runs died with the old worker; the new one will never emit
    # their done frames, and a stuck entry would block idle linger forever.
    rm(list = ls(sockets$running), envir = sockets$running)
    rm(list = ls(sockets$worker_routes), envir = sockets$worker_routes)
    sockets$worker_queue <- list()
    sockets$worker_active <- NULL
    sockets$worker_terminal <- NULL
    return(invisible(NULL))
  }
  # A SIBLING SESSION: another supervisor, another R process, another origin.
  #
  # One notebook per window is the shell's shape — the cell stack is singular —
  # so "open a second document" means a second SESSION, which is what the R
  # package's launcher has always done (tools/r-pkg/R/run.R). A notebook opened
  # from the Files pane could not reach that, so its only option was to replace
  # the document you were reading.
  #
  # Capped, and the cap is the point: this starts a whole R process from a
  # browser request, so an unbounded version is a resource-exhaustion button on
  # a page. The count is of OUR OWN runtime files, health-checked, so kernels
  # that died do not hold a slot.
  if (identical(cmd$type, "session-new")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    audit("session-new", id = cmd$id)
    reply <- function(...) try(rec$ws$send(toJSON(list(type = "session-new", id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    live <- live_session_ports()
    if (length(live) >= MAX_SESSIONS) {
      reply(error = sprintf("%d sessions are already open, which is the limit.", length(live)))
      return(invisible(NULL))
    }
    started <- start_sibling_session()
    if (is.null(started)) {
      reply(error = "Could not start another session.")
      return(invisible(NULL))
    }
    reply(port = started$port, url = started$url)
    return(invisible(NULL))
  }
  if (cmd$type %in% ANALYZE_FORWARDED) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    audit(cmd$type, id = cmd$id)
    routed <- route_analyze(cmd, rec)
    enqueue_analyze_command(routed$cmd, routed$wire_id)
    return(invisible(NULL))
  }
  if (cmd$type %in% FORWARDED) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    audit(cmd$type, id = cmd$id)
    routed <- route_command(cmd, rec, "request")
    enqueue_worker_command(routed$cmd, routed$wire_id)
    return(invisible(NULL))
  }
  audit("unknown-command", detail = cmd$type)
  invisible(NULL)
}

#' Route worker replies only to the browser that originated the command.
pump <- function() {
  # The analysis plane drains first and independently: it must not wait behind
  # the evaluating worker's frames, which is the entire reason it exists.
  pump_analyzer()
  events <- kernel_poll(k, 0L)
  if (length(events) == 0L) {
    # stdout and stderr are separate OS pipes. A done/response frame can be
    # observed just before the final stderr bytes become readable, so retire
    # its owner only after one completely empty drain cycle.
    if (!is.null(sockets$worker_terminal)) {
      terminal <- sockets$worker_terminal
      wire_id <- terminal$wire_id
      try(terminal$route$rec$ws$send(
            relay_frame(terminal$frame, wire_id, terminal$route$client_id)),
          silent = TRUE)
      drop_worker_route(wire_id)
      if (identical(sockets$worker_active, wire_id)) sockets$worker_active <- NULL
      sockets$worker_terminal <- NULL
      dispatch_worker_queue()
    }
    return(invisible(NULL))
  }
  lapply(events, function(e) {
    # Plain stdout/stderr has no protocol id. The worker is serial, so it
    # belongs to the oldest execution still in flight.
    had_id <- scalar_chr(e$id)
    wire_id <- if (had_id) e$id else
      if (e$type %in% c("stdout", "stderr")) sockets$worker_active else NULL
    route <- if (!is.null(wire_id) &&
                 exists(wire_id, envir = sockets$worker_routes, inherits = FALSE))
      sockets$worker_routes[[wire_id]] else NULL

    if (!is.null(route)) {
      e$id <- route$client_id
      # An exec route normally ends in `done` — but an OLDER worker whose
      # dispatch died outside run_cell answers `{"type":"exec", error}`
      # instead, and treating that as ordinary traffic left the route (and
      # `worker_active`) held forever: one bad frame and no run, completion
      # or pane request ever went out again. An error frame on an exec route
      # is terminal too.
      is_terminal <- (identical(route$kind, "request") && had_id) ||
        identical(e$type, "done") ||
        (identical(route$kind, "exec") && had_id && !is.null(e$error))
      if (is_terminal) {
        # Hold the terminal frame until the pipes are quiescent. Otherwise the
        # browser settles the result before a trailing stderr frame arrives.
        sockets$worker_terminal <- list(wire_id = wire_id, route = route, frame = e)
      } else {
        try(route$rec$ws$send(relay_frame(e, wire_id, route$client_id)), silent = TRUE)
      }
      return(invisible(NULL))
    }

    # Session-wide lifecycle frames have no owner and must reach every tab —
    # and no id to rewrite, so they forward exactly as the kernel wrote them.
    payload <- relay_frame(e)
    if (identical(e$type, "ready")) sockets$hello <- payload
    lapply(sockets$open, function(r) try(r$ws$send(payload), silent = TRUE))
    invisible(NULL)
  })
  invisible(NULL)
}

server <- httpuv::startServer(host, port, app)
on.exit({
  httpuv::stopServer(server)
  kernel_stop(k)
  if (!is.null(sockets$analyzer)) try(kernel_stop(sockets$analyzer), silent = TRUE)
}, add = TRUE)

url <- paste0(origin_ok, "/")

# The notebook is a CarmNote-style FILE. The loopback address above is private
# transport metadata for MCP/native health checks; it is never the page the
# launcher opens. The fragment selects the independent kernel without sending
# anything to a web server.
notebook_file <- normalizePath(notebook_page())
file_url <- paste0("file://", utils::URLencode(notebook_file, reserved = FALSE),
                   "#kernel=", port)

#' Discovery for local agents — the Jupyter runtime-dir pattern.
#'
#' `tools/mcp/carmar-mcp.mjs` (the stdio server Claude Code / Codex spawn) has
#' no stdout to read the URL from, so the kernel writes one small file a local
#' process owned by the SAME user can find: ~/.carmar/run/kernel-<port>.json,
#' mode 0600. This is discovery metadata, not a browser credential. A same-user
#' local process was never outside the trust line — it could already exec
#' anything. Removed on clean shutdown;
#' consumers must health-check before trusting a file (kills leave litter).
#' CARMAR_RUNTIME_DIR relocates it (tests use a scratch dir);
#' CARMAR_NO_RUNTIME_FILE=1 turns it off.
runtime_file <- ""
if (!identical(Sys.getenv("CARMAR_NO_RUNTIME_FILE"), "1")) {
  runtime_dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                            file.path(path.expand("~"), ".carmar", "run"))
  made <- dir.exists(runtime_dir) ||
    dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
  if (made) {
    candidate <- file.path(runtime_dir, sprintf("kernel-%d.json", port))
    wrote <- tryCatch({
      writeLines(toJSON(list(url = url, file = file_url, port = port, pid = Sys.getpid(),
                             started = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
                        auto_unbox = TRUE), candidate)
      Sys.chmod(candidate, mode = "0600")
      TRUE
    }, error = function(e) FALSE)
    if (wrote) runtime_file <- candidate
  }
}

audit("started", port = port, r = R.version.string, root = Sys.getenv("CARMAR_ROOT", ""))
cat(toJSON(list(url = url, file = file_url), auto_unbox = TRUE), "\n")
flush(stdout())

# --open: open the notebook FILE. The kernel remains hidden transport plumbing.
if ("--open" %in% commandArgs(trailingOnly = TRUE)) {
  try(utils::browseURL(file_url), silent = TRUE)
}

#' Close and forget sockets that have stopped answering.
#'
#' Only sockets that have proven they speak the heartbeat are eligible, so an
#' older notebook build is never disconnected for being quiet. The entry is
#' dropped here as well as closed: on a half-open TCP the close may produce no
#' onClose callback at all, and it is the ENTRY, not the socket, that keeps the
#' linger clock from arming.
reap_dead_sockets <- function() {
  if (socket_silence_s <= 0 || !length(sockets$open)) return(invisible(NULL))
  now <- Sys.time()
  dead <- Filter(function(r) {
    isTRUE(r$beats) && !is.null(r$last_seen) &&
      as.numeric(difftime(now, r$last_seen, units = "secs")) >= socket_silence_s
  }, sockets$open)
  if (!length(dead)) return(invisible(NULL))
  for (rec in dead) {
    audit("socket-silent", role = rec$role %||% "page", silence = socket_silence_s)
    sockets$open <- Filter(function(r) !identical(r, rec), sockets$open)
    if (identical(sockets$active_page, rec)) sockets$active_page <- NULL
    try(rec$ws$close(), silent = TRUE)
  }
  invisible(NULL)
}

#' Idle linger, decided once per loop turn: nobody connected, nothing running,
#' grace elapsed → the same deliberate shutdown /shutdown performs. Any open
#' socket or in-flight run resets the clock entirely.
#'
#' A kernel nobody has EVER connected to is idle from boot rather than exempt.
#' It used to be exempt outright, so that a slow browser launch could not lose
#' the kernel it was opening — but "still starting" was never bounded, and a
#' session whose tab never arrived (a sibling started for a page that was
#' closed, a `run_published()` nobody paired) lived until the machine was
#' rebooted and held a slot in the launcher's cap the whole time. The grace
#' itself is the protection: ten minutes is three orders of magnitude longer
#' than a browser launch, and a page that connects at any point cancels it.
linger_check <- function() {
  if (linger_s <= 0) return(invisible(NULL))
  if (!isTRUE(sockets$ever_connected) && is.null(sockets$idle_since)) {
    sockets$idle_since <- sockets$boot_at
  }
  idle <- length(sockets$open) == 0L && length(ls(sockets$running)) == 0L
  if (!idle) { sockets$idle_since <- NULL; return(invisible(NULL)) }
  now <- Sys.time()
  if (is.null(sockets$idle_since)) {
    sockets$idle_since <- now
    return(invisible(NULL))
  }
  if (as.numeric(difftime(now, sockets$idle_since, units = "secs")) >= linger_s) {
    audit("linger-shutdown", grace = linger_s)
    sockets$quit <- TRUE
  }
  invisible(NULL)
}

# The event loop. `service()` returns at least every 50ms whatever the worker is
# doing, because the worker is a different process — that is the whole design.
repeat {
  httpuv::service(50)
  pump()
  pump_chats()
  reap_dead_sockets()
  linger_check()
  if (isTRUE(sockets$quit)) break
  if (!k$proc$is_alive()) {
    lapply(sockets$open, function(r)
      try(r$ws$send(toJSON(list(type = "worker-died"), auto_unbox = TRUE)), silent = TRUE))
    break
  }
}

# The loop only breaks on a deliberate shutdown or a dead worker — take the
# discovery file with us so agents stop finding a kernel that is gone. A
# SIGKILL skips this, which is why readers must health-check before trusting
# a runtime file (stale files are litter, not authority). Running Claude
# conversations die with the kernel — nothing may keep billing after Quit.
for (id in ls(sockets$chats)) chat_kill(id)
if (nzchar(runtime_file)) unlink(runtime_file)
