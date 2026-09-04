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
source(file.path(here, "notebook-page.R"))
source(file.path(here, "deployment.R"))
source(file.path(here, "ai-policy.R"))
source(file.path(here, "jobs.R"))
source(file.path(here, "settings.R"))

# Apply an administrator-owned desktop policy before any subsystem reads its
# environment. Invalid or user-writable policy fails closed at the shared
# startup gate below; no partial values are applied.
managed_config <- carmar_managed_environment()

# Before anything reads or writes a frame. The supervisor parses every browser
# message and re-encodes every worker reply, so its own character locale
# decides whether `région` survives the round trip — and launchd hands a
# desktop app no locale at all. See utf8_ctype() in kernel.R.
# `invisible`: a bare call at the top level of a script AUTO-PRINTS its value,
# and this stdout is a protocol channel — the launcher reads it to find the
# {"url": ...} line. A stray [1] "UTF-8" ahead of that is noise at best and an
# unparseable first line at worst.
invisible(utf8_ctype())

# Browser/supervisor compatibility. The protocol changes only when the wire
# contract becomes incompatible; the build changes on every release. Packages
# and app bundles stamp kernel-version beside this script. A source checkout
# falls back to notebook.version.cjs so `npm run kernel` has the same identity.
read_kernel_protocol <- function() {
  candidates <- c(file.path(here, "kernel-protocol"),
                  file.path(here, "..", "kernel.protocol"))
  hit <- candidates[file.exists(candidates)][1L]
  if (!length(hit) || is.na(hit)) stop("CarmaR kernel protocol stamp is missing.")
  value <- suppressWarnings(as.integer(trimws(readLines(hit, warn = FALSE, n = 1L))))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop("CarmaR kernel protocol stamp is invalid.")
  }
  value
}
CARMAR_PROTOCOL_VERSION <- read_kernel_protocol()
read_kernel_build <- function() {
  from_env <- trimws(Sys.getenv("CARMAR_KERNEL_BUILD", ""))
  if (nzchar(from_env)) return(from_env)
  stamp <- file.path(here, "kernel-version")
  if (file.exists(stamp)) {
    value <- trimws(readLines(stamp, warn = FALSE, n = 1L))
    if (nzchar(value)) return(value)
  }
  source_version <- file.path(here, "..", "notebook.version.cjs")
  if (file.exists(source_version)) {
    src <- paste(readLines(source_version, warn = FALSE), collapse = "\n")
    hit <- regmatches(src, regexec('version\\s*:\\s*"([^"]+)"', src))[[1]]
    if (length(hit) >= 2L && nzchar(hit[[2]])) return(hit[[2]])
  }
  "unknown"
}
CARMAR_KERNEL_BUILD <- read_kernel_build()

#' What CarmaR is INSTALLED as, right now — re-read on every call.
#'
#' `CARMAR_KERNEL_BUILD` is what this process was started as. The bundle it
#' lives in is swapped IN PLACE by an upgrade, so the stamp beside this file
#' can name a newer (or, after a rollback, an older) build than the one
#' running. Reading it fresh every time is what lets a session that predates
#' an install say so — in /health, in the ready frame and in every heartbeat
#' — instead of serving the old page in silence until someone quits it.
#'
#' When the build was dictated by CARMAR_KERNEL_BUILD (tests, the broker),
#' the installed build is dictated the same way: CARMAR_INSTALLED_BUILD, or
#' the same value — a test kernel must never read "newer installed" off a
#' source checkout's notebook.version.cjs.
installed_build <- function() {
  dictated <- trimws(Sys.getenv("CARMAR_KERNEL_BUILD", ""))
  if (nzchar(dictated)) {
    inst <- trimws(Sys.getenv("CARMAR_INSTALLED_BUILD", ""))
    return(if (nzchar(inst)) inst else dictated)
  }
  stamp <- file.path(here, "kernel-version")
  if (file.exists(stamp)) {
    value <- trimws(readLines(stamp, warn = FALSE, n = 1L))
    if (nzchar(value)) return(value)
  }
  source_version <- file.path(here, "..", "notebook.version.cjs")
  if (file.exists(source_version)) {
    src <- paste(readLines(source_version, warn = FALSE), collapse = "\n")
    hit <- regmatches(src, regexec('version\\s*:\\s*"([^"]+)"', src))[[1]]
    if (length(hit) >= 2L && nzchar(hit[[2]])) return(hit[[2]])
  }
  "unknown"
}
# The build this kernel REPLACED, when it was started by a "Restart into"
# handoff (session_handoff_finish below). Told to /health for the first
# minute and a half so the page that asked can recognise its successor.
HANDOFF_FROM <- trimws(Sys.getenv("CARMAR_HANDOFF_FROM", ""))

# Where to listen. 127.0.0.1 unless an operator says otherwise, and saying
# otherwise inverts the trust model — see spike/deployment.R, which owns that
# decision and refuses the unsafe spellings of it outright.
host <- local({
  h <- trimws(Sys.getenv("CARMAR_BIND", "127.0.0.1"))
  if (nzchar(h)) h else "127.0.0.1"
})

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
    socketConnection("127.0.0.1", p, open = "r+", blocking = TRUE, timeout = 1),
    error = function(e) NULL))
  if (is.null(con)) FALSE else { close(con); TRUE }
}
# CARMAR_PORT is a PREFERENCE by default and a PROMISE when asked.
#
# Moving to a random port when the wanted one is taken is right for a person
# double-clicking the app: they get a session instead of an error, and the
# launcher tells them where it went. It is wrong the moment something else
# chose the port on the kernel's behalf — under the session broker, the port
# is already written into a registry record and about to be handed to Caddy
# as an upstream, so a kernel that quietly moves leaves BOTH pointing at
# nothing and the reader gets a connection refused with no explanation
# anywhere in the system. CARMAR_PORT_STRICT=1 turns the silent move into a
# refusal, which is the only honest answer when the caller cannot be told.
port <- local({
  wanted <- suppressWarnings(as.integer(Sys.getenv("CARMAR_PORT", "4747")))
  if (is.na(wanted) || wanted < 1024L || wanted > 65535L) wanted <- 4747L
  if (!port_taken(wanted)) return(wanted)
  # A successor started by a handoff is spawned while its predecessor still
  # holds the port; it waits for the port to free rather than moving.
  wait_s <- suppressWarnings(as.numeric(Sys.getenv("CARMAR_WAIT_PORT", "0")))
  if (is.finite(wait_s) && wait_s > 0) {
    deadline <- Sys.time() + wait_s
    while (port_taken(wanted) && Sys.time() < deadline) Sys.sleep(0.25)  # a wait is a loop
    if (!port_taken(wanted)) return(wanted)
  }
  if (identical(Sys.getenv("CARMAR_PORT_STRICT", ""), "1")) {
    message("CarmaR refuses to start: CARMAR_PORT=", wanted, " is already in use ",
            "and CARMAR_PORT_STRICT=1 forbids moving to another port. Whoever ",
            "chose this port is expecting to find the kernel on it.")
    quit(status = 2L, save = "no")
  }
  httpuv::randomPort()
})

# The posture: bind, Origin allow-list, Host allow-list, and whether an
# Origin-less client is a same-user native process or an unauthenticated
# stranger. Computed once; every gate below reads a decided answer.
deployment <- carmar_deployment(port)

# The administrator's AI policy, read once. Its errors join the deployment's
# in the same startup gate and for the same reason: a governance control that
# silently failed to apply is worse than one that refuses to start, because
# nobody finds out until the data has already gone to the wrong vendor.
ai_policy <- carmar_ai_policy()

startup_errors <- c(managed_config$errors, deployment$errors, ai_policy$errors)
if (length(startup_errors)) {
  # Refusing to start is the correct outcome. A server that binds to the
  # network with the loopback trust model is not a degraded CarmaR, it is a
  # remote-execution service, and there is no safe way to serve it "for now".
  for (msg in startup_errors) message("CarmaR refuses to start: ", msg)
  quit(status = 2L, save = "no")
}

# The origin this kernel serves its own page from — used to BUILD urls (the
# launcher's, the MCP bridge's). Whether some other origin may connect is a
# different question, answered by `deployment$origins`.
origin_ok <- sprintf("http://%s:%d", if (deployment$loopback) host else "127.0.0.1", port)
origins_ok <- deployment$origins
# What a browser may claim to be talking to. Anything else is a DNS-rebinding
# attempt: the attacker's page keeps its own origin while its hostname is
# re-pointed at 127.0.0.1, and the Host header is the one thing that still
# names the attacker. Same class as CVE-2025-66414 in the MCP TypeScript SDK.
# Off loopback this list is REQUIRED (deployment.R refuses an empty one),
# because the proxy's hostname is the only thing distinguishing a real reader
# from a rebound attacker page.
hosts_ok <- deployment$hosts

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

# interactive = TRUE: the evaluating worker runs under `R --interactive` where
# available, which is what makes a native browser() debugger possible (the
# console becomes the stdin pipe). The analyzer below stays batch — it parses
# and never pauses. Falls back to batch by itself on Windows or a missing R
# binary; the ready frame's `mode` says which one booted.
# The user's own settings, resolved once. `settings_state` is re-resolved by a
# successful settings_set so a later read reports the value that is actually
# in force, and `linger_s` / `HISTORY_ENABLED` below are the two the running
# kernel can honour immediately (see each one's `effect`).
settings_state <- carmar_settings_resolve()
settings_value <- function(key) carmar_settings_value(settings_state, key)
audit("settings", detail = "startup", status = settings_state$file$status,
      keys = length(settings_state$file$values))

# What every child inherits from the user's settings, and which R to run.
# Defined here so the three spawn sites cannot drift: a setting that reaches
# the worker but not the restarted worker is the worst kind of half-applied.
settings_child_env <- function() carmar_settings_child_env(settings_state)
settings_rscript <- function() {
  chosen <- settings_value("rscript_path")
  detect_rscript(if (is.character(chosen) && nzchar(chosen)) chosen
                 else Sys.getenv("CARMAR_RSCRIPT", ""))
}

# Where THIS session's workspace is saved for a "Restart into" handoff, and
# where a successor finds it. The state dir is the launcher's (CARMAR_STATE)
# or the package's; the file is per port, so two sessions never share one.
session_file <- local({
  state <- Sys.getenv("CARMAR_STATE", "")
  if (!nzchar(state)) state <- tools::R_user_dir("carmar", "data")
  file.path(state, sprintf("session-%d.RData", port))
})
start_execution_worker <- function() {
  started <- kernel_start(file.path(here, "worker-boot.R"), interactive = TRUE,
                          rscript = settings_rscript(),
                          env_extra = c(settings_child_env(),
                                        CARMAR_SESSION_FILE = session_file))
  # A restored workspace is restored ONCE, by the first worker of a successor
  # kernel; a plain Restart R later must start empty, as it says it does.
  Sys.unsetenv("CARMAR_RESTORE_WORKSPACE")
  started
}
k <- start_execution_worker()
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
sockets$worker_recovering <- FALSE
sockets$worker_restart_attempts <- 0L
sockets$worker_restart_after <- 0
# MCP routing state: which page most recently claimed to be the user's active
# window, and which agent socket is waiting for which request id.
sockets$active_page <- NULL
sockets$mcp_pending <- new.env(parent = emptyenv())
# Subscription chats: id → record of one running `claude -p` process, owned by
# the page socket that asked. See the agent-chat frames in handle_frame.
sockets$chats <- new.env(parent = emptyenv())
# Development jobs: id -> record of one `spike/job-run.R` process. Like chats
# they are supervisor children, and UNLIKE chats they are DETACHED — a job
# belongs to the kernel, not to the socket that started it, so its output is
# broadcast to every page and its log is buffered for a page that reconnects.
# That is Stage 6 item 7 and Stage 5 slice 3 in one field: the same model both
# asked for, which is why neither was built without the other.
sockets$jobs <- new.env(parent = emptyenv())
sockets$job_seq <- 0L
# Terminals: id -> record of one PTY shell, owned by the page that opened it.
# See "the terminal plane" below.
sockets$terms <- new.env(parent = emptyenv())
sockets$term_seq <- 0L
# Idle linger: a kernel nobody is connected to stops itself, so closing the
# notebook tab does not strand an R process (and five stranded days do not
# greet day six with the capacity prompt). The clock starts when the LAST
# socket — page or agent — closes, and only while the worker is idle: a run
# someone left cooking finishes first, and the finished session then waits a
# full grace period for its page to come back (a reload or auto-reattach
# cancels the clock). CARMAR_LINGER is the grace in seconds; 0 or negative
# disables. The clock never starts before the first connection ever, so a
# slow browser launch cannot lose the kernel it was opening.
linger_s <- suppressWarnings(as.numeric(settings_value("linger_seconds")))
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
# TRUE while the worker sits at a Browse prompt (a `debug` frame arrived and
# the paused run has not yet finished). Gates debug_cmd: raw console lines may
# only be sent when a browser() is actually reading them — at any other moment
# they would reach the dispatch loop as junk.
sockets$debug_paused <- FALSE
# The same shape one level along: a raw console line answering a readline() may
# only be sent while a readline() is actually reading it. Set by the worker's
# input_request frame, cleared by input_done and by the run ending, so an
# interrupted prompt cannot leave the gate open.
sockets$input_waiting <- FALSE
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
#' otherwise the bare spike page. The page is PINNED to this kernel's build
#' (never the highest version present), `.beta.min` ignored so
#' a debuggable build is served during development. The repo keeps builds in
#' ../dist; a distribution copy keeps the notebook right next to this folder.
#'
#' @return Path to an HTML file.
notebook_page <- function() {
  # One implementation, in notebook-page.R, because tools/app/launch.sh has to
  # ask the same question at open time — the announcement below is computed
  # once at startup and goes stale the moment a new build lands.
  page <- carmar_notebook_page(here, CARMAR_KERNEL_BUILD)
  if (nzchar(page)) return(page)
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

# A Finder double-click hands the document to the KERNEL, not the page URL:
# macOS `open` strips the fragment from file:// URLs before the browser sees
# it, so anything riding the address can silently vanish. The launcher parks
# the path here; the first PAGE to ask (open-request) receives it exactly
# once. Only the native launcher's environment can set this — no browser
# input reaches it, so the trust model is unchanged: the page still reads the
# file through the worker's ordinary readfile rules.
# A page opened from disk sends `Origin: null` — the literal four-character
# string, not an absent header. It is not a web origin and never becomes one,
# so it is spelled once here and approved through the SAME consent path as a
# published site rather than getting a second, parallel one.
#
# Until 0.60.88 it was allowed unconditionally, and that was the whole of
# issue #14: /health grants `Access-Control-Allow-Origin: *` on purpose so a
# saved notebook can find its kernel, and lib/kernel-ref.js walks 4747..65535
# looking for one. Any HTML file the user opened could therefore sweep for a
# kernel and send `exec` — the same shape as the Chrome extension deleted in
# 0.38.0 ("no token and no page identity"), which is documented there as
# unacceptable. A file page cannot hold a secret CarmaR never gave it, and
# CarmaR never sees it open (CarmaR.app claims .qmd/.Rmd/.R/... but not
# .html, so a double-clicked notebook goes straight to the browser). What is
# left is to ASK — once per session, on a local page that names what is
# asking. Absent Origin (native same-user clients) is deliberately unchanged:
# that process could run Rscript itself.
#' Bytes no caller can predict.
#'
#' `sample()` draws from the Mersenne Twister, which is seeded from the clock
#' and the pid and is trivially reconstructible — fine for shuffling a vector,
#' not for a value that decides whether a page may run R. /dev/urandom is the
#' OS CSPRNG on every platform CarmaR ships a kernel for except Windows, where
#' the RNG fallback is documented rather than hidden.
secure_token <- function(n = 32L) {
  alphabet <- c(letters, LETTERS, 0:9)
  con <- tryCatch(file("/dev/urandom", "rb"), error = function(e) NULL,
                  warning = function(w) NULL)
  if (!is.null(con)) {
    on.exit(close(con), add = TRUE)
    bytes <- tryCatch(readBin(con, "integer", n = n, size = 1L, signed = FALSE),
                      error = function(e) integer(0))
    if (length(bytes) == n) {
      return(paste(alphabet[(bytes %% length(alphabet)) + 1L], collapse = ""))
    }
  }
  # Windows, or an unreadable /dev/urandom. Weaker, and said so out loud.
  paste(sample(alphabet, n, replace = TRUE), collapse = "")
}

FILE_ORIGIN <- "null"

# A notebook page SERVED BY ANOTHER CarmaR KERNEL on this machine. When a
# page's own kernel dies the page stays open (it is HTML the browser already
# holds) and can attach to a different session through the badge's plug icon;
# it arrives here with the Origin of the kernel that served it,
# `http://127.0.0.1:<other port>`. That origin is approved through the same
# /pair click as a published site — nothing is granted for being loopback —
# but once approved it is classed `local`, not `published`, and `local` is in
# PAGE_ONLY_CLASSES: the page is the user's own notebook, and a notebook that
# could run R but not open its terminal or read its console history would be
# a half-attached one. Loopback only, so off-loopback deployments never see
# this class; a `localhost` origin is not a proxy's.
LOCAL_ORIGIN_RE <- "^http://(127\\.0\\.0\\.1|localhost|\\[::1\\]):[0-9]{1,5}$"
local_origin <- function(origin) {
  length(origin) == 1L && !is.na(origin) && grepl(LOCAL_ORIGIN_RE, origin)
}

# The capability that lets the notebook this kernel POINTED AT skip the
# consent click, while a file page that merely found the port cannot.
#
# serve.R prints the exact file:// URL it expects to be opened, and that URL
# is the one channel CarmaR controls: it already carries `#kernel=<port>`, so
# it can carry a secret too. The page reads it out of its own fragment and
# presents it on the WebSocket URL. A drive-by html file probing 4747..65535
# can reach /health and learn a kernel is there, but it was never handed this,
# and it is not in anything /health discloses.
#
# It is NOT a general kernel token: an absent Origin (native same-user
# clients) is unaffected, and the served http page still proves itself by
# being same-origin. It exists for exactly one case — "is this the file I told
# you to open, or a file that went looking?"
#
# Fragments do not survive a Finder double-click (macOS `open` strips them),
# which is precisely why the /pair consent click remains: this is the quiet
# path, not the only one.
# A handoff pre-mints the capability of its successor (session_handoff_begin),
# so the page can be told where to go BEFORE this process exits. Only the exact
# shape secure_token() makes is accepted; anything else is minted afresh.
FILE_LAUNCH_CAP <- local({
  given <- Sys.getenv("CARMAR_FILE_LAUNCH_CAP", "")
  Sys.unsetenv("CARMAR_FILE_LAUNCH_CAP")
  if (grepl("^[A-Za-z0-9]{32}$", given)) given else secure_token(32L)
})

open_file_env <- Sys.getenv("CARMAR_OPEN_FILE", "")
sockets$pending_open <- if (nzchar(open_file_env)) open_file_env else NULL
if (!is.null(sockets$pending_open)) audit("open-file-parked")

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
pairing_challenge <- function() secure_token(48L)

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
  is_file <- identical(origin, FILE_ORIGIN)
  is_local <- local_origin(origin)
  challenge <- pairing_challenge()
  sockets$pairing_requests[[challenge]] <- list(
    origin = origin, nonce = substr(nonce, 1L, 200L), created = as.numeric(Sys.time()))
  paste0('<!doctype html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; ',
    "style-src 'unsafe-inline'; form-action 'self'; base-uri 'none'\">",
    '<title>', if (is_file) 'Allow this notebook file to use CarmaR?'
               else if (is_local) 'Attach this notebook to this CarmaR session?'
               else 'Allow this book to use CarmaR?', '</title><style>',
    'body{font:16px/1.5 system-ui,sans-serif;max-width:42rem;margin:10vh auto;padding:0 1.25rem;color:#172033}',
    '.site{padding:.75rem 1rem;background:#f3f6fb;border-radius:.55rem;overflow-wrap:anywhere}',
    'button{border:0;border-radius:.5rem;background:#2357d9;color:white;padding:.7rem 1rem;font:inherit;font-weight:650;cursor:pointer}',
    'small{color:#59657a}</style></head><body>',
    if (is_file)
      paste0('<h1>Run this notebook file with your R?</h1>',
        '<p>A notebook opened from a file on this computer</p>',
        '<p class="site"><strong>a page opened from disk (file://)</strong></p>',
        '<p>wants to use this CarmaR session. The code will run as your user ',
        'and can access your files and network. A page opened from a file ',
        'cannot be identified any more exactly than this, so approve it only ',
        'if you just opened a notebook of your own.</p>')
    else if (is_local)
      paste0('<h1>Attach this notebook to this R session?</h1>',
        '<p>A CarmaR notebook page served at</p>',
        '<p class="site"><strong>', html_escape(origin), '</strong></p>',
        '<p>wants to use this CarmaR session instead of its own &mdash; usually ',
        'because the session that served it has stopped. It will be treated as ',
        'your notebook: its chunks run as your user, and it may open a terminal ',
        'and use your saved AI settings. Approve it only if you just asked a ',
        'notebook of your own to attach.</p>')
    else
      paste0('<h1>Run this book with your R?</h1><p>The published site</p>',
        '<p class="site"><strong>', html_escape(origin), '</strong></p>',
        '<p>wants to send R chunks to this CarmaR session on your computer. ',
        'The code will run as your user and can access your files and network.</p>'),
    '<form method="get" action="/pair/approve"><input type="hidden" name="challenge" value="',
    challenge, '"><button type="submit">',
    if (is_local) 'Attach for this session' else 'Allow for this session',
    '</button></form>',
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
    '<h1>CarmaR is connected</h1><p>This session now runs chunks pressed on</p>',
    '<p class="site"><strong>', html_escape(rec$origin), '</strong></p>',
    '<p>The page connects by itself and closes this window; if it stays open, you can close it. ',
    'The approval lasts until this R session stops.</p><script>(function(){',
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
  # A published site talks through the bridge window; a file or local notebook
  # page dials this kernel directly the moment its retry fires, so the only
  # thing left for this window to do is say so and let the user close it.
  if (identical(rec$origin, FILE_ORIGIN) || local_origin(rec$origin)) {
    return(pairing_done_page(rec))
  }
  pairing_bridge_page(rec)
}

pairing_done_page <- function(rec) {
  paste0('<!doctype html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; ',
    "style-src 'unsafe-inline'; base-uri 'none'\">",
    '<title>CarmaR attached</title><style>',
    'body{font:16px/1.5 system-ui,sans-serif;max-width:38rem;margin:10vh auto;padding:0 1.25rem;color:#172033}',
    '.site{overflow-wrap:anywhere;color:#2357d9}</style></head><body>',
    '<h1>Attached</h1><p>The notebook</p>',
    '<p class="site"><strong>',
    if (identical(rec$origin, FILE_ORIGIN)) 'a page opened from disk (file://)' else html_escape(rec$origin),
    '</strong></p>',
    '<p>may use this CarmaR session until it stops. It reconnects by itself within ',
    'a few seconds; you can close this window.</p></body></html>')
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
  if (nzchar(origin) && !(origin %in% origins_ok) &&
      !(allow_file && identical(origin, "null"))) {
    return(reject("bad origin", origin))
  }
  # Off loopback, "no Origin" is not a same-user native client any more — it is
  # anyone with a socket. Control endpoints (shutdown, authorize, open-file)
  # are the ones where that difference is expensive, so they close first.
  if (!nzchar(origin) && !isTRUE(deployment$allow_native)) {
    return(reject("origin required", "(none)"))
  }
  site <- tolower(req$HTTP_SEC_FETCH_SITE %||% "")
  if (identical(site, "cross-site")) return(reject("cross-site request", site))
  NULL
}

#' Which of the four accepted classes opened this socket?
#'
#' The upgrade gate decides WHETHER a socket may open and then throws its
#' reasoning away: four different trust cases all end in the same `NULL`. Some
#' decisions need the reasoning rather than the verdict, because "may this
#' connection read the user's provider key?" is not the question "may it
#' connect?".
#'
#' httpuv hands onWSOpen the very Rook environment the gate inspected
#' (WebSocket$initialize stores it as `$request`), so the class is recomputed
#' from the same header instead of threaded through shared state — one reader,
#' no second copy to fall out of step.
#'
#' It fails CLOSED: no request environment means "unknown", and unknown is
#' trusted with nothing.
socket_class <- function(req) {
  if (is.null(req)) return("unknown")
  origin <- req$HTTP_ORIGIN
  # No Origin at all: not a browser. On loopback that is the launcher, R, curl
  # or the MCP bridge — the same OS user, who could have run Rscript anyway. Off
  # loopback the same header means an unauthenticated stranger, and the upgrade
  # gate has already refused it; "unknown" is trusted with nothing either way.
  if (is.null(origin)) return(if (isTRUE(deployment$allow_native)) "native" else "unknown")
  if (origin %in% origins_ok) return("served")
  if (identical(origin, FILE_ORIGIN)) return("file")
  # A page another loopback kernel served, approved through /pair (see
  # LOCAL_ORIGIN_RE): the user's own notebook re-attached, not a reader's site.
  if (local_origin(origin)) return("local")
  # Anything else reached a 101 only by being in published_approvals.
  "published"
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

# ── the AI key lives HERE, not in the browser ────────────────────────────────
#
# It used to sit in localStorage under `carmar-ai-key-kept`: in the browser's
# storage for this origin, readable by anything that can run script in the page,
# and one careless export away from being inside a notebook file. A provider key
# is not notebook content — it is a machine credential — so it belongs where the
# machine's other state lives: the user's own R config directory, 0600.
#
# THE SUPERVISOR ANSWERS THIS ITSELF. It never reaches worker.R (which evaluates
# user code) or analyze.R.
#
# Ops a declared MCP agent is refused outright: raw evaluation (it must go
# through a visible chunk_run) and the user's credentials.
AGENT_REFUSED <- c("exec", "interrupt", "force_stop", "restart", "session_upgrade", "debug_cmd", "ai-key",
                   "console_history", "input_reply", "ai-audit-read",
                   "job_start", "job_stop", "job_open",
                   "term_open", "term_input", "term_close",
                   "session_list", "r_versions",
                   "settings_get", "settings_set", "settings_reset",
                   "update_status", "update_action", "project_action")

# `job_start` is on that list DELIBERATELY, and it is the entry most open to
# argument. A job is visible — it announces itself, streams into a pane and
# survives in a list — so the "run it where the user can see it" rule that
# sends an agent through chunk_run is already satisfied, and "run the test
# suite" is among the most useful things an agent could ask for.
#
# It is refused anyway, for now, because it would be a SECOND evaluation door
# and the case for opening it is a policy decision, not a consequence of
# building the mechanism. Reading job state (`job_list`, `job_root`) is not
# refused: an agent that can see the suite is red is more useful and evaluates
# nothing.

# ...and WHO may touch the key, which is a different question with a different
# kind of answer. AGENT_REFUSED asks "did this client DECLARE itself an agent?"
# — an honour system: rec$role is "page" until the client volunteers mcp-hello,
# so an agent CLI that simply never declared used to be handed the key. This
# asks what the upgrade gate already PROVED about the connection, which nothing
# the client says afterwards can change. Two lists rather than one flagged one,
# for the same reason FORWARDED and ANALYZE_FORWARDED stay two.
#
# Only a notebook page qualifies. A native client is refused even though it
# could read the 0600 file itself as this user: the point is not that CarmaR
# can stop it, but that CarmaR does not hand the key over — so the promise
# holds for every agent instead of only the polite ones. A published site is
# refused because its reader approved it to RUN THE CHUNKS THEY PRESS, not to
# collect a credential on the way past.
PAGE_ONLY_CLASSES <- c("served", "file", "local")

# ── what the page may write into the audit stream ───────────────────────────
#
# The AI conversation happens in the BROWSER — a key-based turn is a fetch from
# the page — so the kernel cannot observe it and the page has to report it. That
# makes this the one op where the log's content comes from outside, which is a
# log-injection surface if it is a free-text channel. It is not one:
#
#   · `event` must be one of a fixed vocabulary,
#   · every other field is an ALLOW-LIST of scalars, bounded and stripped of
#     control characters (a newline in a JSON-lines log is a forged record),
#   · and the fields that ESTABLISH the record — the timestamp, the pid, which
#     connection sent it and whether that connection is a page or a declared
#     agent — are stamped by the supervisor and cannot be supplied at all.
#
# An allow-list rather than a deny-list, for the same reason FORWARDED is one:
# a field added to the page later is absent from the log until someone puts it
# here on purpose.
AI_AUDIT_EVENTS <- c("turn", "tool", "insert", "patch", "accept", "reject", "error")
AI_AUDIT_FIELDS <- c("provider", "model", "lane", "tool", "chunk", "decision",
                     "kind", "by", "reason", "ms", "input_tokens",
                     "output_tokens", "prompt_chars", "response_chars", "error")

# The prompt and the response themselves. OFF unless an administrator asks,
# because a notebook that silently recorded everything its user typed at an AI
# would be a worse thing to ship than no audit at all — the same judgement
# CARMAR_LOG itself is off by default for. A compliance deployment turns it on
# knowingly; a laptop never does.
AI_AUDIT_TEXT_FIELDS <- c("prompt", "response")
ai_audit_text <- identical(Sys.getenv("CARMAR_LOG_AI_TEXT", ""), "1")

# How many ai-* records the reviewer export may read back at once.
AI_AUDIT_READ_MAX <- 5000L

#' One field of an audit record, made safe to write on one line.
ai_audit_scalar <- function(value, limit) {
  if (is.null(value) || length(value) != 1L || is.list(value)) return(NULL)
  if (is.numeric(value) && is.finite(value)) return(value)
  if (is.logical(value) && !is.na(value)) return(value)
  if (!is.character(value) || is.na(value)) return(NULL)
  out <- gsub("[[:cntrl:]]", " ", value)
  trimws(substr(out, 1L, limit))
}

ai_key_path <- function() {
  dir <- tools::R_user_dir("carmar", "config")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "ai-key")
}

ai_key_read <- function() {
  path <- ai_key_path()
  if (!file.exists(path)) return("")
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (!length(lines)) "" else trimws(lines[[1]])
}

ai_key_write <- function(value) {
  path <- ai_key_path()
  if (!nzchar(value)) {
    # unlink() REPORTS failure in its return value rather than raising, so an
    # unwritable config directory used to answer {ok:true} while the key sat
    # there. "Forget the key" reporting success over a key that is still on
    # disk is the one lie this function must not tell.
    if (!file.exists(path)) return(TRUE)
    return(identical(unlink(path), 0L) && !file.exists(path))
  }
  # Lock the file down BEFORE the secret goes into it. Creating it with the key
  # already inside leaves a window where it is world-readable, and a key that
  # was briefly readable has already leaked.
  if (!file.exists(path) && !file.create(path)) return(FALSE)
  # The chmod runs BEFORE the secret exists in the file, and its result is
  # checked: Sys.chmod returns FALSE rather than raising, so on a filesystem
  # that allows writing but not mode changes the key would otherwise have been
  # written world-readable under a promise of 0600. Refuse instead.
  if (!isTRUE(Sys.chmod(path, "0600"))) {
    unlink(path)
    return(FALSE)
  }
  ok <- tryCatch({ writeLines(value, path); TRUE }, error = function(e) FALSE)
  if (!ok) unlink(path)
  ok
}

# ── the console's history, across sessions ───────────────────────────────────
#
# RStudio keeps `.Rhistory`; a console whose ↑ forgets everything the moment the
# tab reloads is a demo, not a tool. It lives beside the key and for the same
# reason: this is MACHINE state, not notebook content, so it belongs in the R
# user directory and never in the notebook file, an export, or localStorage —
# where any local page sharing the file:// origin could read the user's code.
#
# Off entirely with CARMAR_NO_HISTORY=1, because "record everything I type"
# must be refusable.
HISTORY_MAX <- 2000L
# Consulted INSIDE history_read()/history_add() per call, never captured at the
# top — which is what makes flipping it in Settings take effect immediately
# rather than at the next launch.
HISTORY_ENABLED <- isTRUE(settings_value("history_enabled"))

history_path <- function() {
  dir <- tools::R_user_dir("carmar", "data")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "console-history")
}

history_read <- function() {
  if (!HISTORY_ENABLED) return(character())
  path <- history_path()
  if (!file.exists(path)) return(character())
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  # ONE JSON STRING PER LINE, not raw text. A history entry can be a whole
  # multi-line function definition, and the file is line-oriented, so the entry
  # has to survive a newline inside it — hand-rolled backslash escaping got
  # this wrong in both directions on the first attempt. jsonlite already knows
  # how; a malformed line is dropped rather than allowed to poison the rest.
  raw <- lines[nzchar(lines)]
  if (!length(raw)) return(character())
  decoded <- vapply(raw, function(x) {
    v <- tryCatch(fromJSON(x), error = function(e) NA_character_)
    if (is.character(v) && length(v) == 1L) v else NA_character_
  }, character(1), USE.NAMES = FALSE)
  decoded[!is.na(decoded)]
}

history_add <- function(line) {
  if (!HISTORY_ENABLED || !nzchar(trimws(line))) return(TRUE)
  path <- history_path()
  flat <- as.character(toJSON(line, auto_unbox = TRUE))
  kept <- tryCatch({
    prior <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
    # Consecutive duplicates are noise in a history: pressing ↑ twice should
    # reach the command BEFORE the one just run, not the same one again.
    if (length(prior) && identical(prior[[length(prior)]], flat)) prior
    else c(prior, flat)
  }, error = function(e) flat)
  if (length(kept) > HISTORY_MAX) kept <- utils::tail(kept, HISTORY_MAX)
  # Same order as the key: create, lock, then write. History is a record of
  # everything the user has typed and is nobody else's business.
  if (!file.exists(path) && !file.create(path)) return(FALSE)
  if (!isTRUE(Sys.chmod(path, "0600"))) return(FALSE)
  tryCatch({ writeLines(kept, path); TRUE }, error = function(e) FALSE)
}

FORWARDED <- c("env", "obj", "struct", "view", "colstats", "rm", "packages", "doctor",
               "package_action", "package_help", "project_status", "project_action", "help", "wd",
               "parse", "complete", "files", "import", "readfile", "writefile", "writefiles_atomic",
               "hover", "format", "sniff", "choose",
               # The file tree's New Folder, Rename and Delete. These MUTATE
               # the filesystem, which is a wider door than the read-only ops
               # around them, so the bar they clear is stated rather than
               # assumed: `writefile` — already here — can create, overwrite
               # and truncate any file the worker may touch, so creating an
               # empty directory and renaming an entry add no reach it did not
               # already have. `deletepath` genuinely does add reach, and it is
               # here because a file explorer without Delete is not one; it is
               # bounded by the same `within_root()` every file op uses, checks
               # BOTH ends of a rename, refuses to clobber, and reports each
               # path's outcome instead of a single success flag. The
               # confirmation naming the files lives in the page, which is the
               # only place that knows what the user selected.
               "mkdir", "renamepath", "deletepath", "copypath", "revealpath",
               # Registers breakpoint lines; the worker arms/clears traces and
               # answers with what it armed. NOTE `cmdfile` is deliberately NOT
               # here: it makes the worker read a file path as a command, and
               # only kernel.R's own spill path may mint one.
               "debug_breaks")

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
ANALYZE_FORWARDED <- c("analyze", "analyze_ping", "analyze_workspace",
                       "analyze_workspace_references")

# Output held for a run whose page went away, replayed to whoever adopts it.
# A quarter of a megabyte is far more than any chunk prints on purpose and far
# less than a runaway `print()` loop prints by accident — and the cap matters
# because the page that would have consumed this output is, by definition, not
# there to apply back-pressure.
ORPHAN_BUFFER_BYTES <- 262144L

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
    # `approved` covers both consent cases, because they ARE one case: an
    # exact string the user approved this session. FILE_ORIGIN is in the same
    # store rather than a flag of its own, so "who may open a socket?" has one
    # answer to read and one place to revoke.
    approved <- !is.null(origin) &&
      exists(origin, sockets$published_approvals, inherits = FALSE)
    # A file page may also present the capability from the URL this kernel
    # printed. Compared with a constant-time-ish identical() on two strings of
    # equal length; the value never appears in a response, only in the URL the
    # launcher opened.
    launched <- identical(origin, FILE_ORIGIN) &&
      identical(query_param(req$QUERY_STRING, "pair"), FILE_LAUNCH_CAP)
    # THE INVERSION, and the whole security change of serving off loopback: an
    # absent Origin is allowed on 127.0.0.1 because it proves the client is not
    # a browser and therefore is this same OS user. Bound to the network it
    # proves nothing at all, so it is refused instead of trusted.
    if (is.null(origin) && !isTRUE(deployment$allow_native)) {
      return(reject("origin required", "(none)"))
    }
    if (!is.null(origin) && !(origin %in% origins_ok) &&
        !approved && !launched) {
      return(reject("bad origin", origin))
    }
    if (length(sockets$open) >= MAX_SOCKETS) return(reject("too many connections"))
    NULL
  },

  call = function(req) {
    if (!host_ok(req)) return(reject("bad host", req$HTTP_HOST %||% "(none)"))
    if (identical(req$PATH_INFO, "/health")) {
      # CORS is granted HERE AND ONLY HERE, so a notebook page (a file:
      # origin) probing for its kernel can read which port holds a pending
      # double-clicked document and prefer it. Booleans only — the path
      # itself never appears in an unauthenticated response.
      return(resp(200L, "application/json",
                  toJSON(c(list(ok = TRUE, worker = k$proc$is_alive(),
                              pending_open = !is.null(sockets$pending_open),
                              # For the attach chooser on a page whose own
                              # kernel died: which R, and how old the
                              # session is. No title, no path — /health is
                              # unauthenticated and CORS-open on purpose.
                              r = paste(R.version$major, R.version$minor, sep = "."),
                              started = as.numeric(sockets$boot_at),
                              protocol = CARMAR_PROTOCOL_VERSION,
                              kernel_build = CARMAR_KERNEL_BUILD,
                              # What is installed beside this kernel NOW, and
                              # how many pages hold it — the two facts an
                              # upgrade needs from a running session.
                              installed_build = installed_build(),
                              pages = length(page_recs()),
                              capabilities = c("published-direct-v1",
                                               "published-pairing-v3")),
                           handoff_health()),
                         auto_unbox = TRUE),
                  extra = list("Access-Control-Allow-Origin" = "*")))
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
      # FILE_ORIGIN is accepted HERE and only here. `published_origin_valid()`
      # stays strict (`^https?://…`) because it also guards
      # /published/authorize and the CARMAR_PUBLISHED_ORIGIN seed — widening it
      # would let an environment variable or a native launch silently
      # pre-approve every file on disk, which is the opposite of the point.
      if (!identical(origin, FILE_ORIGIN) && !published_origin_valid(origin)) {
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
    # The native door of "Restart into the installed CarmaR" — the menu
    # helper's row and the upgrade script use it (launch.sh --restart). Gated
    # exactly like /shutdown; the page's own door is the session_upgrade op.
    if (identical(req$PATH_INFO, "/session/upgrade")) {
      blocked <- control_rejection(req, allow_file = TRUE)
      if (!is.null(blocked)) return(blocked)
      if (!identical(req$REQUEST_METHOD, "POST")) {
        return(resp(405L, "application/json",
                    toJSON(list(ok = FALSE, error = "POST only"), auto_unbox = TRUE)))
      }
      begun <- session_handoff_begin()
      return(resp(if (isTRUE(begun$ok)) 200L else 409L, "application/json",
                  toJSON(begun, auto_unbox = TRUE)))
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
    # The double-click door: /open?file=/abs/path. This is how a Finder-opened
    # document reaches a kernel that is ALREADY RUNNING, and it is the reason
    # opening a file no longer costs an R session.
    #
    # The other channel — CARMAR_OPEN_FILE in the supervisor's environment —
    # is read once at startup, so the only way to deliver a document through
    # it is to start a new process. That is precisely what the launcher used
    # to do: every double-click was a fresh supervisor, worker and analyzer,
    # and four files opened in a minute were twelve R processes. This door
    # takes the same path and hands it to the session the user already has.
    #
    # There is NO extension whitelist here, deliberately. The old one
    # (`qmd|rmd|md|markdown`) predated the page's own router and contradicted
    # the app's Info.plist, which claims .R and .RData — so a double-clicked
    # R script was rejected by the very door meant to open it. The authority
    # on what may be read is `readfile` in the WORKER, which enforces the path
    # rules, refuses binaries and bounds the size, with a reason; a second,
    # weaker copy of that judgment here only ever produced false rejections.
    #
    # Origin/Sec-Fetch gated like /shutdown: a drive-by page must not be able
    # to make the notebook open files by guessing paths.
    if (identical(req$PATH_INFO, "/open")) {
      blocked <- control_rejection(req)
      if (!is.null(blocked)) return(blocked)
      f <- query_param(req$QUERY_STRING, "file")
      if (!nzchar(f) || !file.exists(f) || dir.exists(f)) {
        audit("open-rejected", detail = f)
        return(resp(400L, "text/plain", "no such file"))
      }
      f <- normalizePath(f)
      page <- target_page()
      if (is.null(page)) {
        # No page is connected yet — this kernel was started for this very
        # document and its notebook is still loading. Park it exactly as the
        # environment channel does, and the first page to ask collects it.
        sockets$pending_open <- f
        audit("open-parked", detail = f)
        return(resp(200L, "application/json", '{"ok":true,"delivered":"parked"}'))
      }
      audit("open", detail = f)
      sent <- tryCatch({
        page$ws$send(toJSON(list(type = "open-file", path = f), auto_unbox = TRUE))
        TRUE
      }, error = function(e) FALSE)
      if (!sent) {
        # The page we picked died between the roster read and the send. Park
        # rather than report a success nobody acted on.
        sockets$pending_open <- f
        return(resp(200L, "application/json", '{"ok":true,"delivered":"parked"}'))
      }
      return(resp(200L, "application/json", '{"ok":true,"delivered":"page"}'))
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
    # What the upgrade gate proved, kept. Read once here rather than per frame:
    # the Rook environment belongs to the request that opened this socket and
    # cannot change under it.
    rec$class <- socket_class(ws$request)
    # Who the reverse proxy says this is. "" on loopback, always — CarmaR has
    # no login of its own and must not grow one here: a header is an identity
    # only for as long as nobody but the proxy can reach the port, which is a
    # property of the deployment, not of this process.
    rec$user <- proxy_user(ws$request, deployment)
    # Liveness: `last_seen` is stamped by every frame this socket sends, and
    # `beats` records that it speaks the heartbeat at all. Both are read only
    # by reap_dead_sockets().
    rec$last_seen <- Sys.time()
    rec$beats <- FALSE
    sockets$open <- c(sockets$open, rec)
    sockets$ever_connected <- TRUE
    audit("socket-open", sockets = length(sockets$open), class = rec$class,
          user = rec$user)
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
      # A terminal is the page's: no page, no shell.
      for (id in ls(sockets$terms)) {
        if (identical(sockets$terms[[id]]$rec, rec)) term_close(id, "page closed")
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
  # THE OTHER DOOR THE SUPERVISOR OWNS, and the one with no page-side bypass
  # at all: a Claude Code / Codex turn is a process THIS process spawns. If the
  # policy does not name the provider, it is never spawned — there is nothing
  # in the browser to work around, because the spawn was never there.
  if (!ai_provider_allowed(client, ai_policy)) {
    audit("agent-chat-refused", client = client, reason = "policy")
    chat_send(rec, id, done = TRUE, code = -1L,
      error = sprintf("%s is not permitted here. %s", label, ai_policy_reason(ai_policy)))
    return(invisible(NULL))
  }
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

# ── sessions and R installations ───────────────────────────────────────────

#' Every kernel this user is running, from the runtime records, health-checked.
#' @return An unnamed list of `list(port, url, title, pid, alive, this)`.
session_list <- function() {
  dir <- Sys.getenv("CARMAR_RUNTIME_DIR", file.path(path.expand("~"), ".carmar", "run"))
  files <- list.files(dir, pattern = "^kernel-[0-9]+\\.json$", full.names = TRUE)
  out <- lapply(files, function(f) {
    rec <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.list(rec) || is.null(rec$port) || is.null(rec$url)) return(NULL)
    p <- suppressWarnings(as.integer(rec$port))
    if (is.na(p)) return(NULL)
    this <- identical(p, as.integer(port))
    alive <- if (this) TRUE else tryCatch({
      con <- url(paste0(sub("/+$", "", rec$url), "/health"))
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      old <- options(timeout = 1); on.exit(options(old), add = TRUE)
      txt <- readLines(con, n = 1L, warn = FALSE)
      length(txt) == 1L && grepl("\"ok\":true", txt, fixed = TRUE)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    list(port = p, url = as.character(rec$url), title = rec$title %||% "",
         pid = rec$pid %||% NA, alive = alive, this = this)
  })
  out <- Filter(Negate(is.null), out)
  out[order(vapply(out, function(s) s$port, integer(1)))]
}

#' The Rscript the running worker was started with.
current_rscript <- function() tryCatch(normalizePath(settings_rscript(), mustWork = FALSE),
                                        error = function(e) "")

#' Every R installation on this machine, with its version. Discovery walks the
#' same ladder detect_rscript() does plus the versioned framework directories,
#' /opt/R (rig, Posit) and Homebrew; versions are read once per kernel life.
#' @return An unnamed list of `list(path, version, current)`, newest first.
r_versions <- function() {
  if (!is.null(sockets$r_versions_cache)) return(sockets$r_versions_cache)
  cands <- c(
    Sys.glob("/Library/Frameworks/R.framework/Versions/*/Resources/bin/Rscript"),
    Sys.glob("/opt/R/*/bin/Rscript"),
    Sys.glob("/opt/homebrew/Cellar/r/*/bin/Rscript"),
    "/opt/homebrew/bin/Rscript", "/usr/local/bin/Rscript", "/opt/local/bin/Rscript",
    "/usr/bin/Rscript", "/usr/lib/R/bin/Rscript",
    Sys.glob("/usr/lib/R-*/bin/Rscript"),
    unname(Sys.which("Rscript")))
  cands <- unique(cands[nzchar(cands) & file.exists(cands)])
  # `Versions/Current` and a Homebrew symlink point at a versioned install
  # already in the list: keep one entry per real binary, preferring the
  # versioned path because it says which R it is.
  real <- vapply(cands, function(p) tryCatch(normalizePath(p), error = function(e) p), character(1))
  keep <- !duplicated(real)
  cands <- cands[keep]; real <- real[keep]
  cur <- current_rscript()
  ver <- vapply(cands, function(p) {
    out <- tryCatch(suppressWarnings(system2(p, "--version", stdout = TRUE, stderr = TRUE, timeout = 10)),
                    error = function(e) character())
    hit <- regmatches(out, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", out))
    if (length(hit)) hit[[1]] else ""
  }, character(1))
  rows <- Map(function(p, r, v) list(path = p, version = v, current = identical(r, cur)), cands, real, ver)
  rows <- unname(rows[order(vapply(rows, function(x) x$version, character(1)), decreasing = TRUE)])
  sockets$r_versions_cache <- rows
  rows
}

# ── the terminal plane ──────────────────────────────────────────────────────
#
# A shell in the page: a FOURTH kind of child, and the same argument as the
# jobs plane one step further out. It evaluates whatever the user types, so it
# may not be analyze.R; it holds no session anybody else can see, so it need
# not be worker.R — a `make` that runs for a minute costs neither the user's
# variables nor a working Stop. Three properties, each a decision:
#
#   * It is a REAL pty (processx pty = TRUE): the shell sees a terminal, so
#     prompts, colours, line editing, ^C and password prompts all behave. A
#     pipe would give a shell that prints "not a tty" and buffers everything.
#   * It belongs to the PAGE, not to the kernel (the opposite of a job): it
#     dies with the socket that opened it. A shell nobody can see is not a
#     detached task, it is an orphan with the user's credentials.
#   * It is page-only and agent-refused. An agent runs code through chunk_run,
#     visibly, or not at all — a terminal would be exactly the raw `exec` door
#     the MCP plane refuses, wearing a prompt.
#
# What the shell can do is what the user can do; this grants nothing new and
# is audited like every other door. `CARMAR_NO_TERMINAL=1` removes it (a
# deployment that must not offer a shell); Windows has no pty here and says so.
MAX_TERMS <- 4L
TERM_INPUT_MAX <- 65536L

term_send <- function(rec, id, ...) {
  frame <- toJSON(list(type = "term", id = id, ...), auto_unbox = TRUE, null = "null")
  try(rec$ws$send(frame), silent = TRUE)
}

#' The shell to run and how to make it interactive. bash and zsh get a login
#' shell so PATH matches the user's own terminal; sh gets -i.
#' @return `list(cmd, args)`.
term_shell <- function() {
  shell <- Sys.getenv("SHELL", "")
  if (!nzchar(shell) || !file.exists(shell)) shell <- "/bin/sh"
  base <- basename(shell)
  args <- if (base %in% c("bash", "zsh", "fish")) c("-i", "-l") else "-i"
  list(cmd = shell, args = args)
}

term_open <- function(rec, cmd) {
  refuse <- function(why) {
    audit("term-refused", detail = why)
    term_send(rec, "", event = "refused", error = why)
    invisible(NULL)
  }
  if (identical(Sys.getenv("CARMAR_NO_TERMINAL"), "1")) {
    return(refuse("The terminal is turned off on this kernel (CARMAR_NO_TERMINAL=1)."))
  }
  if (.Platform$OS.type == "windows") {
    return(refuse("The terminal needs a pty, which CarmaR does not provide on Windows yet."))
  }
  mine <- Filter(function(t) identical(t$rec, rec), mget(ls(sockets$terms), envir = sockets$terms))
  if (length(mine) >= MAX_TERMS) {
    return(refuse(sprintf("%d terminals are already open on this page.", length(mine))))
  }
  clamp <- function(x, lo, hi, default) {
    v <- suppressWarnings(as.integer(x))
    if (length(v) != 1L || is.na(v)) default else max(lo, min(hi, v))
  }
  cols <- clamp(cmd$cols, 20L, 400L, 100L)
  rows <- clamp(cmd$rows, 5L, 200L, 30L)
  wd <- if (scalar_chr(cmd$cwd) && dir.exists(cmd$cwd)) cmd$cwd else getwd()
  sh <- term_shell()
  proc <- tryCatch(
    processx::process$new(sh$cmd, sh$args, pty = TRUE,
                          pty_options = list(rows = rows, cols = cols),
                          wd = wd, encoding = "UTF-8",
                          env = c("current", TERM = "xterm-256color",
                                  COLORTERM = "truecolor",
                                  COLUMNS = as.character(cols), LINES = as.character(rows),
                                  CARMAR_PORT = as.character(port))),
    error = function(e) conditionMessage(e))
  if (is.character(proc)) return(refuse(paste("the shell could not start:", proc)))
  sockets$term_seq <- sockets$term_seq + 1L
  id <- paste0("term-", sockets$term_seq)
  rec_t <- new.env(parent = emptyenv())
  rec_t$id <- id
  rec_t$proc <- proc
  rec_t$rec <- rec
  rec_t$started <- as.numeric(Sys.time())
  sockets$terms[[id]] <- rec_t
  audit("term-open", id = id, detail = sh$cmd, class = rec$class %||% "unknown")
  term_send(rec, id, event = "open", shell = sh$cmd, pid = proc$get_pid(),
            cols = cols, rows = rows, cwd = wd)
  invisible(NULL)
}

#' Only the page that opened a terminal may type into it or close it.
term_owned <- function(rec, id) {
  t <- if (scalar_chr(id)) sockets$terms[[id]] else NULL
  if (is.null(t) || !identical(t$rec, rec)) NULL else t
}

term_input <- function(rec, cmd) {
  t <- term_owned(rec, cmd$id)
  if (is.null(t) || !scalar_chr(cmd$text) || !nzchar(cmd$text)) return(invisible(NULL))
  if (nchar(cmd$text, type = "bytes") > TERM_INPUT_MAX) return(invisible(NULL))
  try(t$proc$write_input(cmd$text), silent = TRUE)
  invisible(NULL)
}

term_close <- function(id, why = "closed") {
  t <- sockets$terms[[id]]
  if (is.null(t)) return(invisible(NULL))
  try(t$proc$kill(), silent = TRUE)
  rm(list = id, envir = sockets$terms)
  audit("term-close", id = id, detail = why)
  term_send(t$rec, id, event = "exit", code = NA, reason = why)
  invisible(NULL)
}

#' Stream every terminal's bytes to its page; close out the ones that ended.
pump_terms <- function() {
  for (id in ls(sockets$terms)) {
    t <- sockets$terms[[id]]
    try(t$proc$poll_io(0L), silent = TRUE)
    out <- tryCatch(t$proc$read_output(), error = function(e) "")
    if (length(out) == 1L && nzchar(out)) term_send(t$rec, id, event = "data", text = out)
    if (!t$proc$is_alive()) {
      leftover <- tryCatch(t$proc$read_output(), error = function(e) "")
      if (length(leftover) == 1L && nzchar(leftover)) term_send(t$rec, id, event = "data", text = leftover)
      code <- as.integer(tryCatch(t$proc$get_exit_status(), error = function(e) NA_integer_) %||% NA_integer_)
      rm(list = id, envir = sockets$terms)
      audit("term-exit", id = id, detail = as.character(code))
      term_send(t$rec, id, event = "exit", code = code, reason = "exited")
    }
  }
  invisible(NULL)
}

# ── the jobs plane ──────────────────────────────────────────────────────────
#
# A development task (test / check / document / load / build / install) runs
# in its OWN R process, spawned here. Three properties, each of which is the
# reason a stage doc asked for this:
#
#   * It never consumes the interactive worker. `devtools::check()` takes
#     minutes and evaluates the package's own tests; doing that in the session
#     would take the user's variables with it and make Stop a lie for the
#     duration. (Stage 5 slice 3.)
#   * It is DETACHED: the job belongs to the kernel, not to the socket that
#     started it. Its output is broadcast to every page, its log is buffered
#     so a page that reloads mid-check catches up, and it keeps the kernel
#     alive past the idle linger. (Stage 6 item 7.)
#   * It carries no session identity. Nothing it does can be mistaken for the
#     notebook's state, which is what makes it safe for a third child process
#     to evaluate at all — see the header of spike/job-run.R.
#
# Reuses kernel_start/kernel_poll rather than a second spawn path, so a job
# inherits the UTF-8 child locale, the scrubbed R_HOME/R_LIBS environment and
# the bounded pipe drain that keep the event loop responsive.

# How much of a job's output is kept for a page that reconnects. A `check`
# prints a few thousand lines; this holds a whole one and truncates the
# pathological case rather than letting one job's log grow without limit in a
# supervisor that must stay responsive.
JOB_LOG_MAX <- 5000L
# Finished jobs stay in the list so their log can still be read. Beyond this
# the oldest are dropped.
JOB_HISTORY_MAX <- 20L
# Concurrent jobs. R package tasks are CPU- and disk-heavy; four at once is
# already more than any machine enjoys, and an unbounded count is a way to
# fork-bomb a laptop by holding down a button.
MAX_JOBS <- 4L

#' Send one frame to every notebook page.
#'
#' Broadcast, not addressed, because a job is detached: the page that pressed
#' Test may be gone, and the page that is here now is the one that needs to
#' see the result.
#' @param text A JSON frame, already encoded.
#' @return Invisibly NULL.
job_broadcast <- function(text) {
  lapply(page_recs(), function(r) try(r$ws$send(text), silent = TRUE))
  invisible(NULL)
}

#' Build and broadcast a supervisor-authored job frame.
#' @param id Job id.
#' @param ... Fields of the frame.
#' @return Invisibly NULL.
job_emit <- function(id, ...) {
  job_broadcast(toJSON(list(type = "job", id = id, ...),
                       auto_unbox = TRUE, null = "null"))
}

#' Append output to a job's buffer, bounded.
#' @param job A job record.
#' @param text One or more lines.
#' @return Invisibly NULL.
job_log <- function(job, text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (!length(lines)) return(invisible(NULL))
  job$log <- c(job$log, lines)
  if (length(job$log) > JOB_LOG_MAX) {
    dropped <- length(job$log) - JOB_LOG_MAX
    job$dropped <- job$dropped + dropped
    job$log <- utils::tail(job$log, JOB_LOG_MAX)
  }
  invisible(NULL)
}

#' What a page needs to render one job in a list.
#' @param job A job record.
#' @param with_log Include the buffered log.
#' @return A list.
job_snapshot <- function(job, with_log = FALSE) {
  out <- list(id = job$id, task = job$task, root = job$root,
              target = job$target, output = job$output, title = job$title,
              state = job$state, ok = job$ok, error = job$error,
              started = job$started, elapsed = job$elapsed,
              counts = job$counts, rows = I(job$rows),
              truncated = job$truncated, dropped = job$dropped)
  if (with_log) out$log <- I(as.list(job$log))
  out
}

#' Start one development task.
#' @param rec The page socket that asked.
#' @param cmd The parsed frame.
#' @return Invisibly NULL.
job_start <- function(rec, cmd) {
  running <- Filter(function(j) identical(j$state, "running"),
                    mget(ls(sockets$jobs), envir = sockets$jobs))
  if (length(running) >= MAX_JOBS) {
    job_emit("", event = "refused",
             error = sprintf("%d jobs are already running. Wait for one to finish, or stop it.",
                             length(running)))
    return(invisible(NULL))
  }
  # The pure decision, made in one place so the UI's message and the
  # supervisor's behaviour cannot disagree.
  # `target` is the one thing the task acts on — a package root, or a document
  # for `render`. jobs.R decides which of those it must be; the supervisor
  # does not guess, and does not build a command out of any of it.
  spec <- job_spec(cmd$task %||% "", cmd$target %||% "",
                   list(filter = cmd$filter %||% "", format = cmd$format %||% ""))
  if (!isTRUE(spec$ok)) {
    audit("job-refused", detail = spec$error)
    job_emit("", event = "refused", error = spec$error)
    return(invisible(NULL))
  }
  sockets$job_seq <- sockets$job_seq + 1L
  id <- paste0("job-", sockets$job_seq)
  started <- tryCatch(
    # The job's own env FIRST, then the job's identity — and the user's
    # settings under both, so a quarto_path set in Settings reaches the render
    # while a job that names its own still wins.
    kernel_start(file.path(here, "job-run.R"), rscript = settings_rscript(),
                 env_extra = c(settings_child_env(), spec$env, CARMAR_JOB_ID = id)),
    error = function(e) conditionMessage(e))
  if (is.character(started)) {
    audit("job-start-failed", detail = started)
    job_emit(id, event = "refused",
             error = paste("the job process could not start:", started))
    return(invisible(NULL))
  }
  job <- new.env(parent = emptyenv())
  job$id <- id
  job$k <- started
  job$task <- spec$task
  job$root <- spec$root
  job$target <- spec$target
  job$output <- NA_character_
  job$title <- job_title(spec)
  job$state <- "running"
  job$ok <- NA
  job$error <- ""
  job$started <- as.numeric(Sys.time())
  job$elapsed <- NA_real_
  job$log <- character(0)
  job$dropped <- 0L
  job$rows <- list()
  job$renderer <- ""
  job$counts <- NULL
  job$truncated <- 0L
  sockets$jobs[[id]] <- job
  audit("job-start", id = id, detail = spec$task)
  # Announced before it runs, in the words of the thing that will happen —
  # the same rule Stage 6's describePlan follows.
  job_emit(id, event = "accepted", task = spec$task, root = spec$root,
           target = spec$target, title = job$title, started = job$started)
  invisible(NULL)
}

#' Stop a running job. Any page may stop any job: a job is the kernel's, not
#' a socket's, so the page that can see it is the page that can stop it.
#' @param id Job id.
#' @return Invisibly NULL.
job_stop <- function(id) {
  job <- sockets$jobs[[id]]
  if (is.null(job) || !identical(job$state, "running")) return(invisible(NULL))
  # No graceful shutdown protocol: job-run.R never reads stdin, so kernel_stop's
  # shutdown frame would go unread and only its kill would land. Kill directly.
  try(job$k$proc$kill(), silent = TRUE)
  job$state <- "stopped"
  job$ok <- FALSE
  job$error <- "stopped"
  job$elapsed <- round(as.numeric(Sys.time()) - job$started, 2)
  audit("job-stop", id = id)
  job_emit(id, event = "done", ok = FALSE, stopped = TRUE,
           error = "stopped", elapsed = job$elapsed)
  invisible(NULL)
}

#' Drop finished jobs beyond the history cap, oldest first.
#' @return Invisibly NULL.
prune_jobs <- function() {
  jobs <- mget(ls(sockets$jobs), envir = sockets$jobs)
  finished <- Filter(function(j) !identical(j$state, "running"), jobs)
  if (length(finished) <= JOB_HISTORY_MAX) return(invisible(NULL))
  order_by <- order(vapply(finished, function(j) j$started, numeric(1)))
  drop <- names(finished)[order_by][seq_len(length(finished) - JOB_HISTORY_MAX)]
  rm(list = drop, envir = sockets$jobs)
  invisible(NULL)
}

#' Is any job still running? The idle linger must not stop a kernel that is
#' three minutes into a check nobody is watching — that is precisely the case
#' a detached job exists for.
#' @return TRUE when at least one job is running.
jobs_running <- function() {
  any(vapply(mget(ls(sockets$jobs), envir = sockets$jobs),
             function(j) identical(j$state, "running"), logical(1)))
}

#' Drain every running job: stream its output, keep it, and close it out.
#' @return Invisibly NULL.
pump_jobs <- function() {
  for (id in ls(sockets$jobs)) {
    job <- sockets$jobs[[id]]
    if (!identical(job$state, "running")) next
    for (e in kernel_poll(job$k, 0L)) {
      if (identical(e$type, "stdout") || identical(e$type, "stderr")) {
        text <- e$text %||% ""
        job_log(job, text)
        job_emit(id, event = "log", stream = e$type, text = text)
        next
      }
      if (!identical(e$type, "job")) next
      event <- e$event %||% ""
      if (identical(event, "started")) job$pid <- e$pid
      if (identical(event, "output")) {
        # Kept so a page that connects after the render still learns where the
        # file went — and so `job_open` has a path of its OWN to trust.
        job$output <- if (scalar_chr(e$path)) e$path else NA_character_
        job$renderer <- e$renderer %||% ""
      }
      if (identical(event, "tests")) {
        # Kept for a page that connects after the run, and forwarded now.
        job$rows <- e$rows
        job$counts <- e$counts
        job$truncated <- e$truncated %||% 0L
      }
      if (identical(event, "done")) {
        job$state <- "done"
        job$ok <- isTRUE(e$ok)
        job$error <- e$error %||% ""
        job$elapsed <- e$elapsed %||% NA_real_
      }
      # The child stamps its own id, so this is byte-faithful: no re-encode,
      # no four-digit rounding, no `{}` where the child wrote null.
      job_broadcast(relay_frame(e))
    }
    if (identical(job$state, "running") && !job$k$proc$is_alive()) {
      # Died without a `done`. Never silent: an empty log and a crashed job
      # look identical on screen and mean opposite things.
      code <- as.integer(tryCatch(job$k$proc$get_exit_status(),
                                  error = function(e) NA_integer_) %||% NA_integer_)
      job$state <- "done"
      job$ok <- FALSE
      job$error <- sprintf("the job process stopped unexpectedly (exit %s)",
                           if (is.na(code)) "unknown" else code)
      job$elapsed <- round(as.numeric(Sys.time()) - job$started, 2)
      audit("job-died", id = id, detail = as.character(code))
      job_emit(id, event = "done", ok = FALSE, error = job$error,
               elapsed = job$elapsed)
    }
  }
  prune_jobs()
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

# ── signed product updates ─────────────────────────────────────────────────
#
# The updater is a product-owned script injected by the launcher. The browser
# never supplies a path or a command line: it may name one action from this
# fixed vocabulary, and the supervisor starts that exact script directly.
# Checks run out-of-process because a slow proxy/download must not freeze the
# R worker, WebSocket heartbeats, recovery, or document saves.
update_job <- new.env(parent = emptyenv())
update_job$proc <- NULL
update_job$action <- ""
update_job$started <- as.POSIXct(NA)

update_runner_spec <- function(script, args = character(),
                               sysname = unname(Sys.info()[["sysname"]]),
                               which = Sys.which) {
  if (identical(sysname, "Windows")) {
    candidates <- unname(which(c("powershell.exe", "powershell")))
    command <- candidates[nzchar(candidates)][1L]
    if (!length(command) || is.na(command) || !grepl("[.]ps1$", script, ignore.case = TRUE)) {
      return(NULL)
    }
    return(list(command = command,
                args = c("-NoLogo", "-NoProfile", "-NonInteractive",
                         "-ExecutionPolicy", "Bypass", "-File", script, args)))
  }
  if (!file.exists("/bin/sh")) return(NULL)
  list(command = "/bin/sh", args = c(script, args))
}

update_script_path <- function() {
  script <- trimws(Sys.getenv("CARMAR_UPDATE_SCRIPT", ""))
  if (!isTRUE(deployment$loopback) || !nzchar(script) || !file.exists(script)) return("")
  script <- normalizePath(script, winslash = "/", mustWork = TRUE)
  if (is.null(update_runner_spec(script))) return("")
  script
}

update_text <- function(x, max = 240L) {
  if (!length(x) || is.na(x[[1L]])) return("")
  value <- enc2utf8(as.character(x[[1L]]))
  value <- gsub("[[:cntrl:]]", " ", value)
  trimws(substr(value, 1L, max))
}

update_version <- function(x) {
  value <- update_text(x, 64L)
  if (grepl("^[A-Za-z0-9][A-Za-z0-9._+-]{0,63}$", value)) value else ""
}

update_job_busy <- function() {
  proc <- update_job$proc
  if (is.null(proc)) return(FALSE)
  alive <- isTRUE(tryCatch(proc$is_alive(), error = function(e) FALSE))
  if (alive && is.finite(as.numeric(update_job$started)) &&
      as.numeric(difftime(Sys.time(), update_job$started, units = "secs")) > 360) {
    try(proc$kill(), silent = TRUE)
    alive <- FALSE
    audit("update-timeout", detail = update_job$action)
  }
  if (!alive) {
    update_job$proc <- NULL
    update_job$action <- ""
  }
  alive
}

#' Read the updater's deliberately small status record.
#'
#' The fourth TSV field is an installer PATH. It is intentionally discarded:
#' the settings page needs state and versions, not a map of the user's disk.
update_status_state <- function() {
  script <- update_script_path()
  if (!nzchar(script)) return(list(
    supported = FALSE, state = "unavailable", current = CARMAR_KERNEL_BUILD,
    offered = "", previous = "", busy = FALSE, action = "",
    can_check = FALSE, can_update = FALSE, can_defer = FALSE,
    can_rollback = FALSE,
    message = "Signed desktop updates are not available in this deployment."
  ))

  busy <- update_job_busy()
  if (busy) return(list(
    supported = TRUE, state = "checking", current = CARMAR_KERNEL_BUILD,
    offered = "", previous = "", busy = TRUE, action = update_job$action,
    can_check = FALSE, can_update = FALSE, can_defer = FALSE,
    can_rollback = FALSE,
    message = switch(update_job$action,
      check = "Checking and verifying the signed update…",
      install = "Opening the verified installer…",
      rollback = "Opening the last-known-good installer…",
      defer = "Saving the defer choice…", "Working…")
  ))

  spec <- update_runner_spec(script, "status")
  ran <- tryCatch(processx::run(
    spec$command, spec$args, timeout = 5000,
    error_on_status = FALSE, echo = FALSE
  ), error = function(e) NULL)
  line <- if (!is.null(ran) && identical(ran$status, 0L)) {
    strsplit(ran$stdout %||% "", "\n", fixed = TRUE)[[1L]][1L]
  } else ""
  fields <- if (nzchar(line)) strsplit(line, "\t", fixed = TRUE)[[1L]] else character()
  at <- function(i) if (length(fields) >= i) fields[[i]] else ""
  state <- update_text(at(1L), 32L)
  allowed <- c("unknown", "current", "available", "deferred", "installing",
               "rolling-back", "error", "offline", "refused", "unconfigured")
  if (!(state %in% allowed)) state <- "error"
  current <- update_version(at(2L))
  offered <- update_version(at(3L))
  previous <- update_version(at(5L))
  message <- update_text(at(6L))
  if (!nzchar(message)) message <- if (identical(state, "error"))
    "The signed updater did not return a valid status." else "Update status is available."
  list(
    supported = TRUE, state = state,
    current = current %||% "", offered = offered, previous = previous,
    busy = FALSE, action = "", can_check = !identical(state, "unconfigured"),
    can_update = identical(state, "available") && nzchar(offered),
    can_defer = state %in% c("current", "available", "deferred", "offline"),
    can_rollback = !identical(state, "unconfigured") && nzchar(previous), message = message
  )
}

update_start_action <- function(action, days = NULL) {
  script <- update_script_path()
  if (!nzchar(script)) return(list(ok = FALSE, reason = "unavailable",
    error = "Signed desktop updates are not available in this deployment."))
  if (!(action %in% c("check", "defer", "install", "rollback"))) {
    return(list(ok = FALSE, reason = "action", error = "No such update action."))
  }
  if (update_job_busy()) return(list(ok = FALSE, reason = "busy",
    error = "Another update action is still running."))

  status <- update_status_state()
  if (identical(action, "check") && !isTRUE(status$can_check)) {
    return(list(ok = FALSE, reason = "state", error = status$message))
  }
  if (identical(action, "defer") && !isTRUE(status$can_defer)) {
    return(list(ok = FALSE, reason = "state", error = "Update checks cannot be deferred in the current state."))
  }
  if (identical(action, "install") && !isTRUE(status$can_update)) {
    return(list(ok = FALSE, reason = "state", error = "No verified update is ready."))
  }
  if (identical(action, "rollback") && !isTRUE(status$can_rollback)) {
    return(list(ok = FALSE, reason = "state", error = "No last-known-good version is available."))
  }
  if (identical(action, "defer")) {
    days <- suppressWarnings(as.integer(days))
    if (length(days) != 1L || is.na(days) || days < 1L || days > 30L) {
      return(list(ok = FALSE, reason = "days", error = "Defer days must be from 1 to 30."))
    }
  }

  args <- c(action, if (identical(action, "defer")) as.character(days))
  spec <- update_runner_spec(script, args)
  proc <- tryCatch(processx::process$new(
    spec$command, spec$args, env = Sys.getenv(),
    stdout = if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
    stderr = "2>&1", cleanup = TRUE
  ), error = function(e) NULL)
  if (is.null(proc)) return(list(ok = FALSE, reason = "start",
    error = "The signed updater could not be started."))
  update_job$proc <- proc
  update_job$action <- action
  update_job$started <- Sys.time()
  audit("update-action", detail = action)
  list(ok = TRUE, accepted = TRUE, action = action)
}

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

#' Has this route's page gone away?
#'
#' A socket record is removed from `sockets$open` the moment its close handler
#' fires, so "still open" is identity in that list — not a flag on the record,
#' which would need clearing in every path that can lose a page.
route_is_orphan <- function(route) {
  if (is.null(route)) return(TRUE)
  # A supervisor-owned route (route_internal) has no page to lose.
  if (is.function(route$on_reply)) return(FALSE)
  if (is.null(route$rec)) return(TRUE)
  !any(vapply(sockets$open, function(r) identical(r, route$rec), logical(1)))
}

#' Keep one output frame for a run, bounded.
#'
#' Called for EVERY exec frame, attached page or not — see the note at the call
#' site. The cap protects against a runaway `print()` loop, and the overflow is
#' dropped from the END: the first lines of a long run are the ones that say
#' what it was doing, so losing the tail is the survivable half.
record_output <- function(route, frame) {
  held <- route$buffered_bytes %||% 0L
  # Measure the FRAME, not its text: a plot arrives as base64 and a view as a
  # table, and neither has a `text` field to charge for.
  size <- nchar(attr(frame, "raw") %||% (frame$text %||% ""), type = "bytes")
  if (held + size > ORPHAN_BUFFER_BYTES) {
    route$buffer_truncated <- TRUE
    return(invisible(NULL))
  }
  # `buf[[n]] <- frame`, NOT `c(buf, list(frame))`. The second re-allocates and
  # copies the whole list on every frame, which is O(n²) over a run: measured
  # at 0.35 s of supervisor CPU for 12,000 lines against 0.00 s here, 117x.
  # That cost was invisible while this only ran for ORPHANED runs; recording
  # every run put it on the hot path of the loop that services the socket, so
  # a chatty `for` loop would have stalled the whole session. R over-allocates
  # on `[[<-`, so the indexed form is amortised constant.
  n <- (route$buffer_n %||% 0L) + 1L
  if (n == 1L) route$buffer <- list()
  route$buffer[[n]] <- frame
  route$buffer_n <- n
  route$buffered_bytes <- held + size
  invisible(NULL)
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
      deliver_route_frame(route, frame)
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

# ── restart into the installed build ───────────────────────────────────────
#
# An upgrade swaps the bundle IN PLACE, so the kernel-version stamp beside
# this script names the installed build while CARMAR_KERNEL_BUILD names the
# one running. The two disagreeing is the one honest signal that a session
# is stale, and this is the one door through it: save the workspace, start a
# SUCCESSOR supervisor from the installed files on this same port, tell every
# page where its successor's notebook is, and exit. The successor restores
# the workspace into its first worker (worker.R, CARMAR_RESTORE_WORKSPACE),
# and the page — which is HTML the browser already holds — navigates to the
# installed build's page once /health answers with the new build.
#
# Three refusals matter. A running chunk: `save.image()` would queue behind
# it and the person would be staring at a spinner with no chunk to blame. A
# missing page for the installed build: a successor with nothing to serve is
# worse than a stale session. And a second request while one is under way.
sockets$handoff <- NULL

handoff_refusal <- function() {
  installed <- installed_build()
  if (identical(installed, "unknown")) {
    return("This kernel cannot tell which CarmaR is installed.")
  }
  if (identical(installed, CARMAR_KERNEL_BUILD)) {
    return(sprintf("This session already runs the installed CarmaR %s.", installed))
  }
  if (!nzchar(carmar_notebook_page(here, installed))) {
    return(sprintf("The installed CarmaR %s has no notebook page beside this kernel.", installed))
  }
  if (!is.null(sockets$handoff)) return("A restart into the installed CarmaR is already under way.")
  if (length(ls(sockets$running)) > 0L || !is.null(sockets$worker_active)) {
    return("R is busy — wait for the running chunk, or interrupt it, then try again.")
  }
  if (isTRUE(sockets$debug_paused) || isTRUE(sockets$input_waiting)) {
    return("R is waiting at a prompt — finish it first.")
  }
  NULL
}

#' The successor's page URL: the installed build's notebook beside this
#' kernel, with this port and a capability minted here so it can be told
#' to the page NOW and honoured by the successor later.
handoff_page_url <- function(installed, cap) {
  page_file <- normalizePath(carmar_notebook_page(here, installed))
  paste0("file://", utils::URLencode(page_file, reserved = FALSE),
         "#kernel=", port, "&pair=", cap)
}

session_handoff_begin <- function() {
  why <- handoff_refusal()
  if (!is.null(why)) {
    audit("session-upgrade-refused", reason = why)
    return(list(ok = FALSE, error = why))
  }
  installed <- installed_build()
  cap <- secure_token(32L)
  page <- handoff_page_url(installed, cap)
  sockets$handoff <- list(to = installed, cap = cap, page = page, started = Sys.time())
  audit("session-upgrade", from = CARMAR_KERNEL_BUILD, to = installed)
  # Every page learns where its successor's notebook is — over the gated
  # socket only; the capability never appears in /health.
  told <- toJSON(list(type = "session-upgrade", from = CARMAR_KERNEL_BUILD,
                      to = installed, page = page), auto_unbox = TRUE)
  lapply(page_recs(), function(r) try(r$ws$send(told), silent = TRUE))
  routed <- route_internal(list(type = "workspace_save"), session_handoff_finish)
  enqueue_worker_command(routed$cmd, routed$wire_id)
  list(ok = TRUE, from = CARMAR_KERNEL_BUILD, to = installed, page = page)
}

handoff_abandon <- function(reason) {
  audit("session-upgrade-failed", reason = reason)
  sockets$handoff <- NULL
  told <- toJSON(list(type = "session-upgrade-failed", error = reason), auto_unbox = TRUE)
  lapply(page_recs(), function(r) try(r$ws$send(told), silent = TRUE))
  invisible(NULL)
}

#' The worker answered workspace_save: start the successor and go.
session_handoff_finish <- function(frame) {
  h <- sockets$handoff
  if (is.null(h)) return(invisible(NULL))
  if (!isTRUE(frame$ok) || !scalar_chr(frame$file)) {
    return(handoff_abandon(frame$error %||% "R could not save the workspace."))
  }
  state <- dirname(session_file)
  dir.create(state, recursive = TRUE, showWarnings = FALSE)
  env <- Sys.getenv()
  env[["CARMAR_PORT"]] <- as.character(port)
  env[["CARMAR_WAIT_PORT"]] <- "30"
  env[["CARMAR_RESTORE_WORKSPACE"]] <- frame$file
  env[["CARMAR_FILE_LAUNCH_CAP"]] <- h$cap
  env[["CARMAR_HANDOFF_FROM"]] <- CARMAR_KERNEL_BUILD
  env[["CARMAR_SESSION_TITLE"]] <- runtime_record$title %||% ""
  if (isTRUE(runtime_record$listen)) env[["CARMAR_LISTEN"]] <- "1"
  # The same R this supervisor runs on, and the serve.R beside THIS file —
  # which, after the in-place swap, is the installed build's.
  rscript <- file.path(R.home("bin"), "Rscript")
  log <- file.path(state, sprintf("kernel-handoff-%d.log", port))
  started <- tryCatch(processx::process$new(
    rscript, file.path(here, "serve.R"), env = env,
    stdout = log, stderr = "2>&1", cleanup = FALSE), error = function(e) NULL)
  if (is.null(started) || !started$is_alive()) {
    return(handoff_abandon("The installed CarmaR could not be started."))
  }
  audit("session-upgrade-spawned", to = h$to, pid = started$get_pid())
  sockets$quit <- TRUE
  invisible(NULL)
}

#' /health's word about a handoff: which build this kernel replaced, for the
#' first 90 s of its life. A page that asked can recognise its successor by
#' it; nothing else is disclosed (no page, no capability).
handoff_health <- function() {
  if (!nzchar(HANDOFF_FROM)) return(list())
  age <- as.numeric(difftime(Sys.time(), sockets$boot_at, units = "secs"))
  if (age > 90) return(list())
  list(handoff_from = HANDOFF_FROM)
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
  started <- tryCatch(kernel_start(file.path(here, "analyze.R"),
                                   rscript = settings_rscript(),
                                   env_extra = settings_child_env()),
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
    else if (identical(cmd$type, "analyze_workspace_references")) "workspace_references"
    else "analyze"
  sockets$analyze_routes[[wire_id]] <- route
  cmd$id <- wire_id
  # The wire name is the analyzer's vocabulary, not the browser's.
  if (identical(cmd$type, "analyze_ping")) cmd$type <- "ping"
  if (identical(cmd$type, "analyze_workspace")) cmd$type <- "workspace"
  if (identical(cmd$type, "analyze_workspace_references")) cmd$type <- "workspace_references"
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
    deliver_route_frame(route, frame)
    # A failed route is a finished route. The restart path clears everything
    # itself two lines later, but any future caller that forgot would leave
    # routes answered AND still registered — the next reply with a recycled
    # wire id would then reach the wrong socket.
    drop_worker_route(wire_id)
  }
  invisible(NULL)
}

#' Recover the execution worker without taking the notebook server down.
#'
#' The page, its unsaved buffers, WebSocket and analysis worker all belong to
#' the supervisor. Only R's in-memory session is lost. A dead worker therefore
#' starts a bounded-backoff respawn loop instead of ending serve.R and turning a
#' recoverable calculation crash into a dead application.
#' A frame built by the SUPERVISOR for a route (an error, a refusal): to the
#' page that asked, or to the supervisor's own continuation.
deliver_route_frame <- function(route, frame) {
  if (is.function(route$on_reply)) {
    try(route$on_reply(frame), silent = TRUE)
  } else {
    try(route$rec$ws$send(toJSON(frame, auto_unbox = TRUE)), silent = TRUE)
  }
  invisible(NULL)
}

#' A worker command the SUPERVISOR asks on its own behalf — no page owns it,
#' and its reply goes to `on_reply(frame)` instead of a socket. It rides the
#' ordinary queue, so it waits its turn behind a running chunk like anything
#' a page asks, and it dies with the worker like anything a page asks.
route_internal <- function(cmd, on_reply) {
  stopifnot("`on_reply` must be a function" = is.function(on_reply))
  sockets$route_seq <- sockets$route_seq + 1L
  wire_id <- paste0("wire-", sockets$route_seq)
  route <- new.env(parent = emptyenv())
  route$rec <- NULL
  route$client_id <- wire_id
  route$kind <- "request"
  route$response_type <- cmd$type
  route$on_reply <- on_reply
  sockets$worker_routes[[wire_id]] <- route
  cmd$id <- wire_id
  list(cmd = cmd, wire_id = wire_id)
}

recover_execution_worker <- function() {
  if (!isTRUE(sockets$worker_recovering)) {
    sockets$worker_recovering <- TRUE
    sockets$worker_restart_attempts <- 0L
    sockets$worker_restart_after <- 0
    sockets$hello <- NULL
    fail_worker_routes(if (isTRUE(sockets$force_stop_asked))
      "R was force-stopped. Its variables and loaded packages are gone; CarmaR is starting a fresh R session."
      else "The R worker stopped unexpectedly. Its session state was lost; CarmaR is starting a fresh R session.")
    rm(list = ls(sockets$running), envir = sockets$running)
    rm(list = ls(sockets$worker_routes), envir = sockets$worker_routes)
    sockets$worker_queue <- list()
    sockets$worker_active <- NULL
    sockets$worker_terminal <- NULL
    sockets$debug_paused <- FALSE
    sockets$input_waiting <- FALSE
    # `forced`: the page must not call a death it asked for "unexpected".
    payload <- toJSON(list(type = "worker-died", recovering = TRUE,
                           forced = isTRUE(sockets$force_stop_asked)), auto_unbox = TRUE)
    lapply(sockets$open, function(r) try(r$ws$send(payload), silent = TRUE))
    audit("worker-died", recovering = TRUE, forced = isTRUE(sockets$force_stop_asked))
    sockets$force_stop_asked <- FALSE
  }
  now <- as.numeric(Sys.time())
  if (now < sockets$worker_restart_after) return(invisible(FALSE))
  sockets$worker_restart_attempts <- sockets$worker_restart_attempts + 1L
  attempt <- sockets$worker_restart_attempts
  delay <- c(0.25, 0.5, 1, 2, 5, 10)[min(attempt, 6L)]
  sockets$worker_restart_after <- now + delay
  started <- tryCatch(start_execution_worker(), error = function(e) {
    audit("worker-respawn-failed", attempt = attempt, detail = conditionMessage(e))
    NULL
  })
  if (is.null(started)) return(invisible(FALSE))
  k <<- started
  audit("worker-respawn", attempt = attempt)
  payload <- toJSON(list(type = "worker-restarting", attempt = attempt), auto_unbox = TRUE)
  lapply(sockets$open, function(r) try(r$ws$send(payload), silent = TRUE))
  invisible(TRUE)
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
    # The heartbeat answer carries the installed build, so an open notebook
    # learns of an upgrade within one beat instead of at its next reconnect.
    try(rec$ws$send(toJSON(list(type = "hb", installed_build = installed_build()),
                           auto_unbox = TRUE)), silent = TRUE)
    return(invisible(NULL))
  }

  # The page names its document. Display metadata only — it flows to the
  # runtime record so the menu helper can label the session by what the user
  # called it, never into the worker. Pages only: a declared agent renaming
  # the user's session would be attribution theft. Sending it (even empty)
  # also proves a real page is attached, which retires the "listen" mark a
  # headless start left in the record.
  if (identical(cmd$type, "page-title")) {
    if (identical(rec$role, "page")) {
      clear_runtime_listen()
      if (scalar_chr(cmd$title) || is.null(cmd$title)) {
        set_runtime_title(cmd$title %||% "")
      }
    }
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
  if (identical(cmd$type, "open-request")) {
    # The double-clicked document parked by the launcher (CARMAR_OPEN_FILE).
    # Pages only — an agent must never consume it — and consume-once: the
    # first page to ask gets the path, everyone after gets "". An empty
    # string, not a JSON null, because jsonlite renders NULL as a truthy {}.
    if (!identical(rec$role, "page")) return(invisible(NULL))
    path <- sockets$pending_open
    sockets$pending_open <- NULL
    if (!is.null(path)) audit("open-file-delivered")
    reply <- toJSON(list(type = "open-request",
                         id = if (scalar_chr(cmd$id)) cmd$id else "open",
                         path = path %||% ""),
                    auto_unbox = TRUE)
    try(rec$ws$send(reply), silent = TRUE)
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
  # ── development jobs ──────────────────────────────────────────────────────
  # Pages only (see AGENT_REFUSED above). The wire carries a task NAME and a
  # folder; spike/jobs.R decides whether that pair may run and the supervisor
  # builds the argument vector. Nothing here is pasted into an expression.
  # ── sessions and R installations (Session ▾) ──────────────────────────────
  # Page-only: a list of the user's running kernels (ports, titles) and of the
  # R installations on the machine is a map of the desk, not something a
  # native client or an agent needs the kernel to draw for it.
  if (cmd$type %in% c("session_list", "r_versions") && identical(rec$role, "page")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(...) try(rec$ws$send(toJSON(c(list(type = cmd$type, id = cmd$id), list(...)),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit(paste0(cmd$type, "-refused"), class = rec$class %||% "unknown")
      reply(error = "Only the local notebook page may ask this.")
      return(invisible(NULL))
    }
    if (identical(cmd$type, "session_list")) reply(sessions = I(session_list()))
    else reply(versions = I(r_versions()), current = current_rscript())
    return(invisible(NULL))
  }
  # "Restart into the installed CarmaR": the page's door. Page-only in both
  # senses (AGENT_REFUSED has it too) — a session handoff is the person's
  # decision about their own session, never an agent's.
  if (identical(cmd$type, "session_upgrade") && identical(rec$role, "page")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(x) try(rec$ws$send(toJSON(c(list(type = "session_upgrade", id = cmd$id), x),
                                                auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("session-upgrade-refused", class = rec$class %||% "unknown")
      reply(list(ok = FALSE, error = "Only the local notebook page may ask this."))
      return(invisible(NULL))
    }
    reply(session_handoff_begin())
    return(invisible(NULL))
  }
  # ── terminals ─────────────────────────────────────────────────────────────
  # Page-only in BOTH senses (see PAGE_ONLY_CLASSES): a declared agent is
  # refused above, and a client that merely never declared is refused here.
  # A declared agent falls THROUGH to the AGENT_REFUSED reply below, so it is
  # told why in the op's own name rather than met with silence.
  if (cmd$type %in% c("term_open", "term_input", "term_close") && identical(rec$role, "page")) {
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("term-refused", reason = "class", class = rec$class %||% "unknown")
      term_send(rec, cmd$id %||% "", event = "refused",
                error = "Only the local notebook page may open a terminal.")
      return(invisible(NULL))
    }
    if (identical(cmd$type, "term_open")) term_open(rec, cmd)
    else if (identical(cmd$type, "term_input")) term_input(rec, cmd)
    else if (!is.null(term_owned(rec, cmd$id))) term_close(cmd$id)
    return(invisible(NULL))
  }
  if (identical(cmd$type, "job_start")) {
    if (!identical(rec$role, "page")) return(invisible(NULL))
    job_start(rec, cmd)
    return(invisible(NULL))
  }
  if (identical(cmd$type, "job_stop")) {
    if (!identical(rec$role, "page")) return(invisible(NULL))
    if (!scalar_chr(cmd$job)) return(invisible(NULL))
    job_stop(cmd$job)
    return(invisible(NULL))
  }
  # Reattachment: everything a page needs to render the pane it just opened,
  # including the buffered log of a job that started before this page existed.
  # This is what makes a job detached rather than merely asynchronous.
  if (identical(cmd$type, "job_list")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    want_log <- scalar_chr(cmd$job)
    jobs <- mget(ls(sockets$jobs), envir = sockets$jobs)
    ordered <- jobs[order(vapply(jobs, function(j) j$started, numeric(1)))]
    payload <- lapply(ordered, function(j)
      job_snapshot(j, with_log = want_log && identical(j$id, cmd$job)))
    try(rec$ws$send(toJSON(list(type = "job_list", id = cmd$id,
                                jobs = I(unname(payload))),
                           auto_unbox = TRUE, null = "null")), silent = TRUE)
    return(invisible(NULL))
  }
  # "What can I do with what I am looking at?" — the question the UI asks
  # before it offers any button at all, and it is ONE question with two
  # halves: the file may be inside a package, and the file may itself be
  # renderable. It was called `job_root` while it could only answer the first;
  # a reply carrying `document` under that name would have been a lie.
  # Read-only, and answered without starting anything.
  if (identical(cmd$type, "job_context")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    path <- cmd$path %||% ""
    root <- find_package_root(path)
    renderable <- scalar_chr(path) && nzchar(path) && file.exists(path) &&
      !dir.exists(path) && path_extension(path) %in% RENDER_EXTENSIONS
    # Report the path the JOB will act on, not the one the page happened to
    # send. job_spec() normalizes, so echoing the raw string here would show
    # the reader one path and use another — and on macOS those differ by a
    # /private prefix on everything under /var.
    if (renderable) {
      path <- tryCatch(normalizePath(path, mustWork = TRUE), error = function(e) path)
    }
    try(rec$ws$send(toJSON(list(type = "job_context", id = cmd$id,
                                root = root, package = package_name(root),
                                document = if (renderable) path else "",
                                formats = I(RENDER_FORMATS),
                                tasks = I(names(JOB_TASKS))),
                           auto_unbox = TRUE, null = "null")), silent = TRUE)
    return(invisible(NULL))
  }
  # Open what a render produced.
  #
  # The wire names a JOB, never a path, and the supervisor opens the path IT
  # recorded from the child's own `output` frame. That is the whole security
  # content of this op: `utils::browseURL` on a wire-supplied string would be
  # "open any file on this machine" wearing a helpful label, and there is no
  # reason to accept a path here when the kernel already knows the answer.
  if (identical(cmd$type, "job_open")) {
    if (!identical(rec$role, "page") || !scalar_chr(cmd$job)) return(invisible(NULL))
    job <- sockets$jobs[[cmd$job]]
    if (is.null(job) || !scalar_chr(job$output) || !file.exists(job$output)) {
      return(invisible(NULL))
    }
    audit("job-open", id = cmd$job)
    try(utils::browseURL(job$output), silent = TRUE)
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

  # ── "is the run I am still waiting for actually alive?" ───────────────────
  #
  # A page waits for a `done` with no timeout, on purpose: a model fit may run
  # for hours. The cost is that a done LOST on the way out — a send that threw
  # on a socket in a bad state, a frame the page missed — leaves the chunk
  # showing "Running…" for the rest of the session while R sits idle. Nothing
  # on screen is wrong except everything, and only a reload clears it.
  #
  # The supervisor already knows the answer: a live exec has a route, and the
  # route is dropped the moment its terminal frame is relayed. So this is a
  # question it can answer ITSELF — no frame is written to the worker, which
  # matters, because a worker parked at a `browser()` prompt or inside
  # `readline()` would read that frame as console input.
  #
  # Scoped to the ASKING socket: exec ids are per-page counters (`c1`, `c2`),
  # so two tabs collide on every one of them, and a neighbour's live run must
  # never be the reason this page keeps waiting.
  if (identical(cmd$type, "runstate")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    run_id <- if (scalar_chr(cmd$run)) cmd$run else ""
    alive <- nzchar(run_id) && any(vapply(ls(sockets$worker_routes), function(w) {
      route <- sockets$worker_routes[[w]]
      !is.null(route) && identical(route$kind, "exec") &&
        identical(route$client_id, run_id) && identical(route$rec, rec)
    }, logical(1)))
    # A terminal frame waiting out the drain cycle is still in flight: its
    # route is gone, but the result is one turn away. Answering "not running"
    # here would race the real answer and settle the chunk as lost.
    if (!alive && !is.null(sockets$worker_terminal)) {
      held <- sockets$worker_terminal$route
      alive <- !is.null(held) && identical(held$client_id, run_id) &&
        identical(held$rec, rec)
    }
    try(rec$ws$send(toJSON(list(type = "runstate", id = cmd$id, run = run_id,
                                running = alive), auto_unbox = TRUE)), silent = TRUE)
    return(invisible(NULL))
  }

  # ── what is still running, and adopting it ────────────────────────────────
  #
  # R outlives the page. Closing a tab, reloading it, or following a link does
  # not stop a fit — it strands it: the run keeps burning CPU and its result is
  # relayed to a socket that is gone. Both halves of that are fixed here.
  # `runs` lets a page that has just connected SEE what its session is doing,
  # and `adopt` re-points an orphaned run at the page asking for it, so the
  # result lands in the chunk that asked for it however long ago.
  #
  # Only an ORPHANED run may be adopted. A live page's run is not up for
  # grabs by a second tab — that would silently move someone's output.
  if (identical(cmd$type, "runs")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    rows <- Filter(Negate(is.null), lapply(ls(sockets$worker_routes), function(w) {
      route <- sockets$worker_routes[[w]]
      if (is.null(route) || !identical(route$kind, "exec")) return(NULL)
      list(run = route$client_id %||% "", srcname = route$srcname %||% "",
           mine = identical(route$rec, rec), orphan = route_is_orphan(route),
           finished = !is.null(route$parked),
           seconds = round(as.numeric(difftime(Sys.time(),
             route$started %||% Sys.time(), units = "secs")), 1))
    }))
    try(rec$ws$send(toJSON(list(type = "runs", id = cmd$id,
                                runs = if (length(rows)) rows else I(list())),
                           auto_unbox = TRUE)), silent = TRUE)
    return(invisible(NULL))
  }

  if (identical(cmd$type, "adopt")) {
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$srcname) || !nzchar(cmd$srcname)) {
      return(invisible(NULL))
    }
    refuse <- function(msg) {
      try(rec$ws$send(toJSON(list(type = "done", id = cmd$id, status = "lost",
                                  message = msg), auto_unbox = TRUE)), silent = TRUE)
      invisible(NULL)
    }
    hit <- NULL
    for (w in ls(sockets$worker_routes)) {
      route <- sockets$worker_routes[[w]]
      if (!is.null(route) && identical(route$kind, "exec") &&
          identical(route$srcname %||% "", cmd$srcname) && route_is_orphan(route)) {
        hit <- w
        break
      }
    }
    if (is.null(hit)) return(refuse("that run is no longer in this session."))
    route <- sockets$worker_routes[[hit]]
    route$rec <- rec
    route$client_id <- cmd$id
    audit("adopt", srcname = cmd$srcname)
    # It may already be FINISHED: the result was parked when its page went
    # away, and adopting is how it finally gets delivered.
    # Everything it printed while nobody was attached, in order, BEFORE the
    # result — the adopting page then sees exactly the sequence a run nobody
    # interrupted produces, and its accumulator fills the same way.
    buffered <- route$buffer %||% list()
    route$buffer <- NULL
    route$buffer_n <- 0L
    route$buffered_bytes <- 0L
    lapply(buffered, function(frame) {
      # Plain stdout/stderr carries no protocol id, so relay_frame's needle
      # ("id":<wire id> in the worker's own bytes) cannot match and the frame
      # falls through to an encode of the parsed list — whose `id` the pump
      # had already rewritten to the client id of the page that is GONE. The
      # adopting client drops a frame addressed to someone else, which is how
      # a rejoined run arrived with a status and an empty output panel. Re-
      # address it here; frames that do carry an id take the needle path and
      # are rewritten there.
      frame$id <- cmd$id
      try(rec$ws$send(relay_frame(frame, hit, cmd$id)), silent = TRUE)
    })
    # A gap in a transcript that does not admit to being a gap is worse than
    # no transcript, so the truncation says so in the output itself.
    if (isTRUE(route$buffer_truncated)) {
      route$buffer_truncated <- NULL
      try(rec$ws$send(toJSON(list(type = "message", id = cmd$id, kind = "warning",
        text = paste("Output printed while this notebook was closed exceeded",
                     ORPHAN_BUFFER_BYTES %/% 1024L, "KB; the rest was dropped.")),
        auto_unbox = TRUE)), silent = TRUE)
    }
    parked <- route$parked
    if (!is.null(parked)) {
      route$parked <- NULL
      try(rec$ws$send(relay_frame(parked, hit, cmd$id)), silent = TRUE)
      drop_worker_route(hit)
    }
    return(invisible(NULL))
  }

  # WHAT A DECLARED AGENT MAY NEVER DO — one list, so an auditor reading it
  # gets the whole answer. `ai-key` belongs here and not in its own handler
  # below: a second, differently-shaped refusal would make this set look
  # complete when it was not.
  if (cmd$type %in% AGENT_REFUSED && identical(rec$role, "mcp")) {
    audit("mcp-refused", reason = paste("agent asked for", cmd$type))
    if (scalar_chr(cmd$id)) {
      why <- switch(cmd$type,
        "ai-key" = "Agents cannot read or write the stored key.",
        "console_history" = "Agents cannot read the console history.",
        "input_reply" = "Agents cannot answer a prompt on the user's behalf.",
        "ai-audit-read" = "Agents cannot read the audit stream.",
        "job_start" = "Agents cannot start development jobs; ask the user to run one.",
        "job_stop" = "Agents cannot stop development jobs.",
        "term_open" = "Agents cannot open a terminal; run code through chunk_run.",
        "term_input" = "Agents cannot type into the user's terminal.",
        "term_close" = "Agents cannot close the user's terminal.",
        "session_list" = "Agents cannot list the user's sessions.",
        "r_versions" = "Agents cannot list or choose R installations.",
        "session_upgrade" = "Agents cannot restart the user's session into another CarmaR.",
        # settings_GET is refused as firmly as the writes. Not because the
        # values are secret, but because handing back the confinement root,
        # the audit-log path and whether AI-text logging is on is a MAP OF THE
        # DEPLOYMENT'S CONTROLS, given to a process whose entire premise is
        # that it is not the user.
        "settings_get" = "Agents cannot read CarmaR's settings.",
        "settings_set" = "Agents cannot change CarmaR's settings.",
        "settings_reset" = "Agents cannot reset CarmaR's settings.",
        "update_status" = "Agents cannot inspect desktop update state.",
        "update_action" = "Agents cannot install, defer, or roll back CarmaR.",
        "project_action" = "Agents cannot install or restore project packages.",
        "job_open" = "Agents cannot open files on the user's desktop.",
        "Agents run code through notebook chunks (chunk_run), not raw exec.")
      reply <- toJSON(list(type = cmd$type, id = cmd$id, error = why), auto_unbox = TRUE)
      try(rec$ws$send(reply), silent = TRUE)
    }
    return(invisible(NULL))
  }

  # Signed desktop updates are supervisor operations, never worker commands.
  # Local notebook pages only: a remote/shared kernel is updated by its fleet
  # operator, and a native or agent socket does not get a UI gesture by
  # pretending to be the person looking at this settings pane.
  if (cmd$type %in% c("update_status", "update_action")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(payload) try(rec$ws$send(toJSON(c(
      list(type = cmd$type, id = cmd$id), payload),
      auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("update-refused", reason = "class", class = rec$class %||% "unknown")
      reply(list(ok = FALSE, reason = "class",
                 error = "Only the local notebook page may manage desktop updates."))
      return(invisible(NULL))
    }
    if (!isTRUE(deployment$loopback)) {
      audit("update-refused", reason = "posture", class = rec$class %||% "unknown")
      reply(list(ok = FALSE, reason = "posture",
                 error = "This deployment is updated by its administrator."))
      return(invisible(NULL))
    }
    if (identical(cmd$type, "update_status")) {
      reply(update_status_state())
    } else {
      action <- if (scalar_chr(cmd$action)) cmd$action else ""
      reply(update_start_action(action, cmd$days %||% NULL))
    }
    return(invisible(NULL))
  }

  if (identical(cmd$type, "project_action") && !isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
    if (scalar_chr(cmd$id)) try(rec$ws$send(toJSON(list(
      type = "project_action", id = cmd$id, ok = FALSE, reason = "class",
      error = "Only the local notebook page may change the project environment."
    ), auto_unbox = TRUE)), silent = TRUE)
    audit("project-action-refused", reason = "class", class = rec$class %||% "unknown")
    return(invisible(NULL))
  }

  if (identical(cmd$type, "exec")) {
    if (!scalar_chr(cmd$id) || !scalar_chr(cmd$source)) return(invisible(NULL))
    audit("exec", id = cmd$id, bytes = nchar(cmd$source, type = "bytes"))
    routed <- route_command(cmd, rec, "exec")
    # The chunk's stable identity travels with the route. Exec ids are per-page
    # counters and die with the page; `chunk:<stableId>` is the same string the
    # NEXT page will use for the same chunk, which is what makes a run
    # adoptable after a reload.
    sockets$worker_routes[[routed$wire_id]]$srcname <-
      if (scalar_chr(cmd$srcname)) cmd$srcname else ""
    sockets$worker_routes[[routed$wire_id]]$started <- Sys.time()
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
        # The ladder: one SIGINT is not always heard — R inside system() has
        # it ignored until the child dies — so the same run is signalled
        # again every 400 ms while it is still the active one, up to four
        # times. The page offers Force Stop on its own clock; this is the
        # supervisor doing what a person hammering ^C does, without the person.
        sockets$interrupt_ladder <- list(wire_id = wire_id, at = as.numeric(Sys.time()), count = 1L,
                                         step = INTERRUPT_LADDER_FIRST)
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
  # ── the debugger's raw console ────────────────────────────────────────────
  # While the worker is paused at a Browse prompt, step commands and console
  # expressions are RAW lines on the worker's stdin, not NDJSON — R's own
  # debugger REPL is reading them. This is arbitrary code execution by
  # design, exactly like exec: pages only (agents are refused above with
  # exec), and only the page that owns the paused run.
  # Answering a readline(). Deliberately the same shape as debug_cmd below —
  # both write a RAW console line into an interactive worker, and both are only
  # safe while something is actually reading one. A line sent at any other
  # moment reaches the dispatch loop as junk.
  if (identical(cmd$type, "input_reply")) {
    reply_err <- function(msg) {
      if (scalar_chr(cmd$id)) {
        try(rec$ws$send(toJSON(list(type = "input_reply", id = cmd$id, error = msg),
                               auto_unbox = TRUE)), silent = TRUE)
      }
    }
    if (!identical(k$mode, "interactive")) {
      reply_err("this worker cannot read input")
      return(invisible(NULL))
    }
    if (!isTRUE(sockets$input_waiting)) {
      reply_err("R is not waiting for input")
      return(invisible(NULL))
    }
    route <- if (!is.null(sockets$worker_active))
      sockets$worker_routes[[sockets$worker_active]] else NULL
    if (is.null(route) || !identical(route$rec, rec)) {
      reply_err("another page owns the waiting run")
      return(invisible(NULL))
    }
    value <- if (scalar_chr(cmd$value)) cmd$value else ""
    # One line, always. A newline would be a SECOND console line, and the second
    # one lands wherever R happens to be reading next.
    if (grepl("[\n\r]", value) || nchar(value) > 4000L) {
      reply_err("an answer must be a single line under 4000 characters")
      return(invisible(NULL))
    }
    # The ANSWER is the user's own text and never reaches the audit log; that a
    # prompt was answered, and how big the answer was, does.
    audit("input_reply", bytes = nchar(value, type = "bytes"))
    sockets$input_waiting <- FALSE
    try(kernel_console(k, value), silent = TRUE)
    return(invisible(NULL))
  }

  if (identical(cmd$type, "debug_cmd")) {
    reply_err <- function(msg) {
      if (scalar_chr(cmd$id)) {
        try(rec$ws$send(toJSON(list(type = "debug_cmd", id = cmd$id, error = msg),
                               auto_unbox = TRUE)), silent = TRUE)
      }
    }
    if (!identical(k$mode, "interactive")) {
      reply_err("the debugger needs an interactive worker")
      return(invisible(NULL))
    }
    if (!isTRUE(sockets$debug_paused)) {
      reply_err("R is not paused in the debugger")
      return(invisible(NULL))
    }
    route <- if (!is.null(sockets$worker_active))
      sockets$worker_routes[[sockets$worker_active]] else NULL
    if (is.null(route) || !identical(route$rec, rec)) {
      reply_err("another page owns the paused run")
      return(invisible(NULL))
    }
    action <- if (scalar_chr(cmd$action)) cmd$action else ""
    line <- switch(action,
      continue = "c", over = "n", into = "s", out = "f",
      where = ".carmar_debug_where()",
      abort = 'invokeRestart("carmar_abort_cell")',
      eval = if (scalar_chr(cmd$expr)) cmd$expr else NULL)
    if (is.null(line) || grepl("[\n\r]", line) || nchar(line) > 4000L) {
      reply_err("bad debug command")
      return(invisible(NULL))
    }
    audit("debug_cmd", action = action)
    try(kernel_console(k, line), silent = TRUE)
    # A step moves the position; follow it with a where so the client gets a
    # structured stack instead of parsing narration. If the step ran off the
    # end of the debuggable code the where line reaches the dispatch loop,
    # which ignores what it cannot parse — harmless by construction.
    if (action %in% c("over", "into", "out")) {
      try(kernel_console(k, ".carmar_debug_where()"), silent = TRUE)
    }
    return(invisible(NULL))
  }
  # Escalation after SIGINT did not land. This deliberately kills only the
  # execution worker; the supervisor's recovery loop keeps the notebook,
  # WebSocket, analyzer and unsaved text alive and starts a fresh R session.
  if (identical(cmd$type, "force_stop")) {
    audit("force-stop", class = rec$class %||% "unknown")
    # The death detector below reports the loss; it must say it was ASKED
    # for, not that R "stopped unexpectedly" — the words a person reads
    # after pressing Force Stop decide whether they trust the button.
    sockets$force_stop_asked <- TRUE
    if (k$proc$is_alive()) try(k$proc$kill(), silent = TRUE)
    return(invisible(NULL))
  }
  # Restart: a NEW worker process — fresh globalenv, fresh packages. The stored
  # hello is stale the moment the old worker dies; the new worker's ready frame
  # replaces it via pump() and reaches every open socket.
  if (identical(cmd$type, "restart")) {
    audit("restart")
    fail_worker_routes("R was restarted — this request was abandoned.")
    try(kernel_stop(k, grace = 1), silent = TRUE)
    k <<- start_execution_worker()
    sockets$hello <- NULL
    sockets$worker_recovering <- FALSE
    sockets$worker_restart_attempts <- 0L
    # In-flight runs died with the old worker; the new one will never emit
    # their done frames, and a stuck entry would block idle linger forever.
    rm(list = ls(sockets$running), envir = sockets$running)
    rm(list = ls(sockets$worker_routes), envir = sockets$worker_routes)
    sockets$worker_queue <- list()
    sockets$worker_active <- NULL
    sockets$worker_terminal <- NULL
    sockets$debug_paused <- FALSE
    sockets$input_waiting <- FALSE
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
  # This is an explicit New Session command from the local notebook. It starts
  # the sibling it was asked for; there is no count at which it silently turns
  # into some other action.
  if (identical(cmd$type, "session-new")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    audit("session-new", id = cmd$id)
    reply <- function(...) try(rec$ws$send(toJSON(list(type = "session-new", id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    started <- start_sibling_session()
    if (is.null(started)) {
      reply(error = "Could not start another session.")
      return(invisible(NULL))
    }
    reply(port = started$port, url = started$url)
    return(invisible(NULL))
  }
  # The key: get / set / clear, answered by the supervisor and by nobody else.
  if (identical(cmd$type, "ai-key")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(...) try(rec$ws$send(toJSON(list(type = "ai-key", id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    # See PAGE_ONLY_CLASSES: the key is for the notebook page, and "is this the
    # notebook page?" is answered by the connection, not by the sender.
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("ai-key-refused", class = rec$class %||% "unknown")
      reply(error = "Only the notebook page may read or write the stored key.")
      return(invisible(NULL))
    }
    # ONE OF THE TWO DOORS THE SUPERVISOR ACTUALLY OWNS. A policy that permits
    # no key-based provider means CarmaR does not keep a provider key at all:
    # not handed out, not stored. The user can still paste one into the tab —
    # this is their own browser and no kernel can prevent that — but nothing
    # persists it for them and the attempt is in the audit log. The refusal
    # covers `set` as much as `get`, because a key CarmaR holds under a
    # local-only policy is a key CarmaR should never have accepted.
    if (isTRUE(ai_policy$set) && !isTRUE(ai_policy$allows_key)) {
      audit("ai-key-refused", reason = "policy", class = rec$class %||% "unknown")
      reply(error = ai_policy_reason(ai_policy), policy = TRUE)
      return(invisible(NULL))
    }
    action <- if (scalar_chr(cmd$action)) cmd$action else "get"
    if (identical(action, "get")) {
      audit("ai-key", detail = "get", class = rec$class)
      reply(key = ai_key_read())
    } else if (action %in% c("set", "clear")) {
      # A `set` whose key is not a string used to fall through to value = "",
      # DELETE the stored key, and answer ok:true. A malformed request must not
      # be answered by destroying the thing it named. (`set ""` is still a
      # clear — that one is deliberate, and is how the browser moves a key out
      # of the kernel.)
      if (identical(action, "set") && !scalar_chr(cmd$key)) {
        reply(error = "ai-key: `set` needs `key` to be a string.")
        return(invisible(NULL))
      }
      # One branch: ai_key_write("") already deletes the file, so "clear" is
      # "set to nothing" and does not need a second tryCatch/audit pair to keep
      # in step with this one.
      value <- if (identical(action, "set")) cmd$key else ""
      ok <- tryCatch(ai_key_write(value), error = function(e) FALSE)
      # `ok` belongs in the log. Recording only the INTENT left the one question
      # an audit trail exists to answer — did the write land? — unanswerable.
      audit("ai-key", detail = if (nzchar(value)) "set" else "clear",
            class = rec$class, ok = isTRUE(ok))
      reply(ok = isTRUE(ok))
    } else {
      reply(error = "ai-key: action must be get, set or clear.")
    }
    return(invisible(NULL))
  }

  # What the administrator permits. Deliberately readable by ANY socket,
  # including a declared agent: it discloses provider names and an
  # administrator's own sentence, never a credential, and an agent that knows
  # the constraint can respect it instead of failing into it.
  if (identical(cmd$type, "ai-policy")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    try(rec$ws$send(toJSON(list(
      type = "ai-policy", id = cmd$id,
      set = isTRUE(ai_policy$set),
      # I() so a single permitted provider stays a JSON ARRAY. jsonlite
      # collapses a length-1 vector to a scalar under auto_unbox, and a page
      # that received "anthropic" where it expected ["anthropic"] would render
      # every provider as forbidden. Same trap frame-fidelity pins for bins.
      providers = I(as.character(ai_policy$providers)),
      # Each entry wrapped so a provider pinned to ONE model still arrives as
      # an array. Same collapsed-scalar trap as `providers` above, one level
      # deeper — and here the page would render a select with no options.
      models = lapply(ai_policy$models, I),
      # Administrator-pinned, credential-free provider endpoints. Unlike a
      # model list each value is deliberately scalar.
      base_urls = ai_policy$base_urls,
      local_only = isTRUE(ai_policy$local_only),
      note = ai_policy$note,
      # Is anything listening? The page asks once, here, rather than sending
      # audit frames into a kernel that discards them — and rather than a
      # second round trip, since "what governance is configured" is the same
      # question this op already answers.
      audit = nzchar(log_path),
      audit_text = ai_audit_text,
      source = if (identical(ai_policy$source, "env")) "environment"
               else if (nzchar(ai_policy$source)) "file" else ""),
      auto_unbox = TRUE, null = "null")), silent = TRUE)
    return(invisible(NULL))
  }

  # The AI conversation, into the audit stream. Fire-and-forget: there is no
  # reply, because a turn must never wait on its own logging, and a page that
  # could tell whether the write landed would be a probe for whether auditing
  # is on. `audit()` is already a no-op without CARMAR_LOG, so an ordinary
  # laptop pays nothing here beyond the parse it already did.
  if (identical(cmd$type, "ai-audit")) {
    event <- ai_audit_scalar(cmd$event, 32L)
    if (is.null(event) || !(event %in% AI_AUDIT_EVENTS)) return(invisible(NULL))
    rec_fields <- list()
    for (f in AI_AUDIT_FIELDS) {
      v <- ai_audit_scalar(cmd[[f]], 200L)
      if (!is.null(v) && !identical(v, "")) rec_fields[[f]] <- v
    }
    if (ai_audit_text) {
      for (f in AI_AUDIT_TEXT_FIELDS) {
        v <- ai_audit_scalar(cmd[[f]], 4000L)
        if (!is.null(v) && !identical(v, "")) rec_fields[[f]] <- v
      }
    }
    # Stamped, never accepted: who this is, and what kind of client it is.
    # A declared agent reporting its own work is exactly what should be in the
    # log — labelled as an agent, which is a thing it cannot say otherwise.
    do.call(audit, c(list(paste0("ai-", event)), rec_fields,
                     list(class = rec$class %||% "unknown",
                          role = rec$role %||% "page",
                          user = rec$user %||% "")))
    return(invisible(NULL))
  }

  # Reading the audit stream BACK, for the reviewer export.
  #
  # Page-only on the same grounds as the console history, and more so: with
  # CARMAR_LOG_AI_TEXT on, these lines contain the prompts the user typed. A
  # declared agent is refused (AGENT_REFUSED); a published origin and a native
  # client are refused by class.
  #
  # Only `ai-*` records are returned. The same file also holds socket opens,
  # refusals and the startup posture — operational security detail that a
  # notebook page has no business reading back, and that the reviewer export
  # would have no use for. The narrower answer is the whole point of having a
  # separate op rather than "read me the log".
  if (identical(cmd$type, "ai-audit-read")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(...) try(rec$ws$send(toJSON(list(type = "ai-audit-read", id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("ai-audit-read-refused", class = rec$class %||% "unknown")
      reply(error = "Only the notebook page may read the audit stream.")
      return(invisible(NULL))
    }
    if (!nzchar(log_path) || !file.exists(log_path)) {
      # Not an error: no log configured is the ordinary single-user state, and
      # the export says so in words rather than showing an empty table that
      # would read as "the AI did nothing".
      reply(records = I(list()), configured = nzchar(log_path))
      return(invisible(NULL))
    }
    lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) character())
    lines <- grep('"event":"ai-', lines, fixed = TRUE, value = TRUE)
    # Bounded: a long-lived kernel accumulates, and one frame must not become
    # megabytes. The TAIL, because a reviewer wants this session.
    if (length(lines) > AI_AUDIT_READ_MAX) {
      lines <- utils::tail(lines, AI_AUDIT_READ_MAX)
    }
    # Forwarded as the BYTES they were written as, parsed once by the browser:
    # re-encoding through jsonlite here would round numbers and collapse
    # fields, which is the frame-fidelity lesson one level down.
    reply(records = I(lines), configured = TRUE,
          truncated = length(lines) >= AI_AUDIT_READ_MAX)
    return(invisible(NULL))
  }

  # The console's cross-session history. Page-only on the same grounds as the
  # key: it is a record of everything this user has typed at their own machine.
  if (identical(cmd$type, "console_history")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(...) try(rec$ws$send(toJSON(list(type = "console_history", id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("console-history-refused", class = rec$class %||% "unknown")
      reply(error = "Only the notebook page may read or write the console history.")
      return(invisible(NULL))
    }
    action <- if (scalar_chr(cmd$action)) cmd$action else "get"
    if (identical(action, "get")) {
      # I() so a one-entry history stays an ARRAY on the wire. Without it
      # jsonlite unboxes it to a bare string and the page's history becomes
      # that string's characters.
      reply(lines = I(as.character(history_read())), enabled = HISTORY_ENABLED)
    } else if (identical(action, "add")) {
      if (!scalar_chr(cmd$line)) { reply(error = "console_history: `line` must be a string."); return(invisible(NULL)) }
      reply(ok = isTRUE(tryCatch(history_add(cmd$line), error = function(e) FALSE)))
    } else if (identical(action, "clear")) {
      reply(ok = identical(unlink(history_path()), 0L) && !file.exists(history_path()))
    } else {
      reply(error = "console_history: action must be get, add or clear.")
    }
    return(invisible(NULL))
  }

  # ── the user's own settings ────────────────────────────────────────────────
  #
  # In NEITHER allow-list, and that is the decision rather than an omission.
  # FORWARDED routes to the evaluating worker and ANALYZE_FORWARDED to the
  # analyzer; these read a file the SUPERVISOR owns and mutate SUPERVISOR
  # globals (linger_s, HISTORY_ENABLED), which worker.R could not change at
  # all — and routing a config-file WRITER into the process that evaluates
  # user code is precisely where such an op is most dangerous. Same shape and
  # same reason as ai-key: the supervisor answers it itself.
  #
  # Page-only for the ai-key reason exactly: a native client could edit
  # ~/.config/R/carmar/settings.json itself as this user, but CarmaR does not
  # do it for it, so the promise holds for every client rather than only the
  # polite ones. A published origin is refused because its reader approved that
  # site to run the chunks they press, not to reconfigure their kernel.
  if (cmd$type %in% c("settings_get", "settings_set", "settings_reset")) {
    if (!scalar_chr(cmd$id)) return(invisible(NULL))
    reply <- function(...) try(rec$ws$send(toJSON(list(type = cmd$type, id = cmd$id, ...),
                                                  auto_unbox = TRUE, null = "null")), silent = TRUE)
    if (!isTRUE(rec$class %in% PAGE_ONLY_CLASSES)) {
      audit("settings-refused", reason = "class", detail = cmd$type,
            class = rec$class %||% "unknown")
      reply(ok = FALSE, reason = "class",
            error = "Only the notebook page may read or change CarmaR's settings.")
      return(invisible(NULL))
    }
    # An unauthenticated shared kernel has no "the user" whose home directory
    # this is, so there is nobody to save a preference for.
    if (!isTRUE(deployment$loopback) && !nzchar(rec$user %||% "")) {
      audit("settings-refused", reason = "posture", detail = cmd$type)
      reply(ok = FALSE, reason = "posture",
            error = "This kernel serves unauthenticated clients, so it keeps no per-user settings.")
      return(invisible(NULL))
    }

    send_state <- function(extra = list()) {
      st <- settings_state
      rows <- lapply(names(st$settings), function(k) {
        e <- st$settings[[k]]
        stored <- st$file$values[[k]]
        list(key = e$key, label = e$label, kind = e$kind, effect = e$effect,
             value = e$value, source = e$source, locked = isTRUE(e$locked),
             writable = isTRUE(e$writable), env = e$env,
             min = e$min, max = e$max,
             # "the stored value differs from the one in force" — which is how
             # a restart_r / next_launch setting shows a pending badge instead
             # of pretending it already applied.
             pending = !is.null(stored) && !identical(stored, e$value))
      })
      admin <- lapply(st$admin, function(a)
        list(key = a$key, value = a$value, source = a$source))
      # I() on both, or a one-entry list unboxes to a scalar and the page
      # renders it character by character — the trap console_history and
      # ai-policy each already carry a comment about.
      args <- c(list(settings = I(rows), admin = I(admin),
                     file = list(status = st$file$status, path = st$file$path,
                                 error = st$file$error,
                                 ignored = I(as.character(st$file$ignored)),
                                 rejected = I(st$file$rejected))), extra)
      do.call(reply, args)
    }

    if (identical(cmd$type, "settings_get")) {
      # RE-RESOLVE, never report the cache. The file can change under a running
      # kernel — a hand edit, another session, a restored backup — and a page
      # that was told the cached value would be shown a setting the kernel is
      # not actually using. The live ones are re-applied at the same moment,
      # because those are precisely the ones that can be honoured now, and
      # reporting a new linger while running on the old one is the lie the
      # whole `effect` field exists to prevent.
      settings_state <<- carmar_settings_resolve()
      linger_s <<- suppressWarnings(as.numeric(settings_value("linger_seconds")))
      if (!length(linger_s) || !is.finite(linger_s)) linger_s <<- 600
      HISTORY_ENABLED <<- isTRUE(settings_value("history_enabled"))
      audit("settings", detail = "get", status = settings_state$file$status)
      send_state()
      return(invisible(NULL))
    }

    if (identical(cmd$type, "settings_reset")) {
      unlink(carmar_settings_path())
      settings_state <<- carmar_settings_resolve()
      linger_s <<- suppressWarnings(as.numeric(settings_value("linger_seconds")))
      HISTORY_ENABLED <<- isTRUE(settings_value("history_enabled"))
      audit("settings", detail = "reset", ok = TRUE)
      send_state(list(ok = TRUE))
      return(invisible(NULL))
    }

    # settings_set — one key per frame. A batch has to answer "three landed,
    # one did not", and the honest answer is per-key anyway.
    key <- if (scalar_chr(cmd$key)) cmd$key else ""
    entry <- settings_entry(key)
    if (is.null(entry)) {
      audit("settings-refused", reason = "unknown", key = key)
      reply(ok = FALSE, key = key, reason = if (key %in% SETTINGS_ADMIN) "admin" else "unknown",
            error = if (key %in% SETTINGS_ADMIN)
              "That setting belongs to this deployment's administrator."
            else "No such setting.")
      return(invisible(NULL))
    }
    if (isTRUE(settings_state$settings[[key]]$locked)) {
      audit("settings-refused", reason = "env", key = key)
      reply(ok = FALSE, key = key, reason = "env",
            error = paste0("This deployment sets ", entry$env,
                           ", so it cannot be changed here."))
      return(invisible(NULL))
    }
    if (!identical(settings_state$file$status, "ok") &&
        !identical(settings_state$file$status, "missing")) {
      # Writing over a file a human hand-edited, because of one stray byte,
      # would destroy their config. Reset is the documented way out.
      audit("settings-refused", reason = "file", key = key)
      reply(ok = FALSE, key = key, reason = "file", error = settings_state$file$error)
      return(invisible(NULL))
    }

    values <- settings_state$file$values
    if (is.null(cmd$value)) {
      values[[key]] <- NULL                       # unset: back to env/default
    } else {
      verdict <- carmar_settings_validate(key, cmd$value)
      if (!isTRUE(verdict$ok)) {
        # Refused, never coerced, and the file is NOT written — the ai-key
        # scar, where a payload of the wrong type fell through to "" and
        # deleted the key while replying ok:true.
        audit("settings-refused", reason = "value", key = key)
        reply(ok = FALSE, key = key,
              reason = if (identical(entry$kind, "number")) "range" else "type",
              error = paste0(entry$label, " ", verdict$reason, "."))
        return(invisible(NULL))
      }
      values[[key]] <- verdict$value
    }

    if (!isTRUE(carmar_settings_write(values))) {
      audit("settings", detail = "set", key = key, ok = FALSE)
      reply(ok = FALSE, key = key, reason = "write",
            error = "The settings file could not be written.")
      return(invisible(NULL))
    }
    settings_state <<- carmar_settings_resolve()
    applied <- FALSE
    if (identical(entry$effect, "live")) {
      if (identical(key, "linger_seconds")) {
        linger_s <<- suppressWarnings(as.numeric(settings_value("linger_seconds")))
        applied <- TRUE
      } else if (identical(key, "history_enabled")) {
        HISTORY_ENABLED <<- isTRUE(settings_value("history_enabled"))
        applied <- TRUE
      } else if (identical(key, "quarto_path")) {
        applied <- TRUE          # the job child is spawned per job
      }
    }
    audit("settings", detail = "set", key = key, ok = TRUE)
    send_state(list(ok = TRUE, key = key, effect = entry$effect,
                    applied = applied, pending = !applied))
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
# The FIRST re-send is fast: when the group SIGINT ends a child under
# system(), libc has thrown away the SIGINT R got while waiting, and R walks
# on to the next line. A second SIGINT 60 ms later reaches R itself, which is
# what makes the CHUNK stop rather than just the child. Later re-sends are
# slower — they exist for a child that ignores the first signal.
INTERRUPT_LADDER_FIRST <- 0.06
INTERRUPT_LADDER_STEP <- 0.4
INTERRUPT_LADDER_MAX <- 5L

#' Re-send SIGINT to a run that did not end after the last one.
#' @return Invisibly NULL.
interrupt_ladder <- function() {
  l <- sockets$interrupt_ladder
  if (is.null(l)) return(invisible(NULL))
  if (!identical(l$wire_id, sockets$worker_active)) {
    sockets$interrupt_ladder <- NULL
    return(invisible(NULL))
  }
  now <- as.numeric(Sys.time())
  if (now - l$at < (l$step %||% INTERRUPT_LADDER_STEP)) return(invisible(NULL))
  if (l$count >= INTERRUPT_LADDER_MAX) { sockets$interrupt_ladder <- NULL; return(invisible(NULL)) }
  audit("interrupt-again", count = l$count + 1L)
  kernel_interrupt(k)
  sockets$interrupt_ladder <- list(wire_id = l$wire_id, at = now, count = l$count + 1L,
                                   step = INTERRUPT_LADDER_STEP)
  invisible(NULL)
}

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
      # The page that asked may be gone — a reload, a closed tab, a followed
      # link. The RUN is not: R finished it, and the result is the only copy
      # of work that may have taken an hour. Sending it into a dead socket
      # (which `try(silent)` used to do, uncomplainingly) threw it away. Park
      # it on the route instead and keep the route alive, so the page that
      # comes back can adopt it and see its own output.
      if (is.function(terminal$route$on_reply)) {
        # The supervisor's own question (route_internal): its continuation
        # runs here, after the drain, with the frame the worker answered.
        try(terminal$route$on_reply(terminal$frame), silent = TRUE)
        drop_worker_route(wire_id)
      } else if (route_is_orphan(terminal$route) && identical(terminal$route$kind, "exec")) {
        terminal$route$parked <- terminal$frame
        terminal$route$parked_at <- Sys.time()
        audit("result-parked", srcname = terminal$route$srcname %||% "")
      } else {
        try(terminal$route$rec$ws$send(
              relay_frame(terminal$frame, wire_id, terminal$route$client_id)),
            silent = TRUE)
        drop_worker_route(wire_id)
      }
      if (identical(sockets$worker_active, wire_id)) sockets$worker_active <- NULL
      sockets$worker_terminal <- NULL
      dispatch_worker_queue()
    }
    return(invisible(NULL))
  }
  lapply(events, function(e) {
    # Debug frames mark the pause window; the run's own done closes it. The
    # flag is what gates debug_cmd, so it must flip on the frame, not on
    # anything the client does.
    if (identical(e$type, "debug")) sockets$debug_paused <- TRUE
    if (identical(e$type, "done")) sockets$debug_paused <- FALSE
    if (identical(e$type, "input_request")) sockets$input_waiting <- TRUE
    if (identical(e$type, "input_done") || identical(e$type, "done")) {
      sockets$input_waiting <- FALSE
    }
    # Plain stdout/stderr has no protocol id. The worker is serial, so it
    # belongs to the oldest execution still in flight — and so does a debug
    # frame, which carries no id by design (it is emitted mid-pause, from
    # inside the paused evaluation).
    had_id <- scalar_chr(e$id)
    wire_id <- if (had_id) e$id else
      if (e$type %in% c("stdout", "stderr", "debug")) sockets$worker_active else NULL
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
        # An exec's output is recorded for the WHOLE run, not only the part
        # after its page left. The buffer used to start at the moment of
        # orphaning, which loses everything printed before it — and a page that
        # reloads mid-run has an EMPTY output panel, so the part it is missing
        # is precisely the part that already went to the socket it no longer
        # has. Adoption then delivered a status and half a transcript.
        #
        # Recording always is affordable because it is bounded twice over: the
        # worker is one R session, so at most one exec route is producing
        # output at a time, and that route's buffer is capped. A run whose page
        # is still there drops its route (and the buffer with it) the moment it
        # finishes, so nothing accumulates across runs.
        if (identical(route$kind, "exec")) record_output(route, e)
        if (!route_is_orphan(route)) {
          try(route$rec$ws$send(relay_frame(e, wire_id, route$client_id)), silent = TRUE)
        }
      }
      return(invisible(NULL))
    }

    # Session-wide lifecycle frames have no owner and must reach every tab —
    # and no id to rewrite, so they forward exactly as the kernel wrote them.
    if (identical(e$type, "ready")) {
      e$protocol <- CARMAR_PROTOCOL_VERSION
      e$kernel_build <- CARMAR_KERNEL_BUILD
      e$installed_build <- installed_build()
      # session_upgrade is the SUPERVISOR's verb; it joins the worker's
      # vocabulary here so a page can tell, without probing, that this
      # kernel knows how to hand a session to the installed build.
      e$commands <- I(unique(c(as.character(e$commands %||% character()), "session_upgrade")))
      # relay_frame normally preserves the worker's original bytes verbatim.
      # This frame is now supervisor-owned, so force the faithful re-encode or
      # the two fields above would exist only in this R object, not on the wire.
      attr(e, "raw") <- NULL
      sockets$worker_recovering <- FALSE
      sockets$worker_restart_attempts <- 0L
      sockets$worker_restart_after <- 0
      audit("worker-ready", pid = e$pid %||% NA)
    }
    payload <- relay_frame(e)
    if (identical(e$type, "ready")) sockets$hello <- payload
    lapply(sockets$open, function(r) try(r$ws$send(payload), silent = TRUE))
    invisible(NULL)
  })
  invisible(NULL)
}

server <- httpuv::startServer(host, port, app)
audit("listening", bind = host, port = port, loopback = deployment$loopback,
      require_origin = deployment$require_origin,
      trust_proxy = deployment$trust_proxy,
      managed_config = identical(managed_config$status, "ok"),
      ai_policy = isTRUE(ai_policy$set),
      ai_providers = paste(ai_policy$providers, collapse = ","),
      unauthenticated = deployment$allow_unauthenticated)
if (deployment$require_origin) {
  # stderr, never stdout: the first stdout line is the {"url": ...} protocol
  # frame the launcher parses, and a banner ahead of it is an unparseable
  # first line. An operator who has bound to the network gets the posture
  # stated back to them, because the two spellings differ enormously and the
  # difference is invisible from the outside.
  message("CarmaR listening on ", host, ":", port,
          " — strict: a client with no Origin is refused")
  message(if (deployment$trust_proxy)
            paste0("  identity: ", deployment$user_header, " from ",
                   paste(deployment$proxy_addrs, collapse = ", "))
          else if (deployment$loopback)
            "  identity: none — loopback, so reachable only from this machine"
          else
            "  identity: NONE, and bound off loopback — anyone who can reach this port can run R as this user")
  message("  accepted Host headers: ", paste(deployment$hosts, collapse = ", "))
  message("  accepted Origins:      ", paste(deployment$origins, collapse = ", "))
}
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
                   "#kernel=", port, "&pair=", FILE_LAUNCH_CAP)

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
runtime_record <- list(url = url, file = file_url, port = port, pid = Sys.getpid(),
                       started = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
# A successor started by a handoff keeps the session's name, so the menu
# row does not flicker to "Untitled" between the two processes.
local({
  title <- Sys.getenv("CARMAR_SESSION_TITLE", "")
  Sys.unsetenv("CARMAR_SESSION_TITLE")
  title <- trimws(substr(gsub("[\"\\\\]", "'", gsub("[[:cntrl:]]", " ", title)), 1L, 120L))
  if (nzchar(title)) runtime_record$title <<- title
})
# CARMAR_LISTEN=1 marks a kernel started headless for published pages (the
# menu's Listen for Web Pages, the keep-ready daemon). The menu helper shows
# such a kernel as its Listen checkbox, not as a session row — there is no
# document behind it to name. The mark is HOW IT STARTED, so the first page
# that actually attaches clears it (see the upgrade handler): from then on it
# is an ordinary session.
if (identical(Sys.getenv("CARMAR_LISTEN"), "1")) runtime_record$listen <- TRUE

write_runtime <- function() {
  if (!nzchar(runtime_file)) return(invisible(FALSE))
  ok <- tryCatch({
    writeLines(toJSON(runtime_record, auto_unbox = TRUE), runtime_file)
    Sys.chmod(runtime_file, mode = "0600")
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

#' The page's own name for its document, into the runtime record.
#'
#' Display metadata only — nothing reads it back into the kernel. It exists so
#' the menu helper can label a session by what the user called it instead of
#' the notebook file's version stamp. Quotes are flattened because
#' helper-sessions.sh reads the record with a bounded sed, not a JSON parser,
#' and an embedded quote would truncate every field after it.
set_runtime_title <- function(title) {
  title <- gsub("[[:cntrl:]]", " ", title)
  title <- gsub("[\"\\\\]", "'", title)
  title <- trimws(substr(title, 1L, 120L))
  if (identical(runtime_record$title %||% "", title)) return(invisible(NULL))
  if (nzchar(title)) runtime_record$title <<- title
  else runtime_record$title <<- NULL
  write_runtime()
}

#' A page attached: this kernel is now a session, not a bare listener.
clear_runtime_listen <- function() {
  if (is.null(runtime_record$listen)) return(invisible(NULL))
  runtime_record$listen <<- NULL
  write_runtime()
}

if (!identical(Sys.getenv("CARMAR_NO_RUNTIME_FILE"), "1")) {
  runtime_dir <- Sys.getenv("CARMAR_RUNTIME_DIR",
                            file.path(path.expand("~"), ".carmar", "run"))
  made <- dir.exists(runtime_dir) ||
    dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
  if (made) {
    runtime_file <- file.path(runtime_dir, sprintf("kernel-%d.json", port))
    if (!write_runtime()) runtime_file <- ""
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
#' Forget results nobody came back for.
#'
#' A parked result keeps its route — and therefore `sockets$running` — alive,
#' which is exactly right for the reload it exists for and exactly wrong
#' forever: an unclaimed result would hold the kernel open for good. One
#' linger window is the same grace the kernel gives a page that never returns.
sweep_parked_results <- function() {
  if (linger_s <= 0) return(invisible(NULL))
  now <- Sys.time()
  for (w in ls(sockets$worker_routes)) {
    route <- sockets$worker_routes[[w]]
    if (is.null(route) || is.null(route$parked)) next
    if (as.numeric(difftime(now, route$parked_at %||% now, units = "secs")) >= linger_s) {
      audit("result-forgotten", srcname = route$srcname %||% "")
      drop_worker_route(w)
    }
  }
  invisible(NULL)
}

linger_check <- function() {
  if (linger_s <= 0) return(invisible(NULL))
  sweep_parked_results()
  if (!isTRUE(sockets$ever_connected) && is.null(sockets$idle_since)) {
    sockets$idle_since <- sockets$boot_at
  }
  # A running job counts as busy. This is the whole point of a DETACHED job:
  # you press Check, close the tab, and come back to a finished check. Without
  # this line the linger would stop the kernel — and the job with it — at the
  # exact moment nobody is watching, which is when a long task is most useful.
  idle <- length(sockets$open) == 0L && length(ls(sockets$running)) == 0L &&
    !jobs_running()
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
  pump_jobs()
  pump_terms()
  interrupt_ladder()
  reap_dead_sockets()
  linger_check()
  if (isTRUE(sockets$quit)) break
  if (!k$proc$is_alive()) {
    recover_execution_worker()
  }
}

# The loop only breaks on a deliberate shutdown — take the
# discovery file with us so agents stop finding a kernel that is gone. A
# SIGKILL skips this, which is why readers must health-check before trusting
# a runtime file (stale files are litter, not authority). Running Claude
# conversations die with the kernel — nothing may keep billing after Quit.
for (id in ls(sockets$chats)) chat_kill(id)
# Detached from a page, never from the kernel: a job is a child of THIS
# process, and a supervisor that exits leaving an orphaned `devtools::check()`
# holding a lock on the library is exactly the stranded-process problem the
# linger exists to prevent.
for (id in ls(sockets$jobs)) {
  job <- sockets$jobs[[id]]
  if (identical(job$state, "running")) try(job$k$proc$kill(), silent = TRUE)
}
# Only OUR record: a successor on this port (session_handoff_finish) may
# already have written its own, and taking that one down would hide a live
# session from every launcher and menu.
if (nzchar(runtime_file)) {
  mine <- tryCatch({
    rec <- jsonlite::fromJSON(runtime_file, simplifyVector = TRUE)
    identical(as.integer(rec$pid), Sys.getpid())
  }, error = function(e) TRUE)
  if (isTRUE(mine)) unlink(runtime_file)
}
