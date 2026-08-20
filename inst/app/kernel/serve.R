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
# Runs in flight: exec id → TRUE, cleared when the worker's done frame for
# that id comes back through pump(). This is the busy guard's whole evidence.
sockets$running <- new.env(parent = emptyenv())
sockets$ever_connected <- FALSE
sockets$idle_since <- NULL

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
FORWARDED <- c("env", "obj", "struct", "view", "rm", "packages", "help", "wd",
               "parse", "complete", "files", "import", "readfile", "writefile",
               "hover", "format", "sniff", "choose")

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
    if (!is.null(origin) && !identical(origin, origin_ok) &&
        !identical(origin, "null")) {
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

#' Act on one WebSocket frame. Validates, then forwards; never evaluates.
handle_frame <- function(message, rec) {
  if (is.raw(message)) return(invisible(NULL))           # binary: not our protocol
  if (nchar(message, type = "bytes") > MAX_FRAME_BYTES) {
    audit("frame-too-large", bytes = nchar(message, type = "bytes"))
    return(invisible(NULL))
  }
  cmd <- tryCatch(fromJSON(message, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(cmd) || !scalar_chr(cmd$type)) return(invisible(NULL))

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
    assign(cmd$id, TRUE, envir = sockets$running)
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
    k <<- kernel_start(file.path(here, "worker-boot.R"))
    sockets$hello <- NULL
    # In-flight runs died with the old worker; the new one will never emit
    # their done frames, and a stuck entry would block idle linger forever.
    rm(list = ls(sockets$running), envir = sockets$running)
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
      lapply(sockets$open, function(r) try(r$ws$send(reply), silent = TRUE))
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
  # A done frame retires its run from the in-flight set even when no socket
  # is left to deliver it to — that is the moment the idle clock may start.
  lapply(events, function(e) {
    if (identical(e$type, "done") && length(e$id) == 1L && is.character(e$id) &&
        exists(e$id, envir = sockets$running, inherits = FALSE)) {
      rm(list = e$id, envir = sockets$running)
    }
  })
  lapply(sockets$open, function(r) {
    lapply(payload, function(p) try(r$ws$send(p), silent = TRUE))
  })
  invisible(NULL)
}

server <- httpuv::startServer(host, port, app)
on.exit({ httpuv::stopServer(server); kernel_stop(k) }, add = TRUE)

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

#' Idle linger, decided once per loop turn: nobody connected, nothing running,
#' grace elapsed → the same deliberate shutdown /shutdown performs. Any open
#' socket or in-flight run resets the clock entirely.
linger_check <- function() {
  if (linger_s <= 0 || !isTRUE(sockets$ever_connected)) return(invisible(NULL))
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
