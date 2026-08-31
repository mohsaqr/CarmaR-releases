# deployment.R — where this kernel listens, and who is allowed to reach it.
#
# CarmaR was written for exactly one deployment: a loopback socket serving one
# person on their own machine. Every trust rule in serve.R is an expression of
# that, and one of them INVERTS the moment the socket leaves loopback:
#
#   a request with NO Origin header is currently allowed outright.
#
# On 127.0.0.1 that is correct and deliberate — no Origin means "not a
# browser", i.e. the launcher, R, curl, the MCP bridge, all of which are the
# same OS user, all of which could have run `Rscript` themselves. The socket
# grants them nothing they did not already have.
#
# On 0.0.0.0 the identical rule reads: `curl` from anywhere on the network may
# execute R as this user, without so much as a consent click. Same code, same
# header, opposite meaning — because the premise that "no Origin" implies
# "same user" was a fact about loopback, not a fact about HTTP.
#
# So this module computes the posture ONCE, at startup, and hands serve.R a
# decided answer rather than an environment variable to re-interpret at each
# call site. Everything here is pure: `env` is injected so the whole policy is
# testable without a server (test/deployment.test.mjs drives it through R).
#
# The refusals are the point. Binding off loopback is a deployment decision
# with consequences the operator must state out loud:
#
#   · no CARMAR_HOSTS            → refuse. An unset Host allow-list off
#                                  loopback is a DNS-rebinding hole with the
#                                  door held open.
#   · no authentication in front → refuse, unless CARMAR_ALLOW_UNAUTHENTICATED
#                                  is set. The variable is deliberately long
#                                  and unpleasant to type; it is a decision,
#                                  not a default.
#
# The intended P0 shape is one CarmaR per person, each as their own OS user on
# its own port, with TLS and login in a reverse proxy (Caddy + OIDC
# forward-auth). serve.R never sees a password; it trusts an identity header,
# and only from the proxy address. See docs/server.md.

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a)) b else a

LOOPBACK_HOSTS <- c("127.0.0.1", "::1", "[::1]", "localhost")

#' Split a comma/space-separated environment value into trimmed, non-empty parts.
split_list <- function(value) {
  parts <- trimws(unlist(strsplit(value %||% "", "[,[:space:]]+")))
  parts[nzchar(parts)]
}

#' Is this bind address one of the loopback literals?
#'
#' Deliberately a literal comparison and not a resolve: a NAME that currently
#' resolves to 127.0.0.1 is not the same promise as an address that cannot
#' leave the machine, and the whole point of the flag is that it cannot be
#' talked into being true.
is_loopback_bind <- function(bind) tolower(bind %||% "") %in% LOOPBACK_HOSTS

#' Compute the deployment posture from the environment.
#'
#' @param port the port the server will listen on.
#' @param env  a getter with `Sys.getenv`'s signature; injected for tests.
#' @return a list with `bind`, `loopback`, `origins` (exact strings that may
#'   open a socket with no consent click), `hosts` (accepted Host headers),
#'   `allow_native` (may an Origin-less client connect?), `trust_proxy`,
#'   `user_header`, `proxy_addrs`, and `errors` — a character vector that is
#'   EMPTY when the posture is safe to serve. A non-empty `errors` must abort
#'   startup; it is never a warning.
carmar_deployment <- function(port, env = Sys.getenv) {
  bind <- trimws(env("CARMAR_BIND", "127.0.0.1"))
  if (!nzchar(bind)) bind <- "127.0.0.1"
  loopback <- is_loopback_bind(bind)

  # The origin this kernel serves its own page from is always allowed: that is
  # the same-origin case, and it is what `origin_ok` meant before this file
  # existed. Off loopback the server is behind a proxy, so its own page
  # arrives with the PROXY's origin — which the operator must name.
  self_origins <- c(sprintf("http://%s:%d", bind, port),
                    sprintf("http://127.0.0.1:%d", port),
                    sprintf("http://localhost:%d", port))
  origins <- unique(c(self_origins, split_list(env("CARMAR_ORIGINS", ""))))

  hosts_env <- split_list(env("CARMAR_HOSTS", ""))
  default_hosts <- c(sprintf("127.0.0.1:%d", port), sprintf("localhost:%d", port),
                     sprintf("[::1]:%d", port))
  # A configured host may or may not carry a port. Accept both spellings so an
  # operator writing `stats.example.edu` is not silently refused behind a proxy
  # that forwards the default 80/443 and therefore sends no port at all.
  hosts_env <- unique(c(hosts_env, ifelse(grepl(":", hosts_env, fixed = TRUE),
                                          hosts_env,
                                          sprintf("%s:%d", hosts_env, port))))
  hosts <- tolower(unique(c(default_hosts, hosts_env)))

  # "Anyone who reaches this port is this same OS user" is the premise behind
  # allowing an Origin-less client, and binding off loopback is the obvious way
  # to break it — but not the only one. A shared multi-user host, or a container
  # whose 127.0.0.1 is port-forwarded out, breaks it while still binding
  # loopback. So the posture is declarable in its own right and the bind
  # address merely IMPLIES it; an operator hardening a shared machine does not
  # have to move the socket to say so.
  require_origin <- identical(env("CARMAR_REQUIRE_ORIGIN", ""), "1") || !loopback

  trust_proxy <- identical(env("CARMAR_TRUST_PROXY", ""), "1")
  user_header <- trimws(env("CARMAR_USER_HEADER", "X-Forwarded-User"))
  proxy_addrs <- split_list(env("CARMAR_TRUSTED_PROXY", "127.0.0.1 ::1"))
  allow_open <- identical(env("CARMAR_ALLOW_UNAUTHENTICATED", ""), "1")

  errors <- character(0)
  if (!loopback) {
    if (!length(hosts_env)) {
      errors <- c(errors, paste0(
        "CARMAR_BIND is ", bind, " but CARMAR_HOSTS is unset. Off loopback the ",
        "Host allow-list is the only defence against DNS rebinding, so it must ",
        "name the hostname readers will use (e.g. CARMAR_HOSTS=stats.example.edu)."))
    }
    if (!trust_proxy && !allow_open) {
      errors <- c(errors, paste0(
        "CARMAR_BIND is ", bind, " with no authentication in front of it. Put ",
        "CarmaR behind a reverse proxy that authenticates and set ",
        "CARMAR_TRUST_PROXY=1, or set CARMAR_ALLOW_UNAUTHENTICATED=1 to serve R ",
        "execution to everyone who can reach this port. See docs/server.md."))
    }
    if (trust_proxy && !nzchar(user_header)) {
      errors <- c(errors, "CARMAR_TRUST_PROXY=1 requires a non-empty CARMAR_USER_HEADER.")
    }
    if (trust_proxy && !length(proxy_addrs)) {
      errors <- c(errors, "CARMAR_TRUST_PROXY=1 requires CARMAR_TRUSTED_PROXY to name the proxy's address.")
    }
  }

  list(
    bind = bind,
    port = port,
    loopback = loopback,
    origins = origins,
    hosts = hosts,
    # THE INVERSION. On a private loopback an absent Origin is a same-user
    # native client and is allowed; anywhere the port is reachable by someone
    # else it is an unauthenticated stranger and is not.
    allow_native = !require_origin,
    require_origin = require_origin,
    trust_proxy = trust_proxy && require_origin,
    user_header = user_header,
    # httpuv's Rook environment spells a header `HTTP_` + uppercase, `-`→`_`.
    user_header_key = paste0("HTTP_", toupper(gsub("-", "_", user_header, fixed = TRUE))),
    proxy_addrs = proxy_addrs,
    allow_unauthenticated = allow_open,
    errors = errors)
}

#' Who is this request, according to the proxy in front of us?
#'
#' Returns "" unless the posture trusts a proxy AND the request actually came
#' from one of the named proxy addresses. That second half is what stops the
#' header being a self-service login: anyone who can reach the port directly
#' can set `X-Forwarded-User: root`, so the header is only ever as trustworthy
#' as the guarantee that nobody but the proxy can reach the port.
proxy_user <- function(req, deployment) {
  if (!isTRUE(deployment$trust_proxy)) return("")
  peer <- tolower(req$REMOTE_ADDR %||% "")
  if (!nzchar(peer) || !(peer %in% tolower(deployment$proxy_addrs))) return("")
  name <- req[[deployment$user_header_key]] %||% ""
  # One line, printable, bounded — it reaches the audit log and the page title.
  name <- gsub("[[:cntrl:]]", "", name)
  trimws(substr(name, 1L, 128L))
}
