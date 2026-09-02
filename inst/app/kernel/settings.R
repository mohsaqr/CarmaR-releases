# settings.R — the settings a USER may change, and the ones only an
# administrator may.
#
# Until now every kernel-side preference was an environment variable read once
# at startup: CARMAR_LINGER, CARMAR_NO_HISTORY, CARMAR_ROOT, CARMAR_LOG. That
# is exactly right for a deployment and useless for a person, who has no place
# to set one and no way to see what it currently is. This module adds a user
# config file and — more importantly — the rule for who wins.
#
# Pure, in the shape of deployment.R and ai-policy.R: `env` is injectable, the
# resolve returns a whole decision rather than mutating anything, and the only
# I/O is the one file. serve.R sources it; nothing else does.
#
# ── THE PRECEDENCE RULE ─────────────────────────────────────────────────────
#
#   environment variable (set and non-empty)  ▸  config file  ▸  built-in default
#
# and two consequences that matter more than the ordering:
#
#   · A setting whose env var is set is READ-ONLY for the life of this kernel.
#     Not "the file is ignored this time" — `settings_set` refuses it and does
#     not write the file, because a value silently stored and never applied is
#     a worse lie than a refusal.
#
#   · An administrator's setting is NOT IN THIS FILE'S VOCABULARY AT ALL. It is
#     not "env wins if present": the key is rejected on read and refused on
#     write, always. "Env wins when set" only guarantees anything WHEN THE ENV
#     IS SET, and a deployment that never sets CARMAR_ROOT is still one where a
#     user must not invent one — a root the user chose reads as protection
#     while being none (worker.R is explicit that it is a guardrail, not a
#     sandbox). The refusal must not depend on an administrator having
#     remembered to set every variable defensively.
#
# The tempting alternative — a layered Sys.getenv-shaped getter handed to
# carmar_deployment() and carmar_ai_policy() — is deliberately NOT built. It
# would put every admin variable one argument-passing mistake away from being
# file-settable. Those two keep plain Sys.getenv; only the user-writable call
# sites in serve.R read this module.

SETTINGS_VERSION <- 1L

# The whole vocabulary. A key that is not here does not exist: there is no
# pass-through, so the file cannot grow a control by accident.
#
# `effect` is a FIELD, not a sentence the page composes, so a setting added
# later cannot arrive without an honest answer to "when does this take
# effect?". Its three values:
#   live         the running kernel changes now
#   restart_r    the next R session picks it up (Session ▸ Restart R)
#   next_launch  only a new kernel
SETTINGS <- list(
  list(key = "linger_seconds", env = "CARMAR_LINGER", kind = "number",
       default = 600, min = 0, max = 86400, effect = "live",
       label = "Keep R running after the last tab closes"),
  list(key = "history_enabled", env = "CARMAR_NO_HISTORY", kind = "bool",
       default = TRUE, effect = "live", inverted = TRUE,
       label = "Remember console history between sessions"),
  list(key = "analyze_budget_s", env = "CARMAR_ANALYZE_BUDGET", kind = "number",
       default = 3, min = 0, max = 60, effect = "next_launch",
       label = "Code-intelligence time budget"),
  list(key = "native_dialog", env = "CARMAR_NO_NATIVE_DIALOG", kind = "bool",
       default = TRUE, effect = "restart_r", inverted = TRUE,
       label = "Use the operating system's file dialogs"),
  list(key = "quarto_path", env = "CARMAR_QUARTO", kind = "path",
       default = "", effect = "live",
       label = "Quarto binary"),
  list(key = "rscript_path", env = "CARMAR_RSCRIPT", kind = "path",
       default = "", effect = "restart_r",
       label = "R binary"),
  list(key = "port", env = "CARMAR_PORT", kind = "number",
       default = 4747, min = 1024, max = 65535, effect = "next_launch",
       label = "Preferred port")
)

# Reported to the page as read-only facts, never settable. Everything an
# administrator owns lives here and NOWHERE in SETTINGS, which is what makes
# the refusal structural rather than a lookup someone can forget.
SETTINGS_ADMIN <- c(
  "CARMAR_MANAGED_CONFIG",
  "CARMAR_ROOT", "CARMAR_LOG", "CARMAR_LOG_AI_TEXT",
  "CARMAR_CRAN_MIRROR",
  "CARMAR_AI_LOCAL_ONLY", "CARMAR_AI_POLICY", "CARMAR_AI_PROVIDERS",
  "CARMAR_BIND", "CARMAR_HOSTS", "CARMAR_ORIGINS", "CARMAR_REQUIRE_ORIGIN",
  "CARMAR_TRUST_PROXY", "CARMAR_USER_HEADER", "CARMAR_TRUSTED_PROXY",
  "CARMAR_ALLOW_UNAUTHENTICATED", "CARMAR_PORT_STRICT",
  "CARMAR_UPDATE_FEED", "CARMAR_UPDATE_MIRROR", "CARMAR_UPDATE_OFFLINE_DIR",
  "CARMAR_UPDATE_MANAGED"
)

# A managed desktop cannot depend on shell startup files: Finder/LaunchServices
# does not read them, and a standard user can edit them. Jamf/Intune instead
# place this small JSON file in an administrator-owned directory. Its values
# become environment variables before deployment, AI, update, or user-setting
# policy is resolved, so the existing env-wins-and-locks rule remains the one
# source of truth.
MANAGED_SETTINGS_SCHEMA <- "carmar-managed-settings-v1"
MANAGED_SETTINGS_ENV <- unique(c(
  vapply(SETTINGS, function(s) s$env, character(1)),
  setdiff(SETTINGS_ADMIN, "CARMAR_MANAGED_CONFIG"),
  "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY"
))

carmar_managed_default_path <- function(env = Sys.getenv,
                                        os = .Platform$OS.type,
                                        sysname = Sys.info()[["sysname"]]) {
  if (identical(sysname, "Darwin")) {
    return("/Library/Application Support/CarmaR/managed-settings.json")
  }
  if (identical(os, "windows")) {
    base <- trimws(env("ProgramData", ""))
    if (!nzchar(base)) base <- "C:/ProgramData"
    return(file.path(base, "CarmaR", "managed-settings.json"))
  }
  "/etc/carmar/managed-settings.json"
}

# Protection is checked from the standard user's point of view. Unix also
# requires root ownership because a user-owned 0400 file can simply be chmod'd
# and replaced. On Windows, the deployment recipe gives ProgramData/CarmaR an
# Administrators+SYSTEM write ACL; neither the file nor its parent may be
# writable by the running account.
carmar_managed_protected <- function(path, os = .Platform$OS.type) {
  if (nzchar(Sys.readlink(path))) return(FALSE)
  if (file.access(path, 2L) == 0L || file.access(dirname(path), 2L) == 0L) {
    return(FALSE)
  }
  if (!identical(os, "windows")) {
    info <- file.info(path)
    if (is.na(info$uid[[1]]) || info$uid[[1]] != 0L || settings_insecure(path)) {
      return(FALSE)
    }
  }
  TRUE
}

# Read one administrator policy atomically. Unlike the user settings file,
# unknown or malformed entries fail the WHOLE policy closed: partial policy is
# not a safe enterprise fallback.
carmar_managed_read <- function(path, required = FALSE,
                                protection = carmar_managed_protected) {
  empty <- list(status = "missing", path = path, values = character(), errors = character())
  if (!file.exists(path)) {
    if (isTRUE(required)) {
      empty$status <- "invalid"
      empty$errors <- "CARMAR_MANAGED_CONFIG names a file that does not exist."
    }
    return(empty)
  }
  if (!isTRUE(protection(path))) {
    empty$status <- "insecure"
    empty$errors <- paste0(
      "The managed settings file is writable by the current account, is a symlink, ",
      "or is not administrator-owned; CarmaR refused it.")
    return(empty)
  }
  size <- file.info(path)$size[[1]]
  if (is.na(size) || size > 1024 * 1024) {
    empty$status <- "invalid"
    empty$errors <- "The managed settings file exceeds the 1 MB limit."
    return(empty)
  }
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                  error = function(e) NULL)
  root_names <- if (is.list(raw)) names(raw) else NULL
  if (is.null(root_names) || anyDuplicated(root_names) ||
      !setequal(root_names, c("schema", "settings")) ||
      !identical(raw$schema, MANAGED_SETTINGS_SCHEMA) ||
      !is.list(raw$settings) || is.null(names(raw$settings)) ||
      anyDuplicated(names(raw$settings))) {
    empty$status <- "invalid"
    empty$errors <- paste0("The managed settings file must use schema ",
                           MANAGED_SETTINGS_SCHEMA, " with one settings object.")
    return(empty)
  }
  keys <- names(raw$settings)
  unknown <- setdiff(keys, MANAGED_SETTINGS_ENV)
  bad <- keys[!vapply(raw$settings, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(value) && !grepl("[\r\n\t]", value)
  }, logical(1))]
  errors <- character()
  if (length(unknown)) {
    errors <- c(errors, paste0("Unknown managed setting: ", paste(unknown, collapse = ", "), "."))
  }
  if (length(bad)) {
    errors <- c(errors, paste0("Managed settings must be non-empty single-line strings: ",
                               paste(bad, collapse = ", "), "."))
  }
  if (length(errors)) {
    empty$status <- "invalid"
    empty$errors <- errors
    return(empty)
  }
  values <- vapply(raw$settings, identity, character(1))
  list(status = "ok", path = path, values = values, errors = character())
}

carmar_managed_environment <- function(env = Sys.getenv, path = NULL,
                                       protection = carmar_managed_protected,
                                       apply_values = TRUE) {
  explicit <- trimws(env("CARMAR_MANAGED_CONFIG", ""))
  if (is.null(path)) path <- if (nzchar(explicit)) explicit else carmar_managed_default_path(env)
  result <- carmar_managed_read(path, required = nzchar(explicit), protection = protection)
  if (isTRUE(apply_values) && identical(result$status, "ok") && length(result$values)) {
    do.call(Sys.setenv, as.list(result$values))
  }
  result
}

settings_entry <- function(key) {
  for (s in SETTINGS) if (identical(s$key, key)) return(s)
  NULL
}

#' Where the user's settings live — beside the AI key, same directory.
carmar_settings_path <- function(env = Sys.getenv) {
  dir <- tools::R_user_dir("carmar", "config")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "settings.json")
}

#' Is the file writable by anyone but its owner?
#'
#' This file is an INTEGRITY surface rather than a secret: another local account
#' that can write it turns a knob in this user's next kernel. So a
#' group- or other-writable file is IGNORED rather than repaired — silently
#' chmod-ing someone else's file back to 0600 would hide that it had been
#' changed at all.
settings_insecure <- function(path) {
  info <- file.info(path)
  if (is.na(info$mode[[1]])) return(FALSE)
  mode <- as.integer(info$mode[[1]])
  bitwAnd(mode, as.integer(strtoi("077", 8L))) != 0L
}

#' Validate one value against its entry.
#' @return list(ok, value, reason)
carmar_settings_validate <- function(key, value) {
  entry <- settings_entry(key)
  if (is.null(entry)) {
    if (key %in% SETTINGS_ADMIN) {
      return(list(ok = FALSE, reason = "administrator setting"))
    }
    return(list(ok = FALSE, reason = "unknown setting"))
  }
  if (identical(entry$kind, "bool")) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(list(ok = FALSE, reason = "must be true or false"))
    }
    return(list(ok = TRUE, value = value))
  }
  if (identical(entry$kind, "number")) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      return(list(ok = FALSE, reason = "must be a number"))
    }
    if (!is.null(entry$min) && value < entry$min) {
      return(list(ok = FALSE, reason = paste0("must be at least ", entry$min)))
    }
    if (!is.null(entry$max) && value > entry$max) {
      return(list(ok = FALSE, reason = paste0("must be at most ", entry$max)))
    }
    # Always a double. jsonlite reads 900 from a file as an INTEGER and 900.0
    # from a set frame as a double, so without this the same setting has two
    # types depending on how it arrived — and every `identical()` against it
    # becomes a coin toss.
    return(list(ok = TRUE, value = as.numeric(value)))
  }
  # path
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(list(ok = FALSE, reason = "must be a string"))
  }
  # An empty path means "use the ladder" — that is a legitimate value, not a
  # missing one, and it is how a user clears an override.
  if (nzchar(value) && !file.exists(value)) {
    return(list(ok = FALSE, reason = "no such file"))
  }
  list(ok = TRUE, value = value)
}

#' Read the config file.
#'
#' FOUR states, kept distinct — the ai-policy.R lesson that folding failure
#' modes together is the fail-open bug:
#'   missing   no file. The ordinary single-user state, NOT an error.
#'   ok        parsed, an object, the right version.
#'   invalid   unparseable, not an object, wrong version → all defaults.
#'   insecure  group/other writable → all defaults, different words.
#'
#' Per-key failure stays per-key: an unknown key joins `ignored`, a bad value
#' joins `rejected`, and the rest of the file still applies. One bad number
#' must not silently revert eleven good ones.
carmar_settings_read <- function(path = carmar_settings_path()) {
  empty <- list(status = "missing", path = path, values = list(),
                ignored = character(), rejected = list(), error = "")
  if (!file.exists(path)) return(empty)
  if (settings_insecure(path)) {
    return(modifyList(empty, list(status = "insecure",
      error = "The settings file is writable by other accounts on this machine, so it was ignored.")))
  }
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || !is.list(raw) || is.null(names(raw))) {
    return(modifyList(empty, list(status = "invalid",
      error = "The settings file could not be read as JSON.")))
  }
  if (!identical(as.integer(raw$version %||% 0L), SETTINGS_VERSION)) {
    return(modifyList(empty, list(status = "invalid",
      error = "The settings file is a version this CarmaR does not know.")))
  }
  block <- raw$settings
  if (is.null(block)) block <- list()
  if (!is.list(block) || (length(block) && is.null(names(block)))) {
    return(modifyList(empty, list(status = "invalid",
      error = "The settings file's `settings` is not an object.")))
  }
  values <- list()
  ignored <- character()
  rejected <- list()
  for (key in names(block)) {
    verdict <- carmar_settings_validate(key, block[[key]])
    if (isTRUE(verdict$ok)) {
      values[[key]] <- verdict$value
    } else if (identical(verdict$reason, "unknown setting")) {
      ignored <- c(ignored, key)
    } else {
      rejected[[length(rejected) + 1L]] <- list(key = key, reason = verdict$reason)
    }
  }
  list(status = "ok", path = path, values = values,
       ignored = ignored, rejected = rejected, error = "")
}

#' Write the whole settings block. Create → chmod 0600 → CHECK → write, the
#' ordering ai_key_write() uses and for the same reason: a file that was
#' briefly group-writable was briefly tamperable.
carmar_settings_write <- function(values, path = carmar_settings_path()) {
  if (!file.exists(path) && !file.create(path, showWarnings = FALSE)) return(FALSE)
  if (!isTRUE(Sys.chmod(path, "0600"))) {
    unlink(path)
    return(FALSE)
  }
  body <- list(version = SETTINGS_VERSION,
               settings = if (length(values)) values else structure(list(), names = character()))
  ok <- tryCatch({
    writeLines(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
    TRUE
  }, error = function(e) FALSE)
  ok
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' The whole decision: what every setting resolves to, and from where.
#'
#' @return list(settings = list of per-key records, admin = list, file = the read)
carmar_settings_resolve <- function(env = Sys.getenv, file = NULL) {
  if (is.null(file)) file <- carmar_settings_read()
  usable <- identical(file$status, "ok")

  out <- lapply(SETTINGS, function(s) {
    raw <- env(s$env, "")
    from_env <- nzchar(raw)
    value <- s$default
    source <- "default"
    if (from_env) {
      source <- "env"
      value <- if (identical(s$kind, "bool")) {
        # CARMAR_NO_HISTORY and CARMAR_NO_NATIVE_DIALOG are NEGATIVE flags —
        # any value means "off" — while the file and the UI hold the positive.
        # Storing the positive is what a checkbox actually is; translating
        # happens here, once.
        if (isTRUE(s$inverted)) FALSE else TRUE
      } else if (identical(s$kind, "number")) {
        n <- suppressWarnings(as.numeric(raw))
        if (is.finite(n)) n else s$default
      } else raw
    } else if (usable && !is.null(file$values[[s$key]])) {
      source <- "file"
      value <- file$values[[s$key]]
    }
    list(key = s$key, label = s$label, kind = s$kind, effect = s$effect,
         value = value, source = source, locked = from_env,
         writable = !from_env, min = s$min, max = s$max, env = s$env)
  })
  names(out) <- vapply(SETTINGS, function(s) s$key, character(1))

  admin <- lapply(SETTINGS_ADMIN, function(name) {
    raw <- env(name, "")
    list(key = name, value = raw, source = if (nzchar(raw)) "env" else "default")
  })

  list(settings = out, admin = admin, file = file)
}

#' The resolved scalar for one key.
carmar_settings_value <- function(resolved, key) {
  entry <- resolved$settings[[key]]
  if (is.null(entry)) NULL else entry$value
}

#' What a CHILD process should inherit — the settings that only take effect in
#' a freshly spawned worker, job or analyzer, expressed back in the environment
#' vocabulary those processes read.
carmar_settings_child_env <- function(resolved) {
  out <- character()
  nd <- carmar_settings_value(resolved, "native_dialog")
  if (isFALSE(nd)) out["CARMAR_NO_NATIVE_DIALOG"] <- "1"
  q <- carmar_settings_value(resolved, "quarto_path")
  if (is.character(q) && nzchar(q)) out["CARMAR_QUARTO"] <- q
  b <- carmar_settings_value(resolved, "analyze_budget_s")
  if (is.numeric(b)) out["CARMAR_ANALYZE_BUDGET"] <- as.character(b)
  out
}
