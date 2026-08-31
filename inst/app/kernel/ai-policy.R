# ai-policy.R — which AI providers an administrator permits, and what CarmaR
# can actually do about it.
#
# ── the honest scope, first, because everything else depends on it ──────────
#
# CarmaR runs in a browser the user controls, and the AI request for a
# key-based provider is a `fetch()` from that page. No kernel can prevent a
# determined user with devtools and their own API key from calling any endpoint
# they like. Any document promising otherwise would be lying.
#
# What an administrator policy DOES buy, and what procurement actually asks
# for, is three things:
#
#   1. a default the user did not choose and cannot casually change,
#   2. the constraint made VISIBLE in the notebook rather than discovered by
#      hitting it, and
#   3. the two doors the kernel genuinely owns, closed.
#
# Those two doors are real enforcement, not theatre:
#
#   · `ai-key` — the provider key lives in the user's R config directory and is
#     handed out by the supervisor. Under a local-only policy the supervisor
#     stops handing it out at all. A user can still paste a key into the tab,
#     but CarmaR will not keep one for them, and the refusal is audited.
#   · `agent-chat` — Claude Code and Codex turns are processes the SUPERVISOR
#     spawns. A policy that does not permit them means they are never spawned.
#     There is no page-side bypass, because there is no page-side spawn.
#
# Everything else is disclosure and audit. That is stated in docs/server.md in
# the same words, so nobody buys this expecting a sandbox.
#
# ── where a policy comes from ──────────────────────────────────────────────
#
#   CARMAR_AI_POLICY=/etc/carmar/ai-policy.json   a file the user cannot write
#   CARMAR_AI_PROVIDERS=anthropic,lmstudio        the quick form
#   CARMAR_AI_LOCAL_ONLY=1                        only on-machine providers
#
# The file wins where both are set, because a file is the form an administrator
# can own (root-owned, 0644) while an environment variable is inherited from
# whoever started the process — on a shared host, that may be the user.

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a)) b else a

# The providers whose text never leaves the machine.
#
# THIS LIST IS A CROSS-LANGUAGE CONTRACT. It must agree with
# `staysOnMachine()` in lib/llm.js, which is the browser's answer to the same
# question; test/ai-policy.test.mjs reads both and fails if they diverge.
# Note what is NOT here: `claudecode` and `codex` carry `local: true` in
# lib/llm.js because they cost no API key, but they hand the prompt to a CLI
# that sends it to a vendor. Using `local` to mean "stays here" would quietly
# ship the user's code out.
ON_MACHINE_PROVIDERS <- c("chrome", "webgpu", "ollama", "lmstudio", "jan")

# Providers reached with the user's API key — the ones the `ai-key` door serves.
KEY_PROVIDERS <- c("openai", "anthropic", "google", "groq", "together",
                   "openrouter", "azure", "custom")

# Providers that are a CLI process the SUPERVISOR spawns. The door it owns.
CLI_PROVIDERS <- c("claudecode", "codex")

ALL_PROVIDERS <- c(ON_MACHINE_PROVIDERS, KEY_PROVIDERS, CLI_PROVIDERS)

#' Normalise a policy file's `models` map into provider -> character vector.
#'
#' Accepts a single string as a one-element vector, because
#' `"anthropic": "claude-sonnet-4"` is what an administrator will write and
#' refusing it over a missing pair of brackets would be pedantry with a
#' startup failure attached.
normalise_models <- function(raw) {
  if (is.null(raw) || !length(raw)) return(list())
  keys <- names(raw)
  if (is.null(keys)) return(list())
  out <- list()
  for (key in keys[nzchar(keys)]) {
    ids <- unique(trimws(as.character(unlist(raw[[key]]))))
    ids <- ids[nzchar(ids) & !is.na(ids)]
    if (length(ids)) out[[key]] <- ids
  }
  out
}

policy_split <- function(value) {
  parts <- trimws(unlist(strsplit(value %||% "", "[,[:space:]]+")))
  parts[nzchar(parts)]
}

#' Read the administrator's AI policy.
#'
#' @param env a getter with `Sys.getenv`'s signature; injected for tests.
#' @return a list with `set` (is there a policy at all?), `source`,
#'   `providers` (the permitted ids — every id when unset), `local_only`,
#'   `note` (an administrator's sentence for the UI), `unknown` (ids named by
#'   the policy that CarmaR does not have, reported rather than silently
#'   dropped) and `errors`.
#'
#'   A policy that names NOTHING valid is an error, never an empty allow-list:
#'   silently permitting no provider would look identical to CarmaR's AI being
#'   broken, and an administrator would have no way to tell which it was.
carmar_ai_policy <- function(env = Sys.getenv) {
  path <- trimws(env("CARMAR_AI_POLICY", ""))
  named <- character(0)
  models <- list()
  local_only <- identical(env("CARMAR_AI_LOCAL_ONLY", ""), "1")
  note <- ""
  source <- ""
  errors <- character(0)

  if (nzchar(path)) {
    source <- path
    if (!file.exists(path)) {
      errors <- c(errors, paste0("CARMAR_AI_POLICY names a file that does not exist: ", path))
    } else {
      doc <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                      error = function(e) e)
      if (inherits(doc, "error")) {
        errors <- c(errors, paste0("CARMAR_AI_POLICY is not valid JSON: ", conditionMessage(doc)))
      } else {
        named <- as.character(doc$providers %||% character(0))
        if (isTRUE(doc$local_only)) local_only <- TRUE
        note <- trimws(as.character(doc$note %||% "")[1] %||% "")
        if (is.na(note)) note <- ""
        # `models` is FILE-ONLY, deliberately. A provider list flattens to a
        # comma-separated environment variable without losing anything; a map
        # of provider to permitted model ids does not, and every syntax that
        # crams one into a single string ("anthropic:a|b;openai:c") is a syntax
        # a typo hides in. An administrator pinning models is already writing a
        # file.
        models <- normalise_models(doc$models)
      }
    }
  } else if (nzchar(env("CARMAR_AI_PROVIDERS", ""))) {
    source <- "env"
    named <- policy_split(env("CARMAR_AI_PROVIDERS", ""))
  } else if (local_only) {
    source <- "env"
  }

  unknown <- setdiff(named, ALL_PROVIDERS)
  asked <- length(named) > 0L            # did the policy NAME anything at all?
  named <- intersect(named, ALL_PROVIDERS)

  # Three states, not two, and conflating the last two fails OPEN — which is
  # how a policy of `providers: ["anthropc"]` (one typo) used to permit every
  # provider on the machine instead of refusing to start:
  #   named nothing         → no constraint, everything is permitted
  #   named something valid → exactly that
  #   named ONLY junk       → an empty allow-list, which the check below turns
  #                           into a startup error rather than a silent "all".
  providers <- if (length(named)) named else if (asked) character(0) else ALL_PROVIDERS
  if (local_only) providers <- intersect(providers, ON_MACHINE_PROVIDERS)

  set <- nzchar(source) && !length(errors)
  if (set && !length(providers)) {
    errors <- c(errors, paste0(
      "The AI policy permits no provider at all",
      if (length(unknown)) paste0(" (unrecognised: ", paste(unknown, collapse = ", "), ")") else "",
      if (local_only) " — local_only was combined with providers that all send text off the machine" else "",
      ". Name at least one, or remove the policy."))
  }
  if (length(errors)) { set <- FALSE; providers <- ALL_PROVIDERS }

  # Control characters out: this string is rendered in the notebook and written
  # to the audit log, and an administrator's file is still a file.
  note <- gsub("[[:cntrl:]]", " ", note)
  note <- trimws(substr(note, 1L, 400L))

  # A models entry for a provider that is not permitted is dead weight, and
  # leaving it in would make the page render a constraint on a control the user
  # cannot reach anyway.
  models <- models[intersect(names(models), providers)]
  if (length(errors)) models <- list()

  list(set = set, source = source, providers = providers,
       models = models,
       local_only = local_only, note = note, unknown = unknown,
       errors = errors,
       # The two doors the supervisor actually owns, precomputed so the call
       # sites read as the questions they are asking.
       allows_key = length(intersect(providers, KEY_PROVIDERS)) > 0L,
       allows_cli = intersect(providers, CLI_PROVIDERS))
}

#' May this model be used with this provider?
#'
#' TRUE when no models are pinned for that provider — pinning is opt-in per
#' provider, so an administrator who names models for `anthropic` has not
#' thereby said anything about `lmstudio`.
#'
#' DISCLOSURE ONLY, and the difference from `ai_provider_allowed()` matters:
#' the provider half closes two doors the supervisor owns (it will not hand out
#' a key, it will not spawn a CLI). A model id is a field in a request body the
#' page builds and sends itself, so there is no door here to close — the kernel
#' states the constraint and the settings dialog offers only what is permitted.
#' Nothing stops a user with devtools, and nothing here pretends to.
ai_model_allowed <- function(provider, model, policy) {
  if (!isTRUE(policy$set)) return(TRUE)
  allowed <- policy$models[[provider %||% ""]]
  if (is.null(allowed) || !length(allowed)) return(TRUE)
  nzchar(model %||% "") && model %in% allowed
}

#' May this provider be used at all?
ai_provider_allowed <- function(provider, policy) {
  !isTRUE(policy$set) || (nzchar(provider %||% "") && provider %in% policy$providers)
}

#' The sentence the notebook shows when something is refused.
#'
#' Names the administrator's own note when there is one, because "your
#' administrator has limited this" without saying who or why is the worst
#' version of a policy message.
ai_policy_reason <- function(policy, what = "provider") {
  base <- sprintf("This CarmaR is configured to allow only: %s.",
                  paste(policy$providers, collapse = ", "))
  if (nzchar(policy$note)) paste(base, policy$note) else base
}
