#
# analyze.R — the ANALYSIS worker. It reads R; it never runs R.
#
# Stage 2 (docs/stages/stage-2-intelligence-v1.md): "intelligence must not
# depend on the execution kernel being idle". The evaluating session
# (spike/worker.R) services its stdin only between expressions, so while a cell
# is inside a 40-second model fit it answers nothing — and today that takes
# syntax checking, completion and hover down with it. This is the third
# process: same supervisor, same framing, no user code.
#
#   browser  ⇅  serve.R (supervisor)  ⇅  worker.R    — evaluates everything
#                                    ⇅  analyze.R    — evaluates NOTHING
#
# THE ONE RULE: nothing in this file may evaluate user source. `parse(text=)`
# builds a syntax tree without running a line of it, and that is the only thing
# here that touches the user's text. There is no eval(), no source(), no
# do.call on parsed input, and none may be added — the moment this process can
# run user code it becomes a second evaluating session with no Stop button and
# no session identity, which is exactly the door the deleted browser extension
# opened (see CLAUDE.md, "One transport").
#
# ── POSITIONS: R COUNTS CODE POINTS, THE BROWSER COUNTS UTF-16 UNITS ───────
# Measured, not assumed (the probe is reproduced in spike/test-analyze.R):
#
#     parse(text = 'x <- "\U0001F469" z')  ->  <text>:1:10: unexpected symbol
#
# That 10 is a 1-based CODE POINT column: the emoji counts once. The same
# position is UTF-16 index 10 (0-based) in JavaScript, because the emoji is a
# surrogate pair — so a column shipped as "a number" and consumed as an offset
# lands one unit early on every line containing an astral character, and worse
# the further along the line the error sits. Every position this file emits is
# therefore labelled with its unit (`unit: "codepoint"`, 1-based line and col)
# and MUST be converted by the consumer against the real text, not cast.
#
# R reports only the FIRST syntax error in a document (it stops there), so a
# reply carries at most one parse diagnostic. That is honest for v1: a second
# error cannot be trusted while the first one is unresolved anyway.

suppressPackageStartupMessages(stopifnot(requireNamespace("jsonlite", quietly = TRUE)))

# A private scope, for the same reason worker.R uses one: the protocol must not
# be clobberable, and nothing here should leak into any environment a future
# introspection command might report.
local({

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L, nzchar(args[1]))
sentinel <- args[1]

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Write one control frame: the session's random sentinel, then compact JSON.
#'
#' Identical framing to worker.R so the supervisor's existing reader works
#' unchanged. Nothing else is ever written to stdout by this process — unlike
#' the evaluating worker, which streams user output around its frames.
emit <- function(obj) {
  cat(sentinel, jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null",
                                 na = "null", digits = NA), "\n", sep = "")
  flush(stdout())
  invisible(NULL)
}

#' Pull `<text>:LINE:COL:` off a parser error message.
#'
#' R puts the position in the message text rather than in structured fields of
#' the condition, so this is the supported way to get it. Messages that carry
#' no position at all (e.g. "invalid multibyte character in parser") are real
#' and must still produce a diagnostic — they come back with `line`/`col` NULL
#' and the consumer places them on the whole document.
#'
#' @param msg conditionMessage() of a parse error.
#' @return list(line, col, text) — line/col integer or NULL, text the message
#'   with the position prefix and the parser's ASCII-art context stripped.
parse_error_position <- function(msg) {
  first <- strsplit(msg, "\n", fixed = TRUE)[[1L]]
  first <- if (length(first)) first[[1L]] else ""
  m <- regmatches(first, regexec("^<text>:([0-9]+):([0-9]+):[ ]*(.*)$", first))[[1L]]
  if (length(m) == 4L) {
    return(list(line = as.integer(m[[2L]]), col = as.integer(m[[3L]]),
                text = m[[4L]]))
  }
  list(line = NULL, col = NULL, text = first)
}

#' Would more input make this parse? (The console's continuation rule.)
#'
#' Kept identical to worker.R's emit_parse so the two never disagree about
#' what "incomplete" means — a REPL that thinks an expression is finished when
#' the analyzer thinks it is not would print the wrong prompt.
is_incomplete <- function(msg) {
  grepl("unexpected end of input|unexpected INCOMPLETE_STRING", msg)
}

#' Top-level names a document defines, with the line each sits on.
#'
#' Walks the PARSED expressions — never evaluates them — for `name <- ...`,
#' `name = ...` and `name <<- ...` at top level, which is what a notebook's
#' chunk graph and an outline are actually made of. Feeds completion ranking
#' now and document symbols in Stage 3.
#'
#' @param exprs Result of parse(keep.source = TRUE).
#' @return A list of list(name, line, kind).
top_symbols <- function(exprs) {
  refs <- attr(exprs, "srcref")
  out <- list()
  n <- length(exprs)
  if (n == 0L) return(out)
  for (i in seq_len(n)) {
    e <- exprs[[i]]
    if (!is.call(e) || length(e) < 3L) next
    op <- as.character(e[[1L]])
    if (!(op %in% c("<-", "=", "<<-"))) next
    lhs <- e[[2L]]
    if (!is.name(lhs)) next                       # skip f(x) <- and df$a <- forms
    rhs <- e[[3L]]
    is_fn <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    kind <- if (is_fn) "function" else "variable"
    line <- if (!is.null(refs) && length(refs) >= i && !is.null(refs[[i]])) {
      as.integer(refs[[i]][[1L]])
    } else {
      NA_integer_
    }
    # A function's ARGUMENT NAMES, straight off the parse tree. This is the
    # one piece of signature help that needs no session at all: `f <- function
    # (data, n = 10)` states its own formals, so completing `f(` and showing
    # `f(data, n = 10)` works with the worker busy, and works for a function
    # the user wrote thirty seconds ago and has not run yet — which is exactly
    # when a signature is most useful and least available from a live session.
    params <- character(0)
    signature <- NULL
    if (is_fn) {
      fmls <- tryCatch(as.list(rhs[[2L]]), error = function(e) NULL)
      if (!is.null(fmls) && length(fmls)) {
        params <- names(fmls)
        # An argument with no default holds the EMPTY SYMBOL, and binding that
        # to a variable raises "argument is missing" the moment it is used —
        # so it is tested and deparsed in place, never assigned. `deparse` on
        # the default expression, never eval: `n = nrow(d)` shows as written
        # rather than as whatever nrow(d) would return.
        parts <- vapply(seq_along(fmls), function(k) {
          if (is.symbol(fmls[[k]]) && !nzchar(as.character(fmls[[k]]))) params[[k]]
          else paste0(params[[k]], " = ", paste(deparse(fmls[[k]]), collapse = " "))
        }, character(1))
        signature <- paste0(as.character(lhs), "(", paste(parts, collapse = ", "), ")")
      } else {
        signature <- paste0(as.character(lhs), "()")
      }
    }
    out[[length(out) + 1L]] <- list(name = as.character(lhs), line = line, kind = kind,
                                    params = as.list(params), signature = signature)
  }
  out
}

#' Every symbol OCCURRENCE in a document, with its position and its role.
#'
#' This is the reference index Stage 3 navigates with and Stage 4 renames
#' through, and it comes from `getParseData()` rather than from a regex over
#' the text — which is the entire point. A text search for `f` finds it in
#' comments, inside strings, and in `d$f`; the parser knows which characters
#' are code and which are not, so strings and comments never enter the index
#' at all. Rename correctness is decided HERE, not in the rename.
#'
#' Roles, and why each is separate:
#'   def    `f <- ...` / `f = ...` at any depth — the thing to jump TO.
#'   call   `f(...)` — a use, but worth knowing it is a call.
#'   use    a plain mention of the value.
#'   formal `function(f)` — a BINDING, so it shadows the outer `f`.
#'   field  `d$f`, `f = 1` inside a call, `obj@f` — a name in ANOTHER
#'          namespace. It looks identical in the text and must never be
#'          renamed with the variable; keeping it in the index (rather than
#'          dropping it) lets a caller SHOW what it declined to touch.
#'
#' Columns are 1-based CODE POINTS, exactly like the parse-error position —
#' verified with the same astral probe (`getParseData` counts "👩" as one
#' column). The consumer converts; nothing here casts.
#'
#' @param exprs parse(keep.source = TRUE) result.
#' @return list of list(name, line, col, endCol, role).
references <- function(exprs) {
  pd <- tryCatch(utils::getParseData(exprs), error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(list())
  full <- pd                                    # the tree, for scope resolution
  pd <- pd[pd$terminal, , drop = FALSE]
  if (!nrow(pd)) return(list())
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]

  tok <- pd$token
  keep <- which(tok %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS", "SYMBOL_SUB"))
  if (!length(keep)) return(list())

  # The token before and after each kept one, ignoring comments — that is how
  # `d$f` (field) is told from `f` (variable) and `f <-` (definition) from a
  # bare mention. Both are decided by the neighbour, never by the spelling.
  code <- which(tok != "COMMENT")
  pos_in_code <- match(seq_len(nrow(pd)), code)
  prev_tok <- function(i) {
    k <- pos_in_code[[i]]
    if (is.na(k) || k <= 1L) "" else tok[[code[[k - 1L]]]]
  }
  next_tok <- function(i) {
    k <- pos_in_code[[i]]
    if (is.na(k) || k >= length(code)) "" else tok[[code[[k + 1L]]]]
  }

  scopes <- tryCatch(resolve_scopes(full, pd$id[keep]),
                     error = function(e) rep(0L, length(keep)))
  out <- list()
  for (n in seq_along(keep)) {
    i <- keep[[n]]
    before <- prev_tok(i)
    after <- next_tok(i)
    role <- if (identical(tok[[i]], "SYMBOL_FORMALS")) {
      "formal"
    } else if (identical(tok[[i]], "SYMBOL_SUB") || before %in% c("'$'", "'@'", "SLOT")) {
      "field"
    } else if (after %in% c("LEFT_ASSIGN", "EQ_ASSIGN") ||
               before %in% c("RIGHT_ASSIGN", "SUPER_RIGHT_ASSIGN")) {
      "def"
    } else if (identical(tok[[i]], "SYMBOL_FUNCTION_CALL")) {
      "call"
    } else {
      "use"
    }
    out[[length(out) + 1L]] <- list(
      name = pd$text[[i]], line = pd$line1[[i]], col = pd$col1[[i]],
      endCol = pd$col2[[i]] + 1L, role = role, unit = "codepoint",
      # 0 = the document's top level. Two occurrences are the SAME symbol only
      # when name and scope both match — the rule Stage 4's rename rests on.
      scope = scopes[[n]]
    )
  }
  out
}

#' Which BINDING each reference belongs to — the scope, resolved.
#'
#' Stage 4's rename exit criterion says unrelated locals and same-spelled
#' out-of-scope symbols must not be renamed. That cannot be decided from a
#' name: in
#'
#'     total <- 0
#'     f <- function(total) total + 1     # a DIFFERENT total
#'
#' there are two `total`s and renaming one must not touch the other. The parse
#' data carries the tree (`parent`), so the answer is computable rather than
#' guessable: walk each occurrence's ancestors outward and stop at the first
#' enclosing function that BINDS that name (as a formal or by assigning it);
#' if none does, the occurrence belongs to the document's top level.
#'
#' The result is a scope id per reference. Two occurrences may be renamed
#' together if and only if their name AND their scope id match. That rule is
#' the whole of rename safety, and it lives here rather than in the browser
#' because only the parser knows the tree.
#'
#' @param pd The FULL getParseData() output — terminals AND the interior
#'   nodes, because the parent chain runs through the interior ones. Handing
#'   this a terminal-only table silently resolves everything to the top level,
#'   which looks like working code and renames the wrong things.
#' @param ids Ids of the symbol occurrences to resolve.
#' @return integer vector, parallel to `ids`: the id of the binding function
#'   node, or 0 for the document's top level.
resolve_scopes <- function(pd, ids) {
  parent_of <- setNames(pd$parent, as.character(pd$id))
  # A function's node is the parent of its FUNCTION token — `function(a) a`
  # and `\(a) a` both produce one.
  fn_nodes <- unique(pd$parent[pd$token %in% c("FUNCTION", "OP-LAMBDA")])
  fn_nodes <- fn_nodes[fn_nodes != 0L]

  #' Ancestors of a node, innermost first, restricted to function nodes.
  fn_ancestors <- function(id) {
    out <- integer(0)
    cur <- id
    guard <- 0L
    while (!is.na(cur) && cur != 0L && guard < 1000L) {
      if (cur %in% fn_nodes) out <- c(out, cur)
      nxt <- parent_of[[as.character(cur)]]
      cur <- if (is.null(nxt) || is.na(nxt)) 0L else nxt
      guard <- guard + 1L
    }
    out
  }

  # What each function binds: its formals, plus every name assigned anywhere
  # inside it whose innermost enclosing function is that function.
  binds <- new.env(parent = emptyenv())
  add_bind <- function(node, name) {
    key <- as.character(node)
    cur <- if (exists(key, envir = binds, inherits = FALSE)) get(key, envir = binds) else character(0)
    assign(key, unique(c(cur, name)), envir = binds)
  }
  # "Is this an assignment?" is a question about the token SEQUENCE, and the
  # full table interleaves interior nodes with terminals — so the neighbour
  # test runs over a terminal-only, source-ordered view while the ancestor
  # walk keeps using the full tree. Mixing the two was a real bug: every
  # binding resolved to the top level, which looks exactly like working code.
  term <- pd[pd$terminal, , drop = FALSE]
  term <- term[order(term$line1, term$col1), , drop = FALSE]
  tok <- term$token
  code <- which(tok != "COMMENT")
  pos_in_code <- match(seq_len(nrow(term)), code)
  next_tok <- function(i) {
    k <- pos_in_code[[i]]
    if (is.na(k) || k >= length(code)) "" else tok[[code[[k + 1L]]]]
  }
  prev_tok <- function(i) {
    k <- pos_in_code[[i]]
    if (is.na(k) || k <= 1L) "" else tok[[code[[k - 1L]]]]
  }
  for (i in seq_len(nrow(term))) {
    is_formal <- identical(tok[[i]], "SYMBOL_FORMALS")
    is_def <- identical(tok[[i]], "SYMBOL") &&
      (next_tok(i) %in% c("LEFT_ASSIGN", "EQ_ASSIGN") ||
       prev_tok(i) %in% c("RIGHT_ASSIGN", "SUPER_RIGHT_ASSIGN")) &&
      !(prev_tok(i) %in% c("'$'", "'@'"))
    if (!is_formal && !is_def) next
    anc <- fn_ancestors(term$id[[i]])
    if (!length(anc)) next                        # a top-level binding
    # A formal belongs to ITS OWN function; a local assignment belongs to the
    # innermost function containing it. Both are `anc[[1]]`, because a
    # formal's ancestors start at the function it is declared on.
    add_bind(anc[[1L]], term$text[[i]])
  }

  row_of <- setNames(seq_len(nrow(pd)), as.character(pd$id))
  vapply(ids, function(id) {
    i <- row_of[[as.character(id)]]
    name <- pd$text[[i]]
    for (node in fn_ancestors(id)) {
      key <- as.character(node)
      if (exists(key, envir = binds, inherits = FALSE) &&
          name %in% get(key, envir = binds)) return(as.integer(node))
    }
    0L
  }, integer(1))
}

#' Analyze one document: syntax diagnostics, and symbols when it parses.
#'
#' @param source The document text.
#' @return list(diagnostics, symbols, complete).
analyze_source <- function(source) {
  src <- if (is.null(source)) "" else paste(as.character(source), collapse = "\n")
  diags <- list()
  symbols <- list()
  refs <- list()
  complete <- TRUE

  exprs <- tryCatch(
    parse(text = src, keep.source = TRUE),
    error = function(e) e,
    # A parse warning ("incomplete final line") is not a diagnostic users need.
    warning = function(w) invokeRestart("muffleWarning")
  )

  if (inherits(exprs, "error")) {
    msg <- conditionMessage(exprs)
    pos <- parse_error_position(msg)
    complete <- !is_incomplete(msg)
    diags[[1L]] <- list(
      severity = "error",
      rule = "parse",
      message = pos$text,
      raw = msg,                      # the parser's own words, for details
      # 1-based, CODE POINTS — see the header. `col` can be 0 when R points at
      # end-of-input, and `line` can be one past the last line for the same
      # reason; the consumer clamps against the real text.
      unit = "codepoint",
      line = pos$line, col = pos$col,
      # v1 range: the point itself. The browser widens it to the token under
      # that position with the tokenizer it already has (lib/r-highlight.js) —
      # R cannot tell us the token's extent from an error message, and guessing
      # a width here would underline the wrong text.
      endLine = pos$line, endCol = if (is.null(pos$col)) NULL else pos$col + 1L,
      incomplete = !complete
    )
  } else {
    symbols <- top_symbols(exprs)
    refs <- references(exprs)
  }

  list(diagnostics = diags, symbols = symbols, references = refs, complete = complete)
}

#' Every definition in the trusted `.R` files below a root.
#'
#' Stage 3, work item 3: workspace symbols. Three rules make this safe enough
#' to live in a process that is otherwise pure text analysis:
#'
#'   BOUNDED. Depth, file count and file size are all capped. An analyzer that
#'   walks an unbounded tree is a way to hang the supervisor's child on a home
#'   directory, and "the user pointed at a big folder" is not an error case
#'   worth crashing for.
#'
#'   READ, NEVER RUN. Files are read and PARSED. `source()` appears nowhere in
#'   this file and must not: a workspace scan that executed what it found would
#'   run arbitrary code because someone opened a folder.
#'
#'   ROOT-RELATIVE. Everything is reported relative to the root it was found
#'   under, and nothing above the root is ever visited — the trusted-root rule
#'   from lib/workdir.js, enforced where the reading happens rather than where
#'   the asking happens.
#'
#' @param root Directory to scan.
#' @param max_files,max_depth,max_bytes Caps.
#' @return list(files, symbols, truncated).
workspace_symbols <- function(root, max_files = 300L, max_depth = 4L,
                              max_bytes = 512000L) {
  if (is.null(root) || !nzchar(root) || !dir.exists(root)) {
    return(list(files = 0L, symbols = list(), truncated = FALSE,
                error = "no such directory"))
  }
  base <- normalizePath(root, winslash = "/", mustWork = TRUE)
  # Directories nobody wants indexed. Named rather than pattern-matched, so
  # adding one is a decision someone can read.
  skip <- c(".git", ".Rproj.user", "node_modules", "renv", "packrat",
            ".venv", "__pycache__", ".quarto", "_site", "dist")

  files <- character(0)
  walk <- function(dir, depth) {
    if (depth > max_depth || length(files) >= max_files) return(invisible(NULL))
    entries <- tryCatch(list.files(dir, all.files = FALSE, full.names = TRUE,
                                   no.. = TRUE), error = function(e) character(0))
    for (e in entries) {
      if (length(files) >= max_files) return(invisible(NULL))
      if (dir.exists(e)) {
        if (basename(e) %in% skip) next
        walk(e, depth + 1L)
      } else if (grepl("\\.(R|r)$", e)) {
        files <<- c(files, e)
      }
    }
    invisible(NULL)
  }
  walk(base, 1L)
  truncated <- length(files) >= max_files

  out <- list()
  for (f in files) {
    size <- tryCatch(file.info(f)$size, error = function(e) NA_real_)
    if (is.na(size) || size > max_bytes) next
    txt <- tryCatch(paste(readLines(f, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    if (is.null(txt)) next
    exprs <- tryCatch(parse(text = txt, keep.source = TRUE),
                      error = function(e) NULL, warning = function(w) invokeRestart("muffleWarning"))
    if (is.null(exprs)) next                       # a file that does not parse
    rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base), "/?"), "", f)
    for (sym in top_symbols(exprs)) {
      sym$path <- f
      sym$file <- rel
      out[[length(out) + 1L]] <- sym
    }
  }
  list(files = length(files), symbols = out, truncated = truncated, error = NULL)
}

# ── command loop ───────────────────────────────────────────────────────────
# NDJSON in, framed JSON out. Every reply echoes the request's `id`, `uri` and
# `version` so a consumer can discard a stale answer by strict version equality
# without keeping a side table (stage-0-editor-adapter.md's stale rule).
con <- file("stdin", open = "rt", encoding = "UTF-8")
emit(list(type = "analyze-ready", pid = Sys.getpid(),
          rversion = paste0(R.version$major, ".", R.version$minor)))

repeat {
  line <- tryCatch(readLines(con, n = 1L, warn = FALSE),
                   interrupt = function(i) NA_character_)
  if (length(line) == 0L) break                        # EOF: the supervisor left
  if (length(line) != 1L || is.na(line) || !nzchar(line)) next
  cmd <- tryCatch(jsonlite::fromJSON(line, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(cmd) || is.null(cmd$type)) next
  type <- as.character(cmd$type)[[1L]]
  id <- if (is.null(cmd$id)) NULL else as.character(cmd$id)[[1L]]

  if (identical(type, "ping")) {
    emit(list(type = "pong", id = id))
    next
  }
  if (identical(type, "analyze")) {
    res <- tryCatch(
      analyze_source(cmd$source),
      # A crash in analysis must not take the process down: the editor would
      # lose diagnostics for the rest of the session over one odd document.
      error = function(e) list(diagnostics = list(), symbols = list(),
                               references = list(), complete = TRUE,
                               failed = conditionMessage(e))
    )
    emit(list(type = "analyze", id = id,
              uri = cmd$uri %||% NULL,
              version = cmd$version %||% NULL,
              diagnostics = res$diagnostics, symbols = res$symbols,
              references = res$references, complete = res$complete,
              failed = res$failed %||% NULL))
    next
  }
  if (identical(type, "workspace")) {
    res <- tryCatch(
      workspace_symbols(if (is.null(cmd$root)) "" else as.character(cmd$root)[[1L]]),
      error = function(e) list(files = 0L, symbols = list(), truncated = FALSE,
                               error = conditionMessage(e))
    )
    emit(list(type = "workspace", id = id, root = cmd$root %||% NULL,
              files = res$files, symbols = res$symbols,
              truncated = res$truncated, error = res$error %||% NULL))
    next
  }
  # An unknown command is answered, not ignored: a consumer waiting on an id
  # must never hang because it asked for something this build does not have.
  emit(list(type = "analyze-error", id = id, message = paste0("unknown command: ", type)))
}

})
