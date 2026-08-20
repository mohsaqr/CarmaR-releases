# sniff.R — format and column-type detection for the import wizard.
#
# Pure functions, no I/O side effects beyond reading the file it is asked
# about, and no `emit`. worker.R sources this into its private scope; the test
# sources it into a plain environment. Nothing here touches globalenv().
#
# THE PRINCIPLE: read every column as `character`, then decide. R's own
# `read.csv` decides during the read, where a wrong guess is silent and
# unrecoverable — a column of "03/04/2024" becomes a factor or a string and
# the date is gone; a column of "1,5" becomes character and the number is
# gone. Detection on raw text is inspectable, overridable, and reversible.
#
# THE OTHER PRINCIPLE: a format is accepted only if it explains EVERY value.
# strptime ignores trailing characters — as.Date("2024-03-04 10:00", "%Y-%m-%d")
# returns a valid date and throws the time away without a word. So every
# candidate format is first shape-gated by an anchored regex built from the
# format itself, and only then parsed. That single rule is the difference
# between detection you can trust and RStudio's guess.

# ── the NA vocabulary ───────────────────────────────────────────────────────
# The tokens that mean "missing" in files people actually receive. Detected,
# reported, and written into the generated code as `na.strings` — never
# applied invisibly.
NA_TOKENS <- c("", "NA", "N/A", "n/a", "#N/A", "#NA", "NULL", "null", "None",
               "none", "nan", "NaN", ".", "-", "--", "?", "missing", "MISSING",
               "Missing", "unknown", "Unknown", "<NA>")

LOGICAL_TRUE <- c("TRUE", "true", "True", "T", "YES", "yes", "Yes", "Y")
LOGICAL_FALSE <- c("FALSE", "false", "False", "F", "NO", "no", "No", "N")

DELIMITERS <- list(
  list(char = ",",  name = "comma"),
  list(char = ";",  name = "semicolon"),
  list(char = "\t", name = "tab"),
  list(char = "|",  name = "pipe"),
  list(char = " ",  name = "space")
)

MAX_SNIFF_LINES <- 500L      # lines read to decide format
MAX_SNIFF_VALUES <- 5000L    # values per column examined for type
MAX_PREVIEW_ROWS <- 30L      # rows returned to the wizard
MAX_PREVIEW_COLS <- 60L
MAX_EXAMPLES <- 4L

`%||%` <- function(a, b) if (is.null(a)) b else a

REGEX_META <- c(".", "^", "$", "|", "(", ")", "[", "]", "{", "}", "*", "+", "?", "\\")

# ── format → anchored regex ────────────────────────────────────────────────

#' The regex that a string must match before a format is even tried.
#'
#' strptime accepts trailing garbage, so "2024-03-04 10:00:00" parses cleanly
#' as "%Y-%m-%d" and the time vanishes. Anchoring a shape built from the same
#' format string closes that hole, and it closes it for every format at once
#' rather than one special case at a time.
#'
#' @param fmt A strptime format string.
#' @return A single anchored regex, or NA when the format uses a directive
#'   this builder does not model (the caller then declines the candidate).
format_regex <- function(fmt) {
  stopifnot(is.character(fmt), length(fmt) == 1L)
  pieces <- list(
    "%Y" = "[0-9]{4}", "%y" = "[0-9]{2}",
    "%m" = "[0-9]{1,2}", "%d" = "[0-9]{1,2}", "%e" = "[ 0-9][0-9]",
    "%H" = "[0-9]{1,2}", "%I" = "[0-9]{1,2}",
    "%M" = "[0-9]{2}", "%S" = "[0-9]{2}",
    "%OS" = "[0-9]{2}(?:[.,][0-9]+)?",
    "%j" = "[0-9]{3}",
    "%b" = "[A-Za-z]{3,9}[.]?", "%B" = "[A-Za-z]{3,9}",
    "%p" = "[AaPp][.]?[Mm][.]?",
    "%z" = "(?:Z|[+-][0-9]{2}:?[0-9]{2})",
    "%Z" = "[A-Za-z/_+-]{1,32}"
  )
  # Longest directives first: %OS must win over %O/%S, %B over %b's shape.
  keys <- names(pieces)[order(-nchar(names(pieces)))]
  out <- ""
  rest <- fmt
  while (nzchar(rest)) {
    if (substr(rest, 1L, 1L) == "%") {
      hit <- Find(function(k) startsWith(rest, k), keys)
      if (is.null(hit)) return(NA_character_)          # unmodelled directive
      out <- paste0(out, pieces[[hit]])
      rest <- substring(rest, nchar(hit) + 1L)
    } else {
      ch <- substr(rest, 1L, 1L)
      # A literal space in a format matches run-of-whitespace in real files.
      # Escaping by MEMBERSHIP rather than by a regex over regex metacharacters:
      # the pattern that quotes `{` is itself invalid in TRE, which is exactly
      # the kind of nested-escaping bug that hides until a "%B %d, %Y" shows up.
      out <- paste0(out, if (ch == " ") "[[:space:]]+"
                         else if (ch %in% REGEX_META) paste0("\\", ch)
                         else ch)
      rest <- substring(rest, 2L)
    }
  }
  paste0("^", out, "$")
}

#' Does this format explain every value in `x`?
#'
#' All-or-nothing by design: a format that parses 97% of a column is not the
#' column's format, it is a coincidence plus three rows of damage.
#'
#' @param x Character vector, already stripped of NA tokens.
#' @param fmt strptime format.
#' @param kind "Date", "POSIXct" or "time".
#' @param tz Timezone used for the POSIXct trial parse.
#' @return TRUE when the shape matches everywhere AND every value parses.
format_explains <- function(x, fmt, kind = "Date", tz = "UTC") {
  if (!length(x)) return(FALSE)
  rx <- format_regex(fmt)
  if (is.na(rx)) return(FALSE)
  if (!all(grepl(rx, x, perl = TRUE))) return(FALSE)
  parsed <- tryCatch({
    if (identical(kind, "Date")) as.Date(x, format = fmt)
    else as.POSIXct(strptime(x, format = fmt, tz = tz), tz = tz)
  }, error = function(e) NULL, warning = function(w) NULL)
  !is.null(parsed) && !anyNA(parsed)
}

# ── the format battery ──────────────────────────────────────────────────────

DATE_FORMATS <- c(
  # ISO and other year-first shapes: unambiguous, so they go first.
  "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d", "%Y-%j",
  # Day-first and month-first are listed as a PAIR; when both survive the
  # column is genuinely ambiguous and the caller is told so.
  "%d/%m/%Y", "%m/%d/%Y",
  "%d-%m-%Y", "%m-%d-%Y",
  "%d.%m.%Y", "%m.%d.%Y",
  "%d/%m/%y", "%m/%d/%y", "%y-%m-%d", "%y/%m/%d",
  # Month names — unambiguous whichever side they sit on.
  "%d %b %Y", "%d %B %Y", "%b %d %Y", "%B %d %Y",
  "%b %d, %Y", "%B %d, %Y", "%d-%b-%Y", "%d-%b-%y", "%b-%Y", "%B %Y"
)

TIME_SUFFIXES <- c(
  " %H:%M:%S", "T%H:%M:%S", " %H:%M", "T%H:%M",
  " %H:%M:%OS", "T%H:%M:%OS",
  " %H:%M:%SZ", "T%H:%M:%SZ", "T%H:%M:%OSZ",
  " %H:%M:%S%z", "T%H:%M:%S%z", "T%H:%M:%OS%z",
  " %H:%M:%S %Z", " %I:%M %p", " %I:%M:%S %p"
)

TIME_ONLY_FORMATS <- c("%H:%M:%S", "%H:%M", "%H:%M:%OS", "%I:%M %p", "%I:%M:%S %p")

#' Every datetime format worth trying, cheapest-shape-first.
#' The cross product is large, so callers shape-gate on ONE value before
#' spending an all-or-nothing pass over the sample.
datetime_formats <- function() {
  as.vector(t(outer(DATE_FORMATS, TIME_SUFFIXES, paste0)))
}

#' The pair a day/month ambiguity is made of, if this format is half of one.
#' @return The other format, or NA when the format cannot be ambiguous.
ambiguous_twin <- function(fmt) {
  pairs <- c("%d/%m/%Y" = "%m/%d/%Y", "%m/%d/%Y" = "%d/%m/%Y",
             "%d-%m-%Y" = "%m-%d-%Y", "%m-%d-%Y" = "%d-%m-%Y",
             "%d.%m.%Y" = "%m.%d.%Y", "%m.%d.%Y" = "%d.%m.%Y",
             "%d/%m/%y" = "%m/%d/%y", "%m/%d/%y" = "%d/%m/%y")
  base_fmt <- sub("[ T].*$", "", fmt)
  # match(), not `[[`: a named character vector THROWS on a missing name
  # rather than returning NULL, so `%||%` never gets the chance to catch it.
  idx <- match(base_fmt, names(pairs))
  if (is.na(idx)) return(NA_character_)
  twin <- unname(pairs[idx])
  sub(base_fmt, twin, fmt, fixed = TRUE)
}

# ── numbers ─────────────────────────────────────────────────────────────────

#' Strip the decoration real spreadsheets put on numbers.
#'
#' @param x Character vector.
#' @param decimal "." or "," — the mark that survives; the other is grouping.
#' @return Character vector ready for as.numeric().
undecorate_number <- function(x, decimal = ".") {
  out <- trimws(x)
  out <- gsub("^[(]([^)]*)[)]$", "-\\1", out)         # (1 234) accounting negative
  out <- gsub("[   [:space:]]", "", out)  # thin/no-break spaces
  out <- gsub("[$€£¥%]", "", out)      # currency and percent
  if (identical(decimal, ",")) {
    out <- gsub(".", "", out, fixed = TRUE)
    out <- gsub(",", ".", out, fixed = TRUE)
  } else {
    out <- gsub(",", "", out, fixed = TRUE)
  }
  out
}

#' Is every value a number once the decoration is removed?
#' @return list(ok, decimal, decorated) — `decorated` says the plain read
#'   would have failed, which is what the generated code has to handle.
numeric_reading <- function(x) {
  plain <- suppressWarnings(as.numeric(x))
  if (!anyNA(plain)) return(list(ok = TRUE, decimal = ".", decorated = FALSE))
  try_mark <- function(mark) {
    v <- suppressWarnings(as.numeric(undecorate_number(x, mark)))
    !anyNA(v)
  }
  # Comma-decimal is tried first only when a comma is actually present as the
  # LAST separator — otherwise "1,234" (a US thousand) would read as 1.234.
  comma_decimal <- all(grepl("^[^,]*,[0-9]+$", trimws(x)))
  if (comma_decimal && try_mark(",")) return(list(ok = TRUE, decimal = ",", decorated = TRUE))
  if (try_mark(".")) return(list(ok = TRUE, decimal = ".", decorated = TRUE))
  if (try_mark(",")) return(list(ok = TRUE, decimal = ",", decorated = TRUE))
  list(ok = FALSE, decimal = ".", decorated = FALSE)
}

# ── one column ──────────────────────────────────────────────────────────────

#' Guess what a column of raw text actually is.
#'
#' @param x Character vector as read from the file (NA tokens still present).
#' @param name The column's name, used only for date-ish hints on numbers.
#' @param na_strings Tokens treated as missing.
#' @param tz Timezone assumed for naive timestamps.
#' @return A list describing the column: `type`, `format`, `tz`, `decimal`,
#'   `ambiguous`, `alternatives`, counts and examples. Every field the wizard
#'   shows and every field the code generator needs.
guess_column <- function(x, name = "", na_strings = NA_TOKENS, tz = "UTC") {
  x <- as.character(x)
  n_total <- length(x)
  is_na <- is.na(x) | trimws(x) %in% na_strings
  seen_na <- unique(trimws(x[is_na & !is.na(x)]))
  xs <- trimws(x[!is_na])
  if (length(xs) > MAX_SNIFF_VALUES) xs <- xs[seq_len(MAX_SNIFF_VALUES)]

  out <- list(
    name = name, type = "character", format = NULL, tz = NULL, decimal = NULL,
    decorated = FALSE, strip = FALSE, pin = FALSE, ambiguous = FALSE,
    alternatives = I(list()),
    n_total = n_total, n_missing = sum(is_na), n_checked = length(xs),
    n_distinct = length(unique(xs)),
    # I(): jsonlite's auto_unbox turns a length-1 vector into a SCALAR, and a
    # client doing `examples.join(...)` then gets a string and throws. The
    # worker already learned this once (emit_dataframe marks columns/types
    # the same way). Every field the wire promises as an array is marked, so
    # the promise holds at length 0, 1 and many.
    na_tokens = I(as.character(sort(seen_na))),
    examples = I(as.character(utils::head(unique(xs), MAX_EXAMPLES))),
    note = NULL
  )
  if (!length(xs)) {
    out$note <- "every value is missing"
    return(as_wire_column(out))
  }

  # 1. logical — spelled words only. 0/1 is a number until someone says
  #    otherwise; silently turning a count into TRUE/FALSE is not a guess a
  #    reader can undo.
  if (all(xs %in% c(LOGICAL_TRUE, LOGICAL_FALSE))) {
    out$type <- "logical"
    return(as_wire_column(out))
  }

  # 2. identifiers that merely LOOK numeric. Two ways a number destroys an id,
  #    both silent and both common:
  #      * leading zeros carry meaning — 007, postal codes, gene ids, Finnish
  #        municipality codes — and reading them as numbers deletes them;
  #      * a digit string longer than a double holds exactly (2^53) comes back
  #        rounded, so the last digits of a long accession or account number
  #        quietly change.
  #    Both stay text, with the numeric reading offered rather than taken.
  digits_only <- gsub("[^0-9]", "", xs)
  looks_id <- any(grepl("^[+-]?0[0-9]", xs)) || any(nchar(digits_only) > 15L)
  if (looks_id && !anyNA(suppressWarnings(as.numeric(xs)))) {
    out$type <- "character"
    # A STRUCTURED flag, not a sentence. The code generator has to decide
    # whether to pin this column's class, and deciding it by matching the
    # wording of a human-readable note is how "007" quietly became 7 again.
    out$pin <- TRUE
    out$note <- if (any(grepl("^[+-]?0[0-9]", xs))) {
      "leading zeros — kept as text, because reading it as a number deletes them"
    } else {
      "too many digits for a number to hold exactly — kept as text"
    }
    out$alternatives <- c(out$alternatives, list(list(
      type = "numeric", label = "number (drops leading zeros)", suggested = FALSE)))
    return(as_wire_column(out))
  }

  # 3. numbers, including the decorated ones a plain read would drop.
  num <- numeric_reading(xs)
  if (num$ok) {
    values <- suppressWarnings(as.numeric(undecorate_number(xs, num$decimal)))
    whole <- all(values == floor(values)) && all(abs(values) <= .Machine$integer.max)
    out$type <- if (whole && !any(grepl("[.]", xs))) "integer" else "numeric"
    out$decimal <- num$decimal
    out$decorated <- num$decorated
    # Does `dec = ","` alone rescue this column, or does it need characters
    # stripped out? The distinction decides whether the generated code is one
    # clean read argument or a visible repair step, so it is settled here
    # rather than guessed by the code generator.
    out$strip <- num$decorated && anyNA(suppressWarnings(as.numeric(
      if (identical(num$decimal, ",")) sub(",", ".", xs, fixed = TRUE) else xs)))
    # A number that is really a date, offered but never imposed: Excel ships
    # dates as days since 1899-12-30 and the column looks like 45000.
    date_ish <- grepl("date|day|dob|birth|time|stamp", name, ignore.case = TRUE)
    if (whole && all(values >= 1) && all(values <= 80000)) {
      out$alternatives <- c(out$alternatives, list(list(
        type = "excel_date", label = "Excel serial date (days since 1899-12-30)",
        suggested = date_ish)))
    }
    if (all(values >= 5e8) && all(values <= 4e9)) {
      out$alternatives <- c(out$alternatives, list(list(
        type = "epoch_seconds", label = "Unix time (seconds since 1970-01-01)",
        suggested = date_ish)))
    }
    if (all(values >= 5e11) && all(values <= 4e12)) {
      out$alternatives <- c(out$alternatives, list(list(
        type = "epoch_millis", label = "Unix time (milliseconds since 1970-01-01)",
        suggested = date_ish)))
    }
    return(as_wire_column(out))
  }

  # 4. dates and timestamps. Shape-gate on one value, then demand the format
  #    explain the whole sample.
  probe <- xs[1L]
  shape_ok <- function(fmt) {
    rx <- format_regex(fmt)
    !is.na(rx) && grepl(rx, probe, perl = TRUE)
  }
  accept <- function(fmt, kind) {
    out$type <<- kind
    out$format <<- fmt
    if (identical(kind, "POSIXct")) {
      # A naive timestamp has no timezone in it, so one is CHOSEN here and
      # written into the generated code where the reader can see it. R's
      # default (tz = "") silently means "this machine, today" — the same file
      # then reads differently in Helsinki and in Boston.
      out$tz <<- if (grepl("%z|Z$", fmt)) "UTC" else tz
    }
    twin <- ambiguous_twin(fmt)
    if (!is.na(twin) && format_explains(xs, twin, kind, tz)) {
      out$ambiguous <<- TRUE
      out$alternatives <<- c(out$alternatives, list(list(
        type = kind, format = twin, label = describe_format(twin), suggested = FALSE)))
      out$note <<- sprintf(
        "Ambiguous: every value fits both %s and %s. No value has a day above 12, so the file itself cannot settle it — choose.",
        describe_format(fmt), describe_format(twin))
    }
    as_wire_column(out)
  }

  dt_candidates <- Filter(shape_ok, datetime_formats())
  hit <- Find(function(f) format_explains(xs, f, "POSIXct", tz), dt_candidates)
  if (!is.null(hit)) return(accept(hit, "POSIXct"))

  d_candidates <- Filter(shape_ok, DATE_FORMATS)
  hit <- Find(function(f) format_explains(xs, f, "Date", tz), d_candidates)
  if (!is.null(hit)) return(accept(hit, "Date"))

  t_candidates <- Filter(shape_ok, TIME_ONLY_FORMATS)
  hit <- Find(function(f) format_explains(xs, f, "POSIXct", tz), t_candidates)
  if (!is.null(hit)) {
    out$type <- "time"
    out$format <- hit
    out$note <- "clock time with no date — read as a duration since midnight"
    return(as_wire_column(out))
  }

  # 5. character. A short, repeating vocabulary is offered as a factor, never
  #    imposed: stringsAsFactors bit a generation of R users precisely because
  #    it was the default rather than a decision.
  if (out$n_distinct <= 25L && length(xs) >= 20L && out$n_distinct / length(xs) < 0.5) {
    out$alternatives <- c(out$alternatives, list(list(
      type = "factor", label = sprintf("factor (%d levels)", out$n_distinct),
      suggested = FALSE)))
  }
  as_wire_column(out)
}

#' Re-assert the array shape of every list field after the appends above.
#'
#' `c(x, list(y))` drops the AsIs class, so marking `alternatives` once at
#' construction is not enough — it has to be re-marked at every exit.
as_wire_column <- function(col) {
  col$alternatives <- I(unname(as.list(col$alternatives)))
  col$examples <- I(as.character(col$examples))
  col$na_tokens <- I(as.character(col$na_tokens))
  col
}

#' A format string, in words — the wizard shows this, not "%d/%m/%Y".
describe_format <- function(fmt) {
  if (is.null(fmt) || is.na(fmt)) return("")
  words <- fmt
  subs <- c("%Y" = "YYYY", "%y" = "YY", "%m" = "MM", "%d" = "DD", "%e" = "D",
            "%H" = "hh", "%I" = "hh", "%M" = "mm", "%S" = "ss", "%OS" = "ss.s",
            "%b" = "Mon", "%B" = "Month", "%p" = "AM/PM", "%z" = "+ZZZZ",
            "%Z" = "TZ", "%j" = "DDD")
  keys <- names(subs)[order(-nchar(names(subs)))]
  Reduce(function(acc, k) gsub(k, subs[[k]], acc, fixed = TRUE), keys, words)
}

# ── the file ────────────────────────────────────────────────────────────────

#' Encoding, by evidence rather than hope.
#' @return list(encoding, bom) — `bom` is the byte count to skip.
sniff_encoding <- function(path) {
  raw_head <- readBin(path, "raw", n = 65536L)
  if (length(raw_head) >= 3L && identical(as.integer(raw_head[1:3]), c(239L, 187L, 191L))) {
    return(list(encoding = "UTF-8", bom = 3L))
  }
  if (length(raw_head) >= 2L && identical(as.integer(raw_head[1:2]), c(255L, 254L))) {
    return(list(encoding = "UTF-16LE", bom = 2L))
  }
  if (length(raw_head) >= 2L && identical(as.integer(raw_head[1:2]), c(254L, 255L))) {
    return(list(encoding = "UTF-16BE", bom = 2L))
  }
  txt <- tryCatch(rawToChar(raw_head), error = function(e) "")
  Encoding(txt) <- "UTF-8"
  # validUTF8 on the whole blob: a single invalid byte means this is not UTF-8,
  # and latin1 is the overwhelmingly likely alternative for CSVs in the wild.
  list(encoding = if (validUTF8(txt)) "UTF-8" else "latin1", bom = 0L)
}

#' Which delimiter makes this file rectangular?
#'
#' Scored on CONSISTENCY, not frequency: the right delimiter is the one that
#' gives every line the same field count, which is a property a comma inside
#' quoted prose cannot fake. count.fields does the quote-aware counting — the
#' same parser read.table itself uses, so agreement is guaranteed.
#'
#' @return list(delim, name, fields, confidence)
sniff_delimiter <- function(lines, quote = "\"") {
  score_one <- function(d) {
    counts <- tryCatch(
      utils::count.fields(textConnection(lines), sep = d$char, quote = quote,
                          blank.lines.skip = TRUE, comment.char = ""),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(counts) || !length(counts)) return(NULL)
    modal <- as.integer(names(sort(table(counts), decreasing = TRUE))[1L])
    if (is.na(modal) || modal < 2L) return(NULL)
    list(delim = d$char, name = d$name, fields = modal,
         confidence = mean(counts == modal))
  }
  scored <- Filter(Negate(is.null), lapply(DELIMITERS, score_one))
  if (!length(scored)) {
    return(list(delim = ",", name = "comma", fields = 1L, confidence = 0))
  }
  # More columns breaks a tie between two perfectly consistent delimiters:
  # a semicolon file also reads "consistently" as one comma-free column.
  best <- scored[[which.max(vapply(scored, function(s)
    s$confidence * 1000 + min(s$fields, 50L), numeric(1)))]]
  best
}

#' Does the first row name the columns, or is it data?
#'
#' The test that actually discriminates: a header row is all text, while the
#' body has at least one field that is NOT text. A file that is character
#' throughout falls back to "distinct, non-empty, no leading digits" — weaker,
#' and reported as such so the wizard can show it as a guess.
sniff_header <- function(first, rest) {
  if (!length(first)) return(TRUE)
  looks_data <- function(v) {
    v <- trimws(v[!is.na(v) & nzchar(trimws(v))])
    if (!length(v)) return(FALSE)
    num <- suppressWarnings(as.numeric(undecorate_number(v)))
    any(!is.na(num)) || any(vapply(DATE_FORMATS, function(f)
      format_explains(v, f, "Date"), logical(1)))
  }
  if (!length(rest) || !nrow(rest)) return(!looks_data(first))
  body_typed <- any(vapply(seq_along(first), function(i)
    looks_data(rest[, i]), logical(1)))
  if (looks_data(first)) return(FALSE)
  if (body_typed) return(TRUE)
  all(nzchar(trimws(first))) && !anyDuplicated(first)
}

#' Everything the wizard needs to describe a delimited file.
#' @return A list: format, settings, columns, preview, and how it was decided.
sniff_delimited <- function(path, opts = list()) {
  enc <- sniff_encoding(path)
  encoding <- opts$encoding %||% enc$encoding
  con <- file(path, open = "r", encoding = encoding)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, n = MAX_SNIFF_LINES, warn = FALSE)
  if (enc$bom > 0L && length(lines)) {
    lines[1L] <- sub("^﻿", "", lines[1L])
  }
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    # A second attempt with no declared encoding, because a wrong guess must
    # not be the reason a perfectly good file "has no lines". Then, if it is
    # still empty, say WHICH kind of empty — a zero-byte file and a cloud
    # placeholder that has not downloaded yet are different problems.
    con2 <- file(path, open = "r")
    lines <- tryCatch(readLines(con2, n = MAX_SNIFF_LINES, warn = FALSE),
                      error = function(e) character(0))
    close(con2)
    lines <- lines[nzchar(lines)]
  }
  if (!length(lines)) {
    size <- as.numeric(file.info(path)$size)
    stop(if (is.na(size) || size == 0) "the file is empty (0 bytes)"
         else sprintf("no readable text in %.0f KB - if this lives in iCloud or Google Drive it may not be downloaded yet",
                      size / 1024))
  }

  # Leading commentary — the "# exported from ..." banner spreadsheets add.
  comment_lines <- which(grepl("^\\s*#", lines))
  skip <- if (length(comment_lines) && all(comment_lines == seq_along(comment_lines))) {
    length(comment_lines)
  } else 0L
  skip <- opts$skip %||% skip
  body <- if (skip > 0L) lines[-seq_len(skip)] else lines

  quote <- opts$quote %||% "\""
  det <- if (!is.null(opts$delim)) {
    list(delim = opts$delim, name = "chosen", fields = NA_integer_, confidence = 1)
  } else sniff_delimiter(body, quote)

  raw <- utils::read.table(
    text = paste(body, collapse = "\n"), sep = det$delim, quote = quote,
    header = FALSE, colClasses = "character", stringsAsFactors = FALSE,
    na.strings = character(0), comment.char = "", check.names = FALSE,
    fill = TRUE, blank.lines.skip = TRUE)

  header <- opts$header %||% sniff_header(as.character(unlist(raw[1L, ])),
                                          if (nrow(raw) > 1L) raw[-1L, , drop = FALSE] else raw[0, , drop = FALSE])
  if (isTRUE(header)) {
    nms <- as.character(unlist(raw[1L, ]))
    data <- raw[-1L, , drop = FALSE]
  } else {
    nms <- paste0("V", seq_len(ncol(raw)))
    data <- raw
  }
  nms[is.na(nms) | !nzchar(trimws(nms))] <- ""
  blank <- !nzchar(nms)
  nms[blank] <- paste0("V", which(blank))

  na_strings <- opts$naStrings %||% NA_TOKENS
  tz <- opts$tz %||% "UTC"
  keep <- seq_len(min(ncol(data), MAX_PREVIEW_COLS))
  cols <- lapply(keep, function(i)
    guess_column(data[[i]], nms[i], na_strings = na_strings, tz = tz))

  list(
    format = "delimited",
    settings = list(
      delim = det$delim, delimName = det$name, quote = quote,
      encoding = encoding, header = isTRUE(header), skip = skip,
      naStrings = I(as.character(na_strings)), tz = tz,
      # What DETECTION used, and what the generated code should say. Keeping
      # them apart stops a re-sniff from narrowing its own vocabulary.
      naObserved = I(observed_na_tokens(cols)),
      confidence = det$confidence
    ),
    columns = I(unname(cols)),
    names = I(as.character(nms[keep])),
    preview = I(preview_rows(data[keep], nms[keep])),
    nrow = nrow(data), ncol = ncol(data),
    # `nrow` is what was SAMPLED. A 400 KB file sampled at 500 lines was
    # reporting "499 rows" in the badge, which is a lie about the file rather
    # than a description of the sample — so the true count travels with it.
    totalRows = count_data_rows(path, header = isTRUE(header), skip = skip),
    truncated = length(lines) >= MAX_SNIFF_LINES
  )
}

#' How many data rows the file actually has.
#'
#' Counted in chunks so a large file costs a scan and not its size in memory,
#' and capped so a pathological one cannot become an unbounded wait. NA means
#' "more than the cap", which the caller renders as "500,000+".
#'
#' @return Integer row count excluding the header, or NA past the cap.
COUNT_ROW_CAP <- 5e6
count_data_rows <- function(path, header = TRUE, skip = 0L) {
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  n <- 0
  repeat {
    chunk <- tryCatch(readLines(con, n = 50000L, warn = FALSE),
                      error = function(e) character(0))
    if (!length(chunk)) break
    n <- n + sum(nzchar(chunk))
    if (n > COUNT_ROW_CAP) return(NA_integer_)
  }
  as.integer(max(0, n - skip - (if (isTRUE(header)) 1L else 0L)))
}

#' The first rows, as plain strings, for the wizard\'s grid.
preview_rows <- function(data, nms) {
  if (!ncol(data)) return(list())
  head_df <- utils::head(data, MAX_PREVIEW_ROWS)
  lapply(seq_len(nrow(head_df)), function(i)
    as.list(stats::setNames(as.character(unlist(head_df[i, ], use.names = FALSE)), nms)))
}

#' An Excel workbook: which sheets it has, and what the chosen one holds.
sniff_excel <- function(path, opts = list()) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is not installed - install.packages(\"readxl\") to import Excel files")
  }
  sheets <- readxl::excel_sheets(path)
  sheet <- opts$sheet %||% sheets[1L]
  skip <- opts$skip %||% 0L
  # col_types = "text" is the same discipline as colClasses = "character":
  # readxl's own guessing is what turns a mixed column into NAs.
  raw <- as.data.frame(readxl::read_excel(path, sheet = sheet, skip = skip,
                                          col_types = "text", .name_repair = "minimal"),
                       stringsAsFactors = FALSE)
  nms <- names(raw)
  nms[is.na(nms) | !nzchar(trimws(nms))] <- ""
  blank <- !nzchar(nms)
  nms[blank] <- paste0("V", which(blank))
  na_strings <- opts$naStrings %||% NA_TOKENS
  tz <- opts$tz %||% "UTC"
  keep <- seq_len(min(ncol(raw), MAX_PREVIEW_COLS))
  cols <- lapply(keep, function(i)
    guess_column(raw[[i]], nms[i], na_strings = na_strings, tz = tz))
  list(
    format = "excel",
    settings = list(sheet = sheet, sheets = as.list(sheets), skip = skip,
                    naStrings = I(as.character(na_strings)), tz = tz, header = TRUE),
    columns = I(unname(cols)),
    names = I(as.character(nms[keep])),
    preview = I(preview_rows(raw[keep], nms[keep])),
    nrow = nrow(raw), ncol = ncol(raw), truncated = FALSE
  )
}

#' A serialized R object — nothing to detect, everything already typed.
sniff_native <- function(path, format) {
  list(format = format, settings = list(), columns = I(list()), names = I(character(0)),
       preview = I(list()), nrow = NA_integer_, ncol = NA_integer_,
       truncated = FALSE,
       note = "R keeps the column types inside the file - there is nothing to guess.")
}

#' Which reader this file needs, by extension and then by content.
#'
#' Extension first because it is right almost always and costs nothing; magic
#' bytes second because a .txt holding a zip header is an .xlsx someone
#' renamed, and reading it as text produces line noise rather than an error.
detect_format <- function(path, name = path) {
  # Named off the SOURCE (a URL keeps its extension; its temp copy may not),
  # sniffed off the bytes actually downloaded.
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", name)))
  magic <- tryCatch(readBin(path, "raw", n = 8L), error = function(e) raw(0))
  is_zip <- length(magic) >= 4L && identical(as.integer(magic[1:2]), c(80L, 75L))
  is_gz <- length(magic) >= 2L && identical(as.integer(magic[1:2]), c(31L, 139L))
  if (ext %in% c("xlsx", "xlsm")) return("excel")
  if (ext == "xls") return("excel")
  if (ext == "rds" || (is_gz && ext == "rds")) return("rds")
  if (ext %in% c("rdata", "rda")) return("rdata")
  if (ext %in% c("sav", "zsav", "por")) return("spss")
  if (ext == "dta") return("stata")
  if (ext %in% c("sas7bdat", "xpt")) return("sas")
  if (ext == "json") return("json")
  if (ext == "parquet") return("parquet")
  if (ext == "fst") return("fst")
  if (is_zip) return("excel")
  "delimited"
}

#' The missing-value tokens this file actually uses.
#'
#' The DETECTION vocabulary is deliberately broad — 22 tokens — because a
#' sniffer should recognise every spelling of "missing" it might meet. Writing
#' all 22 into the generated code is a different matter: it is a wall of noise
#' in the one artefact the user has to read, and it is WRONG, because it
#' silently converts a legitimate "-" or "." value into NA in some future file
#' the same chunk is pointed at. So the code gets the tokens actually observed,
#' plus the two nobody argues about.
#'
#' @param cols The per-column guesses.
#' @return Character vector for `na.strings`.
observed_na_tokens <- function(cols) {
  seen <- unlist(lapply(cols, function(cl) as.character(cl$na_tokens)), use.names = FALSE)
  unique(c("", "NA", sort(seen)))
}

#' Which reader packages this session actually has.
#'
#' `system.file()`, not `requireNamespace()`: the question is "is it
#' installed", and requireNamespace answers it by LOADING the package —
#' which for arrow or rio is seconds of work and a namespace the user never
#' asked to load, every time the wizard opens.
#'
#' @return Named list of logicals, one per reader package.
reader_packages <- function() {
  want <- c("rio", "readxl", "haven", "data.table", "arrow", "fst", "jsonlite", "readr")
  as.list(stats::setNames(
    vapply(want, function(p) nzchar(system.file(package = p)), logical(1)), want))
}

# ── remote sources ──────────────────────────────────────────────────────────
# A URL is a first-class source. Detection needs BYTES, so the file is fetched
# once to a temp copy and sniffed there — but the generated code keeps the
# URL, because a chunk that reads a temp path is reproducible for nobody.
#
# Cached per session: the wizard re-sniffs on every settings change, and
# re-downloading a 40 MB CSV each time somebody flips a delimiter is not a
# design, it is an accident.
URL_CACHE <- new.env(parent = emptyenv())
URL_TIMEOUT <- 60

is_url <- function(path) {
  is.character(path) && length(path) == 1L &&
    grepl("^(https?|ftps?)://", path, ignore.case = TRUE)
}

#' The local bytes for a source, downloading a URL once.
#' @return list(local, display, remote)
resolve_source <- function(path) {
  if (!is_url(path)) return(list(local = path.expand(path), display = path, remote = FALSE))
  hit <- URL_CACHE[[path]]
  if (!is.null(hit) && file.exists(hit)) {
    return(list(local = hit, display = path, remote = TRUE))
  }
  # The extension is kept so detect_format() can still read it off the name.
  ext <- tools::file_ext(sub("[?#].*$", "", path))
  dest <- tempfile("carmar-url-", fileext = if (nzchar(ext)) paste0(".", ext) else "")
  old <- options(timeout = URL_TIMEOUT)
  on.exit(options(old), add = TRUE)
  ok <- tryCatch({
    utils::download.file(path, dest, quiet = TRUE, mode = "wb")
    file.exists(dest) && file.info(dest)$size > 0
  }, error = function(e) structure(class = "carmar_fail", list(msg = conditionMessage(e))),
     warning = function(w) structure(class = "carmar_fail", list(msg = conditionMessage(w))))
  if (inherits(ok, "carmar_fail")) stop("could not download: ", ok$msg)
  if (!isTRUE(ok)) stop("could not download that URL")
  assign(path, dest, envir = URL_CACHE)
  list(local = dest, display = path, remote = TRUE)
}

#' The whole detection pass for one file.
#'
#' @param path File to inspect.
#' @param opts Overrides from the wizard — any setting the user has changed is
#'   respected and everything else is re-detected around it, which is what
#'   makes the panel feel live rather than one-shot.
#' @return The description the wizard renders and the code generator consumes.
sniff_file <- function(path, opts = list()) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  src <- resolve_source(path)
  local <- src$local
  if (!file.exists(local) || dir.exists(local)) stop("not a readable file")
  fmt <- opts$format %||% detect_format(local, name = src$display)
  out <- switch(fmt,
    excel = sniff_excel(local, opts),
    delimited = sniff_delimited(local, opts),
    rds = sniff_native(path, "rds"),
    rdata = sniff_native(path, "rdata"),
    spss = sniff_native(path, "spss"),
    stata = sniff_native(path, "stata"),
    sas = sniff_native(path, "sas"),
    json = sniff_native(path, "json"),
    parquet = sniff_native(path, "parquet"),
    fst = sniff_native(path, "fst"),
    stop("unsupported file type: ", fmt))
  # The DISPLAY path travels onward — the wizard shows it and the code
  # generator writes it — so a URL import produces a chunk anyone can re-run.
  out$path <- src$display
  out$remote <- src$remote
  out$size <- as.numeric(file.info(local)$size)
  # Reported with every sniff so the wizard can offer only engines this
  # session can actually run — generating rio code for a session without rio
  # is a chunk that fails on the first line.
  out$packages <- reader_packages()
  out
}
