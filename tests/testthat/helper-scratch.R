# Test helpers: scratch state, environment variables restored on exit, and
# the extension source (installed package first, then a developer's
# CARMAR_EXTENSION_SOURCE, else skip).

local_envvar <- function(..., .env = parent.frame()) {
  new <- c(...)
  old <- Sys.getenv(names(new), unset = NA_character_, names = TRUE)
  apply_env <- function(vals) {
    unset <- is.na(vals)
    if (any(unset)) Sys.unsetenv(names(vals)[unset])
    if (any(!unset)) do.call(Sys.setenv, as.list(vals[!unset]))
  }
  apply_env(new)
  # LIFO: a second call in the same test restores to the first call's value
  # and the first call then restores the original, so nothing leaks.
  do.call(on.exit, list(bquote(.(apply_env)(.(old))), add = TRUE, after = FALSE),
          envir = .env)
  invisible(old)
}

# A scratch state directory (R_USER_DATA_DIR, which tools::R_user_dir honours)
# and a scratch runtime registry, both empty, both restored on exit.
local_scratch_state <- function(.env = parent.frame()) {
  data <- tempfile("carmar-data-")
  runtime <- tempfile("carmar-run-")
  dir.create(data); dir.create(runtime)
  local_envvar(R_USER_DATA_DIR = data, CARMAR_RUNTIME_DIR = runtime, .env = .env)
  state <- tools::R_user_dir("carmar", "data")
  dir.create(state, recursive = TRUE, showWarnings = FALSE)
  list(state = state, runtime = runtime)
}

extension_source_for_tests <- function() {
  installed <- system.file("quarto", "_extensions", "carmar", package = "carmar")
  if (nzchar(installed) && dir.exists(installed)) return(installed)
  dev <- Sys.getenv("CARMAR_EXTENSION_SOURCE", "")
  if (nzchar(dev) && dir.exists(dev)) return(dev)
  testthat::skip("the Quarto extension is not available here (install the package, or set CARMAR_EXTENSION_SOURCE)")
}

health_json <- function(build = "1.0.0-test", pairing = TRUE) {
  caps <- if (pairing) '"capabilities":["published-direct-v1","published-pairing-v3"],' else ""
  sprintf('{"ok":true,"worker":true,%s"kernel_build":"%s","protocol":1}', caps, build)
}

write_runtime_record <- function(runtime, port, ...) {
  jsonlite::write_json(
    c(list(url = sprintf("http://127.0.0.1:%d/", port), port = port, pid = 1L), list(...)),
    file.path(runtime, sprintf("kernel-%d.json", port)), auto_unbox = TRUE)
}

kernel_tests_enabled <- function() {
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("CARMAR_KERNEL_TESTS"), "1")) {
    testthat::skip("kernel-starting tests run only with CARMAR_KERNEL_TESTS=1")
  }
  invisible(TRUE)
}

# A loopback port nothing is listening on, found by asking the OS.
free_port <- function() httpuv::randomPort()
