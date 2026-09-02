# project.R — inspect and restore a project's package environment without
# requiring terminal commands. Pure status logic is separate from worker.R so
# lockfile edge cases and organisation-mirror policy can be tested directly.

carmar_project_or <- function(value, fallback) if (is.null(value)) fallback else value

carmar_repository_display <- function(value) {
  value <- as.character(carmar_project_or(value, ""))[[1L]]
  value <- sub("^([A-Za-z]+://)[^/@]+@", "\\1", value)
  sub("[?#].*$", "", value)
}

carmar_project_status <- function(path = getwd(), env = Sys.getenv,
                                  installed = NULL, repos = getOption("repos"),
                                  renv_available = NULL, libraries = .libPaths()) {
  root <- normalizePath(path, winslash = "/", mustWork = FALSE)
  lockfile <- file.path(root, "renv.lock")
  has_lock <- file.exists(lockfile)
  project_files <- list.files(root, pattern = "[.]Rproj$", ignore.case = TRUE)
  has_quarto <- any(file.exists(file.path(root, c("_quarto.yml", "_quarto.yaml"))))
  managed_raw <- trimws(env("CARMAR_CRAN_MIRROR", ""))
  mirror_error <- if (nzchar(managed_raw) && !grepl("^https://", managed_raw))
    "The administrator's package mirror must use HTTPS." else ""
  cran <- if (length(repos) && "CRAN" %in% names(repos)) unname(repos[["CRAN"]]) else ""
  repository <- if (nzchar(managed_raw)) managed_raw else cran
  repository <- carmar_repository_display(repository)
  if (is.null(renv_available)) renv_available <- requireNamespace("renv", quietly = TRUE)
  active_project <- trimws(env("RENV_PROJECT", ""))
  active <- nzchar(active_project) && identical(
    normalizePath(active_project, winslash = "/", mustWork = FALSE), root)
  if (!active) {
    prefix <- paste0(root, "/renv/library/")
    active <- any(startsWith(normalizePath(libraries, winslash = "/", mustWork = FALSE), prefix))
  }

  locked <- character()
  wanted <- character()
  lock_error <- ""
  if (has_lock) {
    size <- file.info(lockfile)$size[[1L]]
    if (is.na(size) || size > 10 * 1024 * 1024) {
      lock_error <- "renv.lock is too large to inspect safely."
    } else {
      parsed <- tryCatch(jsonlite::fromJSON(lockfile, simplifyVector = FALSE),
                         error = function(e) e)
      if (inherits(parsed, "error") || !is.list(parsed$Packages)) {
        lock_error <- "renv.lock is not valid JSON package metadata."
      } else {
        locked <- names(parsed$Packages)
        wanted <- vapply(parsed$Packages, function(record) {
          value <- carmar_project_or(record$Version, "")
          if (is.character(value) && length(value) == 1L) value else ""
        }, character(1))
      }
    }
  }

  if (is.null(installed)) {
    matrix <- utils::installed.packages()[, c("Package", "Version"), drop = FALSE]
    installed <- stats::setNames(as.character(matrix[, "Version"]), matrix[, "Package"])
  }
  installed <- stats::setNames(as.character(installed), names(installed))
  missing <- setdiff(locked, names(installed))
  shared <- intersect(locked, names(installed))
  different <- shared[nzchar(wanted[shared]) & installed[shared] != wanted[shared]]
  list(
    project = length(project_files) > 0L || has_quarto || has_lock,
    rproj = length(project_files) > 0L,
    quarto = has_quarto,
    lockfile = has_lock,
    lock_error = lock_error,
    renv_available = isTRUE(renv_available),
    active = isTRUE(active),
    packages = length(locked),
    missing = length(missing),
    different = length(different),
    missing_names = I(as.character(utils::head(missing, 12L))),
    different_names = I(as.character(utils::head(different, 12L))),
    managed_mirror = nzchar(managed_raw),
    repository = repository,
    mirror_error = mirror_error,
    can_restore = has_lock && !nzchar(lock_error) && !nzchar(mirror_error)
      && isTRUE(renv_available)
  )
}

carmar_project_action <- function(action, path = getwd(), env = Sys.getenv,
                                  install = NULL, restore = NULL,
                                  available = function() requireNamespace("renv", quietly = TRUE)) {
  action <- if (is.character(action) && length(action) == 1L) action else ""
  if (!action %in% c("bootstrap", "restore")) stop("unknown project environment action")
  state <- carmar_project_status(path = path, env = env,
                                 renv_available = isTRUE(available()))
  if (nzchar(state$mirror_error)) stop(state$mirror_error)
  managed <- trimws(env("CARMAR_CRAN_MIRROR", ""))
  repos <- getOption("repos")
  if (nzchar(managed)) repos <- c(CRAN = managed)

  if (identical(action, "bootstrap")) {
    if (isTRUE(state$renv_available)) {
      return(list(ok = TRUE, message = "renv is already installed.", restart = FALSE))
    }
    if (is.null(install)) install <- function(repositories) {
      writable <- .libPaths()[file.access(.libPaths(), 2L) == 0L]
      if (!length(writable)) stop("This R session has no writable package library.")
      suppressWarnings(utils::install.packages("renv", lib = writable[[1L]],
        repos = repositories, dependencies = NA, quiet = TRUE))
    }
    install(repos)
    if (!isTRUE(available())) stop("renv installation did not complete.")
    return(list(ok = TRUE, message = "renv was installed from the configured package repository.",
                restart = FALSE))
  }

  if (!isTRUE(state$lockfile)) stop("This project has no renv.lock to restore.")
  if (nzchar(state$lock_error)) stop(state$lock_error)
  if (!isTRUE(state$renv_available)) stop("renv is not installed. Install renv first.")
  if (is.null(restore)) restore <- function(project) {
    renv::restore(project = project, prompt = FALSE, clean = FALSE)
  }
  old_override <- Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE", unset = NA_character_)
  on.exit({
    if (is.na(old_override)) Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
    else Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = old_override)
  }, add = TRUE)
  if (nzchar(managed)) Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = managed)
  restore(normalizePath(path, winslash = "/", mustWork = TRUE))
  list(ok = TRUE,
       message = "The packages recorded in renv.lock were restored from the configured repository.",
       restart = !isTRUE(state$active))
}
