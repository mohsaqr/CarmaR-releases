# app.R — CarmaR as a native application installed by the R package.
#
# On macOS the source package carries the two release-built apps in an archive.
# R extracts and installs that archive locally, so the apps do not inherit a
# browser-download quarantine marker. Windows and Linux build their small
# launchers locally in the Start menu or application menu.
#
# Every launcher is two layers, and both are deliberately dumb:
#   1. a platform shim that finds Rscript AT CLICK TIME (an R upgrade must
#      not strand the icon) and runs, hidden,
#   2. launch.R — plain readable R that reinstalls carmar if it has gone
#      missing, then calls carmar::run(). Self-healing, and auditable by
#      anyone who can read ten lines of R.
# On macOS the package carries the exact release-built CarmaR.app and menu
# helper. Installing them through R does not attach a browser-download
# quarantine attribute, while preserving the document and carmar:// handlers.

#' The bootstrap the launcher runs — kept as its own file, not an inline -e,
#' so every platform shim escapes NOTHING and a curious user can open it.
#' @keywords internal
launch_r_code <- function() {
  paste0(
    "# CarmaR bootstrap - written by carmar::install_app().\n",
    "# The launcher runs this with the R it found on this machine.\n",
    "cran <- \"https://cloud.r-project.org\"\n",
    "need <- c(\"httpuv\", \"jsonlite\", \"processx\")\n",
    "miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]\n",
    "if (length(miss)) {\n",
    "  message(\"CarmaR is preparing this R installation: \", paste(miss, collapse = \", \"), \" ...\")\n",
    "  install.packages(miss, repos = cran)\n",
    "}\n",
    "miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]\n",
    "if (length(miss)) stop(\"CarmaR could not install: \", paste(miss, collapse = \", \"),\n",
    "  \". Check the internet connection, then open CarmaR again.\", call. = FALSE)\n",
    "if (!requireNamespace(\"carmar\", quietly = TRUE)) {\n",
    "  message(\"CarmaR is not installed in this R - installing...\")\n",
    "  cdn <- paste0(\"https://lacarm.com/carmar/carmar.tar.gz?install=\", as.integer(Sys.time()))\n",
    "  try(install.packages(cdn, repos = NULL, type = \"source\"), silent = TRUE)\n",
    "  if (!requireNamespace(\"carmar\", quietly = TRUE))\n",
    "    install.packages(\"carmar\", repos = c(\"https://mohsaqr.r-universe.dev\", cran))\n",
    "  if (!requireNamespace(\"carmar\", quietly = TRUE)) {\n",
    "    github <- \"https://github.com/mohsaqr/CarmaR-releases/releases/latest/download/carmar.tar.gz\"\n",
    "    try(install.packages(github, repos = NULL, type = \"source\"), silent = TRUE)\n",
    "  }\n",
    "}\n",
    "if (!requireNamespace(\"carmar\", quietly = TRUE))\n",
    "  stop(\"CarmaR could not be installed. Check the internet connection, then open CarmaR again.\", call. = FALSE)\n",
    "carmar::run()\n")
}

#' The POSIX shim: find Rscript, run launch.R, say something useful on
#' failure. `alert` is the platform's way of talking to a person who did not
#' open a terminal.
#' @keywords internal
launch_sh_code <- function(launch_r, alert) {
  paste0(
    "#!/bin/sh\n",
    "# CarmaR launcher - manufactured locally by carmar::install_app().\n",
    "set -u\n",
    "alert() { ", alert, "; }\n",
    "rscript=\"\"\n",
    "for c in /Library/Frameworks/R.framework/Resources/bin/Rscript \\\n",
    "         /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /usr/bin/Rscript; do\n",
    "  [ -x \"$c\" ] && { rscript=\"$c\"; break; }\n",
    "done\n",
    "[ -z \"$rscript\" ] && command -v Rscript >/dev/null 2>&1 && rscript=\"$(command -v Rscript)\"\n",
    "if [ -z \"$rscript\" ]; then\n",
    "  alert \"R was not found. Install R from cran.r-project.org, then open CarmaR again.\"\n",
    "  exit 1\n",
    "fi\n",
    "\"$rscript\" ", shQuote(launch_r),
    " || alert \"CarmaR could not start. Open R and run: carmar::run()\"\n")
}

#' Where the shipped icon files live — inst/launcher in the installed
#' package, "" when running from source (the generators then skip the icon,
#' never fail on it).
#' @keywords internal
launcher_assets <- function() {
  p <- system.file("launcher", package = "carmar")
  if (nzchar(p)) p else ""
}

#' Release-built macOS bundles carried inside the installed R package.
#' @keywords internal
launcher_macos_bundles <- function() {
  p <- system.file("app", "macos", "carmar-apps.tar.gz", package = "carmar")
  if (nzchar(p) && file.exists(p)) p else ""
}

#' Build CarmaR.app — a plain bundle with a shell-script executable. Scripts
#' need no code signature even on Apple silicon, and a bundle this machine
#' wrote has no quarantine bit: it opens like any app, first click included.
#' @keywords internal
app_macos <- function(apps = "~/Applications", assets = launcher_assets(),
                      helper = TRUE, keep_ready = FALSE,
                      bundles = launcher_macos_bundles(),
                      launch_helper = TRUE, preserve_keep_ready = TRUE) {
  apps <- path.expand(apps)
  app <- file.path(apps, "CarmaR.app")
  helper_app <- file.path(apps, "CarmaR Helper.app")

  # Release packages carry the two signed bundles in one archive. Keeping
  # Mach-O executables out of the source-package file list also keeps normal
  # R package checks portable. A directory remains accepted for development
  # and tests that exercise the installer without building the apps.
  bundle_root <- bundles
  if (nzchar(bundles) && file.exists(bundles) && !dir.exists(bundles)) {
    unpacked <- tempfile("carmar-apps-")
    dir.create(unpacked)
    status <- tryCatch(utils::untar(bundles, exdir = unpacked),
                       error = function(e) 1L)
    if (!identical(status, 0L))
      stop("The CarmaR application archive could not be unpacked.", call. = FALSE)
    on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
    bundle_root <- unpacked
  }
  bundled_app <- if (nzchar(bundle_root)) file.path(bundle_root, "CarmaR.app") else ""
  bundled_helper <- if (nzchar(bundle_root)) file.path(bundle_root, "CarmaR Helper.app") else ""

  # A release package contains the full app, not the old run()-only shim. It
  # is the same bundle installed by CarmaR.pkg: document handlers, carmar://
  # published-page authorization, updater, kernel and menu integration.
  if (nzchar(bundled_app) && dir.exists(bundled_app)) {
    dir.create(apps, recursive = TRUE, showWarnings = FALSE)
    agent_was_installed <- isTRUE(preserve_keep_ready) && file.exists(path.expand(
      "~/Library/LaunchAgents/me.saqr.carmar.helper.plist"))
    old_agents <- file.path(helper_app, "Contents", "Resources", "helper-agent.sh")
    old_agent <- old_agents[file.exists(old_agents)][1]

    if (isTRUE(helper) && length(old_agent) && !is.na(old_agent)) {
      suppressWarnings(system2("/bin/sh", c(shQuote(old_agent), "uninstall"),
                               stdout = FALSE, stderr = FALSE))
    }
    if (isTRUE(helper)) {
      suppressWarnings(system2("/usr/bin/osascript", c("-e", shQuote(
        'tell application id "me.saqr.carmar.helper.app" to quit')),
        stdout = FALSE, stderr = FALSE))
    }

    install_bundle <- function(from, to) {
      unlink(to, recursive = TRUE)
      status <- system2("/usr/bin/ditto", c(shQuote(from), shQuote(to)),
                        stdout = FALSE, stderr = FALSE)
      if (!identical(status, 0L) || !dir.exists(to))
        stop("Could not install ", basename(to), " in ", apps, call. = FALSE)
    }
    install_bundle(bundled_app, app)
    if (isTRUE(helper)) {
      if (!dir.exists(bundled_helper))
        stop("The CarmaR package does not contain CarmaR Helper.app.", call. = FALSE)
      install_bundle(bundled_helper, helper_app)
      agent <- file.path(helper_app, "Contents", "Resources", "helper-agent.sh")
      if (isTRUE(keep_ready) || agent_was_installed) {
        status <- system2("/bin/sh", c(shQuote(agent), "install"),
                          stdout = FALSE, stderr = FALSE)
        if (!identical(status, 0L))
          warning("CarmaR was installed, but its keep-ready service could not be restored.")
      }
      if (isTRUE(launch_helper))
        suppressWarnings(system2("/usr/bin/open", c("-g", shQuote(helper_app)),
                                 stdout = FALSE, stderr = FALSE))
    }

    register <- "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"
    if (file.exists(register)) {
      suppressWarnings(system2(register, c("-f", shQuote(app)),
                               stdout = FALSE, stderr = FALSE))
      if (isTRUE(helper)) suppressWarnings(system2(register,
        c("-f", shQuote(helper_app)), stdout = FALSE, stderr = FALSE))
    }
    return(app)
  }

  # Development/source fallback. Release packages always take the full-bundle
  # path above; keeping this generator makes source-level tests and unusual
  # hand-built packages retain a basic launcher.
  res <- file.path(app, "Contents", "Resources")
  bin <- file.path(app, "Contents", "MacOS")
  unlink(app, recursive = TRUE)
  dir.create(res, recursive = TRUE, showWarnings = FALSE)
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)

  icns <- if (nzchar(assets)) file.path(assets, "carmar.icns") else ""
  has_icon <- nzchar(icns) && file.exists(icns)
  if (has_icon) file.copy(icns, file.path(res, "CarmaR.icns"))

  version <- tryCatch(as.character(utils::packageVersion("carmar")),
                      error = function(e) "0.0")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">',
    '<plist version="1.0"><dict>',
    "  <key>CFBundleName</key><string>CarmaR</string>",
    "  <key>CFBundleDisplayName</key><string>CarmaR</string>",
    "  <key>CFBundleIdentifier</key><string>me.saqr.carmar</string>",
    "  <key>CFBundleExecutable</key><string>CarmaR</string>",
    "  <key>CFBundlePackageType</key><string>APPL</string>",
    if (has_icon) "  <key>CFBundleIconFile</key><string>CarmaR</string>",
    paste0("  <key>CFBundleShortVersionString</key><string>", version, "</string>"),
    "  <key>LSApplicationCategoryType</key><string>public.app-category.education</string>",
    "</dict></plist>"),
    file.path(app, "Contents", "Info.plist"))

  launch_r <- file.path(res, "launch.R")
  writeLines(launch_r_code(), launch_r)
  sh <- file.path(bin, "CarmaR")
  writeLines(launch_sh_code(
    launch_r,
    "osascript -e \"display alert \\\"CarmaR\\\" message \\\"$1\\\"\" >/dev/null 2>&1 || true"), sh)
  Sys.chmod(sh, "0755")
  app
}

#' Windows: a hidden-window .vbs shim plus a Start-menu shortcut. The .lnk
#' points at wscript so no console flashes; RegRead finds R wherever the
#' installer put it, per-user or per-machine. HKCU only — no admin anywhere.
#' `make_shortcut` exists because the .lnk needs cscript, which only exists
#' on Windows, while the FILES can be generated (and tested) on any OS.
#' @keywords internal
app_windows <- function(base = file.path(Sys.getenv("LOCALAPPDATA"), "CarmaR"),
                        startmenu = file.path(Sys.getenv("APPDATA"),
                          "Microsoft", "Windows", "Start Menu", "Programs"),
                        make_shortcut = identical(.Platform$OS.type, "windows"),
                        assets = launcher_assets()) {
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  launch_r <- file.path(base, "launch.R")
  writeLines(launch_r_code(), launch_r)
  ico <- if (nzchar(assets)) file.path(assets, "carmar.ico") else ""
  has_icon <- nzchar(ico) && file.exists(ico)
  if (has_icon) file.copy(ico, file.path(base, "carmar.ico"), overwrite = TRUE)
  # Shell APIs want backslashes; R built these paths with forward slashes.
  winpath <- function(p) chartr("/", "\\", p)

  vbs <- file.path(base, "launch.vbs")
  writeLines(c(
    "' CarmaR launcher - manufactured locally by carmar::install_app().",
    'Set sh = CreateObject("WScript.Shell")',
    'Set fso = CreateObject("Scripting.FileSystemObject")',
    'rhome = ""',
    "On Error Resume Next",
    'rhome = sh.RegRead("HKCU\\Software\\R-core\\R\\InstallPath")',
    'If rhome = "" Then rhome = sh.RegRead("HKLM\\Software\\R-core\\R\\InstallPath")',
    'If rhome = "" Then rhome = sh.RegRead("HKLM\\Software\\WOW6432Node\\R-core\\R\\InstallPath")',
    "On Error Goto 0",
    'rscript = rhome & "\\bin\\Rscript.exe"',
    'If rhome = "" Or Not fso.FileExists(rscript) Then',
    '  MsgBox "R was not found. Install R from cran.r-project.org, then open CarmaR again.", 48, "CarmaR"',
    "  WScript.Quit 1",
    "End If",
    paste0('code = sh.Run("""" & rscript & """ """ & "', launch_r, '" & """", 0, True)'),
    'If code <> 0 Then',
    '  MsgBox "CarmaR could not start. Check your internet connection, then open CarmaR again. If it still fails, open R and run: carmar::run()", 48, "CarmaR"',
    "End If"),
    vbs)

  if (isTRUE(make_shortcut)) {
    dir.create(startmenu, recursive = TRUE, showWarnings = FALSE)
    maker <- tempfile(fileext = ".vbs")
    writeLines(c(
      'Set sh = CreateObject("WScript.Shell")',
      paste0('Set lnk = sh.CreateShortcut("', winpath(file.path(startmenu, "CarmaR.lnk")), '")'),
      'lnk.TargetPath = "wscript.exe"',
      paste0('lnk.Arguments = """', winpath(vbs), '"""'),
      if (has_icon) paste0('lnk.IconLocation = "', winpath(file.path(base, "carmar.ico")), ', 0"'),
      'lnk.Description = "CarmaR - R notebook"',
      "lnk.Save"), maker)
    system2("cscript", c("//nologo", maker), stdout = FALSE, stderr = FALSE)
    unlink(maker)
  }
  base
}

#' Linux: a launcher script plus a freedesktop .desktop entry — the app menu
#' on every desktop that follows the spec, which is all of them.
#' @keywords internal
app_linux <- function(share = "~/.local/share", assets = launcher_assets()) {
  share <- path.expand(share)
  base <- file.path(share, "carmar")
  apps <- file.path(share, "applications")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  dir.create(apps, recursive = TRUE, showWarnings = FALSE)

  launch_r <- file.path(base, "launch.R")
  writeLines(launch_r_code(), launch_r)
  sh <- file.path(base, "launch.sh")
  writeLines(launch_sh_code(
    launch_r,
    "notify-send CarmaR \"$1\" 2>/dev/null || echo \"CarmaR: $1\" >&2"), sh)
  Sys.chmod(sh, "0755")

  png <- if (nzchar(assets)) file.path(assets, "carmar.png") else ""
  has_icon <- nzchar(png) && file.exists(png)
  if (has_icon) file.copy(png, file.path(base, "carmar.png"), overwrite = TRUE)

  writeLines(c(
    "[Desktop Entry]",
    "Type=Application",
    "Name=CarmaR",
    "Comment=R notebook - your R, your files, your machine",
    paste0("Exec=", sh),
    if (has_icon) paste0("Icon=", file.path(base, "carmar.png")),
    "Terminal=false",
    "Categories=Development;Science;Education;"),
    file.path(apps, "carmar.desktop"))
  base
}

#' Install CarmaR as an application on this computer
#'
#' Creates a native launcher — CarmaR in ~/Applications on macOS, in the
#' Start menu on Windows, in the app menu on Linux — so starting CarmaR is
#' a click, with R and the console nowhere in sight. On macOS the release-built
#' app and CarmaR Helper are installed together from the package. Because R
#' performs the local installation, they are not given a browser-download
#' quarantine marker. The helper menu lists and controls local sessions, and
#' opening CarmaR brings it back if it was closed. The keep-ready login service
#' remains opt-in unless requested. Other platforms use a small launcher that
#' finds R fresh and repairs a missing package automatically.
#'
#' @param quiet Say nothing on success? Default `FALSE`.
#' @param helper On macOS, install and open the menu helper? Default `TRUE`.
#' @param keep_ready On macOS, keep one kernel ready at login? Default `FALSE`.
#' @return Invisibly, the path of what was created.
#' @export
install_app <- function(quiet = FALSE, helper = TRUE, keep_ready = FALSE) {
  say <- function(...) if (!quiet) message(...)
  os <- unname(Sys.info()["sysname"])
  made <- if (identical(os, "Darwin")) {
    p <- app_macos(helper = helper, keep_ready = keep_ready)
    say("CarmaR is now an app: ", p,
        "\nFind it in Launchpad / Spotlight as CarmaR.",
        if (isTRUE(helper)) " The CarmaR menu is running too." else "")
    p
  } else if (identical(.Platform$OS.type, "windows")) {
    p <- app_windows()
    say("CarmaR is now in your Start menu. Search for CarmaR and pin it if you like.")
    p
  } else {
    p <- app_linux()
    say("CarmaR is now in your application menu (carmar.desktop).")
    p
  }
  invisible(made)
}

#' Remove the launcher `install_app()` created
#'
#' Removes only what `install_app()` made — the package, your notebooks and
#' your R are untouched.
#'
#' @param quiet Say nothing? Default `FALSE`.
#' @param helper On macOS, also stop and remove CarmaR Helper and its
#'   keep-ready service? Default `TRUE`.
#' @return Invisibly, `TRUE` if something was removed.
#' @export
uninstall_app <- function(quiet = FALSE, helper = TRUE) {
  say <- function(...) if (!quiet) message(...)
  os <- unname(Sys.info()["sysname"])
  targets <- if (identical(os, "Darwin")) {
    main <- path.expand("~/Applications/CarmaR.app")
    helper_app <- path.expand("~/Applications/CarmaR Helper.app")
    if (isTRUE(helper)) {
      agent <- file.path(helper_app, "Contents", "Resources", "helper-agent.sh")
      if (file.exists(agent)) suppressWarnings(system2(
        "/bin/sh", c(shQuote(agent), "uninstall"), stdout = FALSE, stderr = FALSE))
      suppressWarnings(system2("/usr/bin/osascript", c("-e", shQuote(
        'tell application id "me.saqr.carmar.helper.app" to quit')),
        stdout = FALSE, stderr = FALSE))
      update_label <- "me.saqr.carmar.update"
      uid <- tryCatch(trimws(system2("/usr/bin/id", "-u", stdout = TRUE,
                                    stderr = FALSE)),
                      error = function(e) "")
      if (nzchar(uid)) suppressWarnings(system2("/bin/launchctl", c("bootout",
        paste0("gui/", uid, "/", update_label)),
        stdout = FALSE, stderr = FALSE))
      unlink(path.expand(paste0("~/Library/LaunchAgents/", update_label, ".plist")))
      c(main, helper_app)
    } else main
  } else if (identical(.Platform$OS.type, "windows")) {
    c(file.path(Sys.getenv("LOCALAPPDATA"), "CarmaR"),
      file.path(Sys.getenv("APPDATA"), "Microsoft", "Windows", "Start Menu",
                "Programs", "CarmaR.lnk"))
  } else {
    c(path.expand("~/.local/share/carmar"),
      path.expand("~/.local/share/applications/carmar.desktop"))
  }
  existed <- file.exists(targets)
  unlink(targets[existed], recursive = TRUE)
  if (any(existed)) say("Removed: ", paste(targets[existed], collapse = ", "))
  else say("No CarmaR launcher found - nothing to remove.")
  invisible(any(existed))
}
