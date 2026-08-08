# app.R — CarmaR as a native application, manufactured ON this machine.
#
# The whole trick, and the reason this beats shipping an app: a downloaded
# .app carries the quarantine bit and meets Gatekeeper, which demands a
# Developer ID we refuse to make students depend on. An app WRITTEN by R on
# the user's own machine carries no quarantine bit at all — same code, no
# dialog, no signing, nothing to notarize. So the package builds the launcher
# locally: a bundle in ~/Applications on macOS, a Start-menu entry on
# Windows, a .desktop file on Linux.
#
# Every launcher is two layers, and both are deliberately dumb:
#   1. a platform shim that finds Rscript AT CLICK TIME (an R upgrade must
#      not strand the icon) and runs, hidden,
#   2. launch.R — plain readable R that reinstalls carmar if it has gone
#      missing, then calls carmar::run(). Self-healing, and auditable by
#      anyone who can read ten lines of R.
# No protocol handlers, no agents, no background jobs: the app IS run().

#' The bootstrap the launcher runs — kept as its own file, not an inline -e,
#' so every platform shim escapes NOTHING and a curious user can open it.
#' @keywords internal
launch_r_code <- function() {
  paste0(
    "# CarmaR bootstrap - written by carmar::install_app().\n",
    "# The launcher runs this with the R it found on this machine.\n",
    "if (!requireNamespace(\"carmar\", quietly = TRUE)) {\n",
    "  message(\"CarmaR is not installed in this R - installing...\")\n",
    "  options(pkgType = \"binary\")\n",
    "  install.packages(\"carmar\",\n",
    "    repos = c(\"https://mohsaqr.r-universe.dev\", \"https://cloud.r-project.org\"))\n",
    "}\n",
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

#' Build CarmaR.app — a plain bundle with a shell-script executable. Scripts
#' need no code signature even on Apple silicon, and a bundle this machine
#' wrote has no quarantine bit: it opens like any app, first click included.
#' @keywords internal
app_macos <- function(apps = "~/Applications", assets = launcher_assets()) {
  apps <- path.expand(apps)
  app <- file.path(apps, "CarmaR.app")
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
    paste0('sh.Run """" & rscript & """ """ & "', launch_r, '" & """", 0, False')),
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
#' a click, with R and the console nowhere in sight. The launcher is
#' manufactured locally by this function, which is why macOS shows no
#' "unidentified developer" dialog: nothing was downloaded, so there is
#' nothing for Gatekeeper to quarantine. It finds R fresh on every click
#' and reinstalls carmar if it has gone missing, so it survives R upgrades
#' and accidental removals. Typing carmar::run() keeps working exactly as
#' before — the launcher is the same call with an icon on it.
#'
#' @param quiet Say nothing on success? Default `FALSE`.
#' @return Invisibly, the path of what was created.
#' @export
install_app <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  os <- unname(Sys.info()["sysname"])
  made <- if (identical(os, "Darwin")) {
    p <- app_macos()
    say("CarmaR is now an app: ", p, "\nFind it in Launchpad / Spotlight as CarmaR.")
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
#' @return Invisibly, `TRUE` if something was removed.
#' @export
uninstall_app <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  os <- unname(Sys.info()["sysname"])
  targets <- if (identical(os, "Darwin")) {
    path.expand("~/Applications/CarmaR.app")
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
