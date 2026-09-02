#!/bin/sh
# Signed CarmaR updater (macOS app).
#
# Daily invocation performs CHECK only: download a detached-signed manifest,
# verify it against the public key embedded in the signed app, select the full
# macOS package, and verify both SHA-256 and Developer ID Team ID before making
# Update available. It never writes executable product files in the
# background. Explicit actions:
#
#   update.sh check        download and verify an available installer
#   update.sh status       print the current tab-separated status record
#   update.sh defer [days] suppress checks for 1–30 days (default 7)
#   update.sh install      open the already-verified macOS installer
#   update.sh rollback     open the cached last-known-good installer
set -u

res="$(cd "$(dirname "$0")" && pwd)"
state="${CARMAR_STATE:-$HOME/Library/Application Support/CarmaR}"
public_key="${CARMAR_UPDATE_PUBLIC_KEY:-$res/update-public.pem}"
team_file="${CARMAR_UPDATE_TEAM_FILE:-$res/update-team-id}"
reader="${CARMAR_UPDATE_READER:-$res/update-read.R}"
openssl="${CARMAR_OPENSSL:-/usr/bin/openssl}"
pkgutil="${CARMAR_PKGUTIL:-/usr/sbin/pkgutil}"
action="${1:-check}"
status="$state/update-status.tsv"
log="$state/update.log"
downloads="$state/updates"
mkdir -p "$state" "$downloads" 2>/dev/null || exit 0
umask 077

say() { echo "$(date '+%Y-%m-%d %H:%M:%S') $1" >> "$log"; }
clean() { printf '%s' "${1:-}" | tr '\r\n\t' '   '; }
write_status() { # state current available pending previous message
  tmp="$status.part.$$"
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$(clean "$1")" "$(clean "$2")" "$(clean "$3")" \
    "$(clean "$4")" "$(clean "$5")" "$(clean "$6")" > "$tmp" \
    && mv "$tmp" "$status"
}

# A Finder-launched app and its LaunchAgent do not inherit shell profiles.
# Read the same protected administrator policy as the R supervisor so pilot
# feeds, mirrors, offline folders, and MDM-owned update mode apply to both the
# background check and the visible Settings actions. The parser refuses
# user-writable files and unknown keys; no network request happens on failure.
RS="/Library/Frameworks/R.framework/Versions/Current/Resources/bin/Rscript"
if [ ! -x "$RS" ]; then
  for candidate in /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /opt/local/bin/Rscript /usr/bin/Rscript; do
    if [ -x "$candidate" ]; then RS="$candidate"; break; fi
  done
fi
managed_values=""
if [ -x "${RS:-}" ] && [ -f "$res/kernel/settings.R" ]; then
  if ! managed_values="$("$RS" --vanilla -e '
    args <- commandArgs(TRUE)
    source(args[[1]])
    x <- carmar_managed_environment(apply_values = FALSE)
    if (length(x$errors)) {
      writeLines(x$errors, stderr())
      quit(status = 3L, save = "no")
    }
    for (key in c("CARMAR_UPDATE_FEED", "CARMAR_UPDATE_MIRROR",
                  "CARMAR_UPDATE_OFFLINE_DIR", "CARMAR_UPDATE_MANAGED",
                  "HTTPS_PROXY", "HTTP_PROXY", "NO_PROXY")) {
      value <- unname(x$values[[key]])
      if (length(value) == 1L && nzchar(value)) cat(key, "=", value, "\n", sep = "")
    }
  ' "$res/kernel/settings.R" 2>>"$log")"; then
    say "managed update policy refused"
    write_status "refused" "" "" "" "" "The administrator update policy is invalid or not protected."
    exit 0
  fi
fi
managed_feed="$(printf '%s\n' "$managed_values" | sed -n 's/^CARMAR_UPDATE_FEED=//p' | sed -n '1p')"
managed_mirror="$(printf '%s\n' "$managed_values" | sed -n 's/^CARMAR_UPDATE_MIRROR=//p' | sed -n '1p')"
managed_offline="$(printf '%s\n' "$managed_values" | sed -n 's/^CARMAR_UPDATE_OFFLINE_DIR=//p' | sed -n '1p')"
managed_mode="$(printf '%s\n' "$managed_values" | sed -n 's/^CARMAR_UPDATE_MANAGED=//p' | sed -n '1p')"
managed_https_proxy="$(printf '%s\n' "$managed_values" | sed -n 's/^HTTPS_PROXY=//p' | sed -n '1p')"
managed_http_proxy="$(printf '%s\n' "$managed_values" | sed -n 's/^HTTP_PROXY=//p' | sed -n '1p')"
managed_no_proxy="$(printf '%s\n' "$managed_values" | sed -n 's/^NO_PROXY=//p' | sed -n '1p')"
feed="${CARMAR_UPDATE_FEED:-${managed_feed:-https://lacarm.com/carmar/update/manifest.json}}"
update_mirror="${CARMAR_UPDATE_MIRROR:-$managed_mirror}"
update_offline="${CARMAR_UPDATE_OFFLINE_DIR:-$managed_offline}"
update_managed="${CARMAR_UPDATE_MANAGED:-$managed_mode}"
if [ -n "$managed_https_proxy" ]; then HTTPS_PROXY="$managed_https_proxy"; export HTTPS_PROXY; fi
if [ -n "$managed_http_proxy" ]; then HTTP_PROXY="$managed_http_proxy"; export HTTP_PROXY; fi
if [ -n "$managed_no_proxy" ]; then NO_PROXY="$managed_no_proxy"; export NO_PROXY; fi

have="$(sed -n '1p' "$res/kernel/kernel-version" 2>/dev/null || true)"
expected_protocol="$(sed -n '1p' "$res/kernel/kernel-protocol" 2>/dev/null || true)"
installed_pkg="$downloads/installed.pkg"
installed_version_file="$downloads/installed.version"
previous_pkg="$downloads/previous.pkg"
previous_version_file="$downloads/previous.version"
pending_pkg="$downloads/pending.pkg"
pending_version_file="$downloads/pending.version"
pending_version="$(sed -n '1p' "$pending_version_file" 2>/dev/null || true)"
installed_version="$(sed -n '1p' "$installed_version_file" 2>/dev/null || true)"
previous_version="$(sed -n '1p' "$previous_version_file" 2>/dev/null || true)"

if [ "$update_managed" = "1" ]; then
  write_status "unconfigured" "$have" "" "" "$previous_version" "Updates are installed by your organisation."
  say "update check delegated to device management"
  exit 0
fi

# A package opened on the previous check is installed only when the app's own
# stamped build says so on a later run. A cancelled Installer never becomes a
# rollback point by wishful thinking.
if [ -n "$pending_version" ] && [ "$have" = "$pending_version" ] && [ -f "$pending_pkg" ]; then
  if [ -f "$installed_pkg" ] && [ -n "$installed_version" ]; then
    mv "$installed_pkg" "$previous_pkg" 2>/dev/null || true
    printf '%s\n' "$installed_version" > "$previous_version_file"
    previous_version="$installed_version"
  fi
  mv "$pending_pkg" "$installed_pkg" 2>/dev/null || true
  printf '%s\n' "$pending_version" > "$installed_version_file"
  rm -f "$pending_version_file"
  installed_version="$pending_version"
  pending_version=""
fi

case "$action" in
  status)
    if [ -f "$status" ]; then cat "$status"
    else write_status "unknown" "$have" "" "" "$previous_version" "No update check has completed yet."; cat "$status"
    fi
    exit 0
    ;;
  defer)
    days="${2:-7}"
    case "$days" in *[!0-9]*|'') days=7 ;; esac
    [ "$days" -lt 1 ] && days=1
    [ "$days" -gt 30 ] && days=30
    until_epoch="$(( $(date +%s) + days * 86400 ))"
    printf '%s\n' "$until_epoch" > "$downloads/deferred-until"
    write_status "deferred" "$have" "$pending_version" "$pending_pkg" "$previous_version" "Update checks deferred for $days days."
    say "updates deferred for $days days"
    exit 0
    ;;
  install)
    if [ ! -f "$pending_pkg" ] || [ -z "$pending_version" ]; then
      write_status "error" "$have" "" "" "$previous_version" "No verified update is ready."
      exit 1
    fi
    /usr/bin/open "$pending_pkg" >/dev/null 2>&1 || exit 1
    write_status "installing" "$have" "$pending_version" "$pending_pkg" "$previous_version" "The verified installer is open."
    exit 0
    ;;
  rollback)
    if [ ! -f "$previous_pkg" ] || [ -z "$previous_version" ]; then
      write_status "error" "$have" "" "" "" "No last-known-good installer is cached."
      exit 1
    fi
    /usr/bin/open "$previous_pkg" >/dev/null 2>&1 || exit 1
    write_status "rolling-back" "$have" "" "" "$previous_version" "The last-known-good installer is open."
    exit 0
    ;;
  check) ;;
  *) echo "usage: update.sh [check|status|defer [days]|install|rollback]" >&2; exit 2 ;;
esac

deferred_until="$(sed -n '1p' "$downloads/deferred-until" 2>/dev/null || true)"
case "$deferred_until" in *[!0-9]*|'') deferred_until=0 ;; esac
if [ "$deferred_until" -gt "$(date +%s)" ]; then
  write_status "deferred" "$have" "$pending_version" "$pending_pkg" "$previous_version" "Update checks are deferred."
  exit 0
fi

team="$(sed -n '1p' "$team_file" 2>/dev/null || true)"
case "$team" in
  ''|*[!A-Z0-9]*)
    say "no trusted Apple Team ID"
    write_status "unconfigured" "$have" "" "" "$previous_version" "Signed updates are not configured in this build."
    exit 0
    ;;
esac
if [ ! -x "$openssl" ] || [ ! -x "$pkgutil" ] || [ ! -x "${RS:-}" ] \
   || [ ! -f "$public_key" ] || [ ! -f "$reader" ]; then
  say "signed update verifier unavailable"
  write_status "unconfigured" "$have" "" "" "$previous_version" "Signed updates are not configured in this build."
  exit 0
fi

work="$(mktemp -d "$downloads/check.XXXXXX")" || exit 0
trap 'rm -rf "$work"' EXIT HUP INT TERM
manifest="$work/manifest.json"
signature="$work/manifest.json.sig"
curl -fsSL --max-time 30 "$feed" -o "$manifest" 2>/dev/null \
  || { say "manifest unreachable"; write_status "offline" "$have" "" "" "$previous_version" "The update service is unreachable; the installed version is unchanged."; exit 0; }
curl -fsSL --max-time 30 "${feed}.sig" -o "$signature" 2>/dev/null \
  || { say "manifest signature unreachable"; write_status "error" "$have" "" "" "$previous_version" "The update manifest has no verifiable signature."; exit 0; }
if ! "$openssl" dgst -sha256 -verify "$public_key" -signature "$signature" "$manifest" >/dev/null 2>&1; then
  say "manifest signature refused"
  write_status "refused" "$have" "" "" "$previous_version" "The update manifest failed signature verification and was refused."
  exit 0
fi

meta="$work/meta"
mkdir -p "$meta"
if ! "$RS" --vanilla "$reader" "$manifest" macos-universal "$meta" "$expected_protocol" >/dev/null 2>&1; then
  say "signed manifest schema refused"
  write_status "refused" "$have" "" "" "$previous_version" "The signed update manifest is invalid or incompatible."
  exit 0
fi
want="$(sed -n '1p' "$meta/version")"
filename="$(sed -n '1p' "$meta/filename")"
expected_hash="$(sed -n '1p' "$meta/sha256")"
expected_size="$(sed -n '1p' "$meta/size")"
minimum_installed="$(sed -n '1p' "$meta/minimum-installed-version")"

compatible="$($RS --vanilla -e 'cat(as.integer(utils::compareVersion(commandArgs(TRUE)[1], commandArgs(TRUE)[2]) >= 0))' "${have:-0.0}" "$minimum_installed" 2>/dev/null || echo 0)"
if [ "$compatible" != "1" ]; then
  say "installed version below update compatibility floor"
  write_status "refused" "$have" "" "" "$previous_version" "This installed CarmaR is too old for the offered update; install the current full package."
  exit 0
fi

newer="$($RS --vanilla -e 'cat(as.integer(utils::compareVersion(commandArgs(TRUE)[1], commandArgs(TRUE)[2]) > 0))' "$want" "${have:-0.0}" 2>/dev/null || echo 0)"
if [ "$newer" != "1" ]; then
  write_status "current" "$have" "" "" "$previous_version" "CarmaR is up to date."
  say "up to date ($have)"
  exit 0
fi

part="$work/$filename"
got=0
try_location() {
  location="$1"
  case "$location" in ./*) location="${feed%/*}/${location#./}" ;; esac
  curl -fsSL --max-time 300 "$location" -o "$part" 2>/dev/null && got=1
}
if [ -n "$update_offline" ] && [ -f "$update_offline/$filename" ]; then
  cp "$update_offline/$filename" "$part" 2>/dev/null && got=1
fi
if [ "$got" = 0 ] && [ -n "$update_mirror" ]; then
  try_location "${update_mirror%/}/$filename"
fi
if [ "$got" = 0 ]; then
  while IFS= read -r location; do
    [ -n "$location" ] || continue
    try_location "$location"
    [ "$got" = 1 ] && break
  done < "$meta/locations"
fi
if [ "$got" != 1 ] || [ ! -f "$part" ]; then
  say "all artifact locations failed"
  write_status "offline" "$have" "$want" "" "$previous_version" "The verified update is known, but its installer could not be downloaded."
  exit 0
fi
actual_size="$(wc -c < "$part" | tr -d ' ')"
actual_hash="$(/usr/bin/shasum -a 256 "$part" | awk '{print $1}')"
if [ "$actual_size" != "$expected_size" ] || [ "$actual_hash" != "$expected_hash" ]; then
  say "artifact hash or size refused"
  write_status "refused" "$have" "$want" "" "$previous_version" "The downloaded installer did not match the signed manifest and was refused."
  exit 0
fi
signature_report="$($pkgutil --check-signature "$part" 2>&1 || true)"
if ! printf '%s' "$signature_report" | grep -q "Developer ID Installer:" \
   || ! printf '%s' "$signature_report" | grep -q "($team)"; then
  say "installer Developer ID refused"
  write_status "refused" "$have" "$want" "" "$previous_version" "The installer is not signed by the trusted CarmaR developer and was refused."
  exit 0
fi

mv "$part" "$pending_pkg" || exit 0
printf '%s\n' "$want" > "$pending_version_file"
write_status "available" "$have" "$want" "$pending_pkg" "$previous_version" "A verified full-product update is ready."
say "verified full update available ($have -> $want)"
exit 0
