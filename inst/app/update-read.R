#!/usr/bin/env Rscript
# Parse only an already signature-verified manifest. Values are written into
# separate files so the shell never evals or word-splits network-controlled
# text. This repeats the release-side schema checks at the runtime boundary.

args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
manifest_file <- args[[1]]
platform <- args[[2]]
out <- args[[3]]
expected_protocol <- suppressWarnings(as.integer(args[[4]]))
stopifnot(length(expected_protocol) == 1L, !is.na(expected_protocol), expected_protocol > 0L)

scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
numeric_version_text <- function(x) {
  scalar(x) && grepl("^(0|[1-9][0-9]*)([.](0|[1-9][0-9]*)){1,3}$", x)
}
safe_location <- function(x) {
  scalar(x) && !grepl("[\r\n]", x) && (
    grepl("^https://", x) || grepl("^file://", x) ||
    (grepl("^[.]/[A-Za-z0-9][A-Za-z0-9._/-]*$", x) &&
     !".." %in% strsplit(x, "/", fixed = TRUE)[[1]]))
}

m <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
stopifnot(is.list(m), identical(m$schema, "carmar-update-v1"), is.list(m$release),
          numeric_version_text(m$release$version),
          m$release$channel %in% c("stable", "pilot"),
          scalar(m$release$publishedAt),
          !is.na(as.POSIXct(m$release$publishedAt,
                           format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")),
          is.list(m$versions), is.list(m$compatibility), is.list(m$signing),
          identical(m$signing$algorithm, "rsa-sha256"),
          scalar(m$signing$keyId),
          grepl("^[a-z0-9][a-z0-9._-]{2,63}$", m$signing$keyId))

version <- m$release$version
for (component in c("app", "notebook", "kernel", "rPackage")) {
  stopifnot(identical(m$versions[[component]], version))
}
protocol <- unlist(m$versions$protocol, use.names = FALSE)
compatible_protocol <- unlist(m$compatibility$protocol, use.names = FALSE)
stopifnot(length(protocol) == 1L, is.numeric(protocol), protocol == expected_protocol,
          identical(protocol, compatible_protocol),
          numeric_version_text(m$compatibility$minimumInstalledVersion))

a <- m$artifacts[[platform]]
stopifnot(is.list(a), scalar(a$filename),
          identical(basename(a$filename), a$filename),
          a$kind %in% c("pkg", "exe", "msi", "zip", "tar.gz"),
          scalar(a$sha256), grepl("^[0-9a-f]{64}$", a$sha256),
          length(a$size) == 1L, is.numeric(unlist(a$size)), unlist(a$size) > 0,
          is.list(a$locations), length(a$locations) > 0L)
locations <- unlist(a$locations, use.names = FALSE)
stopifnot(all(vapply(locations, safe_location, logical(1))))

dir.create(out, recursive = TRUE, showWarnings = FALSE)
writeLines(version, file.path(out, "version"), useBytes = TRUE)
writeLines(a$filename, file.path(out, "filename"), useBytes = TRUE)
writeLines(a$sha256, file.path(out, "sha256"), useBytes = TRUE)
writeLines(as.character(unlist(a$size)), file.path(out, "size"), useBytes = TRUE)
writeLines(m$compatibility$minimumInstalledVersion,
           file.path(out, "minimum-installed-version"), useBytes = TRUE)
writeLines(locations, file.path(out, "locations"), useBytes = TRUE)
