\name{upgrade}
\alias{upgrade}
\title{Check for a signed full-product CarmaR update}
\usage{
upgrade(quiet = FALSE)
}
\arguments{
\item{quiet}{Say nothing unless something was updated? Default \code{FALSE}.}
}
\value{
Invisibly, \code{TRUE} if anything was updated.
}
\description{
Invokes the signed updater carried by CarmaR.app or the package. The updater
verifies a detached release signature, unified component versions, artifact
SHA-256 and the platform publisher identity. A check may cache a verified
installer, but never replaces executable files in the background. Unsigned
legacy release metadata is refused. \code{run()} performs this check quietly.
}
