\name{upgrade}
\alias{upgrade}
\title{Upgrade CarmaR --- the notebook and the package}
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
Reads the release feed once and updates BOTH halves of an installation: the
newest notebook is downloaded into your user data folder, and when the
release is newer than the installed carmar package, the release's own
package is installed too. You never need to call this: \code{run()} does it
on EVERY call --- the launcher icon included, since the icon is \code{run()}
with a picture on it --- so every click checks for updated versions.
Neither check may block or fail a launch: any trouble is a quiet no-op and
CarmaR starts on what it has.
}
