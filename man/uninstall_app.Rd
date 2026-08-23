\name{uninstall_app}
\alias{uninstall_app}
\title{Remove the launcher install_app() created}
\usage{
uninstall_app(quiet = FALSE, helper = TRUE)
}
\arguments{
\item{quiet}{Say nothing? Default \code{FALSE}.}
\item{helper}{On macOS, also stop and remove CarmaR Helper and its keep-ready service? Default \code{TRUE}.}
}
\value{
Invisibly, \code{TRUE} if something was removed.
}
\description{
Removes only what \code{\link{install_app}} made --- the package, your
notebooks and your R are untouched.
}
