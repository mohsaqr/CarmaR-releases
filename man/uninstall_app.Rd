\name{uninstall_app}
\alias{uninstall_app}
\title{Remove the launcher install_app() created}
\usage{
uninstall_app(quiet = FALSE)
}
\arguments{
\item{quiet}{Say nothing? Default \code{FALSE}.}
}
\value{
Invisibly, \code{TRUE} if something was removed.
}
\description{
Removes only what \code{\link{install_app}} made --- the package, your
notebooks and your R are untouched.
}
