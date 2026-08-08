\name{upgrade}
\alias{upgrade}
\title{Upgrade the CarmaR notebook}
\usage{
upgrade(quiet = FALSE)
}
\arguments{
\item{quiet}{Say nothing unless something was updated? Default \code{FALSE}.}
}
\value{
Invisibly, \code{TRUE} if a newer notebook was downloaded.
}
\description{
Downloads the newest notebook from the course's release page into your user
data folder, where the kernel finds it on the next reload. This is the fast
channel: the notebook is one small file and a release is live seconds after
the instructor publishes it --- no package rebuild, no reinstall.
\code{run()} also checks quietly whenever it starts a kernel.
}
