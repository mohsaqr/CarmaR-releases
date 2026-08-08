\name{stop_kernel}
\alias{stop_kernel}
\title{Stop the CarmaR kernel}
\usage{
stop_kernel(port = 4747)
}
\arguments{
\item{port}{The port \code{run()} used. Default 4747.}
}
\value{
Invisibly, \code{TRUE} if a kernel was asked to stop, \code{FALSE} if none
was running.
}
\description{
Asks the kernel on \code{port} to shut down, through its own token-gated
\code{/shutdown} --- the clean path, the same one the notebook's Quit uses.
}
