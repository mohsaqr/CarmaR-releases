\name{run_published}
\alias{run_published}
\title{Start CarmaR for a published book}
\usage{
run_published(port = 4747, open = FALSE)
}
\arguments{
\item{port}{Fixed loopback port used by the published page. Default 4747.}
\item{open}{Open the full local notebook too? Published books normally leave
this \code{FALSE}.}
}
\value{
Invisibly, the local notebook URL returned by \code{run()}.
}
\description{
Reuses a published-page-capable kernel on \code{port}. If that port contains
an older CarmaR kernel, it is cleanly restarted from the current package.
}
