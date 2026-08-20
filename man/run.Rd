\name{run}
\alias{run}
\title{Launch CarmaR}
\usage{
run(port = 4747, open = TRUE, new = TRUE, allow_more = NULL)
}
\arguments{
\item{port}{Port for the kernel. The default 4747 is deliberate and fixed:
browsers keep a page's saved notebooks per address, so a kernel that moved
ports would greet the student with an empty notebook every time.}

\item{open}{Open the browser once the kernel answers? Default \code{TRUE}.}

\item{new}{Start an independent session? Default \code{TRUE}. Set \code{FALSE}
to reopen a live session on \code{port} when there is one.}

\item{allow_more}{Confirmation override after five live sessions. Leave
\code{NULL} for an explicit prompt, \code{TRUE} to allow, or \code{FALSE} to
reopen the newest existing session.}
}
\value{
Invisibly, the versioned notebook file URL with its kernel selected.
}
\description{
Starts an independent local CarmaR kernel and opens the notebook in the
default browser. Up to five sessions start directly; starting a sixth asks
first. The kernel is a
separate R process; closing the R session that called \code{run()} does not
close the notebook.
}
\examples{
\dontrun{
carmar::run()
carmar::run(new = FALSE) # reopen the session on port 4747
}
}
