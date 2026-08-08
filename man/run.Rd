\name{run}
\alias{run}
\title{Launch CarmaR}
\usage{
run(port = 4747, open = TRUE)
}
\arguments{
\item{port}{Port for the kernel. The default 4747 is deliberate and fixed:
browsers keep a page's saved notebooks per address, so a kernel that moved
ports would greet the student with an empty notebook every time.}

\item{open}{Open the browser once the kernel answers? Default \code{TRUE}.}
}
\value{
Invisibly, the notebook URL (with its access token).
}
\description{
Starts the local CarmaR kernel --- or reuses one already running on the same
port --- and opens the notebook in the default browser. The kernel is a
separate R process; closing the R session that called \code{run()} does not
close the notebook.
}
\examples{
\dontrun{
carmar::run()
}
}
