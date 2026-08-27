\name{install_app}
\alias{install_app}
\title{Install CarmaR as an application on this computer}
\usage{
install_app(quiet = FALSE, helper = TRUE, keep_ready = FALSE)
}
\arguments{
\item{quiet}{Say nothing on success? Default \code{FALSE}.}
\item{helper}{On macOS, also install the optional CarmaR menu helper? Default \code{TRUE}.}
\item{keep_ready}{On macOS, install the optional login service that keeps one local R kernel ready? Default \code{FALSE}.}
}
\value{
Invisibly, the path of what was created.
}
\description{
Creates a native launcher --- CarmaR in \verb{~/Applications} on macOS, in
the Start menu on Windows, in the application menu on Linux --- so starting
CarmaR is a click, with R and the console nowhere in sight. On macOS the
release-built app and CarmaR Helper are installed together from the package.
Because R performs the local installation, they are not given a
browser-download quarantine marker. Launching CarmaR starts the helper menu,
which lists and controls local sessions while keeping R's background processes
out of the Dock. Published Quarto
pages continue to authorize their exact web origin through CarmaR.app; the
helper does not relay execution. Other platforms use a small launcher that
finds R fresh and repairs a missing package automatically. Typing
\code{carmar::run()} keeps working exactly as before.
}
