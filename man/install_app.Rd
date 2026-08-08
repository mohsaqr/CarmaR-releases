\name{install_app}
\alias{install_app}
\title{Install CarmaR as an application on this computer}
\usage{
install_app(quiet = FALSE)
}
\arguments{
\item{quiet}{Say nothing on success? Default \code{FALSE}.}
}
\value{
Invisibly, the path of what was created.
}
\description{
Creates a native launcher --- CarmaR in \verb{~/Applications} on macOS, in
the Start menu on Windows, in the application menu on Linux --- so starting
CarmaR is a click, with R and the console nowhere in sight. The launcher is
manufactured locally by this function, which is why macOS shows no
"unidentified developer" dialog: nothing was downloaded, so there is nothing
for Gatekeeper to quarantine. The launcher finds R fresh on every click and
reinstalls carmar if it has gone missing, so it survives R upgrades and
accidental removals. Typing \code{carmar::run()} keeps working exactly as
before --- the launcher is the same call with an icon on it.
}
