\name{sessions}
\alias{sessions}
\title{List CarmaR sessions}
\usage{
sessions()
}
\value{
A data frame with one row per recorded session and columns \code{port},
\code{alive}, \code{title} (what the attached notebook calls itself, \code{NA}
until a page names one), \code{listen} (a headless kernel waiting for
published pages), \code{page}, and \code{source}.
}
\description{
Lists package-launched and runtime-registered CarmaR kernels, one row per
port, and checks whether each is currently alive. This operation is read-only.
}
\examples{
\dontrun{
carmar::sessions()
carmar::stop_kernel("all")
}
}
