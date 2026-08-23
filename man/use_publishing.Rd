\name{use_publishing}
\alias{use_publishing}
\title{Make a Quarto project's R chunks runnable with CarmaR}
\usage{
use_publishing(
  project = ".",
  extension_source = system.file("quarto", "_extensions", "carmar",
    package = "carmar")
)
}
\arguments{
\item{project}{Path to the Quarto project (the directory holding
\code{_quarto.yml}, or where one should be created). Default: the working
directory.}

\item{extension_source}{Directory holding the extension files. The default is
the copy shipped inside the installed carmar package; a development checkout
can pass its own \code{_extensions/carmar} instead.}
}
\value{
Invisibly, a list with \code{extension} (the destination directory),
\code{config} (the \code{_quarto.yml} path), and \code{enabled} (\code{TRUE}
when the filter is active in that file, \code{FALSE} when the file was left
for the author).
}
\description{
Copies the CarmaR Quarto extension into \code{project/_extensions/carmar} and
enables the \code{carmar} filter in the project's \code{_quarto.yml}. After
the next \code{quarto render}, every visible R chunk on the published pages
gains Run and Edit controls against each reader's own local R.

Idempotent: running it again refreshes the extension files (JS and CSS
together) and leaves an already-enabled \code{_quarto.yml} untouched. A
\code{_quarto.yml} it cannot edit safely (an inline \code{filters: [...]}
list, or several top-level \code{filters:} keys) is left exactly as it was,
and the lines to add are printed instead.
}
\examples{
\dontrun{
carmar::use_publishing("~/my-book")
# then: quarto render
}
}
