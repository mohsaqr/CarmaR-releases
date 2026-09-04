#' @keywords internal
#' @section The verbs:
#' [run()] opens the notebook on your own R. [listen()] keeps a kernel
#' waiting for the runnable chunks of a published page. [use_publishing()]
#' and [check_publishing()] are the author's side of that page.
#' [sessions()] and [stop_kernel()] list and stop what is running.
#' [serve_shared()] is the one-kernel-per-person deployment behind a reverse
#' proxy.
#'
#' Nothing in this package contacts the network. Every request goes to a
#' kernel on 127.0.0.1 that the user started.
"_PACKAGE"

# httpuv is not called from this file. It is the HTTP and WebSocket server
# the kernel process (inst/app/kernel/serve.R) is built on, and the kernel is
# spawned by run()/listen()/serve_shared() as a separate Rscript: declaring
# the dependency here is what makes install.packages("carmar") install the
# server the kernel needs. One importFrom keeps R CMD check from reporting
# the declared Import as unused.
#' @importFrom httpuv startServer
NULL
