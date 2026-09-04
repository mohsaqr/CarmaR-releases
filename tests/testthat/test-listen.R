# listen() without ever starting a kernel: every process edge is mocked at
# the seam the code itself uses (kernel_alive, port_is_free, spawn_kernel,
# authorize_published_origin). The real-kernel test is at the end, gated.

test_that("a site that is not an exact origin is refused before anything happens", {
  local_scratch_state()
  local_mocked_bindings(kernel_alive = function(u) stop("must not be reached"))
  expect_error(listen("https://book.example/chapter-1.html"), class = "carmar_bad_site")
  expect_error(listen("book.example"), class = "carmar_bad_site")
  expect_error(listen(c("https://a.example", "https://b.example")), class = "carmar_bad_site")
  expect_error(listen(port = 4747.5), class = "carmar_bad_port")
  expect_error(listen(open = NA), "open")
})

test_that("a port held by something that is not CarmaR is carmar_port_taken, never a move", {
  local_scratch_state()
  local_mocked_bindings(kernel_alive = function(u) FALSE,
                        port_is_free = function(port) FALSE,
                        spawn_kernel = function(...) stop("must not spawn"))
  err <- expect_error(listen(port = 4949), class = "carmar_port_taken")
  expect_match(conditionMessage(err), "4949")
})

test_that("a pairing-capable kernel already on the port is reused and the site approved", {
  local_scratch_state()
  approved_for <- character()
  local_mocked_bindings(
    kernel_alive = function(u) TRUE,
    kernel_supports_published_pairing = function(u) TRUE,
    authorize_published_origin = function(u, site) { approved_for <<- c(approved_for, site); TRUE },
    spawn_kernel = function(...) stop("must not spawn"))
  expect_message(out <- listen("https://book.example", port = 4949),
                 "listening on port 4949 for https://book.example")
  expect_s3_class(out, c("carmar_listener", "data.frame"))
  expect_identical(names(out), c("port", "url", "site", "approved", "started"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$port, 4949L)
  expect_identical(out$url, "http://127.0.0.1:4949/")
  expect_identical(out$site, "https://book.example")
  expect_true(out$approved)
  expect_false(out$started)
  expect_identical(approved_for, "https://book.example")
})

test_that("without a site, a reused kernel approves nothing and says so", {
  local_scratch_state()
  local_mocked_bindings(
    kernel_alive = function(u) TRUE,
    kernel_supports_published_pairing = function(u) TRUE,
    authorize_published_origin = function(u, site) stop("must not be asked"))
  out <- suppressMessages(listen(port = 4949))
  expect_true(is.na(out$site))
  expect_false(out$approved)
  expect_false(out$started)
  expect_output(print(out), "listening on port 4949 for published pages - open the page and press Run.",
                fixed = TRUE)
})

test_that("a kernel that refuses the approval is a classed error", {
  local_scratch_state()
  local_mocked_bindings(
    kernel_alive = function(u) TRUE,
    kernel_supports_published_pairing = function(u) TRUE,
    authorize_published_origin = function(u, site) FALSE)
  expect_error(listen("https://book.example", port = 4949), class = "carmar_not_approved")
})

test_that("a free port starts a strict, listening kernel carrying the site", {
  dirs <- local_scratch_state()
  seen <- NULL
  local_mocked_bindings(
    kernel_alive = function(u) FALSE,
    port_is_free = function(port) TRUE,
    spawn_kernel = function(port, state, env = character()) {
      seen <<- list(port = port, state = state, env = env)
      list(url = sprintf("http://127.0.0.1:%d/", port), page = NULL,
           log = file.path(state, "kernel.log"), log_lines = character())
    })
  out <- suppressMessages(listen("https://book.example", port = 4949))
  expect_true(out$started)
  expect_true(out$approved)
  expect_identical(seen$port, 4949L)
  expect_identical(seen$env[["CARMAR_PORT_STRICT"]], "1")
  expect_identical(seen$env[["CARMAR_LISTEN"]], "1")
  expect_identical(seen$env[["CARMAR_PUBLISHED_ORIGIN"]], "https://book.example")
  expect_identical(readLines(file.path(dirs$state, "url-4949")), "http://127.0.0.1:4949/")
})

test_that("a started kernel with no site passes an empty origin and is not approved", {
  local_scratch_state()
  seen <- NULL
  local_mocked_bindings(
    kernel_alive = function(u) FALSE,
    port_is_free = function(port) TRUE,
    spawn_kernel = function(port, state, env = character()) {
      seen <<- env
      list(url = sprintf("http://127.0.0.1:%d/", port), page = NULL, log = "", log_lines = character())
    })
  out <- suppressMessages(listen(port = 4949))
  expect_true(out$started)
  expect_false(out$approved)
  expect_identical(seen[["CARMAR_PUBLISHED_ORIGIN"]], "")
})

test_that("a kernel that refuses to start because the port was taken is carmar_port_taken", {
  local_scratch_state()
  local_mocked_bindings(
    kernel_alive = function(u) FALSE,
    port_is_free = function(port) TRUE,
    spawn_kernel = function(port, state, env = character()) list(
      url = NULL, page = NULL, log = "x.log",
      log_lines = "CarmaR refuses to start: CARMAR_PORT=4949 is already in use"))
  expect_error(listen(port = 4949), class = "carmar_port_taken")
})

test_that("a kernel that answered on another port is refused - a listener never moves", {
  local_scratch_state()
  local_mocked_bindings(
    kernel_alive = function(u) FALSE,
    port_is_free = function(port) TRUE,
    spawn_kernel = function(port, state, env = character()) list(
      url = "http://127.0.0.1:5050/", page = NULL, log = "", log_lines = character()))
  expect_error(listen(port = 4949), class = "carmar_port_taken")
})

test_that("a kernel that never announced itself is carmar_no_start with its log", {
  local_scratch_state()
  local_mocked_bindings(
    kernel_alive = function(u) FALSE,
    port_is_free = function(port) TRUE,
    spawn_kernel = function(port, state, env = character()) list(
      url = NULL, page = NULL, log = "/x/kernel.log", log_lines = c("a", "b")))
  err <- expect_error(listen(port = 4949), class = "carmar_no_start")
  expect_match(conditionMessage(err), "/x/kernel.log", fixed = TRUE)
})

test_that("run_published() is listen() under another name", {
  local_mocked_bindings(listen = function(site = NULL, port = 4747, open = FALSE)
    list(site = site, port = port, open = open))
  expect_identical(run_published("https://book.example", port = 4949, open = TRUE),
                   list(site = "https://book.example", port = 4949, open = TRUE))
  expect_identical(run_published(), list(site = NULL, port = 4747, open = FALSE))
})

test_that("kernel_dir prefers the installed package and honours CARMAR_DEV_KERNEL", {
  dev <- tempfile("spike-"); dir.create(dev)
  writeLines("# serve", file.path(dev, "serve.R"))
  local_envvar(CARMAR_DEV_KERNEL = dev)
  expect_identical(kernel_dir(), normalizePath(dev, winslash = "/"))
  local_envvar(CARMAR_DEV_KERNEL = tempfile("nowhere-"))
  expect_identical(kernel_dir(), system.file("app", "kernel", package = "carmar"))
})

test_that("a real listening kernel on a scratch port approves the site and stops cleanly", {
  kernel_tests_enabled()
  dirs <- local_scratch_state()
  local_envvar(CARMAR_NO_HISTORY = "1", CARMAR_NO_TERMINAL = "1")
  port <- free_port()
  out <- suppressMessages(listen("https://book.example", port = port))
  # after = FALSE: the stop must run BEFORE the scratch environment is
  # restored, or stop_kernel() looks for the record in the wrong directory
  # and the scratch kernel outlives the test.
  on.exit(suppressMessages(stop_kernel(port)), add = TRUE, after = FALSE)
  expect_true(out$started)
  expect_true(out$approved)
  expect_identical(out$port, as.integer(port))
  expect_true(kernel_alive(out$url))
  rec <- jsonlite::fromJSON(file.path(dirs$runtime, sprintf("kernel-%d.json", port)))
  expect_true(isTRUE(rec$listen))
  # A second call reuses it and approves a second site on the running kernel.
  again <- suppressMessages(listen("https://other.example", port = port))
  expect_false(again$started)
  expect_true(again$approved)
  wanted <- as.integer(port)
  expect_true(subset(sessions(), port == wanted)$listen)
  expect_true(suppressMessages(stop_kernel(port)))
})
