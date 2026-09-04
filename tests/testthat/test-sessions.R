test_that("sessions() with no records is a tidy empty data.frame", {
  local_scratch_state()
  empty <- sessions()
  expect_s3_class(empty, "data.frame")
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("port", "alive", "title", "listen", "page", "source"))
})

test_that("sessions() pools package and runtime records, one row per port", {
  dirs <- local_scratch_state()
  writeLines("http://127.0.0.1:5001/", file.path(dirs$state, "url-5001"))
  write_runtime_record(dirs$runtime, 5002L, title = "My analysis", listen = TRUE)
  local_mocked_bindings(
    kernel_alive = function(u) grepl(":5002/", u, fixed = TRUE),
    notebook_launch_url = function(u, ...) paste0("file:///x/carmar_V1.html#kernel=", kernel_port(u)))
  listed <- sessions()
  expect_identical(listed$port, c(5001L, 5002L))
  expect_identical(listed$source, c("package", "runtime"))
  expect_identical(listed$alive, c(FALSE, TRUE))
  expect_identical(listed$title, c(NA_character_, "My analysis"))
  expect_identical(listed$listen, c(FALSE, TRUE))
  expect_true(is.na(listed$page[[1L]]))
  expect_match(listed$page[[2L]], "#kernel=5002$")
})

test_that("a valid runtime duplicate wins over a malformed package record", {
  dirs <- local_scratch_state()
  writeLines("not a URL", file.path(dirs$state, "url-5003"))
  write_runtime_record(dirs$runtime, 5003L)
  # A runtime record whose declared port disagrees with its file name is never live.
  jsonlite::write_json(list(url = "http://127.0.0.1:5004/", port = 5999L, pid = 1L),
                       file.path(dirs$runtime, "kernel-5004.json"), auto_unbox = TRUE)
  local_mocked_bindings(
    kernel_alive = function(u) TRUE,
    notebook_launch_url = function(u, ...) "file:///x/carmar_V1.html#kernel=5003")
  listed <- sessions()
  expect_identical(nrow(listed), 2L)
  row5003 <- subset(listed, port == 5003L)
  row5004 <- subset(listed, port == 5004L)
  expect_true(row5003$alive)
  expect_identical(row5003$source, "runtime")
  expect_false(row5004$alive)
})

test_that("a live kernel whose page is installed nowhere still lists, with page NA", {
  dirs <- local_scratch_state()
  write_runtime_record(dirs$runtime, 5005L)
  local_mocked_bindings(kernel_alive = function(u) TRUE,
                        slurp = function(u) '{"ok":true,"worker":true}')
  listed <- sessions()
  expect_true(listed$alive)
  expect_true(is.na(listed$page))
})

test_that("stop_kernel() on a port with nothing recorded is FALSE, not an error", {
  local_scratch_state()
  expect_message(out <- stop_kernel(65000), "No CarmaR kernel recorded")
  expect_false(out)
  expect_error(stop_kernel(5002.5), class = "carmar_bad_port")
})

test_that("stop_kernel() on a dead record removes the package record and says so", {
  dirs <- local_scratch_state()
  writeLines("http://127.0.0.1:5001/", file.path(dirs$state, "url-5001"))
  local_mocked_bindings(kernel_alive = function(u) FALSE)
  expect_message(out <- stop_kernel(5001), "No CarmaR kernel is running")
  expect_false(out)
  expect_false(file.exists(file.path(dirs$state, "url-5001")))
})

test_that("stop_kernel('all') asks every live kernel and returns port + stopped", {
  dirs <- local_scratch_state()
  writeLines("http://127.0.0.1:5001/", file.path(dirs$state, "url-5001"))
  write_runtime_record(dirs$runtime, 5002L)
  asked <- character()
  local_mocked_bindings(
    kernel_alive = function(u) grepl(":5002/", u, fixed = TRUE),
    notebook_launch_url = function(u, ...) "file:///x/carmar_V1.html#kernel=5002",
    slurp = function(u) { asked <<- c(asked, u); "ok" })
  stopped <- suppressMessages(stop_kernel("all"))
  expect_s3_class(stopped, "data.frame")
  expect_identical(names(stopped), c("port", "stopped"))
  expect_identical(stopped$port, 5002L)
  expect_true(stopped$stopped)
  expect_identical(asked, "http://127.0.0.1:5002/shutdown")
})

test_that("stop_kernel('all') with nothing live is a zero-row data.frame", {
  local_scratch_state()
  local_mocked_bindings(kernel_alive = function(u) FALSE)
  expect_message(none <- stop_kernel("all"), "No CarmaR kernels are running")
  expect_s3_class(none, "data.frame")
  expect_identical(names(none), c("port", "stopped"))
  expect_identical(nrow(none), 0L)
})

test_that("live_kernel_urls prunes dead records and keeps one URL per kernel", {
  dirs <- local_scratch_state()
  writeLines("http://127.0.0.1:5001/", file.path(dirs$state, "url-5001"))
  write_runtime_record(dirs$runtime, 5001L)
  write_runtime_record(dirs$runtime, 5002L)
  local_mocked_bindings(kernel_alive = function(u) grepl(":5001/", u, fixed = TRUE))
  live <- live_kernel_urls(dirs$state)
  expect_identical(live, "http://127.0.0.1:5001/")
  expect_false(file.exists(file.path(dirs$runtime, "kernel-5002.json")))
})
