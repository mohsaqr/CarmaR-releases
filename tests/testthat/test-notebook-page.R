test_that("notebook_launch_url pins carmar_V<kernel_build>.html, never a higher file beside it", {
  local_scratch_state()
  dist <- tempfile("carmar-dist-"); dir.create(dist)
  writeLines("<html>", file.path(dist, "carmar_V0.0.1-test.html"))
  writeLines("<html>", file.path(dist, "carmar_V0.0.2-test.html"))
  local_mocked_bindings(slurp = function(u) health_json("0.0.1-test"))
  pinned <- notebook_launch_url("http://127.0.0.1:5002/", dist = dist)
  expect_match(pinned, "^file://")
  expect_match(pinned, "/carmar_V0\\.0\\.1-test\\.html#kernel=5002$")
})

test_that("a build whose page is installed nowhere is a classed error naming that build", {
  local_scratch_state()
  dist <- tempfile("carmar-dist-"); dir.create(dist)
  writeLines("<html>", file.path(dist, "carmar_V0.0.2-test.html"))
  local_mocked_bindings(slurp = function(u) health_json("0.0.3-test"))
  err <- expect_error(notebook_launch_url("http://127.0.0.1:5002/", dist = dist),
                      class = "carmar_no_page")
  expect_match(conditionMessage(err), "0.0.3-test", fixed = TRUE)
  expect_false(grepl("0.0.2-test", conditionMessage(err), fixed = TRUE))
})

test_that("a kernel that does not name its build, or names a path, is refused", {
  local_scratch_state()
  dist <- tempfile("carmar-dist-"); dir.create(dist)
  local_mocked_bindings(slurp = function(u) '{"ok":true,"worker":true}')
  expect_error(notebook_launch_url("http://127.0.0.1:5002/", dist = dist),
               class = "carmar_no_page")
  local_mocked_bindings(slurp = function(u) '{"ok":true,"worker":true,"kernel_build":"../etc"}')
  expect_error(notebook_launch_url("http://127.0.0.1:5002/", dist = dist),
               class = "carmar_no_page")
})

test_that("the launch capability a kernel announced rides the regenerated page URL", {
  dirs <- local_scratch_state()
  dist <- tempfile("carmar-dist-"); dir.create(dist)
  writeLines("<html>", file.path(dist, "carmar_V1.html"))
  write_runtime_record(dirs$runtime, 5003L,
                       file = "file:///tmp/carmar_V1.html#kernel=5003&pair=AbC123xyz")
  write_runtime_record(dirs$runtime, 5002L)
  local_mocked_bindings(slurp = function(u) health_json("1"))
  expect_identical(recorded_pair(5003), "AbC123xyz")
  expect_identical(recorded_pair(5002), "")
  expect_identical(recorded_pair(NA), "")
  expect_identical(recorded_pair("x"), "")
  expect_match(notebook_launch_url("http://127.0.0.1:5003/", dist = dist),
               "#kernel=5003&pair=AbC123xyz$")
  expect_match(notebook_launch_url("http://127.0.0.1:5002/", dist = dist),
               "#kernel=5002$")
})

test_that("open_notebook hands the browser a 0600 trampoline that keeps the fragment", {
  dirs <- local_scratch_state()
  handed <- character()
  tramp <- open_notebook("file:///tmp/carmar_V1.html#kernel=5003&pair=AbC123xyz",
                         dirs$state, browse = function(u) handed <<- c(handed, u))
  expect_identical(handed, tramp)
  expect_match(basename(tramp), "^open-[0-9]+\\.html$")
  src <- paste(readLines(tramp, warn = FALSE), collapse = "")
  expect_match(src, 'url=file:///tmp/carmar_V1.html#kernel=5003&amp;pair=AbC123xyz"', fixed = TRUE)
  expect_match(src, "location.replace", fixed = TRUE)
  skip_on_os("windows")
  expect_identical(format(file.info(tramp)$mode), "600")
})

test_that("a URL with no fragment is opened directly", {
  dirs <- local_scratch_state()
  handed <- character()
  open_notebook("http://127.0.0.1:5003/", dirs$state, browse = function(u) handed <<- c(handed, u))
  expect_identical(handed, "http://127.0.0.1:5003/")
})
