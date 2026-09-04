test_that("kernel_health is NULL for anything that is not a loopback kernel URL", {
  # No request is made for a non-loopback URL, so no mock is needed.
  expect_null(kernel_health("http://example.com:5002/"))
  expect_null(kernel_health("nonsense"))
})

test_that("an unrelated HTTP response is not a healthy kernel", {
  local_mocked_bindings(slurp = function(u) "this is not CarmaR")
  expect_false(kernel_alive("http://127.0.0.1:4751/"))
  expect_null(kernel_health("http://127.0.0.1:4751/"))
})

test_that("health requires a live worker, not only valid JSON", {
  local_mocked_bindings(slurp = function(u) '{"ok":true,"worker":false}')
  expect_false(kernel_alive("http://127.0.0.1:4751/"))
})

test_that("the exact CarmaR health shape is accepted and pairing is a capability", {
  local_mocked_bindings(slurp = function(u) '{"ok":true,"worker":true}')
  expect_true(kernel_alive("http://127.0.0.1:4751/"))
  expect_false(kernel_supports_published_pairing("http://127.0.0.1:4751/"))
  local_mocked_bindings(slurp = function(u) health_json(pairing = TRUE))
  expect_true(kernel_supports_published_pairing("http://127.0.0.1:4751/"))
})

test_that("an unreachable kernel is not alive and raises nothing", {
  local_mocked_bindings(slurp = function(u) stop("connection refused"))
  expect_false(kernel_alive("http://127.0.0.1:4751/"))
})

test_that("authorize_published_origin asks /published/authorize as a native client", {
  asked <- character()
  local_mocked_bindings(slurp = function(u) {
    asked <<- c(asked, u)
    '{"ok":true,"origin":"https://book.example"}'
  })
  expect_true(authorize_published_origin("http://127.0.0.1:4751/", "https://book.example"))
  expect_identical(asked,
    "http://127.0.0.1:4751/published/authorize?origin=https%3A%2F%2Fbook.example")
})

test_that("an approval for a different origin, or a refusal, is not an approval", {
  local_mocked_bindings(slurp = function(u) '{"ok":true,"origin":"https://other.example"}')
  expect_false(authorize_published_origin("http://127.0.0.1:4751/", "https://book.example"))
  local_mocked_bindings(slurp = function(u) '{"ok":false,"error":"invalid origin"}')
  expect_false(authorize_published_origin("http://127.0.0.1:4751/", "https://book.example"))
  local_mocked_bindings(slurp = function(u) stop("403"))
  expect_false(authorize_published_origin("http://127.0.0.1:4751/", "https://book.example"))
})
