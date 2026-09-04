test_that("kernel_port reads the port of a clean kernel URL", {
  expect_identical(kernel_port("http://127.0.0.1:4751/"), 4751L)
  expect_identical(kernel_port("http://localhost:4747"), 4747L)
  expect_identical(kernel_port("not a url"), NA_integer_)
})

test_that("valid_kernel_url is loopback-only and may not disagree with its record", {
  expect_true(valid_kernel_url("http://127.0.0.1:4751/", 4751L))
  expect_true(valid_kernel_url("http://localhost:4751/?token=legacy", 4751L))
  expect_true(valid_kernel_url("http://[::1]:4751/"))
  expect_false(valid_kernel_url("http://example.com:4751/", 4751L))
  expect_false(valid_kernel_url("http://127.0.0.1:4752/", 4751L))
  expect_false(valid_kernel_url("https://127.0.0.1:4751/"))
  expect_false(valid_kernel_url(""))
  expect_false(valid_kernel_url(NA_character_))
  expect_false(valid_kernel_url(4751))
})

test_that("kernel_base strips the trailing slash and a legacy token", {
  expect_identical(kernel_base("http://127.0.0.1:4751/"), "http://127.0.0.1:4751")
  expect_identical(kernel_base("http://127.0.0.1:4751/?token=abc"), "http://127.0.0.1:4751")
})

test_that("valid_site accepts an exact origin and nothing more", {
  expect_true(valid_site("https://book.example"))
  expect_true(valid_site("http://localhost:8080"))
  expect_true(valid_site("https://a-b.example.edu:443"))
  expect_false(valid_site("https://book.example/"))
  expect_false(valid_site("https://book.example/chapter.html"))
  expect_false(valid_site("book.example"))
  expect_false(valid_site("ftp://book.example"))
  expect_false(valid_site(c("https://a.example", "https://b.example")))
  expect_false(valid_site(NA_character_))
  expect_false(valid_site(paste0("https://", strrep("a", 600))))
})

test_that("check_port is a classed contract", {
  expect_true(check_port(4747))
  expect_error(check_port(4747.5), class = "carmar_bad_port")
  expect_error(check_port(0), class = "carmar_bad_port")
  expect_error(check_port(70000), class = "carmar_bad_port")
  expect_error(check_port("4747"), class = "carmar_bad_port")
  expect_error(check_port(c(4747, 4748)), class = "carmar_bad_port")
})
