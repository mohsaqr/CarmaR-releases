# make_runnable(): one tag before </head>, once, into already rendered pages.

page <- function(dir, name, body = "<pre class=\"r\"><code>1 + 1</code></pre>", head = TRUE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, name)
  writeLines(if (head) c("<html><head><title>t</title></head>", paste0("<body>", body, "</body></html>"))
             else c("<html>", paste0("<body>", body, "</body></html>")), f)
  f
}

test_that("a directory of rendered pages is stamped once, with the CDN runtime", {
  root <- file.path(tempdir(), "runnable-a")
  unlink(root, recursive = TRUE)
  page(root, "index.html")
  page(file.path(root, "chapters"), "two.html")
  res <- make_runnable(root)
  expect_s3_class(res, c("carmar_runnable", "data.frame"))
  expect_equal(nrow(res), 2L)
  expect_true(all(res$stamped))
  expect_equal(unique(res$assets), "https://cdn.jsdelivr.net/gh/mohsaqr/CarmaR-releases@main/inst/quarto/_extensions/carmar/carmar-publish.js")
  html <- paste(readLines(file.path(root, "index.html")), collapse = "\n")
  expect_match(html, '<meta name="carmar-port" content="4747">', fixed = TRUE)
  expect_match(html, '<script defer src="https://cdn.jsdelivr.net/gh/mohsaqr/CarmaR-releases@main/inst/quarto/_extensions/carmar/carmar-publish.js"></script></head>', fixed = TRUE)
  # idempotent: a second call changes no byte and reports nothing stamped
  before <- readLines(file.path(root, "index.html"))
  again <- make_runnable(root)
  expect_false(any(again$stamped))
  expect_identical(readLines(file.path(root, "index.html")), before)
  expect_output(print(again), "0 HTML pages made runnable \\(2 already were\\)")
})

test_that("local assets are copied beside the pages and referenced relatively", {
  skip_if(!nzchar(system.file("quarto", "_extensions", "carmar", package = "carmar")),
          "the runtime is staged only in a built package")
  root <- file.path(tempdir(), "runnable-b")
  unlink(root, recursive = TRUE)
  page(root, "index.html")
  page(file.path(root, "deep", "er"), "page.html")
  res <- make_runnable(root, assets = "local", port = 4750, label = "Run <here>")
  expect_true(file.exists(file.path(root, "carmar", "carmar-publish.js")))
  expect_true(file.exists(file.path(root, "carmar", "carmar-publish.css")))
  expect_equal(res$assets[res$file == normalizePath(file.path(root, "index.html"), winslash = "/")],
               "carmar/carmar-publish.js")
  expect_equal(res$assets[grepl("deep/er/page.html$", res$file)], "../../carmar/carmar-publish.js")
  html <- paste(readLines(file.path(root, "index.html")), collapse = "\n")
  expect_match(html, 'content="4750"', fixed = TRUE)
  expect_match(html, 'content="Run &lt;here&gt;"', fixed = TRUE)
})

test_that("a page with no <head> gets the tags first; one file works; errors are classed", {
  root <- file.path(tempdir(), "runnable-c")
  unlink(root, recursive = TRUE)
  f <- page(root, "bare.html", head = FALSE)
  res <- make_runnable(f)
  expect_equal(nrow(res), 1L)
  expect_match(readLines(f)[[1L]], '^<meta name="carmar-port"')
  expect_error(make_runnable(file.path(root, "nothing-here")), class = "carmar_no_html")
  expect_error(make_runnable(root, assets = "not-a-prefix"), "URL prefix")
  expect_error(make_runnable(root, port = 0), "port")
})

test_that("relative_path walks up and down correctly", {
  expect_equal(carmar:::relative_path("/a/b/c", "/a/b/carmar"), "../carmar")
  expect_equal(carmar:::relative_path("/a/b", "/a/b/carmar"), "carmar")
  expect_equal(carmar:::relative_path("/a/b", "/a/b"), ".")
})
