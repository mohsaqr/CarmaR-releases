ext_files <- c("carmar.lua", "carmar-publish.js", "carmar-publish.css")

fresh_project <- function(yml = NULL) {
  p <- tempfile("qproj")
  dir.create(p)
  if (!is.null(yml)) writeLines(yml, file.path(p, "_quarto.yml"))
  p
}

test_that("enable_carmar_filter creates, appends, keeps order and refuses", {
  yml <- file.path(tempfile("y"), "_quarto.yml"); dir.create(dirname(yml))
  expect_true(enable_carmar_filter(yml))
  expect_true(any(grepl("^\\s*-\\s*carmar\\s*$", readLines(yml))))

  writeLines(c("project:", "  type: book", "", "filters:", "  - section-bibliographies",
               "", "format:", "  html: default"), yml)
  expect_true(enable_carmar_filter(yml))
  lines <- readLines(yml)
  expect_identical(lines[4:6], c("filters:", "  - section-bibliographies", "  - carmar"))
  expect_identical(lines[c(1:3, 7:9)],
                   c("project:", "  type: book", "", "", "format:", "  html: default"))

  expect_message(expect_true(enable_carmar_filter(yml)), "already enabled")
  expect_identical(sum(grepl("^\\s*-\\s*carmar\\s*$", readLines(yml))), 1L)

  writeLines("filters: [section-bibliographies]", yml)
  expect_message(expect_false(enable_carmar_filter(yml)), "inline filters list")
  expect_identical(readLines(yml), "filters: [section-bibliographies]")

  writeLines(c("filters:", "  - a", "filters:", "  - b"), yml)
  expect_message(expect_false(enable_carmar_filter(yml)), "several")

  writeLines(c("project:", "  type: website"), yml)
  expect_true(enable_carmar_filter(yml))
  expect_identical(utils::tail(readLines(yml), 2), c("filters:", "  - carmar"))
})

test_that("write_carmar_options appends a block once and never touches an existing one", {
  yml <- tempfile(fileext = ".yml")
  writeLines(c("filters:", "  - carmar"), yml)
  expect_true(write_carmar_options(yml))
  lines <- readLines(yml)
  expect_true(any(lines == "carmar:"))
  expect_true(any(lines == "  port: 4747"))
  expect_true(any(lines == "  label: Run on my computer"))
  expect_true(write_carmar_options(yml))
  expect_identical(readLines(yml), lines)
  writeLines(c("carmar:", "  port: 4800"), yml)
  expect_true(write_carmar_options(yml))
  expect_identical(readLines(yml), c("carmar:", "  port: 4800"))
})

test_that("use_publishing installs the extension, enables the filter and writes the options", {
  src <- extension_source_for_tests()
  p <- fresh_project()
  res <- suppressMessages(use_publishing(p, extension_source = src))
  expect_s3_class(res, c("carmar_publishing", "data.frame"))
  expect_identical(names(res), c("project", "extension", "config", "enabled", "options"))
  expect_identical(nrow(res), 1L)
  expect_true(res$enabled)
  expect_true(res$options)
  expect_identical(res$config, file.path(p, "_quarto.yml"))
  expect_true(all(file.exists(file.path(p, "_extensions", "carmar", ext_files))))
  lines <- readLines(res$config)
  expect_true(any(grepl("^\\s*-\\s*carmar\\s*$", lines)))
  expect_true(any(lines == "carmar:"))
  expect_output(print(res), "publishing is enabled")
})

test_that("use_publishing is idempotent", {
  src <- extension_source_for_tests()
  p <- fresh_project(c("project:", "  type: book", "", "filters:", "  - section-bibliographies"))
  suppressMessages(use_publishing(p, extension_source = src))
  before <- readLines(file.path(p, "_quarto.yml"))
  suppressMessages(use_publishing(p, extension_source = src))
  expect_identical(readLines(file.path(p, "_quarto.yml")), before)
  expect_identical(sum(grepl("^\\s*-\\s*carmar\\s*$", before)), 1L)
  expect_identical(sum(before == "carmar:"), 1L)
})

test_that("YAML that cannot be edited safely is left byte-identical, extension still installed", {
  src <- extension_source_for_tests()
  p <- fresh_project("filters: [section-bibliographies]")
  res <- suppressMessages(use_publishing(p, extension_source = src))
  expect_identical(readLines(file.path(p, "_quarto.yml")), "filters: [section-bibliographies]")
  expect_false(res$enabled)
  expect_false(res$options)
  expect_true(all(file.exists(file.path(p, "_extensions", "carmar", ext_files))))
  expect_output(print(res), "left for you to edit")
})

test_that("use_publishing error paths are classed", {
  expect_error(use_publishing(tempfile("missing")), "existing directory")
  expect_error(use_publishing(fresh_project(), extension_source = tempfile("noext")),
               class = "carmar_no_extension")
  bare <- tempfile("bare"); dir.create(bare)
  file.create(file.path(bare, "carmar.lua"))
  expect_error(use_publishing(fresh_project(), extension_source = bare), "both runtime assets")
})

rendered_site <- function() {
  site <- tempfile("site")
  libs <- file.path(site, "site_libs", "carmar-published-0.1.0")
  dir.create(libs, recursive = TRUE)
  dir.create(file.path(site, "chapters"))
  file.create(file.path(libs, c("carmar-publish.js", "carmar-publish.css")))
  page <- function(prefix, port = 4747, label = "Run on my computer") c(
    "<html><head>",
    sprintf('<meta name="carmar-port" content="%d">', port),
    sprintf('<meta name="carmar-label" content="%s">', label),
    sprintf('<script src="%ssite_libs/carmar-published-0.1.0/carmar-publish.js" defer></script>', prefix),
    sprintf('<link href="%ssite_libs/carmar-published-0.1.0/carmar-publish.css" rel="stylesheet">', prefix),
    "</head><body></body></html>")
  writeLines(page(""), file.path(site, "index.html"))
  writeLines(page("../", 4800, "Run it &amp; see"), file.path(site, "chapters", "one.html"))
  writeLines("<html><body>no filter</body></html>", file.path(site, "about.html"))
  site
}

test_that("check_publishing reports one row per page with port, label and assets", {
  site <- rendered_site()
  out <- check_publishing(site)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("file", "port", "label", "assets"))
  expect_identical(out$file, c("about.html", "chapters/one.html", "index.html"))
  expect_identical(out$port, c(NA_integer_, 4800L, 4747L))
  expect_identical(out$label, c(NA_character_, "Run it & see", "Run on my computer"))
  expect_identical(out$assets, c(FALSE, TRUE, TRUE))
})

test_that("check_publishing sees missing assets, an empty site, and a missing site", {
  site <- rendered_site()
  unlink(file.path(site, "site_libs"), recursive = TRUE)
  out <- check_publishing(site)
  expect_identical(out$assets, c(FALSE, FALSE, FALSE))
  expect_identical(out$port, c(NA_integer_, 4800L, 4747L))

  empty <- tempfile("empty"); dir.create(empty)
  none <- check_publishing(empty)
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), c("file", "port", "label", "assets"))

  expect_error(check_publishing(tempfile("gone")), class = "carmar_no_site")
  expect_error(check_publishing(c("a", "b")), "single path")
})
