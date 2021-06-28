test_that("unknown theme raises an error", {
  expect_error(bulma_page(theme = "unknown", "theme '.*' is not installed"))
  skip_if(identical(Sys.getenv("NOT_CRAN"), "true"), "Not on CRAN")
  expect_error(bulma_page(theme = "flatly", "theme '.*' is not installed"))
})

test_that("bulma requirements are met", {
  html <- htmltools::renderTags(bulma_page())
  deps <- html$dependencies
  expect_length(deps, 1L)
  bulma_dep <- deps[[1L]]
  expect_match(bulma_dep$meta$viewport,
               "width\\s*=\\s*device-width,\\s*initial-scale\\s*=\\s*1")

  css_src <- paste0(bulma_dep$src$file, "/", bulma_dep$stylesheet) %>%
    normalizePath(mustWork = FALSE)
  expect_true(file.exists(css_src))

  css <- readLines(css_src, warn = FALSE) %>%
    paste(collapse = "\n")

  ## check some classes to make sure it is a "valid" bulma css
  expect_match(css, "\\.columns")
  expect_match(css, "\\.container")
  expect_match(css, "\\.has-text-primary")
  expect_match(css, "\\.is-size-1")
  expect_match(css, "\\.is-pulled-right")
  expect_match(css, "\\.field")
  expect_match(css, "\\.has-addons")
})

test_that("title is properly displayed", {
  html <- htmltools::renderTags(bulma_page(title = "Test"))
  expect_match(html$head, "<title>Test</title>")
})
