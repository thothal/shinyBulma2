test_that("bulma_heading warns/stops when mal-formatted arguments are passed", {
  expect_error(bulma_heading(type = "h1"),
               "'arg' should be one of \"title\", \"subtitle\"")
  expect_error(bulma_heading(tag = "no_func"),
               "object 'no_func' of mode 'function' was not found")
  size_err_msg <- "size must be an integer between 1 and 6"
  invalid_sizes <- list(NA, 1:2, 0, 7, "small")
  lapply(invalid_sizes, function(size) {
    expect_error(bulma_heading(size = size),
                 size_err_msg)
  })
  expect_warning(bulma_heading(type = "subtitle", maintain_space = TRUE),
                 paste("`maintain_space = TRUE` makes only sense for titles",
                       "and not for subtitles"))
})

test_that("bulma_heading is properly formatted", {
  title <- bulma_heading("Test", tag = htmltools::h2,
                         size = 3, type = "title",
                         maintain_space = TRUE)
  expect_tag_classed_type(title, "h2", add_class("title",
                                                 make_class(c("3", "spaced"))))
  expect_tag_children(title, "text_node")
})

test_that("bulma_heading gives same result as bulma_{title|subtitle}", {
  expect_equal(bulma_heading("Test", type = "title"),
               bulma_title("Test"))
  expect_equal(bulma_heading("Test", type = "subtitle"),
               bulma_subtitle("Test"))
})
