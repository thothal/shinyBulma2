test_that("bulma_tag warns/stops when mal-formatted arguments are passed", {
  expect_error(bulma_tag(style = "squared"),
               "'arg' should be one of \"delete\", \"rounded\"")
})

test_that("bulma_tag is properly formatted", {
  empty <- bulma_tag()
  expect_tag_classed_type(empty, "span", "tag")
  full <- bulma_tag("Test", "link", TRUE, "medium", "rounded", TRUE, "medium", htmltools::a, id = "mytag")
  expect_tag_classed_type(full, "a", c("tag", make_class(c("link", "light", "medium", "rounded"),
                                                         collapse = FALSE)))
  expect_tag_children(full, c("text_node", "button"), list(NULL, c("delete", make_class("medium"))),
                      partial = FALSE, strict = TRUE)
  expect_tag_attrib(full, "id", "mytag")
})

test_that("bulma_tags is properly formatted", {
  empty <- bulma_tags()
  expect_tag_classed_type(empty, "div", "tags")
  full <- bulma_tags(bulma_tag(), bulma_tag(), addons = TRUE, size = "large", container = htmltools::p,
                     style = "background-color: #123123")
  expect_tag_classed_type(full, "p", c("tags", make_class(c("addons", "large"),
                                                          prefix = c("has", "are"),
                                                         collapse = FALSE)))
  expect_tag_children(full, c("span", "span"), c("tag", "tag"),
                      partial = FALSE, strict = TRUE)
  expect_tag_attrib(full, "style", "background-color: #123123")
})


