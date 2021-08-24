test_that("bulma_media is properly formatted", {
  empty <- bulma_media()
  expect_tag_classed_type(empty, "article", "media")
  pure_text <- bulma_media(left = NULL, content = NULL, right = NULL,
                            container = htmltools::div,
                           "Direct Text", id = "test")
  expect_tag_classed_type(pure_text, "div", "media")
  expect_tag_children(pure_text, "text_node", NULL)
  expect_tag_attrib(pure_text, "id", "test")
  left_right <- bulma_media(htmltools::img(),
                            htmltools::div(),
                            htmltools::a())
  expect_tag_children(left_right, c("figure", "div", "div"),
                      c("media-left", "media-content", "media-right"))
  expect_tag_children(left_right$children[[1]],
                      "img", NULL)
  expect_tag_children(left_right$children[[2]],
                      "div", NULL)
  expect_tag_children(left_right$children[[3]],
                      "a", NULL)
})
