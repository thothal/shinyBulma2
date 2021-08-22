test_that("bulma elements are properly formatted", {
  expect_tag_classed_type(bulma_block(), "div", "block")
  expect_tag_classed_type(bulma_box(), "div", "box")
  expect_tag_classed_type(bulma_content(), "div",
                          c("content"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_content(size = "large"), "div",
                          c("content", "is-large"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_heading(), "div", "heading")
})

