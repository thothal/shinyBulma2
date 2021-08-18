test_that("bulma_progressbar is properly formatted", {
  pgb <- bulma_progressbar(20, color = "primary", size = "large")
  expect_tag_classed_type(pgb, "progress", c("progress",
                                             make_class(c("primary", "large"))))
  expect_tag_attrib(pgb, c("max", "value"), c(100, 20))
  expect_tag_children(pgb, "text_node")
  pgb <- bulma_progressbar(NULL)
  expect_tag_attrib(pgb, "max", 100)
})
