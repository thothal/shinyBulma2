test_that("bulma_container warns/stops when mal-formatted arguments are passed", {
  warn_msg <- paste("'fluid' is TRUE - non NULL 'fullwidth_breakpoint' or",
                    "'narrow_down_to' will be ignored")
  expect_warning(bulma_container(fluid = TRUE, fullwidth_breakpoint = "fullhd"),
                 warn_msg)
  expect_warning(bulma_container(fluid = TRUE, narrow_down_to = "widescreen"),
                 warn_msg)
  expect_warning(bulma_container(fluid = TRUE, fullwidth_breakpoint = "fullhs",
                                 narrow_down_to = "widescreen"),
                 warn_msg)

  expect_warning(bulma_container(fullwidth_breakpoint = "fullhd",
                                 narrow_down_to = "widescreen"),
                 paste("both 'fullwidth_breakoint' and 'narrow_down_to' are not NULL -",
                       "only the former will be considered"))

  expect_warning(bulma_container(fullwidth_breakpoint = "desktop"),
                 paste("\"desktop\" is not supported by bulma container as",
                       "a fullwidth breakpoint"))
  expect_warning(bulma_container(narrow_down_to = "fullhd"),
                 paste("\"fullhd\" is not supported by bulma container as",
                       "a narrow-down-to breakpoint"))
})

test_that("bulma_container is properly formatted", {
  con <- bulma_container(id = "test", fullwidth_breakpoint = "widescreen")
  expect_tag_classed_type(con, "div", c("container", make_class("widescreen")))
  expect_tag_attrib(con, "id", "test")
  con <- bulma_container(narrow_down_to = "widescreen")
  expect_tag_classed_type(con, "div", c("container", make_class("max-widescreen")))
})
