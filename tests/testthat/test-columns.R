test_that("bulma_columns are properly formatted", {
  expect_tag_classed_type(bulma_columns(), "div", "columns")
  expect_tag_classed_type(bulma_columns(center = "both"), "div",
                          c("columns", "is-centered", "is-vcentered"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_columns(multiline = TRUE), "div",
                          c("columns", "is-multiline"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_columns(gap_width = 1), "div",
                          c("columns", "is-1 is-variable"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_columns(gap_width = "gapless"), "div",
                          c("columns", "is-gapless"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_columns(enable_from = "mobile"), "div",
                          c("columns", "is-mobile"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_columns(enable_from = "desktop"), "div",
                          c("columns", "is-desktop"),
                          all_or_any = "all")
})

test_that("bulma_columns raises a warning when an invalid media breakpoint is used", {
  expect_warning(bulma_columns(enable_from = "fullhd"),
                 "\".*\" is not supported by bulma columns")
  expect_warning(bulma_columns(enable_from = "touch"),
                 "\".*\" is not supported by bulma columns")
})

test_that("bulma_column is properly formatted", {
  expect_tag_classed_type(bulma_column(), "div", "column")
  expect_tag_classed_type(bulma_column(width = "2/3"), "div",
                          c("column", "is-two-thirds"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_column(width = "2/3", offset = 3L), "div",
                          c("column", "is-two-thirds", "is-offset-3"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_column(width = "full-fullhd", offset = 1L), "div",
                          c("column", "is-full-fullhd", "is-offset-1"),
                          all_or_any = "all")
})
