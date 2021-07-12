test_that("bulma_columns are properly formatted", {
  expect_equal(as.character(bulma_columns()), "<div class=\"columns\"></div>")
  expect_equal(as.character(bulma_columns(center = "both")),
               "<div class=\"columns is-centered is-vcentered\"></div>")
  expect_equal(as.character(bulma_columns(multiline = TRUE)),
               "<div class=\"columns is-multiline\"></div>")
  expect_equal(as.character(bulma_columns(gap_width = 1)),
               "<div class=\"columns is-1 is-variable\"></div>")
  expect_equal(as.character(bulma_columns(gap_width = "gapless")),
               "<div class=\"columns is-gapless\"></div>")
  expect_equal(as.character(bulma_columns(enable_from = "mobile")),
               "<div class=\"columns is-mobile\"></div>")
  expect_equal(as.character(bulma_columns(enable_from = "desktop")),
               "<div class=\"columns is-desktop\"></div>")
})

test_that("bulma_columns raises a warning when an invalid media breakpoint is used", {
  expect_warning(bulma_columns(enable_from = "fullhd"),
                 "\".*\" is not supported by bulma columns")
  expect_warning(bulma_columns(enable_from = "touch"),
                 "\".*\" is not supported by bulma columns")
})

test_that("bulma_column is properly formatted", {
  expect_equal(as.character(bulma_column()), "<div class=\"column\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3")),
               "<div class=\"column is-two-thirds\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3", offset = 3)),
               "<div class=\"column is-two-thirds is-offset-3\"></div>")
  expect_equal(as.character(bulma_column(width = "full-fullhd", offset = 1)),
               "<div class=\"column is-full-fullhd is-offset-1\"></div>")
})
