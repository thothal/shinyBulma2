test_that("no themes are present on a fresh install", {
  skip_if(identical(Sys.getenv("NOT_CRAN"), "true"), "Not a fresh install")
  expect_length(get_bulma_themes(), 0)
})
