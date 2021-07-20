library(htmltools)
test_that("bulma elements are properly formatted", {
  expect_equal(as.character(bulma_block()), "<div class=\"block\"></div>")
  expect_equal(as.character(bulma_box()), "<div class=\"box\"></div>")
  expect_equal(as.character(bulma_content()),
               "<div class=\"content is-normal\"></div>")
  expect_equal(as.character(bulma_content(size = "large")),
               "<div class=\"content is-large\"></div>")
})

test_that("bulma_icon is properly formatted", {
  single_icon <- bulma_icon("cog")
  icon_with_text <- bulma_icon("cog", "Cog")

  expect_tag(single_icon, "span")
  expect_tag_class(single_icon, c("icon", "is-normal"))
  expect_equal(as.character(tagSetChildren(single_icon, list = list())),
              "<span class=\"icon is-normal\"></span>")
  expect_match(as.character(single_icon$children[[1]]),
               "<i class=\"fa fa-cog\" .*></i>")
})
