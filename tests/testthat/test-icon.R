test_that("bulma_icon is properly formatted", {
  single_icon <- bulma_icon("cog", size = "large")
  icon_with_text <- bulma_icon("cog", "Cog", color = "primary")
  multiple_icons <- bulma_icon(c("cog", "table", "list-alt", "bar-chart-o"),
                               c("Cog", "Table", "List", NA),
                               container = htmltools::div)

  expect_tag_classed_type(single_icon, "span",
                          c("icon", "is-large"), all_or_any = "all")
  expect_tag_children(single_icon, "i", "fa fa-cog")

  expect_tag_classed_type(icon_with_text, "span",
                          c("icon-text", "has-text-primary"))
  expect_tag_children(icon_with_text, list("span", "span"),
                      list("icon is-normal", NULL),
                      partial = FALSE)
  expect_tag_children(icon_with_text$children[[1L]],
                      "i", "fa fa-cog")

  expect_tag_classed_type(multiple_icons, "div", "icon-text")
  expect_tag_children(multiple_icons,
                      rep("span", 7),
                      rep_len(list("icon is-normal", NULL), 7))

  expect_error(bulma_icon(c("cog", "table")),
               "\"name\" must be length one, if no \"text\" is given")
  expect_error(bulma_icon(c("cog", "table"),
                          "Cog"),
               "number of icons must match the number of icon texts")

})
