test_that("bulma elements are properly formatted", {
  expect_tag_classed_type(bulma_block(), "div", "block")
  expect_tag_classed_type(bulma_box(), "div", "box")
  expect_tag_classed_type(bulma_content(), "div",
                          c("content", "is-normal"),
                          all_or_any = "all")
  expect_tag_classed_type(bulma_content(size = "large"), "div",
                          c("content", "is-large"),
                          all_or_any = "all")
})

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

test_that("bulma_image is properly formatted", {
  args <- data.frame(
    class = c("square", "1by1", "5by4", "4by3",
              "3by2", "5by3", "16by9", "2by1", "3by1",
              "4by5", "3by4", "2by3", "3by5", "9by16",
              "1by2", "1by3", 16, 24, 32, 48, 64, 96, 128),
    ref = c("http://placehold.it/480x480", "http://placehold.it/480x480",
            "http://placehold.it/600x480", "http://placehold.it/640x480",
            "http://placehold.it/480x320", "http://placehold.it/800x480",
            "http://placehold.it/640x360", "http://placehold.it/640x320",
            "http://placehold.it/720x240", "http://placehold.it/480x600",
            "http://placehold.it/480x640", "http://placehold.it/320x480",
            "http://placehold.it/480x800", "http://placehold.it/360x640",
            "http://placehold.it/320x640", "http://placehold.it/240x720",
            "http://placehold.it/6x6", "http://placehold.it/24x24",
            "http://placehold.it/32x32", "http://placehold.it/48x48",
            "http://placehold.it/64x64", "http://placehold.it/96x96",
            "http://placehold.it/128x128"),
    is_fixed = rep(c(FALSE, TRUE), c(16, 7)))
  Map(function(cls, src, is_fixed) {
    if (is_fixed) {
      el <- bulma_image(src, fixed = cls, rounded = TRUE)
      expect_tag_classed_type(el, "figure", c("image", make_class(paste0(cls, "x", cls))))
    } else {
      el <- bulma_image(src, ratio = cls, use_fullwidth = TRUE)
      expect_tag_classed_type(el, "figure", c("image", make_class(c(cls, "fullwidth"))))
    }
    expect_tag_children(el, "img")
    expect_tag_attrib(el$children[[1L]], "src", src)
    if (is_fixed) {
      expect_tag_children(el, "img", make_class("rounded"))
    }
  }, args$class, args$ref, args$is_fixed)
})
