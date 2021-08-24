test_that("bulma_level warns/stops when mal-formatted arguments are passed", {
  tmp <- htmltools::p()
  expect_warning(bulma_level(tmp, center = tmp),
                 "both 'center' and 'left'/'right' are not NULL - ignoring 'center'")
  expect_warning(bulma_level(right = tmp, center = tmp),
                 "both 'center' and 'left'/'right' are not NULL - ignoring 'center'")
  expect_warning(bulma_level(tmp, tmp, tmp),
                 "both 'center' and 'left'/'right' are not NULL - ignoring 'center'")
})

test_that("bulma_level is properly formatted", {
  ## centered level
  lvl <- bulma_level(center = htmltools::p(), horizontal_on_mobile = TRUE,
                     container = htmltools::div, id = "test")
  expect_tag_classed_type(lvl, "div", add_class("level", make_class("mobile")))
  expect_tag_children(lvl, "div", add_class("level-item",
                                            make_class("text-centered", prefix = "has")))
  expect_equal(lvl,
               bulma_level(center = bulma_level_item(content = htmltools::p()),
                           horizontal_on_mobile = TRUE,
                           container = htmltools::div,
                           id = "test"))
  expect_tag_attrib(lvl, "id", "test")

  ## left/right level
  lvl <- bulma_level(htmltools::p(), htmltools::p())
  expect_tag_children(lvl, rep("div", 2), paste0("level-", c("left", "right")))
  lapply(lvl$children,
         expect_tag_children,
         "div", add_class("level-item", make_class("text-centered", prefix = "has")))

  expect_equal(lvl,
               bulma_level(bulma_level_item(content = htmltools::p()),
                           bulma_level_item(content = htmltools::p())))
})

test_that("bulma_level_item warns/stops when mal-formatted arguments are passed", {
  tmp <- htmltools::p()
  warn_msg <- paste("both 'heading' / 'title' and 'content' are not NULL -",
                    "ignoring 'heading' / 'title'")
  expect_warning(bulma_level_item("Test", content = tmp),
                 warn_msg)
  expect_warning(bulma_level_item(title = "Test", content = tmp),
                 warn_msg)
  expect_warning(bulma_level_item("Test", "Test", tmp),
                 warn_msg)
  expect_warning(bulma_level_item(),
                 paste("'heading', 'title' and 'content' are NULL - will result",
                       "in an empty element"))
})

test_that("bulma_level_item is properly formatted", {
  # heading / title
  lvl_item <- bulma_level_item("title", "heading", container = htmltools::p,
                               id = "test")
  expect_tag_classed_type(lvl_item, "p",
                          add_class("level-item",
                                    make_class("text-centered", prefix = "has")))
  expect_tag_children(lvl_item, "div", list(NULL))
  expect_tag_children(lvl_item$children[[1]], rep("p", 2),
                      c("heading", "title"))
  expect_tag_attrib(lvl_item, "id", "test")

  lvl_item <- bulma_level_item("title", "heading", heading_on_top = FALSE)
  expect_tag_children(lvl_item$children[[1]], rep("p", 2),
                      c("title", "heading"))

  # content
  lvl_item <- bulma_level_item(content = htmltools::p(),
                              centered = FALSE)
  expect_tag_class(lvl_item, "level-item", exact = TRUE)
})
