test_that("bulma_hero warns/stops when mal-formatted arguments are passed", {
  expect_warning(bulma_hero(),
                 paste("'body', 'title' and 'subtitle' are NULL -",
                 "will result in an empty element"))
  expect_warning(bulma_hero("Title", "Subtitle", htmltools::p()),
                 paste("both 'title' / 'subtitle' and 'body' are not NULL -",
                 "ignoring 'title' / 'subtitle'"))
  expect_warning(bulma_hero(body = htmltools::p("Test"), with_navbar = TRUE),
                 "'with_navbar' makes only sense with fullheight sized hero banners")
  expect_warning(bulma_hero(body = htmltools::p("Test"), size = "medium",
                            with_navbar = TRUE),
                 "'with_navbar' makes only sense with fullheight sized hero banners")
})

test_that("bulma_hero is properly formatted", {
  hero <- bulma_hero("Title", "Subtitle",
                     header = htmltools::p(), footer = htmltools::p(),
                     color = "primary", size = "fullheight",
                     with_navbar = TRUE, container = htmltools::div,
                     id = "test")
  expect_tag_classed_type(hero, "div",
                          c("hero", make_class(c("primary", "fullheight-with-navbar"))))
  expect_tag_children(hero, rep("div", 3), c("hero-head", "hero-body", "hero-foot"))
  expect_tag_children(hero$children[[2]], "div", NULL)
  expect_tag_children(hero$children[[2]]$children[[1]], rep("p", 2),
                      c("title", "subtitle"))
  expect_tag_attrib(hero, "id", "test")
  hero <- bulma_hero("Title Only")
  expect_tag_classed_type(hero, "section", "hero")
  expect_tag_children(hero, "div", "hero-body")
  expect_tag_children(hero$children[[1]],
                      "p", "title")
  hero <- bulma_hero(subtitle = "Subtitle Only")
  expect_tag_classed_type(hero, "section", "hero")
  expect_tag_children(hero, "div", "hero-body")
  expect_tag_children(hero$children[[1]],
                      "p", "subtitle")
  hero <- bulma_hero(body = bulma_title("Title", size = 4), size = "large")
  expect_tag_classed_type(hero, "section", c("hero", make_class("large")))
  expect_tag_children(hero, "div", "hero-body")
  expect_tag_children(hero$children[[1]], "h1", list(c("title", make_class(4))))
})
