test_that("slot expectation works as intended", {
  vec <- c(a = 1, b = 2)
  vec0 <- 1:2
  lst <- list(a = 1, b = 2)
  lst0 <- list(1, 2)
  expect_success(expect_has_slots(vec, "a"))
  expect_success(expect_has_slots(vec, c("a", "b")))
  expect_failure(expect_has_slots(vec, "c"), "`vec` does not contain slot 'c'")
  expect_failure(expect_has_slots(vec, c("c", "d")),
                 "`vec` does not contain slots 'c', 'd'")
  expect_success(expect_has_slots(lst, "a"))
  expect_success(expect_has_slots(lst, c("a", "b")))
  expect_failure(expect_has_slots(lst, "c"), "`lst` does not contain slot 'c'")
  expect_failure(expect_has_slots(lst, c("c", "d")),
                 "`lst` does not contain slots 'c', 'd'")
  expect_failure(expect_has_slots(vec0, "a"), "`vec0` does not have names")
  expect_failure(expect_has_slots(lst0, "a"), "`lst0` does not have names")
})

test_that("tag class expectation works as intended", {
  e0 <- htmltools::p()
  e1 <- htmltools::p(class = "a")
  e2 <- htmltools::p(class = "a b")
  e3 <- htmltools::p(class = "a", class = "b")
  e4 <- list(class = "a")
  expect_failure(expect_tag_class(e0, "a"), "`e0` does not have a class attribute")
  expect_success(expect_tag_class(e1, "a"))
  expect_success(expect_tag_class(e2, "a"))
  expect_success(expect_tag_class(e3, "a"))
  expect_failure(expect_tag_class(e4, "a"), "`e4` is not a tag object")
  expect_failure(expect_tag_class(e2, "c"),
                 "'c' is not a part of `e2`'s classes \\['a b'\\]")
  expect_failure(expect_tag_class(e3, "c"),
                 "'c' is not a part of `e3`'s classes \\['a b'\\]")
  expect_success(expect_tag_class(e2, "a b"))
  expect_success(expect_tag_class(e2, c("a", "b")))
  expect_success(expect_tag_class(e2, "a c", "any"))
  expect_success(expect_tag_class(e2, c("a", "c"), "any"))
  expect_failure(expect_tag_class(e2, "c d"),
                 "'c', 'd' are not part of `e2`'s classes \\['a b'\\]")
  expect_failure(expect_tag_class(e2, c("c", "d")),
                 "'c', 'd' are not part of `e2`'s classes \\['a b'\\]")

})

test_that("tag type expectation works as intended", {
  e <- htmltools::p()
  f <- htmltools::tagList(e, e)
  expect_success(expect_tag_type(e, "p"))
  expect_failure(expect_tag_type(e, "div"), "`e` is not a `<div>` tag but a `<p>` tag")
  expect_success(expect_tag_type(f, "shiny.tag.list"))
  expect_failure(expect_tag_type(f, "p"), "`f` is not a tag object")
  expect_failure(expect_tag_type(e, "shiny.tag.list"), "`e` is not a shiny tag list")
})

test_that("tag classed type expectation works as intended", {
  ## include test for the sake of completeness as it is a mere wrapper for other tests
  expect_success(expect_tag_classed_type(htmltools::p(class = "test"),
                                         "p", "test"))
})

test_that("tag attrib expectation works as intended", {
  e <- htmltools::p(disable = NA, id = "test", tabindex = 2)
  expect_success(expect_tag_attrib(e, c("disable", "id"), c(NA, "test")))
  expect_success(expect_tag_attrib(e, "tabindex", 2))
  expect_success(expect_tag_attrib(e, "id", "t..t", regex = TRUE))

  expect_failure(expect_tag_attrib(e, "style", "width = 120px"),
                 "'style' is not a valid attribute of `e`")
  expect_failure(expect_tag_attrib(htmltools::tagList(htmltools::p(id = "test")),
                                   "id", "test"),
                 ".* is not a tag object")
  expect_failure(expect_tag_attrib(e, "id", "test2"),
                 "`e` does not contain attribute \\[id = \"test2\"\\]")
  expect_failure(expect_tag_attrib(e, "id", "t..t2", regex = TRUE),
                 "`e` does not contain attribute \\[id ~= /t\\.\\.t2/\\]")
  expect_failure(expect_tag_attrib(e, c("tabindex", "id"), list(3, "test2")),
                 "`e` does not contain attributes \\[tabindex = \"3\", id = \"test2\"\\]")
})

test_that("number of children expectation works as intended", {
  e <- htmltools::p()
  f <- htmltools::p(htmltools::span())
  expect_success(expect_tag_children_length(e, 0))
  expect_success(expect_tag_children_length(f, 1))
  expect_failure(expect_tag_children_length(f, 2),
                 "`f` has 1 children, not 2 children")
  expect_failure(expect_tag_children_length(list(), 0),
                 ".* is not a tag object")
})

test_that("type and class of children expectation works as intended", {
  grp <- htmltools::p(
    htmltools::h1(class = "c1"),
    htmltools::h2(class = "c2"),
    htmltools::h3(class = "c3a", class = "c3b"),
    htmltools::h4(class = "c4a c4b"),
    htmltools::h5(class = "c2"),
    htmltools::h1(class = "c1"),
    htmltools::h1(class = "c2"),
    htmltools::h2()
  )

  nested <- htmltools::p(
    htmltools::tagList(htmltools::p(class = "c1"),
                       htmltools::p(class = "c2"))
  )

  text_node <- htmltools::p(
    htmltools::h1(class = "header"),
    "Sample Text",
    1L,
    1.4
  )

  empty <- htmltools::p(character(0), list())

  expect_success(expect_tag_children(grp, list("h1", "h1")))
  expect_success(expect_tag_children(grp, class = list("c1", "c1")))
  expect_success(expect_tag_children(grp, list("h1", NULL, "h4", "h2"),
                                     list("c1", "c3b", NULL, NULL)))
  expect_success(expect_tag_children(grp, class = list(c("c3a", "c3b"), c("c4a", "c4b"))))
  expect_success(expect_tag_children(grp, class = list("c3a c3b", "c4a c4b")))
  expect_success(expect_tag_children(grp, list("h1", "h2", "h3", "h4", "h5",
                                               "h1", "h1", "h2"), partial = FALSE))
  expect_success(expect_tag_children(grp,
                                     class = list("c1", "c2", "c3a", "c4a", "c2",
                                                  "c1", "c2", NULL), partial = FALSE))
  expect_success(expect_tag_children(grp, list("h3", "h4"), strict = TRUE))
  expect_success(expect_tag_children(grp, class = list("c3a", "c4a"), strict = TRUE))

  expect_success(expect_tag_children(nested, class = list("c1", "c2")))

  expect_success(expect_tag_children(text_node,
                                     list("h1", "text_node", "text_node", "text_node"),
                                     partial = FALSE))

  expect_success(expect_tag_children(text_node,
                                     list("h1", NULL, NULL, NULL),
                                     partial = FALSE))

  expect_success(expect_tag_children(empty,
                                     list(NULL)))

  expect_failure(expect_tag_children(list(), "h1"),
                 ".* is not a tag object")

  expect_failure(expect_tag_children(grp, list("h1", "h1", "h1", "h1")),
                 "element <h1> could not be matched at position #4")
  expect_failure(expect_tag_children(grp, class = list("c1", "c1", "c1")),
                 "element <\\* class = \"c1\"> could not be matched at position #3")
  expect_failure(expect_tag_children(grp, list("h1", "h3", "h4"), list("c1", "c3", "c4")),
                 "elements .* could not be matched at positions #2, #3")
  expect_failure(expect_tag_children(grp, class = list(c("c3a", "c3"))),
                 "element <\\* class = \"c3a c3\"> could not be matched at position #1")
  expect_failure(expect_tag_children(grp, class = list("c3a c3")),
                 "element <\\* class = \"c3a c3\"> could not be matched at position #1")
  expect_failure(expect_tag_children(grp, list("h1", "h2"), partial = FALSE),
                 "6 elements too few specified in non partial mode \\(8 needed\\)")
  expect_failure(expect_tag_children(grp, list("h1", "h3", "h5"), strict = TRUE),
                 "elements <h3>, <h5> could not be matched at positions #2, #3")
  expect_failure(expect_tag_children(grp, class = list("c1", "c3b", "c2"), strict = TRUE),
                 "elements .* could not be matched at positions #2, #3")
  expect_failure(expect_tag_children(text_node, list("h1", NULL, "h1")),
                 "element <h1> could not be matched at position #3")
})

