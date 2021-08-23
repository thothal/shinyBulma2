test_that("is_integer recognizes integers", {
  expect_true(is_integer(1L))
  expect_true(is_integer(1))
  expect_true(is_integer(1.0))
  expect_true(is_integer(1:2))
  expect_true(is_integer(c(1, 2.0)))

  expect_false(is_integer(NULL))
  expect_false(is_integer(NA))
  expect_false(is_integer("small"))
  expect_false(is_integer(list(1, 2)))
})

test_that("add_class adds classes properly", {
  expect_null(add_class(NULL, NULL))
  expect_equal(add_class("a", NULL), "a")
  expect_equal(add_class(NULL, "a"), "a")
  expect_equal(add_class("a", "b"), "a b")
  expect_equal(add_class("a", "b c"), "a b c")
  expect_equal(add_class("a", c("b", "c")), "a b c")
})

test_that("add_class stops when mal-formatted arguments are passed", {
  err_msg <-"`old` must be either 'NULL' or a character of length one"
  expect_error(add_class(1:2, "a"), err_msg)
  expect_error(add_class(NA, "a"), err_msg)
  expect_error(add_class("a", c(1, NA)),
               "`new` must not contain NAs")
})

test_that("make_class makes proper bulma classes", {
  expect_null(make_class())
  expect_null(make_class(NULL))
  expect_equal(make_class(letters[1:3], prefix = c("is", "has", "are")),
               "is-a has-b are-c")
  expect_equal(make_class(letters[1:3]),
               "is-a is-b is-c")
  expect_equal(make_class(letters[1:3], prefix = "has"),
               "has-a has-b has-c")
  expect_equal(make_class("a-"), "is-a")
  expect_equal(make_class(letters[1:3], prefix = "are", collapse = FALSE),
               c("are-a", "are-b", "are-c"))
})

test_that("make_class stops when mal-formatted arguments are passed", {
  expect_error(make_class("a", prefix = "like"),
               "'arg' should be one of \"is\", \"has\", \"are\"")
})

test_that("`%||%` returns the proper value", {
  expect_null(NULL %||% NULL)
  expect_equal(1 %||% NULL, 1)
  expect_equal(NULL %||% 1, 1)
  expect_equal(2 %||% 1, 2)
  expect_equal(logical(0) %||% 1, 1)
  expect_equal(1:2 %||% 1, 1:2)
  expect_equal(1 %||% stop("should not evaluate"), 1)
})

test_that("parse_attributes returns all attributes", {
  skip_if_not_installed("xml2")
  tag <- htmltools::p(id = "a", disabled = NA, val = 2, `data-test` = "test",
                      `i-am-an-attribute` = "i-am-an-attribute")
  node <- xml2::xml_find_first(xml2::read_html(as.character(tag)), "//p")
  ## if attribute's value equals the attributes name we assume it is boolean value => NA
  expect_equal(parse_attributes(node),
               list(id = "a", disabled = NA, val = "2", `data-test` = "test",
                    `i-am-an-attribute` = NA))
})

test_that("html parsing from text and tags works", {
  skip_if_not_installed("xml2")
  html <- paste("<p id = 'start'>",
                "<span class = 'a'>",
                "<strong>Text</strong>",
                "<!-- comment -->",
                "<button disabled type = 'button'>Button</button>",
                "</span>",
                "</p>", sep = "\n")
  node <- xml2::xml_find_first(
    xml2::read_html(html),
    "/html/body/p")
  tags <- htmltools::tags
  goal <- tags$p(id = "start",
                       tags$span(class = "a",
                                       tags$strong("Text"),
                                       tags$button(disabled = NA, type = "button",
                                                   "Button")))
  expect_equal(parse_node(node),
               goal)
  expect_equal(html_to_tags(html),
               goal)
  expect_warning(html_to_tags(paste(html, "<invalid>Invalid</invalid>", sep = "\n")),
                 "unknown HTML tag <invalid>")
})

