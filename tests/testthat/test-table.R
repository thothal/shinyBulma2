test_that("bulma_table warns/stops when mal-formatted arguments are passed", {
  bulma_env <- environment(bulma_table)
  temp_env <- new.env(parent = bulma_env)
  temp_env$requireNamespace <- function(...) FALSE
  ## this will create a copy of bulma_table
  ## needed to locally overwrite requireNamespace
  environment(bulma_table) <- temp_env
  expect_warning(bulma_table(mtcars, convert_to_tags = TRUE),
                 "`convert_to_tags = TRUE` requires package `xml2` to be installed")
  expect_warning(bulma_table(mtcars, escape = FALSE, sanitize.text.function = identity),
                 paste("setting `escape = FALSE` will overwrite argument",
                       "`sanitize.text.function`"))
  tmp <- data.frame(x = 1:2)
  out_of_bounds_msg <- "`selected_row` must be between . and `nrow\\(data\\)`"
  expect_warning(bulma_table(tmp, selected_row = -1),
                 out_of_bounds_msg)
  expect_warning(bulma_table(tmp, col_names = FALSE, selected_row = 0),
                 out_of_bounds_msg)
  expect_warning(bulma_table(tmp, selected_row = 3),
                 out_of_bounds_msg)
  expect_warning(bulma_table(tmp, col_names = FALSE, selected_row = 3),
                 out_of_bounds_msg)

  align_error_msg <- paste("`align` must contain only the characters `l`, `c`, `r`",
                           "and/or `\\?` and have length either equal to 1 or to the",
                           "total number of columns")
  expect_error(bulma_table(mtcars, align = "cc"),
               align_error_msg)
  expect_error(bulma_table(mtcars, align = "x"),
               align_error_msg)

  expect_error(bulma_table(list(1:2, 3:5)),
               "arguments imply differing number of rows: .*")
})

test_that("bulma_table is properly formatted", {
  ## start with this rather advanced test, as it may be the only one we can run w/o xml2
  tmp <- matrix(NA, 1, 1)
  rownames(tmp) <- "line"
  tab <- bulma_table(tmp, convert_to_tags = FALSE)
  expect_match(tab, perl = TRUE,
               paste0("(?s)<table.*<thead.*<tr.*<th.*</th.*</tr.*</thead.*",
                      "<tbody.*<tr.*<td.*</td.*</tr.*</tbody.*</table"))

  ## from here we need xml2 available to proceed
  skip_if_not_installed("xml2")
  tab <- bulma_table(tmp, row_names = TRUE, na = "Nothing", selected_row = 1L,
                     convert_to_tags = TRUE)
  expect_tag_children(tab$children[[2]], "tr", make_class("selected"))
  expect_tag_children(tab$children[[2]]$children[[1]], rep("td", 2),
                      list(NULL, paste(make_class("text-grey", prefix = "has"),
                                       make_class("italic"))))



  colnames(tmp) <- "<strong>V1</strong>"
  tab <- bulma_table(tmp, escape = FALSE,
                     convert_to_tags = TRUE)
  expect_tag_children(tab$children[[1]]$children[[1]]$children[[1]],
                      "strong")
  expect_tag_classed_type(bulma_table(NULL), "table", "table")
  tmp <- data.frame(a = 1L, b = "text", c = 2.3)
  tab <- bulma_table(tmp, TRUE, TRUE, TRUE, TRUE, TRUE,
                     convert_to_tags = TRUE)
  expect_tag_classed_type(tab, "table",
                          list(make_class(c("bordered", "striped", "hoverable", "narrow",
                                            "fullwidth"))))
  expect_tag_children(tab,
                      c("thead", "tbody"))
  expect_tag_children(tab$children[[1]], "tr")
  expect_tag_children(tab$children[[1]]$children[[1]],
                      rep("th", 3),
                      make_class(c("text-right", "text-left", "text-right"),
                                 prefix = "has", collapse = FALSE))
  expect_tag_children(tab$children[[2]], "tr")
  expect_tag_children(tab$children[[2]]$children[[1]],
                      rep("td", 3),
                      list(make_class("text-right", prefix = "has"),
                           NULL,
                           make_class("text-right", prefix = "has")))

  tab <- bulma_table(tmp, align = "cr?", scrollable = TRUE,
                     convert_to_tags = TRUE)
  expect_tag_classed_type(tab, "div" ,"table-container")
  expect_tag_children(tab$children[[1]]$children[[2]]$children[[1]],
                      rep("td", 3),
                      make_class(c("text-centered", "text-right", "text-right"),
                                           prefix = "has", collapse = FALSE)
  )

  tmp <- list(1, 2, "text")
  tab <- bulma_table(tmp, include.colnames = FALSE, include.rownames = FALSE,
                     html.table.attributes = "style = 'background-color:steelblue'",
                     align = "c", convert_to_tags = TRUE)
  expect_tag_children_length(tab, 2L)
})

