test_that("bulma_notification is properly formatted", {
  notif <- bulma_notification("Text", color = "primary", light = TRUE, id = "myid")
  notif2 <- bulma_notification(htmltools::h1("Test", class = "title"))
  expect_tag_classed_type(notif,
                          "div",
                          c("notification", make_class(c("primary", "light"))),
                          all_or_any = "all")
  expect_tag_children(notif,
                      c("button", "text_node"),
                      list("delete", NULL))
  expect_tag_attrib(notif, "id", "myid")
  expect_tag_children(notif2,
                      c("button", "h1"),
                      list("delete", "title"))
})
