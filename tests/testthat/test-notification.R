test_that("bulma_notification is properly formatted", {
  notif <- bulma_notification("Text", color = "primary", light = TRUE, id = "myid")
  notif2 <- bulma_notification(bulma_title("Test", add_delete = FALSE))
  expect_tag_classed_type(notif,
                          "div",
                          c("notification", make_class(c("primary", "light"))))
  expect_tag_children(notif,
                      c("button", "text_node"),
                      list("delete", NULL))
  expect_tag_attrib(notif, "id", "myid")
  expect_tag_children(notif2,
                      "h1",
                      "title")
})
