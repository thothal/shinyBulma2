test_that("bulma_button is properly formatted", {
  btn <- bulma_button("Label", "cog", "primary", TRUE,
                      "medium", "small", icon_pos = "end", full_width = TRUE,
                      disabled = TRUE, style = c("outlined", "inverted", "rounded"),
                      state = "hovered", id = "myid")
  btn2 <- bulma_button("Label", "cog", container = htmltools::a)
  expect_tag_classed_type(btn,
                          "button",
                          c("button", make_class(c("primary", "light", "medium",
                            "outlined", "inverted", "rounded", "hovered",
                            "fullwidth"))),
                          all_or_any = "all")
  expect_tag_attrib(btn, c("disabled", "id"), c(NA, "myid"))
  expect_tag_children(btn, c("span", "span"), list(NULL, c("icon", make_class("small"))),
                      strict = TRUE)
  expect_tag_classed_type(btn2,
                          "a", "button")
  expect_tag_children(btn2,
                      c("span", "span"),
                      list("icon", NULL))
  expect_error(bulma_button("Label", "cog", icon_pos = "middle"),
               "'arg' should be one of \"start\", \"end\"")
  expect_warning(bulma_button("Label", color = "ghost", light = TRUE),
                 "'light' is not defined for color \"ghost\"")
})

test_that("bulma_buttons is properly formatted", {
  btns <- bulma_buttons(alignment = "right", addons = TRUE, size = "medium",
                        container = htmltools::p)
  expect_tag_classed_type(btns,
                          "p",
                          c("buttons",
                            make_class("right"),
                            make_class("addons", prefix = "has"),
                            make_class("medium", prefix = "are")),
                          all_or_any = "all")
  expect_error(bulma_buttons(alignment = "left"),
               "'arg' should be one of \"centered\", \"right\"")
})

test_that("bulma_delete is properl formatted", {
  btn <- bulma_delete("small")
  expect_tag_classed_type(btn, "button", c("delete", make_class("small")))
  expect_error(bulma_delete("middle"),
               "'arg' should be one of .*")
})
