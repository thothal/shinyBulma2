test_that("valid column sizes are properly translated", {
  expect_null(validate_bulma_size(NULL), NULL)
  expect_equal(validate_bulma_size(1), "is-1")
  expect_equal(validate_bulma_size(6), "is-6")
  expect_equal(validate_bulma_size(12), "is-12")
  expect_equal(validate_bulma_size("4"), "is-4")
  expect_equal(validate_bulma_size("one-third"), "is-one-third")
  expect_equal(validate_bulma_size("three-quarters"), "is-three-quarters")
  expect_equal(validate_bulma_size("three-fifth"), "is-three-fifth")
  expect_equal(validate_bulma_size("4/5"), "is-four-fifth")
  expect_equal(validate_bulma_size("1/1"), "is-full")
  expect_equal(validate_bulma_size("1/2"), "is-half")
  expect_equal(validate_bulma_size("narrow"), "is-narrow")
  expect_equal(validate_bulma_size("narrow-widescreen"), "is-narrow-widescreen")
  expect_equal(validate_bulma_size("1-widescreen"), "is-1-widescreen")
  expect_equal(validate_bulma_size("2/3-desktop"), "is-two-thirds-desktop")
  expect_equal(validate_bulma_size("one-fifth-touch"), "is-one-fifth-touch")
  expect_equal(validate_bulma_size(c("1/4-touch", "1/5-fullhd", "1/3-widescreen")),
               "is-one-quarter-touch is-one-fifth-fullhd is-one-third-widescreen")
})

test_that("valid column offsets are properly translated", {
  expect_null(validate_bulma_offset(NULL))
  expect_equal(validate_bulma_offset(1), "is-offset-1")
  expect_equal(validate_bulma_offset(6), "is-offset-6")
  expect_equal(validate_bulma_offset(12), "is-offset-12")
  expect_equal(validate_bulma_offset("4"), "is-offset-4")
  expect_equal(validate_bulma_offset("one-third"), "is-offset-one-third")
  expect_equal(validate_bulma_offset("three-quarters"), "is-offset-three-quarters")
  expect_equal(validate_bulma_offset("three-fifth"), "is-offset-three-fifth")
  expect_equal(validate_bulma_offset("4/5"), "is-offset-four-fifth")
  expect_equal(validate_bulma_offset("1/2"), "is-offset-half")
  expect_equal(validate_bulma_offset("4-tablet"), "is-offset-4-tablet")
  expect_equal(validate_bulma_offset("two-thirds-mobile"),
               "is-offset-two-thirds-mobile")
  expect_equal(validate_bulma_offset(c("2/3-tablet", "4/5-mobile", "1/2-desktop")),
               paste("is-offset-two-thirds-tablet is-offset-four-fifth-mobile",
                     "is-offset-half-desktop"))
})

test_that("valid column gaps are properly translated", {
  expect_null(validate_bulma_gap(NULL))
  expect_equal(validate_bulma_gap(0), "is-0 is-variable")
  expect_equal(validate_bulma_gap(4), "is-4 is-variable")
  expect_equal(validate_bulma_gap(8), "is-8 is-variable")
  expect_equal(validate_bulma_gap("4"), "is-4 is-variable")
  expect_equal(validate_bulma_gap("gapless"), "is-gapless")
  expect_equal(validate_bulma_gap(c("1-mobile", "2-tablet", "3-touch", "4-desktop",
                                    "5-widescreen", "6-fullhd")),
               paste("is-1-mobile is-2-tablet is-3-touch is-4-desktop",
                     "is-5-widescreen is-6-fullhd is-variable"))
})

test_that("improper sizes are raising an error", {
  expect_error(validate_bulma_size(13),
               "column size must be an integer between 1 and 12")
  expect_error(validate_bulma_size(11.3),
               "column size must be an integer between 1 and 12")
  expect_error(validate_bulma_size(NA),
               "column size must not contain any 'NAs'")
  expect_error(validate_bulma_size("one-sixth"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_size("one.third"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_size("1/1-fulhd"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_size(list("1/3")),
               "column size must be a numeric or character vector")
  expect_error(validate_bulma_size(c("1/2-touch", "1", "two-thirds-mobile")),
               paste("column size must contain breakpoints either for all elements",
                     "or for none at all"))
})

test_that("improper offsets are raising an error", {
  expect_error(validate_bulma_offset(13),
               "column offset must be an integer between 1 and 12")
  expect_error(validate_bulma_offset(11.3),
               "column offset must be an integer between 1 and 12")
  expect_error(validate_bulma_offset(NA),
               "column offset must not contain any 'NAs'")
  expect_error(validate_bulma_offset("1/1"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_offset("full"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_offset("one-sixth"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_offset("one.third"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_offset("3/4-touchscreen"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_offset(list("two-thirds")),
               "column offset must be a numeric or character vector")
  expect_error(validate_bulma_offset(c("4/5-touch", "11", "half-mobile")),
               paste("column offset must contain breakpoints either for all elements",
                     "or for none at all"))
})

test_that("improper gaps are raising an error", {
  expect_error(validate_bulma_gap(9),
               "column gap must be an integer between 0 and 8")
  expect_error(validate_bulma_gap(8.3),
               "column gap must be an integer between 0 and 8")
  expect_error(validate_bulma_gap(NA),
               "column gap must not contain any 'NAs'")
  expect_error(validate_bulma_gap("1/2"),
               "\".*\" is not a valid gap specifier")
  expect_error(validate_bulma_gap("full"),
               "\".*\" is not a valid gap specifier")
  expect_error(validate_bulma_gap("one-sixth"),
               "\".*\" is not a valid gap specifier")
  expect_error(validate_bulma_gap("gap-less"),
               "\".*\" is not a valid gap specifier")
  expect_error(validate_bulma_gap("1-touchscreen"),
               "\".*\" is not a valid gap specifier")
  expect_error(validate_bulma_gap(list("2")),
               "column gap must be a numeric or character vector")
  expect_error(validate_bulma_gap(c("2-touch", "gapless", "1-mobile")),
               paste("column gap must contain breakpoints either for all elements",
                     "or for none at all"))
})

test_that("duplicated media breakpoints raise warning", {
  expect_warning(validate_bulma_size(c("1-touch", "2-touch")),
                 "duplicated media breakpoints found")
  expect_warning(validate_bulma_offset(c("1/5-touch", "4/5-touch")),
                 "duplicated media breakpoints found")
  expect_warning(validate_bulma_gap(c("0-touch", "1-touch")),
                 "duplicated media breakpoints found")
})

test_that("multiple sizes without media breakpoint raise warning", {
  expect_warning(validate_bulma_size(1:3),
                 "multiple sizes without media breakpoints found")
  expect_warning(validate_bulma_size(1:3),
                 "multiple sizes without media breakpoints found")
  expect_warning(validate_bulma_gap(1:3),
                 "multiple sizes without media breakpoints found")

})

test_that("validate_bulma_unit gives same results as validate_bulma_{size|offset|gap}", {
  expect_equal(validate_bulma_unit(1, "size"),
               validate_bulma_size(1))
  expect_equal(validate_bulma_unit(2, "offset"),
               validate_bulma_offset(2))
  expect_equal(validate_bulma_unit(3, "gap"),
               validate_bulma_gap(3))
})

test_that("bulma_columns are properly formatted", {
  expect_equal(as.character(bulma_columns()), "<div class=\"columns\"></div>")
  expect_equal(as.character(bulma_columns(center = "both")),
               "<div class=\"columns is-centered is-vcentered\"></div>")
  expect_equal(as.character(bulma_columns(multiline = TRUE)),
               "<div class=\"columns is-multiline\"></div>")
  expect_equal(as.character(bulma_columns(gap_width = 1)),
               "<div class=\"columns is-1 is-variable\"></div>")
  expect_equal(as.character(bulma_columns(gap_width = "gapless")),
               "<div class=\"columns is-gapless\"></div>")
  expect_equal(as.character(bulma_columns(media_breakpoint = "mobile")),
               "<div class=\"columns is-mobile\"></div>")
  expect_equal(as.character(bulma_columns(media_breakpoint = "desktop")),
               "<div class=\"columns is-desktop\"></div>")
})

test_that("bulma_columns raises a warning when an invalid media breakpoint is used", {
  expect_warning(bulma_columns(media_breakpoint = "fullhd"),
                 "\".*\" is not supported by bulma columns")
  expect_warning(bulma_columns(media_breakpoint = "touch"),
                 "\".*\" is not supported by bulma columns")
})

test_that("bulma_column is properly formatted", {
  expect_equal(as.character(bulma_column()), "<div class=\"column\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3")),
               "<div class=\"column is-two-thirds\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3", offset = 3)),
               "<div class=\"column is-two-thirds is-offset-3\"></div>")
  expect_equal(as.character(bulma_column(width = "full-fullhd", offset = 1)),
               "<div class=\"column is-full-fullhd is-offset-1\"></div>")
})
