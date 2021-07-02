test_that("column sizes are properly validated", {
  expect_null(validate_bulma_column_size(NULL))
  expect_equal(validate_bulma_column_size(1), "is-1")
  expect_equal(validate_bulma_column_size(6), "is-6")
  expect_equal(validate_bulma_column_size(12), "is-12")
  expect_equal(validate_bulma_column_size("one-third"), "is-one-third")
  expect_equal(validate_bulma_column_size("three-quarters"), "is-three-quarters")
  expect_equal(validate_bulma_column_size("three-fifth"), "is-three-fifth")
  expect_equal(validate_bulma_column_size("4/5"), "is-four-fifth")
  expect_equal(validate_bulma_column_size("1/1"), "is-full")
  expect_equal(validate_bulma_column_size("1/2"), "is-half")
  expect_equal(validate_bulma_column_size("narrow"), "is-narrow")
  expect_equal(validate_bulma_column_size("narrow", "widescreen"), "is-narrow-widescreen")
  expect_equal(validate_bulma_column_size(1, "widescreen"), "is-1-widescreen")
  expect_equal(validate_bulma_column_size("2/3", "desktop"), "is-two-thirds-desktop")
  expect_equal(validate_bulma_column_size("one-fifth", "touch"), "is-one-fifth-touch")
})

test_that("column offsets are properly validated", {
  expect_null(validate_bulma_column_offset(NULL))
  expect_equal(validate_bulma_column_offset(1), "is-offset-1")
  expect_equal(validate_bulma_column_offset(6), "is-offset-6")
  expect_equal(validate_bulma_column_offset(12), "is-offset-12")
  expect_equal(validate_bulma_column_offset("one-third"), "is-offset-one-third")
  expect_equal(validate_bulma_column_offset("three-quarters"), "is-offset-three-quarters")
  expect_equal(validate_bulma_column_offset("three-fifth"), "is-offset-three-fifth")
  expect_equal(validate_bulma_column_offset("4/5"), "is-offset-four-fifth")
  expect_equal(validate_bulma_column_offset("1/2"), "is-offset-half")
  expect_equal(validate_bulma_column_offset(4, "tablet"), "is-offset-4-tablet")
  expect_equal(validate_bulma_column_offset("two-thirds", "mobile"),
               "is-offset-two-thirds-mobile")
})

test_that("media breakpoints are properly validated", {
  expect_null(validate_bulma_media_breakpoint(NULL))
  expect_equal(validate_bulma_media_breakpoint("mobile"), "mobile")
  expect_equal(validate_bulma_media_breakpoint("tablet"), "tablet")
  expect_equal(validate_bulma_media_breakpoint("touch"), "touch")
  expect_equal(validate_bulma_media_breakpoint("desktop"), "desktop")
  expect_equal(validate_bulma_media_breakpoint("widescreen"), "widescreen")
  expect_equal(validate_bulma_media_breakpoint("fullhd"), "fullhd")
})

test_that("improper sizes are raising an error", {
  expect_error(validate_bulma_column_size(13),
               "column size must be an integer between 1 and 12")
  expect_error(validate_bulma_column_size(11.3),
               "column size must be an integer between 1 and 12")
  expect_error(validate_bulma_column_size(NA),
               "column size must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_size(1:2),
               "column size must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_size(c("full", "half")),
               "column size must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_size("1"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_column_size("one-sixth"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_column_size("one.third"),
               "\".*\" is not a valid size specifier")
  expect_error(validate_bulma_column_size("1/1", "fulhd"),
               "\".*\" is not a valid media breakpoint")
})

test_that("improper offsets are raising an error", {
  expect_error(validate_bulma_column_offset(13),
               "column offset must be an integer between 1 and 12")
  expect_error(validate_bulma_column_offset(11.3),
               "column offset must be an integer between 1 and 12")
  expect_error(validate_bulma_column_offset(NA),
               "column offset must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_offset(1:2),
               "column offset must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_offset(c("full", "half")),
               "column offset must be non 'NA' and a single-element numeric or character vector")
  expect_error(validate_bulma_column_offset("1"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_column_offset("1/1"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_column_offset("full"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_column_offset("one-sixth"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_column_offset("one.third"),
               "\".*\" is not a valid offset specifier")
  expect_error(validate_bulma_column_size("3/4", "touchscreen"),
               "\".*\" is not a valid media breakpoint")
})

test_that("improper media breakpoints are rasiing an error", {
  expect_error(validate_bulma_media_breakpoint("fulhd"),
               "\".*\" is not a valid media breakpoint")
  expect_error(validate_bulma_media_breakpoint("touchscreen"),
               "\".*\" is not a valid media breakpoint")
  expect_error(validate_bulma_media_breakpoint("tablett"),
               "\".*\" is not a valid media breakpoint")
})

test_that("bulma columns are properly formatted", {
  expect_equal(as.character(bulma_columns()), "<div class=\"columns\"></div>")
  expect_equal(as.character(bulma_column()), "<div class=\"column\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3")),
               "<div class=\"column is-two-thirds\"></div>")
  expect_equal(as.character(bulma_column(width = "2/3", offset = 3)),
               "<div class=\"column is-two-thirds is-offset-3\"></div>")
  expect_equal(as.character(bulma_column(width = "full", offset = 1,
                                         media_breakpoint = "fullhd")),
               "<div class=\"column is-full-fullhd is-offset-1-fullhd\"></div>")
})
