validate_column_or_offset  <- function(x, media_breakpoint = NULL,
                                       type = c("size", "offset")) {
  type <- match.arg(type)
  if (is.null(x)) {
    return(x)
  }
  if (length(x) > 1 || (!is.character(x) && !is.numeric(x)) || is.na(x)) {
    stop("column ", type,
         " must be non 'NA' and a single-element numeric or character vector",
         domain = NA)
  }

  if (is.numeric(x)) {
    if (x < 1 || x > 12 || !is_integer(x)) {
      stop("column ", type, " must be an integer between 1 and 12",
           domain = NA)
    }
    res <- x
  } else if (is.character(x)) {
    standard_qualifiers <- c("full",
                             "four-fifth",
                             "three-quarters",
                             "two-thirds",
                             "three-fifth",
                             "half",
                             "two-fifth",
                             "one-third",
                             "one-quarter",
                             "one-fifth")
    aliases <- c("1/1", "4/5", "3/4", "2/3", "3/5", "1/2", "2/5", "1/3", "1/4", "1/5")
    valid_qualifiers <- c(standard_qualifiers, aliases)
    lkp <- stats::setNames(rep(standard_qualifiers, 2), valid_qualifiers)
    if (type == "size") {
      valid_qualifiers <- c(valid_qualifiers, "narrow")
      lkp <- c(lkp, narrow = "narrow")
    } else if (type == "offset") {
      valid_qualifiers <- setdiff(valid_qualifiers, c("1/1", "full"))
    }
    if (!x %in% valid_qualifiers) {
      stop("\"", x, "\" is not a valid ", type, " specifier - valid specifiers are:\n",
           paste0("'", valid_qualifiers, "'", collapse = ", "),
           domain = NA)
    }
    res <- lkp[x]
  }
  if (!is.null(media_breakpoint)) {
    media_breakpoint <- paste0("-", media_breakpoint)
  }
  if (type == "size") {
    paste0("is-", res, media_breakpoint)
  } else {
    paste0("is-offset-", res, media_breakpoint)
  }
}

#' Validate Proper Width, Offset or Media Breakpoint Definition for a Bulma Column
#'
#' `validate_bulma_column_(size|offset)` checks that the argument is a valid column width
#' or offset specifier, respectively. Valid values are integers between 1 and 12, or
#' strings like `two-fifth` or `full`. For the size `narrow` is also a valid option.
#' Additionally, to the original specifiers
#' (cf. [column sizes](https://bulma.io/documentation/columns/sizes/)), you can also
#' abbreviate the number-ish strings, e.g. `"2/5"` instead of `"two-fifth"`.
#'
#' `validate_media_brakpoint` checks that the argument is a valid media breakpoint.
#'
#' @param x \[`character(1)` or `integer(1)`\]\cr
#'        The size, offset or media breakpoint to validate.
#' @param media_breakpoint \[`character(1)`: \sQuote{NULL}\]\cr
#'        The media breakpoint to be used.
#'
#' @return The proper bulma size or offset class, if possible. Otherwise an error.
#' `validate_bulma_media_brakpoint` returns simply its argument if it is valid,
#' otherwise it will raise an error.
#' @export
#'
#' @examples
#' validate_bulma_column_size(1)
#' validate_bulma_column_size("one-third")
#' validate_bulma_column_size("1/1")
#' validate_bulma_column_size("narrow", "tablet")
#'
#' validate_bulma_column_offset(6)
#' validate_bulma_column_offset("two-thirds")
#' validate_bulma_column_offset("4/5", "desktop")
#'
#' validate_bulma_media_breakpoint("fullhd")
#' validate_bulma_media_breakpoint("mobile")
#' validate_bulma_media_breakpoint("touch")
#'
#' \dontrun{
#' ## these will each raise an error
#' validate_bulma_column_size(13)
#' validate_bulma_column_size(1:2)
#' validate_bulma_column_size(11.3)
#' validate_bulma_column_size("one-sixth")
#' validate_bulma_column_size("one.third")
#' validate_bulma_column_size(1, "touchscreen")
#'
#' validate_bulma_column_offset("narrow")
#' validate_bulma_column_offset("full")
#' validate_bulma_column_offset("1/1")
#' validate_bulma_column_offset("1/3", "4k")
#'
#' validate_bulma_media_breakpoint("fulhd")
#' }
validate_bulma_column_size <- function(x, media_breakpoint = NULL) {
  validate_column_or_offset(x, validate_bulma_media_breakpoint(media_breakpoint), "size")
}

#' @rdname validate_bulma_column_size
#' @export
validate_bulma_column_offset <- function(x, media_breakpoint = NULL) {
  validate_column_or_offset(x, validate_bulma_media_breakpoint(media_breakpoint), "offset")
}

#' @rdname validate_bulma_column_size
#' @export
validate_bulma_media_breakpoint <- function(x) {
  valid_breakpoints <- c("mobile", "tablet", "touch",
                         "desktop", "widescreen", "fullhd")
  if (!(is.null(x) || x %in% valid_breakpoints)) {
    stop("\"", x, "\" is not a valid media breakpoint - valid breakpoints are:\n",
         paste0("'", valid_breakpoints, "'", collapse = ", "),
         domain = NA)
  }
  x
}

#' Create a Columns Layout
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the column.
#' @param width \[`character(1)` or `integer(1)`: \sQuote{NULL}\]\cr
#'        The width to be used (see [validate_bulma_column_size] for valid values).
#'        If `NULL`, all available space is used. If several columns use `NULL`, the
#'        space is distributed among those columns.
#' @param offset \[`character(1)` or `integer(1)`: \sQuote{NULL}\]\cr
#'        The offset to be used (see [validate_bulma_column_offset] for valid values).
#' @param media_breakpoint \[`character(1)`: \sQuote{NULL}\]\cr
#'
#' @seealso [validate_bulma_column_size],[validate_bulma_column_offset],
#'          [validate_bulma_media_breakpoint],
#'          [Bulma Columns](https://bulma.io/documentation/columns/basics/)
#'
#' @return A Bulma column layout.
#' @export
#'
#' @examples
#'
#' ## Only run examples in interactive R sessions
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'
#' ui <- bulma_page(
#'         bulma_columns(
#'            bulma_column(tags$h1("Column 1"), width = 3, offset = 2,
#'                         class = "has-background-primary"),
#'            bulma_column(tags$h1("Column 2"), width = 4, offset = 1,
#'                         class = "has-background-link")
#'         )
#'       )
#' server <- function(input, output) {
#' }
#'
#' shiny::shinyApp(ui, server)
#' }
bulma_columns <- function(...) {
  htmltools::tags$div(class = "columns", ...)
}

#' @rdname bulma_columns
#' @export
bulma_column <- function(..., width = NULL, offset = NULL, media_breakpoint = NULL) {
  width <- validate_bulma_column_size(width, media_breakpoint)
  offset <- validate_bulma_column_offset(offset, media_breakpoint)
  column_classes <- trimws(paste("column", width, offset))
  htmltools::tags$div(class = column_classes, ...)
}
