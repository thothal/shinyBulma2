#' Validate Proper Width, Offset or Gap Definition for a Bulma Column Layout
#'
#' `validate_bulma_unit` checks that the argument is a valid column
#' width, offset or gap specifier, respectively.
#'
#' `validate_bulma_{size|offset|gap}` are convenience wrappers which set `type` to the
#' respective value.
#'
#' `get_bulma_media_breakpoints` returns all valid media breakpoints.
#'
#' @param x \[`character(n)` or `integer(n)`\]\cr
#'        The sizes, offsets or gaps to validate.
#' @param type \[`character(1)`: \sQuote{size}\]\cr
#'        The type of unit we want to validate. Must be one of `size`, `offset` or `gap`.
#'
#' @details
#' Valid values for sizes and offsets are integers between 1 and 12, or strings like
#' `two-fifth` or `full`. For the size `narrow` is also a valid option. Additionally, to
#' the original specifiers
#' (cf. [column sizes](https://bulma.io/documentation/columns/sizes/)), you can also
#' abbreviate the number-ish strings, e.g. `"2/5"` instead of `"two-fifth"`.
#'
#' Gap values are integers between 0 and 8 or the string \dQuote{gapless}.
#'
#' All values can additionally be suffixed by valid media breakpoint qualifiers
#' in which case these sizes are only valid on screens which meet at least the size given
#' by the breakpoints.
#'
#' Multiple values can be passed. This makes sense, if we want to define different values
#' for different media breakpoints. Warnings are issued, either if
#' duplicated media breakpoints are detected, or if we provide multiple values without
#' media breakpoints.
#'
#' @return The proper bulma size, offset or gap classes, if possible. Otherwise an error
#' is raised.
#' `get_bulma_media_breakpoints` returns a character vector with all valid breakpoints.
#' @export
#'
#' @examples
#' validate_bulma_size(1)
#' validate_bulma_size("one-third")
#' validate_bulma_size("1/1")
#' validate_bulma_size("narrow-tablet")
#' validate_bulma_size(c("1-touch", "2-desktop", "3-fullhd"))
#'
#' validate_bulma_offset(6)
#' validate_bulma_offset("two-thirds")
#' validate_bulma_offset("4/5-desktop")
#' validate_bulma_offset(c("two-thirds-touch", "11-desktop", "4/5-fullhd"))
#'
#' validate_bulma_gap("gapless")
#' validate_bulma_gap(1)
#' validate_bulma_gap("2-touch")
#' validate_bulma_gap(c("1-touch", "2-mobile", "3-desktop"))
#'
#' get_bulma_media_breakpoints()
#'
#' \dontrun{
#' ## these will each raise an error
#' validate_bulma_size(13)
#' validate_bulma_size(11.3)
#' validate_bulma_size("one-sixth")
#' validate_bulma_size("one.third")
#' validate_bulma_size("1-touchscreen")
#' validate_bulma_size(c("1-fullhd", "2"))
#' validate_bulma_size(c("1-fullhds", "2-fullhds"))
#'
#' validate_bulma_offset("narrow")
#' validate_bulma_offset("full")
#' validate_bulma_offset("1/1")
#' validate_bulma_offset("1/3-4k")
#'
#' validate_bulma_gap(9)
#' validate_bulma_gap("one-third")
#'
#' ## these will raise warnings
#' validate_bulma_size(1:3)
#' validate_bulma_offset(c("1-fullhd", "2-fullhd"))
#' }
validate_bulma_unit  <- function(x,
                                 type = c("size", "offset", "gap")) {
  type <- match.arg(type)
  if (is.null(x)) {
    return(x)
  }
  if (any(is.na(x))) {
    stop("column ", type,
         " must not contain any 'NAs'",
         domain = NA)
  }
  if (!(is.character(x)  || is.numeric(x))) {
    stop("column ", type,
         " must be a numeric or character vector",
         domain = NA)
  }
  if (is.numeric(x)) {
    if (type == "gap") {
      if (any(x < 0 | x > 8 | !is_integer(x))) {
        stop("column ", type, " must be an integer between 0 and 8",
             domain = NA)
      }
    } else {
      if (any(x < 1 | x > 12 | !is_integer(x))) {
        stop("column ", type, " must be an integer between 1 and 12",
             domain = NA)
      }
    }
    res <- x
    breakpoints <- character(0)
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
    valid_qualifiers <- c(standard_qualifiers,
                          aliases,
                          "gapless",
                          as.character(0:12) ## add this to allow for e.g. 1-touch
    )
    lkp <- stats::setNames(c(rep(standard_qualifiers, 2), "gapless", as.character(0:12)),
                           valid_qualifiers)

    ## extract any valid media breakpoint
    pattern <- paste0("(", paste(get_bulma_media_breakpoints(), collapse = "|"), ")$")
    matches <- regexpr(pattern, x)
    breakpoints <- regmatches(x, matches)
    ## make sure that there are either 0 breakpoints or as many as elements
    if (length(breakpoints) && length(breakpoints) != length(x)) {
      NOK <- matches == -1
      bad_media <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
      msg <- ngettext(sum(NOK),
                      paste(bad_media, "does not contain a valid",
                            "media breakpoint"),
                      paste(bad_media, "do not contain valid",
                            "media breakpoints"))
      stop("column ", type, " must contain breakpoints either for all elements ",
           "or for none at all:\n", msg, domain = NA)
    }
    if (type == "size") {
      valid_qualifiers <- setdiff(c(valid_qualifiers, "narrow"), c("0", "gapless"))
      lkp <- c(lkp, narrow = "narrow")
    } else if (type == "offset") {
      valid_qualifiers <- setdiff(valid_qualifiers, c("1/1", "full", "0", "gapless"))
    } else if (type == "gap") {
      valid_qualifiers <- intersect(valid_qualifiers, c("gapless", as.character(0:8)))
    }
    ## remove any valid(!) breakpoints
    x <- sub(paste0("-", pattern), "", x)
    NOK <- !x %in% valid_qualifiers
    if (any(NOK)) {
      bad_cols <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
      msg <- ngettext(sum(NOK),
                      paste(bad_cols, "is not a valid", type, "specifier"),
                      paste(bad_cols, "are not valid", type, "specifiers"))
      stop(msg,
           domain = NA)
    }
    res <- lkp[x]
  }
  if (any(duplicated(breakpoints))) {
    warning("duplicated media breakpoints found - this may lead to unwanted results")
  }
  if (!length(breakpoints) && length(x) > 1) {
    warning("multiple sizes without media breakpoints found - this may lead to",
            " unwanted results")
  }
  if (type == "size") {
    make_class(res, breakpoints)
  } else if (type == "offset") {
    make_class("offset", res, breakpoints)
  } else if (type == "gap") {
    class <- make_class(res, breakpoints)
    if (any(res != "gapless")) {
      class <- add_class(class, make_class("variable"))
    }
    class
  }
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_size <- function(x) {
  validate_bulma_unit(x, "size")
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_offset <- function(x) {
  validate_bulma_unit(x, "offset")
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_gap <- function(x) {
  validate_bulma_unit(x, "gap")
}

#' @rdname validate_bulma_unit
#' @export
get_bulma_media_breakpoints <- function(x) {
  c("mobile", "tablet", "touch",
    "desktop", "widescreen", "fullhd")
}

#' Create a Columns Layout
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the column.
#' @param width \[`character(n)` or `integer(n)`: \sQuote{NULL}\]\cr
#'        The width to be used (see [validate_bulma_size] for valid values).
#'        If `NULL`, all available space is used. If several columns use `NULL`, the
#'        space is distributed among those columns.
#' @param offset \[`character(n)` or `integer(n)`: \sQuote{NULL}\]\cr
#'        The offset to be used (see [validate_bulma_offset] for valid values).
#' @param gap_width \[`integer(1)`: sQuote{NULL}\]\cr
#'        The size of the gap between columns. Must be an integer between 0 and 8.
#' @param center \[`character(1)`: \sQuote{none}\]\cr
#'        Must be either \dQuote{none}, \dQuote{horizontal}, \dQuote{vertical} or
#'        \dQuote{both} specifying whether columns should be horizontally and/or
#'        vertically centered.
#' @param multiline \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        Should the columns be defined as \dQuote{multiline}, i.e. spreading more than
#'        one line in case space is needed.
#' @param enable_from \[`character(1)`: \sQuote{NULL}\]\cr
#'        Define where columns are shown in the first place. By default columns are used
#'        on tablets onwards and stacked at lower widths. By specifying either `mobile` or
#'        `desktop` you can either show columns earlier or later. Other media breakpoints
#'        are ignored and will raise a warning.
#'
#' @seealso [validate_bulma_size],[validate_bulma_offset],
#'          [get_bulma_media_breakpoints],
#'          [Bulma Columns](https://bulma.io/documentation/columns/basics/)
#'
#' @return A Bulma column layout.
#' @export
#'
#' @examples
#'
#' ## Only run examples in interactive R sessions
#' ## Examples inspired by <https://bulma.io/documentation/columns/>
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'
#'   make_notification <- function(..., color = TRUE) {
#'     p(..., class = "notification",
#'       class = if (color) "has-background-primary")
#'   }
#'
#'   ui <- bulma_page(
#'     h1("Equal Size", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification("First Column")),
#'       bulma_column(make_notification("Second Column")),
#'       bulma_column(make_notification("Third Column")),
#'       bulma_column(make_notification("Fourth Column"))
#'     ),
#'     h1("Proportional Size", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification("is-two-third"), width = "2/3"),
#'       bulma_column(make_notification("auto", color = FALSE)),
#'       bulma_column(make_notification("auto", color = FALSE))
#'     ),
#'     h1("1-12 \"Grid\" Sizes", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification("is-11"), width = 11),
#'       bulma_column(make_notification("1", color = FALSE))
#'     ),
#'     h1("Offset", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification(span("is-half", tags$br(), "is-offset-one-third")),
#'                    width = "half", offset = "one-third")
#'     ),
#'     h1("Narrow Size", class = "title"),
#'     bulma_columns(
#'       bulma_column(div(class = "box", style = "width: 200px",
#'                        p(class = "title is-5", "Narrow Column"),
#'                        p(class = "subtitle", "This column is only 200px wide.")),
#'                    width = "narrow"),
#'       bulma_column(div(class = "box",
#'                        p(class = "title is-5", "Flexible Column"),
#'                        p(class = "subtitle", paste("This column will take up the",
#'                        "remaining space available."))))
#'     ),
#'     h1("Responisve Sizes", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification(tags$code("is-three-quarters-mobile"), tags$br(),
#'                                      tags$code("is-two-thirds-tablet"), tags$br(),
#'                                      tags$code("is-half-desktop"), tags$br(),
#'                                      tags$code("is-one-third-widescreen"), tags$br(),
#'                                      tags$code("is-one-quarter-fullhd")),
#'                    width = c("3/4-mobile", "two-thirds-tablet", "1/2-desktop",
#'                              "1/3-widescreen", "1/4-fullhd")),
#'       bulma_column(make_notification(2)),
#'       bulma_column(make_notification(3)),
#'       bulma_column(make_notification(4)),
#'       bulma_column(make_notification(5)),
#'       enable_from = "mobile"
#'     ),
#'     h1("Nesting", class = "title"),
#'     bulma_columns(
#'       bulma_column(
#'         make_notification("First column"),
#'         bulma_columns(
#'           bulma_column(
#'             make_notification("First nested column")
#'           ),
#'           bulma_column(
#'             make_notification("Second nested column")
#'           ),
#'           enable_from = "mobile"
#'         )
#'       ),
#'       bulma_column(
#'         make_notification("Second column"),
#'         bulma_columns(
#'           bulma_column(
#'             make_notification("50%"),
#'             width = "1/2"
#'           ),
#'           bulma_column(
#'             make_notification("Auto")
#'           ),
#'           bulma_column(
#'             make_notification("Auto")
#'           )
#'         )
#'       )
#'     ),
#'     h1("Gap Size & Multiline", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-half"),
#'                    width = "1/2"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("is-one-quarter"),
#'                    width = "1/4"),
#'       bulma_column(make_notification("Auto")),
#'       multiline = TRUE, gap_width = "gapless",
#'       enable_from = "mobile"
#'     ),
#'     h1("Vertical Alignment", class = "title"),
#'     bulma_columns(
#'       bulma_column(
#'         make_notification("First Column"),
#'         width = 8
#'       ),
#'       bulma_column(
#'         make_notification(paste("Second column with more content. This is so you can",
#'                                 "see the vertical alignment."))
#'       ),
#'       center = "vertical"
#'     ),
#'     h1("Horizontal", class = "title"),
#'     bulma_columns(
#'       bulma_column(make_notification("is-half"), width = "1/2"),
#'       center = "horizontal"
#'     )
#'   )
#'
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shiny::shinyApp(ui, server)
#' }
bulma_columns <- function(..., center = c("none", "horizontal", "vertical", "both"),
                          multiline = FALSE, gap_width = NULL,
                          enable_from = NULL) {
  class <- "columns"
  center <- match.arg(center)
  valid_breakpoints <- c("mobile", "desktop")
  if (!is.null(enable_from)) {
    if (enable_from %in% valid_breakpoints) {
      class <- add_class(class, make_class(enable_from))
    } else {
      warning("\"", enable_from, "\" is not supported by bulma columns")
    }
  }
  gap_class <- validate_bulma_gap(gap_width)
  if (!is.null(gap_class)) {
    class <- add_class(class, gap_class)
  }
  if (multiline) {
    class <- add_class(class, make_class("multiline"))
  }
  if (center %in% c("both", "horizontal")) {
    class <- add_class(class, make_class("centered"))
  }
  if (center %in% c("both", "vertical")) {
    class <- add_class(class, make_class("vcentered"))
  }
  htmltools::tags$div(class = class, ...)
}

#' @rdname bulma_columns
#' @export
bulma_column <- function(..., width = NULL, offset = NULL) {
  width <- validate_bulma_size(width)
  offset <- validate_bulma_offset(offset)
  column_classes <- trimws(paste("column", width, offset))
  htmltools::tags$div(class = column_classes, ...)
}
