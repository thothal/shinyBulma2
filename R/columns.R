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
