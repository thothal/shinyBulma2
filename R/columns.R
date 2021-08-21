#' Create a Columns Layout
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the column.
#' @param width \[`character(n)` or `integer(n)`: \sQuote{NULL}\]\cr
#'        The width to be used (see [validate_bulma_column_size] for valid values).
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
#' @seealso [validate_bulma_column_size],[validate_bulma_offset],
#'          [get_bulma_media_breakpoints],
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
#'   ui <- bulma_page(
#'     bulma_title("Equal Size"),
#'     bulma_columns(
#'       bulma_column(bulma_notification("First Column", color = "primary", add_delete = FALSE)),
#'       bulma_column(bulma_notification("Second Column", color = "primary", add_delete = FALSE)),
#'       bulma_column(bulma_notification("Third Column", color = "primary", add_delete = FALSE)),
#'       bulma_column(bulma_notification("Fourth Column", color = "primary", add_delete = FALSE))
#'     ),
#'     bulma_title("Proportional Size"),
#'     bulma_columns(
#'       bulma_column(bulma_notification("is-two-third", color = "primary", add_delete = FALSE),
#'                    width = "2/3"),
#'       bulma_column(bulma_notification("auto", add_delete = FALSE)),
#'       bulma_column(bulma_notification("auto", add_delete = FALSE))
#'     ),
#'     bulma_title("1-12 \"Grid\" Sizes"),
#'     bulma_columns(
#'       bulma_column(bulma_notification("is-11", color = "primary", add_delete = FALSE), width = 11),
#'       bulma_column(bulma_notification("1", add_delete = FALSE))
#'     ),
#'     bulma_title("Offset"),
#'     bulma_columns(
#'       bulma_column(bulma_notification(span("is-half", tags$br(), "is-offset-one-third"),
#'                                       add_delete = FALSE),
#'                    width = "half", offset = "one-third")
#'     ),
#'     bulma_title("Narrow Size"),
#'     bulma_columns(
#'       bulma_column(bulma_box(style = "width: 200px",
#'                        bulma_title("Narrow Column", size = 5, tag = p),
#'                        bulma_subtitle("This column is only 200px wide.")),
#'                    width = "narrow"),
#'       bulma_column(bulma_box(
#'                        bulma_title("Flexible Column", size = 5),
#'                        bulma_subtitle(paste("This column will take up the",
#'                        "remaining space available."))))
#'     ),
#'     bulma_title("Responisve Sizes"),
#'     bulma_columns(
#'       bulma_column(bulma_notification(tags$code("is-three-quarters-mobile"), tags$br(),
#'                                      tags$code("is-two-thirds-tablet"), tags$br(),
#'                                      tags$code("is-half-desktop"), tags$br(),
#'                                      tags$code("is-one-third-widescreen"), tags$br(),
#'                                      tags$code("is-one-quarter-fullhd"), add_delete = FALSE),
#'                    width = c("3/4-mobile", "two-thirds-tablet", "1/2-desktop",
#'                              "1/3-widescreen", "1/4-fullhd")),
#'       bulma_column(bulma_notification("2", add_delete = FALSE)),
#'       bulma_column(bulma_notification("3", add_delete = FALSE)),
#'       bulma_column(bulma_notification("4", add_delete = FALSE)),
#'       bulma_column(bulma_notification("5", add_delete = FALSE)),
#'       enable_from = "mobile"
#'     ),
#'     bulma_title("Nesting"),
#'     bulma_columns(
#'       bulma_column(
#'         bulma_notification("First column", color = "primary", add_delete = FALSE),
#'         bulma_columns(
#'           bulma_column(
#'             bulma_notification("First nested column", color = "primary", add_delete = FALSE)
#'           ),
#'           bulma_column(
#'             bulma_notification("Second nested column", color = "primary", add_delete = FALSE)
#'           ),
#'           enable_from = "mobile"
#'         )
#'       ),
#'       bulma_column(
#'         bulma_notification("Second column", color = "primary", add_delete = FALSE),
#'         bulma_columns(
#'           bulma_column(
#'             bulma_notification("50%", color = "primary", add_delete = FALSE),
#'             width = "1/2"
#'           ),
#'           bulma_column(
#'             bulma_notification("Auto", color = "primary", add_delete = FALSE)
#'           ),
#'           bulma_column(
#'             bulma_notification("Auto", color = "primary", add_delete = FALSE)
#'           )
#'         )
#'       )
#'     ),
#'     bulma_title("Gap Size & Multiline"),
#'     bulma_columns(
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-half", color = "primary", add_delete = FALSE),
#'                    width = "1/2"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("is-one-quarter", color = "primary", add_delete = FALSE),
#'                    width = "1/4"),
#'       bulma_column(bulma_notification("Auto", color = "primary", add_delete = FALSE)),
#'       multiline = TRUE, gap_width = "gapless",
#'       enable_from = "mobile"
#'     ),
#'     bulma_title("Vertical Alignment"),
#'     bulma_columns(
#'       bulma_column(
#'         bulma_notification("First Column", color = "primary", add_delete = FALSE),
#'         width = 8
#'       ),
#'       bulma_column(
#'         bulma_notification(paste("Second column with more content. This is so you can",
#'                                 "see the vertical alignment."), add_delete = FALSE)
#'       ),
#'       center = "vertical"
#'     ),
#'     bulma_title("Horizontal"),
#'     bulma_columns(
#'       bulma_column(bulma_notification("is-half", color = "primary", add_delete = FALSE),
#'                    width = "1/2"),
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
  width <- validate_bulma_column_size(width)
  offset <- validate_bulma_offset(offset)
  column_classes <- trimws(paste("column", width, offset))
  htmltools::tags$div(class = column_classes, ...)
}
