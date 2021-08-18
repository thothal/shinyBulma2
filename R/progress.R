#' Create a Bulma Progress Bar
#'
#' @param value \[`numeric(1)`: \sQuote{NULL}\]\cr
#'        The value of the progress bar. If \sQuote{NULL} the progress bar shows an
#'        indeterminate progress.
#' @param max \[`numeric(1)`: \sQuote{100}\]\cr
#'        The maximum value of the progress bar.
#' @param color \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param size \[`character(1)`: \sQuote{NULL}\]\cr
#'        The size of the progress bar. Valid sizes are
#'        `small`, `normal`, `medium`, `large`.
#'
#' @seealso [Bulma Progressbar](https://bulma.io/documentation/elements/progress/)
#'
#' @return A bulma progress bar.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'    cols <- bulma_config[bulma_config$is_color_map_key, "variable"]
#'    perc <- 10 * seq_along(cols)
#'    ui <- bulma_page(
#'      bulma_block(
#'        bulma_title("Basic Progressbar"),
#'        bulma_progressbar(15)
#'      ),
#'      bulma_block(
#'        bulma_title("Colored Progressbars"),
#'        Map(function(val, col) bulma_progressbar(val, color = col),
#'            perc[-c(5, 10)], cols[-c(5, 10)])
#'      ),
#'      bulma_block(
#'        bulma_title("Different Sizes"),
#'        Map(function(val, size) bulma_progressbar(val, size = size),
#'            15 * 1:4, c("small", "normal", "medium", "large"))
#'      ),
#'      bulma_block(
#'        bulma_title("Indeterminate Progressbar"),
#'        Map(function(col) bulma_progressbar(color = col), cols)
#'      )
#'    )
#'
#'    server <- function(input, output) {
#'    }
#'
#'    shinyApp(ui, server)
#' }
bulma_progressbar <- function(value = NULL, max = 100, color = NULL, size = NULL) {
  max <- max %||% 1
  stopifnot(max > 0)
  stopifnot(is.null(value) || value <= max)
  color_class <- validate_bulma_color(color, "background", must_be_key = TRUE)
  size_class <- validate_bulma_size(size)
  if (!is.null(value)) {
    htmltools::tags$progress(class = add_class("progress",
                                               c(color_class, size_class)),
                             max = max, value = value,
                             paste0(round(value / max * 100, 2), "%"))
  } else {
    htmltools::tags$progress(class = add_class("progress",
                                               c(color_class, size_class)),
                             max = max)
  }
}
