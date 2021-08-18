#' Create a bulma Notification
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the container.
#' @param color \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param light \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the `is-light` modifier is used, resulting in a lighter color.
#'
#' @seealso [Bulma Notification](https://bulma.io/documentation/elements/notification/)
#'
#' @return A bulma `notification` element.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'    text <- tagList(
#'      "Primar lorem ipsum dolor sit amet, consectetur",
#'      "adipiscing elit lorem ipsum dolor. ",
#'      tags$strong("Pellentesque risus mi"),
#'      "tempus quis placerat ut, porta nec nulla. Vestibulum rhoncus ac ex sit amet fringilla.",
#'      " Nullam gravida purus diam, et dictum ",
#'      a("felis venenatis"),
#'      " efficitur."
#'    )
#'
#'    cols <- bulma_config[bulma_config$is_color_map_key, "variable"]
#'
#'    ui <- bulma_page(
#'      bulma_block(
#'        bulma_title("Basic Notification"),
#'        bulma_notification(text)
#'      ),
#'      bulma_block(
#'        bulma_title("Colored Notification"),
#'        lapply(cols, function(col) bulma_notification(text, color = col))
#'      ),
#'      bulma_block(
#'        bulma_title("Light Colored Notification"),
#'        lapply(cols, function(col) bulma_notification(text, color = col, light = TRUE))
#'      )
#'    )
#'
#'    server <- function(input, output) {
#'    }
#'
#'    shinyApp(ui, server)
#' }
bulma_notification <- function(..., color = NULL, light = FALSE) {
  color <- validate_bulma_color(color, "background", must_be_key = TRUE)
  if (light) {
    color <- add_class(color, make_class("light"))
  }
  htmltools::div(bulma_delete(), ..., class = add_class("notification", color))
}
