#' Create a bulma Tag
#'
#' @param label \[`character(1)`: \sQuote{NULL}\]\cr
#'        The label of the tag.
#' @param color \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param light \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the `is-light` modifier is used, resulting in a lighter color.
#' @param size \[`character(1)`: \sQuote{NULL}\]\cr
#'        The size of the tag or the tag list. Valid sizes are
#'        `normal`, `medium`, `large`.
#' @param style \[`character(1)`: \sQuote{NULL}\]\cr
#'        Can be either \sQuote{NULL}, \sQuote{delete} or \sQuote{rounded} which
#'        potentially transforms the tag to a \dQuote{delete} tag or a rounded tag.
#' @param add_delete \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} a delete button is added to the right of the tag.
#' @param delete_size \[`character(1)`: \sQuote{small}\]\cr
#'        The size of the additional delete button.
#' @param container \[`function`\]\cr
#'        The container to be used for the `tag` or the tag list.
#'        For the `tag` it should be a function creating either
#'        a `<span>` or an `<a>` HTML tag.
#' @param addons \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE} the tags in the tag list are attached together.
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Further arguments passed down to `container`.
#'
#' @seealso [Bulma Tag](https://bulma.io/documentation/elements/tag/)
#'
#' @return A bulma `tag` or a tag list.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   numbers <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
#'                "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen",
#'                "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty")
#'   ui <- bulma_page(
#'     bulma_block(
#'       bulma_title("Simple Tag"),
#'       bulma_button("Tag label")
#'     ),
#'     bulma_block(
#'       bulma_title("Different Colors"),
#'       bulma_tag("Dark", color = "dark"),
#'       bulma_tag("Light", color = "light"),
#'       bulma_tag("White", color = "white"),
#'       bulma_tag("Primary", color = "primary"),
#'       bulma_tag("Link", color = "link"),
#'       bulma_tag("Info", color = "info"),
#'       bulma_tag("Success", color = "success"),
#'       bulma_tag("Warning", color = "warning"),
#'       bulma_tag("Danger", color = "danger")
#'     ),
#'     bulma_block(
#'       bulma_title("Light Colors"),
#'       bulma_tag("Primary", color = "primary", light = TRUE),
#'       bulma_tag("Link", color = "link", light = TRUE),
#'       bulma_tag("Info", color = "info", light = TRUE),
#'       bulma_tag("Success", color = "success", light = TRUE),
#'       bulma_tag("Warning", color = "warning", light = TRUE),
#'       bulma_tag("Danger", color = "danger", light = TRUE)
#'     ),
#'     bulma_block(
#'       bulma_title("Different Sizes"),
#'       bulma_tag("Normal [Default]", size = "normal", color = "link"),
#'       bulma_tag("Medium", size = "medium", color = "primary"),
#'       bulma_tag("Large", size = "large", color = "info"),
#'       bulma_tags(
#'         size = "medium",
#'         bulma_tag("All"),
#'         bulma_tag("Medium"),
#'         bulma_tag("Size")
#'       ),
#'       bulma_tags(
#'         size = "large",
#'         bulma_tag("All"),
#'         bulma_tag("Large"),
#'         bulma_tag("Size")
#'       ),
#'       bulma_tags(
#'         size = "medium",
#'         bulma_tag("Medium"),
#'         bulma_tag("Normal", size = "normal"),
#'         bulma_tag("Medium"),
#'         bulma_tag("Large", size = "large"),
#'         bulma_tag("Medium")
#'       ),
#'       bulma_block(
#'         bulma_title("Modifiers"),
#'         bulma_tag("Rounded", style = "rounded"),
#'         bulma_tag(style = "delete", container = htmltools::a)
#'       ),
#'       bulma_block(
#'         bulma_title("Combinations"),
#'         bulma_tag("Bar", color = "success", add_delete = TRUE),
#'         bulma_tag("Hello", color = "warning", size = "medium", add_delete = TRUE),
#'         bulma_tag("World", color = "danger", size = "large", add_delete = TRUE,
#'                   delete_size = NULL)
#'       ),
#'       bulma_block(
#'         bulma_title("List of Tags"),
#'         bulma_tags(
#'           lapply(numbers[1:3], bulma_tag)
#'         ),
#'         bulma_tags(
#'           lapply(numbers, bulma_tag)
#'         )
#'       ),
#'       bulma_block(
#'         bulma_title("Tag Addons"),
#'         bulma_tags(
#'           bulma_tag("Package"),
#'           bulma_tag("shinyBulma", color = "primary"),
#'           addons = TRUE
#'         ),
#'         bulma_tags(
#'           bulma_tag("Alex Smith", color = "danger"),
#'           bulma_tag(color = "dark", style = "delete", container = htmltools::a),
#'           addons = TRUE
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
bulma_tag <- function(label = NULL,
                      color = NULL,
                      light = FALSE,
                      size = NULL,
                      style = NULL,
                      add_delete = FALSE,
                      delete_size = "small",
                      container = htmltools::span,
                      ...) {
  size_class <- validate_bulma_size(size, FALSE, disallow = "small")
  valid_styles <- c("delete", "rounded")
  style_class <- if (!is.null(style)) make_class(match.arg(style, valid_styles, TRUE))
  color_class <- validate_bulma_color(color, "background", must_be_key = TRUE)
  if (light) {
    color_class <- add_class(color_class, make_class("light"))
  }
  cls <- add_class("tag",
                   c(color_class, size_class, style_class))
  tag <- container(label,
                   class = cls,
                   ...)
  if (add_delete) {
    tag <- htmltools::tagAppendChild(tag, bulma_delete(delete_size))
  }
  tag
}

#' @rdname bulma_tag
#' @export
bulma_tags <- function(...,
                       addons = FALSE,
                       size = NULL,
                       container = htmltools::div) {
  addons_class <- if (addons) make_class("addons", prefix = "has")
  size_class <- validate_bulma_size(size, prefix = "are", disallow = "small")
  container(..., class = add_class("tags",
                                   c(addons_class, size_class)))
}
