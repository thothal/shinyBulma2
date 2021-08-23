#' Create bulma Icon Container
#'
#' @param name \[`character(n)`\]\cr
#'        Name of the icons.
#' @param text \[`character(n)`; \sQuote{NULL}\]\cr
#'        Text to be displayed next to the icon. Must be of the same length as `name` or
#'        \sQuote{NULL}, in which case a single icon without any text is created. If you
#'        want to skip some text, provide `NA`.
#' @param color \[`character(1)`; \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param size \[`character(1)`; \sQuote{NULL}\]\cr
#'        The size of the icon *container*. The size of the icon itself is controlled via
#'        arguments passed to [shiny::icon] and depends on the icon library used.
#' @param container \[`function`\]\cr
#'        Either `div` or `span`. The parent element of the icon. Only relevant, if we
#'        want to display text next to the icon.
#' @param class,lib,... \[`character(1)` and `html tags` or `html attributes`\]\cr
#'        These arguments are passed down to [shiny::icon].
#'
#' @seealso [validate_bulma_color],
#'          [Bulma Icons](https://bulma.io/documentation/elements/icon/)
#'
#' @return A bulma icon container optionally ammended by some text.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- bulma_page(
#'     tags$head(tags$style(HTML("#sizes > span {vertical-align: middle;}"))),
#'     bulma_block(
#'       bulma_title("Icon with Text"),
#'       bulma_icon("home", "Home")),
#'     bulma_block(
#'       bulma_title("Several Icons with Text"),
#'       bulma_icon(c("train", "arrow-right", "arrow-right",
#'                    "arrow-right", "flag-checkered"),
#'                  c("Paris", "Budapest", "Bucharest", "Istanbul", NA))),
#'     bulma_block(
#'       bulma_title("Inline Icon"),
#'       p("An invitation to", bulma_icon("utensils", "dinner"), "was soon afterwards ",
#'         "dispatched; and already had Mrs. Bennet planned the courses that were to do",
#'         "credit to her housekeeping, when an answer arrived which deferred it all.",
#'         "Mr. Bingley was obliged to be in", bulma_icon("city", "town"),
#'         "the following day, and, consequently, unable to accept the honour of their",
#'         bulma_icon("envelope-open-text", "invitation"), ", etc.")
#'     ),
#'     bulma_block(
#'       bulma_title("Icon With Text, Color in a Different Container"),
#'       bulma_icon("info-circle", "Information", color = "link", container = div),
#'       bulma_block("Your package will be delivered on", strong("Tuesday, 08:00")),
#'       bulma_icon("check-square", "Success", color = "success", container = div),
#'       bulma_block("Your image has been successfully uploaded.")
#'     ),
#'     bulma_block(
#'       id = "sizes",
#'       bulma_title("Icons with Sizing"),
#'       tagAppendAttributes(bulma_icon("home", size = "medium", class = "fas"),
#'                           class = "has-background-warning"),
#'       tagAppendAttributes(bulma_icon("home", size = "medium", class = "fas fa-lg"),
#'                           class = "has-background-warning"),
#'       tagAppendAttributes(bulma_icon("home", size = "medium", class = "fas fa-2x"),
#'                           class = "has-background-warning")
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
bulma_icon <- function(name, text = NULL,
                       color = NULL,
                       size = NULL,
                       container = htmltools::span,
                       class = NULL, lib = "font-awesome", ...) {
  color <- validate_bulma_color(color)
  container <- match.fun(container)
  size <- validate_bulma_size(size)
  if (is.null(text)) {
    if (length(name) > 1) {
      stop("\"name\" must be length one, if no \"text\" is given",
           domain = NA)
    }
    res <- htmltools::span(
      class = "icon",
      shiny::icon(name, class, lib, ...)
    )
    res <- htmltools::tagAppendAttributes(res, class = size)

  } else {
    if (length(name) != length(text)) {
      stop("number of icons must match the number of icon texts",
           domain = NA)
    }
    icons <- Map(function(nm, txt) {
      icon <- htmltools::span(
        class = "icon",
        shiny::icon(nm, class, lib, ...)
      )
      icon <- htmltools::tagAppendAttributes(icon, class = size)
      text <- if (!is.na(txt)) htmltools::span(txt)
      htmltools::tagList(icon, text)
    }, name, text, USE.NAMES = FALSE)
    res <- do.call(container, c(class = "icon-text",
                                unlist(icons, recursive = FALSE)))
  }
  htmltools::tagAppendAttributes(res, class = color)
}

