#' Create Various bulma Elements
#'
#' These functions create the respective bulma containers.
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the container.
#' @param size \[`character(1)`: \sQuote{normal}]\cr
#'        Text size for the `content` container.
#'
#' @name Bulma-Elements
#'
#' @seealso [Bulma Elements](https://bulma.io/documentation/elements/)
#'
#' @return The bulma element container.
#'
#' @examples
#'
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'
#'   blind_text <- tagList(h1("Hello World"),
#'                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
#'                            "Nulla accumsan, metus ultrices eleifend gravida, nulla",
#'                            "nunc varius lectus, nec rutrum justo nibh eu lectus. Ut",
#'                            "vulputate semper dui. Fusce erat odio, sollicitudin vel",
#'                            "erat vel, interdum mattis neque."),
#'                         h2("Second Level"),
#'                         p("Curabitur accumsan turpis pharetra",
#'                           strong("augue tincidunt"), "blandit. Quisque",
#'                           "condimentum maximus mi, sit amet commodo arcu rutrum id.",
#'                           "Proin pretium urna vel cursus venenatis.",
#'                           "Suspendisse potenti. Etiam mattis sem rhoncus lacus",
#'                           "dapibus facilisis. Donec at dignissim dui. Ut et neque",
#'                           "nisl."),
#'                         tags$ul(tags$li("In fermentum leo eu lectus mollis, quis",
#'                                         "dictum mi aliquet."),
#'                                 tags$li("Morbi eu nulla lobortis, lobortis est in,",
#'                                         "fringilla felis."),
#'                                 tags$li("Aliquam nec felis in sapien venenatis",
#'                                         "viverra fermentum nec lectus."),
#'                                 tags$li("Ut non enim metus.")))
#'
#'
#'   ui <- bulma_page(
#'     h1("Block", class = "title is-1"),
#'     bulma_block("This text is within a", strong("block"), "."),
#'     bulma_block("This text is within a", strong("second block"), ". Lorem ipsum",
#'                 "dolor sit amet, consectetur adipiscing elit. Aenean efficitur sit",
#'                 "amet massa fringilla egestas. Nullam condimentum luctus turpis."),
#'     bulma_block("This text is within a ", strong("third block"), ". This block",
#'                 "has no margin at the bottom."),
#'     h1("Box", class = "title is-1"),
#'     bulma_box("I'm a box"),
#'     h1("Content", class = "title is-1"),
#'     h2("Normal Size", class = "title is-3"),
#'     bulma_content(blind_text),
#'     h2("Small Size", class = "title is-3"),
#'     bulma_content(blind_text, size = "small")
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
NULL

#' @rdname Bulma-Elements
#' @export
bulma_block <- function(...) {
  htmltools::div(..., class = "block")
}

#' @rdname Bulma-Elements
#' @export
bulma_box <- function(...) {
  htmltools::div(..., class = "box")
}

#' @rdname Bulma-Elements
#' @export
bulma_content <- function(...,
                          size = c("normal", "small", "medium", "large")) {
  size <- make_class(match.arg(size))
  htmltools::div(..., class = add_class("content", size))
}
