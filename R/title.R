#' Create a bulma Title or Subtitle
#'
#' `bulma_heading` creates a title or a subtitle for bulma.
#'
#' `bulma_title` and `bulma_subtitle` are convenience wrappers which set `type`
#'  accordingly.
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the container.
#' @param tag \[`function`\: \sQuote{h1} or \sQuote{h2}]\cr
#'        The container for the title. Typically one of `htmltools::h[1-6]`.
#' @param size \[`numeric(1)`: \sQuote{NULL}\]\cr
#'        The size for the title. A valid size is an integer between 1 and 6.
#' @param type \[`character(1)`: \sQuote{NULL}]\cr
#'        The type of the heading, must be either \sQuote{title} or \sQuote{subtitle}.
#' @param maintain_space \[`logical(1)`: \sQuote{FALSE}]\cr
#'        If \sQuote{TRUE} the sapce between a title and a subtitle is maintained.
#'        Otherwise they move closer together.
#'
#' @seealso [Bulma Titles](https://bulma.io/documentation/elements/title/)
#'
#' @return A bulma title or subtitle.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'    ui <- bulma_page(
#'      bulma_block(
#'        bulma_title("Basic Usage",
#'                    size = 1,
#'                    class = make_class("text-primary", prefix = "has")),
#'        bulma_title("A Title"),
#'        bulma_subtitle("A Subtitle")
#'      ),
#'      bulma_block(
#'        bulma_title("Different Sizes",
#'                    size = 1,
#'                    class = make_class("text-primary", prefix = "has")),
#'        Map(function(size) {
#'          text <- paste("Title", size)
#'          if (size == 3) {
#'            text <- paste(text, "(default size)")
#'          }
#'          bulma_title(text, size = size)
#'        }, 1:6),
#'        Map(function(size) {
#'          text <- paste("Subtitle", size)
#'          if (size == 5) {
#'            text <- paste(text, "(default size)")
#'          }
#'          bulma_title(text, size = size)
#'        }, 1:6)
#'      ),
#'      bulma_block(
#'        bulma_title("Combine Title and Subtitle",
#'                    size = 1,
#'                    class = make_class("text-primary", prefix = "has")),
#'        Map(function(size) {
#'          htmltools::tagList(
#'            bulma_title(paste("Title", size), size = size),
#'            bulma_subtitle(paste("Subtitle", size + 2), size = size + 2)
#'          )
#'        }, 1:3)
#'      ),
#'      bulma_block(
#'        bulma_title("Maintain Space Between Title and Subtitle",
#'                    size = 1,
#'                    class = make_class("text-primary", prefix = "has")),
#'        Map(function(size) {
#'          htmltools::tagList(
#'            bulma_title(paste("Title", size), size = size, maintain_space = TRUE),
#'            bulma_subtitle(paste("Subtitle", size + 2), size = size + 2)
#'          )
#'        }, 1:3)
#'      )
#'    )
#'
#'    server <- function(input, output) {
#'    }
#'
#'    shinyApp(ui, server)
#' }
bulma_heading <- function(...,
                          tag = if (type == "title") htmltools::h1 else htmltools::h2,
                          size = NULL,
                          type = c("title", "subtitle"),
                          maintain_space = FALSE) {
  type <- match.arg(type)
  tag <- match.fun(tag)
  if (!is.null(size) && !(is_integer(size) && size >= 1 && size <= 6)) {
    stop("size must be an integer between 1 and 6",
         domain = NA)
  } else {
    size_class <- make_class(size)
  }
  if (type == "subtitle" && maintain_space) {
    warning("`maintain_space = TRUE` makes only sense for titles and not for subtitles",
            domain = NA)
    maintain_space <- FALSE
  }
  maintain_class <- if (maintain_space) make_class("spaced")
  tag(..., class = add_class(type, c(size_class, maintain_class)))
}

#' @rdname bulma_heading
#' @export
bulma_title <- function(...,
                        tag = htmltools::h1,
                        size = NULL,
                        maintain_space = FALSE) {
  bulma_heading(...,
                tag = tag,
                size = size,
                type = "title",
                maintain_space = maintain_space)
}

#' @rdname bulma_heading
#' @export
bulma_subtitle <- function(...,
                           tag = htmltools::h2,
                           size = NULL) {
  bulma_heading(...,
                tag = tag,
                size = size,
                type = "subtitle")
}
