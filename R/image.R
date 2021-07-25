#' Create a Responsive Figure
#'
#' The bulma `image` class creates a container with either a fixed (squared) size or a
#' defined ratio. The former is typically used for avatars, while the latter can display
#' arbitrary sized images (with the only requirement that the parent has a defined
#' `width`). Either way, the container is precisely sized, which prevents the design to
#' break or wobble in case an image needs longer to load or does not load at all.
#'
#' @param src \[`character(1)`\]\cr
#'        Path to the image to be displayed.
#' @param alt \[`character(1)`: \sQuote{NULL}\]\cr
#'        Alternative text description for the image.
#' @param ... \[`named arguments`\]\cr
#'        Further attributes to be passed to the `<img>` tag.
#' @param fixed \[`integer(1)`: \sQuote{NULL}\]\cr
#'        A valid size for a *fixed sized* bulma image or \sQuote{NULL}. Valid sizes are
#'        16, 24, 32, 48, 64, 96 and 128.
#' @param ratio \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid ratio for a *ratio sized* bulma image or \sQuote{NULL}. Valid ratios are
#'        `square`, `1by1`, `5by4`, `4by3`, `3by2`, `5by3`, `16by9`, `2by1`, `3by1`,
#'        `4by5`, `3by4`, `2by3`, `3by5`, `9by16`, `1by2`, `1by3`.
#' @param rounded \[`logical(1)`: \sQuote{FALSE}]\cr
#'        Should the image be rounded?
#' @param use_fullwidth \[`logical(1)`: \sQuote{FALSE}]\cr
#'        By default, the image container takes up the full width while maintaining the
#'        given ratio. If it does not, you can force it by setting `use_fullwidth` to
#'        \sQuote{TRUE}.
#'
#' @return A bulma `figure` tag containing the given `<img>`.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ## some blind text
#'   li <- paste("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam",
#'               "nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam",
#'               "erat, sed diam voluptua. At vero eos et accusam et justo duo dolores",
#'               "et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est",
#'               "Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur",
#'               "sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et",
#'               "dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam",
#'               "et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea",
#'               "takimata sanctus est Lorem ipsum dolor sit amet.")
#'
#'   ## show the effect of bulma_image:
#'   ## the right column does not change in size while the left does as soon as the
#'   ## image is loaded
#'
#'   ## delayed placeholder image
#'   src <- "http://www.deelay.me/2000/http://placehold.it/800x600"
#'   ui <- bulma_page(div(class = "container",
#'                        bulma_columns(bulma_column(tags$figure(img(src = src)), li),
#'                                      bulma_column(bulma_image(src, ratio = "4by3"),
#'                                                   li))))
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
bulma_image <- function(src, alt = NULL, ...,
                        fixed = NULL, ratio = NULL, rounded = FALSE,
                        use_fullwidth = FALSE) {
  ## need the `is.null` clause, because otherwise it will take the first per default
  if (!is.null(fixed)) {
    valid_sizes <- c(16, 24, 32, 48, 64, 96, 128)
    fixed <- match.arg(as.character(fixed),
                       as.character(valid_sizes))
  }
  valid_ratios <- c("square", "1by1", "5by4", "4by3", "3by2", "5by3", "16by9",
                    "2by1", "3by1", "4by5", "3by4", "2by3", "3by5", "9by16", "1by2",
                    "1by3")
  if (!is.null(ratio)) {
    ratio <- match.arg(ratio, valid_ratios)
  }
  if (is.null(ratio) && is.null(fixed)) {
    stop("either \"fixed\" or \"ratio\" must be non NULL",
         domain = NA)
  } else if (!is.null(ratio) && !is.null(fixed)) {
    stop("either \"fixed\" or \"ratio\" must be defined but not both",
         domain = NA)
  }
  class <- "image"
  if (!is.null(fixed)) {
    class <- add_class(class, make_class(paste0(fixed, "x", fixed)))
  } else if (!is.null(ratio)) {
    class <- add_class(class, make_class(ratio))
  }
  img <- htmltools::tags$img(src = src, alt = alt,
                             class = if(rounded) make_class("rounded"),
                             ...)
  res <- htmltools::tags$figure(class = class, img)
  htmltools::tagAppendAttributes(res,
                                 class = if(use_fullwidth) make_class("fullwidth"))
}
