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
#' @param size \[`character(1)`; \sQuote{normal}\]\cr
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
#'       h1("Icon with Text", class = "title"),
#'       bulma_icon("home", "Home")),
#'     bulma_block(
#'       h1("Several Icons with Text", class = "title"),
#'       bulma_icon(c("train", "arrow-right", "arrow-right",
#'                    "arrow-right", "flag-checkered"),
#'                  c("Paris", "Budapest", "Bucharest", "Istanbul", NA))),
#'     bulma_block(
#'       h1("Inline Icon", class = "title"),
#'       p("An invitation to", bulma_icon("utensils", "dinner"), "was soon afterwards ",
#'         "dispatched; and already had Mrs. Bennet planned the courses that were to do",
#'         "credit to her housekeeping, when an answer arrived which deferred it all.",
#'         "Mr. Bingley was obliged to be in", bulma_icon("city", "town"),
#'         "the following day, and, consequently, unable to accept the honour of their",
#'         bulma_icon("envelope-open-text", "invitation"), ", etc.")
#'     ),
#'     bulma_block(
#'       h1("Icon With Text, Color in a Different Container", class = "title"),
#'       bulma_icon("info-circle", "Information", color = "link", container = div),
#'       bulma_block("Your package will be delivered on", strong("Tuesday, 08:00")),
#'       bulma_icon("check-square", "Success", color = "success", container = div),
#'       bulma_block("Your image has been successfully uploaded.")
#'     ),
#'     bulma_block(
#'       id = "sizes",
#'       h1("Icons with Sizing", class = "title"),
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
                       size = c("normal", "small", "medium", "large"),
                       container = htmltools::span,
                       class = NULL, lib = "font-awesome", ...) {
  color <- validate_bulma_color(color)
  container <- match.fun(container)
  size <- match.arg(size)
  if (is.null(text)) {
    res <- htmltools::span(
      class = "icon",
      shiny::icon(name, class, lib, ...)
    )
    res <- htmltools::tagAppendAttributes(res, class = make_class(size))

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
      icon <- htmltools::tagAppendAttributes(icon, class = make_class(size))
      text <- if (!is.na(txt)) htmltools::span(txt)
      htmltools::tagList(icon, text)
    }, name, text, USE.NAMES = FALSE)
    res <- do.call(container, c(class = "icon-text",
                                unlist(icons, recursive = FALSE)))
  }
  htmltools::tagAppendAttributes(res, class = color)
}

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

