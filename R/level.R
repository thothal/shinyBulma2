#' Create bulma Level & bulma Level Item
#'
#' @param left,right,center \[`shiny.tag` or `listish`; \sQuote{NULL}\]\cr
#'        These elements are placed in the left or right part of the
#'        level. Alternatively (but not simultaneously) you can set
#'        `center`, in which case it will be placed horizontally
#'        centered. Typically, you want to pass a [htmltools::tagList()]
#'        to these arguments. The function will then loop through all
#'        elements in the list and place them in the right place.
#' @param horizontal_on_mobile \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, the level is displayed horizonatlly on
#'        mobile devices and not vertically as per default.
#' @param container \[`function`\]\cr
#'        The container to be used for the bulma level or bulma level item.
#' @param heading,title,content \[shiny.tag` or `listish`; \sQuote{NULL}\]\cr
#'        The heading, title or free content of the level item.
#'        Either one specifies heading and title to display a
#'        simple heading with a value, or one uses `content` to
#'        freely style the content.
#' @param centered \[`logical(1)`: \sQuote{TRUE}\]\cr
#'        If \sQuote{TRUE}, the content (either given by `heading/title` or
#'        `content`) is centered using class `has-text-centered`.
#' @param heading_on_top \[`logical(1)`: \sQuote{TRUE}\]\cr
#'        If \sQuote{TRUE}, the heading is placed on top of the title,
#'        otherwise reversed.
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Further arguments passed down to `container`.
#'
#' @note
#' Elements within a bulma level, need to have a class `level-item`. The function adds
#' this class, if the respective elment does not have it. This is done by calling
#' `bulma_level_item`, that is, the respective element is wrapped in another `<div>`
#' element. If you do not want this behaviour, make sure that the element has the
#' `level-item` class.
#'
#' @seealso [Bulma Level](https://bulma.io/documentation/layout/level/)
#'
#' @return A bulma level and a bulma level item respectively.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ## replace as soon as form elements are available
#'   right <- tagList(
#'     tags$strong("All"),
#'     tags$a("Published"),
#'     tags$a("Drafts"),
#'     tags$a("Deleted"),
#'     bulma_button("New", color = "success", container = tags$a)
#'   )
#'
#'   left <- tagList(
#'     bulma_subtitle(tags$strong(123)),
#'     tags$div(
#'       tags$p(
#'         tags$input(class = "input", type = "text", placeholder = "Find a post"),
#'         class = "control"
#'       ),
#'       tags$p(
#'         bulma_button("Search")
#'       ),
#'       class = "field has-addons"
#'     )
#'   )
#'
#'   center <- tagList(
#'     bulma_level_item("Tweets", "3,456"),
#'     bulma_level_item("Following", 123),
#'     bulma_level_item("Followers", "456k"),
#'     bulma_level_item("Likes", 789)
#'   )
#'
#'   ui <- bulma_page(
#'     bulma_block(
#'       bulma_title("Basic Level"),
#'       bulma_level(left, right)
#'     ),
#'     bulma_block(
#'       bulma_title("Centered Level"),
#'       bulma_block(
#'         bulma_level(
#'           center = center
#'         )
#'       ),
#'       bulma_block(
#'         bulma_level(
#'           center = tagList(
#'             bulma_level_item(content = tags$a("Home", class = "link is-info")),
#'             bulma_level_item(content = tags$a("Menu", class = "link is-info")),
#'             bulma_level_item(content = tags$img(
#'               alt = "",
#'               style = "height: 30px",
#'               src = "https://bulma.io/images/bulma-type.png")),
#'             bulma_level_item(content = tags$a("Reservations", class = "link is-info")),
#'             bulma_level_item(content = tags$a("contact", class = "link is-info"))
#'           )
#'         )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Mobile"),
#'       bulma_subtitle("(Resize to see effect)"),
#'       bulma_level(center = center, horizontal_on_mobile = TRUE)
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
bulma_level <- function(left = NULL, right = NULL, center = NULL,
                        horizontal_on_mobile = FALSE,
                        container = htmltools::tags$nav, ...) {
  if (!is.null(center) && !(is.null(left) && is.null(right))) {
    warning("both 'center' and 'left'/'right' are not NULL - ignoring 'center'",
            domain = NA)
    center <- NULL
  }

  make_item <- function(tag) {
    class <- htmltools::tagGetAttribute(tag, "class")
    if (is.null(class) || !grepl("\\blevel-item\\b", class)) {
      bulma_level_item(content = tag)
    } else {
      tag
    }
  }
  class <- "level"
  if (horizontal_on_mobile) {
    class <- add_class(class, make_class("mobile"))
  }
  if (!is.null(center)) {
    ## if center is just a single element, wrap it in a tagList to make sure the
    ## loop works as expected
    if (!inherits(center, "list")) {
      center <- htmltools::tagList(center)
    }
    center <- do.call(htmltools::tagList,
                      lapply(center, make_item))
    container(center, ..., class = class)
  } else {
    if (!is.null(left)) {
      if (!inherits(left, "list")) {
        left <- htmltools::tagList(left)
      }
      left <- htmltools::div(lapply(left, make_item),
                             class = "level-left")
    }
    if (!is.null(right)) {
      if (!inherits(right, "list")) {
        right <- htmltools::tagList(right)
      }
      right <- htmltools::div(lapply(right, make_item),
                              class = "level-right")
    }
    container(left, right, ..., class = class)
  }
}

#' @rdname bulma_level
#' @export
bulma_level_item <- function(heading = NULL, title = NULL, content = NULL,
                             centered = TRUE, heading_on_top = TRUE,
                             container = htmltools::div, ...) {
  class <- "level-item"
  if (centered) {
    class <- add_class(class, make_class("text-centered", prefix = "has"))
  }
  if (!is.null(content)) {
    if (!is.null(heading) || !is.null(title)) {
      warning("both 'heading' / 'title' and 'content' are not NULL - ",
              "ignoring 'heading' / 'title'",
              domain = NA)
    }
    container(content, class = class, ...)
  } else {
    if (is.null(heading) && is.null(title)) {
      warning("'heading', 'title' and 'content' are NULL - will result ",
              "in an empty element", domain = NA)
    }
    heading <- bulma_heading(heading, container = htmltools::p)
    title <- bulma_title(title, container = htmltools::p)
    if (heading_on_top) {
      container(htmltools::div(heading, title), class = class, ...)
    } else {
      container(htmltools::div(title, heading), class = class, ...)
    }
  }
}
