#' Title
#'
#' @param title,subtitle,body \[`shiny.tag` or `listish`; \sQuote{NULL}\]\cr
#'        The title and the subtitle of the bulma hero banner. Either one specifies
#'        `title` and `subtitle` to display a title and subtitle on the hero banner,
#'        or one uses `body` to freely style the content. If, for instance, you want to
#'        use a different size for the title, you can also use
#'        `body = bulma_title("Title", size = 1)`.
#' @param header,footer \[`shiny.tag` or `listish`; \sQuote{NULL}\]\cr
#'        The header and footer of the bulma hero banner. Can be used wit
#'        `title`/`subtitle`  and `body`.
#' @param color \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma color name.
#' @param size \[`character(1)`: \sQuote{NULL}\]\cr
#'        The size of the hero banner. Valid sizes are
#'        `small`, `medium`, `large`, `halfheight` and `fullheight`.
#' @param with_navbar \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, the hero banner will be made smaller to allow space for
#'        the navabr.
#' @param container \[`function`\]\cr
#'        The container to be used for the bulma hero banner.
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Further arguments passed down to `container`.
#'
#' @seealso [Bulma Hero Banner](https://bulma.io/documentation/layout/hero/)
#'
#' @return A bulma hero banner.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   capitalize <- function(x) paste0(toupper(substr(x, 1, 1)),
#'                                    substr(x, 2, nchar(x)))
#'
#'   ui <- bulma_page(
#'     bulma_block(
#'       bulma_title("Colors"),
#'       lapply(bulma_config[bulma_config$is_color_map_key, "variable"],
#'              function(color) bulma_block(
#'                bulma_hero(paste(capitalize(color), "hero"),
#'                           paste(capitalize(color), "subtitle"),
#'                           color = color)
#'              )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Sizes"),
#'       Map(function(size, color) bulma_block(
#'         bulma_hero(
#'           paste(capitalize(size), "hero"),
#'           paste(capitalize(size), "subtitle"),
#'           size = size,
#'           color = color
#'         )
#'       ),
#'       c("small", "medium", "large", "halfheight", "fullheight"),
#'       c("primary", "link", "info", "success", "danger"))
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#'
#'   navbar <- tags$nav(
#'     class = "navbar",
#'     bulma_container(
#'       div(class = "navbar-menu",
#'           div(class = "navbar-start",
#'               tags$a(class = "navbar-item", "Home"),
#'               tags$a(class = "navbar-item", "Documentation")
#'           ),
#'           div(class = "navbar-end",
#'               div(class = "navbar-item",
#'                   bulma_buttons(
#'                     bulma_button("Github", color = "dark", container = tags$a),
#'                     bulma_button("Download", color = "link", container = tags$a)
#'                   )
#'               )
#'
#'           )
#'       )
#'     )
#'   )
#'
#'   ui <- bulma_page(
#'     navbar,
#'     bulma_hero(
#'       "Fullheight hero with navbar",
#'       color = "link",
#'       with_navbar = TRUE
#'     )
#'   )
#'
#'   shinyApp(ui, server)
#' }
bulma_hero <- function(title = NULL, subtitle = NULL,
                       body = NULL, header = NULL, footer = NULL,
                       color = NULL, size = NULL,
                       with_navbar = FALSE,
                       container = htmltools::tags$section, ...) {
  header_tag <- htmltools::div(
    header,
    class = "hero-head"
  )
  footer_tag <- htmltools::div(
    footer,
    class = "hero-foot"
  )
  if (with_navbar && (size %||% "") != "fullheight") {
    warning("'with_navbar' makes only sense with fullheight sized hero banners",
            domain = NA)
    with_navbar <- FALSE
  }
  color_class <- validate_bulma_color(color, "background", must_be_key = TRUE)
  size_class <- validate_bulma_size(size,
                                    disallow = "normal",
                                    additional = c("halfheight", "fullheight"))
  if (with_navbar) {
    size_class <- paste0(size_class, "-with-navbar")
  }
  if (!is.null(body)) {
    if (!is.null(title) || !is.null(subtitle)) {
      warning("both 'title' / 'subtitle' and 'body' are not NULL - ",
              "ignoring 'title' / 'subtitle'", domain = NA)
    }
    body <- htmltools::div(
      body,
      class = "hero-body")
  } else {
    if (is.null(title) && is.null(subtitle)) {
      warning("'body', 'title' and 'subtitle' are NULL - will result ",
              "in an empty element", domain = NA)
    }
    title_tag <- bulma_title(title, container = htmltools::p)
    subtitle_tag <- bulma_subtitle(subtitle, container = htmltools::p)
    if (!is.null(size) && size %in% c("halfheight", "fullheight")) {
      ## wrap in div b/c in hh/fh divs are flex and thus floating
      body <- htmltools::div(
        htmltools::div(
          if (!is.null(title)) title_tag,
          if (!is.null(subtitle)) subtitle_tag
        ),
        class = "hero-body"
      )
    } else {
      body <- htmltools::div(
        if (!is.null(title)) title_tag,
        if (!is.null(subtitle)) subtitle_tag,
        class = "hero-body"
      )
    }
  }

  container(if (!is.null(header)) header_tag,
            body,
            if (!is.null(footer)) footer_tag,
            class = add_class("hero", c(color_class, size_class)),
            ...)

}
