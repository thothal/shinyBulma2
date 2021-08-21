#' Create a bulma Container
#'
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the column.
#' @param fluid \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, the `is-fluid` class is added to teh container, resulting in
#'        a container without a maximum width.
#' @param fullwidth_breakpoint \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma media breakpoint until which the container is shown in full width.
#'        Must be either `widescreen` or `fullhd`.
#' @param narrow_down_to \[`character(1)`: \sQuote{NULL}\]\cr
#'        A valid bulma breakpoint to whose maximum size the container is narrowed down
#'        to. Must be either `desktop` or `widescreen`.
#'
#' @seealso [Bulma Container](https://bulma.io/documentation/layout/container/)
#'
#' @return A bulma `container`.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- bulma_page(
#'     bulma_block(
#'       bulma_title("Simple Container"),
#'       bulma_container(
#'         bulma_notification(
#'           tagList(
#'             "This is container is ",
#'             strong("centered"),
#'             " on desktop and larger viewports"),
#'           color = "primary",
#'           add_delete = FALSE)
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Widescreen or FullHD only"),
#'       bulma_block(
#'         bulma_container(
#'           bulma_notification(
#'             tagList(
#'               "This container is ",
#'               strong("fullwidth"),
#'               " ",
#'               tags$em("until"),
#'               " the ",
#'               tags$code("$widescreen"),
#'               " breakpoint."
#'             ),
#'             color = "primary",
#'             add_delete = FALSE
#'           ),
#'           fullwidth_breakpoint = "widescreen"
#'         )
#'       ),
#'       bulma_block(
#'         bulma_container(
#'           bulma_notification(
#'             tagList(
#'               "This container is ",
#'               strong("fullwidth"),
#'               " ",
#'               tags$em("until"),
#'               " the ",
#'               tags$code("$fullhd"),
#'               " breakpoint."
#'             ),
#'             color = "primary",
#'             add_delete = FALSE
#'           ),
#'           fullwidth_breakpoint = "fullhd"
#'         )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Desktop and Widescreen maximum widths"),
#'       bulma_block(
#'         bulma_container(
#'           bulma_notification(
#'             tagList(
#'               "This container has a ",
#'               tags$code("max-width"),
#'               " of ",
#'               tags$code("$desktop - $container-offset"),
#'               " on widescreen and fullhd."
#'             ),
#'             color = "primary",
#'             add_delete = FALSE
#'           ),
#'           narrow_down_to = "desktop"
#'         )
#'       ),
#'       bulma_block(
#'         bulma_container(
#'           bulma_notification(
#'             tagList(
#'               "This container has a ",
#'               tags$code("max-width"),
#'               " of ",
#'               tags$code("$widescreen - $container-offset"),
#'               " on fullhd."
#'             ),
#'             color = "primary",
#'             add_delete = FALSE
#'           ),
#'           narrow_down_to = "widescreen"
#'         )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Fluid container"),
#'       bulma_container(
#'         bulma_notification(
#'           tagList(
#'             "This is container is ",
#'             strong("fluid"),
#'             "; it will have a 32px gap on either side, on any viewport sie."),
#'           color = "primary",
#'           add_delete = FALSE),
#'         fluid = TRUE
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
bulma_container <- function(...,
                            fluid = FALSE,
                            fullwidth_breakpoint = NULL,
                            narrow_down_to = NULL) {
  class <- "container"
  if (fluid && (!is.null(fullwidth_breakpoint) || !is.null(narrow_down_to))) {
    warning("'fluid' is TRUE - non NULL 'fullwidth_breakpoint' or 'narrow_down_to' ",
            "will be ignored", domain = NA)
    fullwidth_breakpoint <- narrow_down_to <- NULL
  }
  if (!is.null(fullwidth_breakpoint) && !is.null(narrow_down_to)) {
    warning("both 'fullwidth_breakoint' and 'narrow_down_to' are not NULL - ",
            "only the former will be considered", domain = NA)
    narrow_to <- NULL
  }
  if (!is.null(fullwidth_breakpoint)) {
    valid_breakpoints <- c("widescreen", "fullhd")
    if (fullwidth_breakpoint %in% valid_breakpoints) {
      class <- add_class(class, make_class(fullwidth_breakpoint))
    } else {
      warning("\"", fullwidth_breakpoint,
              "\" is not supported by bulma container as a fullwidth breakpoint",
              domain = NA)
    }
  }
  if (!is.null(narrow_down_to)) {
    valid_breakpoints <- c("desktop", "widescreen")
    if (narrow_down_to %in% valid_breakpoints) {
      class <- add_class(class, make_class(paste0("max-", narrow_down_to)))
    } else {
      warning("\"", narrow_down_to,
              "\" is not supported by bulma container as a narrow-down-to breakpoint",
              domain = NA)
    }
  }
  if (fluid) {
    class <- add_class(class, make_class("fluid"))
  }
  htmltools::div(..., class = class)
}
