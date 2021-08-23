#' Create a bulma Media Object
#'
#' @param left,content,right \[`shiny.tag` or `listish`: \sQuote{NULL}\]\cr
#'        The content to be put in the left, middle and reight part of the media object.
#'        `content` support nesting of media objects up to 3 levels. All elements can
#'        contain any other bulma elemnts.
#' @param container \[`function`\]\cr
#'        The container to be used for the bulma level or bulma level item.
#' @param ... \[`html tags` or `html attributes`\]\cr
#'        Elements to include within the container.
#'
#' @seealso [Bulma Media Object](https://bulma.io/documentation/layout/media-object/)
#'
#' @return A bulma media object.
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- bulma_page(
#'     bulma_block(
#'       bulma_title("Media Object"),
#'       bulma_block(
#'         bulma_media(
#'           bulma_image("https://bulma.io/images/placeholders/128x128.png",
#'                       fixed = 64, container = p),
#'           tagList(
#'             bulma_content(
#'               tags$p(
#'                 tags$strong("John Smith"),
#'                 tags$small("@johnsmith"),
#'                 tags$small("31m"),
#'                 tags$br(),
#'                 paste("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin",
#'                       "ornare magna eros, eu pellentesque tortor vestibulum ut.",
#'                       "Maecenas non massa sem. Etiam finibus odio quis feugiat",
#'                       "facilisis.")
#'               )
#'             ),
#'             bulma_level(
#'               tagList(
#'                 bulma_level_item(content = bulma_icon("reply", size = "small"),
#'                                  centered = FALSE, container = tags$a),
#'                 bulma_level_item(content = bulma_icon("retweet", size = "small"),
#'                                  centered = FALSE, container = tags$a),
#'                 bulma_level_item(content = bulma_icon("heart", size = "small"),
#'                                  centered = FALSE, container = tags$a)
#'               ),
#'               horizontal_on_mobile = TRUE
#'             )
#'           ),
#'           bulma_delete()
#'         )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_media(
#'         bulma_image("https://bulma.io/images/placeholders/128x128.png",
#'                     fixed = 64, container = p),
#'         tagList(
#'           div(
#'             tags$p(
#'               tags$textarea(class = "textarea", placeholder = "Add a comment..."),
#'               class = "control"
#'             ),
#'             class = "field"
#'           ),
#'           bulma_level(
#'             bulma_button("Submit", color = "info"),
#'             tags$label(
#'               tags$input(type = "checkbox", "Press enter to submit"),
#'               class = "checkbox"
#'             )
#'           )
#'
#'         )
#'       )
#'     ),
#'     bulma_block(
#'       bulma_title("Nesting"),
#'       bulma_media(
#'         bulma_image("https://bulma.io/images/placeholders/128x128.png",
#'                     fixed = 64, container = p),
#'         tagList(
#'           bulma_content(
#'             tags$p(
#'               tags$strong("Barbara Middleton"),
#'               tags$br(),
#'               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis porta",
#'               " eros lacus, nec ultricies elit blandit non. Suspendisse pellentesque",
#'               " mauris sit amet dolor blandit rutrum. Nunc in tempus turpis.",
#'               tags$br(),
#'               tags$small(
#'                 tags$a("Like"), " · ", tags$a("Reply"), " · 3 hrs"
#'               )
#'             )
#'           ),
#'           bulma_media(
#'             bulma_image("https://bulma.io/images/placeholders/96x96.png",
#'                         fixed = 48, container = p),
#'             tagList(
#'               bulma_content(
#'                 tags$p(
#'                   tags$strong("Sean Brown"),
#'                   tags$br(),
#'                   "Donec sollicitudin urna eget eros malesuada sagittis. Pellentesque",
#'                   "habitant morbi tristique senectus et netus et malesuada fames ac",
#'                   "turpis egestas. Aliquam blandit nisl a nulla sagittis, a lobortis",
#'                   "leo feugiat.",
#'                   tags$br(),
#'                   tags$small(
#'                     tags$a("Like"), " · ", tags$a("Reply"), " · 2 hrs"
#'                   )
#'                 )
#'               ),
#'               bulma_media(left = NULL,
#'                           content = NULL,
#'                           right = NULL,
#'                           container = tags$article,
#'                           "Vivamus quis semper metus, non tincidunt dolor. Vivamus in",
#'                           "mi eu lorem cursus ullamcorper sit amet nec massa."
#'               ),
#'               bulma_media(left = NULL,
#'                           content = NULL,
#'                           right = NULL,
#'                           container = tags$article,
#'                           "Morbi vitae diam et purus tincidunt porttitor vel vitae",
#'                           "augue. Praesent malesuada metus sed pharetra euismod. Cras",
#'                           "tellus odio, tincidunt iaculis diam non, porta aliquet",
#'                           "tortor."
#'               )
#'             )
#'
#'           ),
#'           bulma_media(
#'             bulma_image("https://bulma.io/images/placeholders/96x96.png",
#'                         fixed = 48, container = p),
#'             bulma_content(
#'               tags$p(
#'                 tags$strong("Kayli Eunice"),
#'                 tags$br(),
#'                 "Sed convallis scelerisque mauris, non pulvinar nunc mattis vel. ",
#'                 "Maecenas varius felis sit amet magna vestibulum euismod malesuada ",
#'                 "cursus libero. Vestibulum ante ipsum primis in faucibus orci luctus ",
#'                 "et ultrices posuere cubilia Curae; Phasellus lacinia non nisl id",
#'                 "feugiat.",
#'                 tags$br(),
#'                 tags$small(
#'                   tags$a("Like"), " · ", tags$a("Reply"), " · 2 hrs"
#'                 )
#'               )
#'             )
#'           )
#'         )
#'       ),
#'
#'       bulma_media(
#'         bulma_image("https://bulma.io/images/placeholders/128x128.png",
#'                     fixed = 64, container = p),
#'         tagList(
#'           div(
#'             tags$p(
#'               tags$textarea(class = "textarea", placeholder = "Add a comment..."),
#'               class = "control"
#'             ),
#'             class = "field"
#'           ),
#'           div(
#'             tags$p(
#'               bulma_button("Post comment"),
#'               class = "control"
#'             ),
#'             class = "field"
#'           )
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
bulma_media <- function(left = NULL, content = NULL, right = NULL,
                        container = htmltools::tags$article, ...) {
  left <- if (!is.null(left)) htmltools::tags$figure(left, class = "media-left")
  content <- if(!is.null(content)) htmltools::div(content, class = "media-content")
  right <- if(!is.null(right)) htmltools::div(right, class = "media-right")
  container(left, content, right, class = "media", ...)
}
