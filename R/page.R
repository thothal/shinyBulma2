extract_version <- function(fn) {
  first_line <- readLines(fn, n = 1L, warn = FALSE)
  sub(".*?v(\\S+)\\s.*", "\\1", first_line)
}

bulma_lib <- function(theme = NULL) {
  if (is.null(theme)) {
    fn <- system.file("lib", "bulma", "bulma.min.css",
                      package = get_package_name(),
                      mustWork = TRUE)
  } else { # nocov start
    fn <- system.file("lib", "bulmaswatch", paste0(theme, ".min.css"),
                      package = get_package_name(),
                      mustWork = TRUE)
  } # nocov end
  version <- extract_version(fn)
  shiny::addResourcePath("fonts",
                         system.file("fonts",
                                           package = get_package_name(),
                                           mustWork = TRUE))
  htmltools::htmlDependency(if (is.null(theme)) "bulma" else "bulmaswatch",
                            version,
                            c(file = dirname(fn)),
                            stylesheet = basename(fn),
                            meta = list(viewport = "width=device-width, initial-scale=1"))
}

#' Create a Bulma Page
#'
#' This function creates a bulma page and allows themes from bulmsawatch to be used.
#'
#' @param ... Elements to include within the page
#' @param title The browser window title (defaults to the host URL of the page).
#' @param theme If \code{NULL} the original bulma theme is used. Otherwise, it must be a
#'              valid bulma theme name from
#'              \href{https://jenil.github.io/bulmaswatch/}{bulmaswatch}.
#' @param lang ISO 639-1 language code for the HTML page, such as \dQuote{en} or
#'             \dQuote{de}. This will be used as the lang in the \code{<html>} tag, as in
#'             \code{<html lang="en">}. The default (\code{NULL}) results in an empty
#'             string.
#'
#' @return A UI definition that can be passed to \code{\link[shiny]{shinyApp}}.
#' @export
#'
#' @seealso \code{\link{get_bulma_themes}} for getting a list of all installed themes,
#'          \url{https://bulma.io}
#'
#' @examples
#'
#' ## Only run examples in interactive R sessions
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'
#' ui <- bulma_page(
#'         tags$section(class = "section",
#'                      div(class = "container",
#'                          bulma_title("Hello World!"),
#'                          tags$p(class = "subtitle", "My first website with",
#'                                 tags$strong("Bulma", .noWS = "after"), "!")))
#'       )
#' server <- function(input, output) {
#' }
#'
#' shiny::shinyApp(ui, server)
#' }
bulma_page <- function(..., title = NULL, theme = NULL, lang = NULL) {
  if (!(is.null(theme) || theme %in% get_bulma_themes())) {
    msg <- sprintf("theme '%s' is not installed", theme)
    stop(msg, domain = NA)
  }
  # store used in theme as a global, needed for determining the used colors for instance
  bulma_global$theme <- theme
  args <- list(if (!is.null(title)) htmltools::tags$head(htmltools::tags$title(title)),
               list(...),
               bulma_lib(theme))
  ui <- do.call(htmltools::tagList, args)
  attr(ui, "lang") <- lang
  ui
}
