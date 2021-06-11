#' Get List of Installed Bulma Themes
#'
#' This function looks into the lib/ folder located in the package's base folder and
#' returns all found themes in the corresponding subfolder. Themes are compiled from
#' bulmaswatch.
#'
#' @seealso \url{https://jenil.github.io/bulmaswatch/}
#'
#' @return character vector, all installed bulma themes
#' @export
#'
#' @examples
#' get_bulma_themes()
get_bulma_themes <- function() {
  me <- get_package_name()
  themes_folder <- system.file("lib", "bulmaswatch", package = me)
  themes <- list.files(themes_folder, "\\.css$")
  sub("(\\.min)?\\.css", "", themes)
}
