#' Colors and Font Used in Bulma
#'
#' A dataset containing all the colors and the imported Google font in the bulma
#' stylesheet.
#'
#' @format A data frame with 70 rows and 4 variables:
#' \describe{
#'   \item{theme}{character, the name of the theme (here this equals always
#'      \sQuote{bulma} and is included to be inline with
#'      \code{\link{bulmaswatch_config}}.)}
#'   \item{group}{character, either \sQuote{color} or \sQuote{font} describing whether the
#'   variable describes a color or the font.}
#'   \item{variable}{character, the name of color or \sQuote{name} or \sQuote{id} for
#'   fonts describing the name or the id.}
#'   \item{value}{character, the hex color code or the name of the font used.}
#' }
#' @seealso \code{\link{bulmaswatch_config}}
#' @source \url{https://bulma.io/documentation/customize/variables/#derived-variables}
"bulma_config"

#' Colors and Font Used in the Bulmaswatch Themes
#'
#' A dataset containing all the colors and the imported Google font in the bulmaswatch
#' themes.
#'
#' @format A data frame with 1476 rows and 4 variables:
#' \describe{
#'   \item{theme}{character, the name of the theme.}
#'   \item{group}{character, either \sQuote{color} or \sQuote{font} describing whether the
#'   variable describes a color or the font.}
#'   \item{variable}{character, the name of color or \sQuote{name} or \sQuote{id} for
#'   fonts describing the name or the id.}
#'   \item{value}{character, the hex color code or the name of the font used.}
#' }
#' @seealso \code{\link{bulma_config}}
#' @source \url{https://jenil.github.io/bulmaswatch/}
"bulmaswatch_config"
