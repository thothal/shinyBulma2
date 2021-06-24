#' Colors and Font Used in Bulma
#'
#' A dataset containing all the colors and the imported Google font in the bulma
#' stylesheet.
#'
#' @format A data frame with `r nrow(bulma_config)` rows and `r ncol(bulma_config)`
#' variables:
#' \describe{
#'   \item{theme}{\[`character(1)`\], the name of the theme (here this equals always
#'      \sQuote{bulma} and is included to be inline with
#'      [bulmaswatch_config].)}
#'   \item{group}{\[`character(1)`\], either \sQuote{color} or \sQuote{font} describing
#'      whether the variable describes a color or the font.}
#'   \item{variable}{\[`character(1)`\], the name of color or \sQuote{name} or
#'      \sQuote{id} for fonts describing the name or the id.}
#'   \item{value}{\[`character(1)`\], the hex color code or the name of the font used.}
#' }
#' @name bulma_config
#' @seealso [bulmaswatch_config]
#' @docType data
#' @source <https://bulma.io/documentation/customize/variables/#derived-variables>
"bulma_config"

#' Colors and Font Used in the Bulmaswatch Themes
#'
#' A dataset containing all the colors and the imported Google font in the bulmaswatch
#' themes.
#'
#' @format A data frame with `r nrow(bulmaswatch_config)` rows and
#' `r ncol(bulmaswatch_config)` variables:
#' \describe{
#'   \item{theme}{\[`character(1)`\], the name of the theme.}
#'   \item{group}{\[`character(1)`\], either \sQuote{color} or \sQuote{font} describing whether the
#'   variable describes a color or the font.}
#'   \item{variable}{\[`character(1)`\], the name of color or \sQuote{name} or \sQuote{id} for
#'   fonts describing the name or the id.}
#'   \item{value}{\[`character(1)`\], the hex color code or the name of the font used.}
#' }
#' @name bulmaswatch_config
#' @seealso [bulma_config]
#' @docType data
#' @source <https://jenil.github.io/bulmaswatch/>
"bulmaswatch_config"
