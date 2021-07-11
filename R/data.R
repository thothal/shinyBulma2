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
#'   \item{is_color_map_key}{\[`logical(1)`\], is the particular color a key in the SASS
#'   `$colors` map? These colors are used in `is-{color}` classes, e.g. `is-primary` or
#'   `is-black`.}
#'   \item{color_class}{\[`character(1)`\], either `NA` if the variable is not a color or
#'   there is no color helper class to use this color. Otherwise it contains a comma
#'   separated string with elements \sQuote{text} if it can be used via
#'   `has-text-{color}` and \sQuote{background} it it can be used via
#'   `has-background-{color}`.}
#' }
#' @name bulma_config
#' @seealso [bulmaswatch_config]
#' @docType data
#' @export
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
#'   \item{is_color_map_key}{\[`logical(1)`\], is the particular color a key in the SASS
#'   `$colors` map? These colors are used in `is-{color}` classes, e.g. `is-primary` or
#'   `is-black`.}
#'   \item{color_class}{\[`character(1)`\], either `NA` if the variable is not a color or
#'   there is no color helper class to use this color. Otherwise it contains a comma
#'   separated string with elements \sQuote{text} if it can be used via
#'   `has-text-{color}` and \sQuote{background} it it can be used via
#'   `has-background-{color}`.}
#' }
#' @name bulmaswatch_config
#' @seealso [bulma_config]
#' @docType data
#' @export
#' @source <https://jenil.github.io/bulmaswatch/>
"bulmaswatch_config"
