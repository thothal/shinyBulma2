validate_column_or_offset  <- function(x, type = c("size", "offset")) {
  type <- match.arg(type)
  if (is.null(x)) {
    return(x)
  }
  if (length(x) > 1 || (!is.character(x) && !is.numeric(x)) || is.na(x)) {
    stop("column ", type,
         " must be non 'NA' and a single-element numeric or character vector",
         domain = NA)
  }

  if (is.numeric(x)) {
    if (x < 1 || x > 12 || !is_integer(x)) {
      stop("column ", type, " must be an integer between 1 and 12",
           domain = NA)
    }
    res <- x
  } else if (is.character(x)) {
    standard_qualifiers <- c("full",
                             "four-fifth",
                             "three-quarters",
                             "two-thirds",
                             "three-fifth",
                             "half",
                             "two-fifth",
                             "one-third",
                             "one-quarter",
                             "one-fifth")
    aliases <- c("1/1", "4/5", "3/4", "2/3", "3/5", "1/2", "2/5", "1/3", "1/4", "1/5")
    valid_qualifiers <- c(standard_qualifiers, aliases)
    lkp <- stats::setNames(rep(standard_qualifiers, 2), valid_qualifiers)
    if (type == "size") {
      narrow_qualifiers <- c("narrow", paste0("narrow-",
                                              c("mobile", "tablet", "touch",
                                                "desktop", "widescreen", "fullhd")))
      valid_qualifiers <- c(valid_qualifiers, narrow_qualifiers)
      lkp <- c(lkp, setNames(narrow_qualifiers, narrow_qualifiers))
    } else if (type == "offset") {
      valid_qualifiers <- setdiff(valid_qualifiers, c("1/1", "full"))
    }
    if (!x %in% valid_qualifiers) {
      stop("\"", x, "\" is not a valid ", type, " specifier - valid specifiers are:\n",
           paste0("'", valid_qualifiers, "'", collapse = ", "),
           domain = NA)
    }
    res <- lkp[x]
  }
  if (type == "size") {
    paste0("is-", res)
  } else {
    paste0("is-offset-", res)
  }
}

#' Validate Proper Width or Offset Definition for a Bulma Column
#'
#' Checks that the argument is a valid column width or offset specifier. Valid values are
#' integers between 1 and 12, or strings like `two-fifth` or `full`. For the size `narrow`
#' is also a valid option. Additionally, to the original specifiers
#' (cf. [column sizes](https://bulma.io/documentation/columns/sizes/)), you can also
#' abbreviate the number-ish strings, e.g. `"2/5"` instead of `"two-fifth"`.
#'
#' @param x \[`character(1)` or `integer(1)`\]\cr
#'        The size or offset to validate.
#'
#' @return The proper bulma size or offset class, if possible. Otherwise an error.
#' @export
#'
#' @examples
#' validate_bulma_column_size(1)
#' validate_bulma_column_size("one-third")
#' validate_bulma_column_size("1/1")
#' validate_bulma_column_size("narrow-widescreen")
#'
#' validate_bulma_column_offset(6)
#' validate_bulma_column_offset("two-thirds")
#' validate_bulma_column_offset("4/5")
#'
#' \dontrun{
#' ## these will each raise an error
#' validate_bulma_column_size(13)
#' validate_bulma_column_size(1:2)
#' validate_bulma_column_size(11.3)
#' validate_bulma_column_size("one-sixth")
#' validate_bulma_column_size("one.third")
#'
#' validate_bulma_column_offset("narrow")
#' validate_bulma_column_offset("full")
#' validate_bulma_column_offset("1/1")
#' }
validate_bulma_column_size <- function(x) {
  validate_column_or_offset(x, "size")
}

#' @rdname validate_bulma_column_size
#' @export
validate_bulma_column_offset <- function(x) {
  validate_column_or_offset(x, "offset")
}

bulma_columns <- function(...) {
  htmltools::tags$div(class = "columns", ...)
}

bulma_column <- function(..., width = NULL, offset = NULL) {
  width <- validate_bulma_column_size(width)
}
