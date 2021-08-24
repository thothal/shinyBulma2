#' Validate Proper Width, Offset or Gap Definition for a Bulma Column Layout
#'
#' `validate_bulma_unit` checks that the argument is a valid column
#' width, offset or gap specifier, respectively.
#'
#' `validate_bulma_{column_size|offset|gap}` are convenience wrappers which set `type` to the
#' respective value.
#'
#' `get_bulma_media_breakpoints` returns all valid media breakpoints.
#'
#' @param x \[`character(n)` or `integer(n)`\]\cr
#'        The sizes, offsets or gaps to validate.
#' @param type \[`character(1)`: \sQuote{size}\]\cr
#'        The type of unit we want to validate. Must be one of `size`, `offset` or `gap`.
#'
#' @details
#' Valid values for sizes and offsets are integers between 1 and 12, or strings like
#' `two-fifth` or `full`. For the size `narrow` is also a valid option. Additionally, to
#' the original specifiers
#' (cf. [column sizes](https://bulma.io/documentation/columns/sizes/)), you can also
#' abbreviate the number-ish strings, e.g. `"2/5"` instead of `"two-fifth"`.
#'
#' Gap values are integers between 0 and 8 or the string \dQuote{gapless}.
#'
#' All values can additionally be suffixed by valid media breakpoint qualifiers
#' in which case these sizes are only valid on screens which meet at least the size given
#' by the breakpoints.
#'
#' Multiple values can be passed. This makes sense, if we want to define different values
#' for different media breakpoints. Warnings are issued, either if
#' duplicated media breakpoints are detected, or if we provide multiple values without
#' media breakpoints.
#'
#' @return The proper bulma column size, offset or gap classes, if possible.
#' Otherwise an error is raised.
#' `get_bulma_media_breakpoints` returns a character vector with all valid breakpoints.
#' @export
#'
#' @examples
#' validate_bulma_column_size(1)
#' validate_bulma_column_size("one-third")
#' validate_bulma_column_size("1/1")
#' validate_bulma_column_size("narrow-tablet")
#' validate_bulma_column_size(c("1-touch", "2-desktop", "3-fullhd"))
#'
#' validate_bulma_offset(6)
#' validate_bulma_offset("two-thirds")
#' validate_bulma_offset("4/5-desktop")
#' validate_bulma_offset(c("two-thirds-touch", "11-desktop", "4/5-fullhd"))
#'
#' validate_bulma_gap("gapless")
#' validate_bulma_gap(1)
#' validate_bulma_gap("2-touch")
#' validate_bulma_gap(c("1-touch", "2-mobile", "3-desktop"))
#'
#' get_bulma_media_breakpoints()
#'
#' \dontrun{
#' ## these will each raise an error
#' validate_bulma_column_size(13)
#' validate_bulma_column_size(11.3)
#' validate_bulma_column_size("one-sixth")
#' validate_bulma_column_size("one.third")
#' validate_bulma_column_size("1-touchscreen")
#' validate_bulma_column_size(c("1-fullhd", "2"))
#' validate_bulma_column_size(c("1-fullhds", "2-fullhds"))
#'
#' validate_bulma_offset("narrow")
#' validate_bulma_offset("full")
#' validate_bulma_offset("1/1")
#' validate_bulma_offset("1/3-4k")
#'
#' validate_bulma_gap(9)
#' validate_bulma_gap("one-third")
#'
#' ## these will raise warnings
#' validate_bulma_column_size(1:3)
#' validate_bulma_offset(c("1-fullhd", "2-fullhd"))
#' }
validate_bulma_unit  <- function(x,
                                 type = c("size", "offset", "gap")) {
  type <- match.arg(type)
  if (is.null(x)) {
    return(x)
  }
  if (any(is.na(x))) {
    stop("column ", type,
         " must not contain any 'NAs'",
         domain = NA)
  }
  if (!(is.character(x)  || is.numeric(x))) {
    stop("column ", type,
         " must be a numeric or character vector",
         domain = NA)
  }
  if (is.numeric(x)) {
    if (type == "gap") {
      if (any(x < 0 | x > 8 | !is_integer(x))) {
        stop("column ", type, " must be an integer between 0 and 8",
             domain = NA)
      }
    } else {
      if (any(x < 1 | x > 12 | !is_integer(x))) {
        stop("column ", type, " must be an integer between 1 and 12",
             domain = NA)
      }
    }
    res <- x
    breakpoints <- character(0)
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
    valid_qualifiers <- c(standard_qualifiers,
                          aliases,
                          "gapless",
                          as.character(0:12) ## add this to allow for e.g. 1-touch
    )
    lkp <- stats::setNames(c(rep(standard_qualifiers, 2), "gapless", as.character(0:12)),
                           valid_qualifiers)

    ## extract any valid media breakpoint
    pattern <- paste0("(", paste(get_bulma_media_breakpoints(), collapse = "|"), ")$")
    matches <- regexpr(pattern, x)
    breakpoints <- regmatches(x, matches)
    ## make sure that there are either 0 breakpoints or as many as elements
    if (length(breakpoints) && length(breakpoints) != length(x)) {
      NOK <- matches == -1
      bad_media <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
      msg <- ngettext(sum(NOK),
                      paste(bad_media, "does not contain a valid",
                            "media breakpoint"),
                      paste(bad_media, "do not contain valid",
                            "media breakpoints"))
      stop("column ", type, " must contain breakpoints either for all elements ",
           "or for none at all:\n", msg, domain = NA)
    }
    if (type == "size") {
      valid_qualifiers <- setdiff(c(valid_qualifiers, "narrow"), c("0", "gapless"))
      lkp <- c(lkp, narrow = "narrow")
    } else if (type == "offset") {
      valid_qualifiers <- setdiff(valid_qualifiers, c("1/1", "full", "0", "gapless"))
    } else if (type == "gap") {
      valid_qualifiers <- intersect(valid_qualifiers, c("gapless", as.character(0:8)))
    }
    ## remove any valid(!) breakpoints
    x <- sub(paste0("-", pattern), "", x)
    NOK <- !x %in% valid_qualifiers
    if (any(NOK)) {
      bad_cols <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
      msg <- ngettext(sum(NOK),
                      paste(bad_cols, "is not a valid", type, "specifier"),
                      paste(bad_cols, "are not valid", type, "specifiers"))
      stop(msg,
           domain = NA)
    }
    res <- lkp[x]
  }
  if (any(duplicated(breakpoints))) {
    warning("duplicated media breakpoints found - this may lead to unwanted results")
  }
  if (!length(breakpoints) && length(x) > 1) {
    warning("multiple sizes without media breakpoints found - this may lead to",
            " unwanted results")
  }
  if (type == "size") {
    make_class(res, breakpoints)
  } else if (type == "offset") {
    make_class("offset", res, breakpoints)
  } else if (type == "gap") {
    class <- make_class(res, breakpoints)
    if (any(res != "gapless")) {
      class <- add_class(class, make_class("variable"))
    }
    class
  }
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_column_size <- function(x) {
  validate_bulma_unit(x, "size")
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_offset <- function(x) {
  validate_bulma_unit(x, "offset")
}

#' @rdname validate_bulma_unit
#' @export
validate_bulma_gap <- function(x) {
  validate_bulma_unit(x, "gap")
}

#' @rdname validate_bulma_unit
#' @export
get_bulma_media_breakpoints <- function() {
  c("mobile", "tablet", "touch",
    "desktop", "widescreen", "fullhd")
}


#' Validate Proper Bulma Color Definition
#'
#' `validate_bulma_color` checks that the argument is a valid bulma color and can be used
#' by the `has-(text|background)-.*` helpers respectively.
#'
#' @param x \[`character(n)`\]\cr
#'        The color to validate.
#' @param context \[`character(1)`: \sQuote{text}\]\cr
#'        If \dQuote{text} the color must work as a text color via `has-text-{color}`. If
#'        \dQuote{background} the color must work as a background color via
#'        `has-background-{color}`, if \dQuote{button} the color must work as button
#'        (background) color via `is-{color}`.
#' @param must_be_key \[`logical(1)`: \sQuote{FALSE}\]\cr
#'        If \sQuote{TRUE}, the color to checked must be a key of the SASS `$colors` map.
#'        These colors can be used by some elements directly via `is-{color}` instead of
#'        `has-text-{color}`.
#'
#' @details This function uses the bulma config data sets [bulma_config] and
#' [bulmaswatch_config] respectively to check whether the argument is valid. The relevant
#' column in these datasets are scraped from the official bulma documentation.
#'
#' # Note
#' In order to determine whether the bulma or the bulmaswatch config dataset should be
#' used, this function relies on an internal variable which points to the theme used in
#' `bulma_page` (if any was specified.). Thus, the results may differ depending on the
#' last call to `bulma_page`. At the time being, the same colors are defined for the
#' default `bulma` theme and the `bulmaswatch` themes. However, this may change in the
#' future. Thus, we opted for this approach.
#'
#' @return The proper bulma color class, if possible. Otherwise an error is raised.
#' @export
#'
#' @examples
#' validate_bulma_color("black")
#' validate_bulma_color("primary-light", "text")
#' ## standard bulma config will be used
#' validate_bulma_color("link")
#' validate_bulma_color(c("primary", "ghost"), "button")
#' if ("flatly" %in% get_bulma_themes()) {
#'    ## flatly theme config will be used
#'    bulma_page(theme = "flatly")
#'    validate_bulma_color("link")
#'    ## reset theme
#'    bulma_page()
#' }
#' validate_bulma_color("success", "background")
validate_bulma_color <- function(x, context = c("text", "background", "button"),
                                 must_be_key = FALSE) {
  if (is.null(x)) {
    return(x)
  }
  if (any(is.na(x))) {
    stop("color must not contain any 'NAs'",
         domain = NA)
  }
  if (!is.character(x)) {
    stop("color must be a character vector",
         domain = NA)
  }
  context <- match.arg(context)
  theme <- bulma_global$theme
  if (is.null(theme) || is.na(theme)) {
    lkp <- bulma_config
  } else { # nocov start
    message("using config for theme <", theme, ">",
            domain = NA)
    lkp <- bulmaswatch_config[bulmaswatch_config$theme == theme, ]
  } # nocov end
  lkp <- lkp[lkp$group == "color" &
               !is.na(lkp$color_class), ]
  lkp <- lkp[grepl(context, lkp$color_class, fixed = TRUE), ]
  orig <- lkp
  if (must_be_key) {
    lkp <- lkp[lkp$is_color_map_key, ]
  }
  NOK <- !x %in% lkp$variable
  if (any(NOK)) {
    bad_cols <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
    not_key <- x[NOK] %in% orig$variable
    msg <- ngettext(sum(NOK),
                    paste(bad_cols, "is not a valid bulma", context, "color"),
                    paste(bad_cols, "are not valid bulma", context, "colors"))
    if (any(not_key)) {
      bad_cols <- paste(paste0("\"", x[NOK][not_key], "\""), collapse = ", ")
      msg <- ngettext(sum(not_key),
                      paste0(msg, " (", bad_cols,
                             " is indeed a bulma color but not a color key)"),
                      paste0(msg, " (", bad_cols,
                             " are indeed bulma colors but not color keys)"))
    }
    stop(msg,
         domain = NA)
  }
  lkp <- lkp[lkp$variable %in% x, ]
  if (must_be_key || context == "button") {
    make_class(lkp$variable, collapse = FALSE)
  } else {
    make_class(context, lkp$variable,
               prefix = "has", collapse = FALSE)
  }
}

#' Validate Proper Bulma Size Definition
#'
#' @param x \[`character(n)`\]\cr
#'        The size to validate.
#' @param normal_to_null \[`logical(1)`: \sQuote{TRUE}\]\cr
#'        If \sQuote{TRUE} size \sQuote{normal} is replaced by \sQuote{NULL}. This is
#'        useful if \sQuote{normal} is anyways the default.
#' @param prefix \[`character(1)`: \sQuote{NULL}\]\cr
#'        The prefix used for the size class, should be \sQuote{is} (or equivalently
#'        \sQuote{NULL}) or \sQuote{are}.
#' @param disallow \[`character(n)`: \sQuote{NULL}\]\cr
#'        Disallow certain classes in the validation. This is needed in case an element
#'        does not support all of the standard bulma sizes (e.g. `<tags>`).
#' @param additional \[`character(n)`: \sQUote{NULL}\]\cr
#'        Include additional classes in the validation. This is needed for elements like
#'        `bulma_hero` which allow sizes like `fullheight`.
#'
#' @details This function simply compares its arguments with the list of valid bulma
#' sizes and returns the proper class or throws an error if an invalid size is passed.
#' Valid sizes are:
#' * `small`
#' * `normal`
#' * `medium`
#' * `large`
#' * plus additional elements from `additional`
#'
#' @return The proper bulma size class, if possible. Otherwise an error is raised.
#' @export
#'
#' @examples
#' validate_bulma_size(c("small", "normal", "medium", "large"), FALSE)
#' validate_bulma_size("normal") ## returns NULL as it is replaced
#' ## in this form this is pretty useless, but in a function it makes
#' ## snese to allow additional elements to be valid
#' validate_bulma_size("fullheight", addiitonal = "fullheight")
#'
#' \dontrun{
#' ## these will all raise an error
#' validate_bulma_size("xlarge")
#' validate_bulma_size("small", disallow = "small")
#' }
validate_bulma_size <- function(x, normal_to_null = TRUE, prefix = NULL,
                                disallow = NULL, additional = NULL) {
  if (is.null(x)) {
    return(x)
  }
  if (!is.null(prefix) && !prefix %in% c("is", "are")) {
    stop("invalid prefix - must be either \"is\", \"are\" or NULL",
         domain = NA)
  }
  if (any(is.na(x))) {
    stop("size must not contain any 'NAs'",
         domain = NA)
  }
  if (!is.character(x)) {
    stop("size must be a character vector",
         domain = NA)
  }
  valid_sizes <- c("small", "normal", "medium", "large", additional)
  if (!is.null(disallow)) {
    if (!disallow %in% valid_sizes) {
      warning(paste0("'", disallow , "' is not a valid size and removing it is a no-op"),
                     domain = NA)
    }
    valid_sizes <- setdiff(valid_sizes, disallow)
  }
  NOK <- !x %in% valid_sizes
  if (any(NOK)) {
    bad_sizes <- paste(paste0("\"", x[NOK], "\""), collapse = ", ")
    msg <- ngettext(sum(NOK),
                    paste(bad_sizes, "is not a valid bulma size"),
                    paste(bad_sizes, "are not valid bulma sizes"))
    stop(msg,
         domain = NA)
  }
  if (normal_to_null) {
    x <- x[x != "normal"]
  }
  make_class(x, prefix = prefix, collapse = FALSE)
}
