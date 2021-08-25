split_class <- function(x) {
  if (length(x)) {
    unname(unlist(strsplit(unlist(x), "\\s+")))
  } else {
    NULL
  }
}

get_class <- function(tag) {
  if (inherits(tag, "shiny.tag")) {
    split_class(tag$attribs[names(tag$attribs) == "class"])
  } else {
    NULL
  }
}

get_type <- function(tag) {
  if (inherits(tag, "shiny.tag")) {
    tag$name
  } else {
    "text_node"
  }
}

`%in0%` <- function(x, y) {
  stopifnot(length(x) <= 1 || length(x) == length(y))
  if (is.null(x)) {
    res <- rep(TRUE, length(y))
  } else {
    res <- x == y
  }
  res
}

grepl0 <- function(pattern, x, ...) {
  if (is.null(pattern)) {
    res <- rep(TRUE, length(x))
  } else {
    pattern <- paste(pattern, collapse = "\t")
    res <- grepl(paste0("(^|\\t)", pattern, "($|\\t)"), x, ...)
  }
  res
}

flatten_children <- function(tag) {
  ## code copied from htmltools:::flattenTags
  flatten <- function(x) {
    if (inherits(x, "shiny.tag")) {
      list(x)
    } else if (is.list(x) && (inherits(x, "shiny.tag.list") ||
                              identical(class(x), "list"))) {
      if (length(x) == 0) {
        x
      } else {
        unlist(lapply(x, flatten), recursive = FALSE)
      }
    } else if (is.character(x)) {
      list(x)
    } else {
      flatten(htmltools::as.tags(x))
    }
  }
  tag$children <- flatten(tag$children)
  tag
}

expect_has_slots <- function(object, expected) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  act$names <- names(act$val)
  if (identical(act$names, NULL)) {
    testthat::fail(sprintf("%s does not have names.",
                           act$lab))
  } else {
    NOK <- !expected %in% act$names
    if(any(NOK)) {
      testthat::fail(sprintf("%s does not contain slot%s %s.",
                             act$lab,
                             ifelse(sum(NOK) > 1, "s", ""),
                             paste0("'", expected[NOK], "'", collapse = ", ")))
    }
  }
  testthat::succeed()
  invisible(act$val) #nocov
}

expect_tag_class <- function(object, class, all_or_any = c("all", "any"),
                             exact = FALSE) {
  all_or_any <- match.arg(all_or_any)
  if (exact && all_or_any == "any") {
    warning("'exact == TRUE' makes sense only for 'all' - ignoring it",
            domain = NA)
    exact <- FALSE
  }
  all_or_any <- match.fun(all_or_any)
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  if (!inherits(act$val, "shiny.tag")) {
    testthat::fail(sprintf("%s is not a tag object.", act$lab))
  } else {
    all_classes <- get_class(act$val)
    if (is.null(all_classes)) {
      testthat::fail(sprintf("%s does not have a class attribute.", act$lab))
    } else {
      class <- split_class(class)
      OK <- class %in% all_classes
      if (!all_or_any(OK)) {
        testthat::fail(sprintf("%s %s of %s's classes ['%s'].",
                               paste0("'", class[!OK], "'", collapse = ", "),
                               ifelse(sum(!OK) > 1, "are not part", "is not a part"),
                               act$lab, paste(all_classes, collapse = " ")))
      }
      if (exact) {
        OK <- all_classes %in% class
        if (!all(OK)) {
          testthat::fail(sprintf("%s %s not matched by given class%s ['%s']",
                                 paste0("'", all_classes[!OK], "'", collapse = ", "),
                                 ifelse(sum(!OK) > 1, "are", "is"),
                                 ifelse(length(class) > 1, "es", ""),
                                 paste(class, collapse = " ")))
        }
      }
    }
  }
  testthat::succeed()
  invisible(act$val)
}

expect_tag_type <- function(object, tag) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  if (tag == "shiny.tag.list") {
    if (!inherits(act$val, "shiny.tag.list")) {
      testthat::fail(sprintf("%s is not a shiny tag list.", act$lab))
    }
  } else if (!inherits(act$val, "shiny.tag")) {
    testthat::fail(sprintf("%s is not a tag object.", act$lab))
  } else {
    if (!identical(get_type(act$val), tag)) {
      testthat::fail(sprintf("%s is not a `<%s>` tag but a `<%s>` tag.",
                             act$lab, tag, act$val$name))
    }
  }
  testthat::succeed()
  invisible(act$val)
}

expect_tag_classed_type <- function(object, tag, class, ...) {
  expect_tag_type({{object}}, tag)
  expect_tag_class({{object}}, class, ...)
}

expect_tag_attrib <- function(object, attrib, value, regex = FALSE, ...) {
  stopifnot(length(attrib) == length(value))
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  if (!inherits(act$val, "shiny.tag")) {
    testthat::fail(sprintf("%s is not a tag object.", act$lab))
  } else {
    attribs <- split(object$attribs, names(object$attribs))
    NOK <- !attrib %in% names(attribs)
    if (any(NOK)) {
      testthat::fail(sprintf("%s %s valid attribute%s of %s",
                             paste0("'", attrib[NOK], "'", collapse = ", "),
                             ifelse(sum(NOK) > 1, "are not", "is not a"),
                             ifelse(sum(NOK) > 1, "s", ""),
                             act$lab))
    } else {
      values <- stats::setNames(value, attrib)
      res <- vapply(attrib, function(nm) {
        vals <- unlist(values[nm])
        if (!regex) {
          NOK <- !vals %in% unlist(attribs[[nm]])
        } else {
          NOK <- !grepl(vals, unlist(attribs[[nm]]), ...)
        }
        if (any(NOK)) {
          if (!regex) {
            vals <- paste0("= \"", paste(vals[NOK], collapse  = " "), "\"")
          } else {
            vals <- paste0("~= /", paste(vals[NOK], collapse = "|"), "/")
          }
          paste(nm, vals)
        } else {
          NA_character_
        }
      }, character(1))
      res <- res[!is.na(res)]
      if (length(res)) {
        testthat::fail(sprintf("%s does not contain attribute%s %s",
                               act$lab,
                               ifelse(length(res) > 1, "s", ""),
                               paste0("[", paste(res, collapse = ", "), "]")))
      }
    }

  }
  testthat::succeed()
  invisible(act$val)
}

expect_tag_children_length <- function(object, n) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  if (!inherits(act$val, "shiny.tag")) {
    testthat::fail(sprintf("%s is not a tag object.", act$lab))
  } else {
    if (n != length(act$val$children)) {
      testthat::fail(sprintf("%s has %i children, not %i children.",
                             act$lab, length(act$val$children), n))
    }
  }
  testthat::succeed()
  invisible(act$val) #nocov
}

expect_tag_children <- function(object, type = NULL, class = NULL,
                                strict = FALSE, partial = TRUE) {
  stopifnot(!is.null(class) || !is.null(type))
  stopifnot(is.null(class) || is.null(type) || length(class) == length(type))
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  if (!inherits(act$val, "shiny.tag")) {
    testthat::fail(sprintf("%s is not a tag object.", act$lab))
  } else {
    if (is.null(type)) {
      type <- rep(list(NULL), length(class))
    }
    if (is.null(class)) {
      class <- rep(list(NULL), length(type))
    }
    kids <- flatten_children(act$val)$children
    if (!partial) {
      diff <- length(class) - length(kids)
      if (diff != 0) {
        testthat::fail(
          sprintf("%i element%s too %s specified in non partial mode (%i needed).",
                  abs(diff),
                  ifelse(abs(diff) > 1, "s", ""),
                  ifelse(diff > 0, "many", "few"),
                  length(kids)))
      }
    }
    all_classes <- vapply(kids, function(tag) {
      res <- get_class(tag)
      if (is.null(res)) {
        NA_character_
      } else {
        paste(res, collapse = "\t")
      }
    }, character(1))
    all_types <- vapply(kids, get_type, character(1))
    res <- Map(function(cls, ty) {
      cls <- split_class(cls)
      which(ty %in0% all_types &
              grepl0(cls, all_classes))
    }, class, type)
    get_pos <- function(x, y) {
      candidates <- y[y > max(x, na.rm = TRUE)]
      if (length(candidates)) {
        res <- min(candidates)
      } else {
        res <- NA
      }
      c(x, res)
    }
    format_type_class <- function(type, class) {
      idxn <- vapply(class, is.null, logical(1))
      class[!idxn] <- paste0(" class = \"", class[!idxn], "\"")
      class[idxn] <- ""
      idxn <- vapply(type, is.null, logical(1))
      type[idxn] <- "*"
      paste0("<", type, class, ">")
    }
    first <- res[[1L]]
    if (length(first)) {
      res <- Reduce(get_pos, res[-1L], min(first))
      NOK <- is.na(res)
      if (any(NOK)) {
        testthat::fail(
          sprintf("element%s %s could not be matched at position%s %s.",
                  ifelse(sum(NOK) > 1, "s", ""),
                  paste(format_type_class(type[NOK], class[NOK]), collapse = ", "),
                  ifelse(sum(NOK) > 1, "s", ""),
                  paste0("#", which(NOK), collapse = ", ")
          ))
      } else {
        if (strict) {
          NOK <- diff(res) > 1
          if (any(NOK)) {
            NOK <- which(NOK) + 1
            testthat::fail(
              sprintf("element%s %s could not be matched at position%s %s.",
                      ifelse(length(NOK) > 1, "s", ""),
                      paste(format_type_class(type[NOK], class[NOK]), collapse = ", "),
                      ifelse(length(NOK) > 1, "s", ""),
                      paste0("#", NOK, collapse = ", ")
              ))
          }
        }
      }
    } else {
      testthat::fail(
        sprintf("element %s could not be matched at position #1.",
                format_type_class(type[1L], paste(class[[1L]], collapse = " "))))
    }
  }
  testthat::succeed()
  invisible(act$val)
}

