get_package_name <- function() {
  environmentName(parent.env(environment()))
}

get_download_links_gh <- function(owner, repo,
                                  path,
                                  recursive = TRUE,
                                  branch = NULL,
                                  gh_user = NULL,
                                  gh_password = NULL) {
  parse_list <- function(.) {
    len <- vapply(., length, integer(1L))
    .[len == 0L] <- NA
    as.data.frame(.[len <= 1L])
  }
  download_fun <- function(path, depth) {
    base_url <- "https://api.github.com/repos"
    path <- gsub("^/|/$", "", path)

    url <- sprintf("%s/%s/%s/contents/%s",
                   base_url, owner, repo, path)
    if (!is.null(branch)) {
      url <- paste0(url, "?ref=", branch)
    }
    ## Allow end users to pass their github credentials
    ## to circumvent download restrictions
    if (!(is.null(gh_user) | is.null(gh_password))) {
      result <- httr::GET(url,
                          httr::authenticate(gh_user, gh_password))
    } else {
      result <- httr::GET(url)
    }
    if (httr::http_error(result)) {
      stop(sprintf("An error occured while trying to fetch <%s>:\n%s",
                   url, httr::content(result)$message),
           domain = NA)
    }
    content <- do.call(rbind,
                       lapply(httr::content(result), parse_list))
    content$parent <- path
    content$depth <- depth
    subdirs <- content$path[content$type == "dir"]

    if (recursive && length(subdirs)) {
      children <- lapply(subdirs, sys.function(0), depth = depth + 1L)
      content <- do.call(rbind, c(list(content), children))
    }
    content
  }
  download_fun(path, 1L)
}

is_integer <- function(x) {
  is.numeric(x) & x %% 1L == 0L
}

add_class <- function(old, new) {
  if (is.null(new)) {
    old
  } else {
    new <- paste(new, collapse = " ")
    if (is.null(old)) {
      new
    } else {
      paste(old, new)
    }
  }
}


make_class <- function(..., prefix = NULL, collapse = TRUE) {
  prefix <- match.arg(prefix, c("is", "has", "are"), several.ok = TRUE)
  if (!length(c(...))) {
    return(NULL)
  }
  args <- list(prefix, ..., sep = "-")
  classes <- do.call(paste, args)
  ## remove trailing dashes
  classes <- sub("-$", "", classes)
  if (collapse) {
    paste(classes,
          collapse = " ")
  } else {
    classes
  }
}

`%||%` <- function(x, y) {
  if (is.null(x) | length(x) == 0) {
    y
  } else {
    x
  }
}

parse_attributes <- function(node) {
  attribs <- as.list(xml2::xml_attrs(node))
  ## XML does not support attributes without a value
  ## thus properties wihtut a value receive the name of the attribute as value
  ## test for this and replace by NA (whihc is the syntax htmltools uses)
  attribs[names(attribs) == attribs] <- NA
  attribs
}

parse_node <- function(node) {
  tag_name <- xml2::xml_name(node)
  if (tag_name == "text") {
    value <- trimws(xml2::xml_text(node))
    if (nchar(value) > 0) {
      value
    } else {
      NULL
    }
  } else if (tag_name != "comment") {
    attr <- parse_attributes(node)
    children <- lapply(xml2::xml_contents(node), parse_node)
    children <- Filter(Negate(is.null), children)
    args <- c(attr, children)
    if (tag_name %in% names(htmltools::tags)) {
      fn <- htmltools::tags[[tag_name]]
    } else {
      warning("unknown HTML tag <", tag_name, ">",
              domain = NA)
      fn <- htmltools::tag
      args <- list(`_tag_name` = tag_name, varArgs = args)
    }
    do.call(fn, args)
  }
}

html_to_tags <- function(html_string) {
  ## this function is inspired by https://github.com/alandipert/html2r/blob/master/app.R
  xml <- xml2::read_html(as.character(htmltools::div(id = "parse_me",
                                                     htmltools::HTML(html_string))))
  elements <- xml2::xml_find_all(xml, "//div[@id='parse_me']/*")
  wrap <- if (length(elements) > 1) htmltools::tagList else identity
  do.call(wrap,
         lapply(elements, parse_node))

}
