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
    paste(old, paste(new, collapse = " "))
  }
}


make_class <- function(..., prefix = NULL, collapse = TRUE) {
  prefix <- match.arg(prefix, c("is", "has"), several.ok = TRUE)
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
