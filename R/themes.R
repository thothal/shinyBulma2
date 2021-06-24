#' Get List of Installed Bulma Themes
#'
#' This function looks into the `lib/` folder located in the package's base folder and
#' returns all found themes in the corresponding subfolder. Themes are compiled from
#' bulmaswatch.
#'
#' @seealso <https://jenil.github.io/bulmaswatch/>
#'
#' @return character vector, all installed bulma themes.
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

#' Download Pre-Compiled Bulma Themes
#'
#' This function downloads pre-compiled CSS files and Google font files for the
#' [bulmaswatch themes](https://jenil.github.io/bulmaswatch/). These files are
#' hosted on this package's [Github repository](`r sub("URL:\\s*", "", grep("^URL:", readLines("DESCRIPTION"), value = TRUE))`).
#'
#' @param themes \[`character(n)`: \sQuote{all}\]\cr the names of the themes to be
#' installed. If \sQuote{all}, all available themes are downloaded.
#' @param gh_user,gh_password \[`character(1)`: \sQuote{NULL}\]\cr GitHub credentials,
#' see *Note*.
#'
#' @section Note:
#' To dynamically get the URLs from the GitHub repo, we make use of
#' [GitHub's API](https://docs.github.com/en/rest). Without authentification, one can make
#' up to 60 calls to the API per hour. In very rare occasions, you may exceed this limit
#' (for instance if you are calling this function several times). The limit for
#' authenticated API calls is 1000 calls. Thus, you can provide your credentials to
#' this function to significantly increase the limit.
#'
#' @return list (invisible), the used download links for the fonts and the themes
#' respectively.
#' @export
#'
#' @examples
#' \dontrun{
#' download_bulma_themes("flatly")
#' }
download_bulma_themes <- function(themes = "all",
                                  gh_user = NULL,
                                  gh_password = NULL) {
  fonts_src <- normalizePath(system.file("fonts",
                                         package = get_package_name()),
                             mustWork = FALSE)
  themes_src <- normalizePath(system.file("lib", "bulmaswatch",
                                          package = get_package_name()),
                              mustWork = FALSE)

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed to install themes. Please install it.",
         call. = FALSE)
  }

  if (!dir.exists(fonts_src)) {
    message(paste0("Creating directory <", fonts_src, ">"))
    dir.create(fonts_src)
  }
  if (!dir.exists(themes_src)) {
    message(paste0("Creating directory <", themes_src, ">"))
    dir.create(themes_src)
  }

  branch <- "feat-add-bulma-page"

  themes_resources <- get_download_links_gh("thothal",
                                            "shinyBulma2",
                                            "inst/lib/bulmaswatch",
                                            branch = branch,
                                            gh_user = gh_user,
                                            gh_password = gh_password)
  fonts_resources <- get_download_links_gh("thothal",
                                           "shinyBulma2",
                                           "inst/fonts",
                                           branch = branch,
                                           gh_user = gh_user,
                                           gh_password = gh_password)
  avail_themes <- sub("\\.min\\.css$", "", themes_resources$name)
  if (themes == "all") {
    themes <- avail_themes
  }
  NOK <- !themes %in% avail_themes
  if (any(NOK)) {
    warning(sprintf("Unknown theme%s: [%s]",
                    ifelse(sum(NOK) > 1, "s", ""),
                    paste0("'", themes[NOK], "'", collapse = ", ")))
    themes <- themes[!NOK]
  }

  requested_themes <- themes_resources[avail_themes %in% themes, , drop = FALSE]

  download_to_folder <- function(url, base_folder) {
    message(sprintf("Downloading file <%s>", basename(url)))
    dest_fn <- file.path(base_folder, basename(url))
    utils::download.file(url, dest_fn, quiet = TRUE)
  }

  ## I like to use plain old for loops when a function is called only for its side effects
  for (url in requested_themes$download_url) {
    download_to_folder(url, base_folder = themes_src)
  }

  ## need to namespace data even if it is in the very same package
  config <- shinyBulma2::bulmaswatch_config
  needed_fonts <- config[config$group == "font" &
                           config$variable == "id" &
                           config$theme %in% themes &
                           !is.na(config$value), , drop = FALSE]
  folder <- paste0("inst/fonts/", needed_fonts$value, collapse = "|")
  fonts_resources <- fonts_resources[grep(folder, fonts_resources$parent), , drop = FALSE]

  for (row in seq_len(nrow(fonts_resources))) {
    font_sub_folder <- normalizePath(file.path(fonts_src,
                                               basename(fonts_resources[row, "parent"])),
                                     mustWork = FALSE)
    if (!dir.exists(font_sub_folder)) {
      message(paste0("Creating directory <", font_sub_folder, ">"))
      dir.create(font_sub_folder)
    }
    download_to_folder(fonts_resources[row, "download_url"], font_sub_folder)
  }
  invisible(list(themes = requested_themes$download_url,
                 fonts = fonts_resources$download_url))
}
