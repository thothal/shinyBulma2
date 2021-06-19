suppressPackageStartupMessages({
  library(crayon)
  library(stringr)
  library(here)
  library(gfonts)
  library(cli)
  library(rprojroot)
  library(glue)
  library(diffobj)
})
options(cli.num_colors = 256)

## need this hack lest colors do not work
cat(blue(""))
cli_h1("Download Google Fonts")
## get command line arguments (I cannot convince grunt to pass the array as array)
args <- strsplit(commandArgs(TRUE), ",")[[1]]

if(!length(args)) {
  cli_alert_danger("This script requires at least one filename as argument")
  cli_rule()
  quit(status = 64)
}

.proj_root <- normalizePath(find_root(is_r_package))
.fonts_home <- here("inst", "fonts")

get_relative_path <- function(path, proj_root = .proj_root,
                              add_root = TRUE) {
  path <- normalizePath(path, mustWork = FALSE)
  if (add_root) {
    glue(str_replace(path, fixed(proj_root), "{col_grey('project root')}"))
  } else {
    str_remove(path, fixed(proj_root))
  }
}

get_installed_fonts <- function(fonts_home = .fonts_home) {
  fonts <- basename(list.files(fonts_home, "\\.woff2$", recursive = TRUE))
  matches <- str_match(fonts, "(.*)-v\\d{1,2}-.*-([^.]+)\\.woff2")
  split(matches[, 3], matches[, 2])
}

get_import <- function(css) {
  ## get all @import statements
  msg <- "Scanning file for @import directives"
  imports <- str_subset(css, fixed("@import"))
  cli_process_start(msg,
                    msg_done = "{msg} ...{length(imports)} directive{?s} found:",
                    msg_failed = "{msg} ...none found",
                    on_exit = "done",
                    .envir = environment())
  if (!length(imports)) {
    cli_process_failed()
    cli_rule()
    NA
  } else {
    str_match(imports,
              "^.*url\\(\"([^\"]+)")[, 2]
  }
}

parse_url <- function(url) {
  ## sample inputs:
  ## https://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&display=swap
  ## https://fonts.googleapis.com/css?family=Varela+Round&display=swap
  mtch <- str_match(url, "family=([^:]+):?([^&]*)&")
  mtch[nchar(mtch[, 3]) == 0, 3] <- NA
  variants <- str_replace_all(
    str_replace_all(unlist(str_split(mtch[, 3], ",")),
                    "^400$",
                    "regular"),
    fixed("400"),
    "")
  res <-   list(family = str_replace_all(mtch[,2], "\\+", " "),
                variants = ifelse(is.na(variants), "regular", variants))
  cli_rule(col_cyan("Parameter"))
  cli_li(c("{col_blue('Family')}: {res$family}",
           "{col_blue('Variants')}: {cli_format(variants)}"))
  cli_rule()
  res
}

process_css <- function(css_fn) {
  css_fn <- normalizePath(css_fn, mustWork = FALSE)
  cli_alert_info("Parsing file <{get_relative_path(css_fn)}>")
  if (!file.exists(css_fn)) {
    cli_alert_danger("File <{css_fn}> cannot be found")
    cli_rule()
    return(FALSE)
  }
  css <- readLines(css_fn, warn = FALSE)

  urls <- get_import(css)
  if (!is.na(urls)) {
    d <- cli_div(theme = list(li = list(before = "   ")))
    cli_li(urls)
    cli_end(d)

    fonts <- get_all_fonts()
    installed_fonts <- get_installed_fonts()

    invisible(Map(function(url) {
      font_info <- parse_url(url)
      id <- fonts[str_which(fonts$family,
                            regex(paste0("^", font_info$family, "[[:alpha:]]*$"),
                                  ignore_case = TRUE)),
                  "id"]
      font_subfolder <- normalizePath(here(.fonts_home, id), mustWork = FALSE)
      cli_alert_info("Installing to <{get_relative_path(font_subfolder)}>")
      if (!dir.exists(font_subfolder)) dir.create(font_subfolder, recursive = TRUE)
      installed_variants <- installed_fonts[[id]]
      variants <- setdiff(font_info$variants, installed_variants)
      if (length(variants)) {
        installed_fonts[[id]] <<- c(installed_fonts, variants)
        if (!is.null(installed_variants)) {
          cli_alert_info("Variants {cli_format(installed_variants)} already installed")
        }
        cli_alert_info("Installing variants {cli_format(variants)}")
        dl <- download_font(id,
                            font_subfolder,
                            variants) %>%
          normalizePath()

        cli_alert_info("Installing the following files:")
        cli_div(class = "li", theme = list(`ul li` = list(`list-style-type` = symbol$bullet,
                                                          before = " ")))
        dl <- sapply(paste0(str_replace_all(dl, fixed(.proj_root),
                                            "   <{col_grey('project root')}"), ">"),
                     glue) %>%
          sort()
        cli_ul()
        cli_li(dl)
        cli_end()
        cli_end()
      } else {
        cli_alert_success("All variants already installed")
      }
      font_css <- generate_css(id, font_info$variants,
                               font_dir = str_remove(
                                 get_relative_path(font_subfolder, add_root = FALSE),
                                 "[/\\\\]?inst[/\\\\]?"))
      cli_alert_info("Replacing @import directive with generic font css")
      css_new <- paste(str_replace_all(css,
                                       fixed(as.character(glue("@import url(\"{url}\");"))),
                                       font_css),
                       collapse = "\n")
      cli_rule(col_cyan("Diff"))
      diff_files <- diffChr(css, css_new, format = "ansi256",
                            mode = "unified", disp.width = console_width(),
                            context = 0)
      cat(paste(as.character(diff_files),
                collapse = "\n"), "\n")
      cat(css_new, file = css_fn) # str_replace(css_fn, fixed(".css"), "_local_fonts.css"))
    }, urls))
  }
  return(TRUE)
}

res <- Map(process_css, args)

cli_rule()

if (!all(unlist(res))) {
  quit(status = 65)
}
