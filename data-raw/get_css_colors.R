library(here)
library(rvest)
library(dplyr)
library(stringr)
library(htmltools)
library(sass)
library(glue)
library(purrr)

get_css_colors <- function(theme = NULL) {
  ## get primary colors from official bulma doc

  bulma_doc <- read_html("https://bulma.io/documentation/customize/variables/")

  ## initial values
  initial_vars <- bulma_doc %>%
    html_element("#initial-variables ~ .table-container > table") %>%
    html_table() %>%
    setNames(c("variable", "type", "value"))

  color_names_initial <- initial_vars %>%
    filter(type == "color") %>%
    transmute(variable = str_remove(variable, fixed("$"))) %>%
    pull(variable)

  ## derived values
  bulma_table <- bulma_doc %>%
    html_elements(".bd-is-body")

  colors_derived_idx <- which(bulma_table %>%
                                html_nodes(".bd-var-computed-type") %>%
                                html_text() == "color" |
                                bulma_table %>%
                                html_nodes(".bd-var-type") %>%
                                html_text() == "compound")

  color_names_derived <- (bulma_table %>%
                            html_nodes(".bd-var-name .nt") %>%
                            html_text())[colors_derived_idx]

  ## create a temporary SASS file which includes all colors and imports
  ## bulma function definitions and assign each color to a own class
  ## parse this document and get back the final colors / fonts

  color_classes <- lapply(c(color_names_initial, color_names_derived),
                          function(color) {
                            glue("${color} : default !default;",
                                 ".{color} {{",
                                 "  color: ${color};",
                                 "}}",
                                 .sep  ="\n", .trim = FALSE)
                          }) %>%
    paste(collapse = "\n")

  tmp_sass <- glue("{sass_import('derived-variables')}",
                   "{color_classes}\n",
                   .sep = "\n")
  if(!is.null(theme)) {
    tmp_sass <- glue("{sass_import('variables')}",
                     "{tmp_sass}", .sep = "\n")
  }
  tmp_sass <- glue("@use \"sass:map\";",
                   "{tmp_sass}",
                   "/*! Color Map Keys: #{{map.keys($colors)}} */",
                   .sep = "\n")
  sass_f <- normalizePath(tempfile(fileext = ".saas"), mustWork = FALSE)
  css_f <- normalizePath(tempfile(fileext = ".css"), mustWork = FALSE)
  cat(tmp_sass, file = sass_f)

  load_paths <- normalizePath(here("tools", "node_modules", "bulma", "sass", "utilities"))
  extract_colors <- function(theme) {
    if (!is.null(theme)) {
      load_paths <- c(load_paths,
                      normalizePath(here("tools", "node_modules", "bulmaswatch", theme)))
    }

    ## need to call sass directly (instead of sass) b/c need to use the dart implementation
    withr::with_dir(here("tools"),
                    system(glue("sass --style=compressed --no-source-map ",
                                "{paste0('-I ', '\"', load_paths, '\"', collapse = ' ')} ",
                                "\"{sass_f}\":\"{css_f}\"")))
    colors <- readLines(css_f)
    res <- str_match_all(colors, "\\.([^{]+)\\{color:([^}]+)\\}")[[1]] %>%
      as_tibble(.name_repair = "minimal") %>%
      setNames(c("match", "variable", "value")) %>%
      filter(value != "default") %>%
      transmute(theme = ifelse(is.null(theme), "bulma", theme),
                group = "color",
                variable,
                value = parseCssColors(value))
    color_map_keys <- str_match(colors, "/\\*! Color Map Keys: ([\\w\\s,]+) ")[, 2] %>%
      str_split(", ") %>%
      unlist()
    if (is.null(theme)) {
      res <- res %>%
        bind_rows(c(theme = "bulma",
                    group = "font",
                    variable = "name",
                    value = NA_character_),
                  c(theme = "bulma",
                    group = "font",
                    variable = "id",
                    value = NA_character_))
    } else {
      theme_scss <- readLines(here("tools", "node_modules", "bulmaswatch", theme,
                                   "_overrides.scss"), warn = FALSE)
      mtch <- str_subset(theme_scss, fixed("@import")) %>%
        str_match("family=([^:&]+)")
      family <- str_replace_all(mtch[,2], "\\+", " ")
      id <- gfonts::get_all_fonts()[gfonts::get_all_fonts()$family == family, "id"]
      res <- res %>%
        bind_rows(c(theme = theme,
                    group = "font",
                    variable = "name",
                    value = family),
                  c(theme = theme,
                    group = "font",
                    variable = "id",
                    value = id))
    }
    res %>%
      mutate(is_color_map_key = variable %in% color_map_keys) %>%
      as.data.frame()
  }
  if (is.null(theme)) {
    res  <- extract_colors(NULL)
  } else {
    res <- map_dfr(theme, extract_colors)
  }
  color_doc <- read_html("https://bulma.io/documentation/helpers/color-helpers/")
  color_classes <- color_doc %>%
    html_elements("table") %>%
    html_table() %>%
    map_dfr(~ .x %>% select(Class)) %>%
    transmute(color = str_remove(Class, "has-(text|background)-"),
           type = str_extract(Class, "text|background")) %>%
    group_by(color) %>%
    summarise(color_class = paste(type, collapse = ", "), .groups = "drop")
  res %>%
    left_join(color_classes,
              c(variable = "color"))

}
