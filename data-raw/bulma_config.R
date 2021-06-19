## code to prepare `bulma_config` dataset goes here
library(here)
library(rvest)
library(dplyr)
library(stringr)
library(htmltools)
library(sass)
library(glue)

## get primary colors from official bulma doc

bulma_doc <- read_html("https://bulma.io/documentation/customize/variables/")

## initial values
initial_vars <- bulma_doc %>%
  html_node("#initial-variables ~ .table-container > table") %>%
  html_table() %>%
  setNames(c("variable", "type", "value"))

bulma_table <- bulma_doc %>%
  html_nodes(".bd-is-body")

colors_derived_idx <- which(bulma_table %>%
                              html_elements(".bd-var-computed-type") %>%
                              html_text() == "color")

color_names_initial <- initial_vars %>%
  filter(type == "color") %>%
  transmute(variable = str_remove(variable, fixed("$"))) %>%
  pull(variable)
color_names_derived <- (bulma_table %>%
                          html_elements(".bd-var-name .nt") %>%
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

tmp_sass <- glue("{sass_import('initial-variables')}",
                 "{sass_import('derived-variables')}",
                 "{sass_import('functions')}",
                 "{color_classes}\n",
                 .sep = "\n")

sass_f <- normalizePath(tempfile(fileext = ".saas"), mustWork = FALSE)
css_f <- normalizePath(tempfile(fileext = ".css"), mustWork = FALSE)
cat(tmp_sass, file = sass_f)

bulma_src <- normalizePath(here("tools", "node_modules", "bulma", "sass", "utilities"))

## need to call sass directly (instead of sass) b/c need to use the dart implementation
withr::with_dir(here("tools"),
                system(glue("sass --style=compressed --no-source-map --load-path=\"{bulma_src}\" ",
                            "\"{sass_f}\":\"{css_f}\"")))
colors <- readLines(css_f)
bulma_config <- str_match_all(colors, "\\.([^{]+)\\{color:([^}]+)\\}")[[1]] %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(c("match", "variable", "value")) %>%
  filter(value != "default") %>%
  transmute(theme = "bulma",
            group = "color",
            variable,
            value = parseCssColors(value)) %>%
  bind_rows(c(theme = "bulma",
              group = "font",
              variable = "Google Font",
              value = NA_character_))

usethis::use_data(bulma_config, overwrite = TRUE)
