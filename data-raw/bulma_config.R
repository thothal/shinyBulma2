## code to prepare `bulma_config` dataset goes here
library(here)
library(rvest)
library(dplyr)
library(stringr)
library(htmltools)

## get primary colors from official bulma doc

bulma_doc <- read_html("https://bulma.io/documentation/customize/variables/")

## initial values
initial_vars <- bulma_doc %>%
  html_node("#initial-variables ~ .table-container > table") %>%
  html_table() %>%
  setNames(c("variable", "type", "value"))

## theoretically we could get the values also from here, but rather parse it from sass

bulma_color_initial_sass <- here("tools", "node_modules", "bulma", "sass", "utilities",
                         "initial-variables.sass") %>%
  readLines()

color_names_initial <- initial_vars %>%
  filter(type == "color") %>%
  transmute(variable = str_remove(variable, fixed("$"))) %>%
  pull(variable)

colors_initial <- bulma_color_initial_sass %>%
  str_subset(str_c("\\$", color_names_initial, collapse = "|")) %>%
  str_match("^\\$(.*):\\s*([^!]+) .*")

colors_initial <- tibble(theme = "bulma",
                       group = "color",
                       subgroup = "initial",
                       variable = colors_initial[, 2],
                       value = parseCssColors(colors_initial[, 3])) %>%
  bind_rows(c(theme = "bulma",
            group = "font",
            subgroup = NA_character_,
            variable = "Google Font",
            value = NA_character_))

## derived values
bulma_table <- bulma_doc %>%
  html_nodes(".bd-is-body")

colors_derived_idx <- which(bulma_table %>%
  html_elements(".bd-var-computed-type") %>%
  html_text() == "color" &
    bulma_table %>%
    html_elements(".bd-var-type") %>%
    html_text() == "variable")

color_names_derived <- (bulma_table %>%
  html_elements(".bd-var-name .nt") %>%
  html_text())[colors_derived_idx]

bulma_color_derived_sass <- here("tools", "node_modules", "bulma", "sass", "utilities",
                                 "derived-variables.sass") %>%
  readLines()

colors_derived <- bulma_color_derived_sass %>%
  str_subset(str_c("^\\$", color_names_derived, ":", collapse = "|")) %>%
  str_match("^\\$(.*):\\s*\\$([^!]+) .*") %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(c("match", "variable", "link")) %>%
  left_join(colors_initial %>%
              select(link = variable, value),
            "link") %>%
  left_join(., I(.) %>% select(variable, value), by = c(link = "variable")) %>%
  transmute(theme = "bulma", group = "color", subgroup = "derived", variable,
            value = coalesce(value.x, value.y))

bulma_config <- rbind(colors_initial, colors_derived) %>%
  arrange(group, desc(subgroup, variable))

usethis::use_data(bulma_config, overwrite = TRUE)
