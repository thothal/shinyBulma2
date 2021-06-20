## code to prepare `bulmaswatch_config` dataset goes here
library(here)
library(purrr)

source(here("data-raw", "get_css_colors.R"))

themes <- list.dirs(here("tools", "node_modules", "bulmaswatch"), recursive = FALSE) %>%
  basename() %>%
  setdiff(c("api", "default"))

bulmaswatch_config <- get_css_colors(themes)

usethis::use_data(bulmaswatch_config, overwrite = TRUE)
