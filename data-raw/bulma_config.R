## code to prepare `bulma_config` dataset goes here

source(here("data-raw", "get_css_colors.R"))

bulma_config <- get_css_colors()


usethis::use_data(bulma_config, overwrite = TRUE)
