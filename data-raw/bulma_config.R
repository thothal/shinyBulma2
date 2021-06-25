## code to prepare `bulma_config` dataset goes here

library(here)

source(here::here("data-raw", "get_css_colors.R"))

bulma_config <- get_css_colors()
