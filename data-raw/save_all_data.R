library(here)

source(here("data-raw", "bulma_config.R"))
source(here("data-raw", "bulmaswatch_config.R"))

usethis::use_data(bulma_config,
                  bulmaswatch_config,
                  internal = TRUE,
                  overwrite = TRUE)
