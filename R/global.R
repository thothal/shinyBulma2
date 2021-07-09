# Environment that holds various global variables and settings for this package,
# such as the current theme. It is not exported and should not be directly
# manipulated by other packages.
bulma_global <- new.env(parent = emptyenv())

# This variable will hold the theme used (if any)
# This is useful if we want to validate color names using bulma{swatch}_config
# we can then use this cariable to filter
bulma_global$theme <- NA
