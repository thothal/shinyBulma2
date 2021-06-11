get_package_name <- function() {
  environmentName(parent.env(environment()))
}
