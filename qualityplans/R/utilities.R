#' @title Reading in QIP config
#'
#' @description Function to read in configs of the QIPS
#'
#' @param config_file Yaml file containing config
#'
#' @return list of variables

# Should just be "config.yaml"

read_config <- function(config_file) {
  config <- yaml::read_yaml("D:/Repos/quality_improvement_plans/config.yaml")
  return(config)
}
