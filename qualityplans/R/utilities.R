#' @title Reading in QIP config
#'
#' @description Function to read in configs of the QIPS
#'
#' @param config_file Yaml file containing config
#'
#' @return list of variables


read_config <- function(config_file) {
  config <- yaml::read_yaml("config.yaml")
  return(config)
}
