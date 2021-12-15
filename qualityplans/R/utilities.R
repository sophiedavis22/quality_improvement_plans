#' @title Reading in QIP config
#'
#' @description Function to read in configs of the QIPs, including directory and null_values
#'
#' @param config_file Yaml file containing config, with default setting
#'
#' @return List of variables

# Should just be "config.yaml"

read_config <- function(config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- yaml::read_yaml(config_file)
  return(config)
}





#' @title Extract file list
#'
#' @description Function to extract list of file paths from folder with QIPs, sorted alphabetically
#'
#' @param config_file Yaml file containing config
#'
#' @return List of file paths

get_file_path_list <- function(config_file) {
  config_file <- read_config()
  file_path_list <- list.files(path=config_file$directory, pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
  file_path_list <- sort(file_path_list)
  return(file_path_list)
}





#' @title Extract division names
#'
#' @description Function to extract division names (acronyms) from file paths
#'
#' @param file_path_list List of file paths
#'
#' @return Division names as values

get_division_name_list <- function(file_path_list) {
  config_file <- read_config()
  alt_file_path <- gsub("$", "\\$", config_file$directory, fixed=TRUE)
  division_name <- gsub(".xlsx", "", file_path_list)
  division_name <- gsub(alt_file_path, "", division_name)
  division_name <- gsub("/UPDATED_Quality_Improvement_Plan_", "", division_name)
  division_name <- sort(division_name)
  return(division_name)
}





#' @title Assign names to list elements
#'
#' @description Function to name elements of list
#'
#' @param list Any list
#' @param new_names Names as values
#'
#' @return List named elements

assign_division_names <- function(list, new_names){
  names(list) <- new_names
  return(list)
}





#' @title Convert number to percentage
#'
#' @description Function to convert input number to percentage format
#'
#' @param number Numeric variable
#'
#' @return Percentage as character

as.percent <- function(number, digits = 1, format = "f", ...) {      # Create user-defined function
  paste0(formatC(number * 100, format = format, digits = digits, ...), "%")
}
