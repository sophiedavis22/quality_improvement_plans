#' @title Reading in config file
#'
#' @description Function to read in configs of the QIPs, including directory and null_values
#'
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of variables
#'
#' @export

read_config <- function(config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- yaml::read_yaml(config_file)
  return(config)
}



#' @title Extract file names and locations
#'
#' @description Function to extract list of file paths from folder with QIPs, sorted alphabetically
#'
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return All file paths as character vector (alphabetical)
#'
#' @export

get_file_paths <- function(config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  file_paths <- list.files(path=config$directory, pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
  file_paths <- sort(file_paths)
  return(file_paths)
}



#' @title Extract division names
#'
#' @description Function to extract division names (acronyms) from file paths
#'
#' @param file_paths List of file paths as character vector
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return All division names as character vector
#'
#' @export

get_division_names <- function(file_paths, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  config <- read_config(config_file)
  alt_file_path <- gsub("$", "\\$", config$directory, fixed=TRUE)
  division_names <- gsub(".xlsx", "", file_paths)
  division_names <- gsub(alt_file_path, "", division_names)
  division_names <- gsub(config$file_prefix, "", division_names)
  division_names <- sort(division_names)
  return(division_names)
}



#' @title Extract (single) division name
#'
#' @description Function to extract division name (acronym) from file path
#'
#' @param file_paths File path as character
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metir ranges and reporting months)
#'
#' @return Division name as character
#'
#' @export

#get_division_name <- function(file_path, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
#  config_file <- read_config()
#  alt_file_path <- gsub("$", "\\$", config_file$directory, fixed=TRUE)
#  division_name <- gsub(".xlsx", "", file_path)
#  division_name <- gsub(alt_file_path, "", division_name)
#  division_name <- gsub(config$file_prefix, "", division_name)
#  return(division_name)
#}



#' @title Assign new names to list elements
#'
#' @description Function to name elements of list
#'
#' @param list Any list
#' @param new_names Names as character vector of same length as list
#'
#' @return List named elements
#'
#' @export

assign_list_names <- function(list, new_names){
  if(length(list)==length(new_names)) {
    names(list) <- new_names
    return(list)
  } else {
    stop("List and proposed new names are different lengths")
  }
}



#' @title Convert value to percentage
#'
#' @description Function to convert input value to percentage format
#'
#' @param value Numeric variable
#' @param digits Number of decimal places
#' @param format format
#'
#' @return Percentage as character
#'
#' @export

as_percent <- function(value, digits = 0, format = "f", ...) {      # Create user-defined function
  if(typeof(value)=="double"){
    paste0(formatC(value * 100, format = format, digits = digits, ...), "%")
  } else {
    stop("Input not of type double")
  }
}
