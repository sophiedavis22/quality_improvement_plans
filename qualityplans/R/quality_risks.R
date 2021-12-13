#' @title Reading in QIP risks
#'
#' @description Function to read in Quality Risks from multiple excel tabs
#'
#' @param file_path File path for divisional QIP
#' @param risk_tabs Tab names for quality risks
#'
#' @return list of tables

read_single_division_risk <- function(file_path, risk_tabs){
  lapply(risk_tabs, function(x){
    quality_risk_list <- readxl::read_xlsx(file_path, sheet = x , range = "B5:C10", na=null_values)
    return(quality_risk_list)

    })

}


#' @title Reading in divisional QIP files
#'
#' @description Function to read in Quality Risks for multiple divisions
#'
#' @param file_path File path for folder containing divisional QIPs
#'
#' @return list of tables

read_division_files <- function(file_path){
  lapply(file_path, function(x){
    tab_list <- excel_sheets(path = x)
    risk_tabs <- tab_list[!(tab_list %in% c("Contents", "Foreword by Sarah Henry", "Background", "Instructions", "Key Metrics", "BLANK Quality Risk", "Progress Check", "EXAMPLE Quality Risk"))]
    quality_risk_list <- read_single_division_risk(x, risk_tabs)
    return(quality_risk_list)
  })
}



#' @title Reading in divisional QIP files
#'
#' @description Function to read in Quality Risks for multiple divisions
#'
#' @param file_path File path for folder containing divisional QIPs
#'
#' @return list of tables

sum_risks <- function(division_risk_table){
  division_risk_table$frequency <- rowSums(!is.na(division_risk_table[-1]))
  return(division_risk_table)
}




#' @title Reading in divisional QIP files
#'
#' @description Function to read in Quality Risks for multiple divisions
#'
#' @param file_path File path for folder containing divisional QIPs
#'
#' @return list of tables

##MAKE DATA FRAME AND REMOVE DUPLICATES
create_division_risk <- function(quality_risk_list, division_name, divison_order){
  division_name <- data.frame(quality_risk_list[[divison_order]])
  division_name <- division_name[!duplicated(as.list(division_name))]
  division_name <- sum_risks(division_name) return(division_name)
  #RENAME COLs colnames(divison_name)
}#make table for all divisons
create_all_divison_risks <- function(list, divison_name_list){
  lapply(1:length(divison_name_list), function(x){ list_of_divs <- create_division_risk(list, divison_name_list[x], x) })}

