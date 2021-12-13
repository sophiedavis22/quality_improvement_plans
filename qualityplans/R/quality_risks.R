#' @title Reading in QIP risks from
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
