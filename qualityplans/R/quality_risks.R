#' @title Extract Quality Risks for single division
#'
#' @description Function to extract Quality Risk tables from multiple excel tabs in one divisional file
#'
#' @param file_path File path for divisional QIP
#' @param risk_tabs Tab names for quality risks
#'
#' @return List of tables

extract_single_division_risks <- function(file_path, risk_tabs){
  lapply(risk_tabs, function(x){
    quality_risk_list <- readxl::read_xlsx(file_path, sheet = x , range = "B5:C10", na=null_values)
    return(quality_risk_list)
    })
}


#' @title Extract tab names and Quality Risks for multiple divisions
#'
#' @description Function to extract tab names (for each Quality Risk) for multiple divisions and extract Quality Risk tables
#'
#' @param file_path File path for all QIP folder
#'
#' @return List of tables

extract_all_division_risks <- function(file_path){
  lapply(file_path, function(x){
    tab_list <- excel_sheets(path = x)
    risk_tabs <- tab_list[!(tab_list %in% c("Contents", "Foreword by Sarah Henry", "Background", "Instructions", "Key Metrics", "BLANK Quality Risk", "Progress Check", "EXAMPLE Quality Risk"))]
    quality_risk_list <- extract_single_division_risks(x, risk_tabs)
    return(quality_risk_list)
  })
}






#' @title Sum risks to dimensions per division
#'
#' @description Function to sum rows containing entries for each division, to get number of risks to quality for each dimension
#'
#' @param division_risk_table Collated risk table for single division
#'
#' @return Table with added frequency column

sum_risks <- function(division_risk_table){
  division_risk_table$frequency <- rowSums(!is.na(division_risk_table[-1]))
  return(division_risk_table)
}




#' @title Collate risks for single division
#'
#' @description Function to merge all risks for each division into one table, with number of each
#'
#' @param quality_risk_list List of divisional risks containing sub lists
#' @param division_order Numeric vector from 1 to number of divisions
#'
#' @return Tidied table with total column

##MAKE DATA FRAME AND REMOVE DUPLICATES
create_single_division_risk_table <- function(quality_risk_list, division_order){
  division_risk_table <- data.frame(quality_risk_list[[division_order]])
  division_risk_table <- division_risk_table[!duplicated(as.list(division_risk_table))]
  division_risk_table <- sum_risks(division_risk_table)

  colnames(division_risk_table) <- c("dimension", paste0("qr_",1:(ncol(division_risk_table)-2)), "total")
  return(division_risk_table)
  }




#' @title Collate risks in tables for each division
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension
#'
#' @param quality_risk_list List of divisional risks containing sub lists
#'
#' @return List of tables

create_all_division_risk_tables <- function(quality_risk_list){
  lapply(1:length(quality_risk_list), function(x){
    each_division_risk_table_list <- create_single_division_risk_table(quality_risk_list, x)
    return(each_division_risk_table_list)
    })
  }






#' @title Sum risks to dimensions across divisions
#'
#' @description Function to sum risks to each quality dimension - give ONS total
#'
#' @param all_division_risk_table List of Collated risk table for single division
#'
#' @return Table with added frequency column

sum_no_risks <- function(all_division_risk_table){
  all_division_risk_table$frequency <- rowSums(all_division_risk_table[-1])
  return(all_division_risk_table)
}




#' @title Collate risks in tables for each division ####
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension ####
#'
#' @param quality_risk_list List of divisional risks containing ####
#'
#' @return Table of totals by division with added total column

merge_all_division_risks <- function(each_division_risk_table_list){
  all_division_risk_table <- data.frame(each_division_risk_table_list)
  all_division_risk_table <- all_division_risk_table[!duplicated(as.list(all_division_risk_table))]
  all_division_risk_table <- all_division_risk_table[,-grep("qr", colnames(all_division_risk_table))]
  all_division_risk_table <- sum_no_risks(all_division_risk_table)

  #colnames(all_division_risk_table) <- c("dimension", paste0("qr_",1:(ncol(all_division_risk_table)-2)), "total")
  return(all_division_risk_table)
}

