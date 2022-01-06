#' @title Extract Quality Risks for single division
#'
#' @description Function to extract Quality Risk tables from multiple excel tabs in one divisional file
#'
#' @param file_path File path for single division
#' @param risk_tabs Tab names for division
#'
#' @return List of tables (output table for each risk)

extract_risks_from_tabs <- function(file_path, risk_tabs){
  config_file <- read_config()
  lapply(risk_tabs, function(x){
    quality_risk_list <- readxl::read_xlsx(file_path, sheet = x , range = "B5:C10", na=config_file$null_values)
    return(quality_risk_list)
    })
}





#' @title Extract Quality Risks for all divisions
#'
#' @description Function to extract tab names (for each division) and extract Quality Risk tables for all divisions
#'
#' @param file_path List of file paths
#'
#' @return List of tables (list by division, sub-lists by risk)

extract_risks_all_div <- function(file_path){
  lapply(file_path, function(x){
    tab_list <- excel_sheets(path = x)
    risk_tabs <- tab_list[!(tab_list %in% c("Contents", "Foreword by Sarah Henry", "Background", "Instructions", "Key Metrics", "BLANK Quality Risk", "Progress Check", "EXAMPLE Quality Risk"))]
    all_div_risk_list <- extract_risks_from_tabs(x, risk_tabs)
    return(all_div_risk_list)
  })
}



## can I build this into the function above rather than being separate?


#' @title Extract Quality Risks for all divisions (with named list!)
#'
#' @description Function to extract tab names (for each division), extract Quality Risk tables for all divisions and name list elements
#'
#' @param file_path List of file paths
#' @param division_names  Names as values
#'
#' @return List of tables

get_sep_risks_all_div <- function(file_path, division_names) {
  all_div_sep_risk_list <- extract_risks_all_div(file_path)
  all_div_sep_risk_list_named <- assign_division_names(all_div_sep_risk_list, division_names)
  return(all_div_sep_risk_list_named)
}





#' @title Sum over non-empty rows
#'
#' @description Function to sum non-empty cells in rows (excluding first row as row name)
#'
#' @param table Table with character entries
#'
#' @return Table with added frequency column

sum_risks <- function(table){
  table$frequency <- rowSums(!is.na(table[-1]))
  return(table)
}





#' @title Collate Quality Risks for single division
#'
#' @description Function to merge all Quality Risks for each division into one table, with row sums for number of risks affecting each dimension
#'
#' @param all_div_sep_risk_list_named List of Quality divisions for single division
#' @param division_order Numeric vector from 1 to number of divisions
#'
#' @return Table with total column

##MAKE DATA FRAME AND REMOVE DUPLICATES
merge_single_div_risks <- function(all_div_sep_risk_list_named, division_order){
  division_risk_table <- data.frame(all_div_sep_risk_list_named[[division_order]])
  division_risk_table <- division_risk_table[!duplicated(as.list(division_risk_table))]
  division_risk_table <- sum_risks(division_risk_table)

  colnames(division_risk_table) <- c("dimension", paste0("qr_",1:(ncol(division_risk_table)-2)), "total")
  return(division_risk_table)
  }




#' @title Collate risks in tables for each division
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension
#'
#' @param all_div_sep_risk_list_named List of divisional risks containing sub lists
#'
#' @return List of tables (not named)

merge_all_div_risks <- function(all_div_sep_risk_list_named){
  lapply(1:length(all_div_sep_risk_list_named), function(x){
    each_div_risk_table <- merge_single_div_risks(all_div_sep_risk_list_named, x)
    return(each_div_risk_table)
    })
  }


## can I build this into the function above rather than being separate?


#' @title Collate risks in tables for each division (list elements names)
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension and named list elements
#'
#' @param all_div_sep_risk_list_named List by division, sub-list by risk
#' @param division_names Names as values
#'
#' @return List of tables (named)

get_merged_risks_all_div <- function(all_div_sep_risk_list_named, division_names) {
  all_div_risks_merged <- merge_all_div_risks(all_div_sep_risk_list_named)
  all_div_risks_merged <- assign_division_names(all_div_risks_merged, division_names)
  return(all_div_risks_merged)
}





#' @title Sum over numerical rows
#'
#' @description Function to sum numerical cells in rows (excluding first row as row name)
#'
#' @param table Table with character and numeric entries
#'
#' @return Table with added total column

sum_no_risks <- function(table){
  table$total <- rowSums(table[-1])
  return(table)
}





#' @title Create single division/dimension table - numbers only
#'
#' @description Function to create a single table with dimension breakdown by division, and sum for each dimension
#'
#' @param all_div_risks_merged List of merged risks by division
#'
#' @return Table of totals by division with added total column

create_all_div_dimension_table <- function(all_div_risks_merged){
  all_div_dim_table <- data.frame(all_div_risks_merged)
  all_div_dim_table <- all_div_dim_table[!duplicated(as.list(all_div_dim_table))]
  all_div_dim_table <- all_div_dim_table[,-grep("qr", colnames(all_div_dim_table))]
  #all_div_dim_table <- sum_no_risks(all_div_dim_table)

  #colnames(all_div_dim_table) <- gsub(".total", "", colnames(all_div_dim_table))
  #colnames(all_div_dim_table)[1] <- "dimension"
  return(all_div_dim_table)
}


create_all_div_dimension_table(all_divisions_risks_merged)


#' @title Count number of quality risks per division
#'
#' @description Function to count number of quality risks in each division from list of risks by division
#'
#' @param all_div_risks_merged List of tables for each division with merged quality risks
#'
#' @return List with number of risks for each division

extract_n_risk_division <- function(all_div_risks_merged){
  lapply(all_div_risks_merged, function(x){
    n_risk_division_list <- as.numeric(ncol(x)-2)
    return(n_risk_division_list)
  })
}





#' @title Number of Quality Risks by division in table
#'
#' @description Function to present number of quality risks in table from list of totals by division
#'
#' @param all_div_risks_merged List of tables for each division with merged quality risks
#'
#' @return Table

create_all_div_risk_table <- function(all_div_risks_merged){
  n_risk_division_list <- extract_n_risk_division(all_div_risks_merged)
  n_risk_table <- data.frame(n_risk_division_list)
  n_risk_table$dimension <- "No. quality risks"
  n_risk_table <- n_risk_table[c(ncol(n_risk_table),1:(ncol(n_risk_table)-1))]
  n_risk_table <- sum_no_risks(n_risk_table)
  return(n_risk_table)
}





#' @title Table of Quality risks by division and dimension
#'
#' @description Function to produce table with all risks by dimension and division, with final % of Quality risks affecting each division
#'
#' @param all_div_risks_merg List of tables for each division with merged quality risks
#'
#' @return Table

get_final_risk_dimension_table <- function(all_div_risks_merg){
  table_by_dimension <- create_all_div_dimension_table(all_div_risks_merg)
  table_by_risk <- create_all_div_risk_table(all_div_risks_merg)
  total_n_risks <- table_by_risk$total

  final_table <- rbind(table_by_dimension, table_by_risk)

  final_table$pct <- as.percent(final_table$total/total_n_risks)
  return(final_table)
}
