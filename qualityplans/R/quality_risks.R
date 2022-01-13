#' @title Extract Quality Risks for single division
#'
#' @description Function to extract Quality Risk tables from multiple excel tabs in one divisional file
#'
#' @param file_path File path for single division
#' @param risk_tabs Tab names for division
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (output table for each risk)

extract_risks_from_tabs <- function(file_path, risk_tabs, config_file="D:/Repos/quality_improvement_plans/config.yaml"){
  config <- read_config(config_file)
  lapply(risk_tabs, function(x){
    quality_risk_list <- readxl::read_xlsx(file_path, sheet = x , range = config$dimension_range, na=config$null_values)
    return(quality_risk_list)
    })
}





#' @title Extract Quality Risks for all divisions
#'
#' @description Function to extract tab names (for each division) and extract Quality Risk tables for all divisions
#'
#' @param file_paths Character vector of file paths
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (list by division, sub-lists by risk)

extract_all_risks_unnamed <- function(file_paths, config_file="D:/Repos/quality_improvement_plans/config.yaml"){
  config <- read_config(config_file)
  lapply(file_paths, function(x){
    tab_list <- readxl::excel_sheets(path = x)
    risk_tabs <- tab_list[!(tab_list %in% config$non_risk_tab_names)]
    risk_list <- extract_risks_from_tabs(x, risk_tabs)

    risk_names <- paste0("qr_", 1:length(risk_tabs))
    risk_list <- assign_list_names(risk_list, risk_names)
    return(risk_list)
  })
}



#' @title Extract Quality Risks for all divisions (with named list!)
#'
#' @description Function to extract tab names (for each division), extract Quality Risk tables for all divisions and name list elements
#'
#' @param file_paths Character vector of file paths
#' @param new_list_names  New names as values
#' @param config_file .yaml file containing config, with default settings (data directory, null values, metric ranges etc)
#'
#' @return List of tables (list by division, sub-lists of tibbles for each risk)

get_all_risks_list <- function(file_paths, new_list_names, config_file="D:/Repos/quality_improvement_plans/config.yaml") {
  all_risk_list <- extract_all_risks_unnamed(file_paths, config_file)
  all_risk_list <- assign_list_names(all_risk_list, new_list_names)
  return(all_risk_list)
}



#' @title Sum over non-empty rows
#'
#' @description Function to sum non-empty cells in rows (excluding first row as row name)
#'
#' @param table Table with characters
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
#' @param risk_list List of Quality divisions for single division
#' @param division_order Numeric vector from 1 to number of divisions
#'
#' @return Table with total column

##MAKE DATA FRAME AND REMOVE DUPLICATES
merge_risks <- function(risk_list, division_order){
  risk_table <- data.frame(risk_list[[division_order]])
  risk_table <- risk_table[!duplicated(as.list(risk_table))]
  risk_table <- sum_risks(risk_table)

  colnames(risk_table) <- c("dimension", paste0("qr_",1:(ncol(risk_table)-2)), "total")
  return(risk_table)
  }



#' @title Collate risks in tables for each division
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension
#'
#' @param all_risk_list List of divisional risks containing sub lists
#'
#' @return List of tables (not named)

merge_all_risks <- function(all_risk_list){
  lapply(1:length(all_risk_list), function(x){
    each_div_risk_table <- merge_risks(all_risk_list, x)
    return(each_div_risk_table)
    })
  }


## can I build this into the function above rather than being separate?


#' @title Collate risks in tables for each division (list elements names)
#'
#' @description Function to produce table of risks for each division, with totals for risk to each dimension and named list elements
#'
#' @param all_risk_list List by division, sub-list by risk
#' @param division_names Names as values
#'
#' @return List of tables (named)

get_risk_dimension_tables <- function(all_risk_list, division_names) {
  merged_risks <- merge_all_risks(all_risk_list)
  merged_risks <- assign_list_names(merged_risks, division_names)
  return(merged_risks)
}



### UNIT TESTS completed to this line (look at documentation for last 3)
########################################################################################################

#=============================================================================






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
  dimension_names <- all_div_dim_table[,1]
  all_div_dim_table <- all_div_dim_table[,-grep("dimension", colnames(all_div_dim_table))]
  all_div_dim_table <- all_div_dim_table[,-grep("qr", colnames(all_div_dim_table))]
  all_div_dim_table <- sum_no_risks(all_div_dim_table)

  colnames(all_div_dim_table) <- gsub(".total", "", colnames(all_div_dim_table))
  all_div_dim_table <- cbind(dimension_names, all_div_dim_table)
  colnames(all_div_dim_table)[1] <- "dimension"
  return(all_div_dim_table)
}



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

  final_table$pct <- as_percent(final_table$total/total_n_risks)
  return(final_table)
}
